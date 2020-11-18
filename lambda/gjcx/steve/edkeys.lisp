; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*- 1983

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

; History:
;   removed references to editor-defun-key  5/8/84


(defvar re-generate-screen-really)
(defvar *editor-cursor*)
(defvar *editor-buffer*)
(defvar *editor-current-key*)
(defvar creamed-tty-lines-to 0)
(defconstant left-paren #\()
(defconstant right-paren #\))

(defvar *argument* 1)
(defvar argument-supplied? ())
;These are used by #\control-0 etc.
;AUTO-DIGIT-ARG-SAVE stores the old *ARGUMENT* so the new value can
;be computed when more digits are added. Auto digit save is similar.
(defvar auto-digit-arg-save nil)

(define-bit-prefix #\control-^ char-control-bit "C-")
(define-bit-prefix #\altmode char-meta-bit "M-")
(define-bit-prefix #\control-\\ char-meta-bit "M-")
(define-bit-prefix #\control-z (logior& char-control-bit char-meta-bit) "C-M-")

(define-prefix-char #\control-x "C-X ")

(editor-bind-key #\control-q (quoted-insert))

(editor-bind-key #\control-b backward-character)

(defun backward-character ()
  (send *editor-cursor* :advance-pos (minus *argument*))
  nil)

(editor-bind-key #\control-h backward-character)

(editor-bind-key #\backspace backward-character)

(editor-bind-key #\control-f forward-character)

(defun forward-character ()
   (send *editor-cursor* :advance-pos *argument*)
   nil)

(defvar real-line-target-column nil)

(defvar real-line-goal-column nil)

(editor-bind-key #\control-n down-real-line)

(defun down-real-line ()
 (cond ((-p *argument*)
        (setq *argument* (-& *argument*)) ;Cannot handle *:min-fixnum. BIG deal
        (up-real-line))
       (t (cond ((argument?) (not-last-line))
                ((null (line-next (bp-line *editor-cursor*)))
                 (send (bp-line *editor-cursor*) :insert-line "")))
          (or (memq *editor-last-command* '(down-real-line up-real-line))
              real-line-goal-column
              (setq real-line-target-column
                    (edit-cursor-position-really *editor-cursor*)))
          (let ((line (nth-next-line (bp-line *editor-cursor*) *argument*)))
            (send *editor-cursor*
                  :move line (position-from-graphic-position
                              line (or real-line-goal-column
                                       real-line-target-column))))
          nil)))

(editor-bind-key #\control-p up-real-line)

(defun up-real-line ()
  (cond ((-p *argument*)
         (setq *argument* (-& *argument*))
         (down-real-line))
        (t (not-first-line) ;Error out if at beginning of buffer.
           (or (memq *editor-last-command* '(up-real-line down-real-line))
               real-line-goal-column
               (setq real-line-target-column
                     (edit-cursor-position-really *editor-cursor*)))
           (let ((line (nth-previous-line (bp-line *editor-cursor*)
                                          *argument*)))
             (send *editor-cursor*
                   :move line (position-from-graphic-position
                               line (or real-line-goal-column
                                        real-line-target-column))))
           nil)))

(editor-bind-key '(#\control-x #\control-n) set-goal-column)

(defun set-goal-column ()
 (cond ((and (null argument-supplied?) (=& 1 *argument*))
        (setq real-line-goal-column
         (edit-cursor-position-really *editor-cursor*))
        (with-notify-line-remaining
         (format terminal-io "Goal column is ~a" real-line-goal-column)))
       (t (setq real-line-goal-column nil)
          (with-notify-line-remaining
           (oustr "No goal column" terminal-io)))))

(editor-bind-key #\control-d delete-character)

(defun delete-character ()
  (not-buffer-end)
  (cond ((argument?) (kill-and-delete-from-bp *editor-cursor* *argument*))
        (t (send *editor-cursor* :delete-characters *argument*)))
  nil)

;;; Note that the key assignments of Rubout and c-Rubout have been swapped
;;; This makes Lisp Mode's bindings the default

(editor-bind-key #\control-rubout backward-delete-character)

(defun backward-delete-character ()
  (not-buffer-begin)
  (cond ((not (null *overwrite-mode*))
         (send *editor-cursor* :advance-pos (-& *argument*))
         (self-insert #\space *argument*)
         (send *editor-cursor* :advance-pos (-& *argument*)))
        ((=& *argument* 1) (send *editor-cursor* :delete-characters -1))
        (t (kill-and-delete-from-bp *editor-cursor* (-& *argument*))))
  nil)

;The complicated computation to figure out how big the tab was can be
;eliminated by using the stored location of the cursor as the
;current "position-really". However, this is not valid except right after a
;screen re-fresh. This code is slower, but only for one char out of eight,
;and it makes fewer assumptions which might prove dangerous.
(editor-bind-key #\rubout backward-delete-hacking-tabs)

(defun backward-delete-hacking-tabs ()
  (not-buffer-begin)
  (cond ((null *overwrite-mode*)
         ;;If there is a tab which gets expanded then this kill
         ;;will not really be right, but it can't be so there.
         (unless (null (argument?))
           (kill-characters-from-bp *editor-cursor* (-& *argument*)))
         (loop for i from 1 to *argument*
               for char-was = (send *editor-cursor* :get-char-backward)
               do (send *editor-cursor* :delete-char)
               if (char= char-was #\tab)
               do (self-insert #\space
                               (1-& (graphic-char-length
                                     #\tab
                                     (position-really
                                      (bp-line *editor-cursor*)
                                      (bp-position *editor-cursor*)
                                      (window-stream-x-size
                                       (edit-cursor-window
                                        *editor-cursor*))))))))
        (t (backward-delete-character)))
  nil)

(editor-bind-key #\return crlf)

(defun crlf ()
 (let ((pos (bp-position *editor-cursor*))
       (line (bp-line *editor-cursor*)))
  (when (and (not (string-equal comment-end ""))
             (=& (line-char-count line)
                 (+& pos (string-length comment-end)))
             (send line :match-string comment-end pos))
   (send *editor-cursor* :set-pos (line-char-count line))))
 (loop for i from 1 to *argument*
       for line = (bp-line *editor-cursor*)
       if (and (=& (bp-position *editor-cursor*) (line-char-count line))
                   (line-next line)
                   (empty-line? (line-next line))
                   (line-next (line-next line))
                   (empty-line? (line-next (line-next line))))
       do (send *editor-cursor* :to-beginning-of-next-line)
       else
       do (send (bp-line *editor-cursor*)
                :break (bp-position *editor-cursor*)))
 nil)

(editor-bind-key #\line linefeed)

(defun linefeed ()
  (delete-horizontal-space)
  (loop for i from 1 to *argument*
        do (crlf))
  (funcall (internal-editor-lookup-key #\tab))
  nil)

(editor-bind-key #\page new-window)

(defun new-window ()
  (cond ((and (=& *argument* 4) (null argument-supplied?)) ;just control-U
         (let ((y (window-stream-y-now (edit-cursor-window *editor-cursor*))))
           (cursorpos y 0 terminal-io)
           (send terminal-io :raw-oustr (svref old-screen-image y)
                 0 *tty-width*)
           (send terminal-io :set-cursor-fried-flag t)))
        (t (let ((y-size (window-stream-y-size
                          (edit-cursor-window *editor-cursor*))))
             (cond ((and (null argument-supplied?) (=& *argument* 1))
                    (setq *argument* (ceiling (* y-size *recentering-fraction*)))
                    (clear-one-screen-image old-screen-image)
                    (send terminal-io :clear-screen)
                    (setq re-generate-screen-really t)
                    (setq *last-major-mode* "No Mode"))
                   ((-p *argument*)
                    (setq *argument* (+& y-size *argument*))))
             (setq *argument* (max& 0 (min& *argument* (1-& y-size)))))
           (loop for i from 0 to *argument*
                 for line first (bp-line *editor-cursor*)
                 then (line-previous line)
                 while (not (null (line-previous line)))
                 finally
                 (unless (null line)
                   (setf (edit-cursor-home-line *editor-cursor*) line)))))
 nil)

(editor-bind-key #\control-meta-r reposition-window)

(defun reposition-window ()
 (do ((line (bp-line *editor-cursor*) (line-previous line))
      (chars))
     ((null line) (ed-warn :unbalanced-parens))
   (setq chars (line-chars line))
   (when (and (+p (line-char-count line)) (char= (schar chars 0) left-paren))
         (setf (edit-cursor-home-line *editor-cursor*) line)
         (return t)))
 nil)

(editor-bind-key #\control-a beginning-of-line)

(defun beginning-of-line ()
   (send *editor-cursor* :set-pos 0)
   nil)

(editor-bind-key #\control-e end-of-line)

(defun end-of-line ()
 (send *editor-cursor* :set-pos (line-char-count (bp-line *editor-cursor*)))
 nil)

(editor-bind-key #\control-g editor-abort)

(defun editor-abort ()
  (steve-beep)
  nil)

(editor-bind-key #\bell editor-abort)

(editor-bind-key '(#\control-x #\bell) editor-abort)

(editor-bind-key '(#\control-x #\control-g) editor-abort)

(editor-bind-key #\control-o open-line)

(defun open-line ()
  (loop for i from 1 to *argument*
        do (send (bp-line *editor-cursor*)
                 :break (bp-position *editor-cursor*))
        do (send *editor-cursor* :to-end-of-previous-line))
 nil)

;
;File i/o commands and related functions.
;

(editor-bind-key #\meta-~ buffer-not-modified)

(defun buffer-not-modified ()
  (setf (buffer-modified? *editor-buffer*) nil)
  nil)

(editor-bind-key '(#\control-x #\control-q) set-file-read-only)

(defun set-file-read-only (&optional (buffer *editor-buffer*))
  (cond ((=& *argument* 0)
         (setf (buffer-access buffer) buffer-access-any)
         (with-error-line-remaining
           (oustr "Any Access Allowed" terminal-io)))
        ((+p *argument*)
         (setf (buffer-access buffer)
               (logior& (buffer-access buffer)
                        buffer-access-file-read-only))
         (with-error-line-remaining
           (oustr "File Read Only" terminal-io)))
        (t (setf (buffer-access buffer)
                 (logior& (buffer-access buffer)
                          buffer-access-buffer-read-only))
           (with-error-line-remaining
             (oustr "Buffer Read Only" terminal-io))))
  nil)

(editor-bind-key '(#\control-x #\control-w) write-file)

(defun write-file ()
  (with-query-line
    (princ "Write file: " terminal-io)
    (let ((filename (editor-default (read-file-name))))
      (output-buffer-to-file filename *editor-buffer* T)))
  nil)

(editor-bind-key '(#\control-x #\control-s) save-file)

(defun save-file (&optional (buffer *editor-buffer*) &aux ok-to-write?)
  (cond ((null (buffer-modified? buffer))
         (with-notify-line-remaining
           (oustr "(No changes need to be written)" terminal-io)))
        ((send buffer :send-if-handles :not-last-version?)
         (with-double-second-line
           (with-double-line
             (format terminal-io
                     "The file now on disk is not what you last visited or saved!~
                    ~%Should I write the file anyway (Y or N)? ")
             (setq ok-to-write? (ed-y-or-n-p)))))
        (:else (setq ok-to-write? t)))
  (when ok-to-write?
    (send buffer :set-file-name (buffer-file-name buffer))
    (output-buffer-to-file (buffer-file-name buffer) buffer))
  nil)


(editor-bind-key '(#\control-x #\control-v) visit-file)

(defun visit-file ()
 (with-query-line
   (princ "Visit File: " terminal-io)
   (let ((file-name (read-file-name)))
     (when (buffer-modified? *editor-buffer*)
       (with-error-line
         (format terminal-io "Write out changes to ~a (Y or N)? "
                 (send (buffer-file-name *editor-buffer*) :string-for-editor))
         (when (ed-y-or-n-p)
           (save-file))))
     (read-file-into-buffer (editor-default file-name))))
 nil)

(editor-bind-key '(#\control-x #\control-r) visit-file)

(bindmetax revert-file revert-file)

(defun revert-file ()
  (with-query-line
   (oustr "Restore file from disk? " terminal-io)
   (when (ed-y-or-n-p)
     (mark-beginning)
     (let ((line (count-between (pop-mark) *editor-cursor*))
           (index (bp-position *editor-cursor*)))
       (read-file-into-buffer (buffer-file-name *editor-buffer*))
       (let (we-won)
         (unwind-protect
             (progn (let ((*argument* (1-& line)))
                      (forward-line))
                    (send *editor-cursor* :advance-pos index)
                    (setq we-won t))
           (unless we-won
             (send *editor-cursor* :move (last-line) 0))))))))

(editor-bind-key '(#\control-x #\control-f) find-file)

(defun find-file ()
 (with-query-line
  (princ "Find File: " terminal-io)
  (point-selected (editor-default (read-file-name))))
 nil)

(editor-bind-key '(#\control-x #\B) select-buffer)

(defun select-buffer ()
 (with-query-line
   (format terminal-io "Select buffer (~a): "
           (buffer-name *last-buffer-selected*))
   (let ((name (prescan #'read-buffer-name 0)))
     (if (not (stringp name))
         (steve-beep)
       (select-buffer-named name *last-buffer-selected*))))
 nil)

(editor-bind-key '#\control-meta-l select-last-buffer)

(defun select-last-buffer ()
  (if *last-buffer-selected*
      (point-selected *last-buffer-selected*)
    (ed-warn "No buffer to select"))
  nil)

(editor-bind-key '#\meta-page select-last-buffer)


;
;A bunch of cursor movement and window movement functions.
;

(defun adjust-window-by-lines (lines &optional (point *editor-cursor*))
  (let ((home (nth-next-line (edit-cursor-home-line point) lines)))
    (setf (edit-cursor-home-line point) home
          (edit-cursor-home-pos point) 0)
    (when (null (point-on-screen? point))
      (send point :move (edit-cursor-home-line point) 0))))

(editor-bind-key #\control-v next-screen)

(defun next-screen (&optional (point *editor-cursor*))
  (cond ((argument?) (adjust-window-by-lines *argument* point))
        ((buffer-end-on-screen? point) (ed-warn "Last Screen"))
        (t (loop with win = (edit-cursor-window point)
                 with x-size = (window-stream-x-size win)
                 with line = (edit-cursor-home-line point)
                 with max-i = (max& (-& (window-stream-y-size win) 2) 1)
                 for i first 0 then (+& i (aprox-line-height line x-size))
                 for next = (line-next line)
                 while (<& i max-i)
                 until (null next)
                 do (setq line next)
                 finally (setf (edit-cursor-home-line point) line)
                 finally (setf (edit-cursor-home-pos point) 0)
                 finally (send point :move line 0))))
 nil)

(defun previous-screen (&optional (point *editor-cursor*))
  (cond ((argument?) (adjust-window-by-lines (-& *argument*) point))
        (t (not-buffer-begin point)
           (loop with win = (edit-cursor-window point)
                 with x-size = (window-stream-x-size win)
                 with line = (edit-cursor-home-line point)
                 with max-i = (max& (-& (window-stream-y-size win) 2) 1)
                 for i first 0 then (+& i (aprox-line-height line x-size))
                 for previous = (line-previous line)
                 while (<& i max-i)
                 until (null previous)
                 do (setq line previous)
                 finally (setf (edit-cursor-home-line point) line)
                 finally (setf (edit-cursor-home-pos point) 0)
                 finally (send point :move line 0))))
 nil)

(editor-bind-key #\meta-v previous-screen)

(editor-bind-key #\meta-r move-to-screen-edge)

(defun move-to-screen-edge ()
 (let* ((x-size (window-stream-x-size (edit-cursor-window *editor-cursor*)))
        (y-size (window-stream-y-size (edit-cursor-window *editor-cursor*)))
        (target (cond ((and (=& *argument* 1) (null argument-supplied?))
                       (/& (1+& y-size) 3))
                      ((-p *argument*) (+& y-size *argument*))
                      (t *argument*))))
  (setq target (max& 0 (min& target y-size)))
  (loop for i from 0 to target
        for line first (edit-cursor-home-line *editor-cursor*)
        then (line-next line)
        for j first (aprox-line-height line x-size)
        then (+& j (aprox-line-height line x-size))
        while (and (<& j target)
                   (line-next line))
;       Finally (send *editor-cursor* :move line
;                      (position-from-graphic-position
;                       line
;                       (edit-cursor-position-really *editor-cursor*)))
        finally (send *editor-cursor* :move line 0)))
 nil)

(editor-bind-key #\meta-< goto-beginning) ;See documentation for hair.

(defun goto-beginning ()
   (push-mark *editor-cursor*)
   (send *editor-cursor* :move (buffer-content *editor-buffer*) 0)
   (setf (edit-cursor-home-line *editor-cursor*)
         (buffer-content *editor-buffer*))
   nil)

(editor-bind-key #\meta-> goto-end)

(defun goto-end ()
  (let ((last (last-line)))
    (push-mark *editor-cursor*)
    (send *editor-cursor*
          :move last (line-char-count last)))
  nil)

(editor-bind-key #\meta-i insert-tab)

(defun insert-tab ()
 (self-insert #\tab)
 nil)

(editor-bind-key #\meta-tab insert-tab)

(editor-bind-key #\control-i #\tab)



;Multiple windows.

(defvar dash-buffer)

(defun make-dash-buffer (&aux (width (1-& (window-stream-x-size
                                           (edit-cursor-window
                                            *editor-cursor*)))))
  (setq dash-buffer
        (make-instance 'buffer
                       :name "Dash Buffer"
                       :content
                       (make-instance 'line
                                      :chars
                                      (make-string width :initial-element #\-)
                                      :char-count width
                                      :bashed? 0
                                      :graphic-length width
                                      :buffer-pointers nil
                                      :previous nil
                                      :next nil)
                       :file-name nil
                       :environment nil
                       :mark-ring (make-vector mark-ring-size :initial-element nil)
                       :mark-ring-index 0
                       :access 0
                       :modified? nil))
  (setf (line-buffer (buffer-content dash-buffer)) dash-buffer))

(defun put-dashes (y)
  (or (boundp 'dash-buffer) (make-dash-buffer))
  (let ((point
         (make-instance 'edit-cursor
                        :buffer dash-buffer
                        :line (buffer-content dash-buffer)
                        :position 0
                        :home-line (buffer-content dash-buffer)
                        :window (create-window 0 y *tty-width* 1))))
    (push point *all-edit-cursors*)))

(defun remove-dashes ()
  (loop for point in *all-edit-cursors*
        if (eq (bp-buffer point) dash-buffer)
        do (setq *all-edit-cursors* (delq point *all-edit-cursors*))))

(editor-bind-key '(#\control-x #\1) one-window)

(defun one-window ()
 (remove-dashes)
 (loop for point in *all-edit-cursors*
       do (setf (edit-cursor-window point) nil))
 (setf (edit-cursor-window *editor-cursor*)
       (create-window 0 0 *tty-width* *window-height*))
 nil)

(editor-bind-key '(#\control-x #\2) two-windows)

(defun two-windows ()
  (let* ((window (edit-cursor-window *editor-cursor*))
         (new-window (send window :split-window)))
    (put-dashes (1-& (window-stream-y-pos new-window)))
    (select-point
     (create-edit-cursor (bp-buffer *editor-cursor*)
                         (bp-line *editor-cursor*)
                         (bp-position *editor-cursor*)
                         new-window)))
  (setq re-generate-screen-really t)
  nil)

(editor-bind-key '(#\control-x #\3) view-two-windows)

(defun view-two-windows ()
  (let* ((window (edit-cursor-window *editor-cursor*))
         (new-window (send window :split-window)))
    (put-dashes (1-& (window-stream-y-pos new-window)))
    (create-edit-cursor (bp-buffer *editor-cursor*)
                        (bp-line *editor-cursor*)
                        (bp-position *editor-cursor*)
                        new-window))
  (setq re-generate-screen-really t)
  nil)

(editor-bind-key '(#\control-x #\4) visit-in-other-window)

(defun visit-in-other-window ()
  (let* ((window (edit-cursor-window *editor-cursor*))
         (new-window (send window :split-window)))
    (put-dashes (1-& (window-stream-y-pos new-window)))
    (select-point
     (create-edit-cursor (bp-buffer *editor-cursor*)
                         (bp-line *editor-cursor*)
                         (bp-position *editor-cursor*)
                         new-window))
    (find-file))
  (setq re-generate-screen-really t)
  nil)

(defun find-other-window ()
 (or (loop for point in (memq *editor-cursor* *all-edit-cursors*)
           if (and (not (eq *editor-cursor* point))
                   (not (null (edit-cursor-window point)))
                   (not (eq dash-buffer (bp-buffer point))))
           return point)
     (loop for point in *all-edit-cursors*
           if (and (not (eq *editor-cursor* point))
                   (not (null (edit-cursor-window point)))
                   (not (eq dash-buffer (bp-buffer point))))
           return point)))

(editor-bind-key '(#\control-x #\O) other-window)

(defun other-window ()
  (let ((point (find-other-window)))
    (cond ((null point)
           (select-last-buffer))
          (t (select-point point))))
  nil)

(editor-bind-key #\control-meta-v scroll-other-window)

(defun scroll-other-window ()
 (let ((point (find-other-window)))
   (when (null point) (ed-warn "There is only one window"))
   (next-screen point))
 nil)


;
;Now some of the "syntactic" commands.
;

(editor-bind-key #\meta-f forward-word)

(defun forward-word ()
  (cond ((plusp& *argument*)
         (not-buffer-end)
         (preserving-point-on-error
           (loop for i from 1 to *argument*
                 do (send *editor-cursor* :forward-over-not-word)
                    (not-buffer-end)
                    (send *editor-cursor* :forward-over-word))))
        ((zerop& *argument*)
         nil)
        (t (setq *argument* (-& *argument*))
           (backward-word)))
 nil)

(editor-bind-key #\meta-b backward-word)

(defun backward-word ()
  (cond ((plusp& *argument*)
         (not-buffer-begin)
         (preserving-point-on-error
           (loop for i from 1 to *argument*
                 do (send *editor-cursor* :backward-over-not-word)
                    (not-buffer-begin)
                    (send *editor-cursor* :backward-over-word))))
        ((zerop& *argument*)
         nil)
        (t (setq *argument* (-& *argument*))
           (forward-word)))
 nil)

(editor-bind-key #\control-meta-f forward-sexp)

(defun forward-sexp ()
  (cond ((plusp& *argument*)
         (not-buffer-end)
         (preserving-point-on-error
           (loop for i from 1 to *argument*
                 do (bp-forward-over-sexp *editor-cursor*))))
        ((zerop& *argument*)
         nil)
        (t (setq *argument* (-& *argument*))
           (backward-sexp)))
 nil)

(editor-bind-key #\control-meta-b backward-sexp)

(defun backward-sexp ()
  (cond ((plusp& *argument*)
         (not-buffer-begin)
         (preserving-point-on-error
           (loop for i from 1 to *argument*
                 do (bp-backward-over-sexp *editor-cursor*))))
        ((zerop& *argument*)
         nil)
        (t (setq *argument* (-& *argument*))
           (forward-sexp)))
 nil)

(editor-bind-key #\control-meta-\) forward-up-list)

(defun forward-up-list ()
  (not-buffer-end)
  (let ((bp (copy-bp *editor-cursor*)))
    (loop for line = (bp-line bp)
          for pos = (bp-position bp)
          do (bp-forward-over-sexp bp)
          until (char= (send bp :get-char) right-paren)
          if (and (eq (bp-line bp) line)
                  (=& (bp-position bp) pos))
          do (send bp :advance-pos 1))
    (send bp :advance-pos 1)
    (send *editor-cursor* :move (bp-line bp) (bp-position bp))
    (send bp :send-if-handles :expire))
  nil)

(editor-bind-key #\control-meta-\( backward-up-list)

(defun backward-up-list ()
  (not-buffer-begin)
  (let ((bp (copy-bp *editor-cursor*)))
    (loop for line = (bp-line bp)
          for pos = (bp-position bp)
          do (bp-backward-over-sexp bp)
          until (char= (send bp :peek-char-backward) left-paren)
          if (and (eq (bp-line bp) line)
                  (=& (bp-position bp) pos))
          do (send bp :advance-pos -1))
    (send bp :advance-pos -1)
    (send *editor-cursor* :move (bp-line bp) (bp-position bp))
    (send bp :send-if-handles  :expire))
  nil)

(editor-bind-key #\control-meta-u backward-up-list)

(editor-bind-key #\control-meta-n forward-list)

(defun forward-list ()
  (cond ((zerop *argument*)
         nil)
        ((minusp *argument*)
         (setq *argument* (- *argument*))
         (backward-list))
        (:normally
          (not-buffer-end)
          (loop repeat *argument* do
            (loop for line = (bp-line *editor-cursor*)
                  for pos = (bp-position *editor-cursor*)
                  do (send *editor-cursor* :forward-over-syntax
                           (logior& prefix-mask white-space-mask))
                  until (or (send *editor-cursor* :end-of-buffer?)
                            (char= (send *editor-cursor* :get-char)
                                   left-paren))
                  do (send *editor-cursor* :forward-over-sexp)
                  if (and (eq (bp-line *editor-cursor*) line)
                          (=& (bp-position *editor-cursor*) pos))
                  do (send *editor-cursor* :advance-pos 1))
            (or (send *editor-cursor* :end-of-buffer?)
                (send *editor-cursor* :forward-over-sexp)))
          nil)))

(editor-bind-key #\control-meta-p backward-list)

(defun backward-list ()
  (cond ((zerop *argument*)
         nil)
        ((minusp *argument*)
         (setq *argument* (- *argument*))
         (forward-list))
        (:normally
          (not-buffer-begin)
          (loop repeat *argument* do
            (loop for line = (bp-line *editor-cursor*)
                  for pos = (bp-position *editor-cursor*)
                  do (send *editor-cursor* :backward-over-syntax
                           (logior& prefix-mask white-space-mask))
                  until (or (send *editor-cursor* :beginning-of-buffer?)
                            (char= (send *editor-cursor* :peek-char-backward)
                                   right-paren))
                  do (send *editor-cursor* :backward-over-sexp)
                  if (and (eq (bp-line *editor-cursor*) line)
                          (=& (bp-position *editor-cursor*) pos))
                  do (send *editor-cursor* :advance-pos 1))
            (or (send *editor-cursor* :beginning-of-buffer?)
                (send *editor-cursor* :backward-over-sexp)))
          nil)))

(editor-bind-key #\control-meta-d down-list)

(defun down-list ()
  (not-buffer-end)
  (loop for line = (bp-line *editor-cursor*)
        for pos = (bp-position *editor-cursor*)
        do (send *editor-cursor* :forward-over-syntax
                 (logior& prefix-mask white-space-mask))
        until (or (send *editor-cursor* :end-of-buffer?)
                  (char= (send *editor-cursor* :get-char) left-paren))
        do (send *editor-cursor* :forward-over-sexp)
        if (and (eq (bp-line *editor-cursor*) line)
                (=& (bp-position *editor-cursor*) pos))
        do (send *editor-cursor* :advance-pos 1))
  (or (send *editor-cursor* :end-of-buffer?)
      (send *editor-cursor* :advance-pos 1))
  nil)



;
;sentence, paragraph and comment functions.
;

(defvar *single-space-sentence-delimiter*)

;(defun sentence-end (bp)
   ;;Waste a full CONS per character! YOW!
;  (let ((mark (send bp :get-mark)))
;    (prog1 (and (memq (send bp :get-char-forward) '(#\. #\? #\!))
;               (loop while (memq (send bp :get-char-forward) '(#\) #\] #\' #\"))
;                     finally
;                     (return
;                      (or (memq (send bp :peek-char-backward)
;                                '(#\return #\page #\tab))
;                          (and (char= (send bp :peek-char-backward) #\space)
;                               (or *single-space-sentence-delimiter*
;                                   (char= (send bp :get-char) #\space)))))))
;          (send bp :goto-mark mark))))

(defun sentence-end (bp)
  (let* ((line (bp-line bp))
         (chars (line-chars line))
         (max (line-char-count line))
         (pos (bp-position bp)))
    (and (<& pos max)
         (memq (schar chars pos) '(#\. #\? #\!))
         (loop do (setq pos (1+& pos))
               until (>=& pos max)
               while (memq (schar chars pos) '(#\) #\] #\' #\"))
               finally (return t))
         (or (>=& pos max)
             (char= (schar chars pos) #\space))
         (or *single-space-sentence-delimiter*
             (>= (setq pos (1+& pos)) max)
             (char= (schar chars pos) #\space)))))

(defun forward-sentence (&optional (bp *editor-cursor*))
  (cond ((-p *argument*)
         (let ((*argument* (-& *argument*)))
           (backward-sentence bp)))
        (t (not-buffer-end bp)
           (loop repeat *argument*
                 do (loop do (send bp :advance-pos 1)
                          until (and (=& (bp-position bp)
                                         (1-& (line-char-count (bp-line bp))))
                                     (paragraph-last-line (bp-line bp)))
                          until (sentence-end bp))
                 do (send bp :advance-pos 1))))
  nil)

;;; Fixed to leave point in the right place

(defun backward-sentence (&optional (bp *editor-cursor*))
  (cond ((-p *argument*)
         (let ((*argument* (-& *argument*)))
           (forward-sentence bp)))
        (t (not-buffer-begin bp)
           (send bp :backward-over-not-word)
           (loop repeat *argument*
                 do (loop do (send bp :advance-pos -1)
                          until (and (0p (bp-position bp))
                                     (paragraph-first-line (bp-line bp)))
                          until (sentence-end bp))
                 )
           (send bp :forward-over-not-word)))
  nil)

(editor-bind-key #\meta-a backward-sentence)
(editor-bind-key #\meta-e forward-sentence)

(editor-bind-key #\meta-k kill-sentence)

(defun kill-sentence ()
  (let ((bp (copy-bp *editor-cursor*)))
    (forward-sentence bp)
    (push-mark-no-copy bp)
    (kill-region)
    (send bp :send-if-handles :expire)
    nil))

(editor-bind-key '(#\control-x #\rubout) backward-kill-sentence)

(defun backward-kill-sentence ()
  (let ((bp (copy-bp *editor-cursor*)))
    (backward-sentence bp)
    (push-mark-no-copy bp)
    (kill-region)
    (send bp :send-if-handles :expire)
    nil))

;
;Paragraphs.
;We do not have search patterns, so we have to make do with something
;weaker.
;
;We will allow paragraphs to be delimited by either a line with leading
;white-space (for text) or a blank line, for program modes.
;

(defvar *paragraph-text-style*)

;(defun paragraph-line (line)
;  (or (empty-line? line)
;      (null (line-previous line))
;      (and *paragraph-text-style*
;          (or (null (line-next line))
;              (0p (line-char-count (line-next line)))
;              (white-space? (char (line-chars (line-next line)) 0))))))

(defun paragraph-first-line (line)
  (and (not (empty-line? line))
       (or (null (line-previous line))
           (empty-line? (line-previous line))
           (and *paragraph-text-style*
                (white-space? (schar (line-chars line) 0)))
           (char= (schar (line-chars line) 0) #\page)
           (and (char= (schar (line-chars (line-previous line)) 0) #\page)
                (loop for pos from 1
                      below (line-char-count (line-previous line))
                      for char = (schar (line-chars (line-previous line)) pos)
                      always (white-space? char))))))

(defun paragraph-last-line (line)
  (and (not (empty-line? line))
       (or (null (line-next line))
           (empty-line? (line-next line))
           (and *paragraph-text-style*
                (white-space? (schar (line-chars (line-next line)) 0))))))

(editor-bind-key #\meta-h mark-paragraph)

(defun mark-paragraph ()
  (let ((mark (copy-bp *editor-cursor*)))
    (push-mark *editor-cursor*)
    (let ((*argument* (if (-p *argument*) 1 *argument*)))
      (forward-paragraph mark))
    (let ((*argument* (if (-p *argument*) *argument* 1)))
      (backward-paragraph *editor-cursor*))
    (push-mark-no-copy mark)
    nil))

(defun forward-paragraph (&optional (bp *editor-cursor*))
  (cond ((-p *argument*)
         (let ((*argument* (-& *argument*)))
           (backward-paragraph bp)))
        (t (not-buffer-end)
           (loop repeat *argument*
                 ;;Move over blank lines.
                 do (loop for line first (bp-line bp) then next
                          for next = (line-next line)
                          while (and (not (null next)) (empty-line? line))
                          finally (send bp :move line 0))
                 ;;Search for end of paragraph.
                 do (loop for line first (bp-line bp) then next
                          for next = (line-next line)
                          until (paragraph-last-line line)
                          finally (if (null next)
                                      (send bp :move
                                            line (line-char-count line))
                                      (send bp :move next 0)))))))

(defun backward-paragraph (&optional (bp *editor-cursor*))
  (cond ((-p *argument*)
         (let ((*argument* (-& *argument*)))
           (forward-paragraph bp)))
        (t (not-buffer-begin)
           (loop repeat *argument*
                 ;;Skip blank lines.
                 if (and (0p (bp-position bp)) (line-previous (bp-line bp)))
                 do (send bp :to-end-of-previous-line)
                 do (loop for line first (bp-line bp) then prev
                          for prev = (line-previous line)
                          while (and (not (null prev)) (empty-line? line))
                          finally (send bp :move line 0))
                 ;;Now search for paragraph beginning.
                 do (loop for line first (bp-line bp) then prev
                          for prev = (line-previous line)
                          until (paragraph-first-line line)
                          finally (send bp :move line 0))))))

(editor-bind-key #\meta-] forward-paragraph)

(editor-bind-key #\meta-[ backward-paragraph)



;The mark ring.

;Now the emacs mark ring commands.
(editor-bind-key #\control-@ set-pop-mark)

(defun set-pop-mark ()
 (cond ((and (null argument-supplied?) (=& *argument* 1))
          (push-mark *editor-cursor*))
       ((=& *argument* 4) (goto-mark (pop-mark)))
       ((>=& *argument* 16) (pop-mark)))
 nil)

(editor-bind-key #\control-space set-pop-mark)

(editor-bind-key '(#\control-x #\control-x) exchange-point-and-mark)

(defun exchange-point-and-mark ()
 (goto-mark (prog1 (pop-mark)
                   (push-mark *editor-cursor*)))
 nil)

(editor-bind-key #\control-< mark-beginning)

(defun mark-beginning ()
  (push-mark-no-copy (make-bp *editor-buffer*
                              (buffer-content *editor-buffer*)
                              0))
  nil)

(editor-bind-key #\control-> mark-end)

(defun mark-end ()
  (let ((line (last-line)))
    (push-mark-no-copy (make-bp *editor-buffer* line (line-char-count line))))
  nil)

(editor-bind-key #\meta-@ mark-word)

(defun mark-word ()
  (let ((mark (copy-bp *editor-cursor*)))
    (loop for i from 0 below *argument*
          do (send mark :forward-over-not-word)
          do (send mark :forward-over-word))
    (push-mark-no-copy mark)
    nil))

(editor-bind-key #\control-meta-h mark-defun)

(defun mark-defun ()
  (find-beginning-of-defun *editor-cursor*)
  (bp-forward-over-sexp *editor-cursor*)
  (push-mark-no-copy (make-bp *editor-buffer*
                              (bp-line *editor-cursor*)
                              (bp-position *editor-cursor*)))
  (bp-backward-over-sexp *editor-cursor*)
  nil)

(editor-bind-key #\control-meta-backspace mark-defun)

(editor-bind-key #\meta-backspace mark-defun)

(editor-bind-key '(#\control-x #\h) mark-whole-buffer)

(defun mark-whole-buffer ()
  (mark-end)
  (send *editor-cursor* :move (buffer-content *editor-buffer*) 0)
  nil)

(editor-bind-key '(#\control-x #\control-p) mark-page)

(defun mark-page ()
  (let ((bp (copy-bp *editor-cursor*)))
    (send *editor-cursor* :set-pos 0)
    (send *editor-cursor* :backward-to-char #\page)
    (send bp :set-pos (line-char-count (bp-line bp)))
    (send bp :forward-to-char #\page)
    (unless (buffer-end? bp)
      (send bp :advance-pos 1))
    (push-mark-no-copy bp))
  nil)

;;; This is broken (still?)

(editor-bind-key '(#\control-x #\[) previous-page)

(defun previous-page ()
  (cond ((plusp& *argument*)
         (not-buffer-begin)
         (loop repeat *argument*
               do (send *editor-cursor* :advance-pos -1)
                  (send *editor-cursor* :backward-to-char #\page)))
        ((zerop& *argument*) nil)
        (t (setq *argument* (-& *argument*))
           (next-page)))
  nil)

(editor-bind-key '(#\control-x #\]) next-page)

(defun next-page ()
  (cond ((plusp& *argument*)
         (not-buffer-end)
         (loop repeat *argument*
               do (send *editor-cursor* :forward-to-char #\page)
                  (send *editor-cursor* :advance-pos 1)))
        ((zerop& *argument*) nil)
        (t (setq *argument* (-& *argument*))
           (previous-page)))
 nil)



;More syntactic commands.

(editor-bind-key #\control-meta-a beginning-of-defun)

(defun beginning-of-defun (&optional (bp *editor-cursor*))
  (cond ((-p *argument*)
         (setq *argument* (-& *argument*))
         (end-of-defun bp))
        (t (not-buffer-begin bp)
           (loop for i from 1 to *argument*
                 do (send bp :advance-pos -1) ;always at least one char.
                 do (find-beginning-of-defun bp))))
 nil)

(editor-bind-key #\control-meta-[ beginning-of-defun)

(defun find-beginning-of-defun (point)
  (do ((line (bp-line point) (line-previous line))
       (chars))
      ((null line))
    (send point :move line 0)
    (setq chars (line-chars line))
    (when (and (+p (line-char-count line))
               (char= (schar chars 0) left-paren))
      (return t))))

(editor-bind-key #\control-meta-e end-of-defun)

;;; This is wrong: seems to only search for an empty line

(defun end-of-defun (&optional (bp *editor-cursor*))
 (cond ((-p *argument*)
        (let ((*argument* (-& *argument*)))
         (beginning-of-defun bp)))
       (t (not-buffer-end bp)
          (loop repeat *argument*
                do (loop for line first (bp-line bp) then next
                         for next = (line-next line)
                         while (and next (empty-line? line))
                         finally (send bp :move line (line-char-count line)))
                do (loop for line first (bp-line bp) then next
                         for next = (line-next line)
                         while (and next (null (empty-line? line)))
                         finally (send bp :move
                                       line (line-char-count line)))))))

(editor-bind-key #\control-meta-] end-of-defun)

(editor-bind-key #\meta-\( make-parens)

(defun make-parens ()
  (self-insert left-paren 1)
  (preserving-point
    (when argument-supplied?
      (forward-sexp))
    (let ((*display-matching-paren* 0))
      (self-insert right-paren 1)))
  nil)

(editor-bind-key #\control-t transpose-characters)

(defun transpose-characters ()
  (cond ((and (not (argument?)) (send *editor-cursor* :end-of-line?))
           ;Twiddle the two characters before the cursor.
           (send *editor-cursor* :advance-pos -2)
           (let ((char (send *editor-cursor* :get-char)))
             (send *editor-cursor* :delete-char)
             (send *editor-cursor* :advance-pos 1)
             (send *editor-cursor* :insert-char char)))
        (t (send *editor-cursor* :advance-pos -1)
           (let ((char (send *editor-cursor* :get-char)))
             (send *editor-cursor* :delete-char)
             (send *editor-cursor* :advance-pos *argument*)
             (send *editor-cursor* :insert-char char)
             )))
 nil)

(editor-bind-key '(#\control-x #\t) transpose-regions)

(defun transpose-regions ()
  (setq *last-kill-command-count* -1000)
  (let ((mark (get-mark))
        (carcass))
    (kill-region)
    (setq carcass (pop-kill))
    (goto-mark (pop-mark))
    (kill-region)
    (un-kill carcass)
    (goto-mark mark)
    (un-kill)
    (pop-kill)
    nil))

;;; Finally, a generic twiddler   2/83 -- Tim

(defun exchange-subr (motion-fn n)
  (cond ((plusp& n)
           (exchange-subr-+n motion-fn n))
        ((minusp& n)
           (exchange-subr--n motion-fn n))
        (:zerop&
           (exchange-subr-0 motion-fn n)))
  nil)

;;; For 0 argument: exchange at point & mark

(defun exchange-subr-0 (motion-fn n)
  (region (bp1 bp2)
    (with-bp* ((bp1l (funcall-for-bp motion-fn -1
                                     (funcall-for-bp motion-fn 1 bp1)))
               (bp2r (funcall-for-bp motion-fn 1 bp2))
               (bp2l (funcall-for-bp motion-fn -1 bp2r))
               (bp1r (funcall-for-bp motion-fn 1 bp1l)))
      (let ((buf2 (copy-interval bp2l bp2r t)))
        (cond ((equal bp1r bp2l)
                 (delete-interval bp2l bp2r t)
                 (move-bp *editor-cursor* (insert-interval bp1l buf2))
                 (move-bp (mark) bp2r))
              (:normally
                 (let ((buf1 (copy-interval bp1l bp1r t)))
                   (delete-interval bp2l bp2r t)
                   (delete-interval bp1l bp1r t)
                   (move-bp *editor-cursor* (insert-interval bp2l buf1))
                   (move-bp (mark) (insert-interval bp1r buf2)))))))))

;;; For positive args: twiddle and move past

(defun exchange-subr-+n (motion-fn n)
  (move-bp *editor-cursor*
           (funcall-for-bp motion-fn 1
                           (funcall-for-bp motion-fn -2
                                           (funcall-for-bp motion-fn 1
                                                           *editor-cursor*))))
  (dotimes (i n)
    (with-bp* ((bp1r (make-temp-bp *editor-cursor*))
               (bp2r (funcall-for-bp motion-fn 1 bp1r))
               (bp2l (funcall-for-bp motion-fn -1 bp2r))
               (bp1l (funcall-for-bp motion-fn -1 bp1r)))
      (let ((buf2 (copy-interval bp2l bp2r t)))
        (cond ((equal bp1r bp2l)
                 (delete-interval bp2l bp2r t)
                 (insert-interval bp1l buf2)
                 (move-bp *editor-cursor* bp2r))
              (:normally
                 (let ((buf1 (copy-interval bp1l bp1r t)))
                   (delete-interval bp2l bp2r t)
                   (delete-interval bp1l bp1r t)
                   (insert-interval bp1r buf2)
                   (move-bp *editor-cursor* (insert-interval bp2l buf1)))))))))

;;; For negative args: move back in between then twiddle

(defun exchange-subr--n (motion-fn n)
  (move-bp *editor-cursor*
           (funcall-for-bp motion-fn 1
                           (funcall-for-bp motion-fn -1 *editor-cursor*)))
  (do ((i 0 (1- i))) ((<= i n))
    (with-bp* ((bp2r (make-temp-bp *editor-cursor*))
               (bp1l (funcall-for-bp motion-fn -2 bp2r))
               (bp1r (funcall-for-bp motion-fn 1 bp1l))
               (bp2l (funcall-for-bp motion-fn -1 bp2r)))
      (let ((buf2 (copy-interval bp2l bp2r t)))
        (cond ((equal bp1r bp2l)
                 (delete-interval bp2l bp2r t)
                 (move-bp *editor-cursor*
                          (insert-interval bp1l buf2)))
              (:normally
                 (let ((buf1 (copy-interval bp1l bp1r t)))
                   (delete-interval bp2l bp2r t)
                   (delete-interval bp1l bp1r t)
                   (insert-interval bp2l buf1)
                   (move-bp *editor-cursor* (insert-interval bp1r buf2)))))))))

;;; Now we can describe these in terms of the above functions

(defun transpose-words ()
  (exchange-subr #'forward-word *argument*))

(defun transpose-sexps ()
  (exchange-subr #'forward-sexp *argument*))

(defun transpose-lines ()
  (exchange-subr #'forward-line *argument*))

(editor-bind-key '(#\control-x #\control-t) transpose-lines)

(editor-bind-key '#\meta-t transpose-words)

(editor-bind-key '#\control-meta-t transpose-sexps)

;More random stuff.

(editor-bind-key #\meta-m back-to-indentation)

(defun back-to-indentation ()
  (send *editor-cursor* :set-pos 0)
  (loop while (not (send *editor-cursor* :end-of-line?))
        for chr = (send *editor-cursor* :get-char-forward)
        do (when (not (horizontal-white-space? chr))
             (send *editor-cursor* :advance-pos -1)
             (return)))
  nil)

(editor-bind-key #\meta-return back-to-indentation)
(editor-bind-key #\control-meta-m back-to-indentation)

(editor-bind-key #\meta-^ delete-indentation)

(defun delete-indentation ()
  (send *editor-cursor* :set-pos 0)
  (send (bp-line *editor-cursor*) :delete-line-separator)
  (delete-horizontal-space)
  (unless (or (char= right-paren (send *editor-cursor* :get-char))
              (char= left-paren (send *editor-cursor* :peek-char-backward)))
   (self-insert #\space 1))
  nil)

(editor-bind-key #\control-meta-^ delete-indentation)

(editor-bind-key #\control-meta-o split-line)

(defun split-line ()
  (send *editor-cursor* :forward-over-horizontal-white-space)
  (multiple-value-bind (xpos ypos)
        (edit-cursor-position-really *editor-cursor*)
    (send (bp-line *editor-cursor*) :break (bp-position *editor-cursor*))
    (send *editor-cursor* :to-end-of-previous-line) ;Fix for new :break.
    (indent-line-to-column (line-next (bp-line *editor-cursor*)) xpos)
    (loop with line = (line-next (bp-line *editor-cursor*))
          for i from 1 below *argument* ;Skip first iteration.
          do (send line :break 0))
    nil))

(editor-bind-key #\meta-s center-line)

(defun center-line ()
  (let ((bp (copy-bp *editor-cursor*))
        (x-size (window-stream-x-size (edit-cursor-window *editor-cursor*))))
    (loop for i from 0 below (abs& *argument*)
          do (send bp :set-pos 0)
          do (delete-horizontal-space bp)
          do (send bp :set-pos (line-char-count (bp-line bp)))
          do (send bp :backward-over-horizontal-white-space)
          do (indent-line-to-column (bp-line bp)
                                    (max& 0 (/& (-& x-size
                                                    (bp-position bp)) 2)))
          do (and (line-next (bp-line bp))
                  (send bp :to-beginning-of-next-line)))
    (when (or argument-supplied? (not (=& 1 *argument*)))
          (send *editor-cursor* :move (bp-line bp) (bp-position bp)))
    (send bp :send-if-handles :expire)
    nil))

(editor-bind-key #\control-meta-@ mark-sexp)

(defun mark-sexp ()
  (let ((mark (make-bp *editor-buffer*
                       (bp-line *editor-cursor*)
                       (bp-position *editor-cursor*))))
    (cond ((=& *argument* 0))
          ((>& *argument* 0)
             (not-buffer-end)
             (loop for i from 1 to *argument* do (bp-forward-over-sexp mark)))
          (t (not-buffer-begin)
             (loop for i from 1 to (- *argument*)
                   do (bp-backward-over-sexp mark))))
    (push-mark-no-copy mark)
    nil))

(defun lines-in-sexp (point &aux (bp (copy-bp point)))
 (bp-forward-over-sexp bp)
 (count-lines-between point bp))

(editor-bind-key #\control-meta-q indent-sexp)

(defun indent-sexp ()
  (not-buffer-end)
  (loop with bp = (copy-bp *editor-cursor*)
        for line first (line-next (bp-line *editor-cursor*))
        then (line-next line)
        for i from 1 below (lines-in-sexp *editor-cursor*)
        do (send bp :move line 0)
        do (lisp-indentor (copy-bp bp))))

(editor-bind-key #\control-meta-g indent-sexp)

(editor-bind-key #\control-meta-z exit-editing-level)

(defun exit-editing-level ()
  (throw 'exit-editor t))

(editor-bind-key '(#\control-x #\control-z) return-to-superior)

(defun return-to-superior ()
  (with-no-passall
    (send terminal-io :set-device-mode :ttysync *terminal-ttsync-on-exit*)
    (setq steve:creamed-tty-lines-to steve:*tty-height*)
    (editor-destroy-mode-line-for-exiting)
    (valret)
    (send terminal-io :set-device-mode :ttysync nil)
    (cursorpos 'c terminal-io))
  nil)


(editor-bind-key #\control-altmode exit-editing-level)

(editor-bind-key #\control-C not-in-this-editor-you-dont)

(defun not-in-this-editor-you-dont ()
  (ed-warn "Control-C is reserved for future use..."))

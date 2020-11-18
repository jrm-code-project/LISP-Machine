;-*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;New redisplay scheme.
;
;This will still use a screen image, but only one.
;

(defvar old-screen-image nil)

(defconstant line-not-modified 0) ;Also defined in EM.LSP
(defconstant *tab-spacing* 8)
(defvar creamed-tty-lines-to 0)
(defvar *overwrite-line*) ;Used by functions in OVER.LSP
(defvar re-generate-screen-really nil)

(defvar *window*)
(defvar *win-x-pos*)
(defvar *win-y-pos*)
(defvar *win-x-size*)
(defvar *win-y-size*)
(defvar *win-y-limit*)

;
;; These variables are vectors indexed by the physical tty line #.
;;  The buffer-line on this tty line.
(defvar *line-map*)
;;  The first buffer-line-index on this tty-line.
(defvar *p1-map*)
;;  The last buffer-line-index on this tty-line.
(defvar *p2-map*)
;

(defvar *editor-redisplay-count* 0)

;;; Assume the various TTY operations, and then notice when they fail.
;;; Be carefull to handle the case when they fail somehow, even
;;; if this means redisplaying the whole screen. This will happen
;;; only once per editing session anyhow.
(defvar ins&del-line-p t)
(defvar ins&del-char-p t)

(defun clear-one-screen-image (image)
  (loop for i from 0 below *tty-height*
        do (%string-replace (vref image i) 80spaces 0 0 *tty-width*)))

(defun init-tty-for-editor ()
  (let ((h (send terminal-io :pagel))
        (w (send terminal-io :linel)))
    (when (or (not (=& h *tty-height*))
              (not (=& w *tty-width*))
              (not (boundp '*line-map*))
              (null *line-map*)
              (not (null *tty-init-flag*)))
      (setq old-screen-image (make-vector h))
      (loop for i from 0 below h
            do (setf (svref old-screen-image i)
                     (make-string w :initial-element #\sp)))
      (setq *line-map* (make-vector h)) ;Initial values should not be critical.
      (setq *p1-map* (make-vector h :initial-element 0))
      (setq *p2-map* (make-vector h :initial-element (1-& w)))
      (setq *tty-height* h)
      (setq *tty-width* w)
      (unless (null *editor-cursor*)
        ;;Has some side effects but that can't be helped.
        (one-window)))
    (setq *tty-height* h)
    (setq *tty-width* w)
    (setq ins&del-line-p t)
    (setq ins&del-char-p t)))

(defun make-screen-image (&optional unused)
  (unless (type-ahead-p)
    (when (+p creamed-tty-lines-to)
      (setq *overwrite-line* 0)
      (cond ((>& creamed-tty-lines-to *first-mode-line*)
             (send terminal-io :clear-screen)
             (setup-mode-area)
             (loop for i from 0 below *tty-height*
                   do (setf (svref *line-map* i) nil)
                   do (%string-replace (svref old-screen-image i) 80spaces
                                       0 0 *tty-width*)))
            (t (loop for i from 0 below creamed-tty-lines-to
                     do (cursorpos i 0 terminal-io)
                     do (send terminal-io :clear-eol)
                     do (setf (svref *line-map* i) nil)
                     do (%string-replace (svref old-screen-image i) 80spaces
                                         0 0 *tty-width*))))
      (setq creamed-tty-lines-to 0))
    (when re-generate-screen-really
      (loop for i from 0 below *tty-height*
            do (setf (svref *line-map* i) nil))
      (setq re-generate-screen-really nil))
    (catch 'listen-tag
      (loop for curse in *all-edit-cursors*
            for *window* = (edit-cursor-window curse)
            with *win-x-size* = 0
            with *win-y-size* = 0
            with *win-y-pos* = 0
            with *win-x-pos* = 0
            with *win-y-limit* = 0
            if (not (null (edit-cursor-window curse)))
            do (progn (setq *win-x-size* (window-stream-x-size *window*)
                            *win-y-size* (window-stream-y-size *window*)
                            *win-x-pos* (window-stream-x-pos *window*)
                            *win-y-pos* (window-stream-y-pos *window*)
                            *win-y-limit* (+& *win-y-pos* *win-y-size*))
                      (when (not (point-on-screen? curse))
                        (auto-recenter-window curse))
                      (redisplay-buffer-lines curse))
            finally (setq *editor-redisplay-count*
                          (1+& *editor-redisplay-count*))
            finally (return t)))))

(defsubst line-height* (len1 x-size)
  (let ((len len1))
    (if (0p len)
        1
        (1+& (/& (1-& len) (1-& x-size))))))

(defun aprox-line-height (line x-size)
  (line-height* (if (and (+p (line-graphic-length line))
                         (line-not-bashed? line))
                    (line-graphic-length line)
                    (line-char-count line))
                x-size))

;This function is a heuristic.  I used to think that it was accurate, and
;I believe there are bugs because of this.  All use of this function should
;be scrutinized.  This function estimates the width of a line by its character
;count.  If a line has special characters in it then this will be low, so
;this function is accurate when it claims that the cursor is not on the screen
;but may be wrong when it claims the cursor IS on the screen. (This is probably
;the less useful heuristic.)
(defun point-on-screen? (point)
  (setq *window* (edit-cursor-window point)
        *win-x-size* (window-x-size *window*)
        *win-y-size* (window-y-size *window*))
  (cond ((eq (edit-cursor-home-line point) (bp-line point))
         (and (>=& (bp-position point) (edit-cursor-home-pos point))
              (<=& (line-height* (-& (bp-position point)
                                    (edit-cursor-home-pos point))
                                *win-x-size*)
                  *win-y-size*)))
        (t (loop with lines = 0
                 with target = (bp-line point)
                 for line first (edit-cursor-home-line point)
                 then (line-next line)
                 while line
                 for start first (edit-cursor-home-pos point) then 0
                 for l = (cond ((eq line target) ;Start must = 0
                                (bp-position point))
                               ((and (line-not-bashed? line)
                                     (not (-p (line-graphic-length line)))
                                     (0p start))
                                (line-graphic-length line))
                               (t (-& (line-char-count line) start)))
                 summing (line-height* l *win-x-size*) into height
                 if (eq line target)
                 return (<=& height *win-y-size*)
                 while (<=& lines *win-y-size*)))))

;This returns the length of the graphic representation of a character.
;For most characters this is 1, but for special characters it may be longer.
;For example a <BELL> prints as #\BELL using 6 columns.
;A tab uses a variable number of columns, depending upon where it is.
;The second argument is the real column in which the character will be printed.
;See GRAPHIC-CHAR-LENGTH-CALC for more imformation.
(defsubst graphic-char-length (char col)
   (if (graphic-char-p char)
      1
      (simple-string-length (stringify-char char col))))

(defun position-from-graphic-position (line graphic-pos
                                       &aux (line-string (line-chars line))
                                       (line-length (line-char-count line)))
  (do ((i 0 (1+& i))
       (chr)
       (so-far 0))
      ((or (>=& i line-length) (>=& so-far graphic-pos)) i)
      (setq chr (char line-string i)
            so-far (+& so-far (graphic-char-length chr so-far)))))

;This function is a good place to look for caching optimizations.
(defun position-really (line pos x-size  &optional (skip 0)
                             &aux (line-string (line-chars line)))
  ;;Values are relative X, Y.
  (loop with so-far = 0
        with ypos = 0
        for i from skip below pos
        for chr = (char line-string i)
        do (setq so-far (+& so-far (graphic-char-length chr so-far)))
        if (>=& so-far x-size)
        do (setq so-far (-& so-far x-size -1)
                 ypos (1+& ypos))
        finally (return (values so-far ypos))))

(defsubst edit-cursor-position-really (point)
  (position-really (bp-line point)
                   (bp-position point)
                   (window-stream-x-size (edit-cursor-window point))))

(defun real-line-height (line start end x-size)
  (line-height* (real-line-width line start end) x-size))

(defun real-line-width (line start end)
  (loop with chars = (line-chars line)
        with column = 0
        for i from start below end
        for char = (char chars i)
        for char-width = (graphic-char-length char column)
        do (setq column (+& column char-width))
        finally (return column)))

;;; This must be made to work perfectly, even if slow.
(defun auto-recenter-window (point)
  (setq *window* (edit-cursor-window point)
        *win-y-size* (window-stream-y-size *window*)
        *win-x-size* (window-stream-x-size *window*))
  ;;(ed-warning "%Recentering window")
  (loop with target = (ceiling (* *win-y-size* *recentering-fraction*))
        with lines = (real-line-height (bp-line point)
                                       0
                                       (bp-position point)
                                       *win-x-size*)
        for line first (bp-line point) then previous
        for previous = (line-previous line)
        for height = (if previous
                         (real-line-height previous
                                           0
                                           (line-char-count previous)
                                           *win-x-size*)
                         *win-y-size*)
        if (>& (+& lines height) target)
        return
        (cond ((<=& lines *win-y-size*)
               (setf (edit-cursor-home-pos point) 0)
               (setf (edit-cursor-home-line point) line)
               t)
              ;; First iteration only. Home-pos must be > 0.
              ((not (eq line (bp-line point)))
               (break "Implementation bug in recentering")
               (setf (edit-cursor-home-line point) (bp-line point))
               (setf (edit-cursor-home-pos point) (bp-position point)))
              (t (setf (edit-cursor-home-line point) line)
                 (setf (edit-cursor-home-pos point)
                       (-& (bp-position point)
                           (*& target (1-& *win-x-size*))))
                 t))
        do (setq lines (+& lines height))))

(defsubst page-break? (line)
  (%string-posq #\page (line-chars line) 0 (line-char-count line)))

(defun redisplay-buffer-lines (point)
  (when (and (eq (bp-line point) (edit-cursor-home-line point))
             (>& (edit-cursor-home-pos point) (bp-position point)))
    (auto-recenter-window point))
  (loop with cursor-on-screen = nil
        with cline = (bp-line point)
        ;;Y is absolute now.
        with y = *win-y-pos*
        ;;for y first *win-y-pos*
        ;;then (+& (line-height* (line-graphic-length line) *win-x-size*) y)
        for line first (edit-cursor-home-line point) then (line-next line)
        for x-start first (edit-cursor-home-pos point) then 0
        if (null line) return (clear-rest-of-window y)
        if (eq line cline) ;Find real cursor position.
        do (multiple-value-bind (xpos ypos)
             (position-really line (bp-position point) *win-x-size* x-start)
             (when (<& (+& ypos y) *win-y-limit*)
               (setq cursor-on-screen t))
             (cursorpos (-& (+& y ypos) *win-y-pos*) xpos *window*))
        do (setq y (+& y (expand-line line y point x-start)))
        while (<& y *win-y-limit*)
        finally (when (null cursor-on-screen)
                  (auto-recenter-window point)
                  (redisplay-buffer-lines point))))

(defun real-x-y-position (bp)
  ;; Used by underline feature.
  (loop with window = (edit-cursor-window *editor-cursor*)
        with x-size = (window-stream-x-size window)
        for line first (edit-cursor-home-line *editor-cursor*)
        then (line-next line)
        while line
        for y first (window-y-pos window)
        then (+& y (line-height* (line-graphic-length (line-previous line))
                                 x-size))
        while (<=& y *tty-height*)
        if (eq line (bp-line bp))
        return (multiple-value-bind (xpos ypos)
                 (position-really line (bp-position bp) x-size)
                 (values xpos (+& ypos y)))))

(defun clear-rest-of-window (y)
  ;;Y is absolute here.
  (loop for i from y below *win-y-limit*
        do (setf (svref *line-map* i) nil)
        unless (%string-eqv (svref old-screen-image i) 80spaces
                            *win-x-pos* 0 *win-x-size*)
        do (progn (%string-replace (svref old-screen-image i) 80spaces
                                   *win-x-pos* 0 *win-x-size*)
                  (cursorpos i *win-x-pos* terminal-io)
                  (send terminal-io :clear-eol))))

(defun i&d-line-too-slow-p (nlines)
  (< (* 40 (- *win-y-size* nlines))
     (* nlines (send terminal-io :insert-line-cost 3))))

;The basic function to display buffer lines.

(defun expand-line (line y point x-start &aux i&d-line-too-slow-flag)
 ;;The line is a buffer line.
 ;;Y is the screen line where the line should be displayed.
 ;;point contains the window and its line-map.
 ;;The VALUE must be a fixnum count of the number of tty lines used
 ;;to display the buffer line. (>= 1)
 (cond ((and (line-not-bashed? line)
             (eq line (svref *line-map* y))
             ;;We must make sure that the WHOLE line is displayed correctly.
             (loop for i upfrom y
                   for first first x-start then (1+& last)
                   for last = (svref *p2-map* i)
                   never (not (=& (svref *p1-map* i) first))
                   if (or (=& last (line-char-count line))
                          (>=& (1+& i) *win-y-limit*))
                   ;;The line is already displayed correctly.
                   return (1+& (-& i y)))))
       ;; Ins&del-line-p is set to NIL if operation fails.
       ((null ins&del-line-p) (just-expand-line line y point x-start))
       ;; Here we should handle the cases for insert-line and delete-line.
       ;; This is the check for deleted lines.
       ((and (or (not (eq line (svref *line-map* y)))
                 (not (0p (svref *p1-map* y))))
             (loop for k from y below *win-y-limit*
                   if (and (eq (svref *line-map* k) line)
                           (0p (svref *p1-map* k)))
                   return (let ((nlines (-& k y)))
                            (unless (i&d-line-too-slow-p nlines)
                              (delete-old-lines y nlines)))))
        (just-expand-line line y point x-start))
       ;; Check here for possible inserted lines.
       ((and (not (eq line (svref *line-map* y)))
             (0p (svref *p1-map* y))
             (loop with line** = (svref *line-map* y)
                   for k from y below *win-y-limit*
                   for line* first line then (line-next line*)
                   while (not (null line*))
                   if (eq line** line*)
                   return (let ((nlines (-& k y)))
                            (unless (i&d-line-too-slow-p nlines)
                              (insert-new-lines y nlines)))))
        (just-expand-line line y point x-start))
       (t (just-expand-line line y point x-start))))

;;;These have to modify the maps.
(defun insert-tty-lines (line n)
  (cursorpos line 0 terminal-io)
  (cond ((null (send terminal-io :send-if-handles :insert-line n))
         (setq ins&del-line-p nil)
         nil)
        (t (loop for i from 1 to n      ;Shift up.
                 for k = (-& *tty-height* i)
                 for hold = (svref old-screen-image k)
                 do (loop for j downfrom k by n
                          while (>=& j (+& line n))
                          do (move-screen-image-line (-& j n) j)
                          finally (%string-replace hold 80spaces
                                                   0 0 *tty-width*)
                          finally (setf (svref old-screen-image j) hold
                                        (svref *line-map* j) nil
                                        (svref *p1-map* j) 0
                                        (svref *p2-map* j) 0)))
           t)))

(defun delete-tty-lines (line n)
  (cursorpos line 0 terminal-io)
  (cond ((null (send terminal-io :send-if-handles :delete-line n))
         (setq ins&del-line-p nil)
         nil)
        (t (loop for i from 0 below n
                 for k = (+& line i)
                 for hold = (svref old-screen-image k)
                 do (loop for j upfrom k by n
                          while (<& j (-& *tty-height* n))
                          do (move-screen-image-line (+& j n) j)
                          finally (%string-replace hold 80spaces
                                                   0 0 *tty-width*)
                          finally (setf (svref old-screen-image j) hold
                                        (svref *line-map* j) nil
                                        (svref *p1-map* j) 0
                                        (svref *p2-map* j) 0)))
           t)))

(defun move-screen-image-line (from to)
  (setf (svref old-screen-image to) (svref old-screen-image from))
  (setf (svref *line-map* to) (svref *line-map* from))
  (setf (svref *p1-map* to) (svref *p1-map* from))
  (setf (svref *p2-map* to) (svref *p2-map* from)))

(defun delete-old-lines (ypos add)
  (and (delete-tty-lines ypos add)
       (insert-tty-lines (-& (+& *win-y-pos* *win-y-size*) add) add)))

(defun insert-new-lines (ypos add)
  (and (delete-tty-lines (-& (+& *win-y-pos* *win-y-size*) add) add)
       (insert-tty-lines ypos add)))

;;This function and macro are purely auxillaries for just-expand-line.
(defun redisplay-cont-line (point change1 change2 y
                            &aux (x-max (+& *win-x-pos* *win-x-size* -1)))
  (when (<& y *win-y-limit*)
    (let ((screen-line (svref old-screen-image y)))
      (unless (char= (char screen-line x-max) #\!)
        (setf (char screen-line x-max) #\!)
        (setq change1 (min& change1 x-max)
              change2 x-max))
      (when (and (<=& change1 change2) ;output line to TTY.
                 (<& y *win-y-limit*))
        (cursorpos y change1 terminal-io)
        (send terminal-io :raw-oustr screen-line
              change1 (1+& (-& x-max change1)))
        (send terminal-io :set-cursor-fried-flag t)))))

;;Note that this macro is used to obtain call by reference.
(defmacro update-tty-map (y line p1 p2)
  `(progn (when (<& y *win-y-limit*)
            (setf (svref *line-map* ,y) ,line)
            (setf (svref *p1-map* ,y) ,p1)
            (setf (svref *p2-map* ,y) ,p2))
          (setf ,p1 (1+& ,p2))))

;;Note that this macro is used to obtain call by reference.
(defmacro cont-line-reset (change1 change2 j y x-max screen-line gr-len)
  `(setq ,change1 ,x-max
         ,change2 -1
         ,gr-len (+& ,gr-len ,j)
         ,j 0
         ,y (1+& ,y)
         ,screen-line (if (<& ,y *win-y-limit*) (svref old-screen-image ,y)
                          nil)))

;;;The value (if no throw done) is a fixnum count of the number of TTY lines
;;;used to display the buffer line. (Nessesarilly >= 1.)
(defun just-expand-line (line y point x-start &aux (y-initial y))
  (loop
   with g-len = (if (0p x-start) 0 (position-really line x-start *win-x-size*))
   with chrs = (line-chars line)
   with screen-line = (svref old-screen-image y)
   with x-max = (+& *win-x-pos* *win-x-size* -1)
   with change1 = x-max
   with change2 = -1
   with line-char-count = (line-char-count line)
   ;;Steppers.
   with j = *win-x-pos*
   for i upfrom x-start while (<& i line-char-count)
   for chr = (char chrs i)
   if (>=& j x-max)
   do (progn (redisplay-cont-line point change1 change2 y)
             (update-tty-map y line x-start i)
             (cont-line-reset change1 change2 j y x-max screen-line g-len)
             (when (and (0p (\\& y 4))
                        (type-ahead-p))
               (throw 'listen-tag nil)))
   do (cond ((graphic-char-p chr)
             (cond ((null screen-line)) ;past screen. just count.
                   ((char= chr (char screen-line j))) ;already there.
                   (t (setq change1 (min& change1 j) ;change it.
                            change2 j)
                      (setf (char screen-line j) chr)))
             (setq j (1+& j)))
            (t (loop with string = (stringify-char chr j)
                     for k from 0 below (simple-string-length string)
                     for ch = (char string k)
                     if (>=& j x-max)
                     do (progn (redisplay-cont-line point change1 change2 y)
                               (update-tty-map y line x-start i)
                               (cont-line-reset change1 change2 j y x-max
                                                screen-line g-len)
                               (when (and (0p (\\& y 4))
                                          (type-ahead-p))
                                 (throw 'listen-tag nil)))
                     do (cond ((null screen-line)) ;past screen. just count.
                              ((char= ch (char screen-line j))) ;already there
                              (t (setq change1 (min& change1 j) ;change it.
                                       change2 j)
                                 (setf (char screen-line j) ch)))
                     do (setq j (1+& j)))))
   finally (setf (line-graphic-length line) (+& g-len j))
   finally (unless (<& y *win-y-limit*) (return (1+& (-& y y-initial))))
   finally (when (<=& change1 change2) ;output line to tty.
             (cursorpos y change1 terminal-io)
             (send terminal-io :raw-oustr screen-line
                   change1 (1+& (-& change2 change1)))
             (send terminal-io :set-cursor-fried-flag t))
   finally (unless (%string-eqv screen-line 80spaces j 0 (1+& (-& x-max j)))
             (%string-replace screen-line 80spaces j 0 (1+& (-& x-max j)))
             (unless (=& change2 j) (cursorpos y j terminal-io))
             (send terminal-io :clear-eol))
   finally (update-tty-map y line x-start line-char-count)
   finally (when (and (0p (\\& y 4))
                      (type-ahead-p))
             (throw 'listen-tag nil))
   finally (return (1+& (-& y y-initial)))))

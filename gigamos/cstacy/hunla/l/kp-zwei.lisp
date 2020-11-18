;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL; Patch-file: T -*-
;;;
;;; "Kung-Pao" ZWEI hacks
;;; Copyright 1988,1987 (c) by Christopher C. Stacy

(defun fix-losing-buffer ()
  (com-set-pop-mark)
  (com-goto-beginning)
  (com-swap-point-and-mark)
  *interval*)

(defcom com-down-real-straight-line
        "Move to the beginning of the next line."
        (km r)
  (straight-down-real-line *numeric-arg*))

(defcom com-down-real-straight-line
        "Move to the beginning of the previous line."
        (km -r)
  (straight-down-real-line (- *numeric-arg*)))

(defun straight-down-real-line (n-lines)
  (let* ((*current-command-type* 'real-move)
         (point (point))
         (dest (forward-line point n-lines))
         (dis-return dis-bps))
    (set-centering-fraction n-lines)
    (cond (dest
           (move-bp point (bp-line dest) (bp-index dest)))
          ((not *numeric-arg-p*)
           (setq dis-return dis-text)
           (move-bp point (insert (interval-last-bp *interval*)
                                  #\cr)))
          (t
           (move-bp point (interval-last-bp *interval*))))
    dis-return))


(defcom com-hyperspace
        "Warp the mouse almost to the right margin, and enter Hyperspace."
        (nm)
  (multiple-value-bind (ignore ignore right ignore)
      (send tv:mouse-sheet ':inside-edges)
    (tv:mouse-warp (- right 16) tv:mouse-y))
  (zippy-noise (not *numeric-arg-p*))
  dis-none)

(defun zippy-noise (&optional (upward? t)
                    (start 2700)
                    (end 300)
                    (step 25)
                    (duration 5000))
  (if upward?
      (setq step (- step))
      (setq end (prog1 start (setq start end))))
  (do ((pitch start (+ pitch step)))
      ((= pitch end))
    (sys:%beep pitch duration))
  dis-none)


(defcom com-rotate-three-buffers
        "Rotate the top three buffers in the buffer stack."
        ()
  (rotate-buffer-history 3)
  dis-none)

(defcom com-draw-vertical-line
        "Draws a fill-column line on the screen."
        ()
  (let ((line-x (if *numeric-arg-p*
                    (times *numeric-arg-p* (font-char-width (current-font *window*)))
                    *fill-column*)))
    (multiple-value-bind (nil y1 nil y2)
        (send tv:selected-window :inside-edges)
      (send tv:selected-window :draw-line line-x y1 line-x y2)
      (report-column-setting "Fill Column" *fill-column*)))
    dis-none)




(defvar *read-only-pathnames* '("SYS:"))

(defmethod (zmacs-buffer :revert) (&optional new-pathname connect-flag select-flag quietly-flag)
  (prog1
    (funcall (or (get (buffer-mode self) 'major-mode-revert-function)
                 #'revert-file-buffer)
             self new-pathname connect-flag select-flag quietly-flag)
    (when (and pathname
               (not (eq file-id t))             ;When creating file buffers, always allow R/W.
               (not (member *major-mode* '(dired-mode))))
      (dolist (dir *read-only-pathnames*)
        (let ((buffer-dir (send pathname :new-pathname :name :unspecific :type :unspecific))
              (ro-dir (fs:parse-pathname dir)))
          (when (typep ro-dir 'fs:logical-pathname)
            (setq buffer-dir (or (send ro-dir :back-translated-pathname buffer-dir)
                                 buffer-dir)))
          (when (and (equal (pathname-host ro-dir) (pathname-host buffer-dir))
                     (or (null (pathname-directory ro-dir))
                         (lisp:search (pathname-directory ro-dir) (pathname-directory buffer-dir)
                                      :test #'string-equal)))
            (return (zwei:make-buffer-read-only self))))))))

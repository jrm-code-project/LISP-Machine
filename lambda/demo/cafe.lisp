;;; -*- Mode:LISP; Package:HACKS; Base:8; Lowercase: T -*-

;;; This program creates a color window and paints it with a familiar
;;; optical-illusion pattern whose name I can't remember at the moment.
;;; By frobbing the color map, you can see how various color versions
;;; of the illusion look.
;;; It is based on the function "checker" in MC: RWG1; PREHAX.

;;; It only works if you have a color monitor.

;;; The window to use, created when needed by color-checker.
(defvar *cc-window* nil)

;;; Parameters:
(defconst *cc-n-columns* 22)                    ; Number of columns.
(defconst *cc-n-rows* 22)                       ; Number of rows.
(defconst *cc-spacer-fraction* .0625s0)         ; Fraction of height devoted to spacer.

(defun draw-color-into-rectangle (window color x1 y1 x2 y2)
  (color:color-bitblt (+ (min (max x1 0) (tv:sheet-inside-width window))
                         (tv:sheet-left-margin-size window))
                      (+ (min (max y1 0) (tv:sheet-inside-height window))
                         (tv:sheet-top-margin-size window))
                      (+ (min (max x2 0) (tv:sheet-inside-width window))
                         (tv:sheet-left-margin-size window))
                      (+ (min (max y2 0) (tv:sheet-inside-height window))
                         (tv:sheet-top-margin-size window))
                      color
                      tv:alu-seta
                      window))

;;; Create the *cc-window* if it doesn't exist already, expose and clear it.
(defun initialize-cc-window ()
  (if (null *cc-window*)
      (setq *cc-window* (tv:make-window 'tv:window
                                        ':superior color:color-screen
                                        ':blinker-p nil ':borders nil
                                        ':label nil)))
  (funcall *cc-window* ':expose)
  (funcall *cc-window* ':clear-screen))

;;; This function draws the regular cafe wall pattern onto *cc-window*.
;;; The blocks are drawn in colors 1 and 2, and the spacer is drawn in color 3.
(defun draw-color-checker ()
  (initialize-cc-window)
  (multiple-value-bind (window-width window-height)
      (funcall *cc-window* ':inside-size)
    ;; The image is made up of a bunch of rows and columns.  Between successive
    ;; rows are spacers, which use up some of the available height.
    (let* ((width (truncate window-width *cc-n-columns*))
           (height (truncate window-height *cc-n-rows*))
           (spacer-height (fixr (* height *cc-spacer-fraction*))))
      (dotimes (row *cc-n-rows*)
        ;; Do this for each row:
        (let* ((y1 (* row height))
               (y2 (- (+ y1 height) spacer-height)))
          ;; y1 and y2 are the top and bottom of the bricks in this row.
          (do ((x (if (evenp row) 0 (- (truncate width 2))) (+ x width))
               (color 1 (- 3 color)))
              (( x window-width))
            ;; x is where to start; every other row is offset by half.
            ;; color is the color to draw; it alternates between 1 and 2.
            ;; Do this for each column.
            (draw-color-into-rectangle *cc-window* color x y1 (+ x width) y2))
          (draw-color-into-rectangle *cc-window* 3 0 y2 window-width (+ y2 spacer-height)))))))

;;; Top level function to produce regular B&W effect.
(defun cch ()
  (color:write-color-map 1 0 0 0)
  (color:write-color-map 2 377 377 377)
  (color:write-color-map 3 200 200 200)
  (draw-color-checker))

(defun cchc (&optional (wait 1000) (speed 0.03s0))
  (cch)
  (do ((angle 0.0s0 (+ angle speed)))
      (nil)
    (let* ((x (max 0 (min 377 (+ 200 (fixr (* 200.0s0 (sin angle)))))))
           (y (- 377 x)))
      (color:write-color-map 1 x x x)
      (color:write-color-map 2 y y y))
    (dotimes (i wait))))

;;; This has the problem that linear motion would probably look better than sine.
;;; Try making each odd row out of two new colors, and even rows get black->gray-> black
;;; while odd rows get black->gray->white.  Also try using lots of color map
;;; entries in stripes to get real motion in animation, moving every other row
;;; or moving alternate rows in alternate directions.

;;; This function also draws the cafe wall pattern, but in a more complicated
;;; way to allow hairier animation.  Color 0 is reserved for the background
;;; and color 17 for the spacer.  Then, even rows are drawn using colors
;;; 1 through 16, and odd rows are drawn using colors 16 through 1 (i.e. in
;;; the other order.
(defun draw-elaborate-color-checker ()
  (initialize-cc-window)
  (multiple-value-bind (window-width window-height)
      (funcall *cc-window* ':inside-size)
    ;; The image is made up of a bunch of rows and columns.  Between successive
    ;; rows are spacers, which use up some of the available height.
    (let* ((width (truncate window-width *cc-n-columns*))
           (height (truncate window-height *cc-n-rows*))
           (spacer-height (fixr (* height *cc-spacer-fraction*))))
      (dotimes (row *cc-n-rows*)
        ;; Do this for each row:
        (let* ((y1 (* row height))
               (y2 (- (+ y1 height) spacer-height)))
          ;; y1 and y2 are the top and bottom of the bricks in this row.
          (let* ((parity (evenp row))
                 (strip-width (truncate width 7)))
            (do ((x 0 (+ x strip-width))
                 (color 0 (if parity
                              (if ( color 16) 1 (1+ color))
                              (if ( color 1) 16 (1- color)))))
                (( x window-width))
              (draw-color-into-rectangle *cc-window* color x y1 (+ x strip-width) y2)))
          (draw-color-into-rectangle *cc-window* 17 0 y2
                                     window-width (+ y2 spacer-height)))))))

;;; This is the top level function.  It creates a cafe wall and slides alternate
;;; rows in alternate directions.
(defun cafe-slide (&optional (wait 100))
  (cond ((color:color-exists-p)
         (draw-elaborate-color-checker)
         (color:write-color-map 0 0 0 0)
         (loop for color from 1 to 7
               do (color:write-color-map color 0 0 0))
         (loop for color from 10 to 16
               do (color:write-color-map color 377 377 377))
         (color:write-color-map 17 200 200 200)
         (funcall standard-input ':tyi)
         (loop do
               (loop for b-to-w from 1 to 16
                     do (hack-two-slots b-to-w
                                        (1+ (\ (+ b-to-w 6) 16))
                                        wait))
           ;(if (funcall standard-input ':tyi-no-hang)
           ;    (return))
               ))
        (t
         (no-color-demo))))

(defun hack-two-slots (b-to-w w-to-b &optional (wait 1000))
  (loop for x from 0 to 377
        for y from 377 downto 0
        do (color:write-color-map b-to-w x x x)
           (color:write-color-map w-to-b y y y)
           (dotimes (i wait))))

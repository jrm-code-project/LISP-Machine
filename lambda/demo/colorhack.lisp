;;  -*-Mode:Lisp; Package:Hacks; Base: 8-*- stuff to play with the standard 4-bit color-screens

(defun COLORHACK ()

 " This package contains a bunch of routines to do graphics on the standard 4-bit
   color screens.

  First call should be to USE-COLOR-SCREEN to init color screen

       Defined colors (for color map stuff) stored as a list of 3 numbers '(red blue green)
    black, white, red, green, blue lime, cyan, violet

       Color map stuff:


   SET-COLOR         - sets a color map location to a color (red, blue, or a list)
      <location list>
   READ-COLOR        - returns the current color list for a particular location
      <location>
   PRINT-COLOR-MAP   - prints current color map
   SHADED-COLOR-MAP  - makes smooth (linear) color transitions between start and end colors
      <start-color end-color>
   RANDOM-COLOR-MAP  - randomizes color map
   BLACK-MAP         - makes all colors black
   WANDER-MAP        - randomly shade around color map, ala SMOKING CLOVER

       Basic graphics:

   COLOR-DRAW-POINT  - makes the point (x,y) a particular color
       (x y &optional <color *last-color*> <alu-type tv:alu-seta>)
   COLOR-DRAW-CLIPPED-POIINT - like above but checks to make sure point is in bounds first
       (x y &optional <color *last-color*> <alu-type tv:alu-seta>)
   COLOR-DRAW-LINE   - draws a colored line between (x1 y1) and (x2 y2)
       (x1 y1 x2 y2
           &optional <color *last-color*> <alu-type tv:alu-seta> <draw-end-point t>)
   COLOR-DRAW-CLIPPED-LINE   - like above but clips line to screen boundaries
       (x1 y1 x2 y2
           &optional <color *last-color*> <alu-type tv:alu-seta>)

   COLOR-DRAW-CIRCLE - draws a circle fairly quickly
       (x y r &optional <color *last-color*> <alu-type tv:alu-seta>)
   COLOR-DRAW-FILLED-IN-CIRCLE - draws a filled in circle very quickly
       (x y r &optional <color *last-color*> <alu-type tv:alu-seta>)

       Character string graphics:

   COLOR-DRAW-STRING - draws a string on the screen in a particular color
       (string x y
        &optional <color *last-color*> <flag t> <font tv:default-font*) <alu-type tv:alu-seta>
   COLOR-DRAW-CHAR   - draws a character at the point (x,y) in a particular color
       (font char x y &optional <color *last-color*> <alu-type tv:alu-seta>)

       Random hacks:

   Draw-Primaries    -  Three overlapping filled in circles
     (radius overlap)

   Define-Color      -

   Mix-Color         -


    J.Bradstreet  6-8-83
    With a lot of the initial work done by Gren. 12-82
----------------------------------------------------------------------------------------")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;****************************************************************************************
;                               Constants and routines for accessing color screens
;****************************************************************************************

;;  Constants that we need for messing a screen

(defvar *color-screen* color:color-screen)      ;Superior screen
(defvar *blit-array*  (tv:Make-sheet-bit-array *color-screen* 8 2))

(defvar c*screen-array        nil "The screen array (4(?)-bit pixel array)")
(defvar c*color-width         nil "Width in pixels of screen")
(defvar c*inside-color-width  nil "Likewise, but excluding border")
(defvar c*max-x               nil "Maximum asettable x position")
(defvar c*color-height        nil "Height in pixels")
(defvar c*inside-color-height nil "Analogous")
(defvar c*max-y               nil "Likewise")
(defvar c*left-border-width   nil "What do you expect it to be, you ninny ?")
(defvar c*top-border-width    nil "Why ask me, Gren wrote this !")
(defvar c*center-x            nil "The center x point, for centering")
(defvar c*center-y            nil "Likewise")

;----------------------------------------------------------------------------------------
(defconst *number-of-colors*          16.            "Number of colors in the color map")
(defconst *max-intensity*            255.            "Max intensity (8 bits/color")
(defconst *last-color*       (1- *number-of-colors*) "One minus *number-colors*")

;                          R                G               B
;                          |                |               |
(defconst black  `(        0                0               0))
(defconst white  `(,*max-intensity* ,*max-intensity* ,*max-intensity*))
(defconst red    `(,*max-intensity*        0               0))
(defconst green  `(       0         ,*max-intensity*       0))
(defconst blue   `(       0                0         ,*max-intensity*))
(defconst lime   `(,*max-intensity* ,*max-intensity*       0))
(defconst cyan   `(       0         ,*max-intensity* ,*max-intensity*))
(defconst violet `(,*max-intensity*        0         ,*max-intensity*))

;;
;;  A color-map is an array, with each row being the R G B's for one color.
;;  Note that color 0 is the background, and is usually left alone, as black.
;;

(defconst map-size `(,*number-of-colors* 3))

(defconst c*red   0)
(defconst c*green 1)
(defconst c*blue  2)

(defun make-color-map-array ()
  (make-array map-size ':type 'art-8b))

(defvar c*temp-map (make-color-map-array))
(defvar c*save-map (make-color-map-array))

;----------------------------------------------------------------------------------------


;****************************************************************************************
;                                                             Initialization Routines
;****************************************************************************************


(defun USE-COLOR-SCREEN (&optional (screen *color-screen*))
  (make-colorhack-windows)
  (setq c*screen-array        (tv:sheet-screen-array screen)
        c*color-width         (tv:sheet-width screen)
        c*inside-color-width  (tv:sheet-inside-width screen)
        c*max-x               (1- c*inside-color-width)
        c*color-height        (tv:sheet-height screen)
        c*inside-color-height (tv:sheet-inside-height screen)
        c*max-y               (1- c*inside-color-height)
        c*left-border-width   (tv:sheet-inside-left screen)
        c*top-border-width    (tv:sheet-inside-top screen)
        c*center-x            (truncate c*color-width 2)
        c*center-y            (truncate c*color-height 2))
  (IF (null c*save-map) (Shaded-color-map cyan red))
  *color-screen*)


;----------------------------------------------------------------------------------------



;****************************************************************************************
;                                                                     Color map stuff
;****************************************************************************************

       ;  Set/Read one color from a RGB-list

(defun set-color (location list)
  (color:write-color-map location (first list) (second list) (third list)))

(defun read-color (location)
  (firstn 3 (multiple-value-list (color:read-color-map location))))

;;
;;  Print out a color map for you to see
;;

(defun print-color-map (&optional (array (read-map)))
  (format t "~% Color: R G B")
  (do ((dimensions (array-dimensions array))
       (x 0 (1+ x)))
      ((= x (first dimensions)))
    (format standard-output "~& ~5O:~X" x)
    (do ((y 0 (1+ y)))
        ((= y (second dimensions)))
      (format standard-output "~A ~X" (aref array x y))))
  array)

;;
;;  Set the entire color map from the specified array, and do it while the color
;;  screen is busy elsewhere, so it happens all at once.
;;

(defmacro set-map (array)
  `(color:blt-color-map ,array))

(defun read-map (&optional (array c*save-map))
  (do* ((r 0) (g 0) (b 0)
        (color 0 (1+ color)))
      ((= color *last-color*))
    (multiple-value (r g b) (color:read-color-map color))
    (aset r array color c*red)
    (aset g array color c*green)
    (aset b array color c*blue))
  array)

;;
;;  Do something inside this shell if you want the color map restored to it's
;;  original value when you're done
;;

(defmacro without-clobbering-map (&body body)
  `(progn
     (progn (read-map)
            . ,body)
     (set-map ,'c*save-map)))

;;  Set the BLT-from array to COLOR in preparation for BITBLTing an area of the
;;  screen to one color.
;;

(defun set-blit-color (color)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (as-2-reverse color *blit-array* i 0)))


;****************************************************************************************
;                                                            Color Map Hacks
;****************************************************************************************

;;  Generate or fill an array with color #1 being start-color, color #last-color
;;  to end-color, and the inbetween colors to gradual shadings...
;;

(defun SHADED-COLOR-MAP (&optional
                         (start-color red) (end-color cyan)
                         (array (make-color-map-array)))
  (let* ((r (first start-color))
         (g (second start-color))
         (b (third start-color))
         (delta-r (truncate (- (first end-color) r) *last-color*))
         (delta-g (truncate (- (second end-color) g) *last-color*))
         (delta-b (truncate (- (third end-color) b) *last-color*)))
    (do ((color 1 (1+ color))
         (r r (+ r delta-r))
         (g g (+ g delta-g))
         (b b (+ b delta-b)))
        ((> color *last-color*))
      (aset r array color c*red)
      (aset g array color c*green)
      (aset b array color c*blue)))
  (set-map array))


;;
;;
;;  Generate or fill a color-map array with random colors
;;

(defun RANDOM-COLOR-MAP (&optional (array (make-color-map-array)))
  (do ((color 1 (1+ color)))
      ((> color *last-color*))
    (aset (random *max-intensity*) array color c*red)
    (aset (random *max-intensity*) array color c*green)
    (aset (random *max-intensity*) array color c*blue))
  (set-map array))

;;
;;  Yawrn
;;

(defun BLACK-MAP (ARRAY)
  (DO ((COLOR 0 (1+ COLOR)))
      ((> COLOR *LAST-COLOR*))
    (ASET 0 ARRAY COLOR C*red)
    (aset 0 array color c*green)
    (aset 0 array color c*blue)))


;;
;;  Just apply DEFINE-COLOR to each color in turn, and set the array accordingly
;;

(defun define-map (array)
  (do ((color 1 (1+ color)))
      ((> color *last-color*))
    (format standard-output "~&Define color #~O:" color)
    (let ((r-g-b (define-color color)))
      (format standard-output " RGB  ~A" r-g-b)
      (aset (first r-g-b)  array color c*red)
      (aset (second r-g-b) array color c*green)
      (aset (third r-g-b)  array color c*blue)))
  (set-map array))



;;
;;  In the given color map, roll the colors down (... 32, 21, 1RGB) over the
;;  specified time, melting the new color, RGB, into the last color of the map.
;;

(defvar float-array  (make-array map-size ':type 'art-float))
(defvar delta-array  (make-array map-size ':type 'art-float))


(defun melt-in (old-array r g b time &optional (factor 10.))
  (copy-array-contents old-array float-array)
  (let* ((n (max (* time factor) 1.0))
         (sleep (// time n)) )
    (do* ((this-color 1 (1+ this-color))
          (that-color 2 (1+ that-color))
          (that-r (aref float-array that-color c*red)
                  (if ( that-color *number-of-colors*) r (aref float-array that-color 0)))
          (that-g (aref float-array that-color c*green)
                  (if ( that-color *number-of-colors*) g (aref float-array that-color 1)))
          (that-b (aref float-array that-color c*blue)
                  (if ( that-color *number-of-colors*) b (aref float-array that-color 2))))
         ((> this-color *last-color*))
      (aset (// (- that-r (aref float-array this-color c*red)) n)
            delta-array this-color c*red)
      (aset (// (- that-g (aref float-array this-color c*green)) n)
            delta-array this-color c*green)
      (aset (// (- that-b (aref float-array this-color c*blue)) n)
            delta-array this-color c*blue))
    (with-real-time
      (do ((count 0 (1+ count)))
          (( count n))
        (do ((color 1 (1+ color)))
            ((> color *last-color*))
          (aset (+ (aref float-array color c*red) (aref delta-array color c*red))
                float-array color c*red)
          (aset (fixr (aref float-array color c*red)) old-array color c*red)
          (aset (+ (aref float-array color c*green) (aref delta-array color c*green))
                float-array color c*green)
          (aset (fixr (aref float-array color c*green)) old-array color c*green)
          (aset (+ (aref float-array color c*blue) (aref delta-array color c*blue))
                float-array color c*blue)
          (aset (fixr (aref float-array color c*blue)) old-array color c*blue))
        (set-map old-array)
        (cadr:sleep sleep)))))

;;
;;  Randomly shade around the color map, ala SMOKING CLOVER.  delta- is the change in
;;  RGB values between MELT-INs, time is the time it takes to finish a melt.
;;

(defun wander-map (&optional (delta-color 20) (time .01))
  (without-clobbering-map
    (black-map c*temp-map)
    (let ((delta-r delta-color)
          (delta-g 0)
          (delta-b 0))
      (set-map c*temp-map)
      (with-real-time
        (do ((r 0 (+ r delta-r))
             (g 0 (+ g delta-g))
             (b 0 (+ b delta-b)))
            ((funcall standard-output ':listen))
          (if (> r *max-intensity*)
              (setq r (- *max-intensity* (- r *max-intensity*))
                    delta-r (- delta-r)))
          (if (minusp r)
              (setq r (- r)
                    delta-r (- delta-r)
                    delta-g (+ delta-g delta-color)))
          (if (> g *max-intensity*)
              (setq g (- *max-intensity* (- g *max-intensity*))
                    delta-g (- delta-g)))
          (if (minusp g)
              (setq g (- g)
                    delta-g (- delta-g)
                    delta-b (+ delta-b delta-color)))
          (if (> b *max-intensity*)
              (setq b (- *max-intensity* (- b *max-intensity*))
                    delta-b (- delta-b)))
          (if (minusp b)
              (setq b (- b)
                    delta-b (- delta-b)
                    delta-r (+ delta-r delta-color)))
          (melt-in c*temp-map r g b time)
          (aset r c*temp-map *last-color* c*red)
          (aset g c*temp-map *last-color* c*green)
          (aset b c*temp-map *last-color* c*blue))))))


;****************************************************************************************
;                                                                 Graphics routines
;****************************************************************************************
;;
;;  Plot a point on the screen-array, with boundry-checking
;;

(defmacro COLOR-DRAW-POINT ( x y
                            &optional
                            (color '*last-color*)
                            (alu-type 'tv:alu-seta)
                            (array 'c*screen-array))
  "This routine draws a point into an array with a possible alu-type.  It does not
   include border widths like the routine this came from"

  `(as-2-reverse (IF (= ,alu-type tv:alu-seta)
                     ,color
                   (Boole ,alu-type ,color (ar-2-reverse ,array ,x ,y)))
                 ,array ,x ,y ))

;-----------

(defmacro COLOR-DRAW-CLIPPED-POINT ( x y
                                    &optional
                                    (color '*last-color*)
                                    (alu-type 'tv:alu-seta)
                                    (array 'c*screen-array))
  "Note that this only really works for screen arrays -  Else the boundary points may
   not be set correctly"

  `(and ( ,x 0) (< ,x c*inside-color-width) ( ,y 0) (< ,y c*inside-color-height)
        (color-draw-point ,x ,y ,color ,alu-type ,array)))

;-----------
(defsubst COLOR-DRAW-LINE (x1 y1 x2 y2
                        &optional
                        (color *last-color*)
                        (alu-type   tv:alu-seta)
                        (draw-end-pointp t)
                        (array c*screen-array))

  (do* ((fx1 (fixr x1)) (fx2 (fixr x2)) (fy1 (fixr y1)) (fy2 (fixr y2))
        (delta-x (- fx2 fx1)) (delta-y (- fy2 fy1))
        (n (float (max (abs delta-x) (abs delta-y))))
        (delta-delta-x (if (not (zerop n)) (truncate delta-x n)))
        (delta-delta-y (if (not (zerop n)) (truncate delta-y n)))
        (x fx1 (+ x delta-delta-x))
        (fixed-x fx1 (fixr x))
        (y fy1 (+ y delta-delta-y))
        (fixed-y fy1 (fixr y))
        (count 0 (1+ count)))
       (( count (IF draw-end-pointp n (1- n))))
    (COLOR-DRAW-POINT fixed-x fixed-y color alu-type array)))


;----------------------------------------------------------------------------------------
;    color line clipping stuff
;----------------------------------------------------------------------------------------
;;
;;  Given a point, calculate the 4-bit clip code.
;;

(defun CLIP-BITS (x y)
  (logior (if (minusp x) #2r0010 0)
          (if (minusp y) #2r0100 0)
          (if ( x c*inside-color-width) #2r0001 0)
          (if ( y c*inside-color-height) #2r1000 0)))

;;
;;  Clipping routines stolen and abused from Newman and Sproull
;;

(defmacro CLIP-ON-PLANE (c1 c2)
  `(progn
     (setq *t* (// (float ,c1)
                   (if (= ,c1 ,c2) 1e-10
                       (- ,c1 ,c2))))
     (if (or (minusp ,c1) (minusp ,c2))
         (if (minusp ,c1)
             (setq t-min (max t-min *t*))
             (setq t-max (min t-max *t*))))))

(defun COLOR-DRAW-CLIPPED-LINE (x1 y1 x2 y2
                          &optional
                          (color *last-color*)
                          (alu-type tv:alu-seta)
                          (array c*screen-array))

  (and (zerop (logand (clip-bits x1 y1) (clip-bits x2 y2)))
       (let ((t-min 0.0)
             (t-max 1.0)
             (*t* 0.0)
             (new-x1 x1)
             (new-y1 y1)
             (new-x2 x2)
             (new-y2 y2)
             (
x (- x2 x1))
             (
y (- y2 y1)))
         (clip-on-plane (- c*max-x x1) (- c*max-x x2))
         (clip-on-plane (- c*max-y y1) (- c*max-y y2))
         (clip-on-plane x1 x2)
         (clip-on-plane y1 y2)
         (cond ((> t-max t-min)
                (if (plusp t-min) (setq new-x1 (+ x1 (* t-min
x))
                                        new-y1 (+ y1 (* t-min
y))))
                (if (< t-max 1.0) (setq new-x2 (+ x1 (* t-max
x))
                                        new-y2 (+ y1 (* t-max
y))))
                (color-draw-line (fixr new-x1) (fixr new-y1)
                                 (fixr new-x2) (fixr new-y2)
                                 color alu-type array))))))

;----------------------------------------------------------------------------------------
;                                                  Draw a color character on the screen
;----------------------------------------------------------------------------------------

(defun COLOR-DRAW-CHAR (FONT CHAR X Y &optional (color *last-color*) (alu-type tv:alu-seta))
  (let ((width (FONT-RASTER-WIDTH FONT))
        (height (FONT-RASTER-HEIGHT FONT)))
    (DO ((H 0 (1+ H)))
        ((> H HEIGHT))
      (DO ((W 0 (1+ W)))
          ((> W WIDTH))
        (IF (= (FED:FONT-GET-PIXEL FONT CHAR H W) 1)
            (COLOR-DRAW-POINT (+ y w) (+ x h) color alu-type))))))


(defun COLOR-DRAW-STRING (STRING X Y
                          &optional
                          (Color *last-color*) (flag t) (FONT TV:*DEFAULT-FONT*)
                          (alu-type tv:alu-seta))
  "Draws a character string in a particular color.
   Note that this is not in the standard TV:DRAW-STRING format"

  (LET ((WIDTH (FONT-CHAR-WIDTH FONT))
        (WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
    (DO ((W X (+ W (IF WIDTH-TABLE (AREF WIDTH-TABLE (AREF C I)) WIDTH)))
         (C STRING)
         (CH)
         (I 0 (1+ I)))
        ((>= I (STRING-LENGTH C)))
      (OR (= (SETQ CH (AREF C I)) #\SPACE)
          (COLOR-DRAW-CHAR FONT (AREF C I)
                     (if flag Y W)
                     (if flag W Y)
                     color alu-type)))))

;----------------------------------------------------------------------------------------
;                                                                     Simple Objects
;----------------------------------------------------------------------------------------

;;  Pretty quickly draw a circle (outline)
;;
(defun COLOR-DRAW-CIRCLE (x y radius &optional (color *last-color*) (alu-type tv:alu-seta))
  (set-blit-color color)
  (do* ((r-squared (* radius radius))
        (row-1 y (1+ row-1))
        (row-2 y (1- row-2))
        (column-1 x (1+ column-1))
        (column-2 x (1- column-2))
        (n 0 (1+ n))
        (l radius (fixr (sqrt (- r-squared (* n n)))))
        (left-x (- x l) (- x l))
        (right-x (+ x l) (+ x l))
        (top-y (- y l) (- y l))
        (bottom-y (+ y l) (+ y l)))
      (( n (// radius (sqrt 2))))
    (color-draw-clipped-point left-x row-1 color alu-type)
    (color-draw-clipped-point right-x row-1 color alu-type)
    (color-draw-clipped-point left-x row-2 color alu-type)
    (color-draw-clipped-point right-x row-2 color alu-type)
    (color-draw-clipped-point column-1 top-y color alu-type)
    (color-draw-clipped-point column-1 bottom-y color alu-type)
    (color-draw-clipped-point column-2 top-y color alu-type)
    (color-draw-clipped-point column-2 bottom-y color alu-type)))

;;
;;  Even quicker, draw a filled-in circle
;;

(defun COLOR-DRAW-FILLED-IN-CIRCLE (x y radius
                                    &optional (color *last-color*) (alu tv:alu-xor))
  (if (zerop radius) (color-draw-point x y color)
      (set-blit-color color)
      (do* ((r-squared (* radius radius))
            (row-1 y (1+ row-1))
            (row-2 y (1- row-2))
            (n 0 (1+ n))
            (l radius (fixr (sqrt (- r-squared (* n n)))))
            (2l (* 2 radius) (* 2 l)))
           ((= n radius))
        (h-bar alu 2l *blit-array* 0 0 c*screen-array (- x l) row-1)
        (if (plusp n)
            (h-bar alu 2l *blit-array* 0 0 c*screen-array (- x l) row-2)))))

(defun h-bar (alu width from-array from-x from-y to-array to-x to-y)
  (and (plusp to-y) (< to-y c*inside-color-height)
       (let* ((left-x (max to-x 0))
              (right-x (min (+ to-x width) c*inside-color-width))
              (w (- right-x left-x)))
         (and (plusp right-x) (< to-x c*inside-color-width)
              (bitblt alu w 1
                      from-array from-x from-y
                      to-array (+ left-x c*left-border-width) (+ to-y c*top-border-width))))))




;****************************************************************************************
;                                                         Hacks to ponder
;****************************************************************************************
;;
;;  Olympics time
;;

(defun draw-primaries (radius overlap)
  (do* ((theta 30. (+ theta 120.))
        (r (* radius overlap))
        (x (+ c*center-x (* r (sind theta)))
           (+ c*center-x (* r (sind theta))))
        (y (+ c*center-y (* r (cosd theta)))
           (+ c*center-y (* r (cosd theta))))
        (color 1 (lsh color 1)))
       ((> theta 360.))
    (color-draw-filled-in-circle (fixr x) (fixr y) radius color tv:alu-xor)))



;;----------------------------------------------------------------------------------------
;;  Use the mouse buttons (L:R M:G R:B) to mix up a new color...
;;  Hold the button down to increase the intensity of that color, double-click and
;;  hold down to decrease intensity...  hit a key on the KB when done.  Returns a
;;  list (R G B)
;;

(defflavor color-definition-flavor ()
           (tv:window))

(defvar *RGB-counter* nil)

(defvar *RGB-mixer* nil)

(defvar *mixer-array* nil)

(defun make-colorhack-windows ()
  (unless *RGB-counter*
    (setq *RGB-counter* (tv:make-window 'color-definition-flavor
                                        ':save-bits nil
                                        ':borders 10
                                        ':activate-p t
                                        ':blinker-p nil
                                        ':expose-p nil
                                        ':font-map `(,fonts:43vxms)
                                        ':character-width "888"
                                        ':character-height 4)))
  (unless *RGB-mixer*
    (setq *RGB-mixer* (tv:make-window 'tv:window
                                      ':superior *color-screen*
                                      ':save-bits nil
                                      ':activate-p t
                                      ':expose-p nil
                                      ':blinker-p nil))
    (setq *mixer-array* (tv:sheet-screen-array *RGB-mixer*))))

(defconst left 1)
(defconst middle 2)
(defconst right 4)

(defmacro new-delta-mouse (old new)
  `(if (< ,new ,old) 0
     (if (bit-test 10 (tv:mouse-button-encode (boole 2 ,old ,new))) -1
       1)))

(defun define-color (color &optional (initial-color (read-color color)))
  (send *RGB-counter* ':set-label (format nil "Color #~O" color))
  (send *RGB-counter* ':expose-near '(:mouse))  ;Let's see the counter
  (send *RGB-counter* ':select)
  (tv:with-mouse-grabbed                        ;So no interrupting menus...
    (do* ((old-screen terminal-io)
          (old-buttons 0 current-buttons)
          (current-buttons 0 (tv:mouse-buttons))
          (button 0 (logxor old-buttons current-buttons))
          (old-r -1 r)                          ;-1's to force initial printing
          (old-g -1 g)                          ;of RGB values.
          (old-b -1 b)
          (delta-r 0) (delta-g 0) (delta-b 0)
          (r (first initial-color) (max (min (+ r delta-r) *max-intensity*) 0))
          (g (second initial-color) (max (min (+ g delta-g) *max-intensity*) 0))
          (b (third initial-color) (max (min (+ b delta-b) *max-intensity*) 0)))
         ((send *RGB-counter* ':tyi-no-hang)
          (send *RGB-counter* ':deexpose)       ;Don't want to see this anymore...
          (send old-screen ':select)
          (list r g b))
      (color:write-color-map color r g b)
      (and (plusp button)
           (if (= button left)
               (setq delta-r (new-delta-mouse old-buttons current-buttons))
             (if (= button middle)
                 (setq delta-g (new-delta-mouse old-buttons current-buttons))
                 (setq delta-b (new-delta-mouse old-buttons current-buttons)))))
      (if ( r old-r)
          (progn (send *RGB-counter* ':set-cursorpos 0 0 ':character)
                 (send *RGB-counter* ':clear-eol)
                 (send *RGB-counter* ':string-out (format nil "~3O" r))))
      (if ( g old-g)
          (progn (send *RGB-counter* ':set-cursorpos 0 1 ':character)
                 (send *RGB-counter* ':clear-eol)
                 (send *RGB-counter* ':string-out (format nil "~3O" g))))
      (if ( b old-b)
          (progn  (send *RGB-counter* ':set-cursorpos 0 2 ':character)
                  (send *RGB-counter* ':clear-eol)
                  (send *RGB-counter* ':string-out (format nil "~3O" b)))))))

;;
;;  Another variant of the same ...
;;

(defun mix-color (&optional (initial-color black) (overlap 0.333))
  (send *RGB-mixer* ':expose)
  (send *RGB-mixer* ':clear-screen)             ;In case someone else was using it
  (black-map c*temp-map)
  (set-map c*temp-map)
  (set-color 17 white)                          ;White border and label
  (draw-primaries 200 overlap)
  (tv:with-mouse-grabbed                        ;So no interrupting menus...
    (do* ((old-buttons 0 current-buttons)
          (current-buttons 0 (tv:mouse-buttons))
          (button 0 (logxor old-buttons current-buttons))
          (old-r -1 r)                          ;-1's to force initial printing
          (old-g -1 g)                          ;of RGB values.
          (old-b -1 b)
          (delta-r 0) (delta-g 0) (delta-b 0)
          (r (first initial-color) (max (min (+ r delta-r) *max-intensity*) 0))
          (g (second initial-color) (max (min (+ g delta-g) *max-intensity*) 0))
          (b (third initial-color) (max (min (+ b delta-b) *max-intensity*) 0)))
         ((send terminal-io ':tyi-no-hang)
          (send *RGB-mixer* ':deexpose)         ;Don't want to see this anymore...
          (send *RGB-mixer* ':deselect)
          (list r g b))
      (color:write-color-map 1 r 0 0)           ;Red primary
      (color:write-color-map 2 0 g 0)           ;Green primary
      (color:write-color-map 3 r g 0)           ;Red/Green
      (color:write-color-map 4 0 0 b)           ;Blue primary
      (color:write-color-map 5 r 0 b)           ;Red/Blue
      (color:write-color-map 6 0 g b)           ;Green/Blue
      (color:write-color-map 7 r g b)           ;Final color - RGB
      (and (plusp button)
           (if (= button left)
               (setq delta-r (new-delta-mouse old-buttons current-buttons))
             (if (= button middle)
                 (setq delta-g (new-delta-mouse old-buttons current-buttons))
                 (setq delta-b (new-delta-mouse old-buttons current-buttons))))))))

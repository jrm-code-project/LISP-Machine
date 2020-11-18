 ;;; -*- Mode: LISP;  Package: hacks; base: 8; lowercase: t -*-

(defvar *color-screen-array*)

(defsubst //- (n d) (floor n d))

(defun \- (n d)
  (multiple-value-bind (nil rem) (floor n d) rem))

(defsubst //+ (n d) (ceiling n d))

(defmacro plot (x1 y1)
  `(as-2-reverse (1+ (ar-2-reverse *color-screen-array* ,x1 ,y1))
                 *color-screen-array*
                 ,x1
                 ,y1))

(defun draw-sym-line (x0 y0 xn yn &optional ignore ignore
                         &aux (max (max (abs (- xn x0)) (abs (- yn y0)))))
       (draw-sym-subline x0 y0 xn yn 0 max))

(defun draw-sym-fractional-line (x0 y0 xn yn begfrac endfrac
                                    &aux (max (max (abs (- xn x0)) (abs (- yn y0)))))
       (draw-sym-subline x0 y0 xn yn
                         (- (fix (* -1 begfrac max)))
                         (fix (* endfrac max))))

(defun draw-sym-subline (x0 y0 xn yn i j &optional (dx (abs (- xn x0))) (dy (abs (- yn y0))))
       (cond ((> xn x0) (cond ((> yn y0) (cond ((> dx dy) (line-loop #'plot0 x0 y0 dx dy i j))
                                               ((line-loop #'plot1 y0 x0 dy dx i j))))
                              ((cond ((> dx dy) (line-loop #'plot7 x0 (- y0) dx dy i j))
                                     ((line-loop #'plot6 (- y0) x0 dy dx i j))))))
             ((cond ((> yn y0) (cond ((> dx dy) (line-loop #'plot3 (- x0) y0 dx dy i j))
                                     ((line-loop #'plot2 y0 (- x0) dy dx i j))))
                    ((cond ((> dx dy) (line-loop #'plot4 (- x0) (- y0) dx dy i j))
                           ((line-loop #'plot5 (- y0) (- x0) dy dx i j))))))))

(defun line-loop (fun x0 y0 dx dy i j
                      &aux (num (+ dx (* 2 i dy))))
       (do ((j2 (min j (ash dx -1)))
            (y (+ y0 (truncate num (ash dx 1))))
            (i i (1+ i))
            (x (+ x0 i) (1+ x))
            (f (ash (- (\ num (ash dx 1)) dx) -1) (+ f dy)))
           ((> i j2) (do ((i i (1+ i))
                          (x x (1+ x))
                          (f f (+ f dy)))
                         ((> i j))
                         (and (> (+ f f) dx) (setq f (- f dx) y (1+ y)))
                         (funcall fun x y)))
           (and ( (+ f f) dx) (setq f (- f dx) y (1+ y)))
           (funcall fun x y)))

(defun draw-clip-sym-line (x0 y0 xn yn xe ye xf yf
                            &optional (dx (abs (- xn x0))) (dy (abs (- yn y0))))
       (cond ((> xn x0) (cond ((> yn y0) (cond ((> dx dy)
                                                (line-clip #'plot0 x0 y0 dx dy xe ye xf yf))
                                               ((line-clip #'plot1 y0 x0 dy dx ye xe yf xf))))
                              ((cond ((> dx dy)
                                      (line-clip #'plot7 x0 (- y0) dx dy xe (- yf) xf (- ye)))
                                     ((line-clip #'plot6 (- y0) x0 dy dx (- yf) xe (- ye) xf))))))
             ((cond ((> yn y0)
                     (cond ((> dx dy)
                            (line-clip #'plot3 (- x0) y0 dx dy (- xf) ye (- xe) yf))
                           ((line-clip #'plot2 y0 (- x0) dy dx ye (- xf) yf (- xe)))))
                    ((cond ((> dx dy)
                            (line-clip #'plot4 (- x0) (- y0) dx dy (- xf) (- yf) (- xe) (- ye)))
                           ((line-clip #'plot5 (- y0) (- x0) dy dx (- yf) (- xf) (- ye) (- xe)))))))))
;clip symmetric segment (x0, y0) thru (xn, yn) to the rectangle (xe, ye) < (xf,yf)

(defun line-clip (fun x0 y0 dx dy xe ye xf yf
                      &aux (x (max x0 xe (if (= dy 0) xe (+ x0 (//+ (* dx
                                                                       (1- (ash (- ye y0) 1)))
                                                                    (ash dy 1))))))
                           (num (+ dx (* 2 dy (- x x0))))
                           (lx (min xf (if (= dy 0) xf (+ x0 (//+ (* dx (1- (ash (- yf y0) 1)))
                                                                 (ash dy 1)))))))
       (do ((xx (min (+ x0 (ash dx -1)) lx))
            (y (+ y0 (//- num (ash dx 1))))
            (x x (1+ x))
            (f (ash (- (\- num (ash dx 1)) dx) -1) (+ f dy)))
           ((> x xx) (do ((xx lx)
                          (x x (1+ x))
                          (f f (+ f dy)))
                         ((> x xx))
                         (and (> (+ f f) dx) (setq f (- f dx) y (1+ y)))
                         (funcall fun x y)))
           (and ( (+ f f) dx) (setq f (- f dx) y (1+ y)))
           (funcall fun x y)))

;line-clip incorrectly assumes that subsegment starts prior to midpoint of supersegment.
;the "divide for nearest integer" (ie divide for remainder of minimum magnitude),
;which is simulated the //- and \- of num and (ash dx 1), always rounds up on the
;half integer case, but should round down (for symmetry) if startup is in 2nd half.
;it would be nice to have these other flavors of divide.

(defun plot0 (x y) (plot x y))
(defun plot1 (x y) (plot y x))
(defun plot2 (x y) (plot (- y) x))
(defun plot3 (x y) (plot (- x) y))
(defun plot4 (x y) (plot (- x) (- y)))
(defun plot5 (x y) (plot (- y) (- x)))
(defun plot6 (x y) (plot y (- x)))
(defun plot7 (x y) (plot x (- y)))

(declare (special min-x min-y max-x max-y mid-x mid-y beg end))

(COMMENT
(defun semi-circ (r &optional (y 0) (x r) (f 0))
;  (color:clear)
  (let ((min-x (screen-x1 tv-color-screen))
        (min-y (screen-y1 tv-color-screen))
        (max-x (1- (screen-x2 tv-color-screen)))
        (max-y (1- (screen-y2 tv-color-screen)))
        (mid-x (truncate (screen-width tv-color-screen) 2))
        (mid-y (truncate (screen-height tv-color-screen) 2)))
    (semi-circ-1 r y x f)))  )

(defun semi-circ-1 (r y x f)
      (rect-points x y)
      (and (< y (1- x)) (semi-circ-1 r
                                     (1+ y)
                                     (cond (( (setq f (+ f y y 1)) x)
                                            (setq f (- f x x -1))
                                            (1- x))
                                           (t x))
                                     f))
      (and ( x y) ( y 0) (rect-points y x)))

(defun semi-wedge (r)
;  (color:clear)
  (MULTIPLE-VALUE-BIND (MIN-X MIN-Y MAX-X MAX-Y)
      (FUNCALL COLOR:COLOR-SCREEN ':EDGES)
    (SETQ MAX-X (1- MAX-X) MAX-Y (1- MAX-Y))
    (let ((mid-x (truncate (- MAX-X MIN-X) 2))
          (mid-y (truncate (- MAX-Y MIN-Y) 2)))
      (do ((y 0 (1+ y))
           (x r)
           (f 0 (+ f y y 1)))
          ((> y x))
        (and ( f x) (setq x (1- x) f (- f x x -1)))
        (draw-clip-sym-line mid-x mid-y (+ x mid-x) (+ y mid-y) min-x min-y max-x max-y))
      (do ((a (TV:SHEET-SCREEN-ARRAY COLOR:COLOR-SCREEN))
         (x mid-x (1+ x)))
        ((> x max-x))
      (as-2-reverse (1- (ash (ar-2-reverse a x mid-y) 1)) a x mid-y)
      (and ( (+ x (- mid-x) mid-y) max-y)
           (as-2-reverse
             (1- (ash (ar-2-reverse a x (+ x (- mid-x) mid-y)) 1))
             a x (+ x (- mid-x) mid-y)))
      (do ((yy (min max-y (+ x mid-y (- mid-x))))
           (y mid-y (1+ y)))
          ((> y yy))
        (do ((v (ar-2-reverse a x y))
             (x x (+ mid-x mid-y (- y)))
             (y y (+ mid-y x (- mid-x)))
             (i 0 (1+ i)))
            (( i 4))
          (and ( y max-y) (> y min-y)
               (as-2-reverse (as-2-reverse v a (+ mid-x mid-x (- x)) y)
                             a x y))))))))

(DEFUN NO-COLOR-DEMO ()
  "Report that we can't do this demo."
  ;;this is better than wedging the machine, or generating an ugly error, or doing nothing
  (TV:NOTIFY NIL "Sorry, apparently you don't have a color screen."))

(defun smoking-clover (&optional (size 5432) (speed 4321))
  "Displays a really neat pattern on the color screen.  Slowly at first, then speed up."
  (COND ((COLOR:COLOR-EXISTS-P)
         (WITH-REAL-TIME
           (setq *color-screen-array* (tv:sheet-screen-array color:color-screen))
           (COLOR:write-color-map 0 0 0 0)
           (color:clear)
           (COLOR:random-color-map)
           (semi-wedge size)
           (color-guard speed)))
        (T
         (NO-COLOR-DEMO))))

(defun semi-circ-1 (r y x f)
      (rect-points x y)
      (and (< y (1- x)) (semi-circ-1 r
                                     (1+ y)
                                     (cond (( (setq f (+ f y y 1)) x)
                                            (setq f (- f x x -1))
                                            (1- x))
                                           (t x))
                                     f))
      (and ( x y) ( y 0) (rect-points y x)))

(defun mask-points (x y)
      (draw-sym-fractional-line
           (- mid-x x) (- mid-y y) (+ mid-x x) (+ mid-y y) beg end)
      (draw-sym-fractional-line
           (+ mid-x y) (- mid-y x) (- mid-x y) (+ mid-y x) beg end))

(defun rect-points (x y)
      (draw-clip-sym-line
           (- mid-x x) (- mid-y y) (+ mid-x x) (+ mid-y y) min-x min-y max-x max-y)
      (draw-clip-sym-line
           (+ mid-x y) (- mid-y x) (- mid-x y) (+ mid-y x) min-x min-y max-x max-y))

(defun mash-points (x y &aux (m1 (cond ((> y x) (1- mid-y))
                                       ((min mid-x
                                             (- (truncate (- (* mid-x mid-x (- y x))
                                                    (* mid-y (- (* y mid-x) (* x mid-y))))
                                                 (* x (- mid-y mid-x))) 5)))))
                             (z (max x y)))
        (draw-sym-subline
           (- mid-x x) (- mid-y y) (+ mid-x x) (+ mid-y y) (- z m1 -1) (+ z m1))
                (or (= y 0) (draw-sym-subline
           (- mid-x x) (+ mid-y y) (+ mid-x x) (- mid-y y) (- z m1 -1) (+ z m1))))

(defun color-ramp (red green blue)
  (WITH-REAL-TIME
      (do ((r 0 (+ r red))
           (g 0 (+ g green))
           (b 0 (+ b blue))
           (i 0 (1+ i)))
          ((= i 20))
        (COLOR:write-color-map i r g b))))

(defun color-march (&optional (y 0))
  (COND ((COLOR:COLOR-EXISTS-P)
         (WITH-REAL-TIME
           (do ((dr 0 (- (random 42) 20))
                (dg -21 (- (random 42) 20))
                (db 21 (- (random 42) 20)))
               ((funcall terminal-io ':tyi-no-hang))
             (multiple-value-bind (r g b) (COLOR:read-color-map y)
               (do ((r r (+ r dr))
                    (g g (+ g dg))
                    (b b (+ b db)))
                   ((bit-test (logior r g b) 400))
                 (do ((i 17 (1- i))
                      (r r) (g g) (b b))
                     ((< i y))
                   (cond ((= (logand i 1) 1)
                          (do ((tv-adr (TV:screen-control-address color:color-screen)))
                              ((bit-test (%xbus-read tv-adr) 40)))))
                   (COLOR:write-color-map-immediate i r g
                                                    (prog1 b
                                                           (multiple-value (r g b)
                                                             (COLOR:read-color-map i))))))))))
        (T
         (NO-COLOR-DEMO))))

(defun color-guard (&optional (snooze 0) (y 0)
                    &aux (map-values (make-array '(400 3)
                                                 ':type 'art-8b)))
      (do ((i 0 (1+ i))
           (r) (g) (b))
          (( i 400))
        (multiple-value (r g b) (COLOR:read-color-map i))
        (aset r map-values i 0)
        (aset g map-values i 1)
        (aset b map-values i 2))
      (do ((dr 0 (- (random 1020) 400))
           (dg -1010 (- (random 1020) 400))
           (db 1010 (- (random 1020) 400)))
          ((funcall terminal-io ':tyi-no-hang)
           (return-array (prog1 map-values (setq map-values nil))))
        (do ((r (aref map-values y 0) (+ r dr))
             (g (aref map-values y 1) (+ g dg))
             (b (aref map-values y 2) (+ b db)))
            ((bit-test (logior r g b) 400))
          (do ((i snooze (1- i))) ((< i 0)))
          (do ((i 377 (1- i))
               (or) (og) (ob)
               (r r or)
               (g g og)
               (b b ob))
              ((< i y))
            (setq or (aref map-values i 0) og (aref map-values i 1) ob (aref map-values i 2))
            (aset r map-values i 0)
            (aset g map-values i 1)
            (aset b map-values i 2)
            (grey:write-color-map-immediate i r g b)))))

(defun color-zoom (&optional (z 0) &aux (map-values (make-array '(20 3)
                                                                ':type 'art-8b)))
      (do ((i 0 (1+ i))
           (r) (g) (b))
          (( i 20))
        (multiple-value (r g b) (COLOR:read-color-map i))
        (aset r map-values i 0)
        (aset g map-values i 1)
        (aset b map-values i 2))
      (do ((j 1)
           (dr 0 (- (random 80) 36))
           (dg -21 (- (random 80) 36))
           (db 21 (- (random 80) 36)))
          ((funcall terminal-io ':tyi-no-hang)
           (return-array (prog1 map-values (setq map-values nil))))
        (do ((r (aref map-values j 0) (+ r dr))
             (g (aref map-values j 1) (+ g dg))
             (b (aref map-values j 2) (+ b db)))
            ((bit-test (logior r g b) 400))
          (setq j (logand (1- j) 17))
          (do ((i j (logand (1- i) 17))
               (r r) (g g) (b b)
               (rr)  (gg)  (bb)
               (k 0 (1+ k)))
              ((= k 20))
            (do ((i 0 (1+ i)))((> i z)))        ;snooze
            (setq rr (aref map-values i 0)
                  gg (aref map-values i 1)
                  bb (aref map-values i 2))
            (aset r map-values i 0)
            (aset g map-values i 1)
            (aset b map-values i 2)
            (setq r (ash (+ r (* 37 rr) 25) -5)
                  g (ash (+ g (* 37 gg) 25) -5)
                  b (ash (+ b (* 37 bb) 25) -5)))
          (COLOR:blt-color-map map-values))))

(defun color-mash ()
  (COND ((COLOR:COLOR-EXISTS-P)
         (WITH-REAL-TIME
           (do ((i 1)
                (dr 0 (- (random 8) 4))
                (dg -21 (- (random 8) 4))
                (db 21 (- (random 8) 4)))
               ((funcall terminal-io ':tyi-no-hang))
             (multiple-value-bind (r g b) (COLOR:read-color-map i)
               (do ((r r (+ r dr))
                    (g g (+ g dg))
                    (b b (+ b db)))
                   ((bit-test (logior r g b) 400))
                 ;          (and (bit-test i 1)
                 ;               (do ((tv-adr (screen-control-address tv-color-screen)))
                 ;                   ((bit-test (%xbus-read tv-adr) 40))))
                 (COLOR:write-color-map (setq i (logand (1- i) 17))
                                        r
                                        g
                                        b
                               t))))))
        (T
         (NO-COLOR-DEMO))))

(COMMENT
(defun frac-tour (a b &optional (xx (screen-x2 tv-color-screen))
                                (yy (screen-y2 tv-color-screen)))
      (do ((pixel-array (screen-buffer-pixel-array tv-color-screen))
           (x (screen-x1 tv-color-screen) (1+ x)))
          (( x xx))
        (do ((y (screen-y1 tv-color-screen) (1+ y)))
            (( y yy))
          (as-2-reverse (fracpart (+ (* a x) (* b y))) pixel-array x y)))) )

;(defun fracpart (a) (fix (ash (- a (fix a)) 4)))

(defun fracpart (a) (- 17 (haulong (fix (ash (- a (fix (+ a .5))) 20)))))

(defun random-ramp ()
  (COND ((COLOR:COLOR-EXISTS-P)
         (WITH-REAL-TIME
           (do ((i 0 (1+ i)))
               ((= i 20))
             (COLOR:write-color-map i (random (+ 17 (ash i 4)))
                                    (random (+ 17 (ash i 4)))
                                    (random (+ 17 (ash i 4)))))))
        (T
         (NO-COLOR-DEMO))))

(defun brighten ()
  "Possibly make the color screen more visible."
  (COND ((COLOR:COLOR-EXISTS-P)
         (WITH-REAL-TIME
           (do ((i 17 (- i 3))
                (r 377 (- r 60))
                (g 377 (- g 60))
                (b 377 (- b 60)))
               (( i 2))
             (color:write-color-map i r 0 0)
             (color:write-color-map (1- i) 0 g 0)
             (color:write-color-map (- i 2) 0 0 b))
           (color:write-color-map 0 0 0 0)))
        (T
         (NO-COLOR-DEMO))))

(defdemo "Color TV Hacks" "Various demos that run on the color screen, if you have one."
  "Color"
  ("Smoking Clover" "Gosper's spectacular display hack." (smoking-clover))
  ("Cafe Slide" "Cafe wall illusion.  Type space to start it sliding." (cafe-slide))
  ("Color Mash" "Mash up the color map." (color-mash))
  ("Color March" "March colors through the color map." (color-march))
; ("Color Ramp" "This can't work." (color-ramp))
  ("Random Ramp" "Randomize color map." (random-ramp))
  ("Brighten" "" (brighten)))

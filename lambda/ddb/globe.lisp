;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


(defconstant %pi/180 0.01745329252)
(defconstant %180/pi 57.29577953)


(defvar *dirty-old-x* 0)                        ;places to keep next position to draw from
(defvar *dirty-old-y* 0)                        ;


(defvar *xmin* 0)                               ;global places for screen characteristics
(defvar *xmid* 512)                             ;particularly for the TRANSLATE and
(defvar *xmax* 1024)                            ;LIST-PLOT functions

(defvar *ymin* 0)
(defvar *ymid* 386)
(defvar *ymax* 772)

(defvar *zmin* -10000)
(defvar *zmid* 0)
(defvar *zmax* 10000)


(defvar sin-theta)                              ;having sines and cosines of translation angles
(defvar cos-theta)                              ;global saves time when generating curve point-lists
(defvar sin-phi)
(defvar cos-phi)
(defvar sin-alpha)
(defvar cos-alpha)


(defun putcursor (x y)                          ;puts cursor at position (x y)
  "Places cursor at position (x y) on landscape"
  (send *terminal-io* :set-cursorpos x y))


(defun clears ()
  "Clears screen on landscape"
  (send *terminal-io* :refresh))                ;clears screen


(defun moveto (x y)                             ;sets next draw from position
  "Changes internal last (x y) to arguments"
  (setq *dirty-old-x* x)
  (setq *dirty-old-y* y))


(defun drawto (x y)                             ;draws line from current position to next
  "Draws line from last (x y) to new (x y) argument"
  (send *terminal-io* :draw-line *dirty-old-x* *dirty-old-y* x y)
  (moveto x y))                                 ;makes new position current


(defun putdot (x y)                             ;turns on pixel at (x y)
  "Turns on pixel at (x y) and sets internal last position"
  (send *terminal-io* :draw-point x y)
  (moveto x y))                                 ;makes new position current



(defun list-plot-with-types (list)              ;plot lines from a list
  "Plots lines on landscape from LIST containing: x1 y1 type1 x2 y2 type2 ..."
  (let ((max (- (length list) 3))
        (x (first list))
        (y (second list))
        (type (third list)))
    (moveto x y)
    (do ((i 3 (+ i 3)))
        ((> i max))
      (setq x (elt list i)
            y (elt list (1+ i))
            type (elt list (+ i 2)))
      (case type
        (dotted (putdot x y))
        (clear  (moveto x y))
;       (dashed (dashto x y))
        (solid  (drawto x y))
        (otherwise (moveto x y))))
    (+ max 3)))


(defun spherical-to-cartesian (rho theta phi)
  "Returns multiple values X Y Z"
  (values
    (* rho (sin phi) (cos theta))
    (* rho (sin phi) (sin theta))
    (* rho (cos phi))))


(defun init-angles (theta phi alpha)
  "Assigns global variables to sines and cosines of arguments"
  (setq sin-theta (sin theta)
        cos-theta (cos theta)
        sin-phi   (sin phi)
        cos-phi   (cos phi)
        sin-alpha (sin alpha)
        cos-alpha (cos alpha)))


(defun rotate3 (x y z)
  "Returns new X Y Z rotated by the three angles in that order"
  (let (xp yp zp)
    (setq xp x
          yp y
          zp z
          x (- (* xp cos-theta) (* yp sin-theta))
          y (+ (* xp sin-theta) (* yp cos-theta))
          xp x
          yp y
          x (+ (* zp sin-phi) (* xp cos-phi))
          z (- (* zp cos-phi) (* xp sin-phi))
          zp z
          y (- (* yp cos-alpha) (* zp sin-alpha))
          z (+ (* yp sin-alpha) (* zp cos-alpha)))
    (values x y z)))


(defun translate (x y z)
  "Returns X Y Z translated to landscape"
  (values
    (round (+ *xmid* y))
    (round (- *ymid* z))
    (round x)))



(defun globe-plot
       (&key
        (radius 300)
        (longs t)   (latts t)
        (hide t)                                ;nil if to show hidden lines
;       (shade nil)                             ;t if to set illumination zenith
        (inform nil)
        (right-rotation nil)  (forward-tilt nil)  (left-tilt nil)
        )
  "Plots a globe according to supplied keywords"
  (format t "~%Globe plotter...~%")
  (let* ((delta-theta (* %pi/180                                ;(/ pi 180.0)
                         (if right-rotation right-rotation
                           (prompt-and-read :number "degrees right-rotation: "))))
         (delta-phi   (* %pi/180
                         (if forward-tilt   forward-tilt
                           (prompt-and-read :number "degrees forward-tilt:   "))))
         (rotation    (* %pi/180
                         (if left-tilt      left-tilt
                           (prompt-and-read :number "degrees left-tilt:      "))))
         (theta)
         (phi)
         (x) (y) (z)
         (plot-list)
         )
    (init-angles delta-theta delta-phi rotation)
    (clears)
    ;;Draw Latittude Lines
    (when latts
      (do ((angle-latt 80 (- angle-latt 10))) ((< angle-latt -80))
        (setq phi (* %pi/180 (- 90 angle-latt)))
        (do ((angle-long 0 (+ angle-long 2))) ((> angle-long 360))
          (setq theta (* %pi/180 angle-long))
          (multiple-value-setq
            (x y z)
            (spherical-to-cartesian radius theta phi))
          (multiple-value-setq
            (x y z)
            (rotate3 x y z))
          (multiple-value-setq
            (x y z)
            (translate x y z))
          (setq plot-list
                (nconc plot-list
                       (list x y (if (minusp z)
                                     (if hide
                                         'clear
                                       'dotted)
                                   'solid)))))
        (list-plot-with-types plot-list)
        (setq plot-list nil))
      )
    ;;Draw Longitude Lines
    (when longs
      (do* ((angle-long 0 (+ angle-long 15))
            (end-latt 90 (if (zerop (remainder angle-long 2))
                             90
                           70)))
           ((> angle-long 360))
        (setq theta (* %pi/180 angle-long))
        (do ((angle-latt end-latt (- angle-latt 2))) ((< angle-latt (- end-latt)))
          (setq phi (* %pi/180 (- 90 angle-latt)))
          (multiple-value-setq
            (x y z)
            (spherical-to-cartesian radius theta phi))
          (multiple-value-setq
            (x y z)
            (rotate3 x y z))
          (multiple-value-setq
            (x y z)
            (translate x y z))
          (setq plot-list
                (nconc plot-list
                       (list x y (if (minusp z)
                                     (if hide
                                         'clear
                                       'dotted)
                                   'solid)))))
        (list-plot-with-types plot-list)
        (setq plot-list nil))
      )
    (putcursor 0 0)
    (when inform
      (setq right-rotation (* delta-theta %180/pi)
            forward-tilt   (* delta-phi   %180/pi)
            left-tilt      (* rotation    %180/pi))
      (format t "theta ~f~%phi   ~f~%alpha ~f"
              right-rotation forward-tilt left-tilt))
    (values)
    ))

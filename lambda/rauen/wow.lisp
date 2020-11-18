;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(defun wow ()
  (dotimes (i 350)
    (send tv:initial-lisp-listener :draw-circle 400 400 (* 2 i)))
  (loop))

(defvar *window* tv:initial-lisp-listener)

(defconstant *square-root-of-two* (sqrt 2))

(defconstant *square-root-of-two-over-two* (/ 1 (sqrt 2)))

(defconstant *pi-over-four* (/ pi 4))

(defconstant *45-degree-left-rotation*
             (complex *square-root-of-two-over-two* *square-root-of-two-over-two*))

(defconstant *45-degree-right-rotation*
             (complex *square-root-of-two-over-two* (- *square-root-of-two-over-two*)))

(defvar *x* 0)

(defvar *y* 0)

(defun c-curve (length angle min-length)
  (if (< length min-length)
      (plot-line length angle)
      (progn
        (c-curve (* length *square-root-of-two-over-two*)
                 (+ angle *pi-over-four*)
                 min-length)
        (c-curve (* length *square-root-of-two-over-two*)
                 (- angle *pi-over-four*)
                 min-length))))

(defun dragon-curve (length angle min-length handedness)
  "See p. 348, 'LISP'."
  (if (< length min-length)
      (plot-line length angle)
      (progn
        (dragon-curve (* length *square-root-of-two-over-two*)
                      (+ angle (* handedness *pi-over-four*))
                      min-length
                      +1)
        (dragon-curve (* length *square-root-of-two-over-two*)
                      (- angle (* handedness *pi-over-four*))
                      min-length
                      -1))))

(defun turtle-depth-dragon-curve (length angle depth handedness)
  "See p. 348, 'LISP'."
  (if (zerop depth)
      (plot-line length angle)
      (progn
        (turtle-depth-dragon-curve (* length *square-root-of-two-over-two*)
                            (+ angle (* handedness *pi-over-four*))
                            (1- depth)
                            +1)
        (turtle-depth-dragon-curve (* length *square-root-of-two-over-two*)
                            (- angle (* handedness *pi-over-four*))
                            (1- depth)
                            -1))))

(defun depth-dragon-curve (x0 y0 x1 y1 depth handedness)
  "See p. 348, 'LISP'."
  (if (zerop depth)
      (send *window* :draw-lines tv:alu-ior (round x0) (round y0) (round x1) (round y1))
      (let* ((newpoint (+ (* *square-root-of-two-over-two*
                             (if (= handedness 1)
                                 *45-degree-left-rotation*
                                 *45-degree-right-rotation*)
                             (complex (- x1 x0) (- y1 y0)))
                          (complex x0 y0)))
             (xnew     (realpart newpoint))
             (ynew     (imagpart newpoint)))
        (depth-dragon-curve x0 y0 xnew ynew (1- depth) +1)
        (depth-dragon-curve xnew ynew x1 y1 (1- depth) -1))))

(defun funny-curve (length angle min-length handedness)
  (if (< length min-length)
      (plot-line length angle)
      (progn
        (funny-curve (* length *square-root-of-two-over-two*)
                      (+ angle (* handedness *pi-over-four*))
                      min-length
                      (- handedness))
        (funny-curve (* length *square-root-of-two-over-two*)
                      (- angle (* handedness *pi-over-four*))
                      min-length
                      (+ handedness)))))

(defun plot-line (length angle)
  (let* ((dx (* length (cos angle)))
         (dy (* length (sin angle)))
         (x2 (round (+ *x* dx)))
         (y2 (round (+ *y* dy))))
    (send *window* :draw-line *x* *y* x2 y2)
    (setq *x* x2)
    (setq *y* y2)))

(defun center ()
  (setq *x* 300)
  (setq *y* 300))

(eval-when (eval) #'try)
(NAMED-LAMBDA TRY NIL (BLOCK TRY (CENTER) (DRAGON-CURVE 200 0 3 1) (DRAGON-CURVE 200 0 2 1) (CENTER) (DRAGON-CURVE 200 PI 2 1) (CENTER) (DRAGON-CURVE 200 (* PI 1/2) 1 1) (CENTER) (DRAGON-CURVE 200 (* PI 3/2) 1 1)))

(dragon-curve
(eval-when (eval)
  (center)
  (setq *x* (+ 200 *x*))
  (dragon-curve 200 (* pi 3/4) 2 1))

(defun dragon-iter ()
  (let ((i 0)
        (length 500)
        (x0 200)
        (y0 200))
    (loop
      (let ((char (read-char)))
        (cond
          ((char= char #\Page)
           (send *window* :clear-window))
          ((char= char #\Resume)
           (send *window* :clear-window)
           (incf i))
          ((char= char #\Overstrike)
           (let ((xleft  (round (- x0 (* length 1/3))))
                 (ytop   (round (- y0 (* length 1/3))))
                 (xright (round (+ x0 (* length 7/6))))
                 (ybot   (round (+ y0 (* length 2/3)))))
             (send *window* :draw-lines tv:alu-ior
                   xleft ytop xright ytop xright ybot xleft ybot xleft ytop)))
          (t
           (incf i))))
;      (setq *x* x0)
;      (setq *y* y0)
      (depth-dragon-curve 200 200 700 200 i 1))))

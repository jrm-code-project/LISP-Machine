;;; -*- Mode:LISP; Package:USER; Base:16; Readtable:CL -*-
; plot-package.lisp  --  a compilation of various routines
;

(defun clears () (send *terminal-io* :refresh))


(defvar *dirty-old-x* 0)
(defvar *dirty-old-y* 0)


(defun moveto (x y)
  (setq *dirty-old-x* x)
  (setq *dirty-old-y* y))


(defun drawto (x y)
  (send *terminal-io* :draw-line *dirty-old-x* *dirty-old-y* x y)
  (moveto x y))


(defun bufplot (plot-list)
  (let* ((number-of-lines (- (// (length plot-list) 2) 1))
         (x (car  plot-list))
         (y (cadr plot-list))
         (plot-list (rest2 plot-list)))
    (moveto x y)
    (do* ((i 0 (+ i 1)))
         ((= i number-of-lines))
      (setq x (car  plot-list))
      (setq y (cadr plot-list))
      (setq plot-list (rest2 plot-list))
      (drawto x y))
    number-of-lines))


(defun LIST-PLOT (list)                         ;plot lines from a list
  "Plots lines on landscape from LIST containing: x1 y1 x2 y2 ..."
  (let ((max (- (length list) 2))
        (x (first list))
        (y (second list)))
    (moveto x y)
    (do ((i 2 (+ i 2)))
        ((> i max))
      (setq x (elt list i)
            y (elt list (1+ i)))
      (drawto x y))
    (+ max 2)))


(defun putcursor (x y)
  (send *terminal-io* :set-cursorpos x y))


(defun liss (theta-one theta-two delta-one delta-two radius number-of-iterations)
  (let ((xmid 512)
        (ymid 386)
        (theta-one theta-one)
        (theta-two theta-two)
        (x 0)
        (y 0)
        (plot-list ()))
    (do ((i 0 (+ i 1)))
        ((= i (+ 1 number-of-iterations)))
      (setq x (round (+ (* radius (sin theta-one)) xmid)))
      (setq y (round (+ (* radius (sin theta-one) (sin theta-two)) ymid)))
      (setq plot-list (append plot-list (list x y)))
      (setq theta-one (+ theta-one delta-one))
      (setq theta-two (+ theta-two delta-two)))
    (clears)
;    (print plot-list)
    (bufplot plot-list)
    (putcursor 0 0))
  (list 'liss theta-one theta-two delta-one delta-two radius number-of-iterations))



(defun zig-zag-left-to-right ()
  (let ((toggle-direction 1)
        (x 0) (y 500)
        (oldx 0) (oldy 0))
    (do ((i 0 (+ i 50)))
        ((= i 1000))
      (setq oldx x)
      (setq oldy y)
      (setq toggle-direction (* toggle-direction -1))
      (setq x (+ 50 x))
      (setq y (+ y (* 50 toggle-direction)))
      (send *terminal-io* :draw-line oldx oldy x y))
    (send *terminal-io* :draw-point 0 650)
    ))

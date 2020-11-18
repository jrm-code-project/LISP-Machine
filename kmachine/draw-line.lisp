;;; -*- Mode:LISP; Package:NEW-MATH; Base:10; Readtable:CL -*-

(defun draw-line (x1 y1 x2 y2)
  (draw-point x1 y1)
  (draw-point x2 y2)
  (draw-line-1 x1 y1 x2 y2))

(defun draw-line-1 (x1 y1 x2 y2)
    (let ((x-mid (ash (+ x1 x2) -1))
          (y-mid (ash (+ y1 y2) -1)))
      (draw-point x-mid y-mid)
      (when (not (and (= x1 x2) (= y1 y2)))
        (draw-line-1 x1 y1 x-mid y-mid)
        (draw-line-1 x-mid y-mid x2 y2))))

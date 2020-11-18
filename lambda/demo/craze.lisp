;;; -*- Mode: Lisp; Package: HACKS; Lowercase: T; Base: 8 -*-

(special  spokes theta thetax thetay foox fooy)

(defmacro frandom () `(// (random 65535.) (small-float 65536.)))

(setq  3.141592654
      spokes 29.
      theta (*  2 (frandom))
      foox (cos theta)
      fooy (sin theta)
      theta  (// (*  2) spokes)
      thetax    (cos theta)
      thetay (sin theta))

(defun clipped-line (x0 y0 x1 y1 xl yl xh yh &optional (sheet tv:selected-window) &aux clipped-p)
  (if (< x0 xl) (setq x0 xl
                      clipped-p t))
  (if (< x1 xl) (setq x1 xl
                      clipped-p t))
  (if (> x0 xh) (setq x0 xh
                      clipped-p t))
  (if (> x1 xh) (setq x1 xh
                      clipped-p t))
  (if (< y0 yl) (setq y0 yl
                      clipped-p t))
  (if (< y1 yl) (setq y1 yl
                      clipped-p t))
  (if (> y0 yh) (setq y0 yh
                      clipped-p t))
  (if (> y1 yh) (setq y1 yh
                      clipped-p t))
  (funcall sheet ':draw-line x0 y0 x1 y1 tv:alu-xor)
  clipped-p)

(defun crack (p x0 y0 x1 y1 xl yl xh yh &optional (sheet tv:selected-window) &aux x y)
  (if (> (frandom) p)
      (clipped-line x0 y0 x1 y1 xl yl xh yh sheet)
      (progn
        (setq p (* p p)
              x (fix (+ x0 (* (frandom) (- x1 x0))))
              y (fix (+ y0 (* (frandom) (- y1 y0)))))
        (or (crack p x0 y0 x y xl yl xh yh sheet)
            (crack p x y x1 y1 xl yl xh yh sheet)))))

(defun jag-ray (p pl x0 y0 dx dy xl yl xh yh &optional (screen tv:selected-window) last-ray
                  &aux x1 y1 this-ray)
  (do ()
      ((clipped-line x0 y0
                     (setq x1 (fix (+ x0 (* 2 (frandom) dx))))
                     (setq y1 (fix (+ y0 (* 2 (frandom) dy))))
                     xl yl xh yh screen) (reverse this-ray))
    (setq x0 x1 y0 y1)
    (setq this-ray (cons (cons x1 y1) this-ray))
    (setq p (+ p (* (- pl p) .1)))
    (if (< (frandom) p)
        (setq last-ray (jag-ray p pl
                                x0 y0
                                (- (* dx thetax) (* dy thetay))
                                (+ (* dy thetax) (* dx thetay))
                                xl yl xh yh screen last-ray)))
    (if last-ray (progn
                   (if (< (frandom) (- 1.0 (// p pl)))
                       (crack .85 x1 y1 (caar last-ray) (cdar last-ray) xl yl xh yh screen))
                   (setq last-ray (cdr last-ray))))))


(defun outspoken (x0 y0 xl yl xh yh &optional (sheet tv:selected-window)
                     &aux (dx foox) (dy fooy) ray ray0)
  (dotimes (n spokes)
    (setq ray (jag-ray 0 .15 x0 y0
                       (// (* dx (- xh xl)) 23.)
                       (// (* dy (- yh yl)) 23.)
                       xl yl xh yh sheet ray))
    (if (= n 0) (setq ray0 ray))
    (psetq dx (- (* dx thetax) (* dy thetay))
           dy (+ (* dy thetax) (* dx thetay))))
  (dotimes (n (min (length ray) (length ray0)))
    (crack .85 (caar ray) (cdar ray) (caar ray0) (cdar ray0) xl yl xh yh sheet)
    (setq ray (cdr ray) ray0 (cdr ray0))))


(defun craze (&optional (sheet tv:selected-window)
                        (xl (tv:sheet-inside-left sheet))
                        (yl (tv:sheet-inside-top sheet))
                        (xh (tv:sheet-inside-width sheet))
                        (yh (tv:sheet-inside-height sheet)))
  (outspoken (+ xl (fix (* (- xh xl) (+ .15 (* .7 (frandom))))))
             (+ yl (fix (* (- yh yl) (+ .15 (* .7 (frandom))))))
             xl yl xh yh sheet))

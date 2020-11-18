;;; -*- Mode: Lisp; Package: Hacks; Lowercase: Yes; Base: 10 -*-

;;; Dcrock.
;;; Note: This file is in decimal, not octal.

;;; Settable parameters.
(defconst *dc-height* 80)       ; Character height in pixels.
(defconst *dc-width* 60)        ; Character width in pixels.
(defconst *dc-thickness* 6)     ; Character stroke thickness.
(defconst *dc-offset* 10)       ; Slant -- difference in x pos from bottom to top.
(defconst *dc-colon-size* 7)    ; Width and height of colon dots.
(defconst *dc-char-spacing* 25) ; Char-to-char spacing.
(defconst *dc-colon-spacing* 20)        ; Char-to-colon spacing.
(defconst *dc-colon-separation* 20)
(defconst *dc-start-x* 10)      ; Distance from left edge of window to first digit.
(defconst *dc-start-y* 10)      ; Distance from top edge of window to tops of digits.

;;; Derived parameters.
(defconst *dc-total-width*      ; Width of working part of window.
  (+ (* 6 *dc-width*)           ; six digits
     (* 3 *dc-char-spacing*)    ; three char-to-char gaps
     (* 2 *dc-colon-size*)      ; two colons
     (* 4 *dc-colon-spacing*)   ; four char-to-colon gaps
     (* 2 *dc-start-x*)
     *dc-offset*))
(defconst *dc-total-height* (+ *dc-height* (* 2 *dc-start-y*)))
(defconst *dc-half-thickness*
  (truncate *dc-thickness* 2))
(defconst *dc-half-height*
  (truncate *dc-height* 2))
(defconst *dc-slope* (// (small-float *dc-offset*) *dc-height*))

(defconst *dc-whiteness* (make-array '(32 32) ':type 'art-1b))
(dotimes (x 32)
  (dotimes (y 32)
    (aset 0 *dc-whiteness* x y)))

(defconst *dc-x-offsets* (make-array 6))
(loop for form in '((- *dc-start-x* *dc-width*)
                    *dc-char-spacing*
                    (+ *dc-colon-size* (* 2 *dc-colon-spacing*))
                    *dc-char-spacing*
                    (+ *dc-colon-size* (* 2 *dc-colon-spacing*))
                    *dc-char-spacing*)
      for i from 0
      with x = 0 do
      (setq x (+ x *dc-width* (eval form)))
      (aset x *dc-x-offsets* i))

(defmacro draw-stroke (x1-form y1-form width-form height-form array-form)
  `(let* ((y1 (+ base-y ,y1-form))
          (x1 (+ base-x ,x1-form (fix (* (- *dc-height* ,y1-form) *dc-slope*))))
          (x1-offset (+ base-x ,x1-form (fix (* (- *dc-height* (+ ,y1-form ,height-form)) *dc-slope*)))))
     (sys:%draw-triangle
      x1 y1
      x1-offset (+ y1 ,height-form)
      (+ x1-offset ,width-form) (+ y1 ,height-form)
      tv:alu-ior ,array-form)
     (sys:%draw-triangle
      x1 y1
      (+ x1 ,width-form) y1
      (+ x1-offset ,width-form) (+ y1 ,height-form)
      tv:alu-ior ,array-form)))

(defun draw-stroke-one (base-x base-y array)
  (draw-stroke 0
               0
               *dc-width*
               *dc-thickness*
               array))

(defun draw-stroke-two (base-x base-y array)
  (draw-stroke (- *dc-width* *dc-thickness*)
               0
               *dc-thickness*
               (+ *dc-half-height* *dc-half-thickness*)
               array))

(defun draw-stroke-three (base-x base-y array)
  (draw-stroke (- *dc-width* *dc-thickness*)
               (- *dc-half-height* *dc-half-thickness*)
               *dc-thickness*
               (+ *dc-half-height* *dc-half-thickness*)
               array))

(defun draw-stroke-four (base-x base-y array)
  (draw-stroke 0
               (- *dc-height* *dc-thickness*)
               *dc-width*
               *dc-thickness*
               array))

(defun draw-stroke-five (base-x base-y array)
  (draw-stroke 0
               (- *dc-half-height* *dc-half-thickness*)
               *dc-thickness*
               (+ *dc-half-height* *dc-half-thickness*)
               array))

(defun draw-stroke-six (base-x base-y array)
  (draw-stroke 0
               0
               *dc-thickness*
               (+ *dc-half-height* *dc-half-thickness*)
               array))

(defun draw-stroke-seven (base-x base-y array)
  (draw-stroke 0
               (- *dc-half-height* *dc-half-thickness*)
               *dc-width*
               *dc-thickness*
               array))

(defun draw-colon (base-x base-y array)
  (draw-stroke 0
               (+ *dc-half-height* *dc-colon-separation*)
               *dc-colon-size*
               *dc-colon-size*
               array)
  (draw-stroke 0
               (- *dc-half-height* (+ *dc-colon-size* *dc-colon-separation*))
               *dc-colon-size*
               *dc-colon-size*
               array))

(defconst *dc-stroke-list*
           '(draw-stroke-one draw-stroke-two draw-stroke-three draw-stroke-four
             draw-stroke-five draw-stroke-six draw-stroke-seven))

(defconst *dc-digit-array*
          (make-array 10 :initial-contents '((  t   t   t   t   t   t nil)      ;0
                                             (nil   t   t nil nil nil nil)      ;1
                                             (  t   t nil   t   t nil   t)      ;2
                                             (  t   t   t   t nil nil   t)      ;3
                                             (nil   t   t nil nil   t   t)      ;4
                                             (  t nil   t   t nil   t   t)      ;5
                                             (  t nil   t   t   t   t   t)      ;6
                                             (  t   t   t nil nil nil nil)      ;7
                                             (  t   t   t   t   t   t   t)      ;8
                                             (  t   t   t   t nil   t   t)      ;9
                                             )))

(defun draw-digit (digit base-x base-y array)
  (if (not (null digit))
      (loop for on-p in (aref *dc-digit-array* digit)
            for fcn in *dc-stroke-list*
            when on-p do
            (funcall fcn base-x base-y array))))

(defflavor dc-window
  (old-digits new-digits working-array-1 working-array-2 go)
  (tv:process-mixin tv:window)
  (:default-init-plist :process '(dc-top-level)
                       :blinker-p nil :label nil))

(defmethod (dc-window :after :init) (&rest ignore)
  (setq old-digits (make-array 6)
        new-digits (make-array 6)
        working-array-1 (tv:make-sheet-bit-array self (+ *dc-width* *dc-offset*) *dc-height*)
        working-array-2 (tv:make-sheet-bit-array self (+ *dc-width* *dc-offset*) *dc-height*)
        go nil))

(defmacro defun-dc (arglist &body body)
  `(declare-flavor-instance-variables (dc-window)
      (defun ,arglist . ,body)))

(defun-dc change-digit (old-digit new-digit base-x base-y)
  (bitblt tv:alu-seta (+ *dc-width* *dc-offset*) *dc-height*
          *dc-whiteness* 0 0
          working-array-1 0 0)
  (bitblt tv:alu-seta (+ *dc-width* *dc-offset*) *dc-height*
          *dc-whiteness* 0 0
          working-array-2 0 0)
  (draw-digit old-digit 0 0 working-array-1)
  (draw-digit new-digit 0 0 working-array-2)
  (bitblt tv:alu-xor (+ *dc-width* *dc-offset*) *dc-height*
          working-array-1 0 0
          working-array-2 0 0)
  (bitblt tv:alu-xor (+ *dc-width* *dc-offset*) *dc-height*
          working-array-2 0 0
          tv:screen-array base-x base-y))

(defmethod (dc-window :after :refresh) (&optional ignore)
  (cond ((not tv:restored-bits-p)
         (get-new-time)
         (copy-array-contents new-digits old-digits)
         (dotimes (d 6)
           (draw-digit (aref old-digits d) (aref *dc-x-offsets* d) *dc-start-y*
                       tv:screen-array))
         (draw-colon (+ (aref *dc-x-offsets* 1) *dc-width* *dc-colon-spacing*)
                     *dc-start-y* tv:screen-array)
         (draw-colon (+ (aref *dc-x-offsets* 3) *dc-width* *dc-colon-spacing*)
                     *dc-start-y* tv:screen-array))))

(defun-dc update-time ()
  (get-new-time)
  (tv:prepare-sheet (self)
    (dotimes (d 6)
      (let ((old (aref old-digits d))
            (new (aref new-digits d)))
        (if (neq old new)
            (change-digit old new (aref *dc-x-offsets* d) *dc-start-y*)))))
  (copy-array-contents new-digits old-digits))

(defun-dc get-new-time ()
  (multiple-value-bind (sec min hou)
        (time:get-time)
    (and (zerop sec) (zerop (\ min 30.)) (fboundp 'play-time)
         (play-time (if (zerop min) (\ hou 12.) 1)))
    (put-digits hou 0)
    (put-digits min 2)
    (put-digits sec 4)
    (if (zerop (aref new-digits 0))
        (aset nil new-digits 0))))

(defun-dc put-digits (num i)
  (aset (truncate num 10) new-digits i)
  (aset (\ num 10) new-digits (1+ i)))

(defun dc-top-level (window)
  (time:get-time)               ; Make sure timebase initialized
  (funcall window ':top-level))

(defmethod (dc-window :top-level) ()
  (process-wait "go" #'car (locate-in-instance self 'go))
  (loop doing
        (update-time)
        (process-sleep 30.)))

(defvar *dc* nil)

(defun dc (&optional (superior tv:mouse-sheet))
  (if (null *dc*)
      (setq *dc* (tv:make-window 'dc-window
                                 ':superior superior
                                 ':width *dc-total-width*
                                 ':height *dc-total-height*)))
  (multiple-value-bind (w h)
      (funcall superior ':inside-size)
    (funcall *dc* ':center-around (truncate w 2) (truncate h 2)))
  (funcall *dc* ':start)
  (funcall *dc* ':expose))

(defmethod (dc-window :start) ()
  (setq go t))

(compile-flavor-methods dc-window)

(defdemo "Digital Crock" "Standard digital clock" (dc-demo))

(defun dc-demo ()
  (dc)
  (funcall tv:selected-window ':tyi)
  (funcall *dc* ':bury))

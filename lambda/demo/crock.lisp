;;;-*-mode:lisp;package:hacks;lowercase:t;base:8-*-

;;; Magic number
(defvar *crock-radius* 500)

(defvar 2pi 6.2832s0)

(defflavor crock-window
        (hour-hand
         minute-hand
         second-hand
         (center-x nil)
         (center-y nil))
        (tv:process-mixin tv:window)
  (:default-init-plist :process '(crock-top-level)
                       :font-map '(fonts:43vxms)
                       :blinker-p nil :label nil))

(defun crock-top-level (window)
  (time:get-time)                               ;Make sure timebase initialized
  (funcall window ':top-level))

;; Hour hand
(defvar *hour-hand* '((6 0) (6 1) (6 2) (4 3) (5 4) (5 5) (6 6) (3 7) (6 10) (3 11) (6 12)
                      (3 13) (6 14) (6 15) (6 16) (14 17) (12 20) (11 17) (6 16) (6 15)
                      (3 14) (6 13) (3 12) (3 11) (3 10) (3 7) (3 6) (3 5) (3 4) (3 3) (3 2)
                      (4 1)))

;; Minute hand
(defvar *minute-hand* '((1 0 0) (1 0 1) (1 0 2) (2 0 3) (2 0 5) (1 0 6) (2 0 7)
                        (1 1 10) (1 2 10) (1 2 11) (3 3 11) (3 4 12) (3 3 11) (1 2 11)
                        (1 2 10) (1 1 10) (2 0 7) (11 0 6) (6 0 3) (100 0 2) (6 0 2) (6 0 3)
                        (6 0 4) (6 0 6) (6 0 7) (6 0 10) (6 0 11) (6 0 12) (7 0 13) (22 0 14)
                        (30 0 15) (14 0 14) (15 0 13) (6 0 12) (6 0 11) (6 0 10) (6 0 7)
                        (6 0 6) (6 0 5) (6 0 4) (6 0 3) (6 0 2) (7 0 1)))

(defmethod (crock-window :after :init) (ignore)
  (setq hour-hand (make-instance 'wide-hand ':curve *hour-hand*)
        minute-hand (make-instance 'wide-hand ':curve *minute-hand*)
        second-hand (make-instance 'thin-hand ':length *crock-radius*)))

(defmethod (crock-window :after :refresh) (&optional ignore)
  (cond ((not tv:restored-bits-p)
         (funcall-self ':draw-face)
         (funcall hour-hand ':set-current-angle nil)
         (funcall minute-hand ':set-current-angle nil)
         (funcall second-hand ':set-current-angle nil))))

(defflavor hand ((current-angle nil)) ()
  (:settable-instance-variables current-angle)
  (:required-methods :draw))

(defmethod (hand :draw-if-necessary) (window x0 y0 angle)
  (cond ((not (and current-angle (= angle current-angle)))
         (tv:prepare-sheet (window)
           (and current-angle (funcall-self ':draw window x0 y0 current-angle))
           (setq current-angle angle)
           (funcall-self ':draw window x0 y0 current-angle)))))

(defflavor thin-hand (length) (hand)
  (:initable-instance-variables length))

(defmethod (thin-hand :draw) (window x0 y0 angle)
  (setq angle (// (* angle 2pi) 360.))
  (funcall window ':draw-line x0 y0
           (fix (+ x0 (* length (cos angle))))
           (fix (+ y0 (* length (sin angle))))
           tv:alu-xor))

(defflavor wide-hand (curve) (hand)
  (:initable-instance-variables curve))

(defmethod (wide-hand :draw) (window x0 y0 angle)
  (draw-curve-at-angle window x0 y0 curve angle))

;;; Angle is in degrees
(defun draw-curve-at-angle (window x0 y0 curve angle &aux sin cos nseg)
  (setq angle (// (* angle 2pi) 360.)
        sin (sin angle)
        cos (cos angle))
  (setq nseg (loop for elem in curve maximize (length (cdr elem))))
  (tv:prepare-sheet (window)
    (loop for elem in curve
          with p1xs = (make-list nseg ':initial-value x0)
          and p1ys = (make-list nseg ':initial-value y0)
          and p2xs = (make-list nseg ':initial-value x0)
          and p2ys = (make-list nseg ':initial-value y0)
          for rx0 = (+ x0 (tv:sheet-inside-left window)) then rx1
          and ry0 = (+ y0 (tv:sheet-inside-top window)) then ry1
          as width = (pop elem)
          as rx1 = (+ rx0 (* width cos))
          and ry1 = (+ ry0 (* width sin))
          do (loop for width in elem
                   and p1xs on p1xs
                   and p1ys on p1ys
                   and p2xs on p2xs
                   and p2ys on p2ys
                   as dx = (* width sin)
                   and dy = (- (* width cos))
                   do (let ((p1x (car p1xs)) (p1y (car p1ys))
                            (p2x (car p2xs)) (p2y (car p2ys))
                            (p3x (fix (+ rx1 dx))) (p3y (fix (+ ry1 dy)))
                            (p4x (fix (- rx1 dx))) (p4y (fix (- ry1 dy))))
                        (sys:%draw-triangle p1x p1y p2x p2y p3x p3y tv:alu-xor window)
                        (sys:%draw-triangle p2x p2y p3x p3y p4x p4y tv:alu-xor window)
                        (rplaca p1xs p3x)
                        (rplaca p1ys p3y)
                        (rplaca p2xs p4x)
                        (rplaca p2ys p4y))))))

(defmethod (crock-window :draw-face) ()
  (multiple-value-bind (w h)
      (funcall-self ':inside-size)
    (setq center-x (truncate w 2)
          center-y (truncate h 2)))
  (funcall-self ':draw-filled-in-circle center-x center-y *crock-radius* tv:alu-xor)
  (funcall-self ':draw-filled-in-circle center-x center-y (- *crock-radius* 4) tv:alu-xor)
  (loop for i from 1. to 12.
        do (put-string-in-circle self center-x center-y *crock-radius*
                                 (format nil "~D" i)
                                 (+ -90.0s0 (* i '#,(truncate 360. 12.))))))

(defun put-string-in-circle (window center-x center-y radius string angle
                             &aux width height sin cos rad2)
  (setq width (truncate (compute-string-length string (tv:sheet-current-font window)) 2)
        height (truncate (font-char-height (tv:sheet-current-font window)) 2))
  (setq angle (// (* angle 2pi) 360.)
        sin (sin angle)
        cos (cos angle)
        rad2 (* radius radius))
  (loop with wid = (if (< cos -0.5s0) (+ width 10) width)
        and hei = (if (< sin -0.5s0) (+ height 4) height)
        for x = (+ center-x (* radius cos)) then (- x cos)
        and y = (+ center-y (* radius sin)) then (- y sin)
        when (< (+ (let ((dx (- (if ( x center-x) (- x wid) (+ x wid)) center-x)))
                     (* dx dx))
                   (let ((dy (- (if ( y center-y) (- y hei) (+ y hei)) center-y)))
                     (* dy dy)))
                rad2)
        do (funcall window ':set-cursorpos
                    (- x (if (> cos 0.5s0) (+ width 10) width))
                    (- y (if (> sin 0.5s0) (+ height 4) height))
                    ':pixel)
           (funcall window ':string-out string)
           (return nil)))

(defun compute-string-length (string font &aux cwt)
  (setq cwt (font-char-width-table font))
  (loop with len = (string-length string)
        for i from 0 below len
        as char = (aref string i)
        sum (cond ((= i (1- len)) (fed:font-char-min-raster-width font char))
                  (cwt (aref cwt char))
                  (t (font-char-width font)))))

(defmethod (crock-window :top-level) ()
  (process-wait "go" #'car (locate-in-instance self 'center-x))
  (loop doing
    (multiple-value-bind (sec min hou)
        (time:get-time)
      (setq hou (\ hou 12.))
      (and (zerop hou) (setq hou 12.))
      (funcall hour-hand ':draw-if-necessary self center-x center-y
               ;; Display accurate to 15 mins
               (+ -90.0s0 (// (* (+ (// (+ min 7.5s0) 15.) (* hou 4)) 360.) '#,(* 12. 4))))
      (funcall minute-hand ':draw-if-necessary self center-x center-y
               ;; Display accurate to 1/2 minute
               (+ -90.0s0 (// (* (+ (// (+ sec 15.) 30.) (* min 2)) 360.) '#,(* 60. 2))))
      (funcall second-hand ':draw-if-necessary self center-x center-y
               (+ -90.0s0 (* sec 6.)))
      (and (zerop sec) (zerop (\ min 30.))
           (play-time (if (zerop min) hou 1))))
    (process-sleep 30.)))                       ;Just less than a second

(defun play-time (nbong)
  (play-string
    "aaaaddddssssbbbbbb--bbbbssssddddaaaaaa--ddddaaaassssbbbbbb--bbbbssssddddaaaaaaaaaa")
  (loop for i from 0 below nbong
        do (process-sleep 15.)
           (sys:%beep 30000 4000000)))

(defvar *crock*)

(defun crock ()
  (or (boundp '*crock*)
      (let ((width (tv:sheet-inside-width tv:mouse-sheet))
            (height (tv:sheet-inside-height tv:mouse-sheet)))
        (setq *crock* (tv:make-window 'crock-window
                                      ':width (- width 40) ':left 20
                                      ':height (- height 200) ':top 100))))
  (funcall *crock* ':select))

(compile-flavor-methods crock-window thin-hand wide-hand)

(defdemo "Crock" "Standard wall clock" (crock-demo))

(defun crock-demo ()
  (crock)
  (funcall tv:selected-window ':tyi)
  (funcall *crock* ':bury))

;;; Some help in drawing these things

(comment

(defun make-curve-list-from-spline (point-list &aux px py cx cy len)
  (setq len (length point-list)
        px (make-array len)
        py (make-array len))
  (loop for (x y) in point-list
        as i from 0
        do (aset x px i)
           (aset y py i))
  (multiple-value (cx cy len)
    (tv:spline px py 20.))
  (loop for i from 0 below len
        with x0 = 0
        and y0 = 0
        as x1 = (fix (aref cx i))
        and y1 = (fix (aref cy i))
        when ( y1 y0)
        collect (list (- x1 (prog1 x0 (setq x0 x1)))
                      (prog1 y0 (setq y0 y1)))))
)

;  -*- Mode:LISP; Package:HACKS; Lowercase:YES; Base:8; Readtable:ZL -*-
;More QTVHAX

;       "Nunc Pulchritas pro Saeculis"
;                               --Ovid
;                                       and Devo

;;; Note: I have moved the hof window to HAKDEF so that other HACKS
;;; files can depend on it.

;;; Simple linear motion of the endpoints
(defun godel (x1-rate y1-rate x2-rate y2-rate &optional (slowness 4000))
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
    (godel-internal x1-rate y1-rate x2-rate y2-rate slowness)))

(defun godel-and-wait (x1-rate y1-rate x2-rate y2-rate &optional (slowness 4000))
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
    (godel-internal x1-rate y1-rate x2-rate y2-rate slowness)
    (send *hof-window* ':tyi)))

(defun godel-internal (x1-rate y1-rate x2-rate y2-rate &optional (slowness 4000))
 (with-real-time
    (send *hof-window* :set-label
          (format nil "Godel.  Velocity 1 = (~D,~D), velocity 2 = (~D,~D)."
                  x1-rate y1-rate x2-rate y2-rate))
    (do ((x1 0 (logand 1777 (+ x1 x1-rate)))
         (y1 0 (logand 1777 (+ y1 y1-rate)))
         (x2 0 (logand 1777 (+ x2 x2-rate)))
         (y2 0 (logand 1777 (+ y2 y2-rate)))
         (first t nil))
        ((send *hof-window* ':tyi-no-hang))
      (cond ((and (not first)
                  (memq x1 '(0 1000))           ;Pause at repeat point
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000)))
             (return nil)))
      (do ((i 1 (1+ i)))
          ((> i slowness)))
      (send *hof-window* :draw-line
                         (abs (- x1 1000))
                         (abs (- y1 1000))
                         (abs (- x2 1000))
                         (abs (- y2 1000))
                         tv:alu-xor))))

(defdemo "Godel (Insides of Escher)"
         "Display the basic algorithm used in Escher for various pairs of velocities."
         "Godel"
         ("0 1 1 0" "Run the algorithm for velocities (0,1) and (1,0)."
          (godel-and-wait 0 1 1 0))
         ("1 2 2 3" "Run the algorithm for velocities (1,2) and (2,3)."
          (godel-and-wait 1 2 2 3))
         ("0 1 3 2" "Run the algorithm for velocities (0,1) and (3,2)."
          (godel-and-wait 0 1 3 2))
         ("2 1 4 3" "Run the algorithm for velocities (2,1) and (4,3)."
          (godel-and-wait 2 1 4 3))
         ("0 2 1 3" "Run the algorithm for velocities (0,2) and (1,3)."
          (godel-and-wait 0 2 1 3))
         ("0 1 2 3" "Run the algorithm for velocities (0,1) and (2,3)."
          (godel-and-wait 0 1 2 3)))

;Same thing with 4-fold symmetry
(defun escher (x1-rate y1-rate x2-rate y2-rate &optional (slowness 1000))
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
      (escher-internal x1-rate y1-rate x2-rate y2-rate slowness)))

(defun escher-internal (x1-rate y1-rate x2-rate y2-rate &optional (slowness 1000))
  (send *hof-window* :set-label
                     (format nil "Escher.  Velocity 1 = (~D,~D), velocity 2 = (~D,~D)."
                             x1-rate y1-rate x2-rate y2-rate))
  (with-real-time
    (do ((x1 0 (logand 1777 (+ x1 x1-rate)))
         (y1 0 (logand 1777 (+ y1 y1-rate)))
         (x2 0 (logand 1777 (+ x2 x2-rate)))
         (y2 0 (logand 1777 (+ y2 y2-rate)))
         (first t nil))
        ((send *hof-window* :tyi-no-hang))
      (cond ((and (not first)
                  (memq x1 '(0 1000))           ;Pause at repeat point
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000)))
             (return nil)))
      (do ((i 1 (1+ i)))
          ((> i slowness)))
      (let ((x1 (abs (- x1 1000)))
            (y1 (abs (- y1 1000)))
            (x2 (abs (- x2 1000)))
            (y2 (abs (- y2 1000)))
            (s *hof-window*))
        (send s :draw-line x1 y1 x2 y2 tv:alu-xor)
        (send s :draw-line (- 1000 x1) y1 (- 1000 x2) y2 tv:alu-xor)
        (send s :draw-line x1 (- 1000 y1) x2 (- 1000 y2) tv:alu-xor)
        (send s :draw-line (- 1000 x1) (- 1000 y1) (- 1000 x2) (- 1000 y2) tv:alu-xor)))))

(defun escher-demo (&optional (slowness 0))
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
     (loop for x1 in '(2 0 1 0 0)
           for y1 in '(1 1 2 2 1)
           for x2 in '(4 3 2 1 2)
           for y2 in '(3 2 3 3 3)
           do (progn
                (send *hof-window* :clear-window)
                (escher-internal x1 y1 x2 y2 slowness)
                (send *hof-window* :tyi)))))

(defdemo "Escher (xor'ing lines)"
         "Display patters of xor'ing line segments whose endpoints act like billiard balls."
         (escher-demo))

;Hardly!
(defun bach (x1-rate y1-rate x2-rate y2-rate
             &optional (tempo 60000.) (keyhigh 500.) (keylow 10.))
 (with-real-time
  (do ((x1 0 (logand 1777 (+ x1 x1-rate)))
       (y1 0 (logand 1777 (+ y1 y1-rate)))
       (x2 0 (logand 1777 (+ x2 x2-rate)))
       (y2 0 (logand 1777 (+ y2 y2-rate))))
      ((send *standard-input* :tyi-no-hang))
    (let ((x1 (abs (- x1 1000)))
          (y1 (abs (- y1 1000)))
          (x2 (abs (- x2 1000)))
          (y2 (abs (- y2 1000))))
      (sys:%beep (+ (* x1 keylow) keyhigh) tempo)
      (sys:%beep (+ (* (- 1000 x1) keylow) keyhigh) tempo)
      (sys:%beep (+ (* x2 keylow) keyhigh) tempo)
      (sys:%beep (+ (* (- 1000 x2) keylow) keyhigh) tempo)
      (sys:%beep (+ (* y1 keylow) keyhigh) tempo)
      (sys:%beep (+ (* (- 1000 y1) keylow) keyhigh) tempo)
      (sys:%beep (+ (* y2 keylow) keyhigh) tempo)
      (sys:%beep (+ (* (- 1000 y2) keylow) keyhigh) tempo)))))

;This is godel but when both points are on corners, it's going to reverse
;so instead we arbitrarily increment all the coordinates by 1 to make it
;do something interestingly different.
(defun godel* (x1-rate y1-rate x2-rate y2-rate &optional (slowness 4000))
  (hof-window)
 (with-real-time
  (tv:window-call (*hof-window* :deactivate)
    (do ((x1 0 (logand 1777 (+ x1 x1-rate)))
         (y1 0 (logand 1777 (+ y1 y1-rate)))
         (x2 0 (logand 1777 (+ x2 x2-rate)))
         (y2 0 (logand 1777 (+ y2 y2-rate))))
        ((send *hof-window* :tyi-no-hang))
      (do ((i 1 (1+ i)))
          ((> i slowness)))
      (send *hof-window* :draw-line (+ 100 (abs (- x1 1000)))
                                    (+ 100 (abs (- y1 1000)))
                                    (+ 100 (abs (- x2 1000)))
                                    (+ 100 (abs (- y2 1000)))
                                    tv:alu-xor)
      (cond ((and (memq x1 '(0 1000))
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000)))
             (setq x1 (1+ x1) x2 (1+ x2) y1 (1+ y1) y2 (1+ y2))))))))

;This is the same as godel except that the rates can be flonums
(defun kupfer (x1-rate y1-rate x2-rate y2-rate &optional (slowness 4000))
  (hof-window)
 (with-real-time
  (setq x1-rate (small-float x1-rate)
        x2-rate (small-float x2-rate)
        y1-rate (small-float y1-rate)
        y2-rate (small-float y2-rate))
  (tv:window-call (*hof-window* :deactivate)
    (do ((xx1 0 (+ xx1 x1-rate))
         (yy1 0 (+ yy1 y1-rate))
         (xx2 0 (+ xx2 x2-rate))
         (yy2 0 (+ yy2 y2-rate))
         (first t nil)
         (x1)(y1)(x2)(y2))
        ((send *hof-window* :tyi-no-hang))
      (and (>= xx1 2000) (setq xx1 (- xx1 2000)))
      (and (>= xx2 2000) (setq xx2 (- xx2 2000)))
      (and (>= yy1 2000) (setq yy1 (- yy1 2000)))
      (and (>= yy2 2000) (setq yy2 (- yy2 2000)))
      (setq x1 (fix xx1) x2 (fix xx2) y1 (fix yy1) y2 (fix yy2))
      (cond ((and (not first)
                  (memq x1 '(0 1000))           ;Pause at repeat point
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000)))
             (send *hof-window* :tyi)))
      (do ((i 1 (1+ i)))
          ((> i slowness)))
      (send *hof-window* :draw-line (+ 100 (abs (- x1 1000)))
                                    (+ 100 (abs (- y1 1000)))
                                    (+ 100 (abs (- x2 1000)))
                                    (+ 100 (abs (- y2 1000)))
                                    tv:alu-xor)))))

(defun kupfer-gold (x1-rate y1-rate x2-rate y2-rate &optional (key 600.) (tempo 40000.))
  (hof-window)
 (with-real-time
  (setq x1-rate (small-float x1-rate)
        x2-rate (small-float x2-rate)
        y1-rate (small-float y1-rate)
        y2-rate (small-float y2-rate))
  (tv:window-call (*hof-window* :deactivate)
    (do ((xx1 0 (+ xx1 x1-rate))
         (yy1 0 (+ yy1 y1-rate))
         (xx2 0 (+ xx2 x2-rate))
         (yy2 0 (+ yy2 y2-rate))
         (first t nil)
         (x1)(y1)(x2)(y2))
        ((send *hof-window* :tyi-no-hang))
      (and (>= xx1 2000) (setq xx1 (- xx1 2000)))
      (and (>= xx2 2000) (setq xx2 (- xx2 2000)))
      (and (>= yy1 2000) (setq yy1 (- yy1 2000)))
      (and (>= yy2 2000) (setq yy2 (- yy2 2000)))
      (setq x1 (fix xx1) x2 (fix xx2) y1 (fix yy1) y2 (fix yy2))
      (cond ((and (not first)
                  (memq x1 '(0 1000))           ;Pause at repeat point
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000)))
             (send *hof-window* :tyi)))
      (let ((angle (atan* (small-float (- (abs (- y2 1000)) (abs (- y1 1000))))
                          (small-float (- (abs (- x2 1000)) (abs (- x1 1000)))))))
        (and (> angle 3.14) (setq angle (- 6.28 angle)))
        (sys:%beep (+ 500. (fix (* angle key))) tempo))
      (send *hof-window* :draw-line (+ 100 (abs (- x1 1000)))
                                    (+ 100 (abs (- y1 1000)))
                                    (+ 100 (abs (- x2 1000)))
                                    (+ 100 (abs (- y2 1000)))
                                    tv:alu-xor)))))

(defun atan* (y x)
  (if (and (zerop y) (zerop x)) 0 (atan y x)))

;Symmetric xoring of 2 triangles
(defun birds (x y) (hack-in-m-silent 1 1 2 2 x y y x))

(defun hack-in-m-silent (x1-rate y1-rate x2-rate y2-rate x3-rate y3-rate x4-rate y4-rate
                                &optional (slowness 0))
  (hof-window)
   (tv:window-call (*hof-window* :deactivate)
     (hack-in-m-silent-internal x1-rate y1-rate x2-rate y2-rate
                                x3-rate y3-rate x4-rate y4-rate
                                slowness)))

(defun hack-in-m-silent-internal (x1-rate y1-rate x2-rate y2-rate x3-rate y3-rate x4-rate y4-rate
                                &optional (slowness 0))
  (send *hof-window* :set-label (format nil "Velocities: ~@{(~D,~D) ~}"
                                        x1-rate y1-rate x2-rate y2-rate x3-rate y3-rate x4-rate y4-rate))
  (with-real-time
    (do ((x1 0 (logand 1777 (+ x1 x1-rate)))
         (y1 0 (logand 1777 (+ y1 y1-rate)))
         (x2 0 (logand 1777 (+ x2 x2-rate)))
         (y2 0 (logand 1777 (+ y2 y2-rate)))
         (x3 0 (logand 1777 (+ x3 x3-rate)))
         (y3 0 (logand 1777 (+ y3 y3-rate)))
         (x4 0 (logand 1777 (+ x4 x4-rate)))
         (y4 0 (logand 1777 (+ y4 y4-rate)))
         (first t nil))
        ((or (and (not first)
                  (memq x1 '(0 1000))
                  (memq x2 '(0 1000))
                  (memq y1 '(0 1000))
                  (memq y2 '(0 1000))
                  (memq x3 '(0 1000))
                  (memq x4 '(0 1000))
                  (memq y3 '(0 1000))
                  (memq y4 '(0 1000)))
             (send *hof-window* :tyi-no-hang)))
      (do ((i 1 (1+ i)))
          ((> i slowness)))
      (draw-tri (abs (- x1 1000))
                (abs (- y1 1000))
                (abs (- x2 1000))
                (abs (- y2 1000))
                (abs (- x3 1000))
                (abs (- y3 1000)))
      (draw-tri (abs (- x1 1000))
                (abs (- y1 1000))
                (abs (- x2 1000))
                (abs (- y2 1000))
                (abs (- x4 1000))
                (abs (- y4 1000))))))

(defun draw-tri (x1 y1 x2 y2 x3 y3)
  (send *hof-window* :draw-triangle x1 y1 x2 y2 x3 y3 tv:alu-xor))

(defun birds-demo (&optional (slowness 0))
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
     (loop with x1 := 1
           with y1 := 1
           with x2 := 2
           with y2 := 2
           for x3 in '(2 3 20 200 237 400)
           for y3 in '(3 4 30 300 259 500)
           with x4
           with y4
           do (progn
                (setq y4 x3 x4 y3)
                (send *hof-window* :clear-window)
                (hack-in-m-silent-internal x1 y1 x2 y2 x3 y3 x4 y4 slowness)
                (send *hof-window* :tyi)))))

(defdemo "Birds (xor'ing triangles)"
         "Display patters of xor'ing triangles whose endpoints act like billiard balls."
         (birds-demo))

;;; Arctangent plot.  By Danny, hacked further by DLW, 11/29/80

(defvar *atan-window* nil)
(defun atan-window nil
  (cond (*atan-window*)
        (t (setq *atan-window*
                 (tv:make-window 'tv:window
                                   ':edges '(300 300 500 500)
                                   ':blinker-p nil
                                   ':label nil)))))

(defun draw-atan (size mul &optional (window tv:main-screen))
  (atan-window)
  (tv:window-call (*atan-window* :deactivate)
    (draw-atan-internal size mul window)))

(defun atan-demo ()
  (atan-window)
  (tv:window-call (*atan-window* :deactivate)
     (loop for size in '(50. 50. 50.)
           for mul in '(50. 200. 500.)
           do (progn
                (send *atan-window* :clear-window)
                (draw-atan-internal size mul *atan-window*)
                (send *atan-window* :tyi)))))

(comment ;it isn't interesting enough to be in the menu.
(defdemo "Atan (arc tangent xor hack)"
         "Plot low-order bit of the arctangent of Y over X in a window."
         (atan-demo))
)

(defun draw-atan-internal (size mul &optional (window tv:main-screen))
  (with-real-time
    (let ((screen-array (tv:sheet-screen-array window))
          (center-x (truncate (tv:sheet-width window) 2))
          (center-y (truncate (tv:sheet-height window) 2)))
      (do ((x (- size) (1+ x)))
          ((= x size))
        (let ((total-x (+ x center-x)))
          (do ((y (- size) (1+ y)))
              ((= y size))
            (or (= x 0)
                (as-2-reverse
                  (fix (* mul (atan y x)))
                  screen-array
                  total-x (+ y center-y)))))))))

(defun draw-potn (size point-list)
  (tv:window-call (*atan-window* :deactivate)
    (draw-potn-internal size point-list *atan-window*)
    (send *atan-window* :tyi))
  nil)

(defun potn-demo ()
  (tv:window-call (*atan-window* :deactivate)
     (loop for size in '(50. 50. 50.)
           for point-list in '()
        do (send *atan-window* :clear-window)
           (draw-potn-internal size point-list *atan-window*)
           (send *atan-window* :tyi))))

;;; Each point is (x y value).
(defun draw-potn-internal (size point-list &optional (window tv:main-screen))
  (with-real-time
    (let* ((screen-array (tv:sheet-screen-array window))
           (center-x (truncate (tv:sheet-width window) 2))
           (center-y (truncate (tv:sheet-height window) 2))
           (point-list (loop for point in point-list
                             collect (list (+ (first point) center-x)
                                           (+ (second point) center-y)
                                           (third point)))))
      (loop for x from (- center-x size) to (+ center-x size)
            do (loop for y from (- center-y size) to (+ center-y size)
                     do (as-2-reverse
                          (ldb 1001 (loop for point in point-list
                                          sum (let ((delta-x (- x (first point)))
                                                    (delta-y (- y (second point))))
                                                (* (third point)
                                                   (isqrt (+ (* delta-x delta-x)
                                                             (* delta-y delta-y)))))))
                          screen-array x y))))))

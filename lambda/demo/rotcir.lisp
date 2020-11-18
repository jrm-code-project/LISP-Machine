;;;Hey, EMACS, You hosehead!  This is -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

(defvar *circle* (make-instance 'tv:window
                                :borders 5
                                :activate-p nil
                                :blinker-p nil
                                :label '(:top
                                          :string "The Rotating circles hack!!!"
                                          :font fonts:cptfontb)))

(defun output ()
  *circle*)

(defun input ()
  *circle*)

(defun list-opers ()
  (send (output) :which-operations))

(defun clear ()
  (send (output) :clear-window))

(defun screen-size ()
  (multiple-value-list (send (output) :inside-size)))

(defun modified-origin ()
  (mapcar (lambda (x) (truncate x 2))
          (screen-size)))

(defun draw-circle (x y r)
  (send (output) :draw-circle x y r))

(defun find-center-point (r)
  (list (list (- (truncate (car (screen-size)) 2) r)
              (truncate (cadr (screen-size)) 2))
        (list (truncate (car (screen-size)) 2)
              (- (truncate (cadr (screen-size)) 2) r))
        (list (+ (truncate (car (screen-size)) 2) r)
              (truncate (cadr (screen-size)) 2))
        (list (truncate (car (screen-size)) 2)
              (+ (truncate (cadr (screen-size)) 2) r))))

(defun draw-all-circles (r angle)
  (draw-circle (car (modified-origin))
               (cadr (modified-origin))
               (+ 10 r))
  (draw-circle (+ (car (first (find-center-point r)))
                  (fix (* (- 1 (cosd angle)) r)))
               (+ (cadr (first (find-center-point r)))
                  (fix (* (sind angle) r)))
               r)
  (draw-circle (+ (car (second (find-center-point r)))
                  (fix (* (- (sind angle)) r)))
               (+ (cadr (second (find-center-point r)))
                  (fix (* (- 1 (cosd angle)) r)))
               r)
  (draw-circle (+ (car (third (find-center-point r)))
                  (fix (* (- (cosd angle) 1) r)))
               (+ (cadr (third (find-center-point r)))
                  (fix (* (- (sind angle)) r)))
               r)
  (draw-circle (+ (car (fourth (find-center-point r)))
                  (fix (* (sind angle) r)))
               (+ (cadr (fourth (find-center-point r)))
                  (fix (* (- (cosd angle) 1) r)))
               r))


(defun rotate-circles-once (step radius)
  (hacks:with-real-time (clear)
  (do ((angle 0 (+ step angle)))
      ((or (> angle 90.)
           (= angle 90.)))
    (draw-all-circles radius angle))))

(defun do-rc (step radius)
  (send *circle* :select)
  (rotate-circles-once step radius)
  (send *circle* :home-down)
  (send *circle* :string-out "Hit any key to flush:")
  (send *circle* :tyi)
  (send *circle* :kill))

(defvar *poppin-fresh* nil)

(defun poppin-fresh nil (cond (*poppin-fresh*)
                              (t (setq *poppin-fresh*
                                       (tv:make-window 'tv:pop-up-text-window
                                       ':borders 3
                                       ':activate-p nil
                                       ':label nil
                                       ':blinker-p t
                                       ':font-map (list fonts:cptfont)
                                       ':edges-from (list 100 100 (- (car (screen-size)) 100) 200)
                                       ':integral-p t)))))

(defun rotating-circles ()
  (poppin-fresh)
  (let ((*read-base* 10.)
        (*print-base* 10.))
    (send *circle* :expose)
    (send *circle* :select)
    (send *poppin-fresh* :expose)
    (send *poppin-fresh* :select)
    (send *poppin-fresh* :home-cursor)
    (prog (r c)
          (send *poppin-fresh* :string-out "Radius (0 - ) : ")
          (setq r (read-from-string (readline *poppin-fresh*)))
          (send *poppin-fresh* :string-out "Angle (1 - 90) : ")
          (setq c (read-from-string (readline *poppin-fresh*)))
          (send *poppin-fresh* :kill)
          (do-rc c r))))

(defdemo "Rotating Circles" "A spirograph crock" (rotating-circles))

;;; -*- Mode:LISP; Package:TV; Base:10; Readtable:CL -*-
;;; Author: jrm

(defvar *control-panel-screen*)

(defconst control-panel-width 128.)

(defflavor control-panel () (screen))

(defvar control-panel-position)

(defun initialize-control-panel ()
  (setq control-panel-position (list (- main-screen-width control-panel-width) 0))
  (when (not (zerop (remainder control-panel-width 32.)))
    (ferror nil "control-panel-width must be multiple of 32."))
  (when (= main-screen-width (* 32 25.))
    (ferror nil "bad monitor"))
  (setq *control-panel-screen*
        (define-screen 'control-panel "Control Panel"
          :area who-line-area
          :default-font fonts:cptfont
          :buffer          (+ main-screen-buffer-address
                              (floor (car control-panel-position) 32.))
          :control-address main-screen-control-address
          :property-list '(:video :black-and-white
                                  :controller :simple)
          :width              control-panel-width
          :height             (- main-screen-height (sheet-height who-line-screen))
          :locations-per-line main-screen-locations-per-line
          :position           control-panel-position))
  (set-console-size (car control-panel-position) main-screen-height)
  (send who-line-screen :deexpose)
  (send who-line-screen :change-of-size-or-margins
        :width 1024.
        :top (- main-screen-height (sheet-height who-line-screen)))
  (send who-line-run-state-sheet
        :change-of-size-or-margins :left 328. :right 520.)
  (send who-line-file-state-sheet
        :change-of-size-or-margins :left 520. :right 1024.)
  (send who-line-documentation-window
        :change-of-size-or-margins :width 1024.)
  (send who-line-screen :expose)
  (make-initial-control-panel-window)
  )

(defvar *control-panel*)

(defflavor control-panel-window ()
           (tv:stream-mixin tv:borders-mixin tv:label-mixin
            tv:graphics-mixin tv:minimum-window))

(defun make-initial-control-panel-window ()
  (when (and (boundp '*control-panel*)
             *control-panel*)
    (send *control-panel* :kill))

  (setq *control-panel*
        (make-instance 'control-panel-window
                       :superior *control-panel-screen*
                       :borders '(0 2 1 2)
                       :blinker-p nil
                       :label '(:string "LISP Machine, Inc."
                                :centered
                                :font fonts:tvfont)
                       ))
  (send *control-panel* :expose)
  )

#|

;;; The following code implements a meter allocator.

(defvar *meter-list*)

(defvar *default-meter-width* (floor control-panel-width 2))
(defvar *default-meter-height* (floor control-panel-width 2))



(defun set-up-meters ()
  (setq *meter-list* nil)
  (push (make-instance 'meter-window
                       :superior *control-panel*
                       :height 128.)
        *meter-list*))

(defun check-control-panel ()
  (dolist (w (send *control-panel* :inferiors))
    (when (not (send w :exposed-p))
      (ferror nil "bad configuration"))))

(defun find-place-for-meter (width height &aux meters)
  (check-control-panel)
  (setq meters (copylist (send *control-panel* :inferiors)))
  (unwind-protect
      (tv:delaying-screen-management
        (mapcar #'(lambda (m)
                    (send m :deexpose)
                    (send m :deactivate)) meters)
        (mapcar #'(lambda (m)
                    (let ((left (send *control-panel* :left-margin-size))
                          (top (send *control-panel* :top-margin-size)))
                      (send *control-panel* :draw-rectangle
                            (send m :width)
                            (send m :height)
                            (- (send m :x-offset) left)
                            (- (send m :y-offset) top)
                            )))
                meters)

        (let ((control-panel-width (send *control-panel* :inside-width))
              (control-panel-height (send *control-panel* :inside-height))
              (left-margin (send *control-panel* :left-margin-size))
              (top-margin (send *control-panel* :top-margin-size))
              (array (send *control-panel* :screen-array))
              )
          (do ((y top-margin (1+ y)))
              ((= y control-panel-height))
            (do ((x left-margin (1+ x)))
                ((= x control-panel-width))
              (mouse-warp 600. y)
              (when (>= (+ x width) control-panel-width)
                (return nil))
              (when (zerop (ar-2-reverse array x y))
                (when (acceptable-position-p x y width height array)
                  (return-from find-place-for-meter (values x y))))))))
    (mapcar #'(lambda (m)
                (send m :expose))
            meters)
    ))

(defun acceptable-position-p (x y width height array)
  (do ((check-y y (1+ check-y))
       (end-y (+ y height)))
      ((= check-y end-y))
    (do ((check-x x (1+ check-x))
         (end-x (+ x width)))
        ((= check-x end-x))
      (if (not (zerop (ar-2-reverse array check-x check-y)))
          (return-from acceptable-position-p nil))))
  t)

(defun make-new-meter (flavor width height &rest make-instance-args)
  (multiple-value-bind (x y)
      (find-place-for-meter width height)
    (apply #'make-instance
           flavor
           :x x
           :y y
           :width width
           :height height
           :superior *control-panel*
           make-instance-args)))
|#

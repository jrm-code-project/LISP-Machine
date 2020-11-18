;;; -*- Mode:LISP; Package:TV; Readtable:CL; Base:10 -*-
;;; Author: jrm

(defflavor basic-gauge ((last-value 0)
                         last-bottom-x
                         last-bottom-y
                         last-top-x
                         last-top-y
                         internal-computer
                         )
           (process-mixin
            stream-mixin
            centered-label-mixin
            label-mixin
            borders-mixin
            graphics-mixin
            minimum-window)
  :settable-instance-variables
  (:default-init-plist
    :blinker-p nil
    :borders   nil
    :label     nil
    :height    64
    :width     64
    )
  (:method-combination (:pass-on (:base-flavor-last value)
                                 :set-value))
  )

(defmethod (basic-gauge :clear) ()
  (send self :draw-rectangle (send self :width) (send self :height) 0 0 alu-andca)
  (erase-label))

(defvar *overlap-ratio* .5
  "Ratio of radius of little circle at bottom of gauge to gauge radius.")

(defvar *needle-ratio* 1.6
  "Ratio of radius of imaginary circle over which the needle tip travels
to gauge radius.")

(defconstant pi/2 (/ pi 2))

(defmethod (basic-gauge :draw-gauge-and-create-computer) ()
  "Draws a gauge and returns a procedure of one argument that can calculate
where the ends of the needle are."
  (let ((center-x (truncate (1- (send self :inside-width)) 2))
        (center-y (truncate (1- (send self :inside-height)) 2)))
    (let ((radius (min center-x center-y)))
      (let ((needle-source-y (+ radius center-y)))
        (send self :draw-circle center-x center-y radius)
        (labels ((magic-formula (ratio)
                   (- pi/2 (asin (/ ratio 2))))
                 (compute-radial-line-end (ratio theta)
                   (values (truncate (- center-x         (* ratio radius (cos theta))))
                           (truncate (- needle-source-y  (* ratio radius (sin theta)))))))
          (let ((bottom-theta (magic-formula *overlap-ratio*)))
            (send self :draw-circular-arc center-x needle-source-y
                  (truncate (* radius *overlap-ratio*))
                  (- pi/2 bottom-theta) (+ pi/2 bottom-theta)))
          (let ((needle-maximum-theta (magic-formula *needle-ratio*)))
            #'(lambda (percent)
                (let ((valid-percent (max (min percent 1.) -1.)))
                  (let ((needle-theta (- pi/2 (* (- valid-percent) needle-maximum-theta))))
                    (multiple-value-call
                      #'values
                      (compute-radial-line-end *overlap-ratio* needle-theta)
                      (compute-radial-line-end *needle-ratio* needle-theta)))))))))))

(defmethod (basic-gauge :set-value) (percent)
  "Position the needle on the gauge if gauge is exposed.  Noop if not.
Legit values range from -1 for far left to +1 for far right.  Zero is
centered."
  (if exposed-p
      (when (not (= percent last-value))
        (multiple-value-bind (bottom-x bottom-y top-x top-y)
            (funcall internal-computer percent)
          (without-interrupts
            (send self :draw-line last-bottom-x last-bottom-y last-top-x last-top-y alu-xor)
            (send self :draw-line bottom-x bottom-y top-x top-y alu-xor))
          (setq last-value    percent
                last-bottom-x bottom-x
                last-bottom-y bottom-y
                last-top-x    top-x
                last-top-y    top-y)))
          nil))

(defmethod (basic-gauge :redraw) ()
  (send self :clear)
  (setq internal-computer (send self :draw-gauge-and-create-computer))
  (multiple-value-setq (last-bottom-x last-bottom-y last-top-x last-top-y)
    (funcall internal-computer last-value))
  (send self :draw-line last-bottom-x last-bottom-y last-top-x last-top-y alu-xor)
  (draw-label)
  )

(defmethod (basic-gauge :after :expose) ()
  (send self :redraw))

(defmethod (basic-gauge :before :deexpose) (&rest ignore)
  (send self :clear))

(defmethod (basic-gauge :before :deactivate) (&rest ignore)
  (send self :set-label nil))

(defmethod (basic-gauge :after :refresh) (&rest ignore)
  (send self :redraw))

(defmethod (basic-gauge :before :change-of-size-or-margins) (&rest ignore)
  (erase-label))

;;; Mapping

;;; The mapping function gets called on the new needle value whenever a
;;; :set-value message is sent.  It is expected to produce a number between
;;; -1 and 1.

(defflavor gauge-mapping-mixin ((mapping-function #'(lambda (x) x))) ()
  :settable-instance-variables
  (:required-flavors basic-gauge))

(defmethod (gauge-mapping-mixin :pass-on :set-value) (new-value)
  (funcall mapping-function new-value))

;;; Some useful mapping functions.

(defun percent->gauge (percent)
  "Coerces a number between 0 and 100 into a number between -1 and 1."
  (- (/ percent 50.0) 1))

(defun fraction->gauge (small-number)
  "Coerces a number between 0 and 1 into a number between -1 and 1."
  (- (* small-number 2) 1))

;;; Probe

;;; This mixin gives you the :update message which will call the probe function
;;; The probe function is a procedure of no arguments which produces a value
;;; to use for :set-value.
;;; (send foo :update) <=> (send foo :set-value (funcall (send foo :probe-function)))

(defflavor gauge-probe-mixin (probe-function)
           ()
  :settable-instance-variables
  (:required-flavors basic-gauge))

(defmethod (gauge-probe-mixin :update) ()
  (multiple-value-call
    self
    :set-value (funcall probe-function)))

;;; Value in label mixin

;;; Whenever a :set-value message is sent, the label-function gets called on
;;; the value.  The label function should return two values: a label string
;;; and a flag to say whether the label should be updated (T means yes).  The flag
;;; is so we don't repaint the label all the time.

(defflavor gauge-value-in-label-mixin ((label-function ignore)) ()
  :settable-instance-variables
  (:required-flavors basic-gauge))

(defmethod (gauge-value-in-label-mixin :pass-on :set-value) (value)
  (multiple-value-bind
    (new-label changed?)
      (funcall label-function value)
    (when changed?
      (send self :set-label new-label)))
  value)

;;; Standard gauges.

(defflavor mapping-gauge ()             (gauge-mapping-mixin basic-gauge))
(defflavor mapping-gauge-with-value ()  (gauge-value-in-label-mixin gauge-mapping-mixin basic-gauge))
(defflavor probe-map-gauge ()           (gauge-probe-mixin gauge-mapping-mixin basic-gauge))
(defflavor probe-map-gauge-with-value ()(gauge-probe-mixin
                                         gauge-value-in-label-mixin
                                         gauge-mapping-mixin
                                         basic-gauge))

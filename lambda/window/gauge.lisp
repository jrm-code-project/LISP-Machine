;;; -*- Mode:LISP; Package:TV; Readtable:CL; Base:10 -*-

;; jrm wrote this

(defflavor basic-gauge ((last-value 0)
                         last-bottom-x
                         last-bottom-y
                         last-top-x
                         last-top-y
                         internal-computer
                         (tick-mark-generator #'end-tick-mark-generator)
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
  (tv:sheet-clear self t))

(defvar *overlap-ratio* .5
  "Ratio of radius of little circle at bottom of gauge to gauge radius.")

(defvar *needle-ratio* 1.4
  "Ratio of radius of imaginary circle over which the needle tip travels
to gauge radius.")

(defvar *scale-ratio* 1.7
  "Ratio of radius of circle upon which the tick marks are placed to the
gauge radius.")

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
          (let ((needle-maximum-theta (magic-formula *scale-ratio*)))
            (let ((compute-ends
                    #'(lambda (value end-ratio1 end-ratio2)
                        (let ((valid-value (max (min value 1.) -1.)))
                          (let ((theta (- pi/2 (* (- valid-value) needle-maximum-theta))))
                            (multiple-value-call
                              #'values
                              (compute-radial-line-end end-ratio1 theta)
                              (compute-radial-line-end end-ratio2 theta))))))
                  (full-size (- *scale-ratio* *needle-ratio*)))
              (dolist (mark (funcall tick-mark-generator))
                (multiple-value-bind (tick-bottom-x tick-bottom-y tick-top-x tick-top-y)
                    (funcall compute-ends
                             (first mark)
                             (- *scale-ratio* (* (second mark) full-size))
                             *scale-ratio*)
                  (send self :draw-line tick-bottom-x tick-bottom-y tick-top-x tick-top-y)))
              compute-ends)))))))

(defun end-tick-mark-generator ()
  `((-1 1) (1 1)))

(defmethod (basic-gauge :set-value) (percent)
  "Position the needle on the gauge if gauge is exposed.  Noop if not.
Legit values range from -1 for far left to +1 for far right.  Zero is
centered."
  (if exposed-p
      (when (not (= percent last-value))
        (multiple-value-bind (bottom-x bottom-y top-x top-y)
            (funcall internal-computer percent *overlap-ratio* *needle-ratio*)
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
  ;; Make sure updates don't get in here before the gauge is
  ;; finished redrawing.  This would leave needle turds on the
  ;; screen.
  (without-interrupts
    (send self :clear)
    (setq internal-computer (send self :draw-gauge-and-create-computer))
    (multiple-value-setq (last-bottom-x last-bottom-y last-top-x last-top-y)
      (funcall internal-computer last-value *overlap-ratio* *needle-ratio*))
    (send self :draw-line last-bottom-x last-bottom-y last-top-x last-top-y alu-xor)
    (send self :refresh-margins)))

(defmethod (basic-gauge :after :expose) (&rest ignore)
  (when (sheet-exposed-p self)
    (send self :redraw)))

(defmethod (basic-gauge :before :deexpose) (&rest ignore)
  (when (sheet-exposed-p self)
    (send self :clear)))

(defmethod (basic-gauge :after :refresh) (&rest ignore)
  (when (sheet-exposed-p self)
    (send self :redraw)))

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
  (- (let ((zunderflow t))
         (/ percent 50.0))
     1))

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
  (when (sheet-exposed-p self)
    (multiple-value-call self :set-value (funcall probe-function))))

;;; Value in label mixin

;;; Whenever a :set-value message is sent, the label-function gets called on
;;; the value.  The label function should return two values: a label string
;;; and a flag to say whether the label should be updated (T means yes).  The flag
;;; is so we don't repaint the label all the time.

(defflavor gauge-value-in-label-mixin ((label-function 'ignore)) ()
  :settable-instance-variables
  (:required-flavors basic-gauge))

(defmethod (gauge-value-in-label-mixin :pass-on :set-value) (value)
  (multiple-value-bind
    (new-label changed?)
      (funcall label-function value)
    (when changed?
      (send self :set-label new-label)))
  value)

;;; Named Gauge mixin

;;;This will put a name at the top of the gauge

(defflavor margin-name-mixin
         ((margin-name "") margin-name-area margin-name-font)
         ()
  (:required-flavors tv:minimum-window)
  (:inittable-instance-variables margin-name)
  (:settable-instance-variables margin-name))

(defmethod (margin-name-mixin :compute-margins) (lm tm rm bm)
  (let* ((font (send (sheet-get-screen self) :font-name-for :label))
         (name-height (font-char-height (font-evaluate font))))
    (setq margin-name-font font)
    (setq margin-name-area (list lm tm (- tv:width rm) (+ tm name-height)))
    (values lm (+ tm name-height) rm bm)))

(defmethod (margin-name-mixin :after :refresh-margins) ()
  (let ((font (font-evaluate margin-name-font)))
    (tv:sheet-force-access (self)
      (send self
            :string-out-centered-explicit
            margin-name
            (first margin-name-area)
            (second margin-name-area)
            (third margin-name-area)
            (fourth margin-name-area)
            font char-aluf
            0 nil
            (+ 2 (font-char-height font))))))

;;; Standard gauges.

(defflavor mapping-gauge ()             (gauge-mapping-mixin basic-gauge))
(defflavor mapping-gauge-with-value ()  (gauge-value-in-label-mixin gauge-mapping-mixin basic-gauge))
(defflavor mapping-gauge-with-value-and-name ()  (margin-name-mixin gauge-value-in-label-mixin
                                                  gauge-mapping-mixin basic-gauge))
(defflavor probe-map-gauge ()           (gauge-probe-mixin gauge-mapping-mixin basic-gauge))
(defflavor probe-map-gauge-with-value ()(gauge-probe-mixin
                                         gauge-value-in-label-mixin
                                         gauge-mapping-mixin
                                         basic-gauge))
(defflavor probe-map-gauge-with-value-and-name () (margin-name-mixin
                                                   gauge-probe-mixin
                                                   gauge-value-in-label-mixin
                                                   gauge-mapping-mixin
                                                   basic-gauge))

(compile-flavor-methods probe-map-gauge probe-map-gauge-with-value-and-name)

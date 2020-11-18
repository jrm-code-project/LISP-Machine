;;; -*- Mode:LISP; Package:TV; Readtable:CL; Base:10 -*-
;;; Author: jrm

(defun setup-landscape-with-gauges ()
  (generate-gc-gauges)
  (setup-gauge-configuration)
  (start-gauge-process))

(defun make-cheap-averager (averaging-factor)
  (let ((last-value 0)
        (inverse-factor (- 1 averaging-factor)))
    #'(lambda (next-value)
        (setq last-value (+ (* averaging-factor last-value)
                            (* inverse-factor   next-value)))
        last-value)))

(defvar *disk-average-value* .8)

(defun make-disk-usage-meter-function ()
  (let ((averager  (make-cheap-averager *disk-average-value*))
        (last-time (time:fixnum-microsecond-time))
        (last-disk-time (logand (read-meter 'sys:%disk-wait-time) (- (expt 2 24) 1))))
    #'(lambda ()
        (let ((now       (time:fixnum-microsecond-time))
              (this-wait (logand (read-meter 'sys:%disk-wait-time) (- (expt 2 24) 1))))
          (prog1 (funcall averager (* 100. (/ (time:time-difference this-wait last-disk-time)
                                              (time:time-difference now last-time))))
                 (setq last-time now
                       last-disk-time this-wait))))))

(defun total-processor-utilization (&aux (total 0))
  (without-interrupts
    (dolist (ape si:active-processes)
      (when (car ape)
        (incf total (send (car ape) :percent-utilization)))))
  (values (round total)))

(defvar *utilization-gauge*
        (make-instance 'probe-map-gauge
                       :superior         *control-panel*
                       :label            "Utilization"
                       :mapping-function #'percent->gauge
                       :probe-function   #'total-processor-utilization))

(defvar *disk-gauge*
        (make-instance 'probe-map-gauge
                       :superior         *control-panel*
                       :label            "Disk"
                       :mapping-function #'percent->gauge
                       :probe-function   (make-disk-usage-meter-function)))

(defvar *big-gauge-list* (list *disk-gauge* *utilization-gauge*))

(defconstant *number-of-volatility-levels* 4)
(defconstant *gc-gauges* (make-array *number-of-volatility-levels*))

(defun gc-probe (level)
  #'(lambda ()
      (let ((lc (aref gc:*level-control* level))
            (sdb (if (and (boundp 'gc:*gc-process*)
                          gc:*gc-process*
                          (send gc:*gc-process* :run-reasons))
                     gc:*most-recent-storage-distribution-block*  ;; try to avoid costly gc computations.
                     (gc:compute-storage-distribution))))
        (cond ((numberp lc) (/ (aref sdb level)(float lc)))
              ((null lc)    (/ (gc:committed-free-space level :batch       sdb)
                               (float si:virtual-memory-size)))
              (t            (/ (gc:committed-free-space level :incremental sdb) (aref sdb 5)))))))

(defun generate-gc-gauges ()
  (dotimes (v *number-of-volatility-levels*)
    (setf (aref *gc-gauges* v)
                (make-instance 'probe-map-gauge
                               :superior         tv:*control-panel*
                               :mapping-function #'fraction->gauge
                               :label            (format nil "Vol ~D" v)
                               :probe-function   (gc-probe v)))))

(defun setup-gauge-configuration ()
  ;; stack the gauges from bottom up
  (multiple-value-bind (left ignore right bottom) (send tv:*control-panel* :inside-edges)
    (let ((big-gauge-size (- right left)))
      (do ((big-gauges *big-gauge-list* (cdr big-gauges))
           (bottom bottom top)
           (top (- bottom big-gauge-size) (- top big-gauge-size)))
          ((null big-gauges)
           (let ((middle (truncate big-gauge-size 2)))
             (do ((v 0 (1+ v))
                  (flag nil (not flag)) ;T if going to next layer
                  (layer-bottom bottom      (if flag layer-top layer-bottom))
                  (layer-top (- bottom middle) (if flag (- layer-top middle) layer-top)))
                 ((= v *number-of-volatility-levels*) '())
               (let ((this-gauge (aref *gc-gauges* v)))
                 (send this-gauge :activate)
                 (send this-gauge :expose)
                 (send this-gauge :set-edges (if flag left middle) layer-top
                                             (if flag middle right) layer-bottom)
                 ))))
        (let ((this-gauge (first big-gauges)))
          (send this-gauge :activate)
          (send this-gauge :expose)
          (send this-gauge :set-edges left top right bottom)
          )))))

(defvar *gauge-process* (make-process "Gauges" :arrest-reasons '(:gauges-off)))
(defvar *gauge-process-sleep-time* 1.)

(defun gauge-process ()
  (do-forever
    (sleep *gauge-process-sleep-time*)
    (send *disk-gauge* :update)
    (send *utilization-gauge* :update)
    (dotimes (v *number-of-volatility-levels*)
      (send (aref *gc-gauges* v) :update))))

(defun start-gauge-process ()
  (send *gauge-process* :preset 'gauge-process)
  (send *gauge-process* :reset)
  (send *gauge-process* :run-reason :enable)
  (send *gauge-process* :revoke-arrest-reason :gauges-off))

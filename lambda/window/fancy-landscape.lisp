;;; -*- Mode:LISP; Package:TV; Readtable:CL; Base:10 -*-

;; jrm wrote this

(defun make-cheap-averager (averaging-factor)
  (let ((last-value 0)
        (inverse-factor (- 1 averaging-factor)))
    #'(lambda (next-value)
        (setq last-value (+ (or (ignore-errors (* averaging-factor last-value)) 0.0)
                            (or (ignore-errors (* inverse-factor   next-value)) 0.0)))
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

(defvar *utilization-gauge* nil
  "The window containing the processor utilization gauge")

(defvar *disk-gauge* nil
  "The window containing the disk usage gauge")

(defvar *big-gauge-list* nil "The list of big gauges")

(defun setup-landscape-with-gauges ()
  (setq *utilization-gauge* (make-instance 'probe-map-gauge
                                           :superior         *control-panel*
                                           :label            "Utilization"
                                           :mapping-function #'(lambda (percent)
                                                                 (percent->gauge (* percent 1.4)))
                                           :probe-function   #'total-processor-utilization))
  (setq *disk-gauge* (make-instance 'probe-map-gauge
                                    :superior         *control-panel*
                                    :label            "Disk"
                                    :mapping-function #'percent->gauge
                                    :probe-function   (make-disk-usage-meter-function)))
  (setq *big-gauge-list* (list *disk-gauge* *utilization-gauge*))
  (generate-gc-gauges)
  (setup-gauge-configuration)
  (when (fboundp 'net:setup-network-gauges)
    (net:setup-network-gauges))
  (start-gauge-process))

(defconstant *number-of-volatility-levels* 4)
(defvar *gc-gauges* (make-array *number-of-volatility-levels*))

(defun gc-probe (level)
  #'(lambda ()
      (let ((lc (aref gc::*level-control* level))
            (sdb (if (and (boundp 'gc::*gc-process*)
                          gc::*gc-process*
                          (send gc::*gc-process* :run-reasons))
                     gc::*most-recent-storage-distribution-block*  ;; try to avoid costly gc computations.
                     (if (= level 0)
                         (gc::compute-storage-distribution)
                         gc::*most-recent-storage-distribution-block*))))
        (cond ((numberp lc) (do ((vol level (1+ vol))
                                 (sum 0     (+ sum (aref sdb vol))))
                                ((= vol *number-of-volatility-levels*) (/ sum (float lc)))))
              ((null lc)    (/ (gc::committed-free-space level :batch       sdb)
                               (float si::virtual-memory-size)))
              (t            (/ (gc::committed-free-space level :incremental sdb) (aref sdb 5)))))))

(defun generate-gc-gauges ()
  (dotimes (v *number-of-volatility-levels*)
    (setf (aref *gc-gauges* v)
                (make-instance 'probe-map-gauge
                               :superior         tv:*control-panel*
                               :mapping-function #'fraction->gauge
                               :label            (format nil "Vol ~D" v)
                               :probe-function   (gc-probe v)))))

(defvar top-of-gc-gauges 0)

(defvar top-of-network-gauges 0)
(defvar network-gauge-bottom 0)
(defvar network-gauge-flag nil)

(defvar *network-gauges* nil "The list of network gauges")
(defvar network-gauge-lock nil "Lock on use of *network-gauges*")

(defun add-network-gauges (&rest gauges &aux new-list)
  (when (setq new-list (set-difference gauges *network-gauges*))
    (multiple-value-bind (left top right ignore) (send tv:*control-panel* :inside-edges)
      (let* ((middle (truncate (- right left) 2))
             (layer-bottom network-gauge-bottom)
             (layer-top (- network-gauge-bottom middle)))
        (dolist (g new-list)
          (send g :activate)
          (send g :set-edges (if network-gauge-flag middle left) layer-top
                (if network-gauge-flag right middle) layer-bottom)
          (unless (< layer-top top)
            (send g :expose))
          (setq top-of-network-gauges layer-top)
          (when network-gauge-flag
            (setq network-gauge-bottom layer-top)
            (setq layer-bottom layer-top)
            (decf layer-top middle))
          (setq network-gauge-flag (not network-gauge-flag)))))
    (with-lock (network-gauge-lock)
      (dolist (g new-list)
        (push g *network-gauges*)))))

(defun delete-network-gauges (&rest gauges &aux old-list new-list)
  (when (intersection gauges *network-gauges*)
    (with-lock (network-gauge-lock)
      (setq old-list *network-gauges*
            new-list (nreverse (set-difference *network-gauges* gauges))
            *network-gauges* nil))
    (delaying-screen-management
      (dolist (g old-list)
        (send g :deactivate)))
    (setq network-gauge-bottom top-of-gc-gauges)
    (setq network-gauge-flag nil)
    (apply 'add-network-gauges new-list)))

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
                  (flag nil (not flag))         ;T if going to next layer
                  (layer-bottom bottom      (if flag layer-top layer-bottom))
                  (layer-top (- bottom middle) (if flag (- layer-top middle) layer-top)))
                 ((= v *number-of-volatility-levels*))
               (let ((this-gauge (aref *gc-gauges* v)))
                 (send this-gauge :activate)
                 (send this-gauge :set-edges (if flag left middle) layer-top
                       (if flag middle right) layer-bottom)
                 (send this-gauge :expose))
               (setq top-of-gc-gauges layer-top)
               (setq network-gauge-bottom layer-top))))
        (let ((this-gauge (first big-gauges)))
          (send this-gauge :activate)
          (send this-gauge :set-edges left top right bottom)
          (send this-gauge :expose)
          )))))

(defvar *gauge-process* nil)
(defvar *gauge-process-sleep-time* 1.)

(defun gauge-process ()
  (do-forever
    (sleep *gauge-process-sleep-time*)
    (send *disk-gauge* :update)
    (send *utilization-gauge* :update)
    (dotimes (v *number-of-volatility-levels*)
      (send (aref *gc-gauges* v) :update))
    (with-lock (network-gauge-lock)
      (dolist (n *network-gauges*)
        (send n :update)))))

(defun start-gauge-process ()
  (when *gauge-process*
    (send *gauge-process* :kill))
  (setq *gauge-process* (make-process "Gauges" :arrest-reasons '(:gauges-off)))
  (send *gauge-process* :preset 'gauge-process)
  (send *gauge-process* :reset)
  (send *gauge-process* :run-reason :enable)
  (send *gauge-process* :revoke-arrest-reason :gauges-off))

(defvar fancy-landscape nil "T if fancy-landscape software loaded")

(defun fancy-landscape (&optional (enable t))
  "Set up landscape monitor with gauges.  T to enable, NIL to disable"
  (unless (or (not enable) fancy-landscape)
    (initialize-control-panel)
    (setq fancy-landscape t)
    (setup-landscape-with-gauges))
  (when fancy-landscape
    (send *control-panel-screen* (if enable :expose :deexpose)))
  enable)

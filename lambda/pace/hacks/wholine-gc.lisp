;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-

;;; Free 64000K+64000K; L3 1000K

(defvar old-level-3 1)
(defvar old-free-space 1)
(defvar old-fragments 1)

(defun who-line-gc-status-change-p ()
  (let ((level-3 (round (aref (gc:compute-storage-distribution) 3) 1024.))
        (free-space (round (si::unallocated-space) 1024.))
        (fragments (round (* gc:*unused-space-free-fraction* (si::unused-space)) 1024.)))
    (cond ((or (zerop old-level-3)
               (> (// (short-float (abs (- level-3 old-level-3))) old-level-3) 0.1s0)
               (zerop old-free-space)
               (> (// (short-float (abs (- free-space old-free-space))) old-free-space) 0.1s0)
               (zerop old-fragments)
               (> (// (short-float (abs (- fragments old-fragments))) old-fragments) 0.1s0))
           (setq old-level-3 level-3)
           (setq old-free-space free-space)
           (setq old-fragments fragments)
           t)
          (t
           nil))))

(defun who-line-gc-status-string ()
  (cond ((not (null gc:*gc-status*))
         (without-interrupts
           (let ((scavenged 0)
                 (scavengeable 0))
             (gc:with-quick-region-area-accessors
               (gc:for-every-region (region)
                 (when (= (si:%region-scavenge-enable region) 1)
                   (incf scavengeable (si:%region-free-pointer region))
                   (incf scavenged (si:%region-gc-pointer region)))))
             (format nil "Free ~6:dK+~6:dK; Flip ~d%"
                     old-free-space
                     old-fragments
                     (round (gc:percentage scavenged scavengeable))))))
        ((integerp (aref gc:*level-control* 3))
         (format nil "Free ~6:dK+~6:dK; L3 ~2d%"
                 old-free-space
                 old-fragments
                 (round (* 100. (// (short-float (* 1024. old-level-3)) (aref gc:*level-control* 3))))))
        (t
         (format nil "Free ~6:dK+~6:dK; L3 ~3:dK"
                 old-free-space
                 old-fragments
                 old-level-3))))

(defvar last-who-line-gc-error nil)
(defun who-line-gc-update-function (sheet)
  (ignore-errors
    (condition-case (x)
        (cond ((who-line-gc-status-change-p)
               (let ((s (who-line-gc-status-string)))
                 (cond ((< (send sheet :inside-width) (* 8 (string-length s)))
                        ;;you can't do :set-edges from the scheduler
                        (process-run-function "set edges" 'set-who-line-gc-width (* 8 (+ 2 (string-length s))))))
                 (sheet-clear sheet)
                 (sheet-string-out sheet s 0 (min (floor (send sheet :inside-width) 8)
                                                  (array-length s))))))
      (error (setq last-who-line-gc-error x)
             (sheet-clear sheet)
             (sheet-string-out sheet "Error getting GC stats")))))


(defvar who-line-gc-window nil)

(defun set-who-line-gc-width (width)
  (if (not (<= 0 width (floor (send who-line-screen :width) 2)))
      (ferror nil "bad width for gc window"))
  (send who-line-documentation-window :set-size
        (- (send who-line-screen :width) width)
        (send who-line-documentation-window :height))
  (send who-line-gc-window :set-edges
        (- (send who-line-screen :width) width)
        0
        (send who-line-screen :width)
        (send who-line-gc-window :height)))


(defun make-who-line-gc-window ()
  (when who-line-gc-window
    (send who-line-gc-window :kill)
    (setq who-line-gc-window nil))

  (send who-line-documentation-window :set-size
        (- (send who-line-screen :width)
           (* 8 (string-length (who-line-gc-status-string))))
        (send who-line-documentation-window :height))
  (setq who-line-gc-window
        (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                        :WHO-LINE-UPDATE-FUNCTION 'who-line-gc-update-function
                        :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                        :TOP 0
                        :left (send who-line-documentation-window :width)
                        :REVERSE-VIDEO-P nil)))

(defun turn-off-who-line-gc-window ()
  (when who-line-gc-window
    (send who-line-gc-window :kill)
    (setq who-line-gc-window nil)
    (send who-line-documentation-window :set-size
          (send who-line-screen :width)
          (send who-line-documentation-window :height))
    (who-line-update)))


(defun turn-on-who-line-gc-window ()
  (make-who-line-gc-window)
  (who-line-update))

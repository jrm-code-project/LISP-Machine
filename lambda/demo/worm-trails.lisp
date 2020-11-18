;;; -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

(defflavor wormy-flavor ()
           (tv:label-mixin tv:kbd-mouse-buttons-mixin tv:select-mixin
            tv:graphics-mixin tv:borders-mixin tv:stream-mixin
            tv:minimum-window))

(defvar *worm-step-length* 10.)
(defvar *decision-node-radius* 3)

(defvar *worm-pond* nil)
(defvar *blinker* nil)

(defvar *pond-dimensions*)
(defvar *x-delta-array* (make-array 9. :element-type '(signed-byte 16.)))
(defvar *y-delta-array* (make-array 9. :element-type '(signed-byte 16.)))
(defvar *direction-to-go* (make-array 256. :element-type '(mod 16.)))

(defconst pi//4 (// pi 4))
(defconst pi//8 (// pi 8.))
(defconst  0.0000001)

(defvar *diagonal-trails*)
(defvar *straight-trails*)
(defvar *move-list*)
(defvar *x-pos*)
(defvar *y-pos*)

(defun worm-trails ()
  (unwind-protect
      (progn
        (if (null *worm-pond*) (initialize-pond))
        (send *worm-pond* ':expose)
        (send *worm-pond* ':select)
        (do ((play-it-again t
                            (let ((*query-io* *worm-pond*))
                              (y-or-n-p "Again? "))))
            ((null play-it-again))
          (send *worm-pond* :clear-window)
          (let ((returnage (wander-around)))
            (send *worm-pond* ':set-cursorpos 0 0 ':character)
            (format *worm-pond*
                    "Died of ~A~&Length: ~A~&Moves were ~A~%"
                    (first returnage) (second returnage)
                    (nreverse (cddr returnage))))))
    (send *worm-pond* :deselect)))

(defun wander-around ()
  (let ((*x-pos* (truncate (car *pond-dimensions*) 2))  ;start in the center of the screen
        (*y-pos* (truncate (cadr *pond-dimensions*) 2))
        (*move-list* nil)
        (*straight-trails* 0)
        (*diagonal-trails* 0))
    (fill *direction-to-go* 0)  ;you don't know what to do!
    (do ((forced-move 1 (1+ forced-move))
         (forced-nodes '(254. 253. 251. 247. 239. 223. 191. 127.) (cdr forced-nodes)))
        ((null forced-nodes))
      (aset forced-move *direction-to-go* (car forced-nodes)))
    (hacks:with-real-time
      (do ((node (calculate-node) (calculate-node)))
          ((or (not (plusp *x-pos*))            ;run until you hit a wall
               (not (plusp *y-pos*))
               ( *x-pos* (car *pond-dimensions*))
               ( *y-pos* (cadr *pond-dimensions*)))
           (punt 'brain-damage))
        (if (= node 255.) (return (punt 'starvation))
            (move (if (zerop (aref *direction-to-go* node)) (ask-where-to-go node)
                      (aref *direction-to-go* node))))))))

(defun move (direction)
  (let ((new-x (+ *x-pos* (* (aref *x-delta-array* direction) *worm-step-length*)))
        (new-y (+ *y-pos* (* (aref *y-delta-array* direction) *worm-step-length*))))
    (send *worm-pond* ':draw-line
             *x-pos* *y-pos*
             new-x new-y tv:alu-ior)
    (if (evenp direction)
        (setq *diagonal-trails* (1+ *diagonal-trails*))
      (setq *straight-trails* (1+ *straight-trails*)))
    (setq *x-pos* new-x                         ;update global position of worm.
          *y-pos* new-y)))

(defun calculate-node ()
  (do ((node-value 0)
       (direction 1 (1+ direction))
       (pixel-value 1 (lsh pixel-value 1)))
      ((= direction 9.) node-value)
    (if (not (zerop (send *worm-pond* ':point
                             (+ *x-pos* (aref *x-delta-array* direction))
                             (+ *y-pos* (aref *y-delta-array* direction)))))
        (setq node-value (+ node-value pixel-value)))))

(defun ask-where-to-go (node &aux (offset (* *worm-trails-blinker-halfsize*
                                             *worm-trails-blinker-factor*)))
  (send *worm-pond* ':draw-circle *x-pos* *y-pos* *decision-node-radius*)
  (sys:%beep 500. 100000.)
  (setq tv:mouse-x *x-pos*
        tv:mouse-y *y-pos*)
  (send *blinker* ':set-cursorpos (- *x-pos* offset) (- *y-pos* offset))
  (send *blinker* ':set-visibility ':on)
  (tv:with-mouse-usurped
    (setq tv:who-line-mouse-grabbed-documentation
          "  Move mouse around to see possible worm moves -- Click any to select one.")
    (do ((over-line nil)
         (old-direction 0)
         (new-direction 0)
         (delta-mouse (multiple-value-list (tv:mouse-input))
                      (multiple-value-list (tv:mouse-input)))
         (mouse-x *x-pos* (fifth delta-mouse))
         (mouse-y *y-pos* (sixth delta-mouse)))
        ((and (not (zerop tv:mouse-last-buttons))
              (de-click)
              (not over-line))
         (send *blinker* ':set-visibility ':off)
         (learn-to-go-from node new-direction)
         new-direction)
      (send *blinker* ':set-cursorpos (- mouse-x offset) (- mouse-y offset))
      (setq new-direction (calculate-direction mouse-x mouse-y))
      (cond (( old-direction new-direction)
             (if (not over-line)
                 (ghost-line old-direction))
             (cond ((zerop (send *worm-pond* ':point
                                    (+ *x-pos* (aref *x-delta-array* new-direction))
                                    (+ *y-pos* (aref *y-delta-array* new-direction))))
                    (ghost-line new-direction)
                    (setq over-line nil))
                   (t
                    (sys:%beep 250. 10000.)
                    (setq over-line t)))
             (setq old-direction new-direction))))))

(defun de-click ()
  (do ((delta-mouse (multiple-value-list (tv:mouse-input))
                    (multiple-value-list (tv:mouse-input))))
      ((zerop tv:mouse-last-buttons)
       t)))

(defun ghost-line (direction)
  (send *worm-pond* ':draw-line
           *x-pos* *y-pos*
           (+ *x-pos* (* (aref *x-delta-array* direction) *worm-step-length*))
           (+ *y-pos* (* (aref *y-delta-array* direction) *worm-step-length*))
           tv:alu-xor nil)
  T)

(defun calculate-direction (x y)
  (do ((angle (atan (- *y-pos* y)
                    (+ (- x *x-pos*) )))
       (border-angle pi//8 (+ border-angle pi//4))
       (direction 1)
       (doop 2 (1+ doop)))
      ((= doop 10.)
       (if (= direction 9.) 1
         direction))
    (if (> angle border-angle)
        (setq direction doop))))

(defun learn-to-go-from (node direction)
  (setq *move-list* (cons direction *move-list*))
  (do ((rotated-nodes nil (cons temp-node rotated-nodes))
       (temp-node node (* temp-node 2))
       (temp-direction direction (1+ temp-direction)))
      ((mem #'= temp-node rotated-nodes))
    (if (> temp-node 255.)
        (setq temp-node (- temp-node 255.)))
    (if (> temp-direction 8.)
        (setq temp-direction (- temp-direction 8.)))
    (aset temp-direction *direction-to-go* temp-node)))

(defun punt (reason)
  (sys:%beep 2000. 100000.)
;  (setq tv:who-line-mouse-grabbed-documentation
;       "  hit any character when done gawking at screen")
  (send *worm-pond* :tyi)
  (cons reason (cons (+ *straight-trails*
;I think this number is more what the user wants to know - RMS.
                        *diagonal-trails*)
;                       (* *diagonal-trails* (sqrt 2.0)))
                               *move-list*)))

(defdemo "Worm-Trails" "Teach this ignorant worm to be efficient and live." (worm-trails))

(compile-flavor-methods wormy-flavor)

(defconst *worm-trails-blinker-factor* 2)
(defconst *worm-trails-blinker-halfsize* 6)

(defun initialize-pond ()
  (setq *worm-pond* (make-instance 'wormy-flavor
                                   :borders 5
                                   :label nil
                                   :activate-p t
                                   :expose-p nil
                                   :blinker-p nil)
        *blinker* (make-instance 'tv:magnifying-blinker
                                 :sheet *worm-pond*
                                 :height (* 2 *worm-trails-blinker-halfsize*
                                            *worm-trails-blinker-factor*)
                                 :width (* 2 *worm-trails-blinker-halfsize*
                                           *worm-trails-blinker-factor*)
                                 :x-offset (* *worm-trails-blinker-halfsize*
                                              *worm-trails-blinker-factor*)
                                 :y-offset (* *worm-trails-blinker-halfsize*
                                              *worm-trails-blinker-factor*)
                                 :magnification *worm-trails-blinker-factor*
                                 :half-period 10.
                                 :visibility :off)
        *pond-dimensions* (multiple-value-list (send *worm-pond* :size)))
  (fillarray *x-delta-array* '(0 1 1 0 -1 -1 -1 0 1))
  (fillarray *y-delta-array* '(0 0 -1 -1 -1 0 1 1 1)))

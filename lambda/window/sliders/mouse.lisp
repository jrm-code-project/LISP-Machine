;;; -*- Mode:LISP; Package:OBIE; Base:10 -*-

(defobclass mouseable-rect (window-rect))

(defobclass inverting-rect (window-rect)
  inverted-p)

(defobfun (invert inverting-rect) ()
  (send window :draw-rectangle width height x y tv:alu-xor)
  (setq inverted-p (not inverted-p)))

(defobfun (erase inverting-rect) ()
  (if inverted-p (invert))
  (shadowed-erase))

(defobfun (remove-frippery inverting-rect) ()
  (if inverted-p (invert))
  (shadowed-remove-frippery))

; Special hack for images
(defobfun (invert image-icon) ()
  (send window :bitblt tv:alu-xor width height mask 0 0 x y)
  (setq inverted-p (not inverted-p)))

(defobfun (invert char-icon) ()
  (send window :draw-char font shape-char x y tv:alu-xor))

(defobfun (draw inverting-rect) ()
  (shadowed-draw)
  (setq inverted-p (not inverted-p))                   ;sort of a hack
  (invert))

(defobclass mouse-highlighting-rect (inverting-rect mouseable-rect))

(defobfun (mouse-in mouse-highlighting-rect) ()
  (unless inverted-p (invert)))

(defobfun (mouse-out mouse-highlighting-rect) ()
  (when inverted-p (invert)))

; Temp, for demo
(defobfun (mouse-click mouse-highlighting-rect) (char x y)
  char
  (and (point-in-region (- x (tv:sheet-left-margin-size window)) (- y (tv:sheet-top-margin-size window)))
       (progn
         (drag))))


(defobclass dynamic-mouseable-text-icon (dynamic-text-icon mouse-highlighting-rect))
(defobclass dynamic-mouseable-image-icon (dynamic-image-icon mouse-highlighting-rect))
(defobclass dynamic-mouseable-char-icon (dynamic-char-icon mouse-highlighting-rect))

(defobfun test (&aux ike)
  (setq window (make-instance 'obie-window :edges-from :mouse :expose-p t :borders 10))
  (dolist (i '("foo" "bar" "blather" "ugh" "fnord"))
    (setq ike (oneof dynamic-mouseable-text-icon 'window window 'opaque-p t 'bordered-p nil 'x 0  'y 0 'font fonts:hl12b 'text i))
    (send window :add-object ike t)
;    (ask ike (draw))
    (ask ike (drag))))

(defobfun icon-madness (&aux ike window)
  (unless (boundp '*file-image*)
    (load "lad:efh;icons")
    (setq *file-image* (load-image "efh.icons;file")
          *file-group-image* (load-image "efh.icons;double-file")
          *dir-image* (load-image "efh.icons;file-folder")
          *host-image* (load-image "efh.icons;lambda")))
  (setq window (make-instance 'obie-window :edges-from :mouse :expose-p t :borders 10))
  (dotimes (i 5)
    (setq ike
          (caseq i
            (0 (oneof dynamic-mouseable-text-icon 'window window 'opaque-p t 'bordered-p nil 'x 0  'y 0 'font fonts:hl12b 'text "frobozz"))
            (1 (oneof dynamic-mouseable-image-icon 'window window 'x 0 'y 0 'image *host-image*))
            (2 (oneof dynamic-mouseable-char-icon 'window window 'x 0 'y 0 'font fonts:mouse 'shape-char #\ 'image-char #\0))
            (3 (oneof dynamic-mouseable-image-icon 'window window 'x 0 'y 0 'image *file-group-image*))
            (4 (oneof dynamic-mouseable-text-icon 'window window 'opaque-p t 'bordered-p nil 'x 0  'y 0 'font fonts:25fr3 'text "Icon Madness!"))              ;

))
    (send window :add-object ike t)
    (ask ike (drag))))

tv:
(defmacro obie:with-mouse-grabbed (&rest body)
  `(let ((.old.value. window-owning-mouse))
     (let-globally ((who-line-mouse-grabbed-documentation nil))
       (unwind-protect
         (progn
           (with-mouse-grabbed-internal t)
           . ,body)
         (setq window-owning-mouse .old.value.)
         (setq mouse-reconsider t)))))


;;; Tell the mouse process to switch "modes" and wait for it to do so
; this version can be called from mouse process (no-op)
tv:
(DEFUN WITH-MOUSE-GRABBED-INTERNAL (WOM &AUX (INHIBIT-SCHEDULING-FLAG T))
  (unless (eq si:current-process mouse-process)
    (SETQ WINDOW-OWNING-MOUSE WOM)
    (WHEN (NEQ WOM MOUSE-WINDOW)
      (SETQ MOUSE-RECONSIDER T
            INHIBIT-SCHEDULING-FLAG NIL )
      (PROCESS-WAIT "Grab Mouse" #'(LAMBDA (WOM) (AND (NULL MOUSE-RECONSIDER)
                                                      (EQ MOUSE-WINDOW WOM)))
                    WOM))))


(defobfun (drag window-point) (&aux starting-mouse-buttons)
  (setq starting-mouse-buttons (tv:mouse-buttons))
  (tv:with-mouse-grabbed
    (do-forever
      (tv:mouse-wait)
      (if (eql (tv:mouse-buttons) starting-mouse-buttons)
          (move (- tv:mouse-x (send window :x-offset) (tv:sheet-left-margin-size window))
                (- tv:mouse-y (send window :y-offset) (tv:sheet-top-margin-size window)))
        (return)))))


; system version is broken
tv:
(DEFUN MOUSE-WAIT (&OPTIONAL (OLD-X MOUSE-X) (OLD-Y MOUSE-Y) (OLD-BUTTONS MOUSE-LAST-BUTTONS)
                   (WHOSTATE "MOUSE"))
  "Wait for the mouse to move or a button transition.  For processes other than MOUSE-PROCESS.
If the arguments are supplied, we wait for the mouse state to be
different from them.  To avoid lossage, save the values of
MOUSE-X and MOUSE-Y and MOUSE-LAST-BUTTONS, use the saved values,
then pass the saved values to this function.
WHOSTATE is displayed in the who line while we wait."
  (PROCESS-WAIT WHOSTATE
    #'(LAMBDA (OLD-X OLD-Y OLD-BUTTONS)
        (OR ( MOUSE-X OLD-X)
            ( MOUSE-Y OLD-Y)
            ( (tv:%lambda-mouse-buttons) OLD-BUTTONS)))
    OLD-X OLD-Y OLD-BUTTONS))

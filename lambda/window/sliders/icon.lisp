;;; -*- Mode:LISP; Package:OBIE; Readtable:CL; Base:10 -*-
;;; Icons

#|
Char icons leave inexplicable turds.  Maybe just flush them.
|#

(defobclass icon (window-rect))

; Instances of this class are types of icons
(defobclass char-icon (icon)
  width
  height
  font
  image-char
  shape-char)

; An icon with a save-array
(defobclass dynamic-icon (icon)
  save-array)

(defobfun (exist char-icon) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (unless height
    (setq height (tv:font-char-height font))
    (setq width (if (tv:font-char-width-table font)
                    (aref (tv:font-char-width-table font) shape-char)
                  (tv:font-char-width font)))))

(defsubst 32ize (num)
  (* 32 (1+ (floor (1- num) 32))))

(defobfun (exist dynamic-icon) (&rest stuff &aux bitblt-width)
  (apply 'shadowed-exist stuff)
  (setq bitblt-width (32ize width))
  (unless save-array
    (setq save-array
          (make-array (list height bitblt-width) :element-type 'bit)))
  (draw))                                              ;+++ marginal

; An icon with a hot spot, for mouse cursors
(defobclass pointer (icon)
  hot-x
  hot-y)

(defobfun (draw char-icon) ()
  (send window :draw-char font shape-char x y tv:alu-andca)
  (send window :draw-char font image-char x y tv:alu-ior))

(defobfun (draw dynamic-icon) ()
  (tv:prepare-sheet (window)
    (bitblt tv:alu-seta width height (send window :screen-array)  (+ x (tv:sheet-left-margin-size window)) (+ y (tv:sheet-top-margin-size window)) save-array 0 0))
  (shadowed-draw))

(defobfun (erase dynamic-icon) ()
;  (shadowed-erase)
  (send window :bitblt tv:alu-seta width height save-array 0 0 x y))

(defobfun (move dynamic-icon) (nx ny)
  (erase)
  (shadowed-move nx ny)
  (draw))

(defobclass dynamic-char-icon (dynamic-icon char-icon))

;;; Text icons are made up of a text string.  Multiple lines are supported
(defobclass text-icon (icon)
  (opaque-p nil)                                       ;T means clear a box around the text
  (bordered-p t)
  text
  font)

(defobfun (exist text-icon) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (unless width
    (multiple-value-setq (width height)
      (send window :compute-motion text 0 nil 0 0 nil 0 nil nil nil font))
    (setq height (+ height  (tv:font-char-height font)))))


(defobfun (draw text-icon) ()
  (if opaque-p (send window :draw-rectangle width height x y tv:alu-andca))
  (if bordered-p (draw-border))
  (send window :string-out-explicit text (+ x (tv:sheet-left-margin-size window)) (+ y (tv:sheet-top-margin-size window))
        nil nil font tv:alu-ior 0 nil (tv:font-char-height font)))

(defobclass dynamic-text-icon (dynamic-icon text-icon))

(defobclass array-icon (icon)
  image-array
  mask-array)

(defobfun (draw array-icon) ()
  (send window :bitblt tv:alu-andca width height mask-array 0 0 x y)
  (send window :bitblt tv:alu-ior width height image-array 0 0 x y))

(defobfun (exist array-icon) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (unless image-array
    (when (or image-char (memq 'image-char stuff))                             ;Allow a font and char to specify the image
      (multiple-value-setq (image-array height width)
        (character-array (or font (cadr (memq 'font stuff))) (or image-char (cadr (memq 'image-char stuff)))))))
  (unless width
    (setq width (array-dimension image-array 0)
          height (array-dimension image-array 1)))
  (unless mask-array
    (setq mask-array (make-mask image-array)))
  )

(defobclass dynamic-array-icon (dynamic-icon array-icon))

;;; Interface to EFH stuff

; An icon with an image object
(defobclass image-icon (icon)
  image
  mask)

(defobfun (exist image-icon) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (unless width
    (setq width (ask image user:width)
          height (ask image user:height)))
  (setq mask (make-mask (ask image array))))

(defobfun (draw image-icon) ()
  (send window :bitblt tv:alu-andca width height mask 0 0 x y)
  (ask-funcall image 'user:draw x y window))

(defobclass dynamic-image-icon (dynamic-icon image-icon))

(defobfun test (&aux ike)
  (setq ike (oneof dynamic-image-icon 'window (tv:window-under-mouse) 'x 0 'y 0 'image user:*file-image*))
;  (setq ike (oneof dynamic-char-icon 'window (tv:window-under-mouse) 'x 0 'y 0 'font fonts:mouse 'shape-char #\ 'image-char #\0))  ;
; 'shape-char #\4 'image-char #\)
;  (setq ike (oneof dynamic-text-icon 'window (tv:window-under-mouse) 'x 0 'y 0 'font fonts:hl12b 'text "Yog shnozz" 'opaque-p t))
  (ask ike (drag))
  (ask ike (erase)))

(defobfun atest (&aux ike image-array)
  (do-forever
    (multiple-value-bind (left top right bottom)
        (tv:mouse-specify-rectangle)
      (let* ((width (- right left))
             (height (- bottom top))
             (bitblt-width (32ize width))
             (image (make-array (list height bitblt-width) :element-type '(mod 2))))
        (send tv:mouse-sheet :bitblt-from-sheet tv:alu-seta width height left top image 0 0)
        (setq ike (oneof dynamic-array-icon 'window tv:mouse-sheet 'x left 'y top 'width width 'height height
                         'image-array image
                         'mask-array (make-array (list height bitblt-width) :element-type '(mod 2) :initial-element 1)))
        (tv:mouse-warp left top)
        (ask ike (draw))
        (ask ike (drag))))))

(defobfun (drag dynamic-icon) (&aux starting-mouse-buttons)
  (setq starting-mouse-buttons (tv:mouse-buttons))
  (remove-frippery)
  (tv:with-mouse-grabbed
    (do-forever
      (tv:mouse-wait)
      (if (eql (tv:mouse-buttons) starting-mouse-buttons)
          (move (- tv:mouse-x (send window :x-offset) (tv:sheet-left-margin-size window))
                (- tv:mouse-y (send window :y-offset) (tv:sheet-top-margin-size window)))
        (return)))))

(defobfun obtest (fspec &aux icon)
  (setq icon (oneof dynamic-text-icon 'window (tv:window-under-mouse) 'x 0 'y 0 'font fonts:cptfont 'opaque-p t
                    'text (with-output-to-string (stream)
                            (grind-top-level fspec 100 stream))))
  (ask icon (drag)))

;;; Region hacking

(defobfun lasso-region (&aux last-x last-y (entry-buttons tv:mouse-last-buttons) list)
  (tv:with-mouse-grabbed
    (do-forever
      (tv:mouse-wait)
      (if (eq tv:mouse-last-buttons entry-buttons)
          (tv:prepare-sheet (tv:mouse-sheet)
            (when last-x (tv:%draw-line tv:mouse-x tv:mouse-y last-x last-y tv:alu-ior nil tv:mouse-sheet))
            (setq last-x tv:mouse-x
                  last-y tv:mouse-y)
            (push last-y list)
            (push last-x list))
        (return list last-x last-y)))))

; Given list of points (x0 y0 x1 y1 ...) return 4 values: min-x min-y max-x max-y
(defobfun point-list-range (list &aux (min-x most-positive-fixnum) (min-y most-positive-fixnum) (max-x 0) (max-y 0))
  (do ((rest list (cddr rest)))
      ((null rest)
       (values min-x min-y max-x max-y))
    (if (> (car rest) max-x)
        (setq max-x (car rest)))
    (if (> (cadr rest) max-y)
        (setq max-y (cadr rest)))
    (if (< (car rest) min-x)
        (setq min-x (car rest)))
    (if (< (cadr rest) min-y)
        (setq min-y (cadr rest)))))

(defobfun lasso-icon (&aux bitblt-width ike)
  (multiple-value-bind (list end-x end-y)
      (lasso-region)
    (multiple-value-bind (x0 y0 x1 y1)
        (point-list-range list)
      (setq width (- x1 x0 -1)
            height (- y1 y0 -1)
            bitblt-width (32ize width)
            image-array (make-array (list height bitblt-width) :element-type '(mod 2) )
            mask-array (make-array (list height bitblt-width) :element-type '(mod 2)))
      ;; Draw the lasso curve in the mask
      (do ((rest list (cddr rest)))
          ((null (cddr rest)))
        (sys:%draw-line (- (car rest) x0) (- (cadr rest) y0) (- (caddr rest) x0) (- (cadddr rest) y0) tv:alu-ior t mask-array))
      ;; Generate the mask
      (aset 0 mask-array (- end-y y0) (- end-x x0))
      (fill-region mask-array (- end-y y0) (- end-x x0))
      ;; Generate the image
      (send tv:mouse-sheet :bitblt-from-sheet tv:alu-seta width height x0 y0 image-array 0 0)
      (bitblt tv:alu-and width height mask-array 0 0 image-array 0 0)
      (setq ike (oneof dynamic-array-icon 'window tv:mouse-sheet 'x x0 'y y0 'width width 'height height
                       'image-array image-array 'mask-array mask-array))
      (tv:mouse-warp x0 y0)
      (ask ike (draw))
      (ask ike (drag)))))

; This is far too stack-hungry an algorithm
(defobfun fill-region (array sx sy)
  (when (zerop (aref array sx sy))
    (aset 1 array sx sy)
    (fill-region array (1+ sx) sy)
    (fill-region array (1- sx) sy)
    (fill-region array sx (1+ sy))
    (fill-region array sx (1- sy))
    ))


; Fill the raster line, and return the limits (+++ not yet working or used)
(defun fill-line (array sx sy)
  (values
    (do ((xx sx (1+ xx)))
        ((not (zerop (aref array xx sy))) xx)
      (aset 1 array xx sy))
    (do ((xx (1- sx) (1- xx)))
        ((not (zerop (aref array xx sy))) xx)
      (aset 1 array xx sy))))

; Given a bit array representing an image, return a mask for it.
; This works by scanning each line for the extreme points.  It won't do
; well on non-convex images.
(defun make-mask (image &aux mask min-x max-x)
  (setq mask (make-array (array-dimensions image) :type (array-type image) :initial-element 0))
  (dotimes (y (array-dimension image 1))
    (setq min-x
          (do ((xx 0 (1+ xx)))
              ((or (= xx (1- (array-dimension image 0)))
                   (not (zerop (aref image xx y)))) xx))
          max-x
          (do ((xx (1- (array-dimension image 0)) (1- xx)))
              ((or (minusp xx)
                   (not (zerop (aref image xx y)))) (1+ xx))))
    (do ((xx min-x (1+ xx)))
        (( xx max-x))
      (aset 1 mask xx y)))
  mask)

; Return an image array for a character
(defun character-array (font char
                         &aux fd cd image)
  (setq fd (fed:font-name-font-descriptor font)
        cd (aref fd char)
        image (make-array (list (array-dimension cd 0) (32ize (array-dimension cd 0))) :element-type '(mod 2)))
  (dotimes (x (array-dimension cd 0))
    (dotimes (y (array-dimension cd 1))
      (aset (aref cd x y) image x y)))
  (values-list (cons image (array-dimensions cd))))


;;; Ported icons

(defobclass ported-icon (icon)
  (ports-visible nil)
  (ports nil))                                         ;A list of ports

(defobfun (draw ported-icon) ()
  (shadowed-draw)
  (when ports-visible (draw-ports)))

(defobfun (draw-ports ported-icon) ()
  (dolist (port ports)
    (ask port (draw))))

; These two functions are what user programs should call.
(defobfun (hide-ports ported-icon) ()
  (when ports-visible
    (draw-ports)
    (setq ports-visible nil)))

(defobfun (expose-ports ported-icon) ()
  (unless ports-visible
    (draw-ports)
    (setq ports-visible t)))

; +++ This probably won't work.  Need before and after demons
(defobfun (move ported-icon) (nx ny)
  (when ports-visible (draw-ports))                    ;erase ports
  (dolist (p ports)
    (ask-funcall p 'move-relative (- nx x) (- ny y)))
  (shadowed-move nx ny)
  (when ports-visible (draw-ports)))

(defobfun (disappear ported-icon) ()
  (mapc-ask ports
    (disappear))
  (shadowed-disappear))

;;; Debugging tools
(defobfun obj-under-mouse (&optional class mumble)
  (ask (send (tv:window-under-mouse) :oblisp-window) (obj-under-mouse class mumble)))



#| Make this work somedau
;;; Mouse blinkers
(defobfun make-mouse-blinker (screen)
  (tv:make-blinker screen 'mouse-icon-blinker :visibility :on))

(tv:mouse-define-blinker-type :icon #'make-mouse-blinker)

(defflavor mouse-icon-blinker (ike) (tv:mouse-blinker-mixin tv:blinker)
  :settable-instance-variables)

(defmethod (mouse-icon-blinker :blink) ()
  (Ask-funcall ike  (if tv:phase 'erase 'draw)))

(defmethod (mouse-icon-blinker :size) ()
  (ask ike (values width height)))

|#

(defobfun (be-the-mouse-blinker dynamic-icon) ()
   (tv:mouse-set-blinker-definition :icon 8 8 :on :set-ike obj:*object))

;  (tv:mouse-set-blinker-definition :character 8 8 :on :set-character image-char font))

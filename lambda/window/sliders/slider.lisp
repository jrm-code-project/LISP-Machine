;;; -*- Mode:LISP; Package:OBIE; Readtable:CL; Base:10 -*-
#|
To do: (prioritized)
 ZMAIL
  profile window gets old-style bars
 slider height adjustment-some problems with big things (like a font in inspector)
 rationalize interface
 new last-minute features
  move line (works! now document it)
  current-item marker
 slider doesn't always update on buffer change
 adjusting box size
  don't draw slider if stuff fits in window?
  better handling of small files (see below)
 cvv windows (seem to work now?)
 a zwei buffer that ends with a Newline has bad behavior on scroll-to-end
 turds with outside sliders
 blinker for inside sliders
 zwei variable for left-hand sliders
 line-at-a-time scrolling in zmacs is slow
 ghost move box
 interconstraining multiple bars

Reshape troubles:
 specify-rect - should not let you go out-of-bounds, make better blinker
   should have a frame that is a superior to all text windows
 need a way to kill a pane
 Super-W windows have selection, label deficiencies
  seemingly fixed
 Burying leaves mouse turds, sometimes no selected window
 graphic improvements: thick border for selected window?
 mouse click behavior is confusing
  move-window shouldn't warp mouse to topleft--use bottomright
 C-X C-B loses in small window
 whole-screen panes should still have a label--they may be covered
 click-right on bottom arrow of nonselected pane can lose

Inspector
  reshaping works, but partially covered windows get totally deexposed
  Make a new pane command in menu
  Put in Anchor box, meaning leave this thing here

top line in fileBox size: requires a better theory.  Right now top of box is
position of file.  We need to handle cases such as small files that are scrolled
off the top better.
|#

#| Scroll bars

Parts
  frame
  top-arrow and bottom-arrow
  slider
    should change size to reflect visible portion of document (down to minimum)
  slider-region

Mouse actions
  arrow: L: scroll continuously, M: scroll one page, R: scroll all the way
  slider: any button: pick up slider and drag it
  slider-region: move slider to clicked-on point

Implementation:
  margin-regions in standard window system
  scroll bars and parts thereof are oblisp objects
|#

;+++ Temp: load new mouse font
(unless (and (tv:font-char-width-table fonts:mouse)
             (not (zerop (aref (tv:font-char-width-table fonts:mouse) 48))))
  (load "lad:mt;mouse"))


;;; Mouse regions
(defobclass region ()
  window
  xl yl xu yu)

(defobfun (box-region region) ()
  (send window :draw-lines tv:alu-ior
        xl yl xu yl xu yu xl yu xl yl))

; Shades inside the box
(defobfun (shade-region region) (shade &optional (alu tv:alu-seta))
  (send window :bitblt alu (- xu xl 1) (- yu yl 1) shade 0 0 (1+ xl) (1+ yl)))

(defobclass mouse-region (region))

(defobfun (point-in-region region) (x y)
  (and ( xl x xu)
       ( yl y yu)))

; x and y are outside coordinates
(defobfun (outside-point-in-region region) (x y)
  (and ( xl (- x (tv:sheet-inside-left window)) xu)
       ( yl (- y (tv:sheet-inside-top window)) yu)))

; Convert outside to our coordinates
(defobfun (inside-x region) (x)
  (- x (tv:sheet-inside-left window)))

(defobfun (inside-y region) (y)
  (- y (tv:sheet-inside-top window)))

; Region relative to outside border of window
(defobclass outside-region (region))

(defobfun (inside-x region) (x) x)

(defobfun (inside-y region) (y) y)

(defobfun (outside-point-in-region outside-region) (x y)
  (and ( xl x xu)
       ( yl y yu)))

(defmacro %draw-lines (alu sheet &rest coords)
  `(progn
     ,@(do ((rest coords (cddr rest))
            result)
           ((null (cddr rest)) (nreverse result))
         (push `(sys:%draw-line ,(first rest) ,(second rest) ,(third rest) ,(fourth rest) ,alu nil ,sheet)
               result))))

(defobfun (box-region outside-region) ()
  (tv:prepare-sheet (window)
    (%draw-lines tv:alu-ior window xl yl xu yl xu yu xl yu xl yl)))

(defobfun (shade-region outside-region) (shade &optional (alu tv:alu-seta))
  (tv:prepare-sheet (window)
    (bitblt alu (- xu xl 1) (- yu yl 1) shade 0 0 (send window :screen-array) (1+ xl) (1+ yl))))

(defobclass outside-mouse-region (outside-region mouse-region))

(defobfun (mouse-click region) (x y ignore)
  (outside-point-in-region x y))

(defobfun (mouse-moves mouse-region) (x y)
  (outside-point-in-region x y))



;;; Some size parameters
(defconst slider-width 17)
(defconst slider-arrow-height 17)
(defconst default-slider-box-height 24)

(defobclass basic-slider ()
  window                                               ; window it belongs to
  (x 0) (y 0)                                          ; position on window
  length                                               ; long dimension
  (width slider-width)                                 ; short dimension
  top-arrow                                            ; Subobjects of the slider
  bottom-arrow
  slider-region
  slider-box
  slider-region-length
  (slider-box-pos 0)                                   ;The position of slider-box from top in pixels
  (slider-box-height default-slider-box-height))

(defobclass basic-slider-arrow ()
  owning-slider
  direction
  activatedp)

(defobclass slider-arrow (basic-slider-arrow mouse-region))
(defobclass outside-slider-arrow (basic-slider-arrow outside-mouse-region))

tv:(defconst 0%-gray (make-gray 1 1 0))                ;>>>

;;; The magic numbers in the next two functions are dependent on the mouse font
(defobfun (draw-self slider-arrow) ()
  (shade-region tv:0%-gray)
  (send window :draw-char fonts:mouse
        (+ (if activatedp 8 48)
           (caseq direction
             (:up 0)
             (:right 1)
             (:down 2)
             (:left 3)))
        (+ xl (caseq direction (:right 1) (t 2)))
        (+ yl (caseq direction (:up 2) (t 1)))
        tv:alu-ior)
  (box-region))

(defobfun (draw-self outside-slider-arrow) ()
  (tv:prepare-sheet (window)
    (shade-region tv:0%-gray)
    (sys:%draw-char fonts:mouse
        (+ (if activatedp 8 48)
           (caseq direction
             (:up 0)
             (:right 1)
             (:down 2)
             (:left 3)))
        (+ xl (caseq direction (:right 1) (t 2)))
        (+ yl (caseq direction (:up 2) (t 1)))
        tv:alu-ior window)
      (box-region)))


(defobclass slider (basic-slider mouse-region))
(defobclass outside-slider (basic-slider outside-mouse-region))

;;; These two variables say what the right class to use for certain subobjects
(defclassvar (arrow-object-type slider) slider-arrow)
(defclassvar (mouse-region-object-type slider) mouse-region)
(defclassvar (arrow-object-type outside-slider) outside-slider-arrow)
(defclassvar (mouse-region-object-type outside-slider) outside-mouse-region)

(defobclass basic-vertical-slider (basic-slider))
(defobclass basic-horizontal-slider (basic-slider))

;;; These are the lowest-level instantiable classes.
(defobclass vertical-slider (basic-vertical-slider slider))
(defobclass horizontal-slider (basic-horizontal-slider slider))
(defobclass outside-vertical-slider (basic-vertical-slider outside-slider))
(defobclass outside-horizontal-slider (basic-horizontal-slider outside-slider))

(defobfun (exist basic-vertical-slider) (&rest args)
  (apply 'shadowed-exist args)
  (unless length
    (setq length (send window :height)))               ;+++
  (setq xl x
        yl y)
  (unless top-arrow
    (setq top-arrow
          (oneof arrow-object-type 'window window 'direction :up 'owning-slider (current-obj))))
  (unless bottom-arrow
    (setq bottom-arrow
          (oneof arrow-object-type 'window window 'direction :down 'owning-slider (current-obj))))
  (unless slider-region
    (setq slider-region
          (oneof mouse-region-object-type 'window window)))
  (unless slider-box
    (setq slider-box
          (oneof mouse-region-object-type 'window window)))
  (recompute-part-positions xl yl length width))



; Setup xl, yl, width, length, then call this to do the rest
(defobfun (recompute-part-positions basic-vertical-slider) (x y length width &aux slider-box-length)
  (setq slider-region-length (- length (* 2 slider-arrow-height)))
  (if (< slider-region-length 2)
      (error nil "Slider length of ~D is too small" length))
  (setq xu (+ xl width)
        yu (+ yl length))
  (ask top-arrow
    (setq xl x xu (+ x width) yl y yu (+ y slider-arrow-height)))
  (ask bottom-arrow
    (setq xl x xu (+ x width) yl (+ y length (- slider-arrow-height)) yu (+ y length)))
  (ask slider-region
    (setq xl x xu (+ x width) yl (+ y slider-arrow-height) yu (+ y length (- slider-arrow-height))))
  (setq slider-box-pos 0
        slider-box-height (min default-slider-box-height (floor slider-region-length 2)))
  (setq slider-box-length slider-box-height)           ;entirely for oblisp screw
  (ask slider-box
    (setq xl (1+ x)
          yl (+ y slider-arrow-height 1)
          xu (+ x width -1)
          yu (+ y slider-arrow-height slider-box-length))))

(defobfun (exist basic-horizontal-slider) (&rest args)
  (apply 'shadowed-exist args)
  (unless length
    (setq length (send window :width)))
  (setq xl x
        yl y)
  (unless top-arrow
    (setq top-arrow
          (oneof arrow-object-type 'window window 'direction :left 'owning-slider (current-obj))))
  (unless bottom-arrow
    (setq bottom-arrow
          (oneof arrow-object-type 'window window 'direction :right 'owning-slider (current-obj))))
  (unless slider-region
    (setq slider-region
          (oneof mouse-region-object-type 'window window)))
  (unless slider-box
    (setq slider-box
          (oneof mouse-region-object-type 'window window)))
  (recompute-part-positions xl yl length width))

(defobfun (change-of-window-size outside-horizontal-slider) ()
  (setq length (send window :width)
        xu (+ x length))
  (recompute-part-positions xl yl length width))

(defobfun (recompute-part-positions basic-horizontal-slider) (x y length width &aux slider-box-length)
  (setq slider-region-length (- length (* 2 slider-arrow-height)))
  (if (< slider-region-length 2)
      (error nil "Slider length of ~D is too small" length))
  (setq yu (+ yl width)
        xu (+ xl length))
  (ask top-arrow
    (setq xl x yl y xu (+ x slider-arrow-height) yu (+ y width)))
  (ask bottom-arrow
    (setq xl (+ x length (- slider-arrow-height)) yl y xu (+ x length) yu (+ y width)))
  (ask slider-region
    (setq xl (+ x slider-arrow-height) yl y xu (+ x length (- slider-arrow-height)) yu (+ y width)))
  (setq slider-box-pos 0
        slider-box-height (min default-slider-box-height (floor slider-region-length 2)))
  (setq slider-box-length slider-box-height)           ;entirely for oblisp screw  (ask slider-box
  (ask slider-box
    (setq xl (+ 1 x slider-arrow-height)
          yl (1+ y)
          xu (+ x slider-arrow-height slider-box-length)       ;see vert
          yu (+ y width -1))))


(defobfun (draw-self basic-slider) ()
  (box-region)
  (ask top-arrow (draw-self))
  (ask bottom-arrow (draw-self))
  (ask slider-region (shade-region tv:33%-gray))
  (ask slider-box (box-region) (shade-region tv:33%-gray tv:alu-setz)))

(defclassvar (orientation basic-vertical-slider) :vertical)
(defclassvar (orientation basic-horizontal-slider) :horizontal)

;;; This is the thing to call to move the slider-box.
;;; Note: most boundary errors are fixed now, but there are still problems with scrolling
;;; to the end of a very large things (like a font in the inspector).
(defobfun (move-box basic-slider) (new-pos &optional new-box-height)
  (unless new-box-height (setq new-box-height slider-box-height))
  (let* ((trimmed-new-pos (min (- slider-region-length 4) (max 0 new-pos)))    ;The 4 gives minimal slider a 1-pixel inside
         (trimmed-box-height (max 4 (min new-box-height (- slider-region-length 1 trimmed-new-pos))))
         (amount (- trimmed-new-pos slider-box-pos))
         (our-orientation orientation))
    (setq slider-box-height new-box-height)
    (set-box-size trimmed-box-height)
    (ask slider-box
      (slide amount our-orientation))
    (setq slider-box-pos trimmed-new-pos)
    (ask slider-region
      (shade-region tv:33%-gray))
    (ask slider-box
      (box-region)
      (shade-region tv:33%-gray tv:alu-setz))))

(defobfun (set-box-size basic-slider) (newsize)
  (ask slider-box (setq yu (+ yl newsize -1))))

(defobfun (slide region) (amount orientation)
  (caseq orientation
    (:horizontal (incf xl amount) (incf xu amount))
    (:vertical (incf yl amount) (incf yu amount))))

;;; Window system interface

(defobclass scroll-bar (basic-slider)
  position
  margin-region-descriptor)

(defobfun (exist scroll-bar) (&rest stuff)
  (apply 'shadowed-exist stuff)
;  (change-of-window-size)
  )

(defobfun (change-of-window-size scroll-bar) ()
  (setq xl (caseq position
            ((:left :top) 0)
            (:bottom 0)
            (:right (- (send window :width) slider-width 1)))
        yl (caseq position
            ((:left :top :right) 0)
            (:bottom (- (send window :height) slider-width 1)))
        length (caseq position
                 ((:left :right) (1- (send window :height)))
                 ((:top :bottom) (1- (send window :width)))))
  (recompute-part-positions xl yl length width))

;;; Compensate for window system braindamage - margin region descriptor dimensions are relative to either side depending
;;; on sign, if it's zero you can't tell which side is meant.
(defmacro v-margin-fixup (n)
  (once-only (n)
    `(cond ((zerop ,n) (ferror nil "Scroll bars can't be outermost thing in margin - put TV:BORDERS-MIXIN earlier in component list"))
           ((minusp ,n) (+ (send window :height) ,n))
           (t ,n))))

(defmacro h-margin-fixup (n)
  (once-only (n)
    `(cond ((zerop ,n) (ferror nil "Scroll bars can't be outermost thing in margin - put TV:BORDERS-MIXIN earlier in component list"))
           ((minusp ,n) (+ (send window :width) ,n))
           (t ,n))))

; Compute dimensions to overlap border by 1
(defobfun (change-of-window-size scroll-bar) ()
  (setq xl (+ (h-margin-fixup (tv:margin-region-left margin-region-descriptor))
              (caseq position (:left -2) (:right +1) (t -2)))
        yl (+ (v-margin-fixup (tv:margin-region-top margin-region-descriptor))
              (caseq position (:top -2) (:bottom +1) (t -2)))
        length (+ 3
                  (caseq orientation
                    (:vertical (- (v-margin-fixup (tv:margin-region-bottom margin-region-descriptor))
                                  (v-margin-fixup (tv:margin-region-top margin-region-descriptor))))
                    (:horizontal (- (h-margin-fixup (tv:margin-region-right margin-region-descriptor))
                                    (h-margin-fixup (tv:margin-region-left margin-region-descriptor)))))))
  (recompute-part-positions xl yl length width))


(defobclass vertical-scroll-bar (scroll-bar outside-vertical-slider))
(defobclass horizontal-scroll-bar (scroll-bar outside-horizontal-slider))

;;; Borders is here to make sure there is some margin space outside the scroll bar
(defflavor tv:new-scroll-bar-mixin (sliders) (tv:borders-mixin tv:margin-region-mixin)
  (:init-keywords :slider-edge                         ;an edge keyword, or list thereof
                  :reshape-box                         ;T to make give slider a reshape box
                  :scroll-bar)                         ;For compatability--ignored
  (:default-init-plist :slider-edge '(:left)))

(defmethod (tv:new-scroll-bar-mixin :after :init) (&rest plist &aux)
  (let ((slider-arg (cadr (member :slider-edge (car plist))))
        (reshape-arg (cadr (member :reshape-box (car plist)))))
    (obj::micro-load)                                  ;Safety feature - ensure objectlisp is working
    (setq sliders
          (loop for slider-pos in (setq slider-arg (if (listp slider-arg) slider-arg (list slider-arg)))
                collect
                (oneof (caseq slider-pos
                         ((:left :right) (if reshape-arg
                                             vertical-scroll-bar-with-reshape
                                           vertical-scroll-bar))
                         ((:top :bottom) horizontal-scroll-bar))        ;+++
                       'window self
                       'position slider-pos
                       'margin-region-descriptor (list #'new-scroll-bar-message-handler
                                                       slider-pos
                                                       slider-width
                                                       nil nil nil nil
                                                       nil)))) ;Pointer to slider (gets RPLACAd)
    (send self :set-region-list
          (append (mapcar #'fixup-margin-region-descriptor-kludge sliders)
                  tv:region-list))))

(defobfun fixup-margin-region-descriptor-kludge (sl)
  (ask sl
    (setf (nth 7 margin-region-descriptor) (current-obj))
    margin-region-descriptor))

(defmethod (tv:new-scroll-bar-mixin :after :change-of-size-or-margins) (&rest ignore)
  (change-size-kludge sliders tv:width tv:height))

(defobfun change-size-kludge (sliders w h)
  w h
  (mapc-ask sliders
    (change-of-window-size)))

;;; Scrolling protocol
(defmethod (tv:new-scroll-bar-mixin :after :new-scroll-position) (&optional ignore)
  (multiple-value-bind (top-line total-lines line-size display-lines)
      (funcall-self :scroll-position)
    (new-scroll-position-kludge sliders top-line total-lines line-size display-lines)))

(defobfun new-scroll-position-kludge (things top-line total-lines line-size display-lines)
  (mapc-ask things
    (new-scroll-position top-line total-lines line-size display-lines)))

; seems like line numbering is 1-based in scrolling protocol
(defobfun (new-scroll-position scroll-bar) (top-line total-lines line-size display-lines)
  line-size                            ;Stop compiler warnings
  (unless (zerop total-lines)
    (move-box (round (* (- slider-region-length 1) top-line) (1- total-lines))
              (and display-lines (round (* slider-region-length display-lines) total-lines)))))

(comment
    (when display-lines                                ;Some windows don't have this info
      (set-box-size (max (min (round (* slider-region-length display-lines) total-lines)
                              slider-region-length)
                         10))))


; Basic arrow handler, classes can redefine it or its subfunctions
(defobfun (mouse-on-arrow basic-slider) (char dir)
  (caseq char
    (#\mouse-m-1 (scroll-one-page dir))
    (#\mouse-r-1 (scroll-all-the-way dir))
    (t (scroll-one-line dir))))

(defobfun (scroll-one-line scroll-bar) (dir)
  (send window :scroll-to dir :relative))

(defobfun (scroll-one-page scroll-bar) (dir)
  (multiple-value-bind (ignore ignore ignore display-lines)
      (send window :scroll-position)
    (send window :scroll-to (* dir (- display-lines 2)) :relative)))   ;-2 for continuity

(defobfun (scroll-all-the-way scroll-bar) (dir)
  (send window :scroll-to
        (if (plusp dir)
            (multiple-value-bind (ignore total-lines) (send window :scroll-position)
              total-lines)
          0)
        :absolute))


(defselect new-scroll-bar-message-handler
  (:refresh (region)
    (draw-self (eighth region)))
  (:mouse-enters-region (region)
    (comment ;I flushed the double-headed arrow because it didn't have well-definied hot spot
    (let ((orientation (orientation-kludge (eighth region))))
     (tv:mouse-set-blinker-definition :character
                                      (if (eq orientation :vertical) 4 7)
                                      (if (eq orientation :vertical) 7 4)
                                      :on :set-character (if (eq orientation :vertical) #\ #\) fonts:mouse))))
  (:mouse-leaves-region (ignore)
    (comment (tv:mouse-standard-blinker)))
  (:mouse-moves (x y region)
;    (mouse-moves-kludge (eighth region) x y))
    (ignore))
  (:mouse-click (x y region char)
    (mouse-click-kludge (eighth region) x y char))
  (:who-line-documentation-string (region)
    (who-line-kludge (eighth region))))

(defobfun who-line-kludge (slider)
  (multiple-value-bind (mx my)
      (window-mouse-coords (ask slider window))
    (ask slider (who-line-documentation-string mx my))))

(defobfun (who-line-documentation-string scroll-bar) (mx my)
  (cond ((ask top-arrow (outside-point-in-region mx my))
        "Hold down button on arrow to scroll up: L: line at a time, M: page at a time, R: to beginning of buffer")
        ((ask bottom-arrow (outside-point-in-region mx my))
        "Hold down button on arrow to scroll down: L: line at a time, M: page at a time, R: to end of buffer")
        ((ask slider-box (outside-point-in-region mx my))
         "L: drag the box; M: move this line")
        ((and (there? 'reshape-region)
              (ask reshape-region (outside-point-in-region mx my)))
         "L: specify new corners; M: move window; R: bury window")
        (t                                             ;It's in grey area
         "L: move box here and drag; M: move this line")))

(defobfun mouse-moves-kludge (thing x y)
  (ask thing (mouse-moves x y)))

(defobfun mouse-click-kludge (thing x y char)
  (ask thing (mouse-click x y char)))

(defobfun orientation-kludge (thing)
  (ask thing orientation))

(defmacro trim (lower value upper)
  `(max ,lower (min ,upper ,value)))

(defun window-mouse-coords (window)
  (multiple-value-bind (xoff yoff)
      (tv:sheet-calculate-offsets window tv:mouse-sheet)
    (values (trim 0 (- tv:mouse-x xoff) (tv:sheet-width window))
            (trim 0 (- tv:mouse-y yoff) (tv:sheet-height window)))))

; Note: this version runs in the mouse process.  Perhaps there should be another form that
; runs in a user process
(defobfun (mouse-click basic-slider) (x y char)
  (and (outside-point-in-region x y)
       (or (ask top-arrow (mouse-click x y char))
           (ask bottom-arrow (mouse-click x y char))
           (caseq (coerce char 'character)
             (#\mouse-l-1 (track-mouse))
             (#\mouse-m-1 (drag-line))))))

(defobfun (track-mouse basic-slider) (&aux click-pos)
  (multiple-value-bind (click-x click-y)
      (window-mouse-coords window)
    (warp-box-if-necessary click-x click-y)
    (setq click-pos slider-box-pos)
    (do-forever
      (multiple-value-bind (mx my)
          (window-mouse-coords window)
        (mouse-moves mx my click-pos click-x click-y))
      (process-wait "Scrolling" #'(lambda (ox oy)
                                    (or ( ox tv:mouse-x)
                                        ( oy tv:mouse-y)
                                        (zerop (tv:mouse-buttons))))
                    tv:mouse-x tv:mouse-y)
      (if (zerop (tv:mouse-buttons))
          (return :slider-moved)))))

;+++ same for horizontal
;;; Idea for a slicker version:  when you click down the blinker becomes the line you clicked on, in inverse video
(defobfun (drag-line basic-vertical-slider) (&aux click-line)
  (multiple-value-bind (click-x click-y)
      (window-mouse-coords window)
    (setq click-line (truncate click-y (tv:sheet-line-height window)))
    (do-forever
      (multiple-value-bind (mx my)
          (window-mouse-coords window)
        (setq my (trim (tv:sheet-inside-top window) my (- (tv:sheet-inside-bottom window) 2)))
        (draw-drag-line-blinker mx my)
        (process-wait "Scrolling" #'(lambda (ox oy)
                                      (or ( ox tv:mouse-x)
                                          ( oy tv:mouse-y)
                                          (zerop (tv:mouse-buttons))))
                      tv:mouse-x tv:mouse-y)
        (draw-drag-line-blinker mx my)
        (when (zerop (tv:mouse-buttons))
          (send window :scroll-to (- click-line (truncate my (tv:sheet-line-height window))) :relative)       ;+++ wrong modularity
          (return :foo))))))


(defobfun (draw-drag-line-blinker basic-slider) (x y)
  (tv:prepare-sheet (window)
    (sys:%draw-rectangle (tv:sheet-inside-width window) 2 (tv:sheet-inside-left window) y tv:alu-xor window)))

(defobfun (mouse-click vertical-scroll-bar) (x y char)
  (if (eq (shadowed-mouse-click x y char) :slider-moved)
      (multiple-value-bind (ignore total-lines ignore display-lines)
          (send window :scroll-position)
        display-lines                                  ;+++ temp: stop warning
        (send window :scroll-to (round (* (1- total-lines) slider-box-pos) (- slider-region-length 1))
              :absolute))))

(defobfun (mouse-click basic-slider-arrow) (x y char)
  (and (outside-point-in-region x y)
       (unwind-protect
           (progn
             (setq activatedp t)
             (draw-self)
             (let ((dir (if (memq direction '(:left :up)) -1 1)))
               (do-forever
                 (ask owning-slider (mouse-on-arrow (coerce char 'character) dir))
                 (when (zerop (tv:mouse-buttons))
                   (return t)))))
         (setq activatedp nil)
         (draw-self))))

; These are only called from mouse-click method now
(defobfun (mouse-moves basic-vertical-slider) (ignore y &optional org-pos ignore org-y)
  (move-box (- (inside-y y) (inside-y org-y) (- org-pos))))

(defobfun (warp-box-if-necessary basic-vertical-slider) (click-x click-y)
  (unless (ask slider-box
            (outside-point-in-region click-x click-y))
    (move-box (- (inside-y click-y)
                 (ask slider-region yl)))))

(defobfun (mouse-moves basic-horizontal-slider) (x ignore &optional org-pos org-x ignore)
  (move-box (- (inside-x x) (inside-x org-x) (- org-pos))))

(defobfun (warp-box-if-necessary basic-horizontal-slider) (click-x click-y)
  (unless (ask slider-box
            (outside-point-in-region click-x click-y))
    (move-box (- (inside-x click-x)
                 (ask slider-region xl)))))

(defobfun draw-self (thing)
  (ask thing (draw-self)))

;;; Modifications to make existing flavors use new scroll bars
; loses (let ((fs:inhibit-fdefine-warnings t))                 ;used to be compiler-let, see if this better (+++)

tv:
(DEFFLAVOR SCROLL-WINDOW ()
  (BASIC-SCROLL-WINDOW BORDERS-MIXIN new-scroll-bar-mixin WINDOW)
  (:DOCUMENTATION :COMBINATION)
  (:default-init-plist :slider-edge '(:left))
  )


;;; This is to fix an obscure bug with typeout windows.
tv:
(defmethod (scroll-window-with-typeout-mixin :before :redefine-margins) (&rest ignore)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW (APPLY 'MAKE-WINDOW (CAR TYPEOUT-WINDOW)
                                                ':SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))

tv:
(defmethod (text-scroll-window-typeout-mixin :before :redefine-margins) (&rest ignore)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW (APPLY 'MAKE-WINDOW (CAR TYPEOUT-WINDOW)
                                                ':SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))


tv:
(defmethod (zwei:zmail-window :before :redefine-margins) (&rest ignore)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW (APPLY 'MAKE-WINDOW (CAR TYPEOUT-WINDOW)
                                                ':SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))

zwei:
(DEFFLAVOR ZMAIL-SUMMARY-SCROLL-WINDOW
        ((CURRENT-ZMAIL-BUFFER NIL)
         (CURRENT-MSG NIL)
         (MSGS-TO-BE-REDISPLAYED NIL)
         (RECENTER-P NIL)
         (LAST-DISPLAYED-TOP-ZMAIL-BUFFER NIL))
        (TV:BORDERS-MIXIN TV:TOP-BOX-LABEL-MIXIN
         ARROW-PRINTING-MIXIN TV:SCROLL-MOUSE-MIXIN TV:SCROLL-WINDOW-WITH-TYPEOUT-MIXIN
         TV:WINDOW-WITH-TYPEOUT-MIXIN TV:BASIC-SCROLL-WINDOW tv:new-scroll-bar-mixin
         ZMAIL-WHO-LINE-OVERRIDE-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :TRUNCATION T :SAVE-BITS :DELAYED :CR-NOT-NEWLINE-FLAG 1
                       :LABEL 'FONTS:CPTFONT
                       :slider-edge :left :DISPLAY-ITEM (SUMMARY-DISPLAY-ITEM)))

tv:
(DEFMETHOD (BASIC-SCROLL-WINDOW :SCROLL-TO) (TO &OPTIONAL (TYPE ':ABSOLUTE))
;  (IF (EQ CURRENT-PROCESS MOUSE-PROCESS)
;      (PROCESS-RUN-FUNCTION "Scroll" SELF :SCROLL-TO TO TYPE)
      (OR TOP-ITEM
          ;; Redisplay if a redisplay hasn't been done recently
          (SEND SELF :REDISPLAY))
      (SETQ TARGET-TOP-ITEM (CASE TYPE
                              (:ABSOLUTE TO)
                              (:RELATIVE (+ TO TOP-ITEM))
                              (OTHERWISE (FERROR NIL "~A is an unknown type of scrolling"
                                                 TYPE))))
      (AND (< TARGET-TOP-ITEM 0) (SETQ TARGET-TOP-ITEM 0))
      (SEND SELF :REDISPLAY))

(defvar zwei:*slider-position* :right)          ;Temporary until 110 zwei bug fixed

zwei:
(DEFFLAVOR ZWEI-WITHOUT-TYPEOUT ()
       (ZWEI TV:BORDERS-MIXIN tv:new-scroll-bar-mixin
        TV:DELAYED-REDISPLAY-LABEL-MIXIN TV:WINDOW)
  :ABSTRACT-FLAVOR
  (:default-init-plist :slider-edge (list *slider-position*)))

; Minibuffers have a (ridiculously small) slider, but no reshape box
zwei:
(DEFFLAVOR ZWEI-MINI-BUFFER
        ()
        (ZWEI-WITHOUT-TYPEOUT TV:DONT-SELECT-WITH-MOUSE-MIXIN)
  (:default-init-plist :reshape-box nil))

; flush flashy-scrolling
zwei:
(DEFFLAVOR ZWEI-WINDOW ()
       (ZWEI-TYPEOUT-MIXIN ZWEI-WITHOUT-TYPEOUT)
  :ABSTRACT-FLAVOR
  (:DOCUMENTATION :COMBINATION "A non-ZMACS editor window.
It is not instantiable, since it does not handle :MODE-LINE-WINDOW
\(for good reason, since at this level of generality it is not determined
where the mode line window would be found).")
  (:default-init-plist :reshape-box t))

;;; The window-sheet definitions
zwei:
(DEFFLAVOR ZWEI
           (;;T means this window wants to be exposed and selected, but is waiting until
            ;;typeahead has been absorbed before popping up.
            (DELAYED-SELECT-PENDING NIL)
            ;;T if should scroll to handle wraparound. Used by editor top level, etc.
            (GLITCH-AT-END-OF-PAGE NIL)
            (MOUSE-DOCUMENTATION-STRING (MAKE-STRING 100. ':FILL-POINTER 0)))
           (WINDOW)
  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW TV:ESSENTIAL-MOUSE
                     TV:STREAM-MIXIN TV:LABEL-MIXIN tv:new-scroll-bar-mixin)
  (:GETTABLE-INSTANCE-VARIABLES MOUSE-DOCUMENTATION-STRING)
  (:SETTABLE-INSTANCE-VARIABLES GLITCH-AT-END-OF-PAGE)
  (:DEFAULT-INIT-PLIST :SAVE-BITS ':DELAYED :RIGHT-MARGIN-CHARACTER-FLAG 1
                       :BACKSPACE-NOT-OVERPRINTING-FLAG 1
                       :MINIMUM-WIDTH 32. :MINIMUM-HEIGHT 32.)  ;for creation with mouse
  (:DOCUMENTATION :MIXIN "Editor windows"))

zwei:
(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
  (SETQ BUTTON (TV:MERGE-SHIFT-KEYS BUTTON))
  (COND ((or (not (< (tv:sheet-inside-left) x (tv:sheet-inside-right)))
             (not (< (tv:sheet-inside-top) y (tv:sheet-inside-bottom))))
         nil)
        ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
                    ':SELF-OR-SUBSTITUTE-SELECTED-P))
         ;; This frame or whatever is not selected.
         (TV:MOUSE-SELECT SELF)
         t)
        ((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
              (OR (= BUTTON #\MOUSE-1-1)
                  *MOUSE-CLICK-ALWAYS-SELECTS*))
         ;; Frame selected but this editor window is not.  Just switch to it.
         (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
         (IF *MOUSE-CLICK-ALWAYS-SELECTS*
             ;; And maybe also do the command for the mouse button.
             (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y)))
         (SETQ HANDLED-P T))
        (T
         (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y))
         t)))

;;; This causes incremental scrolling to update incrementally
zwei:
(defmethod (zwei :after :scroll-to) (&rest ignore)
  (process-wait "Scroll" #'(lambda (iob) (tv:io-buffer-empty-p iob)) tv:io-buffer))

; This makes zwei windows fulfill scrolling protocal (was missing fourth value)
;;; Returns 2 values: current line#, total #lines
zwei:
(DEFMETHOD (ZWEI :SCROLL-POSITION) ()
  (LET (INT BP)
    ;; INT and BP get the window's interval and start-bp,
    ;; but wait until we get two values that go together.
    (DO-FOREVER
      (SETQ INT INTERVAL BP START-BP)
      (WHEN (EQ (BP-TOP-LEVEL-NODE BP)
                (NODE-TOP-LEVEL-NODE INT))
        (RETURN))
      (PROCESS-WAIT "Synchronize"
                    #'(LAMBDA (WINDOW)
                        (EQ (BP-TOP-LEVEL-NODE (WINDOW-START-BP WINDOW))
                            (NODE-TOP-LEVEL-NODE (WINDOW-INTERVAL WINDOW))))
                    SELF))
    (LET ((TOP-LINE-NUMBER (1- (COUNT-LINES (INTERVAL-FIRST-BP INT) BP T))))
      (VALUES TOP-LINE-NUMBER
              (+ TOP-LINE-NUMBER (COUNT-LINES BP (INTERVAL-LAST-BP INT) T))
              (send self :LINE-HEIGHT)
              n-plines))))

; Make this taller
zwei:
(DEFFLAVOR TEMPORARY-MODE-LINE-WINDOW
        ((BACKGROUND-TYPEOUT-WINDOW NIL)
         BACKGROUND-TYPEOUT-STREAM)
        (MODE-LINE-SUPERIOR-MIXIN MODE-LINE-WINDOW-MIXIN
         TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:ANY-TYI-MIXIN
         TV:TEMPORARY-WINDOW-MIXIN TV:NO-SCREEN-MANAGING-MIXIN
         TV:STREAM-MIXIN TV:SELECT-MIXIN
         FIXED-HEIGHT-WINDOW-MIXIN TV:MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :NUMBER-OF-MINI-BUFFER-LINES 3
    :MINI-BUFFER-FLAVOR 'TEMPORARY-MODE-LINE-MINI-BUFFER
    :MINI-BUFFER-EDITOR-CLOSURE-VARIABLES MEDIUM-LEVEL-EDITOR-CLOSURE-VARIABLES)
  (:SETTABLE-INSTANCE-VARIABLES BACKGROUND-TYPEOUT-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES BACKGROUND-TYPEOUT-STREAM))

; From ZMAIL;FILTER - make mode line bigger
zwei:
(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :BEFORE :INIT) (IGNORE &AUX MODE-LINE-LINE-HEIGHT
                                                                  MODE-LINE-HEIGHT)
  (SETQ MODE-LINE-LINE-HEIGHT (+ 2 (MAX (FONT-CHAR-HEIGHT TV:(SCREEN-DEFAULT-FONT
                                                              (SHEET-GET-SCREEN SUPERIOR)))
                                       (FONT-CHAR-HEIGHT FONTS:SEARCH)))
        MODE-LINE-HEIGHT (+ 11 (* 3 MODE-LINE-LINE-HEIGHT)))
  (SETQ TV:PANES `((UNION-BUTTON TV:BUTTON-PANE :NAME "Union"
                                 :DOCUMENTATION "Set union of several universes.")
                   (INTERSECTION-BUTTON TV:BUTTON-PANE :NAME "Intersection"
                                                       :DOCUMENTATION
                                                    "Set intersection of several universes.")
                   (NOT-BUTTON TV:BUTTON-PANE :NAME "Not"
                                              :DOCUMENTATION
                                              "Set of all messages not in a universe.")
                   (CLOSE-BUTTON TV:BUTTON-PANE :NAME "Close"
                                                :DOCUMENTATION
                                                "Move to next higher Union or Intersection.")
                   (ZMAIL-BUFFER-MENU ZMAIL-COMMAND-MENU-PANE
                                      :ITEM-LIST NIL :LABEL "File buffers:")
                   (TEMP-ZMAIL-BUFFER-MENU ZMAIL-COMMAND-MENU-PANE
                                           :ITEM-LIST NIL :LABEL "")
                   (UNIVERSE-MENU ZMAIL-COMMAND-MENU-PANE
                                  :ITEM-LIST NIL :LABEL "Universes:")
                   (NAME-BUTTON TV:BIG-BUTTON-PANE :NAME "Name" :BORDERS 3
                                :DOCUMENTATION
 "Specify a new name for this universe.  Click right for a menu of existing filters to edit.")
                   (EDITOR-WINDOW ZMAIL-WINDOW :LABEL NIL :BORDERS (2 2 2 1) :SAVE-BITS NIL
                                  :CHARACTER-HEIGHT 10.)
                   (MODE-LINE-WINDOW ZMAIL-MOUSE-SENSITIVE-MODE-LINE-PANE
                                     :HEIGHT ,MODE-LINE-HEIGHT
                                     :MORE-P NIL :BORDERS (2 1 2 2)
                                     :BLINKER-DESELECTED-VISIBILITY :OFF)
                   (DONE-BUTTON TV:BUTTON-PANE :NAME "Done"
                                :DOCUMENTATION "Use this universe definition.")
                   (ABORT-BUTTON TV:BUTTON-PANE :NAME "Abort"
                                 :DOCUMENTATION "Abort this command."))
        TV:CONSTRAINTS
              `((ONLY . ( (WHOLE-THING)
                          ((WHOLE-THING :HORIZONTAL (:EVEN)
                            (WHOLE)
                            ((WHOLE TV:WHITE-INCLUDE-WHITESPACE ;Vert
                              (0.95) (:EVEN)
                              (OPERATIONS MENUS NAME EDITOR CONTROLS)
                              ((OPERATIONS TV:FLOATING-BUTTONS
                                (UNION-BUTTON INTERSECTION-BUTTON NOT-BUTTON CLOSE-BUTTON)))
                              ((NAME TV:SINGLE-PANE-IN-WHITESPACE NAME-BUTTON))
                              ((CONTROLS TV:FLOATING-BUTTONS
                                (DONE-BUTTON ABORT-BUTTON)))
                              ((EDITOR TV:WHITE-INCLUDE-WHITESPACE      ;Horiz
                                (:ASK-WINDOW SELF :EDITOR-SIZE) (:EVEN)
                                (EDITORS)
                                ((EDITORS :VERTICAL (0.8)
                                  (EDITOR-WINDOW MODE-LINE-WINDOW)
                                  ((MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                                  ((EDITOR-WINDOW :EVEN))))))
                              ;; This comes last since it can afford a scroll bar
                              ((MENUS TV:FLOATING-MENUS
                                (:ASK-WINDOW SELF :MENUS-SIZE)
                                (ZMAIL-BUFFER-MENU TEMP-ZMAIL-BUFFER-MENU UNIVERSE-MENU))))))))))))

; From ZMAIL;TOP - make mode line bigger
zwei:
(DEFMETHOD (ZMAIL-FRAME :BEFORE :INIT) (PLIST &AUX MODE-LINE-LINE-HEIGHT
                                                   MODE-LINE-HEIGHT EDITOR-MODE-LINE-HEIGHT)
  (PUTPROP PLIST T :SAVE-BITS)                  ;Things depend on working like this
  (SETQ TV:PROCESS '(ZMAIL-PROCESS-TOP-LEVEL :SPECIAL-PDL-SIZE 4000
                                             :REGULAR-PDL-SIZE 4000.))
  (SETQ MODE-LINE-LINE-HEIGHT (+ 2 (MAX (FONT-CHAR-HEIGHT TV:(SCREEN-DEFAULT-FONT
                                                              (SHEET-GET-SCREEN SUPERIOR)))
                                       (FONT-CHAR-HEIGHT FONTS:SEARCH)))
        MODE-LINE-HEIGHT (+ 10 (* 3 MODE-LINE-LINE-HEIGHT))
        EDITOR-MODE-LINE-HEIGHT (+ 10 (* 4 MODE-LINE-LINE-HEIGHT)))
  (SETQ TV:PANES `((MODE-LINE-WINDOW ZMAIL-MOUSE-SENSITIVE-MODE-LINE-PANE
                                     :HEIGHT ,MODE-LINE-HEIGHT
                                     :BLINKER-DESELECTED-VISIBILITY :OFF)
                   (MSG-WINDOW ZMAIL-WINDOW :LABEL "Message"
                                            :BLINKER-DESELECTED-VISIBILITY :OFF
                                            :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
                                              ,*EDIT-MSG-DOCUMENTATION*)
                   (DRAFT-HEADER-WINDOW ZMAIL-WINDOW :LABEL "Headers")
                   (DRAFT-TEXT-WINDOW ZMAIL-WINDOW :LABEL "Mail")
                   (SUMMARY-WINDOW ZMAIL-SUMMARY-SCROLL-WINDOW)
                   (FILTER-WINDOW ZMAIL-FILTER-FRAME)
                   (PROFILE-WINDOW ZMAIL-PROFILE-FRAME)
                   (PROFILE-EDITOR-WINDOW ZMAIL-WINDOW :LABEL "Profile"
                                          :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
                                            "L: edit profile buffer.")
                   (COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                 :COLUMNS 5 :ITEM-LIST ,*ZMAIL-COMMAND-ALIST*)
                   (NO-FILTER-COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                           :COLUMNS 6
                                           :ITEM-LIST ,*ZMAIL-NO-FILTER-COMMAND-ALIST*)
                   (FILTER-COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                        :COLUMNS 5
                                        :ITEM-LIST ,*ZMAIL-FILTER-COMMAND-ALIST*)
                   (BUTTONS-FRAME TV:BUTTONS-FRAME
                                  :PANES ((UNIVERSE-BUTTON TV:MEDIUM-BUTTON-PANE
                                           :NAME "Just current message"
                                           :DOCUMENTATION ,*UNIVERSE-BUTTON-DOCUMENTATION*)
                                          (FILTER-BUTTON TV:MEDIUM-BUTTON-PANE
                                           :NAME "All"
                                           :DOCUMENTATION ,*FILTER-BUTTON-DOCUMENTATION*))))
        TV:SUBSTITUTIONS `((STANDARD-MODE-LINE . (MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                           (STANDARD-EDITOR-MODE-LINE
                             . (MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                           (STANDARD-HEADER . (DRAFT-HEADER-WINDOW :LIMIT (3 8 :LINES)
                                                                   :ASK :HEADER-WINDOW-HEIGHT))
                           (STANDARD-COMMAND-MENU . (COMMAND-MENU :ASK :PANE-SIZE)))
        TV:CONSTRAINTS '((:MSG . ((MSG-WINDOW COMMAND-MENU MODE-LINE-WINDOW)
                                  (STANDARD-MODE-LINE
                                   STANDARD-COMMAND-MENU)
                                  ((MSG-WINDOW :EVEN))))
                         (:SUMMARY . ((SUMMARY-WINDOW MODE-LINE-WINDOW)
                                      (STANDARD-MODE-LINE)
                                      ((SUMMARY-WINDOW :EVEN))))
                         (:BOTH . ((SUMMARY-WINDOW COMMAND-MENU MSG-WINDOW MODE-LINE-WINDOW)
                                   (STANDARD-MODE-LINE
                                    STANDARD-COMMAND-MENU
                                    (SUMMARY-WINDOW :EVAL (TV:CONSTRAINT-ROUND
                                                            (* *SUMMARY-WINDOW-FRACTION*
                                                               TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                            '(NIL :LINES)
                                                            TV:**CONSTRAINT-NODE**)))
                                   ((MSG-WINDOW :EVEN))))
                         (:NEW . ((SUMMARY-WINDOW NO-FILTER-COMMAND-MENU BUTTONS-FRAME
                                   FILTER-COMMAND-MENU MSG-WINDOW MODE-LINE-WINDOW)
                                  (STANDARD-MODE-LINE
                                   (NO-FILTER-COMMAND-MENU :ASK :PANE-SIZE)
                                   (FILTER-COMMAND-MENU :ASK :PANE-SIZE)
                                   (SUMMARY-WINDOW :EVAL (TV:CONSTRAINT-ROUND
                                                           (* *SUMMARY-WINDOW-FRACTION*
                                                              TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                           '(NIL :LINES)
                                                           TV:**CONSTRAINT-NODE**)))
                                  ((BUTTONS-FRAME :ASK :PANE-SIZE))
                                  ((MSG-WINDOW :EVEN))))
                         ;; mail composing configurations
                         (:SEND . ((DRAFT-HEADER-WINDOW DRAFT-TEXT-WINDOW MODE-LINE-WINDOW)
                                   (STANDARD-EDITOR-MODE-LINE
                                    STANDARD-HEADER)
                                   ((DRAFT-TEXT-WINDOW :EVEN))))
                         (:REPLY . ((MSG-WINDOW DRAFT-HEADER-WINDOW DRAFT-TEXT-WINDOW
                                     MODE-LINE-WINDOW)
                                    (STANDARD-EDITOR-MODE-LINE
                                     (MSG-WINDOW 0.50s0 :LINES)
                                     STANDARD-HEADER)
                                    ((DRAFT-TEXT-WINDOW :EVEN))))
                         (:FILTER . ((SUMMARY-WINDOW FILTER-WINDOW)
                                     ((SUMMARY-WINDOW
                                        :EVAL (TV:CONSTRAINT-ROUND
                                                (* (OR *FILTER-SUMMARY-WINDOW-FRACTION*
                                                       *SUMMARY-WINDOW-FRACTION*)
                                                   TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                '(NIL :LINES)
                                                TV:**CONSTRAINT-NODE**)))
                                     ((FILTER-WINDOW :EVEN))))
                         (:PROFILE . ((PROFILE-WINDOW
                                       PROFILE-EDITOR-WINDOW MODE-LINE-WINDOW)
                                      (STANDARD-EDITOR-MODE-LINE
                                       (PROFILE-WINDOW 0.425s0))
                                      ((PROFILE-EDITOR-WINDOW :EVEN))))
                         )
        TV:CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*)
  (SETQ STANDARD-INPUT-FOR-PANES (MAKE-MACRO-STREAM SELF))
  (LET ((*STANDARD-INPUT* STANDARD-INPUT-FOR-PANES)
        (*ZMAIL-WINDOW* SELF)
        (*COMTAB* *STANDARD-COMTAB*))
    (SETQ EDITOR-CLOSURE (MAKE-EDITOR-CLOSURE ZMAIL-FRAME-EDITOR-CLOSURE-VARIABLES NIL))))

; From ZMAIL;WINDOW
zwei:
(undefmethod (zmail-window :mouse-click))

tv:
(DEFFLAVOR INSPECT-WINDOW ()
           (BASIC-INSPECT
            FUNCTION-TEXT-SCROLL-WINDOW MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
            BORDERS-MIXIN MARGIN-REGION-MIXIN TOP-LABEL-MIXIN tv:new-scroll-bar-mixin
            WINDOW)
  (:DEFAULT-INIT-PLIST :slider-edge '(:left)
                       :LABEL (LIST NIL NIL NIL NIL FONTS:HL12B "Empty"))
  (:DOCUMENTATION :COMBINATION "Scroll window for the inspector."))

tv:
(DEFMETHOD (INSPECT-FRAME :BEFORE :INIT) (PLIST &AUX IO-BUFFER)
  (LET ((NOI (OR (GET PLIST ':NUMBER-OF-INSPECTORS) 3))
        (NAMES NIL))
    (SETQ IO-BUFFER (MAKE-DEFAULT-IO-BUFFER))
    (SETQ PANES (LIST `(INTERACTOR INTERACTION-PANE :LABEL NIL
                                                    :IO-BUFFER ,IO-BUFFER
                                                    :MORE-P NIL)
                      `(HISTORY INSPECT-HISTORY-PANE-WITH-MARGIN-SCROLLING
                                :IO-BUFFER ,IO-BUFFER)
                      `(MENU COMMAND-MENU-PANE
                             :FONT-MAP ,(LIST FONTS:CPTFONT)
                             :ITEM-LIST ,INSPECT-FRAME-ITEM-LIST
                             :IO-BUFFER ,IO-BUFFER)))
    (DOTIMES (I NOI)
      (LET ((NAME1 (INTERN (FORMAT NIL "INSPECTOR-~D" I) "TV")))
        (PUSH `(,NAME1 ,(IF (= I (1- NOI))
                            'INSPECT-PANE-WITH-TYPEOUT
                          'INSPECT-PANE)
                :IO-BUFFER ,IO-BUFFER) PANES)
        (PUSH NAME1 NAMES)))
    (SETQ INSPECTORS NAMES)
    (SETQ CONSTRAINTS `((MAIN . ((INTERACTOR-AND-HISTORY MENU . ,(REVERSE NAMES))
                                 ((INTERACTOR-AND-HISTORY
                                    :HORIZONTAL (:LIMIT (3 NIL :LINES HISTORY)
                                                        0.20s0 :LINES HISTORY)
                                    (INTERACTOR HISTORY)
                                    ((HISTORY 0.55s0))
                                    ((INTERACTOR :EVEN))))
                                 ((MENU :ASK :PANE-SIZE))
                                 (,@(MAPCAR #'(LAMBDA (NAME1)
                                                `(,NAME1 :LIMIT (1 30. :LINES)
                                                  ,(/ 0.30s0 (1- NOI)) ;+++ watch for cl/zl syntax!
                                                  :LINES))
                                            (CDR NAMES)))
                                  ((,(CAR NAMES) :EVEN))))))))

tv:
(DEFFLAVOR TEXT-SCROLL-WINDOW
       ((ITEMS NIL)                             ;An array of all items
        (TOP-ITEM 0)                            ;The index of the topmost displayed item
        (ITEM-GENERATOR NIL)
        )
       (tv:new-scroll-bar-mixin)
;  (:REQUIRED-FLAVORS tv:new-scroll-bar-mixin)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL)
  (:DOCUMENTATION :MIXIN "Scrolling of lines all of one type"))

tv:
(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-POSITION) ()
  (VALUES TOP-ITEM (SEND SELF ':NUMBER-OF-ITEMS) LINE-HEIGHT (sheet-number-of-inside-lines)))

;;; This is to fix an obscure bug with typeout windows.
tv:
(defmethod (inspect-window-with-typeout :before :redefine-margins) (&rest ignore)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW (APPLY 'MAKE-WINDOW (CAR TYPEOUT-WINDOW)
                                                ':SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))

tv:
(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE)
  (when (variable-boundp displayed-items)
    (LET ((NLINES (SHEET-NUMBER-OF-INSIDE-LINES)))
      (AND (< (ARRAY-LENGTH DISPLAYED-ITEMS) NLINES)
           (ADJUST-ARRAY-SIZE DISPLAYED-ITEMS NLINES)))))

tv:
(DEFFLAVOR INSPECT-HISTORY-WINDOW ((CACHE NIL))
           (LINE-AREA-TEXT-SCROLL-MIXIN
            FUNCTION-TEXT-SCROLL-WINDOW
            tv:new-scroll-bar-mixin
            MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
            ANY-TYI-MIXIN WINDOW)
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :LABEL NIL
                       :slider-edge '(:left)
                       :LINE-AREA-WIDTH 24)
  (:DOCUMENTATION :COMBINATION
                  "History window for the inspector, but no margin scroll region"))

; name is now a lie
tv:
(DEFFLAVOR INSPECT-HISTORY-WINDOW-WITH-MARGIN-SCROLLING
        ()
        (INSPECT-HISTORY-WINDOW)
  (:DOCUMENTATION :COMBINATION "History window for the inspector."))

; ) for (let ((fs:fdefine...

;;; Test application of inside sliders.
(defflavor instrument-handling-mixin
           ((instruments nil)) ())

(defmethod (instrument-handling-mixin :after :refresh) (&rest ignore)
  (draw-things-kludge instruments))

(defmethod (instrument-handling-mixin :mouse-click) (char x y)
  (dolist (inst instruments)
    (if (mouse-click-kludge inst x y char)
        (return))))

(defmethod (instrument-handling-mixin :mouse-moves) (x y)
;  (dolist (inst instruments)
;    (if (mouse-moves-kludge inst x y)
;       (return))))
  )

(defobfun draw-things-kludge (things)
  (mapc-ask things (draw-self)))

(defmethod (instrument-handling-mixin :add-instrument) (instrument)
  (push instrument instruments))

(defobclass instrument-mixin () window)

(defobfun (exist instrument-mixin) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (send window :add-instrument (current-obj)))

(defobfun (move-box instrument-mixin) (pos &optional ignore)
  (shadowed-move-box pos)
  (position-has-changed))

(defobfun (scroll-one-line instrument-mixin) (dir)
  (move-box (+ slider-box-pos dir)))

(defobfun (scroll-one-page instrument-mixin) (dir)
  (move-box (+ slider-box-pos (* slider-box-height dir))))      ;sort of

(defobfun (scroll-all-the-way instrument-mixin) (dir)
  (move-box (if (minusp dir)
                0
              1000000)))                        ;move-box will trim

(defobfun (position-has-changed instrument-mixin) ()
  )

(defobclass vertical-slider-instrument (instrument-mixin vertical-slider))
(defobclass horizontal-slider-instrument (instrument-mixin horizontal-slider))

(defflavor frotz-window () (instrument-handling-mixin tv:window))

(defobfun frotz ()
  (let* ((window (tv:make-window 'frotz-window ':edges-from '(100 200 500 600) :expose-p t :border-margin-width 10))
         (xslider (oneof vertical-slider-instrument 'x 100 'y 100 'length 200 'window window))
         (yslider (oneof vertical-slider-instrument 'x 150 'y 100 'length 200 'window window))
         (zslider (oneof vertical-slider-instrument 'x 200 'y 100 'length 200 'window window)))
    (send window :refresh)))                           ;

;crap for tetra roll
(defmethod (frotz-window :slider-values) ()
  (values-list (read-kludge instruments)))

(defobfun read-kludge (instruments)
  (mapcar-ask instruments (get-pos)))

(defobfun (get-pos slider) ()
  (/  slider-box-pos length))

; Latest kludge: make a scroll bar with a reshape rectangle.

(defobclass reshape-box (mouse-region)
  owning-slider)

(defobclass outside-reshape-box (reshape-box outside-mouse-region))

(defobfun (draw-self reshape-box) ()
  (send window :draw-char fonts:mouse #\R xl yl tv:alu-ior)    ;temp
  (box-region))

(defobfun (draw-self outside-reshape-box) ()
  (tv:prepare-sheet (window)
    (sys:%draw-char fonts:mouse (char-code #\R) (+ 2 xl) (+ 2 yl) tv:alu-ior window)
    (box-region)))

(defobclass vertical-scroll-bar-with-reshape (vertical-scroll-bar)
  reshape-region)

(defobfun (exist vertical-scroll-bar-with-reshape) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (setq reshape-region (oneof outside-reshape-box 'window window 'owning-slider (current-obj)))
  (recompute-part-positions xl yl length width))       ;this is done twice.  too bad.

(defobfun (recompute-part-positions vertical-scroll-bar-with-reshape) (x y length width &aux slider-box-length)
  (setq slider-region-length (- length (* 3 slider-arrow-height)))
  (if (< slider-region-length 2)
      (error nil "Slider length of ~D is too small" length))
  (setq xu (+ xl width)
        yu (+ yl length))
  (ask top-arrow
    (setq xl x xu (+ x width) yl y yu (+ y slider-arrow-height)))
  (ask bottom-arrow
    (setq xl x xu (+ x width) yl (+ y length (- (* 2 slider-arrow-height))) yu (+ y length (- slider-arrow-height))))
  (ask reshape-region
    (setq xl x xu (+ x width) yl (+ y length (- slider-arrow-height)) yu (+ y length)))
  (ask slider-region
    (setq xl x xu (+ x width) yl (+ y slider-arrow-height) yu (+ y length (- (* 2 slider-arrow-height)))))
  (setq slider-box-pos 0
        slider-box-height (min default-slider-box-height (floor slider-region-length 2)))
  (setq slider-box-length slider-box-height)           ;entirely for oblisp screw
  (ask slider-box
    (setq xl (1+ x)
          yl (+ y slider-arrow-height 1)
          xu (+ x width -1)
          yu (+ y slider-arrow-height slider-box-length))))

(defobfun (draw-self vertical-scroll-bar-with-reshape) ()
  (shadowed-draw-self)
  (ask reshape-region (draw-self)))

; rather kludgey
(defobfun (mouse-click vertical-scroll-bar-with-reshape) (x y char)
  (or (and (outside-point-in-region x y)
           (ask reshape-region (mouse-click x y char)))
      (shadowed-mouse-click x y char)))

tv:
(DEFUN MOUSE-SPECIFY-RECTANGLE (&OPTIONAL LEFT TOP RIGHT BOTTOM (SHEET MOUSE-SHEET)
                                          (MINIMUM-WIDTH 40) (MINIMUM-HEIGHT 40) ABORTABLE
                                &AUX LEFT1 TOP1 right1 bottom1 WIDTH HEIGHT BUTTON ABORT)
"Ask user to specify a rectangle with the mouse, by clicking at two corners.
Returns four values, the left, top, right, and bottom of the rectangle,
all relative to SHEET.
The left button puts a corner down,
the right button puts it down at the nearest 'good' place,
the middle button aborts if that is possible.
Specifying a rectangle of zero or negative size instead gives the full screen.
Our arguments are where to start the corners out:
The upper left corner goes at LEFT and TOP, or where the mouse is if they are NIL;
the lower right corner goes near the other one by default, unless all
four args are present, in which case it starts off so as to make a rectangle
congruent to the one specified by the arguments.
If ABORTABLE is T, this can return NIL.
SHEET specifies the area within which we are allowed to act."
  (WHEN (EQ CURRENT-PROCESS MOUSE-PROCESS)
    (FERROR NIL "MOUSE-SPECIFY-RECTANGLE cannot be called in the mouse process"))
  (UNLESS (SHEET-ME-OR-MY-KID-P SHEET MOUSE-SHEET)
    (FERROR NIL
            "MOUSE-SPECIFY-RECTANGLE attempted on ~S which is not inferior of MOUSE-SHEET"
            SHEET))
  (WITH-MOUSE-GRABBED
    (DO-FOREVER
      ;; make mouse character an upper left corner
      (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON :SET-CHARACTER 17.)
      (MOUSE-WARP (OR LEFT MOUSE-X) (OR TOP MOUSE-Y))
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
            (IF ABORTABLE
                "Left: Select upper left corner of rectangle.  Middle aborts.  Right is smart."
                "Left: Select upper left corner of rectangle.  Right is smart."))
      ;; In case this was called in response to a mouse click, wait for
      ;; the buttons to be released.
      (PROCESS-WAIT "Release Button" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
      (PROCESS-WAIT "Button" #'(LAMBDA () (NOT (ZEROP MOUSE-LAST-BUTTONS))))
      (SETQ BUTTON MOUSE-LAST-BUTTONS)
      (PROCESS-WAIT "Release Button" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
      ;; The first click determines the upper left corner.
      (AND ABORTABLE (BIT-TEST 2 BUTTON) (RETURN (SETQ ABORT T)))
      (MULTIPLE-VALUE (LEFT1 TOP1)
        (MOUSE-SPECIFIED-POINT SHEET MOUSE-X MOUSE-Y (BIT-TEST 4 BUTTON) NIL))
      ;; Set up the mouse for finding the lower right corner.
      (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 12. 12. :ON :SET-CHARACTER 18.)
      (COND ((AND LEFT TOP RIGHT BOTTOM)
             (MOUSE-WARP (+ LEFT1 (- RIGHT LEFT)) (+ TOP1 (- BOTTOM TOP))))
            (T (MOUSE-WARP (+ MOUSE-X 20.) (+ MOUSE-Y 20.))))
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
            (IF ABORTABLE
                "Left: Select lower right corner of rectangle.  Middle aborts.  Right is smart."
                "Left: Select lower right corner of rectangle.  Right is smart."))
      ;; Leave the auxiliary blinker behind to continue to show the first corner.
      (LET ((MOUSE-RECTANGLE-BLINKER  (MOUSE-GET-BLINKER :RECTANGLE-CORNER-BLINKER)))
        (UNWIND-PROTECT
          (PROGN
;           (BLINKER-SET-CURSORPOS MOUSE-RECTANGLE-BLINKER LEFT1 TOP1)
;           (BLINKER-SET-VISIBILITY MOUSE-RECTANGLE-BLINKER T)
            ;; The next click fixes the lower right corner.
            ;; The frame-drawing is kind of icky--maybe we can use a blinker (or at least make faster).
            (do-forever
              (multiple-value (right1 bottom1)
                (tv:mouse-specified-point sheet mouse-x mouse-y nil t))
              (setq right1 (max right1 (+ left1 minimum-width))
                    bottom1 (max bottom1 (+ top1 minimum-height)))
              (tv:prepare-sheet (tv:mouse-sheet)
                (obie:%draw-lines tv:alu-xor tv:mouse-sheet left1 top1 right1 top1 right1 bottom1 left1 bottom1 left1 top1)
                (obie:%draw-lines tv:alu-xor tv:mouse-sheet (1+ left1) (1+ top1) (1- right1) (1+ top1) (1- right1) (1- bottom1) (1+ left1) (1- bottom1) (1+ left1) (1+ top1)))
              ;; mouse-x and -y can be messed up here for some reason
              (tv:mouse-wait)
;             (multiple-value-bind (xo yo) (tv:sheet-calculate-offsets sheet tv:mouse-sheet)
;               (tv:mouse-wait (+ xo right1) (+ yo bottom1)))
              (tv:prepare-sheet (tv:mouse-sheet)
                (obie:%draw-lines tv:alu-xor tv:mouse-sheet left1 top1 right1 top1 right1 bottom1 left1 bottom1 left1 top1)
                (obie:%draw-lines tv:alu-xor tv:mouse-sheet (1+ left1) (1+ top1) (1- right1) (1+ top1) (1- right1) (1- bottom1) (1+ left1) (1- bottom1) (1+ left1) (1+ top1)))
              (unless (zerop (mouse-buttons))
                (return)))
            (SETQ BUTTON MOUSE-LAST-BUTTONS))
          (BLINKER-SET-VISIBILITY MOUSE-RECTANGLE-BLINKER NIL)))
      (MOUSE-STANDARD-BLINKER)
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL)
      (AND ABORTABLE (BIT-TEST 2 BUTTON) (RETURN (SETQ ABORT T)))
      (MULTIPLE-VALUE-BIND (X Y)
          (MOUSE-SPECIFIED-POINT SHEET (1+ MOUSE-X) (1+ MOUSE-Y) (BIT-TEST 4 BUTTON) T)
        (SETQ WIDTH (- X LEFT1)
              HEIGHT (- Y TOP1)))
      (COND ((AND (PLUSP WIDTH) (PLUSP HEIGHT))
             (MULTIPLE-VALUE-BIND (XOFF YOFF)
                 (SHEET-CALCULATE-OFFSETS SHEET MOUSE-SHEET)
               (SETQ LEFT1 (- LEFT1 XOFF)
                     TOP1 (- TOP1 YOFF)))
             (IF (OR (< WIDTH MINIMUM-WIDTH) (< HEIGHT MINIMUM-HEIGHT)
                     (MINUSP LEFT1) (MINUSP TOP1)
                     (> (+ LEFT1 WIDTH) (SHEET-WIDTH SHEET))
                     (> (+ TOP1 HEIGHT) (SHEET-HEIGHT SHEET)))
         (BEEP)
                 (RETURN NIL)))
            (T (SETQ LEFT1 (SHEET-INSIDE-LEFT SHEET)
                     TOP1 (SHEET-INSIDE-TOP SHEET)
                     WIDTH (SHEET-INSIDE-WIDTH SHEET)
                     HEIGHT (SHEET-INSIDE-HEIGHT SHEET))
               (RETURN NIL)))))
  (IF ABORT NIL
    (VALUES LEFT1 TOP1 (+ LEFT1 WIDTH) (+ TOP1 HEIGHT))))


(defobfun (mouse-click reshape-box) (x y char)
  (and (outside-point-in-region x y)
       (process-run-function "Reshape" (caseq (make-char char)
                                         (#\mouse-1-1 #'tv:mouse-set-window-size)      ;Set corners
                                         (#\mouse-2-1 #'tv:mouse-set-window-position)  ;Move rigidly
                                         (#\mouse-3-1 #'(lambda (w) (send w :bury))))  ;Bury
                             window)))

;;; This stuff makes mousing on partially exposed ZWEI panes expose and select them.  Doesn't quite work yet.

zwei:
(DEFFLAVOR ZWEI-FRAME
        ((TV:IO-BUFFER NIL)
         EDITOR-CLOSURE
         (MODE-LINE-WINDOW 'MODE-LINE-WINDOW))
        (TV:INITIALLY-INVISIBLE-MIXIN TV:BORDERS-MIXIN tv:essential-mouse
         TV:ALIAS-FOR-INFERIORS-MIXIN TV:BASIC-FRAME)
  (:INITABLE-INSTANCE-VARIABLES TV:IO-BUFFER MODE-LINE-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES MODE-LINE-WINDOW EDITOR-CLOSURE)
; (:REQUIRED-METHODS :SWAP-IN-MODES :SWAP-OUT-MODES)
;    ;Normally these come from TOP-LEVEL-EDITOR; different though for a ZMACS-FRAME.
  (:INIT-KEYWORDS :NUMBER-OF-MINI-BUFFER-LINES
                  :COMTAB :MODE-LINE-LIST :EDITOR-CLOSURE-VARIABLES)
  (:DEFAULT-INIT-PLIST
    :NUMBER-OF-MINI-BUFFER-LINES 3
    :BORDER-MARGIN-WIDTH 0
    :MODE-LINE-LIST '("ZWEI " "(" *MODE-NAME-LIST*
                      ") " *MORE-ABOVE-BELOW*)))


; handle clicking on a partially exposed pane
zwei:
(defmethod (zwei-frame :mouse-click) (buttons x y &aux pane)
  (declare (ignore x y))
  (when (= buttons #\mouse-1-1)
    (setq pane (tv:window-under-mouse :mouse-select))
    (send pane :expose)
;   (command-buffer-push `(select-window ,pane))
    (send pane :select)))

zwei:
(defcom com-new-pane "Create a new ZMACS pane.
You choose the edges with the mouse" nil
  (let* ((frame (send *window* :superior))
         (newpane (send frame :create-window 'zmacs-window-pane)))
    (send newpane :set-interval *interval*)
    (multiple-value-bind (left top)
        (tv:mouse-specified-point frame tv:mouse-x tv:mouse-y nil nil)
      (send newpane :set-edges left top (+ left 100) (+ top 100)))      ;Start corners off in a reasonable way
    (if (tv:mouse-set-window-size newpane)
        (progn
          (send newpane :expose nil :clean)
          (send newpane :recompute-label)
          (send newpane :update-label)
          (send newpane :select)
          (make-window-current newpane)
          dis-text)
      (send newpane :kill))))

zwei:(set-comtab *standard-comtab* '(#\Super-W com-new-pane))

zwei:
(DEFMETHOD (ZMACS-FRAME :CHANGE-PANE-LABEL) (PANE NEW-LABEL)
  (SEND PANE ':DELAYED-SET-LABEL NEW-LABEL))

zwei:
(DEFMETHOD (ZMACS-FRAME :UPDATE-LABELS) ()
  (DOLIST (W TV:INFERIORS)
    (OR (EQ W MODE-LINE-WINDOW)
        (SEND W ':DELAYED-SET-LABEL (BUFFER-NAME (WINDOW-INTERVAL W)))))
  (DOLIST (W TV:INFERIORS)
    (OR (EQ W MODE-LINE-WINDOW)
        (SEND W ':SET-LABEL NIL))))

; To come:  Meta-. and edit obfun that make a new pane sized to the new function

(comment "This doesn't work, the Meta-. code is impossibly hairy"
; version that pops up new window
zwei:
(DEFUN EDIT-DEFINITION-1 (OBJECT &OPTIONAL (OBJECTS (LIST OBJECT)) STRING
                          DEFINITION-GENERIC-PATHNAME DEFINITION-TYPE
                          &AUX DEF newpane)
  "Visit the definition(s) of OBJECT, or OBJECTS.
STRING will be used eventually to look for look-alike objects,
so it should be a printed representation of OBJECT.
If OBJECTS is T, only OBJECT precisely is used, no matter how desperate the user gets.
Then STRING is not needed.
DEFINITION-GENERIC-PATHNAME restricts to definitions in that file,
and DEFINITION-TYPE restricts to that type of definition.
DEFINITION-TYPE should be something like DEFUN, DEFVAR, etc., or NIL."
  (SETQ OBJECT (FLUSH-INTERNAL-SPEC OBJECT))
  (IF (CONSP OBJECTS)
      (SETQ OBJECTS (MAPCAR 'FLUSH-INTERNAL-SPEC OBJECTS)))
  (AND (OR (EQ OBJECTS T)
           (AND (NULL (CDR OBJECTS))
                (EQUAL (CAR OBJECTS) OBJECT)))
       (SETQ DEF (ONLY-DEFINITION-OR-NIL OBJECT DEFINITION-TYPE)))
  (IF DEF
      ;; If there is only one definition of this object that could possibly be meant,
      ;; and it is in a buffer and still real, just go there.
      ;; Don't do any hacking with the possibilities buffer.
      ;; This is probably the most common case, so it should be fast.
      ;; If there is only one definition but it is in a file,
      ;; that is going to be slow enough anyway so no need to special case it.
      (PROGN (SOURCE-FILE-NAMES OBJECT T DEFINITION-TYPE)  ;Print names of any patch files.
             (POINT-PDL-PUSH (POINT) *WINDOW* T)
             (setq newpane (send (send *window* :superior) ':CREATE-WINDOW 'ZMACS-WINDOW-PANE))
             (send newpane :set-edges 100 100 500 500) ;+++
             (send newpane :set-interval (car def))
             (move-bp (window-point newpane) (cdr def) 0)
             (RECENTER-WINDOW newpane :START
                              (BACKWARD-OVER-COMMENT-LINES (OR (BACKWARD-OVER-PACKAGE-PREFIX (window-point newpane))
                                                               (window-point newpane))
                                                           nil))
             (send newpane :expose)
             (send newpane :select))
    (INSERT-EDIT-DEFINITION-POSSIBILITY OBJECT OBJECTS STRING
                                        DEFINITION-GENERIC-PATHNAME
                                        DEFINITION-TYPE))))


;;; Debugging

(defflavor test-window (pos) (tv:new-scroll-bar-mixin tv:window))

(defmethod (test-window :scroll-to) (n &optional (type :absolute))
  (if (eq type :absolute)
      (setq pos n)
    (incf pos n))
  (format self "~&~D" pos))

(defmethod (test-window :scroll-position) ()
  (values pos 100 20 20))

(defun slider-test (&optional (sides '(:right)))
  (tv:make-window 'test-window ':edges-from :mouse :expose-p t :slider-edge sides :reshape-box t))

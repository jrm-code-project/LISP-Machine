;;;-*- Mode:LISP; Package:WINDOW-MAKER; Fonts:(CPTFONT); Base:8; Readtable:ZL -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;
;;;
;;; Functions which help choose a slicing point.
;;;

(defun output-number-on-the-panel (number)
  (let ((string-to-output (format nil "~D" number))
        length-of-string)
    (if (< (setq length-of-string (string-length string-to-output)) 4)
        (setq string-to-output (string-append (make-string (- 4 length-of-string) ':initial-element #\space)
                                              string-to-output)))
    (funcall *instrument-pane* ':clear-screen)
    (funcall *instrument-pane* ':string-out-explicit string-to-output 20.
             (// (funcall *instrument-pane* ':height) 6)
             nil nil fonts:40vshd tv:alu-ior)))

(defun get-point-of-slicing (direction top left lower-limit upper-limit frame &optional (flag NIL)
                             &aux (slicing-point NIL))
  (funcall *instrument-pane* :set-label '(:string "Percent" :font fonts:cptfontb))
  (multiple-value-bind (x-off y-off)
      (tv:sheet-calculate-offsets *graphic-window-area* (funcall *graphic-window-area* :superior))
    ;; clear io buffer from possible mouse-clicks and mouse-moves
    (tv:io-buffer-clear (funcall *graphic-window-area* :io-buffer))
    (let* ((direction-flag (equal direction ':horizontal))
           (blinker (if direction-flag #/ #/))        ;first is the left arrow the second the up arrow
           (cursor-x-position (if direction-flag left (if flag upper-limit left)))
           (cursor-y-position (if direction-flag (if flag upper-limit top) top))
           (max-percent (funcall frame :compute-percent lower-limit upper-limit direction)))
      (funcall *graphic-window-area* :set-in-slicing-procedures
               "click left to take slicing point, middle or right to abort operation")
      (funcall *graphic-window-area* :draw-char fonts:mouse blinker
               (if direction-flag cursor-x-position (- cursor-x-position 8.))
               (if direction-flag (- cursor-y-position 8.) cursor-y-position)
               tv:alu-xor)
          ;; position the mouse
      (tv:mouse-warp (+ cursor-x-position  x-off) (+ cursor-y-position y-off))
      (output-number-on-the-panel (if flag (- max-percent
                                              (funcall frame :compute-percent lower-limit
                                                       (if direction-flag
                                                           cursor-y-position cursor-x-position)
                                                       direction))
                                    (funcall frame :compute-percent lower-limit
                                             (if direction-flag
                                                 cursor-y-position cursor-x-position)
                                             direction)))
      (unwind-protect
          (loop as blip = (funcall *graphic-window-area* :list-tyi)
                with mouse-x and mouse-y
                do
                (selectq (first blip)
                  (:my-mouse-move
                   (funcall *graphic-window-area* :draw-char fonts:mouse blinker
                            (if direction-flag cursor-x-position (- cursor-x-position 8.))
                            (if direction-flag (- cursor-y-position 8.) cursor-y-position)
                            tv:alu-xor)
                   (multiple-value (mouse-x mouse-y) (apply #'values (cdr blip)))
                   (if direction-flag
                       (setq cursor-y-position (if (and (>= mouse-y lower-limit)
                                                        (<= mouse-y upper-limit))
                                                   mouse-y
                                                 cursor-y-position))
                     (setq cursor-x-position (if (and (>= mouse-x lower-limit)
                                                      (<= mouse-x upper-limit))
                                                 mouse-x
                                               cursor-x-position)))
                   ; force the blinker mouse to occupy the same place as before in on of the coordinates.
                   (tv:mouse-warp (+ x-off cursor-x-position) (+ y-off cursor-y-position))
                   (funcall *graphic-window-area* :draw-char fonts:mouse blinker
                            (if direction-flag cursor-x-position (- cursor-x-position 8.))
                            (if direction-flag (- cursor-y-position 8.) cursor-y-position)
                            tv:alu-xor)
                   (output-number-on-the-panel (if flag (- max-percent
                                                           (funcall frame :compute-percent lower-limit
                                                                    (if direction-flag
                                                                        cursor-y-position cursor-x-position)
                                                                    direction))
                                                 (funcall frame :compute-percent lower-limit
                                                          (if direction-flag
                                                              cursor-y-position cursor-x-position)
                                                          direction))))
                  (:my-mouse-click
                   (setq slicing-point
                         (selectq (second blip)
                           (#/mouse-1-1 (if direction-flag
                                            (fourth blip) (third blip)))
                           (#/mouse-2-1 nil)))
                   (return nil))))
        (funcall *graphic-window-area* :draw-char fonts:mouse blinker
                            (if direction-flag cursor-x-position (- cursor-x-position 8.))
                            (if direction-flag (- cursor-y-position 8.) cursor-y-position)
                            tv:alu-xor)
        (funcall *graphic-window-area* :set-in-slicing-procedures NIL)
        (funcall *instrument-pane* :set-label nil)
        (funcall *instrument-pane* :clear-screen)
        )))
  slicing-point)


(defun get-number-of-characters (lower-limit upper-limit frame flag &optional (char-number nil))
  (let* ((line-height (funcall *Graphic-window-area* :line-height))
         (character-width (funcall *graphic-window-area* :char-width))
         (max-number-of-char (// (- upper-limit lower-limit) character-width))
         (io-buffer (funcall *graphic-window-area* :io-buffer))
         left top right bottom bottom-side cursor-x-position cursor-y-position x-off y-off)
    (if (not flag) (setq char-number max-number-of-char))
    (multiple-value (left top right bottom) (funcall frame :get-slots))
    (multiple-value (x-off y-off)
      (tv:sheet-calculate-offsets *graphic-window-area* (funcall *graphic-window-area* :superior)))
    (setq cursor-x-position (+ lower-limit (* (1- (if flag 1 max-number-of-char)) character-width))
          cursor-y-position (+ top 1)
          bottom-side (+ cursor-y-position line-height))
    (funcall *graphic-window-area* :set-in-slicing-procedures
             "click left to take slicing point, middle or right to abort operation")
    (funcall *instrument-pane* ':set-label '(:string "characters" :font fonts:cptfontb))
    (output-number-on-the-panel 1)
    (draw-box cursor-x-position cursor-y-position (+ cursor-x-position character-width) bottom-side)
    (tv:mouse-warp (+ cursor-x-position x-off) (+ cursor-y-position y-off))
    (tv:io-buffer-clear io-buffer)              ;clear io-buffer from accidentally generated blips
    (unwind-protect
        (loop as blip = (funcall *graphic-window-area* :list-tyi)
              with mouse-x
              with mouse-y
              with computed-char-number = (if flag 1 max-number-of-char)
              do
              (selectq (car blip)
                (:my-mouse-move
                 ;; erase current position of the character box.
                 (draw-box cursor-x-position cursor-y-position (+ cursor-x-position character-width) bottom-side)
                 (multiple-value (mouse-x mouse-y) (apply #'values (cdr blip)))
                 (if (and (>= mouse-x lower-limit)
                          (<= mouse-x upper-limit))
                     (setq computed-char-number (1+ (// (- mouse-x lower-limit) character-width))
                           cursor-x-position (+ lower-limit (* (1- computed-char-number) character-width))))
                 (output-number-on-the-panel (if flag computed-char-number (- max-number-of-char computed-char-number -1)))
                 (draw-box cursor-x-position cursor-y-position (+ cursor-x-position character-width) bottom-side)
                 (tv:mouse-warp (+ mouse-x x-off) (+ cursor-y-position y-off)))
                (:my-mouse-click
                 (setq char-number
                       (selectq (second blip)
                         (#/mouse-1-1 (if flag computed-char-number (- max-number-of-char computed-char-number -1)))
                         (#/mouse-2-1 nil)))
                 (return nil))))
      ;; now clean up after you.
      (funcall *graphic-window-area* :set-in-slicing-procedures NIL)
      (funcall *instrument-pane* :set-label nil)
      (funcall *instrument-pane* :clear-screen)
      (draw-box cursor-x-position cursor-y-position (+ cursor-x-position character-width) bottom-side))
    (values char-number (and char-number (+ cursor-x-position character-width)))))

(defun get-number-of-lines (lower-limit upper-limit frame flag &optional (line-number nil))
  "routine to do the graphic to the *window-maker* and get the number of lines"
  (let* ((line-height (funcall *graphic-window-area* ':line-height))
         (max-number-of-lines (// (- upper-limit lower-limit) line-height))
         right-side left top right bottom cursor-x-position cursor-y-position x-off y-off)
    (multiple-value (left top right bottom) (funcall frame :get-slots))
    (multiple-value (x-off y-off)
      (tv:sheet-calculate-offsets *graphic-window-area* (funcall *graphic-window-area* :superior)))
    (setq right-side (1- right))
    (funcall *instrument-pane* ':set-label '(:string "lines" :font fonts:cptfontb))
    (output-number-on-the-panel 1)
    (setq cursor-x-position (1+ left)
          cursor-y-position (+ lower-limit (* (1- (if flag 1 max-number-of-lines)) line-height)))
    (funcall *graphic-window-area* :set-in-slicing-procedures
             "click left to take slicing point, middle or right to abort operation")
    (draw-box cursor-x-position cursor-y-position right-side (+ cursor-y-position line-height))
    (tv:mouse-warp (+ cursor-x-position x-off) (+ cursor-y-position y-off))
    ;; now clear the io buffer from any pending blips or character input.
    (tv:io-buffer-clear (funcall *graphic-window-area* :io-buffer))
    (unwind-protect
        (loop as blip = (funcall *graphic-window-area* :list-tyi)
              with mouse-x and mouse-y
              with new-line-number = (if flag 1 max-number-of-lines)
              do
              (selectq (car blip)
                (:my-mouse-move
                 ;; erase current position of cursor.
                 (draw-box cursor-x-position cursor-y-position right-side (+ cursor-y-position line-height))
                 (multiple-value (mouse-x mouse-y) (apply #'values (cdr blip)))
                 (if (and (>= mouse-y lower-limit)
                          (<= mouse-y upper-limit))
                     (setq new-line-number (1+ (// (- mouse-y lower-limit) line-height))
                           cursor-y-position (+ lower-limit (* (1- new-line-number) line-height))))
                 (output-number-on-the-panel (if flag new-line-number (- max-number-of-lines new-line-number -1)))
                 (draw-box cursor-x-position cursor-y-position right-side (+ cursor-y-position line-height))
                 (tv:mouse-warp (+ cursor-x-position x-off) (+ mouse-y y-off)))
                (:my-mouse-click
                 (setq line-number
                       (selectq (second blip)
                         (#/mouse-1-1 (if flag new-line-number (- max-number-of-lines new-line-number -1)))
                         (#/mouse-2-1 nil)))
                 (return nil))))
      ;; clean after you.
      (funcall *instrument-pane* :set-label nil)
      (funcall *instrument-pane* :clear-screen)
      (draw-box cursor-x-position cursor-y-position right-side (+ cursor-y-position line-height))
      (funcall *graphic-window-area* :set-in-slicing-procedures NIL))
    (values line-number (and line-number (+ cursor-y-position line-height)))))


(defun get-number-of-pixels (direction left top lower-limit upper-limit flag
                             &optional (pixel-number 1)
                             &aux x-off y-off max-number-of-pixels slicing-point
                             cursor-x-position cursor-y-position)
  (funcall *instrument-pane* ':set-label '(:string "pixels" :font fonts:cptfontb))
  (output-number-on-the-panel 1)
  (setq cursor-x-position left cursor-y-position top
        slicing-point nil)
  (setq max-number-of-pixels (- upper-limit lower-limit))
  (multiple-value (x-off y-off) (tv:sheet-calculate-offsets *graphic-window-area* (funcall *graphic-window-area* :superior)))
  (funcall *graphic-window-area* :draw-char fonts:mouse #/ (- cursor-x-position 6) (- cursor-y-position 6) tv:alu-xor)
  (tv:mouse-warp (+ cursor-x-position x-off) (+ cursor-y-position y-off))
  (tv:io-buffer-clear (funcall *graphic-window-area* :io-buffer))
  (funcall *graphic-window-area* :set-in-slicing-procedures
           "click left to take slicing point, middle or right to abort operation")
  (unwind-protect
      (loop as blip = (funcall *graphic-window-area* :list-tyi)
            with mouse-x and mouse-y
            with computed-pixel-number = 1
            do
            (selectq (car blip)
              (:my-mouse-move
               (funcall *graphic-window-area* :draw-char fonts:mouse #/
                        (- cursor-x-position 6) (- cursor-y-position 6) tv:alu-xor)
               (multiple-value (mouse-x mouse-y) (apply #'values (cdr blip)))
               (if (equal direction ':horizontal)
                   (if (and (>= mouse-y lower-limit)
                            (<= mouse-y upper-limit))
                       (setq cursor-y-position mouse-y
                             computed-pixel-number (1+ (if flag (- mouse-y lower-limit) (- upper-limit mouse-y)))))
                 (if (and (>= mouse-x lower-limit)
                          (<= mouse-x upper-limit))
                     (setq cursor-x-position mouse-x
                           computed-pixel-number (1+ (if flag (- mouse-x lower-limit) (- upper-limit mouse-x))))))
               (output-number-on-the-panel computed-pixel-number)
               (funcall *graphic-window-area* :draw-char fonts:mouse #/
                        (- cursor-x-position 6) (- cursor-y-position 6) tv:alu-xor)
               (tv:mouse-warp (+ cursor-x-position x-off) (+ cursor-y-position y-off)))
              (:my-mouse-click
               (setq pixel-number
                     (selectq (second blip)
                       (#/mouse-1-1 computed-pixel-number)
                       (#/mouse-2-1 nil)))
               (return nil))))
    ;; now clean up after you.
    (funcall *graphic-window-area* :set-in-slicing-procedures nil)
    (funcall *instrument-pane* :set-label nil)
    (funcall *instrument-pane* :clear-screen)
    (funcall *graphic-window-area* :draw-char fonts:mouse #/ (- cursor-x-position 6) (- cursor-y-position 6) tv:alu-xor))
  (values pixel-number (if (equal direction ':horizontal) cursor-y-position cursor-x-position)))

(defun get-argument-for-other-keyword (key direction frame lower-limit upper-limit
                                       &optional (flag nil) &aux number-to-return slicing-point left top)
  "this function is to provide the user for communicating the absolute size either in
   :lines or :characters or :pixels"

  (selectq key
    (:lines (multiple-value (number-to-return slicing-point)
              (get-number-of-lines lower-limit upper-limit frame flag)))
    (:characters (multiple-value (number-to-return slicing-point)
                   (get-number-of-characters lower-limit upper-limit frame flag)))
    (:pixels
     (if (equal direction ':horizontal)
         (setq left (funcall frame ':left)
               top (if flag lower-limit upper-limit))
       (setq left (if flag lower-limit upper-limit)
             top (funcall frame ':top)))
     (multiple-value (number-to-return slicing-point)
       (get-number-of-pixels direction left top
                             lower-limit upper-limit flag))))
  (values number-to-return slicing-point))

(defun get-arguments-for-slicing-for-absolute-size (direction lower-limit upper-limit frame)
  (let (key1 key2 left1 left2 right1 right2 top1 top2 bottom1 bottom2 l t1 r b
        (pane nil)
        (slicing-point nil)
        (number1 nil)
        (number2 nil)
        (flag nil)
        slicing-point1 slicing-point2)
    (multiple-value (l t1 r b) (funcall frame ':get-slots))
    (setq key1 (tv:menu-choose (if (equal direction ':horizontal) *menu-2* *menu-3*)
                               '(:string "Specify size for first pane in:" :font fonts:metsi)))
    (or (equal key1 ':EVEN)
        (and (setq flag t) nil)
        (multiple-value (number1 slicing-point1)
          (get-argument-for-other-keyword key1 direction frame lower-limit upper-limit t)))
    (setq key2 (tv:menu-choose (if (equal direction ':horizontal) *menu-2* *menu-3*)
                               '(:string "Specify size for second pane in:" :font fonts:metsi)))
    (or (equal key2 ':EVEN)
        (multiple-value (number2 slicing-point2)
          (get-argument-for-other-keyword key2 direction frame
                                          (if flag slicing-point1 lower-limit)
                                          upper-limit flag)))
    (if (and key1 key2 (or (equal key1 ':EVEN) number1) (or (equal key2 ':EVEN) number2))
        (progn
          (if (and (equal key1 ':EVEN)
                   (equal key2 ':EVEN))         ; it is the case of slicing the window evenly
              (setq slicing-point (+ lower-limit (// (- upper-limit lower-limit) 2))
                    slicing-point1 slicing-point
                    slicing-point2 slicing-point)
            (if (equal key1 ':EVEN)
                (setq slicing-point slicing-point2
                      slicing-point1 slicing-point2)
              (if (equal key2 ':EVEN)
                  (setq slicing-point slicing-point1
                        slicing-point2 slicing-point1))))
          (if slicing-point
              ;;
              ;; Either window or both have keyword :EVEN
              ;;
              (if (equal direction ':horizontal)
                  (setq left1 l right1 r top1 lower-limit bottom1 (1- slicing-point)
                        left2 l right2 r top2 (1+ slicing-point) bottom2 upper-limit)
                (setq left1 lower-limit top1 t1 right1 (1- slicing-point) bottom1 b
                      left2 (1+ slicing-point) top2 t1 right2 upper-limit bottom2 b))
            ;;
            ;; this means that both windows have been specified with absolute measures.
            ;;
            (if (equal direction ':horizontal)
                (setq left1 l top1 lower-limit right1 r bottom1 (1- slicing-point1)
                      left2 l top2 (1+ slicing-point1) right2 r bottom2 (1- slicing-point2)
                      pane (make-instance 'pane :owner frame :left l :top (1+ slicing-point2) :right r :bottom upper-limit
                                          :keyword ':EVEN))
              (setq left1 lower-limit top1 t1 right1 (1- slicing-point1) bottom1 b
                    left2 (1+ slicing-point1) top2 t1 right2 (1- slicing-point2) bottom2 b
                    pane (make-instance 'pane :owner frame :left (1+ slicing-point2)
                                        :top t1 :right upper-limit :bottom b :keyword ':PERCENTWISE)))))
      ;; prevents it from breaking if somebody has clicked middle when selecting the slicing point.
      (setq key1 nil key2 nil))
    (values slicing-point1 number1 slicing-point2 number2
            key1 key2 left1 left2 right1 right2 bottom1 bottom2 top1 top2 pane)))


(defun sort-lines (list-of-lines)
  (let ((direction (equal (funcall (first list-of-lines) :direction) ':horizontal))
        (first-line (first list-of-lines))
        (second-line (second list-of-lines))
        x-pos-1 y-pos-1 x-pos-2 y-pos-2)
    (multiple-value (x-pos-1 y-pos-1 x-pos-2 y-pos-2)
      (apply #'values (list (funcall first-line :x-position)
                           (funcall first-line :y-position)
                           (funcall second-line :x-position)
                           (funcall second-line :y-position))))
    (if direction
        (if (> y-pos-1 y-pos-2) (list second-line first-line) list-of-lines)
      (if (> x-pos-1 x-pos-2) (list second-line first-line) list-of-lines))))

(defun update-owner-ship-of-lines-of-frame-to-change (pane line frame-to-change location)
  (let ((lines-border-frame-to-change (funcall frame-to-change :list-of-border-lines))
        line-to-share border-pane-or-frame)
    (if (= (length lines-border-frame-to-change) 1)
        ;; only one line. Check to see if line has to be shared or not.
        (if (member location '(:top :left))
            ;; pane inserted does not have to share the line of frame-to-change.
            ;; but it has to share its own.
            (progn
              (funcall line :set-owner2 frame-to-change)
              (funcall frame-to-change :update-list-of-border-lines line))
          (setq line-to-share (first lines-border-frame-to-change))
          (funcall line :set-owner2 frame-to-change)
          (funcall frame-to-change :delete-line-from-border-list line-to-share)
          (funcall frame-to-change :update-list-of-border-lines line)
          (funcall line-to-share :update-owner frame-to-change pane)
          (funcall pane :update-list-of-border-lines line-to-share))
      ;; otherwise sort them both
      (setq lines-border-frame-to-change (sort-lines lines-border-frame-to-change)
            line-to-share (if (member location '(:top :left)) (first lines-border-frame-to-change)
                            (second lines-border-frame-to-change))
            border-pane-or-frame (funcall line-to-share :get-other-window frame-to-change))
      (funcall frame-to-change :delete-line-from-border-list line-to-share)
      (funcall frame-to-change :update-list-of-border-lines line)
      (funcall line :set-owner2 frame-to-change)
      (funcall pane :update-list-of-border-lines line-to-share)
      (funcall line-to-share :replace-element frame-to-change pane))))

(defun sort-lines-by-position (list-of-lines)
  (selectq (length list-of-lines)
    ((0 1) list-of-lines)
    (otherwise
     (if (equal (funcall (first list-of-lines) :direction) ':horizontal)
         (if (> (funcall (first list-of-lines) :y-position) (funcall (second list-of-lines) :y-position))
             (list (second list-of-lines) (first list-of-lines)) list-of-lines)
       (if (> (funcall (first list-of-lines) :x-position) (funcall (second list-of-lines) :x-position))
             (list (second list-of-lines) (first list-of-lines)) list-of-lines)))))


;;
;;    This function make sure that all the graphic is done right. The reason is, there is a problem when
;; shrinking down the size of the panes. At some point of time the borders between two panes might overlap
;; This function makes sure that the borders never overlapp.
;;


(defun fix-all-new-coordinates (frame list-of-panes-or-frames)
  (let ((direction (equal (funcall frame :direction-of-slice) ':horizontal))
        (sorted-list-of-panes-or-frames
          (sort-panes-and-frames-in-frame list-of-panes-or-frames (funcall frame :direction-of-slice))))
    (loop for pane-or-frame in (butlast (cdr sorted-list-of-panes-or-frames))
          with set-operation = (if direction ':set-top ':set-left)
          with get-operation = (if direction ':bottom ':right)
          with current-slot = (funcall (first sorted-list-of-panes-or-frames) get-operation)
          with last-pane-or-frame = (nth (1- (length sorted-list-of-panes-or-frames)) sorted-list-of-panes-or-frames)
          when (typep pane-or-frame 'frame)
          DO
          (funcall pane-or-frame set-operation (+ current-slot 2))
          (setq current-slot (funcall pane-or-frame get-operation))
          ELSE
          DO
          (multiple-value-bind (x y z s) (funcall pane-or-frame :get-slots)
            (draw-box x y z s))
          (funcall pane-or-frame set-operation (+ current-slot 2))
          (setq current-slot (funcall pane-or-frame get-operation))
          (multiple-value-bind (x y z s) (funcall pane-or-frame :get-slots)
            (draw-box x y z s))
          finally
          (if (typep last-pane-or-frame 'frame)
              (funcall last-pane-or-frame set-operation (+ current-slot 2))
            (multiple-value-bind (x y z s) (funcall last-pane-or-frame :get-slots)
              (draw-box x y z s))
            (funcall last-pane-or-frame set-operation (+ current-slot 2))
            (multiple-value-bind (x y z s) (funcall last-pane-or-frame :get-slots)
              (draw-box x y z s))))))

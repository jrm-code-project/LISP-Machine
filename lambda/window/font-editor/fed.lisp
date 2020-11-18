;;;  -*- Mode:LISP; Package:FED; Base:8; Readtable:ZL -*-
;;;
;;; This is the [frame grabbing] Font Editor for the Lisp Machines.
;;;
;;; It is invoked with: (FED)
;;;
;;; It presents the user with a grid and a repetoire of commands
;;;   for manipulating an existing font or creating a new one.
;;;
;;; It [vanilla fed] was written by RMS, and is in the public domain.
;;;
;;;
;;; NEVER SAVE INSTANCES OF THIS FED IN A BAND!
;;; (It instantiates differently for color and no-color environments.)
;;;
;;; Interesting additions:  Latch Inverse Of First drawing mode,
;;;                         Re-constrain frame after possible font height change (to benefit Label Window),
;;;                         Display Font spacing fixed,
;;;                         Flush typeout on rubout, return, or blip (mouse-sensitive item selection),
;;;                         Alternate configuration and assorted hair for frame-grabbing, medium res clip copy.

;;; Some random constants

(DEFCONST MIN-BOX-SIZE 6)                               ; If box size is smaller than this,
                                                        ;  no grid is shown.
(DEFCONST DEFAULT-BOX-SIZE 14)                          ; How big to create box.
(DEFCONST GRID-POINT-SIZE 2)                            ; Size of a point on the grid.

;;; Redisplay degree definitions.

(DEFCONST REDISPLAY-NONE 0)                             ; No redisplay needed.
(DEFCONST REDISPLAY-ONE 1)                              ; Only one box wrong.
(DEFCONST REDISPLAY-SOME 2)                             ; A few boxes wrong.
(DEFCONST REDISPLAY-ALL 3)                              ; Everything you know is wrong.
(DEFCONST COMMAND-LIST
          '((#/ #/ #/ #/) COM-SHIFT-WINDOW
            (#/HAND-LEFT #/HAND-RIGHT #/HAND-DOWN #/HAND-UP) COM-SHIFT-CURSOR
            #/SP FALSE                                  ; Noop command.
            #/RUBOUT FALSE                      ;Noop - flush typeout.
            #/RETURN FALSE                      ;Noop - flush typeout.
            #/H COM-HOME
            #/@ COM-SCALE
            #/F COM-SELECT-FONT
            #/C COM-SPECIFY-CHARACTER
            #/M COM-MERGE-CHARACTER
            #/S COM-SAVE-CHARACTER
            #/E COM-ERASE-ALL
            #/X COM-SET-X
            #/Y COM-SET-Y
            #/D COM-DISPLAY-FONT
            #/V COM-SET-SAMPLE
            #/ COM-ROTATE-CHARACTER-RIGHT
            #/R COM-READ-FILE
            #/W COM-WRITE-FILE
            #/X COM-EXCHANGE-PLANES
            #/. COM-COMPLEMENT-SQUARE
            #/ COM-REFLECT-CHARACTER
            (#/? #/HELP) COM-HELP
            #/FORM TV:SCREEN-REDISPLAY
            ))

(DEFCONST FG-MENU-COMMAND-ALIST
          '(("Character"
             :BUTTONS
             ((NIL :VALUE COM-SPECIFY-CHARACTER)
              (NIL :VALUE COM-SPECIFY-CHARACTER-BY-NUMBER)
              (NIL :VALUE COM-READ-GRAY-CHARACTER))
             :DOCUMENTATION "L,M: Select character by (L) name or (M) code number.   R: Read a character into gray plane.")
            ("Font"
             :BUTTONS
             ((NIL :VALUE COM-LIST-FONTS)
              (NIL :VALUE COM-COPY-FONT)
              (NIL :VALUE COM-DISPLAY-FONT))
             :DOCUMENTATION "L: Select font by menu. M: Copy current font to a new font.  R: Display entire font.")
            ("Save Char"
             :BUTTONS
             ((NIL :VALUE COM-SAVE-CHARACTER)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-STORE-CHARACTER-EXPLICIT))
             :DOCUMENTATION "L: Store edits in selected character.   R: Store in specified font//char.")
            ("Home" :VALUE COM-HOME
             :DOCUMENTATION "Move drawing so char box is centered.")
            ("Erase Plane"
             :BUTTONS
             ((NIL :VALUE COM-ERASE-ALL)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-ERASE-GRAY))
             :DOCUMENTATION "L: Clear black plane.    R: Clear gray plane.")
            ("Move Plane"
             :BUTTONS
             ((NIL :VALUE COM-MOUSE-SHIFT-WINDOW)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MOUSE-SHIFT-GRAY))
             :DOCUMENTATION "L: Move both planes.   R: Move gray plane relative to black.")
            ("Exchange" :VALUE COM-EXCHANGE-PLANES
             :DOCUMENTATION "Exchange gray plane and foreground plane.")
            ("Merge"
             :BUTTONS
             ((NIL :VALUE COM-MERGE-GRAY)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MERGE-GRAY-MENU))
             :DOCUMENTATION "L: Merge gray plane into black.   R: Menu.")
            ("Rotate" :VALUE COM-ROTATE-CHARACTER-RIGHT
             :DOCUMENTATION "Rotate character right 90 degrees.")
            ("Reflect" :VALUE COM-REFLECT-CHARACTER
             :DOCUMENTATION "Reflect character.  Choose axis with menu.")
            ("Stretch Char" :VALUE COM-SCALE-CHARACTER
             :DOCUMENTATION "Scale the character in X, Y or both.")
            ("Rectangle"
             :BUTTONS
             ((NIL :VALUE COM-CLEAR-RECTANGLE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-OPERATE-ON-RECTANGLE))
             :DOCUMENTATION "L: Erase specified rectangle.   R: Menu of rectangle operations.")
            ("Draw (Sp)Line"
             :BUTTONS
             ((NIL :VALUE COM-MOUSE-DRAW-LINE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MOUSE-DRAW-SPLINE))
             :DOCUMENTATION "L: Click on two points; draws line.   R: Click on points; draws spline")
            ("Display Scale" :VALUE COM-SCALE
             :DOCUMENTATION "Set scale ofdisplay.")
            ("Files"
             :BUTTONS
             ((NIL :VALUE COM-READ-FILE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-WRITE-FILE))
             :DOCUMENTATION "L: Read selected font from file.  R: Write selected font into file.")
            ("Help" :VALUE COM-HELP
             :DOCUMENTATION "Get menu for help subjects.")
            ("Camera"
             :BUTTONS
             ((NIL :VALUE COM-FG-CAMERA)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-FG-CAMERA-TIL-SPACE))
             :DOCUMENTATION "Grab a new frame with the camera.  L: Momentary flow-through.  R: Flow-through until space.")
            ("Crop" :VALUE COM-FG-CROP
             :DOCUMENTATION "Crop a region on the medium res monitor.")
            ("Snap"
             :BUTTONS
             ((NIL :VALUE COM-FG-SNAP-TO-BLACK)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-FG-SNAP-TO-GRAY))
             :DOCUMENTATION "Monocontrast-copy cropped region to (L) Black Plane or (R) Gray Plane, threshold from slider.")
            ("Med. Res. File"
             :BUTTONS
             ((NIL :VALUE COM-LOAD-COLOR)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-SAVE-COLOR))
             :DOCUMENTATION "(L) Load medium res from file.  (R) Save medium res to file.")
))

(DEFCONST MENU-COMMAND-ALIST
          '(("Character"
             :BUTTONS
             ((NIL :VALUE COM-SPECIFY-CHARACTER)
              (NIL :VALUE COM-SPECIFY-CHARACTER-BY-NUMBER)
              (NIL :VALUE COM-READ-GRAY-CHARACTER))
             :DOCUMENTATION "L,M: Select character by (L) name or (M) code number.   R: Read a character into gray plane.")
            ("Font"
             :BUTTONS
             ((NIL :VALUE COM-LIST-FONTS)
              (NIL :VALUE COM-COPY-FONT)
              (NIL :VALUE COM-DISPLAY-FONT))
             :DOCUMENTATION "L: Select font by menu. M: Copy current font to a new font.  R: Display entire font.")
            ("Save Char"
             :BUTTONS
             ((NIL :VALUE COM-SAVE-CHARACTER)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-STORE-CHARACTER-EXPLICIT))
             :DOCUMENTATION "L: Store edits in selected character.   R: Store in specified font//char.")
            ("Home" :VALUE COM-HOME
             :DOCUMENTATION "Move drawing so char box is centered.")
            ("Erase Plane"
             :BUTTONS
             ((NIL :VALUE COM-ERASE-ALL)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-ERASE-GRAY))
             :DOCUMENTATION "L: Clear black plane.    R: Clear gray plane.")
            ("Move Plane"
             :BUTTONS
             ((NIL :VALUE COM-MOUSE-SHIFT-WINDOW)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MOUSE-SHIFT-GRAY))
             :DOCUMENTATION "L: Move both planes.   R: Move gray plane relative to black.")
            ("Exchange" :VALUE COM-EXCHANGE-PLANES
             :DOCUMENTATION "Exchange gray plane and foreground plane.")
            ("Merge"
             :BUTTONS
             ((NIL :VALUE COM-MERGE-GRAY)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MERGE-GRAY-MENU))
             :DOCUMENTATION "L: Merge gray plane into black.   R: Menu.")
            ("Rotate" :VALUE COM-ROTATE-CHARACTER-RIGHT
             :DOCUMENTATION "Rotate character right 90 degrees.")
            ("Reflect" :VALUE COM-REFLECT-CHARACTER
             :DOCUMENTATION "Reflect character.  Choose axis with menu.")
            ("Stretch Char" :VALUE COM-SCALE-CHARACTER
             :DOCUMENTATION "Scale the character in X, Y or both.")
            ("Rectangle"
             :BUTTONS
             ((NIL :VALUE COM-CLEAR-RECTANGLE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-OPERATE-ON-RECTANGLE))
             :DOCUMENTATION "L: Erase specified rectangle.   R: Menu of rectangle operations.")
            ("Draw (Sp)Line"
             :BUTTONS
             ((NIL :VALUE COM-MOUSE-DRAW-LINE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-MOUSE-DRAW-SPLINE))
             :DOCUMENTATION "L: Click on two points; draws line.   R: Click on points; draws spline")
            ("Display Scale" :VALUE COM-SCALE
             :DOCUMENTATION "Set scale ofdisplay.")
            ("Files"
             :BUTTONS
             ((NIL :VALUE COM-READ-FILE)
              (NIL :VALUE BARF)
              (NIL :VALUE COM-WRITE-FILE))
             :DOCUMENTATION "L: Read selected font from file.  R: Write selected font into file.")
            ("Help" :VALUE COM-HELP
             :DOCUMENTATION "Get menu for help subjects.")


))


;;; Windows that display a bunch of points inside a grid.

(DEFFLAVOR GRID-MIXIN
       (WINDOW-ARRAY                                    ; This represents the displayed image.
        WINDOW-X-SIZE                                   ; Size in pixels in the X direction.
        WINDOW-Y-SIZE                                   ; Size in pixels in the Y direction.
        (BOX-X-SIZE DEFAULT-BOX-SIZE)                   ; The size of an element of the grid.
        (BOX-Y-SIZE DEFAULT-BOX-SIZE)
        (WINDOW-X-POS 0)                                ; The offset position of our array.
        (WINDOW-Y-POS 0)
        (REDISPLAY-DEGREE REDISPLAY-NONE)               ; A number, REDISPLAY-<n>. (see above).
        (MIN-CHANGED-X 0)                               ; Range of area to bother checking.
        (MIN-CHANGED-Y 0)
        (MAX-CHANGED-X 0)
        (MAX-CHANGED-Y 0)
        ;; PREVIOUS-EDGES describes an area of boxes which must be
        ;; redisplayed independent of MAX/MIN-CHANGED-X/Y and the current :PLANE-EDGES
        ;; because the planes occupied those areas prior to erasing or moving them.
        ;; The elements are relative to the window, not the planes.
        (PREVIOUS-EDGES '(0 0 0 0))
        REDISPLAY-SUPPRESSED                            ; The last redisplay did not complete.
        )
       ()

  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW)
  (:INIT-KEYWORDS :WINDOW-ARRAY-TYPE)
  (:INITABLE-INSTANCE-VARIABLES)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL)
  (:REQUIRED-METHODS :AREF :ASET :PLANE-EDGES :LISTEN)

  (:DOCUMENTATION :MIXIN "Displays a set of points within a grid
    and allows for incremental redisplay of points and updating
    the data structure for changes in the display."))

(DEFMETHOD (GRID-MIXIN :AFTER :INIT) (INIT-PLIST)
  (DEDUCE-WINDOW-ARRAY-SIZE (GET INIT-PLIST ':WINDOW-ARRAY-TYPE 'ART-1B)))

(DEFMETHOD (GRID-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (LET ((OLD-X-SIZE WINDOW-X-SIZE)
        (OLD-Y-SIZE WINDOW-Y-SIZE))
    (DEDUCE-WINDOW-ARRAY-SIZE)
    ;; Contrive to preserve which font pixel is in the center of the window.
    (DECF WINDOW-X-POS (TRUNCATE (- WINDOW-X-SIZE OLD-X-SIZE) 2))
    (DECF WINDOW-Y-POS (TRUNCATE (- WINDOW-Y-SIZE OLD-Y-SIZE) 2))))

(DEFUN DEDUCE-WINDOW-ARRAY-SIZE (&OPTIONAL ARRAY-TYPE)
  "Set WINDOW-ARRAY to an array of type ARRAY-TYPE, with the correct size.
The correct size is enough to record enough boxes of size BOX-X-SIZE
by BOX-Y-SIZE to fill up the window.
Also sets WINDOW-X-SIZE and WINDOW-Y-SIZE to the size values."
  (DECLARE (:SELF-FLAVOR GRID-MIXIN))
  (OR ARRAY-TYPE (SETQ ARRAY-TYPE (ARRAY-TYPE WINDOW-ARRAY)))
  (LET ((LAST-ROW-OF-DOTS (IF (AND (> BOX-X-SIZE MIN-BOX-SIZE)
                                   (> BOX-Y-SIZE MIN-BOX-SIZE))
                              2
                              0)))
    (SETQ WINDOW-X-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-WIDTH) LAST-ROW-OF-DOTS) BOX-X-SIZE)
          WINDOW-Y-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-HEIGHT) LAST-ROW-OF-DOTS) BOX-Y-SIZE))
    (OR (AND (VARIABLE-BOUNDP WINDOW-ARRAY)
             ( WINDOW-X-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 1))
             ( WINDOW-Y-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 0)))
        (SETQ WINDOW-ARRAY (MAKE-ARRAY (LIST WINDOW-X-SIZE WINDOW-Y-SIZE)
                                       :TYPE ARRAY-TYPE)))))

;;; If we didn't come back, remember that the screen is clobbered.

(DEFMETHOD (GRID-MIXIN :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P
      (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))

;;; Note that something has changed for the redisplay loop.

(DEFMETHOD (GRID-MIXIN :MUST-REDISPLAY) (DEGREE &OPTIONAL MIN-X MIN-Y MAX-X MAX-Y)
  (IF (= DEGREE REDISPLAY-ONE)          ;Just one box to hack
      (SETQ MAX-X MIN-X MAX-Y MIN-Y))
  (COND (( REDISPLAY-DEGREE REDISPLAY-ONE)
         ;; If some redisplay already requested, merge in new request.
         (AND MIN-X (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X MIN-X)))
         (AND MIN-Y (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y MIN-Y)))
         (AND MAX-X (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X MAX-X)))
         (AND MAX-Y (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y MAX-Y))))
        ;; This is the first redisplay requested => take just what is requested now.
        (T (AND MIN-X (SETQ MIN-CHANGED-X MIN-X))
           (AND MAX-X (SETQ MAX-CHANGED-X MAX-X))
           (AND MIN-Y (SETQ MIN-CHANGED-Y MIN-Y))
           (AND MAX-Y (SETQ MAX-CHANGED-Y MAX-Y))))
  (SETQ REDISPLAY-DEGREE (MAX REDISPLAY-DEGREE DEGREE)))

;;; Function is an argument of the two grid points which returns the correct array value
;;;  from the other data structure

(DEFMETHOD (GRID-MIXIN :REDISPLAY) (&OPTIONAL (FORCE-TO-COMPLETION) &AUX PLANE-EDGES)
  (SETQ REDISPLAY-SUPPRESSED NIL)
  (COND ((= REDISPLAY-DEGREE REDISPLAY-NONE))   ;No redisplay needed
        ((AND (NOT FORCE-TO-COMPLETION) (SEND SELF :LISTEN))
         (SETQ REDISPLAY-SUPPRESSED T))
        (T
         (COND ((= REDISPLAY-DEGREE REDISPLAY-ALL)
                (SEND SELF :CLEAR-WINDOW)

                ;; Every box is now clear on the screen.
                (ARRAY-INITIALIZE WINDOW-ARRAY 0)

                ;; but every box must be checked for redisplay.

                (SETQ MIN-CHANGED-X 0 MIN-CHANGED-Y 0
                      MAX-CHANGED-X (1- WINDOW-X-SIZE)
                      MAX-CHANGED-Y (1- WINDOW-Y-SIZE))
                (SETQ REDISPLAY-DEGREE REDISPLAY-SOME)))

         ;; Since the commands don't seem to clip the change boundaries, do so here
         ;;  in case the font is too big to fit in the window.

         (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X 0)
               MIN-CHANGED-Y (MAX MIN-CHANGED-Y 0)
               MAX-CHANGED-X (MIN MAX-CHANGED-X (1- WINDOW-X-SIZE))
               MAX-CHANGED-Y (MIN MAX-CHANGED-Y (1- WINDOW-Y-SIZE)))

         ;; Realize that nothing outside the stored area of the plane
         ;; can possibly have changed.
         ;; Also merge in the PREVIOUS-EDGES, which is an additional
         ;; area of screen that may have changed and is not limited
         ;; to the currently existing plane areas.
         (SETQ PLANE-EDGES (SEND SELF :PLANE-EDGES))

         (SETQ MIN-CHANGED-X (MIN (MAX 0 (CAR PREVIOUS-EDGES))
                                  (MAX MIN-CHANGED-X
                                       (- (CAR PLANE-EDGES) WINDOW-X-POS))))
         (SETQ MIN-CHANGED-Y (MIN (MAX 0 (CADR PREVIOUS-EDGES))
                                  (MAX MIN-CHANGED-Y
                                       (- (CADR PLANE-EDGES) WINDOW-Y-POS))))
         (SETQ MAX-CHANGED-X (MAX (1- (MIN WINDOW-X-SIZE (CADDR PREVIOUS-EDGES)))
                                  (MIN MAX-CHANGED-X
                                       (- (CADDR PLANE-EDGES) WINDOW-X-POS))))
         (SETQ MAX-CHANGED-Y (MAX (1- (MIN WINDOW-Y-SIZE (CADDDR PREVIOUS-EDGES)))
                                  (MIN MAX-CHANGED-Y
                                       (- (CADDDR PLANE-EDGES) WINDOW-Y-POS))))

         ;; Now, for each box which isn't already displayed in the right state,
         ;;  update it.

         (TV:PREPARE-SHEET (SELF)
           (BLOCK ABORT-REDISPLAY
             (DO ((J MIN-CHANGED-Y (1+ J)))
                 ((> J MAX-CHANGED-Y)
                  (SETQ PREVIOUS-EDGES '(0 0 0 0))
                  (SETQ MAX-CHANGED-X MIN-CHANGED-X
                        MAX-CHANGED-Y MIN-CHANGED-Y)
                  (SETQ REDISPLAY-DEGREE REDISPLAY-NONE))
               (DO ((I MIN-CHANGED-X (1+ I))
                    (OLD-VALUE)
                    (NEW-VALUE))
                   ((> I MAX-CHANGED-X))
                 (OR (= (SETQ NEW-VALUE (SEND SELF :AREF (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS)))
                        (SETQ OLD-VALUE (AREF WINDOW-ARRAY I J)))
                     (SEND SELF :REDISPLAY-POINT I J NEW-VALUE OLD-VALUE)))
               (WHEN (AND (NOT FORCE-TO-COMPLETION) (SEND SELF :LISTEN))
                 (SETQ MIN-CHANGED-Y (1+ J))
                 (SETQ REDISPLAY-SUPPRESSED T)
                 (RETURN-FROM ABORT-REDISPLAY NIL))))))))

(DEFMETHOD (GRID-MIXIN :AFTER :CLEAR-WINDOW) ()
  (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE)
  (SEND SELF :DRAW-GRID))

;;; This is a message so you can put some daemons on it to draw other things (like the
;;;   character box).

(DEFVAR GRID-BITBLT-KLUDGE (MAKE-ARRAY '(64. 64.) :ELEMENT-TYPE 'BIT))
(DEFVAR GRID-BITBLT-ONES (MAKE-ARRAY '(32. 32.) :ELEMENT-TYPE 'BIT))

(DEFMETHOD (GRID-MIXIN :DRAW-GRID) ()
  ;; Now add in the grid points, unless the grid is too small.
  (COND ((NOT (OR (< BOX-X-SIZE MIN-BOX-SIZE) (< BOX-Y-SIZE MIN-BOX-SIZE)))

         ;; Make an array containing the necessary dots.

         (BITBLT 0 64. 64. GRID-BITBLT-KLUDGE 0 0 GRID-BITBLT-KLUDGE 0 0)
         (BITBLT 17 32. 32. GRID-BITBLT-ONES 0 0 GRID-BITBLT-ONES 0 0)
         (DO ((I 0 (+ I BOX-X-SIZE)))
             ((> (+ I GRID-POINT-SIZE) 64.))
           (DO ((J 0 (+ J BOX-Y-SIZE)))
               ((> (+ J GRID-POINT-SIZE) 64.))
             (BITBLT TV:ALU-IOR GRID-POINT-SIZE GRID-POINT-SIZE
                     GRID-BITBLT-ONES 0 0 GRID-BITBLT-KLUDGE I J)))

         ;; Smear the array over the window.

         (LOOP WITH XINC = (* (TRUNCATE 64. BOX-X-SIZE) BOX-X-SIZE)
               WITH XSIZE = (* (1+ WINDOW-X-SIZE) BOX-X-SIZE)
               FOR I FROM 0 BY XINC BELOW XSIZE
            DO (LOOP WITH YINC = (* (TRUNCATE 64. BOX-Y-SIZE) BOX-Y-SIZE)
                     WITH YSIZE = (* (1+ WINDOW-Y-SIZE) BOX-Y-SIZE)
                     FOR J FROM 0 BY YINC BELOW YSIZE
                  DO (SEND SELF :BITBLT TV:ALU-SETA
                                        (MIN (- XSIZE I) XINC) (MIN (- YSIZE J) YINC)
                                        GRID-BITBLT-KLUDGE 0 0 I J))))))

;;; Complement the state of a point in the grid, and store the new value in our array
;;;  FROM-REDISPLAY means that this value came from the other data structure, so don't
;;;  bother trying to update it.

(DEFMETHOD (GRID-MIXIN :REDISPLAY-POINT) (I J &OPTIONAL NEW-VALUE OLD-VALUE)
  (OR (= NEW-VALUE OLD-VALUE)
      (%DRAW-RECTANGLE BOX-X-SIZE BOX-Y-SIZE
                       (+ (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
                       (+ (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF))
                       TV:ALU-XOR SELF))
  (SETF (AREF WINDOW-ARRAY I J) NEW-VALUE))

;This is what mouse commands use to alter a point to a new value.
(DEFMETHOD (GRID-MIXIN :DRAW-POINT) (I J NEW-VALUE)
  ;; First set the value (normal plane only)
  (SEND SELF :ASET NEW-VALUE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))
  ;; Then redisplay both planes.
  (TV:PREPARE-SHEET (SELF)
    (SEND SELF :REDISPLAY-POINT I J
               (SEND SELF :AREF (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))
               (AREF WINDOW-ARRAY I J))))

(DEFVAR GRAY-ARRAY (LET ((ARRAY (TV:MAKE-PIXEL-ARRAY 32. 4 :ELEMENT-TYPE 'BIT)))
                     (DOTIMES (I 32.) (DOTIMES (J 4) (AS-2-REVERSE (LOGXOR I J) ARRAY I J)))
                     ARRAY))

(DEFMETHOD (GRID-MIXIN :GRAY-POINT) (X Y)
  (SEND SELF :BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE GRAY-ARRAY 0 0
                                (* X BOX-X-SIZE) (* Y BOX-Y-SIZE)))

(DEFMETHOD (GRID-MIXIN :SET-BOX-SIZE) (&OPTIONAL (NEW-X-SIZE DEFAULT-BOX-SIZE)
                                       (NEW-Y-SIZE NEW-X-SIZE))
  (UNLESS (AND (= BOX-X-SIZE NEW-X-SIZE) (= BOX-Y-SIZE NEW-Y-SIZE))
    (SETQ BOX-X-SIZE NEW-X-SIZE
          BOX-Y-SIZE NEW-Y-SIZE
          REDISPLAY-DEGREE REDISPLAY-ALL)
    (DEDUCE-WINDOW-ARRAY-SIZE)))

;;; This performs the indicated operation on the grid until you release the button.

(DEFMETHOD (GRID-MIXIN :MOUSE-BOOLE-SQUARES) (BOOLE CLICK-XPOS CLICK-YPOS)
  (SEND SELF :REDISPLAY T)                      ;Force redisplay to completion first
  (LET (OLD-X OLD-Y (FIRST T) (BOOLE1 BOOLE))
    (SEND SELF :HANDLE-MOUSE-EXPLICITLY
               #'(LAMBDA (WINDOW-X WINDOW-Y)
                   (TV:MOUSE-SET-BLINKER-CURSORPOS :MOUSE-MOVES WINDOW-X WINDOW-Y)
                   (LET ((X (TRUNCATE (- WINDOW-X TV:LEFT-MARGIN-SIZE 1) BOX-X-SIZE))
                         (Y (TRUNCATE (- WINDOW-Y TV:TOP-MARGIN-SIZE 1) BOX-Y-SIZE)))
                     (COND ((NOT (AND (LESSP -1 X WINDOW-X-SIZE)
                                      (LESSP -1 Y WINDOW-Y-SIZE)))
                            NIL)
                           ((AND (NOT FIRST) (= X OLD-X) (= Y OLD-Y))
                            T)
                           (T
                            (SETQ OLD-X X OLD-Y Y FIRST NIL)
                            (LET* ((OLD-VALUE (LOGAND 1 (AREF WINDOW-ARRAY X Y)))
                                   (NEW-VALUE (BOOLE BOOLE1 1 OLD-VALUE)))
                              (OR (= OLD-VALUE NEW-VALUE)
                                  (SEND SELF :DRAW-POINT X Y NEW-VALUE)))
                            T))))
               CLICK-XPOS CLICK-YPOS)))

(DEFMETHOD (GRID-MIXIN :HANDLE-MOUSE-EXPLICITLY)
           (MOVE-HANDLER CLICK-X CLICK-Y
             &AUX (WINDOW-X-OFFSET 0) (WINDOW-Y-OFFSET 0)
                  WINDOW-X WINDOW-Y)
  (TV:WITH-MOUSE-GRABBED
    (SEND SELF :MOUSE-STANDARD-BLINKER)
    (MULTIPLE-VALUE-SETQ (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
      (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET))
    (DO ((WAIT-FLAG NIL T)
         OLD-X OLD-Y)
        (())
      (COND (WAIT-FLAG
             (TV:MOUSE-WAIT OLD-X OLD-Y)
             (SETQ OLD-X TV:MOUSE-X OLD-Y TV:MOUSE-Y))
            (T (SETQ OLD-X (+ CLICK-X WINDOW-X-OFFSET)
                     OLD-Y (+ CLICK-Y WINDOW-Y-OFFSET))))
      (SETQ WINDOW-X (- OLD-X WINDOW-X-OFFSET)
            WINDOW-Y (- OLD-Y WINDOW-Y-OFFSET))
      ;; Update the position of the mouse before checking for button clicks, so
      ;; that button clicks get processed with knowledge of where the mouse
      ;; was when the button was first clicked.  The arguments to the move handler
      ;; may be where the mouse was when the button was clicked, whereas the
      ;; mouse cursor follows MOUSE-X and MOUSE-Y, which may be different.
      (OR (FUNCALL MOVE-HANDLER WINDOW-X WINDOW-Y)
          (RETURN NIL))
      (IF (ZEROP (TV:MOUSE-BUTTONS)) (RETURN NIL)))))

(DEFMETHOD (GRID-MIXIN :SET-OFFSET) (NEW-X-POS NEW-Y-POS)
  ;; Must redisplay the areas we occupied BEFORE
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS)
  (SETQ WINDOW-X-POS NEW-X-POS
        WINDOW-Y-POS NEW-Y-POS)
  ;; and the ones we occupy AFTER.
  (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE))

(DEFMETHOD (GRID-MIXIN :MOVE-PLANE) (X-MOTION Y-MOTION &OPTIONAL HOME-FIRST)
  (IF HOME-FIRST
      (SEND SELF :SET-OFFSET
                 (- X-MOTION)
                 (- Y-MOTION))
      (SEND SELF :SET-OFFSET
                 (- WINDOW-X-POS X-MOTION)
                 (- WINDOW-Y-POS Y-MOTION))))

(DEFMETHOD (GRID-MIXIN :DRAW-GRID-LINE) (X0 Y0 X1 Y1 DRAW-MODE &AUX DX DY YI FLAG)
  (SETQ DX (- X1 X0)
        DY (- Y1 Y0))
  (AND (MINUSP DX)
       (SETQ DX (- DX) X0 X1 DY (- DY) Y0 Y1))
  (IF (MINUSP DY)
      (SETQ DY (- DY) YI -1)
      (SETQ YI 1))
  (AND (SETQ FLAG (> DY DX)) (PSETQ DX DY DY DX))
  (DO ((A (TRUNCATE DX 2))
       (C DX (1- C)))
      ((< C 0))
    (SELECTQ DRAW-MODE
      ;; IOR
      (7 (AND (ZEROP (AREF WINDOW-ARRAY X0 Y0))
              (SEND SELF :DRAW-POINT X0 Y0 1)))
      ;; ANDCA
      (2 (OR (ZEROP (AREF WINDOW-ARRAY X0 Y0))
             (SEND SELF :DRAW-POINT X0 Y0 0)))
      ;; XOR
      (6 (SEND SELF :DRAW-POINT X0 Y0
                                (- 1 (AREF WINDOW-ARRAY X0 Y0))))
      ;; LATCH must be bound away before calling this!  (IOR suggested.)
      (0 (FERROR NIL "draw-mode LATCH not acceptable here -- fed programming/maintenance error.")))
    (COND ((MINUSP (SETQ A (- A DY)))
           (SETQ A (+ A DX))
           (SETQ X0 (1+ X0) Y0 (+ Y0 YI)))
          (FLAG
           (SETQ Y0 (+ Y0 YI)))
          (T
           (SETQ X0 (1+ X0))))))

(DEFMETHOD (GRID-MIXIN :DRAW-CURVE) (PX PY &OPTIONAL END (DRAW-MODE 7))
  (OR END (SETQ END (LENGTH PX)))
  (DO ((I 1 (1+ I))
       (X0)
       (X1 (FIX (AREF PX 0)))
       (Y0)
       (Y1 (FIX (AREF PY 0))))
      (( I END))
    (SETQ X0 X1)
    (OR (SETQ X1 (AREF PX I)) (RETURN NIL))
    (SETQ X1 (FIX X1))
    (SETQ Y0 Y1)
    (OR (SETQ Y1 (AREF PY I)) (RETURN NIL))
    (SETQ Y1 (FIX Y1))
    (SEND SELF :DRAW-GRID-LINE X0 Y0 X1 Y1 DRAW-MODE)))

;;; Grid windows that display a plane.

(DEFFLAVOR PLANE-GRID-MIXIN
       (PLANE)
       (GRID-MIXIN)
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION "A grid window that displays a plane. The plane instance variable is displayed
in the grid and updated when it is changed via the mouse."))

(DEFUN PLANE-ENSURE-EXISTS (PLANE X Y)
  "Make sure that location X, Y in PLANE is explicitly represented."
  (SETF (PLANE-AREF PLANE X Y) (PLANE-AREF PLANE X Y)))

(DEFUN PLANE-END (PLANE)
  "Return a list whose elements are the ends of the explicitly allocated index regions of PLANE.
Each element corresponds to one dimension, and is one plus the highest
value in that dimension for which storage in PLANE is allocated."
  (MAPCAR '+ (PLANE-ORIGIN PLANE) (ARRAY-DIMENSIONS PLANE)))

(DEFUN PLANE-EDGES (PLANE)
  "Return a list containing the origin of PLANE followed by the end.
The elements of the list completely describe what coordinate ranges have actual storage."
  (APPEND (PLANE-ORIGIN PLANE) (PLANE-END PLANE)))

(DEFMETHOD (PLANE-GRID-MIXIN :AREF) (I J)
  (PLANE-AREF PLANE I J))

(DEFMETHOD (PLANE-GRID-MIXIN :ASET) (VAL I J)
  (SETF (PLANE-AREF PLANE I J) VAL))

(DEFMETHOD (PLANE-GRID-MIXIN :PLANE-EDGES) ()
  (PLANE-EDGES PLANE))

;;; Note that the entire contents of the planes is new.
(DEFMETHOD (PLANE-GRID-MIXIN :MUST-REDISPLAY-ENTIRE-PLANE) (&OPTIONAL PLANE1)
  (LEXPR-SEND SELF :MUST-REDISPLAY REDISPLAY-SOME
                   (MAPCAR #'-
                           (IF PLANE1 (PLANE-EDGES PLANE1)
                             (SEND SELF :PLANE-EDGES))
                           (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS))))

;;; When we are about to change the size or offsets of the planes,
;;; note that every box NOW in the planes must be redisplayed.
(DEFMETHOD (PLANE-GRID-MIXIN :MUST-REDISPLAY-CURRENT-AREAS) (&OPTIONAL PLANE1)
  (SEND SELF :MUST-REDISPLAY REDISPLAY-SOME
                             MIN-CHANGED-X MIN-CHANGED-Y
                             MAX-CHANGED-X MAX-CHANGED-Y)
  (LET ((CURRENT-PLANE-AREAS (MAPCAR #'-
                                     (IF PLANE1 (PLANE-EDGES PLANE1)
                                       (SEND SELF :PLANE-EDGES))
                                       (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS))))
    (SETQ PREVIOUS-EDGES
          (MAPCAR #'FUNCALL '(MIN MIN MAX MAX) PREVIOUS-EDGES CURRENT-PLANE-AREAS))))

(DEFMETHOD (PLANE-GRID-MIXIN :ERASE-ALL) ()
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS PLANE)
  (SETQ PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.)))

(DEFFLAVOR GRAY-GRID-MIXIN
        ((GRAY-PLANE NIL)
         ;; Add these offsets to a co-ordinate in the regular plane
         ;; to get the corresponding coordinate in the gray plane.
         (GRAY-X-OFFSET 0)
         (GRAY-Y-OFFSET 0))
        ()
  (:REQUIRED-FLAVORS PLANE-GRID-MIXIN)
  (:DEFAULT-INIT-PLIST :WINDOW-ARRAY-TYPE 'ART-2B))

(DEFUN DISPLAY-PLANE (PLANE)
  "Print out contents of two-dimensional plane PLANE as matrix of characters."
  (LET ((EDGES (PLANE-EDGES PLANE)))
    (DO ((I (CAR EDGES) (1+ I))) ((= I (CADDR EDGES)))
      (DO ((J (CADR EDGES) (1+ J))) ((= J (CADDDR EDGES)))
        (PRINC (PLANE-AREF PLANE I J)))
      (TERPRI))
    (TERPRI)))

(DEFMETHOD (GRAY-GRID-MIXIN :AREF) (I J)
  (IF GRAY-PLANE
      (DPB (PLANE-AREF GRAY-PLANE (+ I GRAY-X-OFFSET) (+ J GRAY-Y-OFFSET))
           (BYTE 1 1)
           (PLANE-AREF PLANE I J))
    (PLANE-AREF PLANE I J)))

(DEFMETHOD (GRAY-GRID-MIXIN :PLANE-EDGES) ()
  (LET* ((PLANE-EDGES (PLANE-EDGES PLANE))
         (GRAY-PLANE-EDGES
           (AND GRAY-PLANE (PLANE-EDGES GRAY-PLANE))))
    (IF GRAY-PLANE
        (LIST (MIN (FIRST PLANE-EDGES) (- (FIRST GRAY-PLANE-EDGES) GRAY-X-OFFSET))
              (MIN (SECOND PLANE-EDGES) (- (SECOND GRAY-PLANE-EDGES) GRAY-Y-OFFSET))
              (MAX (THIRD PLANE-EDGES) (- (THIRD GRAY-PLANE-EDGES) GRAY-X-OFFSET))
              (MAX (FOURTH PLANE-EDGES) (- (FOURTH GRAY-PLANE-EDGES) GRAY-Y-OFFSET)))
      PLANE-EDGES)))

(DEFMETHOD (GRAY-GRID-MIXIN :MOVE-GRAY-PLANE) (X-MOTION Y-MOTION &OPTIONAL HOME-FIRST)
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS GRAY-PLANE)
  (IF HOME-FIRST (SETQ GRAY-X-OFFSET 0 GRAY-Y-OFFSET 0))
  (SETQ GRAY-X-OFFSET (- GRAY-X-OFFSET X-MOTION))
  (SETQ GRAY-Y-OFFSET (- GRAY-Y-OFFSET Y-MOTION))
  (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE GRAY-PLANE))

(DEFMETHOD (GRAY-GRID-MIXIN :ERASE-GRAY) ()
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS GRAY-PLANE)
  (SETQ GRAY-PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.)))

;Array of gray tone with only 1 out of 4 points black.
(DEFVAR LIGHT-GRAY-ARRAY (LET ((ARRAY (TV:MAKE-PIXEL-ARRAY 32. 4 :ELEMENT-TYPE 'BIT)))
                           (DOTIMES (I 32.)
                             (DOTIMES (J 4)
                               (AS-2-REVERSE (LOGAND 1 I (LOGXOR J 1)) ARRAY I J)))
                           ARRAY))

(DEFMETHOD (GRAY-GRID-MIXIN :REDISPLAY-POINT) (I J NEW-VALUE OLD-VALUE)
  (IF (BIT-TEST 1 (LOGXOR NEW-VALUE OLD-VALUE))
      (%DRAW-RECTANGLE BOX-X-SIZE BOX-Y-SIZE
                       (+ 1 (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
                       (+ 1 (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF))
                       TV:ALU-XOR SELF))
  (IF (BIT-TEST 2 (LOGXOR NEW-VALUE OLD-VALUE))
      (BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE LIGHT-GRAY-ARRAY 0 0
              (TV:SHEET-SCREEN-ARRAY SELF)
              (+ 1 (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
              (+ 1 (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF))))
  (SETF (AREF WINDOW-ARRAY I J) NEW-VALUE))

;;; Plane windows with a special outline someplace (the character box and baseline).

(DEFFLAVOR CHAR-BOX-GRID-MIXIN
       ((CHAR-BOX-X1 0) (CHAR-BOX-Y1 0)                 ; The real position.
        (CHAR-BOX-X2 0) (CHAR-BOX-Y2 0)
        (CHAR-BOX-Y3 0)
        (INHIBIT-CHAR-BOX NIL)
        DISPLAYED-CHAR-BOX-X1 DISPLAYED-CHAR-BOX-Y1     ; The displayed position.
        DISPLAYED-CHAR-BOX-X2 DISPLAYED-CHAR-BOX-Y2
        DISPLAYED-CHAR-BOX-Y3)
       ()
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS GRID-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE
    "Grid windows with a special outline. The outline is used to show
     the actual character area and baseline by the font-editor."))

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :MOVE-CHAR-BOX) (X-MOTION Y-MOTION)
  (INCF CHAR-BOX-X1 X-MOTION)
  (INCF CHAR-BOX-X2 X-MOTION)
  (INCF CHAR-BOX-Y1 Y-MOTION)
  (INCF CHAR-BOX-Y2 Y-MOTION)
  (INCF CHAR-BOX-Y3 Y-MOTION))

;;; When the grid gets drawn, draw the character box as well.

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :DRAW-GRID) ()
  (COND ((NOT INHIBIT-CHAR-BOX)
         (SETQ DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS)
               DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS)
               DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS)
               DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS)
               DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS))
         (SEND SELF :DISPLAY-CHAR-BOX))
        (T (SETQ DISPLAYED-CHAR-BOX-X1 NIL))))

;;; After redisplay, check that the character box is correct.

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :REDISPLAY) (&REST IGNORE)
  (COND ((OR REDISPLAY-SUPPRESSED (= BOX-X-SIZE 1) (= BOX-Y-SIZE 1)))
        ((AND DISPLAYED-CHAR-BOX-X1
              (= DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS))
              (= DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS))
              (= DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS))
              (= DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS))
              (= DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS))))
        (T
         (AND DISPLAYED-CHAR-BOX-X1
              (SEND SELF :DISPLAY-CHAR-BOX))
         (COND ((NULL INHIBIT-CHAR-BOX)
                (SETQ DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS))
                (SETQ DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS))
                (SETQ DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS))
                (SETQ DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS))
                (SETQ DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS))
                (SEND SELF :DISPLAY-CHAR-BOX))))))

;;; XOR the char box and the baseline line in.

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :DISPLAY-CHAR-BOX) ()
  (LET ((X1 (* BOX-X-SIZE DISPLAYED-CHAR-BOX-X1))
        (Y1 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y1))
        (X2 (* BOX-X-SIZE DISPLAYED-CHAR-BOX-X2))
        (Y2 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y2))
        (Y3 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y3)))
    (SEND SELF :DRAW-RECTANGLE 2 (- Y2 Y1) X1 Y1 TV:ALU-XOR)
    (COND ((= X1 X2))
          (T
           (SEND SELF :DRAW-RECTANGLE (- X2 X1) 2 (+ 2 X1) Y1 TV:ALU-XOR)
           (SEND SELF :DRAW-RECTANGLE 2 (- Y2 Y1) X2 (+ 2 Y1) TV:ALU-XOR)
           (SEND SELF :DRAW-RECTANGLE (- X2 X1) 2 X1 Y2 TV:ALU-XOR)
           (OR (= Y2 Y3)
               (SEND SELF :DRAW-RECTANGLE (- X2 -2 X1) 2 X1 Y3 TV:ALU-XOR))))))

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :ERASE-ALL) ()
  (SETQ CHAR-BOX-X1 0 CHAR-BOX-Y1 0
        CHAR-BOX-X2 7 CHAR-BOX-Y2 11 CHAR-BOX-Y3 14))

;;; Push this button when the mouse is near an edge or corner of the character box,
;;;  and then as long as you hold the button down you are moving that corner.

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :MOUSE-MOVE-CHAR-BOX) (&AUX X-POS-NAME Y-POS-NAME XOFF YOFF)
  (MULTIPLE-VALUE (XOFF YOFF) (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET))

  ;; Decide which corner or edge of the character box we will move
  ;;  (or maybe we aren't in range of any of them).
  ;;  All horizontal edges move together,
  ;;  and so do all vertical edges, since the size of the box
  ;;  is set by mouse-sensitive items in FED-LABEL-WINDOW

  (COND ((< (ABS (- TV:MOUSE-X (* (- CHAR-BOX-X1 WINDOW-X-POS) BOX-X-SIZE) XOFF))
            (TRUNCATE BOX-X-SIZE 2))
         (SETQ X-POS-NAME 'CHAR-BOX-X1))
        ((< (ABS (- TV:MOUSE-X (* (- CHAR-BOX-X2 WINDOW-X-POS) BOX-X-SIZE) XOFF))
            (TRUNCATE BOX-X-SIZE 2))
         (SETQ X-POS-NAME 'CHAR-BOX-X2)))
  (COND ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y1 WINDOW-Y-POS) BOX-Y-SIZE) YOFF))
            (TRUNCATE BOX-Y-SIZE 2))
         (SETQ Y-POS-NAME 'CHAR-BOX-Y1))
        ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y2 WINDOW-Y-POS) BOX-Y-SIZE) YOFF))
            (TRUNCATE BOX-Y-SIZE 2))
         (SETQ Y-POS-NAME 'CHAR-BOX-Y2))
        ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y3 WINDOW-Y-POS) BOX-Y-SIZE) YOFF))
            (TRUNCATE BOX-Y-SIZE 2))
         (SETQ Y-POS-NAME 'CHAR-BOX-Y3)))

  (IF (NOT (OR X-POS-NAME Y-POS-NAME))                  ; If not in range to move any edge,
      (TV:BEEP)                                         ;  complain.

      (DO ((NOT-FIRST NIL T) (X) (Y) (OX) (OY) (OLD-M-X) (OLD-M-Y)
           DELTA-X DELTA-Y)
          ((AND NOT-FIRST (ZEROP TV:MOUSE-LAST-BUTTONS)))
        (AND NOT-FIRST (TV:MOUSE-WAIT OLD-M-X OLD-M-Y))
        (OR (EQ SELF (TV:WINDOW-OWNING-MOUSE))
            (RETURN NIL))
        (SETQ OLD-M-X TV:MOUSE-X OLD-M-Y TV:MOUSE-Y)
        (SETQ X (TRUNCATE (+ (TRUNCATE BOX-X-SIZE 2) (- TV:MOUSE-X XOFF)) BOX-X-SIZE))
        (SETQ Y (TRUNCATE (+ (TRUNCATE BOX-Y-SIZE 2) (- TV:MOUSE-Y YOFF)) BOX-Y-SIZE))

        ;; Exit if mouse is outside of FED grid area.

        (OR (AND (LESSP -1 X (1+ WINDOW-X-SIZE)) (LESSP -1 Y (1+ WINDOW-Y-SIZE)))
            (RETURN NIL))
        (SETQ X (+ X WINDOW-X-POS) Y (+ Y WINDOW-Y-POS))

        ;; Try moving the edges, remember where they used to be.

        (COND (NOT-FIRST
               (SETQ DELTA-X (IF X-POS-NAME (- X OX) 0))
               (INCF CHAR-BOX-X1 DELTA-X)
               (INCF CHAR-BOX-X2 DELTA-X)
               (SETQ DELTA-Y (IF Y-POS-NAME (- Y OY) 0))
               (INCF CHAR-BOX-Y1 DELTA-Y)
               (INCF CHAR-BOX-Y2 DELTA-Y)
               (INCF CHAR-BOX-Y3 DELTA-Y)

               ;; If we are really moving an edge to a new place, redisplay.

               (OR (AND (ZEROP DELTA-X)
                        (ZEROP DELTA-Y))
                   (SEND SELF :REDISPLAY))))
        (SETQ OX X OY Y))))



;;; The Font Editor itself.

(DEFFLAVOR BASIC-FED ((CURRENT-FONT NIL)
                      (CURRENT-CHARACTER NIL)
                      (UNSAVED-CHANGES NIL)
                      (CURSOR-X 0)
                      (CURSOR-Y 0)
                      (CURSOR-ON NIL))
           (GRAY-GRID-MIXIN PLANE-GRID-MIXIN CHAR-BOX-GRID-MIXIN
            TV:LIST-MOUSE-BUTTONS-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE
    "The font editor itself uses its grid for displaying the character being edited."))

(DEFMETHOD (BASIC-FED :SELECTED-FONT) ()
  CURRENT-FONT)

(DEFMETHOD (BASIC-FED :AFTER :INIT) (&REST IGNORE)
  (SETQ PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
  (SETQ GRAY-PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
  (SEND SELF :ERASE-ALL)
  (SEND SELF :HOME-BOX))

(DEFMETHOD (BASIC-FED :AFTER :ERASE-ALL) ()
  (SETQ UNSAVED-CHANGES (NOT (NULL CURRENT-CHARACTER)))
  (AND CURRENT-FONT
       (LET ((FD (FONT-GET-FD CURRENT-FONT)))
         (SETQ CHAR-BOX-Y2 (FD-BASELINE FD)
               CHAR-BOX-X2 (FIXR (FD-SPACE-WIDTH FD))
               CHAR-BOX-Y3 (FD-LINE-SPACING FD)))))

(DEFWRAPPER (BASIC-FED :DRAW-POINT) (IGNORE . BODY)
  `(PROGN (SETQ UNSAVED-CHANGES T)
          . ,BODY))

(DEFMETHOD (BASIC-FED :CONTENTS) ()
  (LIST PLANE
        CHAR-BOX-X1 CHAR-BOX-Y1
        CHAR-BOX-X2 CHAR-BOX-Y2
        CHAR-BOX-Y3))

(DEFMETHOD (BASIC-FED :SET-CONTENTS) (OTHER-PLANE BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  (SETQ PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
  (MERGE-OTHER-PLANE OTHER-PLANE 0 0)
  (SETQ CHAR-BOX-X1 BOX-X1
        CHAR-BOX-Y1 BOX-Y1
        CHAR-BOX-X2 BOX-X2
        CHAR-BOX-Y2 BOX-Y2
        CHAR-BOX-Y3 BOX-Y3)
  (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)
  (SETQ UNSAVED-CHANGES T)
  (SEND SELF :HOME-BOX))

(DEFMETHOD (BASIC-FED :MERGE-CONTENTS) (MERGE-OP OTHER-PLANE
                                        BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  BOX-X2 BOX-Y1 BOX-Y3
  (IF (EQ MERGE-OP ':COPY)
      (SETQ PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.)
            MERGE-OP ':SET))
  (SETQ UNSAVED-CHANGES T)
  (MERGE-OTHER-PLANE OTHER-PLANE
                     (- BOX-X1 CHAR-BOX-X1)
                     (- BOX-Y2 CHAR-BOX-Y2)
                     MERGE-OP))

(DEFMETHOD (BASIC-FED :MERGE-GRAY) (MERGE-OP OTHER-PLANE
                                    BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  BOX-X2 BOX-Y1 BOX-Y3
  (IF (EQ MERGE-OP ':COPY)
      (SETQ GRAY-PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.)
            MERGE-OP ':SET))
  (MERGE-OTHER-PLANE OTHER-PLANE
                     (- BOX-X1 CHAR-BOX-X1 GRAY-X-OFFSET)
                     (- BOX-Y2 CHAR-BOX-Y2 GRAY-Y-OFFSET)
                     MERGE-OP GRAY-PLANE))

;;; Return the window of the FED window to home position.

(DEFMETHOD (BASIC-FED :HOME-BOX) ()
  (SEND SELF :SET-OFFSET
             (- (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2) (TRUNCATE WINDOW-X-SIZE 2))
             (- (TRUNCATE (+ CHAR-BOX-Y1 CHAR-BOX-Y3) 2) (TRUNCATE WINDOW-Y-SIZE 2)))
  (SETQ CURSOR-X 0 CURSOR-Y 0))

(DEFMETHOD (BASIC-FED :BEFORE :SET-OFFSET) (X Y)
  (SETQ CURSOR-X (MAX 0 (MIN WINDOW-X-SIZE (- CURSOR-X (- X WINDOW-X-POS)))))
  (SETQ CURSOR-Y (MAX 0 (MIN WINDOW-Y-SIZE (- CURSOR-Y (- Y WINDOW-Y-POS))))))

(DEFMETHOD (BASIC-FED :AFTER :REDISPLAY) (&REST IGNORE)
  (WHEN (AND CURSOR-ON (NOT REDISPLAY-SUPPRESSED))
    (MULTIPLE-VALUE-BIND (X Y) (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET)
      (TV:BLINKER-SET-CURSORPOS TV:MOUSE-BLINKER
                                (+ X (ROUND (* BOX-X-SIZE (+ 0.5s0 CURSOR-X))))
                                (+ Y (ROUND (* BOX-Y-SIZE (+ 0.5s0 CURSOR-Y))))))
    (TV:BLINKER-SET-VISIBILITY TV:MOUSE-BLINKER :BLINK)))


;;; REGISTER-PANE is a kind of basic-fed that only displays its contents
;;; and passes mouse-clicks along to the FED-WINDOW itself.

(DEFFLAVOR REGISTER-PANE ()
           (BASIC-FED TV:WINDOW)
  (:DEFAULT-INIT-PLIST :LABEL NIL :BLINKER-P NIL :BOX-X-SIZE 4 :BOX-Y-SIZE 4
                       :INHIBIT-CHAR-BOX T))

(DEFMETHOD (REGISTER-PANE :WHO-LINE-DOCUMENTATION-STRING) ()
  "L: Load from FED   M: Retrieve contents to FED   R: Menu")

(DEFMETHOD (REGISTER-PANE :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P
      INHIBIT-CHAR-BOX
      (PROGN (SEND SELF :MUST-REDISPLAY REDISPLAY-ALL)
             (SEND SELF :REDISPLAY T))))

(DEFMETHOD (REGISTER-PANE :BEFORE :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SEND SELF :SET-BOX-SIZE (SEND TV:SUPERIOR :REGISTER-BOX-SIZE)))

(DEFMETHOD (REGISTER-PANE :AFTER :ERASE-ALL) ()
  (SETQ INHIBIT-CHAR-BOX T))

(DEFMETHOD (REGISTER-PANE :MOUSE-CLICK) (BUTTON X Y)
  (DECLARE (IGNORE X Y))
  (AND (= BUTTON #/MOUSE-1-1) (NEQ SELF TV:SELECTED-WINDOW)
       (TV:MOUSE-SELECT TV:SUPERIOR))
  (SEND SELF :FORCE-KBD-INPUT `(:TYPEOUT-EXECUTE :REGISTER-CLICK ,SELF ,BUTTON))
  T)

(DEFMETHOD (REGISTER-PANE :AFTER :MERGE-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX NIL)
  (SEND SELF :REDISPLAY T))

(DEFMETHOD (REGISTER-PANE :AFTER :SET-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX NIL)
  (SEND SELF :REDISPLAY T))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:29:59
(DEFUN FED ()
  "Select a font editor window."
  (TV:SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'FED-FRAME)
  (tv:await-window-exposure))

;;; The actual FED-WINDOW.

(DEFFLAVOR FED-TYPEOUT-WINDOW ()
           (TV:ANY-TYI-MIXIN TV:TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS))

(DEFMETHOD (FED-TYPEOUT-WINDOW :MORE-EXCEPTION) (&AUX CH)       ;imported from zwei & altered.
  (UNLESS (ZEROP (TV:SHEET-MORE-FLAG))
    (SETQ CH (TV:SHEET-MORE-HANDLER ':ANY-TYI))
    (WHEN (OR (MEMQ CH '(#/RUBOUT #/CR))        ;kbd flush command
              (and ch (LISTP CH)))              ;mouse selection flush & preserve blip
      (SEND SELF ':UNTYI CH)
      (SIGNAL-CONDITION EH:ABORT-OBJECT))))

(DEFFLAVOR FED
          ((DRAW-MODE 0)                        ; Initially LATCH.
           (LABEL-WINDOW NIL)
           PROMPT-WINDOW
           (SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
           (SAMPLE-STRING "Sample")
           (FINAL-COLOR-ARRAY NIL))             ;medium res support
  (BASIC-FED TV:INTRINSIC-NO-MORE-MIXIN TV:WINDOW-WITH-TYPEOUT-MIXIN
   TV:PROCESS-MIXIN TV:WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES PROMPT-WINDOW LABEL-WINDOW)
  (:DOCUMENTATION :COMBINATION "The actual FED window"))

(DEFMETHOD (FED :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR SPECIAL-COMMAND-MOUSE-DOCUMENTATION
      (SELECTQ DRAW-MODE
        (7
         "L:Draw dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu")
        (2
         "L:Erase dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu")
        (6
         "L:Flip dots, M:Change mode (Draw//Erase//Flip), R:Move edges of char box, R2:System menu")
        (0
         "L:Draw in flip of first dot, M:Change mode (Draw//Erase//Flip//Latch), R:Move edges of char box, R2:System menu")
        )))

(DEFMETHOD (FED :SYMBOLIC-DRAW-MODE) ()
  (SELECTQ DRAW-MODE
    (7 'SET)
    (2 'CLEAR)
    (6 'XOR)
    (0 'LATCH)))

(DEFMETHOD (FED :DRAW-MODE-STRING) ()
  (SELECTQ DRAW-MODE
    (7 "Draw")
    (2 "Erase")
    (6 "Flip")
    (0 "Latch")))

;Don't check for double-click on left and middle button in this pane.
;This makes the single clicks faster.
(DEFMETHOD (FED :MOUSE-BUTTONS) (BD X Y)
  (IF (= BD 4)
      (LET ((BUTTONS (TV:MOUSE-BUTTON-ENCODE BD)))
        (IF (= BUTTONS #/MOUSE-3-2)
            (TV:MOUSE-CALL-SYSTEM-MENU)
          (SEND SELF :MOUSE-CLICK BUTTONS X Y)))
    (SEND SELF :MOUSE-CLICK (TV:MAKE-MOUSE-CHAR (1- (HAULONG BD)) 0) X Y)))

(DEFMETHOD (FED :AFTER :INIT) (&REST IGNORE)
  (OR TV:TYPEOUT-WINDOW
      (SETQ TV:TYPEOUT-WINDOW (MAKE-INSTANCE 'FED-TYPEOUT-WINDOW
                                             :ITEM-TYPE-ALIST
                                               '((FONT :SELECT-FONT "L: Select this font for editing")
                                                 (CHARACTER :SELECT-CHAR "L: Select this character"))
                                             :DEEXPOSED-TYPEOUT-ACTION '(:EXPOSE-FOR-TYPEOUT)
                                             :IO-BUFFER TV:IO-BUFFER
                                             :SUPERIOR SELF)))
  (SETQ TV:PROCESS (MAKE-PROCESS TV:NAME NIL :SPECIAL-PDL-SIZE 4000.))
  (SEND TV:PROCESS :PRESET SELF :COMMAND-LOOP)
  (SEND TV:PROCESS :RUN-REASON SELF))

(DEFMETHOD (FED :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P
      (SEND SELF :FORCE-KBD-INPUT '(REDISPLAY))))       ; Make the command loop wake up.

(DEFMETHOD (FED :BEFORE :REDISPLAY) (&REST IGNORE)
  (OR (EQ SELF (SEND TV:SUPERIOR :SELECTED-PANE))
      (SEND TV:SUPERIOR :SELECT-PANE SELF))
  (WHEN (SEND TV:TYPEOUT-WINDOW :ACTIVE-P)
    (SEND TV:TYPEOUT-WINDOW :MAKE-COMPLETE)
    (SEND TV:TYPEOUT-WINDOW :DEACTIVATE)
    (SEND TV:SUPERIOR :SELECT-PANE SELF)
    (SETQ REDISPLAY-DEGREE REDISPLAY-ALL))
  (WHEN (> REDISPLAY-DEGREE REDISPLAY-NONE)
    (REDISPLAY-LABELS)))

;Redisplay the label of a FED window (usually not the selected one).
(DEFMETHOD (FED :REDISPLAY-LABEL-IF-EXPOSED) ()
  (WHEN (SEND TV:SUPERIOR :EXPOSED-P)
    (DISPLAY-LABEL)))

(DEFUN REDISPLAY-LABELS ()
  "Redisplay the labels of all FED windows displaying the font current in this one."
  ;;That is in case they are displaying a sample string
  ;;which includes the char being edited in this FED.
  (DECLARE (:SELF-FLAVOR FED))
  (DISPLAY-LABEL)
  (DOLIST (ELT FED-EDITED-CHARS)
    (AND (EQ CURRENT-FONT (CAR ELT))
         (NEQ SELF (CADDR ELT))
         (SEND (CADDR ELT) :REDISPLAY-LABEL-IF-EXPOSED))))

(DEFMETHOD (FED :MUST-REDISPLAY-LABEL) ()
  (SEND SELF :MUST-REDISPLAY REDISPLAY-ONE
             MIN-CHANGED-X MIN-CHANGED-Y
             MIN-CHANGED-X MIN-CHANGED-Y))

(DEFMETHOD (FED :AFTER :DRAW-POINT) (&REST IGNORE)
  (SEND SELF :MUST-REDISPLAY-LABEL))

(DEFMETHOD (FED :SELECT-CHAR) (CHAR)
  (SETQ CURRENT-CHARACTER CHAR)
  (SETQ PLANE (GOBBLE-CHARACTER CURRENT-FONT CURRENT-CHARACTER T))
  (SETQ UNSAVED-CHANGES NIL)
  (UPDATE-FED-EDITED-CHARS))

;;; If the next thing the user does is click LEFT,
;;; Return the X and Y co-ords of the grid point the user clicks the mouse on.
;;; Otherwise discard his input, beep, and return NIL and the char.

(DEFMETHOD (FED :MOUSE-SELECT-POINT) (&OPTIONAL RIGHT-BUTTON-OK &AUX CH X Y)
  (SETQ CH (SEND SELF :ANY-TYI))
  (COND ((AND (EQ (CAR-SAFE CH) ':MOUSE-BUTTON) (= (CADR CH) #/MOUSE-1-1))
         (MULTIPLE-VALUE-BIND (DX DY) (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET)
           (SETQ X (TRUNCATE (- TV:MOUSE-X DX TV:LEFT-MARGIN-SIZE) BOX-X-SIZE)
                 Y (TRUNCATE (- TV:MOUSE-Y DY TV:TOP-MARGIN-SIZE) BOX-Y-SIZE)))
         (AND (LESSP -1 X WINDOW-X-SIZE) (LESSP -1 Y WINDOW-Y-SIZE)
              (VALUES X Y)))
        ((AND RIGHT-BUTTON-OK
              (EQ (CAR-SAFE CH) ':MOUSE-BUTTON)
              (= (CADR CH) #/MOUSE-3-1))
         (VALUES NIL CH))
        (T (TV:BEEP)
           (VALUES NIL CH))))

;list of elements describing all chars being edited in fed windows.
;Each element is (font char fed-window)
(DEFVAR FED-EDITED-CHARS NIL)

(DEFUN UPDATE-FED-EDITED-CHARS ()
  "Update the data base of which FED windows are editing which fonts and characters.
Call this after changing the font or character of this FED window."
  (DECLARE (:SELF-FLAVOR FED))
  (DOLIST (ELT FED-EDITED-CHARS)
    (IF (EQ (CADDR ELT) SELF)
        (SETQ FED-EDITED-CHARS (DELQ ELT FED-EDITED-CHARS))))
  (AND CURRENT-FONT CURRENT-CHARACTER
       (PUSH (LIST CURRENT-FONT CURRENT-CHARACTER SELF)
             FED-EDITED-CHARS)))

;; Get the plane in which a certain char/font is being edited,
;; or nil if it is not being edited.
(DEFUN GET-CHAR-EDITING-PLANE (FONTNAME CHAR)
  "If some FED window is editing CHAR in FONTNAME, return the plane storing that FED's data.
Otherwise return NIL."
  (DECLARE (:SELF-FLAVOR FED))
  (DOLIST (ELT FED-EDITED-CHARS)
    (AND (NEQ SELF (CADDR ELT))
         (EQ FONTNAME (CAR ELT))
         (EQ CHAR (CADR ELT))
         (RETURN (SEND (CADDR ELT) :PLANE)))))

;; Don't include inactive FED windows for consideration by other FED windows.
(DEFMETHOD (FED :DEACTIVATE) ()
  (DOLIST (ELT FED-EDITED-CHARS)
    (IF (EQ SELF (CADDR ELT))
        (SETQ FED-EDITED-CHARS (DELQ ELT FED-EDITED-CHARS)))))

;;; FED-FRAMEs, and the various random panes that live in them.

(DEFFLAVOR FED-FRAME
        ((MIN-REGISTERS 2)
         (MAX-REGISTERS 8)
         (REGISTER-PANES NIL))
        (TV:FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
         TV:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER)
  (:DEFAULT-INIT-PLIST
    :PANES
    `((COMMAND-MENU TV:COMMAND-MENU-PANE        ; For the menu.
                    :ITEM-LIST ,MENU-COMMAND-ALIST)
      (FG-COMMAND-MENU TV:COMMAND-MENU-PANE     ; For the menu.
                       :ITEM-LIST ,FG-MENU-COMMAND-ALIST)
      (FED-WINDOW FED                           ; For the Grid.
                  :LABEL NIL :BLINKER-P NIL)
      (LABEL-WINDOW FED-LABEL-WINDOW :BLINKER-P NIL :LABEL NIL :MORE-P NIL)
      (REGISTER-PANE-0 REGISTER-PANE)
      (REGISTER-PANE-1 REGISTER-PANE)
      (REGISTER-PANE-2 REGISTER-PANE)
      (REGISTER-PANE-3 REGISTER-PANE)
      (REGISTER-PANE-4 REGISTER-PANE)
      (REGISTER-PANE-5 REGISTER-PANE)
      (PROMPT-WINDOW TV:WINDOW  ; Prompts and messages
                     :BLINKER-DESELECTED-VISIBILITY :OFF
                     :LABEL NIL :MORE-P NIL
                     :BLINKER-P T)
      (SLIDE-POT-PANE BASIC-FED-POT))           ;medium res support
    :SAVE-BITS T
    :CONSTRAINTS
    '((MAIN . ((COMMAND-MENU FED-WINDOW LABEL-WINDOW REGISTERS PROMPT-WINDOW)
               ((COMMAND-MENU :ASK :PANE-SIZE))
               ((PROMPT-WINDOW 2 :LINES))
               ((LABEL-WINDOW :ASK :LABEL-WINDOW-SIZE))
               ((REGISTERS :HORIZONTAL (:EVAL (SEND SELF :REGISTER-HEIGHT))
                           (REGISTER-PANE-0 REGISTER-PANE-1 REGISTER-PANE-2
                                            REGISTER-PANE-3 REGISTER-PANE-4 REGISTER-PANE-5)
                           ((REGISTER-PANE-0 :EVEN)
                            (REGISTER-PANE-1 :EVEN)
                            (REGISTER-PANE-2 :EVEN)
                            (REGISTER-PANE-3 :EVEN)
                            (REGISTER-PANE-4 :EVEN)
                            (REGISTER-PANE-5 :EVEN))))
               ((FED-WINDOW :EVEN))))
      (FG-MAIN . ((THE-WHOLE-SHEBANG)
                  ((THE-WHOLE-SHEBANG :HORIZONTAL (1.0)
                    (THE-OLD-SHEBANG SLIDE-POT-PANE)
                    ((SLIDE-POT-PANE 32.))
                    ((THE-OLD-SHEBANG :VERTICAL (:EVEN)
                      (FG-COMMAND-MENU FED-WINDOW LABEL-WINDOW REGISTERS PROMPT-WINDOW)
                      ((FG-COMMAND-MENU :ASK :PANE-SIZE))
                      ((PROMPT-WINDOW 2 :LINES))
                      ((LABEL-WINDOW :ASK :LABEL-WINDOW-SIZE))
                      ((REGISTERS :HORIZONTAL (:EVAL (SEND SELF ':REGISTER-HEIGHT))
                       (REGISTER-PANE-0 REGISTER-PANE-1 REGISTER-PANE-2
                                                   REGISTER-PANE-3 REGISTER-PANE-4 REGISTER-PANE-5)
                       ((REGISTER-PANE-0 :EVEN)
                        (REGISTER-PANE-1 :EVEN)
                        (REGISTER-PANE-2 :EVEN)
                        (REGISTER-PANE-3 :EVEN)
                        (REGISTER-PANE-4 :EVEN)
                        (REGISTER-PANE-5 :EVEN))))
                      ((FED-WINDOW :EVEN)))))))))
    :MORE-P NIL))                               ;USED TO MAKE IT 1/4-SCREEN HERE, NOW FULL SCREEN.

(DEFMETHOD (FED-FRAME :AFTER :INIT) (&REST IGNORE)
  ;; This seems to be the thing required to make selection win.
  (SEND SELF :SELECT-PANE (SEND SELF :GET-PANE 'FED-WINDOW))
  (SEND (SEND SELF :GET-PANE 'LABEL-WINDOW) :SET-FED-WINDOW (SEND SELF :GET-PANE 'FED-WINDOW))
  (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SET-LABEL-WINDOW (SEND SELF :GET-PANE 'LABEL-WINDOW))
  (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SET-PROMPT-WINDOW (SEND SELF :GET-PANE 'PROMPT-WINDOW))
  (WHEN (AND (FBOUNDP 'COLOR:COLOR-EXISTS-P)
             (COLOR:COLOR-EXISTS-P))
    (SEND SELF :SET-CONFIGURATION 'FG-MAIN)     ;medium res support configuration
    (OR *COLOR-CHAR-BOX-AND-LINE*                       ;only if we're the first fed!
        (SETQ *COLOR-CHAR-BOX-AND-LINE* (MAKE-INSTANCE 'BASIC-COLOR-CHAR-BOX-AND-LINE)))))

;Decide what scale to use for the register panes.
;The bigger the font, the smaller the scale.
(DEFMETHOD (FED-FRAME :REGISTER-BOX-SIZE) ()
  (LET ((CURFONT (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SELECTED-FONT)))
    (COND ((NULL CURFONT) 4)
          ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 30.)
           4)
          ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 45.)
           3)
          ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 60.)
           2)
          (T 1))))

(DEFMETHOD (FED-FRAME :REGISTER-HEIGHT) (&REST IGNORE)
  ;; Don't let the registers wipe out the whole frame height.
  (MIN (ROUND (* 0.2s0 TV:HEIGHT))
       (+ 6 (* (SEND SELF :REGISTER-BOX-SIZE)
               (LET ((MINHT (FONT-CHAR-HEIGHT (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN SELF))))
                     (CURFNT (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SELECTED-FONT)))
                 (IF CURFNT
                     (MAX MINHT (FD-LINE-SPACING (FONT-GET-FD CURFNT)))
                   MINHT))))))

(DEFMETHOD (FED-FRAME :NUM-REGISTERS-ACROSS) (WIDTH &REST IGNORE)
  (LET ((REGWIDTH (FIX (* 1.5S0 (SEND SELF :REGISTER-HEIGHT)))))
    (MAX MIN-REGISTERS (MIN MAX-REGISTERS (TRUNCATE WIDTH REGWIDTH)))))

;;; The FED-LABEL-WINDOW displays the font name, character code, sample string.
(DEFFLAVOR FED-LABEL-WINDOW ((FED-WINDOW NIL))
  (TV:BASIC-MOUSE-SENSITIVE-ITEMS
   TV:TRUNCATING-WINDOW)
  :SETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST
    :ITEM-TYPE-ALIST
      '((FONT :PROMPT-LINE-SELECT-FONT "Select new font for editing")
        (LINE-HEIGHT :PROMPT-LINE-SET-LINE-HEIGHT "Set font line height")
        (BASELINE :PROMPT-LINE-SET-BASELINE "Set font height above baseline")
        (BLINKER-HEIGHT :PROMPT-LINE-SET-BLINKER-HEIGHT "Set font blinker height")
        (BLINKER-WIDTH :PROMPT-LINE-SET-BLINKER-WIDTH "Set font blinker width")
        (CHAR :PROMPT-LINE-SELECT-CHAR "Select new character for editing")
        (CHAR-BY-NUMBER :PROMPT-LINE-SELECT-CHAR-CODE "Select new character by octal character code")
        (WIDTH :PROMPT-LINE-SET-CHAR-WIDTH "Specify character width")
        (SAMPLE-STRING :PROMPT-LINE-SET-SAMPLE "Specify new sample string to display"))))

(DEFMETHOD (FED-LABEL-WINDOW :SECOND-LINE-HEIGHT) ()
  (MAX TV:LINE-HEIGHT
       (LET ((FONT (AND FED-WINDOW (SEND FED-WINDOW :SELECTED-FONT))))
         (IF FONT
             (FD-LINE-SPACING (FONT-GET-FD FONT))
           0))))

(DEFMETHOD (FED-LABEL-WINDOW :LABEL-WINDOW-SIZE) (&REST IGNORE)
  (SETF (TV:SHEET-LINE-HEIGHT SELF)
        (FONT-CHAR-HEIGHT (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN SELF))))
  (+ 8. TV:LINE-HEIGHT (SEND SELF :SECOND-LINE-HEIGHT)))

(DEFUN DISPLAY-LABEL ()
  "Redisplay the label window of this FED window's frame."
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((*STANDARD-OUTPUT* LABEL-WINDOW)
        (SECOND-LINE-HEIGHT (SEND LABEL-WINDOW :SECOND-LINE-HEIGHT)))
    ;; Clean up the state of the window, to make it the ordinary one
    ;; for the default font.
    (SETF (TV:SHEET-LINE-HEIGHT LABEL-WINDOW)
          (FONT-CHAR-HEIGHT (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN SELF))))
    (SETF (TV:SHEET-BASELINE LABEL-WINDOW)
          (FONT-BASELINE (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN LABEL-WINDOW))))
    (TV:SHEET-SET-FONT LABEL-WINDOW
                       (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN LABEL-WINDOW)))
    (SEND LABEL-WINDOW :CLEAR-WINDOW)
    ;; Avoid inexplicable **MORE** from FED-TYO.
    (SETF (TV:SHEET-MORE-VPOS LABEL-WINDOW) NIL)
    (PRINC (SEND SELF :DRAW-MODE-STRING))
    (PRINC "  Font: ")
    (SEND LABEL-WINDOW :ITEM 'FONT (OR CURRENT-FONT "None"))
    ;; Now describe the current character.
    ;; We have to print SOMETHING even if there is no current character
    ;; so that there is someplace to put the mouse sensitive items.
    ;; On the other hand, if there is no current FONT, omit these things
    ;; because we don't WANT to tempt the user to try to specify a character.
    (COND (CURRENT-FONT
           (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T)
                 (FD (FONT-GET-FD CURRENT-FONT)))
             (PRINC " Total Ht ")
             (SEND LABEL-WINDOW :ITEM 'LINE-HEIGHT (FD-LINE-SPACING FD))
             (PRINC " Above Base ")
             (SEND LABEL-WINDOW :ITEM 'BASELINE (FD-BASELINE FD))
             (PRINC " Blinker ")
             (SEND LABEL-WINDOW :ITEM 'BLINKER-WIDTH (FD-BLINKER-WIDTH FD))
             (PRINC "x")
             (SEND LABEL-WINDOW :ITEM 'BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)))
           (AND CURRENT-FONT
                (SETF (TV:SHEET-BASELINE LABEL-WINDOW)
                      (MAX (TV:SHEET-BASELINE LABEL-WINDOW)
                           (FD-BASELINE (FONT-GET-FD CURRENT-FONT)))))
           (TV:SHEET-CRLF LABEL-WINDOW)
           ;; This font is already current, but setting it now
           ;; makes the vertical position come out right.
           (TV:SHEET-SET-FONT LABEL-WINDOW
                              (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN LABEL-WINDOW)))
           ;; Set the line height to that for the second line.
           ;; even though we will not CRLF from here,.
           ;; this makes the mouse sensitive items come out the right height.
           (SETF (TV:SHEET-LINE-HEIGHT LABEL-WINDOW) SECOND-LINE-HEIGHT)
           (IF UNSAVED-CHANGES (TYO #/*))
           (PRINC " Char: ")
           (IF CURRENT-CHARACTER
               (SEND LABEL-WINDOW :ITEM 'CHAR-BY-NUMBER NIL "~3O" CURRENT-CHARACTER)
             (SEND LABEL-WINDOW :ITEM 'CHAR-BY-NUMBER "nnn"))
           (PRINC " ")
           (LET ((OLD-X (TV:SHEET-CURSOR-X LABEL-WINDOW)))
             (IF (NULL CURRENT-CHARACTER)
                 (PRINC "None")
               (IF (= CURRENT-CHARACTER #/SPACE)
                   (PRINC "Space")
                 (TYO CURRENT-CHARACTER))
               (WRITE-CHAR #/SPACE)
               (AND (BOUNDP CURRENT-FONT)
                    CURRENT-FONT
                    (SYMBOL-VALUE CURRENT-FONT)
                    (TV:SHEET-SET-FONT LABEL-WINDOW (SYMBOL-VALUE CURRENT-FONT)))
               (FED-TYO LABEL-WINDOW CURRENT-CHARACTER CURRENT-FONT))
             (SEND LABEL-WINDOW :PRIMITIVE-ITEM-OUTSIDE
                                'CHAR NIL
                                OLD-X (TV:SHEET-CURSOR-Y LABEL-WINDOW)
                                (+ 2 (TV:SHEET-CURSOR-X LABEL-WINDOW))
                                (+ (TV:SHEET-CURSOR-Y LABEL-WINDOW) SECOND-LINE-HEIGHT)))
           (TV:SHEET-SET-FONT LABEL-WINDOW
                              (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN LABEL-WINDOW)))
           (AND CURRENT-FONT
                (SETF (TV:SHEET-BASELINE LABEL-WINDOW)
                      (MAX (TV:SHEET-BASELINE LABEL-WINDOW)
                           (FD-BASELINE (FONT-GET-FD CURRENT-FONT)))))
           (PRINC " Width: ")
           (SEND LABEL-WINDOW :ITEM 'WIDTH NIL "~D" (- CHAR-BOX-X2 CHAR-BOX-X1))
           (PRINC "  ")

           ;; Make the sample string area mouse sensitive
           ;; regardless of whether we display any sample string
           ;; or how wide it appears.
           (SEND LABEL-WINDOW :PRIMITIVE-ITEM-OUTSIDE
                              'SAMPLE-STRING NIL
                              (TV:SHEET-CURSOR-X LABEL-WINDOW) (TV:SHEET-CURSOR-Y LABEL-WINDOW)
                              (TV:SHEET-INSIDE-RIGHT LABEL-WINDOW)
                              (+ (TV:SHEET-CURSOR-Y LABEL-WINDOW) SECOND-LINE-HEIGHT))
           (WHEN SAMPLE-STRING
             (AND (BOUNDP CURRENT-FONT)
                  CURRENT-FONT
                  (SYMBOL-VALUE CURRENT-FONT)
                  (TV:SHEET-SET-FONT LABEL-WINDOW (SYMBOL-VALUE CURRENT-FONT)))
             (DOTIMES (I (STRING-LENGTH SAMPLE-STRING))
               (FED-TYO LABEL-WINDOW (AREF SAMPLE-STRING I) CURRENT-FONT))))))
  (TV:SHEET-SET-FONT LABEL-WINDOW
                     (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN LABEL-WINDOW))))


;;;
;;; This is the main command loop.
;;;
;;; The command table is set up here if it hasn't been
;;;   previously. Commands are read from the mouse or
;;;   the keyboard. Output to the prompt line is kept
;;;   track of here.
;;;

(DEFVAR NUMERIC-ARG)                                    ; Numeric argument to a command.
(DEFVAR NUMERIC-ARG-P)                                  ; Flag numeric argument tracking.
(DEFVAR COMMAND-CHAR)                                   ; Character that invoked this command.

(DEFMETHOD (FED :COMMAND-LOOP) (&AUX (*TERMINAL-IO* TV:TYPEOUT-WINDOW))

  (OR (VARIABLE-BOUNDP COMMAND-TABLE) (SETUP-COMMAND-TABLE))    ; Make command table if we haven't.
  (PROCESS-WAIT "Expose" #'CAR (LOCF (TV:SHEET-EXPOSED-P TV:SUPERIOR)))  ; Wait for this sheet to
                                                        ; be exposed before output.
  (PROG ((PROMPT-LINE-USED NIL)
         (PROMPT-LINE-WAS-USED T)
          COMMAND-CHAR
          COMMAND
          NUMERIC-ARG
          NUMERIC-ARG-P)
       (SEND PROMPT-WINDOW :CLEAR-WINDOW)               ;So it is cleared after an Abort.

   TOP
       (SEND SELF :REDISPLAY)

       (SETQ NUMERIC-ARG 1 NUMERIC-ARG-P NIL)           ; Give default numeric agument.

   ARG
       (SETQ COMMAND-CHAR (SEND SELF :ANY-TYI))         ; Get input from mouse or keyboard.

       ;; Handle selection of mouse-sensitive items in typeout, mouse clicks,
       ;; and signals that it is time to redisplay.
       (COND ((CONSP COMMAND-CHAR)
              (CASE (CAR COMMAND-CHAR)
                (:TYPEOUT-EXECUTE
                 ;; Use the "action" as operation and item as arg.
                 (LEXPR-SEND SELF (CADR COMMAND-CHAR) (CDDR COMMAND-CHAR)))
                (:MENU
                 (FUNCALL (THIRD (SECOND COMMAND-CHAR))))
                (:MOUSE-BUTTON
                 (FUNCALL (CASE (CADR COMMAND-CHAR)
                            (#/MOUSE-1-1 'COM-MOUSE-DRAW)
                            (#/MOUSE-2-1 'COM-MOUSE-CHANGE-DRAW-MODE)
                            (#/MOUSE-3-1 'COM-MOUSE-MOVE-CHAR-BOX)
                            (OTHERWISE (BARF) (GO TOP)))
                          (FOURTH COMMAND-CHAR) ;Args are xpos and ypos at time
                          (FIFTH COMMAND-CHAR)))        ; of click, relative to window.
                (T
                 (SEND SELF :REDISPLAY)
                 (GO ARG))))
             (T
              ;; Ordinary characters that really are characters.
              (SETQ COMMAND (COMMAND-LOOKUP COMMAND-CHAR))      ; Lookup this character in the table.
              (COND ((EQ COMMAND 'COM-NUMBER)   ; Handle numeric prefixes.
                     (SETQ NUMERIC-ARG (+ (IF NUMERIC-ARG-P (* NUMERIC-ARG 10.) 0)
                                          (- COMMAND-CHAR #/0))
                           NUMERIC-ARG-P T)
                     (GO ARG))                  ; Keep looking for the command.

                    (COMMAND                    ; If what we get from the table is
                     (FUNCALL COMMAND))         ;  non-NIL call it as a function,
                    (T                          ; else, complain.
                     (BARF "~:C is not a defined command." COMMAND-CHAR)))))

       (COND ((SEND TV:TYPEOUT-WINDOW :INCOMPLETE-P)
              (LET ((NEXTCH (SEND TV:TYPEOUT-WINDOW :ANY-TYI)))
                (SEND TV:TYPEOUT-WINDOW :MAKE-COMPLETE)
                (OR (EQ NEXTCH #/SP) (SEND SELF :UNTYI NEXTCH)))))

       (AND PROMPT-LINE-WAS-USED (NOT PROMPT-LINE-USED) ; If the prompt line has old news,
            (SEND PROMPT-WINDOW :CLEAR-WINDOW))         ;  clear it.
       (SETQ PROMPT-LINE-WAS-USED PROMPT-LINE-USED      ; Update the prompt-line status.
             PROMPT-LINE-USED NIL)


       (GO TOP)))                                       ; End of Command Loop


;;; Utility Functions, and Functions to Implement Commands


(DEFVAR PROMPT-LINE-USED)                               ; Non-NIL when the prompt-window was
                                                        ; typed on last command.
(DEFUN PROMPT-LINE (STRING &REST FORMAT-ARGS)
  "Pass STRING and ARGS to FORMAT, outputting to the prompt window in the FED frame."
  (DECLARE (:SELF-FLAVOR FED))
  (SEND PROMPT-WINDOW :CLEAR-WINDOW)
  (APPLY #'FORMAT PROMPT-WINDOW STRING FORMAT-ARGS)
  (SETQ PROMPT-LINE-USED T))

(DEFUN PROMPT-LINE-READLINE (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Do READLINE prompting and echoing in the prompt window of the FED frame.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
We return what READLINE returns."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (READLINE PROMPT-WINDOW)))

(DEFUN PROMPT-LINE-DEFAULTED-READLINE (DEFAULT PARSER-FUNCTION STRING &REST FORMAT-ARGS)
  "Do READLINE, echoing in the prompt window, parse result with PARSER-FUNCTION or default.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
An empty line read means use the default; we return DEFAULT.
Otherwise we pass the line contents to PARSER-FUNCTION and return
what it returns."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (LET ((INSTRING (STRING-TRIM " " (READLINE PROMPT-WINDOW))))
      (IF (ZEROP (STRING-LENGTH INSTRING))
          DEFAULT
        (FUNCALL PARSER-FUNCTION INSTRING)))))

(DEFUN PROMPT-LINE-READ (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Call READ, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
We return what READ returns."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (READ PROMPT-WINDOW)))

(DEFUN PROMPT-LINE-Y-OR-N-P (&OPTIONAL STRING &REST FORMAT-ARGS)
  "As the user for Y or N confirmation, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ PROMPT-LINE-USED T)
    (LET ((QUERY-IO PROMPT-WINDOW))
      (Y-OR-N-P NIL))))

(DEFUN PROMPT-LINE-TYI (&OPTIONAL STRING &REST FORMAT-ARGS &AUX CH)
  "Read one character, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt."
  (DECLARE (:SELF-FLAVOR FED))
  (TV:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (SETQ CH (SEND PROMPT-WINDOW :TYI))
    (FORMAT PROMPT-WINDOW "~:C" CH)
    (SETQ PROMPT-LINE-USED T)
    CH))

(DEFUN FED-Y-OR-N-P (&REST ARGS)
  "Ask for confirmation with either a menu or keyboard, whichever user is using.
If the current command was invoked from the keyboard, we use the keyboard;
if the current command was invoked with the mouse, we use a mouse menu."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NUMBERP COMMAND-CHAR)
      (APPLY #'PROMPT-LINE-Y-OR-N-P ARGS)
    (TV:MOUSE-Y-OR-N-P (APPLY #'FORMAT NIL ARGS))))


(DEFUN FED-CHOOSE (ALIST MESSAGE)
  "Choose an alternative from ALIST, prompting with MESSAGE, using keyboard or mouse.
ALIST is like the first argument to MENU-CHOOSE.
If the current command was invoked with the mouse, we use a mouse menu.
If the current command was invoked from the keyboard, we use the keyboard,
reading a line and matching it against the cars of the alist elements.

Extra feature: if an element of ALIST has a null cdr,
and that element is selected, the value is the string in the car of the element,
rather than NIL."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NUMBERP COMMAND-CHAR)
      (LET (INPUT)
        (PROMPT-LINE "~A (one of " MESSAGE)
        (DOLIST (A ALIST)
          (OR (EQ A (CAR ALIST))
              (PRINC ", " PROMPT-WINDOW))
          (PRINC (CAR A) PROMPT-WINDOW))
        (PRINC "): " PROMPT-WINDOW)
        (SETQ INPUT (PROMPT-LINE-READLINE))
        (LET ((MATCH (ASS 'EQUALP INPUT ALIST)))
          (OR MATCH
              ;; No exact match; is there a unique completion?
              (LET (MATCHES)
                (DOLIST (ELT ALIST)
                  (IF (STRING-EQUAL (CAR ELT) INPUT :START1 0 :START2 0 :END1 (STRING-LENGTH INPUT))
                      (PUSH ELT MATCHES)))
                (IF (= (LENGTH MATCHES) 1)
                    (SETQ MATCH (CAR MATCHES)))))
          (IF MATCH
              (OR (GET MATCH ':VALUE) (CAR MATCH))
            (BARF "~A isn't an allowed alternative." INPUT))))
    (MULTIPLE-VALUE-BIND (NIL ALIST-ELT)
        (TV:MENU-CHOOSE ALIST MESSAGE)
      (OR (GET ALIST-ELT ':VALUE) (CAR ALIST-ELT)))))

(DEFUN BARF (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Beep and print error message made with FORMAT.
If STRING is NIL, we just beep."
  (TV:BEEP)
  (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS)))

(DEFVAR COMMAND-TABLE)
(DEFVAR MOUSE-COMMAND-TABLE)

(MAKUNBOUND 'COMMAND-TABLE)                             ; Always regenerate the COMMAND-TABLE.

(DEFUN COMMAND-LOOKUP (CHAR)
  "Return the function that defines CHAR as a FED command, or NIL."
  (DECLARE (:SELF-FLAVOR FED))
  (COND ((TV:CHAR-MOUSE-P CHAR)
         (AREF MOUSE-COMMAND-TABLE (LDB %%KBD-MOUSE-BUTTON CHAR)
               (LDB %%KBD-MOUSE-N-CLICKS CHAR)))
        (T
         (SETQ CHAR (CHAR-CODE CHAR))
         (DO () (NIL) (OR (NUMBERP (SETQ CHAR (AREF COMMAND-TABLE CHAR))) (RETURN CHAR))))))

(DEFUN SETUP-COMMAND-TABLE ()
  (SETQ COMMAND-TABLE (MAKE-ARRAY #o300)
        MOUSE-COMMAND-TABLE (MAKE-ARRAY '(3 3)))
  (TV:DOPLIST (COMMAND-LIST COMMAND CHAR)
    (COND ((CONSP CHAR)
           (DOLIST (CHAR CHAR) (SETF (AREF COMMAND-TABLE CHAR) COMMAND)))
          ((TV:CHAR-MOUSE-P CHAR)
           (SETF (AREF MOUSE-COMMAND-TABLE (LDB %%KBD-MOUSE-BUTTON CHAR)
                       (LDB %%KBD-MOUSE-N-CLICKS CHAR))
                 COMMAND))
          (T
           (SETF (AREF COMMAND-TABLE CHAR) COMMAND))))
  (DO ((CHAR #/0 (1+ CHAR)))
      ((> CHAR #/9))
    (SETF (AREF COMMAND-TABLE CHAR) 'COM-NUMBER))
  (DO ((CHAR #/a (1+ CHAR)))
      ((> CHAR #/z))
    (SETF (AREF COMMAND-TABLE CHAR) (- CHAR #o40))))


;;;
;;; Funtions to carry out commands issued by the command loop.
;;;

(DEFUN COM-HELP (&AUX TYPE)
  (SETQ TYPE (FED-CHOOSE '(("Intro") ("General") ("Menu") ("Keyboard"))
                         "Choose topic."))
  (COND ((STRING-EQUAL TYPE "Intro")
         (COM-HELP-INTRO))
        ((STRING-EQUAL TYPE "General")
         (COM-HELP-GENERAL))
        ((STRING-EQUAL TYPE "Menu")
         (COM-HELP-MENU))
        ((STRING-EQUAL TYPE "Keyboard")
         (COM-HELP-KEYBOARD))))

(DEFUN COM-HELP-INTRO ()
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "FED introductory help"
                               :SUPERIOR (TV:SHEET-GET-SCREEN SELF))
    (SEND WINDOW :STRING-OUT "
To edit with FED, you must first select a font, then select a character.

Select a font by clicking left on Font, then clicking on the font name.
Or type F and then type the font name.  You can create a font this way.

When you select a font that already exists, you get a display of all the
existing characters in it.  Clicking right on Font repeats this display.
So does typing D.

To select a character, click on the character code or symbol in the label.
If you click on the character code, you must specify the new character by
its character code.  If you click on the character symbol, you type the
character you want to edit.

The measurements in the label can be changed by clicking on them.  Total
Ht and Above Base affect all characters in the font and are usually
changed only before any characters are edited.  Width affects only the
current character.

When you select a character that is undefined, the width defaults from
the width of the Space character in that font, so if you are creating a
fixed-width font from scratch you will undoubtedly want to edit the
Space character first and set its width.

Having selected a character, edit its pixels by clicking left on them.
Clicking middle on the character display switches draw mode.  The draw
modes are set dots, clear dots, flip dots, and Latch.  The label always
says what draw mode you are in.  Drawing lines and splines are also
affected by the current draw mode.

To make changes to a character permanent, click left on Save or type S.
** If you select another character before doing this, any changes are lost.
To write the font into a file, click right on Files or type W.

Click left on Move Plane to move the drawing in the window.
To move the character box (the five lines displayed with the pixels)
which controls how the pixels will be positioned, move near one of
the lines and hold down the right button while moving the mouse.
Only the position is changed this way.  The size is controlled by the
parameters displayed in the label.  To change them, click on them
where they appear in the label.

To copy one font into another, select the original font and then click
middle on Font.  You then type the new font name.  That font becomes
a copy of the old one.

To copy one character into another, you can select the original character
and then click right on Save.  You then specify which font and character
to save into.  Alternatively, you can select the font and character you
want to change, and get the existing character into the gray plane
and merge it in from there.  See the General and Menu documentation.
")))

(DEFUN COM-HELP-KEYBOARD ()
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "Help for FED keyboard commands"
                               :SUPERIOR (TV:SHEET-GET-SCREEN SELF))
    (SEND WINDOW :STRING-OUT "

  A few convenient keyboard commands (for keyboard enthusiasts):
    C -- visit character; precede by a character code or follow by a character
    D -- display the entire font as presently configured
        type a space to continue, or mouse a character to edit
    E -- clear the display (erase)
    F -- specify a font
    H -- move the box to the center of the display
    S -- save the visited character in the present font
    V -- specify a new sample-string for display
    R -- read in a new font file
    W -- write out the font file
    X -- exchange grey and black planes
     -- reflect the character
    ,,, -- move the display by numeric argument (default is 8 boxes)
    ?,<HELP> -- choose from a list of available help topics
    <CLEAR-SCREEN> -- refresh display

  You can also use keyboard commands to set dots at precise coordinates.
  This is done by positioning the /"non-mouse/" cursor.
  When you start using the non-mouse cursor, the mouse blinker starts blinking,
  moves to that cursor, and follows it until you move the actual mouse again.

    Hand-L,U,R,D -- move a non-mouse cursor
    X,Y -- set X or Y coordinate of non-mouse cursor (with numeric argument)
    Period -- flip the dot under the non-mouse cursor.
")))

(DEFUN COM-HELP-MENU ()
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "Help for FED menu commands"
                               :SUPERIOR (TV:SHEET-GET-SCREEN SELF))
    (FORMAT WINDOW "

 Normally the commands in the menu appear as follows:

Character  Font     Save Char      Home      Erase Plane    Move Plane   Exchange  Merge
 Rotate   Reflect  Stretch Char  Rectangle  Draw (Sp)Line  Display Scale  Files    Help

When fed is instantiated with a medium resolution color subsystem present,
the following extended version of the menu is displayed:

Character  Font  Save Char     Home     Erase Plane   Move Plane      Exchange
  Merge   Rotate  Reflect  Stretch Char  Rectangle   Draw (Sp)Line  Display Scale
  Files    Help   Camera       Crop        Snap     Med. Res. File

Here are brief explanations for each command:

Character:    Mouse-Left to visit and edit a character from the visited font.
                Type in the desired character then without hitting Return.
              Mouse-Middle to visit the character having the character code
                typed in from the keyboard.
              Mouse-Right to read into the gray plane.  Type in the font and the
                character to read in.

Font:         Mouse-Left and a menu of the loaded fonts will appear.  Mouse one
                of these to edit it.
              Mouse-Middle to copy this font.  The old font name remains defined,
                while the new name you specify is defined as an identical font.
                You continue editing the new font.
              Mouse-Right to give a display of the entire visited font as it is
                presently configured.  Mouse one of the characters to edit it.

Save:         Mouse-Left, and the present character will be saved into the visited
                font.  This does NOT store it permanently...Saving a character is
                valid only for the duration of the session.  You must write the
                file out when you wish to make any permanent changes.
              Mouse-Right, and you will be prompted for a font to store a character
                into.  You must then specify which character, and the same rules
                for permanency apply as stated above.

Home:         Mouse this to center the display in the grid window.

Erase Plane:  Mouse-Left to erase the black plane.  You will first be
                asked, however, for confirmation.
              Mouse-Right to erase the gray plane, after giving the
                necessary confirmation.

Move Plane:   Mouse-Left to move both planes.  You first specify the point of
                reference with the mouse, then specify the point to which you wish
                to move the reference point for the shift.
              Mouse-Right to do the same, but only with the gray plane.

Exchange:     Mouse to exchange the gray and black planes.  This makes it
                easy to copy some character from another font without visiting it,
                by just reading it into the gray plane, then exchanging planes,
                then saving the character as usual...

Merge:        Mouse-Left to merge the black and gray planes into a new
                black plane.  (This is the equivalent of the /"set/" operation in
                the menu from the right mouse.)
              Mouse-Right to receive a menu of the merging procedures:

        copy: This makes the black plane have the same configuration as the current
              gray plane has.
         set: This takes the pattern in the gray plane, and makes sure that all the
              corresponding black plane bits are set.
       clear: This does the exact opposite of /"set/".  It erases all the bits in
              the black plane corresponding to those set in the gray plane.
        flip: This takes all the bits set in the gray plane, and flips the bits in
              the black plane that correspond with them.  Thus if one in the black
              plane is set, it will be cleared if it is above one set in the gray
              plane.  And those of such that are clear will be set in the same way.

Rotate:       Mouse this to rotate the character 90 degrees to the right.

Reflect:      Mouse this to reflect the character across the chosen axis, which
                is picked from menu.  The old character position is cleared in
                the process of the reflection.

Stretch Char: Mouse this to stretch or compact the character in either the X or Y
                direction. You will be prompted to type in numerators and
                denominators for the new X and Y scaling, relative to the present
                values.

Rectangle:    Mouse-Left here to erase a rectangular area in the black plane.
                You will then need to specify two corners for the rectangle's
                position on the display of the current character.
              Mouse-Right for a menu of rectangle operations.  These all work like
                erasing one above (for the left button) but you will need to first
                mouse over the operation, set/clear/flip, to perform over the
                rectangle's area.

Draw (Sp)Line:Mouse-Left over this to draw a line segment in the display. You will
                need to mouse left over the two endpoints.  The current drawing mode
                is used except that Latch mode this will just draw and never erase.
              Mouse-Right over this to draw a spline. You can mouse left over any
                number of points (IN ORDER) and then when you Mouse-Right they
                will all be connected in this order. FED will attempt to connect
                these points in some sort of smooth manner.  The current drawing mode
                is used except that in Latch mode this will just draw and never erase.

Display Scale:Mouse this to change the size of the display given for the character.
                The maximum size is (should be) 63, and the minimum size that will
                still display the grid dots is 6.  Any size five or below will give
                a blank grid with the char box in it.  (This is convenient for
                seeing a more-scaled version of what your character will actually
                look like, but also makes it difficult to continue editing the
                character until the size is set back to something where one can
                easily see the grid.

Files:        Mouse-Left to read in a font from a file.  You will be prompted for
                the file type and name.
              Mouse-Right to write the visited font out into a file.  You will be
                prompted for a file type. Then you will type in a file name.  The
                default file name should be the name of the file from which you
                read the font.

Help:         Mouse this for a menu of available documentation on FED.  So far the
                topics covered are:

        General:  Gives a general overview of the editor and what it can do.
           Menu:  Gives brief descriptions of each command in the window's top
                     menu.  (This is the one you are in right now)
       Keyboard:  Gives a list of the corresponding keyboard commands for most of
                     the Mouse-able menu commands, for those who use this without
                     the mouse.

Camera:       If you have a frame grabber, Mouse-Left to load the Medium
                Resolution Color screen from the camera  (Hold button down for
                flow-through mode.)
              Mouse-Right to put the frame grabber in flow-through mode.  (Exit
                flow-through mode by pressing the space bar.)

Crop:         Mouse this to enter cropping mode on the Medium Resolution Color
                screen.  Exit cropping mode by clicking Right.

Snap:         Mouse-Left to copy the cropped region on the Medium Resolution Color
                screen into the Black Plane in the character box.  The ``clipping''
                threshold is determined by the slider at the right of the screen.
              Mouse-Right to snap into the gray plane.

Med. Res. File: Mouse-Left to load an image qfasl file into the Medium Resolution
                  Color screen.
                Mouse-Right to save the contents of the Medium Resolution Color
                  screen as a qfasl file.

")))

(DEFUN COM-HELP-GENERAL ()
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "General help for FED"
                               :SUPERIOR (TV:SHEET-GET-SCREEN SELF))
    (FORMAT WINDOW "

  The FED frame contains, from top to bottom: a command menu, an editing area,
  the label (where the font and character you are editing, and their parameters,
  are displayed), and /"register/" panes where you can store patterns from
  the editing area, and retrieve them for later use.

  The editing area displays the character you are editing.  A rectangle
  called the /"character box/" shows the defined outline of the character;
  this controls where it will line up when printed, both vertically and
  on both sides horizontally.

  Use the mouse on the editing area to make simple changes on the picture.  Use
  the left mouse button to draw dots in the matrix, by clicking over each dot
  or by dragging the mouse over dots with the left button down.  Click middle to
  change the mode of how the left button draws, from setting dots, to clearing
  dots, to flipping them, to laying down the flip of the color first clicked on
  during a stroke.  (Watch out for the little dots that make up the grid!)

  To move the character box across the dots, put the mouse over an edge and
  move it while holding down the right button.

  Most other commands can be found in the menu; refer to the MENU help topic.

  All the items displayed in the label can be changed by positioning the mouse
  there and clicking on the item to be changed.  You can select a new font
  or character, set the font parameters or character width, or specify a
  sample string.

  The gray plane: FED can display two characters at once, from the same font
  or different fonts.  The one you are actually editing is displayed in black.
  The other character is displayed in gray.  We call them the black plane and
  the gray plane.  Normal editing operations use only the black plane; you
  can use the gray plane for reference as you edit, or you can merge the gray
  plane into the black plane with the MERGE menu operation.

  You can edit the contents of the gray plane by doing EXCHANGE
  which brings those contents into the black plane.  After editing them,
  do EXCHANGE again.  However, only the black plane can be stored.

  Many menu operations can operate on either plane, or sometimes
  on both, depending on the mouse button you use.

  The registers: each register can hold a configuration of dots, which
  you can use later.  Click left on the area of a register to load it
  from the contents of the editing area.  Click right on the area of a
  register to get a menu of other operations, including merging the
  contents of the register back into the black plane or gray plane.
")))

(DEFUN COM-SET-SAMPLE ()
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ SAMPLE-STRING (PROMPT-LINE-READLINE "String to display in ~A: " CURRENT-FONT))
  (AND (ZEROP (STRING-LENGTH SAMPLE-STRING)) (SETQ SAMPLE-STRING NIL))
  (SEND SELF :MUST-REDISPLAY-LABEL))

(DEFMETHOD (FED :PROMPT-LINE-SET-SAMPLE) (&REST IGNORE)
  (COM-SET-SAMPLE))

(DEFUN COM-MOUSE-CHANGE-DRAW-MODE (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ DRAW-MODE (SELECTQ DRAW-MODE (2 6) (6 0) (0 7) (7 2) (OTHERWISE 6)))
  (SEND SELF :MUST-REDISPLAY-LABEL)
  (PROMPT-LINE "Drawing mode is ~A"
               (STRING-UPCASE (SEND SELF :DRAW-MODE-STRING))))

;;; Complement the square which the mouse is on.

(DEFUN COM-COMPLEMENT-SQUARE ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NOT CURSOR-ON)
      (BARF)
    (SEND SELF :DRAW-POINT CURSOR-X CURSOR-Y
                           (LOGXOR 1 (AREF WINDOW-ARRAY CURSOR-X CURSOR-Y)))))

(DEFUN COM-ERASE-ALL ()
  (AND (FED-Y-OR-N-P "Erase the black plane? ")
       (SEND SELF :ERASE-ALL)))

(DEFUN COM-ERASE-GRAY ()
  (AND (FED-Y-OR-N-P "Erase the gray plane? ")
       (SEND SELF :ERASE-GRAY)))

(DEFUN COM-EXCHANGE-PLANES ()
  (DECLARE (:SELF-FLAVOR FED))
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS)
  (PSETQ GRAY-PLANE PLANE
         PLANE (OR GRAY-PLANE
                   (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.)))
  (SETQ UNSAVED-CHANGES T)
  (SEND SELF :MOVE-CHAR-BOX GRAY-X-OFFSET GRAY-Y-OFFSET)
  (SETQ GRAY-X-OFFSET (- GRAY-X-OFFSET)
        GRAY-Y-OFFSET (- GRAY-Y-OFFSET))
  (DECF WINDOW-X-POS GRAY-X-OFFSET)
  (DECF WINDOW-Y-POS GRAY-Y-OFFSET))

(DEFUN COM-HOME ()
  (SEND SELF :HOME-BOX))

;;; Set the position of the cursor, which is used as an alternate to the mouse
;;;  for complementing squares.  Also say that the cursor ought to be displayed.

(DEFUN COM-SET-X (&OPTIONAL (XPOS NUMERIC-ARG))
  (DECLARE (:SELF-FLAVOR FED))
  (COND ((OR (< XPOS 0) ( XPOS WINDOW-X-SIZE))
         (BARF "X out of range: ~D" XPOS)))
  (SETQ CURSOR-X (MAX 0 (MIN (1- WINDOW-X-SIZE) XPOS)))
  (SETQ CURSOR-ON T))

(DEFUN COM-SET-Y (&OPTIONAL (YPOS NUMERIC-ARG))
  (DECLARE (:SELF-FLAVOR FED))
  (COND ((OR (< YPOS 0) ( YPOS WINDOW-Y-SIZE))
         (BARF "Y out of range: ~D" YPOS)))
  (SETQ CURSOR-Y (MAX 0 (MIN (1- WINDOW-Y-SIZE) YPOS)))
  (SETQ CURSOR-ON T))

(DEFUN COM-SHIFT-CURSOR (&AUX (DISTANCE NUMERIC-ARG) DX DY ARROW)
  (DECLARE (:SELF-FLAVOR FED))
  (OR NUMERIC-ARG-P (SETQ DISTANCE (LSH 1 (LDB %%KBD-CONTROL-META COMMAND-CHAR))))
  (SETQ ARROW (LDB %%KBD-CHAR COMMAND-CHAR))
  (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/HAND-LEFT -1) (#/HAND-RIGHT 1)))) 0)))
  (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((#/HAND-UP -1) (#/HAND-DOWN 1)))) 0)))
  (COM-SET-X (+ CURSOR-X DX))
  (COM-SET-Y (+ CURSOR-Y DY)))

(DEFUN COM-SHIFT-WINDOW (&AUX DISTANCE DX DY ARROW)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ DISTANCE (IF NUMERIC-ARG-P NUMERIC-ARG 10))
  (SETQ ARROW (CHAR-CODE COMMAND-CHAR))
  (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/ -1) (#/ 1)))) 0)))
  (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((#/ -1) (#/ 1)))) 0)))
  (SEND SELF (IF (NOT (ZEROP (CHAR-BITS COMMAND-CHAR)))
                 :MOVE-GRAY-PLANE
                 :MOVE-PLANE)
             DX DY))

(DEFUN COM-MOUSE-SHIFT-WINDOW ()
  (MOUSE-MOVE-OPERATION :MOVE-PLANE
                        "Move both planes. Mark reference point with LEFT button."
                        "Select another point to move the first one to."))

(DEFUN COM-MOUSE-SHIFT-GRAY ()
  (MOUSE-MOVE-OPERATION :MOVE-GRAY-PLANE
                        "Move gray plane. Mark reference point with LEFT button."
                        "Select another point to move the first one to."))

(DEFUN MOUSE-MOVE-OPERATION (WINDOW-OP STRING1 STRING2 &AUX OX OY X Y)
  "Get two points using the mouse, then perform WINDOW-OP on the delta between the points.
WINDOW-OP is sent to SELF with two args, the delta X and the delta Y.
STRING1 is printed in the prompt area while waiting for the first point
and STRING2 is printed while waiting for the second."
  (DECLARE (:SELF-FLAVOR FED))
  (BLOCK NIL
    (UNWIND-PROTECT
        (PROGN
          (PROMPT-LINE STRING1)
          (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
                "Left: Specify reference point.  Middle//Right: Abort")
          (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
          (OR OX (RETURN NIL))
          (SEND SELF :GRAY-POINT OX OY)
          (PROMPT-LINE STRING2)
          (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
                "Left: Specify point to move reference to.  Middle//Right: Abort.")
          (UNWIND-PROTECT
              (MULTIPLE-VALUE-SETQ (X Y) (SEND SELF :MOUSE-SELECT-POINT))
            (SEND SELF :GRAY-POINT OX OY)))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
      (SEND PROMPT-WINDOW :CLEAR-WINDOW))
    (OR X (RETURN NIL))
    (SEND SELF WINDOW-OP (- X OX) (- Y OY))))

;;; Set the box-size (in both X and Y) of the fed-window to SCALE.
;;;  We try to keep the center of the window in the center.

(DEFUN COM-SCALE ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET* ((SCALE (IF NUMERIC-ARG-P
                    NUMERIC-ARG
                  (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.))
                    (PROMPT-LINE-DEFAULTED-READLINE
                      BOX-X-SIZE 'READ-FROM-STRING
                      "New box size (currently ~D) " BOX-X-SIZE)))))
    (COND ((AND (FIXP SCALE)
                (> SCALE 0)
                (< SCALE (TRUNCATE (TV:SHEET-INSIDE-WIDTH SELF) 2))
                (< SCALE (TRUNCATE (TV:SHEET-INSIDE-HEIGHT SELF) 2)))
           (SETQ BOX-X-SIZE SCALE BOX-Y-SIZE SCALE
                 REDISPLAY-DEGREE REDISPLAY-ALL)
           (deduce-window-array-size)
           (SEND SELF :REDEFINE-MARGINS))
          ((BARF "Bad scale: ~D" SCALE)))))


;;; Selecting fonts.

(DEFMETHOD (FED :SELECT-FONT) (NEW-FONT)
  (IF (EQ NEW-FONT 'FILE-COMPUTER-FONT)
      (COM-LIST-FC-FONTS)
    (IF (EQ NEW-FONT 'NEW-FONT)
        (SETQ NEW-FONT (PROMPT-LINE-DEFAULTED-READLINE NIL 'INTERN-FONT-NAME
                                                       "Font to create: ")))
    (SELECT-FONT NEW-FONT)))

;Comes from clicking on font name in label window.
(DEFMETHOD (FED :PROMPT-LINE-SELECT-FONT) (IGNORE)
  (COM-SELECT-FONT))

(DEFUN COM-SELECT-FONT ()
  (SELECT-FONT (PROMPT-LINE-DEFAULTED-READLINE NIL 'INTERN-FONT-NAME "Font to select: ")))

(DEFUN SELECT-FONT (NEW-FONT &AUX TEM)
  "Select the font named NEW-FONT for editing in SELF, a FED window."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL NEW-FONT)
      (BARF "")
    (COND ((BOUNDP NEW-FONT))                   ; Font already exists.
          ((AND (PROBEF (SETQ TEM (STRING-APPEND "SYS: FONTS; " NEW-FONT " QFASL >")))
                (PROMPT-LINE-Y-OR-N-P "Load ~A? " TEM))
           (LOAD TEM "FONTS")))
    (SETQ CURRENT-CHARACTER NIL CURRENT-FONT NEW-FONT UNSAVED-CHANGES NIL)
    (FONT-GET-FD CURRENT-FONT)
    (UPDATE-FED-EDITED-CHARS)
    (SEND TV:SUPERIOR :REDEFINE-MARGINS)        ;Forces input of '(REDISPLAY)
    (DISPLAY-LABEL)
    (SEND SELF :ERASE-ALL)
    (WHEN (SEND *STANDARD-INPUT* :LISTEN)
      (SEND *STANDARD-INPUT* :ANY-TYI))         ;Discard the '(REDISPLAY)
    (SEND TV:SUPERIOR :SET-CONFIGURATION (SEND TV:SUPERIOR :CONFIGURATION)) ; Force frame constraint redigest.
    (AND (SEND *STANDARD-INPUT* ':LISTEN)
         (SEND *STANDARD-INPUT* ':ANY-TYI))     ;Discard the '(REDISPLAY) (or something).
    (AND (SEND *STANDARD-INPUT* ':LISTEN)
         (SEND *STANDARD-INPUT* ':ANY-TYI))     ;Discard the (?) '(REDISPLAY) Needed only if selected font wasn't already.
    (COM-DISPLAY-FONT)
    (SEND SELF :HOME-BOX)))                     ;Debbie Ellerin's bug fix.

(DEFUN COM-COPY-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No font is selected.")
    (LET ((NEW-FONT (PROMPT-LINE-DEFAULTED-READLINE NIL 'INTERN-FONT-NAME
                                                    "Copy to (new font name): "))
          (OLD-FD (FONT-GET-FD CURRENT-FONT))
          NEW-FD)
      (IF (AND (BOUNDP NEW-FONT)
               (NOT (FED-Y-OR-N-P "Font ~A exists.  Clobber it? " NEW-FONT)))
          (BARF "Aborted."))
      (SETF (SYMBOL-VALUE NEW-FONT) (SI:COPY-OBJECT (SYMBOL-VALUE CURRENT-FONT)))
      (SETF (FONT-NAME (SYMBOL-VALUE NEW-FONT)) NEW-FONT)
      (SETQ CURRENT-FONT NEW-FONT)
      ;; Make the new font's FD a copy of the old one, but don't copy name.
      (SETQ NEW-FD (FONT-GET-FD NEW-FONT))
      (COPY-ARRAY-CONTENTS-AND-LEADER OLD-FD NEW-FD)
      (SETF (FD-NAME NEW-FD) NEW-FONT)
      ;; Replace the CD's with copies, too.
      (DOTIMES (I (ARRAY-LENGTH NEW-FD))
        (IF (AREF NEW-FD I)
            (SETF (AREF NEW-FD I)
                  (SI:COPY-OBJECT (AREF NEW-FD I)))))
      (PUTPROP NEW-FONT NEW-FD 'FONT-DESCRIPTOR)
      (PUTPROP NEW-FONT (SYMBOL-VALUE NEW-FONT) 'FONT-DESCRIBED)
      (UPDATE-FED-EDITED-CHARS)
      (DISPLAY-LABEL))))

(DEFUN INTERN-FONT-NAME (STRING)
  "Given a STRING, return the font-name symbol of that name."
  (INTERN (STRING-UPCASE STRING) "FONTS"))

;;; Setting the font parameters (in response to clicks on the fed-label-window)

(DEFMETHOD (FED :PROMPT-LINE-SET-LINE-HEIGHT) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
         (VALUE (READ-DEFAULTED-FONT-PARAMETER "line-height" (FD-LINE-SPACING FD))))
    (COND ((OR (NOT (FIXNUMP VALUE)) (NOT (PLUSP VALUE)))
           (BARF "~D is not a positive fixnum." VALUE))
          ((< VALUE (FD-BASELINE FD))
           (BARF "~D is smaller than the height above baseline." VALUE))
          (T
           (SETF (FD-LINE-SPACING FD) VALUE)
           (AND (BOUNDP CURRENT-FONT)
                (SYMBOL-VALUE CURRENT-FONT)
                (SETF (FONT-CHAR-HEIGHT (SYMBOL-VALUE CURRENT-FONT)) VALUE))
           (SETQ CHAR-BOX-Y3 (+ CHAR-BOX-Y1 VALUE))
           (SEND TV:SUPERIOR :REDEFINE-MARGINS)
           (SEND TV:SUPERIOR :SET-CONFIGURATION (SEND TV:SUPERIOR :CONFIGURATION)) ; force frame constraint redigest
           (DISPLAY-LABEL)))))

(DEFMETHOD (FED :PROMPT-LINE-SET-BASELINE) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
         (VALUE (READ-DEFAULTED-FONT-PARAMETER "baseline" (FD-BASELINE FD))))
    (COND ((OR (NOT (FIXP VALUE)) (NOT (PLUSP VALUE)))
           (BARF "~D is not a positive fixnum." VALUE))
          ((> VALUE (FD-LINE-SPACING FD))
           (BARF "~D is bigger than the total height." VALUE))
          (T
           (SETF (FD-BASELINE FD) VALUE)
           (AND (BOUNDP CURRENT-FONT)
                (SYMBOL-VALUE CURRENT-FONT)
                (SETF (FONT-BASELINE (SYMBOL-VALUE CURRENT-FONT)) VALUE))
           (SETQ CHAR-BOX-Y2 (+ CHAR-BOX-Y1 VALUE))
           (DISPLAY-LABEL)))))

(DEFMETHOD (FED :PROMPT-LINE-SET-BLINKER-WIDTH) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
         (VALUE (READ-DEFAULTED-FONT-PARAMETER "blinker-width" (FD-BLINKER-WIDTH FD))))
    (IF (OR (NULL VALUE) (NOT (PLUSP VALUE)))
        (BARF "~D is not a positive fixnum." VALUE)
      (SETF (FD-BLINKER-WIDTH FD) VALUE)
      (AND (BOUNDP CURRENT-FONT)
           (SYMBOL-VALUE CURRENT-FONT)
           (SETF (FONT-BLINKER-WIDTH (SYMBOL-VALUE CURRENT-FONT)) VALUE))
      (DISPLAY-LABEL))))

(DEFMETHOD (FED :PROMPT-LINE-SET-BLINKER-HEIGHT) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
         (VALUE (READ-DEFAULTED-FONT-PARAMETER "blinker-height" (FD-BLINKER-HEIGHT FD))))
    (IF (OR (NULL VALUE) (NOT (PLUSP VALUE)))
        (BARF "~D is not a positive fixnum." VALUE)
      (SETF (FD-BLINKER-HEIGHT FD) VALUE)
      (AND (BOUNDP CURRENT-FONT)
           (SYMBOL-VALUE CURRENT-FONT)
           (SETF (FONT-BLINKER-HEIGHT (SYMBOL-VALUE CURRENT-FONT)) VALUE))
      (DISPLAY-LABEL))))

(DEFUN READ-DEFAULTED-FONT-PARAMETER (NAME CURRENT-VALUE)
  "Read a number, prompting for parameter named NAME, defaulting to CURRENT-VALUE."
  (LET* ((*PRINT-BASE* 10.) (*READ-BASE* 10.)
         (VALUE (PROMPT-LINE-DEFAULTED-READLINE
                  CURRENT-VALUE 'READ-FROM-STRING
                  "~&Font ~A (default ~D): "
                  NAME CURRENT-VALUE)))
    (AND (FIXNUMP VALUE) VALUE)))

(DEFMETHOD (FED :PROMPT-LINE-SET-CHAR-WIDTH) (IGNORE)
  (LET* ((*READ-BASE* 10.) (*PRINT-BASE* 10.)
         (VALUE (PROMPT-LINE-DEFAULTED-READLINE
                  (- CHAR-BOX-X2 CHAR-BOX-X1) 'READ-FROM-STRING
                  "~&Character width (default ~D): "
                  (- CHAR-BOX-X2 CHAR-BOX-X1))))
    (IF (OR (NOT (FIXNUMP VALUE)) (NOT (PLUSP VALUE)))
        (BARF "Value must be a positive fixnum")
      (SETQ CHAR-BOX-X2 (+ CHAR-BOX-X1 VALUE))
      (DISPLAY-LABEL))))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:08
(DEFUN QFASL-COM-READ-FILE (FILENAME FONTNAME)
  FONTNAME
  (LOAD FILENAME "FONTS"))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:09
(DEFUN AST-COM-READ-FILE (FILENAME FONTNAME &AUX FD)
  (SETQ FD (READ-AST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  (PUTPROP FONTNAME FILENAME 'AST-FILE)
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:09
(DEFUN KST-COM-READ-FILE (FILENAME FONTNAME &AUX FD)
  (SETQ FD (READ-KST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  (PUTPROP FONTNAME FILENAME 'KST-FILE)
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:10
(DEFVAR COM-READ-FILE-TYPES
        '(("KST" KST-COM-READ-FILE)
          (:QFASL QFASL-COM-READ-FILE)
          ("AC" READ-AC-INTO-FONT)
          ("AL" READ-AL-INTO-FONT)
          ("KS" READ-KS-INTO-FONT)
          ("AST" AST-COM-READ-FILE)))

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:10
(DEFUN COM-READ-FILE (&AUX FILENAME TYPE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ TYPE (FED-CHOOSE (MAPCAR #'(LAMBDA (X) (LIST (CAR X))) COM-READ-FILE-TYPES)
                         "Read which format of font file"))
  (COND ((NULL TYPE))
        ('ELSE
         (SETQ FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Read" TYPE))
         (SETQ CURRENT-FONT (INTERN (SEND FILENAME :NAME) 'FONTS))))
  (FUNCALL (CADR (ASS #'STRING-EQUAL TYPE COM-READ-FILE-TYPES)) FILENAME CURRENT-FONT)
  (SEND SELF :REDEFINE-MARGINS)
  (SELECT-FONT CURRENT-FONT))

(DEFUN COM-WRITE-FILE (&AUX FILENAME TYPE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ TYPE (FED-CHOOSE '(("KST") (:QFASL) ("AC") ("AST"))
                         "Write which format of font file"))
  (IF (NULL TYPE)
      NIL
    (SETQ FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Write" TYPE))
    (COND ((STRING-EQUAL TYPE "KST")
           (WRITE-FONT-INTO-KST CURRENT-FONT FILENAME)
           (PUTPROP CURRENT-FONT FILENAME 'KST-FILE))
          ((STRING-EQUAL TYPE "AST")
           (WRITE-FONT-INTO-AST CURRENT-FONT FILENAME)
           (PUTPROP CURRENT-FONT FILENAME 'AST-FILE))
          ((STRING-EQUAL TYPE "QFASL")
           (COMPILER:FASD-SYMBOL-VALUE FILENAME CURRENT-FONT))
          ((STRING-EQUAL TYPE "AC")
           (WRITE-FONT-INTO-AC FILENAME CURRENT-FONT)))))

(DEFVAR PATHNAME-DEFAULTS)

(DEFUN PATHNAME-DEFAULTS ()
  "Return the pathname defaults for file I//O in FED."
  (COND ((NOT (VARIABLE-BOUNDP PATHNAME-DEFAULTS))
         (SETQ PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))
         (FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) PATHNAME-DEFAULTS)))
  PATHNAME-DEFAULTS)

(DEFUN READ-DEFAULTED-FILENAME (FONT OPERATION TYPE &AUX TEM TEM1 SPEC)
  "Read a filename for doing OPERATION (a string), default type TYPE, default name FONT."
  (SETQ TEM (FS:MAKE-PATHNAME :DEFAULTS (PATHNAME-DEFAULTS)
                              :NAME (STRING FONT)
                              :TYPE TYPE))
  (IF (AND (STRING-EQUAL TYPE "KST") (SETQ TEM1 (GET FONT 'KST-FILE)))
      (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS TEM1 TEM))
      (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS TEM PATHNAME-DEFAULTS)))
  (SETQ SPEC (PROMPT-LINE-READLINE "~A ~A file: (default ~A) " OPERATION TYPE TEM))
  (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS SPEC TEM TYPE))
  (FS:SET-DEFAULT-PATHNAME TEM PATHNAME-DEFAULTS)
  TEM)


;;; Specify a character.

;Comes from clicking on current character in fed-label-window.
(DEFMETHOD (FED :PROMPT-LINE-SELECT-CHAR) (IGNORE)
  (COM-SPECIFY-CHARACTER))

;Comes from clicking on current character code in fed-label-window.
(DEFMETHOD (FED :PROMPT-LINE-SELECT-CHAR-CODE) (IGNORE)
  (COM-SPECIFY-CHARACTER-BY-NUMBER))

(DEFUN COM-SPECIFY-CHARACTER (&AUX CH (CC CURRENT-CHARACTER))
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font.")
    (COND ((AND NUMERIC-ARG-P ( 0 NUMERIC-ARG 400))
           (SETQ CH 0)
           (SETQ CURRENT-CHARACTER NUMERIC-ARG))
          (T (SETQ CH (PROMPT-LINE-TYI "Character: "))
             (SETQ CURRENT-CHARACTER CH)))
    (COND ((> CH 400)
           (SETQ CURRENT-CHARACTER CC)
           (BARF "Aborted."))
          ((AND UNSAVED-CHANGES
                (NOT (FED-Y-OR-N-P
                       "This will discard the editing you have done.  Proceed? ")))
           (SETQ CURRENT-CHARACTER CC))
          (T
           (LET ((VAL (GOBBLE-CHARACTER CURRENT-FONT CURRENT-CHARACTER T)))
             (IF (NULL VAL) (SEND SELF :ERASE-ALL)
               (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS)
               (SETQ PLANE VAL)
               (SETQ UNSAVED-CHANGES NIL)
               (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE))
             (SEND SELF :HOME-BOX)
             (UPDATE-FED-EDITED-CHARS))))))

(DEFUN COM-SPECIFY-CHARACTER-BY-NUMBER ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font.")
    (LET* ((*read-base* 8.)
           (NUMERIC-ARG-P T)
           (NUMERIC-ARG (PROMPT-LINE-READ "Character code number: ")))
      (COND ((NOT (FIXP NUMERIC-ARG)) (BARF "~S is not a number." NUMERIC-ARG))
            ((NOT ( 0 NUMERIC-ARG 400)) (BARF "~o is out of range." NUMERIC-ARG))
            (T (COM-SPECIFY-CHARACTER))))))

(DEFUN COM-READ-GRAY-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((FONT (PROMPT-LINE-DEFAULTED-READLINE
                CURRENT-FONT 'INTERN-FONT-NAME
                "Font of char to put in gray plane (default ~A): "
                CURRENT-FONT)))
    (IF (NOT (AND FONT (BOUNDP FONT)))
        (BARF "Font ~A does not exist." FONT)
      (LET ((VAL (GOBBLE-CHARACTER FONT (PROMPT-LINE-TYI "Character: "))))
        (SEND SELF :MOVE-GRAY-PLANE 0 0 T)
        (IF (NULL VAL) (SEND SELF :ERASE-GRAY)
          (SETQ GRAY-PLANE VAL)
          (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE GRAY-PLANE))))))

;;; Copy the data from character CHAR in font FONT
;;;  into a plane, and return it (or NIL, if the character doesn't exist).

(DEFUN GOBBLE-CHARACTER (FONT CHAR &OPTIONAL SET-CHAR-BOX
                         &AUX FD CD -PLANE- OTHER-PLANE-MAYBE)
  "Copy the definition of character CHAR in FONT into a plane, and return the plane.
SET-CHAR-BOX says set SELF's character box for that character as well."
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ OTHER-PLANE-MAYBE (GET-CHAR-EDITING-PLANE FONT CHAR))
  (IF OTHER-PLANE-MAYBE
      (PROGN
        (SETQ -PLANE- (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
        (MERGE-OTHER-PLANE OTHER-PLANE-MAYBE 0 0)
        -PLANE-)
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FONT-GET-FD FONT))

    ;; Get the character descriptor for the desired character out of the FD.
    (IF (NOT (AND (< CHAR (ARRAY-LENGTH FD))
                  (SETQ CD (AREF FD CHAR))))
        NIL
      (SETQ -PLANE- (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
      (SETF (PLANE-ORIGIN -PLANE-) (LIST (- (CD-CHAR-LEFT-KERN CD)) 0))

      (COND (SET-CHAR-BOX
             ;; Put sides of character frame at right place,
             ;; according to char width and left kern.
             (SETQ CHAR-BOX-X1 0
                   CHAR-BOX-X2 (FIXR (CD-CHAR-WIDTH CD)))

             ;; Put top of character at top of font line, and bottom at baseline
             ;;   so that descenders go below the "bottom".
             (SETQ CHAR-BOX-Y1 0
                   CHAR-BOX-Y2 (FD-BASELINE FD)
                   CHAR-BOX-Y3 (FD-LINE-SPACING FD))))

      ;; Now XWIDTH and YWIDTH get the size of the character's raster,
      ;;   and copy the data into the plane in CHARACTER-ARRAY.

      (LET ((XWIDTH (SECOND (ARRAY-DIMENSIONS CD)))
            (YWIDTH (FIRST (ARRAY-DIMENSIONS CD)))
            (XORG (CAR (PLANE-ORIGIN -PLANE-))))
        (DO ((I 0 (1+ I)))
            ((= I XWIDTH))
          (DO ((J 0 (1+ J)))
              ((= J YWIDTH))
            (SETF (PLANE-AREF -PLANE- (+ I XORG) J) (AREF CD J I))))))
    -PLANE-))

;;; Simple merge-gray click.  Uses IOR.
(DEFUN COM-MERGE-GRAY ()
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ UNSAVED-CHANGES T)
  (MERGE-OTHER-PLANE GRAY-PLANE GRAY-X-OFFSET GRAY-Y-OFFSET))

;;; Merge operation menu
(DEFCONST MERGE-GRAY-MENU
          '(("Copy" :VALUE :COPY)
            ("Set bits" :VALUE :SET)
            ("Clear bits" :VALUE :CLEAR)
            ("Flip bits" :VALUE :FLIP)))

;Right button on MERGE.  Uses menu to choose merge mode.
(DEFUN COM-MERGE-GRAY-MENU ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((SUBOP (FED-CHOOSE MERGE-GRAY-MENU "Merge gray into black")))
    (IF SUBOP
        (SEND SELF :MERGE-CONTENTS SUBOP GRAY-PLANE
                   (+ CHAR-BOX-X1 GRAY-X-OFFSET) (+ CHAR-BOX-Y1 GRAY-Y-OFFSET)
                   (+ CHAR-BOX-X2 GRAY-X-OFFSET) (+ CHAR-BOX-Y2 GRAY-Y-OFFSET)
                   (+ CHAR-BOX-Y3 GRAY-Y-OFFSET)))))

;;; M => merge in the data from the gray plane, doing IOR,
;;; Control-M => ask for merge mode.
(DEFUN COM-MERGE-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (COND ((LDB-TEST %%KBD-CONTROL-META COMMAND-CHAR)
         (COM-MERGE-GRAY-MENU))
        (T (COM-MERGE-GRAY))))

(DEFUN MERGE-OTHER-PLANE (OTHER-PLANE OTHER-X-OFFSET OTHER-Y-OFFSET
                          &OPTIONAL (OP :SET) (TO-PLANE PLANE))
  "Merge the contents of OTHER-PLANE into that of TO-PLANE.
OP is SET, CLEAR or FLIP.  The two offset args are the coordinates
of the point in OTHER-PLANE that corresponds to 0, 0 in TO-PLANE."
  (DECLARE (:SELF-FLAVOR BASIC-FED))
  (LET* ((OTHER-ORIGIN (PLANE-ORIGIN OTHER-PLANE))
         (OTHER-END (PLANE-END OTHER-PLANE)))
    (COND ((NEQ OP ':CLEAR)
           (PLANE-ENSURE-EXISTS TO-PLANE
                                (- (CAR OTHER-ORIGIN) OTHER-X-OFFSET)
                                (- (CADR OTHER-ORIGIN) OTHER-Y-OFFSET))
           (PLANE-ENSURE-EXISTS TO-PLANE
                                (- (CAR OTHER-END) OTHER-X-OFFSET 1)
                                (- (CADR OTHER-END) OTHER-Y-OFFSET 1))))
    (LET* ((NORM-ORIGIN (PLANE-ORIGIN TO-PLANE))
           (BOOLE-OP
             (SELECTQ OP
               (:SET TV:ALU-IOR)
               (:CLEAR TV:ALU-ANDCA)
               (:FLIP TV:ALU-XOR)
               (OTHERWISE (FERROR NIL "Invalid OP - ~S" OP))))
           (X-OFFSET (+ (CAR OTHER-ORIGIN) (- OTHER-X-OFFSET) (- (CAR NORM-ORIGIN))))
           (Y-OFFSET (+ (CADR OTHER-ORIGIN) (- OTHER-Y-OFFSET) (- (CADR NORM-ORIGIN))))
           (X-BEG (MAX 0 (- X-OFFSET)))
           (Y-BEG (MAX 0 (- Y-OFFSET)))
           (X-END (MIN (CAR (ARRAY-DIMENSIONS OTHER-PLANE))
                       (- (CAR (ARRAY-DIMENSIONS TO-PLANE)) X-OFFSET)))
           (Y-END (MIN (CADR (ARRAY-DIMENSIONS OTHER-PLANE))
                       (- (CADR (ARRAY-DIMENSIONS TO-PLANE)) Y-OFFSET))))
      (DO ((I X-BEG (1+ I))) ((= I X-END))
        (DO ((J Y-BEG (1+ J))) ((= J Y-END))
          (SETF (AREF TO-PLANE (+ I X-OFFSET) (+ J Y-OFFSET))
                (BOOLE BOOLE-OP
                       (AREF OTHER-PLANE I J)
                       (AREF TO-PLANE (+ I X-OFFSET) (+ J Y-OFFSET))))))
      (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE TO-PLANE))))

;;; Operations on registers.
(DEFCONST FED-REGISTER-OP-MENU-ALIST
          '(("Clear register" :VALUE (:CLEAR-REG))
            ("Save black" :VALUE (:LOAD-REG))
            ("Save gray" :VALUE (:LOAD-REG-GRAY))
            ("Restore to black" :VALUE (:LOAD-BLACK))
            ("" :NO-SELECT NIL)
            ("Merge Operations" :NO-SELECT NIL
             :FONT :MENU-STANDOUT)
            ("Copy to black" :VALUE (:MERGE-BLACK :COPY))
            ("Copy to gray" :VALUE (:MERGE-GRAY :COPY))
            ("Set bits in black" :VALUE (:MERGE-BLACK :SET))
            ("Set bits in gray" :VALUE (:MERGE-GRAY :SET))
            ("Clear bits in black" :VALUE (:MERGE-BLACK :CLEAR))
            ("Clear bits in gray" :VALUE (:MERGE-GRAY :CLEAR))
            ("Flip bits in black" :VALUE (:MERGE-BLACK :FLIP))
            ("Flip bits in gray" :VALUE (:MERGE-GRAY :FLIP))))

;We get this message when the mouse is clicked on a register.
(DEFMETHOD (FED :REGISTER-CLICK) (REGISTER CLICK)
  (COND ((= CLICK #/MOUSE-1-1)
         (SEND REGISTER :SET-CONTENTS PLANE
                                      CHAR-BOX-X1 CHAR-BOX-Y1
                                      CHAR-BOX-X2 CHAR-BOX-Y2
                                      CHAR-BOX-Y3))
        ((= CLICK #/MOUSE-2-1)
         (LEXPR-SEND SELF :SET-CONTENTS (SEND REGISTER :CONTENTS)))
        ((= CLICK #/MOUSE-3-1)
         (LET* ((SUBOP (FED-CHOOSE FED-REGISTER-OP-MENU-ALIST "Register operation")))
           (ECASE (CAR SUBOP)
             (:CLEAR-REG (SEND REGISTER :ERASE-ALL)
                         (SEND REGISTER :CLEAR-WINDOW))
             (:LOAD-REG (SEND REGISTER :SET-CONTENTS PLANE
                                       CHAR-BOX-X1 CHAR-BOX-Y1
                                       CHAR-BOX-X2 CHAR-BOX-Y2
                                       CHAR-BOX-Y3))
             (:LOAD-REG-GRAY (SEND REGISTER :SET-CONTENTS GRAY-PLANE
                                            (+ CHAR-BOX-X1 GRAY-X-OFFSET)
                                            (+ CHAR-BOX-Y1 GRAY-Y-OFFSET)
                                            (+ CHAR-BOX-X2 GRAY-X-OFFSET)
                                            (+ CHAR-BOX-Y2 GRAY-Y-OFFSET)
                                            (+ CHAR-BOX-Y3 GRAY-Y-OFFSET)))
             (:LOAD-BLACK (LEXPR-SEND SELF :SET-CONTENTS (SEND REGISTER :CONTENTS)))
             (:MERGE-BLACK (LEXPR-SEND SELF :MERGE-CONTENTS
                                            (CADR SUBOP)
                                            (SEND REGISTER :CONTENTS)))
             (:MERGE-GRAY (LEXPR-SEND SELF :MERGE-GRAY
                                           (CADR SUBOP)
                                           (SEND REGISTER :CONTENTS)))
             (NIL))))))

(DEFUN COM-SCALE-CHARACTER ()
  (LET ((XNUM (PROMPT-LINE-DEFAULTED-READLINE
                1 'READ-FROM-STRING
                "Numerator of X scale factor (default 1): "))
        (XDENOM (PROMPT-LINE-DEFAULTED-READLINE
                  1 'READ-FROM-STRING
                  "Denominator of X scale factor (default 1): "))
        (YNUM (PROMPT-LINE-DEFAULTED-READLINE
                1 'READ-FROM-STRING
                "Numerator of Y scale factor (default 1): "))
        (YDENOM (PROMPT-LINE-DEFAULTED-READLINE
                  1 'READ-FROM-STRING
                  "Denominator of Y scale factor (default 1): ")))
    (SCALE-CHARACTER XNUM XDENOM YNUM YDENOM)))

(DEFUN SCALE-CHARACTER (XNUM XDENOM YNUM YDENOM)
  (DECLARE (:SELF-FLAVOR FED))
  (SEND SELF :MUST-REDISPLAY-CURRENT-AREAS PLANE)
  (SETQ UNSAVED-CHANGES T)
  (LET* ((OPLANE PLANE)
         (XOFFS (+ (TRUNCATE (* (- (CAR (PLANE-ORIGIN OPLANE))
                                   (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2))
                                XNUM)
                             XDENOM)
                   (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2)))
         (YOFFS (+ (TRUNCATE (* (- (CADR (PLANE-ORIGIN OPLANE)) CHAR-BOX-Y2) YNUM) YDENOM)
                   CHAR-BOX-Y2))
         (XEND (+ XOFFS (TRUNCATE (* (ARRAY-DIMENSION OPLANE 0) XNUM) XDENOM)))
         (YEND (+ YOFFS (TRUNCATE (* (ARRAY-DIMENSION OPLANE 1) YNUM) YDENOM))))
    (SETQ PLANE (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
    (PLANE-ENSURE-EXISTS PLANE XOFFS YOFFS)
    (PLANE-ENSURE-EXISTS PLANE (1- XEND) (1- YEND))
    (LET ((XORG (CAR (PLANE-ORIGIN PLANE)))
          (YORG (CADR (PLANE-ORIGIN PLANE)))
          (BIG (MAKE-ARRAY (LIST (* (ARRAY-DIMENSION OPLANE 0) XNUM)
                                 (* (ARRAY-DIMENSION OPLANE 1) YNUM))
                           :ELEMENT-TYPE '(MOD 16.))))
      (DO ((I (1- (ARRAY-DIMENSION OPLANE 0)) (1- I)))
          ((MINUSP I))
        (DO ((J (1- (ARRAY-DIMENSION OPLANE 1)) (1- J)))
            ((MINUSP J))
          (IF (NOT (ZEROP (AREF OPLANE I J)))
              (DO ((M 0 (1+ M)))
                  ((= M XNUM))
                (DO ((N 0 (1+ N)))
                    ((= N YNUM))
                  (SETF (AREF BIG (+ (* I XNUM) M) (+ (* J YNUM) N)) 1))))))
      (DO ((I XOFFS (1+ I)))
          ((= I XEND))
        (DO ((J YOFFS (1+ J)))
            ((= J YEND))
          (IF (> (LOOP FOR X FROM (* (- I XOFFS) XDENOM) BELOW (* (- I XOFFS -1) XDENOM)
                       SUMMING (LOOP FOR Y FROM (* (- J YOFFS) YDENOM)
                                     BELOW (* (- J YOFFS -1) YDENOM)
                                     COUNT (NOT (ZEROP (AREF BIG X Y)))))
                 (TRUNCATE (* XDENOM YDENOM) 2))
              (SETF (AREF PLANE (- I XORG) (- J YORG)) 1))))
      (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE PLANE))))

(DEFCONST RECTANGLE-OPERATION-MENU-ALIST
          `(("Clear rectangle" :VALUE ,TV:ALU-ANDCA)
            ("Set rectangle" :VALUE ,TV:ALU-IOR)
            ("Flip rectangle" :VALUE ,TV:ALU-XOR)))

(DEFUN COM-CLEAR-RECTANGLE ()
  (COM-OPERATE-ON-RECTANGLE T))

(DEFUN COM-OPERATE-ON-RECTANGLE (&OPTIONAL CLEAR-FLAG)
  (DECLARE (:SELF-FLAVOR FED))
  (PROG (X Y OX OY
         (BOOLE-OP
           (IF CLEAR-FLAG
               TV:ALU-ANDCA
             (FED-CHOOSE RECTANGLE-OPERATION-MENU-ALIST "Operation on rectangle"))))
        (OR BOOLE-OP (RETURN NIL))
        (UNWIND-PROTECT
            (PROGN
              (PROMPT-LINE
                (IF CLEAR-FLAG
                    "Clear rectangle.  Select one corner with LEFT button."
                    "Operate on rectangle.  Select one corner with LEFT button."))
              (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
                    "Left: Specify corner.  Middle//Right: Abort.")
              (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
              (OR OX (RETURN NIL))
              (SEND SELF :GRAY-POINT OX OY)
              (PROMPT-LINE "Select the other corner with LEFT button.")
              (UNWIND-PROTECT
                  (SETF (VALUES X Y) (SEND SELF :MOUSE-SELECT-POINT))
                (SEND SELF :GRAY-POINT OX OY)))
          (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
          (SEND PROMPT-WINDOW :CLEAR-WINDOW))
        (OR X (RETURN NIL))
        (LET ((MIN-X (+ WINDOW-X-POS (MIN X OX))) (MIN-Y (+ WINDOW-Y-POS (MIN Y OY)))
              (MAX-X (+ WINDOW-X-POS (MAX X OX))) (MAX-Y (+ WINDOW-Y-POS (MAX Y OY))))
          (DO ((I MIN-X (1+ I))) ((> I MAX-X))
            (DO ((J MIN-Y (1+ J))) ((> J MAX-Y))
              (SETF (PLANE-AREF PLANE I J) (BOOLE BOOLE-OP 1 (PLANE-AREF PLANE I J)))))
          (SETQ UNSAVED-CHANGES T)
          (SEND SELF :MUST-REDISPLAY REDISPLAY-SOME
                     (- MIN-X WINDOW-X-POS)
                     (- MIN-Y WINDOW-Y-POS)
                     (- MAX-X WINDOW-X-POS)
                     (- MAX-Y WINDOW-Y-POS)))))


(DEFUN COM-REFLECT-CHARACTER (&AUX AXIS)
  (DECLARE (:SELF-FLAVOR FED))
  (AND (SETQ AXIS (FED-CHOOSE '(("X") ("Y") ("XY") ("-XY")) "Axis to reflect in"))
       (REFLECT-CHARACTER AXIS)))

(DEFUN REFLECT-CHARACTER (AXIS &AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ NEW-CHAR (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
  (SETQ ORIGINS (PLANE-ORIGIN PLANE))
  (SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
  (DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
       (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
      (( HPOS HEND))
    (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
         (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
        (( VPOS VEND))
      (LET ((NEWVPOS VPOS) (NEWHPOS HPOS))
        (COND ((EQUALP AXIS "X")
               (SETQ NEWVPOS
                     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS)))
              ((EQUALP AXIS "Y")
               (SETQ NEWHPOS
                     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS)))
              ((EQUALP AXIS "-XY")
               (SETQ NEWHPOS (+ CHAR-BOX-X1 (- VPOS CHAR-BOX-Y1))
                     NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1))))
              ((EQUALP AXIS "XY")

               ;; Invert in the origin, then reflect in X-Y.

               (SETQ NEWVPOS
                     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS))
               (SETQ NEWHPOS
                     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS))
               (PSETQ NEWHPOS (+ CHAR-BOX-X1 (- NEWVPOS CHAR-BOX-Y1))
                      NEWVPOS (+ CHAR-BOX-Y1 (- NEWHPOS CHAR-BOX-X1)))))
        (SETF (PLANE-AREF NEW-CHAR NEWHPOS NEWVPOS) (PLANE-AREF PLANE HPOS VPOS)))))
  (SETQ PLANE NEW-CHAR)
  (SETQ UNSAVED-CHANGES T)
  (SETQ REDISPLAY-DEGREE REDISPLAY-ALL))

(DEFUN COM-ROTATE-CHARACTER-RIGHT (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ NEW-CHAR (MAKE-PLANE 2 :ELEMENT-TYPE '(MOD 16.) :DEFAULT-VALUE 0 :EXTENSION 8.))
  (SETQ ORIGINS (PLANE-ORIGIN PLANE))
  (SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
  (DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
       (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
      (( HPOS HEND))
    (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
         (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
        (( VPOS VEND))
      (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1)))
            (NEWHPOS (- CHAR-BOX-X2 1 (- VPOS CHAR-BOX-Y1))))
        (SETF (PLANE-AREF NEW-CHAR NEWHPOS NEWVPOS) (PLANE-AREF PLANE HPOS VPOS)))))
  (SETQ PLANE NEW-CHAR)
  (SETQ UNSAVED-CHANGES T)
  (SETQ REDISPLAY-DEGREE REDISPLAY-ALL))

(DEFUN COM-REGENERATE-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (AND CURRENT-CHARACTER (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER NIL))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT (FONT-GET-FD CURRENT-FONT)))

;;; Save the editing that has been done on the current character.

(DEFUN COM-SAVE-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character.")
    (PROMPT-LINE "Saving ~:C (~O) in ~A" CURRENT-CHARACTER CURRENT-CHARACTER CURRENT-FONT)
    (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER)
    (SETQ UNSAVED-CHANGES NIL)
    (SEND SELF :MUST-REDISPLAY-LABEL)))

(DEFUN COM-STORE-CHARACTER-EXPLICIT ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((FONT (PROMPT-LINE-DEFAULTED-READLINE
                CURRENT-FONT 'INTERN-FONT-NAME
                "Font to save character in (default ~A): " CURRENT-FONT)))
    (IF (NOT (AND FONT (BOUNDP FONT)))
        (BARF "Font does not exist.")
      (LET ((CH (PROMPT-LINE-TYI "Character of ~A to store in: " FONT)))
        (IF (GET-CHAR-EDITING-PLANE FONT CH)
            (BARF "Warning: ~A character ~:C is being edited in another FED"
                  FONT CH))
        (FONT-STORE-CD FONT CH)))))

(DEFUN FONT-STORE-CD (FONTNAME CHAR &OPTIONAL (UPDATE-FONT-FLAG T)
                      &AUX FD CD YSTART XSTART YWIDTH XWIDTH KERN
                      PLANE-X1 PLANE-Y1 PLANE-WIDTH PLANE-HEIGHT)
  "Store the current FED data into character CHAR of FONTNAME.
If UPDATE-FONT-FLAG is NIL, only the font-descriptor for that font
is updated, not the actual font."
  (DECLARE (:SELF-FLAVOR FED))
  ;; Find the FD format array for this font.
  (SETQ FD (FONT-GET-FD FONTNAME))
  ;; Warn if char box now displayed is incompatible with the font.
  (COND ((OR ( (- CHAR-BOX-Y2 CHAR-BOX-Y1) (FD-BASELINE FD))
             ( (- CHAR-BOX-Y3 CHAR-BOX-Y1) (FD-LINE-SPACING FD)))
         (OR (Y-OR-N-P
               "Character height and baseline are incompatible with font.
If actually stored, the character will be aligned by the
top of its box.

Proceed to store anyway?")
             (RETURN-FROM FONT-STORE-CD NIL))))
  ;; What are the regions of the fed data plane which actually are stored?
  (SETQ PLANE-X1 (FIRST (PLANE-ORIGIN PLANE)))
  (SETQ PLANE-Y1 (SECOND (PLANE-ORIGIN PLANE)))
  (SETQ PLANE-WIDTH (FIRST (ARRAY-DIMENSIONS PLANE)))
  (SETQ PLANE-HEIGHT (SECOND (ARRAY-DIMENSIONS PLANE)))
  ;; Figure out what portion of the plane holding the fed data is really nonzero.
  ;;   XSTART and YSTART get the indices in PLANE (as an array, not as a plane!)
  ;;   of what is going to go into the upper left corner of the CD.
  ;;   XWIDTH and YWIDTH get the dimensions which the CD will need to hold all nonzero data.
  ;;   XSTART is determined by the leftmost nonzero data, and its distance from
  ;;   CHAR-BOX-X1 determines the left kern.  YSTART has to correspond to CHAR-BOX-Y1
  ;;   because that is not a per-character parameter.
  (SETQ YSTART (- CHAR-BOX-Y1 PLANE-Y1) YWIDTH 0)
  (DO ((J (MAX YSTART 0) (1+ J)))
      ((= J PLANE-HEIGHT))
    (DO ((I 0 (1+ I)))
        ((= I PLANE-WIDTH))
      (WHEN (NOT (ZEROP (AREF PLANE I J)))
        (SETQ YWIDTH (1+ (- J YSTART))))))
  (SETQ XSTART NIL XWIDTH 0)
  (DO ((I 0 (1+ I)))
      ((= I PLANE-WIDTH))
    (DO ((J (MAX YSTART 0) (1+ J)))
        ((= J (+ YSTART YWIDTH)))
      (WHEN (NOT (ZEROP (AREF PLANE I J)))
        (OR XSTART (SETQ XSTART I))
        (SETQ XWIDTH (1+ (- I XSTART))))))
  ;; Make sure XSTART isn't NIL, and neither width is zero.
  (WHEN (NULL XSTART)
    (SETQ XSTART 0 XWIDTH 1))
  (AND (ZEROP YWIDTH) (SETQ YWIDTH 1))
  ;; Warn about dots to be lost above YSTART.
  (BLOCK FOO
    (DO ((I 0 (1+ I)))
        ((= I PLANE-WIDTH))
      (DO J 0 (1+ J) ( J YSTART)
          (OR (ZEROP (AREF PLANE I J))
              (COND ((Y-OR-N-P
                       "Dots above character top will be lost. Store anyway? ")
                     (RETURN-FROM FOO NIL))
                    (T (RETURN-FROM FONT-STORE-CD NIL)))))
      (DO ((J (+ YSTART YWIDTH) (1+ J)))
          (( J PLANE-HEIGHT))
        (OR (ZEROP (AREF PLANE I J))
            (COND ((Y-OR-N-P
                     "Dots below character bottom will be lost. Store anyway? ")
                   (RETURN-FROM FOO NIL))
                  (T (RETURN-FROM FONT-STORE-CD NIL)))))))
  (SETQ KERN (- CHAR-BOX-X1 XSTART PLANE-X1))
  ;; Copy the data in the FED buffer into a CD.
  (SETQ CD (MAKE-CHAR-DESCRIPTOR
             :MAKE-ARRAY (:TYPE 'ART-4B :DIMENSIONS (LIST YWIDTH XWIDTH))
             :CD-CHAR-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1)
             :CD-CHAR-LEFT-KERN KERN))
  (DO ((J 0 (1+ J)))
      ((= J YWIDTH))
    (IF (OR (MINUSP (+ J YSTART))
            (> (+ J YSTART) PLANE-HEIGHT))
        ;; If we are outside the existing plane,
        ;; fetch zeros.
        (DOTIMES (I XWIDTH)
          (SETF (AREF CD J I) 0))
      (DOTIMES (I XWIDTH)
        (SETF (AREF CD J I)
              (AREF PLANE (+ I XSTART) (+ J YSTART))))))
  (COND (UPDATE-FONT-FLAG
         ;; Use the CD just made to update the font itself, or make a new font.
         (FONT-NAME-STORE-CD FONTNAME CD CHAR))
        (T
         ;; Store the CD in the FD.
         (AND ( CHAR (ARRAY-LENGTH FD))
              (ADJUST-ARRAY-SIZE FD (1+ CHAR)))
         (SETF (AREF FD CHAR) CD)
         (AND (= CHAR #/SP)
              (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD))))))

;;; Display all of the characters of the font  being edited, to show what they look like.
;;;   Above each one is the corresponding character of default font, so you
;;;   can see which character is which in non-alphabetic fonts.
;;;   FROM-FED is T when called from FED, NIL when called from elsewhere, eg ZWEI.

;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:15
(DEFUN COM-DISPLAY-FONT (&OPTIONAL FONT (WINDOW *TERMINAL-IO*) (FROM-FED T)
                                   (CLEAR-FIRST-P T))
  (DECLARE (:SELF-FLAVOR FED))
  (OR FONT
      (SETQ FONT
            (OR (AND (BOUNDP CURRENT-FONT)
                     (SYMBOL-VALUE CURRENT-FONT))
                (WHEN CURRENT-FONT
                  (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT
                                                     (FONT-GET-FD CURRENT-FONT))
                  (SYMBOL-VALUE CURRENT-FONT)))))
  (IF FONT
      (DISPLAY-FONT FONT WINDOW CLEAR-FIRST-P FROM-FED)
    (FORMAT T "~%~%Can't display current font -- no current font.~%~%Press <SPACE> to continue.~%~%")))

(DEFUN COM-LIST-FONTS ()
  (LIST-FONTS-LOADED)
  (SEND *STANDARD-OUTPUT* :ITEM 'FONT 'FILE-COMPUTER-FONT "--- List fonts on file computer ---")
  (FORMAT T "~&Type any character to flush:"))

(DEFUN COM-LIST-FC-FONTS ()
  (SEND *STANDARD-OUTPUT* :HOME-CURSOR)
  (SEND *STANDARD-OUTPUT* :CLEAR-REST-OF-LINE)
  (LIST-FONTS-FILE-COMPUTER)
  (FORMAT T "~&Type any character to flush:"))

(DEFUN LIST-FONTS-LOADED ()
  (FORMAT T "Loaded fonts - Mouse one to select~%")
  (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
                          (LET ((LIST NIL))
                            (MAPATOMS #'(LAMBDA (X) (AND (BOUNDP X)
                                                       (TYPEP (SYMBOL-VALUE X) 'FONT)
                                                       (PUSH X LIST)))
                                      "FONTS" NIL)
                            (SORT LIST #'STRING-LESSP))))

(DEFUN LIST-FONTS-FILE-COMPUTER ()
  (FORMAT T "Fonts on file computer - Mouse one to select~%")
  (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FONT
                         (SORT (MAPCAR #'(LAMBDA (X)
                                           (INTERN (STRING (SEND (CAR X) :NAME)) "FONTS"))
                                       (CDR (FS:DIRECTORY-LIST "SYS:FONTS;* QFASL >" :FAST)))
                               #'ALPHALESSP)))

;;; Print a character on a sheet, assuming that sheet is set up to that font.
;;;   If the character is the one being edited,
;;;   the picture being edited is displayed.

(DEFUN FED-TYO (SHEET CH FONTNAME)
  "Output CH on SHEET in font FONTNAME, as the character appears in FED.
This function is like TYO except the character appears as it would
appear in the FED that is editing the character, not as it actually is in the font.
You need not be inside FED to use this function.
FONTNAME should also be the current font of SHEET
in case the character is not being edited in any FED."
  (DO ((ELTS FED-EDITED-CHARS (CDR ELTS)))
      ((NULL ELTS) (TV:SHEET-TYO SHEET CH))
    (LET ((ELT (CAR ELTS)))
      (AND (EQ (CAR ELT) FONTNAME)
           (EQ (CADR ELT) CH)
           (RETURN (SEND (CADDR ELT) :TYO-EDITED-CHAR SHEET))))))

(DEFMETHOD (BASIC-FED :TYO-EDITED-CHAR) (SHEET)
    (LET (

          ;; Offset from horiz idx in plane to hpos of dot on screen.

          (LEFT (+ (- (TV:SHEET-CURSOR-X SHEET) CHAR-BOX-X1)
                   (FIRST (PLANE-ORIGIN PLANE))))

          ;; Offset from vert idx in plane to vpos of dot on screen.

          (TOP (+ (- (TV:SHEET-CURSOR-Y SHEET) CHAR-BOX-Y2)
                  (TV:SHEET-BASELINE SHEET)
                  (SECOND (PLANE-ORIGIN PLANE))))
          (PLANE-WIDTH (FIRST (ARRAY-DIMENSIONS PLANE)))

          ;; First vertical idx to print from in plane.

          (PLANE-TOP (MAX 0 (- CHAR-BOX-Y1 (SECOND (PLANE-ORIGIN PLANE)))))

          ;; Last+1 vertical idx to print from in plane.

          (PLANE-BOTTOM (MIN (SECOND (ARRAY-DIMENSIONS PLANE))
                             (- CHAR-BOX-Y3 (SECOND (PLANE-ORIGIN PLANE))))))

      (TV:PREPARE-SHEET (SHEET)
        (DOTIMES (HPOS PLANE-WIDTH)
          (DO ((VPOS PLANE-TOP (1+ VPOS)))
              (( VPOS PLANE-BOTTOM))
            (OR (ZEROP (AREF PLANE HPOS VPOS))
                (%DRAW-RECTANGLE 1 1 (+ HPOS LEFT) (+ VPOS TOP) TV:ALU-IOR SHEET))))
        (TV:SHEET-INCREMENT-BITPOS SHEET (- CHAR-BOX-X2 CHAR-BOX-X1) 0))))

(DEFUN COM-MOUSE-DRAW (XPOS YPOS)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ CURSOR-ON NIL)
  (SEND SELF
        ':MOUSE-BOOLE-SQUARES
        (IF (ZEROP DRAW-MODE)                   ;LATCH mode
            (IF (ZEROP (MULTIPLE-VALUE-BIND (WINDOW-X-OFFSET WINDOW-Y-OFFSET) ;algorithm from GRID-MIXIN :MOUSE-BOOLE-SQUARES
                           (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET)
                         (LET ((X (TRUNCATE (- TV:MOUSE-X
                                               WINDOW-X-OFFSET
                                               TV:LEFT-MARGIN-SIZE
                                               1)
                                            BOX-X-SIZE))
                               (Y (TRUNCATE (- TV:MOUSE-Y
                                               WINDOW-Y-OFFSET
                                               TV:TOP-MARGIN-SIZE
                                               1)
                                            BOX-Y-SIZE)))
                           (COND ((NOT (AND (LESSP -1 X WINDOW-X-SIZE)
                                            (LESSP -1 Y WINDOW-Y-SIZE)))
                                  0)
                                 (T
                                  (AREF WINDOW-ARRAY X Y))))))
                7
              2)
          DRAW-MODE)
        XPOS YPOS))

(DEFUN COM-MOUSE-MOVE-CHAR-BOX (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ CURSOR-ON NIL UNSAVED-CHANGES T)
  (SEND SELF :MOUSE-MOVE-CHAR-BOX)
  (SEND SELF :MUST-REDISPLAY-LABEL))


(DEFUN COM-MOUSE-DRAW-LINE (&AUX OX OY X Y)
  (DECLARE (:SELF-FLAVOR FED))
  (BLOCK NIL
    (UNWIND-PROTECT
        (PROGN
          (PROMPT-LINE "Select end points with LEFT mouse button.")
          (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
                "Left: Specify end points of line.  Middle//Right: Abort.")
          (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
          (OR OX (RETURN NIL))
          (SEND SELF :GRAY-POINT OX OY)
          (UNWIND-PROTECT
              (SETF (VALUES X Y) (SEND SELF :MOUSE-SELECT-POINT))
            (SEND SELF :GRAY-POINT OX OY)))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
      (SEND PROMPT-WINDOW :CLEAR-WINDOW))
    (OR X (RETURN NIL))
    (SEND SELF :DRAW-GRID-LINE OX OY X Y (IF (ZEROP DRAW-MODE)
                                             TV:ALU-IOR
                                           DRAW-MODE))
    (SETQ UNSAVED-CHANGES T)))

(DEFVAR SPLINE-X)
(DEFVAR SPLINE-Y)
(DEFVAR SPLINE-CX NIL)
(DEFVAR SPLINE-CY NIL)
;; Copied from LAD: RELEASE-3.WINDOW; FED.LISP#240 on 2-Oct-86 04:30:19
(DEFUN COM-MOUSE-DRAW-SPLINE (&AUX I Y)
  (DECLARE (:SELF-FLAVOR FED))
  (LET-GLOBALLY ((DRAW-MODE (IF (OR (ZEROP DRAW-MODE)
                                    (= DRAW-MODE TV:ALU-XOR)) ;XOR doesn't work; some points multiply frobbed?
                                TV:ALU-IOR DRAW-MODE)))
    (UNLESS (VARIABLE-BOUNDP SPLINE-X)
      (SETQ SPLINE-X (MAKE-ARRAY 100. :FILL-POINTER 0)
            SPLINE-Y (MAKE-ARRAY 100. :FILL-POINTER 0)))
    (STORE-ARRAY-LEADER 0 SPLINE-X 0)
    (STORE-ARRAY-LEADER 0 SPLINE-Y 0)
    (PROMPT-LINE
      "~A Spline.  Select points with left mouse button.
Middle to abort.  Click right when done."
      (SEND SELF :DRAW-MODE-STRING))
    (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
          "Left: Specify points.  Middle: Abort.  Right: Finish.")
    (UNWIND-PROTECT
        (DO ((X)) (NIL)
          (MULTIPLE-VALUE-SETQ (X Y) (SEND SELF :MOUSE-SELECT-POINT T))
          (OR X (RETURN NIL))
          (SEND SELF :GRAY-POINT X Y)
          (VECTOR-PUSH-EXTEND X SPLINE-X)
          (VECTOR-PUSH-EXTEND Y SPLINE-Y))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
      (SEND PROMPT-WINDOW :CLEAR-WINDOW)
      (DOTIMES (I (LENGTH SPLINE-X))            ;Erase old marks
        (SEND SELF :GRAY-POINT (AREF SPLINE-X I) (AREF SPLINE-Y I))))
    (COND ((AND (EQ (CAR-SAFE Y) :MOUSE-BUTTON)
                (= (CADR Y) #/MOUSE-3-1)
                (>= (LENGTH SPLINE-X) 2))
           (MULTIPLE-VALUE (SPLINE-CX SPLINE-CY I)
             (TV:SPLINE SPLINE-X SPLINE-Y 10. SPLINE-CX SPLINE-CY))
           (SEND SELF :DRAW-CURVE SPLINE-CX SPLINE-CY I DRAW-MODE)
           (SETQ UNSAVED-CHANGES T))
          (T (TV:BEEP)))))

(COMPILE-FLAVOR-METHODS FED-FRAME FED REGISTER-PANE FED-LABEL-WINDOW
                        FED-TYPEOUT-WINDOW)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Frame-grabbing FED overlay; see also defmethod (fed-frame :after :init), defflavor FED-frame & fg-menu-command-alist, above.

(DEFFLAVOR BASIC-COLOR-CHAR-BOX-AND-LINE ((CENTER-X (// (TV:SHEET-WIDTH COLOR:COLOR-SCREEN) 2))
                                          (CENTER-Y (// (TV:SHEET-HEIGHT COLOR:COLOR-SCREEN) 2))
                                          (X2 7)
                                          (Y2 9)
                                          (Y3 12.)
                                          (SCALE 1.)
                                          (ANGLE 0)
                                          BOX-TL-X
                                          BOX-TL-Y
                                          BOX-TR-X
                                          BOX-TR-Y
                                          BOX-BL-X
                                          BOX-BL-Y
                                          BOX-BR-X
                                          BOX-BR-Y
                                          LINE-L-X
                                          LINE-L-Y
                                          LINE-R-X
                                          LINE-R-Y)
           ()
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (BASIC-COLOR-CHAR-BOX-AND-LINE :UPDATE-VARS) ()
  (SETQ  BOX-TL-X (TRUNCATE (- CENTER-X (* Y3 -.5 (SIN ANGLE) SCALE) (* X2 .5 (COS ANGLE) SCALE)))
         BOX-TL-Y (TRUNCATE (- CENTER-Y (* Y3 .5 (COS ANGLE) SCALE) (* X2 .5 (SIN ANGLE) SCALE)))
         BOX-TR-X (TRUNCATE (+ BOX-TL-X (* X2 (COS ANGLE) SCALE)))
         BOX-TR-Y (TRUNCATE (+ BOX-TL-Y (* X2 (SIN ANGLE) SCALE)))
         BOX-BL-X (TRUNCATE (- BOX-TL-X (* Y2 (SIN ANGLE) SCALE)))
         BOX-BL-Y (TRUNCATE (+ BOX-TL-Y (* Y2 (COS ANGLE) SCALE)))
         BOX-BR-X (TRUNCATE (+ BOX-BL-X (* X2 (COS ANGLE) SCALE)))
         BOX-BR-Y (TRUNCATE (+ BOX-BL-Y (* X2 (SIN ANGLE) SCALE)))
         LINE-L-X (TRUNCATE (- BOX-TL-X (* Y3 (SIN ANGLE) SCALE)))
         LINE-L-Y (TRUNCATE (+ BOX-TL-Y (* Y3 (COS ANGLE) SCALE)))
         LINE-R-X (TRUNCATE (+ LINE-L-X (* X2 (COS ANGLE) SCALE)))
         LINE-R-Y (TRUNCATE (+ LINE-L-Y (* X2 (SIN ANGLE) SCALE)))))

(DEFMETHOD (BASIC-COLOR-CHAR-BOX-AND-LINE :UNDRAW) ()
  (SEND SELF ':UPDATE-VARS)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-TL-X) (ROUND BOX-TL-Y) (ROUND BOX-TR-X) (ROUND BOX-TR-Y) 1 TV:ALU-ANDCA)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-TR-X) (ROUND BOX-TR-Y) (ROUND BOX-BR-X) (ROUND BOX-BR-Y) 1 TV:ALU-ANDCA)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-BR-X) (ROUND BOX-BR-Y) (ROUND BOX-BL-X) (ROUND BOX-BL-Y) 1 TV:ALU-ANDCA)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-BL-X) (ROUND BOX-BL-Y) (ROUND BOX-TL-X) (ROUND BOX-TL-Y) 1 TV:ALU-ANDCA)
  (COLOR:COLOR-DRAW-LINE (ROUND LINE-L-X) (ROUND LINE-L-Y) (ROUND LINE-R-X) (ROUND LINE-R-Y) 1 TV:ALU-ANDCA))

(DEFMETHOD (BASIC-COLOR-CHAR-BOX-AND-LINE :DRAW) ()
  (SEND SELF ':UPDATE-VARS)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-TL-X) (ROUND BOX-TL-Y) (ROUND BOX-TR-X) (ROUND BOX-TR-Y) 1 TV:ALU-IOR)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-TR-X) (ROUND BOX-TR-Y) (ROUND BOX-BR-X) (ROUND BOX-BR-Y) 1 TV:ALU-IOR)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-BR-X) (ROUND BOX-BR-Y) (ROUND BOX-BL-X) (ROUND BOX-BL-Y) 1 TV:ALU-IOR)
  (COLOR:COLOR-DRAW-LINE (ROUND BOX-BL-X) (ROUND BOX-BL-Y) (ROUND BOX-TL-X) (ROUND BOX-TL-Y) 1 TV:ALU-IOR)
  (COLOR:COLOR-DRAW-LINE (ROUND LINE-L-X) (ROUND LINE-L-Y) (ROUND LINE-R-X) (ROUND LINE-R-Y) 1 TV:ALU-IOR))


(DEFMETHOD (BASIC-COLOR-CHAR-BOX-AND-LINE :CHECK-P) (&OPTIONAL &KEY (CENTER-X CENTER-X)
                                                                  (CENTER-Y CENTER-Y)
                                                                  (ANGLE ANGLE)
                                                                  (SCALE SCALE))
    (LET*  ((BOX-TL-X (- CENTER-X (* Y3 -.5 (SIN ANGLE) SCALE) (* X2 .5 (COS ANGLE) SCALE)))
            (BOX-TL-Y (- CENTER-Y (* Y3 .5 (COS ANGLE) SCALE) (* X2 .5 (SIN ANGLE) SCALE)))
            (BOX-TR-X (+ BOX-TL-X (* X2 (COS ANGLE) SCALE)))
            (BOX-TR-Y (+ BOX-TL-Y (* X2 (SIN ANGLE) SCALE)))
            (BOX-BL-X (- BOX-TL-X (* Y2 (SIN ANGLE) SCALE)))
            (BOX-BL-Y (+ BOX-TL-Y (* Y2 (COS ANGLE) SCALE)))
            (BOX-BR-X (+ BOX-BL-X (* X2 (COS ANGLE) SCALE)))
            (BOX-BR-Y (+ BOX-BL-Y (* X2 (SIN ANGLE) SCALE)))
            (LINE-L-X (- BOX-TL-X (* Y3 (SIN ANGLE) SCALE)))
            (LINE-L-Y (+ BOX-TL-Y (* Y3 (COS ANGLE) SCALE)))
            (LINE-R-X (+ LINE-L-X (* X2 (COS ANGLE) SCALE)))
            (LINE-R-Y (+ LINE-L-Y (* X2 (SIN ANGLE) SCALE))))
      (NOT (OR (MINUSP (MIN BOX-TL-X BOX-TL-Y BOX-TR-X BOX-TR-Y LINE-L-X LINE-L-Y LINE-R-X LINE-R-Y))
               (>= (MAX BOX-TL-X BOX-TR-X LINE-L-X LINE-R-X) (- (TV:SHEET-WIDTH COLOR:COLOR-SCREEN) 2))
               (>= (MAX BOX-TL-Y BOX-TR-Y LINE-L-Y LINE-R-Y) (- (TV:SHEET-HEIGHT COLOR:COLOR-SCREEN) 2))))))


(DEFVAR *COLOR-CHAR-BOX-AND-LINE* nil)  ;One for all feds.  (see basic-fed-pot :after :init)
(DEFVAR *SNAP-THRESHOLD* NIL)

(DEFFLAVOR BASIC-FED-POT ()
           (TV:WINDOW)
  (:DEFAULT-INIT-PLIST :LABEL NIL :BLINKER-P :NIL))

(DEFMETHOD (BASIC-FED-POT :WHO-LINE-DOCUMENTATION-STRING) ()
  "Monocontrast conversion threshold.  Up makes more black.")

(DEFMETHOD (BASIC-FED-POT :MOUSE-BUTTONS) (B X Y) ;necessary dummy to keep clicks away from grid window.
  (DECLARE (IGNORE B X Y))
  NIL)

(DEFMETHOD (BASIC-FED-POT :MOUSE-MOVES) (X Y)
  (DECLARE (IGNORE X))
  (IF (EQ SELF (TV:WINDOW-UNDER-MOUSE))
      (PROGN
        (IF (NOT (ZEROP (TV:MOUSE-BUTTONS)))
            (PROGN
              (SEND (FIRST TV:BLINKER-LIST) ':SET-CURSORPOS 4 Y)
              (SETQ *SNAP-THRESHOLD* (// (- TV:HEIGHT Y) 1.0 TV:HEIGHT)))) ; 1.0 forces flonum result.
        (DOTIMES (I 128.)
          (COLOR:WRITE-COLOR-MAP (+ (* 2 I) 1) 255. 192. 0.))
        (LET ((MAP-THRESHOLD (* *SNAP-THRESHOLD* 128.)))
          (DOTIMES (I 128.)
            (IF (> I MAP-THRESHOLD)
                (COLOR:WRITE-COLOR-MAP (* 2 I) 128. 128. 128.)
                (COLOR:WRITE-COLOR-MAP (* 2 I) 0. 0. 0.)))))
      (SEND COLOR:COLOR-SCREEN :LOAD-COLORMAP-FALSE-COLOR-ODD)))

(DEFMETHOD (BASIC-FED-POT :AFTER :INIT) (&REST IGNORE)
  (SEND (FIRST TV:BLINKER-LIST) ':SET-SIZE-AND-CURSORPOS 20. 3 4 (// (SEND SELF ':HEIGHT) 2))
  (SETQ *SNAP-THRESHOLD* .5))

(DEFUN COM-FG-CAMERA ()
  (DECLARE (:SELF-FLAVOR FED))
  (PREPARE-COLOR-SCREEN)
  (SEND COLOR:COLOR-SCREEN :MOUSEBUTTON-GRAB-FRAME)
  (SETQ FINAL-COLOR-ARRAY NIL))

(DEFUN COM-FG-CAMERA-TIL-SPACE ()
  (DECLARE (:SELF-FLAVOR FED))
  (PREPARE-COLOR-SCREEN)
  (FORMAT T "~%Medium-resolution color subsustem now in frame-grabber flow-through mode.~%Press <SPACE> when done.")
  (SEND COLOR:COLOR-SCREEN :GRAB-FRAME)
  (SEND STANDARD-INPUT :FORCE-KBD-INPUT #/ )
  (SETQ FINAL-COLOR-ARRAY NIL))

(DEFUN CHAR-BOX-CHANGED-SINCE-CROP-P ()
  (DECLARE (:SELF-FLAVOR FED))
  (NOT (AND (= (- (SEND SELF ':DISPLAYED-CHAR-BOX-X2) (SEND SELF ':DISPLAYED-CHAR-BOX-X1))
                         (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2))
                      (= (- (SEND SELF ':DISPLAYED-CHAR-BOX-Y2) (SEND SELF ':DISPLAYED-CHAR-BOX-Y1))
                         (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y2))
                      (= (- (SEND SELF ':DISPLAYED-CHAR-BOX-Y3) (SEND SELF ':DISPLAYED-CHAR-BOX-Y1))
                         (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3)))))


(DEFUN COM-FG-CROP ()
  (DECLARE (:SELF-FLAVOR FED))
  (LOOP WHILE (NOT (ZEROP (TV:MOUSE-BUTTONS))))
  (prepare-color-screen)
  (SETQ FINAL-COLOR-ARRAY NIL)
  (IF (OR (>= (- (SEND SELF ':DISPLAYED-CHAR-BOX-X2) (SEND SELF ':DISPLAYED-CHAR-BOX-X1))
              (- (TV:SHEET-WIDTH COLOR:COLOR-SCREEN) 2))
          (>= (- (SEND SELF ':DISPLAYED-CHAR-BOX-Y3) (SEND SELF ':DISPLAYED-CHAR-BOX-Y1))
              (- (TV:SHEET-HEIGHT COLOR:COLOR-SCREEN) 2)))
      (FORMAT T "Character cell dimensions exceed color screen dimensions.~%Type any character to flush:")
    (PROGN
      (IF (CHAR-BOX-CHANGED-SINCE-CROP-P)
          (PROGN
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':UNDRAW)
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-X2 (- (SEND SELF ':DISPLAYED-CHAR-BOX-X2)
                                                         (SEND SELF ':DISPLAYED-CHAR-BOX-X1)))
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-Y2 (- (SEND SELF ':DISPLAYED-CHAR-BOX-Y2)
                                                         (SEND SELF ':DISPLAYED-CHAR-BOX-Y1)))
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-Y3 (- (SEND SELF ':DISPLAYED-CHAR-BOX-Y3)
                                                         (SEND SELF ':DISPLAYED-CHAR-BOX-Y1)))
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-CENTER-X (// (TV:SHEET-WIDTH COLOR:COLOR-SCREEN) 2))
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-CENTER-Y (// (TV:SHEET-HEIGHT COLOR:COLOR-SCREEN) 2))
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-SCALE 1)
            (SEND *COLOR-CHAR-BOX-AND-LINE* ':SET-ANGLE 0)))
      (TV:WITH-MOUSE-USURPED
        (SETQ TV:WHO-LINE-MOUSE-GRABBED-DOCUMENTATION "Move Color Character-Box.  L: Zoom.   M: Rotate.   R: Done.")
        (LOOP WHILE (MULTIPLE-VALUE-BIND (MOUSE-DELTA-X MOUSE-DELTA-Y) (TV:MOUSE-INPUT NIL)
                      (SELECTQ (TV:MOUSE-BUTTONS)
                        (0 (COND ((SEND *COLOR-CHAR-BOX-AND-LINE* ':CHECK-P
                                           ':CENTER-X (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-X) MOUSE-DELTA-X)
                                           ':CENTER-Y (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-Y) MOUSE-DELTA-Y))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':UNDRAW)
                                  (SEND *COLOR-CHAR-BOX-AND-LINE*
                                           ':SET-CENTER-X
                                           (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-X) MOUSE-DELTA-X))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE*
                                           ':SET-CENTER-Y
                                           (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-Y) MOUSE-DELTA-Y))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':DRAW)
                                  T)
                                 (T
                                  (BEEP)
                                  T)))
                        (2 (COND ((SEND *COLOR-CHAR-BOX-AND-LINE* ':CHECK-P
                                           ':ANGLE (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':ANGLE)
                                                      (// MOUSE-DELTA-X 256.0)))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':UNDRAW)
                                  (SEND *COLOR-CHAR-BOX-AND-LINE*
                                           ':SET-ANGLE (mod (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':ANGLE)
                                                               (// MOUSE-DELTA-X 256.0))
                                                            (* 2 pi)))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':DRAW)
                                  T)
                                 (T
                                  (BEEP)
                                  T)))
                        (1 (COND ((SEND *COLOR-CHAR-BOX-AND-LINE* ':CHECK-P
                                           ':SCALE (MAX
                                                     (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':SCALE)
                                                        (// MOUSE-DELTA-Y 64.0))
                                                     1))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':UNDRAW)
                                  (SEND *COLOR-CHAR-BOX-AND-LINE*
                                           ':SET-SCALE (MAX
                                                         (+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':SCALE)
                                                            (// MOUSE-DELTA-Y 64.0))
                                                         1))
                                  (SEND *COLOR-CHAR-BOX-AND-LINE* ':DRAW)
                                  T)
                                 (T
                                  (BEEP)
                                  T)))
                        (4 NIL)
                        (T (SEND *COLOR-CHAR-BOX-AND-LINE* ':DRAW)
                           T)))))))
  (LOOP WHILE (NOT (ZEROP (TV:MOUSE-BUTTONS)))))


(DEFUN COM-FG-SNAP-TO-GRAY ()
  "Take the area under the color cursor and scale it
into an art-8b the same size as the desired character. Then
clip the scaled image at the threshold set by the slider pot"
  (DECLARE (:SELF-FLAVOR FED))
  (IF (CHAR-BOX-CHANGED-SINCE-CROP-P)
      (IF (TV:MOUSE-Y-OR-N-P "Char box has changed.  Please re-crop.")
          (PROGN
            (COM-FG-CROP)
            (COM-FG-SNAP-TO-GRAY)))
      (PROGN
        (SEND *COLOR-CHAR-BOX-AND-LINE* :UPDATE-VARS)
        (IF (NOT FINAL-COLOR-ARRAY)
            (PROGN
              (FORMAT T "~%Weiman transform on greyscale data in progress...~%")
              (SETQ FINAL-COLOR-ARRAY
                    (MAKE-ARRAY `(,(+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3) 16.)
                                  ,(+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2) 16.))
                                ':TYPE ART-8B))
              (LET ((S-TL-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TL-X))
                    (S-TL-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TL-Y))
                    (S-TR-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TR-X))
                    (S-TR-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TR-Y))
                    (S-BL-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-L-X))
                    (S-BL-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-L-Y))
                    (S-BR-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-R-X))
                    (S-BR-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-R-Y)))
                (IMAGE-CONVEX-REGION-ROTATE-SCALE-AND-TRANSLATE-COPY
                  (TV:SHEET-SCREEN-ARRAY COLOR:COLOR-SCREEN)
                  (SHORT-FLOAT (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-X))
                  (SHORT-FLOAT (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-Y))
                  `(((,(SHORT-FLOAT S-TL-X) ,(SHORT-FLOAT S-TL-Y)) (,(SHORT-FLOAT S-TR-X) ,(SHORT-FLOAT S-TR-Y)))
                    ((,(SHORT-FLOAT S-TR-X) ,(SHORT-FLOAT S-TR-Y)) (,(SHORT-FLOAT S-BR-X) ,(SHORT-FLOAT S-BR-Y)))
                    ((,(SHORT-FLOAT S-BR-X) ,(SHORT-FLOAT S-BR-Y)) (,(SHORT-FLOAT S-BL-X) ,(SHORT-FLOAT S-BL-Y)))
                    ((,(SHORT-FLOAT S-BL-X) ,(SHORT-FLOAT S-BL-Y)) (,(SHORT-FLOAT S-TL-X) ,(SHORT-FLOAT S-TL-Y))))
                  (SHORT-FLOAT (- (SEND *COLOR-CHAR-BOX-AND-LINE* ':ANGLE)))
                  (SHORT-FLOAT (// 1S0 (SEND *COLOR-CHAR-BOX-AND-LINE* ':SCALE)))
                  FINAL-COLOR-ARRAY
                  (+ 8S0 (// (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2) 2S0))        ;X2, Y3 INTEGERS
                  (+ 8S0 (// (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3) 2S0))
                  T T))
              (SEND STANDARD-INPUT :FORCE-KBD-INPUT #/ )))
        (SEND SELF ':MOVE-GRAY-PLANE 0 0 T)
        (SEND SELF ':ERASE-GRAY)
        (DOTIMES (Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3))
          (DOTIMES (X (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2))
            (PLANE-ASET (IF (> (AREF FINAL-COLOR-ARRAY (+ Y 8.) (+ X 8.))
                               (* 256. *SNAP-THRESHOLD*))
                            0
                            1)
                        GRAY-PLANE X Y)))
        (SEND SELF ':MUST-REDISPLAY-ENTIRE-PLANE GRAY-PLANE))))

(DEFUN COM-FG-SNAP-TO-BLACK ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (CHAR-BOX-CHANGED-SINCE-CROP-P)
      (IF (TV:MOUSE-Y-OR-N-P "Char box has changed.  Please re-crop.")
          (PROGN
            (COM-FG-CROP)
            (COM-FG-SNAP-TO-BLACK)))
      (PROGN
        (SEND *COLOR-CHAR-BOX-AND-LINE* :UPDATE-VARS)
        (IF (NOT FINAL-COLOR-ARRAY)
            (PROGN
              (format t "~%Weiman transform on greyscale data in progress...~%")
              (SETQ FINAL-COLOR-ARRAY
                    (MAKE-ARRAY `(,(+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3) 16.)
                                  ,(+ (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2) 16.))
                                ':TYPE ART-8B))
              (LET ((S-TL-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TL-X))
                    (S-TL-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TL-Y))
                    (S-TR-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TR-X))
                    (S-TR-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':BOX-TR-Y))
                    (S-BL-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-L-X))
                    (S-BL-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-L-Y))
                    (S-BR-X (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-R-X))
                    (S-BR-Y (SEND *COLOR-CHAR-BOX-AND-LINE* ':LINE-R-Y)))
                (IMAGE-CONVEX-REGION-ROTATE-SCALE-AND-TRANSLATE-COPY
                  (TV:SHEET-SCREEN-ARRAY COLOR:COLOR-SCREEN)
                  (SHORT-FLOAT (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-X))
                  (SHORT-FLOAT (SEND *COLOR-CHAR-BOX-AND-LINE* ':CENTER-Y))
                  `(((,(SHORT-FLOAT S-TL-X) ,(SHORT-FLOAT S-TL-Y)) (,(SHORT-FLOAT S-TR-X) ,(SHORT-FLOAT S-TR-Y)))
                    ((,(SHORT-FLOAT S-TR-X) ,(SHORT-FLOAT S-TR-Y)) (,(SHORT-FLOAT S-BR-X) ,(SHORT-FLOAT S-BR-Y)))
                    ((,(SHORT-FLOAT S-BR-X) ,(SHORT-FLOAT S-BR-Y)) (,(SHORT-FLOAT S-BL-X) ,(SHORT-FLOAT S-BL-Y)))
                    ((,(SHORT-FLOAT S-BL-X) ,(SHORT-FLOAT S-BL-Y)) (,(SHORT-FLOAT S-TL-X) ,(SHORT-FLOAT S-TL-Y))))
                  (SHORT-FLOAT (- (SEND *COLOR-CHAR-BOX-AND-LINE* ':ANGLE)))
                  (SHORT-FLOAT (// 1S0 (SEND *COLOR-CHAR-BOX-AND-LINE* ':SCALE)))
                  FINAL-COLOR-ARRAY
                  (+ 8S0 (// (SEND *COLOR-CHAR-BOX-AND-LINE* ':X2) 2S0))        ;X2, Y3 INTEGERS
                  (+ 8S0 (// (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3) 2S0))
                  T T))
              (SEND STANDARD-INPUT :FORCE-KBD-INPUT #/ )))
        (DOTIMES (I (SEND *COLOR-CHAR-BOX-AND-LINE*  ':X2))
          (DOTIMES (J (SEND *COLOR-CHAR-BOX-AND-LINE* ':Y3))
            (SEND SELF ':DRAW-POINT (+ I (SEND SELF ':DISPLAYED-CHAR-BOX-X1))
                                       (+ J (SEND SELF ':DISPLAYED-CHAR-BOX-Y1))
                                       (IF (> (AREF FINAL-COLOR-ARRAY  (+ J 8.) (+ I 8.)) (* 256. *SNAP-THRESHOLD*)) 0 1)))))))

(DEFUN COM-LOAD-COLOR (&AUX FILE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ FILE (PROMPT-AND-READ :PATHNAME "~%Pathname to load medium resolution color screen from:~
                                         ~%(Default ~a) " (NAMESTRING (FS:DEFAULT-PATHNAME))))
  (FS:SET-DEFAULT-PATHNAME FILE)
  (SEND COLOR:COLOR-SCREEN :BLT (SEND COLOR:COLOR-SCREEN :LOAD FILE) (TV:SHEET-SCREEN-ARRAY COLOR:COLOR-SCREEN))
  (SEND *COLOR-CHAR-BOX-AND-LINE* :DRAW)
  (SETQ FINAL-COLOR-ARRAY NIL)
  (SEND STANDARD-INPUT :FORCE-KBD-INPUT #/ ))

(DEFUN COM-SAVE-COLOR (&AUX FILE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ FILE (PROMPT-AND-READ :PATHNAME "~%Pathname to save medium resolution color screen to:~
                                         ~%(Default ~a) " (NAMESTRING (FS:DEFAULT-PATHNAME))))
  (FS:SET-DEFAULT-PATHNAME FILE)
  (SEND *COLOR-CHAR-BOX-AND-LINE* :UNDRAW)
  (SEND COLOR:COLOR-SCREEN :SAVE (TV:SHEET-SCREEN-ARRAY COLOR:COLOR-SCREEN) FILE)
  (SEND *COLOR-CHAR-BOX-AND-LINE* :DRAW)
  (SEND STANDARD-INPUT :FORCE-KBD-INPUT #/ ))

(DEFUN PREPARE-COLOR-SCREEN ()
  (SEND COLOR:COLOR-SCREEN :EXPOSE)
  (SEND COLOR:COLOR-SCREEN :LOAD-COLORMAP-FALSE-COLOR-ODD))

(COMPILE-FLAVOR-METHODS BASIC-COLOR-CHAR-BOX-AND-LINE BASIC-FED-POT)

(tv:add-system-key #/F 'FED:FED-FRAME "Font Edit" T)

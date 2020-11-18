;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-

;; this stuff used to live in sys:window;stream

;;; Hacked on 6/2/86 by RDM to make sure everything could deal with color pixels
;;; of size larger than one {mostly making sure everything takes a value or an
;;; ALU function containing the color}

(DEFFLAVOR GRAPHICS-MIXIN () ()
  ;; Explicit presence of SHEET helps init the flavor-unmapped-instance-variables.
  (:REQUIRED-FLAVORS SHEET ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN
   "Provides graphics output operations for windows."))

(DEFMETHOD (GRAPHICS-MIXIN :POINT) (X Y)
  (SETQ X (+ X (SHEET-INSIDE-LEFT)) Y (+ Y (SHEET-INSIDE-TOP)))
  (IF (OR (< X (SHEET-INSIDE-LEFT)) ( X (SHEET-INSIDE-RIGHT))
          (< Y (SHEET-INSIDE-TOP)) ( Y (SHEET-INSIDE-BOTTOM)))
      0
    (PREPARE-SHEET (SELF)
      (AR-2-REVERSE SCREEN-ARRAY X Y))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-POINT) (X Y &OPTIONAL (ALU CHAR-ALUF) (VALUE -1))
  (SETQ X (+ X (SHEET-INSIDE-LEFT)) Y (+ Y (SHEET-INSIDE-TOP)))
  (OR (< X (SHEET-INSIDE-LEFT)) ( X (SHEET-INSIDE-RIGHT))
      (< Y (SHEET-INSIDE-TOP)) ( Y (SHEET-INSIDE-BOTTOM))
      (PREPARE-SHEET (SELF)
         (AS-2-REVERSE (BOOLE ALU VALUE (AR-2-REVERSE SCREEN-ARRAY X Y))
                       SCREEN-ARRAY X Y))))

;; Copied from LAD: RELEASE-3.WINDOW; GRAPHICS.LISP#9 on 2-Oct-86 04:21:43
(DEFUN DRAW-LINE-CLIP-VISIBILITY (POINT-X POINT-Y)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (LOGIOR (COND ((< POINT-X (SHEET-INSIDE-LEFT)) 1)
                (( POINT-X (SHEET-INSIDE-RIGHT)) 2)
                (T 0))
          (COND ((< POINT-Y (SHEET-INSIDE-TOP)) 4)
                (( POINT-Y (SHEET-INSIDE-BOTTOM)) 8)
                (T 0))))

;; Copied from LAD: RELEASE-3.WINDOW; GRAPHICS.LISP#9 on 2-Oct-86 04:21:44
(DEFMETHOD (GRAPHICS-MIXIN :CLIP) (FROM-X FROM-Y TO-X TO-Y)
  (SETQ FROM-X (+ FROM-X (SHEET-INSIDE-LEFT))
        FROM-Y (+ FROM-Y (SHEET-INSIDE-TOP))
        TO-X (+ TO-X (SHEET-INSIDE-LEFT))
        TO-Y (+ TO-Y (SHEET-INSIDE-TOP)))
  (DO ((FROM-VISIBILITY (DRAW-LINE-CLIP-VISIBILITY FROM-X FROM-Y)
                        (DRAW-LINE-CLIP-VISIBILITY FROM-X FROM-Y))
       (TO-VISIBILITY (DRAW-LINE-CLIP-VISIBILITY TO-X TO-Y))
       (EXCHANGED NIL))
      ;;When completely visible, draw the line
      ((AND (ZEROP FROM-VISIBILITY) (ZEROP TO-VISIBILITY))
       (AND EXCHANGED (PSETQ FROM-X TO-X TO-X FROM-X FROM-Y TO-Y TO-Y FROM-Y))
       (VALUES (- FROM-X (SHEET-INSIDE-LEFT))
               (- FROM-Y (SHEET-INSIDE-TOP))
               (- TO-X (SHEET-INSIDE-LEFT))
               (- TO-Y (SHEET-INSIDE-TOP))))
    ;;If all off the screen, dont draw anything
    (OR (ZEROP (LOGAND FROM-VISIBILITY TO-VISIBILITY)) (RETURN (VALUES NIL NIL NIL NIL)))
    ;;Exchange points to try to make to point visible
    (AND (ZEROP FROM-VISIBILITY)
         (PSETQ FROM-X TO-X TO-X FROM-X FROM-Y TO-Y TO-Y FROM-Y
                FROM-VISIBILITY TO-VISIBILITY TO-VISIBILITY FROM-VISIBILITY
                EXCHANGED (NOT EXCHANGED)))
    ;;If TO-X = FROM-X then FROM-VISIBILITY = 0, 4 or 8 so there is no danger
    ;; of divide by zero in the next "Push"
    (COND ((LDB-TEST #o0001 FROM-VISIBILITY)    ;Push toward left edge
           (SETQ FROM-Y (+ FROM-Y (TRUNCATE (* (- TO-Y FROM-Y) (- (SHEET-INSIDE-LEFT) FROM-X))
                                            (- TO-X FROM-X)))
                 FROM-X (SHEET-INSIDE-LEFT)))
          ((LDB-TEST #o0101 FROM-VISIBILITY)    ;Push toward right edge
           (SETQ FROM-Y (+ FROM-Y (TRUNCATE (* (- TO-Y FROM-Y) (- (SHEET-INSIDE-RIGHT) FROM-X 1))
                                            (- TO-X FROM-X)))
                 FROM-X (1- (SHEET-INSIDE-RIGHT)))))
    (COND ((LDB-TEST #o0201 FROM-VISIBILITY)    ;Push toward top
           ;;It is possible that TO-Y = FROM-Y at this point because of the effects of
           ;; the last "Push", but in that case TO-X is probably equal to FROM-X as well
           ;; (or at least close to it) so we needn't draw anything:
           (AND (= TO-Y FROM-Y) (RETURN (VALUES NIL NIL NIL NIL)))
           (SETQ FROM-X (+ FROM-X (TRUNCATE (* (- TO-X FROM-X) (- (SHEET-INSIDE-TOP) FROM-Y))
                                            (- TO-Y FROM-Y)))
                 FROM-Y (SHEET-INSIDE-TOP)))
          ((LDB-TEST #o0301 FROM-VISIBILITY)    ;Push toward bottom
           ;; Same:
           (AND (= TO-Y FROM-Y) (RETURN (VALUES NIL NIL NIL NIL)))
           (SETQ FROM-X (+ FROM-X (TRUNCATE (* (- TO-X FROM-X) (- (SHEET-INSIDE-BOTTOM) FROM-Y 1))
                                            (- TO-Y FROM-Y)))
                 FROM-Y (1- (SHEET-INSIDE-BOTTOM)))))))

;; Copied from LAD: RELEASE-3.WINDOW; GRAPHICS.LISP#9 on 2-Oct-86 04:21:44
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-LINE) (FROM-X FROM-Y TO-X TO-Y
                                        &OPTIONAL (ALU CHAR-ALUF) (DRAW-END-POINT T))
  (MULTIPLE-VALUE-SETQ (FROM-X FROM-Y TO-X TO-Y)
    (SEND SELF :CLIP FROM-X FROM-Y TO-X TO-Y))
  (IF FROM-X
      (PREPARE-SHEET (SELF)
        (%DRAW-LINE (+ FROM-X (SHEET-INSIDE-LEFT))
                    (+ FROM-Y (SHEET-INSIDE-TOP))
                    (+ TO-X (SHEET-INSIDE-LEFT))
                    (+ TO-Y (SHEET-INSIDE-TOP))
                    ALU DRAW-END-POINT SELF))))

;;; This never draws any end points, thus it is good for making closed polygons.
;;; Calls the :DRAW-LINE method to do the clipping.
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-LINES) (ALU X1 Y1 &REST END-XS-AND-YS)
  (DO ((X2) (Y2) (METH (GET-HANDLER-FOR SELF :DRAW-LINE))) ((NULL END-XS-AND-YS))
    (SETQ X2 (CAR END-XS-AND-YS)
          Y2 (CADR END-XS-AND-YS)
          END-XS-AND-YS (CDDR END-XS-AND-YS))
    (FUNCALL METH NIL X1 Y1 X2 Y2 ALU NIL)
    (SETQ X1 X2
          Y1 Y2)))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-DASHED-LINE)
           (X0 Y0 X1 Y1 &OPTIONAL (ALU CHAR-ALUF)
            (DASH-SPACING 20.) SPACE-LITERALLY-P (OFFSET 0)
            (DASH-LENGTH (FLOOR DASH-SPACING 2)))
  (LET (N-DASHES DISTANCE
        (REAL-DASH-SPACING DASH-SPACING)
        (REAL-DASH-LENGTH DASH-LENGTH)
        (METH (GET-HANDLER-FOR SELF ':DRAW-LINE)))
    (SETQ DISTANCE (SQRT (SMALL-FLOAT (+ (^ (- X1 X0) 2) (^ (- Y1 Y0)  2)))))
    (IF SPACE-LITERALLY-P
        ;; Get number of dashes of specified size that will fit.
        (SETQ N-DASHES
              (FLOOR (+ DISTANCE (- DASH-SPACING DASH-LENGTH)) DASH-SPACING))
      ;; Get approximate number of dashes that will fit,
      ;; then change spacing to make them fit exactly.
      (SETQ N-DASHES (ROUND (+ DISTANCE (- DASH-SPACING DASH-LENGTH)) DASH-SPACING))
      (IF (= N-DASHES 1)
          (SETQ REAL-DASH-SPACING DISTANCE
                REAL-DASH-LENGTH (- DISTANCE OFFSET OFFSET))
        (SETQ REAL-DASH-SPACING
              (// (- DISTANCE OFFSET OFFSET DASH-LENGTH) (1- N-DASHES)))))
    (LET ((X (+ X0 (* OFFSET (// (- X1 X0) DISTANCE))))
          (Y (+ Y0 (* OFFSET (// (- Y1 Y0) DISTANCE))))
          (DX (* REAL-DASH-LENGTH (// (- X1 X0) DISTANCE)))
          (DY (* REAL-DASH-LENGTH (// (- Y1 Y0) DISTANCE)))
          (DX2 (* REAL-DASH-SPACING (// (- X1 X0) DISTANCE)))
          (DY2 (* REAL-DASH-SPACING (// (- Y1 Y0) DISTANCE))))
      ;the modifications (mainly from the draw-line method)
      (let ((il (sheet-inside-left))
            (it (sheet-inside-top)))
        (let ((from-x (+ x0 il)) (from-y (+ y0 it))
              (to-x (+ x1 il)) (to-y (+ y1 it)))
          (let ((from-visibility (draw-line-clip-visibility from-x from-y))
                (to-visibility (draw-line-clip-visibility to-x to-y)))
            (cond ;;when completely visible, draw the line internally
              ((and (zerop from-visibility) (zerop to-visibility))
               (prepare-sheet (self)
                 (let ((x (+ x il))
                       (y (+ y it)))
                   (dotimes (i n-dashes)
                     (%draw-line (fixr x) (fixr y) (fixr (+ x dx)) (fixr (+ y dy))
                                 alu (< (1+ i) n-dashes) self)
                     (incf x dx2)
                     (incf y dy2)))))           ;        suspect
              ;;if all off the screen, dont draw anything
              ((not (zerop (logand from-visibility to-visibility))))
              (t ;;otherwise, check for each dash
               (DOTIMES (I N-DASHES)
                 (let ((from-x (fixr (+ x il))) (from-y (fixr (+ y it)))
                       (to-x (fixr (+ x dx il))) (to-y (fixr (+ y dy it))))
                   (let ((from-visibility (draw-line-clip-visibility from-x from-y))
                         (to-visibility (draw-line-clip-visibility to-x to-y)))
                     (cond ;;when completely visible, draw the dash internally
                       ((and (zerop from-visibility) (zerop to-visibility))
                        (prepare-sheet (self)
                          (%draw-line from-x from-y to-x to-y alu (< (1+ i) n-dashes) self)))
                       ;;if entire dash if off the screen, dont draw anything
                       ((not (zerop (logand from-visibility to-visibility))))
                       (t ;;otherwise draw a partial dash (doing this is slow)
                        (FUNCALL METH ':DRAW-LINE (FIXR X) (FIXR Y)
                                 (FIXR (+ X DX)) (FIXR (+ Y DY))
                                 ALU (< (1+ I) N-DASHES))))))
                 (INCF X DX2)
                 (INCF Y DY2))))))))))

;;; This clips in microcode
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-TRIANGLE) (X1 Y1 X2 Y2 X3 Y3 &OPTIONAL (ALU CHAR-ALUF))
  (PREPARE-SHEET (SELF)
    (%DRAW-TRIANGLE (+ X1 (SHEET-INSIDE-LEFT)) (+ Y1 (SHEET-INSIDE-TOP))
                    (+ X2 (SHEET-INSIDE-LEFT)) (+ Y2 (SHEET-INSIDE-TOP))
                    (+ X3 (SHEET-INSIDE-LEFT)) (+ Y3 (SHEET-INSIDE-TOP))
                    ALU SELF)))

;;; Very special kludgey macro for :DRAW-CIRCLE.
(DEFMACRO DRAW-CLIPPED-POINT (X-FORM Y-FORM)
  `(PROGN
     (SETQ X-VAL ,X-FORM
           Y-VAL ,Y-FORM)
     (OR (< X-VAL IL) ( X-VAL IR)
         (< Y-VAL IT) ( Y-VAL IB)
         (AS-2-REVERSE (BOOLE ALU VALUE (AR-2-REVERSE SCREEN-ARRAY X-VAL Y-VAL))
                       SCREEN-ARRAY X-VAL Y-VAL))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CIRCLE)
           (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF) (VALUE -1))
  (LET* ((IL (SHEET-INSIDE-LEFT))
         (IT (SHEET-INSIDE-TOP))
         (IR (SHEET-INSIDE-RIGHT))
         (IB (SHEET-INSIDE-BOTTOM))
         (CENTER-X (+ CENTER-X IL))
         (CENTER-Y (+ CENTER-Y IT)))
    (PREPARE-SHEET (SELF)
      (DO ((Y 0)
           (X-VAL) (Y-VAL)
           (F 0)                                ; F is just Y squared without any multiplies
           (X RADIUS))
          (NIL)
        (DRAW-CLIPPED-POINT (+ CENTER-X X) (- CENTER-Y Y))
        (DRAW-CLIPPED-POINT (- CENTER-X X) (+ CENTER-Y Y))
        (DRAW-CLIPPED-POINT (+ CENTER-X Y) (+ CENTER-Y X))
        (DRAW-CLIPPED-POINT (- CENTER-X Y) (- CENTER-Y X))
        (SETQ F (+ F Y Y 1) Y (1+ Y))
        (IF ( F X) (SETQ F (- F X X -1) X (- X 1)))
        (IF (> Y X) (RETURN NIL))
        (DRAW-CLIPPED-POINT (+ CENTER-X X) (+ CENTER-Y Y))
        (DRAW-CLIPPED-POINT (- CENTER-X X) (- CENTER-Y Y))
        (DRAW-CLIPPED-POINT (+ CENTER-X Y) (- CENTER-Y X))
        (DRAW-CLIPPED-POINT (- CENTER-X Y) (+ CENTER-Y X))
        (IF (= Y X) (RETURN NIL))))))

;;; faster method by drm@xx
;;; Does not work for values 4. 11. 134. 373.
;;; I don't know why.  The old method below seems to work fine. -JRM
;;; OK, we tested this out and found that this code is measurably
;;; but not significantly faster.  Comment out the bagbiter!
;(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-CIRCLE)
;           (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF))
;  (PREPARE-SHEET (SELF)
;     (DO ((X RADIUS)
;         (Y 0 (1+ Y))
;         (ERROR 0 (+ ERROR Y Y 1))
;         OLD-Y)
;        ((> Y X))
;       (WHEN ( ERROR X)                       ; Will the next chord be shorter?
;        ;; Draw the middle region.
;        (IF (NULL OLD-Y)                       ; First time through, draw one big rectangle.
;            (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X 1) (+ Y Y 1)
;                                           (- CENTER-X X) (- CENTER-Y Y) ALU SELF)
;          ;; Otherwise draw upper & lower rectangles.
;          (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X 1) (- Y OLD-Y)
;                                         (- CENTER-X X) (- CENTER-Y Y) ALU SELF)
;          (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X 1) (- Y OLD-Y)
;                                         (- CENTER-X X) (+ CENTER-Y OLD-Y 1) ALU SELF))
;        (SETQ OLD-Y Y)
;        (AND (= X Y) (RETURN NIL))             ;Finished?
;        ;; Draw the top line.
;        (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1
;                                       (- CENTER-X Y) (+ CENTER-Y X) ALU SELF)
;        ;; Draw the bottom line.
;        (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1
;                                       (- CENTER-X Y) (- CENTER-Y X) ALU SELF)
;        (SETQ ERROR (- ERROR X X -1))
;        (DECF X)))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-CIRCLE)
           (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF))
  (LET ((CENTER-X (+ CENTER-X (SHEET-INSIDE-LEFT)))
        (CENTER-Y (+ CENTER-Y (SHEET-INSIDE-TOP))))
    (PREPARE-SHEET (SELF)
      (DO ((X 0)
           (F 0)                                ; F is just x^2. Don't use multiplication!
           (Y RADIUS))
          ((> X Y))
        (UNLESS (= X Y)
          (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1 (- CENTER-X Y) (+ CENTER-Y X)
                                         ALU SELF)
          (UNLESS (ZEROP X)
            (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1 (- CENTER-X Y) (- CENTER-Y X)
                                           ALU SELF)))
        (SETQ F (+ F X X 1) X (1+ X))
        (WHEN ( F Y)
          (SETQ F (- F Y Y -1) Y (- Y 1))
          (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
                                         (- CENTER-X X -1) (+ CENTER-Y Y 1)
                                         ALU SELF)
          (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
                                         (- CENTER-X X -1) (- CENTER-Y Y 1)
                                         ALU SELF))))))


(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CIRCLE-OCTANT-ARC)
           (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF)
            RIGHT-UP-START RIGHT-UP-END TOP-RIGHT-START TOP-RIGHT-END
            TOP-LEFT-START TOP-LEFT-END LEFT-UP-START LEFT-UP-END
            LEFT-DOWN-START LEFT-DOWN-END BOTTOM-LEFT-START BOTTOM-LEFT-END
            BOTTOM-RIGHT-START BOTTOM-RIGHT-END RIGHT-DOWN-START RIGHT-DOWN-END (VALUE -1))
  "Draw a portion of each octant of a circle.
There is one pair of a -START and a -END argument for each octant,
which controls the portion of that octant which is actually drawn."
  (LET* ((IL (SHEET-INSIDE-LEFT))
         (IT (SHEET-INSIDE-TOP))
         (IR (SHEET-INSIDE-RIGHT))
         (IB (SHEET-INSIDE-BOTTOM))
         (MAX-END (MAX RIGHT-UP-END TOP-LEFT-END LEFT-DOWN-END BOTTOM-RIGHT-END
                       (- (// 3.14159s0 4)
                          (MIN RIGHT-DOWN-END BOTTOM-LEFT-END
                               LEFT-UP-END TOP-RIGHT-END))))
         (CENTER-X (+ CENTER-X IL))
         (CENTER-Y (+ CENTER-Y IT)))
    (IF (NOT (ZEROP RADIUS))
        (PREPARE-SHEET (SELF)
          (DO ((Y 0)
               (X-VAL) (Y-VAL)
               ANGLE
               (F 0)                            ; F is just R squared without any multiplies
               (X RADIUS))
              (NIL)
            (SETQ ANGLE (ATAN2 (SMALL-FLOAT Y) (SMALL-FLOAT X)))
            ;; Octants counter clockwise from an axis
            (IF (AND (< ANGLE RIGHT-UP-END) ( ANGLE RIGHT-UP-START))
                (DRAW-CLIPPED-POINT (+ CENTER-X X) (- CENTER-Y Y)))
            (IF (AND (< ANGLE LEFT-DOWN-END) ( ANGLE LEFT-DOWN-START))
                (DRAW-CLIPPED-POINT (- CENTER-X X) (+ CENTER-Y Y)))
            (IF (AND (< ANGLE BOTTOM-RIGHT-END) ( ANGLE BOTTOM-RIGHT-START))
                (DRAW-CLIPPED-POINT (+ CENTER-X Y) (+ CENTER-Y X)))
            (IF (AND (< ANGLE TOP-LEFT-END) ( ANGLE TOP-LEFT-START))
                (DRAW-CLIPPED-POINT (- CENTER-X Y) (- CENTER-Y X)))
            (IF (> ANGLE MAX-END) (RETURN NIL))
            (SETQ F (+ F Y Y 1) Y (1+ Y))
            (IF ( F X) (SETQ F (- F X X -1) X (- X 1)))
            (IF (> Y X) (RETURN NIL))
            ;; Clockwise
            (SETQ ANGLE (- (// 3.14159s0 4) ANGLE))
            (IF (AND (< ANGLE RIGHT-DOWN-END) ( ANGLE RIGHT-DOWN-START))
                (DRAW-CLIPPED-POINT (+ CENTER-X X) (+ CENTER-Y Y)))
            (IF (AND (< ANGLE LEFT-UP-END) ( ANGLE LEFT-UP-START))
                (DRAW-CLIPPED-POINT (- CENTER-X X) (- CENTER-Y Y)))
            (IF (AND (< ANGLE TOP-RIGHT-END) ( ANGLE TOP-RIGHT-START))
                (DRAW-CLIPPED-POINT (+ CENTER-X Y) (- CENTER-Y X)))
            (IF (AND (< ANGLE BOTTOM-LEFT-END) ( ANGLE BOTTOM-LEFT-START))
                (DRAW-CLIPPED-POINT (- CENTER-X Y) (+ CENTER-Y X)))
            (IF (= Y X) (RETURN NIL)))))))

;Draw a circular arc using the Minsky algorithm (the present version tend to clip
;erroneously near window edges.  Actually, the bug seems to be caused by the macro call
;to DRAW-CLIPPED-POINT, which seems to sometimes "miss" il, it, ir, and ib).
;The ASH's are multiplies or divides by 2 to the appropriate power.
;The internal arithmetic (accumulation) is done with actual figures (fixnums) multiplied
;by 2**10 for added precision.

(defmethod (graphics-mixin :draw-circular-arc)
           (center-x center-y radius start-theta end-theta &optional (alu char-aluf) (VALUE -1)
            &aux (two-pi (* 2 3.14159s0)))
  ;alter theta's to their equivalents between 0 and 2pi.
  (multiple-value (nil start-theta) (floor start-theta two-pi))
  (multiple-value (nil end-theta) (floor end-theta two-pi))
  ;the Ix variables are for the DRAW-CLIPPED-POINT macro
  (let ((il (sheet-inside-left))
        (it (sheet-inside-top))
        (ir (sheet-inside-right))
        (ib (sheet-inside-bottom)))
    (let ((xc (+ center-x il))
          (yc (+ center-y it))
          (dtheta (if (> start-theta end-theta)
                      (- (+ end-theta two-pi) start-theta)
                    (- end-theta start-theta)))
          (rx (round (* (ash radius 10.) (cos start-theta))))
          (ry (- (round (* (ash radius 10.) (sin start-theta))))))
      (prepare-sheet (self)
        (loop ;2**sd is the angle of rotation. The + 2 and use of HAULONG = ceiling of log2
              ;is to make sure that the rotation is smaller than 1 pixel at the circumfrence
              with sd = (- (haulong (+ radius 2)))
              as xr = rx then (+ xr (ash yr sd))
              as yr = ry then (- yr (ash xr sd))
              repeat (ceiling (* dtheta (ash 1 (- sd))))
              do ; 512 causes rounding on the divide
                 (let ((x-val (+ xc (ash (+ xr 512.) -10.)))
                       (y-val (+ yc (ash (+ yr 512.) -10.))))
                   ;substituting the macro definition here seems to fix the bug
                   (or (< x-val il) (>= x-val ir)
                       (< y-val it) (>= y-val ib)
                       (as-2-reverse (boole alu value (ar-2-reverse screen-array x-val y-val))
                                     screen-array x-val y-val))))))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-SECTOR) (CENTER-X CENTER-Y RADIUS THETA-1 THETA-2
                                                    &OPTIONAL (ALU CHAR-ALUF))
  (PREPARE-SHEET (SELF)
    (DO ((Y (- RADIUS) (1+ Y))
         (X 0)
         (U0 0) (U1 0)                          ;Clipped plane 1
         (V0 0) (V1 0)                          ;Clipped plane 2
         (CO-X0 (FIX (* -1000.0 (SIN THETA-1))))
         (CO-Y0 (FIX (*  1000.0 (COS THETA-1))))
         (CO-X1 (FIX (* -1000.0 (SIN THETA-2))))
         (CO-Y1 (FIX (*  1000.0 (COS THETA-2))))
         (FLAG (> (ABS (- THETA-1 THETA-2)) 3.14159))
         (R2 (* RADIUS RADIUS)))
        ((> Y RADIUS))
      (SETQ X (ISQRT (- R2 (* Y Y))))           ;Unclipped line
      (SETQ U0 (- X) U1 X
            V0 (- X) V1 X)                      ;Init clipped lines

      (AND (PLUSP (- (* CO-Y0 Y) (* CO-X0 U1))) ;Clip with first plane
           (SETQ U1 (IF (= 0 CO-X0) 0 (TRUNCATE (* CO-Y0 Y) CO-X0))))
      (AND (PLUSP (- (* CO-Y0 Y) (* CO-X0 U0)))
           (SETQ U0 (IF (= 0 CO-X0) 0 (TRUNCATE (* CO-Y0 Y) CO-X0))))

      (AND (MINUSP (- (* CO-Y1 Y) (* CO-X1 V1)))        ;Clip with second plane
           (SETQ V1 (IF (= 0 CO-X1) 0 (TRUNCATE (* CO-Y1 Y) CO-X1))))
      (AND (MINUSP (- (* CO-Y1 Y) (* CO-X1 V0)))
           (SETQ V0 (IF (= 0 CO-X1) 0 (TRUNCATE (* CO-Y1 Y) CO-X1))))

      ;; Ok, we have two lines, [U0 U1] and [V0 V1].
      ;; If the angle was greater than pi, then draw both of them,
      ;; otherwise draw their intersection
      (COND (FLAG
             (AND (> U1 U0)
                  (SEND SELF :DRAW-LINE
                                (+ CENTER-X U0) (+ CENTER-Y Y)
                                (+ CENTER-X U1) (+ CENTER-Y Y)
                                ALU T))
             (AND (> V1 V0)
                  (SEND SELF :DRAW-LINE
                                (+ CENTER-X V0) (+ CENTER-Y Y)
                                (+ CENTER-X V1) (+ CENTER-Y Y)
                                ALU T)))
            (T                                  ;Compute intersection
             (LET ((LEFT  (MAX U0 V0))
                   (RIGHT (MIN U1 V1)))
               (AND (> RIGHT LEFT)
                    (SEND SELF :DRAW-LINE
                                  (+ CENTER-X LEFT)  (+ CENTER-Y Y)
                                  (+ CENTER-X RIGHT) (+ CENTER-Y Y)
                                  ALU T))))))))

;;; Given an edge and a number of sides, draw something
;;; The sign of N determines which side of the line the figure is drawn on.
;;; If the line is horizontal, the rest of the polygon is in the positive direction
;;; when N is positive.
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-REGULAR-POLYGON) (X1 Y1 X2 Y2 N &OPTIONAL (ALU CHAR-ALUF)
                                                   &AUX THETA)
  (UNLESS (ZEROP N)
    (SETQ THETA (* 3.14159 (1- (// 2.0 N)))
          N (ABS N))
    (PREPARE-SHEET (SELF)
      (DO ((I 2 (1+ I))
           (SIN-THETA (SIN THETA))
           (COS-THETA (COS THETA))
           (X0 X1) (Y0 Y1)
           (X3) (Y3))
          (( I N))
        (SETQ X3 (+ (- (- (* X1 COS-THETA)
                          (* Y1 SIN-THETA))
                       (* X2 (1- COS-THETA)))
                    (* Y2 SIN-THETA))
              Y3 (- (- (+ (* X1 SIN-THETA)
                          (* Y1 COS-THETA))
                       (* X2 SIN-THETA))
                    (* Y2 (1- COS-THETA))))
        (%DRAW-TRIANGLE (+ (SHEET-INSIDE-LEFT) (FIX X0)) (+ (SHEET-INSIDE-TOP) (FIX Y0))
                        (+ (SHEET-INSIDE-LEFT) (FIX X2)) (+ (SHEET-INSIDE-TOP) (FIX Y2))
                        (+ (SHEET-INSIDE-LEFT) (FIX X3)) (+ (SHEET-INSIDE-TOP) (FIX Y3))
                        ALU SELF)
        (SETQ X1 X2 Y1 Y2
              X2 X3 Y2 Y3)))))

;;; Display vectors of points
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CURVE) (PX PY &OPTIONAL END (ALU CHAR-ALUF)
                                         CLOSED-CURVE-P)
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH PX)))
  (LET ((X0)
        (X1 (FIX (AREF PX 0)))
        (Y0)
        (Y1 (FIX (AREF PY 0)))
        (METH (GET-HANDLER-FOR SELF :DRAW-LINE)))
    (DO ((I 1 (1+ I)))
        (( I END))
      (SETQ X0 X1)
      (OR (SETQ X1 (AREF PX I)) (RETURN NIL))
      (SETQ X1 (FIX X1))
      (SETQ Y0 Y1)
      (OR (SETQ Y1 (AREF PY I)) (RETURN NIL))
      (SETQ Y1 (FIX Y1))
      (FUNCALL METH NIL X0 Y0 X1 Y1 ALU NIL))
    (WHEN CLOSED-CURVE-P
      (FUNCALL METH NIL X1 Y1 (FIX (AREF PX 0)) (FIX (AREF PY 0)) ALU NIL))))

;;; Display vectors of points
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CLOSED-CURVE) (PX PY &OPTIONAL END (ALU CHAR-ALUF))
  (SEND SELF :DRAW-CURVE PX PY END ALU T))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-WIDE-CURVE) (PX PY -WIDTH- &OPTIONAL END (ALU CHAR-ALUF)
                                              CLOSED-CURVE-P)
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH PX)))
  (SETQ -WIDTH- (// -WIDTH- 2.0s0))
  (PREPARE-SHEET (SELF)
    (DO ((I 0 (1+ I))
         (X0) (Y0)
         (X1) (Y1)
         (PX1) (PY1)
         (PX2) (PY2)
         (PX3) (PY3)
         (PX4) (PY4)
         EXIT-NEXT-TIME)
        (EXIT-NEXT-TIME)
      (SETQ X0 X1)
      (SETQ Y0 Y1)
      (IF ( I END)
          (SETQ X1 NIL Y1 NIL)
        (SETQ X1 (AREF PX I))
        (SETQ Y1 (AREF PY I)))
      (UNLESS (AND X1 Y1)
        ;; If we have passed the last point, either exit now or close the curve and then exit.
        (IF CLOSED-CURVE-P
            (SETQ X1 (AREF PX 0) Y1 (AREF PY 0)
                  EXIT-NEXT-TIME T)
          (RETURN NIL)))
      (UNLESS (= I 0)
        (LET ((DX (- X1 X0))
              (DY (- Y1 Y0))
              LEN)
          (SETQ LEN (SMALL-FLOAT (SQRT (+ (* DX DX) (* DY DY)))))
          (AND (ZEROP LEN) (= I 1) (SETQ LEN 1))
          (COND ((NOT (ZEROP LEN))
                 (PSETQ DX (// (* -WIDTH- DY) LEN)
                        DY (// (* -WIDTH- DX) LEN))
                 (IF (= I 1)
                     (SETQ PX1 (FIX (- X0 DX)) PY1 (FIX (+ Y0 DY))
                           PX2 (FIX (+ X0 DX)) PY2 (FIX (- Y0 DY)))
                   (SETQ PX1 PX3 PY1 PY3 PX2 PX4 PY2 PY4))
                 (SETQ PX3 (FIX (- X1 DX)) PY3 (FIX (+ Y1 DY))
                       PX4 (FIX (+ X1 DX)) PY4 (FIX (- Y1 DY)))
                 (%DRAW-TRIANGLE (+ (SHEET-INSIDE-LEFT) PX1) (+ (SHEET-INSIDE-TOP) PY1)
                                 (+ (SHEET-INSIDE-LEFT) PX2) (+ (SHEET-INSIDE-TOP) PY2)
                                 (+ (SHEET-INSIDE-LEFT) PX4) (+ (SHEET-INSIDE-TOP) PY4)
                                 ALU SELF)
                 (%DRAW-TRIANGLE (+ (SHEET-INSIDE-LEFT) PX1) (+ (SHEET-INSIDE-TOP) PY1)
                                 (+ (SHEET-INSIDE-LEFT) PX3) (+ (SHEET-INSIDE-TOP) PY3)
                                 (+ (SHEET-INSIDE-LEFT) PX4) (+ (SHEET-INSIDE-TOP) PY4)
                                 ALU SELF))))))))

;;; Cubic splines from Rogers and Adams, "Mathematical Elements
;;; for Computer Graphics".  This began as a translation from
;;; a BASIC program, but has been changed a bit.  The original
;;; program uses a full matrix inversion when the boundary conditions
;;; are cyclic or anti-cyclic, which is inefficient; in this version
;;; the special-case tridiagonal solver is extended to handle the
;;; cyclic and anti-cyclic end conditions.  (Also, the original program
;;; has a bug wherein it neglects to initialize one diagonal of the M matrix.)

(DEFUN SPLINE (PX PY Z &OPTIONAL CX CY (C1 :RELAXED) (C2 C1)
               P1-PRIME-X P1-PRIME-Y PN-PRIME-X PN-PRIME-Y
               &AUX N N-1 N-2 N-3 BX BY L UX UY N1 N2 N3 N4 SIGN
                    (ZUNDERFLOW T) CLEN)
  "Compute cubic splines.  PX and PY are arrays of X-coords and Y-coords.
They describe a sequeuce of points through which a smooth
curve should be drawn.  This program generates Z intermediate
points between each pair of points, returning a sequence of points
in CX and CY that includes the original points with the intermediate
points inserted.  The caller can then plot lines between successive
pairs of points of CX and CY to draw the curve.

The caller may pass in arrays to be filled in with the answers (used as
CX and CY); they should be (+ N (* Z (- N 1))) long.  If NIL is passed,
this function creates the arrays itself.  If they are not long enough,
they are adjusted with ADJUST-ARRAY-SIZE.

The optional argument C1 is the initial end condition, one of
:RELAXED, :CLAMPED, :CYCLIC, or :ANTI-CYCLIC; C2 is the final end
condition, one of :RELAXED or :CLAMPED.  The first defaults to
:RELAXED, and the second defaults to the first.  The second must be
the same as the first if the first is :CYCLIC or :ANTI-CYCLIC.  The
last four arguments are the X and Y values to which the endpoints are
being clamped if the corresponding boundary condition is :CLAMPED.
For cyclic splines that join themselves, the caller must pass the same
point twice, as both the first point and the last point.

P1-PRIME-X, etc., specify the slopes at the two endpoints,
for the sake of :CLAMPED constraints.

Three values are returned: The two arrays CX and CY, and the number
of active elements those arrays."
  (DECLARE (VALUES CX CY NUMBER-OF-POINTS))
  (SETQ N (ARRAY-ACTIVE-LENGTH PX)              ;The number of points
        N-1 (1- N)
        N-2 (1- N-1)
        N-3 (1- N-2))
  (SETQ CLEN (+ N (* N-1 Z)))

  ;; Create the arrays if they were not given them, or redimension them if needed.
  (COND ((NULL CX)
         (SETQ CX (MAKE-ARRAY CLEN)))
        ((< (ARRAY-LENGTH CX) CLEN)
         (SETQ CX (ADJUST-ARRAY-SIZE CX CLEN))))
  (COND ((NULL CY)
         (SETQ CY (MAKE-ARRAY CLEN)))
        ((< (ARRAY-LENGTH CY) CLEN)
         (SETQ CY (ADJUST-ARRAY-SIZE CY CLEN))))

  ;; Set up L to hold the approximate spline segment lengths.
  ;; The Nth element of L holds the distance between the Nth and N+1st
  ;; points of PX,PY.  The last element of L is not used.
  (SETQ L (MAKE-ARRAY N))
  (LOOP FOR J FROM 0 TO N-2
        DO (ASET (SMALL-FLOAT (SQRT (+ (^ (- (AREF PX (1+ J)) (AREF PX J)) 2)
                                       (^ (- (AREF PY (1+ J)) (AREF PY J)) 2))))
                 L J))

  ;; The bulk of the code here is concerned with solving a set of
  ;; simultaneous linear equations, expressed by the matrix equation
  ;; M * U = B.  M is an N by N square matrix, and B and U are N by 1
  ;; column matricies.  U will hold the values of the slope of the curve
  ;; at each point PX, PY.

  ;; The M matrix is tridiagonal for :RELAXED and :CLAMPED end conditions.
  ;; We represent it by storing M(I,I-1) in N1(I), M(I,I) in N2(I), and
  ;; M(I,I+1) in N3(I).  This means N1(0) and N3(N-1) are unused.
  (SETQ N1 (MAKE-ARRAY N)
        N2 (MAKE-ARRAY N)
        N3 (MAKE-ARRAY N))

  ;; These quantities are meaningless, but they get referred to as part
  ;; of array bound conditions; these values just prevent errors from happening.
  (ASET 0.0s0 N1 0)
  (ASET 0.0s0 N3 N-1)

  (COND ((MEMQ C1 '(:CYCLIC :ANTI-CYCLIC))
         ;; With these conditions, the M matrix is not quite tri-diagonal;
         ;; it is initialize with a 1 in the upper-right hand corner, and
         ;; during the solution of the equations the whole right column
         ;; gets non-zero values.  Also, it is only N-1 by N-1!  So the upper
         ;; right corner is M(0, N-2).  N4 represents the N-2 column; element
         ;; M(I,N-2) is stored in N4(I).  The last two elements are not
         ;; used, because N4(N-2) = N2(N-2) and N4(N-3) = N3(N-3).  We also
         ;; set up this handy SIGN variable.
         (SETQ N4 (MAKE-ARRAY (1- N)))
         (SETQ SIGN (IF (EQ C1 :CYCLIC) 1.0s0 -1.0s0)))
        ((NOT (MEMQ C1 '(:RELAXED :CLAMPED)))
         (FERROR NIL "~S is not known spline type" C1)))
  ;; B is just a column vector, represented normally.
  (SETQ BX (MAKE-ARRAY N)
        BY (MAKE-ARRAY N))

  ;; Set up the boundary conditions.
  ;; The 0th row of M and B are determined by the initial boundary conditions,
  ;; and the N-1st row is determined by the final boundary condition.
  ;; Note that the 0th row of M is implemented as the 0th element of N2, N3,
  ;; and sometimes N4; N1(0) is not used.  A similar thing is true of the
  ;; N-1st row.
  (SELECTQ C1
    (:CLAMPED
       (ASET 1.0s0 N2 0)
       (ASET 0.0s0 N3 0)
       (ASET P1-PRIME-X BX 0)
       (ASET P1-PRIME-Y BY 0))
    (:RELAXED
       (ASET 1.0s0 N2 0)
       (ASET 0.5s0 N3 0)
       (LET ((TEM (// 3.0s0 (* 2.0s0 (AREF L 0)))))
         (ASET (* TEM (- (AREF PX 1) (AREF PX 0))) BX 0)
         (ASET (* TEM (- (AREF PY 1) (AREF PY 0))) BY 0)))
    ((:CYCLIC :ANTI-CYCLIC)
       (LET ((S3 (// (AREF L N-2) (AREF L 0))))
         (ASET (+ 2.0s0 (* S3 2.0s0)) N2 0)
         (ASET S3 N3 0)
         (ASET SIGN N4 0)
         (LET ((TEM (// 3.0s0 (AREF L 0))))
           (ASET (* TEM (+ (* S3 (- (AREF PX 1) (AREF PX 0)))
                           (* SIGN (// (- (AREF PX N-1) (AREF PX N-2)) S3))))
                 BX 0)
           (ASET (* TEM (+ (* S3 (- (AREF PY 1) (AREF PY 0)))
                           (* SIGN (// (- (AREF PY N-1) (AREF PY N-2)) S3))))
                 BY 0)))))
  (SELECTQ C2
    (:CLAMPED
       (ASET 0.0s0 N1 N-1)
       (ASET 1.0s0 N2 N-1)
       (ASET PN-PRIME-X BX N-1)
       (ASET PN-PRIME-Y BY N-1))
    (:RELAXED
       (ASET 2.0s0 N1 N-1)
       (ASET 4.0s0 N2 N-1)
       (LET ((TEM (// 6.0s0 (AREF L N-2))))
         (ASET (* TEM (- (AREF PX N-1) (AREF PX N-2))) BX N-1)
         (ASET (* TEM (- (AREF PY N-1) (AREF PY N-2))) BY N-1)))
    ;; Note: there are no final end conditions for :CYCLIC and :ANTI-CYCLIC,
    ;; since they are the same at each end.  The M matrix has no N-1st row,
    ;; either, as it is smaller by one row and one column.
    )

  ;; Now fill in the insides of M and B arrays.
  (LOOP FOR J FROM 1 TO N-2
        AS L0 := (AREF L 0) THEN L1
        AS L1 := (AREF L 1) THEN (AREF L J)
        AS PX0 := (AREF PX 0) THEN PX1
        AS PX1 := (AREF PX 1) THEN PX2
        AS PX2 := (AREF PX (1+ J))
        AS PY0 := (AREF PY 0) THEN PY1
        AS PY1 := (AREF PY 1) THEN PY2
        AS PY2 := (AREF PY (1+ J))
        DO (ASET L1 N1 J)
           (ASET (* 2 (+ L0 L1)) N2 J)
           (ASET L0 N3 J)
           (IF N4 (ASET 0.0s0 N4 J))
           (ASET (// (* 3.0s0 (+ (* (^ L0 2) (- PX2 PX1)) (* (^ L1 2) (- PX1 PX0))))
                     (* L0 L1)) BX J)
           (ASET (// (* 3.0s0 (+ (* (^ L0 2) (- PY2 PY1)) (* (^ L1 2) (- PY1 PY0))))
                     (* L0 L1)) BY J))

  ;; Now that we have the matricies filled in, we solve the equations.
  ;; We use Gaussian elimination, with a special version that takes
  ;; advantage of the sparsity of this tridiagonal or almost-tridiagonal
  ;; matrix to run in time O(n) instead of O(n**3).  No pivoting is used,
  ;; because for any real dat (not all zeroes, for example) the matrix
  ;; is both irreducible and diagonally-dominant, and therefore pivoting
  ;; is not needed (Forsythe and Moler, p. 117,  exercise 23.10).
  ;; The first step is to make the matrix upper-triangular, by making all of
  ;; N1 be zero.
  (LET ((Q (AREF N2 0)))                                ;Normalize row 0.
    (ASET (// (AREF N3 0) Q) N3 0)
    (IF N4 (ASET (// (AREF N4 0) Q) N4 0))
    (ASET (// (AREF BX 0) Q) BX 0)
    (ASET (// (AREF BY 0) Q) BY 0))
  (LOOP FOR I FROM 1 TO (IF (NULL N4) N-1 N-2)
        AS N1I := (AREF N1 I)
        WHEN (NOT (ZEROP N1I))                          ;If it is zero already, OK.
        DO (LET ((D (// 1.0s0 N1I)))
             ;; D = M(I-1, I-1) / M(I, I-1)  so multiply row I
             ;;   by D and subtract row I-1 from row I.
             (ASET (- (* D (AREF N2 I)) (AREF N3 (1- I))) N2 I)
             (ASET (* D (AREF N3 I)) N3 I) ; Uses N3(N-1), a garbage element.
             (COND (N4
                    (ASET (- (* D (AREF N4 I)) (AREF N4 (1- I))) N4 I)
                    (IF (= I N-3)
                        ;; In this case, N4(N-4) is above N3(N-3), so
                        ;; it must be subtracted out.
                        (ASET (- (AREF N3 I) (AREF N4 (1- I))) N3 I))))
             (ASET (- (* D (AREF BX I)) (AREF BX (1- I))) BX I)
             (ASET (- (* D (AREF BY I)) (AREF BY (1- I))) BY I)
             )
        ;; Next normalize, by dividing row I through by M(I,I).
        ;; This leaves the center diagonal all 1.0s0, which the
        ;; back-solver in R&A doesn't take advantage of.
           (LET ((Q (AREF N2 I)))
             (ASET (// (AREF N3 I) Q) N3 I)
             (IF N4 (ASET (// (AREF N4 I) Q) N4 I))
             (ASET (// (AREF BX I) Q) BX I)
             (ASET (// (AREF BY I) Q) BY I)))

  ;; Create the arrays to hold the answers.
  (SETQ UX (MAKE-ARRAY N)               ;Tangent vector matrix
        UY (MAKE-ARRAY N))

  ;; Backsolve the upper-triangular matrix.
  (COND ((NOT N4)
         ;; Simpler version if there is no N4.
         (ASET (AREF BX N-1) UX N-1)
         (ASET (AREF BY N-1) UY N-1)
         (LOOP FOR J FROM N-2 DOWNTO 0
               DO (LET ((N3J (AREF N3 J)))
                    (ASET (- (AREF BX J) (* N3J (AREF UX (1+ J)))) UX J)
                    (ASET (- (AREF BY J) (* N3J (AREF UY (1+ J)))) UY J))))
        (T
         ;; Hairier version with N4.
         (LET ((UXN-2 (AREF BX N-2))
               (UYN-2 (AREF BY N-2)))
           (ASET UXN-2 UX N-2)
           (ASET UYN-2 UY N-2)
           (ASET (- (AREF BX N-3) (* (AREF N3 N-3) UXN-2)) UX N-3)
           (ASET (- (AREF BY N-3) (* (AREF N3 N-3) UYN-2)) UY N-3)
           (LOOP FOR J FROM (1- N-3) DOWNTO 0
                 DO (LET ((N3J (AREF N3 J))
                          (N4J (AREF N4 J)))
                      (ASET (- (AREF BX J)
                               (* N3J (AREF UX (1+ J)))
                               (* N4J UXN-2))
                            UX J)
                      (ASET (- (AREF BY J)
                               (* N3J (AREF UY (1+ J)))
                               (* N4J UYN-2))
                            UY J))))
         (ASET (* SIGN (AREF UX 0)) UX N-1)
         (ASET (* SIGN (AREF UY 0)) UY N-1)))

  (MULTIPLE-VALUE (CX CY)
    (CURGEN N PX PY Z CX CY L UX UY))           ; Generate it

  (VALUES CX CY CLEN))

;;; Generate the spline curve points.
;;; This is a separate function because if it got merged, there would
;;; be too many local variables.
(DEFUN CURGEN (N PX PY Z CX CY L UX UY)
  (LOOP WITH I := 0
        FOR J FROM 0 TO (- N 2)
        FOR LEN := (AREF L J)
        FOR LEN^2 := (^ LEN 2)
        FOR LEN^3 := (* LEN^2 LEN)
        FOR FX1 := (AREF PX J)
        FOR FX2 := (AREF UX J)
        FOR TEMX := (- (AREF PX (1+ J)) FX1)
        FOR TEMX1 := (+ (AREF UX (1+ J)) FX2)
        FOR FX3 := (- (* (// 3.0s0 LEN^2) TEMX) (// (+ TEMX1 FX2) LEN))
        FOR FX4 := (+ (* (// -2.0s0 LEN^3) TEMX) (// TEMX1 LEN^2))
        FOR FY1 := (AREF PY J)
        FOR FY2 := (AREF UY J)
        FOR TEMY := (- (AREF PY (1+ J)) FY1)
        FOR TEMY1 := (+ (AREF UY (1+ J)) FY2)
        FOR FY3 := (- (* (// 3.0s0 LEN^2) TEMY) (// (+ TEMY1 FY2) LEN))
        FOR FY4 := (+ (* (// -2.0s0 LEN^3) TEMY) (// TEMY1 LEN^2))
        DO (LOOP FOR K FROM 0 TO Z
                 FOR X FROM 0 BY (// LEN (1+ Z))
                 DO (ASET (+ FX1 (* FX2 X) (* FX3 (^ X 2)) (* FX4 (^ X 3))) CX I)
                    (ASET (+ FY1 (* FY2 X) (* FY3 (^ X 2)) (* FY4 (^ X 3))) CY I)
                    (SETQ I (1+ I)))
        FINALLY (PROGN (ASET (SMALL-FLOAT (AREF PX (1- N))) CX I)
                       (ASET (SMALL-FLOAT (AREF PY (1- N))) CY I)
                       (RETURN (VALUES CX CY)))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CUBIC-SPLINE)
           (PX PY Z &OPTIONAL CURVE-WIDTH ALU (C1 :RELAXED) (C2 C1)
                       P1-PRIME-X P1-PRIME-Y PN-PRIME-X PN-PRIME-Y)
  (IF (NULL CURVE-WIDTH)
      (SETQ CURVE-WIDTH 1))
  (IF (NULL ALU)
      (SETQ ALU CHAR-ALUF))
  (MULTIPLE-VALUE-BIND (CX CY I)
      (SPLINE PX PY Z NIL NIL C1 C2 P1-PRIME-X P1-PRIME-Y PN-PRIME-X PN-PRIME-Y)
    (IF (= CURVE-WIDTH 1)
        (SEND SELF :DRAW-CURVE CX CY I ALU)
      (SEND SELF :DRAW-WIDE-CURVE CX CY CURVE-WIDTH I ALU))))

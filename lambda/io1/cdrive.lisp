;;-*- Mode:LISP; Package:USER; Base:8 -*-

(defvar ctest-unibus-loc 764126)        ;General I/O locn of IOB
(defvar ctest-last-output 0)            ;remember last output since cant read it back.
(defvar ctest-probes-downp nil)
(defvar ctest-slow-speed 2000)          ;use this to assure no steps dropped
(defvar ctest-ramp-speed 200)           ;ramp up to this for normal movements.
(defvar ctest-use-ramping t)

(DEFVAR MILLS-TO-STEPS NIL)

(DECLARE (SPECIAL CTEST-BOARD-TYPE))

(DECLARE (SPECIAL MPG216-GXOFST))

(DECLARE (SPECIAL LEFT-PROBE RIGHT-PROBE))

(DEFCLASS PROBE-CLASS OBJECT-CLASS (NAME DOWN-P OTHER-PROBE
                                         XPOS-STEPS YPOS-STEPS
                                         CALIBRATOR
                                         XCE YCE)) ;"CUMULATIVE" ERRORS (* 100)


;GENERALIZED CALIBRATOR

;INITIALIZE
;SET-CALIBRATION-POINT (X, Y, DX, DY, BOARD-NUM)
;COMPUTE-CORRECTION (X, Y) RETURNS DX DY
;FLUSH-CALIBRATION-FOR-BOARD (BOARD-NUM)

;  TO COMPUTE THE DESIRED CORRECTION AT XG, YG WE PROCEED AS FOLLOWS:
;FIRST, DETERMINE THE INFLUENCE OF EACH CALIBRATION POINT AT XG, YG.

;   INFLUENCE IS INVERSELY PROPORTIONAL TO DISTANCE-SQUARED.
;   INFLUENCE CAN BE FURTHER REDUCED BY MASKING, IE, IF THERE IS A CALIBRATION
; POINT WITH NEARLY THE SAME RELATIVE BEARING AND CLOSER TO XG, YG, THE INFLUENCE
; OF THE FARTHER AWAY CALIBRATION POINT IS REDUCED (OR EVEN ELIMINATED ENTIRELY
; IF THE RELATIVE BEARING IS EXACTLY THE SAME.)  WE IMPLEMENT THIS BY A COMPUTATION
; BASED ON THE LENGTHS OF THE SIDES OF THE TRIANGLE FORMED BY THE TWO CALIBRATION POINTS
; AND THE DESIRED POINT.

;   OBTAIN THE WEIGHT OF EACH CALIBRATION POINT AT XG,YG BY DIVIDING ITS INFLUENCE BY THE
;SUM OF ALL INFLUENCES AT XG,YG.

;   OBTAIN THE CORRECTION FACTOR BY MULTIPLYING THE CORRECTION OF EACH CALIBRATION POINT
;TIMES ITS WEIGHT, AND SUMMING THE RESULTS.

(DEFCLASS CALIBRATOR-CLASS OBJECT-CLASS (NAME CALIBRATION-POINTS))

(DEFMETHOD (CALIBRATOR-CLASS :INITIALIZE) ()
        (SETQ CALIBRATION-POINTS NIL))

(DEFMETHOD (CALIBRATOR-CLASS :FLUSH-CALIBRATION-FOR-BOARD) (BOARD-NUM)
  (DOLIST (PT CALIBRATION-POINTS)
    (COND ((= (NTH 6 PT) BOARD-NUM)
           (SETQ CALIBRATION-POINTS (DELQ PT CALIBRATION-POINTS))))))

(DEFMETHOD (CALIBRATOR-CLASS :SET-CALIBRATION-POINT) (X Y DX DY BOARD-NUM)
        (SETQ X (SMALL-FLOAT X) Y (SMALL-FLOAT Y))
        (SETQ CALIBRATION-POINTS (CONS (LIST NIL NIL     ;FILL IN WITH DIST, INFLUENCE
                                             X Y
                                             (SMALL-FLOAT DX) (SMALL-FLOAT DY)
                                             BOARD-NUM)
                                       CALIBRATION-POINTS)))

(DEFMETHOD (CALIBRATOR-CLASS :COMPUTE-CORRECTION) (X Y)
   (PROG (TINF INF P P-DIST-SQ P-DIST NP NP-DIST-SQ NP-DIST P-NP-DIST-SQ P-NP-DIST
               P-NP-XD P-NP-YD TEM XD YD)
        (SETQ X (SMALL-FLOAT X) Y (SMALL-FLOAT Y))
        (DOLIST (C CALIBRATION-POINTS)
          (SETQ TEM (CDDR C))
          (SETQ XD (- X (CAR TEM))
                YD (- Y (CADR TEM)))
          (RPLACA C (SETQ P-DIST (+ (* XD XD) (* YD YD))))   ;DIST SQUARED
          (COND ((< P-DIST .01)
                 (SETQ XD (CADDR TEM) YD (CADDDR TEM))   ;THIS ONE EXACTLY
                 (GO E2))))
        (SETQ CALIBRATION-POINTS (SORTCAR CALIBRATION-POINTS (FUNCTION <)))

        (SETQ TINF 0)   ;TOTAL INFLUENCE
        (SETQ P CALIBRATION-POINTS)
    L   (COND ((NULL P) (GO E)))
        (SETQ INF (// 1S0 (SETQ P-DIST-SQ (CAAR P))))   ;BASIC INFLUENCE INVERSELY
                                                     ;  PROPORTIONAL TO DIST SQUARED
        (SETQ P-DIST NIL)
        (SETQ XD (- X (CADDAR P)) YD (- Y (CADDDR (CAR P))))
        (SETQ NP CALIBRATION-POINTS)
    L1  (COND ((EQ NP P) (GO E1)))     ;EXAMINED ALL CLOSER
        (SETQ NP-DIST-SQ (CAAR NP))     ;DIST-SQUARED HE IS.
;NOW COMPUTE DIST SQUARED OF THIRD SIDE OF TRIANGLE, BETWEEN THE TWO CALIB POINTS.
        (SETQ P-NP-XD (- (CADDAR P) (CADDAR NP))
              P-NP-YD (- (CADDDR (CAR P)) (CADDDR (CAR NP))))
        (SETQ P-NP-DIST-SQ (+ (* P-NP-XD P-NP-XD) (* P-NP-YD P-NP-YD)))
;IF SHADOWING IS GOING ON, THIS THIRD SIDE WILL BE NEARLY EQUAL TO (P-DIST - NP-DIST),
;OTHERWISE IT WILL BE LONGER.
        (COND ((> P-NP-DIST-SQ P-DIST-SQ)
               (GO X1)))        ;CLEARLY NO SHADOWING
        (COND ((NULL P-DIST)
               (SETQ P-DIST (SQRT P-DIST-SQ))))
        (SETQ NP-DIST (SQRT NP-DIST-SQ) P-NP-DIST (SQRT P-NP-DIST-SQ))
;       (FORMAT T "~%P-DIST ~S, NP-DIST ~S, P-NP-DIST ~S" P-DIST NP-DIST P-NP-DIST)
        (SETQ INF (* INF (MIN 1.0S0 (// (- P-NP-DIST (- P-DIST NP-DIST)) P-NP-DIST))))
    X1  (SETQ NP (CDR NP))
        (GO L1)
    E1  (SETQ TINF (+ TINF INF))
        (RPLACA (CDAR P) INF)
        (SETQ P (CDR P))
        (GO L)
    E   (SETQ XD 0S0 YD 0S0)
;       (FORMAT T "~%TINF=~S, ~S" TINF CALIBRATION-POINTS)
        (SETQ P CALIBRATION-POINTS)
    L2  (COND ((NULL P) (GO E2)))
        (SETQ TEM (CDAR P))
        (SETQ XD (+ XD (* (CADDDR TEM)
                          (// (CAR TEM) TINF))))
        (SETQ YD (+ YD (* (CADDDR (CDR TEM))
                          (// (CAR TEM) TINF))))
        (SETQ P (CDR P))
        (GO L2)
    E2  (SETQ XD (FIX XD) YD (FIX YD))
;       (FORMAT T "~%Correction at X=~S, Y=~S; ~S ~S" X Y XD YD)
        (RETURN XD YD)
))

(defun ctest-initialize-array (area array-type init-value-list)
      (prog (arrayp)
            (setq arrayp (make-array (length init-value-list)
                                     ':type array-type
                                     ':area area))
            (dotimes (i (length init-value-list))
              (as-1 (car init-value-list) arrayp i)
              (setq init-value-list (cdr init-value-list)))
            (return arrayp)))

(defvar %%ctest-probes-com 1602)
(defvar %%ctest-probe-drive 1402)
(defvar %%ctest-probe-sense 1402)
(defvar %%ctest-vcc-sense 1301)   ;sense point, clipped to VCC

(defvar ctest-motor-fields (ctest-initialize-array nil art-q '( 0002 0202 0402 0602)))
(defvar ctest-left-probe-codes '(
   (y-plus . (0 -1)) (y-minus . (0 1)) (x-plus . (0 -1 1 1)) (x-minus . (0 1 1 -1))))
(defvar ctest-right-probe-codes '(
   (y-plus . (2 1)) (y-minus . (2 -1)) (x-plus . (3 1 2 -1)) (x-minus . (3 -1 2 1))))

(defun ctest-out (n)
  (%unibus-write ctest-unibus-loc (setq ctest-last-output n)))

(defun ctest-read nil
  (%unibus-read ctest-unibus-loc))

(defun ctest-init nil
  (ctest-out 3777))

(defun ctest-buzz (arg &optional (delay 10000))
  (do () (())
     (ctest-out arg)
     (dotimes (c delay)
       )))

(defun ctest-convert-code (probe dir)
  (cdr (assq dir (cond ((eq probe 'left) ctest-left-probe-codes)
                       ((eq probe 'right) ctest-right-probe-codes)))))

(defun ctest-step-motor (mot dir current-state)
  (let ((bp (ar-1 ctest-motor-fields mot)))
    (let ((cstate (ldb bp current-state)))
      (dpb (+ dir cstate)
           bp
           current-state))))

(defun ctest-step-list (steps rate &rest lst)
  (dotimes (s steps)
    (prog (state tem)
          (setq state ctest-last-output
                tem lst)
       l  (cond ((null tem)
                 (dotimes (r rate) )
                 (return (ctest-out state))))
          (setq state (ctest-step-motor (car tem) (cadr tem) state))
          (setq tem (cddr tem))
          (go l))))

(defun ctest-zot (lst)
  (prog (state)
        (setq state ctest-last-output)
     l  (cond ((null lst)
               (return (ctest-out state))))
        (setq state (ctest-step-motor (car lst) (cadr lst) state))
        (setq lst (cddr lst))
        (go l)))

(defun ctest-move (probe dir dist spd
                  &optional (base-speed ctest-slow-speed)  ;this fast should never lose..
                            (ramp-rate 30))     ;counts per tick speedup or slowdown
 (cond ((null ctest-use-ramping)
        (lexpr-funcall (function ctest-step-list) dist spd (ctest-convert-code probe dir)))
       (t
  (prog (ramp-steps cruise-steps code rate)
        (setq code (ctest-convert-code probe dir))
        (setq ramp-steps (min (truncate (- base-speed spd) ramp-rate)
                              (truncate dist 2)))
        (setq cruise-steps (- dist (* 2 ramp-steps)))
        (setq rate base-speed)
        (dotimes (c ramp-steps)
          (ctest-zot code)
          (dotimes (r rate) )
          (setq rate (- rate ramp-rate)))
        (dotimes (c cruise-steps)
          (ctest-zot code)
          (dotimes (r spd) ))
        (dotimes (c ramp-steps)
          (ctest-zot code)
          (dotimes (r rate) )
          (setq rate (+ rate ramp-rate)))))))

(defun ctest-move-keyed (probe dir &aux code)
  (setq code (ctest-convert-code probe dir))
  (do () (())
    (cond ((kbd-tyi-no-hang)
           (ctest-zot code)))))

(defun ctest-probes (&optional dir)
  (cond ((eq dir 'down)
         (ctest-out (dpb -1 %%ctest-probes-com ctest-last-output))
         (setq ctest-probes-downp t))
        ((eq dir 'up)
         (ctest-out (dpb 0 %%ctest-probes-com ctest-last-output))
         (setq ctest-probes-downp nil))
        (ctest-probes-downp
          (ctest-probes 'up))
        (t (ctest-probes 'down))))

(declare (special ctest-retry-count))

(defun ctest-test-cont (&optional (signal-name nil)
                                  (retry-flag nil) &aux tem (winp 'win) readback)
  (do ((com '(0 1 2 3) (cdr com))
       (good-reply (cond ((equal signal-name "GND") '(4 4 4 4))
                         ((equal signal-name "+5.0V") '(0 0 0 7))
                         (t '(4 4 4 7)))
                   (cdr good-reply)))
      ((null com) winp)
    (ctest-out (dpb (car com) %%ctest-probe-drive ctest-last-output))
  l (setq readback (ctest-read))
    (cond ((not (= (setq tem (dpb (ldb %%ctest-vcc-sense readback)
                                  0201
                                  (ldb %%ctest-probe-sense readback)))
                   (car good-reply)))
           (process-sleep 60.)  ;maybe probes need to settle
           (setq readback (ctest-read))
           (cond ((not (= (setq tem (dpb (ldb %%ctest-vcc-sense readback)
                                         0201
                                         (ldb %%ctest-probe-sense readback)))
                          (car good-reply)))
                  (cond ((and retry-flag
                              (< ctest-retry-count 2))
                         (setq ctest-retry-count (1+ ctest-retry-count))
                         (COMMENT (FORMAT t "~%Probes wrote ~s, expected ~s, read ~s"
                                 (car com)
                                 (car good-reply)
                                 tem))
                         (*throw 'error-restart nil))
                        (t (RETURN NIL)   ;COMMENT THIS OUT IF YOU WANT
                           (cerror t nil nil "~%Probes wrote ~s, expected ~s, read ~s"
                                   (car com)
                                   (car good-reply)
                                   tem)
                           (go l)))
                  (setq winp 'lose)))))))

(COMMENT
(DEFUN KBD-TYI-FULL ()
  SI: (WITHOUT-INTERRUPTS
        (PROG ((LEFT 0) (RIGHT 0) KBD)
              (COND (KBD-BUFFER
                      (SETQ KBD KBD-BUFFER)
                      (SETQ KBD-BUFFER NIL)))
              (COND ((NULL KBD) (RETURN NIL)))
              (COND ((= (LDB 2003 KBD) 1) NIL))   ;DONT KNOW HOW TO WIN WITH NEW KBD
              (COND ((BIT-TEST 4000 KBD) (SETQ LEFT (+ LEFT 1))))       ;LEFT CONTROL
              (COND ((BIT-TEST 2000 KBD) (SETQ RIGHT (+ RIGHT 1))))     ;RIGHT CONTROL
              (COND ((BIT-TEST 20000 KBD) (SETQ LEFT (+ LEFT 2))))      ;LEFT META
              (COND ((BIT-TEST 10000 KBD) (SETQ RIGHT (+ RIGHT 2))))   ;RIGHT META
              (COND ((BIT-TEST 200 KBD) (SETQ LEFT (+ LEFT 4))))        ;LEFT SHIFT
              (COND ((BIT-TEST 100 KBD) (SETQ RIGHT (+ RIGHT 4))))      ;RIGHT SHIFT
              (COND ((BIT-TEST 1000 KBD) (SETQ LEFT (+ LEFT 10))))      ;LEFT TOP
              (COND ((BIT-TEST 400 KBD) (SETQ RIGHT (+ RIGHT 10))))     ;RIGHT TOP
              (RETURN (KBD-CONVERT KBD) LEFT RIGHT))))

;LIKE MOUSE-INPUT, BUT RETURN ONLY WHEN BUTTONS ARE PUSHED DOWN.
(DEFUN MOUSE-INPUT-WHEN-BUTTONS-DOWN-OR-TYI NIL
 si:(PROG (NEW-X NEW-Y X-MODULUS Y-MODULUS DELTA-X DELTA-Y NEW-BUTTONS CHANGED-BUTTONS)
     L  (PROCESS-ALLOW-SCHEDULE)
        (PROCESS-WAIT "MOUSE"
                      #'(LAMBDA (&AUX (NH1 (%UNIBUS-READ MOUSE-REG1)))
                          ;; Wait till mouse buttons change.
                          (OR (KBD-CHAR-AVAILABLE)
                              ( MOUSE-LAST-BUTTONS
                                 (LDB 1403 NH1)))))
        (SETQ NEW-BUTTONS (MOUSE-BUTTONS)
              CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
              MOUSE-LAST-BUTTONS NEW-BUTTONS)
        (COND ((AND (NULL (KBD-CHAR-AVAILABLE))
                    (ZEROP (LOGAND NEW-BUTTONS CHANGED-BUTTONS)))
               (GO L)))     ;JUST BUTTONS UP.
        (SETQ NEW-X (TRUNCATE (* (LOGAND 7777 (SETQ MOUSE-H2 (%UNIBUS-READ MOUSE-REG2)))
                                 (CAR MOUSE-X-SCALE))
                              (CDR MOUSE-X-SCALE))
              NEW-Y (TRUNCATE (* (LOGAND 7777 (SETQ MOUSE-H1 (%UNIBUS-READ MOUSE-REG1)))
                                 (CAR MOUSE-Y-SCALE))
                              (CDR MOUSE-Y-SCALE)))
        ;; Compute moduli for wrap-around (these are constants unless scale is changed)
        (SETQ X-MODULUS (ABS (TRUNCATE (* 10000 (CAR MOUSE-X-SCALE)) (CDR MOUSE-X-SCALE)))
              Y-MODULUS (ABS (TRUNCATE (* 10000 (CAR MOUSE-Y-SCALE)) (CDR MOUSE-Y-SCALE))))
        ;; Compute delta X and Y, allowing for wrap-around in the original table coordinates
        (SETQ DELTA-X (\ (- NEW-X MOUSE-LAST-X) X-MODULUS)
              DELTA-Y (\ (- NEW-Y MOUSE-LAST-Y) Y-MODULUS))
        (AND ( DELTA-X (LSH X-MODULUS -1)) (SETQ DELTA-X (- DELTA-X X-MODULUS)))
        (AND ( DELTA-X (- (LSH X-MODULUS -1))) (SETQ DELTA-X (+ DELTA-X X-MODULUS)))
        (AND ( DELTA-Y (LSH Y-MODULUS -1)) (SETQ DELTA-Y (- DELTA-Y Y-MODULUS)))
        (AND ( DELTA-Y (- (LSH Y-MODULUS -1))) (SETQ DELTA-Y (+ DELTA-Y Y-MODULUS)))
        (SETQ MOUSE-LAST-X NEW-X MOUSE-LAST-Y NEW-Y)
        (RETURN DELTA-X
                DELTA-Y
                (LOGAND NEW-BUTTONS CHANGED-BUTTONS))))

 );end comment

(DEFUN KEYSTROKE-MOVE-DELTA (CH)
  (LET ((MULTIPLIER (LSH 1 (LDB %%KBD-CONTROL-META CH)))        ;TOP accounted separately
        (CH0 (AND (NOT (LDB-TEST %%KBD-MOUSE CH)) (LDB %%KBD-CHAR CH)))
        (STEP 1)
        (-STEP -1)
        (BIGMULT 16.))
    (PROG NIL
          (IF (= (LDB 0003 (%UNIBUS-READ 764102)) 1)    ;Last character new keyboard?
              (SELECTQ CH0                      ;Yes, same keys but different glyphs
                (#/( (RETURN (* -STEP MULTIPLIER) 0))
                (#/[ (RETURN (FIX (* -STEP BIGMULT MULTIPLIER)) 0))
                (#/) (RETURN (* STEP MULTIPLIER) 0))
                (#/] (RETURN (FIX (* STEP BIGMULT MULTIPLIER)) 0))
                (#/` (RETURN 0 (* STEP MULTIPLIER)))
                (#/~ (RETURN 0 (FIX (* STEP BIGMULT MULTIPLIER))))
                (#/\ (RETURN 0 (* -STEP MULTIPLIER)))
                (#/| (RETURN 0 (FIX (* -STEP BIGMULT MULTIPLIER)))))
              (SELECTQ CH0
                (#/[ (RETURN (* -STEP MULTIPLIER) 0))
                (#/{ (RETURN (FIX (* -STEP BIGMULT MULTIPLIER)) 0))
                (#/] (RETURN (* STEP MULTIPLIER) 0))
                (#/} (RETURN (FIX (* STEP BIGMULT MULTIPLIER)) 0))
                (#/\ (RETURN 0 (* STEP MULTIPLIER)))
                (#/| (RETURN 0 (FIX (* STEP BIGMULT MULTIPLIER))))
                (#// (RETURN 0 (* -STEP MULTIPLIER)))
                (#/ (RETURN 0 (FIX (* -STEP BIGMULT MULTIPLIER))))))
          (RETURN NIL))))

(DEFMETHOD (PROBE-CLASS :PROBE-UP-LOOP) ()
  (DO () (())
    (PRIN1 (<- SELF ':PROBE-SENSE))
    (PROCESS-SLEEP 60.)))

(DEFMETHOD (PROBE-CLASS :PROBE-SENSE) ()
  (LDB (COND ((EQ NAME 'RIGHT) 1501)
             (T 1401))
       (CTEST-READ)))

(DEFMETHOD (PROBE-CLASS :PROBE-DOWN-LOOP) ()
  (DO () (())
    (PRIN1 (<- SELF ':PROBE-DOWN-PAST-PIN))
    (PROCESS-SLEEP 60.)))

(DEFMETHOD (PROBE-CLASS :PROBE-DOWN-PAST-PIN) ()
  (LDB (COND ((EQ NAME 'RIGHT) 0501)
             (T 0101))
       (CTEST-READ)))

(DEFMETHOD (PROBE-CLASS :PROBE-DOWN) ()
  (PROG (UP-SWITCH START-TIME RETRY-COUNT)
        (SETQ RETRY-COUNT 0)
        (SETQ UP-SWITCH (COND ((EQ NAME 'RIGHT) 1701)
                              (T 1601)))
     L0 (CTEST-OUT (DPB -1
                        (COND ((EQ NAME 'RIGHT) 1701)
                              (T 1601))
                        CTEST-LAST-OUTPUT))
        (SETQ START-TIME (TIME))
     L  (COND ((ZEROP (LDB UP-SWITCH (CTEST-READ)))  ;AT LEAST MAKE SURE ITS NOT
               (PROCESS-SLEEP 1)    ;ALL THE WAY UP.  IF IT GETS STUCK AT THE TOP,
               (COND ((< (- (TIME) START-TIME) 120.)
                      (GO L))             ;IT WILL THINK ITS ON A MOBY PIN
                     (T (COND ((<- OTHER-PROBE ':DOWN-P)     ;HUNG, TRY A LITTLE
                               (<- OTHER-PROBE ':PROBE-UP)   ;MECHANICAL SHOCK
                               (<- OTHER-PROBE ':PROBE-DOWN))
                              (T (<- OTHER-PROBE ':PROBE-DOWN)
                                 (<- OTHER-PROBE ':PROBE-UP)))
                        (COND ((< (SETQ RETRY-COUNT (1+ RETRY-COUNT)) 20.)
                               (GO L0))
                              (T (CERROR T NIL NIL "~%PROBE STUCK UP")
                                 (GO L0)))))))
        (COND ((NULL DOWN-P) (process-sleep 30.)))
        (SETQ DOWN-P T)))

(DEFMETHOD (PROBE-CLASS :PROBE-UP) ()
  (PROG (DOWN-COM UP-SWITCH START-TIME)
        (SETQ DOWN-COM (COND ((EQ NAME 'RIGHT) 1701)
                              (T 1601)))
        (SETQ UP-SWITCH (COND ((EQ NAME 'RIGHT) 1701)
                              (T 1601)))
    L0  (CTEST-OUT (DPB 0
                        DOWN-COM
                        CTEST-LAST-OUTPUT))
        (SETQ START-TIME (TIME))
     L  (COND ((NOT (ZEROP (LDB UP-SWITCH (CTEST-READ))))
               (PROCESS-SLEEP 1)
               (COND ((< (- (TIME) START-TIME) 1200.)
                      (GO L))
                     (T (CTEST-OUT (DPB 1  ;IT MIGHT BE FRYING, SO STOP TRYING
                                        DOWN-COM
                                        CTEST-LAST-OUTPUT))
                        (CERROR T NIL NIL "~%PROBE WONT COME UP")
                        (GO L0)))))
        (PROCESS-SLEEP 10.)
        (COND ((NOT (ZEROP (LDB UP-SWITCH (CTEST-READ))))
               (GO L)))
        (SETQ DOWN-P NIL)))

(DEFMETHOD (PROBE-CLASS :PROBE-DRIVE) (ARG)
  (CTEST-OUT (DPB ARG
                  (COND ((EQ NAME 'RIGHT) 1501)
                        (T 1401))
                  CTEST-LAST-OUTPUT)))

(DEFMETHOD (PROBE-CLASS :CENTER-ON-PIN) ()
  (PROG (XC XR YC YR XS YS)
        (MULTIPLE-VALUE (XC XR) (PROBE-CENTER SELF ':STEP-X ':PROBE-DOWN-PAST-PIN))
        (<- SELF ':STEP-X (SETQ XS XC))
        (MULTIPLE-VALUE (YC YR) (PROBE-CENTER SELF ':STEP-Y ':PROBE-DOWN-PAST-PIN))
        (<- SELF ':STEP-Y (SETQ YS YC))
        (COND ((< XR 5)
               (MULTIPLE-VALUE (XC XR) (PROBE-CENTER SELF ':STEP-X ':PROBE-DOWN-PAST-PIN))
               (SETQ XS (+ XS XC))
               (<- SELF ':STEP-X XC)))
        (<- SELF ':PROBE-DOWN)
        (RETURN XS YS)
))

;CIRCLE OUTWARDS UNTIL RESTS ON A PIN
(DEFMETHOD (PROBE-CLASS :CLIMB-ON-PIN) ()
  (PROG (XC YC XAMP YAMP XPHASE YPHASE LC)
        (SETQ XC 0 YC 0 XAMP 1 YAMP 1 XPHASE 1 YPHASE 1)
        (COND ((ZEROP (<- SELF ':PROBE-DOWN-PAST-PIN))
               (RETURN XC YC)))
    LX0 (SETQ LC XAMP)
    LX  (COND ((<= (SETQ LC (1- LC)) 0)
               (SETQ XAMP (+ XAMP 2) XPHASE (MINUS XPHASE))
               (GO LY0)))
        (<- SELF ':STEP-X XPHASE)
        (SETQ XC (+ XC XPHASE))
        (<- SELF ':PROBE-DOWN)
        (COND ((ZEROP (<- SELF ':PROBE-DOWN-PAST-PIN))
               (RETURN XC YC))
              (T (GO LX)))
    LY0 (SETQ LC YAMP)
    LY  (COND ((<= (SETQ LC (1- LC)) 0)
               (SETQ YAMP (+ YAMP 2) YPHASE (MINUS YPHASE))
               (GO LX0)))
        (<- SELF ':STEP-Y YPHASE)
        (SETQ YC (+ YC YPHASE))
        (<- SELF ':PROBE-DOWN)
        (COND ((ZEROP (<- SELF ':PROBE-DOWN-PAST-PIN))
               (RETURN XC YC))
              (T (GO LY)))
))

(DEFUN PROBE-CENTER (PROBE-OBJECT STEP-MSG SENSE-MSG)
  (PROG (+C -C )
        (SETQ +C 0 -C 0)
    L1  (<- PROBE-OBJECT STEP-MSG 1)
        (SETQ +C (1+ +C))
        (<- PROBE-OBJECT ':PROBE-DOWN)
        (COND ((ZEROP (<- PROBE-OBJECT SENSE-MSG))
               (GO L1)))        ;STILL IN CONTACT, TRY ANOTHER STEP
        (<- PROBE-OBJECT STEP-MSG (MINUS +C))   ;BACK TO ORIGINAL PLACE
    L2  (<- PROBE-OBJECT STEP-MSG -1)
        (SETQ -C (1+ -C))
        (<- PROBE-OBJECT ':PROBE-DOWN)
        (COND ((ZEROP (<- PROBE-OBJECT SENSE-MSG))
               (GO L2)))
        (<- PROBE-OBJECT STEP-MSG -C)   ;BACK TO ORIGINAL PLACE
        (FORMAT T "~%~S +C:~D, -C:~D" STEP-MSG +C -C)
        (RETURN (TRUNCATE (- +C -C) 2) (+ +C -C -2))
))

;CALL THIS WITH PROBE DOWN AND GROUNDED.  THIS WILL FROB AROUND, ATTEMPTING
; TO CENTER THE PROBE ON THE GROUND PIN.  RETURNS (X,Y) STEPPED, OR NIL IF LOST GROUND.
(DEFMETHOD (PROBE-CLASS :PROBE-CENTER-GROUND) ()
  (PROG (XC XR YC YR XS YS)
        (SETQ XS 0 YS 0)
        (<- SELF ':PROBE-DRIVE 1)
        (COND ((NOT (ZEROP (<- SELF ':PROBE-SENSE)))
               (RETURN NIL)))   ;NOT WINNING
        (MULTIPLE-VALUE (XC XR) (CENTER-GROUND SELF ':STEP-X))
        (<- SELF ':STEP-X (SETQ XS XC))
        (MULTIPLE-VALUE (YC YR) (CENTER-GROUND SELF ':STEP-Y))
        (<- SELF ':STEP-Y (SETQ YS YC))
        (COND ((< XR 5)
               (MULTIPLE-VALUE (XC XR) (CENTER-GROUND SELF ':STEP-X))
               (SETQ XS (+ XS XC))
               (<- SELF ':STEP-X XC)))
        (<- SELF ':PROBE-DOWN)
        (RETURN XS YS)
))


(DEFUN CENTER-GROUND (PROBE-OBJECT MSG)
  (PROG (+C -C )
        (SETQ +C 0 -C 0)
    L1  (<- PROBE-OBJECT MSG 1)
        (SETQ +C (1+ +C))
        (<- PROBE-OBJECT ':PROBE-DOWN)
        (COND ((ZEROP (<- PROBE-OBJECT ':PROBE-SENSE))
               (GO L1)))        ;STILL IN CONTACT, TRY ANOTHER STEP
        (<- PROBE-OBJECT MSG (MINUS +C))        ;BACK TO ORIGINAL PLACE
    L2  (<- PROBE-OBJECT MSG -1)
        (SETQ -C (1+ -C))
        (<- PROBE-OBJECT ':PROBE-DOWN)
        (COND ((ZEROP (<- PROBE-OBJECT ':PROBE-SENSE))
               (GO L2)))
        (<- PROBE-OBJECT MSG -C)        ;BACK TO ORIGINAL PLACE
        (FORMAT T "~%~S +C:~D, -C:~D" MSG +C -C)
        (RETURN (TRUNCATE (- +C -C) 2) (+ +C -C -2))
))

(DEFMETHOD (PROBE-CLASS :DEFINE-POSITION) (X Y)
  (multiple-value-bind (XG YG)
      (<- SELF ':CONVERT-TO-STEPS X Y)
    (SETQ XPOS-STEPS XG YPOS-STEPS YG)))

(DEFMETHOD (PROBE-CLASS :DEFINE-POSITION-STRING-LOC) (STRING-LOC)
  (LET ((LOC (CTEST-GETLOC STRING-LOC)))
    (MULTIPLE-VALUE-BIND (Y X)
        (CTEST-MAPLOC LOC)
      (multiple-value-bind (XG YG)
          (<- SELF ':CONVERT-TO-STEPS X Y)
        (SETQ XPOS-STEPS XG YPOS-STEPS YG)))))

(DEFMETHOD (PROBE-CLASS :APPARENT-ERROR) (XE YE &AUX TEM)
  (SETQ XCE (TRUNCATE (+ (* 9 XCE) (* XE 100.)) 10.))
  (SETQ YCE (TRUNCATE (+ (* 9 YCE) (* YE 100.)) 10.))
  (COND ((> XCE 70.)
         (SETQ TEM (MAX 1 (TRUNCATE XCE 100.)))
         (SETQ XPOS-STEPS (- XPOS-STEPS TEM))
         (SETQ XCE 0)
         (FORMAT T "~%CORRECTING ~S XPOS-STEPS -~S" NAME TEM))
        ((< XCE -70.)
         (SETQ TEM (MAX 1 (TRUNCATE (MINUS XCE) 100.)))
         (SETQ XPOS-STEPS (+ XPOS-STEPS TEM))
         (SETQ XCE 0)
         (FORMAT T "~%CORRECTING ~S XPOS-STEPS +~S" NAME TEM)))
  (COND ((> YCE 70.)
         (SETQ TEM (MAX 1 (TRUNCATE YCE 100.)))
         (SETQ YPOS-STEPS (- YPOS-STEPS TEM))
         (SETQ YCE 0)
         (FORMAT T "~%CORRECTING ~S YPOS-STEPS -~S" NAME TEM))
        ((< YCE -70.)
         (SETQ TEM (MAX 1 (TRUNCATE (MINUS YCE) 100.)))
         (SETQ YPOS-STEPS (+ YPOS-STEPS TEM))
         (SETQ YCE 0)
         (FORMAT T "~%CORRECTING ~S YPOS-STEPS +~S" NAME TEM))))

(DEFMETHOD (PROBE-CLASS :GOTO) (X Y)
  (multiple-value-bind (XG YG)
      (<- SELF ':CONVERT-TO-STEPS X Y)
;   (FORMAT T "~%Stepping ~d x, ~d y" (- xg xpos-steps) (- yg ypos-steps))
    (LET ((XS (- XG XPOS-STEPS))
          (YS (- YG YPOS-STEPS)))
      (COND ((OR (AND (EQ NAME 'LEFT)
                      (PLUSP XS))
                 (AND (EQ NAME 'RIGHT)
                      (MINUSP XS)))
             (<- SELF ':STEP-X XS)      ;moving "away", step x first
             (<- SELF ':STEP-Y YS))
            (T
              (<- SELF ':STEP-Y YS)     ;moving "towards", step y first
              (<- SELF ':STEP-X XS))))))

(DEFMETHOD (PROBE-CLASS :CONVERT-TO-STEPS) (FX FY)
 (PROG NIL
       (MULTIPLE-VALUE-BIND (XC YC)
           (<- CALIBRATOR ':COMPUTE-CORRECTION FX FY)
         (RETURN (- (FIX (* FX MILLS-TO-STEPS)) XC)
                 (- (FIX (* FY MILLS-TO-STEPS)) YC)))))

(DEFMETHOD (PROBE-CLASS :GOTO-LOC) (LOC)
  (MULTIPLE-VALUE-BIND (Y X)
      (CTEST-MAPLOC LOC)
    (<- SELF ':GOTO X Y)))

(DEFMETHOD (PROBE-CLASS :GOTO-STRING-LOC) (STRING-LOC)
  (MULTIPLE-VALUE-BIND (Y X)
      (CTEST-MAPLOC (CTEST-GETLOC STRING-LOC))
    (<- SELF ':GOTO X Y)))

(DEFMETHOD (PROBE-CLASS :STEP-X) (STEPS &OPTIONAL (SPD CTEST-SLOW-SPEED))
  (<- SELF ':PROBE-UP)
  (CTEST-MOVE NAME
              (COND ((MINUSP STEPS) 'X-MINUS) (T 'X-PLUS))
              (ABS STEPS)
              SPD)
  (SETQ XPOS-STEPS (+ STEPS XPOS-STEPS)))

(DEFMETHOD (PROBE-CLASS :STEP-Y) (STEPS &OPTIONAL (SPD CTEST-SLOW-SPEED))
  (<- SELF ':PROBE-UP)
  (CTEST-MOVE NAME
              (COND ((MINUSP STEPS) 'Y-MINUS) (T 'Y-PLUS))
              (ABS STEPS)
              SPD)
  (SETQ YPOS-STEPS (+ STEPS YPOS-STEPS)))

(DEFMETHOD (PROBE-CLASS :CALIBRATE) (&OPTIONAL POS NBOARDS NO-ASK)
  (PROG (XD YD REDO-CALIB NEXT-POS)
        (IF (NULL POS)
            (SETQ POS (SELECTQ CTEST-BOARD-TYPE
                        (MPG216 "1A01-10")
                        (LG684 "A1-10"))))
        (COND ((SETQ REDO-CALIB (OR NO-ASK
                                    (FQUERY FORMAT:Y-OR-N-P-OPTIONS
                                            "~%REDO CALIBRATOR?")))
               (<- CALIBRATOR ':INITIALIZE)))
        (COND ((NULL MILLS-TO-STEPS)
               (SETQ MILLS-TO-STEPS (// 1434. (FLOAT (* 5 MPG216-GXOFST))))))
        (FORMAT T "~%Position ~S probe over ~A" NAME POS)
        (COND ((NOT NO-ASK)
               (<- SELF ':MANUAL-CONTROL)
               (<- SELF ':PROBE-UP)
               (COND ((Y-OR-N-P "RECOMPUTE MILLS-TO-STEPS?")
                      (<- SELF ':MANUAL-CONTROL)
                      (SETQ NEXT-POS (SELECTQ CTEST-BOARD-TYPE
                                       (MPG216 "1F01-10")
                                       (LG684 "A30-10")))
                      (FORMAT T "~%Now position it over ~A" NEXT-POS)
               (<- SELF ':probe-up)
               (MULTIPLE-VALUE (XD YD) (<- self ':manual-control ctest-slow-speed))
               (FORMAT T "~%That was ~d xsteps, ~d ysteps" XD YD)
               (SETQ MILLS-TO-STEPS (// YD (FLOAT
                                             (SELECTQ CTEST-BOARD-TYPE
                                               (MPG216 (* 5 MPG216-GXOFST))
                                               (LG684 (* 30. LG684-XDIPSP))))))
               (SETQ POS NEXT-POS)))))
        (<- SELF ':DEFINE-POSITION-STRING-LOC POS)
        (COND (REDO-CALIB
               (SELECTQ CTEST-BOARD-TYPE
                 (MPG216
                  (IF (NULL NBOARDS)
                      (PROGN (FORMAT T "~%HOW MANY BOARDS?")
                             (SETQ NBOARDS (READ))))
                  (DOTIMES (C NBOARDS)
                    (CALIBRATE-AT-LOC (1+ C) (FORMAT NIL "~DA1-10" (1+ C)) CALIBRATOR)
                    (CALIBRATE-AT-LOC (1+ C) (FORMAT NIL "~DF1-10" (1+ C)) CALIBRATOR))
                  (CALIBRATE-AT-LOC NBOARDS (FORMAT NIL "~DA29-10" NBOARDS) CALIBRATOR)
                  (CALIBRATE-AT-LOC NBOARDS (FORMAT NIL "~DF25-10" NBOARDS) CALIBRATOR))
                 (LG684
                  (CALIBRATE-AT-LOC 1 "F1-10" CALIBRATOR)
                  (CALIBRATE-AT-LOC 1 "F30-10" CALIBRATOR)))))
        (PRINT-CALIBRATION)))

(DEFMETHOD (PROBE-CLASS :RECALIBRATE-BOARD) (BOARD-NUM
    &OPTIONAL AUTO-FLAG (LOCS (SELECTQ CTEST-BOARD-TYPE
                                (MPG216 '("A1-10" "F1-10" "A29-10" "F26-10"))
                                (LG684 '("A1-10" "A1-30" "F1-10" "F1-30")))))
  (<- CALIBRATOR ':FLUSH-CALIBRATION-FOR-BOARD BOARD-NUM)
  (DOLIST (L LOCS)
    (CALIBRATE-AT-LOC BOARD-NUM
                      (SELECTQ CTEST-BOARD-TYPE
                        (MPG216 (STRING-APPEND (FORMAT NIL "~D" BOARD-NUM) L))
                        (LG684 L))
                      CALIBRATOR
                      AUTO-FLAG)
    (SETQ AUTO-FLAG T)))

(DEFUN CALIBRATE-AT-LOC (BOARD-NUM STRING-LOC CALIB &OPTIONAL (AUTO-FLAG T)
                                   &AUX XC YC XC1 YC1)
  (MULTIPLE-VALUE-BIND (Y X)
      (CTEST-MAPLOC (CTEST-GETLOC STRING-LOC))
    (FORMAT T "~%CALIBRATING AT ~A" STRING-LOC)
    (<- SELF ':GOTO X Y)        ;GOES TO NOMINAL SPOT, MINUS CURRENT CORRECTION
    (MULTIPLE-VALUE (XC YC)
      (<- CALIB ':COMPUTE-CORRECTION X Y))  ;CURRENT CORRECTION
    (FORMAT T "~%CURRENT-CORRECTION ~S,~S" XC YC)
    (COND (AUTO-FLAG
            (<- SELF ':PROBE-DOWN)
            (<- SELF ':PROBE-DRIVE 1)
            (COND ((NOT (ZEROP (<- SELF ':PROBE-DOWN-PAST-PIN)))
                   (COND ((ZEROP (<- SELF ':PROBE-SENSE))
                          (FORMAT T "~%CENTER ON GROUND")
                          (MULTIPLE-VALUE (XC1 YC1)
                            (<- SELF ':PROBE-CENTER-GROUND))
                          (SETQ XC (- XC XC1) YC (- YC YC1))
                          (FORMAT T "~%AFTER CENTER GROUND, ~S ~S" XC YC))
                         (T (FORMAT T "~%CLIMB-ON-PIN")
                            (MULTIPLE-VALUE (XC1 YC1)
                              (<- SELF ':CLIMB-ON-PIN))
                            (SETQ XC (- XC XC1) YC (- YC YC1))
                            (FORMAT T "~%AFTER CLIMB-ON-PIN, ~S ~S" XC YC))))))
          (T
            (MULTIPLE-VALUE (XC1 YC1)
               (<- SELF 'MANUAL-CONTROL CTEST-SLOW-SPEED))
             (SETQ XC (- XC XC1) YC (- YC YC1))))
    (MULTIPLE-VALUE (XC1 YC1)
      (<- SELF ':CENTER-ON-PIN))
    (SETQ XC (- XC XC1) YC (- YC YC1))
    (FORMAT T "~%AFTER CENTER-ON-PIN ~S ~S" XC YC)
    (<- CALIB ':SET-CALIBRATION-POINT X Y XC YC BOARD-NUM)))

(DEFMETHOD (PROBE-CLASS :PRINT-CALIBRATION) ()
  (FORMAT T "~%~S PROBE, CALIBRATOR ~S"
          NAME (<- CALIBRATOR ':CALIBRATION-POINTS)))


(DEFUN CTEST-CALIBRATE NIL
  (<- RIGHT-PROBE ':CALIBRATE)
  (<- RIGHT-PROBE ':STEP-X -400)                ;clear field
  (<- LEFT-PROBE ':CALIBRATE))

(DEFUN TEST-ICMEM (&OPTIONAL NO-CAL (SKIP-COUNT 0) NO-READIN)
  (IF (NULL NO-CAL) (SETUP 2))
  (SETQ CTEST-BAD-RUNS NIL)
  (CTEST-PROCESS-WLR "CADRWD;ICMEM3 WLR" SKIP-COUNT NO-READIN))

(DEFUN TEST-CADR (&OPTIONAL NO-CAL (SKIP-COUNT 0) NO-READIN)
  (IF (NULL NO-CAL) (SETUP 4))
  (SETQ CTEST-BAD-RUNS NIL)
  (CTEST-PROCESS-WLR "CADRWD;CADR4 WLR" SKIP-COUNT NO-READIN))

(DEFUN RESET-POSITION NIL
  (FQUERY FORMAT:Y-OR-N-P-OPTIONS
          "~% Is the right probe over 1A01-10 and the left probe over 1AJ1-1?")
  (<- RIGHT-PROBE ':CENTER-ON-PIN)
  (<- RIGHT-PROBE ':DEFINE-POSITION-STRING-LOC "1A01-10")
  (<- LEFT-PROBE ':CENTER-ON-PIN)
  (<- LEFT-PROBE ':DEFINE-POSITION-STRING-LOC "1AJ1-1"))

(DEFUN SETUP (NBOARDS)
  (FQUERY FORMAT:Y-OR-N-P-OPTIONS
          "~% Is the right probe over 1A01-10 and the left probe over 1AJ1-1?")
  (SETUP-RIGHT NBOARDS "1A01-10")
  (SETUP-LEFT NBOARDS  "1AJ1-1"))

(DEFUN SETUP-RIGHT (NBOARDS &OPTIONAL (POS "1A01-10"))
  (<- RIGHT-PROBE ':CALIBRATE POS NBOARDS T)
  (<- RIGHT-PROBE ':STEP-X -400))               ;clear field

(DEFUN SETUP-LEFT (NBOARDS &OPTIONAL (POS "1A01-10"))
  (<- LEFT-PROBE ':CALIBRATE POS NBOARDS T))

(DEFUN PRINT-CALIBRATION NIL
  (FORMAT T "~% MILLS-TO-STEPS"
          MILLS-TO-STEPS)
  (<- LEFT-PROBE ':PRINT-CALIBRATION)
  (<- RIGHT-PROBE ':PRINT-CALIBRATION))

(DEFUN DEFINE-PROBE-LOCATION (PROBE-OB STRING-LOC)
  (MULTIPLE-VALUE-BIND (Y X)
      (CTEST-MAPLOC (CTEST-GETLOC STRING-LOC))
    (<- PROBE-OB ':DEFINE-POSITION X Y)))

;return approx position in mills.  Ignores cross term.
(defmethod (probe-class :xpos) ()
  (convert-steps-to-mills xpos-steps))

(defmethod (probe-class :ypos) ()
  (convert-steps-to-mills ypos-steps))

;single axis approx
(DEFUN CONVERT-MILLS-TO-STEPS (MILLS)
  (COND ((NULL MILLS-TO-STEPS)
         (CTEST-CALIBRATE)))
  (FIX (* MILLS MILLS-TO-STEPS)))

(DEFUN CONVERT-STEPS-TO-MILLS (STEPS)
  (COND ((NULL MILLS-TO-STEPS)
         (CTEST-CALIBRATE)))
  (TRUNCATE STEPS MILLS-TO-STEPS))

(defun probe-buzz (probe)
  (do () (())
    (<- probe ':PROBE-UP)
    (<- probe ':PROBE-DOWN)))

;RETURNS X,Y NUMBER OF STEPS MOVED INITIAL SPECIFIED PROBE
(DEFMETHOD (PROBE-CLASS :MANUAL-CONTROL) (&optional (spd ctest-ramp-speed))
 (PROG (IX IY)
       (SETQ IX XPOS-STEPS IY YPOS-STEPS)
   (TV:WITH-MOUSE-USURPED               ;we want all the buttons
    (PROG (DX DY BD CH)
       L  (MULTIPLE-VALUE (CH BD DX DY)
            (MOVE-CHAR-OR-TYI-OR-MOUSE-BUTTON))
          (COND ((OR DX DY) (GO MOVE))
                ((EQ CH #\ALT)
                 (<-  (COND ((EQ NAME 'LEFT) RIGHT-PROBE)
                            (T LEFT-PROBE))
                      ':MANUAL-CONTROL
                      CTEST-SLOW-SPEED))  ;IT MIGHT BE IN CALIBRATION
                ((EQ CH #\RUBOUT)
                 (RETURN T)))
          (COND ((NULL BD) (GO L))
                ((BIT-TEST BD 1)
                 (<- SELF (COND (DOWN-P ':PROBE-UP)
                                (T ':PROBE-DOWN))))
                ((BIT-TEST BD 2)
                 (<- SELF ':PROBE-CENTER-GROUND))
                ((BIT-TEST BD 4)
                 (<- SELF ':CENTER-ON-PIN)))
          (GO L)
      MOVE(IF DX (<- SELF ':STEP-X DX SPD))
          (IF DY (<- SELF ':STEP-Y DY SPD))
          (GO L)))
  (RETURN (- XPOS-STEPS IX) (- YPOS-STEPS IY)))
 )

(DEFUN MOVE-CHAR-OR-TYI-OR-MOUSE-BUTTON (&OPTIONAL (TV:STREAM STANDARD-INPUT))
 TV:(PROG (NEW-BUTTONS CHANGED-BUTTONS IO-BUFFER)
         (SETQ IO-BUFFER (FUNCALL STREAM ':IO-BUFFER))
      L  (PROCESS-ALLOW-SCHEDULE)
         (PROCESS-WAIT "mouse"
                       #'(LAMBDA (IO-BUFFER)
                           (OR (NULL (IO-BUFFER-EMPTY-P IO-BUFFER))
                               (IF (EQ IO-BUFFER (KBD-GET-IO-BUFFER))
                                   (NULL (IO-BUFFER-EMPTY-P KBD-IO-BUFFER)))
                               ( MOUSE-LAST-BUTTONS
                                  (LDB 1403 (%UNIBUS-READ MOUSE-REG1)))))
                       IO-BUFFER)
         (IF (FUNCALL STREAM ':LISTEN)
             (LET ((CH (FUNCALL STREAM ':TYI)))
               (MULTIPLE-VALUE-BIND (DX DY) (USER:KEYSTROKE-MOVE-DELTA CH)
                 (IF (NULL DX)
                     (RETURN CH)
                     (RETURN NIL NIL DX DY)))))
         (SETQ NEW-BUTTONS (MOUSE-BUTTONS)
               CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
               MOUSE-LAST-BUTTONS NEW-BUTTONS)
         (IF (ZEROP (LOGAND NEW-BUTTONS CHANGED-BUTTONS))
             (GO L)
             (RETURN NIL (LOGAND NEW-BUTTONS CHANGED-BUTTONS))))
  )

(defmethod (probe-class :feep-test) (&optional (rate 20.))
  (do () (())
    (process-sleep rate)
    (cond ((zerop (<- self ':probe-down-past-pin))
           (tv:beep)))))

;;INITIALIZATION

(DEFVAR LEFT-PROBE (<- PROBE-CLASS ':NEW ':NAME 'LEFT
                     ':XPOS-STEPS 0 ':YPOS-STEPS 0 ':XCE 0 ':YCE 0
                     ':CALIBRATOR (<- CALIBRATOR-CLASS ':NEW ':NAME 'LEFT)))
(DEFVAR RIGHT-PROBE (<- PROBE-CLASS ':NEW ':NAME 'RIGHT
                      ':XPOS-STEPS 0 ':YPOS-STEPS 0 ':XCE 0 ':YCE 0
                      ':CALIBRATOR (<- CALIBRATOR-CLASS ':NEW ':NAME 'RIGHT)))

(<- LEFT-PROBE ':OTHER-PROBE<- RIGHT-PROBE)
(<- RIGHT-PROBE ':OTHER-PROBE<- LEFT-PROBE)

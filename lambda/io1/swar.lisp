;-*- Mode:LISP; Package:(SPACEWAR GLOBAL 400); Base:8 -*-

;; These two constants relate the program's units to dots and seconds.
;; All of the parameters are set using these two so that their effective
;; values can be independent of changes of the quantization.
(DEFVAR CPS 10.)     ;; Cycles per second
(DEFVAR UPD 1000.)   ;; units per dot.
(DEFVAR HALF-UPD 500.)

;; Here are the parameters you might want to change.

;; Acceleration of ships' engines, in units per cycle per cycle.
(DEFVAR SHIP-ACCELERATION (TRUNC (* 40. UPD) CPS #|CPS|#))
;; Angular velocity of ships' turning.
(DEFVAR SHIP-TURNING-RATE .50)

;; Number of torps each ship has.
;; Takes effect only at the start of a game.
(DEFVAR SHIP-TORP-SUPPLY 40.)
;; Number of cycles between firings
(DEFVAR TORP-RELOAD-TIME (FIX (* .6 CPS)))
;; Velocity of torp relative to ship that fires it, in units per cycle.
(DEFVAR TORP-VELOCITY (TRUNC (* 100. UPD) CPS))
;; Collision radius of a torpedo.
(DEFVAR TORP-RADIUS (* 3 UPD))
;; Collision radius of a ship.
(DEFVAR SHIP-RADIUS (* 9 UPD))
;; Time a torp can travel before exploding, in cycles.
(DEFVAR TORP-LIFETIME (* 6 CPS))
;; Distance for torp-torp collisions (or 0, to use twice the torp radius).
;; The reason for making torp-torp collisions use a larger distance
;; is to make defending with torps relatively easier.
;; Actually, this collision distance applies to any two objects
;; whose radii add up to less than this minimum.
(DEFVAR TORP-TORP-COLLISION-DISTANCE (* 10. UPD))

;; Beep frequencies for the two ships' death knells.
(DEFVAR SHIP-FREQ-ALIST '((FOO 1000) (BAR 2000)))

;; T => bounce off walls, NIL => warp to opposite edge.
(DEFVAR WALL-BOUNCE NIL)

;; Time between death of ship and end of game, in cycles.
(DEFVAR DEATH-DELAY (* 3 CPS))

;; Number of cycles during which a ship can be thrusting before it runs out of fuel.
;; Takes effect only at the start of a game.
(DEFVAR SHIP-FUEL-SUPPLY (* 50. CPS))

;; T => there should be a sun.
;; Takes effect only at the start of a game.
(DEFVAR SUN-FLAG T)
;; T => collision with a sun is fatal.  NIL => it has no effect.
;; :CORNER => it sends you to the corner of the universe.
(DEFVAR SUN-COLLISION-FLAG ':CORNER)
;; Acceleration due to the sun at 10 units away,
;; in units per cycle per cycle.
;; The numeric parameter is the acceleration at 10 dots in dots per second per second.
(DEFVAR MASS-OF-SUN (TRUNC (* 2000. UPD UPD UPD) CPS #|CPS|#))

;; Radius of sun for collision purposes, in units
(DEFVAR SUN-RADIUS (* 8 UPD))

;; Time delay after leaving hyperspace before you can enter it again.
(DEFVAR HYPERSPACE-RELOAD-TIME (* 5 CPS))
;; Time ship spends in hyperspace.
(DEFVAR HYPERSPACE-DELAY (* 3 CPS))
;; Probability of death on return from hyperspace.
(DEFVAR HYPERSPACE-DEATH-PROBABILITY .12)
;; Magnitude of velocity which ships have on return from hyperspace.
;; The direction of the velocity is random, but its magnitude is this parameter.
(DEFVAR HYPERSPACE-VELOCITY (TRUNC (* 150. UPD) CPS))

;; These variables are not interesting for the user to set.

(DEFVAR SPACEWAR-WINDOW)

;; This is a list of all the objects visible and moving on the screen.
;; Both ships and torps are on this list.
(DEFVAR OBJECTS)

;; List of all suns.
(DEFVAR SUNS NIL)

;; List of all ships
(DEFVAR SHIPS NIL)

;; List of torpedos which were used and freed up, for re-use.
(DEFVAR FREE-TORPS NIL)

;; Schedule of when to return ships from hyperspace.
;; Each element is a list (time ship) where time is the cycle count.
(DEFVAR HYPERSPACE-RETURN-SCHEDULE)

;; Number of cycles the game has lasted.
;; Used for timing torp and hyperspace reloads.
(DEFVAR CURRENT-CYCLE-NUMBER)

;; Time in cycles until game should end.
;; Normally nil meaning end not scheduled yet.
(DEFVAR TIME-TILL-GAME-END NIL)

;; These are the offsets in DOTS from the official position of a blinker
;; to the center of the blinker's character, using the font SHIP.
(DEFVAR HOFFSET 10.)
(DEFVAR VOFFSET 10.)

(DECLARE (SPECIAL FONTS:SHIP))

;; An object is a list (type mass hvel vvel hpos vpos blinker)
;; where type is nil for a torpedo and t for a ship.
;; The type is used to tell whether the object's direction of facing
;; is visible.  If it is, rotation of that direction causes the blinker's
;; character to be changed.
;; Position is always measured in units, and time in cycles.
;; The length of a cycle is determined by the function MOVE-LOOP.
(DEFSTRUCT (OBJECT :ARRAY :NAMED)
  OBJECT-NAME
  OBJECT-TYPE           ;SHIP, TORP or SUN
  OBJECT-BASE-CHAR
  OBJECT-RADIUS         ;Symbol whose value is radius of object in units.
  OBJECT-HPOS           ;Horizontal position in units.
  OBJECT-VPOS           ;Vertical position in units.
  OBJECT-HVEL           ;Horizontal velocity in units per cycle.
  OBJECT-VVEL           ;Vertical "
  OBJECT-ACCELERATION   ;Magnitude only, in units per cycle per cycle.
  OBJECT-HPROJ          ;horiz projection of unit vector aligned with object.
  OBJECT-VPROJ          ;vert projection of that unit vector.
  OBJECT-ANGLE          ;Angle of pointing of object, in radians from rightward past downward.
  OBJECT-ANGULAR-VEL    ;Rate of change of angle, in rads per cycle.
  OBJECT-COLLISION-FUNCTION     ;Function to call if this collides.  Gets obj as arg.
  OBJECT-IMMOVABLE-FLAG ;T => this object should't gravitate.
  OBJECT-TORP-SUPPLY    ;Number of torps left to be fired.
  OBJECT-FUEL-SUPPLY    ;Time left of having acceleration, in cycles.
  OBJECT-LAST-FIRING-TIME       ;Time at which torp was last fired by this ship.  OR
                                ;Time at which this torp should die.
  OBJECT-LAST-HYPERSPACE-TIME   ;Time at which this ship last entered hyperspace.
  OBJECT-BLINKER                ;The blinker which displays the position of the object.
  OBJECT-DEAD-FLAG)             ;T for a ship which has been killed,
                                ;HYPERSPACE for a ship which is in hyperspace.

;; Coordinates of the walls.  Note that MAXHPOS and MAXVPOS are INCLUSIVE!
(DEFVAR MINHPOS)
(DEFVAR MAXHPOS)
(DEFVAR MINVPOS)
(DEFVAR MAXVPOS)

;; Move each object whose velocity is nonzero.
(DEFUN MOVE-OBJECTS ()
  (DO ((OBJECTS-LEFT OBJECTS (CDR OBJECTS-LEFT)))
      ((NULL OBJECTS-LEFT))
    (OR (OBJECT-IMMOVABLE-FLAG (CAR OBJECTS-LEFT))
        (PROG* ((OBJECT (CAR OBJECTS-LEFT))
                (HVEL (OBJECT-HVEL OBJECT)) (VVEL (OBJECT-VVEL OBJECT))
                (HPOS (OBJECT-HPOS OBJECT)) (VPOS (OBJECT-VPOS OBJECT))
                (BLINKER (OBJECT-BLINKER OBJECT))
                (NEWHPOS (+ HPOS HVEL))
                (NEWVPOS (+ VPOS VVEL)))
            KEEP-BOUNCING
               (COND (WALL-BOUNCE
                      ;; Bounce off walls.
                      (COND ((> NEWVPOS MAXVPOS)
                             (SETQ NEWVPOS (- MAXVPOS (- NEWVPOS MAXVPOS)))
                             (SETF (OBJECT-VVEL OBJECT)
                                   (- (OBJECT-VVEL OBJECT)))
                             (GO KEEP-BOUNCING)))
                      (COND ((> NEWHPOS MAXHPOS)
                             (SETQ NEWHPOS (- MAXHPOS (- NEWHPOS MAXHPOS)))
                             (SETF (OBJECT-HVEL OBJECT)
                                   (- (OBJECT-HVEL OBJECT)))
                             (GO KEEP-BOUNCING)))
                      (COND ((< NEWVPOS MINVPOS)
                             (SETQ NEWVPOS (+ MINVPOS (- MINVPOS NEWVPOS)))
                             (SETF (OBJECT-VVEL OBJECT)
                                   (- (OBJECT-VVEL OBJECT)))
                             (GO KEEP-BOUNCING)))
                      (COND ((< NEWHPOS MINHPOS)
                             (SETQ NEWHPOS (+ MINHPOS (- MINHPOS NEWHPOS)))
                             (SETF (OBJECT-HVEL OBJECT)
                                   (- (OBJECT-HVEL OBJECT)))
                             (GO KEEP-BOUNCING))))
                     (T
                      ;; warp from one edge to the opposite edge.
                      (COND ((> NEWHPOS MAXHPOS)
                             (SETQ NEWHPOS (- NEWHPOS (- MAXHPOS MINHPOS -1)))))
                      (COND ((> NEWVPOS MAXVPOS)
                             (SETQ NEWVPOS (- NEWVPOS (- MAXVPOS MINVPOS -1)))))
                      (COND ((< NEWHPOS MINHPOS)
                             (SETQ NEWHPOS (+ NEWHPOS (- MAXHPOS MINHPOS -1)))))
                      (COND ((< NEWVPOS MINVPOS)
                             (SETQ NEWVPOS (+ NEWVPOS (- MAXVPOS MINVPOS -1)))))))
               (SETF (OBJECT-HPOS OBJECT) NEWHPOS)
               (SETF (OBJECT-VPOS OBJECT) NEWVPOS)
               (SET-BLINKER-CURSORPOS BLINKER HPOS VPOS)))))

;;; Set the cursor position of a blinker.  The position is specified in units.
(DEFUN SET-BLINKER-CURSORPOS (BLINKER X Y)
  (SETQ X (- (ROUND X UPD) HOFFSET)
        Y (- (ROUND Y UPD) VOFFSET))
  (FUNCALL BLINKER ':SET-CURSORPOS X Y))

;; Rotate each object a step of size specified by the object's angular velocity.
(DEFUN ROTATE-OBJECTS (&AUX (PI 3.14159) (TWOPI (* 2 PI)) TEM)
  (DOLIST (OBJECT OBJECTS)
    (OR (ZEROP (SETQ TEM (OBJECT-ANGULAR-VEL OBJECT)))
        (LET ((ANGLE (+ TEM (OBJECT-ANGLE OBJECT))))
          (AND ( ANGLE TWOPI) (SETQ ANGLE (- ANGLE TWOPI)))
          (AND (< ANGLE 0) (SETQ ANGLE (+ ANGLE TWOPI)))
          (SETF (OBJECT-HPROJ OBJECT) (COS ANGLE))
          (SETF (OBJECT-VPROJ OBJECT) (SIN ANGLE))
          (SETF (OBJECT-ANGLE OBJECT) ANGLE)
          (FUNCALL (OBJECT-BLINKER OBJECT) ':SET-CHARACTER
                   (+ (OBJECT-BASE-CHAR OBJECT)
                      (\ (ROUND (* ANGLE 32.) TWOPI) 32.)))))))

(DEFUN SET-DIRECTION (OBJECT ANGLE &AUX (PI 3.14159) (TWOPI (* 2 PI)))
  (SETF (OBJECT-HPROJ OBJECT) (COS ANGLE))
  (SETF (OBJECT-VPROJ OBJECT) (SIN ANGLE))
  (SETF (OBJECT-ANGLE OBJECT) ANGLE)
  (FUNCALL (OBJECT-BLINKER OBJECT) ':SET-CHARACTER
           (+ (OBJECT-BASE-CHAR OBJECT)
              (\ (ROUND (* ANGLE 32.) TWOPI) 32.))))

;; Accelerate each object.  The magnitude of the acceleration
;; is specified explicitly in the object.
;; The direction of acceleration is the way the object is facing.
(DEFUN ACCELERATE-OBJECTS (&AUX TEM)
  (DOLIST (OBJECT OBJECTS)
    (OR (ZEROP (SETQ TEM (OBJECT-ACCELERATION OBJECT)))
        (PROGN (SETF (OBJECT-HVEL OBJECT)
                     (+ (OBJECT-HVEL OBJECT)
                        (ROUND (* TEM (OBJECT-HPROJ OBJECT)))))
               (SETF (OBJECT-VVEL OBJECT)
                     (+ (OBJECT-VVEL OBJECT)
                        (ROUND (* TEM (OBJECT-VPROJ OBJECT)))))))))

;; Fire a torpedo from a specified ship.
(DEFUN FIRE-TORP (SHIP)
  (AND (> (OBJECT-TORP-SUPPLY SHIP) 0)
       (LET ((TORP)
             (HPOS (+ (OBJECT-HPOS SHIP)
                      (ROUND (* (+ 2 (SYMEVAL (OBJECT-RADIUS SHIP)) TORP-RADIUS)
                                (OBJECT-HPROJ SHIP)))))
             (VPOS (+ (OBJECT-VPOS SHIP)
                      (ROUND (* (+ 2 (SYMEVAL (OBJECT-RADIUS SHIP)) TORP-RADIUS)
                                (OBJECT-VPROJ SHIP)))))
             (HVEL (+ (OBJECT-HVEL SHIP)
                      (ROUND (* TORP-VELOCITY
                                (OBJECT-HPROJ SHIP)))))
             (VVEL (+ (OBJECT-VVEL SHIP)
                      (ROUND (* TORP-VELOCITY
                                (OBJECT-VPROJ SHIP))))))
         (SETF (OBJECT-TORP-SUPPLY SHIP) (1- (OBJECT-TORP-SUPPLY SHIP)))
         (SETF (OBJECT-LAST-FIRING-TIME SHIP) CURRENT-CYCLE-NUMBER)
         (OR (AND FREE-TORPS (POP FREE-TORPS TORP))
             (LET ((BLINKER (TV:MAKE-BLINKER SPACEWAR-WINDOW
                                             'TV:CHARACTER-BLINKER
                                             ':FONT FONTS:SHIP
                                             ':CHAR 0)))
               (SETQ TORP
                     (MAKE-OBJECT
                       OBJECT-NAME NIL OBJECT-BASE-CHAR NIL
                       OBJECT-TYPE 'TORP
                       OBJECT-RADIUS 'TORP-RADIUS
                       OBJECT-ACCELERATION 0
                       OBJECT-ANGLE 0
                       OBJECT-ANGULAR-VEL 0
                       OBJECT-HPROJ 1.0
                       OBJECT-VPROJ 0.0
                       OBJECT-COLLISION-FUNCTION 'TORP-COLLISION-FUNCTION
                       OBJECT-IMMOVABLE-FLAG NIL
                       OBJECT-BLINKER BLINKER))))
         (LET ((BLINKER (OBJECT-BLINKER TORP)))
           (SETF (OBJECT-HPOS TORP) HPOS)
           (SETF (OBJECT-VPOS TORP) VPOS)
           (SETF (OBJECT-HVEL TORP) HVEL)
           (SETF (OBJECT-VVEL TORP) VVEL)
           (SETF (OBJECT-LAST-FIRING-TIME TORP)
                 (+ CURRENT-CYCLE-NUMBER TORP-LIFETIME))
           (SET-BLINKER-CURSORPOS BLINKER HPOS VPOS)
           (FUNCALL BLINKER ':SET-VISIBILITY T)
           (PUSH TORP OBJECTS)))))

;; Detect collisions.
;; To make our work linear instead of quadratic,
;; we sort the list of objects by horizontal position.
;; Then, starting with each object, we need only check for collisions
;; with other objects until we find one that's too far away in
;; horizontal position.  All following objects must be even farther away.
;; However, this cannot be done for an object that's close to the left edge
;; since it might collide around the edge with an object far to the right.
(DEFUN COLLIDE-OBJECTS ()
  (SETQ OBJECTS (SORT OBJECTS #'(LAMBDA (O1 O2) ( (OBJECT-HPOS O1) (OBJECT-HPOS O2)))))
  (DO ((OBJECTS-REMAINING OBJECTS (CDR OBJECTS-REMAINING)))
      ((NULL OBJECTS-REMAINING))
    (LET ((OBJECT (CAR OBJECTS-REMAINING)) dist maxcoll)
      (DOLIST (OBJECT1 (CDR OBJECTS-REMAINING))
        (AND (> (MIN (- (OBJECT-HPOS OBJECT1) (OBJECT-HPOS OBJECT))
                     (- (OBJECT-HPOS OBJECT) MINHPOS))
                (MAX (+ (SYMEVAL (OBJECT-RADIUS OBJECT)) SHIP-RADIUS)
                     TORP-TORP-COLLISION-DISTANCE))
             (RETURN NIL))
        (setq maxcoll (MAX (+ (SYMEVAL (OBJECT-RADIUS OBJECT)) (SYMEVAL (OBJECT-RADIUS OBJECT1)))
                           TORP-TORP-COLLISION-DISTANCE))
        (setq dist (FIX (SQRT (+ (^ (FLOAT (- (OBJECT-HPOS OBJECT)
                                              (OBJECT-HPOS OBJECT1)))
                                    2)
                                 (^ (FLOAT (- (OBJECT-VPOS OBJECT)
                                              (OBJECT-VPOS OBJECT1)))
                                    2)))))
        (AND (< dist maxcoll)
             (PROGN (FUNCALL (COND ((EQ (OBJECT-TYPE OBJECT1) 'SUN) 'COLLIDE-WITH-SUN)
                                   (T (OBJECT-COLLISION-FUNCTION OBJECT)))
                             OBJECT)
                    (FUNCALL (COND ((EQ (OBJECT-TYPE OBJECT) 'SUN) 'COLLIDE-WITH-SUN)
                                   (T (OBJECT-COLLISION-FUNCTION OBJECT1)))
                             OBJECT1)))))))

;; Call this function on an object which collides with a sun.
(DEFUN COLLIDE-WITH-SUN (OBJECT)
  (COND ((EQ SUN-COLLISION-FLAG T)
         ;; If the collision flag is T, colliding with a sun
         ;; is like colliding with a ship or torp.
         (FUNCALL (OBJECT-COLLISION-FUNCTION OBJECT) OBJECT))
        ((EQ SUN-COLLISION-FLAG ':CORNER)
         ;; If the collision flag is ':CORNER, colliding with a sun
         ;; sends everything to the corner of the universe with no velocity.
         (SETF (OBJECT-HVEL OBJECT) 0)
         (SETF (OBJECT-VVEL OBJECT) 0)
         (SETF (OBJECT-HPOS OBJECT) MINHPOS)
         (SETF (OBJECT-VPOS OBJECT) MINVPOS))))

;; Colliding with something other than a sun
;; calls the colliding object's collision function.

;; When a ship collides, make it stop dead and blink.
(DEFUN SHIP-COLLISION-FUNCTION (SHIP)
  (COND ((NULL (OBJECT-DEAD-FLAG SHIP))
         (SETF (OBJECT-ACCELERATION SHIP) 0)
         (SETF (OBJECT-ANGULAR-VEL SHIP) 0)
         (FUNCALL (OBJECT-BLINKER SHIP) ':SET-VISIBILITY ':BLINK)
         (SI:%BEEP (OR (CADR (ASSQ (OBJECT-NAME SHIP) SHIP-FREQ-ALIST))
                       TV:BEEP-WAVELENGTH)
                   100000)
         (SETF (OBJECT-DEAD-FLAG SHIP) T)
         (SETQ TIME-TILL-GAME-END DEATH-DELAY))))

;; When a torp collides, it disappears (and becomes free for re-use).
(DEFUN TORP-COLLISION-FUNCTION (TORP)
  (FUNCALL (OBJECT-BLINKER TORP) ':SET-VISIBILITY NIL)
  (SETQ OBJECTS (DELQ TORP OBJECTS))
  (PUSH TORP FREE-TORPS))

;; Suns don't get hurt by collisions.
(DEFUN SUN-COLLISION-FUNCTION (SUN) SUN NIL)

;; Get rid of torps that have lived out their spans.
(DEFUN TORP-EXPIRE ()
  (DOLIST (TORP OBJECTS)
    (AND (EQ (OBJECT-TYPE TORP) 'TORP)
         (< (OBJECT-LAST-FIRING-TIME TORP) CURRENT-CYCLE-NUMBER)
         (TORP-COLLISION-FUNCTION TORP))))

;; Check for everyone out of torps and fuel and no torps on the screen.
(DEFUN GAME-EXPIRE ()
   (PROG NOT-OVER ()
         (AND TIME-TILL-GAME-END (RETURN NIL))
         (DOLIST (SHIP SHIPS)
           (AND (OR (NOT (ZEROP (OBJECT-TORP-SUPPLY SHIP)))
                    (NOT (ZEROP (OBJECT-FUEL-SUPPLY SHIP))))
                (RETURN-FROM NOT-OVER NIL)))
         (DOLIST (OBJ OBJECTS)
           (AND (EQ (OBJECT-TYPE OBJ) 'TORP)
                (RETURN-FROM NOT-OVER NIL)))
         (BEEP)
         (SETQ TIME-TILL-GAME-END DEATH-DELAY)))

;; Gravity and suns.

(DEFUN MAKE-SUN (HPOS VPOS)
  (LET ((BLINKER (TV:MAKE-BLINKER SPACEWAR-WINDOW
                                  'TV:CHARACTER-BLINKER
                                  ':FONT FONTS:SHIP
                                  ':CHAR #/)))
    (SET-BLINKER-CURSORPOS BLINKER HPOS VPOS)
    (FUNCALL BLINKER ':SET-VISIBILITY T)
    (PUSH (MAKE-OBJECT
            OBJECT-NAME 'SUN OBJECT-BASE-CHAR NIL
            OBJECT-TYPE 'SUN
            OBJECT-RADIUS 'SUN-RADIUS
            OBJECT-HPOS HPOS OBJECT-VPOS VPOS
            OBJECT-HVEL 0 OBJECT-VVEL 0
            OBJECT-ACCELERATION 0
            OBJECT-ANGLE 0
            OBJECT-ANGULAR-VEL 0
            OBJECT-HPROJ 1.0
            OBJECT-VPROJ 0.0
            OBJECT-COLLISION-FUNCTION 'SUN-COLLISION-FUNCTION
            OBJECT-IMMOVABLE-FLAG T
            OBJECT-BLINKER BLINKER)
          SUNS))
  (PUSH (CAR SUNS) OBJECTS))

(DEFUN GRAVITATE-OBJECTS ()
  (DOLIST (SUN SUNS)
    (LET ((SHPOS (OBJECT-HPOS SUN))
          (SVPOS (OBJECT-VPOS SUN)))
      (DOLIST (OBJECT OBJECTS)
        (OR (OBJECT-IMMOVABLE-FLAG OBJECT)
            (PROG* ((RHPOS (- SHPOS (OBJECT-HPOS OBJECT)))      ;H component of vector from
                    (RVPOS (- SVPOS (OBJECT-VPOS OBJECT)))      ;obj to sun, and v component.
                    (RSQR (FLOAT (+ (* RHPOS RHPOS) (* RVPOS RVPOS))))  ;Distance squared.
                    (DISTANCE (SQRT RSQR))
                    (ACCEL (* MASS-OF-SUN (// 100.0S0 RSQR))))
              (SETF (OBJECT-HVEL OBJECT)
                    (+ (OBJECT-HVEL OBJECT)
                       (ROUND (* ACCEL (// RHPOS DISTANCE)))))
              (SETF (OBJECT-VVEL OBJECT)
                    (+ (OBJECT-VVEL OBJECT)
                       (ROUND (* ACCEL (// RVPOS DISTANCE)))))))))))

;; Hyperspace, entering and returning.

(DEFUN HYPERSPACE (SHIP)
  (PUSH (LIST (+ CURRENT-CYCLE-NUMBER HYPERSPACE-DELAY) SHIP)
        HYPERSPACE-RETURN-SCHEDULE)
  ;; While in hyperspace, the ship is not visible,
  ;; does not move, accelerate or rotate, and can't collide or fire.
  (FUNCALL (OBJECT-BLINKER SHIP) ':SET-VISIBILITY NIL)
  (SETF (OBJECT-DEAD-FLAG SHIP) 'HYPERSPACE)
  (SETQ OBJECTS (DELQ SHIP OBJECTS)))

(DEFUN RETURN-OBJECTS-FROM-HYPERSPACE (&AUX (PI 3.14159) (TWOPI (* 2 PI)))
  (DOLIST (ENTRY HYPERSPACE-RETURN-SCHEDULE)
    (AND (< (CAR ENTRY) CURRENT-CYCLE-NUMBER)
         (LET ((OBJECT (CADR ENTRY))
               ;; Random angle of orientation
               (ANGLE (RANDOM-IN-RANGE 0 TWOPI))
               ;; Random direction to move in.
               (VELOCITY-ANGLE (RANDOM-IN-RANGE 0 TWOPI)))
           (SETF (OBJECT-LAST-HYPERSPACE-TIME OBJECT) CURRENT-CYCLE-NUMBER)
           ;; Give the ship a random position.
           (SETF (OBJECT-HPOS OBJECT) (FIX (RANDOM-IN-RANGE MINHPOS MAXHPOS)))
           (SETF (OBJECT-VPOS OBJECT) (FIX (RANDOM-IN-RANGE MINVPOS MAXVPOS)))
           (SET-BLINKER-CURSORPOS (OBJECT-BLINKER OBJECT)
                                  (OBJECT-HPOS OBJECT) (OBJECT-VPOS OBJECT))
           ;; Give it a standard velocity in a random direction.
           (SETF (OBJECT-HVEL OBJECT) (FIX (* (COS VELOCITY-ANGLE) HYPERSPACE-VELOCITY)))
           (SETF (OBJECT-VVEL OBJECT) (FIX (* (SIN VELOCITY-ANGLE) HYPERSPACE-VELOCITY)))
           ;; Give it a random orientation.
           (SET-DIRECTION OBJECT ANGLE)
           ;; Turn the ship "back on".
           (SETF (OBJECT-DEAD-FLAG OBJECT) NIL)
           (PUSH OBJECT OBJECTS)
           (FUNCALL (OBJECT-BLINKER OBJECT) ':SET-VISIBILITY T)
           ;; The object has a certain chance of dying on return.
           (AND (< (RANDOM-IN-RANGE 0 1.0) HYPERSPACE-DEATH-PROBABILITY)
                (FUNCALL (OBJECT-COLLISION-FUNCTION OBJECT) OBJECT))
           ;; Now remove this entry from the hyperspace return schedule.
           (SETQ HYPERSPACE-RETURN-SCHEDULE (DELQ ENTRY HYPERSPACE-RETURN-SCHEDULE))))))

(DEFUN RANDOM-IN-RANGE (LOW HIGH)
  (PROG* ((R (RANDOM))
          (RNORM (// (LOGAND R 777777) (FLOAT 1000000))))
         (RETURN (+ LOW (* RNORM (- HIGH LOW))))))

;; Top-level and process hackery.

(DEFUN SPACEWAR-GAME ()
  (INIT)
  (MOVE-LOOP))

(DEFUN SPACEWAR-TOP-LEVEL ()
  (DO () (())
    (SPACEWAR-GAME)))

(DEFVAR SPACEWAR-PROCESS (PROCESS-CREATE "Spacewar"))

;; Start playing spacewar.
(DEFUN SPACEWAR ()
  "Start playing spacewar.  Creates and selects the spacewar window."
  (PROCESS-PRESET SPACEWAR-PROCESS 'SPACEWAR-TOP-LEVEL)
  (PROCESS-ENABLE SPACEWAR-PROCESS))

(DEFUN FREEZE ()
  "Stop the action in the spacewar game."
  (MAPC #'(LAMBDA (REASON) (FUNCALL SPACEWAR-PROCESS ':REVOKE-RUN-REASON REASON))
        (FUNCALL SPACEWAR-PROCESS ':RUN-REASONS)))

(DEFUN RESUME ()
  "Resume action in the spacewar game."
  (PROCESS-ENABLE SPACEWAR-PROCESS))

(DEFUN STOP-SPACEWAR ()
  "Stops the spacewar process and removes the ships from the screen."
  (AND (BOUNDP 'OBJECTS)
       (DOLIST (OBJECT OBJECTS)
         (FUNCALL (OBJECT-BLINKER OBJECT) ':SET-VISIBILITY NIL)))
  (MAPC #'(LAMBDA (REASON) (FUNCALL SPACEWAR-PROCESS ':REVOKE-RUN-REASON REASON))
        (FUNCALL SPACEWAR-PROCESS ':RUN-REASONS)))

;; This is the main loop while a game is in progress.
(DEFUN MOVE-LOOP ()
  (DO ((CURRENT-CYCLE-NUMBER 0 (1+ CURRENT-CYCLE-NUMBER))
       (NEXT-CYCLE (TIME)))
      (())
    (AND TIME-TILL-GAME-END
         (OR (> (SETQ TIME-TILL-GAME-END (1- TIME-TILL-GAME-END)) 0)
             (RETURN NIL)))
    (PROCESS-ALLOW-SCHEDULE)
    (PROCESS-WAIT "Sleep" #'(LAMBDA (TIME1)
                              (TIME-LESSP TIME1 (TIME)))
                  NEXT-CYCLE)
    (OR (SEND SPACEWAR-WINDOW ':EXPOSED-P)
        (PROCESS-WAIT "Expose" SPACEWAR-WINDOW ':EXPOSED-P))
    (SETQ NEXT-CYCLE (+ (TIME) (FLOOR 60. CPS)))
    (DECODE-BITS)
    (AND HYPERSPACE-RETURN-SCHEDULE
         (RETURN-OBJECTS-FROM-HYPERSPACE))
    (TORP-EXPIRE)
    (GAME-EXPIRE)  ;Check for everyone out of torps and fuel.
    (COLLIDE-OBJECTS)
    (MOVE-OBJECTS)
    (ACCELERATE-OBJECTS)
    (ROTATE-OBJECTS)
    (GRAVITATE-OBJECTS)))

(DEFFLAVOR SPACEWAR-WINDOW () (TV:BOX-LABEL-MIXIN TV:LISTENER-MIXIN-INTERNAL TV:WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL))

(TV:ADD-SYSTEM-KEY #\W 'SPACEWAR-WINDOW "Spacewar!" ())

;; Set up for a new game.
(DEFUN INIT ()
  (OR (AND (BOUNDP 'FONTS:SHIP) FONTS:SHIP)
      (LOAD "SYS:FONTS;SHIP QFASL" NIL NIL NIL T))
  (OR (BOUNDP 'SPACEWAR-WINDOW)
      (SETQ SPACEWAR-WINDOW
            (LET ((SIZE (- (MIN (SEND TV:DEFAULT-SCREEN ':WIDTH)
                                (SEND TV:DEFAULT-SCREEN ':HEIGHT))
                           20)))
              (SETQ MINHPOS (* UPD 5) MINVPOS (* UPD 5)
                    MAXHPOS (* UPD (- SIZE 6)) MAXVPOS (* UPD (- SIZE 6)))
              (TV:MAKE-WINDOW 'SPACEWAR-WINDOW ':INSIDE-HEIGHT SIZE ':INSIDE-WIDTH SIZE))))
  (SEND SPACEWAR-WINDOW ':SELECT)
  (AND (BOUNDP 'OBJECTS)
       (DOLIST (OBJECT OBJECTS)
         (AND (EQ (OBJECT-TYPE OBJECT) 'TORP)
              (NOT (MEMQ OBJECT FREE-TORPS))
              (PUSH OBJECT FREE-TORPS))
         (FUNCALL (OBJECT-BLINKER OBJECT) ':SET-VISIBILITY NIL)))
  (SETQ FREE-TORPS (SI:ELIMINATE-DUPLICATES FREE-TORPS))
  (SETQ OBJECTS NIL SUNS NIL SHIPS NIL HYPERSPACE-RETURN-SCHEDULE NIL)
  (SETQ TIME-TILL-GAME-END NIL)
  (SEND SPACEWAR-WINDOW ':CLEAR-SCREEN)
  (FORMAT SPACEWAR-WINDOW "
Welcome to Spacewar, the first video game -- invented around 1960.

Each player controls one space ship.  You can accelerate forward,
turn, fire torpedoes forward, and go into hyperspace.

The left ship is controlled by the left-hand bank of shift keys,
and the right ship by the right-hand bank of shift keys.
The TOP key accelerates the ship.  The two keys underneath TOP
/(HYPER and SUPER on the left, META and SUPER on the right)
turn the ship.

To fire torpedoes, use the CONTROL key.  After firing a torpedo,
there is a delay before you can fire again.  Holding down CONTROL
fires at the maximum rate.

Go into hyperspace by pressing the SHIFT key (sorry, not the HYPER key!)
You disappear for a while, and come back at a random position with a
random velocity.  But sometimes you fail to come back!  Use it only
in emergencies.

You have only a fixed supply of torpedoes and fuel.  If both ships run
out of both, the game ends.

Refer to the source file for many parameters which you can change to
modify the game.  For example, try negative gravity.  Changing the
parameters is a traditional part of the game.  This window is a Lisp
Listener, so you can type expressions now to set parameters.

Depress any control, meta, super or hyper key to start the game.")
  (PROCESS-WAIT "Start Game" #'(LAMBDA ()
                                 (OR (TV:KEY-STATE ':CONTROL)
                                     (TV:KEY-STATE ':META)
                                     (TV:KEY-STATE ':SUPER)
                                     (TV:KEY-STATE ':HYPER))))
  (SEND SPACEWAR-WINDOW ':CLEAR-SCREEN)
  (LET ((WIDTH (- MAXHPOS MINHPOS)))
    (AND SUN-FLAG (MAKE-SUN (FLOOR WIDTH 2) (FLOOR WIDTH 2)))
    (MAKE-SHIP 'FOO #/@ (FLOOR WIDTH 6) (FLOOR WIDTH 6) 0.0)
    (MAKE-SHIP 'BAR #/` (- WIDTH (FLOOR WIDTH 6)) (- WIDTH (FLOOR WIDTH 6)) 3.14159)))

;; Create a ship, and put it on the list of objects.
;; The ship name should be a symbol.  The ship becomes the value of that symbol.
;; The base-char is the ascii code for the character which is the first
;; of the 32 character codes used for displaying this model of ship.
;; DIRECTION is which way the ship should face.  Typical values are 0 and 3.14159.
(DEFUN MAKE-SHIP (NAME BASE-CHAR HPOS VPOS DIRECTION)
  (LET ((BLINKER (TV:MAKE-BLINKER SPACEWAR-WINDOW
                                  'TV:CHARACTER-BLINKER
                                  ':FONT FONTS:SHIP
                                  ':CHAR BASE-CHAR)))
    (SET-BLINKER-CURSORPOS BLINKER HPOS VPOS)
    (PUSH (SET NAME
               (MAKE-OBJECT
                 OBJECT-NAME NAME OBJECT-BASE-CHAR BASE-CHAR
                 OBJECT-TYPE 'SHIP
                 OBJECT-RADIUS 'SHIP-RADIUS
                 OBJECT-HPOS HPOS OBJECT-VPOS VPOS
                 OBJECT-HVEL 0 OBJECT-VVEL 0
                 OBJECT-ACCELERATION 0
                 OBJECT-ANGULAR-VEL 0
                 OBJECT-COLLISION-FUNCTION 'SHIP-COLLISION-FUNCTION
                 OBJECT-IMMOVABLE-FLAG NIL
                 OBJECT-TORP-SUPPLY SHIP-TORP-SUPPLY
                 OBJECT-FUEL-SUPPLY SHIP-FUEL-SUPPLY
                 OBJECT-LAST-FIRING-TIME -1000
                 OBJECT-LAST-HYPERSPACE-TIME -1000
                 OBJECT-BLINKER BLINKER))
          OBJECTS)
    (SET-DIRECTION (CAR OBJECTS) DIRECTION)
    (FUNCALL BLINKER ':SET-VISIBILITY T))
  (PUSH (CAR OBJECTS) SHIPS)
  (CAR OBJECTS))

;; Decoding the control switches

;; Each element of BIT-LIST looks like
;; (ship-name accel-bit turn-right-bit turn-left-bit fire-bit hyperspace-bit)
(DEFCONST SHIP-KEY-ALIST
          '((FOO :LEFT-TOP :LEFT-SUPER :LEFT-HYPER :LEFT-CONTROL :LEFT-SHIFT)
            (BAR :RIGHT-TOP :RIGHT-SUPER :RIGHT-META :RIGHT-CONTROL :RIGHT-SHIFT)))

(DEFUN DECODE-BITS ()
  (DOLIST (SHIP-KEYS SHIP-KEY-ALIST)
    (LET ((SHIP (SYMEVAL (CAR SHIP-KEYS))))
      (COND ((NULL (OBJECT-DEAD-FLAG SHIP))
             (COND ((AND (TV:KEY-STATE (SECOND SHIP-KEYS))
                         (NOT (ZEROP (OBJECT-FUEL-SUPPLY SHIP))))
                    (SETF (OBJECT-FUEL-SUPPLY SHIP)
                          (1- (OBJECT-FUEL-SUPPLY SHIP)))
                    (SETF (OBJECT-ACCELERATION SHIP) SHIP-ACCELERATION))
                   (T (SETF (OBJECT-ACCELERATION SHIP) 0)))
             (COND ((TV:KEY-STATE (THIRD SHIP-KEYS))
                    (SETF (OBJECT-ANGULAR-VEL SHIP) SHIP-TURNING-RATE))
                   ((TV:KEY-STATE (FOURTH SHIP-KEYS))
                    (SETF (OBJECT-ANGULAR-VEL SHIP) (- SHIP-TURNING-RATE)))
                   (T (SETF (OBJECT-ANGULAR-VEL SHIP) 0)))
             (COND ((TV:KEY-STATE (FIFTH SHIP-KEYS))
                    (TRY-TO-FIRE SHIP)))
             (COND ((TV:KEY-STATE (SIXTH SHIP-KEYS))
                    (TRY-TO-HYPERSPACE SHIP))))))))

(DEFUN TRY-TO-FIRE (SHIP)
  (AND (< (+ TORP-RELOAD-TIME (OBJECT-LAST-FIRING-TIME SHIP))
          CURRENT-CYCLE-NUMBER)
       (FIRE-TORP SHIP)))

(DEFUN TRY-TO-HYPERSPACE (SHIP)
  (AND (< (+ HYPERSPACE-RELOAD-TIME (OBJECT-LAST-HYPERSPACE-TIME SHIP))
          CURRENT-CYCLE-NUMBER)
       (HYPERSPACE SHIP)))

;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Lisp Machine Mouse Handler - revised to use microcode tracking

;;; These variables are low-level and more or less private to this file
;;; The higher-level variables that have to do with the mouse are declared
;;; in TVDEFS.

;these used to be completely magic - set up by the cold load
;builder, and used by the microcode.  now they are just
;wireable arrays
(defvar mouse-cursor-page)
(defvar MOUSE-CURSOR-PATTERN)   ;art-1b 32x32
(defvar MOUSE-BUTTONS-BUFFER)   ;art-inum 32
(defvar MOUSE-X-SCALE-ARRAY)    ;art-inum 16
(defvar MOUSE-Y-SCALE-ARRAY)    ;art-inum 16
;(remprop 'mouse-cursor-pattern 'system-constant)
;(remprop 'mouse-buttons-buffer 'system-constant)
;(remprop 'mouse-x-scale-array 'system-constant)
;(remprop 'mouse-y-scale-array 'system-constant)

(DEFVAR MOUSE-LAST-X 0
  "Previous value of MOUSE-X.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-Y 0
  "Previous value of MOUSE-Y.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-BUTTONS 0
  "Previous value of MOUSE-BUTTONS.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-BUTTONS-TIME NIL
  "Value of FIXNUM-MICROSECOND-TIME when buttons last changed.")
(DEFVAR MOUSE-LAST-BUTTONS-X NIL
  "Value of MOUSE-X when last a button changed.")
(DEFVAR MOUSE-LAST-BUTTONS-Y NIL
  "Value of MOUSE-Y when last a button changed.")
(DEFVAR MOUSE-BUTTONS-IN-PROGRESS NIL
  "If non-NIL, next state for buttons to enter.")

(DEFVAR USE-KBD-BUTTONS T
  "T says interpret the roman keys (I, II, III) as mouse clicks.")
(DEFVAR KBD-BUTTONS 0)                  ;buttons input via roman numerials on new keyboard.
                                        ;These get IORed with buttons from the mouse.
(DEFCONST MOUSE-BOUNCE-TIME 2000.
  "Delay in usec after button raised or lifted before we check for another change.")
(DEFCONST MOUSE-DOUBLE-CLICK-TIME 200000.
  "Delay in usec before deciding there has been no double click.")

;(DEFVAR MOUSE-RECONSIDER)              ;T => mouse process should return to overseer
                                        ;and decide anew which window should get the mouse.
                                        ;For use by :MOUSE-MOVES methods, etc.
                                        ;Declared in TVDEFS
(DEFVAR MOUSE-CURSOR-CLOSED-STATE 1)    ;1 if cursor not being drawn by microcode,
                                        ;2 if it is

;(DEFCONST MOUSE-REG1 #o764104)         ;Unibus address of buttons and Y position
;(DEFCONST MOUSE-REG2 #o764106)         ;Unibus address of raw quadrature and X position


;;;; Low-level routines

;;; MOUSE-INPUT blocks until the mouse status changes (it moves or a button
;;; is depressed or raised).  It then returns 6 values: delta-X, delta-Y,
;;; buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y
;;; (the current position normally, but if any buttons changed, the position then).
;;; There are 3 coordinate systems involved:
;;;  Table coordinates - physical motion of the mouse
;;;                      Speeds are expressed in terms of these.
;;;  Mouse coordinates - These are the scaled table coordinates.  Only deltas
;;;                      are valid here.
;;;  Screen coordinates - these are where the mouse-blinker appears on
;;;                      the TV screen; the same as mouse coordinates except
;;;                      for non-simple geometry caused by e.g. scroll bars.
;;;                      These appear in the variables MOUSE-X and MOUSE-Y
;;; Note that because of non-simple geometry, the deltas returned by MOUSE-INPUT
;;; are not necessarily equal to the change in MOUSE-X and MOUSE-Y.

(DEFUN MOUSE-INPUT (&OPTIONAL (WAIT-FLAG T))
  "Wait until mouse moves or a button goes up or down, then report what happened.
The values are the delta-x, the delta-y (amounts of mouse motion),
buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y
/(the current position normally, but if any buttons changed, the position then)."
  ;; Await a change in hardware status from what it was last time
  (COND (WAIT-FLAG
         ;; This sleep makes it reasonable for the mouse process to be high priority.
         ;; It insures that moving the mouse does not lock out lower priority
         ;; processes for a long time.  This constant may want to be tweaked.  A
         ;; value of 1 (1/60 of a second) does not noticably affect mouse response.
         (PROCESS-SLEEP 1.)
         (PROCESS-WAIT "MOUSE" #'(LAMBDA () (OR MOUSE-WAKEUP MOUSE-RECONSIDER)))))
  ;; Clear wakeup flag unless there are buffered mouse button transitions, since we
  ;; might not read all of them before calling MOUSE-INPUT again.
  (SETQ MOUSE-WAKEUP ( MOUSE-BUTTONS-BUFFER-IN-INDEX MOUSE-BUTTONS-BUFFER-OUT-INDEX))
  ;; Compute delta-X and delta-Y in screen coordinates
  (LET ((DELTA-X (- MOUSE-X MOUSE-LAST-X))
        (DELTA-Y (- MOUSE-Y MOUSE-LAST-Y))
        (GLITCH-X NIL) (GLITCH-Y NIL) NEW-BUTTONS CHANGED-BUTTONS)
    (INCF MOUSE-LAST-X DELTA-X)
    (INCF MOUSE-LAST-Y DELTA-Y)
    ;; Compute change in button status
    (MULTIPLE-VALUE (NEW-BUTTONS MOUSE-LAST-BUTTONS-TIME
                     MOUSE-LAST-BUTTONS-X MOUSE-LAST-BUTTONS-Y)
      (MOUSE-BUTTONS))
    (SETQ CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
          MOUSE-LAST-BUTTONS NEW-BUTTONS)
    ;; Force blinker to stay within mouse-sheet.  If the mouse moves during this
    ;; computation, it will glitch back.  So we only SETQ the variables
    ;; if the mouse position actually needs to be changed, rather than using
    ;; MAX and MIN which would be more readable.
    (IF (> 0 MOUSE-X)
        (SETQ GLITCH-X 0))
    (IF ( (SHEET-WIDTH MOUSE-SHEET) MOUSE-X)
        (SETQ GLITCH-X (1- (SHEET-WIDTH MOUSE-SHEET))))
    (IF (> 0 MOUSE-Y)
        (SETQ GLITCH-Y 0))
    (IF ( (SHEET-HEIGHT MOUSE-SHEET) MOUSE-Y)
        (SETQ GLITCH-Y (1- (SHEET-HEIGHT MOUSE-SHEET))))
    ;; If mouse blinker needs to be glitched, do so
    (IF (OR GLITCH-X GLITCH-Y)
        (WITHOUT-INTERRUPTS
           (%OPEN-MOUSE-CURSOR)
           (IF GLITCH-X
               (SETQ MOUSE-LAST-X (SETQ MOUSE-X GLITCH-X)))
           (IF GLITCH-Y
               (SETQ MOUSE-LAST-Y (SETQ MOUSE-Y GLITCH-Y)))
           (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
                 PREPARED-SHEET NIL)))
    (VALUES DELTA-X
            DELTA-Y
            (LOGAND NEW-BUTTONS CHANGED-BUTTONS)
            (BOOLE 2 NEW-BUTTONS CHANGED-BUTTONS) ;BOOLE 2 is ANDCA
            (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-X MOUSE-LAST-BUTTONS-X)
            (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-Y MOUSE-LAST-BUTTONS-Y))))

(DEFUN MOUSE-BUTTONS (&OPTIONAL PEEK)
  "returns a word with a 1 for each button currently held down, plus related info.
The remaining values are the time when that was the true state of the buttons,
and the X and Y coordinates of the mouse then.
PEEK means to look at the state without pulling anything out of the buffer
/(processes other than the mouse process use this)."
  (DECLARE (VALUES MOUSE-LAST-BUTTONS MOUSE-LAST-BUTTONS-TIME MOUSE-X MOUSE-Y))
  (LET ((TEM MOUSE-BUTTONS-BUFFER-OUT-INDEX))
    (COND (MOUSE-BUTTONS-IN-PROGRESS
           (VALUES (PROG1 MOUSE-BUTTONS-IN-PROGRESS (SETQ MOUSE-BUTTONS-IN-PROGRESS NIL))
                   MOUSE-LAST-BUTTONS-TIME
                   MOUSE-LAST-BUTTONS-X
                   MOUSE-LAST-BUTTONS-Y))
          ((= TEM MOUSE-BUTTONS-BUFFER-IN-INDEX)
           (VALUES (LOGIOR (IF USE-KBD-BUTTONS KBD-BUTTONS 0)
                           (select processor-type-code
                             ;(si:cadr-type-code (LDB #o1403 (%UNIBUS-READ MOUSE-REG1)))
                             (si:lambda-type-code (%lambda-mouse-buttons))
                             (si:explorer-type-code (%lambda-mouse-buttons))
                             ))
                   (TIME:FIXNUM-MICROSECOND-TIME) MOUSE-X MOUSE-Y))
          (T (OR PEEK (SETQ MOUSE-BUTTONS-BUFFER-OUT-INDEX (\ (+ TEM 4) 32.)))
             (VALUES (LOGIOR (IF USE-KBD-BUTTONS KBD-BUTTONS 0)
                             (AREF MOUSE-BUTTONS-BUFFER (+ TEM 3)))
                     (AREF MOUSE-BUTTONS-BUFFER TEM)
                     (AREF MOUSE-BUTTONS-BUFFER (+ TEM 1))
                     (AREF MOUSE-BUTTONS-BUFFER (+ TEM 2)))))))

(DEFUN MOUSE-DISCARD-CLICKAHEAD ()
  "Discard any complete mouse clicks already recorded by the microcode."
  (SETQ MOUSE-BUTTONS-BUFFER-OUT-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX
        MOUSE-BUTTONS-IN-PROGRESS NIL))

(DEFUN MOUSE-DEFER-BUTTONS (BU BD)
  "Defer a change in the mouse buttons, to be handled later.
It is added at the front of the buffer of clicks coming from the microcode.
BU is a mask of buttons that went up, and BD a mask of those that went down."
  (IF (OR (NOT (ZEROP BU)) (NOT (ZEROP BD)))
      (SETQ MOUSE-BUTTONS-IN-PROGRESS MOUSE-LAST-BUTTONS
            MOUSE-LAST-BUTTONS (LOGIOR BU
                                       (BOOLE 2 BD      ;BOOLE 2 is ANDCA
                                              MOUSE-LAST-BUTTONS)))))

(DEFCONST *MOUSE-INCREMENTING-KEYSTATES* '(:SHIFT)
  "If any key in this list is down, it turns a single click into a double.")

(DEFUN MAKE-MOUSE-CHAR (BUTTON N-CLICKS)
  "Make a mouse chararacter representing clicking N-CLICKS times on BUTTON (1, 2 or 3)"
  (DPB 1 %%KBD-MOUSE (DPB N-CLICKS %%KBD-MOUSE-N-CLICKS BUTTON)))

(DEFUN CHAR-MOUSE-P (CHAR)
  "T if CHAR is a mouse character."
  (LDB-TEST %%KBD-MOUSE CHAR))

;character lossage
(DEFUN MOUSE-BUTTON-ENCODE (BD &AUX BUTTON MASK CH TIME
                            (NEW-BUTTONS MOUSE-LAST-BUTTONS)
                            (NEW-TIME MOUSE-LAST-BUTTONS-TIME))
  "Look at mouse button transitions and detect double clicks.
BD is a mask of buttons that went down on the initial transition;
it presumably came from MOUSE-INPUT.
The value is NIL if no button is pushed (BD is 0),
or #o2000 + 8 N + B, where B is the bit number in the button word,
and N is one less than the number of clicks."
  (COND (( (SETQ BUTTON (1- (HAULONG BD))) 0)  ;Pick a button that was just pushed
         (SETQ MASK (LSH 1 BUTTON)
               CH  (MERGE-SHIFT-KEYS (TV:MAKE-MOUSE-CHAR BUTTON 0))
               TIME MOUSE-LAST-BUTTONS-TIME)
         ;; Each incrementing key that is held down
         ;; counts as an extra click in the number of clicks.
         (DOLIST (KEY *MOUSE-INCREMENTING-KEYSTATES*)
           (IF (KEY-STATE KEY)
               (INCF CH 8.)))
;        )
         (PROG1
           (LOOP NAMED MOUSE-BUTTON-ENCODE      ;Do forever (until guy's finger wears out)
             UNLESS MOUSE-DOUBLE-CLICK-TIME
              RETURN CH
             DOING
             ;; Ignore any clicking during the bounce delay
             (LOOP DOING (MULTIPLE-VALUE (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
                   UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
                   FINALLY (SETQ TIME NEW-TIME))
             ;; Look for button to be lifted, or for double-click timeout
             (LOOP WHILE (BIT-TEST MASK NEW-BUTTONS)
                   DO (MULTIPLE-VALUE (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
                   WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
                     ;; Timed-out with button still down
                     DO (RETURN-FROM MOUSE-BUTTON-ENCODE CH)
                   FINALLY (SETQ TIME NEW-TIME))
             ;; Button was lifted, do another bounce delay
             (LOOP DOING (MULTIPLE-VALUE (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
                   UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
                   FINALLY (SETQ TIME NEW-TIME))
             ;; Now watch for button to be pushed again
             (LOOP UNTIL (BIT-TEST MASK NEW-BUTTONS)
                   DO (MULTIPLE-VALUE (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS))
                   WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
                     ;; Timed-out with button still up
                     DO (RETURN-FROM MOUSE-BUTTON-ENCODE CH)
                   FINALLY (SETQ CH (+ CH 8.)   ;Count multiplicity of clicks
                                 TIME NEW-TIME))
             ;; Continue scanning (for triple click)
             )
           (SETQ MOUSE-LAST-BUTTONS NEW-BUTTONS
                 MOUSE-LAST-BUTTONS-TIME NEW-TIME)))))

(DEFVAR *DEFAULT-MOUSE-SPEEDS-FOR-HACK* '(.5 80. .8 120. 1.2 180. 2 320. 4)
  "The system default values supplied to MOUSE-SPEED-HACK")
(DEFUN MOUSE-SPEED-HACK (&REST SPECS)
  "Specs are SCALE-FACTOR SPEED-BREAK SCALE-FACTOR SPEED-BREAK ... SCALE-FACTOR
Each SCALE-FACTOR applies to speeds up to the following SPEED-BREAK.
The last SCALE-FACTOR applies to all higher speeds.
The value of *TV::DEFAULT-MOUSE-SPEEDS-FOR-HACK* are used if no args are supplied
These apply to both X and Y."
  (IF (NULL SPECS) (SETQ SPECS *DEFAULT-MOUSE-SPEEDS-FOR-HACK*))
  (LOOP FOR (SCALE SPEED) ON SPECS BY 'CDDR
        FOR I FROM 0 BY 2
     DO (SETF (AREF TV:MOUSE-X-SCALE-ARRAY I) (OR SPEED MOST-POSITIVE-FIXNUM))
        (SETF (AREF TV:MOUSE-Y-SCALE-ARRAY I) (OR SPEED MOST-POSITIVE-FIXNUM))
        (SETF (AREF TV:MOUSE-X-SCALE-ARRAY (1+ I)) (// (ROUND (* 2 SCALE 1024.)) 3))
        (SETF (AREF TV:MOUSE-Y-SCALE-ARRAY (1+ I)) (// (ROUND (* 3 SCALE 1024.)) 5))))

;;;; Middle-level routines

(DEFVAR MOUSE-PROCESS)                  ;This global-process is in charge of the mouse
(DEFVAR MOUSE-BLINKER NIL
  "The blinker that is currently being used to track the mouse.")
;(DEFVAR MOUSE-WINDOW)                  ;Window controlling the mouse, NIL if none
;(DEFVAR WINDOW-OWNING-MOUSE)           ;NIL, or window which has seized the mouse, or
;                                       ;T if someone has seized the mouse and can't identify
;                                       ;himself as any particular window,
;                                       ;or STOP to make the mouse process do nothing.
;(DEFVAR MOUSE-X)                       ;X coordinate of MOUSE-BLINKER
;(DEFVAR MOUSE-Y)                       ;Y coordinate of MOUSE-BLINKER
;(DEFVAR MOUSE-SHEET NIL)               ;Which sheet MOUSE-BLINKER lives on
(DEFVAR MOUSE-WARP NIL
  "Set to T whenever mouse is /"warped/" -- moved virtually by program control.")

(DEFUN MOUSE-STANDARD-BLINKER (&OPTIONAL (WINDOW (WINDOW-OWNING-MOUSE)))
  "Set the mouse blinker to the standard kind (with a north-west arrow.)"
  (AND (SYMBOLP WINDOW)
       (SETQ WINDOW MOUSE-SHEET))
  (SEND WINDOW :MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SHEET :MOUSE-STANDARD-BLINKER) ()
  (SEND SUPERIOR :MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SCREEN :MOUSE-STANDARD-BLINKER) ()
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON
                                :SET-CHARACTER 6 'FONTS:MOUSE))

(DEFMETHOD (SCREEN :MOUSE-FONT) ()
  (SEND SELF :PARSE-FONT-DESCRIPTOR 'FONTS:MOUSE))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :OFFSETS) ()
  (VALUES X-OFFSET Y-OFFSET))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :SET-OFFSETS) (X Y)
  (WITH-BLINKER-READY T
    (COND ((NULL VISIBILITY)                    ;Don't open if visibility NIL
           (SETQ X-OFFSET X Y-OFFSET Y))        ; (especially the mouse cursor!)
          ((OR (NEQ X-OFFSET X)                 ;Only blink if actually moving blinker
               (NEQ Y-OFFSET Y))
           (OPEN-BLINKER SELF)
           (SETQ X-OFFSET X Y-OFFSET Y)
           (SETQ TIME-UNTIL-BLINK 0)))))

(DEFMETHOD (MOUSE-BLINKER-FAST-TRACKING-MIXIN :SET-OFFSETS) (X Y)
  (SETQ X-OFFSET X
        Y-OFFSET Y)
  (IF (EQ MOUSE-BLINKER SELF)
      (WITHOUT-INTERRUPTS
        (%OPEN-MOUSE-CURSOR)
        (SETQ MOUSE-CURSOR-X-OFFSET X
              MOUSE-CURSOR-Y-OFFSET Y
              MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
              PREPARED-SHEET NIL))))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :TRACK-MOUSE) ()
  (WITHOUT-INTERRUPTS
    (%OPEN-MOUSE-CURSOR)
    (SETQ MOUSE-BLINKER SELF
          MOUSE-CURSOR-CLOSED-STATE 1
          MOUSE-CURSOR-X-OFFSET X-OFFSET
          MOUSE-CURSOR-Y-OFFSET Y-OFFSET)))

(DEFMETHOD (MOUSE-BLINKER-FAST-TRACKING-MIXIN :TRACK-MOUSE) ()
  (WITHOUT-INTERRUPTS
    (SEND SELF :SET-VISIBILITY NIL)             ;We are not in charge of drawing anything
    (%OPEN-MOUSE-CURSOR)                        ;Get rid of old microcode cursor
    (SETQ MOUSE-BLINKER SELF
          MOUSE-CURSOR-CLOSED-STATE 2
          MOUSE-CURSOR-X-OFFSET X-OFFSET
          MOUSE-CURSOR-Y-OFFSET Y-OFFSET)
    (MULTIPLE-VALUE (MOUSE-CURSOR-WIDTH MOUSE-CURSOR-HEIGHT) (SEND SELF :SIZE))
    (SETQ MOUSE-CURSOR-WIDTH (* MOUSE-CURSOR-WIDTH (SHEET-BITS-PER-PIXEL SHEET)))
    (COND ((OR (> MOUSE-CURSOR-WIDTH 32.) (> MOUSE-CURSOR-HEIGHT 32.))
           (SETQ MOUSE-CURSOR-CLOSED-STATE 1)   ;Oops, too big to use microcode tracking
           (SEND SELF :SET-VISIBILITY T))
          (T ;; Draw self into microcode cursor
             (BITBLT 0 32. 32. MOUSE-CURSOR-PATTERN 0 0 MOUSE-CURSOR-PATTERN 0 0)
             (LETF ((X-POS 0)
                    (Y-POS 0)
                    ((SHEET-SCREEN-ARRAY SHEET) MOUSE-CURSOR-PATTERN)
                    ((SHEET-LOCATIONS-PER-LINE SHEET) 1))
               ;; Cause recalculation of sheet parameters, because SCREEN-ARRAY changed
               (SETQ SYS:%CURRENT-SHEET NIL)
               (SEND SELF :BLINK)
               ;; SCREEN-ARRAY will change back when unbinding gets done
               (SETQ SYS:%CURRENT-SHEET NIL)
               (SETQ PREPARED-SHEET NIL)
               (SETQ PHASE NIL))
             (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE)))))

;;; All the standard mouse blinkers are character blinkers, so enable fast tracking for them.
(DEFFLAVOR MOUSE-CHARACTER-BLINKER () (MOUSE-BLINKER-FAST-TRACKING-MIXIN CHARACTER-BLINKER))
(DEFFLAVOR MOUSE-RECTANGULAR-BLINKER () (MOUSE-BLINKER-MIXIN RECTANGULAR-BLINKER))
(DEFFLAVOR MOUSE-HOLLOW-RECTANGULAR-BLINKER
        ()
        (MOUSE-BLINKER-MIXIN HOLLOW-RECTANGULAR-BLINKER))

(COMPILE-FLAVOR-METHODS MOUSE-CHARACTER-BLINKER MOUSE-RECTANGULAR-BLINKER
                        MOUSE-HOLLOW-RECTANGULAR-BLINKER)

(DEFVAR MOUSE-BLINKER-TYPES NIL)
(DEFUN MOUSE-DEFINE-BLINKER-TYPE (TYPE CREATION-FUN)
  (SETQ MOUSE-BLINKER-TYPES (DELQ (ASSQ TYPE MOUSE-BLINKER-TYPES) MOUSE-BLINKER-TYPES))
  (PUSH (CONS TYPE CREATION-FUN) MOUSE-BLINKER-TYPES)
  (AND MOUSE-SHEET (MOUSE-GET-BLINKER TYPE)))

(DEFUN MOUSE-GET-BLINKER (TYPE &OPTIONAL (SHEET MOUSE-SHEET)
                               &AUX (SCREEN (SHEET-GET-SCREEN SHEET))
                               BLINKERS)
  "Get a mouse blinker of type TYPE that can appear on SHEET.
TYPE is a keyword on MOUSE-BLINKER-TYPES."
  (LET ((BE (ASSQ TYPE MOUSE-BLINKER-TYPES)))
    (OR BE (FERROR "~A is unknown mouse blinker type" TYPE))
    (LET ((BL (CDR (ASSQ TYPE (SETQ BLINKERS (SEND SCREEN :MOUSE-BLINKERS))))))
      (WHEN (NULL BL)
        (SETQ BL (SEND (CDR BE) SCREEN))
        (PUSH (CONS TYPE BL) BLINKERS)
        (SEND SCREEN :SET-MOUSE-BLINKERS BLINKERS))
      (BLINKER-SET-SHEET BL SHEET)
      BL)))

(DEFUN MOUSE-SET-BLINKER-DEFINITION (TYPE X-OFFSET Y-OFFSET VISIBILITY
                                     OPERATION &REST OPERATION-ARGS)
  "Switch to a mouse blinker of type TYPE.
Send it OPERATION with arguments OPERATION-ARGS before using it.
X-OFFSET and Y-OFFSET are offsets which we subtract from the mouse position
to get the position to display the blinker.
VISIBILITY is the visibility for the blinker (usually T)."
  (LET ((BL (MOUSE-GET-BLINKER TYPE)))
    (AND MOUSE-BLINKER (NEQ BL MOUSE-BLINKER) (SEND MOUSE-BLINKER :SET-VISIBILITY NIL))
    (WHEN OPERATION (LEXPR-SEND BL OPERATION OPERATION-ARGS))
    (WHEN X-OFFSET (SEND BL :SET-OFFSETS X-OFFSET Y-OFFSET))
    (SEND BL :SET-VISIBILITY (IF (EQ VISIBILITY ':ON) T VISIBILITY))
    (SEND BL :TRACK-MOUSE)
    BL))

;; Copied from LAD: RELEASE-3.WINDOW; MOUSE.LISP#264 on 2-Oct-86 16:48:15
(DEFUN MOUSE-SET-BLINKER (TYPE-OR-BLINKER &OPTIONAL X-OFFSET Y-OFFSET)
  "Switch to mouse blinker BLINKER, or to a blinker of type TYPE.
X-OFFSET and Y-OFFSET are offsets which we subtract from the mouse position
to get the position to display the blinker."
  (LET ((BL (IF (TYPEP TYPE-OR-BLINKER 'INSTANCE)
                TYPE-OR-BLINKER
              (MOUSE-GET-BLINKER TYPE-OR-BLINKER))))
    (WHEN X-OFFSET (SEND BL :SET-OFFSETS X-OFFSET Y-OFFSET))
    (AND (NEQ BL MOUSE-BLINKER) (SEND MOUSE-BLINKER :SET-VISIBILITY NIL))
    (SEND BL :SET-VISIBILITY T)
    (SEND BL :TRACK-MOUSE)
    BL))

(MOUSE-DEFINE-BLINKER-TYPE :CHARACTER-BLINKER
                           (LAMBDA (SCREEN)
                             (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                                    :VISIBILITY NIL
                                                    :FONT (SEND SCREEN :MOUSE-FONT)
                                                    :CHAR 6)))

;;; Old name, same as :CHARACTER-BLINKER.
(MOUSE-DEFINE-BLINKER-TYPE :CHARACTER
                           (LAMBDA (SCREEN)
                             (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                                    :VISIBILITY NIL
                                                    :FONT (SEND SCREEN :MOUSE-FONT)
                                                    :CHAR 6)))

;;; Until system 93, this was called :RECTANGLE-BLINKER!  What a crock.
(MOUSE-DEFINE-BLINKER-TYPE :RECTANGLE-CORNER-BLINKER
                           (LAMBDA (SCREEN)
                             (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                                    :VISIBILITY NIL
                                                    :FONT (SEND SCREEN :MOUSE-FONT)
                                                    :CHAR #o21)))

(MOUSE-DEFINE-BLINKER-TYPE :RECTANGLE-BLINKER
                           (LAMBDA (SCREEN)
                             (DEFINE-BLINKER SCREEN 'MOUSE-RECTANGULAR-BLINKER
                                                    :VISIBILITY NIL
                                                    :HEIGHT 8.
                                                    :WIDTH 8.)))

(DEFUN MOUSE-SET-BLINKER-CURSORPOS (&REST IGNORE)
  "Move the mouse blinker to the current mouse position."
  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
      (SEND MOUSE-BLINKER :OFFSETS)
    (BLINKER-SET-CURSORPOS MOUSE-BLINKER
                           (- MOUSE-X X-OFF (SHEET-INSIDE-LEFT MOUSE-SHEET))
                           (- MOUSE-Y Y-OFF (SHEET-INSIDE-TOP MOUSE-SHEET)))))

(DEFUN MOUSE-CALL-SYSTEM-MENU (&OPTIONAL (SUP MOUSE-SHEET))
  (PROCESS-RUN-FUNCTION '(:NAME "System Menu" :PRIORITY 10.)
                        (LAMBDA (SUP)
                          (USING-RESOURCE (MENU SYSTEM-MENU SUP)
                            (SEND MENU :CHOOSE)))
                        SUP))

;;; This function as a warm initialization
;;; to initialize the mouse process and associated variable.
(DEFUN MOUSE-INITIALIZE (&OPTIONAL (SHEET DEFAULT-SCREEN))
  (OR (BOUNDP 'MOUSE-PROCESS)                   ;If first time loaded, initialize everything
      (SETQ MOUSE-PROCESS (MAKE-PROCESS "Mouse" :SPECIAL-PDL-SIZE 2000.
                                                :PRIORITY 30.
                                                :WARM-BOOT-ACTION NIL)))
                ;Above warm-boot-action prevents the process from starting up
                ;until after these initializations have been completed.
  (SETQ MOUSE-WINDOW NIL
        WINDOW-OWNING-MOUSE NIL
        MOUSE-X 0
        MOUSE-Y 0
        MOUSE-SHEET SHEET)
  (set-up-mouse-arrays-warm)
  (%SET-MOUSE-SCREEN SHEET)
;  ;; Fill the mouse tracker's arrays with NIL instead of the garbage
;  ;; that they contain initially.  At least interpreted ASET won't work otherwise.
;  (LOOP FOR I FROM #o1640 BELOW #o1740
;     DO (%P-STORE-CONTENTS (+ A-MEMORY-VIRTUAL-ADDRESS I) NIL))
  ;; Set scaling and speed dependence.
  (MOUSE-SPEED-HACK)
  ;; Make sure at least one blinker of each type exists
  (MOUSE-GET-BLINKER :CHARACTER)
  (MOUSE-GET-BLINKER :RECTANGLE-CORNER-BLINKER)
  (MOUSE-GET-BLINKER :RECTANGLE-BLINKER)
  (MOUSE-GET-BLINKER 'FLASHY-CHARACTER)
  (AND MOUSE-BLINKER (BLINKER-SET-VISIBILITY MOUSE-BLINKER NIL))
  (MOUSE-STANDARD-BLINKER)
  (MOUSE-WARP (- (SHEET-INSIDE-WIDTH MOUSE-SHEET) 8.)
              (- (SHEET-INSIDE-HEIGHT MOUSE-SHEET) 16.))
  ;; Call MOUSE-INPUT once to flush any pending motion and update variables, but don't wait.
  (SETQ MOUSE-BUTTONS-BUFFER-OUT-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX)
  (MOUSE-INPUT NIL)
  (SETQ MOUSE-X-SPEED 0 MOUSE-Y-SPEED 0)
  ;; Start up the mouse process
  (SEND MOUSE-PROCESS :PRESET 'MOUSE-OVERSEER)
  (SEND MOUSE-PROCESS :RUN-REASON))

(DEFUN MOUSE-SET-SHEET (NEW-SHEET)
  "Specify the sheet for the mouse to track on.  Sets MOUSE-SHEET."
  (LOOP FOR SH = NEW-SHEET THEN (SHEET-SUPERIOR SH) UNTIL (NULL SH)
        UNLESS (SHEET-EXPOSED-P SH)
          DO (FERROR "Attempt to set MOUSE-SHEET to a non-visible sheet"))
  (WITH-MOUSE-USURPED
    (%OPEN-MOUSE-CURSOR)
    (SETQ MOUSE-SHEET NEW-SHEET)
    (%SET-MOUSE-SCREEN NEW-SHEET)
    (MOUSE-DISCARD-CLICKAHEAD)                  ;Since the coordinate system has changed
    (MOUSE-STANDARD-BLINKER)
    (MULTIPLE-VALUE-BIND (X Y) (SEND MOUSE-BLINKER :READ-CURSORPOS)
      (SETQ MOUSE-X (+ X (SHEET-INSIDE-LEFT MOUSE-SHEET))
            MOUSE-Y (+ Y (SHEET-INSIDE-TOP MOUSE-SHEET))))))

(DEFUN MOUSE-WARP (X Y)
  "Sets mouse position on MOUSE-SHEET to X and Y."
  (UNLESS (AND (= MOUSE-X X) (= MOUSE-Y Y))
    (SETQ MOUSE-WARP T)
    (WITHOUT-INTERRUPTS
      (%OPEN-MOUSE-CURSOR)
      (SETQ MOUSE-LAST-X
            (SETQ MOUSE-X (MAX 0 (MIN (1- (SHEET-WIDTH MOUSE-SHEET)) X))))
      (SETQ MOUSE-LAST-Y
            (SETQ MOUSE-Y (MAX 0 (MIN (1- (SHEET-HEIGHT MOUSE-SHEET)) Y))))
      (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
            PREPARED-SHEET NIL))
    (MOUSE-SET-BLINKER-CURSORPOS)
    (SETQ MOUSE-WAKEUP T)))                     ;Make sure the mouse tracker process notices

;;; This returns the lowest window under the mouse prepared to handle the given operation.
(DEFUN WINDOW-UNDER-MOUSE (&OPTIONAL METHOD (ACTIVE-CONDITION :ACTIVE) X Y)
  (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET (OR X MOUSE-X) (OR Y MOUSE-Y)
                            METHOD ACTIVE-CONDITION))

;;; This is the window or special thing that owns the mouse
(DEFUN WINDOW-OWNING-MOUSE (&OPTIONAL X Y)
  "Return the window that is allowed to handle the mouse now.
If X and Y are specified, then return the WINDOW that would
be handling the mouse if it were at that position."
  (OR WINDOW-OWNING-MOUSE
      (WINDOW-UNDER-MOUSE :HANDLE-MOUSE :EXPOSED X Y)))

(DEFUN WINDOW-OWNS-MOUSE-P (WINDOW &OPTIONAL X Y)
  "T if WINDOW is the window that is allowed to handle the mouse now.
If one of WINDOW's inferiors is the one, we also return T.
If X and Y are specified, then T if WINDOW is the window that would
be handling the mouse if it were at that position."
  (COND ((EQ WINDOW T)
         (EQ WINDOW-OWNING-MOUSE T))
        (WINDOW-OWNING-MOUSE
         (DO ((W WINDOW (SHEET-SUPERIOR W)))
             ((NULL W))
           (AND (EQ W WINDOW-OWNING-MOUSE) (RETURN T))))
        (T
         (AND (SHEET-EXPOSED-P WINDOW)
              (SHEET-CONTAINS-SHEET-POINT-P WINDOW MOUSE-SHEET
                                            (OR X MOUSE-X) (OR Y MOUSE-Y))))))

(DEFUN MOUSE-SEIZE ()
  "Make the currently selected window able to handle the mouse wherever it goes.
Even if the mouse moves out of the window, this window will still handle it.
Set WINDOW-OWNING-MOUSE to NIL to let other windows have a chance again."
  (SETQ WINDOW-OWNING-MOUSE SELECTED-WINDOW))

(DEFUN MOUSE-WAKEUP ()
  "Inform mouse tracking that the screen layout has changed."
; (SETQ MOUSE-RECONSIDER T)
  (SETQ MOUSE-WAKEUP T))

;;;; MOUSE-OVERSEER
;;; This is the top-level function of the mouse process.  It tracks the
;;; mouse and does default things with the buttons.  If the mouse enters
;;; the region of the screen occupied by a window that has its own mouse handler,
;;; that mouse handler is called.  It then has control of the mouse until
;;; it returns.  Both this function and specialized mouse handlers are
;;; to call the above low-level routines.  MOUSE-WINDOW is the window which
;;; has control of the mouse, or NIL if there is none.  Note that window
;;; need not be selected, nor exposed.  It does have to be active.
;;; Mouse handlers are free to mung the mouse blinker however they like.
;;; The mouse overseer is guaranteed to put it back.

;;; Most mouse handlers will return whenever WINDOW-OWNING-MOUSE says that the
;;; mouse has moved outside of the visible part of that window.  Some however,
;;; will not return until they feel like it.
;;; The convention to be used is up to the individual handler.

;;; Frobbing with MOUSE-RECONSIDER is for race-free interface with WITH-MOUSE-GRABBED
(DEFUN MOUSE-OVERSEER ()
  (ERROR-RESTART-LOOP ((SYS:ABORT ERROR) "Return to top level of MOUSE-PROCESS.")
    (DO-FOREVER
      (SETQ MOUSE-RECONSIDER NIL)
      (IF MOUSE-SHEET
          (MOUSE-SET-BLINKER-CURSORPOS))
      (COND ((EQ (WITHOUT-INTERRUPTS (SETQ MOUSE-WINDOW (WINDOW-OWNING-MOUSE))) 'STOP)
             (PROCESS-WAIT "Usurped"
                           (LAMBDA () (OR MOUSE-RECONSIDER      ;This can happen randomly
                                          (NEQ WINDOW-OWNING-MOUSE 'STOP)))))
            ((NOT (SYMBOLP MOUSE-WINDOW))
             (SEND MOUSE-WINDOW :HANDLE-MOUSE))
            (T
             ;; Standardize the blinker if no one else will
             (OR MOUSE-WINDOW (MOUSE-STANDARD-BLINKER MOUSE-SHEET))
             (MOUSE-DEFAULT-HANDLER MOUSE-WINDOW))))))

;;; Magic adjustments

;;; Magic top-level values for scroll bar parameters
;;; These can be bound by :HANDLE-MOUSE methods.
(DEFVAR MOUSE-SPEED 0)                          ;Speed mouse is travelling (inches per sec)

(DEFCONST SCROLL-BAR-MAX-SPEED 5)               ;Speed at which scroll bar gets ignored
(DEFCONST SCROLL-BAR-MAX-EXIT-SPEED 5)          ;Speed at which you leave scroll bar
(DEFCONST SCROLL-BAR-RELUCTANCE 10.)            ;Pixels before entering scroll bar
(DEFCONST SCROLL-BAR-WIDTH 40.)                 ;Width of scroll bar region

(DEFCONST MOUSE-FAST-MOTION-SPEED 25.)          ;Moving faster than this
(DEFCONST MOUSE-FAST-MOTION-CROSS-SIZE 30.)     ; triggers a cross 1 cm in diameter
(DEFCONST MOUSE-FAST-MOTION-CROSS-TIME 2500.)   ; which lasts this long (DO-loop units)

;;; This mouse handler serves for windows which want to do things the simple way.
;;; A second argument of T says that the window should have a scroll bar.
;;; This function is also used to track the mouse when it isn't inside any window,
;;; by calling it with an argument of NIL.
;;; An arg of T is used when the mouse has been seized by a process not
;;; for any specific window.
(DEFUN MOUSE-DEFAULT-HANDLER (WINDOW &OPTIONAL SCROLL-BAR-FLAG
                              &AUX (WINDOW-X-OFFSET 0) (WINDOW-Y-OFFSET 0)
                                   WINDOW-X WINDOW-Y
                                   (MOVE-METHOD (IF (EQ SCROLL-BAR-FLAG ':IN)
                                                    :MOUSE-MOVES-SCROLL :MOUSE-MOVES))
                                   (BUTTONS-METHOD (IF (EQ SCROLL-BAR-FLAG ':IN)
                                                       :MOUSE-BUTTONS-SCROLL :MOUSE-BUTTONS)))
  (OR (SYMBOLP WINDOW)
      (MULTIPLE-VALUE-SETQ (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
        (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)))
  (DO (HAND
       (OLD-OWNER WINDOW-OWNING-MOUSE WINDOW-OWNING-MOUSE)
       (X-OFFSET 0)
       (WAIT-FLAG NIL T))
      (MOUSE-RECONSIDER)
    (MULTIPLE-VALUE-BIND (DX DY BD BU X Y)
        (MOUSE-INPUT WAIT-FLAG)
      (DECLARE (IGNORE DY))
      ;; If asked to reconsider, do so immediately.
      ;; Don't bother updating blinker since it is likely to change soon, and
      ;; in any case we are going to be called back shortly.
      (IF MOUSE-RECONSIDER (RETURN NIL))
      ;; Update console-idle time when buttons pushed
      (IF (NOT (ZEROP BD)) (SETQ KBD-LAST-ACTIVITY-TIME (TIME)))
      (SETQ WINDOW-X (- X WINDOW-X-OFFSET)
            WINDOW-Y (- Y WINDOW-Y-OFFSET))
      ;; Approximate speed of the mouse in inches per second
      (SETQ MOUSE-SPEED (// (ISQRT (+ (* MOUSE-X-SPEED MOUSE-X-SPEED)
                                      (* MOUSE-Y-SPEED MOUSE-Y-SPEED)))
                            1.0s2))
      ;; If the mouse is moving incredibly fast, flash up something to
      ;; help the user find it.  Thus if you can't find the mouse, you must whip it.
      (IF (> MOUSE-SPEED MOUSE-FAST-MOTION-SPEED)
          (LET ((MIN-X 0) (MIN-Y 0)
                (MAX-X (1- (SHEET-INSIDE-WIDTH MOUSE-SHEET)))
                (MAX-Y (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET))))
            (LET ((XTOP (MAX (- MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE) MIN-Y))
                  (XBOTTOM (MIN (+ MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE 20) MAX-Y))
                  (XLEFT (MAX (- MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE) MIN-X))
                  (XRIGHT (MIN (+ MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE 20) MAX-X))
                  (XX (MAX MIN-X (MIN MOUSE-X (- MAX-X 20))))
                  (YY (MAX MIN-Y (MIN MOUSE-Y (- MAX-Y 20)))))
              (SHEET-IS-PREPARED (MOUSE-SHEET)
                (WITHOUT-INTERRUPTS
                  (%DRAW-RECTANGLE 16. (MAX (- XBOTTOM XTOP) 0) XX XTOP ALU-XOR MOUSE-SHEET)
                  (%DRAW-RECTANGLE (MAX (- XRIGHT XLEFT) 0) 20 XLEFT YY ALU-XOR MOUSE-SHEET)
                  (DOTIMES (I MOUSE-FAST-MOTION-CROSS-TIME) )
                  (%DRAW-RECTANGLE 16. (MAX (- XBOTTOM XTOP) 0) XX XTOP ALU-XOR MOUSE-SHEET)
                  (%DRAW-RECTANGLE (MAX (- XRIGHT XLEFT) 0) 20 XLEFT YY ALU-XOR MOUSE-SHEET)
                  )))))
      ;; X-OFFSET is how far out the left side of the window the mouse has moved,
      ;; or 0 if the mouse is inside the window.
      ;; If the window is at the left edge of the screen, MOUSE-X will not itself
      ;; move out the left edge of the window, but DX will.  When the mouse
      ;; reaches the left edge of the window, accumulate leftward motion
      ;; into X-OFFSET.  Rightward motion of more than SCROLL-BAR-RELUCTANCE
      ;; means that the mouse is well away from the left edge, so give up on X-OFFSET.
      (COND (( WINDOW-X 0)
             (SETQ X-OFFSET (IF (PLUSP X-OFFSET)
                                (MAX (- X-OFFSET DX) 1)
                              1)))              ;First time, don't use all of DX
            ((> WINDOW-X SCROLL-BAR-RELUCTANCE)
             (SETQ X-OFFSET 0)))
      ;; Consider entering the scroll bar.  [Perhaps this should be changed so that
      ;; it is in the move-handler rather than here.  Problem with that is X-OFFSET.]
      ;; If there is a scroll bar and we are entering it, activate it.
      ;; However, the mouse must move at least a certain distance past the left
      ;; edge of the window in order to qualify for scrolling (this is set by
      ;; the SCROLL-BAR-RELUCTANCE variable in the window).  Before entering
      ;; scroll bar, send a :MOUSE-MOVES message in order to let the window know
      ;; what's happening.
      (COND ((OR OLD-OWNER WINDOW-OWNING-MOUSE))        ;These disable scroll bar
            ((AND (EQ SCROLL-BAR-FLAG 'T) (PLUSP X-OFFSET))
             (COND ((NOT (SEND WINDOW :SEND-IF-HANDLES :SCROLL-BAR))
                    ;; Window doesn't really have a scroll bar.
                    (RETURN NIL))
                   ((AND SCROLL-BAR-MAX-SPEED
                         (> MOUSE-SPEED SCROLL-BAR-MAX-SPEED))
                    (RETURN NIL))               ;Too fast, pass right through
                   ((> X-OFFSET SCROLL-BAR-RELUCTANCE)
                    (IF (SYMBOLP WINDOW)
                        (MOUSE-SET-BLINKER-CURSORPOS MOVE-METHOD WINDOW-X WINDOW-Y)
                      (SEND WINDOW MOVE-METHOD WINDOW-X WINDOW-Y))
                    (RETURN (SEND WINDOW :HANDLE-MOUSE-SCROLL)))
                   (T (SETQ WINDOW-X 0))))      ;Don't escape the window yet
            ((EQ SCROLL-BAR-FLAG ':IN)
             ;; We are in the scroll bar.  Moving the mouse faster than the exit
             ;; speed, or moving it to the right by more than the scroll bar width,
             ;; will escape.  You cannot escape to the left.  This is different from
             ;; the previous algorithm, which involved a "hidden variable" which
             ;; let you out when you moved the mouse (but not the cursor) more
             ;; than a certain amount to left or right.
             (COND ((AND SCROLL-BAR-MAX-EXIT-SPEED
                         (> MOUSE-SPEED SCROLL-BAR-MAX-EXIT-SPEED))
                    ;; Moving like a bat, let the guy out of the scroll bar
                    (RETURN NIL))
                   ((MINUSP WINDOW-X)           ;Trying to go out left, shove back in
                    (WITHOUT-INTERRUPTS
                      (%OPEN-MOUSE-CURSOR)
                      (SETQ WINDOW-X 0)
                      (SETQ MOUSE-LAST-X (SETQ MOUSE-X WINDOW-X-OFFSET))
                      (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
                            PREPARED-SHEET NIL)))
                   ((> WINDOW-X SCROLL-BAR-WIDTH)       ;Escape out right
                    (RETURN NIL)))))
      ;; Update the position of the mouse before checking for button clicks, so
      ;; that button clicks get processed with knowledge of where the mouse
      ;; was when the button was first clicked.  The arguments to the move handler
      ;; may be where the mouse was when the button was clicked, whereas the
      ;; mouse cursor follows MOUSE-X and MOUSE-Y, which may be different.
      (SETQ MOUSE-WARP NIL)
      (IF (SYMBOLP WINDOW)
          (MOUSE-SET-BLINKER-CURSORPOS MOVE-METHOD WINDOW-X WINDOW-Y)
        (SEND WINDOW MOVE-METHOD WINDOW-X WINDOW-Y))
      ;; Check for all the ways of losing control of the mouse.
      (IF (COND ;; The move handler may have decided to warp the mouse so that it will not
            ;; move out of the window.  This test is a crock but should work.
            (MOUSE-WARP NIL)
            ;; Check for mouse ceasing to be grabbed.
            ((EQ WINDOW T)
             (NEQ WINDOW-OWNING-MOUSE T))
            ;; Check for window becoming grabbed.
            ((EQ WINDOW-OWNING-MOUSE T)
             (NEQ WINDOW T))
            ;; Check for some other window (not above this one) being greedy.
            (WINDOW-OWNING-MOUSE
             (NOT (SHEET-ME-OR-MY-KID-P WINDOW WINDOW-OWNING-MOUSE)))
            ;; Check for moving into a window when not in any
            ((NULL WINDOW)
             (WINDOW-OWNING-MOUSE X Y))
            ;; Check for leaving the boundaries of the current window
            ;; HYSTERETIC-WINDOW-MIXIN requires that we wait at least once before returning
            ((NOT (AND (SHEET-EXPOSED-P WINDOW)
                       ( WINDOW-X 0)
                       (< WINDOW-X (SHEET-WIDTH WINDOW))
                       ( WINDOW-Y 0)
                       (< WINDOW-Y (SHEET-HEIGHT WINDOW))))
             WAIT-FLAG)
            ;; Check for moving into an inferior of the current window
            ((NEQ (LOWEST-SHEET-UNDER-POINT WINDOW WINDOW-X WINDOW-Y :HANDLE-MOUSE :EXPOSED)
                  WINDOW)
             T))
          ;; Return to overseer, saving any pending button click.
          (RETURN (MOUSE-DEFER-BUTTONS BU BD)))
      ;; Now process button pushes if mouse is not seized
      (COND ((OR (ZEROP BD) (EQ WINDOW T) OLD-OWNER))
            ;; If over an exposed window, send it the button-push
            (WINDOW (SEND WINDOW BUTTONS-METHOD BD WINDOW-X WINDOW-Y))
            ;; Default action for left button is to select what mouse is pointing at
            ((BIT-TEST 1 BD)
             (AND (SETQ HAND (WINDOW-UNDER-MOUSE :MOUSE-SELECT :ACTIVE X Y))
                  ;; Next line temporarily papers over a bug with :MOUSE-SELECT
                  (GET-HANDLER-FOR HAND :SELECT)
                  (MOUSE-SELECT HAND)))
            ;; Default action for middle button is to switch to the main screen
            ((BIT-TEST 2 BD)
             (IF (TYPEP MOUSE-SHEET 'SCREEN)
                 (PROCESS-RUN-FUNCTION "Set mouse sheet" 'MOUSE-SET-SHEET DEFAULT-SCREEN)))
            ;; Default action for right button is to call the system menu
            ((BIT-TEST 4 BD)
             (MOUSE-BUTTON-ENCODE BD)           ;Satisfy those who double-click out of habit
             (MOUSE-CALL-SYSTEM-MENU))))))

(DEFUN MOUSE-WAIT (&OPTIONAL (OLD-X MOUSE-X) (OLD-Y MOUSE-Y) (OLD-BUTTONS MOUSE-LAST-BUTTONS)
                   (WHOSTATE "MOUSE"))
  "Wait for the mouse to move or a button transition.  For processes other than MOUSE-PROCESS.
If the arguments are supplied, we wait for the mouse state to be
different from them.  To avoid lossage, save the values of
MOUSE-X and MOUSE-Y and MOUSE-LAST-BUTTONS, use the saved values,
then pass the saved values to this function.
WHOSTATE is displayed in the who line while we wait."
  (PROCESS-WAIT WHOSTATE
    (LAMBDA (OLD-X OLD-Y OLD-BUTTONS)
      (OR ( MOUSE-X OLD-X)
          ( MOUSE-Y OLD-Y)
          ( MOUSE-LAST-BUTTONS OLD-BUTTONS)))
    OLD-X OLD-Y OLD-BUTTONS))

(DEFFLAVOR ESSENTIAL-MOUSE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :MOUSE-CLICK)))

(DEFMETHOD (ESSENTIAL-MOUSE :HANDLE-MOUSE) ()
  (MOUSE-STANDARD-BLINKER SELF)
  (MOUSE-DEFAULT-HANDLER SELF (SEND SELF :ENABLE-SCROLLING-P)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-CURSORPOS) (X Y)
  (SEND SELF :SET-MOUSE-POSITION (+ (SHEET-INSIDE-LEFT) X) (+ (SHEET-INSIDE-TOP) Y)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-POSITION) (X Y &AUX X-OFF Y-OFF)
  (COND ((SHEET-ME-OR-MY-KID-P SELF MOUSE-SHEET)
         (MULTIPLE-VALUE (X-OFF Y-OFF)
           (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
         (SETQ X (IF X
                     (+ X X-OFF)
                     MOUSE-X)
               Y (IF Y
                     (+ Y Y-OFF)
                     MOUSE-Y))
         (MOUSE-WARP X Y))
        (T (FERROR "Attempt to ~S on ~S, which is not related to ~S."
                   :SET-MOUSE-POSITION SELF 'MOUSE-SHEET))))

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-MOVES) MOUSE-SET-BLINKER-CURSORPOS)

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-BUTTONS) (BD X Y)
;character lossage
  (LET ((BUTTONS (MOUSE-BUTTON-ENCODE BD)))
    (IF (= BUTTONS #/MOUSE-3-2)
        (MOUSE-CALL-SYSTEM-MENU)
      (SEND SELF :MOUSE-CLICK BUTTONS X Y))))

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-CLICK) (BUTTONS X Y)
  (COND ((AND (= BUTTONS #/MOUSE-1-1)
              (NOT (SEND (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS) :SELF-OR-SUBSTITUTE-SELECTED-P))
              (OPERATION-HANDLED-P SELF :SELECT))       ;paper over a bug
         (MOUSE-SELECT SELF)
         T)
        (T
         (OR (SEND SELF :SEND-IF-HANDLES
                        :FORCE-KBD-INPUT `(:MOUSE-BUTTON ,BUTTONS ,SELF ,X ,Y))
             (AND (= BUTTONS #/MOUSE-3-1)
                  (MOUSE-CALL-SYSTEM-MENU)
                  T)
             (BEEP)))))                         ;click not handled

;character lossage
(DEFUN MERGE-SHIFT-KEYS (CHAR)
  "Return CHAR with the bits for various shift keys updated.
For example, the %%KBD-HYPER bit of the result will be 1
if a Hyper key is depressed right now."
  (%LOGDPB (IF (KEY-STATE :HYPER) 1 0)
           %%KBD-HYPER
           (DPB (IF (KEY-STATE :SUPER) 1 0)
                %%KBD-SUPER
                (DPB (IF (KEY-STATE :META) 1 0)
                     %%KBD-META
                     (DPB (IF (KEY-STATE :CONTROL) 1 0)
                          %%KBD-CONTROL
                          CHAR)))))

(DEFUN MOUSE-SELECT (WINDOW)
  (IF (EQ CURRENT-PROCESS MOUSE-PROCESS)
      (PROCESS-RUN-FUNCTION '(:NAME "Mouse Select" :PRIORITY 20.) 'MOUSE-SELECT WINDOW)
    (SEND WINDOW :MOUSE-SELECT NIL)))

;;; New name.
(DEFMETHOD (ESSENTIAL-MOUSE :ENABLE-SCROLLING-P) ()
  (SEND SELF :SCROLL-BAR-P))

;;; Old name.
(DEFMETHOD (ESSENTIAL-MOUSE :SCROLL-BAR-P) () NIL)

;;; Just to avoid errors.
(DEFMETHOD (ESSENTIAL-MOUSE :NEW-SCROLL-POSITION) (&OPTIONAL IGNORE) NIL)

(DEFFLAVOR KBD-MOUSE-BUTTONS-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:DOCUMENTATION :MIXIN "Sticks clicks in input buffer as characters
Clicking on the window when it is not selected will select it; mouse-right-twice
calls the system menu; any other number of mouse clicks is sent as a fixnum
via :FORCE-KDB-INPUT, %%KBD-MOUSE-BUTTON is button clicked on, %%KBD-MOUSE-N-CLICKS
the number of click."))

(DEFMETHOD (KBD-MOUSE-BUTTONS-MIXIN :MOUSE-CLICK) (BUTTON X Y)
  (DECLARE (IGNORE X Y))
  (AND (= BUTTON #/MOUSE-1-1) (NEQ SELF SELECTED-WINDOW)
       (MOUSE-SELECT SELF))
  (SEND SELF :FORCE-KBD-INPUT BUTTON)
  T)

(DEFFLAVOR LIST-MOUSE-BUTTONS-MIXIN () ())

(DEFFLAVOR HYSTERETIC-WINDOW-MIXIN ((HYSTERESIS 25.)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES HYSTERESIS)
  (:DOCUMENTATION :MIXIN "Controls mouse for small area outside of itself too.
The hysteresis instance variable is the number of pixels outside of its own
area within the :handle-mouse method still retain control."))

(DEFMETHOD (HYSTERETIC-WINDOW-MIXIN :HANDLE-MOUSE) ()
  (LET (LEFT-LIM TOP-LIM
        RIGHT-LIM BOTTOM-LIM)
    (MULTIPLE-VALUE (LEFT-LIM TOP-LIM)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (SETQ RIGHT-LIM (+ LEFT-LIM WIDTH HYSTERESIS)
          BOTTOM-LIM (+ TOP-LIM HEIGHT HYSTERESIS)
          LEFT-LIM (- LEFT-LIM HYSTERESIS)
          TOP-LIM (- TOP-LIM HYSTERESIS))
    (DO (W) (())
      ;; Let the mouse out of the window only if it moves more than <hysteresis> away
      (AND (OR MOUSE-RECONSIDER
               ;; Also leave if mouse fell into inferior
               (AND (NEQ SELF (SETQ W (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y
                                                                NIL :EXPOSED)))
                    (SHEET-ME-OR-MY-KID-P W SELF))
               (< MOUSE-X LEFT-LIM)
               (> MOUSE-X RIGHT-LIM)
               (< MOUSE-Y TOP-LIM)
               (> MOUSE-Y BOTTOM-LIM))
         (RETURN T))
      (MOUSE-STANDARD-BLINKER SELF)
      (MOUSE-DEFAULT-HANDLER SELF (SEND SELF :ENABLE-SCROLLING-P)))))

;;;; The Scroll-Bar

;;; Moving out the left edge of a window that has a scroll bar causes the
;;; window's mouse handler to call this routine.
;;; While the mouse is inside the scroll bar, the mouse buttons are used
;;; to scroll the window for which the scroll bar was brought up.
;;; You can tell that you are in the scroll bar, because the mouse cursor
;;; changes to a fat doubleheaded arrow and part of the left margin of
;;; of the window is darkened, showing where in the buffer is the visible portion.

;;; The commands in the scroll bar are:
;;;   Left: Move the line next to the mouse to the top.
;;;   Left-double: Move the line next to the mouse to the bottom.
;;;   Right: Move the top line to the place where the mouse is.
;;;   Right-double: Move the bottom line to the place where the mouse is.
;;;             Note that this does not call the system menu as you would
;;;             normally expect.
;;;   Middle: Jump to a place in the "buffer" as far (proportionally) from
;;;          the beginning as the mouse is from the top.
;;;   Middle-double: not used


;;; The window should handle the :SCROLL-POSITION message by returning these four values:
;;;  TOP-LINE-NUM - the line-number of the line currently at the top of the window
;;;  TOTAL-LINES - the total number of lines available to scroll through.
;;;  LINE-HEIGHT - the height (in pixels) of a line
;;;  N-ITEMS - the number of items displayed on the screen (there are occaisions where
;;;            this is not trivially calcuable from the other information)
;;; The window should handle the message :SCROLL-TO line-number by scrolling
;;;  to that line, or trying to.  This handler should truncate its arg into range.

;;; A window which can use the scroll bar should send a :NEW-SCROLL-POSITION
;;;  message to itself whenever it scrolls, for any reason, mouse-related or not.
;;; This causes the scroll bar to update its display.
(DEFFLAVOR BASIC-SCROLL-BAR
           ((SCROLL-BAR T) (SCROLL-BAR-ALWAYS-DISPLAYED NIL) (SCROLL-BAR-IN NIL))
           ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:GETTABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED)
  (:INITABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED))

(DEFMETHOD (BASIC-SCROLL-BAR :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND SCROLL-BAR-IN
    "Left: this line to top (L2: bottom); Middle: percentage-wise; Right: top line to here."))

(DEFUN DECODE-SCROLL-BAR (SPEC)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (VALUES (FIRST SPEC) (SECOND SPEC) (THIRD SPEC) (+ (FOURTH SPEC) HEIGHT)))

(DEFUN SCROLL-BAR-DRAW (&AUX BAR-HEIGHT BAR-TOP BAR-BOTTOM BAR-WIDTH)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (WHEN SCROLL-BAR
    (MULTIPLE-VALUE-BIND (TOP-ITEM-NUM TOTAL-ITEMS ITEM-HEIGHT N-ITEMS)
        (SEND SELF :SCROLL-POSITION)
      (SETQ TOTAL-ITEMS (MAX TOP-ITEM-NUM TOTAL-ITEMS)) ;In case we get a bad number, don't
                                                        ; blow the whole mouse process away
      (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
          (DECODE-SCROLL-BAR SCROLL-BAR)
        (SETQ BAR-HEIGHT (MAX 0 (- BOTTOM TOP 5)))
        (OR N-ITEMS (SETQ N-ITEMS (TRUNCATE (SHEET-INSIDE-HEIGHT) ITEM-HEIGHT)))
        (SETQ BAR-TOP (FLOOR (+ TOP
                                (* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
                                                  0
                                                (// TOP-ITEM-NUM (SMALL-FLOAT TOTAL-ITEMS)))))))
        (SETQ BAR-BOTTOM (CEILING (+ TOP (MIN 5 (- BOTTOM TOP))
                                     (* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
                                                       1
                                                     (// (+ TOP-ITEM-NUM N-ITEMS)
                                                         (SMALL-FLOAT TOTAL-ITEMS)))))))
        (SETQ BAR-WIDTH (- RIGHT LEFT))
        (PREPARE-SHEET (SELF)
          (%DRAW-RECTANGLE BAR-WIDTH (- BOTTOM TOP) LEFT TOP ALU-ANDCA SELF)
          (%DRAW-RECTANGLE BAR-WIDTH (MAX 1 (- (MIN BOTTOM BAR-BOTTOM) BAR-TOP))
                           LEFT BAR-TOP ALU-IOR SELF))))))

(DEFUN SCROLL-BAR-ERASE (&AUX LEFT TOP RIGHT BOTTOM)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (WHEN SCROLL-BAR
    (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
      (DECODE-SCROLL-BAR SCROLL-BAR))
    (PREPARE-SHEET (SELF)
      (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF))))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR) (NEW-SCROLL-BAR)
  ;; Just make sure SCROLL-BAR's value is valid and canonicalized.
  (SEND SELF :SET-SCROLL-BAR-SPEC NEW-SCROLL-BAR 0 0 0 0)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :REFRESH-MARGINS) ()
  (AND (OR (EQ SCROLL-BAR-IN T) SCROLL-BAR-ALWAYS-DISPLAYED)
       (SCROLL-BAR-DRAW)))

(DEFMETHOD (BASIC-SCROLL-BAR :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :SET-SCROLL-BAR-SPEC SCROLL-BAR LM TM RM BM))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR-SPEC) (SPEC LM TM RM BM &AUX BAR-WIDTH)
  (WHEN SPEC
    (AND (EQ SPEC T) (SETQ SPEC 1))
    (IF (NUMBERP SPEC)
        (SETQ BAR-WIDTH SPEC SPEC (MAKE-LIST 4))
        (SETQ BAR-WIDTH (- (THIRD SPEC) (FIRST SPEC))))
    (SETF (FIRST SPEC) LM)
    (SETF (SECOND SPEC) TM)
    (SETF (THIRD SPEC) (SETQ LM (+ LM BAR-WIDTH)))
    (SETF (FOURTH SPEC) (- BM)))
  (SETQ SCROLL-BAR SPEC)
  (VALUES LM TM RM BM))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-BAR-P) () (NOT (NULL SCROLL-BAR)))

;;; Next two methods are defaults, some flavors do these more efficiently
(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-ABOVE) ()
  (PLUSP (SEND SELF :SCROLL-POSITION)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-BELOW) ()
  (MULTIPLE-VALUE-BIND (TOP-LINE N-LINES LN-HEIGHT N-SCREEN-LINES)
      (SEND SELF :SCROLL-POSITION)
    ;; Some bag-chompers forget to return this value
    (OR N-SCREEN-LINES
        (SETQ N-SCREEN-LINES (TRUNCATE (SHEET-INSIDE-HEIGHT) LN-HEIGHT)))
    (< (+ TOP-LINE N-SCREEN-LINES) N-LINES)))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR-ALWAYS-DISPLAYED) (NEW)
  (SETQ SCROLL-BAR-ALWAYS-DISPLAYED NEW)
  (COND ((EQ SCROLL-BAR-IN T))
        (SCROLL-BAR-ALWAYS-DISPLAYED (SCROLL-BAR-DRAW))
        (T (SCROLL-BAR-ERASE))))
(DEFMETHOD (BASIC-SCROLL-BAR :HANDLE-MOUSE-SCROLL) (&AUX Y-OFF BOTTOM)
  "Called when the mouse enters the scroll bar"
  (SETQ SCROLL-BAR-IN T)
  ;; Give feedback by changing mouse cursor before calling SCROLL-BAR-DRAW, which pages a lot
  (SEND SELF :SET-MOUSE-POSITION (TRUNCATE SCROLL-BAR-WIDTH 2) NIL)
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 7 :ON
                                :SET-CHARACTER 12.)
  (WITHOUT-INTERRUPTS
    (IF (SHEET-CAN-GET-LOCK SELF)
        (SCROLL-BAR-DRAW)
        (PROCESS-RUN-FUNCTION "Draw Scroll Bar"
                              (LAMBDA (.SELF.)
                                (LETF (((SYMBOL-VALUE 'SELF) .SELF.))
                                  (SHEET-FORCE-ACCESS (SELF)
                                    ;; It is possible that the mouse moved out while we were
                                    ;; waiting.  If this is the case, punt drawing.
                                    (WHEN SCROLL-BAR-IN (SCROLL-BAR-DRAW)))))
                              SELF)))
  (DO-FOREVER
    (MOUSE-DEFAULT-HANDLER SELF :IN)
    (MULTIPLE-VALUE (NIL Y-OFF)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (COND ((< MOUSE-Y Y-OFF)
           (MOUSE-WARP MOUSE-X Y-OFF))
          (( MOUSE-Y (SETQ BOTTOM (+ Y-OFF HEIGHT)))
           (MOUSE-WARP MOUSE-X (1- BOTTOM)))
          (T (RETURN T))))
  (WITHOUT-INTERRUPTS
    ;;There is this funny case where the sheet could be locked by the person waiting
    ;; for us to back out.  For us to block here would be a disaster, so undraw the
    ;; scroll bar in another process
    (COND ((SHEET-CAN-GET-LOCK SELF)
           (OR SCROLL-BAR-ALWAYS-DISPLAYED
               (SHEET-FORCE-ACCESS (SELF) (SCROLL-BAR-ERASE)))
           (SETQ SCROLL-BAR-IN NIL))
          (T
           (SETQ SCROLL-BAR-IN NIL)
           (OR SCROLL-BAR-ALWAYS-DISPLAYED
               (PROCESS-RUN-FUNCTION "Undraw Scroll Bar"
                 (LAMBDA (.SELF.)
                   (LETF (((SYMBOL-VALUE 'SELF) .SELF.))
                     (SHEET-FORCE-ACCESS (SELF)
                       ;; It is possible that the user reentered the
                       ;; scroll bar before this code ran.  In that
                       ;; case, don't actually erase it.
                       (OR SCROLL-BAR-IN
                           (SCROLL-BAR-ERASE)))))
                 SELF))))))

(DEFMETHOD (BASIC-SCROLL-BAR :MOUSE-BUTTONS-SCROLL) (BD IGNORE Y &AUX CHAR TOP BOTTOM SHEIGHT)
  (SETQ CHAR (MOUSE-BUTTON-ENCODE BD))
  (MULTIPLE-VALUE (NIL TOP NIL BOTTOM)
    (DECODE-SCROLL-BAR SCROLL-BAR))
  (SETQ SHEIGHT (- BOTTOM TOP))
  (SELECTQ CHAR
    (#/MOUSE-1-1
      ;; Left: Here to top
      (SEND SELF :SCROLL-RELATIVE Y :TOP))
    (#/MOUSE-3-1
      ;; Right: Top to here
      (SEND SELF :SCROLL-RELATIVE :TOP Y))
    (#/MOUSE-1-2
      ;; Double Left: Here to bottom
      (SEND SELF :SCROLL-RELATIVE Y :BOTTOM))
    (#/MOUSE-3-2
      ;; Double right: Bottom to here
      (SEND SELF :SCROLL-RELATIVE :BOTTOM Y))
    (#/MOUSE-2-1
      ;; Middle: Jump to a proportional place in the "buffer"
      ;; If we are n% of the window down, we want the point
      ;; n% through the buffer to appear at the top of the window.
      (MULTIPLE-VALUE-BIND (IGNORE TOTAL-ITEMS)
          (SEND SELF :SCROLL-POSITION)
        (SEND SELF :SCROLL-ABSOLUTE
                   (ROUND (+ .5s0 (// (* TOTAL-ITEMS (- Y TOP)) (SMALL-FLOAT SHEIGHT)))))))
    (OTHERWISE (BEEP))))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-RELATIVE) (FROM TO)
  "Put the FROM Y-position on the TO Y-position.  This assumes that each item is LINE-HEIGHT
high, and that there is a :SCROLL-TO message which accepts a line number to scroll to,
or a relative number of lines to scroll by.
Each argument is :TOP, :BOTTOM or a number of pixels from the top of the window."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE ITEM-HEIGHT)
      (SEND SELF :SCROLL-POSITION)
    (SETQ FROM (ETYPECASE FROM
                 ((MEMBER :TOP)
                  0)
                 ((MEMBER :BOTTOM)
                  (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                            ITEM-HEIGHT))
                 (NUMBER
                  (TRUNCATE (- FROM TOP-MARGIN-SIZE) ITEM-HEIGHT)))
          TO  (ETYPECASE TO
                ((MEMBER :TOP)
                 0)
                ((MEMBER :BOTTOM)
                 (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                           ITEM-HEIGHT))
                (NUMBER
                 (TRUNCATE (- TO TOP-MARGIN-SIZE) ITEM-HEIGHT))))
    ;; We now know what item we are scrolling from, and what item we are scrolling to.
    ;; Scroll that relative amount.
    (SEND SELF :SCROLL-TO (- FROM TO) :RELATIVE)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-ABSOLUTE) (TO)
  "Scroll to the specified item"
  (SEND SELF :SCROLL-TO TO :ABSOLUTE))

(DEFMETHOD (BASIC-SCROLL-BAR :MOUSE-MOVES-SCROLL) (&REST IGNORE)
  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
      (SEND MOUSE-BLINKER :OFFSETS)
    (BLINKER-SET-CURSORPOS MOUSE-BLINKER
                           (- MOUSE-X X-OFF (SHEET-INSIDE-LEFT MOUSE-SHEET))
                           (- MOUSE-Y Y-OFF (SHEET-INSIDE-TOP MOUSE-SHEET)))))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :NEW-SCROLL-POSITION) (&OPTIONAL IGNORE)
  (AND (OR SCROLL-BAR-IN SCROLL-BAR-ALWAYS-DISPLAYED)
       (SHEET-FORCE-ACCESS (SELF)
         (SCROLL-BAR-DRAW))))

;;;; Flashy Scrolling

;;; If you move the mouse slowly out the top or bottom of a window that
;;; has this flavor, it gets scrolled up or down by a line, and the mouse
;;; jumps back in so that if you keep moving it, it keeps getting scrolled.
(DEFFLAVOR FLASHY-SCROLLING-MIXIN
        ((FLASHY-SCROLLING-REGION '((32. 0.80 :RIGHT) (32. 0.80 :RIGHT)))
         ;; *** I'm not sure there's any point to making this an instance variable --Moon ***
         (FLASHY-SCROLLING-MAX-SPEED 6) ;Default to 6 inches per second
         (FLASHY-SCROLLING-BLINKER NIL))
        ()
  (:INITABLE-INSTANCE-VARIABLES FLASHY-SCROLLING-REGION FLASHY-SCROLLING-MAX-SPEED)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:DOCUMENTATION :MIXIN "Automatic scrolling when moving over the margins
Moving slowly out of the top or bottom of a window that includes this and keep moving,
and it will scroll up or down by a single line and the mouse will be moved back."))

(MOUSE-DEFINE-BLINKER-TYPE 'FLASHY-CHARACTER
                           (LAMBDA (SCREEN)
                             (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                               :VISIBILITY NIL
                               :FONT (SEND SCREEN :MOUSE-FONT)
                               :CHAR 8.)))

(DEFMETHOD (FLASHY-SCROLLING-MIXIN :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (WHEN FLASHY-SCROLLING-BLINKER
    (IF (= (SEND MOUSE-BLINKER :CHARACTER) 8.)
        ;; Character 8 is upward pointing arrow for top of window
        "Bump blinker against top of window to scroll down by one line."
        "Bump blinker against bottom of window to scroll up by one line.")))

(DEFUN FLASHY-SCROLLING-MOUSE-MOVES (W X Y &AUX REGION TOP-P)
  (DECLARE (:SELF-FLAVOR FLASHY-SCROLLING-MIXIN))
  (WHEN (AND ( X 0) (< X WIDTH))
    (SETQ REGION (IF (SETQ TOP-P (< Y (TRUNCATE HEIGHT 2)))
                     (FIRST FLASHY-SCROLLING-REGION)
                   (SECOND FLASHY-SCROLLING-REGION)))
    ;; Make sure is within the appropriate region
    (COND ((AND (SEND SELF :ENABLE-SCROLLING-P)
                (IF TOP-P
                    (< Y (FIRST REGION))
                  (> Y (- HEIGHT (FIRST REGION))))
                (> X (FLASHY-SCROLLING-PARSE-X-SPEC (SECOND REGION)))
                ( X (FLASHY-SCROLLING-PARSE-X-SPEC (THIRD REGION)))
                (SEND SELF (IF TOP-P :SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW)))
           (OR FLASHY-SCROLLING-BLINKER
               (SETQ FLASHY-SCROLLING-BLINKER MOUSE-BLINKER))
           (COND (TOP-P
                  (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 0 :ON
                                                :SET-CHARACTER 8.))
                 (T
                  (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 13. :ON
                                                :SET-CHARACTER 10.)))
           (AND ;; If mouse is moving slowly enough
             (OR (NULL FLASHY-SCROLLING-MAX-SPEED)
                 (< MOUSE-SPEED FLASHY-SCROLLING-MAX-SPEED))
             ;; and out the top or bottom
             (OR (SETQ TOP-P ( Y 0)) ( Y (1- HEIGHT)))
             ;; then warp the mouse and send the appropriate message and return T
             (MULTIPLE-VALUE-BIND (IGNORE WINDOW-Y-OFFSET)
                 (SHEET-CALCULATE-OFFSETS W MOUSE-SHEET)
               (MOUSE-WARP MOUSE-X (+ (IF TOP-P 10. (- HEIGHT 10.)) WINDOW-Y-OFFSET))
               ;; Express scrolling 1 line up or down by relations of lines 0 and 1
               (SEND SELF :SCROLL-TO (IF TOP-P -1 +1) :RELATIVE)
               T)))
          (FLASHY-SCROLLING-BLINKER
           (MOUSE-SET-BLINKER FLASHY-SCROLLING-BLINKER)
           (SETQ FLASHY-SCROLLING-BLINKER NIL)
           NIL))))

(DEFUN FLASHY-SCROLLING-PARSE-X-SPEC (SPEC)
  (DECLARE (:SELF-FLAVOR FLASHY-SCROLLING-MIXIN))
  (TYPECASE SPEC
    (FLOAT (ROUND (* SPEC WIDTH)))
    (FIXNUM SPEC)
    ((MEMBER :RIGHT) WIDTH)
    ((MEMBER :LEFT) 0)
    (T (FERROR "~A is illegal X position specification for flashy scrolling" SPEC))))

;;; Arguments are window-relative position of the mouse
;;; This only does the rest of the processing if the flashy scrolling didn't happen
(DEFWRAPPER (FLASHY-SCROLLING-MIXIN :MOUSE-MOVES) ((X Y) . BODY)
  `(COND ((NOT (FLASHY-SCROLLING-MOUSE-MOVES SELF X Y))
          . ,BODY)))

(defun set-up-mouse-arrays-once ()
  (setq mouse-cursor-page (si:make-wireable-array 1 'art-inum nil))
  (let ((adr (%pointer-plus mouse-cursor-page
                            (si:array-data-offset mouse-cursor-page))))
    (setq mouse-cursor-pattern (make-array '(32. 32.)
                                           :type :art-1b
                                           :displaced-to adr))
    (setq mouse-buttons-buffer (make-array 32.
                                           :type :art-inum
                                           :displaced-to (+ adr 32.)))
    (setq mouse-x-scale-array (make-array 16.
                                          :type :art-inum
                                          :displaced-to (+ adr 32. 32.)))
    (setq mouse-y-scale-array (make-array 16.
                                          :type :art-inum
                                          :displaced-to (+ adr 32. 32. 16.)))))

;(when (null (get '%set-mouse-arrays 'compiler:qlval))
;  (compiler:defmic %set-mouse-arrays #o1163 (cursor-pattern buttons-buffer x-scale y-scale) t))

(defun set-up-mouse-arrays-warm ()
;         (< %microcode-version-number 1500.)
;        ;; Fill the mouse tracker's arrays with NIL instead of the garbage
;        ;; that they contain initially.  At least interpreted ASET won't work otherwise.
;        (LOOP FOR I FROM #o1640 BELOW #o1740
;              DO (%P-STORE-CONTENTS (+ A-MEMORY-VIRTUAL-ADDRESS I) NIL))
  (labels ((data-adr (array)
                     (when (not (si:array-displaced-p array))
                       (ferror nil "array must be displaced"))
                     (%p-contents-offset array (si:array-data-offset array))))
    (without-interrupts
      (setq MOUSE-CURSOR-STATE 0)               ;this keeps track-mouse from doing
                                                ;anything until arrays are set up
      (when (not (boundp 'mouse-cursor-page))
        (set-up-mouse-arrays-once))
      (si:%wire-structure mouse-cursor-page)
      (let ((adr (%pointer-plus mouse-cursor-page
                                (si:array-data-offset mouse-cursor-page))))
                                                ;write all zeros onto the page
        (%p-dpb 0 (byte 16. 0) adr)
        (%p-dpb 0 (byte 16. 16.) adr)
        (%blt adr (%pointer-plus adr 1) (1- page-size) 1)
        (%set-mouse-arrays (data-adr mouse-cursor-pattern)
                           (data-adr mouse-buttons-buffer)
                           (data-adr mouse-x-scale-array)
                           (data-adr mouse-y-scale-array))))))

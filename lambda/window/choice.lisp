;-*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; this whole file should be contained in an inhibit-nausea special form.

;;; This file contains stuff that goes along with TSCROL for letting the
;;; user choose things in various ways other than menus.

;;; Margin region windows, various special areas can be defined within the window's
;;; margins that are allowed to handle the mouse
(DEFFLAVOR MARGIN-REGION-MIXIN
       ((REGION-LIST NIL)                               ;A list of active regions
        (CURRENT-REGION NIL)                            ;The one currently owning the mouse
        )
       ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE ESSENTIAL-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES REGION-LIST)
  (:DOCUMENTATION :MIXIN "Allows separate mouse handling in parts of the margins"))

(DEFSTRUCT (MARGIN-REGION :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  MARGIN-REGION-FUNCTION                                ;A DTP-SELECT-METHOD for this one
  MARGIN-REGION-MARGIN                                  ;Name of the margin occupied
  MARGIN-REGION-SIZE                                    ;Amount of that to occupy
  MARGIN-REGION-LEFT                                    ;Its area of the screen
  MARGIN-REGION-TOP
  MARGIN-REGION-RIGHT
  MARGIN-REGION-BOTTOM)

(DEFMETHOD (MARGIN-REGION-MIXIN :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND CURRENT-REGION
       (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION)
                :WHO-LINE-DOCUMENTATION-STRING CURRENT-REGION)))

(DEFMETHOD (MARGIN-REGION-MIXIN :SET-REGION-LIST) (NEW-REGION-LIST)
  (SETQ REGION-LIST NEW-REGION-LIST)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (MARGIN-REGION-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :DECODE-REGION-LIST REGION-LIST LM TM RM BM))

(DEFMETHOD (MARGIN-REGION-MIXIN :DECODE-REGION-LIST) (SPEC LM TM RM BM)
  (DO ((SPEC SPEC (CDR SPEC))
       (REGION) (SIZE))
      ((NULL SPEC))
    (SETQ REGION (CAR SPEC)
          SIZE (MARGIN-REGION-SIZE REGION))
    (AND ( SIZE 0)
         ;; Negative size means the region took care of setting this stuff
         (MULTIPLE-VALUE (NIL LM TM RM BM)
           (MARGIN-REGION-SET-SIZE REGION SIZE LM TM RM BM))))
  (SETQ REGION-LIST SPEC)
  (VALUES LM TM RM BM))

(DEFUN MARGIN-REGION-SET-SIZE (REGION SIZE LM TM RM BM)
  "Given margins outside REGION, set REGION's edges and return margins inside REGION.
LM, TM, RM and BM are the widths of the four margins, not counting REGION.
We compute where REGION should go, based on them,
and update them to take account of the space used up by REGION.
The values are REGION and the four updated margin-widths,
one of which has been incremented by SIZE."
  (DECLARE (VALUES REGION LM TM RM BM))
  (LET ((LEFT LM) (TOP TM)
        (RIGHT (- RM)) (BOTTOM (- BM)))
    (CASE (MARGIN-REGION-MARGIN REGION)
      (:LEFT
       (SETQ RIGHT (SETQ LM (+ LM SIZE))))
      (:TOP
       (SETQ BOTTOM (SETQ TM (+ TM SIZE))))
      (:RIGHT
       (SETQ LEFT (- (SETQ RM (+ RM SIZE)))))
      (:BOTTOM
       (SETQ TOP (- (SETQ BM (+ BM SIZE))))))
    (SETF (MARGIN-REGION-LEFT REGION) LEFT)
    (SETF (MARGIN-REGION-TOP REGION) TOP)
    (SETF (MARGIN-REGION-RIGHT REGION) RIGHT)
    (SETF (MARGIN-REGION-BOTTOM REGION) BOTTOM))
  (VALUES REGION LM TM RM BM))

(DEFMETHOD (MARGIN-REGION-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DOLIST (REGION REGION-LIST)
    (FUNCALL (MARGIN-REGION-FUNCTION REGION) :REFRESH REGION)))

;(DEFWRAPPER (MARGIN-REGION-MIXIN :MOUSE-MOVES) (IGNORE . BODY)
;  `(CATCH 'REGION-HANDLED-MOUSE
;     (PROGN . ,BODY)))

(DEFWRAPPER (MARGIN-REGION-MIXIN :HANDLE-MOUSE) (IGNORE . BODY)
  `(UNWIND-PROTECT
     (PROGN . ,BODY)
     (IF CURRENT-REGION
         (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION)
                  :MOUSE-LEAVES-REGION CURRENT-REGION)
       (SEND SELF :MOUSE-LEAVES-REGION))
     (SETQ CURRENT-REGION NIL)))

(DEFUN MARGIN-REGION-AREA (REGION &AUX LEFT TOP RIGHT BOTTOM)
  "Return the four edges of REGION, relative to the window SELF."
  (DECLARE (:SELF-FLAVOR MARGIN-REGION-MIXIN)
           (RETURN-LIST LEFT TOP RIGHT BOTTOM))
  (SETQ LEFT (MARGIN-REGION-LEFT REGION)
        TOP (MARGIN-REGION-TOP REGION)
        RIGHT (MARGIN-REGION-RIGHT REGION)
        BOTTOM (MARGIN-REGION-BOTTOM REGION))
  (AND (< LEFT 0) (SETQ LEFT (+ WIDTH LEFT)))
  (AND (< TOP 0) (SETQ TOP (+ HEIGHT TOP)))
  (AND ( RIGHT 0) (SETQ RIGHT (+ WIDTH RIGHT)))
  (AND ( BOTTOM 0) (SETQ BOTTOM (+ HEIGHT BOTTOM)))
  (VALUES LEFT TOP RIGHT BOTTOM))

(DEFMETHOD (MARGIN-REGION-MIXIN :AFTER :MOUSE-MOVES) (X Y &AUX REGION)
  (DOLIST (REG REGION-LIST)
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (MARGIN-REGION-AREA REG)
      (AND ( X LEFT) (< X RIGHT) ( Y TOP) (< Y BOTTOM)
           (RETURN (SETQ REGION REG)))))
  (UNLESS (EQ REGION CURRENT-REGION)
    (IF CURRENT-REGION
        (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION) :MOUSE-LEAVES-REGION
                 CURRENT-REGION)
      (SEND SELF :MOUSE-LEAVES-REGION))
    (IF REGION
        (FUNCALL (MARGIN-REGION-FUNCTION REGION) :MOUSE-ENTERS-REGION REGION)
      (SEND SELF :MOUSE-ENTERS-REGION)))
  (COND ((SETQ CURRENT-REGION REGION)
;        (MOUSE-SET-BLINKER-CURSORPOS)
         (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION) :MOUSE-MOVES X Y CURRENT-REGION)
;        (THROW 'REGION-HANDLED-MOUSE T)
         )))

(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-CLICK) (BUTTON X Y)
  (WHEN (AND CURRENT-REGION (CHAR BUTTON #/MOUSE-3-2))
    (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION) :MOUSE-CLICK X Y
                                                     CURRENT-REGION
                                                     BUTTON)
    T))

(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-ENTERS-REGION) ())

(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-LEAVES-REGION) ())

;;; Special scrolling windows that tell when there is more above or below and scroll if
;;; you click there
(DEFFLAVOR MARGIN-SCROLL-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-REGION-MIXIN BASIC-SCROLL-BAR)
  (:INIT-KEYWORDS :MARGIN-SCROLL-REGIONS)
  (:DOCUMENTATION :MIXIN "Shows if there is more above or below"))

(DEFSTRUCT (MARGIN-SCROLL-REGION :LIST (:INCLUDE MARGIN-REGION)
                                       (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  MARGIN-SCROLL-REGION-EMPTY-MSG                        ;Message when nothing more to scroll
  MARGIN-SCROLL-REGION-MORE-MSG                         ;Other message
  MARGIN-SCROLL-REGION-MSG-FONT                         ;Font for that
  MARGIN-SCROLL-REGION-CURRENT-STRING                   ;String now displayed in region
)

(DEFMETHOD (MARGIN-SCROLL-MIXIN :BEFORE :INIT) (INIT-PLIST &AUX TOP-P FONT)
  (DOLIST (REGION (GET INIT-PLIST ':MARGIN-SCROLL-REGIONS))
    (COND ((MEMQ REGION '(:TOP :BOTTOM))
           (SETQ TOP-P (EQ REGION ':TOP)
                 REGION (LIST 'MARGIN-SCROLL-REGION REGION 0 0 0 0 0 NIL NIL NIL NIL)))
          ((MEMQ (CAR REGION) '(:TOP :BOTTOM))
           (SETQ TOP-P (EQ (CAR REGION) ':TOP)
                 REGION (LIST 'MARGIN-SCROLL-REGION (CAR REGION) 0 0 0 0 0 (CADR REGION)
                              (CADDR REGION) (CADDDR REGION) NIL)))
          (T
           (SETQ TOP-P (EQ (MARGIN-REGION-MARGIN REGION) ':TOP))))
    (OR (MARGIN-SCROLL-REGION-EMPTY-MSG REGION)
        (SETF (MARGIN-SCROLL-REGION-EMPTY-MSG REGION)
              (IF TOP-P "Top" "Bottom")))
    (OR (MARGIN-SCROLL-REGION-MORE-MSG REGION)
        (SETF (MARGIN-SCROLL-REGION-MORE-MSG REGION)
              (IF TOP-P "More above" "More below")))
    (SETQ FONT (OR (MARGIN-SCROLL-REGION-MSG-FONT REGION) FONTS:TR10I))
    (SETF (MARGIN-SCROLL-REGION-MSG-FONT REGION)
          (SETQ FONT (SEND (SHEET-GET-SCREEN SELF) :PARSE-FONT-DESCRIPTOR FONT)))
    (SETF (MARGIN-REGION-SIZE REGION) (+ 2 (FONT-CHAR-HEIGHT FONT)))
    (PUSH REGION REGION-LIST)))

(DEFMETHOD (MARGIN-SCROLL-MIXIN :AFTER :NEW-SCROLL-POSITION) (&REST IGNORE)
  (DOLIST (REGION REGION-LIST)
    (AND (EQ (MARGIN-REGION-FUNCTION REGION) 'MARGIN-SCROLL-REGION)
         (MARGIN-SCROLL-REGION :REFRESH REGION T))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (MARGIN-SCROLL-MIXIN)
(DEFSELECT MARGIN-SCROLL-REGION
  (:REFRESH (REGION &OPTIONAL OLD-VALID &AUX MORE-P LEFT TOP RIGHT BOTTOM)
   (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
     (MARGIN-REGION-AREA REGION))
   (SETQ MORE-P (SEND SELF (IF (EQ (MARGIN-REGION-MARGIN REGION) ':TOP)
                                  :SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW)))
   (COND ((ZEROP (MARGIN-REGION-SIZE REGION)))  ;Turned off
         ((OR (NOT (EQUAL (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
                          (SETF (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
                                (EVAL
                                  (IF MORE-P (MARGIN-SCROLL-REGION-MORE-MSG REGION)
                                    (MARGIN-SCROLL-REGION-EMPTY-MSG REGION))))))
              (NOT OLD-VALID))
          (SHEET-FORCE-ACCESS (SELF)
            (AND OLD-VALID
                 (TV:PREPARE-SHEET (SELF)
                   (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF)))
            (SEND SELF :STRING-OUT-CENTERED-EXPLICIT
                  (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
                  LEFT TOP RIGHT NIL
                  (MARGIN-SCROLL-REGION-MSG-FONT REGION) CHAR-ALUF
                  0 NIL NIL)))))
  ((:MOUSE-ENTERS-REGION :MOUSE-LEAVES-REGION :MOUSE-MOVES) (&REST IGNORE))
  (:MOUSE-CLICK (IGNORE IGNORE REGION IGNORE)
   (IF (SEND SELF (IF (EQ (MARGIN-REGION-MARGIN REGION) ':TOP)
                         :SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW))
       (LET ((FROM (MARGIN-REGION-MARGIN REGION)))
         (SEND SELF :SCROLL-RELATIVE FROM (IF (EQ FROM ':TOP) ':BOTTOM ':TOP)))
     (BEEP)))
  (:WHO-LINE-DOCUMENTATION-STRING (IGNORE) "Any button to scroll one page.")))

;;; No longer needed.
(DEFFLAVOR MARGIN-SCROLL-REGION-ON-AND-OFF-WITH-SCROLL-BAR-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-SCROLL-MIXIN)
  (:DOCUMENTATION :MIXIN
     "Makes the margin-scroll-regions disappear if the scroll-bar is set to NIL"))

(DEFFLAVOR MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-SCROLL-MIXIN MARGIN-REGION-MIXIN FLASHY-SCROLLING-MIXIN)
  (:DEFAULT-INIT-PLIST :FLASHY-SCROLLING-REGION '((32. 0.40s0 0.60s0) (32. 0.40s0 0.60s0))))

(DEFMETHOD (MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN
             :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND FLASHY-SCROLLING-BLINKER CURRENT-REGION
       (IF (= (SEND MOUSE-BLINKER ':CHARACTER) 8.)
           ;; Character 8. is upward pointing arrow for top of window

  "Bump blinker against top to scroll down by one line.  Any button to scroll one page."
  "Bump blinker against bottom to scroll up by one line.  Any button to scroll one page.")))

(DEFFLAVOR FLASHY-MARGIN-SCROLLING-MIXIN ()
           (MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN
            MARGIN-SCROLL-MIXIN
            FLASHY-SCROLLING-MIXIN)
  (:INCLUDED-FLAVORS MARGIN-REGION-MIXIN)
  (:DOCUMENTATION :MIXIN "Provides margin scrolling and flashy scrolling.
Interfaces them properly, and provides appropriate support."))

(DEFFLAVOR SCROLL-STUFF-ON-OFF-MIXIN
        ((MAKING-SCROLL-DECISION NIL))  ;Internal, prevents infinite recursion
        (FLASHY-MARGIN-SCROLLING-MIXIN MARGIN-REGION-MIXIN BASIC-SCROLL-BAR)
        (:REQUIRED-METHODS :ENABLE-SCROLLING-P  ;T if scrolling needed
                           :ADJUSTABLE-SIZE-P)  ;T if outside size can change
                                                ; to preserve inside size,
                                                ; NIL if something like a pane
        (:DOCUMENTATION :MIXIN "Scroll bar, flashy scrolling, and margin scrolling, which turn on and off with :ENABLE-SCROLLING-P")
        (:DEFAULT-INIT-PLIST :SCROLL-BAR 2  ;This 2 is unmodular, sigh.
                             :MARGIN-SCROLL-REGIONS '(:TOP :BOTTOM)))

;(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :BEFORE :REDEFINE-MARGINS)
;                                               (PLIST &AUX TEM)
;  (COND ((SETQ TEM (GETL PLIST '(:SCROLL-BAR)))        ;If changing the scroll-bar
;        (SETQ TEM (CADR TEM))
;        (DOLIST (R REGION-LIST)
;          (AND (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
;               (SETF (MARGIN-REGION-SIZE R)
;                     (IF (NULL TEM) 0
;                         (+ 2 (FONT-CHAR-HEIGHT (MARGIN-SCROLL-REGION-MSG-FONT R)))))))
;        (PUTPROP PLIST REGION-LIST ':REGION-LIST))))   ;Cause those changes to get parsed

(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (OR MAKING-SCROLL-DECISION
      (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)))

;;; This assumes that the pane-size value will be the amount that the
;;; window wants to use in order to scroll, and therefore that the window
;;; should assume no margin scroll regions when computing it!
(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :AROUND :PANE-SIZE) (CONT MT ARGS &REST IGNORE)
  (letf ((region-list (copy-list region-list)))
    (DO ((RS REGION-LIST (CDR RS)))
        ((NULL RS))
      (LET ((R (CAR RS)))
        (WHEN (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
          (SETF (CAR RS) (COPYLIST R))
          (SETF (MARGIN-REGION-SIZE (CAR RS)) 0))))
    (MULTIPLE-VALUE-BIND (NIL TOP NIL BOT)
        (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
      (letf ((top-margin-size top)
             (bottom-margin-size bot))
        (AROUND-METHOD-CONTINUE CONT MT ARGS)))))

;;; Window should send this message to itself after changing the
;;; number of displayable items, but before doing the associated
;;; redisplay.  This method will decide whether to turn the scroll
;;; bar, flashy scrolling, and margin-scroll regions on and off.
;;; If :ADJUSTABLE-SIZE-P, then if changing the number of displayable
;;; items changes the height of the window, that should be done
;;; before sending this message.
;;; This can change the inside-height of the window, unless the
;;; :ADJUSTABLE-SIZE-P message returns T.
;;; Note that redisplay can happen inside this method, you may want
;;; to do a WITH-SHEET-DEEXPOSED to avoid letting the user see
;;; gratuitous double redisplays, or to suppress the redisplay
;;; entirely if there is no bit-save-array.
(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :DECIDE-IF-SCROLLING-NECESSARY) ()
  (LETF (((SYMEVAL-IN-INSTANCE SELF 'MAKING-SCROLL-DECISION) t))
    (LET ((IW (SHEET-INSIDE-WIDTH)) (IH (SHEET-INSIDE-HEIGHT)) (CHANGEP NIL)
          SCROLL-NOW)
      ;; When we ask whether everything fits, pretend there are no scroll regions.
      (LETF ((REGION-LIST (COPY-LIST REGION-LIST)))
        (DO ((RS REGION-LIST (CDR RS)))
            ((NULL RS))
          (LET ((R (CAR RS)))
            (WHEN (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
              (SETF (CAR RS) (COPYLIST R))
              (SETF (MARGIN-REGION-SIZE (CAR RS)) 0))))
        (MULTIPLE-VALUE-BIND (NIL TOP NIL BOT)
            (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
          (LETF ((top-margin-size top)
                 (bottom-margin-size bot))
            (SETQ SCROLL-NOW (SEND SELF :ENABLE-SCROLLING-P)))))
      ;; Now SCROLL-NOW says whether we must now have scrolling.
      (DOLIST (R REGION-LIST)
        (AND (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
             (LET ((MARGIN-REGION-NEW-HEIGHT
                     (IF (NULL SCROLL-NOW) 0
                       (+ 2 (FONT-CHAR-HEIGHT (MARGIN-SCROLL-REGION-MSG-FONT R))))))
               (UNLESS (= (MARGIN-REGION-SIZE R) MARGIN-REGION-NEW-HEIGHT)
                 (SETQ CHANGEP T)
                 (SETF (MARGIN-REGION-SIZE R) MARGIN-REGION-NEW-HEIGHT)))))
      (WHEN CHANGEP (SEND SELF :REDEFINE-MARGINS))
      (COND ((SEND SELF :ENABLE-SCROLLING-P)    ;Need scrolling?
             (COND ((NOT SCROLL-BAR)            ;If scroll stuff not on, turn on
                    (SETQ CHANGEP T)
                    (SEND SELF :SET-SCROLL-BAR 2))))
            (T                                  ;Doesn't need scrolling
             (MULTIPLE-VALUE-BIND (IGNORE N-ITEMS IGNORE)
                 (SEND SELF :SCROLL-POSITION)
               (COND ((ZEROP N-ITEMS))          ;Obviously not set up yet
                     ((NULL SCROLL-BAR))        ;Already off
                     (T (SETQ CHANGEP T)        ;Turn scroll stuff off
                        (SEND SELF :SET-SCROLL-BAR NIL))))))
      (AND CHANGEP (SEND SELF :ADJUSTABLE-SIZE-P)
           (SEND SELF :SET-INSIDE-SIZE IW IH)))))

;;; The LINE-AREA-TEXT-SCROLL-MIXIN defines a "line area" near the left edge
;;; in which the mouse cursor becomes a rightward-arrow and mouse clicks' meaning is changed.

;;; Clicks while in the line area force an input blip
;;; (:LINE-AREA this-line's-item the-window encoded-mouse-click)
;;; this-line's-item stands for the item displayed on this line (see TSCROL).

;;; You should provide a :LINE-AREA-MOUSE-DOCUMENTATION method to provide
;;; the mouse documentation while the cursor is in the line area.

(DEFFLAVOR LINE-AREA-TEXT-SCROLL-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-REGION-MIXIN TEXT-SCROLL-WINDOW)
  (:INIT-KEYWORDS :LINE-AREA-WIDTH)
  (:METHOD-COMBINATION (:DAEMON-WITH-OVERRIDE :BASE-FLAVOR-LAST :LINE-AREA-MOUSE-DOCUMENTATION))
  (:DOCUMENTATION :MIXIN "Allows selection of a line from the left margin"))

(DEFMETHOD (LINE-AREA-TEXT-SCROLL-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (PUSH (LIST 'LINE-AREA-REGION ':LEFT (GET INIT-PLIST ':LINE-AREA-WIDTH #o30)
              0 0 0 0)
        REGION-LIST))

(DEFMETHOD (LINE-AREA-TEXT-SCROLL-MIXIN :LINE-AREA-MOUSE-DOCUMENTATION) ()
           "Select a line.")

(DECLARE-FLAVOR-INSTANCE-VARIABLES (LINE-AREA-TEXT-SCROLL-MIXIN)
(DEFSELECT LINE-AREA-REGION
  ((:REFRESH :MOUSE-MOVES) (&REST IGNORE))
  (:MOUSE-ENTERS-REGION (IGNORE)
;character lossage
   (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 15 6 :ON :SET-CHARACTER (CHAR-INT #/)))
  (:MOUSE-LEAVES-REGION (IGNORE)
    (MOUSE-STANDARD-BLINKER))
  (:MOUSE-CLICK (IGNORE Y IGNORE BD &AUX ITEM)
   (IF (AND ( Y (SHEET-INSIDE-TOP))
            (LET ((LINE (+ TOP-ITEM (SHEET-LINE-NO NIL Y))))
              (AND (< LINE (ARRAY-ACTIVE-LENGTH ITEMS))
                   (SETQ ITEM (AREF ITEMS LINE)))))
       (SEND SELF :FORCE-KBD-INPUT `(:LINE-AREA ,ITEM ,SELF ,BD))
       (BEEP)))
  (:WHO-LINE-DOCUMENTATION-STRING (IGNORE) (SEND SELF :LINE-AREA-MOUSE-DOCUMENTATION))))


(DEFFLAVOR LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN ()
           (LINE-AREA-TEXT-SCROLL-MIXIN BORDERS-MIXIN BASIC-SCROLL-BAR)
  (:REQUIRED-FLAVORS MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION :COMBINATION))

(DEFMETHOD (LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN :MOUSE-LEAVES-REGION) ()
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL))


(DEFFLAVOR CURRENT-ITEM-MIXIN ((CURRENT-ITEM NIL)) ()
  (:REQUIRED-FLAVORS LINE-AREA-TEXT-SCROLL-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-ITEM)
  (:DOCUMENTATION :MIXIN "Provides an arrow in the line-area pointing to current-item"))

(DEFUN UPDATE-CURRENT-ITEM (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR CURRENT-ITEM-MIXIN))
  (LET ((REGION (ASSQ 'LINE-AREA-REGION REGION-LIST))
        (ITEM-NO (SEND SELF :NUMBER-OF-ITEM CURRENT-ITEM)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (MARGIN-REGION-AREA REGION)
      (MULTIPLE-VALUE-BIND (FIRST-ITEM TOTAL-ITEMS ITEM-HEIGHT)
          (SEND SELF :SCROLL-POSITION)
        (LET ((CURRENT-ITEM-Y (AND ITEM-NO
                                   ( ITEM-NO TOTAL-ITEMS)  ;Can be 1 off end
                                   (+ (* (- ITEM-NO FIRST-ITEM) ITEM-HEIGHT)
                                      (SHEET-INSIDE-TOP))))
              (FONT (SCREEN-DEFAULT-FONT (SHEET-GET-SCREEN SELF))))
          (SHEET-FORCE-ACCESS (SELF)
            (TV:PREPARE-SHEET (SELF)
              (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF)
              (AND CURRENT-ITEM-Y
                   ( CURRENT-ITEM-Y TOP)
                   ( (+ CURRENT-ITEM-Y (FONT-CHAR-HEIGHT FONT)) BOTTOM)
;character lossage
                   (%DRAW-CHAR FONT (CHAR-INT #/) (- RIGHT (FONT-CHAR-WIDTH FONT) 1)
                               CURRENT-ITEM-Y CHAR-ALUF SELF)))))))))

(DEFMETHOD (CURRENT-ITEM-MIXIN :SET-CURRENT-ITEM) (NEW-CURRENT-ITEM)
  (COND ((NEQ NEW-CURRENT-ITEM CURRENT-ITEM)
         (SETQ CURRENT-ITEM NEW-CURRENT-ITEM)
         (UPDATE-CURRENT-ITEM))))

(DEFMETHOD (CURRENT-ITEM-MIXIN :AFTER :REFRESH-MARGINS) UPDATE-CURRENT-ITEM)

(DEFMETHOD (CURRENT-ITEM-MIXIN :AFTER :NEW-SCROLL-POSITION) UPDATE-CURRENT-ITEM)

(DEFFLAVOR MARGIN-CHOICE-MIXIN
         ((MARGIN-CHOICES NIL))
         ()
  (:SETTABLE-INSTANCE-VARIABLES MARGIN-CHOICES)
  (:INCLUDED-FLAVORS MARGIN-REGION-MIXIN)
  (:DOCUMENTATION :MIXIN "Provides a few boxes in the bottom margin"))

(DEFSTRUCT (CHOICE-BOX :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  CHOICE-BOX-NAME
  CHOICE-BOX-STATE
  CHOICE-BOX-FUNCTION
  CHOICE-BOX-X1
  CHOICE-BOX-X2)

(DEFUN DRAW-CHOICE-BOX (SHEET X Y ON-P
                        &OPTIONAL (SIZE (FONT-BLINKER-HEIGHT (SHEET-CURRENT-FONT SHEET)))
                        &AUX (WIDTH (TRUNCATE SIZE 4)))
  (PREPARE-SHEET (SHEET)
    (LET ((CHAR-ALUF (SHEET-CHAR-ALUF SHEET))
          (ERASE-ALUF (SHEET-ERASE-ALUF SHEET)))
      (%DRAW-RECTANGLE SIZE SIZE X Y CHAR-ALUF SHEET)
      (LET ((TEM (- SIZE (* WIDTH 2)))
            (X1 (+ X WIDTH)) (Y1 (+ Y WIDTH)))
        (%DRAW-RECTANGLE TEM TEM X1 Y1 ERASE-ALUF SHEET)
        (AND ON-P
             (LET ((X2 (+ X1 TEM)) (Y2 (+ Y1 TEM)))
               ;; This is a diagonal hexagon
               (%DRAW-TRIANGLE (1- X2) Y1 (1+ X1) Y2 X1 (1- Y2) CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE (1- X2) Y1 X2 Y1 X2 (1+ Y1) CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE (1- X2) Y1 X2 (1+ Y1) (1+ X1) Y2 CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE (1+ X1) Y2 X1 Y2 X1 (1- Y2) CHAR-ALUF SHEET)
               ;; So is this
               (%DRAW-TRIANGLE (1+ X1) Y1 X2 (1- Y2) (1- X2) Y2 CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE X2 (1- Y2) X2 Y2 (1- X2) Y2 CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE (1- X2) Y2 X1 (1+ Y1) (1+ X1) Y1 CHAR-ALUF SHEET)
               (%DRAW-TRIANGLE X1 (1+ Y1) X1 Y1 (1+ X1) Y1 CHAR-ALUF SHEET)
               )))))
  (VALUES (+ X SIZE) Y))

(DEFMETHOD (MARGIN-CHOICE-MIXIN :BEFORE :INIT) (IGNORE)
  (PUSH (LIST 'MARGIN-CHOICE-REGION ':BOTTOM
              (IF (NULL MARGIN-CHOICES) 0 (1+ (SHEET-LINE-HEIGHT SUPERIOR)))
              0 0 0 0)
        REGION-LIST))

(DEFUN HANDLE-CHOICE-BUTTON (BOXES X Y THING &AUX CHOSEN)
  (DECLARE (:SELF-FLAVOR MARGIN-CHOICE-MIXIN))
  (IF (SETQ CHOSEN (DOLIST (BOX BOXES)
                     (AND ( X (CHOICE-BOX-X1 BOX))
                          (< X (CHOICE-BOX-X2 BOX))
                          (RETURN BOX))))
      (PROCESS-RUN-FUNCTION "Choice"
                            ;; God this is SO revolting.
                            (lambda (.self. function chosen thing y)
                              (declare (sys:downward-function))
                              (letf (((symbol-value 'self) .self.))
                                (funcall function chosen thing y)))
                            self
                            (CHOICE-BOX-FUNCTION CHOSEN) CHOSEN THING Y)
      (BEEP)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (MARGIN-CHOICE-MIXIN)
(DEFSELECT MARGIN-CHOICE-REGION
  (:REFRESH (REGION &OPTIONAL ERASE-P &AUX LEFT TOP RIGHT BOTTOM)
   (COND ((NOT (ZEROP (MARGIN-REGION-SIZE REGION)))
          (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
            (MARGIN-REGION-AREA REGION))
          (PREPARE-SHEET (SELF)
            (AND ERASE-P
                 (%DRAW-RECTANGLE (- RIGHT LEFT) (- TOP BOTTOM) LEFT TOP ERASE-ALUF SELF))
            (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT TOP CHAR-ALUF SELF))
          (SETQ TOP (+ TOP 2))
          (DO ((CHOICES MARGIN-CHOICES (CDR CHOICES))
               (SHARE (AND MARGIN-CHOICES (TRUNCATE (- RIGHT LEFT) (LENGTH MARGIN-CHOICES))))
               (X LEFT (+ X SHARE))
               (FONT (AREF FONT-MAP 0))
               (CHOICE) (X0))
              ((NULL CHOICES))
            (SETQ CHOICE (CAR CHOICES))
            (SETQ X0 (+ (SEND SELF :STRING-OUT-EXPLICIT (CHOICE-BOX-NAME CHOICE)
                              X TOP RIGHT NIL FONT CHAR-ALUF 0 NIL NIL)
                        CHAR-WIDTH))
            (SETF (CHOICE-BOX-X1 CHOICE) X0)
            (SETF (CHOICE-BOX-X2 CHOICE) (DRAW-CHOICE-BOX SELF X0 TOP
                                           (CHOICE-BOX-STATE CHOICE)
                                           (FONT-BLINKER-HEIGHT FONT)))))))
  (:MOUSE-MOVES (&REST IGNORE))
  ((:MOUSE-ENTERS-REGION :MOUSE-LEAVES-REGION) (IGNORE))
  (:MOUSE-CLICK (X Y REGION IGNORE)
   (HANDLE-CHOICE-BUTTON MARGIN-CHOICES X Y REGION))
  (:WHO-LINE-DOCUMENTATION-STRING (IGNORE)
    (LET ((X (- MOUSE-X (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))))
      (DOLIST (BOX MARGIN-CHOICES)
        (AND ( X (CHOICE-BOX-X1 BOX))
             (< X (CHOICE-BOX-X2 BOX))
             (RETURN "Any button to select choice.")))))))

;;;Here because momentary-multiple-menu needs margin-choice-mixin....
(DEFWINDOW-RESOURCE MOMENTARY-MULTIPLE-MENU ()
        :MAKE-WINDOW (MOMENTARY-MULTIPLE-MENU)
        :REUSABLE-WHEN :DEEXPOSED)

(DEFFLAVOR MULTIPLE-CHOICE () (BORDERS-MIXIN TOP-BOX-LABEL-MIXIN BASIC-MULTIPLE-CHOICE
                               WINDOW))

(DEFFLAVOR BASIC-MULTIPLE-CHOICE
        ((ITEM-NAME NIL)
         (CHOICE-TYPES NIL)
         (MARGIN-CHOICES DEFAULT-FINISHING-CHOICES)
         (CHOICE-VALUE))
        (SCROLL-STUFF-ON-OFF-MIXIN MARGIN-CHOICE-MIXIN DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES ITEM-NAME CHOICE-TYPES)
  (:INIT-KEYWORDS :CHOICES :ITEM-LIST)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL :SCROLL-BAR 2))

(DEFSTRUCT (CHOICE-TYPE :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  CHOICE-TYPE-KEYWORD
  CHOICE-TYPE-NAME
  CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS
  CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS
  CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS
  CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS)

(DEFSTRUCT (CHOICE-ITEM :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  CHOICE-ITEM-ITEM
  CHOICE-ITEM-NAME
  CHOICE-ITEM-BOXES)

(DEFVAR DEFAULT-FINISHING-CHOICES
  '(("Do It" NIL MULTIPLE-CHOICE-DONE NIL NIL)
    ("Abort" NIL MULTIPLE-CHOICE-ABORT NIL NIL)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :AFTER :INIT) (INIT-PLIST &AUX CHOICES)
  (AND (SETQ CHOICES (OR (GET INIT-PLIST ':CHOICES) (GET INIT-PLIST ':ITEM-LIST)))
       (SEND SELF :SET-CHOICES CHOICES)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :ADJUSTABLE-SIZE-P) ()
  T)

;;; This method is a kludge to make SCROLL-STUFF-ON-OFF-MIXIN work.  What
;;; is the right thing here?
(DEFMETHOD (BASIC-MULTIPLE-CHOICE :ENABLE-SCROLLING-P) ()
  SCROLL-BAR-ALWAYS-DISPLAYED)

;;; I don't think the user is supposed to call this directly; use :SETUP
(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SET-CHOICES) (NEW-CHOICES &AUX NAME-LENGTH CHOICE-BOXES
                                                                  MAX-X NITEMS NEW-LABEL)
  ;; Substitute the name of all types where needed
  (DECLARE (VALUES INSIDE-WIDTH INSIDE-HEIGHT NEW-LABEL))
  (LET ((ALLTYPES (MAPCAR #'CAR CHOICE-TYPES)))
    (DOLIST (CHOICE-TYPE CHOICE-TYPES)
      (AND (EQ (CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS CHOICE-TYPE) T)
           (SETF (CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS CHOICE-TYPE) ALLTYPES))
      (AND (EQ (CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS CHOICE-TYPE) T)
           (SETF (CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS CHOICE-TYPE) ALLTYPES))
      (AND (EQ (CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS CHOICE-TYPE) T)
           (SETF (CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS CHOICE-TYPE) ALLTYPES))
      (AND (EQ (CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS CHOICE-TYPE) T)
           (SETF (CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS CHOICE-TYPE) ALLTYPES))))
  ;; Now compute the length of the name needed
  (SETQ NITEMS 0
        NAME-LENGTH (IF ITEM-NAME (+ CHAR-WIDTH (SHEET-STRING-LENGTH SELF ITEM-NAME)) 0))
  (DOLIST (CHOICE NEW-CHOICES)
    (INCF NITEMS)
    (AND (CHOICE-ITEM-NAME CHOICE)
         (SETQ NAME-LENGTH (MAX NAME-LENGTH
                                (+ (SHEET-STRING-LENGTH SELF (CHOICE-ITEM-NAME CHOICE))
                                   CHAR-WIDTH)))))
  ;; Make prototype boxes
  (DO ((X NAME-LENGTH (+ X TYPE-WIDTH))
       (TYPES CHOICE-TYPES (CDR TYPES))
       (TYPE) (TYPE-WIDTH))
      ((NULL TYPES)
       (SETQ MAX-X (+ X CHAR-WIDTH)))
    (SETQ TYPE (CAR TYPES)
          TYPE-WIDTH (+ (SHEET-STRING-LENGTH SELF (CHOICE-TYPE-NAME TYPE)) CHAR-WIDTH))
    (PUSH (LIST (CHOICE-TYPE-KEYWORD TYPE) NIL 'MULTIPLE-CHOICE-CHOOSE
                (+ X (TRUNCATE TYPE-WIDTH 2)) #o177777)
          CHOICE-BOXES))
  (LET ((MAXIMUM-POSSIBLE-MAX-X (- (SHEET-INSIDE-WIDTH SUPERIOR)
                                   (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))))
    (WHEN (> MAX-X MAXIMUM-POSSIBLE-MAX-X)
      ;; This will not fit inside the superior horizontally, so arrange to truncate.
      (DOLIST (BOX CHOICE-BOXES)
        (DECF (CHOICE-BOX-X1 BOX) (- MAX-X MAXIMUM-POSSIBLE-MAX-X)))
      (DECF NAME-LENGTH (- MAX-X MAXIMUM-POSSIBLE-MAX-X))
      (SETQ MAX-X MAXIMUM-POSSIBLE-MAX-X)))
  ;; Compute the new label
  (SETQ NEW-LABEL (MAKE-STRING (TRUNCATE MAX-X CHAR-WIDTH) :FILL-POINTER 0))
  (AND ITEM-NAME (SETQ NEW-LABEL (STRING-NCONC NEW-LABEL ITEM-NAME)))
  (DO ((I (STRING-LENGTH NEW-LABEL) (1+ I))
       (LIM (TRUNCATE NAME-LENGTH CHAR-WIDTH)))
      (( I LIM)
       (SETF (FILL-POINTER NEW-LABEL) LIM))
    (SETF (CHAR NEW-LABEL I) #/SP))
  (DOLIST (CHOICE-TYPE CHOICE-TYPES)
    (SETQ NEW-LABEL (STRING-NCONC NEW-LABEL #/SP (CHOICE-TYPE-NAME CHOICE-TYPE))))
  ;; Now fill in the items
  (AND (> NITEMS (ARRAY-LENGTH ITEMS))
       (ADJUST-ARRAY-SIZE ITEMS NITEMS))
  (SETF (ARRAY-LEADER ITEMS 0) NITEMS)
  (DO ((CHOICES NEW-CHOICES (CDR CHOICES))
       (I 0 (1+ I))
       (MAX-NAME-CHARS (TRUNCATE NAME-LENGTH CHAR-WIDTH))
       (CHOICE) (CHOICE-ITEM))
      ((NULL CHOICES))
    (SETQ CHOICE (CAR CHOICES)
          CHOICE-ITEM (LIST (CHOICE-ITEM-ITEM CHOICE) (CHOICE-ITEM-NAME CHOICE) NIL))
    ;; Truncate each item name to fit the space available.
    (IF (> (STRING-LENGTH (CHOICE-ITEM-NAME CHOICE-ITEM)) MAX-NAME-CHARS)
        (SETF (CHOICE-ITEM-NAME CHOICE-ITEM)
              (SUBSTRING (CHOICE-ITEM-NAME CHOICE-ITEM) 0 MAX-NAME-CHARS)))
    ;; Create a set of choice boxes for this item, copied from the prototypes.
    ;; The boxes' x positions are copied from the prototypes
    ;; so the order they are stored in for this choice-item does not matter.
    (DO ((BOXES (CHOICE-ITEM-BOXES CHOICE) (CDR BOXES))
         (BOX) (TYPE) (INITIAL-STATE))
        ((NULL BOXES))
      (SETQ BOX (CAR BOXES))
      (IF (SYMBOLP BOX)
          (SETQ TYPE BOX
                INITIAL-STATE NIL)
          (SETQ TYPE (CHOICE-BOX-NAME BOX)
                INITIAL-STATE (CHOICE-BOX-STATE BOX)))
      (SETQ BOX (COPYLIST (ASSQ TYPE CHOICE-BOXES)))
      (SETF (CHOICE-BOX-STATE BOX) INITIAL-STATE)
      (PUSH BOX (CHOICE-ITEM-BOXES CHOICE-ITEM)))
    (ASET CHOICE-ITEM ITEMS I))
  ;; Now we return some reasonable sizes
  (VALUES MAX-X (* NITEMS LINE-HEIGHT) NEW-LABEL))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SETUP) (NEW-ITEM-NAME NEW-CHOICE-TYPES
                                           NEW-FINISHING-CHOICES NEW-CHOICES
                                           &OPTIONAL (MAXLINES 20.) &AUX WID HGT LBL)
  (SETQ ITEM-NAME NEW-ITEM-NAME
        CHOICE-TYPES (copytree NEW-CHOICE-TYPES))
  ;this copytree is required to ensure that you dont try to
  ;modify any list structure which might have originated as constants
  ;in user code.  Such lists would have been consed in MACRO-COMPILED-PROGRAM
  ;area which is read-only.
  (MULTIPLE-VALUE (WID HGT LBL)
    (SEND SELF :SET-CHOICES NEW-CHOICES))
  (SETQ TOP-ITEM 0)             ;Un-scroll
  (SEND SELF :SET-LABEL `(:STRING ,LBL
                          :FONT ,(SEND (SHEET-GET-SCREEN SELF) :PARSE-FONT-NAME ':DEFAULT)))
  (SETQ SCROLL-BAR-ALWAYS-DISPLAYED (< (* MAXLINES LINE-HEIGHT) HGT))
  (SEND SELF :SET-INSIDE-SIZE WID (MIN (* MAXLINES LINE-HEIGHT) HGT))
  (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)
  (SEND SELF :SET-MARGIN-CHOICES NEW-FINISHING-CHOICES)
  (SHEET-FORCE-ACCESS (SELF T)
    (SEND SELF :REFRESH)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :PRINT-ITEM) (ITEM LINE-NO ITEM-NO)
  (DECLARE (IGNORE ITEM-NO))
  (SHEET-STRING-OUT SELF (CHOICE-ITEM-NAME ITEM))
  (DOLIST (BOX (CHOICE-ITEM-BOXES ITEM))
    (SETF (CHOICE-BOX-X2 BOX)
          (DRAW-CHOICE-BOX SELF (CHOICE-BOX-X1 BOX) CURSOR-Y (CHOICE-BOX-STATE BOX))))
  (SETF (AREF DISPLAYED-ITEMS LINE-NO) ITEM))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :MOUSE-CLICK) (BUTTON X Y &AUX LINE-NO ITEM)
  (WHEN (OR (CHAR= BUTTON #/MOUSE-1-1) (CHAR= BUTTON #/MOUSE-3-1))
    (SETQ LINE-NO (SHEET-LINE-NO NIL Y))
    (WHEN (AND ( Y (SHEET-INSIDE-TOP))
               (< Y (+ (SHEET-INSIDE-TOP) (* (SHEET-NUMBER-OF-INSIDE-LINES)
                                             LINE-HEIGHT)))
               (SETQ ITEM (AREF DISPLAYED-ITEMS LINE-NO)))
      (HANDLE-CHOICE-BUTTON (CHOICE-ITEM-BOXES ITEM) X Y ITEM)
      T)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :WHO-LINE-DOCUMENTATION-STRING) ()
  "Any button on a box turns it on or off.")

(DEFUN MULTIPLE-CHOICE-CHOOSE (BOX ITEM Y)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ Y (+ (SHEET-INSIDE-TOP) (* (SHEET-LINE-NO NIL Y) LINE-HEIGHT)))
  (SEND SELF :SET-ITEM-BOX-STATE ITEM Y (CHOICE-BOX-NAME BOX)
                                 (NOT (CHOICE-BOX-STATE BOX))))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SET-ITEM-BOX-STATE) (ITEM Y KEYWORD NEW-STATE &AUX BOX TYP)
  (WHEN (AND (SETQ BOX (ASSQ KEYWORD (CHOICE-ITEM-BOXES ITEM)))
             (NEQ NEW-STATE (CHOICE-BOX-STATE BOX)))
    (SETF (CHOICE-BOX-STATE BOX) NEW-STATE)
    (AND Y (DRAW-CHOICE-BOX SELF (CHOICE-BOX-X1 BOX) Y NEW-STATE))
    (SETQ TYP (ASSQ KEYWORD CHOICE-TYPES))
    (DOLIST (POS (IF NEW-STATE (CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS TYP)
                   (CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS TYP)))
      (OR (EQ POS KEYWORD)
          (SEND SELF :SET-ITEM-BOX-STATE ITEM Y POS T)))
    (DOLIST (NEG (IF NEW-STATE (CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS TYP)
                   (CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS TYP)))
      (OR (EQ NEG KEYWORD)
          (SEND SELF :SET-ITEM-BOX-STATE ITEM Y NEG NIL)))))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :CHOOSE) (&OPTIONAL (NEAR-MODE '(:MOUSE)) &AUX OLD-STATUS)
  (SETQ CHOICE-VALUE NIL)
  (SETQ OLD-STATUS (SEND SELF :STATUS))
  (UNWIND-PROTECT
      (PROGN
        (EXPOSE-WINDOW-NEAR SELF NEAR-MODE)
        (PROCESS-WAIT "Choose" #'CAR (LOCATE-IN-INSTANCE SELF 'CHOICE-VALUE)))
    (SEND SELF :SET-STATUS OLD-STATUS))
  (IF (CONSP CHOICE-VALUE)
      CHOICE-VALUE
    (VALUES NIL CHOICE-VALUE)))

(DEFUN MULTIPLE-CHOICE-DONE (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ CHOICE-VALUE
        (DO ((I 0 (1+ I))
             (LIM (ARRAY-ACTIVE-LENGTH ITEMS))
             (ITEM) (RET NIL))
            (( I LIM) (NREVERSE RET))
          (SETQ ITEM (AREF ITEMS I))
          (PUSH (CONS (CHOICE-ITEM-ITEM ITEM)
                      (DO ((BOXES (CHOICE-ITEM-BOXES ITEM) (CDR BOXES))
                           (BOX) (RET NIL))
                          ((NULL BOXES) (NREVERSE RET))
                        (AND (CHOICE-BOX-STATE (SETQ BOX (CAR BOXES)))
                             (PUSH (CHOICE-BOX-NAME BOX) RET))))
                RET))))

(DEFUN MULTIPLE-CHOICE-ABORT (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ CHOICE-VALUE ':ABORT))

(DEFFLAVOR TEMPORARY-MULTIPLE-CHOICE-WINDOW () (TEMPORARY-WINDOW-MIXIN MULTIPLE-CHOICE))

(DEFMETHOD (TEMPORARY-MULTIPLE-CHOICE-WINDOW :AFTER :DEEXPOSE) (&REST IGNORE)
  (OR CHOICE-VALUE (SETQ CHOICE-VALUE ':ABORT)))

(COMPILE-FLAVOR-METHODS TEMPORARY-MULTIPLE-CHOICE-WINDOW)

(DEFWINDOW-RESOURCE TEMPORARY-MULTIPLE-CHOICE-WINDOW ()
  :MAKE-WINDOW (TEMPORARY-MULTIPLE-CHOICE-WINDOW)
  :REUSABLE-WHEN :DEACTIVATED)

(DEFUN MULTIPLE-CHOOSE (ITEM-NAME ITEM-LIST KEYWORD-ALIST
                        &OPTIONAL (NEAR-MODE '(:MOUSE)) (MAXLINES 20.) SUP)
  "Ask several multiple-choice questions with a menu-like window.
ITEM-NAME is a string of the name of the type of item, e.g. /" Buffer/".
 It goes in the label, above the item names.
ITEM-LIST is an alist, in which each element is (ITEM NAME CHOICES).
 ITEM is the item itself, NAME its name (a string),
 and CHOICES a list of possible keywords, either KEYWORD or (KEYWORD DEFAULT),
 where if DEFAULT is non-NIL the KEYWORD is initially on.
 The purpose of ITEM is that it appears in the value returned,
 and allows you to identify what the returned answers apply to.
KEYWORD-ALIST is a list of the possible keywords, (KEYWORD NAME . IMPLICATIONS).
 KEYWORD is a symbol, the same as in ITEM-LIST's CHOICES.
 NAME is a string of its name.
 IMPLICATIONS is a list of on-positive, on-negative, off-positive, and off-negative
 implications for when the keyword is selected, each one either a list of (other) keywords
 or T for all other keywords.  The default for IMPLICATIONS is (NIL T NIL NIL).

We return two values: first, the list of choices chosen;
second, the reason why we exited (NIL means normal exit).
Each element of the first value is a list (ITEM SELECTED-CHOICES...)."
  (DECLARE (VALUES CHOICES EXIT-REASON))
  ;; Decide what superior to use
  (UNLESS SUP
    (SETQ SUP (IF (EQ (CAR NEAR-MODE) ':WINDOW)
                  (SHEET-SUPERIOR (CADR NEAR-MODE))
                  MOUSE-SHEET)))
  ;; avoid bashing user's code!
  ;; 4/11/86 - notice that
  ;; copy-list doesnt copy the ELEMENTS of keyword alist and therefore
  ;; would fail if you try to setf inside those elements!
  ;; (quoted lists are consed in MACRO-COMPILED-PROGRAM area
  ;; and are therefore READ-ONLY, so we have to make a copy if we want
  ;; to bash it.)
  (SETQ KEYWORD-ALIST (COPYTREE KEYWORD-ALIST))
  (UNLESS (LOOP FOR X IN KEYWORD-ALIST ALWAYS (CDDDR X))
    (DO ((L KEYWORD-ALIST (CDR L)))
        ((NULL L))
      (AND (< (LENGTH (CAR L)) 3)
           (SETF (CAR L) (APPEND (CAR L) (LIST NIL T NIL NIL))))))
  (USING-RESOURCE (WINDOW TEMPORARY-MULTIPLE-CHOICE-WINDOW SUP)
    (SEND WINDOW :SETUP ITEM-NAME KEYWORD-ALIST
                 DEFAULT-FINISHING-CHOICES (copylist ITEM-LIST) MAXLINES)
    (UNWIND-PROTECT
      (SEND WINDOW :CHOOSE NEAR-MODE)
      (SEND WINDOW :DEACTIVATE))))

;;;;Choose-variable-values stuff.
#|
Basic idea is that the program has a list of special variables, and
the user is asked to confirm and possibly modify their values.  Values
can be either general expressions, or a choice from a list (menu like).

The printing of the display is not actually done in the user's stack group,
but it acts as if it were.  The reading of new values is done in the user's stack group.
Thus you can bind *PRINT-BASE*, *PRINT-LEVEL*, *READTABLE*, etc.

The user can point at a displayed value and click the mouse, to modify it.
The new value is input from the keyboard; over-rubbing-out restores the
old value.  For values chosen from a list, clicking the mouse selects
the value pointed-to.

VARIABLES is a list of elements, each describing one line of the display
These become text-scroll items.  Kinds of elements allowed are:
 string - just displayed
 special-variable - value is printed, and if the user clicks on it
                with the mouse a new value is read.
 locative - like special-variable but value is accessed by car and written by rplaca
Otherwise a list whose car is the variable, optionally
followed by a string to use as label instead of the var, or nil for
no label, followed by a keyword for the type of variable, followed by
args to the keyword.  The default keyword is :SEXP
Keywords are:
   :SEXP or :ANY - value of variable is a Lisp S-expression, printed with PRIN1,
                        read in with READ
   :PRINC - same as :SEXP but print it with PRINC instead of PRIN1
   :STRING - print with PRINC, read with READLINE
   :NUMBER - print with PRIN1, read with READ but must be a number
   :CHOOSE values-list print-function - value of variable is one of the
                elements of values-list (EQUAL testing is used).  Printed
                by printing all the value choices, with the current one in
                boldface, read in by the user pointing and clicking.
                print-function is optional and defaults to PRINC
   :ASSOC values-list print-function - like :CHOOSE, but car of
                values-list element is displayed, cdr is variable-value
   :BOOLEAN - value is T or NIL, but displays as Yes or No
   :CHARACTER - value is a character, prints with ~:@C, reads as one keystroke
   :CHARACTER-OR-NIL - same but can also be NIL, displays as "none", inputs as CLEAR

If :DOCUMENTATION appears where the keyword is expected, it is followed by
a string to display when the mouse is pointing here, and then by the keyword.
This is implemented by :DECODE-VARIABLE-TYPE (see below) so that you can
change it.

Should there also be ones which are constrained to be lists of chars?
Keywords automatically forced into the keyword package?
Should there be a provision for documentation of a variable, and a way
to make that print somewhere?  (As in ZMACS Alter Options)

The :DECODE-VARIABLE-TYPE message to the window is used to look at the
keyword and options and return information about how to print and change
the variable's value.  The argument to this message is
the tail of a VARIABLES element starting with the keyword, and it
returns 6 values:
 The print function (args are object and stream).
 The read function, or NIL if it works by pointing (arg is stream).
   Crockishness: usually this is called inside a rubout-handler, with the
   feature supplied that over-rubout causes the variable to left at its old
   value.  But with a list here the car of the list is the function which
   just gets called directly.
 The choices to be printed (NIL if just print current value).
 The function which translates a value to its printed form (NIL for identity).
 The function which translates a value to the form
   which goes in the variable (NIL for identity).
 The who-line mouse documentation string.  If this is a symbol, then NIL means
   use the default documentation, and any other symbol is the name of a function
   which translates a value to its documentation.
The two functions only apply when there are choices.
The default handler looks for a TV:CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION property which
is a function to call or a TV:CHOOSE-VARIABLE-VALUES-KEYWORD property which is
(print-func read-func choices ptransfn vtransfn mouse-documentation)

FUNCTION can be NIL or a function called on window, special-variable, old-value, new-value
 when a variable is changed.  It may make other changes.  Returns T if it did
 its own redisplay (typically by sending a :SET-VARIABLES),
 NIL if that variable's new value needs to be displayed.
 Typically this function implements constraints among the variable
 values and sends a refresh message and returns T.

STACK-GROUP is the stack-group in which the variables may be evaluated.

Height of window is chosen automatically upon creation if not specified
in the init-plist.  Also is automatically adjustable if you send
a :SET-VARIABLES.

The following messages can come back through the io-buffer:
 (:CHOICE-BOX window box)
 (:VARIABLE-CHOICE window VARIABLES-element value line-no)

Font-map:
 0  string
 1  name
 2  value
 3  unselected-choice
 4  selected-choice
|#

(DEFFLAVOR BASIC-CHOOSE-VARIABLE-VALUES
        ((FUNCTION NIL) STACK-GROUP (LINE-OVERFLOW-ALLOWED T) (RECURSION NIL))
        (ANY-TYI-MIXIN MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS STREAM-MIXIN)
  (:INIT-KEYWORDS :VARIABLES :NAME-FONT :VALUE-FONT :STRING-FONT
                  :UNSELECTED-CHOICE-FONT :SELECTED-CHOICE-FONT)
  (:DEFAULT-INIT-PLIST :SAVE-BITS NIL :CHARACTER-WIDTH 50.
                       :BLINKER-P '(:VISIBILITY NIL) :BLINKER-DESELECTED-VISIBILITY NIL
                       :NAME-FONT FONTS:CPTFONT :VALUE-FONT FONTS:CPTFONT
                       :STRING-FONT FONTS:CPTFONT
                       :UNSELECTED-CHOICE-FONT FONTS:HL10 :SELECTED-CHOICE-FONT FONTS:HL10B
                       :stack-group system:%current-stack-group))

(DEFFLAVOR CHOOSE-VARIABLE-VALUES-WINDOW
        ()
        (BASIC-CHOOSE-VARIABLE-VALUES BORDERS-MIXIN TOP-BOX-LABEL-MIXIN
         SCROLL-STUFF-ON-OFF-MIXIN MARGIN-CHOICE-MIXIN
         ANY-TYI-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :MARGIN-CHOICES
                       '(("Exit" NIL CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER NIL NIL))))

(DEFUN CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER (BOX REGION YPOS)
  REGION YPOS ;ignored
  (SEND SELF :FORCE-KBD-INPUT `(:CHOICE-BOX ,SELF ,BOX)))

;;; I don't know if this function's list of options is up to date...
(DEFUN HEIGHT-SPECIFIED-IN-INIT-PLIST (PLIST)
  "Returns T if the PLIST contains anything that specifies the window height"
  (OR (GETL PLIST '(:EDGES-FROM :EDGES :HEIGHT :CHARACTER-HEIGHT))
      (AND (GETL PLIST '(:TOP :Y)) (GET PLIST ':BOTTOM))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :BEFORE :INIT) (PLIST)
  ;; Default the height according to the number of variables, unless
  ;; it was specified explicitly.
  (OR (HEIGHT-SPECIFIED-IN-INIT-PLIST PLIST)
      (PUTPROP PLIST (MAX (MIN (LENGTH (GET PLIST ':VARIABLES)) 25.) 1) ':CHARACTER-HEIGHT))
  ;; Set up font map according to fonts specified by name
  (SETQ FONT-MAP (LIST (GET PLIST ':STRING-FONT)
                       (GET PLIST ':NAME-FONT)
                       (GET PLIST ':VALUE-FONT)
                       (GET PLIST ':UNSELECTED-CHOICE-FONT)
                       (GET PLIST ':SELECTED-CHOICE-FONT))))

;;; This sets the variables and adjusts the scrolling but never changes the height
;;; which was set either by the before-init method or by the creator.
;;; Except that the outside height may be changed to preserve what the creator
;;; is thought to have specified as the inside height.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :AFTER :INIT) (PLIST &AUX ELEMS)
  (AND (SETQ ELEMS (GET PLIST ':VARIABLES))
       (SEND SELF :SET-VARIABLES ELEMS T)))

;;; Default is that size adjusts according to the number of items present,
;;; provided that the window is de-exposed.  This is because if it was
;;; exposed the user would see it spastically redisplay several times.
;;; Also it probably looks very bad for it to change size while it's exposed.
;;; You are welcome to redefine this method.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :ADJUSTABLE-SIZE-P) ()
  (NOT EXPOSED-P))



;;; original
;(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SET-VARIABLES) (ELEMS &OPTIONAL NO-SET-HEIGHT
;                                                         -WIDTH- EXTRA-WIDTH
;                                                         &AUX (NELEM (LENGTH ELEMS)))
;  (SETQ TOP-ITEM 0)                    ;Unscroll
;  (AND (< (ARRAY-LENGTH ITEMS) NELEM)
;       (SETQ ITEMS (ADJUST-ARRAY-SIZE ITEMS NELEM)))
;  (SETF (ARRAY-LEADER ITEMS 0) 0)
;  (DOLIST (ELEM ELEMS)
;    (ARRAY-PUSH ITEMS ELEM))
;  ;; -WIDTH- can be a string, a number of chars, or T meaning look at the variable specs.
;  (setq -width-
;       (etypecase -width-
;         (STRING (SEND SELF :STRING-LENGTH -WIDTH-))
;         ((integer 0) (* (SHEET-CHAR-WIDTH SELF) -WIDTH-))
;         (null (sheet-inside-width))
;         ((member t) (SEND SELF :APPROPRIATE-WIDTH EXTRA-WIDTH))))
;  (SETQ NELEM (LENGTH ITEMS))
;  (LET ((DESIRED-HEIGHT (* (MIN 25. NELEM) LINE-HEIGHT))
;       (DESIRED-WIDTH (MAX -WIDTH- (SHEET-INSIDE-WIDTH)))) ;; DON'T let user make window
;                                                           ;; too small to display current items
;    (AND (or ( (SHEET-INSIDE-HEIGHT) DESIRED-HEIGHT)
;            (NOT NO-SET-HEIGHT)
;            (< (sheet-inside-width) DESIRED-WIDTH))
;        (SEND SELF :ADJUSTABLE-SIZE-P)
;        ( (+ DESIRED-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
;           (SHEET-INSIDE-HEIGHT SUPERIOR))
;        (SEND SELF :SET-INSIDE-SIZE DESIRED-WIDTH DESIRED-HEIGHT))
;    (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)
;    (SEND SELF :SET-ITEMS ITEMS)))     ;Redisplay

; modified by mrc
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SET-VARIABLES) (ELEMS &OPTIONAL NO-SET-HEIGHT
                                                          -WIDTH- EXTRA-WIDTH
                                                          &AUX (NELEM (LENGTH ELEMS)))
  (SETQ TOP-ITEM 0)                     ;Unscroll
  (AND (< (ARRAY-LENGTH ITEMS) NELEM)
       (SETQ ITEMS (ADJUST-ARRAY-SIZE ITEMS NELEM)))
  (SETF (ARRAY-LEADER ITEMS 0) 0)
  (DOLIST (ELEM ELEMS)
    (ARRAY-PUSH ITEMS ELEM))
  ;; -WIDTH- can be a string, a number of chars, or T meaning look at the variable specs.
  (setq -width-
        (etypecase -width-
          (STRING (SEND SELF :STRING-LENGTH -WIDTH-))
          ((integer 0) (* (SHEET-CHAR-WIDTH SELF) -WIDTH-))
          (null (sheet-inside-width))
          ((member t) (SEND SELF :APPROPRIATE-WIDTH EXTRA-WIDTH))))
  (SETQ NELEM (LENGTH ITEMS))
  (LET ((DESIRED-HEIGHT (* (MIN 25. NELEM) LINE-HEIGHT))
        (DESIRED-WIDTH (MAX -WIDTH-
                            (send self :appropriate-width 0)))) ;; DON'T let user make window
                                                ;; too small to display current items

    (AND (or ( (SHEET-INSIDE-HEIGHT) DESIRED-HEIGHT)
             (NOT NO-SET-HEIGHT)
             (< (sheet-inside-width) DESIRED-WIDTH))
         (SEND SELF :ADJUSTABLE-SIZE-P)
         ( (+ DESIRED-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
            (SHEET-INSIDE-HEIGHT SUPERIOR))
         (SEND SELF :SET-INSIDE-SIZE DESIRED-WIDTH DESIRED-HEIGHT))
    (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)
    (SEND SELF :SET-ITEMS ITEMS)))      ;Redisplay

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SETUP) (ELEMS NEW-LABEL NEW-FUNCTION
                                                   NEW-MARGIN-CHOICES
                                                   &OPTIONAL -WIDTH- EXTRA-WIDTH)
  (SETQ FUNCTION NEW-FUNCTION)
  (SETQ STACK-GROUP %CURRENT-STACK-GROUP)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) CURRENT-PROCESS)  ;Kludge
  (SEND SELF :SET-LABEL NEW-LABEL)
  (SEND SELF :SET-MARGIN-CHOICES NEW-MARGIN-CHOICES)
  (SEND SELF :SET-VARIABLES ELEMS NIL -WIDTH- EXTRA-WIDTH))

(DEFPROP :SEXP (PRIN1 READ) CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFPROP :ANY (PRIN1 READ) CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFPROP :PRINC (PRINC READ) CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFPROP :STRING (PRINC READLINE) CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFPROP :CHOOSE CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
(DEFPROP :ASSOC CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
(DEFUN CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE (KWD-AND-ARGS)
  (VALUES (OR (THIRD KWD-AND-ARGS) 'PRINC)
          NIL
          (SECOND KWD-AND-ARGS)
          (AND (EQ (FIRST KWD-AND-ARGS) ':ASSOC) 'CAR)
          (AND (EQ (FIRST KWD-AND-ARGS) ':ASSOC) 'CDR)))

(DEFPROP :BOOLEAN (CHOOSE-VARIABLE-VALUES-BOOLEAN-PRINT NIL (T NIL))
         CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFUN CHOOSE-VARIABLE-VALUES-BOOLEAN-PRINT (VALUE STREAM)
  (SEND STREAM :STRING-OUT (IF VALUE "Yes" "No")))

(DEFPROP :CHARACTER
         (CHOOSE-VARIABLE-VALUES-CHARACTER-PRINT (TYI) NIL
          NIL NIL "Click left to input a new character from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-PRINT (VALUE STREAM)
  (FORMAT STREAM "~:@C" VALUE))

(DEFPROP :CHARACTER-OR-NIL
         (CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-PRINT
          CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-READ
          NIL NIL NIL "Click left to input a new character from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-PRINT (VALUE STREAM)
  (FORMAT STREAM (IF VALUE "~:@C" "none") VALUE))

(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-READ (STREAM &AUX CH)
  (IF (CHAR= (SETQ CH (READ-CHAR STREAM)) #/CLEAR)
      NIL
    (UNREAD-CHAR CH STREAM)
    (TYI STREAM)))

(DEFPROP :NUMBER
         (PRIN1 READ-NUMBER NIL NIL NIL "Click left to input a new number from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)
(DEFUN READ-NUMBER (STREAM)
  (LET ((VAL (READ STREAM)))
    (OR (NUMBERP VAL) (FERROR "A number is required"))
    VAL))

(DEFPROP :DATE
         (TIME:PRINT-UNIVERSAL-TIME READ-DATE NIL
          NIL NIL "Click left to input a new date from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN READ-DATE (STREAM)
  (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME (READLINE-TRIM STREAM))))
    VAL))

(DEFPROP :PAST-DATE
         (TIME:PRINT-UNIVERSAL-TIME READ-PAST-DATE NIL
          NIL NIL "Click left to input a new date from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN READ-PAST-DATE (STREAM)
  (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME (READLINE-TRIM STREAM) 0 NIL NIL)))
    VAL))

(DEFPROP :DATE-OR-NEVER
         (PRINT-UNIVERSAL-TIME-OR-NEVER READ-DATE-OR-NEVER NIL
          NIL NIL "Click left to input a new date from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN PRINT-UNIVERSAL-TIME-OR-NEVER (TIME STREAM)
  (IF (NULL TIME) (PRINC "never" STREAM)
      (TIME:PRINT-UNIVERSAL-TIME TIME STREAM)))

(DEFUN READ-DATE-OR-NEVER (STREAM)
  (LET ((STRING (READLINE-TRIM STREAM)))
    (IF (EQUALP STRING "never") NIL
        (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME STRING)))
          VAL))))

(DEFPROP :INTERVAL-OR-NEVER
         (TIME:PRINT-INTERVAL-OR-NEVER TIME:READ-INTERVAL-OR-NEVER NIL
          NIL NIL "Click left to input a new interval, or /"never/", from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFPROP :NUMBER-OR-NIL
         (PRIN1 READ NIL
          NIL NIL "Click left to enter a new number, or NIL, from the keyboard.")
         CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN (:MENU-ALIST CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) (KWD-AND-ARGS)
  (VALUES 'PRINC
          NIL
          (SECOND KWD-AND-ARGS)
          'CAR
          'MENU-EXECUTE-NO-SIDE-EFFECTS
          'MENU-ITEM-WHO-LINE-DOCUMENTATION))

(DEFPROP :STRING-LIST (PRINT-STRING-LIST READ-STRING-LIST) CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN PRINT-STRING-LIST (STRING-LIST STREAM)
  (FORMAT STREAM "~{~A~^, ~}" STRING-LIST))

(DEFUN READ-STRING-LIST (STREAM)
  (DO ((STRING (READLINE STREAM))
       (I 0 (1+ J))
       (J)
       (STRING-LIST NIL))
      (NIL)
    (SETQ J (STRING-SEARCH-CHAR #/, STRING I))
    (PUSH (STRING-TRIM '(#/SP #/TAB) (NSUBSTRING STRING I J)) STRING-LIST)
    (OR J (RETURN (NREVERSE STRING-LIST)))))

(DEFPROP :PATHNAME DECODE-PATHNAME-ITEM CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
(DEFUN DECODE-PATHNAME-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
    (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINC
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME)
      NIL NIL NIL
      "Click left to enter a new pathname from the keyboard.")))

(DEFPROP :PATHNAME-OR-NIL DECODE-PATHNAME-OR-NIL-ITEM CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
(DEFUN DECODE-PATHNAME-OR-NIL-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
  (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINC
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME-OR-NIL)
      NIL NIL NIL
      "Click left to enter a new pathname from the keyboard.")))

(DEFPROP :PATHNAME-LIST DECODE-PATHNAME-LIST-ITEM CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
(DEFUN DECODE-PATHNAME-LIST-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
    (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINT-STRING-LIST
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME-LIST)
      NIL NIL NIL
      "Click left to enter new pathnames from the keyboard.")))

(DEFUN READ-PATHNAME-OR-NIL (STREAM &AUX STRING)
  (DECLARE (SPECIAL DEFAULTS))
  (SETQ STRING (READLINE STREAM))
  (AND (PLUSP (STRING-LENGTH STRING))
       (STRING (FS:MERGE-PATHNAME-DEFAULTS STRING
                                           (IF (SYMBOLP DEFAULTS) (SYMEVAL DEFAULTS) DEFAULTS)))))

(DEFUN READ-PATHNAME (STREAM)
  (DECLARE (SPECIAL DEFAULTS))
  (STRING (FS:MERGE-PATHNAME-DEFAULTS (READLINE STREAM)
                                      (IF (SYMBOLP DEFAULTS) (SYMEVAL DEFAULTS) DEFAULTS))))

(DEFUN READ-PATHNAME-LIST (STREAM)
  (MAPCAR #'STRING (PARSE-PATHNAME-LIST (READLINE STREAM))))

(DEFUN PARSE-PATHNAME-LIST (STRING &OPTIONAL (DEFAULTS DEFAULTS))
  (DECLARE (SPECIAL DEFAULTS))
  (DO ((I 0 (1+ J))
       (J)
       (STRING-LIST NIL))
      (NIL)
    (SETQ J (STRING-SEARCH-CHAR #/, STRING I))
    (PUSH (FS:MERGE-PATHNAME-DEFAULTS (NSUBSTRING STRING I J)
                                      (IF (SYMBOLP DEFAULTS) (SYMEVAL DEFAULTS) DEFAULTS))
          STRING-LIST)
    (OR J (RETURN (NREVERSE STRING-LIST)))))

(defun read-host-name (stream)
  (si:parse-host (readline stream)))

(defun read-host-name-or-nil (stream)
  (let ((name (readline-or-nil stream)))
    (and name
         (si:parse-host name t))))

(defprop :host (princ read-host-name) choose-variable-values-keyword)

(defprop :host-or-nil (princ read-host-name-or-nil)
  choose-variable-values-keyword)

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :DECODE-VARIABLE-TYPE) (KWD-AND-ARGS &AUX KEY TEM)
  (SETQ KEY (CAR KWD-AND-ARGS))
  (COND ((EQ KEY ':DOCUMENTATION)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF)
             (SEND SELF ':DECODE-VARIABLE-TYPE (OR (CDDR KWD-AND-ARGS) '(:SEXP)))
           (VALUES PF RF CHOICES GPVF GVVF (CADR KWD-AND-ARGS))))
        ((SETQ TEM (GET KEY 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION))
         (SEND TEM KWD-AND-ARGS))
        ((SETQ TEM (GET KEY 'CHOOSE-VARIABLE-VALUES-KEYWORD))
         (VALUES-LIST TEM))
        (T
         (FERROR "~S bad keyword in a CHOOSE-VARIABLE-VALUES-WINDOW" KEY))))

;So lines can wrap around when reading
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :AROUND :END-OF-LINE-EXCEPTION) (&REST IGNORE)
  (IF LINE-OVERFLOW-ALLOWED
      ;>> *BARF* ********************
      (FUNCALL #'(:METHOD SHEET :END-OF-LINE-EXCEPTION) ':END-OF-LINE-EXCEPTION)
    (THROW 'LINE-OVERFLOW T)))

;;; Make printing work in environment of owning stack group
(DEFWRAPPER (BASIC-CHOOSE-VARIABLE-VALUES :REDISPLAY) (IGNORE . BODY)
  `(LET ((*PACKAGE* (SYMEVAL-IN-STACK-GROUP '*PACKAGE* STACK-GROUP))
         (*PRINT-BASE* (SYMEVAL-IN-STACK-GROUP '*PRINT-BASE* STACK-GROUP))
         (*NOPOINT (SYMEVAL-IN-STACK-GROUP '*NOPOINT STACK-GROUP))
         (*PRINT-RADIX* (SYMEVAL-IN-STACK-GROUP '*PRINT-RADIX* STACK-GROUP))
         (*PRINT-LEVEL* (SYMEVAL-IN-STACK-GROUP '*PRINT-LEVEL* STACK-GROUP))
         (*PRINT-LENGTH* (SYMEVAL-IN-STACK-GROUP '*PRINT-LENGTH* STACK-GROUP))
         (*READTABLE* (SYMEVAL-IN-STACK-GROUP '*READTABLE* STACK-GROUP)))
     (LETF (((SYMEVAL-IN-INSTANCE SELF 'LINE-OVERFLOW-ALLOWED) NIL))
       . ,BODY)))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :PRINT-ITEM) (ITEM LINE-NO ITEM-NO
                                                        &AUX VAR VAL STR FONTNO
                                                             CHOICES PF RF K&A
                                                             GPVF GVVF PVAL CVAL)
  LINE-NO ITEM-NO ;ignored
  ;; Parse ITEM into label string, font to print that in, variable, and keyword-&-arguments
  (COND ((STRINGP ITEM)
         (SETQ STR ITEM FONTNO 0))
        ((SYMBOLP ITEM)
         (SETQ VAR ITEM STR (GET-PNAME VAR) FONTNO 1))
        (T (SETQ VAR (CAR ITEM)
                 STR (IF (OR (STRINGP (CADR ITEM)) (NUMBERP (CADR ITEM)) (NULL (CADR ITEM)))
                         (CAR (SETQ ITEM (CDR ITEM)))
                       (IF (SYMBOLP VAR)
                           (GET-PNAME VAR)
                         NIL))
                 FONTNO 1
                 K&A (CDR ITEM))))
  ;; If any label string, print it and a colon
  (COND ((EQ (CAR K&A) 'MORE-CHOICES)
         (SHEET-SET-FONT SELF (AREF FONT-MAP FONTNO))
         (SHEET-STRING-OUT SELF "     "))
        (STR
         (SHEET-SET-FONT SELF (AREF FONT-MAP FONTNO))
         (SHEET-STRING-OUT SELF STR)
         (IF VAR (SHEET-STRING-OUT SELF ": "))))
  ;; If any variable, get its value and decide how to print it
  (WHEN VAR
    (SETQ VAL (IF (SYMBOLP VAR)
                  (SYMEVAL-IN-STACK-GROUP VAR STACK-GROUP)
                (CAR VAR)))
    (MULTIPLE-VALUE (PF RF CHOICES GPVF GVVF)
      (SEND SELF ':DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
    (COND ((NOT CHOICES)
           (SHEET-SET-FONT SELF (AREF FONT-MAP 2))
           (SEND SELF ':ITEM1 VAL ':VARIABLE-CHOICE PF))
          (T (LET (LAST-X (CHOICES-LEFT CHOICES))
               (CATCH 'LINE-OVERFLOW
                 (DO () ((NULL CHOICES-LEFT))
                   (LET ((CHOICE (CAR CHOICES-LEFT)))
                     (SETQ PVAL (IF GPVF (FUNCALL GPVF CHOICE) CHOICE)
                           CVAL (IF GVVF (FUNCALL GVVF CHOICE) CHOICE))
                     (SHEET-SET-FONT SELF (AREF FONT-MAP (IF (EQUAL CVAL VAL) 4 3)))
                     (SETQ LAST-X CURSOR-X)
                     (SEND SELF ':ITEM1 CHOICE ':VARIABLE-CHOICE
                                   'CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION PF PVAL)
                     (SEND SELF ':TYO #/SPACE))
                   (POP CHOICES-LEFT)))
               ;; If this is a contin line and not even one choice fits,
               ;; leave that gigantic choice name on this line.
               (WHEN (AND (EQ CHOICES-LEFT CHOICES)
                          (EQ (THIRD ITEM) 'MORE-CHOICES))
                 (SETQ LAST-X CURSOR-X)
                 (POP CHOICES-LEFT))
               ;; If choices don't fit on line, push some into following line.
               (WHEN CHOICES-LEFT
                 (SHEET-SET-CURSORPOS SELF (- LAST-X LEFT-MARGIN-SIZE)
                                      (- CURSOR-Y TOP-MARGIN-SIZE))
                 (SHEET-CLEAR-EOL SELF)
                 (LET* ((NO-ITEMS (ARRAY-LEADER ITEMS 0))
                        (NEXT-ITEM
                          (IF (> NO-ITEMS (1+ ITEM-NO))
                              (AREF ITEMS (1+ ITEM-NO)))))
                   ;; See if we ALREADY made a continuation item for this one.
                   (UNLESS (AND (CONSP NEXT-ITEM)
                                (EQ (THIRD NEXT-ITEM) 'MORE-CHOICES))
                     ;; If not, make one, and put into it
                     ;; all the choices we could not fit on this line.
                     (ARRAY-PUSH-EXTEND ITEMS NIL)
                     (DO ((I 1 (1+ I))
                          (LIM (- NO-ITEMS ITEM-NO)))
                         ((= I LIM))
                       ;; Bubble items up
                       (ASET (AREF ITEMS (- NO-ITEMS I)) ITEMS (1+ (- NO-ITEMS I))))
                     (SETF (AREF ITEMS (1+ ITEM-NO))
                           (LIST VAR NIL 'MORE-CHOICES
                                 (IF (EQ (THIRD ITEM) 'MORE-CHOICES)
                                     (FOURTH ITEM) ITEM)
                                 CHOICES-LEFT PF GPVF GVVF))))))))))

(DEFUN (MORE-CHOICES CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) (KWD-AND-ARGS)
  (VALUES (FOURTH KWD-AND-ARGS) NIL (THIRD KWD-AND-ARGS)
          (FIFTH KWD-AND-ARGS) (SIXTH KWD-AND-ARGS)))

;; Return the item numbers of the first and last items
;; that are really all continuations of item number ITEM-NO.
;; In the usual case, both values equal ITEM-NO.  Both are inclusive.
(DEFUN CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS (WINDOW ITEM-NO)
  (LET ((FIRST-ITEM ITEM-NO)
        (LAST-ITEM ITEM-NO)
        (ITEMS (SEND WINDOW ':ITEMS))
        TEM)
    (DO ()
        ((= FIRST-ITEM 0))
      (DECF FIRST-ITEM)
      (IF (OR (ATOM (SETQ TEM (AREF ITEMS FIRST-ITEM)))
              (NEQ (THIRD TEM) 'MORE-CHOICES))
          (RETURN)))
    (DO ((END (1- (SEND WINDOW ':NUMBER-OF-ITEMS))))
        ((OR (= LAST-ITEM END)
             (ATOM (SETQ TEM (AREF ITEMS (1+ LAST-ITEM))))
             (NEQ (THIRD TEM) 'MORE-CHOICES)))
      (INCF LAST-ITEM))
    (VALUES FIRST-ITEM LAST-ITEM)))

(DEFUN CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION (ITEM WINDOW PF PVAL)
  ITEM ;ignored
  (FUNCALL PF PVAL WINDOW))

;Modified from the :PRINT-ITEM method.  Hard to be completely modular about this.
;Extra-width is amount of space to allow for non-menu items to grow
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :ITEM-WIDTH) (ITEM &OPTIONAL (EXTRA-WIDTH 0) ITEM-NO
                                                       &AUX VAR VAL STR FONTNO
                                                            CHOICES PF RF K&A
                                                            GPVF GVVF PVAL (X 0))
  ITEM-NO
  ;; Parse ITEM into label string, font to print that in, variable, and keyword-&-arguments
  (COND ((STRINGP ITEM)
         (SETQ STR ITEM FONTNO 0))
        ((SYMBOLP ITEM)
         (SETQ VAR ITEM STR (GET-PNAME VAR) FONTNO 1))
        (T (SETQ VAR (CAR ITEM)
                 STR (IF (OR (STRINGP (CADR ITEM)) (NULL (CADR ITEM)))
                         (CAR (SETQ ITEM (CDR ITEM)))
                         (GET-PNAME VAR))
                 FONTNO 1
                 K&A (CDR ITEM))))
  ;; If any label string, print it and a colon
  (COND (STR
         (SETQ X (SEND SELF ':STRING-LENGTH STR 0 NIL NIL (AREF FONT-MAP FONTNO) X))
         (SETQ X (SEND SELF ':STRING-LENGTH ": " 0 NIL NIL (AREF FONT-MAP FONTNO) X))))
  ;; If any variable, get its value and decide how to print it
  (WHEN VAR
    (SETQ VAL (IF (SYMBOLP VAR)
                  (SYMEVAL-IN-STACK-GROUP VAR STACK-GROUP)
                (CAR VAR)))
    (MULTIPLE-VALUE (PF RF CHOICES GPVF GVVF)
      (SEND SELF ':DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
    (COND ((NOT CHOICES)
           (SETQ X (+ (SEND SELF ':STRING-LENGTH
                                    (FORMAT:OUTPUT NIL
                                      (FUNCALL PF VAL *STANDARD-OUTPUT*))
                                    0 NIL NIL (AREF FONT-MAP 2) X)
                      EXTRA-WIDTH)))
          (T (DOLIST (CHOICE CHOICES)
               (SETQ PVAL (IF GPVF (FUNCALL GPVF CHOICE) CHOICE)
                     CHOICE (IF GVVF (FUNCALL GVVF CHOICE) CHOICE))
               (SETQ X (SEND SELF ':STRING-LENGTH
                                     (FORMAT:OUTPUT NIL
                                       (FUNCALL PF PVAL *STANDARD-OUTPUT*))
                                     0 NIL NIL
                                     (AREF FONT-MAP (IF (EQUAL CHOICE VAL) 4 3)) X))
               (INCF X CHAR-WIDTH)))))
  X)

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :APPROPRIATE-WIDTH) (&OPTIONAL EXTRA-WIDTH)
  "Returns the inside-width appropriate to accommodate the current set of variables
   with their current values.  If EXTRA-WIDTH is specified that much room for expansion,
   which can be a number of characters or a string, is left after non-menu items."
  (SETQ EXTRA-WIDTH
        (COND ((STRINGP EXTRA-WIDTH) (SEND SELF ':STRING-LENGTH EXTRA-WIDTH))
              ((NUMBERP EXTRA-WIDTH) (* CHAR-WIDTH EXTRA-WIDTH))
              (T 0)))
  (MIN (MAX (SEND SELF ':LABEL-SIZE)
            (LET ((NITEMS (SEND SELF ':NUMBER-OF-ITEMS)))
              (LOOP FOR I = 0 THEN (1+ I)
                    UNTIL (AND ( I NITEMS)
                               ( I (SETQ NITEMS (SEND SELF ':NUMBER-OF-ITEMS))))
                    AS ITEM = (AREF ITEMS I)
                    MAXIMIZE (SEND SELF ':ITEM-WIDTH ITEM EXTRA-WIDTH I))))
       (- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))

;This is quite a bit slower than it needs to be.  However these windows aren't used much.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :WHO-LINE-DOCUMENTATION-STRING) ()
  (MULTIPLE-VALUE-BIND (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
        (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
    (LET ((X (- MOUSE-X WINDOW-X-OFFSET))
          (Y (- MOUSE-Y WINDOW-Y-OFFSET)))
      (MULTIPLE-VALUE-BIND (VALUE TYPE) (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y)
        (AND TYPE
             (LET ((ITEM (AREF ITEMS (+ TOP-ITEM (TRUNCATE (- Y (SHEET-INSIDE-TOP)) LINE-HEIGHT)))))
               (IF (ATOM ITEM) "Click left to input a new value from the keyboard."
                   (SETQ ITEM (CDR ITEM))
                   (AND (OR (STRINGP (CAR ITEM)) (NULL (CAR ITEM)))
                        (SETQ ITEM (CDR ITEM)))
                   (MULTIPLE-VALUE-BIND (IGNORE RF IGNORE IGNORE IGNORE DOC)
                       (SEND SELF ':DECODE-VARIABLE-TYPE (OR ITEM '(:SEXP)))
                     (COND ((STRINGP DOC) DOC)
                           ((AND DOC (FUNCALL DOC VALUE)))
                           ((NULL RF) "Click left to change to this value.")
                           (T "Click left to input a new value from the keyboard."))))))))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :MOUSE-CLICK) (BUTTON X Y &AUX VALUE TYPE LINE-NO)
  (COND ((OR (CHAR= BUTTON #/MOUSE-1-1) (CHAR= BUTTON #/MOUSE-3-1))
         (MULTIPLE-VALUE (VALUE TYPE) (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y))
         (COND (TYPE
                (SETQ LINE-NO (TRUNCATE (- Y (SHEET-INSIDE-TOP)) LINE-HEIGHT))
                (SEND SELF ':FORCE-KBD-INPUT
                              (LIST TYPE SELF (AREF ITEMS (+ TOP-ITEM LINE-NO))
                                    VALUE LINE-NO BUTTON))
                T)))))

;;; Called when a :VARIABLE-CHOICE message comes back through the io-buffer
;;; This is not a message, so that instance-variables won't be bound in it
;;; This is assumed to be called in the relevant stack group and binding environment
(DEFUN CHOOSE-VARIABLE-VALUES-CHOICE (WINDOW ITEM CHOICE LINE-NO BUTTON
                                      &AUX FCN STR VAR OLDVAL NEWVAL NO-CHANGE
                                           K&A PF RF GPVF GVVF CHOICES REDIS)
  ;; Parse ITEM into label string, variable, and keyword-&-arguments
  (COND ((STRINGP ITEM)
         (SETQ STR ITEM))       ;Can't happen
        ((SYMBOLP ITEM)
         (SETQ VAR ITEM STR (GET-PNAME VAR)))
        (T (SETQ VAR (CAR ITEM)
                 STR (IF (OR (STRINGP (CADR ITEM)) (NULL (CADR ITEM)))
                         (CAR (SETQ ITEM (CDR ITEM)))
                         (GET-PNAME VAR))
                 K&A (CDR ITEM))))
  (MULTIPLE-VALUE (PF RF CHOICES GPVF GVVF)
    (SEND WINDOW ':DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
  (COND ((NOT (NULL RF))        ;Not "menu" case
         (SHEET-SET-FONT WINDOW (AREF (SHEET-FONT-MAP WINDOW) 1))
         (LET ((BL (SHEET-FOLLOWING-BLINKER WINDOW))
               (WS (SEND WINDOW :STATUS)))
           (UNWIND-PROTECT
             (PROGN (SEND WINDOW :SELECT)
                    ;; Next line makes the mouse highlight go away
                    (SEND WINDOW :SET-SENSITIVE-ITEM-TYPES NIL)
                    (SEND BL :SET-VISIBILITY ':BLINK)
                    (SEND WINDOW :SET-CURSORPOS
                                 (IF (NULL STR) 0
                                   (+ (SHEET-STRING-LENGTH WINDOW (STRING STR))
                                      (SHEET-CHAR-WIDTH WINDOW)))
                                 (* LINE-NO (SHEET-LINE-HEIGHT WINDOW)))
                    (SEND WINDOW :CLEAR-REST-OF-LINE)
                    (IF (CONSP RF) (SETQ NEWVAL (SEND (CAR RF) WINDOW))
                        ;; Hair for over-rubout => save old value
                      (DO ((CH) (FULL-RUBOUT T) (REDISPLAY-FLAG NIL)
                           (FIRST-TIME T NIL)
                           (*TERMINAL-IO* WINDOW))      ;Should be ERROR-OUTPUT
                          ((NOT FULL-RUBOUT))
                        (UNLESS FIRST-TIME
                          (AND (CHAR= (SETQ CH (SEND WINDOW :TYI)) #/RUBOUT)
                               (RETURN (SETQ NO-CHANGE T)))
                          (SEND WINDOW ':UNTYI CH))
                        (CONDITION-CASE (ERROR)
                            (MULTIPLE-VALUE (NEWVAL FULL-RUBOUT)
                              (SEND WINDOW :RUBOUT-HANDLER
                                    (IF (OR (CHAR= BUTTON #/MOUSE-1-1)
                                            (NOT FIRST-TIME))
                                        '((:FULL-RUBOUT T))
                                      `((:FULL-RUBOUT T)
                                        (:INITIAL-INPUT
                                          ,(WITH-OUTPUT-TO-STRING (STRM)
                                             (FUNCALL PF
                                                      (IF (SYMBOLP VAR)
                                                          (SYMEVAL VAR)
                                                        (CONTENTS VAR))
                                                      STRM)))))
                                    (LAMBDA (RF STREAM &AUX WIN)
                                      (UNWIND-PROTECT
                                          (PROG1
                                            (FUNCALL RF STREAM)
                                            (SETQ WIN T))
                                        (UNLESS WIN
                                          (SETQ REDISPLAY-FLAG T))))
                                    RF WINDOW))
                          (ERROR
                           (SEND WINDOW :FRESH-LINE)
                           (SEND ERROR :REPORT WINDOW)
                           (LET ((CH (SEND WINDOW :TYI)))
                             (UNLESS (CHAR= CH #/SP)
                               (SEND WINDOW :UNTYI CH)))
                           ;; Redisplay changed lines.
                           ;; :REDISPLAY doesn't erase first, so erase those lines
                           (SEND WINDOW :DRAW-RECTANGLE
                                        (SHEET-INSIDE-WIDTH WINDOW)
                                        (- (+ (SHEET-CURSOR-Y WINDOW)
                                              (SHEET-LINE-HEIGHT WINDOW))
                                           (* LINE-NO (SHEET-LINE-HEIGHT WINDOW))
                                           (SHEET-INSIDE-TOP WINDOW))
                                        0 (* LINE-NO (SHEET-LINE-HEIGHT WINDOW))
                                        (SHEET-ERASE-ALUF WINDOW))
                           (SEND WINDOW :REDISPLAY LINE-NO
                                        (TRUNCATE (- (+ (SHEET-CURSOR-Y WINDOW)
                                                        (SHEET-LINE-HEIGHT WINDOW))
                                                     (* LINE-NO (SHEET-LINE-HEIGHT WINDOW))
                                                     (SHEET-INSIDE-TOP WINDOW))
                                                  (SHEET-LINE-HEIGHT WINDOW)))
                           (SEND WINDOW :SET-CURSORPOS
                                        (IF (NULL STR) 0
                                          (+ (SHEET-STRING-LENGTH WINDOW (STRING STR))
                                             (SHEET-CHAR-WIDTH WINDOW)))
                                        (* LINE-NO (SHEET-LINE-HEIGHT WINDOW)))
                           (SEND WINDOW :CLEAR-REST-OF-LINE)
                           (SETQ FULL-RUBOUT T REDIS T)))
                        ;; If we got a read error, try to avoid garbage in the display
                        ;; This is really a kludge, is there a better way?
                        (SETQ REDIS REDISPLAY-FLAG))))
             (SEND BL :SET-VISIBILITY NIL)
             (SEND WINDOW :SET-SENSITIVE-ITEM-TYPES T)
             (OR (EQ WS ':SELECTED) (SEND WINDOW :SET-STATUS WS)))))
        (T (SETQ NEWVAL CHOICE)))
  (AND GVVF (SETQ NEWVAL (FUNCALL GVVF NEWVAL)))
  (SETQ OLDVAL (IF (SYMBOLP VAR)
                   (SYMEVAL VAR)
                 (CAR VAR)))
  (AND NO-CHANGE (SETQ NEWVAL OLDVAL))
  (IF (SYMBOLP VAR)
      (SET VAR NEWVAL)
      (RPLACA VAR NEWVAL))
  (OR (AND (SETQ FCN (SEND WINDOW :FUNCTION))
           (FUNCALL FCN WINDOW VAR OLDVAL NEWVAL))
      ;; Redisplay
      (LET ((LAST-LINE-CLOBBERED
             (1+ (IF (NULL RF) LINE-NO  ;If menu always one line, otherwise could have cr'ed
                     (TRUNCATE (- (SHEET-CURSOR-Y WINDOW) (SHEET-INSIDE-TOP WINDOW))
                               (SHEET-LINE-HEIGHT WINDOW)))))
            (N-LINES (TRUNCATE (SHEET-INSIDE-HEIGHT WINDOW) (SHEET-LINE-HEIGHT WINDOW)))
            ;; In menu case, more than one item can be affected
            ;; since this variable's choices may span several lines (items).
            (THIS-ITEM (+ LINE-NO (SEND WINDOW ':TOP-ITEM)))
            FIRST-ITEM LAST-ITEM)
        (UNLESS RF
          ;; so figure out first and last item affected.
          (SETF (VALUES FIRST-ITEM LAST-ITEM)
                (CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS
                  WINDOW THIS-ITEM))
          ;; Extend range of lines to redisplay
          ;; by number ofitems that are relevant before or after this one.
          (SETQ LAST-LINE-CLOBBERED (MIN N-LINES (+ LINE-NO 1 (- LAST-ITEM THIS-ITEM)))
                LINE-NO (MAX 0 (- LINE-NO (- THIS-ITEM FIRST-ITEM)))))
        (AND (OR ( LAST-LINE-CLOBBERED LINE-NO) ;wrap-around => full redisplay
                 REDIS)
             (SETQ LAST-LINE-CLOBBERED N-LINES
                   LINE-NO 0))
        (SHEET-FORCE-ACCESS (WINDOW T)
          ;; :REDISPLAY doesn't erase first, so erase those lines
          (SEND WINDOW :DRAW-RECTANGLE
                       (SHEET-INSIDE-WIDTH WINDOW)
                       (* (- LAST-LINE-CLOBBERED LINE-NO) (SHEET-LINE-HEIGHT WINDOW))
                       0 (* LINE-NO (SHEET-LINE-HEIGHT WINDOW)) (SHEET-ERASE-ALUF WINDOW))
          (SEND WINDOW :REDISPLAY LINE-NO LAST-LINE-CLOBBERED)))))

;;; Redisplay a single choice item, when you know its value has been changed elsewhere
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :REDISPLAY-VARIABLE) (VARIABLE)
  (DO ((I 0 (1+ I))
       (NITEMS (ARRAY-ACTIVE-LENGTH ITEMS))
       (ITEM))
      (( I NITEMS)
       (FERROR "~S is not a variable in ~S" VARIABLE SELF))
    (AND (EQ VARIABLE (IF (ATOM (SETQ ITEM (AREF ITEMS I))) ITEM (CAR ITEM)))
         (LET ((LINE-NO (- I TOP-ITEM)))
           (COND ((AND ( LINE-NO 0) (< LINE-NO (SHEET-NUMBER-OF-INSIDE-LINES)))
                  (SEND SELF :DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) LINE-HEIGHT
                                             0 (* LINE-NO LINE-HEIGHT) ERASE-ALUF)
                  (SEND SELF :REDISPLAY LINE-NO (1+ LINE-NO))))
           (RETURN NIL)))))

(DEFFLAVOR CHOOSE-VARIABLE-VALUES-PANE-MIXIN () ())
(DEFFLAVOR CHOOSE-VARIABLE-VALUES-PANE
        ()
        (CHOOSE-VARIABLE-VALUES-PANE-MIXIN CHOOSE-VARIABLE-VALUES-WINDOW))

;;; Let it be determined by the superior
(DEFMETHOD (CHOOSE-VARIABLE-VALUES-PANE-MIXIN :ADJUSTABLE-SIZE-P) ()
  NIL)

;;; Even though we the vertical and horizontal dimensions are independent, this gives
;;; what we prefer.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :PANE-SIZE) (REM-WIDTH REM-HEIGHT
                                                      IGNORE IGNORE STACKING)
  (CASE STACKING
    (:VERTICAL (MIN REM-HEIGHT
                    (+ TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE
                       (* (ARRAY-ACTIVE-LENGTH ITEMS) LINE-HEIGHT))))
    (:HORIZONTAL (MIN REM-WIDTH
                      (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE
                         (SEND SELF ':APPROPRIATE-WIDTH))))))

(DEFFLAVOR TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW ()
           (TEMPORARY-WINDOW-MIXIN CHOOSE-VARIABLE-VALUES-WINDOW))
;Should this send itself a "exit" if it gets deexposed?  I think probably not.

(DEFMETHOD (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW :NAME-FOR-SELECTION) ()
  NIL)

(COMPILE-FLAVOR-METHODS CHOOSE-VARIABLE-VALUES-WINDOW
                        TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
                        CHOOSE-VARIABLE-VALUES-PANE)

(DEFWINDOW-RESOURCE TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW ()
        :MAKE-WINDOW (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW))

(DEFUN CHOOSE-VARIABLE-VALUES (VARIABLES &KEY &OPTIONAL FUNCTION
                               (NEAR-MODE '(:MOUSE)) (LABEL "Choose Variable Values")
                               WIDTH (EXTRA-WIDTH 10.) MARGIN-CHOICES SUPERIOR
                               REVERSE-VIDEO-P
                               &AUX OSW)
  "Invoke a temporary Choose-variable-values window to choose VARIABLES.
VARIABLES is a list of elements, each describing one line of the display
 These become text-scroll items.  Kinds of elements allowed are:
  string - just displayed
  symbol - value is printed, and if the user clicks on it
        with the mouse a new value is read.
  locative - like special-variable but value is accessed by car and written by rplaca
  list - (VAR LABEL TYPE ARGS...).  VAR is the variable (symbol or locative),
        LABEL if not NIL is a string to print instead of VAR's name,
        TYPE is a keyword saying what kinds of values are allowed (default :SEXP),
        and ARGS are args used by the TYPE's parsing functions.
Keyword args are:
:LABEL  Window label (default is /"Choose Variable Values/")
:FUNCTION  Function called if user changes anything (default is NIL)
:NEAR-MODE  Where to appear the window (default is (:MOUSE))
:WIDTH  Desired width of window.  Default is to set wide enough for items.
:EXTRA-WIDTH  Amount of extra width to allow for growing items.  Default 10 characters.
   Each of the above widths may be a number of characters or a string.
:MARGIN-CHOICES  List of elements.  A string is the label for the
   box which means /"exit/" (Default is /"Exit/"), cons of
   a string and a form means eval that form if box clicked upon.
:SUPERIOR  Window to put under, default is MOUSE-SHEET or the superior
   of the window it is supposed to be near, like MENU-CHOOSE.
:REVERSE-VIDEO-P  T means display this window reverse-video."
  ;; Decide what superior to use
  (OR SUPERIOR
      (SETQ SUPERIOR (IF (EQ (CAR NEAR-MODE) ':WINDOW) (SHEET-SUPERIOR (CADR NEAR-MODE))
                       MOUSE-SHEET)))
  ;; MARGIN-CHOICES must always contain a "exit" box so user can stop choosing
  (DO ((L MARGIN-CHOICES (CDR L)))
      ((NULL L) (PUSH "Exit" MARGIN-CHOICES))
    (COND ((STRINGP (CAR L)) (RETURN NIL))
          ((OR (ATOM (CAR L)) (NOT (STRINGP (CAAR L))))
           (FERROR "~S garbage in MARGIN-CHOICES" (CAR L)))))
  (SETQ MARGIN-CHOICES
        (MAPCAR (LAMBDA (X) (LIST (IF (ATOM X) X (CAR X))
                                  NIL
                                  'CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER
                                  NIL
                                  NIL
                                  (IF (ATOM X) NIL (CADR X))))
                MARGIN-CHOICES))
  (DOLIST (ELEM VARIABLES)  ;Make sure all variables are bound, while in caller's environment
    (IF (CONSP ELEM)
        (SETQ ELEM (CAR ELEM)))
    (ETYPECASE ELEM
      (SYMBOL (SYMBOL-VALUE ELEM))
      (LOCATIVE (SETQ ELEM (CAR ELEM)))
      (STRING ELEM)))
  (USING-RESOURCE (WINDOW TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW SUPERIOR)
    (SEND WINDOW :SETUP VARIABLES LABEL FUNCTION MARGIN-CHOICES (OR WIDTH T) EXTRA-WIDTH)
    (SEND WINDOW :SET-REVERSE-VIDEO-P REVERSE-VIDEO-P)
    (SETQ OSW SELECTED-WINDOW)
    (UNWIND-PROTECT
      (LET ((IOB (SEND WINDOW :IO-BUFFER)))
        (IO-BUFFER-CLEAR IOB)
        (DELAYING-SCREEN-MANAGEMENT
          (EXPOSE-WINDOW-NEAR WINDOW NEAR-MODE)
          (SEND WINDOW :SELECT)) ;For who-line
        (DO-FOREVER
          (PROCESS-WAIT "Choose" (LAMBDA (IOB) (NOT (IO-BUFFER-EMPTY-P IOB))) IOB)
          (AND (CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE WINDOW (SEND WINDOW ':ANY-TYI))
               (RETURN NIL))))
      (DELAYING-SCREEN-MANAGEMENT
        (SEND WINDOW :DEACTIVATE)
        (AND OSW (SEND OSW :SELECT NIL))))))

(DEFUN CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE (WINDOW MSG)
  ;; Returns T if message is "exit", else does variable-changing or special action
  ;; and returns NIL.  msg is either a list that came in whose cadr is
  ;; this window, or it is a regular character; only #/FORM is used.
  (COND ((CONSP MSG)
         (CASE (CAR MSG)
           (:MOUSE-BUTTON)                      ;Can get these, randomly.
           (:CHOICE-BOX
            (SETQ MSG (SIXTH (THIRD MSG)))      ;NIL if done or form to eval
            (IF (NULL MSG) T (EVAL MSG)))
           (:VARIABLE-CHOICE
            (APPLY #'CHOOSE-VARIABLE-VALUES-CHOICE (CDR MSG)))
           (OTHERWISE (FERROR "~S unknown message from ~S" MSG WINDOW))))
        ((MEMQ MSG '(#/FORM #.(CHAR-INT #/FORM)))
         (SEND WINDOW ':REFRESH))))

;;; User program macro interface

(DEFMACRO DEFINE-USER-OPTION-ALIST (ALIST &OPTIONAL CONSTRUCTOR DOCUMENTATION)
  "Define ALIST as a user option alist for CHOOSE-USER-OPTIONS.
DOCUMENTATION is documentation of the variable.
CONSTRUCTOR is an unrecommended feature."
  `(PROGN 'COMPILE
     ,(AND CONSTRUCTOR
           `(DEFMACRO ,CONSTRUCTOR (OPTION DEFAULT &OPTIONAL NAME TYPE &REST ARGS)
              `(DEFVAR-USER-OPTION ,OPTION ,DEFAULT NIL ,',ALIST ,NAME ,TYPE . ,ARGS)))
     (DEFVAR ,ALIST NIL ,DOCUMENTATION)))

(DEFMACRO DEFVAR-USER-OPTION (OPTION DEFAULT DOCUMENTATION ALIST
                              &OPTIONAL NAME TYPE &REST ARGS
                              &AUX REALNAME REALTYPE)
  "Define variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
DEFAULT and DOCUMENTATION are used to DEFVAR OPTION.
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name.
TYPE is a keyword for choose-variable-values windows, and
 ARGS are arguments (evaluated at definition time) for that keyword."
  (IF (STRINGP NAME)
      (SETQ REALNAME NAME REALTYPE TYPE)
    (SETQ REALTYPE NAME REALNAME TYPE))
  `(PROGN 'COMPILE
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST ,DEFAULT ',(OR REALTYPE ':SEXP)
                           ',(OR REALNAME (MAKE-OPTION-NAME OPTION)) . ,ARGS)
     (DEFVAR ,OPTION ,DEFAULT ,DOCUMENTATION)))

(DEFUN MAKE-OPTION-NAME (STRING)
  (STRING-CAPITALIZE-WORDS (STRING-TRIM #/* STRING)))

(DEFMACRO DEFINE-USER-OPTION ((OPTION ALIST) DEFAULT &OPTIONAL TYPE NAME &REST ARGS)
  `(PROGN 'COMPILE
     (DEFPARAMETER ,OPTION ,DEFAULT)
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST ,OPTION ',(OR TYPE :SEXP)
                           ',(OR NAME (MAKE-OPTION-NAME OPTION)) . ,ARGS)
     ))

(DEFUN DEFINE-USER-OPTION-1 (OPTION ALIST DEFAULT TYPE NAME &REST ARGS)
  (PUTPROP OPTION DEFAULT 'DEFAULT-VALUE)
  (LET ((ELEM (ASSQ OPTION (SYMEVAL ALIST))))
    (AND ELEM (SET ALIST (DELQ ELEM (SYMEVAL ALIST)))))
  (PUSH (LIST* OPTION NAME TYPE (COPYLIST ARGS))
        (SYMEVAL ALIST)))

(DEFUN RESET-USER-OPTIONS (ALIST)
  "Reset all variables specified by ALIST to their default values.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST
and variables put on it with DEFVAR-USER-OPTION."
  (DO ((X ALIST (CDR X))
       (SYM))
      ((NULL X))
    (SETQ SYM (CAAR X))
    (SET SYM (GET SYM 'DEFAULT-VALUE))))

(DEFUN CHOOSE-USER-OPTIONS (ALIST &REST ARGS)
  "Invoke a pop-up choose-variable-values window on variables specified by ALIST.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST
and variables put on it with DEFVAR-USER-OPTION."
  (APPLY #'CHOOSE-VARIABLE-VALUES ALIST ARGS))

(DEFUN WRITE-USER-OPTIONS (ALIST STREAM &AUX (SI:PRINT-READABLY T))
  "Write forms on STREAM to set all non-default variables on ALIST to their current values.
That is, for each variable on ALIST whose current value is not its default,
a LOGIN-SETQ form is output to STREAM which records the variable's
current value."
  (DO ((ALIST ALIST (CDR ALIST))
       (OPTION) (type) (DEFAULT) (VALUE))
      ((NULL ALIST))
    (SETQ OPTION (CAAR ALIST)
          type (third (car alist))                      ; The kind of value
                                                ; For now, only :HOST-OR-NIL is special.
          DEFAULT (GET OPTION 'DEFAULT-VALUE)
          VALUE (SYMEVAL OPTION))
    (OR (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
          (EQUAL VALUE DEFAULT))
        (progn (cond ((OR (NUMBERP VALUE) (MEMQ VALUE '(T NIL))))
                     ((eq type :host-or-nil)    ; We can't qfasl hosts, and they should
                                                ; be looked up in the load world anyway.
                      (etypecase value
                        (si:host (setq value `(si:parse-host ,(send value :name))))
                        ((or string symbol)
                         (setq value `(si:parse-host ',value)))))
                     (t (setq value `',VALUE)))
               (GRIND-TOP-LEVEL `(LOGIN-SETQ ,OPTION ,value)
                                95. STREAM)))))

;;; Site dependent versions
(DEFMACRO DEFVAR-SITE-USER-OPTION (OPTION KEYWORD DOCUMENTATION ALIST
                                   &OPTIONAL NAME TYPE &REST ARGS
                                   &AUX REALNAME REALTYPE)
  "Define site-dependent variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
KEYWORD is a site option name whose value,
 according to current site configuration, will be the default for OPTION.
DOCUMENTATION is documentation for OPTION as a variable (a la DEFVAR).
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name.
TYPE is a keyword for choose-variable-values windows, and
 ARGS are arguments (evaluated at definition time) for that keyword."
  (IF (STRINGP NAME)
      (SETQ REALNAME NAME REALTYPE TYPE)
    (SETQ REALTYPE NAME REALNAME TYPE))
  `(PROGN 'COMPILE
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST NIL ',(OR REALTYPE ':SEXP)
                           ',(OR REALNAME (MAKE-OPTION-NAME OPTION)) . ,ARGS)
     (DEFVAR ,OPTION :UNBOUND ,DOCUMENTATION)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" OPTION)
                         `(RESET-USER-OPTION ',',OPTION (SI:GET-SITE-OPTION ',',KEYWORD))
                         '(SITE-OPTION))))

(DEFMACRO DEFINE-SITE-USER-OPTION ((OPTION ALIST) KEYWORD &OPTIONAL TYPE NAME &REST ARGS)
  `(PROGN 'COMPILE
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST NIL ',(OR TYPE ':SEXP)
                           ',(OR NAME (MAKE-OPTION-NAME OPTION)) . ,ARGS)
     (DEFVAR ,OPTION)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" OPTION)
                         `(RESET-USER-OPTION ',',OPTION (SI:GET-SITE-OPTION ',',KEYWORD))
                         '(SITE-OPTION))))

;;; Change the default value of an option
(DEFUN RESET-USER-OPTION (OPTION VALUE)
  "Set user-option variable OPTION to VALUE, and make VALUE the new default."
  (SET OPTION VALUE)
  (PUTPROP OPTION VALUE 'DEFAULT-VALUE))

;;; A :MENU-ALIST type variable whose alist changes
(DEFMACRO DEFVAR-SITE-ALIST-USER-OPTION (OPTION DEFAULT DOCUMENTATION ALIST NAME MENU-ALIST)
  "Define variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
MENU-ALIST is a menu item list of possible values of OPTION;
 but each item is actually valid only if a corresponding site option's
 value (in the current site configuration) is non-NIL.
 The site option name is by default the same as the menu item value;
  but the item may contain :SITE-KEYWORD followed by a site option name.
DEFAULT is the name of a site keyword whose value, in the current
 site configuration, is the default value of OPTION.
DOCUMENTATION is documentation for OPTION as a variable (a la DEFVAR).
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name."
  `(PROGN 'COMPILE
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST NIL ':MENU-ALIST
                           ',(OR NAME (MAKE-OPTION-NAME OPTION))
                           ,MENU-ALIST)
     (DEFVAR ,OPTION :UNBOUND ,DOCUMENTATION)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" OPTION)
                         `(RESET-ALIST-USER-OPTION ',',OPTION ,',ALIST ,',MENU-ALIST
                                                   ',',DEFAULT)
                         '(SITE-OPTION))))

;;; A :MENU-ALIST type variable whose alist changes
(DEFMACRO DEFINE-SITE-ALIST-USER-OPTION ((OPTION ALIST) NAME MENU-ALIST &OPTIONAL DEFAULT)
  `(PROGN 'COMPILE
     (DEFINE-USER-OPTION-1 ',OPTION ',ALIST NIL ':MENU-ALIST
                           ',(OR NAME (MAKE-OPTION-NAME OPTION))
                           ,MENU-ALIST)
     (DEFVAR ,OPTION)
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" OPTION)
                         `(RESET-ALIST-USER-OPTION ',',OPTION ,',ALIST ,',MENU-ALIST
                                                   ',',DEFAULT)
                         '(SITE-OPTION))))

(DEFUN RESET-ALIST-USER-OPTION (OPTION ALIST MENU-ALIST DEFAULT)
  (AND DEFAULT
       (SETQ DEFAULT (SI:GET-SITE-OPTION DEFAULT)))
  (LOOP FOR ELEM IN MENU-ALIST
        AS SITE-KEYWORD = (OR (AND (CONSP (CDR ELEM)) (GET ELEM ':SITE-KEYWORD))
                              (MENU-EXECUTE-NO-SIDE-EFFECTS ELEM))
        AS DEFAULT-SITE-KEYWORD = (OR (AND (CONSP (CDR ELEM))
                                           (GET ELEM ':DEFAULT-SITE-KEYWORD))
                                      SITE-KEYWORD)
        WHEN (NOT (NULL (SI:GET-SITE-OPTION SITE-KEYWORD)))
        COLLECT ELEM INTO NEW-ALIST
        WITH DEFAULT-ELEM
        WHEN (AND (EQ DEFAULT-SITE-KEYWORD DEFAULT) (NULL DEFAULT-ELEM))
        DO (SETQ DEFAULT-ELEM ELEM)
        FINALLY (AND DEFAULT-ELEM (SETQ NEW-ALIST (CONS DEFAULT-ELEM
                                                        (DELQ DEFAULT-ELEM NEW-ALIST))))
                (SETQ MENU-ALIST NEW-ALIST))
  (LET ((ELEM (ASSQ OPTION ALIST)))
    (SETF (FOURTH ELEM) MENU-ALIST))
  (RESET-USER-OPTION OPTION (AND MENU-ALIST
                                 (MENU-EXECUTE-NO-SIDE-EFFECTS (CAR MENU-ALIST)))))

(DEFMACRO RESTRICT-USER-OPTION (OPTION RESTRICTION-TYPE &REST SITE-KEYWORDS &AUX IF IF-NOT)
  "Allow site tables to control whether OPTION is significant.
OPTION should be a user-option variable (see DEFVAR-USER-OPTION).
RESTRICTION-TYPE should be :IF, :UNLESS or :NEVER.
 :IF means offer OPTION to the user only if one of
  SITE-KEYWORDS is non-NIL in the current site configuration.
 :UNLESS means offer OPTION only if none of SITE-KEYWORDS
  are non-NIL in the current site configuration.
 :NEVER means never offer OPTION to the user.
Restrictions are actually implemented by TV:PRUNE-USER-OPTION-ALIST;
to avoid presenting suppressed options to the user, you must
call that function."
  (SETQ SITE-KEYWORDS (COPYLIST SITE-KEYWORDS))
  (SELECTQ RESTRICTION-TYPE
    (:IF (SETQ IF SITE-KEYWORDS))
    (:UNLESS (SETQ IF-NOT SITE-KEYWORDS))
    (:NEVER (SETQ IF-NOT T)))
  `(DEFPROP ,OPTION ,(OR IF IF-NOT)
            ,(IF IF 'SITE-KEYWORDS-RESTRICTION 'NOT-SITE-KEYWORDS-RESTRICTION)))

;;; This removes all user options that are restricted or choices with less than two
;;; possibilities.
(DEFUN PRUNE-USER-OPTION-ALIST (ALIST)
  "Return a subset of ALIST eliminating trivial choices.
Options on ALIST that are restricted, or that have only one valid
alternative, are omitted from the value returned.  Usage:
 (CHOOSE-USER-OPTIONS (PRUNE-USER-OPTION-ALIST ALIST))."
  (LOOP FOR ELEM IN ALIST
        AS OPTION = (CAR ELEM)
        WITH TEM
        UNLESS (OR (AND (NOT (NULL (SETQ TEM (GET OPTION 'NOT-SITE-KEYWORDS-RESTRICTION))))
                        (OR (EQ TEM T)
                            (LOOP FOR KEY IN TEM
                                  THEREIS (SI:GET-SITE-OPTION KEY))))
                   (AND (NOT (NULL (SETQ TEM (GET OPTION 'SITE-KEYWORDS-RESTRICTION))))
                        (NOT (LOOP FOR KEY IN TEM
                                   ALWAYS (SI:GET-SITE-OPTION KEY))))
                   (AND (MEMQ (THIRD ELEM) '(:ASSOC :MENU-ALIST))
                        (NULL (CDR (FOURTH ELEM)))))
        COLLECT ELEM))

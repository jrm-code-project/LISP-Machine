;;; -*- Mode:LISP; Package:TV; Readtable:T; Base:8 -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

#|
;;;Obsolete list of instance variables
(DEFVAR |*All-instance-variables-on-one-page/|*|
  '(BASELINE BASELINE-ADJ BIT-ARRAY BITS-PER-PIXEL BLANK-RECTANGLES
    BLINKER-LIST BORDER-MARGIN-WIDTH BORDERS BOTTOM-ITEM BOTTOM-MARGIN-SIZE
    BOTTOM-REACHED BUFFER BUFFER-HALFWORD-ARRAY CACHE CHAR CHAR-ALUF
    CHAR-WIDTH CHOICE-TYPES CHOICE-VALUE CHOSEN-ITEM COLUMN-WIDTH COLUMNS
    CONSTRAINTS CONTROL-ADDRESS CURRENT-DISPLAY CURRENT-FONT CURRENT-ITEM
    CURRENT-OBJECT CURRENT-REGION CURSOR-X CURSOR-Y DEEXPOSED-TYPEOUT-ACTION
    DEFAULT-FONT DESELECTED-VISIBILITY DISPLAY-ITEM DISPLAYED-ITEMS
    DISPLAYING-LIST ERASE-ALUF EXPOSED-INFERIORS EXPOSED-P EXPOSED-PANES
    FLAGS FLASHY-SCROLLING-BLINKER FLASHY-SCROLLING-MAX-SPEED
    FLASHY-SCROLLING-REGION FOLLOW-P FONT FONT-ALIST FONT-MAP FROBS FUNCTION
    GEOMETRY GRAY-ARRAY HAD-MOUSE-P HALF-PERIOD HAVE-EDGES HEIGHT HYSTERESIS
    INCOMPLETE-P INFERIORS INSPECTORS INTERNAL-CONSTRAINTS INTERNAL-PANES
    INVISIBLE-TO-MOUSE-P IO-BUFFER ITEM-BLINKER ITEM-LIST ITEM-LIST-POINTER
    ITEM-NAME ITEM-TYPE-ALIST ITEMS LABEL LABEL-NEEDS-UPDATING LAST-ITEM
    LEFT-MARGIN-SIZE LEVEL-COUNT LINE-HEIGHT LINE-OVERFLOW-ALLOWED
    LIST-BLINKER LIST-ITEM LOCATIONS-PER-LINE LOCK LOCK-COUNT
    MAKING-SCROLL-DECISION MARGIN-CHOICES MENU MODE-ALIST MODIFY-MODE
    MORE-VPOS MOUSE-BLINKERS NAME NEEDS-REDISPLAY OLD-BORDERS OLD-LABEL
    OLD-SCREEN-ARRAY OLD-TYPEAHEAD OLD-X OLD-Y OUTPUT-LOCK PANES
    PARSED-CONSTRAINTS PHASE PRINT-FUNCTION PRINT-FUNCTION-ARG PRIORITY
    PROCESS PROPERTY-LIST RECT-HEIGHT RECT-WIDTH RECTANGLE-LIST RECURSION
    REGION-LIST RESTORED-BITS-P RIGHT-MARGIN-SIZE ROW-HEIGHT ROW-MAP
    RUBOUT-HANDLER-BUFFER SCREEN-ARRAY SCREEN-IMAGE SCREEN-LINES
    SCREEN-MANAGER-SCREEN-IMAGE SCREEN-ROWS SCROLL-BAR
    SCROLL-BAR-ALWAYS-DISPLAYED SCROLL-BAR-IN SELECTED-PANE
    SENSITIVE-ITEM-TYPES SET-EDGES-MODE SHEET SINGLE-RIGHT-MENU STACK-GROUP
    STATE SUBSTITUTIONS SUPERIOR TARGET-TOP-ITEM TEMPORARY-BIT-ARRAY
    TEMPORARY-WINDOWS-LOCKED TIME-STAMP TIME-UNTIL-BLINK TOP-ITEM TOP-MARGIN-SIZE
    TOP-ROW TOTAL-ROWS TRI-HEIGHT TRI-WIDTH TRUNCATION TYPE-ALIST
    TYPEOUT-WINDOW VALUE-ARRAY VISIBILITY WIDTH WINDOW-OF-INTEREST
    WINDOW-UNDER-MENU X-OFFSET X-ORIGIN X-POS Y-OFFSET Y-ORIGIN Y-POS))
|#

(DEFVAR DEFAULT-SCREEN NIL
  "This is the screen on which windows are created by default.")
(DEFVAR ALL-THE-SCREENS NIL
  "List of all screen objects.")
(DEFVAR MAIN-SCREEN NIL
  "This is the screen-object for the main black-and-white display,
not counting the who-line.")
(DEFVAR WHO-LINE-SCREEN NIL
  "This is the screen-object for the who-line area.
The parts of the who-line are inferiors of this screen.")
(DEFVAR WHO-LINE-RUN-STATE-SHEET NIL
  "This is the window, part of the who-line, on which /"Run/", etc., appears.")
(DEFVAR WHO-LINE-FILE-STATE-SHEET NIL
  "This is the window, part of the who-line, on which the file being read appears.")
(DEFVAR NWATCH-WHO-LINE-SHEET NIL)
(DEFVAR WHO-LINE-DOCUMENTATION-WINDOW NIL
  "This is the window, part of the who-line, for the mouse-documentation line.")
(DEFVAR WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL
  "If non-NIL, this overrides all other sources of mouse documentation
for the who-line.")
(DEFVAR *DEFAULT-FONT* FONTS:CPTFONT)

(DEFVAR BEEP T)
(DEFVAR BEEP-DURATION #o400000
  "This is the number of cycles of beep tone to output.")
(DEFVAR BEEP-WAVELENGTH #o1350
  "This is the duration of one cycle of beep tone.")

;;; These are here because they are needed in more than one file
(DEFVAR SELECTED-WINDOW NIL "The currently selected window, or NIL.")
(DEFVAR SELECTED-IO-BUFFER NIL
  "The IO-BUFFER that input is currently being directed to.")
(DEFVAR WHO-LINE-PROCESS CURRENT-PROCESS
  "Process that the wholine is should display,
or NIL => selected window's process.")
(PUSH '(SETQ WHO-LINE-PROCESS NIL) LOGOUT-LIST) ;Freeze wholine during loading, unfreeze later
(DEFVAR LAST-WHO-LINE-PROCESS CURRENT-PROCESS
  "The last process which was displayed in the who-line.")
(DEFVAR INHIBIT-WHO-LINE NIL
  "Set this to T with CC if who line is broken.")
(DEFVAR INHIBIT-SCREEN-MANAGEMENT NIL
  "T causes screen management not to happen automatically.
This is set by DELAYING-SCREEN-MANAGEMENT.")
(DEFVAR MOUSE-WINDOW :UNBOUND "Window controlling the mouse, NIL if none")
(DEFVAR MOUSE-RECONSIDER)               ;T => mouse process should return to overseer
                                        ;and decide anew which window should get the mouse.
                                        ;For use by :MOUSE-MOVES methods, etc.
(DEFVAR WINDOW-OWNING-MOUSE NIL
  "NIL, or window which has seized the mouse, or T
if someone has seized the mouse and can't identify
himself as any particular window,
or STOP to make the mouse process do nothing.")
(DEFVAR MOUSE-X :UNBOUND "X coordinate of MOUSE-BLINKER, on MOUSE-SHEET.")
(DEFVAR MOUSE-Y :UNBOUND "Y coordinate of MOUSE-BLINKER, on MOUSE-SHEET.")
(DEFVAR MOUSE-SHEET NIL "The sheet on which the mouse is being tracked.
Usually this is a screen.")

;;; Dummy function for load-time use
(UNLESS (FBOUNDP 'MOUSE-WAKEUP)
  (FSET 'MOUSE-WAKEUP #'(LAMBDA () NIL)))

(DEFVAR SHEET-AREA (MAKE-AREA :NAME 'SHEET-AREA :REGION-SIZE #o400000 :VOLATILITY 1)
  "Area which windows and some related data are consed in.")
(DEFVAR WHO-LINE-AREA (MAKE-AREA :NAME 'WHO-LINE-AREA :REGION-SIZE #o40000 :VOLATILITY 0)
  "Area for the who-line to cons in.")

(DEFMACRO SHEET-CONSING (&BODY BODY)
  "Execute BODY with DEFAULT-CONS-AREA bound to the area SELF is in."
  `(LET ((DEFAULT-CONS-AREA (%AREA-NUMBER SELF)))
     . ,BODY))

(DEFVAR BLINKER-AREA (MAKE-AREA :NAME 'BLINKER-AREA :VOLATILITY 1)
  "Area which blinkers are consed in.")

(DEFFLAVOR SHEET
           ((SCREEN-ARRAY NIL)  ;Array that output goes on.  Either a standard array
                                ; or a section of the physical screen.  May be null when
                                ;deexposed if no BIT-ARRAY. (microcode use)
            LOCATIONS-PER-LINE  ;Number of locations per raster line (microcode use)
            OLD-SCREEN-ARRAY    ;SCREEN-ARRAY when last exposed if there is no BIT-ARRAY
            (BIT-ARRAY NIL)     ;"In-core" array used when sheet not exposed (may be null)

            (NAME NIL)          ;What this here sheet is called
            (LOCK NIL)          ;Lock cell, contains unique-id of owner of lock, or a list
                                ;of temporary locking unique-ids.
            (LOCK-COUNT 0)      ;Number of times lock is locked by this id
                                ;(lock is freed when 0)

            (SUPERIOR MOUSE-SHEET) ;Null superior is top.
            (INFERIORS NIL)

            (EXPOSED-P NIL)     ;T when exposed, NIL otherwise.  In this context "exposed"
                                ;means that it is among the superior's exposed-inferiors
                                ;and the superior either has a bit-array or is exposed.
                                ;T here does not necessarily mean it's visible on the screen.
            (EXPOSED-INFERIORS NIL)

            (X-OFFSET NIL)      ;Position relative to position of superior
            (Y-OFFSET NIL)
            (WIDTH NIL)         ;Size of sheet
            (HEIGHT NIL)

            CURSOR-X            ;Position at which to draw next character
            CURSOR-Y

            MORE-VPOS           ;Y passing here triggers MORE processing

            (TOP-MARGIN-SIZE 0) ;Reserved region around outside of sheet (for borders, etc.)
            (BOTTOM-MARGIN-SIZE 0)
            (LEFT-MARGIN-SIZE 0)
            (RIGHT-MARGIN-SIZE 0)

            (FLAGS 0)           ;A fixnum containing various flags

            ;; Font information
            BASELINE            ;# raster lines from top of char cell to baseline.
            FONT-MAP            ;Map from font numbers to font arrays
            CURRENT-FONT        ;Currently selected font
            BASELINE-ADJ        ;Y offset for current font to align baseline
            LINE-HEIGHT         ;Total number of raster lines per character line
            CHAR-WIDTH          ;Character width for cursor blinker + (X,Y) positioning
            CHAR-ALUF           ;ALU function for drawing characters
            ERASE-ALUF          ;ALU function for erasing characters/lines/whole thing
            (BLINKER-LIST NIL)  ;Possibly null list of blinkers on this sheet

            (DEEXPOSED-TYPEOUT-ACTION ':NORMAL)
            (TEMPORARY-BIT-ARRAY NIL)
            (TEMPORARY-WINDOWS-LOCKED NIL)
            RESTORED-BITS-P
            (INVISIBLE-TO-MOUSE-P NIL)
            (SCREEN-MANAGER-SCREEN-IMAGE NIL)
            (PRIORITY NIL)
            (TIME-STAMP NIL)
            (SELECTION-SUBSTITUTE NIL)
            )
           ()
  :ORDERED-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES DEEXPOSED-TYPEOUT-ACTION CHAR-ALUF ERASE-ALUF)
  (:INITABLE-INSTANCE-VARIABLES
    NAME WIDTH HEIGHT BIT-ARRAY
    CHAR-ALUF ERASE-ALUF
    LEFT-MARGIN-SIZE TOP-MARGIN-SIZE RIGHT-MARGIN-SIZE BOTTOM-MARGIN-SIZE
    SUPERIOR FONT-MAP PRIORITY)
  (:INIT-KEYWORDS :TOP :Y :BOTTOM :LEFT :X :RIGHT :POSITION :EDGES :BLINKER-P :REVERSE-VIDEO-P
                  :CHARACTER-WIDTH :CHARACTER-HEIGHT :INSIDE-SIZE :INSIDE-WIDTH :INSIDE-HEIGHT
                  :MORE-P :VSP :BLINKER-FLAVOR :BLINKER-DESELECTED-VISIBILITY :INTEGRAL-P
                  :SAVE-BITS :RIGHT-MARGIN-CHARACTER-FLAG :TRUNCATE-LINE-OUT-FLAG
                  :BACKSPACE-NOT-OVERPRINTING-FLAG :CR-NOT-NEWLINE-FLAG :AREA
                  :DEEXPOSED-TYPEIN-ACTION :TAB-NCHARS)
  (:INSTANCE-AREA-FUNCTION SHEET-INSTANCE-AREA-FUNCTION)
  (:DEFAULT-INIT-PLIST :TAB-NCHARS 8)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :NOTICE)
                       (:APPEND :BASE-FLAVOR-LAST :PROCESSES)
                       (:PASS-ON (:BASE-FLAVOR-LAST LM TM RM BM) :COMPUTE-MARGINS)
                       (:DAEMON-WITH-OVERRIDE :BASE-FLAVOR-LAST
                                              :WHO-LINE-DOCUMENTATION-STRING))
  (:DOCUMENTATION :LOWLEVEL-MIXIN "A lowest level window type
This is the data structure known about by the microcode."))

(DEFUN SHEET-INSTANCE-AREA-FUNCTION (INIT-PLIST)
  (OR (GET INIT-PLIST ':AREA) SHEET-AREA))

;;;*****
(DEFF SHEET-X 'SHEET-X-OFFSET)
(DEFF SHEET-Y 'SHEET-Y-OFFSET)
(compiler:make-obsolete sheet-x "use TV:SHEET-X-OFFSET")
(compiler:make-obsolete sheet-y "use TV:SHEET-Y-OFFSET")
;;;*****

;;; The font-alists of actual screens are built using this as a pattern.
(DEFCONST DEFAULT-FONT-ALIST
          '((:DEFAULT . FONTS:CPTFONT)
            (:LABEL . FONTS:CPTFONT)
            (:MENU . FONTS:MEDFNT)
            (:MENU-STANDOUT . FONTS:HL12I)
            (:MARGIN-CHOICE . FONTS:CPTFONT))
  "This is a pattern for creating the FONT-ALIST variables of screens.
It maps standard font purposes into names of fonts.")

(DEFFLAVOR SCREEN
           ((BITS-PER-PIXEL 1)  ;For gray or color
            (FONT-ALIST NIL)    ;Maps font purposes to symbols whose values are font names.
            BUFFER              ;Virtual memory address of video buffer
            CONTROL-ADDRESS     ;XBUS I/O address of control register
            BUFFER-HALFWORD-ARRAY       ;One-dimensional array of 16-bit buffer hunks
            (DEFAULT-FONT 'FONTS:CPTFONT)
            PROPERTY-LIST
            (X-OFFSET 0)
            (Y-OFFSET 0)
            (SUPERIOR NIL)
            LOCATIONS-PER-LINE
            (LEVEL-COUNT 0)
            (MOUSE-BLINKERS NIL)
            )
           (SHEET)
; :ORDERED-INSTANCE-VARIABLES                   ;This cannot work
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES BUFFER-HALFWORD-ARRAY DEFAULT-FONT
                                          CONTROL-ADDRESS PROPERTY-LIST FONT-ALIST
                                          BITS-PER-PIXEL BUFFER MOUSE-BLINKERS)
  (:INITABLE-INSTANCE-VARIABLES
    BITS-PER-PIXEL FONT-ALIST BUFFER CONTROL-ADDRESS BUFFER-HALFWORD-ARRAY DEFAULT-FONT
    PROPERTY-LIST LOCATIONS-PER-LINE)
  (:GETTABLE-INSTANCE-VARIABLES MOUSE-BLINKERS BUFFER BUFFER-HALFWORD-ARRAY FONT-ALIST)
  (:SETTABLE-INSTANCE-VARIABLES MOUSE-BLINKERS)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The software data structure for the actual screen
The top of a window hierachy should be of this type.  There will be only one for each
hardware display."))

(DEFMACRO DEFINE-SHEET-FLAGS (&REST FLAGS)
  `(PROGN 'COMPILE
          ,@(MAPCAR #'(LAMBDA (FLAG)
                        `(DEFMACRO ,(INTERN (STRING-APPEND "SHEET-" (CAR FLAG)))
                                   (&OPTIONAL SHEET)
                           `(LDB ,',(CADR FLAG)
                                 ,(IF SHEET `(SHEET-FLAGS ,SHEET) 'FLAGS))))
                    FLAGS)))

(defvar *more-timeout-interval* (* 60. 60.))    ;a minute.

(DEFINE-SHEET-FLAGS
  (time-out-on-more #o0001)     ;After *more-timeout-interval*, if exposed.
  (EXCEPTIONS #o0104)           ;Reasons why typeout can't happen:
   ;(END-LINE-FLAG #o0101)      ;(spare bit no longer used)
    (END-PAGE-FLAG #o0201)      ;Cursor is below bottom limit
    (MORE-FLAG #o0301)          ;More processing needs to happen
    (OUTPUT-HOLD-FLAG #o0401)   ;Output may not happen on this sheet
  (RIGHT-MARGIN-CHARACTER-FLAG #o0501)   ;A special character (!) indicates wraparound
  (TRUNCATE-LINE-OUT-FLAG #o0601)        ;SHEET-LINE-OUT should truncate rather than wrap
  (DONT-BLINK-BLINKERS-FLAG #o0701)     ;Don't blink blinkers on this sheet or its inferiors
  (BACKSPACE-NOT-OVERPRINTING-FLAG #o1001)      ;Backspace is another losenge character
  (CR-NOT-NEWLINE-FLAG #o1101)                  ;Newline is also
  (DEEXPOSED-TYPEIN-NOTIFY #o1201)      ;:DEEXPOSED-TYPEIN-ACTION :NOTIFY
  (FORCE-SAVE-BITS #o1301)              ;Force bit saving on first deexposed
  (TAB-NCHARS #o1407)                   ;Number of character widths in a tab
  )

(defmacro sheet-handle-exceptions-if-necessary (&optional sheet)
  (if sheet
      `(or (zerop (sheet-exceptions ,sheet))
           (sheet-handle-exceptions ,sheet))
    `(or (zerop (sheet-exceptions)) (sheet-handle-exceptions self))))

(DEFMACRO SHEET-END-OF-PAGE-FLAG (&OPTIONAL SHEET)
  "Returns the end-of-page flag for SHEET (value is 1 or 0)."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET `(SHEET-END-PAGE-FLAG ,SHEET) `(SHEET-END-PAGE-FLAG)))

;;; Sizes within margins
(DEFMACRO SHEET-INSIDE-LEFT (&OPTIONAL SHEET)
  "Return the width of SHEET's left margin."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET `(SHEET-LEFT-MARGIN-SIZE ,SHEET) 'LEFT-MARGIN-SIZE))

(DEFMACRO SHEET-INSIDE-TOP (&OPTIONAL SHEET)
  "Return the width of SHEET's top margin."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET `(SHEET-TOP-MARGIN-SIZE ,SHEET) 'TOP-MARGIN-SIZE))

(DEFMACRO SHEET-INSIDE-RIGHT (&OPTIONAL SHEET)
  "Return the position of SHEET's right margin.
The value is relative to the left edge of the sheet."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET
      `(- (SHEET-WIDTH ,SHEET) (SHEET-RIGHT-MARGIN-SIZE ,SHEET))
      `(- WIDTH RIGHT-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-BOTTOM (&OPTIONAL SHEET)
  "Return the position of SHEET's bottom margin.
The value is relative to the top edge of the sheet."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET
      `(- (SHEET-HEIGHT ,SHEET) (SHEET-BOTTOM-MARGIN-SIZE ,SHEET))
      `(- HEIGHT BOTTOM-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-WIDTH (&OPTIONAL SHEET)
  "Return the distance between SHEET's left and right margins."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET
      `(- (SHEET-WIDTH ,SHEET) (SHEET-LEFT-MARGIN-SIZE ,SHEET)
          (SHEET-RIGHT-MARGIN-SIZE ,SHEET))
      `(- WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-HEIGHT (&OPTIONAL SHEET)
  "Return the distance between SHEET's top and bottom margins."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET
      `(- (SHEET-HEIGHT ,SHEET) (SHEET-TOP-MARGIN-SIZE ,SHEET)
          (SHEET-BOTTOM-MARGIN-SIZE ,SHEET))
      `(- HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))

(DEFMACRO SHEET-TEMPORARY-P (&OPTIONAL SHEET)
  "T if SHEET is a temporary window.
A temporary window is one which saves the bits of whatever
is underneath it, when it is exposed."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  `(NOT (NULL ,(IF SHEET `(SHEET-TEMPORARY-BIT-ARRAY ,SHEET) 'TEMPORARY-BIT-ARRAY))))

(DEFMACRO SHEET-SUPERIOR-SCREEN-ARRAY (&OPTIONAL SHEET)
  "Return the screen array of SHEET's superior."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (LET ((SUPERIOR (IF SHEET `(SHEET-SUPERIOR ,SHEET) 'SUPERIOR)))
    `(OR (SHEET-SCREEN-ARRAY ,SUPERIOR) (SHEET-OLD-SCREEN-ARRAY ,SUPERIOR))))

(DEFMACRO SHEET-OUTPUT-HELD-P (&OPTIONAL SHEET)
  "T if SHEET's output is now being held."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  (IF SHEET
      `(OR (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG ,SHEET)))
           (CONSP (SHEET-LOCK ,SHEET)))
      `(OR (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG)))
           (CONSP LOCK))))

(DEFSUBST ARRAY-BITS-PER-PIXEL (ARRAY)
  "The number of bits per element of ARRAY, which must be a numeric array."
  (LSH 1 (1- (%P-LDB %%ARRAY-TYPE-FIELD ARRAY))))

(DEFSUBST SHEET-BITS-PER-PIXEL (&OPTIONAL (SHEET SELF))
  "The number of bits in each pixel of SHEET.
Only callable when the sheet can be output on -- BEWARE!"
  (ARRAY-BITS-PER-PIXEL (SHEET-SCREEN-ARRAY SHEET)))

;;; Added for Lambda VCMEM board, in which the number of locations per line
;;; does not correspond with the visible pixel width of the screen.  This determines
;;; the number of pixels in a line in the video memory, whether or not they will
;;; be visible.  It can depend only on locations per line and the *screen* instance
;;; of bits per pixel.
(DEFMACRO PIXELS-PER-PHYSICAL-LINE ()
  "Number of pixels worth of memory corresponding to one display line.
This can be more than the width of the display, if the first pixel
of one line does not immediately follow the last pixel of the previous line."
  '(TRUNCATE (* LOCATIONS-PER-LINE 32.) (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SELF))))

(DEFMACRO SHEET-LINE-NO (&OPTIONAL SHEET CURSOR-Y)
  "Return SHEET's cursor-y as a line number."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF) (CURSOR-Y (SHEET-CURSOR-Y SHEET))))
  `(FLOOR (- ,(COND (CURSOR-Y CURSOR-Y)
                    (SHEET `(SHEET-CURSOR-Y ,SHEET))
                    (T 'CURSOR-Y))
             (SHEET-INSIDE-TOP ,SHEET))
          ,(IF SHEET
               `(SHEET-LINE-HEIGHT ,SHEET)
             'LINE-HEIGHT)))

(DEFMACRO SHEET-NUMBER-OF-INSIDE-LINES (&OPTIONAL SHEET)
  "Return the number of lines that fit between SHEET's top and bottom margins."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  `(FLOOR (SHEET-INSIDE-HEIGHT ,SHEET)
          ,(IF SHEET `(SHEET-LINE-HEIGHT ,SHEET) 'LINE-HEIGHT)))

(DEFMACRO SHEET-TAB-WIDTH (&OPTIONAL SHEET)
  "Returns the number of char positions between tab stops on SHEET."
  (DECLARE (ARGLIST &OPTIONAL (SHEET SELF)))
  `(* (SHEET-TAB-NCHARS ,SHEET)
      ,(IF SHEET `(SHEET-CHAR-WIDTH ,SHEET) 'CHAR-WIDTH)))

;;;A blinker is an actor, described as follows:
(DEFFLAVOR BLINKER
        ((X-POS NIL)            ;X position of blinker (left) NIL if should follow sheet
         (Y-POS NIL)            ;Y position of blinker (top)
         SHEET                  ;Sheet associated with
         (VISIBILITY ':BLINK)   ;NIL invisible, T visible, :BLINK blinking, :ON visible but
                                ; blinking when selected, :OFF invisibile but ...
         (DESELECTED-VISIBILITY ':ON)   ;Blinker's visibility when the sheet it is on is
                                ; not selected, reasonable values :ON, :OFF and :BLINK
         (HALF-PERIOD 15.)      ;Time interval (60ths) between phase blinks
         (PHASE NIL)            ;NIL not visible, anything else visible in some form
                                ; (Complementing blinker has only two phases, uses NIL, T)
         (TIME-UNTIL-BLINK 0)   ;Time interval until next blink.  NIL means not blinking,
                                ; the clock level should ignore this blinker.
         (FOLLOW-P NIL)
         )
        ()
  :ORDERED-INSTANCE-VARIABLES :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:INITABLE-INSTANCE-VARIABLES X-POS Y-POS SHEET VISIBILITY FOLLOW-P)
  (:GETTABLE-INSTANCE-VARIABLES X-POS Y-POS SHEET VISIBILITY FOLLOW-P)
  (:SETTABLE-INSTANCE-VARIABLES DESELECTED-VISIBILITY HALF-PERIOD)
  (:REQUIRED-METHODS :BLINK :SIZE)
  (:SELECT-METHOD-ORDER :BLINK))

(DEFMACRO BLINKER-SET-CURSORPOS (BLINKER X Y)
  "Move BLINKER to position X, Y (relative to margins of BLINKER's sheet)."
  `(SEND ,BLINKER ':SET-CURSORPOS ,X ,Y))

(DEFMACRO BLINKER-READ-CURSORPOS (BLINKER)
  "Return BLINKER's X, Y position (relative to margins of BLINKER's sheet)."
  `(SEND ,BLINKER ':READ-CURSORPOS))

(DEFMACRO BLINKER-SET-VISIBILITY (BLINKER VISIBILITY)
  "Set visibility of BLINKER to VISIBLITY."
  `(SEND ,BLINKER ':SET-VISIBILITY ,VISIBILITY))

(DEFMACRO BLINK (BLINKER)
  `(SEND ,BLINKER ':BLINK))

(DEFMACRO BLINKER-SET-SIZE (BLINKER WIDTH HEIGHT)
  "Set size of BLINKER to WIDTH by HEIGHT."
  `(SEND ,BLINKER ':SET-SIZE ,WIDTH ,HEIGHT))

(DEFMACRO BLINKER-SET-CHARACTER (BLINKER FONT CHAR)
  "Set character BLINKER displays as to CHAR, in FONT.
Applies only to character blinkers."
  `(SEND ,BLINKER ':SET-CHARACTER ,CHAR ,FONT))

(DEFMACRO BLINKER-SET-SHEET (BLINKER SHEET)
  "Set the sheet BLINKER moves on to SHEET."
  `(SEND ,BLINKER ':SET-SHEET ,SHEET))

;;; This macro generates the prologue code that most of the
;;; interesting blinker methods need.
(DEFMACRO WITH-BLINKER-READY (DO-NOT-OPEN-P &BODY BODY)
  "Execute BODY after preparing to modify SELF, a blinker.
DO-NOT-OPEN-P non-NIL inhibits removing SELF from the screen,
in case the caller wishes to avoid doing so if it turns
out that no change needs to be made."
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     (DO () ((OR (NOT (SHEET-OUTPUT-HELD-P SHEET))
                 (NULL PHASE)))
       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
       (SEND SHEET ':OUTPUT-HOLD-EXCEPTION)
       (SETQ INHIBIT-SCHEDULING-FLAG T))
     ,@(IF (NOT DO-NOT-OPEN-P)
           `((OPEN-BLINKER SELF)))
     (MULTIPLE-VALUE-PROG1
       (PROGN . ,BODY)
       ,(OR DO-NOT-OPEN-P '(SETQ TIME-UNTIL-BLINK 0)))))

(DEFFLAVOR RECTANGULAR-BLINKER
        ((WIDTH NIL)            ;The width
         (HEIGHT NIL))
        (BLINKER)
  (:INITABLE-INSTANCE-VARIABLES WIDTH HEIGHT)
  (:DOCUMENTATION :COMBINATION
   "A blinker that displays as a solid rectangle."))

(DEFFLAVOR MOUSE-BLINKER-MIXIN ((X-OFFSET 0) (Y-OFFSET 0)) ()
  (:REQUIRED-FLAVORS BLINKER)
  :INITTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :MIXIN "Blinker that is capable of being MOUSE-BLINKER"))

(DEFFLAVOR MOUSE-BLINKER-FAST-TRACKING-MIXIN () (MOUSE-BLINKER-MIXIN)
  (:REQUIRED-FLAVORS BLINKER)
  (:DOCUMENTATION :MIXIN
     "Blinker that is capable of being MOUSE-BLINKER and gets tracked by microcode."))

;;;Who line variables
(DEFVAR WHO-LINE-WINDOW)        ;Sheet used for writing the who line
(DEFVAR WHO-LINE-RUN-STATE "")  ;Variable containing the current state (RUN, STOP, TYI, etc.)
;(DEFVAR WHO-LINE-RUN-LIGHT-LOC);Contains the address of the run light under the who line
                ;in COLD actually
(DEFVAR WHO-LINE-LIST)          ;List of WHO-LINE-ITEM's, see DEFSTRUCT below

(DEFSTRUCT (WHO-LINE-ITEM :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  "Each field of the who-line has one of these asscoiated with it.
There may be additional slots in the structure for the various who-line fields"
  (WHO-LINE-ITEM-FUNCTION NIL :DOCUMENTATION
    "Function to be called, see WHO-LINE-UPDATE")
  (WHO-LINE-ITEM-STATE NIL :DOCUMENTATION
    "Previous contents, to save time")
  (WHO-LINE-ITEM-LEFT NIL :DOCUMENTATION
    "Left-hand bit address")
  (WHO-LINE-ITEM-RIGHT NIL :DOCUMENTATION
    "Right-hand bit address")
  ;; More fields may exist, depending on the function
  )

(DEFVAR-RESETTABLE *WHO-LINE-RUN-STATE-OVERRIDE* NIL NIL
  "If non-NIL, is a string to be put in the run state of the who-line,
overriding the default process-state-based value computed by the window system.")

(DEFVAR-RESETTABLE *WHO-LINE-DOCUMENTATION-OVERRIDE* NIL NIL
  "If non-NIL, is a string to be put in the documentation line of the who-line screen,
overriding the default value computed by the window system.")

;(DEFMACRO WITH-WHO-LINE-RUN-STATE ((STRING) &BODY BODY)
;  "Executes BODY with the run state of the who-line set to STRING"
;  `(MULTIPLE-VALUE-PROG1
;     (LET ((*WHO-LINE-RUN-STATE-OVERRIDE* ,STRING))
;       (WHO-LINE-RUN-STATE-UPDATE)
;       . ,BODY)
;     (WHO-LINE-RUN-STATE-UPDATE)))

;;;; Fonts.

;;; A font array may not be displaced or anything hairy like that, because
;;; it is looked at directly by microcode.
;;; Its array leader contains:

(DEFSTRUCT (FONT :NAMED :ARRAY-LEADER (:SIZE-SYMBOL FONT-LEADER-SIZE) (:ALTERANT NIL))
  (FONT-FILL-POINTER 0 :DOCUMENTATION
   "1 plus highest character code defined in font.
At present, must be at least 200.")
  (FONT-NAME NIL :DOCUMENTATION
   "This should be to be the symbol in the FONTS package whose value is this font.")
  (FONT-CHAR-HEIGHT 0 :DOCUMENTATION
   "Character cell height")
  (FONT-CHAR-WIDTH 0 :DOCUMENTATION
   "Character cell width (used if char-width-table is NIL)")
  (FONT-RASTER-HEIGHT 0 :DOCUMENTATION
   "Raster height")
  (FONT-RASTER-WIDTH 0 :DOCUMENTATION
   "Raster width")
  (FONT-RASTERS-PER-WORD 0 :DOCUMENTATION
   "(FLOOR 32. RASTER-WIDTH) -- # rows per word")
  (FONT-WORDS-PER-CHAR 0 :DOCUMENTATION
   "(CEILING RASTER-HEIGHT RASTERS-PER-WORD) -- # words per char")
  (FONT-BASELINE 0 :DOCUMENTATION
   "# Raster lines down from top to align with other fonts")
  (FONT-CHAR-WIDTH-TABLE NIL :DOCUMENTATION
   "NIL or array pointer to character width table")
  (FONT-LEFT-KERN-TABLE NIL :DOCUMENTATION
    "NIL or array pointer to left kern table")
  (FONT-INDEXING-TABLE NIL :DOCUMENTATION
    "NIL or array pointer to index table.
This is used for characters whose raster is > 32 wide.
Use real char code to look up char code for raster purposes in the indexing table.
Draw several columns, until raster for next character is reached.
Index table length must be GREATER than FILL-POINTER so as to handle end condition right.")
  (FONT-NEXT-PLANE NIL :DOCUMENTATION
    "*COMPLETELY OBSOLETE*. NIL or font containing next higher plane of this font.")
  (FONT-BLINKER-WIDTH 0 :DOCUMENTATION
    "Default width for blinkers.")
  (FONT-BLINKER-HEIGHT 0 :DOCUMENTATION
    "Default height for blinkers.")
  (FONT-CHARS-EXIST-TABLE NIL :DOCUMENTATION
    "Array of bits saying which chars /"really exist/".
NIL means not in use, if which case all chars are assumed to exist."))

;;; The data part of the array contains an integral number of words
;;; per character.  Each word contains an integral number of rows
;;; of raster, right adjusted and processed from right to left.
;;; All 32 bits of each Q in this array are used.  For easiest processing
;;; by Lisp programs, it should be of 1-bit byte array type.

;;; %DRAW-CHAR only works for raster widths of at most 32.
;;; because that is the most that can be shifted without overlapping 3 TV buffer words.
;;; For larger widths it traps to ILLOP.  Wider characters are drawn
;;; by drawing several narrow characters side by side.  See the comment
;;; next to FONT-INDEXING-TABLE for how this is done.

(DEFSELECT ((:PROPERTY FONT NAMED-STRUCTURE-INVOKE))
  ((:PRINT-SELF) (SLF STREAM &REST IGNORE)
     (SYS:PRINTING-RANDOM-OBJECT (SLF STREAM :TYPE)
       (PRINC (FONT-NAME SLF) STREAM))))

;;; Grab the lock on a sheet
(DEFMACRO LOCK-SHEET ((SHEET) &BODY BODY)
  "Execute BODY with SHEET locked."
  `(UNWIND-PROTECT
     (PROGN (SHEET-GET-LOCK ,SHEET)
       . ,BODY)
     (SHEET-RELEASE-LOCK ,SHEET)))

;;; Allow output to a sheet
(DEFMACRO SHEET-IS-PREPARED ((SHEET) &BODY BODY)
  "Execute BODY, telling the microcode to treat SHEET as prepared.
Should only be used with XOR'ing, and with interrupts off."
  `(LET ((SYS:CURRENTLY-PREPARED-SHEET ,SHEET))
     . ,BODY))

;;; Open up a sheet
(DEFVAR PREPARED-SHEET NIL)
(DEFMACRO PREPARE-SHEET ((SHEET) &BODY BODY)
  "Execute BODY with SHEET prepared for output.
The microcode output primitives output to the prepared sheet."
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     (AND (OR (NEQ PREPARED-SHEET ,SHEET)
              (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG ,SHEET)))
              (NOT (SHEET-CAN-GET-LOCK ,SHEET)))
          (SHEET-PREPARE-SHEET-INTERNAL ,SHEET))
     (SETQ PREPARED-SHEET ,SHEET)
     (SHEET-IS-PREPARED (,SHEET)
       . ,BODY)))

;;; Redirects a screen array
(DEFMACRO REDIRECT-ARRAY (ARRAY TYPE WIDTH HEIGHT &REST ARGS)
  "Modify the indirect bit-array ARRAY as to what it indirects to.
The new array type is TYPE, the new width and height as specified.
The remaining args are passed along to SI:CHANGE-INDIRECT-ARRAY."
  `(WITHOUT-INTERRUPTS
     (SETQ SYS:%CURRENT-SHEET NIL)
     (SI:CHANGE-INDIRECT-ARRAY ,ARRAY ,TYPE
                               (IF ARRAY-INDEX-ORDER
                                   (LIST ,HEIGHT ,WIDTH) (LIST ,WIDTH ,HEIGHT))
                               . ,ARGS)))

;;; Force access to a sheet and execute the code within.  If access cannot be
;;; forced, then the code is not executed.  Forcing access means binding off
;;; the output hold flag if the sheet is deexposed and has a bit-save array.
;;; The code is also executed of the sheet is exposed and not output-held.
(DEFMACRO SHEET-FORCE-ACCESS ((SHEET IGNORE) &BODY BODY)
  "Execute BODY, outputting to SHEET whether SHEET is exposed or not.
This does so even if SHEET does not ordinarily allow output
while deexposed.  If sheet is deexposed and has no bit array,
then the body is not executed at all.  (But then SHEET
will refresh completely when it is exposed.)"
  `(LOCK-SHEET (,SHEET)
       ;; Sheet can't have temporary lock here as we own lock, so SHEET-OUTPUT-HELD-P not
       ;; required for proper operation
       (LET ((.OLD.OUTPUT.HOLD. (SHEET-OUTPUT-HOLD-FLAG ,SHEET)))
         (COND ((SHEET-SCREEN-ARRAY ,SHEET)
                (UNWIND-PROTECT
                  (PROGN (SETF (SHEET-OUTPUT-HOLD-FLAG ,SHEET) 0)
                         . ,BODY)
                  (SETF (SHEET-OUTPUT-HOLD-FLAG ,SHEET) .OLD.OUTPUT.HOLD.)))))))

;;; I/O buffer stuff
(DEFSTRUCT (IO-BUFFER :ARRAY-LEADER :NAMED (:CONSTRUCTOR NIL) (:ALTERANT NIL)
                      (:SIZE-SYMBOL IO-BUFFER-LEADER-SIZE))
  "Input-output buffer for the window system"
  (IO-BUFFER-FILL-POINTER NIL :DOCUMENTATION
    "Unused")
  (IO-BUFFER-SIZE NIL :DOCUMENTATION
    "Size of IO buffer (max index + 1)
All pointers are mod this.")
  (IO-BUFFER-INPUT-POINTER NIL :DOCUMENTATION
    "Index in which data is next stored")
  (IO-BUFFER-OUTPUT-POINTER NIL :DOCUMENTATION
    "Index from which data is next to be taken")
  ;; If OUTPUT-POINTER = INPUT-POINTER, then the buffer is empty.
  ;; If INPUT-POINTER + 1 = OUTPUT-POINTER, then the buffer is full (This wastes a location)
  ;; Actual pointer manipulation should be done with interrupts disabled.
  (IO-BUFFER-INPUT-FUNCTION NIL :DOCUMENTATION
    "If non-NIL, function to be run on inputing data")
  (IO-BUFFER-OUTPUT-FUNCTION NIL :DOCUMENTATION
    "If non-NIL, function to be run when taking data out")
  (IO-BUFFER-STATE NIL :DOCUMENTATION
    "NIL means ok to put data in
T means data may not be put in or taken out
:INPUT means data may only be put in
:OUTPUT means data may only be taken out")
  (IO-BUFFER-PLIST NIL :DOCUMENTATION
    "Property list used to hold various bits of information about the buffer
/(Eg what the asynchronous characters are)")
  (IO-BUFFER-LAST-INPUT-PROCESS NIL :DOCUMENTATION
    "The last process which did input to this io-buffer")
  (IO-BUFFER-LAST-OUTPUT-PROCESS NIL :DOCUMENTATION
    "The last process which did output from this io-buffer")
  (IO-BUFFER-RECORD NIL :DOCUMENTATION
    "Records history of recent input characters")
  )

(DEFCONST IO-BUFFER-RECORD-LENGTH 60.
  "Number of last input characters to record in each IO buffer.")

(DEFSUBST IO-BUFFER-RECORD-POINTER (INPUT-RECORD)
  "Index of last slot of INPUT-RECORD stored in."
  (ARRAY-LEADER INPUT-RECORD 1))

(DEFMACRO IO-BUFFER-EMPTY-P (BUFFER)
  "T if io-buffer BUFFER is empty."
  `(= (IO-BUFFER-INPUT-POINTER ,BUFFER)
      (IO-BUFFER-OUTPUT-POINTER ,BUFFER)))

(DEFMACRO IO-BUFFER-FULL-P (BUFFER)
  "T if io-buffer BUFFER is full (no more can be put in it)."
  ;; Always leave room for at one unget to be done
  `(= (\ (+ (IO-BUFFER-INPUT-POINTER ,BUFFER) 2) (IO-BUFFER-SIZE ,BUFFER))
      (IO-BUFFER-OUTPUT-POINTER ,BUFFER)))

(DEFMACRO WITH-SHEET-DEEXPOSED ((SHEET) &BODY BODY)
  "Execute BODY with SHEET deexposed.
SHEET will be reexposed afterward if it was exposed to begin with.
Many kinds of changes to SHEET are simpler to make
if you know that SHEET is not exposed."
  `(LET ((.STATUS. (SEND ,SHEET ':STATUS)))
     (DELAYING-SCREEN-MANAGEMENT
      (UNWIND-PROTECT
        (PROGN (SEND ,SHEET ':DEEXPOSE ':DEFAULT ':NOOP)
               . ,BODY)
        (SEND ,SHEET ':SET-STATUS .STATUS.)))))

;(DEFMACRO WINDOW-BIND ((WINDOW NEW-TYPE . INIT-PAIRS) &BODY BODY)
;  "Change the flavor of WINDOW to NEW-TYPE over execution of BODY.
;INIT-PAIRS are extra arguments to pass to MAKE-WINDOW."
;  (CHECK-ARG WINDOW SYMBOLP "a symbol which is set to a window")
;    `(LET ((.O.WINDOW. ,WINDOW) (.N.WINDOW.) (,WINDOW ,WINDOW) (TERMINAL-IO TERMINAL-IO))
;       (UNWIND-PROTECT
;        (PROGN (SETQ .N.WINDOW. (WINDOW-PUSH ,WINDOW ,NEW-TYPE . ,INIT-PAIRS))
;               (SETQ ,WINDOW .N.WINDOW.)
;               (AND (EQ .O.WINDOW. TERMINAL-IO) (SETQ TERMINAL-IO ,WINDOW))
;          . ,BODY)
;        (AND .N.WINDOW. (WINDOW-POP .O.WINDOW. .N.WINDOW.))))))


(DEFMACRO PRESERVE-SUBSTITUTE-STATUS (WINDOW &BODY BODY)
  "Execute BODY, then select WINDOW if it (or its substitute) used to be selected."
  `(LET* ((.WINDOW. ,WINDOW)
          (.STATUS. (SEND .WINDOW. ':SELF-OR-SUBSTITUTE-SELECTED-P)))
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (IF .STATUS. (SEND .WINDOW. ':SELECT)))))

(DEFMACRO WITH-SELECTION-SUBSTITUTE ((WINDOW FOR-WINDOW) &BODY BODY)
  "Execute BODY with WINDOW as a selection substitute for FOR-WINDOW.
While FOR-WINDOW has the selection substitute, the substitute
will be selected whenever FOR-WINDOW would have been selected."
  `(LET* ((.WINDOW. ,WINDOW)
          (.FOR-WINDOW. ,FOR-WINDOW)
          (.OSTATUS. (and .window. (SEND .WINDOW. ':STATUS)))
          (.OSUBST. (SEND .FOR-WINDOW. ':SELECTION-SUBSTITUTE)))
     (SEND .FOR-WINDOW. ':SET-SELECTION-SUBSTITUTE .WINDOW.)
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (DELAYING-SCREEN-MANAGEMENT
         (SEND .FOR-WINDOW. ':SET-SELECTION-SUBSTITUTE .OSUBST.)
         (IF .WINDOW. (SEND .WINDOW. ':SET-STATUS .OSTATUS.))))))

;;; Temporarily select a window
(DEFMACRO WINDOW-CALL ((WINDOW FINAL-ACTION . FINAL-ACTION-ARGS) &BODY BODY)
  "Select WINDOW then execute BODY.  Afterward, send a FINAL-ACTION message.
After BODY, WINDOW is sent the FINAL-ACTION operation
with FINAL-ACTION-ARGS as args.
:DEACTIVATE is frequently used for FINAL-ACTION."
  `(LET ((.CURRENT-WINDOW. SELECTED-WINDOW))
     (UNWIND-PROTECT
       (PROGN
         (let ((sup (send ,window :superior)))
           ;avoid possible exposing-outside-of-superior error.
           (cond ((and sup
                       (not (sheet-within-sheet-p ,window sup)))
                  (send ,window :change-of-size-or-margins
                        :left 0
                        :top 0
                        :width (send sup :inside-width)
                       :height (send sup :inside-height)))))
         (SEND ,WINDOW ':SELECT)
         . ,BODY)
       ;; Reselect old window -- be careful not to reselect if we aren't still the currently
       ;; selected window, thus preventing spurious selection
       ,(IF FINAL-ACTION
            `(DELAYING-SCREEN-MANAGEMENT
               (LET ((.FLAG. (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW ,WINDOW)))
                 (SEND ,WINDOW ',FINAL-ACTION . ,FINAL-ACTION-ARGS)
                 (AND .CURRENT-WINDOW. .FLAG. (SEND .CURRENT-WINDOW. ':SELECT NIL))))
            `(AND .CURRENT-WINDOW. (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW ,WINDOW)
                  (SEND .CURRENT-WINDOW. ':SELECT NIL))))))

(DEFMACRO WINDOW-MOUSE-CALL ((WINDOW FINAL-ACTION . FINAL-ACTION-ARGS) &BODY BODY)
  "Mouse-select WINDOW then execute BODY.  Afterward, send a FINAL-ACTION message.
This is different from WINDOW-CALL in that it selects WINDOW
usingthe :MOUSE-SELECT operation, appropriate if this is being
done in response to some sort of mouse command.
After BODY, WINDOW is sent the FINAL-ACTION operation
with FINAL-ACTION-ARGS as args.
:DEACTIVATE is frequently used for FINAL-ACTION."
  `(LET ((.CURRENT-WINDOW. SELECTED-WINDOW))
     (UNWIND-PROTECT
       (PROGN
         (let ((sup (send ,window :superior)))
           ;avoid possible exposing-outside-of-superior error.
           (cond ((and sup
                       (not (sheet-within-sheet-p ,window sup)))
                  (send ,window :change-of-size-or-margins
                        :left 0
                        :top 0
                        :width (send sup :inside-width)
                       :height (send sup :inside-height)))))
         (SEND ,WINDOW ':MOUSE-SELECT)
         . ,BODY)
       ,(IF FINAL-ACTION
            `(DELAYING-SCREEN-MANAGEMENT
               (SEND ,WINDOW ',FINAL-ACTION . ,FINAL-ACTION-ARGS)
               (AND .CURRENT-WINDOW. (SEND .CURRENT-WINDOW. ':SELECT NIL)))
            `(AND .CURRENT-WINDOW. (SEND .CURRENT-WINDOW. ':SELECT NIL))))))

;;; Maybe this should go somewhere else
(DEFMACRO DOPLIST ((PLIST PROP IND) &BODY BODY)
  `(DO ((PLIST ,PLIST (CDDR PLIST))
        (,PROP)
        (,IND))
       ((NULL PLIST))
     (SETQ ,IND (CAR PLIST)
           ,PROP (CADR PLIST))
     . ,BODY))

;;; There are certain kinds of windows that are associated with screens.  These include
;;; the system menu, and associated windows.  This a facility for defining those
;;; kinds of windows, and allocating them automatically.

(DEFMACRO DEFWINDOW-RESOURCE (NAME PARAMETERS &REST OPTIONS
                              &AUX DOC
                                   (CONSTRUCTOR NIL)
                                   (INITIAL-COPIES 1)
                                   (CHECKER 'CHECK-UNLOCKED-WINDOW-RESOURCE)
                                   resource-options)
  "Define a resource (a la DEFRESOURCE) of windows of a given flavor.
The resource is defined with the specified parameters and one additional
one, which is the desired superior.  When you allocate a window from
this resource, you can specify the superior as well as the other
parameters, or you can let it default to MOUSE-SHEET.
Options are
:INITIAL-COPIES -- number of windows to create initially.
 Default is one, which is made an inferior of DEFAULT-SCREEN.
:CONSTRUCTOR -- as with DEFRESOURCE.
:MAKE-WINDOW -- value is list of flavor name followed by keyword args.
 The default constructor passes this list to MAKE-WINDOW,
 evaluating the elements of it.
:REUSABLE-WHEN (one of :DEEXPOSED or :DEACTIVATED).  The default is to be
 reusable when nobody is using it and it is not locked.
 If you specify :DEEXPOSED or :DEACTIVATED, it means that
 a window that is not exposed or not active can automatically be reused.
Other options supplied will be passed to DEFRESOURCE."
  (IF (STRINGP (CAR OPTIONS)) (SETQ DOC (POP OPTIONS)))
  (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
        DO (SELECTQ KEYWORD
             (:INITIAL-COPIES (SETQ INITIAL-COPIES VALUE))
             (:CONSTRUCTOR (SETQ CONSTRUCTOR VALUE))
             ((:MAKE-WINDOW :WINDOW-CREATE)     ;:WINDOW-CREATE obsolete old name
                (SETQ CONSTRUCTOR `(MAKE-WINDOW ',(CAR VALUE)
                                     ':SUPERIOR SUPERIOR
                                     ,@(LOOP FOR (KEYWORD VALUE) ON (CDR VALUE) BY 'CDDR
                                             COLLECT `',KEYWORD
                                             COLLECT VALUE))))
             (:REUSABLE-WHEN
                (SETQ CHECKER (SELECTQ VALUE
                                (:DEEXPOSED 'CHECK-DEEXPOSED-WINDOW-RESOURCE)
                                (:DEACTIVATED 'CHECK-DEACTIVATED-WINDOW-RESOURCE)
                                (OTHERWISE (FERROR NIL "~S ~S - only :DEEXPOSED and ~
                                                       :DEACTIVATED are allowed"
                                                   KEYWORD VALUE)))))
             (t (unless (memq keyword '(:checker))
                  (setq resource-options (nconc resource-options (list keyword value)))))))
  (OR CONSTRUCTOR (FERROR NIL "DEFWINDOW-RESOURCE requires either the :CONSTRUCTOR or~@
                               the :MAKE-WINDOW option."))
  (LET ((STUFF `(:INITIAL-COPIES ,INITIAL-COPIES
                 :CONSTRUCTOR ,CONSTRUCTOR
                 :CHECKER ,CHECKER
                 ,@resource-options)))
    (IF DOC (PUSH DOC STUFF))
    `(PROGN
       (PUSHNEW ',NAME WINDOW-RESOURCE-NAMES :TEST 'EQ)
       (DEFRESOURCE ,NAME ,(APPEND PARAMETERS
                                   (IF (MEMQ '&OPTIONAL PARAMETERS)
                                       '((SUPERIOR MOUSE-SHEET))
                                     '(&OPTIONAL (SUPERIOR MOUSE-SHEET))))
         . ,STUFF))))

;;; This gets a list of all window resources
;;; so that :CHANGE-OF-DEFAULT-FONT can find the windows even when not active.
(DEFVAR WINDOW-RESOURCE-NAMES NIL
  "List of names of all DEFWINDOW-RESOURCEs.")
;;; This name was used by mistake in the window manual.
(FORWARD-VALUE-CELL 'WINDOW-RESOURCE-LIST 'WINDOW-RESOURCE-NAMES)
(DEFVAR WINDOW-RESOURCE-LIST :UNBOUND
  "List of names of all DEFWINDOW-RESOURCEs.")

(DEFUN CHECK-UNLOCKED-WINDOW-RESOURCE (IGNORE WINDOW IN-USE-P &REST IGNORE)
  (AND (NOT IN-USE-P)
       (SHEET-CAN-GET-LOCK WINDOW)))

(DEFUN CHECK-DEEXPOSED-WINDOW-RESOURCE (IGNORE WINDOW IGNORE &REST IGNORE)
  (AND (NOT (SHEET-EXPOSED-P WINDOW))
       (SHEET-CAN-GET-LOCK WINDOW)))

(DEFUN CHECK-DEACTIVATED-WINDOW-RESOURCE (IGNORE WINDOW IGNORE &REST IGNORE)
  (AND (NOT (MEMQ WINDOW (SHEET-INFERIORS (SHEET-SUPERIOR WINDOW))))
       (SHEET-CAN-GET-LOCK WINDOW)))

;;;; Defintions for screen management

(DEFMACRO RECT-SOURCE (R) `(FIRST ,R))
(DEFMACRO RECT-LEFT (R) `(SECOND ,R))
(DEFMACRO RECT-TOP (R) `(THIRD ,R))
(DEFMACRO RECT-RIGHT (R) `(FOURTH ,R))
(DEFMACRO RECT-BOTTOM (R) `(FIFTH ,R))
(DEFMACRO RECT-WITHIN-RECT-P (R1 R2)
  "R1 within R2"
  `(AND ( (RECT-LEFT ,R1) (RECT-LEFT ,R2))
        ( (RECT-RIGHT ,R1) (RECT-RIGHT ,R2))
        ( (RECT-TOP ,R1) (RECT-TOP ,R2))
        ( (RECT-BOTTOM ,R1) (RECT-BOTTOM ,R2))))

(DEFMACRO RECT-NOT-OVERLAP-RECT-P (R1 R2)
  `(OR ( (RECT-RIGHT ,R2) (RECT-LEFT ,R1))
       ( (RECT-RIGHT ,R1) (RECT-LEFT ,R2))
       ( (RECT-BOTTOM ,R2) (RECT-TOP ,R1))
       ( (RECT-BOTTOM ,R1) (RECT-TOP ,R2))))

(DEFVAR SCREEN-MANAGER-QUEUE NIL
  "List of operations to be performed eventually by the screen manager.
Each operation involves filling a screen rectangle from some sheet.
See the file SYS:WINDOW;SCRMAN.")
(DEFVAR SCREEN-MANAGER-TOP-LEVEL T)

(DEFMACRO DELAYING-SCREEN-MANAGEMENT (&BODY BODY)
  "Collect any screen manages that get queued during its body,
and force them to happen later.  This code is unwind-
protected so that all pending manages get done, as they are
necessary to have the screen look correct.  The code tries to
remove duplicate screen manages when it finally does them, and
after it finishes all the managing does an autoexpose on all
superiors that it hacked."
  `(LET ((.QUEUE-LEFT. T))
     (UNWIND-PROTECT
       (LET-IF SCREEN-MANAGER-TOP-LEVEL ((SCREEN-MANAGER-QUEUE NIL))
               (UNWIND-PROTECT
                 (LET ((INHIBIT-SCREEN-MANAGEMENT T)
                       (SCREEN-MANAGER-TOP-LEVEL NIL))
                   (PROGN . ,BODY))
                 (SCREEN-MANAGE-DELAYING-SCREEN-MANAGEMENT-INTERNAL)
                 (SETQ .QUEUE-LEFT. SCREEN-MANAGER-QUEUE)))
       (AND (NEQ .QUEUE-LEFT. T)
            SCREEN-MANAGER-TOP-LEVEL
            (DOLIST (E .QUEUE-LEFT.)
              (APPLY #'SCREEN-MANAGE-QUEUE (FIRST (FIRST E)) (CDR E)))))))

(DEFMACRO WITHOUT-SCREEN-MANAGEMENT (&BODY BODY)
  "This causes any screen manages that get queued during its
body to get flushed if the body exits normally.  Abnormal exit
will cause the screen manages to remain on the queue so that they
do get done.  This is useful in circumstances when you know
you'll be doing screen management on the same stuff right away."
  `(LET ((.FLAG. NIL))
     (UNWIND-PROTECT
       (LET ((SCREEN-MANAGER-QUEUE NIL)
             (SCREEN-MANAGER-TOP-LEVEL NIL))
         (UNWIND-PROTECT
           (LET ((INHIBIT-SCREEN-MANAGEMENT T))
             (MULTIPLE-VALUE-PROG1
               ,@BODY
               ;; Body completed successfully, flush any screen manages that got queued
               (SETQ SCREEN-MANAGER-QUEUE NIL)))
           (SETQ .FLAG. SCREEN-MANAGER-QUEUE)))
       (DOLIST (E .FLAG.)
         ;; Requeue entries
         (APPLY #'SCREEN-MANAGE-QUEUE (FIRST (FIRST E)) (CDR E))))))

;;;; Macros to help out the squeaking furry things

(DEFMACRO WITH-MOUSE-GRABBED (&BODY BODY)
  "Tell the mouse process to just track the mouse while BODY is executing.
The mouse process no longer sends windows messages about the mouse.
It does update TV:MOUSE-X and TV:MOUSE-Y.
BODY can wait for changes in the tracked mouse position with TV:MOUSE-WAIT.
BODY must set the mouse blinker right away, for example by (MOUSE-STANDARD-BLINKER)."
  `(LET ((.OLD.VALUE. WINDOW-OWNING-MOUSE))
     (LET-GLOBALLY ((WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL))
       (UNWIND-PROTECT
         (PROGN
           (WITH-MOUSE-GRABBED-INTERNAL T)
           . ,BODY)
         (SETQ WINDOW-OWNING-MOUSE .OLD.VALUE.)
         (SETQ MOUSE-RECONSIDER T)))))

(DEFMACRO WITH-MOUSE-GRABBED-ON-SHEET ((SHEET) &BODY BODY)
  "Like TV:WITH-MOUSE-GRABBED, but additionally restricts the mouse to moving within SHEET."
  (IF (NULL SHEET) (SETQ SHEET 'SELF))
  `(LET ((.OLD.VALUE. MOUSE-SHEET))
     (UNWIND-PROTECT
       (WITH-MOUSE-GRABBED
         (MOUSE-SET-SHEET ,SHEET)
         . ,BODY)
       (MOUSE-SET-SHEET .OLD.VALUE.))))

(DEFMACRO WITH-MOUSE-USURPED (&BODY BODY)
  "Tell mouse process to do absolutely nothing while BODY is executed.
BODY can track the mouse using TV:MOUSE-INPUT."
  `(LET ((.OLD.VALUE. WINDOW-OWNING-MOUSE))
     (LET-GLOBALLY ((WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL))
       (UNWIND-PROTECT
         (PROGN
           (WITH-MOUSE-GRABBED-INTERNAL 'STOP)
           . ,BODY)
         (SETQ WINDOW-OWNING-MOUSE .OLD.VALUE.)
         (SETQ MOUSE-RECONSIDER T)))))

;;; Tell the mouse process to switch "modes" and wait for it to do so
(DEFUN WITH-MOUSE-GRABBED-INTERNAL (WOM &AUX (INHIBIT-SCHEDULING-FLAG T))
  (SETQ WINDOW-OWNING-MOUSE WOM)
  (WHEN (NEQ WOM MOUSE-WINDOW)
    (SETQ MOUSE-RECONSIDER T
          INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Grab Mouse" #'(LAMBDA (WOM) (AND (NULL MOUSE-RECONSIDER)
                                                    (EQ MOUSE-WINDOW WOM)))
                  WOM)))

;;; Server structure used by WHOLIN and PEEK
(DEFSTRUCT (SERVER-DESC :CONC-NAME (:TYPE :LIST) (:ALTERANT NIL))
  CONNECTION HOST-NAME CONTACT-NAME PROCESS FUNCTION ARGS)


;;; used to be defined in window; stream
;;; The third, fourth and fifth components of this structure are used only by WINDOW; RH
;;; and not by the default rubout handler.
(DEFSTRUCT (RUBOUT-HANDLER-BUFFER (:TYPE :ARRAY-LEADER)
                                  (:MAKE-ARRAY (:LENGTH #o1000))
                                  (:DEFAULT-POINTER RUBOUT-HANDLER-BUFFER)
                                  (:CONC-NAME "RHB-")
                                  (:ALTERANT NIL))
  (FILL-POINTER 0 :documentation
    "Furthest index in buffer of any input")
  (SCAN-POINTER 0 :documentation
    "Furthest index in buffer which has been scanned by the reading function")
  (TYPEIN-POINTER 0 :documentation
    "Index of where the next input character goes in the buffer")
  (DONT-SAVE-FLAG NIL :documentation
    "T means this input should not be saved on the input history")
  (INPUT-HISTORY NIL :documentation
    "History of past input to this rubout-handler")
  (STATUS NIL :documentation
    "Possible values include NIL, :RESTORED, :INITIAL-ENTRY, :RUBOUT")
  (PLIST NIL))

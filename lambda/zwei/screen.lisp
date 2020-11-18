;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains the interface between the editor and the screen system.
;;; A ZWEI-window is the editor's data structure.  A window-sheet is the screen
;;; system's active data structure.

;;; Most of the functions in this file do not run in the regular editor
;;; environment and may not use the editor global variables.  The sheet
;;; methods are only allowed to touch the "wrapper" of a window: the outline,
;;; the label, and the blinkers.  Any operation involving the window per se
;;; or other editor data structures is done by sending a request to the
;;; editor process that owns the window.  A request is a "character" in
;;; the input stream which is actually a list.  There is a slight exception
;;; to this in changing the edges of a window, the ZWEI-window data-structure is
;;; altered immediately.

;;; The following are the requests to the editor process.  Note that if these
;;; are seen by other than the command-loop, some should be ignored and some should
;;; be executed anyway.
;;; (REDISPLAY) - wakes up the editor process so that it will do a redisplay.
;;;     The degree is not passed in this request, but stored directly into the
;;;     window, so that if the editor does a redisplay on its own before processing
;;;     the request the right things will happen.  If the window system tells the window
;;;     to redisplay with DIS-ALL, we want to make sure the window knows it is munged
;;;     if the editor displays on it, and we want to avoid redisplaying twice.
;;; (SELECT-WINDOW window) - causes Zmacs to select this window as the current window.
;;;     This is used to reflect window-selection by mouse to the editor, mainly
;;;     for the case of one editor that knows about more than one window.
;;; (SCROLL window line-number) - put line-number line of the buffer at the top of the window.
;;;     The argument is not guaranteed to be in range.
;;;     This does not deal in continuation lines.
;;; (:MOUSE-BUTTON mouse-char window window-relative-X window-relative-Y) - mouse command.

(DEFFLAVOR MENU-COMMAND-MENU-MIXIN () ()
  (:REQUIRED-FLAVORS TV:BASIC-MENU))

(DEFMETHOD (MENU-COMMAND-MENU-MIXIN :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND (VARIABLE-BOUNDP TV:CURRENT-ITEM)
       (OR (GET (CDR TV:CURRENT-ITEM) ':WHO-LINE-DOCUMENTATION)
           (GET (CDR TV:CURRENT-ITEM) 'DOCUMENTATION)
           (GET (CDR TV:CURRENT-ITEM) :DOCUMENTATION))))

(DEFFLAVOR MENU-COMMAND-MOMENTARY-MENU () (MENU-COMMAND-MENU-MIXIN TV:MOMENTARY-MENU))

;;; Menu stuff
;;; Return a command that does a pop-up menu from the given command-list
;;; We actually make a menu rather than calling TV:MENU-CHOOSE to provide faster
;;; pop up and remembering of the last-selected item in that menu (per screen).

;;; This is the one resource for all of these menus; the item-list parameter
;;; is used to distinguish among them.
(DEFWINDOW-RESOURCE MENU-COMMAND-MENU (ITEM-LIST)
        :MAKE-WINDOW (MENU-COMMAND-MOMENTARY-MENU :ITEM-LIST ITEM-LIST
                                                  :SAVE-BITS ':DELAYED)
        :INITIAL-COPIES 0
        :REUSABLE-WHEN :DEACTIVATED)

(DEFUN MAKE-MENU-COMMAND (COMMAND-LIST &OPTIONAL LABEL-COMPUTING-FUNCTION)
  (LET-CLOSED ((ITEM-LIST (MAPCAR 'MAKE-MENU-COMMAND-1 (MAKE-COMMAND-ALIST COMMAND-LIST)))
               (*LABEL-COMPUTING-FUNCTION* LABEL-COMPUTING-FUNCTION))
    'MAKE-MENU-COMMAND-DRIVER))

(DEFUN MAKE-MENU-COMMAND-FROM-ALIST (COMMAND-ALIST &OPTIONAL LABEL-COMPUTING-FUNCTION)
  (LET-CLOSED ((ITEM-LIST COMMAND-ALIST)
               (*LABEL-COMPUTING-FUNCTION* LABEL-COMPUTING-FUNCTION))
    'MAKE-MENU-COMMAND-DRIVER))

(DEFUN MAKE-MENU-COMMAND-1 (ALIST-ELT)
  (IF (STRING-EQUAL (CAR ALIST-ELT) "Mouse " :START1 0 :START2 0 :END1 6 :END2 6)
      (CONS (SUBSTRING (CAR ALIST-ELT) 6) (CDR ALIST-ELT))
    ALIST-ELT))

(DEFUN MAKE-MENU-COMMAND-DRIVER (&AUX COMMAND)
  (LOCAL-DECLARE ((SPECIAL ITEM-LIST *LABEL-COMPUTING-FUNCTION*))
    (USING-RESOURCE (MENU MENU-COMMAND-MENU ITEM-LIST)
      (AND *LABEL-COMPUTING-FUNCTION*
           (SEND MENU ':SET-LABEL (FUNCALL *LABEL-COMPUTING-FUNCTION*)))
      (SETQ COMMAND (SEND MENU ':CHOOSE)))
    (IF COMMAND (FUNCALL COMMAND) DIS-NONE)))

(DEFUN MENU-COMMAND-P (X)
  (AND (CLOSUREP X)
       (EQ (CLOSURE-FUNCTION X) 'MAKE-MENU-COMMAND-DRIVER)))

(DEFUN GET-MENU-COMMAND-COMMANDS (X)
  (SYMEVAL-IN-CLOSURE X 'ITEM-LIST))

;;; The window-sheet definitions
(DEFFLAVOR ZWEI
           (;;T means this window wants to be exposed and selected, but is waiting until
            ;;typeahead has been absorbed before popping up.
            (DELAYED-SELECT-PENDING NIL)
            ;;T if should scroll to handle wraparound. Used by editor top level, etc.
            (GLITCH-AT-END-OF-PAGE NIL)
            (MOUSE-DOCUMENTATION-STRING (MAKE-STRING 100. ':FILL-POINTER 0)))
           (WINDOW)
  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW TV:ESSENTIAL-MOUSE
                     TV:STREAM-MIXIN TV:LABEL-MIXIN TV:BASIC-SCROLL-BAR)
  (:GETTABLE-INSTANCE-VARIABLES MOUSE-DOCUMENTATION-STRING)
  (:SETTABLE-INSTANCE-VARIABLES GLITCH-AT-END-OF-PAGE)
  (:DEFAULT-INIT-PLIST :SAVE-BITS ':DELAYED :RIGHT-MARGIN-CHARACTER-FLAG 1
                       :BACKSPACE-NOT-OVERPRINTING-FLAG 1
                       :MINIMUM-WIDTH 32. :MINIMUM-HEIGHT 32.)  ;for creation with mouse
  (:DOCUMENTATION "Editor windows"))

(DEFMETHOD (ZWEI :BEFORE :EDIT) (&REST IGNORE)
  (COMTAB-MOUSE-PROMPT *COMTAB* MOUSE-DOCUMENTATION-STRING))

(DEFMETHOD (ZWEI :AFTER :EXPOSE) (&REST IGNORE)
  (AND (SEND SELF ':TOP-LEVEL-P)
       (WHEN (BOUNDP-IN-CLOSURE EDITOR-CLOSURE '*COMTAB*)
         (COMTAB-MOUSE-PROMPT (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*COMTAB*)
                              MOUSE-DOCUMENTATION-STRING)))
  (SETQ DELAYED-SELECT-PENDING NIL))            ;We have appeared now

(DEFMETHOD (ZWEI :START-DELAYED-SELECT) ()
  (SETQ DELAYED-SELECT-PENDING (NOT TV:EXPOSED-P)))

(DEFMETHOD (ZWEI :FLUSH-DELAYED-SELECT) ()
  (SETQ DELAYED-SELECT-PENDING NIL))

(DEFMETHOD (ZWEI :FINISH-DELAYED-SELECT) ()
  (AND DELAYED-SELECT-PENDING (NOT TV:EXPOSED-P)
       (SELECT-WINDOW SELF))
  NIL)

(DEFMETHOD (ZWEI :CHANGE-LABEL) (NEW-LABEL)
  (SEND SELF ':DELAYED-SET-LABEL NEW-LABEL))

(DEFMETHOD (ZWEI :READY-FOR-REDISPLAY-P) (&OPTIONAL IGNORE)
  (ZEROP (TV:SHEET-OUTPUT-HOLD-FLAG)))

;;; This is for BIND-MOUSE-DOCUMENTATION-STRING
(DEFMETHOD (ZWEI :MOUSE-DOCUMENTATION-STRING-LOCATION) ()
  (LOCF MOUSE-DOCUMENTATION-STRING))

(DEFMETHOD (ZWEI :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
      MOUSE-DOCUMENTATION-STRING))

(DEFMETHOD (ZWEI :PRINT-NOTIFICATION) (TIME STRING WINDOW-OF-INTEREST)
  (WINDOW-TYPEIN-NOTIFICATION SELF TIME STRING WINDOW-OF-INTEREST))

(DEFUN WINDOW-TYPEIN-NOTIFICATION (WINDOW TIME STRING &OPTIONAL WINDOW-OF-INTEREST)
  (SEND (SEND (SEND WINDOW ':MODE-LINE-WINDOW) ':TYPEIN-WINDOW)
        ':PRINT-NOTIFICATION TIME STRING WINDOW-OF-INTEREST))

;;; This makes the redisplay mechanism forget everything it knows.
(DEFMETHOD (ZWEI :AFTER :REFRESH) (&OPTIONAL TYPE)
  (WHEN (OR (NOT TV:RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
    (WHEN TV:EXPOSED-P
      (PREPARE-WINDOW-FOR-REDISPLAY SELF))  ;Pop down any typeout window.
    (TELL-EDITOR-TO-REDISPLAY DIS-ALL)))

(DEFMETHOD (ZWEI :AFTER :SELECT) (&OPTIONAL IGNORE)
  (OR (MEMQ (TV:BLINKER-VISIBILITY POINT-BLINKER) '(:BLINK :ON))
      ;Make sure the blinkers are correct
      (SETQ REDISPLAY-DEGREE (MAX REDISPLAY-DEGREE DIS-BPS))))

;;; If the window has changed completely, don't bring back any bits
(DEFWRAPPER (ZWEI :EXPOSE) (IGNORE . BODY)
  `(PROGN
     (COND (( REDISPLAY-DEGREE DIS-ALL)
            (AND (< (LENGTH SI:.DAEMON-CALLER-ARGS.) 3)
                 (SETQ SI:.DAEMON-CALLER-ARGS. (LIST (CAR SI:.DAEMON-CALLER-ARGS.)
                                                     (CADR SI:.DAEMON-CALLER-ARGS.)
                                                     NIL)))
            (AND (NEQ (CADDR SI:.DAEMON-CALLER-ARGS.) ':NOOP)
                 (SETF (CADDR SI:.DAEMON-CALLER-ARGS.) ':CLEAN))))
     . ,BODY))

(DEFMETHOD (ZWEI :VERIFY-NEW-EDGES) (NEW-LEFT NEW-TOP NEW-WIDTH NEW-HEIGHT)
  NEW-LEFT NEW-TOP NEW-WIDTH
  (AND (< (- NEW-HEIGHT TV:TOP-MARGIN-SIZE TV:BOTTOM-MARGIN-SIZE)
          TV:LINE-HEIGHT)
       "Zero line editor windows do not work"))

;;; Get a handle on the text in the window
(DEFMETHOD (ZWEI :STREAM) ()
  (MAKE-EDITOR-STREAM-FROM-WINDOW SELF))

;;;; Window operations that can change the number of plines displayed.

;;; If a default font has changed, and it is a font we use, redisplay everything.
(DEFMETHOD (ZWEI :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD NEW)
  OLD
  (LET ((LEN (ARRAY-LENGTH TV:FONT-MAP)))
    (DOTIMES (I LEN)
      (IF (EQ NEW (AREF TV:FONT-MAP I))
          (RETURN (ZWEI-INSIDE-CHANGED))))))

(DEFMETHOD (ZWEI :AFTER :CHANGE-OF-SIZE-OR-MARGINS) ZWEI-INSIDE-CHANGED)

(DEFMETHOD (ZWEI :AFTER :SET-FONT-MAP) ZWEI-INSIDE-CHANGED)

(DEFMETHOD (ZWEI :AFTER :SET-VSP) ZWEI-INSIDE-CHANGED)

(DEFUN ZWEI-INSIDE-CHANGED (&REST IGNORE &AUX INSIDE-HEIGHT)
  (DECLARE (:SELF-FLAVOR ZWEI))
  (SETQ INSIDE-HEIGHT (TV:SHEET-INSIDE-HEIGHT))
  (WHEN (VARIABLE-BOUNDP N-PLINES)
    (LET ((NEW-N-PLINES (TRUNCATE INSIDE-HEIGHT TV:LINE-HEIGHT)))
      ;; If number of screen lines has changed, we must change the window array.
      (WHEN ( NEW-N-PLINES N-PLINES)
        (UNLESS ( (ARRAY-LENGTH PLINE-LINE-ARRAY) NEW-N-PLINES)
          (LET ((NEW-LENGTHS (MAX NEW-N-PLINES (FIX (* 1.5s0 N-PLINES)))))
            (ADJUST-ARRAY-SIZE PLINE-LINE-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-FROM-INDEX-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-TO-INDEX-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-TICK-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-MARKING-LEFT-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-MARKING-WIDTH-ARRAY NEW-LENGTHS)
            (ADJUST-ARRAY-SIZE PLINE-TEXT-WIDTH-ARRAY NEW-LENGTHS)))
        (LET ((LEN (ARRAY-LENGTH PLINE-LINE-ARRAY)))
          ;; Too big, forget old information
          (DO ((I NEW-N-PLINES (1+ I)))
              (( I LEN))
            (SETF (PLINE-LINE SELF I) NIL)))
        (SETQ N-PLINES NEW-N-PLINES))
      ;; Clear anything between the last line and the margins.
      (LET ((LINES-HEIGHT (* TV:LINE-HEIGHT N-PLINES)))
        (OR (= LINES-HEIGHT INSIDE-HEIGHT)
            (TV:SHEET-FORCE-ACCESS (SELF)
              (TV:PREPARE-SHEET (SELF)
                (TV:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH) (- INSIDE-HEIGHT LINES-HEIGHT)
                                    (TV:SHEET-INSIDE-LEFT)
                                    (+ (TV:SHEET-INSIDE-TOP) LINES-HEIGHT)
                                    TV:ERASE-ALUF SELF))))))))

;;;; Methods that pertain to editor-streams.

(PROCLAIM '(SPECIAL *INSIDE-EDITOR-STREAM*))    ;Defvar is in STREAM.

;;; When inside the editor stream (but NOT when inside the editor inside it)
;;; we glitch up at the end of the page.
(DEFMETHOD (ZWEI :BEFORE :END-OF-PAGE-EXCEPTION) (&AUX (M-VP TV:MORE-VPOS)
                                                 (OLD-CURSOR-Y TV:CURSOR-Y))
  (WHEN *INSIDE-EDITOR-STREAM*
    (LET ((LINES (TRUNCATE (1- N-PLINES) 3)))
      (TV:SHEET-HOME SELF)
      (TV:SHEET-DELETE-LINE SELF LINES)
      ;; Arrange for more processing next time around
      (COND ((NULL M-VP))                       ;No more processing at all
            (( M-VP 100000)                    ;More processing delayed?
             (SETQ TV:MORE-VPOS (- M-VP 100000)))       ;Cause to happen next time around
            ;; Cause more processing after two more glitches.
            (T (SETQ TV:MORE-VPOS (+ 200000 (TV:SHEET-DEDUCE-MORE-VPOS SELF)))))
      (TV:PREPARE-SHEET (SELF)
        (SETQ TV:CURSOR-Y
              (- OLD-CURSOR-Y
                 (* LINES TV:LINE-HEIGHT))))
      (LET ((BP START-BP))
        (MULTIPLE-VALUE-BIND (LINE INDEX)
            (PUT-POINT-AT-PLINE SELF (BP-LINE BP) (BP-INDEX BP) (- LINES)
                                (INTERVAL-FIRST-BP INTERVAL)
                                (INTERVAL-LAST-BP INTERVAL))
          (MOVE-BP BP LINE INDEX))))))

;;; After waiting for input, don't **more** until three more glitches go by
;;; because until then all text that will scroll off has already been read.
;;; This method has no effect if it returns nil (method-combination-type is OR).
(DEFMETHOD (ZWEI :NOTICE) (EVENT &REST IGNORE)
  (AND (EQ EVENT ':INPUT-WAIT)
       *INSIDE-EDITOR-STREAM*
       TV:MORE-VPOS
       (SETQ TV:MORE-VPOS
             (+ (* 100000
                   ;; Number of glitches we can do without this line going off screen.
                   (TRUNCATE (- TV:CURSOR-Y TV:TOP-MARGIN-SIZE)
                             (* TV:LINE-HEIGHT
                                (TRUNCATE (1- N-PLINES) 3))))
                (TV:SHEET-DEDUCE-MORE-VPOS SELF)))))

;;; We hereby contrive to do *MORE* processing directly on the sheet
;;; without going through the editor stream mechanism for either input or output.
(DEFMETHOD (ZWEI :MORE-EXCEPTION) ()
  (IF *INSIDE-EDITOR-STREAM*
      (OR (ZEROP (TV:SHEET-MORE-FLAG))
          (TV:SHEET-MORE-HANDLER))
    ;; If not in editor stream, ignore **MORE**s.
    (SETF (TV:SHEET-MORE-FLAG) 0)))

;;; These are sent by ZTOP streams to inform us of changes of ZTOP's state.
;;; That information is displayed and communicated in other ways,
;;; so we ignore these messages, whose intended recipient is the EDITOR-STREAM-WINDOW.
(DEFMETHOD (ZWEI :ENTER-EDITOR) () NIL)
(DEFMETHOD (ZWEI :EXIT-EDITOR) () NIL)
(DEFMETHOD (ZWEI :ACTIVATION-NEEDED) () NIL)

(DEFFLAVOR ZWEI-WITHOUT-TYPEOUT ()
       (ZWEI TV:BORDERS-MIXIN TV:BASIC-SCROLL-BAR
        TV:DELAYED-REDISPLAY-LABEL-MIXIN TV:WINDOW)
  :ABSTRACT-FLAVOR)

;;;; ZWEI-WITH-TYPEOUT gives it a typeout window.

(DEFFLAVOR EDITOR-TYPEOUT-WINDOW ()
           (TV:TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS))

(DEFUN TYPEOUT-WINDOW-INCOMPLETE-P (TYPEOUT-WINDOW)
  (IF (TYPEP TYPEOUT-WINDOW 'TV:BASIC-TYPEOUT-WINDOW)
      (TV:BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW)
    (SEND TYPEOUT-WINDOW ':INCOMPLETE-P)))

(DEFMETHOD (EDITOR-TYPEOUT-WINDOW :MORE-TYI) (&AUX CH)
  (DO-FOREVER
    (SETQ CH (SEND SELF ':ANY-TYI))
    (WHEN (OR (NUMBERP CH)
              (AND (CONSP CH) (EQ (CAR CH) ':TYPEOUT-EXECUTE)))
      (RETURN CH))))

(DEFMETHOD (EDITOR-TYPEOUT-WINDOW :MORE-EXCEPTION) (&AUX CH)
  (UNLESS (ZEROP (TV:SHEET-MORE-FLAG))
    (SETQ CH (TV:SHEET-MORE-HANDLER ':MORE-TYI))
    (COND ((NULL CH))   ;on timeout, do nothing.
          ((OR (NOT (NUMBERP CH)) ( CH #/SP))
           (OR (MEMQ CH '(#/RUBOUT #/CR))
               (SEND SELF ':UNTYI CH))
           (LET ((EPF (TV:SHEET-END-PAGE-FLAG)))
             (SETF (TV:SHEET-END-PAGE-FLAG) 0)
             (TV:SHEET-STRING-OUT SELF "**FLUSHED**")
             (SETF (TV:SHEET-END-PAGE-FLAG) EPF))
           (SIGNAL-CONDITION EH:ABORT-OBJECT)))))

;(defmethod (editor-typeout-window :before :deactivate) ()
;  tv:(prepare-sheet (self)
;  (let ((bottom (min (sheet-inside-bottom) (1+ (send self :bottom-reached)))))
;    (%draw-rectangle (sheet-inside-width)
;                    (- bottom (sheet-inside-top))
;                    (sheet-inside-left)
;                    (sheet-inside-top)
;                    (sheet-erase-aluf self)
;                    self))))

(DEFFLAVOR ZWEI-TYPEOUT-MIXIN () (TV:WINDOW-WITH-TYPEOUT-MIXIN)
  (:REQUIRED-FLAVORS ZWEI-WITHOUT-TYPEOUT)
  (:INIT-KEYWORDS :ITEM-TYPE-ALIST)
  (:DEFAULT-INIT-PLIST :ITEM-TYPE-ALIST NIL)
  (:DOCUMENTATION "An editor window with a typeout window too"))

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :AFTER :INIT) (INIT-PLIST)
  (OR TV:TYPEOUT-WINDOW
      (SETQ TV:TYPEOUT-WINDOW
            (MAKE-INSTANCE 'EDITOR-TYPEOUT-WINDOW
                           :ITEM-TYPE-ALIST (GET INIT-PLIST ':ITEM-TYPE-ALIST)
                           :IO-BUFFER TV:IO-BUFFER
                           :SUPERIOR SELF))))

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :SCREEN-MANAGE) (&REST IGNORE))

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :TURN-OFF-BLINKERS-FOR-TYPEOUT) ()
  (DECLARE (SPECIAL *MOUSE-BLINKER*))
  (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* NIL)
  (TV:MOUSE-STANDARD-BLINKER))

(DEFVAR *MOUSE-FONT-CHAR* 25.)
(DEFVAR *MOUSE-X-OFFSET* 8)
(DEFVAR *MOUSE-Y-OFFSET* 0)

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :TURN-ON-BLINKERS-FOR-TYPEOUT) ()
  (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET* ':ON
                                   ':SET-CHARACTER *MOUSE-FONT-CHAR*))

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :READY-FOR-REDISPLAY-P) (&OPTIONAL (CURRENT-WINDOW-SPECIAL T))
  (AND (ZEROP (TV:SHEET-OUTPUT-HOLD-FLAG))
       (OR (AND CURRENT-WINDOW-SPECIAL (EQ SELF *WINDOW*))
           (NOT (TV:BASIC-TYPEOUT-WINDOW-BOTTOM-REACHED
                  (TV:ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN-TYPEOUT-WINDOW SELF))))))

(DEFMETHOD (ZWEI-TYPEOUT-MIXIN :PREPARE-FOR-REDISPLAY) ()
  (UNLESS TV:EXPOSED-P (SEND SELF ':EXPOSE))
  (WHEN (TV:BASIC-TYPEOUT-WINDOW-BOTTOM-REACHED TV:TYPEOUT-WINDOW)
    (LET ((BOTTOM-REACHED (SEND TV:TYPEOUT-WINDOW ':BOTTOM-REACHED)))
      (SEND TV:TYPEOUT-WINDOW ':DEACTIVATE)
      (LET ((N-PLINES-CLOBBERED (MIN (CEILING (1+ BOTTOM-REACHED) TV:LINE-HEIGHT)
                                     N-PLINES)))
        (SETQ REDISPLAY-DEGREE
              (MAX DIS-TEXT REDISPLAY-DEGREE))
        ;; Force redisplay of the lines that were used for typeout.
        (DOTIMES (I N-PLINES-CLOBBERED)
          (SETF (PLINE-TICK SELF I) -1)))
      (LET ((LINES-HEIGHT (* TV:LINE-HEIGHT N-PLINES))
            (INSIDE-HEIGHT (TV:SHEET-INSIDE-HEIGHT)))
        (OR (< BOTTOM-REACHED LINES-HEIGHT)
            (= LINES-HEIGHT INSIDE-HEIGHT)
            (TV:SHEET-FORCE-ACCESS (SELF)
              (TV:PREPARE-SHEET (SELF)
                (TV:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH)
                                    (- INSIDE-HEIGHT LINES-HEIGHT)
                                    (TV:SHEET-INSIDE-LEFT)
                                    (+ (TV:SHEET-INSIDE-TOP) LINES-HEIGHT)
                                    TV:ERASE-ALUF SELF)))))))
  (AND TV:LABEL-NEEDS-UPDATING
       (SEND SELF ':UPDATE-LABEL)))

(DEFFLAVOR ZWEI-WINDOW ()
       (ZWEI-TYPEOUT-MIXIN ZWEI-WITHOUT-TYPEOUT TV:FLASHY-SCROLLING-MIXIN)
  :ABSTRACT-FLAVOR
  (:DOCUMENTATION "A non-ZMACS editor window.
It is not instantiable, since it does not handle :MODE-LINE-WINDOW
/(for good reason, since at this level of generality it is not determined
where the mode line window would be found)."))

;;; These windows are made by CREATE-OVERLYING-WINDOW
;;; and used for View commands.
(DEFFLAVOR ZWEI-OVERLYING-WINDOW (FOR-WINDOW) (ZWEI-WINDOW)
  :INITTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (ZWEI-OVERLYING-WINDOW :SET-BASE-TICK) (TICK)
  (SETQ BASE-TICK TICK))

(DEFMETHOD (ZWEI-OVERLYING-WINDOW :TOP-OF-EDITOR-HIERARCHY) ()
  (SEND FOR-WINDOW ':TOP-OF-EDITOR-HIERARCHY))

(DEFMETHOD (ZWEI-OVERLYING-WINDOW :MODE-LINE-WINDOW) ()
  (SEND FOR-WINDOW ':MODE-LINE-WINDOW))

;;;; Mouse screen primitives

(PROCLAIM '(SPECIAL *MOUSE-P* *MOUSE-BLINKER*
                    *GLOBAL-MOUSE-CHAR-BLINKER*
                    *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*))

(DEFVAR *MOUSE-CLICK-ALWAYS-SELECTS* NIL
  "T => clicking on an editor window always moves point to whereyou clicked,
even if the window had not been selected.")

;;; Called when the mouse enters a ZWEI window.  Change the shape of the blinker,
;;; then call the standard mouse tracker, telling it we have a scroll bar.
(DEFMETHOD (ZWEI :HANDLE-MOUSE) ()
  (LET-GLOBALLY ((*MOUSE-P* T))
    (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET* ':ON
                                     ':SET-CHARACTER *MOUSE-FONT-CHAR*)
    (TV:MOUSE-DEFAULT-HANDLER SELF (SEND SELF ':SCROLL-BAR-P))
    (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* NIL)))

(DEFMETHOD (ZWEI :SCROLL-BAR-P) () T)

;;; Handle a movement of the mouse within an editor window.
;;; Update the blinker which flashes the character being pointed at.
(DEFMETHOD (ZWEI :MOUSE-MOVES) (NEW-X NEW-Y &AUX CHAR CHAR-X CHAR-Y LINE INDEX WIDTH)
  NEW-X NEW-Y
  (TV:MOUSE-SET-BLINKER-CURSORPOS)
  (AND (OR (EDITOR-WINDOW-SELECTED-P SELF) *MOUSE-CLICK-ALWAYS-SELECTS*)
       ( NEW-X (TV:SHEET-INSIDE-LEFT)) (< NEW-X (TV:SHEET-INSIDE-RIGHT))
       (MULTIPLE-VALUE (CHAR CHAR-X CHAR-Y LINE INDEX WIDTH)
         (MOUSE-CHAR SELF)))
  (AND CHAR
       ;; There is a timing problem if the editor's process can disable the global blinker
       ;; handler while we are inside it, it will turn on the blinker after the editor has
       ;; just turned it off.
       (WITHOUT-INTERRUPTS
         (COND (*GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
                (FUNCALL *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
                         *GLOBAL-MOUSE-CHAR-BLINKER* SELF
                         CHAR CHAR-X CHAR-Y LINE INDEX)
                (SETQ CHAR NIL)))))             ;Only have one blinker
  (COND (CHAR
         (TV:BLINKER-SET-SHEET *MOUSE-BLINKER* SELF)
         (LET ((FONT-NUMBER (CHAR-FONT CHAR)))
           (IF ( FONT-NUMBER (ARRAY-LENGTH TV:FONT-MAP))
               (SETQ FONT-NUMBER 0))
           (LET ((FONT (AREF TV:FONT-MAP FONT-NUMBER))
                 (CH (CHAR-CODE CHAR)))
             (COND ((TYPEP *MOUSE-BLINKER* 'TV:CHARACTER-BLINKER)
                    (LET ((LKT (FONT-LEFT-KERN-TABLE FONT)))
                      (AND LKT ( CH (ARRAY-ACTIVE-LENGTH LKT))
                           (SETQ CHAR-X (- CHAR-X (AREF LKT CH)))))
                    (SHEET-SET-BLINKER-CURSORPOS SELF *MOUSE-BLINKER* CHAR-X CHAR-Y)
                    (TV:BLINKER-SET-CHARACTER *MOUSE-BLINKER* FONT
                                      ;; Non printing characters get blinking underscore
                                              (IF (OR (= CH #/SP) ( CH 200)) #/_ CH))
                    (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* ':BLINK))
                   ((OR ( CH #/CR)
                        ;; Don't show the char mouse is "on" if it's a Return
                        ;; and the mouse is way past the last actual text on the line.
                        (AND (< NEW-X (+ (TV:SHEET-INSIDE-LEFT) CHAR-X
                                         (SETQ WIDTH TV:CHAR-WIDTH)))
                             (< NEW-Y (+ (TV:SHEET-INSIDE-TOP) CHAR-Y TV:LINE-HEIGHT))))
                    (SHEET-SET-BLINKER-CURSORPOS SELF *MOUSE-BLINKER* CHAR-X CHAR-Y)
                    (IF WIDTH   ;double check, to avoid bombout in mouse-process.
                        (TV:BLINKER-SET-SIZE *MOUSE-BLINKER* WIDTH
                                             (FONT-BLINKER-HEIGHT FONT)))
                    (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* T))
                   (T
                    (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* NIL))))))
        (T
         (TV:BLINKER-SET-SHEET *MOUSE-BLINKER* SELF)
         (TV:BLINKER-SET-VISIBILITY *MOUSE-BLINKER* NIL))))

(DEFUN SHEET-SET-BLINKER-CURSORPOS (SHEET BLINKER X Y)
  (MULTIPLE-VALUE-BIND (XOFF YOFF)
      (TV:SHEET-CALCULATE-OFFSETS SHEET (TV:BLINKER-SHEET BLINKER))
    (TV:BLINKER-SET-CURSORPOS BLINKER (+ X XOFF) (+ Y YOFF))))

(DEFMETHOD (ZWEI :MOUSE-CLICK) (BUTTON X Y &AUX HANDLED-P)
  (SETQ BUTTON (TV:MERGE-SHIFT-KEYS BUTTON))
  (COND ((NOT (SEND (SEND SELF ':TOP-OF-EDITOR-HIERARCHY)
                    ':SELF-OR-SUBSTITUTE-SELECTED-P))
         ;; This frame or whatever is not selected.
         (TV:MOUSE-SELECT SELF))
        ((AND (NOT (EDITOR-WINDOW-SELECTED-P SELF))
              (OR (= BUTTON #/MOUSE-1-1)
                  *MOUSE-CLICK-ALWAYS-SELECTS*))
         ;; Frame selected but this editor window is not.  Just switch to it.
         (COMMAND-BUFFER-PUSH `(SELECT-WINDOW ,SELF))
         (IF *MOUSE-CLICK-ALWAYS-SELECTS*
             ;; And maybe also do the command for the mouse button.
             (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y)))
         (SETQ HANDLED-P T))
        (T
         (COMMAND-BUFFER-PUSH `(:MOUSE-BUTTON ,BUTTON ,SELF ,X ,Y))))
  T)

(DEFUN EDITOR-WINDOW-SELECTED-P (WINDOW)
  "T if WINDOW is the selected editor window in its own editor closure."
  (EQ WINDOW (FUNCALL (WINDOW-EDITOR-CLOSURE WINDOW) 'SYMBOL-VALUE '*WINDOW*)))

;;;; Scrolling

;;; Returns 2 values: current line#, total #lines
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
              TV:LINE-HEIGHT))))

(DEFMETHOD (ZWEI :SCROLL-MORE-ABOVE) ()
  (NOT (BP-= START-BP (INTERVAL-FIRST-BP INTERVAL))))

;;; Valid only if redisplay up to date
(DEFMETHOD (ZWEI :SCROLL-MORE-BELOW) ()
  (PLINE-LINE SELF (1- N-PLINES)))

;;; Scroll the window.
(DEFMETHOD (ZWEI :SCROLL-TO) (POS TYPE)
  (OR (AND (EQ TYPE ':RELATIVE) (ZEROP POS))            ;Don't ask not to scroll
      (COMMAND-BUFFER-PUSH `(SCROLL ,SELF ,POS ,TYPE)))
  POS)

;;;; The mini buffer and stuff like that

#| ;Not needed any more.
;;;These windows can pop up like a temporary window with :TEMPORARY-EXPOSE,
;;;or can be exposed in a non-temporary manner with plain :EXPOSE.
(DEFFLAVOR DEEXPOSED-TEMPORARY-WINDOW (DEEXPOSED-TEMPORARY-BIT-ARRAY) ()
  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW))

(DEFMETHOD (DEEXPOSED-TEMPORARY-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ DEEXPOSED-TEMPORARY-BIT-ARRAY
        (MAKE-ARRAY (LIST (LOGAND -40 (+ 37 TV:WIDTH)) TV:HEIGHT) ':TYPE 'ART-1B)))

(DEFMETHOD (DEEXPOSED-TEMPORARY-WINDOW :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SETQ DEEXPOSED-TEMPORARY-BIT-ARRAY
        (TV:GROW-BIT-ARRAY DEEXPOSED-TEMPORARY-BIT-ARRAY TV:WIDTH TV:HEIGHT)))

(DEFMETHOD (DEEXPOSED-TEMPORARY-WINDOW :TEMPORARY-EXPOSE) (&REST IGNORE)
  (COND ((NOT TV:EXPOSED-P)
         (IF (TV:SHEET-EXPOSED-P TV:SUPERIOR)
;            (SETQ TV:TEMPORARY-BIT-ARRAY DEEXPOSED-TEMPORARY-BIT-ARRAY)
             (SEND TV:SUPERIOR ':TEMPORARY-EXPOSE))
         (SEND SELF ':EXPOSE))))

(DEFMETHOD (DEEXPOSED-TEMPORARY-WINDOW :TEMPORARY-DEEXPOSE) ()
  (SEND SELF ':DEEXPOSE)
  (SETQ TV:TEMPORARY-BIT-ARRAY NIL))

|#;end comment

(DEFFLAVOR FIXED-HEIGHT-WINDOW-MIXIN () ()
  (:REQUIRED-FLAVORS TV:ESSENTIAL-SET-EDGES TV:ESSENTIAL-WINDOW)
  (:DOCUMENTATION "Does not allow changing the height of the window after init"))

;;; Bind this if you want to insist on changing the window's height
;;; (for example, if you are its superior and you know what you are doing).
(DEFVAR *ALLOW-CHANGING-HEIGHT* NIL)

(DEFMETHOD (FIXED-HEIGHT-WINDOW-MIXIN :VERIFY-NEW-EDGES)
           (NEW-LEFT NEW-TOP NEW-WIDTH NEW-HEIGHT)
  NEW-LEFT NEW-TOP NEW-WIDTH
  (AND ( NEW-HEIGHT TV:HEIGHT)
       (NOT *ALLOW-CHANGING-HEIGHT*)
       "Attempt to change height"))

(DEFMETHOD (FIXED-HEIGHT-WINDOW-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (DOLIST (INFERIOR TV:INFERIORS)
    (SEND INFERIOR ':SET-SIZE (TV:SHEET-INSIDE-WIDTH) (TV:SHEET-HEIGHT INFERIOR))))

(DEFFLAVOR ECHO-AREA-WINDOW () (TV:DONT-SELECT-WITH-MOUSE-MIXIN
                                TV:NOTIFICATION-MIXIN
                                TV:NO-SCREEN-MANAGING-MIXIN
                                TV:STREAM-MIXIN TV:SELECT-MIXIN
                                FIXED-HEIGHT-WINDOW-MIXIN TV:MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :MORE-P NIL))

#| I believe this is not necessary - RMS 2/23/83
(DEFMETHOD (ECHO-AREA-WINDOW :MOUSE-SELECT) (&REST IGNORE)
  (AND TV:SUPERIOR (SEND TV:SUPERIOR ':MOUSE-SELECT))))
|#

;;; This is used to make a *TYPEIN-WINDOW*; it will acquire a blinker
;;; if anyone tries to read input from it.  This window can be the value of
;;; *QUERY-IO*.
(DEFFLAVOR ECHO-AREA-QUERY-WINDOW () (ECHO-AREA-WINDOW))

(DEFWRAPPER (ECHO-AREA-QUERY-WINDOW :RUBOUT-HANDLER) (IGNORE . BODY)
  `(IF (NEQ SELF (SEND (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY)
                       ':SELECTION-SUBSTITUTE))
       ;(EQ (TV:BLINKER-VISIBILITY (CAR TV:BLINKER-LIST)) ':OFF)
       (TV:WITH-SELECTION-SUBSTITUTE (SELF (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY))
         . ,BODY)
     (PROGN . ,BODY)))

(DEFWRAPPER (ECHO-AREA-QUERY-WINDOW :ANY-TYI) (IGNORE . BODY)
  `(IF (NEQ SELF (SEND (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY)
                       ':SELECTION-SUBSTITUTE))
       ;(EQ (TV:BLINKER-VISIBILITY (CAR TV:BLINKER-LIST)) ':OFF)
       (TV:WITH-SELECTION-SUBSTITUTE (SELF (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY))
         . ,BODY)
     (PROGN . ,BODY)))

(DEFMETHOD (ECHO-AREA-QUERY-WINDOW :TOP-OF-EDITOR-HIERARCHY) ()
  (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY))

;;; This is the flavor used to make a *TYPEIN-WINDOW*.
(DEFFLAVOR TYPEIN-WINDOW
        ((TYPEIN-STATUS ':CLEAR)) ;:CLEAR, :IN-USE, :USED, :IN-USE-STAYS, :USED-STAYS
        (ECHO-AREA-QUERY-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES TYPEIN-STATUS)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES TYPEIN-STATUS))

(DEFMETHOD (TYPEIN-WINDOW :AFTER :REFRESH) (&REST IGNORE)
  (COND ((NOT TV:RESTORED-BITS-P)
         (TV:SHEET-HOME SELF)
         (SETQ TYPEIN-STATUS ':CLEAR))))

(DEFMETHOD (TYPEIN-WINDOW :AFTER :CLEAR-WINDOW) ()
  (SETQ TYPEIN-STATUS :CLEAR))
(DEFMETHOD (TYPEIN-WINDOW :AFTER :CLEAR-SCREEN) ()
  (SETQ TYPEIN-STATUS :CLEAR))

(DEFMETHOD (TYPEIN-WINDOW :COMMAND-LOOP-REDISPLAY) ()
  (CASE TYPEIN-STATUS
    (:IN-USE
     (SETQ TYPEIN-STATUS ':USED))
    (:IN-USE-STAYS
     (SETQ TYPEIN-STATUS ':USED-STAYS))
    (:USED
     (IF TV:EXPOSED-P
         (SEND SELF :CLEAR-WINDOW)
       (SETQ TYPEIN-STATUS ':CLEAR)))))

(DEFMETHOD (TYPEIN-WINDOW :PREPARE-FOR-TYPEOUT) ()
  (LET* ((MINI-BUFFER-SHEET (WINDOW-SHEET (SEND TV:SUPERIOR ':MINI-BUFFER-WINDOW)))
         (SEARCH-MINI-BUFFER-SHEET (WINDOW-SHEET
                                     (SEND TV:SUPERIOR ':SEARCH-MINI-BUFFER-WINDOW)))
         (MINI-BUFFER-IN-USE (TV:SHEET-EXPOSED-P MINI-BUFFER-SHEET))
         (SEARCH-MINI-BUFFER-IN-USE (TV:SHEET-EXPOSED-P SEARCH-MINI-BUFFER-SHEET)))
    ;; Expose our superior, if necessary.
    ;; Only happens for the POP-UP-MODE-LINE-WINDOW.
    (OR TV:EXPOSED-P
        (TV:SHEET-EXPOSED-P TV:SUPERIOR)
        (SEND TV:SUPERIOR ':EXPOSE))
    (if (or mini-buffer-in-use search-mini-buffer-in-use)
        (SEND (SEND TV:SUPERIOR :TOP-OF-EDITOR-HIERARCHY) :SET-SELECTION-SUBSTITUTE SELF))
    (SEND SELF ':EXPOSE)
    (AND MINI-BUFFER-IN-USE (SEND MINI-BUFFER-SHEET :START-DELAYED-SELECT))
    (AND SEARCH-MINI-BUFFER-IN-USE (SEND SEARCH-MINI-BUFFER-SHEET :START-DELAYED-SELECT)))
  (IF (MEMQ TYPEIN-STATUS '(:IN-USE :IN-USE-STAYS))
      (SEND SELF :FRESH-LINE)
    (OR (EQ TYPEIN-STATUS ':CLEAR)
        (TV:SHEET-CLEAR SELF))
    (SETQ TYPEIN-STATUS ':IN-USE)
    T))

;;; This differs from the default in that we don't print a Return after the message
;;; and we take account of that in deciding whether it will fit.
(DEFMETHOD (TYPEIN-WINDOW :PRINT-NOTIFICATION)
           (TIME STRING WINDOW-OF-INTEREST &AUX (COMBINED-STRING STRING))
  (SETQ COMBINED-STRING
        (FORMAT:OUTPUT NIL
          (TYO #/[)
          (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME)
          (TYO #/SP)
          (PRINC STRING)
          (TYO #/])))
  (IF (MULTIPLE-VALUE-BIND (NIL NIL FINAL-INDEX)
          (TV:SHEET-COMPUTE-MOTION SELF 0 0 STRING 0 NIL NIL
                                   0 (- (TV:SHEET-INSIDE-HEIGHT) TV:LINE-HEIGHT) 1.0S6)
        FINAL-INDEX)
      ;; We are too small to print this notification.  Use a pop-up-window.
      (TV:POP-UP-NOTIFY ':PRINT-NOTIFICATION
                        TIME STRING WINDOW-OF-INTEREST)
    ;; We can print this notification on the seleted window, so do it.
    (SEND SELF ':PREPARE-FOR-TYPEOUT)
    (SEND SELF ':BEEP)
    (PRINC COMBINED-STRING SELF)
    (SEND SELF ':TYPEOUT-STAYS)))

(DEFMETHOD (TYPEIN-WINDOW :TYPEOUT-STAYS) ()
  (AND (EQ TYPEIN-STATUS ':IN-USE) (SETQ TYPEIN-STATUS ':IN-USE-STAYS)))

(DEFMETHOD (TYPEIN-WINDOW :REMAKE-INCOMPLETE) ()
  (SETQ TYPEIN-STATUS
        (SELECTQ TYPEIN-STATUS
          (:USED ':IN-USE)
          (:USED-STAYS ':IN-USE-STAYS)
          (T TYPEIN-STATUS))))

(DEFUN MAKE-INCOMPLETE (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR TYPEIN-WINDOW))
  (OR (MEMQ TYPEIN-STATUS '(:IN-USE :IN-USE-STAYS))
      (SEND SELF ':PREPARE-FOR-TYPEOUT)))

(DEFMETHOD (TYPEIN-WINDOW :BEFORE :CLEAR-WINDOW) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :CLEAR-SCREEN) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :TYO) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :WRITE-CHAR) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :STRING-OUT) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :LINE-OUT) MAKE-INCOMPLETE)
(DEFMETHOD (TYPEIN-WINDOW :BEFORE :FRESH-LINE) MAKE-INCOMPLETE)

;;; This improves interaction with FQUERY
(DEFMETHOD (TYPEIN-WINDOW :MAKE-COMPLETE) ()
  (AND (EQ TYPEIN-STATUS ':IN-USE) (SETQ TYPEIN-STATUS ':USED)))

;;; T if there is recent output on the window, which is still
;;; being output, and therefore it should not be flushed immediately.
(DEFMETHOD (TYPEIN-WINDOW :INCOMPLETE-P) ()
  (MEMQ TYPEIN-STATUS '(:IN-USE :IN-USE-STAYS)))

;;; This knows how to display the editor's mode line properly
(DEFFLAVOR MODE-LINE-WINDOW-MIXIN
           ((PREVIOUS-MODE-LINE NIL)
            (RECALCULATE-FUNCTION NIL))
           ()
  (:INITTABLE-INSTANCE-VARIABLES RECALCULATE-FUNCTION)
  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW)
  (:DEFAULT-INIT-PLIST :TRUNCATE-LINE-OUT-FLAG 1
                       :CR-NOT-NEWLINE-FLAG 1))

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P (SETQ PREVIOUS-MODE-LINE NIL)))

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :CLOBBER) ()
  (SETQ PREVIOUS-MODE-LINE NIL))

;;; Update the mode line if necessary, FORCE says really do it
;;; MODE-LINE-LIST is a list of things to be displayed, whose elements can be:
;;;  a constant string
;;;  a symbol, which is evaluated to either a string or NIL, and printed in the former case
;;;  a list, the CAR of which should be an atom, which is evaluated and the rest of the
;;;    list handled as strings or symbols as above if it is non-NIL (up to any :ELSE), or
;;;    if NIL, anything after a :ELSE in the list.
;;;  eg ("FOOMACS" "(" MODE-NAME ")" (BUFFER-NAMED-P BUFFER-NAME :ELSE "(Null buffer)")
;;;      (FILE-NAME-P FILE-NAME))
;;;  a list starting with the symbol :RIGHT-FLUSH is special:
;;;    the cadr of the list is a string to be displayed flush against the right margin.
;;; As a special hack, if MODE-LINE-LIST is NIL, then the mode line is not changed,
;;;  this is appropriate for things that want to typeout on the prompt-line and then
;;;  invoke the mini-buffer.
;;; PREVIOUS-MODE-LINE is a list of strings that make up the line, since nothing we do
;;;  generates new guys for this, EQness is used to determine if the mode-line has changed

;;; The RECALCULATE-FUNCTION if non-NIL is called before doing anything else.
;;; It can therefore update some of the variables that will be used to do displaying.
;;; Also, any symbol whose value is to be displayed is checked for a
;;; MODE-LINE-RECALCULATE property; if it is non-NIL, it is called before
;;; evaluating the symbol.

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :REDISPLAY) (MODE-LINE-LIST &OPTIONAL FORCE)
  (AND RECALCULATE-FUNCTION (FUNCALL RECALCULATE-FUNCTION))
  (AND FORCE                                    ;If we are going to type things out
       MODE-LINE-LIST                           ;unless suppressed
       (SETQ PREVIOUS-MODE-LINE NIL))
  (DO ((MODES MODE-LINE-LIST)
       (PREV PREVIOUS-MODE-LINE)
       (L)
       (THING))
      (NIL)
      (COND (L                                  ;Still more to go on a list
             (POP L THING)
             (AND (EQ THING ':ELSE)
                  (SETQ L NIL THING NIL)))
            ((NULL MODES)                       ;All done with MODE-LINE-LIST
             (AND PREV (NOT FORCE) (SEND SELF ':REDISPLAY MODE-LINE-LIST T))
             (RETURN NIL))
            (T                                  ;Get next object from MODE-LINE-LIST
             (POP MODES THING)
             (COND ((SYMBOLP THING)
                    (LET ((PROP (GET THING 'MODE-LINE-RECALCULATE)))
                      (IF PROP (FUNCALL PROP)))
                    (SETQ THING (SI:SYMEVAL-MAYBE-IN-INSTANCE SELF THING))
                    (AND (CONSP THING)          ;If value is a list, dont check CAR
                         (SETQ L THING THING NIL)))
                   ((AND (CONSP THING)          ;It's a list,
                         (NEQ (CAR THING) ':RIGHT-FLUSH))
                    (SETQ L THING)
                    (POP L THING)
                    (COND ((NULL (SI:SYMEVAL-MAYBE-IN-INSTANCE SELF THING))
                           (DO ()               ;Failing conditional, look for :ELSE
                               ((NULL L))
                             (POP L THING)
                             (AND (EQ THING ':ELSE)
                                  (RETURN NIL)))))
                    (SETQ THING NIL)))))        ;And get stuff next pass
      (WHEN (SYMBOLP THING)
        (LET ((PROP (GET THING 'MODE-LINE-RECALCULATE)))
          (IF PROP (FUNCALL PROP)))
        (SETQ THING (SI:SYMEVAL-MAYBE-IN-INSTANCE SELF THING)))
      (COND ((NULL THING))
            ;;THING is now the next string to be put into the mode line
            (FORCE                              ;Put it in if consing new one
             (PUSH THING PREVIOUS-MODE-LINE))
            ((AND PREV (EQ THING (POP PREV))))  ;Still matching?
            (T                                  ;Different thing,
             (SEND SELF ':REDISPLAY MODE-LINE-LIST T)   ;do it right this time!
             (RETURN NIL))))
  (WHEN FORCE
    (SETQ PREVIOUS-MODE-LINE (NREVERSE PREVIOUS-MODE-LINE))
    (WHEN TV:EXPOSED-P
      (TV:SHEET-HOME SELF)
      (TV:SHEET-CLEAR-EOL SELF)
      (CATCH 'MODE-LINE-OVERFLOW
        (DOLIST (STR PREVIOUS-MODE-LINE)
          (AND (STRINGP STR) (SEND SELF ':STRING-OUT STR))))
      (DOLIST (ELT PREVIOUS-MODE-LINE)
        (AND (CONSP ELT)
             (LET* ((STR (CADR ELT))
                    (LEN (TV:SHEET-STRING-LENGTH SELF STR)))
               (TV:SHEET-SET-CURSORPOS SELF
                                       (- (TV:SHEET-INSIDE-WIDTH SELF) LEN)
                                       0)
               (TV:SHEET-CLEAR-EOL SELF)
               (CATCH 'MODE-LINE-OVERFLOW
                 (SEND SELF ':STRING-OUT STR))
               (RETURN NIL)))))))

(DEFUN (:PROPERTY *MORE-ABOVE-BELOW* MODE-LINE-RECALCULATE) ()
  (LET ((ABOVE (NOT (BP-= (WINDOW-START-BP *WINDOW*)
                          (INTERVAL-FIRST-BP (WINDOW-INTERVAL *WINDOW*)))))
        (BELOW (PLINE-LINE *WINDOW*
                           (1- (WINDOW-N-PLINES *WINDOW*)))))
    (SETQ *MORE-ABOVE-BELOW*
          (IF ABOVE (IF BELOW " " " ")
            (IF BELOW " " NIL)))))

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :BEFORE :END-OF-LINE-EXCEPTION) ()
  (OR (ZEROP (TV:SHEET-TRUNCATE-LINE-OUT-FLAG))
      (THROW 'MODE-LINE-OVERFLOW T)))

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD NEW)
  (DECLARE (IGNORE OLD))
  (IF (EQ NEW TV:CURRENT-FONT)
      (SETQ PREVIOUS-MODE-LINE NIL)))

(DEFMETHOD (MODE-LINE-WINDOW-MIXIN :DONE-WITH-MODE-LINE-WINDOW) ())

;;; This is a window that knows how to manage a typein window
;;; and mini buffer windows as inferiors.
(DEFFLAVOR MODE-LINE-SUPERIOR-MIXIN
        (TYPEIN-WINDOW MINI-BUFFER-WINDOW SEARCH-MINI-BUFFER-WINDOW)
        ()
  (:REQUIRED-FLAVORS TV:STREAM-MIXIN TV:MINIMUM-WINDOW)
  (:INIT-KEYWORDS :NUMBER-OF-MINI-BUFFER-LINES :MINI-BUFFER-EDITOR-CLOSURE-VARIABLES
                  :MINI-BUFFER-FLAVOR)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BLINKER-DESELECTED-VISIBILITY ':OFF))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :BEFORE :INIT) (PLIST &AUX NLINES)
  (AND (SETQ NLINES (GET PLIST ':NUMBER-OF-MINI-BUFFER-LINES))
       (PUTPROP PLIST
                (+ 4  ;Room for margins of mini buffer window.
                   (* (1+ NLINES)
                      (+ (MAX (FONT-CHAR-HEIGHT
                                TV:(SCREEN-DEFAULT-FONT (SHEET-GET-SCREEN SUPERIOR)))
                              (FONT-CHAR-HEIGHT FONTS:SEARCH))
                         2)))
                ':INSIDE-HEIGHT)))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :AFTER :INIT) (INIT-PLIST)
  (LET ((MINI-BUFFER-FLAVOR
          (OR (GET INIT-PLIST ':MINI-BUFFER-FLAVOR)
              'ZWEI-MINI-BUFFER)))
    (SETQ TYPEIN-WINDOW (MAKE-INSTANCE 'TYPEIN-WINDOW
                                       :TOP TV:(+ (SHEET-INSIDE-TOP) LINE-HEIGHT)
                                       :IO-BUFFER TV:IO-BUFFER
                                       :SUPERIOR SELF
                                       :MORE-P NIL
                                       :ACTIVATE-P T
                                       :BLINKER-DESELECTED-VISIBILITY ':OFF)
          MINI-BUFFER-WINDOW (MAKE-INSTANCE MINI-BUFFER-FLAVOR
                                       :TOP TV:(+ (SHEET-INSIDE-TOP) LINE-HEIGHT)
                                       :IO-BUFFER TV:IO-BUFFER
                                       :SUPERIOR SELF
                                       :EDITOR-CLOSURE-VARIABLES
                                         (GET INIT-PLIST ':MINI-BUFFER-EDITOR-CLOSURE-VARIABLES)
                                       :LABEL NIL
                                       :SAVE-BITS NIL
                                       :ACTIVATE-P T)
          SEARCH-MINI-BUFFER-WINDOW (MAKE-INSTANCE MINI-BUFFER-FLAVOR
                                       :FONT-MAP '(FONTS:CPTFONT FONTS:SEARCH)
                                       :TOP TV:(+ (SHEET-INSIDE-TOP) LINE-HEIGHT)
                                       :IO-BUFFER TV:IO-BUFFER
                                       :SUPERIOR SELF
                                       :FONT-ALIST `(("CPTFONT" . ,FONTS:CPTFONT)
                                                     ("SEARCH" . ,FONTS:SEARCH))
                                       :INTERVAL (LET* ((LINE (CREATE-LINE 'ART-FAT-STRING 50. NIL))
                                                        (INT (CREATE-INTERVAL
                                                               (CREATE-BP LINE 0 :NORMAL)
                                                               (CREATE-BP LINE 0 :MOVES))))
                                                   (SETF (LINE-NODE LINE) INT)
                                                   INT)
                                       :EDITOR-CLOSURE-VARIABLES
                                         (GET INIT-PLIST ':MINI-BUFFER-EDITOR-CLOSURE-VARIABLES)
                                       :LABEL NIL
                                       :SAVE-BITS NIL
                                       :ACTIVATE-P T))))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :NLINES) ()
  (1- (TRUNCATE (- (TV:SHEET-INSIDE-HEIGHT) 4)
                (+ (MAX (FONT-CHAR-HEIGHT TV:CURRENT-FONT)
                        (FONT-CHAR-HEIGHT FONTS:SEARCH))
                   2))))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :SET-NLINES) (NLINES)
  (LET ((*ALLOW-CHANGING-HEIGHT* T))
    (MULTIPLE-VALUE-BIND (LFT NIL RT BOT)
        (SEND SELF :EDGES)
      (SEND SELF :SET-EDGES LFT
                 (- BOT (+ 4 TV:TOP-MARGIN-SIZE TV:BOTTOM-MARGIN-SIZE
                           (* (1+ NLINES)
                              (+ (MAX (FONT-CHAR-HEIGHT TV:CURRENT-FONT)
                                      (FONT-CHAR-HEIGHT FONTS:SEARCH))
                                 2))))
                 RT BOT))))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :SET-IO-BUFFER) (NEW-IO-BUFFER)
  (SETQ TV:IO-BUFFER NEW-IO-BUFFER)
  (SEND TYPEIN-WINDOW ':SET-IO-BUFFER TV:IO-BUFFER)
  (SEND MINI-BUFFER-WINDOW ':SET-IO-BUFFER TV:IO-BUFFER)
; (SEND (SEND MINI-BUFFER-WINDOW :TYPEOUT-WINDOW) :SET-IO-BUFFER TV:IO-BUFFER)
  (SEND SEARCH-MINI-BUFFER-WINDOW :SET-IO-BUFFER TV:IO-BUFFER)
; (SEND (SEND SEARCH-MINI-BUFFER-WINDOW :TYPEOUT-WINDOW) :SET-IO-BUFFER TV:IO-BUFFER)
  )

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (LET ((LEFT (TV:SHEET-INSIDE-LEFT))
        (TOP (+ (TV:SHEET-INSIDE-TOP) TV:LINE-HEIGHT))
        (RIGHT (TV:SHEET-INSIDE-RIGHT))
        (BOTTOM (TV:SHEET-INSIDE-BOTTOM)))
    (SEND TYPEIN-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
    (SEND MINI-BUFFER-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
    (SEND SEARCH-MINI-BUFFER-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD NEW)
  (SEND TYPEIN-WINDOW :CHANGE-OF-DEFAULT-FONT OLD NEW)
  (SEND MINI-BUFFER-WINDOW :CHANGE-OF-DEFAULT-FONT OLD NEW)
  (SEND SEARCH-MINI-BUFFER-WINDOW :CHANGE-OF-DEFAULT-FONT OLD NEW)
  (LET ((*ALLOW-CHANGING-HEIGHT* T)
        (LEFT (TV:SHEET-INSIDE-LEFT))
        (TOP (+ (TV:SHEET-INSIDE-TOP) TV:LINE-HEIGHT))
        (RIGHT (TV:SHEET-INSIDE-RIGHT))
        (BOTTOM (TV:SHEET-INSIDE-BOTTOM)))
    (SEND TYPEIN-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
    (SEND MINI-BUFFER-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
    (SEND SEARCH-MINI-BUFFER-WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :EDITOR-WINDOWS) ()
  (SEND TV:SUPERIOR :EDITOR-WINDOWS))

(DEFMETHOD (MODE-LINE-SUPERIOR-MIXIN :TOP-OF-EDITOR-HIERARCHY) () TV:SUPERIOR)

(DEFFLAVOR MODE-LINE-WINDOW ()
           (MODE-LINE-SUPERIOR-MIXIN TV:BORDERS-MIXIN
            MODE-LINE-WINDOW-MIXIN ECHO-AREA-WINDOW)
  (:DEFAULT-INIT-PLIST :BORDERS NIL))

(DEFUN CREATE-MODE-LINE-WINDOW (&OPTIONAL (NLINES 3) (SUPERIOR TV:DEFAULT-SCREEN)
                                (MODE-LINE-FLAVOR 'MODE-LINE-WINDOW)
                                IO-BUFFER
                                &REST OPTIONS)
  (APPLY #'MAKE-INSTANCE MODE-LINE-FLAVOR
                        :NUMBER-OF-MINI-BUFFER-LINES NLINES
                        :SUPERIOR SUPERIOR
                        :IO-BUFFER IO-BUFFER
                        OPTIONS))

(DEFFLAVOR ZWEI-MINI-BUFFER
        ()
        (ZWEI-WITHOUT-TYPEOUT TV:DONT-SELECT-WITH-MOUSE-MIXIN))

(DEFCONST *MINI-BUFFER-TYPEIN-DELAY* 100.)

(DEFMETHOD (ZWEI-MINI-BUFFER :MODE-LINE-WINDOW) ()
  TV:SUPERIOR)

;;; Let the mode-line-window decide whether other editor windows
;;; (such as those directly under the ZWEI-FRAME) should be included.
(DEFMETHOD (ZWEI-MINI-BUFFER :EDITOR-WINDOWS) ()
  (SEND TV:SUPERIOR ':EDITOR-WINDOWS))

(DEFMETHOD (ZWEI-MINI-BUFFER :BEFORE :PREPARE-FOR-REDISPLAY) ()
  (SEND TV:SUPERIOR ':SEND-IF-HANDLES ':TEMPORARY-EXPOSE)
  (LET ((TOP-W (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY)))
    (SEND TOP-W ':SET-SELECTION-SUBSTITUTE SELF)))

(DEFMETHOD (ZWEI-MINI-BUFFER :RESELECT) ()
  (AND (NEQ SELF TV:SELECTED-WINDOW)
       (LET ((TOP-W (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY)))
         (SEND TV:SUPERIOR ':SEND-IF-HANDLES ':TEMPORARY-EXPOSE)
         (SEND TOP-W ':SET-SELECTION-SUBSTITUTE SELF))))

(DEFMETHOD (ZWEI-MINI-BUFFER :TYPEOUT-WINDOW) ()
  (ERROR "shouldn't get here"))

(DEFMETHOD (ZWEI-MINI-BUFFER :TOP-OF-EDITOR-HIERARCHY) ()
  (SEND TV:SUPERIOR ':TOP-OF-EDITOR-HIERARCHY))

(DEFMETHOD (ZWEI-MINI-BUFFER :BEFORE :FINISH-DELAYED-SELECT) ()
  (AND DELAYED-SELECT-PENDING (NOT TV:EXPOSED-P)
       (SEND (SEND TV:SUPERIOR ':TYPEIN-WINDOW) ':INCOMPLETE-P)
       (PROGN
         (SEND SELF ':WAIT-FOR-INPUT-WITH-TIMEOUT *MINI-BUFFER-TYPEIN-DELAY*)
         (SEND SELF ':SELECT)))
  NIL)

;;; This is sent by edit-in-mini-buffer.
(DEFMETHOD (ZWEI-MINI-BUFFER :MINI-BUFFER-ENTERED) ()
  (TV:BLINKER-SET-VISIBILITY (WINDOW-POINT-BLINKER SELF) NIL)  ;Redisplay will turn it back on.
  (SEND (SEND TV:SUPERIOR ':TYPEIN-WINDOW) ':MAKE-COMPLETE)
  (SETQ BASE-TICK *TICK*)
  T)

;;;??? Must arrange that the mini buffer editor is a top level one.

;;; The temporary mini buffer is used to allow programs other than ZWEI
;;; to do mini-buffer and typein-window style i/o in a pop-up window.
;;; This differs from the POP-UP-MODE-LINE-WINDOW in that that is designed
;;; to work with a ZWEI window, and be called through the standard mini-buffer
;;; and mode-line interfaces of ZWEI, whereas the TEMPORARY-MODE-LINE-WINDOW
;;; is designed to be called through special user interface functions
;;; (see below) and is independent of whatever other windows are on the screen.
;;; A special BACKGROUND-TYPEOUT-WINDOW is used as *TERMINAL-IO* while you are
;;; in the temporary mode line window.

;;; In addition, this window is actually a temporary window (saves bits of
;;; windows under it) and pops up near, rather than on top of, another window.

;;; BACKGROUND-TYPEOUT-WINDOW if non-NIL is a typeout window
;;; currently exposed and allocated to us.
;;; BACKGROUND-TYPEOUT-STREAM is always a closure over BACKGROUND-TYPEOUT-STREAM.
;;; It is our *TERMINAL-IO* when BACKGROUND-TYPEOUT-WINDOW is NIL.

(DEFFLAVOR TEMPORARY-MODE-LINE-WINDOW
        ((BACKGROUND-TYPEOUT-WINDOW NIL)
         BACKGROUND-TYPEOUT-STREAM)
        (MODE-LINE-SUPERIOR-MIXIN MODE-LINE-WINDOW-MIXIN
         TV:DONT-SELECT-WITH-MOUSE-MIXIN TV:ANY-TYI-MIXIN
         TV:TEMPORARY-WINDOW-MIXIN TV:NO-SCREEN-MANAGING-MIXIN
         TV:STREAM-MIXIN TV:SELECT-MIXIN
         FIXED-HEIGHT-WINDOW-MIXIN TV:MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :NUMBER-OF-MINI-BUFFER-LINES 1
    :MINI-BUFFER-FLAVOR 'TEMPORARY-MODE-LINE-MINI-BUFFER
    :MINI-BUFFER-EDITOR-CLOSURE-VARIABLES MEDIUM-LEVEL-EDITOR-CLOSURE-VARIABLES)
  (:SETTABLE-INSTANCE-VARIABLES BACKGROUND-TYPEOUT-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES BACKGROUND-TYPEOUT-STREAM))

(DEFFLAVOR TEMPORARY-MODE-LINE-MINI-BUFFER () (ZWEI-MINI-BUFFER))

(DEFMETHOD (TEMPORARY-MODE-LINE-MINI-BUFFER :TYPEOUT-WINDOW) ()
  (SEND TV:SUPERIOR ':BACKGROUND-TYPEOUT-STREAM))

(DEFVAR *TEMPORARY-MODE-LINE-WINDOW* :UNBOUND
  "Closure variable in the BACKGROUND-TYPEOUT-STREAM of a TEMPORARY-MODE-LINE-WINDOW.
Its value is the window the stream is associated with.")

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ BACKGROUND-TYPEOUT-STREAM (LET-CLOSED ((*TEMPORARY-MODE-LINE-WINDOW* SELF))
                                    'BACKGROUND-TYPEOUT-STREAM))
  (SETQ TV:DEEXPOSED-TYPEOUT-ACTION ':EXPOSE))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :TEMPORARY-EXPOSE) (&REST ARGS)
  (LEXPR-SEND SELF ':EXPOSE ARGS))

;;; Once the mini buffer decides it is time to pop up,
;;; it makes itself our selection substutute.  At that time,
;;; cause it to become actually selected.
(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AFTER :SET-SELECTION-SUBSTITUTE) (W)
  (IF W (SEND W ':SELECT)))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :TOP-OF-EDITOR-HIERARCHY) () SELF)

;;; Perhaps this should go someplace else
(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :MOVE-NEAR-WINDOW) (WINDOW &OPTIONAL (EXPOSE-P T))
  (SEND SELF ':SET-SUPERIOR (TV:SHEET-SUPERIOR WINDOW))
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW ':EDGES)
    ;; If it won't fit below try putting it above
    (AND (> (+ BOTTOM TV:HEIGHT) TV:(SHEET-INSIDE-BOTTOM SUPERIOR))
         (SETQ BOTTOM (MAX (- TOP TV:HEIGHT) TV:(SHEET-INSIDE-TOP SUPERIOR))))
    ;; Put it there
    (SEND SELF ':SET-EDGES LEFT BOTTOM RIGHT (+ BOTTOM TV:HEIGHT))
    (AND EXPOSE-P (SEND SELF ':EXPOSE))))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AFTER :REDISPLAY) (IGNORE &OPTIONAL FORCE)
  (AND FORCE (NOT TV:EXPOSED-P)
       (LET ((LEN (LOOP FOR STR IN PREVIOUS-MODE-LINE
                        WHEN (CONSP STR) DO (SETQ STR (SECOND STR))
                        SUM (TV:SHEET-STRING-LENGTH SELF STR))))
         (AND (> LEN (TV:SHEET-INSIDE-WIDTH))
              (SEND SELF ':SET-SIZE
                         (MIN (+ TV:LEFT-MARGIN-SIZE LEN TV:RIGHT-MARGIN-SIZE)
                              (TV:SHEET-INSIDE-WIDTH TV:SUPERIOR))
                         TV:HEIGHT))
         (AND (> (+ TV:X-OFFSET TV:WIDTH) (TV:SHEET-INSIDE-RIGHT TV:SUPERIOR))
              (SEND SELF ':SET-POSITION (- (TV:SHEET-INSIDE-RIGHT TV:SUPERIOR) TV:WIDTH)
                            TV:Y-OFFSET)))))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AFTER :DEACTIVATE) ()
  (WHEN BACKGROUND-TYPEOUT-WINDOW
    (SEND BACKGROUND-TYPEOUT-WINDOW ':DEACTIVATE)
    (SETQ BACKGROUND-TYPEOUT-WINDOW BACKGROUND-TYPEOUT-STREAM)))

(DEFFLAVOR TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS ()
           (TV:BORDERS-MIXIN TEMPORARY-MODE-LINE-WINDOW))

(DEFWINDOW-RESOURCE TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE ()
  :REUSABLE-WHEN :DEEXPOSED
  :MAKE-WINDOW (TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS)
  :INITIAL-COPIES 0)    ;Due to bootstrapping

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :AROUND :CALL-MINI-BUFFER-NEAR-WINDOW)
           (CONT MT ALL-ARGS &REST IGNORE)
  MT
  (LEXPR-SEND (WINDOW-EDITOR-CLOSURE MINI-BUFFER-WINDOW) CONT ALL-ARGS))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :CALL-MINI-BUFFER-NEAR-WINDOW)
           (WINDOW FUNCTION &REST ARGS
            &AUX OLD-SELECTED-WINDOW
            (*STANDARD-INPUT* SELF)
;           (*IO-BUFFER* TV:IO-BUFFER)
            (*TYPEOUT-WINDOW* BACKGROUND-TYPEOUT-STREAM)
            (*TERMINAL-IO* BACKGROUND-TYPEOUT-STREAM)
            (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
            (*TYPEIN-WINDOW* TYPEIN-WINDOW)
            (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
            (*MINI-BUFFER-WINDOW* MINI-BUFFER-WINDOW)
            (*MODE-LINE-WINDOW* SELF)
            (*MINI-BUFFER-DONT-RECORD* T))
  (COND ((EQ WINDOW ':MOUSE)
         (SEND SELF ':SET-SIZE 700 (TV:SHEET-HEIGHT SELF))
         (TV:EXPOSE-WINDOW-NEAR SELF '(:MOUSE) NIL NIL))
        (T
         (SEND SELF ':MOVE-NEAR-WINDOW WINDOW NIL)))
  (UNWIND-PROTECT
    (PROGN
      (SETQ OLD-SELECTED-WINDOW TV:SELECTED-WINDOW)
      ;; This is a crock.  If the user has managed to typeahead the whole thing, we can
      ;; avoid exposing the pop-up window.  Take hardware input and force it in.
      TV:(LET ((SELECTED-IO-BUFFER IO-BUFFER))
           (KBD-SNARF-INPUT IO-BUFFER))
      (LET ((*CURRENT-COMMAND* 'TEMPORARY-MINI-BUFFER))
        (CATCH 'TOP-LEVEL
          (APPLY FUNCTION ARGS))))
    (AND OLD-SELECTED-WINDOW (SEND OLD-SELECTED-WINDOW ':SELECT))
    (SEND SELF ':DEACTIVATE)
    (SEND (SEND SELF ':MINI-BUFFER-WINDOW) ':DEACTIVATE)))

(DEFMETHOD (TEMPORARY-MODE-LINE-WINDOW :EDITOR-WINDOWS) ()
  (IF (OR (EQ *WINDOW* MINI-BUFFER-WINDOW)
          (TV:SHEET-EXPOSED-P MINI-BUFFER-WINDOW))
      (LIST MINI-BUFFER-WINDOW)
    (IF (OR (EQ *WINDOW* SEARCH-MINI-BUFFER-WINDOW)
            (TV:SHEET-EXPOSED-P SEARCH-MINI-BUFFER-WINDOW))
        (LIST SEARCH-MINI-BUFFER-WINDOW))))

;;;; User interface functions that use the temporary-mode-line-window.

(DEFUN TYPEIN-LINE-READLINE-NEAR-WINDOW (WINDOW CTL-STRING &REST ARGS)
  (USING-RESOURCE (W TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE)
    (LEXPR-SEND W ':CALL-MINI-BUFFER-NEAR-WINDOW WINDOW
                  'TYPEIN-LINE-READLINE CTL-STRING ARGS)))

(DEFUN READ-DEFAULTED-PATHNAME-NEAR-WINDOW (WINDOW PROMPT
                                            &OPTIONAL (DEFAULTS (PATHNAME-DEFAULTS))
                                                      SPECIAL-TYPE)
  (USING-RESOURCE (W TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE)
    (SEND W ':CALL-MINI-BUFFER-NEAR-WINDOW WINDOW
            'READ-DEFAULTED-PATHNAME PROMPT DEFAULTS SPECIAL-TYPE)))

(DEFUN READ-BUFFER-NAME-NEAR-WINDOW (WINDOW PROMPT DEFAULT &OPTIONAL IMPOSSIBLE-IS-OK-P)
  (USING-RESOURCE (W TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE)
    (SEND W ':CALL-MINI-BUFFER-NEAR-WINDOW WINDOW
            'READ-BUFFER-NAME PROMPT DEFAULT IMPOSSIBLE-IS-OK-P
            (SEND *WINDOW* ':BUFFER-HISTORY))))

(DEFVAR *BACKGROUND-TYPEOUT-WHICH-OPERATIONS* NIL)

(DEFUN BACKGROUND-TYPEOUT-STREAM (OP &REST ARGS)
  (CASE OP
    (:WHICH-OPERATIONS
     (OR *BACKGROUND-TYPEOUT-WHICH-OPERATIONS*
         (USING-RESOURCE (WINDOW BACKGROUND-TYPEOUT-WINDOWS)
           (LET ((WO (SEND WINDOW ':WHICH-OPERATIONS)))
             (SETQ *BACKGROUND-TYPEOUT-WHICH-OPERATIONS*
                   (UNION '(:BEEP :NEVER-FLUSH-TYPEOUT :INHIBIT-OUTPUT-FOR-ABORT-P) WO)))))
     *BACKGROUND-TYPEOUT-WHICH-OPERATIONS*)
    (:SEND-IF-HANDLES
     (IF (MEMQ (FIRST ARGS) (BACKGROUND-TYPEOUT-STREAM :WHICH-OPERATIONS))
         (APPLY 'BACKGROUND-TYPEOUT-STREAM ARGS)))
    (:OPERATION-HANDLED-P
     (NOT (NULL (MEMQ (FIRST ARGS) (BACKGROUND-TYPEOUT-STREAM :WHICH-OPERATIONS)))))
    (:NEVER-FLUSH-TYPEOUT T)
    (:INHIBIT-OUTPUT-FOR-ABORT-P T)
    (:BEEP
     (LET ((W (WITHOUT-INTERRUPTS
                TV:(IF SELECTED-WINDOW
                       (SHEET-GET-SCREEN SELECTED-WINDOW)
                       DEFAULT-SCREEN))))
       (LEXPR-SEND W ':BEEP ARGS)))
    ((:EXPOSED-P :DEACTIVATE :DEEXPOSE :INCOMPLETE-P :MAKE-COMPLETE) NIL)
    (:SUPERIOR (TV:SHEET-SUPERIOR *TEMPORARY-MODE-LINE-WINDOW*))
    (OTHERWISE
     (SETQ *TERMINAL-IO* (ALLOCATE-RESOURCE 'BACKGROUND-TYPEOUT-WINDOWS
                                            (TV:SHEET-SUPERIOR *TEMPORARY-MODE-LINE-WINDOW*)))
     (SEND *TEMPORARY-MODE-LINE-WINDOW* ':SET-BACKGROUND-TYPEOUT-WINDOW *TERMINAL-IO*)
     (TV:SHEET-FORCE-ACCESS (*TERMINAL-IO* :NO-PREPARE)
       (SEND *TERMINAL-IO* :SET-LABEL
                           (STRING-APPEND (SEND *TEMPORARY-MODE-LINE-WINDOW* :NAME)
                                          " Background Typeout Window"))
       (SEND *TERMINAL-IO* :SET-PROCESS CURRENT-PROCESS)
       (SEND *TERMINAL-IO* :SET-IO-BUFFER (SEND *TEMPORARY-MODE-LINE-WINDOW* :IO-BUFFER))
       (SEND *TERMINAL-IO* :CLEAR-WINDOW))
     (LET ((OLD-SEL TV:SELECTED-WINDOW))
       (TV:WITH-SHEET-DEEXPOSED (*TEMPORARY-MODE-LINE-WINDOW*)
         (SEND *TEMPORARY-MODE-LINE-WINDOW* :MOVE-NEAR-WINDOW *TERMINAL-IO* NIL)
         (SEND *TERMINAL-IO* :EXPOSE))
       (AND OLD-SEL (SEND OLD-SEL :SELECT NIL)))
     (SETQ *TYPEOUT-WINDOW* *TERMINAL-IO*)
     (LEXPR-SEND *TERMINAL-IO* OP ARGS))))

(DEFFLAVOR BACKGROUND-TYPEOUT-WINDOW
        ()
        TV:(ANY-TYI-MIXIN BASIC-MOUSE-SENSITIVE-ITEMS TEMPORARY-WINDOW-MIXIN WINDOW))

(DEFMETHOD (BACKGROUND-TYPEOUT-WINDOW :NEVER-FLUSH-TYPEOUT) () T)

(DEFMETHOD (BACKGROUND-TYPEOUT-WINDOW :INCOMPLETE-P) () NIL)

(DEFMETHOD (BACKGROUND-TYPEOUT-WINDOW :MAKE-COMPLETE) () NIL)

(DEFWINDOW-RESOURCE BACKGROUND-TYPEOUT-WINDOWS ()
  :MAKE-WINDOW (BACKGROUND-TYPEOUT-WINDOW
                 :HEIGHT (TRUNCATE TV:(SHEET-HEIGHT DEFAULT-SCREEN) 3))
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

(DEFUN WINDOW-FRAME (WINDOW)
  (TV:SHEET-SUPERIOR WINDOW))

(DEFFLAVOR ZWEI-FRAME
        ((TV:IO-BUFFER NIL)
         EDITOR-CLOSURE
         (MODE-LINE-WINDOW 'MODE-LINE-WINDOW))
        (TV:INITIALLY-INVISIBLE-MIXIN TV:BORDERS-MIXIN
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

(DEFMETHOD (ZWEI-FRAME :BEFORE :INIT) (INIT-PLIST)
  (UNLESS (VARIABLE-BOUNDP EDITOR-CLOSURE)
    (LET ((*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
          (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
          (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
          (*COMTAB* (GET INIT-PLIST ':COMTAB))
          (*MODE-LINE-LIST* (GET INIT-PLIST ':MODE-LINE-LIST)))
      (SETQ EDITOR-CLOSURE
            (MAKE-EDITOR-CLOSURE (OR (GET INIT-PLIST ':EDITOR-CLOSURE-VARIABLES)
                                     TOP-LEVEL-EDITOR-CLOSURE-VARIABLES)
                                 NIL)))))

(DEFMETHOD (ZWEI-FRAME :AFTER :INIT) (INIT-PLIST)
  (WHEN (OR (SYMBOLP MODE-LINE-WINDOW) (CONSP MODE-LINE-WINDOW))
    (LET ((FLAVOR (IF (SYMBOLP MODE-LINE-WINDOW) MODE-LINE-WINDOW (CAR MODE-LINE-WINDOW)))
          (OPTIONS (IF (SYMBOLP MODE-LINE-WINDOW) NIL (CDR MODE-LINE-WINDOW))))
      (SETQ MODE-LINE-WINDOW
            (APPLY 'CREATE-MODE-LINE-WINDOW
                   (GET INIT-PLIST ':NUMBER-OF-MINI-BUFFER-LINES)
                   SELF FLAVOR TV:IO-BUFFER
                   ':BORDERS 1
                   ':BOTTOM (TV:SHEET-INSIDE-BOTTOM)
                   ':EXPOSE-P T
                   OPTIONS)
            TV:IO-BUFFER (SEND MODE-LINE-WINDOW ':IO-BUFFER)))
    (SEND (SEND MODE-LINE-WINDOW ':TYPEIN-WINDOW) ':EXPOSE)))

(DEFMETHOD (ZWEI-FRAME :NAME-FOR-SELECTION) ()
  (SEND (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*WINDOW*) ':NAME-FOR-SELECTION))

(DEFMETHOD (ZWEI-FRAME :EDIT) (&REST ARGS)
  (LEXPR-SEND (SEND SELF ':SELECTED-PANE) ':EDIT ARGS))

(DEFMETHOD (ZWEI-FRAME :CREATE-WINDOW) (FLAVOR &REST OPTIONS)
  (APPLY #'MAKE-INSTANCE FLAVOR
                        :SUPERIOR SELF
                        :BOTTOM (TV:SHEET-Y-OFFSET MODE-LINE-WINDOW)
                        :IO-BUFFER TV:IO-BUFFER
                        OPTIONS))

(DEFMETHOD (ZWEI-FRAME :EDITOR-WINDOW) ()
  (DO ((L TV:INFERIORS (CDR L)))
      ((NULL L) (FERROR "No inferiors"))
    (OR (EQ (CAR L) MODE-LINE-WINDOW)
        (RETURN (SEND (CAR L) :ZWEI-WINDOW)))))

(DEFMETHOD (ZWEI-FRAME :EDITOR-WINDOWS) ()
  (APPEND (SORT (REMQ MODE-LINE-WINDOW TV:EXPOSED-INFERIORS)
                (LAMBDA (W1 W2) (< (TV:SHEET-Y-OFFSET W1) (TV:SHEET-Y-OFFSET W2))))
          (SUBSET (LAMBDA (W) (TYPEP W 'DISPLAYER))
                  (IF (EQ MODE-LINE-WINDOW (SEND *WINDOW* :SUPERIOR))
                      (LIST *WINDOW*)
                    (SEND MODE-LINE-WINDOW ':EXPOSED-INFERIORS)))))

(DEFMETHOD (ZWEI-FRAME :SELECTABLE-WINDOWS) ()
  (LET ((SELECTABLE-WINDOWS NIL))
    (DOLIST (I TV:EXPOSED-INFERIORS)
      (OR (EQ I MODE-LINE-WINDOW)
          (LET ((STRING (SEND I :NAME-FOR-SELECTION)))
            (AND STRING
                 (PUSH (LIST STRING SELF) SELECTABLE-WINDOWS)))))
    (NREVERSE SELECTABLE-WINDOWS)))

(DEFMETHOD (ZWEI-FRAME :INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW) ()
  (VALUES (TV:SHEET-INSIDE-LEFT) (TV:SHEET-INSIDE-TOP) (TV:SHEET-INSIDE-RIGHT)
          (- (TV:SHEET-INSIDE-BOTTOM) (TV:SHEET-HEIGHT MODE-LINE-WINDOW))))

(DEFWRAPPER (ZWEI-FRAME :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  `(LET ((OLD-EXPOSED-INFERIORS (COPY-LIST TV:EXPOSED-INFERIORS)))
     (DECLARE (SPECIAL OLD-EXPOSED-INFERIORS))
     (MULTIPLE-VALUE-BIND (OLD-INSIDE-LEFT OLD-INSIDE-TOP
                           OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM)
         (SEND SELF :INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
       (DECLARE (SPECIAL OLD-INSIDE-LEFT OLD-INSIDE-TOP
                         OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM))
       . ,BODY)))

(DEFMETHOD (ZWEI-FRAME :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE &AUX OLD-INSIDE-WIDTH OLD-INSIDE-HEIGHT
                              NEW-INSIDE-LEFT NEW-INSIDE-TOP
                              NEW-INSIDE-RIGHT NEW-INSIDE-BOTTOM
                              NEW-INSIDE-WIDTH NEW-INSIDE-HEIGHT)
  (DECLARE (SPECIAL OLD-EXPOSED-INFERIORS
                    OLD-INSIDE-LEFT OLD-INSIDE-TOP
                    OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM))
  (SETQ OLD-INSIDE-WIDTH (- OLD-INSIDE-RIGHT OLD-INSIDE-LEFT)
        OLD-INSIDE-HEIGHT (- OLD-INSIDE-BOTTOM OLD-INSIDE-TOP))
  (MULTIPLE-VALUE (NEW-INSIDE-LEFT NEW-INSIDE-TOP NEW-INSIDE-RIGHT NEW-INSIDE-BOTTOM)
    (SEND SELF ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (SETQ NEW-INSIDE-WIDTH (- NEW-INSIDE-RIGHT NEW-INSIDE-LEFT)
        NEW-INSIDE-HEIGHT (- NEW-INSIDE-BOTTOM NEW-INSIDE-TOP))
  (TV:WITH-SHEET-DEEXPOSED (SELF)
    (DO ((WL (COPY-LIST TV:INFERIORS) (CDR WL))
         (WINDOW)
         (OLD-LEFT) (OLD-TOP) (OLD-RIGHT) (OLD-BOTTOM)
         (NEW-LEFT) (NEW-TOP) (NEW-RIGHT) (NEW-BOTTOM))
        ((NULL WL))
      (SETQ WINDOW (CAR WL))
      (MULTIPLE-VALUE (OLD-LEFT OLD-TOP OLD-RIGHT OLD-BOTTOM)
        (SEND WINDOW ':EDGES))
      (IF (EQ WINDOW MODE-LINE-WINDOW)
          (SETQ NEW-LEFT NEW-INSIDE-LEFT
                NEW-TOP NEW-INSIDE-BOTTOM
                NEW-RIGHT NEW-INSIDE-RIGHT
                NEW-BOTTOM (+ NEW-INSIDE-BOTTOM (- OLD-BOTTOM OLD-TOP)))
        (SETQ NEW-LEFT (IF (= OLD-LEFT OLD-INSIDE-LEFT) NEW-INSIDE-LEFT
                         (TRUNCATE (* OLD-LEFT NEW-INSIDE-WIDTH) OLD-INSIDE-WIDTH))
              NEW-TOP (IF (= OLD-TOP OLD-INSIDE-TOP) NEW-INSIDE-TOP
                        (TRUNCATE (* OLD-TOP NEW-INSIDE-HEIGHT) OLD-INSIDE-HEIGHT))
              NEW-RIGHT (IF (= OLD-RIGHT OLD-INSIDE-RIGHT) NEW-INSIDE-RIGHT
                          (TRUNCATE (* OLD-RIGHT NEW-INSIDE-WIDTH) OLD-INSIDE-WIDTH))
              NEW-BOTTOM (IF (= OLD-BOTTOM OLD-INSIDE-BOTTOM) NEW-INSIDE-BOTTOM
                           (TRUNCATE (* OLD-BOTTOM NEW-INSIDE-HEIGHT)
                                     OLD-INSIDE-HEIGHT))))
      (SEND WINDOW ':SET-EDGES NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
      (AND (MEMQ WINDOW OLD-EXPOSED-INFERIORS)
           (SEND WINDOW ':EXPOSE)))))

(DEFWRAPPER (ZWEI-FRAME :CHANGE-OF-DEFAULT-FONT) (IGNORE . BODY)
  `(LET ((OLD-EXPOSED-INFERIORS (COPYLIST TV:EXPOSED-INFERIORS))
         (OLD-MODE-LINE-NLINES (SEND MODE-LINE-WINDOW ':NLINES)))
     (DECLARE (SPECIAL OLD-EXPOSED-INFERIORS OLD-MODE-LINE-NLINES))
     (MULTIPLE-VALUE-BIND (OLD-INSIDE-LEFT OLD-INSIDE-TOP
                           OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM)
         (SEND SELF ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
       (DECLARE (SPECIAL OLD-INSIDE-LEFT OLD-INSIDE-TOP
                         OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM))
       . ,BODY)))

(DEFMETHOD (ZWEI-FRAME :AFTER :CHANGE-OF-DEFAULT-FONT)
           (IGNORE IGNORE &AUX OLD-INSIDE-WIDTH OLD-INSIDE-HEIGHT
                              NEW-INSIDE-LEFT NEW-INSIDE-TOP
                              NEW-INSIDE-RIGHT NEW-INSIDE-BOTTOM
                              NEW-INSIDE-WIDTH NEW-INSIDE-HEIGHT)
  (DECLARE (SPECIAL OLD-EXPOSED-INFERIORS OLD-MODE-LINE-NLINES
                    OLD-INSIDE-LEFT OLD-INSIDE-TOP
                    OLD-INSIDE-RIGHT OLD-INSIDE-BOTTOM))
  (SETQ OLD-INSIDE-WIDTH (- OLD-INSIDE-RIGHT OLD-INSIDE-LEFT)
        OLD-INSIDE-HEIGHT (- OLD-INSIDE-BOTTOM OLD-INSIDE-TOP))
  (SEND MODE-LINE-WINDOW ':SET-NLINES OLD-MODE-LINE-NLINES)
  (MULTIPLE-VALUE (NEW-INSIDE-LEFT NEW-INSIDE-TOP NEW-INSIDE-RIGHT NEW-INSIDE-BOTTOM)
    (SEND SELF ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (SETQ NEW-INSIDE-WIDTH (- NEW-INSIDE-RIGHT NEW-INSIDE-LEFT)
        NEW-INSIDE-HEIGHT (- NEW-INSIDE-BOTTOM NEW-INSIDE-TOP))
  (TV:WITH-SHEET-DEEXPOSED (SELF)
    (DO ((WL (COPYLIST TV:INFERIORS) (CDR WL))
         (WINDOW)
         (OLD-LEFT) (OLD-TOP) (OLD-RIGHT) (OLD-BOTTOM)
         (NEW-LEFT) (NEW-TOP) (NEW-RIGHT) (NEW-BOTTOM))
        ((NULL WL))
      (SETQ WINDOW (CAR WL))
      (MULTIPLE-VALUE (OLD-LEFT OLD-TOP OLD-RIGHT OLD-BOTTOM)
        (SEND WINDOW ':EDGES))
      (IF (EQ WINDOW MODE-LINE-WINDOW)
          (SETQ NEW-LEFT NEW-INSIDE-LEFT
                NEW-TOP NEW-INSIDE-BOTTOM
                NEW-RIGHT NEW-INSIDE-RIGHT
                NEW-BOTTOM (TV:SHEET-INSIDE-BOTTOM))
          (SETQ NEW-LEFT (IF (= OLD-LEFT OLD-INSIDE-LEFT) NEW-INSIDE-LEFT
                             (TRUNCATE (* OLD-LEFT NEW-INSIDE-WIDTH) OLD-INSIDE-WIDTH))
                NEW-TOP (IF (= OLD-TOP OLD-INSIDE-TOP) NEW-INSIDE-TOP
                            (TRUNCATE (* OLD-TOP NEW-INSIDE-HEIGHT) OLD-INSIDE-HEIGHT))
                NEW-RIGHT (IF (= OLD-RIGHT OLD-INSIDE-RIGHT) NEW-INSIDE-RIGHT
                              (TRUNCATE (* OLD-RIGHT NEW-INSIDE-WIDTH) OLD-INSIDE-WIDTH))
                NEW-BOTTOM (IF (= OLD-BOTTOM OLD-INSIDE-BOTTOM) NEW-INSIDE-BOTTOM
                               (TRUNCATE (* OLD-BOTTOM NEW-INSIDE-HEIGHT)
                                         OLD-INSIDE-HEIGHT))))
      (SEND WINDOW ':SET-EDGES NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
      (AND (MEMQ WINDOW OLD-EXPOSED-INFERIORS)
           (SEND WINDOW ':EXPOSE)))))

(DEFFLAVOR ZWEI-PANE-MIXIN () ()
  (:REQUIRED-FLAVORS ZWEI)
  (:DOCUMENTATION "ZWEI windows which are panes of ZWEI-FRAME frames."))

(DEFMETHOD (ZWEI-PANE-MIXIN :MODE-LINE-WINDOW) ()
  (SEND TV:SUPERIOR ':MODE-LINE-WINDOW))

(DEFMETHOD (ZWEI-PANE-MIXIN :TOP-OF-EDITOR-HIERARCHY) ()
  TV:SUPERIOR)

(DEFMETHOD (ZWEI-PANE-MIXIN :EDITOR-WINDOWS) ()
  (OR (SEND TV:SUPERIOR ':SEND-IF-HANDLES ':EDITOR-WINDOWS)
      (LIST SELF)))

(DEFMETHOD (ZWEI-PANE-MIXIN :BEFORE :INIT) (IGNORE)
  (LET ((TEM (SEND TV:SUPERIOR ':SEND-IF-HANDLES ':EDITOR-CLOSURE)))
    (IF TEM (SETQ EDITOR-CLOSURE TEM))))

(DEFFLAVOR ZWEI-WINDOW-PANE () (ZWEI-PANE-MIXIN TOP-LEVEL-DISPLAYER-MIXIN ZWEI-WINDOW))

;;; Use this in windows that are going to use the macro stream (such as ZMACS windows).
(DEFFLAVOR ZWEI-MACRO-MIXIN () ())

(DEFMETHOD (ZWEI-MACRO-MIXIN :MACRO-TERMINATE) ()
  (LET ((PT (POINT)))
    (OR (BP-= PT (INTERVAL-FIRST-BP *INTERVAL*))
        (BP-= PT (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFMETHOD (ZWEI-MACRO-MIXIN :SET-MACRO-LEVEL) (LEVEL)
  (SETQ *MACRO-LEVEL* LEVEL)
  (REDISPLAY-MODE-LINE))

(DEFMETHOD (ZWEI-MACRO-MIXIN :READ-MACRO-LINE) (PROMPT
                                                &AUX (*CURRENT-COMMAND* 'READ-MACRO-LINE))
  (TYPEIN-LINE-READLINE PROMPT))

;;;; ZMACS windows and panes and frames.

(DEFFLAVOR ZMACS-WINDOW
        ((BUFFER-HISTORY (MAKE-HISTORY "" :CLEAR-P NIL)))
        (TV:PROCESS-MIXIN TOP-LEVEL-DISPLAYER-MIXIN ZWEI-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES BUFFER-HISTORY)
  (:DEFAULT-INIT-PLIST :ITEM-TYPE-ALIST *TYPEOUT-COMMAND-ALIST* :INTERVAL 'ZMACS-BUFFER)
  :ABSTRACT-FLAVOR
  (:DOCUMENTATION "A window which can select any ZMACS buffer to edit."))

(DEFMETHOD (ZMACS-WINDOW :NAME-FOR-SELECTION) ()
;  (AND INTERVAL        ;Can be NIL if has never been used
;Should not be possible now.
  (FORMAT NIL "Edit: ~A" (BUFFER-NAME INTERVAL)))

(DEFMETHOD (ZMACS-WINDOW :INFERIOR-EXPOSE) (SHEET)
  ;; Each time the typeout window is exposed, update its command list.
  (IF (EQ SHEET TV:TYPEOUT-WINDOW)
      (SEND TV:TYPEOUT-WINDOW ':SET-ITEM-TYPE-ALIST *TYPEOUT-COMMAND-ALIST*))
  ;; Do let the inferior expose itself.
  SHEET)

(DEFMETHOD (ZMACS-WINDOW :BEFORE :INIT) (IGNORE)
  (OR (MEMQ SELF *ALL-ZMACS-WINDOWS*)
      (PUSH SELF *ALL-ZMACS-WINDOWS*))
  (COMTAB-MOUSE-PROMPT *ZMACS-COMTAB* MOUSE-DOCUMENTATION-STRING))

(DEFMETHOD (ZMACS-WINDOW :AFTER :INIT) (IGNORE)
  (SETF (HISTORY-NAME BUFFER-HISTORY) (STRING-APPEND "buffer selection history for "
                                                     TV:NAME)
        (HISTORY-LIST BUFFER-HISTORY) (COPYLIST *ZMACS-BUFFER-LIST*)
        (HISTORY-LENGTH BUFFER-HISTORY) (LENGTH *ZMACS-BUFFER-LIST*))
  (PUSH-REMOVE-ON-HISTORY INTERVAL BUFFER-HISTORY))

(DEFMETHOD (ZMACS-WINDOW :BEFORE :KILL) ()
  (SETQ *ALL-ZMACS-WINDOWS*
        (DELQ SELF *ALL-ZMACS-WINDOWS*)))

(DEFMETHOD (ZMACS-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (COMMAND-BUFFER-PUSH '(CONFIGURATION-CHANGED)))

;(DEFVAR *PACKAGE-RECURSION* NIL)
;(DEFMETHOD (ZMACS-WINDOW :AFTER :SELECT) (&REST IGNORE)
;  (OR *PACKAGE-RECURSION*
;      (LET ((*PACKAGE-RECURSION* T))
;       (FUNCALL EDITOR-CLOSURE
;                #'(LAMBDA (OUTSIDE-PACKAGE)
;                    (SETQ PACKAGE OUTSIDE-PACKAGE)
;                    (COMPUTE-BUFFER-PACKAGE *INTERVAL*))
;                *PACKAGE*))))


(DEFWRAPPER (ZMACS-WINDOW :MOUSE-CLICK) ((BUTTON) . BODY)
  `(PROG1 (PROGN . ,BODY)
          (AND ( BUTTON #/MOUSE-3-2)
               (SYMEVAL-IN-STACK-GROUP '*INSIDE-BREAK* (PROCESS-STACK-GROUP TV:PROCESS))
               (SEND TV:PROCESS ':RESET))))

(DEFMETHOD (ZMACS-WINDOW :RECOMPUTE-LABEL) ()
  (SEND SELF ':CHANGE-LABEL (BUFFER-NAME INTERVAL)))

(DEFFLAVOR ZMACS-WINDOW-PANE () (ZWEI-PANE-MIXIN ZMACS-WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS NIL))

(DEFMETHOD (ZMACS-WINDOW-PANE :CHANGE-LABEL) (NEW-LABEL)
  (SEND TV:SUPERIOR ':CHANGE-PANE-LABEL SELF NEW-LABEL))

(DEFMETHOD (ZMACS-WINDOW-PANE :AFTER :INIT) (IGNORE)
  (SETQ TV:PROCESS (SEND TV:SUPERIOR ':PROCESS)))

(DEFFLAVOR ZMACS-FRAME
           ((STANDARD-INPUT-FOR-PANES NIL))
           (TV:PROCESS-MIXIN TV:STREAM-MIXIN ZWEI-FRAME
            ZWEI-MACRO-MIXIN)
  :SETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :MODE-LINE-WINDOW '(MODE-LINE-WINDOW
                                            :RECALCULATE-FUNCTION
                                            ZMACS-MODE-LINE-RECALCULATE-FUNCTION)
                       :SAVE-BITS T))

(DEFMETHOD (ZMACS-FRAME :BEFORE :INIT) (PLIST)
  (SETQ STANDARD-INPUT-FOR-PANES (MAKE-MACRO-STREAM SELF))
  (LET ((*STANDARD-INPUT* STANDARD-INPUT-FOR-PANES)
        (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
        (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
        (*COMTAB* *ZMACS-COMTAB*)
        (*MODE-LINE-LIST* '("ZMACS " "(" *MODE-NAME-LIST*
                            ") " *BUFFER-MODIFIED-P* *MORE-ABOVE-BELOW*
                            *ZMACS-BUFFER-NAME* *ZMACS-BUFFER-VERSION-STRING*
                            (*FONT-NAME* "  Font: " *FONT-NAME*)
                            (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*))))
    (or (get plist ':width tv:width)
        (setq tv:width (send (get plist ':superior tv:superior) :inside-width)))
    (SETQ EDITOR-CLOSURE
          (MAKE-EDITOR-CLOSURE ZMACS-TOP-LEVEL-EDITOR-CLOSURE-VARIABLES NIL))))

(DEFMETHOD (ZMACS-FRAME :AFTER :INIT) (IGNORE)
  (SETQ TV:PROCESS (MAKE-PROCESS TV:NAME
                                 ':FLAVOR 'SI:COROUTINING-PROCESS
                                 ':INITIAL-FORM `(ZMACS-WINDOW-TOP-LEVEL ,SELF)
                                 ':REGULAR-PDL-SIZE #o40000
                                 ':SPECIAL-PDL-SIZE #o10000))
  (SEND TV:PROCESS :RESET)
  (SEND SELF ':SELECT-PANE
        (SEND SELF ':CREATE-WINDOW 'ZMACS-WINDOW-PANE
                                   ':ACTIVATE-P T ':LABEL NIL)))

(DEFUN ZMACS-MODE-LINE-RECALCULATE-FUNCTION (&AUX INT-TICK)
  (SETQ INT-TICK (NODE-TICK *INTERVAL*))
  (SETQ *BUFFER-MODIFIED-P* (COND ((BUFFER-READ-ONLY-P *INTERVAL*) "(RO) ")
                                  ((BUFFER-MODIFIED-P *INTERVAL*) "* ")
                                  (T NIL))))

;;; This is the top-level function for *ZMACS-WINDOW-PROCESS*.
(DEFUN ZMACS-WINDOW-TOP-LEVEL (FRAME)
  (SEND (SEND FRAME ':STANDARD-INPUT-FOR-PANES) ':MACRO-ERROR)  ;Halt runaway keyboard macro
  (DO-FOREVER
    (SEND FRAME ':AWAIT-EXPOSURE)
    (SEND (SEND FRAME ':EDITOR-WINDOW) ':EDIT)
    (TV:DESELECT-AND-MAYBE-BURY-WINDOW FRAME ':FIRST)))

;;; Make sure we redisplay when exposed, in case we are displaying
;;; a buffer that was modified in some other process.
;;; Also make sure we have noticed this user's global value of BASE.
(DEFMETHOD (ZMACS-FRAME :BEFORE :EXPOSE) (&REST IGNORE)
  (INITIALIZE-DEFAULT-BASE)
  (UNLESS (TV:SHEET-EXPOSED-P SELF)
    (SEND SELF ':FORCE-KBD-INPUT '(REDISPLAY))))

(DEFUN INITIALIZE-DEFAULT-BASE ()
  "Set *DEFAULT-BASE* if this is first entry to ZMACS since logging in.
*DEFAULT-BASE* is set from the global value of *READ-BASE*,
in case user's init file has set that."
  (DECLARE (:SELF-FLAVOR ZMACS-FRAME))
  (UNLESS *DEFAULT-BASE*
    (SETQ-GLOBALLY *DEFAULT-BASE* *READ-BASE*))
  (UNLESS *DEFAULT-READTABLE*
    (SETQ-GLOBALLY *DEFAULT-READTABLE* *READTABLE*))
  ;; Make sure *READ-BASE* and *PRINT-BASE* are not still NIL in this ZMACS window.
  (UNLESS (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*READ-BASE*)
    (FUNCALL EDITOR-CLOSURE #'(LAMBDA () (SETQ *PRINT-BASE*
                                               (SETQ *READ-BASE* *DEFAULT-BASE*)))))
  (UNLESS (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*READTABLE*)
    (FUNCALL EDITOR-CLOSURE #'(LAMBDA () (SETQ *READTABLE* *DEFAULT-READTABLE*)))))

(DEFMETHOD (ZMACS-FRAME :CHANGE-PANE-LABEL) (PANE NEW-LABEL)
  (AND ( (LENGTH TV:EXPOSED-INFERIORS) 3)
       (SEND PANE ':DELAYED-SET-LABEL NEW-LABEL)))

(DEFMETHOD (ZMACS-FRAME :UPDATE-LABELS) ()
  (IF ( (LENGTH TV:EXPOSED-INFERIORS) 3)
      (DOLIST (W TV:EXPOSED-INFERIORS)
        (OR (EQ W MODE-LINE-WINDOW)
            (SEND W ':DELAYED-SET-LABEL (BUFFER-NAME (WINDOW-INTERVAL W)))))
    (DOLIST (W TV:EXPOSED-INFERIORS)
      (OR (EQ W MODE-LINE-WINDOW)
          (SEND W ':SET-LABEL NIL)))))

(DEFMETHOD (ZMACS-FRAME :TWO-EDITOR-WINDOWS) ()
  (DO ((L TV:INFERIORS (CDR L))
       (WINDOW)
       (TOP-WINDOW) (BOTTOM-WINDOW))
      ((NULL L)
       (VALUES TOP-WINDOW
               (SEND SELF ':CREATE-WINDOW 'ZMACS-WINDOW-PANE
                                          ':INTERVAL (OR (PREVIOUS-BUFFER)
                                                         'ZMACS-BUFFER))))
    (COND ((EQ (SETQ WINDOW (CAR L)) MODE-LINE-WINDOW))
          ((NULL TOP-WINDOW)
           (SETQ TOP-WINDOW WINDOW))
          (T
           (SETQ BOTTOM-WINDOW WINDOW)
           (AND (TV:SHEET-EXPOSED-P BOTTOM-WINDOW)
                (< (TV:SHEET-Y-OFFSET BOTTOM-WINDOW) (TV:SHEET-Y-OFFSET TOP-WINDOW))
                (PSETQ TOP-WINDOW BOTTOM-WINDOW BOTTOM-WINDOW TOP-WINDOW))
           (RETURN (VALUES TOP-WINDOW BOTTOM-WINDOW))))))

(DEFMETHOD (ZMACS-FRAME :N-EDITOR-WINDOWS) (N &AUX LIST)
  (DO ((L TV:INFERIORS (CDR L))
       (I 0 (1+ I)))
      ((OR (NULL L) ( I N)))
    (OR (EQ (CAR L) MODE-LINE-WINDOW)
        (PUSH (SEND (CAR L) ':ZWEI-WINDOW) LIST)))
  (DOTIMES (I (- N (LENGTH LIST)))
    (PUSH (SEND SELF ':CREATE-WINDOW 'ZMACS-WINDOW-PANE) LIST))
  LIST)

(DEFMETHOD (ZMACS-FRAME :PANE-TYPES-ALIST) ()
  '(("Edit" . ZMACS-WINDOW-PANE)))

;;;; ZMACS Multiple-window commands.

(DEFCOM COM-TWO-WINDOWS "Select two windows.
Split the frame into two editor windows and select the bottom one.
With a numeric argument, make the second window point to the same
buffer that the first window does." ()
  (SWITCH-WINDOWS *NUMERIC-ARG-P* 2)
  DIS-NONE)

;; Copied from LAD: RELEASE-3.ZWEI; SCREEN.LISP#480 on 26-Mar-87 18:37:58
(DEFCOM COM-VIEW-TWO-WINDOWS "Select two windows, but stay in the first one.
Split the frame into two editor windows and select the top one.
With a numeric argument, make the second window point to the same
buffer that the first window does.
If the screen is already in two window mode, this selects the other window." ()
  (SWITCH-WINDOWS *NUMERIC-ARG-P* 1)
  DIS-NONE)

;;; Note that this is designed to ask the question before doing any display
(DEFCOM COM-MODIFIED-TWO-WINDOWS "Find a buffer, file or tag in the other window." ()
  (LET (CHAR)
    (FORMAT *QUERY-IO* "~&Buffer, File, Jump or Tag (B, F, J or T): ")
    (DO ()
        ((NEQ (SETQ CHAR (CHAR-UPCASE
                           (CHAR-CODE
                             (TYPEIN-LINE-ACTIVATE
                               (SEND *STANDARD-INPUT* ':TYI)))))
              #/HELP))
      (SEND *QUERY-IO* :CLEAR-WINDOW)
      (FORMAT *QUERY-IO* "~&Four ways to specify the buffer to display in the other window:
B and a buffer name, F and a file name, J and a register name,
or T or Period and the name of a section (as in Meta-Period).  B, F, J or T: "))
    (CASE CHAR
      (#/B (LET ((BUFFER (READ-BUFFER-NAME "Select buffer:" T 'MAYBE)))
             (SWITCH-WINDOWS)
             (MAKE-BUFFER-CURRENT BUFFER)
             DIS-TEXT))
      (#/F (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
                                                    NIL NIL ':NEW-OK)))
             (SWITCH-WINDOWS)
             (FIND-FILE PATHNAME))
           (MAYBE-DISPLAY-DIRECTORY ':READ)
           DIS-TEXT)
      (#/J
       (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
         (LET ((PT (GET Q-REG 'POINT)))
           (COND ((NULL PT)
                  (BARF "The register ~A doesn't point anywhere." Q-REG)))
           (SWITCH-WINDOWS)
           (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
           (MAKE-BUFFER-CURRENT (CDR PT))
           (MOVE-BP (POINT) (CAR PT))))
       DIS-BPS)
      ((#/T #/.)
       (COND (*NUMERIC-ARG-P* (SWITCH-WINDOWS)
                              (EDIT-NEXT-DEFINITION))
             (T (LET ((SPEC
                        (READ-FUNCTION-NAME "Edit function" (RELEVANT-FUNCTION-NAME (POINT))
                                            'AARRAY-OK)))
                  (SWITCH-WINDOWS)
                  (EDIT-DEFINITION SPEC))))
       DIS-NONE)
      (T (SEND *QUERY-IO* ':MAKE-COMPLETE)
         (BARF)))))

(DEFUN SWITCH-WINDOWS (&OPTIONAL CHANGE-INTERVAL (ONE-TO-SELECT 2))
  (MULTIPLE-VALUE-bind (TOP-WINDOW BOTTOM-WINDOW) (SEND (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS)
    (when CHANGE-INTERVAL
      (SEND BOTTOM-WINDOW ':SET-INTERVAL *INTERVAL*)
      (MOVE-BP (WINDOW-POINT BOTTOM-WINDOW) (POINT)))
    (LET ((FEW (FRAME-EXPOSED-WINDOWS)))
      (COND ((AND (MEMQ TOP-WINDOW FEW) (MEMQ BOTTOM-WINDOW FEW))
             (COM-OTHER-WINDOW))
            (T
             (TWO-WINDOWS TOP-WINDOW BOTTOM-WINDOW)
             (let ((haec top-window)     ; Latin demonstrative feminine singular nominative
                   (illa bottom-window)) ; pronouns (fenestra)
               (when (= one-to-select 2) (swapf haec illa))
               (if (eq haec *window*) (select-window haec)
                 (make-window-current haec))
               (send illa :redisplay :point nil nil t)))))))

;;; Two window stuff, takes two windows (structures, not sheets)
;;; and makes them share the area originally occupied by the first of the two.
(DEFUN TWO-WINDOWS (ZWEI-WINDOW-1 ZWEI-WINDOW-2)
  (REDISPLAY ZWEI-WINDOW-1 ':NONE)
  (LET ((W1 (WINDOW-SHEET ZWEI-WINDOW-1))
        (W2 (WINDOW-SHEET ZWEI-WINDOW-2))
        (FRAME (WINDOW-FRAME ZWEI-WINDOW-1)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
      (TV:PRESERVE-SUBSTITUTE-STATUS (SEND W1 ':SUPERIOR)
        (TV:DELAYING-SCREEN-MANAGEMENT
          (SEND W1 ':DEEXPOSE)
          (SEND W2 ':DEEXPOSE)
          (LET ((HEIGHT (TRUNCATE (- BOTTOM TOP) 2)))
            (SEND W1 ':SET-EDGES LEFT TOP RIGHT (+ TOP HEIGHT))
            (SEND W2 ':SET-EDGES LEFT (+ TOP HEIGHT) RIGHT BOTTOM))
          (SEND W1 ':SET-LABEL NIL)
          (SEND W2 ':SET-LABEL NIL)
          (SEND W1 ':EXPOSE NIL ':CLEAN)                ;Make sure they are both there
          (SEND W2 ':EXPOSE NIL ':CLEAN))))
    (SEND FRAME ':UPDATE-LABELS)))

(DEFCOM COM-ONE-WINDOW "Go back to one-window mode.
Causes one of the editor windows in this frame to fill the whole frame.
With a numeric arg, the current window (where the cursor is) is always used.
Otherwise, it is controlled by the value of ZWEI:*ONE-WINDOW-DEFAULT*.
It can be :TOP (select the uppermost window), :BOTTOM (the lowermost),
:CURRENT (keep only the window the cursor is now in),
or :OTHER (keep the uppermost window other than the one the cursor is in).
The default is :CURRENT.
Exception: if there is no arg, windows in Warnings mode will never be used." ()
  (LET ((WINDOWS (FRAME-EXPOSED-WINDOWS))
        WINDOW)
    (UNLESS (CDR WINDOWS) (BARF "You are already in one window mode."))
    (SETQ WINDOW
          (IF *NUMERIC-ARG-P* *WINDOW*
            (SEND
              (SELECTQ *ONE-WINDOW-DEFAULT*
                (:TOP (FIRST WINDOWS))
                (:BOTTOM (CAR (LAST WINDOWS)))
                (:CURRENT (WINDOW-SHEET *WINDOW*))
                (:OTHER (CAR (MEM 'NEQ (WINDOW-SHEET *WINDOW*) WINDOWS))))
              ':ZWEI-WINDOW)))
    ;; Don't default to a window that is in Warnings mode.
    (AND (NOT *NUMERIC-ARG-P*)
         (EQ (BUFFER-SAVED-MAJOR-MODE (WINDOW-INTERVAL *WINDOW*)) 'WARNINGS-MODE)
         (SETQ WINDOW
               (SEND (CAR (MEM 'NEQ (WINDOW-SHEET *WINDOW*) WINDOWS)) ':ZWEI-WINDOW)))
    (MAKE-WINDOW-FULL-SCREEN WINDOW))
  DIS-NONE)

(DEFCOM COM-OTHER-WINDOW "Move to the other window.
If there are several windows, go through them all in cyclic order.
A numeric argument specifies the window to go to, counting from 1 at the top." ()
  (LET ((WINDOW (OTHER-WINDOW)))
    (IF *NUMERIC-ARG-P*
        (SELECT-NUMBERED-WINDOW *NUMERIC-ARG*)
      (IF WINDOW
          (MAKE-WINDOW-CURRENT WINDOW)
        (MULTIPLE-VALUE (NIL WINDOW)
          (SEND (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS))
        (OR (WINDOW-INTERVAL WINDOW) (BARF "Only one window"))
        (MAKE-WINDOW-FULL-SCREEN WINDOW))))
  DIS-BPS)

(DEFUN MAKE-WINDOW-FULL-SCREEN (WINDOW &AUX FRAME LEFT TOP RIGHT BOTTOM)
  (SETQ FRAME (WINDOW-FRAME WINDOW))
  (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
    (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (TV:PRESERVE-SUBSTITUTE-STATUS FRAME
    (TV:DELAYING-SCREEN-MANAGEMENT
      (SEND WINDOW ':DEEXPOSE)
      (SEND WINDOW ':SET-EDGES LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW ':SET-LABEL NIL)
      (SEND WINDOW ':EXPOSE NIL ':CLEAN)
      (SEND FRAME ':UPDATE-LABELS)
      (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
      (OR (EQ WINDOW *WINDOW*) (MAKE-WINDOW-CURRENT WINDOW)))))

(DEFUN SELECT-NUMBERED-WINDOW (NUMBER)
  "Select the NUMBERth window from the top, of those in this frame."
  (LET ((WINDOWS (FRAME-EXPOSED-WINDOWS)))
    (IF (< NUMBER 0) (SETQ NUMBER (- (LENGTH WINDOWS) NUMBER)))
    (COND ((= NUMBER 0) (BEEP))
          ((NTH (1- NUMBER) WINDOWS)
           (MAKE-WINDOW-CURRENT (SEND (NTH (1- NUMBER) WINDOWS) ':ZWEI-WINDOW)))
          (T (BEEP)))))

(DEFCOM COM-SCROLL-OTHER-WINDOW "Scroll other window up several lines.
Specify the number as a numeric argument, negative for down.
The default is a whole screenful up." (KM)
  (LET ((WINDOW (OTHER-WINDOW)))
    (OR WINDOW (BARF "There is only one window"))
    (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
    (RECENTER-WINDOW-RELATIVE WINDOW (IF (MEMQ *NUMERIC-ARG-P* '(:DIGITS :CONTROL-U))
                                         *NUMERIC-ARG*
                                         (* (1- (WINDOW-N-PLINES WINDOW)) *NUMERIC-ARG*))))
  DIS-NONE)

(DEFCOM COM-GROW-WINDOW "Make this window larger by argument number of lines." (KM)
  (LET ((WINDOWS (MAPCAR 'FUNCALL (FRAME-EXPOSED-WINDOWS) (CIRCULAR-LIST ':ZWEI-WINDOW))))
    (OR (CDR WINDOWS) (BARF "There is only one window"))
    (LET (FIRST-WINDOW SECOND-WINDOW FIRST-WINDOW-GROWTH)
      (IF (EQ *WINDOW* (CAR (LAST WINDOWS)))
          (SETQ FIRST-WINDOW (SECOND (NREVERSE WINDOWS))
                SECOND-WINDOW *WINDOW*
                FIRST-WINDOW-GROWTH (- *NUMERIC-ARG*))
        (SETQ FIRST-WINDOW *WINDOW*
              SECOND-WINDOW (CADR (MEMQ *WINDOW* WINDOWS))
              FIRST-WINDOW-GROWTH *NUMERIC-ARG*))
      (PREPARE-WINDOW-FOR-REDISPLAY FIRST-WINDOW)
      (PREPARE-WINDOW-FOR-REDISPLAY SECOND-WINDOW)
      (GROW-WINDOW FIRST-WINDOW SECOND-WINDOW FIRST-WINDOW-GROWTH)))
  DIS-NONE)

;;; Grow a window, shrinking the other one
(DEFUN GROW-WINDOW (ZWEI-WINDOW-1 ZWEI-WINDOW-2 NLINES)
  (LET ((W1 (WINDOW-SHEET ZWEI-WINDOW-1))
        (W2 (WINDOW-SHEET ZWEI-WINDOW-2)))
    (LET ((HEIGHT (* NLINES (TV:SHEET-LINE-HEIGHT W1)))
          LEFT TOP RIGHT BOTTOM BOTTOM1 TOP2)
      (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM1)
          (SEND W1 ':EDGES))
      (MULTIPLE-VALUE (NIL TOP2 NIL BOTTOM)
        (SEND W2 ':EDGES))
      (SETQ BOTTOM1 (+ BOTTOM1 HEIGHT))
      (SETQ TOP2 (+ TOP2 HEIGHT))
      (AND (OR (< BOTTOM1 TOP) (> BOTTOM1 BOTTOM)
               (< TOP2 TOP) (> TOP2 BOTTOM))
           (BARF))
      (COND ((> HEIGHT 0)
             (SEND W2 ':SET-EDGES LEFT TOP2 RIGHT BOTTOM)
             (SEND W1 ':SET-EDGES LEFT TOP RIGHT BOTTOM1))
            (T
             (SEND W1 ':SET-EDGES LEFT TOP RIGHT BOTTOM1)
             (SEND W2 ':SET-EDGES LEFT TOP2 RIGHT BOTTOM))))))

(DEFUN OTHER-WINDOW ()
  "Return an exposed window other than the current one, or NIL.
Actually returns the one below the current one, or the top one
if the current one is the bottom one."
  (LET ((WINDOWS (FRAME-EXPOSED-WINDOWS)))
    (OR (CADR (MEMQ *WINDOW* WINDOWS))
        (IF (NEQ (CAR WINDOWS) *WINDOW*)
            (CAR WINDOWS)))))

(DEFCOM COM-TWO-WINDOWS-SHOWING-REGION "Make two windows on the same buffer.
The top one showing the current region." ()
  (REGION (BP1 BP2)
    (MULTIPLE-VALUE-BIND (TOP-WINDOW BOTTOM-WINDOW)
        (SEND (WINDOW-FRAME *WINDOW*) ':TWO-EDITOR-WINDOWS)
      (SPLIT-SCREEN-BETWEEN-TWO-WINDOWS TOP-WINDOW BOTTOM-WINDOW (COUNT-LINES BP1 BP2 T))
      (SEND TOP-WINDOW ':SET-INTERVAL *INTERVAL*)
      (RECENTER-WINDOW TOP-WINDOW ':START BP1)
      (MOVE-BP (WINDOW-POINT TOP-WINDOW) BP2)
      (SEND BOTTOM-WINDOW ':SET-INTERVAL *INTERVAL*)
      (MOVE-BP (WINDOW-POINT BOTTOM-WINDOW) BP1)
      (MAKE-WINDOW-CURRENT BOTTOM-WINDOW)))
  DIS-TEXT)

;;; This puts the specified number of lines in the top window
(DEFUN SPLIT-SCREEN-BETWEEN-TWO-WINDOWS (TOP-ZWEI-WINDOW BOTTOM-ZWEI-WINDOW NLINES)
  (LET ((W1 (WINDOW-SHEET TOP-ZWEI-WINDOW))
        (W2 (WINDOW-SHEET BOTTOM-ZWEI-WINDOW))
        (FRAME (WINDOW-FRAME TOP-ZWEI-WINDOW)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM MIDDLE)
        (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
      (SETQ MIDDLE (+ TOP (* NLINES (TV:SHEET-LINE-HEIGHT W1))
                      (TV:SHEET-TOP-MARGIN-SIZE W1) (TV:SHEET-BOTTOM-MARGIN-SIZE W1)))
      (OR (AND (TV:SHEET-EXPOSED-P W1)
               (TV:SHEET-EXPOSED-P W2)
               (MULTIPLE-VALUE-BIND (LF TP RT BT)
                   (SEND W1 ':EDGES)
                 (AND (= LF LEFT) (= TP TOP) (= RT RIGHT) (= BT MIDDLE)))
               (MULTIPLE-VALUE-BIND (LF TP RT BT)
                   (SEND W2 ':EDGES)
                 (AND (= LF LEFT) (= TP MIDDLE) (= RT RIGHT) (= BT BOTTOM))))
          (TV:PRESERVE-SUBSTITUTE-STATUS (SEND W1 ':SUPERIOR)
            (TV:DELAYING-SCREEN-MANAGEMENT
              (SEND W1 ':DEEXPOSE)
              (SEND W2 ':DEEXPOSE)
              (SEND W1 ':SET-EDGES LEFT TOP RIGHT MIDDLE)
              (SEND W2 ':SET-EDGES LEFT MIDDLE RIGHT BOTTOM)
              (SEND W1 ':EXPOSE))
              (SEND W2 ':EXPOSE))
          (SEND FRAME ':UPDATE-LABELS)))))

(DEFVAR *SPLIT-SCREEN-WINDOW-LIST*)

(DEFCOM COM-SPLIT-SCREEN "Make several windows split among the buffers as specified." ()
  (LET* ((FRAME (WINDOW-FRAME *WINDOW*))
         (BUFFER-LIST (SPLIT-SCREEN-AMONG-BUFFERS-VIA-MENUS FRAME
                                                            *ZMACS-BUFFER-NAME-ALIST*))
         WINDOW-LIST)
    (COND (BUFFER-LIST
           (TV:PRESERVE-SUBSTITUTE-STATUS FRAME
             (SETQ WINDOW-LIST (SPLIT-SCREEN-AMONG-BUFFERS-DO-IT FRAME BUFFER-LIST))
             (DO ((BL BUFFER-LIST (CDR BL)))
                 ((NULL BL))
               (AND (TYPEP (CAR BL) 'FS:PATHNAME)
                    (SETF (CAR BL) (FIND-FILE (CAR BL) NIL))))
             (MAKE-WINDOW-CURRENT (CAR WINDOW-LIST)))
           (MAPC 'FUNCALL WINDOW-LIST (CIRCULAR-LIST ':SET-INTERVAL) BUFFER-LIST))))
  DIS-TEXT)

;;;; Windows used by COM-SPLIT-SCREEN.

(DEFWINDOW-RESOURCE SPLIT-SCREEN-MENU ()
  :MAKE-WINDOW (TV:TEMPORARY-MENU-WITH-FLASHY-SCROLLING :NAME "Split Screen" :LABEL "Split screen buffer:"
                                  :COLUMNS 2
                                  :ROWS 20)    ;prevent it taking too much room to fit layout.
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)

(DEFCONST *SPLIT-SCREEN-AMONG-BUFFERS-FIXED-ITEMS*
     '(("New buffer" :VALUE "New buffer"
        :DOCUMENTATION "Create a new, empty buffer.  Prompt for its name.")
       ("Find file" :VALUE "Find file"
        :DOCUMENTATION "Do a Find File command and put the resulting buffer in a window.")
       ("Undo" :VALUE "Undo"
        :FONT :MENU-STANDOUT
        :DOCUMENTATION "Undo last selection.")
       ("Do It" :VALUE "Do It"
        :FONT :MENU-STANDOUT
        :DOCUMENTATION "Complete the selection and set up the windows as specified.")
       ("Abort" :VALUE "Abort"
        :FONT :MENU-STANDOUT
        :DOCUMENTATION "Abort the Split Screen command.")))

(DEFUN SPLIT-SCREEN-AMONG-BUFFERS-VIA-MENUS (FRAME BUFFER-ALIST)
  (USING-RESOURCE (MENU SPLIT-SCREEN-MENU)
    (SEND MENU :SET-ITEM-LIST (APPEND BUFFER-ALIST
                                       (IF (ODDP (LENGTH BUFFER-ALIST))
                                           '(("" :NO-SELECT T)))
                                       '(("" :NO-SELECT T)) '(("" :NO-SELECT T))
                                       *SPLIT-SCREEN-AMONG-BUFFERS-FIXED-ITEMS*))
    (TV:EXPOSE-WINDOW-NEAR MENU '(:MOUSE))
    (USING-RESOURCE (LAYWIN TV:SPLIT-SCREEN-LAYOUT-WINDOW)
      (SEND LAYWIN :CLEAR-FROBS)
      (UNWIND-PROTECT
        (DO ((BUFFER-LIST NIL)
             (N-WINDOWS 0)
             (RES))
            (NIL)
          (AND (= N-WINDOWS 1)
               (SEND LAYWIN :MOVE-NEAR-WINDOW MENU
                     (MULTIPLE-VALUE-BIND (WIDTH HEIGHT)
                         (SEND FRAME :INSIDE-SIZE)
                       (CONS WIDTH HEIGHT))))
          (SETQ RES (SEND MENU :CHOOSE))
          (AND (EQUAL RES "New buffer")
               (SETQ RES (READ-BUFFER-NAME-NEAR-WINDOW MENU "New buffer:" NIL T)))
          (COND ((SYMBOLP RES))
                ((NOT (STRINGP RES))
                 (PUSH RES BUFFER-LIST)
                 (SEND LAYWIN :ADD-FROB (BUFFER-NAME RES))
                 (SETQ N-WINDOWS (1+ N-WINDOWS)))
                ((STRING-EQUAL RES "Abort")
                 (RETURN NIL))
                ((STRING-EQUAL RES "Find file")
                 (SETQ RES (READ-DEFAULTED-PATHNAME-NEAR-WINDOW MENU "Find file:"))
                 (COND ((TYPEP RES 'FS:PATHNAME)
                        (PUSH RES BUFFER-LIST)
                        (SEND LAYWIN :ADD-FROB (SEND RES :STRING-FOR-EDITOR))
                        (SETQ N-WINDOWS (1+ N-WINDOWS)))))
                ((STRING-EQUAL RES "Undo")
                 (COND ((PLUSP N-WINDOWS)
                        (SETQ N-WINDOWS (1- N-WINDOWS)
                              BUFFER-LIST (CDR BUFFER-LIST))
                        (SEND LAYWIN :REMOVE-LAST-FROB))))
                (T (RETURN (NREVERSE BUFFER-LIST)))))
        (TV:DELAYING-SCREEN-MANAGEMENT
          (SEND LAYWIN :DEACTIVATE)
          (SEND MENU :DEACTIVATE))))))

(DEFUN SPLIT-SCREEN-AMONG-BUFFERS-DO-IT (FRAME BUFFER-LIST &AUX N-COLUMNS N-ROWS WIDTH HEIGHT
                                               FRAME-LEFT FRAME-TOP FRAME-RIGHT FRAME-BOTTOM
                                               WINDOW-LIST)
  (LET ((N-WINDOWS (LENGTH BUFFER-LIST)))
    (IF (< N-WINDOWS 4)
        (SETQ N-COLUMNS 1 N-ROWS N-WINDOWS)
        (SETQ N-COLUMNS 2 N-ROWS (TRUNCATE (1+ N-WINDOWS) 2))))
  (MULTIPLE-VALUE (FRAME-LEFT FRAME-TOP FRAME-RIGHT FRAME-BOTTOM)
    (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (SETQ WIDTH (TRUNCATE (- FRAME-RIGHT FRAME-LEFT) N-COLUMNS)
        HEIGHT (TRUNCATE (- FRAME-BOTTOM FRAME-TOP) N-ROWS))
  (SETQ WINDOW-LIST (SEND FRAME ':N-EDITOR-WINDOWS (LENGTH BUFFER-LIST)))
  (TV:DELAYING-SCREEN-MANAGEMENT
    (DO ((BL BUFFER-LIST (CDR BL))
         (WL WINDOW-LIST (CDR WL))
         (I 0 (1+ I))
         (LEFT) (RIGHT) (TOP) (BOTTOM) (WINDOW))
        ((NULL BL))
      (SETQ LEFT (+ FRAME-LEFT (* (\ I N-COLUMNS) WIDTH))
            RIGHT (+ LEFT WIDTH)
            TOP (+ FRAME-TOP (* (TRUNCATE I N-COLUMNS) HEIGHT))
            BOTTOM (+ TOP HEIGHT))
      ;; The bottom-most window is wider if there are an odd number of them
      (AND (NULL (CDR BL)) (SETQ RIGHT FRAME-RIGHT))
      (SETQ WINDOW (WINDOW-SHEET (CAR WL)))
      (SEND WINDOW ':SET-EDGES LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW ':EXPOSE NIL ':CLEAN)
      (AND (ZEROP I) (SEND WINDOW ':SELECT NIL))))
  (SEND FRAME ':UPDATE-LABELS)
  WINDOW-LIST)

;;;; Pop-up mini buffers and the editors and editor windows that work with them,

(DEFFLAVOR EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN
           (MODE-LINE-WINDOW)
           ()
  :INITTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS ZWEI-WINDOW)
  (:DOCUMENTATION "Make an ZWEI window work with a pop-up mini buffer."))

;;; The mode line window must get made before the
;;; editor-closure is initialized.
;;; So this mixin must come after TOP-LEVEL-DISPLAYER-MIXIN.
(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :AFTER :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP MODE-LINE-WINDOW)
    (SETQ MODE-LINE-WINDOW
          (CREATE-MODE-LINE-WINDOW 3
                                   SELF
                                   'POP-UP-MODE-LINE-WINDOW
                                   TV:IO-BUFFER
                                   :CONTROLLING-WINDOW SELF)))
  (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN-POSITION-MODE-LINE-WINDOW))

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :BEFORE :FINISH-DELAYED-SELECT)
           ()
  (SEND SELF ':PREPARE-FOR-REDISPLAY))

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :BEFORE :PREPARE-FOR-REDISPLAY)
           ()
  ;; If there is typein or prompts that do not have to stay, clear them out.
  (LET ((*TYPEIN-WINDOW* (SEND MODE-LINE-WINDOW ':TYPEIN-WINDOW)))
    (COND ((TV:SHEET-EXPOSED-P *TYPEIN-WINDOW*)
           ;; If the mode line is displayed only because the typein window is
           ;; and the typein window is clear, just flush it.  Don't wait for input.
           (COND ((MEMQ (SEND *TYPEIN-WINDOW* :TYPEIN-STATUS) '(:CLEAR :USED))
                  (TV:WINDOW-CALL (MODE-LINE-WINDOW :DONE-WITH-MODE-LINE-WINDOW)
                    (SEND *TYPEIN-WINDOW* :CLEAR-WINDOW)
                    (PROMPT-LINE "")))))))
  (AND (TV:SHEET-EXPOSED-P MODE-LINE-WINDOW)
       (UNWIND-PROTECT
         (PROGN
           (SEND MODE-LINE-WINDOW ':EXPOSE)
           (PROMPT-LINE "")
           (LET ((CH (SEND *STANDARD-INPUT* ':ANY-TYI)))
             (SEND *STANDARD-INPUT* ':UNTYI CH)))
         (SEND MODE-LINE-WINDOW ':DONE-WITH-MODE-LINE-WINDOW))))

;;; If not for this, we will not be able to deexpose when the mode line is exposed
;;; because (:method tv:sheet :deexpose) does a tv:prepare-sheet
;;; which waits if output-hold is set.
(DEFWRAPPER (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :DEEXPOSE) (IGNORE . BODY)
  `(PROGN
     (SETF (TV:SHEET-OUTPUT-HOLD-FLAG) 0)
     . ,BODY))

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :AFTER :EXPOSE) (&REST IGNORE)
  (SETF (TV:SHEET-OUTPUT-HOLD-FLAG)
        (IF (SEND MODE-LINE-WINDOW ':EXPOSED-P) 1 0)))

;;; Make sure that the pop up mode line window is positioned in the bottom part
;;; of the space occupied by the editor window itself.
(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :AFTER :SET-EDGES)
           EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN-POSITION-MODE-LINE-WINDOW)

(DEFUN EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN-POSITION-MODE-LINE-WINDOW (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN))
  (LET* ((BOTTOM (TV:SHEET-INSIDE-HEIGHT))
         (HEIGHT (TV:SHEET-HEIGHT MODE-LINE-WINDOW))
         (TOP (MAX (- BOTTOM HEIGHT) (TV:SHEET-INSIDE-TOP))))
    (SEND MODE-LINE-WINDOW ':SET-EDGES
                           (TV:SHEET-INSIDE-LEFT)
                           TOP
                           (TV:SHEET-INSIDE-RIGHT)
                           (+ TOP HEIGHT))
    ;; Make the typeout window not overlap with the mode line window.
    (MULTIPLE-VALUE-BIND (LEFT TOP1 RIGHT)
        (SEND TV:TYPEOUT-WINDOW ':EDGES)
      (SEND TV:TYPEOUT-WINDOW ':SET-EDGES LEFT TOP1 RIGHT TOP))))

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :AFTER :PREPARE-WINDOW-FOR-REDISPLAY)
           ()
  (IF (TV:SHEET-EXPOSED-P MODE-LINE-WINDOW)
      (SEND MODE-LINE-WINDOW ':DONE-WITH-MODE-LINE-WINDOW)))

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :TOP-OF-EDITOR-HIERARCHY) () SELF)

(DEFMETHOD (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :REMOVE-SELECTION-SUBSTITUTE)
           (REMOVE-WINDOW SUGGEST-WINDOW)
  (AND TV:SELECTION-SUBSTITUTE
       (TV:SHEET-ME-OR-MY-KID-P TV:SELECTION-SUBSTITUTE REMOVE-WINDOW)
       (SEND SELF ':SET-SELECTION-SUBSTITUTE
             (AND SUGGEST-WINDOW (TV:SHEET-EXPOSED-P SUGGEST-WINDOW) SUGGEST-WINDOW))))

(DEFFLAVOR POP-UP-MODE-LINE-WINDOW ((CONTROLLING-WINDOW))
           (TV:TEMPORARY-WINDOW-MIXIN MODE-LINE-WINDOW)
  :SETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :AFTER :INIT) (IGNORE)
; (SETF (TV:SHEET-DEEXPOSED-TYPEOUT-ACTION TYPEIN-WINDOW) '(:TEMPORARY-EXPOSE))
  (SETQ TV:DEEXPOSED-TYPEOUT-ACTION '(:TEMPORARY-EXPOSE)))

(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :TEMPORARY-EXPOSE) (&REST ARGS)
  (LEXPR-SEND SELF ':EXPOSE ARGS))

;;; Since we prevent redisplay of the editor window we belong to,
;;; the *WINDOW-LIST* while in our mini buffer should be just the mini buffer,
;;; which will be *WINDOW*, since if it is not the current window
;;; this method will not be the one called.
(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :EDITOR-WINDOWS) ()
  (LIST *WINDOW*))

;; Our mini buffers should get selected as a substitute for us.
(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :ALIAS-FOR-INFERIORS) ()
  (SEND TV:SUPERIOR ':ALIAS-FOR-SELECTED-WINDOWS))

(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (SETF (TV:SHEET-OUTPUT-HOLD-FLAG (WINDOW-SHEET CONTROLLING-WINDOW)) 1))

(DEFMETHOD (POP-UP-MODE-LINE-WINDOW :DONE-WITH-MODE-LINE-WINDOW) ()
  (SETF (TV:SHEET-OUTPUT-HOLD-FLAG (WINDOW-SHEET CONTROLLING-WINDOW)) 0)
   (SEND TV:SUPERIOR ':REMOVE-SELECTION-SUBSTITUTE SELF NIL)
; (SEND SELF ':TEMPORARY-DEEXPOSE)
  (SEND SELF ':DEACTIVATE))

;;; Editor windows that are also editor objects and provide a simple
;;; interface for programs to request editing independent of ZMACS.

;;; This is useful for standalone small editing tasks
(DEFFLAVOR STANDALONE-EDITOR-WINDOW
        ()
        (TOP-LEVEL-DISPLAYER-MIXIN
         EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN
         ZWEI-WINDOW)
  (:DEFAULT-INIT-PLIST :COMTAB *STANDALONE-COMTAB*)
  (:DOCUMENTATION "An editor window that is complete in itself
and arranges for a pop-up window to serve as the mini buffer."))

(DEFFLAVOR STANDALONE-EDITOR-PANE () (STANDALONE-EDITOR-WINDOW)
  :ALIAS-FLAVOR)

(DEFFLAVOR EDITOR-WINDOW () (TOP-LEVEL-DISPLAYER-MIXIN ZWEI-WINDOW)
  (:DOCUMENTATION "An editor window that is complete in itself and does not
share anything with other editor windows.
However, it does not do anything special about the mini buffer."))

(DEFMETHOD (EDITOR-WINDOW :EDIT-STRING) (STRING)
  (EDSTRING STRING SELF))

(DEFUN EDSTRING (STRING &OPTIONAL WINDOW-OR-LIST-OR-LEFT TOP RIGHT BOTTOM COMTAB
                        &AUX FLA SUP OPTS)
  (UNLESS (TYPEP WINDOW-OR-LIST-OR-LEFT 'TV:SHEET)
    (SETQ FLA (COND ((SYMBOLP WINDOW-OR-LIST-OR-LEFT) WINDOW-OR-LIST-OR-LEFT)
                    ((CONSP WINDOW-OR-LIST-OR-LEFT)
                     (SETQ SUP (GET WINDOW-OR-LIST-OR-LEFT ':SUPERIOR)
                           OPTS (CDR WINDOW-OR-LIST-OR-LEFT))
                     (CAR WINDOW-OR-LIST-OR-LEFT))))
    (OR FLA (SETQ FLA 'STANDALONE-EDITOR-WINDOW))
    (OR SUP (SETQ SUP TV:DEFAULT-SCREEN))
    (OR (NUMBERP WINDOW-OR-LIST-OR-LEFT) (SETQ WINDOW-OR-LIST-OR-LEFT 0))
    (OR TOP (SETQ TOP 0))
    (OR RIGHT (SETQ RIGHT (TV:SHEET-WIDTH SUP)))
    (OR BOTTOM (SETQ BOTTOM (* 4 (TV:SHEET-LINE-HEIGHT SUP))))
    (OR COMTAB (SETQ COMTAB *STANDALONE-COMTAB*))
    (SETQ WINDOW-OR-LIST-OR-LEFT (APPLY #'MAKE-INSTANCE FLA
                                                        :SUPERIOR SUP
                                                        :LEFT WINDOW-OR-LIST-OR-LEFT
                                                        :TOP TOP
                                                        :RIGHT RIGHT
                                                        :BOTTOM BOTTOM
                                                        :LABEL NIL
                                                        :COMTAB COMTAB
                                                        OPTS)))
  (SEND WINDOW-OR-LIST-OR-LEFT :SET-INTERVAL-STRING STRING)
  (TV:WINDOW-CALL (WINDOW-OR-LIST-OR-LEFT :DEACTIVATE)
    (SEND WINDOW-OR-LIST-OR-LEFT :EDIT))
  (VALUES (SEND WINDOW-OR-LIST-OR-LEFT :INTERVAL-STRING) WINDOW-OR-LIST-OR-LEFT))

;;; This flavor is useful for the COMPLETING-READ function
(DEFFLAVOR ZWEI-WINDOW-WITH-TOP-OUTSIDE-LABEL () (TV:TOP-LABEL-MIXIN ZWEI-WINDOW))

;;; The STANDALONE-EDITOR-FRAME differs from the STANDALONE-EDITOR-WINDOW
;;; in that it is a frame which contains an always-visible mini buffer
;;; just as a ZMACS frame does.

;;; We also have a pop-up version.

(DEFFLAVOR STANDALONE-EDITOR-FRAME ()
           (BASIC-STANDALONE-EDITOR-FRAME ZWEI-FRAME)
  (:DEFAULT-INIT-PLIST :NUMBER-OF-MINI-BUFFER-LINES 2))

(DEFFLAVOR POP-UP-STANDALONE-EDITOR-FRAME ()
           (TV:TEMPORARY-WINDOW-MIXIN TV:BORDERS-MIXIN STANDALONE-EDITOR-FRAME
            TV:ESSENTIAL-MOUSE)
  (:DEFAULT-INIT-PLIST :BORDER-MARGIN-WIDTH 0))

(DEFFLAVOR BASIC-STANDALONE-EDITOR-FRAME
        (BASE-TICK)
        ()
  (:DEFAULT-INIT-PLIST :COMTAB *STANDALONE-COMTAB*)
  :SETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS ZWEI-FRAME))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :INTERVAL-STRING) PASS-ON-TO-SELECTED-PANE)
(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :SET-INTERVAL-STRING) PASS-ON-TO-SELECTED-PANE)

(DEFUN PASS-ON-TO-SELECTED-PANE (&REST ARGS)
  (DECLARE (:SELF-FLAVOR BASIC-STANDALONE-EDITOR-FRAME))
  (LEXPR-SEND (SEND SELF :SELECTED-PANE) ARGS))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :AFTER :INIT) (IGNORE)
  (SEND SELF ':SELECT-PANE (SEND SELF ':CREATE-WINDOW 'STANDALONE-ZWEI-WINDOW-PANE
                                                      ':ACTIVATE-P T ':LABEL NIL)))

(DEFCOM COM-STANDALONE-ABORT "Abort this editing" ()
  (THROW 'ABORT-STANDALONE-EDIT NIL))

(DEFFLAVOR STANDALONE-ZWEI-WINDOW-PANE () (ZWEI-WINDOW-PANE)
  (:DOCUMENTATION "This is the flavor of pane that lives inside the STANDALONE-EDITOR-FRAME."))

(DEFMETHOD (STANDALONE-ZWEI-WINDOW-PANE :EXIT-SPECIAL-BUFFER) (&REST ARGS)
  (LEXPR-SEND TV:SUPERIOR :EXIT-SPECIAL-BUFFER ARGS))

(DEFMETHOD (STANDALONE-ZWEI-WINDOW-PANE :FIND-SPECIAL-BUFFER) (&REST ARGS)
  (LEXPR-SEND TV:SUPERIOR :FIND-SPECIAL-BUFFER ARGS))

(DEFMETHOD (STANDALONE-ZWEI-WINDOW-PANE :BASE-TICK) ()
  (SEND TV:SUPERIOR :BASE-TICK))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :SET-SIZE-FROM-STRING)
           (STRING &OPTIONAL MIN-WIDTH MIN-HEIGHT MAX-WIDTH MAX-HEIGHT)
  (MULTIPLE-VALUE-BIND (DWT DHT)
      (SEND TV:SUPERIOR :INSIDE-SIZE)
    (SETQ MAX-WIDTH (OR MAX-WIDTH DWT)
          MAX-HEIGHT (OR MAX-HEIGHT DHT))
    (LET ((PANE (SEND SELF :SELECTED-PANE)))
      (MULTIPLE-VALUE (NIL DHT NIL DWT)
        (SEND PANE :COMPUTE-MOTION STRING 0 NIL 0 0 T 0
                                   MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM))
      (INCF DWT (+ (TV:SHEET-CHAR-WIDTH PANE)
                   (TV:SHEET-LEFT-MARGIN-SIZE PANE)
                   (TV:SHEET-RIGHT-MARGIN-SIZE PANE)
                   (TV:SHEET-LEFT-MARGIN-SIZE SELF)
                   (TV:SHEET-RIGHT-MARGIN-SIZE SELF)))
      (INCF DHT (+ (TV:SHEET-TOP-MARGIN-SIZE PANE)
                   (TV:SHEET-BOTTOM-MARGIN-SIZE PANE)
                   (TV:SHEET-TOP-MARGIN-SIZE SELF)
                   (TV:SHEET-BOTTOM-MARGIN-SIZE SELF))))
    (MULTIPLE-VALUE-BIND (NIL MODE-LINE-HEIGHT)
        (SEND MODE-LINE-WINDOW :SIZE)
      (SETQ DHT (+ DHT MODE-LINE-HEIGHT))
      (AND MIN-WIDTH (SETQ DWT (MAX MIN-WIDTH DWT)))
      (AND MIN-HEIGHT (SETQ DHT (MAX MIN-HEIGHT DHT)))
      (SETQ DWT (MIN DWT MAX-WIDTH)
            DHT (MIN DHT MAX-HEIGHT))
      (SEND SELF :SET-SIZE DWT DHT))))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :TYPEIN-LINE) (STRING &REST ARGS)
  (LET ((TYPEIN-WINDOW (SEND MODE-LINE-WINDOW ':TYPEIN-WINDOW)))
    (SEND TYPEIN-WINDOW ':FRESH-LINE)
    (APPLY 'FORMAT TYPEIN-WINDOW STRING ARGS)))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :SET-COMTAB) (NEW-COMTAB)
  (FUNCALL EDITOR-CLOSURE
           #'(LAMBDA (C)
               (SETQ *COMTAB* *MODE-COMTAB*)
               (SETF (COMTAB-INDIRECT-TO *MODE-COMTAB*) C))
           NEW-COMTAB))

(DEFMETHOD (BASIC-STANDALONE-EDITOR-FRAME :SET-MODE-LINE-LIST) (NEW-MODE-LINE-LIST)
  (FUNCALL EDITOR-CLOSURE #'(LAMBDA (C) (SETQ *MODE-LINE-LIST* C)) NEW-MODE-LINE-LIST))

(DEFMETHOD (POP-UP-STANDALONE-EDITOR-FRAME :BEFORE :EDIT) (&REST IGNORE)
  (SETQ BASE-TICK *TICK*))

(DEFMETHOD (POP-UP-STANDALONE-EDITOR-FRAME :EDIT-STRING) (STRING)
  (EDSTRING STRING SELF))

(DEFWINDOW-RESOURCE POP-UP-STANDALONE-EDITOR-FRAME ()
  :MAKE-WINDOW (POP-UP-STANDALONE-EDITOR-FRAME :NUMBER-OF-MINI-BUFFER-LINES 1)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

;;; Pop up a window and edit the specified string in it.
;;; The value is NIL if the user aborts.
(DEFUN POP-UP-EDSTRING (STRING &OPTIONAL (NEAR-MODE '(:MOUSE)) MODE-LINE-LIST
                                         MIN-WIDTH MIN-HEIGHT INITIAL-MESSAGE
                                         (COMTAB *STANDALONE-COMTAB*))
  (USING-RESOURCE (WINDOW POP-UP-STANDALONE-EDITOR-FRAME)
    (SEND WINDOW :SET-COMTAB COMTAB)
    (SEND WINDOW :SET-SIZE-FROM-STRING STRING MIN-WIDTH MIN-HEIGHT)
    (SEND WINDOW :SET-INTERVAL-STRING STRING)
    (SEND WINDOW :SET-MODE-LINE-LIST (OR MODE-LINE-LIST '("Standalone editor frame")))
    (TV:EXPOSE-WINDOW-NEAR WINDOW NEAR-MODE)
    (AND INITIAL-MESSAGE (SEND WINDOW ':TYPEIN-LINE INITIAL-MESSAGE))
    (TV:WINDOW-CALL (WINDOW :DEACTIVATE)
      (CATCH 'ABORT-STANDALONE-EDIT
        (SEND WINDOW :EDIT)
        (SEND WINDOW :INTERVAL-STRING)))))

;;; Like POP-UP-EDSTRING, but you specify a window to use
;;; (presumably a STANDALONE-EDITOR-FRAME already exposed, already the size you want, etc.)
;;; The value is NIL if the user aborts.
(DEFUN NON-POP-UP-EDSTRING (STRING WINDOW &OPTIONAL INITIAL-MESSAGE)
  (SEND WINDOW :SET-INTERVAL-STRING STRING)
  (SEND WINDOW :SET-BASE-TICK (TICK))
  (AND INITIAL-MESSAGE (SEND WINDOW :TYPEIN-LINE INITIAL-MESSAGE))
  (TV:WINDOW-CALL (WINDOW)
    (CATCH 'ABORT-STANDALONE-EDIT
      (SEND WINDOW :EDIT)
      (SEND WINDOW :INTERVAL-STRING))))

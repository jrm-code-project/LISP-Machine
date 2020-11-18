;-*- Mode:LISP; Package:TV; Base:8 -*-

;Questionnaires.
;A questionnaire frame is a frame containing a bunch of questionnaire elements,
;which are windows containing a label and often a "value".  You can change the
;value with the mouse.  This is intended for displaying options to complicated
;programs and the like; it is a generalization of menus.
;
;The elements have names in the usual constraint-frame fashion.
;
;The following messages are special to questionnaire frames:
;They work by sending to the questionnaire elements
;       :ELEMENT-VALUE name
;       :SET-ELEMENT-VALUE name value
;       :ELEMENT-ACCENT name
;       :SET-ELEMENT-ACCENT name value
;       :BROADCAST message &rest args
;               Sends message to all elements.  This is useful
;               for various forms of resetting.
;          Broadcasting the :QUESTIONNAIRE-RESET message resets all the state
;       :SEND-ELEMENT pane-name message args
;       :FIND-PANE name   (returns the pane [this doesn't belong here])
;
;Note that since a questionnaire frame has a bit save array, and its component
;questionnaire element panes do not, you can update them if the frame is
;de-exposed and they will automatically update in the bit save array,
;which is the right thing to avoid gratuitous-looking redisplay.  This also
;implies that all questionnaire panes need to know how to redisplay themselves
;without the aid of a bit-save array, e.g. when the pane configuration is changed.
;The ones in this file do so.
;
;If you want a function to get called when a questionnaire element is changed
;with the mouse, give it a :AFTER :MOUSE-BUTTONS method which looks at its
;VALUE and/or ACCENT instance variables.  This will do exactly the right thing.

(DEFFLAVOR QUESTIONNAIRE-FRAME-MIXIN () ()
  (:INCLUDED-FLAVORS BASIC-CONSTRAINT-FRAME)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :MIXIN "Stuff specific to questionnaires"))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :ELEMENT-VALUE) (ELEMENT-NAME)
  (LET ((PANE (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES))))
    (IF PANE (FUNCALL PANE ':VALUE)
        (FERROR NIL "No ~S element in ~S" ELEMENT-NAME SELF))))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :SET-ELEMENT-VALUE) (ELEMENT-NAME VALUE)
  (LET ((PANE (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES))))
    (IF PANE (FUNCALL PANE ':SET-VALUE VALUE)
        (FERROR NIL "No ~S element in ~S" ELEMENT-NAME SELF))))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :ELEMENT-ACCENT) (ELEMENT-NAME)
  (LET ((PANE (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES))))
    (IF PANE (FUNCALL PANE ':ACCENT)
        (FERROR NIL "No ~S element in ~S" ELEMENT-NAME SELF))))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :SET-ELEMENT-ACCENT) (ELEMENT-NAME ACCENT-P)
  (LET ((PANE (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES))))
    (IF PANE (FUNCALL PANE ':SET-ACCENT ACCENT-P)
        (FERROR NIL "No ~S element in ~S" ELEMENT-NAME SELF))))

;This sends to all panes, not just the exposed ones
(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :BROADCAST) (MESSAGE &REST ARGS)
  (DOLIST (X INTERNAL-PANES)
    (LEXPR-FUNCALL (CDR X) MESSAGE ARGS)))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :FIND-PANE) (ELEMENT-NAME)
  (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES)))

(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :SEND-ELEMENT) (ELEMENT-NAME MESSAGE &REST ARGS)
  (LEXPR-FUNCALL (CDR (ASSQ ELEMENT-NAME INTERNAL-PANES)) MESSAGE ARGS))

;Given a pane, this returns the name for that pane the user gave in his alist.
;NIL if for some reason it is not found.
(DEFMETHOD (QUESTIONNAIRE-FRAME-MIXIN :NAME-FOR-PANE) (WINDOW)
  (DOLIST (X INTERNAL-PANES)
    (AND (EQ (CDR X) WINDOW) (RETURN (CAR X)))))

;Default kind of questionnaire frame has moby label and thick borders
;I may decide to change this later
(DEFFLAVOR QUESTIONNAIRE-FRAME ()
           (BORDERS-MIXIN TOP-CENTERED-LABEL-MIXIN DIVIDER-MIXIN QUESTIONNAIRE-FRAME-MIXIN
            BASIC-CONSTRAINT-FRAME BASIC-FRAME WINDOW)
  (:DEFAULT-INIT-PLIST :BORDERS 3 :DIVIDER 3 :LABEL FONTS:METS)
  (:DOCUMENTATION :COMBINATION "Something like a menu but with more-active elements"))

;;; Flavors I depend on which should perhaps be installed but aren't currently.

(DEFFLAVOR TOP-CENTERED-LABEL-MIXIN () (TOP-LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Puts the label at the top of the window and centered"))

(DEFMETHOD (TOP-CENTERED-LABEL-MIXIN :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  (COND (SPEC
         (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF)
         (SHEET-DISPLAY-X-Y-CENTERED-STRING SELF (LABEL-STRING SPEC)
            (- LEFT (SHEET-INSIDE-LEFT)) (- TOP (SHEET-INSIDE-TOP))
            (- RIGHT (SHEET-INSIDE-LEFT)) (- BOTTOM (SHEET-INSIDE-TOP)) (LABEL-FONT SPEC)))))

;This flavor provides a line between the label and the data, if you
;put it in your flavor list after the other borders stuff.
;I haven't bothered to make this provide a way to say that
;the label is at other than the top.  The totally(?) ((well, slightly)) hairy way
;this works is necessary to make redefinition of the margins,
;which constraint frames always do, work right.
;The DIVIDER variable internally contains cons of Y-pos and thickness,
;or NIL to turn it off.  Externally you may give NIL to turn it off
;or a number which is the thickness of the black part (default=1).
;There is also one raster line of white above and one below.

(DEFFLAVOR DIVIDER-MIXIN ((DIVIDER 1)) (MARGIN-HACKER-MIXIN)
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES DIVIDER)
  (:DOCUMENTATION :MIXIN
               "Provides a line between the top-centered label and the body of the window"))

(DEFMETHOD (DIVIDER-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (ADJUST-MARGINS 'DIVIDER ':PARSE-DIVIDER-SPEC INIT-PLIST NIL))

(DEFMETHOD (DIVIDER-MIXIN :BEFORE :REDEFINE-MARGINS) (PLIST)
  (ADJUST-MARGINS 'DIVIDER ':PARSE-DIVIDER-SPEC PLIST ':DIVIDER))

(DEFMETHOD (DIVIDER-MIXIN :PARSE-DIVIDER-SPEC) (SPEC LM TM RM BM)
  (COND ((NULL SPEC))           ;NIL means no divider
        (T (OR (NUMBERP SPEC) (SETQ SPEC (CDR SPEC)))   ;Get thickness number
           (PSETQ SPEC (CONS (1+ TM) SPEC)
                  TM (+ TM SPEC 2))))
  (PROG () (RETURN SPEC LM TM RM BM)))

(DEFMETHOD (DIVIDER-MIXIN :AFTER :REFRESH-MARGINS) ()
  (%DRAW-RECTANGLE WIDTH (CDR DIVIDER) 0 (CAR DIVIDER) CHAR-ALUF SELF))

;;; Questionnaire panes

;There are two basic flavors for questionnaire panes.  One for those with labels,
;and one for those without.  The difference is mainly the :XOR-ACCENT message,
;which in one case XORs just the label area, while in the other case it XORs the
;whole window.

;This flavor is the real low-level questionnaire pane stuff.  It is shared between
;the two basic kinds, and not to be used otherwise.
(DEFFLAVOR ESSENTIAL-QUESTIONNAIRE-PANE () (PANE-MIXIN)
  (:DEFAULT-INIT-PLIST :SAVE-BITS NIL           ;The frame takes care of it
                       :MORE-P NIL              ;Normally don't want more-processing
                       :BLINKER-DESELECTED-VISIBILITY NIL       ;nor blinker turd
                       :BLINKER-P NIL           ;nor a blinker at all
                                ;You can turn the above back on in your own flavor if you like
                       :BORDERS 1)
  (:REQUIRED-METHODS :XOR-ACCENT                ;Highlight the window by XOR'ing over it
                     :QUESTIONNAIRE-RESET)      ;Broadcast to all panes for general reset
  (:METHOD-COMBINATION (:PROGN :BASE-FLAVOR-LAST :QUESTIONNAIRE-RESET))
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Lowest level of the questionnaire-pane family"))

;This is just here to get rid of a gratuitous error message I put in for
;not having any methods
(DEFMETHOD (ESSENTIAL-QUESTIONNAIRE-PANE :DEFAULT :QUESTIONNAIRE-RESET) ()
  NIL)

;The regular mouse-buttons method screws me over by trying to select the pane.
;Punt that, but still have it call the system menu for the right-hand button,
;and have the feature of exposing the frame if not exposed and you click the
;left button.  There is a wrapper to prevent it calling the daemons if
;the mouse was just used in one of those ways.

;This gets rid of the ESSENTIAL-MOUSE method, which tries to select it, fails, and beeps
(DEFMETHOD (ESSENTIAL-QUESTIONNAIRE-PANE :MOUSE-BUTTONS) (IGNORE IGNORE IGNORE)
  NIL)

(DEFWRAPPER (ESSENTIAL-QUESTIONNAIRE-PANE :MOUSE-BUTTONS) ((BD IGNORE IGNORE) . BODY)
  `(COND ((BIT-TEST 4 BD) (MOUSE-CALL-SYSTEM-MENU))
         ((NOT (SHEET-EXPOSED-P SUPERIOR))
          (FUNCALL SUPERIOR ':EXPOSE))
         (T . ,BODY)))


(DEFFLAVOR QUESTIONNAIRE-PANE-WITHOUT-LABEL ()
  (ESSENTIAL-QUESTIONNAIRE-PANE STREAM-MIXIN BORDERS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Base flavor for unlabelled questionnaire panes"))

;; XOR over the whole interior of the window
(DEFMETHOD (QUESTIONNAIRE-PANE-WITHOUT-LABEL :XOR-ACCENT) ()
  (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
                   (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP) ALU-XOR SELF))


(DEFFLAVOR QUESTIONNAIRE-PANE-WITH-LABEL ()
  (ESSENTIAL-QUESTIONNAIRE-PANE STREAM-MIXIN BORDERS-MIXIN TOP-CENTERED-LABEL-MIXIN
   DIVIDER-MIXIN CHANGEABLE-NAME-MIXIN SELECT-MIXIN MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :INTEGRAL-P T    ;Do I want this?
                       :BORDERS 2)      ;I suspect this wants thicker borders
  (:DOCUMENTATION :COMBINATION "Base flavor for labelled questionnaire panes"))

;; XOR over just the label
(DEFMETHOD (QUESTIONNAIRE-PANE-WITH-LABEL :XOR-ACCENT) ()
  (MULTIPLE-VALUE-BIND (LLEFT LTOP LRIGHT LBOTTOM)
      (COMPUTE-LABEL-POSITION)
    (%DRAW-RECTANGLE (- LRIGHT LLEFT) (- LBOTTOM LTOP) LLEFT LTOP ALU-XOR SELF)))


;; This is a type of questionnaire pane which sort of consists of just a label.
;; It isn't implemented that way because that turns out to be hard to do.
;; Instead it has no label and automatically displays its name.
;; It's called a "button" because it sort of looks like a big square pushbutton.
(DEFFLAVOR QUESTIONNAIRE-BUTTON-PANE () (QUESTIONNAIRE-PANE-WITHOUT-LABEL)
  (:DEFAULT-INIT-PLIST :FONT-MAP (LIST FONTS:MEDFNT))
  (:DOCUMENTATION :COMBINATION "A questionnaire pane consisting of just a box and its name"))

(DEFMETHOD (QUESTIONNAIRE-BUTTON-PANE :AFTER :REFRESH) (&REST IGNORE)
  (OR RESTORED-BITS-P (SHEET-DISPLAY-X-Y-CENTERED-STRING SELF NAME)))

;;; *** I still need to figure out what are really the best default border thicknesses ***

;;; Mixins for questionnaire panes with additional features

;;; This flavor provides the gettable/settable ACCENT value, which puts
;;; inverse video on the window if it is non-NIL.
;;; The :XOR-ACCENT method is not defined here, but elsewhere since it
;;; depends on whether or not the window has separate label and text areas.

(DEFFLAVOR QUESTIONNAIRE-ACCENT-MIXIN ((ACCENT NIL)) ()
  (:GETTABLE-INSTANCE-VARIABLES ACCENT) ;Really settable, but auto method would screw me
  (:INCLUDED-FLAVORS ESSENTIAL-QUESTIONNAIRE-PANE ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Provides the accenting feature for questionnaire panes"))

(DEFMETHOD (QUESTIONNAIRE-ACCENT-MIXIN :SET-ACCENT) (ACCENT-P)
  (OR (EQ (NOT ACCENT-P) (NOT ACCENT))          ;If complementing accent,
      (SHEET-FORCE-ACCESS (SELF)                ; update it on the display
        (FUNCALL-SELF ':XOR-ACCENT)))
  (SETQ ACCENT ACCENT-P))

(DEFMETHOD (QUESTIONNAIRE-ACCENT-MIXIN :QUESTIONNAIRE-RESET) ()
  (FUNCALL-SELF ':SET-ACCENT NIL))

;; This assumes this flavor comes earlier in the flavor list than the label,
;; so that the label is already refreshed before this accenting happens.
;; **** This needs to be hacked for REFRESH-MARGINS ****
(DEFMETHOD (QUESTIONNAIRE-ACCENT-MIXIN :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR RESTORED-BITS-P (NOT ACCENT)
      (FUNCALL-SELF ':XOR-ACCENT)))


;;; This flavor provides for a function to be called when the user clicks
;;; the mouse on the pane.  The FUNCTION instance variable is the function,
;;; which may be set to NIL to disable the feature, or :BEEP to just beep,
;;; or a function to be called with four arguments:
;;;     1. The questionnaire frame
;;;     2. The name of the pane
;;;     3. The buttons mask
;;;     4. What happened:
;;;        If this pane has a value, this is the new value.
;;;        Else if this pane has an accent, this is the current accent; thus NIL if
;;;             the user thinks he turned it on, and T if he thinks he turned it off,
;;;             if the function is going to complement the accent.
;;;        Else just NIL.
;;; Note that the function is run in a separate process, not in the mouse process,
;;; so it's free to do what it likes without worrying about hanging the mouse process.
;;; Note that the function gets called even if the user clicks to change the value
;;; then punts by hitting rubout.

(DEFFLAVOR QUESTIONNAIRE-FUNCTION-MIXIN ((FUNCTION NIL)) ()
  (:SETTABLE-INSTANCE-VARIABLES FUNCTION)
  (:INCLUDED-FLAVORS ESSENTIAL-QUESTIONNAIRE-PANE ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN
     "Provides the feature of calling a function when moused, for questionnaire panes"))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (QUESTIONNAIRE-VALUE-ELEMENT) ;A bit of a kludge I admit
(DEFMETHOD (QUESTIONNAIRE-FUNCTION-MIXIN :AFTER :MOUSE-BUTTONS) (BUTTONS IGNORE IGNORE)
  (COND ((NULL FUNCTION) NIL)
        ((OR (EQ FUNCTION ':BEEP) (AND (SYMBOLP FUNCTION) (NOT (FBOUNDP FUNCTION))))
         (BEEP))
        (T (PROCESS-RUN-FUNCTION "Questionnaire" FUNCTION
                (SHEET-SUPERIOR SELF) (FUNCALL (SHEET-SUPERIOR SELF) ':NAME-FOR-PANE SELF)
                BUTTONS (COND ((TYPEP SELF 'QUESTIONNAIRE-VALUE-ELEMENT) VALUE)
                              ((TYPEP SELF 'QUESTIONNAIRE-ACCENT-MIXIN) ACCENT)))))))

;;; NOTE:  You probably don't want to use this.  It causes extraneous flashing,
;;;        which could be fixed, but it also seems to be obnoxious and doesn't
;;;        really fit in with our ways of doing things.  I'm keeping it "in
;;;        storage for man's use".
;This flavor provides that the accent will complement
;when a mouse button is pushed with the mouse over the window, and moving
;the mouse out of the window before releasing the button will cause the
;button to take no effect.  Note that it does not change the ACCENT instance
;variable, which represents the desired state, not the current state.
;Note that this flavor precludes the ability to do multiple clicks and to
;get to the system menu from this type of window.  If that is a problem
;it could be changed so that the right button gets you to the system menu
;and only the left button is for these hacks.

(DEFFLAVOR QUESTIONNAIRE-MOUSE-MIXIN () ()
  (:INCLUDED-FLAVORS ESSENTIAL-QUESTIONNAIRE-PANE)
  (:DOCUMENTATION :MIXIN
     "Provides xerox-like mouse (active when button released) for questionnaire panes"))

;Before processing the mouse-buttons message, complement the accent
;to show the user that he is winning, then run a function to wait
;for the button to be raised or the mouse to be moved out of the
;window, and to decide if the rest of the mosue button processing
;should be performed or skipped.  The unwind-protect guarantees
;that the complementing of the accent gets undone.  It is done just
;the way it is so that if the mouse button processor changes the
;accent state, it gets restored properly.
(DEFWRAPPER (QUESTIONNAIRE-MOUSE-MIXIN :MOUSE-BUTTONS) (IGNORE . BODY)
  `(PROGN
     (FUNCALL-SELF ':XOR-ACCENT)
     (UNWIND-PROTECT
        (COND ((QUESTIONNAIRE-MOUSE-HACK-BUTTONS)
               . ,BODY))
        (FUNCALL-SELF ':XOR-ACCENT))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (QUESTIONNAIRE-MOUSE-MIXIN)
(DEFUN QUESTIONNAIRE-MOUSE-HACK-BUTTONS ()
  ;; Wait for the button to be lifted.  Return NIL if it should be ignored,
  ;; T if it should go through.  Don't bother about inferiors and scroll bars.
  (MULTIPLE-VALUE-BIND (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
    (DO ((MIN-X 0) (MIN-Y 0)
         (MAX-X (1- (SHEET-INSIDE-WIDTH MOUSE-SHEET)))
         (MAX-Y (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET)))
         (DX) (DY) (BU) (BD)
         (MOVE-METHOD (OR (GET-HANDLER-FOR SELF ':MOUSE-MOVES)
                          #'MOUSE-SET-BLINKER-CURSORPOS)))
        (MOUSE-RECONSIDER NIL)
      ;; Wait for the mouse to do something.
      (MULTIPLE-VALUE (DX DY BD BU) (MOUSE-INPUT))
      (AND MOUSE-RECONSIDER (RETURN NIL))
      ;; If button was released, then we are done.  If still inside the window
      ;; process the previous button depression.
      (OR (ZEROP BU) (RETURN (WINDOW-OWNS-MOUSE-P SELF)))
      ;; Update the position of the mouse
      (SETQ MOUSE-X (MAX MIN-X (MIN MAX-X (+ MOUSE-X DX)))
            MOUSE-Y (MAX MIN-Y (MIN MAX-Y (+ MOUSE-Y DY))))
      (FUNCALL MOVE-METHOD ':MOUSE-MOVES
               (- MOUSE-X WINDOW-X-OFFSET) (- MOUSE-Y WINDOW-Y-OFFSET))
      ;; We are also done if the mouse has moved out of the window
      (OR (WINDOW-OWNS-MOUSE-P SELF) (RETURN NIL))))))

;Entering the window with a button held down should work just like pushing
;the button after entering it.  This is different from the way other windows
;work but seems right for this.
(DEFMETHOD (QUESTIONNAIRE-MOUSE-MIXIN :BEFORE :HANDLE-MOUSE) ()
  (AND (PLUSP MOUSE-LAST-BUTTONS)
       (MULTIPLE-VALUE-BIND (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
           (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
         (FUNCALL-SELF ':MOUSE-BUTTONS MOUSE-LAST-BUTTONS
                       (- MOUSE-X WINDOW-X-OFFSET) (- MOUSE-Y WINDOW-Y-OFFSET)))))

;;; A questionnaire element is a window designed to go inside a questionnaire frame.
;;; Its main purpose is to maintain a value which can be changed with the mouse.
;;; It also maintains a true/false "accent" state; accent is normally displayed
;;; as inverse video over the label.  Typically accenting is used to tell the
;;; user what elements are important to the current operation.

;;; You want to specify in your init-plist :NAME "foo" to set the label.


;;; This type of questionnaire element has no value; if you ask for the value
;;; you get T or NIL depending on the accent.  Clicking the mouse just complements
;;; the accent.  The initial value is always NIL.
;;; You don't want to mix this with QUESTIONNAIRE-FUNCTION-MIXIN, since in that
;;; case the function should control the turning on and off of the accent.

(DEFFLAVOR QUESTIONNAIRE-ONOFF-ELEMENT ()
           (QUESTIONNAIRE-ACCENT-MIXIN QUESTIONNAIRE-BUTTON-PANE)
  (:DOCUMENTATION :COMBINATION
     "A questionnaire element with T//NIL value, complemented by mouse"))

;These three methods simulate the value with the accent.  I don't seem to
;use that, but it sounds like it ought to be convenient.
(DEFMETHOD (QUESTIONNAIRE-ONOFF-ELEMENT :VALUE) ()
  ACCENT)

(DEFMETHOD (QUESTIONNAIRE-ONOFF-ELEMENT :SET-VALUE) (NEW-VALUE)
  (FUNCALL-SELF ':SET-ACCENT NEW-VALUE))

(DEFMETHOD (QUESTIONNAIRE-ONOFF-ELEMENT :RESET-VALUE) ()
  (FUNCALL-SELF ':SET-ACCENT NIL))

;Clicking any mouse button complements the accent (value).
(DEFMETHOD (QUESTIONNAIRE-ONOFF-ELEMENT :MOUSE-BUTTONS) (BUTTON-MASK X-POS Y-POS)
  BUTTON-MASK X-POS Y-POS       ;Ignored
  (FUNCALL-SELF ':SET-ACCENT (NOT ACCENT)))


;;; This type of questionnaire element consists of a button which if you click
;;; on it calls a function.  This turns out to be pretty convenient.
;;; It comes in two sizes, regular and the large inflated-economy size.
;;; The sizes of these do not default from the name you give them, since
;;; that would interfere with the ability to make an array of them all
;;; the same size.  Instead they default to what I wanted for TRACE,
;;; which may turn out to be the right thing for others as well.

(DEFFLAVOR QUESTIONNAIRE-FUNCTION-BUTTON ()
        (QUESTIONNAIRE-FUNCTION-MIXIN QUESTIONNAIRE-ACCENT-MIXIN QUESTIONNAIRE-BUTTON-PANE)
  (:DEFAULT-INIT-PLIST :CHARACTER-HEIGHT 1 :CHARACTER-WIDTH 10.)
  (:DOCUMENTATION :COMBINATION "A questionnaire element which calls a function when moused"))

(DEFFLAVOR QUESTIONNAIRE-BIG-FUNCTION-BUTTON ()
           (QUESTIONNAIRE-FUNCTION-BUTTON)
  (:DEFAULT-INIT-PLIST :FONT-MAP (LIST FONTS:BIGFNT) :BORDERS 4
                       :CHARACTER-HEIGHT 2 :CHARACTER-WIDTH 10.)
  (:DOCUMENTATION :COMBINATION
      "A questionnaire element, of prominent size, which calls a function when moused"))

;;; Basic flavor for a questionnaire element that has a value which is displayed.
;;; The default-init-plist sets up a label at the top, a divider below the label,
;;; and borders all around with a width of 2.
;;; Special messages:
;;;     :RESET-VALUE    restore value to INITIAL-VALUE
;;;     :NEW-VALUE-FROM-KEYBOARD        So you can redefine this method
;;;     Also the accent and value messages implied by questionnaire frame messages.

(DEFFLAVOR QUESTIONNAIRE-VALUE-ELEMENT (VALUE (INITIAL-VALUE ""))
           (QUESTIONNAIRE-ACCENT-MIXIN QUESTIONNAIRE-PANE-WITH-LABEL)
  ;(:SETTABLE-INSTANCE-VARIABLES VALUE INITIAL-VALUE)  ;commented due to auto method bug
  (:SETTABLE-INSTANCE-VARIABLES INITIAL-VALUE)
  (:GETTABLE-INSTANCE-VARIABLES VALUE)  ;Settable but there is an explicit set method
  (:DEFAULT-INIT-PLIST :BLINKER-P T)    ;I need a blinker when asking for input
  (:DOCUMENTATION :COMBINATION
      "A questionnaire element containing a displayed value, input from keyboard"))

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :BEFORE :INIT) (IGNORE)
  (SETQ VALUE INITIAL-VALUE))           ;Value will be displayed later

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :RESET-VALUE) ()
  (FUNCALL-SELF ':SET-VALUE INITIAL-VALUE))

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :QUESTIONNAIRE-RESET) ()
  (FUNCALL-SELF ':SET-VALUE INITIAL-VALUE))

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :SET-VALUE) (NEW-VALUE)
  (SETQ VALUE NEW-VALUE)
  (DISPLAY-QUESTIONNAIRE-VALUE))

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P (DISPLAY-QUESTIONNAIRE-VALUE)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (QUESTIONNAIRE-VALUE-ELEMENT)
(DEFUN DISPLAY-QUESTIONNAIRE-VALUE (&REST IGNORE)
  (FUNCALL-SELF ':CLEAR-SCREEN)
  (LET ((STRING (COND ((STRINGP VALUE) VALUE)
                      ((SYMBOLP VALUE) (GET-PNAME VALUE))
                      (T (FORMAT NIL "~D" VALUE)))))
    (SHEET-DISPLAY-X-Y-CENTERED-STRING SELF STRING))))

;Fix bugs in the system select and deselect
(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :AFTER :SELECT) (&REST IGNORE)
  (BLINKER-SET-VISIBILITY (FIRST (SHEET-BLINKER-LIST SELF)) ':BLINK))

(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :AFTER :DESELECT) (&REST IGNORE)
  (BLINKER-SET-VISIBILITY (FIRST (SHEET-BLINKER-LIST SELF)) NIL))

;Clicking any mouse button gets a new string value from the keyboard
(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :MOUSE-BUTTONS) (BUTTON-MASK X-POS Y-POS)
  BUTTON-MASK X-POS Y-POS       ;Ignored
  (SHEET-HOME SELF)             ;Blinker should not appear in random place
  (FUNCALL-SELF ':SELECT NIL)
  ;; Peek ahead at the first character so present value not immediately erased
  ;; Typing just a rubout leaves the present value alone
  (LET ((CH (FUNCALL-SELF ':TYI)))
    (COND ((NOT (= CH #\RUBOUT))
           (FUNCALL-SELF ':UNTYI CH)
           (FUNCALL-SELF ':CLEAR-SCREEN)
           (SETQ VALUE (FUNCALL-SELF ':NEW-VALUE-FROM-KEYBOARD))
           (DISPLAY-QUESTIONNAIRE-VALUE))))
  (FUNCALL-SELF ':DESELECT NIL))

;This method is the default, but typically gets replaced, for flavors with different syntax
(DEFMETHOD (QUESTIONNAIRE-VALUE-ELEMENT :NEW-VALUE-FROM-KEYBOARD) ()
  (READLINE SELF))


;;; A questionnaire element whose value is constrained to be an integer.
(DEFFLAVOR QUESTIONNAIRE-INTEGER-ELEMENT () (QUESTIONNAIRE-VALUE-ELEMENT)
  (:DOCUMENTATION :COMBINATION "A questionnaire value element constrained to be an integer"))

(DEFMETHOD (QUESTIONNAIRE-INTEGER-ELEMENT :NEW-VALUE-FROM-KEYBOARD) ()
  (LET ((STANDARD-INPUT SELF)
        (IBASE 10.))
    (DO ((NUM)) (NIL)
      (SETQ NUM (READ-FOR-TOP-LEVEL))
      (AND (FIXP NUM) (RETURN NUM))
      (FUNCALL-SELF ':CLEAR-SCREEN)
      (FUNCALL-SELF ':STRING-OUT "Number: "))))


;;; A questionnaire element whose value is an S-expression.  Also remembers the
;;; package to work in.  Input radix is decimal since output radix is.
;;; Perhaps that should be controllable?
;;; Note that PACKAGE is initialized to self rather than to unbound, because otherwise
;;; the who-line can see the unbound symbol.
(DEFFLAVOR QUESTIONNAIRE-SEXP-ELEMENT ((PACKAGE PACKAGE)) (QUESTIONNAIRE-VALUE-ELEMENT)
  (:SETTABLE-INSTANCE-VARIABLES PACKAGE)
  (:DOCUMENTATION :COMBINATION
                  "A questionnaire element whose displayed value is a Lisp S-expression"))

(DEFMETHOD (QUESTIONNAIRE-SEXP-ELEMENT :NEW-VALUE-FROM-KEYBOARD) ()
  (LET ((STANDARD-INPUT SELF)
        (IBASE 10.))
    (READ-FOR-TOP-LEVEL)))


;;; One that takes from a set of choices, you could use TRUE and FALSE if you like.
;;; This one doesn't use the keyboard, but rotates around the set as you click with
;;; the mouse.
(DEFFLAVOR QUESTIONNAIRE-CHOICES-ELEMENT (CHOICES) (QUESTIONNAIRE-VALUE-ELEMENT)
  (:SETTABLE-INSTANCE-VARIABLES CHOICES)
  (:DOCUMENTATION :COMBINATION "A questionnaire element with a fixed set of values"))

;If no initial value given, default to the first choice.  You better
;give the choices in the init-plist!
(DEFMETHOD (QUESTIONNAIRE-CHOICES-ELEMENT :BEFORE :INIT) (INIT-PLIST)
  (OR (GET INIT-PLIST ':INITIAL-VALUE)
      (SETQ INITIAL-VALUE (FIRST CHOICES))))

;Clicking the mouse picks the next choice
(DEFMETHOD (QUESTIONNAIRE-CHOICES-ELEMENT :MOUSE-BUTTONS) (IGNORE IGNORE IGNORE)
  (FUNCALL-SELF ':SET-VALUE (OR (CADR (MEMQ VALUE CHOICES)) (CAR CHOICES))))


;;; Value elements that also call a function after the user changes the value
(DEFFLAVOR QUESTIONNAIRE-VALUE-FUNCTION-ELEMENT ()
           (QUESTIONNAIRE-FUNCTION-MIXIN QUESTIONNAIRE-VALUE-ELEMENT)
  (:DOCUMENTATION :COMBINATION
                  "A questionnaire value element that calls a function when value changed"))

(DEFFLAVOR QUESTIONNAIRE-INTEGER-FUNCTION-ELEMENT ()
           (QUESTIONNAIRE-FUNCTION-MIXIN QUESTIONNAIRE-INTEGER-ELEMENT)
  (:DOCUMENTATION :COMBINATION
                  "A questionnaire integer element that calls a function when value changed"))

(DEFFLAVOR QUESTIONNAIRE-SEXP-FUNCTION-ELEMENT ()
           (QUESTIONNAIRE-FUNCTION-MIXIN QUESTIONNAIRE-SEXP-ELEMENT)
  (:DOCUMENTATION :COMBINATION
                  "A questionnaire sexp element that calls a function when value changed"))

(DEFFLAVOR QUESTIONNAIRE-CHOICES-FUNCTION-ELEMENT ()
           (QUESTIONNAIRE-FUNCTION-MIXIN QUESTIONNAIRE-CHOICES-ELEMENT)
  (:DOCUMENTATION :COMBINATION
                  "A questionnaire choices element that calls a function when value changed"))


;;; This should come after all flavor definitions
(COMPILE-FLAVOR-METHODS QUESTIONNAIRE-FRAME
                        QUESTIONNAIRE-PANE-WITHOUT-LABEL QUESTIONNAIRE-PANE-WITH-LABEL
                        QUESTIONNAIRE-BUTTON-PANE QUESTIONNAIRE-ONOFF-ELEMENT
                        QUESTIONNAIRE-FUNCTION-BUTTON QUESTIONNAIRE-BIG-FUNCTION-BUTTON
                        QUESTIONNAIRE-VALUE-ELEMENT QUESTIONNAIRE-INTEGER-ELEMENT
                        QUESTIONNAIRE-SEXP-ELEMENT QUESTIONNAIRE-CHOICES-ELEMENT
                        QUESTIONNAIRE-VALUE-FUNCTION-ELEMENT
                        QUESTIONNAIRE-INTEGER-FUNCTION-ELEMENT
                        QUESTIONNAIRE-SEXP-FUNCTION-ELEMENT
                        QUESTIONNAIRE-CHOICES-FUNCTION-ELEMENT)

;Trace window.  I am trying to get something which looks like this:
;
;                       TRACE
;                      Function
;       Print
;       Break      When        Conditional
;       Step
;       Argpdl             Form
;       Wherein
;       CANCEL          DO IT           UNTRACE
;
;Most of these are function-button type elements.  Function contains the name of the function
;to do.  When lets you choose between Before, After, and Before&After.
;Conditional is usually a trace conditional, if you select Print or Break
;it becomes a conditional just for that, if you select argpdl or wherein it
;is renamed and used to read the argument to those.
;Form shows the trace form being built up.
;CANCEL normally cancels the whole thing, when lit it cancels just the current
;operation that is waiting for arguments.

;Should this be a resource?
(DEFVAR TRACE-WINDOW)
(DEFVAR TRACE-WINDOW-FORM)

(DEFUN MAKE-TRACE-WINDOW ()
  (SETQ TRACE-WINDOW (WINDOW-CREATE 'QUESTIONNAIRE-FRAME ':NAME "TRACE"
         ':LEFT 100 ':TOP 100
         ':WIDTH 1000 ':HEIGHT 600
         ':PANES '((PRINT QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Print"
                                :FUNCTION TRACE-WINDOW-PRINT-BUTTON)
                   (BREAK QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Break"
                                :FUNCTION TRACE-WINDOW-BREAK-BUTTON)
                   (STEP QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Step"
                                :FUNCTION TRACE-WINDOW-STEP-BUTTON)
                   (ARGPDL QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Arg pdl"
                                :FUNCTION TRACE-WINDOW-ARGPDL-BUTTON)
                   (WHEREIN QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Wherein"
                                :FUNCTION TRACE-WINDOW-WHEREIN-BUTTON)
                   (UNDO QUESTIONNAIRE-FUNCTION-BUTTON :NAME "Undo"
                                :FUNCTION TRACE-WINDOW-UNDO-BUTTON)
                   (CANCEL QUESTIONNAIRE-BIG-FUNCTION-BUTTON :NAME "Cancel"
                                :FUNCTION TRACE-WINDOW-CANCEL-BUTTON)
                   (DO-IT QUESTIONNAIRE-BIG-FUNCTION-BUTTON :NAME "Do It"
                                :FUNCTION TRACE-WINDOW-DO-IT-BUTTON)
                   (UNTRACE QUESTIONNAIRE-BIG-FUNCTION-BUTTON :NAME "UnTrace"
                                :FUNCTION TRACE-WINDOW-UNTRACE-BUTTON)
                   (FUNCTION QUESTIONNAIRE-SEXP-FUNCTION-ELEMENT :NAME "Function"
                                :CHARACTER-HEIGHT 2
                                :FUNCTION TRACE-WINDOW-FUNCTION-SPECIFIED)
                   (WHEN QUESTIONNAIRE-CHOICES-ELEMENT :NAME "When"
                         :CHOICES (BEFORE&AFTER BEFORE AFTER) :EDGES-FROM "BEFORE&AFTER ")
                   (CONDITIONAL QUESTIONNAIRE-SEXP-FUNCTION-ELEMENT :NAME "Conditional"
                                :CHARACTER-HEIGHT 2
                                :FUNCTION TRACE-WINDOW-CONDITIONAL-SPECIFIED)
                   (FORM QUESTIONNAIRE-PANE-WITHOUT-LABEL))
         ':CONSTRAINTS
              '((MAIN . ( (1DUMY)
                          ((1DUMY :HORIZONTAL (:EVEN)
                            (2DUMY)
                            ((2DUMY INTERDIGITATED-WHITESPACE :WHITE :INCLUDE  ;Vertical
                               (:EVEN) (:EVEN)
                               ;; We start with a vertical stack of the function name,
                               ;; the bulk of the cruft, and the 3 big buttons at the bottom
                               (FUNCTIONX 3DUMY 3-BIG-BUTTONS)
                               ((FUNCTIONX INTERDIGITATED-WHITESPACE :WHITE :INCLUDE ;Hor
                                  (:ASK-WINDOW FUNCTION :PANE-HEIGHT) (:EVEN)
                                  (FUNCTION)
                                  ((FUNCTION 0.9)))
                                (3-BIG-BUTTONS INTERDIGITATED-WHITESPACE :WHITE :INCLUDE ;Hor
                                  (:ASK-WINDOW CANCEL :PANE-HEIGHT) (:EVEN)
                                  (CANCEL DO-IT UNTRACE)
                                  ((CANCEL :ASK :PANE-WIDTH)
                                   (DO-IT :ASK :PANE-WIDTH)
                                   (UNTRACE :ASK :PANE-WIDTH))))
                               ((3DUMY INTERDIGITATED-WHITESPACE :WHITE :INCLUDE  ;Horizontal
                                  (0.85) (:EVEN)
                                  ;; This consists of the function buttons in a column
                                  ;; on the left, and to the right of that the When
                                  ;; and Conditional above the Form.
                                  (SMALL-BUTTONS 4DUMY)
                                  ((SMALL-BUTTONS INTERDIGITATED-WHITESPACE :WHITE :EXCLUDE ;V
                                     (:ASK-WINDOW PRINT :PANE-WIDTH) (:EVEN)
                                     (PRINT BREAK STEP ARGPDL WHEREIN UNDO)
                                     ((PRINT :ASK :PANE-HEIGHT) (BREAK :ASK :PANE-HEIGHT)
                                      (STEP :ASK :PANE-HEIGHT) (ARGPDL :ASK :PANE-HEIGHT)
                                      (WHEREIN :ASK :PANE-HEIGHT) (UNDO :ASK :PANE-HEIGHT))))
                                  ((4DUMY INTERDIGITATED-WHITESPACE :WHITE :EXCLUDE ;Vertical
                                     (0.9) (:EVEN)
                                     (5DUMY FORM)
                                     ((5DUMY INTERDIGITATED-WHITESPACE :WHITE :EXCLUDE ;Hor
                                        ;; I really just want the max of the two heights
                                        (:ASK-WINDOW CONDITIONAL :PANE-HEIGHT) (:EVEN)
                                        (WHEN CONDITIONAL)
                                        ((WHEN :ASK :PANE-WIDTH))
                                        ((CONDITIONAL 0.9))))
                                     ((FORM 0.9))))))))))))))))

(DEFUN CALL-TRACE-WINDOW (&OPTIONAL FUNCTION (PKG PACKAGE))
  (OR (BOUNDP 'TRACE-WINDOW) (MAKE-TRACE-WINDOW))
  (FUNCALL TRACE-WINDOW ':BROADCAST ':QUESTIONNAIRE-RESET)
  (FUNCALL TRACE-WINDOW ':SEND-ELEMENT 'CONDITIONAL ':SET-NAME "Conditional")
  (FUNCALL TRACE-WINDOW ':SEND-ELEMENT 'FUNCTION ':SET-PACKAGE PKG)
  (FUNCALL TRACE-WINDOW ':SEND-ELEMENT 'CONDITIONAL ':SET-PACKAGE PKG)
  (SETQ TRACE-WINDOW-FORM NIL)          ;Should perhaps be some sort of instance variable?
  (IF FUNCTION (FUNCALL TRACE-WINDOW ':SET-ELEMENT-VALUE 'FUNCTION FUNCTION)
      (FUNCALL TRACE-WINDOW ':SET-ELEMENT-ACCENT 'FUNCTION T))
  (IF FUNCTION (TRACE-WINDOW-FUNCTION-SPECIFIED TRACE-WINDOW 'FUNCTION 1 FUNCTION)
      (FUNCALL TRACE-WINDOW ':SEND-ELEMENT 'FORM ':CLEAR-SCREEN))
  (FUNCALL TRACE-WINDOW ':SELECT T)
  T)

;Magic arguments are frame, pane-name, buttons, new-value or accent-state

;This is called when a new value has been specified in the function window.
;Initialize the trace form, but if there's already options in it retain them.
(DEFUN TRACE-WINDOW-FUNCTION-SPECIFIED (W IGNORE IGNORE FUNCTION)
  (LET ((OPTIONS (COND ((NULL TRACE-WINDOW-FORM) NIL)   ;Parse out current options
                       ((EQ (CAADR TRACE-WINDOW-FORM) ':FUNCTION) (CDDADR TRACE-WINDOW-FORM))
                       (T (CDADR TRACE-WINDOW-FORM)))))
    (PUSH FUNCTION OPTIONS)                     ;Put function onto front of options list
    (AND (OR (LISTP FUNCTION) (EQ FUNCTION ':FUNCTION))
         (PUSH ':FUNCTION OPTIONS))
    (SETQ TRACE-WINDOW-FORM (LIST 'TRACE OPTIONS))
    (FUNCALL W ':SET-ELEMENT-ACCENT 'FUNCTION NIL)
    (TRACE-WINDOW-REDISPLAY W)))

;CLAUSE is a list which is nconc'ed onto the end of the trace options
(DEFUN TRACE-WINDOW-ADD-CLAUSE (W CLAUSE &OPTIONAL INHIBIT-REDISPLAY)
  (OR TRACE-WINDOW-FORM
      (SETQ TRACE-WINDOW-FORM (SUBST NIL NIL '(TRACE (:FUNCTION "not yet specified")))))
  (DOLIST (PIECE CLAUSE)
    (NCONC (CADR TRACE-WINDOW-FORM) (NCONS PIECE)))
  (OR INHIBIT-REDISPLAY (TRACE-WINDOW-REDISPLAY W)))

(DEFUN TRACE-WINDOW-REDISPLAY (W)
  (LET ((WW (FUNCALL W ':FIND-PANE 'FORM)))     ;Redisplay the form
    (FUNCALL WW ':CLEAR-SCREEN)
    (GRIND-TOP-LEVEL TRACE-WINDOW-FORM (FUNCALL WW ':SIZE-IN-CHARACTERS) WW)))

;This is used to make function buttons light up while they are executing.
;Useful for making the display more busy, and confirmation that it is working.
(DEFMACRO WITH-TEMPORARY-ACCENT ((FRAME PANE-NAME) . BODY)
  `(LET ((.OLD-ACCENT. (FUNCALL ,FRAME ':ELEMENT-ACCENT ,PANE-NAME)))
     (UNWIND-PROTECT
       (PROGN (FUNCALL ,FRAME ':SET-ELEMENT-ACCENT ,PANE-NAME (NOT .OLD-ACCENT.))
              . ,BODY)
       (FUNCALL ,FRAME ':SET-ELEMENT-ACCENT ,PANE-NAME .OLD-ACCENT.))))

;This is called when someone clicks on the Print button.  Normally, it puts you
;into "print" mode, lighting Print, When, Cancel, and Form (over Conditional).
;But when lit, it completes Print mode, adding to the form.
(DEFUN TRACE-WINDOW-PRINT-BUTTON (W IGNORE IGNORE MODE)
  (COND ((NOT MODE)                                     ;Not lit
          (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)        ;Cancel special modes
          (FUNCALL W ':SET-ELEMENT-ACCENT 'PRINT T)     ;Light appropriate frobs
          (FUNCALL W ':SET-ELEMENT-ACCENT 'WHEN T)
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CANCEL T)
          (FUNCALL W ':SEND-ELEMENT 'CONDITIONAL ':SET-NAME "Form to print")
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CONDITIONAL T))
        (T                                      ;Lit, set up a PRINT operation
          (LET ((FORM (FUNCALL W ':ELEMENT-VALUE 'CONDITIONAL))
                (WHEN (FUNCALL W ':ELEMENT-VALUE 'WHEN)))
            (TRACE-WINDOW-ADD-CLAUSE W (LIST (SELECTQ WHEN (BEFORE ':ENTRYPRINT)
                                                           (AFTER ':EXITPRINT)
                                                           (OTHERWISE ':PRINT))
                                             FORM))
            (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)))))  ;Cancel special modes

;This is called when someone clicks on the Break button.  Normally, it puts you
;into "break" mode, lighting Break, When, Cancel, and Conditional.
;But when lit, it completes Break mode, adding to the form.
(DEFUN TRACE-WINDOW-BREAK-BUTTON (W IGNORE IGNORE MODE)
  (COND ((NOT MODE)                                     ;Not lit
          (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)        ;Cancel special modes
          (FUNCALL W ':SET-ELEMENT-ACCENT 'BREAK T)     ;Light appropriate frobs
          (FUNCALL W ':SET-ELEMENT-ACCENT 'WHEN T)
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CANCEL T)
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CONDITIONAL T))
        (T                                      ;Lit, set up a BREAK operation
          (LET ((COND (FUNCALL W ':ELEMENT-VALUE 'CONDITIONAL))
                (WHEN (FUNCALL W ':ELEMENT-VALUE 'WHEN)))
            (AND (EQUAL COND "") (SETQ COND T)) ;Unspecified means always (superfluous code)
            (OR (EQ WHEN 'AFTER) (TRACE-WINDOW-ADD-CLAUSE W `(:BREAK ,COND) T))
            (OR (EQ WHEN 'BEFORE) (TRACE-WINDOW-ADD-CLAUSE W `(:EXITBREAK ,COND)))
            (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)))))  ;Cancel special modes

;This is called when someone clicks on the Step button.  It has no options.
(DEFUN TRACE-WINDOW-STEP-BUTTON (W P IGNORE IGNORE)
  (WITH-TEMPORARY-ACCENT (W P)
    (TRACE-WINDOW-ADD-CLAUSE W '(:STEP))))

;This is called when someone clicks on the Undo button.  Remove the last
;option clause from the trace form.
(DEFUN TRACE-WINDOW-UNDO-BUTTON (W P IGNORE IGNORE)
  (WITH-TEMPORARY-ACCENT (W P)
    (DO ((L (CADR TRACE-WINDOW-FORM) (CDR L))   ;Find the place where option list should end
         (LAST NIL))
        ((NULL L)
         (IF (NULL LAST) (BEEP) (RPLACD LAST NIL)))
      ;; If a clause starts after here, remember here
      (AND (MEMQ (CADR L) '(:ENTRYPRINT :EXITPRINT :PRINT :BREAK :EXITBREAK :STEP
                            :ARGPDL :WHEREIN :COND :ENTRYCOND :EXITCOND))
           (SETQ LAST L)))
    (TRACE-WINDOW-REDISPLAY W)))

;This is called when someone clicks on the Argpdl button.  Need to get an argument.
(DEFUN TRACE-WINDOW-ARGPDL-BUTTON (W IGNORE IGNORE MODE)
  (COND ((NOT MODE)                                     ;Not lit
          (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)        ;Cancel special modes
          (FUNCALL W ':SET-ELEMENT-ACCENT 'ARGPDL T)    ;Light appropriate frobs
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CANCEL T)
          (FUNCALL W ':SEND-ELEMENT 'CONDITIONAL ':SET-NAME "Argpdl variable")
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CONDITIONAL T))
        (T                                      ;Lit, complete operation
          (LET ((VAR (FUNCALL W ':ELEMENT-VALUE 'CONDITIONAL)))
            (TRACE-WINDOW-ADD-CLAUSE W `(:ARGPDL ,VAR))
            (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)))))

;This is called when someone clicks on the Wherein button.  Need to get an argument.
(DEFUN TRACE-WINDOW-WHEREIN-BUTTON (W IGNORE IGNORE MODE)
  (COND ((NOT MODE)                                     ;Not lit
          (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)        ;Cancel special modes
          (FUNCALL W ':SET-ELEMENT-ACCENT 'WHEREIN T)   ;Light appropriate frobs
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CANCEL T)
          (FUNCALL W ':SEND-ELEMENT 'CONDITIONAL ':SET-NAME "Wherein function")
          (FUNCALL W ':SET-ELEMENT-ACCENT 'CONDITIONAL T))
        (T                                      ;Lit, complete operation
          (LET ((FCN (FUNCALL W ':ELEMENT-VALUE 'CONDITIONAL)))
            (TRACE-WINDOW-ADD-CLAUSE W `(:WHEREIN ,FCN))
            (TRACE-WINDOW-CANCEL-BUTTON W NIL 0 T)))))

;This is called when someone clicks on the Do It button.  Call trace.
(DEFUN TRACE-WINDOW-DO-IT-BUTTON (W IGNORE IGNORE IGNORE)
  (LET ((TERMINAL-IO (FUNCALL W ':FIND-PANE 'FORM))
        (ERROR-OUTPUT SI:SYN-TERMINAL-IO))
    (COND ((ERRSET (EVAL TRACE-WINDOW-FORM))
           (FUNCALL W ':DESELECT T)
           (FUNCALL W ':DEACTIVATE)))))

;MODE is NIL if cancelling the whole thing, T ("when lit") if cancelling a special mode
(DEFUN TRACE-WINDOW-CANCEL-BUTTON (W IGNORE IGNORE MODE)
  (COND (MODE                                   ;When lit
         (DOLIST (X '(PRINT BREAK ARGPDL WHEREIN WHEN CANCEL CONDITIONAL))
           (FUNCALL W ':SET-ELEMENT-ACCENT X NIL))
         (FUNCALL W ':SEND-ELEMENT 'CONDITIONAL ':SET-NAME "Conditional")
         (FUNCALL W ':SET-ELEMENT-VALUE 'CONDITIONAL ""))
        (T (FUNCALL W ':DESELECT T)
           (FUNCALL W ':DEACTIVATE))))

;Called when someone clicks on the conditional/utility pane.  This can conditionalize
;the whole trace, provided it is not lit and not being used for something else.
;Otherwise, the value that was just entered will be picked up later by another function.
(DEFUN TRACE-WINDOW-CONDITIONAL-SPECIFIED (W P IGNORE COND)
  (LET ((WW (FUNCALL W ':FIND-PANE P)))
    (COND ((AND (NULL (FUNCALL WW ':ACCENT)) (EQUAL (SHEET-NAME WW) "Conditional"))
           (LET ((WHEN (FUNCALL W ':ELEMENT-VALUE 'WHEN)))
             (TRACE-WINDOW-ADD-CLAUSE W (LIST (SELECTQ WHEN (BEFORE ':ENTRYCOND)
                                                            (AFTER ':EXITCOND)
                                                            (OTHERWISE ':COND))
                                              COND)))))))

;******* To be done *******
;The Untrace button.  Wants to reformat and give a menu of guys to untrace, or "all".
;Refreshing the frame needs to refresh the form window.  Good general mechanism?

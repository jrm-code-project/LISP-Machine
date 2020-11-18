;;; -*- Mode:LISP; Package:TV; Base:8 -*-

;;; documentation for window system. operations.  If window system operations are changed or
;;; added, this file should be updated.

;;; operations from BASWIN

(DEFOPERATION (ESSENTIAL-WINDOW :MOUSE-SELECT) ()
  "Selects a window, for mouse click or asynchronous keyboard input.
Typeahead goes to previously selectewd window.")

(DEFOPERATION (ESSENTAIL-WINDOW :LISP-LISTENER-P) ()
  ":IDLE if this window is a lisp listener which is ready to accept input.
:BUSY if it is lisp listener, but is doing something else.
NIL is it does not support a lisp read-eval-print loop.")

(DEFOPERATION (ESSENTAIL-WINDOW :SELECTABLE-WINDOWS) ()
  "Returns inferiors to all levels that are selectable in a form suitable for
use as a menu item-list.")

(DEFOPERATION (ESSENTIAL-ACTIVATE :BURY) ()
  "Deexposes this window and gives it the lowest priority in its grouping for reexposure.")

(DEFOPERATION (ESSENTIAL-ACTIVATE :STATUS) ()
  "Returns one of :SELECTED :EXPOSED :EXPOSED-IN-SUPERIOR :DEEXPOSED :DEACTIVATED")

(DEFOPERATION (ESSENTIAL-ACTIVATE :ACTIVE-P) ()
  "T if this window is active in its superior, or if this is a screen.")

(DEFOPERATION (ESSENTIAL-ACTIVATE :SET-STATUS) (NEW-STATUS)
  "Makes this window's status NEW-STATUS by (de)selecting, (de)exposing (de)activating as
necessary.
Meaningful values for NEW-STATUS are :SELECTED :EXPOSED :EXPOSED-IN-SUPERIOR :DEEXPOSED :DEACTIVATED.")

(DEFOPERATION (SELECT-MIXIN :NAME-FOR-SELECTION) ()
  "Returns this window's label if it has one, or else it's name, or else NIL.
This should return a string suitable for display in the system menu Select menu.")

(DEFOPERATION (SELECT-MIXIN :PROCESS) ()
  "Returns the process associated with this window.")

(DEFOPERATION (SELECT-MIXIN :SET-PROCESS) (PROCESS)
  "Set the process associated with this window to PROCESS.")

(DEFOPERATION (SELECT-MIXIN :CALL) ()
  "Selects an idle lisp listener (which may be this window) If the window selected is not
this one, this window's process is reset with arrest-reason :CALL.")

(DEFOPERATION (SELECT-MIXIN :ARREST) ()
  "Arrests the process asscoiated with this window.")

(DEFOPERATION (SELECT-MIXIN :UN-ARREST) ()
  "Un-arrests the process asscoiated with this window.")

(DEFOPERATION (SELECT-MIXIN :SELECT) ()
  "Makes this window (or its substitute) the selected window.")

(DEFOPERATION (SELECT-MIXIN :MOUSE-SELECT) ()
  "Selects a window, for mouse click or asynchronous keyboard input.
Typeahead goes to previously selectewd window.
Clears any temp locks on this window. Fails if thsi window does not lie within its superior.")

(DEFOPERATION (SELECT-MIXIN :DESELECT) (&OPTIONAL (RESTORE-SELECTED T))
  "Deselects this window (or its substitute)
RESTORE-SELECTED says where to put this window in TV:PREVIOUSLY-SELECTED-WINDOWS.
:DONT-SAVE means don't put it anywhere, and don't select any other window.
NIL or :BEGGINING means put this window at the front of the list. Don't select any window.
:END end of the list. Don't select another window.
:FIRST means put this window at the front of the list, after selecting the window which used
  to be at the front
:LAST or T (the default) Put this window at the end, and select the window at the front.")

(DEFOPERATION (ESSENTAIL-WINDOW :SET-SELECTION-SUBSTITUTE) (WINDOW)
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :REMOVE-SELECTION-SUBSTITUTE) (REMOVE-WINDOW SUGGEST-WINDOW)
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :ULTIMATE-SELECTION-SUBSTITUTE) ()
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :SELF-OR-SUBSTITUTE-SELECTED-P) ()
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :ALIAS-FOR-SELECTED-WINDOWS) ()
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :ALIAS-FOR-INFERIORS) ()
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-EDGES) (NEW-LEFT NEW-RIGHT NEW-TOP NEW-BOTTOM &OPTIONAL OPTION)
  "
OPTION may be :VERIFY, meaning see whether these edges are valid.")

(DEFOPERATION (ESSENTAIL-SET-EDGES :VERIFY-NEW-EDGES) (NEW-LEFT NEW-TOP NEW-WIDTH NEW-HEIGHT)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-INSIDE-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :FULL-SCREEN) (&OPTIONAL OPTION)
  "Sets the (outside) edges of this window to the ouside of its superior.
If OPTION is :VERIFY then just check if this operation is possible.")

(DEFOPERATION (ESSENTAIL-SET-EDGES :CENTER-AROUND) (X Y)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :EXPOSE-NEAR) (X Y)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :REDEFINE-MARGINS) ()
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-BORDER-MARGIN-WIDTH) (NEW-WIDTH)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-BORDERS) (NEW-BORDERS)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :COMPUTE-MARGINS) (LEFT-MARGIN RIGHT-MARGIN TOP-MARGIN BOTTOM-MARGIN)
  "")

(DEFOPERATION (ESSENTAIL-SET-EDGES :SET-BORDERS-INTERNAL) (SPEC LEFT-MARGIN RIGHT-MARGIN TOP-MARGIN BOTTOM-MARGIN)
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :LABEL-SIZE) ()
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :SET-LABEL) (NEW-LABEL)
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :LABEL-SIZE) ()
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :COMPUTE-MARGINS) (LEFT-MARGIN RIGHT-MARGIN TOP-MARGIN BOTTOM-MARGIN)
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :PARSE-LABEL-SPEC) (LEFT-MARGIN RIGHT-MARGIN TOP-MARGIN BOTTOM-MARGIN &OPTIONAL TOP-P)
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :LABEL-SIZE) ()
  "")

(DEFOPERATION (ESSENTIAL-LABEL-MIXIN :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  "")

(DEFOPERATION (DELAYED-REDISPLAY-LABEL-MIXIN :DELAYED-SET-LABEL) (NEW-LABEL)
  "")

(DEFOPERATION (DELAYED-REDISPLAY-LABEL-MIXIN :UPDATE-LABEL) ()
  "")

(DEFOPERATION (CHANGEABLE-NAME-MIXIN :NAME) ()
  "")

(DEFOPERATION (CHANGEABLE-NAME-MIXIN :SET-NAME) (NEW-NAME)
  "")

(DEFOPERATION (PROCESS-MIXIN :PROCESS) ()
  "")

(DEFOPERATION (LISTENER-MIXIN-INTERNAL :PACKAGE) ()
  "")

(DEFOPERATION (LISTENER-MIXIN-INTERNAL :SET-PACKAGE) (NEW-PACKAGE)
  "")

(DEFOPERATION (POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  "")

(DEFOPERATION (TRUNCATING-POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  "")

(DEFOPERATION (RESET-ON-OUTPUT-HOLD-FLAG-MIXIN :RESET-ON-OUTPUT-HOLD-FLAG) ()
  "")

(DEFOPERATION (NOTIFICATION-MIXIN :PRINT-NOTIFICATION) (TIME STRING WINDOW-OF-INTEREST)
  "")

(DEFOPERATION (NOTIFICATION-MIXIN :PRINT-NOTIFICATION-ON-SELF) (TIME STRING WINDOW-OF-INTEREST)
  "")

(DEFOPERATION (POP-UP-NOTIFICATION-WINDOW :SET-WINDOW-OF-INTEREST) (WINDOW)
  "")

(DEFOPERATION (ESSENTAIL-WINDOW :AWAIT-EXPOSURE) ()
  "")

;;; operations from BASSTR

(DEFOPERATION (BACKGROUND-LISP-INTERACTOR :WAIT-UNTIL-SEEN) ()
  "")

;;; operations from SHEET

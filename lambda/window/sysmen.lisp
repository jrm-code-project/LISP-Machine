;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Operations for moving, reshaping and creating windows,
;;; and menus to get them from.

;; Copied from LAD: RELEASE-3.WINDOW; SYSMEN.LISP#195 on 2-Oct-86 04:04:58
;;;; Item lists for the System Menu
(DEFVAR *SYSTEM-MENU-WINDOWS-COLUMN*            ;General window operations
      '(("Create" :FUNCALL SYSTEM-MENU-CREATE-WINDOW
         :DOCUMENTATION " Create a new window.  Flavor of window selected from a menu.")
        ("Select" :FUNCALL SYSTEM-MENU-SELECT-WINDOW
         :DOCUMENTATION " Select a window from a menu.")
        ("Split Screen" :FUNCALL SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS
         :DOCUMENTATION " Create a split screen configuration.  Options from menu.")
        ("Layouts" :FUNCALL SYSTEM-MENU-LAYOUTS
         :DOCUMENTATION " Save//restore current screen configuration.  Options from menu.")
        ("Edit Screen" :BUTTONS
                ((NIL :EVAL (EDIT-SCREEN MOUSE-SHEET))
                 (NIL :EVAL (EDIT-SCREEN MOUSE-SHEET))
                 (NIL :WINDOW-OP (LAMBDA (WINDOW IGNORE IGNORE)
                                   (SETQ WINDOW (SCREEN-EDITOR-FIND-SCREEN-TO-EDIT WINDOW))
                                   (AND WINDOW (EDIT-SCREEN WINDOW)))))
                :DOCUMENTATION " Edit a screen.  Left edits screen the mouse is on, right button gives menu of frames."
                )
        ("Set Mouse Screen" :BUTTONS ((NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN NIL))
                                      (NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN NIL))
                                      (NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN T)))
         :DOCUMENTATION " Set the screen the mouse is on.  Left defaults if possible, right always uses menu."))
  "List of items that go in the first column of the system menu.")

;; Copied from LAD: RELEASE-3.WINDOW; SYSMEN.LISP#195 on 2-Oct-86 04:04:59
(DEFVAR *SYSTEM-MENU-THIS-WINDOW-COLUMN*        ;Operations on window mouse is over
      '(("Kill" :WINDOW-OP SYSTEM-MENU-KILL-WINDOW
         :DOCUMENTATION " Kill the window that the mouse is over.")
        ("Refresh" :WINDOW-OP (LAMBDA (WINDOW IGNORE IGNORE)
                                (AND WINDOW (SEND WINDOW :REFRESH)))
         :DOCUMENTATION " Refresh the window that the mouse is over.")
        ("Bury" :WINDOW-OP SYSTEM-MENU-BURY-WINDOW
         :DOCUMENTATION
            " Bury the window that the mouse is over, beneath all other active windows.")
        ("Attributes" :WINDOW-OP SYSTEM-MENU-EDIT-WINDOW-ATTRIBUTES
         :DOCUMENTATION
            " View or change the attributes of the window that the mouse is over.")
        ("Reset" :WINDOW-OP SYSTEM-MENU-RESET-WINDOW
         :DOCUMENTATION
            " Reset the process that can be found via the window the mouse is over.")
        ("Arrest" :WINDOW-OP (LAMBDA (WINDOW IGNORE IGNORE)
                               (AND WINDOW (SEND WINDOW :ARREST)))
         :DOCUMENTATION
            " Arrest the process that can be found via the window the mouse is over.")
        ("Un-Arrest" :WINDOW-OP
         (LAMBDA (WINDOW IGNORE IGNORE)
           (AND WINDOW (SEND WINDOW :UN-ARREST)))
         :DOCUMENTATION
            " Un-arrest the process that can be found via the window the mouse is over (inverse of Arrest)."))
  "List of items that go in the second column of the system menu.")

;; Copied from LAD: RELEASE-3.WINDOW; SYSMEN.LISP#195 on 2-Oct-86 04:05:00
(DEFVAR *SYSTEM-MENU-PROGRAMS-COLUMN*           ;Invoke the most commonly-needed programs
        `(("Lisp" :EVAL (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'LISTENER-MIXIN 'LISP-LISTENER)
           :DOCUMENTATION " Select a Lisp listener, to evaluate Lisp forms.")
          ("Edit" :EVAL (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'ZWEI:ZMACS-FRAME)
           :DOCUMENTATION " Select an Editor, to edit text or write a program.")
          ("Inspect" :EVAL (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'INSPECT-FRAME)
           :DOCUMENTATION " Select an Inspector, to browse through data structure.")
          ("Trace" :FUNCALL TRACE-VIA-MENUS
           :DOCUMENTATION " Trace a function.  Options selected from menu.")
          ("Peek" :EVAL (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'PEEK-FRAME)
           :DOCUMENTATION " Select Peek, to look at system activity.")
          ("Mail" :EVAL (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'ZWEI:ZMAIL-FRAME)
           :DOCUMENTATION " Select ZMail, to read or send mail.")
          ("Emergency Break"
           :EVAL (PROCESS-RUN-FUNCTION "Emergency Break" #'KBD-USE-COLD-LOAD-STREAM)
           :DOCUMENTATION
           " Evaluate Lisp forms without using the window system.  ** Use with caution **")
          )
  "List of columns that go in the third column of the system menu.")

(DEFUN ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN (NAME FORM DOCUMENTATION &OPTIONAL AFTER)
  "Add an item named NAME to the third column of the system menu.
FORM is what to execute if the user clicks on the item,
and DOCUMENTATION is the mouse documentation string.
AFTER is the name of an item to add after (a string).
AFTER=T means add at the top.  NIL means add at the bottom."
  (LET ((ITEM `(,NAME :EVAL ,FORM :DOCUMENTATION ,DOCUMENTATION))
        (OLD))
  (IF (SETQ OLD (ASS #'EQUALP NAME *SYSTEM-MENU-PROGRAMS-COLUMN*))
      (SETF (CDR OLD) (CDR ITEM))
    (SETQ *SYSTEM-MENU-PROGRAMS-COLUMN*
          (IF (EQ AFTER T)
              (CONS ITEM *SYSTEM-MENU-PROGRAMS-COLUMN*)
            (LOOP WITH AFTER = (OR (ASS 'EQUALP AFTER *SYSTEM-MENU-PROGRAMS-COLUMN*)
                                   (CAR (LAST *SYSTEM-MENU-PROGRAMS-COLUMN*)))
                  FOR X IN *SYSTEM-MENU-PROGRAMS-COLUMN*
               COLLECT X
               WHEN (EQ X AFTER)
                 COLLECT ITEM))))))

(defvar *system-menu-system-column*
        `(("Emergency Break"
           :EVAL (PROCESS-RUN-FUNCTION "Emergency Break" #'KBD-USE-COLD-LOAD-STREAM)
           :DOCUMENTATION
           "Evaluate Lisp forms without using the window system.  ** Use with caution **")
;         ,@(when (memq si:site-name '(:lmi :mit))
;             '(("Halt Processor"
;                :eval (si:%halt)
;                :documentation
;                "Halt the processor. * Be sure you want to do this *")
;               ("Emergency Power Off"
;                :eval (process-run-function "Emergency Power Off" 'kbd-power-off-make-sure)
;                :documentation
;                "Turn off the power to the machine.  **!!! Use with extreme caution !!!**"))]
          )
          "List of columns that go in the third column of the system menu.")

;(defun kbd-power-off-make-sure (&rest ignore)
;  (if (mouse-y-or-n-p "Really turn off the power?")
;      (kbd-power-off)))

;(defun kbd-power-off (&rest ignore)
;  "Yow!"
;  (%multibus-write-8 #x1c080 #x80))

;(defun kbd-power-on (&rest ignore)
;  (format t "The power is already on."))

;;; Maybe someone else will want a without-power macro...
;(defmacro without-power (&rest forms)
;  "Execute forms with the power turned off."
;  `(UNWIND-PROTECT
;       (PROGN (KBD-POWER-OFF)
;             ,@forms)
;     (KBD-POWER-ON)))

(defvar *system-menu-column-header-font* fonts:hl12i)

(defvar *system-menu-columns*
        '(("Windows"     *SYSTEM-MENU-WINDOWS-COLUMN*)
          ("This window" *SYSTEM-MENU-THIS-WINDOW-COLUMN*)
          ("Programs"    *SYSTEM-MENU-PROGRAMS-COLUMN*)
;         ("System"      *system-menu-system-column*)   ;Pace didn't like the "System" column.
          ))

(DEFWINDOW-RESOURCE SYSTEM-MENU ()
  "Resource of system menus"
  :MAKE-WINDOW (DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU
                 :COLUMN-SPEC-LIST
                 (mapcar #'(lambda (spec)    ;Add font info to specifiers.
                             (append spec '(:font *system-menu-column-header-font*)))
                         *system-menu-columns*)
                 :SAVE-BITS T)
  :REUSABLE-WHEN :DEEXPOSED)

(DEFVAR DEFAULT-WINDOW-TYPES-ITEM-LIST
      '(("Supdup" :EVAL SUPDUP:SUPDUP-FLAVOR :DOCUMENTATION "Supdup to ITS or local TOPS-20.")
        ("Telnet" :VALUE SUPDUP:TELNET :DOCUMENTATION "TELNET to Chaos or ARPAnet host.")
        ("Lisp" :VALUE LISP-LISTENER
         :DOCUMENTATION "A READ-EVAL-PRINT loop in separate process.")
        ("Edit" :VALUE ZWEI:ZMACS-FRAME
         :DOCUMENTATION "An editor, sharing buffers with other editors.")
        ("Peek" :VALUE PEEK-FRAME :DOCUMENTATION "Display status information.")
        ("Inspect" :VALUE INSPECT-FRAME :DOCUMENTATION "Browse through data structure.")
        ("Font Edit" :VALUE FED:FED-FRAME :DOCUMENTATION "Edit characters in fonts.")
        ("Lisp (Edit)" :VALUE ZWEI:EDITOR-TOP-LEVEL
         :DOCUMENTATION
         "A READ-EVAL-PRINT loop in separate process with editing capabilities.")
        ("Any" :VALUE T :FONT :MENU-STANDOUT
         :DOCUMENTATION "Prompts for any flavor name."))
  "Item list of the Create menu for creating inferiors of screens.")

;;; This variable is usually bound to something appropriate when using the menus that
;;; depend on it.
;;; But it needs a global value so that the initial copy of the menu can get created.
(DEFVAR WINDOW-TYPES-ITEM-LIST DEFAULT-WINDOW-TYPES-ITEM-LIST
  "Item list for the Create menu to use right now.
When creating a pane of a frame, this is bound to the pane types
of that frame.  At top level, it DEFAULT-WINDOW-TYPES-ITEM-LIST.")

;;; Resource of menus of flavors of windows user can create with mouse
(DEFWINDOW-RESOURCE WINDOW-TYPE-MENU ()
  :MAKE-WINDOW (DYNAMIC-MOMENTARY-MENU :ITEM-LIST-POINTER 'WINDOW-TYPES-ITEM-LIST
                                       :SAVE-BITS T)
  :REUSABLE-WHEN :DEEXPOSED)

(DEFMETHOD (SHEET :PANE-TYPES-ALIST) ()
  (SEND SUPERIOR :PANE-TYPES-ALIST))

(DEFMETHOD (SCREEN :PANE-TYPES-ALIST) ()
  DEFAULT-WINDOW-TYPES-ITEM-LIST)

(DEFUN SELECTABLE-WINDOWS (SUP)
  "Get a list of all windows that should appear in the Select menu."
  (SEND SUP :SELECTABLE-WINDOWS))

;;; Resource of menus of windows that user can select
(DEFWINDOW-RESOURCE SELECTABLE-WINDOWS-MENU ()
  :MAKE-WINDOW (DYNAMIC-MOMENTARY-MENU
                 :ITEM-LIST-POINTER '(MAPCAN #'SELECTABLE-WINDOWS ALL-THE-SCREENS)
                 :SAVE-BITS NIL)                ;changes every time anyway
  :REUSABLE-WHEN :DEEXPOSED)

(DEFUN SYSTEM-MENU-SELECT-WINDOW (&OPTIONAL (SUP MOUSE-SHEET))
  "Offer the user the Select menu and select a window if he says so.
The menu contains the inferiors of SUP."
  (LET ((MENU (ALLOCATE-RESOURCE 'SELECTABLE-WINDOWS-MENU SUP)))
    (COND ((NULL (SEND MENU :ITEM-LIST))
           (BEEP)
           (POP-UP-MESSAGE "Error: There are no windows that can be selected."))
          (T
           (LET ((WINDOW (SEND MENU :CHOOSE)))
             (AND WINDOW (MOUSE-SELECT WINDOW)))))))

;;; Must return the window or NIL
(DEFUN SYSTEM-MENU-CREATE-WINDOW (&OPTIONAL (SUP MOUSE-SHEET) (EDGES-FROM 'MOUSE))
  "Offer the user the Create menu and let him create an inferior of SUP.
EDGES-FROM is passed to EXPOSE-WINDOW-NEAR; :MOUSE means ask the
user to specify the corners.  Returns a window or NIL."
  (LET* ((WINDOW-TYPES-ITEM-LIST (SEND SUP :PANE-TYPES-ALIST))
         (WINDOW-TYPE (COND ((NULL WINDOW-TYPES-ITEM-LIST)
                             (BEEP)
                             NIL)
                            ((NULL (CDR WINDOW-TYPES-ITEM-LIST))
                             (MENU-EXECUTE-NO-SIDE-EFFECTS
                               (CAR WINDOW-TYPES-ITEM-LIST)))
                            (T
                             (SEND (ALLOCATE-RESOURCE 'WINDOW-TYPE-MENU) :CHOOSE))))
         (MS MOUSE-SHEET))
    (AND WINDOW-TYPE (CONSP WINDOW-TYPE)
         (SETQ WINDOW-TYPE (EVAL WINDOW-TYPE)))
    (UNWIND-PROTECT (PROGN (AND (EQ WINDOW-TYPE T)      ;"Any"
                                (SETQ WINDOW-TYPE
                                      (GET-WINDOW-TYPE-FROM-KEYBOARD MOUSE-SHEET :EDGES-FROM)))
                           (COND (WINDOW-TYPE
                                  (MOUSE-SET-SHEET SUP)
                                  (CREATE-WINDOW-WITH-MOUSE WINDOW-TYPE EDGES-FROM))))
                    (MOUSE-SET-SHEET MS))))

;;; Must return the window or NIL
(DEFUN CREATE-WINDOW-WITH-MOUSE (FLAVOR-NAME &OPTIONAL (EDGES-FROM 'MOUSE) &AUX TEM)
  "Create and return a window of FLAVOR-NAME, using mouse to get edges.
EDGES is how to get the edges, and :MOUSE or :EXPAND will use
the mouse to get them."
  (AND FLAVOR-NAME
       ;; Get the edges before creating the window so can abort.
       (CAR (SETQ TEM
                  (SELECTQ EDGES-FROM
                    (MOUSE
                      (LET* ((INIT-PLIST (SI:FLAVOR-DEFAULT-INIT-PLIST FLAVOR-NAME))
                             (MINIMUM-WIDTH (GET INIT-PLIST ':MINIMUM-WIDTH 0))
                             (MINIMUM-HEIGHT (GET INIT-PLIST ':MINIMUM-HEIGHT 0)))
                        (MULTIPLE-VALUE-LIST
                          (MOUSE-SPECIFY-RECTANGLE NIL NIL NIL NIL MOUSE-SHEET
                                                   MINIMUM-WIDTH MINIMUM-HEIGHT T))))
                    (EXPAND
                     (MULTIPLE-VALUE-LIST
                       (MOUSE-SPECIFY-EXPAND MOUSE-SHEET))))))
       (LET ((WINDOW (MAKE-INSTANCE FLAVOR-NAME :SUPERIOR MOUSE-SHEET :EDGES TEM)))
         (SEND WINDOW :SELECT)
         WINDOW)))

(DEFUN SYSTEM-MENU-SET-MOUSE-SCREEN (HAIRY &AUX SCREENS)
  (DOLIST (S ALL-THE-SCREENS)
    (AND (SHEET-EXPOSED-P S)
         (SEND S :USER-VISIBLE)
         (PUSH (CONS (SHEET-NAME S) S) SCREENS)))
  (COND ((= (LENGTH SCREENS) 1)
         (MOUSE-SET-SHEET (CDAR SCREENS)))
        ((NOT HAIRY)
         (MOUSE-SET-SHEET (DO ((L SCREENS (CDR L)))
                              ((NULL L) (CDAR SCREENS))
                            (AND (EQ (CDAR L) MOUSE-SHEET)
                                 (CDR L)
                                 (RETURN (CDADR L))))))
        (T (LET ((S (MENU-CHOOSE SCREENS "Mouse onto:")))
             (AND S (MOUSE-SET-SHEET S))))))

(DEFUN MOUSE-Y-OR-N-P (MESSAGE)
  "Ask for confirmation from the user with a click on a one-item menu."
  (MENU-CHOOSE (LIST MESSAGE) "Confirm:" '(:MOUSE) MESSAGE))

(DEFUN SYSTEM-MENU-KILL-WINDOW (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (MOUSE-Y-OR-N-P
         (FORMAT NIL "Kill ~A"
                 (SHEET-NAME (SETQ WINDOW (OR (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)
                                              WINDOW)))))
       (SEND WINDOW :KILL)))

(DEFUN SYSTEM-MENU-RESET-WINDOW (WINDOW IGNORE IGNORE &AUX P)
  (AND WINDOW
       (MOUSE-Y-OR-N-P (FORMAT NIL "Reset process in ~A" (SHEET-NAME WINDOW)))
       (SETQ P (SEND WINDOW :SEND-IF-HANDLES :PROCESS))
       (SEND P :RESET)))

(DEFUN SYSTEM-MENU-BURY-WINDOW (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (SEND (OR (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS) WINDOW) :BURY)))

(DEFUN SELECT-OR-CREATE-WINDOW-OF-FLAVOR (FIND-FLAVOR &OPTIONAL (CREATE-FLAVOR FIND-FLAVOR))
  (SEND (OR (FIND-WINDOW-OF-FLAVOR FIND-FLAVOR)
            (MAKE-INSTANCE CREATE-FLAVOR))
        :MOUSE-SELECT))

(DEFUN SYSTEM-MENU-EDIT-WINDOW-ATTRIBUTES (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (SCREEN-EDITOR-EDIT-ATTRIBUTES WINDOW)))

;;;; Stuff for setting up a screen layout.

;;; Suggested improvements:
;;;  Find out why it thrashes the disk for several seconds before coming up,
;;;   after displaying all the windows.
;;;  Provide the ability to edit saved screen layouts.
;;;  Provide the ability to edit the SPLIT-SCREEN-LAYOUT-WINDOW with the mouse
;;;  Figure out why the choose-variable-values window sometimes fails to
;;;   appear and also why it sometimes fails to use a frame when I clearly told it to.

(DEFVAR SPLIT-SCREEN-ITEM-LIST
        '(("Existing Lisp" :VALUE "Existing Lisp"
           :DOCUMENTATION "An already existing LISP Listener.")
          ("Existing Window" :VALUE "Existing Window"
           :DOCUMENTATION "An already existing window chosen from a menu.")
          ("Plain Window" :VALUE "Plain Window"
           :DOCUMENTATION "A window with no special attributes, suitable for simple output.")
          ("Trace & Error" :VALUE "Trace & Error"
           :DOCUMENTATION "Where trace and error is directed.")
          ("Trace" :VALUE "Trace" :DOCUMENTATION "Where trace output is directed.")
          ("Error" :VALUE "Error" :DOCUMENTATION "Where the error handler will run.")
          ("" :NO-SELECT T) ("" :NO-SELECT T)
          ("Frame" :VALUE "Frame" :DOCUMENTATION "Put chosen windows together in a frame.")
          ("Mouse Corners" :VALUE "Mouse Corners"
           :DOCUMENTATION "Specify the area to fill from the mouse.")
          ("" :NO-SELECT T)
          ("Undo" :VALUE "Undo" :DOCUMENTATION "Undo last selection.")
          ("Do It" :VALUE "Do It" :FONT :MENU-STANDOUT :DOCUMENTATION "Complete selection.")
          ("Abort" :VALUE "Abort" :FONT :MENU-STANDOUT :DOCUMENTATION "Abort Split Screen.")
          ))

(DEFUN SPLIT-SCREEN-ITEM-LIST ()
  (APPEND WINDOW-TYPES-ITEM-LIST
          (IF (ODDP (LENGTH WINDOW-TYPES-ITEM-LIST))
              '(("" :NO-SELECT T)))
          SPLIT-SCREEN-ITEM-LIST))

(DEFWINDOW-RESOURCE SPLIT-SCREEN-MENU ()
  :MAKE-WINDOW (DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU
                 :NAME "Split Screen Menu"
                 :LABEL "Split screen element:"
                 :COLUMNS 2
                 :SAVE-BITS :DELAYED
                 :IO-BUFFER (MAKE-IO-BUFFER 8.)
                 :ITEM-LIST-POINTER '(SPLIT-SCREEN-ITEM-LIST))
  :REUSABLE-WHEN :DEEXPOSED)

(DEFWINDOW-RESOURCE SPLIT-SCREEN-CHOOSE-VALUES ()
  :WINDOW-CREATE (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
                   :NAME "Split Screen Choose Values" :LABEL "Frame characteristics:"
                   :CHARACTER-WIDTH 40. :IO-BUFFER NIL
                   :MARGIN-CHOICES
                   (LIST (LIST "Cancel the Frame" NIL
                               'SPLIT-SCREEN-PUNT-FRAME NIL NIL))
                   :VARIABLES '((*FRAME-NAME* "Name of frame" :STRING)
                                (*SYSTEM-KEY* "[SYSTEM] <char> selects it"
                                              :CHARACTER-OR-NIL)))
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES NIL)  ;due to order of loading of files

(DEFUN SPLIT-SCREEN-PUNT-FRAME (&REST IGNORE)
  (SEND SELF :FORCE-KBD-INPUT '(PUNT-FRAME)))

(DEFUN SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS (&OPTIONAL (SUP MOUSE-SHEET))
  (USING-RESOURCE (SCVM-MENU SPLIT-SCREEN-MENU SUP)
    (USING-RESOURCE (LAYWIN SPLIT-SCREEN-LAYOUT-WINDOW SUP)
      (LET* ((WINDOW-TYPES-ITEM-LIST (SEND SUP :PANE-TYPES-ALIST))
             (EDGES (LIST (SHEET-INSIDE-LEFT SUP) (SHEET-INSIDE-TOP SUP)
                          (SHEET-INSIDE-RIGHT SUP) (SHEET-INSIDE-BOTTOM SUP)))
             (INTERACTION-WINDOWS NIL) (CVVW NIL)
             (USE-FRAME NIL) (*FRAME-NAME* "Split-screen frame") (*SYSTEM-KEY* NIL)
             (IO-BUFFER) (ITEM))
        (DECLARE (SPECIAL *FRAME-NAME* *SYSTEM-KEY*))
        (SEND LAYWIN :CLEAR-FROBS)
        (SETQ IO-BUFFER (SEND SCVM-MENU :IO-BUFFER))
        (IO-BUFFER-CLEAR IO-BUFFER)
        (EXPOSE-WINDOW-NEAR SCVM-MENU '(:MOUSE))
        (PUSH SCVM-MENU INTERACTION-WINDOWS)
        (UNWIND-PROTECT
          (DO ((WINDOW-TYPE-LIST NIL)
               (N-WINDOWS 0)
               (RES))
              (NIL)
            (COND ((AND (PLUSP N-WINDOWS) (NOT (MEMQ LAYWIN INTERACTION-WINDOWS)))
                   (AND CVVW (SEND CVVW :DEEXPOSE))             ;May need to be moved
                   (SEND LAYWIN :MOVE-NEAR-WINDOW SCVM-MENU
                                                  (CONS (- (THIRD EDGES) (FIRST EDGES))
                                                        (- (FOURTH EDGES) (SECOND EDGES))))
                   (PUSH LAYWIN INTERACTION-WINDOWS)
                   (AND CVVW (EXPOSE-WINDOW-NEAR CVVW
                               (CONS ':WINDOW (REMQ CVVW INTERACTION-WINDOWS))))))
            (PROCESS-WAIT "Choose" (LAMBDA (B) (NOT (IO-BUFFER-EMPTY-P B))) IO-BUFFER)
            (SETQ RES (IO-BUFFER-GET IO-BUFFER))
            (COND ((AND (EQ (FIRST RES) ':MENU) (EQ (FOURTH RES) SCVM-MENU))
                   (SETQ RES (SEND SCVM-MENU :EXECUTE (SETQ ITEM (SECOND RES))))
                   (AND (EQ RES T)              ;"Any"
                        (SETQ RES (GET-WINDOW-TYPE-FROM-KEYBOARD SUP ':EDGES-FROM
                                    (CONS ':WINDOW INTERACTION-WINDOWS))))
                   (COND ((NULL RES))   ;Maybe failed getting type from keyboard
                         ((EQUALP RES "Abort") (RETURN NIL))
                         ((EQUALP RES "Mouse Corners")
                          (COND ((CAR (SETQ RES (MULTIPLE-VALUE-LIST
                                                  (MOUSE-SPECIFY-RECTANGLE NIL NIL NIL NIL
                                                                           SUP 0 0 T))))
                                 (SETQ EDGES RES)
                                 ;; Next line causes shape of LAYWIN to be recomputed
                                 (SETQ INTERACTION-WINDOWS
                                       (DELQ LAYWIN INTERACTION-WINDOWS)))))
                         ((EQUALP RES "Undo")
                          (COND ((PLUSP N-WINDOWS)
                                 (SETQ N-WINDOWS (1- N-WINDOWS)
                                       WINDOW-TYPE-LIST (CDR WINDOW-TYPE-LIST))
                                 (SEND LAYWIN :REMOVE-LAST-FROB))))
                         ((EQUALP RES "Frame")
                          (SETQ USE-FRAME T)
                          (COND ((NULL CVVW)
                                 (SETQ CVVW (ALLOCATE-RESOURCE
                                              'SPLIT-SCREEN-CHOOSE-VALUES SUP))
                                 (SEND CVVW :SET-IO-BUFFER IO-BUFFER)
                                 (SEND CVVW :SET-STACK-GROUP %CURRENT-STACK-GROUP)
                                 (EXPOSE-WINDOW-NEAR CVVW (CONS ':WINDOW INTERACTION-WINDOWS))
                                 (PUSH CVVW INTERACTION-WINDOWS))))
                         ((EQUALP RES "Existing Window")
                          (USING-RESOURCE (AW-MENU SELECTABLE-WINDOWS-MENU SUP)
                            (EXPOSE-WINDOW-NEAR AW-MENU (CONS ':WINDOW INTERACTION-WINDOWS))
                            (LOOP FOR W = (SEND AW-MENU ':CHOOSE) THEN (SHEET-SUPERIOR W)
                                  AND WW = NIL THEN W
                                  WHILE (NOT (NULL W))
                                  WHEN (EQ W SUP)
                                    RETURN (PROGN
                                             (SEND LAYWIN :ADD-FROB
                                                          (SEND WW :NAME-FOR-SELECTION))
                                             (PUSH WW WINDOW-TYPE-LIST)
                                             (SETQ N-WINDOWS (1+ N-WINDOWS)))))
                          (LEXPR-SEND SCVM-MENU :SET-MOUSE-CURSORPOS
                                                (MULTIPLE-VALUE-LIST
                                                  (SEND SCVM-MENU :ITEM-CURSORPOS ITEM))))
                         ((NOT (EQUALP RES "Do It"))
                          (PUSH RES WINDOW-TYPE-LIST)
                          (SEND LAYWIN :ADD-FROB (OR (GET-STRING-FROM-WINDOW-TYPE RES)
                                                     (STRING RES)))
                          (SETQ N-WINDOWS (1+ N-WINDOWS)))
                         ((ZEROP N-WINDOWS) (BEEP))     ;Do It with nothing to do
                         (T (DELAYING-SCREEN-MANAGEMENT
                              (DOLIST (W INTERACTION-WINDOWS)   ;Done with these now
                                (SEND W :DEACTIVATE))
                              (IF (NOT USE-FRAME)
                                  (SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW SUP EDGES
                                    WINDOW-TYPE-LIST N-WINDOWS LAYWIN)
                                  ;; SPLIT-SCREEN-FRAME isn't necessarily the right
                                  ;; flavor.  Maybe ask user whether it should be a
                                  ;; constraint-frame.  Maybe put borders around it, but
                                  ;; need a way for them to appear when partially
                                  ;; exposed even though it doesn't have a
                                  ;; bit-save array.
                                  (LET ((FRAME (MAKE-INSTANCE 'SPLIT-SCREEN-FRAME
                                                              :SUPERIOR SUP
                                                              :EDGES-FROM EDGES
                                                              :NAME *FRAME-NAME*
                                                              :EXPOSE-P T)))
                                    (AND *SYSTEM-KEY*
                                         (SETQ *SYSTEM-KEY* (CHAR-UPCASE *SYSTEM-KEY*)
                                               *SYSTEM-KEYS*
                                                 (CONS (LIST *SYSTEM-KEY* FRAME
                                                             *FRAME-NAME* NIL)
                                                       (DELQ (ASSQ *SYSTEM-KEY* *SYSTEM-KEYS*)
                                                             *SYSTEM-KEYS*))))
                                    (LET ((SEL (SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW FRAME
                                                 (LIST (SHEET-INSIDE-LEFT FRAME)
                                                       (SHEET-INSIDE-TOP FRAME)
                                                       (SHEET-INSIDE-RIGHT FRAME)
                                                       (SHEET-INSIDE-BOTTOM FRAME))
                                                 WINDOW-TYPE-LIST N-WINDOWS LAYWIN)))
                                      ;; This wouldn't be needed if frames weren't broken
                                      (AND (MEMQ SEL (SHEET-EXPOSED-INFERIORS FRAME))
                                           (SEND FRAME :SELECT-PANE SEL))))))
                            (RETURN NIL))))
                  ((EQ (FIRST RES) ':VARIABLE-CHOICE)
                   (APPLY #'CHOOSE-VARIABLE-VALUES-CHOICE (CDR RES)))
                  ((AND (EQ (FIRST RES) 'PUNT-FRAME) USE-FRAME)
                   (SEND CVVW :DEACTIVATE)
                   (SETQ INTERACTION-WINDOWS (DELQ CVVW INTERACTION-WINDOWS))
                   (SETQ USE-FRAME NIL CVVW NIL))
                  (T (FERROR "Garbage from I//O buffer: ~S" RES))))
          (DELAYING-SCREEN-MANAGEMENT
            (DOLIST (W INTERACTION-WINDOWS)     ;Done with these now
              (SEND W :DEACTIVATE))))))))

;;; Return the string that was displayed to the user when he chose type TYPE, or
;;; NIL if we can't figure it out for some reason.
(DEFUN GET-STRING-FROM-WINDOW-TYPE (TYPE &AUX TEM)
  (DOLIST (ITEM WINDOW-TYPES-ITEM-LIST)
    (COND ((SETQ TEM (GET ITEM ':VALUE))
           (IF (EQ TEM TYPE)
               (RETURN (CAR ITEM))))
          ((SETQ TEM (GET ITEM ':EVAL))
           (IF (EQ (EVAL TEM) TYPE)
               (RETURN (CAR ITEM)))))))

;;; We now have the list of windows, lay out the screen and set them up.
;;; The general rule for screen layout is that 2 or 3 windows stack vertically,
;;; 4 are in a square, 5 are a square with 1 below it, etc.
;;; To generalize, you have floor(n/2) rows in 2 columns and 1 below if n is odd
;;; This returns the window it selects, or NIL
(DEFUN SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW (SUP EDGES WINDOW-TYPE-LIST N-WINDOWS LAYWIN
                                            &AUX N-COLUMNS N-ROWS WIDTH HEIGHT TEM WINDOW SEL)
  LAYWIN  ;ignored for now
  (IF (< N-WINDOWS 4)
      (SETQ N-COLUMNS 1 N-ROWS N-WINDOWS)
      (SETQ N-COLUMNS 2 N-ROWS (TRUNCATE (1+ N-WINDOWS) 2)))
  (SETQ WIDTH (TRUNCATE (- (THIRD EDGES) (FIRST EDGES)) N-COLUMNS)
        HEIGHT (TRUNCATE (- (FOURTH EDGES) (SECOND EDGES)) N-ROWS))
  (LOCK-SHEET (SUP)
    (DOLIST (WINDOW (SHEET-EXPOSED-INFERIORS SUP))
      (SEND WINDOW :DEEXPOSE))
    (DO ((L (NREVERSE WINDOW-TYPE-LIST) (CDR L))
         (I 0 (1+ I)) (LEFT) (RIGHT) (TOP) (BOTTOM))
        ((NULL L))
      (SETQ LEFT (+ (FIRST EDGES) (* (\ I N-COLUMNS) WIDTH))
            RIGHT (+ LEFT WIDTH)
            TOP (+ (SECOND EDGES) (* (TRUNCATE I N-COLUMNS) HEIGHT))
            BOTTOM (+ TOP HEIGHT))
      ;; The bottom-most window is wider if there are an odd number of them
      (AND (NULL (CDR L))
           (SETQ RIGHT (THIRD EDGES)))
      (COND ((EQUALP (CAR L) "Existing Lisp")
             (SETQ WINDOW (IDLE-LISP-LISTENER SUP))
             (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
             (OR SEL (SETQ SEL WINDOW)))
            ((SETQ TEM (ASS 'EQUALP (CAR L)
                            '(("Plain Window")
                              ("Trace" *TRACE-OUTPUT*)
                              ("Error" *DEBUG-IO*)
                              ("Trace & Error" *TRACE-OUTPUT* *DEBUG-IO*)
                              )))
             (SETQ WINDOW (MAKE-INSTANCE 'TRACE-OR-ERROR-WINDOW
                                         :STREAM-VARIABLES (CDR TEM)
                                         :SUPERIOR SUP
                                         :NAME (AND (CDR TEM) (CAR TEM))
                                         :LEFT LEFT :TOP TOP :RIGHT RIGHT :BOTTOM BOTTOM)))
            ((NOT (SYMBOLP (CAR L)))            ;Window itself
             (SETQ WINDOW (CAR L))
             (SEND WINDOW :SET-SUPERIOR SUP)
             (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM)
             (OR SEL (SETQ SEL WINDOW)))
            (T
             (SETQ WINDOW (MAKE-INSTANCE (CAR L)
                                         :SUPERIOR SUP
                                         :LEFT LEFT :TOP TOP
                                         :RIGHT RIGHT :BOTTOM BOTTOM))
             (OR SEL (SETQ SEL WINDOW))))
      (SEND WINDOW :EXPOSE))
    (AND SEL (SEND SEL :SELECT)))
  SEL)

(DEFFLAVOR TRACE-OR-ERROR-WINDOW
        ((STREAM-VARIABLES NIL)
         (OLD-STREAM-VALUES NIL))
        (WINDOW)
  (:INITABLE-INSTANCE-VARIABLES STREAM-VARIABLES)
  (:DOCUMENTATION "Window which, when exposed, serves as the value of some stream variables.
STREAM-VARIABLES is a list of stream variables which should be set to this window
when this window is exposed.  When the window is deexposed, those variables
are set back to their former values."))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (WHEN STREAM-VARIABLES
    (UNLESS OLD-STREAM-VALUES
      (SETQ OLD-STREAM-VALUES (MAPCAR #'SYMBOL-VALUE STREAM-VARIABLES))
      (MAPC (LAMBDA (X) (SET X SELF)) STREAM-VARIABLES))))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :BEFORE :DEEXPOSE) (&REST IGNORE)
  (WHEN OLD-STREAM-VALUES
    (MAPC #'SET STREAM-VARIABLES OLD-STREAM-VALUES)
    (SETQ OLD-STREAM-VALUES NIL)))

(COMPILE-FLAVOR-METHODS TRACE-OR-ERROR-WINDOW)

(DEFVAR SCREEN-LAYOUT-MENU-ALIST NIL)
(DEFWINDOW-RESOURCE SCREEN-LAYOUT-MENU ()
  :MAKE-WINDOW (MOMENTARY-MENU :NAME "Screen Layout Menu" :LABEL "Screen Layouts"
                               :ITEM-LIST `(("Just Lisp" :EVAL
                                             (LET ((SUPERIOR (SEND TV:SELECTED-WINDOW :SUPERIOR)))
                                               `((,(IDLE-LISP-LISTENER SUPERIOR)
                                                  ,(SEND TV:SELECTED-WINDOW :STATUS)
                                                  ,(SHEET-INSIDE-LEFT SUPERIOR)
                                                  ,(SHEET-INSIDE-TOP SUPERIOR)
                                                  ,(SHEET-INSIDE-RIGHT SUPERIOR)
                                                  ,(SHEET-INSIDE-BOTTOM SUPERIOR)))))
                                            ("Save This" :EVAL (PROGN (SAVE-THIS-SCREEN-LAYOUT) NIL))))
  :REUSABLE-WHEN :DEEXPOSED)

;;; This needs grossly more error checking!!
(DEFUN SYSTEM-MENU-LAYOUTS (&OPTIONAL (SCREEN MOUSE-SHEET))
  (USING-RESOURCE (MENU SCREEN-LAYOUT-MENU SCREEN)
    (LET ((X (SEND MENU :CHOOSE)))
      (RESELECT-SCREEN-LAYOUT X))))

(DEFUN RESELECT-SCREEN-LAYOUT (LAYOUT)
  "Reselect//expose the windows described in LAYOUT.
Each element of LAYOUT is a list of a window followed by four edges for it.
We reeexpose each window with the specified edges.
The first window in the list is selected as well."
  (WHEN LAYOUT
    (DELAYING-SCREEN-MANAGEMENT
      (DOLIST (Y LAYOUT)
        (LET ((WINDOW (CAR Y))
              (STATUS (CADR Y))
              (EDGES (CDDR Y)))
          (SEND WINDOW :SET-EDGES (FIRST EDGES) (SECOND EDGES)
                                  (THIRD EDGES) (FOURTH EDGES))
          (SEND WINDOW :SET-STATUS (IF (EQ STATUS ':SELECTED) ':EXPOSED STATUS))))
      ;; Make sure that inferiors are in same order now as when layout saved.
      (DOLIST (Y (REVERSE LAYOUT))
        (IF (MEMQ (CAR Y) (SHEET-INFERIORS MOUSE-SHEET))
            (SETF (SHEET-INFERIORS MOUSE-SHEET)
                  (CONS (CAR Y)
                        (DELQ (CAR Y) (SHEET-INFERIORS MOUSE-SHEET)))))))
    (SEND (CAAR LAYOUT) :SELECT NIL)
    (SEND MOUSE-SHEET :SCREEN-MANAGE)
    NIL))

(DEFUN SAVE-THIS-SCREEN-LAYOUT (&OPTIONAL (SCREEN MOUSE-SHEET))
  (USING-RESOURCE (MENU SCREEN-LAYOUT-MENU SCREEN)
    (LET* ((L (MAPCAR (LAMBDA (W)
                        (AND (NOT (SHEET-TEMPORARY-P W))
                             (NEQ W MOUSE-SHEET)
                             (LIST* W
                                    (SEND W :STATUS)
                                    (MULTIPLE-VALUE-LIST (SEND W :EDGES)))))
                      (SHEET-INFERIORS MOUSE-SHEET)))
           (FOO (GET-LINE-FROM-KEYBOARD "Name for this screen layout"))
           (BAR (SEND MENU :ITEM-LIST))
           (SW (ASSQ SELECTED-WINDOW L)))
      ;; Move selected window to the front
      (AND SW (SETQ L (CONS SW (DELQ SW L))))
      (SEND MENU :SET-ITEM-LIST
            (CONS (LIST FOO ':VALUE L)
                  (DELQ (ASSOC FOO BAR) BAR)))  ;delete duplicate names
      (LET ((CHAR (CHAR-UPCASE
                    (GET-LINE-FROM-KEYBOARD
                      "System key for this screen layout
/(Rubout means none)"
                      MOUSE-SHEET 'TYI))))
        (COND ((EQ CHAR #/RUBOUT))
              ((OR (EQ CHAR #/SPACE)
                   ( CHAR #o200))
               (BEEP))
              (T
               (ADD-SYSTEM-KEY CHAR `(RESELECT-SCREEN-LAYOUT ',L) FOO L)))))))

(DEFWINDOW-RESOURCE POP-UP-TEXT-WINDOW ()
  :MAKE-WINDOW (POP-UP-TEXT-WINDOW))

(DEFWINDOW-RESOURCE POP-UP-TEXT-WINDOW-WITHOUT-MORE ()
  :MAKE-WINDOW (POP-UP-TEXT-WINDOW :MORE-P NIL))

;;; Pop up a window with a message in it, require user to type a character to flush.
(DEFUN POP-UP-MESSAGE (PROMPT &OPTIONAL (SUP MOUSE-SHEET) (POP-UP-NEAR '(:MOUSE)))
  (LET ((MESSAGE (STRING-APPEND PROMPT "
Type any character to flush:  ")))
    (USING-RESOURCE (POP-UP-MESSAGE-WINDOW POP-UP-TEXT-WINDOW-WITHOUT-MORE SUP)
      (SEND POP-UP-MESSAGE-WINDOW :SET-LABEL NIL)
      (SEND POP-UP-MESSAGE-WINDOW :SET-SIZE-IN-CHARACTERS MESSAGE MESSAGE)
      (SEND POP-UP-MESSAGE-WINDOW :CLEAR-INPUT)
      (EXPOSE-WINDOW-NEAR POP-UP-MESSAGE-WINDOW POP-UP-NEAR NIL)
      (WINDOW-CALL (POP-UP-MESSAGE-WINDOW :DEACTIVATE)
        (SEND POP-UP-MESSAGE-WINDOW :STRING-OUT MESSAGE)
        ;; Back up the cursor by one.  This is easier than trying to make the window
        ;; come out wider, because of the interface to :set-size-in-characters.
        (MULTIPLE-VALUE-BIND (X-POS Y-POS)
            (SEND POP-UP-MESSAGE-WINDOW :READ-CURSORPOS :CHARACTER)
        (SEND POP-UP-MESSAGE-WINDOW :SET-CURSORPOS (1- X-POS) Y-POS :CHARACTER))
        (SEND POP-UP-MESSAGE-WINDOW :TYI)))))

;Pop up a formatted message near the mouse
(DEFUN POP-UP-FORMAT (CONTROL &REST ARGS)
  (POP-UP-MESSAGE (APPLY 'FORMAT NIL CONTROL ARGS)))

;;; Pop up a window near where the mouse is, then read a line from it.
(DEFUN GET-LINE-FROM-KEYBOARD (PROMPT &OPTIONAL (SUP MOUSE-SHEET) (FUNCTION 'READLINE)
                                                (POP-UP-NEAR '(:MOUSE)))
  (USING-RESOURCE (GET-LINE-FROM-KEYBOARD-WINDOW POP-UP-TEXT-WINDOW SUP)
    (SEND GET-LINE-FROM-KEYBOARD-WINDOW :SET-SIZE #o500 #o120)
    (SEND GET-LINE-FROM-KEYBOARD-WINDOW :SET-LABEL NIL)
    (SEND GET-LINE-FROM-KEYBOARD-WINDOW :CLEAR-INPUT)
    (EXPOSE-WINDOW-NEAR GET-LINE-FROM-KEYBOARD-WINDOW POP-UP-NEAR NIL)
    (WINDOW-CALL (GET-LINE-FROM-KEYBOARD-WINDOW :DEACTIVATE)
      (FORMAT GET-LINE-FROM-KEYBOARD-WINDOW "~A:~%" PROMPT)
      (FUNCALL FUNCTION GET-LINE-FROM-KEYBOARD-WINDOW))))

(DEFUN GET-WINDOW-TYPE-FROM-KEYBOARD (&OPTIONAL (SUP MOUSE-SHEET) REQUIRED-INIT-OPTION
                                                (POP-UP-NEAR '(:MOUSE))
                                      &AUX (WT NIL) FL)
  (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Abort this screen operation.")
    (SETQ WT (GET-LINE-FROM-KEYBOARD "Flavor of window" SUP #'READ POP-UP-NEAR)))
  (COND ((NULL WT) NIL)
        ((OR (NULL (SETQ FL (GET WT 'SI:FLAVOR)))
             (NOT (SI:MAP-OVER-COMPONENT-FLAVORS 0 NIL T        ;T if it's built on SHEET
                                                 (LAMBDA (FL IGNORE)
                                                   (EQ FL (GET 'SHEET 'SI:FLAVOR)))
                                                 WT NIL))
             (AND REQUIRED-INIT-OPTION
                  (NOT (FLAVOR-ALLOWS-INIT-KEYWORD-P WT REQUIRED-INIT-OPTION))))
         (BEEP)
         NIL)
        (T WT)))

;;;Hack window for split screen
(DEFFLAVOR DISPLAY-LAYOUT-WINDOW ((FROBS NIL))
                                 (TEMPORARY-WINDOW-MIXIN BORDERS-MIXIN MINIMUM-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES FROBS))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :BEFORE :INIT) (INIT-PAIRS)
  (PUTPROP INIT-PAIRS NIL :BLINKER-P))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ LEFT-MARGIN-SIZE 1 TOP-MARGIN-SIZE 1
        RIGHT-MARGIN-SIZE 1 BOTTOM-MARGIN-SIZE 1))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :CLEAR-FROBS) ()
  (SETQ FROBS NIL)
  (SHEET-FORCE-ACCESS (SELF T)
    (SHEET-CLEAR SELF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :ADD-FROB) (FROB &AUX N)
  (SETQ N (LENGTH FROBS)
        FROBS (NCONC FROBS (NCONS FROB)))
  (SHEET-FORCE-ACCESS (SELF)
    (DRAW-FROBS SELF FROBS N ERASE-ALUF)
    (DRAW-FROBS SELF FROBS (1+ N) CHAR-ALUF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :REMOVE-LAST-FROB) ()
  (SETQ FROBS (NREVERSE (CDR (NREVERSE FROBS))))
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
    (SHEET-CLEAR SELF)
    (DRAW-FROBS SELF FROBS (LENGTH FROBS) CHAR-ALUF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P
      (DRAW-FROBS SELF FROBS (LENGTH FROBS) CHAR-ALUF)))

(DEFUN DRAW-FROBS (SHEET FROBS N ALU)
  (OR (ZEROP N)
      (LET ((INSIDE-LEFT (SHEET-INSIDE-LEFT SHEET))
            (INSIDE-TOP (SHEET-INSIDE-TOP SHEET))
            (INSIDE-RIGHT (SHEET-INSIDE-RIGHT SHEET))
            (INSIDE-BOTTOM (SHEET-INSIDE-BOTTOM SHEET))
            (INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT SHEET))
            (INSIDE-WIDTH (SHEET-INSIDE-WIDTH SHEET))
            MIDDLE NROW)
        (IF (< N 4)
            (SETQ NROW N)
            (SETQ NROW (TRUNCATE (1+ N) 2)
                  MIDDLE (+ INSIDE-LEFT (TRUNCATE INSIDE-WIDTH 2))))
        (PREPARE-SHEET (SELF)
          (DO ((I NROW (1- I))
               (J 0 (1+ J))
               (FROBS FROBS (CDR FROBS))
               (Y) (Y1)
               (LHEIGHT (TRUNCATE INSIDE-HEIGHT NROW))
               (ODDP (ODDP N)))
              (( I 0))
            (SETQ Y (+ INSIDE-TOP (TRUNCATE (* INSIDE-HEIGHT J) NROW))
                  Y1 (IF (= I 1) (1- INSIDE-BOTTOM) (+ Y LHEIGHT)))
            (OR (= I 1)
                (%DRAW-LINE INSIDE-LEFT Y1 INSIDE-WIDTH Y1 ALU T SHEET))
            (DRAW-LAYOUT-LABEL SHEET (CAR FROBS) INSIDE-LEFT Y1
                               (IF (OR (NULL MIDDLE) (AND (= I 1) ODDP)) INSIDE-RIGHT MIDDLE)
                               LHEIGHT ALU)
            (COND ((NOT (OR (NULL MIDDLE) (AND (= I 1) ODDP)))
                   (%DRAW-LINE MIDDLE Y MIDDLE Y1 ALU T SHEET)
                   (SETQ FROBS (CDR FROBS))
                   (DRAW-LAYOUT-LABEL SHEET (CAR FROBS) MIDDLE Y1 INSIDE-RIGHT
                                      LHEIGHT ALU))))))))

(DEFUN DRAW-LAYOUT-LABEL (SHEET STRING X Y XLIM LHEIGHT ALU)
  (COND ((< LHEIGHT 3))                         ;Too small for anything
        ((< LHEIGHT 7)                          ;Too small for 5X5
         (DRAW-LAYOUT-TURDS SHEET STRING (1+ X) (- Y 2) XLIM ALU))
        (T
         (SEND SHEET :STRING-OUT-EXPLICIT (STRING-UPCASE STRING)
                                          (1+ X) (- Y 6) XLIM NIL
                                          (SEND (SHEET-GET-SCREEN SHEET)
                                                :PARSE-FONT-DESCRIPTOR FONTS:5X5)
                                          ALU 0 NIL NIL))))

(DEFUN DRAW-LAYOUT-TURDS (SHEET STRING X Y XLIM ALU)
  (DO ((I 0 (1+ I))
       (X X (+ X 2))
       (ARRAY (SHEET-SCREEN-ARRAY SHEET))
       (LEN (STRING-LENGTH STRING)))
      ((OR ( I LEN) ( X XLIM)))
    (OR (EQL (CHAR STRING I) #/SPACE)
        (AS-2-REVERSE (SELECT ALU
                        (ALU-XOR (1+ (AR-2-REVERSE ARRAY X Y)))
                        (ALU-IOR 1)
                        (ALU-ANDCA 0))
                      ARRAY X Y))))

;;;Move along side a window
;;;Try to make the same height as that window, but if that won't fit because it
;;;comes out too wide then become shorter, and center.
;;;DIMENSIONS argument controls the width to height ratio.
(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :MOVE-NEAR-WINDOW) (WINDOW &OPTIONAL (DIMENSIONS '(1 . 1)))
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW :EDGES)
    (LET ((NEW-WIDTH (TRUNCATE (* (CAR DIMENSIONS) (- BOTTOM TOP)) (CDR DIMENSIONS)))
          (NEW-HEIGHT (- BOTTOM TOP))
          (SLEFT (SHEET-INSIDE-LEFT SUPERIOR))
          (SRIGHT (SHEET-INSIDE-RIGHT SUPERIOR))
          (NTOP TOP)
          (NBOTTOM BOTTOM)
          NLEFT NRIGHT NCENTER)
      (COND (( (SETQ NLEFT (- LEFT NEW-WIDTH)) SLEFT)
             (SETQ NRIGHT LEFT))        ;Fits on the left
            ((< (SETQ NRIGHT (+ RIGHT NEW-WIDTH)) SRIGHT)
             (SETQ NLEFT RIGHT))        ;Fits on the right
            (T
             ;; Make it short enough to fit either above or below,
             ;; and scale the width to match.
;            (SETQ NEW-HEIGHT
;                  (MAX (- TOP (SHEET-INSIDE-TOP SUPERIOR))
;                       (- (SHEET-INSIDE-BOTTOM SUPERIOR) BOTTOM))
;                  NEW-WIDTH (TRUNCATE (* (CAR DIMENSIONS) NEW-HEIGHT) (CDR DIMENSIONS)))
             (SETQ NCENTER (TRUNCATE (+ SLEFT SRIGHT) 2)
                   NLEFT (- NCENTER (TRUNCATE NEW-WIDTH 2))
                   NRIGHT (+ NCENTER (TRUNCATE NEW-WIDTH 2)))
             (COND ((< (SETQ NBOTTOM (+ BOTTOM NEW-HEIGHT)) (SHEET-INSIDE-BOTTOM SUPERIOR))
                    (SETQ NTOP BOTTOM))
                   (( (SETQ NTOP (- TOP NEW-HEIGHT)) (SHEET-INSIDE-TOP SUPERIOR))
                    (SETQ NBOTTOM TOP))
                   (T (FERROR "Insufficient room to display layout window.")))))
      (SEND SELF :SET-EDGES NLEFT NTOP NRIGHT NBOTTOM)))
  (SEND SELF :EXPOSE))

(COMPILE-FLAVOR-METHODS DISPLAY-LAYOUT-WINDOW)


(DEFWINDOW-RESOURCE SPLIT-SCREEN-LAYOUT-WINDOW ()
  :MAKE-WINDOW (DISPLAY-LAYOUT-WINDOW :HEIGHT (TRUNCATE (SHEET-HEIGHT MOUSE-SHEET) 4.))
  :REUSABLE-WHEN :DEEXPOSED)

;;;; This goes in QTRACE

;;; Display Features

;;; Items in this menu are lists of the form:
;;;  ("name" :VALUE (S-expr-arg-p . what-to-append-into-trace-options))
;;;                     ^-- if this is UNTRACE, QUIT, or DO-IT, that special function
;;;                     if NIL, nothing special
;;;                     otherwise is prompt for reading what goes into trace options
;;; Try to keep this so it comes out in 3 columns
(DEFCONST TRACE-ITEM-LIST
        `(
;; This sort of break loop is pretty useless in a lexical lisp
;         ("Break before" :VALUE (NIL :BREAK T)
;          :DOCUMENTATION "Call BREAK before entering the function, binding ARGLIST.")
;         ("Break after" :VALUE (NIL :EXITBREAK T)
;          :DOCUMENTATION "Call BREAK after leaving the function, binding VALUES.")
          ("Step" :VALUE (NIL :STEP)
           :DOCUMENTATION "Single-step through the body of the (interpreted) function.")
          ("Cond Step"
           :VALUE ("Enter expression:
Stepping will happen if the expression evaluates non-NIL"
                   :STEPCOND)
           :DOCUMENTATION "Asks for an expression which controls whether stepping happens.")
          ("Error" :VALUE (NIL :ERROR)
           :DOCUMENTATION "Enter the debugger when the function is called.")
          ("Print" :VALUE ("Form to evaluate and print in trace messages" :PRINT)
           :DOCUMENTATION "Asks for a form to be evaluated and printed in trace messages.")
          ("Print before" :VALUE ("Form to evaluate and print before calling" :ENTRYPRINT)
           :DOCUMENTATION "Asks for a form to be evaluated and printed upon function entry.")
          ("Print after" :VALUE ("Form to evaluate and print after returning" :EXITPRINT)
           :DOCUMENTATION "Asks for a form to be evaluated and printed upon function exit.")
          ("Conditional"
           :VALUE ("Enter expression:
Tracing will only will happen if the expression evaluates non-NIL"
                   :COND)
           :DOCUMENTATION "Asks for an expression which controls whether tracing happens.")
          ("Cond before"
           :VALUE ("Enter expression:
Entry tracing will only happen if the expression evaluates non-NIL"
                   :ENTRYCOND)
           :DOCUMENTATION "Asks for an expression which controls tracing of function entry.")
          ("Cond after"
           :VALUE ("Enter expression:
Exit tracing will only happen when the expression evaluates non-NIL"
                   :EXITCOND)
           :DOCUMENTATION "Asks for an expression which controls tracing of function exit.")
;         ("Cond break before"
;          :VALUE ("Predicate for breaking before"
;                  :BREAK)
;          :DOCUMENTATION "Asks for a predicate which controls BREAKing on function entry.")
;         ("Cond break after"
;          :VALUE ("Predicate for breaking after"
;                  :EXITBREAK)
;          :DOCUMENTATION "Asks for a predicate which controls BREAKing on function exit.")
          ("ARGPDL" :VALUE ("Arg pdl variable" :ARGPDL)
           :DOCUMENTATION "Asks for a variable which gets list of recursive argument lists.")
          ("Wherein" :VALUE ("Function within which to trace" :WHEREIN)
           :DOCUMENTATION "Asks for a function, within which tracing will be active.")
          ("To editor buffer" :VALUE (NIL :TO-EDITOR)
           :DOCUMENTATION "Sends trace output to ed-buffer:trace-output.")
          ("Do It" :VALUE (DO-IT) :FONT :MENU-STANDOUT
           :DOCUMENTATION "Click here to do the tracing with the selected options.")
          ("Untrace" :VALUE (UNTRACE) :FONT :MENU-STANDOUT
           :DOCUMENTATION "Instead of tracing this function, stop tracing it.")
          ("Abort" :VALUE (QUIT) :FONT :MENU-STANDOUT
           :DOCUMENTATION "Click here to get out of this without doing anything.")))

(DEFWINDOW-RESOURCE TRACE-POP-UP-MENU ()
  :MAKE-WINDOW (DYNAMIC-TEMPORARY-MENU :NAME "Trace Options"
                                       :ITEM-LIST-POINTER 'TRACE-ITEM-LIST)
  :REUSABLE-WHEN :DEEXPOSED)

;;; This function is invoked in the momentary menu process when the user clicks "trace"
;;; and in the editor process by the editor's Trace command.
;;; If the function isn't supplied as an argument the user is asked for it.
(DEFUN TRACE-VIA-MENUS (&OPTIONAL FCN)
  (USING-RESOURCE (TRACE-POP-UP-WINDOW POP-UP-TEXT-WINDOW)
    (USING-RESOURCE (TRACE-POP-UP-MENU TRACE-POP-UP-MENU)
      (SEND TRACE-POP-UP-WINDOW :SET-LABEL "Trace")
      (SEND TRACE-POP-UP-WINDOW :SET-SIZE #o1000 #o300)
      (SEND TRACE-POP-UP-WINDOW :CENTER-AROUND MOUSE-X MOUSE-Y)
      (WINDOW-CALL (TRACE-POP-UP-WINDOW :DEACTIVATE)
        (UNWIND-PROTECT
            (trace-via-menus-guts fcn trace-pop-up-window trace-pop-up-menu)
          (SEND TRACE-POP-UP-MENU :DEACTIVATE))))))

(defun trace-via-menus-guts (fcn trace-pop-up-window trace-pop-up-menu)
  (LET ((BLINKER (CAR (SHEET-BLINKER-LIST TRACE-POP-UP-WINDOW)))
        (*TERMINAL-IO* TRACE-POP-UP-WINDOW)
        (*QUERY-IO* TRACE-POP-UP-WINDOW)
        (*STANDARD-INPUT* TRACE-POP-UP-WINDOW)
        (*STANDARD-OUTPUT* TRACE-POP-UP-WINDOW))
    (WHEN (NULL FCN)
      ;; Make sure blinker is blinking
      (BLINKER-SET-VISIBILITY BLINKER :BLINK)
      (loop
        (setq fcn (prompt-and-read :read "Type in name of function to be traced or untraced.  Abort quits~%: "))
        (IF (FDEFINEDP FCN)
            (RETURN NIL)
          (FORMAT T " ;not a defined function, try again~%"))))
    (SEND TRACE-POP-UP-MENU :MOVE-NEAR-WINDOW TRACE-POP-UP-WINDOW)
    (DO ((FORM (IF (ATOM FCN) `(TRACE (,FCN)) `(TRACE (:FUNCTION ,FCN))))
         (CHOICE) (OPTION))
        (NIL)
      ;;Put the current status on the text window
      (SEND TRACE-POP-UP-WINDOW :CLEAR-WINDOW)
      (GRIND-TOP-LEVEL FORM #o76 TRACE-POP-UP-WINDOW)   ;76 is width in characters
      ;; Not listening to the keyboard any more, shut off blinker
      (BLINKER-SET-VISIBILITY BLINKER NIL)
      ;; Get input from the menu
      (SETQ CHOICE (SEND TRACE-POP-UP-MENU :CHOOSE)
            OPTION (FIRST CHOICE))
      (COND ((NULL CHOICE))                     ;Try again if outside menu
            ((EQ OPTION 'UNTRACE)
             (EVAL `(UNTRACE ,FCN))
             (RETURN NIL))
            ((EQ OPTION 'QUIT)
             (RETURN NIL))
            ((EQ OPTION 'DO-IT)
             (EVAL FORM)
             (RETURN NIL))
            (T ;; let's make it smarter so that we can undo choices.
             (let ((previous-entry-position
                     (find-position-in-list (second choice) (second form))))
               (cond ((and previous-entry-position
                           (or (eq (second choice) ':BREAK)
                               (eq (second choice) ':EXITBREAK))
                           (cddr choice))
                      ;; then we should either remove it from the list if it is
                      ;; the same or just change the argument.
                      (if (eq (third choice) (nth (1+ previous-entry-position)
                                                  (second form)))
                          ;; remove the choice
                          (if (eq previous-entry-position 0)
                              (setf (second form) (cddr (second form)))
                            (setf (nthcdr previous-entry-position (second form))
                                  (nthcdr (+ 2 previous-entry-position)
                                          (second form))))
                        ;; fix the choice
                        (setf (nth (1+ previous-entry-position)
                                   (second form))
                              (third choice))))
                     ((and previous-entry-position (null option))
                      ;; delete the choice
                      (if (eq previous-entry-position 0)
                          (setf (second form) (cdr (second form)))
                        (setf (nthcdr previous-entry-position (second form))
                              (nthcdr (1+ previous-entry-position)
                                      (second form)))))
                     ((null option)
                      ;; just add the object
                      (SETF (SECOND FORM)
                            (APPEND (SECOND FORM) (CDR CHOICE))))
                     (T                         ;Needs an arg, get it
                      ;; Turn on blinker
                      (BLINKER-SET-VISIBILITY BLINKER :BLINK)
                      (MULTIPLE-VALUE-BIND (ARG FLAG)
                          (condition-case ()
                              (with-input-editing (*query-io*
                                                    '((:full-rubout :full-rubout)
                                                      (:activation char= #/end)))
                                (prompt-and-read :expression "~2%~A:~%" OPTION))
                            (abort (values nil :full-rubout)))
                        (UNLESS FLAG
                          ;; if previous entry replace the argument
                          ;; otherwise add on new argument
                          (if (null previous-entry-position)
                              (SETF (SECOND FORM)
                                    (APPEND (SECOND FORM) (cdr choice) (ncons ARG)))
                            (setf (nth (1+ previous-entry-position)
                                       (second form))
                                  arg))))))))))))

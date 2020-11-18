;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; WINDOW type flavor
(DEFFLAVOR ESSENTIAL-WINDOW () (SHEET)
  (:INIT-KEYWORDS :EDGES-FROM :MINIMUM-WIDTH :MINIMUM-HEIGHT :ACTIVATE-P :EXPOSE-P)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "The flavor that is part of every window
This had better be at the end of your any hierarchy, it should also always
be a :required-flavor of any window mixin just so that instance variables
are declared properly."))

(DEFMETHOD (ESSENTIAL-WINDOW :BEFORE :MOUSE-SELECT) (&REST IGNORE)
;; Snarfing input from the selected window before the new one is selected made
;; characters typed after a new mouse selection go to the old window.  KHS 1/7/85.
;  (WITHOUT-INTERRUPTS
;    (AND SELECTED-WINDOW
;        (SETQ BUF (SEND SELECTED-WINDOW ':IO-BUFFER))
;        (KBD-SNARF-INPUT BUF))))
  (UNLESS (NULL SELECTED-WINDOW)
    (SEND SELECTED-WINDOW :IO-BUFFER)))

(DEFMETHOD (ESSENTIAL-WINDOW :MOUSE-SELECT) (&REST ARGS)
  (LEXPR-SEND (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS) :SELECT ARGS))

(DEFMETHOD (ESSENTIAL-WINDOW :LISP-LISTENER-P) () NIL)

(DEFMETHOD (ESSENTIAL-WINDOW :SELECTABLE-WINDOWS) ()
  "Returns inferiors to all levels that are selectable in a form suitable for
use as a menu item-list."
  (LET ((SELECTABLE-WINDOWS (MAPCAN (LAMBDA (I) (SEND I :SELECTABLE-WINDOWS))
                                    INFERIORS))
        (STRING))
    (AND (SETQ STRING (SEND SELF :NAME-FOR-SELECTION))
         (PUSH (LIST STRING SELF) SELECTABLE-WINDOWS))))

(DEFMETHOD (ESSENTIAL-WINDOW :BEFORE :INIT) (INIT-PLIST &AUX EDGES-FROM)
  (OR NAME
      (LET ((FLAVOR-NAME (TYPE-OF SELF)))
        (LET ((N (1+ (OR (GET FLAVOR-NAME 'UNNAMED-WINDOW-INSTANCE-COUNT) 0))))
          (PUTPROP FLAVOR-NAME N 'UNNAMED-WINDOW-INSTANCE-COUNT)
          (SETQ NAME (STRING-CAPITALIZE-WORDS (FORMAT NIL "~A-~D" FLAVOR-NAME N) NIL)))))

  (SETQ EDGES-FROM (GET INIT-PLIST ':EDGES-FROM))
  (COND ((NULL EDGES-FROM))
        ((STRINGP EDGES-FROM)
         (SETF (GET INIT-PLIST ':CHARACTER-WIDTH) EDGES-FROM)
         (SETF (GET INIT-PLIST ':CHARACTER-HEIGHT) EDGES-FROM))
        (T
         (SETF (GET INIT-PLIST ':EDGES)
               (COND ((CONSP EDGES-FROM)
                      ;; If a list, means explicit edges specified
                      (OR (= (LENGTH EDGES-FROM) 4)
                          (FERROR "The ~S list ~S is not of length four."
                                  :EDGES-FROM EDGES-FROM))
                      EDGES-FROM)
                     ((EQ EDGES-FROM ':MOUSE)
                      ;; Get edges from mouse
                      (LET ((MINIMUM-WIDTH (GET INIT-PLIST ':MINIMUM-WIDTH 0))
                            (MINIMUM-HEIGHT (GET INIT-PLIST ':MINIMUM-HEIGHT 0)))
                        (MULTIPLE-VALUE-LIST
                          (MOUSE-SPECIFY-RECTANGLE-SET-SHEET NIL NIL NIL NIL SUPERIOR
                                                             MINIMUM-WIDTH
                                                             MINIMUM-HEIGHT))))
                     ((TYPEP EDGES-FROM 'ESSENTIAL-WINDOW)
                      ;; A window, use its edges
                      (OR (EQ SUPERIOR (SHEET-SUPERIOR EDGES-FROM))
                          (FERROR
                            "Attempt to get edges from sheet (~S) with different superior"
                            EDGES-FROM))
                      (LIST (SHEET-X-OFFSET EDGES-FROM)
                            (SHEET-Y-OFFSET EDGES-FROM)
                            (+ (SHEET-X-OFFSET EDGES-FROM) (SHEET-WIDTH EDGES-FROM))
                            (+ (SHEET-Y-OFFSET EDGES-FROM) (SHEET-HEIGHT EDGES-FROM))))
                     (T (FERROR "~S is illegal ~S specification" EDGES-FROM :EDGES-FROM))))))
  (LET ((INSIDE-WIDTH (GET INIT-PLIST ':INSIDE-WIDTH (FIRST (GET INIT-PLIST ':INSIDE-SIZE))))
        (INSIDE-HEIGHT (GET INIT-PLIST ':INSIDE-HEIGHT (SECOND (GET INIT-PLIST ':INSIDE-SIZE)))))
    (AND INSIDE-WIDTH (SETQ WIDTH (+ INSIDE-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
    (AND INSIDE-HEIGHT (SETQ HEIGHT (+ INSIDE-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))))

(DEFFLAVOR MINIMUM-WINDOW () (ESSENTIAL-EXPOSE ESSENTIAL-ACTIVATE ESSENTIAL-SET-EDGES
                              ESSENTIAL-MOUSE ESSENTIAL-WINDOW)
  (:DOCUMENTATION :COMBINATION "Essential flavors for most normal windows
Most windows should include this at the end of their hierachy or all of its components."))

(DEFFLAVOR WINDOW-WITHOUT-LABEL () (STREAM-MIXIN BORDERS-MIXIN SELECT-MIXIN
                                    DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW))

(DEFFLAVOR WINDOW () (STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN SELECT-MIXIN
                      DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "This is the simplest practical window
It probably isn't what you want, except for testing purposes; although it is useful for
mixing with one or two simple mixins to get something useful."))

(DEFFLAVOR WINDOW-WITH-INSIDE-SCROLL-BAR
        () (STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN BASIC-SCROLL-BAR
            SELECT-MIXIN DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Simple window with scroll bar inside borders."))

;;; Basic exposure/deexposure
(DEFFLAVOR ESSENTIAL-EXPOSE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Handles default exposure behaviour.
Makes sure the screen manager is aware of a window leaving or entering the screen."))

(DEFMETHOD (ESSENTIAL-EXPOSE :AFTER :EXPOSE) (&REST IGNORE)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF :EXPOSE))

(DEFMETHOD (ESSENTIAL-EXPOSE :AFTER :DEEXPOSE) (&REST IGNORE)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF :DEEXPOSE))

(DEFWRAPPER (ESSENTIAL-EXPOSE :BURY) (IGNORE . BODY)
  `(AND (OR (NULL SUPERIOR)
            (SEND SUPERIOR :INFERIOR-BURY SELF))
        (DELAYING-SCREEN-MANAGEMENT . ,BODY)))

;;; Basic activation/deactivation
(DEFFLAVOR ESSENTIAL-ACTIVATE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Handles default activation behaviour
Makes sure a window is activated before it can get exposed (see discussion of activation).
Also provides for the :STATUS and :SET-STATUS messages (q.v.)."))

(DEFMETHOD (ESSENTIAL-ACTIVATE :BEFORE :EXPOSE) (&REST IGNORE)
  (WITHOUT-SCREEN-MANAGEMENT
    (SEND SELF :ACTIVATE)))

(DEFMETHOD (ESSENTIAL-ACTIVATE :AFTER :ACTIVATE) ()
  (SCREEN-ACTIVITY-HAS-CHANGED SELF T))

;;; This could simply do (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF T) except
;;; this method gets run whether or not the window was already active when
;;; the :ACTIVATE message was sent.  What a drag.
(DEFMETHOD (ESSENTIAL-ACTIVATE :AROUND :ACTIVATE) (CONT MT ARGS)
  (LET ((W (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS))
        (ACTIVE-P (SEND SELF :ACTIVE-P)))
    (LEXPR-FUNCALL-WITH-MAPPING-TABLE CONT MT ARGS)
    (OR ACTIVE-P
        ;; Only windows that are top level for selection should go on the list.
        (NEQ W SELF)
        ;; Can be activating a pane relative to a frame which is not itself activated
        (DO ((SUP (SHEET-SUPERIOR W) (SHEET-SUPERIOR SUP))
             (INF W SUP))
            ((NULL SUP) NIL)
          (OR (MEMQ INF (SHEET-INFERIORS SUP)) (RETURN T)))
        ;; Do nothing if W is already on the list.
        (DO ((I 0 (1+ I))
             (N (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS)))
            (( I N) (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS W T))
          (AND (EQ (AREF PREVIOUSLY-SELECTED-WINDOWS I) W) (RETURN NIL))))))

(DEFMETHOD (ESSENTIAL-ACTIVATE :AFTER :DEACTIVATE) ()
  (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS SELF)
  (SCREEN-ACTIVITY-HAS-CHANGED SELF NIL))

(DEFMETHOD (ESSENTIAL-ACTIVATE :BURY) () (SYSTEM-BURY SELF))

(DEFUN SYSTEM-BURY (WINDOW &AUX (INHIBIT-SCHEDULING-FLAG T) SUP INFS)
  (SEND SELF :DESELECT :BURY)
  (SETQ SUP (SHEET-SUPERIOR WINDOW))
  (DO () ((NOT (MEMQ WINDOW (SHEET-EXPOSED-INFERIORS SUP))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (SEND WINDOW :DEEXPOSE)
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (COND ((MEMQ WINDOW (SETQ INFS (SHEET-INFERIORS SUP)))
         (SETQ INFS (DELQ WINDOW INFS))
         (SHEET-CONSING
           (COND (INFS (RPLACD (LAST (SETQ INFS (COPYLIST INFS))) (NCONS WINDOW)))
                 (T (SETQ INFS (NCONS WINDOW)))))
         (SETF (SHEET-INFERIORS SUP) INFS)
         (SETQ INHIBIT-SCHEDULING-FLAG NIL)
         (SCREEN-CONFIGURATION-HAS-CHANGED WINDOW))))

(DEFMETHOD (ESSENTIAL-ACTIVATE :STATUS) ()
  (COND ((EQ SELF SELECTED-WINDOW) :SELECTED)
        ((DO ((WINDOW SELF (SHEET-SUPERIOR WINDOW)))
             ((NULL WINDOW) T)
           (OR (SHEET-EXPOSED-P WINDOW) (RETURN NIL)))
         :EXPOSED)                              ;Only if really on screen
        ((AND SUPERIOR (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))
         :EXPOSED-IN-SUPERIOR)                  ;Would be exposed if superior was
        ((OR (NULL SUPERIOR) (MEMQ SELF (SHEET-INFERIORS SUPERIOR)))
         :DEEXPOSED)
        (T
         :DEACTIVATED)))

(DEFMETHOD (ESSENTIAL-ACTIVATE :ACTIVE-P) ()
  (OR (NULL SUPERIOR) (MEMQ SELF (SHEET-INFERIORS SUPERIOR))))

(DEFMETHOD (ESSENTIAL-ACTIVATE :SET-STATUS) (NEW-STATUS)
  (ECASE NEW-STATUS
    (:SELECTED (SEND SELF :SELECT))
    (:EXPOSED
      (SEND SELF :EXPOSE)
      (AND (EQ SELF SELECTED-WINDOW)
           (SEND SELF :DESELECT NIL)))  ;Don't restore-selected!
    (:EXPOSED-IN-SUPERIOR
      (OR (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR))
          (SEND SELF :EXPOSE))
      (AND (EQ SELF SELECTED-WINDOW)
           (SEND SELF :DESELECT NIL)))  ;Don't restore-selected!
    (:DEEXPOSED
      (OR (MEMQ SELF (SHEET-INFERIORS SUPERIOR))
          (SEND SELF :ACTIVATE))
      (AND (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR))
           (SEND SELF :DEEXPOSE)))
    (:DEACTIVATED
      (AND (MEMQ SELF (SHEET-INFERIORS SUPERIOR))
           (SEND SELF :DEACTIVATE)))))

;;; This must come here to prevent lossage defining SELECT-MIXIN
;;; The methods come later.
(DEFFLAVOR DELAY-NOTIFICATION-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Delays printing notifications, but announces them in the who line.
This is the default way of handling them.  See NOTIFICATION-MIXIN for an alternative."))

;;; Basic selection/deselection
(DEFFLAVOR SELECT-MIXIN () ()
  (:INCLUDED-FLAVORS DELAY-NOTIFICATION-MIXIN)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-INSTANCE-VARIABLES IO-BUFFER)
  (:DOCUMENTATION :MIXIN "Default SELECTion behaviour
Provides a :NAME-FOR-SELECTION message that gives the window's label or name, and simple
:CALL, :BREAK, and :ABORT messages.  Note that any window that can be selected is expected
to handle these messages, and should probably include this flavor somewhere."))

(DEFMETHOD (SELECT-MIXIN :NAME-FOR-SELECTION) (&AUX LABEL)
  "Decide whether or not the Label is pretty enough to use
   as a name for selection, otherwise, use the window's name."
  (OR (AND (SETQ LABEL (SEND-IF-HANDLES SELF :LABEL))
           (OR (STRINGP LABEL) (SETQ LABEL (LABEL-STRING LABEL)))
           (AND (STRING-SEARCH-NOT-CHAR #/SPACE LABEL) LABEL))
      NAME))

(DEFMETHOD (SELECT-MIXIN :PROCESS) ()
  (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER))

(DEFMETHOD (SELECT-MIXIN :SET-PROCESS) (PROC)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) PROC))

;;; If process not otherwise known, default to process which is activating
;;; the window which will usually be good enough for the who-line.
(DEFMETHOD (SELECT-MIXIN :AFTER :ACTIVATE) (&REST IGNORE)
  (OR (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER)
      (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) CURRENT-PROCESS)))

;>> Crock -- God only knows why someone would want to use this.
(DEFMETHOD (SELECT-MIXIN :CALL) ()
  (process-run-function
    "Lisp :CALL handler"
    #'(lambda (old-window &aux new-window)
        (LET ((LAST-PROCESS (SEND OLD-WINDOW :PROCESS)))
          (AND LAST-PROCESS (SEND LAST-PROCESS :ARREST-REASON :CALL)))
        (SETQ NEW-WINDOW
              (IF (EQ (SEND OLD-WINDOW :LISP-LISTENER-P) ':IDLE)
                  OLD-WINDOW
                (KBD-DEFAULT-CALL-WINDOW (send old-window :SUPERIOR))))
        (SHEET-FREE-TEMPORARY-LOCKS NEW-WINDOW)
        (SEND NEW-WINDOW :SELECT))
    self)
  nil)

(DEFMETHOD (SELECT-MIXIN :ARREST) (&AUX LAST-PROCESS)
  (AND (SETQ LAST-PROCESS (SEND SELF :PROCESS))
       (SEND LAST-PROCESS :ARREST-REASON)))

(DEFMETHOD (SELECT-MIXIN :UN-ARREST) (&AUX LAST-PROCESS)
  (AND (SETQ LAST-PROCESS (SEND SELF :PROCESS))
       (SEND LAST-PROCESS :REVOKE-ARREST-REASON)))

(DEFUN SYSTEM-SELECT (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Select a window.  Make its blinkers blink and input to be read through it."
  (DECLARE (:SELF-FLAVOR SELECT-MIXIN))
  (DO () ((MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (SEND SELF :EXPOSE T)
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (COND ((NEQ SELECTED-WINDOW SELF)
         (AND SELECTED-WINDOW (SEND SELECTED-WINDOW :DESELECT NIL))
         (SELECT-SHEET-BLINKERS SELF)
         (SETQ SELECTED-WINDOW SELF)))
  T)

(DEFMETHOD (SELECT-MIXIN :SELECT) (&OPTIONAL IGNORE)
  (LET ((LAST-PROCESS (SEND SELF :PROCESS)))
    (AND LAST-PROCESS (SEND LAST-PROCESS :REVOKE-ARREST-REASON :CALL)))
  (SYSTEM-SELECT)
  (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS))
  T)    ;For frames and :MOUSE-SELECT

(DEFMETHOD (SELECT-MIXIN :MOUSE-SELECT) (&REST ARGS)
  "Form of select used when 'mouseing' windows.  Clears all temp locks that are on the
window, as well as failing if the window is not fully within its superior."
  (SEND SELF :ACTIVATE)                         ;Maybe our size has to get adjusted first
  (WHEN (SHEET-WITHIN-SHEET-P SELF SUPERIOR)
    (SHEET-FREE-TEMPORARY-LOCKS SELF)           ;Flush all temp windows that cover us
    (LEXPR-SEND (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS) :SELECT ARGS)))

(DEFMETHOD (SELECT-MIXIN :BEFORE :SELECT) (&OPTIONAL (SAVE-SELECTED T) &AUX OSW)
  (SETQ OSW SELECTED-WINDOW)
  (OR EXPOSED-P (SEND SELF :EXPOSE T))
  (DO SHEET SUPERIOR (SHEET-SUPERIOR SHEET) (NULL SHEET)        ;Really onto the screen
    (OR (SHEET-EXPOSED-P SHEET) (SEND SHEET :EXPOSE)))
  (WITHOUT-INTERRUPTS
    (AND OSW SAVE-SELECTED (NEQ SELF OSW)
         (NEQ (SEND OSW :STATUS) ':DEACTIVATED) ;Deexposing can deactivate
         (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS OSW))))

(DEFMETHOD (SELECT-MIXIN :AFTER :SELECT) (&REST IGNORE)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) (SEND SELF :PROCESS))
  (KBD-GET-IO-BUFFER))

(DEFMETHOD (SELECT-MIXIN :DESELECT) (&OPTIONAL (RESTORE-SELECTED T))
  (LET ((SEL-P (EQ SELF SELECTED-WINDOW)))
    (WHEN SEL-P
      (DESELECT-SHEET-BLINKERS SELF)
      (SETQ SELECTED-WINDOW NIL)
      (COND ((EQ RESTORE-SELECTED ':DONT-SAVE))
            ((AND RESTORE-SELECTED
                  (NEQ RESTORE-SELECTED ':BEGINNING))
             (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF T)
             (IF (MEMQ RESTORE-SELECTED '(:LAST T))
                 (SELECT-PREVIOUS-WINDOW NIL NIL NIL))
             (WHEN (EQ RESTORE-SELECTED ':FIRST)
               (SELECT-PREVIOUS-WINDOW NIL NIL NIL)
               (ROTATE-TOP-OF-ARRAY PREVIOUSLY-SELECTED-WINDOWS -1)))
            (T (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF))))))

(DEFMETHOD (SELECT-MIXIN :AFTER :DESELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (SELECT-MIXIN :BEFORE :DEEXPOSE) (&REST IGNORE)
  (SEND SELF :DESELECT NIL))

;;;; Selection substitutes

;;; By putting this on ESSENTIAL-WINDOW we cause all windows to
;;; have a handler, if only for the sake of the wrapper below.
(DEFMETHOD (ESSENTIAL-WINDOW :DEFAULT :SELECT) (&OPTIONAL IGNORE)
  (FERROR "Attempt to select ~S.  Is this an error?" self)
  NIL)

(DEFWRAPPER (ESSENTIAL-WINDOW :SELECT) (ARGS . BODY)
  `(IF SELECTION-SUBSTITUTE
       (LEXPR-SEND SELECTION-SUBSTITUTE :SELECT ARGS)
     (DELAYING-SCREEN-MANAGEMENT
       (WHEN (SEND SUPERIOR :INFERIOR-SELECT SELF)
         ;; Not all of the flavors we are combined with can be selected -- this prevents
         ;; a gratuitous error from the compiler
         . ,BODY))))

;;; By putting this on ESSENTIAL-WINDOW we cause all windows to
;;; have a handler, if only for the sake of the wrapper below.
(DEFMETHOD (ESSENTIAL-WINDOW :DEFAULT :DESELECT) (&OPTIONAL IGNORE)
  NIL)

(DEFWRAPPER (ESSENTIAL-WINDOW :DESELECT) (ARGS . BODY)
  `(IF SELECTION-SUBSTITUTE
       (LEXPR-SEND SELECTION-SUBSTITUTE :DESELECT ARGS)
     (DELAYING-SCREEN-MANAGEMENT . ,BODY)))

(DEFMETHOD (ESSENTIAL-WINDOW :SET-SELECTION-SUBSTITUTE) (WINDOW)
  (LET ((FLAG (SEND SELF :SELF-OR-SUBSTITUTE-SELECTED-P)))
    (SETQ SELECTION-SUBSTITUTE (AND (NEQ WINDOW SELF) WINDOW))
    (WHEN FLAG
      (DELAYING-SCREEN-MANAGEMENT
        (SEND SELECTED-WINDOW :DESELECT :DONT-SAVE)
        (SEND SELF :SELECT NIL)))))

(DEFMETHOD (ESSENTIAL-WINDOW :REMOVE-SELECTION-SUBSTITUTE) (REMOVE-WINDOW SUGGEST-WINDOW)
  (AND TV:SELECTION-SUBSTITUTE
       (TV:SHEET-ME-OR-MY-KID-P TV:SELECTION-SUBSTITUTE REMOVE-WINDOW)
       (SEND SELF :SET-SELECTION-SUBSTITUTE SUGGEST-WINDOW)))

(DEFMETHOD (ESSENTIAL-WINDOW :ULTIMATE-SELECTION-SUBSTITUTE) ()
  (IF SELECTION-SUBSTITUTE
      (SEND SELECTION-SUBSTITUTE :ULTIMATE-SELECTION-SUBSTITUTE)
    SELF))

(DEFMETHOD (ESSENTIAL-WINDOW :SELF-OR-SUBSTITUTE-SELECTED-P) ()
  (OR (EQ SELF SELECTED-WINDOW)
      (AND SELECTION-SUBSTITUTE
           (SEND SELECTION-SUBSTITUTE :SELF-OR-SUBSTITUTE-SELECTED-P))))

(defmethod (essential-window :selected-p) ()
  (or (eq self selected-window)
      (and selection-substitute
           (send selection-substitute :self-or-substitute-selected-p))))

;;; Every window handles :ALIAS-FOR-SELECTED-WINDOWS.
;;; Normally that returns SELF.  In that case, the window is "top level for selection".
;;; However, it may also return a superior (to one or more levels) of the window.
;;; In that case, the window is not "top level for selection".
;;; It can also return a window not a superior, if this window
;;; is the selection-substitute (to one or more levels) of that one.
;;; The window which is returned ought to be top level for selection.

;;; By default, a window is "top level for selection"
;;; unless one of its superiors claims to override.
(DEFMETHOD (ESSENTIAL-WINDOW :ALIAS-FOR-SELECTED-WINDOWS) ()
  (OR (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-INFERIORS)) SELF))

(DEFMETHOD (ESSENTIAL-WINDOW :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-INFERIORS)))

;;; Only "top level for selection" windows appear in the list of previously selected windows.

(DEFFLAVOR DONT-SELECT-WITH-MOUSE-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Don't allow selection via the mouse and similar ways
Include this for windows that may be selected internally by a program, but which
will not work if just randomly selected, e.g. they do not have their own process.
They will then not show up in the Select system menu, or be gettable to in other
similar ways."))

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :ALIAS-FOR-SELECTED-WINDOWS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFFLAVOR NOT-EXTERNALLY-SELECTABLE-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Don't allow selection via the mouse and similar ways
Include this for windows that may be selected internally by a program, but which
will not work if just randomly selected, e.g. they do not have their own process.
They will then not show up in the Select system menu, or be gettable to in other
similar ways."))

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :ALIAS-FOR-SELECTED-WINDOWS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFFLAVOR ALIAS-FOR-INFERIORS-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "This window is top level for selection,
and its inferiors are all included under it for selection"))

(DEFMETHOD (ALIAS-FOR-INFERIORS-MIXIN :ALIAS-FOR-INFERIORS) ()
  (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS))

;;; Stuff for remembering a "ring buffer" of recently-selected windows
;;; This is an array whose 0'th element is the most recently selected
;;; window.  Successive elements are windows that were selected before
;;; that.  After the oldest entry, the rest of the array is NIL.  A
;;; window may only appear once in this array.  The selected-window does
;;; not appear at all, nor do deactivated windows.

;;; Only "top-level-for-selection" windows appear.

(DEFVAR PREVIOUSLY-SELECTED-WINDOWS (MAKE-ARRAY 20. :AREA PERMANENT-STORAGE-AREA))

(DEFUN ADD-TO-PREVIOUSLY-SELECTED-WINDOWS (WINDOW &OPTIONAL AT-END)
  (WITHOUT-INTERRUPTS
    (SETQ WINDOW (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
    (AND WINDOW (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS WINDOW))
    (DO ((I 0 (1+ I))
         (N (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS)))
        ((OR (NULL WINDOW) (= I N))
         (WHEN WINDOW
           (SETQ PREVIOUSLY-SELECTED-WINDOWS
                 (ADJUST-ARRAY-SIZE PREVIOUSLY-SELECTED-WINDOWS (+ N 10.)))
           (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS N) WINDOW)))
      (LET ((TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I)))
        (COND ((OR (NOT AT-END) (NULL TEM))
               (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) WINDOW)
               (SETQ WINDOW TEM)))))
    NIL))

(DEFUN REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS (WINDOW)
  (WITHOUT-INTERRUPTS
    (OR (NULL WINDOW)
        (DO ((I 0 (1+ I))
             (N (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS)))
            ((= I N) (NOT WINDOW))
          (COND ((EQ (AREF PREVIOUSLY-SELECTED-WINDOWS I) WINDOW)
                 (WHEN (NULL WINDOW)
                   (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS (1- I)) NIL)
                   (RETURN T))
                 (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) NIL)
                 (SETQ WINDOW NIL))
                ((NULL WINDOW)
                 (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS (1- I))
                       (AREF PREVIOUSLY-SELECTED-WINDOWS I))))))))

(DEFUN CHANGE-IN-PREVIOUSLY-SELECTED-WINDOWS (FROM-WINDOW TO-WINDOW)
  (WITHOUT-INTERRUPTS
    (SETQ FROM-WINDOW (SEND FROM-WINDOW :ALIAS-FOR-SELECTED-WINDOWS)
          TO-WINDOW (SEND TO-WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
    (COND ((AND (NULL FROM-WINDOW) (NULL TO-WINDOW)))
          ((AND FROM-WINDOW (NULL TO-WINDOW))
           (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS FROM-WINDOW))
          ((NULL FROM-WINDOW)
           ;; This shouldn't happen, but...
           (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS TO-WINDOW))
          (T (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
               (WHEN (EQ (AREF PREVIOUSLY-SELECTED-WINDOWS I) FROM-WINDOW)
                 (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) TO-WINDOW)
                 (RETURN T)))))))

(DEFUN SELECT-PREVIOUS-WINDOW (&OPTIONAL WINDOW (MOUSE-P T) (DEFAULT-TO-LISP-LISTENER T)
                                         MOUSE-SELECT)
  "Select the window that was selected before the current one.
  If WINDOW is non-NIL it tries to select that one, if it is active.
  MOUSE-P T (the default) means consider only windows selectable from the mouse.
  If no previously-selected window can be found, gets a Lisp listener,
  unless DEFAULT-TO-LISP-LISTENER is specified as NIL.
  Moves the current window to the end of the ring buffer rather than the beginning.
  Returns the window that was selected.  If MOUSE-SELECT a :MOUSE-SELECT message is
  sent rather than a :SELECT message."
  (AND WINDOW
       (SETQ WINDOW (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
       (EQ (SEND WINDOW :STATUS) ':DEACTIVATED)
       (SETQ WINDOW NIL))
  (OR WINDOW
      (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
        (AND (SETQ WINDOW (AREF PREVIOUSLY-SELECTED-WINDOWS I))
             (OR (NOT MOUSE-P) (SEND WINDOW :NAME-FOR-SELECTION))
             (RETURN WINDOW)))
      (SETQ WINDOW (AND DEFAULT-TO-LISP-LISTENER (IDLE-LISP-LISTENER))))
  (DELAYING-SCREEN-MANAGEMENT                   ;Avoid auto-select
    (LET ((SW SELECTED-WINDOW))
      (COND (SW (SEND SW :DESELECT NIL)
                (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SW T))))
    (COND ((AND WINDOW MOUSE-SELECT)
           (SEND WINDOW :MOUSE-SELECT))
          (WINDOW
           (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
           (SEND WINDOW :SELECT))))
  WINDOW)

(DEFUN DESELECT-AND-MAYBE-BURY-WINDOW (WINDOW &OPTIONAL (DESELECT-MODE :LAST))
  "Reselect previously selected window and bury WINDOW if that leaves it deexposed.
DESELECT-MODE may be :FIRST or :LAST, which says where to put WINDOW
on the list of previously selected windows.
:FIRST makes it the one that Terminal S will select."
  (DELAYING-SCREEN-MANAGEMENT
    (SEND WINDOW :DESELECT DESELECT-MODE)
    (UNLESS (SHEET-EXPOSED-P WINDOW) (SEND WINDOW :BURY))))

;;;; Basic set-edges stuff
(DEFFLAVOR ESSENTIAL-SET-EDGES () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-FIRST :VERIFY-NEW-EDGES))
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Normal EDGES getting//setting messages
Provides :SET-EDGES and related messages such as :SET-SIZE, :SET-POSITION, :FULL-SCREEN,
and :CENTER-AROUND."))

(DEFWRAPPER (ESSENTIAL-SET-EDGES :SET-EDGES) ((NL NT NR NB OPTION) . BODY)
  `(LET ((LIST (MULTIPLE-VALUE-LIST
                (SEND SUPERIOR :INFERIOR-SET-EDGES SELF NL NT NR NB OPTION))))
     (IF (NULL (CAR LIST))
         (VALUES-LIST (CDR LIST))
       . ,BODY)))

(DEFMETHOD (ESSENTIAL-SET-EDGES :AFTER :INIT) (IGNORE)
  (LET ((ERROR-MESSAGE (SEND SELF :VERIFY-NEW-EDGES X-OFFSET Y-OFFSET WIDTH HEIGHT)))
    (IF (NOT (NULL ERROR-MESSAGE))
        (FERROR ERROR-MESSAGE))))

(DEFUN SYSTEM-SET-EDGES (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM &OPTIONAL OPTION
                         &AUX (NEW-WIDTH (- NEW-RIGHT NEW-LEFT))
                              (NEW-HEIGHT (- NEW-BOTTOM NEW-TOP))
                              ERROR WINDOW-TO-BE-DEEXPOSED)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-SET-EDGES))
  (DELAYING-SCREEN-MANAGEMENT
    (DO (DONE RESULT) (())
      (SETQ WINDOW-TO-BE-DEEXPOSED
          (CATCH 'SET-EDGES
            (LOCK-SHEET (SELF)
              (SETQ RESULT
                (COND ((SETQ ERROR (SEND SELF :VERIFY-NEW-EDGES NEW-LEFT NEW-TOP
                                              NEW-WIDTH NEW-HEIGHT))
                       ;; Can't put window there
                       (CASE OPTION
                         (:VERIFY NIL)
                         (OTHERWISE
                          (FERROR ERROR))))
                      ((EQ OPTION ':VERIFY)
                       ;; "Only want to know"
                       T)
                      ((AND (= NEW-WIDTH WIDTH)
                            (= NEW-HEIGHT HEIGHT)
                            (= NEW-LEFT X-OFFSET)
                            (= NEW-TOP Y-OFFSET))
                       ;;Not changing size or position, just return T (we do the verify
                       ;; anyway in case something in the environment has made the current
                       ;; size no longer "ok", such as having the size of the
                       ;; superior change.)
                       T)
                      ((AND (= NEW-WIDTH WIDTH)
                            (= NEW-HEIGHT HEIGHT))
                       ;; Only moving the window, move it's bits behind its back
                       (LET ((CURRENT-RECTANGLE (LIST X-OFFSET Y-OFFSET
                                                      (+ X-OFFSET WIDTH)
                                                      (+ Y-OFFSET HEIGHT))))
                         (COND ((NOT EXPOSED-P)
                                (SHEET-SET-DEEXPOSED-POSITION NEW-LEFT NEW-TOP)
                                (APPLY 'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                                (SCREEN-CONFIGURATION-HAS-CHANGED SELF))
                               ((SHEET-TEMPORARY-P)
                                ;; For temporary windows, just deexpose and reexpose
                                (LET ((SELECT-P (EQ SELF SELECTED-WINDOW)))
                                  (SEND SELF :DEEXPOSE)
                                  (SEND SELF :EXPOSE NIL NIL NEW-LEFT NEW-TOP)
                                  (AND SELECT-P (SEND SELF :SELECT))))
                               (T
                                (OR (SHEET-BOUNDS-WITHIN-SHEET-P NEW-LEFT NEW-TOP
                                                                 WIDTH HEIGHT
                                                                 SUPERIOR)
                                    (FERROR "Attempt to move sheet ~S outside of superior"
                                            SELF))
                                ;; Make sure everyone under us is deexposed
                                (WITHOUT-INTERRUPTS
                                  (DOLIST (SISTER (SHEET-EXPOSED-INFERIORS SUPERIOR))
                                    (WHEN (AND (NEQ SELF SISTER)
                                               (SHEET-OVERLAPS-P SISTER NEW-LEFT NEW-TOP
                                                                 WIDTH HEIGHT))
                                      (THROW 'SET-EDGES SISTER))))
                                (SHEET-SET-EXPOSED-POSITION NEW-LEFT NEW-TOP)
                                (APPLY 'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                                (SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
                      (T
                       (LET ((CURRENT-RECTANGLE (LIST X-OFFSET Y-OFFSET
                                                      (+ X-OFFSET WIDTH)
                                                      (+ Y-OFFSET HEIGHT))))
                         (PRESERVE-SUBSTITUTE-STATUS SELF
                           (WITH-SHEET-DEEXPOSED (SELF)
                             (AND BIT-ARRAY
                                  (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
                             (SEND SELF :CHANGE-OF-SIZE-OR-MARGINS
                                        :LEFT NEW-LEFT
                                        :TOP NEW-TOP
                                        :WIDTH NEW-WIDTH
                                        :HEIGHT NEW-HEIGHT)
                             (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
                               (SEND SELF :REFRESH :SIZE-CHANGED))))
                         (AND BIT-ARRAY (SI:PAGE-OUT-ARRAY BIT-ARRAY))
                         (SETQ MOUSE-RECONSIDER T)
                         (APPLY #'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                         (SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
              (SETQ DONE T))))
      (IF DONE
          (RETURN (VALUES RESULT ERROR))
        (SEND WINDOW-TO-BE-DEEXPOSED :DEEXPOSE)))))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-EDGES) (&REST ARGS) (APPLY 'SYSTEM-SET-EDGES ARGS))

(DEFMETHOD (ESSENTIAL-SET-EDGES :VERIFY-NEW-EDGES) (NL NT NW NH)
  "Verifies that the edges are ok.  This method returns NIL unless the edges do not allow
enough room for the margins, or the window is exposed and will not fit within its superior."
  (COND ((OR (< NW (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))
             (< NH (+ TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))
         "Not enough room for margins")
        ((AND EXPOSED-P
              (NOT (SHEET-BOUNDS-WITHIN-SHEET-P NL NT NW NH SUPERIOR)))
         "Attempt to expose outside of superior")))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES X-OFFSET Y-OFFSET
                        (+ NEW-WIDTH X-OFFSET) (+ NEW-HEIGHT Y-OFFSET)
                        OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-INSIDE-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES X-OFFSET Y-OFFSET
                        (+ X-OFFSET NEW-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
                        (+ Y-OFFSET NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
                        OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES NEW-X NEW-Y
                        (+ WIDTH NEW-X) (+ HEIGHT NEW-Y)
                        OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :FULL-SCREEN) (&OPTIONAL OPTION)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND SUPERIOR :INSIDE-EDGES)
    (SEND SELF :SET-EDGES LEFT TOP RIGHT BOTTOM OPTION)))

(DEFMETHOD (ESSENTIAL-SET-EDGES :CENTER-AROUND) (X Y)
  (CENTER-WINDOW-AROUND SELF X Y))

(DEFMETHOD (ESSENTIAL-SET-EDGES :EXPOSE-NEAR) (MODE &OPTIONAL (WARP-MOUSE-P T))
  (EXPOSE-WINDOW-NEAR SELF MODE WARP-MOUSE-P))

(DEFUN CENTER-WINDOW-AROUND (WINDOW X Y &AUX (W (SHEET-WIDTH WINDOW))
                                             (H (SHEET-HEIGHT WINDOW))
                                             (SUPERIOR (SHEET-SUPERIOR WINDOW))
                                             SH SW)
  (SETQ X (MAX (SHEET-INSIDE-LEFT SUPERIOR) (- X (TRUNCATE W 2)))
        Y (MAX (SHEET-INSIDE-TOP SUPERIOR) (- Y (TRUNCATE H 2))))
  (AND (> (+ X W) (SETQ SW (SHEET-INSIDE-RIGHT SUPERIOR)))
       (SETQ X (MAX (SHEET-INSIDE-LEFT SUPERIOR) (- SW W))))
  (AND (> (+ Y H) (SETQ SH (SHEET-INSIDE-BOTTOM SUPERIOR)))
       (SETQ Y (MAX (SHEET-INSIDE-TOP SUPERIOR) (- SH H))))
  (SEND WINDOW :SET-POSITION X Y)
  (VALUES (+ X (TRUNCATE W 2)) (+ Y (TRUNCATE H 2))))

;;; Expose the window next to the rectangle, to the right if it will fit
(DEFUN MOVE-WINDOW-NEAR-RECTANGLE (WINDOW LEFT TOP RIGHT BOTTOM
                                   &OPTIONAL (EXPOSE-P T) (WARP-MOUSE-P T)
                                   &AUX WIDTH HEIGHT SUPERIOR
                                        NLEFT NTOP NRIGHT NBOTTOM TEM)
  "Move WINDOW near the specified rectangle.
Expose the window there unless EXPOSE-P is NIL.
Move the mouse to the window unless WARP-MOUSE-P is NIL."
  (MULTIPLE-VALUE (WIDTH HEIGHT) (SEND WINDOW :SIZE))
  (SETQ SUPERIOR (SHEET-SUPERIOR WINDOW))
  ;; Assuming window will go beside rectangle, try to center it vertically
  ;; but if that doesn't work butt it against the bottom of the superior.
  (SETQ NTOP (MIN (- (SHEET-INSIDE-BOTTOM SUPERIOR) HEIGHT)
                  (MAX (SHEET-INSIDE-TOP SUPERIOR)
                       (- (TRUNCATE (+ TOP BOTTOM) 2) (TRUNCATE HEIGHT 2))))
        NBOTTOM (+ NTOP HEIGHT))
  (COND (( (SHEET-INSIDE-RIGHT SUPERIOR) (SETQ TEM (+ RIGHT WIDTH)))
         (SETQ NLEFT RIGHT NRIGHT TEM))
        (( (SHEET-INSIDE-LEFT SUPERIOR) (SETQ TEM (- LEFT WIDTH)))
         (SETQ NRIGHT LEFT NLEFT TEM))
        (T  ;Not enough room on either side, center it horizontally above or below the rect
         (SETQ NLEFT (MIN (- (SHEET-INSIDE-RIGHT SUPERIOR) WIDTH)
                          (MAX (SHEET-INSIDE-LEFT SUPERIOR)
                               (- (TRUNCATE (+ LEFT RIGHT) 2) (TRUNCATE WIDTH 2))))
               NRIGHT (+ NLEFT WIDTH))
         (COND (( (SHEET-INSIDE-TOP SUPERIOR) (SETQ TEM (- TOP HEIGHT)))
                (SETQ NBOTTOM TOP NTOP TEM))
               (( (SHEET-INSIDE-BOTTOM SUPERIOR) (SETQ TEM (+ BOTTOM HEIGHT)))
                (SETQ NTOP BOTTOM NBOTTOM TEM))
               (T (SETQ NTOP (SHEET-INSIDE-TOP SUPERIOR)
                        NBOTTOM (+ NTOP HEIGHT))))))
  (SEND WINDOW :SET-EDGES NLEFT NTOP NRIGHT NBOTTOM :TEMPORARY)
  (AND EXPOSE-P (SEND WINDOW :EXPOSE))
  (AND WARP-MOUSE-P (SEND WINDOW :SET-MOUSE-POSITION
                                 (TRUNCATE WIDTH 2) (TRUNCATE HEIGHT 2))))

(compiler:make-obsolete EXPOSE-WINDOW-NEAR "use the :EXPOSE-NEAR operation.")

(DEFUN EXPOSE-WINDOW-NEAR (WINDOW MODE &OPTIONAL (WARP-MOUSE-P T) (EXPOSE-P T))
  "Move WINDOW near a place specified in MODE.
MODE is a list whose car is a keyword saying what the rest of the list means.
 (:POINT x y) - center window around that point.
 (:MOUSE) - center window around the current mouse position.
 (:RECTANGLE left top right bottom) - put window next to that rectangle.
 (:WINDOW windows...) - put this window near but not on top of the others.
Expose the window there unless EXPOSE-P is NIL.
Move the mouse to the window unless WARP-MOUSE-P is NIL."
  (UNLESS (SHEET-EXPOSED-P WINDOW)
    (SELECTQ (FIRST MODE)
      (:POINT
       (SEND WINDOW :CENTER-AROUND (SECOND MODE) (THIRD MODE))
       (when warp-mouse-p
         (mouse-warp (second mode) (third mode))))
      (:MOUSE
       (IF (SHEET-ME-OR-MY-KID-P (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
           (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
               (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
             (MULTIPLE-VALUE-BIND (X Y)
                 (SEND WINDOW :CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
               (AND WARP-MOUSE-P (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF)))))
         ;; If mouse is not on a relevant sheet for this window,
         ;; pick any old place.
         (SEND WINDOW :CENTER-AROUND 0 0)))
      (:RECTANGLE
       (MOVE-WINDOW-NEAR-RECTANGLE WINDOW
                                   (SECOND MODE) (THIRD MODE) (FOURTH MODE) (FIFTH MODE)
                                   NIL WARP-MOUSE-P))
      (:WINDOW
       (LOOP FOR NEAR-WINDOW IN (CDR MODE) WITH (LEFT1 RIGHT1 TOP1 BOTTOM1 X-OFF Y-OFF)
          DO (MULTIPLE-VALUE-SETQ (LEFT1 TOP1 RIGHT1 BOTTOM1)
               (SEND NEAR-WINDOW :EDGES))
             (MULTIPLE-VALUE-BIND (X-OFF-1 Y-OFF-1)
                 (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR WINDOW)
                                          (SHEET-GET-SCREEN WINDOW))
               (MULTIPLE-VALUE-BIND (X-OFF-2 Y-OFF-2)
                   (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR NEAR-WINDOW)
                                            (SHEET-GET-SCREEN NEAR-WINDOW))
                 (SETQ X-OFF (- X-OFF-1 X-OFF-2)
                       Y-OFF (- Y-OFF-1 Y-OFF-2))))
          MINIMIZE (- LEFT1 X-OFF) INTO LEFT
          MINIMIZE (- TOP1 Y-OFF) INTO TOP
          MAXIMIZE (- RIGHT1 X-OFF) INTO RIGHT
          MAXIMIZE (- BOTTOM1 Y-OFF) INTO BOTTOM
          FINALLY (MOVE-WINDOW-NEAR-RECTANGLE WINDOW LEFT TOP RIGHT BOTTOM
                                              NIL WARP-MOUSE-P)))
      (OTHERWISE (FERROR "~S invalid mode" (FIRST MODE))))
    (AND EXPOSE-P (SEND WINDOW :EXPOSE))))

;;;;Things that hack margins (borders and labels and other such things)

;;;In order to interact correctly with adjusting the size of the margins, flavors
;;;that handle an area of the window further outside should come higher in the hierarchy,
;;;that is their pre-daemons should be called first.

#|
Here is what you write to make a mixin define something that uses up margin space:

;; This links this mixin into the computation of how much margin space is used.
;; :PASS-ON method combination is used, so actually four values are expected
;; and they resemble the arguments (but they are different).
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :RECALCULATE-MUMBLE-MARGINS LM TM RM BM))

This method returns updated values of LM, TM, RM and BM that are made
larger as appropriate, to take account of the space used up by the "mumbles".
It should also record
:RECALCULATE-MUMBLE-MARGINS is a separate operation so that mixins
can modify where the mumbles go in the margins by redefining it.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :RECALCULATE-MUMBLE-MARGINS) (LM TM RM BM)
  ;; Here we assume that CURRENT-MUMBLES is an instance variable
  ;; that specifies what kind of mumbles this window should display now,
  ;; and MUMBLE-MARGIN-WIDTH figures out how wide a space they need.
  ;; MUMBLE-MARGIN-AREA is set to a list describing the rectangle
  ;; where the mumbles should go;
  ;; all four values relative to outside top left corner of window.
  (LET ((WID (MUMBLE-MARGIN-WIDTH CURRENT-MUMBLES)))
    (SETQ MUMBLE-MARGIN-AREA (LIST LM TM (+ LM WID) (- TV:HEIGHT BM)))
    (VALUES (+ LM WID) TM RM BM)))

;; Here is an example of an operation provided to the user
;; whereby he can change the stuff to go in the margins.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :SET-MUMBLES) (NEW-MUMBLES)
  (SETQ CURRENT-MUMBLES (CANONICALIZE-MUMBLE-SPEC NEW-MUMBLES))
  ;; Cause the changed specs for mumbles to be redigested
  ;; together with all the other kinds of margin items,
  ;; and the window inside size to be changed if necessary.
  (SEND SELF :REDEFINE-MARGINS)
  (WHEN RESTORED-BITS-P
    ;; This is true if the margin area has not changed.
    ;; We must clear out the margin area for mumbles and draw the new mumbles there.
    ;; (If the margin size changed, everything has been updated by :REDEFINE-MARGINS).
    (ERASE-MUMBLE-AREA MUMBLE-MARGIN-AREA)
    (DRAW-MUMBLES CURRENT-MUMBLES MUMBLE-MARGIN-AREA)))

;; This operation's purpose is to redraw everything in the margins.
;; So each mixin that defines something in the margins must add to it.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DRAW-MUMBLES CURRENT-MUMBLES MUMBLE-MARGIN-AREA))
|#

(DEFMETHOD (ESSENTIAL-SET-EDGES :REDEFINE-MARGINS) ()
  (SETQ RESTORED-BITS-P T)
  (MULTIPLE-VALUE-BIND (LM TM RM BM)
      (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
    (UNLESS (AND (= LEFT-MARGIN-SIZE LM)
                 (= TOP-MARGIN-SIZE TM)
                 (= RIGHT-MARGIN-SIZE RM)
                 (= BOTTOM-MARGIN-SIZE BM))
      (PRESERVE-SUBSTITUTE-STATUS SELF
        (WITH-SHEET-DEEXPOSED (SELF)
          (AND BIT-ARRAY (SI:PAGE-IN-ARRAY BIT-ARRAY))
          (LET ((INSIDE-SIZE-CHANGED
                  (SEND SELF :CHANGE-OF-SIZE-OR-MARGINS
                             :LEFT-MARGIN-SIZE LM
                             :TOP-MARGIN-SIZE TM
                             :RIGHT-MARGIN-SIZE RM
                             :BOTTOM-MARGIN-SIZE BM)))
            (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
              (SEND SELF :REFRESH (IF INSIDE-SIZE-CHANGED
                                      :SIZE-CHANGED
                                      :MARGINS-ONLY))))
          (AND BIT-ARRAY (SI:PAGE-OUT-ARRAY BIT-ARRAY)))))))

(DEFMETHOD (ESSENTIAL-SET-EDGES :BEFORE :INIT) (IGNORE)
  (MULTIPLE-VALUE (LEFT-MARGIN-SIZE TOP-MARGIN-SIZE RIGHT-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
    (SEND SELF :COMPUTE-MARGINS 0 0 0 0)))

(DEFMETHOD (ESSENTIAL-SET-EDGES :DEFAULT :COMPUTE-MARGINS) (LM TM RM BM)
  (VALUES LM TM RM BM))

;;;; Borders - a kind of thing to put in a margin.
(DEFFLAVOR BORDERS-MIXIN ((BORDERS T) (BORDER-MARGIN-WIDTH 1)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES BORDERS BORDER-MARGIN-WIDTH)
  (:INITABLE-INSTANCE-VARIABLES BORDERS BORDER-MARGIN-WIDTH)
  (:DOCUMENTATION :MIXIN "Normal borders.
This flavor should provide general enough handling of the borders for most uses.
See the description of the :BORDERS init option for the format of the BORDERS instance
variable."))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDER-MARGIN-WIDTH) (NEW-WIDTH)
  (SETQ BORDER-MARGIN-WIDTH NEW-WIDTH)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDERS) (NEW-BORDERS)
  (SEND SELF :SET-BORDERS-INTERNAL NEW-BORDERS 0 0 0 0)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (BORDERS-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :SET-BORDERS-INTERNAL BORDERS LM TM RM BM))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDERS-INTERNAL) (SPEC LM TM RM BM)
  (MULTIPLE-VALUE (BORDERS LM TM RM BM)
    (PARSE-BORDERS-SPEC SPEC LM TM RM BM 'DRAW-RECTANGULAR-BORDER))
  (VALUES LM TM RM BM))

;;; This handles the actual drawing of the borders
(DEFUN DRAW-BORDERS (ALU)
  (DECLARE (:SELF-FLAVOR BORDERS-MIXIN))
  (SHEET-FORCE-ACCESS (SELF)
    (PREPARE-SHEET (SELF)
      (DOLIST (BORDER BORDERS)
        (AND BORDER
             (NEQ BORDER ':ZERO)
             (LET ((LEFT (SECOND BORDER))
                   (TOP (THIRD BORDER))
                   (RIGHT (FOURTH BORDER))
                   (BOTTOM (FIFTH BORDER)))
               (FUNCALL (FIRST BORDER) SELF ALU
                        (IF (MINUSP LEFT) (+ LEFT WIDTH) LEFT)
                        (IF (MINUSP TOP) (+ TOP HEIGHT) TOP)
                        (IF (PLUSP RIGHT) RIGHT (+ RIGHT WIDTH))
                        (IF (PLUSP BOTTOM) BOTTOM (+ BOTTOM HEIGHT)))))))))

;;; This is called with the new border specification and the current (relative to this
;;; redefining) margins, and should return the canonical form of the border, and the four new
;;; margins.
(DEFUN PARSE-BORDERS-SPEC (SPEC LM TM RM BM FUNCTION &OPTIONAL DEFAULT-SIZE)
  (DECLARE (:SELF-FLAVOR BORDERS-MIXIN))
  (COND ;;NIL means no borders at all
        (SPEC
         ;;A symbol or an number means that type for each of the four, else make a copy
         ;;a plist of (:LEFT FOO :RIGHT BAR) works too
         (SETQ SPEC (COND ((ATOM SPEC)
                           (SETQ SPEC (LIST SPEC SPEC SPEC SPEC)))
                          ((MEMQ (CAR SPEC) '(:LEFT :RIGHT :TOP :BOTTOM))
                           (DO ((NSPEC (IF (ATOM BORDERS)
                                           (LIST BORDERS BORDERS BORDERS BORDERS)
                                         (COPY-LIST BORDERS)))
                                (SPEC SPEC (CDDR SPEC)))
                               ((NULL SPEC) NSPEC)
                             (SETF (NTH (FIND-POSITION-IN-LIST (CAR SPEC)
                                                               '(:LEFT :RIGHT :TOP :BOTTOM))
                                        NSPEC)
                                   (CADR SPEC))))
                          (T
                           (COPY-LIST SPEC))))
         (DO ((SPEC SPEC (CDR SPEC))
              (ITEM))
             ((NULL SPEC))
           (COND ((OR (NULL (SETQ ITEM (CAR SPEC)))
                      (EQ ITEM ':ZERO)))
                 ;;A number means that width of the default function
                 ((NUMBERP ITEM)
                  (SETF (CAR SPEC) (CONS FUNCTION ITEM)))
                 ;;A symbol means that function and its default width
                 ((SYMBOLP ITEM)
                  (AND (EQ ITEM T) (SETQ ITEM FUNCTION))
                  (SETF (CAR SPEC) (CONS ITEM (OR DEFAULT-SIZE
                                                  (GET ITEM 'DEFAULT-BORDER-SIZE)))))))
         (DO ((SPEC SPEC (CDR SPEC))
              (TYPES '(:LEFT :TOP :RIGHT :BOTTOM) (CDR TYPES))
              (TYPE)
              (ITEM)
              (-WIDTH-))
             ((NULL SPEC))
           ;;A cons of a symbol and a number is the CAR function with the CDR width
           (AND (SETQ ITEM (CAR SPEC)) (CONSP ITEM) (SETQ -WIDTH- (CDR ITEM))
                (IF (ATOM -WIDTH-)
                    (SETF (CDR ITEM) (LIST (IF (EQ (SETQ TYPE (CAR TYPES)) ':RIGHT) -WIDTH- 0)
                                           (IF (EQ TYPE ':BOTTOM) -WIDTH- 0)
                                           (IF (EQ TYPE ':LEFT) -WIDTH- 0)
                                           (IF (EQ TYPE ':TOP) -WIDTH- 0)))
                  ;;Else make entries relative
                  (SETQ TYPE (CAR TYPES))
                  (LET ((-WIDTH- (- (FOURTH ITEM) (SECOND ITEM)))
                        (-HEIGHT- (- (FIFTH ITEM) (THIRD ITEM))))
                    (SETF (SECOND ITEM) (IF (EQ TYPE ':RIGHT) -WIDTH- 0))
                    (SETF (THIRD ITEM) (IF (EQ TYPE ':BOTTOM) -HEIGHT- 0))
                    (SETF (FOURTH ITEM) (IF (EQ TYPE ':LEFT) -WIDTH- 0))
                    (SETF (FIFTH ITEM) (IF (EQ TYPE ':TOP) -HEIGHT- 0))))))
         ;;Now adjust all non-NIL items for the current margins
         (DO ((SPEC SPEC (CDR SPEC))
              (TYPES '(:LEFT :TOP :RIGHT :BOTTOM) (CDR TYPES))
              (TYPE)
              (ITEM)
              (-WIDTH-)
              (-HEIGHT-))
             ((NULL SPEC))
           (COND ((AND (SETQ ITEM (CAR SPEC)) (CONSP ITEM))
                  (SETQ TYPE (CAR TYPES))
                  (SETQ -WIDTH- (ABS (- (FOURTH ITEM) (SECOND ITEM)))
                        -HEIGHT- (ABS (- (FIFTH ITEM) (THIRD ITEM))))
                  (COND ((SELECTQ TYPE
                           ((:LEFT :RIGHT) (ZEROP -WIDTH-))
                           ((:TOP :BOTTOM) (ZEROP -HEIGHT-)))
                         (SETF (CAR SPEC) ':ZERO))
                        (T ;; Order here is L R T B to give symmetry
                           (SETF (SECOND ITEM)
                                 (IF (EQ TYPE ':RIGHT)
                                     (- (+ (SECOND ITEM) RM))
                                   (+ (SECOND ITEM) LM)))
                           (SETF (FOURTH ITEM)
                                 (IF (EQ TYPE ':LEFT)
                                     (+ (FOURTH ITEM) LM)
                                   (- (+ (FOURTH ITEM) RM))))
                           (SETF (THIRD ITEM)
                                 (IF (EQ TYPE ':BOTTOM)
                                     (- (+ (THIRD ITEM) BM))
                                   (+ (THIRD ITEM) TM)))
                           (SETF (FIFTH ITEM)
                                 (IF (EQ TYPE ':TOP)
                                     (+ (FIFTH ITEM) TM)
                                   (- (+ (FIFTH ITEM) BM))))
                           (SELECTQ TYPE
                             (:LEFT (SETQ LM (+ LM -WIDTH-)))
                             (:TOP (SETQ TM (+ TM -HEIGHT-)))
                             (:RIGHT (SETQ RM (+ RM -WIDTH-)))
                             (:BOTTOM (SETQ BM (+ BM -HEIGHT-)))))))))
         ;;Now account for the extra margin
         (AND (FIRST SPEC) (SETQ LM (+ LM BORDER-MARGIN-WIDTH)))
         (AND (SECOND SPEC) (SETQ TM (+ TM BORDER-MARGIN-WIDTH)))
         (AND (THIRD SPEC) (SETQ RM (+ RM BORDER-MARGIN-WIDTH)))
         (AND (FOURTH SPEC) (SETQ BM (+ BM BORDER-MARGIN-WIDTH)))))
  (VALUES SPEC LM TM RM BM))

(DEFMETHOD (BORDERS-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DRAW-BORDERS CHAR-ALUF))

(DEFPROP DRAW-RECTANGULAR-BORDER 1 DEFAULT-BORDER-SIZE)
(DEFUN DRAW-RECTANGULAR-BORDER (WINDOW ALU LEFT TOP RIGHT BOTTOM)
  (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ALU WINDOW))

(DEFFLAVOR MARGIN-SPACE-MIXIN ((SPACE (LIST 0 0 0 0))) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES SPACE)
  (:INITTABLE-INSTANCE-VARIABLES SPACE))

(DEFMETHOD (MARGIN-SPACE-MIXIN :BEFORE :INIT) (IGNORE)
  (COND ((NULL SPACE)
         (SETQ SPACE '(0 0 0 0)))
        ((EQ SPACE T)
         (SETQ SPACE '(1 1 1 1)))
        ((FIXP SPACE)
         (SETQ SPACE (MAKE-LIST 4 :INITIAL-ELEMENT SPACE)))
        ((ATOM SPACE)
         (SETQ SPACE '(0 0 0 0)))))

(DEFMETHOD (MARGIN-SPACE-MIXIN :SET-SPACE) (NEW-SPACE)
  (COND ((NULL NEW-SPACE)
         (SETQ SPACE '(0 0 0 0)))
        ((EQ NEW-SPACE T)
         (SETQ SPACE '(1 1 1 1)))
        ((FIXP NEW-SPACE)
         (SETQ SPACE (MAKE-LIST 4 :INITIAL-ELEMENT NEW-SPACE)))
        ((ATOM NEW-SPACE)
         (SETQ SPACE '(0 0 0 0)))
        (T (SETQ SPACE NEW-SPACE)))
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (MARGIN-SPACE-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (VALUES (+ LM (FIRST SPACE)) (+ TM (SECOND SPACE))
          (+ RM (THIRD SPACE)) (+ BM (FOURTH SPACE))))

;;;; Labels
(DEFSTRUCT (ESSENTIAL-LABEL-MIXIN :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  LABEL-LEFT                                    ;Coordinates of the label, all relative to the
  LABEL-TOP                                     ;edges of the window
  LABEL-RIGHT
  LABEL-BOTTOM)

(DEFFLAVOR ESSENTIAL-LABEL-MIXIN ((LABEL T)) ()
  (:GETTABLE-INSTANCE-VARIABLES LABEL)
  (:INITABLE-INSTANCE-VARIABLES LABEL)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :PARSE-LABEL-SPEC :DRAW-LABEL)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Lowlevel LABEL handling
This flavor probably isn't any good without some other label mixin.  See LABEL-MIXIN
for the normal label handler."))

(DEFFLAVOR WINDOW-WITH-ESSENTIAL-LABEL () (STREAM-MIXIN BORDERS-MIXIN ESSENTIAL-LABEL-MIXIN
                                                 SELECT-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Simple window for special label handling
Mix this with a special type of label mixin to get the simplest usable case of that mixin."))

(DEFUN ERASE-LABEL (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN))
  (AND LABEL
       (SHEET-FORCE-ACCESS (SELF)
         (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
             (COMPUTE-LABEL-POSITION)
           (PREPARE-SHEET (SELF)
             (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF))))))

(DEFUN DRAW-LABEL (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN))
  (AND LABEL
       (SHEET-FORCE-ACCESS (SELF)
         (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
             (COMPUTE-LABEL-POSITION)
           (SEND SELF :DRAW-LABEL LABEL LEFT TOP RIGHT BOTTOM)))))

(DEFUN COMPUTE-LABEL-POSITION (&AUX LEFT TOP RIGHT BOTTOM)
  "Return four edges of the rectangle the label of SELF occupies.
These are relative to the top left corner of SELF."
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN)
           (RETURN-LIST LEFT TOP RIGHT BOTTOM))
  (SETQ LEFT (LABEL-LEFT LABEL) TOP (LABEL-TOP LABEL)
        RIGHT (LABEL-RIGHT LABEL) BOTTOM (LABEL-BOTTOM LABEL))
  (SETQ BOTTOM (- BOTTOM TOP))                  ;Really height
  (AND (MINUSP TOP) (SETQ TOP (+ HEIGHT TOP)))
  (VALUES (IF (MINUSP LEFT) (+ WIDTH LEFT) LEFT) TOP
          (IF (PLUSP RIGHT) RIGHT (+ WIDTH RIGHT)) (+ TOP BOTTOM)))

;;; This is designed to be a subroutine of :PARSE-LABEL-SPEC messages.  It makes the label
;;; into a list, onto which other things can then be added.
(DEFUN PARSE-LABEL-SPEC-1 (SPEC LM TM RM BM &OPTIONAL (-HEIGHT- NIL HEIGHT-P) TOP-P)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN))
  (OR -HEIGHT-
      (SETQ -HEIGHT- (FONT-CHAR-HEIGHT (SCREEN-DEFAULT-FONT (SHEET-GET-SCREEN SELF)))))
  (COND (SPEC
         (SETQ TOP-P (COND ((MEMQ SPEC '(:TOP :BOTTOM))
                            (EQ SPEC ':TOP))
                           ((AND (CONSP SPEC) (LABEL-TOP SPEC))
                            (NOT (MINUSP (LABEL-TOP SPEC))))
                           (T
                            TOP-P)))
         (SETQ SPEC (IF (CONSP SPEC) (COPY-LIST SPEC) (MAKE-LIST 4)))
         (LET ((BOTTOM (LABEL-BOTTOM SPEC)) (TOP (LABEL-TOP SPEC)))
           (AND BOTTOM TOP (NOT HEIGHT-P) (SETQ -HEIGHT- (- BOTTOM TOP))))
         (SETF (LABEL-LEFT SPEC) LM)
         (SETF (LABEL-RIGHT SPEC) (- RM))
         (LET ((TOP (IF TOP-P TM (- (+ BM -HEIGHT-)))))
           (SETF (LABEL-TOP SPEC) TOP)
           (SETF (LABEL-BOTTOM SPEC) (+ TOP -HEIGHT-)))
         (IF TOP-P (SETQ TM (LABEL-BOTTOM SPEC)) (SETQ BM (- (LABEL-TOP SPEC))))))
  (VALUES SPEC LM TM RM BM))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :LABEL-SIZE) ()
  (IF LABEL (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
                (COMPUTE-LABEL-POSITION)
              (VALUES (- RIGHT LEFT) (- BOTTOM TOP)))
    (VALUES 0 0)))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :SET-LABEL) (NEW-LABEL)
  (SETQ LABEL (SEND SELF :PARSE-LABEL-SPEC NEW-LABEL 0 0 0 0))
  (SEND SELF ':REDEFINE-MARGINS)
  (WHEN RESTORED-BITS-P
    (ERASE-LABEL)                       ;This has the right dimensions, even though it is the
    (DRAW-LABEL)))                      ;new label, because it occupies the same margin space.

;;;The label is refreshed by an after demon on :REFRESH-MARGINS, below.
;;;But this is intended for other callers who need the label redrawn.
;;;(Possibly some dark external force has clobbered the bits, e.g. the
;;;SDU on the Lambda during warm-boot.)

(defmethod (essential-label-mixin :redraw-label) ()
  (erase-label)
  (draw-label))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DRAW-LABEL))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (MULTIPLE-VALUE (LABEL LM TM RM BM)
      (SEND SELF ':PARSE-LABEL-SPEC LABEL LM TM RM BM))
  (VALUES LM TM RM BM))

(DEFSTRUCT (LABEL-MIXIN :LIST (:INCLUDE ESSENTIAL-LABEL-MIXIN)
                        (:CONSTRUCTOR NIL) (:ALTERANT NIL)
                        (:SIZE-SYMBOL LABEL-DEFSTRUCT-SIZE))
  LABEL-FONT
  LABEL-STRING
  LABEL-VSP
  LABEL-CENTERED)

(DEFFLAVOR LABEL-MIXIN () (ESSENTIAL-LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Normal LABEL handling.
This is the usual type of label a window will want, it provides for an arbitrary string
in an arbitrary font."))

(DEFMETHOD (LABEL-MIXIN :AFTER :INIT) (IGNORE)
  (AND LABEL (OR (LABEL-STRING LABEL) (SETF (LABEL-STRING LABEL) NAME))))

(DEFMETHOD (LABEL-MIXIN :PARSE-LABEL-SPEC) (SPEC LM TM RM BM
                                            &OPTIONAL TOP-P
                                            &AUX FONT NSPEC LSTRING)
  (COND (SPEC
         (AND (MEMQ (CAR-SAFE SPEC) '(:STRING :FONT :TOP :BOTTOM :VSP :CENTERED))
              (DO ((LIST SPEC (CDR LIST))
                   (STRING NIL)
                   VSP
                   CENTERED)
                  ((NULL LIST)
                   (SETQ SPEC (LIST NIL NIL NIL NIL FONT STRING VSP CENTERED)))
                (SELECTQ (CAR LIST)
                  (:STRING (SETQ STRING (CADR LIST)
                                 LIST (CDR LIST)))
                  (:FONT (SETQ FONT (CADR LIST)
                               LIST (CDR LIST)))
                  (:VSP (SETQ VSP (CADR LIST) LIST (CDR LIST)))
                  (:CENTERED (SETQ CENTERED T))
                  (:TOP (SETQ TOP-P T))
                  (:BOTTOM (SETQ TOP-P NIL))
                  (OTHERWISE (FERROR "~S is not a recognized keyword" (CAR LIST))))))
         (SETQ FONT (OR (AND (TYPEP SPEC 'FONT) (PROG1 SPEC (SETQ SPEC T)))
                        (AND (CONSP SPEC) (LABEL-FONT SPEC))
                        (SEND  (SHEET-GET-SCREEN SELF) ':FONT-NAME-FOR ':LABEL)))
         (SETQ FONT (SEND (SHEET-GET-SCREEN SELF) ':PARSE-FONT-NAME FONT))
         (SETQ LSTRING (COND ((STRINGP SPEC) SPEC)
                             ((AND (CONSP SPEC) (LABEL-STRING SPEC))
                              (LABEL-STRING SPEC))
                             ((NEQ SPEC T) (STRING SPEC))
                             (T NAME)))
         (AND (CONSP SPEC) (LABEL-TOP SPEC) (SETQ TOP-P (NOT (MINUSP (LABEL-TOP SPEC)))))
         (MULTIPLE-VALUE (NSPEC LM TM RM BM)
           (PARSE-LABEL-SPEC-1 SPEC LM TM RM BM
                               (LABEL-HEIGHT SELF LSTRING FONT
                                             (AND (CONSP SPEC) (LABEL-VSP SPEC)))
                               TOP-P))
         (LET ((TEM (- LABEL-DEFSTRUCT-SIZE (LENGTH NSPEC))))
           (AND (> TEM 0) (RPLACD (LAST NSPEC) (MAKE-LIST TEM))))
         (SETF (LABEL-FONT NSPEC) FONT)
         (OR (LABEL-STRING NSPEC)
             (SETF (LABEL-STRING NSPEC) LSTRING))
         (SETQ SPEC NSPEC)))
  (VALUES SPEC LM TM RM BM))

(DEFUN LABEL-HEIGHT (SHEET LABEL-STRING FONT &OPTIONAL LABEL-VSP)
  (COND ((NULL LABEL-STRING)    ;Kludge patch for bug that label string may not be set up yet.
         (FONT-CHAR-HEIGHT (FONT-EVALUATE FONT)))
        (T
         (LET ((FONT (FONT-EVALUATE FONT)))
           (MULTIPLE-VALUE-BIND (NIL FINAL-Y)
               (SHEET-COMPUTE-MOTION SHEET 0 0 LABEL-STRING
                                     0 NIL T 0 1.0S10 1.0S10 1.0S10
                                     FONT
                                     (+ (OR LABEL-VSP 2)
                                        (FONT-CHAR-HEIGHT FONT)))
             FINAL-Y)))))

(DEFMETHOD (LABEL-MIXIN :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  (AND SPEC
       (LET ((FONT (FONT-EVALUATE (LABEL-FONT SPEC))))
         (SEND SELF
           (IF (LABEL-CENTERED SPEC)
               ':STRING-OUT-CENTERED-EXPLICIT
             ':STRING-OUT-EXPLICIT)
           (LABEL-STRING SPEC) LEFT TOP RIGHT BOTTOM
           FONT CHAR-ALUF
           0 NIL
           (+ (OR (LABEL-VSP SPEC) 2)
              (FONT-CHAR-HEIGHT FONT))))))

(DEFMETHOD (LABEL-MIXIN :LABEL-SIZE) ()
  (IF LABEL
      (LET ((FONT (FONT-EVALUATE (LABEL-FONT LABEL))))
        (MULTIPLE-VALUE-BIND (NIL FINAL-Y NIL MAXIMUM-X)
            (SHEET-COMPUTE-MOTION SELF 0 0 (LABEL-STRING LABEL)
                                  0 NIL T 0 1.0S10 1.0S10 1.0S10
                                  FONT
                                  (+ (OR (LABEL-VSP LABEL) 2)
                                     (FONT-CHAR-HEIGHT FONT)))
          (VALUES MAXIMUM-X FINAL-Y)))
    (VALUES 0 0)))

(DEFMETHOD (LABEL-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (COND ((AND LABEL (EQ (LABEL-FONT LABEL) OLD-FONT))
         (SETF (LABEL-FONT LABEL) NEW-FONT)))
  (SEND SELF ':SET-LABEL LABEL))

(DEFFLAVOR DELAYED-REDISPLAY-LABEL-MIXIN ((LABEL-NEEDS-UPDATING NIL)) ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES LABEL-NEEDS-UPDATING)
  (:DOCUMENTATION :MIXIN "Delays the setting of the label until a normal redisplay loop.
Send a :DELAYED-SET-LABEL to cause the label to be changed when a :UPDATE-LABEL message
is sent.  This is especially useful for things with suppressed redisplay for typeahead,
where the user's typein may change the label several times, and where the label wants to
change along with the rest of the window."))

(DEFMETHOD (DELAYED-REDISPLAY-LABEL-MIXIN :DELAYED-SET-LABEL) (NEW-LABEL)
  (SETQ LABEL-NEEDS-UPDATING NEW-LABEL))

(DEFMETHOD (DELAYED-REDISPLAY-LABEL-MIXIN :UPDATE-LABEL) ()
  (COND (LABEL-NEEDS-UPDATING
         (SEND SELF ':SET-LABEL LABEL-NEEDS-UPDATING)
         (SETQ LABEL-NEEDS-UPDATING NIL))))

(DEFFLAVOR TOP-LABEL-MIXIN () (LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label positioned at the top
If the label is specified only as a string or defaults to the name of the window, it
will be at the top of the window."))

(DEFMETHOD (TOP-LABEL-MIXIN :PARSE-LABEL-SPEC) (SPEC LM TM RM BM)
  (FUNCALL #'(:METHOD LABEL-MIXIN :PARSE-LABEL-SPEC) ':PARSE-LABEL-SPEC SPEC LM TM RM BM T))

(DEFFLAVOR CENTERED-LABEL-MIXIN () ()
  (:REQUIRED-FLAVORS LABEL-MIXIN))

(DEFMETHOD (CENTERED-LABEL-MIXIN :BEFORE :DRAW-LABEL) (&REST IGNORE)
  (IF LABEL (SETF (LABEL-CENTERED LABEL) T)))

(DEFFLAVOR BOX-LABEL-MIXIN (LABEL-BOX-P) ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  :INITABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :LABEL-BOX-P T)
  (:DOCUMENTATION :MIXIN "Puts lines above and below the label
(Lines at the edges of the window are omitted).
When combined with BORDERS-MIXIN, you get a box around the label."))

(DEFMETHOD (BOX-LABEL-MIXIN :AFTER :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  SPEC
  (AND LABEL-BOX-P
       (SHEET-FORCE-ACCESS (SELF)
         (PREPARE-SHEET (SELF)
           (IF (>= RIGHT (SHEET-INSIDE-RIGHT)) (SETQ RIGHT WIDTH))
           (IF (<= LEFT (SHEET-INSIDE-LEFT)) (SETQ LEFT 0))
           (OR (>= BOTTOM (SHEET-INSIDE-BOTTOM))
               (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- BOTTOM) CHAR-ALUF SELF))
           (OR (<= TOP (SHEET-INSIDE-TOP))
               (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- TOP) CHAR-ALUF SELF))))))

(DEFWRAPPER (BOX-LABEL-MIXIN :PARSE-LABEL-SPEC) ((SPEC LM TM RM BM) . BODY)
  `(MULTIPLE-VALUE-BIND (NSPEC NLM NTM NRM NBM)
       (PROGN . ,BODY)
     SPEC RM LM
     (COND (LABEL-BOX-P
            (OR (= NTM TM)
                (INCF NTM 2))
            (OR (= NBM BM)
                (INCF NBM 2))))
     (PROG () (RETURN (values NSPEC NLM NTM NRM NBM)))))

(DEFFLAVOR TOP-BOX-LABEL-MIXIN () (BOX-LABEL-MIXIN TOP-LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label at the top, with a line underneath
If the label is a string or defaults to the name, it is at the top.
When combined with BORDERS-MIXIN, the label will be surrounded by a box."))

(DEFFLAVOR BOTTOM-BOX-LABEL-MIXIN () (BOX-LABEL-MIXIN LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label at the bottom, with a line above.
If the label is a string or defaults to the name, it is at the bottom.
When combined with BORDERS-MIXIN, the label will be surrounded by a box."))

;;; Flavor that allows you to change the name of the window, and
;;; if the label is the same as the name, changes the label, too.
(DEFFLAVOR CHANGEABLE-NAME-MIXIN () ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Allows setting of name via :SET-NAME
Also changes the label if it happens to be the same."))

(DEFMETHOD (CHANGEABLE-NAME-MIXIN :NAME) () NAME)

(DEFMETHOD (CHANGEABLE-NAME-MIXIN :SET-NAME) (NEW-NAME)
  (LET ((LABEL-EQUALS-NAME (AND LABEL (EQ (LABEL-STRING LABEL) NAME))))
    (SETQ NAME NEW-NAME)
    (COND (LABEL-EQUALS-NAME
           (SETF (LABEL-STRING LABEL) NEW-NAME)
           (SHEET-FORCE-ACCESS (SELF T)
             (ERASE-LABEL)
             (DRAW-LABEL))))))

(DEFUN LOWEST-SHEET-UNDER-POINT (SHEET X Y &OPTIONAL OPERATION (ACTIVE-CONDITION ':ACTIVE))
  "Return the sheet lowest in the sheet hierarchy which contains the given point."
  ;; Trace down to find the lowest sheet under the point
  (DO-NAMED FOO
      ((X X (- X (SHEET-X-OFFSET SHEET)))
       (Y Y (- Y (SHEET-Y-OFFSET SHEET))))
      (NIL)
    (DO ((INFERIORS (IF (EQ ACTIVE-CONDITION ':EXPOSED)
                        (SHEET-EXPOSED-INFERIORS SHEET)
                      (SHEET-INFERIORS SHEET))
                    (CDR INFERIORS))
         (INFERIOR))
        ((NULL INFERIORS)
         (RETURN-FROM FOO))
      (SETQ INFERIOR (CAR INFERIORS))
      (COND ((AND (NOT (SHEET-INVISIBLE-TO-MOUSE-P INFERIOR))
                  ( X (SHEET-X-OFFSET INFERIOR)) ( Y (SHEET-Y-OFFSET INFERIOR))
                  (< X (+ (SHEET-X-OFFSET INFERIOR) (SHEET-WIDTH INFERIOR)))
                  (< Y (+ (SHEET-Y-OFFSET INFERIOR) (SHEET-HEIGHT INFERIOR)))
                  (SELECTQ ACTIVE-CONDITION
                    (:ACTIVE (OR (SHEET-EXPOSED-P INFERIOR)
                                 (SEND INFERIOR ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY)))
                    (:EXPOSED (NOT (SHEET-OUTPUT-HELD-P INFERIOR)))
                    (OTHERWISE T)))
             (SETQ SHEET INFERIOR)
             (RETURN T)))))
  (IF (NULL OPERATION) SHEET
      ;; Now trace back up until we find someone to handle the message
      (DO SHEET SHEET (SHEET-SUPERIOR SHEET) (NULL SHEET)
        (AND (GET-HANDLER-FOR SHEET OPERATION)
             (RETURN SHEET)))))

(DEFFLAVOR TEMPORARY-WINDOW-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Windows that save bits underneath and lock when exposed
Causes the temporary-bit-array instance variable to get set, which makes sheet exposure
behave appropriately."))

(DEFMETHOD (TEMPORARY-WINDOW-MIXIN :AFTER :INIT) (IGNORE)
  (OR (AND (VARIABLE-BOUNDP TEMPORARY-BIT-ARRAY) TEMPORARY-BIT-ARRAY)
      ;; T means will get created when needed
      (SETQ TEMPORARY-BIT-ARRAY T)))

;(DEFUN WINDOW-PUSH (WINDOW NEW-TYPE &REST INIT-PAIRS
;                                   &AUX NEW-WINDOW INIT-PLIST STATUS)
;  (SETQ INIT-PAIRS (COPYLIST INIT-PAIRS))      ;There should be a comment here saying why
;  (SETQ INIT-PLIST (LOCF INIT-PAIRS))
;  (LOCK-SHEET (WINDOW)
;    (SETQ STATUS (SEND WINDOW ':STATUS))
;    ;; Window we are "pushing" gets deactivated
;    (DELAYING-SCREEN-MANAGEMENT
;      (PUTPROP INIT-PLIST (SHEET-SUPERIOR WINDOW) ':SUPERIOR)
;      (PUTPROP INIT-PLIST (SHEET-X-OFFSET WINDOW) ':LEFT)
;      (PUTPROP INIT-PLIST (SHEET-Y-OFFSET WINDOW) ':TOP)
;      (PUTPROP INIT-PLIST (SHEET-HEIGHT WINDOW) ':HEIGHT)
;      (PUTPROP INIT-PLIST (SHEET-WIDTH WINDOW) ':WIDTH)
;      (AND (SI:FLAVOR-ALLOWS-INIT-KEYWORD-P NEW-TYPE ':PROCESS)
;          (GET-HANDLER-FOR WINDOW ':PROCESS)
;          (PUTPROP INIT-PLIST (FUNCALL WINDOW ':PROCESS) ':PROCESS))
;      (SETQ NEW-WINDOW (LEXPR-FUNCALL #'MAKE-WINDOW NEW-TYPE (CAR INIT-PLIST)))
;      (CHANGE-IN-PREVIOUSLY-SELECTED-WINDOWS WINDOW NEW-WINDOW)
;      (FUNCALL WINDOW ':DEACTIVATE)
;      (SCREEN-CONFIGURATION-HAS-CHANGED WINDOW)
;      (FUNCALL NEW-WINDOW ':SET-STATUS STATUS) ;Activate, expose, or select
;      (SCREEN-CONFIGURATION-HAS-CHANGED NEW-WINDOW))
;    NEW-WINDOW))

;(DEFUN WINDOW-POP (OLD-WINDOW WINDOW)
;  (LOCK-SHEET (WINDOW)
;    (DELAYING-SCREEN-MANAGEMENT
;      ;; Put back most of the sheet
;      (LET ((STATUS (FUNCALL WINDOW ':STATUS))
;           (SUPERIOR (SHEET-SUPERIOR WINDOW))
;           (X (SHEET-X-OFFSET WINDOW))
;           (Y (SHEET-Y-OFFSET WINDOW))
;           (W (SHEET-WIDTH WINDOW))
;           (H (SHEET-HEIGHT WINDOW)))
;       (CHANGE-IN-PREVIOUSLY-SELECTED-WINDOWS WINDOW OLD-WINDOW)
;       (FUNCALL WINDOW ':DEACTIVATE)
;       (FUNCALL OLD-WINDOW ':SET-SUPERIOR SUPERIOR)
;       (COND ((AND (FUNCALL OLD-WINDOW ':SET-SIZE W H ':VERIFY)
;                   (SHEET-BOUNDS-WITHIN-SHEET-P X Y W H SUPERIOR))
;              ;; Legal to set new edges, do it
;              (FUNCALL OLD-WINDOW ':SET-POSITION X Y)
;              (FUNCALL OLD-WINDOW ':SET-SIZE W H)))
;       (FUNCALL OLD-WINDOW ':SET-STATUS STATUS)))))


(DEFFLAVOR FULL-SCREEN-HACK-MIXIN ((OLD-BORDERS NIL)) ()
  (:REQUIRED-FLAVORS BORDERS-MIXIN)
  (:DOCUMENTATION :MIXIN "Has borders only when not the full size of the screen
For windows like the initial lisp listener which frequently occupy the whole screen and
are immediately recognizable."))

(DEFVAR *FULL-SCREEN-HACKING-WINDOWS* NIL)
(DEFVAR *FULL-SCREEN-WINDOWS-HAVE-BORDERS* T)

;;; Call this to erase (or redraw) the margins
(DEFUN FLUSH-FULL-SCREEN-BORDERS (&OPTIONAL (FLUSH T))
  (SETQ *FULL-SCREEN-WINDOWS-HAVE-BORDERS* (NOT FLUSH))
  (MAPC #'(LAMBDA (WINDOW)
            (SEND WINDOW ':ADJUST-MARGINS)) *FULL-SCREEN-HACKING-WINDOWS*))

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :BEFORE :KILL) ()
  (SETQ *FULL-SCREEN-HACKING-WINDOWS* (DELQ SELF *FULL-SCREEN-HACKING-WINDOWS*)))

;;; This unfortunately has to redefine the sheet, since the width and
;;; height are not known at (:BEFORE :INIT) time.
(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :AFTER :INIT) (IGNORE)
  (PUSH SELF *FULL-SCREEN-HACKING-WINDOWS*)
  (IF *FULL-SCREEN-WINDOWS-HAVE-BORDERS*
      (FULL-SCREEN-HACK X-OFFSET Y-OFFSET WIDTH HEIGHT)))

(DEFUN FULL-SCREEN-HACK (LEFT TOP WID HEI)
  (DECLARE (:SELF-FLAVOR FULL-SCREEN-HACK-MIXIN))
  (COND ((AND (= LEFT (SHEET-INSIDE-LEFT SUPERIOR))
              (= TOP (SHEET-INSIDE-TOP SUPERIOR))
              (= WID (SHEET-INSIDE-WIDTH SUPERIOR))
              (= HEI (SHEET-INSIDE-HEIGHT SUPERIOR)))
         (COND ((AND BORDERS (NULL OLD-BORDERS) (NOT *FULL-SCREEN-WINDOWS-HAVE-BORDERS*))
                (SETQ OLD-BORDERS BORDERS)
                (SEND SELF ':SET-BORDERS NIL))
               ((AND OLD-BORDERS (NULL BORDERS) *FULL-SCREEN-WINDOWS-HAVE-BORDERS*)
                (SEND SELF ':SET-BORDERS OLD-BORDERS)
                (SETQ OLD-BORDERS NIL))))
        (T
         (COND ((AND OLD-BORDERS (NULL BORDERS))
                (SEND SELF ':SET-BORDERS OLD-BORDERS)
                (SETQ OLD-BORDERS NIL))))))

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :ADJUST-MARGINS) ()
  (SEND SELF ':CHANGE-OF-SIZE-OR-MARGINS ':LEFT X-OFFSET
                                         ':TOP Y-OFFSET
                                         ':WIDTH WIDTH
                                         ':HEIGHT HEIGHT))

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :BEFORE :CHANGE-OF-SIZE-OR-MARGINS) (&REST OPTIONS
                                                                  &AUX (PLIST (LOCF OPTIONS)))
  (SHEET-FORCE-ACCESS (SELF)
    (ERASE-MARGINS))                            ;Insure that old margins get erased
  (AND (GET PLIST ':LEFT)
       (FULL-SCREEN-HACK (GET PLIST ':LEFT) (GET PLIST ':TOP) (GET PLIST ':WIDTH)
                         (GET PLIST ':HEIGHT))))


(DEFFLAVOR PROCESS-MIXIN ((PROCESS NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES PROCESS)
  (:INITABLE-INSTANCE-VARIABLES PROCESS)
  (:DOCUMENTATION :MIXIN "For a window which has its own process.
To enable this feature, specify the init keyword :PROCESS when you create the window.
The value should be either a process, a list or a symbol.

If it is a list, the cdr provides the keyword agrs to MAKE-PROCESS
and the car is then used as the top level function to run in the new process.
It will receive one arg when it is called: this window.

If PROCESS is a symbol, it is used as the top level function
and MAKE-PROCESS is called with no keyword arguments.
But, as an exception, if PROCESS is T, the top level function is
to send the window a :PROCESS-TOP-LEVEL message with no arguments.

The first time the window is exposed or selected, the process (if any)
receives the window as a run reason.
Each time the window is exposed or selected, if the process is flushed,
it gets reset and can run again."))

;;; This is explicit to ensure shadowing
(DEFMETHOD (PROCESS-MIXIN :PROCESS) () PROCESS)

(DEFMETHOD (PROCESS-MIXIN :AFTER :INIT) (IGNORE)
  (LET ((TEM PROCESS))
    (CTYPECASE PROCESS
      (NULL)
      (SYMBOL (SETQ PROCESS (MAKE-PROCESS NAME))
              (IF (EQ TEM T)
                  (if (send self :operation-handled-p :process-top-level)
                      (SEND PROCESS :PRESET SELF ':PROCESS-TOP-LEVEL)
                    (ferror nil "Process preset option to ~S was given as T, ~%~
                                 but there is no :PROCESS-TOP-LEVEL method defined for the window."
                            self))
                (SEND PROCESS :PRESET TEM SELF)))
      (CONS (SETQ PROCESS (APPLY 'MAKE-PROCESS NAME (CDR TEM)))
            (SEND PROCESS :PRESET (CAR TEM) SELF))
      (SI:PROCESS))))

(DEFUN MAYBE-RESET-PROCESS (MESSAGE &REST IGNORE)
  (DECLARE (:SELF-FLAVOR PROCESS-MIXIN))
  (COND ((OR (EQ MESSAGE ':SELECT)
             (LOOP FOR SUP = SUPERIOR THEN (SHEET-SUPERIOR SUP) UNTIL (NULL SUP)
                   ALWAYS (SHEET-EXPOSED-P SUP)))
         ;; Only touch the process if the window is going to become visible.  This
         ;; makes many of the processes in the initial cold-load not have run reasons
         ;; until you first select their window.  This makes booting faster (pages less).
         ;; Also this is necessary to make the editor work:
         ;; What was happening was that when the editor created its first
         ;; pane and exposed it within its deactivated frame, the editor's process was
         ;; being prematurely started up when it didn't even have all its instance
         ;; variables yet, never mind enough editor environment set up.  The editor
         ;; process would thus immediately get an error, which would later be reset
         ;; asynchronously, leaving a second-level error handler around forever.
         (COND ((TYPEP PROCESS 'SI:PROCESS)
                ;; If we really have a process (not just NIL or something),
                ;; Reset the process if it is flushed, then make sure it has a run reason.
                (IF (EQ (PROCESS-WAIT-FUNCTION PROCESS) 'SI:FLUSHED-PROCESS)
                    (SEND PROCESS ':RESET))
                (SEND PROCESS ':RUN-REASON SELF))))))

;;; *** This is a horrible crock.  If the "program system" is ever implemented,
;;; *** this should be flushed and replaced by the concept that selecting a program
;;; *** does something appropriate to its processes.
;;; I don't know if this is really the right thing
(DEFMETHOD (PROCESS-MIXIN :BEFORE :EXPOSE) MAYBE-RESET-PROCESS)
(DEFMETHOD (PROCESS-MIXIN :BEFORE :SELECT) MAYBE-RESET-PROCESS)

(DEFMETHOD (PROCESS-MIXIN :PROCESSES) ()
  (AND PROCESS (LIST PROCESS)))

(DEFFLAVOR LISTENER-MIXIN-INTERNAL (LISTENER-PACKAGE) (PROCESS-MIXIN FULL-SCREEN-HACK-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE "An actual LISP window
Includes a process that will run the lisp top level read-eval-print loop.
Use this rather than LISTENER-MIXIN when you want to be invisible to the SYSTEM L key."))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :PACKAGE) ()
  (IF (VARIABLE-BOUNDP LISTENER-PACKAGE) LISTENER-PACKAGE))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :SET-PACKAGE) (PKG)
  (SETQ LISTENER-PACKAGE (PKG-FIND-PACKAGE PKG)))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :BEFORE :INIT) (IGNORE)
  (OR PROCESS (SETQ PROCESS '(SI:LISP-TOP-LEVEL1 :REGULAR-PDL-SIZE 40000
                                                 :SPECIAL-PDL-SIZE 4000))))

;;; Listener mixin internal is invisible to system L, but Listener mixin is visible.
(DEFFLAVOR LISTENER-MIXIN () (LISTENER-MIXIN-INTERNAL)
  (:DOCUMENTATION :SPECIAL-PURPOSE "An actual LISP window
Includes a process that will run the lisp top level read-eval-print loop.
Use this when you want to be visible to the SYSTEM L key."))


(DEFFLAVOR LISP-INTERACTOR () (NOTIFICATION-MIXIN LISTENER-MIXIN-INTERNAL WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :COMBINATION "LISP window, but not LISP-LISTENER-P"))


(DEFFLAVOR LISP-LISTENER () (NOTIFICATION-MIXIN LISTENER-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :COMBINATION "Normal LISP window"))

(DEFMETHOD (LISP-LISTENER :LISP-LISTENER-P) ()
  (IF (SYMEVAL-IN-STACK-GROUP 'SI:LISP-TOP-LEVEL-INSIDE-EVAL (PROCESS-STACK-GROUP PROCESS))
      ':BUSY
    ':IDLE))

(DEFUN IDLE-LISP-LISTENER (&OPTIONAL (SUPERIOR DEFAULT-SCREEN)
                           &AUX LL (FULL-SCREEN (MULTIPLE-VALUE-LIST
                                                  (SEND SUPERIOR ':INSIDE-SIZE))))
  "Find a Lisp Listener that's not in use, and is the full size of the specified
superior.   Creates one if none is available."
  (SETQ LL (DOLIST (I (SHEET-INFERIORS SUPERIOR))
             (AND (EQ (SEND I ':LISP-LISTENER-P) ':IDLE)
                  (EQUAL FULL-SCREEN (MULTIPLE-VALUE-LIST (SEND I ':SIZE)))
                  (RETURN I))))
  (OR LL (MAKE-WINDOW 'LISP-LISTENER ':SUPERIOR SUPERIOR)))

(DEFFLAVOR POP-UP-TEXT-WINDOW () (TEMPORARY-WINDOW-MIXIN WINDOW)
  (:DOCUMENTATION :COMBINATION "A simple temporary window for stream type output
Useful for things like Terminal-F which just want a typeout stream that will not
disturb things underneath."))

(DEFMETHOD (POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  (KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE IO-BUFFER SELF))

(DEFFLAVOR TRUNCATING-POP-UP-TEXT-WINDOW () (TEMPORARY-WINDOW-MIXIN TRUNCATING-WINDOW)
  (:DOCUMENTATION :COMBINATION "A pop up window what truncates lines"))

(DEFMETHOD (TRUNCATING-POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  (KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE IO-BUFFER SELF))

(DEFFLAVOR RESET-ON-OUTPUT-HOLD-FLAG-MIXIN () ()
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:RESET-ON-OUTPUT-HOLD-FLAG)))

(DEFMETHOD (RESET-ON-OUTPUT-HOLD-FLAG-MIXIN :RESET-ON-OUTPUT-HOLD-FLAG) ()
  (SEND CURRENT-PROCESS ':RESET ':ALWAYS))

(DEFFLAVOR TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET ()
           (RESET-ON-OUTPUT-HOLD-FLAG-MIXIN TRUNCATING-POP-UP-TEXT-WINDOW))

;;; This mixin is useful for those windows that are created during the world-load.
;;; It is disconcerting when you suddenly see them appearing after you reshape
;;; some window.  This mixin causes them to be invisible and immune to autoexposure.
;;; They don't appear on the screen until you explicitly ask for them.  However, they
;;; are still active and appear on the Select menu.
(DEFFLAVOR INITIALLY-INVISIBLE-MIXIN () ()
  (:DEFAULT-INIT-PLIST :PRIORITY -2))

(DEFMETHOD (INITIALLY-INVISIBLE-MIXIN :BEFORE :EXPOSE) (&REST IGNORE)
  (SEND SELF ':SET-PRIORITY NIL))


;;;; Some notification stuff

(DEFFLAVOR NOTIFICATION-MIXIN () ()
  (:REQUIRED-METHODS :PROCESS)
  (:REQUIRED-FLAVORS STREAM-MIXIN ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Prints notifications on itself when selected.
A window which can easily accomodate unsolicited typeout, such as a Lisp listener,
uses this mixin to cause notifications to be printed on it when it is selected.
The user's attention is assumed to be at the cursor of the selected window.
This mixin also interacts with the rubout-handler of STREAM-MIXIN."))

;;; Note: this does not try to do anything smart with the prompt, because doing
;;; that right requires resolving some hairy issues which simply are not worth it.
(DEFMETHOD (NOTIFICATION-MIXIN :PRINT-NOTIFICATION) (TIME STRING WINDOW-OF-INTEREST)
  (IF (MULTIPLE-VALUE-BIND (NIL NIL FINAL-INDEX)
          (LET ((END (STRING-LENGTH STRING)))
            (AND (PLUSP END) (= (AREF STRING (1- END)) #/RETURN)
                 (DECF END))
            (SHEET-COMPUTE-MOTION SELF 0 0 STRING 0 END T
                                  0 (- (SHEET-INSIDE-HEIGHT) LINE-HEIGHT) 1.0s6))
        FINAL-INDEX)
      ;; We are too small to print this notification.  Use a pop-up-window.
      (POP-UP-NOTIFY :PRINT-NOTIFICATION TIME STRING WINDOW-OF-INTEREST)
    ;; We can print this notification on the seleted window, so do it.
    (SEND SELF :PRINT-NOTIFICATION-ON-SELF TIME STRING WINDOW-OF-INTEREST)))

;;; Execute the body and then redisplay any rubout handler input that is on the window
;;; below what was printed by the body.
(DEFMACRO OUTPUT-BEFORE-RUBOUT-HANDLER ((WINDOW) &BODY BODY)
  `(LET (PROCESS SG RUBOUT-X RUBOUT-Y RUBOUT-X-LOC RUBOUT-Y-LOC)
      (WITHOUT-INTERRUPTS
        (AND (SETQ PROCESS (SEND ,WINDOW :PROCESS))
             (SETQ SG (SEND PROCESS :STACK-GROUP))
             (SYMEVAL-IN-STACK-GROUP 'RUBOUT-HANDLER-INSIDE SG)
             (SETF (VALUES RUBOUT-X RUBOUT-X-LOC)
                     (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-X SG)
                   (VALUES RUBOUT-Y RUBOUT-Y-LOC)
                     (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-Y SG))))
      ;; If the process is in the rubout-handler, back up over the echoed input and erase it.
      (COND (RUBOUT-X (SEND ,WINDOW :SET-CURSORPOS RUBOUT-X RUBOUT-Y)
                      (SEND ,WINDOW :CLEAR-REST-OF-LINE)))
      (UNWIND-PROTECT
        (PROGN . ,BODY)
        ;; Reprint rubout-handler buffer if necessary, and change the rubout-handler's
        ;; starting cursorpos
        (WHEN RUBOUT-X
          (MULTIPLE-VALUE-BIND (NEW-X NEW-Y)
              (SEND ,WINDOW :READ-CURSORPOS)
            (SETF (CONTENTS RUBOUT-X-LOC) NEW-X
                  (CONTENTS RUBOUT-Y-LOC) NEW-Y)
            (IO-BUFFER-PUSH (SEND ,WINDOW :IO-BUFFER)
                            `(REDISPLAY-RUBOUT-HANDLER)))))))

;;; Some windows that use the usual test to see whether they are able to print
;;; put demons on this to do hairy things when they do print.
(DEFMETHOD (NOTIFICATION-MIXIN :PRINT-NOTIFICATION-ON-SELF) (TIME STRING WINDOW-OF-INTEREST)
  (DECLARE (IGNORE WINDOW-OF-INTEREST))
  (LOCK-SHEET (SELF)
    (OUTPUT-BEFORE-RUBOUT-HANDLER (SELF)
      (SEND SELF :FRESH-LINE)
      (SEND SELF :BEEP)
      (SEND SELF :TYO #/[)
      (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME SELF)
      (SEND SELF :TYO #/SP)
      (LET ((END (STRING-LENGTH STRING)))
        (OR (ZEROP END)
            (SEND SELF :STRING-OUT STRING 0
                       (IF (= (AREF STRING (1- END)) #/RETURN) (1- END) END))))
      (SEND SELF :TYO #/])
      (SEND SELF :TYO #/CR))))

(DEFFLAVOR POP-UP-NOTIFICATION-MIXIN () (DELAY-NOTIFICATION-MIXIN)
  :ALIAS-FLAVOR)

;This actually appears earlier in the file, to avoid lossage.
;(DEFFLAVOR DELAY-NOTIFICATION-MIXIN () ()
;  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
;  (:DOCUMENTATION :MIXIN "Delays printing notifications, but announces them in the who line.
;This is the default way of handling them.  See NOTIFICATION-MIXIN for an alternative."))

(DEFVAR PENDING-NOTIFICATIONS NIL
  "Notifications waiting for the user to switch windows or do Terminal N.
When this is non-NIL, who-line announces notifications.")
(DEFVAR DEFERRED-NOTIFICATIONS NIL
  "Like PENDING-NOTIFICATIONS, but these don't make who-line blink.")
(DEFCONST WAIT-FOR-NOTIFICATIONS-FLAG T
  "Non-NIL means wait for user to switch windows rather than pop up a notification window.")

(DEFMETHOD (DELAY-NOTIFICATION-MIXIN :PRINT-NOTIFICATION) POP-UP-NOTIFY)

(DEFUN POP-UP-NOTIFY (IGNORE TIME STRING WINDOW-OF-INTEREST)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-WINDOW))
  (IF WAIT-FOR-NOTIFICATIONS-FLAG
      (WAIT-TO-NOTIFY TIME STRING WINDOW-OF-INTEREST)
    ;; Now we must spawn a process and return.  See comments in CAREFUL-NOTIFY.
    (PROCESS-RUN-FUNCTION "Notify"
      #'(LAMBDA (TIME STRING WINDOW-OF-INTEREST SLF START-TIME NOTE-WINDOW)
          (SEND NOTE-WINDOW :SET-WINDOW-OF-INTEREST WINDOW-OF-INTEREST)
          ;;Above sets up for mouse click.  Caller has already set up for Terminal-0-S
          (SEND NOTE-WINDOW :SET-LABEL
                (FORMAT NIL "Notification: Type any character to flush."))
          ;; If window gets deexposed while we're typing out, typically because
          ;; user types Terminal-0-S before we finish cranking out our message, punt.
          (CATCH ':DEEXPOSE
            (CONDITION-BIND ((OUTPUT-ON-DEEXPOSED-SHEET
                               (LAMBDA (&REST IGNORE) (THROW ':DEEXPOSE NIL))))
              (LET* ((OSW SELECTED-WINDOW)      ;Almost certainly SLF
                     (MESSAGE-STRING
                       (WITH-OUTPUT-TO-STRING (STR)
                         (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME STR)
                         (TYO #/SPACE STR)
                         (PRINC STRING STR)
                         (TYO #/RETURN STR)))
                     (SCREEN (SHEET-GET-SCREEN SLF))
                     (PROMPT-STRING
                       (IF WINDOW-OF-INTEREST
                           (FORMAT NIL
"Select ~A by typing Terminal-0-S or by clicking the mouse here.
"
                                   WINDOW-OF-INTEREST)
                         ""))
                     (STRING-WE-NEED-ROOM-FOR (STRING-APPEND MESSAGE-STRING PROMPT-STRING)))
                (MULTIPLE-VALUE-BIND (NIL FINAL-Y NIL MAXIMUM-X)
                    (SHEET-COMPUTE-MOTION NOTE-WINDOW 0 0 STRING-WE-NEED-ROOM-FOR
                                          0 NIL T 0 1.0s6
                                          1.0s6
                                          ;; Don't let it be wider than can fit on screen.
                                          (- (SHEET-WIDTH SCREEN)
                                             (- (SHEET-WIDTH NOTE-WINDOW)
                                                (SHEET-INSIDE-WIDTH NOTE-WINDOW))))
                  (SEND NOTE-WINDOW :SET-INSIDE-SIZE
                        (MAX (SEND NOTE-WINDOW ':LABEL-SIZE) MAXIMUM-X)
                        (MIN (- (SHEET-HEIGHT SCREEN)
                                (- (SHEET-HEIGHT NOTE-WINDOW)
                                   (SHEET-INSIDE-HEIGHT NOTE-WINDOW)))
                             FINAL-Y)))
                (MULTIPLE-VALUE-BIND (X Y)
                    (SHEET-CALCULATE-OFFSETS SLF SCREEN)
                  (SEND NOTE-WINDOW :CENTER-AROUND
                                    (+ X (TRUNCATE (SHEET-WIDTH SLF) 2))
                                    (+ Y (TRUNCATE (SHEET-HEIGHT SLF) 2))))
                (SEND NOTE-WINDOW :SELECT)      ;Exposes blank with homed cursor
                (SEND NOTE-WINDOW :STRING-OUT MESSAGE-STRING)
                (FINISH-UNEXPECTED-SELECT START-TIME OSW)       ;By now user has seen what's up
                (SEND NOTE-WINDOW :CLEAR-INPUT) ;Flush typeahead before inviting typein
                (SEND NOTE-WINDOW :STRING-OUT PROMPT-STRING)
                (SEND NOTE-WINDOW :ANY-TYI))))
          (SEND NOTE-WINDOW ':DEACTIVATE))
      TIME STRING WINDOW-OF-INTEREST SELF
      (START-UNEXPECTED-SELECT)
      (ALLOCATE-RESOURCE 'POP-UP-NOTIFICATION-WINDOW (SHEET-GET-SCREEN SELF)))))

;;; Wait until selected window changes, then print this notification
;;; if it is then possible, or else wait again.
;;; While we wait, PENDING-NOTIFICATIONS is non-NIL
;;; so the mouse doc line says thaht notifications are waiting.
(DEFUN WAIT-TO-NOTIFY (TIME STRING WINDOW-OF-INTEREST)
  (PUSH (LIST TIME STRING WINDOW-OF-INTEREST) PENDING-NOTIFICATIONS)
  (BEEP 'NOTIFY)
  (LOOP
    (PROCESS-WAIT "Notification wait"
                  (LAMBDA (W)
                    (OR (AND (NULL PENDING-NOTIFICATIONS)
                             (NULL DEFERRED-NOTIFICATIONS))
                        (AND SELECTED-WINDOW
                             (NEQ SELECTED-WINDOW W))))
                  SELECTED-WINDOW)
    (WITHOUT-INTERRUPTS
      ;; Flush any notifications that just tell user
      ;; to go to the window he just selected.
      (DOLIST (NOTE PENDING-NOTIFICATIONS)
        (IF (and (third note)
                 (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW (THIRD NOTE))
                 (string= (second note) "Process " :end1 #.(length "Process "))
                 (or (string= (second note) "wants to type out"
                              :start1 (max (- (length (second note))
                                              #.(length "wants to type out"))
                                           0))
                     (string= (second note) "wants typein"
                              :start1 (max (- (length (second note)) #.(length "wants typein"))
                                           0))))
            (SETQ PENDING-NOTIFICATIONS
                  (DELQ NOTE PENDING-NOTIFICATIONS))))
      (DOLIST (NOTE DEFERRED-NOTIFICATIONS)
        (IF (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW (THIRD NOTE))
            (SETQ DEFERRED-NOTIFICATIONS
                  (DELQ NOTE DEFERRED-NOTIFICATIONS)))))
    ;; If any notifications left to print, print them
    ;; if new selected window likes to print them.
    (IF (OR PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS)
        (LET ((SW SELECTED-WINDOW))
          (WHEN (TYPEP SW 'NOTIFICATION-MIXIN)
            (SETQ PENDING-NOTIFICATIONS
                  (APPEND PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS))
            (SETQ DEFERRED-NOTIFICATIONS NIL)
            (DO ()
                ((NULL PENDING-NOTIFICATIONS) (RETURN NIL))
              (LEXPR-SEND SW :PRINT-NOTIFICATION (POP PENDING-NOTIFICATIONS))))))
    ;; If could not print them, wait for another window-switch.
    (OR PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS
        (RETURN NIL))))

;;; These two functions are for unexpected pop-up selectable windows
;;; They give the user a chance to get his typing straightened out

(DEFVAR UNEXPECTED-SELECT-DELAY 180.)   ;Give user 3 seconds to notice beep and stop typing

;;; Beep, return time to be passed back in to FINISH-UNEXPECTED-SELECT
(DEFUN START-UNEXPECTED-SELECT ()
  (BEEP 'NOTIFY)
  (TIME))

;;; Sleep until enough time has passed, then snarf typeahead into old-selected-window
;;;  which is no longer selected-window because by now the new thing has been exposed
(DEFUN FINISH-UNEXPECTED-SELECT (START-TIME OLD-SELECTED-WINDOW &AUX BUF)
  (PROCESS-WAIT "Sleep" #'(LAMBDA (START-TIME) (> (TIME-DIFFERENCE (TIME) START-TIME)
                                                  UNEXPECTED-SELECT-DELAY))
                        START-TIME)
  (WITHOUT-INTERRUPTS
    (AND OLD-SELECTED-WINDOW
         (SETQ BUF (SEND OLD-SELECTED-WINDOW ':IO-BUFFER))
         (KBD-SNARF-INPUT BUF))))

(DEFFLAVOR POP-UP-NOTIFICATION-WINDOW
        ((WINDOW-OF-INTEREST NIL))
        (NOTIFICATION-MIXIN POP-UP-TEXT-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES WINDOW-OF-INTEREST)
  (:DEFAULT-INIT-PLIST :SAVE-BITS NIL  ;Thus will not come up with old garbage contents
                       :CHARACTER-HEIGHT 5      ;5 lines.  Width is full width of sup.
                       :DEEXPOSED-TYPEOUT-ACTION ':ERROR)
  (:DOCUMENTATION :SPECIAL-PURPOSE "Pops down and selects window of interest when clicked on
One of these is created when a notify message is sent to a normal window, it pops up, prints
the notification, and when it is selected with the mouse, pops back down and exposes the
window that got the error, which for background processes will be a slightly larger
pop-up type window."))

;;; Record that this notification window is being used to notify about
;;; the window of interest.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :SET-WINDOW-OF-INTEREST) (WINDOW)
  (SETQ WINDOW-OF-INTEREST WINDOW)
  (LET ((TEM (ASSQ WINDOW BACKGROUND-INTERESTING-WINDOWS)))
    (AND TEM (SETF (CDR TEM) SELF))))

;;; When clicked on, always send a :MOUSE-SELECT message, even if already selected
;;; so that WINDOW-OF-INTEREST will get selected.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :MOUSE-CLICK) (BUTTON IGNORE IGNORE)
  (COND ((= BUTTON #/MOUSE-1-1)
         (MOUSE-SELECT SELF)
         T)))

(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :MOUSE-SELECT) (&REST ARGS)
  "If selected with the mouse, then deexpose us and really select the guy that we are
notifying about."
  (SEND SELF ':DEEXPOSE)                        ;This will also deactivate us
  (AND WINDOW-OF-INTEREST
       (LEXPR-SEND WINDOW-OF-INTEREST ':MOUSE-SELECT ARGS)))

;;; This wakes up the process which is sitting around waiting for the user
;;; to type something to flush the notification window.  It will deactivate us.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :AFTER :DEEXPOSE) (&REST IGNORE)
  (SEND SELF ':FORCE-KBD-INPUT ':DEEXPOSE))

;;; Resource to supply reasonably sized bit arrays.  This is especially useful
;;; for window-bind type windows that don't want to go through the overhead of
;;; creating a new bit array every time they get invoked
(DEFRESOURCE BIT-ARRAYS (&OPTIONAL (WIDTH (SHEET-WIDTH DEFAULT-SCREEN))
                                   (HEIGHT (SHEET-HEIGHT DEFAULT-SCREEN)))
  :CONSTRUCTOR (MAKE-ARRAY (LIST WIDTH HEIGHT) ':TYPE 'ART-1B)
  :INITIAL-COPIES 0)

(DEFUN AWAIT-WINDOW-EXPOSURE ()
  "Wait until *TERMINAL-IO* is exposed (if it is a window).
To be called by functions like ED.
If you want to await the re-exposure of the Lisp listener after activating
some other window, call this.  Usually it does nothing, but if the *TERMINAL-IO*
window is an auto-exposing window, if you didn't call this you would get into
a loop where two windows were fighting for exposure, each de-exposing the other.
If that would happen this function causes a wait until *TERMINAL-IO* is exposed."
  (SEND *TERMINAL-IO* ':SEND-IF-HANDLES ':AWAIT-EXPOSURE)
  T)

(DEFMETHOD (ESSENTIAL-WINDOW :AWAIT-EXPOSURE) ()
  (OR (EQ DEEXPOSED-TYPEOUT-ACTION ':NORMAL)
      (PROCESS-WAIT "Await exposure" #'CAR (LOCF EXPOSED-P))))

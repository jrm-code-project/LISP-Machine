;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Typeout window and mouse-sensitive items

;;; Menu type item typeout window
(DEFFLAVOR BASIC-MOUSE-SENSITIVE-ITEMS
        ((ITEM-TYPE-ALIST NIL)                  ;Associates actions with types of items
         (ITEM-LIST NIL)                        ;All the currently exposed items
         ITEM-BLINKER                           ;Highlights mousable items
         MENU)                                  ;For when item clicked on with right button
        ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE STREAM-MIXIN)
  (:SETTABLE-INSTANCE-VARIABLES ITEM-TYPE-ALIST)
  (:DOCUMENTATION :MIXIN "Menu like operations for a typeout window"))

;;; Item typed out by :ITEM or :ITEM-LIST messages
(DEFSTRUCT (TYPEOUT-ITEM LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  TYPEOUT-ITEM-TYPE                             ;For looking in ITEM-TYPE-ALIST
  TYPEOUT-ITEM-ITEM                             ;Identifier of item
  TYPEOUT-ITEM-LEFT                             ;Screen area occupied by item, relative to
  TYPEOUT-ITEM-TOP                              ;sheet, not to margins
  TYPEOUT-ITEM-RIGHT
  TYPEOUT-ITEM-BOTTOM)

;;; Make a blinker for the menu type items and the pop-up menu
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :INIT) (IGNORE)
  (SETQ ITEM-BLINKER (MAKE-BLINKER SELF 'HOLLOW-RECTANGULAR-BLINKER :VISIBILITY NIL)
        MENU (MAKE-WINDOW 'MOMENTARY-MENU :SUPERIOR SELF)))

(DEFUN TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MOUSE-SENSITIVE-ITEMS))
  (SETQ ITEM-LIST NIL)
  (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))

;;; Forget any items on screen if cleared
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P (TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-WINDOW)
    TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-SCREEN)
    TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-REST-OF-WINDOW) (&AUX TEM)
  (COND ((SETQ TEM (MEMQ 'WRAPAROUND ITEM-LIST))
         (RPLACD TEM NIL)
         (MOUSE-WAKEUP))))
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-EOF) (&AUX TEM)
  (COND ((SETQ TEM (MEMQ 'WRAPAROUND ITEM-LIST))
         (RPLACD TEM NIL)
         (MOUSE-WAKEUP))))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :EXPOSE-FOR-TYPEOUT)
    TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)

;;; Record a blip when the screen wraps around
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :END-OF-PAGE-EXCEPTION) ()
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH 'WRAPAROUND ITEM-LIST)))

;;; Type out item, either as itself or FORMAT-ARGS.  TYPE is used for indexing into
;;; ITEM-TYPE-ALIST
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM) (TYPE ITEM &REST FORMAT-ARGS)
  ;; Wrap around, if necessary, before recording the cursor.
  (SEND SELF :INCREMENT-CURSORPOS 0 0)
  (LET ((X CURSOR-X) (Y CURSOR-Y))
    (IF FORMAT-ARGS (APPLY 'FORMAT SELF FORMAT-ARGS) (PRINC ITEM SELF))
    (DO ((LINE-Y Y (+ LINE-Y LINE-HEIGHT))
         (LINE-X X LEFT-MARGIN-SIZE))
        (NIL)
      (IF (> (+ LINE-Y LINE-HEIGHT)
             (SHEET-INSIDE-BOTTOM))
          (SETQ LINE-Y TOP-MARGIN-SIZE))
      (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
        (PUSH (LIST TYPE ITEM LINE-X LINE-Y
                    (IF (= LINE-Y CURSOR-Y) CURSOR-X (SHEET-INSIDE-WIDTH))
                    (+ LINE-Y LINE-HEIGHT))
              ITEM-LIST))
      (IF (= LINE-Y CURSOR-Y) (RETURN)))
    (TV:MOUSE-WAKEUP)
    NIL))

;;; Make an item without drawing anything (assuming the caller has drawn it already)
;;; Instead you just pass in an enclosing rectangle
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM) (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM (+ LEFT (SHEET-INSIDE-LEFT)) (+ TOP (SHEET-INSIDE-TOP))
                (+ RIGHT (SHEET-INSIDE-LEFT)) (+ BOTTOM (SHEET-INSIDE-TOP)))
          ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

;;; Like :PRIMITIVE-ITEM except that the edges are wrt the outside of the window.
;;; so you can use values such as CURSOR-X without subtracting (SHEET-INSIDE-TOP).
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM-OUTSIDE)
           (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM LEFT TOP RIGHT BOTTOM)
          ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

;;; Type out list of item as many as will fit on each line, centered.
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM-LIST) (TYPE LIST &AUX (MAXL 0) N
                                                  (INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
  (SEND SELF :FRESH-LINE)
  (WHEN LIST                                    ;Do nothing if empty list
    ;; Compute the maximum width of any item, in dots (MAXL).
    (DOLIST (ITEM LIST)
      (LET ((STRING (STRING (IF (CONSP ITEM) (CAR ITEM) ITEM))))
        (SETQ MAXL (MAX (SHEET-STRING-LENGTH SELF STRING) MAXL))))
    ;; How many items go on each line (except the last)?
    (SETQ N (MAX (MIN (TRUNCATE INSIDE-WIDTH (+ MAXL (FONT-CHAR-WIDTH CURRENT-FONT)))
                      (LENGTH LIST))
                 1))                            ;Always print something, even if continuation
    ;; Now print the items and store the data in the table.
    ;; Move to a new line when we exhaust a line, and at the end.
    ;; I counts from 1 thru N on each line.
    (DO ((I 1 (1+ I))
         (LIST LIST (CDR LIST))
         (WIDTH-PER (TRUNCATE INSIDE-WIDTH N)))
        ((NULL LIST))
      ;; Actually make this item.
      (IF (CONSP (CAR LIST))
          (SEND SELF :ITEM TYPE (CDAR LIST) "~A" (CAAR LIST))
        (SEND SELF :ITEM TYPE (CAR LIST)))
      ;; Space out for next item, or move to new line.
      (IF (AND ( I N) (CDR LIST))
          ;; Not end of line, space out for next item.
          (MULTIPLE-VALUE-BIND (X Y)
              (SHEET-READ-CURSORPOS SELF)
            (SHEET-SET-CURSORPOS SELF
                                 (* WIDTH-PER
                                    (TRUNCATE (+ (1- WIDTH-PER) X)
                                              WIDTH-PER))
                                 Y))
        ;; End of line.
        (SEND SELF :TYO #/RETURN)
        (SETQ I 0))))
  (TV:MOUSE-WAKEUP)
  NIL)

;;; When mouse leaves the window, turn off the item blinker
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :HANDLE-MOUSE) ()
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL))

;;; Blink any item the mouse points to
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-MOVES) (X Y &AUX ITEM)
  (MOUSE-SET-BLINKER-CURSORPOS)
  (COND ((AND (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
              (ASSQ (TYPEOUT-ITEM-TYPE ITEM) ITEM-TYPE-ALIST))
         (LET ((LEFT (TYPEOUT-ITEM-LEFT ITEM))
               (TOP (TYPEOUT-ITEM-TOP ITEM))
               (RIGHT (TYPEOUT-ITEM-RIGHT ITEM))
               (BOTTOM (TYPEOUT-ITEM-BOTTOM ITEM))
               BWIDTH BHEIGHT)
           (SETQ BWIDTH (- RIGHT LEFT)
                 BHEIGHT (- BOTTOM TOP))
           (BLINKER-SET-CURSORPOS ITEM-BLINKER (- LEFT (SHEET-INSIDE-LEFT))
                                  (- TOP (SHEET-INSIDE-TOP)))
           (BLINKER-SET-SIZE ITEM-BLINKER BWIDTH BHEIGHT)
           (BLINKER-SET-VISIBILITY ITEM-BLINKER T)))
        (T (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))))

;;; Mouse-left selects the blinking item, mouse-right pops up a menu near it
(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-CLICK) (BUTTON X Y &AUX ITEM)
  (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (OR (WHEN ITEM
        (LET ((ITEM-TYPE (ASSQ (TYPEOUT-ITEM-TYPE ITEM) ITEM-TYPE-ALIST)))
          (WHEN ITEM-TYPE
            (COND
              ((EQ BUTTON #/MOUSE-1-1)
               (SEND SELF :FORCE-KBD-INPUT
                     (LIST ':TYPEOUT-EXECUTE (CADR ITEM-TYPE)
                           (TYPEOUT-ITEM-ITEM ITEM)))
               T)
              ((AND (EQ BUTTON #/MOUSE-3-1)
                    (CDDDR ITEM-TYPE))
               (PROCESS-RUN-FUNCTION "Menu Choose" #'TYPEOUT-MENU-CHOOSE
                                     MENU (CDDDR ITEM-TYPE) ITEM SELF
                                     ;; Compute a label for the menu.
                                     (OR (AND (CONSP (THIRD ITEM-TYPE))
                                              (CADR (THIRD ITEM-TYPE))
                                              (FUNCALL (CADR (THIRD ITEM-TYPE))
                                                       ITEM))
                                         (AND (TYPEP (SECOND ITEM) 'INSTANCE)
                                              (OR (SEND (SECOND ITEM) :SEND-IF-HANDLES
                                                        :STRING-FOR-PRINTING)
                                                  (SEND (SECOND ITEM) :SEND-IF-HANDLES
                                                        :NAME)))))
               T)
              (T (BEEP))))))
      ;; Return T unless this is double-right, to inhibit the blip made by default.
      (NEQ BUTTON #/MOUSE-R-2)))

;;; Return the item the mouse if pointing to
(DEFUN TYPEOUT-MOUSE-ITEM (X Y)
  "Return the mouse-sensitive item at X, Y, or NIL if none.
SELF should be a BASIC-MOUSE-SENSITIVE-ITEMS instance."
  (DECLARE (:SELF-FLAVOR BASIC-MOUSE-SENSITIVE-ITEMS))
  (DO ((ITEMS ITEM-LIST (CDR ITEMS))
       (ITEM) (ITEM-Y) (WRAPPED-AROUND))
      ((NULL ITEMS))
    (IF (SYMBOLP (SETQ ITEM (CAR ITEMS)))
        (SETQ WRAPPED-AROUND T)
        (AND ( (SETQ ITEM-Y (TYPEOUT-ITEM-TOP ITEM)) CURSOR-Y) WRAPPED-AROUND
             (RETURN NIL))
        (AND ( Y ITEM-Y)
             (< Y (TYPEOUT-ITEM-BOTTOM ITEM))
             ( X (TYPEOUT-ITEM-LEFT ITEM))
             (< X (TYPEOUT-ITEM-RIGHT ITEM))
             (RETURN ITEM)))))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-SENSITIVE-ITEM) (X Y)
  (TYPEOUT-MOUSE-ITEM X Y))

;(DEFFLAVOR TYPEOUT-ITEM-TEST-WINDOW () (BASIC-MOUSE-SENSITIVE-ITEMS WINDOW)
;  (:DOCUMENTATION :COMBINATION))

;;; Select thing to do with selected item from menu
(DEFUN TYPEOUT-MENU-CHOOSE (MENU ALIST TYPEOUT-ITEM TYPEOUT-WINDOW MENU-LABEL)
  "Select a thing to do to mouse-sensitive item TYPEOUT-ITEM.
MENU is the window used to ask for the choice,
and ALIST is the menu item-list to be displayed in MENU.
MENU-LABEL is a string to display as the menu's label, or NIL for no label.
TYPEOUT-WINDOW is the typeout window in which TYPEOUT-ITEM appeared.
The user's choice is processed by forcing input
of the same sort as is done by clicking left on the typeout-item,
except that the operation which the user chose in the menu
is supplied as the second elementof the blip, which looks like
/(:TYPEOUT-EXECUTE operation item-information)."
  (SEND MENU :SET-ITEM-LIST ALIST)
  (SEND MENU :SET-LABEL MENU-LABEL)
  (MOVE-WINDOW-NEAR-RECTANGLE MENU
                              (TYPEOUT-ITEM-LEFT TYPEOUT-ITEM)
                              (TYPEOUT-ITEM-TOP TYPEOUT-ITEM)
                              (TYPEOUT-ITEM-RIGHT TYPEOUT-ITEM)
                              (TYPEOUT-ITEM-BOTTOM TYPEOUT-ITEM))
  (LET ((CHOICE-RESULT (SEND MENU :CHOOSE)))
    (AND CHOICE-RESULT
         (SEND TYPEOUT-WINDOW :FORCE-KBD-INPUT
               (LIST ':TYPEOUT-EXECUTE CHOICE-RESULT (TYPEOUT-ITEM-ITEM TYPEOUT-ITEM))))))

;;; Useful for adding new types in various places
(DEFMACRO ADD-TYPEOUT-ITEM-TYPE (ALIST TYPE NAME-STRING FUNCTION &OPTIONAL DEFAULT-P DOCUMENTATION font)
  "Add a new operation named NAME-STRING for typeout items of type TYPE.
ALIST is a variable used as an ITEM-TYPE-ALIST in windows
that items of type TYPE will be output to.
FUNCTION is the function to be called when the user selects this operation.
DEFAULT-P non-NIL says make this operation the default one
 for items of this type (the operation that click-left does).
 Otherwise, it just becomes one of the items in the click-right menu."
  `(SETQ ,ALIST (ADD-TYPEOUT-ITEM-TYPE-1 ,ALIST ',TYPE ',FUNCTION ,NAME-STRING ,DEFAULT-P
                                         ,DOCUMENTATION ,font)))

(DEFUN ADD-TYPEOUT-ITEM-TYPE-1 (ALIST TYPE FUNCTION NAME DEFAULT-P DOCUMENTATION
                                &optional (font fonts:cptfont) &AUX EL1 EL2)
  (OR (SETQ EL1 (ASSQ TYPE ALIST))
      (PUSH (SETQ EL1 (LIST TYPE NIL NIL)) ALIST))
  (AND DEFAULT-P (SETF (SECOND EL1) FUNCTION))
  (OR (SETQ EL2 (ASS 'EQUALP NAME (CDDDR EL1)))
      (PUSH (SETQ EL2 (NCONS NAME)) (CDDDR EL1)))
  (SETF (CDR EL2) `(:VALUE ,FUNCTION :DOCUMENTATION ,DOCUMENTATION :FONT ,font))
  (SETF (THIRD EL1) (MAKE-TYPEOUT-MOUSE-PROMPT (THIRD EL1) (SECOND EL1)
                                               (CDDDR EL1)))
  ALIST)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :WHO-LINE-DOCUMENTATION-STRING) (&AUX ITEM ITEM-TYPE)
  (MULTIPLE-VALUE-BIND (X Y)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
    (SETQ X (- MOUSE-X X)
          Y (- MOUSE-Y Y))
    (AND (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
         (SETQ ITEM-TYPE (TYPEOUT-ITEM-TYPE ITEM))
         (SETQ ITEM-TYPE (ASSQ ITEM-TYPE ITEM-TYPE-ALIST))
         (COND ((STRINGP (THIRD ITEM-TYPE)) (THIRD ITEM-TYPE))
               ((CONSP (THIRD ITEM-TYPE))
                (FUNCALL (CAR (THIRD ITEM-TYPE)) ITEM))))))

(DEFUN MAKE-TYPEOUT-MOUSE-PROMPT (STRING DEFAULT ALIST)
  (IF STRING
      (SETF (FILL-POINTER STRING) 0)
    (SETQ STRING (MAKE-STRING 100. :FILL-POINTER 0)))
  (DO ((L ALIST (CDR L)))
      ((NULL L))
    (AND (EQ DEFAULT (GET (CAR L) ':VALUE))
         (SETQ DEFAULT (OR (GET (CAR L) ':DOCUMENTATION)
                           (CAAR L)))))
  (FORMAT STRING "Left: ~A  Right: menu of " DEFAULT)
  (DO ((L ALIST (CDR L))
       (FIRST-P T NIL))
      ((NULL L)
       (VECTOR-PUSH #/. STRING))
    (IF FIRST-P
        (SETQ FIRST-P NIL)
      (FORMAT STRING ", "))
    (FORMAT STRING "~A" (CAAR L)))
  STRING)

;;; Windows with typeout windows as inferiors
(DEFFLAVOR ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN ((TYPEOUT-WINDOW NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:GETTABLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:DOCUMENTATION :MIXIN "A window that has a typeout window as an inferior"))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :INIT) (IGNORE)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW (APPLY 'MAKE-WINDOW (CAR TYPEOUT-WINDOW)
                                                :SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))

(DEFWRAPPER (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  ;`(WITH-SHEET-DEEXPOSED (TYPEOUT-WINDOW) . ,BODY)
  `(LET (.STATUS. .INCOMPLETE-P.)
     (DELAYING-SCREEN-MANAGEMENT
       (UNWIND-PROTECT
         (PROGN
           (WHEN TYPEOUT-WINDOW                 ;May not be present during init
             (SETQ .STATUS. (SEND TYPEOUT-WINDOW :STATUS))
             (SETQ .INCOMPLETE-P. (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW)))
           . ,BODY)
         (WHEN .STATUS.
           (SEND TYPEOUT-WINDOW :SET-STATUS .STATUS.)
           (SETF (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW) .INCOMPLETE-P.))))))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
                                                                                (&REST IGNORE)
  (AND TYPEOUT-WINDOW
       (SEND TYPEOUT-WINDOW :SET-EDGES (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)
                                        (SHEET-INSIDE-RIGHT) (SHEET-INSIDE-BOTTOM))))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT)
           (OLD-FONT NEW-FONT)
  (AND TYPEOUT-WINDOW (SEND TYPEOUT-WINDOW :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :TURN-OFF-BLINKERS-FOR-TYPEOUT) ())

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :TURN-ON-BLINKERS-FOR-TYPEOUT) ())

;(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :SELECT) (&REST ARGS)
;  (AND (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW)
;       (LEXPR-SEND TYPEOUT-WINDOW :SELECT ARGS)))

(DEFFLAVOR WINDOW-WITH-TYPEOUT-MIXIN
        ()
        (NO-SCREEN-MANAGING-MIXIN ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN))

(DEFFLAVOR INTRINSIC-NO-MORE-MIXIN ((MORE-ENABLED-FOR-INFERIORS T)) ()
  (:DEFAULT-INIT-PLIST :MORE-P NIL)
  (:DOCUMENTATION :MIXIN "Suppress **more** for this window, but remember :more-p for inferiors."))

(DEFMETHOD (INTRINSIC-NO-MORE-MIXIN :MORE-P) () MORE-ENABLED-FOR-INFERIORS)

(DEFMETHOD (INTRINSIC-NO-MORE-MIXIN :SET-MORE-P) (MORE-P)
  (SETQ MORE-ENABLED-FOR-INFERIORS MORE-P))

(DEFCONST *ENABLE-TYPEOUT-WINDOW-BORDERS* T
  "Non-NIL enables the line below the occupied part of a typeout window to be drawn.")

;;; Typeout windows themselves
;;;  BOTTOM-REACHED is set to the largest Y clobbered (outside coords),
;;;   or NIL if nothing is clobbered.
;;;  INCOMPLETE-P is set to T when the window is exposed, and NIL when it is deexposed
;;;   or by the :MAKE-COMPLETE method.
;;;  For ordinary use, the command process of the program should check INCOMPLETE-P and wait
;;;    for the user to type space if that is set; the redisplay process should check
;;;    BOTTOM-REACHED and redisplay (only that portion, if it can) if that is set.
;;;   Thus things that typeout but that need not be saved for the user (like Y-OR-N-P's)
;;;    should send the :MAKE-COMPLETE message.
;;;  WINDOW-SUBSTITUTING-FOR is the window we are selected as a substitute for.
;;;   This is not necessarily the immediate superior.
;;;   It is whichever ancestor was selected when we started to be used.
;;;  PREVIOUS-SUBSTITUTE is what that window's selection substitute was
;;;   before we set its substitute to be us.
;;;  By default, these windows cannot be selected from the system menu.
(DEFFLAVOR BASIC-TYPEOUT-WINDOW
           ((BOTTOM-REACHED NIL) (HAD-MOUSE-P NIL) (INCOMPLETE-P NIL)
            (WINDOW-SUBSTITUTING-FOR NIL)
            (PREVIOUS-SUBSTITUTE NIL)
            IO-BUFFER)
           (NO-SCREEN-MANAGING-MIXIN DONT-SELECT-WITH-MOUSE-MIXIN)
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:GETTABLE-INSTANCE-VARIABLES INCOMPLETE-P)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:EXPOSE-FOR-TYPEOUT))
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES INCOMPLETE-P BOTTOM-REACHED)
  (:DOCUMENTATION :MIXIN "A window that grows over its superior"))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP IO-BUFFER)
          (SETQ IO-BUFFER (SEND SUPERIOR :IO-BUFFER))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :HANDLE-MOUSE) ()
  (SETQ HAD-MOUSE-P T))

;;; For MOUSE-MOVES and MOUSE-BUTTONS message, the typeout-window, if exposed, will
;;; receive the messages and if it is not in the covered area, pass them off to the
;;; superior and throw out of the original message.
(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :MOUSE-MOVES) ((IGNORE IGNORE) . BODY)
  `(CATCH 'SUPERIOR-HANDLED-MOUSE
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :MOUSE-MOVES) (X Y)
  (IF (HANDLE-MOUSE-P X Y)
      (COND ((NOT HAD-MOUSE-P)
             (SEND SUPERIOR :send-if-handles :TURN-OFF-BLINKERS-FOR-TYPEOUT)
             (SETQ HAD-MOUSE-P T)))
    (COND (HAD-MOUSE-P                          ;Send one extra message the first time out
           (SEND SUPERIOR :send-if-handles :TURN-ON-BLINKERS-FOR-TYPEOUT)
           (SETQ HAD-MOUSE-P NIL))              ;to turn off any blinkers
          (T
           (LET ((X (+ X X-OFFSET)) (Y (+ Y Y-OFFSET)))
             (SEND SUPERIOR :MOUSE-MOVES X Y)
             (THROW 'SUPERIOR-HANDLED-MOUSE T))))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MOUSE-MOVES) MOUSE-SET-BLINKER-CURSORPOS)

(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :MOUSE-BUTTONS) (IGNORE . BODY)
  `(CATCH 'SUPERIOR-HANDLED-MOUSE
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :MOUSE-BUTTONS) (BD X Y)
  (OR (HANDLE-MOUSE-P X Y)
      (LET ((X (+ X X-OFFSET)) (Y (+ Y Y-OFFSET)))
        (SEND SUPERIOR :MOUSE-BUTTONS BD X Y)
        (THROW 'SUPERIOR-HANDLED-MOUSE T))))

(DEFUN HANDLE-MOUSE-P (X Y)
  "T if position X, Y is inside the part of SELF that has been output in.
SELF must be a BASIC-TYPEOUT-WINDOW.
This is how clicks in the part where output has reached
apply to the typeout in the typeout window,
whereas clicks in the bottom part apply to the typeout window's superior."
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED (< Y (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y)))
       ( X 0) ( Y 0) (< X WIDTH)))

(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :WHO-LINE-DOCUMENTATION-STRING) (IGNORE . BODY)
  `(MULTIPLE-VALUE-BIND (XOFF YOFF)
       (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
     (IF (NULL (HANDLE-MOUSE-P (- MOUSE-X XOFF) (- MOUSE-Y YOFF)))
         (SEND SUPERIOR :WHO-LINE-DOCUMENTATION-STRING)
         . ,BODY)))

;;; We do **MORE** iff our superior does.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MORE-P) ()
  (SEND SUPERIOR :MORE-P))

;;; Telling us whether to do **MORE** is like telling our superior.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SET-MORE-P) (MORE-P)
  (SEND SUPERIOR :SET-MORE-P MORE-P))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :DEEXPOSED-TYPEIN-ACTION) ()
  (SEND SUPERIOR :DEEXPOSED-TYPEIN-ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :SET-DEEXPOSED-TYPEIN-ACTION) (ACTION)
  (SEND SUPERIOR :SET-DEEXPOSED-TYPEIN-ACTION ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :DEEXPOSED-TYPEOUT-ACTION) ()
  (SEND SUPERIOR :DEEXPOSED-TYPEOUT-ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION) (ACTION)
  (SEND SUPERIOR :SET-DEEXPOSED-TYPEOUT-ACTION ACTION))

;;; This wrapper is still present in 98, though it no longer has an effect.
;(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :EXPOSE) (IGNORE . BODY)
;  `(LET ((.TYPEOUT-WAS-EXPOSABLE. (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))
;        (.OLD-INCOMPLETE-P. INCOMPLETE-P))
;     (DECLARE (SPECIAL .TYPEOUT-WAS-EXPOSABLE. .OLD-INCOMPLETE-P.))
;     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (SETQ MORE-VPOS (AND (SEND SUPERIOR :MORE-P) (SHEET-DEDUCE-MORE-VPOS SELF))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MAKE-INCOMPLETE) ()
  (SETQ INCOMPLETE-P T))

;;; The fundamental output operations make the window incomplete
;;; and also may require redrawing the line that goes underneath
;;; the occupied area.

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :TYO) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :WRITE-CHAR) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :TERPRI) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :STRING-OUT) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :LINE-OUT) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :FRESH-LINE) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :CLEAR-REST-OF-LINE) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :CLEAR-EOL) MAYBE-MOVE-BOTTOM-REACHED)

(DEFUN MAYBE-MOVE-BOTTOM-REACHED (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (SETQ INCOMPLETE-P T)
  (AND BOTTOM-REACHED
       (LET ((OLD-BOTTOM BOTTOM-REACHED))
         (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y)))
         (UNLESS (= OLD-BOTTOM BOTTOM-REACHED)
           ;; Don't bother to undraw the line where it used to be,
           ;; because it was erased by clearing the line when the cursor moved down
           ;; if that was due to outputting text.
           (OR (ZEROP BOTTOM-REACHED)
               (= BOTTOM-REACHED (SHEET-INSIDE-BOTTOM))
               (AND *ENABLE-TYPEOUT-WINDOW-BORDERS*
                    (PREPARE-SHEET (SELF)
                      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
                                       BOTTOM-REACHED
                                       ALU-IOR SELF))))))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SET-CURSORPOS) COMPUTE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :INCREMENT-CURSORPOS) COMPUTE-BOTTOM-REACHED)

(DEFUN COMPUTE-BOTTOM-REACHED (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED
       (LET ((NEW-BOTTOM (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y))))
         (UNLESS (= NEW-BOTTOM BOTTOM-REACHED)
           (UNLESS (ZEROP BOTTOM-REACHED)
             (PREPARE-SHEET (SELF)
               (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
                                BOTTOM-REACHED
                                ALU-ANDCA SELF)))
           (OR (ZEROP NEW-BOTTOM)
               (= NEW-BOTTOM (SHEET-INSIDE-BOTTOM))
               (AND *ENABLE-TYPEOUT-WINDOW-BORDERS*
                    (PREPARE-SHEET (SELF)
                      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
                                       NEW-BOTTOM
                                       ALU-IOR SELF)))))
         (SETQ BOTTOM-REACHED NEW-BOTTOM))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BOTTOM-REACHED) ()
  (AND BOTTOM-REACHED (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P (REACHED-BOTTOM)))

(DEFUN REACHED-BOTTOM (&REST IGNORE)
  "Say that typeout on the typeout window has occupied it all, down to its bottom."
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED
       (NOT (ZEROP BOTTOM-REACHED))
       ( BOTTOM-REACHED (SHEET-INSIDE-BOTTOM))
       (PREPARE-SHEET (SELF)
         (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
                          BOTTOM-REACHED
                          ALU-ANDCA SELF)))
  (SETQ INCOMPLETE-P T
        BOTTOM-REACHED (SHEET-INSIDE-BOTTOM)))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-WINDOW) REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-SCREEN) REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-REST-OF-WINDOW) REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-EOF) REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :END-OF-PAGE-EXCEPTION) REACHED-BOTTOM)

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :RUBOUT-HANDLER) (&REST IGNORE)
  (IF (OR (NOT BOTTOM-REACHED) (EQ BOTTOM-REACHED 0))
      (SEND SELF :CLEAR-REST-OF-LINE)))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MAKE-COMPLETE) ()
  (SETQ INCOMPLETE-P NIL))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :ACTIVE-P) ()
  BOTTOM-REACHED)

;;; Expose, but don't clear the screen
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :EXPOSE-FOR-TYPEOUT) ()
  ;; This is here so that we don't try to activate ourselves while we are locked,
  ;; so that we don't violate locking order, because activating requires getting
  ;; a lock on our superior
  (SEND SELF :ACTIVATE)
  (SEND SELF :EXPOSE NIL :NOOP)
  (OR EXPOSED-P
      ;; If our superior has no screen array, we won't really be exposed.  So wait
      ;; until really exposed to prevent infinite regression
      (PROCESS-WAIT "Typeout Exposed" #'CAR (LOCF (SHEET-EXPOSED-P SELF))))
  (SETQ BOTTOM-REACHED (OR BOTTOM-REACHED 0)
        INCOMPLETE-P T)
  ;;On becoming exposed, also be the selection substitute for an appropriate ancestor.
  (AND (NEQ SELF SELECTED-WINDOW)
       (LET ((TEM (TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR SELF)))
         (UNLESS (EQ (SEND TEM :SELECTION-SUBSTITUTE) SELF)
           (SETQ WINDOW-SUBSTITUTING-FOR TEM)
           (SETQ PREVIOUS-SUBSTITUTE (SEND TEM :SELECTION-SUBSTITUTE))
           (SEND WINDOW-SUBSTITUTING-FOR :SET-SELECTION-SUBSTITUTE SELF))))
  (SEND SELF :HOME-CURSOR)
  (SEND SELF :CLEAR-REST-OF-LINE))

;;; This message is sent to *TERMINAL-IO*.  The sender doesn't really
;;; want to wait till the typeout window is exposed; he wants to wait
;;; till the window he knows about is exposed.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AWAIT-EXPOSURE) ()
  (SEND SUPERIOR :AWAIT-EXPOSURE))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :NOTICE) (EVENT &REST IGNORE)
  (AND (EQ EVENT ':ERROR)
       (SHEET-CAN-GET-LOCK SELF)        ;Try not to get hung before deciding
       (SHEET-CAN-GET-LOCK SUPERIOR)    ;to use the cold-load stream
       (SHEET-SCREEN-ARRAY SUPERIOR)    ;KLUDGE: don't wait in above method for screen-array
       (EQUAL DEEXPOSED-TYPEOUT-ACTION '(:EXPOSE-FOR-TYPEOUT))  ; and make sure of this too
       (SEND SELF :OUTPUT-HOLD-EXCEPTION))
  NIL)

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :DEEXPOSE)
           (&OPTIONAL IGNORE IGNORE (REMOVE-FROM-SUPERIOR T))
  (WHEN REMOVE-FROM-SUPERIOR
    (SETQ BOTTOM-REACHED NIL)
    ;; The following used to be in a :BEFORE :DEEXPOSE method, now eliminated.
    (SETQ INCOMPLETE-P NIL)))

;;; Find the nearest (smallest) ancestor
;;; which is currently in the chain of selection substitution
;;; coming down from our ultimate ancestor.
(DEFUN TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR (WINDOW)
  (LET ((HIGHEST (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)))
    (DO ((W HIGHEST (SEND W :SELECTION-SUBSTITUTE))
         (PREV HIGHEST W))
        ((OR (NULL W)
             (EQ W WINDOW)
             (NOT (SHEET-ME-OR-MY-KID-P SELF W)))
         PREV))))

;;; :DEACTIVATE is how the superior gets rid of the typeout window.
;;; Allow the ancestor (or his previous substitute) to become selected again.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :DEACTIVATE) ()
  (WHEN (EQ SELF (SEND WINDOW-SUBSTITUTING-FOR :ULTIMATE-SELECTION-SUBSTITUTE))
    (SEND WINDOW-SUBSTITUTING-FOR :REMOVE-SELECTION-SUBSTITUTE SELF
          (IF (NEQ PREVIOUS-SUBSTITUTE SELF)
              PREVIOUS-SUBSTITUTE
            WINDOW-SUBSTITUTING-FOR))))

;;; Our superior's blinkers should disappear entirely while we are selected.
;;; In fact, that should happen whenever we are exposed,
;;; but I am not confident of how to implement that.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SELECT) (&REST IGNORE)
  (TURN-OFF-SHEET-BLINKERS SUPERIOR))

(DEFFLAVOR TYPEOUT-WINDOW ((LABEL NIL) (BORDERS NIL))
  (BASIC-TYPEOUT-WINDOW NOTIFICATION-MIXIN WINDOW))

(DEFFLAVOR TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS ((LABEL NIL) (BORDERS NIL))
  (BASIC-MOUSE-SENSITIVE-ITEMS TYPEOUT-WINDOW)
  (:DOCUMENTATION :COMBINATION "Typeout window with item operations"))

(defflavor temporary-typeout-window-mixin
         ((superior-bit-array nil)
          (superior-bits-saved-p nil))
         ()
  (:required-flavors basic-typeout-window)
  (:documentation "A typeout window which saves and restores its superior's screen array."))

(defmethod (temporary-typeout-window-mixin :before :expose-for-typeout) ()
  ;; Make sure we have an array big enough to save the covered part of our superior
  (if superior-bit-array
      (let ((swidth (pixel-array-width superior-bit-array))
            (sheight (pixel-array-height superior-bit-array)))
        (unless (and ( swidth width) ( sheight height))
          (setq superior-bit-array (grow-bit-array superior-bit-array
                                                   width height
                                                   width
                                                   swidth sheight
                                                   nil))))
      (setq superior-bit-array (make-sheet-bit-array self width height)))
  (send superior :bitblt-from-sheet alu-seta (sheet-inside-width) (sheet-inside-height)
                                    0 0
                                    superior-bit-array 0 0)
  (setq superior-bits-saved-p t))

(defmethod (temporary-typeout-window-mixin :after :deexpose) (&rest ignore)
  (when (and superior-bit-array superior-bits-saved-p)
    (sheet-force-access (superior t)
      (send superior :bitblt alu-seta (sheet-inside-width) (sheet-inside-height)
                             superior-bit-array 0 0 0 0))
    (setq superior-bits-saved-p nil)))

(defflavor temporary-typeout-window ()
           (temporary-typeout-window-mixin typeout-window)
  (:documentation "A typeout window which saves and restores its superior's screen array."))

(COMPILE-FLAVOR-METHODS TYPEOUT-WINDOW TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS
                        temporary-typeout-window)

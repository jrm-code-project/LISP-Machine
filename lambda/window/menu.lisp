;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; New menu system

;Documentation on menu item-lists:
;
;Each item in the item-list may be one of the following:
; 1. A string (or a symbol).
; 2. Cons of a string (or a symbol) and an atom.
; 3. List of a string (or a symbol) and any object.  The list may
;    not be more than 2 long.
; 4. List of a string (or a symbol), a flavor keyword, and an argument.
;    After the first 3 elements of the list, the rest of the list is
;    a property list of modifier keywords and values.
;The string (or symbol) is displayed in the menu to represent this item.
;The value returned by the :CHOOSE method is the item in case 1, the cdr
;in case 2, the cadr in case 3, and varies in case 4 depending on the flavor.
;Case 4 menu items can also have side-effects.
;The following are the permissible flavor keywords:
;               :VALUE - argument is returned by the :CHOOSE method
;               :EVAL - argument is evaluated then returned
;               :FUNCALL - argument is a function of no args to be called
;               :NO-SELECT - this item cannot be selected
;               :WINDOW-OP - argument is a function of one argument.  This
;                       argument is a list of window, mouse-x, mouse-y as they
;                       were before the menu popped up.
;               :KBD - argument is forced into keyboard input of appropriate process.
;               :MENU - argument is a new menu to choose from.
;               :MENU-CHOOSE - arg is a list (label . menu-choose-alist)
;                       passed to MENU-CHOOSE
;               :BUTTONS - argument is 3 items, which one is used depends on
;                       which mouse button was clicked
;The following are the known modifier keywords:
;               :FONT - the font in which to display the item
;               :DOCUMENTATION - a string documenting this item
;               :BINDINGS - a LET-list of bindings to do with PROGW
;                       before funcalling, evalling, etc.
;                       With :BUTTONS, the :BINDINGS must be INSIDE the :BUTTONS.
;This stuff is largely although not entirely controlled by the :EXECUTE method,
;which you may redefine.

;;; These special variables exist so that there are less random numbers
;;; in the code, giving somewhat more chance of understanding it.
;;; You might even want to change them.
(DEFCONST MENU-INTERWORD-SPACING 27.
  "Minimum spacing between items used in filled menus.")
(DEFCONST MENU-INTERCOLUMN-SPACING 10.
  "Minimum spacing between columns in non-filled menus.")
(DEFCONST MENU-FILL-BREAKAGE 60.
  "Estimated spacing needed for each item in a filled menu.")
(DEFCONST MENU-GOLDEN-RATIO 1.6s0
  "Aspect ratio used for menus when there are no constraints.")

;;; MENU-EXECUTE-MIXIN flavor processes a menu-like item

(DEFFLAVOR MENU-EXECUTE-MIXIN () ()
  (:DOCUMENTATION "Processes a menu-like item
This is a part of every menu, it is a separate flavor so that it can be included in other
things which want to act like menus with regard to the format of an item passed to a
:EXECUTE message.  This message is what handles most of the interpretation of the
item-list instance variable."))

;;; Decide what to return based on the item selected.  Also have side-effects
;;; such as calling a function if the item says to.
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE) (ITEM &AUX OP ARG)
  (COND ((ATOM ITEM) ITEM)
        ((ATOM (CDR ITEM)) (CDR ITEM))
        ((ATOM (CDDR ITEM)) (CADR ITEM))
        (T
         (LET ((BINDINGS (GETF (CDDDR ITEM) ':BINDINGS)))
           (PROGW BINDINGS
             (SETQ ARG (CADDR ITEM)
                   OP (CADR ITEM))
             (RETURN
               (CASE OP
                 (:VALUE
                  ARG)
                 (:EVAL (SI:EVAL-SPECIAL-OK ARG))
                 (:FUNCALL (FUNCALL ARG))
                 (:FUNCALL-WITH-SELF (FUNCALL ARG SELF))
                 (:WINDOW-OP (SEND SELF :EXECUTE-WINDOW-OP ARG))
                 (:KBD
                  (AND SELECTED-WINDOW (SEND SELECTED-WINDOW :FORCE-KBD-INPUT ARG)))
                 (:MENU (SEND (EVAL ARG) :CHOOSE))
                 (:MENU-CHOOSE
                  (LET (RESULT DONE)
                    (PROCESS-RUN-FUNCTION "Menu"
                                          #'(LAMBDA (ARG BINDINGS RESULT-LOC DONE-LOC)
                                              (PROGW BINDINGS
                                                (UNWIND-PROTECT
                                                    (SETF (CONTENTS RESULT-LOC)
                                                          (MENU-CHOOSE (CDR ARG) (CAR ARG)))
                                                  (SETF (CONTENTS DONE-LOC) T))))
                                          ARG BINDINGS
                                          (LOCF RESULT) (LOCF DONE))
                    (OR (EQ CURRENT-PROCESS MOUSE-PROCESS)
                        (PROCESS-WAIT "Menu" #'CONTENTS (LOCF DONE)))
                    RESULT))
                 (T (FERROR "~S is unknown operation for ~S" OP :EXECUTE)))))))))

;Same as above but returns NIL if getting the value would require side-effects.
;This is used by MENU-HIGHLIGHTING-MIXIN
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-NO-SIDE-EFFECTS) (ITEM)
  (MENU-EXECUTE-NO-SIDE-EFFECTS ITEM))

(DEFUN MENU-EXECUTE-NO-SIDE-EFFECTS (ITEM &AUX OP ARG)
  "Try to get the value a menu would return if ITEM were chosen, but avoid side effects.
If getting the value might require a side-effect, just return NIL."
  (COND ((ATOM ITEM) ITEM)
        ((ATOM (CDR ITEM)) (CDR ITEM))
        ((ATOM (CDDR ITEM)) (CADR ITEM))
        ((EQ (SETQ ARG (CADDR ITEM)
                   OP (CADR ITEM))
             ':VALUE)
         ARG)
        (T NIL)))

(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION)
  (FUNCALL FUNCTION))

(DEFFLAVOR BASIC-MENU
           (ITEM-LIST                   ;List of items being displayed.
            CURRENT-ITEM                ;Item being pointed at now.
            LAST-ITEM                   ;The last item to have been selected.
            (CHOSEN-ITEM NIL)           ;The same, but it's ok to set this to NIL
                                        ;and wait for it to become non-NIL.
            SCREEN-ROWS                 ;Number of rows in menu on screen
            TOTAL-ROWS                  ;Total number of rows in menu.
                                        ;If this is greater than SCREEN-ROWS, then the latter
                                        ;represent a window on all the rows.
            TOP-ROW                     ;This is first row visible now.
            ROW-HEIGHT                  ;Height in dots of a row (including vsp).
            ROW-MAP                     ;Array of tails of ITEM-LIST.  For each row
                                        ;in the menu, points to first item on that row.
                                        ;An extra element at the end is NIL.
                                        ;The length is thus (1+ TOTAL-ROWS).
            (COLUMNS NIL)               ;Number of columns (NIL in fill mode).
            COLUMN-WIDTH                ;Width in dots of a column (NIL in fill mode).

            ;; GEOMETRY is the user specified geometry.  It is a list of:
            ;; Number of columns or 0 if FILL-P, number of rows, inside width, inside height,
            ;; maximum width, maximum height.  NIL means it's free to change, as was not
            ;; explicitly specified by the user.  Default is to leave everything free.
            (GEOMETRY
             (LIST NIL NIL NIL NIL NIL NIL))
            (LAST-INSIDE-HEIGHT NIL)            ;The height and width we used the last time
            (LAST-INSIDE-WIDTH NIL)             ;we computed the row map.
            DEFAULT-FONT                        ;what to display item in if not otherwise
           )                                    ; specified
           (MENU-EXECUTE-MIXIN)
  (:REQUIRED-FLAVORS SHEET BASIC-SCROLL-BAR)
  (:GETTABLE-INSTANCE-VARIABLES ITEM-LIST CURRENT-ITEM LAST-ITEM CHOSEN-ITEM GEOMETRY
                                DEFAULT-FONT)
  (:SETTABLE-INSTANCE-VARIABLES LAST-ITEM CHOSEN-ITEM)
  (:INITABLE-INSTANCE-VARIABLES ITEM-LIST DEFAULT-FONT)
  (:INIT-KEYWORDS :ROWS :COLUMNS :FILL-P :GEOMETRY)  ;Set parts of geometry
  (:DEFAULT-INIT-PLIST :BLINKER-FLAVOR 'TV:HOLLOW-RECTANGULAR-BLINKER)
  (:DOCUMENTATION "Regular menu messages
Provides methods and instance variables common to all menus, such as the item-list,
the geometry hacking, a default :choose message, and a scroll bar if necessary."))

;The GEOMETRY variable in a menu has a structure of this type as its value.
;That variable records "permanent" constraining shape information,
;that is to be used in recomputing the shape when things (such as the item list) change.
(DEFSTRUCT (GEOMETRY :LIST (:CONSTRUCTOR NIL))
  GEOMETRY-N-COLUMNS
  GEOMETRY-N-ROWS
  GEOMETRY-INSIDE-WIDTH
  GEOMETRY-INSIDE-HEIGHT
  GEOMETRY-MAX-WIDTH
  GEOMETRY-MAX-HEIGHT)

(DEFMACRO GEOMETRY-FILL-P (GEO) `(AND (GEOMETRY-N-COLUMNS ,GEO)
                                      (ZEROP (GEOMETRY-N-COLUMNS ,GEO))))

(DEFSETF GEOMETRY-FILL-P (GEO) (VALUE)
  `(SETF (GEOMETRY-N-COLUMNS ,GEO) (IF ,VALUE 0 NIL)))

(DEFFLAVOR MENU ((LABEL NIL))
                (BASIC-MENU BORDERS-MIXIN TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION "The simplest instantiatable menu.
Defaults to not having a label, a label whose position is not initially specified will
be at the top, in a small auxiliary box, unlike most windows."))

(DEFFLAVOR TEMPORARY-MENU () (TEMPORARY-WINDOW-MIXIN MENU)
  (:DOCUMENTATION "A menu that is temporary
This is not a momentary menu, it must be exposed and deexposed normally, it does save
the state beneath itself when exposed."))

(DEFFLAVOR POP-UP-MENU () (TEMPORARY-MENU) :ALIAS-FLAVOR)

(defflavor temporary-menu-with-flashy-scrolling () (flashy-margin-scrolling-mixin temporary-menu)
  (:documentation "A menu like it sounds."))

(DEFMETHOD (SCREEN :MENU-FONT) ()
  (SEND SELF :FONT-NAME-FOR :MENU))

(DEFUN MENU-ITEM-STRING (ITEM &OPTIONAL ITEM-DEFAULT-FONT MENU &AUX STRING FONT)
  "Return the string to print for item ITEM, and the font to use.
Uses ITEM-DEFAULT-FONT if the item does not specify one."
  (DECLARE (VALUES STRING FONT))
  (IF (ATOM ITEM)
      (SETQ STRING ITEM)
      (SETQ STRING (CAR ITEM))
      (AND (CONSP (CDR ITEM))
           (SETQ FONT (GET (CDDR ITEM) ':FONT))))
  (COND ((NULL FONT) (SETQ FONT ITEM-DEFAULT-FONT))
        ((NULL MENU))
        ((SYMBOLP FONT)
         (SETQ FONT (SEND (SHEET-GET-SCREEN MENU) :PARSE-FONT-DESCRIPTOR FONT)))
        ((NUMBERP FONT) (SETQ FONT (AREF (SHEET-FONT-MAP MENU) FONT))))
  (VALUES (STRING STRING) FONT))

(DEFUN MENU-ITEM-STRING-WIDTH (ITEM &OPTIONAL STOP-X)
  "Return the width in pixels of the way item ITEM is displayed on the menu.
If STOP-X is specified, we will not return a value larger than that."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (MULTIPLE-VALUE-BIND (STRING FONT)
      (MENU-ITEM-STRING ITEM (FONT-EVALUATE DEFAULT-FONT) SELF) ;don't need the font-evaluate
    (SHEET-STRING-LENGTH SELF STRING 0 NIL STOP-X FONT)))       ; after installation into 98

(DEFUN MENU-COMPUTE-FONT-MAP (ITEMS &AUX (MAP (NCONS DEFAULT-FONT)) FONT)
  "Compute the font map to use for a menu, from an item list.
The font map we compute has all the fonts any items need."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (DOLIST (ITEM ITEMS)
    (SETQ FONT (AND (CONSP ITEM)
                    (CONSP (CDR ITEM))
                    (GET (CDDR ITEM) ':FONT)))
    (AND FONT (NOT (MEMQ FONT MAP))
         (PUSH FONT MAP)))
  (NREVERSE MAP))

(DEFUN MENU-COMPUTE-GEOMETRY (DRAW-P &OPTIONAL INSIDE-WIDTH INSIDE-HEIGHT)
  "This function is called whenever something related to the geometry changes.
INSIDE-WIDTH and INSIDE-HEIGHT optionally specify the shape to use,
if we are recomputing the row layout but want to use a particular shape
/(such as, if the menu has been reshaped by the user).
The menu is redrawn if DRAW-P is T.  In any case, we do everything necessary
to adapt the menu to the current item-list and the optionally specified size"
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (WHEN (VARIABLE-BOUNDP ITEM-LIST)  ;Do nothing if item-list not specified yet
    ;; Get the new N-ROWS and so forth.
    (MULTIPLE-VALUE (COLUMNS SCREEN-ROWS INSIDE-WIDTH INSIDE-HEIGHT)
      (MENU-DEDUCE-PARAMETERS NIL NIL INSIDE-WIDTH INSIDE-HEIGHT NIL NIL))
    ;; Recompute the row map
    (MULTIPLE-VALUE (ROW-MAP TOTAL-ROWS)
      (MENU-COMPUTE-ROW-MAP INSIDE-WIDTH))
    (SETQ TOP-ROW 0
          ROW-HEIGHT LINE-HEIGHT)
    (SEND SELF :NEW-SCROLL-POSITION TOP-ROW)
    (SETQ COLUMN-WIDTH
          (AND (NOT (GEOMETRY-FILL-P GEOMETRY))
               (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING) COLUMNS)))
    (SETQ LAST-INSIDE-WIDTH INSIDE-WIDTH
          LAST-INSIDE-HEIGHT INSIDE-HEIGHT)
    (COND ((AND (= INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
                (= INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
           (AND DRAW-P
                (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
                  (SEND SELF :MENU-DRAW))))
          ((SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :VERIFY)
           ;; Room to do this in current place.
           (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :TEMPORARY))
          (T
           ;; Else try to be approximately in the same place
           (LET ((CX (+ X-OFFSET (TRUNCATE WIDTH 2)))
                 (CY (+ Y-OFFSET (TRUNCATE HEIGHT 2))))
             (WITH-SHEET-DEEXPOSED (SELF)
               (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT ':TEMPORARY)
               (CENTER-WINDOW-AROUND SELF CX CY))))))
  NIL)

;;; This function, given a bunch of parameters some of which are NIL meaning
;;; unspecified, deduces the rest of the parameters from constraints.
;;; For parameters passed in as NIL, the corresponding element of GEOMETRY
;;; is used.

;;; First, compute the geometry
;;;  (1) The user has supplied the width and the number of columns or fill-p, nothing special.
;;;  (2) The user has supplied the width, we compute the number of columns
;;;             by finding the widest string in the item-list.
;;;  (3) The user has not supplied the width, but has supplied n-columns, we compute width
;;;             again by finding the widest string in the item-list.
;;;  (4) The user has supplied neither width nor n-columns.
;;;    (4a) The user has, however, supplied height or n-rows, so we pick a suitable width
;;;         to make the entire menu come out to n-rows, depending on fill mode.  Then if
;;;         it doesn't fit, this width will be wider than the screen, and will be limited.
;;;    (4b) The user has supplied no geometry, it's up to us.
;;;         Compute the total width depending on fill-mode, then pick n-rows and
;;;         n-columns to make this a square array.  Then limit each to the available
;;;         area of the screen, in case the menu is too big to fit.
;;;         Not actually square but the prettiest looking shape.

;;; Once the horizontal business has been straightened out, if we don't have the
;;; height already, we pick a height to make it all fit on the screen, and limit that
;;; if it is too big.  Note that fill-mode has a line-breakage problem, which will
;;; need to be solved here (may change shape "slightly" from square.)

;;; Arguments:
;;; SELF, ITEM-LIST and GEOMETRY are used freely.
;;; SELF should have the right font, screen, vsp but not
;;;       yet the right dimensions and location.

(DEFUN MENU-DEDUCE-PARAMETERS (N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
                               MAX-WIDTH MAX-HEIGHT
                               &AUX TEM COL-WIDTH ;NIL if N-COLUMNS not chosen in here
                                    (N-ITEMS (LENGTH ITEM-LIST))
                                    FILL-P MIN-WIDTH)
  "Compute or default unspecified menu geometry parameters from the specified ones.
Each parameter can be specified as a number, can be :UNCONSTRAINED meaning
compute it from the others, or can be NIL meaning use the corresponding
/"permanent/" parameter which is an element of GEOMETRY.
The specified parameters, together with the item list, default font and vsp,
are used to compute the ones that were not specified.
If there is not enough information to determine things, pretty choices are made.
The values have the same meanings as the first four arguments,
except that all four values will always be numbers.

N-COLUMNS = 0 means a /"filled/" menu is desired, where different rows
can have different number of items."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (DECLARE (VALUES N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT))
  ;; Pick up default constraints from GEOMETRY
  (SETQ N-COLUMNS (OR N-COLUMNS (GEOMETRY-N-COLUMNS GEOMETRY))
        N-ROWS (OR N-ROWS (GEOMETRY-N-ROWS GEOMETRY))
        INSIDE-WIDTH (OR INSIDE-WIDTH (GEOMETRY-INSIDE-WIDTH GEOMETRY))
        INSIDE-HEIGHT (OR INSIDE-HEIGHT (GEOMETRY-INSIDE-HEIGHT GEOMETRY))
        MAX-WIDTH (OR MAX-WIDTH (GEOMETRY-MAX-WIDTH GEOMETRY))
        MAX-HEIGHT (OR MAX-HEIGHT (GEOMETRY-MAX-HEIGHT GEOMETRY)))
  ;; If any of the arguments was :UNCONSTRAINED, that means use NIL
  ;; even if the geometry is non-NIL, whereas if an argument was NIL
  ;; that means use any constraint that is in the geometry.
  (AND (EQ N-COLUMNS ':UNCONSTRAINED) (SETQ N-COLUMNS NIL))
  (AND (EQ N-ROWS ':UNCONSTRAINED) (SETQ N-ROWS NIL))
  (AND (EQ INSIDE-WIDTH ':UNCONSTRAINED) (SETQ INSIDE-WIDTH NIL))
  (AND (EQ INSIDE-HEIGHT ':UNCONSTRAINED) (SETQ INSIDE-HEIGHT NIL))
  (AND (EQ MAX-WIDTH ':UNCONSTRAINED) (SETQ MAX-WIDTH NIL))
  (AND (EQ MAX-HEIGHT ':UNCONSTRAINED) (SETQ MAX-HEIGHT NIL))
  ;; Decide whether it is fill mode or array mode
  (AND (SETQ FILL-P (AND N-COLUMNS (ZEROP N-COLUMNS)))
       (SETQ N-COLUMNS NIL))

  (IF INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (MAX INSIDE-HEIGHT LINE-HEIGHT)))

  ;; Realize any immediately clear implications
  (AND N-ROWS (NULL INSIDE-HEIGHT) (SETQ INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))
  (AND INSIDE-HEIGHT (NULL N-ROWS) (SETQ N-ROWS (TRUNCATE INSIDE-HEIGHT LINE-HEIGHT)))
  (SETQ MAX-HEIGHT (MIN (OR INSIDE-HEIGHT MAX-HEIGHT 10000)
                        (- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
        MAX-WIDTH (MIN (OR INSIDE-WIDTH MAX-WIDTH 10000)
                       (- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
  (SETQ MIN-WIDTH (MIN (SEND SELF :MINIMUM-WIDTH) MAX-WIDTH))
  (IF INSIDE-WIDTH (SETQ INSIDE-WIDTH (MAX INSIDE-WIDTH MIN-WIDTH)))

  ;; Compute the horizontal parameters.
  (COND ((AND INSIDE-WIDTH (OR N-COLUMNS FILL-P)))              ;It's fully-determined
        (INSIDE-WIDTH           ;We have the width, and it's not in fill mode, compute
         (SETQ N-COLUMNS        ; N-COLUMNS based on widest item, but always fill the space
               (MAX (MIN (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
                                   (+ (MENU-MAX-WIDTH ITEM-LIST) MENU-INTERCOLUMN-SPACING))
                         (IF N-ROWS (CEILING N-ITEMS N-ROWS) N-ITEMS))
                    1)))
        (N-COLUMNS  ;We don't have the width, but do know how many columns, compute width
         (SETQ INSIDE-WIDTH (MIN (- (* (+ (MENU-MAX-WIDTH ITEM-LIST)
                                          MENU-INTERCOLUMN-SPACING)
                                       N-COLUMNS)
                                    MENU-INTERCOLUMN-SPACING)
                                 MAX-WIDTH)))
        (N-ROWS  ;We know how high, make it wide enough to come out this high
         (IF FILL-P
             (SETQ INSIDE-WIDTH (MIN (CEILING (MENU-FILL-WIDTH ITEM-LIST)
                                              N-ROWS)
                                     MAX-WIDTH))
             (SETQ N-COLUMNS (MAX (CEILING N-ITEMS N-ROWS) 1)
                   COL-WIDTH (+ (MENU-MAX-WIDTH ITEM-LIST)
                                MENU-INTERCOLUMN-SPACING)
                   INSIDE-WIDTH (- (* COL-WIDTH
                                      N-COLUMNS)
                                   MENU-INTERCOLUMN-SPACING))))
        ((NOT FILL-P) ;No geometry supplied, pick N-ROWS and N-COLUMNS to make it look nice
                      ;Use the largest number of columns which does not make the ratio
                      ;of height to width less than the Golden ratio
         (SETQ TEM (* (SETQ COL-WIDTH (MENU-MAX-WIDTH ITEM-LIST))
                      N-ITEMS
                      LINE-HEIGHT)
               COL-WIDTH (+ COL-WIDTH MENU-INTERCOLUMN-SPACING)
               N-COLUMNS (MAX (TRUNCATE (ISQRT (FLOOR TEM MENU-GOLDEN-RATIO)) COL-WIDTH) 1)
               INSIDE-WIDTH (- (* COL-WIDTH N-COLUMNS) MENU-INTERCOLUMN-SPACING)))
        (T      ;No geometry supplied, and in fill mode, make it like above
         (SETQ INSIDE-WIDTH (MAX (ISQRT (FLOOR (* (MENU-FILL-WIDTH ITEM-LIST)
                                                  LINE-HEIGHT)
                                               MENU-GOLDEN-RATIO))
                                 #o40))))       ;Don't get zero, and don't get absurdly small

  ;; If too wide, or not wide enough for label (etc.), alter the width.
  (UNLESS ( MIN-WIDTH INSIDE-WIDTH MAX-WIDTH)
    (SETQ INSIDE-WIDTH (MIN MAX-WIDTH (MAX MIN-WIDTH INSIDE-WIDTH)))
    (AND COL-WIDTH  ;If N-COLUMNS was not user-supplied, recompute it
         (SETQ N-COLUMNS (MAX (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
                                        COL-WIDTH)
                              1))))

  ;; Now figure out the vertical characteristics
  (OR N-ROWS
      (SETQ N-ROWS (IF FILL-P
                       (CEILING (MENU-FILL-WIDTH ITEM-LIST) INSIDE-WIDTH)
                     (CEILING N-ITEMS N-COLUMNS))))
  (OR INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (MAX (* N-ROWS LINE-HEIGHT) 8)))

  ;; If this came out too high, retrench
  (AND (> INSIDE-HEIGHT MAX-HEIGHT)
       (SETQ N-ROWS (TRUNCATE MAX-HEIGHT LINE-HEIGHT)
             INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))

  ;; At this point, INSIDE-WIDTH, INSIDE-HEIGHT, N-COLUMNS (if not FILL-P), and N-ROWS
  ;; are all valid and consistent, and not bigger than the available area,
  ;; provided that the user's original parameters were not illegally huge.

  ;; Return all the dependent parameters as multiple values
  (VALUES (IF FILL-P 0 N-COLUMNS) N-ROWS INSIDE-WIDTH INSIDE-HEIGHT))

(DEFMETHOD (BASIC-MENU :MINIMUM-WIDTH) ()
  ;; If there is a label, the menu must be at least wide enough to accomodate it
  (LET ((L (SEND-IF-HANDLES SELF :LABEL-SIZE)))
    (IF L (MAX L 20.) 20.)))

;;; This function computes the ROW-MAP, which determines how many strings per line, & c.
;;; The first value is the row-map and the second is the n-total-rows
(DEFUN MENU-COMPUTE-ROW-MAP (&OPTIONAL (INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
                             &AUX (MAP (MAKE-ARRAY (1+ (LENGTH ITEM-LIST))))
                                  WID
                                  (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (DO ((ITEMS ITEM-LIST) (ROW 0 (1+ ROW)))
      ((NULL ITEMS)
       (VALUES (ADJUST-ARRAY-SIZE MAP (1+ ROW))  ;Last element always contains NIL
               ROW))
    (ASET ITEMS MAP ROW)        ;This is where this row starts
    (IF FILL-P          ;Fill mode, we have some hairy calculation to do
        (DO ((SPACE INSIDE-WIDTH))
            ((NULL ITEMS))
          (SETQ WID (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))
          (WHEN (> WID SPACE)   ;This one won't fit, break the line
            (AND (> WID INSIDE-WIDTH)
                 (FERROR "The item /"~A/" is too wide for this fill-mode menu"
                         (CAR ITEMS)))
            (RETURN NIL))
          (SETQ SPACE (- SPACE (+ WID MENU-INTERWORD-SPACING))
                ITEMS (CDR ITEMS)))
        (SETQ ITEMS (NTHCDR COLUMNS ITEMS)))))

(DEFMETHOD (BASIC-MENU :BEFORE :INIT) (INIT-PLIST &AUX (SUP SUPERIOR) TEM)
  (SETQ SUP (OR SUP (GET INIT-PLIST ':SUPERIOR) DEFAULT-SCREEN))
  (SETQ DEFAULT-FONT (IF (VARIABLE-BOUNDP DEFAULT-FONT)
                         (SEND (SHEET-GET-SCREEN SUP) ':PARSE-FONT-SPECIFIER DEFAULT-FONT)
                       (SEND (SHEET-GET-SCREEN SUP) ':PARSE-FONT-SPECIFIER ':MENU)))
  (OR (VARIABLE-BOUNDP FONT-MAP)
      (SETQ FONT-MAP (MENU-COMPUTE-FONT-MAP (GET INIT-PLIST ':ITEM-LIST))))
  (PUTPROP INIT-PLIST NIL ':MORE-P)
  (SETQ TEM (GET INIT-PLIST ':GEOMETRY))
  (IF (> (LENGTH TEM) (LENGTH GEOMETRY))
      ;; Longer than we need, take a copy of the list
      (SETQ GEOMETRY (COPYLIST TEM))
      ;; Else copy the appropriate piece of user specified list into our list
      (DO ((TEM TEM (CDR TEM))
           (GEO GEOMETRY (CDR GEO)))
          ((NULL TEM))
        (SETF (CAR GEO) (CAR TEM))))
  (AND (GET INIT-PLIST ':FILL-P)
       ;(SETF (GEOMETRY-FILL-P GEOMETRY) T)  ;Compiler gives a gratuitous warning for this
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) 0))
  (AND (SETQ TEM (GET INIT-PLIST ':ROWS))
       (SETF (GEOMETRY-N-ROWS GEOMETRY) TEM))
  (AND (SETQ TEM (GET INIT-PLIST ':COLUMNS))
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) TEM))
  ;; We'll handle SAVE-BITS ourselves later
  ;; This is so the bit array doesn't get created until we know the size
  (PUTPROP INIT-PLIST (GET INIT-PLIST ':SAVE-BITS) ':MENU-SAVE-BITS)
  (PUTPROP INIT-PLIST NIL ':SAVE-BITS))

(DEFMETHOD (BASIC-MENU :AFTER :INIT) (INIT-PLIST)
  (SETF (BLINKER-VISIBILITY (CAR BLINKER-LIST)) NIL)
  (MENU-COMPUTE-GEOMETRY NIL)
  (SEND SELF :SET-SAVE-BITS (GET INIT-PLIST ':MENU-SAVE-BITS)))

(DEFMETHOD (BASIC-MENU :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND RESTORED-BITS-P (NEQ TYPE ':SIZE-CHANGED))
      (SEND SELF :MENU-DRAW)))

;;; When we change our inside size, we must recompute the geometry with the new inside size,
;;; unless it is the same as we last computed the geometry for.
;;; If we get here from recomputing the geometry, that will be true, and avoid a loop.
(DEFMETHOD (BASIC-MENU :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (OR (AND (EQUAL (SHEET-INSIDE-WIDTH) LAST-INSIDE-WIDTH)
           (EQUAL (SHEET-INSIDE-HEIGHT) LAST-INSIDE-HEIGHT))
      (MENU-COMPUTE-GEOMETRY NIL (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT))))

(DEFMETHOD (BASIC-MENU :AFTER :CHANGE-OF-DEFAULT-FONT) (IGNORE IGNORE)
  (MENU-COMPUTE-GEOMETRY T (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))

;OPTION = :TEMPORARY means don't change the constraints.
;That is used in calls from within this file.
;If the user randomly does a :SET-EDGES, we assume he wants it to stay as he said.
(DEFMETHOD (BASIC-MENU :AFTER :SET-EDGES) (IGNORE IGNORE IGNORE IGNORE &OPTIONAL OPTION)
  (WHEN (NOT (MEMQ OPTION '(:VERIFY :TEMPORARY)))
    (SETF (GEOMETRY-INSIDE-WIDTH GEOMETRY) (SHEET-INSIDE-WIDTH))
    (SETF (GEOMETRY-INSIDE-HEIGHT GEOMETRY) (SHEET-INSIDE-HEIGHT))))

(DEFMETHOD (BASIC-MENU :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES NEW-X NEW-Y
                        (+ WIDTH NEW-X) (+ HEIGHT NEW-Y)
                        (OR OPTION ':TEMPORARY)))

;;; Changing our borders should preserve our INSIDE size, not our outside size as normally.
(DEFWRAPPER (BASIC-MENU :REDEFINE-MARGINS) (IGNORE . BODY)
  `(LET ((IW (SHEET-INSIDE-WIDTH)) (IH (SHEET-INSIDE-HEIGHT)))
     (PROG1 (PROGN . ,BODY)
            (MENU-COMPUTE-GEOMETRY NIL IW IH))))

(DEFMETHOD (BASIC-MENU :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SETQ NEW-ITEM-LIST (REMQ NIL NEW-ITEM-LIST))
  (SETQ ITEM-LIST NEW-ITEM-LIST
        LAST-ITEM NIL
        CURRENT-ITEM NIL)
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST))
  NEW-ITEM-LIST)

(DEFMETHOD (BASIC-MENU :AROUND :SET-LABEL) (CONT MT ARGS &REST NEW-LABEL)
  (UNLESS (EQUAL NEW-LABEL (SEND SELF :LABEL))
    (AROUND-METHOD-CONTINUE CONT MT ARGS)
    (MENU-COMPUTE-GEOMETRY T)))

(DEFMETHOD (BASIC-MENU :SET-DEFAULT-FONT) (FONT)
  (SETQ DEFAULT-FONT (SEND (SHEET-GET-SCREEN SELF) :PARSE-FONT-SPECIFIER FONT))
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))

(DEFMETHOD (BASIC-MENU :AFTER :SET-FONT-MAP) (&REST IGNORE)
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (BASIC-MENU :SET-GEOMETRY) (&REST NEW-GEOMETRY)
  (DECLARE (ARGLIST (&OPTIONAL N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
                               MAX-WIDTH MAX-HEIGHT)))
  "NIL for an argument means make it unconstrained.  T or unsupplied means leave it alone"
  (OR ( (LENGTH NEW-GEOMETRY) (LENGTH GEOMETRY))
      (FERROR "Too many args to ~S" :SET-GEOMETRY))
  (DO ((G NEW-GEOMETRY (CDR G))
       (CG GEOMETRY (CDR CG)))
      ((NULL G))
    (IF (NEQ (CAR G) T)
        (SETF (CAR CG) (CAR G))))
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (BASIC-MENU :CURRENT-GEOMETRY) ()
  "Like :GEOMETRY but returns the current state rather than the default"
  (LIST (IF (GEOMETRY-FILL-P GEOMETRY) 0 COLUMNS) TOTAL-ROWS
        (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
        (GEOMETRY-MAX-WIDTH GEOMETRY) (GEOMETRY-MAX-HEIGHT GEOMETRY)))

(DEFMETHOD (BASIC-MENU :FILL-P) ()
  (GEOMETRY-FILL-P GEOMETRY))
(DEFMETHOD (BASIC-MENU :SET-FILL-P) (FILL-P)
  (SEND SELF :SET-GEOMETRY (IF FILL-P 0 NIL)))

(DEFMETHOD (BASIC-MENU :MOUSE-STANDARD-BLINKER) ()
  ;; Change the mouse cursor to a small X so it doesn't get in the way
  (MOUSE-SET-BLINKER-DEFINITION ':CHARACTER 4 5 ':ON
                                ':SET-CHARACTER 7))

;;; Mouse handler for menus
(DEFMETHOD (BASIC-MENU :BEFORE :HANDLE-MOUSE) ()
  ;; Forget anything we knew before about the highlight, so it will really be positioned
  (SETQ CURRENT-ITEM NIL))

(DEFMETHOD (BASIC-MENU :AFTER :HANDLE-MOUSE) ()
  ;; When mouse leaves this window, stop flashing any item
  (BLINKER-SET-VISIBILITY (CAR BLINKER-LIST) NIL))

;;; Mouse-click handler for menus.
;;; All buttons are treated the same, select the item you are on.
;;; There are no double-clicks and you can't get to the system command menu.
;;; Clicking when the menu is not exposed just exposes it.

(DEFMETHOD (BASIC-MENU :MOUSE-BUTTONS) (BD X Y)
  (COND (CURRENT-ITEM                           ;Any button, select item.
         (SEND SELF :MOUSE-BUTTONS-ON-ITEM BD))
        ((AND ( X (SHEET-INSIDE-LEFT)) (< X (SHEET-INSIDE-RIGHT))
              ( Y (SHEET-INSIDE-TOP)) (< Y (SHEET-INSIDE-BOTTOM))))
        (T
         ;; Here, clicked on the window, but outside of the window proper.
         ;; Send a :MOUSE-CLICK message so things like margin regions can work.
         (SEND SELF :MOUSE-CLICK (MOUSE-BUTTON-ENCODE BD) X Y))))

(DEFMETHOD (BASIC-MENU :MOUSE-BUTTONS-ON-ITEM) (BD)
  (SETQ LAST-ITEM CURRENT-ITEM
        CHOSEN-ITEM CURRENT-ITEM)
  (IF (AND (CONSP CHOSEN-ITEM)
           ( (LENGTH CHOSEN-ITEM) 3)
           (EQ (SECOND CHOSEN-ITEM) ':BUTTONS))
      (SETQ CHOSEN-ITEM (NTH (1- (HAULONG BD)) (THIRD CHOSEN-ITEM)))))

(DEFMETHOD (BASIC-MENU :CHOOSE) ()
  (SETQ CHOSEN-ITEM NIL)
  (OR EXPOSED-P (SEND SELF :EXPOSE))
  (PROCESS-WAIT "Menu choose" #'(LAMBDA (ITEM-LOC STATUS-LOC)
                                  (OR (CONTENTS ITEM-LOC) (NULL (CONTENTS STATUS-LOC))))
                (LOCF CHOSEN-ITEM)
                (LOCF EXPOSED-P))
  (UNWIND-PROTECT
      (SEND SELF :EXECUTE CHOSEN-ITEM)
    (SETQ CHOSEN-ITEM NIL)))

;;; This is called from the scheduler
(DEFMETHOD (BASIC-MENU :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR (AND (VARIABLE-BOUNDP CURRENT-ITEM) (MENU-ITEM-WHO-LINE-DOCUMENTATION CURRENT-ITEM))
      ""))

(DEFUN MENU-ITEM-WHO-LINE-DOCUMENTATION (ITEM)
  "Return the who-line string for menu item ITEM."
  (AND (CONSP ITEM) (CONSP (CDR ITEM))
       (GET (CDDR ITEM) ':DOCUMENTATION)))

;;; This is the guts.  Given a menu and a set of coordinates, it finds
;;; the corresponding item, if any, sets CURRENT-ITEM to it, and sets up
;;; the blinker to mark that item.  If no item, the blinker is shut off.
;;;*** This tvobish code should be rewritten ***
(DEFMETHOD (BASIC-MENU :MOUSE-MOVES) (X Y
                                      &AUX ITEM ITEMS ROW XREL BLINKER BLX (BLWIDTH 0)
                                           COLN STOP-ITEM ITEM-BASELINE-ADJUST
                                           (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (MOUSE-SET-BLINKER-CURSORPOS)
  (SETQ ROW (TRUNCATE (- Y (SHEET-INSIDE-TOP) -2) ROW-HEIGHT)
        XREL (- X (SHEET-INSIDE-LEFT) -2)
        BLINKER (CAR BLINKER-LIST))
  (WHEN (AND ( X (SHEET-INSIDE-LEFT) 0)        ;If inside the menu
             (< X (SHEET-INSIDE-RIGHT))
             ( Y (SHEET-INSIDE-TOP))
             (< Y (SHEET-INSIDE-BOTTOM)))
    ;;If mouse is past the last displayed row, blink item on that row.
    (AND (OR ( (+ TOP-ROW ROW) TOTAL-ROWS) (>= ROW SCREEN-ROWS))
         (SETQ ROW (1- (MIN SCREEN-ROWS (- TOTAL-ROWS TOP-ROW)))))
    (IF (MINUSP ROW) (SETQ ITEMS NIL STOP-ITEM NIL)     ;No items visible
      (SETQ ITEMS (AREF ROW-MAP (+ TOP-ROW ROW))
            STOP-ITEM (AREF ROW-MAP (+ TOP-ROW ROW 1))))
    (COND (FILL-P                                       ;Fill mode, cogitate
           (SETQ BLX 0)
           (DO ((L ITEMS (CDR L))
                (ITM) (OITM NIL ITM)
                (X 0 (+ X
                        (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITM))
                        MENU-INTERWORD-SPACING)))
               ((OR (NULL L)
                    (> X XREL))                 ;If this string crosses the mouse, it's the one
                (SETQ ITEM OITM))
             (AND (EQ L STOP-ITEM)
                  ;; The next item on next line -- punt
                  (RETURN NIL))
             (SETQ ITM (CAR L)
                   BLX X)))
          (T                                            ;Columnated, find which column
           (SETQ COLN (TRUNCATE XREL COLUMN-WIDTH))     ;Column selected
           (SETQ ITEM (CAR (NTHCDR COLN ITEMS)))        ;This may be NIL
           (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH))
           (SETQ BLX (+ (* COLN COLUMN-WIDTH)           ;Start of column
                        (MAX 0 (TRUNCATE (- COLUMN-WIDTH;Centering
                                            MENU-INTERCOLUMN-SPACING
                                            BLWIDTH)
                                         2)))))))
  (MULTIPLE-VALUE-BIND (NIL ITEM-FONT)
      (MENU-ITEM-STRING ITEM CURRENT-FONT SELF)
    (SETQ ITEM-BASELINE-ADJUST (- BASELINE (FONT-BASELINE ITEM-FONT)))
    ;; If this item is non-selectable, don't select it.
    (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
         (EQ (CADR ITEM) ':NO-SELECT)
         (SETQ ITEM NIL))
    ;; Now make the blinker be where and what we have just found it should be.
    (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
    (SETQ CURRENT-ITEM ITEM)
    (WHEN ITEM
      (SEND BLINKER :SET-SIZE-AND-CURSORPOS
                    (+ BLWIDTH 1)
                    (+ (FONT-CHAR-HEIGHT ITEM-FONT)
                       0)
;                      1)
                    BLX
;                   (+ BLX 1)
                    (+ (* ROW ROW-HEIGHT) ITEM-BASELINE-ADJUST)
                    ))))

(DEFMETHOD (BASIC-MENU :SCROLL-POSITION) ()
  (VALUES TOP-ROW TOTAL-ROWS ROW-HEIGHT))

(DEFMETHOD (BASIC-MENU :SCROLL-TO) (LINE MODE)
  (ECASE MODE
    (:ABSOLUTE)
    (:RELATIVE (SETQ LINE (+ TOP-ROW LINE))))
  (WHEN ( (SETQ LINE (MAX 0 (MIN LINE (1- TOTAL-ROWS)))) TOP-ROW)
    ;; Actually changing something, update
    (SETQ TOP-ROW LINE)
    (SEND SELF :MENU-DRAW)
    (SEND SELF :NEW-SCROLL-POSITION TOP-ROW)))

;;; Put a menu near another window.  This will normally try to put it just below
;;; it and give it the same width.
(DEFMETHOD (BASIC-MENU :MOVE-NEAR-WINDOW) (W)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND W :EDGES)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE NEW-HEIGHT)
        (MENU-DEDUCE-PARAMETERS NIL NIL (- RIGHT LEFT) NIL NIL NIL)
      (SETQ NEW-HEIGHT (+ NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
      ;If it won't fit below try putting it above
      (AND (> (+ BOTTOM NEW-HEIGHT)
              (SHEET-INSIDE-BOTTOM SUPERIOR))
           (SETQ BOTTOM (MAX (- TOP NEW-HEIGHT) 0)))
      ;Put it there
      (SEND SELF :SET-EDGES LEFT BOTTOM RIGHT (+ BOTTOM NEW-HEIGHT) :TEMPORARY)
      (SEND SELF :EXPOSE))))

;;; This is used by othogonal things like hysteretic window
(DEFMETHOD (BASIC-MENU :SCROLL-BAR-P) () (< SCREEN-ROWS TOTAL-ROWS))

(DEFMETHOD (BASIC-MENU :MENU-DRAW) (&AUX (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  ;; Make sure the mouse knows we're changing
  (AND EXPOSED-P (MOUSE-WAKEUP))
  (PREPARE-SHEET (SELF)
    (SHEET-CLEAR SELF)
    (DO ((ROW TOP-ROW (1+ ROW))
         (Y-POS 0 (+ Y-POS ROW-HEIGHT))
         (LIM (MIN TOTAL-ROWS (+ TOP-ROW SCREEN-ROWS))))
        (( ROW LIM))
      (DO ((ITEMS (AREF ROW-MAP ROW) (CDR ITEMS))
           (END-ITEM-LIST (AREF ROW-MAP (1+ ROW)))
           (STR) (FONT) (FLAG)
           (X-POS 0))
          ((EQ ITEMS END-ITEM-LIST))
         (MULTIPLE-VALUE (STR FONT)
           (MENU-ITEM-STRING (CAR ITEMS) CURRENT-FONT SELF))
         (UNWIND-PROTECT
           (PROGN
             (AND (SETQ FLAG (AND (NEQ FONT CURRENT-FONT) CURRENT-FONT))
                  (SHEET-SET-FONT SELF FONT))
             (COND (FILL-P                      ;Filled, put string followed by spacing
                    (SHEET-SET-CURSORPOS SELF X-POS Y-POS)
                    (SHEET-STRING-OUT SELF STR)
                    (SETQ X-POS (+ (SHEET-READ-CURSORPOS SELF) MENU-INTERWORD-SPACING)))
                   (T                           ;Columnated, center text within column
                    (SHEET-DISPLAY-CENTERED-STRING SELF STR
                                                   X-POS
                                                   (- (SETQ X-POS (+ X-POS COLUMN-WIDTH))
                                                      MENU-INTERCOLUMN-SPACING)
                                                   Y-POS))))
           (AND FLAG (SHEET-SET-FONT SELF FLAG)))))))

(DEFUN MENU-MAX-WIDTH (ITEM-LIST)
  "Return the maximum width in pixels of any item in ITEM-LIST.
Normally you should add allowances for interword spacing to this."
  (DO ((L ITEM-LIST (CDR L))
       (MXW 1 (MAX MXW (MENU-ITEM-STRING-WIDTH (CAR L)))))
      ((NULL L) MXW)))

(DEFUN MENU-FILL-WIDTH (ITEM-LIST)
  "Return an estimate of the total length required for ITEM-LIST in a filled menu.
This is what the lengths of all the lines must add up to.
We take account of spacing between items, but we can only estimate
the amount of space wasted due to line breakage."
  (DO ((L ITEM-LIST (CDR L))
       (WID 0))
      ((NULL L) WID)
    (SETQ WID (+ WID (MENU-ITEM-STRING-WIDTH (CAR L)) MENU-FILL-BREAKAGE))))

;;; Here is how we make a menu appear with the last item chosen under the mouse.

;;; Return the x and y co-ordinates (inside the margins)
;;; of the center of the specified item, NIL if scrolled off display
(DEFMETHOD (BASIC-MENU :ITEM-CURSORPOS) (ITEM &AUX (ALEN (ARRAY-LENGTH ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)    ;last row on screen
                     ALEN))                     ;last row that exists
            (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (AND (MEMBER ITEM (AREF ROW-MAP ROW))
         (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW)))))
         (RETURN (VALUES (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
                             (+ (* (POSITION ITEM (AREF ROW-MAP ROW) :TEST #'EQUAL) COLUMN-WIDTH)
                                (TRUNCATE COLUMN-WIDTH 2))
                           (DO ((L (AREF ROW-MAP ROW) (CDR L))
                                (XSTART 0 (+ XSTART SWIDTH MENU-INTERWORD-SPACING))
                                (SWIDTH))
                               (NIL)
                             (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH (CAR L)))
                             (AND (EQ (CAR L) ITEM)
                                  (RETURN (+ XSTART (TRUNCATE SWIDTH 2))))))
                         (+ (* (- ROW TOP-ROW) ROW-HEIGHT) (TRUNCATE ROW-HEIGHT 2)))))))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:01
;;; Return the left, top, right, bottom coordinates (inside the margins)
;;; of the rectangle enclosing the specified item, including one bit of
;;; margin all around, or NIL if scrolled off the display.
;;; Note that because of the one bit of margin, returned values can be outside
;;; the window.
(DEFMETHOD (BASIC-MENU :ITEM-RECTANGLE) (ITEM &AUX (X 0) SWIDTH (ALEN (ARRAY-LENGTH ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)    ;last row on screen
                     ALEN))                     ;last row that exists
            (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (WHEN (AND (MEMBER ITEM (AREF ROW-MAP ROW))
               (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW))))))
      (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
          (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH)
                X (+ (* (POSITION ITEM (AREF ROW-MAP ROW) :TEST #'EQUAL) COLUMN-WIDTH)
                     (TRUNCATE (- COLUMN-WIDTH MENU-INTERCOLUMN-SPACING SWIDTH) 2)))
        (DOLIST (IT (AREF ROW-MAP ROW))
          (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH IT))
          (AND (EQ IT ITEM) (RETURN))
          (SETQ X (+ X SWIDTH MENU-INTERWORD-SPACING))))
      (RETURN (VALUES (1- X)
                      (1- (* (- ROW TOP-ROW) ROW-HEIGHT))
                      (+ X SWIDTH 1)
                      (- (* (1+ (- ROW TOP-ROW)) ROW-HEIGHT) 2))))))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:02
;; When we move a menu to a spot, make it go so that the last item chosen
;; appears at that spot.
(DEFMETHOD (BASIC-MENU :CENTER-AROUND) (X Y &AUX (XI 0) (YI 0))
  (AND (VARIABLE-BOUNDP LAST-ITEM)
       (MEMBER LAST-ITEM ITEM-LIST)
       ;; If we remember a previous choice,
       ;; let XI and YI get the offsets from that item to the center.
       (MULTIPLE-VALUE-BIND (X1 Y1)
           (SEND SELF :ITEM-CURSORPOS LAST-ITEM)
         (AND X1 Y1
              (SETQ XI (- (TRUNCATE WIDTH 2) X1 (SHEET-INSIDE-LEFT))
                    YI (- (TRUNCATE HEIGHT 2) Y1 (SHEET-INSIDE-TOP))))))
  (MULTIPLE-VALUE-BIND (X1 Y1)
      (CENTER-WINDOW-AROUND SELF (+ X XI) (+ Y YI))
    (VALUES (- X1 XI) (- Y1 YI))))

(DEFMETHOD (BASIC-MENU :COLUMN-ROW-SIZE) ()
  (VALUES COLUMN-WIDTH ROW-HEIGHT))

;; Permanent menus for giving "keyboard" commands from a menu alist
(DEFFLAVOR COMMAND-MENU-MIXIN (IO-BUFFER) ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:SETTABLE-INSTANCE-VARIABLES IO-BUFFER))

(DEFMETHOD (COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS) (BD IGNORE IGNORE)
  (WHEN CHOSEN-ITEM
    (if (not (IO-BUFFER-PUT IO-BUFFER `(:MENU ,CHOSEN-ITEM ,BD ,SELF) t))
        (send self :beep))
    (SETQ CHOSEN-ITEM NIL)))

(DEFFLAVOR COMMAND-MENU () (COMMAND-MENU-MIXIN MENU))

(DEFFLAVOR COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN () ()
  (:REQUIRED-FLAVORS COMMAND-MENU)
  (:DOCUMENTATION "Automatically clicks on the ABORT item if the menu is deexposed"))

(DEFMETHOD (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN :BEFORE :DEEXPOSE) (&REST IGNORE)
  (IF EXPOSED-P
      (DOLIST (ITEM ITEM-LIST)
        (IF (STRING-EQUAL (MENU-ITEM-STRING ITEM) "ABORT")
            (RETURN (IO-BUFFER-PUT IO-BUFFER `(:MENU ,ITEM 1 ,SELF)))))))

(DEFFLAVOR MENU-HIGHLIGHTING-MIXIN ((HIGHLIGHTED-ITEMS NIL)) ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:GETTABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:INITABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:DOCUMENTATION "Provides for highlighting of items with inverse video"))

; This does not remember it on the list, you probably don't want to use it yourself
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHT-ITEM) (ITEM)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM) (SEND SELF :ITEM-RECTANGLE ITEM)
    (AND (NOT (NULL LEFT))
         (PREPARE-SHEET (SELF)                  ;Clip but allow extension into margins
           (SETQ LEFT (MAX (+ LEFT (SHEET-INSIDE-LEFT)) 0)
                 RIGHT (MIN (+ RIGHT (SHEET-INSIDE-LEFT)) WIDTH)
                 TOP (MAX (+ TOP (SHEET-INSIDE-TOP)) 0)
                 BOTTOM (MIN (+ BOTTOM (SHEET-INSIDE-TOP)) HEIGHT))
           (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ALU-XOR SELF)))))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:03
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-ITEM) (ITEM)
  (UNLESS (MEMBER ITEM HIGHLIGHTED-ITEMS)
    (PUSH ITEM HIGHLIGHTED-ITEMS)
    (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM))))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:04
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-ITEM) (ITEM)
  (WHEN (MEMBER ITEM HIGHLIGHTED-ITEMS)
    (SETQ HIGHLIGHTED-ITEMS (REMOVE ITEM HIGHLIGHTED-ITEMS))
    (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM))))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:04
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-ITEMS) (NEW-HIGHLIGHTED-ITEMS &AUX OLD)
  (SETQ OLD HIGHLIGHTED-ITEMS
        HIGHLIGHTED-ITEMS NEW-HIGHLIGHTED-ITEMS)
  (SHEET-FORCE-ACCESS (SELF T)
    (DOLIST (X OLD)
      (OR (MEMBER X NEW-HIGHLIGHTED-ITEMS) (SEND SELF :HIGHLIGHT-ITEM X)))
    (DOLIST (X NEW-HIGHLIGHTED-ITEMS)
      (OR (MEMBER X OLD) (SEND SELF :HIGHLIGHT-ITEM X)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :AFTER :MENU-DRAW) ()
  (DOLIST (X HIGHLIGHTED-ITEMS)
    (SEND SELF :HIGHLIGHT-ITEM X)))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHTED-VALUES) ()
  (MAPCAR (LAMBDA (X) (SEND SELF ':EXECUTE-NO-SIDE-EFFECTS X)) HIGHLIGHTED-ITEMS))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-VALUES) (VALUES &AUX ITEMS)
  (DOLIST (ITEM ITEM-LIST)
    (AND (SI:MEMBER-EQUAL (SEND SELF :EXECUTE-NO-SIDE-EFFECTS ITEM) VALUES)
         (PUSH ITEM ITEMS)))
  (OR (= (LENGTH ITEMS) (LENGTH VALUES))
      (FERROR "Missing or duplicate value"))
  (SEND SELF :SET-HIGHLIGHTED-ITEMS ITEMS))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR "Value not found"))
    (AND (EQUAL (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
         (RETURN (SEND SELF :ADD-HIGHLIGHTED-ITEM (CAR L))))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR "Value not found"))
    (AND (EQUAL (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
         (RETURN (SEND SELF :REMOVE-HIGHLIGHTED-ITEM (CAR L))))))


(DEFFLAVOR MENU-MARGIN-CHOICE-MIXIN () (MARGIN-CHOICE-MIXIN)
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:DOCUMENTATION "Puts choice boxes in the bottom margin of a menu.
Clicking on a choice box simulates clicking on a menu item")
  (:INIT-KEYWORDS :MENU-MARGIN-CHOICES))

;An element of :MENU-MARGIN-CHOICES is just like an element of :ITEM-LIST

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETQ MARGIN-CHOICES
        (MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM (GET INIT-PLIST ':MENU-MARGIN-CHOICES))))

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :SET-MENU-MARGIN-CHOICES) (LIST)
  (SEND SELF :SET-MARGIN-CHOICES (MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM LIST)))

(DEFUN MENU-MARGIN-CHOICE-FROM-ITEM (X)
  (DECLARE (:SELF-FLAVOR MENU-MARGIN-CHOICE-MIXIN))
  (LIST (MENU-ITEM-STRING X NIL) NIL 'MENU-MARGIN-CHOICE-FUNCTION NIL NIL X))

(DEFUN MENU-MARGIN-CHOICE-FUNCTION (CHOICE-BOX REGION Y-POS)
  (DECLARE (:SELF-FLAVOR MENU-MARGIN-CHOICE-MIXIN))
  (DECLARE (IGNORE REGION Y-POS))
  (SETQ CHOSEN-ITEM (SIXTH CHOICE-BOX)))

;Really we want a MAX form of method combination for this operation.
(DEFWRAPPER (MENU-MARGIN-CHOICE-MIXIN :MINIMUM-WIDTH) (IGNORE . BODY)
  `(MAX (PROGN . ,BODY)
        (SEND SELF :MARGIN-CHOICES-MINIMUM-WIDTH)))

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :MARGIN-CHOICES-MINIMUM-WIDTH) ()
  (DO ((W 0)
       (CHOICES MARGIN-CHOICES (CDR CHOICES)))
      ((NULL CHOICES)
       (+ W (* (LENGTH MARGIN-CHOICES)
               (+ (* 2 (FONT-CHAR-WIDTH CURRENT-FONT))
                  (FONT-BLINKER-HEIGHT CURRENT-FONT)))))
    (INCF W (MENU-ITEM-STRING-WIDTH (CAAR CHOICES)))))

(DEFFLAVOR MARGIN-CHOICE-MENU ((LABEL NIL))
           (BASIC-MENU BORDERS-MIXIN MENU-MARGIN-CHOICE-MIXIN
            TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION "An instantiable menu with choice boxes at bottom.
Otherwise like an ordinary menu.  This example shows how to mix in
the flavor MENU-MARGIN-CHOICE-MIXIN."))

(DEFFLAVOR MOMENTARY-MARGIN-CHOICE-MENU
        ((LABEL NIL))
        (BASIC-MOMENTARY-MENU TEMPORARY-WINDOW-MIXIN
;Changed to share more combined methods.
         MARGIN-CHOICE-MENU)
;        BORDERS-MIXIN MENU-MARGIN-CHOICE-MIXIN
;        TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION "Momentary menu with choice boxes at bottom.
This is a nontrivial flavor combination because MENU-MARGIN-CHOICE-MIXIN
must be ordered properly with respect to other flavors."))

(DEFFLAVOR MULTIPLE-MENU-MIXIN (SPECIAL-CHOICE-ITEMS) (MENU-HIGHLIGHTING-MIXIN)
  (:INIT-KEYWORDS :SPECIAL-CHOICES)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
                       :SPECIAL-CHOICES '(("Do It"
                                           :EVAL (VALUES (SEND SELF :HIGHLIGHTED-VALUES)
                                                         T))))
  (:DOCUMENTATION "A menu in which you can select more than one choice.
 HIGHLIGHTED-ITEMS is a list of those items in the ITEM-LIST that are currently
 selected.  SPECIAL-CHOICES are those items that don't highlight when
 you click on them, but instead are executed in the usual way.  The default
 special choice is just Done, which returns a list of the values of the highlighted
 items.  SPECIAL-CHOICES are displayed in italics at the top of the menu."))

;;; Insert the special-choices into the item-list
;;; Buglet - if n-columns is not specified explicitly, and turns out to be more than 1,
;;; there will not be automatic blank space inserted to keep the special-choices on
;;; a separate row.  There is probably nothing wrong with this.
(DEFUN MULTIPLE-MENU-HACK-ITEM-LIST (ITM-LST &OPTIONAL N-COLUMNS)
  (DECLARE (:SELF-FLAVOR MULTIPLE-MENU-MIXIN))
  (SETQ N-COLUMNS (OR N-COLUMNS (CAR GEOMETRY) 1))
  (APPEND SPECIAL-CHOICE-ITEMS
          (AND N-COLUMNS (> N-COLUMNS 1)
               (DO ((N (\ (LENGTH SPECIAL-CHOICE-ITEMS) N-COLUMNS) (1+ N))
                    (L NIL (CONS '("" :NO-SELECT NIL) L)))
                   ((OR (ZEROP N) (= N N-COLUMNS)) L)))
          ITM-LST))

(DEFMETHOD (MULTIPLE-MENU-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETQ SPECIAL-CHOICE-ITEMS
        (MAPCAR (LAMBDA (X)
                  (APPEND (COND ((ATOM X) (LIST X ':VALUE X))
                                ((ATOM (CDR X)) (LIST (CAR X) ':VALUE (CDR X)))
                                ((NULL (CDDR X)) (LIST (CAR X) ':VALUE (CADR X)))
                                (T X))
                          '(:FONT FONTS:HL12I :SPECIAL-CHOICE T)))
                (GET INIT-PLIST ':SPECIAL-CHOICES)))
  (AND (VARIABLE-BOUNDP ITEM-LIST)      ;Only if items specified in init-plist
       (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST ITEM-LIST (GET INIT-PLIST ':COLUMNS)))))

(DEFMETHOD (MULTIPLE-MENU-MIXIN :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SEND SELF :SET-HIGHLIGHTED-ITEMS NIL)
  (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST NEW-ITEM-LIST)
        LAST-ITEM NIL
        CURRENT-ITEM NIL)
  (MENU-COMPUTE-GEOMETRY T)             ;Recompute parameters, and redraw menu
  ITEM-LIST)

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:06
;Modified mouse-button handler.  Does normal thing for special-choices, otherwise
;just complements highlight state.
(DEFWRAPPER (MULTIPLE-MENU-MIXIN :MOUSE-BUTTONS-ON-ITEM) (IGNORE . BODY)
  `(LET ((ITEM CURRENT-ITEM))
     (COND ((AND (NOT (ATOM ITEM))              ;Special-choice selected?
                 (NOT (ATOM (CDR ITEM)))
                 (GET (CDDR ITEM) ':SPECIAL-CHOICE))
            . ,BODY)                            ;Yes, do normal action
           (T                                   ;Ordinary item, highlight or un-highlight it
            (SEND SELF (IF (MEMBER ITEM HIGHLIGHTED-ITEMS)
                           :REMOVE-HIGHLIGHTED-ITEM
                           :ADD-HIGHLIGHTED-ITEM)
                       ITEM)))))

;(DEFFLAVOR MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MENU))

;(DEFFLAVOR MOMENTARY-MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MOMENTARY-MENU))

(DEFFLAVOR MULTIPLE-MENU () (MARGIN-MULTIPLE-MENU-MIXIN MARGIN-CHOICE-MENU))

(DEFFLAVOR MOMENTARY-MULTIPLE-MENU
        () (MARGIN-MULTIPLE-MENU-MIXIN MOMENTARY-MARGIN-CHOICE-MENU))

(DEFFLAVOR MARGIN-MULTIPLE-MENU-MIXIN () (MENU-HIGHLIGHTING-MIXIN)
  (:REQUIRED-FLAVORS MENU-MARGIN-CHOICE-MIXIN)
  (:INIT-KEYWORDS :SPECIAL-CHOICES)
  (:DEFAULT-INIT-PLIST :SPECIAL-CHOICES '(("Do It"
                                           :EVAL (VALUES (SEND SELF :HIGHLIGHTED-VALUES)
                                                         T))))
  (:DOCUMENTATION "A menu in which you can select more than one choice.
 HIGHLIGHTED-ITEMS is a list of those items in the ITEM-LIST that are currently
 selected.  SPECIAL-CHOICES are those items that don't highlight when
 you click on them, but instead are executed in the usual way.
 They go in choice boxes in the bottom margin."))

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :BEFORE :SET-ITEM-LIST) (IGNORE)
  (SEND SELF :SET-HIGHLIGHTED-ITEMS NIL))

;This is like setting the item list, but we do not unhighlight
;any of the existing items in the menu.
(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :ADD-ITEM) (NEW-ITEM)
  (UNLESS (SI:MEMBER-EQUAL NEW-ITEM ITEM-LIST)
    (SETQ ITEM-LIST (NCONC ITEM-LIST (LIST NEW-ITEM)))
    (SETQ LAST-ITEM NIL CURRENT-ITEM NIL)
    (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))
  NEW-ITEM)

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#113 on 30-Mar-87 13:14:08
(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :MOUSE-BUTTONS-ON-ITEM) (IGNORE)
  (SEND SELF (IF (MEMBER CURRENT-ITEM HIGHLIGHTED-ITEMS)
                 :REMOVE-HIGHLIGHTED-ITEM
                 :ADD-HIGHLIGHTED-ITEM)
             CURRENT-ITEM))

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (PUTPROP INIT-PLIST (GET INIT-PLIST ':SPECIAL-CHOICES) ':MENU-MARGIN-CHOICES))

(DEFFLAVOR BASIC-MOMENTARY-MENU () (HYSTERETIC-WINDOW-MIXIN BASIC-MENU)
  (:DOCUMENTATION "A menu that holds control of the mouse.
Menus of this type handle the mouse for a small area outside of their
actual edges.  They also are automatically deactivated whenever an item
is chosen or the mouse moves even further, out of its control."))

(DEFMETHOD (BASIC-MOMENTARY-MENU :AROUND :CHOOSE) (CONT MT ARGS)
  (BLOCK NIL
    (LET ((X MOUSE-X) (Y MOUSE-Y) (SUCCESS T))
      (UNWIND-PROTECT
          (PROGN (CATCH 'ABORT
                   (RETURN (AROUND-METHOD-CONTINUE CONT MT ARGS)))
                 (SETQ SUCCESS NIL))
        (WHEN SUCCESS
          (MOUSE-WARP X Y))))))

(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :CHOOSE) ()
  (WHEN (NOT EXPOSED-P)
    (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
        (SHEET-CALCULATE-OFFSETS SUPERIOR MOUSE-SHEET)
      (MULTIPLE-VALUE-BIND (X Y)
          (SEND SELF :CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
        (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF))))
    ;; Expose self, and seize the mouse.
    (WITH-MOUSE-GRABBED
      (SEND SELF :EXPOSE)
      (UNLESS (EQ SELF (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y
                                                 NIL :EXPOSED))
        (SEND SELF :DEACTIVATE)
        (THROW 'ABORT NIL)))))

;;; When no selection, but mouse moved out of range, deexpose menu
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :HANDLE-MOUSE) ()
  (OR CHOSEN-ITEM
      ;; Don't flush if mouse being usurped
      WINDOW-OWNING-MOUSE
      ;; Only flush us if either not explicitly flushing or we don't own mouse
      (AND MOUSE-RECONSIDER (EQ SELF (WINDOW-OWNING-MOUSE)))
      ;; This is called in the mouse process.  We don't want to take the chance that
      ;; we might go blocked, so run in another process.
      (PROCESS-RUN-FUNCTION '(:NAME "Menu Deactivate" :PRIORITY 20.) SELF ':DEACTIVATE)))

;;; Make MOUSE-DEFAULT-HANDLER return so menu gets deactivated.
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :MOUSE-BUTTONS) (IGNORE IGNORE IGNORE)
  (AND CHOSEN-ITEM (SETQ MOUSE-RECONSIDER T)))

;; Get here if either 1) user clicks on an item or 2) menu is deactivated.
(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :EXECUTE) (ITEM &REST IGNORE)
  (SEND SELF :DEACTIVATE)
  (UNLESS ITEM (THROW 'ABORT NIL)))

(DEFFLAVOR WINDOW-HACKING-MENU-MIXIN (WINDOW-UNDER-MENU OLD-X OLD-Y) ()
  (:DOCUMENTATION "Menu which handles :WINDOW-OP when called over another window
The window that the menu is exposed over is remembered when the :choose message is sent,
and then used if a :window-op type item is selected."))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :BEFORE :CHOOSE) ()
  (SETQ WINDOW-UNDER-MENU (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y)
        OLD-X MOUSE-X
        OLD-Y MOUSE-Y))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION)
  (FUNCALL FUNCTION WINDOW-UNDER-MENU OLD-X OLD-Y))

(DEFFLAVOR ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN () ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:REQUIRED-METHODS :UPDATE-ITEM-LIST)
  (:DEFAULT-INIT-PLIST :ITEM-LIST NIL)
  (:DOCUMENTATION "Allows the menu to have an item list that's being dynamically
modified.  Causes the menu's item list to be updated at appropriate times.
The actual item list is computed via the :UPDATE-ITEM-LIST message."))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CHOOSE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :MOVE-NEAR-WINDOW) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CENTER-AROUND) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :PANE-SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))


(DEFFLAVOR DYNAMIC-ITEM-LIST-MIXIN ((ITEM-LIST-POINTER NIL))
           (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN)
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION "Allows the menu to have an item list that's being dynamically
modified.  Causes the menu's item list to be updated at appropriate times.
The ITEM-LIST-POINTER instance variable is a form to be evaluated to get the item list."))

(DEFUN DYNAMIC-ITEM-LIST ()
  (DECLARE (:SELF-FLAVOR DYNAMIC-ITEM-LIST-MIXIN))
  (IF (SYMBOLP ITEM-LIST-POINTER)
      (SYMBOL-VALUE ITEM-LIST-POINTER)
    (EVAL ITEM-LIST-POINTER)))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :INIT) (IGNORE)
  (AND ITEM-LIST-POINTER
       (SETQ ITEM-LIST (DYNAMIC-ITEM-LIST))))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :UPDATE-ITEM-LIST) (&AUX NEW-ITEM-LIST)
  (AND ITEM-LIST-POINTER
       (OR (EQUAL ITEM-LIST (SETQ NEW-ITEM-LIST (DYNAMIC-ITEM-LIST)))
           (SEND SELF :SET-ITEM-LIST NEW-ITEM-LIST))))


(DEFFLAVOR DYNAMIC-MULTICOLUMN-MIXIN (COLUMN-SPEC-LIST PREVIOUS-STATE)
           (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES COLUMN-SPEC-LIST)
  (:GETTABLE-INSTANCE-VARIABLES COLUMN-SPEC-LIST)
  (:DOCUMENTATION "Makes a menu have multiple 'dynamic' columns.
Each column comes from a separate item-list which is recomputed at appropriate times.
The instance variable COLUMN-SPEC-LIST is a list of columns, each column consists
of (heading item-list-form . options).  Heading is a string to go at the top of the
column, and options are menu-item options for it (typically font).  item-list-form is
a form to be evaluated (without side-effects) to get the item list for that column."))

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :BEFORE :INIT) (IGNORE)
  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH COLUMN-SPEC-LIST))))

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :UPDATE-ITEM-LIST) (&OPTIONAL FORCE)
  (IF (OR FORCE
          (LOOP FOR (HEADING FORM) IN COLUMN-SPEC-LIST
                AND OLD-ITEM-LIST IN PREVIOUS-STATE
                THEREIS (NEQ (IF (SYMBOLP FORM) (SYMEVAL FORM) (EVAL FORM)) OLD-ITEM-LIST)))
      ;; Something has changed, set up new item list.
      ;; Start by extracting the column lists and setting up the headings.
      (LOOP FOR (HEADING FORM . OPTIONS) IN COLUMN-SPEC-LIST
            AND STATEL ON PREVIOUS-STATE
            COLLECT `(,HEADING :NO-SELECT T . ,OPTIONS) INTO NEW-ITEM-LIST
            COLLECT (IF (SYMBOLP FORM) (SYMEVAL FORM) (EVAL FORM)) INTO COLUMN-VALUES
            FINALLY
        ;; Now interleave the columns, and save the old state.
        (SETQ NEW-ITEM-LIST (NREVERSE NEW-ITEM-LIST))
        (LOOP FOR C IN COLUMN-VALUES AND L ON PREVIOUS-STATE DO (RPLACA L C))
        (LOOP REPEAT (LOOP FOR C IN COLUMN-VALUES MAXIMIZE (LENGTH C))
              DO (LOOP FOR L ON COLUMN-VALUES
                       DO (PUSH (OR (CAAR L) `("" :NO-SELECT T)) NEW-ITEM-LIST)
                          (RPLACA L (CDAR L))))
        (OR (EQ (CAR GEOMETRY) (LENGTH COLUMN-SPEC-LIST))
            (SEND SELF :SET-GEOMETRY (LENGTH COLUMN-SPEC-LIST)))
        (SEND SELF :SET-ITEM-LIST (NREVERSE NEW-ITEM-LIST)))))

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :SET-COLUMN-SPEC-LIST) (NEW-COLUMN-SPEC-LIST)
  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH NEW-COLUMN-SPEC-LIST)))
  (SETQ COLUMN-SPEC-LIST NEW-COLUMN-SPEC-LIST)
  (SEND SELF :UPDATE-ITEM-LIST T))

;;; This is a bit of a kludge.  It is necessary because this method is not
;;; loaded before the COMPILE-FLAVOR-METHODS is done, and therefore the
;;; flavor system assumes that the method has been deleted and recompiles
;;; the combined method.  So we tell it that there is going to be a method,
;;; but the actual code for the method is in FRAME.
(SI:FLAVOR-NOTICE-METHOD '(:METHOD BASIC-MENU :PANE-SIZE))


;;; Menus to be used for a momentary choice.
;;; Send a menu of this type a :CHOOSE message to use the menu.
;;; When the user selects an item, or moves the mouse off the menu,
;;; the menu will disappear, and whatever was underneath it will reappear.
;;; It will return the chosen item, or NIL.  If the item is not atomic
;;; and its cadr is non-NIL, the cadr will be called with no arguments.
;;; In this case, if the caddr of the item is also non-nil,
;;; no windows will be re-exposed before the cadr is called.
(DEFFLAVOR MOMENTARY-MENU ((LABEL NIL)) (BASIC-MOMENTARY-MENU TEMPORARY-WINDOW-MIXIN
                                         BORDERS-MIXIN TOP-BOX-LABEL-MIXIN
                                         BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION "Temporary menu that goes away after item is chosen"))

;; Copied from LAD: RELEASE-3.WINDOW; MENU.LISP#112 on 2-Oct-86 04:16:19
; Don't complain if selected
(defmethod (momentary-menu :select) (&optional ignore))


(DEFFLAVOR MOMENTARY-WINDOW-HACKING-MENU () (WINDOW-HACKING-MENU-MIXIN MOMENTARY-MENU)
  #|(:DOCUMENTATION)|#)

(DEFFLAVOR DYNAMIC-MOMENTARY-MENU () (DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-MENU))
(DEFFLAVOR DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
        ()
        (DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-WINDOW-HACKING-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-MENU () (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-COMMAND-MENU ()
           (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-WINDOW-MIXIN COMMAND-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-ABORT-ON-DEEXPOSE-COMMAND-MENU ()
           (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN DYNAMIC-POP-UP-COMMAND-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-MENU () (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-COMMAND-MENU ()
           (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-WINDOW-MIXIN COMMAND-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU ()
           (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN DYNAMIC-TEMPORARY-COMMAND-MENU))
(DEFFLAVOR DYNAMIC-MULTICOLUMN-MOMENTARY-MENU ()
           (DYNAMIC-MULTICOLUMN-MIXIN MOMENTARY-MENU))
(DEFFLAVOR DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU ()
           (WINDOW-HACKING-MENU-MIXIN DYNAMIC-MULTICOLUMN-MOMENTARY-MENU))

(DEFUN MENU-CHOOSE (ALIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE)) DEFAULT-ITEM
                    (SUPERIOR MOUSE-SHEET))
  "Let user choose an element of ALIST with a menu.
ALIST looks like an ITEM-LIST for a menu.
LABEL is used as the label of the menu, if not NIL.
NEAR-MODE specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
DEFAULT-ITEM is an element of ALIST; the mouse will be positioned
initially over that item.  Or it can be NIL.
The menu is made an inferior of SUPERIOR.

If the user chooses an item, the values are 1) the value computed
from that item, and 2) the item itself (an element of ALIST).
Otherwise, NIL is returned."
  (WHEN ALIST
    (AND (EQ (CAR NEAR-MODE) ':WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
    (USING-RESOURCE (MENU MOMENTARY-MENU SUPERIOR)
      (SEND MENU :SET-ITEM-LIST ALIST)
      (SEND MENU :SET-LABEL LABEL)
      (SEND MENU :SET-LAST-ITEM DEFAULT-ITEM)
      (send menu :expose-near near-mode nil)            ;do not warp the mouse yet.
      (multiple-value-bind (x y)
        (send menu :item-cursorpos (or default-item (car alist)))
        (when (and x y)
          (send menu :set-mouse-position (+ x (sheet-inside-left menu))
                (+ y (sheet-inside-top menu)))))
      (VALUES (SEND MENU :CHOOSE)
              (SEND MENU :LAST-ITEM)))))

(DEFUN MULTIPLE-MENU-CHOOSE (ALIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE))
                             HIGHLIGHTED-ITEMS
                             (SUPERIOR MOUSE-SHEET))
  "Let user choose some set of elements of ALIST with a multiple menu.
ALIST looks like an ITEM-LIST for a menu.
LABEL is used as the label of the menu, if not NIL.
NEAR-MODE specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
HIGHLIGHTED-ITEMS is a set of elements of ALIST;
 these will form the initially chosen subset, which the user can modify.
The menu is made an inferior of SUPERIOR.

If the user exits by clicking on the Do It box, there are two values:
1) a list of the values from executing the selected set of items, and
2) T.
If the user moves the mouse away from the menu,
NIL is returned for both values."
  (WHEN ALIST
    (AND (EQ (CAR NEAR-MODE) ':WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
    (USING-RESOURCE (MENU MOMENTARY-MULTIPLE-MENU SUPERIOR)
      (SEND MENU :SET-LABEL LABEL)
      (SEND MENU :SET-ITEM-LIST ALIST)
      (SEND MENU :SET-HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS)
      (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
      (SEND MENU :CHOOSE)
      )))

;All types of menus, since they probably all get used
(COMPILE-FLAVOR-METHODS MENU TEMPORARY-MENU COMMAND-MENU MOMENTARY-MENU
                        MARGIN-CHOICE-MENU MULTIPLE-MENU MOMENTARY-MULTIPLE-MENU
                        MOMENTARY-WINDOW-HACKING-MENU
                        DYNAMIC-MOMENTARY-MENU DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
                        DYNAMIC-TEMPORARY-MENU DYNAMIC-TEMPORARY-COMMAND-MENU
                        DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU
                        DYNAMIC-MULTICOLUMN-MOMENTARY-MENU
                        DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU)

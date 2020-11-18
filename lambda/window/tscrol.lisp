;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFFLAVOR TEXT-SCROLL-WINDOW
       ((ITEMS NIL)                             ;An array of all items
        (TOP-ITEM 0)                            ;The index of the topmost displayed item
        (ITEM-GENERATOR NIL)
        )
       ()
  (:REQUIRED-FLAVORS BASIC-SCROLL-BAR)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL)
  (:DOCUMENTATION "Scrolling of lines all of one type"))

;;;; The item-generator feature:

;;; If the ITEM-GENERATOR variable is non-nil, then it replaces the array ITEMS
;;; in remembering what items we are displaying.  It should be a function which
;;; understands these operations:
;;;  :NUMBER-OF-ITEMS              returns the number of items being displayed.
;;;  :NUMBER-OF-ITEM item-value    returns the item-number of that item-value.
;;;  :ITEM-OF-NUMBER item-number   returns the item-value at that item number.
;;; The next two are optional, needed only if you want to use the :INSERT-ITEM,
;;;  :DELETE-ITEM and :APPEND-ITEM operations on the window, which many applications
;;;  for item generators will not need to do:
;;;  :INSERT-ITEM item-number item-value   inserts the item-value at that item number.
;;;  :DELETE-ITEM item-number              deletes the item at that item number.

;;; If ITEM-GENERATOR is nil, the variable ITEMS contains an array (with fill pointer)
;;; containing all the items.

(DEFMETHOD (TEXT-SCROLL-WINDOW :BEFORE :INIT) (PLIST)
  (DECLARE (IGNORE PLIST))
  (TYPECASE ITEMS
    (ARRAY)
    (CONS
     (SETQ ITEMS (MAKE-ARRAY (LENGTH ITEMS)
                             :FILL-POINTER (LENGTH ITEMS)
                             :INITIAL-CONTENTS ITEMS)))
    (T
     (SETQ ITEMS (MAKE-ARRAY (OR ITEMS 100.) :FILL-POINTER 0)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :MORE-EXCEPTION) ()
  (SETF (SHEET-MORE-FLAG) 0))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SET-ITEMS) (NEW-ITEMS &OPTIONAL NO-DISPLAY)
  (SETQ ITEM-GENERATOR NIL)
  (SETQ ITEMS NEW-ITEMS)
  (TYPECASE ITEMS
    (ARRAY)
    (CONS
     (SETQ ITEMS (MAKE-ARRAY (LENGTH ITEMS)
                             :FILL-POINTER (LENGTH ITEMS)
                             :INITIAL-CONTENTS ITEMS)))
    (T
     (SETQ ITEMS (MAKE-ARRAY (OR ITEMS 100.) :FILL-POINTER 0))))
  (UNLESS NO-DISPLAY
    (SHEET-FORCE-ACCESS (SELF T)
      (SEND SELF :CLEAR-WINDOW)
      (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SET-ITEM-GENERATOR) (NEW-ITEM-GENERATOR)
  (SETQ ITEM-GENERATOR NEW-ITEM-GENERATOR)
  (SHEET-FORCE-ACCESS (SELF T)
    (SEND SELF :CLEAR-WINDOW)
    (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :LAST-ITEM) ()
  (LET ((NITEMS (SEND SELF :NUMBER-OF-ITEMS)))
    (IF (PLUSP NITEMS)
        (SEND SELF :ITEM-OF-NUMBER (1- NITEMS)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :NUMBER-OF-ITEMS) ()
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :NUMBER-OF-ITEMS)
    (LENGTH ITEMS)))

(DEFMETHOD (TEXT-SCROLL-WINDOW :NUMBER-OF-ITEM) (ITEM)
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :NUMBER-OF-ITEM ITEM)
    (DOTIMES (I (LENGTH ITEMS))
      (AND (EQ (AREF ITEMS I) ITEM) (RETURN I)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :ITEM-OF-NUMBER) (NUMBER)
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :ITEM-OF-NUMBER NUMBER)
    (AREF ITEMS NUMBER)))

(DEFMETHOD (TEXT-SCROLL-WINDOW :PUT-LAST-ITEM-IN-WINDOW) ()
  (OR ( (SEND SELF :NUMBER-OF-ITEMS)
         (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES) -1))
      ;; Last item not on screen -- put it on bottom line
      (SEND SELF :SCROLL-TO (- (SEND SELF :NUMBER-OF-ITEMS) (SHEET-NUMBER-OF-INSIDE-LINES))
                            :ABSOLUTE)))

(DEFMETHOD (TEXT-SCROLL-WINDOW :PUT-ITEM-IN-WINDOW) (ITEM)
  ;; If item not visible, put it in the window; if off the top, bring it to the
  ;; top.  If off the bottom, bring it to the bottom.
  (LET ((ITEM-NO (SEND SELF :NUMBER-OF-ITEM ITEM))
        (BOTTOM-ITEM (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES) -1)))
    (COND ((NULL ITEM-NO))
          ((< ITEM-NO TOP-ITEM)
           (SEND SELF :SCROLL-TO ITEM-NO :ABSOLUTE))
          ((> ITEM-NO BOTTOM-ITEM)
           (SEND SELF :SCROLL-TO (- ITEM-NO (- BOTTOM-ITEM TOP-ITEM)) :ABSOLUTE)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :APPEND-ITEM) (NEW-ITEM)
  (SEND SELF :INSERT-ITEM (SEND SELF :NUMBER-OF-ITEMS) NEW-ITEM))

(DEFMETHOD (TEXT-SCROLL-WINDOW :DELETE-ITEM) (ITEM-NO &AUX I)
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :DELETE-ITEM ITEM-NO)   ;Probably gets an error.
    (DECF (FILL-POINTER ITEMS))
    (DO ((I ITEM-NO (1+ I)))
        (( I (LENGTH ITEMS)))
      (SETF (AREF ITEMS I) (AREF ITEMS (1+ I))))
    (COND ((< ITEM-NO TOP-ITEM)
           (SETQ TOP-ITEM (1- TOP-ITEM))
           (SEND SELF :NEW-SCROLL-POSITION))
          ((< ITEM-NO (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES)))
           ;; Old item was on the screen -- flush it
           (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
             (SHEET-SET-CURSORPOS SELF 0 (* LINE-HEIGHT (- ITEM-NO TOP-ITEM)))
             (SEND SELF :DELETE-LINE 1)
             (SEND SELF :REDISPLAY (SETQ I (1- (SHEET-NUMBER-OF-INSIDE-LINES))) (1+ I))))
          (T (SEND SELF :NEW-SCROLL-POSITION))))
  ITEM-NO)

(DEFMETHOD (TEXT-SCROLL-WINDOW :INSERT-ITEM) (ITEM-NO NEW-ITEM)
  "Inserts an item before ITEM-NO"
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :INSERT-ITEM ITEM-NO NEW-ITEM)
    (LET ((NO-ITEMS (FILL-POINTER ITEMS)))
      (SETQ ITEM-NO (MIN (MAX ITEM-NO 0) NO-ITEMS))
      (ARRAY-PUSH-EXTEND ITEMS NIL)
      (DOTIMES (I (- NO-ITEMS ITEM-NO))
        ;; Bubble items up
        (SETF (AREF ITEMS (- NO-ITEMS I)) (AREF ITEMS (- NO-ITEMS I 1))))
      (SETF (AREF ITEMS ITEM-NO) NEW-ITEM)
      (COND ((< ITEM-NO TOP-ITEM)
             (SETQ TOP-ITEM (1+ TOP-ITEM))
             (SEND SELF :NEW-SCROLL-POSITION))
            ((< ITEM-NO (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES)))
             ;; New item is on screen, insert a line then redisplay it
             (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
               (SHEET-SET-CURSORPOS SELF
                                    0 (* LINE-HEIGHT (SETQ ITEM-NO (- ITEM-NO TOP-ITEM))))
               (SEND SELF :INSERT-LINE 1)
               (SEND SELF :REDISPLAY ITEM-NO (1+ ITEM-NO))))
            (T (SEND SELF :NEW-SCROLL-POSITION)))))
  ITEM-NO)

;;; When exposed, draw in the items
(DEFMETHOD (TEXT-SCROLL-WINDOW :AFTER :REFRESH) (&OPTIONAL TYPE)
  (AND (OR (NOT RESTORED-BITS-P) (EQ TYPE :SIZE-CHANGED))
       (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES))))

;;; Arguments are screen line indices -- assumes screen area already erased
(DEFMETHOD (TEXT-SCROLL-WINDOW :REDISPLAY) (START END)
  (DO ((I START (1+ I))
       (J (+ START TOP-ITEM) (1+ J))
       (LIM (SEND SELF :NUMBER-OF-ITEMS)))
      ((OR ( I END)
           (AND ( J LIM)
                ;; Displaying some items can create more,
                ;; so recheck how many items there are.
                ( J (SETQ LIM (SEND SELF :NUMBER-OF-ITEMS))))))
    (SHEET-SET-CURSORPOS SELF 0 (* LINE-HEIGHT I))
    (SEND SELF :PRINT-ITEM (SEND SELF :ITEM-OF-NUMBER J) I J))
  (SEND SELF :NEW-SCROLL-POSITION))

;;; Each item is allowed only one line
(DEFWRAPPER (TEXT-SCROLL-WINDOW :PRINT-ITEM) (IGNORE . BODY)
  `(CATCH 'LINE-OVERFLOW . ,BODY))

(DEFMETHOD (TEXT-SCROLL-WINDOW :END-OF-LINE-EXCEPTION) ()
  (THROW 'LINE-OVERFLOW T))

;;; Simplest printer, you want to redefine this probably
(DEFMETHOD (TEXT-SCROLL-WINDOW :PRINT-ITEM) (ITEM LINE-NO ITEM-NO)
  (DECLARE (IGNORE LINE-NO ITEM-NO))
  (PRIN1 ITEM SELF))

;;; Scrolling
(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-BAR-P) ()
  (OR (PLUSP TOP-ITEM)
      (> (SEND SELF :NUMBER-OF-ITEMS) (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-POSITION) ()
  (VALUES TOP-ITEM (SEND SELF :NUMBER-OF-ITEMS) LINE-HEIGHT))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-TO) (NEW-TOP TYPE &AUX DELTA)
  (AND (EQ TYPE :RELATIVE) (SETQ NEW-TOP (+ TOP-ITEM NEW-TOP)))
  (SETQ NEW-TOP (MAX (MIN NEW-TOP (1- (SEND SELF :NUMBER-OF-ITEMS))) 0))
  (SETQ DELTA (- NEW-TOP TOP-ITEM))
  (OR (= DELTA 0)                               ;Nothing to change
      (SEND SELF :SCROLL-REDISPLAY NEW-TOP DELTA))
  (SEND SELF :NEW-SCROLL-POSITION))

(DEFMETHOD (TEXT-SCROLL-WINDOW :AFTER :NEW-SCROLL-POSITION) (&REST IGNORE)
  (MOUSE-WAKEUP))

;;; This is a message so it can have daemons
(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-REDISPLAY) (NEW-TOP DELTA &AUX NLINES)
  (SHEET-HOME SELF)
  (SETQ NLINES (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND ((> DELTA 0)                            ;Scrolling forward
         (SETQ DELTA (MIN DELTA NLINES))
         (WITHOUT-INTERRUPTS
           (SEND SELF :DELETE-LINE DELTA)
           (SETQ TOP-ITEM NEW-TOP))
         (SEND SELF :REDISPLAY (- NLINES DELTA) NLINES))
        ((< DELTA 0)                            ;Scrolling backward
         (SETQ DELTA (MIN (- DELTA) NLINES))
         (WITHOUT-INTERRUPTS
           (SEND SELF :INSERT-LINE DELTA)
           (SETQ TOP-ITEM NEW-TOP))
         (SEND SELF :REDISPLAY 0 DELTA)))
  (SEND SELF :NEW-SCROLL-POSITION))

(DEFFLAVOR FUNCTION-TEXT-SCROLL-WINDOW
       (PRINT-FUNCTION                          ;Function called to print the item
        (PRINT-FUNCTION-ARG NIL)                ;Fixed argument for above
        )
       (TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES PRINT-FUNCTION PRINT-FUNCTION-ARG)
  (:DOCUMENTATION "Text scroll windows that print lines by calling a set function"))

;;;LIST is (print-function print-function-arg (items...) top-item-number label item-generator)
;;; Either the list of items or the item-generator will usually be nil.
(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :SETUP) (LIST)
  ;; Label changing should be first -- this may cause redisplay so flush current items too
  (WHEN ITEMS (setf (fill-pointer ITEMS) 0))
  (WHEN ( (LENGTH LIST) 5) (SEND SELF :SET-LABEL (FIFTH LIST)))
  (SEND SELF :SET-PRINT-FUNCTION (FIRST LIST))
  (SEND SELF :SET-PRINT-FUNCTION-ARG (SECOND LIST))
  (SETQ TOP-ITEM (OR (FOURTH LIST) 0))
  (IF (SIXTH LIST)
      (SEND SELF :SET-ITEM-GENERATOR (SIXTH LIST))
    (LET ((ARRAY (OR ITEMS (MAKE-ARRAY (LENGTH (THIRD LIST)) :FILL-POINTER 0))))
      (DOLIST (L (THIRD LIST)) (VECTOR-PUSH-EXTEND L ARRAY))
      (SEND SELF :SET-ITEMS ARRAY)))
  LIST)

(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :PRINT-ITEM) (ITEM IGNORE ITEM-NO)
  (CONDITION-BIND (((SYS:CELL-CONTENTS-ERROR)
                    #'(LAMBDA (COND)
                        (VALUES :NEW-VALUE
                                (FORMAT NIL "#<~S ~O>"
                                        (Q-DATA-TYPES (SEND COND :DATA-TYPE))
                                        (%POINTER (SEND COND :ADDRESS)))))))
    (FUNCALL PRINT-FUNCTION ITEM PRINT-FUNCTION-ARG SELF ITEM-NO)))


(DEFFLAVOR TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN () (WINDOW-WITH-TYPEOUT-MIXIN)
  (:REQUIRED-FLAVORS TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION "Makes a TEXT-SCROLL-WINDOW have a typeout window"))

(DEFUN TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT ()
  "If the typeout window is active, deexpose it, and make sure the redisplayer knows how many lines were clobbered."
  (DECLARE (:SELF-FLAVOR TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN))
  (WHEN (SEND TYPEOUT-WINDOW :ACTIVE-P)
    (LET ((BR (MIN (1- (SHEET-NUMBER-OF-INSIDE-LINES))
                   (1+ (TRUNCATE (SEND TYPEOUT-WINDOW :BOTTOM-REACHED)
                                 LINE-HEIGHT)))))
      (SEND TYPEOUT-WINDOW :DEACTIVATE)
      (SEND SELF :DRAW-RECTANGLE
            (SHEET-INSIDE-WIDTH) (* BR LINE-HEIGHT)
            0 0
            ALU-ANDCA)
      BR)))

(DEFWRAPPER (TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN :REDISPLAY) (ARGS . BODY)
  `(LET ((TO (TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT)))
     (WHEN TO
       (SETF (FIRST ARGS) 0)
       (SETF (SECOND ARGS) (MAX TO (SECOND ARGS))))
     . ,BODY))

(DEFMETHOD (TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN :FLUSH-TYPEOUT) ()
  (LET ((TO (TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT)))
    (WHEN TO (SEND SELF :REDISPLAY 0 TO))))

(DEFFLAVOR DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW
        (DISPLAYED-ITEMS                                ;An array of mouse sensitive items
          )
        (TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION "Keep track of displayed items on the screen.
We take care of everything in maintaining DISPLAYED-ITEMS except one:
the :PRINT-ITEM operation you define is responsible for storing the desired
data into the element of DISPLAYED-ITEMS for the line displayed."))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ DISPLAYED-ITEMS (MAKE-ARRAY (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE)
  (LET ((NLINES (SHEET-NUMBER-OF-INSIDE-LINES)))
    (AND (< (ARRAY-LENGTH DISPLAYED-ITEMS) NLINES)
         (ADJUST-ARRAY-SIZE DISPLAYED-ITEMS NLINES))))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :CHANGE-OF-DEFAULT-FONT)
           (&REST IGNORE)
  (LET ((NLINES (SHEET-NUMBER-OF-INSIDE-LINES)))
    (AND (< (ARRAY-LENGTH DISPLAYED-ITEMS) NLINES)
         (ADJUST-ARRAY-SIZE DISPLAYED-ITEMS NLINES))))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :DELETE-ITEM) (ITEM-NO &AUX AL)
  "Deleting an item -- if on the screen, update the displayed items appropriately"
  (SETQ ITEM-NO (- ITEM-NO TOP-ITEM)
        AL (SHEET-NUMBER-OF-INSIDE-LINES))
  (WHEN (AND ( ITEM-NO 0)
             (< ITEM-NO AL))
    (DOTIMES (I (- AL ITEM-NO 1))
      (SETF (AREF DISPLAYED-ITEMS (+ I ITEM-NO)) (AREF DISPLAYED-ITEMS (+ I ITEM-NO 1))))
    (SETF (AREF DISPLAYED-ITEMS (1- AL)) NIL)))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :INSERT-ITEM) (ITEM-NO IGNORE &AUX AL)
  "Inserting an item -- adjust the data structure appropriatly"
  (SETQ ITEM-NO (- ITEM-NO TOP-ITEM)
        AL (SHEET-NUMBER-OF-INSIDE-LINES))
  (WHEN (AND ( ITEM-NO 0)
             (< ITEM-NO AL))
    ;; The item will be on the screen, adjust the data structure
    (DOTIMES (I (- AL ITEM-NO 1))
      (SETF (AREF DISPLAYED-ITEMS (- AL I 1)) (AREF DISPLAYED-ITEMS (- AL I 2))))
    (SETF (AREF DISPLAYED-ITEMS ITEM-NO) NIL)))

;;; Forget anything that was on screen before
(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :REDISPLAY) (START END)
  (DO ((I START (1+ I))) (( I END))
    (SETF (AREF DISPLAYED-ITEMS I) NIL)))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :SET-ITEMS) (&REST IGNORE)
  ;; Make sure mouse isn't left pointing to gubbish
  (DOTIMES (I (ARRAY-LENGTH DISPLAYED-ITEMS))
    (SETF (AREF DISPLAYED-ITEMS I) NIL)))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :SCROLL-REDISPLAY) (IGNORE DELTA
                                                                           &AUX NLINES)
  (SETQ NLINES (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND ((> DELTA 0)                            ;Scrolling forward
         (DO ((I DELTA (1+ I))
              (J 0 (1+ J)))
             (( I NLINES)
              (DO ((J J (1+ J)))
                  (( J NLINES))
                (SETF (AREF DISPLAYED-ITEMS J) NIL)))
           (SETF (AREF DISPLAYED-ITEMS J) (AREF DISPLAYED-ITEMS I))))
        ((< DELTA 0)                            ;Scrolling backward
         (DO ((I (1- (+ NLINES DELTA)) (1- I))
              (J (1- NLINES) (1- J)))
             ((< I 0)
              (DO ((J J (1- J)))
                  ((< J 0))
                (SETF (AREF DISPLAYED-ITEMS J) nil)))
           (SETF (AREF DISPLAYED-ITEMS J) (AREF DISPLAYED-ITEMS I))))))

(DEFFLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
       ((SENSITIVE-ITEM-TYPES T)                ;Types of items that can be selected
        ITEM-BLINKER                            ;Blinker for displaying things
        )
       (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES SENSITIVE-ITEM-TYPES)
  (:DOCUMENTATION "Text scroll window that allows selection of parts of text"))

(DEFSTRUCT (MOUSE-SENSITIVE-ITEM :LIST (:CONSTRUCTOR NIL))
  DISPLAYED-ITEM-ITEM
  DISPLAYED-ITEM-TYPE
  DISPLAYED-ITEM-LEFT
  DISPLAYED-ITEM-RIGHT)

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :AFTER :INIT) (IGNORE)
  (SETQ ITEM-BLINKER (MAKE-BLINKER SELF 'HOLLOW-RECTANGULAR-BLINKER :VISIBILITY NIL)))

;;; Print something that is sensitive to the mouse -- generally called inside a :PRINT-ITEM
;;; This is what used to be called the :ITEM operation.
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :ITEM1)
           (ITEM TYPE &OPTIONAL (FUNCTION #'PRIN1) &REST PRINT-ARGS &AUX DISITEM)
  (SETQ DISITEM (LIST ITEM TYPE CURSOR-X (SHEET-INSIDE-RIGHT)))
  (PUSH DISITEM (AREF DISPLAYED-ITEMS (SHEET-LINE-NO)))
  (APPLY FUNCTION ITEM SELF PRINT-ARGS)
  ;; Try to avoid making zero-length items that cannot be selected with the mouse
  (SETF (DISPLAYED-ITEM-RIGHT DISITEM)
        (MIN (MAX (+ (DISPLAYED-ITEM-LEFT DISITEM) (SHEET-CHAR-WIDTH SELF)) CURSOR-X)
             (SHEET-INSIDE-RIGHT)))
  (MOUSE-WAKEUP))

;;; Print something that is sensitive to the mouse -- generally called inside a :PRINT-ITEM
;;; This is compatible with the :ITEM operation on typeout windows, etc.
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :ITEM)
           (TYPE ITEM &REST FORMAT-STRING-AND-ARGS &AUX DISITEM)
  (IF (AND FORMAT-STRING-AND-ARGS
           (OR (TYPEP (CAR FORMAT-STRING-AND-ARGS) 'COMPILED-FUNCTION)
               (SYMBOLP (CAR FORMAT-STRING-AND-ARGS))))
      ;; Appears to be a use of the old :ITEM operation, which is now :ITEM1.
      (LEXPR-SEND SELF :ITEM1 TYPE ITEM FORMAT-STRING-AND-ARGS)
    (SETQ DISITEM (LIST ITEM TYPE CURSOR-X (SHEET-INSIDE-RIGHT)))
    (PUSH DISITEM (AREF DISPLAYED-ITEMS (SHEET-LINE-NO)))
    (COND ((NULL FORMAT-STRING-AND-ARGS)
           (PRINC ITEM SELF))
          ((STRINGP (CAR FORMAT-STRING-AND-ARGS))
           (APPLY #'FORMAT SELF FORMAT-STRING-AND-ARGS))
          (T (FUNCALL (CAR FORMAT-STRING-AND-ARGS) ITEM SELF)))
    ;; Try to avoid making zero-length items that cannot be selected with the mouse
    (SETF (DISPLAYED-ITEM-RIGHT DISITEM)
          (MIN (MAX (+ (DISPLAYED-ITEM-LEFT DISITEM) (SHEET-CHAR-WIDTH SELF)) CURSOR-X)
               (SHEET-INSIDE-RIGHT)))
    (MOUSE-WAKEUP)))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :AFTER :HANDLE-MOUSE) ()
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL))

;;; Turn off blinker before setting up new items
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :BEFORE :SET-ITEMS)
           (&REST IGNORE)
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL))

;;; Blink any item the mouse points to
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :MOUSE-MOVES)
           (X Y
            &AUX ITEM TYPE LEFT TOP BWIDTH BHEIGHT)
  (MOUSE-SET-BLINKER-CURSORPOS)
  (MULTIPLE-VALUE-SETQ (ITEM TYPE LEFT BWIDTH TOP)
    (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (COND (TYPE
         (SETQ BWIDTH (- BWIDTH LEFT)
               BHEIGHT (FONT-BLINKER-HEIGHT CURRENT-FONT))
         (SEND ITEM-BLINKER :SET-CURSORPOS (- LEFT (SHEET-INSIDE-LEFT))
                                           (- TOP (SHEET-INSIDE-TOP)))
         (SEND ITEM-BLINKER :SET-SIZE BWIDTH BHEIGHT)
         (SEND ITEM-BLINKER :SET-VISIBILITY T))
        (T (SEND ITEM-BLINKER :SET-VISIBILITY NIL))))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :MOUSE-SENSITIVE-ITEM) (X Y)
  (MOUSE-SENSITIVE-ITEM X Y))

(DEFUN MOUSE-SENSITIVE-ITEM (X Y &AUX LINE-NO)
  "Return the mouse-sensitive-item at position X, Y in a text scroll window.
The window must be of flavor MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK.
Returns NIL if there is no mouse-sensitive item there."
  (DECLARE (:SELF-FLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK))
  (SETQ LINE-NO (SHEET-LINE-NO NIL Y))
  (AND ( Y (SHEET-INSIDE-TOP))
       (< Y (+ (SHEET-INSIDE-TOP) (* (SHEET-NUMBER-OF-INSIDE-LINES) LINE-HEIGHT)))
       (DOLIST (ITEM (AREF DISPLAYED-ITEMS LINE-NO))
         (AND (OR (EQ SENSITIVE-ITEM-TYPES T)   ;If everything visible,
                  (IF (FUNCTIONP SENSITIVE-ITEM-TYPES)
                      (FUNCALL SENSITIVE-ITEM-TYPES ITEM)       ;or filter funtion,
                    (IF (NOT (CLI:LISTP SENSITIVE-ITEM-TYPES))
                        ;; ^ should just be LISTP some day
                        (SEND SELF SENSITIVE-ITEM-TYPES ITEM)
                      (MEMQ (DISPLAYED-ITEM-TYPE ITEM) SENSITIVE-ITEM-TYPES)))) ;or this is ok
              ( (DISPLAYED-ITEM-LEFT ITEM) X)  ;And within this place on the line
              (> (DISPLAYED-ITEM-RIGHT ITEM) X)
              (RETURN
                (values
                  (DISPLAYED-ITEM-ITEM ITEM) (DISPLAYED-ITEM-TYPE ITEM)
                  (DISPLAYED-ITEM-LEFT ITEM) (DISPLAYED-ITEM-RIGHT ITEM)
                  (+ (SHEET-INSIDE-TOP) (* LINE-NO LINE-HEIGHT))))))))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :AFTER :SET-SENSITIVE-ITEM-TYPES)
           (&REST IGNORE)
  (MOUSE-WAKEUP))

(DEFFLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW ()
           (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW :MOUSE-CLICK) (BUTTON X Y &AUX ITEM TYPE)
  (MULTIPLE-VALUE-SETQ (ITEM TYPE) (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (WHEN TYPE
    (SEND SELF :FORCE-KBD-INPUT (LIST TYPE ITEM SELF BUTTON))
    T))

(DEFFLAVOR TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK () ()
  (:REQUIRED-FLAVORS TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION "Text scroll window that is grayed when it has no items"))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :AFTER :REDISPLAY)
           EMPTY-GRAY-HACK-DRAW-GRAY)

(DEFUN-METHOD EMPTY-GRAY-HACK-DRAW-GRAY TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK (&REST IGNORE)
  (OR (PLUSP (SEND SELF :NUMBER-OF-ITEMS))
      (PREPARE-SHEET (SELF)
        (BITBLT CHAR-ALUF (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
                25%-GRAY 0 0
                SCREEN-ARRAY (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :BEFORE :INSERT-ITEM) (&REST IGNORE)
  (OR (PLUSP (SEND SELF :NUMBER-OF-ITEMS))
      ;; We must have been gray -- erase ourselves
      (SEND SELF :CLEAR-WINDOW)))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :AFTER :DELETE-ITEM)
           EMPTY-GRAY-HACK-DRAW-GRAY)

;;;; Fancy printing of an item
(DEFUN PRINT-ITEM-CONCISELY (ITEM STREAM &OPTIONAL (LEVEL 0) &AUX (TYPE (DATA-TYPE ITEM)))
  "Print ITEM on STREAM in a summarized fashion.
LEVEL is the depth in recursive calls to this function."
  (IF (EQ TYPE 'DTP-LIST)
      (COND ((EQ (CAR ITEM) 'QUOTE)
             (SEND STREAM :TYO #/')
             (SEND STREAM :ITEM1 (CADR ITEM) :VALUE #'PRINT-ITEM-CONCISELY (1+ LEVEL)))
            ((AND *PRINT-LEVEL* ( LEVEL *PRINT-LEVEL*))
             (SEND STREAM :STRING-OUT (SI::PTTBL-PRINLEVEL *READTABLE*)))
            (T
             (DO () ((OR (ATOM ITEM) (NEQ (CAR ITEM) 'QUOTE)))
               (SETQ ITEM (CADR ITEM)))
             (SEND STREAM :TYO (SI::PTTBL-OPEN-PAREN *READTABLE*))
             (DO ((L ITEM (CDR L))
                  (FLAG NIL T)
                  (I 1 (1+ I)))
                 ((ATOM L)
                  (COND (L
                         (SEND STREAM :STRING-OUT (SI::PTTBL-CONS-DOT *READTABLE*))
                         (SEND STREAM :ITEM1 L
                                      :VALUE #'PRINT-ITEM-CONCISELY (1+ LEVEL))))
                  (SEND STREAM :TYO (SI::PTTBL-CLOSE-PAREN *READTABLE*)))
               (AND FLAG (SEND STREAM :TYO (SI::PTTBL-SPACE *READTABLE*)))
               (SEND STREAM :ITEM1 (CAR L) :VALUE #'PRINT-ITEM-CONCISELY (1+ LEVEL))
               (WHEN (AND *PRINT-LENGTH* ( I *PRINT-LENGTH*))
                 (SEND STREAM :STRING-OUT (SI::PTTBL-PRINLENGTH *READTABLE*))
                 (RETURN NIL)))))
      (CASE TYPE
        ((DTP-FEF-POINTER DTP-U-ENTRY)
         (SEND STREAM :STRING-OUT "#'"))
        (DTP-ARRAY-POINTER
         (AND (STRINGP ITEM)
              (OR (AND ( LEVEL 0) (> (LENGTH ITEM) 20.))
                  (STRING-SEARCH-CHAR #/NEWLINE ITEM))
              (SETQ ITEM (SI::PTTBL-PRINLENGTH *READTABLE*)))))
      (PRIN1 (SELECTQ TYPE
               (DTP-FEF-POINTER (%P-CONTENTS-OFFSET ITEM %FEFHI-FCTN-NAME))
               (DTP-U-ENTRY (MICRO-CODE-ENTRY-NAME-AREA (%POINTER ITEM)))
               (OTHERWISE ITEM))
             STREAM)))

(DEFVAR GRIND-INTO-LIST-LIST)
(DEFVAR GRIND-INTO-LIST-STRING)
(DEFVAR GRIND-INTO-LIST-ITEMS-P)
(DEFVAR GRIND-INTO-LIST-ITEMS)
(DEFVAR GRIND-INTO-LIST-LIST-ITEMS)
(DEFVAR GRIND-INTO-LIST-LIST-ITEM-STACK)
(DEFVAR GRIND-INTO-LIST-LINE)

(DEFUN GRIND-INTO-LIST (EXP WIDTH &OPTIONAL ITEMS-P
                            &AUX GRIND-INTO-LIST-LIST GRIND-INTO-LIST-STRING
                            (GRIND-INTO-LIST-ITEMS-P ITEMS-P)
                            (GRIND-INTO-LIST-ITEMS (NCONS NIL))
                            GRIND-INTO-LIST-LIST-ITEMS
                            GRIND-INTO-LIST-LIST-ITEM-STACK
                            (GRIND-INTO-LIST-LINE 0))
  "Grind EXP into width WIDTH, recording where each list and atom came out.
The first value is the output text, as a list of strings,
 one for each line of output.

The second and third values are constructed only if ITEMS-P is non-NIL.

The second value describes where each atom in EXP was printed.
 It has one element for each line of output.
 This element is a list with an element for each atom printed on that line.
  These elements look like
 (location :LOCATIVE flag start-index end-index)
 location is the pointer (part of EXP) whose car is this atom.
 If the atom is EXP itself, then location is :TOP-LEVEL.
 start-index and end-index are horizontal positions, indices
  into the string which represents the line (in the first value).

The third value describes where all the lists in EXP ended up.
 It has one element for each list in EXP.  Elements look like
 (location start-idx start-line-number end-idx end-line-number).
 Start-line-number is which line the list starts on, and
 start-idx is the horizontal position in that line.
 End-line-number and end-idx are similar."
  (GRIND-TOP-LEVEL EXP WIDTH 'GRIND-INTO-LIST-IO NIL 'SI:DISPLACED T
                   (AND ITEMS-P 'GRIND-INTO-LIST-MAKE-ITEM) :TOP-LEVEL
                   'SI::GRIND-AS-BLOCK)
  (GRIND-INTO-LIST-IO :TYO #/CR)
  (VALUES (NREVERSE GRIND-INTO-LIST-LIST)
          (NREVERSE GRIND-INTO-LIST-ITEMS)
          GRIND-INTO-LIST-LIST-ITEMS))

(DEFUN GRIND-INTO-LIST-IO (OP &OPTIONAL ARG1 &REST REST)
  (CASE OP
    (:WHICH-OPERATIONS '(:TYO :WRITE-CHAR))
    ((:TYO :WRITE-CHAR)
     (IF (FIXNUMP ARG1) (SETQ ARG1 (INT-CHAR ARG1)))
     (COND ((EQL ARG1 #/NEWLINE)
            (WHEN GRIND-INTO-LIST-STRING
              (PUSH GRIND-INTO-LIST-STRING GRIND-INTO-LIST-LIST)
              (SETQ GRIND-INTO-LIST-LINE (1+ GRIND-INTO-LIST-LINE))
              (AND GRIND-INTO-LIST-ITEMS-P
                   (PUSH NIL GRIND-INTO-LIST-ITEMS)))
            (SETQ GRIND-INTO-LIST-STRING (MAKE-STRING 50. :FILL-POINTER 0)))
           (T
            (VECTOR-PUSH-EXTEND ARG1 GRIND-INTO-LIST-STRING))))
    (T
     (STREAM-DEFAULT-HANDLER 'GRIND-INTO-LIST-IO OP ARG1 REST))))

(DEFUN GRIND-INTO-LIST-MAKE-ITEM (THING LOC ATOM-P)
  (LET ((IDX (IF GRIND-INTO-LIST-STRING
                 (LENGTH GRIND-INTO-LIST-STRING)
                 0)))
    (COND (ATOM-P
           ;; An atom -- make an item for it
           (PUSH (LIST LOC :LOCATIVE IDX (+ IDX (FLATSIZE THING 400.)))
                 (CAR GRIND-INTO-LIST-ITEMS)))
          (T
           ;; Printing an interesting character
           (CASE THING
             (#/(
              ;; Start of a list
              (PUSH (LIST LOC IDX GRIND-INTO-LIST-LINE NIL NIL)
                    GRIND-INTO-LIST-LIST-ITEM-STACK))
             (#/)
              ;; Closing a list
              (LET ((ITEM (POP GRIND-INTO-LIST-LIST-ITEM-STACK)))
                ;; 1+ is to account for close-paren which hasn't been typed yet
                (SETF (FOURTH ITEM) (1+ IDX))
                (SETF (FIFTH ITEM) GRIND-INTO-LIST-LINE)
                (PUSH ITEM GRIND-INTO-LIST-LIST-ITEMS))))))))

(DEFUN CONCISE-FLATSIZE (THING)
  "Return the number of characters it takes to do PRINT-ITEM-CONCISELY on THING."
  (LET ((SI::*IOCH 0))
    (PRINT-ITEM-CONCISELY THING 'CONCISE-FLATSIZE-STREAM)
    SI::*IOCH))

(DEFPROP CONCISE-FLATSIZE-STREAM T SI:IO-STREAM-P)
(DEFUN CONCISE-FLATSIZE-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (IF (EQ OP ':ITEM1)
      (PRINT-ITEM-CONCISELY ARG1 'CONCISE-FLATSIZE-STREAM (THIRD REST))
    (APPLY #'SI::FLATSIZE-STREAM OP ARG1 REST)))

(DEFUN CONCISE-STRING (THING &OPTIONAL TRUNCATE-AT)
  "Prints thing concisely into a string.
Returns two values: the string, and an item-list
in the form: (object starting-position-in-string last-position-in-string)."
  (LET ((CONCISE-STRING (MAKE-STRING (OR TRUNCATE-AT 100.) :FILL-POINTER 0))
        (CONCISE-ITEMS NIL)
        (CONCISE-TRUNCATE TRUNCATE-AT))
    (DECLARE (SPECIAL CONCISE-STRING CONCISE-ITEMS CONCISE-TRUNCATE))
    (CATCH 'CONCISE-TRUNCATE
      (PRINT-ITEM-CONCISELY THING 'CONCISE-STRING-STREAM))
    (VALUES CONCISE-STRING CONCISE-ITEMS)))

(DEFPROP CONCISE-STRING-STREAM T SI:IO-STREAM-P)
(DEFUN CONCISE-STRING-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (DECLARE (SPECIAL CONCISE-STRING CONCISE-ITEMS CONCISE-TRUNCATE))
  (CASE OP
    ((:TYO :WRITE-CHAR)
     (VECTOR-PUSH-EXTEND ARG1 CONCISE-STRING)
     (AND CONCISE-TRUNCATE
          ( (FILL-POINTER CONCISE-STRING) CONCISE-TRUNCATE)
          (THROW 'CONCISE-TRUNCATE NIL)))
    (:WHICH-OPERATIONS '(:TYO :WRITE-CHAR))
    (:ITEM1
     (LET ((ITEM (LIST ARG1 (FILL-POINTER CONCISE-STRING) CONCISE-TRUNCATE)))
       (PUSH ITEM CONCISE-ITEMS)
       (PRINT-ITEM-CONCISELY ARG1 'CONCISE-STRING-STREAM (THIRD REST))
       (SETF (THIRD ITEM) (FILL-POINTER CONCISE-STRING))))
    (T
     (STREAM-DEFAULT-HANDLER 'CONCISE-STRING-STREAM OP ARG1 REST))))

;;; -*- Mode:LISP; Package:TV; Readtable:ZL; Base:8 -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

#|

A scroll window displays a section of a database.  The user can dynamically
change what is displayed on the window.

The datastructure consists of items.  Each item represents an integral number
of lines.

An item is either 1) a list of items or 2) an array which contains entries.
You can create an array-item using the function SCROLL-PARSE-ITEM.
The argument is a list which contains options followed by descriptions of entries.
The options are :MOUSE <mouse-data>, :MOUSE-SELF <mouse-data> and :LEADER <leader-data>.
:MOUSE specifies the SCROLL-ITEM-LINE-SENSITIVITY.  :MOUSE-SELF is similar
except that the symbol SELF is replaced where it occurs by the item array.
:LEADER is either a number of extra leader elements to construct
or a list of contents for extra leader elements to construct.

The array elements of an array item are filled with objecs of type SCROLL-ENTRY.
When you specify an item using SCROLL-PARSE-ITEM you follow the options
with a list of entry-descriptors which are parsed with SCROLL-INTERPRET-ENTRY
to get the entry-arrays which are stored in the item array.

An entry is specified as:
*) a string, which is simply printed.
*) a symbol, whose value is printed.
*) a function, which is an abbreviation for (:FUNCTION <that-function>)
*) a list whose car is not LAMBDA or NAMED-LAMBDA.
 The car of the list can be the symbol :MOUSE, followed by the mouse-data-list
 or :MOUSE-ITEM, which is like :MOUSE except that the symbol ITEM is replaced by
 the current item throughout the mouse-data-list.
 After any of these, the entry must start with :STRING, :SYMEVAL, :VALUE or :FUNCTION
*) :STRING <string> <width-of-item-in-pixels>
*) :SYMEVAL <symbol> <width-of-item-or-NIL-if-varies> (<format-string for printing value>).
*) :VALUE <number> <width-of-item or NIL if varies> (<format string for printing value>).
 The value is the <number>'th element of the window's :VALUE-ARRAY.
*) :FUNCTION <function> <args for that function>
      <width-of-item or nil if varies> (<format-string for printing value>).
 The function is passed the appropriate args as you specified them
 and the result is the value to be printed using the format-string.
If a <width-of-item or NIL> is omitted, the default is nil; it means
 that the item will use however much space it takes up on each occasion.
If a <format string> is omitted, the value is printed.

|#

(DEFVAR SCROLL-NEW-X)
(DEFVAR SCROLL-NEW-Y)

(DEFVAR SCROLL-DEFAULT-VALUE-ARRAY-SIZE 100.)
(DEFSUBST VALUE (N) (AREF (SEND SELF :VALUE-ARRAY) N))

(DEFMACRO SCROLL-LINE () `(SHEET-LINE-NO))

(DEFMACRO SCROLL-ITEM-SIZE (ITEM) `(ARRAY-DIMENSION ,ITEM 0))

;;; Things in the leader of an array item
(DEFSTRUCT (SCROLL-ITEM-LEADER :ARRAY-LEADER (:CONSTRUCTOR NIL) (:ALTERANT NIL)
                                             (:SIZE-SYMBOL SCROLL-ITEM-LEADER-OFFSET))
  SCROLL-ITEM-MOUSE-ITEMS
  SCROLL-ITEM-LINE-SENSITIVITY)

;;; Accessors of a list item.
(DEFSTRUCT (SCROLL-ITEM :LIST* (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  SCROLL-ITEM-PLIST
  SCROLL-ITEM-COMPONENT-ITEMS)

;;; Obsolete names for the preceding.
(DEFMACRO SCROLL-FLAGS (ITEM) `(CAR ,ITEM))
(DEFMACRO SCROLL-ITEMS (ITEM) `(CDR ,ITEM))
(COMPILER:MAKE-OBSOLETE SCROLL-FLAGS "use TV:SCROLL-ITEM-PLIST")
(COMPILER:MAKE-OBSOLETE SCROLL-ITEMS "use TV:SCROLL-ITEM-COMPONENT-ITEMS")

(DEFSTRUCT (SCROLL-ENTRY :ARRAY (:ALTERANT NIL))
  SCROLL-ENTRY-FUNCTION                 ;Function to call to hack entry
  SCROLL-ENTRY-RECOMPUTE-FUNCTION       ;Function called to recompute item (sometimes unused)
  SCROLL-ENTRY-ARGS                     ;Args to above, also included is data
  SCROLL-ENTRY-LINES                    ;Number of lines entry spanned last time
  SCROLL-ENTRY-FINAL-X                  ;Final X position of cursor after this item
  SCROLL-ENTRY-FINAL-PRINTING-X         ;Final X position after item was printed (may be
                                        ; different from final-x if fixed width item)
  SCROLL-ENTRY-WIDTH                    ;Width of entry, or last width if variable width
  SCROLL-ENTRY-VARIABLE-WIDTH-P         ;T if entry is variable width, else NIL
  SCROLL-ENTRY-DATA                     ;Data to be printed
  SCROLL-ENTRY-PRINTED-FORM             ;The data stored in its printed form in case
                                        ; the data isn't a string, and if the data is
                                        ; variable width -- this makes outputting
                                        ; a bit more efficient (Note: this is only used
                                        ; when the item is variable width)
  SCROLL-ENTRY-PRINT-FORMAT             ;Specification of how to print data
                                        ; List of (prin1-or-princ base)
  SCROLL-ENTRY-MOUSE-INFO               ;Mouse data if item is mouse sensitive
  )

(DEFFLAVOR BASIC-SCROLL-WINDOW ((DISPLAY-ITEM NIL) TOP-ITEM TARGET-TOP-ITEM BOTTOM-ITEM
                                SCREEN-IMAGE SCREEN-LINES (TRUNCATION NIL)
                                (VALUE-ARRAY NIL) (OUTPUT-LOCK NIL))
  ()
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES DISPLAY-ITEM TRUNCATION VALUE-ARRAY)
  (:INITABLE-INSTANCE-VARIABLES DISPLAY-ITEM TRUNCATION VALUE-ARRAY))

(DEFUN SCROLL-FLUSH-ITEM-FROM-SCREEN-IMAGE (ITEM)
  "Clear out the records on any screen line whose contents is ITEM."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (DOTIMES (I SCREEN-LINES)
    (WHEN (EQ (AREF SCREEN-IMAGE I 0) ITEM)
      (SETF (AREF SCREEN-IMAGE I 0) NIL)
      (SETF (AREF SCREEN-IMAGE I 1) -1)
      (SETF (AREF SCREEN-IMAGE I 2) -1))))

(DEFUN SCROLL-MAKE-SCREEN-IMAGE ()
  "Create the value of SCREEN-IMAGE, an array recording what items are on what screen lines."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (SETQ SCREEN-LINES (SHEET-NUMBER-OF-INSIDE-LINES))
  (SETQ SCREEN-IMAGE (MAKE-ARRAY `(,SCREEN-LINES 3)))
  (DOTIMES (I SCREEN-LINES)
    (SETF (AREF SCREEN-IMAGE I 0) NIL)
    (SETF (AREF SCREEN-IMAGE I 1) -1)
    (SETF (AREF SCREEN-IMAGE I 2) -1)))

(DEFFLAVOR SCROLL-WINDOW ()
  (FLASHY-SCROLLING-MIXIN BASIC-SCROLL-WINDOW BORDERS-MIXIN BASIC-SCROLL-BAR WINDOW)
  (:DOCUMENTATION :COMBINATION))

(DEFMETHOD (SCROLL-WINDOW :SCROLL-BAR-P) () T)

(DEFMETHOD (SCROLL-WINDOW :SCREEN-MANAGE) (&REST IGNORE) () )

(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :INIT) (PLIST)
  (SETQ TOP-ITEM NIL
        TARGET-TOP-ITEM 0)
  (OR VALUE-ARRAY (SETQ VALUE-ARRAY (MAKE-ARRAY SCROLL-DEFAULT-VALUE-ARRAY-SIZE)))
  (PUTPROP PLIST NIL ':BLINKER-P))

(DEFMETHOD (BASIC-SCROLL-WINDOW :MORE-EXCEPTION) ()
  (SETF (SHEET-MORE-FLAG) 0))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :INIT) (IGNORE)
  (SCROLL-MAKE-SCREEN-IMAGE))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SET-DISPLAY-ITEM) (NEW-DISPLAY-ITEM)
  (SETQ DISPLAY-ITEM NEW-DISPLAY-ITEM
        TOP-ITEM NIL
        TARGET-TOP-ITEM 0)
  (SEND SELF :REDISPLAY T :FORCE))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SET-TRUNCATION) (NEW-VALUE)
  (SETQ TRUNCATION NEW-VALUE)
  (SEND SELF :REDISPLAY T :FORCE))

(DEFVAR SCROLL-SPACES (FORMAT NIL "~2000@T")
  "A list of more spaces than anyone could ever need a substring of.")

;;; All the work is done by the redisplayer
(DEFUN SCROLL-REDISPLAY-DISPLAY-ITEM (ITEM CURRENT-COUNT &OPTIONAL DONT-SET-BOTTOM-ITEM
                                           &AUX CURRENT-LINE CURRENT-ITEM-LINE
                                           FIRST-LINE FORCE-UPDATE OLD-LINE
                                           ENTRY-NEEDS-UPDATING END-OF-ITEM)
  "Display item ITEM with item number CURRENT-COUNT, if it belongs on the screen.
CURSOR-Y should be set up to the place to display the next item
that should be displayed (this item, if this one should be displayed).
The idea is that you call this for successive items
until the cursor gets to the end of the screen.
BOTTOM-ITEM is set to CURRENT-COUNT unless DONT-SET-BOTTOM-ITEM is non-NIL."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (COND (( CURRENT-COUNT TARGET-TOP-ITEM)
         ;; We wanna be on the screen
         (SETQ FIRST-LINE (IF SCROLL-NEW-X
                              (TRUNCATE SCROLL-NEW-Y LINE-HEIGHT)
                              (SCROLL-LINE)))
         (OR DONT-SET-BOTTOM-ITEM (SETQ BOTTOM-ITEM CURRENT-COUNT))
         (COND ((AND (SETQ OLD-LINE
                           (DO ((I FIRST-LINE (1+ I)))
                               (( I SCREEN-LINES) NIL)
                             (AND (EQ ITEM (AREF SCREEN-IMAGE I 0))
                                  (ZEROP (AREF SCREEN-IMAGE I 1))
                                  ;; If first line of this item on screen anywhere, then
                                  ;; can move it to current line
                                  (RETURN I))))
                     ( OLD-LINE FIRST-LINE))
                (AND SCROLL-NEW-X
                     (SEND SELF :SET-CURSORPOS SCROLL-NEW-X SCROLL-NEW-Y))
                (SETQ SCROLL-NEW-X NIL
                      SCROLL-NEW-Y NIL)
                ;; On screen but not in same position, move it up
                (SEND SELF :DELETE-LINE (- OLD-LINE FIRST-LINE))))

         ;; Now redisplay the item.
         (SETQ CURRENT-LINE FIRST-LINE
               CURRENT-ITEM-LINE 0)
         (UNWIND-PROTECT
           (PROGN
             (DOTIMES (I (SCROLL-ITEM-SIZE ITEM))
               (LET ((ENTRY (AREF ITEM I))
                     (WID) (CHANGED-P))
                 ;; Loop over all elements of the item
                 (SETQ ENTRY-NEEDS-UPDATING
                       (OR FORCE-UPDATE
                           (NEQ ITEM (AREF SCREEN-IMAGE CURRENT-LINE 0))
                           ( (AREF SCREEN-IMAGE CURRENT-LINE 1) CURRENT-ITEM-LINE)))
                 (COND ((NOT (OR (SETQ CHANGED-P (FUNCALL (SCROLL-ENTRY-FUNCTION ENTRY)
                                                          :CHANGED-P ENTRY
                                                          (SCROLL-ENTRY-FUNCTION ENTRY)))
                                 ENTRY-NEEDS-UPDATING))
                        ;; Entry didn't change, but take into account how many
                        ;; lines it takes up
                        (LET ((SEL (SCROLL-ENTRY-LINES ENTRY)))
                          (IF (AND TRUNCATION (> SEL 0))
                              ;; Spans more than one line, and truncating -- punt
                              (SETQ END-OF-ITEM T)
                            (SETQ CURRENT-ITEM-LINE (+ SEL CURRENT-ITEM-LINE)
                                  CURRENT-LINE (+ SEL CURRENT-LINE))
                            (SETQ SCROLL-NEW-X (- (SCROLL-ENTRY-FINAL-X ENTRY)
                                                  (SHEET-INSIDE-LEFT))
                                  SCROLL-NEW-Y (+ (OR SCROLL-NEW-Y
                                                      (- CURSOR-Y (SHEET-INSIDE-TOP)))
                                                  (* LINE-HEIGHT SEL)))
                            (AND ( (TRUNCATE SCROLL-NEW-Y LINE-HEIGHT) SCREEN-LINES)
                                 (THROW 'END-OF-PAGE T)))))
                       ;; Set cursor to correct place, and continue with COND
                       ((PROG1 NIL
                               (AND SCROLL-NEW-X
                                    (SEND SELF :SET-CURSORPOS SCROLL-NEW-X SCROLL-NEW-Y))
                               (SETQ SCROLL-NEW-X NIL
                                     SCROLL-NEW-Y NIL)))
                       ;; Entry needs updating, decide whether variable width or not
                       ((AND CHANGED-P
                             (SCROLL-ENTRY-VARIABLE-WIDTH-P ENTRY)
                             ( (SCROLL-ENTRY-WIDTH ENTRY)
                                (SETQ WID (FUNCALL (SCROLL-ENTRY-FUNCTION ENTRY) :WIDTH
                                                   ENTRY (SCROLL-ENTRY-FUNCTION ENTRY)))))
                        ;; Going to kill line, move it down if it belongs below here anyway
                        (AND (AREF SCREEN-IMAGE CURRENT-LINE 0)
                             (NEQ (AREF SCREEN-IMAGE CURRENT-LINE 0) ITEM)
                             (SEND SELF :INSERT-LINE 1))
                        ;; Variable width entry, and the width changed, force
                        ;; complete update of rest of item
                        (SETQ FORCE-UPDATE T)
                        (SETF (SCROLL-ENTRY-WIDTH ENTRY) WID)
                        (SEND SELF :CLEAR-REST-OF-LINE)
                        (SCROLL-FLUSH-ITEM-FROM-SCREEN-IMAGE ITEM)
                        (LET ((*SCROLL-CURRENT-ITEM* ITEM)
                              (*SCROLL-CURRENT-ITEM-LINE* CURRENT-ITEM-LINE))
                          (SETQ END-OF-ITEM
                                (CATCH 'END-OF-LINE
                                  (CATCH-ERROR
                                    (PROGN
                                      (FUNCALL (SCROLL-ENTRY-FUNCTION ENTRY) :PRINT ENTRY)
                                      NIL)
                                    T))))
                        (SETF (SCROLL-ENTRY-FINAL-X ENTRY) CURSOR-X)
                        (SETF (SCROLL-ENTRY-FINAL-PRINTING-X ENTRY) CURSOR-X)
                        (SETF (SCROLL-ENTRY-LINES ENTRY)
                              (- (MIN (SCROLL-LINE) (1- SCREEN-LINES)) CURRENT-LINE))
                        (SETQ CURRENT-LINE (SCROLL-LINE)
                              CURRENT-ITEM-LINE (+ (SCROLL-ENTRY-LINES ENTRY)
                                                   CURRENT-ITEM-LINE)))
                       (T
                        ;; Fixed width entry, or variable width entry and width hasn't changed
                        ;; Using the width, figure out the cursor motion and erase area
                        (MULTIPLE-VALUE-BIND (FINAL-X FINAL-Y FINAL-COUNT)
                            (SHEET-COMPUTE-MOTION SELF NIL NIL SCROLL-SPACES
                                                  0 (SCROLL-ENTRY-WIDTH ENTRY)
                                                  NIL
                                                  (IF TRUNCATION
                                                      (- (SHEET-INSIDE-RIGHT) CHAR-WIDTH)
                                                      0)
                                                  (IF TRUNCATION
                                                      (- CURSOR-Y (SHEET-INSIDE-TOP))
                                                      NIL)
                                                  1000000)
                          (SETQ FINAL-X (+ FINAL-X (SHEET-INSIDE-LEFT))
                                FINAL-Y (+ FINAL-Y (SHEET-INSIDE-TOP))
                                END-OF-ITEM (AND (NUMBERP FINAL-COUNT)
                                                 ( FINAL-COUNT (SCROLL-ENTRY-WIDTH ENTRY))))
                          ;; Don't let the FINAL-Y be past the last screen line.
                          (AND (> FINAL-Y (+ TOP-MARGIN-SIZE
                                             (* (1- SCREEN-LINES) LINE-HEIGHT)))
                               (SETQ FINAL-X (SHEET-INSIDE-RIGHT)
                                     FINAL-Y (+ TOP-MARGIN-SIZE
                                                (* (1- SCREEN-LINES) LINE-HEIGHT))))
                          (SETF (SCROLL-ENTRY-FINAL-X ENTRY) FINAL-X)
                          (SETF (SCROLL-ENTRY-LINES ENTRY)
                                (- (SHEET-LINE-NO NIL FINAL-Y) CURRENT-LINE))
                          ;; Zero the area
                          (PREPARE-SHEET (SELF)
                            (DO ((Y CURSOR-Y (+ Y LINE-HEIGHT))
                                 (LINE 0 (1+ LINE))
                                 (X CURSOR-X (SHEET-INSIDE-LEFT))
                                 (LE) (DELTA-ITEMS))
                                ((> Y FINAL-Y))
                              (SETQ LE (AREF SCREEN-IMAGE (+ CURRENT-LINE LINE) 0))
                              (COND ((OR (AND (EQ LE ITEM)
                                              (= (AREF SCREEN-IMAGE (+ CURRENT-LINE LINE) 1)
                                                 (+ CURRENT-ITEM-LINE LINE)))
                                         (NULL LE))
                                     ;; We know about this line so just clear the area
                                     (%DRAW-RECTANGLE (- (IF (= Y FINAL-Y)
                                                             FINAL-X
                                                             (SHEET-INSIDE-RIGHT))
                                                         X)
                                                      LINE-HEIGHT X Y ALU-ANDCA SELF))
                                    ((EQ LE ITEM)
                                     ;; We own line, but it is wrong number.  Clear the line
                                     ;; and flush all knowledge
                                     (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT) X) LINE-HEIGHT
                                                      X Y ALU-ANDCA SELF)
                                     (SCROLL-FLUSH-ITEM-FROM-SCREEN-IMAGE ITEM))
                                    (T
                                     ;; Make room for remaining number of lines and return
                                     (SETQ DELTA-ITEMS
                                           (- (AREF SCREEN-IMAGE (+ CURRENT-LINE LINE) 2)
                                              CURRENT-COUNT))
                                     ;; DELTA-ITEMS is a guess as to the number of items
                                     ;; in between this and the line it collided with.
                                     ;; Assuming one line per item, this is a good guess as
                                     ;; to the number of additional lines to insert
                                     (LET-GLOBALLY ((CURSOR-Y Y))
                                       (SEND SELF :INSERT-LINE
                                         ;; If we are past the item that's on this line, it
                                         ;; can't possibly appear on the screen -- insert
                                         ;; enough lines to make it go off the screen
                                         (MAX 1 (MIN (+ (TRUNCATE (- FINAL-Y Y) LINE-HEIGHT)
                                                        (ABS DELTA-ITEMS))
                                                     (- SCREEN-LINES (SCROLL-LINE))))))
                                     (RETURN T)))))
                          (LET ((*SCROLL-CURRENT-ITEM* ITEM)
                                (*SCROLL-CURRENT-ITEM-LINE* CURRENT-ITEM-LINE))
                            (COND ((CATCH 'END-OF-LINE
                                     (PROGN
                                       (FUNCALL (SCROLL-ENTRY-FUNCTION ENTRY) :PRINT ENTRY)
                                       (SETQ CURRENT-ITEM-LINE (+ (SCROLL-ENTRY-LINES ENTRY)
                                                                  CURRENT-ITEM-LINE)
                                             CURRENT-LINE (+ (SCROLL-ENTRY-LINES ENTRY)
                                                             CURRENT-LINE))
                                       (SETF (SCROLL-ENTRY-FINAL-PRINTING-X ENTRY) CURSOR-X)
                                       (SETQ SCROLL-NEW-X (- FINAL-X (SHEET-INSIDE-LEFT))
                                             SCROLL-NEW-Y (- FINAL-Y (SHEET-INSIDE-TOP)))
                                       (SEND SELF :HANDLE-EXCEPTIONS)
                                       NIL))
                                   (SETF (SCROLL-ENTRY-FINAL-PRINTING-X ENTRY) CURSOR-X)
                                   (SETQ END-OF-ITEM T))))))))
               (AND END-OF-ITEM (RETURN T)))
             (SETQ SCROLL-NEW-X 0
                   SCROLL-NEW-Y (+ (OR SCROLL-NEW-Y (- CURSOR-Y (SHEET-INSIDE-TOP)))
                                   LINE-HEIGHT))
             (AND ( (1+ CURRENT-LINE) SCREEN-LINES)
                  (THROW 'END-OF-PAGE T)))
           (SETQ CURRENT-LINE (MIN CURRENT-LINE (1- SCREEN-LINES)))
           (DO ((L FIRST-LINE (1+ L)))
               ((> L CURRENT-LINE))
             (SETF (AREF SCREEN-IMAGE L 0) ITEM)
             (SETF (AREF SCREEN-IMAGE L 1) (- L FIRST-LINE))
             (SETF (AREF SCREEN-IMAGE L 2) CURRENT-COUNT))))))

(DEFUN SCROLL-REDISPLAY-ITEM-LOOP (ITEM CURRENT-COUNT FUNCTION NO-RECOMP &REST POSITION
                                   &AUX FUN)
  "Loop over an item and its inferiors until TARGET-TOP-ITEM has been reached,
then start doing the appropriate things to fix up the screen.  This may require
inserting and deleting lines, etc...  Returns what the number of the next item is.
NO-RECOMP says do not recompute the inferiors of ITEM;
redisplay whatever inferiors are recorded for it.
FUNCTION should be the function to redisplay one item,
such as SCROLL-REDISPLAY-DISPLAY-ITEM."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (COND ((NULL ITEM))
        ((CONSP ITEM)
         ;; A list of other items, recurse
         (OR NO-RECOMP
             (DO ((F (SCROLL-FLAGS ITEM) (CDDR F)))
                 ((NULL F))
               (CASE (CAR F)
                 (:FUNCTION (SETQ FUN (CADR F)))
                 (:PRE-PROCESS-FUNCTION (FUNCALL (CADR F) ITEM)))))
         (DO ((ITEMS (SCROLL-ITEMS ITEM) (CDR ITEMS)))
             ((NULL ITEMS))
           (AND FUN
                (SETF (CAR ITEMS) (FUNCALL FUN (CAR ITEMS) POSITION (LOCF (SCROLL-FLAGS ITEM)))))
           (SETQ CURRENT-COUNT
                 (APPLY #'SCROLL-REDISPLAY-ITEM-LOOP
                        (CAR ITEMS) CURRENT-COUNT FUNCTION NO-RECOMP 0 POSITION))
           (SEND SELF :HANDLE-EXCEPTIONS)
           (SETF (FIRST POSITION) (1+ (FIRST POSITION)))))
        ;; An item that really takes up space
        ((> (SETQ CURRENT-COUNT (1+ CURRENT-COUNT)) TARGET-TOP-ITEM)
         ;; This item is of interest
         (FUNCALL FUNCTION ITEM (1- CURRENT-COUNT))))
  CURRENT-COUNT)

(DEFUN SCROLL-REDISPLAY (&OPTIONAL FULL-REDISPLAY &AUX (SCROLL-NEW-X 0) (SCROLL-NEW-Y 0))
  "Redisplay SELF, a scroll window.  FULL-REDISPLAY says redisplay all, else only changes."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
   (LOCK-SHEET (SELF)
    (AND (OR FULL-REDISPLAY (NULL TOP-ITEM))
         ;; If doing full redisplay then must clear whole screen
         ;; :CLEAR-WINDOW will take care of forcing redisplay
         (SEND SELF :CLEAR-WINDOW))
    (WITHOUT-INTERRUPTS
      (COND ((NULL TOP-ITEM)
             ;; Nothing on the screen now, will have to do whole thing
             (AND (OR (NULL TARGET-TOP-ITEM)
                      (< TARGET-TOP-ITEM 0))
                  (SETQ TARGET-TOP-ITEM 0)))
            ((OR (NULL TARGET-TOP-ITEM) (< TARGET-TOP-ITEM 0))
             ;; No change in top line, just target for where we are
             (SETQ TARGET-TOP-ITEM TOP-ITEM))))
    (CATCH 'END-OF-PAGE
      (PROGN
        (SETQ BOTTOM-ITEM -1)
        (SCROLL-REDISPLAY-ITEM-LOOP DISPLAY-ITEM 0
                                    #'SCROLL-REDISPLAY-DISPLAY-ITEM NIL 0)
        (SETQ BOTTOM-ITEM (1+ BOTTOM-ITEM))
        (AND SCROLL-NEW-X
             (SEND SELF :SET-CURSORPOS SCROLL-NEW-X SCROLL-NEW-Y))
        (SEND SELF :CLEAR-REST-OF-LINE)
        (DO ((I (SCROLL-LINE) (1+ I)))
            (( I SCREEN-LINES))
          ;; This does not :TYO a #\CR because SHEET-CR-NOT-NEWLINE-FLAG may be set,
          ;; and we really want to move to the next line.
          (CATCH 'END-OF-PAGE (SHEET-CRLF SELF))
          (SEND SELF :CLEAR-REST-OF-LINE)
          (SETF (AREF SCREEN-IMAGE I 0) NIL)
          (SETF (AREF SCREEN-IMAGE I 1) -1)
          (SETF (AREF SCREEN-IMAGE I 2) -1))))
    (SETQ TOP-ITEM TARGET-TOP-ITEM)
    (SEND SELF :NEW-SCROLL-POSITION TOP-ITEM))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-WINDOW)
(DEFSELECT SCROLL-ENTRY-CONSTANT-STRING-FUNCTION
  (:PRINT (ENTRY)
    (SEND SELF :STRING-OUT (SCROLL-ENTRY-DATA ENTRY)))
  (:RECOMPUTE (IGNORE) NIL)
  (:CHANGED-P (IGNORE IGNORE) NIL)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-WINDOW)
(DEFSELECT SCROLL-ENTRY-SYMBOL-VALUE-FUNCTION
  (:PRINT (ENTRY &AUX (DATA (SCROLL-ENTRY-PRINTED-FORM ENTRY))
                      (FORMAT (SCROLL-ENTRY-PRINT-FORMAT ENTRY)))
    (IF DATA
        (SEND SELF :STRING-OUT DATA)
      (LET ((*PRINT-BASE* (OR (CADR FORMAT) *PRINT-BASE*))
            (*NOPOINT (OR (CADDR FORMAT) *NOPOINT)))
           ;>> *print-radix*??
        (SETQ DATA (SCROLL-ENTRY-DATA ENTRY))
        (IF (CAR FORMAT)
            (FORMAT SELF (CAR FORMAT) DATA)
            (FORMAT SELF "~A" DATA)))))
  (:CHANGED-P (ENTRY US)
    (COND ((NOT (EQUAL (SCROLL-ENTRY-DATA ENTRY)
                       (SEND US :RECOMPUTE ENTRY)))
           (SETF (SCROLL-ENTRY-PRINTED-FORM ENTRY) NIL)
           T)
          (T NIL)))
  (:RECOMPUTE (ENTRY &AUX DATA)
    (SETQ DATA (SYMEVAL (SCROLL-ENTRY-RECOMPUTE-FUNCTION ENTRY)))
    (SETF (SCROLL-ENTRY-DATA ENTRY) DATA)
    DATA)
  (:WIDTH (ENTRY US &AUX DATA (FORMAT (SCROLL-ENTRY-PRINT-FORMAT ENTRY)))
    (SETQ DATA (SEND US :RECOMPUTE ENTRY))
    ;; Stream to return length
    (LET ((*READ-BASE* (OR (CADR FORMAT) *READ-BASE*))
          (*NOPOINT (OR (CADDR FORMAT) *NOPOINT)))
;        (SETF (SCROLL-ENTRY-PRINTED-FORM ENTRY)
;              (SETQ DATA
;                    (IF (AND (STRINGP DATA) (NULL (CAR FORMAT)))
;                        DATA
;                        (IF (CAR FORMAT)
;                            (FORMAT NIL (CAR FORMAT) DATA)
;                            (FORMAT NIL "~A" DATA)))))
;        (MULTIPLE-VALUE-BIND (IGNORE -WIDTH-)
;            (SHEET-STRING-LENGTH SELF DATA)
;          -WIDTH-))))
      (IF (AND (STRINGP DATA) (NULL (CAR FORMAT)))
          (NTH-VALUE 1 (SHEET-STRING-LENGTH SELF DATA))
        ;>> Bletch
        (LET ((SI::*IOCH 0))
          (IF (CAR FORMAT)
              (FORMAT 'SI::FLATSIZE-STREAM (CAR FORMAT) DATA)
            (FORMAT 'SI::FLATSIZE-STREAM "~A" DATA))
          SI::*IOCH))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-WINDOW)
(DEFSELECT (SCROLL-ENTRY-CALL-FUNCTION-FUNCTION SCROLL-ENTRY-SYMBOL-VALUE-FUNCTION)
  (:RECOMPUTE (ENTRY &AUX DATA)
    (SETQ DATA (APPLY (SCROLL-ENTRY-RECOMPUTE-FUNCTION ENTRY) (SCROLL-ENTRY-ARGS ENTRY)))
    (SETF (SCROLL-ENTRY-DATA ENTRY) DATA)
    DATA)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SCROLL-MORE-ABOVE) ()
  (AND TOP-ITEM (> TOP-ITEM 0)))

(DEFUN SCROLL-TOTAL-ITEMS ()
  "Return the total number of items (including inferiors) in SELF."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (1- (SCROLL-REDISPLAY-ITEM-LOOP DISPLAY-ITEM 0 #'IGNORE T 0)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SCROLL-POSITION) ()
  (VALUES (OR TOP-ITEM 0)                       ;Item number at top
          (SCROLL-TOTAL-ITEMS)                  ;Total number of items
          LINE-HEIGHT                           ;Pixels per item
          (IF TOP-ITEM                          ;Number currently displayed
              (MAX 0 (- BOTTOM-ITEM TOP-ITEM))
            1)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :REFRESH) (&OPTIONAL TYPE)
  (AND (OR (NOT RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
       (SCROLL-REDISPLAY T)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :REDISPLAY) (&OPTIONAL (FULL-P NIL) (FORCE-P NIL))
  (IF FORCE-P
      (SCROLL-REDISPLAY FULL-P)
    (DO-FOREVER
      (LOCK-SHEET (SELF)
        (OR (SHEET-OUTPUT-HELD-P SELF)
            (RETURN (SCROLL-REDISPLAY FULL-P))))
      (SEND SELF :OUTPUT-HOLD-EXCEPTION))))

(DEFMETHOD (BASIC-SCROLL-WINDOW :REDISPLAY-SELECTED-ITEMS) (ITEMS)
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
    (LET ((SCROLL-NEW-X) (SCROLL-NEW-Y)
          (ITEM NIL))
      (SETQ TARGET-TOP-ITEM TOP-ITEM)
      (CATCH 'END-OF-PAGE
        (DOTIMES (I SCREEN-LINES)
          (COND ((AND (SETQ ITEM (CAR (MEMQ (AREF SCREEN-IMAGE I 0) ITEMS)))
                      (ZEROP (AREF SCREEN-IMAGE I 1)))
                 (SETQ SCROLL-NEW-Y (* I LINE-HEIGHT)
                       SCROLL-NEW-X 0)
                 (SCROLL-REDISPLAY-DISPLAY-ITEM ITEM (AREF SCREEN-IMAGE I 2) T))))))))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :REDISPLAY) (&REST IGNORE)
  (MOUSE-WAKEUP))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SCROLL-MAKE-SCREEN-IMAGE))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :CHANGE-OF-DEFAULT-FONT) (&REST IGNORE)
  (SCROLL-MAKE-SCREEN-IMAGE)
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
    (SEND SELF :CLEAR-WINDOW)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :CLEAR-WINDOW) ()
  (OR TARGET-TOP-ITEM (SETQ TARGET-TOP-ITEM TOP-ITEM))
  (SETQ TOP-ITEM NIL)
  (DOTIMES (I SCREEN-LINES)
    (SETF (AREF SCREEN-IMAGE I 0) NIL)
    (SETF (AREF SCREEN-IMAGE I 1) -1)
    (SETF (AREF SCREEN-IMAGE I 2) -1)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :CLEAR-SCREEN) ()
  (OR TARGET-TOP-ITEM (SETQ TARGET-TOP-ITEM TOP-ITEM))
  (SETQ TOP-ITEM NIL)
  (DOTIMES (I SCREEN-LINES)
    (SETF (AREF SCREEN-IMAGE I 0) NIL)
    (SETF (AREF SCREEN-IMAGE I 1) -1)
    (SETF (AREF SCREEN-IMAGE I 2) -1)))

;;; At end of page simply throw
(DEFMETHOD (BASIC-SCROLL-WINDOW :END-OF-PAGE-EXCEPTION) (&REST IGNORE)
  (SETF (SHEET-END-PAGE-FLAG SELF) 0)
  (THROW 'END-OF-PAGE T))

;;; If run over end of line, make sure room on next line
(DEFVAR *SCROLL-CURRENT-ITEM*)
(DEFVAR *SCROLL-CURRENT-ITEM-LINE*)
(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :END-OF-LINE-EXCEPTION) ()
  (COND (TRUNCATION
         (THROW 'END-OF-LINE T))
        (( (1+ (SCROLL-LINE)) SCREEN-LINES)
         ;; We're really at the end of the page, nothing to do
         )
        ((NEQ *SCROLL-CURRENT-ITEM* (AREF SCREEN-IMAGE (1+ (SCROLL-LINE)) 0))
         (SCROLL-FLUSH-ITEM-FROM-SCREEN-IMAGE *SCROLL-CURRENT-ITEM*)
         (LET-GLOBALLY ((CURSOR-Y (+ CURSOR-Y LINE-HEIGHT)))
           (SEND SELF :INSERT-LINE)))
        (( (1+ *SCROLL-CURRENT-ITEM-LINE*) (AREF SCREEN-IMAGE (1+ (SCROLL-LINE)) 1))
         (SCROLL-FLUSH-ITEM-FROM-SCREEN-IMAGE *SCROLL-CURRENT-ITEM*))))

(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :DELETE-LINE) (&OPTIONAL (N 1)
                                                       &AUX (CUR-LINE (SCROLL-LINE)))
  (DO ((L CUR-LINE (1+ L)))
      (( (+ L N) SCREEN-LINES))
    (SETF (AREF SCREEN-IMAGE L 0) (AREF SCREEN-IMAGE (+ L N) 0))
    (SETF (AREF SCREEN-IMAGE L 1) (AREF SCREEN-IMAGE (+ L N) 1))
    (SETF (AREF SCREEN-IMAGE L 2) (AREF SCREEN-IMAGE (+ L N) 2)))
  (DOTIMES (I N)
    (SETF (AREF SCREEN-IMAGE (- SCREEN-LINES (1+ I)) 0) NIL)
    (SETF (AREF SCREEN-IMAGE (- SCREEN-LINES (1+ I)) 1) -1)
    (SETF (AREF SCREEN-IMAGE (- SCREEN-LINES (1+ I)) 2) -1)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :BEFORE :INSERT-LINE) (&OPTIONAL (N 1)
                                                       &AUX (CUR-LINE (SCROLL-LINE)))
  (DO ((L (- SCREEN-LINES N 1) (1- L))
       (I (1- SCREEN-LINES) (1- I)))
      ((< L CUR-LINE))
    (SETF (AREF SCREEN-IMAGE I 0) (AREF SCREEN-IMAGE L 0))
    (SETF (AREF SCREEN-IMAGE I 1) (AREF SCREEN-IMAGE L 1))
    (SETF (AREF SCREEN-IMAGE I 2) (AREF SCREEN-IMAGE L 2)))
  (DO ((L CUR-LINE (1+ L)))
      (( L (+ CUR-LINE N)))
    (SETF (AREF SCREEN-IMAGE L 0) NIL)
    (SETF (AREF SCREEN-IMAGE L 1) -1)
    (SETF (AREF SCREEN-IMAGE L 2) -1)))

(DEFUN SCROLL-ITEM-NUMBER-AT-Y (Y &AUX IN)
  "Return the item number of the item displayed at Y-position Y, as of last redisplay."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (SETQ Y (MAX 0 (MIN (SHEET-LINE-NO NIL Y) (1- (SHEET-NUMBER-OF-INSIDE-LINES)))))
  (SETQ IN (AREF SCREEN-IMAGE Y 2))
  (IF (< IN 0)
      NIL
      IN))

(DEFUN SCROLL-FIND-A-TOP-ITEM (TARGET-ITEM TARGET-LINE
                               &AUX (SCROLL-ITEM-TARGET-ITEM TARGET-ITEM)
                               (SCROLL-ITEM-TARGET-DISTANCE TARGET-LINE))
  "Find which item should be the top item to put TARGET-ITEM on screen line TARGET-LINE."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW)
           (SPECIAL SCROLL-ITEM-TARGET-DISTANCE SCROLL-ITEM-TARGET-ITEM))
  (LET ((SCROLL-ITEM-LIST NIL)
        (SCROLL-ITEM-LONGEST-DISTANCE 0))
    (DECLARE (SPECIAL SCROLL-ITEM-LIST SCROLL-ITEM-LONGEST-DISTANCE))
    (LET-GLOBALLY ((TARGET-TOP-ITEM 0))
      (CATCH 'SCROLL-FIND-A-TOP-ITEM
        (SCROLL-REDISPLAY-ITEM-LOOP DISPLAY-ITEM 0
                                    #'SCROLL-FIND-A-TOP-ITEM-INTERNAL NIL 0)))))

(DEFUN SCROLL-FIND-A-TOP-ITEM-INTERNAL (ITEM ITEM-NUMBER)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW)
           (SPECIAL SCROLL-ITEM-LIST SCROLL-ITEM-LONGEST-DISTANCE
                    SCROLL-ITEM-TARGET-DISTANCE SCROLL-ITEM-TARGET-ITEM))
  (COND ((= ITEM-NUMBER SCROLL-ITEM-TARGET-ITEM)
         ;; Found the item of interest, put the last item on top.
         (THROW 'SCROLL-FIND-A-TOP-ITEM (CDAR (LAST SCROLL-ITEM-LIST))))
        (( SCROLL-ITEM-LONGEST-DISTANCE SCROLL-ITEM-TARGET-DISTANCE)
         ;; We have enough items to make up the distance, throw away the last one and
         ;; add in the new one
         (LET ((LAST-ITEM (CAR (LAST SCROLL-ITEM-LIST))))
           (SETQ SCROLL-ITEM-LONGEST-DISTANCE
                 (+ (- SCROLL-ITEM-LONGEST-DISTANCE
                       (IF LAST-ITEM (SCROLL-ITEM-LINES (CAR LAST-ITEM)) 0))
                    (SCROLL-ITEM-LINES ITEM)))
           (SETQ SCROLL-ITEM-LIST (DELQ LAST-ITEM SCROLL-ITEM-LIST))
           (PUSH (CONS ITEM ITEM-NUMBER) SCROLL-ITEM-LIST)))
        (T
         ;; An item, we can use it
         (PUSH (CONS ITEM ITEM-NUMBER) SCROLL-ITEM-LIST)
         (SETQ SCROLL-ITEM-LONGEST-DISTANCE
               (+ SCROLL-ITEM-LONGEST-DISTANCE (SCROLL-ITEM-LINES ITEM))))))

(DEFUN SCROLL-ITEM-LINES (ITEM &AUX (SUM 1))
  "Returns the number of lines ITEM would occupy on the screen."
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW)
           (SPECIAL SCROLL-ITEM-LIST SCROLL-ITEM-LONGEST-DISTANCE
                    SCROLL-ITEM-TARGET-DISTANCE SCROLL-ITEM-TARGET-ITEM))
  (DOTIMES (I (ARRAY-DIMENSION ITEM 0))
    (SETQ SUM (+ SUM (OR (SCROLL-ENTRY-LINES (AREF ITEM I)) 1))))
  SUM)

;;; Scrolling from the mouse
(DEFMETHOD (BASIC-SCROLL-WINDOW :SCROLL-RELATIVE) (FROM TO)
  (OR TOP-ITEM
      ;; Redisplay if a redisplay hasn't been done recently
      (SEND SELF :REDISPLAY))
  ;; Convert FROM into an item number, and TO into a target top item
  (SETQ FROM (COND ((EQ FROM ':TOP) TOP-ITEM)
                   ((EQ FROM ':BOTTOM) BOTTOM-ITEM)
                   ((NUMBERP FROM)
                    ;; Number of pixels down, convert into item number
                    (SCROLL-ITEM-NUMBER-AT-Y FROM))))
  (AND (EQ TO ':BOTTOM)
       (SETQ TO (1- (* SCREEN-LINES LINE-HEIGHT))))
  (SETQ TO (IF (EQ TO ':TOP)
               FROM
               ;; Find an item such that if we put it on the top, then the FROM item will be
               ;; in the desired position.  This is an estimate only and not guaranteed to
               ;; do exactly the right thing
               (AND FROM (SCROLL-FIND-A-TOP-ITEM FROM (TRUNCATE TO LINE-HEIGHT)))))
  (IF TO
      (SEND SELF :SCROLL-TO TO)
    (BEEP)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SCROLL-TO) (TO &OPTIONAL (TYPE ':ABSOLUTE))
  (IF (EQ CURRENT-PROCESS MOUSE-PROCESS)
      (PROCESS-RUN-FUNCTION "Scroll" SELF :SCROLL-TO TO TYPE)
      (OR TOP-ITEM
          ;; Redisplay if a redisplay hasn't been done recently
          (SEND SELF :REDISPLAY))
      (SETQ TARGET-TOP-ITEM (CASE TYPE
                              (:ABSOLUTE TO)
                              (:RELATIVE (+ TO TOP-ITEM))
                              (OTHERWISE (FERROR NIL "~A is an unknown type of scrolling"
                                                 TYPE))))
      (AND (< TARGET-TOP-ITEM 0) (SETQ TARGET-TOP-ITEM 0))
      (SEND SELF :REDISPLAY)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :AFTER :NEW-SCROLL-POSITION) (&REST IGNORE)
  (MOUSE-WAKEUP))

(DEFFLAVOR SCROLL-WINDOW-WITH-TYPEOUT-MIXIN () ()
  (:REQUIRED-FLAVORS WINDOW-WITH-TYPEOUT-MIXIN BASIC-SCROLL-WINDOW)
  (:DOCUMENTATION :MIXIN "A SCROLL-WINDOW which has a typeout window."))

(DEFMETHOD (SCROLL-WINDOW-WITH-TYPEOUT-MIXIN :BEFORE :REDISPLAY) (&REST IGNORE)
  "If the typeout window is active, deexposed it, and make sure the redisplayer knows how many
lines were clobbered."
  (WHEN (SEND TYPEOUT-WINDOW :ACTIVE-P)
    (LET ((BR (MIN SCREEN-LINES
                   (1+ (TRUNCATE (SEND TYPEOUT-WINDOW :BOTTOM-REACHED)
                                 LINE-HEIGHT)))))
      (DOTIMES (L BR)
        ;; Mark lines as clobbered
        (SETF (AREF SCREEN-IMAGE L 0) NIL)
        (SETF (AREF SCREEN-IMAGE L 1) -1)
        (SETF (AREF SCREEN-IMAGE L 2) -1))
      (SEND TYPEOUT-WINDOW :DEACTIVATE)
      (SEND SELF :DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) (* BR LINE-HEIGHT)
                                 0 0
                                 ALU-ANDCA))))

(DEFFLAVOR SCROLL-WINDOW-WITH-TYPEOUT ()
  (SCROLL-WINDOW-WITH-TYPEOUT-MIXIN WINDOW-WITH-TYPEOUT-MIXIN SCROLL-WINDOW)
  (:DEFAULT-INIT-PLIST :TYPEOUT-WINDOW '(TYPEOUT-WINDOW
                                         :DEEXPOSED-TYPEOUT-ACTION (:EXPOSE-FOR-TYPEOUT)
                                         :IO-BUFFER NIL))
  (:DOCUMENTATION :COMBINATION "A scroll window with a typeout window"))

(DEFMETHOD (SCROLL-WINDOW-WITH-TYPEOUT :AFTER :INIT) (IGNORE)
  (SEND TYPEOUT-WINDOW :SET-IO-BUFFER IO-BUFFER))

;;;; More sophisticated user interface functions
(DEFUN SCROLL-GET-ITEM-LOCATIVE (POSITION &AUX (ITEM (LOCATE-IN-INSTANCE SELF 'DISPLAY-ITEM)))
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-WINDOW))
  (AND (NUMBERP POSITION) (SETQ POSITION (NCONS POSITION)))
  (DOLIST (C POSITION)
    (SETQ ITEM (LOCF (NTH (1+ C) (SCROLL-ITEMS (CAR ITEM))))))
  ITEM)

(DEFMETHOD (BASIC-SCROLL-WINDOW :GET-ITEM) (POSITION)
  "Given a position in the tree, returns the specified item."
  (CAR (SCROLL-GET-ITEM-LOCATIVE POSITION)))

(DEFMETHOD (BASIC-SCROLL-WINDOW :SET-ITEM) (POSITION NEW-ITEM)
  (RPLACA (SCROLL-GET-ITEM-LOCATIVE POSITION) NEW-ITEM)
  (SEND SELF :REDISPLAY))

(DEFMETHOD (BASIC-SCROLL-WINDOW :INSERT-ITEM) (POSITION ITEM &AUX WHERE)
  "Inserts an item before the specified position."
  (AND (NUMBERP POSITION) (SETQ POSITION (NCONS POSITION)))
  (SETQ WHERE (LOCATE-IN-INSTANCE SELF 'DISPLAY-ITEM))
  (DOLIST (P POSITION)
    (SETQ WHERE (LOCF (CDAR (CONTENTS WHERE))))
    (DOTIMES (I P)
      (SETQ WHERE (LOCF (CDR (CONTENTS WHERE))))))
  (RPLACA WHERE (CONS ITEM (CAR WHERE)))
  (SEND SELF :REDISPLAY))

(DEFMETHOD (BASIC-SCROLL-WINDOW :DELETE-ITEM) (POSITION &AUX WHERE)
  "Deletes the item at the specified position."
  (AND (NUMBERP POSITION) (SETQ POSITION (NCONS POSITION)))
  (SETQ WHERE (LOCATE-IN-INSTANCE SELF 'DISPLAY-ITEM))
  (DOLIST (P POSITION)
    (SETQ WHERE (LOCF (CDAR (CONTENTS WHERE))))
    (DOTIMES (I P)
      (SETQ WHERE (LOCF (CDR (CONTENTS WHERE))))))
  (RPLACA WHERE (CDAR WHERE))
  (SEND SELF :REDISPLAY))

(DEFUN SCROLL-MAKE-ENTRY (&REST ELTS &AUX ENTRY)
  "Create an entry and initialize its contents from ELTS."
  (SETQ ENTRY (MAKE-SCROLL-ENTRY))
  (FILLARRAY ENTRY ELTS)
  ENTRY)

(DEFUN SCROLL-INTERPRET-ENTRY (DESCRIPTOR ITEM &AUX MOUSE)
  "Given a descriptor (see documentation) returns an entry made by parsing it.
The entry is suitable for inclusion in an array-type item.
ITEM is the item you are planning to use the entry in."
  (COND-EVERY
   ((STRINGP DESCRIPTOR) (SETQ DESCRIPTOR `(:STRING ,DESCRIPTOR)))
   ((SYMBOLP DESCRIPTOR) (SETQ DESCRIPTOR `(:SYMEVAL ,DESCRIPTOR)))
   ((OR (COMPILED-FUNCTION-P DESCRIPTOR)
        (MEMQ (CAR-SAFE DESCRIPTOR) SI:FUNCTION-START-SYMBOLS))
    (SETQ DESCRIPTOR `(:FUNCTION ,DESCRIPTOR)))
   ((CONSP DESCRIPTOR)
    (COND-EVERY ((EQ (FIRST DESCRIPTOR) ':MOUSE)
                 (SETQ MOUSE (SECOND DESCRIPTOR)
                       DESCRIPTOR (CDDR DESCRIPTOR)))
                ((EQ (FIRST DESCRIPTOR) ':MOUSE-ITEM)
                 (SETQ MOUSE (SUBLIS `((ITEM . ,ITEM)) (CADR DESCRIPTOR))
                       DESCRIPTOR (CDDR DESCRIPTOR)))
                ((EQ (FIRST DESCRIPTOR) ':VALUE)
                 (SETQ DESCRIPTOR `(:FUNCTION ,#'VALUE (,(SECOND DESCRIPTOR)) . ,(CDDR DESCRIPTOR)))))
    (SETQ DESCRIPTOR
          (CASE (FIRST DESCRIPTOR)
            (:STRING (SCROLL-MAKE-ENTRY 'SCROLL-ENTRY-CONSTANT-STRING-FUNCTION NIL NIL 0 0 0
                                        (OR (THIRD DESCRIPTOR) (STRING-LENGTH (SECOND DESCRIPTOR)))
                                        NIL (SECOND DESCRIPTOR) NIL NIL NIL))
            (:SYMEVAL (SCROLL-MAKE-ENTRY 'SCROLL-ENTRY-SYMBOL-VALUE-FUNCTION
                                         (SECOND DESCRIPTOR) NIL
                                         0 0 0 (OR (THIRD DESCRIPTOR) 0) (NULL (THIRD DESCRIPTOR))
                                         (NCONS NIL) NIL (FOURTH DESCRIPTOR) NIL))
            (:FUNCTION (SCROLL-MAKE-ENTRY 'SCROLL-ENTRY-CALL-FUNCTION-FUNCTION
                                          (SECOND DESCRIPTOR) (THIRD DESCRIPTOR)
                                          0 0 0 (OR (FOURTH DESCRIPTOR) 0) (NULL (FOURTH DESCRIPTOR))
                                          (NCONS NIL) NIL (FIFTH DESCRIPTOR) NIL)))))
   (MOUSE
     (SETF (SCROLL-ENTRY-MOUSE-INFO DESCRIPTOR) MOUSE))
   (OTHERWISE (FERROR NIL "Unknown kind of descriptor: ~S" DESCRIPTOR)))
  DESCRIPTOR)

(DEFUN SCROLL-PARSE-ITEM (&REST ITEM-SPEC &AUX ITEM (EXTRA-LEADER 0) LEADER-FILL
                                               MOUSE MOUSE-SELF)
  "Given a list of entry descriptors, produce an array-type item."
  (SETQ ITEM-SPEC (DELQ NIL (COPYLIST ITEM-SPEC)))
  (DO ((L ITEM-SPEC (CDDR L)))
      ((NOT (SYMBOLP (CAR L)))
       (SETQ ITEM-SPEC L))
    (CASE (CAR L)
      (:MOUSE (SETQ MOUSE (CADR L)))
      (:MOUSE-SELF (SETQ MOUSE-SELF T MOUSE (CADR L)))
      (:LEADER (SETQ EXTRA-LEADER
                     (IF (NUMBERP (CADR L))
                         (CADR L)
                         (SETQ LEADER-FILL (CADR L))
                         (LENGTH LEADER-FILL))))
      (OTHERWISE (FERROR NIL "~A is unknown keyword to SCROLL-PARSE-ITEM" (CAR L)))))
  (SETQ ITEM (MAKE-ARRAY (LENGTH ITEM-SPEC)
                         :LEADER-LENGTH (+ EXTRA-LEADER SCROLL-ITEM-LEADER-OFFSET)))
  (AND MOUSE-SELF (SETQ MOUSE (SUBLIS `((SELF . ,ITEM)) MOUSE)))
  (AND LEADER-FILL
       (DOTIMES (I EXTRA-LEADER)
         (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET I)) (NTH I LEADER-FILL))))
  (SETF (SCROLL-ITEM-LINE-SENSITIVITY ITEM) MOUSE)
  (DOTIMES (I (LENGTH ITEM-SPEC))
    (SETF (AREF ITEM I) (SCROLL-INTERPRET-ENTRY (CAR ITEM-SPEC) ITEM))
    (SETQ ITEM-SPEC (CDR ITEM-SPEC)))
  ITEM)

(DEFUN SCROLL-STRING-ITEM-WITH-EMBEDDED-NEWLINES (STRING &AUX STRINGS ITEM)
  (DO ((NEXT (STRING-SEARCH-CHAR #/NEWLINE STRING)
             (STRING-SEARCH-CHAR #/NEWLINE STRING (1+ NEXT)))
       (ONEXT 0 (1+ NEXT)))
      ((NULL NEXT)
       (AND (< ONEXT (STRING-LENGTH STRING))
            (PUSH (NSUBSTRING STRING ONEXT) STRINGS)))
    (PUSH (NSUBSTRING STRING ONEXT NEXT) STRINGS))
  (DOLIST (STRING STRINGS)
    (PUSH (SCROLL-PARSE-ITEM STRING) ITEM))
  (LIST* () ITEM))

(DEFUN SCROLL-MAINTAIN-LIST-UNORDERED (INIT-FUN ITEM-FUN &OPTIONAL PER-ELT-FUN STEPPER)
  "Given a function that returns a list, and a function that returns an item spec
when given an element of that list, maintains one item for each element in the list.
This is not useful when recursion is necessary.  Returns an item that should be
inserted somewhere.  The LIST-FUN should return a private copy of the list."
  (LIST `(:PRE-PROCESS-FUNCTION SCROLL-MAINTAIN-LIST-UNORDERED-UPDATE-FUNCTION
          :FUNCTION ,PER-ELT-FUN
          :INIT-FUNCTION ,INIT-FUN
          :ITEM-FUNCTION ,ITEM-FUN
          :OLD-STATE NIL
          :STEPPER-FUNCTION ,(OR STEPPER #'SCROLL-MAINTAIN-LIST-STEPPER))))


(DEFUNP SCROLL-MAINTAIN-LIST-STEPPER (STATE)
  (RETURN (VALUES (CAR STATE) (CDR STATE) (NULL (CDR STATE)))))

(DEFSTRUCT (STATE :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  STATE-VALUE
  STATE-FLAG
  STATE-ITEM)

(DEFUN SCROLL-MAINTAIN-LIST-UNORDERED-UPDATE-FUNCTION (ITEM &AUX (FLAGS-PLIST (LOCF (CAR ITEM))))
  (LET ((STEP-STATE (FUNCALL (GET FLAGS-PLIST ':INIT-FUNCTION)))
        (OLD-STATE (GET FLAGS-PLIST ':OLD-STATE))
        (ITEM-FUN (GET FLAGS-PLIST ':ITEM-FUNCTION))
        (STEPPER (GET FLAGS-PLIST ':STEPPER-FUNCTION)))
    ;; Clear out remembered state
    (DOLIST (E OLD-STATE)
      (SETF (STATE-FLAG E) NIL))
    ;; Loop over all items.  If one is found that doesn't exist, add it and
    ;; remember that.  Any that no longer exist need to be flushed.
    (DO ((CURRENT)
         (LAST)
         (STATE))
        ((OR (NULL STEP-STATE) LAST))
      (MULTIPLE-VALUE (CURRENT STEP-STATE LAST) (FUNCALL STEPPER STEP-STATE))
      (IF (SETQ STATE (ASSQ CURRENT OLD-STATE))
          (SETF (STATE-FLAG STATE) T)
          ;; Doesn't exist.  Add it to the front of the list and add in the item
          (LET ((NEW-ITEM (FUNCALL ITEM-FUN CURRENT)))
            (PUSH (LIST CURRENT T NEW-ITEM) OLD-STATE)
            (PUSH NEW-ITEM (SCROLL-ITEMS ITEM)))))
    (DOLIST (STATE OLD-STATE)
      ;; Delete all items that are no longer valid
      (COND ((NOT (STATE-FLAG STATE))
             (SETF (SCROLL-ITEMS ITEM) (DELQ (STATE-ITEM STATE) (SCROLL-ITEMS ITEM)))
             (SETQ OLD-STATE (DELQ STATE OLD-STATE)))))
    ;; ITEM and OLD-STATE have been updated.  Store back appropriate info.
    (PUTPROP FLAGS-PLIST OLD-STATE ':OLD-STATE)
    ITEM))

(DEFUN SCROLL-MAINTAIN-LIST (INIT-FUN ITEM-FUN
                             &OPTIONAL PER-ELT-FUN STEPPER COMPACT-P
                                       (PRE-PROC-FUN 'SCROLL-MAINTAIN-LIST-UPDATE-FUNCTION))

  "Given a function that returns a list, and a function that returns an item spec
when given an element of that list, maintains one item for each element in the list.
This is not useful when recursion is necessary.  Returns an item that should be
inserted somewhere.  The LIST-FUN should return a private copy of the list."
  (LIST `(:PRE-PROCESS-FUNCTION ,PRE-PROC-FUN
          :FUNCTION ,PER-ELT-FUN
          :INIT-FUNCTION ,INIT-FUN
          :ITEM-FUNCTION ,ITEM-FUN
          :OLD-STATE NIL
          :COMPACT-P ,COMPACT-P
          :STEPPER-FUNCTION ,(OR STEPPER #'SCROLL-MAINTAIN-LIST-STEPPER))))

;;; Now that we've got a garbage collector...
(defvar scroll-list-area working-storage-area)

(DEFUN SCROLL-MAINTAIN-LIST-UPDATE-FUNCTION (ITEM &AUX (FLAGS-PLIST (LOCF (CAR ITEM))))
  (LET* ((STEP-STATE (FUNCALL (GET FLAGS-PLIST ':INIT-FUNCTION)))
         (OLD-STATE (LOCF (GET FLAGS-PLIST ':OLD-STATE)))
         (ITEM-FUN (GET FLAGS-PLIST ':ITEM-FUNCTION))
         (STEPPER (GET FLAGS-PLIST ':STEPPER-FUNCTION))
         (COMPACT-P (GET FLAGS-PLIST ':COMPACT-P))
         (ITEMS (LOCF (SCROLL-ITEMS ITEM))))
    ;; Loop over all items.  If one is found that doesn't exist, add it and
    ;; remember that.  Any that no longer exist need to be flushed.
    (DO ((CURRENT)
         (NEEDS-COMPACTION NIL)
         (LAST (NULL STEP-STATE))
         (PREV-ITEM ITEMS)
         (PREV-STATE OLD-STATE))
        (LAST
          (RPLACD PREV-STATE NIL)
          (RPLACD PREV-ITEM NIL)
          (COND ((AND COMPACT-P NEEDS-COMPACTION)
                 (SETF (SCROLL-ITEMS ITEM) (COPYLIST (SCROLL-ITEMS ITEM) SCROLL-LIST-AREA))
                 (SETF (GET FLAGS-PLIST ':OLD-STATE)
                       (COPYLIST (GET FLAGS-PLIST ':OLD-STATE) SCROLL-LIST-AREA)))))
      (SETQ ITEMS (CDR ITEMS)
            OLD-STATE (CDR OLD-STATE))
      (MULTIPLE-VALUE (CURRENT STEP-STATE LAST) (FUNCALL STEPPER STEP-STATE))
      (COND ((EQ (CAR OLD-STATE) CURRENT)
             ;; No change, ok then
             )
            ((MEMQ CURRENT OLD-STATE)
             ;; Is later on list, therefore must have deleted some things
             (SETQ NEEDS-COMPACTION T)
             (DO ()
                 ((EQ (CAR OLD-STATE) CURRENT))
               (RPLACD PREV-STATE (SETQ OLD-STATE (CDR OLD-STATE)))
               (RPLACD PREV-ITEM (SETQ ITEMS (CDR ITEMS)))))
            (T
             (SETQ NEEDS-COMPACTION T)
             (RPLACD PREV-STATE (SETQ OLD-STATE (CONS-IN-AREA CURRENT (CDR PREV-STATE)
                                                              SCROLL-LIST-AREA)))
             (RPLACD PREV-ITEM (SETQ ITEMS
                                     (CONS-IN-AREA (FUNCALL ITEM-FUN CURRENT)
                                                   (CDR PREV-ITEM) SCROLL-LIST-AREA)))))

      (AND OLD-STATE (SETQ PREV-STATE OLD-STATE))
      (AND ITEMS (SETQ PREV-ITEM ITEMS)))
    ITEM))

(DEFUN SCROLL-MAINTAIN-LIST-UPDATE-STATES (STATES WINDOW
                                           &OPTIONAL (DITEM (SEND WINDOW :DISPLAY-ITEM))
                                           &AUX (FLAGS-PLIST (LOCF (CAR DITEM))))
  (LET ((OLD-STATE (LOCF (GET FLAGS-PLIST ':OLD-STATE)))
        (ITEMS (LOCF (SCROLL-ITEMS DITEM)))
        (ITEMS-TO-BE-REDISPLAYED NIL))
    (DOLIST (STATE OLD-STATE)
      (AND (MEMQ STATE STATES)
           (PUSH (CAR ITEMS) ITEMS-TO-BE-REDISPLAYED))
      (SETQ ITEMS (CDR ITEMS)))
    (SEND WINDOW :REDISPLAY-SELECTED-ITEMS ITEMS-TO-BE-REDISPLAYED)))

;;; Mouse-menu stuff
(DEFFLAVOR ESSENTIAL-SCROLL-MOUSE-MIXIN
        ((TYPE-ALIST) (ITEM-BLINKER NIL) (CURRENT-ITEM NIL))
        ()
  (:INITABLE-INSTANCE-VARIABLES TYPE-ALIST ITEM-BLINKER)
  :GETTABLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES TYPE-ALIST ITEM-BLINKER)
  (:INCLUDED-FLAVORS BASIC-SCROLL-WINDOW SHEET)
  (:DOCUMENTATION :MIXIN "Makes a scroll window suport mouse-sensitive items and entries."))

(DEFFLAVOR SCROLL-MOUSE-MIXIN () (ESSENTIAL-SCROLL-MOUSE-MIXIN MENU-EXECUTE-MIXIN)
  (:DOCUMENTATION :MIXIN "Menu-like scroll windows"))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ ITEM-BLINKER (APPLY #'MAKE-BLINKER SELF
                            (OR (CAR ITEM-BLINKER) 'HOLLOW-RECTANGULAR-BLINKER)
                            :VISIBILITY NIL
                            (CDR ITEM-BLINKER))))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :AFTER :HANDLE-MOUSE) ()
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL)
  (SETQ CURRENT-ITEM NIL))

(DEFSTRUCT (SCROLL-MOUSE-ITEM :LIST (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  SCROLL-MOUSE-ITEM-ITEM
  SCROLL-MOUSE-ITEM-TYPE
  SCROLL-MOUSE-ITEM-LEFT
  SCROLL-MOUSE-ITEM-REL-TOP
  SCROLL-MOUSE-ITEM-WIDTH
  SCROLL-MOUSE-ITEM-HEIGHT)

(DEFUN SCROLL-FIND-SENSITIVE-ITEM (X Y)
  "Return info on the mouse-sensitive item surrounding position X, Y in a scroll window.
A /"mouse-sensitive item/" is not the same thing as an /"item/" in the
sense of a scroll-window; it is anything that makes an area mouse sensitive.
The first value is the (SCROLL-ENTRY-MOUSE-INFO ...) of an entry,
or the SCROLL-MOUSE-ITEM-ITEM of a line, or the SCROLL-ITEM-LINE-SENSITIVITY
of a scroll window item.
The second value is a keyword, the mouse-sensitive item type.
The next four values are the X, Y, WIDTH and HEIGHT of the sensitive area."
  (DECLARE (VALUES SENSITIVE-ITEM SENSITIVE-ITEM-TYPE X Y WIDTH HEIGHT)
           (:SELF-FLAVOR ESSENTIAL-SCROLL-MOUSE-MIXIN))
  (WHEN (AND ( Y (SHEET-INSIDE-TOP))
             (< Y (+ (SHEET-INSIDE-TOP) (* SCREEN-LINES LINE-HEIGHT)))
             ( X (SHEET-INSIDE-LEFT))
             (< X (SHEET-INSIDE-RIGHT)))
    (BLOCK HAVE-ITEM
      (PROG ((LINE-OF-INTEREST (SHEET-LINE-NO NIL Y))
             (MOUSE-INFO) (LINE-ITEM) (LINE) (ENTRY) (START-X (SHEET-INSIDE-LEFT))
             (REL-Y) (FIRST-LINE))
            (SETQ LINE-ITEM (AREF SCREEN-IMAGE LINE-OF-INTEREST 0))
            (OR LINE-ITEM (RETURN-FROM HAVE-ITEM NIL))
            (SETQ FIRST-LINE
                  (SETQ LINE (- LINE-OF-INTEREST (AREF SCREEN-IMAGE LINE-OF-INTEREST 1))))

            ;; First check explicitly set up mouse items
            (SETQ REL-Y (- Y (* LINE LINE-HEIGHT)))
            (DOLIST (I (SCROLL-ITEM-MOUSE-ITEMS LINE-ITEM))
              (AND ( REL-Y (SCROLL-MOUSE-ITEM-REL-TOP I))
                   (< REL-Y (+ (SCROLL-MOUSE-ITEM-REL-TOP I) (SCROLL-MOUSE-ITEM-HEIGHT I)))
                   ( X (SETQ START-X (SCROLL-MOUSE-ITEM-LEFT I)))
                   (< X (+ (SCROLL-MOUSE-ITEM-LEFT I) (SCROLL-MOUSE-ITEM-WIDTH I)))
                   (RETURN-FROM HAVE-ITEM
                     (VALUES (SCROLL-MOUSE-ITEM-ITEM I)
                             (SCROLL-MOUSE-ITEM-TYPE I)
                             START-X
                             (+ (* LINE LINE-HEIGHT) (SCROLL-MOUSE-ITEM-REL-TOP I))
                             (SCROLL-MOUSE-ITEM-WIDTH I)
                             (SCROLL-MOUSE-ITEM-HEIGHT I)))))

            ;; Are we inside an entry which is mouse sensitive?
            ;; Look through this item's entries.
            (DOTIMES (I (ARRAY-DIMENSION LINE-ITEM 0))
              (SETQ ENTRY (AREF LINE-ITEM I))
              (COND ((> LINE LINE-OF-INTEREST)
                     (RETURN NIL))
                    ((< LINE LINE-OF-INTEREST)
                     (SETQ LINE (+ LINE (SCROLL-ENTRY-LINES ENTRY))))
                    ((OR (NULL (SCROLL-ENTRY-MOUSE-INFO ENTRY))
                         ( (IF (> (SCROLL-ENTRY-LINES ENTRY) 0)
                                (SHEET-INSIDE-RIGHT)
                                (SCROLL-ENTRY-FINAL-PRINTING-X ENTRY))
                            X))
                     (SETQ LINE (+ LINE (SCROLL-ENTRY-LINES ENTRY))
                           START-X (SCROLL-ENTRY-FINAL-X ENTRY)))
                    ((> START-X X)
                     (RETURN NIL))
                    (T
                     (RETURN-FROM HAVE-ITEM
                       (VALUES (SETQ MOUSE-INFO (SCROLL-ENTRY-MOUSE-INFO ENTRY))
                               (FIRST MOUSE-INFO)
                               START-X
                               (+ (SHEET-INSIDE-TOP) (* LINE-OF-INTEREST LINE-HEIGHT))
                               (IF (> (SCROLL-ENTRY-LINES ENTRY) 0)
                                   (- (SHEET-INSIDE-RIGHT) START-X)
                                   (- (SCROLL-ENTRY-FINAL-PRINTING-X ENTRY) START-X))
                               LINE-HEIGHT)))))

            ;; No entry is sensitive, perhaps the whole item (line) is sensitive
            (WHEN (SETQ MOUSE-INFO (SCROLL-ITEM-LINE-SENSITIVITY LINE-ITEM))
              (RETURN-FROM HAVE-ITEM
                (VALUES MOUSE-INFO
                        (FIRST MOUSE-INFO)
                        (SHEET-INSIDE-LEFT)
                        (+ (* FIRST-LINE LINE-HEIGHT) (SHEET-INSIDE-TOP))
                        (SHEET-INSIDE-WIDTH)
                        (DO ((I (1+ FIRST-LINE) (1+ I)))
                            ((OR ( I SCREEN-LINES) (NEQ LINE-ITEM (AREF SCREEN-IMAGE I 0)))
                             (* LINE-HEIGHT (- I FIRST-LINE)))))))))))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :MOUSE-SENSITIVE-ITEM) (X Y)
  (SCROLL-FIND-SENSITIVE-ITEM X Y))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :MOUSE-MOVES) (X Y)
  (MOUSE-SET-BLINKER-CURSORPOS)
  (MULTIPLE-VALUE-BIND (ITEM NIL LEFT TOP WID HEI)
      (SEND SELF :MOUSE-SENSITIVE-ITEM X Y)
    (AND TOP (SETQ TOP (1- TOP)))
    (COND (ITEM
           (SEND ITEM-BLINKER :SET-CURSORPOS (- LEFT (SHEET-INSIDE-LEFT))
                                             (- TOP (SHEET-INSIDE-TOP)))
           (SEND ITEM-BLINKER :SET-SIZE WID HEI)
           (SEND ITEM-BLINKER :SET-VISIBILITY T)
           (SETQ CURRENT-ITEM ITEM))
          (T
           (SEND ITEM-BLINKER :SET-VISIBILITY NIL)
           (SETQ CURRENT-ITEM NIL)))))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :MOUSE-CLICK) (BUTTON X Y &AUX OP)
  (UNLESS (LDB-TEST %%KBD-MOUSE-N-CLICKS BUTTON))
  (MULTIPLE-VALUE-BIND (ITEM TYPE)
      (SEND SELF :MOUSE-SENSITIVE-ITEM X Y)
    (COND ((NULL ITEM)
           NIL)
          ((OR (NULL TYPE)
               (SETQ OP (FIRST (CDR (ASSQ TYPE TYPE-ALIST)))))
           ;;psych out :BUTTONS --- Copy of code in (TV:BASIC-MENU :MOUSE-BUTTONS)
           (COND ((AND (CONSP ITEM)
                       ( (LENGTH ITEM) 3)
                       (EQ (SECOND ITEM) ':BUTTONS))
                  (SETQ ITEM (NTH (LDB %%KBD-MOUSE-BUTTON BUTTON) (THIRD ITEM)))))
           (SEND ITEM-BLINKER :SET-VISIBILITY NIL)
           (SEND SELF :EXECUTE (IF OP (LIST* NIL OP ITEM) ITEM))
           T)
          (T
           (SEND SELF :FORCE-KBD-INPUT (LIST TYPE ITEM SELF BUTTON))
           T))))

(DEFMETHOD (ESSENTIAL-SCROLL-MOUSE-MIXIN :WHO-LINE-DOCUMENTATION-STRING) ()
  (GET (CDDR-SAFE CURRENT-ITEM) ':DOCUMENTATION))

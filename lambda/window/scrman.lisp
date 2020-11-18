;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; The screen manager, an unseen entity that many have cursed and
;;; many have praised, is herewithin presented for all to see.

;;; Priorities:
;;; Each sheet has a priority (which is basically how hard it tries to be on the screen).
;;; The priorities are used for ordering the list of inferiors of a sheet, and therefore
;;; affect what windows the mouse sees, how the screen manager works, and how the automatic
;;; exposing decides what to expose.  If the priority is null, then it is considered
;;; to have a priority smaller than that of any with explicit priorities (this includes
;;; negative priorities, but that doesn't really matter).  Positive priorities mean an
;;; absolute priority as compared to all other sheets with a numerical priority -- namely,
;;; the larger the number, the more important the sheet is considered to be.  A negative
;;; priority indicates that the sheet is to be considered "inactive" by various routines
;;; when it is not exposed.  -1 means only be considered inactive by the screen manager,
;;; and -2 or less means don't even be a candidate for auto-exposure.  Exposed sheets
;;; are always uncovered, and therefore are guaranteed to have the largest priority
;;; (virtually, and since they don't overlap, all exposed sheets are considered the same).

;;; Notes: A set of rectangles is a list of rectangles.  All functions that
;;; operate on sets expect them in the canonical form (a list of rectangles
;;; that don't mutually overlap).  All user-level functions that return sets
;;; canonicalize them first.

;;; A rectangle is a four-list of (SOURCE LEFT TOP RIGHT BOTTOM)
;;; SOURCE where the rectangle came from.  Rectangles cannot have
;;; their LEFT, TOP, RIGHT, or BOTTOM destructively altered.

(DEFUN CANONICALIZE-RECTANGLE-SET (S)
  "Given a set of rectangles, returns a set in canonical form (that have no overlaps)."
  (DO ((NEW NIL NIL))
      (NIL)
;;; It's not clear whether the sorting helps at all
;    (SETQ S
;         (SORT S #'(LAMBDA (X Y)
;                     (NOT (< (* (- (RECT-RIGHT X) (RECT-LEFT X))
;                                (- (RECT-BOTTOM X) (RECT-TOP X)))
;                             (* (- (RECT-RIGHT Y) (RECT-LEFT Y))
;                                (- (RECT-BOTTOM Y) (RECT-TOP Y))))))))
    (DO ((R (CAR S) (CAR L))
         (L (CDR S) (CDR L))
         (S-TEM))
        ((NULL L))
      (DOLIST (RA L)
        (COND ((RECT-NOT-OVERLAP-RECT-P R RA)
               ;; No overlap, ok
               )
              ((RECT-WITHIN-RECT-P R RA)
               ;; R completely within RA, throw R away
               (SETQ S (DELQ R S))
               (RETURN NIL))
              ((RECT-WITHIN-RECT-P RA R)
               ;; RA completely within R, throw RA away
               (SETQ S (DELQ RA S))
               (RETURN NIL))
              (T
                (SETQ S-TEM
                      ;; Get all sections of RA not inside of R
                      (RECTANGLE-NOT-INTERSECTION R RA))
                (OR S-TEM
                    ;; No result can't happen if above checks don't succeed
                    (FERROR NIL "Null not-intersection impossible: ~S ~S" R RA))
                (DOLIST (RB S-TEM)
                  (AND (RECT-WITHIN-RECT-P RB R)
                       (SETQ S-TEM (DELQ RB S-TEM))))
                (SETQ NEW (NCONC S-TEM NEW)
                      S (DELQ RA S))))))
    ;; When no new rectangles generated, return the old list
    (OR NEW (RETURN S))
    (SETQ S (NCONC NEW S))))

(DEFUN RECTANGLE-NOT-INTERSECTION (RPRIME RAUX &AUX SET)
  "Return a set of rectangles which consists of all the area in RAUX that
is not also in RPRIME.  The set is guaranteed to be canonical."
  (COND ((RECT-NOT-OVERLAP-RECT-P RPRIME RAUX)
         ;; No intersection at all, just return RAUX
         (NCONS RAUX))
        ((RECT-WITHIN-RECT-P RAUX RPRIME)
         ;; No area that isn't in RPRIME
         NIL)
        (T
         (AND (< (RECT-TOP RAUX) (RECT-TOP RPRIME))
              (PUSH (LIST (RECT-SOURCE RAUX)
                          (RECT-LEFT RAUX) (RECT-TOP RAUX)
                          (RECT-RIGHT RAUX) (RECT-TOP RPRIME))
                    SET))
         (AND (> (RECT-BOTTOM RAUX) (RECT-BOTTOM RPRIME))
              (PUSH (LIST (RECT-SOURCE RAUX)
                          (RECT-LEFT RAUX) (RECT-BOTTOM RPRIME)
                          (RECT-RIGHT RAUX) (RECT-BOTTOM RAUX))
                    SET))
         (AND (< (RECT-LEFT RAUX) (RECT-LEFT RPRIME))
              (PUSH (LIST (RECT-SOURCE RAUX)
                          (RECT-LEFT RAUX) (MAX (RECT-TOP RPRIME) (RECT-TOP RAUX))
                          (RECT-LEFT RPRIME) (MIN (RECT-BOTTOM RPRIME) (RECT-BOTTOM RAUX)))
                    SET))
         (AND (> (RECT-RIGHT RAUX) (RECT-RIGHT RPRIME))
              (PUSH (LIST (RECT-SOURCE RAUX)
                          (RECT-RIGHT RPRIME) (MAX (RECT-TOP RPRIME) (RECT-TOP RAUX))
                          (RECT-RIGHT RAUX) (MIN (RECT-BOTTOM RPRIME) (RECT-BOTTOM RAUX)))
                    SET))
         SET)))

;;; The real screen manager:
(DEFRESOURCE SCREEN-MANAGER-BIT-ARRAY-RESOURCE ()
   :CONSTRUCTOR (MAKE-PIXEL-ARRAY (SHEET-WIDTH DEFAULT-SCREEN) (SHEET-HEIGHT DEFAULT-SCREEN)
                                  :TYPE 'ART-1B :area sheet-area)
   :INITIAL-COPIES 0)

(ADD-INITIALIZATION "Screen manager bit arrays"
                    '(CLEAR-RESOURCE 'SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
                    '(:NOW)
                    'ARRAY-ORDER-INITIALIZATION-LIST)

(DEFUN SCREEN-MANAGE-SHEET (SHEET &OPTIONAL BOUND-RECTANGLES ARRAY-TO-DRAW-ON (X 0) (Y 0) ALU
                                  &AUX RECTANGLE-LIST NOT-WHOLE)
  "Perform screen management on a sheet.
Screen management causes the exposed or partially-visible inferiors of SHEET
to have their contents appearing in the appropriate places within SHEET.
Should be called with SHEET locked, and inferiors ordered,
and inside a method handling a message to that sheet.
The rectangles passed in here better be destructable."
  (LET ((LEFT (SHEET-INSIDE-LEFT SHEET))
        (TOP (SHEET-INSIDE-TOP SHEET))
        (RIGHT (SHEET-INSIDE-RIGHT SHEET))
        (BOTTOM (SHEET-INSIDE-BOTTOM SHEET)))
    (DOLIST (R BOUND-RECTANGLES)
      (SETF (RECT-LEFT R) (MAX LEFT (RECT-LEFT R)))
      (SETF (RECT-TOP R) (MAX TOP (RECT-TOP R)))
      (SETF (RECT-RIGHT R) (MIN RIGHT (RECT-RIGHT R)))
      (SETF (RECT-BOTTOM R) (MIN BOTTOM (RECT-BOTTOM R)))
      ;; Is this now an illegal rectangle?  If so, then punt it altogether
      (OR (AND (< (RECT-LEFT R) (RECT-RIGHT R)) (< (RECT-TOP R) (RECT-BOTTOM R)))
          (SETQ BOUND-RECTANGLES (DELQ R BOUND-RECTANGLES)))))
  (COND (BOUND-RECTANGLES
         (SETQ NOT-WHOLE T
               BOUND-RECTANGLES (CANONICALIZE-RECTANGLE-SET BOUND-RECTANGLES)))
        (T
         (SETQ BOUND-RECTANGLES
               (LIST (LIST (LIST SHEET 0 0)
                           (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
                           (SHEET-INSIDE-RIGHT SHEET) (SHEET-INSIDE-BOTTOM SHEET))))))

  ;; Figure out what should be visible
  ;; This loop is executed with S each of the inferiors then with S the sheet itself
  (DO ((INFS (SHEET-INFERIORS SHEET) (CDR INFS))
       (S) (R-TEM) (S-TEM))
      ((NULL BOUND-RECTANGLES))
    (AND (NULL INFS)
         (RETURN (SETQ RECTANGLE-LIST (NCONC BOUND-RECTANGLES RECTANGLE-LIST))))
    (SETQ S (CAR INFS))
    (SETQ S-TEM (SCREEN-MANAGE-SHEET-RECTANGLES S BOUND-RECTANGLES))
    ;; S-TEM is the set of rectangles which are visible portions of S
    (IF (NULL S-TEM)
        NIL
        (DOLIST (R S-TEM)
          (DOLIST (RA BOUND-RECTANGLES)
            (SETQ BOUND-RECTANGLES (DELQ RA BOUND-RECTANGLES)
                  R-TEM (RECTANGLE-NOT-INTERSECTION R RA))
            (AND R-TEM (SETQ BOUND-RECTANGLES (NCONC R-TEM BOUND-RECTANGLES))))
          (OR BOUND-RECTANGLES (RETURN T)))
        (OR (AND (SHEET-EXPOSED-P S) (SHEET-SCREEN-ARRAY SHEET))
            ;; Never need to restore exposed sheets if superior
            ;; has a screen image
            (SETQ RECTANGLE-LIST (NCONC S-TEM RECTANGLE-LIST)))))

  (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)

  ;; Now do the updates
  (AND RECTANGLE-LIST
       (IF ARRAY-TO-DRAW-ON
           (SCREEN-MANAGE-SHEET-FINAL SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)
           (SHEET-FORCE-ACCESS (SHEET)
             (SCREEN-MANAGE-SHEET-FINAL SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)))))

(DEFUN SCREEN-MANAGE-SHEET-FINAL (SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)
  "Update screen area of some rectangles within SCREEN in ARRAY-TO-DRAW-ON.
If ARRAY-TO-DRAW-ON is NIL, SHEET's screen-array is used.
Drawing in that array is done using ALU at offsets X and Y.
Each rectangle is processed as appropriate to the sheet which is its source."
  (OR ARRAY-TO-DRAW-ON (SETQ ARRAY-TO-DRAW-ON (SHEET-SCREEN-ARRAY SHEET)))
  (DO ((MASTER (CAR RECTANGLE-LIST) (CAR RECTANGLE-LIST))
       (CURRENT-SHEET))
      ((NULL MASTER))
    ;; For all deexposed windows that are of interest, tell them to put their bits up
    ;; (all the sheets are locked by us, so no problem with change of state).  If it's
    ;; an inferior, put up its bits, but if it's the sheet being managed, it means
    ;; there is nothing there.  This is rather misleading; it means there are no
    ;; inferiors there, but not that it is really necessarily blank! See
    ;; comments on SCREEN-MANAGE-MAYBE-BLT-RECTANGLE.
    (SETQ CURRENT-SHEET (CAR (RECT-SOURCE MASTER)))
    (COND ((EQ SHEET CURRENT-SHEET)
           (SETQ RECTANGLE-LIST
                 (FUNCALL SHEET ':SCREEN-MANAGE-UNCOVERED-AREA
                               RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)))
          (T
           (SETQ RECTANGLE-LIST
                 (FUNCALL CURRENT-SHEET ':SCREEN-MANAGE-RESTORE-AREA
                          RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU))))))

(DEFUN SCREEN-MANAGE-FLUSH-KNOWLEDGE (SHEET)
  (SETF (SHEET-SCREEN-MANAGER-SCREEN-IMAGE SHEET) NIL))

(DEFUN SCREEN-MANAGE-SHEET-RECTANGLES (SHEET BOUND-RECTANGLES
                                        &AUX (X-OFF (SHEET-X-OFFSET SHEET))
                                             (Y-OFF (SHEET-Y-OFFSET SHEET))
                                             (SUPERIOR (SHEET-SUPERIOR SHEET))
                                             (LEFT -1) (TOP -1)
                                             (RIGHT (1- 1_23.)) (BOTTOM (1- 1_23.))
                                             RECTS)
  "Return a list of rectangles describing where SHEET intersects any of BOUND-RECTANGLES.
The rectangles returned have (SHEET its-x-offset its-y-offset its-bit-array) as source.
All rectangle edges are relative to SHEET's superior, or to -1, -1 for a screen."
  (COND ((OR (MEMQ SHEET (SHEET-EXPOSED-INFERIORS SUPERIOR))
             (FUNCALL SHEET ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
         (COND (SUPERIOR
                (SETQ LEFT (SHEET-INSIDE-LEFT SUPERIOR)
                      TOP (SHEET-INSIDE-TOP SUPERIOR)
                      RIGHT (SHEET-INSIDE-RIGHT SUPERIOR)
                      BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR))))

         ;; Intersect the rectangles with the bounds of the specified sheet,
         ;; and return a list of the resulting rectangles.
         ;; Include in the source description the bit array so we force an update if that
         ;; changes.
         (DOLIST (BOUND BOUND-RECTANGLES)
           (AND (SHEET-OVERLAPS-P SHEET
                                  (RECT-LEFT BOUND) (RECT-TOP BOUND)
                                  (- (RECT-RIGHT BOUND) (RECT-LEFT BOUND))
                                  (- (RECT-BOTTOM BOUND) (RECT-TOP BOUND)))
                (PUSH (LIST (LIST SHEET X-OFF Y-OFF (SHEET-BIT-ARRAY SHEET))
                            (MAX X-OFF (RECT-LEFT BOUND) LEFT)
                            (MAX Y-OFF (RECT-TOP BOUND) TOP)
                            (MIN (+ X-OFF (SHEET-WIDTH SHEET)) (RECT-RIGHT BOUND) RIGHT)
                            (MIN (+ Y-OFF (SHEET-HEIGHT SHEET)) (RECT-BOTTOM BOUND) BOTTOM))
                      RECTS)))
         RECTS)))

;;; Subroutines used by bit restorers and blank area managers

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SHEET)
(DEFUN SCREEN-MANAGE-CLEAR-RECTANGLE (R ARRAY X Y ALU)
  (BITBLT (OR ALU ERASE-ALUF)
          (- (RECT-RIGHT R) (RECT-LEFT R)) (- (RECT-BOTTOM R) (RECT-TOP R))
          ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R))
          ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R)))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SHEET)
(DEFUN SCREEN-MANAGE-MAYBE-BLT-RECTANGLE (R ARRAY X Y ALU)
  "Restore or clear the rectangle R in ARRAY with offset X and Y.
If SELF has a bit-array, restore from that; otherwise, clear it.
This is a reasonable screen management protocol for blank areas for sheets which
might have bit save arrays and get screen managed, such as LISP-LISTENERS with inferiors."
  (COND (BIT-ARRAY
         (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))
         (BITBLT (OR ALU ALU-SETA)
                 (- (RECT-RIGHT R) (RECT-LEFT R)) (- (RECT-BOTTOM R) (RECT-TOP R))
                 ;; The rectangle is defined to be zero based
                 BIT-ARRAY (RECT-LEFT R) (RECT-TOP R)
                 ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R))))
        (T
         (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU)))))

(DEFUN PAGE-IN-PIXEL-ARRAY (ARRAY &OPTIONAL FROM TO)
  "Page in array of pixels ARRAY, or the part from FROM to TO.
FROM or TO can be NIL or a list (WIDTH HEIGHT)."
  (WHEN SYS:ARRAY-INDEX-ORDER
    (SETQ FROM (REVERSE FROM) TO (REVERSE TO)))
  (SI:PAGE-IN-ARRAY ARRAY FROM TO))

(DEFUN PAGE-OUT-PIXEL-ARRAY (ARRAY &OPTIONAL FROM TO)
  "Page out array of pixels ARRAY, or the part from FROM to TO.
FROM or TO can be NIL or a list (WIDTH HEIGHT)."
  (WHEN SYS:ARRAY-INDEX-ORDER
    (SETQ FROM (REVERSE FROM) TO (REVERSE TO)))
  (SI:PAGE-OUT-ARRAY ARRAY FROM TO))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SHEET)
(DEFUN SCREEN-MANAGE-CLEAR-AREA (RECTS ARRAY X Y ALU)
  (DOLIST (R RECTS)
    (COND ((EQ SELF (CAR (RECT-SOURCE R)))
           (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU)
           (SETQ RECTS (DELQ R RECTS)))))
  RECTS))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SHEET)
(DEFUN SCREEN-MANAGE-RESTORE-AREA (RECTANGLE-LIST TO-ARRAY X Y ALU &OPTIONAL CLEAR-AREA)
  "Restore contents of rectangles in RECTANGLE-LIST to TO-ARRAY with offsets X and Y.
ALU is the alu-function used in BITBLTing.
CLEAR-AREA non-NIL says just use zeros if no data is remembered;
otherwise, regenerate the contents of SELF and use them.
Return RECTANGLE-LIST sans the rectangles that applied to this window."
  (COND (BIT-ARRAY
         (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
                                                    BIT-ARRAY
                                                    TO-ARRAY X Y
                                                    T (OR ALU ALU-SETA)))
        (CLEAR-AREA
         (SCREEN-MANAGE-CLEAR-AREA RECTANGLE-LIST TO-ARRAY X Y ALU))
        (T
         ;; If no saved bits, Refresh into a temporary array and use that as the bits
         (UNWIND-PROTECT
           (USING-RESOURCE (ARRAY SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
             (SETQ SCREEN-ARRAY ARRAY)
             (PAGE-IN-PIXEL-ARRAY ARRAY NIL (LIST WIDTH HEIGHT))
             (SHEET-FORCE-ACCESS (SELF T)
               (FUNCALL-SELF ':REFRESH))
             (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
                                                        ARRAY
                                                        TO-ARRAY X Y
                                                        NIL (OR ALU ALU-SETA)))
          (SETQ SCREEN-ARRAY NIL))))))

(DEFUN SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY (RECTANGLE-LIST ARRAY TO-ARRAY X Y
                                                                 PAGE-FLAG ALU
                                                  &AUX (FROM (LIST 0 0)) (TO (LIST 0 0)))
  (DOLIST (R RECTANGLE-LIST)
    (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
           (SETF (CAR FROM) (- (RECT-LEFT R) (CADR (RECT-SOURCE R))))
           (SETF (CADR FROM) (- (RECT-TOP R) (CADDR (RECT-SOURCE R))))
           (SETF (CAR TO) (+ (CAR FROM) (- (RECT-RIGHT R) (RECT-LEFT R))))
           (SETF (CADR TO) (+ (CADR FROM) (- (RECT-BOTTOM R) (RECT-TOP R))))
           (AND PAGE-FLAG (PAGE-IN-PIXEL-ARRAY ARRAY FROM TO))
           (BITBLT (OR ALU ALU-SETA)
                   (- (RECT-RIGHT R) (RECT-LEFT R))
                   (- (RECT-BOTTOM R) (RECT-TOP R))
                   ARRAY
                   ;; Take chunk offset to rectangle origin
                   (CAR FROM) (CADR FROM)
                   TO-ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R)))
           (SETQ RECTANGLE-LIST (DELQ R RECTANGLE-LIST)))))
  (SI:PAGE-OUT-ARRAY ARRAY)
  RECTANGLE-LIST)

;;; Screen manager message handlers and flavors

(DEFWRAPPER (SHEET :SCREEN-MANAGE) (IGNORE . BODY)
  `(LOCK-SHEET (SELF)
     . ,BODY))

;;; Deexposed sheets are defaultly "visible" -- they will show through if they can
;;; and if their priority allows them to
;;; If you redefine this to return T even if there is no bit-array, the right thing
;;; will happen (the window will be refreshed into a temporary array and the
;;; results of that will show through).
(DEFWRAPPER (SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) (IGNORE . BODY)
  `(AND (OR (NULL PRIORITY) ( PRIORITY 0))
        (PROGN . ,BODY)))

(DEFMETHOD (SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) ()
  (NOT (NULL BIT-ARRAY)))

(DEFMETHOD (SHEET :SCREEN-MANAGE) (&REST ARGS)
  "This performs screen management on a sheet.  This always works, even if screen management
is inhibited.  It will also do autoexposure on the sheet, unless screen management is
inhibited.  This allows you to batch a series of screen manages without running autoexposure
each time.  It is expected that autoexposure gets run explicitly in this case."
  (FUNCALL-SELF ':ORDER-INFERIORS)
  (FUNCALL-SELF ':SCREEN-MANAGE-AUTOEXPOSE-INFERIORS)
  (LEXPR-FUNCALL #'SCREEN-MANAGE-SHEET SELF ARGS))

;;; Tell SHEET to do something appropriate for a rectangle
;;; that is not covered by any inferior of SHEET.
;;; Note: Rectangles given to :SCREEN-MANAGE-UNCOVERED-AREA are 0 origin and
;;;       point into SELF.  This is guaranteed, and need not be checked for.
(DEFMETHOD (SHEET :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y ALU)
  ARRAY X Y ALU                                 ;Unused
  (DOLIST (R RECTS)
    (AND (EQ (CAR (RECT-SOURCE R)) SELF)
         (SETQ RECTS (DELQ R RECTS))))
  RECTS)

(DEFMETHOD (SCREEN :SCREEN-MANAGE-UNCOVERED-AREA) SCREEN-MANAGE-CLEAR-UNCOVERED-AREA)
(DEFUN SCREEN-MANAGE-CLEAR-UNCOVERED-AREA (IGNORE RECTS ARRAY X Y ALU)
  "Default is to clear area.  This can be redefined if that isn't desireable."
  (DOLIST (R RECTS)
    (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
           (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU)
           (SETQ RECTS (DELQ R RECTS)))))
  RECTS)

(DEFMETHOD (SHEET :SCREEN-MANAGE-RESTORE-AREA) (RECTS ARRAY X Y ALU)
  "Default way to restore bits.
If there is a bit array, restore from there.  If there is no bit array, simply
clear the area, unless :SCREEN-MANAGE-DEEXPOSED-VISIBILITY returns T in which
case refresh the bits into a temporary array and restore from that"
  (SCREEN-MANAGE-RESTORE-AREA RECTS ARRAY X Y ALU
                              (NOT (FUNCALL-SELF ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY))))


(DEFFLAVOR NO-SCREEN-MANAGING-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Prevents screen managing within windows of this flavor.
That is, the screen manager will not update the parts of the inferiors
of these windows into their proper places."))

(DEFMETHOD (NO-SCREEN-MANAGING-MIXIN :SCREEN-MANAGE) (&REST IGNORE) NIL)
(DEFMETHOD (NO-SCREEN-MANAGING-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA) (&REST IGNORE) NIL)

(DEFFLAVOR SHOW-PARTIALLY-VISIBLE-MIXIN () ()
  (:DOCUMENTATION :MIXIN "If parts of this window are visible but not all, show partial contents."))

(DEFMETHOD (SHOW-PARTIALLY-VISIBLE-MIXIN :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) () T)

;;; Graying stuff

(DEFUN MAKE-GRAY (HEIGHT WIDTH &REST ROWS &AUX GRAY RWIDTH PAT)
  "Return a bitblt array containing a specified gray pattern.
The pattern's size is HEIGHT by WIDTH, and ROWS is a list of numbers,
each of which describes one row of the pattern, one octal digit per pixel, left to right.
WIDTH cannot be greater than 8.
The array's first dimension will be some multiple of WIDTH
that is suitable for a BITBLT array."
  (CHECK-ARG HEIGHT (= HEIGHT (LENGTH ROWS)) "equal to number of rows")
  (CHECK-ARG WIDTH (< WIDTH 32.) "less than or equal to 31")
  (DO L ROWS (CDR L) (NULL L)                   ;CONVERT OCTAL BINARY TO BINARY
    (RPLACA L (DO ((W (CAR L) (LSH W -3))
                   (M 1 (LSH M 1))
                   (R 0))
                  ((ZEROP W) R)
                (AND (BIT-TEST 1 W) (SETQ R (LOGIOR R M))))))
  (DOTIMES (I WIDTH)
    (AND (ZEROP (\ (* 32. (1+ I)) WIDTH))
         (RETURN (SETQ RWIDTH (* 32. (1+ I))))))
  (SETQ GRAY (MAKE-PIXEL-ARRAY RWIDTH HEIGHT :TYPE 'ART-1B))
  (DOTIMES (H HEIGHT)
    (SETQ PAT (NTH H ROWS))
    (DOTIMES (W RWIDTH)
      (AS-2-REVERSE (LSH PAT (- (1+ (\ W WIDTH)) WIDTH)) GRAY W H)))
  GRAY)

;;; Some common grays
(DEFVAR 50%-GRAY)
(DEFVAR 25%-GRAY)
(DEFVAR 75%-GRAY)
(DEFVAR 33%-GRAY)
(DEFVAR HES-GRAY)
(DEFVAR 12%-GRAY)

(DEFUN INIT-GRAYS ()
  (SETQ 50%-GRAY (MAKE-GRAY 2 2 01 10))
  (SETQ 25%-GRAY (MAKE-GRAY 4 4 1000 0010 0100 0001))
  (SETQ 75%-GRAY (MAKE-GRAY 4 4 0111 1101 1011 1110))
  (SETQ 33%-GRAY (MAKE-GRAY 3 3 100 010 001))
  (SETQ HES-GRAY (MAKE-GRAY 4 4 1000 0000 0010 0000))
  (SETQ 12%-GRAY (MAKE-GRAY 4 4 1000 0000 0010 0000)))

(ADD-INITIALIZATION "Init grays" '(INIT-GRAYS) '(:NOW) 'ARRAY-ORDER-INITIALIZATION-LIST)

(DEFFLAVOR GRAY-DEEXPOSED-WRONG-MIXIN ((GRAY-ARRAY HES-GRAY)) ()
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:INCLUDED-FLAVORS SHEET)
  (:DOCUMENTATION :MIXIN "Grayed over when deexposed"))

(DEFWRAPPER (GRAY-DEEXPOSED-WRONG-MIXIN :SCREEN-MANAGE-RESTORE-AREA)
                ((RECTS ARRAY X Y ALU) . BODY)
  `(LET ((SI:.DAEMON-CALLER-ARGS. (LIST NIL RECTS ARRAY X Y (OR ALU CHAR-ALUF))))
     (DOLIST (R RECTS)
       (AND (EQ (CAR (RECT-SOURCE R)) SELF)
            (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)))
     . ,BODY))

(DEFMETHOD (GRAY-DEEXPOSED-WRONG-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y IGNORE)
  (DOLIST (R RECTS)
    (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
           (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)
           (SETQ RECTS (DELQ R RECTS)))))
  RECTS)

;;; This flavor causes the deexposed window to be grayed over.  It works for windows
;;; which have inferiors, as well as those which do not.
(DEFFLAVOR GRAY-DEEXPOSED-RIGHT-MIXIN ((GRAY-ARRAY HES-GRAY)) ()
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:INCLUDED-FLAVORS SHEET)
  (:DOCUMENTATION :MIXIN "Grayed over when deexposed"))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (GRAY-DEEXPOSED-RIGHT-MIXIN)
(DEFUN SCREEN-MANAGE-GRAY-RECTANGLE (RECT ARRAY X Y ALU)
  "Gray the specified rectangle on the specified array.
All graying is relative to (0, 0) on the sheet that the rectangle is on."
  (LET ((X-OFF (- (RECT-LEFT RECT) (SECOND (RECT-SOURCE RECT))))
        (Y-OFF (- (RECT-TOP RECT) (THIRD (RECT-SOURCE RECT)))))
    (BITBLT (OR ALU CHAR-ALUF)
            (- (RECT-RIGHT RECT) (RECT-LEFT RECT)) (- (RECT-BOTTOM RECT) (RECT-TOP RECT))
            GRAY-ARRAY (\ X-OFF (ARRAY-DIMENSION GRAY-ARRAY 1))
                       (\ Y-OFF (ARRAY-DIMENSION GRAY-ARRAY 0))
            ARRAY (+ X (RECT-LEFT RECT)) (+ Y (RECT-TOP RECT))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (GRAY-DEEXPOSED-RIGHT-MIXIN)
(DEFUN GRAY-DEEXPOSED-RIGHT-RESTORE-INTERNAL (RECTS KLUDGE-ARRAY ARRAY X Y ALU)
  "This is an internal function for the wrapper of the grayer.  It grays the
window in the internal bit array, and then causes the appropriate rectangles
to be blted onto the screen."
  (SCREEN-MANAGE-GRAY-RECTANGLE `((,SELF 0 0) 0 0 ,WIDTH ,HEIGHT)
                                KLUDGE-ARRAY 0 0 CHAR-ALUF)
  (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTS KLUDGE-ARRAY ARRAY X Y NIL
                                             (OR ALU ALU-SETA))))

(DEFMETHOD (GRAY-DEEXPOSED-RIGHT-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y IGNORE)
  (DOLIST (R RECTS)
    (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
           (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)
           (SETQ RECTS (DELQ R RECTS)))))
  RECTS)

(DEFWRAPPER (GRAY-DEEXPOSED-RIGHT-MIXIN :SCREEN-MANAGE-RESTORE-AREA)
                ((RECTS ARRAY X Y ALU) . BODY)
  `(USING-RESOURCE (KLUDGE-ARRAY SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
     (PAGE-IN-PIXEL-ARRAY KLUDGE-ARRAY NIL (LIST WIDTH HEIGHT))
     ;; This is a kludge -- fudge the arguments to all methods inside
     (LET ((SI:.DAEMON-CALLER-ARGS. (LIST NIL (COPYLIST RECTS) KLUDGE-ARRAY
                                          (- X-OFFSET) (- Y-OFFSET) ALU)))
       . ,BODY)
     (GRAY-DEEXPOSED-RIGHT-RESTORE-INTERNAL RECTS KLUDGE-ARRAY ARRAY X Y ALU)))

;;; Interfaces to the other software
(DEFVAR SCREEN-MANAGE-TRACE-OUTPUT NIL
  "If non-NIL, stream for debugging journal of stream-manager actions.")

(DEFUN SCREEN-ACTIVITY-HAS-CHANGED (WINDOW ON-P)
  "Update screen manager when WINDOW becomes active or inactive.
ON-P is T for WINDOW becoming active."
  ON-P                                          ;Isn't very interesting
  (AND SCREEN-MANAGE-TRACE-OUTPUT
       (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
               "~&Activity change: window ~S, ~:[Deactivate~;Activate~]~%"
               WINDOW ON-P))
  (COND ((FUNCALL WINDOW ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY)
         ;; If window is visible when deexposed, then screen management is useful
         (SCREEN-MANAGE-WINDOW-AREA WINDOW))
        (T (SCREEN-MANAGE-FLUSH-KNOWLEDGE WINDOW))))

(DEFUN SCREEN-CONFIGURATION-HAS-CHANGED (WINDOW &OPTIONAL (WHY ':FORCE)
                                                &AUX DO-IT (SUP (SHEET-SUPERIOR WINDOW)))
  "Update screen when WINDOW becomes exposed or deexposed.
WHY should be :EXPOSE if WINDOW is being exposed, :DEEXPOSE if WINDOW
is being deexposed, or :FORCE (actually same as :DEEXPOSE here)."
  (COND ((MEMQ WINDOW (SHEET-INFERIORS SUP))
         ;; Only consider active windows
         (SELECTQ WHY
           (:EXPOSE
            ;; Just exposed a window, don't need to hack it's area any (except that one can
            ;; optimize by removing all areas from consideration for screen management
            ;; that it subsumes)
            (LET ((RECT (LIST NIL (SHEET-X-OFFSET WINDOW) (SHEET-Y-OFFSET WINDOW)
                                  (+ (SHEET-X-OFFSET WINDOW) (SHEET-WIDTH WINDOW))
                                  (+ (SHEET-Y-OFFSET WINDOW) (SHEET-HEIGHT WINDOW)))))
              (WITHOUT-INTERRUPTS
                (DOLIST (QE SCREEN-MANAGER-QUEUE)
                  (AND (EQ (CAR (RECT-SOURCE QE)) SUP)
                       (RECT-WITHIN-RECT-P QE RECT)
                       (SETQ SCREEN-MANAGER-QUEUE (DELQ QE SCREEN-MANAGER-QUEUE)))))))
           ((:DEEXPOSE :FORCE)
            ;; Deexposing, things may have changed underneath it, so screen manage even
            ;; if it is temporary
            (SETQ DO-IT T)))
         (AND SUP (FUNCALL SUP ':ORDER-INFERIORS))
         (AND SCREEN-MANAGE-TRACE-OUTPUT
              (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
                      "~&Configuration change: window ~S, reason ~A~%"
                      WINDOW WHY))
         (COND (DO-IT
                (SCREEN-MANAGE-WINDOW-AREA WINDOW))
               (T (SCREEN-MANAGE-FLUSH-KNOWLEDGE SUP))))))

(DEFUN SCREEN-AREA-HAS-CHANGED (WINDOW &REST RECT
                                       &AUX (SUP (SHEET-SUPERIOR WINDOW)))
  "Update screen when WINDOW's edges have changed.
RECT should be the edges of the area to update."
  (COND ((MEMQ WINDOW (SHEET-INFERIORS SUP))
         (AND SUP (FUNCALL SUP ':ORDER-INFERIORS))
         (COND ((OR (AND RECT (SHEET-EXPOSED-P WINDOW)) ;Explicit rectangle, and exposed
                    (FUNCALL WINDOW ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
                (AND SCREEN-MANAGE-TRACE-OUTPUT
                     (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
                             "~&Area change: window ~S~%"
                             WINDOW))
                (LEXPR-FUNCALL #'SCREEN-MANAGE-WINDOW-AREA WINDOW RECT))))))

(DEFUN SCREEN-MANAGE-WINDOW-AREA (WINDOW
                                  &OPTIONAL (LEFT (SHEET-X-OFFSET WINDOW))
                                            (TOP (SHEET-Y-OFFSET WINDOW))
                                            (RIGHT (+ (SHEET-X-OFFSET WINDOW)
                                                      (SHEET-WIDTH WINDOW)))
                                            (BOTTOM (+ (SHEET-Y-OFFSET WINDOW)
                                                       (SHEET-HEIGHT WINDOW)))
                                  &AUX (SUP (SHEET-SUPERIOR WINDOW)))
  (AND SUP (SCREEN-MANAGE-QUEUE SUP LEFT TOP RIGHT BOTTOM)))

(DEFUN SCREEN-MANAGE-QUEUE (SHEET &OPTIONAL LEFT TOP RIGHT BOTTOM
                                  &AUX E (INHIBIT-SCHEDULING-FLAG T))
  "Add request to update the specified part of the window SUP.
The four edges are relative to SUP.
The request is processed now or goes on the screen manager queue
to be processed later.  Processing puts the bits
of the correct inferior of SUP onto the rectangle."
  (SETQ E (IF (CONSP SHEET)
              SHEET
              (LIST (LIST SHEET 0 0) LEFT TOP RIGHT BOTTOM)))
  ;; Add to queue, eliminating redundant entries
  (COND ((DOLIST (QE SCREEN-MANAGER-QUEUE)
           (COND ((EQ (CAR (RECT-SOURCE E)) (CAR (RECT-SOURCE QE)))
                  (AND (RECT-WITHIN-RECT-P E QE)
                       (RETURN T))
                  (AND (RECT-WITHIN-RECT-P QE E)
                       (SETQ SCREEN-MANAGER-QUEUE (DELQ QE SCREEN-MANAGER-QUEUE)))))))
        (T
         (AND SCREEN-MANAGE-TRACE-OUTPUT
              (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
                      "~&Queueing rectangle ~S, inhibit ~A~%"
                      E INHIBIT-SCREEN-MANAGEMENT))
         (PUSH E SCREEN-MANAGER-QUEUE)
         ;; Do right away if possible, otherwise leave on queue
         (OR INHIBIT-SCREEN-MANAGEMENT
             (SCREEN-MANAGE-DEQUEUE-ENTRY E)))))

(DEFUN SCREEN-MANAGE-DELAYING-SCREEN-MANAGEMENT-INTERNAL ()
  "Called if stuff got queued during a DELAYING-SCREEN-MANAGEMENT.
Process the queued requests now unless management is still being delayed,
but leave them queued if they want to wait for a lock.
Must be called with interrupts allowed because it may turn them on."
;  (if inhibit-scheduling-flag
;      (ferror nil "screen manager must be called with interrupts allowed"))
  (without-interrupts
    (OR INHIBIT-SCREEN-MANAGEMENT
        ;; Only try to dequeue if not delaying anymore
        (SCREEN-MANAGE-DEQUEUE))))

(DEFUN SCREEN-MANAGE-DEQUEUE (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Process the requests for screen management queued during a DELAYING-SCREEN-MANAGEMENT.
If a request requires a lock that is locked, leave it queued."
  (DO ((Q SCREEN-MANAGER-QUEUE))
      ((NULL Q))
    (IF (SCREEN-MANAGE-DEQUEUE-ENTRY (CAR Q))
        ;; If the entry actually got dequeued, then interrupts were allowed and so the
        ;; queue might have gotten hacked.  Restart from the beginning.
        (SETQ Q SCREEN-MANAGER-QUEUE)
        (SETQ Q (CDR Q)))))

(DEFUN SCREEN-MANAGE-DEQUEUE-DELAYED-ENTRIES (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Process queued requests for screen management, waiting for locks if necessary."
  (DO ((Q SCREEN-MANAGER-QUEUE SCREEN-MANAGER-QUEUE))
      ((NULL Q))
    (SCREEN-MANAGE-DEQUEUE-ENTRY (CAR Q) T)))   ;This reenables scheduling if it does anything

(DEFUN SCREEN-MANAGE-DEQUEUE-ENTRY (ENTRY &OPTIONAL UNCOND &AUX ALL)
  "Handle one entry from the screen manager's queue.
UNCOND non-NIL says wait for a lock if one is needed,
 otherwise return NIL and don't dequeue the entry, in that case.
Must be called with INHIBIT-SCHEDULING-FLAG bound to T.
May set that flag to NIL and then set it back to T again,
and during that time more requests may get queued!
The code returns T if it actually dequeued the entry, else NIL."
  (COND ((OR UNCOND (SHEET-CAN-GET-LOCK (CAR (RECT-SOURCE ENTRY))))
         ;; May as well do all rectangles on this sheet together.  ALL gets a list of them.
         (SETQ SCREEN-MANAGER-QUEUE (DELQ ENTRY SCREEN-MANAGER-QUEUE)
               ALL (NCONS ENTRY))
         (DOLIST (E SCREEN-MANAGER-QUEUE)
           (COND ((EQ (CAR (RECT-SOURCE ENTRY)) (CAR (RECT-SOURCE E)))
                  (SETQ SCREEN-MANAGER-QUEUE (DELQ E SCREEN-MANAGER-QUEUE))
                  (PUSH E ALL))))
         (AND SCREEN-MANAGE-TRACE-OUTPUT
              (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
                      "~&Dequeueing ~S~%Queue is ~S~%" ALL SCREEN-MANAGER-QUEUE))

         (LET ((SHEET (CAR (RECT-SOURCE ENTRY))))
           (IF (SHEET-SCREEN-ARRAY SHEET)
               ;; FORCE-ACCESS so that PREPARE-SHEET won't look at the output-hold flag.
               (SHEET-FORCE-ACCESS (SHEET :NO-PREPARE)
                 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                 (FUNCALL (CAR (RECT-SOURCE ENTRY)) ':SCREEN-MANAGE ALL))
               ;; If can't screen manage (no screen!), then just do autoexposure
               (LOCK-SHEET (SHEET)
                 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                 (FUNCALL SHEET ':ORDER-INFERIORS)
                 (FUNCALL SHEET ':SCREEN-MANAGE-AUTOEXPOSE-INFERIORS)))
           (SETQ INHIBIT-SCHEDULING-FLAG T)
           T))
        (T NIL)))

;;; Note that this message does not mean automatically expose this sheet.
;;; It means consider the inferiors of this sheet for automatic exposing.
(DEFMETHOD (SHEET :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS) ()
  (SCREEN-MANAGE-AUTOEXPOSE-INFERIORS SELF))

(DEFUN SCREEN-MANAGE-AUTOEXPOSE-INFERIORS (SHEET &AUX INTERESTING-INFERIORS)
  "Expose all inferiors of SHEET that are uncovered but not exposed.
SHEET should be locked."
  ;; No need to do any screen management,
  ;; since exposure always does the right thing,
  ;; and this can never cause a sheet to become deexposed.

  ;; First, get an ordered list of all sheets of interest
  ;; SHEET-INFERIORS has been ordered by priority.
  (LOCK-SHEET (SHEET)
    (DOLIST (I (SHEET-INFERIORS SHEET))
      (OR (MEMQ I (SHEET-EXPOSED-INFERIORS SHEET))
;         (NOT (FUNCALL I ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
          (NOT (SHEET-WITHIN-SHEET-P I SHEET))
          ( (OR (SHEET-PRIORITY I) 0) -1)
          (PUSH I INTERESTING-INFERIORS)))
    (SETQ INTERESTING-INFERIORS (NREVERSE INTERESTING-INFERIORS))
    (AND SCREEN-MANAGE-TRACE-OUTPUT
         (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
                 "~&Autoexpose-inferiors: ~S~%" INTERESTING-INFERIORS))
    ;; Now, we have a list of interesting: deexposed and active
    ;; Expose them one by one if they aren't covered.
    (DOLIST (I INTERESTING-INFERIORS)
      (COND ((DOLIST (EI (SHEET-EXPOSED-INFERIORS SHEET))
               (AND (SHEET-OVERLAPS-SHEET-P EI I)
                    (RETURN T))))               ;This clause if covered: do nothing
            ;; Don't expose if it would cover anything earlier in the list.  What this
            ;; does is prevent violations of priority; something earlier in the list
            ;; might not be exposed because some other part of it was covered.
            ((DOLIST (HP INTERESTING-INFERIORS)
               (AND (EQ I HP)
                    (RETURN T))
               (AND (FUNCALL HP ':SCREEN-MANAGE-DEEXPOSED-VISIBILITY)
                    (SHEET-OVERLAPS-SHEET-P I HP)
                    (RETURN NIL)))
             (FUNCALL I ':EXPOSE)
             (SETQ INTERESTING-INFERIORS (DELQ I INTERESTING-INFERIORS)))))
    (AND (EQ SHEET MOUSE-SHEET)
         (NULL SELECTED-WINDOW)
         (SETQ INTERESTING-INFERIORS (SHEET-EXPOSED-INFERIORS SHEET))
         ;; If hacking the sheet the mouse is on, and there is no window currently selected,
         ;; select a window
         (DOLIST (I INTERESTING-INFERIORS)
           (AND (FUNCALL I ':NAME-FOR-SELECTION)
                (RETURN (FUNCALL I ':SELECT)))))))

;;; Screen manager's background process

;;; The background process is responsible for trying to handle the pending queue of
;;; screen manages, as well as updating windows which are in :PERMIT mode, deexposed,
;;; and have actually done typeout since we were last here

(DEFVAR SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS NIL)     ;NIL not to do it, or time to sleep
(DEFVAR SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES 10.)        ;Check queue every 1/6 second
                                                        ;while it is non-empty (i.e. try
                                                        ;again to get locks this often)

(DEFUN SCREEN-MANAGE-BACKGROUND-TOP-LEVEL ()
  (DO ((HEAD-OF-QUEUE SCREEN-MANAGER-QUEUE SCREEN-MANAGER-QUEUE)
       (SLEEP-TIME))
      (())
    (SETQ SLEEP-TIME (COND (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
                             (COND ((NULL HEAD-OF-QUEUE)
                                    SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS)
                                   (T (MIN SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
                                           SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES))))
                           (INHIBIT-SCREEN-MANAGEMENT NIL)
                           ((NULL HEAD-OF-QUEUE) NIL)
                           (T SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES)))
    ;; Wait until queue has changed, and screen management not inhibited
    ;; Except SLEEP-TIME if non-null is a timeout
    (PROCESS-WAIT "Screen Manage"
                  #'(LAMBDA (SLEEP-TIME LAST-TIME HEAD-OF-QUEUE)
                      (OR (AND SLEEP-TIME
                               (> (TIME-DIFFERENCE (TIME) LAST-TIME) SLEEP-TIME))
                          (AND (NULL SLEEP-TIME) SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS)
                          (AND (NOT INHIBIT-SCREEN-MANAGEMENT)
                               (NEQ SCREEN-MANAGER-QUEUE HEAD-OF-QUEUE))))
                  SLEEP-TIME (TIME) HEAD-OF-QUEUE)
    (OR INHIBIT-SCREEN-MANAGEMENT (SCREEN-MANAGE-DEQUEUE))
    (AND SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
         (WITHOUT-INTERRUPTS
           (DOLIST (S ALL-THE-SCREENS)
             (AND (SHEET-EXPOSED-P S)
                  (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS S)))))))

(DEFUN SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS (SHEET &AUX (EXPSD-INFS
                                                            (SHEET-EXPOSED-INFERIORS SHEET)))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS I)
    (COND ((AND (NOT (MEMQ I EXPSD-INFS))
                ( (OR (SHEET-PRIORITY I) 0) 0)
                (EQ (SHEET-DEEXPOSED-TYPEOUT-ACTION I) ':PERMIT)
                (ZEROP (SHEET-OUTPUT-HOLD-FLAG I)))
           ;; Sheet is permitted, deexposed, and has been typed on.  Update it on screen.
           (SETF (SHEET-OUTPUT-HOLD-FLAG I) 1)
           (SCREEN-CONFIGURATION-HAS-CHANGED I)))))

(DEFVAR SCREEN-MANAGER-BACKGROUND-PROCESS
        (PROCESS-RUN-RESTARTABLE-FUNCTION "Screen Manager Background"
                                          'SCREEN-MANAGE-BACKGROUND-TOP-LEVEL))

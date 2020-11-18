;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 by Massachusetts Institute of Technology **

(DEFUN %DRAW-RECTANGLE-CLIPPED (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)
  "Draw rectangle in SHEET, coords relative to SHEET, clipping to SHEET."
  (AND (MINUSP X-BITPOS) (SETQ WIDTH (+ WIDTH X-BITPOS)
                               X-BITPOS 0))
  (AND (MINUSP Y-BITPOS) (SETQ HEIGHT (+ HEIGHT Y-BITPOS)
                               Y-BITPOS 0))
  (SETQ WIDTH (MIN WIDTH (MAX 0 (- (SHEET-WIDTH SHEET) X-BITPOS))))
  (SETQ HEIGHT (MIN HEIGHT (MAX 0 (- (SHEET-HEIGHT SHEET) Y-BITPOS))))
  (AND (> WIDTH 0) (> HEIGHT 0)
       (%DRAW-RECTANGLE WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)))

;;;This takes arguments relative to the inside and clips inside
(DEFUN DRAW-RECTANGLE-INSIDE-CLIPPED (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET
                                      &AUX (INSIDE-LEFT (SHEET-INSIDE-LEFT SHEET))
                                           (INSIDE-TOP (SHEET-INSIDE-TOP SHEET)))
  "Draw rectangle in SHEET, coordinates relative to inside of SHEET, clipping to inside.
Recall that the inside of SHEET is what is not part of the margins."
  (SETQ X-BITPOS (+ X-BITPOS INSIDE-LEFT)
        Y-BITPOS (+ Y-BITPOS INSIDE-TOP))
  (AND (< X-BITPOS INSIDE-LEFT) (SETQ WIDTH (- WIDTH (- INSIDE-LEFT X-BITPOS))
                                     X-BITPOS INSIDE-LEFT))
  (AND (< Y-BITPOS INSIDE-TOP) (SETQ HEIGHT (- HEIGHT (- INSIDE-TOP Y-BITPOS))
                                       Y-BITPOS INSIDE-TOP))
  (SETQ WIDTH (MIN WIDTH (MAX 0 (- (SHEET-INSIDE-RIGHT SHEET) X-BITPOS))))
  (SETQ HEIGHT (MIN HEIGHT (MAX 0 (- (SHEET-INSIDE-BOTTOM SHEET) Y-BITPOS))))
  (AND (> WIDTH 0) (> HEIGHT 0)
       (%DRAW-RECTANGLE WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)))


(DEFUN BITBLT-CLIPPED (ALU WIDTH HEIGHT FROM-ARRAY FROM-X FROM-Y TO-ARRAY TO-X TO-Y)
  "Like BITBLT except clips to the actual area of FROM-ARRAY and TO-ARRAY.
Negative WIDTH and HEIGHT are not allowed."
  (LET* ((LEFT-OMIT (MAX (- (MAX 0 TO-X) TO-X)
                         (- (MAX 0 FROM-X) FROM-X)))
         (TOP-OMIT (MAX (- (MAX 0 TO-Y) TO-Y)
                        (- (MAX 0 FROM-Y) FROM-Y)))
         (FROM-XEND (+ FROM-X WIDTH))
         (FROM-YEND (+ FROM-Y HEIGHT))
         (TO-XEND (+ TO-X WIDTH))
         (TO-YEND (+ TO-Y HEIGHT))
         (RIGHT-OMIT (MAX (- TO-XEND (MIN TO-XEND (PIXEL-ARRAY-WIDTH TO-ARRAY)))
                          (- FROM-XEND (MIN FROM-XEND (PIXEL-ARRAY-WIDTH FROM-ARRAY)))))
         (BOTTOM-OMIT (MAX (- TO-YEND (MIN TO-YEND (PIXEL-ARRAY-HEIGHT TO-ARRAY)))
                           (- FROM-YEND (MIN FROM-YEND (PIXEL-ARRAY-HEIGHT FROM-ARRAY)))))
         (CLIPPED-WIDTH
           (MAX 0 (- WIDTH LEFT-OMIT RIGHT-OMIT)))
         (CLIPPED-HEIGHT
           (MAX 0 (- HEIGHT TOP-OMIT BOTTOM-OMIT))))
    (AND (NOT (ZEROP CLIPPED-WIDTH))                            ;bitblt errs when w=h=0
         (NOT (ZEROP CLIPPED-HEIGHT))                           ;and dims are out of bounds
         (BITBLT ALU
                 CLIPPED-WIDTH CLIPPED-HEIGHT
                 FROM-ARRAY (+ FROM-X LEFT-OMIT) (+ FROM-Y TOP-OMIT)
                 TO-ARRAY (+ TO-X LEFT-OMIT) (+ TO-Y TOP-OMIT)))))

;;;; Primitives

(DEFMETHOD (SHEET :PRINT-SELF) (STREAM IGNORE IGNORE)
  (IF *PRINT-ESCAPE*
      (PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE :NO-POINTER)   ;We do %POINTER explicitly
        (FORMAT STREAM "~A ~O ~A"
                NAME (%POINTER SELF)
                (IF EXPOSED-P "exposed"
                    (IF (OR (NULL SUPERIOR)
                            (MEMQ SELF (SHEET-INFERIORS SUPERIOR)))
                        "deexposed"
                        "deactivated"))))
      (SEND STREAM :STRING-OUT (STRING (OR (SEND SELF :NAME-FOR-SELECTION) NAME)))))

(DEFUN SHEET-CALCULATE-OFFSETS (WINDOW TOP)
  "Return the X and Y offset of the top left corner of WINDOW wrt that of TOP.
TOP should be a superior of WINDOW, to one or more levels (or WINDOW itself).
TOP = NIL is equivalent to supplying WINDOW's screen,
assuming that a screen's offsets are always 0."
  (DO ((W WINDOW (SHEET-SUPERIOR W))
       (X-OFFSET 0)
       (Y-OFFSET 0))
      ((EQ W TOP)
       (VALUES X-OFFSET Y-OFFSET))
      (SETQ X-OFFSET (+ X-OFFSET (SHEET-X-OFFSET W))
            Y-OFFSET (+ Y-OFFSET (SHEET-Y-OFFSET W)))))

(DEFUN SHEET-ME-OR-MY-KID-P (SHEET ME)
  "T if SHEET is ME or an inferior to any number of levels of ME."
  (DO ((SHEET SHEET (SHEET-SUPERIOR SHEET)))
      ((NULL SHEET) NIL)
    (AND (EQ SHEET ME) (RETURN T))))

(DEFUN SHEET-SCREEN (SHEET &OPTIONAL HIGHEST)
  "Return the SCREEN that SHEET is on.
Actually, returns the highest level superior of SHEET that is
either a screen or not active.  If HIGHEST is non-NIL,
inferiors of HIGHEST are treated like screens."
  (DO ((SHEET SHEET SUPERIOR)
       (SUPERIOR SHEET (SHEET-SUPERIOR SUPERIOR)))
      ((OR (NULL SUPERIOR)
           (EQ SUPERIOR HIGHEST))
       SHEET)))
(DEFF SHEET-GET-SCREEN 'SHEET-SCREEN)

(DEFUN MAP-OVER-EXPOSED-SHEETS (FUNCTION)
  "Call FUNCTION on every exposed sheet, on all screens."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (AND (SHEET-EXPOSED-P SCREEN)
         (MAP-OVER-EXPOSED-SHEET FUNCTION SCREEN))))

(DEFUN MAP-OVER-EXPOSED-SHEET (FUNCTION SHEET)
  "Call FUNCTION on SHEET and all exposed inferiors to all levels."
  (DOLIST (SHEET (SHEET-EXPOSED-INFERIORS SHEET))
    (MAP-OVER-EXPOSED-SHEET FUNCTION SHEET))
  (FUNCALL FUNCTION SHEET))

(DEFUN MAP-OVER-SHEETS (FUNCTION)
  "Call FUNCTION on every active sheet, on all screens."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (MAP-OVER-SHEET FUNCTION SCREEN)))

(DEFUN MAP-OVER-SHEET (FUNCTION SHEET)
  "Call FUNCTION on SHEET and all active inferiors to all levels."
  (DOLIST (SHEET (SHEET-INFERIORS SHEET))
    (MAP-OVER-SHEET FUNCTION SHEET))
  (FUNCALL FUNCTION SHEET))

;;;; This page implements locking for the window system.
;;; The lock of a SHEET can be
;;; in one of the following states:
;;; Lock cell is NIL -- no lock, LOCK-COUNT must be zero
;;; Lock cell is an atom and
;;;  the lock count equals the lock count of the superior then
;;;   the sheet is locked, but can be temp locked by any inferior of the lowest superior
;;;   that is actually locked (lock-plus state).
;;;  the lock count is greater than the lock count of the superior then
;;;   the sheet is really locked, and can only be locked by the same unique ID.
;;; Lock cell is a list then
;;;  the sheet is temp locked by the windows in that list
;;;  and if the lock count is non-zero then the window is also lock-plus.

;;; What all this says, essentially, is that you can get the lock on the sheet
;;; and the sheet can be temp locked if all the temp lockers are being locked by
;;; the same operation that is locking the original sheet (these locks can happen in
;;; either order)

(defun sheet-can-get-lock (sheet &optional unique-id)
  "Returns T if a sheet's lock can be gotten (and its inferiors' locks, too).
Should be called with interrupts inhibited if it's to be meaningful."
  (or (eq (sheet-lock sheet) (or unique-id current-process))
      (sheet-can-get-lock-internal sheet (or unique-id current-process) sheet)))

(DEFUN SHEET-CAN-GET-LOCK-INTERNAL (SHEET UID WITH-RESPECT-TO &AUX LOCK)
  (COND ((EQ (SETQ LOCK (SHEET-LOCK SHEET)) UID)
         ;; Lock already owned by unique-id, so return OK
         T)
        ((OR (NULL LOCK)
             ;; If window is temp locked, the current sheet isn't the top-level one, and all
             ;; of the temp lockers are inferiors of the top-level sheet, then it's ok
             ;; to lock this sheet, so recurse
             (AND (CONSP LOCK)
                  (NEQ SHEET WITH-RESPECT-TO)
                  (loop for locker in lock
                        always (sheet-me-or-my-kid-p locker with-respect-to))))
         (NOT (DOLIST (I (SHEET-INFERIORS SHEET))
                (OR (SHEET-CAN-GET-LOCK-INTERNAL I UID WITH-RESPECT-TO)
                    (RETURN T)))))
        (T NIL)))

(defun sheet-get-lock (sheet &optional unique-id)
  "Lock SHEET's lock, waiting if necessary.
The locks of SHEET's inferiors are locked also."
  (or unique-id (setq unique-id current-process))
  (without-interrupts
    (unless (sheet-can-get-lock sheet unique-id)
      (process-wait "Window lock" #'sheet-can-get-lock sheet unique-id))
    (sheet-get-lock-internal sheet unique-id)))

(DEFUN SHEET-GET-LOCK-INTERNAL (SHEET UNIQUE-ID)
  "Really get the lock on a sheet and its inferiors, assuming lock is available to us now.
INHIBIT-SCHEDULING-FLAG must be non-NIL."
  (OR INHIBIT-SCHEDULING-FLAG
      (FERROR "~S called with interrupts enabled." 'SHEET-GET-LOCK-INTERNAL))
  ;; If lock is currently NIL, then initialize it to the unique-id
  (OR (SHEET-LOCK SHEET) (SETF (SHEET-LOCK SHEET) UNIQUE-ID))
  ;; Always bump the lock count here.
  (SETF (SHEET-LOCK-COUNT SHEET) (1+ (SHEET-LOCK-COUNT SHEET)))
  (DOLIST (SHEET (SHEET-INFERIORS SHEET))
    ;; If lock is currently NIL, then initialize it to the unique-id.
    (OR (SHEET-LOCK SHEET) (SETF (SHEET-LOCK SHEET) UNIQUE-ID))
    ;; Always bump the lock count here.
    (SETF (SHEET-LOCK-COUNT SHEET) (1+ (SHEET-LOCK-COUNT SHEET)))
    (DOLIST (SHEET (SHEET-INFERIORS SHEET))
      (SHEET-GET-LOCK-INTERNAL SHEET UNIQUE-ID))))

(DEFUN SHEET-RELEASE-LOCK (SHEET &OPTIONAL (UNIQUE-ID CURRENT-PROCESS)
                                 &AUX (INHIBIT-SCHEDULING-FLAG T) LOCK)
  "Release a lock on a sheet and its inferiors"
  (WHEN (OR (EQ UNIQUE-ID (SETQ LOCK (SHEET-LOCK SHEET)))
            (AND LOCK (NOT (ZEROP (SHEET-LOCK-COUNT SHEET)))))
    ;; If we own the lock, or if temp locked and the lock count is non-zero, then
    ;; we must decrement the lock count
    (SETF (SHEET-LOCK-COUNT SHEET) (1- (SHEET-LOCK-COUNT SHEET)))
    (AND (ZEROP (SHEET-LOCK-COUNT SHEET))
         (NOT (CONSP LOCK))
         ;; If the count is currently zero, and the sheet is not temp-locked, then
         ;; cler out the lock cell
         (SETF (SHEET-LOCK SHEET) NIL))
    (DOLIST (INFERIOR (SHEET-INFERIORS SHEET))
      (SHEET-RELEASE-LOCK INFERIOR UNIQUE-ID))))

(DEFUN SHEET-CAN-GET-TEMPORARY-LOCK (SHEET REQUESTOR &AUX LOCK)
  "Returns T if the lock can be grabbed.  Should be called with interrupts inhibited.
REQUESTOR is the temporary sheet that is going to cover SHEET."
  (IF (NULL (SETQ LOCK (SHEET-LOCK SHEET)))
      ;; Can always get temporary lock if no previous locker
      T
    ;; Determine if sheet is in Lock, Temp-Lock, Lock-Plus, or Temp-Lock-Plus.
    (LET* ((LC (SHEET-LOCK-COUNT SHEET))
           (SUP (SHEET-SUPERIOR SHEET))
           ;; In plus state if sheet's lock count is the same as that of its superior,
           ;; and the lock count is non-zero (this is for the case of a window being
           ;; in temp-lock state, but not being plussified)
           (PLUS (AND (NOT (ZEROP LC)) (= LC (SHEET-LOCK-COUNT SUP)))))
      (COND (PLUS
             ;; In plus state, determine if we are a valid temp locker (we must be
             ;; an inferior (direct or indirect) of the lowest superior that is not
             ;; in the plus state)
             (SHEET-ME-OR-MY-KID-P REQUESTOR
                                   (DO ((OSUP SUP SUP))
                                       (())
                                     (SETQ SUP (SHEET-SUPERIOR OSUP))
                                     (WHEN (OR (NULL SUP)
                                               (> LC (SHEET-LOCK-COUNT SUP)))
                                       ;; Found where the buck stops, return the sheet
                                       (RETURN OSUP)))))
            (T
             ;; Otherwise, only ok to lock if already temp locked
             (CONSP LOCK))))))

(DEFUN SHEET-GET-TEMPORARY-LOCK (SHEET REQUESTOR)
  "Get a temporary lock on SHEET.
REQUESTOR is the temporary sheet that is going to cover SHEET."
  (DO ((INHIBIT-SCHEDULING-FLAG T T))
      ((SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)
       ;; Make sure we lock in appropriate fashion (i.e. if the window is already temp locked
       ;; add another locker, else start the list).  We don't have to worry about
       ;; plus states, since SHEET-CAN-GET-TEMPORARY-LOCK already worried for us.
       (LET ((LOCK (SHEET-LOCK SHEET)))
         (SETF (SHEET-LOCK SHEET)
               (IF (CONSP LOCK)
                   (CONS REQUESTOR LOCK)
                   (NCONS REQUESTOR)))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)))

(DEFUN SHEET-FIND-LOCKER (SHEET)
  (DO ((SUP SHEET) (LOCK)) (())
    (SETQ SUP (SHEET-SUPERIOR SUP))
    (OR SUP (FERROR "Internal error - Lock count non-zero, but nobody is locked!"))
    (AND (ATOM (SETQ LOCK (SHEET-LOCK SUP)))
         (RETURN LOCK))))

(DEFUN SHEET-RELEASE-TEMPORARY-LOCK (SHEET REQUESTOR &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Release a temporary lock on a sheet."
  (LET ((LOCK (DELQ REQUESTOR (SHEET-LOCK SHEET))))
    (SETF (SHEET-LOCK SHEET)
          (OR LOCK (IF (ZEROP (SHEET-LOCK-COUNT SHEET))
                       NIL
                       (SHEET-FIND-LOCKER SHEET))))))

(DEFUN SHEET-FREE-TEMPORARY-LOCKS (SHEET)
  "Free all temporary locks on a sheet by deexposing the sheets that own the lock."
  (DO ((LOCK (SHEET-LOCK SHEET) (SHEET-LOCK SHEET)))
      ((NULL LOCK) T)
    (UNLESS (CONSP LOCK)
      (RETURN NIL))                             ;Not temporary locked, can't do anything
    (UNLESS (TYPEP (SETQ LOCK (CAR LOCK)) 'INSTANCE)
      (RETURN NIL))                             ;The lock isn't an instance, can't do anything
    (UNLESS (SEND LOCK :OPERATION-HANDLED-P :DEEXPOSE)
      (RETURN NIL))                             ;An instance, but maybe not a window -- punt
    (WHEN (CONSP (SHEET-LOCK LOCK))             ;Is the locker also temp locked?
      (OR (SHEET-FREE-TEMPORARY-LOCKS LOCK)     ;Yes, free it up first.  If ok, keep going
          (RETURN NIL)))
    (SEND LOCK :DEEXPOSE)))

(DEFUN SHEET-CLEAR-LOCKS ()
  "Called in an emergency to reset all locks"
  (LABELS ((CLEAR (SHEET)
             (SETF (SHEET-LOCK SHEET) NIL)
             (SETF (SHEET-LOCK-COUNT SHEET) 0)
             (SETF (SHEET-TEMPORARY-WINDOWS-LOCKED SHEET) NIL)
             (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL)
             (DOLIST (SHEET (SHEET-INFERIORS SHEET))
               (CLEAR SHEET))))
    (DOLIST (SHEET ALL-THE-SCREENS)
      (CLEAR SHEET))))

(DEFUN SHEET-ASSURE-LOCK-AVAILABLE (SHEET)
  "Wait till SHEET's lock is available, then return with interrupts off.
Must be called with INHIBIT-SCHEDULING-FLAG bound to T.
However, other processes can run when this function is called."
  (DO () ((SHEET-CAN-GET-LOCK SHEET))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Window Lock" 'SHEET-CAN-GET-LOCK SHEET)
    (SETQ INHIBIT-SCHEDULING-FLAG T)))

(DEFUN-METHOD SHEET-MORE-LOCK-KLUDGE SHEET (FUN &REST ARGS)
  ;; **********************************************************************
  ;; ** The following is a total kludge and should not even be looked at **
  ;; **********************************************************************
  (IF (OR (STRING-EQUAL USER-ID "RMS") (STRING-EQUAL USER-ID "MLY"))
      ;; Let's see what this is accomplishing, the hard way.
      (APPLY FUN ARGS)
    ;; It seems we 1) set the lock state of this window and all inferiors
    ;; to be the same as this window's superior's lock state,
    ;; 2) do the operation,
    ;; 3) set the lock states back to what they were.
    ;; However, when this is called, normally the sheet is prepared
    ;; but not locked.  So it ought to do nothing, and that's what appears to happen.
    (LET ((INHIBIT-SCHEDULING-FLAG T)
          (OLD-LOCK-STATE) (CHAR))
      (UNWIND-PROTECT
          (PROGN
            (AND LOCK
                 (NEQ LOCK CURRENT-PROCESS)
                 (FERROR "Attempt to **MORE** when sheet was not locked by current process."))
            (SETQ OLD-LOCK-STATE
                  (AND LOCK (SHEET-MORE-LOCK-KLUDGE-LOCK-STATE SELF (SHEET-LOCK-COUNT SUPERIOR))))
            (SETQ INHIBIT-SCHEDULING-FLAG NIL)
            (SETQ CHAR (APPLY FUN ARGS)))
        (AND OLD-LOCK-STATE (SHEET-GET-LOCK SELF))
        (SETQ INHIBIT-SCHEDULING-FLAG T)
        (AND OLD-LOCK-STATE
             (SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE SELF OLD-LOCK-STATE))
        (PREPARE-SHEET (SELF)))         ;Open blinkers.
      ;; ******************* End of total, complete, and utter kludge *******************
      CHAR)))

(DEFUN SHEET-MORE-LOCK-KLUDGE-LOCK-STATE (SHEET SUPERIOR-LC &OPTIONAL (STATE NIL))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SETQ STATE (SHEET-MORE-LOCK-KLUDGE-LOCK-STATE I SUPERIOR-LC STATE)))
  (PUSH (CONS SHEET (- (SHEET-LOCK-COUNT SHEET) SUPERIOR-LC)) STATE)
  (OR (CONSP (SHEET-LOCK SHEET)) (SETF (SHEET-LOCK SHEET) NIL))
  (SETF (SHEET-LOCK-COUNT SHEET) SUPERIOR-LC)
  STATE)

(DEFUN SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE (SHEET STATE
                                                  &OPTIONAL (SUPERIOR-LOCK-COUNT 0)
                                                  &AUX LOCK-COUNT)
  ;; This code assumes that the caller has locked the sheet once already
  (SETF (SHEET-LOCK-COUNT SHEET)
        (SETQ LOCK-COUNT
              (+ SUPERIOR-LOCK-COUNT (SHEET-LOCK-COUNT SHEET)
                 (OR (CDR (ASSQ SHEET STATE)) 0)
                 -1)))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE SHEET STATE LOCK-COUNT)))

(DEFUN SHEET-CAN-ACTIVATE-INFERIOR (SUPERIOR &AUX SUP-LOCK)
  "T if SUPERIOR's lock state permits this process to activate inferiors of SUPERIOR."
  (OR (NULL (SETQ SUP-LOCK (SHEET-LOCK SUPERIOR)))
      (AND (CONSP SUP-LOCK) (ZEROP (SHEET-LOCK-COUNT SUPERIOR)))
      (EQ SUP-LOCK CURRENT-PROCESS)
      (AND (CONSP SUP-LOCK) (EQ CURRENT-PROCESS (SHEET-FIND-LOCKER SUPERIOR)))))

(DEFMETHOD (SHEET :INFERIOR-ACTIVATE) (INFERIOR) INFERIOR)
(DEFMETHOD (SHEET :INFERIOR-DEACTIVATE) (INFERIOR) INFERIOR)
(DEFMETHOD (SHEET :INFERIOR-TIME-STAMP) (INFERIOR)
  (DECLARE (IGNORE INFERIOR))                   ;Inferior getting stamped -- unused here
  TIME-STAMP)

(DEFMETHOD (SHEET :UPDATE-TIME-STAMP) ()
  (AND SUPERIOR
       (SETQ TIME-STAMP (SEND SUPERIOR :INFERIOR-TIME-STAMP SELF))))

;;; Other flavors which provide the ability to do input will override this.
(DEFMETHOD (SHEET :DIRECTION) ()
  ':OUTPUT)

(DEFMETHOD (SHEET :CHARACTERS) ()
  T)

(DEFMETHOD (SHEET :ELEMENT-TYPE) ()
  'CHARACTER)

;;; Activation and deactivation (these go with locking)
(DEFMETHOD (SHEET :ACTIVATE) ()
  "Activates a sheet."
  (let ((INHIBIT-SCHEDULING-FLAG T))
    (COND ((NOT (SEND SUPERIOR :INFERIOR-ACTIVATE SELF)))
          ((DO () ((MEMQ SELF (SHEET-INFERIORS SUPERIOR)) NIL)
             (COND ((NOT (SHEET-CAN-GET-LOCK SELF))
                    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                    (PROCESS-WAIT "Window Lock" 'SHEET-CAN-GET-LOCK SELF)
                    (SETQ INHIBIT-SCHEDULING-FLAG T))
                   ((SHEET-CAN-ACTIVATE-INFERIOR SUPERIOR)
                    (OR (ZEROP (SHEET-LOCK-COUNT SUPERIOR))
                        ;; Superior is locked by us, must merge lock counts
                        (LOCK-SHEET (SELF)
                          (LET ((ACTIVATE-LOCK-COUNT (SHEET-LOCK-COUNT SUPERIOR)))
                            (MAP-OVER-SHEET (LAMBDA (SHEET)
                                              (SETF (SHEET-LOCK-COUNT SHEET)
                                                    (+ (SHEET-LOCK-COUNT SHEET)
                                                       ACTIVATE-LOCK-COUNT)))
                                            SELF))))
                    (RETURN T))
                   (T
                    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                    ;; Wait for sheet to become activatable or to become activated
                    (PROCESS-WAIT "Activate" (LAMBDA (SHEET SUP)
                                               (OR (SHEET-CAN-ACTIVATE-INFERIOR SUP)
                                                   (MEMQ SHEET (SHEET-INFERIORS SUP))))
                                  SELF SUPERIOR)
                    ;; Loop back to prevent timing screws
                    (SETQ INHIBIT-SCHEDULING-FLAG T))))
           ;; Executed if we are not active already
           (SHEET-SET-SUPERIOR-PARAMS SELF (SHEET-LOCATIONS-PER-LINE SUPERIOR))
           (SHEET-CONSING
             (SETF (SHEET-INFERIORS SUPERIOR)
                   (COPY-LIST (CONS SELF (SHEET-INFERIORS SUPERIOR)))))))))

(DEFWRAPPER (SHEET :DEACTIVATE) (IGNORE . BODY)
  `(LOCK-SHEET (SELF)
     (DELAYING-SCREEN-MANAGEMENT . ,BODY)))

(DEFMETHOD (SHEET :DEACTIVATE) ()
  "Deactivates a sheet.  Should be called by all deactivate methods to do the actual work."
  (LET ((INHIBIT-SCHEDULING-FLAG T))
    (WHEN (SEND SUPERIOR :INFERIOR-DEACTIVATE SELF)
      (DO () ((NOT (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR))))
        (SETQ INHIBIT-SCHEDULING-FLAG NIL)
        (SEND SELF :DEEXPOSE)
        (SETQ INHIBIT-SCHEDULING-FLAG T))
      (WHEN (MEMQ SELF (SHEET-INFERIORS SUPERIOR))
        (OR (ZEROP (SHEET-LOCK-COUNT SUPERIOR))
            ;; Superior is locked by us, must subtract his lock count from ours
            ;; because he isn't going to do it for us when he gets unlocked.
            ;; (Note: the superior can't be locked by someone else as in the
            ;; deactivate case because we own the lock on one of his inferiors (namely,
            ;; us) preventing this situation from arising)
            ;; That lock also prevents the lock count from going to zero in here.
            (LET ((ACTIVATE-LOCK-COUNT (SHEET-LOCK-COUNT SUPERIOR)))
              (MAP-OVER-SHEET (LAMBDA (SHEET)
                                (SETF (SHEET-LOCK-COUNT SHEET)
                                      (- (SHEET-LOCK-COUNT SHEET)
                                         ACTIVATE-LOCK-COUNT)))
                              SELF)))
        (SETF (SHEET-INFERIORS SUPERIOR) (DELQ SELF (SHEET-INFERIORS SUPERIOR)))))))

(DEFUN SHEET-OVERLAPS-P (SHEET LEFT TOP WIDTH HEIGHT
                               &AUX (W-X (SHEET-X-OFFSET SHEET))
                                    (W-Y (SHEET-Y-OFFSET SHEET))
                                    (W-X1 (+ W-X (SHEET-WIDTH SHEET)))
                                    (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if a sheet overlaps the given area.
The specified coordinates are relative to SHEET's superior."
  (NOT (OR ( LEFT W-X1)
           ( W-X (+ LEFT WIDTH))
           ( TOP W-Y1)
           ( W-Y (+ TOP HEIGHT)))))

(DEFUN SHEET-OVERLAPS-EDGES-P (SHEET LEFT TOP RIGHT BOTTOM
                               &AUX (W-X (SHEET-X-OFFSET SHEET))
                                    (W-Y (SHEET-Y-OFFSET SHEET))
                                    (W-X1 (+ W-X (SHEET-WIDTH SHEET)))
                                    (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if a sheet overlaps the given four coordinates.
The specified coordinates are relative to SHEET's superior."
  (NOT (OR ( LEFT W-X1)
           ( W-X RIGHT)
           ( TOP W-Y1)
           ( W-Y BOTTOM))))

(DEFUN SHEET-OVERLAPS-SHEET-P (SHEET-A SHEET-B)
  "True if two sheets overlap.  They need not have the same superior."
  (IF (EQ (SHEET-SUPERIOR SHEET-A) (SHEET-SUPERIOR SHEET-B))
      ;; If superiors are the same, simple comparison
      (SHEET-OVERLAPS-P SHEET-A (SHEET-X-OFFSET SHEET-B) (SHEET-Y-OFFSET SHEET-B)
                                (SHEET-WIDTH SHEET-B) (SHEET-HEIGHT SHEET-B))
    (MULTIPLE-VALUE-BIND (X-OFF-A Y-OFF-A)
        (SHEET-CALCULATE-OFFSETS SHEET-A NIL)
      (MULTIPLE-VALUE-BIND (X-OFF-B Y-OFF-B)
          (SHEET-CALCULATE-OFFSETS SHEET-B NIL)
        (NOT (OR ( X-OFF-A (+ X-OFF-B (SHEET-WIDTH SHEET-B)))
                 ( X-OFF-B (+ X-OFF-A (SHEET-WIDTH SHEET-A)))
                 ( Y-OFF-A (+ Y-OFF-B (SHEET-HEIGHT SHEET-B)))
                 ( Y-OFF-B (+ Y-OFF-A (SHEET-HEIGHT SHEET-A)))))))))

(DEFUN SHEET-WITHIN-P (SHEET OUTER-LEFT OUTER-TOP OUTER-WIDTH OUTER-HEIGHT
                             &AUX (W-X (SHEET-X-OFFSET SHEET))
                                  (W-Y (SHEET-Y-OFFSET SHEET))
                                  (W-X1 (+ W-X (SHEET-WIDTH SHEET)))
                                  (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if the sheet is fully within the specified rectangle.
The specified coordinates are relative to SHEET's superior."
  (AND ( OUTER-LEFT W-X)
       ( W-X1 (+ OUTER-LEFT OUTER-WIDTH))
       ( OUTER-TOP W-Y)
       ( W-Y1 (+ OUTER-TOP OUTER-HEIGHT))))

(DEFUN SHEET-BOUNDS-WITHIN-SHEET-P (W-X W-Y WIDTH HEIGHT OUTER-SHEET
                                    &AUX (OUTER-LEFT (SHEET-INSIDE-LEFT OUTER-SHEET))
                                         (OUTER-TOP (SHEET-INSIDE-TOP OUTER-SHEET))
                                         (OUTER-WIDTH (SHEET-INSIDE-WIDTH OUTER-SHEET))
                                         (OUTER-HEIGHT (SHEET-INSIDE-HEIGHT OUTER-SHEET)))
  "True if the specified rectangle is fully within the non-margin part of the sheet.
The specified coordinates are relative to OUTER-SHEET's superior."
  (AND ( OUTER-LEFT W-X)
       ( (+ W-X WIDTH) (+ OUTER-LEFT OUTER-WIDTH))
       ( OUTER-TOP W-Y)
       ( (+ W-Y HEIGHT) (+ OUTER-TOP OUTER-HEIGHT))))

(DEFUN SHEET-WITHIN-SHEET-P (SHEET OUTER-SHEET)
  "True if SHEET is fully within the non-margin area of OUTER-SHEET."
  (SHEET-WITHIN-P SHEET (SHEET-INSIDE-LEFT OUTER-SHEET) (SHEET-INSIDE-TOP OUTER-SHEET)
                        (SHEET-INSIDE-WIDTH OUTER-SHEET)
                        (SHEET-INSIDE-HEIGHT OUTER-SHEET)))

(DEFUN SHEET-CONTAINS-SHEET-POINT-P (SHEET TOP-SHEET X Y)
  "T if (X,Y) lies in SHEET.  X and Y are co-ordinates in TOP-SHEET."
  (DO ((S SHEET (SHEET-SUPERIOR S))
       (X X (- X (SHEET-X-OFFSET S)))
       (Y Y (- Y (SHEET-Y-OFFSET S))))
      ((NULL S))                        ;Not in the same hierarchy, return nil
    (AND (EQ S TOP-SHEET)
         (RETURN (AND ( X 0) ( Y 0)
                      (< X (SHEET-WIDTH SHEET)) (< Y (SHEET-HEIGHT SHEET)))))))

(DEFUN DESELECT-SHEET-BLINKERS (SHEET)
  "Set visibility of blinkers of SHEET to their /"deselected/" values."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (EQ (BLINKER-VISIBILITY BLINKER) :BLINK)
         (SETF (BLINKER-VISIBILITY BLINKER)
               (BLINKER-DESELECTED-VISIBILITY BLINKER)))))

(DEFUN TURN-OFF-SHEET-BLINKERS (SHEET)
  "Turn visibility of blinkers of SHEET off."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (MEMQ (BLINKER-VISIBILITY BLINKER) '(:BLINK :ON))
         (SETF (BLINKER-VISIBILITY BLINKER) :OFF))))

(DEFUN SELECT-SHEET-BLINKERS (SHEET)
  "Set visibility of blinkers of SHEET to their /"selected/" values."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (MEMQ (BLINKER-VISIBILITY BLINKER) '(:ON :OFF))
         (SETF (BLINKER-VISIBILITY BLINKER) :BLINK))))

(DEFUN SHEET-OPEN-ALL-BLINKERS (FROM-SHEET)
  "Make sure all blinkers that might appear on FROM-SHEET are temporarily off the screen."
  (DO SHEET FROM-SHEET (SHEET-SUPERIOR SHEET) (NULL SHEET)
      (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
        (OPEN-BLINKER BLINKER))
      ;; If this sheet is not exposed, don't have to open blinkers on superior
      (OR (SHEET-EXPOSED-P SHEET) (RETURN NIL))))

(DEFUN SHEET-OPEN-BLINKERS (SHEET)
  "Make sure all blinkers belonging to SHEET are temporarily off the screen."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (OPEN-BLINKER BLINKER)))

(DEFUN SHEET-FOLLOWING-BLINKER (SHEET)
  "Return the blinker which follows SHEET's cursor, if SHEET has one, else NIL.
If there is more than one, which would be strange, only one is returned."
  (DOLIST (B (SHEET-BLINKER-LIST SHEET))
    (AND (BLINKER-FOLLOW-P B) (RETURN B))))

(DEFUN SHEET-PREPARE-SHEET-INTERNAL (SHEET &AUX LOCK)
  "This is an internal function for PREPARE-SHEET, and must be called with
INHIBIT-SCHEDULING-FLAG bound."
  (WHEN COLD-LOAD-STREAM-OWNS-KEYBOARD
    (UNLESS (SHEET-ME-OR-MY-KID-P SHEET WHO-LINE-SCREEN)
      (PROCESS-WAIT "Screen Lock" (LAMBDA () (NOT COLD-LOAD-STREAM-OWNS-KEYBOARD)))))
  (DO () ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
               (NOT (SHEET-OUTPUT-HELD-P SHEET))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
        (SEND SHEET :OUTPUT-HOLD-EXCEPTION)
        (PROCESS-WAIT "Window Lock" 'SHEET-CAN-GET-LOCK SHEET))
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (IF (SHEET-INFERIORS SHEET)
      (MAP-OVER-EXPOSED-SHEET
        (LAMBDA (SHEET)
          (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
            (OPEN-BLINKER BLINKER)))
        SHEET)
    ;; No need to do full hair if no inferiors
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (OPEN-BLINKER BLINKER)))
  (WHEN (SHEET-EXPOSED-P SHEET)
    (SHEET-OPEN-ALL-BLINKERS (SHEET-SUPERIOR SHEET))))

(DEFMETHOD (SHEET :EDGES) ()
  (VALUES X-OFFSET Y-OFFSET (+ X-OFFSET WIDTH) (+ Y-OFFSET HEIGHT)))

(DEFMETHOD (SHEET :SIZE) ()
  (VALUES WIDTH HEIGHT))

(DEFMETHOD (SHEET :INSIDE-SIZE) ()
  (VALUES (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))

(DEFMETHOD (SHEET :INSIDE-WIDTH) ()
  (SHEET-INSIDE-WIDTH))

(DEFMETHOD (SHEET :INSIDE-HEIGHT) ()
  (SHEET-INSIDE-HEIGHT))

(DEFMETHOD (SHEET :SQUARE-PANE-SIZE) (MAX-WIDTH MAX-HEIGHT IGNORE IGNORE STACKING)
  (ECASE STACKING
    (:VERTICAL MAX-WIDTH)
    (:HORIZONTAL MAX-HEIGHT)))

(DEFMETHOD (SHEET :SQUARE-PANE-INSIDE-SIZE) (MAX-WIDTH MAX-HEIGHT IGNORE IGNORE STACKING)
  (ECASE STACKING
    (:VERTICAL
     (+ TOP-MARGIN-SIZE (- MAX-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE) BOTTOM-MARGIN-SIZE))
    (:HORIZONTAL
     (+ LEFT-MARGIN-SIZE (- MAX-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE) RIGHT-MARGIN-SIZE))
    ))

(DEFMETHOD (SHEET :INSIDE-EDGES) ()
  (VALUES (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP) (SHEET-INSIDE-RIGHT)
          (SHEET-INSIDE-BOTTOM)))

(DEFMETHOD (SHEET :POSITION) ()
  (VALUES X-OFFSET Y-OFFSET))

(DEFMETHOD (SHEET :MARGINS) ()
  (VALUES LEFT-MARGIN-SIZE TOP-MARGIN-SIZE RIGHT-MARGIN-SIZE BOTTOM-MARGIN-SIZE))

;;;; Screen management issues

(DEFMETHOD (SHEET :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (SHEET :ORDER-INFERIORS) ()
  (WITHOUT-INTERRUPTS (SETQ INFERIORS (STABLE-SORT INFERIORS #'SHEET-PRIORITY-LESSP))))

(DEFMETHOD (SHEET :SET-PRIORITY) (NEW-PRIORITY)
  (CHECK-TYPE NEW-PRIORITY (OR NUMBER NULL))
  (SETQ PRIORITY NEW-PRIORITY)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF))

(DEFMETHOD (SHEET :BEFORE :REFRESH) (&OPTIONAL IGNORE)
  (SCREEN-MANAGE-FLUSH-KNOWLEDGE SELF))

(DEFUN SHEET-PRIORITY-LESSP (S1 S2 &AUX (EI (SHEET-EXPOSED-INFERIORS (SHEET-SUPERIOR S1)))
                                        (PRI-S1 (SHEET-PRIORITY S1))
                                        (PRI-S2 (SHEET-PRIORITY S2))
                                        (EX1 (MEMQ S1 EI))
                                        (EX2 (MEMQ S2 EI)))
  (COND ((AND EX1 (NOT EX2))
         ;; First exposed, second not -- S1 on top
         T)
        ((AND (NOT EX1) EX2)
         ;; Second exposed, first not -- S1 underneath
         NIL)
        ((OR (EQ PRI-S1 PRI-S2)
             (AND EX1 EX2))
         ;; Both exposed, or equal priority -- S2 remains on bottom
         NIL)
        ((AND (NULL PRI-S1) PRI-S2)
         ;; S2 has explicit priority, and S1 doesn't -- S1 on bottom
         NIL)
        ((AND PRI-S1 (NULL PRI-S2))
         ;; S1 has explicit priority, and S2 doesn't -- S1 on top
         T)
        (T
         ;; Both have explicit priority -- S2 on bottom if it's priority is less,
         ;; stable if equal
         ( PRI-S2 PRI-S1))))

(DEFUN MAKE-WINDOW (FLAVOR-NAME &REST OPTIONS &AUX (PLIST (LOCF OPTIONS)))
  "Obsolete function for creating windows.  Now you can simply use MAKE-INSTANCE."
  (INSTANTIATE-FLAVOR FLAVOR-NAME PLIST T))

(DEFF WINDOW-CREATE 'MAKE-WINDOW)
(COMPILER:MAKE-OBSOLETE WINDOW-CREATE "use MAKE-INSTANCE.")

(DEFMETHOD (SHEET :INVERSE-AROUND :INIT) (CONT MT ARGS INIT-PLIST)
  ;; Make sure no problem if we PUTPROP the init-plist.
  (SETF (CONTENTS INIT-PLIST) (COPYLIST (CONTENTS INIT-PLIST)))
  (DELAYING-SCREEN-MANAGEMENT
    (LOCK-SHEET (SELF)
      (AROUND-METHOD-CONTINUE CONT MT ARGS))
    (AND (SHEET-BIT-ARRAY SELF)
         (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
           (SEND SELF :REFRESH :COMPLETE-REDISPLAY)))
    (IF (GET INIT-PLIST :ACTIVATE-P)
        (SEND SELF :ACTIVATE))
    (LET ((EXPOSE-P (GET INIT-PLIST :EXPOSE-P)))
      (IF EXPOSE-P
          (SEND SELF :EXPOSE (UNLESS (EQ EXPOSE-P T) EXPOSE-P))))))

(DEFUN SHEET-ARRAY-TYPE (SHEET)
  "Return the proper array type to use for bit arrays for SHEET.
This depends on the type of screen SHEET is on."
  (CASE (SCREEN-BITS-PER-PIXEL (SHEET-SCREEN SHEET))
    (1 'ART-1B)
    (2 'ART-2B)
    (4 'ART-4B)
    (8 'ART-8B)
    (16. 'ART-16B)
    (T 'ART-1B)))

(DEFUN-METHOD DECODE-CHARACTER-WIDTH-SPEC SHEET (SPEC)
  "Decode the value of the :CHARACTER-WIDTH init keyword, when creating a window.
Returns a number of pixels."
  (MIN (COND ((NUMBERP SPEC)
              (+ (* SPEC CHAR-WIDTH) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))
             ((STRINGP SPEC)
              (MULTIPLE-VALUE-BIND (NIL NIL MAX-X)
                  (SHEET-STRING-LENGTH SELF SPEC)
                (+ MAX-X LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
             (T (FERROR "~S illegal as ~S; use ~S, number, or string" :CHARACTER-WIDTH NIL)))
       (IF SUPERIOR (SHEET-INSIDE-WIDTH SUPERIOR) #o1000000)))

(DEFUN-METHOD DECODE-CHARACTER-HEIGHT-SPEC SHEET (SPEC &OPTIONAL WIDTH-ALSO &AUX WID)
  "Decode the value of the :CHARACTER-HEIGHT init keyword, when creating a window.
Returns a number of pixels."
  (AND WIDTH-ALSO (STRINGP SPEC)
       (SETQ WID (- (DECODE-CHARACTER-WIDTH-SPEC SPEC) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
  (MIN (COND ((NUMBERP SPEC)
              (+ (* SPEC LINE-HEIGHT)
                 TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
             ((STRINGP SPEC)
              (MULTIPLE-VALUE-BIND (IGNORE HT)
                  (SHEET-COMPUTE-MOTION SELF 0 0 SPEC 0 NIL T 0 #o1000000 #o1000000 WID)
                (+ HT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))
             (T (FERROR "~S illegal as ~S; use ~S, number, or string" :CHARACTER-HEIGHT NIL)))
       (IF SUPERIOR (SHEET-INSIDE-HEIGHT SUPERIOR) #o1000000)))

(DEFUN-METHOD SHEET-DEDUCE-AND-SET-SIZES SHEET (RIGHT BOTTOM VSP INTEGRAL-P
                                                &OPTIONAL CHARACTER-WIDTH CHARACTER-HEIGHT)
  "Given the values (possibly NIL) of six init keywords, set SELF's edges.
If instance variables such as X-OFFSET or WIDTH were specified,
the variables already have their values, and we use those values."
   ;; Standardize the font map
   (UNLESS (AND (VARIABLE-BOUNDP FONT-MAP) (NOT (NULL FONT-MAP)))
     (SETQ FONT-MAP (LIST (SEND (SHEET-SCREEN SELF) :DEFAULT-FONT-NAME))))
   (SHEET-NEW-FONT-MAP FONT-MAP VSP)

   ;; If height and/or width given in terms of characters in font 0, convert to pixels
   (IF (NOT (NULL CHARACTER-WIDTH))
       (SETQ WIDTH (DECODE-CHARACTER-WIDTH-SPEC CHARACTER-WIDTH)))
   (IF (NOT (NULL CHARACTER-HEIGHT))
       (SETQ HEIGHT (DECODE-CHARACTER-HEIGHT-SPEC CHARACTER-HEIGHT)))

   ;; Need to have X-OFFSET, Y-OFFSET, WIDTH, HEIGHT
   (UNLESS X-OFFSET
     (SETQ X-OFFSET (IF (AND RIGHT WIDTH) (- RIGHT WIDTH) (SHEET-INSIDE-LEFT SUPERIOR))))
   (UNLESS Y-OFFSET
     (SETQ Y-OFFSET (IF (AND BOTTOM HEIGHT) (- BOTTOM HEIGHT) (SHEET-INSIDE-TOP SUPERIOR))))
   (UNLESS WIDTH
     (SETQ WIDTH (- (OR RIGHT (SHEET-INSIDE-RIGHT SUPERIOR)) X-OFFSET)))
   (UNLESS HEIGHT
     (SETQ HEIGHT (- (OR BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR)) Y-OFFSET)))
   (WHEN INTEGRAL-P
     (SETQ BOTTOM-MARGIN-SIZE (- HEIGHT TOP-MARGIN-SIZE
                                 (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES)))))
   (SETQ CURSOR-X (SHEET-INSIDE-LEFT))
   (SETQ CURSOR-Y (SHEET-INSIDE-TOP))

   SELF)

(DEFMETHOD (SHEET :INIT) (INIT-PLIST &AUX BOTTOM RIGHT SAVE-BITS (VSP 2) (MORE-P T)
                                          (CHARACTER-WIDTH NIL) (CHARACTER-HEIGHT NIL)
                                          (REVERSE-VIDEO-P NIL) (INTEGRAL-P NIL)
                                          (BLINKER-P T) (BLINK-FL 'RECTANGULAR-BLINKER)
                                          (DESELECTED-VISIBILITY :ON)
                                          (default-cons-area sheet-area))
  ;; Process options
  (DOPLIST ((CAR INIT-PLIST) VAL OP)
    (CASE OP
      ((:LEFT :X) (SETQ X-OFFSET VAL))
      ((:TOP :Y) (SETQ Y-OFFSET VAL))
      (:POSITION (SETQ X-OFFSET (FIRST VAL) Y-OFFSET (SECOND VAL)))
      (:RIGHT (SETQ RIGHT VAL))
      (:BOTTOM (SETQ BOTTOM VAL))
      (:SIZE (AND VAL (SETQ WIDTH (FIRST VAL)
                            HEIGHT (SECOND VAL))))
      (:EDGES (AND VAL (SETQ X-OFFSET (FIRST VAL)
                             Y-OFFSET (SECOND VAL)
                             RIGHT (THIRD VAL)
                             BOTTOM (FOURTH VAL)
                             ;; Override any specified height, probably from default plist.
                             HEIGHT NIL WIDTH NIL))
              (UNLESS (> RIGHT X-OFFSET)
                (FERROR "Specified edges give width ~S" (- RIGHT X-OFFSET)))
              (UNLESS (> BOTTOM Y-OFFSET)
                (FERROR "Specified edges give height ~S" (- BOTTOM Y-OFFSET))))
      (:CHARACTER-WIDTH (SETQ CHARACTER-WIDTH VAL))
      (:CHARACTER-HEIGHT (SETQ CHARACTER-HEIGHT VAL))
      (:BLINKER-P (SETQ BLINKER-P VAL))
      (:REVERSE-VIDEO-P (SETQ REVERSE-VIDEO-P VAL))
      (:MORE-P (SETQ MORE-P VAL))
      (:VSP (SETQ VSP VAL))
      (:BLINKER-FLAVOR (SETQ BLINK-FL VAL))
      (:BLINKER-DESELECTED-VISIBILITY (SETQ DESELECTED-VISIBILITY VAL))
      (:INTEGRAL-P (SETQ INTEGRAL-P VAL))
      (:SAVE-BITS (SETQ SAVE-BITS VAL))
      (:RIGHT-MARGIN-CHARACTER-FLAG (SETF (SHEET-RIGHT-MARGIN-CHARACTER-FLAG) VAL))
      (:BACKSPACE-NOT-OVERPRINTING-FLAG (SETF (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG) VAL))
      (:CR-NOT-NEWLINE-FLAG (SETF (SHEET-CR-NOT-NEWLINE-FLAG) VAL))
      (:TRUNCATE-LINE-OUT-FLAG (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG) VAL))
      (:DEEXPOSED-TYPEIN-ACTION (SEND SELF :SET-DEEXPOSED-TYPEIN-ACTION VAL))
      (:TAB-NCHARS (SETF (SHEET-TAB-NCHARS) VAL))
      ))
  (SHEET-DEDUCE-AND-SET-SIZES RIGHT BOTTOM VSP INTEGRAL-P CHARACTER-WIDTH CHARACTER-HEIGHT)
  (COND ((OR (EQ SAVE-BITS 'T) BIT-ARRAY)
         (SETQ LOCATIONS-PER-LINE (SHEET-LOCATIONS-PER-LINE SUPERIOR))
         (LET ((LINE-WIDTH (PIXELS-PER-PHYSICAL-LINE))
               (ARRAY-TYPE (SHEET-ARRAY-TYPE (OR SUPERIOR SELF))))
           (SETQ BIT-ARRAY
                 (IF BIT-ARRAY
                     (GROW-BIT-ARRAY BIT-ARRAY LINE-WIDTH HEIGHT WIDTH)
                   (MAKE-PIXEL-ARRAY LINE-WIDTH HEIGHT :TYPE ARRAY-TYPE :AREA SHEET-AREA)))
           (SETQ SCREEN-ARRAY (MAKE-PIXEL-ARRAY LINE-WIDTH HEIGHT :TYPE ARRAY-TYPE
                                                :AREA SHEET-AREA
                                                :DISPLACED-TO BIT-ARRAY
                                                :DISPLACED-INDEX-OFFSET 0))))
        ((EQ SAVE-BITS :DELAYED)
         (SETF (SHEET-FORCE-SAVE-BITS) 1)))
  (SETQ MORE-VPOS (AND MORE-P (SHEET-DEDUCE-MORE-VPOS SELF)))
  (WHEN SUPERIOR
    (UNLESS BIT-ARRAY
      (LET ((ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
        (SETQ OLD-SCREEN-ARRAY
              (MAKE-PIXEL-ARRAY
                (PIXEL-ARRAY-WIDTH ARRAY) HEIGHT
                :TYPE (ARRAY-TYPE ARRAY)
                :AREA SHEET-AREA
                :DISPLACED-TO ARRAY
                :DISPLACED-INDEX-OFFSET
                (+ X-OFFSET (* Y-OFFSET (PIXEL-ARRAY-WIDTH ARRAY)))))
        (SETQ LOCATIONS-PER-LINE (SHEET-LOCATIONS-PER-LINE SUPERIOR))))
    (WHEN BLINKER-P
      (APPLY #'MAKE-BLINKER SELF BLINK-FL
             :FOLLOW-P T
             :DESELECTED-VISIBILITY DESELECTED-VISIBILITY
             (AND (CONSP BLINKER-P) BLINKER-P))))
  (SETF (SHEET-OUTPUT-HOLD-FLAG) 1)
  (OR (VARIABLE-BOUNDP CHAR-ALUF)
      (SETQ CHAR-ALUF (IF REVERSE-VIDEO-P ALU-ANDCA ALU-IOR)))
  (OR (VARIABLE-BOUNDP ERASE-ALUF)
      (SETQ ERASE-ALUF (IF REVERSE-VIDEO-P ALU-IOR ALU-ANDCA)))
  (SEND SELF :UPDATE-TIME-STAMP)
  SELF)

(DEFMETHOD (SCREEN :BEFORE :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP LOCATIONS-PER-LINE)
    (SETQ LOCATIONS-PER-LINE (TRUNCATE (* WIDTH BITS-PER-PIXEL) 32.)))
  (SETQ FONT-ALIST NIL)                         ;Overridden by the :AFTER method.
                                                ;Makes :PARSE-FONT-DESCRIPTOR not lose.
  (SETQ DEFAULT-FONT FONTS:CPTFONT  ;These two must get values here,
        FONT-MAP (FILLARRAY NIL (LIST DEFAULT-FONT));but they will be replaced with the right ones later.
        ;; No one uses this anyway...
        BUFFER-HALFWORD-ARRAY (MAKE-ARRAY (* 2 (OR HEIGHT 1) LOCATIONS-PER-LINE)
                                          :TYPE ART-16B :DISPLACED-TO BUFFER))
  (UNLESS BIT-ARRAY
    (SETQ OLD-SCREEN-ARRAY (MAKE-PIXEL-ARRAY (PIXELS-PER-PHYSICAL-LINE) (OR HEIGHT 1)
                                             :AREA SHEET-AREA
                                             :TYPE (SHEET-ARRAY-TYPE SELF)
                                             :DISPLACED-TO BUFFER))))

(DEFMETHOD (SCREEN :AFTER :INIT) (IGNORE)
  (SETQ FONT-ALIST (COPY-ALIST DEFAULT-FONT-ALIST))
  (DOLIST (ELT FONT-ALIST)
    (LET ((SYMBOL (INTERN (FORMAT NIL "~A-~O-~A-FONT"
                                  (TYPE-OF SELF) (%POINTER SELF) (CAR ELT))
                          "TV")))
      (SET SYMBOL (CDR ELT))
      (SETF (CDR ELT) SYMBOL)))
  (SETQ DEFAULT-FONT (SEND SELF :PARSE-FONT-SPECIFIER :DEFAULT))
  (SEND SELF :SET-FONT-MAP (LIST DEFAULT-FONT)))

(DEFMETHOD (SCREEN :BEFORE :EXPOSE) (&REST IGNORE)
  (UNLESS EXPOSED-P
    (SETQ BUFFER-HALFWORD-ARRAY (MAKE-ARRAY (* 2 (OR HEIGHT 1) LOCATIONS-PER-LINE)
                                            :TYPE ART-16B :DISPLACED-TO BUFFER))
    (SI:CHANGE-INDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-TYPE OLD-SCREEN-ARRAY)
                              (IF ARRAY-INDEX-ORDER
                                  (LIST HEIGHT (PIXELS-PER-PHYSICAL-LINE))
                                (LIST (PIXELS-PER-PHYSICAL-LINE) HEIGHT))
                              (+ BUFFER (* Y-OFFSET LOCATIONS-PER-LINE))
                              NIL)))

(DEFMETHOD (SCREEN :SELECTABLE-WINDOWS) ()
  (MAPCAN (LAMBDA (I) (SEND I :SELECTABLE-WINDOWS)) INFERIORS))

;;>> Bogous
(DEFMETHOD (SHEET :IDLE-LISP-LISTENER) ()
  (IF SUPERIOR
      (SEND SUPERIOR :IDLE-LISP-LISTENER)
    (IDLE-LISP-LISTENER SELF)))

(DEFMETHOD (SHEET :ALIAS-FOR-SELECTED-WINDOWS) ()
  SELF)

(DEFMETHOD (SHEET :ALIAS-FOR-INFERIORS) ()
  NIL)

(DEFMETHOD (SCREEN :PARSE-FONT-SPECIFIER) (FD)
  (SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:CPT-FONT))

(DEFMETHOD (SCREEN :PARSE-FONT-DESCRIPTOR) (FD)
  (SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:CPT-FONT))

(DEFUN-METHOD SCREEN-PARSE-FONT-DESCRIPTOR SCREEN (FD TYPE &OPTIONAL DONT-LOAD-P)
  (AND (TYPEP FD 'FONT) (BOUNDP (FONT-NAME FD))
       (SETQ FD (FONT-NAME FD)))
  (COND ((TYPEP FD 'FONT) FD)
        ((SYMBOLP FD)
         ;; Name of font -- find appropriate font
         (BLOCK FONT
           (LET ((FONT (GET FD TYPE)))
             ;; First try a property of the symbol.
             (COND ((TYPEP FONT 'FONT) (RETURN-FROM FONT FONT))
                   ((NULL FONT))
                   ((SYMBOLP FONT)
                    (RETURN-FROM FONT (SCREEN-PARSE-FONT-DESCRIPTOR FONT TYPE DONT-LOAD-P))))
             ;; Then see if it is a known "purpose" of a standard font.
             (IF (ASSQ FD FONT-ALIST)
                 (RETURN-FROM FONT
                   (VALUES
                     (FONT-EVALUATE (CDR (ASSQ FD FONT-ALIST)))
                     (CDR (ASSQ FD FONT-ALIST)))))
             ;; Then try its value.
             (IF (BOUNDP FD) (SETQ FONT (SYMBOL-VALUE FD)))
             (COND ((TYPEP FONT 'FONT) (RETURN-FROM FONT (VALUES FONT FD)))
                   ((NULL FONT))
                   ((EQ FONT FD))
                   ((SYMBOLP FONT)
                    (RETURN-FROM FONT (VALUES (SCREEN-PARSE-FONT-DESCRIPTOR FONT TYPE DONT-LOAD-P) FD))))
             ;; Then try re-interning in FONTS if not already there.
             (SETQ FONT (INTERN-SOFT FD "FONTS"))
             (AND FONT (NEQ FONT FD)
                  (RETURN-FROM FONT (SCREEN-PARSE-FONT-DESCRIPTOR FONT TYPE DONT-LOAD-P)))
             ;; Then maybe try loading a file.
             (UNLESS DONT-LOAD-P
               (CONDITION-CASE () (LOAD (FORMAT NIL "SYS: FONTS; ~A" FD)
                                        :VERBOSE NIL
                                        :IF-DOES-NOT-EXIST NIL)
                 (ERROR)
                 (:NO-ERROR
                  ;; See if we got a value for the symbol from the file.
                  (IF (BOUNDP FD) (SETQ FONT (SYMBOL-VALUE FD)))
                  (COND ((TYPEP FONT 'FONT) (RETURN-FROM FONT (VALUES FONT FD)))
                        ((NULL FONT))
                        ((EQ FONT FD))
                        ((SYMBOLP FONT)
                         (RETURN-FROM FONT (SCREEN-PARSE-FONT-DESCRIPTOR FONT TYPE DONT-LOAD-P)))))))
             ;; Ask the user to specify some other font descriptor.
             (RETURN-FROM FONT
               (SCREEN-PARSE-FONT-DESCRIPTOR
                 (CERROR T NIL NIL "Font ~D not found" FD)
                 TYPE)))))
        (T (FERROR "Illegal font descriptor ~A" FD))))

(DEFMETHOD (SCREEN :PARSE-FONT-NAME) (FD)
  (MULTIPLE-VALUE-BIND (FONT -NAME-)
      (SEND SELF :PARSE-FONT-SPECIFIER FD)
    (OR -NAME-
        (IF (AND (SYMBOLP FD)
             (EQ (FONT-EVALUATE FD) FONT))
            FD (FONT-NAME FONT)))))

(DEFUN FONT-EVALUATE (FONTNAME)
  "Evaluate FONTNAME repeatedly till the result is not a symbol or is an unbound symbol."
  (DO ((F FONTNAME (SYMBOL-VALUE F)))
      ((OR (NOT (SYMBOLP F)) (NOT (BOUNDP F)))
       F)))

(DEFMETHOD (SCREEN :DEFAULT-FONT-NAME) ()
  (SEND SELF :FONT-NAME-FOR :DEFAULT))

(DEFMETHOD (SCREEN :FONT-NAME-FOR) (PURPOSE &OPTIONAL OVERALL-DEFAULT)
  (OR (CDR (ASSQ PURPOSE FONT-ALIST))
      OVERALL-DEFAULT
      (CDR (ASSQ PURPOSE DEFAULT-FONT-ALIST))))

(DEFSTRUCT (FONT-MAP :ARRAY-LEADER :CONC-NAME (:ALTERANT NIL) (:MAKE-ARRAY (:LENGTH 26.)))
  (FILL-POINTER 26.)
  (FONT-LIST NIL
    :DOCUMENTATION "List of fonts or font names from this font-map was constructed.")
  (CURRENT-FONT 0
    :DOCUMENTATION "What was supplied to the last cann to :SET-CURRENT-FONT."))

(DEFUN-METHOD SHEET-NEW-FONT-MAP SHEET (NEW-MAP VSP &AUX (SCREEN (SHEET-SCREEN SELF)))
  "Set SELF's FONT-MAP and VSP according to NEW-MAP and VSP.
NEW-MAP is either a legitimate font-map array,
or a list or ordinary array of fonts or font descriptors.
VSP is a number of pixels."
  (UNLESS ;; really want (typep new-map 'font-map)
          (AND (TYPEP NEW-MAP '(VECTOR T))
               ( (ARRAY-LENGTH NEW-MAP) 26.)
               (EQ (ARRAY-LEADER-LENGTH NEW-MAP) 3))
    (IF (VECTORP NEW-MAP) (SETQ NEW-MAP (LISTARRAY NEW-MAP)))
    (LET* ((LENGTH (MAX (LENGTH NEW-MAP) 26.))
           (FM (MAKE-FONT-MAP :FONT-LIST NEW-MAP
                              :FILL-POINTER LENGTH
                              :MAKE-ARRAY (:LENGTH LENGTH))))
      (FILLARRAY FM NEW-MAP)
      (SETQ NEW-MAP FM)))
  ;; Now that NEW-MAP contains fonts descriptors, extract the real fonts
  (DOTIMES (I (LENGTH NEW-MAP))
    (ASET (SEND SCREEN :PARSE-FONT-SPECIFIER (AREF NEW-MAP I)) NEW-MAP I))
  (WITHOUT-INTERRUPTS
    (SETQ FONT-MAP NEW-MAP)
    ;;Now, find out the character dimensions of this set of fonts
    (LET ((FONT (AREF NEW-MAP 0)))
      (SETQ CURRENT-FONT FONT)
      (SETF (FONT-MAP-CURRENT-FONT FONT-MAP) 0)
      (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH FONT))
      (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
        (IF BL (SEND BL :SET-SIZE (FONT-CHAR-WIDTH FONT) (FONT-CHAR-HEIGHT FONT)))))
    (DO ((I 0 (1+ I))
         (LENGTH (LENGTH NEW-MAP))
;        (MAXWIDTH 0)
         (MAXHEIGHT 0)
         (MAXBASE 0)
         (FONT))
        (( I LENGTH)
         (SETQ BASELINE MAXBASE
               LINE-HEIGHT (+ VSP MAXHEIGHT)))
      (SETQ FONT (AREF NEW-MAP I))
      (DO () ((NOT (SYMBOLP FONT)))
        (SETQ FONT (SYMBOL-VALUE FONT)))
      (SETQ MAXHEIGHT (MAX MAXHEIGHT (FONT-CHAR-HEIGHT FONT))
            MAXBASE (MAX MAXBASE (FONT-BASELINE FONT)))
;     (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
;       (IF CWT
;          (DOTIMES (J #o200)
;              (SETQ MAXWIDTH (MAX MAXWIDTH (AREF TEM J))))
;          (SETQ MAXWIDTH (MAX MAXWIDTH (FONT-CHAR-WIDTH (AREF NEW-MAP I))))))
      )
    (SETQ BASELINE-ADJ (- BASELINE (FONT-BASELINE CURRENT-FONT)))
    ))

(DEFMETHOD (SCREEN :BEFORE :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (WHEN (EQ DEFAULT-FONT OLD-FONT)
    (SETQ DEFAULT-FONT NEW-FONT)
    (SET (SEND SELF :DEFAULT-FONT-NAME)
         (FONT-NAME NEW-FONT))))

(DEFMETHOD (SCREEN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (DOLIST (RESOURCE-NAME WINDOW-RESOURCE-NAMES)
    (MAP-RESOURCE (LAMBDA (WINDOW IGNORE IGNORE)
                    (declare (sys:downward-function))
                    (AND (EQ (SEND WINDOW :STATUS) ':DEACTIVATED)
                         (EQ SELF (SHEET-SCREEN WINDOW))
                         (SEND WINDOW :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))
                  RESOURCE-NAME)))

(DEFMETHOD (SHEET :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT &AUX CURRENT)
  (OR OLD-FONT (SETQ OLD-FONT CURRENT-FONT))
  (DOTIMES (I (ARRAY-LENGTH FONT-MAP))
    (IF (EQ (AREF FONT-MAP I) CURRENT-FONT) (SETQ CURRENT I)))
  (SEND SELF :SET-FONT-MAP (FONT-MAP-FONT-LIST FONT-MAP))
  (IF CURRENT (SEND SELF :SET-CURRENT-FONT CURRENT))
  (SEND SELF :UPDATE-TIME-STAMP)
  (DOLIST (I INFERIORS)
    (SEND I :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))

(DEFMETHOD (SHEET :MORE-P) ()
  (NOT (NULL MORE-VPOS)))

(DEFMETHOD (SHEET :SET-MORE-P) (MORE-P)
  (SETQ MORE-VPOS (AND MORE-P (SHEET-DEDUCE-MORE-VPOS SELF))))

(defmethod (sheet :set-time-out-on-more) (on-p)
  (setf (sheet-time-out-on-more self) (if on-p 1 0)))

(DEFUN SHEET-DEDUCE-MORE-VPOS (SHEET &AUX (LH (SHEET-LINE-HEIGHT SHEET)))
  "Return the vpos of the line just above the bottom margin."
  (+ (SHEET-TOP-MARGIN-SIZE SHEET)
     (1- (* (1- (FLOOR (SHEET-INSIDE-HEIGHT SHEET) LH)) LH))))

(DEFMETHOD (SHEET :VSP) ()
  (- LINE-HEIGHT
     (LET ((H 0))
       (DOTIMES (I (ARRAY-LENGTH FONT-MAP))
         (SETQ H (MAX H (FONT-CHAR-HEIGHT (AREF FONT-MAP I)))))
       H)))

(DEFMETHOD (SHEET :SET-VSP) (NEW-VSP)
  (SHEET-NEW-FONT-MAP FONT-MAP NEW-VSP)
  NEW-VSP)

(DEFMETHOD (SHEET :SET-FONT-MAP) (NEW-MAP)
  (OR NEW-MAP (SETQ NEW-MAP (LIST (SEND (SHEET-SCREEN SELF) :DEFAULT-FONT-NAME))))
  (SHEET-NEW-FONT-MAP NEW-MAP (SEND SELF :VSP))
  FONT-MAP)

(DEFMETHOD (SHEET :SET-CURRENT-FONT) (NEW-FONT &OPTIONAL OK-IF-NOT-IN-FONT-MAP &aux font tem)
  (WITHOUT-INTERRUPTS
    (SETF (FONT-MAP-CURRENT-FONT FONT-MAP)
          (COND ((NUMBERP NEW-FONT) (SETQ FONT (AREF FONT-MAP NEW-FONT)) NEW-FONT)
                (T (SETQ FONT (SEND (SHEET-SCREEN SELF) :PARSE-FONT-SPECIFIER NEW-FONT)
                         TEM (POSITION FONT FONT-MAP))
                   (UNLESS (OR TEM OK-IF-NOT-IN-FONT-MAP)
                     (FERROR "~A is not in the font map of ~S." FONT SELF))
                   (IF (SYMBOLP NEW-FONT) NEW-FONT (OR TEM FONT)))))
    (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
      (IF BL (SEND BL :SET-SIZE (FONT-CHAR-WIDTH FONT) (FONT-CHAR-HEIGHT FONT))))
    (SETQ CURRENT-FONT FONT
          BASELINE-ADJ (- BASELINE (FONT-BASELINE FONT))
          CHAR-WIDTH (FONT-CHAR-WIDTH FONT))))

(DEFMETHOD (SHEET :REVERSE-VIDEO-P) ()
  (EQ CHAR-ALUF ALU-ANDCA))

(DEFMETHOD (SHEET :SET-REVERSE-VIDEO-P) (REVERSE-VIDEO-P)
  (AND ( CHAR-ALUF (IF REVERSE-VIDEO-P ALU-ANDCA ALU-IOR))
       (SHEET-FORCE-ACCESS (SELF)
         (TV:PREPARE-SHEET (SELF)
           (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ALU-XOR SELF))))
  (IF REVERSE-VIDEO-P
      (SETQ CHAR-ALUF ALU-ANDCA ERASE-ALUF ALU-IOR)
      (SETQ CHAR-ALUF ALU-IOR ERASE-ALUF ALU-ANDCA)))

(DEFMETHOD (SHEET :DEEXPOSED-TYPEIN-ACTION) ()
  (IF (ZEROP (SHEET-DEEXPOSED-TYPEIN-NOTIFY)) :NORMAL :NOTIFY))

(DEFMETHOD (SHEET :SET-DEEXPOSED-TYPEIN-ACTION) (VALUE)
  (SETF (SHEET-DEEXPOSED-TYPEIN-NOTIFY)
        (CASE VALUE
          (:NORMAL 0)
          (:NOTIFY 1)
          (OTHERWISE
           (FERROR "~S illegal deexposed-typein-action; use ~S or ~S" :NORMAL :NOTIFY)))))

(DEFMETHOD (SHEET :SAVE-BITS) ()
  (IF BIT-ARRAY T
    (IF (ZEROP (SHEET-FORCE-SAVE-BITS)) NIL
      :DELAYED)))

(DEFMETHOD (SHEET :SET-SAVE-BITS) (SAVE-BITS &AUX (INHIBIT-SCHEDULING-FLAG T))
  (OR SUPERIOR (FERROR "Cannot ~S on a top-level sheet" :SET-SAVE-BITS))
  (LOCK-SHEET (SELF)
    (COND ((EQ SAVE-BITS 'T)
           (LET ((INHIBIT-SCHEDULING-FLAG T))
             (OR BIT-ARRAY
                 (SETQ BIT-ARRAY (MAKE-PIXEL-ARRAY (PIXELS-PER-PHYSICAL-LINE)
                                                   HEIGHT
                                                   :TYPE (SHEET-ARRAY-TYPE SELF)
                                                   :AREA SHEET-AREA)))
             (COND ((NULL SCREEN-ARRAY)
                    (REDIRECT-ARRAY (SETQ SCREEN-ARRAY OLD-SCREEN-ARRAY)
                                    (ARRAY-TYPE BIT-ARRAY)
                                    (PIXEL-ARRAY-WIDTH BIT-ARRAY)
                                    (PIXEL-ARRAY-HEIGHT BIT-ARRAY)
                                    BIT-ARRAY 0)
                    (SETQ OLD-SCREEN-ARRAY NIL))))
           (COND ((NOT EXPOSED-P)
                  ;; We are not exposed, first refresh ourself
                  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE) (SEND SELF :REFRESH))
                  ;; Expose in reverse order for the sake of temporary windows
                  (DOLIST (I (REVERSE EXPOSED-INFERIORS))
                    ;; Then actually expose all of our virtually exposed inferiors.
                    ;; Note that we already own the lock on all of them, and the mouse
                    ;; can't be in them since we are deexposed.
                    (SEND I :EXPOSE)))))
          ((NULL BIT-ARRAY))
          (T
           (SETQ BIT-ARRAY NIL)
           ;; Note that SCREEN-ARRAY still points to the old value of BIT-ARRAY.  This is
           ;; important for the following deexposes to work.
           (COND ((NOT EXPOSED-P)
                  ;; The mouse can't possibly be in any of these windows, so it's alright
                  ;; to just go ahead and deexpose them with us locked
                  (DOLIST (I EXPOSED-INFERIORS)
                    (SEND I :DEEXPOSE :DEFAULT :NOOP NIL))
                  (WITHOUT-INTERRUPTS
                    (SETQ OLD-SCREEN-ARRAY SCREEN-ARRAY)
                    (LET ((ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
                      (REDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-TYPE OLD-SCREEN-ARRAY)
                                      (PIXEL-ARRAY-WIDTH ARRAY)
                                      (PIXEL-ARRAY-HEIGHT OLD-SCREEN-ARRAY)
                                      ARRAY
                                      (+ X-OFFSET (* Y-OFFSET
                                                     (PIXEL-ARRAY-WIDTH ARRAY)))))
                    (SETQ SCREEN-ARRAY NIL))))))
    (SETF (SHEET-FORCE-SAVE-BITS) (IF (EQ SAVE-BITS :DELAYED) 1 0)))
  SAVE-BITS)

(DEFMETHOD (SHEET :AFTER :SET-SAVE-BITS) (IGNORE)
  (SCREEN-MANAGE-WINDOW-AREA SELF))

(DEFUN-METHOD ERASE-MARGINS SHEET ()
  (WHEN SCREEN-ARRAY
    (PREPARE-SHEET (SELF)
      (%DRAW-RECTANGLE LEFT-MARGIN-SIZE HEIGHT
                       0 0 ERASE-ALUF SELF)
      (%DRAW-RECTANGLE RIGHT-MARGIN-SIZE HEIGHT
                       (SHEET-INSIDE-RIGHT) 0 ERASE-ALUF SELF)
      (%DRAW-RECTANGLE WIDTH TOP-MARGIN-SIZE
                       0 0 ERASE-ALUF SELF)
      (%DRAW-RECTANGLE WIDTH BOTTOM-MARGIN-SIZE
                       0 (SHEET-INSIDE-BOTTOM) ERASE-ALUF SELF))))

(DEFMETHOD (SHEET :CHANGE-OF-SIZE-OR-MARGINS) (&REST OPTIONS
                                               &AUX TOP BOTTOM LEFT RIGHT
                                                    NEW-HEIGHT NEW-WIDTH OLD-X OLD-Y
                                                    (OLD-TOP-MARGIN-SIZE TOP-MARGIN-SIZE)
                                                    (OLD-LEFT-MARGIN-SIZE LEFT-MARGIN-SIZE)
                                                    DELTA-TOP-MARGIN DELTA-LEFT-MARGIN
                                                    (INTEGRAL-P NIL)
                                                    (OLD-INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
                                                    (OLD-INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
                                                    (OLD-WIDTH WIDTH)
                                                    (OLD-HEIGHT HEIGHT))
  "Change some sheet parameters"
  (OR SUPERIOR (NOT EXPOSED-P)
      (FERROR "Cannot change size or margins of an exposed window with no superior"))
  (SHEET-FORCE-ACCESS (SELF)
    (ERASE-MARGINS))
  (SETQ OLD-X (- CURSOR-X LEFT-MARGIN-SIZE)
        OLD-Y (- CURSOR-Y TOP-MARGIN-SIZE))
  ;; Process options
  (DOPLIST (OPTIONS VAL OP)
    (CASE OP
      ((:TOP :Y) (SETQ TOP VAL))
      (:BOTTOM (SETQ BOTTOM VAL))
      ((:LEFT :X) (SETQ LEFT VAL))
      (:RIGHT (SETQ RIGHT VAL))
      (:WIDTH (SETQ NEW-WIDTH VAL))
      (:HEIGHT (SETQ NEW-HEIGHT VAL))
      (:TOP-MARGIN-SIZE (SETQ TOP-MARGIN-SIZE VAL))
      (:BOTTOM-MARGIN-SIZE (SETQ BOTTOM-MARGIN-SIZE VAL))
      (:LEFT-MARGIN-SIZE (SETQ LEFT-MARGIN-SIZE VAL))
      (:RIGHT-MARGIN-SIZE (SETQ RIGHT-MARGIN-SIZE VAL))
      (:INTEGRAL-P (SETQ INTEGRAL-P VAL))
      (OTHERWISE (FERROR "~S is not a recognized option" OP))))
  (SETQ X-OFFSET (OR LEFT (IF RIGHT (- RIGHT (OR NEW-WIDTH WIDTH)) X-OFFSET)))
  (SETQ Y-OFFSET (OR TOP (IF BOTTOM (- BOTTOM (OR NEW-HEIGHT HEIGHT)) Y-OFFSET)))
  (SETQ NEW-WIDTH (OR NEW-WIDTH (IF RIGHT (- RIGHT LEFT) WIDTH)))
  (SETQ NEW-HEIGHT (OR NEW-HEIGHT (IF BOTTOM (- BOTTOM TOP) HEIGHT)))
  (SETQ WIDTH NEW-WIDTH HEIGHT NEW-HEIGHT)

  ;; We need to deexpose all of our inferiors that won't fit anymore
  (DOLIST (I EXPOSED-INFERIORS)
    (OR (SHEET-WITHIN-P I (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)
                        (SHEET-INSIDE-RIGHT) (SHEET-INSIDE-BOTTOM))
        (SEND I :DEEXPOSE)))

  (WITHOUT-INTERRUPTS
    (SHEET-FORCE-ACCESS (SELF T)
      (MAPC 'OPEN-BLINKER BLINKER-LIST))
    (SHEET-DEDUCE-AND-SET-SIZES RIGHT BOTTOM (SEND SELF :VSP) INTEGRAL-P)
    (SETQ CURSOR-X
          (MIN (+ LEFT-MARGIN-SIZE OLD-X) (- WIDTH RIGHT-MARGIN-SIZE CHAR-WIDTH)))
    (SETQ CURSOR-Y
          (MIN (+ TOP-MARGIN-SIZE OLD-Y) (- HEIGHT BOTTOM-MARGIN-SIZE LINE-HEIGHT)))
    (DOLIST (BL BLINKER-LIST)
      (COND ((NULL (BLINKER-X-POS BL)))
            (( (BLINKER-X-POS BL) (SHEET-INSIDE-RIGHT))
             (SETF (BLINKER-X-POS BL) (SHEET-INSIDE-LEFT))))
      (COND ((NULL (BLINKER-Y-POS BL)))
            (( (BLINKER-Y-POS BL) (SHEET-INSIDE-BOTTOM))
             (SETF (BLINKER-Y-POS BL) (SHEET-INSIDE-TOP)))))
    (WHEN BIT-ARRAY
      (SETQ BIT-ARRAY (GROW-BIT-ARRAY BIT-ARRAY
                                      (PIXELS-PER-PHYSICAL-LINE)
                                      HEIGHT WIDTH
                                      OLD-HEIGHT OLD-WIDTH)))
    (WHEN SUPERIOR
      ;;If we have a bit-array, SCREEN-ARRAY indirects to it, else OLD-SCREEN-ARRAY
      ;; indirects into our superior.
      (LET ((ARRAY (OR SCREEN-ARRAY OLD-SCREEN-ARRAY))
            (INDIRECT-TO (OR (AND (NOT EXPOSED-P) BIT-ARRAY)
                             (SHEET-SUPERIOR-SCREEN-ARRAY))))
        (REDIRECT-ARRAY
          ARRAY (ARRAY-TYPE INDIRECT-TO)
          (PIXEL-ARRAY-WIDTH INDIRECT-TO) HEIGHT
          INDIRECT-TO
          (IF (AND BIT-ARRAY (NOT EXPOSED-P)) 0
            (+ X-OFFSET (* Y-OFFSET (PIXEL-ARRAY-WIDTH INDIRECT-TO)))))
        (IF (OR BIT-ARRAY EXPOSED-P)
            (SETQ SCREEN-ARRAY ARRAY
                  OLD-SCREEN-ARRAY NIL)
          (SETQ OLD-SCREEN-ARRAY ARRAY
                SCREEN-ARRAY NIL))
        ;; If the size of the top and/or left margin changed, move the inside bits around
        (SETQ DELTA-TOP-MARGIN (- TOP-MARGIN-SIZE OLD-TOP-MARGIN-SIZE)
              DELTA-LEFT-MARGIN (- LEFT-MARGIN-SIZE OLD-LEFT-MARGIN-SIZE))
        (COND ((AND (ZEROP DELTA-TOP-MARGIN) (ZEROP DELTA-LEFT-MARGIN)))
              ((NULL SCREEN-ARRAY))             ;Don't BITBLT some other guy's bits!!
              (T ;; This should be BITBLT-WITH-FAST-PAGING, sometimes it is not paged in
               (OR EXPOSED-P (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
               (BITBLT ALU-SETA (IF (PLUSP DELTA-LEFT-MARGIN) (- (SHEET-INSIDE-WIDTH))
                                  (SHEET-INSIDE-WIDTH))
                       (IF (PLUSP DELTA-TOP-MARGIN) (- (SHEET-INSIDE-HEIGHT))
                         (SHEET-INSIDE-HEIGHT))
                       ARRAY OLD-LEFT-MARGIN-SIZE OLD-TOP-MARGIN-SIZE
                       ARRAY LEFT-MARGIN-SIZE TOP-MARGIN-SIZE)
               ;; If margins got smaller, may be space to clear out on bottom and right
               (AND (MINUSP DELTA-LEFT-MARGIN)
                    (BITBLT ERASE-ALUF (- DELTA-LEFT-MARGIN) (SHEET-INSIDE-HEIGHT)
                            ARRAY (+ (SHEET-INSIDE-RIGHT) DELTA-LEFT-MARGIN)
                            (SHEET-INSIDE-TOP)
                            ARRAY (+ (SHEET-INSIDE-RIGHT) DELTA-LEFT-MARGIN)
                            (SHEET-INSIDE-TOP)))
               (AND (MINUSP DELTA-TOP-MARGIN)
                    (BITBLT ERASE-ALUF (SHEET-INSIDE-WIDTH) (- DELTA-TOP-MARGIN)
                            ARRAY (SHEET-INSIDE-LEFT)
                            (+ (SHEET-INSIDE-BOTTOM) DELTA-TOP-MARGIN)
                            ARRAY (SHEET-INSIDE-LEFT)
                            (+ (SHEET-INSIDE-BOTTOM) DELTA-TOP-MARGIN))))))
      (AND TEMPORARY-BIT-ARRAY (NEQ TEMPORARY-BIT-ARRAY T)
           (SETQ TEMPORARY-BIT-ARRAY (GROW-BIT-ARRAY TEMPORARY-BIT-ARRAY
                                                     (PIXELS-PER-PHYSICAL-LINE)
                                                     HEIGHT WIDTH NIL NIL NIL)))
      (SHEET-FORCE-ACCESS (SELF)
        (ERASE-MARGINS)))
    (SEND SELF :UPDATE-TIME-STAMP)
    (OR ( OLD-INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
        ( OLD-INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT)))))


(DEFUN MAKE-SHEET-BIT-ARRAY (SHEET X Y &REST MAKE-ARRAY-OPTIONS)
  "Create a bit-array for SHEET, of size X by Y."
  (LET* ((TYPE (ARRAY-TYPE (WITHOUT-INTERRUPTS
                             (OR (SHEET-SCREEN-ARRAY (SHEET-SCREEN SHEET))
                                 (SHEET-OLD-SCREEN-ARRAY (SHEET-SCREEN SHEET))))))
         (ROUND-TO (TRUNCATE 32. (OR (CDR (ASSQ TYPE ARRAY-BITS-PER-ELEMENT)) 32.))))
    (APPLY #'MAKE-PIXEL-ARRAY
           (* (CEILING X ROUND-TO) ROUND-TO)
           Y
           :TYPE TYPE
           :AREA SHEET-AREA
           MAKE-ARRAY-OPTIONS)))

(DEFUN GROW-BIT-ARRAY (ARRAY WIDTH HEIGHT &OPTIONAL (REAL-WIDTH WIDTH)
                       OLD-HEIGHT OLD-REAL-WIDTH
                       (CONTENTS-MATTER T)
                       &AUX (AWIDTH (PIXEL-ARRAY-WIDTH ARRAY))
                       (AHEIGHT (PIXEL-ARRAY-HEIGHT ARRAY)))
  "Adjust size of ARRAY for a window of size WIDTH, HEIGHT.
REAL-WIDTH is the actual width to make the array.
OLD-HEIGHT and OLD-REAL-WIDTH are the previous size of the array,
and CONTENTS-MATTER says preserve the old contents if possible."
  (LET ((WWIDTH (LOGAND #o-40 (+ WIDTH #o37)))  ;Width as even number of words
        (REAL-ARRAY ARRAY))
    (COND ((NOT (IF CONTENTS-MATTER
                    (AND (EQ HEIGHT OLD-HEIGHT) (EQ REAL-WIDTH OLD-REAL-WIDTH))
                  (AND ( WWIDTH AWIDTH) ( HEIGHT AHEIGHT))))
           (IF (OR (> WWIDTH AWIDTH) (> HEIGHT AHEIGHT))
               ;; Need bigger array, make it and copy in the old one
               (LET ((NARRAY (MAKE-PIXEL-ARRAY WWIDTH HEIGHT :TYPE (ARRAY-TYPE ARRAY) :AREA SHEET-AREA)))
                 (COND (CONTENTS-MATTER
                        (SI:PAGE-IN-ARRAY ARRAY)
                        (BITBLT ALU-SETA (MIN REAL-WIDTH AWIDTH) (MIN HEIGHT AHEIGHT)
                                ARRAY 0 0 NARRAY 0 0)))
                 (STRUCTURE-FORWARD ARRAY NARRAY)
                 (SI:PAGE-OUT-ARRAY ARRAY)
                 (SETQ REAL-ARRAY NARRAY))
             ;; Array is big enough, but used size is changing and we want to
             ;; initialize the contents that are about to be used.
             (COND ((AND OLD-REAL-WIDTH (> REAL-WIDTH OLD-REAL-WIDTH))
                    (PAGE-IN-PIXEL-ARRAY ARRAY NIL (LIST REAL-WIDTH HEIGHT))
                    (BITBLT ALU-SETZ (- REAL-WIDTH OLD-REAL-WIDTH) HEIGHT
                            ARRAY OLD-REAL-WIDTH 0 ARRAY OLD-REAL-WIDTH 0)))
             (COND ((AND OLD-HEIGHT (> HEIGHT OLD-HEIGHT))
                    (PAGE-IN-PIXEL-ARRAY ARRAY (LIST REAL-WIDTH HEIGHT))
                    (BITBLT ALU-SETZ REAL-WIDTH (- HEIGHT OLD-HEIGHT)
                            ARRAY 0 OLD-HEIGHT ARRAY 0 OLD-HEIGHT))))))
    REAL-ARRAY))

;;; Called to set the position of SELF, which must be deexposed and locked.
;;; Can be called on sheets on deexposed screens.  NEW-X and NEW-Y are rel to SELF's superior.
;;; This is not used.
(DEFUN-METHOD SHEET-SET-DEEXPOSED-POSITION SHEET (NEW-X NEW-Y)
  (WHEN EXPOSED-P
    (FERROR "Wrong function called to set position of exposed sheet ~A" SELF))
  (SETQ X-OFFSET NEW-X
        Y-OFFSET NEW-Y)
  (OR BIT-ARRAY (NULL SUPERIOR)
      (LET ((SUP-ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
        (REDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-TYPE OLD-SCREEN-ARRAY)
                        (PIXEL-ARRAY-WIDTH SUP-ARRAY)
                        (PIXEL-ARRAY-HEIGHT OLD-SCREEN-ARRAY)
                        SUP-ARRAY
                        (+ NEW-X (* NEW-Y (PIXEL-ARRAY-WIDTH SUP-ARRAY))))))
  (SEND SELF :UPDATE-TIME-STAMP))

;;; Called to set the position of SELF, which must be exposed and locked.
;;; NEW-X and NEW-Y are rel to SELF's superior.
;;; A subroutine of :SET-EDGES.
(DEFUN-METHOD SHEET-SET-EXPOSED-POSITION SHEET (NEW-X NEW-Y &AUX OX OY)
  (PREPARE-SHEET (SELF)
    (SETQ OX X-OFFSET
          OY Y-OFFSET
          X-OFFSET NEW-X
          Y-OFFSET NEW-Y)
    (LET ((SUP-ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
      (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-TYPE SCREEN-ARRAY)
                      (PIXEL-ARRAY-WIDTH SUP-ARRAY)
                      (PIXEL-ARRAY-HEIGHT SCREEN-ARRAY)
                      SUP-ARRAY
                      (+ NEW-X (* NEW-Y (PIXEL-ARRAY-WIDTH SUP-ARRAY))))
      (BITBLT ALU-SETA
              (IF (> OX NEW-X) WIDTH (- WIDTH))
              (IF (> OY NEW-Y) HEIGHT (- HEIGHT))
              SUP-ARRAY OX OY
              SUP-ARRAY NEW-X NEW-Y))
    (SETQ MOUSE-RECONSIDER T))
  (SEND SELF :UPDATE-TIME-STAMP))

;;; This may need some work to really work right if locations-per-line changes
(DEFMETHOD (SHEET :SET-SUPERIOR) (NEW-SUPERIOR &AUX ACTIVE-P)
  (UNLESS (EQ NEW-SUPERIOR SUPERIOR)
    (DELAYING-SCREEN-MANAGEMENT
      (AND EXPOSED-P (SEND SELF :DEEXPOSE))
      (WITHOUT-INTERRUPTS
        (COND ((SETQ ACTIVE-P (MEMQ SELF (SHEET-INFERIORS SUPERIOR)))
               (SETF (SHEET-INFERIORS SUPERIOR) (DELQ SELF (SHEET-INFERIORS SUPERIOR)))
               (SEND SUPERIOR :ORDER-INFERIORS)
               (SCREEN-AREA-HAS-CHANGED SELF)))
        (SETQ SUPERIOR NEW-SUPERIOR
              LOCATIONS-PER-LINE (SHEET-LOCATIONS-PER-LINE NEW-SUPERIOR))
        (SHEET-SET-SUPERIOR-PARAMS SELF LOCATIONS-PER-LINE)
        (COND (BIT-ARRAY
                 (SETQ BIT-ARRAY
                       (GROW-BIT-ARRAY BIT-ARRAY (PIXELS-PER-PHYSICAL-LINE) HEIGHT WIDTH))
                 (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-TYPE SCREEN-ARRAY)
                                 (PIXEL-ARRAY-WIDTH BIT-ARRAY)
                                 HEIGHT
                                 BIT-ARRAY 0))
              (T
               (REDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-TYPE OLD-SCREEN-ARRAY)
                               (PIXEL-ARRAY-WIDTH (SHEET-SUPERIOR-SCREEN-ARRAY))
                               HEIGHT
                               (SHEET-SUPERIOR-SCREEN-ARRAY)
                               (+ X-OFFSET (* Y-OFFSET (PIXELS-PER-PHYSICAL-LINE))))))
        (WHEN ACTIVE-P
          (SHEET-CONSING
            (SETF (SHEET-INFERIORS NEW-SUPERIOR)
                  (CONS SELF (COPY-LIST (SHEET-INFERIORS NEW-SUPERIOR)))))
          (SEND NEW-SUPERIOR :ORDER-INFERIORS)
          (SCREEN-AREA-HAS-CHANGED SELF))
        (SEND SELF :UPDATE-TIME-STAMP)))))

(DEFUN SHEET-SET-SUPERIOR-PARAMS (SHEET LOC-PER-LINE)
  (SETF (SHEET-LOCATIONS-PER-LINE SHEET) LOC-PER-LINE)
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SHEET-SET-SUPERIOR-PARAMS I LOC-PER-LINE)))

;;;; Sheet exposure/deexposure

;;; Normal sheets ignore notification about exposure/deexposure/change-of-edges/selection
(DEFMETHOD (SHEET :INFERIOR-EXPOSE) (SHEET) SHEET)
(DEFMETHOD (SHEET :INFERIOR-DEEXPOSE) (SHEET) SHEET)
(DEFMETHOD (SHEET :INFERIOR-SET-EDGES) (SHEET &REST IGNORE) SHEET)
(DEFMETHOD (SHEET :INFERIOR-SELECT) (SHEET &REST IGNORE) SHEET)
(DEFMETHOD (SHEET :INFERIOR-BURY) (SHEET) SHEET)

(DEFVAR *SHEETS-MADE-INVISIBLE-TO-MOUSE*)

(DEFMETHOD (SHEET :EXPOSABLE-P) ()
  (NOT (NOT (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))))

(DEFUN-METHOD SHEET-PREPARE-FOR-EXPOSE SHEET
              (SHEET INSIDE-EXPOSE-METHOD
               &OPTIONAL TURN-ON-BLINKERS BITS-ACTION (X X-OFFSET) (Y Y-OFFSET))
  (DECLARE (VALUES OK BITS-ACTION ERROR)
           (IGNORE TURN-ON-BLINKERS))
  (LET ((OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG)
        (INHIBIT-SCHEDULING-FLAG T)
        RESULT)
    (TAGBODY
     MAIN-LOOP
        (SETQ INHIBIT-SCHEDULING-FLAG T)
        (UNLESS (SHEET-CAN-GET-LOCK SHEET)
          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
          (PROCESS-WAIT "Window Lock" 'SHEET-CAN-GET-LOCK SHEET)
          (GO MAIN-LOOP))
        (WHEN EXPOSED-P
          (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
            (VALUES T BITS-ACTION NIL)))
        (OR (NOT INSIDE-EXPOSE-METHOD)
            (NULL SUPERIOR)
            (MEMQ SELF (SHEET-INFERIORS SUPERIOR))
            ;; We can only be exposed if we are activated
            (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
              (VALUES NIL
                      BITS-ACTION
                      (LIST NIL "Attempt to expose deactivated sheet ~S" SELF))))
        (WHEN (OR ( X-OFFSET X) ( Y-OFFSET Y))
          (AND INSIDE-EXPOSE-METHOD
               (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
                 (VALUES NIL BITS-ACTION NIL)))
          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
          (SHEET-SET-DEEXPOSED-POSITION X Y)
          (GO MAIN-LOOP))
        (OR (NULL SUPERIOR)
            (NOT INSIDE-EXPOSE-METHOD)
            (SHEET-WITHIN-SHEET-P SELF SUPERIOR)
            (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
              (VALUES NIL
                      BITS-ACTION
                      (LIST NIL "Attempt to expose ~S outside of its superior" SELF))))
        ;; If our superior is temp locked, see if we will overlap any
        ;; of the temp windows.  If we will, then wait until the temp window is
        ;; deexposed then try again
        (WHEN (AND SUPERIOR
                   (CONSP (SHEET-LOCK SUPERIOR))
                   (SETQ RESULT
                         (DOLIST (TEMP-SHEET (SHEET-LOCK SUPERIOR))
                           (AND (SHEET-OVERLAPS-SHEET-P TEMP-SHEET SELF)
                                (RETURN TEMP-SHEET)))))
          (WHEN INSIDE-EXPOSE-METHOD
            (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
              (VALUES NIL BITS-ACTION NIL)))
          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
          (PROCESS-WAIT "Sheet Deexpose"
                        (LAMBDA (TEMP-SHEET SUP)
                          (OR (NOT (CONSP (SHEET-LOCK SUP)))
                              (NOT (MEMQ TEMP-SHEET (SHEET-LOCK SUP)))))
                        RESULT SUPERIOR)
          (GO MAIN-LOOP))
        (COND ((SHEET-TEMPORARY-P)
               (SETQ RESULT
                     (CATCH 'SHEET-EXPOSE-CANT-GET-LOCK
                       ;; Check to make sure we can get all the locks at once
                       (MAP-OVER-EXPOSED-SHEET
                         (LAMBDA (TARGET)
                           (DECLARE (SYS:DOWNWARD-FUNCTION))
                           (AND ;; Can't be us, we aren't exposed yet
                                (NEQ TARGET (SHEET-SUPERIOR SELF))
                                ;; Sheet may be on EXPOSED-INFERIORS, but not
                                ;; in actuality exposed
                                (SHEET-EXPOSED-P TARGET)
                                (SHEET-OVERLAPS-SHEET-P SELF TARGET)
                                (OR (SHEET-CAN-GET-TEMPORARY-LOCK TARGET SELF)
                                    (THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET))
                                ;; If this window owns the mouse, must force
                                ;; mouse out of it
                                (EQ TARGET MOUSE-WINDOW)
                                (THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET)))
                         SUPERIOR)
                       ;; We can, get them all and win totally, but only do this if
                       ;; we are inside the expose method proper
                       (AND INSIDE-EXPOSE-METHOD
                            (MAP-OVER-EXPOSED-SHEET
                              (LAMBDA (TARGET)
                                (DECLARE (SYS:DOWNWARD-FUNCTION))
                                (COND ((AND ;; Can't be us, we aren't exposed yet
                                            (NEQ TARGET (SHEET-SUPERIOR SELF))
                                            ;; Sheet may be on EXPOSED-INFERIORS, but not
                                            ;; in actuality exposed
                                            (SHEET-EXPOSED-P TARGET)
                                            (SHEET-OVERLAPS-SHEET-P SELF TARGET))
                                       ;; All blinkers must get turned off on this sheet
                                       (SHEET-OPEN-BLINKERS TARGET)
                                       (OR (SHEET-GET-TEMPORARY-LOCK TARGET SELF)
                                           (FERROR "Internal error, can't get lock on ~A, ~
                                                      but we already verified we could get lock"
                                                   TARGET))
                                       (PUSH TARGET TEMPORARY-WINDOWS-LOCKED))))
                              SUPERIOR))
                       ;; Return NIL indicating that we are winning
                       NIL))
               (COND ((NULL RESULT)
                      (AND INSIDE-EXPOSE-METHOD
                           ;; For temporary windows, we must open the blinkers of our
                           ;; superiors to all levels
                           (SHEET-OPEN-ALL-BLINKERS SUPERIOR)))
                     (INSIDE-EXPOSE-METHOD
                      (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
                        (VALUES NIL BITS-ACTION NIL)))
                     ((EQ RESULT MOUSE-WINDOW)
                      (SETQ MOUSE-RECONSIDER T)
                      (PUSH RESULT *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
                      (SETF (SHEET-INVISIBLE-TO-MOUSE-P RESULT) T)
                      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                      (PROCESS-WAIT "Mouse Out"
                                    (LAMBDA (SHEET) (NEQ MOUSE-WINDOW SHEET))
                                    RESULT)
                      (GO MAIN-LOOP))
                     (T
                      ;; One we couldn't get: wait for it
                      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                      (PROCESS-WAIT "Temp Lock"
                                    (LAMBDA (TARGET SHEET)
                                      (OR (NOT (SHEET-EXPOSED-P TARGET))
                                          (NOT (SHEET-OVERLAPS-SHEET-P SHEET TARGET))
                                          (SHEET-CAN-GET-TEMPORARY-LOCK TARGET SHEET)))
                                    RESULT SELF)
                      (GO MAIN-LOOP))))
              (SUPERIOR
               ;; Deexpose all we will overlap, then loop again as the world may have
               ;; changed out from under us
               (LET ((FLAG NIL))
                 (DOLIST (SIBLING (SHEET-EXPOSED-INFERIORS SUPERIOR))
                   (WHEN (AND (NEQ SELF SIBLING) (SHEET-OVERLAPS-SHEET-P SELF SIBLING))
                     (WHEN INSIDE-EXPOSE-METHOD
                       (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
                         (VALUES NIL BITS-ACTION NIL)))
                     (SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG
                           FLAG T)
                     (SEND SIBLING :DEEXPOSE)))
                 (WHEN FLAG
                   ;; If had to deexpose someone, world may have changed
                   (GO MAIN-LOOP)))))
        ;; We have successfully met all of the requirements, be successful
        (RETURN-FROM SHEET-PREPARE-FOR-EXPOSE
          (VALUES T BITS-ACTION NIL)))))

(DEFUN-METHOD SHEET-EXPOSE SHEET
              (DAEMON-ARGS INTERNALS &AUX (*SHEETS-MADE-INVISIBLE-TO-MOUSE* NIL) VAL1 VAL2 VAL3)
  (IF (OR (NULL SUPERIOR)
          (TYPEP SUPERIOR 'SCREEN)
          (SEND SUPERIOR :INFERIOR-EXPOSE SELF))
      (DELAYING-SCREEN-MANAGEMENT
        (UNWIND-PROTECT
            (DO ((DONE NIL) ERROR) (DONE)
              (APPLY #'SHEET-PREPARE-FOR-EXPOSE SELF NIL (CDR DAEMON-ARGS))
              (SETQ ERROR
                    (CATCH 'SHEET-ABORT-EXPOSE
                      (LOCK-SHEET (SELF)
                        (MULTIPLE-VALUE-SETQ (VAL1 VAL2 VAL3) (FUNCALL INTERNALS DAEMON-ARGS))
                        (SETQ DONE T)
                        NIL)))
              (AND (NOT DONE) ERROR
                   (APPLY #'FERROR ERROR)))
          (DOLIST (SHEET *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
            (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL))
          (MOUSE-WAKEUP))
        (VALUES VAL1 VAL2 VAL3))))

(DEFWRAPPER (SHEET :EXPOSE) (IGNORE . BODY)
  `(SHEET-EXPOSE SI::.DAEMON-CALLER-ARGS.
                 (LAMBDA (SI::.DAEMON-CALLER-ARGS.
                          &AUX FOO (SI::.DAEMON-MAPPING-TABLE. SYS:SELF-MAPPING-TABLE))
                   (declare (sys:downward-function))
                   ;; Local slot 1 must contain our mapping table
                   ;; in something that contains combined method code.
                   FOO
                   SI::.DAEMON-MAPPING-TABLE.
                   . ,BODY)))

;;; TURN-ON-BLINKERS means that this window will soon become the SELECTED-WINDOW,
;;; so it is not necessary to change blinkers from :BLINK to their
;;; DESELECTED-BLINKER-VISIBILITY.
(DEFMETHOD (SHEET :EXPOSE) (&OPTIONAL INHIBIT-BLINKERS BITS-ACTION (X X-OFFSET) (Y Y-OFFSET))
  "Expose a sheet (place it on the physical screen)"
  (BLOCK EXPOSE
    (LET ((OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG)
          (INHIBIT-SCHEDULING-FLAG T)
          SUPERIOR-HAS-SCREEN-ARRAY
          OK ERROR)
      (SETQ RESTORED-BITS-P T)
      (OR BITS-ACTION (SETQ BITS-ACTION (IF BIT-ARRAY :RESTORE :CLEAN)))
      (AND EXPOSED-P (RETURN-FROM EXPOSE NIL))
      (SETQ RESTORED-BITS-P NIL)
      (SETQ SUPERIOR-HAS-SCREEN-ARRAY (OR (NULL SUPERIOR) (SHEET-SCREEN-ARRAY SUPERIOR)))
      (MULTIPLE-VALUE (OK BITS-ACTION ERROR)
        (SHEET-PREPARE-FOR-EXPOSE SELF T INHIBIT-BLINKERS BITS-ACTION X Y))
      (OR OK (THROW 'SHEET-ABORT-EXPOSE ERROR))
      ;; Have made our area of the screen safe for us.  We'll now call ourselves
      ;; "exposed", even though we haven't put our bits on the screen at all.  This
      ;; will win, because we have ourself locked, and if someone wants to cover us
      ;; he'll have to go blocked until we are done -- it's a cretinous thing to have
      ;; happen, but the system shouldn't come crashing to the ground because of it.
      ;; *** INHIBIT-SCHEDULING-FLAG had better still be T ***
      (OR INHIBIT-SCHEDULING-FLAG
          (FERROR "Hairy part of expose finished with ~S off" 'INHIBIT-SCHEDULING-FLAG))
      ;; Lie by saying that we are exposed, because we aren't really, but we are
      ;; locked so it doesn't matter
      (AND SUPERIOR-HAS-SCREEN-ARRAY (SETQ EXPOSED-P T PREPARED-SHEET NIL))
      (AND SUPERIOR
           (OR (NOT (MEMQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))
               ;; Must always reorder in the case of temporary windows since they
               ;; are the only type of window that can be exposed and overlapping some
               ;; other exposed window
               (SHEET-TEMPORARY-P))
           (SHEET-CONSING
             (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                   (CONS SELF (COPYLIST (DELQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR)))))))
      (COND ((AND SUPERIOR-HAS-SCREEN-ARRAY BIT-ARRAY)
             (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)
             ;; Open all our blinkers, etc, but don't think this sheet is prepared.
             (PREPARE-SHEET (SELF) )
             (SETQ PREPARED-SHEET NIL)
             (LET ((ARRAY (IF SUPERIOR
                              (SHEET-SUPERIOR-SCREEN-ARRAY)
                            (SCREEN-BUFFER SELF))))
               (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-TYPE SCREEN-ARRAY)
                               (PIXEL-ARRAY-WIDTH ARRAY)
                               (PIXEL-ARRAY-HEIGHT SCREEN-ARRAY)
                               ARRAY
                               (+ X-OFFSET (* Y-OFFSET (PIXEL-ARRAY-WIDTH ARRAY))))))
            (SUPERIOR-HAS-SCREEN-ARRAY
             (SETQ SCREEN-ARRAY OLD-SCREEN-ARRAY)
             (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
      (COND ((AND SUPERIOR-HAS-SCREEN-ARRAY (SHEET-TEMPORARY-P))
             (IF (EQ TEMPORARY-BIT-ARRAY T)
                 (SETQ TEMPORARY-BIT-ARRAY
                       (MAKE-PIXEL-ARRAY (PIXELS-PER-PHYSICAL-LINE) HEIGHT
                                         :AREA SHEET-AREA
                                         :TYPE (SHEET-ARRAY-TYPE SELF)))
               (PAGE-IN-PIXEL-ARRAY TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
             (BITBLT ALU-SETA WIDTH HEIGHT SCREEN-ARRAY 0 0 TEMPORARY-BIT-ARRAY 0 0)
             (PAGE-OUT-PIXEL-ARRAY TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT))))
      (DOLIST (SHEET *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
        (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL))
      (SETQ *SHEETS-MADE-INVISIBLE-TO-MOUSE* NIL)
      (MOUSE-DISCARD-CLICKAHEAD)
      (MOUSE-WAKEUP)
      ;; This goes after preceeding code so that blinkers won't accidentally
      ;; turn on before the bits get BITBLT'ed into the temporary array
      (SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG)
      (COND (SUPERIOR-HAS-SCREEN-ARRAY
             (CCASE BITS-ACTION
               (:NOOP NIL)
               (:RESTORE
                (SEND SELF :REFRESH :USE-OLD-BITS))
               (:CLEAN
                (SHEET-HOME SELF)
                (SEND SELF :REFRESH :COMPLETE-REDISPLAY)))
             (OR INHIBIT-BLINKERS
                 (DESELECT-SHEET-BLINKERS SELF))
             (OR BIT-ARRAY
                 ;; Expose in opposite order for the sake of temporary windows
                 (DOLIST (INFERIOR (REVERSE EXPOSED-INFERIORS))
                   (SEND INFERIOR :EXPOSE NIL)))
             (RETURN-FROM EXPOSE T))))))

(DEFUN-METHOD SHEET-DEEXPOSE SHEET (DAEMON-ARGS INTERNALS)
  (IF (OR (NULL SUPERIOR)
          (TYPEP SUPERIOR 'SCREEN)
          (SEND SUPERIOR :INFERIOR-DEEXPOSE SELF))
      (UNWIND-PROTECT
        (PROGN
          ;; Always make ourselves invisible to the mouse
          (SETF (SHEET-INVISIBLE-TO-MOUSE-P SELF) T)
          (LET ((INHIBIT-SCHEDULING-FLAG T))
            (COND ((SHEET-ME-OR-MY-KID-P MOUSE-SHEET SELF)
                   ;; The mouse is currently on me or one of my inferiors, get it out of there
                   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                   (IF SUPERIOR
                       (MOUSE-SET-SHEET SUPERIOR)
                       (IF (NEQ SELF DEFAULT-SCREEN)
                           (MOUSE-SET-SHEET DEFAULT-SCREEN)
                           (FERROR "Attempt to deexpose sheet ~S, which is the top level sheet~
                                        which owns mouse"
                                   SELF)))
                   (SETQ INHIBIT-SCHEDULING-FLAG T)))
            (COND ((AND (TYPEP MOUSE-WINDOW 'SHEET) (SHEET-ME-OR-MY-KID-P MOUSE-WINDOW SELF))
                   ;; Me or my inferior is the current mouse sheet, so force it out
                   (SETQ MOUSE-RECONSIDER T)
                   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                   (PROCESS-WAIT "Mouse Out"
                                 (LAMBDA (SHEET)
                                   (OR (NOT (TYPEP MOUSE-WINDOW 'SHEET))
                                       (NOT (SHEET-ME-OR-MY-KID-P MOUSE-WINDOW SHEET))))
                                 SELF))))
          (LOCK-SHEET (SELF)
            (FUNCALL INTERNALS DAEMON-ARGS)))
        (SETF (SHEET-INVISIBLE-TO-MOUSE-P SELF) NIL))))

(DEFWRAPPER (SHEET :DEEXPOSE) (IGNORE . BODY)
  `(SHEET-DEEXPOSE SI:.DAEMON-CALLER-ARGS.
                   (LAMBDA (SI:.DAEMON-CALLER-ARGS.
                            &AUX FOO (SI:.DAEMON-MAPPING-TABLE. SYS:SELF-MAPPING-TABLE))
                     (declare (sys:downward-function))
                     ;; Local slot 1 must contain our mapping table
                     ;; in something that contains combined method code.
                     FOO
                     ;; Make sure we are marked as requiring SELF-MAPPING-TABLE
                     ;; to be recomputed, because SHEET-DEEXPOSE set it to SHEET's.
                     SCREEN-ARRAY
                     SI:.DAEMON-MAPPING-TABLE.
                     . ,BODY)))

(DEFMETHOD (SHEET :DEEXPOSE) (&OPTIONAL (SAVE-BITS-P :DEFAULT) SCREEN-BITS-ACTION
                                        (REMOVE-FROM-SUPERIOR T))
  "Deexpose a sheet (removing it virtually from the physical screen, some bits may remain)"
  (DELAYING-SCREEN-MANAGEMENT
    (COND ((AND (EQ SAVE-BITS-P :DEFAULT) (NOT (ZEROP (SHEET-FORCE-SAVE-BITS))) EXPOSED-P)
           (SETQ SAVE-BITS-P :FORCE)
           (SETF (SHEET-FORCE-SAVE-BITS) 0)))
    (LET ((SW SELECTED-WINDOW))
      (AND SW (SHEET-ME-OR-MY-KID-P SW SELF)
           (SEND SW :DESELECT NIL)))
    (OR SCREEN-BITS-ACTION (SETQ SCREEN-BITS-ACTION :NOOP))
    (COND (EXPOSED-P
           (OR BIT-ARRAY        ;If we do not have a bit-array, take our inferiors off screen
               (EQ SAVE-BITS-P :FORCE)  ;but leave them in EXPOSED-INFERIORS
               (DOLIST (INFERIOR EXPOSED-INFERIORS)
                 (SEND INFERIOR :DEEXPOSE SAVE-BITS-P :NOOP NIL)))
           (WITHOUT-INTERRUPTS
             (AND (EQ SAVE-BITS-P :FORCE)
                  (NULL BIT-ARRAY)
                  (SETQ BIT-ARRAY
                        (MAKE-PIXEL-ARRAY (PIXELS-PER-PHYSICAL-LINE) HEIGHT
                                          :TYPE (SHEET-ARRAY-TYPE SELF)
                                          :AREA SHEET-AREA)
                        OLD-SCREEN-ARRAY NIL))
             (PREPARE-SHEET (SELF)
               (AND SAVE-BITS-P BIT-ARRAY
                    (PROGN (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))
                           (BITBLT ALU-SETA WIDTH HEIGHT SCREEN-ARRAY 0 0 BIT-ARRAY 0 0)
                           (PAGE-OUT-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))))
             (COND ((SHEET-TEMPORARY-P)
                    (PAGE-IN-PIXEL-ARRAY TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT))
                    (BITBLT ALU-SETA WIDTH HEIGHT TEMPORARY-BIT-ARRAY 0 0 SCREEN-ARRAY 0 0)
                    (PAGE-OUT-PIXEL-ARRAY TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT))
                    (DOLIST (SHEET TEMPORARY-WINDOWS-LOCKED)
                      (SHEET-RELEASE-TEMPORARY-LOCK SHEET SELF))
                    (SETQ TEMPORARY-WINDOWS-LOCKED NIL))
                   (T
                    (CCASE SCREEN-BITS-ACTION
                      (:NOOP)
                      (:CLEAN
                       (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ALU-ANDCA SELF)))))
             (SETQ EXPOSED-P NIL)
             (AND REMOVE-FROM-SUPERIOR SUPERIOR
                  (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                        (DELQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR))))
             (IF (NULL BIT-ARRAY)
                 (SETQ OLD-SCREEN-ARRAY SCREEN-ARRAY SCREEN-ARRAY NIL)
                 (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-TYPE BIT-ARRAY)
                                 (PIXEL-ARRAY-WIDTH BIT-ARRAY)
                                 (PIXEL-ARRAY-HEIGHT BIT-ARRAY)
                                 BIT-ARRAY 0))
             (cond ((not (eq deexposed-typeout-action :permit))
                    (SETF (SHEET-OUTPUT-HOLD-FLAG) 1)))))
          (REMOVE-FROM-SUPERIOR
           (AND SUPERIOR
                (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                      (DELQ SELF (SHEET-EXPOSED-INFERIORS SUPERIOR))))))))

;;; Don't make this anything but a multiple of 32.
(defvar *wipe-increment* 64.)
(defvar *saved-bit-arrays-wipe?* nil)

(DEFMETHOD (SHEET :REFRESH) (&OPTIONAL (TYPE :COMPLETE-REDISPLAY) &AUX PAGE-IN)
  (SETQ RESTORED-BITS-P (AND BIT-ARRAY (NEQ TYPE :COMPLETE-REDISPLAY)))
  (SETQ PAGE-IN (AND BIT-ARRAY (IF EXPOSED-P RESTORED-BITS-P
                                 (OR (NEQ TYPE :USE-OLD-BITS)
                                     (NOT RESTORED-BITS-P)))))
  (IF PAGE-IN (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
  (COND (RESTORED-BITS-P
          (AND EXPOSED-P                ;If we are deexposed, this is a big no-op!
               (PREPARE-SHEET (SELF)
                 (when *saved-bit-arrays-wipe?*
                   (dotimes (i (floor width *wipe-increment*))
                     (bitblt alu-seta (- (- width *wipe-increment*)) height
                             screen-array 0 0
                             screen-array *wipe-increment* 0)
                     (bitblt alu-seta *wipe-increment* height
                             bit-array (- width (* (+ i 1) *wipe-increment*)) 0
                             screen-array 0 0)))
                   (BITBLT ALU-SETA WIDTH HEIGHT BIT-ARRAY 0 0 SCREEN-ARRAY 0 0)))
          (COND ((NEQ TYPE :USE-OLD-BITS)
                 (ERASE-MARGINS)
                 (SEND SELF :REFRESH-MARGINS))))
        (T
         (PREPARE-SHEET (SELF)
           (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ERASE-ALUF SELF))
         (SEND SELF :REFRESH-MARGINS)
         (DOLIST (INFERIOR INFERIORS)
           (AND (SHEET-EXPOSED-P INFERIOR)      ;EXPOSED-INFERIORS may not all be on screen
                (SEND INFERIOR :REFRESH :COMPLETE-REDISPLAY)))
;        (SEND SELF :SCREEN-MANAGE)
         (SCREEN-MANAGE-QUEUE SELF 0 0 WIDTH HEIGHT)))
  (IF PAGE-IN (PAGE-OUT-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))))

(DEFMETHOD (SHEET :REFRESH-MARGINS) () )

;;;; Exceptions

(DEFMETHOD (SHEET :HANDLE-EXCEPTIONS) ()
  (OR (ZEROP (SHEET-EXCEPTIONS)) (SHEET-HANDLE-EXCEPTIONS SELF)))

(DEFUN SHEET-HANDLE-EXCEPTIONS (SHEET)
  "Handle any exception flags set in SHEET, including output-hold, **more**, end of page, etc.
This is usually called after testing that (SHEET-EXCEPTIONS SHEET) is nonzero
to save time in the normal case."
  (UNLESS (ZEROP (SHEET-OUTPUT-HOLD-FLAG SHEET))
    (SEND SHEET :OUTPUT-HOLD-EXCEPTION))
  (UNLESS (ZEROP (SHEET-END-PAGE-FLAG SHEET))
    (SEND SHEET :END-OF-PAGE-EXCEPTION))
  (UNLESS (ZEROP (SHEET-MORE-FLAG SHEET))
    (COND (MORE-PROCESSING-GLOBAL-ENABLE
           (SEND SHEET :MORE-EXCEPTION)
           (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
               (SEND SHEET :END-OF-PAGE-EXCEPTION)))
          (T (SETF (SHEET-MORE-FLAG SHEET) 0))))
  (UNLESS (ZEROP (SHEET-EXCEPTIONS SHEET))
    (FERROR "Exceptions (~O) on sheet ~S won't go away" (SHEET-EXCEPTIONS SHEET) SHEET))
  NIL)

;;; Called by typeout routines when they discover there is not enough space to output another
;;; character.  Sheet has already been prepared when this is called.
(DEFMETHOD (SHEET :END-OF-LINE-EXCEPTION) ()
  ;; Put an "!" in the right margin if called for.
  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG))
      (SEND SELF :TYO-RIGHT-MARGIN-CHARACTER CURSOR-X CURSOR-Y #/!))
  ;; Move to left margin, next line, and clear it
  (SHEET-INCREMENT-BITPOS SELF (- CURSOR-X) LINE-HEIGHT)
  (SHEET-CLEAR-EOL SELF)                        ;If at end of page, this will home up first
  (OR (ZEROP (SHEET-EXCEPTIONS))                ;Take care of any residual **more**
      (SHEET-HANDLE-EXCEPTIONS SELF)))          ;since caller is about to type out

;;; This used to put continuation-line marks in the margin
;;; Note that when using variable-width fonts, the mark is placed relative to the
;;; right margin rather than relative to the text that is already there.  Hope this is right.
(DEFMETHOD (SHEET :TYO-RIGHT-MARGIN-CHARACTER)
           (XPOS YPOS CH
            &AUX (FONT (AREF FONT-MAP 0))
            (WID (SHEET-CHARACTER-WIDTH SELF CH FONT)))
  XPOS ;Ignored now, but supplied in case I decide to change where the character goes
  (PREPARE-SHEET (SELF)
    (DRAW-CHAR FONT CH (- (SHEET-INSIDE-RIGHT) WID) YPOS CHAR-ALUF SELF)))

(DEFMETHOD (SHEET :END-OF-PAGE-EXCEPTION) ()
  (UNLESS (ZEROP (SHEET-END-PAGE-FLAG))
    (LET ((M-VP MORE-VPOS))             ;SHEET-HOME smashes this, since it moves the cursor
      ;; Wrap around to top of sheet
      (SHEET-HOME SELF)
      (SHEET-CLEAR-EOL SELF)
      ;; Arrange for more processing next time around
      (COND ((NULL M-VP))                       ;No more processing at all
            (( M-VP #o100000)                  ;More processing delayed?
             (SETQ MORE-VPOS (- M-VP #o100000)))        ;Cause to happen next time around
            (T (SETQ MORE-VPOS (SHEET-DEDUCE-MORE-VPOS SELF)))))))

(DEFUN-METHOD SHEET-MORE-HANDLER SHEET
              (&OPTIONAL (OPERATION :TYI) (MORE-STRING "**MORE**")
                         &AUX (CURRENT-X CURSOR-X) CHAR STOP-FLAG)
  "This function is the default way of handing **MORE** exceptions.
It outputs **MORE**, performs OPERATION (by default :TYI) to read a character,
then erases the **MORE** and returns the character read.
Untyi'ing the character or flushing future output and typing **FLUSHED**
is the caller's responsibility.  Note that the character returned can be
NIL if a timeout occurs.

On return, the cursor points at the now-blank line which once held **MORE**.
Note that, if this line is the last line in the window, the end-of-page
exception will be set, so ordinarily the next output will happen on the top line.

Output is done with the SHEET- functions, not by sending messages.
It is sometimes useful that the output does not do any special things
that the normal output operations on SELF would do."
  (SETF (SHEET-MORE-FLAG) 0)            ;"Won't need MORE processing no more"
  (SETQ MORE-VPOS (+ #o100000 MORE-VPOS))       ;Defer more's while typing **MORE**
  (SHEET-CLEAR-EOL SELF)
  (LET ((OLD-FONT CURRENT-FONT)
        (OLD-CHAR-WIDTH CHAR-WIDTH))
    (WHEN MORE-STRING
      (UNWIND-PROTECT
          (PROGN
            (SETQ CURRENT-FONT (AREF FONT-MAP 0))
            (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH CURRENT-FONT))
            (SHEET-STRING-OUT SELF MORE-STRING))
        (SETQ CURRENT-FONT OLD-FONT)
        (SETQ CHAR-WIDTH OLD-CHAR-WIDTH))))

  (do-forever
    (WHEN (SEND SELF :OPERATION-HANDLED-P OPERATION)
      (cond ((or (zerop (sheet-time-out-on-more))
                 (null exposed-p)               ;do not time out if not exposed.
                 stop-flag)
             (SETQ CHAR (SHEET-MORE-LOCK-KLUDGE SELF OPERATION)))
            (t
             (with-timeout (*more-timeout-interval* nil)
               (SETQ CHAR (SHEET-MORE-LOCK-KLUDGE SELF OPERATION))))))
    (cond ((and (fixp char)
                (or (= char #\hold-output)
                    (= char #\stop-output)))
           (setq stop-flag t))
          (t (return nil))))    ;exit do-forever

  (WHEN MORE-STRING
    (SETQ CURSOR-X CURRENT-X)                   ;Wipe out the **MORE**
    (SHEET-CLEAR-EOL SELF))
  (COND (( (+ CURSOR-Y LINE-HEIGHT)
            (+ TOP-MARGIN-SIZE (1- (* (1- (SHEET-NUMBER-OF-INSIDE-LINES)) LINE-HEIGHT))))
         (IF (NOT (NULL MORE-VPOS))             ;Might have been disabled while waiting!!
             (SETQ MORE-VPOS 0))
         (SETF (SHEET-END-PAGE-FLAG) 1))        ;Wrap around unless flushed
        ;; At bottom, wrap around (or scroll)
        ;; Next MORE will happen at same place
        (T (SEND SELF :NOTICE :INPUT-WAIT)))    ;Otherwise, MORE one line up next time
  CHAR)

(DEFMETHOD (SHEET :MORE-EXCEPTION) ()
  (OR (ZEROP (SHEET-MORE-FLAG))
      (SHEET-MORE-HANDLER)))

(DEFMETHOD (SHEET :OUTPUT-HOLD-EXCEPTION) ()
  (OR (ZEROP (SHEET-OUTPUT-HOLD-FLAG))
      EXPOSED-P                         ;Output held due to deexposure
      (CASE DEEXPOSED-TYPEOUT-ACTION
        (:NORMAL)
        (:ERROR                         ;Give error if attempting typeout?
          (FERROR 'OUTPUT-ON-DEEXPOSED-SHEET
                  "Attempt to typeout on ~S, which is deexposed"
                  SELF))
        (:PERMIT
         ;; OUTPUT-HOLD gets cleared at this level, rather than never getting set when
         ;; deexposing, so that software knows if a sheet actually did typeout, as opposed to
         ;; it being permitted.  This allows software to know if it needs to update a
         ;; partially exposed window's bits, for example.  It is similar to a page-fault
         ;; handler's setting the write-protect bit on write enabled pages to detect when a
         ;; page is actually modified (READ-WRITE-FIRST)
         (AND SCREEN-ARRAY (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
        (:EXPOSE
         (SEND SELF :EXPOSE))
        (:NOTIFY
         (SEND SELF :NOTICE :OUTPUT))           ;Consider notifying the user
        (OTHERWISE
         (IF (CONSP DEEXPOSED-TYPEOUT-ACTION)
             (LEXPR-SEND SELF DEEXPOSED-TYPEOUT-ACTION)
             (FERROR "~S is not a recognized ~S"
                     DEEXPOSED-TYPEOUT-ACTION 'DEEXPOSED-TYPEOUT-ACTION)))))
  (PROCESS-WAIT "Output Hold"
                (LAMBDA (SHEET)
                  (NOT (SHEET-OUTPUT-HELD-P SHEET))) ;Wait until no output hold
                SELF))

;;; This is the default method for :NOTICE, which is always called last
;;; if all other methods have returned NIL.  It provides the default handling
;;; for deexposed input and output in notify mode, handles :INPUT-WAIT,
;;; and provides the special handling for errors vis a vis the window system.
;;; Other events are completely ignored; presumably they shouldn't be noticed by windows
;;; which don't have flavors to handle them.
;;; No currently-defined events use the ARGS argument, but it is there for
;;; future extensibility.
(DEFMETHOD (SHEET :NOTICE) (EVENT &REST ARGS)
  (CASE EVENT
    ((:INPUT :OUTPUT)           ;Deexposed window needs some attention
     ;; Wait for there to be a place to notify
     (PROCESS-WAIT "A Selected Window" (LAMBDA () SELECTED-WINDOW))
     ;; Now, if this window is visible we don't need to bother notifying
     (OR (LOOP FOR W = SELF THEN (SHEET-SUPERIOR W) UNTIL (NULL W)
               ALWAYS (SHEET-EXPOSED-P W))
         (NOTIFY SELF "Process ~A wants ~A" (PROCESS-NAME CURRENT-PROCESS)
                 (IF (EQ EVENT :OUTPUT) "to type out" "typein")))
     T)
    (:INPUT-WAIT                ;Hanging up waiting for input.
     (SETF (SHEET-MORE-FLAG) 0)                 ;Decide when we need to **more** next
     (COND ((NULL MORE-VPOS))                   ;Unless MORE inhibited entirely
           ((< (* (- (SHEET-INSIDE-BOTTOM) CURSOR-Y) 4) ;More than 3/4 way down window?
               (SHEET-INSIDE-HEIGHT))
            ;; Wrap around and more just before the current line
            (SETQ MORE-VPOS (+ #o100000 (- CURSOR-Y LINE-HEIGHT))))
           (T ;; More at bottom
            (SETQ MORE-VPOS (SHEET-DEDUCE-MORE-VPOS SELF))))
     (AND (NOT EXPOSED-P)                       ;Send a notification if desired
          (NOT (ZEROP (SHEET-DEEXPOSED-TYPEIN-NOTIFY)))
          (SEND SELF :NOTICE :INPUT))
     T)
    (:ERROR                     ;Error in process using this window as its *TERMINAL-IO*.
                                ;Notify if possible, and decide whether to use this
                                ;window or the cold-load stream.
     ;; Value is COLD-LOAD-STREAM to tell debugger to use that,
     ;; or TOO-SMALL meaning we altered *TERMINAL-IO* because we didn't think
     ;; the user would want to use it, or NIL.
     ;; If NIL and *TERMINAL-IO* has changed,
     ;; the debugger should propagate the change to the SG that bombed.
     (IF (OR (< (SHEET-INSIDE-WIDTH) (* CHAR-WIDTH 35.))
             (< (SHEET-INSIDE-HEIGHT) (* LINE-HEIGHT 5)))
         ;; If window absurdly small, don't use it.
         ;; Get a background window instead.
         (IF (TYPEP SELF 'BACKGROUND-LISP-INTERACTOR)
             'COLD-LOAD-STREAM
           (SETQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
           (OR (SEND *TERMINAL-IO* :NOTICE EVENT ARGS)
               'TOO-SMALL))
       (IF (LOOP FOR W = SELF THEN (SHEET-SUPERIOR W) UNTIL (NULL W)
                 ALWAYS (SHEET-EXPOSED-P W))
           ;; If window is visible, go ahead and use it.
           (WAIT-TILL-SAFE-FOR-ERROR SELF 'SHEET-CAN-GET-LOCK SELF)
         ;; Otherwise must notify.
         (OR (LET ((PROCESS-IS-IN-ERROR SELF))
               (WAIT-TILL-SAFE-FOR-ERROR SELF 'NOTIFY-POSSIBLE-P SELF))
             (PROGN
               (NOTIFY SELF "Process ~A got an error" (PROCESS-NAME CURRENT-PROCESS))
               ;; If notifying for an error, remain "in error" until selected
               (LET ((PROCESS-IS-IN-ERROR SELF))
                 (PROCESS-WAIT "Selected" (LAMBDA (W) (EQ SELECTED-WINDOW W)) SELF)
                 NIL))))))
    (OTHERWISE NIL)))           ;Ignore unknown events (could signal error instead?)

(DEFVAR LOCKED-ERROR-WINDOWS NIL
  "List of windows waiting for window system locks because they want to
enter the error handler.  Looked at by mouse process.")
(ADD-INITIALIZATION "locked error windows" '(SETQ LOCKED-ERROR-WINDOWS NIL)
                    '(:WARM))

(DEFVAR *WINDOWS-LOCKED-ERROR-QUERY* T
  "T means ask (in cold load stream) what to do about background error with windows locked.
NIL means just wait for user to resolve problem, e.g. by pressing something like
Terminal Call or Terminal Control-Clear-Input.")

(DEFUN WAIT-TILL-SAFE-FOR-ERROR (WINDOW FUNCTION &REST ARGS)
  "Wait until either (APPLY FUNCTION ARGS) is non-NIL or user does Terminal Call.
If user does Terminal Call (and picks WINDOW therein), returns
the symbol COLD-LOAD-STREAM; if FUNCTION returns non-NIL, we return NIL.
If *WINDOWS-LOCKED-ERROR-QUERY* is non-NIL, we immediately
ask user to choose to use cold load stream, unlock window locks, or do nothing.
/"Do nothing/" means we wait, as described above."
  (UNLESS (APPLY FUNCTION ARGS)
    (UNWIND-PROTECT
        (PROGN
          (WITHOUT-INTERRUPTS (PUSH WINDOW LOCKED-ERROR-WINDOWS))
          (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
          (IF *WINDOWS-LOCKED-ERROR-QUERY*
              ;; Situation has not been resolved yet; query user in cold-load stream
              (LET (ANSWER)
                (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
                  (LET ((*QUERY-IO* COLD-LOAD-STREAM))
                    (SEND COLD-LOAD-STREAM :CLEAR-INPUT)
                    (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
                    (SETQ ANSWER
                          (FQUERY '(:CHOICES (((:C "Cold load stream") #/C)
                                              ((:U "Clear all locks") #/U)
                                              ((:N "Nothing now") #/N)))
                                  "How do you want to handle error in process ~A?
You can handle it in the error handler by typing
        C  to use the cold-load stream (like Terminal Call),
        U  to forcibly unlock all windows so a notification can come out
           (like Terminal Control-Clear-input)
     or N  to tell it to wait until you do some other thing. "
                                  (PROCESS-NAME CURRENT-PROCESS))))
                  (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T))
                (IF (EQ ANSWER :C)
                    ;; Answer C means use cold load stream now.
                    (WITHOUT-INTERRUPTS
                      (SETQ LOCKED-ERROR-WINDOWS
                            (DELQ WINDOW LOCKED-ERROR-WINDOWS 1))))
                ;; Answer U means unlock locks, allowing notification now.
                (IF (EQ ANSWER :U) (SHEET-CLEAR-LOCKS))))
          ;; Wait until either the function supplied to us returns T
          ;; or someone removes this window from the locked list
          ;; (which means, telling us to use the cold load stream)
          (PROCESS-WAIT "Error notify"
                        (LAMBDA (FUNCTION ARGS WINDOW)
                          (OR (APPLY FUNCTION ARGS)
                              (NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))))
                        FUNCTION ARGS WINDOW)
          (IF (NOT (MEMQ WINDOW LOCKED-ERROR-WINDOWS))
              'COLD-LOAD-STREAM))
      (WITHOUT-INTERRUPTS (SETQ LOCKED-ERROR-WINDOWS
                                (DELQ WINDOW LOCKED-ERROR-WINDOWS 1))))))

(DEFUN NOTIFY-POSSIBLE-P (WINDOW)
  "T if it is possible at this instant to print a notification about WINDOW."
  (LET ((SW SELECTED-WINDOW))
    (AND (SHEET-CAN-GET-LOCK WINDOW)
         SW                                     ;No one in charge
         (NOT (SHEET-OUTPUT-HELD-P SW))         ;Guy in charge locked or broken
         (SHEET-CAN-GET-LOCK                    ;Anything locked, even by this process,
           (SHEET-SCREEN SW) T))))              ;would hang Terminal 0 S.

;;;; Blinkers

(DEFUN MAKE-BLINKER (SHEET &OPTIONAL (TYPE 'RECTANGULAR-BLINKER) &REST OPTIONS
                             &AUX PLIST BLINKER)
  "Return a new blinker of type TYPE that runs over SHEET.
OPTIONS are init keywords and values for MAKE-INSTANCE."
  (SETQ OPTIONS (COPY-LIST OPTIONS)
        PLIST (LOCF OPTIONS))
  (PUTPROP PLIST SHEET :SHEET)
  (SETQ BLINKER (INSTANTIATE-FLAVOR TYPE PLIST T NIL BLINKER-AREA))
  (WITHOUT-INTERRUPTS
    (PUSH BLINKER (SHEET-BLINKER-LIST SHEET)))
  BLINKER)

(DEFF DEFINE-BLINKER 'MAKE-BLINKER) ;Keep old name for compatibility.
(COMPILER:MAKE-OBSOLETE DEFINE-BLINKER "it has been renamed to TV:MAKE-BLINKER")

(DEFMETHOD (BLINKER :INIT) (IGNORE)
  (OR FOLLOW-P X-POS
      (SETQ X-POS (SHEET-CURSOR-X SHEET)
            Y-POS (SHEET-CURSOR-Y SHEET))))

(DEFMETHOD (RECTANGULAR-BLINKER :BEFORE :INIT) (IGNORE &AUX FONT)
  (SETQ FONT (AREF (SHEET-FONT-MAP SHEET) 0))
  (UNLESS WIDTH (SETQ WIDTH (FONT-BLINKER-WIDTH FONT)))
  (UNLESS HEIGHT (SETQ HEIGHT (FONT-BLINKER-HEIGHT FONT))))

(DEFMETHOD (RECTANGULAR-BLINKER :SIZE) ()
  (VALUES WIDTH HEIGHT))

;;; Make a blinker temporarily disappear from the screen.
;;; Anything that moves it or changes its parameters should call this.
;;; When the next clock interrupt happens with INHIBIT-SCHEDULING-FLAG clear,
;;; the blinker will come back on.  This is independent of the time until next
;;; blink, in order to provide the appearance of fast response.
;;; Anyone who calls this should have lambda-bound INHIBIT-SCHEDULING-FLAG to T.
;;; This is a noop if the sheet the blinker is on is output held.
(DEFUN OPEN-BLINKER (BLINKER)
  "Take BLINKER temporarily off the screen so it does not interfere with output.
The blinker will be put back by the scheduler if appropriate;
the caller must turn off interrupts if he expects the blinker
to stay open during any specific piece of code."
  (WHEN (AND (BLINKER-PHASE BLINKER)            ;If blinker on, turn it off
             (NOT (SHEET-OUTPUT-HELD-P (BLINKER-SHEET BLINKER))))
    (BLINK BLINKER)
    (SETF (BLINKER-TIME-UNTIL-BLINK BLINKER) 0))
  ;; Solid blinkers should stay off for a while so they don't flicker
  ;; while the user is typing.
  (AND (NEQ (BLINKER-VISIBILITY BLINKER) :BLINK)
       (< (BLINKER-TIME-UNTIL-BLINK BLINKER) 25.)
       (SEND BLINKER :DEFER-REAPPEARANCE))
  (IF (EQ BLINKER MOUSE-BLINKER) (%OPEN-MOUSE-CURSOR)))

(DEFMETHOD (BLINKER :DEFER-REAPPEARANCE) ()
  (SETQ TIME-UNTIL-BLINK 30.))

;;; This function should get called by the clock about every 60th of a second.
;;; Any blinkers which are supposed to be on but are off are turned on.
;;; Any blinkers which are supposed to be flashed are flashed if it is time.
;;; Note: we depend on the fact that blinkers temporarily turned off
;;; have their BLINKER-TIME-UNTIL-BLINK fields set to 0.
(DEFUN BLINKER-CLOCK (*BLINKER-DELTA-TIME*)
  (DECLARE (SPECIAL *BLINKER-DELTA-TIME*))
  (DOLIST (S ALL-THE-SCREENS)
    (AND (SHEET-EXPOSED-P S)
         (OR (NEQ S MAIN-SCREEN)
             (NOT COLD-LOAD-STREAM-OWNS-KEYBOARD))
         (BLINKER-CLOCK-INTERNAL S))))

(DEFUN BLINKER-CLOCK-INTERNAL (SHEET)
  (DECLARE (SPECIAL *BLINKER-DELTA-TIME*))
  (WHEN (AND (SHEET-EXPOSED-P SHEET)
             (ZEROP (SHEET-DONT-BLINK-BLINKERS-FLAG SHEET)))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (LET ((NEW-TIME (MAX 0 (- (OR (BLINKER-TIME-UNTIL-BLINK BLINKER) 0)
                                *BLINKER-DELTA-TIME*))))
        (SETF (BLINKER-TIME-UNTIL-BLINK BLINKER) NEW-TIME)
        (AND (ZEROP NEW-TIME)
             (CASE (BLINKER-VISIBILITY BLINKER)
               ((NIL :OFF)
                (BLINKER-PHASE BLINKER))
               ((T :ON)
                (NULL (BLINKER-PHASE BLINKER)))
               (:BLINK
                T))
             (NOT (SHEET-OUTPUT-HELD-P SHEET))
             (LET ((LV (SHEET-LOCK SHEET)))
               (OR (NULL LV) (CONSP LV)))
             ;; (CATCH-ERROR (BLINK BLINKER) NIL) -- removed Mly 18-Jul-85
             ;;   -- the debugger should be enough to hack this
             (BLINK BLINKER))))
    (AND (EQ SHEET MOUSE-SHEET)
         (= MOUSE-CURSOR-STATE 1)
         (= MOUSE-CURSOR-CLOSED-STATE 2)
         (NEQ WINDOW-OWNING-MOUSE 'STOP)
         (NOT (SHEET-OUTPUT-HELD-P SHEET))
         ( (OR (BLINKER-TIME-UNTIL-BLINK MOUSE-BLINKER) 0) 0)
         (LET ((LV (SHEET-LOCK SHEET)))
           (OR (NULL LV) (CONSP LV)))
         (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
               PREPARED-SHEET NIL))
    (DOLIST (S (SHEET-EXPOSED-INFERIORS SHEET))
      (BLINKER-CLOCK-INTERNAL S))))

(DEFWRAPPER (BLINKER :BLINK) (IGNORE . BODY)
  `(SHEET-IS-PREPARED (SHEET)
      . ,BODY))

(DEFMETHOD (BLINKER :BEFORE :BLINK) ()
  (SETQ PREPARED-SHEET NIL)                     ;Blinking any blinker makes us forget
  (SETQ TIME-UNTIL-BLINK HALF-PERIOD)           ;Schedule the next blink (wink??)
  (AND FOLLOW-P (SETQ X-POS (SHEET-CURSOR-X SHEET)
                      Y-POS (SHEET-CURSOR-Y SHEET))))

(DEFMETHOD (BLINKER :AFTER :BLINK) ()
  (SETQ PHASE (NOT PHASE)))

(DEFMETHOD (BLINKER :SET-CURSORPOS) (X Y &AUX (OLD-PHASE PHASE))
  "Set the position of a blinker relative to the sheet it is on.  Args in terms of
raster units.  If blinker was following cursor, it will no longer be doing so."
  (WITH-BLINKER-READY T
    (SETQ X (MIN (+ (MAX (FIX X) 0) (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
          Y (MIN (+ (MAX (FIX Y) 0) (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
    (COND ((NULL VISIBILITY)   ;Don't open if visibility NIL (especially the mouse cursor!)
           (SETQ X-POS X Y-POS Y FOLLOW-P NIL))
          ((OR (NEQ X X-POS)                    ;Only blink if actually moving blinker
               (NEQ Y Y-POS))
           (OPEN-BLINKER SELF)
           (SETQ X-POS X Y-POS Y FOLLOW-P NIL TIME-UNTIL-BLINK 0)
           ;; If this is the mouse blinker, and it is not being tracked by microcode,
           ;; then it is important to turn it back on immediately.
           (AND (NEQ VISIBILITY :BLINK)
                OLD-PHASE
                (BLINK SELF))))))

(DEFMETHOD (RECTANGULAR-BLINKER :SET-SIZE) (NWIDTH NHEIGHT)
  (WHEN (OR ( WIDTH NWIDTH)
            ( HEIGHT NHEIGHT))
    (WITH-BLINKER-READY ()
      (SETQ WIDTH NWIDTH HEIGHT NHEIGHT))))

(DEFMETHOD (RECTANGULAR-BLINKER :SET-SIZE-AND-CURSORPOS) (NWIDTH NHEIGHT X Y)
  "This is like :SET-SIZE and :SET-CURSORPOS together, in order to prevent
the user from seeing the intermediate state.  This prevents occasional
spasticity in menu blinkers, which looks terrible."
  (WITH-BLINKER-READY T
    (SETQ X (MIN (+ (MAX (FIX X) 0) (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
          Y (MIN (+ (MAX (FIX Y) 0) (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
    (COND ((NULL VISIBILITY)   ;Don't open if visibility NIL (especially the mouse cursor!)
           (SETQ X-POS X Y-POS Y FOLLOW-P NIL WIDTH NWIDTH HEIGHT NHEIGHT))
          ((OR (NEQ X X-POS)                    ;Only blink if actually moving blinker
               (NEQ Y Y-POS)
               (NEQ WIDTH NWIDTH)
               (NEQ HEIGHT NHEIGHT))
           (OPEN-BLINKER SELF)
           (SETQ X-POS X Y-POS Y FOLLOW-P NIL WIDTH NWIDTH HEIGHT NHEIGHT
                 TIME-UNTIL-BLINK 0)))))

(DEFMETHOD (BLINKER :SET-FOLLOW-P) (NEW-FOLLOW-P)
  "Turn on or off whether the blinker follows the sheet's typeout cursor."
  (UNLESS (EQ FOLLOW-P NEW-FOLLOW-P)
    (WITH-BLINKER-READY ()
      (SETQ FOLLOW-P NEW-FOLLOW-P))))

(DEFMETHOD (BLINKER :READ-CURSORPOS) ()
  "Returns the position of a blinker in raster units relative to the margins of the
sheet it is on"
  (VALUES (- (OR X-POS (SHEET-CURSOR-X SHEET))
             (SHEET-INSIDE-LEFT SHEET))
          (- (OR Y-POS (SHEET-CURSOR-Y SHEET))
             (SHEET-INSIDE-TOP SHEET))))

(DEFMETHOD (BLINKER :SET-VISIBILITY) (NEW-VISIBILITY &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Carefully alter the visibility of a blinker"
  (CHECK-TYPE NEW-VISIBILITY (MEMBER T NIL :BLINK :ON :OFF) "a valid blinker visibility type.")
  (COND ((EQ VISIBILITY NEW-VISIBILITY))
        ((EQ PHASE NEW-VISIBILITY)
         (SETQ VISIBILITY NEW-VISIBILITY))
        (T
         (DO () ((NOT (SHEET-OUTPUT-HELD-P SHEET)))
             (SETQ INHIBIT-SCHEDULING-FLAG NIL)
             (SEND SHEET :OUTPUT-HOLD-EXCEPTION)
             (SETQ INHIBIT-SCHEDULING-FLAG T))
         (OR NEW-VISIBILITY (OPEN-BLINKER SELF))
         (SETQ VISIBILITY NEW-VISIBILITY)
         ;; Blinker clock will fix the screen
         (SETQ TIME-UNTIL-BLINK 0))))

(DEFMETHOD (BLINKER :SET-SHEET) (NEW-SHEET &AUX EXCH-FLAG S-SUP S-INF)
  (UNLESS (EQ NEW-SHEET SHEET)
    ;; Only need to turn off blinker if it is turned on
    (WITH-BLINKER-READY ()
      (SETF (SHEET-BLINKER-LIST SHEET) (DELQ SELF (SHEET-BLINKER-LIST SHEET)))
      (PUSH SELF (SHEET-BLINKER-LIST NEW-SHEET))
      (IF (SHEET-ME-OR-MY-KID-P SHEET NEW-SHEET)
          (SETQ S-SUP NEW-SHEET
                S-INF SHEET
                EXCH-FLAG 1)
        (SETQ S-SUP SHEET
              S-INF NEW-SHEET
              EXCH-FLAG -1))
      (COND ((OR (= EXCH-FLAG 1)
                 (SHEET-ME-OR-MY-KID-P S-INF S-SUP))
             (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
                 (SHEET-CALCULATE-OFFSETS S-INF S-SUP)
               (SETQ X-POS
                     (MIN (MAX 0 (+ X-POS (* EXCH-FLAG X-OFF)))
                          (1- (SHEET-WIDTH NEW-SHEET))))
               (SETQ Y-POS
                     (MIN (MAX 0 (+ Y-POS (* EXCH-FLAG Y-OFF)))
                          (1- (SHEET-HEIGHT NEW-SHEET))))))
            (T
             ;; The sheets aren't related so directly, just put the blinker in the middle
             (SETQ X-POS (TRUNCATE (SHEET-WIDTH NEW-SHEET) 2)
                   Y-POS (TRUNCATE (SHEET-HEIGHT NEW-SHEET) 2))))
      (SETQ SHEET NEW-SHEET))))

(DEFMETHOD (RECTANGULAR-BLINKER :BLINK) ()
  "Standard style, rectangular blinker"
  ;; Should this insure blinker in range?
  (%DRAW-RECTANGLE-CLIPPED
    WIDTH HEIGHT
    (MAX (TRUNCATE WIDTH -2)
         (MIN (- (SHEET-WIDTH SHEET) (TRUNCATE WIDTH 2)) X-POS))
    (MAX (TRUNCATE HEIGHT -2)
         (MIN (- (SHEET-HEIGHT SHEET) (TRUNCATE HEIGHT 2)) Y-POS))
    ALU-XOR SHEET))

(DEFFLAVOR HOLLOW-RECTANGULAR-BLINKER () (RECTANGULAR-BLINKER)
  (:DOCUMENTATION :COMBINATION "A flavor of blinker that appears as a rectangular outline."))

;;; This sticks out by 1 pixel on the top and left but not on the bottom and
;;; right since that seems to be the right thing for boxing text -- this may be a crock
(DEFMETHOD (HOLLOW-RECTANGULAR-BLINKER :BLINK) ()
  (LET ((-X-POS- (1- X-POS)) (-Y-POS- (1- Y-POS)) (-HEIGHT- (1+ HEIGHT)) (-WIDTH- (1+ WIDTH)))
    (%DRAW-RECTANGLE-CLIPPED 1 -HEIGHT- -X-POS- -Y-POS- ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 1) 1 (+ -X-POS- 1) -Y-POS- ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED 1 (- -HEIGHT- 1)
                             (+ -X-POS- -WIDTH- -1) (+ -Y-POS- 1)
                             ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 2) 1
                             (+ -X-POS- 1) (+ -Y-POS- -HEIGHT- -1)
                             ALU-XOR SHEET)))

(DEFFLAVOR BOX-BLINKER () (RECTANGULAR-BLINKER)
  (:DOCUMENTATION :COMBINATION
                  "A flavor of blinker that appears as a thick rectangular outline."))

(DEFMETHOD (BOX-BLINKER :BLINK) ()
  (%DRAW-RECTANGLE-CLIPPED 2 HEIGHT X-POS Y-POS ALU-XOR SHEET)
  (%DRAW-RECTANGLE-CLIPPED (- WIDTH 2) 2 (+ X-POS 2) Y-POS ALU-XOR SHEET)
  (%DRAW-RECTANGLE-CLIPPED 2 (- HEIGHT 2) (+ X-POS WIDTH -2) (+ Y-POS 2) ALU-XOR SHEET)
  (%DRAW-RECTANGLE-CLIPPED (- WIDTH 4) 2 (+ X-POS 2) (+ Y-POS HEIGHT -2) ALU-XOR SHEET))

;;; Mixin that causes a blinker to stay entirely inside its sheet
(DEFFLAVOR STAY-INSIDE-BLINKER-MIXIN () ()
  (:REQUIRED-FLAVORS RECTANGULAR-BLINKER)
  (:DOCUMENTATION :MIXIN
                  "This mixin prevents lower right corner of blinker from being outside the sheet."))

(DEFWRAPPER (STAY-INSIDE-BLINKER-MIXIN :SET-CURSORPOS) (XY . BODY)
  `(PROGN (SETF (FIRST XY) (MIN (FIRST XY) (- (SHEET-INSIDE-WIDTH SHEET) WIDTH)))
          (SETF (SECOND XY) (MIN (SECOND XY) (- (SHEET-INSIDE-HEIGHT SHEET) HEIGHT)))
          . ,BODY))

(DEFFLAVOR IBEAM-BLINKER
        ((HEIGHT NIL))
        (BLINKER)
  (:INITABLE-INSTANCE-VARIABLES HEIGHT)
  (:DOCUMENTATION :COMBINATION "A blinker that appears as an I-beam."))

(DEFMETHOD (IBEAM-BLINKER :BEFORE :INIT) (IGNORE)
  (OR HEIGHT (SETQ HEIGHT (SHEET-LINE-HEIGHT SHEET))))

(DEFMETHOD (IBEAM-BLINKER :SIZE) ()
  (VALUES 9. HEIGHT))

(DEFMETHOD (IBEAM-BLINKER :BLINK) (&AUX X0)
  (%DRAW-RECTANGLE-CLIPPED 2 HEIGHT (MAX 0 (1- X-POS)) Y-POS ALU-XOR SHEET)
  (SETQ X0 (MAX 0 (- X-POS 4)))
  (%DRAW-RECTANGLE-CLIPPED (- (+ X-POS 5) X0) 2 X0 (MAX 0 (- Y-POS 2)) ALU-XOR SHEET)
  (%DRAW-RECTANGLE-CLIPPED (- (+ X-POS 5) X0) 2 X0 (+ Y-POS HEIGHT) ALU-XOR SHEET))

(DEFFLAVOR CHARACTER-BLINKER
        (FONT
         CHARACTER)
        (BLINKER)
  (:INITABLE-INSTANCE-VARIABLES FONT CHARACTER)
  (:INIT-KEYWORDS :CHAR)
  (:DOCUMENTATION :COMBINATION "A blinker whose appearance is a character from a font."))

(DEFMETHOD (CHARACTER-BLINKER :BEFORE :INIT) (PLIST)
  (UNLESS (VARIABLE-BOUNDP CHARACTER)
    (SETQ CHARACTER (GET PLIST :CHAR)))
  (SETQ FONT (SEND (SHEET-SCREEN SHEET) :PARSE-FONT-SPECIFIER FONT)))

(DEFMETHOD (CHARACTER-BLINKER :SIZE) ()
  (VALUES (SHEET-CHARACTER-WIDTH SHEET CHARACTER FONT) (FONT-BLINKER-HEIGHT FONT)))

(DEFMETHOD (CHARACTER-BLINKER :BLINK) ()
  "Use a character as a blinker.  Any font, any character"
  (DRAW-CHAR FONT CHARACTER X-POS Y-POS ALU-XOR SHEET))

(DEFMETHOD (CHARACTER-BLINKER :SET-CHARACTER) (NCHAR &OPTIONAL (NFONT FONT))
  (SETQ NFONT (SEND (SHEET-SCREEN SHEET) :PARSE-FONT-SPECIFIER NFONT))
  (AND (OR (NEQ NCHAR CHARACTER) (NEQ NFONT FONT))
       (WITH-BLINKER-READY NIL
         (SETQ CHARACTER NCHAR FONT NFONT))))

(DEFMETHOD (CHARACTER-BLINKER :CHARACTER) () (VALUES CHARACTER FONT))

(DEFFLAVOR BITBLT-BLINKER
        ((WIDTH NIL)
         (HEIGHT NIL)
         (ARRAY NIL))
        (MOUSE-BLINKER-MIXIN BLINKER)
  :INITABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :COMBINATION "A blinker whose appearance is made by BITBLTing an array."))

(DEFMETHOD (BITBLT-BLINKER :BEFORE :INIT) (IGNORE)
  (UNLESS ARRAY
    (UNLESS (AND WIDTH HEIGHT)
      (FERROR NIL "Attept to create a BITBLT-BLINKER without specifying an array or its size."))
    (SETQ ARRAY (MAKE-PIXEL-ARRAY (* 32. (CEILING WIDTH 32.)) HEIGHT
                                  :AREA SHEET-AREA
                                  :TYPE (SHEET-ARRAY-TYPE SHEET))))
  (IF (NULL WIDTH)
      (SETQ WIDTH (PIXEL-ARRAY-WIDTH ARRAY)))
  (IF (NULL HEIGHT)
      (SETQ HEIGHT (PIXEL-ARRAY-HEIGHT ARRAY))))

(DEFMETHOD (BITBLT-BLINKER :SIZE) ()
  (VALUES WIDTH HEIGHT))

(DEFMETHOD (BITBLT-BLINKER :SET-SIZE) (NWIDTH NHEIGHT)
  (UNLESS (AND ( (PIXEL-ARRAY-WIDTH ARRAY) NWIDTH) ( HEIGHT NHEIGHT))
    (SETQ ARRAY (MAKE-PIXEL-ARRAY (* 32. (CEILING NWIDTH 32.)) NHEIGHT
                                  :AREA SHEET-AREA
                                  :TYPE (SHEET-ARRAY-TYPE SHEET))))
  (SETQ WIDTH NWIDTH HEIGHT NHEIGHT))

(DEFMETHOD (BITBLT-BLINKER :BLINK) ()
  (LET* ((SCREEN-ARRAY (SHEET-SCREEN-ARRAY SHEET))
         (SWIDTH (PIXEL-ARRAY-WIDTH SCREEN-ARRAY))
         (SHEIGHT (PIXEL-ARRAY-HEIGHT SCREEN-ARRAY)))
    (BITBLT ALU-XOR
            (- (MIN SWIDTH (+ X-POS WIDTH))
               (MAX 0 X-POS))
            (- (MIN SHEIGHT (+ Y-POS HEIGHT))
               (MAX 0 Y-POS))
            ARRAY
            (MAX 0 (- X-POS))
            (MAX 0 (- Y-POS))
            SCREEN-ARRAY (MAX 0 X-POS) (MAX 0 Y-POS))))

(DEFFLAVOR MAGNIFYING-BLINKER ((MAGNIFICATION 3.)) (BITBLT-BLINKER)
  (:SETTABLE-INSTANCE-VARIABLES))

(DEFMETHOD (MAGNIFYING-BLINKER :AFTER :INIT) (IGNORE)
  (UNLESS (AND (ZEROP (CL:REM HEIGHT MAGNIFICATION))
               (ZEROP (CL:REM WIDTH MAGNIFICATION)))
    (FERROR "Height (~D) and width (~D) are not multiples of magnification (~D)."
            HEIGHT WIDTH MAGNIFICATION)))

(DEFMETHOD (MAGNIFYING-BLINKER :BEFORE :BLINK) ()
  (WHEN (NULL PHASE)
    (ARRAY-INITIALIZE ARRAY 0)
    (LET* ((SCREEN-ARRAY (SHEET-SCREEN-ARRAY SHEET))
           (SWIDTH (PIXEL-ARRAY-WIDTH SCREEN-ARRAY))
           (SHEIGHT (PIXEL-ARRAY-HEIGHT SCREEN-ARRAY)))
      (WHEN SCREEN-ARRAY
        (DO ((I (+ (TRUNCATE (- X-OFFSET) MAGNIFICATION) X-OFFSET X-POS) (1+ I))
             (I1 0 (+ I1 MAGNIFICATION)))
            (( I1 WIDTH))
          (DO ((J (+ (TRUNCATE (- Y-OFFSET) MAGNIFICATION) Y-OFFSET Y-POS) (1+ J))
               (J1 0 (+ J1 MAGNIFICATION))
               TEM)
              (( J1 HEIGHT))
            (WHEN (AND ( I 0) (< I SWIDTH)
                       ( J 0) (< J SHEIGHT)
                       (NOT (ZEROP (SETQ TEM (AR-2-REVERSE SCREEN-ARRAY I J)))))
              (DOTIMES (I2 MAGNIFICATION)
                (DOTIMES (J2 MAGNIFICATION)
                  (AS-2-REVERSE TEM ARRAY (+ I1 I2) (+ J1 J2)))))))
        (BITBLT ALU-XOR
                (- (MIN SWIDTH (+ X-POS WIDTH))
                   (MAX 0 X-POS))
                (- (MIN SHEIGHT (+ Y-POS HEIGHT))
                   (MAX 0 Y-POS))
                SCREEN-ARRAY (MAX 0 X-POS) (MAX 0 Y-POS)
                ARRAY
                (MAX 0 (- X-POS))
                (MAX 0 (- Y-POS))))))
  ;; Make a box around the whole thing so it can be found if screen is blank under it.
  (LET ((-X-POS- (+ X-POS -1)) (-Y-POS- (+ Y-POS -1))
        (-HEIGHT- (+ 2 HEIGHT)) (-WIDTH- (+ 2 WIDTH)))
    (%DRAW-RECTANGLE-CLIPPED 1 -HEIGHT- -X-POS- -Y-POS- ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 1) 1 (+ -X-POS- 1) -Y-POS- ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED 1 (- -HEIGHT- 1)
                             (+ -X-POS- -WIDTH- -1) (+ -Y-POS- 1)
                             ALU-XOR SHEET)
    (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 2) 1
                             (+ -X-POS- 1) (+ -Y-POS- -HEIGHT- -1)
                             ALU-XOR SHEET)))

(DEFMETHOD (MAGNIFYING-BLINKER :DEFER-REAPPEARANCE) () NIL)

(DEFFLAVOR REVERSE-CHARACTER-BLINKER
        ((CHARACTER NIL)
         (FONT T)
         (CHARACTER-X-OFFSET 0)
         (CHARACTER-Y-OFFSET 0))
        (BITBLT-BLINKER)
  :INITABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :COMBINATION "A blinker whose appearance is a character from a font
against a solid rectangular background."))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :BEFORE :INIT) (IGNORE)
  (IF (NULL CHARACTER) (FERROR "You must specify a character"))
  (SEND SELF :SET-CHARACTER NIL))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :SET-CHARACTER) (NEW-CHARACTER &OPTIONAL NEW-FONT)
  (IF NEW-CHARACTER (SETQ CHARACTER NEW-CHARACTER))
  (IF NEW-FONT (SETQ FONT NEW-FONT))
  (SETQ FONT (IF (EQ FONT T)
                 (SHEET-CURRENT-FONT SHEET)
                 (SEND (SHEET-SCREEN SHEET) :PARSE-FONT-SPECIFIER FONT)))
  (SETQ WIDTH (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
                (IF CWT (AREF CWT CHARACTER) (FONT-CHAR-WIDTH FONT))))
  (SETQ HEIGHT (FONT-BLINKER-HEIGHT FONT))
  (SETQ ARRAY (MAKE-SHEET-BIT-ARRAY SHEET WIDTH HEIGHT))
  (SETQ X-OFFSET
        (- (LET ((LKT (FONT-LEFT-KERN-TABLE FONT)))
             (IF LKT (AREF LKT CHARACTER) 0))))
  (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ALU-IOR ARRAY)
  (DRAW-CHAR FONT CHARACTER CHARACTER-X-OFFSET CHARACTER-Y-OFFSET ALU-ANDCA ARRAY))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :CHARACTER) () (VALUES CHARACTER FONT))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :SET-SIZE) (NEW-WIDTH NEW-HEIGHT)
  NEW-WIDTH NEW-HEIGHT
  ;; no can do
  NIL)


;;; kludgiferous stuff for dealing with the kludges involved in the font-map system.
;;; these functions will make a change in a font (eg by loading or fed) propagate to all the
;;; places that the font is in use.
;;; the reason why the changes don't propagate normally is that the font maps are consist of
;;; the actual font structures rather than symbols like 'fonts:cptfont.
(defun update-font (font)
  (if (typep font 'font)
      (font-evaluate (font-name font))
    font))

(defun ufm (window)
  (let* ((old-font-map (send window :font-map))
         (font-map-font-list (font-map-font-list old-font-map))
         (current-font (font-map-current-font old-font-map))
         tem)
    (setq font-map-font-list
          (mapcar #'update-font font-map-font-list))
    (unless current-font
      (setq current-font (send window :current-font)))
    (setq current-font (cond ((numberp current-font)
                              current-font)
                             ((memq current-font font-map-font-list)
                              current-font)
                             ((memq (font-name current-font) font-map-font-list)
                              (font-name current-font))
                             ((setq tem (position current-font old-font-map))
                              tem)
                             (t (update-font current-font))))
    (send window :set-font-map font-map-font-list)
    (send window :send-if-handles :set-default-font
                 (update-font (or (send window :send-if-handles :default-font)
                                  (send (sheet-screen window) :parse-font-specifier
                                                              :default))))
    (send window :set-current-font current-font t))
  (dolist (i (send window :inferiors))
    (ufm i)))


(defun update-font-maps ()
  "Update all the font maps in the window system.
This is a good thing to call after loading a loading a new version of an old font.
IT makes sure that all windows use the new copy."
  (dolist (screen all-the-screens)
    (let ((inferiors (send screen :inferiors))
          (font-alist (send screen :font-alist)))
      (dolist (tem font-alist)
        (set (cdr tem) (update-font (symbol-value (cdr tem)))))
      (ufm screen)
      (dolist (window inferiors)
        (ufm window)))))

(COMPILE-FLAVOR-METHODS RECTANGULAR-BLINKER CHARACTER-BLINKER IBEAM-BLINKER
                        BOX-BLINKER HOLLOW-RECTANGULAR-BLINKER
                        BITBLT-BLINKER MAGNIFYING-BLINKER REVERSE-CHARACTER-BLINKER)

;;;  This file is part of ZWEI.   -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file provides functions dealing with redisplay:
;;; MUST-REDISPLAY  - Tell a window that redisplay is needed.
;;; MUNG-LINE       - Tell redisplay that a line has changed.
;;; REDISPLAY       - Update the image of a window.

;;; Functions in this file will NOT touch any part of the window
;;; that is in the margins of the window;  the caller may do as he
;;; wishes with the margins.

(DEFUN MUST-REDISPLAY (WINDOW DEGREE &OPTIONAL LINE INDEX NO-NEED-FOR-OTHER-WINDOWS)
  "Tell WINDOW that it must do redisplay of degree DEGREE.
DEGREE is a number which you obtain as the value of one of these symbols:
 DIS-NONE  no redisplay needed.
 DIS-MARK-GOES  no redisplay needed except maybe region went away.
 DIS-BPS   point and mark may have moved.
 DIS-LINE  one line has changed.  LINE is the line,
                and INDEX is the index after which changes occurred.
 DIS-TEXT  any text may have changed.
 DIS-ALL   fonts, size, etc. may have changed.
What you specify here is max'd into some variables in the window
 and remembered until the next opportunity for redisplay.
NO-NEED-FOR-OTHER-WINDOWS says that there is no need to redisplay
 other windows displaying the same interval as WINDOW.
This function applies to all kinds of displayers, including non-sheet."
  (LET ((W-DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)))
    (COND ((= DEGREE DIS-LINE)
           (COND ((AND (WINDOW-OVERPRINTING-FLAG WINDOW)
                       (STRING-SEARCH-CHAR #/OVERSTRIKE LINE))
                  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) (MAX W-DEGREE DIS-TEXT)))
                 ((= W-DEGREE DIS-LINE)
                  (COND ((EQ (WINDOW-REDISPLAY-LINE WINDOW) LINE)
                         (SETF (WINDOW-REDISPLAY-INDEX WINDOW)
                               (MIN INDEX (WINDOW-REDISPLAY-INDEX WINDOW))))
                        (T
                          (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-TEXT))))
                 ((< W-DEGREE DIS-LINE)
                  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-LINE)
                  (SETF (WINDOW-REDISPLAY-LINE WINDOW) LINE)
                  (SETF (WINDOW-REDISPLAY-INDEX WINDOW) INDEX))))
          (T (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) (MAX W-DEGREE DEGREE)))))
  (OR NO-NEED-FOR-OTHER-WINDOWS
      (MUST-REDISPLAY-OTHER-WINDOWS (WINDOW-INTERVAL WINDOW) WINDOW DEGREE LINE INDEX)))

;;; Also redisplay all other windows than WINDOW which point to INTERVAL
(DEFUN MUST-REDISPLAY-OTHER-WINDOWS (INTERVAL WINDOW DEGREE &OPTIONAL LINE INDEX)
  (DOLIST (W (SEND INTERVAL :OTHER-WINDOWS WINDOW))
    (AND (NEQ W WINDOW)
         (MUST-REDISPLAY W DEGREE LINE INDEX T))))

(DEFUN REDISPLAY-ALL-WINDOWS (&OPTIONAL (FORCE-TO-COMPLETION-P NIL) (SELECT-P T))
  "Redisplay all exposed windows of this editor.
If FORCE-TO-COMPLETION-P is not set, we do stop if there is input available,
 and return T in that case.  Otherwise we return NIL.
Unless SELECT-P is NIL, we now actually select in the window system
 the window that is current in the editor."
  (COND ((AND (NOT FORCE-TO-COMPLETION-P)
              (SEND *STANDARD-INPUT* :LISTEN))  ;Suppress redisplay if typeahead
         T)
        (T
         (AND SELECT-P (SEND *WINDOW* :FINISH-DELAYED-SELECT))
         (DOLIST (WINDOW (WINDOW-LIST))
            (AND (WINDOW-READY-P WINDOW SELECT-P)
                 (REDISPLAY WINDOW)))
         (REDISPLAY-MODE-LINE)
         NIL)))

;;; Set the *CENTERING-FRACTION* based on the sign of X.
(DEFUN SET-CENTERING-FRACTION (X)
  (SETQ *CENTERING-FRACTION*
        (IF (MINUSP X) *MAX-RESET-FRACTION* *MIN-RESET-FRACTION*)))

(DEFUN RECENTER-WINDOW-RELATIVE (WINDOW NLINES &AUX START-BP)
  "Scroll WINDOW by NLINES."
  (AND (> (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-MARK-GOES)       ;If there is pending redisplay
       (NULL (PLINE-OF-POINT T WINDOW (WINDOW-POINT WINDOW)))   ;and point no longer valid
       (RECENTER-WINDOW WINDOW :ABSOLUTE))      ;First correct point and start of window
  (SETQ START-BP (COPY-BP (WINDOW-START-BP WINDOW)))
  (RECENTER-WINDOW WINDOW :RELATIVE NLINES)
  (WHEN (BP-= (WINDOW-START-BP WINDOW) (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
    (RECENTER-WINDOW WINDOW :START START-BP)
    (BARF)))

;;; This should ALWAYS leave point within the range of things that will be
;;; displayed according to the start-bp that we set up.
(DEFUN RECENTER-WINDOW (WINDOW RECENTER-TYPE &OPTIONAL RC1 RC2 &AUX TOP-LINE TOP-INDEX
                               POINT-PLINE POINT INTERVAL FIRST-BP LAST-BP DEGREE
                               N-PLINES START-BP)
  "Change the place where display starts in WINDOW.
RECENTER-TYPE is a keyword:
 :ABSOLUTE means that RC1 is a fraction saying where
  point should go in the window (or NIL, meaning use *CENTERING-FRACTION*);
 :RELATIVE means that point should be RC1 plines below where it is now;
 :START says that RC1 is a BP to start displaying at,
  or else RC1 is a line and RC2 is the index within it.
Recenter-types :NONE and :POINT that are allowed by REDISPLAY-WINDOW
 are not allowed here.
Works on sheet and non-sheet windows."
  (SETQ DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)
        N-PLINES (WINDOW-N-PLINES WINDOW)
        POINT (WINDOW-POINT WINDOW)
        INTERVAL (WINDOW-INTERVAL WINDOW)
        START-BP (WINDOW-START-BP WINDOW)
        FIRST-BP (INTERVAL-FIRST-BP INTERVAL)
        LAST-BP (INTERVAL-LAST-BP INTERVAL))
  (CASE RECENTER-TYPE
    (:ABSOLUTE
     (SETQ POINT-PLINE (FIX (* (OR RC1 *CENTERING-FRACTION*) N-PLINES))))
    (:START
      ;; The new start has been specified explicitly.
      (IF RC2
          (SETQ TOP-LINE RC1 TOP-INDEX RC2)
          (SETQ TOP-LINE (BP-LINE RC1) TOP-INDEX (BP-INDEX RC1)))
      (MOVE-BP START-BP TOP-LINE TOP-INDEX)
      (SETF (WINDOW-REDISPLAY-DEGREE WINDOW)
            (SETQ DEGREE (MAX DEGREE DIS-TEXT)))
      (LET ((P (PLINE-OF-POINT T WINDOW POINT)))
        (COND ((NULL P)
               (MOVE-BP POINT TOP-LINE TOP-INDEX)
               (SETQ POINT-PLINE 0)))))
    (:RELATIVE
      ;; Move POINT by RC1 plines.
      (COND ((AND ( DEGREE DIS-BPS)
                  ( RC1 0)
                  (< RC1 N-PLINES)
                  (PLINE-LINE WINDOW RC1))
             ;; What luck! No text has changed, and the goal PLINE is in the window.
             (SETQ TOP-LINE (PLINE-LINE WINDOW RC1)
                   TOP-INDEX (PLINE-FROM-INDEX WINDOW RC1))
             (COND ((< (WINDOW-LAST-POINT-PLINE WINDOW) RC1)
                    (MOVE-BP POINT TOP-LINE TOP-INDEX)
                    (SETQ POINT-PLINE 0))))
            (T
             (SETQ TOP-INDEX (WINDOW-START-BP WINDOW))
             (SETQ TOP-LINE (BP-LINE TOP-INDEX) TOP-INDEX (BP-INDEX TOP-INDEX))
             (MOVE-BP START-BP TOP-LINE TOP-INDEX)
             (LET ((P (PLINE-OF-POINT NIL WINDOW POINT)))
               (SETQ POINT-PLINE (- P RC1)))))))
  (WHEN POINT-PLINE
    (MULTIPLE-VALUE-SETQ (TOP-LINE TOP-INDEX POINT-PLINE)
      (PUT-POINT-AT-PLINE WINDOW (BP-LINE POINT) (BP-INDEX POINT) POINT-PLINE
                          FIRST-BP LAST-BP))
    ;; If recentering pushes point out the top or bottom, pull it back
    ;; just far enough to be back inside.  Also update POINT-PLINE for how point moves.
    (COND ((null point-pline))          ;defensive code, should never be true.
          ((MINUSP POINT-PLINE)
           (SETQ POINT-PLINE 0)
           (MOVE-BP POINT TOP-LINE TOP-INDEX))
          (( POINT-PLINE N-PLINES)
           (SETQ POINT-PLINE (1- N-PLINES))
           (MULTIPLE-VALUE-BIND (POINT-LINE POINT-INDEX NIL)
               (PUT-POINT-AT-PLINE WINDOW TOP-LINE TOP-INDEX (- POINT-PLINE)
                                   FIRST-BP LAST-BP)
             (MOVE-BP POINT POINT-LINE POINT-INDEX))))
    (SETF (WINDOW-LAST-POINT-PLINE WINDOW) POINT-PLINE))
  (if top-line (MOVE-BP START-BP TOP-LINE TOP-INDEX))   ;defensive code...
  (SEND WINDOW :NEW-SCROLL-POSITION)
  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) (MAX DEGREE DIS-TEXT)))

;;; The elements of a window PLINE are the:
;;; PLINE-LINE                  ;Editor line displayed, NIL if blank
;;; PLINE-FROM-INDEX            ;First character displayed
;;; PLINE-TO-INDEX              ;Last character displayed+1
;;; PLINE-TICK                  ;TICK as of last time pline updated on display
;;; PLINE-MARKING-LEFT          ;NIL no marking, or X coord of start of region-marking
;;; PLINE-MARKING-WIDTH         ;Horizontal extent of marking
;;; PLINE-TEXT-WIDTH            ;Horizontal extent of text
;;; Note that for non-continuation lines, PLINE-TEXT-WIDTH includes a little
;;; extra for the pseudo-space at the end of the line which corresponds to the #\CR.
;;; But for continuation lines, it does not include the ! at the end of the line.
;;; (It does now, but that should be regarded as a bug in SHEET-LINE-OUT)
;;; PLINE-TEXT-WIDTH is used only for region marking.

(DEFUN REDISPLAY (WINDOW  &OPTIONAL (RECENTER-TYPE :POINT)
                  RC1 RC2 (FORCE-TO-COMPLETION-P NIL))
  "Redisplay WINDOW (a displayer).
RECENTER-TYPE says how to recompute where to display from.
 :ABSOLUTE means that RC1 is a fraction saying where
  point should go in the window (or NIL, meaning use *CENTERING-FRACTION*);
 :RELATIVE means that point should be RC1 plines below where it is now;
 :START says that RC1 is a BP to start displaying at,
  or else RC1 is a line and RC2 is the index within it.
 :NONE means do not recenter the window even if point is off screen.
 :POINT means recenter only if point is off screen,
  and if so, put point RC1 fraction of the way down the screen.
  (If RC1 is NIL, use *CENTERING-FRACTION*)
Redisplay is done according to the degree that WINDOW remembers it needs,
and stops when input is found to be available."
  (IF (GETF (NODE-PROPERTY-LIST (WINDOW-INTERVAL WINDOW)) ':KILLED)
      (SEND (WINDOW-INTERVAL WINDOW) :FIX-WINDOW-INTERVAL WINDOW))
  (SEND WINDOW :REDISPLAY RECENTER-TYPE RC1 RC2 FORCE-TO-COMPLETION-P))

(DEFUN REDISPLAY-MODE-LINE ()
  "Update the display of the mode line window."
  (SEND *MODE-LINE-WINDOW* :REDISPLAY *MODE-LINE-LIST*))

(DEFUN REDISPLAY-POINT-ON-PLINE (BP WINDOW PLINE &OPTIONAL (NOT-IF-DISPLAYED-P T))
  "Redisplay WINDOW so that BP appears at PLINE.
NOT-IF-DISPLAYED-P means do nothing if BP is already on-screen."
  (OR (AND (FIND-BP-IN-WINDOW WINDOW BP) NOT-IF-DISPLAYED-P)
      (MULTIPLE-VALUE-BIND (LINE INDEX)
          (PUT-POINT-AT-PLINE WINDOW (BP-LINE BP) (BP-INDEX BP) PLINE
                              (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))
                              (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
        (RECENTER-WINDOW WINDOW :START LINE INDEX))))

(DEFUN PLINE-OF-POINT (IN-BOUNDS-P WINDOW POINT &AUX (START-BP (WINDOW-START-BP WINDOW)))
  "Return the pline at which POINT would appear in WINDOW, given how WINDOW is centered now.
IN-BOUNDS-P means return NIL if POINT would be out of bounds;
otherwise return a number that is correct but out of bounds itself.
POINT can be a BP or just a line.
Works on sheet and non-sheet windows."
  (LET (POINT-LINE POINT-INDEX
       (TOP-LINE (BP-LINE START-BP))
       (TOP-INDEX (BP-INDEX START-BP))
       POINT-PLINE)
    (COND ((CONSP POINT)
           (SETQ POINT-LINE (BP-LINE POINT)
                 POINT-INDEX (BP-INDEX POINT)))
          (T (SETQ POINT-LINE POINT POINT-INDEX 0)))
    (COND ((AND ( (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-BPS)
                ;; This clause is a short-cut, to avoid SHEET-COMPUTE-MOTION.
                ;; No text has changed.  If we can find POINT in the old
                ;; state of the window, then that is its current position.
                ;; Otherwise: if IN-BOUNDS-P, return NIL, else try slow way.
                (OR
                 (SETQ POINT-PLINE (FIND-BP-IN-WINDOW WINDOW POINT-LINE POINT-INDEX))
                 IN-BOUNDS-P))
           POINT-PLINE)
          ;; Some text has changed, the existing WINDOW state is useless.
          ;; Assume we were to redisplay with the same TOP-LINE and TOP-INDEX
          ;; and figure out where that would put POINT.
          ((AND (COND (IN-BOUNDS-P
                       (DO ((LINE TOP-LINE (LINE-NEXT LINE))
                            (END-LINE (BP-LINE (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW))))
                            (N-PLINES (WINDOW-N-PLINES WINDOW))
                            (I 0 (1+ I)))
                           (( I N-PLINES) NIL)
                         (AND (EQ LINE POINT-LINE) (RETURN T))
                         (AND (EQ LINE END-LINE) (RETURN NIL))))
                      (T (SEARCH-FOR-LINE POINT-LINE TOP-LINE)))
                (OR (NEQ POINT-LINE TOP-LINE)
                    ( POINT-INDEX TOP-INDEX)))
           ;; POINT is past the top-line, top-index position.
           (SETQ POINT-PLINE (NTH-VALUE 2 (PUT-POINT-AT-PLINE WINDOW POINT-LINE POINT-INDEX
                                                              (IF IN-BOUNDS-P
                                                                  (1+ (WINDOW-N-PLINES WINDOW))
                                                                MOST-POSITIVE-FIXNUM)
                                                              START-BP NIL)))
           (AND (NOT (AND IN-BOUNDS-P ( POINT-PLINE (WINDOW-N-PLINES WINDOW))))
                POINT-PLINE))
           ;; The POINT-LINE is behind TOP-LINE.
          (IN-BOUNDS-P NIL)
          (T
           ;; It's above the top and we really want to know exactly where.
           ;; Amazingly, we can just ask to display point infinitely far before
           ;; the place which is the top, and see where it would manage to appear!
           (NTH-VALUE 2 (PUT-POINT-AT-PLINE WINDOW POINT-LINE POINT-INDEX
                                            most-negative-fixnum
                                            NIL START-BP))))))

(DEFUN FIND-BP-IN-WINDOW (WINDOW BP-LINE &OPTIONAL BP-INDEX)
  "Return the pline in WINDOW at or after which BP was last displayed.
Either BP-LINE is a BP, or else BP-LINE is a line and BP-INDEX is an index.
This assumes that WINDOW's redisplay records are up to date."
  (IF (NULL BP-INDEX) (SETQ BP-INDEX (BP-INDEX BP-LINE) BP-LINE (BP-LINE BP-LINE)))
  (LET ((N-PLINES (WINDOW-N-PLINES WINDOW))
        (HINT (WINDOW-LAST-POINT-PLINE WINDOW)))
    (IF (AND (EQ BP-LINE (PLINE-LINE WINDOW HINT))
             ( (PLINE-FROM-INDEX WINDOW HINT) BP-INDEX)
             (< BP-INDEX (PLINE-TO-INDEX WINDOW HINT)))
        ;; The hint from last time payed off!
        HINT
      ;; The hint didn't do it, search for the pline.
      (DO ((PLINE 0 (1+ PLINE)))
          (( PLINE N-PLINES)
           NIL)
        (WHEN (AND (EQ BP-LINE (PLINE-LINE WINDOW PLINE))
                   ( (PLINE-FROM-INDEX WINDOW PLINE) BP-INDEX)
                   (< BP-INDEX (PLINE-TO-INDEX WINDOW PLINE)))
          (RETURN PLINE))))))

;;; This is an internal function of REDISPLAY.
(DEFUN PUT-POINT-AT-PLINE (WINDOW POINT-LINE POINT-INDEX POINT-PLINE FIRST-BP LAST-BP)
  "Figure out where to start redisplay so that POINT ends up on or near POINT-PLINE.
Specify POINT-LINE as a BP, or else POINT-LINE and POINT-INDEX as line and index.
Returns a LINE and an INDEX indicating where to start redisplay,
 and the third value is the value of POINT-PLINE that would result
 (since it is not always possible to attain the exact value desired)."
  (SEND WINDOW :PUT-POINT-AT-PLINE
               (IF (CONSP POINT-LINE) (CAR POINT-LINE) POINT-LINE)
               (IF (CONSP POINT-LINE) (CADR POINT-LINE) POINT-INDEX)
               POINT-PLINE FIRST-BP LAST-BP))

(DEFUN STRING-WIDTH (STRING &OPTIONAL (FROM 0) (TO (STRING-LENGTH STRING)) (WINDOW *WINDOW*)
                     CONTINUATION STOP-X)
  "Return width in pixels of a substring of STRING, as displayed on WINDOW.
STOP-X is hpos to stop scanning at; then second value is stopping index in STRING.
CONTINUATION means to take account of continuation lines.
WINDOW defaults to *WINDOW*."
  (COND ((NOT (TYPEP WINDOW 'WINDOW))
         (SEND WINDOW :EDITOR-STRING-LENGTH STRING FROM TO CONTINUATION STOP-X))
        ;; This is just to bum the time it takes to send the message.
        ;; Perhaps for system 95 this can be optimized in another way.
        (CONTINUATION
         (MULTIPLE-VALUE-BIND (END-X NIL END-INDEX)
             (TV:SHEET-COMPUTE-MOTION WINDOW 0 0 STRING FROM TO NIL
                                      (OR STOP-X 0) (AND STOP-X 0)      ;STOP-X, STOP-Y
                                      NIL NIL NIL
                                      (TV:SHEET-LINE-HEIGHT WINDOW)
                                      (IF *TAB-WIDTH*
                                          (* *TAB-WIDTH* (TV:SHEET-CHAR-WIDTH WINDOW))
                                        (TV:SHEET-TAB-WIDTH WINDOW)))
           (VALUES END-X END-INDEX)))
        (T
         (TV:SHEET-STRING-LENGTH WINDOW STRING FROM TO
                                 STOP-X NIL 0
                                 (IF *TAB-WIDTH*
                                     (* *TAB-WIDTH* (TV:SHEET-CHAR-WIDTH WINDOW))
                                   (TV:SHEET-TAB-WIDTH WINDOW))))))

(DEFUN WINDOW-CHAR-WIDTH (WINDOW &OPTIONAL CHARACTER)
  "Return width of CHARACTER in WINDOW."
; character lossage
  (if (characterp character) (setq character (char-int character)))
  (IF (AND CHARACTER (EQL CHARACTER (CHAR-INT #/TAB)) *TAB-WIDTH*)
      (SEND WINDOW :EDITOR-TAB-WIDTH)
      (SEND WINDOW :CHARACTER-WIDTH CHARACTER)))

;;;; Most of the following stuff is just for sheet windows.

(DEFMETHOD (WINDOW :EDITOR-STRING-LENGTH) (STRING START END CONTINUATION
                                           &OPTIONAL STOP-X)
  (IF CONTINUATION
      (MULTIPLE-VALUE-BIND (END-X NIL END-INDEX)
          (TV:SHEET-COMPUTE-MOTION SELF 0 0 STRING START END NIL
                                   (OR STOP-X 0) (AND STOP-X 0) ;STOP-X, STOP-Y
                                   NIL NIL NIL
                                   (TV:SHEET-LINE-HEIGHT SELF)
                                   (IF *TAB-WIDTH*
                                       (* *TAB-WIDTH* (TV:SHEET-CHAR-WIDTH SELF))
                                     (TV:SHEET-TAB-WIDTH SELF)))
        (VALUES END-X END-INDEX))
      (TV:SHEET-STRING-LENGTH SELF STRING START END
                              STOP-X NIL 0
                              (IF *TAB-WIDTH*
                                  (* *TAB-WIDTH* (TV:SHEET-CHAR-WIDTH SELF))
                                (TV:SHEET-TAB-WIDTH SELF)))))

(DEFMETHOD (WINDOW :EDITOR-TAB-WIDTH) ()
  (* (OR *TAB-WIDTH* (TV:SHEET-TAB-NCHARS SELF))
     (TV:SHEET-CHAR-WIDTH SELF)))

;; After doing redisplay within a line, remember the cursor pos at the end of that line
;; so that if the next redisplay is just adding a character there, it runs faster
;; (makes a difference in multi-font buffers).
(DEFVAR *LAST-REDISPLAY-SHEET* NIL
  "The sheet for which the cached ZWEI redisplay cursor x is remembered.")
(DEFVAR *LAST-REDISPLAY-LINE* NIL
  "The line for which the cached ZWEI redisplay cursor x is remembered.")
(DEFVAR *LAST-REDISPLAY-INDEX* NIL
  "The index within the line for which the cached ZWEI redisplay cursor x is remembered.")
(DEFVAR *LAST-REDISPLAY-CURSOR-X* :UNBOUND
  "The cursor-x remembered for ZWEI redisplay to start at.")

(defmethod (displayer :redisplay) (recenter-type rc1 rc2 force-to-completion-p)
  (prepare-window-for-redisplay self)
  (block redisplay
    (let ((lh (send self :line-height))
          (now (tick))
          point-pline
          (point-line (bp-line point))
          (point-index (bp-index point))
          (top-line (bp-line start-bp))
          (top-index (bp-index start-bp))
          (initial-degree redisplay-degree)
          ;; Bind *INTERVAL* in case we decide to call any primitives, e.g. inside the
          ;; special-blinker which blinks matching parens.  This is an implicit argument.
          (*interval* interval)
          ;; Temporary.  Move these three instance variables from flavor WINDOW to flavor DISPLAYER
          ;; and then these three bindings will not be needed.
          (pline-marking-left-array (send self :pline-marking-left-array))
;         (pline-marking-width-array (send self :pline-marking-width-array))  ; These two not needed in this function.
;         (pline-text-width-array (send self :pline-text-width-array))
          )
      (declare (special point-pline))
      ;; We prefer not to start redisplay in the middle of a line.
      ;; The start-bp of the window may have ended up there via a command like rubout at
      ;; the beginning of the window or may have been scrolled there explicitly.  If the
      ;; top line has changed so that continuation may not be necessary any more, attempt
      ;; recentering.
      (and (eq recenter-type :point)
           (not (zerop top-index))
           (> n-plines 1)
           (> (line-tick top-line) (or (pline-tick self 0) 0))
           (let ((new-top-index (if (eq top-line (bp-line (interval-first-bp interval)))
                                    (bp-index (interval-first-bp interval))
                                  0)))
             (and ( top-index new-top-index)
                  (< (nth-value 1
                       (tv:sheet-compute-motion self 0 0 top-line new-top-index point-index
                             nil 0 most-positive-fixnum most-positive-fixnum))
                     (* lh n-plines))
                  (setq recenter-type :absolute))))

      ;; :POINT recentering is a conditional sort of :ABSOLUTE recentering.
      ;; So decide here whether :ABSOLUTE recentering should be done.
      (when (eq recenter-type :point)
        (cond (( redisplay-degree dis-mark-goes))
              ((and (bp-= point (interval-last-bp *interval*))
                    (bp-= point start-bp)
                    (not (bp-= point (interval-first-bp *interval*))))
               ;; Don't let display be empty at end of buffer.
               (setq recenter-type :absolute))
              ;; When typing at the end of the line, dont try to compute POINT-PLINE yet,
              ;; but wait till after we have faked out the pline-text-width correctly.
              ;; Otherwise it will be much, much slower
              ((and (= redisplay-degree dis-line)
                    (eq redisplay-line point-line)
                    ;; lines with displayers don't hack dis-line
                    (if (getf (line-plist point-line) 'displayer)
                        (progn (setq redisplay-degree dis-text) nil) t)
                    (neq point-line (pline-line self (1- n-plines)))
                    (or ( (1+ redisplay-index) point-index)
                        (zerop (nth-value 1
                                 (tv:sheet-compute-motion self 0 0 point-line 0 point-index t))))))
              ((setq point-pline (pline-of-point t self point)))
              (t
               (setq recenter-type :absolute))))

      ;; If recentering is needed, do it, and see what changes it made.
      (unless (memq recenter-type '(:none :point))
        (recenter-window self recenter-type rc1 rc2)
        (setq top-line (bp-line start-bp)
              top-index (bp-index start-bp)
              point-line (bp-line point)
              point-index (bp-index point))
        ;; Gobble point-pline as computed by recenter-window
        ;; if it is accurate.
        (setq point-pline last-point-pline)
        (or (and (eq point-line (pline-line self point-pline))
                 ( (pline-from-index self point-pline) point-index)
                 (< point-index (pline-to-index self point-pline)))
            (setq point-pline nil)))
      ;; Now we have TOP-LINE and TOP-INDEX, and possibly POINT-PLINE.

      ;; First, handle the case where just one line needs to be updated.
      (when (= redisplay-degree dis-line)
        (window-redisplay-dis-line now))
      ;; If all the window should be redisplayed, mark each pline as unknown.
      (when ( redisplay-degree dis-all)
        (tv:sheet-clear self t)
        (send self :refresh-margins)
        (do ((i 0 (1+ i)))
            ((= i n-plines))
          (setf (pline-tick self i) -1)
          (setf (pline-marking-left self i) nil)))
      (when ( redisplay-degree dis-text)
        (if (eq (window-redisplay-dis-text now
                  top-line top-index force-to-completion-p)
                :abort)
            (return-from redisplay nil)))
      (when ( redisplay-degree dis-bps)
        (if (eq (window-redisplay-dis-bps now recenter-type initial-degree)
                :retry)
            (return-from redisplay
              (send self :redisplay recenter-type rc1 rc2 force-to-completion-p))))
      (when ( redisplay-degree dis-mark-goes)
        ;; The region marking may have changed.
        (send self :update-region-marking))
      ;;The character under the mouse also
      (when ( redisplay-degree dis-bps) (mouse-rethink self))
      (when ( redisplay-degree dis-text)
        (send self :new-scroll-position))
      (setf redisplay-degree dis-none))))

(defun window-redisplay-dis-line (now)
  (declare (:self-flavor displayer)
           (special point-pline))
  (macrolet ((punt-if (cond) `(when ,cond
                                (setq redisplay-degree dis-text point-pline nil)
                                (return-from window-redisplay-dis-line nil))))
    (let* ((lh (send self :line-height))
           (last-bp (interval-last-bp interval))
           (line redisplay-line)
           (index redisplay-index)
           (p (find-bp-in-window self line index))
           ;; Temporary.  Move these three instance variables from flavor WINDOW to flavor DISPLAYER
           ;; and then these three bindings will not be needed.
           (pline-marking-left-array (send self :pline-marking-left-array))
           (pline-marking-width-array (send self :pline-marking-width-array))
           (pline-text-width-array (send self :pline-text-width-array)))
      (punt-if (null p))
      (let* ((line-length (if (eq line (bp-line last-bp))
                              (bp-index last-bp)
                            (line-length line)))
             ;; LEN gets the raster position in the pline P
             ;; of the character in LINE at position INDEX.
             (len (without-interrupts
                    (if (and (eq (pline-line self p) *last-redisplay-line*)
                             (= index *last-redisplay-index*)
                             (eq self *last-redisplay-sheet*))
                        *last-redisplay-cursor-x*
                        (string-width line (pline-from-index self p) index self))))
             dwid)
        ;; If P and LEN say we are at the start of a continuation line,
        ;; then maybe they are wrong
        ;; (if the contin line has been exactly deleted).
        (punt-if (and (zerop len) (not (zerop index))))
        ;; Reverse-video region marking must be removed before updating.
        (when (and (eq *region-marking-mode* ':reverse-video)
                   (or (pline-marking-left self p)
                       (pline-marking-left self (min (1+ p) (1- n-plines)))))
          (region-unmark-pline p))
        ;; Go to the place in the line where changes start. Clear from there.
        ;; This means that any region marking from there on is gone now.
        (cond ((and (pline-marking-left self p)
                    (< (pline-marking-left self p) len))
               (setf (pline-marking-width self p)
                     (min (- len (pline-marking-left self p))
                          (pline-marking-width self p))))
              (t (setf (pline-marking-left self p) nil)
                 (setf (pline-marking-width self p) nil)))
        ;; If the character is wider than it claims to be, draw an extra
        ;; character, since the clear-eol will erase data.
        (unless (zerop index)
          (let ((ch (aref line (1- index))))
            (when (< (char-code ch) #o200)
              (let ((fmap (send self :font-map)))
                (when fmap
                  (let* ((font (aref fmap (char-font ch)))
                         (cwt (font-char-width-table font)))
                    (when cwt
                      (let ((cwid (aref cwt (setq ch (char-code ch))))
                            (rwid (fed:font-char-min-raster-width font ch)))
                        (when (> rwid cwid)
                          (setq dwid cwid))))))))))
        (multiple-value-bind (i tw)
            ;; Neither displayers nor diagrams can get here.
            (tv:sheet-line-out self line index line-length len (* lh p) dwid)
          ;; Save cursor x to avoid calls to STRING-WIDTH while inserting text.
          (without-interrupts ; Don't confuse other zmacs processes
            (setq *last-redisplay-sheet* self
                  *last-redisplay-line* line
                  *last-redisplay-index* line-length
                  *last-redisplay-cursor-x* tw))
          ;; We have output the first PLINE of this line
          (setf (pline-to-index self p) i)
          (setf (pline-text-width self p)
                (if ( i line-length) tw        ;Continuation needed
                  (+ tw (send self :char-width))))      ;Allow for CR
          (setf (pline-tick self p) now)
          ;; See if plines below this need to be redisplayed, due
          ;; to line-continuation issues
          (when (and (< (1+ p) n-plines)
                     (or ( i line-length)
                         ( tw (send self :inside-width))
                         (eq (pline-line self (1+ p)) line)))
            (setq redisplay-degree dis-text point-pline nil)
            ;; If we are just creating a new continuation line, make it
            ;; still look munged, so REDISPLAY-BLT can understand.
            (or (eq (pline-line self (1+ p)) line)
                (setf (pline-tick self p) -1)))))))
  nil)

(defun window-redisplay-dis-text (now top-line top-index
                                  force-to-completion-p)
  (declare (:self-flavor displayer)
           (special point-pline))
  (macrolet ((abort-redisplay () `(return-from window-redisplay-dis-text :abort)))
    (let (
          ;; Temporary.  Move these three instance variables from flavor WINDOW to flavor DISPLAYER
          ;; and then these three bindings will not be needed.
          (pline-marking-left-array (send self :pline-marking-left-array))
;         (pline-marking-width-array (send self :pline-marking-width-array))  ; This one isn't used here
          (pline-text-width-array (send self :pline-text-width-array)))
      (setq *last-redisplay-line* nil)
      ;; In case we abort before we are done, don't forget what's needed.
      (setf redisplay-degree dis-text)
      (setf last-bp-displayed-p nil)
      (do ((l (send self :special-blinker-list) (cdr l)))
          ((null l))
        (send (cdar l) :set-visibility nil))
      ;; Abort now if input available
      (and (not force-to-completion-p)
           (send *standard-input* :listen)
           (abort-redisplay))
      ;; Attempt to do insert and delete line cleverness.
      (send self :redisplay-blt)
      ;; This might have invalidated the value of POINT-PLINE.
      ;; It won't be hard to recompute, so do so.
      (setq point-pline nil)
      ;; First loop over actual lines.
      (block lines
        (let* ((last-bp (interval-last-bp interval))
               (stop-line (bp-line last-bp))
               (lh (send self :line-height))
               (max-n-plines (send self :max-n-plines)))
          (do ((line top-line (line-next line))
               (from-index top-index 0)
               to-index
               (pline 0))
              ((null line))                     ;should not get here, but be defensive...
            ;; Between lines, check for input available and abort if so.
            (and (not force-to-completion-p)
                 (zerop (cl:rem pline 30.))
                 (send *standard-input* :listen)
                 (abort-redisplay))
            (setq to-index (if (eq line stop-line)
                               (bp-index last-bp)
                             (line-length line)))
            ;; Now loop over the plines of this line.
            (loop
              do (when ( pline max-n-plines)
                   (return-from lines))
              (when ( pline n-plines)
                ;; Note cannot get here if self is a zwei:window, but can for other flavors.
                (send self :new-pline pline))
              ;; Check for a line that has not been changed.
              (if (and (eq line (pline-line self pline))
                       (> (pline-tick self pline) (line-tick line))
                       (= (pline-from-index self pline) from-index))
                  (setq from-index (pline-to-index self pline))
                ;; Reverse-video region marking must be removed before updating.
                (and (eq *region-marking-mode* ':reverse-video)
                     (or (pline-marking-left self pline)
                         (pline-marking-left self (min (1+ pline) (1- n-plines))))
                     (region-unmark-pline pline))
                (multiple-value-bind (i tw)
                    (let ((displayer (getf (line-plist line) 'displayer)))
                      (cond (displayer
                             (tv:sheet-set-cursorpos self 0 (* lh pline))
                             (tv:sheet-clear-eol self)
                             (send displayer :display self line from-index))
                            ((setq displayer (getf (line-plist line) ':diagram))
                             (tv:sheet-set-cursorpos self 0 (* lh pline))
                             (tv:sheet-clear-eol self)
                             (send displayer :draw line self)
                             (values 1 0))
                            (t
                             (tv:sheet-line-out self line
                                                from-index to-index
                                                0 (* lh pline)))))
                  (setf (pline-line self pline) line)
                  (setf (pline-from-index self pline) from-index)
                  (setf (pline-to-index self pline) i)
                  (setf (pline-tick self pline) now)
                  (setf (pline-marking-left self pline) nil)
                  (setf (pline-text-width self pline)
                        (if ( i (line-length line)) tw ;Continuation needed
                          (+ tw (send self :char-width))))      ;Allow for CR
                  (setq from-index i)))
              (setq pline (1+ pline))
              ;; This is >, not , because if line isn't cont'd then PLINE-TO-PLINE
              ;; counts the phony CR which is output by SHEET-LINE-OUT.
              until (> from-index to-index))
            ;; Check for the last line in the interval.
            (when (eq line stop-line)
              (setf last-bp-displayed-p t)
              (when ( pline n-plines)
                (return-from lines))
              (unless (and (null (pline-line self pline))       ;Return if screen already blanked
                           (pline-tick self pline)
                           (> (pline-tick self pline) 0))
                ;; Reverse-video region marking must be removed before updating.
                (and (eq *region-marking-mode* ':reverse-video)

                     (or (pline-marking-left self pline)
                         (pline-marking-left self
                                             (min (1+ pline) (1- n-plines))))
                     (region-unmark-pline pline))
                ;; Clean out the rest of the window beneath it.  Then exit.
                (tv:sheet-set-cursorpos self 0 (* lh pline))
                (tv:sheet-clear-eof self)
                (do ((pline pline (1+ pline)))
                    (( pline n-plines))
                  (setf (pline-line self pline) nil)
                  (setf (pline-tick self pline) now)
                  (setf (pline-marking-left self pline) nil)))
              (return-from lines)))))))
  nil)

(defun window-redisplay-dis-bps (ignore recenter-type initial-degree)
  (declare (:self-flavor displayer)
           (special point-pline))
  (macrolet ((retry-redisplay () `(return-from window-redisplay-dis-bps :retry)))
    (let ((point-line (bp-line point))
          (point-index (bp-index point))
          ;; These are just for debugging the errors reported below.
          (point-node (bp-top-level-node point))
          (start-bp-node (bp-top-level-node start-bp))
          (buf interval))
      (declare (ignore point-node start-bp-node buf))
      ;; BPs have moved.  Reposition the POINT blinker.
      (or point-pline
          (setq point-pline (find-bp-in-window self point-line point-index))
          (eq recenter-type ':none)
          (if (and (= initial-degree dis-line)
                   (= redisplay-degree dis-text))
              ;;Somewhat anomalous case, try again with greater redisplay degree
              (retry-redisplay)
            (unwind-protect
                (ferror "Recenter type ~S left point outside the window."
                        recenter-type)
              ;; Try to clean things up so error won't repeat.
              (move-bp point (interval-first-bp interval))
              (move-bp start-bp (interval-first-bp interval)))))
      (if (null point-pline)
          ;; POINT is not on the window, so make it go away.
          (send (send self :point-blinker) :set-visibility nil)
        ;; POINT is on the window, find its Y position.
        (send (send self :point-blinker) :set-visibility
              (if (eq self tv:selected-window)
                  ':blink
                  (send (send self :point-blinker) :deselected-visibility)))
        (unless (eq point-line (pline-line self point-pline))
          (dprint point-line point-pline (pline-line self point-pline))
          (unwind-protect
              (ferror "Position of POINT on window is screwed up.")
            ;; Try to clean things up so error won't repeat.
            (move-bp point (interval-first-bp interval))
            (move-bp start-bp (interval-first-bp interval))))
        (let ((from-index (pline-from-index self point-pline))
              (displayer (getf (line-plist point-line) 'displayer)))    ;was point-pline??
          (send self :set-blinker-size point (send self :point-blinker)
                (if displayer
                    (send displayer :compute-xpos self point-line from-index point-index)
                  (send self :compute-motion point-line from-index point-index 0 0))
                (* (send self :line-height) point-pline)))
        (setf last-point-pline point-pline))
      ;; Blink the parens, etc.
      (dolist (bl (send self :special-blinker-list))
        (funcall (car bl) (cdr bl) self point start-bp))))
  nil)

;; this used to be the largest fef in the system.
;; Decimated callously by Mly 29-Aug-85
;(DEFMETHOD (WINDOW :REDISPLAY) (RECENTER-TYPE RC1 RC2 FORCE-TO-COMPLETION-P)
;  (PREPARE-WINDOW-FOR-REDISPLAY SELF)
;  (PROG ABORT-REDISPLAY
;       ((LH TV:LINE-HEIGHT)
;        (NOW (TICK))
;        POINT-PLINE
;        (POINT-LINE (BP-LINE POINT))
;        (POINT-INDEX (BP-INDEX POINT))
;        (TOP-LINE (BP-LINE START-BP))
;        (TOP-INDEX (BP-INDEX START-BP))
;        (LAST-BP (INTERVAL-LAST-BP INTERVAL))
;        (INITIAL-DEGREE REDISPLAY-DEGREE)
;        ;; Bind *INTERVAL* in case we decide to call any primitives, e.g. inside the
;        ;; special-blinker which blinks matching parens.  This is an implicit argument.
;        (*INTERVAL* INTERVAL)
;        ;; These are for debugging only.
;        POINT-NODE START-BP-NODE BUF)
;    ;; We prefer not to start redisplay in the middle of a line.
;    ;; The start-bp of the window may have ended up there via a command like rubout at
;    ;; the beginning of the window or may have been scrolled there explicitly.  If the
;    ;; top line has changed so that continuation may not be necessary any more, attempt
;    ;; recentering.
;    (AND (EQ RECENTER-TYPE ':POINT)
;        (NOT (ZEROP TOP-INDEX))
;        (> N-PLINES 1)
;        (> (LINE-TICK TOP-LINE) (OR (PLINE-TICK SELF 0) 0))
;        (LET ((NEW-TOP-INDEX (IF (EQ TOP-LINE (BP-LINE (INTERVAL-FIRST-BP INTERVAL)))
;                                 (BP-INDEX (INTERVAL-FIRST-BP INTERVAL))
;                                 0)))
;          (AND ( TOP-INDEX NEW-TOP-INDEX)
;               (< (MULTIPLE-VALUE-BIND (NIL Y)
;                      (TV:SHEET-COMPUTE-MOTION SELF 0 0 TOP-LINE NEW-TOP-INDEX POINT-INDEX
;                                               NIL 0 most-positive-fixnum most-positive-fixnum)
;                    Y)
;                  (* LH N-PLINES))
;               (SETQ RECENTER-TYPE ':ABSOLUTE))))
;    ;; :POINT recentering is a conditional sort of :ABSOLUTE recentering.
;    ;; So decide here whether :ABSOLUTE recentering should be done.
;    (AND (EQ RECENTER-TYPE ':POINT)
;        (COND (( REDISPLAY-DEGREE DIS-MARK-GOES))
;              ((AND (BP-= POINT (INTERVAL-LAST-BP *INTERVAL*))
;                    (BP-= POINT START-BP)
;                    (NOT (BP-= POINT (INTERVAL-FIRST-BP *INTERVAL*))))
;               ;; Don't let display be empty at end of buffer.
;               (SETQ RECENTER-TYPE ':ABSOLUTE))
;              ;; When typing at the end of the line, dont try to compute POINT-PLINE yet,
;              ;; but wait till after we have faked out the pline-text-width correctly.
;              ;; Otherwise it will be much, much slower
;              ((AND (= REDISPLAY-DEGREE DIS-LINE)
;                    (EQ REDISPLAY-LINE POINT-LINE)
;                    (NEQ POINT-LINE (PLINE-LINE SELF (1- N-PLINES)))
;                    (OR ( (1+ REDISPLAY-INDEX) POINT-INDEX)
;                        (= (MULTIPLE-VALUE-BIND (NIL Y)
;                               (TV:SHEET-COMPUTE-MOTION SELF 0 0 POINT-LINE 0 POINT-INDEX
;                                                        T)
;                             Y)
;                           0))))
;              ((SETQ POINT-PLINE (PLINE-OF-POINT T SELF POINT)))
;              (T (SETQ RECENTER-TYPE ':ABSOLUTE))))
;    ;; If recentering is needed, do it, and see what changes it made.
;    (COND ((MEMQ RECENTER-TYPE '(:NONE :POINT)))
;         (T (RECENTER-WINDOW SELF RECENTER-TYPE RC1 RC2)
;            (SETQ TOP-LINE (BP-LINE START-BP)
;                  TOP-INDEX (BP-INDEX START-BP)
;                  POINT-LINE (BP-LINE POINT)
;                  POINT-INDEX (BP-INDEX POINT))
;            ;; Gobble point-pline as computed by recenter-window
;            ;; if it is accurate.
;            (SETQ POINT-PLINE LAST-POINT-PLINE)
;            (OR (AND (EQ POINT-LINE (PLINE-LINE SELF POINT-PLINE))
;                     ( (PLINE-FROM-INDEX SELF POINT-PLINE) POINT-INDEX)
;                     (< POINT-INDEX (PLINE-TO-INDEX SELF POINT-PLINE)))
;                (SETQ POINT-PLINE NIL))))
;    ;; Now we have TOP-LINE and TOP-INDEX, and possibly POINT-PLINE.

;    ;; First, handle the case where just one line needs to be updated.
;    (AND (= REDISPLAY-DEGREE DIS-LINE)
;        (LET ((LINE REDISPLAY-LINE)
;              (INDEX REDISPLAY-INDEX))
;          (LET ((P (FIND-BP-IN-WINDOW SELF LINE INDEX))
;                (LINE-LENGTH (IF (EQ LINE (BP-LINE LAST-BP)) (BP-INDEX LAST-BP)
;                                 (LINE-LENGTH LINE)))
;                LEN DWID)
;            ;; LEN gets the raster position in the pline P
;            ;; of the character in LINE at position INDEX.
;            (WHEN P
;              (IF (AND P (EQ (PLINE-LINE SELF P) *LAST-REDISPLAY-LINE*)
;                       (= INDEX *LAST-REDISPLAY-INDEX*)
;                       (EQ SELF *LAST-REDISPLAY-SHEET*))
;                  (SETQ LEN *LAST-REDISPLAY-CURSOR-X*)
;                (SETQ LEN (STRING-WIDTH LINE (PLINE-FROM-INDEX SELF P) INDEX SELF))))
;            (COND ((AND P
;                        ;; If P and LEN say we are at the start of a continuation line,
;                        ;; then maybe they are wrong
;                        ;; (if the contin line has been exactly deleted).
;                        (OR (NOT (ZEROP LEN))
;                            (ZEROP INDEX)))
;                   ;; Reverse-video region marking must be removed before updating.
;                   (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
;                        (OR (PLINE-MARKING-LEFT SELF P)
;                            (PLINE-MARKING-LEFT SELF (MIN (1+ P) (1- N-PLINES))))
;                        (REGION-UNMARK-PLINE P))
;                   ;; Go to the place in the line where changes start. Clear from there.
;                   ;; This means that any region marking from there on is gone now.
;                   (COND ((AND (PLINE-MARKING-LEFT SELF P)
;                               (< (PLINE-MARKING-LEFT SELF P) LEN))
;                          (SETF (PLINE-MARKING-WIDTH SELF P)
;                                (MIN (- LEN (PLINE-MARKING-LEFT SELF P))
;                                     (PLINE-MARKING-WIDTH SELF P))))
;                         (T (SETF (PLINE-MARKING-LEFT SELF P) NIL)
;                            (SETF (PLINE-MARKING-WIDTH SELF P) NIL)))
;                   ;; If the character is wider than it claims to be, draw an extra
;                   ;; character, since the clear-eol will erase data.
;                   (OR (ZEROP INDEX)
;                       (LET ((CH (AREF LINE (1- INDEX))))
;                         (AND (< (CHAR-CODE CH) #o200)
;                              (LET ((FONT (AREF (TV:SHEET-FONT-MAP SELF)
;                                                (CHAR-FONT CH)))
;                                    CWT)
;                                (AND (SETQ CWT (FONT-CHAR-WIDTH-TABLE FONT))
;                                     (LET ((CWID (AREF CWT (SETQ CH (CHAR-CODE CH))))
;                                           (RWID (FED:FONT-CHAR-MIN-RASTER-WIDTH FONT CH)))
;                                       (AND (> RWID CWID) (SETQ DWID CWID))))))))
;                   (MULTIPLE-VALUE-BIND (I TW)
;                       (TV:SHEET-LINE-OUT SELF LINE INDEX LINE-LENGTH LEN (* LH P) DWID)
;                     ;; Save cursor x to avoid calls to STRING-WIDTH while inserting text.
;                     (SETQ *LAST-REDISPLAY-SHEET* SELF
;                           *LAST-REDISPLAY-LINE* LINE
;                           *LAST-REDISPLAY-INDEX* LINE-LENGTH
;                           *LAST-REDISPLAY-CURSOR-X* TW)
;                     ;; We have output the first PLINE of this line
;                     (SETF (PLINE-TO-INDEX SELF P) I)
;                     (SETF (PLINE-TEXT-WIDTH SELF P)
;                           (IF ( I LINE-LENGTH) TW    ;Continuation needed
;                               (+ TW (TV:SHEET-CHAR-WIDTH SELF)))) ;Allow for CR
;                     (SETF (PLINE-TICK SELF P) NOW)
;                     ;; See if plines below this need to be redisplayed, due
;                     ;; to line-continuation issues
;                     (COND ((AND (< (1+ P) N-PLINES)
;                                 (OR ( I LINE-LENGTH)
;                                     ( (+ TW (TV:SHEET-INSIDE-LEFT SELF))
;                                        (TV:SHEET-INSIDE-RIGHT SELF))
;                                     (EQ (PLINE-LINE SELF (1+ P)) LINE)))
;                            (SETQ REDISPLAY-DEGREE DIS-TEXT POINT-PLINE NIL)
;                            ;; If we are just creating a new continuation line, make it
;                            ;; still look munged, so REDISPLAY-BLT can understand.
;                            (OR (EQ (PLINE-LINE SELF (1+ P)) LINE)
;                                (SETF (PLINE-TICK SELF P) -1))))))
;                  (T
;                   (SETQ REDISPLAY-DEGREE DIS-TEXT POINT-PLINE NIL))))))
;       ;; If all the window should be redisplayed, mark each pline as unknown.
;       (COND (( REDISPLAY-DEGREE DIS-ALL)
;              (TV:SHEET-CLEAR SELF T)
;              (SEND SELF ':REFRESH-MARGINS)
;              (DO ((I 0 (1+ I)))
;                  ((= I N-PLINES))
;                (SETF (PLINE-TICK SELF I) -1)
;                (SETF (PLINE-MARKING-LEFT SELF I) NIL))))
;       (COND (( REDISPLAY-DEGREE DIS-TEXT)
;              (SETQ *LAST-REDISPLAY-LINE* NIL)
;              ;; In case we abort before we are done, don't forget what's needed.
;              (SETF REDISPLAY-DEGREE DIS-TEXT)
;              (SETF LAST-BP-DISPLAYED-P NIL)
;              (DO ((L SPECIAL-BLINKER-LIST (CDR L)))
;                  ((NULL L))
;                (SEND (CDAR L) :SET-VISIBILITY NIL))
;              ;; Abort now if input available
;              (AND (NOT FORCE-TO-COMPLETION-P)
;                   (SEND *STANDARD-INPUT* :LISTEN)
;                   (RETURN-FROM ABORT-REDISPLAY NIL))
;              ;; Attempt to do insert and delete line cleverness.
;              (REDISPLAY-BLT)
;              ;; This might have invalidated the value of POINT-PLINE.
;              ;; It won't be hard to recompute, so do so.
;              (SETQ POINT-PLINE NIL)
;              ;; First loop over actual lines.
;              (DO-NAMED LINES
;                        ((LINE TOP-LINE (LINE-NEXT LINE))
;                         (FROM-INDEX TOP-INDEX 0)
;                         (TO-INDEX)
;                         (PLINE 0)
;                         (STOP-LINE (BP-LINE LAST-BP)))
;                        (NIL)
;                ;; Between lines, check for input available and abort if so.
;                (AND (NOT FORCE-TO-COMPLETION-P)
;                     (ZEROP (CL:REM PLINE 30.))
;                     (SEND *STANDARD-INPUT* ':LISTEN)
;                     (RETURN-FROM ABORT-REDISPLAY NIL))
;                (SETQ TO-INDEX (IF (EQ LINE STOP-LINE) (BP-INDEX LAST-BP)
;                                   (LINE-LENGTH LINE)))
;                ;; Now loop over the plines of this line.
;                (DO NIL (NIL)
;                  (AND ( PLINE N-PLINES) (RETURN-FROM LINES))
;                  ;; Check for a line that has not been changed.
;                  (COND ((AND (EQ LINE (PLINE-LINE SELF PLINE))
;                              (> (PLINE-TICK SELF PLINE) (LINE-TICK LINE))
;                              (= (PLINE-FROM-INDEX SELF PLINE) FROM-INDEX))
;                         (SETQ FROM-INDEX (PLINE-TO-INDEX SELF PLINE)))
;                        (T
;                         ;; Reverse-video region marking must be removed before updating.
;                         (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
;                              (OR (PLINE-MARKING-LEFT SELF PLINE)
;                                  (PLINE-MARKING-LEFT SELF
;                                                      (MIN (1+ PLINE) (1- N-PLINES))))
;                              (REGION-UNMARK-PLINE PLINE))
;                         ;; This should work differently
;                         (LET ((FROB (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM)) I TW)
;                           (COND (FROB
;                                  (TV:SHEET-SET-CURSORPOS SELF 0 (* LH PLINE))
;                                  (TV:SHEET-CLEAR-EOL SELF)
;                                  (SEND FROB ':DRAW LINE SELF)
;                                  (SETQ I 1 TW 0))
;                                 (T
;                                  (MULTIPLE-VALUE (I TW)
;                                    (TV:SHEET-LINE-OUT SELF LINE
;                                                       FROM-INDEX TO-INDEX
;                                                       0 (* LH PLINE)))))
;                           (SETF (PLINE-LINE SELF PLINE) LINE)
;                           (SETF (PLINE-FROM-INDEX SELF PLINE) FROM-INDEX)
;                           (SETF (PLINE-TO-INDEX SELF PLINE) I)
;                           (SETF (PLINE-TICK SELF PLINE) NOW)
;                           (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL)
;                           (SETF (PLINE-TEXT-WIDTH SELF PLINE)
;                                 (IF ( I (LINE-LENGTH LINE)) TW       ;Continuation needed
;                                     (+ TW (TV:SHEET-CHAR-WIDTH SELF))))       ;Allow for CR
;                           (SETQ FROM-INDEX I))))
;                  (SETQ PLINE (1+ PLINE))
;                  ;; This is >, not , because if line isn't cont'd then PLINE-TO-PLINE
;                  ;; counts the phony CR which is output by SHEET-LINE-OUT.
;                  (AND (> FROM-INDEX TO-INDEX) (RETURN)))
;                ;; Check for the last line in the interval.
;                (COND ((EQ LINE STOP-LINE)
;                       (SETF LAST-BP-DISPLAYED-P T)
;                       (OR (< PLINE N-PLINES) (RETURN-FROM LINES))
;                       (AND (NULL (PLINE-LINE SELF PLINE))
;                            (PLINE-TICK SELF PLINE) (> (PLINE-TICK SELF PLINE) 0)
;                            (RETURN-FROM LINES)) ;Return if screen already blanked
;                       ;; Reverse-video region marking must be removed before updating.
;                       (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
;                            (OR (PLINE-MARKING-LEFT SELF PLINE)
;                                (PLINE-MARKING-LEFT SELF
;                                                    (MIN (1+ PLINE) (1- N-PLINES))))
;                            (REGION-UNMARK-PLINE PLINE))
;                       ;; Clean out the rest of the window beneath it.  Then exit.
;                       (TV:SHEET-SET-CURSORPOS SELF 0 (* LH PLINE))
;                       (TV:SHEET-CLEAR-EOF SELF)
;                       (DO PLINE PLINE (1+ PLINE) ( PLINE N-PLINES)
;                           (SETF (PLINE-LINE SELF PLINE) NIL)
;                           (SETF (PLINE-TICK SELF PLINE) NOW)
;                           (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL))
;                       (RETURN-FROM LINES))))))
;       ;; These are just for debugging the errors reported below.
;       (SETQ POINT-NODE (BP-TOP-LEVEL-NODE POINT)
;             START-BP-NODE (BP-TOP-LEVEL-NODE START-BP)
;             BUF *INTERVAL*)
;       (COND (( REDISPLAY-DEGREE DIS-BPS)
;              ;; BPs have moved.  Reposition the POINT blinker.
;              (OR POINT-PLINE
;                  (SETQ POINT-PLINE (FIND-BP-IN-WINDOW SELF POINT-LINE POINT-INDEX))
;                  (EQ RECENTER-TYPE ':NONE)
;                  (IF (AND (= INITIAL-DEGREE DIS-LINE) (= REDISPLAY-DEGREE DIS-TEXT))
;                      ;;Somewhat anomalous case, try again with greater redisplay degree
;                      (RETURN (REDISPLAY SELF RECENTER-TYPE RC1 RC2
;                                         FORCE-TO-COMPLETION-P))
;                    (UNWIND-PROTECT
;                      (FERROR NIL "Recenter type ~S left point outside the window."
;                              RECENTER-TYPE)
;                      ;; Try to clean things up so error won't repeat.
;                      (MOVE-BP POINT (INTERVAL-FIRST-BP INTERVAL))
;                      (MOVE-BP START-BP (INTERVAL-FIRST-BP INTERVAL)))))
;              (COND ((NULL POINT-PLINE)
;                     ;; POINT is not on the window, so make it go away.
;                     (SEND POINT-BLINKER :SET-VISIBILITY NIL))
;                    (T
;                     ;; POINT is on the window, find its Y position.
;                     (SEND POINT-BLINKER :SET-VISIBILITY
;                           (IF (EQ SELF TV:SELECTED-WINDOW)
;                               ':BLINK
;                               (TV:BLINKER-DESELECTED-VISIBILITY POINT-BLINKER)))
;                     (UNLESS (EQ POINT-LINE (PLINE-LINE SELF POINT-PLINE))
;                       (DPRINT POINT-LINE POINT-PLINE (PLINE-LINE SELF POINT-PLINE))
;                       (UNWIND-PROTECT
;                           (FERROR NIL "Position of POINT on window is screwed up.")
;                         ;; Try to clean things up so error won't repeat.
;                         (MOVE-BP POINT (INTERVAL-FIRST-BP INTERVAL))
;                         (MOVE-BP START-BP (INTERVAL-FIRST-BP INTERVAL))))
;                     (SET-BLINKER-SIZE POINT POINT-BLINKER
;                                       (TV:SHEET-COMPUTE-MOTION SELF 0 0 POINT-LINE
;                                                     (PLINE-FROM-INDEX SELF POINT-PLINE)
;                                                     POINT-INDEX)
;                                       (* LH POINT-PLINE))
;                     (SETF LAST-POINT-PLINE POINT-PLINE)))
;              ;; Blink the parens, etc.
;              (DOLIST (BL SPECIAL-BLINKER-LIST)
;                (FUNCALL (CAR BL) (CDR BL) SELF POINT START-BP))))
;       (COND (( REDISPLAY-DEGREE DIS-MARK-GOES)
;              ;; The region marking may have changed.
;              (UPDATE-REGION-MARKING)))
;       ;;The character under the mouse also
;       (AND ( REDISPLAY-DEGREE DIS-BPS) (MOUSE-RETHINK SELF))
;       (AND ( REDISPLAY-DEGREE DIS-TEXT)
;            (SEND SELF ':NEW-SCROLL-POSITION))
;       (SETF REDISPLAY-DEGREE DIS-NONE)
;       ))

;;; If text has changed, make sure the mouse blinker knows about it
(DEFUN MOUSE-RETHINK (WINDOW)
  (AND (EQ WINDOW TV:MOUSE-WINDOW)
       (TV:MOUSE-WAKEUP)))

(defmethod (window :screen-line-out) (string &optional (start 0) (stop nil) set-xpos set-ypos dwidth)
  (tv:sheet-line-out self string start stop set-xpos set-ypos dwidth))

;; Return the maximum number of plines we can have.
;; For an ordinary zwei:window displayer, the number of plines is constant
;; so just return that.
(defmethod (window :max-n-plines) () n-plines)

;; Move lines up or down on the screen as seems useful to speed redisplay.
(DEFMETHOD (WINDOW :REDISPLAY-BLT) (&OPTIONAL IGNORE)
  (BLOCK REDISPLAY-BLT
    (LET* ((NEW-FIRST-LINE (BP-LINE START-BP))
           (FIRST-CHANGED-LINE NEW-FIRST-LINE) (FIRST-CHANGED-PLINE 0)
           LAST-OCCUPIED-PLINE
           FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE
           FIRST-UNCHANGED-LINE-NEW-PLINE)
      (COND ((AND (EQ NEW-FIRST-LINE (PLINE-LINE SELF 0))
                  (= (BP-INDEX START-BP) (PLINE-FROM-INDEX SELF 0)))
             ;; Find the first place in the window at which anything is changed.
             ;; FIRST-CHANGED-LINE gets the NEW line that should be displayed there.
             ;; Make sure that FIRST-CHANGED-PLINE gets the FIRST pline of that line!
             ;; When a character is inserted into a line which is continued,
             ;; REDISPLAY's DIS-LINE processing updates the first pline including tick
             ;; before noticing the continuation.  Then, the continuation pline would
             ;; be the first mismatch!
             (DO ((PLINE 0 (1+ PLINE))
                  (LINE NEW-FIRST-LINE))
                 ((OR ( PLINE N-PLINES)
                      (NEQ (PLINE-LINE SELF PLINE) LINE)
                      (< (PLINE-TICK SELF PLINE)
                         (LINE-TICK LINE))))
               (AND (> (PLINE-TO-INDEX SELF PLINE)
                       (LINE-LENGTH LINE))
                    (NOT (SETQ LINE (LINE-NEXT LINE)
                               FIRST-CHANGED-PLINE (1+ PLINE)
                               FIRST-CHANGED-LINE LINE))
                    (RETURN-FROM REDISPLAY-BLT NIL)))))

        ;; Now find the last non-null line (that used to be) in the window.
        ;; LAST-OCCUPIED-PLINE says where to find it in the window.
        (DO ((PLINE (1- N-PLINES) (1- PLINE)))
            ((PLINE-LINE SELF PLINE)
             (SETQ LAST-OCCUPIED-PLINE PLINE))
          (AND (ZEROP PLINE) (RETURN-FROM REDISPLAY-BLT NIL)))
        ;; Now scan upward from there till we find a change.
        (DO ((PLINE LAST-OCCUPIED-PLINE (1- PLINE))
             (LINE (PLINE-LINE SELF LAST-OCCUPIED-PLINE)))
            ((OR (MINUSP PLINE)
                 (NEQ (PLINE-LINE SELF PLINE) LINE)
                 ;; Give up if we come across a deleted line.
                 ;; That tells us that these lines are no longer relevant.
                 (AND (EQ (LINE-TICK LINE) 'DELETED)
                      (RETURN-FROM REDISPLAY-BLT NIL))
                 (< (PLINE-TICK SELF PLINE)
                    (LINE-TICK LINE))))
          ;; If we have reached the line which will now occupy the first pline,
          ;; then if it will be split across the top of the window,
          ;; we must not include it in the blt.
          ;; If we are going to include it, we will exit after doing so
          ;; via the RETURN in the COND below.
          (AND (EQ LINE NEW-FIRST-LINE)
               (NOT (ZEROP (BP-INDEX START-BP)))
               (RETURN))
          ;; When we come move back past the start of a line in the window,
          ;; do so also in the interval,
          ;; and include the line we have moved over in the blt.
          ;; This way, a line which used to be split over the top of the window
          (COND ((ZEROP (PLINE-FROM-INDEX SELF PLINE))
                 (SETQ FIRST-UNCHANGED-PLINE PLINE
                       FIRST-UNCHANGED-LINE LINE)
                 (AND (EQ LINE NEW-FIRST-LINE)
                      (RETURN))
                 (SETQ LINE (LINE-PREVIOUS LINE)))))
        ;; FIRST-UNCHANGED-LINE is the first line of those to be blt'ed.
        ;; But make sure that it is still in the interval, and not too far away,
        ;; before we do anything to it.  Note, maybe we passed it in the last DO.
        (DO ((I N-PLINES (1- I))
             (LINE FIRST-UNCHANGED-LINE (LINE-PREVIOUS LINE)))
            ((OR (ZEROP I) (NULL LINE))
             (RETURN-FROM REDISPLAY-BLT NIL))
          (WHEN (EQ LINE FIRST-CHANGED-LINE)
            (RETURN)))
        ;; Now we know we can win, so find out where to blt FIRST-UNCHANGE-LINE to.
        (SETQ FIRST-UNCHANGED-LINE-NEW-PLINE
              (+ FIRST-CHANGED-PLINE
                 (MULTIPLE-VALUE-BIND (NIL NIL PLINE-OFFSET)
                     (PUT-POINT-AT-PLINE SELF FIRST-UNCHANGED-LINE 0
                                         (- N-PLINES FIRST-CHANGED-PLINE)
                                         (IF (ZEROP FIRST-CHANGED-PLINE) START-BP
                                             (CREATE-BP FIRST-CHANGED-LINE
                                               (PLINE-FROM-INDEX SELF FIRST-CHANGED-PLINE)))
                                         NIL)
                   PLINE-OFFSET)))
        (IF (OR ( FIRST-UNCHANGED-LINE-NEW-PLINE N-PLINES)
                (= FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
                ;; If the number of lines to be preserved is less than 1/4 of the distance they move,
                ;; don't bother moving them, since it looks ugly anyway.
                (< (* 4 (- N-PLINES (MAX FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
                   (ABS (- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE))))
            (RETURN-FROM REDISPLAY-BLT NIL))
        ;; Now do the actual moving of text on the screen.
        (TV:SHEET-SET-CURSORPOS SELF 0
                                (* (TV:SHEET-LINE-HEIGHT SELF)
                                   (MIN FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
        ;; Eliminate any region marking on the first line to be moved
        ;; since it may overlap the vsp above the line, and get split by the move, yech.
        ;; It will get turned back on again if necessary by UPDATE-REGION-MARKING.
        ;; Done only if we are using solid region marking and there is marking where it matters.
        (AND (EQ *REGION-MARKING-MODE* ':REVERSE-VIDEO)
             (OR (PLINE-MARKING-LEFT SELF FIRST-UNCHANGED-PLINE)
                 (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
                     (PLINE-MARKING-LEFT SELF FIRST-UNCHANGED-LINE-NEW-PLINE)
                   (PLINE-MARKING-LEFT SELF
                                       (- N-PLINES
                                          (- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
                                          1))))
             ;; We have ascertained that there is marking that needs to be cleared out.
             (LET ((LH (TV:SHEET-LINE-HEIGHT SELF))
                   (OFFSET (- (TV:SHEET-INSIDE-TOP SELF) (FLOOR (SEND SELF :VSP) 2))))
               (REGION-UNMARK-RANGE FIRST-UNCHANGED-PLINE (1+ FIRST-UNCHANGED-PLINE)
                                    LH OFFSET)
               (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
                   ;; Copying upward.  Also flush region marking on old line being moved to.
                   (REGION-UNMARK-RANGE FIRST-UNCHANGED-LINE-NEW-PLINE
                                        (1+ FIRST-UNCHANGED-LINE-NEW-PLINE)
                                        LH OFFSET)
                 ;; Copying downward.  Flush region marking on last line moving to bottom.
                 (REGION-UNMARK-RANGE (- N-PLINES
                                         (- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
                                         1)
                                      N-PLINES
                                      LH OFFSET))))
        (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
            ;; Copying upward.
            (TV:SHEET-DELETE-LINE SELF
                                  (- FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
            ;; Copying downward.
            (TV:SHEET-INSERT-LINE SELF
                                  (- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
        ;; Now copy the contents of the window array just as we moved the bits.
        (LET ((INC (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE) 1 -1))
              (NEW-START FIRST-UNCHANGED-LINE-NEW-PLINE))
          (AND (MINUSP INC) (SETQ NEW-START (1- N-PLINES)))
          (DO ((NEW-PLINE NEW-START (+ INC NEW-PLINE))
               (OLD-PLINE (+ NEW-START (- FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
                          (+ INC OLD-PLINE)))
              ((OR (= OLD-PLINE N-PLINES)
                   (< OLD-PLINE FIRST-UNCHANGED-PLINE)))
            (SETF (PLINE-LINE SELF NEW-PLINE)
                  (PLINE-LINE SELF OLD-PLINE))
            (SETF (PLINE-FROM-INDEX SELF NEW-PLINE)
                  (PLINE-FROM-INDEX SELF OLD-PLINE))
            (SETF (PLINE-TO-INDEX SELF NEW-PLINE)
                  (PLINE-TO-INDEX SELF OLD-PLINE))
            (SETF (PLINE-TEXT-WIDTH SELF NEW-PLINE)
                  (PLINE-TEXT-WIDTH SELF OLD-PLINE))
            (SETF (PLINE-MARKING-LEFT SELF NEW-PLINE)
                  (PLINE-MARKING-LEFT SELF OLD-PLINE))
            (SETF (PLINE-MARKING-WIDTH SELF NEW-PLINE)
                  (PLINE-MARKING-WIDTH SELF OLD-PLINE))
            (SETF (PLINE-TICK SELF NEW-PLINE)
                  (PLINE-TICK SELF OLD-PLINE)))
          ;; Mark as clear the lines cleared by the insert or delete.
          (IF (MINUSP INC)
              (DO ((PLINE FIRST-UNCHANGED-PLINE (1+ PLINE)))
                  ((= PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
                (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL)
                (SETF (PLINE-LINE SELF PLINE) NIL))
              (DO ((PLINE (1- N-PLINES) (1- PLINE))
                   (I FIRST-UNCHANGED-LINE-NEW-PLINE (1+ I)))
                  ((= I FIRST-UNCHANGED-PLINE))
                (SETF (PLINE-LINE SELF PLINE) NIL)
                (SETF (PLINE-MARKING-LEFT SELF PLINE) NIL)))))))

(DEFMETHOD (WINDOW :PUT-POINT-AT-PLINE) (POINT-LINE POINT-INDEX POINT-PLINE FIRST-BP LAST-BP
                                         &AUX
                                         (LH TV:LINE-HEIGHT))
  (IF ( POINT-PLINE 0)
      ;; Algorithm: first find LINE, which will be the new TOP-LINE,
      ;; by scanning backwards.  Then knock off plines from the front
      ;; of it until POINT ends up at POINT-PLINE.
      ;; P is the number of plines between POINT and the beginning
      ;; of the current LINE.
      (DO ((LINE POINT-LINE)
           ;; P is the point-pline if we start at the beginning of LINE.
           (P (FLOOR (NTH-VALUE 1
                       ;; Compute which continuation line of POINT-LINE point is on.
                       (ED-COMPUTE-MOTION SELF POINT-LINE POINT-INDEX FIRST-BP LAST-BP))
                     LH))
           (STOP-LINE (BP-LINE FIRST-BP)))
          (( P POINT-PLINE)
           ;; We have found the new TOP-LINE.  Now find TOP-INDEX.
           (VALUES
             LINE
             (LET ((DIFFERENCE (- P POINT-PLINE)))
               (IF (ZEROP DIFFERENCE)
                   0
                 ;; Compute motion to move Y down DIFFERENCE plines.
                 (MULTIPLE-VALUE-BIND (NIL NIL NC)
                     (TV:SHEET-COMPUTE-MOTION SELF 0 0 LINE
                                              (IF (EQ LINE STOP-LINE)
                                                  (BP-INDEX FIRST-BP) 0)
                                              NIL NIL
                                              0 (* LH DIFFERENCE) MOST-POSITIVE-FIXNUM)
                   (IF (EQ LINE STOP-LINE)
                       (MAX NC (BP-INDEX FIRST-BP))
                       NC))))
             POINT-PLINE))
        (IF (or (EQ LINE STOP-LINE)
                (null (line-previous line)))  ;observed to happen in dired RG.
            (RETURN (VALUES LINE 0 P)))
        (SETQ LINE (LINE-PREVIOUS LINE))
        (INCF P (FLOOR (NTH-VALUE 1
                         ;; Compute downward motion of this line, with fake CR.
                         (TV:SHEET-COMPUTE-MOTION SELF 0 0 LINE
                           (IF (EQ LINE STOP-LINE) (BP-INDEX FIRST-BP) 0)
                           NIL T 0 MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM))
                       LH)))
  ;; POINT-PLINE is negative, do the same thing in reverse.
  (DO* ((LINE POINT-LINE (LINE-NEXT LINE))
        (LINE-START-INDEX POINT-INDEX 0)
        (THIS-LINE-HEIGHT)
        ;; P is the point-pline if we display from beg of (LINE-NEXT LINE).
        ;; This line is the one if P is too far.
        (P 0)
        (STOP-LINE (BP-LINE LAST-BP)))
       ((NULL LINE))            ;should not be true, but be defensive.
    (SETQ THIS-LINE-HEIGHT
          (ED-COMPUTE-LINE-MOTION SELF LINE LINE-START-INDEX LAST-BP))
    (DECF P THIS-LINE-HEIGHT)
    (IF (< P POINT-PLINE)
        ;; We have found the new TOP-LINE.  Now find TOP-INDEX.
        (RETURN
          (VALUES
            LINE
            (LET ((DIFFERENCE (- P POINT-PLINE)))
              (IF (ZEROP (+ THIS-LINE-HEIGHT DIFFERENCE))
                  0
                (MULTIPLE-VALUE-BIND (NIL NIL NC)
                    (TV:SHEET-COMPUTE-MOTION SELF 0 0 LINE LINE-START-INDEX
                                             NIL T
                                             0
                                             (* LH (+ THIS-LINE-HEIGHT DIFFERENCE))
                                             MOST-POSITIVE-FIXNUM)
                  (IF (EQ LINE STOP-LINE)
                      (MIN NC (BP-INDEX LAST-BP))
                      NC))))
            POINT-PLINE)))
    (IF (EQ LINE STOP-LINE)
        (RETURN (VALUES LINE 0 P))))))

;;; Compute the downwarn motion of an entire line, including following CR if any.
;;; The value is in units of lines, not pixels.
(DEFUN ED-COMPUTE-LINE-MOTION (SHEET LINE START-INDEX LAST-BP &OPTIONAL WIDTH
                               &AUX (STOP-LINE (BP-LINE LAST-BP)))
  (FLOOR (NTH-VALUE 1
           (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE START-INDEX
             (AND (EQ LINE STOP-LINE)
                  (MIN (1+ (BP-INDEX LAST-BP))
                       (LINE-LENGTH STOP-LINE)))
             (NEQ LINE STOP-LINE)
             0 MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM WIDTH))
         (TV:SHEET-LINE-HEIGHT SHEET)))

;;; Do a SHEET-COMPUTE-MOTION, but take into account that if the cursor
;;; reaches the right margin in the middle of the line, it moves to the
;;; beginning of the next line "before" the next character if that char is not a Newline.
;;; WIDTH is the width of window to use in computation -- NIL means use its actual width.
(DEFUN ED-COMPUTE-MOTION (SHEET LINE INDEX FIRST-BP LAST-BP &OPTIONAL WIDTH)
  (MULTIPLE-VALUE-BIND (FINAL-X FINAL-Y FINAL-INDEX MAX-X)
      (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE
                               (IF (EQ LINE (BP-LINE FIRST-BP))
                                   (BP-INDEX FIRST-BP) 0)
                               INDEX NIL 0 MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM WIDTH)
    (IF (AND ( INDEX (LINE-LENGTH LINE))
             (NOT (AND (EQ LINE (BP-LINE LAST-BP))
                       (= INDEX (BP-INDEX LAST-BP))))
             (> (+ FINAL-X
                   ;; There must be room for the next character and the "!".
                   (* 2 (TV:SHEET-CHAR-WIDTH SHEET)))
                (TV:SHEET-INSIDE-WIDTH SHEET)))
        (SETQ FINAL-X 0
              FINAL-Y (+ FINAL-Y (TV:SHEET-LINE-HEIGHT SHEET))))
    (VALUES FINAL-X FINAL-Y FINAL-INDEX MAX-X)))

;;;; Update region marking of sheet windows.

;;; This is an internal function of REDISPLAY.
(DEFMETHOD (WINDOW :UPDATE-REGION-MARKING) ()
  (LET ((BP1 POINT)
        (BP2 MARK)
        (LAST-PLINE (1- N-PLINES))
        (LH TV:LINE-HEIGHT)
        (VSP (SEND SELF :VSP))
        (TOP (TV:SHEET-INSIDE-TOP))
        HEIGHT OFFSET
        PLINE-1 X-1 PLINE-2 X-2)
    ;; The four variables above designate what stuff should be marked.
    (ECASE *REGION-MARKING-MODE*
       (:UNDERLINE
        (SETQ OFFSET (+ TOP (- LH VSP 1)) HEIGHT 1)) ;In the highest of the VSP lines
       (:REVERSE-VIDEO
        (SETQ OFFSET (- TOP (FLOOR VSP 2)) HEIGHT LH)))
    (COND ((> HEIGHT 0)
           ;; That is, if marking is turned on.
           (COND ((NOT MARK-P)
                  (REGION-UNMARK-RANGE 0 N-PLINES HEIGHT OFFSET))
                 (T
                  (LET ((LINE-1 (BP-LINE BP1))
                        (INDEX-1 (BP-INDEX BP1))
                        (LINE-2 (BP-LINE BP2))
                        (INDEX-2 (BP-INDEX BP2))
                        (LAST-LINE (PLINE-LINE SELF LAST-PLINE))
                        P1 P2)
                    ;; Each BP may be before the window, after the window, or in the window.
                    (SETQ P1 (FIND-BP-IN-WINDOW SELF LINE-1 INDEX-1))
                    (SETQ P2 (FIND-BP-IN-WINDOW SELF LINE-2 INDEX-2))
                    ;; Hold on to your hats!  Here we effectively do a 9-way dispatch, based
                    ;; on whether each of the two bps is in, before, or after the window.
                    ;; If PLINE-n is left NIL, it and X-n will be set to zero.
                    ;; If PLINE-n is set but X-n isn't, X-n will come from
                    ;;  SHEET-COMPUTE-MOTION.
                    (COND ((NULL P1)
                           ;; Line 1 is not on the screen, which way did he go?
                           (COND ((AND LAST-LINE
                                       (SEARCH-FOR-LINE LINE-1 LAST-LINE))
                                  ;; Line 1 is ahead of the screen, check out Line 2.
                                  (COND ((NULL P2)
                                         ;; Line 2 isn't on the window either.
                                         (COND ((AND LAST-LINE
                                                     (SEARCH-FOR-LINE LINE-2 LAST-LINE))
                                                ;; ** They are both ahead, no display.
                                                )
                                               (T ; ** Line 2 is behind, mark all.
                                                (SETQ X-2 (PLINE-TEXT-WIDTH SELF LAST-PLINE)
                                                      PLINE-2 LAST-PLINE))))
                                        (T ; ** Line 2 is on, Line 1 is ahead.
                                         (SETQ PLINE-1 P2 LINE-1 LINE-2 INDEX-1 INDEX-2
                                               PLINE-2 LAST-PLINE
                                               X-2 (PLINE-TEXT-WIDTH SELF LAST-PLINE)))))
                                 (T ;; Line 1 is behind the window, check out Line 2.
                                  (COND ((NULL P2)
                                         ;; Line 2 isn't on the screen either.
                                         (COND ((AND LAST-LINE
                                                     (SEARCH-FOR-LINE LINE-2 LAST-LINE))
                                                ;; ** Line 2 is ahead, mark all.
                                                ;; ** Otherwise no marking.
                                                (SETQ X-2 (PLINE-TEXT-WIDTH SELF LAST-PLINE)
                                                      PLINE-2 LAST-PLINE))))
                                        (T ; ** Line 2 is on, Line 1 is behind.
                                         (SETQ PLINE-2 P2))))))
                          (T ; Line 1 is on the window, check out Line 2.
                           (COND ((NULL P2)
                                  ;; Line 2 is not on the window.
                                  (COND ((AND LAST-LINE
                                              (SEARCH-FOR-LINE LINE-2 LAST-LINE))
                                         ;; ** Line 2 is ahead and Line 1 is on.
                                         (SETQ PLINE-1 P1 PLINE-2 LAST-PLINE
                                               X-2 (PLINE-TEXT-WIDTH SELF LAST-PLINE)))
                                        (T ; ** Line 2 is behind, Line 1 is on.
                                         (SETQ PLINE-2 P1 LINE-2 LINE-1 INDEX-2 INDEX-1))))
                                 (T ; ** Both are on.
                                  (COND ((OR (NOT (SEARCH-FOR-LINE LINE-1 LINE-2))
                                             (AND (EQ LINE-1 LINE-2)
                                                  (< INDEX-1 INDEX-2)))
                                         ;; Line 1 is behind Line 2.
                                         (SETQ PLINE-1 P1 PLINE-2 P2))
                                        (T
                                         (SETQ LINE-1 (PROG1 LINE-2 (SETQ LINE-2 LINE-1)))
                                         (SETQ INDEX-1 (PROG1 INDEX-2
                                                              (SETQ INDEX-2 INDEX-1)))
                                         (SETQ PLINE-1 P2 PLINE-2 P1)))))))
                    (COND ((NULL PLINE-1)
                           (SETQ PLINE-1 0 X-1 0))
                          ((NULL X-1)
                           (SETQ X-1 (TV:SHEET-COMPUTE-MOTION SELF 0 0 LINE-1 0 INDEX-1))))
                    (COND ((NULL PLINE-2)
                           (SETQ PLINE-2 0 X-2 0))
                          ((NULL X-2)
                           (SETQ X-2 (TV:SHEET-COMPUTE-MOTION SELF 0 0 LINE-2 0 INDEX-2))))
                    ;; Now PLINE-1, X-1 and PLINE-2, X-2 are set up.
                    (REGION-UNMARK-RANGE 0 PLINE-1 HEIGHT OFFSET)
                    (COND ((EQ PLINE-1 PLINE-2)
                           (REGION-MARK-PLINE PLINE-1 HEIGHT (+ OFFSET (* LH PLINE-1))
                                              X-1 X-2))
                          (T
                           (REGION-MARK-PLINE PLINE-1 HEIGHT (+ OFFSET (* LH PLINE-1))
                                              X-1
                                              (IF *REGION-RIGHT-MARGIN-MODE*
                                                  (TV:SHEET-INSIDE-WIDTH SELF)
                                                (PLINE-TEXT-WIDTH SELF PLINE-1)))
                           (DO ((P (1+ PLINE-1) (1+ P))
                                (Y-POS (+ OFFSET (* LH (1+ PLINE-1))) (+ Y-POS LH)))
                               (( P PLINE-2))
                             (REGION-MARK-PLINE P HEIGHT
                                                Y-POS 0
                                              (IF *REGION-RIGHT-MARGIN-MODE*
                                                  (TV:SHEET-INSIDE-WIDTH SELF)
                                                (PLINE-TEXT-WIDTH SELF P))))
                           (REGION-MARK-PLINE PLINE-2 HEIGHT (+ OFFSET (* LH PLINE-2))
                                              0 X-2)))
                    (REGION-UNMARK-RANGE (1+ PLINE-2) N-PLINES HEIGHT OFFSET))))))))

;;; This is an internal function of UPDATE-REGION-MARKING.
(DEFUN REGION-MARK-PLINE (PLINE HEIGHT Y-POS NEW-LEFT NEW-RIGHT)
  (DECLARE (:SELF-FLAVOR WINDOW))
  (LET ((PML (PLINE-MARKING-LEFT SELF PLINE))
        (PMW (PLINE-MARKING-WIDTH SELF PLINE))
        (NEW-WIDTH (MAX 0 (- NEW-RIGHT NEW-LEFT)))) ;A negative number here would lose badly
    (COND ((NOT (AND (EQ PML NEW-LEFT)
                     (EQ PMW NEW-WIDTH)))
           (TV:PREPARE-SHEET (SELF)
             (AND PML
                  (TV:%DRAW-RECTANGLE PMW HEIGHT (+ (TV:SHEET-INSIDE-LEFT) PML) Y-POS
                                      TV:ALU-XOR SELF))
             (TV:%DRAW-RECTANGLE
               NEW-WIDTH HEIGHT (+ (TV:SHEET-INSIDE-LEFT) NEW-LEFT) Y-POS TV:ALU-XOR SELF))
           (SETF (PLINE-MARKING-LEFT SELF PLINE) NEW-LEFT)
           (SETF (PLINE-MARKING-WIDTH SELF PLINE) NEW-WIDTH)))))

(DEFUN REGION-UNMARK-PLINE (PLINE)
  "Remove REVERSE-VIDEO (only!) region-marking from specified pline and following one.
The following one is included since its marking would overlap the specified line."
  (DECLARE (:SELF-FLAVOR WINDOW))
  (LET ((OFFSET (- (TV:SHEET-INSIDE-TOP SELF) (FLOOR (SEND SELF :VSP) 2))))
    (REGION-UNMARK-RANGE PLINE (MIN (+ PLINE 2) N-PLINES)
                         TV:LINE-HEIGHT OFFSET)))

;;; This is an internal function of UPDATE-REGION-MARKING and REDISPLAY-BLT.
(DEFUN REGION-UNMARK-RANGE (FROM-PLINE TO-PLINE HEIGHT OFFSET)
  (DECLARE (:SELF-FLAVOR WINDOW))
  (DO ((P FROM-PLINE (1+ P))
       (Y-POS (+ (* FROM-PLINE TV:LINE-HEIGHT) OFFSET) (+ Y-POS TV:LINE-HEIGHT)))
      (( P TO-PLINE))
    (LET ((PML (PLINE-MARKING-LEFT SELF P)))
      (COND (PML
             (TV:PREPARE-SHEET (SELF)
               (TV:%DRAW-RECTANGLE (PLINE-MARKING-WIDTH SELF P) HEIGHT
                                   (+ (TV:SHEET-INSIDE-LEFT) PML)
                                   Y-POS TV:ALU-XOR SELF))
             (SETF (PLINE-MARKING-LEFT SELF P) NIL))))))

(DEFMETHOD (WINDOW :SET-BLINKER-SIZE) (BP BLINKER X Y &AUX CHAR FONT CHAR2)
  "Set the blinker size and position of BLINKER.
Set the size for the character after BP, using SELF's font map;
then position it at X, Y."
  (SETQ CHAR (BP-CHARACTER BP))
  (SETQ FONT (COND ((CHAR CHAR #/NEWLINE)
                    (CHAR-FONT CHAR))
                   (( (SETQ CHAR2 (BP-CHARACTER-BEFORE BP)) #/NEWLINE)
                    (CHAR-FONT CHAR2))
                   (T
                    *FONT*)))                   ;At start of empty line use insert default
  (IF ( FONT (ARRAY-LENGTH (TV:SHEET-FONT-MAP SELF)))
      (SETQ FONT 0))
  (SETQ FONT (AREF (TV:SHEET-FONT-MAP SELF) FONT)
        CHAR (CHAR-CODE CHAR))
  (COND ((= CHAR (CHAR-INT #/NEWLINE))
         ;; At end of line, make blinker the width of a space.
         (SETQ CHAR (CHAR-INT #/SPACE)))
        ((AND (= CHAR (CHAR-INT #/TAB)) *TAB-BLINKER-FLAG*)
         ;; Some people find blinkers over tabs annoying.
         (SETQ CHAR (CHAR-INT #/SPACE))))
  ;; Set the blinker position, adjusting for the difference between
  ;; this font's baseline and other fonts' baselines.
  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SELF) (FONT-BASELINE FONT))))
  (SEND BLINKER :SET-CURSORPOS X Y)             ;Assure blinker overlaps char

  ;; Set the blinker size to be right for this character and font.
  (TV:SHEET-SET-CURSORPOS SELF X Y) ;Needed to make TABs work right in SHEET-CHARACTER-WIDTH.
  (SEND BLINKER :SET-SIZE
;character lossage
                (MAX 1 (ABS (TV:SHEET-CHARACTER-WIDTH SELF CHAR FONT)))
                (FONT-RASTER-HEIGHT FONT)))

(DEFUN FIND-BP-IN-WINDOW-COORDS (BP WINDOW)
  "Return position (X, Y) at which BP occurs in WINDOW relative to its SHEET
or NIL NIL if BP is not on the screen.
Assumes that the window's display information is up to date."
  (CHECK-TYPE WINDOW WINDOW "a ZWEI::WINDOW (a displayer with a sheet)")
  (LET ((PLINE (FIND-BP-IN-WINDOW WINDOW BP)))
    (COND ((NULL PLINE) NIL)
          (T
           (LET ((SHEET (WINDOW-SHEET WINDOW)))
             (TV:SHEET-COMPUTE-MOTION SHEET 0 (* PLINE (TV:SHEET-LINE-HEIGHT SHEET))
                                      (BP-LINE BP) (PLINE-FROM-INDEX WINDOW PLINE)
                                      (BP-INDEX BP)))))))

;;; Cause the matching paren to flash, WINDOW-START-BP is the beginning
;;; of the window, and gets passed as a magic argument to FORWARD-SEXP
;;; to tell it not to try to go past
;;; that.
(DEFUN BLINK-MATCHING-PAREN (BLINKER WINDOW POINT WINDOW-START-BP
                             &AUX BP X Y OPEN CLOSE)
  (COND ((AND (= (LIST-SYNTAX (SETQ CLOSE (BP-CHARACTER-BEFORE POINT))) LIST-CLOSE)
              *FLASH-MATCHING-PAREN*
              ;; Don't waste time if start of defun is very very far.
              (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
                 *FLASH-MATCHING-PAREN-MAX-LINES*)
              (SETQ BP (FORWARD-SEXP POINT -1 NIL 0 WINDOW-START-BP NIL))
              (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
                  (LISP-BP-SYNTACTIC-CONTEXT (FORWARD-CHAR POINT -1 T) BP)
                (AND (NOT SLASHIFIED) (NOT COMMENT))))
         (SETQ OPEN (MAKE-CHAR (BP-CH-CHAR BP)))
         ;; checks paren type match if open paren on screen
         (AND (not (char-equal (SECOND (ass #'char-equal OPEN *MATCHING-DELIMITER-LIST*))
                               (MAKE-CHAR CLOSE 0 0)))
              ;; this is so we don't barf, the redisplay, then barf, then redisplay, then...
              (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
              (PROGN (BEEP) (FORMAT *QUERY-IO* "Non-matching parenthesis.")))
         ;; NOW move back over singlequotes.
         (SETQ BP (BACKWARD-LEADING-SINGLE-QUOTES BP WINDOW-START-BP))
         (SETQ OPEN (BP-CHAR BP))
         (COND ((MULTIPLE-VALUE-SETQ (X Y) (FIND-BP-IN-WINDOW-COORDS BP WINDOW))
                (LET* ((SHEET (WINDOW-SHEET WINDOW))
                       (FONT-MAP (TV:SHEET-FONT-MAP SHEET))
                       FONT LKT)
                  (IF (< (char-font open) (length font-map))
                      (SETQ FONT (AREF FONT-MAP (CHAR-FONT OPEN)))
                    (RETURN-FROM BLINK-MATCHING-PAREN NIL))
                  (SETQ LKT (TV:FONT-LEFT-KERN-TABLE FONT))
                  (AND LKT (SETQ X (- X (AREF LKT (CHAR-CODE OPEN)))))
                  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
                  (WITHOUT-INTERRUPTS
                    (SEND BLINKER :SET-CHARACTER (CHAR-CODE OPEN) FONT)
                    (SEND BLINKER :SET-CURSORPOS X Y)
                    (SEND BLINKER :SET-VISIBILITY :BLINK))
                  T))))
        ((AND (= (LIST-SYNTAX (SETQ OPEN (BP-CH-CHAR POINT))) LIST-OPEN)
              *FLASH-MATCHING-PAREN*
              ;; Don't waste time if start of defun is very very far.
             (< (COUNT-LINES (FORWARD-DEFUN POINT -1 T) POINT T)
                 *FLASH-MATCHING-PAREN-MAX-LINES*)
              (MULTIPLE-VALUE-BIND (NIL SLASHIFIED COMMENT)
                  (LISP-BP-SYNTACTIC-CONTEXT POINT BP)
                (AND (NOT SLASHIFIED) (NOT COMMENT)))
              (SETQ BP (FORWARD-SEXP POINT 1 NIL 0
                                     ;; Don't look past just below the bottom of the screen.
                                     (LET* ((END-LINE
                                             (PLINE-LINE *WINDOW*
                                                         (1- (WINDOW-N-PLINES *WINDOW*))))
                                            (ans
                                              (AND END-LINE
                                                   (LINE-NEXT END-LINE)
                                                   (CREATE-BP (LINE-NEXT END-LINE) 0))))
     ;** observation is, the bp-< sometimes isnt true!!! *** --rg 7/9/86
     ; It lost repeatly in two window mode with a particular buffer selected in the top window.
     ; Single window mode won, switching back to two window mode lost.
     ; Finally selecting a different buffer in the bottom window seemed to make problem go away!!
                                       (cond ((null ans) nil)
                                             ((bp-< point ans) ans)
                                             (t ;(tv:beep)
                                                nil)))
                                     NIL)))
         (SETQ CLOSE (BP-CHARACTER-BEFORE BP))
         ;; checks paren type match if open paren on screen
         (AND (not (char-equal (SECOND (ass #'char-equal (MAKE-CHAR OPEN) *MATCHING-DELIMITER-LIST*))
                   (MAKE-CHAR CLOSE)))
              ;; see above for reason for this test
              (NOT (TV:SHEET-ME-OR-MY-KID-P *WINDOW* *MODE-LINE-WINDOW*))
              (PROGN (BEEP) (FORMAT *QUERY-IO* "Non-matching parenthesis.")))
         ;; Now move past trailing singlequote-like characters.
         (DO ()
             (( (LIST-SYNTAX (BP-CHARACTER BP))) LIST-SINGLE-QUOTE)
           (IBP BP))
         (SETQ CLOSE (BP-CHARACTER-BEFORE BP))
         (COND ((PROGN (MULTIPLE-VALUE-SETQ (X Y) (FIND-BP-IN-WINDOW-COORDS (DBP BP) WINDOW)) X)
                (LET* ((SHEET (WINDOW-SHEET WINDOW))
                       (FONT-MAP (TV:SHEET-FONT-MAP SHEET))
                       FONT LKT)
                  (IF (< (char-font close) (length font-map))
                      (SETQ FONT (AREF FONT-MAP (CHAR-FONT CLOSE)))
                    (RETURN-FROM BLINK-MATCHING-PAREN NIL))
                  (SETQ LKT (TV:FONT-LEFT-KERN-TABLE FONT))
                  (AND LKT (SETQ X (- X (AREF LKT (CHAR-CODE CLOSE)))))
                  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
                  (WITHOUT-INTERRUPTS
                    (SEND BLINKER :SET-CHARACTER (CHAR-CODE CLOSE) FONT)
                    (SEND BLINKER :SET-CURSORPOS X Y)
                    (SEND BLINKER :SET-VISIBILITY :BLINK))
                  T))))
        (T (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))

;;;; Things dealing with windows

;;; Scrollable file viewing
(DEFUN VIEW-WINDOW (WINDOW &OPTIONAL STREAM RETURN-IF-NO-MORE
                    &AUX CH ATTRIBUTE-LIST FONTSP)
  "Display WINDOW, letting user scroll with Space and Overstrike, reading data from STREAM.
This is used for the editor View File, etc, commands.
STREAM may be a stream to read more data from, and get font names from,
or may be T, meaning do not alter the fonts set up in WINDOW already.
RETURN-IF-NO-MORE non-NIL means exit immediately on attempt
to scroll past the end of the data."
  (IF (EQ STREAM T)
      (SETQ STREAM NIL)
    (AND STREAM (SETQ ATTRIBUTE-LIST (FS:EXTRACT-ATTRIBUTE-LIST STREAM)))
    (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
                                       (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':BACKSPACE))
    (REDEFINE-WINDOW-TAB-NCHARS WINDOW (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':TAB-WIDTH))
    (SETQ FONTSP (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':FONTS))
    (REDEFINE-FONTS WINDOW
                    (AND STREAM
                         (SET-BUFFER-FONTS (WINDOW-INTERVAL WINDOW) FONTSP))
                    (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':VSP)))
  (UNLESS (CL:LISTP FONTSP) (SETQ FONTSP (LIST FONTSP)))
  (SETQ FONTSP (OR (CDR FONTSP)
                   (GET-ATTRIBUTE (LOCF ATTRIBUTE-LIST) ':DIAGRAM)))
  (DO ((N-LINES (1- (WINDOW-N-PLINES WINDOW)))
       (FIRST-P T NIL)
       (AT-END-P))
      (NIL)
    (MULTIPLE-VALUE-SETQ (AT-END-P STREAM)
      (VIEW-WINDOW-DISPLAY WINDOW STREAM FIRST-P FONTSP))
    (AND FIRST-P RETURN-IF-NO-MORE AT-END-P (RETURN NIL))
    (SETQ CH (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
               (SEND *STANDARD-INPUT* :TYI)))
    (typecase ch
; character lossage
      ((or character fixnum)
       (if (fixnump ch) (setq ch (int-char ch)))
       (CASE CH
         ((#/SP #/C-V #/HAND-DOWN)
          (unless at-end-p
            (CONDITION-CASE ()
                (RECENTER-WINDOW-RELATIVE WINDOW N-LINES)
              (BARF NIL))))
         ((#/BS #/M-V #/HAND-UP)
          (RECENTER-WINDOW-RELATIVE WINDOW (- N-LINES)))
         (#/C-SPACE
          (KBD-SCROLL WINDOW
                      #'(LAMBDA (IGNORE N-LINES STREAM WINDOW)
                          (REDISPLAY WINDOW :RELATIVE N-LINES)
                          (VIEW-WINDOW-DISPLAY WINDOW STREAM NIL FONTSP))
                      STREAM WINDOW))
         (#/HELP
          (FORMAT *QUERY-IO*
                  "~&~:@C or ~:@C - scroll forward screen. ~@:C or ~:@C - scroll backward screen.
~:@C enters mode where Control scrolls slowly forward and Meta scrolls back.
~:@C or ~:@C - exit.  Anything else exits and is executed as a command."
                  #/SPACE #/C-V #/OVERSTRIKE #/M-V #/C-SPACE #/RUBOUT #/ABORT))
         (T
          (OR (MEMQ CH '(#/RUBOUT #/ABORT))
;character lossage
              (SEND *STANDARD-INPUT* :UNTYI (CHAR-INT CH)))
          (RETURN NIL))))
      (t        ; should do something useful with scrolling blips
       nil)))
  (VALUES (COPY-BP (WINDOW-POINT WINDOW)) CH))

;; Needs to be rewritten
(DEFUN VIEW-WINDOW-DISPLAY (WINDOW STREAM &OPTIONAL FORCE-P FONTS-P
                            &AUX AT-END-P N-PLINES SHEET
                            LAST-BP PLINE X Y Y-POS)
  (LET ((*WINDOW* WINDOW))
    (SEND *WINDOW* :SET-BASE-TICK *TICK*)
    (SETQ N-PLINES (WINDOW-N-PLINES WINDOW)
          LAST-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
    (AND STREAM (SETQ PLINE (PLINE-OF-POINT NIL WINDOW LAST-BP))
         (LET ((ISTREAM (INTERVAL-STREAM-INTO-BP LAST-BP FONTS-P)))
           (DO ((I PLINE (1+ I))
                (LINE) (EOF))
               (( I N-PLINES))
             (MULTIPLE-VALUE (LINE EOF)
               (SEND STREAM :LINE-IN LINE-LEADER-SIZE))
             (AND LINE (SEND ISTREAM :LINE-OUT LINE))
             (AND EOF (RETURN (SETQ AT-END-P T
                                    STREAM NIL))))))
    (MUST-REDISPLAY WINDOW DIS-TEXT)
    (REDISPLAY WINDOW :POINT NIL NIL FORCE-P)
    (OR STREAM AT-END-P
        (SETQ AT-END-P (FIND-BP-IN-WINDOW WINDOW LAST-BP)))
    (SETQ SHEET WINDOW
          Y (* N-PLINES (TV:SHEET-LINE-HEIGHT SHEET)))
    (LETF (((TV:SHEET-BOTTOM-MARGIN-SIZE SHEET) 0))
      (AND AT-END-P
           (MULTIPLE-VALUE-SETQ (X Y-POS)
             (FIND-BP-IN-WINDOW-COORDS LAST-BP WINDOW)))
      ;; Use default font if there might not be room to print --More-- with current font
      (let* ((system-font (tv:screen-default-font (tv:sheet-get-screen window)))
             (window-font (aref (send window :font-map) 0))
             (more-line-font (if (> (font-char-height window-font)
                                    (font-char-height system-font))
                                 system-font
                               window-font))
             (left-margin (tv:sheet-inside-left sheet))
             (y-within-sheet (+ (tv:sheet-inside-top sheet) y)))
        (clear-line-font sheet y-within-sheet more-line-font)
        (LET ((BLINKER (WINDOW-POINT-BLINKER WINDOW)))
          (COND ((OR (NOT AT-END-P) (NULL Y-POS))
                 (setq x (- (send sheet :string-out-explicit
                                  "--More--"
                                  left-margin
                                  y-within-sheet
                                  nil nil more-line-font (tv:sheet-char-aluf sheet))
                            left-margin)
                       y-pos y)
                 (when (neq more-line-font window-font)
                   (send blinker :set-size (font-char-width more-line-font)
                         (font-char-height more-line-font))))
              (AT-END-P
               (send sheet :string-out-explicit "--Bottom--"
                     left-margin
                     y-within-sheet
                     nil nil more-line-font (tv:sheet-char-aluf sheet))))
          (SEND BLINKER :SET-CURSORPOS x y-pos)
          (SEND BLINKER :SET-VISIBILITY :BLINK))
        (VALUES AT-END-P STREAM)))))

;; Called only by VIEW-WINDOW-DISPLAY
(defun clear-line-font (sheet ypos-within-sheet font)
  (tv:prepare-sheet (sheet)
    (tv:%draw-rectangle
      (tv:sheet-inside-right sheet)
      (font-char-height font)
       (tv:sheet-inside-left sheet)
       ypos-within-sheet
       (tv:sheet-erase-aluf sheet) sheet)))

;;;??? Lossage - works only on sheet windows.
;; Goes into debugger trying to print --More-- with bigfnt files
;(DEFUN VIEW-WINDOW-DISPLAY (WINDOW STREAM &OPTIONAL FORCE-P FONTS-P
;                           &AUX AT-END-P N-PLINES SHEET
;                           LAST-BP PLINE X Y Y-POS)
;  (LET ((*WINDOW* WINDOW))
;    (SEND *WINDOW* :SET-BASE-TICK *TICK*)
;    (SETQ N-PLINES (WINDOW-N-PLINES WINDOW)
;         LAST-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
;    (AND STREAM (SETQ PLINE (PLINE-OF-POINT NIL WINDOW LAST-BP))
;        (LET ((ISTREAM (INTERVAL-STREAM-INTO-BP LAST-BP FONTS-P)))
;          (DO ((I PLINE (1+ I))
;;              (AT-LINE (BP-LINE LAST-BP))
;               (LINE) (EOF))
;              (( I N-PLINES))
;            (MULTIPLE-VALUE (LINE EOF)
;              (SEND STREAM :LINE-IN LINE-LEADER-SIZE))
;            (AND LINE (SEND ISTREAM :LINE-OUT LINE))
;            (AND EOF (RETURN (SETQ AT-END-P T
;                                   STREAM NIL))))))
;    (MUST-REDISPLAY WINDOW DIS-TEXT)
;    (REDISPLAY WINDOW :POINT NIL NIL FORCE-P)
;    (OR STREAM AT-END-P
;       (SETQ AT-END-P (FIND-BP-IN-WINDOW WINDOW LAST-BP)))
;    (SETQ SHEET WINDOW
;         Y (* N-PLINES (TV:SHEET-LINE-HEIGHT SHEET)))
;;  (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET)  ;Erase anything left over
;;                     (MULTIPLE-VALUE-BIND (NIL HEIGHT)
;;                         (SEND SHEET :LABEL-SIZE)
;;                       (- (+ (TV:SHEET-INSIDE-BOTTOM SHEET) HEIGHT) Y))
;;                     (TV:SHEET-INSIDE-LEFT SHEET) Y (TV:SHEET-ERASE-ALUF SHEET) SHEET)
;    (LETF (((TV:SHEET-BOTTOM-MARGIN-SIZE SHEET) 0))
;      (AND AT-END-P
;          (MULTIPLE-VALUE-SETQ (X Y-POS)
;            (FIND-BP-IN-WINDOW-COORDS LAST-BP WINDOW)))
;      (COND ((OR (NOT AT-END-P) (NULL Y-POS))
;            (TV:SHEET-LINE-OUT SHEET "--More--" 0 NIL 0 Y)
;            (MULTIPLE-VALUE-SETQ (X Y-POS)
;              (TV:SHEET-READ-CURSORPOS SHEET)))
;           (AT-END-P (TV:SHEET-LINE-OUT SHEET "--Bottom--" 0 NIL 0 Y)))
;      (LET ((BLINKER (WINDOW-POINT-BLINKER WINDOW)))
;       (SEND BLINKER :SET-CURSORPOS X Y-POS)
;       (SEND BLINKER :SET-VISIBILITY :BLINK))
;      (VALUES AT-END-P STREAM))))

(DEFUN KBD-SCROLL (&OPTIONAL (WINDOW TV:SELECTED-WINDOW) SCROLL-FUNCTION &REST ARGS)
  (OR SCROLL-FUNCTION
      (SETQ SCROLL-FUNCTION #'(LAMBDA (WINDOW NLINES)
                                (SEND WINDOW :SCROLL-TO NLINES :RELATIVE))))
  (DO ((N-LINES (FLOOR (* (TV:SHEET-HEIGHT WINDOW) 3) (* (TV:SHEET-LINE-HEIGHT WINDOW) 4)))
;      (WAIT-P)
       (FULL-P) (BACKWARD-P) (FORWARD-P)
       (KBD-SHIFTS))
      (NIL)
    (SETQ KBD-SHIFTS (LOGIOR SI:KBD-LEFT-SHIFTS SI:KBD-RIGHT-SHIFTS))
    (SETQ FULL-P (LDB-TEST #o0701 KBD-SHIFTS)           ;Hyper
;         WAIT-P (LDB-TEST #o0601 KBD-SHIFTS)           ;Super
          BACKWARD-P (LDB-TEST #o0501 KBD-SHIFTS)       ;Meta
          FORWARD-P (LDB-TEST #o0401 KBD-SHIFTS))       ;Ctrl
    (PROCESS-WAIT "Keyboard"
                  #'(LAMBDA (WINDOW START-TIME)
                      (OR (SEND WINDOW :LISTEN)
                          (AND (OR (NOT (ZEROP SI:KBD-LEFT-SHIFTS))
                                   (NOT (ZEROP SI:KBD-RIGHT-SHIFTS)))
                               ( (TIME-DIFFERENCE (TIME) START-TIME)
                                  1))))
                  WINDOW (TIME))
    (WHEN (SEND WINDOW :LISTEN)
      (RETURN NIL))
    (WHEN (OR FORWARD-P BACKWARD-P)
      (APPLY SCROLL-FUNCTION WINDOW (* (IF BACKWARD-P -1 1) (IF FULL-P N-LINES 1)) ARGS))
    (IF FULL-P (PROCESS-SLEEP 60.))))

;;; Given a window, prints out its contents for debugging to *STANDARD-OUTPUT*.
(DEFUN PRINT-WINDOW (WINDOW)
  (DO ((PLINE 0 (1+ PLINE))
       (N-PLINES (WINDOW-N-PLINES WINDOW)))
      (( PLINE N-PLINES))
    (FORMAT T "#~S Tick=~S  From ~S to ~S  Marking from ~S width ~S.  Text width ~S~%"
            PLINE (PLINE-TICK WINDOW PLINE) (PLINE-FROM-INDEX WINDOW PLINE)
            (PLINE-TO-INDEX WINDOW PLINE) (PLINE-MARKING-LEFT WINDOW PLINE)
            (PLINE-MARKING-WIDTH WINDOW PLINE) (PLINE-TEXT-WIDTH WINDOW PLINE))
    (FORMAT T "~S~%" (PLINE-LINE WINDOW PLINE))))

(DEFUN CHECK-INTERVAL-LINES (BP1 &OPTIONAL BP2 IN-ORDER-P TICK)
  "Verify that the lines in specified interval are linked together properly."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((PREV NIL LINE)
       (LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE BP2))
       (PREV-1) (BAD-POINTERS-P) (DELETED-P) (LINE-TICK))
      (NIL)
    (SETQ PREV-1 (IF LINE (LINE-PREVIOUS LINE) LAST-LINE)
          LINE-TICK (AND LINE (LINE-TICK LINE)))
    (SETQ BAD-POINTERS-P (NEQ PREV PREV-1)
          DELETED-P (AND LINE-TICK (EQ LINE-TICK 'DELETED)))
    (WHEN (OR BAD-POINTERS-P DELETED-P)
      (FORMAT T "~&Line: ~S~%" LINE)
      (IF BAD-POINTERS-P
          (FORMAT T " line previous of line ~S, previous line ~S~%" PREV-1 PREV))
      (IF (OR DELETED-P (AND TICK LINE-TICK (> LINE-TICK TICK)))
          (FORMAT T " line is ~:[modified~;deleted~]" DELETED-P)))
    (WHEN (OR (EQ PREV LAST-LINE) (NULL LINE))
      (RETURN NIL))))

(DEFUN CHECK-BUFFER-LINES (BUFFER)
  "Verify that the lines in BUFFER are linked together properly."
  (CHECK-INTERVAL-LINES BUFFER NIL T (AND (BUFFER-FILE-ID BUFFER) (BUFFER-TICK BUFFER))))

(DEFUN CHECK-ALL-BUFFER-LINES ()
  "Verify that the lines in all ZMACS buffers are linked together properly."
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (FORMAT T "~2&~A:~%" (BUFFER-NAME BUFFER))
    (CHECK-BUFFER-LINES BUFFER)))

;;;; Prompt line and typein line.

(DEFUN PROMPT-LINE (STRING &REST ARGS)
  "Pass args to FORMAT, outputting to the mode line."
  (SEND *MODE-LINE-WINDOW* :CLOBBER)
  (TV:SHEET-HOME *MODE-LINE-WINDOW*)
  (TV:SHEET-CLEAR-EOL *MODE-LINE-WINDOW*)
  (CATCH 'MODE-LINE-OVERFLOW
    (APPLY #'FORMAT *MODE-LINE-WINDOW* STRING ARGS)))

(DEFUN PROMPT-LINE-MORE (STRING &REST ARGS)
  "Pass args to FORMAT, outputting to mode line in addition to stuff already there."
  (SEND *MODE-LINE-WINDOW* :CLOBBER)
  (WHEN (STRING-EQUAL STRING "~&" :END1 2)
    (TV:SHEET-HOME *MODE-LINE-WINDOW*)
    (TV:SHEET-CLEAR-EOL *MODE-LINE-WINDOW*))
  (CATCH 'MODE-LINE-OVERFLOW
    (APPLY #'FORMAT *MODE-LINE-WINDOW* STRING ARGS)))

(DEFUN PROMPT-LINE-WITH-REDISPLAY (STRING &REST ARGS)
  "Redisplay *WINDOW* and then FORMAT STRING and ARGS in the mode line."
  (REDISPLAY *WINDOW* :NONE)
  (APPLY #'PROMPT-LINE STRING ARGS))

(DEFUN TYPEIN-LINE-PREPARE ()
  (SEND *QUERY-IO* :SEND-IF-HANDLES :PREPARE-FOR-TYPEOUT))

(DEFUN TYPEIN-LINE (STRING &REST ARGS)
  "Pass args to FORMAT, outputting to the typein window."
  (SEND *QUERY-IO* :FRESH-LINE)
  (APPLY #'FORMAT *QUERY-IO* STRING ARGS))

(DEFUN TYPEIN-LINE-MORE (STRING &REST ARGS)
  "Pass args to FORMAT, outputting to the typein window, adding to stuff already there."
  (APPLY #'FORMAT *QUERY-IO* STRING ARGS))

(DEFUN TYPEIN-LINE-CHAR-OR-STRING (STRING)
  "Print a prompt in the typein window.
If STRING is a string, it is just output.
If a character, it is printed verbosely, with a space after."
  (IF (STRINGP STRING) (PRINC STRING *QUERY-IO*)
    (LET ((*STANDARD-OUTPUT* *QUERY-IO*))
      (FORMAT:OCHAR STRING :EDITOR))))

(DEFUN TYPEIN-LINE-WITH-REDISPLAY (STRING &REST ARGS)
  "Redisplay *WINDOW*, then pass args to FORMAT, outputting in typein window."
  (AND (WINDOW-READY-P *WINDOW*)        ;E.g. searching inside mini-buffer
       (REDISPLAY *WINDOW* :NONE))
  (FORMAT *QUERY-IO* STRING ARGS))

;;; TYPEIN-LINE-ACTIVATE is in MACROS

(DEFUN TYPEIN-LINE-READLINE (CTL-STRING &REST ARGS)
  "Read a line with the mini buffer, prompting by passing ARGS to FORMAT."
  (APPLY #'TYPEIN-LINE-READLINE-WITH-DEFAULT NIL CTL-STRING ARGS))

(DEFUN TYPEIN-LINE-READLINE-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read a line with the mini buffer with default DEFAULT. Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
                   CTL-STRING
                   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (STRING-INTERVAL (NTH-VALUE 2
                     (EDIT-IN-MINI-BUFFER *MINI-BUFFER-COMTAB*
                                          DEFAULT NIL
                                          (AND PROMPT (NCONS PROMPT))))))

(DEFUN TYPEIN-LINE-MULTI-LINE-READLINE (CTL-STRING &REST ARGS)
  "Read any text (terminated by End) in the minibuffer.
Prompts by passing ARGS to FORMAT."
  (APPLY #'TYPEIN-LINE-MULTI-LINE-READLINE-WITH-DEFAULT NIL CTL-STRING ARGS))

(DEFUN TYPEIN-LINE-MULTI-LINE-READLINE-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read any text (terminated by End) in the minibuffer with default DEFAULT.
Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
                   CTL-STRING
                   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (STRING-INTERVAL (NTH-VALUE 2
                     (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB*
                                          DEFAULT NIL
                                          (AND PROMPT (NCONS PROMPT))))))

(DEFUN TYPEIN-LINE-READ (CTL-STRING &REST ARGS)
  "Read an s-expression in the minibuffer; Return terminates.
Prompts by passing ARGS to FORMAT."
  (APPLY #'TYPEIN-LINE-READ-WITH-DEFAULT NIL CTL-STRING ARGS))

(defvar *typein-line-eof-value* '*eof*
  "Bind this to specify (and therefore reliably test for) EOF value from TYPEIN-LINE-READs.")

(DEFUN TYPEIN-LINE-READ-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read an s-expression in the minibuffer; Return terminates; default value DEFAULT.
Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
                   CTL-STRING
                   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (CL:READ (INTERVAL-STREAM (NTH-VALUE 2
                              (EDIT-IN-MINI-BUFFER *MINI-BUFFER-COMTAB*
                                                   DEFAULT NIL
                                                   (AND PROMPT (NCONS PROMPT)))))
           NIL *typein-line-eof-value*))

(DEFUN TYPEIN-LINE-MULTI-LINE-READ (CTL-STRING &REST ARGS)
  "Read an s-expression in the minibuffer; Return does not terminate.
Prompts by passing ARGS to FORMAT."
  (APPLY #'TYPEIN-LINE-MULTI-LINE-READ-WITH-DEFAULT NIL CTL-STRING ARGS))

(DEFUN TYPEIN-LINE-MULTI-LINE-READ-WITH-DEFAULT (DEFAULT CTL-STRING &REST ARGS &AUX PROMPT)
  "Read an s-expression in the minibuffer; Return does not terminate; default value DEFAULT.
Prompts by passing ARGS to FORMAT."
  (SETQ PROMPT (IF (NULL ARGS)
                   CTL-STRING
                   (APPLY #'FORMAT NIL CTL-STRING ARGS)))
  (CL:READ (INTERVAL-STREAM (NTH-VALUE 2
                              (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB*
                                                   DEFAULT NIL
                                                   (AND PROMPT (NCONS PROMPT)))))
           NIL *typein-line-eof-value*))


;;;Miscellanous TYPEOUT functions - need more standardization here!!

(DEFUN TYPEOUT-BEEP-YES-OR-NO-P (&REST FORMAT-ARGS)
  (LET ((*QUERY-IO* *TYPEOUT-WINDOW*))
    (APPLY 'FQUERY '#,`(:SELECT T
                         :BEEP T
                         :TYPE :READLINE
                         :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
           FORMAT-ARGS)))

(DEFUN TYPEOUT-YES-OR-NO-P (&REST FORMAT-ARGS)
  "Ask the user Yes or No, printing in the typeout window."
  (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
    (APPLY 'FQUERY '#,`(:TYPE :READLINE
                              :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
           FORMAT-ARGS)))

;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Primitive data structure manipulation for ZWEI.
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN CREATE-LINE (ARRAY-TYPE SIZE NODE)
  "Create a line with room for SIZE characters before expansion.
ARRAY-TYPE should be ART-FAT-STRING if you wish to store nonzero fonts,
 otherwise ART-STRING.
NODE should be the node in which this line will become a part of."
  (MAKE-LINE :MAKE-ARRAY (:AREA *ZWEI-LINE-AREA* :TYPE ARRAY-TYPE :LENGTH SIZE)
             :LINE-TICK *TICK*
             :LINE-LENGTH SIZE
             :LINE-NODE NODE))

(DEFUN CREATE-BP (LINE INDEX &OPTIONAL STATUS)
  "Create a buffer pointer to position INDEX in LINE.
STATUS is the type of relocation that should be done:
 :NORMAL means insertion at this pointer leaves the pointer
   before the inserted text,
 :MOVES  means insertion at this pointer leaves the pointer
   after the inserted text,
 NIL or omitted means that the buffer pointer is not relocated
   when editing is done."
  (IF STATUS
      (LET ((BP (MAKE-BP :BP-LINE LINE :BP-INDEX INDEX :BP-STATUS STATUS)))
        (PUSH BP (LINE-BP-LIST LINE))
        BP)
    (MAKE-TEMP-BP :BP-LINE LINE :BP-INDEX INDEX)))

(DEFUN COPY-BP (BP &OPTIONAL STATUS)
  "Make a new buffer pointer with the same line and index as BP.
STATUS specifies how the new buffer pointer is to be relocated by editing;
see CREATE-BP."
  (CREATE-BP (BP-LINE BP) (BP-INDEX BP) STATUS))

(DEFUN CREATE-INTERVAL (&OPTIONAL ARG1 ARG2 (NODE-P T)
                        &AUX INTERVAL (*BATCH-UNDO-SAVE* T))
  "Create an interval.  ARG1 and ARG2 specify initial contents.
NODE-P says whether to make the interval a node; default is T.
NODE-P can also be an uninitialized interval object of any type
 which is initialized by this function.
If ARG1 and ARG2 are NIL, the interval is empty.
If ARG1 is non-NIL, it should be a string, whose contents initialize the interval.
If both ARG1 and ARG2 are non-NIL, they are two buffer pointers
and the text between them is copied."
  (SETQ INTERVAL (IF (SYMBOLP NODE-P)
                     (IF NODE-P
                         (MAKE-NODE)
                       (MAKE-INTERVAL))
                   NODE-P))
  (OR ARG2
      (LET ((LINE (CREATE-LINE 'ART-STRING 0 INTERVAL)))
        (SETF (INTERVAL-FIRST-BP INTERVAL) (CREATE-BP LINE 0 ':NORMAL))
        (SETF (INTERVAL-LAST-BP INTERVAL) (CREATE-BP LINE 0 ':MOVES))))
  (AND ARG1
       (COND (ARG2
              (SETF (INTERVAL-FIRST-BP INTERVAL) ARG1)
              (SETF (INTERVAL-LAST-BP INTERVAL) ARG2))
             (T
              (INSERT (INTERVAL-FIRST-BP INTERVAL) ARG1))))
  INTERVAL)

(DEFUN MOVE-BP (BP LINE &OPTIONAL INDEX &AUX OLINE)
  "Move buffer pointer BP to point somewhere else.
Either LINE is a line and INDEX an index in it,
or LINE is another BP to be copied."
  (SETQ OLINE (BP-LINE BP))
  (COND ((NULL INDEX)
         (SETQ INDEX (BP-INDEX LINE) LINE (BP-LINE LINE)))
        ;; If we were not passed a BP, check that the INDEX is in range.
        ((> INDEX (LINE-LENGTH LINE))
         (FERROR NIL "The index ~O is greater than the length of the line ~S"
                 INDEX LINE)))
  (COND ;; If it is to the same line, there can be no problem.
    ((EQ OLINE LINE)
     (SETF (BP-INDEX BP) INDEX))
    (T
     (COND ((BP-STATUS BP)
            ;; It is a permanent bp changing lines.  Fix relocation lists.
            (SETF (LINE-BP-LIST OLINE) (DELQ BP (LINE-BP-LIST OLINE)))
            (PUSH BP (LINE-BP-LIST LINE))))
     (SETF (BP-LINE BP) LINE)
     (SETF (BP-INDEX BP) INDEX)))
  BP)

(DEFUN DBP (BP &OPTIONAL FIXUP-P)
  "Move BP back one character and return BP.
At beginning of interval, return BP unchanged if FIXUP-P is T,
otherwise return NIL."
  (COND ((BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
         (AND FIXUP-P BP))
        ((= (BP-INDEX BP) 0)
         (MOVE-BP BP (LINE-PREVIOUS (BP-LINE BP))
                  (LINE-LENGTH (LINE-PREVIOUS (BP-LINE BP)))))
        (T (MOVE-BP BP (BP-LINE BP) (1- (BP-INDEX BP))))))

(DEFUN IBP (BP &OPTIONAL FIXUP-P)
  "Move BP forward one character and return BP.
At end of interval, return BP unchanged if FIXUP-P is T,
otherwise return NIL."
  (COND ((BP-= BP (INTERVAL-LAST-BP *INTERVAL*))
         (AND FIXUP-P BP))
        ((= (BP-INDEX BP) (LINE-LENGTH (BP-LINE BP)))
         (MOVE-BP BP (LINE-NEXT (BP-LINE BP)) 0))
        (T (MOVE-BP BP (BP-LINE BP) (1+ (BP-INDEX BP))))))

(DEFUN TICK ()
  "Increment the event counter *TICK*; return the new value."
  (SETQ *TICK* (1+ *TICK*)))

(DEFUN MUNG-LINE (LINE)
  "Mark LINE as having been changed at the current tick."
  (SETF (LINE-CONTENTS-PLIST LINE) NIL)
  (SETF (LINE-TICK LINE) (TICK)))

(DEFUN MUNG-BP-INTERVAL (BP)
  "Mark as changed the interval BP points into.
Call this before making the changes; it will get an error
if the interval is read-only."
  (TICK)
  (MUNG-NODE (BP-NODE BP)))

(DEFUN MUNG-BP-LINE-AND-INTERVAL (BP &AUX (LINE (BP-LINE BP)))
  "Mark the line BP points to as changed, and the interval the line is in.
Call this before making the changes; it will get an error
if the interval is read-only."
  (TICK)
  (AND (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) (BARF "Diagram line"))
  (MUNG-NODE (BP-NODE BP))
  (SETF (LINE-TICK LINE) *TICK*)
  (SETF (LINE-CONTENTS-PLIST LINE) NIL))

(DEFUN BP-NODE (BP &AUX LINE)
  "Return the node that BP points into."
  (SETQ LINE (BP-LINE BP))
  (DO ((NODE (LINE-NODE LINE) (OR (NODE-NEXT NODE) (NODE-SUPERIOR NODE)))
       (INDEX (BP-INDEX BP))
       (LAST-BP))
      ((OR (NEQ LINE (BP-LINE (SETQ LAST-BP (INTERVAL-LAST-BP NODE))))
           ( (BP-INDEX LAST-BP) INDEX))
       NODE)))

(DEFUN BP-TOP-LEVEL-NODE (BP)
  "Return the top-level node that BP points into.
If the BP-NODE of BP is an inferior of another node,
we return the latter one."
  (DO ((NODE (BP-NODE BP) SUPERIOR)
       (SUPERIOR))
      ((NULL (SETQ SUPERIOR (NODE-SUPERIOR NODE)))
       NODE)))

(DEFUN MUNG-NODE (NODE &AUX SUPERIOR)
  "Mark node NODE as modified.  Gets an error if the node is read-only."
  (AND (SETQ SUPERIOR (NODE-SUPERIOR NODE))
       (MUNG-NODE SUPERIOR))                    ;Do up first in case read only
  (AND (NODE-READ-ONLY-P NODE)
       (NEQ NODE *READ-ONLY-SUPPRESSED-INTERVAL*)
       (BARF "Read-only"))
  (SETF (NODE-TICK NODE) *TICK*))

;;; Return T if X is after or at LINE, NIL if X is before LINE.
(DEFUN SEARCH-FOR-LINE (X LINE)
  "Return T if the line X is after the line LINE (or the same line).
Assumes that they are in the same node, and therefore are in some order."
  (DO ((FORWARD LINE (LINE-NEXT FORWARD))
       (BACKWARD (LINE-PREVIOUS LINE) (LINE-PREVIOUS BACKWARD)))
      (NIL)
    (COND ((EQ X FORWARD) (RETURN T))
          ((EQ X BACKWARD) (RETURN NIL))
          ((NULL FORWARD) (RETURN NIL))
          ((NULL BACKWARD) (RETURN T)))))

(DEFUN BEG-LINE-P (BP)
  "T if BP points at the beginning of a line."
  (OR (= (BP-INDEX BP) 0)
      (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))))

(DEFUN END-LINE-P (BP)
  "T if BP points at the end of a line."
  (OR (= (BP-INDEX BP) (LINE-LENGTH (BP-LINE BP)))
      (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))))

(DEFUN BEG-OF-LINE (LINE)
  "Return a BP to the beginning of LINE.
If *INTERVAL* includes only part of LINE,
return a BP to the beginning of the included part."
  (CREATE-BP LINE (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
                      (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
                      0)))

(DEFUN END-OF-LINE (LINE)
  "Return a BP to the end of LINE.
If *INTERVAL* includes only part of LINE,
return a BP to the end of the included part."
  (CREATE-BP LINE (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                      (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
                      (LINE-LENGTH LINE))))

(DEFUN COUNT-LINES (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Return the number of lines in an interval."
  (COND ((NULL TO-BP)
         (SETQ TO-BP (INTERVAL-LAST-BP FROM-BP)
               FROM-BP (INTERVAL-FIRST-BP FROM-BP))))
  (OR IN-ORDER-P (ORDER-BPS FROM-BP TO-BP))
  (DO ((LINE (BP-LINE FROM-BP) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE TO-BP))
       (I 1 (1+ I)))
      ((EQ LINE LAST-LINE) I)))

(DEFUN COUNT-OBJECTS (MOVER FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Return the number of objects (as moved over by MOVER) in an interval.
MOVER is a standard motion function such as FORWARD-WORD."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((*INTERVAL*
          (MAKE-INTERVAL FROM-BP TO-BP)))
    (DO ((BP FROM-BP (FUNCALL MOVER BP 1 NIL))
         (COUNT -1 (1+ COUNT)))
        ((NULL BP) COUNT))))

(DEFUN COUNT-CHARS (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Return the number of characters in an interval."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((FIRST-LINE (BP-LINE FROM-BP))
        (FIRST-INDEX (BP-INDEX FROM-BP))
        (LAST-LINE (BP-LINE TO-BP))
        (LAST-INDEX (BP-INDEX TO-BP)))
    (COND ((EQ FIRST-LINE LAST-LINE)
           (- LAST-INDEX FIRST-INDEX))
          (T (DO ((LINE (LINE-NEXT FIRST-LINE) (LINE-NEXT LINE))
                  (I 1 (+ 1 I (LINE-LENGTH LINE))))
                 ((EQ LINE LAST-LINE)
                  (+ I (- (LINE-LENGTH FIRST-LINE) FIRST-INDEX) LAST-INDEX)))))))

(DEFUN LINE-N-CHARS (LINE)
  "Return the number of characters in LINE.
If *INTERVAL* includes only part of LINE, only that part is counted."
  (LET ((FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
        (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (- (IF (EQ LINE (BP-LINE LAST-BP))
           (BP-INDEX LAST-BP)
           (LINE-LENGTH LINE))
       (IF (EQ LINE (BP-LINE FIRST-BP))
           (BP-INDEX FIRST-BP)
           0))))

(DEFUN SWAP-BPS (BP1 BP2)
  "Moves each of BP1 and BP2 to the other's position."
  (LET ((LINE (BP-LINE BP1))
        (INDEX (BP-INDEX BP1)))
    (MOVE-BP BP1 BP2)
    (MOVE-BP BP2 LINE INDEX)))

(DEFUN FLUSH-BP (BP)
  "Turn off relocation of BP.
Do this to a relocatable BP when it is no longer needed,
or else it will hang around forever in the buffer data structure
slowing things down."
  (LET ((LINE (BP-LINE BP)))
    (SETF (LINE-BP-LIST LINE) (DELQ BP (LINE-BP-LIST LINE) 1))))


(DEFUN BP-< (BP1 BP2)
  "T if BP1 comes before BP2 in the interval they point into."
  (LET ((LINE1 (BP-LINE BP1))
        (LINE2 (BP-LINE BP2)))
    (COND ((EQ LINE1 LINE2)
           (< (BP-INDEX BP1) (BP-INDEX BP2)))
          (T (NOT (SEARCH-FOR-LINE LINE1 LINE2))))))

(DEFUN BP-IN-INTERVAL-P (BP INTERVAL-OR-BP1 &OPTIONAL BP2 IN-ORDER-P)
  "T if BP is inside (or at an end of) the interval specifled.
Either pass an interval object, or two BPs.
IN-ORDER-P says that BP2 follows BP1."
  (GET-INTERVAL INTERVAL-OR-BP1 BP2 IN-ORDER-P)
  (COND ((OR (EQ (BP-LINE BP) (BP-LINE INTERVAL-OR-BP1))
             (EQ (BP-LINE BP) (BP-LINE BP2)))
         (NOT (OR (AND (EQ (BP-LINE BP) (BP-LINE INTERVAL-OR-BP1))
                       (< (BP-INDEX BP) (BP-INDEX INTERVAL-OR-BP1)))
                  (AND (EQ (BP-LINE BP) (BP-LINE BP2))
                       (> (BP-INDEX BP) (BP-INDEX BP2))))))
        (T
         (DO ((LINE (BP-LINE INTERVAL-OR-BP1) (LINE-NEXT LINE))
              (BP-LINE (BP-LINE BP))
              (STOP-LINE (BP-LINE BP2)))
             ((EQ LINE STOP-LINE) NIL)
           (IF (EQ LINE BP-LINE) (RETURN T))))))

(DEFUN BP-= (BP1 BP2)
  "T if BP1 and BP2 point to the same place."
  (AND (EQ (BP-LINE BP1) (BP-LINE BP2))
       (= (BP-INDEX BP1) (BP-INDEX BP2))))


(DEFUN BP-CHARACTER (BP)
  "Return the character after where BP points."
  (LET ((LINE (BP-LINE BP))
        (INDEX (BP-INDEX BP)))
    (IF (= INDEX (LINE-LENGTH LINE))
        #/NEWLINE
        (CL:AREF LINE INDEX))))
(DEFUN BP-CHAR (BP)
  "Return INT-CHAR of the character after where BP points."
  (LET ((LINE (BP-LINE BP))
        (INDEX (BP-INDEX BP)))
    (IF (= INDEX (LINE-LENGTH LINE))
        #/NEWLINE
        (GLOBAL:AREF LINE INDEX))))

(DEFUN BP-CHARACTER-BEFORE (BP)
  "Return the character before where BP points."
  (LET ((INDEX (BP-INDEX BP)))
    (IF (ZEROP INDEX)
        #/NEWLINE
        (CL:AREF (BP-LINE BP) (1- INDEX)))))
(DEFUN BP-CHAR-BEFORE (BP)
  "Return CHAR-INT of the character before where BP points."
  (LET ((INDEX (BP-INDEX BP)))
    (IF (ZEROP INDEX)
        #/NEWLINE
        (GLOBAL:AREF (BP-LINE BP) (1- INDEX)))))


;;; Returns either NIL or the thing it deleted.
(DEFUN DELETE-LAST-ELEMENT (LIST)
  "Delete the last element of LIST, destructively."
  (AND (> (LENGTH LIST) 1)
       (DO ((L LIST (CDR L)))
           ((NULL (CDDR L))
            (PROG1 (CADR L) (RPLACD L NIL))))))

(DEFUN POINT-PDL-PUSH (BP WINDOW &OPTIONAL EXPLICIT (NOTIFICATION T) &AUX TEM)
  "Push BP on the ring of saved points in WINDOW.
EXPLICIT = T says this was explicitly requested by the user.
NOTIFICATION = NIL inhibits printing /"Point Pushed/"."
  (SETQ TEM (LIST (COPY-BP BP ':NORMAL) (PLINE-OF-POINT T WINDOW BP)))
  (AND EXPLICIT (SETQ TEM (NCONC TEM (NCONS EXPLICIT))))
  (PUSH TEM (WINDOW-POINT-PDL WINDOW))
  (AND (> (LENGTH (WINDOW-POINT-PDL WINDOW)) *POINT-PDL-MAX*)
       (FLUSH-BP (CAR (DELETE-LAST-ELEMENT (WINDOW-POINT-PDL WINDOW)))))
  (AND NOTIFICATION (SEND *TYPEIN-WINDOW* ':EXPOSED-P)
       (PROGN
         (SEND *TYPEIN-WINDOW* ':FRESH-LINE)
         (FORMAT *TYPEIN-WINDOW* *AUTO-PUSH-POINT-NOTIFICATION*))))

;;; Rotate nth (1-origin!) element to the front of the list, rotating the
;;; part of the list before it.  With a negative arg rotate the same amount
;;; backwards.  With an arg of 1 rotate the whole list BACKWARDS, i.e. bring
;;; up the same element as with an arg of 2 but store the old front at the back.
;;; Zero arg is undefined, do nothing I guess.  Note that 2 and -2 do the same thing.
;;; Doesn't barf if N is too big.  Alters the list in place.
(DEFUN ROTATE-TOP-OF-LIST (LIST N)
  "Modify LIST, permuting N elements cyclically.
N may be of either sign; positive brings elements toward the front.
If N is 1, rotate the whole list backwards."
  (AND (= (ABS N) 1) (SETQ N (* N -1 (LENGTH LIST))))
  (COND ((PLUSP N)
         (SETQ N (MIN (LENGTH LIST) N))
         (DO ((I 0 (1+ I))
              (LIST LIST (CDR LIST))
              (NTH (NTH (1- N) LIST) OLD)
              (OLD))
             (( I N))
           (SETQ OLD (CAR LIST))
           (SETF (CAR LIST) NTH)))
        ((MINUSP N)
         (SETQ N (MIN (LENGTH LIST) (MINUS N)))
         (DO ((I 1 (1+ I))
              (LIST LIST (CDR LIST))
              (FRONT (CAR LIST)))
             (( I N) (SETF (CAR LIST) FRONT))
           (SETF (CAR LIST) (CADR LIST)))))
  LIST)

(DEFUN POINT-PDL-POP (WINDOW)
  "Pop the last pointer off the point pdl of WINDOW and return its elements as values.
The first value is the saved pointer,
the second is the screen line it was displayed at,
the third is whether it was pushed explicitly."
  (LET ((PDL (WINDOW-POINT-PDL WINDOW)))
    (OR PDL (BARF))
    (LET ((ENTRY (CAR PDL)))
      (SETF (WINDOW-POINT-PDL WINDOW) (NCONC (CDR PDL) (RPLACD PDL NIL)))
      (VALUES-LIST ENTRY))))

(DEFUN POINT-PDL-EXCH (BP WINDOW ARG-P ARG &AUX PDL ENTRY)
  (SETQ PDL (WINDOW-POINT-PDL WINDOW))
  (AND (EQ ARG-P ':CONTROL-U) (SETQ ARG 0))
  (DO ((ARG (ABS ARG))
       (PDL (IF (MINUSP ARG) (REVERSE PDL) PDL) (CDR PDL))
       (ENT))
      ((OR (< ARG 0) (NULL PDL))
       (SETQ ENTRY (OR ENT (BARF))))
    (COND ((THIRD (CAR PDL))
           (SETQ ENT (CAR PDL)
                 ARG (1- ARG)))))
  (SETF (WINDOW-POINT-PDL WINDOW)
        (CONS (LIST (COPY-BP BP ':NORMAL) (PLINE-OF-POINT NIL WINDOW BP) T)
              (NCONC (DELQ ENTRY (DEL #'BP-= BP PDL)) (NCONS ENTRY))))
  (VALUES-LIST ENTRY))

(DEFUN POINT-PDL-MOVE (BP PLINE)
  "Move (POINT) to BP and display it at PLINE (if PLINE is non-NIL)."
  (LET ((INTERVAL (BP-TOP-LEVEL-NODE BP)))
    (OR (EQ INTERVAL (BP-TOP-LEVEL-NODE (INTERVAL-FIRST-BP *INTERVAL*)))
        (SEND INTERVAL ':SELECT)))
  (MOVE-BP (POINT) BP)
  (AND PLINE (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE)))

(DEFUN POINT-PDL-PURGE (BUFFER)
  "Remove all pointers to BUFFER from the point pdl."
  (DOLIST (WINDOW (SEND BUFFER ':POSSIBLE-WINDOWS))
    (SETF (WINDOW-POINT-PDL WINDOW)
          (DEL #'(LAMBDA (BUF POINT) (EQ BUF (BP-TOP-LEVEL-NODE (FIRST POINT)))) BUFFER
               (WINDOW-POINT-PDL WINDOW)))))

(DEFUN ROTATE-POINT-PDL (WINDOW N &AUX POINT ENTRY LIST)
  "Rotate the point pdl of WINDOW by N.
The current (POINT) is taken as part of the pdl for rotation purposes
even though it is not actually stored in the point pdl."
  (SETQ POINT (WINDOW-POINT WINDOW)
        ENTRY (LIST (COPY-BP POINT ':NORMAL) (PLINE-OF-POINT T WINDOW POINT) T)
        LIST (CONS ENTRY (WINDOW-POINT-PDL WINDOW)))
  (ROTATE-TOP-OF-LIST LIST N)
  (SETQ ENTRY (CAR LIST))
  (POINT-PDL-MOVE (CAR ENTRY) (CADR ENTRY))
  DIS-BPS)

(DEFUN MAYBE-PUSH-POINT (BP)
  "Push BP on the point pdl if it is far enough from (POINT)."
  (AND *AUTO-PUSH-POINT-OPTION*
       (BPS-FAR-APART BP (POINT) *AUTO-PUSH-POINT-OPTION*)
       (POINT-PDL-PUSH BP *WINDOW*)))

(DEFUN IN-CURRENT-FONT (X &OPTIONAL (FONT *FONT*))
  "Return string or character X, converted to font FONT."
;character lossage
  (IF (NUMBERP X) (SETQ X (INT-CHAR X)))
  (COND ((MEMQ X '(#/NEWLINE #/TAB))          ;These characters are fontless
         X)
        ((CHARACTERP X)
         (COND ((MAKE-CHAR X (CHAR-BITS X) FONT))
               (T X)))          ;cant win, try not to lose.
        ((ZEROP FONT)                           ;Little efficiency for strings
         X)
        (T (LET* ((LENGTH (STRING-LENGTH X))
                  (S (MAKE-ARRAY LENGTH ':TYPE 'ART-FAT-STRING)))
             (DO ((I 0 (1+ I))
                  CHAR)
                 (( I LENGTH) S)
               (SETQ CHAR (CL:AREF X I))
               (SETF (CL:AREF S I) (MAKE-CHAR CHAR (CHAR-BITS CHAR) FONT)))))))

(DEFUN LINE-BLANK-OR-DIAGRAM-P (LINE)
  "T if LINE is blank or is a diagram line."
  (OR (GETF (LINE-PLIST LINE) ':DIAGRAM)
      (LINE-BLANK-P LINE)))

(DEFUN LINE-BLANK-P (LINE)
  "T if LINE is a blank line; NIL for diagrams."
;character lossage
  (IF (NUMBERP LINE) (SETQ LINE (INT-CHAR LINE)))
  (COND ((CHARACTERP LINE)
         (MEMQ (CHAR-CODE LINE) *BLANKS*))
        ((GETF (LINE-PLIST LINE) ':DIAGRAM) NIL)
        (T
         (DO ((I (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
                     (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
                     0)
                 (1+ I))
              (LIM (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                       (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
                       (LINE-LENGTH LINE))))
             (( I LIM) T)
;character lossage
           (OR (MEMQ (CHAR-CODE (CL:AREF LINE I)) *BLANKS*)
               (RETURN NIL))))))

(DEFUN RANGE (X MIN MAX)
  "X if between MIN and MAX; else either MIN or MAX, whichever is closer."
  (MAX MIN (MIN MAX X)))

(DEFUN CLEAN-POINT-PDL (WINDOW)
  "Remove any pointers from the point pdl that are the same as (POINT)."
  ;; But always leave at least one element.
  (DO ((L (WINDOW-POINT-PDL WINDOW) (CDR L))
       (PT (WINDOW-POINT WINDOW)))
      ((OR (NULL (CDR L))
           (NOT (BP-= (CAAR L) PT)))
       (SETF (WINDOW-POINT-PDL WINDOW) L))
    (FLUSH-BP (CAAR L))))

(DEFUN STRING-MATCH (PATTERN SUBJECT)
  "T if the string SUBJECT begins with the string PATTERN."
  (LET ((PATTERN-LENGTH (STRING-LENGTH PATTERN)))
    (COND ((AND ( (STRING-LENGTH SUBJECT) PATTERN-LENGTH)
                (STRING-EQUAL PATTERN SUBJECT
                              :START1 0 :START2 0 :END1 PATTERN-LENGTH :END2 PATTERN-LENGTH))
           PATTERN-LENGTH)
          (T NIL))))

(DEFUN LINE-TYPE (LINE)
  "Return a keyword classifying LINE's contents.
The value is :BLANK, :COMMENT, :ATOM, :NORMAL or :DIAGRAM."
  (IF (GETF (LINE-PLIST LINE) ':DIAGRAM)
      ':DIAGRAM
    (DO ((I (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
                (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
              0)
            (1+ I))
         (LIM (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                  (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
                (LINE-LENGTH LINE))))
        (( I LIM) ':BLANK)
      (LET* ((CH (CL:AREF LINE I))
             (CODE (CHAR-CODE CH)))
;character lossage
        (OR (MEMQ CODE *BLANKS*)
            (RETURN (COND ((CHAR-EQUAL CH #/;) ':COMMENT)
                          ((CHAR-EQUAL CH #/FF) ':FORM)
                          ((= (LIST-SYNTAX CH) LIST-ALPHABETIC) ':ATOM)
                          (T ':NORMAL))))))))

(DEFUN UPCASE-CHAR (BP)
  "Convert the character BP points to to upper case, modifying the text."
  (LET ((LINE (BP-LINE BP))
        (INDEX (BP-INDEX BP)))
    (WHEN (< INDEX (LINE-LENGTH LINE))
      (MUNG-BP-LINE-AND-INTERVAL BP)
      (SETF (CL:AREF LINE INDEX) (CHAR-UPCASE (CL:AREF LINE INDEX))))))

(DEFUN DOWNCASE-CHAR (BP)
  "Convert the character BP points to to lower case, modifying the text."
  (LET ((LINE (BP-LINE BP))
        (INDEX (BP-INDEX BP)))
    (WHEN (< INDEX (LINE-LENGTH LINE))
      (MUNG-BP-LINE-AND-INTERVAL BP)
      (SETF (CL:AREF LINE INDEX) (CHAR-DOWNCASE (CL:AREF LINE INDEX))))))

(DEFUN UPCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Uppercasify all characters in the specified interval."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS (BP-= BP1 BP2)
    (UNDO-SAVE-NEW-SMALL-CHANGE BP1 BP2)
    (MUNG-BP-INTERVAL BP1)
    (CHARMAP-PER-LINE (BP1 BP2 NIL)
                      ((MUNG-LINE (CHARMAP-LINE)))
      (LET* ((BEFORE (CHARMAP-CHAR))
             (AFTER (CHAR-UPCASE BEFORE)))
        (UNLESS (EQ BEFORE AFTER)
          (CHARMAP-SET-CHAR AFTER))))))


(DEFUN DOWNCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Lowercasify all characters in the specified interval."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS (BP-= BP1 BP2)
    (UNDO-SAVE-NEW-SMALL-CHANGE BP1 BP2)
    (MUNG-BP-INTERVAL BP1)
    (CHARMAP-PER-LINE (BP1 BP2 NIL)
                      ((MUNG-LINE (CHARMAP-LINE)))
      (LET* ((BEFORE (CHARMAP-CHAR))
             (AFTER (CHAR-DOWNCASE BEFORE)))
        (UNLESS (EQ BEFORE AFTER)
          (CHARMAP-SET-CHAR AFTER))))))

(DEFUN RELEVANT-FUNCTION-NAME (BP &OPTIONAL STRINGP (FUNCTION-ONLY T) (FUNCALL-SPECIAL T)
                                  &AUX START-BP X)
  "Return a function spec obtained from the text around BP.
STRINGP = T says print the spec into a string and return that.
FUNCTION-ONLY means only consider actually defined functions (default T).
FUNCALL-SPECIAL (default T) means if BP points inside a SEND or FUNCALL,
see if it looks like sending a message and try to return
a function spec for an existing method it might call."
  ;; If we are in or next to a #' or what follows it,
  ;; set START-BP to point before it.  Otherwise START-BP is NIL.
  ;; We do not here detect #' preceding a list we are in; that is detected later.
  (COND ((LOOKING-AT BP "#'") (SETQ START-BP BP))
        ((LOOKING-AT (FORWARD-CHAR BP -1 T) "#'") (SETQ START-BP (FORWARD-CHAR BP -1 T)))
        ((LOOKING-AT (FORWARD-CHAR BP -2 T) "#'") (SETQ START-BP (FORWARD-CHAR BP -2 T)))
        (T (SETQ START-BP (FORWARD-SEXP BP -1 T))
           (IF (NOT (LOOKING-AT START-BP "#'"))
               (SETQ START-BP NIL)
             (IF (BP-< (FORWARD-SEXP START-BP 1 T 0 (FORWARD-CHAR BP 1)) BP)
                 (SETQ START-BP NIL)))))
  ;; If START-BP was set, and the following thing is a good function, use that.
  (IF (AND START-BP
           (LET ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT))
             (SETQ X (BP-READ-OBJECT (FORWARD-CHAR START-BP 2))))
           (OR (NOT FUNCTION-ONLY)
               (ZMACS-DEFINEDP X)))
      (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)
    ;; Otherwise, look up in list structure to find either
    ;; a function we are in a call to, or a level preceded by #'.
    (SETQ START-BP (FORWARD-DEFUN BP -1 T))
    (DO ((BP1 BP)
         (FIRST-TIME T NIL)
         (FN-START)
         (FN-END)
         (X))
        ((NULL (SETQ BP1 (FORWARD-SEXP BP1 -1 NIL 1 START-BP NIL)))
         ;; If bp is at beginning of a defun, get the function it defines.
         (AND FIRST-TIME
              (TYPEP (BP-NODE BP) 'SECTION-NODE)
              (SETQ X (SECTION-NODE-NAME (BP-NODE BP)))
              (NOT (STRINGP X))
              (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))
      ;; If this level of list structure is preceded by #',
      ;; maybe return this whole list.
      (AND (LOOKING-AT (FORWARD-CHAR BP1 -2 T) "#'")
           (SETQ X (BP-READ-OBJECT BP1))
           (OR (NOT FUNCTION-ONLY)
               (ZMACS-DEFINEDP X))
           (RETURN (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))
      ;; Otherwise consider the function which is the car of this list.
      (OR (SETQ FN-START (FORWARD-CHAR BP1)) (RETURN NIL))
      (OR (SETQ FN-END (FORWARD-SEXP FN-START)) (RETURN NIL))
      (COND ((AND (EQ (BP-LINE FN-START) (BP-LINE FN-END))
                  (LET ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT))
                    (SETQ X (BP-READ-OBJECT FN-START)))
                  (SYMBOLP X)
                  (OR (NOT FUNCTION-ONLY)
                      (ZMACS-DEFINEDP X)
                      (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST X))))
;;; I'm settling this once and for all.  If the cursor is at column zero on the line
;;; of a definition form, the return value is the thing being defined.
;;; If, however, the cursor is inside the definition form, the definition form
;;; symbol itself is returned.  This will make C-Sh-A work for normal people
;;; who forget the arguments and documentation to DEFSYSTEM and for those who
;;; are convinced for whatever reason that it's easier to look at the arglist
;;; in the echo area than the buffer window.  -dg 1/19/86
;;; Anyone who wants to change this back is urged to talk to me about it.
;;; "And if I weren't to do so???" -- random loser
;;; "It would be BAD." -- random bug-buster
;            ;; If default is DEFUN, etc., get the function being defined.
;            (AND (TYPEP (BP-NODE FN-START) 'SECTION-NODE)
;                 (= (BP-INDEX FN-START) 1)
;                 (EQ (BP-LINE FN-START)
;                     (SECTION-NODE-DEFUN-LINE (BP-NODE FN-START)))
;                 (NOT (STRINGP (SECTION-NODE-NAME (BP-NODE FN-START))))
;                 (SETQ X (SECTION-NODE-NAME (BP-NODE FN-START))))
             ;; Rather than one of certain well-known functionals,
             ;; use the function to be passed to it.
             (AND (OR (MEMQ X '(FUNCALL LEXPR-FUNCALL APPLY SEND LEXPR-SEND
                                        MAP MAPC MAPCAR MAPLIST MAPCAN MAPCON)))
                  FUNCALL-SPECIAL
                  (SETQ FN-START (FORWARD-OVER *WHITESPACE-CHARS* FN-END))
                  (SETQ FN-END (FORWARD-SEXP FN-START))
                  (EQ (BP-LINE FN-START) (BP-LINE FN-END))
                  (LET* ((SI:READ-INTERN-FUNCTION 'INTERN-SOFT)
                         (Y (BP-READ-OBJECT FN-START)))
                    (AND (MEMQ (CAR-SAFE Y) '(QUOTE FUNCTION))
                         (SYMBOLP (SETQ Y (CADR Y)))
                         (OR (NOT FUNCTION-ONLY)
                             (ZMACS-DEFINEDP Y))
                         (SETQ X Y))))
             (RETURN (IF STRINGP (FORMAT:OUTPUT NIL (PRIN1 X)) X)))))))

(DEFUN BP-READ-OBJECT (BP)
  "Read and return an object from the text beginning where BP points.
It must all be on one line, or we get an EOF error and return NIL."
  (LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
    (CATCH-ERROR (CL:READ-FROM-STRING (STRING-REMOVE-FONTS (BP-LINE BP)) T NIL
                                      :START (BP-INDEX BP))
                 NIL)))

(DEFUN RELEVANT-METHOD-NAME (BP &OPTIONAL (NSEXP 2) &AUX BP1)
  (WHEN (AND (SETQ BP (FORWARD-LIST BP -1 NIL 1))
             (SETQ BP (FORWARD-LIST BP 1 NIL 1 T))
             (SETQ BP (FORWARD-SEXP BP NSEXP))
             (SETQ BP (FORWARD-TO-WORD BP))
             (SETQ BP1 (FORWARD-ATOM BP)))
    (AND (CHAR-EQUAL (BP-CHARACTER-BEFORE BP) #/:)     ;Pick up package prefix
         (SETQ BP (FORWARD-ATOM BP -1)))
    (CL:READ-FROM-STRING (STRING-INTERVAL BP BP1 T))))

(DEFUN RELEVANT-DEFMETHOD-METHOD-NAME (BP &AUX BP1)
  (WHEN (AND (SETQ BP (FORWARD-LIST BP -1 NIL 1))       ;up 1
             (SETQ BP (FORWARD-LIST BP 1 NIL 1 T))      ;down 1
             (SETQ BP (FORWARD-SEXP BP 1))              ;forward 1
             (SETQ BP (FORWARD-LIST BP 1 NIL 1 T))      ;down 1
             (SETQ BP (FORWARD-SEXP BP 1))              ;forward 1
             (SETQ BP (FORWARD-TO-WORD BP))             ;to start of atom
             (SETQ BP1 (FORWARD-ATOM BP)))
    (AND (CHAR-EQUAL (BP-CHARACTER-BEFORE BP) #/:)     ;Pick up package prefix
         (SETQ BP (FORWARD-ATOM BP -1)))
    (CL:READ-FROM-STRING (STRING-INTERVAL BP BP1 T))))

;;; You might want to change this, if e.g. you are only hacking windows
(DEFVAR *BASE-FLAVOR* 'SI:VANILLA-FLAVOR)

(DEFUN METHOD-ARGLIST (MESSAGE-NAME)
  (MULTIPLE-VALUE-BIND (ARGLIST FUN RETLIST)
      (METHOD-ARGLIST-INTERNAL *BASE-FLAVOR* MESSAGE-NAME NIL NIL NIL)
    (VALUES (IF ARGLIST (CDR ARGLIST) 'NOT-FOUND) FUN RETLIST)))

(DEFUN METHOD-ARGLIST-INTERNAL (FLAVOR MESSAGE-NAME ARGLIST FUN RETLIST
                                &AUX FLAVOR-METHOD-TABLE MESSAGE-ENTRY)
  (SETQ FLAVOR (GET FLAVOR 'SI:FLAVOR))
  (AND (SETQ FLAVOR-METHOD-TABLE (DONT-OPTIMIZE (SI:FLAVOR-METHOD-TABLE FLAVOR)))
       (SETQ MESSAGE-ENTRY (ASSQ MESSAGE-NAME FLAVOR-METHOD-TABLE))
       (DOLIST (METHOD (CDDDR MESSAGE-ENTRY))
         (LET ((FUNCTION (CAR METHOD)))
           (COND ((OR (= (LENGTH FUNCTION) 3)
                      (MEMQ (THIRD FUNCTION) '(:BEFORE :AFTER)))
                  ;; FUN is the first method seen, where we assume most of the
                  ;; argument list names came from.  We are assuming that all methods
                  ;; for a given message name are more or less compatible.
                  (OR FUN (SETQ FUN FUNCTION))
                  (MULTIPLE-VALUE-BIND (THISARG THISRET) (ARGLIST (CADR METHOD))
                    (OR RETLIST (SETQ RETLIST THISRET))
                    (SETQ ARGLIST (METHOD-ARGLIST-MERGE ARGLIST THISARG))))))))
  (DOLIST (FLAVOR (DONT-OPTIMIZE (SI:FLAVOR-DEPENDED-ON-BY FLAVOR)))
    (MULTIPLE-VALUE (ARGLIST FUN)
      (METHOD-ARGLIST-INTERNAL FLAVOR MESSAGE-NAME ARGLIST FUN RETLIST)))
  (VALUES ARGLIST FUN RETLIST))

(DEFUN METHOD-ARGLIST-MERGE (OLD-ARGLIST NEW-ARGLIST)
  (DO ((OLD OLD-ARGLIST (CDR OLD))
       (NEW NEW-ARGLIST (CDR NEW))
       (OLDOLD NIL OLD))
      ((OR (NULL OLD) (NULL NEW)))
    (DO () ((NOT (MEMQ (CAR OLD) '(&OPTIONAL &SPECIAL &LOCAL))))
      (SETQ OLD (CDR OLD)))
    (DO () ((NOT (MEMQ (CAR NEW) '(&OPTIONAL &SPECIAL &LOCAL))))
      (SETQ NEW (CDR NEW)))
    (COND ((EQ (CAR OLD) '&REST)
           (OR (EQ (CAR NEW) '&REST)
               (IF OLDOLD (RPLACD OLDOLD NEW) (SETQ OLD-ARGLIST (COPYLIST NEW))))
           (RETURN))
          ((EQ (CAR NEW) '&REST)
           (AND (SYMBOLP (CADR OLD)) (STRING-EQUAL (CADR OLD) 'IGNORE)
                (NOT (AND (SYMBOLP (CADR NEW)) (STRING-EQUAL (CADR NEW) 'IGNORE)))
                (RPLACA (CDR OLD) (CADR NEW)))
           (RETURN))
          ((AND (SYMBOLP (CAR OLD)) (STRING-EQUAL (CAR OLD) 'IGNORE))
           (OR (AND (SYMBOLP (CAR NEW)) (STRING-EQUAL (CAR NEW) 'IGNORE))
               (RPLACA OLD (CAR NEW))))))
  (OR OLD-ARGLIST (COPYLIST NEW-ARGLIST)))

(DEFUN BPS-FAR-APART (BP1 BP2 N)
  "Return T if BP1 and BP2 are more than N lines apart."
  (LET ((LINE1 (BP-LINE BP1))
        (LINE2 (BP-LINE BP2)))
    (NOT (OR (DO ((L LINE1 (LINE-NEXT L))
                  (I 0 (1+ I)))
                 (( I N) NIL)
               (IF (EQ L LINE2) (RETURN T))
               (IF (NULL L) (RETURN NIL)))
             (DO ((L LINE1 (LINE-PREVIOUS L))
                  (I 0 (1+ I)))
                 (( I N) NIL)
               (IF (EQ L LINE2) (RETURN T))
               (IF (NULL L) (RETURN NIL)))))))

(DEFUN PARAGRAPH-INTERVAL (BP &OPTIONAL (N 1) &AUX OTHER-BP)
  "Return an interval containing the paragraph around or after BP.
If N is greater than 1, additional following paragraphs are included."
  (LET ((TEMP-BP (DO ((BP BP (FORWARD-LINE BP 1)))
                     ((NULL BP)
                      (INTERVAL-LAST-BP *INTERVAL*))
                   (IF (NOT (LINE-BLANK-OR-DIAGRAM-P (BP-LINE BP)))
                       (RETURN BP)))))
    (SETQ TEMP-BP (FORWARD-PARAGRAPH TEMP-BP 1 T))
    (SETQ TEMP-BP (FORWARD-PARAGRAPH TEMP-BP -1 T))
    (SETQ OTHER-BP (FORWARD-PARAGRAPH TEMP-BP N T))
    (CREATE-INTERVAL TEMP-BP OTHER-BP)))

(DEFUN MAKE-INTERVAL-TYPEOUT-STREAM ()
  "Return a stream which inputs from the terminal and outputs to the buffer.
This is used by Execute Command Into Buffer."
  (LET-CLOSED ((*INTERVAL-STREAM* (INTERVAL-STREAM-INTO-BP (POINT)))
               (*TYPEOUT-WINDOW* *TYPEOUT-WINDOW*)
               (*WHICH-OPERATIONS* NIL))
    'INTERVAL-TYPEOUT-STREAM-IO))

(DEFUN INTERVAL-TYPEOUT-STREAM-IO (OP &REST ARGS)
  (DECLARE (SPECIAL *INTERVAL-STREAM* *TYPEOUT-WINDOW* *WHICH-OPERATIONS*))
  (IF (EQ OP ':WHICH-OPERATIONS)
      (OR *WHICH-OPERATIONS*
          (SETQ *WHICH-OPERATIONS*
                (CL:UNION
                  '(:ITEM :ITEM-LIST :READ-BP :SET-BP :DELETE-TEXT :UNTYO-MARK)
                  (SEND *TYPEOUT-WINDOW* ':WHICH-OPERATIONS)
                  :TEST #'EQ)))
    (APPLY (CASE OP
             ((:ITEM :ITEM-LIST) 'INTERVAL-TYPEOUT-STREAM-ITEM-IO)
             ((:TYO :LINE-OUT :STRING-OUT :UNTYO-MARK :READ-BP :UNTYO :SET-BP
                    :DELETE-TEXT :FRESH-LINE :SET-POINTER :READ-CURSORPOS :SET-CURSORPOS
                    :INCREMENT-CURSORPOS :CLEAR-SCREEN)
              *INTERVAL-STREAM*)
             (OTHERWISE *TYPEOUT-WINDOW*))
           OP ARGS)))

(DEFSELECT INTERVAL-TYPEOUT-STREAM-ITEM-IO
  (:ITEM (IGNORE ITEM &REST FORMAT-ARGS)
    (IF FORMAT-ARGS
        (APPLY #'FORMAT 'INTERVAL-TYPEOUT-STREAM-IO FORMAT-ARGS)
        (PRINC ITEM 'INTERVAL-TYPEOUT-STREAM-IO)))
  (:ITEM-LIST (TYPE LIST &AUX (MAXL 0) N (SHEET (WINDOW-SHEET *WINDOW*)))
    (INTERVAL-TYPEOUT-STREAM-IO ':FRESH-LINE)
    (COND (LIST                                 ;Do nothing if empty list
           ;; Compute the maximum width of any item, in dots (MAXL).
           (DOLIST (ITEM LIST)
             (LET ((STRING (STRING (IF (CONSP ITEM) (CAR ITEM) ITEM))))
               (SETQ MAXL (MAX (TV:SHEET-STRING-LENGTH SHEET STRING) MAXL))))
           ;; How many items go on each line (except the last)?
           (SETQ N (MAX (MIN (TRUNCATE *FILL-COLUMN*
                                       (+ MAXL
                                          (FONT-CHAR-WIDTH (TV:SHEET-CURRENT-FONT SHEET))))
                             (LENGTH LIST))
                        1))
           ;; Now print the items and store the data in the table.
           ;; Move to a new line when we exhaust a line, and at the end.
           ;; I counts from 1 thru N on each line.
           (DO* ((I 1 (1+ I))
                 (LIST LIST (CDR LIST))
                 (WIDTH-PER (TRUNCATE (TRUNCATE *FILL-COLUMN* N)
                                      (FONT-CHAR-WIDTH (TV:SHEET-CURRENT-FONT SHEET)))))
                ((NULL LIST))
             ;; Actually make this item.
             (IF (CONSP (CAR LIST))
                 (INTERVAL-TYPEOUT-STREAM-ITEM-IO ':ITEM TYPE (CDAR LIST) "~A" (CAAR LIST))
               (INTERVAL-TYPEOUT-STREAM-ITEM-IO ':ITEM TYPE (CAR LIST)))
             ;; Space out for next item, or move to new line.
             (IF (AND ( I N) (CDR LIST))
                 ;; Not end of line, space out for next item.
                 (LET ((X (INTERVAL-TYPEOUT-STREAM-IO ':READ-CURSORPOS ':CHARACTER)))
                   (INTERVAL-TYPEOUT-STREAM-IO
                     ':INCREMENT-CURSORPOS
                     (- (* WIDTH-PER
                           (CEILING X WIDTH-PER))
                        X)
                     0
                     ':CHARACTER))
               ;; End of line.
               (INTERVAL-TYPEOUT-STREAM-IO ':TYO #/NEWLINE)
               (SETQ I 0)))))))

(DEFUN GRIND-INTO-BP (BP SEXP)
  (SI:GRIND-TOP-LEVEL SEXP 90. (INTERVAL-STREAM-INTO-BP BP) T))

(DEFUN LOOKING-AT (BP STRING)
  "T if STRING matches the text following BP.  STRING must not contain Return."
  (AND BP
       (LET ((CP (BP-INDEX BP))
             (SLEN (STRING-LENGTH STRING)))
         (STRING-EQUAL (BP-LINE BP) STRING :START1 CP :START2 0 :END1 (+ CP SLEN)))))

(DEFUN LOOKING-AT-BACKWARD (BP STRING)
  "T if STRING matches the text before BP.  STRING must not contain Return."
  (AND BP
       (LET ((CP (BP-INDEX BP))
             (SLEN (STRING-LENGTH STRING)))
         (AND ( CP SLEN)
              (STRING-EQUAL (BP-LINE BP) STRING :START1 (- CP SLEN) :START2 0 :END1 CP)))))

(DEFUN DELIMCHAR-P (CHAR)
  "T if CHAR is not alphanumeric."
  (NOT (ALPHANUMERICP (CHAR-CODE CHAR))))

(DEFUN BP-LOOKING-AT-LIST (BP LIST)
  "T if one of the characters or strings in LIST matches the text after BP.
The strings may not contain Return."
  (DO ((LIST LIST (CDR LIST))
       (BP-CH (BP-CH-CHARACTER BP))
       (CH))
      ((NULL LIST) NIL)
    (SETQ CH (CAR LIST))
;character lossage
    (IF (NUMBERP CH) (SETQ CH (INT-CHAR CH)))
    (AND (IF (TYPEP (SETQ CH (CAR LIST)) 'CHARACTER)
             (CHAR-EQUAL BP-CH CH)
           (LET ((LEN (STRING-LENGTH CH))
                 (INDEX (BP-INDEX BP)))
             (STRING-EQUAL (BP-LINE BP) CH :START1 INDEX :END1 (+ INDEX LEN))))
         (RETURN CH))))

(DEFUN NEXT-LINE-WITH-PROPERTY-BP (FROM-BP PROPERTY)
  "Returns a BP to the next line whose plist has a non-NIL PROPERTY property.
The first line considered is the first one whose beginning comes
at or after FROM-BP."
  (DO ((LINE (IF (BEG-LINE-P FROM-BP)
                 (BP-LINE FROM-BP)
               (LINE-NEXT (BP-LINE FROM-BP)))
             (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
      ((OR (EQ LINE LAST-LINE)
           (GETF (LINE-PLIST LINE) PROPERTY))
       (CREATE-BP LINE 0))))


;;; Return a list of callers of a function, like WHO-CALLS prints
;;; The symbol UNBOUND-FUNCTION is treated specially here too.
;;; FUNCTION can be a list of symbols or a single symbol.
(DEFUN LIST-CALLERS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE) &AUX (LIST NIL))
  (SI:FIND-CALLERS-OF-SYMBOLS FUNCTION PKG #'(LAMBDA (CALLER IGNORE IGNORE)
                                               (PUSHNEW CALLER LIST :TEST 'EQ)))
  LIST)

(DEFUN LIST-MATCHING-SYMBOLS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE) &AUX (LIST NIL))
  (FUNCALL (IF (EQ PKG PKG-GLOBAL-PACKAGE) #'MAPATOMS-ALL #'MAPATOMS)
           #'(LAMBDA (SYM) (AND (FUNCALL FUNCTION SYM)
                                (PUSHNEW SYM LIST :TEST 'EQ)))
           PKG)
  LIST)

;;; Interval sorting
(DEFUN SORT-LINES-INTERVAL (LESSP-FN FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Given a lessp predicate and an interval, sort the lines in that interval.
The LESSP-FN is applied to a pair of lines,
 and should be T if the first line is strictly /"less/".
FROM-BP and TO-BP delimit the interval; or FROM-BP can be an interval.
The argument BP's are assumed to point at the beginning of their lines.
BP's to the ends of the interval remain at the ends of the interval, BP's
inside the interval move with their lines."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (WITH-UNDO-SAVE ("Sort" FROM-BP TO-BP T)
    (MUNG-BP-INTERVAL FROM-BP)
    (LET ((PRECEDING-LINE (LINE-PREVIOUS (BP-LINE FROM-BP)))
          (FOLLOWING-LINE (BP-LINE TO-BP))
          (PRECEDING-BPS (DO ((L (LINE-BP-LIST (BP-LINE FROM-BP)) (CDR L))
                              (R NIL))
                             ((NULL L) R)
                           (AND (ZEROP (BP-INDEX (CAR L)))
                                (EQ (BP-STATUS (CAR L)) ':NORMAL)
                                (PUSH (CAR L) R))))
          (N-LINES (1- (COUNT-LINES FROM-BP TO-BP T)))
          LINE-ARRAY FIRST-LINE)
      (SETQ LINE-ARRAY (MAKE-ARRAY N-LINES))
      (DO ((I 0 (1+ I))
           (L (BP-LINE FROM-BP) (LINE-NEXT L)))
          ((EQ L FOLLOWING-LINE))
        (ASET L LINE-ARRAY I))
      (SORT LINE-ARRAY LESSP-FN)
      (DO ((PREC PRECEDING-LINE LINE)
           (I 0 (1+ I))
           (LINE))
          ((= I N-LINES)
           (COND ((NOT (NULL LINE))
                  (SETF (LINE-NEXT LINE) FOLLOWING-LINE)
                  (SETF (LINE-PREVIOUS FOLLOWING-LINE) LINE))))
        (SETQ LINE (CL:AREF LINE-ARRAY I))
        (AND PREC (SETF (LINE-NEXT PREC) LINE))
        (SETF (LINE-PREVIOUS LINE) PREC))
      (SETQ FIRST-LINE (AND (PLUSP N-LINES) (CL:AREF LINE-ARRAY 0)))
      (DOLIST (BP PRECEDING-BPS)
        (MOVE-BP BP FIRST-LINE 0)))))

(DEFUN SORT-INTERVAL-ARRAY (ARRAY LESSP-FN FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Sort the array ARRAY of intervals using LESSP-FN and replace text from FROM-BP to TO-BP.
LESSP-FN is applied to pairs of intervals as taken from ARRAY.
After ARRAY is sorted, the text from FROM-BP to TO-BP is deleted
and replaced by the contents of the intervals in ARRAY, in their new order."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (SORT ARRAY LESSP-FN)
  (LET ((NEW-INTERVAL (CREATE-INTERVAL))
        (*BATCH-UNDO-SAVE* T))
    (DO ((END-BP (INTERVAL-LAST-BP NEW-INTERVAL))
         (I 0 (1+ I))
         (LEN (ARRAY-ACTIVE-LENGTH ARRAY)))
        (( I LEN))
      (INSERT-INTERVAL END-BP (CL:AREF ARRAY I)))
    (WITH-UNDO-SAVE-IF (NOT *BATCH-UNDO-SAVE*) ("Sort" FROM-BP TO-BP T)
      (WITH-BP (SAVE-BP FROM-BP ':MOVES)
        (INSERT-INTERVAL SAVE-BP NEW-INTERVAL)
        (DELETE-INTERVAL SAVE-BP TO-BP T)))))

(DEFUN INTERVAL-LESSP (INTERVAL-1-FROM-BP INTERVAL-1-TO-BP INTERVAL-1-IN-ORDER-P
                       INTERVAL-2-FROM-BP INTERVAL-2-TO-BP INTERVAL-2-IN-ORDER-P)
  "Compare the text of two intervals alphabetically.
Each group of three arguments specifies one interval,
and can be either an interval and two NILs, or two BPs and a flag."
  (GET-INTERVAL INTERVAL-1-FROM-BP INTERVAL-1-TO-BP INTERVAL-1-IN-ORDER-P)
  (GET-INTERVAL INTERVAL-2-FROM-BP INTERVAL-2-TO-BP INTERVAL-2-IN-ORDER-P)
  (DO ((LINE-1 (BP-LINE INTERVAL-1-FROM-BP))
       (LINE-2 (BP-LINE INTERVAL-2-FROM-BP))
       (LEN-1 (LINE-LENGTH (BP-LINE INTERVAL-1-FROM-BP)))
       (LEN-2 (LINE-LENGTH (BP-LINE INTERVAL-2-FROM-BP)))
       (INDEX-1 (BP-INDEX INTERVAL-1-FROM-BP) (1+ INDEX-1))
       (INDEX-2 (BP-INDEX INTERVAL-2-FROM-BP) (1+ INDEX-2))
       (END-LINE-1 (BP-LINE INTERVAL-1-TO-BP))
       (END-LINE-2 (BP-LINE INTERVAL-2-TO-BP))
       (END-INDEX-1 (BP-INDEX INTERVAL-1-TO-BP))
       (END-INDEX-2 (BP-INDEX INTERVAL-2-TO-BP))
       (CH-1) (CH-2))
      (NIL)
    ;; If the second string is exhausted, then the strings are equal or the second one is less
    ;; so we return false.
    (AND (EQ LINE-2 END-LINE-2) (= INDEX-2 END-INDEX-2)
         (RETURN NIL))
    ;; If the first string is exhausted, it is less.
    (AND (EQ LINE-1 END-LINE-1) (= INDEX-1 END-INDEX-1)
         (RETURN T))
    (IF (= INDEX-1 LEN-1)
        (SETQ LINE-1 (LINE-NEXT LINE-1)
              LEN-1 (LINE-LENGTH LINE-1)
              INDEX-1 -1
              CH-1 #/NEWLINE)
      (SETQ CH-1 (CL:AREF LINE-1 INDEX-1)))
    (IF (= INDEX-2 LEN-2)
        (SETQ LINE-2 (LINE-NEXT LINE-2)
              LEN-2 (LINE-LENGTH LINE-2)
              INDEX-2 -1
              CH-2 #/NEWLINE)
      (SETQ CH-2 (CL:AREF LINE-2 INDEX-2)))
    (WHEN (CHAR-LESSP CH-2 CH-1)
      (RETURN NIL))
    (WHEN (CHAR-LESSP CH-1 CH-2)
      (RETURN T))))

(DEFFLAVOR INTERVAL-WITH-SORT-INTERVAL
           (SORT-FIRST-BP
            SORT-LAST-BP)
           (INTERVAL)
  (:ACCESSOR-PREFIX INTERVAL-)
  (:ORDERED-INSTANCE-VARIABLES FIRST-BP LAST-BP SORT-FIRST-BP SORT-LAST-BP)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFSUBST MAKE-INTERVAL-WITH-SORT-INTERVAL (FIRST-BP LAST-BP SORT-FIRST-BP SORT-LAST-BP)
  (SI:%MAKE-INSTANCE 'INTERVAL-WITH-SORT-INTERVAL FIRST-BP LAST-BP
                     SORT-FIRST-BP SORT-LAST-BP))

(DEFUN INTERVAL-WITH-SORT-INTERVAL-LESSP (INT1 INT2)
  "Compare alphabetically the keys in two INTERVAL-WITH-SORT-INTERVALs.
Each of those is an interval with a distinguished subinterval
which is what is actually used for sorting."
  (INTERVAL-LESSP (INTERVAL-SORT-FIRST-BP INT1) (INTERVAL-SORT-LAST-BP INT1) T
                  (INTERVAL-SORT-FIRST-BP INT2) (INTERVAL-SORT-LAST-BP INT2) T))

(DEFUN SORT-INTERVAL-FUNCTIONS (MOVE-TO-KEY-FN MOVE-OVER-KEY-FN MOVE-TO-NEXT-FN LESSP-FN
                                FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Sort parts of an interval using functions to parse it apart.
The interval to be sorted is specified by FROM-BP, TO-BP and IN-ORDER-P:
they can be two BPs and a flag, or FROM-BP can actually be an interval.
MOVE-TO-KEY-FN is given a BP to the beginning of a sort record
 and should return a BP to the beginning of the key in that record.
MOVE-OVER-KEY-FN is given that BP, and should return a BP to the
 end of the key.
MOVE-TO-NEXT-FN is given that BP to the end of the key, and should
 return a BP to the end of the sort record.  If this is not the end
 of the interval to be sorted, it is the beginning of another record.
After the interval is parsed into records, they are sorted using LESSP-FN
/(which gets two INTERVAL-WITH-SORT-INTERVALs as args; see the function
INTERVAL-WITH-SORT-INTERVAL-LESSP for a sample of a suitable function)
and the result is used to replace the text of the interval being sorted."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((ARRAY (MAKE-ARRAY 400. ':FILL-POINTER 0)))
    (DO ((*INTERVAL* (CREATE-INTERVAL (COPY-BP FROM-BP ':NORMAL) (COPY-BP TO-BP ':MOVES)))
         (START-BP FROM-BP END-BP)
         (KEY-START-BP) (KEY-END-BP) (END-BP))
        ((BP-= START-BP TO-BP))
      (SETQ KEY-START-BP (FUNCALL MOVE-TO-KEY-FN START-BP)
            KEY-END-BP (FUNCALL MOVE-OVER-KEY-FN KEY-START-BP)
            END-BP (FUNCALL MOVE-TO-NEXT-FN KEY-END-BP))
      (VECTOR-PUSH-EXTEND
        (MAKE-INTERVAL-WITH-SORT-INTERVAL START-BP END-BP KEY-START-BP KEY-END-BP)
        ARRAY))
    (SORT-INTERVAL-ARRAY ARRAY LESSP-FN FROM-BP TO-BP T)))

(DEFFLAVOR INTERVAL-WITH-SORT-KEY
           ((SORT-KEY NIL))
           (INTERVAL)
  (:ACCESSOR-PREFIX INTERVAL-)
  (:ORDERED-INSTANCE-VARIABLES FIRST-BP LAST-BP SORT-KEY)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFSUBST MAKE-INTERVAL-WITH-SORT-KEY (FIRST-BP LAST-BP KEY)
  (SI:%MAKE-INSTANCE 'INTERVAL-WITH-SORT-KEY FIRST-BP LAST-BP KEY))

(DEFUN SORT-INTERVAL-FUNCTIONS-WITH-KEY (MOVE-TO-KEY-FN GET-KEY-FN MOVE-TO-NEXT-FN LESSP-FN
                                FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Sort parts of an interval using functions to parse it apart.
The interval to be sorted is specified by FROM-BP, TO-BP and IN-ORDER-P:
they can be two BPs and a flag, or FROM-BP can actually be an interval.
MOVE-TO-KEY-FN is given a BP to the beginning of a sort record
 and should return a BP to the beginning of the key in that record.
GET-KEY-FN is given that BP, and should return two values:
 a BP, and the value to use as the key.
MOVE-TO-NEXT-FN is given that BP, and should
 return a BP to the end of the sort record.  If this is not the end
 of the interval to be sorted, it is the beginning of another record.
After the interval is parsed into records, they are sorted using LESSP-FN
/(which gets two INTERVAL-WITH-SORT-KEYs as args)
and the result is used to replace the text of the interval being sorted."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((ARRAY (MAKE-ARRAY 20. ':FILL-POINTER 0)))
    (DO ((*INTERVAL* (CREATE-INTERVAL (COPY-BP FROM-BP ':NORMAL) (COPY-BP TO-BP ':MOVES)))
         (START-BP FROM-BP END-BP)
         (KEY-START-BP) (KEY-END-BP) (KEY) (END-BP))
        ((BP-= START-BP TO-BP))
      (SETQ KEY-START-BP (FUNCALL MOVE-TO-KEY-FN START-BP))
      (MULTIPLE-VALUE (KEY-END-BP KEY)
        (FUNCALL GET-KEY-FN KEY-START-BP))
      (SETQ END-BP (FUNCALL MOVE-TO-NEXT-FN KEY-END-BP))
      (VECTOR-PUSH-EXTEND (MAKE-INTERVAL-WITH-SORT-KEY START-BP END-BP KEY) ARRAY))
    (SORT-INTERVAL-ARRAY ARRAY LESSP-FN FROM-BP TO-BP T)))

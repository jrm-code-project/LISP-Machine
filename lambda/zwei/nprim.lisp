;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

;The new undo system.  Written by RMS.  Feel free to use it,
;provided you make all improvements and changes public.

; An UNDO-ITEM object records one change, for undoing it.
; Its components are:
; UNDO-ITEM-TYPE - a string starting with a capital letter
;  describing the kind of operation that made the changes.  For example, "Yank".
; UNDO-ITEM-START-BP - points to start of changed interval.
; UNDO-ITEM-END-BP - points to end of changed interval.
; UNDO-ITEM-INTERVAL - a copy of the original contents.
; UNDO-ITEM-SAVED-BP-VALUES - information on any permanent BPs
;  that were originally in the interval, so they can be relocated on undo.
;  There is one element for each BP recorded, and it is a list of length 3.
;  The car is the BP it records.  The cadr is the line-number inside the interval
;  that the BP used to point at.  The caddr is the index within its line of the BP.

; Each node has an UNDO-STATUS object, (NODE-UNDO-STATUS node).
;  This records:
; UNDO-STATUS-UNDO-LIST: a list of UNDO-ITEMs, each describing one change to be undone.
; UNDO-STATUS-REDO-LIST: a list of UNDO-ITEMs for changes made by the Undo command,
;  to be undone by Redo.
;  The symbol EDITING can also appear in this list.
;  It indicates that the user has made some changes since
;  the undos which created the rest of the data in the redo list.
; Some information on a current and not finished batch of small changes:
;  UNDO-STATUS-START-BP, UNDO-STATUS-END-BP, UNDO-STATUS-INTERVAL
;  and UNDO-STATUS-SAVED-BP-VALUES mean the same thing as in an UNDO-ITEM.
;  In addition, there is UNDO-STATUS-INTERVAL-NUMBER-OF-LINES,
;  which is how many beginnings of lines are in the saved interval.
;  This is so that the saved BP values can be updated when new intervals
;  are added to either end of the saved interval.
;  There is also UNDO-STATUS-END-ORIGINAL-INDEX, which is the original
;  index-within-line of the end of the already-recorded changes.
;  This makes it possible, when recording a new batch of changes
;  that come after the old ones, to record the original indices of any bps
;  that follow on the same line, so that those bps get the right indices
;  back when the whole thing is undone.

; WITH-UNDO-SAVE (in MACROS) is the way to make one operation specifically save as a unit.
; It makes one UNDO-ITEM on the undo list.  It calls UNDO-SAVE-BEGIN when it is entered
; and UNDO-SAVE-END when it is exited.  *BATCH-UNDO-SAVE* is T inside WITH-UNDO-SAVE.

; If the NODE-UNDO-STATUS of the top-level node is :DONT,
; it means that no undo information should be recorded for this node.

; All changes to the buffer call UNDO-SAVE-NEW-SMALL-CHANGE specifying the
; range to be changed.  If not inside a WITH-UNDO-SAVE, this records the
; change in the UNDO-STATUS.  If there is already a change recorded there,
; and the two changes are contiguous or overlap, the recorded one and the new one
; are combined into one.  Otherwise, the old recorded one is saved out
; on the undo-list and the new one is put in the undo-status.
; All this happens only if *UNDO-SAVE-SMALL-CHANGES* is T.

;;; Flush the byte pointers in an undo-item.
(DEFUN FLUSH-UNDO-ITEM (UNDO-ITEM)
  (WHEN (CONSP UNDO-ITEM)
    (FLUSH-BP (UNDO-ITEM-START-BP UNDO-ITEM))
    (FLUSH-BP (UNDO-ITEM-END-BP UNDO-ITEM))))

(DEFUN UNDO-SAVE-BEGIN (BP1 BP2 IN-ORDER-P NAME &OPTIONAL NO-REDO-MARK)
  "Record the interval from BP1 to BP2 for undoing changes to it, to be done.
This assumes you will call UNDO-SAVE-END when the changes are finished.
NAME is a string (a capitalized word) saying what kind of change this is.
NO-REDO-MARK means do not push a \"user changed things\" mark onto the redo list."
  (UNDO-SAVE-CURRENT-RANGE)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (LET* ((BP-VALUES (RECORD-BP-VALUES BP1 BP2 NIL 0 (BP-INDEX BP1)))
         (INT (NODE-TOP-LEVEL-NODE *INTERVAL*))
         (MODP (BUFFER-MODIFIED-P INT))
         (UNDO-STATUS (NODE-UNDO-STATUS INT)))
    (UNLESS (EQ UNDO-STATUS ':DONT)
      (OR NO-REDO-MARK
          (EQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
          (PUSH 'EDITING (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
      (UNLESS MODP
        (PUSH 'PREVIOUSLY-UNMODIFIED
              (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))
      (PUSH (MAKE-UNDO-ITEM NAME (COPY-BP BP1 ':NORMAL) (COPY-BP BP2 ':MOVES)
                            (COPY-INTERVAL BP1 BP2 T)
                            BP-VALUES
                            MODP)
            (UNDO-STATUS-UNDO-LIST UNDO-STATUS))
      (SETF (NODE-UNDO-STATUS INT) UNDO-STATUS))))

(DEFUN NODE-TOP-LEVEL-NODE (NODE)
  "Return the top level node above NODE, possibly NODE itself."
  (DO ((N NODE (NODE-SUPERIOR N)))
      ((NULL (NODE-SUPERIOR N)) N)))

;;; Convert the types of the bps in an undo item
;;; to update properly for after the undoable operation is complete.
(DEFUN UNDO-SAVE-END ()
  (LET ((UNDO-ITEM (CAR (UNDO-STATUS-UNDO-LIST
                          (NODE-UNDO-STATUS-OR-NIL (NODE-TOP-LEVEL-NODE *INTERVAL*))))))
    (WHEN (CONSP UNDO-ITEM)     ;To avoid lossage on aborting, if undo-list is empty.
                                ;  also empty if MODE-UNDO-STATUS was :DONT
      (SETF (BP-STATUS (UNDO-ITEM-START-BP UNDO-ITEM)) ':MOVES)
      (SETF (BP-STATUS (UNDO-ITEM-END-BP UNDO-ITEM)) ':NORMAL))))

(DEFUN UNDO-SAVE-CURRENT-RANGE (&AUX (INT (NODE-TOP-LEVEL-NODE *INTERVAL*)))
  "Cause the current batch of small changes to be saved for undoing and cleared out."
  (LET ((UNDO-STATUS (NODE-UNDO-STATUS INT)))
    (UNLESS (EQ UNDO-STATUS ':DONT)
      (OR UNDO-STATUS
          (SETF (NODE-UNDO-STATUS INT)
                (SETQ UNDO-STATUS (MAKE-UNDO-STATUS INT))))
      (WHEN (UNDO-STATUS-START-BP UNDO-STATUS)
        (UNLESS (EQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST (NODE-UNDO-STATUS INT))))
          (PUSH 'EDITING (UNDO-STATUS-REDO-LIST (NODE-UNDO-STATUS INT))))
        (UNLESS (UNDO-STATUS-MODIFIED-FLAG UNDO-STATUS)
          (PUSH 'PREVIOUSLY-UNMODIFIED
                (UNDO-STATUS-UNDO-LIST (NODE-UNDO-STATUS INT))))
        (PUSH (MAKE-UNDO-ITEM "Small changes"
                              (UNDO-STATUS-START-BP UNDO-STATUS)
                              (UNDO-STATUS-END-BP UNDO-STATUS)
                              (UNDO-STATUS-INTERVAL UNDO-STATUS)
                              (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS)
                              (UNDO-STATUS-MODIFIED-FLAG UNDO-STATUS))
              (UNDO-STATUS-UNDO-LIST UNDO-STATUS))
        (UNDO-SAVE-END))
      (SETF (UNDO-STATUS-INTERVAL UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-START-BP UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-END-BP UNDO-STATUS) NIL))))

(DEFUN UNDO-SAVE-NEW-SMALL-CHANGE (BP1 BP2)
  "Add a range of the current interval to the current batch of small changes.
If it is not contiguous with the current batch, start a new batch."
  (UNLESS (OR *BATCH-UNDO-SAVE*
              (NULL *UNDO-SAVE-SMALL-CHANGES*))
    (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP1))
           (UNDO-STATUS (NODE-UNDO-STATUS *INTERVAL*))
           (*BATCH-UNDO-SAVE* T)
           BP1-INSIDE BP2-INSIDE)
      (UNLESS (EQ UNDO-STATUS ':DONT)
        (OR UNDO-STATUS
            (SETF (NODE-UNDO-STATUS *INTERVAL*)
                  (SETQ UNDO-STATUS (MAKE-UNDO-STATUS *INTERVAL*))))
        (OR (NULL (UNDO-STATUS-START-BP UNDO-STATUS))
            (WHEN (BP-= BP1 (UNDO-STATUS-END-BP UNDO-STATUS))
              (SETQ BP1-INSIDE T))
            (WHEN (BP-= BP2 (UNDO-STATUS-START-BP UNDO-STATUS))
              (SETQ BP2-INSIDE T))
            ;; Look thru the current range and see if whether BP1 and BP2 are inside it.
            ;; Next to it counts as inside.
            ;; If either one is inside, the loop returns T.
            (DO ((LINE (BP-LINE (UNDO-STATUS-START-BP UNDO-STATUS)) (LINE-NEXT LINE))
                 (END-LINE (BP-LINE (UNDO-STATUS-END-BP UNDO-STATUS))))
                (())
              (IF (EQ LINE (BP-LINE BP1))
                  (SETQ BP1-INSIDE
                        (AND (OR (NEQ LINE END-LINE)
                                 (NOT (BP-< (UNDO-STATUS-END-BP UNDO-STATUS) BP1)))
                             (OR (NEQ LINE (BP-LINE (UNDO-STATUS-START-BP UNDO-STATUS)))
                                 (NOT (BP-< BP1 (UNDO-STATUS-START-BP UNDO-STATUS)))))))
              (IF (EQ LINE (BP-LINE BP2))
                  (SETQ BP2-INSIDE
                        (AND (OR (NEQ LINE END-LINE)
                                 (NOT (BP-< (UNDO-STATUS-END-BP UNDO-STATUS) BP2)))
                             (OR (NEQ LINE (BP-LINE (UNDO-STATUS-START-BP UNDO-STATUS)))
                                 (NOT (BP-< BP2 (UNDO-STATUS-START-BP UNDO-STATUS)))))))
              (IF (EQ LINE END-LINE) (RETURN (OR BP1-INSIDE BP2-INSIDE))))
            ;; Here should really check for case where BP1 - BP2 spans all of the old batch.

            ;; New changes are not contiguous with old ones.
            ;; Save the old ones on the undo-list and clear them out, so we start a new batch.
            (UNDO-SAVE-CURRENT-RANGE))
        (COND ((AND (UNDO-STATUS-START-BP UNDO-STATUS)
                    (BP-= BP1 BP2))
               ;; If we are adding an empty range onto an existing one, do nothing QUICKLY.
               NIL)
              ((UNDO-STATUS-START-BP UNDO-STATUS)
               ;; There are contiguous, old changes.
               (UNLESS BP1-INSIDE
                       ;; Add an extent before.
                       (LET ((*INTERVAL* (UNDO-STATUS-INTERVAL UNDO-STATUS)))
                         (INSERT-INTERVAL (INTERVAL-FIRST-BP *INTERVAL*)
                                          BP1 (UNDO-STATUS-START-BP UNDO-STATUS) T))
                       (MULTIPLE-VALUE-BIND (ALIST COUNT)
                           (RECORD-BP-VALUES BP1 (UNDO-STATUS-START-BP UNDO-STATUS)
                                             (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS)
                                             NIL
                                             (BP-INDEX BP1)
                                             (UNDO-STATUS-END-BP UNDO-STATUS))
                         (SETF (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS) ALIST)
                         ;; The line-count returned is for the end of the new extent,
                         ;; which added to the size of the old extent gives the new size.
                         (INCF (UNDO-STATUS-INTERVAL-NUMBER-OF-LINES UNDO-STATUS) COUNT))
                       (MOVE-BP (UNDO-STATUS-START-BP UNDO-STATUS) BP1))
               (UNLESS BP2-INSIDE
                       ;; Add an extent after.
                       (LET ((*INTERVAL* (UNDO-STATUS-INTERVAL UNDO-STATUS)))
                         (INSERT-INTERVAL (INTERVAL-LAST-BP *INTERVAL*)
                                          (UNDO-STATUS-END-BP UNDO-STATUS) BP2 T))
                       (MULTIPLE-VALUE-BIND (ALIST COUNT INDEX)
                           (RECORD-BP-VALUES (UNDO-STATUS-END-BP UNDO-STATUS) BP2
                                             (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS)
                                             ;; Start recording this interval
                                             ;; with an appropriate initial line-count.
                                             (UNDO-STATUS-INTERVAL-NUMBER-OF-LINES UNDO-STATUS)
                                             (UNDO-STATUS-END-ORIGINAL-INDEX UNDO-STATUS)
                                             (UNDO-STATUS-START-BP UNDO-STATUS))
                         (SETF (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS) ALIST)
                         ;; The line-count returned is for the end of the new extent,
                         ;; which is the end of the whole thing.
                         (SETF (UNDO-STATUS-INTERVAL-NUMBER-OF-LINES UNDO-STATUS) COUNT)
                         (SETF (UNDO-STATUS-END-ORIGINAL-INDEX UNDO-STATUS) INDEX))
                       (MOVE-BP (UNDO-STATUS-END-BP UNDO-STATUS) BP2)))
              (T
               (SETF (UNDO-STATUS-START-BP UNDO-STATUS) (COPY-BP BP1 ':NORMAL))
               (SETF (UNDO-STATUS-END-BP UNDO-STATUS) (COPY-BP BP2 ':MOVES))
               (MULTIPLE-VALUE-BIND (ALIST COUNT INDEX)
                   (RECORD-BP-VALUES
                     (UNDO-STATUS-START-BP UNDO-STATUS)
                     (UNDO-STATUS-END-BP UNDO-STATUS)
                     NIL 0 (BP-INDEX (UNDO-STATUS-START-BP UNDO-STATUS)))
                 (SETF (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS) ALIST)
                 (SETF (UNDO-STATUS-INTERVAL-NUMBER-OF-LINES UNDO-STATUS) COUNT)
                 (SETF (UNDO-STATUS-END-ORIGINAL-INDEX UNDO-STATUS) INDEX))
               (SETF (UNDO-STATUS-MODIFIED-FLAG UNDO-STATUS)
                     (BUFFER-MODIFIED-P *INTERVAL*))
               (SETF (UNDO-STATUS-INTERVAL UNDO-STATUS)
                     (COPY-INTERVAL BP1 BP2 T))))))))

;;; When we record an interval for undoing,
;;; record also any permanent bps that start out inside or next to the interval,
;;; so that if we undo the change, we can reset all the bps.
;;; These bps will include the ends of any previous undoable changes
;;; so that this takes care of providing for multiple undos.

;;; Update the saved-bp-alist STARTING-ALIST for bps between BP1 and BP2.
;;; If STARTING-COUNT is nil, the new range comes before the range already saved.
;;; Otherwise, the new range comes after, and STARTING-COUNT is the number
;;; of lines in the old range (that is, the line number of BP1's line).
;;; If this is the first bunch of changes recorded in this group,
;;; STARTING-COUNT is 0.
;;; The values are the updated saved-bp-alist and the number of lines
;;; from the beginning of the combined range to BP2.
;;; STARTING-INDEX is the index, before any of the recorded changes, of what is now BP1.
;;; The value UPDATED-INDEX is the index, before any of the changes, of what is now at
;;; the end of the combined recorded range.
(DEFUN RECORD-BP-VALUES (BP1 BP2 &OPTIONAL STARTING-ALIST STARTING-COUNT STARTING-INDEX
                         &REST DONT-RECORD-BPS)
  (DECLARE (VALUES UPDATED-ALIST UPDATED-COUNT UPDATED-INDEX))
  (DO ((LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (LINE-COUNT (OR STARTING-COUNT 0) (1+ LINE-COUNT))
       (ALIST ())
       (BUFFER (BP-TOP-LEVEL-NODE BP1))
       (START-LINE (BP-LINE BP1))
       (END-LINE (BP-LINE BP2)))
      (())
    (DOLIST (BP (LINE-BP-LIST LINE))
      (WHEN (AND (DOLIST (W (SEND *INTERVAL* :WINDOWS) T)
                   (IF (OR (EQ BP (WINDOW-POINT W))
                           (EQ BP (WINDOW-MARK W)))
                       (RETURN NIL)))
                 (NEQ BP BP1) (NEQ BP BP2)
                 (NEQ BP (INTERVAL-FIRST-BP BUFFER))
                 (NEQ BP (INTERVAL-LAST-BP BUFFER))
                 (NOT (MEMQ BP DONT-RECORD-BPS))
                 (NOT (ASSQ BP STARTING-ALIST))
                 (OR (NEQ LINE START-LINE) (NOT (BP-< BP BP1)))
                 (OR (NEQ LINE END-LINE) (NOT (BP-< BP2 BP))))
        (PUSH (LIST BP
                    LINE-COUNT
                    ;; Record, not the bp's current index,
                    ;; but the index it had before these changes began.
                    (IF (EQ LINE START-LINE)
                        (- (BP-INDEX BP) (- (BP-INDEX BP1) STARTING-INDEX))
                        (BP-INDEX BP)))
              ALIST)))
    (IF (EQ LINE END-LINE)
        (RETURN
          (VALUES (IF STARTING-COUNT
                      (NCONC STARTING-ALIST (NREVERSE ALIST))
                      (NRECONC ALIST (UPDATE-BP-VALUES STARTING-ALIST LINE-COUNT)))
                  LINE-COUNT
                  (IF STARTING-COUNT
                      (IF (EQ END-LINE START-LINE)
                          (- (BP-INDEX BP2) (- (BP-INDEX BP1) STARTING-INDEX))
                          (BP-INDEX BP2))
                      STARTING-INDEX))))))

(DEFUN UPDATE-BP-VALUES (SAVED-BP-VALUES NUMBER-OF-LINES)
  (DOLIST (ELT SAVED-BP-VALUES)
    (INCF (CADR ELT) NUMBER-OF-LINES))
  SAVED-BP-VALUES)

;;; Restore the bp values recorded in an UNDO-ITEM-SAVED-BP-VALUES
(DEFUN RESTORE-BP-VALUES (SAVED-BP-VALUES STARTING-BP)
  (LET ((LINE-NUMBER 0) (LINE (BP-LINE STARTING-BP)))
    (DOLIST (ELT SAVED-BP-VALUES)
      (DO () ((= LINE-NUMBER (CADR ELT)))
        (IF (> LINE-NUMBER (CADR ELT))
            (FERROR "Saved BPs out of order."))
        (SETQ LINE (LINE-NEXT LINE)
              LINE-NUMBER (1+ LINE-NUMBER)))
      (MOVE-BP (CAR ELT) LINE (CADDR ELT)))))

(DEFCOM COM-QUICK-UNDO "Undo the last undoable command, no query.
Commands to be undone are remembered for each buffer individually.
If there is a region, undo the last batch of changes that occurred
within the current region.  The region remains so that you can
repeat the command on the same region." (KM)
  (do ((i 0 (incf i))
       (doforever? (and *numeric-arg-p* (minusp *numeric-arg*))))
      ((and (not doforever?)
            (= i (if *numeric-arg-p* *numeric-arg* 1))))
    (UNDO-UNDO-ITEM (FIND-UNDO-ITEM) (NOT (WINDOW-MARK-P *WINDOW*))))
  DIS-TEXT)

(DEFCOM COM-UNDO "Undo the last undoable command done in the current buffer.
If there is a region, undo the last batch of changes that occurred
within the current region.  The region remains so that you can
repeat the command on the same region." (KM)
  (LET ((UNDO-ITEM (FIND-UNDO-ITEM)))
    (FRESH-LINE *QUERY-IO*)
    (IF (Y-OR-N-P "Undo ~A (~A)? " (STRING-DOWNCASE (UNDO-ITEM-TYPE UNDO-ITEM))
                                   (SUMMARIZE-UNDO-ITEM UNDO-ITEM))
        (UNDO-UNDO-ITEM UNDO-ITEM (NOT (WINDOW-MARK-P *WINDOW*)))))
  DIS-TEXT)

;;; Subroutine of UNDO.  Undoes the undo item, moving point if desired, and notifying user.
(DEFUN UNDO-UNDO-ITEM (UNDO-ITEM &OPTIONAL MOVE-POINT-AND-MARK
                       &AUX (UNDO-STATUS (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*)))
                            (NAME (UNDO-ITEM-TYPE UNDO-ITEM)))
  (WHEN MOVE-POINT-AND-MARK
    (MOVE-BP (POINT) (UNDO-ITEM-START-BP UNDO-ITEM))
    (MOVE-BP (MARK) (UNDO-ITEM-END-BP UNDO-ITEM)))
  (UNLESS (EQ UNDO-STATUS ':DONT)
    (SETF (UNDO-STATUS-UNDO-LIST UNDO-STATUS)
          (DELQ UNDO-ITEM (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))
    (PUSH (UNDO-SAVED-CHANGE UNDO-ITEM UNDO-STATUS)
          (UNDO-STATUS-REDO-LIST UNDO-STATUS))
    (IF (UNDO-STATUS-UNDO-LIST UNDO-STATUS)
        (FORMAT *QUERY-IO* "~&~A undone.  ~A to undo more, ~A to undo the Undo."
                NAME (KEY-FOR-COMMAND 'COM-QUICK-UNDO *COMTAB* NIL NIL #\c-sh-U)
                (KEY-FOR-COMMAND 'COM-QUICK-REDO *COMTAB* NIL NIL #\c-sh-R))
      (FORMAT *QUERY-IO* "~&~A undone.  Type ~A to undo the Undo." NAME
              (KEY-FOR-COMMAND 'COM-QUICK-REDO *COMTAB* NIL NIL #\c-sh-R)))))

;;; Subroutine of UNDO.  Finds the undo item to undo, and does all error checking.
(DEFUN FIND-UNDO-ITEM ()
  (UNDO-SAVE-CURRENT-RANGE)
  (LET ((UNDO-ITEM
          (IF (WINDOW-MARK-P *WINDOW*)
              (REGION (BP1 BP2)
                (INTERVAL-LAST-UNDO-ITEM BP1 BP2 T))
            (CAR (UNDO-STATUS-UNDO-LIST
                   (NODE-UNDO-STATUS-OR-NIL (NODE-TOP-LEVEL-NODE *INTERVAL*)))))))
    (SELECTQ UNDO-ITEM
      ((NIL) (BARF "No Undoable changes found in buffer."))
      ((T) (BARF "Conflict: last change in region was not completely within it.")))
    UNDO-ITEM))

(DEFUN INTERVAL-LAST-UNDO-ITEM (START-BP &OPTIONAL END-BP IN-ORDER-P)
  "Returns the most recent undo-item whose changes fall between START-BP and END-BP.
Our arguments specify an interval in the standard way;
its text should be part of *INTERVAL*.
The value is NIL if there is no undo-item whose changes fall there;
it is T if the most recent undo-item that overlaps the specified interval
is not fully contained within it."
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (LET ((UNDO-STATUS (NODE-UNDO-STATUS (BP-TOP-LEVEL-NODE START-BP))))
    (IF (EQ UNDO-STATUS ':DONT)
        UNDO-STATUS
      (DOLIST (UNDO-ITEM (UNDO-STATUS-UNDO-LIST UNDO-STATUS))
        (WHEN (CONSP UNDO-ITEM)
          (LET ((START-INSIDE
                  (BP-IN-INTERVAL-P (UNDO-ITEM-START-BP UNDO-ITEM) START-BP END-BP T))
                (END-INSIDE
                  (BP-IN-INTERVAL-P (UNDO-ITEM-END-BP UNDO-ITEM) START-BP END-BP T)))
            (IF (AND START-INSIDE END-INSIDE) (RETURN UNDO-ITEM))
            (IF (OR START-INSIDE END-INSIDE) (RETURN T))))))))

(DEFCOM COM-QUICK-REDO "Redo the last command undone, no query.
Undone commands are remembered for each buffer individually." ()
  (UNDO-SAVE-CURRENT-RANGE)
  (do ((i 0 (incf i))
       (doforever? (and *numeric-arg-p* (minusp *numeric-arg*)))
       numarg)
      ((cond ((not *numeric-arg-p*)
              ;; returns t on second time through, normal case
              (= i 1))
             ((not doforever?) nil)
             ((= i *numeric-arg*)
              (setq numarg t))
             (t nil))
       (cond ((or numarg (not *numeric-arg-p*)) nil)    ;exit the loop, having done all work
             (t (barf "All recent Undo's redone.  Use the Redo command without numeric arg to redo further."))))
    (LET ((UNDO-STATUS (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))
      (IF (EQ UNDO-STATUS ':DONT) (BARF "No existing redoable changes.")
        (WHEN (OR (NEQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
                  (REDO-QUERY UNDO-STATUS *numeric-arg-p*))
          (LET* ((UNDO-ITEM (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS))))
            (OR UNDO-ITEM
                (if (and *numeric-arg-p* (not (minusp *numeric-arg*)))
                    (return nil)
                  (BARF "No more changes to Redo.")))
            (UNDO-ITEM-REDO UNDO-STATUS UNDO-ITEM))))))
  DIS-TEXT)

(DEFUN REDO-QUERY (UNDO-STATUS &optional after-many)
  (unless after-many
    (progn
      (FORMAT *QUERY-IO* "~&The last Redoable Undo occurred previous to other modifications.")
      (BEEP)
      (WHEN (Y-OR-N-P "Redo most recent Undo anyway? ")
        (POP (UNDO-STATUS-REDO-LIST UNDO-STATUS))
        T))))

(DEFCOM COM-REDO "Redo the last command undone in the current buffer." ()
  (UNDO-SAVE-CURRENT-RANGE)
  (LET* ((UNDO-STATUS (NODE-UNDO-STATUS (NODE-TOP-LEVEL-NODE *INTERVAL*))))
    (IF (EQ UNDO-STATUS ':DONT) (BARF "No Redoable changes exist.")
      (WHEN (OR (NEQ 'EDITING (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
                (REDO-QUERY UNDO-STATUS))
        (LET* ((UNDO-ITEM (CAR (UNDO-STATUS-REDO-LIST UNDO-STATUS)))
               (NAME (UNDO-ITEM-TYPE UNDO-ITEM))
               SUMMARY)
          (OR UNDO-ITEM (BARF "No Redoable changes exist."))
          (FRESH-LINE *QUERY-IO*)
          (SETQ SUMMARY (SUMMARIZE-UNDO-ITEM UNDO-ITEM))
          (IF (Y-OR-N-P "Redo ~A (~A)? " (STRING-DOWNCASE NAME) SUMMARY)
              (UNDO-ITEM-REDO UNDO-STATUS UNDO-ITEM))))))
  DIS-TEXT)

(DEFUN UNDO-ITEM-REDO (UNDO-STATUS UNDO-ITEM)
  (LET ((WASMOD (BUFFER-MODIFIED-P (UNDO-STATUS-NODE UNDO-STATUS))))
    (MOVE-BP (POINT) (UNDO-ITEM-START-BP UNDO-ITEM))
    (MOVE-BP (MARK) (UNDO-ITEM-END-BP UNDO-ITEM))
    (POP (UNDO-STATUS-REDO-LIST UNDO-STATUS))
    (LET ((UNDO-REDO (UNDO-SAVED-CHANGE UNDO-ITEM UNDO-STATUS)))
      (UNLESS WASMOD
        (PUSH 'PREVIOUSLY-UNMODIFIED
              (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))
      (PUSH UNDO-REDO
            (UNDO-STATUS-UNDO-LIST UNDO-STATUS))))
  (MUNG-BP-INTERVAL (POINT))
  (FORMAT *QUERY-IO* "~&~A redone." (UNDO-ITEM-TYPE UNDO-ITEM)))

(DEFUN SUMMARIZE-UNDO-ITEM (UNDO-ITEM)
  (OR (SUMMARIZE-INTERVAL (UNDO-ITEM-START-BP UNDO-ITEM)
                          (UNDO-ITEM-END-BP UNDO-ITEM))
      (STRING-APPEND
        (IF (BP-= (UNDO-ITEM-START-BP UNDO-ITEM)
                  (UNDO-ITEM-END-BP UNDO-ITEM))
            "nothing => " "whitespace => ")
        (OR (SUMMARIZE-INTERVAL (UNDO-ITEM-INTERVAL UNDO-ITEM))
            (IF (BP-= (INTERVAL-FIRST-BP (UNDO-ITEM-INTERVAL UNDO-ITEM))
                      (INTERVAL-LAST-BP (UNDO-ITEM-INTERVAL UNDO-ITEM)))
                "nothing" "whitespace")))))

;;; Given an undo item, undo it, flush its bps, and return a new undo item
;;; which will undo the undoing.
(DEFUN UNDO-SAVED-CHANGE (UNDO-ITEM UNDO-STATUS)
  (FLUSH-UNDO-ITEM UNDO-ITEM)
  (WITH-READ-ONLY-SUPPRESSED ((NODE-TOP-LEVEL-NODE *INTERVAL*))
    (OR (BP-= (UNDO-ITEM-START-BP UNDO-ITEM) (UNDO-ITEM-END-BP UNDO-ITEM))
        (BP-< (UNDO-ITEM-START-BP UNDO-ITEM) (UNDO-ITEM-END-BP UNDO-ITEM))
        (SETF (UNDO-ITEM-END-BP UNDO-ITEM) (COPY-BP (UNDO-ITEM-START-BP UNDO-ITEM))))
    ;; This is like WITH-UNDO-SAVE but doesn't push 'EDITING on the redo list.
    (UNDO-SAVE-BEGIN (UNDO-ITEM-START-BP UNDO-ITEM) (UNDO-ITEM-END-BP UNDO-ITEM) T
                     "Undo" T)  ;This T is how we differ from WITH-UNDO-SAVE.
    (UNWIND-PROTECT
        (LET ((*BATCH-UNDO-SAVE* T))
          (WITH-BP (BP (UNDO-ITEM-START-BP UNDO-ITEM) ':NORMAL)
            (DELETE-INTERVAL BP (UNDO-ITEM-END-BP UNDO-ITEM) T)
            (INSERT-INTERVAL BP
                             (UNDO-ITEM-INTERVAL UNDO-ITEM))
            (RESTORE-BP-VALUES (UNDO-ITEM-SAVED-BP-VALUES UNDO-ITEM)
                               BP)))
      (UNDO-SAVE-END)))
  (SETF (UNDO-ITEM-TYPE (CAR (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))
        (UNDO-ITEM-TYPE UNDO-ITEM))
;  (OR (UNDO-ITEM-MODIFIED-FLAG UNDO-ITEM)
;      (NOT-MODIFIED (NODE-TOP-LEVEL-NODE *INTERVAL*)))
  ;; The first thing on (UNDO-STATUS-UNDO-LIST UNDO-STATUS) is for undoing this undo.
  ;; If everything else has been undone, leaving only the marker
  ;; put on saying the buffer had started out unmodified, then
  ;; the buffer must now be back at its unmodified text.
  (WHEN (EQ 'PREVIOUSLY-UNMODIFIED
            (CADR (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))
    (NOT-MODIFIED (NODE-TOP-LEVEL-NODE *INTERVAL*)))
  ;; Discard the PREVIOUSLY-UNMODIFIED flags we have come to.
  (DO () ((NOT (EQ 'PREVIOUSLY-UNMODIFIED
                   (CADR (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))))
    (SETF (CDR (UNDO-STATUS-UNDO-LIST UNDO-STATUS))
          (CDDR (UNDO-STATUS-UNDO-LIST UNDO-STATUS))))
  ;; Take our new undo-item (for undoing the undo) off the undo list.
  ;; Our caller will decide which list to put it on.
  (POP (UNDO-STATUS-UNDO-LIST UNDO-STATUS)))

;;; Show the current range of recorded small changes.
;;; Prints out the old saved interval.  Type space to flush it;
;;; then you see point at the beginning.  Do C-X C-X to see the end.
(DEFCOM COM-SHOW-CURRENT-RANGE "Testing" ()
  (LET ((UNDO-STATUS (NODE-UNDO-STATUS-OR-NIL (NODE-TOP-LEVEL-NODE *INTERVAL*))))
    (WHEN UNDO-STATUS
      (MOVE-BP (POINT) (UNDO-STATUS-START-BP UNDO-STATUS))
      (MOVE-BP (WINDOW-MARK *WINDOW*) (UNDO-STATUS-END-BP UNDO-STATUS))
      (PRINC (STRING-INTERVAL (UNDO-STATUS-INTERVAL UNDO-STATUS)))))
  DIS-BPS)

(DEFCOM COM-DISCARD-UNDO-INFORMATION "Throw away all records of changes to this buffer." ()
  (DISCARD-UNDO-INFORMATION (NODE-TOP-LEVEL-NODE *INTERVAL*))
  DIS-NONE)

(DEFUN DISCARD-UNDO-INFORMATION (NODE)
  "Throw away all records of changes made to NODE."
  (LET ((UNDO-STATUS (NODE-UNDO-STATUS-OR-NIL NODE)))
    (WHEN UNDO-STATUS
      (WHEN (UNDO-STATUS-START-BP UNDO-STATUS)
        (FLUSH-BP (UNDO-STATUS-START-BP UNDO-STATUS)))
      (WHEN (UNDO-STATUS-END-BP UNDO-STATUS)
        (FLUSH-BP (UNDO-STATUS-END-BP UNDO-STATUS)))
      (SETF (UNDO-STATUS-START-BP UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-END-BP UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-INTERVAL UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-INTERVAL-NUMBER-OF-LINES UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-SAVED-BP-VALUES UNDO-STATUS) NIL)
      (MAPC #'FLUSH-UNDO-ITEM (UNDO-STATUS-UNDO-LIST UNDO-STATUS))
      (MAPC #'FLUSH-UNDO-ITEM (UNDO-STATUS-REDO-LIST UNDO-STATUS))
      (SETF (UNDO-STATUS-REDO-LIST UNDO-STATUS) NIL)
      (SETF (UNDO-STATUS-UNDO-LIST UNDO-STATUS) NIL))))

(DEFUN SUMMARIZE-INTERVAL (BEG-BP &OPTIONAL END-BP IN-ORDER-P (SIZE 10.))
  "Return a brief summary of the text from BEG-BP to END-BP, or NIL if all whitespace.
Instead of two BPs, you can pass one arg, an interval.
SIZE is the length of text at beginning or end of interval
to actually include in the description."
  (GET-INTERVAL BEG-BP END-BP IN-ORDER-P)
  (LET ((BP1B (FORWARD-OVER *WHITESPACE-CHARS* BEG-BP END-BP)))
    (IF (BP-= BP1B END-BP)
        NIL
      (LET ((BP1E (BACKWARD-OVER *WHITESPACE-CHARS* END-BP BEG-BP))
            STRING)
        (SETQ STRING
              (IF (AND (EQ (BP-LINE BP1B) (BP-LINE BP1E))
                       (< (- (BP-INDEX BP1E) (BP-INDEX BP1B)) (+ SIZE SIZE)))
                  (STRING-INTERVAL BP1B BP1E)
                (LET ((BP2B (CREATE-BP (BP-LINE BP1B)
                                      (MIN (+ (BP-INDEX BP1B) SIZE)
                                           (LINE-LENGTH (BP-LINE BP1B)))))
                      (BP2E (CREATE-BP (BP-LINE BP1E)
                                       (MAX (- (BP-INDEX BP1E) SIZE)
                                            0))))
                  (STRING-APPEND (STRING-TRIM *WHITESPACE-CHARS*
                                              (STRING-INTERVAL BP1B BP2B T))
                                 " ... "
                                 (STRING-TRIM *WHITESPACE-CHARS*
                                              (STRING-INTERVAL BP2E BP1E T))))))
        (STRING-APPEND (IF (BP-= BEG-BP BP1B) "" "... ")
                       STRING
                       (IF (BP-= BP1E END-BP) "" " ..."))))))

;;; Parse a file, finding the defuns, adding the names to the completion Aarray and the
;;; symbols' property lists
;;; Note that this must be called with PACKAGE bound to the file's package,
;;; which is typically done by making the buffer current.
;;; If a stream is specified, we read that stream into the buffer until eof,
;;; a line at a time, and sectionize the stuff as we go.
;;; In that case, HACK-FONTS is passed on to INTERVAL-STREAM.

;;; If PROPERTY is non-NIL, we maintain on each function spec
;;; a property of that name, whose value is a list of buffers it is defined in.
;;; We add and remove entries in those lists for sections that appear and disappear.
(DEFUN SECTIONIZE-FILE-BUFFER (BUFFER &OPTIONAL AARRAY PROPERTY START-NODE END-NODE
                               STREAM HACK-FONTS
                                 &AUX (PACKAGE PACKAGE)
                                 (NODE-LIST NIL)
                                 (MODE (SEND BUFFER ':MAJOR-MODE))
                                 (*INTERVAL* BUFFER) INT-STREAM
                                 FIRST-BP LAST-BP ADDED-COMPLETIONS
                                 BUFFER-TICK OLD-CHANGED-SECTIONS
                                 NODES-TO-REUSE ACTUAL-NEW-NODES
                                 START-PREDECESSOR END-FOLLOWER)
  "Compute the sectionization of all or part of BUFFER.
If START-NODE and END-NODE are NIL, the whole buffer is resectionized from scratch.
If they are non-NIL, they should be sections of the buffer;
that portion of the buffer (inclusive!) is resectionized,
reusing any existing nodes if objects with the same names are still present."
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  (SETQ FIRST-BP (INTERVAL-FIRST-BP (OR START-NODE BUFFER))
        LAST-BP (IF END-NODE (COPY-BP (INTERVAL-LAST-BP END-NODE))
                  (INTERVAL-LAST-BP BUFFER)))
  ;; If operating on a specified range of sections,
  ;; from START-NODE to END-NODE inclusive,
  ;; put all those nodes on NODES-TO-REUSE.
  (IF START-NODE
      (DO ((N START-NODE (NODE-NEXT N)))
          (())
        (PUSH N NODES-TO-REUSE)
        (IF (EQ N END-NODE) (RETURN))))
  (SETQ END-FOLLOWER (IF END-NODE (NODE-NEXT END-NODE))
        START-PREDECESSOR (IF START-NODE (NODE-PREVIOUS START-NODE)))
  ;;Buffer must be a FILE-BUFFER, but need not be a real ZMACS BUFFER.
  (WHEN AARRAY
    (SETQ ADDED-COMPLETIONS (zl:MAKE-ARRAY #o100 :TYPE 'ART-Q-LIST :LEADER-LENGTH 2 :LEADER-LIST '(0))))
  (AND STREAM
       (SETQ INT-STREAM (INTERVAL-STREAM-INTO-BP LAST-BP HACK-FONTS)))
  ;; Make sure the buffer ends with an empty line.
;  (OR (ZEROP (BP-INDEX LAST-BP))
;      (INSERT LAST-BP #\CR))
  (SETQ BUFFER-TICK (BUFFER-TICK BUFFER))
  (TICK)
  ;; This is no longer needed for computing the NODE-TICK of sections,
  ;; since that can be determined from the text.
  ;; But it is still useful for remembering the compile-ticks.
  (OR NODES-TO-REUSE
      (DOLIST (NODE (NODE-INFERIORS BUFFER))
        (PUSH (CONS (SECTION-NODE-NAME NODE)
                    NODE)
              OLD-CHANGED-SECTIONS)))
  ;; Now scan the buffer and record the definitions.
  (DO* ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
        (LIMIT (IF (ZEROP (BP-INDEX LAST-BP))   ;Line to stop at (may be NIL)
                   (BP-LINE LAST-BP)
                 (LINE-NEXT (BP-LINE LAST-BP))))
        (EOFFLG)
        (BP (COPY-BP FIRST-BP))
        (PREV-NODE-START-BP FIRST-BP)
        (PREV-NODE-DEFUN-LINE NIL)
        (FIRST-NODE-NAME (IF START-NODE "Things deleted" "Buffer header"))
        (PREVIOUS-NODE NIL)
        (ADD-SECTIONS (GET MODE 'EDITING-TYPE))
        (SECTION-P (GET ADD-SECTIONS 'SECTION-P))
        (SECTION-NAME-FUNCTION (GET ADD-SECTIONS 'GET-SECTION-NAME)))
      (NIL)
    ;; If we have a stream, and we are at the limit, read another line.
    (WHEN (AND STREAM (EQ LINE LIMIT) (NOT EOFFLG))
      (LET ((DEFAULT-CONS-AREA *ZWEI-AREA*))
        (MULTIPLE-VALUE-SETQ (LINE EOFFLG)
          (SEND STREAM :LINE-IN LINE-LEADER-SIZE))
        (IF LINE (SETQ LINE (SEND INT-STREAM :LINE-OUT LINE)))
        (SETQ LIMIT (LINE-NEXT LINE))))
    ;; See if the line is the start of a defun.
    ;; If so, record the section that it terminates.
    (WHEN (AND ADD-SECTIONS
               (OR EOFFLG
                   (EQ LINE LIMIT)
                   (AND LINE (FUNCALL SECTION-P LINE))))
      (LET ((START PREV-NODE-START-BP)
            END OLD-NODE)
        (cond ((OR EOFFLG (EQ LINE LIMIT))
               (SETQ END LAST-BP))
              (t
               (MOVE-BP BP LINE 0)
               (SETQ END (COPY-BP BP))
          ;; Include one blank line before the form in the same section with it.
               (IF (AND (LINE-PREVIOUS (BP-LINE END))
                        (LINE-BLANK-P (LINE-PREVIOUS (BP-LINE END))))
                   (MOVE-BP END (LINE-PREVIOUS (BP-LINE END)) 0))
               (SETQ PREV-NODE-START-BP END)))
        (UNLESS (AND (NOT (OR EOFFLG (EQ LINE LIMIT)))
                     (OR (BP-= START END)
                         (AND (NOT PREV-NODE-DEFUN-LINE)
                              START-NODE
                              (NOT (EQ START-NODE (CAR (NODE-INFERIORS BUFFER)))))))
          ;; Now we have decided for certain to create a section ending here.
          ;; Extract the name of the section that is just being terminated.
          ;; By now, all the lines that the name runs over must have been read in.
          (MULTIPLE-VALUE-BIND (SYM STR ERR)
              (IF PREV-NODE-DEFUN-LINE
                  (FUNCALL SECTION-NAME-FUNCTION PREV-NODE-DEFUN-LINE BP)
                FIRST-NODE-NAME)
            (WHEN ERR
              (SETQ SYM "Unknown" STR NIL))
            (UNLESS ERR
              (SETQ OLD-NODE (CDR (SI:ASSOC-EQUAL SYM OLD-CHANGED-SECTIONS))))
            (SETQ PREVIOUS-NODE
                  (ADD-SECTION-NODE START END
                                    SYM PREV-NODE-DEFUN-LINE BUFFER PREVIOUS-NODE
                                    NIL
;                                   (IF OLD-NODE
;                                       (NODE-TICK OLD-NODE)
;                                     (IF STREAM BUFFER-TICK *TICK*))
                                    (IF OLD-NODE
                                        (SECTION-NODE-COMPILE-TICK OLD-NODE)
                                      BUFFER-TICK)
                                    NODES-TO-REUSE))
            (cond ((MEMQ PREVIOUS-NODE NODES-TO-REUSE)
                   (SETQ NODES-TO-REUSE (DELQ PREVIOUS-NODE NODES-TO-REUSE)))
                  (t
                   (PUSH PREVIOUS-NODE ACTUAL-NEW-NODES)
                   (WHEN (AND ADDED-COMPLETIONS (NOT (STRINGP SYM)))
                     (SECTION-COMPLETION SYM STR ADDED-COMPLETIONS)
                     (UNLESS (SYMBOLP SYM)
                       (SECTION-COMPLETION SYM
                                           (DEFINITION-NAME-AS-STRING NIL SYM)
                                           ADDED-COMPLETIONS)))))
            (PUSH PREVIOUS-NODE NODE-LIST))))
      (SETQ PREV-NODE-DEFUN-LINE LINE))
    ;; After processing the last line, exit.
    (WHEN (OR EOFFLG (EQ LINE LIMIT))
      (RETURN)))
  ;; If reading a stream, we should not have inserted a CR
  ;; after the eof line.
  (AND STREAM
       (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP BUFFER) -1 T)
                        (INTERVAL-LAST-BP BUFFER)
                        T))
  ;; Splice the nodes just made in with the nodes
  ;; before START-NODE and after END-NODE
  (LET ((FIRST-NEW-NODE (CAR (LAST NODE-LIST)))
        (FLUSHED-NODES (IF START-NODE NODES-TO-REUSE (NODE-INFERIORS BUFFER))))
    (COND (NODE-LIST
           (WHEN END-FOLLOWER
             (SETF (NODE-PREVIOUS END-FOLLOWER) (CAR NODE-LIST))
             (SETF (NODE-NEXT (CAR NODE-LIST)) END-FOLLOWER))
           (WHEN START-PREDECESSOR
             (SETF (NODE-NEXT START-PREDECESSOR) FIRST-NEW-NODE)
             (SETF (NODE-PREVIOUS FIRST-NEW-NODE) START-PREDECESSOR)))
          ((AND START-PREDECESSOR END-FOLLOWER)
           (SETF (NODE-NEXT START-PREDECESSOR) END-FOLLOWER)
           (SETF (NODE-PREVIOUS END-FOLLOWER) START-PREDECESSOR)))
    ;; Construct the new list of all inferiors of BUFFER.
    ;; Except: if all old nodes were reused, and no new ones made,
    ;; these lists are both still correct.
    (IF (OR FLUSHED-NODES ACTUAL-NEW-NODES)
        (LET (ALL-NODES)
          (DO ((N (IF END-FOLLOWER (CAR (LAST (NODE-INFERIORS BUFFER)))
                    (CAR NODE-LIST))
                  (NODE-PREVIOUS N)))
              ((NULL N))
            (PUSH N ALL-NODES))
          (SETF (NODE-INFERIORS BUFFER) ALL-NODES)))
    ;; Flush old section nodes that were not reused.
    (DOLIST (NODE FLUSHED-NODES)
      ;; Flush ZMACS-BUFFERS properties for old nodes not reused.
      (WHEN PROPERTY
        (LET ((THE-BUFFER BUFFER)
              (SYM (SECTION-NODE-NAME NODE)))
          (OR (STRINGP SYM)
              (CONDITION-CASE ()
                  (SI:FUNCTION-SPEC-PUTPROP
                   SYM
                   (DEL-IF #'(LAMBDA (DEFN) (EQ (CAR DEFN) THE-BUFFER))
                           (SI:FUNCTION-SPEC-GET SYM PROPERTY))
                   PROPERTY)
                (SYS:INVALID-FUNCTION-SPEC NIL)))))
      (FLUSH-BP (INTERVAL-FIRST-BP NODE))
      (FLUSH-BP (INTERVAL-LAST-BP NODE)))
    ;; Attach ZMACS-BUFFERS properties to the symbols defined herein.
    (WHEN PROPERTY
      (DOLIST (NODE ACTUAL-NEW-NODES)
        (UNLESS (STRINGP (SECTION-NODE-NAME NODE))
          (CONDITION-CASE ()
              (SI:FUNCTION-SPEC-PUSH-PROPERTY
               (SECTION-NODE-NAME NODE)
               (CONS BUFFER (SECTION-NODE-DEFUN-LINE NODE))
               PROPERTY)
            (SYS:INVALID-FUNCTION-SPEC NIL))))))
  ;; Merge new entries into the aarray.
  (WHEN ADDED-COMPLETIONS
    ;; Copy all the completion entries now, so they all go on one page.
    (LET ((I (ARRAY-LEADER ADDED-COMPLETIONS 0)))
      (UNLESS (ZEROP I)
        (DOTIMES (J I)
          (SETF (AREF ADDED-COMPLETIONS J)
                (CONS (STRING-APPEND (CAR (AREF ADDED-COMPLETIONS J)))
                      (CDR (AREF ADDED-COMPLETIONS J)))))
        ;; Sort them and merge them into the main list.
        (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
        (MERGE-COMPLETION-AARRAY AARRAY ADDED-COMPLETIONS)))))

(DEFUN SECTION-COMPLETION (THING &OPTIONAL STRING MERGING-AARRAY (EXTEND-BY #o100))
  "Add an entry to the unsorted completion aarray MERGING-AARRAY.
THING is the object name, and STRING is string for it to be recognized by."
  (OR STRING (SETQ STRING (STRING THING)))
  (VECTOR-PUSH-EXTEND (CONS STRING (IF (CONSP THING) (NCONS THING) THING))
                     MERGING-AARRAY
                     EXTEND-BY))

;;; Add a new node with one section in it
;;; Reuse one of NODES-TO-REUSE if it has the same name;
;;; in that case update the pointers in it but don't change the ticks.
(DEFUN ADD-SECTION-NODE (START-BP END-BP NAME DEFUN-LINE BUFFER PREVIOUS-NODE
                         IGNORE TICK1 &OPTIONAL NODES-TO-REUSE &AUX NODE)
  (unless (numberp (line-tick (bp-line end-bp)))
    (ferror "Creating section node with end on a deleted line.
Please make a detailed report of the sequence of editing commands
leading up to this event."))
  (unless (numberp (line-tick (bp-line start-bp)))
    (ferror "Creating section node with start on a deleted line.
Please make a detailed report of the sequence of editing commands
leading up to this event."))
  (DOLIST (NODE1 NODES-TO-REUSE)
    (WHEN (EQUAL (SECTION-NODE-NAME NODE1) NAME)
      (MOVE-BP (INTERVAL-FIRST-BP NODE1) START-BP)
      (MOVE-BP (INTERVAL-LAST-BP NODE1) END-BP)
      (SETF (NODE-PREVIOUS NODE1) PREVIOUS-NODE)
      (SETF (NODE-NEXT NODE1) NIL)
      (SETF (SECTION-NODE-DEFUN-LINE NODE1) DEFUN-LINE)
      (SETF (SECTION-NODE-SECTIONIZE-TICK NODE1) *TICK*)
      (SETQ NODE NODE1)
      (RETURN)))
  (OR NODE
      (SETQ NODE (MAKE-SECTION-NODE-1 (COPY-BP START-BP :NORMAL) (COPY-BP END-BP :MOVES)
                                      *TICK* PREVIOUS-NODE BUFFER NAME DEFUN-LINE TICK1)))
  (AND PREVIOUS-NODE
       (SETF (NODE-NEXT PREVIOUS-NODE) NODE))
  ;; Tell all the lines that they belong to this node.
  ;; Also find the tick of the last change within the node
  ;; to be the node's tick.
  (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
       (FIRST-LINE (BP-LINE START-BP))
       (MAX-TICK 0)
       (LIMIT (BP-LINE END-BP)))
      ((EQ LINE LIMIT)
       (SETF (NODE-TICK NODE)
             (MAX MAX-TICK
                  (OR (GETF (LINE-PLIST LINE) 'PRECEDING-LINES-DELETED-TICK)
                      0)))
       (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
           (SETF (LINE-NODE LINE) NODE)))
    (SETQ MAX-TICK
          (MAX MAX-TICK
               (LINE-TICK LINE)
               (OR (AND (NEQ LINE FIRST-LINE)
                        (GETF (LINE-PLIST LINE) 'PRECEDING-LINES-DELETED-TICK))
                   0)))
    (SETF (LINE-NODE LINE) NODE))
  NODE)

(DEFUN MAKE-SECTION-NODE-1 (FIRST-BP LAST-BP TICK PREVIOUS SUPERIOR
                            NAME DEFUN-LINE COMPILE-TICK)
  (MAKE-SECTION-NODE FIRST-BP LAST-BP TICK NIL PREVIOUS SUPERIOR NIL
                     NIL NIL NIL
                     NAME DEFUN-LINE COMPILE-TICK *TICK*))

(DEFUN RESECTIONIZE-FILE-BUFFER (BUFFER &OPTIONAL AARRAY PROPERTY
                                 START-NODE END-NODE)
  "Make sectionization of BUFFER completely up to date, scanning only changed parts.
Returns non-NIL if something needed to be changed.
AARRAY is an aarray to record any newly added nodes in, or NIL for none.
PROPERTY is the name of a property which we maintain
 for each function spec whose definitions are found;
 the property contains a list of buffers which define that spec.
 If PROPERTY is NIL, we don't maintain any property.
START-NODE and END-NODE are subnodes of BUFFER that serve as a hint,
 bounding the range of subnodes that need to be considered.
 END-NODE will be the last node actually considered."
  (IF (NULL (NODE-INFERIORS BUFFER))
      (PROGN
        (SECTIONIZE-FILE-BUFFER BUFFER AARRAY PROPERTY)
        T)
    (DO ((NODE (OR START-NODE (CAR (NODE-INFERIORS BUFFER))) (NODE-NEXT NODE))
         (STOP-NODE (IF END-NODE (NODE-NEXT (NEXT-CLEAN-SECTION-BREAK END-NODE))))
         (PREV NIL NODE)
         SOMETHING-CHANGED-FLAG)
        ((OR (NULL NODE)
             (EQ NODE STOP-NODE))
         SOMETHING-CHANGED-FLAG)
      (UNLESS (AND ( (SECTION-NODE-SECTIONIZE-TICK NODE)
                      (NODE-TICK NODE))
                   (OR (NULL PREV)
                       (BP-= (INTERVAL-FIRST-BP NODE)
                             (INTERVAL-LAST-BP PREV))))
        (LET ((NODE1 (NEXT-CLEAN-SECTION-BREAK NODE)))
          (SECTIONIZE-FILE-BUFFER BUFFER AARRAY PROPERTY
                                  (PREVIOUS-CLEAN-SECTION-BREAK NODE) NODE1)
          (SETQ SOMETHING-CHANGED-FLAG T)
          (SETQ NODE NODE1))))))

(DEFUN PREVIOUS-CLEAN-SECTION-BREAK (NODE)
  "Trace back through previous nodes of NODE until a clean break between nodes.
A clean break is one where a node and its successor have no overlap.
We return the node that follows the clean break."
  (DO ((NODE1 NODE (NODE-PREVIOUS NODE1)))
      ((OR (NULL (NODE-PREVIOUS NODE1))
           ;; Also avoid returning a node which begins where NODE ends.
           (AND (NOT (BP-= (INTERVAL-FIRST-BP NODE1) (INTERVAL-LAST-BP NODE)))
                (BP-= (INTERVAL-LAST-BP (NODE-PREVIOUS NODE1))
                      (INTERVAL-FIRST-BP NODE1))))
       NODE1)))

(DEFUN NEXT-CLEAN-SECTION-BREAK (NODE)
  "Trace back through following nodes of NODE until a clean break between nodes.
A clean break is one where a node and its successor have no overlap.
We return the node that precedes the clean break."
  (DO ((NODE1 NODE (NODE-NEXT NODE1)))
      ((OR (NULL (NODE-NEXT NODE1))
           ;; Also avoid returning a node which ends where NODE begins.
           (AND (NOT (BP-= (INTERVAL-FIRST-BP NODE) (INTERVAL-LAST-BP NODE1)))
                (BP-= (INTERVAL-FIRST-BP (NODE-NEXT NODE1))
                      (INTERVAL-LAST-BP NODE1))))
       NODE1)))

(DEFUN BP-NEXT-SUBORDINATE-NODE (BP &AUX (TOP-LEVEL-NODE (BP-TOP-LEVEL-NODE BP)))
  "Like BP-NODE but can deal with lines whose LINE-NODE is the buffer itself.
If this happens, we look FORWARD till we find a line which really has a node.
The buffer itself is returned only if we reach the end and don't find an inferior."
  (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE)))
      ((NULL LINE) TOP-LEVEL-NODE)
    (LET ((NODE (LINE-NODE LINE)))
      (IF (NEQ NODE TOP-LEVEL-NODE) (RETURN NODE) (REPORT-BP-LOSSAGE)))))

;;; This function is only temporary until I figure out how to
;;; prevent the weird state (a line whose LINE-NODE is the buffer) from occurring.
(DEFVAR ALREADY-REPORTED-BP-LOSSAGE NIL)

(DEFPROP REPORT-BP-LOSSAGE T :ERROR-REPORTER)
(DEFUN REPORT-BP-LOSSAGE ()
  (UNLESS ALREADY-REPORTED-BP-LOSSAGE
    (SETQ ALREADY-REPORTED-BP-LOSSAGE T)
    (CERROR "Try to continue." "A weird state has been encountered.~%~A type ~c."
            (CASE SI:SITE-NAME
              (:MIT "If MLY is logged in, get him immediately.
Otherwise,")
              (:LMI "If a Zwei wizard is logged in (like MLY), get him immediately.
You might also try send a bug report with c-M.
To resume from this point")
              (OTHERWISE "Please call LMI Customer Service and report this occurrence.
If a Zwei wizard is available at the time, he/she may want to
talk to you about the current state.
To continue after contacting LMI"))
            #\Resume)))

(DEFUN BP-PREV-SUBORDINATE-NODE (BP
                                 &AUX
                                 (TOP-LEVEL-NODE (BP-TOP-LEVEL-NODE BP)))
  "Like BP-NODE but can deal with lines whose LINE-NODE is the buffer itself.
If this happens, we look BACKWARD till we find a line which really has a node.
The buffer itself is returned only if we reach the beginning and don't find an inferior."
  (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE)))
      ((NULL LINE) TOP-LEVEL-NODE)
    (LET ((NODE (LINE-NODE LINE)))
      (IF (NEQ NODE TOP-LEVEL-NODE) (RETURN NODE)
        (REPORT-BP-LOSSAGE)))))

(DEFUN SECTION-SECTIONIZATION-VALID-P (NODE)
  "T if section NODE does not need resectionization to be valid.
In other words, T if you can assume that the boundaries of NODE
do correctly delimit the definition of the object NODE claims to hold."
  (OR ( (SECTION-NODE-SECTIONIZE-TICK NODE)
         (NODE-TICK NODE))
      ;; If the section breaks around this section are clean,
      ;; and the only section-starting line
      ;; is the one that started this section,
      ;; and that line is unchanged, then we win.
      (AND (OR (NULL (NODE-PREVIOUS NODE))
               (BP-= (INTERVAL-LAST-BP (NODE-PREVIOUS NODE))
                     (INTERVAL-FIRST-BP NODE)))
           (OR (NULL (NODE-NEXT NODE))
               (BP-= (INTERVAL-LAST-BP NODE)
                     (INTERVAL-FIRST-BP (NODE-NEXT NODE))))
           ;; Don't hack this for Buffer Header nodes.
           (SECTION-NODE-DEFUN-LINE NODE)
           (NUMBERP (LINE-TICK (SECTION-NODE-DEFUN-LINE NODE)))
           ( (SECTION-NODE-SECTIONIZE-TICK NODE)
              (LINE-TICK (SECTION-NODE-DEFUN-LINE NODE)))
           (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP NODE))
                      (LINE-NEXT LINE))
                (DEFUN-LINE (SECTION-NODE-DEFUN-LINE NODE))
                (END-LINE (BP-LINE (INTERVAL-LAST-BP NODE)))
                (SECTION-P
                  (GET (GET (SEND (NODE-TOP-LEVEL-NODE NODE) ':MAJOR-MODE)
                            'EDITING-TYPE)
                       'SECTION-P)))
               ((EQ LINE END-LINE) T)
             (WHEN (NEQ (EQ LINE DEFUN-LINE)
                        (FUNCALL SECTION-P LINE))
               (RETURN NIL))))))

(DEFUN CHECK-INTERVAL-SECTIONS (START-BP &OPTIONAL END-BP IN-ORDER-P)
  "Fix up sectionization  of specified interval, where text has changed.
The specified interval should be all or part of *INTERVAL*."
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
;  (LET ((FOO (NODE-INFERIORS *INTERVAL*))
;        (INT *INTERVAL*))
;    (DOLIST (N FOO)
;      (IF (NOT (NUMBERP (LINE-TICK (BP-LINE (INTERVAL-FIRST-BP N)))))
;         (FERROR "First-bp of ~S is deleted.  This bug is on the most-wanted list.
;Please make a detailed report." N))
;      (IF (NOT (NUMBERP (LINE-TICK (BP-LINE (INTERVAL-LAST-BP N)))))
;         (FERROR "Last-bp of ~S is deleted.  This bug is on the most-wanted list.
;Please make a detailed report." N))))
  (IF (NODE-INFERIORS *INTERVAL*)
      (WHEN (TYPEP (CAR (NODE-INFERIORS *INTERVAL*)) 'SECTION-NODE)
        ;; Don't bother trying if the subnodes of *INTERVAL* are not from sectionzation.
        (LET* ((START-NODE (PREVIOUS-CLEAN-SECTION-BREAK (BP-PREV-SUBORDINATE-NODE START-BP)))
               ;; If END-BP points at the boundary of two nodes,
               ;; (BP-NODE END-BP) is the following node; but we really don't
               ;; need to include that node if the break at END-BP is clean.
               (END-NODE
                 (NEXT-CLEAN-SECTION-BREAK
                   (IF (BP-= START-BP END-BP)
                       START-NODE
                     (LET ((ENDNODE1 (BP-NEXT-SUBORDINATE-NODE END-BP)))
                       (IF (BP-= END-BP (INTERVAL-FIRST-BP ENDNODE1))
                           (NODE-PREVIOUS ENDNODE1)
                         ENDNODE1))))))
          (IF (OR (EQ START-NODE *INTERVAL*) (EQ END-NODE *INTERVAL*))
              ;; Can this really happen??
              (SECTIONIZE-BUFFER *INTERVAL*)
            ;; No need to do anything if all the nodes are unchanged
            ;; since last resectionize.
            (UNLESS (DO ((NODE START-NODE (NODE-NEXT NODE)))
                        ((NULL NODE) T)
                      (OR ( (SECTION-NODE-SECTIONIZE-TICK NODE)
                             (NODE-TICK NODE))
                          (SECTION-SECTIONIZATION-VALID-P NODE)
                          ;; Unless it is clean and unchanged, resectionize it.
                          (RETURN NIL))
                      (IF (EQ NODE END-NODE) (RETURN T)))
              (RESECTIONIZE-BUFFER *INTERVAL* START-NODE END-NODE)))))
    (SECTIONIZE-BUFFER *INTERVAL*)))

;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Buffer handling, definition are in DEFS
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;; An invalid Enhancements copyright notice on AI:ZMAIL;MFILES 236 removed on 5/7/82 by RG
;;;  This file had been installed as the official MIT source in direct contravention
;;;  to instructions to Symbolics personnel acting on MIT's behalf.

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* ZMAIL-BUFFER "Select"
                          SELECT-ZMAIL-BUFFER T
                          "Select this buffer.")

(DEFUN SELECT-ZMAIL-BUFFER (ZMAIL-BUFFER &OPTIONAL PRIMARY-TOO (MSG-TOO T))
  "Make ZMAIL-BUFFER be ZMAIL's selected buffer (the value of *ZMAIL-BUFFER*).
MSG-TOO non-NIL (the default) means also reselect a message, *MSG* if possible.
PRIMARY-TOO non-NIL says make this buffer the primary buffer."
  (COND (ZMAIL-BUFFER
         (COND ((AND *ZMAIL-BUFFER* *MSG*)
                (SETF (ZMAIL-BUFFER-SAVED-CURRENT-MSG *ZMAIL-BUFFER*) *MSG*)
                (MSG-POINT-PDL-PUSH *MSG* *ZMAIL-BUFFER*)))
         (OR (TYPEP ZMAIL-BUFFER 'INBOX-BUFFER)
             (SETQ *ZMAIL-BUFFER-LIST* (CONS ZMAIL-BUFFER (DELQ ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*))))
         (SETQ *ZMAIL-FILE-NAME* (ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))
        (T
         (SETQ *ZMAIL-FILE-NAME* "No current buffer")))
  (SEND *SUMMARY-WINDOW* :SET-CURRENT-ZMAIL-BUFFER ZMAIL-BUFFER)
  (AND PRIMARY-TOO (SETQ *PRIMARY-ZMAIL-BUFFER* ZMAIL-BUFFER))
  (SETQ *ZMAIL-BUFFER* ZMAIL-BUFFER)
  (SETQ *INTERVAL* ZMAIL-BUFFER)
  (DOLIST (COM '(COM-ZMAIL-SELECT COM-ZMAIL-SORT COM-ZMAIL-RENAME-BUFFER
                 SET-MSG-SUMMARY-LINE))
    (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM))
  (AND MSG-TOO (ZMAIL-SELECT-MSG *MSG* T NIL)))

(DEFUN GET-ZMAIL-BUFFER-FROM-NAME (NAME &OPTIONAL CREATE-P)
  "Given a string, return ZMAIL buffer with that name.
CREATE-P says create one (parsing name as pathname) if none.
Otherwise return NIL in that case."
  (COND ((DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
           (AND (STRING-EQUAL NAME (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))
                (RETURN ZMAIL-BUFFER))))
        (CREATE-P
         (ZMAIL-FIND-FILE-NOSELECT
           (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
             (FS:MERGE-PATHNAME-DEFAULTS NAME *ZMAIL-PATHNAME-DEFAULTS*))))))

(DEFUN GET-ZMAIL-BUFFER-FROM-PATHNAME (PATHNAME &OPTIONAL CREATE-P)
  "Return the ZMAIL buffer visiting PATHNAME.
CREATE-P non-NIL says visit the file if there is none.
Otherwise return NIL in that case."
  (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
    (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *ZMAIL-PATHNAME-DEFAULTS*)))
  (COND ((DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
           (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
                (EQ (FS:TRANSLATED-PATHNAME PATHNAME) (BUFFER-PATHNAME ZMAIL-BUFFER))
                (RETURN ZMAIL-BUFFER))))
        (CREATE-P
         (ZMAIL-FIND-FILE-NOSELECT PATHNAME))))

(DEFUN MSG-IN-ZMAIL-BUFFER-P (MSG ZMAIL-BUFFER &AUX ARRAY)
  "T if MSG is one of the messages in ZMAIL-BUFFER.
Note that a message can be in more than one ZMAIL buffer;
only one non temporary one, but any number of temporaries."
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY)))
      (( I NMSGS))
    (AND (EQ MSG (AREF ARRAY I))
         (RETURN I))))

(DEFUN LOCATE-MSG-IN-ZMAIL-BUFFER (MSG ZMAIL-BUFFER &AUX HINT)
  "Return the index of MSG in ZMAIL-BUFFER.  Error if not in that buffer."
  (COND ((AND (SETQ HINT (MSG-DISPLAYED-INDEX MSG))
              (< HINT (ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER))
              (EQ MSG (AREF (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER) HINT)))
         HINT)
        ((MSG-IN-ZMAIL-BUFFER-P MSG ZMAIL-BUFFER))
        (T
         (ZMAIL-ERROR "Cannot find ~S in ~S" MSG ZMAIL-BUFFER))))

(DEFUN MAKE-EMPTY-MSG (&AUX REAL-INTERVAL INTERVAL START-BP END-BP MSG)
  "Return a new empty MSG with no text in it."
  (SETQ REAL-INTERVAL (CREATE-INTERVAL)
        START-BP (COPY-BP (INTERVAL-FIRST-BP REAL-INTERVAL) :NORMAL)
        END-BP (COPY-BP (INTERVAL-LAST-BP REAL-INTERVAL) :MOVES)
        INTERVAL (CREATE-INTERVAL START-BP END-BP)
        MSG (MAKE-MSG :TICK (TICK)
                      :REAL-INTERVAL REAL-INTERVAL
                      :INTERVAL INTERVAL))
  (SETF (NODE-TICK (MSG-INTERVAL MSG)) (MSG-TICK MSG))
  (SETF (NODE-TICK (MSG-REAL-INTERVAL MSG)) (MSG-TICK MSG))
  (SETF (LINE-NODE (BP-LINE END-BP)) INTERVAL)
  (SETF (NODE-SUPERIOR INTERVAL) REAL-INTERVAL)
  (SETF (NODE-INFERIORS REAL-INTERVAL) (LIST INTERVAL))
  MSG)

(DEFVAR *PROPERTIES-NOT-COPIED* '(DELETED FILED))

(DEFUN COPY-MSG (MSG &AUX NMSG)
  "Return a copy of MSG."
  (SETQ NMSG (MAKE-EMPTY-MSG))
  (SETF (MSG-STATUS NMSG)
        (SOME-PLIST-NOT (CAR (ASSURE-MSG-PARSED MSG)) *PROPERTIES-NOT-COPIED*))
  (SETF (MSG-PARSED-P NMSG) T)
  (LET* ((OLD-LINE (MSG-SUMMARY-LINE MSG))
         (NEW-LINE (MAKE-SUMMARY-LINE :MAKE-ARRAY (:LENGTH (ARRAY-ACTIVE-LENGTH OLD-LINE)))))
    (SETF (MSG-SUMMARY-LINE NMSG) NEW-LINE)
    (COPY-ARRAY-CONTENTS-AND-LEADER OLD-LINE NEW-LINE))
  (SETF (NODE-UNDO-STATUS (MSG-REAL-INTERVAL NMSG)) :DONT)
  (INSERT-INTERVAL (MSG-END-BP NMSG) (MSG-INTERVAL MSG))
  ;; Mpdify the HEADERS-END-BP property in the MSG-STATUS
  ;; so that it points into the copied text.
  ;; Try to preserve where it points.
  (LET ((HEADERS-END-BP (GET (LOCF (MSG-STATUS MSG)) 'HEADERS-END-BP)))
    (WHEN HEADERS-END-BP
      (DO ((OLINE (BP-LINE (INTERVAL-FIRST-BP (MSG-INTERVAL MSG))) (LINE-NEXT OLINE))
           (NLINE (BP-LINE (INTERVAL-FIRST-BP (MSG-INTERVAL NMSG))) (LINE-NEXT NLINE)))
          ((OR (NULL OLINE) (NULL NLINE))
           (REMPROP (LOCF (MSG-STATUS NMSG)) 'HEADERS-END-BP))
        ;; When we find the line in the old interval that the old bp points to,
        ;; make the new bp point at the corresponding new line.
        (WHEN (EQ OLINE (CAR HEADERS-END-BP))
          (PUTPROP (LOCF (MSG-STATUS NMSG)) (CREATE-BP NLINE (BP-INDEX HEADERS-END-BP))
                   'HEADERS-END-BP)
          (RETURN)))))
  NMSG)

;;; Changing the options of a mail file.

(DEFVAR *ZMAIL-BUFFER-FLAVOR-ALIST* NIL
  "Alist of pretty names of ZMAIL flavors vs actual names.
For user in choosing a flavor as part of setting file options.")

(DEFUN ADD-ZMAIL-BUFFER-FLAVOR (FLAVOR NAME &AUX ELEM)
  (IF (SETQ ELEM (ASS 'EQUALP NAME *ZMAIL-BUFFER-FLAVOR-ALIST*))
      (SETF (CDR ELEM) FLAVOR)
    (PUSH (CONS NAME FLAVOR) *ZMAIL-BUFFER-FLAVOR-ALIST*)))

(DEFVAR *ZMAIL-BUFFER-OPTION-ALIST* NIL)

(DEFMACRO DEFINE-SETTABLE-MAIL-FILE-OPTION (OPTION DEFAULT &OPTIONAL TYPE NAME &REST ARGS)
  `(TV:DEFINE-USER-OPTION-1 ',OPTION '*ZMAIL-BUFFER-OPTION-ALIST* ,DEFAULT
                            ',(OR TYPE :SEXP)
                            ',(OR NAME (MAKE-COMMAND-NAME OPTION)) . ,ARGS))

(DEFMACRO DEFINE-NOT-SETTABLE-MAIL-FILE-OPTION (OPTION)
  `(DEFINE-NOT-SETTABLE-MAIL-FILE-OPTION-1 ',OPTION))

(DEFUN DEFINE-NOT-SETTABLE-MAIL-FILE-OPTION-1 (OPTION)
  (OR (ASSQ OPTION *ZMAIL-BUFFER-OPTION-ALIST*)
      (PUSH (NCONS OPTION) *ZMAIL-BUFFER-OPTION-ALIST*)))

(DEFINE-SETTABLE-MAIL-FILE-OPTION :APPEND NIL :BOOLEAN
  "Append messages moved into file")

(DEFUN CHOOSE-MAIL-FILE-OPTIONS (ZMAIL-BUFFER &AUX OLD-FLAVOR OLD-APPEND-P PATHNAME FLAVOR
                                 APPEND-P OPTIONS PLIST OLD-SUMMARY-FORMAT WANT-TO-REVERSE)
  (SETQ OLD-FLAVOR (TYPE-OF ZMAIL-BUFFER)
        PATHNAME (BUFFER-PATHNAME ZMAIL-BUFFER)
        OPTIONS (ZMAIL-BUFFER-OPTIONS ZMAIL-BUFFER)
        PLIST (LOCF OPTIONS)
        OLD-APPEND-P (GET PLIST :APPEND)
        OLD-SUMMARY-FORMAT (GET PLIST :SUMMARY-WINDOW-OPTIONS))
  (*CATCH 'ZWEI-COMMAND-LOOP                    ;In case aborted
    (MULTIPLE-VALUE (FLAVOR OPTIONS)
      (CHOOSE-MAIL-FILE-OPTIONS-1 PATHNAME OLD-FLAVOR OPTIONS))
    (SETQ APPEND-P (GET PLIST :APPEND))
    (AND (NEQ APPEND-P OLD-APPEND-P)
         (> (ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER) 1)
         (TYPEOUT-BEEP-YES-OR-NO-P "Reverse the messages already in ~A? " PATHNAME)
         (SETQ WANT-TO-REVERSE T))
    (COND ((NEQ FLAVOR OLD-FLAVOR)
           (REMPROP PLIST :APPEND)
           (LET ((NEW-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER FLAVOR
                                                      :PATHNAME PATHNAME :FILE-ID T
                                                      :OPTIONS OPTIONS :APPEND-P APPEND-P)))
             ;; ADD-ZMAIL-BUFFER adds messages in forward order, so that if the file is
             ;; prepended, they would be reversed accidentally.  Similary, if now in append
             ;; mode, must reverse first to get old prepend mode messages right.
             (AND (EQ WANT-TO-REVERSE APPEND-P)
                  (REVERSE-ZMAIL-BUFFER ZMAIL-BUFFER))
             (SEND NEW-ZMAIL-BUFFER :ADD-ZMAIL-BUFFER ZMAIL-BUFFER)
             (LET ((OLD-SELECTED-P (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*))
                   (OLD-PRIMARY-P (EQ ZMAIL-BUFFER *PRIMARY-ZMAIL-BUFFER*)))
               (KILL-ZMAIL-BUFFER ZMAIL-BUFFER)
               (AND OLD-SELECTED-P (SELECT-ZMAIL-BUFFER NEW-ZMAIL-BUFFER OLD-PRIMARY-P)))))
          (T
           (AND WANT-TO-REVERSE (REVERSE-ZMAIL-BUFFER ZMAIL-BUFFER))
           (SEND ZMAIL-BUFFER :SET-OPTIONS OPTIONS))))
  (OR (EQ OLD-SUMMARY-FORMAT (GET PLIST :SUMMARY-WINDOW-OPTIONS))
      (CHANGE-ZMAIL-BUFFER-MSGS-SUMMARY-LINES ZMAIL-BUFFER))
  (AND (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
       (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-SORT)))

(DEFUN CHOOSE-MAIL-FILE-OPTIONS-1 (PATHNAME FLAVOR OPTIONS &OPTIONAL (NEAR-MODE '(:MOUSE))
                                                           &AUX VARS VALS)
  (DECLARE (RETURN-LIST FLAVOR OPTIONS))
  (SETQ VARS (NCONS 'FLAVOR)
        VALS (NCONS FLAVOR))
  (LOOP FOR POSS IN *ZMAIL-BUFFER-OPTION-ALIST*
        AS VAR = (CAR POSS)
        AS TEM = (NCONS VAR)
        WITH PLIST = (LOCF OPTIONS)
        DO (SETQ VARS (NCONC VARS TEM))
           (SETQ VALS (NCONC VALS (NCONS (GET PLIST (CAR TEM) (GET VAR 'TV:DEFAULT-VALUE))))))
  (LET ((*POSSIBLE-FLAVORS*
          (LOOP FOR FLAVOR IN (SEND PATHNAME :POSSIBLE-MAIL-FILE-BUFFER-FLAVORS)
                COLLECT (RASSQ FLAVOR *ZMAIL-BUFFER-FLAVOR-ALIST*) INTO FOO
                FINALLY (RETURN (NCONC FOO (NCONS (RASSQ 'TEXT-MAIL-FILE-BUFFER
                                                         *ZMAIL-BUFFER-FLAVOR-ALIST*)))))))
    (DECLARE (SPECIAL *POSSIBLE-FLAVORS*))
    (PROGV VARS VALS
      (TV:CHOOSE-VARIABLE-VALUES (COMPUTE-ZMAIL-BUFFER-CHOICES FLAVOR)
                                 :LABEL (FORMAT NIL "Options for ~A:" PATHNAME)
                                 :NEAR-MODE NEAR-MODE
                                 :MARGIN-CHOICES '("Do It"
                                                    ("Abort" (ABORT-CURRENT-COMMAND)))
                                 :FUNCTION 'CHOOSE-MAIL-FILE-OPTIONS-FUNCTION)
      (SETQ FLAVOR (SYMEVAL 'FLAVOR))
      ;; Somewhat of a kludge
      (AND (MEMQ :MAIL VARS)
           (DO L (SYMEVAL :MAIL) (CDR L) (NULL L)
               (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                 (SETF (CAR L) (FS:MERGE-PATHNAME-DEFAULTS (CAR L) *ZMAIL-PATHNAME-DEFAULTS*)))))
      (SETQ OPTIONS
            (LOOP FOR VAR IN (LET (SELF)
                               (SEND (SI:GET-FLAVOR-HANDLER-FOR FLAVOR :POSSIBLE-OPTIONS)
                                        :POSSIBLE-OPTIONS))
                  NCONC `(,VAR ,(SYMEVAL VAR))))))
  (VALUES FLAVOR OPTIONS))

(DEFUN CHOOSE-MAIL-FILE-OPTIONS-FUNCTION (WINDOW VARIABLE OLDVAL NEWVAL)
  OLDVAL
  (COND ((EQ VARIABLE 'FLAVOR)
         (TV:WITH-SHEET-DEEXPOSED (WINDOW)
           (SEND WINDOW :SETUP (COMPUTE-ZMAIL-BUFFER-CHOICES NEWVAL)
                    (SEND WINDOW :LABEL)
                    (SEND WINDOW :FUNCTION)
                    (SYMEVAL-IN-INSTANCE WINDOW 'TV:MARGIN-CHOICES)))
         T)))

(DEFUN COMPUTE-ZMAIL-BUFFER-CHOICES (FLAVOR)
  (DECLARE (SPECIAL *POSSIBLE-FLAVORS*))
  `((FLAVOR "Format" :ASSOC ,*POSSIBLE-FLAVORS*)
    . ,(MAPCAR #'(LAMBDA (X) (ASSQ X *ZMAIL-BUFFER-OPTION-ALIST*))
               (LET (SELF)
                 (SEND (SI:GET-FLAVOR-HANDLER-FOR FLAVOR :SETTABLE-OPTIONS)
                          :SETTABLE-OPTIONS)))))

;;; Operating on ZMAIL buffers.

(DEFUN MAKE-ZMAIL-BUFFER (TYPE &REST OPTIONS)
  (APPLY 'MAKE-INSTANCE TYPE OPTIONS))

(DEFMETHOD (ZMAIL-BUFFER :AFTER :INIT) (PLIST)
  (SETQ ARRAY (MAKE-ARRAY 100 :FILL-POINTER 0 :area *zmail-msg-area*))
  (AND (GET PLIST :APPEND-P)
       (PUTPROP (LOCF OPTIONS) T :APPEND)))

(DEFMETHOD (ZMAIL-BUFFER :WINDOWS) ()
  (SUBSET #'(LAMBDA (WINDOW) (EQ SELF (WINDOW-INTERVAL WINDOW)))
          *ALL-ZMAIL-WINDOWS*))

(DEFMETHOD (ZMAIL-BUFFER :OTHER-WINDOWS) (WINDOW)
  (DECLARE (SPECIAL WINDOW))
  (SUBSET #'(LAMBDA (W) (AND (NEQ W WINDOW) (EQ SELF (WINDOW-INTERVAL W))))
          *ALL-ZMAIL-WINDOWS*))

(DEFMETHOD (ZMAIL-BUFFER :POSSIBLE-WINDOWS) ()
  *ALL-ZMAIL-WINDOWS*)

(DEFMETHOD (ZMAIL-BUFFER :POINT) ()
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :MARK) ()
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :RENAME) (NEW-NAME)
  (SETQ NAME NEW-NAME)
  (IF (EQ SELF *ZMAIL-BUFFER*)
      (SETQ *ZMAIL-FILE-NAME* NEW-NAME)))

(DEFMETHOD (ZMAIL-BUFFER :REVERT) (&REST IGNORE)
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :ACTIVATE) ()
  (SETQ *ZMAIL-BUFFER-LIST* (NCONC *ZMAIL-BUFFER-LIST* (NCONS SELF)))
  ;; We may make another buffer available for left on Select.
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-SELECT))

(DEFMETHOD (ZMAIL-BUFFER :SELECT) ()
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :SECTIONIZE) ()
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :RESECTIONIZE) ()
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :SET-READ-ONLY-P) (IGNORE)
  (ZMAIL-ERROR "Should not get here."))

(DEFMETHOD (ZMAIL-BUFFER :MODIFIED-P) ()
  "T if ZMAIL-BUFFER has unsaved changes."
  (DOMSGS (MSG SELF)
    (SEND SELF :UPDATE-MSG-OPTIONS-IN-FILE-IF-NECESSARY MSG))
  (COND ((ZMAIL-BUFFER-DISK-P SELF)
         (SETF (ZMAIL-DISK-BUFFER-MSG-UPDATE-TICK SELF) (TICK))
         ;; This may require that the file be saved out.
         (SEND SELF :UPDATE-OPTIONS-IN-FILE)
         (OR (EQ (BUFFER-FILE-ID SELF) T)
             (> (NODE-TICK SELF)
                (BUFFER-TICK SELF))))))

(DEFMETHOD (ZMAIL-BUFFER :KILL) ()
  (SETQ *ZMAIL-BUFFER-LIST* (DELQ SELF *ZMAIL-BUFFER-LIST*))
  (MSG-POINT-PDL-PURGE NIL SELF)
  (AND (EQ SELF *PRIMARY-ZMAIL-BUFFER*)
       (SETQ *PRIMARY-ZMAIL-BUFFER* NIL))
  (AND (EQ SELF *ZMAIL-BUFFER*)
       (SELECT-ZMAIL-BUFFER (CAR *ZMAIL-BUFFER-LIST*)))
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-SELECT))

(DEFMETHOD (ZMAIL-BUFFER :ADD-MSG) (MSG &OPTIONAL AT-INDEX &AUX LEN)
  (SETQ LEN (ARRAY-ACTIVE-LENGTH ARRAY))
  (COND ((NOT (DOTIMES (I LEN) ;If not already in file
                (AND (EQ MSG (AREF ARRAY I)) (RETURN T))))
         (OR AT-INDEX
             (SETQ AT-INDEX (LET (PREDICATE APPEND-P)
                              (LET ((PLIST (LOCF OPTIONS)))
                                (SETQ PREDICATE (GET PLIST :SORT)
                                      APPEND-P (GET PLIST :APPEND)))
                              (IF (NULL PREDICATE)
                                  (IF APPEND-P LEN 0)
                                (LOOP FOR I FROM 0 TO LEN
                                      WHEN (OR (= I LEN)
                                               (EQ APPEND-P
                                                   (FUNCALL PREDICATE MSG (AREF ARRAY I))))
                                      DO (RETURN I))))))
         (AND (< (ARRAY-LENGTH ARRAY) (1+ LEN))
              (ADJUST-ARRAY-SIZE ARRAY (TRUNCATE (* LEN 5) 4)))
         (SETF (FILL-POINTER ARRAY) (1+ LEN))
         (SETQ MSG (SEND SELF :ADD-MSG-TEXT MSG AT-INDEX))
         (DO ((I LEN (1- I))
              (J (1- LEN) (1- J)))
             ((< J AT-INDEX))
           (ASET (AREF ARRAY J) ARRAY I))
         (ASET MSG ARRAY AT-INDEX))))

(DEFMETHOD (ZMAIL-BUFFER :ADD-ZMAIL-BUFFER) (ZMAIL-BUFFER &AUX NARRAY NLENGTH PREDICATE APPEND-P)
  (SETQ NARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)
        NLENGTH (ARRAY-ACTIVE-LENGTH NARRAY))
  (unless (zerop nlength)
    (LET ((PLIST (LOCF OPTIONS)))
      (SETQ PREDICATE (GET PLIST :SORT)
            APPEND-P (GET PLIST :APPEND)))
    (AND (> NLENGTH 1)
         (OR PREDICATE (NULL APPEND-P))
         (LET ((NNARRAY (MAKE-ARRAY NLENGTH)))
           (COPY-ARRAY-CONTENTS NARRAY NNARRAY)
           (IF (NULL PREDICATE)
               (NREVERSE NNARRAY)
             (FUNCALL (IF APPEND-P 'STABLE-SORT 'REVERSE-STABLE-SORT) NNARRAY PREDICATE))
           (SETQ NARRAY NNARRAY)))
    ;; Now merge
    (LOOP FOR OLD-IDX FROM 0
          WITH LENGTH = (ARRAY-ACTIVE-LENGTH ARRAY)
          AND NEW-IDX = 0
          AS NMSG = (AREF NARRAY NEW-IDX)
          WHEN (OR ( OLD-IDX LENGTH)
                   (EQ APPEND-P (AND PREDICATE (FUNCALL PREDICATE NMSG (AREF ARRAY OLD-IDX)))))
          DO (SEND SELF :ADD-MSG NMSG OLD-IDX)
          (INCF NEW-IDX)
          (INCF LENGTH)
          UNTIL ( NEW-IDX NLENGTH))))

(DEFMETHOD (ZMAIL-BUFFER :ADD-MSG-TEXT) (MSG AT-INDEX)
  AT-INDEX                                      ;Not used
  MSG)

(DEFMETHOD (ZMAIL-BUFFER :READ-NEXT-MSG) (&OPTIONAL IGNORE)
  NIL)

(DEFMETHOD (ZMAIL-BUFFER :UPDATE-MSG-OPTIONS-IN-FILE-IF-NECESSARY) (MSG)
  MSG)

(DEFMETHOD (ZMAIL-BUFFER :FULL-NAME) ()
  (SEND SELF :NAME))

(DEFUN TEMP-ZMAIL-BUFFER-AVAILABLE-P (IGNORE ZMAIL-BUFFER IGNORE)
  (NOT (MEMQ ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)))

(DEFUN MAKE-TEMP-ZMAIL-BUFFER (IGNORE)
  (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER :APPEND-P T))

(DEFUN MAKE-NEW-TEMP-ZMAIL-BUFFER (NAME &OPTIONAL (FULL-NAME NAME) &AUX ZMAIL-BUFFER)
  (SETQ ZMAIL-BUFFER (ALLOCATE-RESOURCE 'TEMP-ZMAIL-BUFFER))
  (SETF (ZMAIL-BUFFER-NAME ZMAIL-BUFFER) NAME)
  (SEND ZMAIL-BUFFER :SET-FULL-NAME FULL-NAME)
  (SETF (ZMAIL-BUFFER-SAVED-CURRENT-MSG ZMAIL-BUFFER) NIL)
  (SETF (NODE-TICK ZMAIL-BUFFER) (TICK))
  (SEND ZMAIL-BUFFER :ACTIVATE)
  ZMAIL-BUFFER)

;;; Disk buffers
(DEFMETHOD (ZMAIL-DISK-BUFFER :BEFORE :KILL) ()
  (DOMSGS (MSG SELF)
    (SETF (MSG-PARSED-P MSG) :KILLED))
  ;; Get rid of any messages we killed this way
  (LOOP FOR ZMAIL-BUFFER IN *ZMAIL-BUFFER-LIST*
        WHEN (NOT (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER))
        DO (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER NIL)))

(DEFMETHOD (ZMAIL-DISK-BUFFER :PARSE-MSG) (MSG -STATUS-)
  (SET-PARSED-MSG-HEADERS MSG -STATUS-))

(DEFMETHOD (ZMAIL-DISK-BUFFER :PARSE-MSG-TEXT) (MSG IGNORE)
  (PARSE-HEADERS-INTERVAL (MSG-INTERVAL MSG) NIL NIL T))

(DEFMETHOD (ZMAIL-DISK-BUFFER :UPDATE-MSG-END) (MSG &OPTIONAL IGNORE)
  MSG)

;;; MAIL-FILE-BUFFERs.

;;; ALWAYS-NEW means do not read in any old file.  FLAVOR overrides any default
(DEFUN ZMAIL-FIND-FILE-NOSELECT (PATHNAME &OPTIONAL (NEAR-MODE '(:MOUSE)) FLAVOR ALWAYS-NEW
                                 &AUX STREAM)
  (DECLARE (RETURN-LIST ZMAIL-BUFFER NEW-P))
  (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
    (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *ZMAIL-PATHNAME-DEFAULTS*)) )
  (SETQ PATHNAME (SEND PATHNAME :TRANSLATED-PATHNAME))
  (COND ((NOT (OR ALWAYS-NEW
                  (ERRORP (SETQ STREAM (OPEN PATHNAME '(:IN :NOERROR))))))
         (GET-ZMAIL-FILE STREAM PATHNAME NIL FLAVOR))
        ((AND STREAM
              (NOT (CONDITION-TYPEP STREAM 'FS:FILE-NOT-FOUND)))
         (BARF STREAM))
        (T
         (FORMAT *QUERY-IO* "~&New file: ~A" PATHNAME)
         (VALUES (ZMAIL-FIND-FILE-NOSELECT-1
                   PATHNAME
                   (COND ((OR ALWAYS-NEW (NULL *ZMAIL-BUFFER*)) NIL)
                         ((AND *DEFAULT-MOVE-ZMAIL-BUFFER*
                               (ZMAIL-BUFFER-DISK-P
                                 *DEFAULT-MOVE-ZMAIL-BUFFER*))
                          *DEFAULT-MOVE-ZMAIL-BUFFER*)
                         ((ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
                          *ZMAIL-BUFFER*)
                         (*PRIMARY-ZMAIL-BUFFER*))
                   NEAR-MODE
                   FLAVOR)
                 T))))

(DEFUN ZMAIL-FIND-FILE-NOSELECT-1 (PATHNAME &OPTIONAL ZMAIL-BUFFER NEAR-MODE FLAVOR PRIMARY-P
                                   &AUX OPTIONS PLIST APPEND-P)
  (SETQ PLIST (LOCF OPTIONS))
  (COND (ZMAIL-BUFFER
         (SETQ FLAVOR (TYPE-OF ZMAIL-BUFFER)
               OPTIONS (SEND ZMAIL-BUFFER :STICKY-OPTIONS)
               APPEND-P (GET PLIST :APPEND)))
        ((NULL FLAVOR)
         (MULTIPLE-VALUE (FLAVOR APPEND-P)
           (SEND PATHNAME :MAIL-FILE-FORMAT-COMPUTER NIL))))
  (SETQ APPEND-P (CASE *NEW-MAIL-FILE-APPEND-P*
                   (:APPEND T)
                   (:PREPEND NIL)
                   (:STICKY APPEND-P)
                   (:ASK (IF (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
                             APPEND-P
                           (LET ((CHOICE (TV:MENU-CHOOSE '(("Append" . :APPEND)
                                                           ("Prepend" . :PREPEND)))))
                             (OR CHOICE (ABORT-CURRENT-COMMAND))
                             (EQ CHOICE :APPEND))))))
  (COND ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
         (PUTPROP PLIST APPEND-P :APPEND)
         (OR NEAR-MODE (SETQ NEAR-MODE '(:MOUSE)))
         (MULTIPLE-VALUE (FLAVOR OPTIONS)
           (CHOOSE-MAIL-FILE-OPTIONS-1 PATHNAME FLAVOR OPTIONS NEAR-MODE))
         (SETQ APPEND-P (CAR (REMPROP PLIST :APPEND))))
        (T
         (REMPROP PLIST :APPEND)))                      ;Do not get confused in :INIT method
  (let ((buffer (MAKE-ZMAIL-BUFFER FLAVOR :PATHNAME PATHNAME :FILE-ID T
                                   :OPTIONS OPTIONS :APPEND-P APPEND-P
                                   :NEW-PRIMARY-P PRIMARY-P)))
    (setf (get buffer 'generation-retention-count) *default-mail-buffer-generation-retention-count*)
    buffer))

(DEFMETHOD (MAIL-FILE-BUFFER :SETTABLE-OPTIONS) ()
  NIL)

(DEFMETHOD (MAIL-FILE-BUFFER :POSSIBLE-OPTIONS) ()
  '(:APPEND))

(DEFMETHOD (MAIL-FILE-BUFFER :STICKY-OPTIONS) ()
  (SOME-PLIST OPTIONS '(:APPEND)))

(DEFMETHOD (MAIL-FILE-BUFFER :AFTER :INIT) (IGNORE)
  (SETQ NAME (SEND PATHNAME :STRING-FOR-EDITOR))
  (SETQ FILE-TICK TICK)
  (SETQ MSG-UPDATE-TICK TICK)
  (AND STREAM (SETQ STATUS :LOADING))
  (SEND SELF :ACTIVATE))

(DEFMETHOD (MAIL-FILE-BUFFER :INBOX-BUFFER) (&OPTIONAL NEW-PATHNAME DELETE-P &AUX LIST)
  (IF NEW-PATHNAME
      (SETQ LIST (LIST (LIST NEW-PATHNAME NIL DELETE-P)))
    (SETQ NEW-PATHNAME (SEND PATHNAME :NEW-MAIL-PATHNAME))
    (SETQ LIST (LIST (LIST NEW-PATHNAME
                           (SEND NEW-PATHNAME :NEW-TYPE
                                 (SEND NEW-PATHNAME
                                       :ZMAIL-TEMP-FILE-NAME))
                           T)))
    (COND ((RUN-GMSGS-P)
           (SETQ NEW-PATHNAME (SEND NEW-PATHNAME :NEW-TYPE "GMSGS"))
           (PUSH (LIST NEW-PATHNAME
                       (SEND NEW-PATHNAME :NEW-TYPE
                             (SEND NEW-PATHNAME
                                   :ZMAIL-TEMP-FILE-NAME))
                       T)
                 LIST))))
  (MAKE-INBOX-BUFFER (SEND NEW-PATHNAME :INBOX-BUFFER-FLAVOR) LIST SELF))

;;; Update the buffer version of a message's status
(DEFMETHOD (MAIL-FILE-BUFFER :UPDATE-MSG-OPTIONS-IN-FILE-IF-NECESSARY) (MSG)
  (AND (> (MSG-TICK MSG) MSG-UPDATE-TICK)
       (SEND SELF :UPDATE-MSG-OPTIONS-IN-FILE MSG)))

(DEFMETHOD (MAIL-FILE-BUFFER :UPDATE-MSG-OPTIONS-IN-FILE) (MSG)
  MSG)

;; Mung the message MSG so that its contents are suitable for
;; being part of this mail file buffer.
;; The headers, as well as the invisible stuff at the front and end of the message,
;; are altered to fit.
;; Also, the MSG-MAIL-FILE-BUFFER is set.
(DEFMETHOD (MAIL-FILE-BUFFER :NEW-MSG) (MSG &AUX OLD-FILE)
  (SETQ OLD-FILE (MSG-MAIL-FILE-BUFFER MSG))
  (SETF (MSG-MAIL-FILE-BUFFER MSG) SELF)
  (MULTIPLE-VALUE-BIND (BEFORE AFTER)
      (SEND SELF :NEW-HEADER-AND-TRAILER)
    (REPLACE-REAL-HEADER-AREA MSG BEFORE)
    (REPLACE-REAL-TRAILER-AREA MSG AFTER))
  (SEND SELF :UPDATE-MSG-END MSG)
  ;; If the old mail file of this message wanted different style headers,
  ;; must reformat the headers so the message can be reparsed right in this file.
  (WHEN (AND OLD-FILE (GET (LOCF (MSG-STATUS MSG)) 'HEADERS-END-BP))
    (UNLESS (MEMBER (SEND OLD-FILE :FORMAT-NAME)
                    (SEND SELF :HEADER-COMPATIBLE-MAIL-FILE-FORMATS))
      (SEND SELF :REFORMAT-MSG-HEADER MSG)))
  MSG)

;; Should return a list of :FORMAT-NAMEs of formats whose
;; header standards are the same as this mail file's format's standards.
(DEFMETHOD (MAIL-FILE-BUFFER :HEADER-COMPATIBLE-MAIL-FILE-FORMATS) ()
  (LIST (SEND SELF :FORMAT-NAME)))

;; Replace the visible header of MSG with one which has the right format
;; to be parsed in the type of mail file which SELF is.
;; The default definition makes a standard-ish network header.
(DEFMETHOD (MAIL-FILE-BUFFER :REFORMAT-MSG-HEADER) (MSG)
  (WITH-BP (SEP (INTERVAL-FIRST-BP (MSG-INTERVAL MSG)) :MOVES)
    (LET ((STRM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP (MSG-INTERVAL MSG)))))
      (DO ((TAIL (MSG-STATUS MSG) (CDDR TAIL)))
          ((NULL TAIL))
        (WHEN (RASSQ (CAR TAIL) *HEADER-NAME-ALIST*)
          (PRINT-HEADER STRM (CADR TAIL) (CAR TAIL))))
      (TERPRI STRM))
    (DELETE-INTERVAL SEP (GET (LOCF (MSG-STATUS MSG)) 'HEADERS-END-BP))))

(DEFMETHOD (MAIL-FILE-BUFFER :NEW-HEADER-AND-TRAILER) ()
  (VALUES "" ""))

(DEFUN REPLACE-REAL-HEADER-AREA (MSG STRING &AUX START-BP MSG-REAL-INTERVAL
                                                 PREV-END-BP PREV-END-BP-1
                                                 MOVE-BP-P MOVE-BP-1-P
                                                 NEW-BP)
  (SETQ START-BP (MSG-START-BP MSG)
        MSG-REAL-INTERVAL (MSG-REAL-INTERVAL MSG))
  (DELETE-INTERVAL (INTERVAL-FIRST-BP MSG-REAL-INTERVAL) START-BP T)
  (AND (SETQ MOVE-BP-P (NODE-PREVIOUS MSG-REAL-INTERVAL))
       (SETQ PREV-END-BP (INTERVAL-LAST-BP MOVE-BP-P)
             PREV-END-BP-1 (INTERVAL-LAST-BP (CAR (NODE-INFERIORS MOVE-BP-P)))
             MOVE-BP-P (BP-= PREV-END-BP START-BP)
             MOVE-BP-1-P (BP-= PREV-END-BP-1 START-BP)))
  (SETQ NEW-BP (INSERT START-BP STRING))
  ;; If we inserted text that made the end of the previous message move accidentally,
  ;; put it back where it was.
  (AND MOVE-BP-P
       (MOVE-BP PREV-END-BP START-BP))
  (AND MOVE-BP-1-P
       (MOVE-BP PREV-END-BP-1 START-BP))
  (MOVE-BP START-BP NEW-BP)
  (SEND SELF :UPDATE-MSG-OPTIONS-IN-FILE MSG))

(DEFUN REPLACE-REAL-TRAILER-AREA (MSG STRING &AUX END-BP REAL-INTERVAL
                                                  NEXT-START-BP NEXT-START-BP-1
                                                  MOVE-BP-P MOVE-BP-1-P)
  (SETQ END-BP (MSG-END-BP MSG)
        REAL-INTERVAL (MSG-REAL-INTERVAL MSG))
  (DELETE-INTERVAL END-BP (INTERVAL-LAST-BP REAL-INTERVAL) T)
  (AND (SETQ MOVE-BP-P (NODE-NEXT REAL-INTERVAL))
       (SETQ NEXT-START-BP (INTERVAL-FIRST-BP MOVE-BP-P)
             NEXT-START-BP-1 (INTERVAL-FIRST-BP (CAR (NODE-INFERIORS MOVE-BP-P)))
             MOVE-BP-P (BP-= NEXT-START-BP END-BP)
             MOVE-BP-1-P (BP-= NEXT-START-BP-1 END-BP)))
  (WITH-BP (OLD-END-BP END-BP :NORMAL)
    (INSERT-MOVING END-BP STRING)
    (AND MOVE-BP-P
         (MOVE-BP NEXT-START-BP END-BP))
    (AND MOVE-BP-1-P
         (MOVE-BP NEXT-START-BP-1 END-BP))
    (MOVE-BP END-BP OLD-END-BP)))

;; Return a pointer to the beginning of the first message.
;; By default, that is the beginning of the interval's contents.
(DEFMETHOD (ZMAIL-DISK-BUFFER :FIRST-MSG-BP) ()
  FIRST-BP)

;;; The count of messages can be inconsistent while the text is being added, prevent lossage
;;; with background parsing
(DEFWRAPPER (ZMAIL-DISK-BUFFER :ADD-MSG) (IGNORE . BODY)
  `(LOCK-ZMAIL-BUFFER (SELF)
     . ,BODY))

(DEFWRAPPER (ZMAIL-DISK-BUFFER :ADD-ZMAIL-BUFFER) (IGNORE . BODY)
  `(LOCK-ZMAIL-BUFFER (SELF)
     . ,BODY))

;;; This actually links in the lines of a message
(DEFMETHOD (MAIL-FILE-BUFFER :ADD-MSG-TEXT) (MSG AT-INDEX)
  (COND ((MSG-MAIL-FILE-BUFFER MSG)
         ;; This MSG-PUT and the COPY-MSG below insure that the message has been parsed
         ;; if its text is going to be copied.  This is necessary so that the *** EOOH ***
         ;; line isn't copied as well.
         (MSG-PUT MSG T 'FILED)
         (AND *DELETE-AFTER-MOVE-TO-FILE*
              (ZMAIL-DELETE-MSG MSG))
         (LET ((FILE (MSG-MAIL-FILE-BUFFER MSG)))
           (SETQ MSG (COPY-MSG MSG))
           (SETF (MSG-MAIL-FILE-BUFFER MSG) FILE))))
  (SEND SELF :NEW-MSG MSG)
  (LET* ((MSG-INT (MSG-REAL-INTERVAL MSG))
         (MSG-REAL-START-BP (INTERVAL-FIRST-BP MSG-INT))
         (MSG-REAL-END-BP (INTERVAL-LAST-BP MSG-INT))
         (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
         (INFS (NODE-INFERIORS SELF))
         (LINE2 (BP-LINE MSG-REAL-START-BP))
         (LINE3 (BP-LINE MSG-REAL-END-BP))
         PREV-MSG-END-BPS LINE1 LINE4)
    (SETF (NODE-SUPERIOR MSG-INT) SELF)
    (COND ((= AT-INDEX (1- NMSGS))              ;Putting in at the end
           (MULTIPLE-VALUE (LINE1 PREV-MSG-END-BPS)
             (SEND SELF :LAST-LINE-FOR-APPEND (1- NMSGS))))
          (T
           (UNLESS (ZEROP AT-INDEX)
             (MULTIPLE-VALUE (NIL PREV-MSG-END-BPS)
               (SEND SELF :LAST-LINE-FOR-APPEND AT-INDEX)))
           (LET ((NEXT-MSG (AREF ARRAY AT-INDEX)))
             (LET ((NEXT-NODE (MSG-REAL-INTERVAL NEXT-MSG)))
               (SETF (NODE-NEXT MSG-INT) NEXT-NODE)
               (SETF (NODE-PREVIOUS NEXT-NODE) MSG-INT))
             (SETQ LINE4 (BP-LINE (MSG-REAL-START-BP NEXT-MSG))
                   LINE1 (LINE-PREVIOUS LINE4)))))
    (COND (LINE1
           (SETF (LINE-PREVIOUS LINE2) LINE1)
           (SETF (LINE-NEXT LINE1) LINE2)
           (DOLIST (BP PREV-MSG-END-BPS)
             (MOVE-BP BP LINE2 0)))
          (T
           (MOVE-BP FIRST-BP MSG-REAL-START-BP)))
    (COND (LINE4
           (AND (ZEROP (BP-INDEX MSG-REAL-END-BP))
                (SETQ LINE3 (LINE-PREVIOUS LINE3)))
           (SETF (LINE-NEXT LINE3) LINE4)
           (SETF (LINE-PREVIOUS LINE4) LINE3)
           (LET ((MSG-END-BP (MSG-END-BP MSG)))
             (AND (BP-= MSG-END-BP MSG-REAL-END-BP)
                  (MOVE-BP MSG-END-BP LINE4 0)))
           (MOVE-BP MSG-REAL-END-BP LINE4 0))
          (T
           (MOVE-BP LAST-BP LINE3 (LINE-LENGTH LINE3))))
    (IF (ZEROP AT-INDEX)
        (SETQ INFERIORS (CONS MSG-INT INFS))
      (LET ((PREV-NODE (MSG-REAL-INTERVAL (AREF ARRAY (1- AT-INDEX)))))
        (SETF (NODE-NEXT PREV-NODE) MSG-INT)
        (SETF (NODE-PREVIOUS MSG-INT) PREV-NODE)
        (AND (SETQ INFS (MEMQ PREV-NODE INFS))
             (PUSH MSG-INT (CDR INFS))))))
  (MUNG-NODE SELF)
  MSG)

;;; If there was a message in here before, it may not have had a  or whatever at
;;; the end of it.  Also, there may be an empty line at the end of the file
;;; which signifies a final CR, and that empty line will not be needed
;;; when more text is appended.

;;; The first value is the line to append the new text after.
;;; The second valus is a list of BPs to relocate to point at the
;;; beginning of the appended text.
(DEFMETHOD (MAIL-FILE-BUFFER :LAST-LINE-FOR-APPEND) (&OPTIONAL NMSGS
                                                     &AUX BP BP1 LINE PREV-END-BPS)
  (OR NMSGS (SETQ NMSGS (ARRAY-ACTIVE-LENGTH ARRAY)))
  (IF (PLUSP NMSGS)
      (LET ((MSG (AREF ARRAY (1- NMSGS))))
        (SEND SELF :UPDATE-MSG-END MSG T)
        (SETQ BP (MSG-REAL-END-BP MSG)
              BP1 (MSG-END-BP MSG)))
    (SETQ BP (SEND SELF :FIRST-MSG-BP)))
  (SETQ LINE (BP-LINE BP))
  (AND (ZEROP (BP-INDEX BP))
       (SETQ PREV-END-BPS
             (IF (EQ LINE (BP-LINE BP1))
                 (LIST BP BP1)
               (LIST BP))
;            LINE (LINE-PREVIOUS LINE) fixes bug 337 an additional line may appear in some files as a side effect. jwc
             ))
  (VALUES LINE PREV-END-BPS))

;;; This makes sure that a mail file that corresponds to an actual file has
;;; its messages in the right order.
(DEFUN RESPLICE-ZMAIL-BUFFER (ZMAIL-BUFFER &AUX START-LINE START-BP ARRAY)
  (SETF (NODE-TICK ZMAIL-BUFFER) (TICK))
  (OR (SETQ START-LINE (LINE-PREVIOUS (BP-LINE (SEND ZMAIL-BUFFER :FIRST-MSG-BP))))
      (SETQ START-BP (ZMAIL-BUFFER-START-BP ZMAIL-BUFFER)))
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (LINE1 START-LINE (BP-LINE (MSG-END-BP MSG)))
       (MSG-END-BP NIL (MSG-END-BP MSG))
       (REAL-END-BP START-BP (MSG-REAL-END-BP MSG))
       (REAL-INT) (PREV-REAL-INT NIL REAL-INT)
       (LINE2) (MSG) (REAL-START-BP) (FLAG)
       (INFS NIL (CONS REAL-INT INFS)))
      (NIL)
    (AND (SETQ FLAG (AND MSG-END-BP REAL-END-BP (BP-= MSG-END-BP REAL-END-BP)))
         (DO ((L (BP-LINE (MSG-START-BP MSG)) (LINE-NEXT L))
              (PL LINE1 L))
             ((EQ L LINE1) (SETQ LINE1 PL))))
    (COND (( I NMSGS)
           (LET ((ZMAIL-BUFFER-END-BP (ZMAIL-BUFFER-END-BP ZMAIL-BUFFER))
                 (BP (END-OF-LINE LINE1)))
             (SETF (LINE-NEXT LINE1) NIL)
             (MOVE-BP ZMAIL-BUFFER-END-BP BP)
             (MOVE-BP REAL-END-BP ZMAIL-BUFFER-END-BP)
             (AND FLAG (MOVE-BP MSG-END-BP ZMAIL-BUFFER-END-BP)))
           (AND MSG (SEND ZMAIL-BUFFER :UPDATE-MSG-END MSG))
           (AND REAL-INT (SETF (NODE-NEXT REAL-INT) NIL))
           (SETF (NODE-INFERIORS ZMAIL-BUFFER) (NREVERSE INFS))
           (RETURN NIL)))
    (SETQ MSG (AREF ARRAY I)
          REAL-INT (MSG-REAL-INTERVAL MSG)
          REAL-START-BP (INTERVAL-FIRST-BP REAL-INT))
    (SETF (NODE-PREVIOUS REAL-INT) PREV-REAL-INT)
    (AND PREV-REAL-INT (SETF (NODE-NEXT PREV-REAL-INT) REAL-INT))
    (SEND ZMAIL-BUFFER :UPDATE-MSG-END MSG)
    (AND REAL-END-BP
         (MOVE-BP REAL-END-BP REAL-START-BP))
    (AND FLAG (MOVE-BP MSG-END-BP REAL-START-BP))
    (SETQ LINE2 (BP-LINE REAL-START-BP))
    (AND LINE1 (SETF (LINE-NEXT LINE1) LINE2))
    (SETF (LINE-PREVIOUS LINE2) LINE1))
  (MUNG-NODE ZMAIL-BUFFER))

;;; Reverse the order of messages in a buffer
(DEFUN REVERSE-ZMAIL-BUFFER (ZMAIL-BUFFER)
  (NREVERSE (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (RESPLICE-ZMAIL-BUFFER ZMAIL-BUFFER)))

;;; Update the list of options at the start of the file
(DEFMETHOD (MAIL-FILE-BUFFER :UPDATE-OPTIONS-IN-FILE) ())

;;; Simple write-only mail files without separators
(ADD-ZMAIL-BUFFER-FLAVOR 'TEXT-MAIL-FILE-BUFFER "Text")

(DEFFLAVOR TEXT-MAIL-FILE-BUFFER () (MAIL-FILE-BUFFER))

(DEFMETHOD (TEXT-MAIL-FILE-BUFFER :FORMAT-NAME) () "Text")

;; Don't change appearance of a header just to print it.
(DEFMETHOD (TEXT-MAIL-FILE-BUFFER :REFORMAT-MSG-HEADER) (IGNORE)
  NIL)

(DEFMETHOD (TEXT-MAIL-FILE-BUFFER :MAIL-FILE-REPARSABLE-P) () NIL)

;;; If we try to read one of these in, don't get any messages
(DEFMETHOD (TEXT-MAIL-FILE-BUFFER :LINE-END-OF-MSG-P) (&REST IGNORE)
  T)

(DEFMETHOD (TEXT-MAIL-FILE-BUFFER :UPDATE-MSG-END) (MSG &OPTIONAL IGNORE)
  (LET ((END-LINE (BP-LINE (MSG-END-BP MSG))))
    (SETF (LINE-LENGTH END-LINE) 0)
    (APPEND-TO-ARRAY END-LINE *TEXT-MAIL-FILE-SEPARATOR*)))

;;; I/O

(DEFUN STARTUP-ZMAIL-BUFFER (&OPTIONAL NEW-PATHNAME &AUX STREAM PATHNAME ZMAIL-BUFFER)
  (SET-ZMAIL-USER)
  ;; Do not let the background process run again until all file requests are pending.
  ;; Otherwise the RMAIL file may all get in before the call to get-new-mail below.
  (WITH-BACKGROUND-PROCESS-LOCKED
    (COND ((OR (NULL *PRIMARY-ZMAIL-BUFFER*) NEW-PATHNAME)
           (SETQ PATHNAME (OR NEW-PATHNAME *ZMAIL-STARTUP-FILE-NAME*))
           (AND PATHNAME
                (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME
                                                           *ZMAIL-PATHNAME-DEFAULTS*)))
           (DO ((LIST (AND (NULL PATHNAME)
                           (SEND (FS:USER-HOMEDIR) :POSSIBLE-MAIL-FILE-NAMES))))
               (NIL)
             (OR PATHNAME (POP LIST PATHNAME))  ;Get a pathname to use
             (SETQ STREAM (OPEN PATHNAME '(:IN :NOERROR)))
             (COND ((NOT (ERRORP STREAM))
                    (SETQ ZMAIL-BUFFER (GET-ZMAIL-FILE STREAM PATHNAME T))
                    (RETURN NIL)))
             (COND ((NULL LIST)         ;Ran out of choices
                    (UNLESS (CONDITION-TYPEP STREAM 'FS:FILE-NOT-FOUND)
                      (BARF "Error: ~A" STREAM))
                    (LET ((TEM (READ-DEFAULTED-PATHNAME
                                 (FORMAT NIL "~A, use what file (RETURN to create ~A)"
                                         STREAM PATHNAME)
                                 PATHNAME NIL NIL :NEW-OK)))
                      (IF (NEQ TEM PATHNAME)
                          (SETQ PATHNAME TEM)   ;Gave a new file, try that, else make it
                        (SETQ ZMAIL-BUFFER (ZMAIL-FIND-FILE-NOSELECT-1 PATHNAME NIL NIL NIL T))
                        (RETURN NIL))))
                   (T
                    (SETQ PATHNAME NIL))))
           (SELECT-ZMAIL-BUFFER ZMAIL-BUFFER (NULL NEW-PATHNAME) NIL)
           (COND (NEW-PATHNAME                  ;Not the primary mail file
                  (SETQ *MSG-NO* -1)
                  (ZMAIL-SELECT-NEXT-MSG NIL T))        ;Select the first message
                 (T
                  (COM-GET-NEW-MAIL-INTERNAL T))))
          (T DIS-NONE))))

;;; Getting new mail.

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-GET-NEW-MAIL
  "Merge any new mail:  L: selected or primary mail file; R: specify file.")

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-GET-NEW-MAIL "Read any new mail.
Click right to specify another new mail file.
The file is not deleted in this case." (NO-ZMAIL-BUFFER-OK)
  (COM-GET-NEW-MAIL-INTERNAL NIL))

(DEFUN COM-GET-NEW-MAIL-INTERNAL (FIRST-TIME-P &OPTIONAL FOR-BUFFER &AUX FROM-FILE)
  (OR FOR-BUFFER
      (SETQ FOR-BUFFER (COND ((AND *ZMAIL-BUFFER*
                                   (GET (LOCF (ZMAIL-BUFFER-OPTIONS *ZMAIL-BUFFER*)) :MAIL))
                              *ZMAIL-BUFFER*)
                             ((NULL *PRIMARY-ZMAIL-BUFFER*)
                              NIL)
                             ((TYPEP *PRIMARY-ZMAIL-BUFFER* 'INBOX-BUFFER)
                              (SEND *PRIMARY-ZMAIL-BUFFER* :ASSOCIATED-MAIL-FILE-BUFFER))
                             (T
                              *PRIMARY-ZMAIL-BUFFER*))))
  (COND ((NULL FOR-BUFFER)
         (STARTUP-ZMAIL-BUFFER))        ;This will call COM-GET-NEW-MAIL-INTERNAL with FIRST-TIME-P
        ((SEND FOR-BUFFER :ASSOCIATED-INBOX-BUFFER)
         (IF (MEMQ (ZMAIL-DISK-BUFFER-STATUS FOR-BUFFER)
                   '(:LOADING :AWAITING-NEW-MAIL))
             ;; If it's already reading in new mail, do nothing.
             DIS-NONE
           (IF (ZMAIL-DISK-BUFFER-STATUS FOR-BUFFER)
               ;; Ordinary reading in of other file
               (FOREGROUND-BACKGROUND-FINISH FOR-BUFFER NIL)
             ;; Probably left around from an error
             (SEND FOR-BUFFER :SET-ASSOCIATED-INBOX-BUFFER NIL))
           (COM-GET-NEW-MAIL-INTERNAL NIL FOR-BUFFER)))
        (T
         (AND (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
              (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                (SETQ FROM-FILE (READ-DEFAULTED-PATHNAME
                                  "Get new mail from file"
                                  (DEFAULT-ZMAIL-MOVE-PATHNAME)))))
         (AND (EQ FOR-BUFFER *PRIMARY-ZMAIL-BUFFER*)
              (run-gmsgs-p first-time-p)
              (NULL FROM-FILE)          ;The inbox-buffer won't look at us if this is set.
              (GMSGS (ZMAIL-BUFFER-GMSGS-HOST FOR-BUFFER)))
         ;; Possibly any mail file should be allowed that knows about getting new mail
         (GET-NEW-MAIL-FOR-ZMAIL-BUFFER FOR-BUFFER FROM-FILE NIL FIRST-TIME-P))))

(DEFUN ZMAIL-BUFFER-GMSGS-HOST (ZMAIL-BUFFER)
  (LET ((HOSTNAME (CADR (MEMQ :GMSGS-HOST (SEND ZMAIL-BUFFER :OPTIONS)))))
    (IF HOSTNAME (FS:GET-PATHNAME-HOST HOSTNAME)
      (SEND (BUFFER-PATHNAME ZMAIL-BUFFER) :HOST))))

(DEFUN GET-NEW-MAIL-FOR-ZMAIL-BUFFER (ZMAIL-BUFFER FROM-FILE DELETE-P TELL-BACKGROUND-P
                                   &AUX MOVE-P INBOX-BUFFER APPEND-P)
  (AND (SEND ZMAIL-BUFFER :ASSOCIATED-INBOX-BUFFER)
       (BARF "Already reading new mail into ~A" (ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))
  ;; Don't start reading new mail while in middle of saving.
  (IF (EQ (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) :SAVING)
      (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER))
  (SETQ APPEND-P (ZMAIL-BUFFER-APPEND-P ZMAIL-BUFFER))
  (SELECT-ZMAIL-BUFFER ZMAIL-BUFFER)
  (SETQ INBOX-BUFFER (SEND *ZMAIL-BUFFER* :INBOX-BUFFER FROM-FILE DELETE-P))
  (AND TELL-BACKGROUND-P
       (ZMAIL-BACKGROUND-REQUEST-PUSH
         `(ZMAIL-BACKGROUND-SET-INBOX-BUFFER ,INBOX-BUFFER)))
  (COND ((NULL (SEND INBOX-BUFFER :START-NEXT-FILE))
         (FORMAT *QUERY-IO* "~&No new mail")
         (SEND *ZMAIL-BUFFER* :SET-ASSOCIATED-INBOX-BUFFER NIL)
         (SETQ MOVE-P (OR (NULL *MSG*) *ALWAYS-JUMP-AFTER-GET-NEW-MAIL*)))
        (T
         (SETQ MOVE-P T)
         ;; In case :START-NEXT-FILE changed this.
         (SETQ INBOX-BUFFER (SEND *ZMAIL-BUFFER* :ASSOCIATED-INBOX-BUFFER))
         ;; Make sure ZMAIL-BUFFER's status indicates it may be getting new mail now.
         (UNLESS (EQ (ZMAIL-DISK-BUFFER-STATUS *ZMAIL-BUFFER*) :LOADING)
           (SETF (ZMAIL-DISK-BUFFER-STATUS *ZMAIL-BUFFER*) :AWAITING-NEW-MAIL))
         (SELECT-ZMAIL-BUFFER INBOX-BUFFER (EQ ZMAIL-BUFFER *PRIMARY-ZMAIL-BUFFER*) NIL)))
  (IF (NOT MOVE-P)
      DIS-TEXT
    (LOCK-ZMAIL-BUFFER (*ZMAIL-BUFFER*)         ;Don't allow getting all messages
      (SETQ *MSG-NO* (IF (AND APPEND-P (EQ *ZMAIL-BUFFER* ZMAIL-BUFFER))
                         (1- (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*))       ;but may have already.
                       -1))
      (ZMAIL-SELECT-NEXT-MSG NIL T))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-GMSGS "Run gmsgs" ()
  (GET-NEW-MAIL-FOR-ZMAIL-BUFFER *PRIMARY-ZMAIL-BUFFER*
                              (GMSGS (ZMAIL-BUFFER-GMSGS-HOST *PRIMARY-ZMAIL-BUFFER*))
                              T NIL)
  ;; No need for out output of "(There are messages)" to prevent immediate redisplay
  (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
  DIS-TEXT)


(DEFUN GMSGS (&OPTIONAL (HOST FS:USER-LOGIN-MACHINE) (STREAM *STANDARD-OUTPUT*))
  (IF (STRINGP HOST) (SETQ HOST (SI:PARSE-HOST HOST)))
  (condition-case (error)
      (SEND HOST :DO-GMSGS STREAM)   ;Methods are in MFHOST
    (SYS:NETWORK-ERROR
     (beep)
     (FORMAT *QUERY-IO* "~&GMSGS Error: ~A"
             (send error :REPORT-STRING)))))

;;; Setup for loading a mail file from stream, does not actually read any messages.
;;; FLAVOR is the parsing format to be forced on the file.  If not specified, it
;;; is computed by looking at the file.
(DEFUN GET-ZMAIL-FILE (STREAM PATHNAME &OPTIONAL BACKGROUND-P FLAVOR &AUX INFO SUCCESS)
  (SETQ INFO (SEND STREAM :INFO))
  (UNWIND-PROTECT
      (OR (LET ((ZMAIL-BUFFER (GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME)))
            (WHEN ZMAIL-BUFFER
              (IF (AND (NOT (EQUAL INFO (BUFFER-FILE-ID ZMAIL-BUFFER)))
                       (YES-OR-NO-P
                         (FORMAT NIL "File ~A has been changed since you last read or saved it.
Read in the new version? " PATHNAME)))
                  (PROGN
                    (KILL-ZMAIL-BUFFER ZMAIL-BUFFER)
                    NIL)
                (CLOSE STREAM)
                  (SETQ SUCCESS T)
                (WHEN (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER)
                  (ZMAIL-BACKGROUND-REQUEST-PUSH
                    (LIST 'ZMAIL-BACKGROUND-LOAD-FILE ZMAIL-BUFFER)))
                ZMAIL-BUFFER)))
          (LET (APPEND-P ZMAIL-BUFFER)
            (OR FLAVOR (MULTIPLE-VALUE (FLAVOR APPEND-P)
                         (SEND PATHNAME :MAIL-FILE-FORMAT-COMPUTER STREAM)))
            (SETQ ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER FLAVOR
                                                  :PATHNAME PATHNAME
                                                  :STREAM STREAM
                                                  :FILE-ID INFO
                                                  :APPEND-P APPEND-P))
            (START-LOADING-ZMAIL-BUFFER ZMAIL-BUFFER STREAM BACKGROUND-P)
            (SETQ SUCCESS T)
            ZMAIL-BUFFER))
    (UNLESS SUCCESS (CLOSE STREAM))))

(DEFUN START-LOADING-ZMAIL-BUFFER (ZMAIL-BUFFER STREAM BACKGROUND-P &OPTIONAL TRUENAME)
  (FORMAT *QUERY-IO* "~&Reading ~A file ~A" (SEND ZMAIL-BUFFER :FORMAT-NAME)
          (OR TRUENAME (SEND STREAM :TRUENAME)))
  (IF BACKGROUND-P
      (ZMAIL-BACKGROUND-REQUEST-PUSH (LIST 'ZMAIL-BACKGROUND-LOAD-FILE ZMAIL-BUFFER))
    (LET ((*ZMAIL-BACKGROUND-P* :DISABLE))
      (LOAD-ALL-MSGS ZMAIL-BUFFER))))

(DEFUN LOAD-ALL-MSGS (ZMAIL-BUFFER &AUX MAIL-FILE-BUFFER)
  (SETQ MAIL-FILE-BUFFER (AND (TYPEP ZMAIL-BUFFER 'INBOX-BUFFER)
                              (SEND ZMAIL-BUFFER :ASSOCIATED-MAIL-FILE-BUFFER)))
  (SEND ZMAIL-BUFFER :READ-NEXT-MSG 177777)     ;Finish reading in foreground
  (COND (MAIL-FILE-BUFFER
         (SETQ ZMAIL-BUFFER MAIL-FILE-BUFFER)
         (SEND ZMAIL-BUFFER :READ-NEXT-MSG 177777)))
  ZMAIL-BUFFER)

;;; This is the default message loader.  Different formats provide two messages,
;;; :LINE-END-OF-MSG-P and :CANONICAL-LAST-LINE.
;;; :LINE-END-OF-MSG-P is passed a LINE, its LENGTH, a STATE, an EOF flag, and the START.
;;; START is the line on which this message started.
;;; It should return END-IDX and an updated STATE variable.
;;; END-IDX is T if this was the last line, or index of the end of the message
;;; within the line, or :START-NEXT if this line should also start next message, or NIL.
;;; If END-IDX is a number, it is the index of the end of the message's "contents"
;;; but the next message may not start until the following line.
;;; STATE is NIL for the first line of each message.
;;; Aside from this, the meaning of values of STATE is up to the
;;; individual format's :LINE-END-OF-MESSAGE-P method.
;;; :CANONICAL-LAST-LINE is called to make a dummy line for the end of
;;; the file if the normal format requires this.  This will default to
;;; an empty line.

(DEFMETHOD (ZMAIL-DISK-BUFFER :READ-NEXT-MSG) (&OPTIONAL (NMSGS 1) &AUX EOF)
  (LOCK-ZMAIL-BUFFER (SELF)
    (COND ((MEMQ STATUS '(:LOADING-NEW-MAIL :LOADING))
           (WHEN STREAM
             (LET* ((START (GET (LOCF OPTIONS) 'NEXT-MSG-START-LINE))
                    (LINE-WAITING-FLAG START))
               (REMPROP (LOCF OPTIONS) 'NEXT-MSG-START-LINE)
               (DO ((TEST-FUNCTION (GET-HANDLER-FOR SELF :LINE-END-OF-MSG-P))
                    (END-LINE) (LINE) (LENGTH) (END-IDX)
                    (MSG-REAL-START-BP) (STATE))        ;One piece of state for test function
                   (EOF)
                 (LET ((DEFAULT-CONS-AREA *ZMAIL-MSG-LINE-AREA*))
                   (IF LINE-WAITING-FLAG
                       (SETQ LINE (IF (EQ LINE-WAITING-FLAG T)
                                      (LINE-NEXT LINE)
                                    LINE-WAITING-FLAG)
                             LINE-WAITING-FLAG NIL)
                     (MULTIPLE-VALUE (LINE EOF)
                       (SEND STREAM :LINE-IN LINE-LEADER-SIZE))))
                 (COND ((AND EOF (OR (NULL LINE) (ZEROP (LINE-LENGTH LINE))))
                        (OR START (RETURN NIL))
                        (SETQ LINE (SEND SELF :CANONICAL-LAST-LINE))))
                 (OR END-LINE (SETQ END-LINE (PROGN
                                               (AND (NOT (ZEROP (BP-INDEX LAST-BP)))
                                                    (INSERT LAST-BP #\CR))
                                               (BP-LINE LAST-BP))))
                 (UNLESS (LINE-NEXT LINE) (INSERT-LINE-WITH-LEADER LINE END-LINE))
                 (SETQ LENGTH (LINE-LENGTH LINE))
                 (AND (NULL START)
                      (PLUSP LENGTH)
                      (DO I 0 (1+ I) ( I LENGTH)
                          (OR (MEMQ (CHAR LINE I) '(#/SP #/TAB))
                              (RETURN T)))
                      (SETQ START LINE))
                 (MULTIPLE-VALUE (END-IDX STATE)
                   (FUNCALL TEST-FUNCTION :LINE-END-OF-MSG-P LINE LENGTH STATE EOF START))
                 (UNLESS (EQ END-LINE (LINE-NEXT LINE))
                   ;; :LINE-END-OF-MSG-P made a new line.
                   (SETQ LINE-WAITING-FLAG T))
                 (COND (END-IDX
                        (SETQ MSG-REAL-START-BP (CREATE-BP START 0 :NORMAL))
                        (LET ((MSG-REAL-INTERVAL (CREATE-INTERVAL
                                                   MSG-REAL-START-BP
                                                   (CREATE-BP (IF (EQ END-IDX :START-NEXT)
                                                                  LINE
                                                                (LINE-NEXT LINE))
                                                              0 :MOVES)))
                              (MSG-INTERVAL (CREATE-INTERVAL
                                              (COPY-BP MSG-REAL-START-BP :NORMAL)
                                              (COND ((EQ END-IDX T)
                                                     (CREATE-BP (LINE-NEXT LINE) 0 :MOVES))
                                                    ((EQ END-IDX :START-NEXT)
                                                     (CREATE-BP LINE 0 :MOVES))
                                                    (T
                                                     (CREATE-BP LINE END-IDX :MOVES))))))
                          (SETF (NODE-TICK MSG-INTERVAL) FILE-TICK)
                          (SETF (NODE-TICK MSG-REAL-INTERVAL) FILE-TICK)
                          (VECTOR-PUSH-EXTEND
                            (MAKE-MSG :REAL-INTERVAL MSG-REAL-INTERVAL
                                      :INTERVAL MSG-INTERVAL
                                      :TICK FILE-TICK
                                      :MAIL-FILE-BUFFER SELF)
                            ARRAY)
                          (DO ((LINE START (LINE-NEXT LINE))
                               (LAST (LINE-NEXT LINE)))
                              ((EQ LINE LAST))
                            (SETF (LINE-NODE LINE) MSG-REAL-INTERVAL))
                          (SETF (NODE-SUPERIOR MSG-INTERVAL) MSG-REAL-INTERVAL)
                          (SETF (NODE-INFERIORS MSG-REAL-INTERVAL) (LIST MSG-INTERVAL))
                          (SETF (NODE-SUPERIOR MSG-REAL-INTERVAL) SELF)
                          (LET ((INFS (NODE-INFERIORS SELF)))
                            (LET ((LAST (CAR (LAST INFS))))
                              (SETF (NODE-PREVIOUS MSG-REAL-INTERVAL) LAST)
                              (COND (LAST
                                     (SETF (NODE-NEXT LAST) MSG-REAL-INTERVAL)
                                     ;;The last-bp of the previous interval is :MOVES
                                     ;;but should have stayed at the start of this one.
                                     (LET ((LAST-BP-0 (INTERVAL-LAST-BP LAST))
                                           (LAST-BP-1 (INTERVAL-LAST-BP
                                                        (CAR (NODE-INFERIORS LAST)))))
                                       (AND (BP-= LAST-BP-0 LAST-BP-1)
                                            (MOVE-BP LAST-BP-1 MSG-REAL-START-BP))
                                       (MOVE-BP LAST-BP-0 MSG-REAL-START-BP)))))
                            (SETQ INFERIORS
                                  (NCONC INFS (NCONS MSG-REAL-INTERVAL)))))
                        (COND (( (SETQ NMSGS (1- NMSGS)) 0)
                               (AND (EQ END-IDX :START-NEXT)
                                    (PUTPROP (LOCF OPTIONS) LINE 'NEXT-MSG-START-LINE))
                               (IF LINE-WAITING-FLAG
                                   (PUTPROP (LOCF OPTIONS) (LINE-NEXT LINE)
                                            'NEXT-MSG-START-LINE))
                               (RETURN NIL)))
                        (SETQ START (IF (EQ END-IDX :START-NEXT) LINE)
                              STATE NIL))))))
           (IF (AND STREAM (NOT EOF))
               T
             (SEND SELF :LOADING-DONE)
             (AND STREAM (PLUSP NMSGS) (NOT *ZMAIL-BACKGROUND-P*)
                  (SEND SELF :READ-NEXT-MSG NMSGS)))))))

(DEFMETHOD (ZMAIL-DISK-BUFFER :CANONICAL-LAST-LINE) ()
  (CREATE-LINE 'ART-STRING 0 NIL))

(DEFWRAPPER (ZMAIL-DISK-BUFFER :LOADING-DONE) (IGNORE . BODY)
  `(IF (EQ *ZMAIL-BACKGROUND-P* T)
       (ZMAIL-BACKGROUND-RESPONSE-PUSH `(FILE-LOADED ,SELF))
     SI:.DAEMON-CALLER-ARGS.
     . ,BODY))

(DEFMETHOD (MAIL-FILE-BUFFER :LOADING-DONE) ()
  (AND STREAM (SEND STREAM :CLOSE))
  (SETQ STREAM NIL)
  ;; If no new mail or new mail is not in yet, wait.
  (COND ((NULL ASSOCIATED-INBOX-BUFFER)
         (SETQ STATUS NIL))
        ((NEQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-INBOX-BUFFER) :AWAITING-SAVE)
         (SETQ STATUS :AWAITING-NEW-MAIL))
        (T
         ;; If new mail all in may need to append it now.
         (AND (ZMAIL-BUFFER-APPEND-P SELF)
              (INSERT-NEW-MAIL SELF ASSOCIATED-INBOX-BUFFER))
         ;; Now ready to save back out.
         (SETQ STATUS :SAVING-REQUIRED)
         (LET ((SORT (GET (LOCF OPTIONS) :SORT)))
           (AND SORT (SORT-ZMAIL-BUFFER SELF SORT
                                        (GET (LOCF OPTIONS) :APPEND-P) T)))
         (UNLESS *INHIBIT-BACKGROUND-SAVES*
           (ZMAIL-BUFFER-BACKGROUND-SAVE SELF))))
  (ZMAIL-BACKGROUND-REQUEST-PUSH (LIST 'ZMAIL-BACKGROUND-PARSE-MSGS SELF 0))
  (AND (NEQ *MSG* :NO-SELECT)
       (COMPUTE-CURRENT-MSG-NAME)))             ;We may now know how many messages

(DEFWRAPPER (ZMAIL-DISK-BUFFER :SAVING-DONE) (IGNORE . BODY)
  `(IF *ZMAIL-BACKGROUND-P*
       (ZMAIL-BACKGROUND-RESPONSE-PUSH `(FILE-SAVED ,SELF))
     SI:.DAEMON-CALLER-ARGS.                    ;Prevent compiler warnings
     . ,BODY))

(DEFMETHOD (MAIL-FILE-BUFFER :SAVING-DONE) (&OPTIONAL FORCING-OUT)
  (SETQ FILE-TICK (TICK))
  (SETQ FILE-ID (SEND STREAM :INFO))
  (when (get self 'generation-retention-count) ; Only gets put on for new files
    (send stream :change-properties nil
          :generation-retentation-count (get self 'generation-retention-count))
    (setf (get self 'generation-retention-count) nil))
  (SEND STREAM :CLOSE)
  (FORMAT *QUERY-IO* "~&Written: ~A" (SEND STREAM :TRUENAME))
  (COND (ASSOCIATED-INBOX-BUFFER
         (OR (EQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-INBOX-BUFFER) :AWAITING-SAVE)
             (LET ((OMF ASSOCIATED-INBOX-BUFFER))
               (SETQ ASSOCIATED-INBOX-BUFFER NIL STATUS NIL)
               (ZMAIL-ERROR "Inbox buffer out of synch (~S), get a wizard"
                       (ZMAIL-DISK-BUFFER-STATUS OMF))))
         (COND (ASSOCIATED-INBOX-BUFFER
                (OR (SEND ASSOCIATED-INBOX-BUFFER :SAVING-DONE FORCING-OUT)
                    ;; Returns T if starts to load another inbox file.
                    (SETQ ASSOCIATED-INBOX-BUFFER NIL))))))
  (SETQ STATUS NIL))

(DEFWRAPPER (ZMAIL-DISK-BUFFER :SAVING-ABORTED) (IGNORE . BODY)
  `(IF *ZMAIL-BACKGROUND-P*
       (ZMAIL-BACKGROUND-RESPONSE-PUSH `(FILE-SAVE-ABORTED ,SELF))
     SI:.DAEMON-CALLER-ARGS.                    ;Prevent compiler warnings
     . ,BODY))

;;;Called when saving in background process gets an error.
;;;The message is sent in the background process, intercepted by the wrapper above,
;;;and queued to be resent in the main process.
;;;Set status back to :SAVING-REQUIRED if there is an inbox buffer (in :AWAITING-SAVE).
(DEFMETHOD (MAIL-FILE-BUFFER :SAVING-ABORTED) ()
  (SEND STREAM :CLOSE :ABORT)
  (FORMAT *QUERY-IO* "~&Error saving ~A" (SEND STREAM :TRUENAME))
  (COND (ASSOCIATED-INBOX-BUFFER
         (OR (EQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-INBOX-BUFFER) :AWAITING-SAVE)
             (LET ((OMF ASSOCIATED-INBOX-BUFFER))
               (SETQ ASSOCIATED-INBOX-BUFFER NIL STATUS :SAVING-REQUIRED)
               (ZMAIL-ERROR "Inbox buffer out of synch (~S), get a wizard"
                       (ZMAIL-DISK-BUFFER-STATUS OMF))))
         (SETQ STATUS :SAVING-REQUIRED))
        (T
         (SETQ STATUS NIL))))

(DEFUN INSERT-NEW-MAIL (OLD-FILE NEW-FILE &AUX APPEND-P OLD-INT NEW-INT INT-APPEND-P)
  (COND ((GET (LOCF (ZMAIL-BUFFER-OPTIONS OLD-FILE)) :REVERSE-NEW-MAIL)
         (REVERSE-ZMAIL-BUFFER NEW-FILE)
         ;; Select the lowest numbered message
         (AND (EQ NEW-FILE *ZMAIL-BUFFER*)
              (LET ((NMSGS (ZMAIL-BUFFER-NMSGS NEW-FILE)))
                (AND ( NMSGS 2)
                     (MUST-REDISPLAY *MSG-WINDOW*
                         (ZMAIL-SELECT-MSG (AREF (ZMAIL-BUFFER-ARRAY NEW-FILE) 0) NIL NIL)))))))
  (SETQ APPEND-P (ZMAIL-BUFFER-APPEND-P OLD-FILE)
        OLD-INT (ZMAIL-DISK-BUFFER-INTERVAL OLD-FILE)
        NEW-INT (ZMAIL-DISK-BUFFER-INTERVAL NEW-FILE))
  (LOCK-ZMAIL-BUFFER (OLD-FILE)
    (LOCK-ZMAIL-BUFFER (NEW-FILE)
      (IF (SETQ INT-APPEND-P (OR APPEND-P (ZEROP (ZMAIL-BUFFER-NMSGS OLD-FILE))))
          (MULTIPLE-VALUE-BIND (END-LINE PREV-MSG-END-BPS)
              (SEND OLD-FILE :LAST-LINE-FOR-APPEND)
            (IF END-LINE
                (LET ((START-LINE (BP-LINE (INTERVAL-FIRST-BP NEW-INT))))
                  (SETF (LINE-NEXT END-LINE) START-LINE)
                  (SETF (LINE-PREVIOUS START-LINE) END-LINE)
                  (DOLIST (BP PREV-MSG-END-BPS)
                    (MOVE-BP BP START-LINE 0)))
              (MOVE-BP (INTERVAL-FIRST-BP OLD-INT) (INTERVAL-FIRST-BP NEW-INT)))
            (MOVE-BP (INTERVAL-LAST-BP OLD-INT) (INTERVAL-LAST-BP NEW-INT)))
        (LET ((START-LINE (BP-LINE (SEND OLD-FILE :FIRST-MSG-BP))))
          (LET ((PREV (LINE-PREVIOUS START-LINE))
                (NEW-START-LINE (BP-LINE (INTERVAL-FIRST-BP NEW-INT))))
            (SETF (LINE-PREVIOUS NEW-START-LINE) PREV)
            (IF PREV
                (SETF (LINE-NEXT PREV) NEW-START-LINE)
              (MOVE-BP (INTERVAL-FIRST-BP OLD-INT) NEW-START-LINE 0)))
          (LET ((NEW-END-LINE (BP-LINE (INTERVAL-LAST-BP NEW-INT))))
            (AND (ZEROP (LINE-LENGTH NEW-END-LINE))
                 (LINE-PREVIOUS NEW-END-LINE)
                 (SETQ NEW-END-LINE (LINE-PREVIOUS NEW-END-LINE)))
            (SETF (LINE-NEXT NEW-END-LINE) START-LINE)
            (SETF (LINE-PREVIOUS START-LINE) NEW-END-LINE))))
      (LET ((NEW-INFS (NODE-INFERIORS NEW-INT))
            (OLD-INFS (NODE-INFERIORS OLD-INT))
            LAST-INT FIRST-INT)
        (DOLIST (INT NEW-INFS)
          (SETF (NODE-SUPERIOR INT) OLD-INT))
        (IF INT-APPEND-P
            (SETQ LAST-INT (CAR (LAST OLD-INFS))
                  FIRST-INT (CAR NEW-INFS)
                  OLD-INFS (NCONC OLD-INFS NEW-INFS))
          (SETQ LAST-INT (CAR (LAST NEW-INFS))
                FIRST-INT (CAR OLD-INFS)
                OLD-INFS (NCONC NEW-INFS OLD-INFS)))
        (SETF (NODE-INFERIORS OLD-INT) OLD-INFS)
        (COND ((AND LAST-INT FIRST-INT)
               (LET* ((LAST-INT-END (INTERVAL-LAST-BP LAST-INT))
                      (FIRST-INT-START (INTERVAL-FIRST-BP FIRST-INT))
                      (LAST-INT-END-1 (INTERVAL-LAST-BP (CAR (NODE-INFERIORS LAST-INT))))
                      (MOVE-1-P (BP-= LAST-INT-END LAST-INT-END-1)))
                 (MOVE-BP LAST-INT-END FIRST-INT-START)
                 (AND MOVE-1-P (MOVE-BP LAST-INT-END-1 FIRST-INT-START)))
               (SETF (NODE-NEXT LAST-INT) FIRST-INT)
               (SETF (NODE-PREVIOUS FIRST-INT) LAST-INT))))
      (LET* ((NEW-ARRAY (ZMAIL-BUFFER-ARRAY NEW-FILE))
             (OLD-ARRAY (ZMAIL-BUFFER-ARRAY OLD-FILE))
             (NMSGS (ARRAY-ACTIVE-LENGTH NEW-ARRAY))
             (OLDLEN (ARRAY-ACTIVE-LENGTH OLD-ARRAY))
             (NEWLEN (+ NMSGS OLDLEN)))
        (AND (< (ARRAY-LENGTH OLD-ARRAY) NEWLEN)
             (ADJUST-ARRAY-SIZE OLD-ARRAY (TRUNCATE (* NEWLEN 5) 4)))
        (OR APPEND-P
            ;; If prepending, make space in the array
            (DO ((I (1- OLDLEN) (1- I))
                 (J (1- NEWLEN) (1- J)))
                ((< I 0))
              (ASET (AREF OLD-ARRAY I) OLD-ARRAY J)))
        (SETF (ARRAY-LEADER OLD-ARRAY 0) NEWLEN)
        (DO ((I 0 (1+ I))
             (J (IF APPEND-P OLDLEN 0) (1+ J))
             (MSG))
            (( I NMSGS))
          (SETQ MSG (AREF NEW-ARRAY I))
          ;; It is important that the message be parsed by the inbox buffer, so that UNSEEN
          ;; properties get put on.  That is why this MSG-PUT is before the :NEW-MSG.
          (MSG-PUT MSG T 'RECENT)
          (SEND OLD-FILE :NEW-MSG MSG)
          (ASET MSG OLD-ARRAY J))
        ;; If this is new mail for a prepending BABYL file, and there is still some old mail
        ;; to come in, the last new message won't get a formfeed.  Fix it now.
        (AND (NOT APPEND-P) (ZEROP OLDLEN) (PLUSP NEWLEN)
             (EQ (ZMAIL-DISK-BUFFER-STATUS OLD-FILE) :LOADING)
             (LET* ((MSG (AREF NEW-ARRAY (1- NEWLEN)))
                    (LAST-BP (INTERVAL-LAST-BP OLD-INT))
                    (MSG-LAST-BP (MSG-REAL-END-BP MSG))
                    (AT-END-P (BP-= LAST-BP MSG-LAST-BP)))
               (SEND OLD-FILE :UPDATE-MSG-END MSG T)
               (AND AT-END-P (MOVE-BP LAST-BP (END-LINE MSG-LAST-BP))))))))
  (COND ((EQ NEW-FILE *ZMAIL-BUFFER*)
         (SELECT-ZMAIL-BUFFER OLD-FILE (EQ NEW-FILE *PRIMARY-ZMAIL-BUFFER*)))
        ((EQ OLD-FILE *ZMAIL-BUFFER*)           ;*MSG-NO* may need changing
         (ZMAIL-SELECT-MSG *MSG* T NIL)))
  (MSG-POINT-PDL-FORWARD-ZMAIL-BUFFER NEW-FILE OLD-FILE))

(DEFUN ZMAIL-BUFFER-BACKGROUND-SAVE (ZMAIL-BUFFER)
  (COND ((AND (NEQ *ZMAIL-BACKGROUND-P* :DISABLE)
              (SEND ZMAIL-BUFFER :ASSOCIATED-INBOX-BUFFER))
         (ZMAIL-BUFFER-SAVE-SETUP ZMAIL-BUFFER)
         (ZMAIL-BACKGROUND-REQUEST-PUSH
           (LIST 'ZMAIL-BACKGROUND-SAVE-FILE
                 ZMAIL-BUFFER
                 (INTERVAL-STREAM (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER)))))
        (T
         (SETF (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) NIL))))

;;; New mail

;;; Note: all inbox buffer flavors must have the same instance variables,
;;; namely, those of INBOX-BUFFER.

(DEFUN MAKE-INBOX-BUFFER (TYPE LIST FROM-ZMAIL-BUFFER)
  (OR (ZMAIL-BUFFER-APPEND-P FROM-ZMAIL-BUFFER)
      (SETQ LIST (NREVERSE LIST)))
  (MAKE-ZMAIL-BUFFER TYPE
                     :ASSOCIATED-MAIL-FILE-BUFFER FROM-ZMAIL-BUFFER
                     :NAME (STRING-APPEND "New mail for " (STRING (SEND FROM-ZMAIL-BUFFER :NAME)))
                     :FILE-LIST LIST))

(DEFUN REPLACE-INBOX-BUFFER (FLAVOR)
  (DECLARE (:SELF-FLAVOR INBOX-BUFFER))
  ;; Parse all messages before replacing
  ;; since the way to parse depends on the flavor.
  (DOTIMES (I (ZMAIL-BUFFER-NMSGS SELF))
    (ASSURE-MSG-PARSED (AREF ARRAY I)))
  ;; Just magically change the flavor of the instance.
  (SI:ASSURE-FLAVOR-COMPOSED FLAVOR)
  (%P-STORE-POINTER SELF (GET FLAVOR 'SI:FLAVOR))
  SELF)

(DEFMETHOD (INBOX-BUFFER :AFTER :INIT) (IGNORE)
  (SEND ASSOCIATED-MAIL-FILE-BUFFER :SET-ASSOCIATED-INBOX-BUFFER SELF)
  (SETQ PENDING-FILE-LIST FILE-LIST
        STATUS :NEW-MAIL
        FILE-LIST-MAIL-CHECK-INFO (LOOP FOR X IN FILE-LIST
                                        COLLECT (LIST (CAR X) NIL))))

;;; This gets called when starting to get new mail or after one file of new mail
;;; has been read in.  It should return T if it has started something loading.
(DEFMETHOD (INBOX-BUFFER :START-NEXT-FILE) ()
  (DO ((FILE) (RENAME) (DELETE-P) (STR)
       (LOADING-NAME NIL NIL)
       (LOADING-TRUENAME NIL NIL))
      ((NULL PENDING-FILE-LIST) NIL)
    (SETF `(,FILE ,RENAME ,DELETE-P) (CAR PENDING-FILE-LIST))
    ;; If the next pending file is on a different host,
    ;; and needs a different flavor of INBOX-BUFFER,
    ;; replace this one with a suitable one and try again with that one.
    (UNLESS (EQ (TYPE-OF SELF) (SEND FILE :INBOX-BUFFER-FLAVOR))
      (RETURN (SEND (REPLACE-INBOX-BUFFER (SEND FILE :INBOX-BUFFER-FLAVOR))
                    :START-NEXT-FILE)))
    (COND ((NOT RENAME)
           ;;No file to rename to, see if new file exists
           (CONDITION-CASE ()
               (SETQ STREAM (OPEN FILE :DIRECTION :INPUT))
             (FS:FILE-NOT-FOUND (POP PENDING-FILE-LIST) NIL)
             (:NO-ERROR (POP PENDING-FILE-LIST) NIL)
             (ERROR)))
          ((IGNORE-ERRORS
             (SETQ STREAM (OPEN RENAME :DIRECTION :INPUT)))
           (LET ((TEM (CAR PENDING-FILE-LIST)))
             (POP PENDING-FILE-LIST)
             ;;If file to rename to already exists,
             ;;arrange for real file to get read in after saving done next time
             (SETQ NEXT-PENDING-FILE-LIST
                   (NCONC NEXT-PENDING-FILE-LIST (NCONS TEM)))))
          ((CONDITION-CASE ()
               (SETQ STR (OPEN FILE :DIRECTION NIL))
             (FS:FILE-NOT-FOUND (POP PENDING-FILE-LIST) NIL)
             (ERROR))
           (POP PENDING-FILE-LIST)
           ;;Rename to new file.  This does not use the :RENAME stream operation since that
           ;;doesn't work correctly on Tops-20 and does not return the TRUENAME.
           (SETQ LOADING-NAME (SEND STR :PATHNAME)
                 LOADING-TRUENAME (SEND STR :TRUENAME))
           (CONDITION-CASE ()
               (SEND FILE :RENAME RENAME)
             (FS:FILE-LOCKED
              (FORMAT *QUERY-IO* "~&File ~A is being accessed so cannot be renamed.  Pausing."
                      LOADING-TRUENAME)
              (PROCESS-SLEEP (* 10. 60.))
              (DO ()
                  (())
                (CONDITION-CASE ()
                    (SEND FILE :RENAME RENAME)
                  (FS:FILE-LOCKED NIL)
                  (:NO-ERROR
                   (FORMAT *QUERY-IO* "~&File successfully renamed, continuing.")
                   (RETURN T)))
                (UNLESS (Y-OR-N-P "Try again?  You can also abort. ")
                  (SIGNAL-CONDITION EH:ABORT-OBJECT)))))

           ;;Get the renamed file
           (SETQ STREAM (OPEN RENAME '(:IN)))))
    (WHEN STREAM
      (SETQ PATHNAME (OR LOADING-NAME (SEND STREAM :PATHNAME))
            NAME (SEND PATHNAME :STRING-FOR-PRINTING)
            STATUS :LOADING-NEW-MAIL)
      (START-LOADING-ZMAIL-BUFFER SELF STREAM T LOADING-TRUENAME)
      (AND DELETE-P (PUSH (SEND STREAM :TRUENAME) PENDING-DELETION-LIST))
      (RETURN T))))

;;; This is called when the one file is all the way in
(DEFMETHOD (INBOX-BUFFER :LOADING-DONE) ()
  (COND (STREAM
         (SEND STREAM :CLOSE)
         (SETQ STREAM NIL)))
  (OR (SEND SELF :START-NEXT-FILE)              ;We can still do more, continue
      (SEND (SEND ASSOCIATED-MAIL-FILE-BUFFER :ASSOCIATED-INBOX-BUFFER)
            :LOADING-DONE-INTERNAL)))

(DEFMETHOD (INBOX-BUFFER :LOADING-DONE-INTERNAL) ()
  ;; If the other file is all in or we are prepending, can put together now
  (AND (OR (NOT (ZMAIL-BUFFER-APPEND-P ASSOCIATED-MAIL-FILE-BUFFER))
           (MEMQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-MAIL-FILE-BUFFER)
                 '(NIL :AWAITING-NEW-MAIL :SAVING-REQUIRED)))
       (INSERT-NEW-MAIL ASSOCIATED-MAIL-FILE-BUFFER SELF))
  (SETQ STATUS :AWAITING-SAVE)
  (COND ((NOT (MEMQ (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-MAIL-FILE-BUFFER)
                    ;;Loading or already saving
                    '(NIL :AWAITING-NEW-MAIL :SAVING-REQUIRED))))
        (*INHIBIT-BACKGROUND-SAVES*
         (SETF (ZMAIL-DISK-BUFFER-STATUS ASSOCIATED-MAIL-FILE-BUFFER) :SAVING-REQUIRED))
        (T
         (ZMAIL-BUFFER-BACKGROUND-SAVE ASSOCIATED-MAIL-FILE-BUFFER))))

;;; This is called after the primary mail file has been saved out with us in it
;;; It should return T if it has started up again
(DEFMETHOD (INBOX-BUFFER :SAVING-DONE) (FORCING-OUT)
  (SETQ STATUS :NEW-MAIL)
  (DOLIST (FILE PENDING-DELETION-LIST)
    (CONDITION-CASE (ERROR)
        (SEND FILE :DELETE)
      (FS:FILE-ERROR
       (FORMAT *QUERY-IO* "~&Deletion error: ~A" ERROR))))
  (SETQ PENDING-DELETION-LIST NIL)
  (COND ((AND (NOT FORCING-OUT)
              (SETQ PENDING-FILE-LIST NEXT-PENDING-FILE-LIST))
         (SETQ NEXT-PENDING-FILE-LIST NIL)
         ;; Start things over
         (STORE-ARRAY-LEADER 0 ARRAY 0)
         (SETQ INFERIORS NIL)
         (LET ((LINE (CREATE-LINE 'ART-STRING 0 NIL)))
           (SETF FIRST-BP (CREATE-BP LINE 0 :NORMAL))
           (SETF LAST-BP (CREATE-BP LINE 0 :MOVES))
           (SETF (LINE-NODE LINE) SELF))
         (SEND SELF :START-NEXT-FILE))))

;;; This is called from the background process to see if there is new mail
(DEFMETHOD (INBOX-BUFFER :BACKGROUND-CHECK-FOR-NEW-MAIL) (&AUX -STREAM- CREATION-DATE)
  (AND (EQ STATUS :NEW-MAIL)                    ;Only if idle
       (DOLIST (ELEM FILE-LIST-MAIL-CHECK-INFO)
         (IGNORE-ERRORS
           ;; Don't wait a long time to bomb out if the host is down.
           ;; It could hold up the ZMAIL process.
           (WHEN (CHAOS:HOST-UP-P (PATHNAME-HOST (FIRST ELEM)) 60.)
             (SETQ -STREAM- (OPEN (FIRST ELEM) :DIRECTION NIL))))
         (COND ((AND -STREAM-
                     (NOT (EQUAL (SETQ CREATION-DATE (SEND -STREAM- :CREATION-DATE))
                                 (SECOND ELEM))))
                (SETF (SECOND ELEM) CREATION-DATE)
                (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS)
                    (TIME:DECODE-UNIVERSAL-TIME CREATION-DATE)
                  (ZMAIL-BACKGROUND-RESPONSE-PUSH
                    (LIST 'NEW-MAIL
                          "New mail in ~A at ~D:~2,'0D"
                          (FIRST ELEM)
                          HOURS MINUTES))))))))

;;; Hardcopy functions

(DEFVAR *HARDCOPY-DEVICE*)

;;; Options for all
(DEFVAR *INCLUDE-SUMMARY-ALIST*
        '(("Yes" :VALUE T :DOCUMENTATION "Print summary and messages.")
          ("No" :VALUE NIL :DOCUMENTATION "Do not print a summary.")
          ("Just summary" :VALUE :JUST-SUMMARY
           :DOCUMENTATION "Print summary but not messages themselves.")))

(DEFINE-ZMAIL-HARDCOPY-OPTION *HARDCOPY-SUMMARY-P* T :MENU-ALIST
                              "Include summary" *INCLUDE-SUMMARY-ALIST*)
(DEFINE-ZMAIL-HARDCOPY-OPTION *HARDCOPY-SEPARATE-PAGES* NIL :BOOLEAN
                              "Print each message on a separate page")
(DEFINE-ZMAIL-HARDCOPY-OPTION *HARDCOPY-SEPARATOR-LINE* NIL :STRING-OR-NIL
                              "Line between messages (when not on separate pages)")

(DEFVAR *ZMAIL-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS* NIL)

(DEFVAR *ZMAIL-SINGLE-MSG-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS* NIL)

(DEFVAR *ZMAIL-WHOLE-FILE-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS*
        '(*HARDCOPY-SUMMARY-P* *HARDCOPY-SEPARATE-PAGES* *HARDCOPY-SEPARATOR-LINE*))

(DEFSTRUCT (HARDCOPY-DEVICE :LIST :CONC-NAME)
  KEY
  FLAVOR
  OTHER-OPTIONS)

(DEFMACRO ADD-HARDCOPY-DEVICE (FLAVOR-NAME KEY NAME OTHER-OPTION-VARIABLES)
  `(ADD-HARDCOPY-DEVICE-1 ',FLAVOR-NAME ',KEY ',NAME ,OTHER-OPTION-VARIABLES))

(DEFVAR *HARDCOPY-DEVICE-ALIST* NIL)
(DEFVAR *HARDCOPY-DEVICE-MENU-ALIST* NIL)

(DEFUN ADD-HARDCOPY-DEVICE-1 (FLAVOR KEY NAME OTHER-OPTIONS &AUX DEVICE)
  (SETQ DEVICE (MAKE-HARDCOPY-DEVICE :KEY KEY
                                     :FLAVOR FLAVOR
                                     :OTHER-OPTIONS OTHER-OPTIONS))
  (SETQ *HARDCOPY-DEVICE-ALIST* (CONS DEVICE
                                      (DELQ (ASSQ KEY *HARDCOPY-DEVICE-ALIST*)
                                            *HARDCOPY-DEVICE-ALIST*)))
  (SETQ *HARDCOPY-DEVICE-MENU-ALIST* (CONS (CONS NAME KEY)
                                           (DELQ (RASSQ KEY *HARDCOPY-DEVICE-MENU-ALIST*)
                                                 *HARDCOPY-DEVICE-MENU-ALIST*)))
  ;; If this is being added after normal initializations
  (LET ((INIT (ASSOC "SITE:*HARDCOPY-DEVICE*" SI:SITE-INITIALIZATION-LIST)))
    (AND INIT (EVAL (SI:INIT-FORM INIT)))))

(DEFVAR *HARDCOPY-WHOLE-FILE-P*)

(DEFUN COMPUTE-HARDCOPY-CHOICES (DEVICE &AUX ALIST)
  (OR *HARDCOPY-DEVICE* (BARF "No known hardcopy devices at this site"))
  (SETQ DEVICE (ASSQ DEVICE *HARDCOPY-DEVICE-ALIST*))
  (SETQ ALIST `(*HARDCOPY-DEVICE*
                ,@(HARDCOPY-DEVICE-OTHER-OPTIONS DEVICE)
                ,@*ZMAIL-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS*
                ,@(IF *HARDCOPY-WHOLE-FILE-P*
                      *ZMAIL-WHOLE-FILE-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS*
                    *ZMAIL-SINGLE-MSG-DEVICE-INDEPENDENT-HARDCOPY-OPTIONS*)))
  (SETQ ALIST (MAPCAR #'(LAMBDA (X) (ASSQ X *ZMAIL-HARDCOPY-OPTION-ALIST*)) ALIST))
  (SETQ ALIST (TV:PRUNE-USER-OPTION-ALIST ALIST))
  ALIST)

(DEFUN CHOOSE-HARDCOPY-OPTIONS (NEAR-MODE *HARDCOPY-WHOLE-FILE-P*)
  (TV:CHOOSE-VARIABLE-VALUES (COMPUTE-HARDCOPY-CHOICES *HARDCOPY-DEVICE*)
                             :LABEL "Hardcopy options:"
                             :NEAR-MODE NEAR-MODE
                             :MARGIN-CHOICES '("Do It"
                                                ("Abort" (ABORT-CURRENT-COMMAND)))
                             :FUNCTION 'CHOOSE-HARDCOPY-OPTIONS-FUNCTION))

(DEFUN CHOOSE-HARDCOPY-OPTIONS-FUNCTION (WINDOW VARIABLE OLDVAL NEWVAL)
  OLDVAL
  (SEND *PROFILE-WINDOW* :VARIABLE-TICK)
  (COND ((EQ VARIABLE '*HARDCOPY-DEVICE*)
         (TV:WITH-SHEET-DEEXPOSED (WINDOW)
           (SEND WINDOW :SETUP (COMPUTE-HARDCOPY-CHOICES NEWVAL)
                    (SEND WINDOW :LABEL)
                    (SEND WINDOW :FUNCTION)
                    (SYMEVAL-IN-INSTANCE WINDOW 'TV:MARGIN-CHOICES)))
         T)))

;;; This makes hardcopy be a ZMAIL-BUFFER for GET-MOVE-ZMAIL-BUFFER
(DEFUN MAKE-HARDCOPY-ZMAIL-BUFFER (CHOOSE-OPTIONS-P FOR-WHOLE-FILE-P NEAR-MODE)
  (UNLESS *HARDCOPY-DEVICE* (BARF "No known hardcopy devices at this site"))
  (AND CHOOSE-OPTIONS-P (CHOOSE-HARDCOPY-OPTIONS NEAR-MODE FOR-WHOLE-FILE-P))
  (MAKE-INSTANCE (HARDCOPY-DEVICE-FLAVOR (ASSQ *HARDCOPY-DEVICE* *HARDCOPY-DEVICE-ALIST*))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-HARDCOPY-MSG "Hardcopy the current message." ()
  (SEND (MAKE-HARDCOPY-ZMAIL-BUFFER (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT) NIL
                                    (RECTANGLE-NEAR-COMMAND-MENU))
           :ADD-MSG *MSG*)
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-HARDCOPY-ALL "Hardcopy the current buffer." ()
  (SEND (MAKE-HARDCOPY-ZMAIL-BUFFER (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT) T
                                    (RECTANGLE-NEAR-COMMAND-MENU))
           :ADD-ZMAIL-BUFFER *ZMAIL-BUFFER*)
  DIS-NONE)

(DEFFLAVOR HARDCOPY-ZMAIL-BUFFER () ()
  (:INCLUDED-FLAVORS SI:LINE-OUTPUT-STREAM-MIXIN))

(DEFMETHOD (HARDCOPY-ZMAIL-BUFFER :PRINT-SELF) (STREAM &REST IGNORE)
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPEP)
    (PRIN1 (SEND SELF :NAME) STREAM)))

(DEFMETHOD (HARDCOPY-ZMAIL-BUFFER :DRAW-UNDERLINE) () NIL)

(DEFMACRO WITH-HARDCOPY-OUTPUT ((HARDCOPY-ZMAIL-BUFFER) &BODY BODY)
  `(UNWIND-PROTECT
     (PROGN ,@BODY
            (SEND ,HARDCOPY-ZMAIL-BUFFER :CLOSE))
     (SEND ,HARDCOPY-ZMAIL-BUFFER :CLOSE :ABORT)))

(DEFMETHOD (HARDCOPY-ZMAIL-BUFFER :ADD-MSG) (MSG &OPTIONAL ALREADY-IN-OTHER-PROCESS-P)
  (IF ALREADY-IN-OTHER-PROCESS-P
      (WITH-HARDCOPY-OUTPUT (SELF)
        (SEND SELF :OPEN (ZMAIL-BUFFER-NAME (MSG-MAIL-FILE-BUFFER MSG))
              (MSG-GET MSG :DATE))
        (HARDCOPY-ONE-MSG MSG SELF (MSG-DISPLAYED-INDEX MSG) NIL))
    (PROCESS-RUN-FUNCTION "ZMAIL HARDCOPY" SELF :ADD-MSG MSG T)))

;; Don't change appearance of a header just to print it.
(DEFMETHOD (HARDCOPY-ZMAIL-BUFFER :REFORMAT-MSG-HEADER) (IGNORE)
  NIL)

(DEFMETHOD (HARDCOPY-ZMAIL-BUFFER :ADD-ZMAIL-BUFFER) (ZMAIL-BUFFER
                                                      &OPTIONAL ALREADY-IN-OTHER-PROCESS-P)
  (IF ALREADY-IN-OTHER-PROCESS-P
      (LET (ARRAY NMSGS)
        (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)
              NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
        (WITH-HARDCOPY-OUTPUT (SELF)
          (SEND SELF :OPEN (ZMAIL-BUFFER-NAME ZMAIL-BUFFER) NIL)
          (COND (*HARDCOPY-SUMMARY-P*
                 (SEND SELF :LINE-OUT (SEND ZMAIL-BUFFER :FULL-NAME))
                 (SEND SELF :DRAW-UNDERLINE)
                 (SEND SELF :LINE-OUT *SUMMARY-WINDOW-LABEL*)
                 (DO ((I 0 (1+ I))
                      (MSG) (STATUS) (STRING))
                     (( I NMSGS))
                   (SETQ MSG (AREF ARRAY I)
                         ;; Maessage must be parsed before the MSG-SUMMARY-LINE is valid.
                         STATUS (ASSURE-MSG-PARSED MSG)
                         STRING (MSG-SUMMARY-LINE MSG))
                   (FORMAT SELF " ~3D~C" (1+ I) (STATUS-LETTER STATUS))
                   (SEND SELF :STRING-OUT STRING 0
                         (MIN (STRING-LENGTH STRING) 90.))
                   (SEND SELF :TYO #\CR))))
          (COND ((NEQ *HARDCOPY-SUMMARY-P* :JUST-SUMMARY)
                 (DO ((I 0 (1+ I))
                      (MSG)
                      (FIRST-P T NIL))
                     (( I NMSGS))
                   (COND ((IF FIRST-P *HARDCOPY-SUMMARY-P*
                            *HARDCOPY-SEPARATE-PAGES*)
                          (SEND SELF :TYO #\PAGE))
                         ((AND *HARDCOPY-SEPARATOR-LINE* (NOT FIRST-P))
                          (SEND SELF :LINE-OUT *HARDCOPY-SEPARATOR-LINE*)))
                   (SETQ MSG (AREF ARRAY I))
                   (HARDCOPY-ONE-MSG MSG SELF I ZMAIL-BUFFER))))))
    (PROCESS-RUN-FUNCTION "ZMAIL HARDCOPY" SELF :ADD-ZMAIL-BUFFER ZMAIL-BUFFER T)))

(DEFUN HARDCOPY-ONE-MSG (MSG HARDCOPY-ZMAIL-BUFFER INDEX ZMAIL-BUFFER &AUX TEM)
  (FORMAT HARDCOPY-ZMAIL-BUFFER "Message #~D" (1+ INDEX))
  (AND (NEQ ZMAIL-BUFFER (SETQ TEM (MSG-MAIL-FILE-BUFFER MSG)))
       (FORMAT HARDCOPY-ZMAIL-BUFFER " (from ~A)" (ZMAIL-BUFFER-NAME TEM)))
  (COND ((SETQ TEM (MSG-GET MSG 'KEYWORDS-STRING))
         (SEND HARDCOPY-ZMAIL-BUFFER :TYO #\SP)
         (SEND HARDCOPY-ZMAIL-BUFFER :STRING-OUT TEM)))
  (SEND HARDCOPY-ZMAIL-BUFFER :TYO #\CR)
  (STREAM-OUT-INTERVAL HARDCOPY-ZMAIL-BUFFER (MSG-INTERVAL MSG)))

;;; Hardware specific

(DEFFLAVOR HARDCOPY-TO-FILE-MIXIN
        (*FILE-STREAM*)
        ()
  (:INCLUDED-FLAVORS HARDCOPY-ZMAIL-BUFFER))

(DEFMETHOD (HARDCOPY-TO-FILE-MIXIN :TYO) PASS-MESSAGE-TO-FILE-STREAM)
(DEFMETHOD (HARDCOPY-TO-FILE-MIXIN :STRING-OUT) PASS-MESSAGE-TO-FILE-STREAM)
(DEFMETHOD (HARDCOPY-TO-FILE-MIXIN :LINE-OUT) PASS-MESSAGE-TO-FILE-STREAM)

(DEFUN PASS-MESSAGE-TO-FILE-STREAM (&REST ARGS)
  (DECLARE (:SELF-FLAVOR HARDCOPY-TO-FILE-MIXIN))
  (APPLY *FILE-STREAM* ARGS))

(DEFMETHOD (HARDCOPY-TO-FILE-MIXIN :CLOSE) (&OPTIONAL ABORT-P)
  (SEND *FILE-STREAM* :CLOSE ABORT-P))

;;; TPL
(DEFFLAVOR TPL-HARDCOPY-ZMAIL-BUFFER
        ()
        (HARDCOPY-TO-FILE-MIXIN HARDCOPY-ZMAIL-BUFFER))

(ADD-HARDCOPY-DEVICE TPL-HARDCOPY-ZMAIL-BUFFER :TPL "TPL" NIL)

(DEFMETHOD (TPL-HARDCOPY-ZMAIL-BUFFER :NAME) () "TPL:")

(DEFMETHOD (TPL-HARDCOPY-ZMAIL-BUFFER :OPEN) (IGNORE IGNORE)
  (SETQ *FILE-STREAM* (OPEN "TPL:" '(:OUT))))

;;; This comes after all the hardware specific options, so that it gets the right
;;; possibilities.
(DEFINE-SITE-ALIST-USER-OPTION (*HARDCOPY-DEVICE* *ZMAIL-HARDCOPY-OPTION-ALIST*)
                               "Output device" *HARDCOPY-DEVICE-MENU-ALIST*
                               :DEFAULT-HARDCOPY-MODE)

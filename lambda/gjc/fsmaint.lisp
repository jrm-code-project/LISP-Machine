;;; -*- Mode:LISP; Package:FILE-SYSTEM; Patch-File:T; Base:10 -*-


(defun maint-delete-and-expunge (&optional filename dont-ask &aux pathname file)
  (cond ((null filename)
         (do ()
             ((null (setq filename (prompt-and-read ':string-or-nil "~&Filename to Bash>")))
              "now. dont forget to (LM-SALVAGE)!!!!")
           (maint-delete-and-expunge filename)))
        ((LISTP FILENAME)
         (MAPCAR '(lambda (f) (MAINT-DELETE-AND-EXPUNGE f dont-ask)) filename))
        ('else
         (setq pathname (fs:parse-pathname filename))
         (cond ((not (typep pathname 'lm-pathname))
                (format t "~&;Not a local pathname: ~S~%" pathname))
               ((if dont-ask nil
                  (not (yes-or-no-p (format nil "Bash away on: ~S ?" pathname))))
                (format t "~&;OK. No action taken~%"))
               ((not (SETQ FILE (LOOKUP-FILE
                                  (PATHNAME-RAW-DIRECTORY PATHNAME)
                                  (PATHNAME-RAW-NAME PATHNAME)
                                  (PATHNAME-RAW-TYPE PATHNAME)
                                  (PATHNAME-RAW-VERSION PATHNAME))))
                (format t "Lookup-file failed on: ~S ~S ~S ~S"
                        (PATHNAME-RAW-DIRECTORY PATHNAME)
                        (PATHNAME-RAW-NAME PATHNAME)
                        (PATHNAME-RAW-TYPE PATHNAME)
                        (PATHNAME-RAW-VERSION PATHNAME)))
               ('else
                (catch-error (lmfs-delete-file file nil t))
                (catch-error (lmfs-expunge-file file t t))
                "now. dont forget to (LM-SALVAGE)!!!!")))))


(defun maint-delete-losing-files ()
  (maint-delete-and-expunge (choose-losing-files) t))

(DEFUN CHOOSE-LOSING-FILES ()
  (union ()
         (remq nil
               (MAPCAR #'(LAMBDA (X)
                           (apply 'choose-a-loser
                                  (mapcar #'(lambda (file)
                                              (and (typep file 'file)
                                                   (fs:make-pathname ':host "LM"
                                                                     ':directory (DIRECTORY-FULL-NAME (FILE-DIRECTORY FILE))
                                                                     ':name (FILE-NAME FILE)
                                                                     ':type (FILE-TYPE FILE)
                                                                     ':version (FILE-VERSION FILE))))
                                          x)))
                       *SHARED-BLOCK-SCREWS*))))


(DEFUN CHOOSE-A-LOSER (LOSER-A LOSER-B)
  (cond ((null loser-a)
         (format t "~&Other was directory, so punting ~S" loser-b)
         loser-b)
        ((null loser-b)
         (format t "~&Other was directory, so punting ~S" loser-a)
         loser-a)
        ((not (catch-error (probef loser-a)))
         (format t "~&~S is deleted so it is the loser by default." loser-a)
         loser-a)
        ((not (catch-error (probef loser-b)))
         (format t "~&~S is deleted so it is the loser by default." loser-b)
         loser-b)
        ('else
         (LISTF (SEND LOSER-A ':NEW-VERSION ':WILD))
         (LISTF (SEND LOSER-B ':NEW-VERSION ':WILD))
         (DO ((LOSER))
             ((SETQ LOSER (TV:MENU-CHOOSE (LIST LOSER-A LOSER-B) "Choose a file to lose"))
              LOSER)
           (format t "~&Aw, come on, you gotta chose one!")))))

(DEFUN LMFS-DELETE-FILE (FILE &OPTIONAL (WRITE-DIRECTORY T) dont-change-map)
  "Guarantees that FILE has its Deleted bit set."
  (LOCKING-RECURSIVELY (FILE-LOCK FILE)
    (REQUIRE-DELETABLE-FILE FILE)
    (UNLESS (FILE-DELETED? FILE)
      (IF (DIRECTORY? FILE)
          (IF (NULL (LET ((FILES (DIRECTORY-FILES file)))
                      (IF (EQ FILES ':DISK)
                          (WITH-MAP-STREAM-IN (STREAM (FILE-MAP file))
                            (LMFS-READ-DIRECTORY STREAM file))
                        FILES)))
              (SETF (FILE-DELETED? FILE) T)
            (LM-SIGNAL-ERROR 'DIRECTORY-NOT-EMPTY))
        (SETF (FILE-DELETED? FILE) T))
      (WHEN (and (FILE-CLOSED? FILE) (not dont-change-map))
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE) PUT-USED PUT-RESERVED)))
      (WHEN WRITE-DIRECTORY (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))))))


(DEFUN LMFS-EXPUNGE-FILE (FILE &optional dont-change-map ignore-open-count)
  "Guarantees that FILE is in the Nonexistent state.
Does NOT write out FILE's containing directory.  The caller must do that.
This helps avoid lock-up when trying to create free space when disk is full."
  (LOCKING (FILE-LOCK FILE)
    (REQUIRE-DELETABLE-FILE FILE)
    (COND ((FILE-CLOSED? FILE)
           (IF (or (ZEROP (FILE-OPEN-COUNT FILE)) ignore-open-count)
               (REMOVE-FILE-FROM-DIRECTORY FILE)
             (FERROR NIL "File being expunged is still open.")))
          (T
           (INCF (FILE-OPEN-COUNT FILE))
           (REQUIRE-ZERO-OPEN-COUNT FILE)
           (SETF (FILE-CLOSED? FILE) T)
           (IF (FILE-OVERWRITE-FILE FILE)
               ;; This file was overwriting another, and therefore
               ;; not actually in the directory.
               ;; Make the other file forget this one was overwriting it.
               (LET ((OTHER-FILE (FILE-OVERWRITE-FILE FILE)))
                 (DECF (FILE-OPEN-COUNT OTHER-FILE))
                 (SETF (FILE-OVERWRITE-FILE OTHER-FILE) NIL))
             ;; This file was not overwriting another,
             ;; so it really is in the directory.  Remove it.
             (REMOVE-FILE-FROM-DIRECTORY FILE))))
    (or dont-change-map
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE)
                                 (IF (FILE-DELETED? FILE) PUT-RESERVED PUT-USED)
                                 PUT-FREE)))
    (SETF (FILE-DELETED? FILE) T)
;    (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))
))


(defvar *shared-block-screws* nil)

(DEFUN LM-SALVAGE (&OPTIONAL (SALVAGER-VERBOSE-P T)
                   &AUX (SIZE (DC-PARTITION-SIZE)) (SALVAGER-ERRORS 0))
  (SETQ SALVAGER-BAD-ITEMS NIL)
  (setq *shared-block-screws* nil)
  (IF (AND SALVAGER-TABLE (= (ARRAY-ACTIVE-LENGTH SALVAGER-TABLE) SIZE))
      (FILLARRAY SALVAGER-TABLE '(NIL))
    (SETQ SALVAGER-TABLE (MAKE-ARRAY SIZE ':AREA LOCAL-FILE-SYSTEM-AREA)))
  (NOTIFY "Flushing out any unsaved directories")
  (SAVE-DIRECTORY-TREE ':FIND-ALL)
  (USING-PUT
    (LOCKING DISK-CONFIGURATION-LOCK
      (NOTIFY "Beginning salvage")
      (SALVAGER-ADD-BLOCK 0 1 'CONFIGURATION-BLOCK)
      (SALVAGER-ADD-BLOCK (DC-PUT-BASE) (DC-PUT-SIZE) 'PAGE-USAGE-TABLE)
      (LET ((ROOT (DC-ROOT-DIRECTORY)))
        (SALVAGER-ADD-MAP (FILE-MAP ROOT) ROOT)
        (LM-SALVAGE-DIRECTORY ROOT))
      (COND ((ZEROP SALVAGER-ERRORS)
             (NOTIFY "Salvage Completed.  Updating Page Usage Table.")
             (SALVAGE-RECONSTRUCT-PUT)
             (SETQ OLD-STATE PUT-CONSISTENT     ;See USING-PUT
                   PUT-MODIFIED T)
             (NOTIFY "Page Usage Table Updated."))
            (T (NOTIFY "Salvage Completed.  ~D. error~:P; Page Usage Table not updated."
                       SALVAGER-ERRORS))))))

(DEFUN SALVAGER-ADD-BLOCK (BASE NPAGES THING)
  (LOOP WITH LIM = (+ BASE NPAGES)
        FOR I FROM BASE BELOW LIM
        AS ENTRY = (AREF SALVAGER-TABLE I)
        WHEN ENTRY DO
        (AND SALVAGER-VERBOSE-P
             (FORMAT ERROR-OUTPUT "~%Address ~O contains both ~A and ~A" I ENTRY THING))
        (OR (SYMBOLP ENTRY)
            (MEMQ ENTRY SALVAGER-BAD-ITEMS)
            (PUSH ENTRY SALVAGER-BAD-ITEMS))
        (OR (SYMBOLP THING)
            (MEMQ THING SALVAGER-BAD-ITEMS)
            (PUSH THING SALVAGER-BAD-ITEMS))
        (DO ((L *SHARED-BLOCK-SCREWS* (CDR L)))
            ((OR (NULL L)
                 (OR (AND (EQ (CAAR L) ENTRY)
                          (EQ (CADAR L) THING))
                     (AND (EQ (CAAR L) THING)
                          (EQ (CADAR L) ENTRY))))
             (OR L (PUSH (LIST ENTRY THING) *SHARED-BLOCK-SCREWS*))))
        (INCF SALVAGER-ERRORS)
        ELSE DO (ASET THING SALVAGER-TABLE I)))

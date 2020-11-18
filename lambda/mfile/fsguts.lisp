;;; -*- Mode: Lisp; Package: Moby-File-System; Base: 10.; Readtable: T -*-
;;; The guts of the filesystem

;(DEFUN INITIALIZE-FILE-SYSTEM (PARTITION-HOST)
;  (*CATCH 'BOOT-FILE-SYSTEM
;    (DISMOUNT-FILE-SYSTEM PARTITION-HOST)
;    ;; Setup pathnames.
;    (DEFINE-MOBY-FILE-SYSTEM-HOST PARTITION-HOST)
;    ))

;(DEFUNP BOOT-FILE-SYSTEM (PARTITION-HOST &AUX MUST-SALVAGE)
;  (*CATCH 'BOOT-FILE-SYSTEM
;    (DISMOUNT-FILE-SYSTEM PARTITION-HOST)
;    ;; Setup pathnames.
;    (DEFINE-MOBY-FILE-SYSTEM-HOST PARTITION-HOST)      ;added RG 5/22/85
;    ;; Setup the auxiliary MOBY-DISK-CONFIGURATION variables.
;    ))

(DEFUN DISMOUNT-FILE-SYSTEM (PARTITION-HOST-NAME)
  ;; Declared later in this file.
  (let ((partition-host (partition-host-of-partition-host-name partition-host-name)))
    (DECLARE (SPECIAL SALVAGER-TABLE))
    (COND ((LOOKUP-SECTION-DIRECTORY-HEADER
             partition-host) ; if we were running before  **
         (MBFS-CLOSE-ALL-FILES)
;        (SAVE-DIRECTORY-TREE ':FIND-ALL)       ;Make sure directories written out.

         )
        (t ; if we weren't, remove the kludgey LOCAL host
         (undefine-moby-file-system-host partition-host)))))

(DEFSUBST REQUIRE-MOBY-ROOT-DIRECTORY (PARTITION-HOST)
  (cond ((lookup-section-directory-header partition-host))
        (t (MLM-SIGNAL-ERROR 'FS:NO-FILE-SYSTEM))))

(DEFUN NOTIFY (FORMAT-STRING &REST REST)
  (FORMAT ERROR-OUTPUT "~&[File System: ")
  (LEXPR-FUNCALL #'FORMAT ERROR-OUTPUT FORMAT-STRING REST)
  (FORMAT ERROR-OUTPUT "]~&"))

;;;; Salvager

;(DEFVAR SALVAGER-BAD-ITEMS NIL)
;(DEFVAR SALVAGER-TABLE NIL)
;(DEFVAR SALVAGER-ERRORS)
;(DEFVAR SALVAGER-VERBOSE-P)    ;This being NIL only clamps the long-winded stuff...

;(DEFUN MLM-SALVAGE (&OPTIONAL (SALVAGER-VERBOSE-P T)
;                  &AUX (SIZE (VMEM-PARTITION-SIZE)) (SALVAGER-ERRORS 0))
;  (SETQ SALVAGER-BAD-ITEMS NIL)
;  (IF (AND SALVAGER-TABLE (= (ARRAY-ACTIVE-LENGTH SALVAGER-TABLE) SIZE))
;      (FILLARRAY SALVAGER-TABLE '(NIL))
;    (SETQ SALVAGER-TABLE (MAKE-ARRAY SIZE ':AREA MOBY-FILE-SYSTEM-AREA)))
;  (NOTIFY "Flushing out any unsaved directories")
;  (SAVE-DIRECTORY-TREE ':FIND-ALL)
;  (USING-PUT
;    (with-lock (*moby-root-lock*)
;      (NOTIFY "Beginning salvage")
;      (SALVAGER-ADD-BLOCK 0 1 'CONFIGURATION-BLOCK)
;      (SALVAGER-ADD-BLOCK (MDC-PUT-BASE) (MDC-PUT-SIZE) 'PAGE-USAGE-TABLE)
;      (LET ((ROOT *MOBY-ROOT-DIRECTORY*))
;       ;(SALVAGER-ADD-MAP (MFILE-MAP ROOT) ROOT)
;       (MLM-SALVAGE-DIRECTORY ROOT))
;      (COND ((ZEROP SALVAGER-ERRORS)
;            (NOTIFY "Salvage Completed.  Updating Page Usage Table.")
;            (SALVAGE-RECONSTRUCT-PUT)
;            (SETQ OLD-STATE PUT-CONSISTENT     ;See USING-PUT
;                  PUT-MODIFIED T)
;            (NOTIFY "Page Usage Table Updated."))
;           (T (NOTIFY "Salvage Completed.  ~D. error~:P; Page Usage Table not updated."
;                      SALVAGER-ERRORS))))))

;(DEFUN MLM-SALVAGE-DIRECTORY (DIR)
;  (DOLIST (FILE (MFILE-FILES DIR))
;    (COND ((MFILE-OVERWRITE-FILE FILE)
;          (SETQ FILE (MFILE-OVERWRITE-FILE FILE))
;          (AND SALVAGER-VERBOSE-P
;               (FORMAT ERROR-OUTPUT "~%File overwriting ~A eliminated."
;                       (MLM-NAMESTRING NIL NIL
;                                      (MFILE-NAME DIR)
;                                      (MFILE-NAME FILE)
;                                      (MFILE-TYPE FILE)
;                                      (MFILE-VERSION FILE)))))
;         ((NULL (MFILE-CLOSED? FILE))
;          (FORMAT ERROR-OUTPUT "~%File ~A has not been closed. Closing it."
;                  (MLM-NAMESTRING NIL NIL
;                                 (MFILE-NAME DIR)
;                                 (MFILE-NAME FILE)
;                                 (MFILE-TYPE FILE)
;                                 (MFILE-VERSION FILE)))
;          (SET-MFILE-ATTRIBUTE FILE T ':CLOSED)))
;    ;(SALVAGER-ADD-MAP (MFILE-MAP FILE) FILE)
;    (IF (MDIRECTORY? FILE)
;       (MLM-SALVAGE-DIRECTORY FILE))))

;(DEFUN SALVAGER-ADD-BLOCK (BASE NPAGES THING)
;  (LOOP WITH LIM = (+ BASE NPAGES)
;       FOR I FROM BASE BELOW LIM
;       AS ENTRY = (AREF SALVAGER-TABLE I)
;       WHEN ENTRY DO
;       (AND SALVAGER-VERBOSE-P
;            (FORMAT ERROR-OUTPUT "~%Address ~O contains both ~A and ~A" I ENTRY THING))
;       (OR (SYMBOLP ENTRY)
;           (MEMQ ENTRY SALVAGER-BAD-ITEMS)
;           (PUSH ENTRY SALVAGER-BAD-ITEMS))
;       (OR (SYMBOLP THING)
;           (MEMQ THING SALVAGER-BAD-ITEMS)
;           (PUSH THING SALVAGER-BAD-ITEMS))
;       (INCF SALVAGER-ERRORS)
;       ELSE DO (ASET THING SALVAGER-TABLE I)))

;;(DEFUN SALVAGER-ADD-MAP (MAP THING)
;;  (LOOP WITH NBLOCKS = (MOBY-MAP-NBLOCKS MAP)
;;      FOR I FROM 0 BELOW NBLOCKS
;;      DO (SALVAGER-ADD-BLOCK (MOBY-MAP-BLOCK-LOCATION MAP I)
;;                             (CEILING (MOBY-MAP-BLOCK-SIZE MAP I) PAGE-SIZE-IN-BITS)
;;                             THING)))

;;;; Read/Write Directories

;;; A directory is marked as needing rewriting by marking it, and then
;;; recursively marking all of its superdirectories up to the root.
;;; If this loop is aborted out of, then the change made to that directory
;;; is temporarily lost, unless we are willing to search the entire tree.
;;; For this reason we mark the tree while interrupts are off.

;;; Once the directory has been marked in this way, a treewalk is started
;;; at the root, which identifies the changed directories by walking top-down,
;;; and then saves them walking bottom-up.  Normally we only want to search
;;; down those paths that are marked as modified.  However, an optional
;;; argument is provided to force the saving process to walk the entire tree,
;;; thus guaranteeing that all modified directories are saved even if they
;;; were improperly marked.

;(DEFUN WRITE-DIRECTORY-FILES (DIRECTORY)
;  "Mark all of the directories from here to the root as needing to
;be written out, then treewalk the structure, saving them."
;  (WITHOUT-INTERRUPTS
;    (DO ((DIR DIRECTORY (MFILE-DIRECTORY DIR)))
;       ((NULL DIR))
;      (SETF (MFILE-CLOSED? DIR) NIL)))
;  (SAVE-DIRECTORY-TREE NIL))

;(DEFUN WRITE-DIRECTORY-OF-FILE (FILE)
;  (with-lock ((MFILE-LOCK FILE))
;    (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY FILE))))

;(DEFUN SAVE-DIRECTORY-TREE (&OPTIONAL DO-ALL?)
;  "Saves a directory tree out to disk.
;If DO-ALL? is NIL then only find directory changes that start at the root,
;and only write out changed directories.
;If DO-ALL? is :FIND-ALL then only write out changed directories, but find
;every changed directory by exhaustive search.
;If DO-ALL? is :SAVE-ALL then write out the entire tree independent of its
;state of modification."
;  (LET ((OLD-MAPS (SAVE-DIRECTORY-SUBTREE *MOBY-ROOT-DIRECTORY* DO-ALL?)))
;    (UNLESS (NULL OLD-MAPS)
;      (USING-PUT
; ;     (DOLIST (MAP OLD-MAPS)
; ;       (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-FREE))
;       (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
; ;     (with-lock (*moby-root-lock*)
; ;       (MLM-WRITE-CONFIGURATION))
;       ))))

;(DEFUN SAVE-DIRECTORY-SUBTREE (DIRECTORY DO-ALL?)
;  (LET ((FILES (MFILE-FILES DIRECTORY)))
;    (LET ((SUBDIRECTORY-RESULTS
;           (MAPCAN #'(LAMBDA (FILE)
;                       (AND (MDIRECTORY? FILE)
;                            (OR DO-ALL? (NOT (MFILE-CLOSED? FILE)))
;                            (SAVE-DIRECTORY-SUBTREE FILE DO-ALL?)))
;                   (COND ((NOT (EQ FILES ':DISK))
;                          FILES)
;                         ((EQ DO-ALL? ':SAVE-ALL)
;                          (MFILE-FILES DIRECTORY))
;                         (T '())))))
;      (IF (OR (EQ DO-ALL? ':SAVE-ALL)
;             (NOT (MFILE-CLOSED? DIRECTORY)))
;         (CONS (MBFS-WRITE-DIRECTORY DIRECTORY) SUBDIRECTORY-RESULTS)
;       SUBDIRECTORY-RESULTS))))

(DEFUN TOP-LEVEL-DIRECTORIES (PARTITION-HOST)
  (SUBSET #'MDIRECTORY? (LOOKUP-ROOT-DIRECTORY-FILES PARTITION-HOST)))

(DEFUN MBFS-WRITE-DIRECTORY (DIRECTORY)
  )

;;;; Basic File Operations
;;; Files come in four states:

;;; Nonexistent: an invalid state which should never be encountered.
;;; The storage associated with this state is FREE.

;;; Unclosed:  the normal state for newly-opened output files, it is
;;; indicated by the Closed bit being off.  The Deleted bit can be
;;; either on or off; when closed, the file retains that property.
;;; The storage associated with this state is RESERVED.

;;; Closed:  the normal state for existing files, it is indicated by the
;;; Closed bit being on and the Deleted bit being off.  The storage
;;; associated with this state is USED.

;;; Deleted:  a special state between closed and nonexistent, this is
;;; indicated by the Closed bit being on and the Deleted bit being on.
;;; The storage associated with this state is RESERVED.

;;; Logically, there should be these transition functions:

;;; Nonexistent <-> Unclosed
;;; Unclosed -> Closed
;;; Closed <-> Deleted
;;; Deleted -> Nonexistent

;;; However, the implementation is based on target states only.

;;; The locking strategy is:  lock a file first, then lock its directory.
;;; Never lock the file second.  There is only
;;; one case where two files need to be locked, and one of those is
;;; an Unclosed file; only one process should point to that file.

;;; Unclosed files are in the directory; however it is not possible
;;; to open them.  Deleted files are also in the directory, and can be opened.

;;; When a file is overwritten, it is left in the directory.
;;; It is made to point to the file which is overwriting it,
;;; and the overwriting file is made to point to the overwritten one also.
;;; It is possible to tell which is which on the basis of the closed bit.
;;; The overwritten file is closed, and the overwriting file isn't.

(DEFVAR MBFS-DIRECTORY-TYPE "DIRECTORY" "The file type for all directories.")
(DEFVAR MBFS-DIRECTORY-VERSION 1 "The file version for all directories.")

(DEFSUBST MBFS-DELETED-FILE? (FILE)
  (AND (MFILE-DELETED? FILE)
       (NOT (MFILE-CLOSED? FILE))))

(DEFSUBST MBFS-CLOSED-FILE? (FILE)
  (AND (MFILE-CLOSED? FILE)
       (NOT (MFILE-DELETED? FILE))))

(DEFSUBST MBFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? (FILE)
  (OR (MINUSP (MFILE-OPEN-COUNT FILE))
      (NOT (AND (MFILE-CLOSED? FILE)
                (NULL (MFILE-OVERWRITE-FILE FILE))))))

(DEFSUBST REQUIRE-CLOSED-FILE (FILE)
  (IF (OR (NOT (MFILE-CLOSED? FILE))
          (MINUSP (MFILE-OPEN-COUNT FILE)))
      (MLM-SIGNAL-ERROR 'OPEN-OUTPUT-FILE)))

(DEFSUBST REQUIRE-CLOSED-FILE-NOT-BEING-SUPERSEDED (FILE)
  (IF (MBFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? FILE)
      (MLM-SIGNAL-ERROR 'OPEN-OUTPUT-FILE)))

(DEFSUBST REQUIRE-FILE-NOT-OPEN (FILE)
  (REQUIRE-CLOSED-FILE FILE)
  (UNLESS (ZEROP (MFILE-OPEN-COUNT FILE))
    (MLM-SIGNAL-ERROR 'FS:FILE-LOCKED)))

(DEFSUBST REQUIRE-DELETABLE-FILE (FILE)
  (IF (MFILE-ATTRIBUTE FILE ':DONT-DELETE)
      (MLM-SIGNAL-ERROR 'FS:DONT-DELETE-FLAG-SET)))

(DEFSUBST REQUIRE-ZERO-OPEN-COUNT (FILE)
  (IF (NOT (ZEROP (MFILE-OPEN-COUNT FILE)))
      (FERROR NIL "Expected zero open count for ~S." FILE)))

(DEFUN MBFS-OPEN-OUTPUT-FILE
  (PARTITION-HOST DIRECTORY LOC NAME TYPE VERSION OVERWRITTEN-FILE MOBY-MAPPED OPTIONS)
  ;  (WHEN (< (AREF PUT-USAGE-ARRAY PUT-FREE) PUT-MINIMUM-FREE-PAGES)
  ;    (MLM-SIGNAL-ERROR 'FS:NO-MORE-ROOM))
  (REQUIRE-LOCK (MFILE-LOCK DIRECTORY))
  (LET ((FILE (CREATE-NEW-FILE PARTITION-HOST DIRECTORY NAME TYPE VERSION)))
    (SETF (MFILE-DELETED? FILE) NIL)
    (SETF (MMOBY-MAPPED? FILE) (NOT (NULL MOBY-MAPPED)))
    (COND (MOBY-MAPPED
           (LET* ((MAPPED-AREA (MOBY-MAP-SECTION PARTITION-HOST FILE :OUTPUT OPTIONS))
                  (DEFAULT-CONS-AREA MAPPED-AREA))
             (SETF (MFILE-DATA-POINTER FILE)
                   (COND ((EQ MOBY-MAPPED T)
                          (CONS NIL NIL))
                         (T (APPLY 'MAKE-ARRAY MOBY-MAPPED)))))
           (SETF (MFILE-CLOSED? FILE) T)
           (SETF (MFILE-OPEN-COUNT FILE) 0))
          (T
           (SETF (MFILE-CLOSED? FILE) NIL)
           (SETF (MFILE-OPEN-COUNT FILE) -1)))
    (COND ((NULL OVERWRITTEN-FILE)
           (SI:PUSH-IN-AREA (ROOT-AREA-OF-PARTITION-HOST PARTITION-HOST) FILE (CDR LOC))
           (PROCESS-UNLOCK (LOCF (MFILE-LOCK DIRECTORY))))
          (T
           ;; This is critical; unlock directory before locking file.
           (PROCESS-UNLOCK (LOCF (MFILE-LOCK DIRECTORY)))
           (with-lock ((MFILE-LOCK OVERWRITTEN-FILE))
             (REQUIRE-CLOSED-FILE-NOT-BEING-SUPERSEDED OVERWRITTEN-FILE)
             (REQUIRE-DELETABLE-FILE OVERWRITTEN-FILE)
             (SETF (MFILE-OVERWRITE-FILE FILE) OVERWRITTEN-FILE)
             (INCF (MFILE-OPEN-COUNT OVERWRITTEN-FILE))
             (SETF (MFILE-OVERWRITE-FILE OVERWRITTEN-FILE) FILE))))
 ;    (WRITE-DIRECTORY-FILES DIRECTORY)
    (moby-writeout-root partition-host)
    FILE))

(DEFUN MBFS-OPEN-OVERWRITE-FILE (FILE)
  (with-lock ((MFILE-LOCK FILE))
    (REQUIRE-FILE-NOT-OPEN FILE)
    (DECF (MFILE-OPEN-COUNT FILE))))

(DEFUN MBFS-OPEN-INPUT-FILE (FILE)
  (with-lock ((MFILE-LOCK FILE))
    (REQUIRE-CLOSED-FILE FILE)
    (INCF (MFILE-OPEN-COUNT FILE))))

(DEFUN MBFS-CLOSE-FILE (FILE)
  "Called when one stream reading or writing FILE is closed.
Updates FILE's open-count.  Guarantees that FILE has its Closed bit set."
  (with-lock ((MFILE-LOCK FILE))
    (COND ((MINUSP (MFILE-OPEN-COUNT FILE))
           (UNLESS (= (MFILE-OPEN-COUNT FILE) -1)
             (FERROR NIL "File open count is less than -1."))
           (INCF (MFILE-OPEN-COUNT FILE)))
          ((PLUSP (MFILE-OPEN-COUNT FILE))
           (DECF (MFILE-OPEN-COUNT FILE)))
          (T (FERROR NIL "Open count of file being closed is zero.")))
    (UNLESS (MFILE-CLOSED? FILE)
      (REQUIRE-ZERO-OPEN-COUNT FILE)
      (SETF (MFILE-CLOSED? FILE) T)
      (UNLESS (MFILE-DELETED? FILE)
;       (USING-PUT                              ****
;         (CHANGE-MAP-DISK-SPACE (MFILE-MAP FILE) PUT-RESERVED PUT-USED))
        )
      (LET ((OVERWRITTEN-FILE (MFILE-OVERWRITE-FILE FILE))
            (DIRECTORY (MFILE-DIRECTORY FILE)))
        (UNLESS (NULL OVERWRITTEN-FILE)
          (with-lock ((MFILE-LOCK OVERWRITTEN-FILE))
            (SETF (MFILE-OVERWRITE-FILE FILE) NIL)
            (DECF (MFILE-OPEN-COUNT OVERWRITTEN-FILE))
            (SETF (MFILE-OVERWRITE-FILE OVERWRITTEN-FILE) NIL)
            (REPLACE-FILE-IN-DIRECTORY OVERWRITTEN-FILE FILE)
            (COND ((MFILE-DELETED? OVERWRITTEN-FILE)
;                  (USING-PUT           ****
;                    (CHANGE-MAP-DISK-SPACE (MFILE-MAP OVERWRITTEN-FILE)
;                                           PUT-RESERVED
;                                           PUT-FREE))
                   )
                  (T
                   (REQUIRE-DELETABLE-FILE OVERWRITTEN-FILE)
                   (SETF (MFILE-DELETED? OVERWRITTEN-FILE) T)
;                  (USING-PUT           ****
;                    (CHANGE-MAP-DISK-SPACE (MFILE-MAP OVERWRITTEN-FILE)
;                                           PUT-USED
;                                           PUT-FREE))
                   ))))
;       (WRITE-DIRECTORY-FILES DIRECTORY)
        (let ((mapped-area (moby-mapped-area-for-section file)))
          (if mapped-area (moby-writeout-area mapped-area)))
        (moby-writeout-root (partition-host-of-section file))
        ))))

(DEFUN MBFS-DELETE-FILE (PARTITION-HOST FILE &OPTIONAL (WRITE-DIRECTORY T))
  "Guarantees that FILE has its Deleted bit set."
  (with-lock ((MFILE-LOCK FILE))
    (REQUIRE-DELETABLE-FILE FILE)
    (UNLESS (MFILE-DELETED? FILE)
      (IF (MDIRECTORY? FILE)
          (IF (NULL (MFILE-FILES file))
              (SETF (MFILE-DELETED? FILE) T)
            (MLM-SIGNAL-ERROR 'FS:DIRECTORY-NOT-EMPTY))
        (SETF (MFILE-DELETED? FILE) T))
      (WHEN (MFILE-CLOSED? FILE)
;       (USING-PUT
;         (CHANGE-MAP-DISK-SPACE (MFILE-MAP FILE) PUT-USED PUT-RESERVED))
        )
;      (WHEN WRITE-DIRECTORY (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY FILE)))
      (moby-writeout-root partition-host)
      )))

(DEFUN MBFS-UNDELETE-FILE (FILE)
  "Guarantees that FILE doesn't have its Deleted bit set."
  (with-lock ((MFILE-LOCK FILE))
    (WHEN (MFILE-DELETED? FILE)
      (SETF (MFILE-DELETED? FILE) NIL)
      (WHEN (MFILE-CLOSED? FILE)
;       (USING-PUT
;         (CHANGE-MAP-DISK-SPACE (MFILE-MAP FILE) PUT-RESERVED PUT-USED))
        )
;      (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY FILE))
      (moby-writeout-root (partition-host-of-section file))
      )))


(defun maint-delete-and-expunge (PARTITION-HOST-name &optional filename &aux pathname file)
  (let ((partition-host (partition-host-of-partition-host-name partition-host-name)))
    (cond ((null filename)
           (do ()
               ((null (setq filename (prompt-and-read :string-or-nil "~&Filename to Bash>")))
                "now. dont forget to (MLM-SALVAGE)!!!!")
             (maint-delete-and-expunge filename)))
          ('else
           (setq pathname (fs:parse-pathname filename))
           (cond ((not (typep pathname 'MLM-pathname))
                  (format t "~&;Not a local pathname: ~S~%" pathname))
                 ((not (yes-or-no-p "Bash away on: ~S ?" pathname))
                  (format t "~&;OK. No action taken~%"))
                 ((not (SETQ FILE (LOOKUP-FILE PARTITION-HOST
                                               (FS:PATHNAME-RAW-DIRECTORY PATHNAME)
                                               (FS:PATHNAME-RAW-NAME PATHNAME)
                                               (FS:PATHNAME-RAW-TYPE PATHNAME)
                                               (FS:PATHNAME-RAW-VERSION PATHNAME))))
                  (format t "Lookup-file failed on: ~S ~S ~S ~S"
                          (FS:PATHNAME-RAW-DIRECTORY PATHNAME)
                          (FS:PATHNAME-RAW-NAME PATHNAME)
                          (FS:PATHNAME-RAW-TYPE PATHNAME)
                          (FS:PATHNAME-RAW-VERSION PATHNAME)))
                 ('else
                  (MBFS-delete-file partition-host file nil)
                  (MBFS-expunge-file file t)
                  "now. dont forget to (MLM-SALVAGE)!!!!"))))))


(DEFUN MBFS-EXPUNGE-FILE (FILE &optional ignore-open-count)
  "Guarantees that FILE is in the Nonexistent state.
Does NOT write out FILE's containing directory.  The caller must do that.
This helps avoid lock-up when trying to create free space when disk is full."
  (with-lock ((MFILE-LOCK FILE))
    (REQUIRE-DELETABLE-FILE FILE)
    (COND ((MFILE-CLOSED? FILE)
           (IF (or (ZEROP (MFILE-OPEN-COUNT FILE)) ignore-open-count)
               (REMOVE-FILE-FROM-DIRECTORY FILE)
             (FERROR NIL "File being expunged is still open.")))
          (T
           (INCF (MFILE-OPEN-COUNT FILE))
           (REQUIRE-ZERO-OPEN-COUNT FILE)
           (SETF (MFILE-CLOSED? FILE) T)
           (IF (MFILE-OVERWRITE-FILE FILE)
               ;; This file was overwriting another, and therefore
               ;; not actually in the directory.
               ;; Make the other file forget this one was overwriting it.
               (LET ((OTHER-FILE (MFILE-OVERWRITE-FILE FILE)))
                 (DECF (MFILE-OPEN-COUNT OTHER-FILE))
                 (SETF (MFILE-OVERWRITE-FILE OTHER-FILE) NIL))
             ;; This file was not overwriting another,
             ;; so it really is in the directory.  Remove it.
             (REMOVE-FILE-FROM-DIRECTORY FILE))))
    (moby-delete-map-dataspace (mfile-map file)
                               (partition-header-of-partition-host
                                 (partition-host-of-section file)))
;    (USING-PUT
;      (CHANGE-MAP-DISK-SPACE (MFILE-MAP FILE)
;                            (IF (MFILE-DELETED? FILE) PUT-RESERVED PUT-USED)
;                            PUT-FREE))
    (SETF (MFILE-DELETED? FILE) T)
;    (WRITE-DIRECTORY-FILES (FILE-MFILE FILE))
    (moby-writeout-root (partition-host-of-section file))
    ))

;;; Helper Functions.

(DEFUN REMOVE-FILE-FROM-DIRECTORY (FILE)
  (LET ((DIRECTORY (MFILE-DIRECTORY FILE)))
    (with-lock ((MFILE-LOCK DIRECTORY))
      (DO ((TAIL (LOCF (MFILE-FILES DIRECTORY))
                 (LOCF (CDR (CDR TAIL)))))
          ((NULL TAIL)
           (FERROR NIL "The existing file ~S is missing from its directory." FILE))
        (COND ((EQ FILE (CADR TAIL))
               (RPLACD TAIL (CDDR TAIL))
               (RETURN nil)))))))

(DEFUN REPLACE-FILE-IN-DIRECTORY (FILE NEW-FILE)
  (LET ((DIRECTORY (MFILE-DIRECTORY FILE)))
    (with-lock ((MFILE-LOCK DIRECTORY))
      (RPLACA (OR (MEMQ FILE (MFILE-FILES DIRECTORY))
                  (FERROR NIL "The existing file ~S is missing from its directory." FILE))
              NEW-FILE))))

(DEFUN CREATE-NEW-FILE (PARTITION-HOST DIRECTORY NAME TYPE VERSION)
  "Create a new file.  Sets up the default properties, and records
the author and creation-date."
  ;; Crock introduced for compatibility with old system.
  (IF (MROOT-DIRECTORY? DIRECTORY)
      (MLM-SIGNAL-ERROR 'TOP-LEVEL-FILE NIL NIL ':OPEN))
  (let* ((root-area  (ROOT-AREA-OF-PARTITION-HOST PARTITION-HOST))
         (file (MAKE-MOBY-FILE :MAKE-ARRAY (:AREA root-area)
                              DIRECTORY DIRECTORY
                              NAME (copy-to-area-if-necessary
                                     root-area
                                     NAME)
                              TYPE (copy-to-area-if-necessary
                                     root-area
                                     TYPE)
                              VERSION VERSION
                              AUTHOR-INTERNAL (copy-to-area-if-necessary
                                                root-area
                                                USER-ID)
                              CREATION-DATE-INTERNAL (TIME:GET-UNIVERSAL-TIME)
                              OPEN-COUNT 0
                              DEFAULT-BYTE-SIZE 8
                                                ;            MAP (MAP-CREATE)
                              DATA-POINTER (array-of-arrays-create PARTITION-HOST)
                              ATTRIBUTES 0
                              PLIST NIL)))
    (moby-declare-local-cell (locf (mfile-lock file)))
    file))

(DEFUN CREATE-NEW-DIRECTORY (PARTITION-HOST DIRECTORY NAME)
  "Create a new directory.  Like CREATE-NEW-FILE except that
the file's properties are setup differently."
  (LET* ((root-area (ROOT-AREA-OF-PARTITION-HOST PARTITION-HOST))
         (FILE (MAKE-MOBY-FILE
                 :MAKE-ARRAY (:AREA root-area)
                 DIRECTORY DIRECTORY
                 NAME (copy-to-area-if-necessary
                        root-area
                        NAME)
                 TYPE (copy-to-area-if-necessary
                        root-area
                        MBFS-DIRECTORY-TYPE)
                 VERSION MBFS-DIRECTORY-VERSION
                 AUTHOR-INTERNAL (copy-to-area-if-necessary
                                   root-area
                                   USER-ID)
                 CREATION-DATE-INTERNAL (TIME:GET-UNIVERSAL-TIME)
                 OPEN-COUNT 0
                 DEFAULT-BYTE-SIZE 8
;                        MAP (MAP-CREATE)
                 ATTRIBUTES 0
                 PLIST NIL)))
    (SETF (MDIRECTORY? FILE) T)
    (SETF (MFILE-CLOSED? FILE) T)
    (SETF (MFILE-DELETED? FILE) NIL)
    (moby-declare-local-cell (locf (mfile-lock file)))
    FILE))

;;;; Lookups

;;; This is the basic lookup function.
;;; OBJ is the object which you're looking up.
;;; LOC is a locative pointer into the list you're using
;;; COMP is a function which compares OBJ with an element of the list.
;;;   It should return  a positive number if OBJ > ELEM
;;;                 or  0 if OBJ = ELEM
;;;                 or  a negative number if OBJ < ELEM
;;; Values returned are ELEM and LOC.
;;; ELEM is a list element found if successful, or NIL.
;;; LOC is a locative (actually a sublist, usually) into which OBJ can be
;;;  pushed and still preserve the sorting of the list.  (It will be pushed
;;;  before ELEM, if one was found.  Note that this program does not understand
;;;  duplicates, however, and duplicates should not be entered in the list.)

(DEFUN LOOKUP (OBJ LOC COMP)
  (DECLARE (RETURN-LIST ELEM LOC))
  (DO ((LEN (LENGTH (CDR LOC)))
       (S-LOC LOC)
       S-LEN S-COM VAL)
      ((ZEROP LEN)
       (RETURN (values NIL S-LOC)))
    (SETQ S-LEN (FLOOR LEN 2)
          S-COM (NTHCDR S-LEN S-LOC)
          VAL (FUNCALL COMP OBJ (CADR S-COM)))
    (COND ((ZEROP VAL)
           (RETURN (values (CADR S-COM) S-COM)))
          ((PLUSP VAL)
           (SETQ LEN (1- (- LEN S-LEN))
                 S-LOC (CDR S-COM)))
          (T (SETQ LEN S-LEN)))))

;;;; File Lookup

;;; loc --> Previous file
;;;         Requested file
;;;         Next file

(DEFUN LOOKUP-NAMED-FILE (DIRECTORY &REST OBJ)
  (LOOKUP OBJ (LOCF (MFILE-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE))

(DEFUNP LOOKUP-FILE-COMPARE (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (MFILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (MFILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (- (CADDR LIST) (MFILE-VERSION FILE)))

;;; loc --> Previous file
;;;         File with highest non-deleted version
;;;         Next file
;;; loc is unpredictable if all versions are deleted.

(DEFVAR NEWEST-VERSION-SEEN)

(DEFUN LOOKUP-NEWEST-NON-DELETED-FILE (DIRECTORY &REST OBJ)
  (MULTIPLE-VALUE-BIND (FILE LOC OLDEST-VERSION)
      (LEXPR-FUNCALL #'LOOKUP-OLDEST-FILE DIRECTORY OBJ)
    (IF (NULL OLDEST-VERSION)
        (VALUES FILE LOC OLDEST-VERSION)
      (LET ((SAVED-LOC NIL))
        (DO-FOREVER
          (LET ((FILE (CADR LOC)))
            (COND ((NOT (STRING-EQUAL (FIRST OBJ) (MFILE-NAME FILE)))
                   (RETURN NIL))
                  ((NOT (STRING-EQUAL (SECOND OBJ) (MFILE-TYPE FILE)))
                   (RETURN NIL))
                  ((AND (NOT (MFILE-DELETED? FILE))
                        (MFILE-CLOSED? FILE))
                   (SETQ SAVED-LOC LOC)))
            (COND ((NULL (CDDR LOC))
                   (RETURN NIL))))
          (SETQ LOC (CDR LOC)))
        (IF (NULL SAVED-LOC)
            (VALUES NIL LOC NIL)
          (VALUES (CADR SAVED-LOC)
                  SAVED-LOC
                  (MFILE-VERSION (CADR SAVED-LOC))))))))

;;; loc --> Previous file
;;;         File with highest version, if any
;;;         Next file

(DEFUN LOOKUP-NEWEST-FILE (DIRECTORY &REST OBJ)
  (LET ((NEWEST-VERSION-SEEN NIL))
    (MULTIPLE-VALUE-BIND (IGNORE LOC)
        (LOOKUP OBJ (LOCF (MFILE-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE-NEWEST)
      (VALUES (AND NEWEST-VERSION-SEEN (CAR LOC))
              (IF NEWEST-VERSION-SEEN
                  (NLEFT 1 (LOCF (MFILE-FILES DIRECTORY)) LOC)
                LOC)
              NEWEST-VERSION-SEEN))))

(DEFUNP LOOKUP-FILE-COMPARE-NEWEST (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (MFILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (MFILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ NEWEST-VERSION-SEEN (MFILE-VERSION FILE))
  1)

;;; loc --> Previous file
;;;         File with lowest version
;;;         Next file

(DEFVAR OLDEST-VERSION-SEEN)

(DEFUN LOOKUP-OLDEST-NON-DELETED-FILE (DIRECTORY &REST OBJ)
  (MULTIPLE-VALUE-BIND (FILE LOC OLDEST-VERSION)
      (LEXPR-FUNCALL #'LOOKUP-OLDEST-FILE DIRECTORY OBJ)
    (IF (NULL OLDEST-VERSION)
        (VALUES FILE LOC OLDEST-VERSION)
      (DO-FOREVER
        (LET ((FILE (CADR LOC)))
          (COND ((NOT (STRING-EQUAL (FIRST OBJ) (MFILE-NAME FILE)))
                 (RETURN (values NIL LOC NIL)))
                ((NOT (STRING-EQUAL (SECOND OBJ) (MFILE-TYPE FILE)))
                 (RETURN (values NIL LOC NIL)))
                ((AND (NOT (MFILE-DELETED? FILE))
                      (MFILE-CLOSED? FILE))
                 (RETURN (values FILE LOC (MFILE-VERSION FILE))))
                ((NULL (CDDR LOC))
                 (RETURN (values NIL LOC NIL)))))
        (SETQ LOC (CDR LOC))))))

(DEFUN LOOKUP-OLDEST-FILE (DIRECTORY &REST OBJ)
  (LET ((OLDEST-VERSION-SEEN NIL))
    (MULTIPLE-VALUE-BIND (IGNORE LOC)
        (LOOKUP OBJ (LOCF (MFILE-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE-OLDEST)
      (VALUES (AND OLDEST-VERSION-SEEN (CADR LOC))
              LOC
              OLDEST-VERSION-SEEN))))

(DEFUNP LOOKUP-FILE-COMPARE-OLDEST (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (MFILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (MFILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ OLDEST-VERSION-SEEN (MFILE-VERSION FILE))
  -1)

(DEFUN FILE-LESSP (F1 F2)
  (MINUSP (FILE-COMPARE F1 F2)))

(DEFUNP FILE-COMPARE (F1 F2 &AUX TEM)
  (SETQ TEM (STRING-COMPARE (MFILE-NAME F1) (MFILE-NAME F2)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (MFILE-NAME F1) (MFILE-NAME F2)))
  (OR (ZEROP TEM) (RETURN TEM))
  (- (MFILE-VERSION F1) (MFILE-VERSION F2)))

(DEFUN LOOKUP-FILE (PARTITION-HOST DIRSPEC NAME TYPE VERSION
                    &OPTIONAL IF-DOES-NOT-EXIST IF-EXISTS (REALLY-OPEN ':DIRECTORY-OK)
                    (DELETED? T) MOBY-MAPPED OPTIONS)
  "The basic function for finding files.
If REALLY-OPEN is T, we increment the open count of the file,
and get an error if it is a directory.
If REALLY-OPEN is :DIRECTORY-OK (the default), we increment but allow directories.
If REALLY-OPEN is NIL, we do not increment the open count.
DELETED? non-NIL means deleted files can be opened."
  (%STORE-CONDITIONAL (LOCF DIRSPEC) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF NAME) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC "")
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':NEWEST)
  (LET ((DIRECTORY (IF (NAMED-STRUCTURE-P DIRSPEC) DIRSPEC
                     (LOOKUP-DIRECTORY PARTITION-HOST DIRSPEC
                      (if (getf options :create-directories)
                          'create))))
        NO-NEW-VERSION
        USE-EXISTING
        OLD-FILE)
    (BLOCK WIN
      (MLM-LOOKUP-ERROR PARTITION-HOST
        ;; Must not allow recursive locking -- see MBFS-OPEN-OUTPUT-FILE.
        (with-lock ((MFILE-LOCK DIRECTORY) :norecursive t)
          (*CATCH 'LOOKUP-FILE-ERROR
            (MULTIPLE-VALUE-BIND (FILE LOC LAST-VERSION-SEEN)
                (COND ((AND (EQ VERSION ':NEWEST) (EQ IF-EXISTS ':NEW-VERSION))
                       (MULTIPLE-VALUE-BIND (FILE LOC LAST-VERSION-SEEN)
                           (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)
                         (VALUES FILE
                                 (IF FILE (CDR LOC) LOC)
                                 LAST-VERSION-SEEN)))
                      ((MEMQ VERSION '(:NEWEST 0))
                       (IF DELETED?
                           (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)
                         (LOOKUP-NEWEST-NON-DELETED-FILE DIRECTORY NAME TYPE)))
                      ((EQ VERSION ':OLDEST)
                       (IF (OR DELETED? (EQ IF-EXISTS ':NEW-VERSION))
                           (LOOKUP-OLDEST-FILE DIRECTORY NAME TYPE)
                         (LOOKUP-OLDEST-NON-DELETED-FILE DIRECTORY NAME TYPE)))
                      ;; Depends on extra vars in MULTIPLE-VALUE-BIND
                      ;; becoming bound to NIL.
                      ((MINUSP VERSION)
                       (LET ((NEWEST (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)))
                         (IF (NULL NEWEST)
                             (*THROW 'LOOKUP-FILE-ERROR 'FILE-NOT-FOUND))
                         (LOOKUP-NAMED-FILE DIRECTORY NAME TYPE
                                            (+ (MFILE-VERSION NEWEST) VERSION))))
                      (T
                       (LOOKUP-NAMED-FILE DIRECTORY NAME TYPE VERSION)))
              (IF (AND FILE (MDIRECTORY? FILE) REALLY-OPEN
                       (NEQ REALLY-OPEN ':DIRECTORY-OK))
                  (*THROW 'LOOKUP-FILE-ERROR 'FILE-IS-SUBDIRECTORY))
              (IF (OR (NULL FILE)
                      (AND (NOT DELETED?)
                           (MFILE-DELETED? FILE)))
                  ;; File "does not exist".
                  (ECASE IF-DOES-NOT-EXIST
                    ((NIL) (RETURN-FROM WIN NIL))
                    (:ERROR
                     (*THROW 'LOOKUP-FILE-ERROR
                             (IF FILE 'OPEN-DELETED-FILE 'FILE-NOT-FOUND)))
                    (:CREATE NIL))
                ;; File "exists".  Should we use it?
                (ECASE IF-EXISTS
                  ((NIL)
                   (SETQ USE-EXISTING T))
                  (:NEW-VERSION
                   (UNLESS (MEMQ VERSION '(:NEWEST :OLDEST)) (SETQ USE-EXISTING nil)))
                  (:SUPERSEDE (SETQ NO-NEW-VERSION T) (SETQ USE-EXISTING nil))
                  ((:OVERWRITE :TRUNCATE :APPEND)
                   (UNLESS (ZEROP (MFILE-OPEN-COUNT FILE))
                     (*THROW 'LOOKUP-FILE-ERROR 'FS:FILE-LOCKED))
                   (SETQ USE-EXISTING T))
                  (:ERROR
                   (*THROW 'LOOKUP-FILE-ERROR 'FILE-ALREADY-EXISTS))
                  ((:RENAME :RENAME-AND-DELETE)
                   (SETQ OLD-FILE FILE))))
              (RETURN-FROM WIN
                (IF USE-EXISTING
                    (progn
                       (IF (and (null moby-mapped) REALLY-OPEN)
                           (IF (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))
                               (MBFS-OPEN-OVERWRITE-FILE FILE)
                             (MBFS-OPEN-INPUT-FILE FILE)))
                       FILE)
                  (VALUES
                    (MBFS-OPEN-OUTPUT-FILE PARTITION-HOST
                      DIRECTORY LOC NAME TYPE
                      (COND ((EQ VERSION ':NEWEST)
                             (IF (NULL LAST-VERSION-SEEN)
                                 1
                               (IF NO-NEW-VERSION
                                   LAST-VERSION-SEEN
                                 (1+ LAST-VERSION-SEEN))))
                            ((EQ VERSION ':OLDEST)
                             (IF (NULL LAST-VERSION-SEEN)
                                 1
                               (IF NO-NEW-VERSION
                                   LAST-VERSION-SEEN
                                 (1- LAST-VERSION-SEEN))))
                            ((NUMBERP VERSION)
                             (COND ((MINUSP VERSION)
                                    (*THROW 'LOOKUP-FILE-ERROR 'FILE-NOT-FOUND))
                                   ((NOT (< VERSION 1_16.))
                                    (*THROW 'LOOKUP-FILE-ERROR 'VERSION-TOO-LARGE))
                                   ((ZEROP VERSION)
                                    (OR LAST-VERSION-SEEN 1))
                                   (T VERSION))))
                      (UNLESS (AND (MEMQ VERSION '(:NEWEST :OLDEST))
                                   (NOT NO-NEW-VERSION))
                        FILE)
                      MOBY-MAPPED
                      OPTIONS)
                    OLD-FILE))))))
        DIRECTORY NAME TYPE VERSION))))

;;;; Directory Lookup

;locking when calling LOOKUP-DIRECTORY:
; LOOKUP-DIRECTORY does not seize any locks, however, LOOKUP-SUBDIRECTORY-STEP does.
;  It is possible to encounter subdirectories
;which are being created by other processes (the infamous directory-being-created lossage).
;No locks should be seized when calling here!  I think this is true, it should be
;verified and notated in the listing below.  (If there were any, it might set up a deadly
;embrace situation).

;To survey the problem here are the callers and locking situations on each.
; COMPLETE-PATH
; MBFS-COMPLETE-PATH
; MBFS-CREATE-DIRECTORY
; MBFS-DELETE-DIRECTORY
; MBFS-DELETE-EMPTY-DIRECTORIES
; MBFS-DIRECTORY-LIST-HEADER
; MBFS-LIST-DIRECTORIES
; MBFS-LIST-FILES
; LOOKUP-FILE
;  (MOBY-FILE-ACCESS :CHANGE-PROPERTIES, :DELETE, :DELETE-MULTIPLE-FILES, :MULTIPLE-FILE-PLISTS)
;  (MOBY-FILE-ACCESS :PROPERTIES, :RENAME)
;  MBFS-OPEN-FILE
;   (MOBY-FILE-ACCESS :OPEN)
;  MBFS-RENAME-FILE
;  LOOKUP-DIRECTORY-FILES
; MAKE-SUBDIRECTORY-ALIST (does this before getting MFILE-LOCK)
; TRY-COMPETE-DIRECTORY

(DEFUN LOOKUP-DIRECTORY (PARTITION-HOST NAME &OPTIONAL OK-IF-NOT-THERE)
  "Find a named directory and make sure that it is read in.
No locks should be held when calling here, since this can hang if it finds a subdirectory
being created.  OK-IF-NOT-THERE can be NIL, T, or CREATE."
 (with-complete-access-path    ;This catches FILE-BEING-WRITTEN-OR-SUPERSEDED
  (COND ((AND (TYPEP NAME 'MOBY-FILE)
              (MDIRECTORY? NAME))
         NAME)
        ((CONSP NAME)
         (LET ((ROOT-FILES (LOOKUP-ROOT-DIRECTORY PARTITION-HOST)))
           (IF (and (NULL ROOT-FILES)
                    (not (eq ok-if-not-there 'create)))
               (IF OK-IF-NOT-THERE NIL
                 (MLM-SIGNAL-ERROR 'FS:DIRECTORY-NOT-FOUND))
             (LOOKUP-SUBDIRECTORY PARTITION-HOST ROOT-FILES NAME OK-IF-NOT-THERE))))
        ((MEMQ NAME '(NIL :ROOT))
         (LOOKUP-ROOT-DIRECTORY PARTITION-HOST))
        ((STRINGP NAME)
         (LOOKUP-DIRECTORY PARTITION-HOST (LIST NAME) OK-IF-NOT-THERE))
        (T
         (MLM-SIGNAL-ERROR 'INVALID-DIRECTORY-NAME NAME)))))

(DEFUN LOOKUP-ROOT-DIRECTORY (partition-host)
 ;(REQUIRE-MOBY-DISK-CONFIGURATION)
 ;(READ-DIRECTORY-FILES *MOBY-ROOT-DIRECTORY*)
 ;*MOBY-ROOT-DIRECTORY*
  (find-root-directory partition-host))

(defun partition-host-name-of-section (section)
 "Returns name of topmost directory in heirarchy, which is partition-host-name"
;  (do ((f section (mfile-directory f)))
;      ((null (mfile-directory f))
;       (mfile-name f)))
 (send (partition-host-of-section section) :moby-partition-host-name)
  )

(defun partition-host-of-section (section)
  "Returns host this section stored on."
  (moby-local-partition-host section))

(DEFUN LOOKUP-SUBDIRECTORY (PARTITION-HOST NODE SUBPATH OK-IF-NOT-THERE)
  (IF (NULL (CDR SUBPATH))
      (LOOKUP-SUBDIRECTORY-STEP PARTITION-HOST
                                NODE
                                (CAR SUBPATH)
                                OK-IF-NOT-THERE)
      (LOOKUP-SUBDIRECTORY PARTITION-HOST
                           (LOOKUP-SUBDIRECTORY-STEP PARTITION-HOST
                                                     NODE
                                                     (CAR SUBPATH)
                                                     NIL)
                           (CDR SUBPATH)
                           OK-IF-NOT-THERE)))

(DEFUN LOOKUP-SUBDIRECTORY-STEP (PARTITION-HOST NODE STEP OK-IF-NOT-THERE)
  ;  (MFILE-FILES NODE)
  (with-lock ((MFILE-LOCK NODE))  ;recursive case if proceeding and creating
                                          ; after directory not found error.
    (MULTIPLE-VALUE-BIND (FILE LOC)
        (LOOKUP-NAMED-FILE NODE
                           STEP
                           MBFS-DIRECTORY-TYPE
                           MBFS-DIRECTORY-VERSION)
      (COND ((NOT (NULL FILE))
             (with-lock ((MFILE-LOCK FILE))
               (IF (NOT (MDIRECTORY? FILE))
                   (FERROR NIL "~S: expected a Directory." FILE))
               (IF (MBFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? FILE)
                   (THROW 'FILE-BEING-WRITTEN-OR-SUPERSEDED FILE)
        ;          (MLM-LOOKUP-ERROR PARTITION-HOST 'OPEN-UNFINISHED-DIRECTORY
        ;                           NODE
        ;                           STEP
        ;                           MBFS-DIRECTORY-TYPE
        ;                           MBFS-DIRECTORY-VERSION)
                 )
               (IF (MFILE-DELETED? FILE)
                   (MLM-LOOKUP-ERROR PARTITION-HOST 'OPEN-DELETED-DIRECTORY
                                    NODE
                                    STEP
                                    MBFS-DIRECTORY-TYPE
                                    MBFS-DIRECTORY-VERSION)))
             ;(MFILE-FILES FILE)        ;make sure files are read in.
             FILE)
            ((or MLM-AUTOMATICALLY-CREATE-DIRECTORIES
                 (eq ok-if-not-there 'create))
             (LET ((DIRECTORY (CREATE-NEW-DIRECTORY PARTITION-HOST NODE STEP)))
               (SI:PUSH-IN-AREA (root-area-of-partition-host partition-host)
                                DIRECTORY (CDR LOC))
               (PROCESS-UNLOCK (LOCF (MFILE-LOCK NODE)))
 ;             (WRITE-DIRECTORY-FILES NODE)
               (moby-writeout-root partition-host)
               DIRECTORY))
            ((NOT OK-IF-NOT-THERE)
             (MLM-SIGNAL-ERROR 'FS:DIRECTORY-NOT-FOUND))))))

;;;; Wildcarded Lookup

(DEFUN LOOKUP-FILES (PARTITION-HOST DIRECTORY NAME TYPE VERSION &OPTIONAL (DELETED? T))
  (%STORE-CONDITIONAL (LOCF DIRECTORY) ':UNSPECIFIC '())
  (%STORE-CONDITIONAL (LOCF NAME) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':NEWEST)
  (IF (AND (EQ NAME ':WILD) (EQ TYPE ':WILD) (EQ VERSION ':WILD))
      ;; Optimize simple case.
      (MAPCAN #'(LAMBDA (DIR)
                  (IF DELETED?
                      (COPYLIST (MFILE-FILES DIR))
                      (SUBSET #'MBFS-CLOSED-FILE? (MFILE-FILES DIR))))
              (LOOKUP-DIRECTORIES PARTITION-HOST DIRECTORY))
      (MAPCAN #'(LAMBDA (DIR) (LOOKUP-DIRECTORY-FILES PARTITION-HOST DIR NAME TYPE VERSION DELETED?))
              (LOOKUP-DIRECTORIES PARTITION-HOST DIRECTORY))))

(DEFUN LOOKUP-DIRECTORIES (PARTITION-HOST NAME)
  (LET ((ROOT-DIR (REQUIRE-MOBY-ROOT-DIRECTORY PARTITION-HOST)))
    (COND ((MEMQ NAME '(NIL :ROOT))
           (LIST ROOT-DIR))
          ((CONSP NAME)
           (LOOKUP-SUBDIRECTORIES PARTITION-HOST ROOT-DIR NAME))
          ((EQ NAME ':WILD)
           (LOOKUP-SUBDIRECTORIES PARTITION-HOST ROOT-DIR NAME))
          ((STRINGP NAME)
           (LOOKUP-SUBDIRECTORIES PARTITION-HOST ROOT-DIR (LIST NAME)))
          (T
           (MLM-SIGNAL-ERROR 'INVALID-DIRECTORY-NAME NAME)))))

(DEFUN LOOKUP-SUBDIRECTORIES (PARTITION-HOST NODE PATH)
  (MAPCAN #'(LAMBDA (FILE)
              (COND ((NOT (MDIRECTORY? FILE)) '())
                    ((EQ PATH ':WILD) (LIST FILE))
                    ((WILDCARD-MATCH (CAR PATH) (MFILE-NAME FILE))
                     (IF (NULL (CDR PATH))
                         (LIST FILE)
                         (LOOKUP-SUBDIRECTORIES PARTITION-HOST FILE (CDR PATH))))
                    (T '())))
          (MFILE-FILES NODE)))

;;; This isn't as clean as everything else because it wants to implement
;;; all the possible version tokens.  Also, it could be made faster
;;; by incorporating more knowledge of the directory structure here.

(DEFUN LOOKUP-DIRECTORY-FILES (PARTITION-HOST DIR NAME TYPE VERSION DELETED?)
  (LET ((FILES '()))
    (DOLIST (FILE (MFILE-FILES DIR))
      (IF (AND (WILDCARD-MATCH NAME (MFILE-NAME FILE))
               (WILDCARD-MATCH TYPE (MFILE-TYPE FILE))
               (OR (NOT (NUMBERP VERSION))
                   (NOT (PLUSP VERSION))
                   (= VERSION (MFILE-VERSION FILE))))
          (IF (OR DELETED? (MBFS-CLOSED-FILE? FILE))
              (PUSH FILE FILES))))
    (SETQ FILES (NREVERSE FILES))
    (IF (OR (EQ VERSION ':WILD)
            (AND (NUMBERP VERSION)
                 (> VERSION 0)))
        FILES
        (LOOP FOR FILE IN FILES BY #'NEXT-GENERIC-FILE
              AS NEW-FILE = (LOOKUP-FILE PARTITION-HOST DIR (MFILE-NAME FILE) (MFILE-TYPE FILE) VERSION
                                         NIL NIL NIL DELETED?)
              WHEN NEW-FILE COLLECT NEW-FILE))))

;;; Special Matcher for Wildcards

(DEFUN WILDCARD-MATCH (WILD-STRING STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
  (OR (EQ WILD-STRING ':WILD)
      (EQUAL WILD-STRING "*")
      (LOOP WITH WILD-LENGTH = (STRING-LENGTH WILD-STRING)
            FOR COMPARE-INDEX = START THEN (+ COMPARE-INDEX (- STAR-INDEX MATCH-INDEX))
            FOR MATCH-INDEX = 0 THEN (1+ STAR-INDEX)
            FOR STAR-INDEX = (STRING-SEARCH-CHAR #/* WILD-STRING MATCH-INDEX)
            WHEN (NULL STAR-INDEX)                      ;Rest must match
              RETURN (LET* ((WILD-LEFT  (- WILD-LENGTH MATCH-INDEX))
                            (STARTCMP (IF (ZEROP MATCH-INDEX)
                                          COMPARE-INDEX
                                          (- END WILD-LEFT))))
                       (AND (= (- END STARTCMP) WILD-LEFT)
                            (%STRING-EQUAL WILD-STRING MATCH-INDEX
                                           STRING STARTCMP
                                           (- WILD-LENGTH MATCH-INDEX))))
            ALWAYS (IF (ZEROP MATCH-INDEX)              ;No star to the left
                       (%STRING-EQUAL WILD-STRING MATCH-INDEX STRING COMPARE-INDEX
                                      (- STAR-INDEX MATCH-INDEX))
                       (LET* ((KEY (SUBSTRING WILD-STRING MATCH-INDEX STAR-INDEX))
                              (IDX (STRING-SEARCH KEY STRING COMPARE-INDEX END)))
                         (SETQ COMPARE-INDEX IDX))))))

;;;; Directory Operations

(DEFUN MBFS-CREATE-DIRECTORY (PARTITION-HOST NAME)
  "Create a directory given its NAME."
  (LOOKUP-DIRECTORY PARTITION-HOST NAME 'create))

(DEFUN MBFS-EXPUNGE-DIRECTORY (PARTITION-HOST DIRECTORY NAME TYPE VERSION)
  (%STORE-CONDITIONAL (LOCF DIRECTORY) ':UNSPECIFIC '())
  (%STORE-CONDITIONAL (LOCF NAME) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':WILD)
  (LET ((RESULTING-BLOCKS-FREED 0)
        DIRECTORY-FILE)
    (DOLIST (FILE (LOOKUP-FILES PARTITION-HOST DIRECTORY NAME TYPE VERSION ':DELETED))
      (WHEN (MFILE-DELETED? FILE)
        (SETQ DIRECTORY-FILE (MFILE-DIRECTORY FILE))
        (MBFS-EXPUNGE-FILE FILE)
        (INCF RESULTING-BLOCKS-FREED (MFILE-NPAGES FILE))))
 ;    (WRITE-DIRECTORY-FILES DIRECTORY-FILE)
    (moby-writeout-root partition-host)
    RESULTING-BLOCKS-FREED))

;; Nobody calls this, now.
(DEFUN MBFS-DELETE-DIRECTORY (PARTITION-HOST NAME &OPTIONAL (ERROR-P T))
  "Delete the single directory given by NAME."
  (IDENTIFY-FILE-OPERATION ':DELETE-DIRECTORY
    (HANDLING-ERRORS ERROR-P
      (LET ((DIRECTORY (LOOKUP-DIRECTORY PARTITION-HOST NAME)))
        (IF (NULL (MFILE-FILES DIRECTORY))
            (MBFS-DELETE-FILE PARTITION-HOST DIRECTORY)
            (MLM-SIGNAL-ERROR 'FS:DIRECTORY-NOT-EMPTY))))))

(DEFUN MBFS-DELETE-EMPTY-DIRECTORIES (PARTITION-HOST &OPTIONAL QUERY-P (NAME '()))
  "Locate all of the empty directories in the tree under the directory
node given by name, and delete them from the tree.  If NAME is not
supplied or NIL, then search the entire tree."
  (MBFS-DELETE-EMPTY-SUBDIRECTORIES PARTITION-HOST
                                    (LOOKUP-DIRECTORY PARTITION-HOST NAME) QUERY-P))

(DEFUN MBFS-DELETE-EMPTY-SUBDIRECTORIES (PARTITION-HOST DIRECTORY QUERY-P)
  (DOLIST (FILE (MFILE-FILES DIRECTORY))
    (IF (MDIRECTORY? FILE)
        (MBFS-DELETE-EMPTY-SUBDIRECTORIES PARTITION-HOST FILE QUERY-P)))
  (MBFS-DELETE-EMPTY-DIRECTORY PARTITION-HOST DIRECTORY QUERY-P))

(DEFUN MBFS-DELETE-EMPTY-DIRECTORY (PARTITION-HOST DIRECTORY QUERY-P)
  (IF (NULL (MFILE-FILES DIRECTORY))
      (IF (OR (NULL QUERY-P)
              (Y-OR-N-P (FORMAT NIL "~&Delete ~A? " (MFILE-NAME DIRECTORY))))
          (PROGN
            (FORMAT T "~&Deleting ~S ... " DIRECTORY)
            (MBFS-DELETE-FILE PARTITION-HOST DIRECTORY)
            (FORMAT T "done.")))))

;;;; File Operations

;mapped files
;  for now, an option to open, should it be a separate operation?
;  The :moby-mapped keyword specifies the file is not to be used for stream
;operations at all, but is to be moby-mapped.  This keyword can be used
;with :direction either :input or :output.
;  If a new file is to be created, the paired value of :moby-mapped
;controls the root generation of the new file.
;   if T -> the root is to be a CONS, and will be (cons nil nil) in the mapped area of the
;               new file
;   otherwise, the root is to be an array, and paired with :moby-mapped is an argument
;for MAKE-ARRAY to cons it.  THIS ARGUMENT SHOULD NOT INCLUDE A :AREA KEYWORD!

;The :moby-mapped keyword inhibits generation of any stream-like object.
;Instead, the FILE-ROOT is returnned as the value of MBFS-OPEN-FILE (thence :OPEN etc).
;  for stream-written files, the FILE-ROOT is a pointer to the ARRAY-OF-ARRAYS.
;   (the ARRAY-OF-ARRAYS is stored in the root-area, not the mapped area of the file).

;The following keywords set bits in area-moby-options  of the section map if a section
;   (ie file) is created.
;:write-once causes data in section to be freely distributed without dereconciling it locally.
;:double-mapped causes double-mapped dataspace pages to be used.  This provides additional
;  redundancy and allows "atomic" modifications.  (This will currently cause usage of
;  double-mapped space.  The "atomic" feature is nyi.)

(DEFUN MBFS-OPEN-FILE (PARTITION-HOST PATHNAME DIRECTORY NAME TYPE VERSION
                       &REST OPTIONS
                       &KEY &ALLOW-OTHER-KEYS (ERROR T) (DIRECTION :INPUT) (CHARACTERS :default)
                       (BYTE-SIZE :DEFAULT) DELETED PRESERVE-DATES INHIBIT-LINKS
                       (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
                       (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
                                            ;; :UNSPECIFIC here is to prevent lossage
                                            ;; writing ITS files with no version numbers.
                                            '(:NEWEST :UNSPECIFIC))
                                      :NEW-VERSION :SUPERSEDE)
                                  IF-EXISTS-P)
                       (IF-DOES-NOT-EXIST
                         (COND ((MEMQ DIRECTION '(:PROBE :PROBE-DIRECTORY :PROBE-LINK))
                                NIL)
                               ((AND (EQ DIRECTION :OUTPUT)
                                     (NOT (MEMQ IF-EXISTS '(:OVERWRITE :APPEND))))
                                :CREATE)
                               ;; Note: if DIRECTION is NIL, this defaults to :ERROR
                               ;; for compatibility with the past.
                               ;; A Common-Lisp program would use :PROBE
                               ;; and get NIL as the default for this.
                               (T :ERROR)))
                       MOBY-MAPPED
                       &AUX FILE INITIAL-PLIST OLD-FILE
                       PHONY-CHARACTERS SIGN-EXTEND-BYTES)
  "Implements the :OPEN message for moby-file pathnames."
  INHIBIT-LINKS
  (IDENTIFY-FILE-OPERATION-WITH-PATHNAME :OPEN PATHNAME
    (HANDLING-ERRORS ERROR
      (CASE DIRECTION
        ((:INPUT :OUTPUT :PROBE-DIRECTORY))
        (:IO (FERROR 'UNIMPLEMENTED-OPTION "Bidirectional file streams are not supported."))
        ((NIL :PROBE :PROBE-LINK) (SETQ DIRECTION ':PROBE))
        (T (FERROR 'UNIMPLEMENTED-OPTION "~S is not a valid DIRECTION argument" DIRECTION)))
      (if (eq direction :probe) (setq moby-mapped nil)) ;ignore MOBY-MAPPED on PROBE.
      (UNLESS (MEMQ IF-EXISTS '(:ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
                                :OVERWRITE :APPEND :SUPERSEDE NIL))
        (FERROR 'UNIMPLEMENTED-OPTION "~S is not a valid IF-EXISTS argument" IF-EXISTS))
      (UNLESS (MEMQ IF-DOES-NOT-EXIST
                    '(:ERROR :CREATE NIL))
        (FERROR 'UNIMPLEMENTED-OPTION
                "~S is not a valid IF-DOES-NOT-EXISTS argument" IF-EXISTS))
      (WHEN ELEMENT-TYPE-P
        (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
              (FS:DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
      (IF (OR PHONY-CHARACTERS SIGN-EXTEND-BYTES)
          (FERROR 'UNIMPLEMENTED-OPTION "~S as element-type is not implemented."
                  ELEMENT-TYPE))
      (IF (NOT (MEMQ BYTE-SIZE '(16. 8 4 2 1 :DEFAULT)))
          (MLM-SIGNAL-ERROR 'FS:INVALID-BYTE-SIZE))
      (SETF (VALUES FILE OLD-FILE)
            (LOOKUP-FILE PARTITION-HOST DIRECTORY NAME TYPE VERSION
                         (AND (NEQ DIRECTION ':PROBE-DIRECTORY) IF-DOES-NOT-EXIST)
                         (AND (EQ DIRECTION ':OUTPUT) IF-EXISTS)
                         (AND (NULL MOBY-MAPPED) (NEQ DIRECTION ':PROBE))  ;MOBY-MAPPED does
                                ;not involve streams so dont hack open counts, etc.
                         DELETED
                         MOBY-MAPPED
                         OPTIONS))
      (WHEN (IF FILE (OR (NEQ DIRECTION ':OUTPUT) IF-EXISTS)
              (OR (EQ DIRECTION ':PROBE-DIRECTORY) IF-DOES-NOT-EXIST))
        (WHEN OLD-FILE
          (SELECTQ IF-EXISTS
            (:RENAME
             (MBFS-RENAME-FILE PARTITION-HOST OLD-FILE DIRECTORY
                               (STRING-APPEND "_OLD_" NAME) TYPE ':NEWEST))
            (:RENAME-AND-DELETE
             (MBFS-RENAME-FILE PARTITION-HOST OLD-FILE DIRECTORY
                               (STRING-APPEND "_OLD_" NAME) TYPE ':NEWEST)
             (MBFS-DELETE-FILE PARTITION-HOST OLD-FILE NIL))))
        ;; Empty out the file, if supposed to.
        (WHEN (AND (EQ IF-EXISTS ':TRUNCATE) (NULL MOBY-MAPPED))
          (LET ((NBLOCKS (MOBY-A-OF-A-NBLOCKS (MFILE-DATA-POINTER FILE))))
  ;** SHOULD NOT DO RANDOMNESS IF MOBY-MAPPED.
   ;        (SETF (MOBY-A-OF-A-NBLOCKS (MFILE-DATA-POINTER FILE)) 0)
            ;; Write the directory showing the file empty.
   ;        (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY FILE))
            (moby-writeout-root partition-host)
            (SETF (MOBY-A-OF-A-NBLOCKS (MFILE-DATA-POINTER FILE)) NBLOCKS)
            ;; Then mark the blocks free.
;           (USING-PUT
;             (CHANGE-MAP-DISK-SPACE (MFILE-MAP FILE)
;                                    (IF (MFILE-DELETED? FILE) PUT-RESERVED PUT-USED)
;                                    PUT-FREE))
            (SETF (MOBY-A-OF-A-NBLOCKS (MFILE-DATA-POINTER FILE)) 0)))
        (SELECTQ DIRECTION
          ((:PROBE :INPUT)
           (IF (EQ CHARACTERS :DEFAULT)
               (SETQ CHARACTERS (MFILE-ATTRIBUTE FILE ':CHARACTERS)))
           (COND ((NULL BYTE-SIZE)
                  (SETQ BYTE-SIZE (IF CHARACTERS 8 16.)))
                 ((EQ BYTE-SIZE :DEFAULT)
                  (SETQ BYTE-SIZE (MFILE-DEFAULT-BYTE-SIZE FILE)))))
          (:OUTPUT
           (if (eq characters :default)
               (setq characters t))
           (IF (MEMQ BYTE-SIZE '(:DEFAULT NIL))
               (SETQ BYTE-SIZE (IF CHARACTERS 8 16.)))
           (SETF (MFILE-DEFAULT-BYTE-SIZE FILE) BYTE-SIZE)
           (SETF (MFILE-ATTRIBUTE FILE ':CHARACTERS) CHARACTERS)
           (UNLESS PRESERVE-DATES
             (SETF (MFILE-CREATION-DATE-INTERNAL FILE)
                   (TIME:GET-UNIVERSAL-TIME)))
           (MBFS-CHANGE-FILE-PROPERTIES PARTITION-HOST FILE INITIAL-PLIST)))
        (COND (MOBY-MAPPED
               (MFILE-DATA-POINTER FILE))  ;No stream frob if MOBY-MAPPED.
              ((EQ DIRECTION ':PROBE-DIRECTORY)
               (MAKE-INSTANCE 'MOBY-LM-PROBE-STREAM
                              :TRUENAME (SEND PATHNAME :NEW-PATHNAME
                                               :NAME NIL :TYPE NIL :VERSION NIL)
                              :PATHNAME PATHNAME
                              :partition-host partition-host))
              (T
               (MAKE-INSTANCE
                 (SELECTQ DIRECTION
                   (:INPUT (IF CHARACTERS 'MOBY-LM-CHARACTER-INPUT-STREAM 'MOBY-LM-INPUT-STREAM))
                   (:OUTPUT
                    (IF CHARACTERS 'MOBY-LM-CHARACTER-OUTPUT-STREAM 'MOBY-LM-OUTPUT-STREAM))
                   ((:PROBE :PROBE-DIRECTORY) 'MOBY-LM-PROBE-STREAM))
                 :FILE FILE
                 :APPEND (EQ IF-EXISTS ':APPEND)
                 :PATHNAME PATHNAME
                 :BYTE-SIZE BYTE-SIZE
                 :partition-host partition-host)))))))

(DEFUN MBFS-RENAME-FILE (PARTITION-HOST FILE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION)
  (IF (EQ NEW-DIRECTORY ':UNSPECIFIC) (SETQ NEW-DIRECTORY '()))
  (IF (MEMQ NEW-NAME '(NIL :UNSPECIFIC)) (SETQ NEW-NAME ""))
  (IF (MEMQ NEW-TYPE '(NIL :UNSPECIFIC)) (SETQ NEW-TYPE ""))
  (IF (MEMQ NEW-VERSION '(NIL :UNSPECIFIC)) (SETQ NEW-VERSION ':NEWEST))
  (LET ((NEW-FILE (LOOKUP-FILE PARTITION-HOST NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION
                               ':CREATE ':NEW-VERSION NIL NIL))
        (DIRECTORY (MFILE-DIRECTORY FILE)))
    (SETQ NEW-VERSION (MFILE-VERSION NEW-FILE))
    (UNWIND-PROTECT
      (PROGN
        (IF (AND (MDIRECTORY? FILE)
                 (NOT (AND (EQUAL NEW-TYPE MBFS-DIRECTORY-TYPE)
                           (EQUAL NEW-VERSION MBFS-DIRECTORY-VERSION))))
            (MLM-RENAME-ERROR 'RENAME-DIRECTORY-INVALID-TYPE
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (IF (NOT (EQ (MFILE-DIRECTORY NEW-FILE) DIRECTORY))
            (MLM-RENAME-ERROR 'FS:RENAME-ACROSS-DIRECTORIES
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (IF (NOT (NULL (MFILE-OVERWRITE-FILE NEW-FILE)))
            (MLM-RENAME-ERROR 'FS:RENAME-TO-EXISTING-FILE
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (with-lock ((MFILE-LOCK FILE))
          (WHEN (MFILE-OVERWRITE-FILE FILE)
            ;; Old file was being superseded.
            ;; That is no longer so, though the output file is still there.
            (LET ((OUTFILE (MFILE-OVERWRITE-FILE FILE)))
              (REPLACE-FILE-IN-DIRECTORY FILE OUTFILE)
              (SETF (MFILE-OVERWRITE-FILE FILE) NIL)
              (SETF (MFILE-OVERWRITE-FILE OUTFILE) NIL)
              (DECF (MFILE-OPEN-COUNT FILE))))
          (ALTER-MOBY-FILE FILE
                           NAME NEW-NAME
                           TYPE NEW-TYPE
                           VERSION NEW-VERSION)
          (with-lock ((MFILE-LOCK DIRECTORY))
            (SETF (MFILE-FILES DIRECTORY) (DELQ FILE (MFILE-FILES DIRECTORY)))
            (RPLACA (MEMQ NEW-FILE (MFILE-FILES DIRECTORY))
                    FILE))
 ;        (WRITE-DIRECTORY-FILES DIRECTORY)
          (moby-writeout-root partition-host)
          ;; Fake out the MBFS-EXPUNGE-FILE
          ;; to not get an error due to NEW-FILE not being in the directory.
          (SETF (MFILE-OVERWRITE-FILE NEW-FILE) FILE)
          (INCF (MFILE-OPEN-COUNT FILE))))
      (MBFS-EXPUNGE-FILE NEW-FILE)
 ;    (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY NEW-FILE))
      (moby-writeout-root partition-host)
      ))
  (FILE-TRUENAME FILE))

(DEFPROP MLM-RENAME-ERROR T :ERROR-REPORTER)
(DEFUN MLM-RENAME-ERROR (SIGNAL-NAME NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION)
  (MLM-SIGNAL-ERROR SIGNAL-NAME NIL NIL
                   (MAKE-PATHNAME ':HOST "MLM" ':DEVICE "DSK"
                                  ':DIRECTORY
                                  (COND ((STRINGP NEW-DIRECTORY) NEW-DIRECTORY)
                                        ((LISTP NEW-DIRECTORY) NEW-DIRECTORY)
                                        (T
                                         (MFILE-NAME NEW-DIRECTORY)))
                                  ':NAME NEW-NAME ':TYPE NEW-TYPE
                                  ':VERSION NEW-VERSION)))

;;;; Property Lists

(DEFUN MBFS-DIRECTORY-LIST (PARTITION-HOST PATHNAME HOST DIRECTORY NAME TYPE VERSION OPTIONS
                            &AUX FILES RESULT)
  "Implements the :DIRECTORY-LIST message for pathnames."
  (LET ((ERROR-P (NOT (MEMQ ':NOERROR OPTIONS)))
        (FASTP (MEMQ ':FAST OPTIONS))
        (DELETED? (MEMQ ':DELETED OPTIONS))
        (DIRECTORIES-ONLY (MEMQ ':DIRECTORIES-ONLY OPTIONS))
        (OLD-WHOSTATE (SI:PROCESS-WAIT-WHOSTATE CURRENT-PROCESS)))
    (SETF (SI:PROCESS-WAIT-WHOSTATE CURRENT-PROCESS) "Directory")
    (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)
    (UNWIND-PROTECT
      (HANDLING-ERRORS ERROR-P
        (IDENTIFY-FILE-OPERATION ':DIRECTORY-LIST
          (CONS (IF (OR FASTP DIRECTORIES-ONLY)
                    (LIST NIL)
                    (MBFS-DIRECTORY-LIST-HEADER PARTITION-HOST PATHNAME))
                (IF DIRECTORIES-ONLY
                    (MBFS-ALL-DIRECTORIES PARTITION-HOST HOST ERROR-P)
                    (SETQ FILES (LOOKUP-FILES PARTITION-HOST DIRECTORY NAME TYPE VERSION DELETED?))
                    (SETQ RESULT (MAKE-LIST (LENGTH FILES)))
                    (DO ((F FILES (CDR F))
                         (R RESULT (CDR R)))
                        ((NULL F) RESULT)
                      (SETF (CAR R)
                            (IF FASTP
                                (LIST (FILE-TRUENAME (CAR F)))
                                (CONS (FILE-TRUENAME (CAR F))
                                      (MBFS-FILE-PROPERTIES (CAR F))))))))))
      (PROGN (SETF (PROCESS-WHOSTATE CURRENT-PROCESS) OLD-WHOSTATE)
             (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)))))

(DEFUN MBFS-ALL-DIRECTORIES (PARTITION-HOST HOST ERROR-P)
  (IDENTIFY-FILE-OPERATION ':ALL-DIRECTORIES
    (HANDLING-ERRORS ERROR-P
      (MAPCAR #'(LAMBDA (DIRECTORY)
                  (LIST (MAKE-PATHNAME
                          ':HOST HOST ':DEVICE "DSK"
                          ':DIRECTORY (MFILE-NAME DIRECTORY)
                          ':NAME ':UNSPECIFIC ':TYPE ':UNSPECIFIC
                          ':VERSION ':UNSPECIFIC)))
              (TOP-LEVEL-DIRECTORIES PARTITION-HOST)))))

(DEFVAR MLM-UNSETTABLE-PROPERTIES
        '(:LENGTH-IN-BLOCKS :LENGTH-IN-BYTES)
  "Unsettable properties are those which are uniquely determined by the text of the file.")

(DEFVAR MLM-DEFAULT-SETTABLE-PROPERTIES
        '(:AUTHOR :BYTE-SIZE :CREATION-DATE :DELETED :CHARACTERS
          :DONT-DELETE :DONT-REAP :NOT-BACKED-UP))

(DEFUN MBFS-FILE-PROPERTIES (FILE &AUX DBS)
  "Given a file, return a plist like a stream would want."
  (LET ((PLIST (COPYLIST (MFILE-PLIST FILE))))
    (DOLIST (PROP '(:DONT-DELETE :DELETED :DONT-REAP :CHARACTERS :DIRECTORY :MOBY-MAPPED))
      (IF (MFILE-ATTRIBUTE FILE PROP)
          (SETQ PLIST (LIST* PROP T PLIST))))
    (IF (AND (NOT (MFILE-ATTRIBUTE FILE :CLOSED))
             (NOT (MEMQ :DELETED PLIST)))
        (SETQ PLIST (LIST* :DELETED T PLIST)))
    (IF (NOT (MFILE-ATTRIBUTE FILE :DUMPED))
        (SETQ PLIST (LIST* :NOT-BACKED-UP T PLIST)))    ;Backasswardsness.
    (LIST* :BYTE-SIZE (SETQ DBS (OR (MFILE-DEFAULT-BYTE-SIZE FILE) 8))
           :LENGTH-IN-BLOCKS (MFILE-NPAGES FILE)
           :LENGTH-IN-BYTES (FLOOR (FILE-DATA-LENGTH FILE) DBS)
           :AUTHOR (MFILE-AUTHOR-INTERNAL FILE)
           :CREATION-DATE (MFILE-CREATION-DATE-INTERNAL FILE)
           PLIST)))

(DEFUN MBFS-CHANGE-FILE-PROPERTIES (PARTITION-HOST FILE PLIST)
  (when plist           ;avoid writing anything if not necessary.
    (with-lock ((MFILE-LOCK FILE))
      (DO ((P PLIST (CDDR P)))
          ((NULL P)
;        (IF (MFILE-CLOSED? FILE)
;            (WRITE-DIRECTORY-FILES (MFILE-DIRECTORY FILE)))
           (if (mfile-closed? file)
               (moby-writeout-root partition-host)))
        (SELECTQ (CAR P)
          (:DELETED
           (IF (NULL (CADR P))
               (MBFS-UNDELETE-FILE FILE)
             (MBFS-DELETE-FILE PARTITION-HOST FILE)))
          ((:DONT-DELETE :DONT-REAP :CHARACTERS)
           (SETF (MFILE-ATTRIBUTE FILE (CAR P)) (CADR P)))
          (:NOT-BACKED-UP
           (SETF (MFILE-ATTRIBUTE FILE :DUMPED) (NOT (CADR P))))
          (:BYTE-SIZE
           (SETF (MFILE-DEFAULT-BYTE-SIZE FILE) (CADR P)))
          (:AUTHOR
           (SETF (MFILE-AUTHOR-INTERNAL FILE)
                 (copy-to-area-if-necessary
                   (root-area-of-partition-host partition-host)
                   (CADR P))))
          (:CREATION-DATE
           (SETF (MFILE-CREATION-DATE-INTERNAL FILE) (CADR P)))
          (OTHERWISE
           (COND ((MEMQ (CAR P) MLM-UNSETTABLE-PROPERTIES)
                  (MLM-SIGNAL-ERROR 'UNSETTABLE-PROPERTY NIL NIL (CAR P)))
                 ((NOT (SYMBOLP (CAR P)))
                  (MLM-SIGNAL-ERROR 'FS:INVALID-PROPERTY-NAME NIL NIL (CAR P)))
                 ((GET (CAR P) 'MOBY-ATTRIBUTE)
                  (FERROR NIL "CHANGE-FILE-PROPERTIES hasn't been updated to match the defined attributes."))
                 (T (PUTPROP (LOCF (MFILE-PLIST FILE)) (CADR P) (CAR P))))))))))

(DEFUN MBFS-DIRECTORY-LIST-HEADER (PARTITION-HOST PATHNAME)
  `(NIL :DISK-SPACE-DESCRIPTION
        ,(MBFS-DISK-SPACE-DESCRIPTION PARTITION-HOST
                                      (LOOKUP-DIRECTORY PARTITION-HOST
                                                        (PATHNAME-DIRECTORY PATHNAME)))
        :SETTABLE-PROPERTIES ,MLM-DEFAULT-SETTABLE-PROPERTIES
        :PATHNAME ,PATHNAME))

(DEFUN MBFS-DISK-SPACE-DESCRIPTION (PARTITION-HOST &OPTIONAL DIRECTORY
                                    &AUX (BASE 10.) (*NOPOINT T))
  (WITH-OUTPUT-TO-STRING (STREAM)
    (funcall stream :string-out "Dataspace: ")
    (labels ((hack-uoa (uoa)
                       (let* ((l-u-of-a (array-length uoa))
                              (ua-unusable (1- l-u-of-a))
                              (ua-root (1- ua-unusable))
                              (ua-reserved (1- ua-root))
                              (ua-sticky (1- ua-reserved))
                              (comma nil))
                         (dotimes (c (array-length uoa))
                           (cond ((zerop (aref uoa c))
                                  (go x)))
                           (if comma (funcall stream :string-out ", "))
                           (cond ((zerop c)
                                  (funcall stream :string-out "Free="))
                                 ((= c ua-sticky)
                                  (funcall stream :string-out "Sticky="))
                                 ((= c ua-reserved)
                                  (funcall stream :string-out "Reserved="))
                                 ((= c ua-root)
                                  (funcall stream :string-out "Root="))
                                 ((= c ua-unusable)
                                  (funcall stream :string-out "Unusable="))
                                 (t (funcall stream :string-out "U")
                                    (si:print-fixnum c stream)
                                    (funcall stream :string-out "=")))
                           (si:print-fixnum (aref uoa c) stream)
                           (setq comma t)
                           x))))
      (funcall stream :string-out "2M: ")
      (hack-uoa (moby-mpa-double-mapped-dataspace-usage-occurance-counts partition-host))
      (funcall stream :string-out ", 1M: ")
      (hack-uoa (moby-mpa-single-mapped-dataspace-usage-occurance-counts partition-host))
      )))

;;;; Completion

(DEFUN MBFS-COMPLETE-PATH (PARTITION-HOST DIR NAME TYPE DEFAULT-NAME DEFAULT-TYPE OPTIONS)
  "Implements the :COMPLETE-STRING message for pathnames."
  (%STORE-CONDITIONAL (LOCF DIR) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF NAME) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC "")
  (MULTIPLE-VALUE-BIND (NEW-DIR NEW-NAME NEW-TYPE NIL COMPLETION)
      (COMPLETE-PATH PARTITION-HOST DIR NAME TYPE)
    (AND (EQUAL NEW-NAME "")
         (COND ((MEMQ ':WRITE OPTIONS)
                (SETQ NEW-NAME DEFAULT-NAME)
                (SETQ COMPLETION ':NEW))
               (T (SETQ NEW-NAME NIL))))
    (AND (EQUAL NEW-TYPE "")
         (COND ((MEMQ ':WRITE OPTIONS)
                (SETQ NEW-TYPE DEFAULT-TYPE)
                (SETQ COMPLETION ':NEW))
               ((AND (LOOKUP-DIRECTORY PARTITION-HOST NEW-DIR T)
                     (LOOKUP-FILE PARTITION-HOST NEW-DIR NEW-NAME DEFAULT-TYPE ':NEWEST NIL NIL NIL NIL))
                (SETQ NEW-TYPE DEFAULT-TYPE)
                (SETQ COMPLETION ':OLD))
               (T (SETQ NEW-TYPE NIL))))
    (VALUES NEW-DIR NEW-NAME NEW-TYPE COMPLETION)))

;;; Given a list of directory components, complete them all
;;; and return a list of completed directory components.
;;; Second value is non-nil if we added anything to what we were given.

(DEFUN TRY-COMPLETE-DIRECTORY (PARTITION-HOST DIRECTORY &AUX COMPLETION-SO-FAR)
  (DO ((DIRLEFT DIRECTORY (CDR DIRLEFT)))
      ((NULL DIRLEFT) (VALUES COMPLETION-SO-FAR T))
    (LET* ((DIR-COMPONENT (CAR DIRLEFT))
           (COMPLETED-DIRECTORY
             (OR (LOOKUP-DIRECTORY PARTITION-HOST (APPEND COMPLETION-SO-FAR (LIST DIR-COMPONENT)) T)
                 (MULTIPLE-VALUE-BIND (TEM NIL DIRECTORY-COMPLETED)
                     (ZWEI:COMPLETE-STRING DIR-COMPONENT
                                           (IF COMPLETION-SO-FAR
                                               (MAKE-SUBDIRECTORY-ALIST PARTITION-HOST COMPLETION-SO-FAR)
                                             (MAKE-DIRECTORY-ALIST PARTITION-HOST))
                                           '(#/-) T)
                   (AND DIRECTORY-COMPLETED
                        (LOOKUP-DIRECTORY PARTITION-HOST (APPEND COMPLETION-SO-FAR (LIST TEM))))))))
      (IF COMPLETED-DIRECTORY
          (SETQ COMPLETION-SO-FAR
                (APPEND COMPLETION-SO-FAR (LIST (MFILE-NAME COMPLETED-DIRECTORY))))
        (RETURN
          (values (APPEND COMPLETION-SO-FAR DIRLEFT)
                  (NOT (EQUAL (APPEND COMPLETION-SO-FAR DIRLEFT) DIRECTORY))))))))

;;; Although directories can be completed, in practice it is not intuitive to do so.
;;; Therefore, completion barfs if the directory cannot complete to *something* unique.

(DEFUNP COMPLETE-PATH (PARTITION-HOST DIRECTORY NAME TYPE &AUX TEM
                       COMPLETED-DIRECTORY COMPLETED-NAME COMPLETED-TYPE
                       NAME-COMPLETIONS TYPE-COMPLETIONS
                       DIRECTORY-COMPLETED NAME-COMPLETED TYPE-COMPLETED)
  (DECLARE (RETURN-LIST DIRECTORY NAME TYPE GENERIC-COMPLETIONS MODE))
  (COND ((CONSP DIRECTORY)
         (MULTIPLE-VALUE (TEM DIRECTORY-COMPLETED)
           (TRY-COMPLETE-DIRECTORY PARTITION-HOST DIRECTORY))
         (OR (SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY PARTITION-HOST TEM T))
             (RETURN (values TEM NAME TYPE NIL (AND DIRECTORY-COMPLETED ':NEW)))))
        ((SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY PARTITION-HOST DIRECTORY T)))
        ((PROGN
           (MULTIPLE-VALUE (TEM NIL DIRECTORY-COMPLETED)
             (ZWEI:COMPLETE-STRING DIRECTORY (MAKE-DIRECTORY-ALIST PARTITION-HOST) '(#/-) T))
           (SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY PARTITION-HOST TEM T))))
        (T (RETURN (values TEM NAME TYPE NIL (AND DIRECTORY-COMPLETED ':NEW)))))
  (MULTIPLE-VALUE (COMPLETED-NAME NAME-COMPLETIONS NAME-COMPLETED)
    (ZWEI:COMPLETE-STRING NAME (MAKE-FILE-NAME-ALIST COMPLETED-DIRECTORY) '(#/-)))
  (IF (AND NAME-COMPLETIONS
           (EQUAL COMPLETED-NAME (CAAR NAME-COMPLETIONS))
           (EQUAL COMPLETED-NAME (CAAR (LAST NAME-COMPLETIONS))))
      ;; Name determined uniquely: try to complete the type.
      (MULTIPLE-VALUE (COMPLETED-TYPE TYPE-COMPLETIONS TYPE-COMPLETED)
        (ZWEI:COMPLETE-STRING TYPE (MAKE-FILE-TYPE-ALIST NAME-COMPLETIONS) '(#/-)))
    (SETQ COMPLETED-TYPE TYPE))
  (VALUES (MFILE-FULL-NAME COMPLETED-DIRECTORY) COMPLETED-NAME COMPLETED-TYPE
          (MAPCAR #'CDR TYPE-COMPLETIONS)
          (AND (OR DIRECTORY-COMPLETED NAME-COMPLETED TYPE-COMPLETED)
               (IF (AND (ASSOC COMPLETED-NAME NAME-COMPLETIONS)
                        (ASSOC COMPLETED-TYPE TYPE-COMPLETIONS))
                   ':OLD ':NEW))))

;;; The code here deals only with strings.
;;; The pathname code is responsible for interpreting it correctly

(DEFUN MAKE-DIRECTORY-ALIST (PARTITION-HOST)
  (LET* ((TOP-DIRECTORIES (TOP-LEVEL-DIRECTORIES PARTITION-HOST))
         (LENGTH (LENGTH TOP-DIRECTORIES))
         (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST)))
    (LOOP FOR D IN TOP-DIRECTORIES
          FOR I FROM 0
          DOING (ASET (CONS (MFILE-NAME D) D) ARRAY I))
    ARRAY))

(DEFUN MAKE-SUBDIRECTORY-ALIST (PARTITION-HOST DIRECTORY)
  (SETQ DIRECTORY (LOOKUP-DIRECTORY PARTITION-HOST DIRECTORY))
  (with-lock ((MFILE-LOCK DIRECTORY))
    (LET* ((LENGTH (LENGTH (MFILE-FILES DIRECTORY)))
           (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST ':LEADER-LENGTH 1)))
      (STORE-ARRAY-LEADER 0 ARRAY 0)
      (LOOP FOR F IN (MFILE-FILES DIRECTORY) BY #'NEXT-GENERIC-FILE
            WHEN (MFILE-ATTRIBUTE F ':DIRECTORY)
            DOING (ARRAY-PUSH ARRAY (CONS (MFILE-NAME F) F)))
      ARRAY)))

(DEFUN MAKE-FILE-NAME-ALIST (DIRECTORY)
  (with-lock ((MFILE-LOCK DIRECTORY))
    (LET* ((LENGTH (LENGTH (MFILE-FILES DIRECTORY)))
           (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST ':LEADER-LENGTH 1)))
      (STORE-ARRAY-LEADER 0 ARRAY 0)
      (LOOP FOR F IN (MFILE-FILES DIRECTORY) BY #'NEXT-GENERIC-FILE
            DOING (ARRAY-PUSH ARRAY (CONS (MFILE-NAME F) F)))
      ARRAY)))

(DEFUN MAKE-FILE-TYPE-ALIST (FILE-NAME-ALIST)
  (LOOP FOR ELEM IN FILE-NAME-ALIST
        COLLECTING (CONS (MFILE-TYPE (CDR ELEM)) (CDR ELEM))))

(DEFUN NEXT-GENERIC-FILE (LIST)
  (LET ((NAME (MFILE-NAME (CAR LIST)))
        (TYPE (MFILE-TYPE (CAR LIST))))
    (DO ((L (CDR LIST) (CDR L)))
        ((NULL L))
      (IF (NOT (AND (STRING-EQUAL NAME (MFILE-NAME (CAR L)))
                    (STRING-EQUAL TYPE (MFILE-TYPE (CAR L)))))
          (RETURN L)))))

;;;; Debugging Code
;;; This deserves a little reorganization...

;;; "Peek"

(DEFUN DEBUG (PARTITION-HOST-NAME)
  (let ((partition-host (partition-host-of-partition-host-name partition-host-name)))
    (REQUIRE-MOBY-ROOT-DIRECTORY PARTITION-HOST)
;  (FORMAT T "~&Disk Configuration, version ~D~%~%"
;         (MDC-VERSION))
;  (FORMAT T "Lock: ~S~% Psize: ~S~% PUT-Base: ~S~% PUT-Size: ~S~%~%"
;         MOBY-DISK-CONFIGURATION-LOCK
;         (MDC-PARTITION-SIZE)
;         (MDC-PUT-BASE)
;         (MDC-PUT-SIZE))
;  (FORMAT T "The PUT is ~:[unlocked~;locked by ~0G~S~] and is ~:[in~;~]consistent."
;         PUT-LOCK
;         (= (AREF PAGE-USAGE-TABLE 0) PUT-CONSISTENT))
;  (DBG-ROOM)
  (IF (Y-OR-N-P "Would you like to examine the directory structure? ")
      (DBG-EDIT PARTITION-HOST))
;  (IF (Y-OR-N-P "Would you like to list the PUT? ")
;      (DBG-LIST-PUT))
  ))

(DEFUN DBG-ROOM ()
;  (FORMAT T "~&Free: ~D~%Reserved: ~D~%Used: ~D~%Unusable: ~D~%"
;         (AREF PUT-USAGE-ARRAY 0)
;         (AREF PUT-USAGE-ARRAY 1)
;         (AREF PUT-USAGE-ARRAY 2)
;         (AREF PUT-USAGE-ARRAY 3))
  )

;(DEFUN DBG-LIST-PUT (&AUX SALV)
;  (AND SALVAGER-TABLE
;       (PROGN (FORMAT T "~&Look at salvager table too? ")
;             (Y-OR-N-P))
;       (SETQ SALV T))
;  (FORMAT T "~&Currently scanning at ~S~%" PUT-SCANNING-INDEX)
;  (DO ((I 1 (1+ I))
;       (BLOCK-START 1)
;       (CONTENTS (AREF PAGE-USAGE-TABLE 1))
;       (SALV-CONTENTS (AND SALV (AREF SALVAGER-TABLE 1))))
;      (NIL)
;    (COND ((OR ( I VMEM-PARTITION-SIZE)
;              ( (AREF PAGE-USAGE-TABLE I) CONTENTS)
;              (AND SALV (NEQ (AREF SALVAGER-TABLE I) SALV-CONTENTS)))
;          (IF (= BLOCK-START (1- I))
;              (PRINC BLOCK-START)
;            (FORMAT T "~S-~S" BLOCK-START (1- I)))
;          (FORMAT T ": ~19T~[Unused~;Reserved~;Used~;Unusable~]" CONTENTS)
;          (AND SALV SALV-CONTENTS
;               (FORMAT T "~30T(~A)"
;                       (COND ((TYPEP SALV-CONTENTS 'DIRECTORY)
;                              (FORMAT NIL "Directory ~A" (MFILE-NAME SALV-CONTENTS)))
;                             ((TYPEP SALV-CONTENTS 'MOBY-FILE)
;                              (IF (MDIRECTORY? SALV-CONTENTS)
;                                  SALV-CONTENTS
;                                  (FILE-TRUENAME SALV-CONTENTS)))
;                             (T SALV-CONTENTS))))
;          (TERPRI)
;          (AND ( I VMEM-PARTITION-SIZE) (RETURN))
;          (SETQ CONTENTS (AREF PAGE-USAGE-TABLE I) BLOCK-START I)
;          (AND SALV (SETQ SALV-CONTENTS (AREF SALVAGER-TABLE I)))))))

(DEFUN DBG-EDIT (PARTITION-HOST)
  (LET ((DIRECTORY NIL)
        (FILES NIL)
        (NFILES NIL)
        (NFILE NIL)
        (CURRENT-FILE NIL)
        (debug-verbose-p nil))
    (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
      (DBG-EXAMINE-SELECT-DIRECTORY (LOOKUP-ROOT-DIRECTORY-FILES PARTITION-HOST) NIL))
    (DO-FOREVER
      (FORMAT T "~%~%~S~:[~;[Locked by ~1G~S]~]:" DIRECTORY (MFILE-LOCK DIRECTORY))
      (IF (NULL NFILE)
          (FORMAT T "No files.")
          (PROGN
            (SETQ CURRENT-FILE (NTH NFILE FILES))
            (FORMAT T "File #~D of ~D file~:[s~]." (1+ NFILE) NFILES (= NFILES 1))
            (if debug-verbose-p
                (DBG-EXAMINE-SHOW-FILE CURRENT-FILE)
              (progn (format t "~& ~S " current-file)
                     (print-brief-attribute-description-in-brackets current-file)))))
      (FORMAT T "~%Command: ")
      (SELECTQ (TYI)
        ((#/P #/p)
         (IF (MROOT-DIRECTORY? DIRECTORY)
             (FORMAT T "~%Already at root directory.")
             (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
               (DBG-EXAMINE-SELECT-DIRECTORY (MFILE-DIRECTORY DIRECTORY) DIRECTORY))))
        ((#/N #/n)
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (DBG-REQUIRE-DIRECTORY CURRENT-FILE)
              (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
                (DBG-EXAMINE-SELECT-DIRECTORY CURRENT-FILE NIL))))
        ((#/F #/f)
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (IF (= (SETQ NFILE (1+ NFILE)) NFILES)
                  (SETQ NFILE 0))))
        ((#/B #/b)
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETQ NFILE
                    (1- (IF (= NFILE 0) NFILES NFILE)))))
        ((#/C #/c)
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETF (MFILE-CLOSED? CURRENT-FILE)
                    (NOT (MFILE-CLOSED? CURRENT-FILE)))))
        ((#/D #/d)
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETF (MFILE-DELETED? CURRENT-FILE)
                    (NOT (MFILE-DELETED? CURRENT-FILE)))))
;       ((#/M #/m)
;        (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
;             (LET ((NEW-STATUS (DBG-NEW-STATUS-QUERY)))
;               (USING-PUT
;                 (SET-MAP-DISK-SPACE (MFILE-MAP CURRENT-FILE) NEW-STATUS)))))
;       ((#/W #/w)
;        (WRITE-DIRECTORY-FILES DIRECTORY))
        (#/+
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (INCF (MFILE-OPEN-COUNT CURRENT-FILE))))
        (#/-
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (DECF (MFILE-OPEN-COUNT CURRENT-FILE))))
        ((#/Q #/q)
         (RETURN T))
        ((#/V #/v)
         (setq debug-verbose-p (not debug-verbose-p)))
        ((#/? #\HELP)
         (FORMAT T "~%Available commands:~
~%P = up to the parent directory.~
~%N = down to the currently selected subdirectory.~
~%F = forward one file.~
~%B = backward one file.~
~%C = toggle the Closed bit.~
~%D = toggle the Deleted bit.~
~%M = modify the disk-space map.~
~%+ = increment the Open count.~
~%- = decrement the Open count.~
~%W = write out the current directory.~
~%V = toggle verbose show file mode.~
~%Q = quit.~
~%? = this stuff."))
        (OTHERWISE
         (FORMAT T "~%Invalid."))))))

(DEFUN DBG-REQUIRE-CURRENT-FILE (CURRENT-FILE)
  (OR (NOT (NULL CURRENT-FILE))
      (PROGN (FORMAT T "~%No current file.") NIL)))

(DEFUN DBG-REQUIRE-DIRECTORY (FILE)
  (OR (MDIRECTORY? FILE)
      (PROGN (FORMAT T "~%Current file not a directory.") NIL)))

(DEFUN DBG-EXAMINE-SELECT-DIRECTORY (DIRECTORY CURRENT-FILE-OR-NIL)
;  (IF (AND (EQ (MFILE-FILES DIRECTORY) ':DISK)
;          (Y-OR-N-P "Directory not in core; read it in? "))
;      (MFILE-FILES DIRECTORY))
  (LET ((FILES (MFILE-FILES DIRECTORY)))
;    (IF (EQ FILES ':DISK) (SETQ FILES '()))
    (LET ((NFILES (LENGTH FILES))
          (NFILE (IF (NULL CURRENT-FILE-OR-NIL)
                     (AND FILES 0)
                     (FIND-POSITION-IN-LIST CURRENT-FILE-OR-NIL FILES))))
      (VALUES DIRECTORY FILES NFILES NFILE))))

(DEFUN DBG-NEW-STATUS-QUERY ()
;  (FQUERY `(:CHOICES (((,PUT-USED "Used.") #/U #/u)
;                     ((,PUT-RESERVED "Reserved.") #/R #/r)
;                     ((,PUT-FREE "Free.") #/F #/f)
;                     ((,PUT-UNUSABLE "Bad.") #/B #/b)))
;         "~%Select the new status: ")
  )

(DEFUN DBG-EXAMINE-SHOW-FILE (CURRENT-FILE &OPTIONAL (DEBUG-P T)
                              &AUX FILE
                              NAMESTRING SECONDS MINUTES HOURS DAY MONTH YEAR ARRAY-OF-ARRAYS TEM)
  (UNLESS (NULL CURRENT-FILE)
    (SETQ FILE CURRENT-FILE)
    (SETQ NAMESTRING
          (MLM-NAMESTRING-FOR-DIRED (MFILE-NAME FILE) (MFILE-TYPE FILE) (MFILE-VERSION FILE)))
    (MULTIPLE-VALUE (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:DECODE-UNIVERSAL-TIME (MFILE-CREATION-DATE-INTERNAL FILE)))
    (FORMAT T "~%~24A ~3D ~6D(~2D) ~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
            NAMESTRING
  ;*** RANDOMNESS IN CASE OF MOBY-MAPPED.
            (ARRAY-OF-ARRAYS-NPAGES (SETQ ARRAY-OF-ARRAYS (MFILE-DATA-POINTER FILE)))
            (OR (AND (SETQ TEM (ARRAY-OF-ARRAYS-LENGTH ARRAY-OF-ARRAYS))
                     (FLOOR TEM (MFILE-DEFAULT-BYTE-SIZE FILE)))
                "")
            (MFILE-DEFAULT-BYTE-SIZE FILE)
            MONTH DAY YEAR HOURS MINUTES SECONDS
            (MFILE-AUTHOR-INTERNAL FILE))
    (cond (debug-p
           (print-brief-attribute-description-in-brackets file)
           (TV:DOPLIST ((MFILE-PLIST FILE) PROP IND)
             (FORMAT T "~%   ~A: ~d" IND PROP))
           (FORMAT T "~%   Open Count: ~D" (MFILE-OPEN-COUNT FILE))
           (FORMAT T "~%   Overwriting: ~S" (MFILE-OVERWRITE-FILE FILE))
           (FORMAT T "~%   Directory: ~S~%" (MFILE-DIRECTORY FILE))
           (DESCRIBE-ARRAY-OF-ARRAYS (MFILE-DATA-POINTER FILE))))))

(defun print-brief-attribute-description-in-brackets (file)
           (FORMAT T " {")
           (DO ((I 0 (1+ I))
                (SPACE NIL))
               ((> I 16.))
             (COND ((LDB-TEST (1+ (* I 64.))
                              (MFILE-ATTRIBUTES FILE))
                    (IF (NOT (NULL SPACE))
                        (FUNCALL STANDARD-OUTPUT ':TYO #\SP))
                    (SEND STANDARD-OUTPUT ':STRING-OUT
                          (GET-PNAME (AREF ATTRIBUTES I)))
                    (SETQ SPACE T))))
           (FORMAT T "}"))

;;; These functions are useful for editing the directory in-core.

;(DEFUN DIRECTORY-TREE-IN-CORE? (PARTITION-HOST)
;  (DIRECTORY-SUBTREE-IN-CORE? PARTITION-HOST
;    (LOOKUP-ROOT-directory-files PARTITION-HOST)))

;(DEFUN DIRECTORY-SUBTREE-IN-CORE? (PARTITION-HOST DIRECTORY)
;  (AND (NOT (EQ (MFILE-FILES DIRECTORY) ':DISK))
;       (EVERY DIRECTORY
;             #'(LAMBDA (FILE)
;                 (AND (MDIRECTORY? FILE)
;                      (DIRECTORY-SUBTREE-IN-CORE? PARTITON-HOST FILE))))))

;(DEFUN READ-FILE-SYSTEM-INTO-CORE ()
;  "Read the entire directory tree into core."
;  (READ-DIRECTORY-SUBTREE-INTO-CORE *MOBY-ROOT-DIRECTORY*))

;(DEFUN READ-DIRECTORY-SUBTREE-INTO-CORE (DIRECTORY)
;  (DOLIST (FILE (READ-DIRECTORY-FILES DIRECTORY))
;    (IF (MDIRECTORY? FILE)
;       (READ-DIRECTORY-SUBTREE-INTO-CORE FILE))))

;(DEFUN WRITE-CORE-FILE-SYSTEM-ONTO-DISK ()
;  "Given a consistent directory tree in core, write the entire
;structure out to disk."
;  (SAVE-DIRECTORY-TREE ':SAVE-ALL))

(DEFUN MBFS-LIST-DIRECTORIES (PARTITION-HOST &REST DIRECTORIES)
  (OR DIRECTORIES (SETQ DIRECTORIES (TOP-LEVEL-DIRECTORIES PARTITION-HOST)))
  (LOOP FOR DIRECTORY IN DIRECTORIES
        AS LOOKUP = (LOOKUP-DIRECTORY PARTITION-HOST DIRECTORY T)
        WHEN LOOKUP DO (FORMAT T "~%~A" (MFILE-NAME LOOKUP))
        ELSE DO (FORMAT T "~%~A No Such Directory." DIRECTORY)))

(DEFUN MBFS-LIST-FILES (PARTITION-HOST &REST DIRECTORIES)
  (COND ((FIND-ROOT-DIRECTORY PARTITION-HOST)
         (OR DIRECTORIES (SETQ DIRECTORIES (TOP-LEVEL-DIRECTORIES PARTITION-HOST)))
         (LOOP FOR DIRECTORY IN DIRECTORIES
               AS LOOKUP = (LOOKUP-DIRECTORY PARTITION-HOST DIRECTORY T)
               WHEN LOOKUP DO (FORMAT T "~%~A:" (MFILE-NAME LOOKUP))
                              (DOLIST (FILE (MFILE-FILES LOOKUP))
                                (DBG-EXAMINE-SHOW-FILE FILE NIL))
                              (FORMAT T "~%~%")
               ELSE DO (FORMAT T "~%~A:  No Such Directory.~%~%" DIRECTORY)))
        (T (FORMAT T "~%File system not mounted -- do (FS:BOOT-FILE-SYSTEM)"))))

(DEFUN MBFS-CLOSE-ALL-FILES (&OPTIONAL (ABORTP ':ABORT))
  (LOOP AS STREAM = (WITHOUT-INTERRUPTS (POP MLM-FILE-STREAMS-LIST))
        UNTIL (NULL STREAM)
        DO (CATCH-ERROR (FUNCALL STREAM ':CLOSE ABORTP))))

;(ADD-INITIALIZATION "Boot Moby File System" '(BOOT-FILE-SYSTEM) '(WARM))
;(ADD-INITIALIZATION "Boot Moby File System" '(BOOT-FILE-SYSTEM) '(AFTER-FULL-GC))
;(ADD-INITIALIZATION "Dismount Moby File System" '(DISMOUNT-FILE-SYSTEM) '(BEFORE-COLD))
;(ADD-INITIALIZATION "Dismount Moby File System" '(DISMOUNT-FILE-SYSTEM) '(FULL-GC))

;; Call this to remove all obsolete properties in a file system.

;;;; ***** These are broken since the file system was redesigned *****

;(DEFUN REMOVE-OBSOLETE-PROPERTIES (PARTITION-HOST)
;  (READ-FILE-SYSTEM-INTO-CORE)
;  (DOLIST (DIR (TOP-LEVEL-DIRECTORIES PARTITION-HOST))
;    (DOLIST (FILE (MFILE-FILES DIR))
;      (SETF (MFILE-ATTRIBUTE FILE ':LISP) NIL)
;      (SETF (MFILE-ATTRIBUTE FILE ':TEXT) NIL)
;      ;; Make an educated guess as to the :CHARACTERS status of the files.
;      (SETF (MFILE-ATTRIBUTE FILE ':CHARACTERS) NIL)
;      (AND (= (MFILE-DEFAULT-BYTE-SIZE FILE) 8)
;          ;; Image files are not characters.
;          (NOT (GET (LOCF (MFILE-PLIST FILE)) ':NAME))
;          (SETF (MFILE-ATTRIBUTE FILE ':CHARACTERS) T))
;      (DOLIST (PROP '(:FONTS :PATCH-FILE :BASE :LOWERCASE :MODE :PACKAGE
;                     :COMPILE-DATA :QFASL-SOURCE-FILE-UNIQUE-ID))
;       (REMPROP (LOCF (MFILE-PLIST FILE)) PROP))))
;  (WRITE-CORE-FILE-SYSTEM-ONTO-DISK))

;; Call this to set up necessary properties without ruining old ones.
;(DEFUN SETUP-NECESSARY-PROPERTIES (PARTITION-HOST)
;  (READ-FILE-SYSTEM-INTO-CORE)
;  (DOLIST (DIR (TOP-LEVEL-DIRECTORIES PARTITION-HOST))
;    (DOLIST (FILE (MFILE-FILES DIR))
;      ;; Make an educated guess as to the :CHARACTERS status of the files.
;      (SETF (MFILE-ATTRIBUTE FILE ':CHARACTERS) NIL)
;      (AND (= (MFILE-DEFAULT-BYTE-SIZE FILE) 8)
;          ;; Image files are not characters.
;          (NOT (GET (LOCF (MFILE-PLIST FILE)) ':NAME))
;          (SETF (MFILE-ATTRIBUTE FILE ':CHARACTERS) T))))
;  (WRITE-CORE-FILE-SYSTEM-ONTO-DISK))


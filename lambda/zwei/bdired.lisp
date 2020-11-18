;-*- Mode:LISP; Package:ZWEI; Base:8. -*-
;;; Balance Directories Editor, by HANSON -- feel free to use it.

;;;; BDIRED Lines

(DEFSUBST BDIRED-CURRENT-LINE (STREAM)
  (BP-LINE (SEND STREAM ':READ-BP)))

(DEFSUBST BDIRED-PREVIOUS-LINE (STREAM)
  (LINE-PREVIOUS (BDIRED-CURRENT-LINE STREAM)))

(DEFSUBST BDIRED-NEXT-LINE (STREAM)
  (LINE-NEXT (BDIRED-CURRENT-LINE STREAM)))

(DEFSUBST PUT-LINE (LINE ITEM KEY)
  (PUTPROP (LOCF (LINE-PLIST LINE)) ITEM KEY))

(DEFSUBST GET-LINE (LINE KEY)
  (GET (LOCF (LINE-PLIST LINE)) KEY))

(DEFUN BDIRED-DELETE-LINE (FIRST-LINE &OPTIONAL LAST-LINE)
  (IF (NULL LAST-LINE) (SETQ LAST-LINE FIRST-LINE))
  (WITH-READ-ONLY-SUPPRESSED (*INTERVAL*)
    (DELETE-INTERVAL (BEG-LINE (CREATE-BP FIRST-LINE 0) 0)
                     (BEG-LINE (CREATE-BP LAST-LINE 0) 1)
                     T)))

;;; Line CFILE.

(DEFUN BDIRED-LIST-CFILE (CFILE STREAM)
  (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* CFILE STREAM)
  (LET ((LINE (BDIRED-PREVIOUS-LINE STREAM)))
    (SETF (LINE-PLIST LINE) (CONS ':CFILE CFILE))
    LINE))


(DEFSUBST BDIRED-LINE-CFILE (LINE)
  (GET (LOCF (LINE-PLIST LINE)) :CFILE))

;;;; BDIRED Buffers

;(DEFSUBST BDIRED-ALTERNATE-PATHNAME (BUFFER)
;  (FOURTH (BUFFER-FILE-ID BUFFER)))

(DEFSUBST BDIRED-ALTERNATE-PATHNAME (BUFFER)
  (GET (LOCF (NODE-PROPERTY-LIST BUFFER)) ':ALTERNATE-PATHNAME))

(DEFSUBST UPDATE-BDIRED-BUFFER-PATHNAMES (BUFFER PATHNAME1 PATHNAME2)
  (SETF (BUFFER-PATHNAME BUFFER) PATHNAME1)
  (SETF (BDIRED-ALTERNATE-PATHNAME BUFFER) PATHNAME2))

(DEFUN FIND-OR-CREATE-BDIRED-BUFFER (PATHNAME1 PATHNAME2)
  (LET ((BUFFER (FUNCALL-SELF ':FIND-SPECIAL-BUFFER ':BDIRED
                              T "BDired")))
    (IF (NULL BUFFER) (ERROR "Cannot find BDIRED buffer" ()))
    (UPDATE-BDIRED-BUFFER-PATHNAMES BUFFER PATHNAME1 PATHNAME2)
    BUFFER))

;;;; Pathname Stuff

;(DEFUN BDIRED-READ-DIRECTORY-STRING (PROMPT PATHNAME)
;  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
;       (*READING-PATHNAME-SPECIAL-TYPE* (FS:PATHNAME-TYPE PATHNAME))
;       (*READING-PATHNAME-SPECIAL-VERSION* (FS:PATHNAME-VERSION PATHNAME))
;       (*READING-PATHNAME-DIRECTION* ':READ)
;       (*READING-PATHNAME-LAST-NAME-TYPED* *LAST-FILE-NAME-TYPED*))
;    (MULTIPLE-VALUE-BIND (NIL NIL INTERVAL)
;       (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL
;                            (LIST (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME)
;                                  '(:RIGHT-FLUSH " (Completion)")))
;      (STRING-INTERVAL INTERVAL))))

;(DEFUN BDIRED-READ-DIRECTORY-NAME (PROMPT PATHNAME)
;  (LET ((INTERVAL (BDIRED-READ-DIRECTORY-STRING PROMPT PATHNAME)))
;    (COND ((EQUAL INTERVAL "")
;          PATHNAME)
;         (T
;          (SETQ *LAST-FILE-NAME-TYPED* INTERVAL)
;          (FS:MERGE-PATHNAME-DEFAULTS INTERVAL PATHNAME NIL NIL)))))

;;;; Top Level

(DEFVAR *BDIRED-PATHNAME1-NAME* :UNBOUND
  "Pathname of active BDIRED buffer")

(DEFVAR *BDIRED-PATHNAME2-NAME* :UNBOUND
  "Alternate pathname of active BDIRED buffer")

(DEFMAJOR COM-BDIRED-MODE BDIRED-MODE "BDired"
          "Setup for editing directory differences" ()
  (PROGN                                        ;Due to ZWEI inversion lossage.
  (IF (TYPEP *INTERVAL* 'FILE-BUFFER)
      (LET ((PATHNAME1 (BUFFER-PATHNAME *INTERVAL*))
            (PATHNAME2 (BDIRED-ALTERNATE-PATHNAME *INTERVAL*)))
        (IF (NOT (NULL PATHNAME1))
            (SETQ *BDIRED-PATHNAME1-NAME* (STRING PATHNAME1)))
        (IF (NOT (NULL PATHNAME2))
            (SETQ *BDIRED-PATHNAME2-NAME* (STRING PATHNAME2))))))
  (SET-COMTAB *MODE-COMTAB*
              '(#/SP COM-DOWN-REAL-LINE
                #/= COM-DIRED-SRCCOM
                #/? COM-BDIRED-DOCUMENTATION
                #/HELP COM-BDIRED-DOCUMENTATION
                #/C COM-DIRED-COPY
                #/c (0 #/C)
                ;#/I COM-BDIRED-RESOLVE-INCONSISTENCY
                ;#/i (0 #/I)
                #/P com-bdired-print-transfer
                #/p (0 #/P)
                #/Q COM-BDIRED-EXIT
                #/q (0 #/Q)
                #/R COM-DIRED-RENAME
                #/r (0 #/R)
                #/T COM-BDIRED-TRANSFER
                #/t (0 #/T)
                #/U COM-BDIRED-UNMARK
                #/u (0 #/U)
                #/V COM-DIRED-VIEW-FILE
                #/v (0 #/V)
                #/RUBOUT COM-BDIRED-REVERSE-UNMARK
                #/ABORT COM-BDIRED-ABORT
                #/END COM-BDIRED-EXIT)
              '())
  (SETQ *MODE-LINE-LIST*
        (APPEND *MODE-LINE-LIST*
                '("  " *BDIRED-PATHNAME1-NAME*
                  "  " *BDIRED-PATHNAME2-NAME*
                  "  (Q to exit)"))))

(DEFUN (BDIRED-MODE PATHNAME-DEFAULTING-FUNCTION) (IGNORE BUFFER)
  (AND (EQ BUFFER *INTERVAL*)
       (DONT-OPTIMIZE (DIRED-LINE-PATHNAME (BP-LINE (POINT))))))

(DEFCOM COM-BDIRED "Edit the differences between two directories." ()
  (KILL-NEW-BUFFER-ON-ABORT (*INTERVAL*)
    (LET* ((DEFAULT (SEND (DEFAULT-PATHNAME) ':NEW-PATHNAME
                          ':NAME NIL
                          ':TYPE NIL
                          ':VERSION NIL))
           (PATHNAME1 (READ-DIRECTORY-NAME "Balance directory" DEFAULT)))
      (BDIRECTORY-EDIT
        PATHNAME1
        (READ-DIRECTORY-NAME (FORMAT NIL "Balance directory ~A with directory" PATHNAME1)
                             PATHNAME1)))))

(DEFUN BDIRECTORY-EDIT (PATHNAME1 PATHNAME2)
  "Enter the balance directories editor on directories PATHNAME1 and PATHNAME2.
In ZMACS, this creates a BDIRED buffer and reads the differences into it,
making it the current buffer."
  (LET ((*INTERVAL* (FIND-OR-CREATE-BDIRED-BUFFER PATHNAME1 PATHNAME2)))
    (MAKE-BUFFER-READ-ONLY *INTERVAL*)
    (COM-BDIRED-MODE)
    (BDIRED-INITIALIZE-BUFFER *INTERVAL* PATHNAME1 PATHNAME2)
    DIS-TEXT))

;;;; BDIRED Buffer Initialization

(DEFPROP BDIRED-MODE BDIRECTORY-EDIT-REVERT MAJOR-MODE-REVERT-FUNCTION)

(DEFUN BDIRECTORY-EDIT-REVERT (BUFFER PATHNAME &OPTIONAL IGNORE SELECT-FLAG IGNORE)
  "Revert a BDIRED buffer."

  (BDIRED-INITIALIZE-BUFFER BUFFER
                            PATHNAME
                            (BDIRED-ALTERNATE-PATHNAME BUFFER)
                            SELECT-FLAG))

(DEFUN BDIRED-INITIALIZE-BUFFER (BUFFER PATHNAME1 PATHNAME2 &OPTIONAL SELECT-FLAG)
  "Initialize a BDIRED buffer."

  (WITH-READ-ONLY-SUPPRESSED (BUFFER)
    (LET ((*BATCH-UNDO-SAVE* T)
          (SELECTED-P (EQ BUFFER *INTERVAL*))
          (*INTERVAL* BUFFER)
          (DIR1 (FS:CREATE-CDIRECTORY PATHNAME1 '() T))
          (DIR2 (FS:CREATE-CDIRECTORY PATHNAME2 '() T)))
      ;; What directories did we ultimately read?  Maybe the user corrected an error.
      (SETQ PATHNAME1 (FS:CDIRECTORY-PATHNAME DIR1))
      (SETQ PATHNAME2 (FS:CDIRECTORY-PATHNAME DIR2))
      (UPDATE-BDIRED-BUFFER-PATHNAMES BUFFER PATHNAME1 PATHNAME2)
      (FS:COMPARE-CDIRECTORIES DIR1 DIR2)
      (DELETE-INTERVAL BUFFER)
      (DISCARD-UNDO-INFORMATION BUFFER)
      (IF SELECTED-P
          (SETQ *BDIRED-PATHNAME1-NAME* (FUNCALL PATHNAME1 ':STRING-FOR-PRINTING)
                *BDIRED-PATHNAME2-NAME* (FUNCALL PATHNAME2 ':STRING-FOR-PRINTING)))
      (LET ((STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP BUFFER))))
        (BDIRED-INSERT-DIRECTORY DIR1 STREAM DIR2)
        (SEND STREAM ':TYO #/CR)
        (BDIRED-INSERT-DIRECTORY DIR2 STREAM DIR1))
      (LET ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER))))
        (PUT-LINE FIRST-LINE DIR1 'CDIRECTORY1)
        (PUT-LINE FIRST-LINE DIR2 'CDIRECTORY2)))
    (BDIRED-MARK-INITIAL-TRANSFERS))
  (IF SELECT-FLAG (MAKE-BUFFER-CURRENT BUFFER)))

(DEFUN BDIRED-INSERT-DIRECTORY (DIRECTORY STREAM ALTERNATE-DIRECTORY)
  (LET ((DIRLIST (FS:CDIRECTORY-LIST DIRECTORY))
        (PATHNAME (FS:CDIRECTORY-PATHNAME DIRECTORY)))
    (DOLIST (CFILE (CDR DIRLIST))
      (PUTPROP (FS:CFILE-PLIST CFILE) ALTERNATE-DIRECTORY ':ALTERNATE-DIRECTORY))
    (SEND STREAM ':STRING-OUT (SEND PATHNAME ':STRING-FOR-PRINTING))
    (SEND STREAM ':LINE-PUT ':DIRECTORY PATHNAME)
    (SEND STREAM ':TYO #/CR)
    (SEND *DIRECTORY-SINGLE-FILE-LISTER* (CAR DIRLIST) STREAM)
        ;hands to DIRED of a CDIRECTORY.  The :CFILE property of each line
        ; gets the CAR of the DIRLIST element.
    (BDIRED-INSERT-DIRLIST DIRLIST STREAM 0)))

(DEFUN BDIRED-INSERT-DIRLIST (DIRECTORY STREAM LEVEL)
; this is almost the same function as DIRED-INSERT-DIRECTORY, but it puts the CFILE
;  on the line as a CFILE property. The TRUENAME is also put on as the :PATHNAME property
;  for compatibility with DIRED commands.
  "Insert into a DIRED buffer lines describing the files in DIRECTORY.
DIRECTORY is a value returned by FS:DIRECTORY-LIST.
STREAM is a stream outputting into the DIRED buffer.
LEVEL is the depth in subdirectories of these files.
Returns the first inserted line that describes a file."
  ;; Mark all files that are the newest
  (DIRED-COMPUTE-GREATER-THANS (CDR DIRECTORY))
  (DO ((FILES DIRECTORY (CDR FILES))
       (FILE)
       (LINE) (FIRST-FILE-LINE))
      ((NULL FILES)
       FIRST-FILE-LINE)
    (SETQ FILE (CAR FILES))
    (UNLESS (NULL (FS:CFILE-TRUENAME FILE))
      (IF (GET FILE ':DIRECTORY)
          (LET ((STR (SEND (SEND (SEND (FS:CFILE-TRUENAME FILE)
                                       :NEW-PATHNAME :DEVICE NIL
                                                     ;; Get rid of the version iff
                                                     ;; this is the newest one.
                                                     :VERSION (IF (GET FILE :NEWEST)
                                                                  NIL
                                                                  (SEND (CAR FILE) :VERSION)))
                                 :PATHNAME-AS-DIRECTORY)
                           :STRING-FOR-DIRECTORY)))
            ;; STR has the string we want to print instead of the filename.
            ;; Replace (CAR FILE) with a phony "pathname" that will print as that string.
            (WITH-STACK-LIST* (FILE1 #'(LAMBDA (&REST IGNORE) STR) (CDR FILE))
              (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE1 STREAM)))
        (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* FILE STREAM))
      (SETQ LINE (LINE-PREVIOUS (BP-LINE (SEND STREAM :READ-BP))))
      (INSERT-CHARS (CREATE-BP LINE 5) #/SPACE (* *DIRED-SUBDIRECTORY-INDENTATION* LEVEL))
      ;; Use lower-case "d" to mark already-deleted files.
      (IF (GET FILE ':DELETED)
          (SETF (CHAR LINE 0) #/d))
      (OR FIRST-FILE-LINE
          (SETQ FIRST-FILE-LINE LINE))
      (SETF (GETF (LINE-PLIST LINE) 'LEVEL) LEVEL)
      (LOOP FOR (PROP VAL) ON (CDR FILE) BY 'CDDR       ;transfer properties to dired line.
         DO (SETF (GETF (LINE-PLIST LINE) PROP) VAL))
      (SETF (GETF (LINE-PLIST LINE) ':CFILE) FILE)
      (SETF (GETF (LINE-PLIST LINE) ':PATHNAME) (CAR FILE))
      )))

;;;; Commands

(DEFCOM COM-BDIRED-EXIT "Exit from BDIRED, performing transfers that you have selected." ()
  (IF (BDIRED-PROCESS-FILES)
      (FUNCALL-SELF ':EXIT-SPECIAL-BUFFER T *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-BDIRED-ABORT "Exit BDIRED, not transferring any files." ()
  (FUNCALL-SELF ':EXIT-SPECIAL-BUFFER T *INTERVAL*)
  DIS-BPS)

(DEFUN BDIRED-MAP-OVER-ALL-LINES (FUN)
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))
             (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
      ((EQ LINE LAST-LINE))
    (FUNCALL FUN LINE)))

(DEFUN BDIRED-MARK-INITIAL-TRANSFERS ()
  (BDIRED-MAP-OVER-ALL-LINES #'BDIRED-MARK-INITIAL-TRANSFER))

(DEFUN BDIRED-MARK-INITIAL-TRANSFER (LINE)
  (COND ((NOT (NULL (FS:CFILE-TRANSFER-DESTINATIONS (BDIRED-LINE-CFILE LINE))))
         (MUNG-LINE LINE)
         (ASET #/T LINE 0))))

(DEFUN BDIRED-MARK-TRANSFER (CFILE)
  (FS:MARK-CFILE-FOR-TRANSFER CFILE
                              (GET (FS:CFILE-PLIST CFILE) ':ALTERNATE-DIRECTORY)))

(DEFUN BDIRED-UNMARK-TRANSFER (CFILE)
  (IF (NOT (NULL (FS:CFILE-TRANSFER-DESTINATIONS CFILE)))
      (SETF (FS:CFILE-TRANSFER-DESTINATIONS CFILE) '())))

(DEFCOM COM-BDIRED-PRINT-TRANSFER "Print intended transfers for file(s)." ()
  (DIRED-MAP-OVER-LINES *NUMERIC-ARG*
                        #'(LAMBDA (LINE)
                            (BDIRED-PRINT-TRANSFER (BDIRED-LINE-CFILE LINE)))))

(defun bdired-print-transfer (cfile)
  (let ((*print-circle* t))
    (print (fs:cfile-transfer-destinations cfile))))

(DEFUN BDIRED-PROCESS-FILES ()
  (LET ((TERMINAL-IO *TYPEIN-WINDOW*)
        (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
    (LET ((DIR1 (GET-LINE FIRST-LINE 'CDIRECTORY1))
          (DIR2 (GET-LINE FIRST-LINE 'CDIRECTORY2)))
      (FS:SETUP-CDIRECTORY-TRANSFER-MODE DIR1)
      (FS:SETUP-CDIRECTORY-TRANSFER-MODE DIR2)
      (FS:CDIRECTORY-TRANSFER DIR1)
      (FS:CDIRECTORY-TRANSFER DIR2)))
  T)

(DEFCOM COM-BDIRED-TRANSFER "Mark file(s) for transfer." ()
  (DIRED-MAP-OVER-LINES *NUMERIC-ARG*
                        #'(LAMBDA (LINE)
                            (MUNG-LINE LINE)
                            (ASET #/T LINE 0)
                            (BDIRED-MARK-TRANSFER (BDIRED-LINE-CFILE LINE)))))

(DEFCOM COM-BDIRED-UNMARK "Un-mark file(s) for transfer or other action." ()
  (DIRED-MAP-OVER-LINES (IF (AND (NOT *NUMERIC-ARG-P*)
                                 (OR (NOT (DIRED-LINE-PATHNAME (BP-LINE (POINT))))
                                     (EQ (BP-CHAR (BEG-LINE (POINT))) #/SP)))
                            -1
                            *NUMERIC-ARG*)
                        #'(LAMBDA (LINE)
                            (MUNG-LINE LINE)
                            (ASET #/SP LINE 0)
                            (BDIRED-UNMARK-TRANSFER (BDIRED-LINE-CFILE LINE)))))

(DEFCOM COM-BDIRED-REVERSE-UNMARK "Un-mark previous file(s) for transfer or other action." ()
  (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
  (COM-BDIRED-UNMARK))


(COMMENT                                        ;may want some of this later.

;;; An Inconsistency consists of two Cfiles and
;;; a group of consecutive Lines in a Buffer.

(DEFSTRUCT (INCONSISTENCY (:TYPE :NAMED-ARRAY) :CONC-NAME)
  CFILE1
  CFILE2
  FIRST-LINE
  LAST-LINE)

(DEFUN BDIRED-INSERT-INCONSISTENCY (CFILE1 CFILE2 STREAM)
  (LET* ((LINE1 (BDIRED-LIST-CFILE CFILE1 STREAM))
         (LINE2 (BDIRED-LIST-CFILE CFILE2 STREAM))
         (LINE3 (PROGN (SEND STREAM ':TYO #/CR)
                       (BDIRED-PREVIOUS-LINE STREAM)))
         (INCONSISTENCY (MAKE-INCONSISTENCY FIRST-LINE LINE1
                                            LAST-LINE LINE3
                                            CFILE1 CFILE1
                                            CFILE2 CFILE2)))
    (PUT-LINE LINE1 INCONSISTENCY 'BDIRED-INCONSISTENCY)
    (PUT-LINE LINE2 INCONSISTENCY 'BDIRED-INCONSISTENCY)
    (PUT-LINE LINE3 INCONSISTENCY 'BDIRED-INCONSISTENCY)
    LINE1))

(DEFSUBST BDIRED-DELETE-INCONSISTENCY (INCONSISTENCY)
  (BDIRED-DELETE-LINE (INCONSISTENCY-FIRST-LINE INCONSISTENCY)
                      (INCONSISTENCY-LAST-LINE INCONSISTENCY)))

(DEFSUBST RELATED-INCONSISTENCY (LINE)
  (GET-LINE LINE 'BDIRED-INCONSISTENCY))

(DEFUN INCONSISTENCY-RELATED-CFILE (INCONSISTENCY CFILE)
  (COND ((EQ CFILE (INCONSISTENCY-CFILE1 INCONSISTENCY))
         (INCONSISTENCY-CFILE2 INCONSISTENCY))
        ((EQ CFILE (INCONSISTENCY-CFILE2 INCONSISTENCY))
         (INCONSISTENCY-CFILE1 INCONSISTENCY))
        (T
         (ERROR "Cfile not element of Inconsistency"
                (LIST CFILE INCONSISTENCY)))))

(DEFCOM COM-BDIRED-RESOLVE-INCONSISTENCY
        "Alter the other file's version//creation date.
When pointing at one file of an inconsistency, alters the version//creation-date
information of the other file to match this file." ()
  (LET ((LINE (BP-LINE (POINT))))
    (LET ((CORRECT-CFILE (BDIRED-LINE-CFILE LINE))
          (INCONSISTENCY (OR (RELATED-INCONSISTENCY LINE)
                             (BARF "Not an inconsistency"))))
      (LET ((INCORRECT-CFILE (INCONSISTENCY-RELATED-CFILE INCONSISTENCY
                                                          CORRECT-CFILE)))
        (LET ((V1 (FS:CFILE-VERSION CORRECT-CFILE))
              (V2 (FS:CFILE-VERSION INCORRECT-CFILE))
              (CD1 (FS:CFILE-CREATION-DATE CORRECT-CFILE))
              (CD2 (FS:CFILE-CREATION-DATE INCORRECT-CFILE))
              (TRUENAME (FS:CFILE-TRUENAME INCORRECT-CFILE)))
          (IF (= V1 V2)
              (IF (NOT (= CD1 CD2))
                  (BDIRED-CHANGE-CREATION-DATE TRUENAME CD1))
              (IF (= CD1 CD2)
                  (BDIRED-CHANGE-VERSION TRUENAME V1)
                  (BDIRED-CHANGE-VERSION-AND-CREATION-DATE TRUENAME V1 CD1)))))
      (BDIRED-DELETE-INCONSISTENCY INCONSISTENCY)))
  DIS-TEXT)

(DEFUN BDIRED-CHANGE-CREATION-DATE (TRUENAME DATE)
  (WITH-OPEN-FILE (STREAM TRUENAME ':DIRECTION ':INPUT)
    (SEND STREAM ':CHANGE-PROPERTIES NIL ':CREATION-DATE DATE))
  (FORMAT *QUERY-IO* "~&Creation date of ~A changed to ~A"
          TRUENAME (TIME:PRINT-UNIVERSAL-TIME DATE NIL)))

(DEFUN BDIRED-CHANGE-VERSION (TRUENAME VERSION)
  (LET ((NEWNAME (SEND TRUENAME ':NEW-VERSION VERSION)))
    (WITH-OPEN-FILE (STREAM TRUENAME ':DIRECTION ':INPUT)
      (SEND STREAM ':RENAME NEWNAME))
    (FORMAT *QUERY-IO* "~&~A renamed to ~A" TRUENAME NEWNAME)))

(DEFUN BDIRED-CHANGE-VERSION-AND-CREATION-DATE (TRUENAME VERSION DATE)
  (LET ((NEWNAME (SEND TRUENAME ':NEW-VERSION VERSION)))
    (WITH-OPEN-FILE (STREAM TRUENAME ':DIRECTION ':INPUT)
      (SEND STREAM ':RENAME NEWNAME)
      (SEND STREAM ':CHANGE-PROPERTIES NIL ':CREATION-DATE DATE))
    (FORMAT *QUERY-IO* "~&~A renamed to ~A;~%creation date changed to ~A"
            TRUENAME NEWNAME (TIME:PRINT-UNIVERSAL-TIME DATE NIL))))
)

;;;; Documentation

(DEFCOM COM-BDIRED-DOCUMENTATION "Print various sorts of editor documentation" ()
  (LET ((*COM-DOCUMENTATION-ALIST*
          (CONS '(#/M COM-BDIRED-HELP) *COM-DOCUMENTATION-ALIST*)))
    (COM-DOCUMENTATION)))

(DEFCOM COM-BDIRED-HELP "Explain use of DIRED commands" ()
  (FORMAT T "You are in the balance directories editor.  The commands are:
        T       Mark the current file to be transferred.
        P       Mark the current file to be printed on the standard hardcopy device.
        U       Cancel any request marked on the current line, or else the line above.
        Rubout  Move up and cancel any request marked on that line.
        R       Rename this file.  You type the new filename in a mini buffer.
        C       Copy this file.  You type the new filename in a mini buffer.
        Space   Move to the next line.
          Above commands repeat with a numeric argument,
          backwards if the argument is negative.
        V       View current file.
        Q (or End)  Exit.  The remaining files in the transfer lists will be moved.
        Abort   Exit without performing any transfers.
        =       SRCCOM this file with the > version.
")
  DIS-NONE)

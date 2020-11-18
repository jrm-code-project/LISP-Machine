;;; -*- Mode:LISP; Package:ZWEI; Readtable:CL; Base:10; Fonts:(CPTFONT CPTFONTB) -*-


;(DEFUN NEED-TO-REVERT-BUFFER (PATHNAME)
;  (LET ((PROBE-STREAM (PROBEF (SEND PATHNAME :NEW-VERSION :NEWEST))))
;    (IF (NOT PROBE-STREAM)
;       (BARF "Cannot find any existing versions of ~A.  Perhaps all versions have been deleted."
;             PATHNAME)
;      (let ((desired-version (SEND PROBE-STREAM :VERSION)))
;       (IF (GREATERP desired-version
;                     (SEND PATHNAME :VERSION))
;           (COND ((EQUAL (COMPLETING-READ-FROM-MINI-BUFFER
;                           "A newer version of this file exists now.  Revert this DIRED buffer? (default is no)"
;                           '(("yes") ("no")))
;                         '("yes"))
;                  (values (REVERT-BUFFER *INTERVAL*) nil))
;                 ((EQUAL (COMPLETING-READ-FROM-MINI-BUFFER
;                           "Do you want to see the newest version? (default is no)"
;                           '(("yes") ("no")))
;                         '("yes"))
;                  (VALUES (SEND PATHNAME :NEW-VERSION :NEWEST) desired-version))
;                 (T (VALUES PATHNAME NIL)))
;         (VALUES (SEND PATHNAME :NEW-VERSION :NEWEST) DESIRED-VERSION))))))

;(DEFCOM COM-DIRED-EDIT-FILE "Edit the current file; or DIRED it if it's a directory." ()
;  (OR (TYPEP *WINDOW* 'ZMACS-WINDOW) (BARF "Files can only be edited in ZMACS windows."))
;  (WHEN (GETF (LINE-PLIST (BP-LINE (POINT))) ':DELETED)
;    (BARF "The file ~A has been marked as DELETED.  To compare contents, first undelete this file."
;         (DIRED-LINE-PATHNAME (bp-line (point)))))
;  (LET* ((LINE (BP-LINE (POINT)))
;        (DIR-P (GETF (LINE-PLIST LINE) ':DIRECTORY))
;        (PATHNAME (DIRED-LINE-PATHNAME-OR-BARF LINE))
;        version-we-want) ;if version of pathname is :NEWEST, otherwise NIL.  This avoids bug
;               ;where there exists a > buffer, but some other machine has written a new version
;               ;in the meantime.
;    (AND (NOT DIR-P) ; Reversion doesn't make sense for recursive Dired
;        (GETF (LINE-PLIST LINE) ':NEWEST)
;        (IF (NULL (MULTIPLE-VALUE-SETQ (PATHNAME version-we-want)
;                    (NEED-TO-REVERT-BUFFER PATHNAME)))
;            (BARF "Please retype the command.")))
;    (IF DIR-P
;       (DIRECTORY-EDIT (SEND (SEND PATHNAME :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
;                                                                     :NAME :WILD
;                                                                     :TYPE :WILD
;                                                                     :VERSION :WILD))
;      (select-buffer-pathname pathname version-we-want)
;      (LET ((RETURN-KEY (KEY-FOR-COMMAND 'COM-SELECT-PREVIOUS-BUFFER
;                                   *COMTAB* NIL NIL #/C-M-L)))
;       (AND (NULL RETURN-KEY) (SETQ RETURN-KEY (KEY-FOR-COMMAND 'COM-SELECT-BUFFER))
;            (SETQ RETURN-KEY (STRING-APPEND RETURN-KEY " Return")))
;       (AND RETURN-KEY
;            (FORMAT *QUERY-IO* "~&Type ~A to return to DIRED~%" RETURN-KEY))
;       DIS-TEXT))))

;(defun select-buffer-pathname (pathname &optional version-we-want)
;  "Select buffer associated with PATHNAME, if any.  If version-we-want non-NIL,
;make sure buffer is associated with this version (particularily important for #> buffers."
;  (LET ((BUFFER (FIND-FILE-BUFFER PATHNAME)))
;    (cond ((null buffer) (FIND-FILE PATHNAME))
;         ((or (null version-we-want)
;              (symbolp (buffer-file-id buffer))
;              (eq version-we-want (buffer-file-version-if-known buffer)))
;          (MAKE-BUFFER-CURRENT BUFFER))
;         ((if (buffer-modified-p buffer)
;              (equal (completing-read-from-mini-buffer
;                       "Since #> buffer was created, a newer version exists.  THIS BUFFER HAS BEEN MODIFIED.  Revert? (default is no)"
;                       '(("yes") ("no")))
;                     '("yes"))
;            (not (equal (completing-read-from-mini-buffer
;                          "Since #> buffer was created, a newer version exists.  Revert? (default is yes)"
;                          '(("yes") ("no")))
;                        '("no"))))
;          (revert-buffer buffer)
;          (make-buffer-current buffer))
;         (t (make-buffer-current buffer)))))


;(DEFUN WRITE-BUFFER (BUFFER)
;  "Write BUFFER to a file, reading filename in mini buffer."
;  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME (FORMAT NIL "Write buffer ~A to File:"
;                                                  (BUFFER-NAME BUFFER))
;                                          (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* BUFFER)
;                                          NIL NIL :WRITE)))
;    (WRITE-FILE-INTERNAL PATHNAME BUFFER))
;  DIS-NONE)

;(DEFCOM COM-WRITE-FILE "Write out the buffer to the specified file." ()
;  (WRITE-BUFFER *INTERVAL*)
;  (MAYBE-DISPLAY-DIRECTORY :WRITE)
;  DIS-NONE)

;;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#558 on 2-Oct-86 02:29:19
;(DEFUN WRITE-FILE-INTERNAL (PATHNAME &OPTIONAL (BUFFER *INTERVAL*))
;  "Save BUFFER in file PATHNAME and mark it as visiting that file.
;If the file holds the current ODM node, handle it appropriately."
;  (if (and (boundp '*current-gateway-buffer*)
;          (eq buffer *current-gateway-buffer*)
;          (fboundp 'write-file-internal-odm))
;        (write-file-internal-odm pathname buffer)
;  (SEND BUFFER :WRITE-FILE-INTERNAL PATHNAME)))  ;in METH.LISP

;(DEFUN SET-BUFFER-PATHNAME (PATHNAME &OPTIONAL (BUFFER *INTERVAL*) &AUX STRING)
;  "Set the pathname BUFFER is visiting to PATHNAME."
;  (SETF (BUFFER-PATHNAME BUFFER) (SEND PATHNAME :TRANSLATED-PATHNAME))
;  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) (SEND PATHNAME :GENERIC-PATHNAME))
;  (SETQ STRING (SEND PATHNAME :STRING-FOR-EDITOR))
;  (COND ((EQUALP (BUFFER-NAME BUFFER) STRING)
;        NIL)
;       ((CL:ASSOC STRING *ZMACS-BUFFER-NAME-ALIST* :TEST #'EQUALP)
;        (FORMAT *QUERY-IO* "~&Not renaming the buffer!  There is already a buffer named ~A"
;                STRING))
;       (T
;        (SIMILAR-BUFFER-FILES-WARNING BUFFER)
;        (SEND BUFFER :RENAME STRING)
;;Should no longer be necessary.
;;       ;; Transfer the attribute list info to the new pathname.
;;       (LET ((PKG (BUFFER-PACKAGE BUFFER)))
;;         (REPARSE-BUFFER-attribute-list BUFFER)
;;         (SETF (BUFFER-PACKAGE BUFFER) PKG))
;        )))

;(DEFUN SIMILAR-BUFFER-FILES-WARNING (BUFFER &AUX SAME-NAME SAME-TYPE SAME-EVERYTHING)
;  "Warn if any buffer other than BUFFER is visiting the same or a similar file."
;  (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
;    (AND (NEQ (CDR ELT) BUFFER)
;        (BUFFER-PATHNAME (CDR ELT))
;        (BUFFER-FILE-ID (CDR ELT))
;        (NOT (NODE-SPECIAL-TYPE (CDR ELT)))
;        (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) :STRING-FOR-EDITOR)
;                    (SEND (BUFFER-PATHNAME (CDR ELT)) :STRING-FOR-EDITOR))
;            (RETURN (SETQ SAME-EVERYTHING (CDR ELT)))
;          (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) :NAME)
;                      (SEND (BUFFER-PATHNAME (CDR ELT)) :NAME))
;              (COND ((EQUALP (SEND (BUFFER-PATHNAME BUFFER) :TYPE)
;                             (SEND (BUFFER-PATHNAME (CDR ELT)) :TYPE))
;                     (SETQ SAME-TYPE (CDR ELT)))
;                    (T (SETQ SAME-NAME (CDR ELT))))))))
;  (IF SAME-EVERYTHING
;      (FORMAT *QUERY-IO* "~&Warning: Buffer ~A~&  is also visiting file ~A."
;             (BUFFER-NAME SAME-EVERYTHING) (BUFFER-PATHNAME SAME-EVERYTHING))
;    (LET ((LOSER (OR SAME-TYPE SAME-NAME)))
;      (IF LOSER
;         (FORMAT *QUERY-IO* "~&Note: Another buffer ~A~&  is visiting file ~A."
;                 (BUFFER-NAME LOSER) (BUFFER-PATHNAME LOSER))))))

;(DEFUN SET-BUFFER-FILE-ID (BUFFER INFO)
;  "Set the BUFFER-FILE-ID of BUFFER to INFO.
;Records the file BUFFER was last read or saved in."
;  (SETF (BUFFER-FILE-ID BUFFER) INFO)
;  (LET ((VERSION-STRING (AND (TYPEP (CAR-SAFE INFO) 'FS:PATHNAME)
;                            (BUFFER-PATHNAME BUFFER)
;                            (NOT (NUMBERP (SEND (BUFFER-PATHNAME BUFFER) :VERSION)))
;                            (LET ((VERSION (SEND (CAR INFO) :VERSION)))
;                              (AND (NUMBERP VERSION) (FORMAT NIL " (~D)" VERSION))))))
;    (SETF (BUFFER-VERSION-STRING BUFFER) VERSION-STRING)
;    (AND (EQ BUFFER *INTERVAL*) (SETQ *ZMACS-BUFFER-VERSION-STRING* VERSION-STRING)))
;  INFO)

;(defun buffer-file-version-if-known (buffer)
; "NIL or the version of the truename associated with this buffer"
; ;--unfortunately, the truename is not stored directly, it should be. --rg.
;  (let ((info (buffer-file-id buffer)))
;    (if (and (memq (type-of (car-safe info)) '(fs:pathname fs:lm-pathname))
;            (buffer-pathname buffer))
;       (let ((v (send (buffer-pathname buffer) :version)))
;         (if (numberp v)
;             v
;             (send (car info) :version))))))

;
;(DEFCOM COM-REVERT-BUFFER "Forgets changes to a specified buffer.
;Reads the name of the buffer from the mini-buffer and reads back in the file
;or function." ()
;  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to revert:" *INTERVAL*)))
;    (if (not *numeric-arg*)
;       (progn (REVERT-BUFFER BUFFER)
;              (MUST-REDISPLAY-BUFFER BUFFER DIS-TEXT)
;              DIS-NONE)
;      (let* ((assoc-file (condition-case () (send buffer :pathname)
;                          (fs:file-not-found (warn "The buffer ~A has no associated pathname."))))
;            (truename (send assoc-file :truename))
;            (most-recent-file

;(DEFUN REVERT-BUFFER (BUFFER &OPTIONAL (PATHNAME (BUFFER-PATHNAME BUFFER))
;                                      (CONNECT-FLAG (BUFFER-FILE-ID BUFFER))
;                                      SELECT-FLAG
;                                      QUIETLY-FLAG)
;  "Read file PATHNAME, or BUFFER's visited file into BUFFER.
;CONNECT-FLAG non-NIL means mark BUFFER as visiting the file.
; This may change the buffer's name.
; It defaults non-NIL if BUFFER is visiting a file now.
;If CONNECT-FLAG is NOSECTIONIZE, mark buffer as visiting but don't sectionize it.
;SELECT-FLAG non-NIL means select BUFFER.
;QUIETLY-FLAG means do not print a message about reading a file."
;  (SEND BUFFER :REVERT PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG))

;;; Only the :REVERT method for FILE-BUFFER calls this.
;(DEFUN REVERT-FILE-BUFFER (BUFFER PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG
;                          &AUX GENERIC-PATHNAME PATHNAME-STRING TRUENAME NEW-MODE)
;  (WHEN (AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
;    (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER)))
;  (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
;    (EDITOR-FILE-NAME PATHNAME))
;  (WHEN CONNECT-FLAG
;    (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
;    (SETF (BUFFER-PATHNAME BUFFER) PATHNAME))
;  (SETQ GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
;  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
;  (WITH-OPEN-FILE-CASE (STREAM PATHNAME :CHARACTERS T)
;    (:NO-ERROR
;     (SETQ TRUENAME (SEND STREAM :TRUENAME))
;     (WHEN (MEMQ (SEND PATHNAME :TYPE) '(NIL :UNSPECIFIC))
;       (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
;        (EDITOR-FILE-NAME
;          (IF (EQUALP (SEND TRUENAME :NAME) (SEND PATHNAME :NAME))
;              ;; This is in case user reads FOO > from an ITS, and it is reall FOO BAR.
;              (SEND PATHNAME :NEW-TYPE (SEND TRUENAME :TYPE))
;            ;; This case if user read FOO BAR from an LMFILE, and truename is FOO|BAR.
;            ;; Or if user reads FOO BAR from an ITS and it is a link to UGH QUUX.
;            PATHNAME))))
;     (WHEN CONNECT-FLAG
;       (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
;       (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
;       (SIMILAR-BUFFER-FILES-WARNING BUFFER))
;     (WHEN (NOT QUIETLY-FLAG)
;       (FORMAT *QUERY-IO* "~&Reading ~A" TRUENAME)
;       (LET ((THIS-VERSION (SEND TRUENAME :VERSION))
;            (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
;            INSTALLED-VERSION)
;        (AND INSTALLED-TRUENAME
;             (NUMBERP THIS-VERSION)
;             (NUMBERP (SETQ INSTALLED-VERSION (SEND INSTALLED-TRUENAME :VERSION)))
;             ( INSTALLED-VERSION THIS-VERSION)
;             (FORMAT *QUERY-IO* " (installed version is ~D)" INSTALLED-VERSION))))
;     (FS:READ-ATTRIBUTE-LIST BUFFER STREAM)
;     ;; Forget (and thereby override) and previouse Set Package in this buffer.
;     (SETF (BUFFER-PACKAGE BUFFER) NIL)
;     ;; And recompute from latest attribute list.
;     (INITIALIZE-BUFFER-PACKAGE BUFFER)
;     (UNLESS (SEND BUFFER :GET-ATTRIBUTE ':MODE)
;       (SEND BUFFER :SET-ATTRIBUTE ':MODE
;                                  (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
;                                                           FS:*FILE-TYPE-MODE-ALIST*))
;                                      *DEFAULT-MAJOR-MODE*)))
;     (SETQ NEW-MODE (OR (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE ':MODE))
;                       'FUNDAMENTAL-MODE))
;     (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
;       (IF (EQ BUFFER *INTERVAL*)
;          (COMPUTE-BUFFER-PACKAGE BUFFER))
;       (AND NEW-MODE (SEND BUFFER :SET-MAJOR-MODE NEW-MODE)))
;     (PRESERVE-BUFFER-POINT (BUFFER)
;       (WITH-READ-ONLY-SUPPRESSED (BUFFER)
;        (LET ((*BATCH-UNDO-SAVE* T))           ;Don't save all this for undo!
;          (DISCARD-UNDO-INFORMATION BUFFER)
;          (DELETE-INTERVAL BUFFER)
;          (SETF (BUFFER-TICK BUFFER) (TICK))   ;For SECTIONIZE-BUFFER
;          (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
;          (LET ((FONTS (SET-BUFFER-FONTS BUFFER))
;                FONTS-P)
;            (SETQ FONTS-P (OR (CDR FONTS) (SEND BUFFER :GET-ATTRIBUTE ':DIAGRAM)))
;            (WHEN SELECT-FLAG
;              (SEND BUFFER :ACTIVATE)
;              (MAKE-BUFFER-CURRENT BUFFER)
;              ;; If it is requested, read in the first screenful and then redisplay.
;              (DOTIMES (I (+ 5 (WINDOW-N-PLINES *WINDOW*)))
;                (MULTIPLE-VALUE-BIND (LINE EOFFLG)
;                    (SEND STREAM :LINE-IN LINE-LEADER-SIZE)
;                  (WHEN LINE
;                    (INSERT-LINE-WITH-LEADER LINE
;                                             (BP-LINE (INTERVAL-LAST-BP BUFFER))))
;                  (IF EOFFLG (RETURN))))
;              (REDISPLAY *WINDOW* :START (INTERVAL-FIRST-BP BUFFER) NIL))
;            (IF (NOT CONNECT-FLAG)
;                (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
;              (IF (EQ CONNECT-FLAG 'NOSECTIONIZE)
;                  (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
;                (SECTIONIZE-FILE-BUFFER BUFFER *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
;                                        NIL NIL
;                                        STREAM FONTS-P))
;              (SET-BUFFER-FILE-ID BUFFER (SEND STREAM :INFO))
;              (DOLIST (WINDOW (SEND BUFFER :WINDOWS))
;                (AND FONTS
;                     (REDEFINE-FONTS WINDOW
;                                     FONTS (SEND BUFFER :GET-ATTRIBUTE ':VSP)))
;                (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
;                                                   (SEND BUFFER :GET-ATTRIBUTE ':BACKSPACE))
;                (REDEFINE-WINDOW-TAB-NCHARS WINDOW
;                                            (SEND BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))))
;            (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
;            (NOT-MODIFIED BUFFER)))))
;     (UNLESS SELECT-FLAG                       ;else already done above
;       (SEND BUFFER :ACTIVATE))
;     (UNLESS QUIETLY-FLAG
;       (LET ((NCHARS (SEND-IF-HANDLES STREAM :READ-POINTER)))
;        (COND ((NULL NCHARS)
;               (FORMAT *QUERY-IO* " -- done."))
;              ((< NCHARS 5000.)
;               (FORMAT *QUERY-IO* " -- ~D characters." NCHARS))
;              (T (FORMAT *QUERY-IO* " -- ~DK characters." (ROUND NCHARS 1024.)))))))
;    (FS:FILE-NOT-FOUND
;     (WHEN *FIND-FILE-NOT-FOUND-IS-AN-ERROR* (BARF STREAM))
;     (OR QUIETLY-FLAG (FORMAT *QUERY-IO* "(New File)"))
;     (LET ((*BATCH-UNDO-SAVE* T))
;       (DISCARD-UNDO-INFORMATION BUFFER)
;       (DELETE-INTERVAL BUFFER))
;     (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
;     (SEND BUFFER :SET-ATTRIBUTE ':MODE
;                                (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
;                                                         FS:*FILE-TYPE-MODE-ALIST*))
;                                    *DEFAULT-MAJOR-MODE*))
;     (SETF (BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*)))
;     (LET ((MODE (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE :MODE))))
;       (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
;        (IF (EQ BUFFER *INTERVAL*) (COMPUTE-BUFFER-PACKAGE BUFFER))
;        (AND MODE (SEND BUFFER :SET-MAJOR-MODE MODE)))))
;    (FS:FILE-ERROR (BARF STREAM)))
;  (SETF (BUFFER-TICK BUFFER) (TICK)))          ;Buffer is same as file

;(DEFUN FILE-LOADED-TRUENAME (PATHNAME)
;  "Return the truename of the source of the version of PATHNAME which was LOADed."
;  (OR (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
;            (SOURCE-PATHNAME (SEND GENERIC-PATHNAME :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)))
;       (COND ((STRINGP SOURCE-PATHNAME)                ;Old versions of the compiler
;              (SETQ SOURCE-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS SOURCE-PATHNAME PATHNAME)))
;             ((CONSP SOURCE-PATHNAME)
;              (SETQ SOURCE-PATHNAME (FS:PATHNAME-FROM-COLD-LOAD-PATHLIST SOURCE-PATHNAME))))
;       (AND (NOT (NULL SOURCE-PATHNAME))
;            (LET ((TYPE-1 (SEND SOURCE-PATHNAME :TYPE))
;                  (TYPE-2 (SEND PATHNAME :TYPE)))
;              (OR (EQUAL TYPE-1 TYPE-2)
;                  (AND (OR (EQ TYPE-1 :UNSPECIFIC)
;                           (CL:MEMBER TYPE-1 FS:*ITS-UNINTERESTING-TYPES* :TEST #'STRING=))
;                       (OR (EQ TYPE-2 :UNSPECIFIC)
;                           (CL:MEMBER TYPE-2 FS:*ITS-UNINTERESTING-TYPES* :TEST #'STRING=)))))
;            SOURCE-PATHNAME))
;      (LET* ((NEWEST-PATHNAME (SEND PATHNAME :NEW-VERSION :NEWEST))
;            (ID (SI:GET-FILE-LOADED-ID NEWEST-PATHNAME PACKAGE)))
;       (AND ID (CAR ID)))))




(DEFCOM COM-SOURCE-COMPARE-MERGE-CHANGES
       "Compare two files or buffers and merge the differences into the specified buffer" ()
       (LET (FILE-1 FILE-2 NAME-1 NAME-2 TYPE-1 TYPE-2 BUF-1 BUF-2 DEFAULT OUTPUT-BUFFER)
         (UNWIND-PROTECT
             (PROGN
               (MULTIPLE-VALUE (FILE-1 NAME-1 TYPE-1 DEFAULT BUF-1)
                 (LET* ((NAME (BUFFER-NAME *INTERVAL*)))
                   (VALUES (SRCCOM::MAKE-FILE
                             :FILE-NAME NAME
                             :FILE-TYPE "Buffer"
                             :FILE-STREAM (INTERVAL-STREAM *INTERVAL*)
                             :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
                                                        (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE *INTERVAL*)))
                                                      SI:PKG-KEYWORD-PACKAGE))
                           NAME
                           "BUFFER"
                           (AND (BUFFER-FILE-ID *INTERVAL*) (BUFFER-PATHNAME *INTERVAL*))
                           *INTERVAL*)))
               (MULTIPLE-VALUE (FILE-2 NAME-2 TYPE-2 NIL BUF-2)
                 (let ((bp (buffer-pathname *interval*)))
                   (VALUES (SRCCOM::CREATE-FILE bP)
                           bp bp "File" bp)))
               (SETQ OUTPUT-BUFFER (READ-BUFFER-NAME "Put merged version into buffer"
                                                     (OR BUF-1 BUF-2) T))
               (MAKE-BUFFER-CURRENT OUTPUT-BUFFER)
               (LET ((INTERVAL (CREATE-INTERVAL))
                     (*BATCH-UNDO-SAVE* T)
                     MARKS)
                 (SETQ MARKS (SRCCOM::SOURCE-COMPARE-AUTOMATIC-MERGE-RECORDING
                               FILE-1 FILE-2 (INTERVAL-STREAM INTERVAL)))
                 (REPLACE-INTERVALS OUTPUT-BUFFER INTERVAL)
                 (BIND-MODE-LINE `("Source Compare Merge " ,NAME-1 " vs " ,NAME-2)
                   (SOURCE-COMPARE-MERGE-QUERY MARKS))
                 (FORMAT *QUERY-IO* "~&Done.  Resectionizing the buffer."))
               (COM-REPARSE-ATTRIBUTE-LIST)
               (SECTIONIZE-BUFFER OUTPUT-BUFFER)
               ;; If one input is a file, and the other is the output buffer, and
               ;; the file is the one in the buffer, then update the buffer's file-id
               ;; as if it had been read in from the file.
               (IF BUF-1 (PSETQ BUF-1 BUF-2 BUF-2 BUF-1
                                NAME-1 NAME-2 NAME-2 NAME-1
                                FILE-1 FILE-2 FILE-2 FILE-1))
               (AND (NULL BUF-1) (EQ BUF-2 OUTPUT-BUFFER)
                    (EQ NAME-1 (BUFFER-PATHNAME BUF-2))
                    (SET-BUFFER-FILE-ID BUF-2 (SEND (SRCCOM::FILE-STREAM FILE-1) :INFO))))
           (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
           (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
       DIS-NONE)

(defmacro WITH-VERSION-AND-MODIFICATION-INFO (buffer body)
  (declare (zwei:indentation 1 1))
  `(block no-file
     (let* ((buffer-pathname (zwei:buffer-pathname ,buffer))
          (FILE-TRUENAME (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
     (when (NOT FILE-TRUENAME)
       (return-from no-file
         (format t "~%Cannot find any versions of ~A on disk."
                 buffer-PATHNAME)))
         ;;without the numeric version number if exists in buffer name
     (and buffer-pathname
          ;; none of these have any relevance if there is no file associated with
          ;; <buffer>.
          (let* ((generic-pathname (buffer-generic-pathname ,buffer))
                 (buffer-namestring (namestring buffer-pathname))
                 (buffer-version (or (buffer-file-version-if-known ,buffer)
                                     ;;this is a universal-time, given to all files, whether they have
                                     ;;version numbers or not.  Here it becomes a quasi-version number.
                                     (get (cadr (send buffer-pathname :directory-list nil)) :creation-date)))
                 (file-version (pathname-version buffer-pathname))
                 (file-number (if (numberp file-version)
                                  file-version
                                (pathname-version file-truename)))
                 ;;anything is newer than a non-existent buffer version, and
                 ;;file version numbers are assumed to be positive integers
                 (newer-version-exists (and file-number (< (or buffer-version -1) file-number)))
                 ;;(this-version-is-newest (= buffer-version file-number))
                 (buffer-modified (buffer-needs-saving-p ,buffer)))
            ,body)))))

(defun fquery-unless-arg (return-keywords echo-strings activators user-string &rest string-args)
  ;; This defines an internal function which returns a special symbol if *numeric-arg* has been
  ;; provided; if not, it takes creates a dispatching template using FQUERY.  This template
  ;; will, when invoked at various points in refind-file-1, be used to determine branching
  ;; scenarios at those points.
  (let ((*query-io* *standard-output*))
    (if *numeric-arg-p*
        :GO-AHEAD
      (loop for each-key in return-keywords
            and for each-string in echo-strings
            and for each-char in activators
            with choice-list initially nil
            do
            (let* ((this-choice (list (list each-key each-string) each-char)))
              (setq choice-list (append choice-list (list this-choice))))
            finally
            (return
              (apply
                #'fquery
                (append
                  (list (list :type :tyi :choices choice-list))
                  (list user-string)
                  string-args)))))))


(defun replace-current-buffer-with-file (pathname)
  (without-interrupts
      (setq *zmacs-buffer-name-alist*
            (lisp:remove *interval* *zmacs-buffer-name-alist* :key 'cdr))
      (send *interval* :kill)
      (let* ((new-buffer
               (load-file-into-zmacs pathname)))
        (send new-buffer :select))))

(defun REFIND-FILE (&optional (buffer (read-buffer-name "File to refind: "
                                                        *interval*)))
  ;;  This will give users choices unless they specify (by using com-refind-file with
  ;; an explicit numeric arg) that they want REFIND-FILE to act independently of user
  ;; input.
  (if (not (buffer-pathname buffer))
      (format t "The buffer ~S is not associated with a file." buffer)
    (if (eq (buffer-saved-major-mode buffer) 'dired-mode)
        (without-interrupts
          (format *query-io* "Updating Dired display...")
          (directory-edit-revert buffer)
          (setq *zmacs-buffer-name-alist*
                (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
          (zl:format *query-io* "Dired display current as of ~\\datime\\." (time:get-universal-time)))
      (with-version-and-modification-info buffer
        (cond (buffer-modified
               (cond ((numberp file-version)
                      (if (let ((*query-io* *standard-output*))
                            (yes-or-no-p "The buffer associated with file ~S,~
                           ~%read in with explicit numeric version, has been modified.~
                           ~%Write out the changed version to a file?~% " buffer-namestring))
                          (com-write-file))
                      (selectq (fquery-unless-arg
                                '(:FIND-LATEST :REVERT :NO-ACTION)
                                '("Replacing buffer contents with #> version" "Reverting buffer..." "Aborting")
                                '(#\F #\R #\D)
                                "~%The current buffer contains modifications of version ~S of ~S.~
                             ~%Choose an option:~
                             ~%~% (F) Find and read into the current buffer the #> version of ~:*~S,~
                               ~%     (changing the buffer name to end with \">\" as well),~
                             ~%~% (R) Revert buffer to version ~2:*~S,~
                             ~%~% (D) Do nothing.
                             ~%~% "
                                file-version generic-pathname)
                        (:FIND-LATEST
                         ;; A defun and not a defcom; no automatic redisplay makes sure user reads sees
                         ;; the echo string returned by fquery choice...
                         (replace-current-buffer-with-file  (merge-pathnames "#>" buffer-pathname)))
                        (:REVERT (revert-buffer buffer))
                        (:NO-ACTION nil)
                        (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"))))
                     ;; Non-numeric version (unix file, #> file, etc...)
                     (newer-version-exists
                      (selectq (fquery-unless-arg
                                '(:FIND-LATEST :SRCCOM :SRCCOM-MERGE :NO-ACTION)
                                '("Replacing buffer's contents with #> version" "Do a Source Compare"
                                  "Do a Source Compare Merge" "Aborting")
                                '(#\F #\S #\M #\D)
                                "The current buffer contains modifications, but a newer version of ~S~
                           ~%now exists on disk.  Choose an option:~
                           ~%~%(F) Find and read in the latest version of the file named ~:*~S,~
                           ~%~%(S) Source Compare the current contents of this buffer with the contents of the current~
                           ~%disk file named ~:*~S,~
                           ~%~%(M) Source Compare Merge the current contents of this buffer with the contents of the current~
                           ~%disk file named ~:*~S, or~
                           ~%~%(D) Do nothing~%~%" buffer-namestring)
                        ((:GO-AHEAD :FIND-LATEST)
                         (find-file buffer-namestring))
                        (:SRCCOM (let ((*numeric-arg-p* t))
                                   (com-source-compare-changes)))
                        (:SRCCOM-MERGE (com-source-compare-merge-changes))
                        (:NO-ACTION nil)))
                     ;;modified version of the real #> version (i.e., no newer version exists)
                     (t (selectq (fquery-unless-arg
                                  '(:REVERT :SRCCOM :SRCCOM-MERGE :NO-ACTION)
                                  '("Revert buffer" "Do a Source Compare" "Do a Source Compare Merge" "Aborting")
                                  '(#\R #\S #\M #\D)
                                  "The current buffer contains modifications, and no newer version of~
                             ~%~S exists on disk.  Choose an option:~
                           ~%~%  (R) Revert the buffer~
                           ~%~%  (S) Source Compare the buffer with the contents of~
                             ~%      the file currently named ~:*~S~
                           ~%~%  (M) Source Compare Merge the buffer with the contents of~
                             ~%      the file currently named ~:*~S~
                           ~%~%  (D) Do nothing~%~%" buffer-namestring)
                          ((:GO-AHEAD :REVERT)
                           (revert-buffer buffer))
                          (:SRCCOM (let ((*numeric-arg-p* t))
                                     (com-source-compare-changes)))
                          (:SRCCOM-MERGE (com-source-compare-merge-changes))
                          (:NO-ACTION nil)))))
              ;;if we get here, then buffer not modified.
              (t (cond ((numberp file-version)
                        (selectq (fquery-unless-arg
                                  '(:FIND-LATEST :REVERT :NO-ACTION)
                                  '("Replacing buffer contents with #> version" "Reverting buffer..." "Aborting")
                                  '(#\F #\R #\D)
                                  "The current buffer contains version ~S of ~S, but a newer version of~
                           ~%~1@*~S now exists on disk.  Choose an option:~
                           ~%~% (F) Find and read into the current buffer the > version of ~:*~S,~
                             ~%     (changing the buffer name to end with \">\" as well),~
                           ~%~% (R) Revert buffer to version ~2:*~S,~
                           ~%~% (D) Do nothing.~%~% "
                                  file-version generic-pathname)
                          ((:FIND-LATEST)
                           (replace-current-buffer-with-file (merge-pathnames "#>" buffer-pathname)))
                          (:REVERT (revert-buffer buffer))
                          (:NO-ACTION nil)
                          (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"
                                           buffer-pathname))))
                       ;; Non-numeric version (unix file, #> file, etc...)
                       (newer-version-exists
                        (revert-buffer buffer))
                       (t nil)))))))))

(defcom com-refind-all-files "Revert or update all buffers to contain their most recent versions." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (refind-file each-buffer))
  dis-all)

(defcom com-refind-file "Revert or update the current buffer to contain the most recent version." ()
  (refind-file *interval*)
  dis-all)

(defcom com-revert-all-buffers "Send a :REVERT message to all active buffers."

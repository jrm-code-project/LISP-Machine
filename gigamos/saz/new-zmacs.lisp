;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file provides the initial simple buffer/file stuff for ZWEI.
;;; It uses the utility file stuff in ZWEI; FILES.
;;; Some of the section specific functions are in ZWEI; SECTIO.

;;; A buffer may be in any of the following states, based on the BUFFER-FILE-ID:
;;; * BUFFER-FILE-ID is NIL.  There is no corresponding file.  The group symbol
;;;    is a gensym.  BUFFER-NAME is simply considered the name of the buffer,
;;;    and BUFFER-TICK is the time the buffer was created.
;;; * BUFFER-FILE-ID is T.  There is a file, but no I/O has been done to it.
;;;    That is, it is a "(New File)".  The BUFFER-GENERIC-PATHNAME is for real.
;;;    The buffer name is the file name. BUFFER-TICK is the time the file was created.
;;; * BUFFER-FILE-ID is a list.  There is a file, we have done I/O to it.
;;;    The last I/O was at time BUFFER-TICK; at that time the copy on disk
;;;    was the same as the copy in the machine.  the BUFFER-NAME is the name
;;;    of the file, and the BUFFER-GENERIC-PATHNAME is for real.

;;; The fundamental operations to provide are:
;;; NOTE: these are not the names or calling sequences of the real functions.
;;; c-X c-F: (FIND-FILE <pathname>).  Select or create appropriate buffer.
;;;           Merges the pathname default with argument.  Reads in file if
;;;           creting and the file exists.
;;; c-X B:   (FIND-BUFFER <buffer-name>)  Select or create appropriate buffer.
;;; c-X c-W: (WRITE-BUFFER <buffer> <pathname>)  Write to specified file, altering the
;;;           BUFFER-NAME if asked to.
;;; c-X c-S: (SAVE-BUFFER <buffer>)  Write to its file.  If ID is NIL, turn into WRITE-BUFFER.
;;; Revert:  (REVERT-BUFFER <buffer>)  Read in from most recent version.  If ID = NIL,
;;;           error.
;;; Not Modified: (NOT-MODIFIED <buffer>)  Forget this was modified, by making TICK
;;;           be the present time.
;;; c-X c-B: (LIST-BUFFERS)  List all buffers.
;;; c-X K:   (KILL-BUFFER <buffer>)  Kills it.
;;; Save All Files: (obvious)

;;;; Utility functions.

;;; Changed: activate-p argument flushed.
(DEFUN CREATE-BUFFER ()
  "Create an empty buffer.  This function is obsolete.
Does not set the buffer name, pathnames, or tick,
so you must fix them up before you are done."
  (MAKE-INSTANCE 'ZMACS-BUFFER))

(compiler:make-obsolete create-buffer "perhaps you should be using ZWEI:CREATE-ONE-BUFFER-TO-GO ?")

(DEFUN CREATE-ONE-BUFFER-TO-GO (&OPTIONAL (NAME (GENERATE-BUFFER-NAME)))
  "Create and return an empty buffer named NAME.
The buffer is put on the list of ZMACS buffers and is ready to select."
  (LET ((BUFFER (MAKE-INSTANCE 'ZMACS-BUFFER :NAME NAME)))
    (SEND BUFFER :ACTIVATE)
    BUFFER))

;;;GENERATE-BUFFER-NAME moved to DEFS for use by flavor methods -KmC 5/88

(DEFUN FIND-BUFFER-NAMED (NAME &OPTIONAL CREATE-P)
  "Return the buffer named NAME, or NIL if none.
CREATE-P non-NIL says create one if there is none.
If a file buffer is intended, use a PATHNAME object, not a string."
  (OR (STRINGP NAME)
      (SETQ NAME (SEND NAME :STRING-FOR-EDITOR)))
  (DO ((L *ZMACS-BUFFER-LIST* (CDR L)))
      ((NULL L)
       (AND CREATE-P (CREATE-ONE-BUFFER-TO-GO NAME)))
    (AND (STRING-EQUAL (BUFFER-NAME (CAR L)) NAME)
         (RETURN (CAR L)))))

(DEFUN FIND-FILE-BUFFER (PATHNAME)
  "Return the buffer visiting PATHNAME, or NIL if none."
  (SETQ PATHNAME (SEND PATHNAME :TRANSLATED-PATHNAME))
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (FS:PATHNAME-EQUAL PATHNAME (BUFFER-PATHNAME BUFFER))
         (BUFFER-FILE-ID BUFFER)  ;Make sure it's a buffer really associated with a file.
         (NOT (NODE-SPECIAL-TYPE BUFFER))  ;Not DIRED buffers either.
         (RETURN BUFFER))))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Select"
                          TYPEOUT-MAKE-BUFFER-CURRENT T "Select this buffer.")

(DEFVAR *READ-BUFFER-KLUDGE* NIL)

;Make BUFFER current, for execution from mouse-sensitive item.
(DEFUN TYPEOUT-MAKE-BUFFER-CURRENT (BUFFER)
  (COND ((GET BUFFER ':KILLED)
         (BARF "Buffer ~A has been killed." (BUFFER-NAME BUFFER)))
        ((NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
         (MAKE-BUFFER-CURRENT BUFFER))
        (*READ-BUFFER-KLUDGE*
         ;; We are inside READ-BUFFER-NAME.  Arrange for the minibuffer edit
         ;; to return this buffer.  Also put the buffer name in the minibuffer interval
         ;; so that the right things are recorded for C-X .
         (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
         (INSERT (INTERVAL-FIRST-BP (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
                 (BUFFER-NAME BUFFER))
         (*THROW 'RETURN-FROM-COMMAND-LOOP BUFFER))
        (T
         (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
         (*THROW 'TOP-LEVEL T))))

(DEFUN TYPEOUT-ABORT-MINI-BUFFER ()
  "If in the mini buffer, abort it and unread last input character.
This should be called from the handler of a mouse-sensitive item
if it wants to switch buffers or files, or for other reasons
will not work in the minibuffer.
This exits the mini buffer and tries the command over."
  (WHEN (EQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
    (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
    (THROW 'TOP-LEVEL T)))

(DEFUN MAKE-BUFFER-CURRENT (BUFFER &OPTIONAL PRESERVE-BUFFER-HISTORY)
  "Make BUFFER the current ZMACS buffer in the selected window.
PRESERVE-BUFFER-HISTORY non-NIL says do not reorder the buffers for C-M-L, etc."
  (CHECK-TYPE BUFFER ZMACS-BUFFER)
  ;; Save away the major and minor modes, and turn them off.
  (WHEN *INTERVAL*
    (SETF (BUFFER-SAVED-MODE-LIST *INTERVAL*) *MODE-LIST*)
    (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) *MAJOR-MODE*)
    (SETF (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)
          (MAPCAR #'(LAMBDA (V)
                      (CONS V (IF (MEMQ V *LOCAL-BOUND-VARIABLES*)
                                  (%P-CONTENTS-AS-LOCATIVE (VALUE-CELL-LOCATION V))
                                (LIST (SYMEVAL V)))))
                  *LOCAL-VARIABLES*))
    (UN-SET-MODES)
    ;; Update *ZMACS-BUFFER-LIST*, for C-X C-B,
    ;; and this window's buffer history, for C-M-L.
    (UNLESS PRESERVE-BUFFER-HISTORY
      (UPDATE-BUFFER-HISTORY *INTERVAL* BUFFER)))
  ;; Point the window at this interval, and make it the default interval.
  ;; If called from the two-window commands, the window may already be
  ;; pointing to this buffer, in which case don't change it
  (OR (EQ (WINDOW-INTERVAL *WINDOW*) BUFFER)
      (SEND *WINDOW* :SET-INTERVAL-INTERNAL BUFFER))
  (SETQ *INTERVAL* BUFFER)
  (UPDATE-BUFFER-NAMES BUFFER)
  ;; Recompute which package READs should be done in.
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  ;; Restore the old major and minor modes.
  (SET-MODES (BUFFER-SAVED-MODE-LIST *INTERVAL*) (BUFFER-SAVED-MAJOR-MODE *INTERVAL*)
             (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*))
  NIL)

;;; This updates *WINDOW*'s buffer history, which is used for defaulting args
;;; and for C-M-L in this window, and updates *ZMACS-BUFFER-LIST*,
;;; which is used by C-X C-B.
(DEFUN UPDATE-BUFFER-HISTORY (OLD-BUFFER NEW-BUFFER)
  "Update lists of recently selected buffers when NEW-BUFFER is selected.
OLD-BUFFER should be the buffer that used to be selected."
  (AND (NEQ OLD-BUFFER NEW-BUFFER)
       (WITHOUT-INTERRUPTS
         (PUSH-REMOVE-ON-HISTORY NEW-BUFFER
                                 (SEND *WINDOW* :BUFFER-HISTORY))
         (WHEN (NEQ NEW-BUFFER (CAR *ZMACS-BUFFER-LIST*))
           (SETQ *ZMACS-BUFFER-LIST*
                 (CONS NEW-BUFFER (REMQ NEW-BUFFER *ZMACS-BUFFER-LIST*)))))))

;;; This updates *ZMACS-BUFFER-NAME* and *ZMACS-BUFFER-NAME-ALIST*
(DEFUN UPDATE-BUFFER-NAMES (BUFFER &OPTIONAL NAME &AUX TEM)
  (OR NAME (SETQ NAME (BUFFER-NAME BUFFER)))
  (WITHOUT-INTERRUPTS
    (IF (SETQ TEM (SI:RASSOC-EQUAL BUFFER *ZMACS-BUFFER-NAME-ALIST*))
        (RPLACA TEM NAME)
      (PUSH (CONS NAME BUFFER) *ZMACS-BUFFER-NAME-ALIST*)))
  (SETQ *ZMACS-BUFFER-VERSION-STRING* (BUFFER-VERSION-STRING BUFFER)
        *ZMACS-BUFFER-NAME* NAME))

(DEFUN ROTATE-BUFFER-HISTORY (N)
  "Select the Nth buffer on the cyclic order of old selected buffers.
That list is rotated by this command."
  (LET ((HISTORY-LIST
          (if *history-buffer-lists-per-window*
              (HISTORY-CONTENTS (SEND *WINDOW* :BUFFER-HISTORY))
            *zmacs-buffer-list*)))
    (AND (> N (LENGTH HISTORY-LIST)) (BARF))
    (WITHOUT-INTERRUPTS
      (ROTATE-TOP-OF-LIST HISTORY-LIST N))
    (MAKE-BUFFER-CURRENT (CAR HISTORY-LIST)))
  DIS-TEXT)


(defmacro WITH-VERSION-AND-MODIFICATION-INFO (buffer body)
  ;; binds some variables to interesting properties of a buffer and its associated file.
  ;; Used in REFIND-FILE, below.
  (declare (zwei:indentation 1 1))
  `(block no-file
     (let* ((buffer-pathname (zwei:buffer-pathname ,buffer))
          (FILE-TRUENAME (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
     (when (NOT FILE-TRUENAME)
       (return-from no-file
         (format t "~%No versions of ~S found on disk."
                 BUFFER-PATHNAME)))
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

(DEFUN PATHNAME-DEFAULTS (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*) (BUFFER *INTERVAL*)
                          &AUX (MAJOR-MODE (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
                                             (SEND BUFFER :MAJOR-MODE)))
                          TEM)
  "Update DEFAULTS for BUFFER, and return it.
DEFAULTS is a defaults-alist.
We update it by setting the defaults in it
based on BUFFER's visited pathname, or its name."
  (AND (TYPEP BUFFER 'FILE-BUFFER)
       (FS:SET-DEFAULT-PATHNAME
         (OR (AND (SETQ TEM (GET MAJOR-MODE 'PATHNAME-DEFAULTING-FUNCTION))
                  (FUNCALL TEM DEFAULTS BUFFER))
             (AND (BUFFER-GENERIC-PATHNAME BUFFER)
                  (SEND (BUFFER-GENERIC-PATHNAME BUFFER) :SOURCE-PATHNAME))
             (LET ((TYPE (CAR (RASSQ (INTERN-SOFT
                                       (STRING-UPCASE
                                         (SYMEVAL MAJOR-MODE))
                                       "KEYWORD")
                                     FS:*FILE-TYPE-MODE-ALIST*)))
                   (PN (SEND (FS:DEFAULT-PATHNAME DEFAULTS)
                             :NEW-SUGGESTED-NAME
                             (BUFFER-NAME BUFFER))))
               (IF TYPE (SEND PN :NEW-TYPE TYPE) PN)))
         DEFAULTS))
  DEFAULTS)

(DEFUN DEFAULT-PATHNAME (&OPTIONAL (DEFAULTS *PATHNAME-DEFAULTS*))
  "Return a default pathname for a visit operation.
The default comes from the visited pathname or the buffer name."
  (FS:DEFAULT-PATHNAME (PATHNAME-DEFAULTS DEFAULTS)))

(DEFUN MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM (BUFFER-NAME CONCATENATE-P
                                               &OPTIONAL NO-WINDOWS
                                               &AUX BUFFER)
  "Make a stream reading and writing a buffer named BUFFER-NAME and writing a window as well.
If the buffer is selected in an exposed window, and NO-WINDOWS is NIL,
an editor-stream for that window is returned as both values.
Otherwise, an interval stream for the buffer is the second value
and a broadcast stream for that and *STANDARD-OUTPUT* is the first.
CONCATENTATE-P = NIL means clear out the buffer to begin with.
CONCATENTATE-P = :POINT means start reading//writing at point.
Otherwise starts at the end."
  (SETQ BUFFER (FIND-BUFFER-NAMED BUFFER-NAME T))
  (OR CONCATENATE-P (DELETE-INTERVAL BUFFER))
  (DO ((WINDOWS (UNLESS NO-WINDOWS (SEND BUFFER :WINDOWS))
                (CDR WINDOWS)))
      ((NULL WINDOWS)
       (LET ((ISTREAM (INTERVAL-STREAM BUFFER)))
         (SEND ISTREAM :SET-BP (IF (EQ CONCATENATE-P ':POINT)
                                   (BUFFER-SAVED-POINT BUFFER)
                                   (INTERVAL-LAST-BP BUFFER)))
         (VALUES (MAKE-BROADCAST-STREAM ISTREAM *STANDARD-OUTPUT*) ISTREAM)))
    (AND (SETQ WINDOWS (INTERSECTION WINDOWS (FRAME-EXPOSED-WINDOWS)))
         (LET ((WSTREAM (MAKE-EDITOR-STREAM-FROM-WINDOW (CAR WINDOWS))))
           (SETF (WINDOW-REDISPLAY-DEGREE (CAR WINDOWS))
                 (MAX (WINDOW-REDISPLAY-DEGREE (CAR WINDOWS)) DIS-TEXT))
           (OR (EQ CONCATENATE-P ':POINT)
               (MOVE-BP (SYMEVAL-IN-INSTANCE WSTREAM '*STREAM-BP*)
                        (INTERVAL-LAST-BP BUFFER)))
           (RETURN (VALUES WSTREAM WSTREAM))))))

(DEFUN MAKE-FILE-BUFFER-STREAM (PATHNAME &OPTIONAL (CONCATENATE-P T)
                                         &AUX BUFFER ISTREAM)
  "Return a stream reading and writing a buffer visiting file PATHNAME.
The file is read in if necessary.
CONCATENTATE-P = NIL means clear out the buffer to begin with.
CONCATENTATE-P = :POINT means start reading//writing at point.
Otherwise starts at the end."
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *PATHNAME-DEFAULTS*))
  (SETQ BUFFER (FIND-BUFFER-NAMED PATHNAME T))
  (IF (BUFFER-FILE-ID BUFFER)
      (OR CONCATENATE-P (DELETE-INTERVAL BUFFER))
    (LET ((*INTERVAL* NIL))
      (SET-BUFFER-FILE-ID BUFFER T))
    (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
    (SEND BUFFER :SET-ATTRIBUTE ':MODE
          (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE) FS:*FILE-TYPE-MODE-ALIST*))
              *DEFAULT-MAJOR-MODE*))
    (SETF (BUFFER-PACKAGE BUFFER)
          (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* PACKAGE)))
    (SETF (BUFFER-GENERIC-PATHNAME BUFFER) (SEND PATHNAME :GENERIC-PATHNAME)))
  (SETQ ISTREAM (INTERVAL-STREAM BUFFER NIL NIL NIL T))
  (SEND ISTREAM :SET-BP (IF (EQ CONCATENATE-P :POINT) (BUFFER-SAVED-POINT BUFFER)
                           (INTERVAL-LAST-BP BUFFER)))
  ISTREAM)

;;;; The commands.

(defcom com-create-one-buffer-to-go "Creates a new empty buffer." ()
  (make-buffer-current (create-one-buffer-to-go))
  dis-text)

(DEFCOM COM-FIND-FILE "Visits a file in its own buffer.
Reads in a filename from the minibuffer.  If the file is already
in a buffer, selects that buffer.  Otherwise creates a buffer
whose name is the name of the file, reads the file into
that buffer and selects it." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
                                           NIL NIL :NEW-OK)))
    (FIND-FILE PATHNAME))
  (MAYBE-DISPLAY-DIRECTORY :READ)
  DIS-TEXT)

(defun fquery-unless-arg (return-keywords echo-strings activators fquery-string-for-user &rest string-args)
  ;;  This function returns a special symbol if *numeric-arg* has been
  ;; provided to its caller; if not, it calls FQUERY with the lists provided.
  ;; A surrounding SELECTQ can catch :GO-AHEAD or any of the <RETURN-KEYWORDS>
  ;; provided.
  ;;  Caveat: This does no checking to see if args 1-3 are of equal length --
  ;; make sure that (= (length return-keywords) (,echo
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
                  (list fquery-string-for-user)
                  string-args)))))))

(defun REFIND-FILE (&optional (buffer (read-buffer-name "File to refind: "
                                                        *interval*))
                    &aux buffer-pathname)
  ;;  This will give users choices unless they specify (by using com-refind-file with
  ;; an explicit numeric arg) that they want REFIND-FILE to act independently of user
  ;; input.
  (if buffer (setq buffer-pathname (buffer-pathname buffer)))
  (cond ((null buffer)
         (load-file-into-zmacs
           (READ-DEFAULTED-PATHNAME "Find file:"
                                    (PATHNAME-DEFAULTS) NIL NIL :NEW-OK)))
        ((not buffer-pathname)
         (format t "~%The buffer ~A is not associated with a currently existing file." buffer))
        ((eq (buffer-saved-major-mode buffer) 'dired-mode)
         (without-interrupts
           (format t "~%Updating Dired display of ~A..." buffer-pathname)
           (directory-edit-revert buffer)
           (setq *zmacs-buffer-name-alist*
                 (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
           (zl:format t "Dired display of ~A updated." buffer-pathname)))
        (t
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
                                   '("Replacing buffer contents with #> version" "Reverting buffer..." "No action taken.")
                                   '(#\F #\R #\D)
                                   "~%The buffer ~S contains modifications to version ~S of ~S.~
                             ~%Choose an option:~
                             ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
                               ~%     (changing the buffer name to end with /">/" as well),~
                             ~%~% (R) Revert buffer to version ~2:*~S,~
                             ~%~% (D) Do nothing.
                             ~%~% "
                               buffer-namestring file-version generic-pathname)
                           (:FIND-LATEST
                            ;; A defun and not a defcom; no automatic redisplay makes sure user reads sees
                            ;; the echo string returned by fquery choice...
                            (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
                           (:REVERT (revert-buffer buffer))
                           (:NO-ACTION nil)
                           (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"))))
                        ;; Non-numeric version (unix file, #> file, etc...)
                        ;; Too bad -- except for the disk status user message, this clause and the
                        ;; (t ... ) clause are IDENTICAL, thanks to revert-buffer's behaviour in cases
                        ;; of non-numeric-version-numbered files.  The programmer who feels that telling the
                        ;; user about whether or not a new version exists on disk is unimportant
                        ;; could merge these two clauses (after unifying the text displayed).   -ds
                        (newer-version-exists
                         (selectq (fquery-unless-arg
                                   '(:FIND-LATEST :SRCCOM :SRCCOM-MERGE :NO-ACTION)
                                   '("Replac buffer's contents with #> version" "Do a Source Compare"
                                     "Do a Source Compare Merge" "No action taken.")
                                   '(#\F #\S #\M #\D)
                                   "The buffer ~S contains modifications, but a newer version of ~:*~S~
                           ~%now exists on disk.  Choose an option:~
                           ~%~%(F) Find and read into ZMACS the current > version of ~:*~S,~
                           ~%~%(S) Source Compare the current contents of this buffer with the contents of the current~
                           ~%disk file named ~:*~S,~
                           ~%~%(M) Source Compare Merge the current contents of this buffer with the contents of the current~
                           ~%disk file named ~:*~S, or~
                           ~%~%(D) Do nothing~%~%" buffer-namestring)
                           ((:GO-AHEAD :FIND-LATEST)
                            (format t "~%Updating buffer for ~S to more recent version..." buffer-pathname)
                            (revert-buffer buffer)
                            (format t "Buffer for ~S updated." buffer-pathname))
                           (:SRCCOM (let ((*numeric-arg-p* t))
                                      (source-compare-changes buffer)))
                           (:SRCCOM-MERGE (com-source-compare-merge-changes))
                           (:NO-ACTION nil)))
                        ;;modified version of the real #> version (i.e., no newer version exists)
                        (t (selectq (fquery-unless-arg
                                     '(:REVERT :SRCCOM :SRCCOM-MERGE :NO-ACTION)
                                     '("Revert buffer" "Do a Source Compare" "Do a Source Compare Merge" "No action taken.")
                                     '(#\R #\S #\M #\D)
                                     "The buffer ~S contains modifications, and no newer version of~
                             ~%~:*~S exists on disk.  Choose an option:~
                           ~%~%  (R) Revert the buffer to contain disk's latest version.~
                           ~%~%  (S) Source Compare the buffer with the contents of~
                             ~%      the file currently named ~:*~S~
                           ~%~%  (M) Source Compare Merge the buffer with the contents of~
                             ~%      the file currently named ~:*~S~
                           ~%~%  (D) Do nothing~%~%" buffer-namestring)
                             ((:GO-AHEAD :REVERT)
                              (format t "~%Reverting buffer ~S to its original state..." buffer-pathname)
                              (revert-buffer buffer)
                              (format t "Buffer for ~S reverted." buffer-pathname))
                             (:SRCCOM (let ((*numeric-arg-p* t))
                                        (source-compare-changes buffer)))
                             (:SRCCOM-MERGE (com-source-compare-merge-changes))
                             (:NO-ACTION nil)))))
                 ;;if we get here, then buffer not modified.
                 (t (cond ((numberp file-version)
                           (selectq (fquery-unless-arg
                                     '(:FIND-LATEST :REVERT :NO-ACTION)
                                     '("Replacing buffer contents with #> version" "Reverting buffer..." "No action taken.")
                                     '(#\F #\R #\D)
                                     "The buffer ~S contains modifications to version ~S of ~S, but a newer version of~
                           ~%~S now exists on disk.  Choose an option:~
                           ~%~% (F) Find and read into ZMACS the current > version of ~:*~S,~
                             ~%     (changing the buffer name to end with /">/" as well),~
                           ~%~% (R) Revert buffer to version ~2:*~S,~
                           ~%~% (D) Do nothing.~%~% "
                             buffer-namestring file-version generic-pathname)
                             ((:FIND-LATEST)
                              (replace-current-buffer-with-file buffer (merge-pathnames "#>" buffer-pathname)))
                             (:REVERT (revert-buffer buffer))
                             (:NO-ACTION nil)
                             (:GO-AHEAD (barf "~S is explicitly numeric.  Use Meta-x Revert Buffer or Meta-x Not Modified"
                                              buffer-pathname))))
                          ;; Non-numeric version (unix file, #> file, etc...)
                          (newer-version-exists
                           (format t "~%Reverting buffer ~S" buffer-namestring)
                           (revert-buffer buffer))
                          (t (format t "~%No updating required for ~S." buffer-namestring)))))))))

(defcom com-refind-file "Revert or update the current buffer to contain the most recent version." ()
  (refind-file *interval*)
  dis-all)

(defun refind-buffer-subset (keyword)
  (mapcar
    'refind-file
    (mapcar
      #'(lambda (x)
          (if (typep x 'zmacs-buffer)
              x
            (find-file-buffer x)))
      (selectq keyword
        (:TAG-TABLE
         (cadr
           (memq
             'zmacs-tag-table-file-symbols
             (plist
               (let ((tt
                       (completing-read-from-mini-buffer
                         "Tag table whose files to refind: (Current tag table is the default)"
                         *zmacs-tag-table-alist*)))
                 (cond ((or (and (consp tt) (not (string-equal (car tt) "")))
                            (string-equal tt ""))
                        (or *zmacs-current-tag-table* (barf "No tag table selected")))
                       (t (cdr tt))))))))
        (:SYSTEM
         (si:system-source-files
           (let ((sys (completing-read-from-mini-buffer "System whose files to refind"
                                                        (si:all-systems-name-alist))))
             (cond ((and (stringp sys) (string-equal sys ""))
                    (barf "No system selected"))
                   (t (cdr sys))))))
        (:SPECIFIED
         (multiple-value-bind (buffers-selected do-it?)
             (tv:multiple-menu-choose (subset #'(lambda (x) (buffer-pathname (cdr x)))
                                              *zmacs-buffer-name-alist*)
                                      "                 FILES TO REFIND:
 (Click on files to include them in the Refind list,
  Click Do It when finished. Move off menu to abort.) ")
           (if (null do-it?)
               ;;user moved off menu -- refind nothing
               (*throw 'REFIND-NOTHING dis-text)
             ;;those buffers the user selects
             (or buffers-selected (barf "No buffers selected")))))
        (:ALL *zmacs-buffer-list*)
        (:MODIFIED
          (subset #'(lambda (x) (buffer-modified-p x))
                  *zmacs-buffer-list*))
        (:NOT-MODIFIED
          (subset #'(lambda (x) (not (buffer-modified-p x)))
                  *zmacs-buffer-list*))
        (otherwise (ferror "Unknown buffer subset operation keyword: ~A" keyword))))))

(defcom com-refind-all-files "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :ALL)
  dis-all)

(defcom com-refind-files-in-system "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :SYSTEM)
  dis-all)

(defcom com-refind-tag-table-files "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :TAG-TABLE)
  dis-all)

(defcom com-refind-files-for-modified-buffers "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :MODIFIED)
  dis-all)

(defcom com-refind-files-for-unmodified-buffers "Revert or update all buffers to contain their most recent versions." ()
  (refind-buffer-subset :NOT-MODIFIED)
  dis-all)

(defcom com-refind-specified-files "Revert or update all buffers to contain their most recent versions." ()
  (*catch 'REFIND-NOTHING
    (refind-buffer-subset :SPECIFIED)
    dis-all))

(DEFCOM COM-FIND-FILE-BACKGROUND "Like FIND-FILE but reads the file in another process.
You will be notified when the file has been read in."
        ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file background:" (PATHNAME-DEFAULTS)
                                           NIL NIL :NEW-OK)))
    (process-run-function (format nil "Load ~A into zmacs" pathname)
                          (lambda (name window)
                            (load-file-into-zmacs name nil)
                            (tv:notify window "The file ~a is now available in Zmacs" name))
                          pathname
                          *window*))
  DIS-NONE)

(DEFCOM COM-FIND-FILE-NO-SECTIONIZE "Visits a file in its own buffer; does not record definitions in it.
Like Find File except does not record functions in it for access with Meta-.
Furthermore, nothing will record such information unless you
explicitly give the Sectionize Buffer command on that buffer." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find file:" (PATHNAME-DEFAULTS)
                                           NIL NIL :NEW-OK)))
    (FIND-FILE PATHNAME T NIL T T))
  (MAYBE-DISPLAY-DIRECTORY :READ)
  DIS-TEXT)

(DEFCOM COM-FIND-SYSTEM-FILES "Visits all the files in specified system.
You type the name of a system defined with DEFSYSTEM,
and all the files in that system are brought into the editor if not already in." ()
  (LET* ((SYSTEM (READ-SYSTEM-NAME "System whose files to read in:"))
         (PKG (SI:SYSTEM-PACKAGE-DEFAULT (SI:FIND-SYSTEM-NAMED SYSTEM))))
    (DOLIST (FILE (SI:SYSTEM-SOURCE-FILES SYSTEM))
      (LET ((SOURCE-PATHNAME (SEND FILE :SOURCE-PATHNAME)))
        (UNLESS (FIND-FILE-BUFFER SOURCE-PATHNAME)
          (LET ((BUFFER (MAKE-INSTANCE 'ZMACS-BUFFER)))
            (REVERT-BUFFER BUFFER SOURCE-PATHNAME 'NOSECTIONIZE NIL)
            (WHEN PKG
              (SEND BUFFER :SET-ATTRIBUTE ':PACKAGE PKG)
              (SETF (BUFFER-PACKAGE BUFFER)
                    (PKG-FIND-PACKAGE PKG)))
            (MAKE-BUFFER-CURRENT BUFFER)
            (SECTIONIZE-BUFFER BUFFER)
            (NOT-MODIFIED BUFFER)
            (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*))))))
  DIS-TEXT)

;;; This should only be used when the user spazzes from a C-X C-F
(DEFCOM COM-FIND-ALTERNATE-FILE "Visit a file in the current buffer.
Use this if you give the wrong filename in C-X C-F and it fails or
you get a nonexistent file." ()
  (OR (SYMBOLP (BUFFER-FILE-ID *INTERVAL*))
      (BARF "This buffer is already editing a real file"))
  (OR (BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
      (BARF "This buffer is not empty"))
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Find alternate file:" (PATHNAME-DEFAULTS)
                                           NIL NIL :NEW-OK))
        (OLD-NAME (BUFFER-NAME *INTERVAL*)))
    (AND (BUFFER-FILE-ID *INTERVAL*)
         (FIND-FILE-BUFFER PATHNAME)
         (NEQ (FIND-FILE-BUFFER PATHNAME) *INTERVAL*)
         (BARF "~A is already being edited in another buffer" PATHNAME))
    (REVERT-BUFFER *INTERVAL* PATHNAME T)
    (UNLESS (STRING-EQUAL OLD-NAME (BUFFER-NAME *INTERVAL*))
      (LET ((NAME (BUFFER-NAME *INTERVAL*)))
        (LET ((ELEM (SI:RASSOC-EQUAL *INTERVAL* *ZMACS-BUFFER-NAME-ALIST*)))
          (AND ELEM (SETF (CAR ELEM) NAME)))
        (SETQ *ZMACS-BUFFER-NAME* NAME))
      (DOLIST (W (SEND *INTERVAL* :WINDOWS))
        (CHANGE-WINDOW-LABEL W))))
  (MAYBE-DISPLAY-DIRECTORY :READ)
  DIS-TEXT)

;; Subroutine of FIND-FILE.
(DEFUN FIND-FILE-1 (PATHNAME WILD-PATHNAME-VERSION &OPTIONAL (SELECT-P T) QUIETLY (LOAD-P T)
                    DONT-SECTIONIZE)
  (FIND-FILE (IF (EQ WILD-PATHNAME-VERSION ':NEWEST)
                 (SEND PATHNAME :NEW-VERSION :NEWEST)
                 PATHNAME)
             SELECT-P QUIETLY LOAD-P DONT-SECTIONIZE))

(DEFUN FIND-FILE (PATHNAME &OPTIONAL (SELECT-P T) QUIETLY (LOAD-P T) DONT-SECTIONIZE
                  &AUX BUFFER STREAM)
  "Return a buffer visiting file PATHNAME, reading file in if necessary.
If SELECT-P is T (the default), select the buffer also.
QUIETLY non-NIL means do not print messages about reading the file.
If LOAD-P is NIL, we do not read the file, just create
a buffer supposedly visiting that file (as if the file did not exist).
If there is already a buffer visiting the file, we check to see
if a more recent version of the file exists in the file system
and offer to revert if so.  To avoid this, try FIND-FILE-BUFFER
before you try FIND-FILE.

If DONT-SECTIONIZE is non-NIL, we mark all the buffers
 not to be sectionized.

If PATHNAME has wildcards, we visit all the files specified."
  (IF (STRINGP PATHNAME)
      (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME)))
  (IF (SEND PATHNAME :WILD-P)
      (SEND PATHNAME :WILDCARD-MAP #'FIND-FILE-1 NIL NIL
            (SEND PATHNAME :VERSION) SELECT-P QUIETLY LOAD-P DONT-SECTIONIZE)
    (SETQ BUFFER (OR (FIND-FILE-BUFFER PATHNAME)
                     (MAKE-INSTANCE 'ZMACS-BUFFER :NAME NIL)))
    (IF DONT-SECTIONIZE
        (SETF (GET BUFFER ':DONT-SECTIONIZE) T)
        (REMPROP BUFFER ':DONT-SECTIONIZE))
    (IF (NULL (BUFFER-FILE-ID BUFFER))
        (IF LOAD-P
            (REVERT-BUFFER BUFFER PATHNAME
                           (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
                           (AND SELECT-P *FIND-FILE-EARLY-SELECT*) QUIETLY)
          (SET-BUFFER-PATHNAME PATHNAME BUFFER))
      (IF (NULL (SETQ STREAM (OPEN PATHNAME :DIRECTION :PROBE)))
          (AND (NOT (SYMBOLP (BUFFER-FILE-ID BUFFER)))
               (progn (send *query-io* :clear-window)
                      (FORMAT *QUERY-IO* "~&Note: File ~A has been deleted." PATHNAME)))
        (MULTIPLE-VALUE-BIND (NEW-DESC OLD-DESC)
            (STREAM-CHECK-FILE-ID STREAM (BUFFER-FILE-ID BUFFER))
          (COND ((AND (SYMBOLP (BUFFER-FILE-ID BUFFER))
                      (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER)))
                 (WHEN (YES-OR-NO-P "The file ~A exists now.  Read it in? " PATHNAME)
                   (REVERT-BUFFER BUFFER PATHNAME
                                  (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
                                  (AND SELECT-P *FIND-FILE-EARLY-SELECT*))))
                ((SYMBOLP (BUFFER-FILE-ID BUFFER)))
                ((NOT NEW-DESC))
                ((BUFFER-NEEDS-SAVING-P BUFFER)
                 (BEEP)
                 (FORMAT T "Since you last read or wrote ~A
 (~A),
while you've been editing, someone has written a new copy out
 (~A).
You will lose some work if you are not careful.
I will leave you your old copy instead of reading the new one.
I suggest that you file this out under a different name and then SRCCOM the two files.
Do M-X Revert if you really want the new one.~%" PATHNAME OLD-DESC NEW-DESC))
                (T
                 (FORMAT T "Since you last read or wrote ~A
 (~A),
someone else wrote a new version on disk
 (~A).
Luckily, you haven't edited the buffer since then.
Your old copy is still in the buffer.  " PATHNAME OLD-DESC NEW-DESC)
                 (WHEN (FQUERY `(:STREAM ,*STANDARD-OUTPUT* . ,FORMAT::YES-OR-NO-P-OPTIONS)
                               "Do you want the new version instead? ")
                   (REVERT-BUFFER BUFFER PATHNAME
                                  (IF DONT-SECTIONIZE 'NOSECTIONIZE T)
                                  (AND SELECT-P *FIND-FILE-EARLY-SELECT*))))))))
    (SEND BUFFER :ACTIVATE T)
    (IF SELECT-P (MAKE-BUFFER-CURRENT BUFFER))
    BUFFER))

(DEFUN STREAM-CHECK-FILE-ID (STREAM FILE-ID &AUX FILE-FILE-ID)
  "If STREAM's :INFO doesn't match FILE-ID, return two strings describing new and old values.
Otherwise return NIL.
The new value is STREAM's :INFO, the old value is FILE-ID."
  (COND ((NULL FILE-ID) NIL)                    ;no associated file (yes, I know NIL is symbolp)
        ((SYMBOLP FILE-ID) NIL)                 ;buffer's file not yet created
        ((EQUAL FILE-ID (SETQ FILE-FILE-ID (SEND STREAM :INFO))) NIL)
        (T
         (VALUES (DESCRIBE-FILE-ID FILE-FILE-ID)
                 (DESCRIBE-FILE-ID FILE-ID)))))

(DEFUN DESCRIBE-FILE-ID (FILE-ID)
  "Given a file-id (the value of the :INFO file stream op) return a string describing it."
  (IF (SYMBOLP FILE-ID)
      "a new file, not present on disk"
    (FORMAT NIL "~A, created ~@[by ~A at ~]~A"
            (CAR FILE-ID)
            (LET ((PROPS (SEND (CAR FILE-ID) :PROPERTIES NIL)))
              (AND (CONSP PROPS) (GET PROPS ':AUTHOR)))
            (IF (CDR FILE-ID)
                (TIME:PRINT-UNIVERSAL-TIME (CDR FILE-ID) NIL)
              "an unknown time"))))

(DEFCOM COM-SECTIONIZE-BUFFER "Reparse a buffer for definitions.
Repeat the processing normally done only when the file is visited
which finds the definitions in the file so that M-. can work.
This is useful if you have added functions to the file." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Sectionize Buffer:"
                                  *INTERVAL*    ;Default is current buffer.
                                  NIL)))
    (SEND BUFFER :SELECT)
    (SEND BUFFER :REMPROP ':DONT-SECTIONIZE)
    (SEND BUFFER :SECTIONIZE))
  DIS-TEXT)

(DEFCOM COM-SELECT-BUFFER "Select the specified buffer.
Does a completing read of the buffer name in the echo area.
With a numeric argument, allows you to create a new buffer." ()
  (SELECT-BUFFER "Select buffer:" (IF *NUMERIC-ARG-P* T 'MAYBE)))

(DEFUN SELECT-BUFFER (PROMPT ALLOW-CREATE-NEW)
  (SEND (READ-BUFFER-NAME PROMPT T ALLOW-CREATE-NEW) :SELECT)
  DIS-TEXT)

(DEFUN READ-BUFFER-NAME (PROMPT DEFAULT
                         &OPTIONAL
                         IMPOSSIBLE-IS-OK-P
                         (BUFFER-HISTORY (SEND *WINDOW* :BUFFER-HISTORY))
                         &aux default-string)
  "Read a buffer name in the mini buffer and return a buffer.
DEFAULT is the default to use; T means use most recent buffer other than the current one.
PROMPT is a string to prompt with; should end in a colon if you want one.
IMPOSSIBLE-IS-OK can be T, NIL or MAYBE.
 T means create a new buffer if name does not complete.
 ZWEI:MAYBE means do so, but user must type Return twice to confirm.
 NIL means don't allow names that don't complete, at all.
BUFFER-HISTORY is a history object containing buffers.
 It defaults to *WINDOW*'s buffer history."
  (declare (values buffer))
  (if  (EQ DEFAULT T)   ; Select most recent buffer other than this one
       (SETQ DEFAULT (PREVIOUS-BUFFER)))
  (setq default-string
         (typecase default
           (null nil)
           (zmacs-buffer (BUFFER-NAME DEFAULT))
           ((or string symbol) (string default))
           (t (format nil "~A" default))))
  (SETQ PROMPT (format nil
                       "~A~@[  (~A)~]  <Use Control-Shift-F to specify a filename>"
                       prompt
                       default-string))
  ;;For C-Shift-F, set pathname defaults :
  (if (typep default 'zmacs-buffer)
      (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* default)
    (pathname-defaults *pathname-defaults* *interval*))
  ;;Get name, and find or (maybe) create requested buffer:
  (LET* ((*READ-BUFFER-KLUDGE* T)
         (*MINI-BUFFER-DEFAULT-STRING* default-string)
         (*COMPLETING-DELIMS* '(#/Space #/- #/. #/\ #// #/#))
         (*MINI-BUFFER-VALUE-HISTORY* BUFFER-HISTORY)
         (NAME (COMPLETING-READ-FROM-MINI-BUFFER
                 PROMPT *ZMACS-BUFFER-NAME-ALIST* IMPOSSIBLE-IS-OK-P)))
    (cond
      ((typep name 'zmacs-buffer) name)         ;Got buffer from C-Shift-F
      ((consp name) (cdr name))                 ;Requested buffer already exists
      (t
       (if (equal name "")                      ;User pressed <Return>
           (setq name default-string))
       (or (and name (find-buffer-named name))  ;Look for default buffer, maybe
           (when impossible-is-ok-p             ;Create a new buffer
             (format *query-io* "~&(New Buffer)")
             (apply 'create-one-buffer-to-go    ;Null default? Generate name
                    (if name (ncons name))))
           ;;Punt with appropriate message:
           (barf "There is no ~:[default buffer~;buffer named ~:*~A~]" name))))))

(defun specify-list-of-buffers (&key (buffer-list *zmacs-buffer-list*)
                                     (predicate #'identity)
                                     only-file-buffers
                                     only-non-special-buffers
                                     substring
                                     query-string)
  "Return a subset of BUFFER-LIST subject to certain conditions.
Each of the conditions is checked in the order listed below:
- ONLY-NON-SPECIAL-BUFFERS means not to consider buffers with a null NODE-SPECIAL-TYPE.
- ONLY-FILE-BUFFERS means to consider only buffers visiting files.
- SUBSTRING, if non-NIL, means to consider only buffers whose names contain that string.
- PREDICATE means consider only buffers satisfying that function.
- If QUERY-STRING is non-NIL, it is a format string used to ask the user whether
 to include a buffer satisfying all the above conditions.
 The /"arguments/" to this format string are the buffer's name
 and the file it is visiting (if it is a file buffer -- NIL otherwise)."
  (loop for buffer in buffer-list
        with *query-io* = (if query-string *terminal-io* *query-io*)
        with stop = nil
     when (and (not stop)
               (typep buffer 'zmacs-buffer)
               (or (not only-non-special-buffers)
                   (not (node-special-type buffer)))
               (or (not only-file-buffers)
                   (buffer-file-id buffer))
               (or (not substring)
                   (string-search substring (buffer-name buffer)))
               (funcall predicate buffer)
               (or (not query-string)
                   (case (fquery `(:choices (,@format:y-or-n-p-choices
                                             ((stop "Finished, no more files") #/ #/F)))
                                 query-string
                                 (buffer-name buffer)
                                 (and (typep buffer 'file-buffer) (buffer-pathname buffer)))
                     ((nil) nil)
                     ((t) t)
                     (stop
                      ;; Say no to this file, and say don't ask for any others.
                      (setq stop t)
                      nil))))
       collect buffer))

(DEFCOM COM-SPECIFY-FILE-BUFFER "Specify a pathname instead of a buffer name." ()
  (OR *READ-BUFFER-KLUDGE*
      (BARF "You are not now being asked to type a buffer name."))
  (LET* ((FILENAME
           (LET (*MINI-BUFFER-COMMAND*
                 *MINI-BUFFER-COMMAND-IN-PROGRESS*
                 *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
             (READ-DEFAULTED-PATHNAME
               "Use the buffer of file: "
               *PATHNAME-DEFAULTS*
               NIL NIL :NEW-OK T T
               (STRING-INTERVAL *INTERVAL*)
               (COUNT-CHARS (INTERVAL-FIRST-BP *INTERVAL*) (POINT)))))
         (BUFFER (OR (FIND-FILE-BUFFER FILENAME)
                     (FIND-FILE FILENAME NIL))))
    ;; Arrange for the minibuffer edit to return this buffer.
    ;; Also put the buffer name in the minibuffer interval
    ;; so that the right things are recorded for C-X .
    (DELETE-INTERVAL *INTERVAL*)
    (INSERT (INTERVAL-FIRST-BP *INTERVAL*)
            (BUFFER-NAME BUFFER))
    (THROW 'RETURN-FROM-COMMAND-LOOP BUFFER)))

(DEFCOM COM-SELECT-PREVIOUS-BUFFER "Select the previously selected buffer.
A numeric argument selects the argth previous buffer (the default argument
is 2).  With an argument of 1, rotates the entire buffer history, and
a negative argument rotates the other way.
This uses the order of buffers that is displayed by List Buffers." ()
  (ROTATE-BUFFER-HISTORY (IF (MEMQ *NUMERIC-ARG-P* '(NIL :SIGN))
                             (* *NUMERIC-ARG* 2)
                             *NUMERIC-ARG*)))

(DEFVAR *DEFAULT-PREVIOUS-BUFFER-ARG* 3)
(DEFCOM COM-SELECT-DEFAULT-PREVIOUS-BUFFER "Rotate the stack of previously selected buffers.
A numeric argument specifies the number of entries to rotate, and sets the new default." ()
  (OR (MEMQ *NUMERIC-ARG-P* '(:SIGN NIL))
      (SETQ *DEFAULT-PREVIOUS-BUFFER-ARG* *NUMERIC-ARG*))
  (ROTATE-BUFFER-HISTORY (IF (EQ *NUMERIC-ARG-P* ':SIGN)
                             (* *NUMERIC-ARG* *DEFAULT-PREVIOUS-BUFFER-ARG*)
                             *DEFAULT-PREVIOUS-BUFFER-ARG*)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Write" WRITE-BUFFER NIL
                          "Write this buffer to specified filename.")

(DEFCOM COM-SAVE-FILE "Write out changes to current file.
If the current buffer has no file, reads in a file name from the mini buffer." ()
  (IF (NOT (SEND *INTERVAL* :MODIFIED-P))
      (FORMAT *QUERY-IO* "~&(No changes need to be written.)")
    (SAVE-BUFFER *INTERVAL*)
    (MAYBE-DISPLAY-DIRECTORY :WRITE))
  DIS-NONE)

(DEFCOM COM-SAVE-ALL-FILES "Offer to write out any changed buffers.
A numeric argument causes the query to be skipped." ()
  (LET ((*QUERY-IO* *STANDARD-OUTPUT*)
        SAVE-WORD-ABBREVS
        BUFFERS-TO-BE-SAVED)
    ;; Ask about each buffer.
    ;; If user says save, and buffer has no file, ask for file now.
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (BUFFER-NEEDS-SAVING-P BUFFER)
           (OR *NUMERIC-ARG-P*
               (FQUERY () "Save file ~A ? " (BUFFER-NAME BUFFER)))
           (PROGN
             (READ-BUFFER-PATHNAME-FOR-SAVING BUFFER)
             (PUSH BUFFER BUFFERS-TO-BE-SAVED))))
    ;; Ask, similarly, about saving word abbrevs.
    (SETQ SAVE-WORD-ABBREVS
          (WORD-ABBREVS-NEED-SAVING-P *NUMERIC-ARG-P*))
    (OR *NUMERIC-ARG-P*
        (AND *WINDOW* (OR SAVE-WORD-ABBREVS BUFFERS-TO-BE-SAVED)
             (FORMAT T "~&Saving now.")))
    ;; Now save the things the user has already said should be saved.
    (DOLIST (BUFFER (NREVERSE BUFFERS-TO-BE-SAVED))
      (SAVE-BUFFER BUFFER))
    (AND SAVE-WORD-ABBREVS
         (COM-WRITE-WORD-ABBREV-FILE-INTERNAL)))
  (AND *WINDOW*
       (IF *NUMERIC-ARG-P*
           (FORMAT *QUERY-IO* "~&Done") ; if numeric arg we only want printing at bottom of window.
         (FORMAT T "~&Done.~%")))
  DIS-NONE)

(DEFUN SAVE-BUFFER-IF-NECESSARY (BUFFER &OPTIONAL CONFIRM)
  "Save BUFFER if it is a file buffer with unsaved changes.
CONFIRM non-NIL means ask user to confirm."
  (AND (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (NOT CONFIRM)
           (FQUERY () "Save file ~A ? " (BUFFER-NAME BUFFER)))
       (SAVE-BUFFER BUFFER)))

(DEFUN READ-BUFFER-PATHNAME-FOR-SAVING (BUFFER &AUX PATHNAME)
  "If BUFFER isn't visiting a file, read a pathname in the mini buffer.
The pathname read is put in BUFFER's BUFFER-PATHNAME."
  (WHEN (NULL (BUFFER-FILE-ID BUFFER))
    (SETQ PATHNAME
          (IF *WINDOW* (READ-DEFAULTED-PATHNAME (FORMAT NIL "Save buffer ~A in File:"
                                                        (BUFFER-NAME BUFFER))
                                                (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS*
                                                                   BUFFER)
                                                NIL NIL :WRITE)
            (FORMAT *QUERY-IO* "~&Save file to (Default ~A): "
                    (DEFAULT-PATHNAME *PATHNAME-DEFAULTS*))
            (MAKE-DEFAULTED-PATHNAME (READLINE) (PATHNAME-DEFAULTS))))
    (SET-BUFFER-PATHNAME PATHNAME BUFFER)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Save" SAVE-BUFFER NIL
                          "Save this buffer.")

(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME (FILE-FILE-ID NIL))
  "Save BUFFER unconditionally into its visited file, or a file read from the mini buffer."
  (READ-BUFFER-PATHNAME-FOR-SAVING BUFFER)
  (SETQ FILE-ID (BUFFER-FILE-ID BUFFER)
        PATHNAME (BUFFER-PATHNAME BUFFER))
  (AND (OR (SYMBOLP FILE-ID)
           (WITH-OPEN-FILE (S PATHNAME '(:PROBE :ASCII))
             (AND (NOT (ERRORP S))
                  (EQUAL (SETQ FILE-FILE-ID (SEND S :INFO)) FILE-ID)))
           ;;Sometimes we can get into a situation where the "save it
           ;;anyway" problem, below, occurs every time we go to save a
           ;;particular buffer.  So, we allow the user to say, "stop
           ;;bothering me", allowing the "Proceed" response.
           (getf (plist buffer) 'dont-ask-again-about-saving-it-anyway)
           (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
             (case
               (fquery '(:beep t
                         :type :tyi
                         :choices (((T "Yes")  #\Y #\hand-up)
                                   ((NIL "No") #\N #\hand-down)
                                   ((:proceed "Proceed, and don't ask again about this buffer later") #\P)))
                       "When you last read or wrote ~A~@
                      it was ~A,~@
                      but now it is ~A.~@
                      Do you want to save it? "
                       PATHNAME (DESCRIBE-FILE-ID FILE-ID)
                       (IF FILE-FILE-ID (DESCRIBE-FILE-ID FILE-FILE-ID) "deleted"))
               (NIL NIL)
               (:PROCEED
                (setf (getf (plist buffer) 'dont-ask-again-about-saving-it-anyway) t)
                t)
               (T T))))
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

(DEFUN WRITE-BUFFER (BUFFER)
  "Write BUFFER to a file, reading filename in mini buffer."
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME (FORMAT NIL "Write buffer ~A to File:"
                                                   (BUFFER-NAME BUFFER))
                                           (PATHNAME-DEFAULTS *PATHNAME-DEFAULTS* BUFFER)
                                           NIL NIL :WRITE)))
    (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  DIS-NONE)

(DEFCOM COM-WRITE-FILE "Write out the buffer to the specified file." ()
  (WRITE-BUFFER *INTERVAL*)
  (MAYBE-DISPLAY-DIRECTORY :WRITE)
  DIS-NONE)

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#558 on 2-Oct-86 02:29:19
(DEFUN WRITE-FILE-INTERNAL (PATHNAME &OPTIONAL (BUFFER *INTERVAL*))
  "Save BUFFER in file PATHNAME and mark it as visiting that file.
If the file holds the current ODM node, handle it appropriately."
  (if (and (boundp '*current-gateway-buffer*)
           (eq buffer *current-gateway-buffer*)
           (fboundp 'write-file-internal-odm))
        (write-file-internal-odm pathname buffer)
  (SEND BUFFER :WRITE-FILE-INTERNAL PATHNAME)))  ;in METH.LISP

(DEFUN SET-BUFFER-PATHNAME (PATHNAME &OPTIONAL (BUFFER *INTERVAL*) &AUX STRING)
  "Set the pathname BUFFER is visiting to PATHNAME."
  (SETF (BUFFER-PATHNAME BUFFER) (SEND PATHNAME :TRANSLATED-PATHNAME))
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) (SEND PATHNAME :GENERIC-PATHNAME))
  (SETQ STRING (SEND PATHNAME :STRING-FOR-EDITOR))
  (COND ((EQUALP (BUFFER-NAME BUFFER) STRING)
         NIL)
        ((CL:ASSOC STRING *ZMACS-BUFFER-NAME-ALIST* :TEST #'EQUALP)
         (FORMAT *QUERY-IO* "~&Not renaming the buffer!  There is already a buffer named ~A"
                 STRING))
        (T
         (SIMILAR-BUFFER-FILES-WARNING BUFFER)
         (SEND BUFFER :RENAME STRING)
;Should no longer be necessary.
;        ;; Transfer the attribute list info to the new pathname.
;        (LET ((PKG (BUFFER-PACKAGE BUFFER)))
;          (REPARSE-BUFFER-attribute-list BUFFER)
;          (SETF (BUFFER-PACKAGE BUFFER) PKG))
         )))

(DEFUN SIMILAR-BUFFER-FILES-WARNING (BUFFER &AUX SAME-NAME SAME-TYPE SAME-EVERYTHING)
  "Warn if any buffer other than BUFFER is visiting the same or a similar file."
  (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
    (AND (NEQ (CDR ELT) BUFFER)
         (BUFFER-PATHNAME (CDR ELT))
         (BUFFER-FILE-ID (CDR ELT))
         (NOT (NODE-SPECIAL-TYPE (CDR ELT)))
         (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) :STRING-FOR-EDITOR)
                     (SEND (BUFFER-PATHNAME (CDR ELT)) :STRING-FOR-EDITOR))
             (RETURN (SETQ SAME-EVERYTHING (CDR ELT)))
           (IF (EQUALP (SEND (BUFFER-PATHNAME BUFFER) :NAME)
                       (SEND (BUFFER-PATHNAME (CDR ELT)) :NAME))
               (COND ((EQUALP (SEND (BUFFER-PATHNAME BUFFER) :TYPE)
                              (SEND (BUFFER-PATHNAME (CDR ELT)) :TYPE))
                      (SETQ SAME-TYPE (CDR ELT)))
                     (T (SETQ SAME-NAME (CDR ELT))))))))
  (IF SAME-EVERYTHING
      (FORMAT *QUERY-IO* "~&Warning: Buffer ~A~&  is also visiting file ~A."
              (BUFFER-NAME SAME-EVERYTHING) (BUFFER-PATHNAME SAME-EVERYTHING))
    (LET ((LOSER (OR SAME-TYPE SAME-NAME)))
      (IF LOSER
          (FORMAT *QUERY-IO* "~&Note: Another buffer ~A~&  is visiting file ~A."
                  (BUFFER-NAME LOSER) (BUFFER-PATHNAME LOSER))))))

(DEFUN SET-BUFFER-FILE-ID (BUFFER INFO)
  "Set the BUFFER-FILE-ID of BUFFER to INFO.
Records the file BUFFER was last read or saved in."
  (SETF (BUFFER-FILE-ID BUFFER) INFO)
  (LET ((VERSION-STRING (AND (TYPEP (CAR-SAFE INFO) 'FS:PATHNAME)
                             (BUFFER-PATHNAME BUFFER)
                             (NOT (NUMBERP (SEND (BUFFER-PATHNAME BUFFER) :VERSION)))
                             (LET ((VERSION (SEND (CAR INFO) :VERSION)))
                               (AND (NUMBERP VERSION) (FORMAT NIL " (~D)" VERSION))))))
    (SETF (BUFFER-VERSION-STRING BUFFER) VERSION-STRING)
    (AND (EQ BUFFER *INTERVAL*) (SETQ *ZMACS-BUFFER-VERSION-STRING* VERSION-STRING)))
  INFO)

(defun buffer-file-version-if-known (buffer)
 "NIL or the version of the truename associated with this buffer"
 ;--unfortunately, the truename is not stored directly, it should be. --rg.
  (let ((info (buffer-file-id buffer)))
    (if (and (typep (car-safe info) 'fs:pathname)
             (buffer-pathname buffer))
        (let ((v (send (buffer-pathname buffer) :version)))
          (if (numberp v)
              v
              (send (car info) :version))))))

(DEFCOM COM-REVERT-BUFFER "Forgets changes to a specified buffer.
Reads the name of the buffer from the mini-buffer and reads back in the file
or function." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to revert:" *INTERVAL*)))
    (REVERT-BUFFER BUFFER)
    (MUST-REDISPLAY-BUFFER BUFFER DIS-TEXT)
    DIS-NONE))

(DEFUN REVERT-BUFFER (BUFFER &OPTIONAL (PATHNAME (BUFFER-PATHNAME BUFFER))
                                       (CONNECT-FLAG (BUFFER-FILE-ID BUFFER))
                                       SELECT-FLAG
                                       QUIETLY-FLAG)
  "Read file PATHNAME, or BUFFER's visited file into BUFFER.
CONNECT-FLAG non-NIL means mark BUFFER as visiting the file.
 This may change the buffer's name.
 It defaults non-NIL if BUFFER is visiting a file now.
If CONNECT-FLAG is NOSECTIONIZE, mark buffer as visiting but don't sectionize it.
SELECT-FLAG non-NIL means select BUFFER.
QUIETLY-FLAG means do not print a message about reading a file."
  (SEND BUFFER :REVERT PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG))

;; Only the :REVERT method for FILE-BUFFER calls this.
(DEFUN REVERT-FILE-BUFFER (BUFFER PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG
                           &AUX GENERIC-PATHNAME PATHNAME-STRING TRUENAME NEW-MODE)
  (WHEN (AND (NULL (BUFFER-FILE-ID BUFFER)) (NULL PATHNAME))
    (BARF "The buffer ~A is not associated with a file." (BUFFER-NAME BUFFER)))
  (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
    (EDITOR-FILE-NAME PATHNAME))
  (WHEN CONNECT-FLAG
    (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
    (SETF (BUFFER-PATHNAME BUFFER) PATHNAME))
  (SETQ GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
  (SETF (BUFFER-GENERIC-PATHNAME BUFFER) GENERIC-PATHNAME)
  (WITH-OPEN-FILE-CASE (STREAM PATHNAME :CHARACTERS T)
    (:NO-ERROR
     (SETQ TRUENAME (SEND STREAM :TRUENAME))
     (WHEN (MEMQ (SEND PATHNAME :TYPE) '(NIL :UNSPECIFIC))
       (MULTIPLE-VALUE-SETQ (PATHNAME PATHNAME-STRING)
         (EDITOR-FILE-NAME
           (IF (EQUALP (SEND TRUENAME :NAME) (SEND PATHNAME :NAME))
               ;; This is in case user reads FOO > from an ITS, and it is reall FOO BAR.
               (SEND PATHNAME :NEW-TYPE (SEND TRUENAME :TYPE))
             ;; This case if user read FOO BAR from an LMFILE, and truename is FOO|BAR.
             ;; Or if user reads FOO BAR from an ITS and it is a link to UGH QUUX.
             PATHNAME))))
     (WHEN CONNECT-FLAG
       (SETF (BUFFER-NAME BUFFER) PATHNAME-STRING)
       (SETF (BUFFER-PATHNAME BUFFER) PATHNAME)
       (SIMILAR-BUFFER-FILES-WARNING BUFFER))
     (WHEN (NOT QUIETLY-FLAG)
       (FORMAT *QUERY-IO* "~&Reading ~A" TRUENAME)
       (LET ((THIS-VERSION (SEND TRUENAME :VERSION))
             (INSTALLED-TRUENAME (FILE-LOADED-TRUENAME TRUENAME))
             INSTALLED-VERSION)
         (AND INSTALLED-TRUENAME
              (NUMBERP THIS-VERSION)
              (NUMBERP (SETQ INSTALLED-VERSION (SEND INSTALLED-TRUENAME :VERSION)))
              ( INSTALLED-VERSION THIS-VERSION)
              (FORMAT *QUERY-IO* " (installed version is ~D)" INSTALLED-VERSION))))
     (FS:READ-ATTRIBUTE-LIST BUFFER STREAM)
     ;; Forget (and thereby override) and previouse Set Package in this buffer.
     (SETF (BUFFER-PACKAGE BUFFER) NIL)
     ;; And recompute from latest attribute list.
     (INITIALIZE-BUFFER-PACKAGE BUFFER)
     (UNLESS (SEND BUFFER :GET-ATTRIBUTE ':MODE)
       (SEND BUFFER :SET-ATTRIBUTE ':MODE
                                   (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
                                                            FS:*FILE-TYPE-MODE-ALIST*))
                                       *DEFAULT-MAJOR-MODE*)))
     (SETQ NEW-MODE (OR (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE ':MODE))
                        'FUNDAMENTAL-MODE))
     (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
       (IF (EQ BUFFER *INTERVAL*)
           (COMPUTE-BUFFER-PACKAGE BUFFER))
       (AND NEW-MODE (SEND BUFFER :SET-MAJOR-MODE NEW-MODE)))
     (PRESERVE-BUFFER-POINT (BUFFER)
       (WITH-READ-ONLY-SUPPRESSED (BUFFER)
         (LET ((*BATCH-UNDO-SAVE* T))           ;Don't save all this for undo!
           (DISCARD-UNDO-INFORMATION BUFFER)
           (DELETE-INTERVAL BUFFER)
           (SETF (BUFFER-TICK BUFFER) (TICK))   ;For SECTIONIZE-BUFFER
           (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
           (LET ((FONTS (SET-BUFFER-FONTS BUFFER))
                 FONTS-P)
             (SETQ FONTS-P (OR (CDR FONTS) (SEND BUFFER :GET-ATTRIBUTE ':DIAGRAM)))
             (WHEN SELECT-FLAG
               (SEND BUFFER :ACTIVATE)
               (MAKE-BUFFER-CURRENT BUFFER)
               ;; If it is requested, read in the first screenful and then redisplay.
               (DOTIMES (I (+ 5 (WINDOW-N-PLINES *WINDOW*)))
                 (MULTIPLE-VALUE-BIND (LINE EOFFLG)
                     (SEND STREAM :LINE-IN LINE-LEADER-SIZE)
                   (WHEN LINE
                     (INSERT-LINE-WITH-LEADER LINE
                                              (BP-LINE (INTERVAL-LAST-BP BUFFER))))
                   (IF EOFFLG (RETURN))))
               (REDISPLAY *WINDOW* :START (INTERVAL-FIRST-BP BUFFER) NIL))
             (IF (NOT CONNECT-FLAG)
                 (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
               (IF (EQ CONNECT-FLAG 'NOSECTIONIZE)
                   (STREAM-INTO-BP STREAM (INTERVAL-FIRST-BP BUFFER) FONTS-P)
                 (SECTIONIZE-FILE-BUFFER BUFFER *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
                                         NIL NIL
                                         STREAM FONTS-P))
               (SET-BUFFER-FILE-ID BUFFER (SEND STREAM :INFO))
               (DOLIST (WINDOW (SEND BUFFER :WINDOWS))
                 (AND FONTS
                      (REDEFINE-FONTS WINDOW
                                      FONTS (SEND BUFFER :GET-ATTRIBUTE ':VSP)))
                 (REDEFINE-WINDOW-OVERPRINTING-FLAG WINDOW
                                                    (SEND BUFFER :GET-ATTRIBUTE ':BACKSPACE))
                 (REDEFINE-WINDOW-TAB-NCHARS WINDOW
                                             (SEND BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))))
             (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*)
             (NOT-MODIFIED BUFFER)))))
     (UNLESS SELECT-FLAG                        ;else already done above
       (SEND BUFFER :ACTIVATE))
     (UNLESS QUIETLY-FLAG
       (LET ((NCHARS (SEND-IF-HANDLES STREAM :READ-POINTER)))
         (COND ((NULL NCHARS)
                (FORMAT *QUERY-IO* " -- done."))
               ((< NCHARS 5000.)
                (FORMAT *QUERY-IO* " -- ~D characters." NCHARS))
               (T (FORMAT *QUERY-IO* " -- ~DK characters." (ROUND NCHARS 1024.)))))))
    (FS:FILE-NOT-FOUND
     (WHEN *FIND-FILE-NOT-FOUND-IS-AN-ERROR* (BARF STREAM))
     (OR QUIETLY-FLAG (FORMAT *QUERY-IO* "(New File)"))
     (LET ((*BATCH-UNDO-SAVE* T))
       (DISCARD-UNDO-INFORMATION BUFFER)
       (DELETE-INTERVAL BUFFER))
     (AND CONNECT-FLAG (SET-BUFFER-FILE-ID BUFFER T))
     (SEND BUFFER :SET-ATTRIBUTE ':MODE
                                 (OR (CDR (SI:ASSOC-EQUAL (SEND PATHNAME :CANONICAL-TYPE)
                                                          FS:*FILE-TYPE-MODE-ALIST*))
                                     *DEFAULT-MAJOR-MODE*))
     (SETF (BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*)))
     (LET ((MODE (GET-FILE-MAJOR-MODE (SEND BUFFER :GET-ATTRIBUTE :MODE))))
       (LET-IF QUIETLY-FLAG ((*INTERVAL* NIL))
         (IF (EQ BUFFER *INTERVAL*) (COMPUTE-BUFFER-PACKAGE BUFFER))
         (AND MODE (SEND BUFFER :SET-MAJOR-MODE MODE)))))
    (FS:FILE-ERROR (BARF STREAM)))
  (SETF (BUFFER-TICK BUFFER) (TICK)))           ;Buffer is same as file

(defcom com-revert-all-buffers "Send a :REVERT message to all active buffers." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (send-if-handles each-buffer :REVERT))
  dis-all)

(defun revert-buffer-if-needed (buffer)
  (LET* ((buffer-pathname (buffer-pathname buffer))
         (probe-stream (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
    (IF (NOT PROBE-STREAM)
        (format t "The buffer ~S is not associated with an existing file."
                buffer-pathname)
      (if (buffer-modified-p buffer)
          (send-if-handles buffer :REVERT)))))

(defcom com-revert-buffer-if-needed "Revert any buffer with a handler the :REVERT message, if modified." ()
  (revert-buffer-if-needed *interval*)
  dis-all)

(defcom com-revert-all-buffers-if-needed "Revert all buffers with handlers for the :REVERT message, if modified." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (revert-buffer-if-needed each-buffer)
    dis-all))

(DEFUN FILE-LOADED-TRUENAME (PATHNAME)
  "Return the truename of the source of the version of PATHNAME which was LOADed."
  (OR (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
             (SOURCE-PATHNAME (SEND GENERIC-PATHNAME :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)))
        (COND ((STRINGP SOURCE-PATHNAME)                ;Old versions of the compiler
               (SETQ SOURCE-PATHNAME (FS:MERGE-PATHNAME-DEFAULTS SOURCE-PATHNAME PATHNAME)))
              ((CONSP SOURCE-PATHNAME)
               (SETQ SOURCE-PATHNAME (FS:PATHNAME-FROM-COLD-LOAD-PATHLIST SOURCE-PATHNAME))))
        (AND (NOT (NULL SOURCE-PATHNAME))
             (LET ((TYPE-1 (SEND SOURCE-PATHNAME :TYPE))
                   (TYPE-2 (SEND PATHNAME :TYPE)))
               (OR (EQUAL TYPE-1 TYPE-2)
                   (AND (OR (EQ TYPE-1 :UNSPECIFIC)
                            (CL:MEMBER TYPE-1 FS:*ITS-UNINTERESTING-TYPES* :TEST #'STRING=))
                        (OR (EQ TYPE-2 :UNSPECIFIC)
                            (CL:MEMBER TYPE-2 FS:*ITS-UNINTERESTING-TYPES* :TEST #'STRING=)))))
             SOURCE-PATHNAME))
      (LET* ((NEWEST-PATHNAME (SEND PATHNAME :NEW-VERSION :NEWEST))
             (ID (SI:GET-FILE-LOADED-ID NEWEST-PATHNAME PACKAGE)))
        (AND ID (CAR ID)))))

(DEFCOM COM-NOT-MODIFIED "Pretend that this buffer has not been modified." ()
  (NOT-MODIFIED *INTERVAL*)
  (FORMAT *QUERY-IO* "~&Not modified")
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Unmod" NOT-MODIFIED NIL
                          "Mark this buffer as not modified.")

(DEFCOM COM-TOGGLE-READ-ONLY "Make the current buffer read only, or make it modifiable." ()
  (IF (BUFFER-READ-ONLY-P *INTERVAL*)
      (MAKE-BUFFER-NOT-READ-ONLY *INTERVAL*)
    (MAKE-BUFFER-READ-ONLY *INTERVAL*))
  DIS-NONE)

(DEFCOM COM-LIST-BUFFERS "Print a list of the all buffers and their files (or sizes)." ()
  (LET* ((STAR-FLAG NIL) (PLUS-FLAG NIL) (EQV-FLAG NIL) (CIRCLE-PLUS-FLAG NIL)
         (MAX-SIZE (MIN 40. (- (SEND *STANDARD-OUTPUT* :SIZE-IN-CHARACTERS) 40.)))
         (VERSION-POS (MIN (MAX (+ (FIND-MAXIMUM-BUFFER-NAME-LENGTH MAX-SIZE) 3) 16.)
                           (+ MAX-SIZE 2))))
    (FORMAT T "~&Buffers in ZWEI:~%  Buffer name:~vTFile Version:~vTMajor mode:~2%"
            VERSION-POS (+ VERSION-POS 15.))
    (DOLIST (BUFFER (buffer-list-as-selected))
      (LET ((FILE-ID (BUFFER-FILE-ID BUFFER))
            (NAME) (FLAG))
        (WRITE-STRING (COND ((EQ FILE-ID T)
                             (SETQ PLUS-FLAG T) "+ ")   ;+ means new file, never written out
                            ((BUFFER-READ-ONLY-P BUFFER)
                             (SETQ EQV-FLAG T) "")     ; means read-only
                            ((BUFFER-MODIFIED-P BUFFER)
                             (SETQ STAR-FLAG T) "* ")   ;* means has unsaved changes.
                            (T "  "))                   ;blank if unmodified.
                      *STANDARD-OUTPUT*)
        (MULTIPLE-VALUE-SETQ (NAME FLAG)
          (NAME-FOR-DISPLAY BUFFER MAX-SIZE))
        (IF FLAG (SETQ CIRCLE-PLUS-FLAG FLAG))
        (LET ((MAJOR-MODE (BUFFER-MAJOR-MODE BUFFER)))
          (SEND *STANDARD-OUTPUT* :ITEM 'ZMACS-BUFFER BUFFER
                "~A~vT~:[ [~D Line~:P]~*~;~*~@[~A~]~]~vT(~A)"
                NAME VERSION-POS
                FILE-ID
                (AND (NULL FILE-ID) (COUNT-LINES-BUFFER BUFFER))
                (IF (MEMQ MAJOR-MODE '(DIRED-MODE BDIRED-MODE))
                    (BUFFER-PATHNAME BUFFER)
                  (BUFFER-VERSION-STRING BUFFER))
                (+ VERSION-POS 20.)
                (SYMEVAL MAJOR-MODE)))
        (TERPRI *STANDARD-OUTPUT*)))
    (TERPRI *STANDARD-OUTPUT*) ;extra TERPRI to show you that it's finished.
    (AND PLUS-FLAG (WRITE-STRING "+ means new file.  " *STANDARD-OUTPUT*))
    (AND STAR-FLAG (WRITE-STRING "* means buffer modified.  " *STANDARD-OUTPUT*))
    (AND EQV-FLAG (WRITE-STRING " means read-only.  " *STANDARD-OUTPUT*))
    (AND CIRCLE-PLUS-FLAG (WRITE-STRING "  means name truncated." *STANDARD-OUTPUT*))
    (AND (OR PLUS-FLAG STAR-FLAG EQV-FLAG CIRCLE-PLUS-FLAG) (TERPRI *STANDARD-OUTPUT*))
    DIS-NONE))

(defun buffer-list-as-selected ()
  (if *history-buffer-lists-per-window*
      (history-list (send *window* :buffer-history))
    *ZMACS-BUFFER-LIST*)
        ;;Changed back to *ZMACS-BUFFER-LIST* because that is more usable for the poor loser.
      ;; i.e. expecting him to remember the order of 69 buffer history lists is unreasonable,
      ;; while remembering one global list has some hope.  The idea of having separate buffer
      ;; list for each buffer should probably be flushed --rg. 1/24/86
      ;;was *ZMACS-BUFFER-LIST* -- same thing, different ordering
      ;; RMS changed this to use the current window's order
      ;; because that is the order that all other commands use.
      ;; Anyone who thinks it should be different ought to
      ;; explain what reason he thinks outweighs this.
  )

(DEFUN NAME-FOR-DISPLAY (BUFFER MAX-SIZE &AUX (NAME (BUFFER-NAME BUFFER)))
  "Returns a ZMacs buffer name string suitable for displaying within MAX-SIZE
characters (its string-length will be <= MAX-SIZE)."
  (IF (<= (STRING-LENGTH NAME) MAX-SIZE) NAME
    (VALUES (STRING-APPEND (SUBSTRING NAME 0 (max (- MAX-SIZE 2) 0)) " ") T)))

(DEFUN FIND-MAXIMUM-BUFFER-NAME-LENGTH (MAX-SIZE &optional (buffer-list *zmacs-buffer-list*))
  (LOOP FOR BUFFER IN buffer-list
        FOR SIZE = (STRING-LENGTH (BUFFER-NAME BUFFER))
        MAXIMIZE (MIN MAX-SIZE (STRING-LENGTH (BUFFER-NAME BUFFER)))))

(DEFUN COUNT-LINES-BUFFER (BUFFER &AUX TICK TEM LINES)
  "Return the number of lines in BUFFER.
The value is remembered, and if this is called again and BUFFER has not changed
the remembered value is used again."
  (COND ((AND (NUMBERP (SETQ TICK (NODE-TICK BUFFER)))
              (SETQ TEM (SEND BUFFER :GET 'LAST-LINE-COUNT))
              (= (CADR TEM) TICK))
         (CAR TEM))
        (T
         (SETQ LINES (COUNT-LINES (INTERVAL-FIRST-BP BUFFER)
                                  (INTERVAL-LAST-BP BUFFER) T)
               TEM (LIST LINES TICK))
         (SEND BUFFER :PUTPROP TEM 'LAST-LINE-COUNT)
         LINES)))

(DEFCOM COM-KILL-OR-SAVE-BUFFERS "Put up a choice window various buffer operations." ()
  (LET ((BUFFER-ALIST
          (DO ((BUFFER-LIST *ZMACS-BUFFER-LIST* (CDR BUFFER-LIST))
               (RET NIL) (TEM)
               (BUFFER) (FILE-ID))
              ((NULL BUFFER-LIST) RET)
            (SETQ BUFFER (CAR BUFFER-LIST))
            (SETQ TEM (STRING-APPEND "  " (BUFFER-NAME BUFFER))
                  FILE-ID (BUFFER-FILE-ID BUFFER))
            (SETF (CHAR TEM 0)
                  (COND ((EQ FILE-ID T)
                         #/+)
                        ((BUFFER-READ-ONLY-P BUFFER)
                         #/)
                        ((BUFFER-MODIFIED-P BUFFER)
                         #/*)
                        (T
                         #/SP)))
            (LET ((BASIC-CHOICES (IF (BUFFER-NEEDS-SAVING-P BUFFER)
                                     '((:SAVE T) :KILL :NOT-MODIFIED)
                                   '(:SAVE :KILL :NOT-MODIFIED))))
              (PUSH (LIST BUFFER TEM
                          (IF (AND (BUFFER-PATHNAME BUFFER)
                                   (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                                   ;(EQ (GET (SEND BUFFER ':MAJOR-MODE) 'EDITING-TYPE) ':LISP)
                                   )
                              (APPEND BASIC-CHOICES '(:COMPILE))
                            BASIC-CHOICES))
                    RET))))
        CHOICES EXIT-REASON)
    (SETQ BUFFER-ALIST
          (SORT BUFFER-ALIST (LAMBDA (X Y &AUX STR1 STR2 CH1 CH2)
                               (IF (CHAR= (SETQ CH1 (CHAR (SETQ STR1 (CADR X)) 0))
                                          (SETQ CH2 (CHAR (SETQ STR2 (CADR Y)) 0)))
                                   (STRING-LESSP STR1 STR2)
                                 (< (CASE CH1
                                      (#/* 0)
                                      (#/+ 1)
                                      (#/SP 2)
                                      (#/ 3))
                                    (CASE CH2
                                      (#/* 0)
                                      (#/+ 1)
                                      (#/SP 2)
                                      (#/ 3)))))))
    (SETF (VALUES CHOICES EXIT-REASON)
          (TV:MULTIPLE-CHOOSE "  Buffer" BUFFER-ALIST
                              '((:SAVE "Save" NIL (:NOT-MODIFIED) NIL NIL)
                                (:KILL "Kill" NIL (:NOT-MODIFIED) NIL NIL)
                                (:NOT-MODIFIED "UnMod"
                                               NIL (:SAVE :KILL) NIL NIL)
                                (:COMPILE "Compile" NIL NIL NIL NIL))))
    (IF EXIT-REASON
        NIL
      ;; Make sure the current buffer gets done last
      (LET ((ELEM (ASSQ *INTERVAL* CHOICES)))
        (AND ELEM (SETQ CHOICES (NCONC (DELQ ELEM CHOICES) (NCONS ELEM)))))
      (DOLIST (CHOICE CHOICES)
        (LET ((BUFFER (CAR CHOICE)))
          (IF (MEMQ ':SAVE (CDR CHOICE))
              (SAVE-BUFFER BUFFER))
          (IF (MEMQ ':COMPILE (CDR CHOICE))
              (COMPILE-FILE (BUFFER-PATHNAME BUFFER)
                            :SET-DEFAULT-PATHNAME NIL
                            :PACKAGE (BUFFER-PACKAGE BUFFER)))
          (IF (MEMQ ':NOT-MODIFIED (CDR CHOICE))
              (SETF (BUFFER-TICK BUFFER) (TICK)))
          (IF (MEMQ ':KILL (CDR CHOICE))
              (KILL-BUFFER BUFFER T))))
      (FORMAT *QUERY-IO* "~&Done.")))
  DIS-NONE)

;;; These are commands in ZWEI to allow printing  of buffers.
;;; Print Buffer sends a buffer to the Dover.  The buffer name is received from a minibuffer.
;;; Quick Print Buffer sends the current buffer to the Dover (use this on keys).
;;; Print All Buffers asks for each buffer whether it is to be printed.  It then prints
;;;   all the buffers the user asked for.

(DEFCOM COM-QUICK-PRINT-BUFFER "Prints the current buffer on the default hardcopy device." ()
  (FORMAT *QUERY-IO* "~&Attempting transmission: ")
  (PRINT-BUFFER-1 *INTERVAL*)
  DIS-NONE)

(DEFCOM COM-PRINT-BUFFER "Prints a buffer on the default hardcopy device." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Print buffer:" *INTERVAL* NIL)))
    (FORMAT *QUERY-IO* "~&Attempting transmission: ")
    (PRINT-BUFFER-1 BUFFER))
  DIS-NONE)

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#559 on 26-Mar-87 18:46:54
(DEFCOM COM-PRINT-REGION "Prints the region on the default hardcopy device." ()
  (REGION (BP0 BP1)
    (LET* ((INTERVAL (MAKE-INTERVAL BP0 BP1))
           (FONTS (SEND *INTERVAL* :GET-ATTRIBUTE :FONTS))
           (STREAM (IF (ATOM FONTS)
                       (ZWEI:INTERVAL-STREAM INTERVAL)
                     (ZWEI:INTERVAL-STREAM INTERVAL NIL NIL T))))
      (SI:HARDCOPY-STREAM STREAM
                          :FILE-NAME
                          (FORMAT:OUTPUT NIL
                            "ZWEI Buffer "
                            ;; Print the pathname, or else the buffer name.
                            (princ (or (safe-get-zwei-buffer-instance-variable *interval* 'zwei:pathname)
                                       (safe-get-zwei-buffer-instance-variable *interval* 'zwei:name)
                                       "Chunk"))
                          ;; Print last visited version number, if one is recorded.
                            (when
                              (and (safe-get-zwei-buffer-instance-variable *interval* 'zwei:file-id)
                                   (TYPEP (CAR-SAFE (BUFFER-FILE-ID *INTERVAL*)) 'FS:PATHNAME))
                              (FORMAT *query-io* " (~D)"
                                      (SEND (CAR (BUFFER-FILE-ID *INTERVAL*)) :VERSION)))
                            :TV-FONTS (IF (ATOM FONTS) (LIST FONTS) FONTS)))))
  (FORMAT *query-io* " -- Done.")
  DIS-NONE)

(DEFCOM COM-PRINT-ALL-BUFFERS "Query print all buffers on the default hardcopy device." ()
  (LET* ((*QUERY-IO* *STANDARD-OUTPUT*)
         (ANSWERS (MAPCAR #'(LAMBDA (BUFFER)
                              (Y-OR-N-P (FORMAT NIL "Print ~A? (Y or N) "
                                                (BUFFER-NAME BUFFER))))
            *ZMACS-BUFFER-LIST*)))
    (MAPC #'(LAMBDA (BUFFER ANSWER)
              (COND (ANSWER
                     (PRINT-BUFFER-1 BUFFER *STANDARD-OUTPUT*)
                     (FORMAT *QUERY-IO* "~&Buffer ~A printed." (BUFFER-NAME BUFFER)))))
          *ZMACS-BUFFER-LIST*
          ANSWERS))
  (FORMAT T "~&Done.")
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Print" PRINT-BUFFER-1 NIL
                          "Print a hardcopy of this buffer.")

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#559 on 26-Mar-87 18:46:56
(DEFUN PRINT-BUFFER-1 (INTERVAL &OPTIONAL (*STANDARD-OUTPUT* *QUERY-IO*))
  "Print INTERVAL's contents on default printer.
The value supplied for *STANDARD-OUTPUT* is used for printing
notes about the progress of the printing."
  (LET* ((FONTS (SEND INTERVAL :GET-ATTRIBUTE ':FONTS))
         (STREAM (IF (ATOM FONTS)
                     (ZWEI:INTERVAL-STREAM INTERVAL)
                   (ZWEI:INTERVAL-STREAM INTERVAL NIL NIL T))))
    (SI:HARDCOPY-STREAM STREAM
                        :FILE-NAME
                        (FORMAT:OUTPUT NIL
                          "ZWEI Buffer "
                          ;; Print the pathname, or else the buffer name.
                          (princ (or (safe-get-zwei-buffer-instance-variable *interval* 'zwei:pathname)
                                     (safe-get-zwei-buffer-instance-variable *interval* 'zwei:name)
                                     " "))
                          ;; Print last visited version number, if one is recorded.
                          (when
                            (and (safe-get-zwei-buffer-instance-variable *interval* 'zwei:file-id)
                                 (TYPEP (CAR-SAFE (BUFFER-FILE-ID INTERVAL)) 'FS:PATHNAME))
                            (FORMAT *query-io* " (~D)"
                                    (SEND (CAR (BUFFER-FILE-ID INTERVAL)) :VERSION))))))
  (FORMAT *query-io* " -- Done.")
  T)                                            ;If called from mouse click, preserve the buffer list on the screen.

(DEFCOM COM-DISSOCIATED-PRESS "Dissociate the text in a buffer.
The numeric argument is the number of characters of overlap;
or, if negative, minus the number of words of overlap.
The output appears on the terminal; the buffer is not modified.
To put the output in a buffer, use Execute Command into Buffer
after selecting the buffer that the output should go in." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (LET* ((FORWARD-FUNCTION (IF (MINUSP *NUMERIC-ARG*) 'FORWARD-WORD 'FORWARD-CHAR))
         (OVERLAP-SIZE (IF *NUMERIC-ARG-P* (ABS *NUMERIC-ARG*) 2))
         (BUFFER (READ-BUFFER-NAME
                   (FORMAT NIL "Buffer to dissociate: (Overlap = ~D ~:[character~P~;word~P~])"
                          OVERLAP-SIZE (MINUSP *NUMERIC-ARG*) OVERLAP-SIZE)
                   *INTERVAL* NIL))
         (NLINES (DO ((COUNT 0 (1+ COUNT))
                      (LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER)) (LINE-NEXT LINE))
                      (END-LINE (BP-LINE (INTERVAL-LAST-BP BUFFER))))
                     ((EQ LINE END-LINE) COUNT)))
         (NLINES-OVER-TEN (FLOOR NLINES 10.))
         (CURRENT-P (EQ BUFFER *INTERVAL*))
         (POINT (IF CURRENT-P (POINT) (BUFFER-SAVED-POINT BUFFER)))
         (*INTERVAL* BUFFER)
         TENTHS)
    ;; Set up in TENTHS a list of ten lines, distributed at tenths of the buffer.
    (IF (ZEROP NLINES-OVER-TEN)
        (SETQ TENTHS (LIST (INTERVAL-FIRST-BP BUFFER)))
      (DO ((COUNT 0 (1+ COUNT))
           (LINE (BP-LINE (INTERVAL-FIRST-BP BUFFER)) (LINE-NEXT LINE))
           (END-LINE (BP-LINE (INTERVAL-LAST-BP BUFFER))))
          ((EQ LINE END-LINE))
        (IF (ZEROP (\ COUNT NLINES-OVER-TEN))
            (PUSH (CREATE-BP LINE 0) TENTHS)))
      (SETQ TENTHS (NREVERSE TENTHS)))
    (DO-FOREVER
      ;; Wrap around if at end; otherwise we might get stuck there.
      (IF (BP-= POINT (INTERVAL-LAST-BP BUFFER))
          (MOVE-BP POINT (INTERVAL-FIRST-BP BUFFER)))
      ;; Print and advance over a random amount of stuff.
      (LET ((BP (FUNCALL FORWARD-FUNCTION POINT (FIX (SI:RANDOM-IN-RANGE 2 15.)) T)))
        (SEND *STANDARD-OUTPUT* :STRING-OUT
              (STRING-INTERVAL POINT BP T T))
        (MOVE-BP POINT BP))
      ;; Compute the overlap string -- the last few words or characters.
      (LET ((BP (FUNCALL FORWARD-FUNCTION POINT (- OVERLAP-SIZE) T)))
        (LET ((OVERLAP-STRING (STRING-INTERVAL BP POINT T T))
              (RANDOM-LINE-NUMBER (FIX (SI:RANDOM-IN-RANGE 0 NLINES))))
          ;; Move to a randomly chosen position in the buffer.
          ;; Jump immediately to the correct tenth of the buffer,
          ;; then scan by lines to the chosen line.
          (MOVE-BP POINT
                   (IF (ZEROP NLINES-OVER-TEN)
                       (INTERVAL-FIRST-BP BUFFER)
                     (NTH (FLOOR RANDOM-LINE-NUMBER NLINES-OVER-TEN) TENTHS)))
          (DO ((COUNT (IF (ZEROP NLINES-OVER-TEN)
                          0
                        (* (FLOOR RANDOM-LINE-NUMBER NLINES-OVER-TEN) NLINES-OVER-TEN))
                      (1+ COUNT))
               (LINE (BP-LINE POINT) (LINE-NEXT LINE))
               (END-COUNT RANDOM-LINE-NUMBER))
              ((= COUNT END-COUNT)
               (MOVE-BP POINT LINE (FIX (SI:RANDOM-IN-RANGE 0 (LINE-LENGTH LINE))))))
          ;; Then search for the overlap string.  At end of buffer, wrap around.
          (MOVE-BP POINT
                   (OR (ZWEI-SEARCH POINT OVERLAP-STRING)
                       (ZWEI-SEARCH (INTERVAL-FIRST-BP BUFFER) OVERLAP-STRING NIL T)))))))
  DIS-NONE)

(defun string-to-banner-list (string &optional (font-name 'fonts:cptfont))
  (let ((fd (fed:FONT-NAME-FONT-DESCRIPTOR font-name))
        (font (symeval font-name)))
    (loop for row from 0 below (font-raster-height font)
          collect (with-output-to-string (row-string)
                    (dotimes (i (string-length string))
                      (let ((cd (aref fd (char-code (char string i)))))
                        (dotimes (col (font-raster-width font))
                          (cond ((and (array-in-bounds-p cd row col)
                                      (not (zerop (aref cd row col))))
                                 (send row-string :tyo #/*))
                                (t
                                 (send row-string :tyo #/space))))))))))

(defcom com-banner-region "Convert region to banner." ()
  (region (bp1 bp2)
    (with-undo-save ("Banner Region" bp1 bp2)
      (let ((string (string-interval bp1 bp2)))
        (kill-interval bp1 bp2)
        (let ((banner-list (string-to-banner-list string))
              (new-bp (copy-bp bp1 :moves)))
          (dolist (str banner-list)
            (insert-moving new-bp str)
            (insert-thing new-bp #/newline)))))
    dis-text))


(DEFCOM COM-KILL-BUFFER "Kill a specified buffer.
Reads the name of the buffer to kill from the mini-buffer." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to kill (RETURN to kill current buffer):"
                                  *INTERVAL*)))
    (KILL-BUFFER BUFFER))
  DIS-NONE)


(defun replace-current-buffer-with-file (buffer pathname)
  (without-interrupts
      (setq *zmacs-buffer-name-alist*
            (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
      (send buffer :kill)
      (let* ((new-buffer
               (load-file-into-zmacs pathname)))
        (send new-buffer :select))))

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#558 on 2-Oct-86 02:29:24
(defcom com-remove-buffer-quick "Kill current buffer and select previous buffer." ()
  (send *interval* ':kill)
  (send (previous-buffer) ':select)
  dis-none)

(DEFCOM COM-KILL-SOME-BUFFERS "Offer to kill each buffer.
For each buffer, ask whether to kill it, and for each one to be killed, offer to write
out any changes." ()
  (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
    (DOLIST (BUFFER (APPEND (REMOVE *INTERVAL* *ZMACS-BUFFER-LIST*) (NCONS *INTERVAL*)))
      (AND (FQUERY () "Buffer ~A ~A, kill it? "
                   (BUFFER-NAME BUFFER)
                   (COND ((BP-= (INTERVAL-FIRST-BP BUFFER)
                                (INTERVAL-LAST-BP BUFFER))
                          "is empty")
                         ((NULL (BUFFER-FILE-ID BUFFER))
                          "has no file associated with it")
                         ((EQ (BUFFER-FILE-ID BUFFER) T)
                          "is a new file")
                         ((BUFFER-NEEDS-SAVING-P BUFFER)
                          "has been edited")
                         (T "is unmodified")))
           (KILL-BUFFER BUFFER))))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Kill" KILL-BUFFER NIL
                          "Kill this buffer.")


(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  "Kill BUFFER; remove it from the list which can be selected.
Offers to save it if it is a modified file buffer, unless NO-SAVE-P."
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (NOT NO-SAVE-P)
       (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (CONSP (BUFFER-FILE-ID BUFFER))
           (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))))
       (FQUERY '(:BEEP T
                 :TYPE :READLINE
                 :CHOICES #,FORMAT:YES-OR-NO-P-CHOICES)
               "Buffer ~A has been modified, save it first? "
               (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (when (EQ BUFFER *INTERVAL*)
    ;; Cannot select, or be prompted with, the name of the buffer being killed
    (let* ((*zmacs-buffer-name-alist*
            (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
           (new-buffer
              (SELECT-BUFFER
                "Killing the current buffer, select which other buffer?"
                'MAYBE)))
      (MUST-REDISPLAY *WINDOW* new-buffer)))
  ;; Anybody who refers to this buffer should be redirected.
  (SEND BUFFER :KILL)
  T)

(DEFCOM COM-APPEND-TO-BUFFER "Append region to the specified buffer.
The name of the buffer is read from the kbd; it is created if non-existent.
With an argument, we /"prepend/" instead.  Inserts the text at that buffer's
point, but when prepending leaves the point before the inserted text." ()
  (REGION (BP1 BP2)
    (LET ((POINT) (MARK)
          (BUFFER (READ-BUFFER-NAME
                    (IF *NUMERIC-ARG-P* "Prepend to buffer:" "Append to buffer:")
                    NIL T)))
      (COND ((EQ BUFFER *INTERVAL*)
             (BARF "That is the current buffer.")))
      (SETQ POINT (SEND BUFFER :POINT)
            MARK (SEND BUFFER :MARK))
      (MOVE-BP MARK (INSERT-INTERVAL POINT BP1 BP2 T))
      (OR *NUMERIC-ARG-P* (SWAP-BPS MARK POINT))
      (MUST-REDISPLAY-BUFFER BUFFER DIS-TEXT)))
  DIS-NONE)

(DEFCOM COM-INSERT-BUFFER "Insert a copy of the specified buffer at point." ()
  (LET ((BUFFER (READ-BUFFER-NAME "Insert buffer:" T))
        (POINT (POINT)) (MARK (MARK)))
    (WITH-UNDO-SAVE ("Insert buffer" POINT POINT T)
      (MOVE-BP MARK (INSERT-INTERVAL POINT BUFFER))
      (LET ((FONTS (SEND BUFFER :GET-ATTRIBUTE ':FONTS)))
        (WHEN FONTS
          (FIXUP-FONTS-INTERVAL FONTS POINT MARK))))
    (OR *NUMERIC-ARG-P* (SWAP-BPS MARK POINT)))
  DIS-TEXT)

(DEFCOM COM-SET-DEFAULT-FILE-NAME "Change the default filename for most file commands" ()
  (READ-DEFAULTED-PATHNAME "Set default file name:" (PATHNAME-DEFAULTS) NIL NIL :NEW-OK)
  DIS-NONE)

(DEFCOM COM-SET-VISITED-FILE-NAME "Change the file associated with this buffer" ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Set visited file name:" (PATHNAME-DEFAULTS)
                                            NIL NIL :NEW-OK)))
    (SET-BUFFER-PATHNAME PATHNAME)
    (SET-BUFFER-FILE-ID *INTERVAL* T)
    (DOLIST (W (SEND *INTERVAL* :WINDOWS))
      (CHANGE-WINDOW-LABEL W)))
  DIS-NONE)

(DEFCOM COM-RENAME-BUFFER "Rename the current buffer" ()
  (LET ((STRING (LET ((*MINI-BUFFER-DEFAULT-STRING* (BUFFER-NAME *INTERVAL*)))
                    (TYPEIN-LINE-READLINE "Rename buffer to:"))))
    (IF (EQUAL STRING "")
        (BARF "The null string may not be a buffer name."))
    (RENAME-BUFFER *INTERVAL* STRING)
    (SETQ *ZMACS-BUFFER-NAME* STRING))
  (CHANGE-WINDOW-LABEL *WINDOW*)
  DIS-NONE)

(DEFUN RENAME-BUFFER (BUFFER NEW-NAME)
  "Set BUFFER's name to NEW-NAME, and disconnect it from any visited file."
  (SET-BUFFER-FILE-ID BUFFER NIL)
  (SEND BUFFER :RENAME NEW-NAME))

(DEFCOM COM-EDIT-ZMACS-COMMAND "Edit the function installed on a specified key." ()
  (PROMPT-LINE "Key whose command to edit: ")
  (CLEAR-PROMPTS)
  (ALWAYS-DISPLAY-PROMPTS)
  (DO ((COMTAB *COMTAB*)
       (KEY (INPUT-WITH-PROMPTS *STANDARD-INPUT* :MOUSE-OR-KBD-TYI)
            (INPUT-WITH-PROMPTS *STANDARD-INPUT* :TYI))
       (COMMAND))
      (NIL)
    (RECORD-MINI-BUFFER-VALUE T)
    (SETQ COMMAND (COMMAND-LOOKUP KEY COMTAB))
    (COND ((AND (PREFIX-COMMAND-P COMMAND) (NOT *NUMERIC-ARG-P*))
           (SETQ COMTAB (SYMEVAL-IN-CLOSURE COMMAND 'COMTAB)))
          ((MEMQ COMMAND '(NIL :UNDEFINED))
           (SEND *QUERY-IO* :REMAKE-INCOMPLETE)
           (FORMAT *QUERY-IO* "is not a defined key.")
           (BARF))
          ((AND (SYMBOLP COMMAND) (NOT (FBOUNDP COMMAND)))
           (BARF "~S is not implemented." COMMAND))
          (T
           (SETQ COMMAND (FUNCTION-NAME COMMAND))
           (OR *NUMERIC-ARG-P*
               (LET ((COMPLETION
                       (CASE COMMAND
                         (COM-EXTENDED-COMMAND
                          (GET-EXTENDED-COMMAND "Edit Extended command:" *COMTAB*))
                         (COM-ANY-EXTENDED-COMMAND
                          (GET-ANY-EXTENDED-COMMAND "Edit command:")))))
                 (COND ((STRINGP COMPLETION) (BARF))
                       (COMPLETION (SETQ COMMAND (CDR COMPLETION)))
                       (T
                        (SEND *QUERY-IO* :REMAKE-INCOMPLETE)
                        (FORMAT *QUERY-IO* "(~S)" COMMAND)))))
           (RETURN (EDIT-DEFINITION COMMAND)))))
  DIS-NONE)

(DEFCOM COM-COMPILE-FILE "Compile a file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Compile file:" (PATHNAME-DEFAULTS))))
    (EDITOR-COMPILE-FILE PATHNAME T))
  DIS-NONE)

#+(TARGET LAMBDA)
(DEFCOM COM-FALCON-COMPILE-FILE "Compile a file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Compile file:" (PATHNAME-DEFAULTS))))
    (EDITOR-COMPILE-FILE PATHNAME T :compile-fun #'compiler:compile-file-for-falcon))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Compile File"
                          TYPEOUT-COMPILE-FILE NIL "Compile this buffer's source file.")

#+(TARGET LAMBDA)
(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* ZMACS-BUFFER "Compile File for Falcon"
                          TYPEOUT-COMPILE-FILE-for-Falcon NIL "Compile this buffer's source file for the Falcon.")

(DEFUN TYPEOUT-COMPILE-FILE (BUFFER)
  (UNLESS (BUFFER-PATHNAME BUFFER)
    (BARF "This buffer is not visiting a source file."))
  (EDITOR-COMPILE-FILE (BUFFER-PATHNAME BUFFER) T)
  DIS-NONE)

#+(TARGET LAMBDA)
(DEFUN TYPEOUT-COMPILE-FILE-for-falcon (BUFFER)
  (UNLESS (BUFFER-PATHNAME BUFFER)
    (BARF "This buffer is not visiting a source file."))
  (EDITOR-COMPILE-FILE (BUFFER-PATHNAME BUFFER) T :compile-fun #'compiler:compile-file-for-falcon)
  DIS-NONE)

(DEFUN EDITOR-COMPILE-FILE (PATHNAME &OPTIONAL EVEN-IF-UNCHANGED &key (compile-fun #'compile-file))
  "Compile PATHNAME if it has changed or if EVEN-IF-UNCHANGED.
If PATHNAME is visited in an editor buffer and has changed,
offers to save the buffer first."
  (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
         JUST-WRITTEN
         PKG
         SUCCESS
         (compile-in-roots-prop (get generic-pathname :compile-in-roots))
         (package-from-pathname (get generic-pathname :package))
         (intended-package (cond (package-from-pathname
                                  (find-package package-from-pathname *package*))
                                 (T *package*)))
         (package-from-hierarchy-stuff nil))
    (cond ((and compile-in-roots-prop
                (not (cl:member (si:package-root-name intended-package)
                                compile-in-roots-prop
                                      :test 'string-equal)))
           (cond ((not (= 1 (length compile-in-roots-prop)))
                  (fsignal "The current heirarchy ~S is not among those acceptable ~s."
                           (si:package-root-name intended-package)
                           compile-in-roots-prop))
                 (t (format *query-io*
                            "  Transferring to hierarchy ~s" (car compile-in-roots-prop))
                    (setq package-from-hierarchy-stuff
                          (find-package (si:pkg-name intended-package)
                                        (pkg-find-package (car compile-in-roots-prop))))))))
    (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
      (LET ((BUFFER (CDR ELT)))
        (WHEN (EQ (BUFFER-GENERIC-PATHNAME BUFFER)
                  GENERIC-PATHNAME)
          (SETQ PKG (BUFFER-PACKAGE BUFFER))
          (AND (BUFFER-NEEDS-SAVING-P BUFFER)
               (FQUERY () "Save buffer ~A first? " (BUFFER-NAME BUFFER))
               (PROGN (SETQ JUST-WRITTEN T)
                      (SAVE-BUFFER BUFFER))))))
    (IF (OR EVEN-IF-UNCHANGED
            JUST-WRITTEN
            (AND (FILE-HAS-CHANGED PATHNAME)
                 (Y-OR-N-P (FORMAT NIL "File ~A has changed.  Recompile it? " PATHNAME))))
        (UNWIND-PROTECT
          (PROGN
            (FORMAT *QUERY-IO* "~&Compiling ~A" PATHNAME)
            (funcall compile-fun PATHNAME :PACKAGE (cond (package-from-hierarchy-stuff) (t PKG)))
            (SETQ SUCCESS T))
          (IF SUCCESS
              (FORMAT *QUERY-IO* "~&~A compiled." PATHNAME)
            (FORMAT *QUERY-IO* " -- Compilation aborted."))))))

(DEFUN FILE-HAS-CHANGED (PATHNAME)
  "T if PATHNAME's latest binary is older than its latest source."
  (LET ((SOURCE (OPEN (SEND PATHNAME :NEW-TYPE :LISP) :PROBE))
        (BINARY (OPEN (SEND PATHNAME :NEW-TYPE :QFASL) :PROBE)))
    (AND (NOT (ERRORP SOURCE))
         (OR (ERRORP BINARY)
             (> (SEND SOURCE :CREATION-DATE)
                (SEND BINARY :CREATION-DATE))))))

(DEFCOM COM-MINI-VISITED-FILE "Evaluate a form having to do with the current file." ()
  (EVALUATE-MINI-BUFFER
    (FORMAT NIL "( /"~A/")"
            (DEFAULT-PATHNAME (IF *NUMERIC-ARG-P*
                                  *AUX-PATHNAME-DEFAULTS* *PATHNAME-DEFAULTS*)))
    1))

(DEFCOM COM-VIEW-BUFFER "Look at the contents of the specified buffer" ()
  (LET ((BUFFER (READ-BUFFER-NAME "View buffer:" *INTERVAL*))
        BP CH)
    (PROMPT-LINE "Viewing buffer ~A" (BUFFER-NAME BUFFER))
    (MULTIPLE-VALUE (BP CH)
      (VIEW-BUFFER BUFFER))
    (COND ((EQ CH #/CR)
           (SEND *STANDARD-INPUT* :TYI)
           (OR (EQ BUFFER *INTERVAL*) (MAKE-BUFFER-CURRENT BUFFER))
           (MOVE-BP (POINT) BP)
           DIS-TEXT)
          (T
           DIS-NONE))))

(DEFUN VIEW-BUFFER (BUFFER &AUX (OLD-BUFFER *INTERVAL*))
  (MAKE-BUFFER-CURRENT BUFFER T)
  (WITH-BP (OLD-POINT (POINT) :NORMAL)
    (UNWIND-PROTECT
      (PROGN (REDEFINE-WINDOW-OVERPRINTING-FLAG *WINDOW*
                                                (SEND BUFFER :GET-ATTRIBUTE ':BACKSPACE))
             (REDEFINE-WINDOW-TAB-NCHARS *WINDOW*
                                         (SEND BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))
             (REDEFINE-FONTS *WINDOW*
                             (BUFFER-SAVED-FONT-ALIST BUFFER)
                             (SEND BUFFER :GET-ATTRIBUTE ':VSP))
             (SEND (WINDOW-SHEET *WINDOW*) :SET-LABEL "")
             (VIEW-WINDOW *WINDOW* T))
      (SEND (WINDOW-SHEET *WINDOW*) :SET-LABEL NIL)
      (CHANGE-WINDOW-LABEL *WINDOW*)
      (MOVE-BP (POINT) OLD-POINT)
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (MAKE-BUFFER-CURRENT OLD-BUFFER)
      (REDEFINE-WINDOW-OVERPRINTING-FLAG *WINDOW*
                                         (SEND OLD-BUFFER :GET-ATTRIBUTE ':BACKSPACE))
      (REDEFINE-WINDOW-TAB-NCHARS *WINDOW*
                                  (SEND OLD-BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))
      (REDEFINE-FONTS *WINDOW*
                      (BUFFER-SAVED-FONT-ALIST OLD-BUFFER)
                      (SEND OLD-BUFFER :GET-ATTRIBUTE ':VSP)))))

(DEFCOM COM-PRINT-MODIFICATIONS "Show lines that have changed since file was last saved" ()
  (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
       (BUFFER-TICK (BUFFER-TICK *INTERVAL*))
       (CONTIG-P T)
       (DEFUN-LINE NIL))
      (NIL)
    (AND (PLUSP (LINE-LENGTH LINE))
         (CASE (GET *MAJOR-MODE* 'EDITING-TYPE)
           (:LISP
            (STRING-EQUAL LINE "(DEF" :END1 4))
           (:TEXT
            (STRING-EQUAL LINE ".def" :END1 4))
           (OTHERWISE
            NIL))
         (SETQ DEFUN-LINE LINE))
    (IF ( (LINE-TICK LINE) BUFFER-TICK)        ;Old line?
        (SETQ CONTIG-P NIL)                     ;Yes, remember to type ... next time
        (OR CONTIG-P (SEND *STANDARD-OUTPUT* :LINE-OUT "..."))
        (SETQ CONTIG-P T)
        (COND (DEFUN-LINE
               (AND (NEQ DEFUN-LINE LINE)
                    (SEND *STANDARD-OUTPUT* :LINE-OUT DEFUN-LINE))
               (SETQ DEFUN-LINE NIL)))
        (SEND *STANDARD-OUTPUT* :ITEM 'BP (CREATE-BP LINE 0) "~A" LINE)
        (SEND *STANDARD-OUTPUT* :TYO #/CR))
    (AND (EQ LINE LAST-LINE)
         (RETURN)))
  (SEND *STANDARD-OUTPUT* :LINE-OUT "Done.")
  DIS-NONE)

;;; SRCCOM'ing

(defconstant *ignore-font-flag-in-source-compare* '(#/epsilon 1))

(defmacro with-source-compare-parameter-handling (file-1 file-2 &body body)
  (declare (indentation 2 1))
  `(let ((srccom:*lines-to-print-before* 0.)(srccom:*lines-to-print-after* 0.)
         (srccom:*escape-character-ignore-flag*
           (if (or (getf (fs:file-attribute-list (srccom:file-stream ,file-1)) :fonts)
                   (getf (fs:file-attribute-list (srccom:file-stream ,file-2)) :fonts))
               *ignore-font-flag-in-source-compare*)))
     ,@body))

(DEFCOM COM-SOURCE-COMPARE "Compare two files or buffers.
The output goes on the screen, and also into a buffer named *Source Compare ...*." ()
  (LET (FILE-1 FILE-2 NAME-1 NAME-2 KIND DEFAULT)
    (UNWIND-PROTECT
        (PROGN
          (MULTIPLE-VALUE (FILE-1 NAME-1 Kind DEFAULT)
            (GET-BUFFER-OR-FILE-FILE "Compare" NIL))
          (MULTIPLE-VALUE (FILE-2 NAME-2)
            (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Compare ~A ~A with" Kind NAME-1)
                                     DEFAULT))
          (LET* ((OUTPUT-NAME (FORMAT NIL "*Source Compare ~A // ~A*" NAME-1 NAME-2))
                 (*STANDARD-OUTPUT* (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM OUTPUT-NAME NIL T)))
            (with-source-compare-parameter-handling file-1 file-2
              (SRCCOM:DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 *STANDARD-OUTPUT*)
              (SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2 *STANDARD-OUTPUT* (SRCCOM::QUERY-TYPE))
              (LET ((OUTBUF (FIND-BUFFER-NAMED OUTPUT-NAME)))
                (WHEN OUTBUF
                  (SETF (GET OUTBUF 'SPECIAL-PURPOSE) :SRCCOM-OUTPUT))))
            (FORMAT T "~&Done.")))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE))))
  DIS-NONE)

(DEFUN GET-BUFFER-OR-FILE-FILE (PROMPT &OPTIONAL DEFAULT OLDEST-P)
  (DECLARE (VALUES FILE NAME TYPE DEFAULT BUFFER))
  (IF (FQUERY '(:CHOICES (((T "File.") #/F) ((NIL "Buffer.") #/B)))
              "~A file or buffer: " PROMPT)
      (LET ((PATHNAME (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A ~A" PROMPT "File")
                                               (OR DEFAULT (PATHNAME-DEFAULTS))
                                               NIL (IF OLDEST-P :OLDEST)
                                               :READ NIL)))
        (FILE-RETRY-NEW-PATHNAME (PATHNAME FS:FILE-ERROR)
          (VALUES (SRCCOM::CREATE-FILE PATHNAME) PATHNAME "File" PATHNAME)))
      (LET* ((BUFFER (READ-BUFFER-NAME (FORMAT NIL "~A ~A" PROMPT "Buffer") *INTERVAL*))
             (NAME (BUFFER-NAME BUFFER)))
        (VALUES (SRCCOM::MAKE-FILE
                  :FILE-NAME NAME
                  :FILE-TYPE "Buffer"
                  :FILE-STREAM (INTERVAL-STREAM BUFFER)
                  :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
                                             (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
                                           SI:PKG-KEYWORD-PACKAGE))
                NAME
                "Buffer"
                (AND (BUFFER-FILE-ID BUFFER) (BUFFER-PATHNAME BUFFER))
                BUFFER))))

(DEFCOM COM-SOURCE-COMPARE-CHANGES
  "Compare a buffer vs the buffer's files.  If no changes, clear buffer's modified bit.
The output goes on the screen, and also into a buffer named *Source Compare ...*.
With a numeric argument, perform source compare on the current buffer without query." ()
  (source-compare-changes
    (if *numeric-arg-p*
        *interval*
      (READ-BUFFER-NAME "Compare changes of buffer" *INTERVAL*)))
  dis-none)

(defun source-compare-changes (buffer)
  (LET* ((NAME (BUFFER-NAME BUFFER))
         FILE-1 FILE-2)
    (UNWIND-PROTECT
      (PROGN
        (SETQ FILE-1 (SRCCOM::MAKE-FILE
                       :FILE-NAME NAME
                       :FILE-TYPE "Buffer"
                       :FILE-STREAM (INTERVAL-STREAM BUFFER)
                       :FILE-MAJOR-MODE (INTERN (STRING-UPCASE
                                                  (SYMBOL-VALUE (BUFFER-SAVED-MAJOR-MODE BUFFER)))
                                                SI:PKG-KEYWORD-PACKAGE)))
        (SETQ FILE-2 (SRCCOM::CREATE-FILE (BUFFER-PATHNAME BUFFER)))
        (LET* ((OUTPUT-NAME (FORMAT NIL "*Source Compare Changes of ~A*" NAME))
               (*STANDARD-OUTPUT*
                 (MAKE-BUFFER-WINDOW-OR-BROADCAST-STREAM
                   (FORMAT NIL "*Source Compare Changes of ~A*" NAME) NIL T)))
          (with-source-compare-parameter-handling file-1 file-2
            (SRCCOM:DESCRIBE-SRCCOM-SOURCES FILE-1 FILE-2 *STANDARD-OUTPUT*)
            (COND ((SRCCOM:SOURCE-COMPARE-FILES FILE-1 FILE-2 *STANDARD-OUTPUT*
                                                (SRCCOM::QUERY-TYPE))
                   (SETF (BUFFER-TICK BUFFER) (TICK)))) ;No changes, unmodify buffer.
            (LET ((OUTBUF (FIND-BUFFER-NAMED OUTPUT-NAME)))
              (AND OUTBUF (SETF (GET OUTBUF 'SPECIAL-PURPOSE) :SRCCOM-OUTPUT))))
        (FORMAT T "~&Done.")))
      (AND FILE-1 (SEND (SRCCOM::FILE-STREAM FILE-1) :CLOSE))
      (AND FILE-2 (SEND (SRCCOM::FILE-STREAM FILE-2) :CLOSE)))))


(DEFCOM COM-SOURCE-COMPARE-MERGE-CHANGES
       "Compare current buffer and its associated file, merging differences into the specified buffer" ()
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

(DEFCOM COM-SOURCE-COMPARE-MERGE
  "Compare two files or buffers and merge the differences into the specified buffer" ()
  (LET (FILE-1 FILE-2 NAME-1 NAME-2 TYPE-1 TYPE-2 BUF-1 BUF-2 DEFAULT OUTPUT-BUFFER)
    (UNWIND-PROTECT
      (PROGN
        (MULTIPLE-VALUE (FILE-1 NAME-1 TYPE-1 DEFAULT BUF-1)
          (GET-BUFFER-OR-FILE-FILE "Merge"))
        (MULTIPLE-VALUE (FILE-2 NAME-2 TYPE-2 NIL BUF-2)
          (GET-BUFFER-OR-FILE-FILE (FORMAT NIL "Merge ~A ~A with" TYPE-1 NAME-1) DEFAULT))
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
          (send *query-io* :clear-window)
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

;;; Destructive insertion
(DEFUN REPLACE-INTERVALS (OLD-INTERVAL NEW-INTERVAL)
  (DELETE-INTERVAL OLD-INTERVAL)
  (LET ((FIRST-BP (INTERVAL-FIRST-BP NEW-INTERVAL))
        (LAST-BP (INTERVAL-LAST-BP NEW-INTERVAL)))
    (DOLIST (BP (LINE-BP-LIST (BP-LINE (INTERVAL-FIRST-BP OLD-INTERVAL))))
      (MOVE-BP BP (IF (EQ (BP-STATUS BP) :MOVES) LAST-BP FIRST-BP)))
    (MOVE-BP (INTERVAL-FIRST-BP OLD-INTERVAL) FIRST-BP)
    (MOVE-BP (INTERVAL-LAST-BP OLD-INTERVAL) LAST-BP)
    (DO ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
         (END-LINE (BP-LINE LAST-BP)))
        (NIL)
      (SETF (LINE-NODE LINE) OLD-INTERVAL)
      (IF (EQ LINE END-LINE) (RETURN NIL) NIL))))

(DEFUN SOURCE-COMPARE-MERGE-QUERY (MARKS)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (UNWIND-PROTECT
    (DO (MARK DO-THE-REST PRESERVE-HEADINGS)
        ((NULL MARKS))
      (POP MARKS MARK)
      (SETQ PRESERVE-HEADINGS NIL)
      (UNWIND-PROTECT
        (LET ((BP1 (FIRST MARK)) (BP2 (SECOND MARK)) (BP3 (THIRD MARK)) (BP4 (FOURTH MARK))
              (BP5 (FIFTH MARK)) (BP6 (SIXTH MARK)))
          (UNLESS DO-THE-REST
            (SEND *QUERY-IO* :CLEAR-WINDOW)
            (DO-NAMED ONE-MARK
                      ((REDISPLAY-P T REDISPLAY-NEXT-P)
                       (REDISPLAY-NEXT-P NIL NIL)
                       (DO-IT NIL NIL))
                      (NIL)
              (WHEN REDISPLAY-P
                (MOVE-BP (POINT) BP1)
                (MUST-REDISPLAY *WINDOW* DIS-BPS)
                (LET ((*CENTERING-FRACTION* 0.10s0))
                  (RECENTER-WINDOW *WINDOW* :ABSOLUTE)))
              (REDISPLAY *WINDOW* :POINT)
              (SELECTOR
                (TYPEIN-LINE-ACTIVATE
                  (FORMAT *QUERY-IO* "~&1, 2, *, I, ~\LOZENGED-CHAR\, ~\LOZENGED-CHAR\, !, c-R or ~\LOZENGED-CHAR\: "
                          #/Space #/Rubout #/Help)
                  (CHAR-UPCASE (TYI-WITH-SCROLLING T)))
                CHAR=
                (#/C-G (BARF))
                (#/1 (SETQ DO-IT 1))
                (#/2 (SETQ DO-IT 2))
                (#/* (SETQ DO-IT '*))
                (#/I (SETQ DO-IT 'I))
                (#/MOUSE-1-1
                 (OR (LET ((BP (MOUSE-BP *WINDOW*)))
                       (SETQ DO-IT (COND ((BP-< BP BP2) NIL)
                                         ((BP-< BP BP3) 1)
                                         ((BP-< BP BP4) '*)
                                         ((BP-< BP BP5) 2)
                                         (T NIL))))
                     (BEEP)))
                (#/Space (RETURN NIL))
                (#/Rubout
                 (DELETE-INTERVAL BP2 BP5 T)
                 (RETURN))
                (#/!
                 (SEND *QUERY-IO* :CLEAR-WINDOW)
                 (DO-FOREVER
                   (SELECTOR (TYPEIN-LINE-ACTIVATE
                               (FORMAT *QUERY-IO* "~&Type 1, 2, *, ~\LOZENGED-CHAR\, or ~\LOZENGED-CHAR\: "
                                       #/Rubout #/Help)
                               (CHAR-UPCASE (SEND *STANDARD-INPUT* :TYI)))
                     CHAR=
                     (#/C-G (BARF))
                     (#/1 (SETQ DO-THE-REST 1) (RETURN-FROM ONE-MARK))
                     (#/2 (SETQ DO-THE-REST 2) (RETURN-FROM ONE-MARK))
                     (#/I (SETQ DO-THE-REST 'I) (RETURN-FROM ONE-MARK))
                     (#/* (SETQ DO-THE-REST '*) (RETURN-FROM ONE-MARK))
                     (#/Rubout (progn (send *query-io* :clear-window) (RETURN)))
                     ;;Caution!  This format statement works out to exactly two lines of mini-buffer space.  Altering its
                     ;;control directives could have fatal consequences for display of this help message!
                     (#/Help
                      (FORMAT *QUERY-IO* "~&1: Insert all differences from first source only; ~
                                            2: Insert all differences from second source only; ~
                                          ~%*: Insert all differences; ~
                                            I: Insert all differences, retaining source headings; ~
                             ~\LOZENGED-CHAR\: Abort"
                              #/Rubout)))))
                (#/C-R (CONTROL-R) (SETQ REDISPLAY-NEXT-P T))
                (#/Page (MUST-REDISPLAY *WINDOW* DIS-ALL))
                (#/C-L (MUST-REDISPLAY *WINDOW* (COM-RECENTER-WINDOW)))
                (#/HELP
                 ;;Caution!  This format statement works out to exactly two lines of mini-buffer space.  Altering its
                 ;;control directives could have fatal consequences for display of this help message!
                 (FORMAT *query-io* "~&1: First source; ~
                                       2: Second source; ~
                                       *: Both sources; ~
                                       I: Both versions, with headers; ~
                        ~\LOZENGED-CHAR\: Use both versions w//o confirmation; ~
                      ~%~\LOZENGED-CHAR\: Use neither version; !: Use one or both versions from now on; ~
                               control-R: Edit this buffer."
                         #/Space #/Rubout))
                (OTHERWISE (BEEP)))
              ;; If the command specified which one we want this time, flush the rest.
              (AND DO-IT
                   (LET (OK CONTROL-R-P)
                     (CASE DO-IT
                       (* (MULTIPLE-VALUE (OK CONTROL-R-P)
                            (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP4 BP5 BP6)))
                       (I (MULTIPLE-VALUE (OK CONTROL-R-P)
                            (SOURCE-COMPARE-MERGE-QUERY-1))
                          (AND OK (SETQ PRESERVE-HEADINGS T)))
                       (1 (MULTIPLE-VALUE (OK CONTROL-R-P)
                            (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP3 BP4
                                                          BP4 BP5 BP5 BP6)))
                       (2 (MULTIPLE-VALUE (OK CONTROL-R-P)
                            (SOURCE-COMPARE-MERGE-QUERY-1 BP1 BP2 BP2 BP3
                                                          BP3 BP4 BP5 BP6))))
                     (SETQ REDISPLAY-NEXT-P CONTROL-R-P)
                     OK)
                   (RETURN))))
          ;; If ! was specified this time or a previous time,
          ;; maybe flush one file's stuff.
          (WHEN DO-THE-REST
            (CASE DO-THE-REST
              (* )
              (I (SETQ PRESERVE-HEADINGS T))  ;I => don't flush the *** lines.
              (1 (DELETE-INTERVAL BP4 BP5 T))
              (2 (DELETE-INTERVAL BP2 BP3 T)))
            (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
        ;; Flush the *** lines (unless user typed I) and the permanent BPs.
        (FLUSH-SOURCE-COMPARE-MARK MARK PRESERVE-HEADINGS)))
    ;; Flush all remaining *** lines (unless user typed !I, which sets MARKS to NIL).
    (MAPCAR #'FLUSH-SOURCE-COMPARE-MARK MARKS)))

;; Delete some subintervals, show the user what it looks like.
;; Return T if he likes it that way; otherwise return NIL and reinsert the deleted text.
;; Second value says that the user did a recursive edit within this function,
;; but only if the first value is NIL.
(DEFUN SOURCE-COMPARE-MERGE-QUERY-1 (&REST START-AND-END-BPS &AUX INTS FLAG)
  ;; For each bp pair, get a copy of the text between them.  Make a list of intervals.
  (SETQ INTS (DO ((BPS START-AND-END-BPS (CDDR BPS))
                  (LIST NIL))
                 ((NULL BPS) (NREVERSE LIST))
               (PUSH (COPY-INTERVAL (CAR BPS) (CADR BPS) T) LIST)))
  (UNWIND-PROTECT
    (PROGN
      ;; Delete the text within the bp pairs.
      (DO ((BPS START-AND-END-BPS (CDDR BPS)))
          ((NULL BPS))
        (DELETE-INTERVAL (CAR BPS) (CADR BPS) T))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (REDISPLAY *WINDOW* :POINT)
      (SEND *QUERY-IO* :CLEAR-WINDOW)
      (DO ((CONTROL-R-P NIL)) (NIL)
        (CASE (TYPEIN-LINE-ACTIVATE
                (FORMAT *QUERY-IO* "~&Type ~\LOZENGED-CHAR\ to confirm, ~\LOZENGED-CHAR\ to abort, Control-R to edit."
                        #\Space #\Rubout)
                (CHAR-UPCASE (TYI-WITH-SCROLLING T)))
          (#/C-G (BARF))
          ((#/SP #/MOUSE-1-1) (SETQ FLAG T) (RETURN T))
          ((#/RUBOUT #/MOUSE-2-1) (RETURN (VALUES NIL CONTROL-R-P)))
          (#/C-R (CONTROL-R) (SETQ CONTROL-R-P T))
          (#/HELP (FORMAT *QUERY-IO* "~&~\LOZENGED-CHAR\ confirms this choice, ~\LOZENGED-CHAR\ aborts this choice, ~
                                      ~%Control-R lets you edit this buffer." #/Space #/Rubout))
          (OTHERWISE (BEEP)))))
    (OR FLAG
        ;; If user did not confirm, reinsert the deleted text
        ;; from the copies we made.
        (DO ((BPS START-AND-END-BPS (CDDR BPS))
             (INTS INTS (CDR INTS))
             (BP1) (BP2))
            ((NULL BPS)
             (MUST-REDISPLAY *WINDOW* DIS-TEXT))
          (SETQ BP1 (CAR BPS)
                BP2 (CADR BPS))
          (MOVE-BP BP2 BP1)
          (LET ((TEMP-BP (COPY-BP BP1 :NORMAL)))
            (INSERT-INTERVAL-MOVING BP2 (CAR INTS))
            (MOVE-BP BP1 TEMP-BP)
            (FLUSH-BP TEMP-BP))))))

(DEFUN FLUSH-SOURCE-COMPARE-MARK (MARK &OPTIONAL PRESERVE-HEADINGS)
  (OR PRESERVE-HEADINGS
      (DO MARK MARK (CDDR MARK) (NULL MARK)
          (LET ((BP1 (CAR MARK))
                (BP2 (CADR MARK)))
            (DELETE-INTERVAL BP1 BP2 T))))
  (MAPCAR #'FLUSH-BP MARK))

(DEFUN INITIALIZE-ZMACS-COMTABS ()
  (SETQ *ZMACS-CONTROL-X-COMTAB*
        (SET-COMTAB 'ZMACS-CONTROL-X-COMTAB
                    '(#/C-F COM-FIND-FILE
                      #/C-V COM-FIND-ALTERNATE-FILE
                      #/B COM-SELECT-BUFFER
                      #/C-W COM-WRITE-FILE
                      #/C-S COM-SAVE-FILE
                      #/C-Q COM-TOGGLE-READ-ONLY
                      #/C-B COM-LIST-BUFFERS
                      #/K COM-KILL-BUFFER
                      #/A COM-APPEND-TO-BUFFER
                      #/1 COM-ONE-WINDOW
                      #/2 COM-TWO-WINDOWS
                      #/3 COM-VIEW-TWO-WINDOWS
                      #/4 COM-MODIFIED-TWO-WINDOWS
                      #/^ COM-GROW-WINDOW
                      #/O COM-OTHER-WINDOW
                      #/M COM-MAIL
                      #/D COM-R-DIRED
                      #/V COM-VIEW-FILE
                      #/8 COM-TWO-WINDOWS-SHOWING-REGION
                      #/C-M-L COM-SELECT-DEFAULT-PREVIOUS-BUFFER)))
  (SET-COMTAB-INDIRECTION *ZMACS-CONTROL-X-COMTAB* *STANDARD-CONTROL-X-COMTAB*)
  (SETQ *ZMACS-COMTAB*
        (SET-COMTAB 'ZMACS-COMTAB
                    '(#/C-M-V COM-SCROLL-OTHER-WINDOW
                      #/C-SH-P COM-GO-TO-NEXT-POSSIBILITY
                      #/C-SH-W COM-EDIT-NEXT-WARNING
                      #/M-SH-W COM-EDIT-PREVIOUS-WARNING
                      #/M-~ COM-NOT-MODIFIED
                      #/M-/. COM-EDIT-DEFINITION
                      #/C-M-/. COM-EDIT-ZMACS-COMMAND
                      #/C-M-L COM-SELECT-PREVIOUS-BUFFER)
                    (MAKE-COMMAND-ALIST
                     '(COM-REVERT-BUFFER COM-NOT-MODIFIED COM-VISIT-TAG-TABLE
                       COM-LIST-BUFFERS COM-SAVE-ALL-FILES COM-KILL-SOME-BUFFERS
                       COM-KILL-OR-SAVE-BUFFERS COM-EDIT-BUFFERS COM-BUFFER-EDIT
                       com-create-one-buffer-to-go
                       COM-SPLIT-SCREEN COM-LIST-SECTIONS
                       COM-LIST-TAG-TABLES com-list-tag-buffers COM-SELECT-TAG-TABLE com-current-tag-table
                       COM-SET-DEFAULT-FILE-NAME COM-RENAME-BUFFER
                       COM-SET-VISITED-FILE-NAME
                       COM-TAGS-SEARCH COM-TAGS-QUERY-REPLACE COM-NEXT-FILE
                       COM-TAGS-SEARCH-LIST-SECTIONS
                       COM-EDIT-CALLERS COM-LIST-CALLERS
                       COM-LIST-OBJECT-USERS COM-MULTIPLE-LIST-OBJECT-USERS
                       COM-MULTIPLE-EDIT-CALLERS COM-MULTIPLE-LIST-CALLERS
                       COM-EDIT-OBJECT-USERS COM-MULTIPLE-EDIT-OBJECT-USERS
                       COM-LIST-MATCHING-SYMBOLS COM-FUNCTION-APROPOS COM-SECTIONIZE-BUFFER
                       COM-DESCRIBE-FLAVOR
                       COM-LIST-FLAVOR-COMPONENTS COM-EDIT-FLAVOR-COMPONENTS
                       COM-LIST-FLAVOR-DEPENDENTS COM-EDIT-FLAVOR-DEPENDENTS
                       COM-LIST-FLAVOR-DIRECT-DEPENDENTS COM-EDIT-FLAVOR-DIRECT-DEPENDENTS
                       COM-LIST-FLAVOR-METHODS COM-EDIT-FLAVOR-METHODS
                       COM-LIST-METHODS COM-EDIT-METHODS
                       COM-LIST-COMBINED-METHODS COM-EDIT-COMBINED-METHODS
                       COM-FASL-UPDATE COM-EDIT-ZMACS-COMMAND
                       COM-COMPILE-FILE COM-COMPILE-AND-LOAD-FILE
                       #+(TARGET LAMBDA) COM-COMPILE-FILE-FOR-FALCON
                       COM-REPARSE-ATTRIBUTE-LIST COM-UPDATE-ATTRIBUTE-LIST
                       COM-LIST-FONTS COM-DISPLAY-FONT
                       COM-DIRED COM-BDIRED
                       COM-REAP-FILE COM-CLEAN-DIRECTORY COM-CHANGE-FILE-PROPERTIES
                       COM-EXPUNGE-DIRECTORY COM-CREATE-DIRECTORY COM-CREATE-LINK
                       COM-REMOTE-CONNECT COM-REMOTE-ACCESS
                       COM-MAIL COM-BUG
                       COM-EDIT-WARNINGS COM-EDIT-FILE-WARNINGS COM-EDIT-SYSTEM-WARNINGS
                       COM-INSERT-FILE-WARNINGS COM-INSERT-WARNINGS
                       COM-FIND-SYSTEM-FILES COM-FIND-FILE-NO-SECTIONIZE
                       COM-FIND-FILE-BACKGROUND
                       COM-SELECT-SYSTEM-AS-TAG-TABLE COM-SELECT-ALL-BUFFERS-AS-TAG-TABLE
                       COM-SELECT-SOME-BUFFERS-AS-TAG-TABLE
                       COM-TAGS-MULTIPLE-QUERY-REPLACE
                       COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER COM-WHERE-IS-SYMBOL
                       COM-ZTOP-MODE COM-SELECT-LAST-ZTOP-BUFFER
                       COM-SET-BACKSPACE COM-SET-BASE COM-SET-PACKAGE
                       COM-SET-LOWERCASE COM-SET-NOFILL COM-SET-PATCH-FILE
                       COM-SET-TAB-WIDTH COM-SET-VSP
                       COM-SET-COMMON-LISP COM-SET-READTABLE
                       COM-INDENT-RIGIDLY
                       COM-VIEW-BUFFER COM-INSERT-BUFFER COM-PRINT-MODIFICATIONS
                       COM-COMPILE-CHANGED-SECTIONS COM-COMPILE-BUFFER-CHANGED-SECTIONS
                       COM-EVALUATE-CHANGED-SECTIONS COM-EVALUATE-BUFFER-CHANGED-SECTIONS
                       COM-LIST-CHANGED-SECTIONS COM-EDIT-CHANGED-SECTIONS
                       COM-LIST-BUFFER-CHANGED-SECTIONS COM-EDIT-BUFFER-CHANGED-SECTIONS
                       COM-TAGS-LIST-CHANGED-SECTIONS COM-TAGS-EDIT-CHANGED-SECTIONS
                       COM-TAGS-COMPILE-CHANGED-SECTIONS COM-TAGS-EVALUATE-CHANGED-SECTIONS
                       COM-SOURCE-COMPARE COM-SOURCE-COMPARE-MERGE COM-SOURCE-COMPARE-CHANGES
                       COM-START-PATCH COM-ADD-PATCH COM-FINISH-PATCH COM-CANCEL-PATCH
                       COM-RESUME-PATCH COM-FINISH-PATCH-UNRELEASED COM-RELEASE-PATCH
                       COM-START-PRIVATE-PATCH
                       COM-ADD-PATCH-CHANGED-SECTIONS COM-ADD-PATCH-BUFFER-CHANGED-SECTIONS
                       COM-TEACH-ZMACS
                       ))))
  (SET-COMTAB-INDIRECTION *ZMACS-COMTAB* *STANDARD-COMTAB*)
  (SET-COMTAB *ZMACS-COMTAB*
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *ZMACS-CONTROL-X-COMTAB*)))
  (SET-COMTAB *ZMACS-COMTAB*
              (LIST #/MOUSE-3-1
               (MAKE-MENU-COMMAND '(COM-ARGLIST COM-EDIT-DEFINITION
                                    COM-LIST-CALLERS COM-LIST-SECTIONS COM-LIST-BUFFERS
                                    COM-KILL-OR-SAVE-BUFFERS com-edit-buffers
                                    COM-SPLIT-SCREEN
                                    COM-COMPILE-REGION COM-INDENT-REGION
                                    COM-CHANGE-DEFAULT-FONT COM-CHANGE-FONT-REGION
                                    COM-UPPERCASE-REGION COM-LOWERCASE-REGION
                                    COM-MOUSE-INDENT-RIGIDLY COM-MOUSE-INDENT-UNDER)))))

;;;; External entry points into ZMACS.

(DEFUN LOAD-FILE-INTO-ZMACS (PATHNAME &OPTIONAL (MERGE-PATHNAME-DEFAULTS-P T))
  (LET (*WINDOW*
        (*MODE-LIST-SYNTAX-TABLE* *LIST-SYNTAX-TABLE*)
        (*PRINT-BASE* *PRINT-BASE*)
        (*READ-BASE* *READ-BASE*)
        (*READTABLE* *READTABLE*)
        (PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME)))
    (IF MERGE-PATHNAME-DEFAULTS-P
        (FS:SET-DEFAULT-PATHNAME PATHNAME FS:LOAD-PATHNAME-DEFAULTS))
    (FIND-FILE PATHNAME NIL T)))

(DEFUN LOAD-DIRECTORY-INTO-ZMACS (PATHNAME)
  (LET (*WINDOW* *INTERVAL*
        (*PRINT-BASE* *PRINT-BASE*)
        (*READ-BASE* *READ-BASE*))
    (DIRECTORY-EDIT (FS:MERGE-PATHNAME-DEFAULTS PATHNAME) NIL)))

(DEFUN EDIT-FUNCTIONS (FUNCTIONS)
  (ED (CONS 'FUNCTIONS-TO-BE-EDITED FUNCTIONS)))

(DEFUN ED (&OPTIONAL THING)
  "Select an editor window and look at THING, which
 is normally a function definition to edit.
If THING is T, select a newly created buffer.
If THING is a string or pathname, find that file.
If THING is a list whose CAR is CALLERS-TO-BE-EDITED
 or FUNCTIONS-TO-BE-EDITED,
 make possibilities for them and visit the first one.
If THING is 'ZWEI:RELOAD, reinitialize the editor.
If THING is a function or a function spec, visit that function's definition."
  (when (EQ THING 'RELOAD)
    (return-from ed
      (INITIALIZATIONS '*EDITOR-INITIALIZATION-LIST* T)))
  (LET ((SHEET (FIND-OR-CREATE-IDLE-ZMACS-WINDOW)))
    ;; And select something according to our argument.
    ;; "(ed) or (ed nil) simply enters the editor, leaving you in the same
    ;; state as the last time you were in the editor." --CLtL, p. 442
    (when thing (SEND SHEET :FORCE-KBD-INPUT `(:EXECUTE EDIT-THING ,THING)))
    (SEND SHEET :SELECT)
    (TV:AWAIT-WINDOW-EXPOSURE)))

(DEFUN FIND-OR-CREATE-IDLE-ZMACS-WINDOW ()
  "Return a ZMACS frame that is active and currently waiting for commands at top level."
  (DOLIST (SH (TV:SHEET-INFERIORS TV:DEFAULT-SCREEN)
              (TV:MAKE-WINDOW 'ZMACS-FRAME :ACTIVATE-P T))
    (AND (TYPEP SH 'ZMACS-FRAME)
         (OR (SYMEVAL-IN-STACK-GROUP '*EDITOR-IDLE*
                                     (PROCESS-STACK-GROUP (SEND SH :PROCESS)))
             (= (SYS:SG-CURRENT-STATE (PROCESS-STACK-GROUP (SEND SH :PROCESS)))
                SYS:SG-STATE-AWAITING-INITIAL-CALL))
         (RETURN SH))))

(DEFUN INITIALIZE-ZMACS ()
  (PKG-GOTO 'USER)
  (INITIALIZE-ZWEI-GLOBALS)
  (INITIALIZE-ZMACS-COMTABS)
  (INITIALIZE-MAIL-CONTROL-X-COMTAB)
  (WHEN (VARIABLE-BOUNDP *ALL-ZMACS-WINDOWS*)
    (DOLIST (W *ALL-ZMACS-WINDOWS*)
      (IF (TYPEP (SEND W :SUPERIOR) 'ZMACS-FRAME)
          (SEND (SEND W :SUPERIOR) :DEACTIVATE)
          (SEND W :DEACTIVATE)))
    (SETQ *ALL-ZMACS-WINDOWS* NIL))
  (SETQ *ZMACS-BUFFER-LIST* NIL
        *ZMACS-BUFFER-NAME-ALIST* NIL *ZMACS-TAG-TABLE-ALIST* NIL)
  (SETQ *ZMACS-COMPLETION-AARRAY* (MAKE-ARRAY #o100 :TYPE 'ART-Q-LIST :LEADER-LIST '(0 T)))
  ;; Make command alist (and strings in it) not occupy too many pages.
  (SETQ *COMMAND-ALIST* (COPYALIST *COMMAND-ALIST*))
  (DOLIST (ELT *COMMAND-ALIST*)
    (SETF (CAR ELT) (COPY-SEQ (CAR ELT))))
  ;; Make one frame now.
  (LET ((FRAME (TV:MAKE-WINDOW 'ZMACS-FRAME :ACTIVATE-P T)))
    ;; Make an overlying window now, so that the first View command is faster.
    (LET ((WINDOW (SEND FRAME :EDITOR-WINDOW)))
      (CREATE-OVERLYING-WINDOW WINDOW))))

(ADD-INITIALIZATION "INITIALIZE-ZMACS" '(INITIALIZE-ZMACS) '(:NORMAL)
                    '*EDITOR-INITIALIZATION-LIST*)

(DEFUN INITIALIZE-ZMACS-IF-NECESSARY ()
  "This currently does nothing, so that people's init files don't lose.
If it did, it would do what it says."
  ())

(DEFUN EDIT-THING (THING &AUX (*CURRENT-COMMAND* 'ZED))
  "Select buffer or file according to THING.  Used within the editor.
If THING is T, select a newly created buffer.
If it is a string or pathname, find that file.
If it is a list whose CAR is CALLERS-TO-BE-EDITED
 or FUNCTIONS-TO-BE-EDITED,
 make possibilities for them and visit the first one.
If it is a function or function spec, visit that function's definition."
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COND ((EQ THING T)
         (MAKE-BUFFER-CURRENT (CREATE-ONE-BUFFER-TO-GO)))
        ((TYPEP THING '(OR STRING PATHNAME))
         (FIND-FILE (MAKE-DEFAULTED-PATHNAME (STRING THING) (PATHNAME-DEFAULTS))))
        ((and (typep thing 'list)
              (typep (car-safe thing) '(or symbol string))
              (case (intern (car thing) :keyword)
                (:callers-to-be-edited
                 (LIST-ZMACS-CALLERS-TO-BE-EDITED "callers" NIL T
                                                  (SETUP-ZMACS-CALLERS-TO-BE-EDITED (CDR THING)))
                 t)
                (:functions-to-be-edited
                 (LIST-ZMACS-CALLERS-TO-BE-EDITED "functions waiting to be edited" NIL T
                                                  (SETUP-ZMACS-CALLERS-TO-BE-EDITED (CDR THING)))
                 t)
                (:directory
                 (IF (= (LENGTH (CDR THING)) 1)
                     (DIRECTORY-EDIT (CADR THING))
                   (DIRECTORY-EDIT-MULTIPLE NIL (CDR THING)))
                 t)
                (:mail
                 (APPLY #'ZMACS-COMPOSE-MESSAGE (CDR THING))
                 t)
                (:source-compare-merge
                 (APPLY #'SOURCE-COMPARE-MERGE-1 (CDR THING))
                 t))))
        ((SYS:VALIDATE-FUNCTION-SPEC THING)
         (EDIT-DEFINITION THING))
        ((FUNCTIONP THING T)
         (EDIT-DEFINITION (FUNCTION-NAME THING)))
        (T
         (let ((*print-length* 15.))
           (BARF (format nil "Don't know what to do with ~S" THING))))))

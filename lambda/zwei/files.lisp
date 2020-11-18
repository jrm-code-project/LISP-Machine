;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file contains utility functions for manipulating files, and various
;;; commands to do I/O to intervals.  It does not know about buffers and such,
;;; just intervals.

;;; Get a pathname from the user, return as a pathname actor.
(DEFVAR *READING-PATHNAME-DEFAULTS*)
(DEFVAR *READING-PATHNAME-SPECIAL-TYPE*)
(DEFVAR *READING-PATHNAME-SPECIAL-VERSION*)
(DEFVAR *READING-PATHNAME-DIRECTION*)

(DEFUN READ-DEFAULTED-PATHNAME (PROMPT DEFAULTS
                                &OPTIONAL SPECIAL-TYPE SPECIAL-VERSION (DIRECTION :READ)
                                          (MERGE-IN-SPECIAL-VERSION T) (MERGE-TYPE T)
                                MINI-BUFFER-INITIAL-CONTENTS
                                MINI-BUFFER-INITIAL-CHAR-POS)
  "Read and default a pathname, prompting with PROMPT, which should end with a colon.
DEFAULTS is a defaults-alist used as the defaults, and modified.
DIRECTION should be :READ or :WRITE; used for completion.
The remaining arguments are passed along to MAKE-DEFAULTED-PATHNAME (which see)
MINI-BUFFER-INITIAL-CONTENTS is a string to initialize the mini buffer with,
 MINI-BUFFER-INITIAL-CHAR-POS an index in that string to start the cursor out at."
  (LET* ((STRING (READ-UNDEFAULTED-PATHNAME-STRING PROMPT
                                                   DEFAULTS
                                                   SPECIAL-TYPE
                                                   SPECIAL-VERSION
                                                   DIRECTION
                                                   MERGE-IN-SPECIAL-VERSION
                                                   MINI-BUFFER-INITIAL-CONTENTS
                                                   MINI-BUFFER-INITIAL-CHAR-POS))
         (PATHNAME (MAKE-DEFAULTED-PATHNAME STRING
                                            DEFAULTS
                                            SPECIAL-TYPE
                                            SPECIAL-VERSION
                                            MERGE-IN-SPECIAL-VERSION
                                            MERGE-TYPE)))
    (PUSH-ON-HISTORY (SEND PATHNAME :STRING-FOR-PRINTING) *PATHNAME-ARGUMENT-HISTORY*)
    PATHNAME))

;NIL for now since no longer needed to make period a delimiter
;and it bombs out for C-H-F inside read-buffer-name.
(DEFVAR *TEXT-MODE-WITH-FILENAMES* NIL
  "T => switch to Text mode to read pathnames.")

(DEFUN READ-UNDEFAULTED-PATHNAME-STRING
       (PROMPT *READING-PATHNAME-DEFAULTS*
        &OPTIONAL *READING-PATHNAME-SPECIAL-TYPE*
        *READING-PATHNAME-SPECIAL-VERSION*
        (*READING-PATHNAME-DIRECTION* :READ)
        (MERGE-IN-SPECIAL-VERSION T)
        MINI-BUFFER-INITIAL-CONTENTS
        MINI-BUFFER-INITIAL-CHAR-POS
        &AUX
        (*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
  ;; MERGE-IN-SPECIAL-VERSION is for the case of wanting the default to have :OLDEST, but
  ;; not having pathnames typed in keeping to this.
  (IF (NOT MERGE-IN-SPECIAL-VERSION)
      (SETQ *READING-PATHNAME-SPECIAL-VERSION* NIL))    ;Don't complete from this
  (WITH-STACK-LIST (PROMPT (FORMAT NIL "~A (Default is ~A)" prompt
                                   (FS:DEFAULT-PATHNAME *READING-PATHNAME-DEFAULTS*
                                                        NIL
                                                        *READING-PATHNAME-SPECIAL-TYPE*
                                                        *READING-PATHNAME-SPECIAL-VERSION*))
                           '(:RIGHT-FLUSH " (Completion)"))
    (LET ((STRING (STRING-INTERVAL
                    (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB*
                                                      MINI-BUFFER-INITIAL-CONTENTS
                                                      MINI-BUFFER-INITIAL-CHAR-POS
                                                      PROMPT)))))
      ;; it is now the responsibility of callers to do this
      ;(PUSH-ON-HISTORY STRING *PATHNAME-ARGUMENT-HISTORY*)
      STRING)))         ;Return the correct pathname string

(DEFUN UNDO-ALL-MODES ()                        ;Undoes all the modes currently in effect.
  (DO ()                                        ;It's like this in case the modes have hooks.
      ((NULL *MODE-NAME-LIST*))                 ;do it until they're all off.
    (TURN-OFF-MODE (CAR *MODE-NAME-LIST*))))

(DEFUN READ-DEFAULTED-AUX-PATHNAME (PROMPT &OPTIONAL SPECIAL-TYPE SPECIAL-VERSION
                                                     (DIRECTION :READ))
  "Read and default a pathname, prompting with PROMPT, which should end with a colon.
SPECIAL-TYPE and SPECIAL-VERSION are passed along to MAKE-DEFAULTED-PATHNAME (which see).
DIRECTION should be :READ or :WRITE; used for completion.
*AUX-PATHNAME-DEFAULTS* are used as the defaults."
  (READ-DEFAULTED-PATHNAME PROMPT *AUX-PATHNAME-DEFAULTS* SPECIAL-TYPE SPECIAL-VERSION
                           DIRECTION))

(DEFUN READ-TWO-DEFAULTED-PATHNAMES (PROMPT DEFAULTS &AUX FROM TO)
  "Read and return two pathnames using the minibuffer, defaulting to DEFAULTS.
Prompt with PROMPT, which should fit where a verb would fit."
  (SETQ FROM (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A file:" PROMPT) DEFAULTS)
        TO (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A ~A to:" PROMPT FROM) FROM
                                    NIL NIL :WRITE))
  (VALUES FROM TO))

(DEFUN MAKE-DEFAULTED-PATHNAME (STRING DEFAULTS &OPTIONAL SPECIAL-TYPE SPECIAL-VERSION
                                                          (MERGE-IN-SPECIAL-VERSION T)
                                                          (MERGE-TYPE T))
  "Parse STRING into a pathname with defaults.  STRING is recorded for M-H-Y.
DEFAULTS is a defaults alist; the defaults are set to the specified pathname.
SPECIAL-TYPE is a filetype to use if STRING has no name or type.
If that is NIL, then FS:*NAME-SPECIFIED-DEFAULT-TYPE* is used
 unless MERGE-TYPE is NIL.
SPECIAL-VERSION is a version to use if STRING has no name or version.
If that is NIL, then :NEWEST is used, unless MERGE-TYPE is NIL.
If MERGE-IN-SPECIAL-VERSION is NIL, SPECIAL-VERSION is used
 only if STRING is the empty string."
  (IF (ZEROP (STRING-LENGTH STRING))
      ;; He didn't type anything, use the default.
      (FS:DEFAULT-PATHNAME DEFAULTS NIL SPECIAL-TYPE SPECIAL-VERSION)
    (AND (NOT MERGE-IN-SPECIAL-VERSION)         ;Was only for nullstring case
         (SETQ SPECIAL-VERSION NIL))
    (FS:MERGE-AND-SET-PATHNAME-DEFAULTS STRING DEFAULTS
                                        (OR SPECIAL-TYPE
                                            (AND MERGE-TYPE
                                                 FS:*NAME-SPECIFIED-DEFAULT-TYPE*))
                                        (OR SPECIAL-VERSION (AND MERGE-TYPE :NEWEST)))))

(DEFUN EDITOR-FILE-NAME (FILE-NAME)
  "Given a string or pathname, return pathname and name for buffer.
The first value is the canonicalized pathname to put in the buffer;
the second value is a string to use as the buffer name."
  (AND (STRINGP FILE-NAME)
       (SETQ FILE-NAME (FS:MERGE-PATHNAME-DEFAULTS FILE-NAME *PATHNAME-DEFAULTS*)))
  (SETQ FILE-NAME (SEND FILE-NAME :TRANSLATED-PATHNAME))
  (VALUES FILE-NAME (SEND FILE-NAME :STRING-FOR-EDITOR)))

;;;; Special commands in the pathname mini-buffer

(DEFCOM COM-PATHNAME-COMPLETE "Try to complete the string so far as a pathname" ()
  (LET ((TEM (PATHNAME-COMPLETE)))
    (AND (NULL TEM) (BEEP)))
  DIS-TEXT)

(DEFCOM COM-YANK-DEFAULT-PATHNAME "Insert the default pathname as text." ()
  (INSERT-MOVING (POINT)
                 (SEND (FS:DEFAULT-PATHNAME *READING-PATHNAME-DEFAULTS*)
                       :STRING-FOR-PRINTING))
  DIS-TEXT)

(DEFCOM COM-PATHNAME-COMPLETE-AND-EXIT-IF-UNIQUE
        "Try to complete the string so far as a pathname and return if unique" ()
  (LET ((TEM (PATHNAME-COMPLETE)))
    (COND ((NULL TEM)
           (BEEP))
          ((EQ TEM ':OLD)
           (COND ((WINDOW-READY-P *WINDOW*)
                  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
                  (REDISPLAY *WINDOW* :NONE)))
           (THROW 'RETURN-FROM-COMMAND-LOOP T))))
  DIS-TEXT)

(DEFUN PATHNAME-COMPLETE (&AUX STRING VALUE)
  (SETQ STRING (STRING-APPEND (BP-LINE (POINT))))
  (MULTIPLE-VALUE (STRING VALUE)
    (FS:COMPLETE-PATHNAME *READING-PATHNAME-DEFAULTS* STRING *READING-PATHNAME-SPECIAL-TYPE*
                          *READING-PATHNAME-SPECIAL-VERSION* *READING-PATHNAME-DIRECTION*))
  (DELETE-INTERVAL *INTERVAL*)
  (INSERT-MOVING (POINT) STRING)
  VALUE)

(DEFCOM COM-PATHNAME-LIST-COMPLETIONS "Show possible completions of string so far" ()
  (LET ((COMPLETION-LIST (PATHNAME-LIST-COMPLETIONS)))
    (IF (ERRORP COMPLETION-LIST)
        (BARF COMPLETION-LIST)
      (LET* ((POSS (LOOP FOR PATHNAME IN completion-list
                         COLLECT (CONS (SEND PATHNAME :STRING-FOR-PRINTING) PATHNAME)))
             (LEN (LENGTH POSS)))
        (COND ((ZEROP LEN)
               (FORMAT T
                       "~&There are no possible completions of the text you have typed.~%"))
              ((= LEN 1)
               (FORMAT T
                       "~&The only possible completion of the text you have typed is ")
               (SEND *STANDARD-OUTPUT* :ITEM 'PATHNAME-COMPLETION (CAAR POSS)))
              ((OR (< LEN 50.)
                   (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                     (FQUERY NIL "There are ~D possibilities, do you really want to see them all? "
                             LEN)))
               (FORMAT T
                       "~&These are the possible completions of the text you have typed:~2%")
               (SEND *STANDARD-OUTPUT* :ITEM-LIST 'PATHNAME-COMPLETION
                                       (SORT (MAPCAR #'CAR POSS) #'STRING-LESSP))
               (TERPRI *STANDARD-OUTPUT*))))))
  DIS-TEXT)

(DEFUN PATHNAME-LIST-COMPLETIONS (&OPTIONAL (STRING (STRING-APPEND (BP-LINE (POINT)))))
  (FS:PATHNAME-COMPLETION-LIST
    *READING-PATHNAME-DEFAULTS* STRING *READING-PATHNAME-SPECIAL-TYPE*
    *READING-PATHNAME-SPECIAL-VERSION* :NOERROR))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST*
                          PATHNAME-COMPLETION "Select" SELECT-PATHNAME-COMPLETION T
                          "Use this Pathname.")

;;; Called if the user mouses one of the completions
(DEFUN SELECT-PATHNAME-COMPLETION (string)
  (DELETE-INTERVAL *INTERVAL*)
  (INSERT-MOVING (POINT) STRING)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (REDISPLAY *WINDOW* :NONE)
  (*THROW 'RETURN-FROM-COMMAND-LOOP T))

(DEFCOM COM-DOCUMENT-PATHNAME-READ "Help while getting a pathname" ()
  (FORMAT T "~%You are typing a pathname, in a minibuffer.~%
These special commands are available:
 Type Altmode to complete the pathname as much as possible.
 End attempts completion and exits if it is unique.
 Return exits without completion.

 Control-Shift-Y inserts the defaults as text.
 Control-Meta-Y inserts the last pathname you typed, as text.

Here is what the pathname will be used for:
")
  (AND *MINI-BUFFER-COMMAND-IN-PROGRESS*
       (COM-DOCUMENT-CONTAINING-COMMAND))
  DIS-NONE)

;;;; Various file-related commands on INTERVALs.

(DEFCOM COM-INSERT-FILE "Insert the contents of the specified file at point.
Reads a file name from the mini-buffer, and inserts the contents of that
file at point. Leaves mark at the end of inserted text, and point at the
beginning, unless given an argument.  Acts like Yank (Control-Y) with respect
to the region." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (MOVE-BP (MARK) (POINT))
  (SETQ *CURRENT-COMMAND-TYPE* ':YANK)
  (LET ((PATHNAME (READ-DEFAULTED-AUX-PATHNAME "Insert file:")))
    (WITH-OPEN-FILE (STREAM PATHNAME :ERROR :RETRY)
      (LET* ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST STREAM))
             (FONTS (GETF ATTRIBUTES ':FONTS))
             (START-POINT (COPY-BP (POINT))))
        (WITH-UNDO-SAVE ("Insert file" (POINT) (POINT) T)
          (DBP START-POINT)
          (MOVE-BP (POINT) (STREAM-INTO-BP STREAM (POINT)
                                           (OR (CDR FONTS)
                                               (GETF ATTRIBUTES ':DIAGRAM))))
          (IBP START-POINT)
          (DOLIST (BP (LINE-BP-LIST (BP-LINE (POINT))))
            (WHEN (AND (BP-= (POINT) BP) (NEQ BP (POINT)) (EQ (BP-STATUS BP) ':NORMAL))
              (MOVE-BP BP START-POINT)))
          (FIXUP-FONTS-INTERVAL FONTS START-POINT (POINT)))))
    (MAYBE-DISPLAY-DIRECTORY :READ PATHNAME))
  (OR *NUMERIC-ARG-P* (SWAP-BPS (POINT) (MARK)))
  DIS-TEXT)

(DEFCOM COM-INSERT-FILE-NO-FONTS "Insert the contents of the specified file at point.
Reads a file name from the mini-buffer, and inserts the contents of that
file at point. Leaves mark at the end of inserted text, and point at the
beginning, unless given an argument.  Acts like Yank (Control-Y) with respect
to the region." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (MOVE-BP (MARK) (POINT))
  (SETQ *CURRENT-COMMAND-TYPE* ':YANK)
  (LET ((PATHNAME (READ-DEFAULTED-AUX-PATHNAME "Insert file:")))
    (WITH-OPEN-FILE (STREAM PATHNAME :ERROR :RETRY)
      (LET ((START-POINT (COPY-BP (POINT))))
        (WITH-UNDO-SAVE ("Insert file" (POINT) (POINT) T)
          (DBP START-POINT)
          (MOVE-BP (POINT) (STREAM-INTO-BP STREAM (POINT)))
          (IBP START-POINT)
          (DOLIST (BP (LINE-BP-LIST (BP-LINE (POINT))))
            (WHEN (AND (BP-= (POINT) BP) (NEQ BP (POINT)) (EQ (BP-STATUS BP) ':NORMAL))
              (MOVE-BP BP START-POINT))))))
    (MAYBE-DISPLAY-DIRECTORY :READ PATHNAME))
  (OR *NUMERIC-ARG-P* (SWAP-BPS (POINT) (MARK)))
  DIS-TEXT)

(DEFCOM COM-WRITE-REGION-TO-FILE "Write out the region to the specified file." ()
  (REGION (BP1 BP2)
    (LET ((PATHNAME (READ-DEFAULTED-AUX-PATHNAME "Write region to:"
                                                 NIL NIL :WRITE))
          ;; Examine the buffer's current mode line.
          ;; If the user has edited in a Fonts: property,
          ;; save the font information, even if he has failed to do
          ;; Reparse Mode Line.
          (PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM *INTERVAL*))))
      (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :OUTPUT :ERROR :RETRY)
        (STREAM-OUT-INTERVAL STREAM BP1 BP2 T
                             (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
                                 (SEND *INTERVAL* :GET-ATTRIBUTE ':FONTS)
                                 (SEND *INTERVAL* :GET-ATTRIBUTE ':DIAGRAM)))
        (CLOSE STREAM)
        (PRINT-FILE-WRITTEN STREAM))))
  DIS-NONE)

(DEFUN PRINT-FILE-WRITTEN (STREAM &AUX (TRUENAME (SEND STREAM :TRUENAME)))
  (FORMAT *QUERY-IO* "~&Written: ~A" TRUENAME)
  (IF (OPERATION-HANDLED-P STREAM :READ-POINTER)
      (LET ((NCHARS (SEND STREAM :READ-POINTER)))
        (IF (< NCHARS 5000.)
            (FORMAT *QUERY-IO* " -- ~D characters." NCHARS)
          (FORMAT *QUERY-IO* " -- ~DK characters." (ROUND NCHARS 1024.))
          )
          )))

(DEFCOM COM-APPEND-TO-FILE "Append region to the end of the specified file.
Creates the file if necessary." ()
  (REGION (BP1 BP2)
    (LET ((PATHNAME (READ-DEFAULTED-AUX-PATHNAME "Append region to end of file:"
                                                 NIL NIL :NEW-OK))
          (PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM *INTERVAL*))))
      (WITH-OPEN-FILE-RETRY (OSTREAM (PATHNAME FS:FILE-ERROR) '(:OUT))
        (WITH-OPEN-FILE (ISTREAM PATHNAME '(:IN :NOERROR))
          (IF (ERRORP ISTREAM)
              (IF (CONDITION-TYPEP ISTREAM 'FS:FILE-NOT-FOUND)
                  (FORMAT *QUERY-IO* "~&(New File)")
                (BARF "Error: ~A" ISTREAM)))
          (UNLESS (ERRORP ISTREAM)
            (STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM)))
        (STREAM-OUT-INTERVAL OSTREAM BP1 BP2 T
                             (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
                                 (SEND *INTERVAL* :GET-ATTRIBUTE ':FONTS)
                                 (SEND *INTERVAL* :GET-ATTRIBUTE ':DIAGRAM)))
        (CLOSE OSTREAM)
        (PRINT-FILE-WRITTEN OSTREAM))
      (MAYBE-DISPLAY-DIRECTORY :READ PATHNAME)))
  DIS-NONE)

(DEFCOM COM-PREPEND-TO-FILE "Append region to the beginning of the specified file.
Creates the file if necessary." ()
  (REGION (BP1 BP2)
    (LET ((PATHNAME (READ-DEFAULTED-AUX-PATHNAME "Append region to start of file:"
                                                 NIL NIL :NEW-OK))
          (PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM *INTERVAL*))))
      (WITH-OPEN-FILE-RETRY (OSTREAM (PATHNAME FS:FILE-ERROR) :DIRECTION :OUTPUT)
        (WITH-OPEN-FILE (ISTREAM PATHNAME :ERROR NIL)
          (IF (ERRORP ISTREAM)
              (IF (CONDITION-TYPEP ISTREAM 'FS:FILE-NOT-FOUND)
                  (FORMAT *QUERY-IO* "~&(New File)")
                (BARF "Error: ~A" ISTREAM)))
          (STREAM-OUT-INTERVAL OSTREAM BP1 BP2 T
                               (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
                                   (SEND *INTERVAL* :GET-ATTRIBUTE ':FONTS)
                                   (SEND *INTERVAL* :GET-ATTRIBUTE ':DIAGRAM)))
          (UNLESS (ERRORP ISTREAM)
            (STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM))
          (CLOSE OSTREAM)
          (PRINT-FILE-WRITTEN OSTREAM)))
      (MAYBE-DISPLAY-DIRECTORY :READ PATHNAME)))
  DIS-NONE)

(DEFCOM COM-VIEW-FILE "View contents of a file.
While viewing, you can use Space and Overstrike to scroll forward and backward.
Type Rubout to exit.  You cannot edit; but you can see the beginning
of the file without waiting for the whole file to be read in." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "View file:" (PATHNAME-DEFAULTS))))
    (VIEW-FILE PATHNAME))
  DIS-NONE)

;;; Show the file in the "display window".
;;; The caller should set up a reasonable prompt.
;(DEFUN VIEW-FILE (FILENAME &OPTIONAL (OUTPUT-STREAM *STANDARD-OUTPUT*))
;  (SEND OUTPUT-STREAM :HOME-CURSOR)
;  (SEND OUTPUT-STREAM :CLEAR-REST-OF-LINE)
;  (WITH-OPEN-FILE (STREAM FILENAME :ERROR :RETRY)
;    (STREAM-COPY-UNTIL-EOF STREAM OUTPUT-STREAM))
;  (SEND OUTPUT-STREAM :CLEAR-REST-OF-WINDOW))

(DEFUN VIEW-FILE (PATHNAME &OPTIONAL DELETED-P)
  "/"View/" the text of file PATHNAME.  DELETED-P means allow deleted files.
This means let the user scroll with Space and Overprint.
A window overlying *WINDOW* is used."
  (WITH-OPEN-FILE (STREAM PATHNAME :ERROR :RETRY :PRESERVE-DATES T :DELETED DELETED-P)
    (PROMPT-LINE "Viewing ~A" (SEND STREAM :TRUENAME))
    (VIEW-STREAM STREAM)))

(DEFUN VIEW-STREAM (STREAM &OPTIONAL (WINDOW (CREATE-OVERLYING-WINDOW *WINDOW*))
                           &AUX (INTERVAL (MAKE-INSTANCE 'NODE)))
  "/"View/" the text read from STREAM.
This means let the user scroll with Space and Overstrike.
A window overlying *WINDOW* is used."
  (SEND (WINDOW-SHEET WINDOW) :SET-LABEL "")
  (SEND WINDOW :SET-INTERVAL INTERVAL)
  (TEMPORARY-WINDOW-SELECT (WINDOW)
    (VIEW-WINDOW WINDOW STREAM)))

(DEFCOM COM-DELETE-FILE "Delete a file.
If wildcards are used, many files can be deleted." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Delete file:" (PATHNAME-DEFAULTS))))
    (IF (SEND PATHNAME :WILD-P)
        (LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME))))
          (FORMAT T "~&Files to be deleted:~%")
          (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
          (WHEN (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                  (Y-OR-N-P "Delete them all? "))
            (DOLIST (ELT DIR)
              (CONDITION-CASE (ERROR)
                  (SEND (CAR ELT) :DELETE)
                ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
                 (FORMAT T "~&Deletion failure: ~A" ERROR))))
            (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
          (DELETE-FILE PATHNAME)
        ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
         (BARF VALUE))
        (:NO-ERROR
         (FORMAT *QUERY-IO* "~&~A deleted." (CAAR VALUE))))))
  DIS-NONE)

(DEFCOM COM-UNDELETE-FILE
  "Undelete a file.
If wildcards are used, many files can be undeleted." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Undelete file:" (PATHNAME-DEFAULTS))))
    (IF (SEND PATHNAME :WILD-P)
        (LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME :DELETED))))
          (FORMAT T "~&Files to be undeleted:~%")
          (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
          (WHEN (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                  (Y-OR-N-P "Undelete them all? "))
            (DOLIST (ELT DIR)
              (CONDITION-CASE (ERROR)
                  (SEND (CAR ELT) :UNDELETE)
                ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
                 (FORMAT T "~&Undeletion failure: ~A" ERROR))))
            (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
          (UNDELETE-FILE PATHNAME)
        ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
         (BARF VALUE))
        (:NO-ERROR
         (FORMAT *QUERY-IO* "~&~A undeleted." (CAAR VALUE))))))
  DIS-NONE)

(DEFCOM COM-RENAME-FILE
  "Rename a file.
If wildcards are used, many files can be renamed." ()
  (LET* ((PATHNAME (READ-DEFAULTED-PATHNAME "Rename file:" (PATHNAME-DEFAULTS)))
         (TO-SPEC (READ-UNDEFAULTED-PATHNAME-STRING
                    (FORMAT NIL "Rename file ~A to:" PATHNAME)
                    PATHNAME))
         BUFFERS-CONSIDERED)
    (DECLARE (SPECIAL BUFFERS-CONSIDERED))
    (IF (SEND PATHNAME :WILD-P)
        (LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME)))
              (TO-PATHNAME (FS:MERGE-PATHNAMES TO-SPEC PATHNAME)))
          (FORMAT T "~&Files to be renamed from ~A to ~A:~%" pathname to-pathname)
          (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
          (WHEN (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                  (Y-OR-N-P "Rename them all? "))
            (DOLIST (ELT DIR)
              (CONDITION-CASE (ERROR)
                  (SEND (CAR ELT) :RENAME
                        (SEND PATHNAME :TRANSLATE-WILD-PATHNAME TO-PATHNAME (CAR ELT)))
                ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
                 (FORMAT T "~&Rename failure: ~A" ERROR))
                (:NO-ERROR
                 (RENAME-FILE-1 PATHNAME
                                (SEND PATHNAME :TRANSLATE-WILD-PATHNAME
                                      TO-PATHNAME (CAR ELT))))))
            (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (ERROR OLD-TRUENAME NEW-TRUENAME)
          (RENAME-FILE PATHNAME TO-SPEC)
        ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
         (BARF ERROR))
        (:NO-ERROR
         (FORMAT *QUERY-IO* "~&~A renamed~% to ~A." OLD-TRUENAME NEW-TRUENAME)
         (RENAME-FILE-1 OLD-TRUENAME NEW-TRUENAME)))))
  DIS-NONE)

(DEFUN RENAME-FILE-1 (INPUT-PATHNAME OUTPUT-PATHNAME)
  (DECLARE (SPECIAL BUFFERS-CONSIDERED))
  ;; Offer to rename a buffer visiting this file, no specific version.
  ;; In order to avoid asking the same question for each file version renamed,
  ;; we record buffers that have been asked about and don't ask a second time.
  (LET ((BUF (FIND-FILE-BUFFER (SEND INPUT-PATHNAME :NEW-VERSION :NEWEST))))
    (WHEN (AND BUF (NOT (MEMQ BUF BUFFERS-CONSIDERED)))
      (PUSH BUF BUFFERS-CONSIDERED)
      (IF (FQUERY NIL "~&Rename buffer ~A as well? " BUF)
          (SET-BUFFER-PATHNAME (SEND OUTPUT-PATHNAME :NEW-VERSION :NEWEST) BUF))))
  ;; Offer to rename a buffer visiting this version number specifically.
  (LET ((BUF (FIND-FILE-BUFFER INPUT-PATHNAME)))
    (WHEN (AND BUF (NOT (MEMQ BUF BUFFERS-CONSIDERED)))
      (PUSH BUF BUFFERS-CONSIDERED)
      (WHEN (FQUERY NIL "~&Rename buffer ~A as well? " BUF)
        (SET-BUFFER-PATHNAME OUTPUT-PATHNAME BUF)
        (WHEN (CONSP (BUFFER-FILE-ID BUF))
          (SETF (BUFFER-FILE-ID BUF)
                (CONS OUTPUT-PATHNAME (CDR (BUFFER-FILE-ID BUF)))))))))

(DEFCOM COM-COPY-BINARY-FILE "Copy one file to another, as a binary file.
With a numeric argument, don't copy creation date or author name." ()
  (COPY-FILE-1 NIL *NUMERIC-ARG-P*)
  DIS-NONE)

(DEFCOM COM-COPY-TEXT-FILE "Copy one file to another, as a text file.
With a numeric argument, don't copy creation date or author name." ()
  (COPY-FILE-1 T *NUMERIC-ARG-P*)
  DIS-NONE)

(DEFCOM COM-COPY-FILE "Copy one file to another.
The choice of binary or character mode is made based on
the file's type and contents.
A numeric argument means don't copy creation date or author name." ()
  (COPY-FILE-1 :DEFAULT *NUMERIC-ARG-P*)
  DIS-NONE)

(DEFUN COPY-FILE-1 (COPY-MODE COPY-CREATION-DATE?)
  (LET* ((FILE-TYPE-STRING
           (SELECTQ COPY-MODE
             ((T) "Copy text file")
             ((NIL) "Copy binary file")
             (OTHERWISE "Copy file")))
         (PATHNAME
           (READ-DEFAULTED-PATHNAME (FORMAT NIL "~A:" FILE-TYPE-STRING)
                                    (PATHNAME-DEFAULTS)))
         (TO-SPEC (READ-UNDEFAULTED-PATHNAME-STRING
                    (FORMAT NIL "Copy file ~A to:" PATHNAME)
                    PATHNAME)))
    (IF (SEND PATHNAME :WILD-P)
        (LET ((DIR (CDR (FS:DIRECTORY-LIST PATHNAME)))
              (TO-PATHNAME (FS:MERGE-PATHNAMES TO-SPEC PATHNAME)))
          (PUSH-ON-HISTORY (SEND TO-PATHNAME :STRING-FOR-PRINTING) *PATHNAME-ARGUMENT-HISTORY*)
          (FORMAT T "~&Files to be copied from ~A to ~A:~%" pathname to-pathname)
          (MAPC *DIRECTORY-SINGLE-FILE-LISTER* DIR)
          (WHEN (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                  (Y-OR-N-P "Copy them all? "))
            (DOLIST (ELT DIR)
              (CONDITION-CASE (ERROR)
                  (COPY-FILE (CAR ELT)
                             (SEND PATHNAME :TRANSLATE-WILD-PATHNAME TO-PATHNAME (CAR ELT))
                             :COPY-CREATION-DATE COPY-CREATION-DATE?
                             :COPY-AUTHOR COPY-CREATION-DATE?
                             :CHARACTERS COPY-MODE
                             :REPORT-STREAM *STANDARD-OUTPUT*)
                ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
                 (FORMAT T "~&Copy failure: ~A" ERROR))))
            (FORMAT T "~&Done.~%")))
      (CONDITION-CASE (VALUE)
          (COPY-FILE PATHNAME TO-SPEC
                     :COPY-CREATION-DATE COPY-CREATION-DATE?
                     :COPY-AUTHOR COPY-CREATION-DATE?
                     :CHARACTERS COPY-MODE
                     :REPORT-STREAM *QUERY-IO*)
        ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
         (BARF VALUE))))))

(DEFCOM COM-PRINT-FILE "Print a file on the local hardcopy device." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Print file:" (PATHNAME-DEFAULTS))))
    (FILE-RETRY-NEW-PATHNAME (PATHNAME FS:FILE-ERROR)
      (LET ((*STANDARD-OUTPUT* *QUERY-IO*))
        (HARDCOPY-FILE PATHNAME))))
  DIS-NONE)

(DEFCOM COM-LOAD-FILE "Load a file.  With arg, compile if necessary." ()
  (IF *NUMERIC-ARG-P*
      (COM-COMPILE-AND-LOAD-FILE)
    (LET* ((default (FS:DEFAULT-PATHNAME (PATHNAME-DEFAULTS)))
           (PATHNAME (if (eq (send default :canonical-type) :lisp)
             ;idea here is to let if default to QFASL if in load below.
                         (READ-DEFAULTED-PATHNAME
                           "Load file:"
                           (send default :NEW-PATHNAME
                                 :TYPE NIL
                                 :VERSION NIL)
                           ;; If user omits type, don't default it.
                           NIL NIL :READ T NIL)
                       (read-defaulted-pathname
                         "Load file:"
                         default
                         nil nil :read t t)))
           (BUFFER (FIND-FILE-BUFFER (SEND PATHNAME :SOURCE-PATHNAME)))
           (just-written nil)
           (success nil))
      (AND BUFFER
           (BUFFER-NEEDS-SAVING-P BUFFER)
           (FQUERY () "Save buffer ~A first? " (BUFFER-NAME BUFFER))
           (PROGN (SETQ JUST-WRITTEN T)
                  (SAVE-BUFFER BUFFER)))
      (IF (AND (or just-written (FILE-HAS-CHANGED PATHNAME))
               (Y-OR-N-P (FORMAT NIL "File ~A has changed.  Recompile it? " PATHNAME)))
          (UNWIND-PROTECT
     ;if TYPE left null above, make sure it is LISP when we get to COMPILE-FILE,
     ;  otherwise, it could get randomly defaulted.
              (let ((compile-pathname (if (null (send pathname :type))
                                            (send pathname :new-pathname :type "LISP")
                                          pathname)))
                  (FORMAT *QUERY-IO* "~&Compiling ~A" COMPILE-PATHNAME)
                (COMPILE-FILE COMPILE-PATHNAME :PACKAGE (IF BUFFER (BUFFER-PACKAGE BUFFER)))
                (SETQ SUCCESS T))
            (cond (SUCCESS
                   (FORMAT *QUERY-IO* "~&~A compiled." PATHNAME)
                   (FORMAT *QUERY-IO* "~&~A loaded."
                           (LOAD PATHNAME (IF BUFFER (BUFFER-PACKAGE BUFFER)) NIL NIL T)))
                  (t
                   (FORMAT *QUERY-IO* " -- Compilation aborted."))))
        (FORMAT *QUERY-IO* "~&~A loaded."
                (LOAD PATHNAME (IF BUFFER (BUFFER-PACKAGE BUFFER)) NIL NIL T)))))
  DIS-NONE)

(DEFCOM COM-COMPILE-AND-LOAD-FILE "Load a file, after compiling it if necessary." ()
  (LET* ((PATHNAME (READ-DEFAULTED-PATHNAME "Compile and load file:"
                                            (SEND (FS:DEFAULT-PATHNAME
                                                    (PATHNAME-DEFAULTS))
                                                  :NEW-PATHNAME :TYPE NIL
                                                                :VERSION NIL)
                                            ;; If user omits type, don't default it.
                                            NIL NIL :READ T NIL))
         (SOURCE-PATHNAME (SEND PATHNAME :SOURCE-PATHNAME))
         (BUFFER (FIND-FILE-BUFFER SOURCE-PATHNAME)))
    (EDITOR-COMPILE-FILE SOURCE-PATHNAME T)
    (FORMAT *QUERY-IO* "~&~A loaded."
            (LOAD PATHNAME (IF BUFFER (BUFFER-PACKAGE BUFFER)) NIL NIL T)))
  DIS-NONE)

(defcom com-unfasl-file "Unfasl selected file and visit output" ()
  (let* ((default (fs:default-pathname(pathname-defaults)))
         (file (read-defaulted-pathname "File to UNFASL:"  (send default :new-pathname :type :qfasl) "QFASL"))
         output-file)
    (typein-line "~&UNFASLing /"~A/"" file)
    (setq output-file (si:unfasl-file file))
    (find-file output-file))
  dis-none)


;;;; Directory Listing stuff.

(DEFCOM COM-DISPLAY-DIRECTORY "Display current buffer's file's directory.
Use the directory listing function in the variable Directory Lister.
With an argument, accepts the name of a file to list." ()
  (LET ((PATHNAME (READ-DEFAULTED-WILD-PATHNAME "Display Directory:"
                                                (DEFAULT-PATHNAME)
                                                (NOT *NUMERIC-ARG-P*))))
    (FILE-RETRY-NEW-PATHNAME (PATHNAME FS:FILE-ERROR)
      (FUNCALL *DIRECTORY-LISTER* PATHNAME)))
  DIS-NONE)

(DEFUN READ-DEFAULTED-WILD-PATHNAME (PROMPT &OPTIONAL (DEFAULT (DEFAULT-PATHNAME))
                                     DONT-READ-P)
  "Read and return a pathname, defaulting type and version to :WILD.
DEFAULT is a pathname that provides defaults for the other things.
DONT-READ-P means just return a default, don't read anything."
  (SETQ DEFAULT (SEND DEFAULT :NEW-PATHNAME :TYPE :WILD :VERSION :WILD))
  (OR DONT-READ-P
      (SETQ DEFAULT (READ-DEFAULTED-PATHNAME PROMPT DEFAULT :WILD :WILD)))
  DEFAULT)

(DEFUN MAYBE-DISPLAY-DIRECTORY (TYPE &OPTIONAL (PATHNAME (DEFAULT-PATHNAME)))
  "Do automatic directory display if the user wants it.
TYPE is :READ or :WRITE, saying what sort of file operation
has just been done.  PATHNAME specifies host, device, directory and name to list."
  (COND ((OR (AND (EQ TYPE ':READ) (MEMQ *AUTO-DIRECTORY-DISPLAY* '(:READ T)))
             (AND (EQ TYPE ':WRITE) (MEMQ *AUTO-DIRECTORY-DISPLAY* '(:WRITE T))))
         (SEND *DIRECTORY-LISTER* (SEND PATHNAME :NEW-PATHNAME :TYPE :WILD
                                                               :VERSION :WILD)))))

;;; This is the default directory listing routine
(DEFUN DEFAULT-DIRECTORY-LISTER (PATHNAME)
  "Print a directory listing of PATHNAME in the default manner.
Uses the value of *DIRECTORY-SINGLE-FILE-LISTER* on each element of the directory-list."
  (WITH-OPEN-STREAM (STREAM (FS:DIRECTORY-LIST-STREAM PATHNAME))
    (LET ((NILENTRY (SEND STREAM :ENTRY)))
      (IF (CAR NILENTRY)
          (FERROR NIL "First entry returned by a directory-list stream is not for NIL"))
      ;; What directory did we actually read?
      (SETQ PATHNAME (OR (GET NILENTRY :PATHNAME) PATHNAME))
      (FORMAT T "~&~A~%" PATHNAME)
      (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* NILENTRY)
      (DO-FOREVER
        (LET ((ENTRY (SEND STREAM :ENTRY)))
          (OR ENTRY (RETURN))
          (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* ENTRY)))))
  (FORMAT T "Done.~%"))

;Note that *DIRECTORY-SINGLE-FILE-LISTER* is expected to output lines.

(DEFUN DEFAULT-LIST-ONE-FILE (FILE &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX PATHNAME)
  (COND ((NULL (SETQ PATHNAME (CAR FILE)))
         (COND ((GET FILE :DISK-SPACE-DESCRIPTION)
                (SEND STREAM :LINE-OUT (GET FILE :DISK-SPACE-DESCRIPTION)))
               ((GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS)
                (DO ((FREE (GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS) (CDR FREE))
                     (FLAG T NIL))
                    ((NULL FREE) (SEND STREAM :TYO #/NEWLINE))
                 (FORMAT STREAM "~A #~A=~D" (IF FLAG "Free:" ",") (CAAR FREE) (CDAR FREE))))
               (T
                (SEND STREAM :TYO #/NEWLINE))))
        ((TYPEP STREAM 'INTERVAL-STREAM)
         (LET ((STRING (CREATE-LINE 'ART-STRING 128. NIL)))
           (DEFAULT-LIST-ONE-FILE FILE STRING)
           (SEND STREAM :LINE-OUT STRING)))
        ((OR (NULL STREAM) (STRINGP STREAM))
         (LET ((STRING
                 (OR STREAM (MAKE-ARRAY 128. :TYPE 'ART-STRING :LEADER-LENGTH 1))))
           (SETF (FILL-POINTER STRING) 0)
           (ARRAY-INITIALIZE STRING #/SP 0 (ARRAY-LENGTH STRING))
           (VECTOR-PUSH (IF (GET FILE :DELETED) #/D #/SP) STRING)
           (VECTOR-PUSH #/SP STRING)
           (STRING-NCONC STRING (OR (GET FILE :PHYSICAL-VOLUME) ""))
           (SETF (FILL-POINTER STRING) (1+ (MAX 5 (FILL-POINTER STRING))))
           (STRING-NCONC STRING (SEND PATHNAME :STRING-FOR-DIRED))
           (VECTOR-PUSH #/SP STRING)
           (SETF (FILL-POINTER STRING)
                 (MAX 20. (FILL-POINTER STRING)))
           (LET ((LINK-TO (GET FILE :LINK-TO)))
             (IF LINK-TO
                 (PROGN (STRING-NCONC STRING "=> " LINK-TO " ")
                        (SETF (FILL-POINTER STRING)
                              (MAX 40. (FILL-POINTER STRING))))
               (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
                 (SETF (FILL-POINTER STRING)
                       (MAX 23. (FILL-POINTER STRING)))
                 (COND ((NULL LENGTH)
                        (STRING-NCONC STRING "     "))
                       ((> LENGTH 999.)
                        (SETF (FILL-POINTER STRING)
                              (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                                 (FILL-POINTER STRING) 4))
                        (VECTOR-PUSH #/SP STRING))
                       (T
                        (SETF (FILL-POINTER STRING)
                              (MAX 24. (FILL-POINTER STRING)))
                        (SETF (FILL-POINTER STRING)
                              (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                                 (FILL-POINTER STRING) 3))
                        (VECTOR-PUSH #/SP STRING))))
               (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
                 (IF (GET FILE :DIRECTORY)
                     (STRING-NCONC STRING "  DIRECTORY")
                   (WHEN LENGTH
                     (SETF (FILL-POINTER STRING)
                           (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                              (FILL-POINTER STRING) 6))
                     (VECTOR-PUSH #/( STRING)
                     (SETF (FILL-POINTER STRING)
                           (NUMBER-INTO-ARRAY STRING (GET FILE :BYTE-SIZE) 10.
                                              (FILL-POINTER STRING)))
                     (VECTOR-PUSH #/) STRING))))
               (SETF (FILL-POINTER STRING)
                     (MAX 39. (FILL-POINTER STRING)))
               (VECTOR-PUSH (COND ((GET FILE :OFFLINE) #/O)
                                  ((GET FILE :NOT-BACKED-UP) #/!)
                                  (T #/SP))
                            STRING)))
           (VECTOR-PUSH (IF (GET FILE :DONT-DELETE) #/@ #/SP) STRING)
           (VECTOR-PUSH (IF (GET FILE :DONT-SUPERSEDE) #/# #/SP) STRING)
           (VECTOR-PUSH (IF (GET FILE :DONT-REAP) #/$ #/SP) STRING)
           (TIME-INTO-ARRAY STRING (GET FILE :CREATION-DATE))
           (LET* ((DATE-LAST-EXPUNGE (GET FILE :DATE-LAST-EXPUNGE))
                  (REFERENCE-DATE (OR DATE-LAST-EXPUNGE (GET FILE :REFERENCE-DATE))))
             (WHEN (NOT (MEMQ REFERENCE-DATE '(NIL :NIL)))  ;AVOID LOSSAGE CAUSED BY
                        ;UNMENTIONABLE THINGS HAPPENING DURING RESTORE-MAGTAPE
               (STRING-NCONC STRING (IF DATE-LAST-EXPUNGE " X=" " ("))
               (TIME-INTO-ARRAY STRING REFERENCE-DATE NIL)
               (OR DATE-LAST-EXPUNGE (STRING-NCONC STRING ")"))))
           (LET ((AUTHOR (GET FILE :AUTHOR)))
             (WHEN (AND AUTHOR (NOT (EQUAL AUTHOR (SEND PATHNAME :DIRECTORY))))
               (SETF (FILL-POINTER STRING)
                     (MAX 74. (FILL-POINTER STRING)))
               (STRING-NCONC STRING AUTHOR)))
           (LET ((READER (GET FILE :READER)))
             (WHEN (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY))))
               (SETF (FILL-POINTER STRING)
                     (MAX 84. (FILL-POINTER STRING)))
               (STRING-NCONC STRING READER)))
           STRING))
        (T (FORMAT STREAM "~C ~3A "
                   (IF (GET FILE :DELETED) #/D #/SP)
                   (OR (GET FILE :PHYSICAL-VOLUME) ""))
           (IF (OPERATION-HANDLED-P STREAM :ITEM)
               (SEND STREAM :ITEM 'FILE PATHNAME "~A" (SEND PATHNAME :STRING-FOR-DIRED))
             (SEND STREAM :STRING-OUT (SEND PATHNAME :STRING-FOR-DIRED)))
           (FORMAT STREAM "~20T")
           (LET ((LINK-TO (GET FILE :LINK-TO)))
             (IF LINK-TO
                 (FORMAT STREAM "=> ~A ~40T" LINK-TO)
               (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
                 (LET ((*STANDARD-OUTPUT* STREAM))
                   (FORMAT:TAB 23.))
                 (COND ((NULL LENGTH)
                        (LET ((*STANDARD-OUTPUT* STREAM))
                          (FORMAT:TAB 28.)))
                       ((> LENGTH 999.)
                        (FORMAT STREAM "~4D " LENGTH))
                       (T
                        (LET ((*STANDARD-OUTPUT* STREAM))
                          (FORMAT:TAB 24.))
                        (FORMAT STREAM "~3D " LENGTH))))
               (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
                 (IF (GET FILE :DIRECTORY)
                     (PRINC "  DIRECTORY" STREAM)
                   (AND LENGTH
                        (FORMAT STREAM "~6D(~D)" LENGTH (GET FILE :BYTE-SIZE)))))
               (FORMAT STREAM "~39T")
               (SEND STREAM :TYO
                     (COND ((GET FILE :OFFLINE) #/O)
                           ((GET FILE :NOT-BACKED-UP) #/!)
                           (T #/SP)))))
           (SEND STREAM :TYO (IF (GET FILE :DONT-DELETE) #/@ #/SP))
           (SEND STREAM :TYO (IF (GET FILE :DONT-SUPERSEDE) #/# #/SP))
           (SEND STREAM :TYO (IF (GET FILE :DONT-REAP) #/$ #/SP))
           (LET ((CREATION-DATE (GET FILE :CREATION-DATE)))
             (IF CREATION-DATE
                 (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
                     (TIME:DECODE-UNIVERSAL-TIME CREATION-DATE)
                   (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                           MONTH DAY (MOD YEAR 100.) HOURS MINUTES SECONDS))
                 (FORMAT STREAM "~17@T")))
           (LET* ((DATE-LAST-EXPUNGE (GET FILE :DATE-LAST-EXPUNGE))
                  (REFERENCE-DATE (OR DATE-LAST-EXPUNGE (GET FILE :REFERENCE-DATE))))
             (AND (NOT (MEMQ REFERENCE-DATE '(NIL :NIL)))  ;AVOID LOSSAGE CAUSED BY
                        ;UNMENTIONABLE THINGS HAPPENING DURING RESTORE-MAGTAPE
                  (MULTIPLE-VALUE-BIND (NIL NIL NIL DAY MONTH YEAR)
                      (TIME:DECODE-UNIVERSAL-TIME REFERENCE-DATE)
                    (PRINC (IF DATE-LAST-EXPUNGE " X=" " (")
                           STREAM)
                    (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D" MONTH DAY (MOD YEAR 100.))
                    (OR DATE-LAST-EXPUNGE (PRINC ")" STREAM)))))
           (LET ((AUTHOR (GET FILE :AUTHOR)))
             (AND AUTHOR (NOT (EQUAL AUTHOR (SEND PATHNAME :DIRECTORY)))
                  (FORMAT STREAM "~74T~A" AUTHOR)))
           (LET ((READER (GET FILE :READER)))
             (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY)))
                  (FORMAT STREAM "~84T~A" READER)))
           (SEND STREAM :TYO #/NEWLINE))))

(DEFSUBST 2-DIGIT-INTO-ARRAY (ARRAY NUMBER)
  (VECTOR-PUSH-EXTEND (+ #/0 (FLOOR NUMBER 10.)) ARRAY)
  (VECTOR-PUSH-EXTEND (+ #/0 (\ NUMBER 10.)) ARRAY))

(DEFUN TIME-INTO-ARRAY (ARRAY TIME &OPTIONAL (INCLUDE-TIME T))
  (IF TIME
      (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
          (TIME:DECODE-UNIVERSAL-TIME TIME)
        (2-DIGIT-INTO-ARRAY ARRAY MONTH)
        (VECTOR-PUSH-EXTEND #// ARRAY)
        (2-DIGIT-INTO-ARRAY ARRAY DAY)
        (VECTOR-PUSH-EXTEND #// ARRAY)
        (2-DIGIT-INTO-ARRAY ARRAY (MOD YEAR 100.))
        (WHEN INCLUDE-TIME
          (VECTOR-PUSH-EXTEND #/SP ARRAY)
          (2-DIGIT-INTO-ARRAY ARRAY HOURS)
          (VECTOR-PUSH-EXTEND #/: ARRAY)
          (2-DIGIT-INTO-ARRAY ARRAY MINUTES)
          (VECTOR-PUSH-EXTEND #/: ARRAY)
          (2-DIGIT-INTO-ARRAY ARRAY SECONDS)))
    (STRING-NCONC ARRAY
                  (IF INCLUDE-TIME
                      "                 "
                    "        "))))

(DEFUN READ-DIRECTORY-NAME (PROMPT PATHNAME &OPTIONAL (WILDP :WILD) &AUX TYPEIN)
  "Read a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
PATHNAME gives the defaults for host, device, directory.
WILDP gives the default used for the other components;
 normally :WILD, but could be NIL."
  (SETQ PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME WILDP
                                              :TYPE WILDP
                                              :VERSION WILDP)
        PROMPT (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME))
  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
        (*READING-PATHNAME-SPECIAL-TYPE* :WILD)
        (*READING-PATHNAME-SPECIAL-VERSION* :WILD)
        (*READING-PATHNAME-DIRECTION* :READ)
        (*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
    (WITH-STACK-LIST (PROMPT PROMPT '(:RIGHT-FLUSH " (Completion)"))
      (SETQ TYPEIN (STRING-INTERVAL
                     (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL
                                                       PROMPT))))))
  (COND ((EQUAL TYPEIN "")
         (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
         PATHNAME)
        (T
         (LET ((PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TYPEIN PATHNAME :WILD :WILD)))
           (PUSH-ON-HISTORY PATHNAME *PATHNAME-ARGUMENT-HISTORY*)
           PATHNAME))))

(DEFUN READ-UNDEFAULTED-DIRECTORY-STRING (PROMPT PATHNAME &OPTIONAL (WILDP :WILD))
  "Read a string specifying a pathname to pass to FS:DIRECTORY-LIST.
Prompt with PROMPT, a string probably ending in a colon.
PATHNAME gives the defaults for host, device, directory.
WILDP gives the default used for the other components;
 normally :WILD, but could be NIL.
These defaults are used only for completion."
  (SETQ PATHNAME (SEND PATHNAME :NEW-PATHNAME :NAME WILDP
                                              :TYPE WILDP
                                              :VERSION WILDP)
        PROMPT (FORMAT NIL "~A (Default is ~A)" PROMPT PATHNAME))
  (LET ((*READING-PATHNAME-DEFAULTS* PATHNAME)
        (*READING-PATHNAME-SPECIAL-TYPE* :WILD)
        (*READING-PATHNAME-SPECIAL-VERSION* :WILD)
        (*READING-PATHNAME-DIRECTION* :READ)
        (*MINI-BUFFER-VALUE-HISTORY* *PATHNAME-ARGUMENT-HISTORY*))
    ;; no longer pushes result on *pathname-argument-history*
    ;;  -- that is now caller's responsibility
    (WITH-STACK-LIST (PROMPT PROMPT '(:RIGHT-FLUSH " (Completion)"))
      (STRING-INTERVAL (NTH-VALUE 2 (EDIT-IN-MINI-BUFFER *PATHNAME-READING-COMTAB* NIL NIL
                                                         PROMPT))))))

(DEFCOM COM-LIST-FILES "Brief directory listing.
Lists several files per line" ()
  (LET ((PATHNAME (READ-DIRECTORY-NAME "List Files:" (DEFAULT-PATHNAME))))
    (FORMAT T "~&~A~%" PATHNAME)
    (LET ((LIST (FS:DIRECTORY-LIST PATHNAME :FAST)))
      (SETQ LIST (DELQ (ASSQ NIL LIST) LIST))   ;Don't care about system info
      (DO L LIST (CDR L) (NULL L)
          (SETF (CAR L) (CONS (SEND (CAAR L) :STRING-FOR-DIRED) (CAAR L))))
      (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FILE LIST)))
  DIS-NONE)

(DEFUN VIEW-DIRECTORY (VIEWED-DIRECTORY)
  "/"View/" the text of the directory listing of VIEWED-DIRECTORY.
That is, let user scroll through it using Space and Overprint."
  (SETQ VIEWED-DIRECTORY (FS:MERGE-PATHNAME-DEFAULTS VIEWED-DIRECTORY *PATHNAME-DEFAULTS*))
  (FILE-RETRY-NEW-PATHNAME (VIEWED-DIRECTORY FS:FILE-ERROR)
    (WITH-OPEN-STREAM (STREAM (DIRECTORY-INPUT-STREAM VIEWED-DIRECTORY))
      (SETQ VIEWED-DIRECTORY (SEND STREAM :DIRECTORY-PATHNAME))
      (PROMPT-LINE "Viewing directory ~A" VIEWED-DIRECTORY)
      (VIEW-STREAM STREAM)))
  DIS-NONE)

(DEFUN DIRECTORY-INPUT-STREAM (DIRECTORY &REST OPTIONS &AUX (DIR DIRECTORY))
  "Return a stream that reads a directory listing of pathname DIRECTORY.
The stream supports only the :LINE-IN operation."
  (LET-CLOSED ((DIRECTORY DIR)
               (DIRECTORY-LIST-STREAM NIL)
               (REREAD-ENTRY))
    (COND ((ERRORP (SETQ DIRECTORY-LIST-STREAM
                         (APPLY 'FS:DIRECTORY-LIST-STREAM DIRECTORY OPTIONS)))
           (BARF "Error: ~A"  DIRECTORY-LIST-STREAM)))
    (SETQ REREAD-ENTRY (SEND DIRECTORY-LIST-STREAM :ENTRY))
    (AND REREAD-ENTRY (NULL (CAR REREAD-ENTRY))
         (SETQ DIRECTORY (GET REREAD-ENTRY :PATHNAME)))
    #'DIRECTORY-INPUT-STREAM-IO))

;REREAD-ENTRY, if non-NIL, is an entry from the directory-list-stream
;that was peeked-ahead at.  The analogue of an UNTYI'd character.
(LOCAL-DECLARE ((SPECIAL DIRECTORY-LIST-STREAM DIRECTORY REREAD-ENTRY))
(DEFSELECT DIRECTORY-INPUT-STREAM-IO
  (:DIRECTORY-PATHNAME () DIRECTORY)
  (:CHARACTERS () T)
  (:LINE-IN (&OPTIONAL (LEADER 1) &AUX STRING TEM)
    (COND (DIRECTORY-LIST-STREAM
           (COND ((OR DIRECTORY
                      (SETQ TEM (OR (PROG1 REREAD-ENTRY (SETQ REREAD-ENTRY NIL))
                                    (SEND DIRECTORY-LIST-STREAM :ENTRY))))
                  (SETQ STRING (MAKE-STRING 80.
                                            :LEADER-LENGTH (IF (NUMBERP LEADER) LEADER 1)
                                            :FILL-POINTER 0))
                  (WITH-OUTPUT-TO-STRING (S STRING)
                    (IF TEM (FUNCALL *DIRECTORY-SINGLE-FILE-LISTER* TEM S)
                      (SEND S :STRING-OUT
                              (SEND DIRECTORY :STRING-FOR-PRINTING))
                      (SETQ DIRECTORY NIL)))
                  ;; Chop off any CRs
                  (WHEN (EQL (CHAR STRING (SETQ TEM (1- (FILL-POINTER STRING)))) #/NEWLINE)
                    (SETF (FILL-POINTER STRING) TEM))
                  STRING)
                 (T (SEND DIRECTORY-LIST-STREAM :CLOSE)
                    (SETQ DIRECTORY-LIST-STREAM NIL)
                    (VALUES NIL T))))
          (T (VALUES NIL T))))
  (:CLOSE (&OPTIONAL MODE)
    (COND ((AND DIRECTORY-LIST-STREAM
                (NOT (TYPEP DIRECTORY-LIST-STREAM 'PATHNAME)))
           (SEND DIRECTORY-LIST-STREAM :CLOSE MODE)
           (SETQ DIRECTORY-LIST-STREAM NIL)
           T)))))

;;; Obsolete ITS only functions
;(DEFCOM COM-OLD-LIST-FILES "Brief directory listing.
;Lists directory N entries to a line, with the following
;special characters to the left of the filenames:
;       : this is a link
;       ! this file has not been backed up to tape yet
;       * this file has really been deleted but not yet
;         closed, or is otherwise locked.
;       (blank) this is a plain normal file
;Also the top line contains in order, the device being
;listed from, the directory, Free: followed by the number of
;free blocks on the device (separated into primary, secondary, etc.
;packs), Used: followed by the number of blocks this directory is taking up." ()
;  (LET ((PATHNAME (READ-DIRECTORY-NAME "List Directory:" (DEFAULT-PATHNAME)))
;       (LINE NIL) (X NIL) (Y NIL) (X1 NIL) (Y1 NIL) (TEM1 NIL)
;       (FREE-ARRAY (MAKE-ARRAY 10)) (USED-ARRAY (MAKE-ARRAY 10)))
;    (WITH-OPEN-FILE (STREAM (SEND PATHNAME :DEFAULT-NAMESTRING ".FILE. (DIR)") '(READ))
;      (SETQ LINE (SEND STREAM ':LINE-IN))
;      (SETQ LINE (SEND STREAM ':LINE-IN))
;      (DIRECTORY-FREE-SPACE LINE FREE-ARRAY)
;      (FORMAT T "~6A ~6A  " (SEND PATHNAME ':DEVICE) (SEND PATHNAME ':DIRECTORY))
;      (FORMAT-DISK-BLOCKS-ARRAY *STANDARD-OUTPUT* "Free: " FREE-ARRAY)
;      (FORMAT T ", Used: ")                    ;Filled in later
;      (MULTIPLE-VALUE (X Y) (SEND *STANDARD-OUTPUT* ':READ-CURSORPOS ':PIXEL))
;      ;; Make any pack that exists show up in the "used" display even if used=0
;      (DOTIMES (IDX 10)
;       (AND (AREF FREE-ARRAY IDX)
;            (ASET 0 USED-ARRAY IDX)))
;      (DO ((I 0 (\ (1+ I) 5)))
;         (NIL)
;       (AND (ZEROP I) (TERPRI))
;       (SETQ LINE (SEND STREAM ':LINE-IN))
;       (COND ((OR (NULL LINE)
;                  (ZEROP (ARRAY-ACTIVE-LENGTH LINE))
;                  (= (AREF LINE 0) #/FF))
;              (RETURN NIL)))
;       (SEND *STANDARD-OUTPUT* ':TYO
;             (COND ((= #/* (AREF LINE 0))
;                    #/*)
;                   ((= #/L (AREF LINE 2))
;                    #/:)
;                   (T (LET ((USED)
;                            (PACK (PARSE-NUMBER LINE 2)))
;                        (MULTIPLE-VALUE (USED TEM1) (PARSE-NUMBER LINE 20.))
;                        (LET ((IDX (IF (OR (< PACK 10.) (> PACK 16.)) 0
;                                     (- PACK 9.))))
;                          (ASET (+ (OR (AREF USED-ARRAY IDX) 0) USED)
;                                USED-ARRAY IDX)))
;                      (COND ((= #/! (AREF LINE (1+ TEM1)))
;                             #/!)
;                            (T #/SP)))))
;       (SEND *STANDARD-OUTPUT* ':STRING-OUT (NSUBSTRING LINE 6 19.))
;       (SEND *STANDARD-OUTPUT* ':STRING-OUT "  "))
;      (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
;      (MULTIPLE-VALUE (X1 Y1) (SEND *STANDARD-OUTPUT* ':READ-CURSORPOS ':PIXEL))
;      (SEND *STANDARD-OUTPUT* ':SET-CURSORPOS X Y ':PIXEL)
;      (FORMAT-DISK-BLOCKS-ARRAY *STANDARD-OUTPUT* "" USED-ARRAY)
;      (SEND *STANDARD-OUTPUT* ':SET-CURSORPOS X1 Y1 ':PIXEL)))
;  DIS-NONE)

;(DEFUN SUBSET-DIRECTORY-LISTING (PATHNAME)
;  (LET ((FN1 (SEND PATHNAME ':NAME))
;       (FN2 (SEND PATHNAME ':FN2)))
;    (FORMAT T "~&~A~%" PATHNAME)
;    (LET ((LINE NIL)
;         (FREE-ARRAY (MAKE-ARRAY 10))
;         (USED-ARRAY (MAKE-ARRAY 10)))
;      (WITH-OPEN-FILE (STREAM (SEND PATHNAME ':NEW-STRUCTURED-NAME '(".FILE." "(DIR)"))
;                             '(READ))
;       ;; First find out how much space is free.
;       (SETQ LINE (SEND STREAM ':LINE-IN))
;       (SETQ LINE (SEND STREAM ':LINE-IN))
;       (DIRECTORY-FREE-SPACE LINE FREE-ARRAY)
;       ;; Make any pack that exists show up in the "used" display even if used=0
;       (DOTIMES (IDX 10)
;         (AND (AREF FREE-ARRAY IDX)
;              (ASET 0 USED-ARRAY IDX)))
;       ;; Next, go through lines of dir, counting USED and printing some lines.
;       (DO ((KEY (STRING-APPEND " "
;                                (IF (STRING-EQUAL FN1 "TS") FN2 FN1)
;                                " "))
;            (LINE) (EOF))
;           (NIL)
;         (MULTIPLE-VALUE (LINE EOF)
;           (SEND STREAM ':LINE-IN))
;         (AND (OR EOF (ZEROP (STRING-LENGTH LINE))) (RETURN NIL))
;         (AND (STRING-SEARCH KEY LINE)
;              (SEND *STANDARD-OUTPUT* ':LINE-OUT LINE))
;         (OR (= (AREF LINE 2) #/L)
;             (LET ((USED (PARSE-NUMBER LINE 20.))
;                   (PACK (PARSE-NUMBER LINE 2)))
;               (LET ((IDX (IF (OR (< PACK 10.) (> PACK 16.)) 0
;                              (- PACK 9.))))
;                 (ASET (+ (OR (AREF USED-ARRAY IDX) 0) USED) USED-ARRAY IDX)))))
;       (FORMAT-DISK-BLOCKS-ARRAY T "Free: " FREE-ARRAY)
;       (FORMAT-DISK-BLOCKS-ARRAY T ", Used: " USED-ARRAY)))))

;Element 0 of FREE-ARRAY is for packs other than 10.-16.
;(DEFUN DIRECTORY-FREE-SPACE (LINE FREE-ARRAY)
;  (DO ((I (STRING-SEARCH-CHAR #/# LINE)
;         (STRING-SEARCH-CHAR #/# LINE I))
;       (NUM) (IDX) (BLKS))
;      ((NULL I))
;    (MULTIPLE-VALUE (NUM I)
;      (PARSE-NUMBER LINE (1+ I)))
;    (MULTIPLE-VALUE (BLKS I)
;      (PARSE-NUMBER LINE (1+ I)))
;    (SETQ IDX (IF (OR (< NUM 10.) (> NUM 16.)) 0
;                 (- NUM 9.)))
;    (ASET (+ (OR (AREF FREE-ARRAY IDX) 0) BLKS) FREE-ARRAY IDX)))

;(DEFUN FORMAT-DISK-BLOCKS-ARRAY (STREAM TITLE ARRAY)
;  (FORMAT STREAM TITLE)
;  (DO ((IDX 0 (1+ IDX))
;       (LIM (ARRAY-LENGTH ARRAY))
;       (FIRSTP T)
;       (BLKS))
;      ((= IDX LIM))
;    (COND ((SETQ BLKS (AREF ARRAY IDX))
;          (FORMAT STREAM "~:[+~]~D" FIRSTP BLKS)
;          (SETQ FIRSTP NIL)))))

;(DEFUN ROTATED-DIRECTORY-LISTING (PATHNAME)
;  (CATCH 'ABORT
;     (LET ((DEV (SEND PATHNAME ':DEVICE))
;           (DIR (SEND PATHNAME ':DIRECTORY))
;           (FN1 (SEND PATHNAME ':NAME))
;           (FN NIL))
;       (SETQ FN (SEND PATHNAME ':NEW-STRUCTURED-NAME '(".FILE." "(DIR)")))
;       (PROMPT-LINE "Directory Listing")
;       (FORMAT T "~&~A  ~A    --   ~A~%" DEV DIR PATHNAME)
;       (LET ((LINE NIL) (X 0) (Y 0))
;        (WITH-OPEN-FILE (STREAM FN '(IN))
;          (SETQ LINE (SEND STREAM ':LINE-IN))
;          (FORMAT T "~A~%" (SEND STREAM ':LINE-IN))
;          (DO ((LINE (SETQ LINE (SEND STREAM ':LINE-IN))
;                     (SETQ LINE (SEND STREAM ':LINE-IN)))
;               (LFN1 (STRING-LENGTH FN1))
;               (LFN16 (+ (STRING-LENGTH FN1) 6))
;               )
;              ((NULL LINE)
;               (FORMAT T "There is no file named ~A in the directory.~%" FN1))
;            (COND ((STRING-EQUAL LINE FN1 6 0 LFN16 LFN1)
;                   ;; Found one.
;                   (LET ((FIRST LINE))
;                     (SETQ LINE (DO ((LINE LINE (SEND STREAM ':LINE-IN)))
;                                    ((OR (= (AREF LINE 0) #/FF)
;                                         (NOT (STRING-EQUAL LINE FN1 6 0 LFN16 LFN1)))
;                                     LINE)
;                                  (FORMAT T "~A~%" LINE)))
;                     (FORMAT T "==MORE==")
;                     (OR (= (SEND *STANDARD-INPUT* ':TYI) #/SP)
;                         (*THROW 'ABORT NIL))
;                     (MULTIPLE-VALUE (X Y)
;                       (SEND *STANDARD-OUTPUT* ':READ-CURSORPOS))
;                     (SEND *STANDARD-OUTPUT* ':SET-CURSORPOS 0 Y)
;                     (SEND *STANDARD-OUTPUT* :CLEAR-REST-OF-LINE)
;                     (DO ((LINE LINE (SEND STREAM ':LINE-IN)))
;                         ((EQUAL LINE FIRST))
;                       (COND ((ZEROP (STRING-LENGTH LINE))
;                              (FORMAT T "------------------------------------------------~%")
;                              (CLOSE STREAM)
;                              (SETQ STREAM (OPEN FN '(IN)))
;                              (SEND STREAM ':LINE-IN)
;                              (SEND STREAM ':LINE-IN)
;                              (SETQ LINE (SEND STREAM ':LINE-IN))))
;                       (FORMAT T "~A~%" LINE)))
;                   (RETURN NIL)))))))))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* DIRECTORY "Edit" DIRECTORY-EDIT-1
                          T "Run DIRED on this directory.")

(DEFUN DIRECTORY-EDIT-1 (DIRECTORY)
  (DIRECTORY-EDIT DIRECTORY)
  NIL)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* DIRECTORY "View" VIEW-DIRECTORY
                          NIL "View this directory")

(DEFCOM COM-LIST-ALL-DIRECTORY-NAMES "List names of all disk directories." ()
  (LET* ((DEFAULT (SEND (DEFAULT-PATHNAME) :NEW-PATHNAME
                                           :DIRECTORY :WILD
                                           :NAME :UNSPECIFIC
                                           :TYPE :UNSPECIFIC
                                           :VERSION :UNSPECIFIC))
         (PATHNAME (READ-DEFAULTED-PATHNAME "List directories:" DEFAULT
                                            :UNSPECIFIC :UNSPECIFIC))
         (DIRS (FS:ALL-DIRECTORIES PATHNAME :NOERROR)))
    (IF (ERRORP DIRS)
        (BARF "Error: ~A" DIRS)
      (SETQ DIRS (SORTCAR DIRS #'FS:PATHNAME-LESSP))
      (SEND *STANDARD-OUTPUT* :ITEM-LIST 'DIRECTORY
                              (LOOP FOR (PATHNAME) IN DIRS
                                    COLLECT `(,(SEND PATHNAME :STRING-FOR-DIRECTORY)
                                              . ,(SEND PATHNAME :NEW-PATHNAME
                                                                :NAME :WILD
                                                                :TYPE :WILD
                                                                :VERSION :WILD))))))
  DIS-NONE)

(DEFCOM COM-EXPUNGE-DIRECTORY "Expunge deleted files from a directory" ()
  (LET* ((DIRECTORY (READ-DIRECTORY-NAME "Expunge directory" (DEFAULT-PATHNAME)))
         (RESULT (FS:EXPUNGE-DIRECTORY DIRECTORY :ERROR NIL)))
    (IF (ERRORP RESULT) (BARF "Cannot expunge ~A: ~A" DIRECTORY RESULT)
        (FORMAT *QUERY-IO* "~&~A: ~D block~:P freed" DIRECTORY RESULT)))
  DIS-NONE)

(DEFCOM COM-CREATE-DIRECTORY "Create a directory" ()
  (LET* ((DIRECTORY (READ-DIRECTORY-NAME "Create directory" (DEFAULT-PATHNAME)))
         (RESULT (FS:CREATE-DIRECTORY DIRECTORY :ERROR NIL))
         (DIRNAME (SEND DIRECTORY :STRING-FOR-DIRECTORY)))
    (IF (ERRORP RESULT) (BARF "Cannot create ~A: ~A" DIRNAME RESULT)
      (FORMAT *QUERY-IO* "~&Directory ~A created" DIRNAME)))
  DIS-NONE)

(DEFCOM COM-CREATE-LINK "Creates a link between two files." ()
  (LET* ((LINK (READ-DEFAULTED-PATHNAME
                 "Pathname of link to be created:" *PATHNAME-DEFAULTS*))
         (TO (READ-DEFAULTED-PATHNAME
               (FORMAT NIL "Pathname of target of link ~A:" LINK)
               LINK NIL NIL :WRITE))
         (RESULT (FS:CREATE-LINK LINK TO :ERROR NIL)))
    (IF (ERRORP RESULT)
        (BARF "Cannot create link ~A: ~A" LINK RESULT)
      (FORMAT *QUERY-IO* "~&Link ~A => ~A created." LINK TO)))
  DIS-NONE)

(DEFCOM COM-REMOTE-CONNECT "Connect to a directory, for access.
This does not affect the meaning of filenames; however, it gives
you access to the directory for all file operations if it succeeds.
You are asked for a password if one is needed and not already remembered." ()
  (LET* ((DIRECTORY (READ-DIRECTORY-NAME "Remote connect to directory" (DEFAULT-PATHNAME)))
         (RESULT (FS:REMOTE-CONNECT DIRECTORY :ERROR NIL))
         (DIRNAME (SEND DIRECTORY :STRING-FOR-DIRECTORY)))
    (IF (ERRORP RESULT) (BARF "Cannot connect to ~A: ~A" DIRNAME RESULT)
      (FORMAT *QUERY-IO* "~&Server on ~A connected to directory ~A."
                         (SEND DIRECTORY :HOST) DIRNAME)))
  DIS-NONE)

(DEFCOM COM-REMOTE-ACCESS "Get the access of a directory.
This does not affect the meaning of filenames; however, it gives
you the same access which the owner of that directory would have,
to that directory and others.
You are asked for a password if one is needed and not already remembered." ()
  (LET* ((DIRECTORY (READ-DIRECTORY-NAME "Remote connect to directory" (DEFAULT-PATHNAME)))
         (RESULT (FS:REMOTE-CONNECT DIRECTORY :ERROR NIL :ACCESS T))
         (DIRNAME (SEND DIRECTORY :STRING-FOR-DIRECTORY)))
    (IF (ERRORP RESULT) (BARF "Cannot access to ~A: ~A" DIRNAME RESULT)
      (FORMAT *QUERY-IO* "~&Server on ~A now accessing to directory ~A."
                         (SEND DIRECTORY :HOST) DIRNAME)))
  DIS-NONE)

(DEFCOM COM-SET-WORKING-DIRECTORY "Specify the working device//directory for a host.
If you specify device DSK in a pathname on this host, it is replaced
by whatever the working device is, at defaulting time.  In addition,
if no directory is specified, the working directory is also used by default." ()
  (LET* ((DIRECTORY (READ-DIRECTORY-NAME "Host and device//directory" (DEFAULT-PATHNAME))))
    (FS:SET-HOST-WORKING-DIRECTORY (SEND DIRECTORY :HOST) DIRECTORY))
  DIS-NONE)

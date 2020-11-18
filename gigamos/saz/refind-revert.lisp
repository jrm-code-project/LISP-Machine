;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by SAZ
;;; Written 14-Oct-88 18:26:00 by SAZ at site Gigamos Cambridge
;;; while running on Wolfgang Amadeus Mozart from band 2
;;; with Experimental System 126.103, Experimental ZWEI 126.17, Experimental ZMail 74.9, Experimental Local-File 76.0, Experimental File-Server 25.0, Experimental Lambda-Diag 18.0, Experimental Unix-Interface 15.0, Experimental Tape 26.4, microcode 1762, SDU Boot Tape 3.14, SDU ROM 8, 126.100 104.

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:22:41
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defmacro WITH-VERSION-AND-MODIFICATION-INFO (buffer body)
  ;; binds some variables to interesting properties of a buffer and its associated file.
  ;; Used in REFIND-FILE, below.
  (declare (zwei:indentation 1 1))
  `(block no-file
     (let* ((buffer-pathname (zwei:buffer-pathname ,buffer))
          (FILE-TRUENAME (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
     (when (NOT FILE-TRUENAME)
       (return-from no-file
         (format t "~%Cannot find any versions of ~S on disk."
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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:22:53
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:22:54
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defun REFIND-FILE (&optional (buffer (read-buffer-name "File to refind: "
                                                        *interval*)))
  ;;  This will give users choices unless they specify (by using com-refind-file with
  ;; an explicit numeric arg) that they want REFIND-FILE to act independently of user
  ;; input.
  (if (not (buffer-pathname buffer))
      (format t "The buffer ~S is not associated with an existing file." buffer)
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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:22:56
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defcom com-refind-all-files "Revert or update all buffers to contain their most recent versions." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (refind-file each-buffer))
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:22:57
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defcom com-refind-file "Revert or update the current buffer to contain the most recent version." ()
  (refind-file *interval*)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:08
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defcom com-revert-all-buffers "Send a :REVERT message to all active buffers." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (send-if-handles each-buffer :REVERT))
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:09
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defun revert-buffer-if-needed (buffer)
  (LET* ((buffer-pathname (buffer-pathname buffer))
         (probe-stream (and buffer-pathname (PROBEF (SEND buffer-PATHNAME :NEW-VERSION :NEWEST)))))
    (IF (NOT PROBE-STREAM)
        (format t "The buffer ~S is not associated with an existing file."
                buffer-pathname)
      (if (buffer-modified-p buffer)
          (send-if-handles buffer :REVERT)))))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:10
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defcom com-revert-buffer-if-needed "Revert any buffer with a handler the :REVERT message, if modified." ()
  (revert-buffer-if-needed *interval*)
  dis-all)

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:12
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defcom com-revert-all-buffers-if-needed "Revert all buffers with handlers for the :REVERT message, if modified." ()
  (dolist (each-buffer *zmacs-buffer-list*)
    (revert-buffer-if-needed each-buffer)
    dis-all))

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:18
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


(defun replace-current-buffer-with-file (pathname)
  (without-interrupts
      (setq *zmacs-buffer-name-alist*
            (lisp:remove *interval* *zmacs-buffer-name-alist* :key 'cdr))
      (send *interval* :kill)
      (let* ((new-buffer
               (load-file-into-zmacs pathname)))
        (send new-buffer :select))))

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#558 on 2-Oct-86 02:29:24

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:23:58
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:24:05
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:24:19
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "


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

))

; From modified file DJ: L.ZWEI; ZMACS.LISP#590 at 14-Oct-88 18:24:24
#8R ZWEI#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: ZWEI; ZMACS  "

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
                (FORMAT *QUERY-IO* "~&Type <SPACE> to confirm, <RUBOUT> to abort, <Control-R> to edit.")
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

))

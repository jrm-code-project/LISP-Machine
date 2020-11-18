;;; -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-

(defcom COM-INDENT-SEXP-AND-MOVE "Indent the following SEXP for LISP,
and move to   the point after the closing paren (hopefully on next line
so that repeated invocations will have an effect on subsequent sexps." ()
  (com-indent-for-lisp)
  (com-indent-sexp)
  (com-forward-sexp)
  (com-forward)
  dis-text)


;;; Modified version of two-windows stuff, takes two windows
;;; (structures, not sheets) and makes them share the area
;;; originally occupied by the first of the two, except that
;;; this version makes two side-by-side vertical windows
;;; rather than one on top of the other.

(defvariable *window-to-select* 1 :FIXNUM
  "1 means top or left side (current buffer), 2 means bottom or right side (second buffer)")

(defvariable *split-configuration* :VERTICAL :KEYWORD
  ":VERTICAL means two side by side windows, :HORIZONTAL means one on top of the other.")

(DEFCOM COM-TWO-WINDOWS "Select two windows.  Split the frame into
two editor windows and select the indicated one.  With a numeric
argument, make the second window point to the same buffer that the
first window does." ()
  (SWITCH-WINDOWS *NUMERIC-ARG-P* *window-to-select*)
  DIS-NONE)

(DEFUN TWO-WINDOWS (ZWEI-WINDOW-1 ZWEI-WINDOW-2)
  (REDISPLAY ZWEI-WINDOW-1 ':NONE)
  (LET ((W1 (WINDOW-SHEET ZWEI-WINDOW-1))
        (W2 (WINDOW-SHEET ZWEI-WINDOW-2))
        (FRAME (WINDOW-FRAME ZWEI-WINDOW-1)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
      (TV:PRESERVE-SUBSTITUTE-STATUS (SEND W1 ':SUPERIOR)
        (TV:DELAYING-SCREEN-MANAGEMENT
          (SEND W1 ':DEEXPOSE)
          (SEND W2 ':DEEXPOSE)
          (let ((cut-point
                  (select *split-configuration*
                    (:VERTICAL (TRUNCATE (- right left) 2))
                    (:HORIZONTAL (TRUNCATE (- BOTTOM TOP) 2)))))
            (multiple-value-bind
              (w1-left w1-top w1-right w1-bottom
               w2-left w2-top w2-right w2-bottom)
                (select *split-configuration*
                  (:VERTICAL
                   (values left top (+ left cut-point) bottom
                           (+ left cut-point) top right bottom))
                  (:HORIZONTAL
                   (values left top right (+ top cut-point)
                           left (+ top cut-point) right bottom)))
              (SEND W1 ':SET-EDGES w1-left w1-top
                    w1-right w1-bottom)
              (SEND W2 ':SET-EDGES w2-left w2-top
                    w2-right w2-bottom)
              (SEND W1 ':SET-LABEL NIL)
              (SEND W2 ':SET-LABEL NIL)
              (SEND W1 ':EXPOSE NIL ':CLEAN)    ;Make sure they are both there
              (SEND W2 ':EXPOSE NIL ':CLEAN))))))
    (SEND FRAME ':UPDATE-LABELS)))


(DEFUN MAKE-WINDOW-FULL-SCREEN (WINDOW &AUX FRAME LEFT TOP RIGHT BOTTOM)
  (SETQ FRAME (WINDOW-FRAME WINDOW))
  (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
    (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW))
  (TV:PRESERVE-SUBSTITUTE-STATUS FRAME
    (TV:DELAYING-SCREEN-MANAGEMENT
      (SEND WINDOW ':DEEXPOSE)
      (SEND WINDOW ':SET-EDGES LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW ':SET-LABEL NIL)
      (SEND WINDOW ':EXPOSE NIL ':CLEAN)
      (SEND FRAME ':UPDATE-LABELS)
      (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
      (MAKE-WINDOW-CURRENT WINDOW))))

;;;Handy zmacs extensions and fixes to existing features

;;;Moved to LISPM.INIT
;;;(defmacro with-fdefine-warnings-inhibited ((&body body))
;;;  `(let ((inhibit-fdefine-warnings t))
;;;     ,body))

(load "dj:smh;isearch.qfasl")

(defmacro install-command-and-advertise (command char comtab announcement)
  ;; Install and notify.
  `(progn
     (zwei:command-store ,command ,char ,comtab)
     (format query-io "~%~C~10,5Tnow invokes ~A, which ~A, in ~A." ,char ,command ,announcement ,comtab)))

(DEFUN DEFAULT-LIST-ONE-FILE (FILE &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX PATHNAME)
  (COND ((NULL (SETQ PATHNAME (CAR FILE)))
         (COND ((GET FILE :DISK-SPACE-DESCRIPTION)
                (SEND STREAM :LINE-OUT (GET FILE :DISK-SPACE-DESCRIPTION)))
               ((GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS)
                (DO ((FREE (GET FILE :PHYSICAL-VOLUME-FREE-BLOCKS) (CDR FREE))
                     (FLAG T NIL))
                    ((NULL FREE) (SEND STREAM :TYO #\NEWLINE))
                 (FORMAT STREAM "~A #~A=~D" (IF FLAG "Free:" ",") (CAAR FREE) (CDAR FREE))))
               (T
                (SEND STREAM :TYO #\NEWLINE))))
        ((TYPEP STREAM 'INTERVAL-STREAM)
         (LET ((STRING (CREATE-LINE 'ART-STRING 128. NIL)))
           (DEFAULT-LIST-ONE-FILE FILE STRING)
           (SEND STREAM :LINE-OUT STRING)))
        ((OR (NULL STREAM) (STRINGP STREAM))
         (LET ((STRING
                 (OR STREAM (MAKE-ARRAY 128. :TYPE 'ART-STRING :LEADER-LENGTH 1))))
           (SETF (FILL-POINTER STRING) 0)
           (ARRAY-INITIALIZE STRING #\SP 0 (ARRAY-LENGTH STRING))
           (VECTOR-PUSH (IF (GET FILE :DELETED) #\D #\SP) STRING)
           (VECTOR-PUSH #\SP STRING)
           (STRING-NCONC STRING (OR (GET FILE :PHYSICAL-VOLUME) ""))
           (SETF (FILL-POINTER STRING) (1+ (MAX 5 (FILL-POINTER STRING))))
           (STRING-NCONC STRING (SEND PATHNAME :STRING-FOR-DIRED))
           (VECTOR-PUSH #\SP STRING)
           (LET ((LINK-TO (GET FILE :LINK-TO)))
             (IF LINK-TO
                 (PROGN (STRING-NCONC STRING "=> " LINK-TO " ")
                        (SETF (FILL-POINTER STRING)
                              (MAX 56. (FILL-POINTER STRING))))
               (progn
               (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
                 (SETF (FILL-POINTER STRING)
                       (MAX 39. (FILL-POINTER STRING)))
                 (COND ((NULL LENGTH)
                        (STRING-NCONC STRING "     "))
                       ((> LENGTH 999.)
                        (SETF (FILL-POINTER STRING)
                              (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                                 (FILL-POINTER STRING) 4))
                        (VECTOR-PUSH #\SP STRING))
                       (T
                        (SETF (FILL-POINTER STRING)
                              (MAX 40. (FILL-POINTER STRING)))
                        (SETF (FILL-POINTER STRING)
                              (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                                 (FILL-POINTER STRING) 3))
                        (VECTOR-PUSH #\SP STRING))))
               (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
                 (IF (GET FILE :DIRECTORY)
                     (STRING-NCONC STRING "DIRECTORY")
                   (WHEN LENGTH
                     (SETF (FILL-POINTER STRING)
                           (NUMBER-INTO-ARRAY STRING LENGTH 10.
                                              (FILL-POINTER STRING) 6))
                     (VECTOR-PUSH #\( STRING)
                     (SETF (FILL-POINTER STRING)
                           (NUMBER-INTO-ARRAY STRING (GET FILE :BYTE-SIZE) 10.
                                              (FILL-POINTER STRING)))
                     (VECTOR-PUSH #\) STRING))))
               (SETF (FILL-POINTER STRING)
                     (MAX 55. (FILL-POINTER STRING)))
               (VECTOR-PUSH (COND ((GET FILE :OFFLINE) #\O)
                                  ((GET FILE :NOT-BACKED-UP) #\!)
                                  (T #\SP))
                            STRING))))
           (VECTOR-PUSH (IF (GET FILE :DONT-DELETE) #\@ #\SP) STRING)
           (VECTOR-PUSH (IF (GET FILE :DONT-SUPERSEDE) #\# #\SP) STRING)
           (VECTOR-PUSH (IF (GET FILE :DONT-REAP) #\$ #\SP) STRING)
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
                     (MAX 88. (FILL-POINTER STRING)))
               (STRING-NCONC STRING AUTHOR)))
           (LET ((READER (GET FILE :READER)))
             (WHEN (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY))))
               (SETF (FILL-POINTER STRING)
                     (MAX 98. (FILL-POINTER STRING)))
               (STRING-NCONC STRING READER)))
           STRING))
        (T (FORMAT STREAM "~C ~3A "
                   (IF (GET FILE :DELETED) #\D #\SP)
                   (OR (GET FILE :PHYSICAL-VOLUME) ""))
           (IF (OPERATION-HANDLED-P STREAM :ITEM)
               (SEND STREAM :ITEM 'FILE PATHNAME "~A" (SEND PATHNAME :STRING-FOR-DIRED))
             (SEND STREAM :STRING-OUT (SEND PATHNAME :STRING-FOR-DIRED)))
           (FORMAT STREAM "~20T")
           (LET ((LINK-TO (GET FILE :LINK-TO)))
             (IF LINK-TO
                 (FORMAT STREAM "=> ~A ~40T" LINK-TO)
               (progn
               (LET ((LENGTH (GET FILE :LENGTH-IN-BLOCKS)))
                 (LET ((*STANDARD-OUTPUT* STREAM))
                   (FORMAT:TAB 39.))
                 (COND ((NULL LENGTH)
                        (LET ((*STANDARD-OUTPUT* STREAM))
                          (FORMAT:TAB 44.)))
                       ((> LENGTH 999.)
                        (FORMAT STREAM "~4D " LENGTH))
                       (T
                        (LET ((*STANDARD-OUTPUT* STREAM))
                          (FORMAT:TAB 40.))
                        (FORMAT STREAM "~3D " LENGTH))))
               (LET ((LENGTH (GET FILE :LENGTH-IN-BYTES)))
                 (IF (GET FILE :DIRECTORY)
                     (PRINC "  DIRECTORY" STREAM)
                   (AND LENGTH
                        (FORMAT STREAM "~6D(~D)" LENGTH (GET FILE :BYTE-SIZE)))))
               (FORMAT STREAM "~55T")
               (SEND STREAM :TYO
                     (COND ((GET FILE :OFFLINE) #\O)
                           ((GET FILE :NOT-BACKED-UP) #\!)
                           (T #\SP)))))
           (SEND STREAM :TYO (IF (GET FILE :DONT-DELETE) #\@ #\SP))
           (SEND STREAM :TYO (IF (GET FILE :DONT-SUPERSEDE) #\# #\SP))
           (SEND STREAM :TYO (IF (GET FILE :DONT-REAP) #\$ #\SP))
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
                  (FORMAT STREAM "~88T~A" AUTHOR)))
           (LET ((READER (GET FILE :READER)))
             (AND READER (NOT (EQUAL READER (SEND PATHNAME :DIRECTORY)))
                  (FORMAT STREAM "~98T~A" READER)))
           (SEND STREAM :TYO #\NEWLINE)))))

(DEFCOM COM-STRING-SEARCH-SELECT-BUFFERS-AS-TAG-TABLE
        "Select the buffers whose names contain a specified string as a tag table.
This causes commands such as Tags Search, Tags Query Replace, and
Tags Compile Changed Sections to look through all buffers you specify." ()
  (let (the-string)
    (SELECT-FILE-LIST-AS-TAG-TABLE
      (mapcar #'buffer-pathname
              (specify-list-of-buffers
                :only-non-special-buffers t
                :only-file-buffers t
                :substring (setq the-string
                                 (typein-line-readline "Include buffers whose names contain the string: "))
                :query-string nil))
      (TYPEIN-LINE-READLINE-WITH-DEFAULT (string-append "Buffers with " the-string
                                                        " in their names")      ;disambiguating string in
                                                ;front to aid in specifying
                                                ;the tag table later on...
                                         "Name for this tag table:")))
  DIS-NONE)



;;;;Problem: Fails to clear kill history before pushing marked thing -- previously
;;;;killed text appears at point!
;(DEFCOM COM-MOUSE-MOVE-THING-TO-CURSOR
;  "Takes sexp pointed at by the mouse and zaps a copy of it to current cursor location." ()
;  (COM-SET-POP-MARK)
;  (let ((*mouse-x* system:mouse-x)
;       (*mouse-y* system:mouse-y))
;    (COM-MOUSE-MARK-THING))
;  (com-save-region)
;  (COM-MOVE-TO-PREVIOUS-POINT)
;  (com-yank)  ;;yanks too much!
;  (SETQ *MARK-STAYS* nil)
;  DIS-TEXT)

;(install-command-and-advertise 'COM-MOUSE-MOVE-THING-TO-CURSOR #/ *standard-comtab*
;"")



(DEFUN PRINT-SHORT-DOC-FOR-TABLE (CHAR COMTAB INDENTATION)
  "Document what CHAR does in COMTAB, for subcommands of prefix characters.
It prints one or two lines of stuff, with the given INDENTATION."
  (LET ((X (COMMAND-LOOKUP CHAR COMTAB T)))
    (COND ((MEMQ X '(NIL :UNDEFINED)))          ;undefined
          ((CONSP X))                           ;alias
          ((MACRO-COMMAND-P X)
           (FORMAT T "~&~V@T~:C is a user defined macro.~%" INDENTATION CHAR))
          ((PREFIX-COMMAND-P X)
           (FORMAT T "~&~V@T~:C reads another character and dispatches.~%"
                   INDENTATION CHAR))
          ((NOT (SYMBOLP X)))                   ;??
          (T
           (FORMAT T "~&~V@T~:C is ~A:~57T"     ;more readably than with ~A:~%~@T"
                   INDENTATION CHAR (COMMAND-NAME X) (+ 5 INDENTATION))
           (PRINT-DOC :SHORT X CHAR)))))




(DEFCOM COM-FIND-DIRECTORY-FILES "Reads in all files in specified directory.
You type the name of a directory a la COM-DIRED, and all the files
in that directory are brought into the editor if not already in." ()
  (DOLIST (FILE (FS:DIRECTORY (READ-DIRECTORY-NAME "Read in files for directory:" (DEFAULT-PATHNAME))))
    (UNLESS (member (SEND FILE :TYPE) '("DIRECTORY" "DVI" "QFASL"))
      (LET ((SOURCE-PATHNAME (SEND FILE :SOURCE-PATHNAME)))
        (UNLESS (FIND-FILE-BUFFER SOURCE-PATHNAME)
          (LET ((BUFFER (MAKE-INSTANCE 'ZMACS-BUFFER)))
            (REVERT-BUFFER BUFFER SOURCE-PATHNAME 'NOSECTIONIZE NIL)
            (MAKE-BUFFER-CURRENT BUFFER)
            (SECTIONIZE-BUFFER BUFFER)
            (NOT-MODIFIED BUFFER)
            (SETF (BUFFER-FILE-READ-TICK BUFFER) *TICK*))))))
  DIS-TEXT)


(defcom com-bury-buffer "" ()
  (if (not (typep tv:selected-window 'zwei:zmacs-window-pane))
      (barf "No buffers")
    (WITHOUT-INTERRUPTS
      (DOLIST (W *ALL-ZMACS-WINDOWS*)
        (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
          (APPEND-REMOVE-ON-HISTORY *interval* HISTORY)))
      (SETQ *ZMACS-BUFFER-LIST* (APPEND (REMQ *interval* *ZMACS-BUFFER-LIST*)
                                        (LIST *interval*)))
      (MAKE-BUFFER-CURRENT (first *zmacs-buffer-list*))))
  dis-text)


(defcom com-rotate-buffer-stack "Control-H" ()
  (if (not (typep tv:selected-window 'zwei:zmacs-window-pane))
      (barf "No buffers")
    (send (nth (1- (length *zmacs-buffer-list*)) *zmacs-buffer-list*) :select))
  dis-text)

(DEFCOM COM-FILL-TEXT-BUFFER "Fill (or adjust) the entire buffer." ()
  (FILL-INTERVAL (interval-first-bp *interval*)
                 (interval-last-bp *interval*)
                 T (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*)))
  DIS-TEXT)


(DEFCOM COM-FILL-LISP-BUFFER "Fill (or adjust) an entire lisp buffer." ()
  (INDENT-INTERVAL-FOR-LISP *interval*)
  DIS-TEXT)


;;;Feature: Reads in all files currently without zmacs buffers in the
;;;background after accepting tag table name as unique.  Alerts when
;;;done.
;;;
;;;Feature: Allows specification of identical sets of files for multiple
;;;tag tables (as long as they are given different names).
;;;
;;;Caveat (not specific to this code): Tag table names are not case sensitive!
;;;
;;;Warning: Full defaulting is in effect here, so all versions of a file
;;;are used if you do not specify otherwise!
;;;

(defvariable *zmacs-prefer-preload-tag-table-files-in-background* T :boolean)

(defcom com-select-files-as-tag-table "Prompts for and selects files to use as a tag table.
  With a numeric argument, reads in all specified files in the background." ()
  (let* ((pathname (READ-defaulted-pathname
                     "File or files to use as tag table:"
                     (make-pathname :defaults (DEFAULT-PATHNAME) :name :wild :type :lisp :version :newest)))
         (newest-p (typecase (pathname-version pathname)
                     (null t)
                     ((member :newest :unspecific) t)))
         (file-list (mapcar #'(lambda(path)
                                (if newest-p
                                    (send path :new-pathname :version :newest)
                                  path))
                            (directory pathname))))
    (if (null file-list)
        (barf "No files match ~A" pathname)
      (let ((tag-table-name (typein-line-readline "Name for this tag table:")))
        (SELECT-FILE-LIST-AS-TAG-TABLE
          file-list
          (do ((used-name tag-table-name
                          (typein-line-readline
                            (format nil "/"~a/" is already the name of a tag table. ~
                                       Please enter another name for this new one:"
                                    used-name))))
              (nil)
            (if (not (ass #'string-equal used-name *zmacs-tag-table-alist*))
                (return used-name))))
        ;;;Optionally, pre-load all files
        (when (or *zmacs-prefer-preload-tag-table-files-in-background* *numeric-arg-p*)
          (process-run-function `(:name ,(format nil "Loading of ~A" tag-table-name)
                                        :priority -1)   ;don't subordinate front-line file transactions
                                #'(lambda (files window)
                                    (dolist (file files) (or (find-file-buffer file)
                                                             (load-file-into-zmacs file nil)))
                                    (tv:careful-notify
                                      window t
                                      "The files of ~A have been loaded into ZMacs and selected as current tag table."
                                      tag-table-name))
                                ;;Args to background file-finding function:
                                file-list *window*)))
      ;;;Done.
      (format *query-io* "Done.")))
  DIS-NONE)

;;; Edit a list of section nodes matching a specified string.
(DEFCOM COM-MATCHING-SECTIONS "List all sections in a specified buffer matching a given substring.
Each DEFUN, DEFVAR, DEFSTRUCT, etc. is one section." ()
  (LET ((BUFFER (READ-BUFFER-NAME "List sections in buffer:" *INTERVAL*))
        (substring (completing-read-from-mini-buffer "Substring against which to match sections:" nil)))
    (RESECTIONIZE-BUFFER BUFFER)
    (FORMAT T "Sections in buffer ~A containing string /"~A/":~2%" BUFFER substring)
    (SEND *STANDARD-OUTPUT* :ITEM-LIST 'SECTION-NAME
          (subset #'(LAMBDA (SECTION-NAME)
                      (string-search substring (format nil "~S" section-name)))
                  (mapcar 'section-node-name (NODE-INFERIORS BUFFER)))))
  DIS-NONE)

(defcom com-kill-all-buffers-in-system-related-directories
        "Flushes buffers which are part of a specified system's related directories."
        ()
  (let* ((system-name (read-system-name "System whose associated buffers need removal:"))
         (system-source-files-list (si:system-source-files system-name si:*source-file-types* nil t))
         outlist
         (dir-list (dolist (each-file system-source-files-list outlist)
                     (pushnew (send each-file :string-for-directory)
                              outlist
                              :test #'string-equal)))
         (len (length dir-list))
         (list-of-buffers-to-be-killed)
         )
    (do ((i 0 (1+ i)) (dir-name)) ((= i len))
      (setq dir-name (format () "~a" (elt (print dir-list) i))
            dir-name (print (subseq dir-name 0 (position #/; dir-name))))
      (and (mem #'string-equal dir-name dir-list)
           (y-or-n-p "Release file buffers in system ~A for files in directory ~a? " system-name dir-name)
           (setq dir-list (append dir-list (list dir-name)))))
    (dolist (buffer *zmacs-buffer-list*)
      (let ((buffer-name (format () "~a" (cdar (pathname-defaults *pathname-defaults* buffer)))))
        (dolist (dir-name dir-list)
          (when (string-equal buffer-name dir-name
                              :end1 (position #/; buffer-name)
                              :end2 (position #/; dir-name))
            (setq list-of-buffers-to-be-killed
                  (append list-of-buffers-to-be-killed
                          (progn
                            (and (send buffer :modified-p)
                                 (y-or-n-p "Buffer ~a was modified, save it first? " buffer-name)
                                 (save-buffer buffer))
                            (list buffer))))
            (return)))))
    (and (fquery () "There are ~d buffers associated with system ~a out of ~d Zmacs buffers.
Begin removing associated buffers? "
                 (length list-of-buffers-to-be-killed)
                 system-name
                 (length *zmacs-buffer-list*))
         (progn
           (dolist (buffer list-of-buffers-to-be-killed)
             (setq *zmacs-buffer-list* (remq buffer *zmacs-buffer-list*))
             (send buffer :kill))
           (format *query-io* "~d buffers associated with system ~a flushed."
                   (length list-of-buffers-to-be-killed)
                   system-name)))
    (make-buffer-current (car *zmacs-buffer-list*)))
  dis-text)

(defcom COM-KILL-BUFFERS-BY-DEFAULT "Runs Kill Or Save Buffers, with all buffers marked for killing." ()
  (let ((*numeric-arg* 4))
    (com-kill-or-save-buffers)))

;;;;;;;;;;
;;;;;;;;;; Now install all newly-defined commands (plus some obscure
;;;;;;;;;; Zwei commands), and announce changes to existing
;;;;;;;;;; functionalities.
;;;;;;;;;;

(send terminal-io :clear-screen)

(install-command-and-advertise 'com-rotate-buffer-stack #/c-H *standard-comtab*
                               "rotates the buffer stack")

(install-command-and-advertise 'com-bury-buffer #/c-I *standard-comtab*
                               "buries the current buffer in the current buffer list")

(install-command-and-advertise 'COM-INDENT-SEXP-AND-MOVE #/c-m-q *standard-comtab*
                               "indents the sexp beginning on the current line")

(install-command-and-advertise 'COM-STRING-SEARCH-SELECT-BUFFERS-AS-TAG-TABLE #/c-m-s *standard-comtab*
                               "string matches on buffer names")

(install-command-and-advertise 'com-fill-lisp-buffer #/Control-Meta-Shift-F *zmacs-comtab*
                               "justifies all defuns in a Lisp buffer")

(install-command-and-advertise 'com-fill-long-comment #/Meta-G *zmacs-comtab*
                               "fills a long comment")

(install-command-and-advertise 'com-fill-text-buffer #/Meta-Shift-F *zmacs-comtab*
                               "fills all paragraphs in a text buffer")

(install-command-and-advertise 'com-select-files-as-tag-table #/Meta-Shift-S *zmacs-comtab*
                               "selects user-specified files as a tag table")

(install-command-and-advertise 'com-kill-buffers-by-default #/Super-K *zmacs-comtab*
                               "runs Kill Or Save Buffers, all buffers marked Kill")

(install-command-and-advertise 'com-refind-file #/Super-S *zmacs-comtab*
                               "performs a Refind File on the current buffer")

(install-command-and-advertise 'com-find-directory-files #/s-t *standard-comtab*
                               "reads in all files matching a pathname specification")


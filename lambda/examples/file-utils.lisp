;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-


#||

Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright.Text" for
licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

These are utiltities I use in maintaining our filecomputers at LMI.
-George Carrette. They are used in addition to the tape backup
facilities. The emphasis here is on HOST->HOST backup, and filesystem
purging of extra versions of files. Functions:

General utility functions:

 MAP-ALL-FILES:
   Maps a function over all files the match a given filespec, recursively.
   This is used in many of the other utilities. It takes various optional
   keyword arguments.

 COMPUTE-ALL-FILES:
   Returns a list of all files matching a specification and a predicate.

 COMPUTE-EXTRA-FILES:
   Filters a list of files returning those that extra versions etc.

 APPEND-DIRECTORY-NAMES
  Takes care of the special case of :ROOT etc.

 CDR-DIRECTORY-NAME
  Takes care of the special case of :ROOT etc.

Specific utility functions:

 BACKUP-HOST-TO-HERE:
  Copies source files from certain directories on a host to the local filesystem.
  If you dont supply the :DIRS argument to this then you should set up a property
  on the host, e.g.
   (putprop (si:parse-host "LAMBDA-A")
            '(GJC PACE RG MUSIC DG JRM)
            'DIRECTORIES-TO-BACKUP)
  It is best to use this function every evening to keep up-to-date copies of
  important directories available on more than one filecomputer.

 FILESYSTEM-PURGE:
  Use this to delete extra versions of files. There are two useful kinds of :VERSIONS-TO-KEEP
  argument. The default value of a number "1" deletes all but the latest version of each file.
  (A value of "0" would recursively delete all files.) Otherwise the argument should be
  a symbol with a COMPUTE-EXTRA-FILES property. This property is a function that gets
  called on two arguments, the :VERSIONS-TO-KEEP argument and a list of files
  that all have the same :NAME and :TYPE sorted by version number, greater versions last.

 FILESYSTEM-EXPUNGE:
  Calls FS:EXPUNGE-DIRECTORY on all directories in a filesystem.

 COPY-ALL-FILES
  Used for moving files with transform under functional control.

 COPY-TO-NEW-ROOT
  Used for moving software from one host/place to another, possibly changing
  the root directory. E.G (COPY-TO-NEW-ROOT "DJ:XSYSTEM;" "LAM3:RELEASE-3;" :STRIP-ROOT 1)
  To copy a developement system to the place for the released system.

 16B-FILE-TO-ASCII
  Used for transporting microcode files and other 16B binary files
  across the network. Expands file size by 3/2.

 ASCII-FILE-TO-16B
  Inverse operation of 16B-FILE-TO-ASCII.

 SXHASH-FILE
  Returns the sum of the SXHASH substrings of the characters in the file.

Specialized utilities:

  CLEAN-UBIN-DIR:
   Useful if you are generating a lot of versions of the system microcode.
   Can also be used to cleanup any directory that has a related file type
   property.

||#

(DEFUN PURGE-SOME-DIRS (&OPTIONAL (VERSIONS 3))
  (DOLIST (DIR (SUBSET #'(LAMBDA (X)
                           (Y-OR-N-P "&PURGE DIRECTORY ~S?" X))
                       (DELQ NIL (MAPCAR #'(LAMBDA (X)
                                             (AND (CAR X) (SEND (CAR X) :NAME)))
                                         (FS:DIRECTORY-LIST "LM:~;*.DIRECTORY#>")))))
    (FILESYSTEM-PURGE :VERSIONS-TO-KEEP VERSIONS
                      :DIRECTORY DIR)))

(DEFUN SOURCE-FILEP (X)
  (MEM #'STRING-EQUAL (SEND X :TYPE) '("TEXT" "LISP" "BOTEX" "TEX" "TXT" "INIT" "C" "H" "LSP"
                                       "DIRECTORY" "PATCH-DIRECTORY")))

(defun backup-host-to-here (HOST-NAME &OPTIONAL &KEY DIRS (PURGE T))
  (LET ((MORE-P (SEND TERMINAL-IO :MORE-P))
        (DTO (SEND TERMINAL-IO :DEEXPOSED-TYPEOUT-ACTION))
        (PRIO (SEND CURRENT-PROCESS :PRIORITY))
        (QUANT (SEND CURRENT-PROCESS :QUANTUM)))
    (UNWIND-PROTECT
        (PROGN (SEND TERMINAL-IO :SET-MORE-P NIL)
               (SEND TERMINAL-IO :SET-DEEXPOSED-TYPEOUT-ACTION :PERMIT)
               (SEND CURRENT-PROCESS :SET-QUANTUM 10)
               (SEND CURRENT-PROCESS :SET-PRIORITY -1)
               (backup-host-to-here-1 HOST-NAME DIRS PURGE))
      (SEND TERMINAL-IO :SET-MORE-P MORE-P)
      (SEND TERMINAL-IO :SET-DEEXPOSED-TYPEOUT-ACTION DTO)
      (SEND CURRENT-PROCESS :SET-QUANTUM QUANT)
      (SEND CURRENT-PROCESS :SET-PRIORITY PRIO))))


(DEFUN backup-host-to-here-1 (HOST-NAME DIRS PURGE)
  (LET ((H (SI:PARSE-HOST HOST-NAME))
        (START-TIME (TIME))
        (DIR-TIME)
        (SCAN-TIME)
        (COPY-TIME)
        (PURGE-TIME)
        (TARGET)
        (FILES)
        (CFILES)
        (PURGE-RESULT)
        (NEED-BLOCKS 0))
    (SETQ TARGET (FS:PARSE-PATHNAME (FORMAT NIL "~A:~A;" (SEND SI:LOCAL-HOST :NAME) H)))
    (OR DIRS (SETQ DIRS (GET H 'DIRECTORIES-TO-BACKUP)))
    (FORMAT T "~&[Going to backup ~S to ~S]~%" H TARGET)
    (FORMAT T "[DIRECTORIES: ~S]~%" DIRS)
    (DOLIST (DIR DIRS)
      (MAP-ALL-FILES #'(LAMBDA (P)
                         (IF (SOURCE-FILEP (CAR P))
                             (PUSH P FILES)))
                     (DIR-FROM-SPEC H DIR)
                     :VERSION :NEWEST
                     :BEFORE-F #'(LAMBDA (DIR)
                                   (FORMAT T "~&DESCENDING ~A~%" DIR))
                     :CALL-ON-PLIST T))
      (SETQ FILES (NREVERSE FILES))
      (SETQ DIR-TIME (QUOTIENT (TIME-DIFFERENCE (TIME) START-TIME) 3600.0))
      (SETQ START-TIME (TIME))
      (FORMAT T "~&[TOOK ~$ MINUTES TO LIST THE DIRECTORIES]~%" DIR-TIME)
      (FORMAT T "~&[ ~D FILES TO CONSIDER]~%" (LENGTH FILES))
      (SETQ START-TIME (TIME))
      (DOLIST (FILE FILES)
        (LET ((NEW-FILE (SEND (CAR FILE) :NEW-PATHNAME
                              :HOST (SEND TARGET :HOST)
                              :DIRECTORY (APPEND-DIRECTORY-NAMES (SEND TARGET :DIRECTORY)
                                                                 (SEND (CAR FILE) :DIRECTORY)))))
          (FS:CREATE-DIRECTORY NEW-FILE)
          (COND ((PROBE-FILE NEW-FILE)
                 (FORMAT T "~&ALREADY EXISTS: ~A~%" NEW-FILE))
                ('ELSE
                 (INCF NEED-BLOCKS (GET FILE :LENGTH-IN-BLOCKS))
                 (PUSH `(,(CAR FILE) ,NEW-FILE ,(CDR FILE)) CFILES)))))
      (SETQ SCAN-TIME (QUOTIENT (TIME-DIFFERENCE (TIME) START-TIME) 3600.0))
      (SETQ CFILES (NREVERSE CFILES))
      (FORMAT T "~&[SCAN OF TARGET TOOK ~$ MINUTES. ~D FILES TO COPY, ~D BLOCKS NEEDED]~%"
              SCAN-TIME (LENGTH CFILES) NEED-BLOCKS)
      (IF (< (+ (aref FS:put-usage-array FS:put-free) FS:*disk-space-warner-threshold*)
             NEED-BLOCKS)
          (CERROR "COPY ANYWAY"
                  "Need ~D Blocks to copy the files, but have only ~D free blocks on local file"
                  NEED-BLOCKS
                  (aref FS:put-usage-array FS:put-free)))
      (SETQ START-TIME (TIME))
      (DOLIST (FILED CFILES)
        (APPLY #'COPY-ONE-FILE FILED)
        (FORMAT T "~&[~D FILES TO GO]" (LENGTH (CDR (MEMQ FILED CFILES)))))
      (SETQ COPY-TIME (QUOTIENT (TIME-DIFFERENCE (TIME) START-TIME) 3600.0))
      (FORMAT T "~%[COPY TOOK ~$ MINUTES. PURGING BACKUP TARGET]~%" COPY-TIME)
      (SETQ START-TIME (TIME))
      (IF PURGE
          (SETQ PURGE-RESULT (FILESYSTEM-PURGE :VERSIONS-TO-KEEP 3
                                               :DIRECTORY (SEND TARGET :DIRECTORY)
                                               :HOST (SEND TARGET :HOST))))
      (SETQ PURGE-TIME (QUOTIENT (TIME-DIFFERENCE (TIME) START-TIME) 3600.0))
      (FORMAT T "~%[PURGE TOOK ~$ MINUTES, ~D BLOCKS SAVED]~%" PURGE-TIME PURGE-RESULT)
      (WITH-OPEN-FILE (LOG (SEND TARGET :NEW-PATHNAME :NAME
                                 (SUBSTRING (TIME:PRINT-CURRENT-TIME NIL :YY-MMM-DD) 0 9)
                                 :TYPE "TEXT"
                                 :VERSION :NEWEST)
                           :DIRECTION :OUTPUT)
        (TIME:PRINT-CURRENT-DATE LOG)
        (FORMAT LOG "~%Going to backup ~S to ~S~%" H TARGET)
        (FORMAT LOG "DIRECTORIES: ~S~%" DIRS)
        (FORMAT LOG "Took ~$ minutes to list the directories, ~D files to consider.~%" DIR-TIME (LENGTH FILES))
        (FORMAT LOG "Scan of target took ~$ minutes. ~D files to copy, ~D blocks needed~%"
                SCAN-TIME (LENGTH CFILES) NEED-BLOCKS)
        (format log "Copy took ~$ minutes~%" COPY-TIME)
        (format LOG "Purge took ~$ minutes, ~d blocks saved.~%" PURGE-TIME PURGE-RESULT))))

(DEFUN DIR-FROM-SPEC (HOST DIR)
  (ECASE (SEND HOST :SYSTEM-TYPE)
    (:LISPM
     (FORMAT NIL "~A:~A;"
             (SEND HOST :NAME)
             (IF (CONSP DIR)
                 (FORMAT NIL "~{~A~^.~}" DIR) DIR)))
    (:UNIX
     (FORMAT NIL "~A:~A//"
             (SEND HOST :NAME)
             (IF (CONSP DIR)
                 (FORMAT NIL "~{~A~^//~}" DIR) DIR)))))

(DEFUN FILESYSTEM-PURGE (&OPTIONAL &KEY (VERSIONS-TO-KEEP 1) (DIRECTORY :ROOT) (HOST SI:LOCAL-HOST)
                         IGNORE-DIRECTORIES DONT-ASK)
  (LET ((SAVED-BLOCKS 0)
        (path (fs:make-pathname :host HOST
                                :directory directory)))
    (if (AND (NUMBERP VERSIONS-TO-KEEP) (zerop versions-to-keep))
        (or DONT-ASK
            (yes-or-no-p "~&Do you really want to delete all the files in ~A?" path)
            (return-from filesystem-purge nil)))
    (MAP-ALL-FILES
      #'(LAMBDA (FILES)
          (LET ((EXTRA-FILES (COMPUTE-EXTRA-FILES FILES VERSIONS-TO-KEEP)))
            (WHEN EXTRA-FILES
              (format t "~D extra files " (length extra-files))
              (condition-case (x)
                  (send (car extra-files)
                        :delete-multiple-files
                        t extra-files)
                (fs:dont-delete-flag-set
                 (format t "~&;Losing because ~A~%" (send x :report-string))
                 (dolist (f extra-files)
                   (format t "File-> ~S~%" f)
                   (condition-case (x)
                       (deletef f)
                     ((fs:dont-delete-flag-set fs:open-deleted-file)
                      (format t "~&;Losing because ~A~%" (send x :report-string))))))))
            (if files
                (let ((saved (fs:expunge-directory (send (car files)
                                                         :new-pathname
                                                         :name :wild
                                                         :type :wild
                                                         :version :wild))))
                  (format t "~D block~p saved~%" saved saved)
                  (incf saved-blocks saved)))))
      path
      :before-f #'(lambda (x)
                    (format t "~&Hacking ~A " x)
                    (when (mem #'equalp (send x :directory) ignore-directories)
                      (format t "ignored~%")
                      :ignore))
      :call-on-list t)
    saved-blocks))


(defun compute-extra-files (files versions-to-keep)
  ;; lets not assume any ordering of the names, types, and versions of the files
  ;; Not getting into any general predicate language for versions to keep we
  ;; have but two kinds of values, a number, and :STANDARD.
  (and files
       (let ((sorted (sort files #'(lambda (f1 f2)
                                     (let ((n (string-compare (send f1 :name)
                                                              (send f2 :name)
                                                              0 0 nil nil nil)))
                                       (cond ((minusp n)
                                              t)
                                             ((plusp n)
                                              nil)
                                             ((minusp (setq n (string-compare (send f1 :type)
                                                                              (send f2 :type)
                                                                              0 0 nil nil nil)))
                                              t)
                                             ((plusp n)
                                              nil)
                                             ((minusp (setq n (- (send f1 :version)
                                                                 (send f2 :version))))
                                              t)))))))
         (let ((current-type (send (car sorted) :type))
               (current-name (send (car sorted) :name))
               (partition (list (car sorted)))
               (extras nil))
           (dolist (file (cdr sorted))
             (cond ((and (string= (send file :type) current-type)
                         (string= (send file :name) current-name))
                    (push file partition))
                   ('else
                    (setq extras (append (funcall-extra-files-filter versions-to-keep
                                                                     (nreverse partition))
                                         extras))
                    (setq partition (ncons file))
                    (setq current-type (send file :type))
                    (setq current-name (send file :name)))))
           (setq extras (append (funcall-extra-files-filter versions-to-keep
                                                            (nreverse partition))
                                extras))
           extras))))


(defun funcall-extra-files-filter (versions-to-keep files)
  (funcall (if (numberp versions-to-keep)
               'compute-extra-files-numeric
             (or (get versions-to-keep 'compute-extra-files)
                 'compute-extra-files-default))
           versions-to-keep files))

(defun compute-extra-files-numeric (versions-to-keep files)
  (butlast files versions-to-keep))

(defun compute-extra-files-default (versions-to-keep files)
  (cerror "try again" "dont know how to compute extra files with: ~S" versions-to-keep)
  (funcall-extra-files-filter versions-to-keep files))


(defun (:standard compute-extra-files) (ignore files)
  (format t "~&~A.~A#~{~D~^,~} =>" (send (car files) :name) (send (car files) :type)
          (mapcar #'(lambda (file) (send file :version)) files))
  (let ((extras (selector (send (car files) :type) string-equal
                          ("QFASL"
                           (butlast files))
                          (("LMC" "LMC-DCL" "LMC-LOCS" "LMC-SYM" "LMC-TBL"
                            "EMC" "EMC-DCL" "EMC-LOCS" "EMC-SYM" "EMC-TBL")
                           ;; never purge microcodes.
                           ())
                          (t
                           ;; keep the oldest plus the three newest of source files.
                           (cond ((< (length files) 3) nil)
                                 ('else
                                  (butlast (nthcdr 1 files) 3)))))))
    (format t "~{~D~^,~}~%" (mapcar #'(lambda (file) (send file :version)) extras))
    extras))


(DEFUN (:QFASL COMPUTE-EXTRA-FILES) (IGNORE FILES)
  (IF (STRING-EQUAL "QFASL" (SEND (CAR FILES) :TYPE))
      FILES))

(DEFUN APPEND-DIRECTORY-NAMES (A B)
  (LET ((RESULT (APPEND (COND ((EQ A :ROOT) NIL)
                              ((ATOM A) (LIST A))
                              ('ELSE A))
                        (COND ((EQ B :ROOT) NIL)
                              ((ATOM B) (LIST B))
                              ('ELSE B)))))
    (IF (= (LENGTH RESULT) 1)
        (CAR RESULT)
      RESULT)))


(DEFUN CDR-DIRECTORY-NAME (A)
  (COND ((EQ A :ROOT) A)
        ((ATOM A) :ROOT)
        ((= (LENGTH A) 1) :ROOT)
        ('ELSE (CDR A))))

(DEFUN NTHCDR-DIRECTORY-NAME (N A)
  (DO ((B A (CDR-DIRECTORY-NAME B))
       (J 0 (1+ J)))
      ((= J N) B)))

(defun map-all-files (f filespec &optional &key (recursive t)
                      (name :wild)
                      (type :wild)
                      (version :wild)
                      before-f
                      call-on-list
                      CALL-ON-PLIST)
  (labels ((recurse (path)
                    (cond ((and before-f (eq (funcall before-f path) :ignore))
                           nil)
                          ('else
                           (DO ((L (CDR (FS:DIRECTORY-LIST path))
                                   (CDR L))
                                (DIRS-FOUND NIL)
                                (files-found nil))
                               ((NULL L)
                                (if call-on-list
                                    (funcall f (nreverse files-found))
                                  (dolist (elem (nreverse files-found))
                                    (funcall f elem)))
                                (dolist (dir (nreverse dirs-found))
                                  (recurse (SEND PATH :NEW-PATHNAME
                                                 :DIRECTORY (APPEND-DIRECTORY-NAMES
                                                              (SEND DIR :DIRECTORY)
                                                              (SEND DIR :NAME))
                                                 :NAME name
                                                 :TYPE TYPE
                                                 :VERSION VERSION))))
                             (COND ((GET (CAR L) :DIRECTORY)
                                    (if recursive (PUSH (CAAR L) DIRS-FOUND)))
                                   (CALL-ON-PLIST
                                    (PUSH (CAR L) FILES-FOUND))
                                   ('else
                                    (push (caar l) files-found))))))))
    (recurse (send (fs:parse-pathname filespec)
                   :new-pathname
                   :name name
                   :type type
                   :version version))))


(DEFVAR *UBIN-ALIST* '((("ULAMBDA" "LMC") "LMC-DCL" "LMC-SYM" "LMC-TBL")
                       ;; *-LOCS are preserved for reference. They are small files.
                       (("ULAMBDA" "EMC") "EMC-DCL" "EMC-SYM" "EMC-TBL")))

(defun clean-ubin-dir (&OPTIONAL (DIR "SYS:UBIN;"))
  "Delete types as per *UBIN-ALIST*"
  (LET ((PATH (SEND (FS:PARSE-PATHNAME DIR)
                    :NEW-PATHNAME
                    :NAME "ULAMBDA"
                    :TYPE :WILD
                    :VERSION :WILD)))
    (FORMAT T "~&;CLEANING UP ~S~%" PATH)
    (LET ((L (DELQ NIL (MAPCAR #'CAR (FS:DIRECTORY-LIST PATH))))
          (GARBAGE))
      (DOLIST (F L)
        (DOLIST (A *UBIN-ALIST*)
          (WHEN (AND (MEM #'STRING-EQUAL (SEND F :TYPE) (CDR A))
                     (STRING-EQUAL (SEND F :NAME) (CAAR A)))
            (PRINC ".")
            (WHEN (NOT (MEM #'(LAMBDA (E P)
                                (AND (EQ (CAR E) (SEND P :VERSION))
                                     (STRING-EQUAL (CADR E) (SEND P :NAME))
                                     (STRING-EQUAL (CADDR E) (SEND P :TYPE))))
                            (CONS (SEND F :VERSION) (CAR A))
                            L))
              (FORMAT T "GARBAGE COLLECTING ~S~%" F)
              (PUSH F GARBAGE)))))
      (WHEN GARBAGE
        (FORMAT T "DELETING ~D FILE~P...~%" (LENGTH GARBAGE) (LENGTH GARBAGE))
        (SEND (CAR GARBAGE) :DELETE-MULTIPLE-FILES T GARBAGE)
        (FORMAT T "EXPUNGING...")
        (FORMAT T "~D BLOCKS SAVED~%" (FS:EXPUNGE-DIRECTORY PATH))))))

(defun filesystem-expunge (&optional &key (directory :root) (HOST SI:LOCAL-HOST))
  (let ((total 0))
    (dolist (d (filesystem-directories :host host :directory directory))
      (format t "~&;Expunging ~S~%" d)
      (let ((amount (fs:expunge-directory d)))
        (format t ";~D blocks saved~%" amount)
        (incf total amount)))
    total))


(defun filesystem-directories (&key &optional (host si:local-host) (directory :root))
  (let (dirs)
    (map-all-files #'(lambda (x) x)
                   (make-pathname :host host
                                  :directory directory)
                   :type "DIRECTORY"
                   :name :wild
                   :version :newest
                   :before-f #'(lambda (x)
                                 (format t "~&; ~S~%" x)
                                 (push (send x :new-pathname
                                             :name :wild
                                             :type :wild
                                             :version :wild)
                                       dirs)))
    (sort dirs #'(lambda (p1 p2)
                   (> (directory-name-depth (send p1 :directory))
                      (directory-name-depth (send p2 :directory)))))))


(defun directory-name-depth (x)
  (cond ((eq x :root) 0)
        ((atom x) 1)
        ('else (length x))))


(defun copy-all-files (filespec &optional &key transform (recursive t) (name :wild) (type :wild) (version :newest))
  (let ((files (compute-all-files filespec
                                  :transform transform :recursive recursive
                                  :name name :type type :version version)))
    (format t "~&Computing which files to copy from ~S~%" filespec)
    (format t "~D files to copy~%" (length files))
    (dolist (file files)
      (copy-one-file (caar file) (cadr file) (cdar file)))))


(DEFUN COPY-TO-NEW-ROOT (FILESPEC TARGET-ROOT &OPTIONAL &KEY FILTER-TYPES (STRIP-ROOT 0)
                         FILTER-DIRECTORIES
                         (VERSION :NEWEST))
  (LET ((TARGET (FS:PARSE-PATHNAME TARGET-ROOT)))
    (COPY-ALL-FILES FILESPEC
                    :TRANSFORM #'(LAMBDA (P)
                                   (COND ((MEM #'STRING-EQUAL (SEND P :TYPE) FILTER-TYPES)
                                          ())
                                         ((MEM #'SUBDIRECTORY-P
                                               (SEND P :DIRECTORY)
                                               FILTER-DIRECTORIES)
                                          ())
                                         ('ELSE
                                          (SEND P :NEW-PATHNAME
                                                :HOST (SEND TARGET :HOST)
                                                :DIRECTORY (APPEND-DIRECTORY-NAMES
                                                             (SEND TARGET :DIRECTORY)
                                                             (NTHCDR-DIRECTORY-NAME
                                                               STRIP-ROOT
                                                               (SEND P :DIRECTORY)))))))
                    :VERSION VERSION)))

(DEFUN SUBDIRECTORY-P (DIR2 DIR1)
  "RETURN T IF DIR2 IS A SUBDIRECTORY OF DIR1"
  (COND ((EQ DIR1 :ROOT)
         T)
        ((ATOM DIR1)
         (COND ((ATOM DIR2)
                (STRING-EQUAL DIR1 DIR2))
               ('ELSE
                (STRING-EQUAL DIR1 (CAR DIR2)))))
        ((ATOM DIR2)
         ())
        ('ELSE
         (DO ((L1 DIR1 (CDR L1))
              (L2 DIR2 (CDR L2)))
             ((NULL L1) T)
           (OR (STRING-EQUAL (CAR L1) (CAR L2))
               (RETURN NIL))))))


(defun compute-all-files (filespec &key transform (recursive t) (name :wild) (type :wild) (version :newest))
  (let ((files))
    (map-all-files #'(lambda (p)
                       (let ((new (compute-copy-transform transform (car p))))
                         (cond (new
                                (push (list p new) files)))))
                   filespec
                   :recursive recursive
                   :name name
                   :type type
                   :version version
                   :before-f #'(lambda (path)
                                 (format t "~&Descending ~S~%" path))
                   :call-on-plist t)
    (nreverse files)))

(defun compute-copy-transform (f pathname)
  (cond ((null f)
         pathname)
        ((stringp f)
         (cond ((or (string-equal f "MT")
                    (string-equal f "MT:"))
                "MT:")
               ('else
                (send pathname :new-pathname :host f))))
        ('else
         (funcall f pathname))))

(defun copy-one-file (input-pathname output-pathname plist)
  (FS:CREATE-DIRECTORY OUTPUT-PATHNAME)
  (COND ((PROBE-FILE OUTPUT-PATHNAME)
         (FORMAT T "~&Already exists: ~S~%" OUTPUT-PATHNAME))
        ('ELSE
         (COPY-ONE-FILE-1 input-pathname output-pathname plist))))

(DEFVAR *COPY-ONE-FILE-PROPERTIES* '(:AUTHOR :CREATION-DATE)
  "Properties to carry over to the new file via a :CHANGE-PROPERTIES message")

(DEFUN COPY-ONE-FILE-1 (INPUT-PATHNAME OUTPUT-PATHNAME PLIST)
  (LET ((TIME (TIME)))
    (WITH-OPEN-FILE (INSTREAM INPUT-PATHNAME
                              :DIRECTION :INPUT
                              :CHARACTERS (GETF PLIST :CHARACTERS)
                              :BYTE-SIZE (GETF PLIST :BYTE-SIZE)
                              :ERROR NIL)
      (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
      (COND ((ERRORP INSTREAM)
             (SEND INSTREAM :REPORT *ERROR-OUTPUT*))
            ('ELSE
             (FORMAT T "~&~A (~\scientific\seconds)~%" (SEND INSTREAM :TRUENAME) TIME)
             (SETQ TIME (TIME))
             (WITH-OPEN-FILE (OUTSTREAM OUTPUT-PATHNAME
                                        :DIRECTION :OUTPUT
                                        :CHARACTERS (GETF PLIST :CHARACTERS)
                                        :BYTE-SIZE (GETF PLIST :BYTE-SIZE))
               (LET ((COPY-PLIST NIL)
                     (NOT-VALUE (LIST NIL)))
                 (DOLIST (P *COPY-ONE-FILE-PROPERTIES*)
                   (LET ((VALUE (GETF PLIST P NOT-VALUE)))
                     (OR (EQ VALUE NOT-VALUE)
                         (SETF (GETF COPY-PLIST P) VALUE))))
                 (WHEN COPY-PLIST
                   (LEXPR-SEND OUTSTREAM :CHANGE-PROPERTIES NIL COPY-PLIST)))
               (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
               (FORMAT T "  ==> ~A (~\scientific\seconds)~%" (SEND OUTSTREAM :TRUENAME) TIME)
               (SETQ TIME (TIME))
               (DO ((BUFFER)
                    (OFFSET)
                    (LIMIT)
                    (BYTES 0))
                   (())
                 (MULTIPLE-VALUE (BUFFER OFFSET LIMIT)
                   (SEND INSTREAM :READ-INPUT-BUFFER))
                 (WHEN (NULL BUFFER)
                   (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
                   (FORMAT T
                           " copy: ~\scientific\seconds, ~\scientific\bytes per second~%"
                           TIME (QUOTIENT BYTES (IF (ZEROP TIME) 1 TIME)))
                   (RETURN NIL))
                 (INCF BYTES (- LIMIT OFFSET))
                 (SEND OUTSTREAM :STRING-OUT BUFFER OFFSET LIMIT)
                 (SEND INSTREAM :ADVANCE-INPUT-BUFFER))
               (SETQ TIME (TIME)))
             (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
             (FORMAT T " output-close: ~\scientific\seconds" TIME)))
      (SETQ TIME (TIME)))
    (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
    (FORMAT T " input-close: ~\scientific\seconds." TIME)))


(defun 16b-file-to-ascii (input-file output-file)
  (with-open-file (input input-file :characters nil :byte-size 16)
   (with-open-file (Output output-file :direction :output)
     (16b-stream-to-ascii input output))))

(defun ascii-file-to-16b (input-file output-file)
  (with-open-file (input input-file)
   (with-open-file (Output output-file :direction :output
                           :characters nil :byte-size 16)
     (ascii-stream-to-16b input output))))


(defvar *output-character-table* (make-array 64))
(defvar *input-character-table* (make-array 256))

(do ((j 0 (1+ j))
     (l '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
          #\A #\B #\C #\D #\E #\F #\G #\H #\I
          #\J #\K #\L #\M #\N #\O #\P #\Q #\R
          #\S #\T #\U #\V #\W #\X #\Y #\Z
          #\a #\b #\c #\d #\e #\f #\g #\h #\i
          #\j #\k #\l #\m #\n #\o #\p #\q #\r
          #\s #\t #\u #\v #\w #\x #\y #\z
          #\* #\-)
        (CDR L)))
    ((= J 64))
  (ASET (CAR L) *OUTPUT-CHARACTER-TABLE* J)
  (ASET J *INPUT-CHARACTER-TABLE* (CAR L)))

(defun 16b-stream-to-ascii (input output)
  (do ((j 0 (1+ j))
       (word)
       (TABLE *OUTPUT-CHARACTER-TABLE*))
      ((null (setq word (Send input :tyi)))
       (terpri output))
    (send output :tyo (AREF TABLE (LDB (BYTE 4 12) WORD)))
    (send output :tyo (AREF TABLE (LDB (BYTE 6 6) WORD)))
    (send output :tyo (AREF TABLE (LDB (BYTE 6 0) WORD)))
    (if (= 19 (remainder j 20)) (terpri output))))

(defun ascii-stream-to-16b (input output)
  (do ((char)
       (TABLE *INPUT-CHARACTER-TABLE*))
      ((null (setq char (send input :tyi))))
    (LET ((WORD (AREF TABLE CHAR)))
      (WHEN WORD
        (SETQ WORD (+ (AREF TABLE (SEND INPUT :TYI)) (LSH WORD 6)))
        (SETQ WORD (+ (AREF TABLE (SEND INPUT :TYI)) (LSH WORD 6)))
        (SEND OUTPUT :TYO WORD)))))


(DEFUN SXHASH-FILE (FILENAME &OPTIONAL &KEY (CHUNK-SIZE 1024))
  (LET ((BUFFER (MAKE-STRING CHUNK-SIZE)))
    (WITH-OPEN-FILE (INPUT FILENAME)
      (DO ((END)
           (EOFP)
           (HASHSUM 0 (PROGN (MULTIPLE-VALUE-SETQ (END EOFP) (SEND INPUT :STRING-IN NIL BUFFER))
                             (COMPILER::%SXHASH-SUBSTRING BUFFER #o377 0 END))))
          (EOFP
           HASHSUM)))))

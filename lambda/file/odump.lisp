;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-
;;; Dump stuff (which will be obsolete as of February 1984) moved to SYS:FILE;ODUMP
;;;
;;; Entry functions which deal with dribble files as well as magtape...
;;; LM-FULL-DUMP -- do a full dump of the file system.
;;; LM-INCREMENTAL-DUMP -- do an incremental dump of the file system.
;;; LM-FULL-DUMP-DIRECTORIES -- do a full dump of specific directories
;;; LM-DUMP -- do a dump of specific files.
;;; DUMP-RESTORE -- look up and restore file(s) which has been DUMPed.
;;; DUMP-LIST-FILES -- given a tape name, list the files on it.
;;; RESTORE-FILES -- given a number of pathnames, restore them selectively
;;; RESTORE -- more general form of RESTORE-FILES
;;;

;;; Info on dumps is kept in a file in the FS: MAGTAPE; directory.
;;; Name is DUMP-<tape name>.TEXT.0
;;; Format is (pardon notation):
;;;  entry ::= <truename><cr><creation-date><cr><author><cr><plist><cr>
;;;  plist ::= <prop><sp><val><cr>...
;;; So the end of an entry is signified by two CRs.
;;; Defined properties are:
;;;  :NSC -- The file has not been source compared.

;;; Modification history
;;;
;;; dumping stuff
;;; started by DLA sometime ago
;;; hacked by ALR summer of 1982? so that dumps would work somewhat reasonably
;;; improved by HGA starting 11/18/82


(DEFVAR DUMP-HOST "LM")
(DEFVAR DUMP-VERSION 1)

;;; In core, the format is:
(DEFSTRUCT (TAPE-FILE :LIST* (:DEFAULT-POINTER TAPE-FILE))
  TAPE-NAME                                     ;A string
  TAPE-FILE-NAME                                ;A pathname
  TAPE-FILE-PROPERTIES                          ;File properties of the file
  TAPE-FILE-TAIL-POINTER                        ;For efficiently adding entries
  TAPE-FILE-ENTRIES)                            ;A list or NIL.

(DEFSTRUCT (TAPE-FILE-ENTRY :LIST)
  ENTRY-TRUENAME                                ;A pathname
  ENTRY-CREATION-DATE                           ;A universal time
  ENTRY-AUTHOR                                  ;A string
  ENTRY-PLIST)                                  ;A list or NIL.

(DEFVAR TAPE-FILE)

(defun setup-tape-file (tape-name
                        &OPTIONAL tape-file-name new-p
                        &AUX entries properties)
  (or tape-file-name
      (setq tape-file-name (tape-file-name-from-tape-name tape-name)))
  (or new-p
      (multiple-value (entries properties)
        (read-tape-file tape-file-name)))
  (setq tape-file (make-tape-file tape-name tape-name
                                  tape-file-name tape-file-name
                                  tape-file-properties properties
                                  tape-file-entries entries))
  (setf (tape-file-tail-pointer tape-file)
        (cond ((last (tape-file-entries tape-file)))
              (t (locf (tape-file-entries tape-file))))))

(defun read-tape-file (tape-file)
  (with-open-file (s tape-file '(:READ :NOERROR))
    (cond ((errorp s) NIL)
          (T (values (LOOP FOR entry = (read-tape-entry s)
                           UNTIL (null entry)
                           COLLECTING entry)
                     NIL;(funcall s ':FILE-PLIST)
                     )))))

(defun read-tape-entry (stream &AUX line eof pathname creation-date author plist prop val tem
                        (package si:pkg-user-package) (ibase 10.))
  (prog NIL
        (multiple-value (line eof)
          (SEND stream ':LINE-IN T))
        (and eof (return nil))
        (setq pathname (merge-pathname-defaults line))
        (setq line (SEND stream ':LINE-IN T))
        (setq creation-date (time:parse-universal-time creation-date 0 NIL NIL))
        (setq author (SEND stream ':LINE-IN T))
     l  (setq line (SEND stream ':LINE-IN T))
        (and (equal line "")
             (return (make-tape-file-entry entry-truename pathname
                                           entry-creation-date creation-date
                                           entry-author author
                                           entry-plist plist)))
        (setq tem (string-search-char #\SP line))
        (setq prop (intern (substring line 0 tem) si:pkg-user-package))
        (setq val (read-from-string line 'SI:NO-EOF-OPTION tem))
        (setq plist (nconc plist (list* prop val nil)))
        (go l)))

(defun dump-list-files (tape-name &AUX tape-file)
  (setup-tape-file tape-name)
  (format T "~%Tape ~A~:[ not found.~;~%~]" (tape-name) (tape-file-entries))
  (mapc 'LIST-ONE-ENTRY (tape-file-entries))
  T)

(defun tape-name-from-tape-file-name (file-name)
  (substring (SEND file-name ':NAME) 5))

(defun list-one-entry (entry)
  (format T "~% ~A~40T" (entry-truename entry))
  (time:print-universal-time (entry-creation-date entry))
  (format T "~60T~A" (entry-author entry)))

(defun write-tape-file ()
  (with-open-file (s (tape-file-name) '(:WRITE))
    (dolist (e (tape-file-entries))
      (write-tape-file-entry e s))
    ;(tv:doplist ((tape-file-properties) prop ind)
    ;(funcall s ':PROPERTY-LIST prop ind))))
    ))

(defun write-tape-file-entry (entry stream
                              &AUX (base 10.) (*Nopoint T)
                                   (package si:pkg-user-package))
  (SEND stream ':LINE-OUT (SEND (entry-truename entry) ':STRING-FOR-PRINTING))
  (time:print-universal-time (entry-creation-date entry) stream)
  (SEND stream ':TYO #\CR)
  (SEND stream ':LINE-OUT (entry-author entry))
  (LOOP FOR p ON (entry-plist entry) BY #'CDDR
        DO (format stream "~A ~S~%" (first p) (second p)))
  (SEND stream ':TYO #\CR))

(defun tape-file-name-from-tape-name (tape-name)
  (merge-pathname-defaults
    (make-pathname ':HOST "FS"
                   ':DIRECTORY "Magtape"
                   ':NAME (format NIL "DUMP-~A" tape-name)
                   ':TYPE "TEXT"
                   ':VERSION dump-version)))

(DEFVAR DUMP-FILE-LIST)
(DEFVAR DUMP-TYPE)
(DEFVAR DUMP-GET-MORE-FILES)

(defun file-to-pathname (file)
  (merge-pathname-defaults
    (make-pathname ':Host "fs"
                   ':DIRECTORY (directory-name (file-directory file))
                   ':Name (file-name file)
                   ':TYPE (file-type file)
                   ':VERSION (file-version file))))

(DEFUN LM-FULL-DUMP (&optional (HOST DUMP-HOST))
    (DUMP-1 HOST "Full" (dump-get-all-lm-files)))

(defun lm-incremental-dump (&OPTIONAL (host DUMP-HOST))
    (dump-1 host "Incremental"
            (dump-get-undumped-lm-files)))

(defun lm-full-dump-directories (dirs &OPTIONAL (host DUMP-HOST))
  (dump-1 host "Random directories"
          (dump-get-directories dirs host)))

(DEFUN LM-DUMP (&OPTIONAL (host DUMP-HOST))
  (LET ((FILES (DUMP-PROMPT-AND-READ-FILES host)))
    (AND FILES
         (DUMP-1 (SEND (CAR FILES) ':HOST) "Random files" FILES))))


;;; this doesn't quite work yet for local file systems because
;;; because read-directory-files wants raw directories

(defun dump-get-directories (dirs host &AUX result tem)
  (cond ((equal host DUMP-HOST)
         (ferror nil
                 "Sorry, you can't dump just a few of the local file system's files yet"))
        (T
         (dolist (dir dirs)
           (setq dir (make-pathname ':host host
                                    ':device ':wild
                                    ':directory dir
                                    ':name ':wild
                                    ':type ':wild
                                    ':version ':wild))
           (setq tem (cdr (SEND dir ':directory-list '(:fast))))
           (setq result (nconc result (mapcar 'car tem))))
         result)))

(defun dump-get-all-lm-files (&OPTIONAL dir continue &AUX dirs result)
  (cond (dir
         (setq dirs
               (LOOP FOR d ON (DIRECTORY-FILES (dc-ROOT-directorY))
                     WHEN (equalp dir (directory-name (first d)))
                     DO
                     (return (if continue d (ncons (first d))))
                     FINALLY (return NIL))))
        (T (setq dirs (dc-ROOT-directorY))))
  (dolist (dir dirs)
    (setq result (nconc result (copylist* (read-directory-files dir)))))
  result)

(defun mark-all-undumped-files-dumped (&OPTIONAL dir continue)
  (LOOP FOR file IN (dump-get-all-lm-files dir continue)
        UNLESS (file-attribute file ':DUMPED)
        DO
        (format T "~%Marking file ~A dumped." file)
        (change-file-properties (file-truename file) NIL ':NOT-BACKED-UP NIL)))

(defun dump-get-undumped-lm-files (&AUX (result nil))
  (LOOP FOR dir IN (DIRECTORY-FILES (dc-ROOT-directorY))
        DO
        (LOOP FOR file IN (read-directory-files dir)
              UNLESS (file-attribute file ':DUMPED)
              DO (push file result)))
  (nreverse result))

;;; this is broken for lists of remote dirs.  individual files work.  - hga

(DEFUN DUMP-PROMPT-AND-READ-FILES (HOST &AUX TEM LINES RESULT)
  (DO () (NIL)
    (FORMAT T "~% FILE= ")
    (SETQ TEM (READLINE))
    (AND (EQUAL TEM "")
         (RETURN NIL))
    (PUSH TEM LINES))
  (DOLIST (LINE LINES)
    (SETQ LINE (MERGE-PATHNAME-DEFAULTS
                 (PARSE-PATHNAME LINE HOST *DEFAULT-PATHNAME-DEFAULTS*)))
    (SETQ TEM (SEND LINE ':DIRECTORY-LIST '(:FAST)))
    (SETQ RESULT (NCONC RESULT (MAPCAR 'CAR TEM))))
  (SI:ELIMINATE-DUPLICATES RESULT))

(defun dump-1 (host dump-type dump-file-list
               &AUX tape-file tape-name tape-file-name probe)
  (*catch 'DUMP-1
    (format T "~%Enter Tape Name: ")
    (setq tape-name (readline))
    (setq tape-file-name (tape-file-name-from-tape-name tape-name))
    (setq probe (open tape-file-name '(:PROBE)))
    (setup-tape-file tape-name tape-file-name T))
    (format T "~&~A dump ~:[starting on~;appending to~] tape ~A, "
            dump-type
            (tape-file-entries)
            (tape-name))
    (time:print-current-time)
    (dump-2 host)
    (write-tape-file)
    (format T "~&~A dump completed on tape ~A." dump-type (tape-name)))

(defun dump-2 (host &AUX current-directory this-host
                         current-directory-start
                         (current-tape-start dump-file-list)
                         (current-file-list dump-file-list)
                         #|compare-end|#)
  (SETQ THIS-HOST (EQ (fs:get-pathname-HOST host) si:local-host))
  (*Catch 'DUMP-2
    (do () (NIL)
      (condition-case ()
          (do () (NIL)
            (if (null current-file-list) (*THROW 'DUMP-2 T))
            (cond ((not (equal current-directory
                           (if this-host
                               (file-directory (first current-file-list))
                               (SEND (first current-file-list) ':directory))))
                   (setq current-directory
                         (if this-host
                             (file-directory (first current-file-list))
                             (SEND (first current-file-list) ':directory)))
                   (setq current-directory-start current-file-list)
                   (format T "~% Dumping ~A" current-directory)))
            (dump-one-file (pop current-file-list) this-host))
        (end-of-tape nil))
      ;; Here reached EOT.  Space back over files until beginning of directory, and reset.
      (mt-space-rev-to-bof 0 (find-position-in-list (first current-file-list)
                                                    current-directory-start))
      (mt-write-eof)
      (mt-rewind)
      ;(setq compare-end (dump-compare-files current-tape-start NIL))
           ;(cond ((neq compare-end current-directory-start)
      ;(ferror NIL "Number of files on tape does not match.  (Bug in DUMP)")))
      ;(mt-rewind)
      (mt-offline)
      (dump-get-new-tape T)
      (setq current-file-list current-directory-start
            current-tape-start current-directory-start)))
  ;; Here we've reached the end of the dump...
  (mt-write-eof)
  (mt-rewind)
;  (setq compare-end (dump-compare-files current-tape-start NIL))
;  (cond (compare-end
;        (ferror NIL "All files evidently didn't get dumped. (Bug in DUMP)")))
  (when this-host ; Mark as dumped, locally.
      (dolist (file dump-file-list)
        (set-file-attribute file t ':dumped))
      (save-directory-tree))
  (mt-offline))


;; Cuts a lot of unneccessary crap from COPY-FILE, losing generality.

(defun dump-one-file (file this-host)
  (let* ((truename (if this-host (file-truename file)
                                 (SEND file ':truename)))
         (plist (if this-host (lm-construct-plist file)
                              (cdr (file-properties file))))
         (author (second (memq ':AUTHOR plist)))
         (creation-date (second (memq ':CREATION-DATE plist))))
    (copy-file truename "MT:" ':default-byte-size (second (memq ':byte-size plist)))
    ;; If it's not a local dump, change the property now.
    (if (not this-host) (change-file-properties truename NIL ':NOT-BACKED-UP NIL))
    (append-tape-file-entry truename creation-date author
                            '(:NSC T))))  ;; Not Source Compared

(DEFUN REVERSE-PLIST (PLIST &AUX ANS)
  (TV:DOPLIST (PLIST VALUE IND)
    (SETQ ANS (CONS IND (CONS VALUE ANS))))
  ANS)


;build list in reverse order to eliminate gross paging every time.

(defun append-tape-file-entry (name creation-date author plist)
        (LET ((FOO (ncons (make-tape-file-entry entry-truename name
                                            entry-creation-date creation-date
                                            entry-author author
                                            entry-plist (copylist plist)))))
          (RPLACD (TAPE-FILE-TAIL-POINTER) FOO)
          (SETF (TAPE-FILE-TAIL-POINTER) FOO)))

; save until we're sure the above works
;(defun append-tape-file-entry (name creation-date author plist)
;  (setf (tape-file-entries)
;       (nconc (tape-file-entries)
;              (ncons (make-tape-file-entry entry-truename name
;                                           entry-creation-date creation-date
;                                           entry-author author
;                                           entry-plist (copylist plist))))))


;this guy isn't used by anyone

(defun find-file (filename)
  (let* ((pathname (fs:parse-pathname filename))
         (directory (SEND pathname ':DIRECTORY))
         (name (SEND pathname ':NAME))
         (type (SEND pathname ':TYPE))
         (version (SEND pathname ':VERSION)))
    (LOOP FOR dir IN (dc-ROOT-directorY)
          AS directory-name = (directory-name dir)
          AS printed-flag = NIL
          WHEN (or (not (stringp directory))
                   (string-search directory directory-name))
          DO
          (LOOP FOR file IN (read-directory-files dir)
                WHEN (or (not (stringp name))
                         (string-search name (file-name file)))
                WHEN (or (not (stringp type))
                         (string-search type (file-type file)))
                WHEN (or (not (numberp version))
                         (= version (file-version file)))
                DO
                (if (null printed-flag)
                    (progn
                      (setq printed-flag T)
                      (format T "~2%~A;" directory-name)))
                (format T "~%~A.~A#~A"
                        (file-name file)(file-type file)(file-version file))))))


(defun find-multiple-files ()
  (LOOP FOR dir IN (dc-ROOT-directorY)
        AS directory-name = (directory-name dir)
        AS printed-flag = NIL
        DO
        (print-directory-multiple-versions dir)))

(defun find-multiple-files-in-dir (directory)
  (if (not (typep directory 'FS:DIRECTORY))
      (setq directory (lookup-directory directory)))
  (LOOP FOR file IN (read-directory-files directory)
        WITH directory-results = NIL
        AS file-entry = (assoc (file-name file) directory-results)
        AS type-entry = (assoc (file-type file) (cdr file-entry))
        DO
        (cond ((and (null file-entry)
                    (null type-entry))
               (push (list (file-name file)
                           (list (file-type file) file))
                     directory-results))
              ((null type-entry)
               (push (list (file-type file) file)
                     (cdr file-entry)))
              (T (push file (cdr type-entry))))
        FINALLY
        (return directory-results)))

(defvar type-versions
        '(("LISP" . 2)
          ("TEXT" . 2)
          ("INIT" . 2)
          ("RMAIL" . 2)
          ("QFASL" . 1)
          ("DRAW" . 2)))

(defun print-directory-multiple-versions (directory)
  (format T "~&~%Directory ~A~2%"
          (if (typep directory ':STRING) directory
            (directory-name directory)))
  (LOOP FOR (filename . types) IN (nreverse (find-multiple-files-in-dir directory))
        DO
        (LOOP FOR (type . files) IN types
              AS versions = (cdr (assoc type type-versions))
              WHEN (or (and versions
                            (> (length files) versions))
                       (and (null versions)
                            (> (length files) 1)))
              DO
              (LOOP FOR f IN files
                    DO (format T "~%~A;~A.~A#~A"
                               (directory-name (file-directory f))
                               (file-name f)
                               (file-type f)
                               (file-version f))))))



(defun find-directory-files (directory-name)
  (let* ((directory-pathname
           (fs:parse-pathname (format NIL "~A;*.*" directory-name)))
         (directory-file-list (SEND directory-pathname ':DIRECTORY-LIST '(:SORTED))))
    (LOOP FOR file-entry IN (cdr directory-file-list)
          WITH result = NIL
          AS pathname = (first file-entry)
          AS name = (SEND pathname ':NAME)
          AS type = (SEND pathname ':TYPE)
          AS string = (fs:make-pathname ':DIRECTORY directory-name ':NAME name ':TYPE type)
          AS search = (assoc string result)
          UNLESS search
          DO (push (list string file-entry) result)
          ELSE
          DO (setf (second search) file-entry)
          FINALLY (return result))))

(defun find-all-single-files (&OPTIONAL (dirs *))
  (LOOP FOR dir IN dirs
        APPEND (find-directory-files dir)))

;;;??? :PLISTs will disagree some, I fear.
(DEFUN DUMP-COMPARE-FILES (FILE-LIST ;FILES-NOT-DUMPED
                           &AUX PATH COMPARE-END)
  (*CATCH 'DUMP-COMPARE-FILES
    (DO ((F FILE-LIST (CDR F))
         (E (TAPE-FILE-ENTRIES) (CDR E)))
        ((NULL F))
     (WITH-OPEN-FILE (FILE-STREAM (CAR F)
                                  ':CHARACTERS ':DEFAULT
                                  ':BYTE-SIZE ':DEFAULT
                                  ':PDP10-FORMAT ':DEFAULT)
       (SETQ PATH (SEND (CAR F) ':TRUENAME))
       (WITH-OPEN-FILE (MT-STREAM "MT: *"
                                  ':CHARACTERS (SEND FILE-STREAM ':CHARACTERS)
                                  ':BYTE-SIZE (SEND FILE-STREAM ':BYTE-SIZE))
         (IF (ERRORP MT-STREAM)
             (PROGN (SETQ COMPARE-END F)
                    (*THROW 'DUMP-COMPARE-FILES NIL))
            (LET ((CD-CORRECT (EQUAL (SEND MT-STREAM ':CREATION-DATE)
                                     (SEND FILE-STREAM ':CREATION-DATE)))
                  (AUTHOR-CORRECT (EQUAL (SEND MT-STREAM ':AUTHOR)
                                         (SEND FILE-STREAM ':AUTHOR)))
                  (PLIST-CORRECT (EQUAL (SEND MT-STREAM ':PLIST)
                                        (SEND FILE-STREAM ':PLIST))))
              (COND ((AND CD-CORRECT AUTHOR-CORRECT PLIST-CORRECT
                          (DUMP-SOURCE-COMPARE-STREAMS MT-STREAM FILE-STREAM))
                     (OR (EQUAL PATH (ENTRY-TRUENAME (CAR E)))
                         (FERROR NIL "Shouldn't happen."))
                     (REMPROP (LOCF (ENTRY-PLIST (CAR E))) ':NOT-SOURCE-COMPARED)
                     (SEND (CAR F) ':PUTPROP ':DUMPED T))
                    (T (FORMAT T "~% Compare error ~A: ~
                                    ~:[Creation date ~]~:[Author ~]~:[PLIST~]"
                               PATH CD-CORRECT AUTHOR-CORRECT PLIST-CORRECT)
                       (COND ((NULL PLIST-CORRECT)
                              (FORMAT T "~%  Magtape PLIST ~S, Disk PLIST ~S"
                                      (SEND MT-STREAM ':PLIST)
                                      (SEND FILE-STREAM ':PLIST))))))))))))
  COMPARE-END)

(DEFUN DUMP-SOURCE-COMPARE-STREAMS (S1 S2 &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
  (PROG (BUF1 IDX1 CNT1 BUF2 IDX2 CNT2 CC (BUF-START 0))
     L  (MULTIPLE-VALUE (BUF1 IDX1 CNT1)
          (SEND S1 ':GET-INPUT-BUFFER))
        (MULTIPLE-VALUE (BUF2 IDX2 CNT2)
          (SEND S2 ':GET-INPUT-BUFFER))
        (COND ((NOT (AND BUF1 BUF2))
               (IF BUF1 (FORMAT T "~%Stream 1 longer"))
               (IF BUF2 (FORMAT T "~%Stream 2 longer"))
               (RETURN (NOT (OR BUF1 BUF2)))))
        (SETQ CC (MIN CNT1 CNT2))
        (COND ((NULL (%STRING-EQUAL BUF1 IDX1 BUF2 IDX2 CC))
               (FORMAT T "~%Compare fails in buffer starting at ~D" BUF-START)
               (RETURN NIL)))
        (SEND S1 ':ADVANCE-INPUT-BUFFER (+ IDX1 CC))
        (SEND S2 ':ADVANCE-INPUT-BUFFER (+ IDX2 CC))
        (SETQ BUF-START (+ BUF-START CC))
        (GO L)))

(DEFUNP DUMP-GET-NEW-TAPE (&OPTIONAL OLD-FULL-P &AUX TAPE-NAME TAPE-FILE-NAME PROBE)
  (PUTPROP (LOCF (TAPE-FILE-PROPERTIES)) (NOT OLD-FULL-P) 'TAPE-MAY-APPEND)
  (WRITE-TAPE-FILE)
  (BEEP)
  (FORMAT T "~&>>> Time to mount a new tape <<<")
  FOO
  (FORMAT T "~%Continue ~A dump on tape: " DUMP-TYPE)
  (SETQ TAPE-NAME (READLINE))
  (SETQ TAPE-FILE-NAME (TAPE-FILE-NAME-FROM-TAPE-NAME (SEND (TAPE-FILE-NAME) ':HOST)
                                                      TAPE-NAME))
  (SETQ PROBE (OPEN TAPE-FILE-NAME '(:PROBE)))
  (COND ((NOT (ERRORP PROBE))
         (FORMAT T "~&Tape ~A has been used.  Are you *SURE* you want to overwrite it? ")
         (OR (Y-OR-N-P) (GO FOO))))
  (MT-REWIND)
  (SETUP-TAPE-FILE TAPE-NAME TAPE-FILE-NAME T)
  (FORMAT T "~&Continuing ~A dump on tape ~A ..."
          DUMP-TYPE TAPE-NAME))


;; RESTORE
(DEFVAR RESTORE-PATHNAMES)
(DEFVAR RESTORE-PATHNAME-TAPENAME-ALIST)
(DEFUN RESTORE-FILES (&REST PATHNAMES)
  (LET ((RESTORE-PATHNAMES (MAPCAR 'MERGE-PATHNAME-DEFAULTS PATHNAMES)))
    (RESTORE-1)))

(DEFUN RESTORE ()
  (LET ((RESTORE-PATHNAMES (LOOP FOR PATH = (FORMAT T "~& FILE= ") (READLINE)
                                 WHEN (EQUAL PATH "") RETURN PATHLIST
                                 COLLECT (MERGE-PATHNAME-DEFAULTS PATH) INTO PATHLIST)))
    (RESTORE-1)))

(DEFUN RESTORE-1 (&AUX MAGTAPE-DIRECTORY RESTORE-PATHNAME-TAPENAME-ALIST TAPE-NAME TAPE-FILE
                  TAPE-SHOWN TEM HOST)
  (SETQ HOST (SEND (CAR RESTORE-PATHNAMES) ':HOST))
  (FORMAT T "~%(Type Abort to stop searching for files)")
  (SETQ MAGTAPE-DIRECTORY
        (MAPCAR 'CAR
                (SORT (DIRECTORY-LIST (SEND (DEFAULT-PATHNAME NIL HOST)
                                               ':NEW-PATHNAME ':DIRECTORY "MAGTAPE"
                                               ':NAME ':WILD ':TYPE ':WILD))
                      #'(LAMBDA (X Y)
                          (NOT (< (GET X ':CREATION-DATE) (GET Y ':CREATION-DATE)))))))
  (ERROR-RESTART-LOOP ((SYS:ABORT ERROR) "Continue restoring files to ~A" HOST)
    (DOLIST (TAPE-FILE-NAME MAGTAPE-DIRECTORY)
      (SETQ TAPE-NAME (TAPE-NAME-FROM-TAPE-FILE-NAME TAPE-FILE-NAME))
      (SETUP-TAPE-FILE TAPE-NAME)
      (SETQ TAPE-SHOWN NIL)
      (DOLIST (ENTRY (TAPE-FILE-ENTRIES))
        (COND ((RESTORE-PATHNAMES-MATCH (ENTRY-TRUENAME ENTRY))
               (COND ((NULL TAPE-SHOWN)
                      (FORMAT T "~%~%Tape ~A:" (TAPE-NAME))
                      (SETQ TAPE-SHOWN (LIST* (TAPE-NAME) TAPE-FILE NIL))))
               (LIST-ONE-ENTRY ENTRY)
               (RPLACD (LAST TAPE-SHOWN) (NCONS ENTRY)))))
      (AND TAPE-SHOWN (PUSH TAPE-SHOWN RESTORE-PATHNAME-TAPENAME-ALIST))
      ;; Allow abort to throw now.
      (AND (SETQ TEM (SEND STANDARD-INPUT ':TYI-NO-HANG))
           (SEND STANDARD-INPUT ':UNTYI TEM))))
  (RESTORE-2))

(DEFUN RESTORE-PATHNAMES-MATCH (PATHNAME)
  (DOLIST (P RESTORE-PATHNAMES)
    (RESTORE-PATHNAME-MATCH PATHNAME P)))

;; P2 shouldn't contain wildcards...
(DEFUN RESTORE-PATHNAME-MATCH (P1 P2 &AUX T1 T2)
  (AND (OR (EQUAL (SETQ T1 (PATHNAME-DIRECTORY P1))
                  (SETQ T2 (PATHNAME-DIRECTORY P2)))
           (AND (STRINGP T1)
                (STRINGP T2)
                (STRING-SEARCH-CHAR #\* T1)
                (WILD-STRING-MATCH T1 T2)))
       (OR (EQUAL (SETQ T1 (PATHNAME-NAME P1))
                  (SETQ T2 (PATHNAME-NAME P2)))
           (EQ T1 ':WILD)
           (AND (STRINGP T1)
                (STRINGP T2)
                (STRING-SEARCH-CHAR #\* T1)
                (WILD-STRING-MATCH T1 T2)))
       (OR (EQUAL (SETQ T1 (PATHNAME-TYPE P1))
                  (SETQ T2 (PATHNAME-TYPE P2)))
           (EQ T1 ':WILD)
           (AND (STRINGP T1)
                (STRINGP T2)
                (STRING-SEARCH-CHAR #\* T1)
                (WILD-STRING-MATCH T1 T2)))
       (OR (SYMBOLP (SETQ T1 (PATHNAME-VERSION P1)))
           (EQUAL T1 (PATHNAME-VERSION P2)))))

(DEFUN RESTORE-2 (&AUX TAPE-NAME TEM)
  (DO () (NIL)
    TOP
    (FORMAT T "~% Enter Tape name to restore or [Return]: ")
    (AND (EQUAL (SETQ TAPE-NAME (READLINE)) "") (RETURN NIL))
    (COND ((SETQ TEM (ASSOC TAPE-NAME RESTORE-PATHNAME-TAPENAME-ALIST))
           (SETQ TAPE-FILE (SECOND TEM))
           (FORMAT T "~% Files to be restored on ~A:")
           (MAPC 'LIST-ONE-ENTRY (CDDR TEM))
           (SELECTQ (RESTORE-TAPE-OPTION)
             (ALL (MAPC 'RESTORE-TAPE-ENTRIES (CDDR TEM)))
             (SELECTIVE (MAPC 'RESTORE-TAPE-ENTRIES
                              (SELECT-FILES-TO-RESTORE (CDDR TEM))))))
          (T (SEND T "~% No files were found on ~A.")))))

(DEFUN RESTORE-TAPE-OPTION ()
  ;; Returns NIL if user types abort.
  (CONDITION-CASE ()
      (FQUERY '(:CHOICES (((ALL "All.") #/A)
                          ((SELECTIVE "Selective.") #/S)))
              "Restore all these or be selective? ")
    (SYS:ABORT ())))

(DEFUN RESTORE-TAPE-ENTRIES (ENTRIES &AUX FILE-POS SKIP)
  (MT-REWIND)
  (DO ((E ENTRIES (CDR E))
       (POS 0))
      ((NULL E))
    (OR (SETQ FILE-POS (FIND-POSITION-IN-LIST (CAR E) (TAPE-FILE-ENTRIES)))
        (FERROR NIL "Tape entry not found in tape file on second pass."))
    (AND (MINUSP (SETQ SKIP (- POS FILE-POS)))
         (FERROR NIL "Tape entries out of order on second pass."))
    (MT-SPACE-TO-EOF 0 SKIP)
    (RESTORE-FILE-FROM-TAPE (CAR E))
    (SETQ POS (1+ FILE-POS))))

(DEFUN RESTORE-FILE-FROM-TAPE (ENTRY &AUX PROBE TRUENAME AUTHOR CREATION-DATE FLAVOR
                                     BYTE-SIZE CHARACTERS HOST)
  (SETQ PROBE (OPEN "MT: *" '(:PROBE)))
  (SETQ TRUENAME (SEND PROBE ':TRUENAME)
        AUTHOR (SEND PROBE ':AUTHOR)
        CREATION-DATE (SEND PROBE ':CREATION-DATE)
        CHARACTERS (SEND PROBE ':CHARACTERS)
        BYTE-SIZE (SEND PROBE ':BYTE-SIZE)
        FLAVOR (SEND PROBE ':GET ':FLAVOR))
  (OR (AND (EQ TRUENAME (ENTRY-TRUENAME ENTRY))
           (EQUAL AUTHOR (ENTRY-AUTHOR ENTRY))
           (EQUAL CREATION-DATE (ENTRY-CREATION-DATE ENTRY)))
      (FERROR NIL "Incorrect file found on tape."))
  (WITH-OPEN-FILE (I "MT: *#0" ':CHARACTERS CHARACTERS
                     ':BYTE-SIZE BYTE-SIZE)
    (WITH-OPEN-FILE (O (SEND TRUENAME ':NEW-PATHNAME ':HOST HOST ':DEVICE "DSK")
                       ':CHARACTERS CHARACTERS ':BYTE-SIZE BYTE-SIZE
                       ':DIRECTION ':OUTPUT ':FLAVOR FLAVOR)
      (STREAM-COPY-UNTIL-EOF I O NIL)
      (SEND O ':SET-AUTHOR AUTHOR)
      (SEND O ':SET-CREATION-DATE CREATION-DATE)
      (LEXPR-SEND O ':CHANGE-PROPERTIES NIL (SEND I ':PLIST))
      (SEND O ':CHANGE-PROPERTIES NIL
               ':DUMPED (NOT (GET (LOCF (ENTRY-PLIST ENTRY)) ':NOT-SOURCE-COMPARED))))))

(DEFUN SELECT-FILES-TO-RESTORE (ENTRIES)
; (get (locf hdr) 'truename)
  (LOOP FOR ENTRY IN ENTRIES
        DOING (LIST-ONE-ENTRY ENTRY) (FORMAT T " Restore? ")
        WHEN (Y-OR-N-P) COLLECT ENTRY))

;;; -*- Mode: Lisp; Base: 8; Package: File-System -*-

;; New file copying stuff.
;;
;; Entrypoints:
;; QUEUEING-FILE-COPIES &rest forms [Special form]
;;  evaluates forms in order, delaying any copying until exit.
;;  This should be wrapped around any set of file operations which goes together.
;; FS-COPY-FILE from to &rest options
;; COPY-FILES files to &rest options
;; COPY-DIRECTORY from to &rest options
;; COPY-DIRECTORIES directories to &rest options
;; COPY-SOURCE-FILES-OF-SYSTEM system to &rest options
;; COPY-PATCH-FILES patchable-system to &rest options
;; COPY-INSTALLED-FILES to &rest options

;; The big problem here is that PDP-10 file servers don't know what byte-size their files
;; are suposed to be, and it makes a difference when opening them.  So the hairiest
;; part here is trying to determine this.

;; These types are ASCII files if all else fails.
(DEFVAR CHARACTER-FILE-TYPES '("LISP" "LSP" "TEXT" "TXT" "INIT" "INI" "CMD"
                               "MID" "AST" "LOGIN" "LISPM" "(PDIR)" "DOC" "WLR" "XGP"
                               "-THIS-" "ULOAD" "PROM" "WORDAB" "MAIL" "OMAIL" "RMAIL"))
;; These types are binary (byte-size = 16) if all else fails.
(DEFVAR BINARY-FILE-TYPES '("PRESS"))
;; These types are pdp10 (byte-size = 36) if all else fails.
(DEFVAR PDP10-FILE-TYPES '("KST" "LREC" "FASL" "BIN"))

;;  This variable doesn't seem to be used.
(DEFVAR *DONT-COPY-FILE-TYPES* '("XGP" "FASL" "KST" "LREC" "PRESS" "MAIL" "OMAIL" "RMAIL"))

;; FS-COPY-FILE file destination &rest options
;; This is the basic entry function which all other programs use.
;; <file> is a pathname, string representing one, or a stream.
;;   It may not contain wildcard characters.  If a stream,
;;   as of now it must handle :NAME, :TYPE, :VERSION, :CREATION-DATE, :QFASLP
;;   :BYTE-SIZE and :CHARACTERS messages even if the output file is fully specified.
;; <destination> is either
;;   another pathname, which is "merged" (by special rules) with the first,
;;   a string containing a colon is interpreted as another pathname,
;;   any other string oe symbol is interpreted as a host name.  This is what's normally
;;   done.  Note that "MT" specifies that you should copy the file onto magtape.
;; options:
;;   :DIRECTORY-LIST <list> is the list FOR THIS FILE handed back from :DIRECTORY-LIST.
;;      This speeds things up quite a bit, because it gives vital information about
;;      byte size and creation date.  If this is present, the pathname given
;;      is assumed to be the truename.
;;   :OUTPUT-DIRECTORY-LIST <list> is the entire directory list for the directory
;;      the copy is going to.  Do not specify this without :DIRECTORY-LIST.
;;      If this is specified, there is no need to probe the output file.
;;   :DEFAULT-BYTE-SIZE <n> Use this bytesize if all else fails
;;   :OVERWRITE <flag>      Copy even if the same file already exists.  Default is NIL.
;;   :VERBOSE <flag>        Print each copy operation and result.  Default is T.
;;   :DELETE <flag>         Delete <file> after copying it to destination.
;;   :AFTER <univ-time>     Only copy the file if it was created after the time specified.
;; Other options are ignored, and are presumably used elsewhere...


;; Returns T if the file was copied normally,
;; :ALREADY-EXISTS if the file wasn't copied because it wasn't necessary,
;; :AFTER if the file wasn't copied due to an :AFTER spec.
;; or an error string returned by an open operation,

(DEFVAR COPY-LOSSES ())
(DEFVAR COPY-OVERS ())

;renamed from COPY-FILE to avoid conflict with system function.
(DEFUN FS-COPY-FILE (FROM TO &KEY &OPTIONAL OVERWRITE (VERBOSE T) DELETE AFTER
                  DIRECTORY-LIST DEFAULT-BYTE-SIZE
                  OUTPUT-DIRECTORY-LIST (CREATE-DIRECTORY T)
                  &ALLOW-OTHER-KEYS &AUX
                  TRUE-BYTE-SIZE KNOWN-BYTE-SIZE TRUE-CHARACTERS
                  TRUENAME OUTNAME OUTNAME-UNCERTAIN
                  TYPE QFASLP INSTREAM OUTSTREAM (ABORT-FLAG ':ABORT)
                  FROM-IS-STREAM-P TO-DEFAULTS-FROM-STREAM AUTHOR TEM)
  (*CATCH 'COPY-FILE
    (UNWIND-PROTECT
      (PROG ()
            (COND ((STRINGP FROM))
                  ((TYPEP FROM 'PATHNAME))
                  ((SI:IO-STREAM-P FROM)
                   (SETQ FROM-IS-STREAM-P T)
                   (SETQ INSTREAM FROM)))

            (IF (NULL FROM-IS-STREAM-P)
                (SETQ FROM (MERGE-PATHNAME-DEFAULTS FROM NIL ':UNSPECIFIC ':UNSPECIFIC)))

            ;; If possible, get the byte size from the directory info.
            (COND (DIRECTORY-LIST
                   (IF (NULL FROM-IS-STREAM-P) (SETQ TRUENAME FROM))
                   (SETF (VALUES OUTNAME OUTNAME-UNCERTAIN TO-DEFAULTS-FROM-STREAM)
                         (DETERMINE-COPY-DESTINATION TO TRUENAME NIL INSTREAM))
                   ;; Punt now if :AFTER specification is not met.
                   (AND AFTER
                        ( (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
                            AFTER)
                        (RETURN ':AFTER))
                   ;; Verbosify after calling DETERMINE-COPY-DESTINATION.
                   (IF VERBOSE (FORMAT T "~%~A~23T ~A~50T"
                                       (IF FROM-IS-STREAM-P "" TRUENAME) OUTNAME))
                   ;; If we are sure we know the destination name,
                   ;; and we have an output directory list, check now
                   ;; in case we don't need to copy.
                   (OR TO-DEFAULTS-FROM-STREAM
                       OUTNAME-UNCERTAIN
                       (AND OUTPUT-DIRECTORY-LIST
                            (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
                                            OVERWRITE OUTPUT-DIRECTORY-LIST
                                            OUTNAME TRUENAME VERBOSE
                                            (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
                                                (FUNCALL INSTREAM ':CREATION-DATE)))))
                              (AND DESTEX (RETURN DESTEX)))))
                   (LET ((CHRLOC (LOCF (GET (LOCF DIRECTORY-LIST) ':CHARACTERS))))
                     (AND CHRLOC (SETQ TRUE-CHARACTERS (CDR CHRLOC))))
                   ;; Take :DIRECTORY-LIST information with a grain of salt...
                   ;; Note that we are assuming here that the files are used for LISPMs...
                   (LET ((POSSIBLE-BYTE-SIZE (GET (LOCF DIRECTORY-LIST) ':BYTE-SIZE)))
                     (AND POSSIBLE-BYTE-SIZE
                          (COND ((EQ POSSIBLE-BYTE-SIZE 7.)
                                 (SETQ TRUE-BYTE-SIZE 8.))
                                ((NEQ POSSIBLE-BYTE-SIZE 36.)
                                 (SETQ TRUE-BYTE-SIZE POSSIBLE-BYTE-SIZE)))))))

            ;; Next try opening the file.
            (COND ((NULL FROM-IS-STREAM-P)
                   (SETQ INSTREAM (OPEN FROM ':CHARACTERS (OR TRUE-CHARACTERS ':DEFAULT)
                                        ':BYTE-SIZE (OR TRUE-BYTE-SIZE ':DEFAULT)
                                        ':ERROR NIL))
                   (COND ((ERRORP INSTREAM)
                          (AND VERBOSE (FORMAT T "~%~A~50T~A" FROM INSTREAM))
                          (RETURN INSTREAM)))))

            ;; Punt now if :AFTER specification is not met.
            (AND AFTER
                 ( (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
                        (FUNCALL INSTREAM ':CREATION-DATE)
                        (FERROR NIL "Bletch!!"))
                    AFTER)
                 (RETURN ':AFTER))

            (IF (NULL FROM-IS-STREAM-P)
                (SETQ TRUENAME (FUNCALL INSTREAM ':TRUENAME)))
            (SETQ QFASLP (FUNCALL INSTREAM ':QFASLP))

            ;; Now determine the destination if not done already.
            (IF (OR (NULL OUTNAME) OUTNAME-UNCERTAIN)
                (PROGN
                  (MULTIPLE-VALUE (OUTNAME TEM TO-DEFAULTS-FROM-STREAM)
                    (DETERMINE-COPY-DESTINATION TO TRUENAME QFASLP INSTREAM))
                  (AND VERBOSE (FORMAT T "~%~A~23T ~A~50T" TRUENAME OUTNAME))))

            ;; Does the output file already exist?  Is its date the same?
            ;; Check now if we didn't check before.
            (AND (NULL TO-DEFAULTS-FROM-STREAM)
                 (OR OUTNAME-UNCERTAIN (NOT OUTPUT-DIRECTORY-LIST))
                 (LET ((DESTEX (COPY-DESTINATION-EXISTS-P
                                 OVERWRITE OUTPUT-DIRECTORY-LIST OUTNAME TRUENAME VERBOSE
                                 (OR (GET (LOCF DIRECTORY-LIST) ':CREATION-DATE)
                                     (FUNCALL INSTREAM ':CREATION-DATE)))))
                   (AND DESTEX (RETURN DESTEX))))

            ;; If we knew the byte size before opening the stream, remember that fact.
            (SETQ KNOWN-BYTE-SIZE TRUE-BYTE-SIZE)
            (SETQ TYPE (IF FROM-IS-STREAM-P
                           (FUNCALL INSTREAM ':TYPE)
                           (FUNCALL TRUENAME ':TYPE)))
            (OR TRUE-BYTE-SIZE
                ;; If stream knows its proper byte size, believe it.  QFILE streams don't.
                (AND (SETQ TRUE-BYTE-SIZE (FUNCALL INSTREAM ':SEND-IF-HANDLES ':BYTE-SIZE))
                     ;; If it knows that, it also did :characters :default properly.
                     (PROGN (SETQ TRUE-CHARACTERS (FUNCALL INSTREAM ':CHARACTERS)) T))
                ;; Otherwise guess.
                (SETQ TRUE-BYTE-SIZE
                      (COND (QFASLP 16.)
                            ((MEMBER TYPE BINARY-FILE-TYPES)
                             16.)
                            ((MEMBER TYPE PDP10-FILE-TYPES)
                             9)
                            ((FILE-EXTRACT-ATTRIBUTE-LIST INSTREAM)
                             8)
                            ((OR (MEMQ TYPE '(NIL :UNSPECIFIC))
                                 (MEMBER TYPE CHARACTER-FILE-TYPES))
                             8)
                            (DEFAULT-BYTE-SIZE)
                            ((Y-OR-N-P (FORMAT NIL "~%Is ~A a CHARACTER File? " TRUENAME))
                             8)
                            (T 16.))))
            (OR TRUE-CHARACTERS
                (SETQ TRUE-CHARACTERS (= TRUE-BYTE-SIZE 8)))
            (FORMAT T "~%Byte size ~D, Characters ~S" TRUE-BYTE-SIZE TRUE-CHARACTERS)

            ;; If stream is open in wrong byte size or with wrong :characters, reopen it.
            (OR FROM-IS-STREAM-P
                (AND
                  (OR KNOWN-BYTE-SIZE
                      (= TRUE-BYTE-SIZE
                         (OR (FUNCALL INSTREAM ':SEND-IF-HANDLES ':BYTE-SIZE)
                             (IF (FUNCALL INSTREAM ':CHARACTERS) 8 16.))))
                  (EQ TRUE-CHARACTERS (FUNCALL INSTREAM ':CHARACTERS)))
                (PROGN (PRINC " -- Must reopen stream")
                       (CLOSE INSTREAM)
                       (SETQ INSTREAM (OPEN TRUENAME ':ERROR NIL
                                           ':BYTE-SIZE TRUE-BYTE-SIZE
                                           ':CHARACTERS (= TRUE-BYTE-SIZE 8)))
                       (COND ((ERRORP INSTREAM)
                              (AND VERBOSE (FORMAT T "~%~A~50T~A" FROM INSTREAM))
                              (RETURN INSTREAM)))))

            (SETQ AUTHOR
                  (OR (GET (LOCF DIRECTORY-LIST) ':AUTHOR)
                      (FUNCALL INSTREAM ':GET ':AUTHOR)
                      (IF (NULL FROM-IS-STREAM-P)
                          (DETERMINE-FILE-AUTHOR (FUNCALL INSTREAM ':TRUENAME)))
                      "Unknown"))
         OPEN-OUTPUT
            ;; Do It.
            (CONDITION-CASE (ERROR)
                (SETQ OUTSTREAM
                      (COND (TO-DEFAULTS-FROM-STREAM
                             (FUNCALL OUTNAME ':OPEN OUTNAME
                                      ':DIRECTION ':OUTPUT
                                      ':ERROR NIL
                                      ':BYTE-SIZE TRUE-BYTE-SIZE
                                      ':DEFAULTS-FROM-STREAM INSTREAM
                                      ':AUTHOR AUTHOR))
                            (T
                             (OPEN OUTNAME
                                   (COND ((EQ TRUE-BYTE-SIZE 8.)
                                          '(:WRITE :NOERROR))
                                         ((EQ TRUE-BYTE-SIZE 16.)
                                          '(:WRITE :FIXNUM :NOERROR))
                                         (T `(:WRITE :NOERROR
                                                     :BYTE-SIZE ,TRUE-BYTE-SIZE)))))))
              (DIRECTORY-NOT-FOUND
               (AND CREATE-DIRECTORY
                    (NOT (ERRORP (CREATE-DIRECTORY OUTNAME ':ERROR NIL)))
                    (GO OPEN-OUTPUT))
                   (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT ERROR))
                   (RETURN OUTSTREAM))
              (ERROR
               (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT ERROR))
               (RETURN OUTSTREAM)))

            ;; This now hacks arbitrary property stuff...
            (IF TO-DEFAULTS-FROM-STREAM NIL
                (FUNCALL OUTSTREAM ':CHANGE-PROPERTIES NIL
                         ':AUTHOR AUTHOR
                         ':CREATION-DATE (FUNCALL INSTREAM ':GET ':CREATION-DATE))
                (LOOP WITH other-properties = (or directory-list
                                                  (funcall instream ':plist))
                      AS remove-properties = (funcall outstream ':plist)
                      THEN (cddr remove-properties)
                      WHILE (and remove-properties other-properties)
                      DO
                      (remprop (locf other-properties) (car remove-properties))
                      FINALLY
                      (dolist (p '(:directory :name :version :type))
                        (remprop (locf other-properties) p))
                      FINALLY
                      (cond (other-properties
                             (lexpr-funcall outstream ':CHANGE-PROPERTIES NIL
                                            other-properties)))))
            (STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM NIL)
            (SETQ ABORT-FLAG NIL))

      (OR (NULL OUTSTREAM) (ERRORP OUTSTREAM)
          (FUNCALL OUTSTREAM ':CLOSE ABORT-FLAG))
      (OR (NULL INSTREAM) (ERRORP INSTREAM)
          (PROGN (AND (NOT ABORT-FLAG)
                      DELETE
                      (FUNCALL INSTREAM ':SEND-IF-HANDLES ':DELETE NIL))
                 (FUNCALL INSTREAM ':CLOSE ABORT-FLAG))))))

;Three values: first: output-pathname or file-handle; second: T if output-pathname
;might change if QFASLP or INSTREAM were to change.  The third is non-NIL if
;the first value handles defaulting from streams.  In that case, the destination
;stream should be produced simply by sending the first value a :OPEN message
;with :DEFAULTS-FROM-STREAM <stream> keywords.
;In this latter case the file should be opened by directly calling the filehandle
;with an :OPEN message, giving the keywords :DEFAULTS-FROM-STREAM <stream>.
;TRUENAME may be null if source is simply a stream.  In that case it could
; try digging info out of the stream, but we wont worry about that for now.
(DEFUN DETERMINE-COPY-DESTINATION (TO TRUENAME QFASLP INSTREAM)
  (LET (TYPE VERSION PLIST TEM NOT-CERTAIN)
    (AND (SYMBOLP TO)
         (SETQ TO (GET-PNAME TO)))
    (COND ((TYPEP TO 'PATHNAME))
          ((CAR (ERRSET (GET-PATHNAME-HOST TO NIL NIL) NIL))
           (SETQ TO (MAKE-PATHNAME ':HOST TO ':DIRECTORY NIL ':NAME NIL)))
          (T (SETQ TO (PARSE-PATHNAME TO))))
    (COND ((NOT (TYPEP TO 'PATHNAME))
           (VALUES TO NIL 'DEFAULTS-FROM-STREAM))
          ((NULL TRUENAME)
           (LET ((NAME (FUNCALL TO ':NAME)))
             (COND ((NULL NAME)
                    (SETQ NAME (FUNCALL INSTREAM ':NAME))
                    (AND (LISTP NAME) (NULL (CDR NAME))
                         (SETQ NAME (CAR NAME)))))
             (COND ((MEMQ (SETQ TYPE (FUNCALL TO ':TYPE)) '(NIL :UNSPECIFIC))
                    (SETQ TYPE (FUNCALL INSTREAM ':TYPE))))
             (COND ((SYMBOLP (SETQ VERSION (FUNCALL TO ':VERSION)))
                    (SETQ VERSION (FUNCALL INSTREAM ':VERSION))))
             (VALUES
               (MAKE-PATHNAME ':HOST (FUNCALL TO ':HOST)
                              ':DEVICE (OR (FUNCALL TO ':DEVICE)
                                           (FUNCALL INSTREAM ':DEVICE))
                              ':DIRECTORY (OR (FUNCALL TO ':DIRECTORY)
                                              (FUNCALL INSTREAM ':DIRECTORY))
                              ':NAME NAME
                              ':TYPE TYPE
                              ':VERSION VERSION)
               NIL)))
          (T
           (OR (NOT (MEMQ (SETQ TYPE (FUNCALL TO ':TYPE)) '(NIL :UNSPECIFIC)))
               (NOT (MEMQ (SETQ TYPE (FUNCALL TRUENAME ':TYPE)) '(NIL :UNSPECIFIC)))
               ;; These do not distinguish types LISP and TEXT.
               (TYPEP TO 'ITS-PATHNAME)
               (AND (GET 'LOCAL-FILE-PATHNAME 'SI:FLAVOR)
                    (TYPEP TO 'LOCAL-FILE-PATHNAME))
               (AND (GET 'REMOTE-LMFILE-PATHNAME 'SI:FLAVOR)
                    (TYPEP TO 'REMOTE-LMFILE-PATHNAME))
               (SETQ NOT-CERTAIN T
                     TYPE
                     (OR (AND QFASLP "QFASL")
                         (AND INSTREAM
                              (SETQ PLIST (FILE-READ-ATTRIBUTE-LIST NIL INSTREAM))
                              (EQ (GET (LOCF PLIST) ':MODE) ':LISP)
                              "LISP")
                         "TEXT")))
           (OR (NOT (SYMBOLP (SETQ VERSION (FUNCALL TO ':VERSION))))
               (NOT (SYMBOLP (SETQ VERSION (FUNCALL TRUENAME ':VERSION))))
               (COND (QFASLP
                      (SETQ PLIST (SI:QFASL-STREAM-Property-List INSTREAM))
                      (FUNCALL INSTREAM ':SET-POINTER 0)
                      (COND ((SETQ TEM (GET (LOCF PLIST) ':QFASL-SOURCE-FILE-UNIQUE-ID))
                             (IF (LISTP TEM)
                                 (SETQ VERSION (CAR (LAST TEM)))
                               (SETQ VERSION (FUNCALL TEM ':VERSION))))
                            (T (SETQ VERSION ':NEWEST))))
                     (T (SETQ NOT-CERTAIN T VERSION ':NEWEST))))
           (LET ((INNAME (FUNCALL TRUENAME ':NAME)))
             (AND (LISTP INNAME) (NULL (CDR INNAME))
                  (SETQ INNAME (CAR INNAME)))
             (VALUES
               (MAKE-PATHNAME ':HOST (FUNCALL TO ':HOST)
                              ':DEVICE (OR (FUNCALL TO ':DEVICE)
                                           (FUNCALL TRUENAME ':DEVICE))
                              ':DIRECTORY (OR (FUNCALL TO ':DIRECTORY)
                                              (FUNCALL TRUENAME ':DIRECTORY))
                              ':NAME (OR (FUNCALL TO ':NAME)
                                         INNAME)
                              ':TYPE TYPE
                              ':VERSION VERSION)
               NOT-CERTAIN))))))

;Return T if we should not copy this file because an output file already exists.
;Also prints message if appropriate.
(DEFUN COPY-DESTINATION-EXISTS-P (OVERWRITE OUTPUT-DIRECTORY-LIST
                                  OUTNAME TRUENAME VERBOSE INDATE)
  (LET (OUTPROBE)
    (AND (NEQ OVERWRITE ':ALWAYS)
        (LET (OUTCRDATE OUTEX)
          ;; Take note of the fact that an LMFILE pathname with a "type"
          ;; won't be found as a truename because the truename will have a space.
          (AND (OR (AND (GET 'LOCAL-FILE-PATHNAME 'SI:FLAVOR)
                        (TYPEP OUTNAME 'LOCAL-FILE-PATHNAME))
                   (AND (GET 'REMOTE-LMFILE-PATHNAME 'SI:FLAVOR)
                        (TYPEP OUTNAME 'REMOTE-LMFILE-PATHNAME)))
               (FUNCALL OUTNAME ':TYPE)
               (SETQ OUTPUT-DIRECTORY-LIST NIL))
          (IF OUTPUT-DIRECTORY-LIST
              (PROGN (SETQ OUTEX (ASS #'(LAMBDA (X Y)
                                          (AND Y
                                               (STRING-EQUAL (FUNCALL X ':STRING-FOR-PRINTING)
                                                             (FUNCALL Y ':STRING-FOR-PRINTING))))
                                      OUTNAME OUTPUT-DIRECTORY-LIST))
                     (SETQ OUTCRDATE (GET (LOCF (CDR OUTEX)) ':CREATION-DATE)))
            (UNWIND-PROTECT
             (PROGN
              (SETQ OUTPROBE (OPEN OUTNAME '(:PROBE)))
              (COND ((NOT (ERRORP OUTPROBE))
                     (SETQ OUTCRDATE (FUNCALL OUTPROBE ':CREATION-DATE))
                     (SETQ OUTEX T))))
             (AND OUTPROBE (NOT (ERRORP OUTPROBE))
                  (CLOSE OUTPROBE))))
          (IF OUTEX
              (IF (= OUTCRDATE INDATE)
                  (COND ((NOT OVERWRITE)
                         (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT
                                               "[Already Exists]"))
                         ':ALREADY-EXISTS))
                  (COND ((NEQ (PATHNAME-VERSION OUTNAME) ':NEWEST)
                         (AND VERBOSE (FUNCALL STANDARD-OUTPUT ':STRING-OUT
                                               "[Different file exists at target]"))
                         (IF TRUENAME (PUSH TRUENAME COPY-LOSSES))
                         ':ALREADY-EXISTS)
                        (T (IF TRUENAME (PUSH TRUENAME COPY-OVERS))
                           NIL))))))))

;; This is a really gross kludge...
(DEFVAR *HUMUNGOUS-DIRECTORY-LIST* ())
(DEFUN DETERMINE-FILE-AUTHOR (PATHNAME &AUX ENTRY)
  (COND ((NULL (SETQ ENTRY (ASSQ PATHNAME *HUMUNGOUS-DIRECTORY-LIST*)))
         (SETQ *HUMUNGOUS-DIRECTORY-LIST*
               (NCONC (DIRECTORY-LIST (FUNCALL PATHNAME ':NEW-PATHNAME
                                               ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
                      *HUMUNGOUS-DIRECTORY-LIST*))
         (SETQ ENTRY (ASSQ PATHNAME *HUMUNGOUS-DIRECTORY-LIST*))))
  (GET ENTRY ':AUTHOR))

(DEFUN EQ-CAR (X Y)
  (EQ (CAR X) (CAR Y)))

;; All multiple-file copying stuff accepts these options in addition to the others.
;; :COPY-ONLY <list>, where <list> contains some of the following keywords:
;;     :SOURCE  -- copies source "LISP" and "TEXT" files.
;;     :NEWEST  -- copies only the newest versions, where possible...
;;     :QFASL   -- copies "QFASL" files.
;; :SELECTIVE <flag>, asks about each pathname.
;; :CREATE-DIRECTORY (<flag> T), create directory if possible...

(DEFSUBST PARSE-MULTIPLE-FILE-KEYWORDS ()
  (DO ((O OPTIONS (CDDR O)))
      ((NULL O))
    (SELECTQ (CAR O)
      (:COPY-ONLY (SETQ COPY-ONLY (CADR O)))
      (:SELECTIVE (SETQ SELECTIVE (CADR O)))
      (:CREATE-DIRECTORY (SETQ CREATE-DIRECTORY (CADR O))))))

(DEFUNP CHECK-COPY-ONLY (FILE COPY-ONLY &OPTIONAL DIR-LIST TEM VERSION TYPE)
  (IF (SYMBOLP COPY-ONLY) (SETQ COPY-ONLY (LIST COPY-ONLY)))
  (SETQ VERSION (FUNCALL FILE ':VERSION)
        TYPE (FUNCALL FILE ':TYPE))
  (COND ((MEMQ ':NEWEST COPY-ONLY)
         (AND (SYMBOLP VERSION)
              (NOT (MEMQ VERSION '(:NEWEST NIL :UNSPECIFIC)))
              (RETURN NIL))
         (IF DIR-LIST
             (OR (CHECK-FILE-NEWEST FILE VERSION DIR-LIST) (RETURN NIL))
             (AND (NOT (ERRORP (SETQ TEM (OPEN (FUNCALL FILE ':NEW-VERSION ':NEWEST)
                                                '(:PROBE)))))
                  (NOT (EQUAL VERSION
                              (FUNCALL (FUNCALL TEM ':TRUENAME) ':VERSION)))
                  (RETURN NIL)))))
  (OR (MEMQ ':SOURCE COPY-ONLY) (MEMQ ':QFASL COPY-ONLY)
      (RETURN T))
  (RETURN
    (COND ((MEMBER TYPE '(NIL :UNSPECIFIC "LISP" "TEXT"))
           (MEMQ ':SOURCE COPY-ONLY))
          ((EQUAL TYPE "QFASL")
           (MEMQ ':QFASL COPY-ONLY)))))

(DEFUN CHECK-FILE-NEWEST (FILE VERSION DIR-LIST &AUX NAME TYPE TEM)
  (SETQ NAME (FUNCALL FILE ':NAME)
        TYPE (FUNCALL FILE ':TYPE))
  (DO ((D DIR-LIST (CDR D)))
      ((NULL D) T)
    (AND (CAAR D)
         (EQUAL NAME (FUNCALL (CAAR D) ':NAME))
         (EQUAL TYPE (FUNCALL (CAAR D) ':TYPE))
         (NUMBERP (SETQ TEM (FUNCALL (CAAR D) ':VERSION)))
         (> TEM VERSION)
         (RETURN NIL))))                        ;Found a newer one.

(DEFUN COPY-FILES (FILES TO &REST OPTIONS &AUX COPY-ONLY SELECTIVE (CREATE-DIRECTORY T) TEM)
  (PARSE-MULTIPLE-FILE-KEYWORDS)
  (DOLIST (FILE FILES)
    RETRY
    (COND ((AND (OR (NOT SELECTIVE)
                    (PROGN (FORMAT QUERY-IO "Copy ~A ?" FILE)
                           (Y-OR-N-P)))
                (OR (NULL COPY-ONLY)
                    (CHECK-COPY-ONLY FILE COPY-ONLY)))
           (COND ((AND (ERRORP (SETQ TEM (LEXPR-FUNCALL #'FS-COPY-FILE FILE TO OPTIONS)))
                       (OR (STRING-EQUAL TEM " ERROR NSD" 0 0 12 12)
                           (STRING-EQUAL TEM " ERROR FNF" 0 0 12 12))
                       (EQUALP TO "LM"))
                  (LMFS-CREATE-DIRECTORY
                    (FUNCALL (FUNCALL (MERGE-PATHNAME-DEFAULTS FILE) ':TRANSLATED-PATHNAME)
                             ':DIRECTORY))
                  (GO RETRY)))))))

(DEFUN COPY-DIRECTORY (DIR TO
                       &OPTIONAL &REST OPTIONS
                       &KEY
                       COPY-ONLY
                       SELECTIVE
                       (RECOPY-FILE-ON-EOT T)
                       (ONLY-LATEST NIL)
                       (SINCE NIL)
                       (copy-subdirectories t)
                       &ALLOW-OTHER-KEYS
                       &AUX WHOLE-DIR-LIST OUTPUT-DIR-LIST ODIR TAPE-DUMP)
  (SETQ DIR (PARSE-PATHNAME DIR))
  (SETQ DIR (FUNCALL DIR ':NEW-PATHNAME
                     ':NAME (IF (MEMQ (FUNCALL DIR ':NAME) '(NIL :UNSPECIFIC))
                                ':WILD
                                (FUNCALL DIR ':NAME))
                     ':TYPE (IF (MEMQ (FUNCALL DIR ':TYPE) '(NIL :UNSPECIFIC))
                                ':WILD
                                (FUNCALL DIR ':TYPE))
                     ':VERSION (IF (MEMQ (FUNCALL DIR ':VERSION) '(NIL :UNSPECIFIC))
                                   ':WILD
                                   (FUNCALL DIR ':VERSION))))
  (SETQ ODIR (PARSE-PATHNAME TO))
  (COND ((TYPEP ODIR 'PATHNAME)
         (SETQ ODIR (FUNCALL ODIR ':NEW-PATHNAME
                      ':directory (if (memq (funcall odir ':directory)
                                            '(nil :unspecific))
                                      (funcall dir ':directory)
                                    (funcall odir ':directory))
                      ':NAME (IF (MEMQ (FUNCALL ODIR ':NAME) '(NIL :UNSPECIFIC))
                                 ':WILD
                               (FUNCALL ODIR ':NAME))
                      ':TYPE (IF (MEMQ (FUNCALL ODIR ':TYPE) '(NIL :UNSPECIFIC))
                                 ':WILD
                               (FUNCALL ODIR ':TYPE))
                      ':VERSION (IF (MEMQ (FUNCALL ODIR ':VERSION) '(NIL :UNSPECIFIC :NEWEST))
                                    ':WILD
                                  (FUNCALL ODIR ':VERSION)))))
        ((TYPEP ODIR 'MT-FILEHANDLE)
         (SETQ TAPE-DUMP T)))
  (SETQ WHOLE-DIR-LIST (DIRECTORY-LIST DIR))
  (IF ONLY-LATEST (SETQ WHOLE-DIR-LIST
                        (ELIMINATE-OLDER-FILES WHOLE-DIR-LIST)))
  (COND ((TYPEP ODIR 'PATHNAME)
         (ERRSET (SETQ OUTPUT-DIR-LIST (DIRECTORY-LIST ODIR))
                 NIL)))
  (DOLIST (F (IF SINCE (ELIMINATE-DATED-FILES (TIME:PARSE-UNIVERSAL-TIME SINCE)
                                              WHOLE-DIR-LIST) WHOLE-DIR-LIST))
    (AND (CAR F)
         (NOT (GET F ':LINK-TO))
         (OR (NOT SELECTIVE)
             (PROGN (FORMAT QUERY-IO "Copy ~A ?" (CAR F))
                    (Y-OR-N-P)))
         (OR (NULL COPY-ONLY)
             (CHECK-COPY-ONLY (CAR F) COPY-ONLY WHOLE-DIR-LIST))
         (cond ((get f ':directory)
                (cond (copy-subdirectories
                       (let ((d (funcall (car f) ':directory)))
                         (cond ((not (listp d)) (setq d (list d))))
                         (lexpr-funcall #'copy-directory
                                        (funcall (car f) ':new-pathname
                                                 ':directory (append d
                                                                     (list (funcall (car f)
                                                                                    ':name)))
                                                 ':name ':wild
                                                 ':type ':wild
                                                 ':version ':wild)
                                                ;to
;                                       (format t "[f = ~A] [d = ~A]" f d) (break t)
                                        (format nil "~a.~a;"
                                                (substring to 0 (string-search ";" to))
                                                (funcall (car f) ':name))
                                        options)))))
               (t

         (PROGV (IF RECOPY-FILE-ON-EOT '(*MT-EOT-HANDLER*) NIL)
                '(COPY-EOT-HANDLER)
           (DO ((V) (EOT))
               (())
             (MULTIPLE-VALUE (V EOT)
               (*CATCH (IF RECOPY-FILE-ON-EOT ':EOT ':NEVER)
                 (LEXPR-FUNCALL #'FS-COPY-FILE (CAR F) TO ':DIRECTORY-LIST (CDR F)
                                ':OUTPUT-DIRECTORY-LIST OUTPUT-DIR-LIST OPTIONS)))
             (IF (NULL EOT)
                 (RETURN)
                 (COPY-MOUNT-NEXT-TAPE TAPE-DUMP)))))))))

(DEFUN ELIMINATE-DATED-FILES (DATE DIRLIST)
  "Returns DIRLIST with only files created since DATE (Universal Time)."
  (DO* ((FILES (CDR DIRLIST) (CDR FILES))
       (FILE (CAR FILES) (CAR FILES))
       (RETURNABLES (LIST (CAR DIRLIST))))
       ((NULL FILES) RETURNABLES)
    (IF ( (CADR (MEMBER ':CREATION-DATE FILE)) DATE)
        (SETQ RETURNABLES (APPEND RETURNABLES (LIST FILE))))))

(DEFUN COPY-EOT-HANDLER (&REST IGNORE)
  (*THROW ':EOT NIL))

(DEFUN COPY-MOUNT-NEXT-TAPE (TAPE-DUMP)
  (IF TAPE-DUMP (MT-WRITE-EOF))
  (MT-REWIND)
  (MT-OFFLINE)
  (BEEP)
  (FORMAT T "~&>>> Time to mount a new tape <<<")
  (READLINE))

(DEFUN COPY-DIRS (DIRS FROM TO &REST OPTIONS)
  (DOLIST (DIR DIRS)
    (LEXPR-FUNCALL #'COPY-DIRECTORY (MAKE-PATHNAME ':HOST FROM ':DIRECTORY DIR) TO OPTIONS)))

(DEFUN COPY-ALL-FILES (FROM TO &REST OPTIONS)
  ;; KKLLUUDDGGEE
  (COND ((MEM 'EQUALP FROM '("MT" "MT:"))
         (LOOP UNTIL (ERRORP (LEXPR-FUNCALL #'FS-COPY-FILE "MT:*" TO OPTIONS))))
        (T (LOOP FOR (DIRECTORY) IN (FS:ALL-DIRECTORIES FROM)
                 DO (SETQ DIRECTORY (FUNCALL DIRECTORY ':NEW-PATHNAME
                                             ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
                    (LEXPR-FUNCALL #'COPY-DIRECTORY DIRECTORY TO OPTIONS)))))

(defun eliminate-older-files (dirlist &aux nu)
  "Returns DIRLIST with only the latest versions of the files."
  (do ((ells dirlist (cdr ells))
       el file other)
      ((null ells) (reverse nu))
    (setq el (first ells))
    (if (null (setq file (first el))) (push el nu)
      (setq other (mem
                    #'(lambda (p1 p2)
                        (and (setq p2 (car p2)) ; get pathname, ensure non-()
                             (equal (send p1 ':type) (send p2 ':type))
                             (equal (send p1 ':name) (send p2 ':name))))
                    file
                    nu))
      (if other
          (if (> (get el ':creation-date) (get (first other) ':creation-date))
              (setf (first other) el))
        (push el nu)))))


;; Found this exactly as it is... didn't have time to investigate why it
;; was left like this or how to best fix...
;;(defun same-ignoring-version-and-directory (p1 p2)
;;  (and (equal (send p1 ':type) (send p2 ':type))
;;       (equal (send p1 ':name) (send p2 ':name))

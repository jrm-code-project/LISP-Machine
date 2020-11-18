;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-
;;; Dump stuff (which will be obsolete as of February 1984) moved to SYS:FILE;ODUMP
;;;
;;; Entry functions which deal only with magtape
;;; MAGTAPE-LIST-FILES -- List files on a magtape.
;;; RESTORE-MAGTAPE -- Restore whatever's on the mounted magtape.
;;; MT-WRITE-FILES -- Write files to magtape.
;;; MT-WRITE-DIRECTORY -- Write an entire directory to magtape.
;;; MT-WRITE-PARTITION -- Write a partition onto a magtape.
;;;
;;; Other functions:
;;; PRINT-MAGTAPE -- Directly print contents of the magtape (for debugging).
;;;
;;; Moved to SYS:TAPE;PDP10
;;; LOAD-PDP10-DUMP-MAGTAPE -- Load a tape made on DM.
;;; MT-WRITE-PDP10-FILES -- Write files to be read at DM.
;;; VAX functions moved to SYS:TAPE;VAX

;;; If TRANSFORM is supplied, it should be a function of five arguments
;;; (host, directory, name, type, version) that returns a pathname that actually gets the file
;;; or a symbol with a FS:TAPE-RESTORE-TRANSFORM property that is such a function.
;;; The host argument is a host object.
(DEFUN RESTORE-MAGTAPE (&OPTIONAL &KEY (HOST SI:LOCAL-HOST) (QUERY T) DIRECTORIES
                        TAPE-OPTIONS COPY-OPTIONS FILES TRANSFORM
                        &AUX file last-time-p DIRECTORY NAME TYPE VERSION)
  (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
  (WHEN (AND TRANSFORM (NOT (FUNCTIONP TRANSFORM)))
    (SETQ TRANSFORM (GET TRANSFORM 'TAPE-RESTORE-TRANSFORM))
    (IF (NOT (FUNCTIONP TRANSFORM))
        (FERROR () "The transform supplied is bogus.")))
  (if files (setq files
                  (LOOP FOR file IN files
                        AS path = (parse-pathname file)
                        COLLECT (string-append
                                  (send path ':directory)
                                  ";" (send path ':name)))))
  (LOOP AS stream = (LEXPR-FUNCALL #'MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL
                                   TAPE-OPTIONS)
        UNTIL (errorp stream)
        WHEN (send stream ':GET ':PARTITION) ; If this is a partition, allow loading
        DO
        (format T "~%Tape partition ~S, length ~D.  Load ? "
                (send stream ':GET ':COMMENT) (send stream ':GET ':SIZE))
        (cond ((yes-or-no-p)
               (let ((disk-device
                       (if (yes-or-no-p
                             "Do you want to load the partition onto the local disk ? ")
                           0
                         (format T "~&Enter the machine name (i.e. cadr2): ")
                         (READLINE-TRIM))))
                 (format T "~&Copy into partition: ")
                 (mt-space-rev-to-bof)
                 (si:copy-disk-partition "MT" (send stream ':GET ':NAME)
                                         disk-device (readline)))))
        ELSE
        DO (cond (last-time-p (send stream ':CLOSE) (return t))
                 ((and directories
                       (not (member (send stream ':DIRECTORY) directories))))
                 ((and files
                       (cond ((member
                                (setq file
                                      (string-append
                                        (send stream ':directory)
                                        ";" (send stream ':name)))
                                files)
                              (setq files (delete file files))
                              (if (null files) (setq last-time-p T))
                              NIL)
                             (T T))))
                 ((if (PROGN
                        (SETQ DIRECTORY (SEND STREAM :DIRECTORY) NAME (SEND STREAM :NAME)
                              TYPE (SEND STREAM :TYPE) VERSION (SEND STREAM :VERSION))
                        QUERY)
                      (PROGN
                        (format T "~%~S;~S ~S #~D " DIRECTORY NAME TYPE VERSION)
                        (not (y-or-n-p "Restore ? ")))))
                 (T
                  (LEXPR-FUNCALL #'FS-COPY-FILE STREAM
                                 (IF TRANSFORM (FUNCALL TRANSFORM
                                                        HOST NAME DIRECTORY TYPE VERSION)
                                   HOST)
                                 copy-options)))
                 ;No good to do :NEXT-FILE because the :CLOSE operation does an :ADVANCE-FILE
        DO (send stream ':CLOSE)))

;;; Asssumes standard SYS:FOO; ==> <L.FOO> translations.
(DEFUN (:PROPERTY :STANDARD-SYS TAPE-RESTORE-TRANSFORM) (HOST DIRECTORY NAME TYPE VERSION)
  HOST ; ignored, using standard sys host
  (FS:MAKE-PATHNAME ':HOST (FS:GET-PATHNAME-HOST "SYS") ':DIRECTORY (CDR DIRECTORY)
                    ':NAME NAME ':TYPE TYPE ':VERSION VERSION))

(DEFUN FN-CONCATENATE (LIST)
  (PROG (NAME)
        (SETQ NAME (CAR LIST) LIST (CDR LIST))
     L  (COND ((NULL LIST) (RETURN NAME)))
        (SETQ NAME (STRING-APPEND NAME "-" (CAR LIST))
              LIST (CDR LIST))
        (GO L)))

;; Allow for partitions also.
(DEFUN MAGTAPE-LIST-FILES (&OPTIONAL (OUT-STREAM STANDARD-OUTPUT))
  (DO ((STREAM))
      ((ERRORP (SETQ STREAM (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL))))
    (IF (SEND STREAM ':GET ':PARTITION)
        (FORMAT OUT-STREAM "~%Partition ~A: ~S, Size ~D"
                (SEND STREAM ':GET ':NAME)
                (SEND STREAM ':GET ':COMMENT)
                (SEND STREAM ':GET ':SIZE))
        (FORMAT OUT-STREAM "~%~A; ~A ~A #~D~35TByte Size ~2D, Created "
                (SEND STREAM ':DIRECTORY)
                (SEND STREAM ':NAME)
                (SEND STREAM ':TYPE)
                (SEND STREAM ':VERSION)
                (SEND STREAM ':BYTE-SIZE))
        (TIME:PRINT-UNIVERSAL-TIME (SEND STREAM ':CREATION-DATE) OUT-STREAM)
;       (FORMAT T "~%PLIST: ~S" (SEND STREAM ':FILE-PLIST))
        )
    (SEND STREAM ':CLOSE)))

;; Tape creation functions which hack EOT.

(DEFUN MT-EOT-HANDLER (&REST IGNORE)
  (*THROW 'EOT NIL))

(DEFUN MT-WRITE-FILES (&REST FILES &AUX (*MT-EOT-HANDLER* 'MT-EOT-HANDLER))
  (LOOP FOR FILE IN FILES DOING
        (LOOP UNTIL (*CATCH 'EOT (COPY-FILE FILE "MT") T)
              DO (MT-SPACE-REV-TO-BOF)
                 (MT-WRITE-EOF)
                 (MT-REWIND)
                 (BEEP)
                 (FORMAT ERROR-OUTPUT "~&>>> MagTape reached end of tape <<<")
                 (MT-OFFLINE)
                 (LOOP DOING (FORMAT ERROR-OUTPUT "~%Type [Resume] when new tape is mounted:")
                       UNTIL (SEND STANDARD-INPUT ':CLEAR-INPUT)
                             (EQ (SEND STANDARD-INPUT ':TYI) #\RESUME)
                       DOING (BEEP)))
        FINALLY (MT-WRITE-EOF) (MT-SPACE-REV)))

(DEFUN MT-WRITE-DIRECTORIES (&OPTIONAL (HOST SI:LOCAL-HOST) &REST DIRECTORIES)
  (SETQ DIRECTORIES
        (LOOP FOR D IN (OR DIRECTORIES
                           (LOOP FOR D IN (ALL-DIRECTORIES HOST)
                                 COLLECTING (SEND (CAR D) ':DIRECTORY)))
              WHEN D COLLECT (MAKE-PATHNAME ':HOST HOST ':DIRECTORY D ':NAME NIL)))
  (LOOP FOR DIRECTORY IN DIRECTORIES
        DO (FORMAT T "~%Dumping ~A" DIRECTORY)
           (MT-WRITE-DIRECTORY DIRECTORY)
           (FORMAT T "~%~%")))

(DEFUN MT-WRITE-DIRECTORY (PATH &AUX (*MT-EOT-HANDLER* 'MT-EOT-HANDLER))
  (SETQ PATH (PARSE-PATHNAME PATH))
  (SETQ PATH (SEND PATH ':NEW-PATHNAME
                      ':NAME (OR (PATHNAME-NAME PATH) ':WILD)
                      ':TYPE (OR (PATHNAME-TYPE PATH) ':WILD)
                      ':VERSION (OR (PATHNAME-VERSION PATH) ':WILD)))
  (LOOP AS COUNT = 0 UNTIL
        (*CATCH 'EOT
          (LOOP FOR (FILE . DIR-LIST)
                IN (DIRECTORY-LIST PATH)
                WHEN (AND FILE (NOT (GET (LOCF DIR-LIST) ':LINK-TO)))
                DO (COPY-FILE FILE "MT" ':DIRECTORY-LIST DIR-LIST)
                   (INCF COUNT)
                FINALLY (RETURN T)))
        DO (LOOP REPEAT COUNT DO (MT-SPACE-REV 0))
           (MT-SPACE-REV-TO-BOF)
           (MT-WRITE-EOF)
           (MT-REWIND)
           (BEEP)
           (FORMAT ERROR-OUTPUT "~&>>> MagTape reached end of tape <<<")
           (MT-OFFLINE)
           (LOOP DOING (FORMAT ERROR-OUTPUT "~%Type [Resume] when new tape is mounted:")
                 UNTIL (SEND STANDARD-INPUT ':CLEAR-INPUT)
                       (EQ (SEND STANDARD-INPUT ':TYI) #\RESUME)
                 DOING (BEEP))
        FINALLY (MT-WRITE-EOF) (MT-SPACE-REV)))

(DEFUN MT-WRITE-PARTITION (PARTITION &OPTIONAL (UNIT 0))
  (SI:COPY-DISK-PARTITION UNIT PARTITION "MT" PARTITION)
  (MT-WRITE-EOF)
  (MT-SPACE-REV))


;; MagTape Band transfer handler.
(DECLARE (SPECIAL *BAND-WRITE* *BAND-PLIST* *BAND-STREAM*))

(DEFUN MAKE-BAND-MAGTAPE-HANDLER (*BAND-WRITE*)
  (LET ((*BAND-PLIST* `(:PARTITION T :BYTE-SIZE 20 :AUTHOR ,USER-ID))
        (*BAND-STREAM* NIL))
    (COND ((NULL *BAND-WRITE*)
           (SETQ *BAND-STREAM* (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':CHARACTERS NIL)
                 *BAND-PLIST* (SEND *BAND-STREAM* ':PLIST))))
    (CLOSURE '(*BAND-WRITE* *BAND-PLIST* *BAND-STREAM*)
             'BAND-MAGTAPE-HANDLER)))

(DEFSELECT (BAND-MAGTAPE-HANDLER IGNORE)
  (:READ (RQB BLOCK)
    BLOCK
    (SEND *BAND-STREAM* ':STRING-IN "unexpected EOF"
             (RQB-BUFFER RQB) 0 (* (RQB-NPAGES RQB) 1000)))
  (:WRITE (RQB BLOCK &AUX
           (N-BLOCKS (RQB-NPAGES RQB))
           (N-HWDS (* N-BLOCKS 1000))
           (BUF (RQB-BUFFER RQB)))
    BLOCK
    (OR *BAND-STREAM*
        (SETQ *BAND-STREAM*
              (MAKE-MT-FILE-STREAM ':DIRECTION ':OUTPUT
                                   ':PLIST *BAND-PLIST*
                                   ':CHARACTERS NIL)))
    (SEND *BAND-STREAM* ':STRING-OUT BUF 0 N-HWDS))
  (:DISPOSE ()
    (COND (*BAND-STREAM*
           (SEND *BAND-STREAM* ':CLOSE)
           (SETQ *BAND-STREAM* NIL))))
  (:HANDLES-LABEL () T)
  (:GET (IND) (GET (LOCF *BAND-PLIST*) IND))
  (:PUT (PROP IND) (PUTPROP (LOCF *BAND-PLIST*) PROP IND))
  (:FIND-DISK-PARTITION (NAME &AUX TEM)
    (IF (SETQ TEM (GET-FROM-ALTERNATING-LIST *BAND-PLIST* ':NAME))
        (IF (NOT (EQUALP NAME TEM))
            (IF (NULL (Y-OR-N-P (FORMAT NIL "~%Tape partition ~s, OK?" *BAND-PLIST*)))
                (BREAK "Now what do you do ?" T))) ; this should be changed...
        (PUTPROP (LOCF *BAND-PLIST*) NAME ':NAME))
    (VALUES  0
             (OR (GET (LOCF *BAND-PLIST*) ':SIZE) 3777777)
             NIL))
  (:PARTITION-COMMENT (IGNORE)
    (GET (LOCF *BAND-PLIST*) ':COMMENT)))


;; All stuff hereafter is untested kruft left over from the days of RG;MT.
;; Moved to SYS: TAPE; PDP10

(DEFUN PLIST-VIA-PATHNAME (FILE &AUX ANS)
  (IF (OR (STRINGP FILE)
          (TYPEP FILE 'PATHNAME))
      (SETQ FILE (OPEN FILE '(:PROBE))))
  (IF (STRINGP FILE)
      (FERROR NIL FILE)
      (LET ((INPATH (SEND FILE ':TRUENAME))
            (WO  (SEND FILE ':WHICH-OPERATIONS)))
        (LET ((DIRECTORY (SEND INPATH ':DIRECTORY))
              (NAME (SEND INPATH ':NAME))
              (TYPE (SEND INPATH ':TYPE))
              (VERSION (SEND INPATH ':VERSION)))
          (SETQ ANS `(:DIRECTORY ,DIRECTORY :NAME ,NAME :TYPE ,TYPE :VERSION ,VERSION))
          (LET ((BYTE-SIZE (COND ((MEMQ ':BYTE-SIZE WO) (SEND FILE ':BYTE-SIZE))
                                 ((EQUAL TYPE "QFASL") 16.)
                                 (T 8.)))
                (CREATION-DATE (IF (MEMQ ':CREATION-DATE WO)
                                   (SEND FILE ':CREATION-DATE)))
                (AUTHOR  (IF (MEMQ ':AUTHOR WO)
                             (SEND FILE ':AUTHOR))))
            (SETQ ANS (NCONC ANS `(:BYTE-SIZE ,BYTE-SIZE)))
            (IF CREATION-DATE (SETQ ANS (NCONC ANS `(:CREATION-DATE ,CREATION-DATE))))
            (IF AUTHOR (SETQ ANS (NCONC ANS `(:AUTHOR ,AUTHOR))))
            (IF (MEMQ ':PLIST WO)
                (TV:DOPLIST ((SEND FILE ':PLIST) VAL IND)
                  (IF (GET-FROM-ALTERNATING-LIST ANS IND)
                      NIL
                      (SETQ ANS (NCONC ANS (LIST IND VAL))))))
            ANS)))))

(DEFUN PLIST-FROM-PATHNAME (PATHNAME)
  `(:DIRECTORY ,(SEND PATHNAME ':DIRECTORY)
    :NAME ,(SEND PATHNAME ':NAME)
    :VERSION ,(SEND PATHNAME ':VERSION)
    :TYPE ,(SEND PATHNAME ':TYPE)))

(DEFUN PATHNAME-FROM-PLIST (HOST DIRECTORY PLIST)
  (IF (TYPEP PLIST 'PATHNAME)
      PLIST
      (MAKE-PATHNAME ':HOST HOST
                     ':DIRECTORY (IF DIRECTORY DIRECTORY
                                     (GET-FROM-ALTERNATING-LIST PLIST ':DIRECTORY))
                     ':NAME (GET-FROM-ALTERNATING-LIST PLIST ':NAME)
                     ':TYPE (GET-FROM-ALTERNATING-LIST PLIST ':TYPE)
                     ':VERSION (GET-FROM-ALTERNATING-LIST PLIST ':VERSION))))

(DEFUN ASCII-TO-LISPM (CH)
  (COND ((EQ CH 12) NIL)
        ((MEMQ CH '(10 11 14 15)) (+ CH 200))
        (T CH)))

(DEFUN LISPM-TO-ASCII (CH)
  (COND ((>= CH 200)
         (- CH 200))
        (T CH)))

;SPECIAL FUNCTION TO DUMP TAMI'S PICTURE DATA ARRAY IN IBM MODE.
(DEFUN DUMP-2D-BYTE-ARRAY-RAW (ARRAY)
  (LET ((XLIM (ARRAY-DIMENSION ARRAY 1))
        (YLIM (ARRAY-DIMENSION ARRAY 2)))
    (LET ((STREAM (MAKE-MT-STREAM ':DIRECTION ':OUTPUT
                                  ':IBM-MODE T ':RECORD-SIZE (+ 100 YLIM))))
      (DOTIMES (Y YLIM)
        (DOTIMES (X XLIM)
          (SEND STREAM ':TYO (AR-2 ARRAY X Y)))
        (SEND STREAM ':ADVANCE-OUTPUT-BUFFER))
      (SEND STREAM ':CLOSE))))

;; For Debugging.
(DEFUN PRINT-MAGTAPE (&REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (UNWIND-PROTECT
      (STREAM-COPY-UNTIL-EOF IS STANDARD-OUTPUT NIL)
      (FUNCALL IS ':CLOSE ':RAW)))              ;Avoid searching for EOF
                                                ;This should not normally be done.
  T)


(DEFUN COPY-MAGTAPE-FILE (FN &REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (WITH-OPEN-FILE (OS FN ':OUT)
      (UNWIND-PROTECT
        (STREAM-COPY-UNTIL-EOF IS OS NIL)
        (FUNCALL IS ':CLOSE ':RAW))))           ;Avoid searching for EOF
                                                ;This should not normally be done.
  T)

(DEFUN COPY-INSERTING-CR-EVERY-N (N FROM TO)
  (WITH-OPEN-FILE (OS TO ':OUT)
    (WITH-OPEN-FILE (IS FROM ':IN)
      (DO ((CH T))
          ((NULL CH))
        (DOTIMES (C N)
          (SETQ CH  (FUNCALL IS ':TYI))
          (IF CH (FUNCALL OS ':TYO CH)))
        (FUNCALL OS ':TYO #\CR)))))

(DEFUN PRINT-ASCII-MAGTAPE (&REST OPTIONS)
  (LET ((IS (LEXPR-FUNCALL #'MAKE-MT-STREAM OPTIONS)))
    (UNWIND-PROTECT
      (do ((ch (funcall is ':tyi) (funcall is ':tyi)))
          ((null ch))
        (setq ch (ascii-to-lispm ch))
        (if ch (funcall standard-output ':tyo ch)))
      (FUNCALL IS ':CLOSE ':RAW)))              ;Avoid searching for EOF
                                                ;This should not normally be done.
  T)

(DEFUN WRITE-ASCII-MAGTAPE (FN &REST OPTIONS)
  (LET ((IS (OPEN FN))
        (OS (LEXPR-FUNCALL #'MAKE-MT-STREAM ':DIRECTION ':OUTPUT OPTIONS)))
    (DO ((CH (FUNCALL IS ':TYI) (FUNCALL IS ':TYI)))
        ((NULL CH)
         (FUNCALL OS ':CLOSE)
         (FUNCALL IS ':CLOSE))
      (SETQ CH (LISPM-TO-ASCII CH))
      (FUNCALL OS ':TYO CH))))

(defun read-eof-file (&AUX in-stream)
  (setq in-stream (lexpr-funcall #'MAKE-MT-STREAM ':ERROR NIL))
  (funcall in-stream ':NEXT-FILE)
  (funcall in-stream ':CLOSE ':RAW))

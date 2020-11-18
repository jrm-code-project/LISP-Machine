;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

;;; This file contains stream definitions for the magtape.

;;; Entrypoints are:
;;; MAKE-MT-STREAM &rest options
;;; MAKE-MT-FILE-STREAM &rest options
;;; MAKE-MAGTAPE-FILE-PROBE-STREAM &rest options

;;; Lossages:
;;; PAGE-SIZE-IN-BITS

;; Get a buffer of the correct byte size
(DEFUN MT-GET-RQB-ARRAY (RQB BYTE-SIZE &AUX TYPE)
  (COND ((= BYTE-SIZE 20) (RQB-BUFFER RQB))
        ((= BYTE-SIZE 10) (RQB-8-BIT-BUFFER RQB))
        ((SETQ TYPE (CDR (ASSQ BYTE-SIZE '((4 . ART-4B) (2 . ART-2B) (1 . ART-1B)))))
         (MAKE-ARRAY (FLOOR (* (RQB-NPAGES RQB) (* PAGE-SIZE 32.)) BYTE-SIZE)
                     ':AREA LOCAL-FILE-SYSTEM-AREA
                     ':TYPE TYPE
                     ':DISPLACED-TO RQB
                     ;; This is a system bug.  Don't try to figure it out.
                     ':DISPLACED-INDEX-OFFSET (* (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)
                                                 (FLOOR 20 BYTE-SIZE))))
        (T (FERROR NIL "~D is an invalid byte size." BYTE-SIZE))))

(DEFFLAVOR MT-MIXIN
        ((STATUS ':OPEN)
         (UNIT 0)
         (RECORD-SIZE 10000)
         (DENSITY 0)
         (IBM-MODE NIL)
         (BYTE-SIZE 10)
         (RQB NIL))
        ()
  (:INCLUDED-FLAVORS SI:STREAM)
  (:GETTABLE-INSTANCE-VARIABLES BYTE-SIZE UNIT)
  (:INITABLE-INSTANCE-VARIABLES UNIT RECORD-SIZE DENSITY IBM-MODE BYTE-SIZE RQB))

(DEFMETHOD (MT-MIXIN :ADVANCE-FILE) () NIL)

(DEFMETHOD (MT-MIXIN :CLOSE) (&OPTIONAL ABORTP)
  (OR (EQ ABORTP ':RAW) (FUNCALL-SELF ':ADVANCE-FILE))
  (COND ((NEQ STATUS ':CLOSED)
         (SETQ STATUS ':CLOSED)
         (AND RQB (RETURN-DISK-RQB RQB))
         (SETQ RQB NIL)
         T)))

(DEFMETHOD (MT-MIXIN :RETURN-RQB) ()
  (COND (RQB (RETURN-DISK-RQB RQB)
             (SETQ RQB NIL))))

(DEFFLAVOR MT-INPUT-MIXIN
        ()
        (MT-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR MT-OUTPUT-MIXIN
        ()
        (MT-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-OUTPUT-STREAM))

(DEFMETHOD (MT-INPUT-MIXIN :NEXT-INPUT-BUFFER)
           (&OPTIONAL IGNORE &AUX BUFFER BYTES-TRANSFERRED)
  (COND ((EQ STATUS ':CLOSED)
         (FERROR NIL "Attempt to get input from ~S, which is closed." SELF))
        ((EQ STATUS ':EOF) NIL)
        (T (OR RQB
               (SETQ RQB (GET-DISK-RQB (QUOTIENT-CEILING RECORD-SIZE PAGE-SIZE-IN-BYTES))))
           (MT-RUN RQB %MT-COMMAND-READ (MINUS RECORD-SIZE) UNIT DENSITY)
           (COND ((MT-STATUS-EOF)
                  (SETQ STATUS ':EOF)
                  NIL)
                 (T (SETQ BYTES-TRANSFERRED
                          (- (SIGN-EXTEND-16 (AR-1 RQB %MT-BYTE-COUNT-AFTER))
                             (SIGN-EXTEND-16 (AR-1 RQB %MT-BYTE-COUNT))))
                    (SETQ BUFFER (MT-GET-RQB-ARRAY RQB BYTE-SIZE))
                    (COND ((BIT-TEST BYTES-TRANSFERRED 3)
                           (FORMAT T "~%~D BYTES, YOU LOST DUE TO UNIBUS MAP LOSSAGE!"
                                   BYTES-TRANSFERRED)
                           (PRINT-MT-RQB RQB)))
                    (SETQ BYTES-TRANSFERRED (LOGAND BYTES-TRANSFERRED (LOGNOT 3)))
                    ;; If this is a CHARACTERS file, trim the nulls from the end.
                    ;; These will only appear in the last record if the record size
                    ;; wasn't a multiple of 4.  This could possibly be fixed...
                    (COND ((FUNCALL-SELF ':CHARACTERS)
                           (DO () ((OR (ZEROP BYTES-TRANSFERRED)
                                       (NOT (ZEROP (AR-1 BUFFER (1- BYTES-TRANSFERRED))))))
                             (SETQ BYTES-TRANSFERRED (1- BYTES-TRANSFERRED)))))
                    (VALUES BUFFER 0 (FLOOR (* 8 BYTES-TRANSFERRED) BYTE-SIZE)))))))

(DEFMETHOD (MT-INPUT-MIXIN :DISCARD-INPUT-BUFFER) IGNORE)

(DEFMETHOD (MT-INPUT-MIXIN :NEXT-FILE) ()
  (IF (EQ STATUS ':EOF)
      (SETQ STATUS ':OPEN)
    (MT-SPACE-TO-EOF UNIT)
    (FUNCALL-SELF ':CLEAR-INPUT)))

(DEFMETHOD (MT-INPUT-MIXIN :ADVANCE-FILE) ()
  (COND ((NOT (MEMQ STATUS '(:EOF :CLOSED)))
         (MT-SPACE-TO-EOF UNIT)
         (SETQ STATUS ':EOF))))

(DEFMETHOD (MT-OUTPUT-MIXIN :NEW-OUTPUT-BUFFER) ()
  (OR RQB (SETQ RQB (GET-DISK-RQB (QUOTIENT-CEILING RECORD-SIZE PAGE-SIZE-IN-BYTES))))
  (VALUES (MT-GET-RQB-ARRAY RQB BYTE-SIZE) 0 (FLOOR (* RECORD-SIZE 8) BYTE-SIZE)))

(DEFMETHOD (MT-OUTPUT-MIXIN :SEND-OUTPUT-BUFFER) (BUFFER TO-INDEX)
  (OR (AND (ARRAY-INDIRECT-P BUFFER)
           (EQ (%P-CONTENTS-OFFSET BUFFER
                                   (1+ (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG BUFFER 0)))
               RQB))
      (FERROR NIL "Attempt to :SEND-OUTPUT-BUFFER ~S, which is not indirected into ~S"
              BUFFER RQB))
  (LET ((COUNT (FLOOR (* TO-INDEX BYTE-SIZE) 8)))
    ;; Always write an even number of 16 bit words to
    ;; Avoid unibus map buffer lossage.
    (COND ((BIT-TEST COUNT 1)
           (AS-1 0 BUFFER TO-INDEX)
           (INCF COUNT)
           (INCF TO-INDEX)))
    (COND ((BIT-TEST COUNT 3)
           (AS-1 0 BUFFER TO-INDEX)
           (AS-1 0 BUFFER (1+ TO-INDEX))
           (INCF COUNT 2)))
    (COND ((PLUSP COUNT)
           (MT-RUN RQB %MT-COMMAND-WRITE (MINUS COUNT) UNIT DENSITY IBM-MODE)))))

(DEFMETHOD (MT-OUTPUT-MIXIN :DISCARD-OUTPUT-BUFFER) IGNORE)

(DEFMETHOD (MT-OUTPUT-MIXIN :ADVANCE-FILE) ()
  (OR (EQ STATUS ':CLOSED)
      (MT-WRITE-EOF UNIT)))

(DEFFLAVOR MT-INPUT-STREAM
        ()
        (MT-INPUT-MIXIN SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR MT-OUTPUT-STREAM
        ()
        (MT-OUTPUT-MIXIN SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR MT-CHARACTER-INPUT-STREAM
        ()
        (MT-INPUT-MIXIN SI:BUFFERED-LINE-INPUT-STREAM))

(DEFFLAVOR MT-CHARACTER-OUTPUT-STREAM
        ()
        (MT-OUTPUT-MIXIN SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(COMPILE-FLAVOR-METHODS MT-INPUT-STREAM MT-OUTPUT-STREAM
                        MT-CHARACTER-INPUT-STREAM MT-CHARACTER-OUTPUT-STREAM)

(DEFVAR *DEFAULT-RECORD-SIZE* 10000 "The number of bytes for a magtape record.")
(DEFVAR *MT-EOT-HANDLER* () "Funcalled on EOT by SOME functions.
In most cases, you should use something like CONDITION-BIND.")

;; This function makes a simple stream to the magtape.
;; It can be used by programs which want to read arbitrarily formated magtapes.
(DEFUN MAKE-MT-STREAM (&OPTIONAL &KEY (DIRECTION ':INPUT) (BYTE-SIZE 8) (CHARACTERS T)
                       (UNIT 0) (RECORD-SIZE *DEFAULT-RECORD-SIZE*) (DENSITY 0) (IBM-MODE NIL)
                       &ALLOW-OTHER-KEYS)
  (CHECK-ARG DIRECTION (MEMQ DIRECTION '(:INPUT :OUTPUT)) ":INPUT or :OUTPUT")
  (CHECK-ARG-TYPE BYTE-SIZE :NUMBER)
  (CHECK-ARG CHARACTERS (MEMQ CHARACTERS '(T NIL)) "T or NIL")
  (MAKE-INSTANCE (IF (EQ DIRECTION ':INPUT)
                     (IF CHARACTERS
                         'MT-CHARACTER-INPUT-STREAM
                       'MT-INPUT-STREAM)
                   (IF CHARACTERS
                       'MT-CHARACTER-OUTPUT-STREAM
                     'MT-OUTPUT-STREAM))
                 ':BYTE-SIZE BYTE-SIZE
                 ':UNIT UNIT
                 ':RECORD-SIZE RECORD-SIZE
                 ':DENSITY DENSITY
                 ':IBM-MODE IBM-MODE))

;;; Magtapes with headers.

;;; The following function reads a block from the magtape and tries to
;;; determine the header plist.  If it reads an EOF, it returns NIL.
;;; The second value returned is the style of header.
;;; Currently known about are :MIT, :MIT, and :MIT.

;;; this code returns a pure PLIST and does not make any assumptions about
;;; name formats or anything.

(DEFUN READ-MAGTAPE-HEADER (STREAM &AUX BUF LOW COUNT PLIST)
  (UNWIND-PROTECT
    (PROGN (MULTIPLE-VALUE (BUF LOW COUNT)
             (FUNCALL STREAM ':GET-INPUT-BUFFER))
           (COND ((NULL BUF) NIL)                       ;EOF
                 ((AND ( COUNT 4)
                       (STRING-EQUAL BUF "LMFL" LOW 0 (+ LOW 4) 4))
                  ;; MIT format.
                  (LOOP REPEAT 4 DO (FUNCALL STREAM ':TYI))
                  (LET ((BASE 10.) (IBASE 10.)
                        (PACKAGE FILE-SYSTEM-PACKAGE))  ;for compatibility with old tapes
                                ;and also so :s come out.
                    (SETQ PLIST (READ STREAM)))
                  (VALUES PLIST ':MIT))
                 (T (FERROR NIL "Unknown tape header format.  Header is:~%~A"
                            (SUBSTRING BUF LOW (+ LOW COUNT))))))
    (FUNCALL STREAM ':DISCARD-CURRENT-INPUT-BUFFER)))


(DEFCONST TAPE-HEADER-LENGTH 1024. "Maximum and guaranteed block length for the header.")

(DEFUN WRITE-MAGTAPE-HEADER (STREAM PLIST &OPTIONAL (STYLE ':MIT))
  (UNWIND-PROTECT
      (SELECTQ STYLE
        (:MIT
         (LET* ((*NOPOINT T)
                (BASE 10.)
                (PACKAGE FILE-SYSTEM-PACKAGE)
                (SI:PRINT-READABLY T)
                (READTABLE SI:INITIAL-READTABLE))
           (SEND STREAM ':STRING-OUT "LMFL")
           (PRIN1 PLIST STREAM)
           (FUNCALL STREAM ':PAD-AND-SEND-BUFFER #\SPACE)))
        (OTHERWISE (FERROR NIL "~S is an unknown tape header format." STYLE)))
    (FUNCALL STREAM ':FINISH)))

(DEFFLAVOR HEADER-MIXIN
        (HEADER-FORMAT
         (SI:PROPERTY-LIST NIL))
        (SI:PROPERTY-LIST-MIXIN)
  (:INCLUDED-FLAVORS MT-MIXIN)
  :INITABLE-INSTANCE-VARIABLES)

(DEFFLAVOR HEADER-INPUT-MIXIN
        ()
        (HEADER-MIXIN))

(DEFFLAVOR HEADER-OUTPUT-MIXIN
        ()
        (HEADER-MIXIN))

;; Write the header at the last possible minute.
(DEFMETHOD (HEADER-OUTPUT-MIXIN :BEFORE :NEXT-INPUT-BUFFER) ()
  (AND (EQ STATUS ':OPEN)
       (NULL RQB)
       (WRITE-MAGTAPE-HEADER SELF SI:PROPERTY-LIST HEADER-FORMAT)))

;this tries to responds to pathcomponent stream messages, but may not make any
; assumptions about the exact flavor of path involved (which file system, etc).
(DEFFLAVOR MT-FILE-MIXIN
        ()
        ()
  (:INCLUDED-FLAVORS HEADER-MIXIN MT-MIXIN))

(DEFFLAVOR MT-FILE-INPUT-MIXIN
        ()
        (MT-FILE-MIXIN))

(DEFFLAVOR MT-FILE-OUTPUT-MIXIN
        ()
        (MT-FILE-MIXIN))

(DEFFLAVOR MT-FILE-INPUT-STREAM
        ()
        (MT-FILE-INPUT-MIXIN HEADER-INPUT-MIXIN MT-INPUT-STREAM))

(DEFFLAVOR MT-FILE-CHARACTER-INPUT-STREAM
        ()
        (MT-FILE-INPUT-MIXIN HEADER-INPUT-MIXIN MT-CHARACTER-INPUT-STREAM))

(DEFFLAVOR MT-FILE-OUTPUT-STREAM
        ()
        (MT-FILE-OUTPUT-MIXIN HEADER-OUTPUT-MIXIN MT-OUTPUT-STREAM))

(DEFFLAVOR MT-FILE-CHARACTER-OUTPUT-STREAM
        ()
        (MT-FILE-OUTPUT-MIXIN HEADER-OUTPUT-MIXIN MT-CHARACTER-OUTPUT-STREAM))

(DEFMETHOD (MT-FILE-MIXIN :DEVICE) ()
  NIL)

(DEFMETHOD (MT-FILE-MIXIN :DIRECTORY) ()
  (GET (LOCF SI:PROPERTY-LIST) ':DIRECTORY))

(DEFMETHOD (MT-FILE-MIXIN :NAME) ()
  (GET (LOCF SI:PROPERTY-LIST) ':NAME))

(DEFMETHOD (MT-FILE-MIXIN :TYPE) ()
  (GET (LOCF SI:PROPERTY-LIST) ':TYPE))

(DEFMETHOD (MT-FILE-MIXIN :VERSION) ()
  (GET (LOCF SI:PROPERTY-LIST) ':VERSION))

(DEFMETHOD (MT-FILE-MIXIN :CREATION-DATE) ()
  (GET (LOCF SI:PROPERTY-LIST) ':CREATION-DATE))

(DEFMETHOD (MT-FILE-MIXIN :QFASLP) ()
  (GET (LOCF SI:PROPERTY-LIST) ':QFASLP))

(DEFMETHOD (MT-FILE-OUTPUT-MIXIN :CHANGE-PROPERTIES) (IGNORE &REST PROPERTIES)
  (LOOP FOR (IND PROP) IN PROPERTIES BY #'CDDR
        DO (PUTPROP (LOCF SI:PROPERTY-LIST) PROP IND)))

;; This takes keywords compatibly with OPEN
;; Probe openings are not implemented.
;; This function expects the magtape to be at the beginning of a file header.
;; Errors returned unique to magtape are:
;;  PNA F Probe openings not allowed
;;  EOT F End of Tape

(DEFUN MAKE-MT-FILE-STREAM (&OPTIONAL
                             &REST OPTIONS
                             &KEY
                            (DIRECTION ':INPUT)
                            (CHARACTERS ':DEFAULT)
                            (ERROR T)
                            (BYTE-SIZE ':DEFAULT)
                            (UNIT 0)
                            (RECORD-SIZE *DEFAULT-RECORD-SIZE*)
                            (DENSITY 0)
                            (IBM-MODE NIL)
                            (PLIST NIL)
                            (HEADER-STYLE ':MIT)        ;Default for write
                            &ALLOW-OTHER-KEYS
                            &AUX STREAM TEM)
  (COND ((EQ DIRECTION ':INPUT)
         (MULTIPLE-VALUE (PLIST HEADER-STYLE)
           (READ-MAGTAPE-HEADER
             (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
                                         ':CHARACTERS T
                                         ':BYTE-SIZE 8
                                         ':UNIT UNIT
                                         ':DENSITY DENSITY
                                         OPTIONS))))
         (FUNCALL STREAM ':CLOSE ':RAW)
         (IF (NULL PLIST)
             (MT-OPEN-ERROR (MAKE-CONDITION 'END-OF-TAPE "End of tape on unit ~D." UNIT)
                            ERROR)
             (AND (EQ CHARACTERS ':DEFAULT)
                  (IF (SETQ TEM (GETL (LOCF PLIST) '(:CHARACTERS)))
                      (SETQ CHARACTERS (CADR TEM))
                      ;; Kludge for old format tapes.
                      (SETQ CHARACTERS (= (GET (LOCF PLIST) ':BYTE-SIZE) 8))))
             (AND (EQ BYTE-SIZE ':DEFAULT)
                  (SETQ BYTE-SIZE (GET (LOCF PLIST) ':BYTE-SIZE)))
             (AND (NULL BYTE-SIZE)
                  (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
             (MAKE-INSTANCE (IF CHARACTERS
                                'MT-FILE-CHARACTER-INPUT-STREAM
                                'MT-FILE-INPUT-STREAM)
                            ':BYTE-SIZE BYTE-SIZE
                            ':UNIT UNIT
                            ':RECORD-SIZE RECORD-SIZE
                            ':DENSITY DENSITY
                            ':PROPERTY-LIST PLIST
                            ':HEADER-FORMAT HEADER-STYLE
                            ':IBM-MODE IBM-MODE)))
        ((EQ DIRECTION ':OUTPUT)
         (AND (EQ BYTE-SIZE ':DEFAULT)
              (SETQ BYTE-SIZE (GET (LOCF PLIST) ':BYTE-SIZE)))
         (AND (NULL BYTE-SIZE)
              (SETQ BYTE-SIZE (IF CHARACTERS 8. 16.)))
         (WRITE-MAGTAPE-HEADER
           (SETQ STREAM (LEXPR-FUNCALL 'MAKE-MT-STREAM
                                       ':DIRECTION ':OUTPUT
                                       ':CHARACTERS T
                                       ':BYTE-SIZE 8
                                       ':UNIT UNIT
                                       ':DENSITY DENSITY
                                       OPTIONS))
           PLIST
           HEADER-STYLE)
         (FUNCALL STREAM ':CLOSE ':RAW)
         (MAKE-INSTANCE (IF CHARACTERS
                            'MT-FILE-CHARACTER-OUTPUT-STREAM
                            'MT-FILE-OUTPUT-STREAM)
                        ':BYTE-SIZE BYTE-SIZE
                        ':UNIT UNIT
                        ':RECORD-SIZE RECORD-SIZE
                        ':DENSITY DENSITY
                        ':PROPERTY-LIST PLIST
                        ':HEADER-FORMAT HEADER-STYLE
                        ':IBM-MODE IBM-MODE))
        (T (FERROR NIL "Probe opens not allowed on magtape."))))

(DEFUN MT-OPEN-ERROR (CONDITION ERROR-P)
  (IF ERROR-P
      (SIGNAL-CONDITION CONDITION)
    CONDITION))

(COMPILE-FLAVOR-METHODS MT-FILE-INPUT-STREAM MT-FILE-CHARACTER-INPUT-STREAM
                        MT-FILE-OUTPUT-STREAM MT-FILE-CHARACTER-OUTPUT-STREAM)

;;; RESTORE-MAGTAPE

;; This is the driver for RESTORE-MAGTAPE and friends.
;; The first value returned describes what it found on the magtape.
;; Possible values are:
;;  :EOF                An end of file block was found.
;;  :STREAM             A header block was found and sucessfully decoded.
;;                      Second value is an input stream, from which can be found
;;                      properties, truenames, partition-p, etc.
;;  :TAPE-HEADER        A block was found which was a tape header.
;;                      The plist from this header is the second value.
;;  :HEADER             This is some sort of header which is not a tape header.
;;                      The plist from this header is the second value.
;;  :ERROR              A tape header couldn't be decoded.
;;                      Reason (a string) is the second value.

;This tries to be just enuf to allow opening MT: for input or output.
; It specifially tries to make no assumptions about pathnames.
(DEFFLAVOR MT-HOST
        (DEVICE-NAME)
        (SI:BASIC-HOST)
  (:GETTABLE-INSTANCE-VARIABLES DEVICE-NAME)
  (:INITABLE-INSTANCE-VARIABLES DEVICE-NAME))

(DEFMETHOD (MT-HOST :NAME) NIL DEVICE-NAME)
(DEFMETHOD (MT-HOST :NAME-AS-FILE-COMPUTER) NIL DEVICE-NAME)
(DEFMETHOD (MT-HOST :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME DEVICE-NAME))

(DEFMETHOD (MT-HOST :PATHNAME-FLAVOR) ()
  (VALUES 'MT-PATHNAME NIL))

(DEFFLAVOR MT-PATHNAME () (PATHNAME))

(DEFMETHOD (MT-PATHNAME :PARSE-NAMESTRING)
           (HOST-SPECIFIED STRING &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (VALUES ':NO-INTERN (MAKE-INSTANCE 'MT-FILEHANDLE
                                     ':NAMESTRING (SUBSTRING STRING START END))))

;NAMESTRING is just for pseudo debugging purposes; Magtape files dont really have names.
(DEFFLAVOR MT-FILEHANDLE (NAMESTRING) ()
  (:INITABLE-INSTANCE-VARIABLES NAMESTRING))

(DEFMETHOD (MT-FILEHANDLE :STRING-FOR-PRINTING) () NAMESTRING)
(DEFMETHOD (MT-FILEHANDLE :PRINT-SELF) (STREAM PRINDEPTH SLASHIFY-P) PRINDEPTH
  (COND (SLASHIFY-P
         (SEND STREAM ':STRING-OUT "#<")
         (PRIN1 'MT-FILEHANDLE STREAM)
         (FORMAT STREAM " ~S ~O>" NAMESTRING (%POINTER SELF)))
        (T
         (SEND STREAM ':STRING-OUT NAMESTRING))))

;This is a kludge to make the copy-patch-files-of-system work.
(DEFMETHOD (MT-FILEHANDLE :PATCH-FILE-PATHNAME) (&REST IGNORE)
  "MT:")

(DEFUN PUTPROP-MAYBE (PLIST VALUE PROP)
  (WHEN (AND VALUE (NULL (GET PLIST PROP)))
    (PUTPROP PLIST VALUE PROP)))

(DEFMETHOD (MT-FILEHANDLE :OPEN) (IGNORE &REST KEYWORD-ARGS
                                         &KEY &OPTIONAL
                                         (DIRECTION ':INPUT)
                                         DEFAULTS-FROM-STREAM
                                         BYTE-SIZE
                                         AUTHOR
                                         &ALLOW-OTHER-KEYS)
  (COND ((EQ DIRECTION ':INPUT)
         (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM KEYWORD-ARGS))
        ((EQ DIRECTION ':OUTPUT)
         (COND (DEFAULTS-FROM-STREAM
                (LET* ((TRUENAME (FUNCALL DEFAULTS-FROM-STREAM ':TRUENAME))
                       (PLIST
                         (FILTER-PLIST (FUNCALL DEFAULTS-FROM-STREAM ':PLIST)
                                       (PLIST TRUENAME)))
                       (REAL-PLIST (LOCF PLIST)))
                  (PUTPROP-MAYBE REAL-PLIST BYTE-SIZE ':BYTE-SIZE)
                  (PUTPROP-MAYBE REAL-PLIST AUTHOR ':AUTHOR)
                  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':DIRECTORY) ':DIRECTORY)
                  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':NAME) ':NAME)
                  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':TYPE) ':TYPE)
                  (PUTPROP-MAYBE REAL-PLIST (SEND TRUENAME ':VERSION) ':VERSION)
   ;:UNSPECIFIC would not win when read back in, so guess.
                  (IF (EQ (GET (LOCF PLIST) ':TYPE)
                          ':UNSPECIFIC)
                      (LET ((FPLIST (FILE-READ-ATTRIBUTE-LIST NIL DEFAULTS-FROM-STREAM)))
                        (PUT-ON-ALTERNATING-LIST
                          (COND ((EQ (GET (LOCF FPLIST) ':MODE)
                                     ':LISP)
                                 "LISP")
                                ((= BYTE-SIZE 10)
                                 "TEXT")
                                (T "UNKNOWN"))
                          PLIST
                          ':TYPE)))
                  (LEXPR-FUNCALL 'MAKE-MT-FILE-STREAM
                                 ':PLIST PLIST
                                 KEYWORD-ARGS)))
               (T (FERROR NIL "MT: wins only for copying with stream default"))))
        (T (FERROR () "~A not a recognised :OPEN direction (and probes are not allowed"
                   DIRECTION))))

;TRY TO GET RID OF ANY UNPRINTABLE THINGS FROM PLIST-TO-FILTER..
(DEFUN FILTER-PLIST (PLIST-TO-FILTER PLIST-SO-FAR)
  (COND ((NULL PLIST-TO-FILTER) PLIST-SO-FAR)
        ((OR (NULL (CADR PLIST-TO-FILTER))
             (NUMBERP (CADR PLIST-TO-FILTER))
             (STRINGP (CADR PLIST-TO-FILTER))
             (LISTP (CADR PLIST-TO-FILTER)))
         (FILTER-PLIST (CDDR PLIST-TO-FILTER)
                       (CONS (CAR PLIST-TO-FILTER)
                             (CONS (CADR PLIST-TO-FILTER)
                                   PLIST-SO-FAR))))
        (T (FILTER-PLIST (CDDR PLIST-TO-FILTER) PLIST-SO-FAR))))

(DEFUN ADD-MT-HOST (&OPTIONAL (NAME "MT"))
  (COND ((NULL (GET-PATHNAME-HOST NAME T))
         (LET ((HOST (MAKE-INSTANCE 'MT-HOST ':DEVICE-NAME NAME)))
           (PUSH HOST *PATHNAME-HOST-LIST*)))))
(COMPILE-FLAVOR-METHODS MT-HOST MT-PATHNAME MT-FILEHANDLE)

(ADD-MT-HOST)

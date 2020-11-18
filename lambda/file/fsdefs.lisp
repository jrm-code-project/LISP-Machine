;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10 -*-
;;;; Basic Structures

(DEFVAR LOCAL-FILE-SYSTEM-AREA (MAKE-AREA :NAME 'LOCAL-FILE-SYSTEM-AREA :VOLATILITY 2)
  "All of the internal structures made by the file system are consed in this area.")

(DEFSTRUCT (DISK-CONFIGURATION (:TYPE :NAMED-ARRAY)
                               (:CONC-NAME DC-)
                               (:ALTERANT NIL)
                               (:MAKE-ARRAY (:AREA LOCAL-FILE-SYSTEM-AREA))
                               (:DEFAULT-POINTER DISK-CONFIGURATION))
  VERSION                                       ;The version of the format on disk
  PARTITION-SIZE
  PUT-BASE                                      ;Relative address of the page usage table
  PUT-SIZE
  ROOT-DIRECTORY                                ;The root directory.
  )

(DEFSTRUCT (DIRECTORY (:TYPE :NAMED-ARRAY)
                      :CONC-NAME
                      (:ALTERANT NIL)
                      (:MAKE-ARRAY (:AREA LOCAL-FILE-SYSTEM-AREA)))
  NAME                                          ;The name of the directory
  LOCK                                          ;The lock associated with the directory
  MAP                                           ;The map of the directory.
  FILES                                         ;The files specified in the map.
  )

(DEFSTRUCT (BASIC-FILE (:TYPE :NAMED-ARRAY)
                       (:CONC-NAME FILE-)
                       (:CONSTRUCTOR NIL)
                       (:ALTERANT NIL))
  NAME                                          ;A string.
  LOCK                                          ;A contention lock.
  MAP                                           ;The map of the data.
  FILES                                         ;Used only if the file is a directory.
  TYPE                                          ;A string.
  VERSION                                       ;A number (16b).
  )

(DEFSTRUCT (FILE (:TYPE :NAMED-ARRAY)
                 (:INCLUDE BASIC-FILE)
                 :CONC-NAME
                 (:MAKE-ARRAY (:AREA LOCAL-FILE-SYSTEM-AREA)))
  DEFAULT-BYTE-SIZE
  OVERWRITE-FILE                                ;Links overwriting/overwritten files.
  AUTHOR-INTERNAL                               ;A string
  CREATION-DATE-INTERNAL                        ;A number (32 bits)
  DIRECTORY                                     ;Mommy.
  OPEN-COUNT                                    ;The number of times this file is open
                                                ;-1 if open for overwrite.
  ATTRIBUTES                                    ;A 16-bit word of common properties
  PLIST                                         ;Arbitrary properties.
  GENERATION-NUMBER
  )

(DEFSTRUCT (LINK (:TYPE :NAMED-ARRAY)
                 (:INCLUDE BASIC-FILE)
                 :CONC-NAME
                 (:ALTERANT NIL)
                 (:MAKE-ARRAY (:AREA LOCAL-FILE-SYSTEM-AREA)))
  TO-STRING                                     ;The string linked to, ie "AI: DLA; FOO >"
  )

;;;; Property Bit Definitions

(DEFVAR ATTRIBUTES (MAKE-ARRAY 16.))

(DEFMACRO DEFINE-ATTRIBUTES (&BODY NUMBERS-AND-NAMES)
  `(PROGN 'COMPILE
          . ,(LOOP FOR (NUMBER NAME) ON NUMBERS-AND-NAMES BY #'CDDR
                   NCONCING `((DEFPROP ,NAME ,(1+ (* NUMBER 64.)) ATTRIBUTE)
                              (ASET ',NAME ATTRIBUTES ,NUMBER)))))

(DEFINE-ATTRIBUTES
  0  :DONT-DELETE                               ;File cannot be deleted if on.
  1  :CLOSED                                    ;File is still being written if off.
  2  :DELETED                                   ;File is deleted but still open somewhere.
  3  :DUMPED                                    ;File has been dumped
  4  :DONT-REAP                                 ;Dont reap this file (not used internally)
  5  :CHARACTERS                                ;The file is a CHARACTERS file.
  6  :DIRECTORY                                 ;The file is a subdirectory.

; 10 :QFASLP
; 11 :LISP                                      ;Obsolete
; 12 :TEXT                                      ;Obsolete
; 13 :MAY-BE-REAPED                             ;This bit is used by reaping programs.
                                                ;If it is on, the file is liable to be
                                                ;reaped.  Also, a reaping program may
                                                ;turn it on as a warning that it is liable
                                                ;to be reaped the next time around.
  14 :HEADER-BLOCK
  )

(DEFSUBST FILE-ATTRIBUTE (FILE INDICATOR)
  (LDB-TEST (GET INDICATOR 'ATTRIBUTE) (FILE-ATTRIBUTES FILE)))

(defsetf file-attribute set-file-attribute)

(DEFUN SET-FILE-ATTRIBUTE (FILE INDICATOR VAL)
  (SETF (FILE-ATTRIBUTES FILE)
        (DPB (IF VAL 1 0) (GET INDICATOR 'ATTRIBUTE) (FILE-ATTRIBUTES FILE)))
  (NOT (NULL VAL)))

;;; Syntax for attributes.

(DEFSUBST FILE-CLOSED? (FILE)
  (FILE-ATTRIBUTE FILE :CLOSED))

(DEFSUBST FILE-DELETED? (FILE)
  (FILE-ATTRIBUTE FILE :DELETED))

(DEFSUBST DIRECTORY? (FILE)
  (FILE-ATTRIBUTE FILE :DIRECTORY))

(DEFSUBST ROOT-DIRECTORY? (FILE)
  (NULL (FILE-DIRECTORY FILE)))

(DEFSUBST FILE-NPAGES (FILE)
  (MAP-NPAGES (FILE-MAP FILE)))


;;;; Printers

(DEFSELECT FILE-NSI
  (:PRINT-SELF (FILE STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (FILE STREAM :NO-POINTER)
      (COND ((DIRECTORY? FILE)
             (FORMAT STREAM "LMFS-Directory~{ ~A~}" (DIRECTORY-FULL-NAME FILE)))
            (T
             (FORMAT STREAM "LMFS-File ~S"
                     (LM-NAMESTRING NIL NIL
                                    (DIRECTORY-FULL-NAME (FILE-DIRECTORY FILE))
                                    (FILE-NAME FILE)
                                    (FILE-TYPE FILE)
                                    (FILE-VERSION FILE))))))))

(DEFSELECT DIRECTORY-NSI
  (:PRINT-SELF (DIRECTORY STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (DIRECTORY STREAM :TYPEP :NO-POINTER)
      (PRIN1 (DIRECTORY-NAME DIRECTORY) STREAM))))

(DEFUN DIRECTORY-FULL-NAME (DIRECTORY)
  (AND (NOT (ROOT-DIRECTORY? DIRECTORY))
       (APPEND (DIRECTORY-FULL-NAME (FILE-DIRECTORY DIRECTORY))
               (LIST (DIRECTORY-NAME DIRECTORY)))))

(DEFSELECT MAP-NSI
  (:PRINT-SELF (MAP STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (MAP STREAM :TYPEP)
      (FORMAT STREAM "~D block~:P" (MAP-NBLOCKS MAP))))
  (:DESCRIBE (MAP) (DESCRIBE-MAP MAP)))

(DEFPROP FILE      FILE-NSI      NAMED-STRUCTURE-INVOKE)
(DEFPROP LINK      FILE-NSI      NAMED-STRUCTURE-INVOKE)
(DEFPROP DIRECTORY DIRECTORY-NSI NAMED-STRUCTURE-INVOKE)
(DEFPROP MAP       MAP-NSI       NAMED-STRUCTURE-INVOKE)

;;; Randomness.

(DEFUN LMFS-KEYWORD-OPTION (OPTIONS KEYWORD DEFAULT)
  (COND ((NULL OPTIONS)
         DEFAULT)
        ((EQ KEYWORD (CAR OPTIONS))
         (CADR OPTIONS))
        (T
         (LMFS-KEYWORD-OPTION (CDDR OPTIONS) KEYWORD DEFAULT))))

;;;; Locking Macros

(DEFMACRO LOCKING (LOCK &BODY BODY)
  "Execute BODY with LOCK locked."
  (LET ((LOCK-CELL (GENSYM)))
    `(LET ((,LOCK-CELL (LOCF ,LOCK)))
       (UNWIND-PROTECT
         (PROGN (PROCESS-LOCK ,LOCK-CELL) . ,BODY)
         (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL)))))

(DEFMACRO LOCKING-RECURSIVELY (LOCK &BODY BODY)
  "Execute BODY with LOCK locked; don't die if already locked."
  (LET ((LOCK-CELL (GENSYM))
        (LOCK-OWNED (GENSYM)))
    `(LET* ((,LOCK-CELL (LOCF ,LOCK))
            (,LOCK-OWNED (EQ (CAR ,LOCK-CELL) CURRENT-PROCESS)))
       (UNWIND-PROTECT
         (PROGN (IF (NOT ,LOCK-OWNED)
                    (PROCESS-LOCK ,LOCK-CELL))
                . ,BODY)
         (IF (NOT ,LOCK-OWNED)                  ;unlock it only if you locked it.
             (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL))))))

(DEFMACRO REQUIRE-LOCK (LOCK)
  (LET ((ERSTR (FORMAT NIL "~S should be locked here but is not."
                       (IF (ATOM LOCK) LOCK (CAR LOCK)))))
    `(IF (NOT (EQ ,LOCK CURRENT-PROCESS))
         (FERROR NIL ,ERSTR))))

(DEFMACRO MODIFYING-DIRECTORY (DIRECTORY &BODY BODY)
  "Lock DIRECTORY, execute BODY, and then mark DIRECTORY as modified."
  (LET ((LOCK-CELL (GENSYM)))
    `(LET ((,LOCK-CELL (LOCF (DIRECTORY-LOCK ,DIRECTORY))))
       (UNWIND-PROTECT
         (PROGN (PROCESS-LOCK ,LOCK-CELL) . ,BODY)
         (PROGN (SETF (FILE-CLOSED? DIRECTORY) NIL)
                (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL))))))

(DEFMACRO OPEN-INPUT-FILE ((FILE PATHNAME) &BODY BODY)
  `(LET ((,FILE NIL))
     (UNWIND-PROTECT
       (PROGN
         (SETQ ,FILE (LOOKUP-FILE
                       (PATHNAME-RAW-DIRECTORY ,PATHNAME)
                       (PATHNAME-RAW-NAME ,PATHNAME)
                       (PATHNAME-RAW-TYPE ,PATHNAME)
                       (PATHNAME-RAW-VERSION ,PATHNAME)
                       :ERROR NIL T))
         . ,BODY)
       (WHEN ,FILE
         (LMFS-CLOSE-FILE ,FILE)))))

(DEFMACRO OPEN-INPUT-FILE-OR-DIRECTORY ((FILE PATHNAME) &BODY BODY)
  `(LET ((,FILE NIL))
     (UNWIND-PROTECT
       (PROGN
         (SETQ ,FILE (LOOKUP-FILE
                       (PATHNAME-RAW-DIRECTORY ,PATHNAME)
                       (PATHNAME-RAW-NAME ,PATHNAME)
                       (PATHNAME-RAW-TYPE ,PATHNAME)
                       (PATHNAME-RAW-VERSION ,PATHNAME)
                       :ERROR NIL))
         . ,BODY)
       (WHEN ,FILE
         (LMFS-CLOSE-FILE ,FILE)))))

(DEFCONST LM-VERSION 5
  "Disk format version we will create now if we initialize a partition.")
;Version 4 differs from version 3 in having the
;root directory look like all other directories.
;Version 5 has :HEADER-BLOCK attribute and in-place directory update

(DEFUN INSTALL-NEW-LM-VERSION ()
  (SETF (DC-VERSION) LM-VERSION))

(DEFCONST LM-MINIMUM-VERSION 3
  "Lowest format version we can handle.")

(DEFVAR LM-UNIT 0)                              ;See SET-LM-BAND
(DEFVAR LM-PARTITION "FILE")
(DEFVAR LM-PARTITION-POSSIBILITIES '((0 "DFIL") (0 "FILE") (4 "FILE")))
(DEFVAR LM-PARTITION-BASE)                      ;The address of the partition.

;; ***** This should be replaced by a device scheme.  *****
(DEFVAR DISK-CONFIGURATION)
(DEFVAR DISK-CONFIGURATION-RQB)
(DEFVAR DISK-CONFIGURATION-BUFFER)
(DEFVAR DISK-CONFIGURATION-BUFFER-POINTER)
(DEFVAR DISK-CONFIGURATION-LOCK NIL)

(DEFVAR PUT-LOCK NIL)
(DEFVAR PUT-SCANNING-INDEX 0)
(DEFVAR PUT-RQB)                ;The page usage table is indirected into this.
(DEFVAR PAGE-USAGE-TABLE)       ;The actual table.

;; The first entry in the page usage table tells whether the table is consistent
;; with the rest of the system.  If it is not, this only reflects the fact that
;; some pages are lost...
(DEFCONST PUT-CONSISTENT 1)
(DEFCONST PUT-INCONSISTENT 2)

;; The remaining entries describe all the remaining pages in the file (not counting
;; the configuration page, which is always used).
(DEFCONST PUT-FREE 0)                           ;Page is up for grabs.
(DEFCONST PUT-RESERVED 1)                       ;Page is allocated but (likely) deletable.
(DEFCONST PUT-USED 2)                           ;Page is allocated in core and on disk.
(DEFCONST PUT-UNUSABLE 3)                       ;Page should not be referenced ever.

(DEFVAR PAGE-SIZE-IN-BITS (* PAGE-SIZE 32.))

;; This is also in MTDEFS
(REMPROP 'QUOTIENT-CEILING 'SOURCE-FILE-NAME)
(DEFSUBST QUOTIENT-CEILING (Y X) (CEILING Y X))

;; Get a buffer of the correct byte size
(DEFUN GET-RQB-ARRAY (RQB BYTE-SIZE &AUX TYPE)
  (COND ((= BYTE-SIZE 16.) (RQB-BUFFER RQB))
        ((= BYTE-SIZE 8) (RQB-8-BIT-BUFFER RQB))
        ((SETQ TYPE (CDR (ASSQ BYTE-SIZE '((4 . ART-4B) (2 . ART-2B) (1 . ART-1B)))))
         (MAKE-ARRAY (FLOOR (* (RQB-NPAGES RQB) PAGE-SIZE-IN-BITS) BYTE-SIZE)
                     :AREA LOCAL-FILE-SYSTEM-AREA
                     :TYPE TYPE
                     :DISPLACED-TO RQB
                     ;; This is a system bug.  Don't try to figure it out.
                     :DISPLACED-INDEX-OFFSET (* (%P-CONTENTS-OFFSET (RQB-BUFFER RQB) 3)
                                                 (FLOOR 16. BYTE-SIZE))))
        (T (FERROR NIL "~D is an invalid byte size." BYTE-SIZE))))

;; These are the only interface to the disk in the file system, with the exception
;; of FIND-DISK-PARTITION and UPDATE-PARTITION-COMMENT.
;; The unit is always LM-UNIT, nd the address is relative to
;; the current partition (LM-PARTITION).

(DEFUN LM-DISK-WRITE (RQB ADDR &OPTIONAL (NPAGES (RQB-NPAGES RQB)))
; (COND ((EQ RQB PUT-RQB) (FORMAT T "~%>>Writing the page usage table"))
;       ((EQ RQB DISK-CONFIGURATION-RQB) (FORMAT T "~%>>Writing configuration"))
;       (WITHIN-FILE-SYSTEM (FORMAT T "~%>>Writing an internal structure"))
;       (T (FORMAT T "~%>>Writing a file block")))
  (COND ((< ADDR (DC-PARTITION-SIZE))
         (si:disk-write-n-pages rqb lm-unit (+ addr lm-partition-base) npages))
        (T (FERROR NIL "Disk Write out of range."))))

(DEFUN LM-DISK-READ (RQB ADDR &OPTIONAL (NPAGES (RQB-NPAGES RQB)))
  (COND ((< ADDR (DC-PARTITION-SIZE))
         (si:disk-read-n-pages rqb lm-unit (+ addr lm-partition-base) npages))
        (T (FERROR NIL "Disk Read out of range."))))


;;; Use this to write out new maps.

(DEFMACRO-DISPLACE WITH-MAP-STREAM-OUT ((STREAM) &BODY BODY)
  "A variant of WITH-OPEN-STREAM, it returns a new map."
  `(LET ((.FILE-ABORTED-FLAG. :ABORT)
         (,STREAM NIL))
     (UNWIND-PROTECT
       (PROGN (SETQ ,STREAM (MAKE-MAP-STREAM-OUT))
              ,@BODY
              (SETQ .FILE-ABORTED-FLAG. NIL))
       (IF (AND (NOT (NULL ,STREAM))
                (NOT (ERRORP ,STREAM)))
           (SEND ,STREAM :CLOSE .FILE-ABORTED-FLAG.)))
     (AND (NULL .FILE-ABORTED-FLAG.)
          (SEND ,STREAM :MAP))))

(DEFMACRO-DISPLACE WITH-MAP-STREAM-IN ((STREAM . ARGS) &BODY BODY)
  "A variant of WITH-OPEN-STREAM."
  `(LET ((.FILE-ABORTED-FLAG. :ABORT)
         (,STREAM NIL))
     (UNWIND-PROTECT
       (PROG1 (PROGN (SETQ ,STREAM (MAKE-MAP-STREAM-IN . ,ARGS))
                     . ,BODY)
              (SETQ .FILE-ABORTED-FLAG. NIL))
       (IF (AND (NOT (NULL ,STREAM))
                (NOT (ERRORP ,STREAM)))
           (SEND ,STREAM :CLOSE .FILE-ABORTED-FLAG.)))))

;; Directory information on the disk is stored in packed format.  The
;; following functions allow the file system to read and write fixnums
;; and bignums in such a packed format.

;(DEFUN GET-BYTES (STREAM NBYTES)
;  (LOOP WITH V = 0 AND I = 0
;       AS C = (SEND STREAM :TYI)
;       WHEN (NULL C) RETURN NIL
;       DO (SETQ V (+ (* V 256.) C)
;                I (1+ I))
;       WHEN ( I NBYTES) RETURN V))
;
;(DEFUN PUT-BYTES (STREAM NBYTES VALUE)
;  (LOOP FOR PPSS = (+ (* (1- NBYTES) 512.) 8) THEN (- PPSS 512.)
;       DO (SEND STREAM :TYO (LDB PPSS VALUE))
;       UNTIL (= PPSS 8))
;  VALUE)

;; These macros do not use ONCE-ONLY because it loses in the simple case of NBYTES = 1.

(DEFMACRO GET-BYTES (STREAM NBYTES)
  (LOOP FOR I FROM (1- NBYTES) DOWNTO 0
        AS X = `(SEND ,STREAM :TYI)
        WHEN (PLUSP I) DO (SETQ X `(* ,X ,(^ 256. I)))
        COLLECT X INTO FORMS
        FINALLY (IF (= NBYTES 1)
                    (SETQ FORMS (CAR FORMS))
                  (SETQ FORMS (CONS '+ FORMS)))
                (RETURN FORMS)))

(DEFMACRO PUT-BYTES (STREAM NBYTES VALUE)
  `(PROGN . ,(LOOP FOR PPSS FROM (+ (* (1- NBYTES) #o1000) #o10) DOWNTO #o0010 BY #o1000
                   COLLECTING `(SEND ,STREAM :TYO (LDB ,PPSS ,VALUE)))))

(DEFUN PUT-STRING (STRING STREAM)
  (SEND STREAM :TYO (ARRAY-ACTIVE-LENGTH STRING))
  (SEND STREAM :STRING-OUT STRING))

(DEFUN GET-STRING (STREAM)
  (LET* ((LEN (SEND STREAM :TYI))
         (ARR (MAKE-ARRAY LEN :TYPE 'ART-STRING)))
    (SEND STREAM :STRING-IN NIL ARR 0 LEN)
    ARR))


(DEFVAR *NON-WRITEABLE-PROPERTIES* '(%TRUENAME-INTERNAL %HEADER-INTERNAL))

(DEFUN WRITE-PROPERTY-LIST (STREAM PLIST)
  (DO ((L PLIST (CDDR L))
       (X 0))
      ((NULL L)
       (PUT-BYTES STREAM 1 X))
    (OR (MEMQ (CAR L) *NON-WRITEABLE-PROPERTIES*)
        (INCF X)))
  (DO ((L PLIST (CDDR L))
       (PROP))
      ((NULL L))
    (WHEN (NOT (MEMQ (CAR L) *NON-WRITEABLE-PROPERTIES*))
      (PUT-STRING (GET-PNAME (CAR L)) STREAM)
      (COND ((NULL (SETQ PROP (CADR L)))
             (SEND STREAM :TYO 0))
            ((EQ PROP T)
             (SEND STREAM :TYO 1))
            ((SYMBOLP PROP)
             (COND ((keywordp prop)
                    (SEND STREAM :TYO 2)
                    (PUT-STRING (GET-PNAME PROP) STREAM))
                   (T (SEND STREAM :TYO 3)
                      ;; Use this odd order for ease in interning later.
                      (PUT-STRING (GET-PNAME PROP) STREAM)
                      (PUT-STRING (PACKAGE-NAME (SYMBOL-PACKAGE PROP)) STREAM))))
            ((INTEGERP PROP)
             (SEND STREAM :TYO 4)
             (PUT-BYTES STREAM 3 PROP))
            ((STRINGP PROP)
             (SEND STREAM :TYO 5)
             (PUT-STRING PROP STREAM))
            ((CONSP PROP)
             (SEND STREAM :TYO 6)
             (LET ((*PACKAGE* SI:PKG-USER-PACKAGE)
                   (*READTABLE* si:INITIAL-READTABLE)
                   (*PRINT-BASE* 10.)
                   (*NOPOINT T)
                   (*PRINT-RADIX* NIL)
                   (SI:PRINT-READABLY T))
               (PRIN1 PROP STREAM)))
            ((TYPEP PROP 'PATHNAME)
             (SEND STREAM :TYO 7)
             (LET ((PACKAGE SI:PKG-USER-PACKAGE)
                   (READTABLE SI:INITIAL-READTABLE)
                   (BASE 10.)
                   (*NOPOINT T)
                   (SI:PRINT-READABLY T))
               ;; These are the arguments to MAKE-FASLOAD-PATHNAME.
               (SEND STREAM :TYO #/()
               (PRIN1-THEN-SPACE (SEND (PATHNAME-HOST PROP) :NAME-AS-FILE-COMPUTER) STREAM)
               (PRIN1-THEN-SPACE (PATHNAME-DEVICE PROP) STREAM)
               (PRIN1-THEN-SPACE (PATHNAME-DIRECTORY PROP) STREAM)
               (PRIN1-THEN-SPACE (PATHNAME-NAME PROP) STREAM)
               (PRIN1-THEN-SPACE (PATHNAME-TYPE PROP) STREAM)
               (PRIN1 (PATHNAME-VERSION PROP) STREAM)
               (SEND STREAM :TYO #/))))
            (T (FERROR NIL "I don't know how to write ~S as a property." PROP))))))


(DEFUN READ-PROPERTY-LIST (STREAM &AUX LIST (PAK SI:PKG-KEYWORD-PACKAGE))
  (SETQ LIST (MAKE-LIST (* (SEND STREAM :TYI) 2)))
  (DO ((L LIST (CDDR L)))
      ((NULL L))
    (RPLACA L (INTERN (GET-STRING STREAM) PAK))
    (CASE (SEND STREAM :TYI)
      (0)
      (1 (SETF (CADR L) T))
      (2 (SETF (CADR L) (INTERN (GET-STRING STREAM) PAK)))
      (3 (SETF (CADR L) (INTERN (GET-STRING STREAM)
                                (PKG-FIND-PACKAGE (GET-STRING STREAM) :ASK))))
      (4 (SETF (CADR L) (GET-BYTES STREAM 3)))
      (5 (SETF (CADR L) (GET-STRING STREAM)))
      (6 (SETF (CADR L)
               (LET ((*READ-BASE* 10.)
                     (*PACKAGE* SI:PKG-USER-PACKAGE)
                     (*READTABLE* SI:INITIAL-READTABLE))
                   ;; This can lose pretty badly with #<'s, etc.  -- DLA
                   (CLI:READ STREAM))))
      (7 (LET* ((*READ-BASE* 10.)
                (*PACKAGE* SI:PKG-USER-PACKAGE)
                (*READTABLE* SI:INITIAL-READTABLE)
                (FORM (CLI:READ STREAM))
                (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))       ;<-*
           (SETF (CADR L)
                 (IF (= (LENGTH FORM) 6)
                     (APPLY #'FS:MAKE-FASLOAD-PATHNAME FORM)
                   (EVAL FORM)))))              ;Obsolete form for pathnames to be written.
      (OTHERWISE (FERROR NIL "Invalid Plist property designator."))))
  LIST)

;; Functions for creating and manipulating MAPs.

;;; the default leader length used to be 2, but I cannot find anything
;;; in local file that uses the second slot.  I'm going to use another
;;; slot for the map's owner that will be stored into if the map belongs
;;; to a directory and the :SAVE-DIRECTORY-MAPS debug mode is enabled.
;;; -dg 3/4/86

(defconst *map-leader-length* 3)

(DEFSUBST MAP-NBLOCKS (MAP)
  (ARRAY-LEADER MAP 0))

(defsubst map-owner (map)
  (array-leader map 2))

(defsetf map-owner (map) (owner)
  `(store-array-leader ,owner ,map 2))

(DEFSUBST MAP-BLOCK-LOCATION (MAP INDEX)
  (AREF MAP INDEX 0))

(DEFSUBST MAP-BLOCK-SIZE (MAP INDEX)
  (AREF MAP INDEX 1))

(DEFUN MAP-CREATE (&OPTIONAL (NBLOCKS 32.) INPUTP &AUX MAP)
  (SETQ MAP (MAKE-ARRAY (LIST-IN-AREA LOCAL-FILE-SYSTEM-AREA NBLOCKS 2)
                        :LEADER-LENGTH *map-leader-length*
                        :NAMED-STRUCTURE-SYMBOL 'MAP))
  (SETF (MAP-NBLOCKS MAP) (IF INPUTP NBLOCKS 0))
  MAP)

;; Returns the number of pages used (as opposed to the number of blocks) in the map.
(DEFUN MAP-NPAGES (MAP)
  (LOOP WITH NBLOCKS = (MAP-NBLOCKS MAP)
        FOR I FROM 0 BELOW NBLOCKS
        SUMMING (CEILING (MAP-BLOCK-SIZE MAP I) PAGE-SIZE-IN-BITS)))

(DEFUN MAP-NPAGES-AVAILABLE (MAP)
  (DO ((N (ARRAY-DIMENSION MAP 0))
       (J 0 (1+ J))
       (TOTAL 0)
       (SIZE))
      ((OR (= J N)
           (NULL (SETQ SIZE (MAP-BLOCK-SIZE MAP J))))
       TOTAL)
    (INCF TOTAL (CEILING SIZE PAGE-SIZE-IN-BITS))))

;; This is used during output, when a block is added onto the end of a file.
(DEFUN MAP-APPEND-BLOCK (MAP LOCATION SIZE &AUX (NBLOCKS (MAP-NBLOCKS MAP)))
  (AND (= NBLOCKS (ARRAY-DIMENSION MAP 0))
       (ARRAY-GROW MAP (+ NBLOCKS 32.) 2))
  (WITHOUT-INTERRUPTS
    (SETF (MAP-NBLOCKS MAP) (1+ NBLOCKS))
    (SETF (MAP-BLOCK-LOCATION MAP NBLOCKS) LOCATION)
    (SETF (MAP-BLOCK-SIZE MAP NBLOCKS) SIZE)))

;; Returns the length of the area mapped in bits.
(DEFUN MAP-LENGTH (MAP)
  (LOOP WITH NBLOCKS = (MAP-NBLOCKS MAP)
        FOR I FROM 0 BELOW NBLOCKS
        SUMMING (MAP-BLOCK-SIZE MAP I)))

(DEFUN MAP-READ (STREAM)
  (LOOP WITH NBLOCKS = (GET-BYTES STREAM 2)
        WITH MAP = (MAP-CREATE NBLOCKS T)
        FOR I FROM 0 BELOW NBLOCKS
        DOING (SETF (MAP-BLOCK-LOCATION MAP I) (GET-BYTES STREAM 3))
              (SETF (MAP-BLOCK-SIZE MAP I) (GET-BYTES STREAM 3))
        FINALLY (RETURN MAP)))

(DEFUN MAP-WRITE (STREAM MAP)
  (LET ((NBLOCKS (MAP-NBLOCKS MAP)))
    (PUT-BYTES STREAM 2 NBLOCKS)
    (DOTIMES (I NBLOCKS)
      (MAP-WRITE-1-BLOCK STREAM
                         (MAP-BLOCK-LOCATION MAP I)
                         (MAP-BLOCK-SIZE MAP I))))
  MAP)

(DEFUN EXTENDED-MAP-READ (STREAM)
  (LET ((MAP (MAP-READ STREAM)))
    (SETF (MAP-NBLOCKS MAP) (GET-BYTES STREAM 2))
    MAP))

(DEFUN EXTENDED-MAP-WRITE (STREAM MAP)
  (LET ((NBLOCKS (DO ((N (ARRAY-DIMENSION MAP 0))
                      (J 0 (1+ J)))
                     ((OR (= J N) (NOT (MAP-BLOCK-SIZE MAP J)))
                      J))))
    (PUT-BYTES STREAM 2 NBLOCKS)
    (DOTIMES (I NBLOCKS)
      (MAP-WRITE-1-BLOCK STREAM
                         (MAP-BLOCK-LOCATION MAP I)
                         (MAP-BLOCK-SIZE MAP I)))
    (PUT-BYTES STREAM 2 (MAP-NBLOCKS MAP)))
  MAP)


(DEFUN MAP-WRITE-1-BLOCK (STREAM LOCATION SIZE)
  (PUT-BYTES STREAM 3 LOCATION)
  (PUT-BYTES STREAM 3 SIZE))

(DEFUN DESCRIBE-MAP (MAP &OPTIONAL (STREAM STANDARD-OUTPUT))
  (FORMAT STREAM "~&~S maps out the following blocks:~@
~2@TIndex~3@TLocation~7@TSize~3@TStatus~%" MAP)
  (LET ((NBITS 0))
    (DOTIMES (I (MAP-NBLOCKS MAP))
      (FORMAT STREAM "~6O: ~10O ~10O   ~[Free~;Reserved~;Used~;Bad~]~%"
              I (MAP-BLOCK-LOCATION MAP I) (MAP-BLOCK-SIZE MAP I)
              (AREF PAGE-USAGE-TABLE (MAP-BLOCK-LOCATION MAP I)))
      (INCF NBITS (MAP-BLOCK-SIZE MAP I)))
    (FORMAT STREAM "~%Total of ~D block~:P, ~D bit~:P.~%"
            (MAP-NBLOCKS MAP)
            NBITS)))

(DEFUN FILE-DATA-LENGTH (FILE)
  (MAP-LENGTH (FILE-MAP FILE)))

(DEFUN FILE-NPAGES (FILE)
  (MAP-NPAGES (FILE-MAP FILE)))

(DEFUN FILE-TRUENAME (FILE)
  ;; If at all possible, make the host name be a normal network host name, since these
  ;; names are transportable among machines, whereas "LM" is not.
  (OR (GETF (FILE-PLIST FILE) '%TRUENAME-INTERNAL)
      (SETF (GETF (FILE-PLIST FILE) '%TRUENAME-INTERNAL)
            (MAKE-PATHNAME :HOST (IF SI:LOCAL-HOST (SEND SI:LOCAL-HOST :NAME-AS-FILE-COMPUTER) "LM")
                           :DEVICE "DSK"
                           :DIRECTORY (OR (DIRECTORY-FULL-NAME (FILE-DIRECTORY FILE))
                                          :ROOT)
                           :NAME (FILE-NAME FILE)
                           :TYPE (IF (EQUAL (FILE-TYPE FILE) "")
                                     :UNSPECIFIC
                                   (FILE-TYPE FILE))
                           :VERSION (FILE-VERSION FILE)))))

;; Error handler interface.

(DEFMACRO IDENTIFY-FILE-OPERATION (OPERATION &BODY BODY)
  `(LET ((*CURRENT-FILE-OPERATION* (OR *CURRENT-FILE-OPERATION* ,OPERATION)))
     . ,BODY))

;; Wrap this around things which want to do things the simple way.
;; If ERROR-P is non-NIL and an error occurs, the error object is returned from this form
(DEFMACRO HANDLING-ERRORS (ERROR-P &BODY BODY)
  `(CATCH 'LM-TRAP-ERRORS
     (LET ((LM-TRAP-ERRORS (NOT ,ERROR-P)))
       (FILE-OPERATION-RETRY
         . ,BODY))))

;; This is bound to T if one is inside LM-TRAP-ERROR, in which case control
;; is thrown to LM-TRAP-ERROR
(DEFVAR LM-TRAP-ERRORS NIL)

(DEFVAR *CURRENT-FILE-OPERATION* NIL)
(DEFVAR *CURRENT-OPERATION-PATHNAME* NIL)

(DEFPROP LM-SIGNAL-ERROR T :ERROR-REPORTER)
(DEFUN LM-SIGNAL-ERROR (SYMBOL &OPTIONAL SOURCE PROCEEDABLE &REST MAKE-CONDITION-ARGS)
  (OR SOURCE
      ;; SELF IS PROBABLY NEVER THE PATHNAME
      ;; SINCE SOMEBODY CHANGED FILE-HOST PATHNAMES
      ;; TO USE THE "ACCESS" ABSTRACTION.
      ;; 5-Feb-87 18:29:45 -GJC
      (AND (OR (TYPEP SELF 'PATHNAME)
               (TYPEP SELF 'SI:FILE-STREAM-MIXIN))
           (SETQ SOURCE SELF))
      (SETQ SOURCE *CURRENT-OPERATION-PATHNAME*))
  (AND SOURCE (NOT (TYPEP SOURCE 'PATHNAME))
       (SETQ SOURCE (SEND SOURCE :PATHNAME)))
  (LET ((ERROR
          (APPLY #'MAKE-CONDITION
                 (OR (GET SYMBOL 'ERROR-SIGNALER) SYMBOL)
                 (LET ((STRING (GET SYMBOL 'ERROR-STRING)))
                   (IF SOURCE
                       (STRING-APPEND STRING " for "
                                      (LET ((S (SEND SOURCE :STRING-FOR-PRINTING)))
                                        (COND ((STRING-SEARCH "~" S)
                                               (WITH-OUTPUT-TO-STRING (SO)
                                                 (WITH-INPUT-FROM-STRING (SI S)
                                                   (DO ((C))
                                                       ((NOT (SETQ C (SEND SI :TYI))))
                                                     (IF (= #\~ C) (SEND SO :TYO C))
                                                     (SEND SO :TYO C)))))
                                              ('ELSE
                                               S))))
                     STRING))
                 SOURCE MAKE-CONDITION-ARGS)))
       (IF LM-TRAP-ERRORS
           (THROW 'LM-TRAP-ERRORS ERROR))
       (SIGNAL ERROR
               :PROCEED-TYPES
               (COND ((CONSP PROCEEDABLE) PROCEEDABLE)
                     (PROCEEDABLE '(:RETRY-FILE-OPERATION))))))

;; Use this when an error occurs in file lookup.
(DEFPROP LM-LOOKUP-ERROR T :ERROR-REPORTER)
(DEFUN LM-LOOKUP-ERROR (SYMBOL DIR NAM TYP VER &OPTIONAL (OPERATION *CURRENT-FILE-OPERATION*))
  (LM-SIGNAL-ERROR SYMBOL
                   (MAKE-PATHNAME :HOST "LM" :DEVICE "DSK"
                                  :DIRECTORY
                                  (COND ((STRINGP DIR) DIR)
                                        ((CONSP DIR) DIR)
                                        (T
                                         (DIRECTORY-NAME DIR)))
                                  :NAME NAM :TYPE TYP :VERSION VER)
                   NIL
                   OPERATION))

;; This is to aid in debugging.  It basically provides a warning at compile time
;; if you haven't defined an error.
(DEFUN LM-ERROR-SYMBOL-CHECK (FORM)
  (LET ((X (SECOND FORM)))
    (AND (EQ (CAR-SAFE X) 'QUOTE)
         (PROGN
           (WHEN (NOT (GET (CADR X) 'ERROR-STRING))
             (COMPILER:BARF (CADR X) "is not a defined error symbol" 'COMPILER:WARN))
           (OR (GET (CADR X) 'ERROR-SIGNALER)
               (GET (CADR X) 'EH:MAKE-CONDITION-FUNCTION)
             (COMPILER:BARF (CADR X) "is not a defined signal name" 'COMPILER:WARN))))
    FORM))

(COMPILER:ADD-OPTIMIZER LM-SIGNAL-ERROR LM-ERROR-SYMBOL-CHECK)
(COMPILER:ADD-OPTIMIZER LM-LOOKUP-ERROR LM-ERROR-SYMBOL-CHECK)
(COMPILER:ADD-OPTIMIZER LM-RENAME-ERROR LM-ERROR-SYMBOL-CHECK)

;; Error Definitions
(DEFMACRO DEFINE-ERRORS (&REST SYMBOLS-AND-STRINGS)
  `(PROGN 'COMPILE
          . ,(LOOP FOR (SYMBOL STRING) ON SYMBOLS-AND-STRINGS BY #'CDDR
                   COLLECTING `(DEFPROP ,SYMBOL ,STRING ERROR-STRING))))

(DEFINE-ERRORS
;; Error signalers in SYS:IO;OPEN
  RENAME-TO-EXISTING-FILE       "Attempt to rename to an existing file"
  RENAME-ACROSS-DIRECTORIES     "Attempt to rename across directories"
  FILE-NOT-FOUND                "File not found"
  FILE-ALREADY-EXISTS           "File already exists"
  DIRECTORY-NOT-FOUND           "Directory not found"
  INVALID-BYTE-SIZE             "Illegal byte size"
  NO-MORE-ROOM                  "Disk full"
  NO-FILE-SYSTEM                "File system not mounted"
  DIRECTORY-NOT-EMPTY           "Non-empty directories cannot be deleted"
  DONT-DELETE-FLAG-SET          "File has DONT-DELETE bit set"
  UNIMPLEMENTED-OPTION          "Unimplemented OPEN option"
  FILE-LOCKED                   "File is locked"
  UNKNOWN-OPERATION             "Unknown operation"

;; Error signalers defined below.
  UNDELETE-UNCLOSED-FILE        "Cannot undelete unclosed file"
  OPEN-DELETED-FILE             "File is deleted"
  OPEN-DELETED-DIRECTORY        "Directory is deleted"
  OPEN-OUTPUT-FILE              "File is open for output"
  OPEN-UNFINISHED-DIRECTORY     "Directory still being created"
  FILE-IS-SUBDIRECTORY          "File is a subdirectory"
  UNSETTABLE-PROPERTY           "Unsettable property"
  INVALID-PROPERTY-NAME         "Bad property"
  VERSION-TOO-LARGE             "Version must be smaller than 65536"
  INVALID-DIRECTORY-NAME        "Invalid name given for a directory"
  RENAME-DIRECTORY-INVALID-TYPE "Invalid new type or version for renaming a subdirectory."
  LINKS-NOT-SUPPORTED           "Links not supported"
  BAD-TYPE-FOR-SUBDIRECTORY     "Invalid type for subdirectory"
  SUPERSEDE-DIRECTORY           "Superseding a directory"
  TOP-LEVEL-FILE                "No non-directory files in root"

  ;; also defined in OPEN, experimental.
  INVALID-WILDCARD              "Wildcard that got through pathname parsing but file system didn't like it."
  WRONG-KIND-OF-FILE            "Wrong kind of file for this operation"
  ACCESS-ERROR                  "operation denied by access control"
  INCORRECT-ACCESS-TO-FILE      "operation to file denied by access control"
  INCORRECT-ACCESS-TO-DIRECTORY "operation to directory denied by access control"
  )

(DEFPROP UNDELETE-UNCLOSED-FILE FILE-LOCKED ERROR-SIGNALER)

(DEFSIGNAL OPEN-DELETED-FILE FILE-OPERATION-FAILURE (PATHNAME OPERATION)
           "Opening a file that is deleted.")

(DEFSIGNAL OPEN-DELETED-DIRECTORY FILE-LOOKUP-ERROR (PATHNAME OPERATION)
           "Containing directory is deleted.")

(DEFPROP OPEN-OUTPUT-FILE FILE-OPEN-FOR-OUTPUT ERROR-SIGNALER)

(DEFSIGNAL OPEN-UNFINISHED-DIRECTORY FILE-LOOKUP-ERROR (PATHNAME OPERATION)
  "Containing directory is still being created.")

(DEFSIGNAL FILE-IS-SUBDIRECTORY
           (FILE-OPERATION-FAILURE WRONG-KIND-OF-FILE INVALID-OPERATION-FOR-DIRECTORY)
           (PATHNAME OPERATION)
  "Attempt to open a file which is a directory.")

(DEFSIGNAL UNSETTABLE-PROPERTY CHANGE-PROPERTY-FAILURE (PATHNAME PROPERTY)
           "PROPERTY isn't one of the properties this file system allows users to set.")

(DEFSIGNAL VERSION-TOO-LARGE
           (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX VERSION-TOO-LARGE)
           (PATHNAME OPERATION)
  "Version larger that 65536.")

(DEFSIGNAL INVALID-DIRECTORY-NAME
           (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX INVALID-DIRECTORY-NAME)
           (PATHNAME OPERATION)
  "")

(DEFSIGNAL RENAME-DIRECTORY-INVALID-TYPE
           (RENAME-FAILURE RENAME-DIRECTORY-INVALID-TYPE)
           (PATHNAME NEW-PATHNAME)
  "")

(DEFSIGNAL LINKS-NOT-SUPPORTED
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-LINK-FAILURE
                                   LINKS-NOT-SUPPORTED)
           (PATHNAME OPERATION)
  "Links are not supported in this file system.")

(DEFSIGNAL BAD-TYPE-FOR-SUBDIRECTORY
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-DIRECTORY-FAILURE
                                   BAD-TYPE-FOR-SUBDIRECTORY)
           (PATHNAME OPERATION)
  "")

(DEFSIGNAL SUPERSEDE-DIRECTORY
           (FILE-OPERATION-FAILURE CREATION-FAILURE TOP-LEVEL-FILE)
           (PATHNAME OPERATION)
  "Superseding a file which is a subdirectory.")

(DEFSIGNAL TOP-LEVEL-FILE
           (FILE-OPERATION-FAILURE CREATION-FAILURE TOP-LEVEL-FILE)
           (PATHNAME OPERATION)
  "Attempt to create a file not a directory in the root directory.")

;; ALLOCATE-DISK-BLOCK returns the base and size of a block which has pages
;; pre-set to PUT-RESERVED.
;; {VERIFY, CHANGE, SET}-{BLOCK, MAP}-DISK-SPACE.  They should all be called
;; inside the special form USING-PUT.  SET-... should be used minimally.
;; WRITE-PUT (&optional force-p) may be called any time inside a USING-PUT
;; form to force the put to be written onto the disk.  If FORCE-P is nil, and
;; the PUT hasn't been modified in core, this is a no-op.  (WRITE-PUT) is implicit
;; when exitting a USING-PUT.  The USING-PUT special form may be enterred recursively,
;; and in fact usually is.

;; USE-{MAP,BLOCK}-DISK-SPACE??

;; This variable is T if the put has been modified in core and not written to the disk.
(DEFVAR PUT-MODIFIED NIL)

;; This array contains four numbers:  The number of pages in the PUT which
;; have the index as their status.
(DEFVAR PUT-USAGE-ARRAY (MAKE-ARRAY 4 :TYPE 'ART-32B))

;; This is the minimum number of free pages that must be available in order
;; to write files.  Note that internal structures may still be written.
(DEFVAR PUT-MINIMUM-FREE-PAGES 50.)

;; This is bound by LM-WRITE-DIRECTORY to T.  This tells ALLOCATE-DISK-BLOCK
;; that it is OK to go below PUT-MINIMUM-FREE-PAGES.
(DEFVAR WRITING-INTERNAL-STRUCTURES NIL)

(DEFVAR STANDARD-BLOCK-SIZE 20.)

(DEFUN ALLOCATE-DISK-BLOCK (&OPTIONAL (REQUESTED-NPAGES STANDARD-BLOCK-SIZE)
                            &AUX RNP (AVAILABLE 0) FREE-INDEX FREE-NPAGES
                            (PART-SIZE (DC-PARTITION-SIZE)))
  (DO-NAMED ALLOCATE-DISK-BLOCK () (NIL)
    (LOCKING-RECURSIVELY PUT-LOCK
      (COND ((OR WRITING-INTERNAL-STRUCTURES
                 (PLUSP (SETQ AVAILABLE (- (AREF PUT-USAGE-ARRAY PUT-FREE)
                                           PUT-MINIMUM-FREE-PAGES))))
             (SETQ RNP (IF WRITING-INTERNAL-STRUCTURES
                           REQUESTED-NPAGES
                         (MIN REQUESTED-NPAGES AVAILABLE)))
             (SETQ FREE-INDEX
                   (OR (%STRING-SEARCH-CHAR PUT-FREE PAGE-USAGE-TABLE
                                            PUT-SCANNING-INDEX PART-SIZE)
                       (%STRING-SEARCH-CHAR PUT-FREE PAGE-USAGE-TABLE
                                            1 PUT-SCANNING-INDEX)
                       (FERROR NIL "Ran out of disk space on a directory write")))
             (DO ((I FREE-INDEX (1+ I))
                  (P 0 (1+ P)))
                 (NIL)
               (COND ((OR ( P RNP)
                          ( I PART-SIZE)
                          ( (AREF PAGE-USAGE-TABLE I) PUT-FREE))
                      (SETQ PUT-SCANNING-INDEX I)
                      (DECF (AREF PUT-USAGE-ARRAY PUT-FREE) P)
                      (INCF (AREF PUT-USAGE-ARRAY PUT-RESERVED) P)
                      (SETQ FREE-NPAGES P)
                      (RETURN-FROM ALLOCATE-DISK-BLOCK (VALUES FREE-INDEX FREE-NPAGES)))
                     (T (ASET PUT-RESERVED PAGE-USAGE-TABLE I)))))))
    ;; If we get here, the disk is full.  Leave the PUT unlocked so people
    ;; can delete files, and signal a proceedable error.
    (LM-SIGNAL-ERROR 'NO-MORE-ROOM NIL T *CURRENT-FILE-OPERATION*)))

(DEFUN WRITE-PUT (&OPTIONAL FORCE-P)
  (REQUIRE-LOCK PUT-LOCK)
  (COND ((OR PUT-MODIFIED FORCE-P)
         (LM-DISK-WRITE PUT-RQB (DC-PUT-BASE))
         (SETQ PUT-MODIFIED NIL))))

(DEFMACRO USING-PUT (&BODY BODY)
  (LET ((LOCK-OWNED (GENSYM)))
    `(LET ((,LOCK-OWNED (EQ PUT-LOCK CURRENT-PROCESS))
           OLD-STATE)
       (UNWIND-PROTECT
         (PROGN
           (COND ((NOT ,LOCK-OWNED)
                  (PROCESS-LOCK (LOCF PUT-LOCK))
                  (IF (NOT (NULL PUT-MODIFIED))
                      (FERROR NIL "PUT evidently modified while unlocked"))
                  (SETQ OLD-STATE (AREF PAGE-USAGE-TABLE 0))
                  (ASET PUT-INCONSISTENT PAGE-USAGE-TABLE 0)))
           . ,BODY)
         (COND ((NOT ,LOCK-OWNED)
                (ASET OLD-STATE PAGE-USAGE-TABLE 0)
                (WRITE-PUT)
                (PROCESS-UNLOCK (LOCF PUT-LOCK))))))))

(defun maint-reserve-disk-surface-in-file-partition (physical-cylinder head &optional (new-status put-unusable))
  "CYL and HEAD are physical, such as SDU prints on WHY after crash.
Remember, the SDU prints cylinders and heads in DECIMAL"
  (let* ((unit lm-unit)
         (partition-base lm-partition-base)
         (logical-cylinder (- physical-cylinder (si:get-cylinder-offset-for-unit unit)))  ;**verify this is right
                                ;for unit 0 ***
         (SECTORS-PER-TRACK (AREF si:DISK-SECTORS-PER-TRACK-ARRAY UNIT))
         (HEADS-PER-CYLINDER (AREF si:DISK-HEADS-PER-CYLINDER-ARRAY UNIT))
         (physical-block (+ (* logical-cylinder heads-per-cylinder sectors-per-track)
                            (* head sectors-per-track)))
         (partition-block (- physical-block partition-base)))
    (cond ((or (< partition-block 0)
               (>= partition-block (array-length page-usage-table)))
           (ferror "That physical block is not within the file partition")))
    (using-put
      (dotimes (c sectors-per-track)
        (maint-change-block-disk-space (+ partition-block c) new-status)))
    ))

(DEFUN MAINT-CHANGE-BLOCK-DISK-SPACE (LOC NEW-STATUS)
  (REQUIRE-LOCK PUT-LOCK)
  (SETQ PUT-MODIFIED T)
  (let ((old-status (aref page-usage-table loc)))
    (format t "~%changing usage status for block ~o from ~s to ~s" loc old-status new-status)
    (DECF (AREF PUT-USAGE-ARRAY OLD-STATUS) 1)
    (INCF (AREF PUT-USAGE-ARRAY NEW-STATUS) 1)
    (ASET NEW-STATUS PAGE-USAGE-TABLE loc)))

(DEFUN CHANGE-BLOCK-DISK-SPACE (LOC NPAGES OLD-STATUS NEW-STATUS &AUX LIM)
  (REQUIRE-LOCK PUT-LOCK)
  (SETQ LIM (+ LOC NPAGES))
  (AND (PLUSP NPAGES)
       (NOT (AND (= OLD-STATUS PUT-RESERVED) (= NEW-STATUS PUT-FREE)))
       (SETQ PUT-MODIFIED T))
  (DO ((I LOC (1+ I)))
      (( I LIM)
       (DECF (AREF PUT-USAGE-ARRAY OLD-STATUS) NPAGES)
       (INCF (AREF PUT-USAGE-ARRAY NEW-STATUS) NPAGES)
       T)
      (cond ((not (= (AREF PAGE-USAGE-TABLE I) OLD-STATUS))
             (format t "Unexpected disk space status in CHANGE-MAP-DISK-SPACE")
             (aset (max new-status old-status) page-usage-table i))
            (t
             (ASET NEW-STATUS PAGE-USAGE-TABLE I)))))

(DEFUN CHANGE-MAP-DISK-SPACE (MAP OLD-STATUS NEW-STATUS
                              &AUX NBLOCKS LIM LOC COUNT (NPAGES 0))
  (REQUIRE-LOCK PUT-LOCK)
  (SETQ NBLOCKS (MAP-NBLOCKS MAP))
  (AND (PLUSP NBLOCKS)
       (NOT (AND (= OLD-STATUS PUT-RESERVED) (= NEW-STATUS PUT-FREE)))
       (SETQ PUT-MODIFIED T))
  (DO ((MAP-INDEX 0 (1+ MAP-INDEX)))
      (( MAP-INDEX NBLOCKS)
       (DECF (AREF PUT-USAGE-ARRAY OLD-STATUS) NPAGES)
       (INCF (AREF PUT-USAGE-ARRAY NEW-STATUS) NPAGES)
       T)
    (SETQ LOC (MAP-BLOCK-LOCATION MAP MAP-INDEX)
          COUNT (CEILING (MAP-BLOCK-SIZE MAP MAP-INDEX) PAGE-SIZE-IN-BITS)
          LIM (+ LOC COUNT))
    (DO ((I LOC (1+ I)))
        (( I LIM) (INCF NPAGES COUNT))
      (cond ((not (= (AREF PAGE-USAGE-TABLE I) OLD-STATUS))
             (format t "Unexpected disk space status in CHANGE-MAP-DISK-SPACE")
             (aset (max new-status old-status) page-usage-table i))
            (t
             (ASET NEW-STATUS PAGE-USAGE-TABLE I))))))

(DEFUN SET-MAP-DISK-SPACE (MAP NEW-STATUS)
  (REQUIRE-LOCK PUT-LOCK)
  (WHEN (> (MAP-NBLOCKS MAP) 0)
    (SETQ PUT-MODIFIED T)
    (DOTIMES (MAP-INDEX (MAP-NBLOCKS MAP))
      (LET ((COUNT (CEILING (MAP-BLOCK-SIZE MAP MAP-INDEX) PAGE-SIZE-IN-BITS))
            (LOC (MAP-BLOCK-LOCATION MAP MAP-INDEX)))
        (DOTIMES (I COUNT)
          (DECF (AREF PUT-USAGE-ARRAY (AREF PAGE-USAGE-TABLE (+ LOC I))))
          (INCF (AREF PUT-USAGE-ARRAY NEW-STATUS))
          (ASET NEW-STATUS PAGE-USAGE-TABLE (+ LOC I)))))))

(DEFUN DETERMINE-MAP-DISK-SPACE-STATUS (MAP)
  (REQUIRE-LOCK PUT-LOCK)
  (LET ((STATUS NIL))
    (DOTIMES (MAP-INDEX (MAP-NBLOCKS MAP))
      (LET ((COUNT (CEILING (MAP-BLOCK-SIZE MAP MAP-INDEX) PAGE-SIZE-IN-BITS))
            (LOC (MAP-BLOCK-LOCATION MAP MAP-INDEX)))
        (DOTIMES (I COUNT)
          (COND ((NULL STATUS)
                 (SETQ STATUS (AREF PAGE-USAGE-TABLE (+ LOC I))))
                ((NOT (= STATUS (AREF PAGE-USAGE-TABLE (+ LOC I))))
                 (FERROR NIL "Inconsistent status in DETERMINE-MAP-DISK-SPACE-STATUS"))))))
    STATUS))

;; Binding this variable to non-NIL causes auto creation of directories.
;; This is used internally to create directories, but may be bound by things
;; such as magtape restoration programs.
(DEFVAR LM-AUTOMATICALLY-CREATE-DIRECTORIES NIL)

;; This is a variable with all file streams open since boot time.
;; The :OPEN-STREAMS message to a LOCAL-LISPM-HOST returns COPYLIST of this.
(DEFVAR LM-FILE-STREAMS-LIST NIL)

(DEFVAR LM-FILE-STREAMS-LIST NIL)

;;; debuggung stuff - started by dg 3/4/86

(defconst *lmfs-valid-debug-modes* '(:check-directory-maps))

(defconst *lmfs-active-debug-modes* ()
  "This should be a list of active debug modes, or NIL for normal operation.")


;; [HEADER-ID][VERSION][ID-STRING][CDATE][SELF][FLAG][S0-MAP][S1-MAP]
;; 0          28       32         72     76    80    84

;; in version 1, the Sn-MAP's are regular maps.
;; in version 2, the Sn-MAP's are extended maps.
;; in version 3, the hight bits of the FLAG are a generation counter for
;; files created in the directory and the directory entries are extended
;; to include this number.

(DEFVAR NEW-HEADER-FORMAT-VERSION 2)
(DEFVAR NEW-HEADER-ID (STRING-APPEND "_THIS_" "IS_A_DIRECTORY_" "HEADER_"))
(DEFVAR NEW-HEADER-VERSION-OFFSET (LENGTH NEW-HEADER-ID))
(DEFVAR NEW-HEADER-ID-STRING-OFFSET (+ NEW-HEADER-VERSION-OFFSET 4))
(DEFVAR NEW-HEADER-CDATE-OFFSET (+ NEW-HEADER-ID-STRING-OFFSET 40))
(DEFVAR NEW-HEADER-SELF-OFFSET (+ NEW-HEADER-CDATE-OFFSET 4))
(DEFVAR NEW-HEADER-FLAG-OFFSET (+ NEW-HEADER-SELF-OFFSET 4))
(DEFVAR NEW-HEADER-S0-MAP-OFFSET (+ NEW-HEADER-FLAG-OFFSET 4))
(DEFVAR NEW-HEADER-MAX-MAP-BLOCKS (FLOOR (- (FLOOR (- 1024. NEW-HEADER-S0-MAP-OFFSET) 2) 2)
                                         6.))

(DEFUN STRING-GET-32B (ST OFFSET)
  (DPB (AREF ST (+ OFFSET 3))
       (BYTE 8. 24.)
       (DPB (AREF ST (+ OFFSET 2))
            (BYTE 8. 16.)
            (DPB (AREF ST (+ OFFSET 1))
                 (BYTE 8 8)
                 (AREF ST OFFSET)))))

(DEFUN STRING-PUT-32B (ST OFFSET VALUE)
  (SETF (AREF ST OFFSET) (LDB (BYTE 8. 0) VALUE))
  (SETF (AREF ST (+ OFFSET 1)) (LDB (BYTE 8. 8.) VALUE))
  (SETF (AREF ST (+ OFFSET 2)) (LDB (BYTE 8. 16.) VALUE))
  (SETF (AREF ST (+ OFFSET 3)) (LDB (BYTE 8. 24.) VALUE))
  VALUE)

(DEFSTRUCT (DIRHEADER :CONC-NAME :NAMED)
  ID-STRING
  FLAG
  S0-MAP
  S1-MAP)

(DEFMACRO DIRHEADER-FLAG-BIT (H)
  `(LDB ,(BYTE 1 0) (DIRHEADER-FLAG ,H)))

(DEFMACRO DIRHEADER-FLAG-GENERATION-COUNT (H)
  `(LDB ,(BYTE 31 1) (DIRHEADER-FLAG ,H)))

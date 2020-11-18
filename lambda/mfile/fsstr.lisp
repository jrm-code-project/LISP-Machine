;;; -*- Mode: Lisp; Base: 10.; Package: Moby-File-System -*-

;;; All "MAP-" streams are internal to the file system and support only simple
;;; serial IO.  All "MLM-" streams are streams used when opening files.

;(DEFFLAVOR MOBY-MAP-STREAM-MIXIN
;       ((STATUS :OPEN)                 ;:OPEN, :EOF, :BEING-CLOSED, :CLOSED
;        (MAP (MAP-CREATE))                     ;The map this stream is acting upon.
;        (MAP-INDEX 0)                          ;Current index within map.
;        (BYTE-SIZE 8)                          ;The byte size of the STREAM.
;        (RQB (SI:GET-DISK-RQB STANDARD-BLOCK-SIZE)) ;The RQB in which the physical buffer lives.
;        (RQB-VALID-PAGES NIL))                 ;The number of pages of data in RQB
;       ()
;  (:INCLUDED-FLAVORS SI:STREAM)
;  (:INITABLE-INSTANCE-VARIABLES MAP BYTE-SIZE)
;  (:GETTABLE-INSTANCE-VARIABLES BYTE-SIZE))

(DEFFLAVOR MOBY-ARRAY-STREAM-MIXIN
        ((STATUS :OPEN)                 ;:OPEN, :EOF, :BEING-CLOSED, :CLOSED
         (BYTE-SIZE 8)                  ;The byte size of the STREAM.
         (MAP-INDEX 0)
         (PARTITION-HOST)
         (DATA-POINTER)
         (MAPPED-AREA))
        ()
  (:INCLUDED-FLAVORS SI:STREAM)
  (:REQUIRED-INSTANCE-VARIABLES FILE)
  (:INITABLE-INSTANCE-VARIABLES DATA-POINTER BYTE-SIZE PARTITION-HOST)
  (:GETTABLE-INSTANCE-VARIABLES BYTE-SIZE PARTITION-HOST))

(DEFMETHOD (MOBY-ARRAY-STREAM-MIXIN :AFTER :INIT) (&REST IGNORE)
  (SETQ DATA-POINTER (MFILE-DATA-POINTER FILE))
  (SETQ MAPPED-AREA (MOBY-MAPPED-AREA-FOR-SECTION FILE))
  (IF (NULL MAPPED-AREA)
      (let ((dir (send self :direction)))
        (setq mapped-area
              (moby-map-section partition-host file dir nil))))) ;not possible to be
                                ;output and no section-map, so options doesnt matter.

(defun moby-map-section (partition-host file direction options)
 "Returns area-number of mapped section"
 (let* ((mpa (send partition-host :mpa-defstruct))
        (partition-header (mpa-partition-header mpa))
        (host-index (send partition-host :host-index)))
  (COND ((EQ direction :OUTPUT)
         (let* ((a-name (moby-area-name-for-section file))
                (area-number
                  (cond ((boundp a-name)
                         (symbol-value a-name))
                        (t
                         (moby-setup-area a-name
                                          'section
                                          file
                                          partition-header
                                          host-index)))))
           (cond ((null (mfile-map file))
                  (setf (mfile-map file)
                        (moby-make-section-map
                          (ROOT-AREA-OF-PARTITION-HOST PARTITION-HOST)
                          nil                   ;map-moby-handle supplied as nil.
                          file
                          area-number
                          10.
                          (moby-options-from-options options)))))       ;moby-options
;          ;this just adds to the section map, doesnt actually make any regions.
;          ; host-index used just to test whether dataspace part of region map is
;          ; is required (it is iff host is local).
;          (moby-add-region-to-map-and-allocate-namespace
;            (mfile-map file)
;            (aref *area-to-msa* area-number)
;            #o400
;            partition-header)

           ;this causes actual regions to appear for entire map, as updated.
           (moby-store-map-for-area area-number (mfile-map file)
                                    'all nil host-index)        ;consable?
           area-number))
        ((EQ direction :INPUT)
         (let* ((host-index (send partition-host :host-index))
                (area-number
                  (moby-setup-area
                    (moby-area-name-for-section file)
                    'section
                    file
                    partition-header
                    host-index)))
           (moby-store-map-for-area area-number (mfile-map file) 'all nil
            host-index)
           area-number)))
        ))

;(DEFWRAPPER (MOBY-MAP-STREAM-MIXIN :CLOSE) ((&OPTIONAL IGNORE) . BODY)
;  `(AND (NEQ STATUS :CLOSED)
;       (PROG1 (PROGN ,@BODY) (SETQ STATUS :CLOSED))))

(DEFWRAPPER (MOBY-ARRAY-STREAM-MIXIN :CLOSE) ((&OPTIONAL IGNORE) . BODY)
  `(AND (NEQ STATUS :CLOSED)
        (PROG1 (PROGN ,@BODY) (SETQ STATUS :CLOSED))))

;(DEFMETHOD (MOBY-MAP-STREAM-MIXIN :CLOSE) (&OPTIONAL IGNORE)
;  (AND RQB (SI:RETURN-DISK-RQB RQB))
;  (SETQ RQB NIL))

(DEFMETHOD (MOBY-ARRAY-STREAM-MIXIN :CLOSE) (&OPTIONAL IGNORE)
  NIL)

;(DEFFLAVOR MOBY-MAP-INPUT-STREAM-MIXIN
;       ()
;       (MOBY-MAP-STREAM-MIXIN)
;  (:INCLUDED-FLAVORS SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR MOBY-ARRAY-INPUT-STREAM-MIXIN
        ()
        (MOBY-ARRAY-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-INPUT-STREAM))

;(DEFFLAVOR MOBY-MAP-OUTPUT-STREAM-MIXIN
;       ()
;       (MOBY-MAP-STREAM-MIXIN)
;  (:INCLUDED-FLAVORS SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR MOBY-ARRAY-OUTPUT-STREAM-MIXIN
        ()
        (MOBY-ARRAY-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-OUTPUT-STREAM))

;(DEFMETHOD (MOBY-MAP-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (&OPTIONAL IGNORE)
;  (COND ((EQ STATUS :CLOSED)
;        (FERROR NIL "Attempt to get input from ~S, which is closed." SELF)))
;  (LET ((MAP-NBLOCKS (MOBY-MAP-NBLOCKS MAP))
;       SIZE)
;    (COND ((< MAP-INDEX MAP-NBLOCKS)
;          (SETQ STATUS :OPEN)                  ;May have been at :EOF
;          (SETQ SIZE (MOBY-MAP-BLOCK-SIZE MAP MAP-INDEX)
;                RQB-VALID-PAGES (CEILING SIZE PAGE-SIZE-IN-BITS))
;          (MLM-DISK-READ RQB (MOBY-MAP-BLOCK-LOCATION MAP MAP-INDEX) RQB-VALID-PAGES)
;          (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE) 0 (FLOOR SIZE BYTE-SIZE)))
;         ;; If we're out of range, simply call it an :EOF.
;         (T (SETQ STATUS :EOF)
;            NIL))))

(DEFMETHOD (MOBY-ARRAY-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (&OPTIONAL IGNORE)
  (COND ((EQ STATUS :CLOSED)
         (FERROR NIL "Attempt to get input from ~S, which is closed." SELF)))
  (let ((array-nblocks (moby-a-of-a-nblocks DATA-POINTER))
        (size nil))
    (COND ((< map-index array-nblocks)
           (SETQ STATUS :OPEN)                  ;May have been at :EOF
           (setq size (moby-a-of-a-block-size DATA-POINTER map-index))
           (VALUES (aref DATA-POINTER map-index 0)
                   0
                   (floor size byte-size)))
          ;; If we're out of range, simply call it an :EOF.
          (T (SETQ STATUS :EOF)
             NIL))))

;;; This doesn't really discard the buffer (why bother), but does increment
;;; the buffer pointer for the next :NEXT-INPUT-BUFFER message.
;(DEFMETHOD (MOBY-MAP-INPUT-STREAM-MIXIN :DISCARD-INPUT-BUFFER) (IGNORE)
;  (INCF MAP-INDEX))

;; This doesn't really discard the buffer (why bother), but does increment
;; the buffer pointer for the next :NEXT-INPUT-BUFFER message.
(DEFMETHOD (MOBY-ARRAY-INPUT-STREAM-MIXIN :DISCARD-INPUT-BUFFER) (IGNORE)
  (incf map-index))

;;; This message would work, but would not do anything near what
;;; the user would want -- it would just use up more disk space.
;(DEFMETHOD (MOBY-MAP-OUTPUT-STREAM-MIXIN :FORCE-OUTPUT) IGNORE)

;; This message would work, but would not do anything near what
;; the user would want -- it would just use up more disk space.
(DEFMETHOD (MOBY-ARRAY-OUTPUT-STREAM-MIXIN :FORCE-OUTPUT) IGNORE)

;(DEFMETHOD (MOBY-MAP-OUTPUT-STREAM-MIXIN :NEW-OUTPUT-BUFFER) (&AUX LOC)
;  (COND ((EQ STATUS :CLOSED)
;        (FERROR NIL "Attempt to do output on ~S, which is closed." SELF)))
;  (IF (= MAP-INDEX (MOBY-MAP-NBLOCKS MAP))
;      ;; At end: add a new block.
;      (PROGN
;       (MULTIPLE-VALUE (LOC RQB-VALID-PAGES)
;         (ALLOCATE-DISK-BLOCK))
;       (MAP-APPEND-BLOCK MAP LOC 0)
;       (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE)
;               0
;               (FLOOR (* RQB-VALID-PAGES PAGE-SIZE-IN-BITS) BYTE-SIZE)))
;    ;; In the middle: read contents of next existing block
;    ;; so that we can change only the bytes actually output.
;    (LET ((SIZE (MOBY-MAP-BLOCK-SIZE MAP MAP-INDEX)))
;      (SETQ RQB-VALID-PAGES (CEILING SIZE PAGE-SIZE-IN-BITS))
;      (MLM-DISK-READ RQB (MOBY-MAP-BLOCK-LOCATION MAP MAP-INDEX) RQB-VALID-PAGES)
;      (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE) 0 (FLOOR SIZE BYTE-SIZE)))))

(DEFMETHOD (MOBY-ARRAY-OUTPUT-STREAM-MIXIN :NEW-OUTPUT-BUFFER) (&AUX ARRAY)
  (COND ((EQ STATUS :CLOSED)
         (FERROR NIL "Attempt to do output on ~S, which is closed." SELF)))
  (IF (= MAP-INDEX (MOBY-A-OF-A-NBLOCKS DATA-POINTER))
      ;; At end: add a new block.
      (PROGN
        (SETQ ARRAY (ALLOCATE-DISK-ARRAY FILE MAPPED-AREA BYTE-SIZE))
        (ARRAY-OF-ARRAYS-APPEND-ARRAY DATA-POINTER ARRAY 0)
        (VALUES (AREF DATA-POINTER MAP-INDEX 0)
                0
                ;(FLOOR (* RQB-VALID-PAGES PAGE-SIZE-IN-BITS) BYTE-SIZE)
                (ARRAY-LENGTH (AREF DATA-POINTER MAP-INDEX 0))))
    ;; In the middle: read contents of next existing block
    ;; so that we can change only the bytes actually output.
    (LET ((SIZE (MOBY-A-OF-A-BLOCK-SIZE DATA-POINTER MAP-INDEX)))
      (VALUES (AREF DATA-POINTER MAP-INDEX 0) 0 (FLOOR SIZE BYTE-SIZE)))))

(defun allocate-disk-array
       (file mapped-area byte-size &optional (size 200))  ;**small for test purposes
  file  ;not needed since region is in, therefore *moby-page-association* does not point
        ; to section.
  (let* ((type (car (rassq byte-size si:array-bits-per-element))))
    (make-array size :area mapped-area :type type)))

;;; The :NEW-OUTPUT-BUFFER method gets the old data, so this has nothing to do.
;(DEFMETHOD (MOBY-MAP-OUTPUT-STREAM-MIXIN :GET-OLD-DATA) (&REST IGNORE) NIL)

(DEFMETHOD (MOBY-ARRAY-OUTPUT-STREAM-MIXIN :SEND-OUTPUT-BUFFER) (BUFFER TO-INDEX)
  (OR (= MAPPED-AREA (%AREA-NUMBER BUFFER))
      (FERROR NIL "Attempt to :SEND-OUTPUT-BUFFER ~S, in area ~S"
              BUFFER MAPPED-AREA))
  (LET* (;(ARRAY (MOBY-A-OF-A-ARRAY DATA-POINTER MAP-INDEX))
         (SIZE (* TO-INDEX BYTE-SIZE))
         ;(USED-NPAGES (CEILING SIZE PAGE-SIZE-IN-BITS))
         )
    (COND ((ZEROP SIZE)
           (DECF (MOBY-A-OF-A-NBLOCKS DATA-POINTER)))
          (T (WHEN (< (MOBY-A-OF-A-BLOCK-SIZE DATA-POINTER MAP-INDEX) SIZE)
               (SETF (MOBY-A-OF-A-BLOCK-SIZE DATA-POINTER MAP-INDEX) SIZE))
             (INCF MAP-INDEX)))))

;(DEFMETHOD (MOBY-MAP-OUTPUT-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER) (BUFFER)
;  (SEND SELF :SEND-OUTPUT-BUFFER BUFFER 0))

(DEFMETHOD (MOBY-ARRAY-OUTPUT-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER) (BUFFER)
  (SEND SELF :SEND-OUTPUT-BUFFER BUFFER 0))

;;These flavors are not used.
;;(DEFFLAVOR MAP-INPUT-STREAM
;;      ()
;;      (MOBY-MAP-INPUT-STREAM-MIXIN SI:BUFFERED-INPUT-STREAM))
;;
;;(DEFFLAVOR MAP-OUTPUT-STREAM
;;      ()
;;      (MOBY-MAP-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-STREAM))

;;; The next two flavors are used internally by the file system.
;(DEFFLAVOR MOBY-MAP-CHARACTER-INPUT-STREAM
;       ()
;       (MOBY-MAP-INPUT-STREAM-MIXIN SI:BUFFERED-LINE-INPUT-STREAM))

;(DEFFLAVOR MOBY-ARRAY-CHARACTER-INPUT-STREAM
;       ()
;       (MOBY-ARRAY-INPUT-STREAM-MIXIN SI:BUFFERED-LINE-INPUT-STREAM))

;(DEFFLAVOR MOBY-MAP-CHARACTER-OUTPUT-STREAM
;       ()
;       (MOBY-MAP-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

;(DEFFLAVOR MOBY-ARRAY-CHARACTER-OUTPUT-STREAM
;       ()
;       (MOBY-ARRAY-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

;(DEFMETHOD (MOBY-MAP-CHARACTER-OUTPUT-STREAM :MAP) () MAP)

;(DEFMETHOD (MOBY-ARRAY-CHARACTER-OUTPUT-STREAM :DATA-POINTER) () DATA-POINTER)

;;; The next flavor is for input streams opened in an odd byte size.
;(DEFFLAVOR MOBY-ODD-BYTE-SIZE-MIXIN () ()
;  (:REQUIRED-FLAVORS MOBY-MAP-INPUT-STREAM-MIXIN))

;(DEFWRAPPER (MOBY-ODD-BYTE-SIZE-MIXIN :NEXT-INPUT-BUFFER) ((&OPTIONAL IGNORE) . BODY)
;  (LET ((VAL1 (GENTEMP)) (VAL2 (GENTEMP)) (VAL3 (GENTEMP)) (I (GENTEMP)))
;    `(MULTIPLE-VALUE-PROG1 (,VAL1 ,VAL2 ,VAL3)
;        (PROGN . ,BODY)
;       (COND (,VAL1
;             (LOOP FOR ,I FROM ,VAL2 BELOW ,VAL3
;                   DO (ASET (LDB BYTE-SIZE (AREF ,VAL1 ,I)) ,VAL1 ,I))))
;       (VALUES ,VAL1 ,VAL2 ,VAL3))))

(DEFFLAVOR MOBY-LM-STREAM-MIXIN
        (TRUENAME
         PARTITION-HOST)
        (SI:PROPERTY-LIST-MIXIN SI:FILE-STREAM-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES TRUENAME PARTITION-HOST)
  (:INITABLE-INSTANCE-VARIABLES PARTITION-HOST)
  (:INIT-KEYWORDS :FILE :APPEND :TRUENAME))

(DEFMETHOD (MOBY-LM-STREAM-MIXIN :INIT) (PLIST)
  (LET ((FILE (GET PLIST :FILE)))
    (IF FILE
        (SETQ TRUENAME (FILE-TRUENAME FILE)
              SI:PROPERTY-LIST (MBFS-FILE-PROPERTIES FILE))
      (SETQ TRUENAME (GET PLIST :TRUENAME)))))

(DEFMETHOD (MOBY-LM-STREAM-MIXIN :QFASLP) ()
  (GET (LOCF SI:PROPERTY-LIST) :QFASLP))

;; For probes, assume you really mean :LENGTH-IN-BYTES
(DEFMETHOD (MOBY-LM-STREAM-MIXIN :LENGTH) ()
  (GET (LOCF SI:PROPERTY-LIST) :LENGTH-IN-BYTES))

(DEFMETHOD (MOBY-LM-STREAM-MIXIN :PROPERTIES) (&OPTIONAL ERROR-P)
  ERROR-P ; would be quite hard to get an error here
  (VALUES (CONS TRUENAME SI:PROPERTY-LIST) MLM-UNSETTABLE-PROPERTIES))

(DEFFLAVOR MOBY-LM-DATA-STREAM-MIXIN
        (FILE)
        (MOBY-LM-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MOBY-ARRAY-STREAM-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES FILE)
  (:GETTABLE-INSTANCE-VARIABLES FILE))

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :STATUS) () STATUS)

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :DELETE) (&OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :DELETE
    (HANDLING-ERRORS ERROR-P
      (MBFS-DELETE-FILE PARTITION-HOST FILE))))

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :RENAME) (NEW-NAME &OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :RENAME
    (HANDLING-ERRORS ERROR-P
      (MBFS-RENAME-FILE PARTITION-HOST FILE
                        (PATHNAME-DIRECTORY NEW-NAME)
                        (PATHNAME-NAME NEW-NAME)
                        (PATHNAME-TYPE NEW-NAME)
                        (PATHNAME-VERSION NEW-NAME)))
    (SETQ TRUENAME (FILE-TRUENAME FILE))))

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :CHANGE-PROPERTIES) (ERROR-P &REST PROPERTIES)
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (MBFS-CHANGE-FILE-PROPERTIES PARTITION-HOST FILE PROPERTIES))))

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :INIT) (IGNORE)
  (SETQ DATA-POINTER (MFILE-DATA-POINTER FILE)
        TRUENAME (FILE-TRUENAME FILE)
        SI:PROPERTY-LIST (MBFS-FILE-PROPERTIES FILE))
  (WITHOUT-INTERRUPTS
    (SETQ MLM-FILE-STREAMS-LIST
          (CONS SELF MLM-FILE-STREAMS-LIST))))

(DEFMETHOD (MOBY-LM-DATA-STREAM-MIXIN :SET-BUFFER-POINTER) (NEW-POINTER)
  (DO ((NBLOCKS (MOBY-A-OF-A-NBLOCKS DATA-POINTER))
       (I 0 (1+ I))
       (P 0 P1)
       (P1))
      (NIL)
    (COND ((OR ( I NBLOCKS)
               (> (SETQ P1 (+ P (FLOOR (MOBY-A-OF-A-BLOCK-SIZE DATA-POINTER I) BYTE-SIZE)))
                  NEW-POINTER))
           (SETQ MAP-INDEX I)
           (RETURN P))))
  )

(DEFFLAVOR MOBY-LM-INPUT-STREAM-MIXIN
        ()
        (MOBY-LM-DATA-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MOBY-ARRAY-STREAM-MIXIN)
  )

(DEFFLAVOR MOBY-LM-OUTPUT-STREAM-MIXIN
        ()
        (MOBY-LM-DATA-STREAM-MIXIN SI:OUTPUT-FILE-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MOBY-ARRAY-STREAM-MIXIN)
  )

(DEFMETHOD (MOBY-LM-INPUT-STREAM-MIXIN :AFTER :CLOSE) (&OPTIONAL IGNORE)
  (MBFS-CLOSE-FILE FILE)
  (WITHOUT-INTERRUPTS
    (SETQ MLM-FILE-STREAMS-LIST (DELQ SELF MLM-FILE-STREAMS-LIST)))
  (SETQ FILE NIL)
  (SETQ DATA-POINTER NIL))

;; For input streams, :LENGTH may be different from :LENGTH-IN-BYTES
(DEFMETHOD (MOBY-LM-INPUT-STREAM-MIXIN :LENGTH) ()
  (FLOOR (* (GET (LOCF SI:PROPERTY-LIST) :LENGTH-IN-BYTES)
            (GET (LOCF SI:PROPERTY-LIST) :BYTE-SIZE))
         BYTE-SIZE))

(DEFMETHOD (MOBY-LM-OUTPUT-STREAM-MIXIN :AFTER :INIT) (INIT-PLIST)
  (WHEN (GET INIT-PLIST :APPEND)
    (SETQ MAP-INDEX (MOBY-A-OF-A-NBLOCKS DATA-POINTER)))
  )

(DEFMETHOD (MOBY-LM-OUTPUT-STREAM-MIXIN :AFTER :CLOSE) (&OPTIONAL ABORTP)
  (cond ((and abortp (not (MFILE-CLOSED? file)))
         (setf (MFILE-DELETED? file) t)
         (MBFS-expunge-file file)
;        (write-directory-files (mfile-directory file))
         (moby-writeout-root partition-host))
        (t
         (MBFS-close-file file)))
;    (IF (AND ABORTP (NOT (MFILE-CLOSED? FILE)))
;       (MBFS-EXPUNGE-FILE FILE)
;      (MBFS-CLOSE-FILE FILE))
    (WITHOUT-INTERRUPTS
      (SETQ MLM-FILE-STREAMS-LIST (DELQ SELF MLM-FILE-STREAMS-LIST)))
    (SETQ FILE NIL)
    (setq DATA-POINTER NIL)
    )

(DEFFLAVOR MOBY-LM-INPUT-STREAM
        ()
        (MOBY-LM-INPUT-STREAM-MIXIN MOBY-ARRAY-INPUT-STREAM-MIXIN
         SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR MOBY-LM-OUTPUT-STREAM
        ()
        (MOBY-LM-OUTPUT-STREAM-MIXIN MOBY-ARRAY-OUTPUT-STREAM-MIXIN
         SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR MOBY-LM-CHARACTER-INPUT-STREAM
        ()
        (MOBY-LM-INPUT-STREAM-MIXIN MOBY-ARRAY-INPUT-STREAM-MIXIN
         SI:BUFFERED-INPUT-CHARACTER-STREAM))

(DEFFLAVOR MOBY-LM-CHARACTER-OUTPUT-STREAM
        ()
        (MOBY-LM-OUTPUT-STREAM-MIXIN MOBY-ARRAY-OUTPUT-STREAM-MIXIN
         SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFFLAVOR MOBY-LM-PROBE-STREAM
        ()
        (MOBY-LM-STREAM-MIXIN SI:STREAM)
  ;; Kludge so that OPEN can pass this a :BYTE-SIZE :DEFAULT
  (:INIT-KEYWORDS :BYTE-SIZE))

(DEFMETHOD (MOBY-LM-PROBE-STREAM :STATUS) () :CLOSED)
(DEFMETHOD (MOBY-LM-PROBE-STREAM :DIRECTION) () NIL)
(DEFMETHOD (MOBY-LM-PROBE-STREAM :BYTE-SIZE) () (GETF SI:PROPERTY-LIST :BYTE-SIZE))

(COMPILE-FLAVOR-METHODS ;MOBY-ARRAY-CHARACTER-INPUT-STREAM MOBY-ARRAY-CHARACTER-OUTPUT-STREAM
                        MOBY-LM-INPUT-STREAM MOBY-LM-OUTPUT-STREAM
                        MOBY-LM-CHARACTER-INPUT-STREAM MOBY-LM-CHARACTER-OUTPUT-STREAM
                        MOBY-LM-PROBE-STREAM)

;(DEFUN MAKE-MAP-STREAM (FLAVOR &REST INIT-PLIST)
;  (INSTANTIATE-FLAVOR FLAVOR (LOCF INIT-PLIST) NIL NIL MOBY-FILE-SYSTEM-AREA))

;(DEFUN MAKE-ARRAY-STREAM (FLAVOR &REST INIT-PLIST)
;  (INSTANTIATE-FLAVOR FLAVOR (LOCF INIT-PLIST) NIL NIL MOBY-FILE-SYSTEM-AREA))

;(DEFUN MAKE-MAP-STREAM-IN (MAP)
;  (MAKE-MAP-STREAM 'MOBY-MAP-CHARACTER-INPUT-STREAM
;                  :MAP MAP))

;(DEFUN MAKE-ARRAY-STREAM-IN (DATA-POINTER)
;  (MAKE-ARRAY-STREAM 'MOBY-ARRAY-CHARACTER-INPUT-STREAM
;                    :DATA-POINTER DATA-POINTER))

;(DEFUN MAKE-MAP-STREAM-OUT ()
;  (MAKE-MAP-STREAM 'MOBY-MAP-CHARACTER-OUTPUT-STREAM
;                  :MAP (MAP-CREATE 4)))        ;Internal things tend to be small.

;(DEFUN MAKE-ARRAY-STREAM-OUT (PARTITION-HOST)
;  (MAKE-ARRAY-STREAM 'MOBY-ARRAY-CHARACTER-OUTPUT-STREAM
;                       Internal things tend to be small.
;    :DATA-POINTER (ARRAY-OF-ARRAYS-CREATE PARTITION-HOST 4)))

;;; A simple stream for reading and writing the disk configuration.
;;; MOBY-DISK-CONFIGURATION-BUFFER-POINTER must be bound for this stream to work.

;(DEFSELECT (MOBY-DISK-CONFIGURATION-STREAM MOBY-DISK-CONFIGURATION-STREAM-DEFAULT)
;  (:TYI (&OPTIONAL IGNORE)
;    (AREF MOBY-DISK-CONFIGURATION-BUFFER
;         (PROG1 MOBY-DISK-CONFIGURATION-BUFFER-POINTER
;                (INCF MOBY-DISK-CONFIGURATION-BUFFER-POINTER))))
;  (:UNTYI (CHAR)
;    (DECF MOBY-DISK-CONFIGURATION-BUFFER-POINTER)
;    CHAR)
;  (:TYO (CHAR)
;    (ASET CHAR MOBY-DISK-CONFIGURATION-BUFFER
;         (PROG1 MOBY-DISK-CONFIGURATION-BUFFER-POINTER
;                (INCF MOBY-DISK-CONFIGURATION-BUFFER-POINTER)))))

;(DEFUN MOBY-DISK-CONFIGURATION-STREAM-DEFAULT (OP &OPTIONAL ARG1 &REST ARGS)
;  (STREAM-DEFAULT-HANDLER #'MOBY-DISK-CONFIGURATION-STREAM OP ARG1 ARGS))

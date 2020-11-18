;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10 -*-
;;; The guts of the filesystem


;;; STRUCTURE OF THE FILESYSTEM BAND.
;;; 31-Jan-87 16:46:02 -GEORGE CARRETTE
;;;
;;; 24-Bit integers are used quite a bit for block numbers, sizes and offsets.
;;; A number is noted with square brackets [X] and is 3 bytes unless noted.
;;; The numbers are read in a byte at a time, high-byte-first.
;;; Some things are strings terminated by the lispm <RETURN> character #o215.
;;;
;;; FORMAT OF THE DISK CONFIGURATION BLOCK:
;;;
;;; [VERSION][CHECK][SIZE][PUT-BASE][PUT-SIZE]{ROOT-DIRECTORY-MAP}
;;;
;;; A {MAP} is a description of what blocks make up a file.
;;; It is of the form [LENGTH(2 BYTES)][LOCATION0][SIZE0][LOCATION1][SIZE1] ...
;;; The LOCATION is in BLOCKS (1024 bytes) and the SIZE is in BITS.

;;; The PUT (Page Usage Table) is an array of 2-bit quantities
;;; 0 = free block
;;; 1 = reserved
;;; 2 = used
;;; 3 = bad

;;; A directory is a list of directory entries that describe files.
;;;
;;; <filename-as-string><RETURN>
;;; <filetype-as-string><RETURN>
;;; [VERSION][BYTESIZE(1 BYTE)]
;;; <AUTHOR-as-string><RETURN>
;;; [CREATION-DATE(4 bytes)]
;;; {MAP}
;;; [ATTRIBUTES(2 BYTES)]
;;; {PROPERTY-LIST}

;;; There are 7 file attribute bits that are meaningful:
;;; 0  :DONT-DELETE
;;; 1  :CLOSED
;;; 2  :DELETED
;;; 3  :DUMPED
;;; 4  :DONT-REAP
;;; 5  :CHARACTERS
;;; 6  :DIRECTORY

;;; The {PROPERTY-LIST} is
;;; [NUMBER-OF-ENTRIES(1 BYTE)]{STRING}{ENTRY} ....
;;; A {STRING} is [LENGTH(1 BYTE)]<characters...>
;;; An {ENTRY} is [OPCODE(1 BYTE)]{optional-opcode-dependant}
;;; Opcodes are:
;;; 0 = false,                no optional
;;; 1 = true,                 no optional
;;; 2 {STRING}                keyword symbol
;;; 3 {STRING}{STRING}        symbol name and package
;;; 4 [INTEGER]               3 bytes of course.
;;; 5 {STRING}                a string
;;; 6 {READ}                  Must use lisp READ.
;;; 7 {READ}                  Must use lisp READ.

;;; 6 and 7 are rare.

;;; With LM-VERSION > 4 we introduce a new directory format for those directory files
;;; which have the :HEADER-BLOCK attribute.
;;; In which case the first Page of data is of the form:
;;; [HEADER-ID][VERSION][ID-STRING][CDATE][SELF][FLAG][S0-MAP][S1-MAP]
;;; where the HEADER-ID is a 28 character string, the version is a 4 byte number low byte first,
;;; the ID-STRING is 40 characters, the CDATE, SELF and FLAG 4 bytes, and the S0-MAP and S1-MAP
;;; are submaps of the file map for the directory.
;;; These submaps point to lists of directory entries. Since the submaps almost always
;;; have extra space in them the directory structure can be updated in-place on disk,
;;; getting rid of the need to reallocate a new FILE-MAP for the directory and hence
;;; having to write out the superior directory too. The flag is 0 or 1 to indicate
;;; which submap has the valid data. The implementation goes around but also makes
;;; use of some of the previous pure abstractions such as the file map.
;;; NOTE:
;;; The ID-STRING, CDATE, and SELF are presently written
;;; but not looked at. A disk salvager could use these and the HEADER-ID to identify
;;; valid directory header blocks even if the disk label and significant other data
;;; such as the root directory and file system disk configuration block was lost.
;;; The SELF pointer can be used to calibrate disk offsets.
;;; The [S0-MAP] and [S1-MAP] may be "extended" maps. Which are regular maps
;;; followed by an extra word giving the actual MAP-NBLOCKS. That way a map
;;; can have extra spare blocks at the end that wont show up to a map stream.
;;;

;; Booting.  The hairy thing here is that we want to have arbitrary sized partitions.
;; Given that, the PUT has to change size proportionally.

(DEFVAR LMFS-ROOT-DIRECTORY-NAME "Root-Directory")

(DEFUN INITIALIZE-FILE-SYSTEM (&AUX PBASE PSIZE NECESSARY-PUT-SIZE)
  (*CATCH 'BOOT-FILE-SYSTEM
    ;; Attempt to find the LM band first; if can't,
    ;; will try current LM-UNIT/LM-PARTITION.
    (FIND-LM-BAND)
    ;; Now open up the partition for use.
    (MULTIPLE-VALUE (PBASE PSIZE)
      (FIND-DISK-PARTITION-FOR-WRITE LM-PARTITION NIL LM-UNIT))
    ;; User requests abort.
    (WHEN (NULL PBASE)
      (NOTIFY "Aborting initialization of file system in partition ~A." LM-PARTITION)
      (*THROW 'BOOT-FILE-SYSTEM NIL))
    (DISMOUNT-FILE-SYSTEM)
    ;; Setup the DISK-CONFIGURATION variables.
    (SETQ DISK-CONFIGURATION (MAKE-DISK-CONFIGURATION)
          DISK-CONFIGURATION-LOCK NIL
          DISK-CONFIGURATION-RQB (GET-DISK-RQB)
          DISK-CONFIGURATION-BUFFER (GET-RQB-ARRAY DISK-CONFIGURATION-RQB 8))
    ;; From the size of the partition, compute the size of the PUT
    (SETQ NECESSARY-PUT-SIZE (CEILING (* 2 PSIZE) PAGE-SIZE-IN-BITS))
    ;; Setup the PUT variables.
    (SETQ PUT-RQB (CONDITION-BIND ((RQB-TOO-LARGE #'LM-PUT-RQB-TOO-LARGE))
                    (GET-DISK-RQB NECESSARY-PUT-SIZE))
          PAGE-USAGE-TABLE (GET-RQB-ARRAY PUT-RQB 2)
          PUT-LOCK NIL
          PUT-MODIFIED NIL
          PUT-SCANNING-INDEX 1)
    ;; Setup the DISK-CONFIGURATION structure.
    (SETF (DC-VERSION) LM-VERSION)
    (SETQ LM-PARTITION-BASE PBASE)
    (SETF (DC-PARTITION-SIZE) PSIZE)
    ;; Put the PUT at the beginning of the partition for now.
    (SETF (DC-PUT-BASE) 1)
    (SETF (DC-PUT-SIZE) NECESSARY-PUT-SIZE)
    ;; Make an root directory.
    (SETF (DC-ROOT-DIRECTORY)
          (CREATE-NEW-DIRECTORY NIL LMFS-ROOT-DIRECTORY-NAME))
    ;; Now zero the PUT
    (COPY-ARRAY-CONTENTS "" (RQB-BUFFER PUT-RQB))
    ;; Set it to PUT-CONSISTENT before entering USING-PUT
    (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
    (REINITIALIZE-PUT-USAGE-ARRAY)
    (USING-PUT
      (CHANGE-BLOCK-DISK-SPACE 1 NECESSARY-PUT-SIZE PUT-FREE PUT-USED)
      (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
      (when (> (dc-version) 4)
        ;;If we think the root directory has a :header-block, make sure it exists on disk
        (lmfs-write-new-format-directory (dc-root-directory) ""))
      (LOCKING DISK-CONFIGURATION-LOCK
        (LM-WRITE-CONFIGURATION)
        ;; Update the comment to say what's in the partition.
        (UPDATE-PARTITION-COMMENT LM-PARTITION "LM File System" LM-UNIT)))))

(DEFUNP BOOT-FILE-SYSTEM (&AUX PSIZE-FROM-PARTITION ACTUAL-PSIZE
                          NECESSARY-PUT-SIZE ACTUAL-PUT-SIZE
                          TEM-PUT-RQB NEW-PUT-BASE OLD-PUT-BASE
                          OLD-CONFIGURATION-KLUDGE MUST-SALVAGE)
  (*CATCH 'BOOT-FILE-SYSTEM
    (DISMOUNT-FILE-SYSTEM)
    ;; Setup the auxiliary DISK-CONFIGURATION variables.
    (SETQ DISK-CONFIGURATION-LOCK NIL
          DISK-CONFIGURATION-RQB (GET-DISK-RQB)
          DISK-CONFIGURATION-BUFFER (GET-RQB-ARRAY DISK-CONFIGURATION-RQB 8))
    (FIND-LM-BAND)
    ;; Notify if something different than normal behaviour is going on.
    (IF (OR (NOT (EQUAL LM-PARTITION (SECOND (FIRST LM-PARTITION-POSSIBILITIES))))
            (NOT (EQUAL LM-UNIT (FIRST (FIRST LM-PARTITION-POSSIBILITIES)))))
        (NOTIFY "Booting on unit ~D, partition ~A" LM-UNIT LM-PARTITION))
    ;; Setup DISK-CONFIGURATION, but don't make it global until
    ;; everything has been booted without error.
    (SETQ DISK-CONFIGURATION
          (LET ((DISK-CONFIGURATION (MAKE-DISK-CONFIGURATION)))
            ;; Setup the DISK-CONFIGURATION structure.
            (SETQ PSIZE-FROM-PARTITION (LM-READ-CONFIGURATION)
                  ACTUAL-PSIZE (DC-PARTITION-SIZE))
            (COND (( PSIZE-FROM-PARTITION ACTUAL-PSIZE)
                   (NOTIFY "~A Partition has been ~:[compacted~;expanded~] ~D. pages."
                           LM-PARTITION
                           (< PSIZE-FROM-PARTITION ACTUAL-PSIZE)
                           (ABS (- PSIZE-FROM-PARTITION ACTUAL-PSIZE)))))
            ;; Now setup the put.
            (SETQ NECESSARY-PUT-SIZE (CEILING (* 2 ACTUAL-PSIZE)
                                              PAGE-SIZE-IN-BITS)
                  ACTUAL-PUT-SIZE (DC-PUT-SIZE))
            ;; Temporarily make up for old configuration blocks.
            (COND ((AND (= PSIZE-FROM-PARTITION ACTUAL-PSIZE)
                        (< NECESSARY-PUT-SIZE ACTUAL-PUT-SIZE))
                   (NOTIFY "Label has old configuration.")
                   (SETQ OLD-CONFIGURATION-KLUDGE T)))
            (SETQ PUT-RQB (CONDITION-BIND ((RQB-TOO-LARGE #'LM-PUT-RQB-TOO-LARGE))
                            (GET-DISK-RQB NECESSARY-PUT-SIZE))
                  PAGE-USAGE-TABLE (GET-RQB-ARRAY PUT-RQB 2)
                  PUT-LOCK NIL
                  PUT-MODIFIED NIL
                  PUT-SCANNING-INDEX 1)
            (COND ((< ACTUAL-PUT-SIZE NECESSARY-PUT-SIZE)
                   ;; The partition has expanded enough to force an expansion of the PUT.
                   ;; Try and do so...
                   (NOTIFY "Trying to expand Page Usage Table")
                   ;; First setup the new put.
                   (UNWIND-PROTECT
                     (PROGN (SETQ TEM-PUT-RQB (GET-DISK-RQB ACTUAL-PUT-SIZE))
                            (LM-DISK-READ TEM-PUT-RQB (DC-PUT-BASE))
                            ;; This will set the new space to PUT-FREE (0).
                            (COPY-ARRAY-CONTENTS (RQB-BUFFER TEM-PUT-RQB)
                                                 (RQB-BUFFER PUT-RQB)))
                     (RETURN-DISK-RQB TEM-PUT-RQB))
                   (REINITIALIZE-PUT-USAGE-ARRAY)
                   ;; This is what can fail.
                   (SETQ NEW-PUT-BASE (FIND-DISK-BLOCK NECESSARY-PUT-SIZE))
                   (USING-PUT
                     (CHANGE-BLOCK-DISK-SPACE NEW-PUT-BASE NECESSARY-PUT-SIZE
                                              PUT-FREE PUT-USED)
                     (SETQ OLD-PUT-BASE (DC-PUT-BASE))
                     (SETF (DC-PUT-BASE) NEW-PUT-BASE)
                     (SETF (DC-PUT-SIZE) NECESSARY-PUT-SIZE)
                     (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
                     (LOCKING DISK-CONFIGURATION-LOCK
                       (LM-WRITE-CONFIGURATION))
                     (CHANGE-BLOCK-DISK-SPACE OLD-PUT-BASE ACTUAL-PUT-SIZE
                                              PUT-USED PUT-FREE))
                   (SETQ MUST-SALVAGE ( (AREF PAGE-USAGE-TABLE 0) PUT-CONSISTENT)))
                  ((OR (> PSIZE-FROM-PARTITION ACTUAL-PSIZE) OLD-CONFIGURATION-KLUDGE)
                   ;; The partition has compacted.  Make sure nothing was lost.
                   (UNWIND-PROTECT
                     (PROGN (SETQ TEM-PUT-RQB (GET-DISK-RQB ACTUAL-PUT-SIZE))
                            (LM-DISK-READ TEM-PUT-RQB (DC-PUT-BASE))
                            ;; This may not be zero in old configurations.
                            (OR OLD-CONFIGURATION-KLUDGE
                                (COND ((STRING-SEARCH-NOT-CHAR PUT-FREE
                                                               (GET-RQB-ARRAY TEM-PUT-RQB 2)
                                                               ACTUAL-PSIZE)
                                       (RETURN-DISK-RQB TEM-PUT-RQB)
                                       (NOTIFY
                                         "Data was lost compacting file system.  Aborting.")
                                       (*THROW 'BOOT-FILE-SYSTEM NIL)))))
                     (RETURN-DISK-RQB TEM-PUT-RQB))
                   (COND ((< NECESSARY-PUT-SIZE ACTUAL-PUT-SIZE)
                          (NOTIFY "Compacting the Page Usage Table")))
                   (SETQ MUST-SALVAGE (READ-PAGE-USAGE-TABLE))
                   (USING-PUT
                     (SETF (DC-PUT-SIZE) NECESSARY-PUT-SIZE)
                     (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
                     (LOCKING DISK-CONFIGURATION-LOCK
                       (LM-WRITE-CONFIGURATION))
                     (CHANGE-BLOCK-DISK-SPACE (+ (DC-PUT-BASE) NECESSARY-PUT-SIZE)
                                              (- ACTUAL-PUT-SIZE NECESSARY-PUT-SIZE)
                                              PUT-USED PUT-FREE)))
                  (T (SETQ MUST-SALVAGE (READ-PAGE-USAGE-TABLE))))
            (AND MUST-SALVAGE (LM-SALVAGE NIL))
            DISK-CONFIGURATION))))

(DEFUN FIND-DISK-BLOCK (SIZE)
  (PROG ((LIM (DC-PARTITION-SIZE)) (I 1) (COUNT 0))
     L  (COND ((> I LIM)
               (NOTIFY "Can't find a large enough contiguous block for PUT.  Aborting.")
               (*THROW 'BOOT-FILE-SYSTEM NIL))
              ((= (AREF PAGE-USAGE-TABLE I) PUT-FREE)
               (SETQ COUNT (1+ COUNT))
               (COND (( COUNT SIZE) (RETURN (1+ (- I COUNT))))))
              (T (SETQ COUNT 0)))
        (SETQ I (1+ I))
        (GO L)))

(DEFUN LM-PUT-RQB-TOO-LARGE (&REST IGNORE)
  (NOTIFY "Partition is too large to construct PUT-RQB.  Aborting.")
  (*THROW 'BOOT-FILE-SYSTEM NIL))

(DEFUN DISMOUNT-FILE-SYSTEM ()
  ;; Declared later in this file.
  (DECLARE (SPECIAL SALVAGER-TABLE))
  (COND ((BOUNDP 'DISK-CONFIGURATION) ; if we were running before
         (LMFS-CLOSE-ALL-FILES)
         (SAVE-DIRECTORY-TREE ':FIND-ALL)       ;Make sure directories written out.
         (COND ((BOUNDP 'DISK-CONFIGURATION-RQB)
                (RETURN-DISK-RQB DISK-CONFIGURATION-RQB)
                (MAKUNBOUND 'DISK-CONFIGURATION-RQB)))
         (COND ((BOUNDP 'PUT-RQB)
                (RETURN-DISK-RQB PUT-RQB)
                (MAKUNBOUND 'PUT-RQB)))
         (MAKUNBOUND 'DISK-CONFIGURATION)
         (SETQ SALVAGER-TABLE NIL))
        (t ; if we weren't, remove the kludgey LOCAL host
         (undefine-local-file-system-host))))

(defun maybe-dismount-file-system ()
  (when (or (not (boundp 'disk-configuration))
            (global:with-timeout ((* 30 60.) (progn (write-string "Yes (after timeout)" *query-io*) t))
              (yes-or-no-p "      Dismount file system (Yes after thirty seconds) ? ")))
    (dismount-file-system)))

(add-initialization "Maybe Dismount File System" '(maybe-dismount-file-system) '(:gc-system-release))

(DEFUN CRASH-FILE-SYSTEM (&OPTIONAL REBOOT)
  ;; YOU CAN PUT A CALL TO THIS FUNCTION AT ANY
  ;; CRITICAL PLACE IN THE CODE TO SEE WHAT WOULD HAPPEN
  ;; IF THE MACHINE CRASHED AT THAT PLACE.
  ;; -GJC
  (COND ((NOT (YES-OR-NO-P "DO YOU REALLY WANT TO SIMULATE A MACHINE CRASH?"))
         "OK. THEN DONT CALL THIS FUNCTION")
        ((BOUNDP 'DISK-CONFIGURATION)
         (FORMAT T "~&SIMULATING A FILE SYSTEM CRASH~%")
         (DOLIST (F LM-FILE-STREAMS-LIST)
           (RETURN-DISK-RQB (PROG1 (SYMEVAL-IN-INSTANCE F 'RQB)
                                   (SETF (SYMEVAL-IN-INSTANCE F 'RQB) NIL)))
           (SETF (SYMEVAL-IN-INSTANCE F 'FILE) NIL)
           (SETF (SYMEVAL-IN-INSTANCE F 'TRUENAME) NIL)
           (SETF (SYMEVAL-IN-INSTANCE F 'MAP) NIL)
           (SETF (SYMEVAL-IN-INSTANCE F 'STATUS) :CRASHED)
           (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':DELETE-STREAM F))
         (SETQ LM-FILE-STREAMS-LIST NIL)
         (COND ((BOUNDP 'DISK-CONFIGURATION-RQB)
                (RETURN-DISK-RQB DISK-CONFIGURATION-RQB)
                (MAKUNBOUND 'DISK-CONFIGURATION-RQB)))
         (COND ((BOUNDP 'PUT-RQB)
                (RETURN-DISK-RQB PUT-RQB)
                (MAKUNBOUND 'PUT-RQB)))
         (MAKUNBOUND 'DISK-CONFIGURATION)
         (SETQ SALVAGER-TABLE NIL)
         (IF REBOOT
             (BOOT-FILE-SYSTEM)))
        (REBOOT
         (BOOT-FILE-SYSTEM))
        ('ELSE
         "FILE SYSTEM NOT MOUNTED")))

(DEFUN VIEW-FILE-MAP-DATA (FILE)
  ;; useful for debugging.
  (IF (FILE-ATTRIBUTE FILE :HEADER-BLOCK)
      (VIEW-HEADER-BLOCK (MAP-BLOCK-LOCATION (FILE-MAP FILE) 0)))
  (WITH-MAP-STREAM-IN (S (FILE-MAP FILE))
    (STREAM-COPY-UNTIL-EOF S TERMINAL-IO)))

(DEFUN VIEW-HEADER-BLOCK (LOCATION)
  (SI:WITH-DISK-RQB (RQB 1)
    (LM-DISK-READ RQB LOCATION 1)
    (LET ((S (SI:RQB-8-BIT-BUFFER RQB))
          (S0-MAP)(S1-MAP)(V))
      ;; [HEADER-ID][VERSION][ID-STRING][CDATE][SELF][FLAG][S0-MAP][S1-MAP]
      (FORMAT T "~&~A~%" (SUBSTRING S 0 NEW-HEADER-VERSION-OFFSET))
      (FORMAT T "VERSION:  ~D~%" (SETQ V (STRING-GET-32B S NEW-HEADER-VERSION-OFFSET)))
      (FORMAT T "ID:       ~A~%" (STRING-TRIM '(0) (SUBSTRING S NEW-HEADER-ID-STRING-OFFSET NEW-HEADER-CDATE-OFFSET)))
      (FORMAT T "CDATE:    ~A~%" (TIME:PRINT-UNIVERSAL-TIME (STRING-GET-32B S NEW-HEADER-CDATE-OFFSET) NIL))
      (FORMAT T "LOCATION: ~D~%" (STRING-GET-32B S NEW-HEADER-SELF-OFFSET))
      (FORMAT T "FLAG:     ~D~%" (STRING-GET-32B S NEW-HEADER-FLAG-OFFSET))
      (WITH-INPUT-FROM-STRING (SS S :START NEW-HEADER-S0-MAP-OFFSET)
        (SETQ S0-MAP (IF (= V 1) (MAP-READ SS) (EXTENDED-MAP-READ SS)))
        (SETQ S1-MAP (IF (= V 1) (MAP-READ SS) (EXTENDED-MAP-READ SS))))
      (FORMAT T "S0-MAP: ")
      (SHORT-DESCRIBE-MAP S0-MAP)
      (TERPRI)
      (FORMAT T "S0-MAP: ")
      (SHORT-DESCRIBE-MAP S1-MAP)
      (TERPRI))))

(DEFUN SHORT-DESCRIBE-MAP (MAP)
  (FORMAT T "~D pages in ~D of ~D: " (MAP-NPAGES-AVAILABLE MAP) (MAP-NBLOCKS MAP) (ARRAY-DIMENSION MAP 0))
  (DO ((N (ARRAY-DIMENSION MAP 0))
       (J 0 (1+ J))
       (size))
      ((OR (= J N)
           (NULL (SETQ SIZE (MAP-BLOCK-SIZE MAP J)))))
    (FORMAT T "~D ~D " (map-block-location map j) (CEILING size PAGE-SIZE-IN-BITS))))


(DEFSUBST REQUIRE-DISK-CONFIGURATION ()
  (IF (NOT (BOUNDP 'DISK-CONFIGURATION))
      (LM-SIGNAL-ERROR 'NO-FILE-SYSTEM)))

(defvar lm-partition-override nil)
(defvar lm-unit-override nil)

(add-initialization "reset lm overrides"
                    '(setq lm-partition-override nil
                           lm-unit-override nil)
                    '(:before-cold))

(DEFUN SET-LM-BAND (&OPTIONAL (PARTITION "FILE") (UNIT 0))
  (CHECK-TYPE UNIT :FIXNUM)
  (DISMOUNT-FILE-SYSTEM)
  (SETQ LM-PARTITION-POSSIBILITIES
        (CONS (LIST UNIT PARTITION)
              (DELETE (LIST UNIT PARTITION) LM-PARTITION-POSSIBILITIES)))
  (setq lm-partition-override partition)
  (setq lm-unit-override unit)
  (BOOT-FILE-SYSTEM)
  (VALUES LM-UNIT LM-PARTITION))

(DEFUN FIND-LM-BAND ()
  "Try all of the unit-partition pairs in LM-PARTITION-POSSIBILITIES, in
order, and use the first one that exists."
  (cond ((null lm-partition-override)
         (select-processor
           ((:lambda :cadr)
            (setq lm-partition (si:find-file-partition-name))
            (cond ((and (>= (aref lm-partition 0) #/0)
                        (<= (aref lm-partition 0) #/9))
                   (setq lm-unit (- (aref lm-partition 0) #/0)))
                  (t
                   (setq lm-unit 0))))
           (:explorer
             (setq lm-unit (si:explorer-lod-band-logical-unit))
             (setq lm-partition "LFIL")
             (list lm-unit lm-partition))))
        (t
         (setq lm-partition lm-partition-override)
         (setq lm-unit lm-unit-override)))
  (list lm-partition lm-unit))

(DEFUN NOTIFY (FORMAT-STRING &REST REST)
  (FORMAT ERROR-OUTPUT "~&[File System: ")
  (LEXPR-FUNCALL #'FORMAT ERROR-OUTPUT FORMAT-STRING REST)
  (FORMAT ERROR-OUTPUT "]~&"))

;;;; Salvager

(DEFVAR SALVAGER-BAD-ITEMS NIL)
(DEFVAR SALVAGER-TABLE NIL)
(DEFVAR SALVAGER-ERRORS)
(DEFVAR SALVAGER-VERBOSE-P)     ;This being NIL only clamps the long-winded stuff...

(DEFUN LM-SALVAGE (&OPTIONAL (SALVAGER-VERBOSE-P T)
                   &AUX (SIZE (DC-PARTITION-SIZE)) (SALVAGER-ERRORS 0))
  (SETQ SALVAGER-BAD-ITEMS NIL)
  (IF (AND SALVAGER-TABLE (= (ARRAY-ACTIVE-LENGTH SALVAGER-TABLE) SIZE))
      (FILLARRAY SALVAGER-TABLE '(NIL))
    (SETQ SALVAGER-TABLE (MAKE-ARRAY SIZE ':AREA LOCAL-FILE-SYSTEM-AREA)))
  (NOTIFY "Flushing out any unsaved directories")
  (SAVE-DIRECTORY-TREE ':FIND-ALL)
  (USING-PUT
    (LOCKING DISK-CONFIGURATION-LOCK
      (NOTIFY "Beginning salvage")
      (SALVAGER-ADD-BLOCK 0 1 'CONFIGURATION-BLOCK)
      (SALVAGER-ADD-BLOCK (DC-PUT-BASE) (DC-PUT-SIZE) 'PAGE-USAGE-TABLE)
      (LET ((ROOT (DC-ROOT-DIRECTORY)))
        (SALVAGER-ADD-MAP (FILE-MAP ROOT) ROOT)
        (LM-SALVAGE-DIRECTORY ROOT))
      (COND ((ZEROP SALVAGER-ERRORS)
             (NOTIFY "Salvage Completed.  Updating Page Usage Table.")
             (SALVAGE-RECONSTRUCT-PUT)
             (SETQ OLD-STATE PUT-CONSISTENT     ;See USING-PUT
                   PUT-MODIFIED T)
             (NOTIFY "Page Usage Table Updated."))
            (T (NOTIFY "Salvage Completed.  ~D. error~:P; Page Usage Table not updated."
                       SALVAGER-ERRORS))))))

(DEFUN LM-SALVAGE-DIRECTORY (DIR)
  (DOLIST (FILE (READ-DIRECTORY-FILES DIR))
    (COND ((FILE-OVERWRITE-FILE FILE)
           (SETQ FILE (FILE-OVERWRITE-FILE FILE))
           (AND SALVAGER-VERBOSE-P
                (FORMAT ERROR-OUTPUT "~%File overwriting ~A eliminated."
                        (LM-NAMESTRING NIL NIL
                                       (DIRECTORY-NAME DIR)
                                       (FILE-NAME FILE)
                                       (FILE-TYPE FILE)
                                       (FILE-VERSION FILE)))))
          ((NULL (FILE-CLOSED? FILE))
           (FORMAT ERROR-OUTPUT "~%File ~A has not been closed. Closing it."
                   (LM-NAMESTRING NIL NIL
                                  (DIRECTORY-NAME DIR)
                                  (FILE-NAME FILE)
                                  (FILE-TYPE FILE)
                                  (FILE-VERSION FILE)))
           (SET-FILE-ATTRIBUTE FILE ':CLOSED T)))
    (SALVAGER-ADD-MAP (FILE-MAP FILE) FILE)
    (IF (DIRECTORY? FILE)
        (LM-SALVAGE-DIRECTORY FILE))))

(DEFUN SALVAGER-ADD-BLOCK (BBASE NPAGES THING)
  (LOOP WITH LIM = (+ BBASE NPAGES)
        FOR I FROM BBASE BELOW LIM
        when (= (aref page-usage-table i) put-unusable)
        do (format t "~%The file ~s is resident in block ~O(octal), which is recorded as unusable."
                   thing i)
        AS ENTRY = (AREF SALVAGER-TABLE I)
        WHEN ENTRY DO
        (AND SALVAGER-VERBOSE-P
             (FORMAT ERROR-OUTPUT "~%Address ~O contains both ~A and ~A" I ENTRY THING))
        (OR (SYMBOLP ENTRY)
            (MEMQ ENTRY SALVAGER-BAD-ITEMS)
            (PUSH ENTRY SALVAGER-BAD-ITEMS))
        (OR (SYMBOLP THING)
            (MEMQ THING SALVAGER-BAD-ITEMS)
            (PUSH THING SALVAGER-BAD-ITEMS))
        (INCF SALVAGER-ERRORS)
        ELSE DO (ASET THING SALVAGER-TABLE I)))

(DEFUN SALVAGER-ADD-MAP (MAP THING)
  (LOOP WITH NBLOCKS = (MAP-NBLOCKS MAP)
        FOR I FROM 0 BELOW NBLOCKS
        DO (SALVAGER-ADD-BLOCK (MAP-BLOCK-LOCATION MAP I)
                               (CEILING (MAP-BLOCK-SIZE MAP I) PAGE-SIZE-IN-BITS)
                               THING)))

(DEFUN SALVAGE-RECONSTRUCT-PUT (&AUX NEW-PUT-RQB)
  (UNWIND-PROTECT
    (PROGN
      (SETQ NEW-PUT-RQB (GET-DISK-RQB (RQB-NPAGES PUT-RQB)))
      (LET ((NEW-PUT (GET-RQB-ARRAY NEW-PUT-RQB 2))
            (NEW-PUA (MAKE-ARRAY 4 ':TYPE 'ART-32B)))
        (COPY-ARRAY-CONTENTS "" (RQB-BUFFER NEW-PUT-RQB))
        (SETF (AREF NEW-PUA PUT-FREE) 0)
        (SETF (AREF NEW-PUA PUT-RESERVED) 0)
        (SETF (AREF NEW-PUA PUT-USED) 0)
        (SETF (AREF NEW-PUA PUT-UNUSABLE) 0)
        (ASET PUT-CONSISTENT NEW-PUT 0)
        (DOTIMES (I (DC-PARTITION-SIZE))
          (LET ((OLD-STATUS (AREF PAGE-USAGE-TABLE I))
                (THING (AREF SALVAGER-TABLE I)))
            (LET ((NEW-STATUS
                    (COND ((EQ OLD-STATUS PUT-UNUSABLE) PUT-UNUSABLE)
                          ((NULL THING) PUT-FREE)
                          ((SYMBOLP THING) PUT-USED)
                          ((FILE-DELETED? THING) PUT-RESERVED)
                          (T PUT-USED))))
              (ASET NEW-STATUS NEW-PUT I)
              (INCF (AREF NEW-PUA NEW-STATUS))
              (COND ((AND SALVAGER-VERBOSE-P
                          (NOT (= OLD-STATUS NEW-STATUS)))
                     (FORMAT ERROR-OUTPUT
                             "~%Address ~7O ~[free~;reserved~;used~;bad~] ==> ~
                                ~[free~;reserved~;used~;bad~]~@[  (~A)~]"
                             I OLD-STATUS NEW-STATUS THING))))))
        ;; This stuff isn't long-winded.
        (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
        (DOTIMES (I 4)
          (COND (( (AREF NEW-PUA I) (AREF PUT-USAGE-ARRAY I))
                 (FORMAT ERROR-OUTPUT
                         "~%Pages ~[free~;reserved~;used~;bad~]: ~D ==> ~D"
                         I (AREF PUT-USAGE-ARRAY I) (AREF NEW-PUA I)))))
        (WITHOUT-INTERRUPTS
          (PSETQ PUT-RQB NEW-PUT-RQB
                 PAGE-USAGE-TABLE NEW-PUT
                 PUT-USAGE-ARRAY NEW-PUA
                 NEW-PUT-RQB PUT-RQB))))
    (RETURN-DISK-RQB NEW-PUT-RQB)))

;;;; Read/Write Configuration

;;; The locking order for using LM-WRITE-CONFIGURATION is:
;;;
;;; 1. Lock PUT-LOCK (use USING-PUT).
;;; 2. Lock DISK-CONFIGURATION-LOCK.
;;;
;;; Make SURE that it is done in this order!!
;;; As far as I know, this is the only case when it is necessary for
;;; a single process to lock two things.

;; Only called when booting -- No locks.  Returns the number of pages which the
;; configuration block thought made up the partition, so that BOOT-FILE-SYSTEM
;; can check the consistency.
(DEFUN LM-READ-CONFIGURATION (&AUX PBASE PSIZE)
  (LET ((DISK-CONFIGURATION-BUFFER-POINTER 0))
    (MULTIPLE-VALUE (PBASE PSIZE)
      (FIND-DISK-PARTITION LM-PARTITION NIL LM-UNIT))
    (COND ((NULL PBASE)
           (NOTIFY "No ~A partition.  Aborting." LM-PARTITION)
           (*THROW 'BOOT-FILE-SYSTEM NIL)))
    (SETQ LM-PARTITION-BASE PBASE)
    (SETF (DC-PARTITION-SIZE) PSIZE)
    (LM-DISK-READ DISK-CONFIGURATION-RQB 0)
    (SETF (DC-VERSION) (GET-BYTES #'DISK-CONFIGURATION-STREAM 3))
    (UNLESS ( LM-MINIMUM-VERSION (DC-VERSION) LM-VERSION)
      (NOTIFY "Invalid version ~D.  Aborting." (DC-VERSION))
      (*THROW 'BOOT-FILE-SYSTEM NIL))
    (COND ((NOT (= (GET-BYTES #'DISK-CONFIGURATION-STREAM 3) 0))
           (NOTIFY "Invalid second word in configuration.  Aborting.")
           (*THROW 'BOOT-FILE-SYSTEM NIL)))
    (LET ((PARTITION-SIZE (GET-BYTES #'DISK-CONFIGURATION-STREAM 3)))
      (SETF (DC-PUT-BASE) (GET-BYTES #'DISK-CONFIGURATION-STREAM 3))
      (SETF (DC-PUT-SIZE) (GET-BYTES #'DISK-CONFIGURATION-STREAM 3))
      (LET ((ROOT-DIRECTORY (CREATE-NEW-DIRECTORY NIL LMFS-ROOT-DIRECTORY-NAME)))
        (IF (= (DC-VERSION) 3)
            (SETUP-OLD-ROOT-DIRECTORY ROOT-DIRECTORY (MAP-READ #'DISK-CONFIGURATION-STREAM))
          (SETUP-NEW-ROOT-DIRECTORY ROOT-DIRECTORY (MAP-READ #'DISK-CONFIGURATION-STREAM)))
        (SETF (DC-ROOT-DIRECTORY) ROOT-DIRECTORY))
      PARTITION-SIZE)))

(DEFUN LM-WRITE-CONFIGURATION ()
  (REQUIRE-LOCK PUT-LOCK)
  (REQUIRE-LOCK DISK-CONFIGURATION-LOCK)
  (LET ((DISK-CONFIGURATION-BUFFER-POINTER 0)
        (VERSION (DC-VERSION))
        (PARTITION-SIZE (DC-PARTITION-SIZE))
        (PUT-BASE (DC-PUT-BASE))
        (PUT-SIZE (DC-PUT-SIZE)))
    (PUT-BYTES #'DISK-CONFIGURATION-STREAM 3 VERSION)
    (PUT-BYTES #'DISK-CONFIGURATION-STREAM 3 0) ;Obsolete.
    (PUT-BYTES #'DISK-CONFIGURATION-STREAM 3 PARTITION-SIZE)
    (PUT-BYTES #'DISK-CONFIGURATION-STREAM 3 PUT-BASE)
    (PUT-BYTES #'DISK-CONFIGURATION-STREAM 3 PUT-SIZE)
    (MAP-WRITE #'DISK-CONFIGURATION-STREAM (FILE-MAP (DC-ROOT-DIRECTORY)))
    (WRITE-PUT)
    (LM-DISK-WRITE DISK-CONFIGURATION-RQB 0)))

;;; New, consistent directory structure.
;;; This is completely incompatible with the old structure.
;;; Install when it is reasonable to do so.


(DEFUN SETUP-NEW-ROOT-DIRECTORY (ROOT-DIRECTORY MAP)
  (IF (> (DC-VERSION) 4)
      ;; must infer this from the DC-VERSION since
      ;; there is no "directory-entry" for the root-directory.
      (SETF (FILE-ATTRIBUTE ROOT-DIRECTORY :HEADER-BLOCK) T))
  (SETF (FILE-MAP ROOT-DIRECTORY) MAP)
  (SETF (DIRECTORY-FILES ROOT-DIRECTORY) ':DISK))

;;; Old, inconsistent directory structure.
;;; This restricts the root directory to contain only subdirectories.
;;; Flush whenever possible.

(DEFVAR DMM-CHECK 132435. "Checkword for obsolete Directory Map Map concept.")

(DEFUN SETUP-OLD-ROOT-DIRECTORY (ROOT-DIRECTORY DMM)
  (LET ((FILES '()))
    (WITH-MAP-STREAM-IN (STREAM DMM)
      (LET ((CHECKWORD (GET-BYTES STREAM 3)))
        (IF (NOT (= CHECKWORD DMM-CHECK))
            (FERROR NIL "Directory Map Map is garbage")))
      (DO-FOREVER
        (LET ((FILE (READ-OLD-ROOT-DIRECTORY-ENTRY STREAM ROOT-DIRECTORY)))
          (IF (NULL FILE)
              (RETURN)
              (PUSH FILE FILES)))))
    (SETF (FILE-MAP ROOT-DIRECTORY) DMM)
    (SETF (DIRECTORY-FILES ROOT-DIRECTORY) (NREVERSE FILES))))

(DEFUN GET-OLD-ROOT-DIRECTORY-MAP (ROOT-DIRECTORY)
  (LET ((WRITING-INTERNAL-STRUCTURES T))
    (WITH-MAP-STREAM-OUT (STREAM)
      (PUT-BYTES STREAM 3 DMM-CHECK)
      (DOLIST (DIRECTORY (DIRECTORY-FILES ROOT-DIRECTORY))
        (WRITE-OLD-DIRECTORY-ENTRY STREAM DIRECTORY)))))

(DEFUN READ-OLD-ROOT-DIRECTORY-ENTRY (STREAM DIRECTORY)
  (MULTIPLE-VALUE-BIND (NAME EOF?)
      (SEND STREAM ':LINE-IN T)
    (AND (NOT EOF?)
         (LET ((SUBDIRECTORY (CREATE-NEW-DIRECTORY DIRECTORY NAME)))
           (SETF (DIRECTORY-MAP SUBDIRECTORY) (MAP-READ STREAM))
           (SETF (DIRECTORY-FILES SUBDIRECTORY) ':DISK)
           SUBDIRECTORY))))

(DEFUN WRITE-OLD-DIRECTORY-ENTRY (STREAM DIRECTORY)
  (SEND STREAM ':LINE-OUT (DIRECTORY-NAME DIRECTORY))
  (MAP-WRITE STREAM (DIRECTORY-MAP DIRECTORY)))

;;;; Page Usage Table

;; Only called when booting -- No locks.
(DEFUN READ-PAGE-USAGE-TABLE (&AUX MUST-SALVAGE)
  (LM-DISK-READ PUT-RQB (DC-PUT-BASE))
  (SELECT (AREF PAGE-USAGE-TABLE 0)
    (PUT-CONSISTENT)
    (PUT-INCONSISTENT
     ;; Don't salvage here!!  No directories exist yet.
     (SETQ MUST-SALVAGE T))
    (OTHERWISE
     (CERROR T NIL NIL "Invalid state designator in page usage table")
     (SETQ MUST-SALVAGE T)))
  (REINITIALIZE-PUT-USAGE-ARRAY)
  MUST-SALVAGE)

(DEFUN REINITIALIZE-PUT-USAGE-ARRAY ()

; old definition, simple and direct
;
;(DEFUN REINITIALIZE-PUT-USAGE-ARRAY ()
;  (COPY-ARRAY-CONTENTS "" PUT-USAGE-ARRAY)
;  (DOTIMES (I (DC-PARTITION-SIZE))
;    (INCF (AREF PUT-USAGE-ARRAY (AREF PAGE-USAGE-TABLE I)))))
;   New definition, does about 1/4'th as many arefs because for the inner loop
;   we set up an ART-8B array rather than an ART-2B array to deal with.

  (let (a8b
        (accumulator (make-array 256. :initial-element 0))
        )
    (do ((final-array page-usage-table)
         (index-offset-in-bytes 0)
         )
        ((not (array-displaced-p final-array))
         (setq a8b (make-array (floor (dc-partition-size) 4)
                               :type :art-8b
                               :displaced-to final-array
                               :displaced-index-offset index-offset-in-bytes
                               )))
      (if (not (memq (array-type final-array)
                     '(art-1b art-2b art-4b art-8b art-16b art-32b)))
          (ferror nil "can't handle this array"))

      (incf index-offset-in-bytes
            (floor (* (si:array-index-offset final-array)
                      (cdr (assq (array-type final-array) array-bits-per-element)))
                   8))

      (setq final-array (si:array-indirect-to final-array))
      )


    (dotimes (i (array-length a8b))
      (incf (aref accumulator (aref a8b i))))

    (array-initialize put-usage-array 0)

    (do ((i 0 (1+ i))
         (pua put-usage-array))
        ((>= i (array-length accumulator)))
      (let ((value (aref accumulator i)))
        (incf (aref pua (ldb (byte 2 0) i)) value)
        (incf (aref pua (ldb (byte 2 2) i)) value)
        (incf (aref pua (ldb (byte 2 4) i)) value)
        (incf (aref pua (ldb (byte 2 6) i)) value)))
    ))



;;;; Read/Write Directories

;;; A directory is marked as needing rewriting by marking it, and then
;;; recursively marking all of its superdirectories up to the root.
;;; If this loop is aborted out of, then the change made to that directory
;;; is temporarily lost, unless we are willing to search the entire tree.
;;; For this reason we mark the tree while interrupts are off.

;;; Once the directory has been marked in this way, a treewalk is started
;;; at the root, which identifies the changed directories by walking top-down,
;;; and then saves them walking bottom-up.  Normally we only want to search
;;; down those paths that are marked as modified.  However, an optional
;;; argument is provided to force the saving process to walk the entire tree,
;;; thus guaranteeing that all modified directories are saved even if they
;;; were improperly marked.

(DEFUN WRITE-DIRECTORY-FILES (DIRECTORY)
  (COND ((> (DC-VERSION) 4)
         (NEW-WRITE-DIRECTORY-FILES DIRECTORY))
        ('ELSE
         (OLD-WRITE-DIRECTORY-FILES DIRECTORY))))

(DEFUN WRITE-DIRECTORY-OF-FILE (FILE)
  (LOCKING (FILE-LOCK FILE)
    (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))))


(DEFUN OLD-WRITE-DIRECTORY-FILES (DIRECTORY)
  "Mark all of the directories from here to the root as needing to
be written out, then treewalk the structure, saving them."
  (WITHOUT-INTERRUPTS
    (DO ((DIR DIRECTORY (FILE-DIRECTORY DIR)))
        ((NULL DIR))
      (SETF (FILE-CLOSED? DIR) NIL)))
  (SAVE-DIRECTORY-TREE NIL))

(DEFUN NEW-WRITE-DIRECTORY-FILES (DIRECTORY)
  ;; instead of marking from inside out and then saving
  ;; from outside in we actually save from inside out,
  ;; but only write new directories if needed.
  (HANDLING-ILLEGAL-ABORTIONS "critical filesystem update"
                              #'(lambda (from)
                                  (DO ((DIR FROM (FILE-DIRECTORY DIR))
                                       (OLD-MAPS))
                                      ((NULL DIR)
                                       ;; LIKE OLD CASE, GOT ALL THE WAY TO THE TOP
                                       (USING-PUT
                                         (DOLIST (MAP OLD-MAPS)
                                           (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-FREE))
                                         (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
                                         (LOCKING DISK-CONFIGURATION-LOCK
                                           (LM-WRITE-CONFIGURATION))))
                                    (SETF (FILE-CLOSED? DIR) NIL)
                                    (LET ((OLD-MAP (LMFS-UPDATE-DIRECTORY DIR)))
                                      (COND (OLD-MAP
                                             (PUSH OLD-MAP OLD-MAPS))
                                            (OLD-MAPS
                                             (USING-PUT
                                               (DOLIST (MAP OLD-MAPS)
                                                 (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-FREE))
                                               (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
                                               (WRITE-PUT))
                                             (RETURN NIL))
                                            ('ELSE
                                             (RETURN NIL))))))
                              DIRECTORY))



(DEFRESOURCE DIRECTORY-DATA-STRING ()
  :CONSTRUCTOR (MAKE-ARRAY 100000. :TYPE 'ART-STRING :FILL-POINTER 0 :ADJUSTABLE T)
  :INITIALIZER (SETF (FILL-POINTER OBJECT) 0))


(DEFUN LMFS-UPDATE-DIRECTORY (DIRECTORY &AUX TMP)
  (LOCKING (DIRECTORY-LOCK DIRECTORY)
    (USING-RESOURCE (ENTRIES-STRING DIRECTORY-DATA-STRING)
      (WITH-OUTPUT-TO-STRING (STREAM ENTRIES-STRING)
        (DOLIST (FILE (DIRECTORY-FILES DIRECTORY))
          (WRITE-DIRECTORY-ENTRY STREAM FILE)))
      (COND ((> (LENGTH ENTRIES-STRING)
                (* NEW-HEADER-MAX-MAP-BLOCKS 1024.))
             ;; This is a worst-case-based test assuming no contigous pages are available to be allocated.
             ;; Files take an average of 50 to 100 bytes per directory entrie.
             ;; In any case though a directory this big (over 700 big files) (None exist at LMI at this time)
             ;; wont benefit by the present simple in-place update much anway.
             (SETF (FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK) NIL)
             (LMFS-WRITE-DIRECTORY DIRECTORY))
            ((OR (NOT (FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK))
                 (> (LENGTH ENTRIES-STRING)
                    (SETQ TMP (* 1024.
                                 (MAP-NPAGES-AVAILABLE (IF (= 1 (DIRHEADER-FLAG (SETQ TMP (GETF (FILE-PLIST DIRECTORY)
                                                                                                '%HEADER-INTERNAL))))
                                                           (DIRHEADER-S0-MAP TMP)
                                                         (DIRHEADER-S1-MAP TMP))))))
                 (< (LENGTH ENTRIES-STRING) (- (FLOOR TMP 2) 1024.)))
             (LMFS-WRITE-NEW-FORMAT-DIRECTORY DIRECTORY ENTRIES-STRING))
            ('ELSE
             (LMFS-UPDATE-NEW-FORMAT-DIRECTORY DIRECTORY ENTRIES-STRING))))))


(DEFVAR *1K-BYTE-STRING* (MAKE-STRING 1024.))

(DEFUN LMFS-WRITE-NEW-FORMAT-DIRECTORY (DIRECTORY ENTRIES-STRING)
  (LET ((OLD-MAP (file-map directory))
        (padding (if (zerop (MOD (length entries-string) 1024.))
                     0
                   (- 1024. (MOD (length entries-string) 1024.))))
        (new-map)
        (S0-MAP)(S1-MAP)
        (H (OR (GETF (file-plist directory) '%HEADER-INTERNAL)
               (SETF (GETF (file-plist directory) '%HEADER-INTERNAL)
                     (MAKE-DIRHEADER ID-STRING (DIRECTORY-ID-STRING DIRECTORY))))))
    (setq new-map (let ((writing-internal-structures t))
                    (WITH-MAP-STREAM-OUT (STREAM)
                      (send stream :string-out *1k-byte-string*)
                      (send stream :string-out ENTRIES-STRING)
                      (send stream :string-out *1k-byte-string* 0 padding)
                      (send stream :string-out ENTRIES-STRING)
                      (send stream :string-out *1k-byte-string* 0 padding))))
    (SETQ S0-MAP (make-submap new-map
                              1
                              (* (length entries-string) 8.)))
    (SETQ S1-MAP (make-submap new-map
                              (1+ (ceiling (length entries-string) 1024.))
                              (* (length entries-string) 8.)))
    ;; SHOULD CHECK TO SEE HERE THAT THE MAPS WILL FIT IN THE HEADER.
    ;; MAX IS TWO MAPS OF 78 ENTRIES EACH.
    (WRITE-DIRHEADER (MAP-BLOCK-LOCATION NEW-MAP 0) (DIRHEADER-ID-STRING H) 0 S0-MAP S1-MAP)
    (SETF (FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK) T)
    (SETF (DIRHEADER-FLAG H) 0)
    (SETF (DIRHEADER-S0-MAP H) S0-MAP)
    (SETF (DIRHEADER-S1-MAP H) S1-MAP)
    (SETF (MAP-OWNER NEW-MAP) DIRECTORY)
    (SETF (FILE-CLOSED? DIRECTORY) T)
    (SETF (FILE-MAP DIRECTORY) NEW-MAP)
    (USING-PUT
      (CHANGE-MAP-DISK-SPACE NEW-MAP PUT-RESERVED PUT-USED))
    OLD-MAP))


(DEFUN LMFS-UPDATE-NEW-FORMAT-DIRECTORY (DIRECTORY ENTRIES-STRING)
  (LET* ((H (GETF (FILE-PLIST DIRECTORY) '%HEADER-INTERNAL))
         (FLAG (LOGXOR 1 (DIRHEADER-FLAG H)))
         (S0-MAP (DIRHEADER-S0-MAP H))
         (S1-MAP (DIRHEADER-S1-MAP H))
         (SUBMAP (IF (= FLAG 0) S0-MAP S1-MAP)))
    (SI:WITH-DISK-RQB (RQB STANDARD-BLOCK-SIZE)
      (DO ((J 0 (1+ J))
           (I 0)
           (M (LENGTH ENTRIES-STRING))
           (S (SI:RQB-8-BIT-BUFFER RQB))
           (SIZE)(BSIZE))
          ((= I M)
           (SETF (MAP-NBLOCKS SUBMAP) J))
        (SETQ SIZE (CEILING (MAP-BLOCK-SIZE SUBMAP J) PAGE-SIZE-IN-BITS))
        (SETQ BSIZE (MIN (- M I) (* SIZE 1024.)))
        (COPY-ARRAY-PORTION ENTRIES-STRING I (+ I BSIZE)
                            S              0 BSIZE)
        (LM-DISK-WRITE RQB (MAP-BLOCK-LOCATION SUBMAP J) SIZE)
        (INCF I BSIZE)
        (SETF (MAP-BLOCK-SIZE SUBMAP J) (* BSIZE 8.))))
    (WRITE-DIRHEADER (MAP-BLOCK-LOCATION (FILE-MAP DIRECTORY) 0) (DIRHEADER-ID-STRING H) FLAG S0-MAP S1-MAP)
    (SETF (DIRHEADER-FLAG H) FLAG)
    (SETF (FILE-CLOSED? DIRECTORY) T)
    ;; RETURN NIL, NO OLD MAP TO THROW AWAY.
    ()))

(DEFUN MAKE-SUBMAP (MAP START-PAGE LENGTH)
  (cond ((zerop length)
         ;; need to special case length of ZERO, which is what we
         ;; get when we delete all the files from a directory.
         (map-create-from-list nil))
        ('else
         (DO ((J 0 (1+ J))
              (N (MAP-NBLOCKS MAP))
              (SKIPPED 0)
              (SIZE))
             ((= J N)
              (FERROR NIL "GOT TO END OF MAP"))
           (SETQ SIZE (CEILING (MAP-BLOCK-SIZE MAP J) PAGE-SIZE-IN-BITS))
           (COND ((> (+ SIZE SKIPPED) START-PAGE)
                  (LET ((A (- START-PAGE SKIPPED))
                        (B (MIN (* PAGE-SIZE-IN-BITS (- SIZE (- START-PAGE SKIPPED)))
                                LENGTH)))
                    (RETURN (DO ((L (NCONS (CONS (+ (MAP-BLOCK-LOCATION MAP J) A) B)))
                                 (ACCUMULATED B))
                                ((= LENGTH ACCUMULATED)
                                 (MAP-CREATE-FROM-LIST (NREVERSE L)))
                              (INCF J)
                              (IF (= J N) (FERROR NIL "GOT TO END OF MAP"))
                              (SETQ SIZE (MIN (* (CEILING (MAP-BLOCK-SIZE MAP J) PAGE-SIZE-IN-BITS) PAGE-SIZE-IN-BITS)
                                              (- LENGTH ACCUMULATED)))
                              (PUSH (CONS (MAP-BLOCK-LOCATION MAP J) SIZE) L)
                              (INCF ACCUMULATED SIZE)))))
                 ('ELSE
                  (INCF SKIPPED SIZE)))))))

(DEFUN MAP-CREATE-FROM-LIST (L)
  (DO ((MAP (MAP-CREATE (LENGTH L) T))
       (J 0 (1+ J))
       (L L (CDR L)))
      ((NULL L)
       MAP)
    (SETF (MAP-BLOCK-LOCATION MAP J) (CAAR L))
    (SETF (MAP-BLOCK-SIZE MAP J) (CDAR L))))


(defun write-dirheader (LOCATION ID flag s0-map s1-map)
  ;; [HEADER-ID][VERSION][ID-STRING][CDATE][SELF][FLAG][S0-MAP][S1-MAP]
  (SI:WITH-DISK-RQB (RQB 1)
    (let ((S (si:rqb-8-bit-buffer rqb)))
      (COPY-ARRAY-PORTION NEW-HEADER-ID 0 NEW-HEADER-VERSION-OFFSET S 0 NEW-HEADER-VERSION-OFFSET)
      (STRING-PUT-32B S NEW-HEADER-VERSION-OFFSET NEW-HEADER-FORMAT-VERSION)
      (COPY-ARRAY-PORTION ID 0 (LENGTH ID) S NEW-HEADER-ID-STRING-OFFSET NEW-HEADER-CDATE-OFFSET)
      (STRING-PUT-32B S NEW-HEADER-CDATE-OFFSET (TIME:GET-UNIVERSAL-TIME))
      (STRING-PUT-32B S NEW-HEADER-SELF-OFFSET LOCATION)
      (STRING-PUT-32B S NEW-HEADER-FLAG-OFFSET FLAG)
      (with-output-to-string (stream (make-array (- 1024 NEW-HEADER-S0-MAP-OFFSET)
                                                 :type 'art-string
                                                 :fill-pointer 0
                                                 :displaced-to S
                                                 :displaced-index-offset NEW-HEADER-S0-MAP-OFFSET))
        (COND ((= NEW-HEADER-FORMAT-VERSION 1)
               (MAP-WRITE STREAM S0-MAP)
               (MAP-WRITE STREAM S1-MAP))
              ('ELSE
               (EXTENDED-MAP-WRITE STREAM S0-MAP)
               (EXTENDED-MAP-WRITE STREAM S1-MAP))))
      (LM-DISK-WRITE RQB LOCATION 1))))


(DEFUN HANDLING-ILLEGAL-ABORTIONS (REASON F &REST L)
  ;; USED FOR CRITICAL OPERATIONS. NOT A CURE-ALL
  (LET ((ABORTIONP (LIST NIL)))
    (PROG1 (CONDITION-BIND ((SI:ABORT 'HANDLE-ILLEGAL-ABORTION REASON ABORTIONP))
             (APPLY F L))
           (WHEN (CAR ABORTIONP)
             (CERROR "continue normally" "Condition signalled during ~A: ~S"
                     REASON
                     (CAR ABORTIONP))))))

(DEFUN HANDLE-ILLEGAL-ABORTION (CONDITION REASON ABORTIONP)
  (SETF (CAR ABORTIONP) CONDITION)
  (CERROR "finish critical code"
          "Abort is illegal now because of ~A" REASON)
  :ASYNCHRONOUS-CONDITION-RETURN)

(DEFUN SAVE-DIRECTORY-TREE (&OPTIONAL DO-ALL?)
  "Saves a directory tree out to disk.
If DO-ALL? is NIL then only find directory changes that start at the root,
and only write out changed directories.
If DO-ALL? is :FIND-ALL then only write out changed directories, but find
every changed directory by exhaustive search.
If DO-ALL? is :SAVE-ALL then write out the entire tree independent of its
state of modification."
  (HANDLING-ILLEGAL-ABORTIONS "critical filesystem update" #'SAVE-DIRECTORY-TREE-1 DO-ALL?))

(DEFUN SAVE-DIRECTORY-TREE-1 (DO-ALL?)
  (LET ((OLD-MAPS (SAVE-DIRECTORY-SUBTREE (DC-ROOT-DIRECTORY) DO-ALL?)))
    (when OLD-MAPS
      (when (memq :check-directory-maps *lmfs-active-debug-modes*)
        ;;; this re-reads the directory map for the deepest changed
        ;;; directory in the tree
        (let ((map (car (last old-maps))))
          (when (map-owner map)
            (let* ((directory (map-owner map))
                   (old-files (directory-files directory)))
              old-files                         ;here for debugging if re-read gets an error
              (setf (directory-files directory) :disk)
              (read-directory-files directory)
              ;;; this must be done for abort conditions which must remove the old file
              (setf (directory-files directory) old-files)))))
      (USING-PUT
        (DOLIST (MAP OLD-MAPS)
          ;; BUG: If one or more these function calls happens and then an error or abortion
          ;; happens, then the put is changed, but the root directory is not written.
          ;; therefore blocks in directories will be marked as free.
          (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-FREE))
        (ASET PUT-CONSISTENT PAGE-USAGE-TABLE 0)
        (LOCKING DISK-CONFIGURATION-LOCK
          (LM-WRITE-CONFIGURATION))))))


(DEFUN SAVE-DIRECTORY-SUBTREE (DIRECTORY DO-ALL?)
  (LET ((FILES (DIRECTORY-FILES DIRECTORY)))
    (LET ((SUBDIRECTORY-RESULTS
            (MAPCAN #'(LAMBDA (FILE)
                        (AND (DIRECTORY? FILE)
                             (OR DO-ALL? (NOT (FILE-CLOSED? FILE)))
                             (SAVE-DIRECTORY-SUBTREE FILE DO-ALL?)))
                    (COND ((NOT (EQ FILES ':DISK))
                           FILES)
                          ((EQ DO-ALL? ':SAVE-ALL)
                           (READ-DIRECTORY-FILES DIRECTORY))
                          (T '())))))
      (IF (OR (EQ DO-ALL? ':SAVE-ALL)
              (NOT (FILE-CLOSED? DIRECTORY)))
          (COND ((> (DC-VERSION) 4.)
                 (LET ((TMP (LMFS-UPDATE-DIRECTORY DIRECTORY)))
                   (IF TMP (CONS TMP SUBDIRECTORY-RESULTS) SUBDIRECTORY-RESULTS)))
                ('ELSE
                 (SETF (FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK) NIL)
                 (CONS (LMFS-WRITE-DIRECTORY DIRECTORY) SUBDIRECTORY-RESULTS)))
        SUBDIRECTORY-RESULTS))))

(DEFUN READ-DIRECTORY-FILES (DIRECTORY)
  "Reads in the files of DIRECTORY if they aren't already."
  (LOCKING (DIRECTORY-LOCK DIRECTORY)
    (LET ((FILES (DIRECTORY-FILES DIRECTORY)))
      (IF (EQ FILES ':DISK)
          (WITH-MAP-STREAM-IN (STREAM (FILE-MAP DIRECTORY))
            (LMFS-READ-DIRECTORY STREAM DIRECTORY))
          FILES))))

(DEFUN TOP-LEVEL-DIRECTORIES ()
  (TOP-LEVEL-DIRECTORIES-ACL)
  (SUBSET #'DIRECTORY? (READ-DIRECTORY-FILES (DC-ROOT-DIRECTORY))))

(DEFUN LMFS-WRITE-DIRECTORY (DIRECTORY)
  (LOCKING (DIRECTORY-LOCK DIRECTORY)
    (LET ((OLD-MAP (FILE-MAP DIRECTORY))
          (NEW-MAP (IF (AND (ROOT-DIRECTORY? DIRECTORY)
                            (= (DC-VERSION) 3))
                       ;; Crock introduced for compatibility with old system.
                       (GET-OLD-ROOT-DIRECTORY-MAP DIRECTORY)
                     (LMFS-GET-NEW-DIRECTORY-MAP DIRECTORY))))
      (setf (map-owner new-map) directory)
      (SETF (FILE-MAP DIRECTORY) NEW-MAP)
      (SETF (FILE-CLOSED? DIRECTORY) T)
      (USING-PUT
        (CHANGE-MAP-DISK-SPACE NEW-MAP PUT-RESERVED PUT-USED))
      OLD-MAP)))

(DEFUN LMFS-GET-NEW-DIRECTORY-MAP (DIRECTORY)
  ;; Allow us to use up the last few free disk blocks.
  (LET ((WRITING-INTERNAL-STRUCTURES T))
    (WITH-MAP-STREAM-OUT (STREAM)
      (DOLIST (FILE (DIRECTORY-FILES DIRECTORY))
        (WRITE-DIRECTORY-ENTRY STREAM FILE)))))


(DEFUN LMFS-READ-DIRECTORY-OLD (STREAM DIRECTORY)
  (LET ((FILES '()))
    (DO-FOREVER
      (LET ((FILE (READ-DIRECTORY-ENTRY STREAM DIRECTORY)))
        (COND ((NULL FILE)
               (SETQ FILES (NREVERSE FILES))
               (SETF (DIRECTORY-FILES DIRECTORY) FILES)
               (RETURN FILES))
              (T
               (PUSH FILE FILES)))))))


(DEFUN LMFS-READ-DIRECTORY (STREAM DIRECTORY)
  (COND ((FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK)
         (LMFS-READ-DIRECTORY-NEW STREAM DIRECTORY))
        ('ELSE
         (LMFS-READ-DIRECTORY-OLD STREAM DIRECTORY))))


(DEFUN LMFS-READ-DIRECTORY-NEW (STREAM DIRECTORY)
  ;; KLUDGE: WE GRAP THE RQB AND SET THE MAP OF THE STREAM WE ARE GIVEN.
  (LET ((RQB (SYMEVAL-IN-INSTANCE STREAM 'RQB))
        (MAP (SYMEVAL-IN-INSTANCE STREAM 'MAP)))
    (LM-DISK-READ RQB (MAP-BLOCK-LOCATION MAP 0) 1)
    (LET ((S (SI:RQB-8-BIT-BUFFER RQB))
          (V))
      (OR (STRING-EQUAL NEW-HEADER-ID S
                        :START2 0 :END2 NEW-HEADER-VERSION-OFFSET)
          (FERROR NIL "INVALID DIRECTORY HEADER: ~S" (SUBSTRING S 0 NEW-HEADER-ID-STRING-OFFSET)))
      (CASE (SETQ V (STRING-GET-32B S NEW-HEADER-VERSION-OFFSET))
        ((1 2)
         (LET ((FLAG (STRING-GET-32B S NEW-HEADER-FLAG-OFFSET))
               (S0-MAP)(S1-MAP))
           (OR (= FLAG 0) (= FLAG 1) (FERROR NIL "INVALID DIRECTORY HEADER FLAG: ~D"
                                             (STRING-GET-32B S NEW-HEADER-FLAG-OFFSET)))
           (WITH-INPUT-FROM-STRING (SS S :START NEW-HEADER-S0-MAP-OFFSET)
             (SETQ S0-MAP (IF (= V 1) (MAP-READ SS) (EXTENDED-MAP-READ SS)))
             (SETQ S1-MAP (IF (= V 1) (MAP-READ SS) (EXTENDED-MAP-READ SS))))
           (SETF (GETF (FILE-PLIST DIRECTORY) '%HEADER-INTERNAL)
                 (MAKE-DIRHEADER FLAG FLAG S0-MAP S0-MAP S1-MAP S1-MAP
                                 ID-STRING (DIRECTORY-ID-STRING DIRECTORY)))
           (SET-IN-INSTANCE STREAM 'MAP (IF (= FLAG 0) S0-MAP S1-MAP))
           (LMFS-READ-DIRECTORY-OLD STREAM DIRECTORY)))
        (T
         (FERROR NIL "INVALID DIRECTORY HEADER VERSION NUMBER: ~D"
                 (STRING-GET-32B S NEW-HEADER-VERSION-OFFSET)))))))


(defun view-all-headers (&optional (at (dc-root-directory)))
  (or (typep at 'file) (setq at (lookup-directory at)))
  (FORMAT T "~&~A " AT)
  (SHORT-DESCRIBE-MAP (FILE-MAP AT))
  (TERPRI)
  (cond ((file-attribute at :header-block)
         (view-header-block (map-block-location (file-map at) 0)))
        ('else
         (format t "~&~A has no header block~%" at)))
  (dolist (f (read-directory-files at))
    (and (directory? f) (view-all-headers f))))


(DEFUN DIRECTORY-ID-STRING (DIRECTORY)
  (IF (FILE-DIRECTORY DIRECTORY)
      (SEND (FILE-TRUENAME DIRECTORY) :STRING-FOR-HOST)
    (FILE-NAME DIRECTORY)))


(DEFUN DIRECTORY-ENTRY-STRING-CHECK (S)
  (COND ((STRINGP S) S)
        ((EQ S :UNSPECIFIC) "")
        ((EQ S :WILD) "*")
        ((SYMBOLP S)
         (SYMBOL-NAME S))
        (T
         "GARBAGE")))

(DEFUN WRITE-DIRECTORY-ENTRY (STREAM FILE)
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-NAME FILE)))
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-TYPE FILE)))
  (LET ((VERSION (FILE-VERSION FILE)))
    (PUT-BYTES STREAM 3 VERSION))
  (PUT-BYTES STREAM 1 (FILE-DEFAULT-BYTE-SIZE FILE))
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-AUTHOR-INTERNAL FILE)))
  (LET ((CREATION-DATE (OR (FILE-CREATION-DATE-INTERNAL FILE)
                           (setf (FILE-CREATION-DATE-INTERNAL FILE) (time:get-universal-time)))))
    (PUT-BYTES STREAM 4 CREATION-DATE))
  (MAP-WRITE STREAM (FILE-MAP FILE))
  (LET ((ATTRIBUTES (FILE-ATTRIBUTES FILE)))
    (PUT-BYTES STREAM 2 ATTRIBUTES))
  (WRITE-PROPERTY-LIST STREAM (FILE-PLIST FILE)))


(DEFUN WRITE-DIRECTORY-ENTRY-NEW (STREAM FILE)
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-NAME FILE)))
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-TYPE FILE)))
  (LET ((VERSION (FILE-VERSION FILE)))
    (PUT-BYTES STREAM 3 VERSION))
  (PUT-BYTES STREAM 1 (FILE-DEFAULT-BYTE-SIZE FILE))
  (SEND STREAM ':LINE-OUT (DIRECTORY-ENTRY-STRING-CHECK (FILE-AUTHOR-INTERNAL FILE)))
  (LET ((CREATION-DATE (OR (FILE-CREATION-DATE-INTERNAL FILE)
                           (setf (FILE-CREATION-DATE-INTERNAL FILE) (time:get-universal-time)))))
    (PUT-BYTES STREAM 4 CREATION-DATE))
  (LET ((G (OR (FILE-GENERATION-NUMBER FILE) 0)))
    (PUT-BYTES STREAM 4 G))
  (MAP-WRITE STREAM (FILE-MAP FILE))
  (LET ((ATTRIBUTES (FILE-ATTRIBUTES FILE)))
    (PUT-BYTES STREAM 2 ATTRIBUTES))
  (WRITE-PROPERTY-LIST STREAM (FILE-PLIST FILE)))

(DEFUN READ-DIRECTORY-ENTRY (STREAM DIRECTORY)
  (MULTIPLE-VALUE-BIND (NAME EOF)
      (SEND STREAM ':LINE-IN T)
    (IF (NOT EOF)
        (LET* ((TYPE (SEND STREAM ':LINE-IN T))
               (VERSION (GET-BYTES STREAM 3))
               (DEFAULT-BYTE-SIZE (GET-BYTES STREAM 1))
               (AUTHOR (SEND STREAM ':LINE-IN T))
               (CREATION-DATE (GET-BYTES STREAM 4))
               (MAP (MAP-READ STREAM))
               (ATTRIBUTES (GET-BYTES STREAM 2))
               (PROPERTIES (READ-PROPERTY-LIST STREAM))
               (FILE (MAKE-FILE NAME NAME
                                TYPE TYPE
                                VERSION VERSION
                                DEFAULT-BYTE-SIZE DEFAULT-BYTE-SIZE
                                FILES ':DISK
                                OPEN-COUNT 0
                                AUTHOR-INTERNAL AUTHOR
                                CREATION-DATE-INTERNAL CREATION-DATE
                                DIRECTORY DIRECTORY
                                MAP MAP
                                ATTRIBUTES ATTRIBUTES
                                PLIST PROPERTIES)))
          FILE))))

(DEFUN READ-DIRECTORY-ENTRY-NEW (STREAM DIRECTORY)
  (MULTIPLE-VALUE-BIND (NAME EOF)
      (SEND STREAM ':LINE-IN T)
    (IF (NOT EOF)
        (LET* ((TYPE (SEND STREAM ':LINE-IN T))
               (VERSION (GET-BYTES STREAM 3))
               (DEFAULT-BYTE-SIZE (GET-BYTES STREAM 1))
               (AUTHOR (SEND STREAM ':LINE-IN T))
               (CREATION-DATE (GET-BYTES STREAM 4))
               (GENERATION-NUMBER (GET-BYTES STREAM 4))
               (MAP (MAP-READ STREAM))
               (ATTRIBUTES (GET-BYTES STREAM 2))
               (PROPERTIES (READ-PROPERTY-LIST STREAM))
               (FILE (MAKE-FILE NAME NAME
                                TYPE TYPE
                                VERSION VERSION
                                DEFAULT-BYTE-SIZE DEFAULT-BYTE-SIZE
                                FILES ':DISK
                                OPEN-COUNT 0
                                AUTHOR-INTERNAL AUTHOR
                                CREATION-DATE-INTERNAL CREATION-DATE
                                GENERATION-NUMBER GENERATION-NUMBER
                                DIRECTORY DIRECTORY
                                MAP MAP
                                ATTRIBUTES ATTRIBUTES
                                PLIST PROPERTIES)))
          FILE))))


;;;; Basic File Operations
;;; Files come in four states:

;;; Nonexistent: an invalid state which should never be encountered.
;;; The storage associated with this state is FREE.

;;; Unclosed:  the normal state for newly-opened output files, it is
;;; indicated by the Closed bit being off.  The Deleted bit can be
;;; either on or off; when closed, the file retains that property.
;;; The storage associated with this state is RESERVED.

;;; Closed:  the normal state for existing files, it is indicated by the
;;; Closed bit being on and the Deleted bit being off.  The storage
;;; associated with this state is USED.

;;; Deleted:  a special state between closed and nonexistent, this is
;;; indicated by the Closed bit being on and the Deleted bit being on.
;;; The storage associated with this state is RESERVED.

;;; Logically, there should be these transition functions:

;;; Nonexistent <-> Unclosed
;;; Unclosed -> Closed
;;; Closed <-> Deleted
;;; Deleted -> Nonexistent

;;; However, the implementation is based on target states only.

;;; The locking strategy is:  lock a file first, then lock its directory
;;; or the PUT, as needed.  Never lock the file second.  There is only
;;; one case where two files need to be locked, and one of those is
;;; an Unclosed file; only one process should point to that file.

;;; Unclosed files are in the directory; however it is not possible
;;; to open them.  Deleted files are also in the directory, and can be opened.

;;; When a file is overwritten, it is left in the directory.
;;; It is made to point to the file which is overwriting it,
;;; and the overwriting file is made to point to the overwritten one also.
;;; It is possible to tell which is which on the basis of the closed bit.
;;; The overwritten file is closed, and the overwriting file isn't.

(DEFVAR LMFS-DIRECTORY-TYPE "DIRECTORY" "The file type for all directories.")
(DEFVAR LMFS-DIRECTORY-VERSION 1 "The file version for all directories.")

(DEFSUBST LMFS-DELETED-FILE? (FILE)
  (AND (FILE-DELETED? FILE)
       (NOT (FILE-CLOSED? FILE))))

(DEFSUBST LMFS-CLOSED-FILE? (FILE)
  (AND (FILE-CLOSED? FILE)
       (NOT (FILE-DELETED? FILE))))

(DEFSUBST LMFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? (FILE)
  (OR (MINUSP (FILE-OPEN-COUNT FILE))
      (NOT (AND (FILE-CLOSED? FILE)
                (NULL (FILE-OVERWRITE-FILE FILE))))))

(DEFSUBST REQUIRE-CLOSED-FILE (FILE)
  (IF (OR (NOT (FILE-CLOSED? FILE))
          (MINUSP (FILE-OPEN-COUNT FILE)))
      (LM-SIGNAL-ERROR 'OPEN-OUTPUT-FILE)))

(DEFSUBST REQUIRE-CLOSED-FILE-NOT-BEING-SUPERSEDED (FILE)
  (IF (LMFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? FILE)
      (LM-SIGNAL-ERROR 'OPEN-OUTPUT-FILE)))

(DEFSUBST REQUIRE-FILE-NOT-OPEN (FILE)
  (REQUIRE-CLOSED-FILE FILE)
  (UNLESS (ZEROP (FILE-OPEN-COUNT FILE))
    (LM-SIGNAL-ERROR 'FILE-LOCKED)))

(DEFSUBST REQUIRE-DELETABLE-FILE (FILE)
  (IF (FILE-ATTRIBUTE FILE ':DONT-DELETE)
      (LM-SIGNAL-ERROR 'DONT-DELETE-FLAG-SET)))

(DEFSUBST REQUIRE-ZERO-OPEN-COUNT (FILE)
  (IF (NOT (ZEROP (FILE-OPEN-COUNT FILE)))
      (FERROR NIL "Expected zero open count for ~S." FILE)))

(DEFUN LMFS-OPEN-OUTPUT-FILE (DIRECTORY LOC NAME TYPE VERSION OVERWRITTEN-FILE)
  (WHEN (< (AREF PUT-USAGE-ARRAY PUT-FREE) PUT-MINIMUM-FREE-PAGES)
    (LM-SIGNAL-ERROR 'NO-MORE-ROOM))
  (REQUIRE-LOCK (DIRECTORY-LOCK DIRECTORY))
  (LET ((FILE (CREATE-NEW-FILE DIRECTORY NAME TYPE VERSION)))
    (SETF (FILE-OPEN-COUNT FILE) -1)
    (SETF (FILE-CLOSED? FILE) NIL)
    (SETF (FILE-DELETED? FILE) NIL)
    (COND ((NULL OVERWRITTEN-FILE)
           (PUSH FILE (CDR LOC))
           (PROCESS-UNLOCK (LOCF (DIRECTORY-LOCK DIRECTORY))))
          (T
           ;; This is critical; unlock directory before locking file.
           (PROCESS-UNLOCK (LOCF (DIRECTORY-LOCK DIRECTORY)))
           (LOCKING (FILE-LOCK OVERWRITTEN-FILE)
             (REQUIRE-CLOSED-FILE-NOT-BEING-SUPERSEDED OVERWRITTEN-FILE)
             (REQUIRE-DELETABLE-FILE OVERWRITTEN-FILE)
             (SETF (FILE-OVERWRITE-FILE FILE) OVERWRITTEN-FILE)
             (INCF (FILE-OPEN-COUNT OVERWRITTEN-FILE))
             (SETF (FILE-OVERWRITE-FILE OVERWRITTEN-FILE) FILE))))
    ;; WRITING THIS TO DISK PRESERVES NO USEFUL DATA.
    ;;(WRITE-DIRECTORY-FILES DIRECTORY)
    ;; JUST MAKE THE DIRECTORY KNOWN TO BE OPEN THOUGH.
    ;;(SETF (FILE-CLOSED? DIRECTORY) NIL)
    ;; ARGH CANT set this as open because then LOOKUP-DIRECTORY will throw to fileystem-sleep
    ;; Need to record this concept some other way.
    (IF (FBOUNDP 'GIVE-FILE-GENERATION-NUMBER)
        (GIVE-FILE-GENERATION-NUMBER FILE))
    FILE))


(DEFUN LMFS-OPEN-OVERWRITE-FILE (FILE)
   (LOCKING (FILE-LOCK FILE)
    (REQUIRE-FILE-NOT-OPEN FILE)
    (DECF (FILE-OPEN-COUNT FILE))
    (SETF (FILE-CLOSED? FILE) NIL)))

(DEFUN LMFS-OPEN-INPUT-FILE (FILE)
  (LOCKING (FILE-LOCK FILE)
    (REQUIRE-CLOSED-FILE FILE)
    (INCF (FILE-OPEN-COUNT FILE))))

(DEFUN LMFS-CLOSE-FILE (FILE)
  "Called when one stream reading or writing FILE is closed.
Updates FILE's open-count.  Guarantees that FILE has its Closed bit set."
  (LOCKING (FILE-LOCK FILE)
    (COND ((MINUSP (FILE-OPEN-COUNT FILE))
           (UNLESS (= (FILE-OPEN-COUNT FILE) -1)
             (FERROR NIL "File open count is less than -1."))
           (INCF (FILE-OPEN-COUNT FILE)))
          ((PLUSP (FILE-OPEN-COUNT FILE))
           (DECF (FILE-OPEN-COUNT FILE)))
          (T (FERROR NIL "Open count of file being closed is zero.")))
    (UNLESS (FILE-CLOSED? FILE)
      (REQUIRE-ZERO-OPEN-COUNT FILE)
      (SETF (FILE-CLOSED? FILE) T)
      (UNLESS (FILE-DELETED? FILE)
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE) PUT-RESERVED PUT-USED)))
      (LET ((OVERWRITTEN-FILE (FILE-OVERWRITE-FILE FILE))
            (DIRECTORY (FILE-DIRECTORY FILE)))
        (UNLESS (NULL OVERWRITTEN-FILE)
          (LOCKING (FILE-LOCK OVERWRITTEN-FILE)
            (SETF (FILE-OVERWRITE-FILE FILE) NIL)
            (DECF (FILE-OPEN-COUNT OVERWRITTEN-FILE))
            (SETF (FILE-OVERWRITE-FILE OVERWRITTEN-FILE) NIL)
            (REPLACE-FILE-IN-DIRECTORY OVERWRITTEN-FILE FILE)
            (COND ((FILE-DELETED? OVERWRITTEN-FILE)
                   (USING-PUT
                     (CHANGE-MAP-DISK-SPACE (FILE-MAP OVERWRITTEN-FILE)
                                            PUT-RESERVED
                                            PUT-FREE)))
                  (T
                   (REQUIRE-DELETABLE-FILE OVERWRITTEN-FILE)
                   (SETF (FILE-DELETED? OVERWRITTEN-FILE) T)
                   (USING-PUT
                     (CHANGE-MAP-DISK-SPACE (FILE-MAP OVERWRITTEN-FILE)
                                            PUT-USED
                                            PUT-FREE))))))
        (WRITE-DIRECTORY-FILES DIRECTORY)))))

(DEFUN LMFS-DELETE-FILE (FILE &OPTIONAL (WRITE-DIRECTORY T) dont-change-map)
  "Guarantees that FILE has its Deleted bit set."
  (LOCKING-RECURSIVELY (FILE-LOCK FILE)
    (REQUIRE-DELETABLE-FILE FILE)
    (UNLESS (FILE-DELETED? FILE)
      (IF (DIRECTORY? FILE)
          (IF (NULL (LET ((FILES (DIRECTORY-FILES file)))
                      (IF (EQ FILES ':DISK)
                          (WITH-MAP-STREAM-IN (STREAM (FILE-MAP file))
                            (LMFS-READ-DIRECTORY STREAM file))
                        FILES)))
              (SETF (FILE-DELETED? FILE) T)
            (LM-SIGNAL-ERROR 'DIRECTORY-NOT-EMPTY))
        (SETF (FILE-DELETED? FILE) T))
      (WHEN (and (FILE-CLOSED? FILE) (not dont-change-map))
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE) PUT-USED PUT-RESERVED)))
      (WHEN WRITE-DIRECTORY (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))))))

(DEFUN LMFS-UNDELETE-FILE (FILE)
  "Guarantees that FILE doesn't have its Deleted bit set."
  (LOCKING-RECURSIVELY (FILE-LOCK FILE)
    (WHEN (FILE-DELETED? FILE)
      (SETF (FILE-DELETED? FILE) NIL)
      (WHEN (FILE-CLOSED? FILE)
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE) PUT-RESERVED PUT-USED)))
      (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE)))))


(defun maint-salvage (&optional (shutdown nil) query &aux bad)
  "Calls FS:LM-SALVAGE and further cleanup if needed"
  (when shutdown
    (fs:file-server-shutdown "Doing critical filesystem salvage work"
                             (if (numberp shutdown) shutdown 1))
    (process-sleep (* 60. (+ (* 60. (if (numberp shutdown) shutdown 1))
                             10.))
                   "shutdown")
    (chaos:reset t)
    (SETQ CHAOS:CHAOS-SERVERS-ENABLED NIL)
    (process-sleep (* 60. 5.) "settling down"))
  (lm-salvage)
  (when salvager-bad-items
    (format t "~&Bad items found; trying ~Ainteractive recovery~%"
            (if query "" "non-"))
    (setq bad (maint-killable-files))
    (loop for file in bad do (maint-delete-and-expunge file query))
    (when bad
      (format t "~&Doing another salvage. It should go ok this time...")
      (lm-salvage)
      (format t "~&Writing out core directory to disk...")
      (WRITE-CORE-FILE-SYSTEM-ONTO-DISK)))
  (when shutdown
    (setq chaos:chaos-servers-enabled t)))

(defun maint-killable-files (&optional query &aux bad-boys truename probed)
  "Run this after FS:LM-SALVAGE to find deleted files that are causing problems"
  (dolist (item salvager-bad-items)
    (format t "~&;Considering ~S~%" item)
    (cond ((DIRECTORY? item)
           (format t "; is a directory, ignoring~%"))
          ('else
           (setq truename (file-truename item))
           (format t "; Truename: ~S~%" truename)
           (setq probed (open truename :direction nil :error nil))
           (cond ((errorp probed)
                  (if (null query)
                      (format t "; >>BADBOY: because: ~A~%" (send probed :report-string)))
                  (if (or (null query)
                          (y-or-n-p "~&BAD: ~S because:~% ~A, kill it?"
                                    truename (send probed :report-string)))
                      (push truename bad-boys)))
                 ('else
                  (format t "; Probably OK~%"))))))
  bad-boys)

(defun maint-delete-and-expunge (&optional filename query &aux pathname file)
  (cond ((null filename)
         (do ()
             ((null (setq filename (prompt-and-read :string-or-nil "~&Filename to Bash>")))
              "now. dont forget to (LM-SALVAGE)!!!!")
           (maint-delete-and-expunge filename)))
        ('else
         (setq pathname (fs:parse-pathname filename))
         (cond ((not (typep pathname 'lm-pathname))
                (format t "~&;Not a local pathname: ~S~%" pathname))
               ((not (or (null query) (yes-or-no-p "Bash away on: ~S ?" pathname)))
                (format t "~&;OK. No action taken~%"))
               ((not (SETQ FILE (LOOKUP-FILE
                                  (PATHNAME-RAW-DIRECTORY PATHNAME)
                                  (PATHNAME-RAW-NAME PATHNAME)
                                  (PATHNAME-RAW-TYPE PATHNAME)
                                  (PATHNAME-RAW-VERSION PATHNAME))))
                (format t "Lookup-file failed on: ~S ~S ~S ~S"
                        (PATHNAME-RAW-DIRECTORY PATHNAME)
                        (PATHNAME-RAW-NAME PATHNAME)
                        (PATHNAME-RAW-TYPE PATHNAME)
                        (PATHNAME-RAW-VERSION PATHNAME)))
               ('else
                (lmfs-delete-file file nil t)
                (lmfs-expunge-file file t t)
                "now. dont forget to (LM-SALVAGE)!!!!")))))




(DEFUN LMFS-EXPUNGE-FILE (FILE &optional dont-change-map ignore-open-count)
  "Guarantees that FILE is in the Nonexistent state.
Does NOT write out FILE's containing directory.  The caller must do that.
This helps avoid lock-up when trying to create free space when disk is full."
  (LOCKING (FILE-LOCK FILE)
    (REQUIRE-DELETABLE-FILE FILE)
    (COND ((FILE-CLOSED? FILE)
           (IF (or (ZEROP (FILE-OPEN-COUNT FILE)) ignore-open-count)
               (REMOVE-FILE-FROM-DIRECTORY FILE)
             (FERROR NIL "File being expunged is still open.")))
          (T
           (INCF (FILE-OPEN-COUNT FILE))
           (REQUIRE-ZERO-OPEN-COUNT FILE)
           (SETF (FILE-CLOSED? FILE) T)
           (IF (FILE-OVERWRITE-FILE FILE)
               ;; This file was overwriting another, and therefore
               ;; not actually in the directory.
               ;; Make the other file forget this one was overwriting it.
               (LET ((OTHER-FILE (FILE-OVERWRITE-FILE FILE)))
                 (DECF (FILE-OPEN-COUNT OTHER-FILE))
                 (SETF (FILE-OVERWRITE-FILE OTHER-FILE) NIL))
             ;; This file was not overwriting another,
             ;; so it really is in the directory.  Remove it.
             (REMOVE-FILE-FROM-DIRECTORY FILE))))
    (or dont-change-map
        (USING-PUT
          (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE)
                                 (IF (FILE-DELETED? FILE) PUT-RESERVED PUT-USED)
                                 PUT-FREE)))
    (SETF (FILE-DELETED? FILE) T)
;    (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))
))

;;; Helper Functions.

(DEFUN REMOVE-FILE-FROM-DIRECTORY (FILE)
  (LET ((DIRECTORY (FILE-DIRECTORY FILE)))
    (LOCKING (DIRECTORY-LOCK DIRECTORY)
      (DO ((TAIL (LOCF (DIRECTORY-FILES DIRECTORY))
                 (LOCF (CDR (CDR TAIL)))))
          ((NULL TAIL)
           (FERROR NIL "The existing file ~S is missing from its directory." FILE))
        (COND ((EQ FILE (CADR TAIL))
               (RPLACD TAIL (CDDR TAIL))
               (RETURN NIL)))))))

(DEFUN REPLACE-FILE-IN-DIRECTORY (FILE NEW-FILE)
  (LET ((DIRECTORY (FILE-DIRECTORY FILE)))
    (LOCKING (DIRECTORY-LOCK DIRECTORY)
      (RPLACA (OR (MEMQ FILE (DIRECTORY-FILES DIRECTORY))
                  (FERROR NIL "The existing file ~S is missing from its directory." FILE))
              NEW-FILE))))

(DEFUN CREATE-NEW-FILE (DIRECTORY NAME TYPE VERSION)
  "Create a new file.  Sets up the default properties, and records
the author and creation-date."
  ;; Crock introduced for compatibility with old system.
  (IF (ROOT-DIRECTORY? DIRECTORY)
      (LM-SIGNAL-ERROR 'TOP-LEVEL-FILE NIL NIL ':OPEN))
  (MAKE-FILE DIRECTORY DIRECTORY
             NAME NAME
             TYPE TYPE
             VERSION VERSION
             AUTHOR-INTERNAL USER-ID
             CREATION-DATE-INTERNAL (TIME:GET-UNIVERSAL-TIME)
             OPEN-COUNT 0
             DEFAULT-BYTE-SIZE 8
             MAP (MAP-CREATE)
             ATTRIBUTES 0
             PLIST NIL))

(DEFUN CREATE-NEW-DIRECTORY (DIRECTORY NAME)
  "Create a new directory.  Like CREATE-NEW-FILE except that
the file's properties are setup differently."
  (LET ((FILE (MAKE-FILE DIRECTORY DIRECTORY
                         NAME NAME
                         TYPE LMFS-DIRECTORY-TYPE
                         VERSION LMFS-DIRECTORY-VERSION
                         AUTHOR-INTERNAL USER-ID
                         CREATION-DATE-INTERNAL (TIME:GET-UNIVERSAL-TIME)
                         OPEN-COUNT 0
                         DEFAULT-BYTE-SIZE 8
                         MAP (MAP-CREATE)
                         ATTRIBUTES 0
                         PLIST NIL)))
    (SETF (DIRECTORY? FILE) T)
    (SETF (FILE-CLOSED? FILE) T)
    (SETF (FILE-DELETED? FILE) NIL)
    FILE))

;;;; Lookups

;;; This is the basic lookup function.
;;; OBJ is the object which you're looking up.
;;; LOC is a locative pointer into the list you're using
;;; COMP is a function which compares OBJ with an element of the list.
;;;   It should return  a positive number if OBJ > ELEM
;;;                 or  0 if OBJ = ELEM
;;;                 or  a negative number if OBJ < ELEM
;;; Values returned are ELEM and LOC.
;;; ELEM is a list element found if successful, or NIL.
;;; LOC is a locative (actually a sublist, usually) into which OBJ can be
;;;  pushed and still preserve the sorting of the list.  (It will be pushed
;;;  before ELEM, if one was found.  Note that this program does not understand
;;;  duplicates, however, and duplicates should not be entered in the list.)

(DEFUN LOOKUP (OBJ LOC COMP)
  (DECLARE (RETURN-LIST ELEM LOC))
  (DO ((LEN (LENGTH (CDR LOC)))
       (S-LOC LOC)
       S-LEN S-COM VAL)
      ((ZEROP LEN)
       (RETURN (VALUES NIL S-LOC)))
    (SETQ S-LEN (FLOOR LEN 2)
          S-COM (NTHCDR S-LEN S-LOC)
          VAL (FUNCALL COMP OBJ (CADR S-COM)))
    (COND ((ZEROP VAL)
           (RETURN (VALUES (CADR S-COM) S-COM)))
          ((PLUSP VAL)
           (SETQ LEN (1- (- LEN S-LEN))
                 S-LOC (CDR S-COM)))
          (T (SETQ LEN S-LEN)))))

;;;; File Lookup

;;; loc --> Previous file
;;;         Requested file
;;;         Next file

(DEFUN LOOKUP-NAMED-FILE (DIRECTORY &REST OBJ)
  (LOOKUP OBJ (LOCF (DIRECTORY-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE))

(DEFUNP LOOKUP-FILE-COMPARE (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (FILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (FILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (- (CADDR LIST) (FILE-VERSION FILE)))

;;; loc --> Previous file
;;;         File with highest non-deleted version
;;;         Next file
;;; loc is unpredictable if all versions are deleted.

(DEFVAR NEWEST-VERSION-SEEN)

(DEFUN LOOKUP-NEWEST-NON-DELETED-FILE (DIRECTORY &REST OBJ)
  (MULTIPLE-VALUE-BIND (FILE LOC OLDEST-VERSION)
      (LEXPR-FUNCALL #'LOOKUP-OLDEST-FILE DIRECTORY OBJ)
    (IF (NULL OLDEST-VERSION)
        (VALUES FILE LOC OLDEST-VERSION)
      (LET ((SAVED-LOC NIL))
        (DO-FOREVER
          (LET ((FILE (CADR LOC)))
            (COND ((NOT (STRING-EQUAL (FIRST OBJ) (FILE-NAME FILE)))
                   (RETURN NIL))
                  ((NOT (STRING-EQUAL (SECOND OBJ) (FILE-TYPE FILE)))
                   (RETURN NIL))
                  ((AND (NOT (FILE-DELETED? FILE))
                        (FILE-CLOSED? FILE))
                   (SETQ SAVED-LOC LOC)))
            (COND ((NULL (CDDR LOC))
                   (RETURN NIL))))
          (SETQ LOC (CDR LOC)))
        (IF (NULL SAVED-LOC)
            (VALUES NIL LOC NIL)
          (VALUES (CADR SAVED-LOC)
                  SAVED-LOC
                  (FILE-VERSION (CADR SAVED-LOC))))))))

;;; loc --> Previous file
;;;         File with highest version, if any
;;;         Next file

(DEFUN LOOKUP-NEWEST-FILE (DIRECTORY &REST OBJ)
  (LET ((NEWEST-VERSION-SEEN NIL))
    (MULTIPLE-VALUE-BIND (IGNORE LOC)
        (LOOKUP OBJ (LOCF (DIRECTORY-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE-NEWEST)
      (VALUES (AND NEWEST-VERSION-SEEN (CAR LOC))
              (IF NEWEST-VERSION-SEEN
                  (NLEFT 1 (LOCF (DIRECTORY-FILES DIRECTORY)) LOC)
                LOC)
              NEWEST-VERSION-SEEN))))

(DEFUNP LOOKUP-FILE-COMPARE-NEWEST (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (FILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (FILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ NEWEST-VERSION-SEEN (FILE-VERSION FILE))
  1)

;;; loc --> Previous file
;;;         File with lowest version
;;;         Next file

(DEFVAR OLDEST-VERSION-SEEN)

(DEFUN LOOKUP-OLDEST-NON-DELETED-FILE (DIRECTORY &REST OBJ)
  (MULTIPLE-VALUE-BIND (FILE LOC OLDEST-VERSION)
      (LEXPR-FUNCALL #'LOOKUP-OLDEST-FILE DIRECTORY OBJ)
    (IF (NULL OLDEST-VERSION)
        (VALUES FILE LOC OLDEST-VERSION)
      (DO-FOREVER
        (LET ((FILE (CADR LOC)))
          (COND ((NOT (STRING-EQUAL (FIRST OBJ) (FILE-NAME FILE)))
                 (RETURN (VALUES NIL LOC NIL)))
                ((NOT (STRING-EQUAL (SECOND OBJ) (FILE-TYPE FILE)))
                 (RETURN (VALUES NIL LOC NIL)))
                ((AND (NOT (FILE-DELETED? FILE))
                      (FILE-CLOSED? FILE))
                 (RETURN (VALUES FILE LOC (FILE-VERSION FILE))))
                ((NULL (CDDR LOC))
                 (RETURN (VALUES NIL LOC NIL)))))
        (SETQ LOC (CDR LOC))))))

(DEFUN LOOKUP-OLDEST-FILE (DIRECTORY &REST OBJ)
  (LET ((OLDEST-VERSION-SEEN NIL))
    (MULTIPLE-VALUE-BIND (IGNORE LOC)
        (LOOKUP OBJ (LOCF (DIRECTORY-FILES DIRECTORY)) #'LOOKUP-FILE-COMPARE-OLDEST)
      (VALUES (AND OLDEST-VERSION-SEEN (CADR LOC))
              LOC
              OLDEST-VERSION-SEEN))))

(DEFUNP LOOKUP-FILE-COMPARE-OLDEST (LIST FILE &AUX TEM)
  (SETQ TEM (STRING-COMPARE (CAR LIST) (FILE-NAME FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (CADR LIST) (FILE-TYPE FILE)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ OLDEST-VERSION-SEEN (FILE-VERSION FILE))
  -1)

(DEFUN FILE-LESSP (F1 F2)
  (MINUSP (FILE-COMPARE F1 F2)))

(DEFUNP FILE-COMPARE (F1 F2 &AUX TEM)
  (SETQ TEM (STRING-COMPARE (FILE-NAME F1) (FILE-NAME F2)))
  (OR (ZEROP TEM) (RETURN TEM))
  (SETQ TEM (STRING-COMPARE (FILE-NAME F1) (FILE-NAME F2)))
  (OR (ZEROP TEM) (RETURN TEM))
  (- (FILE-VERSION F1) (FILE-VERSION F2)))

(DEFUN LOOKUP-FILE (DIRSPEC NAME TYPE VERSION
                    &OPTIONAL IF-DOES-NOT-EXIST IF-EXISTS (REALLY-OPEN ':DIRECTORY-OK)
                    (DELETED? T))
  "The basic function for finding files.
If REALLY-OPEN is T, we increment the open count of the file,
and get an error if it is a directory.
If REALLY-OPEN is :DIRECTORY-OK (the default), we increment but allow directories.
If REALLY-OPEN is NIL, we do not increment the open count.
DELETED? non-NIL means deleted files can be opened."
  (%STORE-CONDITIONAL (LOCF DIRSPEC) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF NAME) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC "")
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':NEWEST)
  (LOOKUP-FILE-ACL DIRSPEC NAME TYPE VERSION IF-DOES-NOT-EXIST IF-EXISTS REALLY-OPEN DELETED?)
  (LET ((DIRECTORY (IF (NAMED-STRUCTURE-P DIRSPEC) DIRSPEC
                     (LOOKUP-DIRECTORY DIRSPEC)))
        NO-NEW-VERSION
        USE-EXISTING
        OLD-FILE)
    (BLOCK WIN
      (LM-LOOKUP-ERROR
        ;; Must not allow recursive locking -- see LMFS-OPEN-OUTPUT-FILE.
        (LOCKING (DIRECTORY-LOCK DIRECTORY)
          (*CATCH 'LOOKUP-FILE-ERROR
            (MULTIPLE-VALUE-BIND (FILE LOC LAST-VERSION-SEEN)
                (COND ((AND (EQ VERSION ':NEWEST) (EQ IF-EXISTS ':NEW-VERSION))
                       (MULTIPLE-VALUE-BIND (FILE LOC LAST-VERSION-SEEN)
                           (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)
                         (VALUES FILE
                                 (IF FILE (CDR LOC) LOC)
                                 LAST-VERSION-SEEN)))
                      ((MEMQ VERSION '(:NEWEST 0))
                       (IF DELETED?
                           (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)
                         (LOOKUP-NEWEST-NON-DELETED-FILE DIRECTORY NAME TYPE)))
                      ((EQ VERSION ':OLDEST)
                       (IF (OR DELETED? (EQ IF-EXISTS ':NEW-VERSION))
                           (LOOKUP-OLDEST-FILE DIRECTORY NAME TYPE)
                         (LOOKUP-OLDEST-NON-DELETED-FILE DIRECTORY NAME TYPE)))
                      ;; Depends on extra vars in MULTIPLE-VALUE-BIND
                      ;; becoming bound to NIL.
                      ((MINUSP VERSION)
                       (LET ((NEWEST (LOOKUP-NEWEST-FILE DIRECTORY NAME TYPE)))
                         (IF (NULL NEWEST)
                             (*THROW 'LOOKUP-FILE-ERROR 'FILE-NOT-FOUND))
                         (LOOKUP-NAMED-FILE DIRECTORY NAME TYPE
                                            (+ (FILE-VERSION NEWEST) VERSION))))
                      (T
                       (LOOKUP-NAMED-FILE DIRECTORY NAME TYPE VERSION)))
              (IF (AND FILE (DIRECTORY? FILE) REALLY-OPEN
                       (NEQ REALLY-OPEN ':DIRECTORY-OK))
                  (*THROW 'LOOKUP-FILE-ERROR 'FILE-IS-SUBDIRECTORY))
              (IF (OR (NULL FILE)
                      (AND (NOT DELETED?)
                           (FILE-DELETED? FILE)))
                  ;; File "does not exist".
                  (ECASE IF-DOES-NOT-EXIST
                    ((NIL) (RETURN-FROM WIN NIL))
                    (:ERROR
                     (*THROW 'LOOKUP-FILE-ERROR
                             (IF FILE 'OPEN-DELETED-FILE 'FILE-NOT-FOUND)))
                    (:CREATE NIL))
                ;; File "exists".  Should we use it?
                (ECASE IF-EXISTS
                  ((NIL)
                   (SETQ USE-EXISTING T))
                  (:NEW-VERSION
                   (UNLESS (MEMQ VERSION '(:NEWEST :OLDEST)) (SETQ USE-EXISTING nil)))
                  (:SUPERSEDE (SETQ NO-NEW-VERSION T) (SETQ USE-EXISTING nil))
                  ((:OVERWRITE :TRUNCATE :APPEND)
                   (UNLESS (ZEROP (FILE-OPEN-COUNT FILE))
                     (*THROW 'LOOKUP-FILE-ERROR 'FILE-LOCKED))
                   (SETQ USE-EXISTING T))
                  (:ERROR
                   (*THROW 'LOOKUP-FILE-ERROR 'FILE-ALREADY-EXISTS))
                  ((:RENAME :RENAME-AND-DELETE)
                   (SETQ OLD-FILE FILE))))
              (RETURN-FROM WIN
                (IF USE-EXISTING
                    (PROGN
                      (IF REALLY-OPEN
                          (IF (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))
                              (LMFS-OPEN-OVERWRITE-FILE FILE)
                            (LMFS-OPEN-INPUT-FILE FILE)))
                      FILE)
                  (VALUES
                    (LMFS-OPEN-OUTPUT-FILE
                      DIRECTORY LOC NAME TYPE
                      (COND ((EQ VERSION ':NEWEST)
                             (IF (NULL LAST-VERSION-SEEN)
                                 1
                               (IF NO-NEW-VERSION
                                   LAST-VERSION-SEEN
                                 (1+ LAST-VERSION-SEEN))))
                            ((EQ VERSION ':OLDEST)
                             (IF (NULL LAST-VERSION-SEEN)
                                 1
                               (IF NO-NEW-VERSION
                                   LAST-VERSION-SEEN
                                 (1- LAST-VERSION-SEEN))))
                            ((NUMBERP VERSION)
                             (COND ((MINUSP VERSION)
                                    (*THROW 'LOOKUP-FILE-ERROR 'FILE-NOT-FOUND))
                                   ((NOT (< VERSION 1_16.))
                                    (*THROW 'LOOKUP-FILE-ERROR 'VERSION-TOO-LARGE))
                                   ((ZEROP VERSION)
                                    (OR LAST-VERSION-SEEN 1))
                                   (T VERSION))))
                      (UNLESS (AND (MEMQ VERSION '(:NEWEST :OLDEST))
                                   (NOT NO-NEW-VERSION))
                        FILE))
                    OLD-FILE))))))
        DIRECTORY NAME TYPE VERSION))))

;;;; Directory Lookup

;locking when calling LOOKUP-DIRECTORY:
; LOOKUP-DIRECTORY does not seize any locks, however, LOOKUP-SUBDIRECTORY-STEP does.
;  It is possible to encounter subdirectories
;which are being created by other processes (the infamous directory-being-created lossage).
;No locks should be seized when calling here!  I think this is true, it should be
;verified and notated in the listing below.  (If there were any, it might set up a deadly
;embrace situation).

;To survey the problem here are the callers and locking situations on each.
; COMPLETE-PATH
; LMFS-COMPLETE-PATH
; LMFS-CREATE-DIRECTORY
; LMFS-DELETE-DIRECTORY
; LMFS-DELETE-EMPTY-DIRECTORIES
; LMFS-DIRECTORY-LIST-HEADER
; LMFS-LIST-DIRECTORIES
; LMFS-LIST-FILES
; LOOKUP-FILE
;  (LOCAL-FILE-ACCESS :CHANGE-PROPERTIES, :DELETE, :DELETE-MULTIPLE-FILES, :MULTIPLE-FILE-PLISTS)
;  (LOCAL-FILE-ACCESS :PROPERTIES, :RENAME)
;  LMFS-OPEN-FILE
;   (LOCAL-FILE-ACCESS :OPEN)
;  LMFS-RENAME-FILE
;  LOOKUP-DIRECTORY-FILES
; MAKE-SUBDIRECTORY-ALIST (does this before getting DIRECTORY-LOCK)
; TRY-COMPETE-DIRECTORY

(DEFUN LOOKUP-DIRECTORY (NAME &OPTIONAL OK-IF-NOT-THERE)
  "Find a named directory and make sure that it is read in.
No locks should be held when calling here, since this can hang if it finds a subdirectory
being created"
  (DO-FOREVER
    (CATCH 'FILE-BEING-WRITTEN-OR-SUPERSEDED
      (RETURN-FROM LOOKUP-DIRECTORY (LOOKUP-DIRECTORY-1 NAME OK-IF-NOT-THERE)))
    ;; THE MOST COMMON CASE HERE IS ON A FILE SERVER MACHINE.
    ;; SO WAITING FOR A SECOND IS A REASONABLE WAY TO HANDLE THIS.
    (PROCESS-SLEEP 60. "filesystem sleep")))

(DEFUN LOOKUP-DIRECTORY-1 (NAME &OPTIONAL OK-IF-NOT-THERE)
  (COND ((AND (TYPEP NAME 'FILE)
              (DIRECTORY? NAME))
         NAME)
        ((CONSP NAME)
         (LOOKUP-SUBDIRECTORY (LOOKUP-ROOT-DIRECTORY) NAME OK-IF-NOT-THERE))
        ((MEMQ NAME '(NIL :ROOT))
         (LOOKUP-ROOT-DIRECTORY))
        ((STRINGP NAME)
         (LOOKUP-DIRECTORY-1 (LIST NAME) OK-IF-NOT-THERE))
        (T
         (LM-SIGNAL-ERROR 'INVALID-DIRECTORY-NAME (make-pathname :host "LM" :directory NAME)))))

(DEFUN LOOKUP-ROOT-DIRECTORY ()
  (REQUIRE-DISK-CONFIGURATION)
  (READ-DIRECTORY-FILES (DC-ROOT-DIRECTORY))
  (DC-ROOT-DIRECTORY))

(DEFUN LOOKUP-SUBDIRECTORY (NODE SUBPATH OK-IF-NOT-THERE)
  (IF (NULL (CDR SUBPATH))
      (LOOKUP-SUBDIRECTORY-STEP NODE
                                (CAR SUBPATH)
                                OK-IF-NOT-THERE)
    (let ((step (LOOKUP-SUBDIRECTORY-STEP NODE
                                          (CAR SUBPATH)
                                          OK-IF-NOT-THERE)))
      (and step
           (LOOKUP-SUBDIRECTORY step
                                (CDR SUBPATH)
                                OK-IF-NOT-THERE)))))

(DEFUN LOOKUP-SUBDIRECTORY-STEP (NODE STEP OK-IF-NOT-THERE)
  (READ-DIRECTORY-FILES NODE)
  (case (catch 'lookup-subdirectory-step-error
          (RETURN-from lookup-subdirectory-step
            (LOCKING-recursively (DIRECTORY-LOCK NODE)
              (MULTIPLE-VALUE-BIND (FILE LOC)
                  (LOOKUP-NAMED-FILE NODE
                                     STEP
                                     LMFS-DIRECTORY-TYPE
                                     LMFS-DIRECTORY-VERSION)
                (COND ((NOT (NULL FILE))
                       (LOCKING-recursively (FILE-LOCK FILE)
                         (IF (NOT (DIRECTORY? FILE))
                             (throw 'lookup-subdirectory-step-error 'expected-a-directory))
                         (IF (LMFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? FILE)
                             (THROW 'FILE-BEING-WRITTEN-OR-SUPERSEDED FILE)
                                                ;          (LM-LOOKUP-ERROR 'OPEN-UNFINISHED-DIRECTORY
                                                ;                           NODE
                                                ;                           STEP
                                                ;                           LMFS-DIRECTORY-TYPE
                                                ;                           LMFS-DIRECTORY-VERSION)
                           )
                         (IF (FILE-DELETED? FILE)
                             (LM-LOOKUP-ERROR 'OPEN-DELETED-DIRECTORY
                                              NODE
                                              STEP
                                              LMFS-DIRECTORY-TYPE
                                              LMFS-DIRECTORY-VERSION)))
                       (READ-DIRECTORY-FILES FILE)      ;make sure files are read in.
                       FILE)
                      ((eq step :wild)
                       (throw 'lookup-subdirectory-step-error 'bad-directory-name))
                      (LM-AUTOMATICALLY-CREATE-DIRECTORIES
                       (LET ((DIRECTORY (CREATE-NEW-DIRECTORY NODE STEP)))
                         (PUSH DIRECTORY (CDR LOC))
                         (PROCESS-UNLOCK (LOCF (DIRECTORY-LOCK NODE)))
                         (WRITE-DIRECTORY-FILES NODE)
                         (IF (FBOUNDP 'GIVE-FILE-GENERATION-NUMBER)
                             (GIVE-FILE-GENERATION-NUMBER DIRECTORY))
                         DIRECTORY))
                      ((NOT OK-IF-NOT-THERE)
                       (throw 'lookup-subdirectory-step-error 'DIRECTORY-NOT-FOUND)))))))
    ('DIRECTORY-NOT-FOUND
     (LM-SIGNAL-ERROR 'DIRECTORY-NOT-FOUND))
    ('EXPECTED-A-DIRECTORY
     (lm-signal-error 'wrong-kind-of-file))
    ('bad-directory-name
     (lm-signal-error 'invalid-wildcard))))



;;;; Wildcarded Lookup

(DEFUN LOOKUP-FILES (DIRECTORY NAME TYPE VERSION &OPTIONAL (DELETED? T))
  (%STORE-CONDITIONAL (LOCF DIRECTORY) ':UNSPECIFIC '())
  (%STORE-CONDITIONAL (LOCF NAME) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':NEWEST)
  (LOOKUP-FILES-ACL DIRECTORY NAME TYPE VERSION)
  (IF (AND (EQ NAME ':WILD) (EQ TYPE ':WILD) (EQ VERSION ':WILD))
      ;; Optimize simple case.
      (MAPCAN #'(LAMBDA (DIR)
                  (IF DELETED?
                      (COPYLIST (READ-DIRECTORY-FILES DIR))
                      (SUBSET #'LMFS-CLOSED-FILE? (READ-DIRECTORY-FILES DIR))))
              (LOOKUP-DIRECTORIES DIRECTORY))
      (MAPCAN #'(LAMBDA (DIR) (LOOKUP-DIRECTORY-FILES DIR NAME TYPE VERSION DELETED?))
              (LOOKUP-DIRECTORIES DIRECTORY))))

(DEFUN LOOKUP-DIRECTORIES (NAME)
  (REQUIRE-DISK-CONFIGURATION)
  (COND ((MEMQ NAME '(NIL :ROOT))
         (LIST (DC-ROOT-DIRECTORY)))
        ((CONSP NAME)
         (LOOKUP-SUBDIRECTORIES (DC-ROOT-DIRECTORY) NAME))
        ((EQ NAME ':WILD)
         (LOOKUP-SUBDIRECTORIES (DC-ROOT-DIRECTORY) NAME))
        ((STRINGP NAME)
         (LOOKUP-SUBDIRECTORIES (DC-ROOT-DIRECTORY) (LIST NAME)))
        (T
         (LM-SIGNAL-ERROR 'INVALID-DIRECTORY-NAME (make-pathname :host "LM" :directory NAME)))))

(DEFUN LOOKUP-SUBDIRECTORIES (NODE PATH)
  (MAPCAN #'(LAMBDA (FILE)
              (COND ((NOT (DIRECTORY? FILE)) '())
                    ((EQ PATH ':WILD) (LIST FILE))
                    ((eq (directory-name file) :wild) '())
                    ((WILDCARD-MATCH (CAR PATH) (DIRECTORY-NAME FILE))
                     (IF (NULL (CDR PATH))
                         (LIST FILE)
                         (LOOKUP-SUBDIRECTORIES FILE (CDR PATH))))
                    (T '())))
          (READ-DIRECTORY-FILES NODE)))

;;; This isn't as clean as everything else because it wants to implement
;;; all the possible version tokens.  Also, it could be made faster
;;; by incorporating more knowledge of the directory structure here.

(DEFUN LOOKUP-DIRECTORY-FILES (DIR NAME TYPE VERSION DELETED?)
  (LET ((FILES '()))
    (DOLIST (FILE (READ-DIRECTORY-FILES DIR))
      (IF (AND (WILDCARD-MATCH NAME (FILE-NAME FILE))
               (WILDCARD-MATCH TYPE (FILE-TYPE FILE))
               (OR (NOT (NUMBERP VERSION))
                   (NOT (PLUSP VERSION))
                   (= VERSION (FILE-VERSION FILE))))
          (IF (OR DELETED? (LMFS-CLOSED-FILE? FILE))
              (PUSH FILE FILES))))
    (SETQ FILES (NREVERSE FILES))
    (IF (OR (EQ VERSION ':WILD)
            (AND (NUMBERP VERSION)
                 (> VERSION 0)))
        FILES
        (LOOP FOR FILE IN FILES BY #'NEXT-GENERIC-FILE
              AS NEW-FILE = (LOOKUP-FILE DIR (FILE-NAME FILE) (FILE-TYPE FILE) VERSION
                                         NIL NIL NIL DELETED?)
              WHEN NEW-FILE COLLECT NEW-FILE))))

;;; Special Matcher for Wildcards

(DEFUN WILDCARD-MATCH (WILD-STRING STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
  (OR (EQ WILD-STRING ':WILD)
      (EQUAL WILD-STRING "*")
      (LOOP WITH WILD-LENGTH = (STRING-LENGTH WILD-STRING)
            FOR COMPARE-INDEX = START THEN (+ COMPARE-INDEX (- STAR-INDEX MATCH-INDEX))
            FOR MATCH-INDEX = 0 THEN (1+ STAR-INDEX)
            FOR STAR-INDEX = (STRING-SEARCH-CHAR #/* WILD-STRING MATCH-INDEX)
            WHEN (NULL STAR-INDEX)                      ;Rest must match
              RETURN (LET* ((WILD-LEFT  (- WILD-LENGTH MATCH-INDEX))
                            (STARTCMP (IF (ZEROP MATCH-INDEX)
                                          COMPARE-INDEX
                                          (- END WILD-LEFT))))
                       (AND (= (- END STARTCMP) WILD-LEFT)
                            (%STRING-EQUAL WILD-STRING MATCH-INDEX
                                           STRING STARTCMP
                                           (- WILD-LENGTH MATCH-INDEX))))
            ALWAYS (IF (ZEROP MATCH-INDEX)              ;No star to the left
                       (%STRING-EQUAL WILD-STRING MATCH-INDEX STRING COMPARE-INDEX
                                      (- STAR-INDEX MATCH-INDEX))
                       (LET* ((KEY (SUBSTRING WILD-STRING MATCH-INDEX STAR-INDEX))
                              (IDX (STRING-SEARCH KEY STRING COMPARE-INDEX END)))
                         (SETQ COMPARE-INDEX IDX))))))

;;;; Directory Operations

(DEFUN LMFS-CREATE-DIRECTORY (NAME)
  "Create a directory given its NAME."
  (CREATE-DIRECTORY-ACL NAME)
  (LET ((LM-AUTOMATICALLY-CREATE-DIRECTORIES T))
    (LOOKUP-DIRECTORY NAME)))

(DEFUN LMFS-EXPUNGE-DIRECTORY (DIRECTORY NAME TYPE VERSION)
  (%STORE-CONDITIONAL (LOCF DIRECTORY) ':UNSPECIFIC '())
  (%STORE-CONDITIONAL (LOCF NAME) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC ':WILD)
  (%STORE-CONDITIONAL (LOCF VERSION) ':UNSPECIFIC ':WILD)
  (LET ((RESULTING-BLOCKS-FREED 0)
        (DIRECTORY-FILE nil)
        new-directory-file)
    (DOLIST (FILE (LOOKUP-FILES DIRECTORY NAME TYPE VERSION ':DELETED)
                  (WRITE-DIRECTORY-FILES DIRECTORY-FILE))
      (WHEN (FILE-DELETED? FILE)
        (setq new-directory-file (FILE-DIRECTORY FILE))
        (unless (eq directory-file new-directory-file)
          (when directory-file
            (write-directory-files directory-file))
          (setq directory-file (FILE-DIRECTORY FILE)))
        (LMFS-EXPUNGE-FILE FILE)
        (INCF RESULTING-BLOCKS-FREED (FILE-NPAGES FILE))))
    RESULTING-BLOCKS-FREED))

;; Nobody calls this, now.
(DEFUN LMFS-DELETE-DIRECTORY (NAME &OPTIONAL (ERROR-P T))
  "Delete the single directory given by NAME."
  (IDENTIFY-FILE-OPERATION ':DELETE-DIRECTORY
    (HANDLING-ERRORS ERROR-P
      (LET ((DIRECTORY (LOOKUP-DIRECTORY NAME)))
        (IF (NULL (READ-DIRECTORY-FILES DIRECTORY))
            (LMFS-DELETE-FILE DIRECTORY)
            (LM-SIGNAL-ERROR 'DIRECTORY-NOT-EMPTY))))))

(DEFUN LMFS-DELETE-EMPTY-DIRECTORIES (&OPTIONAL QUERY-P (NAME '()))
  "Locate all of the empty directories in the tree under the directory
node given by name, and delete them from the tree.  If NAME is not
supplied or NIL, then search the entire tree."
  (LMFS-DELETE-EMPTY-SUBDIRECTORIES (LOOKUP-DIRECTORY NAME) QUERY-P))

(DEFUN LMFS-DELETE-EMPTY-SUBDIRECTORIES (DIRECTORY QUERY-P)
  (DOLIST (FILE (READ-DIRECTORY-FILES DIRECTORY))
    (IF (DIRECTORY? FILE)
        (LMFS-DELETE-EMPTY-SUBDIRECTORIES FILE QUERY-P)))
  (LMFS-DELETE-EMPTY-DIRECTORY DIRECTORY QUERY-P))

(DEFUN LMFS-DELETE-EMPTY-DIRECTORY (DIRECTORY QUERY-P)
  (IF (NULL (DIRECTORY-FILES DIRECTORY))
      (IF (OR (NULL QUERY-P)
              (Y-OR-N-P (FORMAT NIL "~&Delete ~A? " (DIRECTORY-NAME DIRECTORY))))
          (PROGN
            (FORMAT T "~&Deleting ~S ... " DIRECTORY)
            (LMFS-DELETE-FILE DIRECTORY)
            (FORMAT T "done.")))))

;;;; File Operations


(DEFUN LMFS-OPEN-FILE (PATHNAME DIRECTORY NAME TYPE VERSION
                       &KEY (ERROR T) (DIRECTION :INPUT) (CHARACTERS :default)
                       (BYTE-SIZE :DEFAULT) DELETED PRESERVE-DATES
                       (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
                       (IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
                                            ;; :UNSPECIFIC here is to prevent lossage
                                            ;; writing ITS files with no version numbers.
                                            '(:NEWEST :UNSPECIFIC))
                                      :NEW-VERSION :SUPERSEDE)
                                  IF-EXISTS-P)
                       (IF-DOES-NOT-EXIST
                         (COND ((MEMQ DIRECTION '(:PROBE :PROBE-DIRECTORY :PROBE-LINK))
                                NIL)
                               ((AND (MEMQ DIRECTION '(:OUTPUT :IO))
                                     (NOT (MEMQ IF-EXISTS '(:OVERWRITE :APPEND))))
                                :CREATE)
                               ;; Note: if DIRECTION is NIL, this defaults to :ERROR
                               ;; for compatibility with the past.
                               ;; A Common-Lisp program would use :PROBE
                               ;; and get NIL as the default for this.
                               (T :ERROR)))
                       ;;The following are ignored
                       INHIBIT-LINKS super-image
                       &AUX FILE INITIAL-PLIST OLD-FILE
                       PHONY-CHARACTERS SIGN-EXTEND-BYTES)
  "Implements the :OPEN message for local-file pathnames."
  (declare (ignore inhibit-links))
  (declare (ignore super-image))
 ;  ESTIMATED-LENGTH
  (IDENTIFY-FILE-OPERATION :OPEN
    (HANDLING-ERRORS ERROR
      (CASE DIRECTION
        ((:INPUT :OUTPUT :IO :PROBE-DIRECTORY))
        ((NIL :PROBE :PROBE-LINK) (SETQ DIRECTION :PROBE))
        (T (FERROR 'UNIMPLEMENTED-OPTION "~S is not a valid DIRECTION argument" DIRECTION)))
      (UNLESS (MEMQ IF-EXISTS '(:ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
                                :OVERWRITE :APPEND :SUPERSEDE NIL))
        (FERROR 'UNIMPLEMENTED-OPTION "~S is not a valid IF-EXISTS argument" IF-EXISTS))
      (UNLESS (MEMQ IF-DOES-NOT-EXIST
                    '(:ERROR :CREATE NIL))
        (FERROR 'UNIMPLEMENTED-OPTION
                "~S is not a valid IF-DOES-NOT-EXISTS argument" IF-EXISTS))
      (WHEN ELEMENT-TYPE-P
        (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
              (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
      (IF (OR PHONY-CHARACTERS SIGN-EXTEND-BYTES)
          (FERROR 'UNIMPLEMENTED-OPTION "~S as element-type is not implemented."
                  ELEMENT-TYPE))
      (IF (NOT (MEMQ BYTE-SIZE '(16. 8 4 2 1 :DEFAULT)))
          (LM-SIGNAL-ERROR 'INVALID-BYTE-SIZE))
      (SETF (VALUES FILE OLD-FILE)
            (LOOKUP-FILE DIRECTORY NAME TYPE VERSION
                         (AND (NEQ DIRECTION :PROBE-DIRECTORY) IF-DOES-NOT-EXIST)
                         (AND (MEMQ DIRECTION '(:OUTPUT :IO))
                              IF-EXISTS)
                         (NEQ DIRECTION :PROBE) DELETED))
      (WHEN (IF FILE (OR (NOT (MEMQ DIRECTION '(:OUTPUT :IO))) IF-EXISTS)
              (OR (EQ DIRECTION :PROBE-DIRECTORY) IF-DOES-NOT-EXIST))
        (WHEN OLD-FILE
          (SELECTQ IF-EXISTS
            (:RENAME
             (LMFS-RENAME-FILE OLD-FILE DIRECTORY
                               (STRING-APPEND "_OLD_" NAME) TYPE :NEWEST))
            (:RENAME-AND-DELETE
             (LMFS-RENAME-FILE OLD-FILE DIRECTORY
                               (STRING-APPEND "_OLD_" NAME) TYPE :NEWEST)
             (LMFS-DELETE-FILE OLD-FILE NIL))))
        ;; Empty out the file, if supposed to.
        (WHEN (EQ IF-EXISTS :TRUNCATE)
          (LET ((NBLOCKS (MAP-NBLOCKS (FILE-MAP FILE))))
            (SETF (MAP-NBLOCKS (FILE-MAP FILE)) 0)
            ;; Write the directory showing the file empty.
            (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))
            (SETF (MAP-NBLOCKS (FILE-MAP FILE)) NBLOCKS)
            ;; Then mark the blocks free.
            (USING-PUT
              (CHANGE-MAP-DISK-SPACE (FILE-MAP FILE)
                                     (IF (FILE-DELETED? FILE) PUT-RESERVED PUT-USED)
                                     PUT-FREE))
            (SETF (MAP-NBLOCKS (FILE-MAP FILE)) 0)))
        (SELECTQ DIRECTION
          ((:PROBE :INPUT)
           (IF (EQ CHARACTERS :DEFAULT)
               (SETQ CHARACTERS (FILE-ATTRIBUTE FILE :CHARACTERS)))
           (COND ((NULL BYTE-SIZE)
                  (SETQ BYTE-SIZE (IF CHARACTERS 8 16.)))
                 ((EQ BYTE-SIZE :DEFAULT)
                  (SETQ BYTE-SIZE (FILE-DEFAULT-BYTE-SIZE FILE)))))
          ((:OUTPUT :IO)
           (if (eq characters :default)
               (setq characters t))
           (IF (MEMQ BYTE-SIZE '(:DEFAULT NIL))
               (SETQ BYTE-SIZE (IF CHARACTERS 8 16.)))
           (SETF (FILE-DEFAULT-BYTE-SIZE FILE) BYTE-SIZE)
           (SETF (FILE-ATTRIBUTE FILE :CHARACTERS) CHARACTERS)
           (UNLESS PRESERVE-DATES
             (SETF (FILE-CREATION-DATE-INTERNAL FILE)
                   (TIME:GET-UNIVERSAL-TIME)))
           (LMFS-CHANGE-FILE-PROPERTIES FILE INITIAL-PLIST)))
        (IF (EQ DIRECTION :PROBE-DIRECTORY)
            (MAKE-INSTANCE 'LM-PROBE-STREAM
                           :TRUENAME (SEND PATHNAME :NEW-PATHNAME
                                            :NAME NIL :TYPE NIL :VERSION NIL)
                           :PATHNAME PATHNAME)
          (MAKE-INSTANCE
            (SELECTQ DIRECTION
              (:INPUT (IF CHARACTERS 'LM-CHARACTER-INPUT-STREAM 'LM-INPUT-STREAM))
              (:OUTPUT (IF CHARACTERS 'LM-CHARACTER-OUTPUT-STREAM 'LM-OUTPUT-STREAM))
              (:IO (IF CHARACTERS 'LM-CHARACTER-IO-STREAM 'LM-IO-STREAM))
              ((:PROBE :PROBE-DIRECTORY) 'LM-PROBE-STREAM))
            :FILE FILE
            :IF-EXISTS IF-EXISTS
            :PATHNAME PATHNAME
            :BYTE-SIZE BYTE-SIZE))))))

(DEFUN LMFS-RENAME-FILE (FILE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION)
  (SETF (GETF (FILE-PLIST FILE) '%TRUENAME-INTERNAL) NIL)
  (IF (EQ NEW-DIRECTORY ':UNSPECIFIC) (SETQ NEW-DIRECTORY '()))
  (IF (MEMQ NEW-NAME '(NIL :UNSPECIFIC)) (SETQ NEW-NAME ""))
  (IF (MEMQ NEW-TYPE '(NIL :UNSPECIFIC)) (SETQ NEW-TYPE ""))
  (IF (MEMQ NEW-VERSION '(NIL :UNSPECIFIC)) (SETQ NEW-VERSION ':NEWEST))
  (LET ((NEW-FILE (LOOKUP-FILE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION
                               ':CREATE ':NEW-VERSION NIL NIL))
        (DIRECTORY (FILE-DIRECTORY FILE)))
    (SETQ NEW-VERSION (FILE-VERSION NEW-FILE))
    (UNWIND-PROTECT
      (PROGN
        (IF (AND (DIRECTORY? FILE)
                 (NOT (AND (EQUAL NEW-TYPE LMFS-DIRECTORY-TYPE)
                           (EQUAL NEW-VERSION LMFS-DIRECTORY-VERSION))))
            (LM-RENAME-ERROR 'RENAME-DIRECTORY-INVALID-TYPE
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (IF (NOT (EQ (FILE-DIRECTORY NEW-FILE) DIRECTORY))
            (LM-RENAME-ERROR 'RENAME-ACROSS-DIRECTORIES
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (IF (NOT (NULL (FILE-OVERWRITE-FILE NEW-FILE)))
            (LM-RENAME-ERROR 'RENAME-TO-EXISTING-FILE
                             NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION))
        (LOCKING (FILE-LOCK FILE)
          (WHEN (FILE-OVERWRITE-FILE FILE)
            ;; Old file was being superseded.
            ;; That is no longer so, though the output file is still there.
            (LET ((OUTFILE (FILE-OVERWRITE-FILE FILE)))
              (REPLACE-FILE-IN-DIRECTORY FILE OUTFILE)
              (SETF (FILE-OVERWRITE-FILE FILE) NIL)
              (SETF (FILE-OVERWRITE-FILE OUTFILE) NIL)
              (DECF (FILE-OPEN-COUNT FILE))))
          (ALTER-FILE FILE
                      NAME NEW-NAME
                      TYPE NEW-TYPE
                      VERSION NEW-VERSION)
          (LOCKING-RECURSIVELY (DIRECTORY-LOCK DIRECTORY)
            (SETF (DIRECTORY-FILES DIRECTORY) (DELQ FILE (DIRECTORY-FILES DIRECTORY)))
            (RPLACA (MEMQ NEW-FILE (DIRECTORY-FILES DIRECTORY))
                    FILE))
          (WRITE-DIRECTORY-FILES DIRECTORY)
          ;; Fake out the LMFS-EXPUNGE-FILE
          ;; to not get an error due to NEW-FILE not being in the directory.
          (SETF (FILE-OVERWRITE-FILE NEW-FILE) FILE)
          (INCF (FILE-OPEN-COUNT FILE))))
      (LMFS-EXPUNGE-FILE NEW-FILE)
      (WRITE-DIRECTORY-FILES (FILE-DIRECTORY NEW-FILE))))
  (setf (getf (file-plist file) '%truename-internal) (FILE-TRUENAME FILE)))

(DEFPROP LM-RENAME-ERROR T :ERROR-REPORTER)
(DEFUN LM-RENAME-ERROR (SIGNAL-NAME NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION)
  (LM-SIGNAL-ERROR SIGNAL-NAME NIL NIL
                   (MAKE-PATHNAME ':HOST "LM" ':DEVICE "DSK"
                                  ':DIRECTORY
                                  (COND ((STRINGP NEW-DIRECTORY) NEW-DIRECTORY)
                                        ((LISTP NEW-DIRECTORY) NEW-DIRECTORY)
                                        (T
                                         (DIRECTORY-NAME NEW-DIRECTORY)))
                                  ':NAME NEW-NAME ':TYPE NEW-TYPE
                                  ':VERSION NEW-VERSION)))

;;;; Property Lists

(DEFUN LMFS-DIRECTORY-LIST (PATHNAME HOST DIRECTORY NAME TYPE VERSION OPTIONS
                            &AUX FILES RESULT)
  "Implements the :DIRECTORY-LIST message for pathnames."
  (LET ((ERROR-P (NOT (MEMQ ':NOERROR OPTIONS)))
        (FASTP (MEMQ ':FAST OPTIONS))
        (DELETED? (MEMQ ':DELETED OPTIONS))
        (not-backed-up? (memq :not-backed-up options))
        (DIRECTORIES-ONLY (MEMQ ':DIRECTORIES-ONLY OPTIONS))
        (OLD-WHOSTATE (SI:PROCESS-WAIT-WHOSTATE CURRENT-PROCESS)))
    (SETF (SI:PROCESS-WAIT-WHOSTATE CURRENT-PROCESS) "Directory")
    (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)
    (UNWIND-PROTECT
        (HANDLING-ERRORS ERROR-P
          (IDENTIFY-FILE-OPERATION ':DIRECTORY-LIST
            (CONS (IF (OR FASTP DIRECTORIES-ONLY)
                      (LIST NIL)
                    (LMFS-DIRECTORY-LIST-HEADER PATHNAME))
                  (IF DIRECTORIES-ONLY
                      (LMFS-ALL-DIRECTORIES HOST ERROR-P)
                    (SETQ FILES
                          (if not-backed-up?
                              (subset #'(lambda (file) (not (file-attribute file :dumped)))
                                      (LOOKUP-FILES DIRECTORY NAME TYPE VERSION DELETED?))
                            (LOOKUP-FILES DIRECTORY NAME TYPE VERSION DELETED?)))
                    (SETQ RESULT (MAKE-LIST (LENGTH FILES)))
                    (DO ((F FILES (CDR F))
                         (R RESULT (CDR R)))
                        ((NULL F) RESULT)
                      (SETF (CAR R)
                            (IF FASTP
                                (LIST (FILE-TRUENAME (CAR F)))
                              (CONS (FILE-TRUENAME (CAR F))
                                    (LMFS-FILE-PROPERTIES (CAR F))))))))))
      (PROGN (SETF (SI:PROCESS-WAIT-WHOSTATE CURRENT-PROCESS) OLD-WHOSTATE)
             (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)))))

(DEFUN LMFS-ALL-DIRECTORIES (HOST ERROR-P)
  (IDENTIFY-FILE-OPERATION ':ALL-DIRECTORIES
    (HANDLING-ERRORS ERROR-P
      (MAPCAR #'(LAMBDA (DIRECTORY)
                  (LIST (MAKE-PATHNAME
                          ':HOST HOST ':DEVICE "DSK"
                          ':DIRECTORY (DIRECTORY-NAME DIRECTORY)
                          ':NAME ':UNSPECIFIC ':TYPE ':UNSPECIFIC
                          ':VERSION ':UNSPECIFIC)))
              (TOP-LEVEL-DIRECTORIES)))))

(DEFVAR LM-UNSETTABLE-PROPERTIES
        '(:LENGTH-IN-BLOCKS :LENGTH-IN-BYTES)
  "Unsettable properties are those which are uniquely determined by the text of the file.")

(DEFVAR LM-DEFAULT-SETTABLE-PROPERTIES
        '(:AUTHOR :BYTE-SIZE :CREATION-DATE :DELETED :CHARACTERS
          :DONT-DELETE :DONT-REAP :NOT-BACKED-UP))

(DEFUN LMFS-FILE-PROPERTIES (FILE &AUX DBS)
  "Given a file, return a plist like a stream would want."
  (LET ((PLIST (COPYLIST (FILE-PLIST FILE))))
    (REMF PLIST '%TRUENAME-INTERNAL)
    (DOLIST (PROP '(:DONT-DELETE :DELETED :DONT-REAP :CHARACTERS :DIRECTORY))
      (IF (FILE-ATTRIBUTE FILE PROP)
          (SETQ PLIST (LIST* PROP T PLIST))))
    (IF (AND (NOT (FILE-ATTRIBUTE FILE ':CLOSED))
             (NOT (MEMQ ':DELETED PLIST)))
        (SETQ PLIST (LIST* ':DELETED T PLIST)))
    (IF (NOT (FILE-ATTRIBUTE FILE ':DUMPED))
        (SETQ PLIST (LIST* ':NOT-BACKED-UP T PLIST)))   ;Backasswardsness.
    (LIST* ':BYTE-SIZE (SETQ DBS (OR (FILE-DEFAULT-BYTE-SIZE FILE) 8))
           ':LENGTH-IN-BLOCKS (FILE-NPAGES FILE)
           ':LENGTH-IN-BYTES (FLOOR (FILE-DATA-LENGTH FILE) DBS)
           ':AUTHOR (FILE-AUTHOR-INTERNAL FILE)
           ':CREATION-DATE (FILE-CREATION-DATE-INTERNAL FILE)
           PLIST)))

(DEFUN LMFS-CHANGE-FILE-PROPERTIES (FILE PLIST)
  (LOCKING (FILE-LOCK FILE)
    (DO ((P PLIST (CDDR P)))
        ((NULL P)
         (IF (FILE-CLOSED? FILE)
             (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE))))
      (SELECTQ (CAR P)
        (:DELETED
         (IF (NULL (CADR P))
             (LMFS-UNDELETE-FILE FILE)
             (LMFS-DELETE-FILE FILE)))
        ((:DONT-DELETE :DONT-REAP :CHARACTERS)
         (SETF (FILE-ATTRIBUTE FILE (CAR P)) (CADR P)))
        (:NOT-BACKED-UP
         (SETF (FILE-ATTRIBUTE FILE ':DUMPED) (NOT (CADR P))))
        (:BYTE-SIZE
         (SETF (FILE-DEFAULT-BYTE-SIZE FILE) (CADR P)))
        (:AUTHOR
         (SETF (FILE-AUTHOR-INTERNAL FILE) (CADR P)))
        (:CREATION-DATE
         (SETF (FILE-CREATION-DATE-INTERNAL FILE) (CADR P)))
        (OTHERWISE
         (COND ((MEMQ (CAR P) LM-UNSETTABLE-PROPERTIES)
                (LM-SIGNAL-ERROR 'UNSETTABLE-PROPERTY NIL NIL (CAR P)))
               ((NOT (SYMBOLP (CAR P)))
                (LM-SIGNAL-ERROR 'INVALID-PROPERTY-NAME NIL NIL (CAR P)))
               ((GET (CAR P) 'ATTRIBUTE)
                (FERROR NIL "CHANGE-FILE-PROPERTIES hasn't been updated to match the defined attributes."))
               (T (PUTPROP (LOCF (FILE-PLIST FILE)) (CADR P) (CAR P)))))))))

(DEFUN LMFS-DIRECTORY-LIST-HEADER (PATHNAME)
  `(NIL :DISK-SPACE-DESCRIPTION
        ,(LMFS-DISK-SPACE-DESCRIPTION (LOOKUP-DIRECTORY (PATHNAME-DIRECTORY PATHNAME)))
        :SETTABLE-PROPERTIES ,LM-DEFAULT-SETTABLE-PROPERTIES
        :PATHNAME ,PATHNAME))

(DEFUN LMFS-DISK-SPACE-DESCRIPTION (&OPTIONAL DIRECTORY &AUX COMMA (BASE 10.) (*NOPOINT T))
  (WITH-OUTPUT-TO-STRING (STREAM)
    (LOOP FOR SYM IN '(PUT-FREE PUT-RESERVED PUT-USED PUT-UNUSABLE)
          FOR STRING IN '("Free=" "Reserved=" "Used=" "Unusable=")
          AS TEM = (AREF PUT-USAGE-ARRAY (SYMEVAL SYM))
          WHEN (PLUSP TEM)
            DO (AND COMMA (FUNCALL STREAM :STRING-OUT ", "))
               (FUNCALL STREAM :STRING-OUT STRING)
               (SI:PRINT-FIXNUM TEM STREAM)
               (SETQ COMMA T))
    (when DIRECTORY     ;If a directory is given, list pages used in that directory.
      (FORMAT STREAM " (~D page~:P used in ~A)"
              (+ (MAP-NPAGES (FILE-MAP DIRECTORY))
                 (LOOP FOR FILE IN (READ-DIRECTORY-FILES DIRECTORY)
                       SUM (MAP-NPAGES (FILE-MAP FILE))))
              (LM-NAMESTRING NIL NIL (or (DIRECTORY-FULL-NAME DIRECTORY) :root) NIL NIL NIL)))))

;;;; Completion

(DEFUN LMFS-COMPLETE-PATH (DIR NAME TYPE DEFAULT-NAME DEFAULT-TYPE OPTIONS)
  "Implements the :COMPLETE-STRING message for pathnames."
  (%STORE-CONDITIONAL (LOCF DIR) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF NAME) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':WILD "*")
  (%STORE-CONDITIONAL (LOCF TYPE) ':UNSPECIFIC "")
  (OR (COMPLETE-PATH-ACLP DIR NAME TYPE)
      (RETURN-FROM LMFS-COMPLETE-PATH (VALUES DIR DEFAULT-NAME DEFAULT-TYPE NIL)))
  (MULTIPLE-VALUE-BIND (NEW-DIR NEW-NAME NEW-TYPE NIL COMPLETION)
      (COMPLETE-PATH DIR NAME TYPE)
    (AND (EQUAL NEW-NAME "")
         (COND ((MEMQ ':WRITE OPTIONS)
                (SETQ NEW-NAME DEFAULT-NAME)
                (SETQ COMPLETION ':NEW))
               (T (SETQ NEW-NAME NIL))))
    (AND (EQUAL NEW-TYPE "")
         (COND ((MEMQ ':WRITE OPTIONS)
                (SETQ NEW-TYPE DEFAULT-TYPE)
                (SETQ COMPLETION ':NEW))
               ((AND (LOOKUP-DIRECTORY NEW-DIR T)
                     (LOOKUP-FILE NEW-DIR NEW-NAME DEFAULT-TYPE ':NEWEST NIL NIL NIL NIL))
                (SETQ NEW-TYPE DEFAULT-TYPE)
                (SETQ COMPLETION ':OLD))
               (T (SETQ NEW-TYPE NIL))))
    (VALUES NEW-DIR NEW-NAME NEW-TYPE COMPLETION)))

;;; Given a list of directory components, complete them all
;;; and return a list of completed directory components.
;;; Second value is non-nil if we added anything to what we were given.

(DEFUN TRY-COMPLETE-DIRECTORY (DIRECTORY &AUX COMPLETION-SO-FAR)
  (DO ((DIRLEFT DIRECTORY (CDR DIRLEFT)))
      ((NULL DIRLEFT) (VALUES COMPLETION-SO-FAR T))
    (LET* ((DIR-COMPONENT (CAR DIRLEFT))
           (COMPLETED-DIRECTORY
             (OR (LOOKUP-DIRECTORY (APPEND COMPLETION-SO-FAR (LIST DIR-COMPONENT)) T)
                 (MULTIPLE-VALUE-BIND (TEM NIL DIRECTORY-COMPLETED)
                     (ZWEI:COMPLETE-STRING DIR-COMPONENT
                                           (IF COMPLETION-SO-FAR
                                               (MAKE-SUBDIRECTORY-ALIST COMPLETION-SO-FAR)
                                             (MAKE-DIRECTORY-ALIST))
                                           '(#/-) T)
                   (AND DIRECTORY-COMPLETED
                        (LOOKUP-DIRECTORY (APPEND COMPLETION-SO-FAR (LIST TEM))))))))
      (IF COMPLETED-DIRECTORY
          (SETQ COMPLETION-SO-FAR
                (APPEND COMPLETION-SO-FAR (LIST (DIRECTORY-NAME COMPLETED-DIRECTORY))))
        (RETURN (VALUES (APPEND COMPLETION-SO-FAR DIRLEFT)
                (NOT (EQUAL (APPEND COMPLETION-SO-FAR DIRLEFT) DIRECTORY))))))))

;;; Although directories can be completed, in practice it is not intuitive to do so.
;;; Therefore, completion barfs if the directory cannot complete to *something* unique.

(DEFUNP COMPLETE-PATH (DIRECTORY NAME TYPE &AUX TEM
                       COMPLETED-DIRECTORY COMPLETED-NAME COMPLETED-TYPE
                       NAME-COMPLETIONS TYPE-COMPLETIONS
                       DIRECTORY-COMPLETED NAME-COMPLETED TYPE-COMPLETED
                       (DEFAULT-CONS-AREA LOCAL-FILE-SYSTEM-AREA))
  (DECLARE (RETURN-LIST DIRECTORY NAME TYPE GENERIC-COMPLETIONS MODE))
  (COND ((CONSP DIRECTORY)
         (MULTIPLE-VALUE (TEM DIRECTORY-COMPLETED)
           (TRY-COMPLETE-DIRECTORY DIRECTORY))
         (OR (SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY TEM T))
             (RETURN (VALUES TEM NAME TYPE NIL (AND DIRECTORY-COMPLETED ':NEW)))))
        ((SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY DIRECTORY T)))
        ((PROGN
           (MULTIPLE-VALUE (TEM NIL DIRECTORY-COMPLETED)
             (ZWEI:COMPLETE-STRING DIRECTORY (MAKE-DIRECTORY-ALIST) '(#/-) T))
           (SETQ COMPLETED-DIRECTORY (LOOKUP-DIRECTORY TEM T))))
        (T (RETURN (VALUES TEM NAME TYPE NIL (AND DIRECTORY-COMPLETED ':NEW)))))
  (MULTIPLE-VALUE (COMPLETED-NAME NAME-COMPLETIONS NAME-COMPLETED)
    (ZWEI:COMPLETE-STRING NAME (MAKE-FILE-NAME-ALIST COMPLETED-DIRECTORY) '(#/-)))
  (IF (AND NAME-COMPLETIONS
           (EQUAL COMPLETED-NAME (CAAR NAME-COMPLETIONS))
           (EQUAL COMPLETED-NAME (CAAR (LAST NAME-COMPLETIONS))))
      ;; Name determined uniquely: try to complete the type.
      (MULTIPLE-VALUE (COMPLETED-TYPE TYPE-COMPLETIONS TYPE-COMPLETED)
        (ZWEI:COMPLETE-STRING TYPE (MAKE-FILE-TYPE-ALIST NAME-COMPLETIONS) '(#/-)))
    (SETQ COMPLETED-TYPE TYPE))
  (VALUES (DIRECTORY-FULL-NAME COMPLETED-DIRECTORY) COMPLETED-NAME COMPLETED-TYPE
          (MAPCAR #'CDR TYPE-COMPLETIONS)
          (AND (OR DIRECTORY-COMPLETED NAME-COMPLETED TYPE-COMPLETED)
               (IF (AND (ASSOC COMPLETED-NAME NAME-COMPLETIONS)
                        (ASSOC COMPLETED-TYPE TYPE-COMPLETIONS))
                   ':OLD ':NEW))))

;;; The code here deals only with strings.
;;; The pathname code is responsible for interpreting it correctly

(DEFUN MAKE-DIRECTORY-ALIST ()
  (LET* ((TOP-DIRECTORIES (TOP-LEVEL-DIRECTORIES))
         (LENGTH (LENGTH TOP-DIRECTORIES))
         (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST)))
    (LOOP FOR D IN TOP-DIRECTORIES
          FOR I FROM 0
          DOING (ASET (CONS (DIRECTORY-NAME D) D) ARRAY I))
    ARRAY))

(DEFUN MAKE-SUBDIRECTORY-ALIST (DIRECTORY)
  (SETQ DIRECTORY (LOOKUP-DIRECTORY DIRECTORY))
  (LOCKING (DIRECTORY-LOCK DIRECTORY)
    (LET* ((LENGTH (LENGTH (DIRECTORY-FILES DIRECTORY)))
           (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST ':LEADER-LENGTH 1)))
      (STORE-ARRAY-LEADER 0 ARRAY 0)
      (LOOP FOR F IN (DIRECTORY-FILES DIRECTORY) BY #'NEXT-GENERIC-FILE
            WHEN (FILE-ATTRIBUTE F ':DIRECTORY)
            DOING (ARRAY-PUSH ARRAY (CONS (FILE-NAME F) F)))
      ARRAY)))

(DEFUN MAKE-FILE-NAME-ALIST (DIRECTORY)
  (LOCKING (DIRECTORY-LOCK DIRECTORY)
    (LET* ((LENGTH (LENGTH (DIRECTORY-FILES DIRECTORY)))
           (ARRAY (MAKE-ARRAY LENGTH ':TYPE 'ART-Q-LIST ':LEADER-LENGTH 1)))
      (STORE-ARRAY-LEADER 0 ARRAY 0)
      (LOOP FOR F IN (DIRECTORY-FILES DIRECTORY) BY #'NEXT-GENERIC-FILE
            DOING (ARRAY-PUSH ARRAY (CONS (FILE-NAME F) F)))
      ARRAY)))

(DEFUN MAKE-FILE-TYPE-ALIST (FILE-NAME-ALIST)
  (LOOP FOR ELEM IN FILE-NAME-ALIST
        COLLECTING (CONS (FILE-TYPE (CDR ELEM)) (CDR ELEM))))

(DEFUN NEXT-GENERIC-FILE (LIST)
  (LET ((NAME (FILE-NAME (CAR LIST)))
        (TYPE (FILE-TYPE (CAR LIST))))
    (DO ((L (CDR LIST) (CDR L)))
        ((NULL L))
      (IF (NOT (AND (STRING-EQUAL NAME (FILE-NAME (CAR L)))
                    (STRING-EQUAL TYPE (FILE-TYPE (CAR L)))))
          (RETURN L)))))

;;;; Debugging Code
;;; This deserves a little reorganization...

;;; "Peek"

(DEFUN DEBUG ()
  (REQUIRE-DISK-CONFIGURATION)
  (FORMAT T "~&Disk Configuration, version ~D~%~%"
          (DC-VERSION))
  (FORMAT T "Lock: ~S~% Psize: ~S~% PUT-Base: ~S~% PUT-Size: ~S~%~%"
          DISK-CONFIGURATION-LOCK
          (DC-PARTITION-SIZE)
          (DC-PUT-BASE)
          (DC-PUT-SIZE))
  (FORMAT T "The PUT is ~:[unlocked~;locked by ~0@*~S~] and is ~:[in~;~]consistent."
          PUT-LOCK
          (= (AREF PAGE-USAGE-TABLE 0) PUT-CONSISTENT))
  (DBG-ROOM)
  (IF (Y-OR-N-P "Would you like to examine the directory structure? ")
      (DBG-EDIT))
  (IF (Y-OR-N-P "Would you like to list the PUT? ")
      (DBG-LIST-PUT)))

(DEFUN DBG-ROOM ()
  (FORMAT T "~&Free: ~D~%Reserved: ~D~%Used: ~D~%Unusable: ~D~%"
          (AREF PUT-USAGE-ARRAY 0)
          (AREF PUT-USAGE-ARRAY 1)
          (AREF PUT-USAGE-ARRAY 2)
          (AREF PUT-USAGE-ARRAY 3)))

(DEFUN DBG-LIST-PUT (&AUX SALV)
  (AND SALVAGER-TABLE
       (PROGN (FORMAT T "~&Look at salvager table too? ")
              (Y-OR-N-P))
       (SETQ SALV T))
  (FORMAT T "~&Currently scanning at ~S~%" PUT-SCANNING-INDEX)
  (DO ((I 1 (1+ I))
       (BLOCK-START 1)
       (CONTENTS (AREF PAGE-USAGE-TABLE 1))
       (SALV-CONTENTS (AND SALV (AREF SALVAGER-TABLE 1))))
      (NIL)
    (COND ((OR ( I (DC-PARTITION-SIZE))
               ( (AREF PAGE-USAGE-TABLE I) CONTENTS)
               (AND SALV (NEQ (AREF SALVAGER-TABLE I) SALV-CONTENTS)))
           (IF (= BLOCK-START (1- I))
               (PRINC BLOCK-START)
             (FORMAT T "~S-~S" BLOCK-START (1- I)))
           (FORMAT T ": ~19T~[Unused~;Reserved~;Used~;Unusable~]" CONTENTS)
           (AND SALV SALV-CONTENTS
                (FORMAT T "~30T(~A)"
                        (COND ((TYPEP SALV-CONTENTS 'DIRECTORY)
                               (FORMAT NIL "Directory ~A" (DIRECTORY-NAME SALV-CONTENTS)))
                              ((TYPEP SALV-CONTENTS 'FILE)
                               (IF (DIRECTORY? SALV-CONTENTS)
                                   SALV-CONTENTS
                                   (FILE-TRUENAME SALV-CONTENTS)))
                              (T SALV-CONTENTS))))
           (TERPRI)
           (AND ( I (DC-PARTITION-SIZE)) (RETURN NIL))
           (SETQ CONTENTS (AREF PAGE-USAGE-TABLE I) BLOCK-START I)
           (AND SALV (SETQ SALV-CONTENTS (AREF SALVAGER-TABLE I)))))))

(DEFUN DBG-EDIT ()
  (LET ((DIRECTORY NIL)
        (FILES NIL)
        (NFILES NIL)
        (NFILE NIL)
        (clear-screen? t)
        (CURRENT-FILE NIL)
        (debug-verbose-p nil))
    (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
      (DBG-EXAMINE-SELECT-DIRECTORY (DC-ROOT-DIRECTORY) NIL))
    (DO-FOREVER
      (FORMAT T "~%~%~S~:[~;[Locked by ~1G~S]~]:" DIRECTORY (DIRECTORY-LOCK DIRECTORY))
      (IF (NULL NFILE)
          (FORMAT T "No files.")
        (SETQ CURRENT-FILE (NTH NFILE FILES))
        (when clear-screen? (cursorpos 'C))
        (FORMAT T "File #~D of ~D file~:[s~]." (1+ NFILE) NFILES (= NFILES 1))
        (if current-file
            (if debug-verbose-p
                (DBG-EXAMINE-SHOW-FILE CURRENT-FILE)
              (progn (format t "~& ~S " current-file)
                     (print-brief-attribute-description-in-brackets current-file)))))
      (FORMAT T "~%Command: ")
      (SELECTOR (READ-CHAR) CHAR-EQUAL
        (#/S (setq clear-screen? (not clear-screen?)))
        (#/P
         (IF (ROOT-DIRECTORY? DIRECTORY)
             (FORMAT T "~%Already at root directory.")
             (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
               (DBG-EXAMINE-SELECT-DIRECTORY (FILE-DIRECTORY DIRECTORY) DIRECTORY))))
        (#/N
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (DBG-REQUIRE-DIRECTORY CURRENT-FILE)
              (MULTIPLE-VALUE (DIRECTORY FILES NFILES NFILE)
                (DBG-EXAMINE-SELECT-DIRECTORY CURRENT-FILE NIL))))
        (#/F
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (IF (= (SETQ NFILE (1+ NFILE)) NFILES)
                  (SETQ NFILE 0))))
        (#/B
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETQ NFILE
                    (1- (IF (= NFILE 0) NFILES NFILE)))))
        (#/C
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETF (FILE-CLOSED? CURRENT-FILE)
                    (NOT (FILE-CLOSED? CURRENT-FILE)))))
        (#/D
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (SETF (FILE-DELETED? CURRENT-FILE)
                    (NOT (FILE-DELETED? CURRENT-FILE)))))
        (#/M
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (LET ((NEW-STATUS (DBG-NEW-STATUS-QUERY)))
                (USING-PUT
                  (SET-MAP-DISK-SPACE (FILE-MAP CURRENT-FILE) NEW-STATUS)))))
        (#/W
         (WRITE-DIRECTORY-FILES DIRECTORY))
        (#/+
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (INCF (FILE-OPEN-COUNT CURRENT-FILE))))
        (#/-
         (AND (DBG-REQUIRE-CURRENT-FILE CURRENT-FILE)
              (DECF (FILE-OPEN-COUNT CURRENT-FILE))))
        (#/Q
         (RETURN T))
        (#/Z
         (cond ((null (dbg-require-current-file current-file)))
               ((yes-or-no-p "Clobber file ~s" current-file)
                (setf (directory-files directory)
                      (delete current-file
                              (directory-files directory))))))
        (#/V
         (setq debug-verbose-p (not debug-verbose-p)))
        ((#/? #\HELP)
         (FORMAT T "~%Available commands:~
P = up to the parent directory.
N = down to the currently selected subdirectory.
F = forward one file.
B = backward one file.
C = toggle the Closed bit.
D = toggle the Deleted bit.
M = modify the disk-space map.
+ = increment the Open count.
- = decrement the Open count.
W = write out the current directory.
V = toggle verbose show file mode.
Z = ZAP current file.
S = toggle clear-screen-between-files mode.
Q = quit.
? = this stuff.~2% --- Hit any Key to continue --- ")
         (tyi))
        (OTHERWISE
         (FORMAT T "~%Invalid. Type Help for help."))))))

(DEFUN DBG-REQUIRE-CURRENT-FILE (CURRENT-FILE)
  (OR (NOT (NULL CURRENT-FILE))
      (PROGN (FORMAT T "~%No current file.") NIL)))

(DEFUN DBG-REQUIRE-DIRECTORY (FILE)
  (OR (DIRECTORY? FILE)
      (PROGN (FORMAT T "~%Current file not a directory.") NIL)))

(DEFUN DBG-EXAMINE-SELECT-DIRECTORY (DIRECTORY CURRENT-FILE-OR-NIL)
  (IF (AND (EQ (DIRECTORY-FILES DIRECTORY) ':DISK)
           (Y-OR-N-P "Directory not in core; read it in? "))
      (READ-DIRECTORY-FILES DIRECTORY))
  (LET ((FILES (DIRECTORY-FILES DIRECTORY)))
    (IF (EQ FILES ':DISK) (SETQ FILES '()))
    (LET ((NFILES (LENGTH FILES))
          (NFILE (IF (NULL CURRENT-FILE-OR-NIL)
                     (AND FILES 0)
                     (FIND-POSITION-IN-LIST CURRENT-FILE-OR-NIL FILES))))
      (VALUES DIRECTORY FILES NFILES NFILE))))

(DEFUN DBG-NEW-STATUS-QUERY ()
  (FQUERY `(:CHOICES (((,PUT-USED "Used.") #/U #/u)
                      ((,PUT-RESERVED "Reserved.") #/R #/r)
                      ((,PUT-FREE "Free.") #/F #/f)
                      ((,PUT-UNUSABLE "Bad.") #/B #/b)))
          "~%Select the new status: "))

(DEFUN DBG-EXAMINE-SHOW-FILE (CURRENT-FILE &OPTIONAL (DEBUG-P T)
                              &AUX FILE
                              NAMESTRING SECONDS MINUTES HOURS DAY MONTH YEAR MAP TEM
                              (DEFAULT-CONS-AREA LOCAL-FILE-SYSTEM-AREA))
  (UNLESS (NULL CURRENT-FILE)
    (SETQ FILE CURRENT-FILE)
    (SETQ NAMESTRING
          (LM-NAMESTRING-FOR-DIRED (FILE-NAME FILE) (FILE-TYPE FILE) (FILE-VERSION FILE)))
    (MULTIPLE-VALUE (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:DECODE-UNIVERSAL-TIME (FILE-CREATION-DATE-INTERNAL FILE)))
    (FORMAT T "~%~24A ~3D ~6D(~2D) ~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
            NAMESTRING
            (MAP-NPAGES (SETQ MAP (FILE-MAP FILE)))
            (OR (AND (SETQ TEM (MAP-LENGTH MAP))
                     (FLOOR TEM (FILE-DEFAULT-BYTE-SIZE FILE)))
                "")
            (FILE-DEFAULT-BYTE-SIZE FILE)
            MONTH DAY YEAR HOURS MINUTES SECONDS
            (FILE-AUTHOR-INTERNAL FILE))
    (cond (debug-p
           (print-brief-attribute-description-in-brackets file)
           (TV:DOPLIST ((FILE-PLIST FILE) PROP IND)
             (FORMAT T "~%   ~A: ~d" IND PROP))
           (FORMAT T "~%   Open Count: ~D" (FILE-OPEN-COUNT FILE))
           (FORMAT T "~%   Overwriting: ~S" (FILE-OVERWRITE-FILE FILE))
           (FORMAT T "~%   Directory: ~S~%" (FILE-DIRECTORY FILE))
           (DESCRIBE-MAP (FILE-MAP FILE))))))

(defun print-brief-attribute-description-in-brackets (file)
           (FORMAT T " {")
           (DO ((I 0 (1+ I))
                (SPACE NIL))
               ((> I 16.))
             (COND ((LDB-TEST (1+ (* I 64.))
                              (FILE-ATTRIBUTES FILE))
                    (IF (NOT (NULL SPACE))
                        (FUNCALL STANDARD-OUTPUT ':TYO #\SP))
                    (SEND STANDARD-OUTPUT ':STRING-OUT
                          (GET-PNAME (AREF ATTRIBUTES I)))
                    (SETQ SPACE T))))
           (FORMAT T "}"))

;;; These functions are useful for editing the directory in-core.

(DEFUN DIRECTORY-TREE-IN-CORE? ()
  (DIRECTORY-SUBTREE-IN-CORE? (DC-ROOT-DIRECTORY)))

(DEFUN DIRECTORY-SUBTREE-IN-CORE? (DIRECTORY)
  (AND (NOT (EQ (DIRECTORY-FILES DIRECTORY) ':DISK))
       (EVERY DIRECTORY
              #'(LAMBDA (FILE)
                  (AND (DIRECTORY? FILE)
                       (DIRECTORY-SUBTREE-IN-CORE? FILE))))))

(DEFUN READ-FILE-SYSTEM-INTO-CORE ()
  "Read the entire directory tree into core."
  (READ-DIRECTORY-SUBTREE-INTO-CORE (DC-ROOT-DIRECTORY)))

(DEFUN READ-DIRECTORY-SUBTREE-INTO-CORE (DIRECTORY)
  (DOLIST (FILE (READ-DIRECTORY-FILES DIRECTORY))
    (IF (DIRECTORY? FILE)
        (READ-DIRECTORY-SUBTREE-INTO-CORE FILE))))

(DEFUN WRITE-CORE-FILE-SYSTEM-ONTO-DISK ()
  "Given a consistent directory tree in core, write the entire
structure out to disk."
  (SAVE-DIRECTORY-TREE ':SAVE-ALL))

(DEFUN LMFS-LIST-DIRECTORIES (&REST DIRECTORIES)
  (OR DIRECTORIES (SETQ DIRECTORIES (TOP-LEVEL-DIRECTORIES)))
  (LOOP FOR DIRECTORY IN DIRECTORIES
        AS LOOKUP = (LOOKUP-DIRECTORY DIRECTORY T)
        WHEN LOOKUP DO (FORMAT T "~%~A" (DIRECTORY-NAME LOOKUP))
        ELSE DO (FORMAT T "~%~A No Such Directory." DIRECTORY)))

(DEFUN LMFS-LIST-FILES (&REST DIRECTORIES)
  (COND ((BOUNDP 'DISK-CONFIGURATION)
         (OR DIRECTORIES (SETQ DIRECTORIES (TOP-LEVEL-DIRECTORIES)))
         (LOOP FOR DIRECTORY IN DIRECTORIES
               AS LOOKUP = (LOOKUP-DIRECTORY DIRECTORY T)
               WHEN LOOKUP DO (FORMAT T "~%~A:" (DIRECTORY-NAME LOOKUP))
                              (DOLIST (FILE (DIRECTORY-FILES LOOKUP))
                                (DBG-EXAMINE-SHOW-FILE FILE NIL))
                              (FORMAT T "~%~%")
               ELSE DO (FORMAT T "~%~A:  No Such Directory.~%~%" DIRECTORY)))
        (T (FORMAT T "~%File system not mounted -- do (FS:BOOT-FILE-SYSTEM)"))))

(DEFUN LMFS-CLOSE-ALL-FILES (&OPTIONAL (ABORTP ':ABORT))
  (LOOP AS STREAM = (WITHOUT-INTERRUPTS (POP LM-FILE-STREAMS-LIST))
        UNTIL (NULL STREAM)
        DO (CATCH-ERROR (FUNCALL STREAM ':CLOSE ABORTP))))

(ADD-INITIALIZATION "Boot File System" '(BOOT-FILE-SYSTEM) '(WARM))
;(ADD-INITIALIZATION "Boot File System" '(BOOT-FILE-SYSTEM) '(AFTER-FULL-GC))
(delete-initialization "Boot File System" '(after-full-gc))
(ADD-INITIALIZATION "Dismount File System" '(DISMOUNT-FILE-SYSTEM) '(BEFORE-COLD))
;(ADD-INITIALIZATION "Dismount File System" '(DISMOUNT-FILE-SYSTEM) '(FULL-GC))
(delete-initialization "Dismount File System" '(full-gc))

;; Functions that were hopelessly obsolete, and have been removed.
;; REMOVE-OBSOLETE-PROPERTIES
;; Call this to remove all obsolete properties in a file system.
;;;; ***** These are broken since the file system was redesigned *****
;; Compression of file bands.
;; LM-COPY-BAND
;; This function copies one file band to another, optimally compressing
;; as it goes.
;; This might be nice to reimplement in terms of an image backup
;; operation.

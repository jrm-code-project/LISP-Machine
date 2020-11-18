;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

#+TI
(defmacro with-decoded-disk-unit ((decoded encoded use . options) &body body)
  (let ((u (gentemp "unit"))
        (d (gentemp "dont-dispose")))
    `(let (,u ,d)
       (unwind-protect
           (let ((,decoded (multiple-value-setq (,u ,d) (decode-unit-argument ,encoded ,use ,@options))))
             ,@body)
         (and ,u (not ,d) (dispose-of-unit ,u))))))

#+TI
(DEFMACRO WITH-DISK-RQB ((RQB . OPTIONS) &BODY BODY)
  (LET ((R (GENTEMP "rqb"))
        (RP (GENTEMP "rqbp")))
    `(LET (,R ,RP)
       (UNWIND-PROTECT
           (LET ((,RQB (MULTIPLE-VALUE (,R ,RP) (MAYBE-GET-DISK-RQB ,@OPTIONS))))
             ,@BODY)
         (AND ,RP ,R (RETURN-DISK-RQB ,R))))))

(DEFUN SXHASH-DISK-PARTITION (UNIT &OPTIONAL (PART NIL PARTP) &KEY
                              SAVE-IN-FILE
                              (PAGES-AT-A-TIME (min #+LMI page-rqb-size 30 85.))
                              (VERBOSE T)
                              (STARTING-HUNDRED 0)
                              (WHOLE-THING-P NIL)
                              (DELAY NIL)
                              (SEQUENTIAL-OPTIMIZE T))
  "Return a list of SXHASH of the blocks in a partition, or save data in a file.
With one argument, a filename, loads saved data from file"
  (COND ((NOT PARTP)
         (LOAD-DISK-PARTITION-SXHASH UNIT))
        ('ELSE
         (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "reading ~A partition" PART))
           (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
               (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT)
             (LET ((PART-COMMENT (PARTITION-COMMENT PART UNIT)))
               (when verbose
                 (format t "~&Computing SXHASH of ~S" PART-COMMENT))
               (LET ((MEASURED-PART-SIZE (AND (NOT WHOLE-THING-P)
                                              (MEASURED-FROM-PART-SIZE UNIT
                                                                       PART
                                                                       PART-BASE
                                                                       PART-SIZE))))
                 (COND ((NOT VERBOSE))
                       (MEASURED-PART-SIZE
                        (FORMAT T "... using measured size of ~D blocks.~%" MEASURED-PART-SIZE))
                       ('ELSE
                        (FORMAT T "... using total size of ~D blocks.~%" PART-SIZE)))
                 (LET ((RESULT (SXHASH-DISK-PORTION
                                 UNIT
                                 PART-BASE
                                 (+ PART-BASE (OR MEASURED-PART-SIZE PART-SIZE))
                                 :OFFSET (* 100. STARTING-HUNDRED)
                                 :PAGES-AT-A-TIME PAGES-AT-A-TIME
                                 :DELAY DELAY
                                 :VERBOSE VERBOSE
                                 :SEQUENTIAL-OPTIMIZE (AND SEQUENTIAL-OPTIMIZE
                                                           (NOT (MAGTAPE-UNIT-P UNIT))))))
                   (COND ((NOT SAVE-IN-FILE)
                          (CONS PART-COMMENT RESULT))
                         ('ELSE
                          (SAVE-DISK-PARTITION-SXHASH PART-COMMENT RESULT SAVE-IN-FILE)))))))))))

(DEFPROP LOAD-DISK-PARTITION-SXHASH
         (#x1A2B #xF7E5 #xABCD #x345E #xFA79 #xFAAA
          #x1234 #x5678 #x9AEA #xCDD4 #xFAFA #x987C)
         T)

(DEFUN LOAD-DISK-PARTITION-SXHASH (FILENAME)
  (WITH-OPEN-FILE (STREAM FILENAME :BYTE-SIZE 16. :CHARACTERS NIL)
    ;; format of file is magic numbers ,PART-COMMENT LENGTH, PART-COMMENT, LENGTH, DATA.
    (DOLIST (X (GET 'LOAD-DISK-PARTITION-SXHASH T))
      (OR (= (SEND STREAM :TYI) X)
          (FERROR NIL "~S is not a disk partition SXHASH file" (SEND STREAM :TRUENAME))))
    (LET ((PART-COMMENT (MAKE-STRING (SEND STREAM :TYI))))
      (DO ((J 0 (1+ J)))
          ((= J (LENGTH PART-COMMENT)))
        (SETF (AREF PART-COMMENT J) (SEND STREAM :TYI)))
      (LET ((L (MAKE-LIST (+ (SEND STREAM :TYI) (ASH (SEND STREAM :TYI) 16.)))))
        (DO ((L L (CDR L)))
            ((NULL L))
          (RPLACA L (+ (SEND STREAM :TYI) (ASH (SEND STREAM :TYI) 16.))))
        (CONS PART-COMMENT L)))))

(DEFUN SAVE-DISK-PARTITION-SXHASH (PART-COMMENT RESULT FILENAME)
  (LET ((FNAME (FS:PARSE-PATHNAME FILENAME)))
    (WHEN (MEMQ (SEND FNAME :NAME) '(NIL :UNSPECIFIC))
      (SETQ FNAME (SEND FNAME :NEW-NAME
                        (WITH-INPUT-FROM-STRING (SI PART-COMMENT)
                          (WITH-OUTPUT-TO-STRING (SO)
                            (DO ((C))
                                ((NOT (SETQ C (SEND SI :TYI))))
                              (SETQ C (CHAR-UPCASE C))
                              (COND ((OR (CHAR<= #\A C #\Z)
                                         (CHAR<= #\0 C #\9)
                                         (CHAR= C #\-))
                                     (SEND SO :TYO C))
                                    ((CHAR= C #\.)
                                     (SEND SO :TYO #\P))
                                    ('ELSE
                                     (SEND SO :TYO #\-)))))))))
    (WHEN (MEMQ (SEND FNAME :TYPE) '(NIL :UNSPECIFIC))
      (SETQ FNAME (SEND FNAME :NEW-TYPE "PDATA")))
    (WITH-OPEN-FILE (STREAM FNAME :DIRECTION :OUTPUT :BYTE-SIZE 16. :CHARACTERS NIL)
      (DOLIST (X (GET 'LOAD-DISK-PARTITION-SXHASH T))
        (SEND STREAM :TYO X))
      (SEND STREAM :TYO (LENGTH PART-COMMENT))
      (DOTIMES (J (LENGTH PART-COMMENT))
        (SEND STREAM :TYO (AREF PART-COMMENT J)))
      (SEND STREAM :TYO (LDB (BYTE 16. 0) (LENGTH RESULT)))
      (SEND STREAM :TYO (LDB (BYTE 16. 16.) (LENGTH RESULT)))
      (DOLIST (X RESULT)
        (SEND STREAM :TYO (LDB (BYTE 16. 0) X))
        (SEND STREAM :TYO (LDB (BYTE 16. 16.) X)))
      (SEND STREAM :TRUENAME))))



(DEFUN SXHASH-DISK-PORTION (UNIT START END &KEY (OFFSET 0) (PAGES-AT-A-TIME 1) DELAY
                            VERBOSE (SEQUENTIAL-OPTIMIZE T))
  (LET ((N-BIG-OPS (FLOOR (- END START OFFSET) PAGES-AT-A-TIME))
        (RQB)
        (HPRINTER (MAKE-SEQUENCE-PRINTER 100.))
        (TIME (TIME))
        (READ-TIME 0)
        (RESULT (NCONS NIL)))
    (WHEN SEQUENTIAL-OPTIMIZE
      (ANTICIPATE-DISK-OPERATIONS UNIT :READ START N-BIG-OPS PAGES-AT-A-TIME))
    (UNWIND-PROTECT
        (DO ((INDEX (+ START OFFSET))
             (AMT)
             (SOFAR RESULT))
            ((>= INDEX END)
             (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
             (WHEN VERBOSE
               (FORMAT
                 T
                 "~&Took ~$ minutes realtime, ~$ reading unit, ~$ Kbytes per second~%"
                 (QUOTIENT TIME (* 60.0 60.0))
                 (QUOTIENT READ-TIME (* 60.0 60.0))
                 (QUOTIENT (* (- END START OFFSET)
                              1024.
                              60.0)
                           (* 1000. TIME))))
             (CDR RESULT))
          (SETQ AMT (MIN (- END INDEX) PAGES-AT-A-TIME))
          (WHEN (NOT (AND RQB (= AMT PAGES-AT-A-TIME)))
            (RETURN-DISK-RQB RQB)
            (SETQ RQB (GET-DISK-RQB AMT)))
          (condition-case ()
              (LET ((TM (TIME)))
                (disk-read rqb unit INDEX)
                (INCF READ-TIME (TIME-DIFFERENCE (TIME) TM)))
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP
                     (- INDEX
                        (COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME UNIT
                                                                 INDEX))))
               (DECF INDEX BACKUP)
               (GO RETRY))))
          (NCONC SOFAR (SXHASH-RQB RQB))
          (SETQ SOFAR (LAST SOFAR))
          (WHEN VERBOSE (FUNCALL HPRINTER (+ AMT (- INDEX START))))
          (IF DELAY
              (PROCESS-SLEEP DELAY)
            (PROCESS-ALLOW-SCHEDULE))
          (INCF INDEX AMT)
          RETRY)
      (AND RQB (RETURN-DISK-RQB RQB)))))


(DEFUN SXHASH-RQB (RQB)
  "Return a list of the SXHASH of the pages in the RQB"
  ;; (%SXHASH-SUBSTRING STRING MASK START END)
  ;; is broken on indirect arrays if START is not 0.
  ;; Also not available on explorer. This is almost as fast.
  ;; Fortunately %STRING-SXHASH is compatible between #+LMI and #+TI.
  (LET ((N (RQB-NPAGES RQB))
        (BUFFER (RQB-8-BIT-BUFFER RQB)))
    (COND ((ZEROP N) NIL)
          ((= N 1)
           (NCONS (%SXHASH-STRING BUFFER #o377)))
          ('ELSE
           (DO ((J 0)
                (STRING (MAKE-ARRAY 1024. :TYPE 'ART-STRING :DISPLACED-TO BUFFER
                                    :DISPLACED-INDEX-OFFSET 0))
                (L NIL))
               (NIL)
             (PUSH (%SXHASH-STRING STRING #o377) L)
             (INCF J)
             (WHEN (= J N)
               (RETURN (NREVERSE L)))
             (CHANGE-INDIRECT-ARRAY STRING 'ART-STRING 1024. BUFFER (* J 1024.)))))))


#+TI
(DEFUN ANTICIPATE-DISK-OPERATIONS (UNIT OPERATION ADDRESS N-OPERATIONS RQB-NPAGES)
  UNIT OPERATION ADDRESS N-OPERATIONS RQB-NPAGES
  NIL)

#+TI
(DEFUN MAGTAPE-UNIT-P (FROM-UNIT)
  FROM-UNIT
  NIL)

#+TI
(DEFUN MEASURED-FROM-PART-SIZE (FROM-UNIT FROM-PART FROM-PART-BASE FROM-PART-SIZE)
  "measure size, or first value of NIL if measured size cant be trusted"
  (COND ((MAGTAPE-UNIT-P FROM-UNIT)
         ;; PROTOCOL KLUDGE. WE ALREADY KNOW THE SIZE IS CORRECT FROM FIND-DISK-PARTITION-FOR-READ
         ;; AND WE ALSO KNOW THAT WE CANNOT DO RANDOM READS ON SUCH A UNIT.
         ())
        ((STRING-EQUAL FROM-PART "LOD" :END1 3)
         (WITH-DISK-RQB (RQB 1)
           (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
           (LET* ((BUF (RQB-BUFFER RQB))
                  (SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE))
                  (FINAL-SIZE (IF (AND (> SIZE #o10) ( SIZE FROM-PART-SIZE))
                                  SIZE
                                FROM-PART-SIZE))
                  (MEMORY-SIZE
                    (SYS-COM-PAGE-NUMBER BUF %SYS-COM-HIGHEST-VIRTUAL-ADDRESS)))
               (AND (> SIZE #o10)
                    ( SIZE FROM-PART-SIZE)
                    (VALUES FINAL-SIZE
                            (IF (= (AREF BUF (* 2 %SYS-COM-BAND-FORMAT)) #o1000)
                                MEMORY-SIZE FINAL-SIZE)
                            (AREF BUF (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION)))))))
        ((OR (STRING-EQUAL FROM-PART "LMC" :END1 3)
             (STRING-EQUAL FROM-PART "MCR" :END1 3))
         (WITH-DISK-RQB (RQB)
           (LET ((LMC-FORMATP (STRING-EQUAL FROM-PART "LMC" :END1 3))
                 (I-MEM-LOCATIONS 0)
                 (MAIN-MEMORY-PAGES 0)
                 (A//M-LOCATIONS 0)
                 (MID-LOCATIONS 0)
                 (MEASURED-SIZE 0)
                 (DISK-WORD-ADDRESS (* PAGE-SIZE FROM-PART-BASE))
                 (M-B-section-type)
                 (M-C-initial-address)
                 (M-D-n-locs))
             (DO-FOREVER
               (SETQ M-B-section-type (READ-lmc-word disk-WORD-address RQB FROM-unit LMC-FORMATP))
               (SETQ M-C-initial-address (READ-LMC-WORD (1+ disk-WORD-address) RQB FROM-unit LMC-FORMATP))
               (SETQ M-D-n-locs (READ-lmc-word (+ 2 disk-WORD-address) RQB FROM-unit LMC-FORMATP))
               (ECASE M-B-section-type
                 (1                        ;I-MEM
                  (setq I-MEM-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ DISK-WORD-ADDRESS 3 (* 2 M-D-n-locs))))
                 (2                        ;D--MEM obsolete
                  nil)
                 (3                        ;MAIN MEMORY
                  (setq MAIN-MEMORY-PAGES M-C-initial-address
                        DISK-WORD-ADDRESS (+ DISK-WORD-ADDRESS 3 1)
                        MEASURED-SIZE (+ M-C-initial-address M-D-n-locs)))
                 (4                        ;A and M Memory
                  (setq A//M-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ 3 DISK-WORD-ADDRESS M-D-n-locs))
                  (return (VALUES MEASURED-SIZE I-MEM-LOCATIONS A//M-LOCATIONS MID-LOCATIONS
                                  MAIN-MEMORY-PAGES)))
                 (5                        ;Macro-IR-Decode
                  (setq MID-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ 3 M-D-n-locs DISK-WORD-ADDRESS))))))))))




#+TI
(DEFUN MAYBE-GET-DISK-RQB (&REST L)
  (DECLARE (ARGLIST &OPTIONAL (N-PAGES 1) LEADER-LENGTH)
           (VALUES RQB RETURNP))
  (COND ((TYPEP (CAR L) 'ARRAY)
         (VALUES (CAR L) NIL))
        ('ELSE
         (VALUES (APPLY 'GET-DISK-RQB L) T))))

#+TI
(DEFUN READ-LMC-WORD (WORD-ADDRESS RQB UNIT LMC-FORMATP)
  (multiple-value-bind (page offset)
      (floor word-address page-size)
    (DISK-READ rqb UNIT page)
    (COND (LMC-FORMATP
           (dpb (aref (rqb-buffer RQB) (1+ (* offset 2)))
                #o2020
                (aref (rqb-buffer RQB) (* offset 2))))
          ('ELSE
           (dpb (aref (rqb-buffer RQB) (* offset 2))
                #o2020
                (aref (rqb-buffer RQB) (1+ (* offset 2))))))))


#+TI
(DEFUN MAKE-SEQUENCE-PRINTER (&OPTIONAL (N 1) &AUX LAST)
  #'(LAMBDA (I)
      (LET ((NEW (FLOOR I N)))
        (COND ((ZEROP NEW))
              ((AND LAST (= NEW LAST)))
              ('ELSE
               (FORMAT T "~D " NEW)
               (SETQ LAST NEW))))))

;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Fonts:(CPTFONTB) -*-

(defmacro with-decoded-disk-unit ((decoded encoded use . options) &body body)
  (let ((u (gentemp "unit"))
        (d (gentemp "dont-dispose")))
    `(let (,u ,d)
       (unwind-protect
           (let ((,decoded (multiple-value-setq (,u ,d) (decode-unit-argument ,encoded ,use ,@options))))
             ,@body)
         (and ,u (not ,d) (dispose-of-unit ,u))))))


(DEFMACRO WITH-DISK-RQB ((RQB . OPTIONS) &BODY BODY)
  (LET ((R (GENTEMP "rqb")))
    `(LET (,R)
       (UNWIND-PROTECT
           (LET ((,RQB (SETQ ,R (GET-DISK-RQB ,@OPTIONS))))
             ,@BODY)
         (AND ,R (RETURN-DISK-RQB ,R))))))


(DEFUN MAGTAPE-UNIT-P (FROM-UNIT)
  (AND (CLOSUREP FROM-UNIT)
       (EQ (CLOSURE-FUNCTION FROM-UNIT) 'FS:BAND-MAGTAPE-HANDLER)))


(DEFUN MEASURED-FROM-PART-SIZE (FROM-UNIT FROM-PART FROM-PART-BASE FROM-PART-SIZE)
  (COND ((MAGTAPE-UNIT-P FROM-UNIT)
         ;; PROTOCOL KLUDGE. WE ALREADY KNOW THE SIZE IS CORRECT FROM FIND-DISK-PARTITION-FOR-READ
         ;; AND WE ALSO KNOW THAT WE CANNOT DO RANDOM READS ON SUCH A UNIT.
         ())
        ((STRING-EQUAL FROM-PART "LOD" :END1 3)
         (WITH-DISK-RQB (RQB 1)
           (LET ((BUF (RQB-BUFFER RQB)))
             (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
             (LET ((SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE)))
               (AND (> SIZE #o10)
                    ( SIZE FROM-PART-SIZE)
                    SIZE)))))
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


(DEFUN NEW-COPY-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART &OPTIONAL &KEY
                                (PAGES-AT-A-TIME (min page-rqb-size 85.))
                                (VERBOSE T)
                                (STARTING-HUNDRED 0)
                                (WHOLE-THING-P NIL)
                                (DELAY NIL)
                                (SEQUENTIAL-OPTIMIZE T)
                                (COMPARE NIL))
  "Copy partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT."
  (WITH-DECODED-DISK-UNIT (FROM-UNIT FROM-UNIT (FORMAT NIL "reading ~A partition" FROM-PART))
    (WITH-DECODED-DISK-UNIT (TO-UNIT TO-UNIT
                                     (FORMAT NIL "~:[writing~;reading~] ~A partition"
                                             COMPARE
                                             TO-PART) NIL T)
      (MULTIPLE-VALUE-BIND (FROM-PART-BASE FROM-PART-SIZE NIL FROM-PART)
          (FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT)
        (MULTIPLE-VALUE-BIND (TO-PART-BASE TO-PART-SIZE NIL TO-PART)
            (IF COMPARE
                (FIND-DISK-PARTITION-FOR-READ TO-PART NIL TO-UNIT)
              (FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
          (LET ((PART-COMMENT (PARTITION-COMMENT FROM-PART FROM-UNIT)))
            (IF VERBOSE (FORMAT T "~&Copying ~S" PART-COMMENT))
            (LET ((MEASURED-FROM-PART-SIZE (AND (NOT WHOLE-THING-P)
                                                (MEASURED-FROM-PART-SIZE FROM-UNIT
                                                                         FROM-PART
                                                                         FROM-PART-BASE
                                                                         FROM-PART-SIZE))))
              (COND (MEASURED-FROM-PART-SIZE
                     (FORMAT T "... using measured size of ~D blocks." MEASURED-FROM-PART-SIZE))
                    ('ELSE
                     (FORMAT T "... using total size of ~D blocks." FROM-PART-SIZE)))
              (UNLESS (>= TO-PART-SIZE (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE))
                (CERROR "leave out last part"
                        "Target partition is only ~D blocks long; ~D needed."
                        TO-PART-SIZE (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE)))
              (FORMAT T "~%")
              (WHEN (NOT COMPARE)
                (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
                (WHEN (AND (CLOSUREP TO-UNIT)
                           (FUNCALL TO-UNIT :HANDLES-LABEL))
                  ;; This is the magtape unit which is sequential
                  (FUNCALL TO-UNIT :PUT PART-COMMENT :COMMENT)
                  (FUNCALL TO-UNIT
                           :PUT
                           (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE)
                           :SIZE)))
              (PROG1 (COPY-DISK-PORTION
                       FROM-UNIT
                       FROM-PART-BASE
                       (+ FROM-PART-BASE (OR MEASURED-FROM-PART-SIZE
                                             FROM-PART-SIZE))
                       TO-UNIT
                       TO-PART-BASE
                       (+ TO-PART-BASE TO-PART-SIZE)
                       :OFFSET (* 100. STARTING-HUNDRED)
                       :PAGES-AT-A-TIME PAGES-AT-A-TIME
                       :COMPARE COMPARE
                       :DELAY DELAY
                       :VERBOSE VERBOSE
                       :SEQUENTIAL-OPTIMIZE (AND SEQUENTIAL-OPTIMIZE
                                                 (NOT (MAGTAPE-UNIT-P FROM-UNIT))
                                                 (NOT (MAGTAPE-UNIT-P TO-UNIT))))
                     (WHEN (NOT COMPARE)
                       (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))))))))))


(DEFUN COPY-DISK-PORTION (FROM-UNIT FROM-START FROM-END TO-UNIT TO-START TO-END &OPTIONAL &KEY
                          (OFFSET 0) (PAGES-AT-A-TIME 1) DELAY (VERBOSE T)
                          (SEQUENTIAL-OPTIMIZE T) COMPARE)
  (LET ((N-BIG-OPS (MIN (FLOOR (- FROM-END FROM-START OFFSET) PAGES-AT-A-TIME)
                        (FLOOR (- TO-END TO-START OFFSET) PAGES-AT-A-TIME)))
        (RQB)(RQB2)
        (COMPARE-OK T)
        (HPRINTER (MAKE-SEQUENCE-PRINTER 100))
        (TIME (TIME)))
    (WHEN SEQUENTIAL-OPTIMIZE
      (ANTICIPATE-DISK-OPERATIONS FROM-UNIT :READ FROM-START N-BIG-OPS PAGES-AT-A-TIME)
      (ANTICIPATE-DISK-OPERATIONS TO-UNIT (IF COMPARE :READ :WRITE)
                                  TO-START N-BIG-OPS PAGES-AT-A-TIME))
    (UNWIND-PROTECT
        (DO ((FROM-INDEX (+ FROM-START OFFSET))
             (TO-INDEX (+ TO-START OFFSET))
             (AMT))
            ((OR (>= FROM-INDEX FROM-END)
                 (>= TO-INDEX TO-END)
                 (< FROM-INDEX FROM-START)
                 (< TO-INDEX TO-START))
             (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
             (IF VERBOSE
                 (FORMAT T "~&Took ~$ minutes realtime, ~$ Kbytes per second~%"
                         (QUOTIENT TIME (* 60.0 60.0))
                         (QUOTIENT (* (- FROM-END FROM-START OFFSET)
                                      1024
                                      60.0)
                                   (* 1000 TIME))))
             COMPARE-OK)
          (SETQ AMT (MIN (- FROM-END FROM-INDEX) (- TO-END TO-INDEX) PAGES-AT-A-TIME))
          (WHEN (NOT (AND RQB (= AMT PAGES-AT-A-TIME)))
            (RETURN-DISK-RQB RQB)
            (SETQ RQB (GET-DISK-RQB AMT)))
          (WHEN (AND COMPARE (NOT (AND RQB2 (= AMT PAGES-AT-A-TIME))))
            (RETURN-DISK-RQB RQB2)
            (SETQ RQB2 (GET-DISK-RQB AMT)))
          (condition-case ()
              (disk-read rqb from-unit FROM-INDEX)
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP
                     (- FROM-INDEX
                        (COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME FROM-UNIT
                                                                 FROM-INDEX))))
               (DECF FROM-INDEX BACKUP)
               (DECF TO-INDEX BACKUP))
             (GO RETRY-READ-WRITE)))
          (CONDITION-CASE ()
              (IF COMPARE
                  (DISK-READ RQB2 TO-UNIT TO-INDEX)
                (DISK-WRITE rqb to-unit TO-INDEX))
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP (- TO-INDEX
                              (FUNCALL (IF COMPARE
                                           #'COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME
                                         #'COPY-DISK-PORTION-NEXT-WRITE-TAPE-VOLUME)
                                       TO-UNIT
                                       TO-INDEX))))
               (DECF FROM-INDEX BACKUP)
               (DECF TO-INDEX BACKUP))
             (GO RETRY-READ-WRITE)))
          (WHEN COMPARE
            (SETQ COMPARE-OK
                  (COMPARE-DISK-DATA RQB RQB2 AMT (- FROM-INDEX FROM-START))))
          (WHEN VERBOSE (FUNCALL HPRINTER (- FROM-INDEX FROM-START)))
          (IF DELAY
              (PROCESS-SLEEP DELAY)
            (PROCESS-ALLOW-SCHEDULE))
          (INCF FROM-INDEX AMT)
          (INCF TO-INDEX AMT)
          RETRY-READ-WRITE)
      (AND RQB (RETURN-DISK-RQB RQB))
      (AND RQB2 (RETURN-DISK-RQB RQB2)))))


(DEFUN COMPARE-DISK-DATA (RQB1 RQB2 AMT BLOCK)
  (OR (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
        (%STRING-EQUAL (RQB-8-BIT-BUFFER RQB1) 0
                       (RQB-8-BIT-BUFFER RQB2) 0
                       (* 1024 AMT)))
      (DO ((C 0 (1+ C))
           (ERRS 0)
           (LIM (* 512 AMT))
           (BUF1 (RQB-BUFFER RQB1))
           (BUF2 (RQB-BUFFER RQB2)))
          ((OR (= C LIM) (= ERRS 3))
           NIL)
        (WHEN (NOT (= (AREF BUF1 C) (AREF BUF2 C)))
          (FORMAT T "~%ERROR in Block ~D. Halfword ~D., S1: ~O S2: ~O "
                  (+ BLOCK (FLOOR C 512))
                  (REMAINDER C 512)
                  (AREF BUF C)
                  (AREF BUF2 C))
          (INCF ERRS)))))



(DEFUN MAKE-SEQUENCE-PRINTER (&OPTIONAL (N 1) &AUX LAST)
  #'(LAMBDA (I)
      (LET ((NEW (FLOOR I N)))
        (COND ((ZEROP NEW))
              ((AND LAST (= NEW LAST)))
              ('ELSE
               (FORMAT T "~D " NEW)
               (SETQ LAST NEW))))))


(DEFUN ANTICIPATE-DISK-OPERATIONS (UNIT OPERATION ADDRESS N-OPERATIONS RQB-NPAGES)
  (AND (INSTANCEP UNIT)
       (SEND-IF-HANDLES UNIT :ANTICIPATE-OPERATIONS OPERATION ADDRESS N-OPERATIONS RQB-NPAGES)))


;;; This next-volume crap is implementated as gross violation of modularity.
;;; DG has implemented new stuff of course, and reimplemented the old FS:MAKE-MT-STREAM
;;; for compatibility, which keeps crufty old code like the FS:BAND-MAGTAPE-HANDLER
;;; working like a charm. A new magtape disk unit implementation is called for of course.

(defun MT-UNIT-MAGTAPE-UNIT (from-unit &AUX WIN)
  ;; a gross violation of modularity.
  (IF (AND (CLOSUREP FROM-UNIT)
           (EQ (CLOSURE-FUNCTION FROM-UNIT) 'FS:BAND-MAGTAPE-HANDLER)
           (SETQ WIN (SYMEVAL-IN-CLOSURE FROM-UNIT 'FS:*BAND-STREAM*)))
      (OR (SEND-IF-HANDLES WIN :UNIT) ;; THE OLD THING
          (SEND (SEND WIN :DEVICE) :UNIT))
    (FERROR NIL "not an open magtape unit: ~S" from-unit)))

(DEFUN COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME (UNIT EXPECT-INDEX)
  (FS:MT-MAYBE-UNLOAD (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                     :list-choices nil)
          "End of tape reached ... mount next tape and type RESUME ")
  (let ((NEXT-INDEX (funcall unit ':prepare-next-volume-for-read)))
    (OR (<= NEXT-INDEX EXPECT-INDEX)
        (cerror "continue anyway"
                "The last tape ended at block ~d., but this tape starts at ~d."
                EXPECT-INDEX
                NEXT-INDEX))
    NEXT-INDEX))


(defun COPY-DISK-PORTION-NEXT-WRITE-TAPE-VOLUME (UNIT INDEX &AUX (BACKUP 10))
  (FS:MT-SPACE-REV 3 (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:mt-write-eof (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:mt-write-eof (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:mt-MAYBE-UNLOAD (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                     :list-choices nil)
          "End of tape reached ... mount next tape and type RESUME ")
  (funcall UNIT ':prepare-next-volume-for-write (- INDEX BACKUP))
  (format t "~&Continuing at partition address ~d.~&" (- INDEX BACKUP))
  (- INDEX BACKUP))

(DEFVAR *REWIND-INSTEAD-OF-UNLOAD* T)

(DEFUN FS:MT-MAYBE-UNLOAD (X)
  (IF *REWIND-INSTEAD-OF-UNLOAD*
      (FS:MT-REWIND X)
    (FS:MT-UNLOAD X)))

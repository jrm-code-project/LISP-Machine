;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-


;;; BANDIF
;;; Prints out what blocks are different between two LOD bands.
;;; For incremental band updates.


(defun band-diff (LOD-A LOD-B &KEY &OPTIONAL (UNIT-A 0) (UNIT-B 0) (QUANTUM 17)
                  &AUX PART-A-BASE PART-A-SIZE PART-B-BASE PART-B-SIZE
                  RQB-A BUF-A RQB-B BUF-B DIFF)
  "returns a list of relative blocks from A that need to be replaced in or added to B
in order to make it the same as LOD-A"
  (MULTIPLE-VALUE-SETQ (part-a-base part-a-size) (FIND-DISK-PARTITION-FOR-READ LOD-A NIL UNIT-A))
  (SETQ PART-A-SIZE (MEASURED-SIZE-OF-PARTITION LOD-A UNIT-A))
  (MULTIPLE-VALUE-SETQ (part-b-base part-b-size) (FIND-DISK-PARTITION-FOR-READ LOD-b NIL UNIT-B))
  (SETQ PART-b-SIZE (MEASURED-SIZE-OF-PARTITION LOD-b UNIT-B))
  (format t "~&~A ~A is ~D blocks long~
             ~%~A ~A is ~D blocks long~%"
          lod-a (PARTITION-COMMENT LOD-A unit-A)
          part-a-size
          lod-b (PARTITION-COMMENT LOD-b unit-B)
          part-b-size)
  (IF (> PART-B-SIZE PART-A-SIZE)
      (FERROR NIL "First partition must be bigger than second compute difference"))
  (UNWIND-PROTECT
      (PROGN (SETQ RQB-A (GET-DISK-RQB QUANTUM))
             (SETQ BUF-A (RQB-BUFFER RQB-A))
             (SETQ RQB-B (GET-DISK-RQB QUANTUM))
             (SETQ BUF-B (RQB-BUFFER RQB-B))
             (DO ((J 0 (+ J QUANTUM)))
                 ((>= J PART-B-SIZE))
               (LET ((N (MIN QUANTUM (- PART-B-SIZE J))))
                 (WIRE-DISK-RQB RQB-A N)
                 (WIRE-DISK-RQB RQB-B N)
                 (DISK-READ-WIRED RQB-A UNIT-A (+ PART-A-BASE J))
                 (DISK-READ-WIRED RQB-B UNIT-B (+ PART-B-BASE J))


(J PART-B-SIZE)


       (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
            (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
       (AND ( (SETQ TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100.)) N-HUNDRED)
            (FORMAT T "~D " (SETQ N-HUNDRED TEM)))

       (ARRAY-FROM-NET BUF1 CONN (* QUANTUM PAGE-SIZE 2))
       (DISK-READ-WIRED RQB TO-UNIT BLOCK)
       (UNLESS (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
                 (%STRING-EQUAL BUF 0
                                BUF1 0
                                (* #o2000 QUANTUM)))




       (UNLESS (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
                 (%STRING-EQUAL BUF 0
                                BUF1 0
                                (* #o2000 QUANTUM)))


     (SETQ RQB
           BUF

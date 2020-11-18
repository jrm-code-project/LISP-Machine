;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; about time we had this! 2/02/86 21:27:41 -GJC
;;; What you will want to do is (partition-hash-info "LODx" :save-in-file "mumble")
;;; on one machine. Then on the other (load "mumble") then
;;; (compare-partition-hash-info (partition-hash-info "LODx"))

(DEFVAR *PARTITION-HASH-INFO* NIL "Bound to the info we want to compare against")

(DEFSTRUCT (PARTITION-HASH-INFO :CONC-NAME :NAMED)
  MACHINE-NAME
  DEVICE
  NAME
  COMMENT
  WHOLE-SIZE
  INDICATED-SIZE
  HASH
  HASH-TYPE
  SIZE)

(DEFVAR *DEFAULT-PARTITION-HASH-TYPE* :16B)

(DEFUN PARTITION-HASH-INFO (PART &OPTIONAL &KEY (UNIT 0) (VERBOSE T) (BLOCK-READ-PAGES (MIN SI:PAGE-RQB-SIZE 85)) SAVE-IN-FILE (HASH-TYPE *DEFAULT-PARTITION-HASH-TYPE*))
  "Reads the blocks of a partition and returns a structure of type PARTITION-HASH-INFO"
  (LET (DEVICE DONT-DISPOSE RQB START WHOLE-SIZE NAME INDICATED-SIZE SIZE HASH COMMENT)
    (UNWIND-PROTECT
        (PROGN (MULTIPLE-VALUE (DEVICE DONT-DISPOSE)
                 (SI:DECODE-UNIT-ARGUMENT UNIT
                                          (FORMAT NIL "READING ~A PARTITION" PART)))
               (MULTIPLE-VALUE (START WHOLE-SIZE NIL NAME)
                 (SI:FIND-DISK-PARTITION-FOR-READ PART NIL DEVICE))
               (SETQ RQB (SI:GET-DISK-RQB 1))
               (SI:DISK-READ RQB DEVICE (1+ START))
               (SETQ INDICATED-SIZE (SI:SYS-COM-PAGE-NUMBER (SI:RQB-BUFFER RQB)
                                                            SI:%SYS-COM-VALID-SIZE))
               (COND ((AND (> INDICATED-SIZE #O10) (<= INDICATED-SIZE WHOLE-SIZE))
                      (SETQ SIZE INDICATED-SIZE))
                     ('ELSE
                      (SETQ SIZE WHOLE-SIZE)))
               (SI:RETURN-DISK-RQB (PROG1 RQB (SETQ RQB NIL)))
               (SETQ COMMENT (SI:PARTITION-COMMENT NAME DEVICE))
               (SETQ HASH (MAKE-ARRAY SIZE))
               (WHEN VERBOSE
                 (FORMAT T "~&LOOKING AT ~S HUNDRED BLOCKS OF ~A (~A)~%"
                         (QUOTIENT SIZE 100) NAME COMMENT))
               (DO ((BLOCK 0 (+ BLOCK BLOCK-READ-PAGES)))
                   ((>= BLOCK SIZE)
                    (LET ((INFO (MAKE-PARTITION-HASH-INFO NAME NAME
                                                          COMMENT COMMENT
                                                          HASH HASH
                                                          HASH-TYPE HASH-TYPE
                                                          WHOLE-SIZE WHOLE-SIZE
                                                          INDICATED-SIZE INDICATED-SIZE
                                                          SIZE SIZE)))
                      (WHEN (NUMBERP DEVICE)
                        (SETF (PARTITION-HASH-INFO-MACHINE-NAME INFO) (SEND SI:LOCAL-HOST :NAME))
                        (SETF (PARTITION-HASH-INFO-DEVICE INFO) DEVICE))
                      (WHEN SAVE-IN-FILE
                        (LET ((*PARTITION-HASH-INFO* INFO))
                          (COMPILER:FASD-SYMBOL-VALUE SAVE-IN-FILE '*PARTITION-HASH-INFO*)))
                      INFO))
                 (LET ((READ-AMOUNT (MIN BLOCK-READ-PAGES (- SIZE BLOCK))))
                   (COND ((NULL RQB)
                          (SETQ RQB (SI:GET-DISK-RQB READ-AMOUNT)))
                         ((NOT (= READ-AMOUNT (SI:RQB-NPAGES RQB)))
                          (SI:RETURN-DISK-RQB (PROG1 RQB (SETQ RQB NIL)))
                          (SETQ RQB (SI:GET-DISK-RQB READ-AMOUNT))))
                   (IF VERBOSE (FORMAT T "~1$ " (QUOTIENT BLOCK 100.0)))
                   (SI:DISK-READ RQB DEVICE BLOCK)
                   (PARTITION-HASH-UPDATE RQB BLOCK READ-AMOUNT HASH HASH-TYPE))))
      (WHEN (AND DEVICE (NOT DONT-DISPOSE))
        (SI:DISPOSE-OF-UNIT DEVICE))
      (WHEN RQB
        (SI:RETURN-DISK-RQB RQB)))))


(DEFUN PARTITION-HASH-UPDATE (RQB BLOCK READ-AMOUNT HASH HASH-TYPE)
  (ECASE HASH-TYPE
    (:NONE)
    (:STRING
     (LET ((STRING (SI:RQB-8-BIT-BUFFER RQB)))
       (DOTIMES (PAGE READ-AMOUNT)
         (SETF (AREF HASH (+ PAGE BLOCK))
               (SI:%SXHASH-SUBSTRING STRING #o377
                                     (ASH PAGE 10)
                                     (ASH (1+ PAGE) 10))))))
    (:16B
     (LET ((BUF (SI:RQB-BUFFER RQB)))
       (DOTIMES (PAGE READ-AMOUNT)
         (DO ((I (ASH PAGE 9) (1+ I))
              (N (ASH (1+ PAGE) 9))
              (H 0 (LOGIOR #xFFFF (+ H (AREF BUF I)))))
             ((= I N)
              (SETF (AREF HASH (+ PAGE BLOCK)) H))))))))


(DEFUN COMPARE-PARTITION-HASH-INFO (INFO-1 &OPTIONAL (INFO-2 *PARTITION-HASH-INFO*))
;  (CHECK-TYPE INFO-1 PARTITION-HASH-INFO)
;  (CHECK-TYPE INFO-2 PARTITION-HASH-INFO)
  (FORMAT T "~&info from ~S (~A) on ~S~%and ..... ~S (~A) on ~S~%"
          (PARTITION-HASH-INFO-NAME INFO-1)
          (PARTITION-HASH-INFO-COMMENT INFO-1)
          (PARTITION-HASH-INFO-MACHINE-NAME INFO-1)
          (PARTITION-HASH-INFO-NAME INFO-2)
          (PARTITION-HASH-INFO-COMMENT INFO-2)
          (PARTITION-HASH-INFO-MACHINE-NAME INFO-2))
  (DO ((HASH-1 (PARTITION-HASH-INFO-HASH INFO-1))
       (HASH-2 (PARTITION-HASH-INFO-HASH INFO-2))
       (SIZE (PARTITION-HASH-INFO-SIZE INFO-1))
       (BLOCK 0 (1+ BLOCK)))
      ((= BLOCK SIZE)
       "ALL OK")
    (OR (= (AREF HASH-1 BLOCK) (AREF HASH-2 BLOCK))
        (FORMAT T "Page ~D different~%" BLOCK))))

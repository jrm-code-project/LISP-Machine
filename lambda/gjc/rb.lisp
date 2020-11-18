;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by GJC
;;; Reason:
;;;  receive-band nowired.
;;; Written 16-Nov-85 13:05:00 by GJC,
;;; while running on Explorer One from band 3
;;; with Experimental System 104.43, microcode 1291.



; From file BAND.LISP#> L.SYS2; DJ: (47)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: SYS2; BAND  "

(DEFUNP RECEIVE-BAND (FROM-MACHINE FROM-PART TO-PART
                      &OPTIONAL SUBSET-START SUBSET-N-BLOCKS (TO-UNIT 0) (WIREP (SI:SELECT-PROCESSOR ((:CADR :LAMBDA)t)
                                                                                                     (:EXPLORER NIL)))
                      &AUX CONN PKT STR TEM RQB BUF (QUANTUM 17.) (WINDOW 36.)
                      NB TOP PART-BASE ORIG-PART-BASE PART-SIZE PART-COMMENT (N-HUNDRED 0))
  "Read the FROM-PART partition from FROM-MACHINE into our partition TO-PART.
If SUBSET-START or SUBSET-N-BLOCKS is specified, they say which part
of the partition to transfer.  They are measured in blocks (or pages).
If a transfer dies, use the last number it printed, times 100.,
as SUBSET-START, to resume where it left off."
  ;; QUANTUM is number of disk blocks we write at once.
  ;; WINDOW is window size for net connection.
  ;; I think it is best if WINDOW is big enough to transfer a whole quantum.
 (OR SUBSET-START (SETQ SUBSET-START 0))
 (UNWIND-PROTECT
   (PROGN
     (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL TO-PART)
       (FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
     (WHEN PART-BASE
       (SETQ CONN (CHAOS:CONNECT FROM-MACHINE
                                 (FORMAT NIL "BAND-TRANSFER READ ~A ~D"
                                         FROM-PART (AND SUBSET-N-BLOCKS
                                                        (LIST SUBSET-START SUBSET-N-BLOCKS)))
                                 WINDOW))
       ;; Receive packet containing size and comment
       (SETQ PKT (CHAOS:GET-NEXT-PKT CONN)
             STR (CHAOS:PKT-STRING PKT))
       (SETQ TEM (LET ((*READ-BASE* 10.)) (READ-FROM-STRING STR)))
       (OR ( TEM PART-SIZE)
           (RETURN (FORMAT NIL "Does not fit in local partition, ~D>~D" TEM PART-SIZE)))
       (SETQ PART-SIZE TEM)
       (SETQ TEM (STRING-SEARCH-CHAR #/SP STR))
       (SETQ PART-COMMENT (READ-FROM-STRING STR NIL (1+ TEM)))
       (FORMAT T "~&Receiving ~A's ~A into ~A: ~D blocks, ~A~%"
               FROM-MACHINE FROM-PART TO-PART PART-SIZE PART-COMMENT)
       (CHAOS:RETURN-PKT PKT)
       (SETQ ORIG-PART-BASE PART-BASE)
       (SETQ PART-BASE (+ PART-BASE SUBSET-START)
             PART-SIZE (- PART-SIZE SUBSET-START))
       (AND SUBSET-N-BLOCKS (SETQ PART-SIZE SUBSET-N-BLOCKS))
       (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
       (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
             BUF (SYS:RQB-BUFFER RQB))
       (SETQ DISK-ERROR-RETRY-COUNT 20.)        ;Try to bypass hardware overrun problem
       (AND WIREP (WIRE-DISK-RQB RQB))
       (SETQ TOP (+ PART-BASE PART-SIZE))
       (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
           (( BLOCK TOP))
         (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
              WIREP
              (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
         (AND ( (SETQ TEM (TRUNCATE (- BLOCK ORIG-PART-BASE) 100.)) N-HUNDRED)
              (FORMAT T "~D " (SETQ N-HUNDRED TEM)))
         (ARRAY-FROM-NET BUF CONN (* QUANTUM PAGE-SIZE 2))
         (IF WIREP (DISK-WRITE-WIRED RQB TO-UNIT BLOCK) (DISK-WRITE RQB TO-UNIT BLOCK)))
       (CHAOS:CLOSE-CONN CONN "Done")
       (OR SUBSET-N-BLOCKS (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))))
   (AND RQB (SYS:RETURN-DISK-RQB RQB))
   (AND CONN (CHAOS:REMOVE-CONN CONN)))
 T)

))

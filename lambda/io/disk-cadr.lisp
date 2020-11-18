;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-


;A hardware bug causes this to lose if xfer > 1 page  (Fixed by DC ECO#1)
;Returns T if they match and NIL if they don't
(DEFUN DISK-READ-COMPARE (RQB UNIT ADDRESS
                  &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
                  do-not-offset)
  "Compare data from disk UNIT at block ADDRESS with the data in RQB.
The length of data compared is simply the number of pages of data in RQB.
UNIT can be either a disk unit number or a function to perform
the transfer.  That is how transfers to other machines' disks are done.
The function receives arguments ':READ-COMPARE, the RQB, and the ADDRESS."
  (lambda-or-cadr-only)
  (COND ((NUMBERP UNIT)
         (WIRE-DISK-RQB RQB)
         (DISK-READ-COMPARE-WIRED RQB UNIT ADDRESS MICROCODE-ERROR-RECOVERY do-not-offset)
         (UNWIRE-DISK-RQB RQB)
         (ZEROP (LDB %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
                     (AREF RQB %DISK-RQ-STATUS-HIGH))))
        ((SEND UNIT (if do-not-offset ':read-compare-physical ':READ-COMPARE) RQB ADDRESS))))

;;; Get STATUS of a unit by doing OFFSET-CLEAR (nebbish command) to it
;;; Leaves the status in the rqb
(DEFUN GET-DISK-STATUS (RQB UNIT)
  (select-processor
    (:cadr
      (DISK-RUN RQB UNIT 0 1 1 %DISK-COMMAND-OFFSET-CLEAR "Offset Clear" T))
    ((:lambda :explorer)
      (ferror nil "obsolete"))))

;;; Power up a drive, return T if successful, NIL if timed out
(DEFUN POWER-UP-DISK (UNIT &AUX RQB)
  "Attempt to turn on disk unit UNIT.  Return T if successful."
  (select-processor
    (:cadr
      (UNWIND-PROTECT
          (PROGN (SETQ RQB (GET-DISK-RQB))
                 (DO ((START-TIME (TIME)))
                     ((OR (> (TIME-DIFFERENCE (TIME) START-TIME) (* 30. 60.))
                          (NOT (LDB-TEST %%DISK-STATUS-LOW-OFF-CYLINDER
                                         (PROGN (GET-DISK-STATUS RQB UNIT)
                                                (AREF RQB %DISK-RQ-STATUS-LOW)))))
                      (NOT (LDB-TEST %%DISK-STATUS-LOW-OFF-CYLINDER
                                     (AREF RQB %DISK-RQ-STATUS-LOW))))
                   (PROCESS-SLEEP 60.)))
        (RETURN-DISK-RQB RQB)))
    ((:lambda :explorer)
      (ferror nil "obsolete"))))

(DEFUN CLEAR-DISK-FAULT (UNIT &AUX RQB)
  "Clear any error indicators on disk drive UNIT."
  (select-processor
    (:cadr
      (UNWIND-PROTECT
          (PROGN (SETQ RQB (GET-DISK-RQB))
                 (DISK-RUN RQB UNIT 0 1 1 %DISK-COMMAND-FAULT-CLEAR "Fault Clear" T))
        (RETURN-DISK-RQB RQB)))
    ((:lambda :explorer)
      (ferror nil "obsolete"))))

;;; A debugging function
(DEFUN PRINT-RQB (RQB)
  (DO ((I 0 (1+ I))
       (L DISK-RQ-HWDS (CDR L)))
      ((NULL (CDR L))
       (PRINT (CAR L))                          ;CCW list
       (DO ((I I (+ I 2))) (())
         (FORMAT T "    ~O" (DPB (AREF RQB (1+ I)) #o2020 (AREF RQB I)))
         (OR (BIT-TEST 1 (AREF RQB I)) (RETURN NIL)))
       (TERPRI))
    (FORMAT T "~%~S  ~O" (CAR L) (AREF RQB I))))

; A hardware bug causes this to lose if xfer > 1 page  (Fixed by DC ECO#1)
;;; Returns T if read-compare difference detected
(DEFUN DISK-READ-COMPARE-WIRED (RQB UNIT ADDRESS
                   &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
                   do-not-offset
                   &AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
                        (HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  (select-processor
    (:cadr
      (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
                (LOGIOR %DISK-COMMAND-READ-COMPARE
                        (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
                "read-compare" nil do-not-offset)
      (LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
                (AREF RQB %DISK-RQ-STATUS-HIGH)))
    ((:lambda :explorer)
      (ferror nil "can't do read-compare on this processor"))))

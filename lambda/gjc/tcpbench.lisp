;;; -*- Mode:LISP; Package:TCP; Base:10; Patch-File:T; Readtable:ZL -*-


(DEFVAR *HANDLE-OWN-EXINTR-WINS* 0)
(DEFVAR *HANDLE-OWN-EXINTR-CALLS* 0)

(DEFVAR *HANDLE-OWN-EXINTR-BUSY* 200)


(DEFUN HANDLE-OWN-EXINTR-STATS (&OPTIONAL RESETP)
  (FORMAT T "~&~D calls, ~D wins, ~D percent, BUSY LOOP COUNT = ~D~%"
          *HANDLE-OWN-EXINTR-CALLS*
          *HANDLE-OWN-EXINTR-WINS*
          (ROUND (TIMES *HANDLE-OWN-EXINTR-WINS* 100)
                 (IF (ZEROP *HANDLE-OWN-EXINTR-CALLS*) 1
                   *HANDLE-OWN-EXINTR-CALLS*))
          *HANDLE-OWN-EXINTR-BUSY* )
  (when RESETP
    (setq *HANDLE-OWN-EXINTR-WINS* 0)
    (setq *HANDLE-OWN-EXINTR-CALLS* 0)))


(DEFUN EXINTR-MESSAGE-READYP ()
  (ZEROP (LOGAND (READ-UCHAR *DMA* (MSG-OFFSET (RMSG-AREA-LASTR RMSGAREA)) 'MESSAGES-STATUS)
                 (LOGIOR MQ-DONE MQ-EXOS))))



(DEFUN EXINTR-WAIT ()
  (PROCESS-WAIT "Exintr wait"
                #'(LAMBDA ()
                    (WHEN (AND (NOT (EQ *EXINTR-IN-PROCESS* :USER))
                               (EXINTR-MESSAGE-READYP))
                      ;; Note, just because the wait function returns T doesnt
                      ;; mean that our process will be run next. That is why
                      ;; we use the flag values of NIL, :SYSTEM, :USER rather than
                      ;; just T and NIL.
                      (SETQ *EXINTR-IN-PROCESS* :SYSTEM)))))



(DEFUN HANDLE-OWN-EXINTR (KP)
  (WHEN (%STORE-CONDITIONAL (LOCF *EXINTR-IN-PROCESS*) NIL :USER)
    (INCF *HANDLE-OWN-EXINTR-CALLS*)
      (DO ((J 0 (1+ J)))
          ((OR (EXINTR-MESSAGE-READYP)
               (= J *HANDLE-OWN-EXINTR-BUSY*))))
      (PROG1 (DO ((K 1 (1+ K)))
                 ((NOT (EXINTR-MESSAGE-READYP))
                  NIL)
               (EXINTR)
               (COND ((KP-READYP KP)
                      (RETURN (INCF *HANDLE-OWN-EXINTR-WINS*)))
                     ((= K 3)
                      (RETURN NIL))))
             (SETQ *EXINTR-IN-PROCESS* NIL))))

(DEFUN BUSY-TIME ()
  (LET ((OFFSET (+ (MSG-OFFSET (RMSG-AREA-LASTR RMSGAREA)) MESSAGES-STATUS))
        (MASK (LOGIOR MQ-DONE MQ-EXOS)))
    (LET ((X (TIME:MICROSECOND-TIME)))
      (DO ((J 0 (1+ J)))
          ((OR (ZEROP (LOGAND MASK (AREF *DMA* OFFSET)))
               (= J *HANDLE-OWN-EXINTR-BUSY*))))
      (- (TIME:MICROSECOND-TIME) X))))

;;; we know that about 25 process switches per second of the kind we need.
;; that is 66 milliseconds. A count of 100 is only 5 miliseconds.

(DEFUN BUSY-TIME-TEST ()
  (FORMAT T "~&COUNT milliseconds used~%")
  (DOTIMES (J 10)
    (LET ((*HANDLE-OWN-EXINTR-BUSY* (* J 30)))
      (FORMAT T "~4D ~$~%" *HANDLE-OWN-EXINTR-BUSY* (QUOTIENT (BUSY-TIME) 1000.0)))))

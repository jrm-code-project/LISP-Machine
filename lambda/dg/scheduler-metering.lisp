;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-

;;; Process scheduler meters
(defconst *psm-total-time* 0)
(defconst *psm-total-before-time* 0)
(defconst *psm-total-count* 0)
(defconst *psm-clock-list-total* 0)
(defconst *psm-clock-list-count* 0)
(defconst *psm-wholine-update-total* 0)
(defconst *psm-wholine-update-count* 0)
(defconst *psm-find-process-total* 0)
(defconst *psm-scheduler-idle-total* 0)
(defconst *psm-scheduler-idle-count* 0)

(defun reset-scheduler-meters ()
  (without-interrupts
    (setq *psm-total-time* 0
          *psm-total-before-time* 0
          *psm-total-count* 0
          *psm-clock-list-total* 0
          *psm-clock-list-count* 0
          *psm-wholine-update-total* 0
          *psm-wholine-update-count* 0
          *psm-find-process-total* 0
          *psm-scheduler-idle-total* 0
          *psm-scheduler-idle-count* 0)))

(defun print-scheduler-meters ()
  (without-interrupts
    (format t "~&Meter:~30tTotal~45tCount~60TAverage~%~
             ----------------------------------------------------------------------")
    (format t "~&Clock~30T~D~45T~D~60T~D"
            *psm-clock-list-total*
            *psm-clock-list-count*
            (if (zerop *psm-clock-list-count*)
                0
              (// *psm-clock-list-total* *psm-clock-list-count* )))
    (format t "~&Wholine~30T~D~45T~D~60T~D"
            *psm-wholine-update-total*
            *psm-wholine-update-count*
            (if (zerop *psm-wholine-update-count*)
                0
              (// *psm-wholine-update-total* *psm-wholine-update-count* )))
    (format t "~&Idle~30T~D~45T~D~60T~D"
            *psm-scheduler-idle-total*
            *psm-scheduler-idle-count*
            (if (zerop *psm-scheduler-idle-count*)
                0
              (// *psm-scheduler-idle-total* *psm-scheduler-idle-count* )))
    (format t "~&Total (before proc)~30T~D~45T~D~60T~D"
            *psm-total-before-time*
            *psm-total-count*
            (if (zerop *psm-total-count*)
                0
              (// *psm-total-before-time* *psm-total-count*)))
    (format t "~&Find process~30T~D~45T~D~60T~D"
            *psm-find-process-total*
            *psm-total-count*
            (if (zerop *psm-total-count*)
                0
              (// *psm-find-process-total* *psm-total-count*)))
    (format t "~&Total (without proc)~30T~D~45T~D~60T~D"
            *psm-total-time*
            *psm-total-count*
            (if (zerop *psm-total-count*)
                0
              (// *psm-total-time* *psm-total-count*)))))

(DEFUN PROCESS-SCHEDULER ()
  (WITHOUT-INTERRUPTS
    (reset-scheduler-meters)
    (DO ((THIS-TIME (TIME) (TIME))
         (LAST-TIME (TIME) THIS-TIME)
         (DELTA-TIME)
         (NEXT-WHO-TIME 0)
         (PROCESS)
         (START-PAGE-FAULTS)
         (START-DISK-TIME)
         (START-TIME)
         (TIME-USED)
         (DISK-TIME-USED)
         (cycle-start-time (%fixnum-microsecond-time) (%fixnum-microsecond-time))
         (scratch-start-time))
        (())
      (%P-STORE-TAG-AND-POINTER (+ %DISK-RUN-LIGHT 2) 0 0)
      (WHEN (PLUSP (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME)))
        (WHEN (NOT (NULL CURRENT-PROCESS))
          (DECF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))
        (CATCH 'PROCESS-WAIT-IN-SCHEDULER  ;minor bum to reduce overhead.  None of these frobs
                ;will normally PROCESS-WAIT, but in exception circumstances (ie GC), they can.
          (setq scratch-start-time (%fixnum-microsecond-time))
          (DOLIST (F CLOCK-FUNCTION-LIST)
            (FUNCALL F DELTA-TIME))
          (incf *psm-clock-list-total* (%pointer-difference (%fixnum-microsecond-time) scratch-start-time))
          (incf *psm-clock-list-count*)
          (WHEN (MINUSP (DECF NEXT-WHO-TIME DELTA-TIME))
            (setq scratch-start-time (%fixnum-microsecond-time))
            (WHEN (FBOUNDP 'TV::WHO-LINE-UPDATE) (TV:WHO-LINE-UPDATE))
            (SETQ NEXT-WHO-TIME *STATUS-LINE-UPDATE-INTERVAL*)
            (incf *psm-wholine-update-total* (%pointer-difference (%fixnum-microsecond-time) scratch-start-time))
            (incf *psm-wholine-update-count*))))
      (setq scratch-start-time (%fixnum-microsecond-time))
      (IF (prog1 (NULL (SETQ PROCESS (SETQ CURRENT-PROCESS (FIND-RUNNABLE-PROCESS))))
                 (incf *psm-find-process-total*
                       (%pointer-difference (%fixnum-microsecond-time) scratch-start-time)))
          (progn
            (setq scratch-start-time (%fixnum-microsecond-time))
            (PROCESS-SCHEDULER-IDLE-FUNCTION)
            (incf *psm-scheduler-idle-total* (%pointer-difference (%fixnum-microsecond-time) scratch-start-time))
            (incf *psm-scheduler-idle-count*)
            (incf *psm-total-time* (%pointer-difference (%fixnum-microsecond-time) cycle-start-time))
            (incf *psm-total-count*))
        (SETF (PROCESS-WAIT-WHOSTATE PROCESS) NIL)
        (SET-PROCESS-WAIT PROCESS #'TRUE NIL)
        (SETQ START-PAGE-FAULTS (FIXNUM-READ-METER %COUNT-DISK-PAGE-READ-OPERATIONS))
        (SETQ START-DISK-TIME (FIXNUM-READ-METER %DISK-WAIT-TIME))
        (SETQ START-TIME (%FIXNUM-MICROSECOND-TIME))
        (%P-STORE-TAG-AND-POINTER (+ %DISK-RUN-LIGHT 2) -1 -1)
        (LET ((STACK-GROUP (PROCESS-STACK-GROUP PROCESS)))
          (incf *psm-total-before-time* (%pointer-difference (%fixnum-microsecond-time) cycle-start-time))
          (IF (TYPEP STACK-GROUP 'STACK-GROUP)
              (STACK-GROUP-RESUME STACK-GROUP NIL)
            (CATCH 'PROCESS-WAIT-IN-SCHEDULER
              (APPLY STACK-GROUP (CDR (PROCESS-INITIAL-FORM PROCESS))))))
        (%P-STORE-TAG-AND-POINTER (+ %DISK-RUN-LIGHT 2) 0 0)
        (INCREMENT-PROCESS-TIME-METER
          (PROCESS-TOTAL-RUN-TIME PROCESS)
          (SETQ TIME-USED (%POINTER-DIFFERENCE
                            (%FIXNUM-MICROSECOND-TIME)
                            START-TIME)))
        (INCREMENT-PROCESS-TIME-METER
          (PROCESS-DISK-WAIT-TIME PROCESS)
          (SETQ DISK-TIME-USED (%POINTER-DIFFERENCE
                                 (FIXNUM-READ-METER %DISK-WAIT-TIME)
                                 START-DISK-TIME)))
        (INCF (PROCESS-PAGE-FAULT-COUNT PROCESS)
              (%POINTER-DIFFERENCE
                (FIXNUM-READ-METER %COUNT-DISK-PAGE-READ-OPERATIONS)
                START-PAGE-FAULTS))
        (LET ((LAST-TIME (PROCESS-LAST-TIME-RUN PROCESS)))
          (SETF (PROCESS-PERCENT-UTILIZATION PROCESS)
                (WITHOUT-FLOATING-UNDERFLOW-TRAPS
                  (+ (IF (NOT (NULL LAST-TIME))
                         (FIX (* (PROCESS-PERCENT-UTILIZATION PROCESS)
                                 (^ *PERCENT-UTILIZATION-DISCOUNT-FACTOR*
                                    (TIME-DIFFERENCE THIS-TIME LAST-TIME))))
                       0)
                     ;; Don't use ROUND -- loses before SYS: SYS2; RAT is loaded.
                     ;;  Believe this, FOOO.
                     (TRUNCATE (+ TIME-USED 500.) 1000.))))
          (SETF (PROCESS-LAST-TIME-RUN PROCESS) THIS-TIME))
        ;; Remember current stack group of process last run, so we resume it next time.
        (UNLESS (PROCESS-SIMPLE-P PROCESS)
          (SETF (PROCESS-STACK-GROUP PROCESS) %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP))
        (incf *psm-total-time* (- (%pointer-difference (%fixnum-microsecond-time) cycle-start-time)
                                  time-used))
        (incf *psm-total-count*)))))

(defconst *random-special-var-counter* 0)

(defun typical-timing-overhead (&aux scratch-start-time time-start)
  (without-interrupts
    (setq time-start (%fixnum-microsecond-time))
    (setq scratch-start-time (%fixnum-microsecond-time))
    (incf *random-special-var-counter* (- (%fixnum-microsecond-time) scratch-start-time))
    (incf *random-special-var-counter*)
    (%pointer-difference (%fixnum-microsecond-time) time-start)))

(defun time-find-runable-process (&optional (times 100)
                                  &aux (total 0)
                                  start-time)
  (dotimes (c times (// total times))
    (setq start-time (%fixnum-microsecond-time))
    (find-runnable-process)
    (incf total (- (%fixnum-microsecond-time) start-time))
    (process-allow-schedule)))

(defconst *process-wait-function-snapshot* ())

(defun snapshot-process-wait-functions (&aux return-list)
  (without-interrupts
    (dolist (l active-processes
               (setq *process-wait-function-snapshot*
                     (reverse return-list)))
      (when (and (car l)
                 (not (memq (second l) (list 'flushed-process #'true))))
        (push (firstn 3 l) return-list)))))

(defun edit-process-wait-functions ()
  (dolist (l *process-wait-function-snapshot*)
    (when (y-or-n-p "Edit ~S : ~S " (car l) (function-name (cadr l)))
      (ed (cadr l)))))

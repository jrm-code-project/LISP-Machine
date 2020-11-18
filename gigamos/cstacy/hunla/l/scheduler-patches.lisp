;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Patch-file: T -*-

(DEFVAR LAST-PROCESS NIL
  "When in the scheduler, the last previous process to execute.")

(DEFSUBST SET-CURRENT-PROCESS (PROCESS)
  (WHEN CURRENT-PROCESS
    (SETQ LAST-PROCESS CURRENT-PROCESS))
  (SETQ CURRENT-PROCESS PROCESS))

(DEFUN PROCESS-SCHEDULER ()
  (WITHOUT-INTERRUPTS
    ;;make sure the global value of inhibit-scheduling-flag is NIL
    ;;(will not be when getting here when doing ABORT of wait function
    ;; from debugger)
    (do ((spp (sg-special-pdl-pointer current-stack-group))
         (sp (sg-special-pdl current-stack-group)))
        ((<= spp 0))
      (cond ((and (eq (aref sp spp) (locf inhibit-scheduling-flag))
                  (not (zerop spp)))
             (let ((loc (locf (aref sp (1- spp)))))
               ;;very important not to clobber the cdr code
               (setf (%p-data-type loc) dtp-fix)
               (setf (%p-pointer loc) (%pointer nil))
               (setf (%p-data-type loc) dtp-symbol))
             (setq spp (- spp 2)))
            ((= (%data-type (aref sp spp)) dtp-locative)
             (setq spp (- spp 2)))
            (t
             (setq spp (- spp 1)))))
    (DO ((THIS-TIME (TIME) (TIME))
         (LAST-TIME (TIME) THIS-TIME)
         (DELTA-TIME)
         (NEXT-WHO-TIME 0)
         (PROCESS)
         (START-PAGE-FAULTS)
         (START-DISK-TIME)
         (START-TIME)
         (TIME-USED)
         (DISK-TIME-USED))
        (())
      (%P-STORE-TAG-AND-POINTER (+ %DISK-RUN-LIGHT 2) 0 0)
      (WHEN (PLUSP (SETQ DELTA-TIME (TIME-DIFFERENCE THIS-TIME LAST-TIME)))
        (WHEN CURRENT-PROCESS
          (DECF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))
        ;;don't blast previous process from the debugger just because
        ;;a clock function gets an error
        (SET-CURRENT-PROCESS NIL)
        (CATCH 'PROCESS-WAIT-IN-SCHEDULER  ;minor bum to reduce overhead.  None of these frobs
                ;will normally PROCESS-WAIT, but in exception circumstances (ie GC), they can.
          (DOLIST (F CLOCK-FUNCTION-LIST)
            (FUNCALL F DELTA-TIME))
          (WHEN (MINUSP (DECF NEXT-WHO-TIME DELTA-TIME))
            (WHEN (FBOUNDP 'TV::WHO-LINE-UPDATE) (TV:WHO-LINE-UPDATE))
            (SETQ NEXT-WHO-TIME *STATUS-LINE-UPDATE-INTERVAL*))))
      (SETQ PROCESS (SET-CURRENT-PROCESS (FIND-RUNNABLE-PROCESS)))
      (IF (NULL PROCESS)
          (PROCESS-SCHEDULER-IDLE-FUNCTION)
        (SETF (PROCESS-WAIT-WHOSTATE PROCESS) NIL)
        (SET-PROCESS-WAIT PROCESS #'TRUE NIL)
        (SETQ START-PAGE-FAULTS (FIXNUM-READ-METER %COUNT-DISK-PAGE-READ-OPERATIONS))
        (SETQ START-DISK-TIME (FIXNUM-READ-METER %DISK-WAIT-TIME))
        (SETQ START-TIME (%FIXNUM-MICROSECOND-TIME))
        (%P-STORE-TAG-AND-POINTER (+ %DISK-RUN-LIGHT 2) -1 -1)
        (LET ((STACK-GROUP (PROCESS-STACK-GROUP PROCESS)))
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
        ))))

(DEFUN PROCESS-INITIALIZE ()
  (UNLESS SCHEDULER-EXISTS
    (OR (FBOUNDP 'MOUSE-WAKEUP)
        (FSET 'MOUSE-WAKEUP #'TRUE))
    (SETQ SCHEDULER-STACK-GROUP (MAKE-STACK-GROUP "Scheduler" :SAFE 0))
    (SETQ INITIAL-PROCESS
          (MAKE-PROCESS "Initial Process"
                        :STACK-GROUP %CURRENT-STACK-GROUP
                        :INITIAL-STACK-GROUP %CURRENT-STACK-GROUP
                        :INITIAL-FORM '(LISP-TOP-LEVEL2)
                        :WARM-BOOT-ACTION 'PROCESS-WARM-BOOT-RESTART)))
 ;; Below is done every time the machine starts up (warm or cold).  Unfortunately,
 ;; the state of the current process has been lost, so it must be reset without
 ;; unwinding it.  This is a total loss, but the only way to prevent this
 ;; is to prevent warm booting.  WARM BOOTING IS STRONGLY DISCOURAGED.
 (COND ((AND (VARIABLE-BOUNDP CURRENT-PROCESS)
             CURRENT-PROCESS)
        (SETQ WARM-BOOTED-PROCESS CURRENT-PROCESS)
        (IF (OR (EQ (PROCESS-WARM-BOOT-ACTION WARM-BOOTED-PROCESS)
                    'PROCESS-WARM-BOOT-RESTART)
                (EQ WARM-BOOTED-PROCESS INITIAL-PROCESS)
                (TYPEP WARM-BOOTED-PROCESS 'SIMPLE-PROCESS))
            ;; Vital system process.  Make sure it can run.
            ;; The initial process => we are running in it now.
            (SEND (PROG1 CURRENT-PROCESS (SET-CURRENT-PROCESS NIL))
                  :RESET T)                     ;T means NOUNWIND
          ;; Some non-essential process.  Leave its state around.
          ;; Later we will ask whether to reset it.
          (SEND WARM-BOOTED-PROCESS :ARREST-REASON :WARM-BOOT)))
       (T (SETQ WARM-BOOTED-PROCESS NIL)))
 ;(SETQ TEMPORARILY-NO-IDLE-SCAVENGING T)
 (SETF (PROCESS-STACK-GROUP INITIAL-PROCESS) %CURRENT-STACK-GROUP)
 (PROCESS-ENABLE INITIAL-PROCESS)               ;enable even if warm-booted out of
 (SET-CURRENT-PROCESS INITIAL-PROCESS)          ;see kludge in PROCESS-CONSIDER-RUNNABILITY
 ;; Do to all active processes what they want done to them.
 ;; The warm-boot-actions can sometimes cause ACTIVE-PROCESSES to get
 ;; re-sorted, so make a copy.
 (DOLIST (P (LOOP FOR (P) IN ACTIVE-PROCESSES UNTIL (NULL P)
                  COLLECT P))
   (SETF (PROCESS-LAST-TIME-RUN P) NIL)
   (SETF (PROCESS-PERCENT-UTILIZATION P) 0)
   (UNLESS (AND (PROCESS-WARM-BOOT-ACTION P)
                (NEQ (PROCESS-WARM-BOOT-ACTION P) ':FLUSH)
                (ERRSET (FUNCALL (PROCESS-WARM-BOOT-ACTION P) P) NIL))
     (SEND P :FLUSH)))

 (SETQ %SCHEDULER-STACK-GROUP SCHEDULER-STACK-GROUP)
 (STACK-GROUP-PRESET SCHEDULER-STACK-GROUP #'PROCESS-SCHEDULER)
 (SETQ SCHEDULER-EXISTS T)
 (FUNCALL SCHEDULER-STACK-GROUP)
 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
 (SB-ON ':CLOCK)
 (setq process-initialize-done t))

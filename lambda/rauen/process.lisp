;;; -*- Mode:LISP; Package:SCHEDULER; Readtable:CL; Base:10 -*-
;;;
;;; PROCESS.LISP

(shadow '(make-process
          process-run-function
          process-run-restartable-function
          process-enable
          process-reset-and-enable
          process-disable
          process-preset
          process-reset
          process-name
          process-stack-group
          process-initial-stack-group
          process-initial-form
          process-wait-function
          process-wait-argument-list
          process-whostate
          ))

(defvar *default-quantum* 60.
  "Don't ask me.  I only work here.")


(defstruct (process)
  name                                  ;Print name
  stack-group                           ;Stack group currently executing on behalf of this process
  (wait-function 'flushed-process)      ;Predicate to determine if process is runnable
  (wait-argument-list nil)              ;Arguments passed to above (use an arg to avoid a closure)
                                        ; This will often be a rest argument in somebody's stack,
                                        ; but it will always be used in a safe manner.
  (wait-whostate "Just Created")        ;The "whostate" string for the who line for use when the
                                        ; process is waiting to run. Set to NIL by the scheduler
                                        ; whenever the process runs.
                                        ; See run-whostate below.
  initial-stack-group                   ;The stack group which PROCESS-RESET (q.v.) will reset to.
  initial-form                          ;Form to preset the initial stack group to when proc is reset.
                                        ; Really cons of function and evaluated args.
  (run-reasons nil)                     ;List of run reasons for this process.
  (arrest-reasons nil)                  ;List of arrest reasons for this process.
  (quantum *default-quantum*)           ;Number of ticks process should run at most before
                                        ; running another process.
  (quantum-remaining 0)                 ;Amount of time remaining for this process to run.
  (priority 0)                          ;Absolute priority of this process.  The larger the number,
                                        ; the more this process wants to run.  It will never be
                                        ; Run for more than its quantum, though.
  (warm-boot-action                     ;Thing to do to this process if it is active when the
   'PROCESS-WARM-BOOT-DELAYED-RESTART)  ; machine is warm-booted.
                                        ;  NIL means the default action
                                        ; (flush it).  If non-NIL, gets funcalled with the process
                                        ; as its argument.
                                        ;The default is to reset it after initializations have been completed
                                        ;[I'm not sure why it's this rather than to leave it alone.]
  (simple-p nil)                        ;T if the process is simple (has no stack group)
  (last-time-run nil)                   ;(TIME) process last woke up, NIL if never
  (total-run-time-low 0)                ;Low bits of total run time in microseconds
  (total-run-time-high 0)               ;High bits of same
  (disk-wait-time-low 0)                ;Low bits of disk wait time in microseconds
  (disk-wait-time-high 0)               ;High bits of same
  (page-fault-count 0)                  ;Number of disk page waits
  (percent-utilization 0)               ;Exponential average of total run time
  closure
  (run-whostate "Run")                  ;The whostate string to be used when the process is running.
  (aentry-with-cons nil)                ;Structure for entry into ACTIVE-PROCESSES.  Or NIL.
  spare-slot-1                          ;Allow experimentation without making new cold load
  spare-slot-2                          ;..
  )


(SETF (DOCUMENTATION 'PROCESS-RUN-WHOSTATE 'FUNCTION)
  "The /"Whostate/" string to be displayed when the process is running")

(SETF (DOCUMENTATION 'PROCESS-WAIT-ARGUMENT-LIST 'FUNCTION)
  "Arguments passed to PROCESS-WAIT-FUNCTION")

(SETF (DOCUMENTATION 'PROCESS-NAME 'FUNCTION)
  "The name of PROCESS (a string)")

(SETF (DOCUMENTATION 'PROCESS-WAIT-FUNCTION 'FUNCTION)
  "Predicate to determine if PROCESS is runnable.")

(SETF (DOCUMENTATION 'PROCESS-INITIAL-FORM 'FUNCTION)
  "Returns the initial form the stack group is preset to when PROCESS is reset.")

(SETF (DOCUMENTATION 'PROCESS-INITIAL-STACK-GROUP 'FUNCTION)
  "Returns the stack group which PROCESS-RESET will reset to.")

(SETF (DOCUMENTATION 'PROCESS-STACK-GROUP 'FUNCTION)
  "Returns the stack group currently executing on behalf of PROCESS.")

(SETF (DOCUMENTATION 'PROCESS-WAIT-WHOSTATE 'FUNCTION)
  "The /"Whostate/" string for the wholine, etc to be displayed when the process is
waiting to run. NIL when the process is running.")

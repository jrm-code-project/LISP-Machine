;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

;;; Processes

(DEFVAR CURRENT-PROCESS NIL "The process which is currently executing.")
(DEFVAR INITIAL-PROCESS)                ;The first process made
;DEFVAR'ed in PROCES
;(DEFVAR ACTIVE-PROCESSES)              ;Alist of all processes being considered for running
                                        ; and their wait conditions.  This list structure is
                                        ; all in contiguous memory to decrease the size of
                                        ; the scheduler's working set.
(DEFVAR ALL-PROCESSES NIL
  "A list of all processes that have not been /"killed/".")
(DEFVAR PROCESS-ACTIVE-LENGTH 30.)      ;Initial length of ACTIVE-PROCESSES
(DEFVAR WARM-BOOTED-PROCESS NIL)        ;When you warm boot
(DEFVAR DELAYED-RESTART-PROCESSES NIL)  ;Processes to be restarted after initialization

;;; Scheduling

(DEFVAR INHIBIT-SCHEDULING-FLAG :UNBOUND
  "Non-NIL inhibits clock and process-switching")
(DEFVAR CLOCK-FUNCTION-LIST NIL)        ;At clock time, each element is funcalled on the
                                        ; number of 60ths that have elapsed recently.
(DEFVAR SCHEDULER-STACK-GROUP)          ;The stack group in which the scheduler runs.
(DEFVAR SCHEDULER-EXISTS NIL)           ;T if the scheduler and processes are set up.
(DEFVAR INHIBIT-IDLE-SCAVENGING-FLAG NIL) ;If NIL scavenger runs when no processes runnable
(DEFVAR GC-IDLE-SCAVENGE-QUANTUM 100.)  ;Argument to GC:SCAVENGE used in that case
(DEFVAR DEFAULT-QUANTUM 60.)            ;Defaultly run each process for at least one second

(DEFVAR SYSTEM-BEING-INITIALIZED-FLAG T) ;T while coming up, mainly for error-handler


(DEFPARAMETER *DEFAULT-PROCESS-CLOSURE-VARIABLES*
              '(+ ++ +++ * ** *** // //// ////// - *VALUES* *READTABLE*)
  "A list of special variables which are by default bound at top level inside each process.
Variables in this list should be [some criterion which I haven't figured out yet]")

;;; Processes
(DEFFLAVOR PROCESS
 (NAME                          ;Print name
  STACK-GROUP                   ;Stack group currently executing on behalf of this process
  (WAIT-FUNCTION 'FLUSHED-PROCESS) ;Predicate to determine if process is runnable
  (WAIT-ARGUMENT-LIST NIL)      ;Arguments passed to above (use an arg to avoid a closure)
                                ; This will often be a rest argument in somebody's stack,
                                ; but it will always be used in a safe manner.
  (WAIT-WHOSTATE "Just Created");The "whostate" string for the who line for use when the
                                ; process is waiting to run. Set to NIL by the scheduler
                                ; whenever the process runs.
                                ; See run-whostate below.
  INITIAL-STACK-GROUP           ;The stack group which PROCESS-RESET (q.v.) will reset to.
  INITIAL-FORM                  ;Form to preset the initial stack group to when proc is reset.
                                ; Really cons of function and evaluated args.
  (RUN-REASONS NIL)             ;List of run reasons for this process.
  (ARREST-REASONS NIL)          ;List of arrest reasons for this process.
  (QUANTUM DEFAULT-QUANTUM)     ;Number of ticks process should run at most before
                                ; running another process.
  (QUANTUM-REMAINING 0)         ;Amount of time remaining for this process to run.
  (PRIORITY 0)                  ;Absolute priority of this process.  The larger the number,
                                ; the more this process wants to run.  It will never be
                                ; run for more than its quantum, though.
  (WARM-BOOT-ACTION             ;Thing to do to this process if it is active when the
   'PROCESS-WARM-BOOT-DELAYED-RESTART)  ; machine is warm-booted.
                                ;  NIL means the default action
                                ; (flush it).  If non-NIL, gets funcalled with the process
                                ; as its argument.
        ;The default is to reset it after initializations have been completed
        ;[I'm not sure why it's this rather than to leave it alone.]
  (SIMPLE-P NIL)                ;T if the process is simple (has no stack group)
  (LAST-TIME-RUN NIL)           ;(TIME) process last woke up, NIL if never
  (TOTAL-RUN-TIME-LOW 0)        ;Low bits of total run time in microseconds
  (TOTAL-RUN-TIME-HIGH 0)       ;High bits of same
  (DISK-WAIT-TIME-LOW 0)        ;Low bits of disk wait time in microseconds
  (DISK-WAIT-TIME-HIGH 0)       ;High bits of same
  (PAGE-FAULT-COUNT 0)          ;Number of disk page waits
  (PERCENT-UTILIZATION 0)       ;Exponential average of total run time
  CLOSURE
; this ivar not patched in 99
  (RUN-WHOSTATE "Run")          ;The whostate string to be used when the process is running.
  (AENTRY-WITH-CONS nil)        ;Structure for entry into ACTIVE-PROCESSES.  Or NIL.
 ;SPARE-SLOT-1                  ;Allow experimentation without making new cold load
  SPARE-SLOT-2                  ;..
  )
  ()
  :ORDERED-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:GETTABLE-INSTANCE-VARIABLES NAME STACK-GROUP WAIT-FUNCTION WAIT-ARGUMENT-LIST
                                INITIAL-STACK-GROUP INITIAL-FORM
                                RUN-REASONS ARREST-REASONS QUANTUM QUANTUM-REMAINING
                                PRIORITY WARM-BOOT-ACTION SIMPLE-P
                                LAST-TIME-RUN PAGE-FAULT-COUNT)
  (:SETTABLE-INSTANCE-VARIABLES WARM-BOOT-ACTION WAIT-WHOSTATE RUN-WHOSTATE)
  (:INITABLE-INSTANCE-VARIABLES NAME STACK-GROUP WAIT-FUNCTION WAIT-ARGUMENT-LIST
                                INITIAL-STACK-GROUP INITIAL-FORM
                                RUN-REASONS ARREST-REASONS QUANTUM
                                PRIORITY WARM-BOOT-ACTION SIMPLE-P)
  (:INIT-KEYWORDS :FLAVOR
                  ;; Keywords for stack group
                  :SG-AREA :REGULAR-PDL-AREA :SPECIAL-PDL-AREA :REGULAR-PDL-SIZE
                  :SPECIAL-PDL-SIZE :CAR-SYM-MODE :CAR-NUM-MODE :CDR-SYM-MODE :CDR-NUM-MODE
                  :SWAP-SV-ON-CALL-OUT :SWAP-SV-OF-SG-THAT-CALLS-ME :TRAP-ENABLE :SAFE
                  :CLOSURE-VARIABLES
                  :WHOSTATE)                    ;for compatibility
  (:DEFAULT-INIT-PLIST :CLOSURE-VARIABLES *DEFAULT-PROCESS-CLOSURE-VARIABLES*))
;; methods are in SYS2; PROCES


(SETF (DOCUMENTATION 'PROCESS-WAIT-WHOSTATE 'FUNCTION)
  "The /"Whostate/" string for the wholine, etc to be displayed when the process is
waiting to run. NIL when the process is running.")
(deff process-whostate 'process-wait-whostate)
(compiler:make-obsolete process-whostate "this function is now SI:PROCESS-WAIT-WHOSTATE")

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

;This probably should be FLUSHED.  Problem is, you can not send ANY messages at all in
; a simple process.  If the GC flips and you get to INSTANCE-HASH-FAILURE, it can try
; to process wait, which will throw out, which can leave locks seized, etc.
;;>> That is what unwind-protect is for.  All locking code uses it.  What is the problem?
(DEFFLAVOR SIMPLE-PROCESS () (PROCESS)
  (:DEFAULT-INIT-PLIST :SIMPLE-P T
                       :WAIT-FUNCTION #'TRUE)
  (:DOCUMENTATION "DO NOT USE THIS!! SEE WARNINGS in LISTING!!
A process that has no stack group of its own.
It runs in the scheduler stack group and keeps no stack state between runs."))

(DEFFLAVOR COROUTINING-PROCESS ((COROUTINE-STACK-GROUPS NIL)) (PROCESS)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION "A process that has several stack groups that call each other."))

;;; Two word meters
(DEFMACRO RESET-PROCESS-TIME-METER (SLOT-NAME)
  (LET ((LOW (INTERN (STRING-APPEND SLOT-NAME "-LOW")))
        (HIGH (INTERN (STRING-APPEND SLOT-NAME "-HIGH"))))
    `(SETQ ,LOW 0 ,HIGH 0)))

(DEFMACRO FIXNUM-PROCESS-TIME-METER (SLOT-NAME)
  (LET ((LOW (INTERN (STRING-APPEND SLOT-NAME "-LOW")))
        (HIGH (INTERN (STRING-APPEND SLOT-NAME "-HIGH"))))
    `(DPB ,HIGH #.(byte (integer-length most-positive-fixnum)
                        (integer-length most-positive-fixnum))
          ,LOW)))

(DEFMACRO INCREMENT-PROCESS-TIME-METER ((SLOT-NAME PROCESS) INCREMENT)
  (LET ((LOW (INTERN (STRING-APPEND SLOT-NAME "-LOW")))
        (HIGH (INTERN (STRING-APPEND SLOT-NAME "-HIGH"))))
    `(LET ((TEM (%POINTER-PLUS ,INCREMENT (,LOW ,PROCESS))))
       (IF (NOT (MINUSP TEM))
           (SETF (,LOW ,PROCESS) TEM)
         (SETF (,LOW ,PROCESS) (logand most-positive-fixnum TEM))
         (INCF (,HIGH ,PROCESS))))))

;;; A version of TIME:FIXNUM-MICROSECOND-TIME which is open-coded and loaded earlier
;;; so that the scheduler can call it
(DEFSUBST FIXNUM-MICROSECOND-TIME-FOR-SCHEDULER-FOR-CADR ()
  (LET ((LOW (%UNIBUS-READ #o764120))
        (HIGH (%UNIBUS-READ #o764122)))
    (DPB HIGH (BYTE 7. 16.) LOW)))

;;; An open-coded, fixnum-returning version of READ-METER.
(DEFMACRO FIXNUM-READ-METER (NAME)
   (LET ((A-OFF (OR (FIND-POSITION-IN-LIST NAME A-MEMORY-COUNTER-BLOCK-NAMES)
                    (FERROR "~S is not a valid counter name" NAME))))
     `(%P-POINTER
        (+ %COUNTER-BLOCK-A-MEM-ADDRESS A-MEMORY-VIRTUAL-ADDRESS ,A-OFF))))

(DEFMACRO %INCREMENT (X) `(SETF ,X (%POINTER-PLUS ,X 1)))

(DEFSTRUCT (PROCESS-QUEUE :NAMED-ARRAY-LEADER
                          (:CONSTRUCTOR MAKE-PROCESS-QUEUE-INTERNAL) (:ALTERANT NIL))
  NAME)

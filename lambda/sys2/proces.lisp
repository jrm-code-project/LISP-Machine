;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-

;;; Process system and scheduler

; A process is an instance which embodies one or several stack groups as well as
; appropriate variables to determine the stack-group's status and runnability.
; See PRODEF

;;; ACTIVE-PROCESSES    An alist of all processes that are runnable.
;;;                     A process is runnable if it has at least one run
;;;                     reason, and no arrest reasons.  This list is maintained
;;;                     because it is considered too expensive to have the
;;;                     scheduler inspect each process' run and arrest reasons.
;;; Each element on ACTIVE-PROCESSES looks like:
;;;     (process wait-function wait-arglist priority <slots for wait args>)
;;;     wait-arglist is usually a tail of this list
;;; CURRENT-PROCESS     The process that is currently running.  NIL inside the
;;;                     scheduler.

(DEFVAR ACTIVE-PROCESSES-ELEMENT-SIZE 9)
(DEFVAR ACTIVE-PROCESSES-PREFIX-SIZE 4) ;Process, wait-function, wait-arglist, priority
(defvar active-processes-args-length (- active-processes-element-size  ;# args which can be
                                        active-processes-prefix-size)) ; accomodated.

(DEFUN MAKE-ACTIVE-PROCESSES (LEN &AUX AP)
  (WITHOUT-INTERRUPTS
    (SETQ AP (MAKE-LIST LEN :AREA PERMANENT-STORAGE-AREA :CDR-CODED NIL))
    (DO ((L AP (CDR L)))
        ((NULL L) AP)
      (SETF (CAR L) (MAKE-LIST ACTIVE-PROCESSES-ELEMENT-SIZE :AREA PERMANENT-STORAGE-AREA)))
    ))

(defun make-aentry-with-cons ()
  (cons-in-area
    (MAKE-LIST ACTIVE-PROCESSES-ELEMENT-SIZE :AREA PERMANENT-STORAGE-AREA)
    nil
    permanent-storage-area))

(DEFVAR ACTIVE-PROCESSES nil
  "List of processes considered for running.  They have run-reasons and are not arrested.")

;This attempts to increase locality by creating data structure for ACTIVE-PROCESSES
; at one crack and recycling it.
(defvar idle-aentries-for-active-processes (MAKE-ACTIVE-PROCESSES PROCESS-ACTIVE-LENGTH))


;this guy no longer used.
;;; Make an entry for this process in ACTIVE-PROCESSES, with its current wait condition,
;;; when it first becomes runnable.  Try not to cons.
;(DEFUN PROCESS-ACTIVE-ENTRY (PROC &AUX AENTRY)
;  (WITHOUT-INTERRUPTS
;    (PROCESS-ALL-PROCESSES PROC T)
;    (OR (SETQ AENTRY (ASSQ PROC ACTIVE-PROCESSES))
;       (SETQ AENTRY (ASSQ NIL ACTIVE-PROCESSES))
;       ;; No free entries => make the list as long, still contiguous.
;       (LET* ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
;              (TEM (MAPCAR #'(LAMBDA (IGNORE)
;                               (MAKE-LIST ACTIVE-PROCESSES-ELEMENT-SIZE
;                                          :AREA PERMANENT-STORAGE-AREA))
;                       ACTIVE-PROCESSES)))
;         (SETQ DEFAULT-CONS-AREA PERMANENT-STORAGE-AREA)
;         (SETQ ACTIVE-PROCESSES
;               (APPEND ACTIVE-PROCESSES TEM NIL))
;         (SETQ AENTRY (ASSQ NIL ACTIVE-PROCESSES))))
;    (SETF (FIRST AENTRY) PROC)
;    (SETF (FOURTH AENTRY) (PROCESS-PRIORITY PROC))
;    (PROCESS-REINSERT-AENTRY AENTRY)
;    (SET-PROCESS-WAIT PROC (PROCESS-WAIT-FUNCTION PROC) (PROCESS-WAIT-ARGUMENT-LIST PROC))))

(DEFUN PROCESS-ALL-PROCESSES (PROC ADDP)
  "Add or remove PROC in ALL-PROCESSES.
Must be called with interrupts inhibited."
  (IF ADDP
      (PUSHNEW PROC ALL-PROCESSES :TEST #'EQ)
      (SETQ ALL-PROCESSES (DELQ PROC ALL-PROCESSES))))

(DEFUN SET-PROCESS-WAIT (PROC FUN ARGS &AUX IDX APE)
  "Set the wait condition of process PROC to function FUN applied to ARGS.
PROC will run when (APPLY FUN ARGS) returns non-NIL."
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-WAIT-FUNCTION PROC) FUN)
  ;Following IF is temporary.  Remove after system 121.  ***
    (if (= (%p-data-type (locf (process-aentry-with-cons proc)))
           dtp-null)
        (setf (process-aentry-with-cons proc) nil))
    (cond ((SETQ APE (car (process-aentry-with-cons proc)))  ;(ASSQ PROC ACTIVE-PROCESSES)
           (SETF (SECOND APE) FUN)
           (COND (( (SETQ IDX (- ACTIVE-PROCESSES-args-length (LENGTH ARGS)))
                     0)
                  (LET ((L (NTHCDR active-processes-prefix-size APE)))
                    (dotimes (c idx)    ;wipe unused slots.
                      (setf (car l) nil)
                      (setq l (cdr l)))
                    (SETF (THIRD APE) L)
                    (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) L)  ;**caution**
        ;process-wait-argument-list is "unsafe" list structure if A-ENTRY is recycled.
                    (DO ((L L (CDR L))
                         (ARGS ARGS (CDR ARGS)))
                        ((NULL ARGS))
                      (SETF (CAR L) (CAR ARGS)))))
                 (T                     ;get here if too many args for pre-allocated list (>5).
                                        ; Formerly, lost completely, case may not arise.
                  (let ((a (copy-list args)))
                    (SETF (THIRD APE) A)
                    (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) A)))))
          (t (setf (process-wait-argument-list proc) (copylist args))))))

;(DEFUN SET-PROCESS-WAIT (PROC FUN ARGS &AUX IDX APE)
;  "Set the wait condition of process PROC to function FUN applied to ARGS.
;PROC will run when (APPLY FUN ARGS) returns non-NIL."
;  (WITHOUT-INTERRUPTS
;    (SETF (PROCESS-WAIT-FUNCTION PROC) FUN)
;    (cond ((SETQ APE (ASSQ PROC ACTIVE-PROCESSES))
;          (SETF (SECOND APE) FUN)
;          (COND (( (SETQ IDX (- ACTIVE-PROCESSES-ELEMENT-SIZE (LENGTH ARGS)))
;                    ACTIVE-PROCESSES-PREFIX-SIZE)
;                 (LET ((L (NTHCDR IDX APE)))
;                   (SETF (THIRD APE) L)
;                   (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) L)
;                   (DO ((L L (CDR L))
;                        (ARGS ARGS (CDR ARGS)))
;                       ((NULL ARGS))
;                     (SETF (CAR L) (CAR ARGS)))))
;                (T                     ;get here if too many args for pre-allocated list (>5).
;                                       ; Formerly, lost completely, case may not arise.
;                 (let ((a (copy-list args)))
;                   (SETF (THIRD APE) A)
;                   (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) A)))))
;         (t (setf (process-wait-argument-list proc) (copylist args))))))

(DEFUN MAKE-PROCESS (NAME &REST INIT-ARGS)
  "Create a process, with name NAME.
:FLAVOR specifies the flavor of process to make.
:SIMPLE-P if non-NIL specifies flavor SI:SIMPLE-PROCESS.
If :FLAVOR and :SIMPLE-P are NIL, the flavor SI:PROCESS is used.
:WARM-BOOT-ACTION is a function to call on warm booting,
 or :FLUSH meaning flush the process.  The default is to restart it.
 SI:PROCESS-WARM-BOOT-RESET kills the process.
 SI:PROCESS-WARM-BOOT-RESTART restarts at an earlier stage of booting.
:QUANTUM is in 60'ths and defaults to one second.
:PRIORITY defaults to 0; larger numbers run more often.
:STACK-GROUP specifies the stack group for this process to run in.
If that is omitted, the keyword arguments :SG-AREA,
:REGULAR-PDL-AREA, :SPECIAL-PDL-AREA, :REGULAR-PDL-SIZE,
and :SPECIAL-PDL-SIZE are passed on to MAKE-STACK-GROUP."
  (DECLARE (ARGLIST NAME &KEY SIMPLE-P FLAVOR STACK-GROUP WARM-BOOT-ACTION QUANTUM PRIORITY
                              SG-AREA REGULAR-PDL-AREA SPECIAL-PDL-AREA
                              REGULAR-PDL-SIZE SPECIAL-PDL-SIZE
                              &ALLOW-OTHER-KEYS))
  (OR (CAR INIT-ARGS) (SETQ INIT-ARGS (CDR INIT-ARGS))) ;For backward compatibility
  (WITH-STACK-LIST* (INIT-ARGS :NAME NAME INIT-ARGS)
    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))     ;don't cons processes in temp areas!
      (INSTANTIATE-FLAVOR (OR (GETF INIT-ARGS :FLAVOR)
                              (AND (GETF INIT-ARGS :SIMPLE-P) 'SIMPLE-PROCESS)
                              'PROCESS)
                          (LOCF INIT-ARGS)
                          T))))

;(DEFF PROCESS-CREATE 'MAKE-PROCESS)
;(compiler::make-obsolete process-create "Use MAKE-PROCESS instead.")

(DEFMETHOD (PROCESS :INIT) (INIT-PLIST)
  (UNLESS (VARIABLE-BOUNDP STACK-GROUP)
    (SETQ STACK-GROUP (APPLY #'MAKE-STACK-GROUP NAME :ALLOW-OTHER-KEYS T :SAFE 0
                                                     (CAR INIT-PLIST))))
  (SETQ INITIAL-STACK-GROUP STACK-GROUP)
  (LET ((VARS (GET INIT-PLIST :CLOSURE-VARIABLES)))
    (SETQ CLOSURE                               ;does the right thing even if vars is ()
          (CLOSURE VARS #'FUNCALL))))

(DEFMETHOD (SIMPLE-PROCESS :INIT) (IGNORE)
  (SETQ INITIAL-FORM NIL
        STACK-GROUP NIL
        INITIAL-STACK-GROUP NIL))

(DEFMETHOD (PROCESS :AFTER :INIT) (IGNORE)
  (WITHOUT-INTERRUPTS
    (PROCESS-ALL-PROCESSES SELF T)))

(DEFMETHOD (PROCESS :PRINT-SELF) (STREAM &REST IGNORE)
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE)
    (PRINC NAME STREAM)))

(DEFUN PROCESS-PRESET (PROCESS FUNCTION &REST ARGS)
  "Preset PROCESS to apply FUNCTION to ARGS when next run."
  (LEXPR-SEND PROCESS :PRESET FUNCTION ARGS))
(compiler::make-obsolete process-preset "Use the :PRESET message")

(DEFMETHOD (PROCESS :PRESET) (FUNCTION &REST ARGS)
  (SETQ INITIAL-FORM (CONS FUNCTION (COPYLIST ARGS)))
  (SEND SELF :RESET))

;;; This is the real initial function of all processes' initial stack groups.
;;; Its purpose is to make sure that the error handler Abort command works.
;;; It also prevents anything bad from happening if the specified top-level returns
;;; and arranges for typing out to do "background" stuff.
(DEFUN PROCESS-TOP-LEVEL (&OPTIONAL IGNORE)
  (LET ((*TERMINAL-IO* TV::DEFAULT-BACKGROUND-STREAM))
    (%USING-BINDING-INSTANCES (CLOSURE-BINDINGS (PROCESS-CLOSURE CURRENT-PROCESS)))
    (DO-FOREVER
      (CATCH-ERROR-RESTART (CONDITION "Reset and arrest process ~A."
                                      (SEND CURRENT-PROCESS :NAME))
        (UNWIND-PROTECT
            (ERROR-RESTART ((SYS:ABORT CONDITION) "Restart process ~A."
                            (SEND CURRENT-PROCESS :NAME))
              (APPLY (CAR (PROCESS-INITIAL-FORM CURRENT-PROCESS))
                     (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS)))
              (PROCESS-FLUSH-BACKGROUND-STREAM)
              (PROCESS-WAIT-FOREVER))
          (PROCESS-FLUSH-BACKGROUND-STREAM)))
      (SEND CURRENT-PROCESS :ARREST-REASON ':USER)
      (PROCESS-ALLOW-SCHEDULE))))

(DEFUN PROCESS-KILL-TOP-LEVEL (&OPTIONAL ARG)
  "Get here after unwinding the stack due to a kill type :RESET.  Makes the
process unrunnable, and removes it from the ALL-PROCESSES list.  The process may be
enabled later.  If so, it will do the right thing by calling PROCESS-TOP-LEVEL."
  (WITHOUT-INTERRUPTS
    (PROCESS-DISABLE CURRENT-PROCESS)
    (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL)
    ;; This will never return unless the process is reenabled
    (PROCESS-ALLOW-SCHEDULE))
  ;; In case we are enabled again, act like we were just reset
  (PROCESS-TOP-LEVEL ARG))

(DEFUN PROCESS-IS-IN-ERROR-P (PROCESS &AUX SG)
  "Non-NIL if PROCESS is waiting for its window to be exposed, to handle an error.
The value is the window PROCESS is waiting for exposure of."
  (AND (TYPEP (SETQ SG (PROCESS-STACK-GROUP PROCESS)) 'STACK-GROUP)
       (SYMEVAL-IN-STACK-GROUP 'TV::PROCESS-IS-IN-ERROR SG)))

(DEFUN PROCESS-FLUSH-BACKGROUND-STREAM ()
  "If *TERMINAL-IO* is a background typeout window, release it for re-use."
  (WHEN (AND (NEQ *TERMINAL-IO* TV::DEFAULT-BACKGROUND-STREAM)
             (TYPEP *TERMINAL-IO* 'TV::BACKGROUND-LISP-INTERACTOR))
    (SEND-IF-HANDLES *TERMINAL-IO* :WAIT-UNTIL-SEEN)
    (SEND *TERMINAL-IO* :DEACTIVATE)
    (DEALLOCATE-RESOURCE 'TV::BACKGROUND-LISP-INTERACTORS *TERMINAL-IO*)
    (SETQ *TERMINAL-IO* TV::DEFAULT-BACKGROUND-STREAM)))

(DEFUN PROCESS-RESET (PROCESS)
  "Unwind PROCESS and make it start over."
  (SEND PROCESS :RESET))
(compiler::make-obsolete process-reset "Use the :RESET message")

(DEFMETHOD (PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL &AUX RESTART-FUN)
  "UNWIND-OPTION: T, never unwind; :UNLESS-CURRENT or NIL, unwinds the stack unless
the stack group is either in the current process or is the current stack group;
:ALWAYS, always unwinds the stack.  KILL is T to kill the process after optionally
unwinding it."
  (WITHOUT-INTERRUPTS
    (SETQ RESTART-FUN
          (COND (KILL #'PROCESS-KILL-TOP-LEVEL)
                ((EQ STACK-GROUP INITIAL-STACK-GROUP) #'PROCESS-TOP-LEVEL)
                (T #'(LAMBDA (&REST IGNORE)     ;Unwind and switch SG's
                       (EH::UNWIND-SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS)
                                      #'PROCESS-TOP-LEVEL NIL NIL)))))
    ;; Wake up
    (SETQ WAIT-WHOSTATE (IF KILL "Killed" "Reset") RUN-WHOSTATE "Run")
    (SET-PROCESS-WAIT SELF #'TRUE NIL)
    (COND ((EQ SELF CURRENT-PROCESS)
           (IF (EQ UNWIND-OPTION ':ALWAYS)
               (*UNWIND-STACK T NIL NIL RESTART-FUN)
             (WHEN KILL
               (PROCESS-DISABLE CURRENT-PROCESS)
               (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL))))
          (T
           ;; Note -- the following code is not logically necessary.  However,
           ;; it is here to make the cold-load come up when EH::UNWIND-SG
           ;; is not loaded yet.  We avoid unwinding the stack-group if it
           ;; has just been created.
           (AND (SG-NEVER-RUN-P STACK-GROUP)
                (SETQ UNWIND-OPTION T))
           ;; Cause the process, when next scheduled, to unwind itself and
           ;; call its initial function in the right stack group.
           (COND ((EQ %CURRENT-STACK-GROUP STACK-GROUP)
                  ;; Not current process, but our stack group is the one running.
                  ;; Respect NOUNWIND
                  (IF (EQ UNWIND-OPTION ':ALWAYS)
                      (*UNWIND-STACK T NIL NIL RESTART-FUN)
                    (WHEN KILL
                      (PROCESS-DISABLE CURRENT-PROCESS)
                      (PROCESS-ALL-PROCESSES CURRENT-PROCESS NIL))))
                 ((NEQ UNWIND-OPTION 'T)
                  (LET ((EH::ALLOW-PDL-GROW-MESSAGE NIL))
                    (EH::UNWIND-SG STACK-GROUP RESTART-FUN NIL T)))
                 (T
                  (STACK-GROUP-PRESET STACK-GROUP RESTART-FUN)))))))

(DEFMETHOD (SIMPLE-PROCESS :RESET) (&OPTIONAL UNWIND-OPTION KILL)
  (DECLARE (IGNORE UNWIND-OPTION))              ;ignored -- there is no stack group
  (WITHOUT-INTERRUPTS
    (SETQ STACK-GROUP (CAR INITIAL-FORM))       ;Reset to initial function
    (SETQ WAIT-WHOSTATE (IF KILL "Killed" "Reset") RUN-WHOSTATE "Run")
    (SET-PROCESS-WAIT SELF #'TRUE NIL)          ;and un-block
    (WHEN KILL
      (PROCESS-DISABLE SELF)                    ;Killing: remove from scheduler lists
      (PROCESS-ALL-PROCESSES SELF NIL))))

;;;; Process Interrupt Mechanism

(DEFMETHOD (SIMPLE-PROCESS :INTERRUPT) (FUNCTION &REST ARGS)
  (DECLARE (IGNORE FUNCTION ARGS))
  (FERROR "Cannot interrupt a simple process"))

;; don't use select-processor, as that is not loaded when this is evaluated
(DEFVAR PDL-BUFFER-LENGTH (COND ((= PROCESSOR-TYPE-CODE CADR-TYPE-CODE)
                                 #o2000)
                                ((= PROCESSOR-TYPE-CODE LAMBDA-TYPE-CODE)
                                 #o4000)
                                ((= processor-type-code explorer-type-code)
                                 #o2000))
  "Length of pdl buffer.  Depends on processor type.")

(DEFMETHOD (PROCESS :INTERRUPT) (FUNCTION &REST ARGS)
  (declare (dbg:error-reporter))                ;(eh current-process) signals from this frame.
  (IF (EQ SELF CURRENT-PROCESS)
      (PROGN (APPLY FUNCTION ARGS) T)           ;Note destination must be D-IGNORE
    (DO (STATE) (())                            ;Loop until in interruptible state
      (WITHOUT-INTERRUPTS
        (SETQ STATE (SG-CURRENT-STATE STACK-GROUP))
        (WHEN (= STATE SG-STATE-AWAITING-RETURN)        ;Called scheduler
          (LET ((RP (SG-REGULAR-PDL STACK-GROUP))
                (PP (SG-REGULAR-PDL-POINTER STACK-GROUP))
                (SP (SG-SPECIAL-PDL STACK-GROUP))
                (SPP (SG-SPECIAL-PDL-POINTER STACK-GROUP))
                (AP (SG-AP STACK-GROUP)))
            (OR (EQ (AREF RP AP) SCHEDULER-STACK-GROUP)
                (FERROR "Call to ~S where scheduler stack group expected"
                        (AREF RP AP)))
            ;; Remove frame of call to scheduler.  PP := M-AP minus 4
            (SETF (SG-PDL-PHASE STACK-GROUP) (LOGAND (- (SG-PDL-PHASE STACK-GROUP)
                                                        (- PP (SETQ PP (- AP 4))))
                                                     (1- PDL-BUFFER-LENGTH)))
            (SETF (SG-REGULAR-PDL-POINTER STACK-GROUP) PP)
            (SETF (SG-IPMARK STACK-GROUP) (EH::SG-NEXT-OPEN STACK-GROUP AP))
            (SETF (SG-AP STACK-GROUP) (SETQ AP (EH::SG-NEXT-ACTIVE STACK-GROUP AP)))
            (SETF (SG-FLAGS-QBBFL STACK-GROUP) ; Must correspond to current frame
                  (RP-BINDING-BLOCK-PUSHED RP AP))
            (SETQ STATE SG-STATE-RESUMABLE)
            (SET-PROCESS-WAIT SELF #'TRUE NIL)  ;Allow to wake up
            ;; If this function is PROCESS-WAIT, restart it at its start PC
            ;; so that when returned to, it will test the wait condition again.
            ;; Its stack level is sort of random, but that shouldn't hurt anything.
            ;; Also it has a binding of INHIBIT-SCHEDULING-FLAG which needs attention
            (WHEN (EQ (AREF RP AP) #'PROCESS-WAIT)
              (SETF (RP-EXIT-PC RP AP) (FEF-INITIAL-PC #'PROCESS-WAIT))
              (OR (EQ (AREF SP SPP)
                      (%P-CONTENTS-AS-LOCATIVE
                        ;(VALUE-CELL-LOCATION 'INHIBIT-SCHEDULING-FLAG)
                        (%MAKE-POINTER-OFFSET DTP-LOCATIVE      ;above doesn't work
                                              'INHIBIT-SCHEDULING-FLAG 1)))
                  (FERROR "Where's my binding of INHIBIT-SCHEDULING-FLAG ?"))
              (%P-STORE-CONTENTS                ;Leave bound to NIL, not T
                (LOCF (AREF SP (1- SPP)))       ;Without clobbering the flag bit
                NIL))))
        (WHEN (= STATE SG-STATE-RESUMABLE)      ;Safe state to interrupt
          (EH::SG-MAYBE-GROW-PDLS STACK-GROUP NIL #o200 #o100) ;Make space with no typeout
          (EH::SG-SAVE-STATE STACK-GROUP T)     ;Save M-T, microcode state
          (EH::SG-OPEN-CALL-BLOCK STACK-GROUP 0 FUNCTION)
          ;(SETF (SG-FLAGS-QBBFL STACK-GROUP) 0);SG-ENTER-CALL won't do it, but SG-SAVE-STATE does it
          (DOLIST (ARG ARGS)
            (EH::SG-REGPDL-PUSH ARG STACK-GROUP))
          (%P-STORE-CDR-CODE (LOCF (AREF (SG-REGULAR-PDL STACK-GROUP)   ;Terminate arg list
                                         (SG-REGULAR-PDL-POINTER STACK-GROUP)))
                             CDR-NIL)
          (SETF (SG-CURRENT-STATE STACK-GROUP) SG-STATE-INVOKE-CALL-ON-RETURN)
          (RETURN NIL)))                        ;Interrupt will go off when process next scheduled
      (PROCESS-WAIT "Interruptible"
                    (LAMBDA (P S)
                      ( (SG-CURRENT-STATE (PROCESS-STACK-GROUP P)) S))
                    SELF STATE))))

(DEFMETHOD (PROCESS :FLUSH) ()
  "Put a process into /"flushed/" state.  The process will remain flushed until it
is reset."
  (IF (EQ SELF CURRENT-PROCESS)
      NIL
    ;; Clobber all random pointers in SG registers, in case the structure is
    ;; reclaimed at some point.
    (IF (TYPEP STACK-GROUP 'STACK-GROUP) (WIPE-STRUCTURE STACK-GROUP))
    (SETQ WAIT-WHOSTATE "Flushed")
    (SET-PROCESS-WAIT SELF 'FLUSHED-PROCESS NIL)))

(DEFUN PROCESS-BLAST (&OPTIONAL (PROC CURRENT-PROCESS))
  "Blasting a process resets its wait function and argument list.  It is useful
when one of these generates an error."
  (SET-PROCESS-WAIT PROC 'FLUSHED-PROCESS NIL))

(DEFF FLUSHED-PROCESS #'FALSE)

(DEFUN PROCESS-DISABLE (PROCESS)
  "Stop PROCESS from running.  Removes all run reasons (and arrest reasons)."
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
    (SETF (PROCESS-ARREST-REASONS PROCESS) NIL)
    (PROCESS-CONSIDER-RUNNABILITY PROCESS)))

(DEFUN PROCESS-ENABLE (PROCESS)
  "Start PROCSS running.  Gives it :ENABLE as a run reason, and removes all arrest reasons."
  (WITHOUT-INTERRUPTS
    (process-all-processes process t)
 ;*temporary*  remove the following if AFTER system 121.
    (if (= (%p-data-type (locf (process-aentry-with-cons process)))
           dtp-null)
        (setf (process-aentry-with-cons process) nil))
    (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
    (SETF (PROCESS-ARREST-REASONS PROCESS) NIL)
    (SEND PROCESS :RUN-REASON :ENABLE)))

(DEFUN PROCESS-RESET-AND-ENABLE (PROCESS)
  "Unwind PROCESS, restart it, and start it running."
  (WITHOUT-INTERRUPTS
   (SEND PROCESS :RESET)
   (PROCESS-ENABLE PROCESS)))

(DEFMETHOD (PROCESS :ACTIVE-P) ()
  (ASSQ SELF ACTIVE-PROCESSES))

(DEFMETHOD (PROCESS :RUNNABLE-P) ()
  (ASSQ SELF ACTIVE-PROCESSES))

(DEFUN PROCESS-CONSIDER-RUNNABILITY (&OPTIONAL (PROCESS SELF))
  "Add PROCESS to ACTIVE-PROCESSES if it should be there; remove it if not."
  (WITHOUT-INTERRUPTS
    (COND ((OR (PROCESS-ARREST-REASONS PROCESS) (NULL (PROCESS-RUN-REASONS PROCESS)))
           ;; Process is arrested, better not be active
           (let ((cons (process-aentry-with-cons process)))
             (when cons
                ;This might be "unsafe", so copy it.  See SET-PROCESS-WAIT.
               (setf (process-wait-argument-list process)
                     (copylist (process-wait-argument-list process)))
               (do ((p (variable-location active-processes) (cdr p)))
                   ((null p))
                 (cond ((eq (cdr p) cons)
                        (return (rplacd p (cddr p))))))
               (rplacd cons idle-aentries-for-active-processes)
               (setq idle-aentries-for-active-processes cons)
               (setf (process-aentry-with-cons process) nil)))
      ;    (LET ((APE (ASSQ PROCESS ACTIVE-PROCESSES)))
      ;      (WHEN APE
      ;        (SETF (CAR APE) NIL)
      ;        (PROCESS-REINSERT-AENTRY APE)))
             (TV::WHO-LINE-RUN-STATE-UPDATE))
      ;   ((ASSQ PROCESS ACTIVE-PROCESSES))
          ((process-aentry-with-cons process))
          (T
  ;        (PROCESS-ALL-PROCESSES PROCESS T)     ;assure on ALL-PROCESSES.  ** flush this**
           (let* ((cons (cond (idle-aentries-for-active-processes
                               (prog1 idle-aentries-for-active-processes
                                      (setq idle-aentries-for-active-processes
                                            (cdr idle-aentries-for-active-processes))))
                              (t (make-aentry-with-cons))))
                  (aentry (car cons)))
             (RPLACD CONS ACTIVE-PROCESSES)
             (SETQ ACTIVE-PROCESSES CONS)
             (SETF (PROCESS-AENTRY-WITH-CONS PROCESS) CONS)
             (SETF (FIRST AENTRY) PROCESS)
             (SETF (FOURTH AENTRY) (PROCESS-PRIORITY PROCESS))
             (PROCESS-REINSERT-AENTRY AENTRY)
             (SET-PROCESS-WAIT PROCESS
                               (PROCESS-WAIT-FUNCTION PROCESS)
                               (PROCESS-WAIT-ARGUMENT-LIST PROCESS)))

           ;; If process's stack group is in a bad state,
           ;; make it wait instead of actually running (unless it's current!).
           ;; ACTIVE is a bad state for a process which isn't running!
           (AND (NOT (PROCESS-SIMPLE-P PROCESS))
                (NOT (SG-RESUMABLE-P (PROCESS-STACK-GROUP PROCESS)))
                CURRENT-PROCESS                 ;Prevents lossage in PROCESS-INITIALIZE
                (SEND PROCESS :FLUSH))
           (TV::WHO-LINE-RUN-STATE-UPDATE)))))

;(DEFUN PROCESS-CONSIDER-RUNNABILITY (&OPTIONAL (PROCESS SELF))
;  "Add PROCESS to ACTIVE-PROCESSES if it should be there; remove it if not."
;  (WITHOUT-INTERRUPTS
;    (COND ((OR (PROCESS-ARREST-REASONS PROCESS) (NULL (PROCESS-RUN-REASONS PROCESS)))
;          ;; Process is arrested, better not be active
;          (LET ((APE (ASSQ PROCESS ACTIVE-PROCESSES)))
;            (WHEN APE
;              (SETF (CAR APE) NIL)
;              (PROCESS-REINSERT-AENTRY APE))
;            (TV::WHO-LINE-RUN-STATE-UPDATE)))
;         ((ASSQ PROCESS ACTIVE-PROCESSES))
;         (T
;          (PROCESS-ACTIVE-ENTRY PROCESS)
;          ;; If process's stack group is in a bad state,
;          ;; make it wait instead of actually running (unless it's current!).
;          ;; ACTIVE is a bad state for a process which isn't running!
;          (AND (NOT (PROCESS-SIMPLE-P PROCESS))
;               (NOT (SG-RESUMABLE-P (PROCESS-STACK-GROUP PROCESS)))
;               CURRENT-PROCESS                 ;Prevents lossage in PROCESS-INITIALIZE
;               (SEND PROCESS :FLUSH))
;          (TV::WHO-LINE-RUN-STATE-UPDATE)))))

(DEFMETHOD (PROCESS :RUN-REASON) (&OPTIONAL (REASON :USER))
  (WITHOUT-INTERRUPTS
    (UNLESS (MEMQ REASON RUN-REASONS)
      (PUSH REASON RUN-REASONS)
      (PROCESS-CONSIDER-RUNNABILITY))))

(DEFMETHOD (PROCESS :REVOKE-RUN-REASON) (&OPTIONAL (REASON :USER))
  (WITHOUT-INTERRUPTS
    (SETQ RUN-REASONS (DELQ REASON RUN-REASONS))
    (PROCESS-CONSIDER-RUNNABILITY)))

(DEFMETHOD (PROCESS :ARREST-REASON) (&OPTIONAL (REASON :USER))
  (WITHOUT-INTERRUPTS
    (UNLESS (MEMQ REASON ARREST-REASONS)
      (PUSH REASON ARREST-REASONS)
      (PROCESS-CONSIDER-RUNNABILITY))))

(DEFMETHOD (PROCESS :REVOKE-ARREST-REASON) (&OPTIONAL (REASON :USER))
  (WITHOUT-INTERRUPTS
    (SETQ ARREST-REASONS (DELQ REASON ARREST-REASONS))
    (PROCESS-CONSIDER-RUNNABILITY)))

(DEFMETHOD (PROCESS :KILL) ()
  (SEND SELF :RESET :ALWAYS T))

;;; Priority and quantum stuff
(DEFMETHOD (PROCESS :SET-QUANTUM) (NEW-QUANTUM)
  (CHECK-TYPE NEW-QUANTUM NON-COMPLEX-NUMBER)
  (SETQ QUANTUM NEW-QUANTUM))

(DEFMETHOD (PROCESS :SET-PRIORITY) (NEW-PRIORITY)
  (CHECK-TYPE NEW-PRIORITY NON-COMPLEX-NUMBER)
  (WITHOUT-INTERRUPTS
    (SETQ PRIORITY NEW-PRIORITY)
    (AND (ASSQ SELF ACTIVE-PROCESSES)
         (process-aentry-with-cons self)                ;was (PROCESS-ACTIVE-ENTRY self).
         (process-reinsert-aentry (car (process-aentry-with-cons self))))))

(defun process-reinsert-aentry (aentry)
  "Make sure AENTRY, which must currently be on ACTIVE-PROCESSES, is in
the right place with regards to priority."
  (prog (p cons)
        (setq p (value-cell-location 'active-processes))
  l1    (cond ((null p)
               (ferror "Should not get here"))
              ((eq (cadr p) aentry)
               (setq cons (cdr p))
               (rplacd p (cddr p))
               (go reinsert)))
        (setq p (cdr p))
        (go l1)
 reinsert
  ; in new scheme, should not be on at all it PROC is NULL.
  ;     (cond ((null (first aentry))
  ;            (rplacd cons idle-aentry-for-active-processes)
  ;            (setq idle-aentry-for-active-processes cons)
  ;            (return nil)))
        (setq p (value-cell-location 'active-processes))
  l2    (cond ((null p)
               (ferror "Should not get here"))
              ((or (null (cdr p))
                   (< (fourth (cadr p)) (fourth aentry)))
               (rplacd cons (cdr p))
               (rplacd p cons)
               (return cons)))
        (setq p (cdr p))
        (go l2)))

;; Put AENTRY into its proper position in ACTIVE-PROCESSES,
;; assuming that that is a cdr-coded list,
;; and that nothing else is out of order.
;(DEFUN PROCESS-REINSERT-AENTRY (AENTRY)
;  (LET ((OLD-POS (FIND-POSITION-IN-LIST AENTRY ACTIVE-PROCESSES))
;       (NEW-POS
;         (DO ((I 0 (1+ I))
;              (TAIL ACTIVE-PROCESSES (CDR TAIL)))
;             ((OR (NULL TAIL)
;                  (AND (NEQ (CAR TAIL) AENTRY)
;                       (OR (NULL (CAAR TAIL))
;                           (AND (CAR AENTRY)
;                                (< (FOURTH (CAR TAIL)) (FOURTH AENTRY))))))
;              I))))
;    ;; NEW-POS is the position to insert before.
;    (COND ((= NEW-POS OLD-POS)
;          (FERROR "Should not get here."))
;         ((= NEW-POS (1+ OLD-POS))
;          ;; In right place already.
;          )
;         ((> NEW-POS OLD-POS)
;          (%BLT-TYPED (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES (1+ OLD-POS))
;                      (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES OLD-POS)
;                      (- NEW-POS OLD-POS 1)
;                      1)
;          (%P-DPB-OFFSET CDR-NEXT %%Q-CDR-CODE ACTIVE-PROCESSES (- NEW-POS 2))
;          (SETF (CAR (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES (1- NEW-POS)))
;                AENTRY))
;         (T
;          (LET ((CDRCODE (%P-LDB-OFFSET %%Q-CDR-CODE ACTIVE-PROCESSES OLD-POS)))
;            (%BLT-TYPED (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES (1- OLD-POS))
;                        (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES OLD-POS)
;                        (- OLD-POS NEW-POS)
;                        -1)
;            (%P-DPB-OFFSET CDRCODE %%Q-CDR-CODE ACTIVE-PROCESSES OLD-POS))
;          (SETF (CAR (%MAKE-POINTER-OFFSET DTP-LIST ACTIVE-PROCESSES NEW-POS))
;                AENTRY)))))

;not called by system.
(DEFUN PROCESS-ORDER-ACTIVE-PROCESSES ()
  "Imposes an ordering on active processes for the priority mechanism.  Order is
from highest to lowest priority.  Priorities are simply compared numerically.  This
function MUST be called with interrupts inhibited."
  (AND (FBOUNDP 'SORT-SHORT-LIST)               ;Cold-load!
       (SETQ ACTIVE-PROCESSES (SORT-SHORT-LIST ACTIVE-PROCESSES
                                               #'(LAMBDA (P1 P2)
                                                   (COND ((NULL (FIRST P1)) (NULL (FIRST P2)))
                                                         ((NULL (FIRST P2)) T)
                                                         (T (> (FOURTH P1) (FOURTH P2)))))
                                               NIL))))

;;; This is for the error handler
(DEFMETHOD (PROCESS :COROUTINE-STACK-GROUPS) ()
  NIL)

(DEFMETHOD (COROUTINING-PROCESS :ADD-COROUTINE-STACK-GROUP) (-STACK-GROUP-)
  (WITHOUT-INTERRUPTS
    (PUSHNEW -STACK-GROUP- COROUTINE-STACK-GROUPS :TEST #'EQ))
  -STACK-GROUP-)

;;;; Metering stuff

(DEFMETHOD (PROCESS :RESET-METERS) ()
  (RESET-PROCESS-TIME-METER TOTAL-RUN-TIME)
  (RESET-PROCESS-TIME-METER DISK-WAIT-TIME)
  (SETQ LAST-TIME-RUN NIL
        PAGE-FAULT-COUNT 0
        PERCENT-UTILIZATION 0))

;;; Idle time in seconds, or NIL if forever
(DEFMETHOD (PROCESS :IDLE-TIME) ()
  (COND ((EQ SELF CURRENT-PROCESS) 0)
        ((NULL LAST-TIME-RUN) NIL)
        (T (TRUNCATE (TIME-DIFFERENCE (TIME) LAST-TIME-RUN) 60.))))

(DEFMETHOD (PROCESS :TOTAL-RUN-TIME) ()
  (FIXNUM-PROCESS-TIME-METER TOTAL-RUN-TIME))

(DEFMETHOD (PROCESS :DISK-WAIT-TIME) ()
  (FIXNUM-PROCESS-TIME-METER DISK-WAIT-TIME))

(DEFMETHOD (PROCESS :CPU-TIME) ()
  (- (FIXNUM-PROCESS-TIME-METER TOTAL-RUN-TIME)
     (FIXNUM-PROCESS-TIME-METER DISK-WAIT-TIME)))

;;; This is the 600th root of 1/2, thus the halflife is 10 seconds
(DEFPARAMETER *PERCENT-UTILIZATION-DISCOUNT-FACTOR* 0.99885s0)

(DEFMETHOD (PROCESS :PERCENT-UTILIZATION) ()
  (WITHOUT-FLOATING-UNDERFLOW-TRAPS
    (IF (NULL LAST-TIME-RUN) 0
        (// (* PERCENT-UTILIZATION (^ *PERCENT-UTILIZATION-DISCOUNT-FACTOR*
                                      (TIME-DIFFERENCE (TIME) LAST-TIME-RUN)))
            200.))))                    ;100% shows up as 20 seconds (20000. milliseconds)

;;;; Miscellaneous process synchronization functions

(DEFUN PROCESS-ALLOW-SCHEDULE ()
  "Allow other processes to run, if they can, before continuing."
  (SETF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) -1)
  (FUNCALL SCHEDULER-STACK-GROUP)
  (TV::WHO-LINE-RUN-STATE-UPDATE))

(DEFUN PROCESS-SLEEP (INTERVAL-IN-60THS &OPTIONAL (WHOSTATE "Sleep"))
  "Wait for INTERVAL 60'ths of a second."
  (PROCESS-WAIT WHOSTATE
                #'(LAMBDA (START-TIME INTERVAL)
                    ( (TIME-DIFFERENCE (TIME) START-TIME)
                       INTERVAL))
                (TIME) INTERVAL-IN-60THS))


(DEFUN SLEEP (INTERVAL-IN-SECONDS &OPTIONAL (WHOSTATE "Sleep"))
  "Wait for about INTERVAL seconds.  INTERVAL need not be an integer."
  (PROCESS-WAIT WHOSTATE
                #'(LAMBDA (START-TIME INTERVAL)
                    ( (TIME-DIFFERENCE (TIME) START-TIME)
                       INTERVAL))
                (TIME) (ROUND (* 60. INTERVAL-IN-SECONDS))))

;;; process-wait is in SYS; LTOP

;;; Returns T if condition is true, NIL if you time out.
(DEFUN PROCESS-WAIT-WITH-TIMEOUT (WHOSTATE INTERVAL-IN-60THS FUNCTION &REST ARGS)
  "Wait until INTERVAL 60'ths of a second elapse, or (APPLY FUNCTION ARGS) is non-NIL.
WHOSTATE appears in the who line and in Peek while waiting.
The value is whatever FUNCTION returned, which will be NIL
if wake-up is due to using up INTERVAL.
If INTERVAL is NIL, we wait forever (same effect as (PROCESS-WAIT WHOSTATE FUNCTION ARGS))"
     ;The lack of this piece of code caused bizarre lossage in the active processes list.
  (setq args (copylist args))
  (IF (NULL INTERVAL-IN-60THS)
      (APPLY #'PROCESS-WAIT WHOSTATE FUNCTION ARGS)
    (PROCESS-WAIT WHOSTATE
                  #'(LAMBDA (START-TIME INTERVAL FUNCTION ARGS)
                      (OR (APPLY FUNCTION ARGS)
                          ( (TIME-DIFFERENCE (TIME) START-TIME) INTERVAL)))
                  (TIME) INTERVAL-IN-60THS
                  FUNCTION ARGS)
    (APPLY FUNCTION ARGS)))

(DEFUN PROCESS-WAIT-FOREVER ()
  "Wait forever.  Does not return.  However, the process may be restarted."
  (PROCESS-WAIT "Wait forever" #'FALSE))

(DEFVAR *TIMEOUT-INSTANCE* :UNBOUND
  "Condition instance used for signalling timeout conditions within WITH-TIMEOUT")
;;; used by with-timeout (in SYS2; LMMAC)
(DEFUN WITH-TIMEOUT-INTERNAL (DURATION PROCESS)
  (UNLESS (VARIABLE-BOUNDP *TIMEOUT-INSTANCE*)
    ;; This can't be done when the file is loaded.
    (SETQ *TIMEOUT-INSTANCE* (MAKE-CONDITION 'CONDITION :CONDITION-NAMES '(TIMEOUT))))
  (PROCESS-SLEEP DURATION)
  (SEND PROCESS ':INTERRUPT #'SIGNAL-CONDITION *TIMEOUT-INSTANCE*)
  (process-wait "To Die" #'false))

;; A lock may be any cell.  When a lock is in the unlocked state, the cell
;; contains NIL; otherwise the cell contains the process which locked the lock.
;; A lock is referred to by a locative pointer to the cell.


(DEFUN PROCESS-LOCK (LOCATIVE-POINTER &OPTIONAL LOCK-VALUE (WHOSTATE "Lock")
                     TIMEOUT)
  "Usage of WITH-LOCK is preferable in most cases.
Lock the cell which LOCATIVE-POINTER points to, waiting if it is already locked.
The lock cell contains NIL when not locked;
when locked, it contains the process that locked it.
Does not hack UNWIND-PROTECT, so caller should provide for that.
If TIMEOUT is non-NIL, it is in 60'ths of a second,
and if that much time elapses we signal the SYS:LOCK-TIMEOUT error condition."
  (OR LOCK-VALUE (SETQ LOCK-VALUE CURRENT-PROCESS))
  (DO ((LOCKER (CAR LOCATIVE-POINTER) (CAR LOCATIVE-POINTER)))
      ((%STORE-CONDITIONAL LOCATIVE-POINTER NIL LOCK-VALUE))
    (AND (EQ LOCKER LOCK-VALUE)
         (FERROR "Lock ~S already locked by this process" LOCATIVE-POINTER))
    (IF TIMEOUT
        (UNLESS
          (PROCESS-WAIT-WITH-TIMEOUT WHOSTATE TIMEOUT
                                     #'(LAMBDA (BAD-CONTENTS POINTER)
                                         (NEQ (CONTENTS POINTER) BAD-CONTENTS))
                                     LOCKER
                                     LOCATIVE-POINTER)
          (CERROR :NO-ACTION NIL 'SYS:LOCK-TIMEOUT
                  "The ~A ~S remained unavailable for ~D//60 seconds."
                  WHOSTATE LOCATIVE-POINTER TIMEOUT))
      (PROCESS-WAIT WHOSTATE
                    #'(LAMBDA (BAD-CONTENTS POINTER)
                        (NEQ (CONTENTS POINTER) BAD-CONTENTS))
                    LOCKER
                    LOCATIVE-POINTER))
    (SETQ TIMEOUT NIL)))

;; Unlock the given lock.  The unlocker must be the same as the locker.
(DEFUN PROCESS-UNLOCK (LOCATIVE-POINTER &OPTIONAL LOCK-VALUE (ERROR-P T))
  "Unlock a lock locked with PROCESS-LOCK.
LOCATIVE-POINTER points to the cell which is the lock."
  (OR LOCK-VALUE (SETQ LOCK-VALUE CURRENT-PROCESS))
  (OR (%STORE-CONDITIONAL LOCATIVE-POINTER LOCK-VALUE NIL)
      (AND ERROR-P
           (FERROR "Attempt to unlock ~S, which you don't have locked"
                   LOCATIVE-POINTER))))


;;;; Process queues

(DEFUN MAKE-PROCESS-QUEUE (NAME SIZE)
  "Makes a process queue whose name is NAME and which can hold SIZE elements.
SIZE matters only in that if more than that many objects are put on the queue
then strict queueing behavior is not guaranteed for objects past the SIZE'th one."
  (MAKE-PROCESS-QUEUE-INTERNAL :NAME NAME
                               :MAKE-ARRAY (:DIMENSIONS (1+ SIZE))))

(DEFUN PROCESS-QUEUE-LOCKER (QUEUE)
  "The process (or other object) which now /"possesses/" QUEUE, a PROCESS-QUEUE."
  (CHECK-TYPE QUEUE PROCESS-QUEUE)
  (AREF QUEUE 0))

(DEFUN RESET-PROCESS-QUEUE (QUEUE)
  "Removes all processes enqueued on QUEUE, so that it is empty."
  (CHECK-TYPE QUEUE PROCESS-QUEUE)
  (WITHOUT-INTERRUPTS
    (ARRAY-INITIALIZE QUEUE NIL))
  QUEUE)

(DEFUN PROCESS-ENQUEUE (QUEUE &OPTIONAL (LOCK-VALUE CURRENT-PROCESS) (WHOSTATE "Lock"))
  "Waits to possess QUEUE in the name of LOCK-VALUE (default is the current process).
Puts LOCK-VALUE at the end of the queue, then waits for it to
reach the front of the queue (to /"possess/" the queue).
Then returns with LOCK-VALUE still in possession of the queue.
WHOSTATE appears in the who line if it is necessary to wait."
  (CHECK-TYPE QUEUE PROCESS-QUEUE)
  (UNLESS (%STORE-CONDITIONAL (LOCF (AREF QUEUE 0)) NIL (OR LOCK-VALUE CURRENT-PROCESS))
    (WITHOUT-INTERRUPTS
      ;; If the queue is full, wait for there to be room.
      (WHEN (AREF QUEUE (- (LENGTH QUEUE) 2))
        (PROCESS-WAIT WHOSTATE
                      (LAMBDA (LOC) (NULL (CONTENTS LOC)))
                      (LOCF (AREF QUEUE (1- (LENGTH QUEUE))))))
      ;; There is room, so put us in the queue.
      (DOTIMES (I (1- (LENGTH QUEUE)))
        (LET ((TEM (LOCF (AREF QUEUE I))))
          (COND ((%STORE-CONDITIONAL TEM NIL (OR LOCK-VALUE CURRENT-PROCESS))
                 ;; Now wait until we reach the front before returning.
                 (UNLESS (ZEROP I)
                   (PROCESS-WAIT WHOSTATE
                                 #'(LAMBDA (SLOT VALUE)
                                     (EQ (CONTENTS SLOT) VALUE))
                                 (LOCF (AREF QUEUE 0))
                                 (OR LOCK-VALUE CURRENT-PROCESS)))
                 (RETURN NIL))
                ((EQ (CONTENTS TEM) (OR LOCK-VALUE CURRENT-PROCESS))
                 (FERROR "~S is already enqueued on ~S."
                         (OR LOCK-VALUE CURRENT-PROCESS) QUEUE))))))))

(DEFUN PROCESS-DEQUEUE (QUEUE &OPTIONAL (LOCK-VALUE CURRENT-PROCESS) (ERRORP T))
  "Assuming that LOCK-VALUE possesses QUEUE, releases possession.
The next thing on the queue will come to the front, or the queue may become empty.
An error occurs if ERRORP is non-NIL and LOCK-VALUE is not currently
the object at the front of the queue.
LOCK-VALUE defaults to the current process."
  (CHECK-TYPE QUEUE PROCESS-QUEUE)
  (COND ((EQ (OR LOCK-VALUE CURRENT-PROCESS) (AREF QUEUE 0))
         (%BLT-TYPED (LOCF (AREF QUEUE 1))
                     (LOCF (AREF QUEUE 0))
                     (1- (LENGTH QUEUE)) 1))
        (ERRORP
         (FERROR "~S is not currently locked by ~S."
                 QUEUE (OR LOCK-VALUE CURRENT-PROCESS)))
        (T NIL)))

;;;; The scheduler

;;; The processes on ACTIVE-PROCESSES are sorted according to priority.
;;; A process is runnable if its flush instruction returns non-NIL.

;;; This function runs in the scheduler stack group.  Its job is to decide which
;;; process is to be run next.  It does this in the following way:

;;; If the current process is runnable, it has not exceeded its quantum, and
;;; no higher priority task is runnable, then it is run.  If not, the queue
;;; is searched from left to right for the highest
;;; priority process that is runnable and has not been run in a while.  This
;;; process is then run for its quantum.

;;; The scheduler also knows about a clock queue.  Every time the clock ticks,
;;; the queue is inspected for entries which should be run.  If any are found,
;;; they are run and the entry is deactivated.

(DEFVAR *STATUS-LINE-UPDATE-INTERVAL* 30.
  "The interval between status-line updates, in 60ths of a second.")

(DEFUN FIND-RUNNABLE-PROCESS ()
  (DO ((PROCS ACTIVE-PROCESSES)
       (THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED)
       (FIRST-OF-THIS-PRIORITY)
       (CURRENT-PRIORITY)
       (APE)
       (PROC)
       (PRI))
      ((NULL (FIRST (CAR PROCS))))
    ;; Loop over all process of the current priority
    (SETQ CURRENT-PRIORITY (FOURTH (CAR PROCS)))
    (SETQ FIRST-OF-THIS-PRIORITY PROCS)
    ;; If we find a process to run return from FOUND-PROCESS.
    ;; If we have looked at all processes of this priority, return from RAN-OUT.
    ;; This hair is equivalent to one loop with a catch around just the APPLY,
    ;; but it avoids entering and exiting the catch so often.
    (BLOCK RAN-OUT
      (DO-FOREVER
        (CATCH 'PROCESS-WAIT-IN-SCHEDULER
          (DO-FOREVER
            (SETQ APE (CAR PROCS))
            (AND (OR (NULL (SETQ PROC (FIRST APE)))
                     (NOT (= (SETQ PRI (FOURTH APE)) CURRENT-PRIORITY)))
                 ;; Hit next priority level, or ran out of processes
                 (RETURN-FROM RAN-OUT NIL))
            ;;temporarily bind current-process so that if we get to the
            ;;debugger, and the user types ABORT, then the right process
            ;;will get "blasted"
            (let ((current-process (first ape)))
              (AND (COND ((APPLY (SECOND APE) (THIRD APE))
                          (SETQ THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED PROC)
                          T))
                   (PLUSP (PROCESS-QUANTUM-REMAINING PROC))
                   ;; It is runnable, and it has time remaining
                   (RETURN-FROM FIND-RUNNABLE-PROCESS PROC))
              (POP PROCS))))
        ;; Get here only on throw.
        (POP PROCS)))
    ;; Ran out of all processes at current priority level.  Reset their quantums.
    (DO ((PS FIRST-OF-THIS-PRIORITY (CDR PS)))
        ((EQ PS PROCS))
      (SETF (PROCESS-QUANTUM-REMAINING (FIRST (CAR PS)))
            (PROCESS-QUANTUM (FIRST (CAR PS)))))
    ;; If a process would have run at this priority level, but couldn't becase
    (AND THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED
         (RETURN-FROM FIND-RUNNABLE-PROCESS
           THIS-PROCESS-WANTS-TO-RUN-BUTS-ITS-QUANTUM-HAS-EXPIRED))))

(DEFUN PROCESS-SCHEDULER-IDLE-FUNCTION ()
  "This function is called by the scheduler every time around, when there are
no runnable processes."
  (and (fboundp 'gc::idle-scavenge)
       (gc::idle-scavenge gc-idle-scavenge-quantum)))

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
        (WHEN (NOT (NULL CURRENT-PROCESS))
          (DECF (PROCESS-QUANTUM-REMAINING CURRENT-PROCESS) DELTA-TIME))
        ;;don't blast previous process from the debugger just because
        ;;a clock function gets an error
        (setq current-process nil)
        (CATCH 'PROCESS-WAIT-IN-SCHEDULER  ;minor bum to reduce overhead.  None of these frobs
                ;will normally PROCESS-WAIT, but in exception circumstances (ie GC), they can.
          (DOLIST (F CLOCK-FUNCTION-LIST)
            (FUNCALL F DELTA-TIME))
          (WHEN (MINUSP (DECF NEXT-WHO-TIME DELTA-TIME))
            (WHEN (FBOUNDP 'TV::WHO-LINE-UPDATE) (TV:WHO-LINE-UPDATE))
            (SETQ NEXT-WHO-TIME *STATUS-LINE-UPDATE-INTERVAL*))))
      (IF (NULL (SETQ PROCESS (SETQ CURRENT-PROCESS (FIND-RUNNABLE-PROCESS))))
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


;;;; PROCESS-RUN-FUNCTION and associated hair

;;; This is a list of processes which may be recycled by PROCESS-RUN-FUNCTION
;;; It exists to avoid excess consing of stacks and reclaiming of them via
;;; the ordinary garbage collector.
(DEFVAR PROCESS-RUN-FUNCTION-SPARE-PROCESSES NIL)

;;; Run a function in its own process
(DEFUN PROCESS-RUN-FUNCTION (NAME-OR-KWDS FUNCTION &REST ARGS)
  "Apply FUNCTION to ARGS in a separate process.
NAME-OR-KWDS is either a name for the process or a list of
alternating keywords and values.  The keywords allowed are:
:NAME - specifies the name for the process.
:RESTART-AFTER-RESET - T means restart the process if it is reset
 (instead of killing it, which is the default).
:RESTART-AFTER-BOOT - T means restart the process after warm booting.
:PRIORITY, :QUANTUM, :WARM-BOOT-ACTION - set those variables in the process."
  (PROCESS-RUN-FUNCTION-1 NAME-OR-KWDS FUNCTION ARGS NIL))

(DEFUN PROCESS-RUN-RESTARTABLE-FUNCTION (NAME FUNCTION &REST ARGS)
  "Like PROCESS-RUN-FUNCTION but default is to restart process after booting or reset."
  (PROCESS-RUN-FUNCTION-1 NAME FUNCTION ARGS '(:RESTART-AFTER-BOOT T :RESTART-AFTER-RESET T)))

(DEFUN PROCESS-RUN-FUNCTION-1 (NAME-OR-KEYS FUNCTION ARGS LOCAL-KEYS)
  (LET ((NAME (IF (STRINGP NAME-OR-KEYS) NAME-OR-KEYS NIL))
        (PRIORITY 0)
        (QUANTUM 60.)
        RESTART-AFTER-RESET RESTART-AFTER-BOOT PROCESS WARM-BOOT-ACTION)
    (KEYWORD-EXTRACT (IF (STRINGP NAME-OR-KEYS) LOCAL-KEYS (APPEND LOCAL-KEYS NAME-OR-KEYS))
                     KEYWORDS
                     (NAME PRIORITY QUANTUM RESTART-AFTER-RESET RESTART-AFTER-BOOT
                           WARM-BOOT-ACTION)
      NIL NIL)
    (SETQ PROCESS (WITHOUT-INTERRUPTS (OR (POP PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
                                          (MAKE-PROCESS NAME :SPECIAL-PDL-SIZE #o4000
                                                             :REGULAR-PDL-SIZE #o15000))))
    (SETF (PROCESS-NAME PROCESS) (OR NAME (SETQ NAME "Anonymous")))
    (SETF (PROCESS-WARM-BOOT-ACTION PROCESS) (IF (EQ WARM-BOOT-ACTION :FLUSH)
                                                 NIL
                                               (OR WARM-BOOT-ACTION
                                                   (AND RESTART-AFTER-BOOT
                                                        'PROCESS-WARM-BOOT-DELAYED-RESTART)
                                                   'PROCESS-RUN-FUNCTION-WARM-BOOT-RESET)))
    (SETF (SG-NAME (PROCESS-INITIAL-STACK-GROUP PROCESS)) NAME)
    (SEND PROCESS :SET-QUANTUM QUANTUM)
    (SEND PROCESS :SET-PRIORITY PRIORITY)
    (SEND PROCESS :RESET-METERS)
    (LEXPR-SEND PROCESS :PRESET
                        'PROCESS-RUN-FUNCTION-INTERNAL RESTART-AFTER-RESET FUNCTION ARGS)
    (PROCESS-ENABLE PROCESS)
    PROCESS))

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL (RESTART-ON-RESET FUNCTION &REST ARGS)
  (OR RESTART-ON-RESET
      (SEND CURRENT-PROCESS :PRESET 'PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))
  (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Terminate and free process ~A."
                        (SEND CURRENT-PROCESS :NAME))
    (APPLY FUNCTION ARGS))
  ;; When the function returns, disable this process and make it available
  ;; for re-use.
  (PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS))

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL-FLUSH-PROCESS ()
  (PROCESS-FLUSH-BACKGROUND-STREAM)
  (WITHOUT-INTERRUPTS
    (PUSHNEW CURRENT-PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES :TEST #'EQ)
    (SEND CURRENT-PROCESS :KILL)))

(DEFUN PROCESS-RUN-FUNCTION-WARM-BOOT-RESET (PROCESS)
  (PROCESS-WARM-BOOT-RESET PROCESS)
  (PUSHNEW PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES :TEST #'EQ))

(DEFUN PROCESS-WARM-BOOT-RESET (PROCESS)
  (WITHOUT-INTERRUPTS
    (SEND PROCESS :PRESET #'(LAMBDA ()
                              (SEND CURRENT-PROCESS :KILL)
                              (PROCESS-WAIT-FOREVER)))
    (SEND PROCESS :RESET)
    (PROCESS-ENABLE PROCESS)))

(DEFUN PROCESS-WARM-BOOT-RESTART (PROCESS)
  (SEND PROCESS :RESET))

;;; Like PROCESS-WARM-BOOT-RESTART but doesn't allow it to run until after
;;; initialization is complete.
(DEFUN PROCESS-WARM-BOOT-DELAYED-RESTART (PROCESS)
  (PUSH (CONS PROCESS (PROCESS-RUN-REASONS PROCESS)) DELAYED-RESTART-PROCESSES)
  (SETF (PROCESS-RUN-REASONS PROCESS) NIL)
  (PROCESS-CONSIDER-RUNNABILITY PROCESS)
  (SEND PROCESS :RESET))                        ;Won't actually unwind until given run reason

(DEFUN SB-ON (&OPTIONAL (WHEN 'JUST-SHOW-CURRENT-STATE)
              &AUX MASK TEM
              (ALIST '((:CALL . 1) (:UNIBUS . 2) (:KEYBOARD . 2);old name still supported.
                       (:CHAOS . 4) (:CLOCK . #o10) )))
  "Sets the sequence break enable flags:
The argument can be a keyword, a list of keywords, or a numeric mask.
Keywords are: :CALL, :UNIBUS, :CHAOS, :CLOCK
With no argument, just returns a list of keywords for what is enabled.
Argument of NIL means turn off sequence breaks."
  (COND ((NUMBERP WHEN) (SETQ MASK WHEN))
        ((NULL WHEN) (SETQ MASK 0))
        ((EQ WHEN 'JUST-SHOW-CURRENT-STATE) (SETQ MASK %SEQUENCE-BREAK-SOURCE-ENABLE))
        ((ATOM WHEN)
         (OR (SETQ MASK (CDR (ASSQ WHEN ALIST)))
             (FERROR "~S invalid keyword.  Use ~S, ~S, ~S, or ~S"
                     WHEN :CALL :UNIBUS :CHAOS :CLOCK)))
        (T (SETQ MASK 0)
           (DOLIST (KWD WHEN)
             (IF (SETQ TEM (CDR (ASSQ KWD ALIST)))
                 (SETQ MASK (LOGIOR MASK TEM))
                 (FERROR "~S invalid keyword.  Use ~S, ~S, ~S, or~S"
                             KWD :CALL :UNIBUS :CHAOS :CLOCK)))))
  (SETQ %SEQUENCE-BREAK-SOURCE-ENABLE MASK)
  (DO ((L NIL)
       (B 1 (LSH B 1)))
      ((ZEROP MASK) L)
    (AND (BIT-TEST B MASK)
         (PUSH (IF (SETQ TEM (CAR (RASSOC B ALIST))) TEM B) L))
    (SETQ MASK (BOOLE 2 B MASK))))

;;;; Initialization

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
            (SEND (PROG1 CURRENT-PROCESS (SETQ CURRENT-PROCESS NIL))
                  :RESET T)                     ;T means NOUNWIND
          ;; Some non-essential process.  Leave its state around.
          ;; Later we will ask whether to reset it.
          (SEND WARM-BOOTED-PROCESS :ARREST-REASON :WARM-BOOT)))
       (T (SETQ WARM-BOOTED-PROCESS NIL)))
 ;(SETQ TEMPORARILY-NO-IDLE-SCAVENGING T)
 (SETF (PROCESS-STACK-GROUP INITIAL-PROCESS) %CURRENT-STACK-GROUP)
 (PROCESS-ENABLE INITIAL-PROCESS)               ;enable even if warm-booted out of
 (SETQ CURRENT-PROCESS INITIAL-PROCESS)         ;see kludge in PROCESS-CONSIDER-RUNNABILITY
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

(DEFUN RESET-WARM-BOOTED-PROCESS ()
  "Reset the process warm-booted out of and let it run again."
  (WHEN WARM-BOOTED-PROCESS
    (SEND WARM-BOOTED-PROCESS :RESET T)
    (SEND WARM-BOOTED-PROCESS :REVOKE-ARREST-REASON :WARM-BOOT)
    (SETQ WARM-BOOTED-PROCESS NIL)))

(DEFUN DEBUG-WARM-BOOTED-PROCESS ()
  "Enter the debugger examining the process that was running at the time of the warm boot."
  (IF WARM-BOOTED-PROCESS
      (EH WARM-BOOTED-PROCESS)
    "The warm-booted process has already been reset, or there never was one."))

(DEFUN RESTART-SCHEDULER ()
  ;; Reinitialize the scheduler process, to help debug the thing.
  (SETQ INHIBIT-SCHEDULING-FLAG T)
  (STACK-GROUP-PRESET SCHEDULER-STACK-GROUP #'PROCESS-SCHEDULER)
  (FUNCALL SCHEDULER-STACK-GROUP)
  (SETQ INHIBIT-SCHEDULING-FLAG NIL))

;;; Don't run this the first time, only when the system initializations normally get run
(ADD-INITIALIZATION "Process" '(PROCESS-INITIALIZE) '(SYSTEM NORMAL))

(COMPILE-FLAVOR-METHODS PROCESS SIMPLE-PROCESS)

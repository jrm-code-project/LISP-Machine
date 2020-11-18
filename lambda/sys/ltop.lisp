;;  -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Readtable:ZL; Base:8 -*-
;;; System initialization.  Read-eval-print loop moved to sys;rep

;;; These are used by DEFVAR, DEFPARAMETER and DEFVAR-RESETTABLE in LMMAC.
;;;  These functions are known about by the cold-load builder, which tries to
;;;  initialize self-evaluating-p values so that the world can win before the
;;;  evaluator is working.
(DEFspecialk DEFVAR-1 (&QUOTE SYMBOL &OPTIONAL (VALUE :UNBOUND) DOCUMENTATION)
  (IF (EQ (CAR-SAFE SYMBOL) 'QUOTE) (SETQ SYMBOL (CADR SYMBOL)))
  (WHEN (RECORD-SOURCE-FILE-NAME SYMBOL 'DEFVAR)
    (SETF (GET SYMBOL 'SPECIAL) (OR FDEFINE-FILE-PATHNAME T))
    (AND (NEQ VALUE :UNBOUND)
         (OR FS:THIS-IS-A-PATCH-FILE (NOT (BOUNDP SYMBOL)))
         (SET SYMBOL (EVAL1 VALUE)))
    (WHEN (OR DOCUMENTATION (DOCUMENTATION SYMBOL 'VARIABLE))
      (SETF (DOCUMENTATION SYMBOL 'VARIABLE) DOCUMENTATION)))
  SYMBOL)

(DEFspecialk DEFCONST-1 (&QUOTE SYMBOL &EVAL VALUE &OPTIONAL DOCUMENTATION)
  (IF (EQ (CAR-SAFE SYMBOL) 'QUOTE) (SETQ SYMBOL (CADR SYMBOL)))
  (WHEN (RECORD-SOURCE-FILE-NAME SYMBOL 'DEFVAR)
    (SETF (GET SYMBOL 'SPECIAL) (OR FDEFINE-FILE-PATHNAME T))
    (SET SYMBOL VALUE)
    (WHEN (OR DOCUMENTATION (DOCUMENTATION SYMBOL 'VARIABLE))
      (SETF (DOCUMENTATION SYMBOL 'VARIABLE) DOCUMENTATION)))
  SYMBOL)

(defvar *boot-reset-values* ()
  "List of (variable . warm-boot-reset-value) pairs
for variables which should be set to a known value on warm boot")
;;>> Death to losing special forms!
(defun defvar-resettable-1 (symbol &optional value warm-boot-value documentation)
  (check-type symbol symbol)
  (check-type documentation (or string null))
  (when (record-source-file-name symbol 'defvar)
    (setf (get symbol 'special) (or fdefine-file-pathname t))
    (when (or fs:this-is-a-patch-file (not (boundp symbol)))
      (set symbol value))
    (when (or documentation (documentation symbol 'variable))
      (setf (documentation symbol 'variable) documentation))
    (let ((tem (assq symbol *boot-reset-values*)))
      (if tem
          (setf (cdr tem) warm-boot-value)
        (let ((default-cons-area background-cons-area))
          (push (cons symbol warm-boot-value)
                *boot-reset-values*)))))
  symbol)


(DEFVAR SYN-TERMINAL-IO (MAKE-SYN-STREAM '*TERMINAL-IO*)
  "A synonym stream that points to the value of *TERMINAL-IO*.")

(DEFVAR LISP-CRASH-LIST :UNBOUND
  "List of forms to be evaluated at next warm or cold boot.
Initialized by the cold-load builder for the first cold-boot.")

(DEFVAR ORIGINAL-LISP-CRASH-LIST :UNBOUND
  "List of forms that was evaluated when the cold load was first booted.")

(DEFVAR ERROR-STACK-GROUP :UNBOUND
  "The first level error handler stack group that handles traps from the microcode.")

(DEFVAR %ERROR-HANDLER-STACK-GROUP :UNBOUND
  "Microcode variable that is initialized by warm boot to be ERROR-STACK-GROUP.")

(DEFVAR COLD-BOOT-HISTORY ()
  "List of elements (HOST UNIVERSAL-TIME), one for each time this band was cold-booted.")


(defvar *cold-booting* t
  "T while booting if this is a cold boot.  Always NIL except when booting or disk-saving.")
(DEFVAR COLD-BOOTING)
(forward-value-cell 'cold-booting '*cold-booting*)

;On a LAMBDA, the function SET-PROCESSOR-OWNING-ETHERNET can be called either by the user
;or by lisp-reinitialize (via find-processor-configuration-structure.)  It may need to
;reset the ethernet process, but that isn't safe it that is before PROCESS-INITIALIZE.
(DEFVAR-RESETTABLE PROCESS-INITIALIZE-DONE NIL nil
  "Safe to do things like resetting processes after this is T.")

(DEFVAR *IN-COLD-LOAD-P* T
  "T if we are executing in a cold-load environment.
Will be NIL by the time YOU get to look at it")

;;; Initial values of the following three come out of the cold load builder,
;;; since it gets them from QCOM anyway.
(DEFVAR A-MEMORY-VIRTUAL-ADDRESS :UNBOUND
  "Virtual address that is mapped by software to location 0 of A memory.")

(DEFVAR IO-SPACE-VIRTUAL-ADDRESS :UNBOUND
  "Virtual address mapped into start of XBUS I//O space.")

(DEFVAR UNIBUS-VIRTUAL-ADDRESS :UNBOUND
  "Virtual address mapped into Unibus location 0.")

(DEFVAR MULTIBUS-VIRTUAL-ADDRESS :UNBOUND
  "Virtual address mapped into Multibus location 0.")

(DEFVAR MULTIBUS-IO-VIRTUAL-ADDRESS :UNBOUND
  "Virtual address mapped into Multibus I//O location 0.")

(defvar quad-video-control-virtual-address :unbound
  "Virtual address of page mapped to quad video control registers for this processor's screen.")

(defvar video-board-type :unbound
  "One of :CADR, :VCMEM, :QUAD, or :EXPLORER.")

(DEFVAR-RESETTABLE TV::TV-QUAD-SLOT NIL nil
  "Slot number of this processor's associated tv board.")
(DEFVAR-RESETTABLE SDU-QUAD-SLOT NIL nil
  "Slot number of SDU.")
(DEFVAR-RESETTABLE RG-QUAD-SLOT NIL nil
  "Slot number of this processor's RG board.")

(defvar-resettable *share-code-ready* nil nil
  "Set to T in FIND-PROCESSOR-CONFIGURATION-STRUCTURE after share code is ready to run.")

(ADD-INITIALIZATION "Next boot is a cold boot"
                    '(SETQ *COLD-BOOTING* T)
                    :BEFORE-COLD)

(DEFUN TV::INITIALIZE-RUN-LIGHT-LOCATIONS ()
  (WHEN (VARIABLE-BOUNDP tv::MAIN-SCREEN)
    (let ((old %disk-run-light))
      (SETQ %DISK-RUN-LIGHT
            (+ (* (1- TV::MAIN-SCREEN-HEIGHT) (TV::SHEET-LOCATIONS-PER-LINE TV:MAIN-SCREEN))
               TV::MAIN-SCREEN-BUFFER-ADDRESS
               #o14))
      (%p-store-tag-and-pointer old 0 0)
      (%p-store-tag-and-pointer (%pointer-plus old 2) 0 0)
      (%p-store-tag-and-pointer (%pointer-plus old -2) 0 0))
    (SI::SET-COLD-LOAD-STREAM-HEIGHT
      (MIN (TV::SHEET-HEIGHT tv:MAIN-SCREEN)
           (ARRAY-DIMENSION (%P-CONTENTS-OFFSET SI::COLD-LOAD-STREAM 1) 1)))
    (SI::SET-COLD-LOAD-STREAM-WIDTH (* 32. (CEILING (TV::SHEET-WIDTH tv:MAIN-SCREEN) 32.))))
  (SETQ TV::WHO-LINE-RUN-LIGHT-LOC (+ 2 (LOGAND %DISK-RUN-LIGHT #o777777))))

(add-initialization "Put run lights at the bottom of the screen"
                    '(tv::initialize-run-light-locations)
                    :before-cold)


;;; Come here when machine starts.  Provides a base frame.
(DEFUN LISP-TOP-LEVEL ()
  (LISP-REINITIALIZE NIL)                       ;(Re)Initialize critical variables and things
  (TERPRI (OR TV::INITIAL-LISP-LISTENER *TERMINAL-IO*))
  ;; LISP-TOP-LEVEL1 supposedly never returns, but loop anyway in case
  ;; someone forces it to return with the error-handler.
  (DO-FOREVER
    (IF (FBOUNDP 'PROCESS-TOP-LEVEL)
        (PROCESS-TOP-LEVEL)
      (LISP-TOP-LEVEL1 (OR TV::INITIAL-LISP-LISTENER *TERMINAL-IO*)))))

;;; Called when the main process is reset.
(DEFUN LISP-TOP-LEVEL2 ()
  (LISP-TOP-LEVEL1 (OR TV::INITIAL-LISP-LISTENER *TERMINAL-IO*)))

(defun print-herald-in-cold-load ()
  (IF (FBOUNDP 'PRINT-HERALD)
      (PRINT-HERALD)
    (progn
      (SEND *STANDARD-OUTPUT* :FRESH-LINE)
      (when *in-cold-load-p*
        (PRINC "Lisp Machine cold load environment."))
      (SEND *STANDARD-OUTPUT* :FRESH-LINE)
      ;; *READ//PRINT-BASE* = ")
      (princ "Reading in base ")
      (LET ((*PRINT-BASE* 10.)) (PRINC *READ-BASE*))
      (PRINC "., in package ")
      (PRINC (PACKAGE-NAME *PACKAGE*))
      (PRINC "."))))

;;; Function to reset various things, do initialization that's inconvenient in cold load, etc.
;;; COLD-BOOT is T if this is for a cold boot.
(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T)
                          &AUX (COLD-BOOT *COLD-BOOTING*))
  "Resets various global constants and initializes the error system."

  (unless called-by-user
    (unclosurebind 'inhibit-scheduling-flag 'default-cons-area)
    ;; Flush any likely losing closure binding forwarding pointers
    ;; left around from a closure we were in when we warm booted.
    (UNCLOSUREBIND 'PRIN1 'SELF '*PACKAGE* '*READTABLE*))

  (SETQ INHIBIT-SCHEDULING-FLAG T)              ;In case called by the user
  (SETQ DEFAULT-CONS-AREA WORKING-STORAGE-AREA)

  ;; These are defvar-resettabled too late for the cold-load, since things use getdecl
  (setq local-declarations nil
        ;; file-local-declarations nil
        compiler::qc-file-in-progress nil
        undo-declarations-flag nil
        eh::condition-resume-handlers nil
        eh::condition-handlers nil
        eh::condition-default-handlers nil)

  ;; |||These are similar to above, they get used w/in GETDECL.  --Keith 27oct88
  ;; Bootstrapping globals, for cross-compiler
  (when cold-boot
    ;; Since MAKUNBOUND blows up:
    (setq compiler:*cross-compiling-enabled* nil)
    ;; Compiler knows where this code will run.
    ;; And initially, that's target and host computer are same.
    (setq compiler:*host-computer* '#.compiler:*target-computer*
          compiler:*target-computer* compiler:*host-computer*))

  ;; This is ok to do asap since it doesn't do any evaluation.
  (when (not cold-boot)                         ;>> Not sure if this is right.
    (dolist (x *boot-reset-values*)
      (unless called-by-user (unclosurebind (car x)))
      (set (car x) (cdr x))))

  (unless called-by-user
    (unclosurebind 'zwei::*local-variables* 'zwei::*local-bound-variables*)
    (when (variable-boundp zwei::*local-bound-variables*)
      (apply #'unclosurebind zwei::*local-bound-variables*))
    (when (variable-boundp *default-process-closure-variables*)
      (apply #'unclosurebind *default-process-closure-variables*)))

  (UNLESS (GET 'CDR-NIL 'SYSTEM-CONSTANT)
    (MAPC (LAMBDA (Y)
            (MAPC (LAMBDA (X)
                    (OR (GET X 'SYSTEM-CONSTANT)
                        (SETF (GET X 'SYSTEM-CONSTANT) T)))
                  (SYMBOL-VALUE Y)))
          SYSTEM-CONSTANT-LISTS)
    (MAPC (LAMBDA (Y)
            (MAPC (LAMBDA (X)
                    (OR (GET X 'SPECIAL)
                        (SETF (GET X 'SPECIAL) T)))
                  (SYMBOL-VALUE Y)))
          SYSTEM-VARIABLE-LISTS)
    (PUTPROP T T 'SYSTEM-CONSTANT)
    (PUTPROP T T 'SPECIAL)
    (PUTPROP NIL T 'SYSTEM-CONSTANT)
    (PUTPROP NIL T 'SPECIAL))

  (SELECT-PROCESSOR
    (:EXPLORER
      (SETQ TV::TV-QUAD-SLOT #xf5)
      (SETQ RG-QUAD-SLOT NIL)
      (SETQ SDU-QUAD-SLOT NIL)
      (setq video-board-type :explorer)
      )
    (:LAMBDA
      (setq rg-quad-slot (%lambda-rg-quad-slot))
      (setq sdu-quad-slot (%lambda-sdu-quad-slot))
      (let ((tv (%lambda-tv-quad-slot)))
        (case (ldb (byte 8 8) tv)
          ((0 1)
           (setq tv::tv-quad-slot (ldb (byte 8 0) tv))
           (setq video-board-type :vcmem))
          (2
           (setq video-board-type :quad)
           (setq quad-video-control-virtual-address (lsh (ash 177277400 -1) 1)))
          (t
           (ferror "bad video board type")))))
    (:CADR
      (SETQ TV::TV-QUAD-SLOT nil)
      (SETQ RG-QUAD-SLOT NIL)
      (SETQ SDU-QUAD-SLOT NIL)
      (setq video-board-type :cadr)))

;;; This section below causes DISK-RESTORE and DISK-SAVE to fail.  The microcode
;;; that refers to AMEM-EVCP-VECTOR has all been commented out (previous to this)
;;; and the LMM says that AMEM-EVCP-VECTOR is obsolete.  --mrc
; ;; Provide ucode with space to keep EVCPs stuck into a-memory locations
; ;; by closure-binding the variables that forward there.
;  (UNLESS (AND (BOUNDP 'AMEM-EVCP-VECTOR) AMEM-EVCP-VECTOR)
;    (SETQ AMEM-EVCP-VECTOR
;         (MAKE-ARRAY (+ (LENGTH A-MEMORY-LOCATION-NAMES) #o100 #o20)
;                     ;;                                        in case ucode grows.
;                     :AREA PERMANENT-STORAGE-AREA)))

  (UNLESS CALLED-BY-USER
    ;;|||I am sick of stupid boundp tricks.  COMPILER:MA-RESET doesn't even exist.
    ;;But in case it turns up, magically... -keith 27oct88
    (AND (FBOUNDP 'COMPILER::MA-RESET)  ;Unload microcompiled defs, because they are gone!
         (funcall 'COMPILER::MA-RESET)) ;Hopefully manage to do this before any gets called.
    ;; Set up the TV sync program as soon as possible; until it is set up
    ;; read references to the TV buffer can get NXM errors which cause a
    ;; main-memory parity error halt.  Who-line updating can do this.
    (TV::INITIALIZE-RUN-LIGHT-LOCATIONS)
    ;; Clear all the bits of the main screen after a cold boot.
    (AND COLD-BOOT (CLEAR-SCREEN-BUFFER IO-SPACE-VIRTUAL-ADDRESS)))

  ;; Do something at least if errors occur during loading
  (OR (FBOUNDP 'FERROR) (FSET 'FERROR #'FERROR-COLD-LOAD))
  (OR (FBOUNDP 'CERROR) (FSET 'CERROR #'CERROR-COLD-LOAD))
  (OR (FBOUNDP 'UNENCAPSULATE-FUNCTION-SPEC)
      (FSET 'UNENCAPSULATE-FUNCTION-SPEC (LAMBDA (X) X)))
  (OR (FBOUNDP 'FS::MAKE-PATHNAME-INTERNAL) (FSET 'FS::MAKE-PATHNAME-INTERNAL #'LIST))
  (OR (FBOUNDP 'FS::MAKE-FASLOAD-PATHNAME) (FSET 'FS::MAKE-FASLOAD-PATHNAME #'LIST))

  ;; defined is in sys2;gc  However, we need this stuff far earlier than that.
  (or (variable-boundp gc::*gc-flip-generations*)
      ;; used by the hasharrays before gc is loaded
      (setq gc::*gc-flip-generations* (make-array 4 :initial-element 0
                                                    :area control-tables)))
  ;; used by expansions of gc:without-flipping and gc:without-scavenging
  (or (fboundp 'gc::without-flipping-internal) (fset 'gc::without-flipping-internal 'funcall))
  (or (fboundp 'gc::without-scavenging-internal) (fset 'gc::without-scavenging-internal 'funcall))

  ;; Allow streams to work before WHOLIN loaded
  (OR (BOUNDP 'TV::WHO-LINE-FILE-STATE-SHEET) (SETQ TV::WHO-LINE-FILE-STATE-SHEET 'IGNORE))

  (NUMBER-GC-ON)                                ;This seems to work now, make it the default

  (UNLESS (VARIABLE-BOUNDP *PACKAGE*)
    (PKG-INITIALIZE))
  (SETQ *PACKAGE* PKG-USER-PACKAGE)

  ;; initialize the reader
  ;; Get the right readtable.
  (unless (variable-boundp initial-readtable)
    (setq initial-readtable *readtable*
          *readtable* (copy-readtable *readtable*)
          standard-readtable *readtable*)
    (setf (rdtbl-names *readtable*) (rdtbl-names initial-readtable))
    (setq initial-common-lisp-readtable common-lisp-readtable
          common-lisp-readtable (copy-readtable common-lisp-readtable))
    (setf (rdtbl-names common-lisp-readtable) (rdtbl-names initial-common-lisp-readtable))
    (setq *all-readtables* (list *readtable* common-lisp-readtable)))

  ;; initialize the printer
  (unless (boundp 'prin1) (setq prin1 nil))

  (WHEN (NOT (BOUNDP 'CURRENT-PROCESS))         ;Very first time around
    (SETQ SCHEDULER-EXISTS NIL
          CURRENT-PROCESS NIL
          TV::WHO-LINE-PROCESS NIL
          TV::LAST-WHO-LINE-PROCESS NIL)
    (UNLESS (FBOUNDP 'TV::WHO-LINE-RUN-STATE-UPDATE)
      (FSET 'TV:WHO-LINE-RUN-STATE-UPDATE (LAMBDA (&REST IGNORE) NIL))))

  (SETQ TV::KBD-LAST-ACTIVITY-TIME (TIME))      ; Booting is keyboard activity.
  (INITIALIZE-WIRED-KBD-BUFFER)
  (ecase video-board-type
    (:vcmem
     ;; now that the "unibus" channel is set up, turn on 60Hz interrupts
     ;; first the vector
     (compiler::%nubus-write tv::tv-quad-slot 8
                             (dpb rg-quad-slot (byte 8 24.) (* 4 (+ #o400 #o260))))
     (compiler::%nubus-write tv::tv-quad-slot 4
                             (logior #o40 (compiler::%nubus-read tv::tv-quad-slot 4))))
    (:quad
     (%p-store-tag-and-pointer (%pointer-plus quad-video-control-virtual-address
                                              (// #x14 4))
                               0 #o242)
     ;;read this location to clear pending vertical blank interrupt
     (%p-pointer (%pointer-plus quad-video-control-virtual-address 4))
     ;;clear pending keyboard interrupt
     (%p-pointer (%pointer-plus quad-video-control-virtual-address 3))
     ;;clear pending mouse interrupt
     (%p-pointer (%pointer-plus quad-video-control-virtual-address 11.))
     )
    (:explorer
     (start-si-ints)))

  (SETQ SELF NIL SELF-MAPPING-TABLE NIL)

  (DISABLE-SERVICES)

  (IF COLD-BOOT (SETQ FS::USER-LOGIN-MACHINE NIL))

  ;; The first time, this does top-level EVAL's from the cold-load files
  (OR (BOUNDP 'ORIGINAL-LISP-CRASH-LIST)        ;Save it for possible later inspection
      (SETQ ORIGINAL-LISP-CRASH-LIST LISP-CRASH-LIST))
  (MAPC #'EVAL LISP-CRASH-LIST)
  (SETQ LISP-CRASH-LIST NIL)

  (when (fboundp 'eh::initialize-debugger)
    (eh::initialize-debugger))

  (if (fboundp 'gc::initialize)
      (gc::initialize))

  ;; Reattach IO streams.  Note that *TERMINAL-IO* will be fixed later to go to a window.
  (UNLESS CALLED-BY-USER
    (UNCLOSUREBIND '*TERMINAL-IO* '*STANDARD-OUTPUT* '*STANDARD-INPUT*
                   '*QUERY-IO* '*TRACE-OUTPUT* '*ERROR-OUTPUT* '*DEBUG-IO*)
    (SETQ *TERMINAL-IO*         COLD-LOAD-STREAM
          *STANDARD-OUTPUT*     SYN-TERMINAL-IO
          *STANDARD-INPUT*      SYN-TERMINAL-IO
          *QUERY-IO*            SYN-TERMINAL-IO
          *DEBUG-IO*            SYN-TERMINAL-IO
          *TRACE-OUTPUT*        SYN-TERMINAL-IO
          *ERROR-OUTPUT*        SYN-TERMINAL-IO)
    (SEND *TERMINAL-IO* :HOME-CURSOR))

  (SETQ TV::MOUSE-WINDOW NIL)   ;This gets looked at before the mouse process is turned on
  (KBD-CONVERT-NEW 1_15.)       ;Reset state of shift keys

;  (select-processor
;    (:cadr
;      (WHEN (FBOUNDP 'CADR::CLEAR-UNIBUS-MAP)  ;clear valid bits on unibus map.
;       (CADR:CLEAR-UNIBUS-MAP)))               ; and necessary if sharing Unibus with PDP11.
;                                               ; Do this before SYSTEM-INITIALIZATION-LIST to
;                                               ; avoid screwing ETHERNET code.
;    ((:explorer :lambda)))

  (if (fboundp 'find-processor-configuration-structure)
      (find-processor-configuration-structure))

  ;; These are initializations that have to be done before other initializations
  (INITIALIZATIONS 'SYSTEM-INITIALIZATION-LIST T)
  ;; At this point if the window system is loaded, it is all ready to go
  ;; and the initial Lisp listener has been exposed and selected.  So do
  ;; any future typeout on it.  But if any typeout happened on the cold-load
  ;; stream, leave it there (clobbering the Lisp listener's bits).  This does not
  ;; normally happen, but just in case we do the set-cursorpos below so that
  ;; if anything strange gets typed out it won't get erased.  Note that normally
  ;; we do not do any typeout nor erasing on the cold-load-stream, to avoid bashing
  ;; the bits of whatever window was exposed before a warm boot.
  (COND (CALLED-BY-USER)
        ((FBOUNDP 'TV::WINDOW-INITIALIZE)
         (MULTIPLE-VALUE-BIND (X Y) (SEND *TERMINAL-IO* :READ-CURSORPOS)
           (SEND TV::INITIAL-LISP-LISTENER :SET-CURSORPOS X Y))
         (SETQ *TERMINAL-IO* TV::INITIAL-LISP-LISTENER)
         (SEND *TERMINAL-IO* :SEND-IF-HANDLES :SET-PACKAGE *PACKAGE*)
         (SEND *TERMINAL-IO* :FRESH-LINE))
        (T
         (SETQ TV::INITIAL-LISP-LISTENER NIL)   ;Not created yet
         (SEND *TERMINAL-IO* :CLEAR-REST-OF-LINE)))

  (WHEN CURRENT-PROCESS
    (SEND CURRENT-PROCESS :RUN-REASON 'LISP-INITIALIZE))

  ;; prevent screw from things being traced during initialization
  (if (fboundp 'untrace) (untrace))
  (if (fboundp 'breakon) (unbreakon))

  ;; Have to check explicitly for cold-booting since can't just rely on initializations
  ;; to see that everything in this list has already run (ie at last cold boot)
  ;; since luser may have added own new inits since then
  ;; The "SYSTEM-START-UP-FILE" is run first, because it may set up the network
  ;; address etc.
  (WHEN *COLD-BOOTING*
    (IF (FBOUNDP 'EXECUTE-SYSTEM-STARTUP-FILE) (EXECUTE-SYSTEM-STARTUP-FILE))
    (INITIALIZATIONS 'COLD-INITIALIZATION-LIST))
  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (IF (BOUNDP 'EH::ERROR-TABLE)
      (EH::ENABLE-TRAPPING))

  (AND *COLD-BOOTING*
       (BOUNDP 'TIME:*LAST-TIME-UPDATE-TIME*)
       (let ((frob (catch-error (list si:local-host (get-universal-time)))))
         (when frob
           (push frob cold-boot-history))))
  (SETQ *COLD-BOOTING* NIL)

  (print-herald-in-cold-load)

  ;; This process no longer needs to be able to run except for the usual reasons.
  ;; The delayed-restart processes may now be allowed to run
  (WHEN CURRENT-PROCESS
    (SEND CURRENT-PROCESS :REVOKE-RUN-REASON 'LISP-INITIALIZE)
    (WHEN WARM-BOOTED-PROCESS
      (FORMAT T "Warm boot while running ~S.
Its variable bindings remain in effect;
its unwind-protects have been lost.~%" WARM-BOOTED-PROCESS)
      (WHEN (NOT (OR (EQ (PROCESS-WARM-BOOT-ACTION WARM-BOOTED-PROCESS)
                         'PROCESS-WARM-BOOT-RESTART)
                     (EQ WARM-BOOTED-PROCESS INITIAL-PROCESS)
                     (TYPEP WARM-BOOTED-PROCESS 'SI:SIMPLE-PROCESS)))
        (IF (YES-OR-NO-P "Reset it?  Answer No if you want to debug it.  ")
            (RESET-WARM-BOOTED-PROCESS)
          (FORMAT T "~&Do ~S to examine it, or do
/~S to reset it and let it run again.~%
If you examine it, you will see a state that is not quite the latest one."
                  '(DEBUG-WARM-BOOTED-PROCESS) '(RESET-WARM-BOOTED-PROCESS)))))
    (LOOP FOR (P . RR) IN DELAYED-RESTART-PROCESSES
       DO (WITHOUT-INTERRUPTS
            (SETF (PROCESS-RUN-REASONS P) RR)
            (PROCESS-CONSIDER-RUNNABILITY P)))
    (SETQ DELAYED-RESTART-PROCESSES NIL))

  ;; The global value of *TERMINAL-IO* is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (WHEN (AND (NOT CALLED-BY-USER)
             (FBOUNDP TV::DEFAULT-BACKGROUND-STREAM)
             (NEQ (SYMBOL-FUNCTION TV::DEFAULT-BACKGROUND-STREAM)
                  COLD-LOAD-STREAM))
    (SETQ *TERMINAL-IO* TV::DEFAULT-BACKGROUND-STREAM))

  ;; Now that -all- initialization has been completed, allow network servers if we are an
  ;; ordinary machine.  If we are a primarily a server, an INIT file should call
  ;; (SI:ENABLE-SERVICES).  The idea is that random machines aren't important enough to
  ;; be manually enabled as servers, but real server machines should, so that they
  ;; have a chance to be properly initialized, which would usually include loading an
  ;; INIT file which did things like load patches and salvage the file system.
  (when (fboundp 'get-site-option)
    (unless (get-site-option :server-machine)
       (enable-services))
    (when (and (get-site-option :default-initial-form)
               cold-boot)
      (maybe-execute-default-initial-form)))
  )

(defun maybe-execute-default-initial-form ()
  (let ((form (get-site-option :default-initial-form))
        (*terminal-io* tv:initial-lisp-listener)
        (*read-base* 10.)
        (*print-base* 10.)
        (*readtable* standard-readtable)
        (minutes 1))
    (when form
      (cond ((with-timeout ((* minutes 60. 60.)
                            (format t " -- timed out, Yes.")
                            t)
               (y-or-n-p "Execute the form ~s ? (Automatic Yes after ~d minute~:p.)" form minutes))
             (send *terminal-io* :fresh-line)
             (print form)
             (print (eval form))))
      (send *terminal-io* :fresh-line))))

;;;The SDU on the Lambda can clobber bits on the exposed window.  In
;;;particular, this is annoying when the label gets clobbered.   (Not
;;;every user knows about [Terminal]+[Clear-Screen]).  So, the following
;;;is provided to clean up the labels on every window (we don't know how
;;;many, where, may have been clobbered). -Kmc
;;;
;;;But I don't know if/how this is to be done for Falcon; it depends on
;;;the final solution to the "alert" problem.

#+(target falcon)
(warn "Maybe need to redo or flush MAYBE-REDRAW-WINDOW-LABELS warm ~
initialization for Falcon?")

(defun maybe-redraw-window-labels ()
  (unless (or *in-cold-load-p* (null tv:main-screen))
    (dolist (window (send tv:main-screen :inferiors))
      ;;;We redraw the label on every top-level window, regardless of exposure status,
      ;;;because we can't now what was exposed and clobbered, e.g. before warm-boot.
      (send-if-handles window :redraw-label))))

(add-initialization "Maybe redraw possibly clobbered window labels"
                    '(maybe-redraw-window-labels)
                    '(:normal :warm))

(DEFUN ENABLE-SERVICES (&REST SERVICES)
  (DECLARE (IGNORE SERVICES))
  (INITIALIZATIONS 'ENABLE-SERVICES-INITIALIZATION-LIST)
  (RESET-INITIALIZATIONS 'DISABLE-SERVICES-INITIALIZATION-LIST))

(DEFUN DISABLE-SERVICES (&REST SERVICES)
  (DECLARE (IGNORE SERVICES))
  (INITIALIZATIONS 'DISABLE-SERVICES-INITIALIZATION-LIST)
  (RESET-INITIALIZATIONS 'ENABLE-SERVICES-INITIALIZATION-LIST))

(defun start-si-ints ()
  ;;first the 60hz
  (%nubus-write #xf5 #xf00014 #xf6e00030)
  (%nubus-write #xf5 #xe00068 #x40)
  (%nubus-write #xf5 #xf00040 2)
  ;;clear pending 60 Hz interrupt
  (%nubus-read #xf5 #xe00068)
  ;; now keyboard
  (new-enable-keyboard-ints)

  ;; set warm boot vector
  (%nubus-write #xf5 #xf00020 #xf6e00004)

  )

(DEFUN UNCLOSUREBIND (&REST SYMBOLS)
  "If any of SYMBOLS has a closure binding evcp pointer in its value cell, remove it.
Does not change the value of the symbol, but unshares it with the closure.
This does not need to be done on A-memory variables."
  (DOLIST (SYMBOL SYMBOLS)
    (LET ((LOC (FOLLOW-CELL-FORWARDING (VALUE-CELL-LOCATION SYMBOL) NIL)))
      (IF (= (%P-DATA-TYPE LOC) DTP-EXTERNAL-VALUE-CELL-POINTER)
          (%BLT-TYPED (FOLLOW-CELL-FORWARDING LOC T) LOC 1 1)))))

(DEFUN CLEAR-SCREEN-BUFFER (BUFFER-ADDRESS)
  (%P-DPB 0 %%Q-LOW-HALF BUFFER-ADDRESS)
  (%P-DPB 0 %%Q-HIGH-HALF BUFFER-ADDRESS)
  (%BLT BUFFER-ADDRESS (1+ BUFFER-ADDRESS)
        #o77777 1))

;;; This is a temporary function, which turns on the "extra-pdl" feature
(DEFUN NUMBER-GC-ON (&OPTIONAL (ON-P T))
  (SETQ NUMBER-CONS-AREA
        (IF ON-P EXTRA-PDL-AREA WORKING-STORAGE-AREA)))


;;;; Initialization stuff

(DEFVAR BEFORE-COLD-INITIALIZATION-LIST NIL
  "Initializations to be run before doing a DISK-SAVE.")

(defun amazing-kludge-order-cold-initialization-list ()
  "Temporary, until initialization list feature generalized"
  ;put "assure lam symbols loaded" first, before net taken down.
  (let ((frob (assoc "Assure LAM Symbols loaded" before-cold-initialization-list)))
    (cond (frob
           (setq before-cold-initialization-list (delq frob before-cold-initialization-list))
           (push frob before-cold-initialization-list)))))

(DEFVAR COLD-INITIALIZATION-LIST NIL
  "Initializations to be run on cold boot.")
(DEFVAR WARM-INITIALIZATION-LIST NIL
  "Initializations to be run on warm or cold boot.")
(DEFVAR ONCE-ONLY-INITIALIZATION-LIST NIL
  "Initializations to be run only once.  They have indeed been run if they are here.")
(DEFVAR SYSTEM-INITIALIZATION-LIST NIL
  "Initializations to be run on warm boot, before the COLD and WARM ones.")
(DEFVAR LOGIN-INITIALIZATION-LIST NIL
  "Initializations to be run on logging in.")
(DEFVAR LOGOUT-INITIALIZATION-LIST NIL
  "Initializations to be run on logging out.")
(DEFVAR DISABLE-SERVICES-INITIALIZATION-LIST NIL
  "Initializations to be run after disabling network services.")
(DEFVAR ENABLE-SERVICES-INITIALIZATION-LIST NIL
  "Initializations to be run before enabling network services.")

;;; Some code relies on INIT-NAME being the CAR of the init entry.  **DO NOT CHANGE THIS**
(DEFSTRUCT (INIT-LIST-ENTRY :LIST
                            (:CONSTRUCTOR MAKE-INIT-LIST-ENTRY (NAME FORM FLAG SOURCE-FILE))
                            (:CONC-NAME "INIT-")
                            (:ALTERANT NIL))
  (NAME NIL :DOCUMENTATION "Pretty name for this initialization (a string)")
  (FORM NIL :DOCUMENTATION "The actual code to eval when the initialization is run")
  (FLAG NIL :DOCUMENTATION "NIL means that this initialization has not yet been run")
  (SOURCE-FILE NIL :DOCUMENTATION "The source file for this initialization"))

(DEFMACRO INIT-LIST-CHECK (NAME)
  `(PROGN (OR (BOUNDP ,NAME)
              (SET ,NAME NIL))
          (OR (GET ,NAME 'INITIALIZATION-LIST)
              (PUTPROP ,NAME T 'INITIALIZATION-LIST))))

(DEFUN INITIALIZATIONS (LIST-NAME &OPTIONAL (REDO-FLAG NIL) (FLAG T))
  "Run the inits in the initialization list whose name is LIST-NAME.
REDO-FLAG if non-NIL says rerun inits that are marked as already run.
If FLAG is T, inits are marked as run; if NIL, they are marked as not already run."
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((INIT (SYMBOL-VALUE LIST-NAME) (CDR INIT)))
      ((NULL INIT))
    (WHEN (OR (NULL (INIT-FLAG (CAR INIT))) REDO-FLAG)
      (CATCH-ERROR-RESTART ((ERROR) "Abort the ~A initialization."
                            (INIT-NAME (CAR INIT)))
        (EVAL (INIT-FORM (CAR INIT))))
      (SETF (INIT-FLAG (CAR INIT)) FLAG))))

;;; Adds a new init to the list.
;;; Keywords are:
;;; NOW         Run the init now
;;; FIRST       Run the init now if this is the first entry for the specified name
;;; NORMAL      Do the "normal" thing (init when initializations normally run)
;;; REDO        Do nothing now, but set up things so init gets redone
;;; COLD        Use the cold boot list
;;; WARM        Use the warm boot list
;;; ONCE        Use the once-only list
;;; SYSTEM      Use the system list
;;; BEFORE-COLD The list that gets done before disk-save'ing out
;;; LOGIN       Use the login list
;;; LOGOUT      Use the logout list
;;; SITE        Use the site list (also run once)
;;; SITE-OPTION Use the site-option list (also run once)
;;; HEAD-OF-LIST If entry not presently on list, add it to front instead of the end of list.
;;; If neither WARM nor COLD are specified, warm is assumed.  If a fourth argument
;;; is given, then it is the list to use.  WARM and COLD will override the fourth argument.
(DEFCONST INITIALIZATION-KEYWORDS
          '((:SITE SITE-INITIALIZATION-LIST :NOW)
            (:SITE-OPTION SITE-OPTION-INITIALIZATION-LIST :NOW)
            (:DISABLE-SERVICES DISABLE-SERVICES-INITIALIZATION-LIST)
            (:ENABLE-SERVICES ENABLE-SERVICES-INITIALIZATION-LIST)
            (:SYSTEM SYSTEM-INITIALIZATION-LIST :FIRST)
            (:FULL-GC GC::FULL-GC-INITIALIZATION-LIST)
            (:AFTER-FULL-GC GC::AFTER-FULL-GC-INITIALIZATION-LIST)
            (:GC-SYSTEM-RELEASE GC::GC-SYSTEM-RELEASE-INITIALIZATION-LIST)
            (:ONCE ONCE-ONLY-INITIALIZATION-LIST :FIRST)
            (:LOGIN LOGIN-INITIALIZATION-LIST)
            (:LOGOUT LOGOUT-INITIALIZATION-LIST)
            (:WARM WARM-INITIALIZATION-LIST)
            (:COLD COLD-INITIALIZATION-LIST)
            (:BEFORE-COLD BEFORE-COLD-INITIALIZATION-LIST)
            (:network-driver net:network-driver-initialization-list))
  "Alist defining keywords accepted by ADD-INITIALIZATION.
Each element looks like (KEYWORD LIST-VARIABLE-NAME [TIME-TO-RUN])
TIME-TO-RUN should be NOW, FIRST, NORMAL or REDO, or omitted.
It is a default in case the ADD-INITIALIZATION doesn't specify any of them.")

(DEFUN ADD-INITIALIZATION (NAME FORM &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST)
                                     &AUX WHEN DEFAULT-WHEN INIT HEAD-OF-LIST)
  "Add an initialization with name NAME and definition FORM to an initialization list.
NAME should be a string and FORM an expression to be evaluated later.
KEYWORDS can be one keyword or a list of them.  These keywords can be in any package.
Keywords can either be HEAD-OF-LIST, meaning add to front of list rather than the end,
:COLD, :WARM, :ONCE, :SYSTEM, :BEFORE-COLD, :LOGIN, :LOGOUT, :SITE, :SITE-OPTION,
 :FULL-GC or :AFTER-FULL-GC, :ENABLE-SERVICES, :DISABLE-SERVICES, specifying a list,
or :NOW, :FIRST, :NORMAL or :REDO, saying when to run the init.
NOW means run the init as well as adding to the list;
:FIRST means run the init now if it isn't on the list;
:NORMAL means don't run the init now;
:REDO means don't run it now, but mark it as never having been run
 even if it is already on the list and has been run.
If the keywords do not specify the list, LIST-NAME is used.
The default for it is SI:WARM-INITIALIZATION-LIST."
  (DOLIST (S (IF (CL:LISTP KEYWORDS) KEYWORDS (LIST KEYWORDS)))
    (LET* ((V (GET-PNAME S))
           (KEYDEF (ASS #'STRING= V INITIALIZATION-KEYWORDS)))
      (IF KEYDEF (SETQ LIST-NAME (CADR KEYDEF)
                       DEFAULT-WHEN (CADDR KEYDEF))
        (COND ((MEM #'STRING= V '(:NOW :FIRST :NORMAL :REDO))
               (SETQ WHEN V))
              ((STRING= ':HEAD-OF-LIST V)
               (SETQ HEAD-OF-LIST T))
              (T (FERROR "Illegal keyword ~S" S))))))
  (SELECTOR (OR WHEN DEFAULT-WHEN) STRING=
    ((NIL) (SETQ WHEN NIL))
    ((:NORMAL) (SETQ WHEN NIL))
    ((:NOW) (SETQ WHEN ':NOW))
    ((:REDO) (SETQ WHEN ':REDO))
    ((:FIRST) (SETQ WHEN ':FIRST)))
  (INIT-LIST-CHECK LIST-NAME)
  (SETQ INIT
        (DOLIST (L (SYMBOL-VALUE LIST-NAME)
                   (IF (OR HEAD-OF-LIST (NULL (SYMBOL-VALUE LIST-NAME)))
                       (CAR (PUSH (MAKE-INIT-LIST-ENTRY NAME FORM NIL FDEFINE-FILE-PATHNAME)
                                  (SYMBOL-VALUE LIST-NAME)))
                     ;;Please, whoever, when you change RPLACD to SETF, remember that
                     ;;the returned value needs one less CDR...
                     (CAR (SETF (CDR (LAST (SYMBOL-VALUE LIST-NAME)))
                                (NCONS (MAKE-INIT-LIST-ENTRY
                                         NAME FORM NIL FDEFINE-FILE-PATHNAME))))))
          (WHEN (STRING= (INIT-NAME L) NAME)
            (cond ((and head-of-list
                        (not (eq l (car (symbol-value list-name)))))
                   ;move it to head of list even if already on list.
                   (setf (symbol-value list-name)
                         (delq l (symbol-value list-name))))
                  (t
                   (SETF (INIT-FORM L) FORM)
                   (SETF (INIT-SOURCE-FILE L) FDEFINE-FILE-PATHNAME)
                   (RETURN L))))))
  (COND ((EQ WHEN ':REDO) (SETF (INIT-FLAG INIT) NIL))
        ((OR (EQ WHEN ':NOW)
             (AND (EQ WHEN ':FIRST) (NULL (INIT-FLAG INIT))))
         (EVAL (INIT-FORM INIT))
         (SETF (INIT-FLAG INIT) T))))

;;; Deletes an init from the list.
;;; All list-name keywords (see INITIALIZATION-KEYWORDS) are allowed.
;;; If there is one, it overrides the third argument.
(DEFUN DELETE-INITIALIZATION (NAME &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST))
  "Remove any initialization named NAME from an initialization list.
NAME should be a string.  KEYWORDS can be a keyword or a list of them;
packages do not matter.  The only thing you can specify with one
is what list to remove from.  Or let KEYWORDS be NIL and supply the
list name symbol as LIST-NAME.
If both KEYWORDS NIL and LIST-NAME are nil, delete from all lists."
  (if (and (null keywords)
           (null list-name))
      (dolist (il initialization-keywords)
        (delete-initialization name nil (second il)))
    (DO ((L KEYWORDS (CDR L))
         KEYDEF V)
        ((NULL L))
      (SETQ V (SYMBOL-NAME (CAR L)))
      (IF (SETQ KEYDEF (ASS #'STRING= V INITIALIZATION-KEYWORDS))
          (SETQ LIST-NAME (CADR KEYDEF))
        (FERROR "Illegal keyword ~S" (CAR L))))
    (INIT-LIST-CHECK LIST-NAME)
    (DO ((L (SYMBOL-VALUE LIST-NAME) (CDR L))
         (FLAG NIL))
        ((NULL L) FLAG)
      (WHEN (STRING= (INIT-NAME (CAR L)) NAME)
        (SET LIST-NAME (DELQ (CAR L) (SYMBOL-VALUE LIST-NAME)))
        (SETQ FLAG T)))))

(DEFUN RESET-INITIALIZATIONS (LIST-NAME)
  "Mark all the inits in the initialization list named LIST-NAME as not yet run."
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L (SYMBOL-VALUE LIST-NAME) (CDR L)))
      ((NULL L))
    (SETF (INIT-FLAG (CAR L)) NIL)))

;;; This is an old name for FDEFINE which everyone uses.
(DEFUN FSET-CAREFULLY (FUNCTION-SPEC DEFINITION &OPTIONAL NO-QUERY-FLAG)
  "This is obsolete.  It is equivalent to (FDEFINE FUNCTION-SPEC DEFINITION T FORCE-FLAG)."
  (FDEFINE FUNCTION-SPEC DEFINITION T NO-QUERY-FLAG))


;;; Stuff which has to go somewhere, to be around in the cold-load,
;;; and doesn't have any logical place where it belongs

(DEFVAR USER-ID ""
  "String for the name you are logged in as, or an empty string if not logged in.")

;;; This is here rather than with the scheduler because it has to be
;;; in the cold-load.  It checks for the non-existence of a scheduler
;;; and does it itself in that case.

;;; Takes a predicate and arguments to it.  The process becomes blocked
;;; until the application of the predicate to those arguments returns T.
;;; Note that the function is run in the SCHEDULER stack group, not the
;;; process's stack group!  This means that bindings in effect at the
;;; time PROCESS-WAIT is called will not be in effect; don't refer to
;;; variables "freely" if you are binding them.
;;;    Kludge:  if the scheduler seems broken, or we ARE the scheduler
;;; (i.e. a clock function tries to block), then loop-wait (no blinkers...)

;;; In case of a process-level interrupt while waiting, this function can get
;;; restarted from its beginning.  Therefore, it must not modify its arguments,
;;; and the way it does its WITHOUT-INTERRUPTS must not be changed.
;;; See (:METHOD SI:PROCESS :INTERRUPT)
(DEFUN PROCESS-WAIT (WHOSTATE FUNCTION &REST ARGUMENTS)
  "Wait until FUNCTION applied to ARGUMENTS returns T.
WHOSTATE is a string to appear in Peek and the who-line until then.
Note that FUNCTION will be called in the scheduler stack group,
so your special variable bindings will not be available.
Pass whatever data or pointers you need in the ARGUMENTS."
  (COND ((APPLY FUNCTION ARGUMENTS)     ;Test condition before doing slow stack-group switch
         NIL)                           ;Hmm, no need to wait after all
        ((AND SCHEDULER-EXISTS
              (EQ SCHEDULER-STACK-GROUP %CURRENT-STACK-GROUP)
              CURRENT-PROCESS)
         ;; Called PROCESS-WAIT from a process's wait-function!
         ;; Rather than hang the system, just say the process is not runnable now.
         (THROW 'PROCESS-WAIT-IN-SCHEDULER NIL))
        ((OR (NOT SCHEDULER-EXISTS)
             (EQ SCHEDULER-STACK-GROUP %CURRENT-STACK-GROUP)
             (NULL CURRENT-PROCESS)
             (LET ((STATE (SG-CURRENT-STATE SCHEDULER-STACK-GROUP)))
               (NOT (OR (= STATE SG-STATE-AWAITING-INITIAL-CALL)
                        (= STATE SG-STATE-AWAITING-CALL)
                        (= STATE SG-STATE-AWAITING-RETURN)))))
         (DO-FOREVER
           (WHEN (APPLY FUNCTION ARGUMENTS)
             (RETURN NIL))))
        (T
         (WITHOUT-INTERRUPTS            ;A sequence break would reset my state to "running"
           (SETF (PROCESS-WAIT-WHOSTATE CURRENT-PROCESS) (OR WHOSTATE "Wait"))
           (TV::WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)
           (SET-PROCESS-WAIT CURRENT-PROCESS FUNCTION ARGUMENTS)
           ;; DON'T change this SEND to a STACK-GROUP-RESUME!  The scheduler
           ;; needs to know what the process's current stack group is.
           (FUNCALL SCHEDULER-STACK-GROUP))
         (TV::WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS))))

;;;; System initialization
(DEFVAR QLD-MINI-DONE NIL)

;;; Procedure for booting up a world load:
;;; 1. Use MINI to load CLPACK.  Create packages.
;;; 2. Use MINI to load kernel system, viz. FORMAT, flavors, processes, error handler,
;;;    chaos, QFILE.
;;; 3. Do a quasi cold boot.  This turns on the real file system.
;;; 4. Load MAKSYS and SYSDCL to build the initial systems.
;;; 5. Use MAKE-SYSTEM to load the rest of the top level system.

(DEFUN QLD (&OPTIONAL (LOAD-KEYWORDS '(:NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION))) ; :SILENT
  "Load the rest of the Lisp machine system into the cold load.
Used only if you are not generating a new Lisp machine system version."
  (IF QLD-MINI-DONE
      (LOGIN "LISPM" T)
    (terpri)
    (princ "Initializing structure handles... ")
    (setup-structure-handles)
    (princ "done.")
    (TERPRI)
    (PRINC "Loading inner-system-file-alist")
    (MINI-LOAD-FILE-ALIST '(("SYS: SYS; INNER-SYSTEM-FILE-ALIST QFASL >" "SI" T)))
    (TERPRI)
    (PRINC "Loading inner system")
    (LET ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
      (MINI-LOAD-FILE-ALIST INNER-SYSTEM-FILE-ALIST)
      ;; Even though PATHNM is now loaded, it doesn't work yet.  So must disable
      ;; FS::MAKE-FASLOAD-PATHNAME until it does.
      (letf (((symbol-function 'fs::make-fasload-pathname) #'list))
        (MINI-LOAD-FILE-ALIST REST-OF-PATHNAMES-FILE-ALIST)
        ;; Read the site files.  Now that QFILE is loaded, this will work properly.
        (UPDATE-SITE-CONFIGURATION-INFO)))      ;Setup site dependent stuff
    (LISP-REINITIALIZE T)                       ;Turn on network, load error table, etc.
    ;;
    ;; +++ why log onto sys host -- may lose w/network... <29-Oct-88 keith>
    (LOGIN "LISPM" (SEND (FS:GET-PATHNAME-HOST "SYS") :PHYSICAL-HOST) T)
    (FS::CANONICALIZE-COLD-LOAD-PATHNAMES)      ;Update properties for real pathnames
    ;; Load MAKE-SYSTEM so we can use it for the rest.
    (DOLIST (F SYSTEM-SYSTEM-FILE-ALIST)
      (LOAD (CAR F) :PACKAGE (CADR F) :SET-DEFAULT-PATHNAME NIL))
    (SETQ QLD-MINI-DONE T))                     ;So that we can do file I/O
  (PRINC "Loading System via MAKE-SYSTEM")
  (LET ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
    ;;; +++ Someday, do (MAKE-SYSTEM :OUTER-SYSTEM) explicitly in QLD <08-Nov-88 keith>.
    (APPLY #'MAKE-SYSTEM "System" LOAD-KEYWORDS)
    (QLD-LOAD-OTHER-SYSTEMS LOAD-KEYWORDS))
  (setq si:original-lisp-crash-list nil)
  (SETQ *IN-COLD-LOAD-P* NIL)
  (clear-input)
  ;;Last system procedures
  (and (yes-or-no-p-with-timeout (* 60. 60. 3.) t "~&Analyze object-use of all loaded files?")
       (si:analyze-all-files))
  (and (yes-or-no-p-with-timeout (* 60. 60. 2.) t "~&Deconfigure network?")
       (net:deconfigure))
  (and (yes-or-no-p-with-timeout (* 60. 60. 2.) nil "~&Perform system-release GC?")
       (gc:full-gc :mode :system-release))
  ;;
  (format t "~2%This is probably a good time to do a ~S:~2&" 'disk-save)
  (FORMAT T "~&(Partition size = ~D.)~%" (ESTIMATE-DUMP-SIZE))
  (PRINT-DISK-LABEL))

;;;This a separate function so it can be retried easily if it bombs:

(DEFUN QLD-LOAD-OTHER-SYSTEMS (LOAD-KEYWORDS)
  ;;; $$$ Suggest to user that they load (OUTER-SYSTEM). <08-Nov-88 keith>
  (DOLIST (SYSTEM (PROMPT-AND-READ :READ "List of names of additional systems to load, such as `(OUTER-SYSTEM)' :~%"))
    (APPLY #'MAKE-SYSTEM SYSTEM LOAD-KEYWORDS)))

;;; This is used for things like host tables which can get loaded again when the world
;;; is already built:

(DEFUN MAYBE-MINI-LOAD-FILE-ALIST (ALIST)
  (IF (NOT QLD-MINI-DONE)
      (MINI-LOAD-FILE-ALIST ALIST)
    (DOLIST (F ALIST)
      (LOAD (CAR F) :PACKAGE (CADR F) :SET-DEFAULT-PATHNAME NIL))))

(DEFVAR SYSTEM-SYSTEM-FILE-ALIST NIL)
;; setq'd by SYS:SYS;INNER-SYSTEM-FILE-ALIST



;stuff to make mini-disk-save work

(DEFUN DISK-RESTORE-DECODE (PARTITION &AUX LOW-16-BITS HI-16-BITS)
  (COND ((NULL PARTITION)
         (SETQ LOW-16-BITS 0 HI-16-BITS 0))
        ((NUMBERP PARTITION)
         (SETQ LOW-16-BITS (+ #/L (LSH #/O 8)))
         (SETQ HI-16-BITS (+ #/D (LSH (+ #/0 PARTITION) 8))))
        ((OR (SYMBOLP PARTITION) (STRINGP PARTITION))
         (IF (= (STRING-LENGTH PARTITION) 1)
             (SETQ PARTITION (STRING-APPEND "LOD" PARTITION))
           (SETQ PARTITION (STRING PARTITION)))
         (SETQ LOW-16-BITS (+ (CHAR-UPCASE (AREF PARTITION 0))
                              (LSH (CHAR-UPCASE (AREF PARTITION 1)) 8)))
         (SETQ HI-16-BITS (+ (CHAR-UPCASE (AREF PARTITION 2))
                             (LSH (CHAR-UPCASE (AREF PARTITION 3)) 8))))
        (T (FERROR "~S is not a valid partition name." PARTITION)))
  (LIST HI-16-BITS LOW-16-BITS))


(DEFUN MINI-DISK-SAVE (PARTITION)
  ;; Cause cold boot initializations to happen when rebooted
  ;; and do the BEFORE-COLD initializations now
  (SETQ *COLD-BOOTING* T) ; usually on before-cold-initialization-list
  (SETQ WHO-LINE-JUST-COLD-BOOTED-P T)
  (WITHOUT-INTERRUPTS
    ;; The process we are now executing in will look like it was warm-booted when
    ;; this saved band is restored.  Suppress the warm-boot message, but disable
    ;; and flush the process so it doesn't start running with its state destroyed.
    ;; We'd like to :RESET it, but can't because we are still running in it.
    ;; If the process is the initial process, it will get a new state and get enabled
    ;; during the boot process.
    (COND ((NOT (NULL CURRENT-PROCESS))
           (PROCESS-DISABLE CURRENT-PROCESS)
           (SET-PROCESS-WAIT CURRENT-PROCESS #'FALSE NIL)
           (SETQ CURRENT-PROCESS NIL)))
    (when (fboundp 'find-max-addr)
      (let ((MAX-ADDR (FIND-MAX-ADDR)))
        ;; Store the size in words rather than pages.  But don't get a bignum!
        (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-HIGHEST-VIRTUAL-ADDRESS)
              (LSH MAX-ADDR 8))))
    (DO ((I #o600 (1+ I)))                      ;Clear the disk error log
        ((= I #o640))
      (%P-STORE-TAG-AND-POINTER I 0 0))
    (let ((l (disk-restore-decode partition)))
      (%DISK-SAVE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE)
                  (CAR L) (CADR L)))))

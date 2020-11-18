;;; -*- Mode:LISP; Package:GC; Lowercase:Yes; Base:10; Readtable:CL -*-

;;; (c) Copyright 1980, Massachusetts Institute of Technology.
;;; (c) Copyright 1986, 1985, Lisp Machine Incorporated (LMI).

;;; Written June 1985 by KHS, once of LMI.  Revised December 1985.

;;; This file contains the (Common) Lisp-coded support for the Garbage Collector.
;;; Some GC-related functions that need to be in the cold-load can be found in QRAND.

(in-package "GC" :use '("GLOBAL" "SYSTEM" "SYSTEM-INTERNALS"))

(export '(flip full-gc status print-statistics gc-on gc-off
          without-flipping without-scavenging))

;;;$$$ Use IMPORT rather than SHADOWING-IMPORT. <11nov88 Keith>

(import '(si::%region-list-thread
           si::%area-region-list
           si::%area-type
           si::with-quick-region-area-accessors
           si::for-every-region-in-area)
        :gc)

;;; GC must be disabled while loading this file.
(defvar *gc-disabled-during-loading* nil)

(eval-when (load)
  ;; Put the GC into its idle state.
  (if (fboundp 'reclaim-oldspace) (reclaim-oldspace :batch))
  (when (fboundp 'gc-off)
    (when (fboundp 'tv:notify)
      (tv:notify () "Turning off GC while new one is loaded."))
    (gc-off)
    (when (boundp '*gc-process*)
      (send *gc-process* :kill)
      (send *gc-process* :revoke-arrest-reason :gc-stopped)
      (send *gc-process* :run-reason))
    (makunbound '*gc-process*)
    (setq *gc-disabled-during-loading* t)))



;;; GC::INITIALIZE (called by LISP-REINITIALIZE) uses the :GC-STOPPED arrest reason to decide
;;; whether to restart the GC process, so it should be there initially.
(defvar *gc-process* (make-process "Garbage Collector"
                                   :warm-boot-action 'ignore
                                   :arrest-reasons '(:gc-stopped))
  "Process that runs the garbage collector.")

;;; GC::INITIALIZE ensures that this always wakes up at 0.  See below for a discussion of
;;; the without-flipping protocol.
(defvar-resettable *inhibit-flipping-count* 0 0
  "If non-zero flipping is inhibited.  WITHOUT-FLIPPING uses this.")

(defvar-resettable *this-process-is-inhibiting-flipping* () ()
  "Bound to T in any stack group that's inside a WITHOUT-FLIPPING form.
This is so one can find all processes inhibiting flipping, if necessary.")

(defvar *all-flips-to-higher-address-space* nil
  "Bound to T for the first of two consecutive flips by FULL-GC
when :SYSTEM-RELEASE mode is used.  Causes all new regions to
be created after the highest virtual page when the flip happens.")

;;; This is forwarded to A-memory -- set up by the cold-load builder.  The microcode sets
;;; this to T as part of warm or cold booting.
(defvar inhibit-scavenging-flag :unbound
  "If non-nil, all forms of scavenging (scavenging due to consing, idle scavenging) are
inhibited.  If this condition persists for a long time, there is a risk that the GC will not
terminate in time to prevent address space overflow.")

;;; In SYS2; LMMAC
;(defmacro gc:without-flipping (&body body)
;  "Execute BODY inhibiting all GC flips."
;  (gc::without-flipping-internal (lambda () . ,body)))
;(defmacro gc:without-scavenging (&body body)
;  "Execute BODY with scavenger disabled.  WITHOUT-INTERRUPTS effectively disables
;idle scavenging, but scavenger can still run if you cons, so use this form instead."
;  (without-interrupts
;    . ,body))

;;; Indirected to the system-communication area by the cold-load builder.
(defvar %gc-generation-number :unbound
  "A sequence number incremented at each flip.  Used by hash-tables and others to determine
that addresses have changed.  See also GC::*GC-FLIP-GENERATIONS*.")

;;; This is actually initialized in SYS; LTOP for the hash-table code, which needs to know
;;; whether rehashing is necessary long before the garbage-collector is loaded. (i.e. long
;;; before rehashing -can- be necessary...).
(defvar *gc-flip-generations* #(0 0 0 0)
  "The most recent %GC-GENERATION-NUMBER that could change addresses in each volatility level.
This is used by hash tables to decide whether or not to rehash.")

(defvar *oldspace-words* 0
  "The number of words in all extant oldspace regions.")

(defvar *condemned-newspace-words* 0
  "The number of words originally in newspace but now either reclaimed or transported to copy
regions.  This number plus the number of words currently in new and static regions is the total
number of words ever allocated in this world.")

(defvar *gc-status* ()
  "NIL if the garbage collector is idle (there is no oldspace), or the volatility of the
the most recent flip.")

(defvar *batch-reclaim-count* 0
  "Normally zero, but non-zero during a batch scavenge prior to reclamation.  The
actual value indicates the number of processes currently scavenging batchly.")

(defvar *level-control* #(() () () ())
  "Controls garbage collection of each level.  Possible values for each level are:
NIL, meaning don't flip this level automatically.  T, meaning flip this level when
necessary as determined by the committed free space.  A fixnum, meaning flip this level
when the amount of storage in this and all more-volatile levels exceeds this threshold.")

(defvar *gc-degree* 1
  "The configured degree of the GC, an integer from 0 to 3 inclusive.
See the documentation of GC:GC-ON for information what the values mean.")

(deftype volatility () '(integer 0 3))

(defvar *report-stream* t
  "Stream to write GC messages on, or NIL meaning don't, or T meaning make notifications.")

;;; This is pretty simplistic.
(defvar *report-volatility* 1
  "Inhibit GC reports about events affecting levels more volatile than this.")

(defun report (format-control &rest format-args)
  (unless (> *flip-volatility* *report-volatility*)
    (cond ((null *report-stream*))
          ((eq *report-stream* t)
           (apply #'process-run-function "GC Notification"
                  #'tv:notify () format-control format-args))
          (t
           (send *report-stream* :fresh-line)
           (apply #'format *report-stream* format-control format-args)))))

;;; The initialization keywords corresponding to these lists are defined in SYS; LTOP.

(defvar after-full-gc-initialization-list ()
  "Initializations performed after GC:FULL-GC.")

(defvar full-gc-initialization-list ()
  "Initializations performed before GC:FULL-GC.")

(defvar gc-system-release-initialization-list ()
  "Initializations performed during (GC:FULL-GC :MODE :SYSTEM-RELEASE)")

;;; I changed my mind about the pseudo-static areas, mainly because the only areas
;;; I could really justify not flipping were NR-SYM and P-N-STRING, and they really
;;; ought to be GCed because of gensyms.



;;;; GC Initialization.

(defun initialize ()
  ;; Called by LISP-REINITIALIZE.  Make sure the microcode variables are consistent
  ;; with reality, as far as we can determine it.  If the GC process was running,
  ;; reset and restart it.
  (setq-globally *this-process-is-inhibiting-flipping* ())
  (setq *this-process-is-inhibiting-flipping* ())
  (setq *inhibit-flipping-count* (setq *batch-reclaim-count* 0))
  (unless si::*in-cold-load-p*
    (without-interrupts
      (setq *oldspace-words* (aref (compute-storage-distribution) 6))
      (if (= *oldspace-words* 0)
          (setq %gc-flip-ready t)
        ;; %gc-flip-ready is true if all scavengeable regions are fully scavenged.
        (setq %gc-flip-ready (loop for region from 0 below sys:number-of-regions
                                   when (= (%region-scavenge-enable region) 1)
                                   always (= (%region-free-pointer region)
                                             (%region-gc-pointer region))))))
    ;; This should be a system initialization (actually, it should just be on, and the software
    ;; shouldn't have to be so suspicious of it).  However, the GC statistics are meaningless
    ;; without it, so we make sure.
    (si::turn-on-microsecond-clock-if-present)
    (make-pdl-areas-static)
    ;; I don't think one can expect to make this any smaller than 64. and still win.
    (write-meter '%transporter-scavenge-queue-work-quantum 64.)
    ;; If we died while scavenging (the only reason why there should ever be oldspace
    ;; during initialization), do something.  I don't really know what "something"
    ;; is yet.  In the worst case, the scavenger crashed the machine, and should not
    ;; be restarted.  If we had a CRASH-TABLE in the microcode, some negotiation with
    ;; the SDU could tell us whether it was the GC's fault.
    (when (null %gc-flip-ready)
      (if (and (boundp 'tv:selected-window) tv:selected-window)
          (tv:notify () "GC: Turning GC off because oldspace exists during initialization.")
        (cerror :yes "Proceed" nil
                "Oldspace exists during GC initialization.~@
                 If you just warm-booted the machine after it crashed, this is understandable.~@
                 Otherwise it is a bug and you should report it before doing anything.~@
                 In any case, incremental garbage collection cannot be turned on."))
      (send *gc-process* :arrest-reason :gc-suspect))
    (if (not (memq :gc-stopped (send *gc-process* :arrest-reasons)))
        (gc-on)
        (progn
          (send *gc-process* :arrest-reason :gc-stopped)
          (send *gc-process* :reset)))
    ;; Clear meters whether GC is on or off.
    (clear-gc-meters)))

(defun make-pdl-areas-static ()
  ;; This is just a temporary kludge.  Currently, systems are waking up with the pdl areas
  ;; dynamic, but the last time I tried it, flipping a PDL area caused the machine to crash
  ;; for reasons that were not immediately clear.  This function is called by gc::initialize
  ;; to guarantee that the pdl areas become static.
  (without-interrupts
    (dolist (area (list sys:pdl-area sys:special-pdl-area))
      (setf (%area-type area) sys:%region-space-static)
      (setf (si::%area-scavenge-enable area) 1)   ;;must scavenge every region with pointers.
      (for-every-region-in-area (region area)
        (setf (%region-type region) sys:%region-space-static)))))

(defconst *gc-process-control* nil
  "This is only for internal system use.
The global value of this must be one of the following:
   :ACTIVE - GC process is active
   :SUSPEND - The GC process has been requested to suspend
              itself by the GC code
   :SUSPENDED - The GC process has suspended itself as the result
                of a :SUSPEND request
   NIL - The GC process hasn't been started up yet.")


(defun gc-on (&key degree)
  "Set the current configuration of the GC, and start it up if necessary.
The trade-off between the desire to reclaim as much garbage as possible and the desire not
  to let the GC interfere with the user is represented as an integer DEGREE between 0 and 3.
The degree 0 represents the minimal amount of garbage collection (other than turning it off),
  with little impact on performance.
The degree  3 is the \"safest\" degree; it tries to garbage collect everything in dynamic storage,
  without regard to performance.
Interactive users should specify 0, 1, or 2.
Large batch programs or server hosts should use 3.
If you do not specify DEGREE, the current value of GC::*GC-DEGREE* is used.
The initial value is 1."
  (check-type degree (or null (integer 0 3)))
  (if (memq :gc-suspect (send *gc-process* :arrest-reasons))
      ;;$$$ Issue a warning, not a printout! <11nov88 keith>
      (warn
        "~Not enabling automatic garbage collection: the GC has been forcibly disabled.~@
        (It was probably in an inconsistent state when the machine was last booted.)~@
        Save your files and reboot after rebuilding your LISP world!~")
      (progn
        (when degree (setq *gc-degree* degree))
        (configure *gc-degree*)
        (send *gc-process* :preset 'gc-process)
        (send *gc-process* :reset)
        (send *gc-process* :run-reason :enable)
        (send *gc-process* :revoke-arrest-reason :gc-stopped)
        (setq *gc-process-control* :active)
        t)))

(defun gc-off (&optional &key reset)
  (without-flipping
    (when ( *oldspace-words* 0)
      (tv:notify () "GC: Reclaiming oldspace before turning GC off.")
      (reclaim-oldspace :batch)))
  (when (and (boundp '*gc-process*) *gc-process*)
    (when (eq *gc-process-control* :active)
      (setq *gc-process-control* :suspend)
      (process-wait "Suspended GC" #'(lambda () (eq *gc-process-control* :suspended))))
    (without-interrupts
      (when reset
        (send *gc-process* :reset))
      (send *gc-process* :arrest-reason :gc-stopped)
      (send *gc-process* :revoke-run-reason :enable)))
  t)

(defun configure (degree)
  (array-initialize *level-control* ())
  (let ((physical-memory (aref #'sys:system-communication-area sys:%sys-com-memory-size)))
    ;; On machines with gargantuan primary memories, the flip interval gets too long.
    ;; The initial effect is that flips take a long time, but a possible effect is that
    ;; flips wouldn't happen often enough to prevent virtual memory overflow (doing a
    ;; test every time around the gc-process loop is pretty burdensome for these poor machines).
    (setq physical-memory (min physical-memory (^ 2 21.)))
    (ccase degree
      (0
       (setf (aref *level-control* 3) (truncate physical-memory 15.)))
      (1
       (setf (aref *level-control* 3) (truncate physical-memory 15.))
       (setf (aref *level-control* 2) (truncate physical-memory 6)))
      (2
       (setf (aref *level-control* 3) (truncate physical-memory 10.))
       (setf (aref *level-control* 2) (truncate physical-memory 6))
       (setf (aref *level-control* 1) (truncate physical-memory 2)))
      (3
       (setf (aref *level-control* 3) (truncate physical-memory 8))
       (setf (aref *level-control* 2) (truncate physical-memory 4))
       (setf (aref *level-control* 1) (truncate physical-memory 2))
       (setf (aref *level-control* 0) t)))
    degree))



;;;; WITHOUT-FLIPPING (and GC locking strategy in general):

;;; We need to be able to defer flipping on several occasions.  MAPHASH and related operations
;;; traverse hash-tables in a non-standard way that gets confused by rehashing.  There may be
;;; a need to defer flipping during certain critical sections, to maintain storage
;;; consistency.  There may also be a need to defer flipping during some response-time
;;; critical section.
;;;
;;; To implement a without-flipping lock under multiprocessing conditions, we use the tried
;;; and true counter method.  To inhibit flipping, *INHIBIT-FLIPPING-COUNT* is incremented.
;;; Flipping is deferred at the lowest level, %FLIP, until the count gets down to zero again.
;;; (If it doesn't happen soon enough, %FLIP starts moaning about not being allowed to flip.)
;;; For debugging purposes, it is desirable to be able to find those processes which have
;;; incremented *INHIBIT-FLIPPING-COUNT*.  We do this by lambda-binding the variable
;;; *THIS-PROCESS-IS-INHIBITING-FLIPPING* to T in such stack groups. Then SYMEVAL-IN-STACK-GROUP
;;; can be used to find them.
;;;
;;; The actual flip is done without-interrupts, so locking is pretty easy.  The primary
;;; concerns are:
;;;
;;;   [] Nobody can flip while oldspace exists, until it is reclaimed.
;;;
;;;   [] Nobody can flip while ( *inhibit-flipping-count* 0); they should wait for this
;;;      to become true, then flip.  If flipping is inhibited for an extended period of
;;;      time, the user should be notified that the GC is in trouble.
;;;
;;;   [] Nobody should flip on the basis of obsolete information (if you make a decision
;;;      to flip, and then someone else flips before you do, then your decision should be
;;;      re-evaluated).
;;;
;;;   [] We should allow interrupts as much as possible.  The one exception I make is
;;;      for %FLIP, in which the lengthy microcode setup time and the initial burst of
;;;      transporter activity effectively defer interrupts a lot more than the explicit
;;;      without-interrupts.
;;;
;;;   [] Just because somebody is doing WITHOUT-FLIPPING shouldn't prevent somebody else
;;;      from deciding whether or not they want to flip.  WITHOUT-FLIPPING should only
;;;      come into effect once they have decided that they do, indeed, want to flip.
;;;
;;; In view of all this, the following idiom is used:
;;;
;;;    (when (null *gc-status*)                               ;Don't do anything if GC busy.
;;;      (let ((generation %gc-generation-number))            ;Record for later comparison.
;;;        ... decide whether to flip ...
;;;        (when (... I want to flip ...)
;;;          (without-interrupts
;;;            ;; Somebody else might have flipped before we got into the without-interrupts,
;;;            ;; so we have to check again to make sure that a) the GC is idle, and b) nobody
;;;            ;; has flipped/scavenged/reclaimed, incrementing the generation number.
;;;            (if (and (null *gc-status*)                    ;GC is still idle.
;;;                     (= generation %gc-generation-number)  ;Our decision is current.
;;;                     (%flip ... arguments ...))            ;%FLIP returns T if it flips.
;;;                I flipped!
;;;              else somebody else flipped -- try again)))))
;;;
;;; Note the behavior of %FLIP.  %FLIP ensures that flipping is not inhibited, waiting
;;; (impatiently) if it is.  If %FLIP actually does condemn some storage (its arguments
;;; could specify that nothing needs to be flipped), it atomically sets all the right
;;; variables to communicate its action and returns T, otherwise it just returns NIL.
;;;>> Note that the above is not really adequate!!!
;;;>>  For example, you could decide that you have to flip level 2, and then another process
;;;>>  flips level 3 (incrementing %gc-generation-number) which would make the above code
;;;>>  fragment believe that its job had been done by somebody else.
;;;>>>>  I'm not saying the job has been done by somebody else.  I'm saying the other process
;;;>>>>  changed the state of the world so much that the current process should reevaluate
;;;>>>>  its decision.  Part of the motivation for this reevaluation is that levels are not
;;;>>>>  independent -- level 2 includes level 3 and so forth.  If %FLIP returns NIL, then
;;;>>>>  you basically haven't the slightest idea what happened, and you're supposed to
;;;>>>>  investigate the situation further if you care (some callers, such as stupid humans,
;;;>>>>  may not).
;;;>> Even watching for a change in the appropriate element of *gc-flip-generations* is
;;;>>  still not entirely good enough, as somebody may flip at your level, but not flip
;;;>>  the area(s) upon which your calculations are based.
;;;
;;; RECLAIM-OLDSPACE is a bit simpler.  You call it to scavenge incrementally or batch.  If
;;; incremental, you wait until the scavenge is done.  If batch, you scavenge until the
;;; scavenge is done.  Then you go without-interrupts, see if oldspace still exists (someone
;;; might have beat you to it), and, if it does, reclaim it.  The result of all this is that
;;; any number of processes may call RECLAIM-OLDSPACE at a time, with exactly the same
;;; results as a single call.  Like %FLIP, RECLAIM-OLDSPACE returns T if your invocation
;;; actually did some work, NIL if someone else did it first.  In any case, any particular
;;; invocation of RECLAIM-OLDSPACE will not return until oldspace is reclaimed, whether by
;;; it or by someone else.
;;;
;;; The end result of all this is that any process can call FLIP or RECLAIM-OLDSPACE at
;;; any time, and it will always do the right thing.

;;; without-flipping-internal and without-scavenging-internal are fset to 'funcall
;;; in the cold-load (in LTOP) so that expansions of gc:without-flipping/scavenging
;;; can run before this file is loaded.

(defvar stack-groups-inhibiting-flipping nil)

(defun without-flipping-internal (thunk)
  ;; However, one could still get in trouble if an abort happened between the start of the
  ;; unwind-protect body and the execution of the incf instruction. (in which case the
  ;; unwind-protect exit would over-decrement the counter)  So we have to do this...
  (let* ((old inhibit-scheduling-flag)
         (inhibit-scheduling-flag t)
         (*this-process-is-inhibiting-flipping* t))
    (unwind-protect
        (progn
         (push current-stack-group stack-groups-inhibiting-flipping)
          (incf *inhibit-flipping-count*)
          (setq inhibit-scheduling-flag old)
          (funcall thunk))
      (decf *inhibit-flipping-count*)
      (setq stack-groups-inhibiting-flipping
            (delq current-stack-group stack-groups-inhibiting-flipping)))))

;;; For old compiled version of without-scavenging -- not used by new compiled code.
;(defun without-scavenging-internal (thunk)
;  ;; The scavenger won't run if scheduling is inhibited.  Initially this was just because
;  ;; there appeared to be lots of critical sections that already used without-interrupts
;  ;; but seemed to want to preclude scavenging also.  This behavior was going to be temporary,
;  ;; but I think it's OK to promote it to a Feature, and perhaps even document it.
;  (without-interrupts
;    (funcall thunk)))



;;;; GC driver -- top level.

(defun gc-process ()
  ;; If the GC process gets some kind of error, the user will always get the option to restart
  ;; the process.  This loop is designed to be restartable -- no matter what state the GC is
  ;; in, it will do the right thing.  Obviously, if something wild and wierd happened in the
  ;; GC internals, there is little chance of winning.  There might be a proceed option to
  ;; turn the GC off, but most serious GC errors happen in user programs, so it wouldn't
  ;; help too much.
  (loop
    ;; Wake up every 4 seconds and take stock of the situation.  This is not ideal.
    ;; The ideal interface would be to have the scheduler occasionally run a fairly
    ;; lengthy wait function to see if a flip is needed.
    (process-sleep 240. "Await flip")
    ;;; Check *GC-PROCESS-CONTROL* for suspend request issued by a FULL-GC
    (when (eq *gc-process-control* :suspend)
      (setq *gc-process-control* :suspended)
      (process-wait "Suspended" 'ignore))
    ;; Don't even think about flipping unless the GC is idle.
    (when (null *gc-status*)
      ;; It is important that we be able to make this computation even if someone is inhibiting
      ;; flipping.  Only when we really decide to flip do we have to sync up with the rest of
      ;; the world.
      (loop with generation = %gc-generation-number
            with sdb = (compute-storage-distribution)
            with free = (aref sdb 5)
            for level from 0 to 3
            ;; Note: it is important to go from least-volatile to most-volatile as we decide
            ;; whether to flip each level.  We need to flip the least volatile level that
            ;; needs to be flipped -- otherwise we'll never get to the lower levels.
            for control = (aref *level-control* level)
         when (cond ((fixnump control)
                     ;; Control is a threshold, if the amount of storage in this and all
                     ;; higher levels exceeds the threshold, flip.  The higher levels are
                     ;; included because they will be flipped too.
                     ( (loop for l from level to 3 summing (aref sdb l)) control))
                    ((null control)
                     ;; If control is (), don't flip this level.
                     ;;>> Should warn if need to do a volatility 0 flip
                     ;;>>  that machine will eventually die unless action is taken soon.
                     ())
                    (t
                     ;; If control is t, flip this level when it needs it.
                     ( (committed-free-space level :incremental sdb) free)))
         do (when (without-interrupts
                    ;; Did anyone flip while we were making up our mind, above?
                    (and (= generation %gc-generation-number)
                         (%flip level)))
              ;; Only report if we actually flipped.
              (report "GC: Flipped ~:D words of volatility ~D storage" *oldspace-words* level))
         and return))
    (when (not (null *gc-status*))
      ;; Clean up after flip, whether caused by this process or any other.
      (reclaim-oldspace :incremental))))



;;;; GC space computations.

;;; [] Definitions:
;;;
;;;    D  dynamic space, meaning dynamic regions volatile enough to be flipped.
;;;    S  static space, meaning scavengeable space that is not flipped.  This includes
;;;        actual static regions plus dynamic regions of lower volatility.
;;;    N  new dynamic space allocated between now and flip.
;;;    W  scavenger work, 1 unit for each word of copy/static space looked at, plus
;;;        1 unit for each word copied.
;;;    K  4
;;;    C  words of consing required to complete scavenge.
;;;    F  free space.
;;;
;;; There is a certain amount of variation in this computation, due to storage fragmentation
;;; and unpredictible scavenging rates (from idle scavenging and the scavenge queue).  We
;;; introduce two fudge factors into the equations, one additive and one multiplicative,
;;; that increase the estimated free space required by a flip.
;;;
;;;    F0  0th order free space fudge factor.
;;;    F1  1st order free space fudge factor.
;;;
;;; [] Equations:
;;;
;;;    W = S + 2 (N+D)
;;;      = 1 unit for scanning each word of static space + one unit for scanning each word
;;;        word of copy space + one unit for copying each word into copy space.
;;;
;;;    C = (N+D) + W/K
;;;      = words required to copy transported objects +
;;;        words required to drive scavenger to completion.
;;;      = [(N+D) + W/k] * F1 + F0                               with fudge factors
;;;
;;;    F = N + C
;;;      = current free space will be divided up by space consed before flip and
;;;        space required by flip.
;;;
;;; [] Solutions: (incremental reclamation)
;;;
;;;    Free space required if we flip right now: (N=0)
;;;
;;;       C = D + (S+2*D) / K
;;;
;;;         = [D + (S+2*D) / K] * F1 + F0                        with fudge factors
;;;
;;;    Amount of consing allowed before we must flip:
;;;
;;;              S + K*(D-F) + 2*D
;;;       N = - -------------------
;;;                  2*K + 2
;;;
;;;             F1*S + K*(D*F1+F0-F) + 2*D*F1
;;;         = - -----------------------------                    with fudge factors
;;;                   2*F1 + K*(F1+1)
;;;
;;; [] Solutions: (batch reclamation)
;;;
;;;    Free space required if we flip right now: (N=0)
;;;
;;;       C = D
;;;
;;;         = D * F1 + F0                                        with fudge factors
;;;
;;;    Amount of consing allowed before we must flip:
;;;
;;;       N = (F - D) / 2
;;;
;;;              D*F1 + F0 - F
;;;       N = - ---------------                                  with fudge factors
;;;                 F1 + 1
;;;
;;; [] Confessions:
;;;
;;;    I used Macsyma to solve for N.  Better safe than studly.
;;;
;;; [] Differences between this and the previous computation:
;;;
;;;    There is very little absolutely static space in the system, so we assume all new
;;;    storage consed during scavenging goes into newspace, which is not scavenged.  This
;;;    is a slightly optimistic assumption.  (this has been promoted to a Problem, below)
;;;
;;;    Not all dynamic space gets flipped, only the volatile portion.  Dynamic space that
;;;    is not flipped is considered static (for the duration of the flip) and must be
;;;    scavenged.
;;;
;;;    All decisions regarding flipping are made on the basis of the current committed
;;;    free space.  The ability to predict when a flip may happen is for user enlightenment
;;;    only.
;;;
;;;    We always assume that all storage will survive a flip.  As the computations get
;;;    more critical (for long low-volatility flips), this gets closer to true, since
;;;    objects there have already survived several flips.  I have no problems with this.
;;;
;;;    "Free space" is defined as the amount of storage not assigned to any region.  Storage
;;;    assigned to regions but not yet used is theoretically free, but may not be usable.
;;;    (quantums at the end of regions to be condemned will be deallocated and could safely
;;;    be regarded as free).
;;;
;;; [] Problems:
;;;
;;;    New storage allocated directly in lower-volatility regions (due to low-volatility
;;;    areas) is needlessly scavenged.  This is not represented in the equations, and I
;;;    don't think it should be.  The right way to deal with this is not to scavenge those
;;;    portions of "static" space that were allocated after a flip, using a per-region
;;;    table to indicate to the scavenger where to stop.  (This would be pretty easy for
;;;    the scavenger, since the part that deals with "am I done with this region yet"
;;;    is isolated.)

;;; Fudge factors for storage fragmentation, variable scavenging rates, and safety margin.
;;; Note that the localizer is a huge first-order fudge factor in itself -- it effectively
;;; scavenges all of copyspace, and the scavenger really only scavenges static space.
;;; Perhaps there is some reasonable way to determine these empirically?
(defvar *0th-order-free-space-fudge* 262144)    ;Additive (16 quantums).
(defvar *1st-order-free-space-fudge* 102/100)   ;Multiplicative (two percent).

;;; Free space consists of 1) space not allocated to any region, and 2) space allocated
;;; to regions but not yet used.  Unallocated space is definitely useful, but unused space
;;; may not be useful due to region fragmentation.  For the GC computations, "free space"
;;; is defined as the amount of unallocated space plus a fraction of the amount of unused
;;; space.  This fraction is *UNUSED-SPACE-FREE-FRACTION*, which is initially set to 50%.
(defvar *unused-space-free-fraction* 50/100)

(defun committed-free-space (level mode sdb)
  (let ((dynamic (loop for l from level to 3 summing (aref sdb l)))
        (static (+ (aref sdb 4)
                   (loop for l from 0 below level summing (aref sdb l))))
        (free (aref sdb 5))
        (f0 *0th-order-free-space-fudge*)
        (f1 *1st-order-free-space-fudge*))
    (declare (ignore free))
    (macrolet ((i* (x y) `(round (* ,x ,y))))
      (case mode
        (:incremental
         (+ (i* (+ dynamic (truncate (+ static dynamic dynamic) 4)) f1) f0))
        (t ;:batch
         (+ (i* dynamic f1) f0))))))

(defun distance-until-flip (level mode sdb)
  (let ((dynamic (loop for l from level to 3 summing (aref sdb l)))
        (static (+ (aref sdb 4)
                   (loop for l from 0 below level summing (aref sdb l))))
        (free (aref sdb 5))
        (f0 *0th-order-free-space-fudge*)
        (f1 *1st-order-free-space-fudge*))
    (macrolet ((i* (x y) `(round (* ,x ,y))))
      (case mode
        (:incremental
         (- (truncate
              (+ (* static f1)
                 (* 4 (+ (* dynamic f1) f0 (- free)))
                 (* 2 dynamic f1))
              (+ f1
                 f1
                 (* 4 (+ f1 1))))))
        (t ;:batch
         (- (truncate
              (+ (i* dynamic f1) f0 (- free))
              (+ f1 1))))))))

(defun compute-storage-distribution (&optional areas)
  ;; This returns a thing called a storage-distribution-block, which is a 7-element
  ;; array.  Elements 0-3 are the number of words in dynamic spaces of each volatility.
  ;; Element 4 is the number of words in scavengeable static space.  Element 5 is
  ;; the number of words free (not assigned to any region).  Element 6 is the number of
  ;; words in oldspace.  This needs to be fast, and is moderately bummed.  Be careful.
  ;; This won't return the exactly correct answer unless it's run uninterruptably (which
  ;; I don't want to do since it takes about 100 milliseconds).  The maximum error is
  ;; pretty small (the amount of consing other processes might do in that 100 milliseconds).
  (loop with sdb = (make-array 7 :initial-element 0)
        ;; It would be useful to teach the compiler to recognized and open-code system
        ;; constants.  Actually, the more desireable facility would recognize loop-invariant
        ;; special references and move them to local variables.
        with new-space = %region-space-new
        with copy-space = %region-space-copy
        with old-space = %region-space-old
        with static-space = %region-space-static
        with free-space = %region-space-free
        with %%type = %%region-space-type
        with %%volatility = %%region-volatility
        with %%scavenge-enable = %%region-scavenge-enable
        with region-free-pointer-base = (%region-origin region-free-pointer)
        with region-bits-base = (%region-origin region-bits)
        with regions = sys:number-of-regions
        initially (setf (aref sdb 5) (round
                                       (+ (si::unallocated-space)
                                          (* (si::unused-space) *unused-space-free-fraction*))))
        for region from 0 below regions
        for free-pointer = (%p-pointer (+ region-free-pointer-base region))
        for bits = (%p-pointer (+ region-bits-base region))
        for type = (ldb %%type bits)
        do (selector type eq                    ;Faster than SELECT (which uses EQL).
             (free-space
              ;; Free regions don't count for anything.  They don't even have a size.
              ())
             ((new-space copy-space)
              ;; If a new or copy region is in one of the specified areas, then it's part
              ;; of dynamic space; otherwise it won't be flipped, but must be scavenged, and
              ;; is therefore static.
              (if (or (null areas) (memq (%region-area region) areas))
                  (incf (aref sdb (ldb %%volatility bits)) free-pointer)
                (incf (aref sdb 4) free-pointer)))
             ((static-space %region-space-moby-fixed %region-space-moby-new)
              ;; If the scavenge-enable is on, then count this as static space.  Otherwise,
              ;; it's unstructured or something and won't be scavenged, and doesn't count at all.
              (if (not (zerop (ldb %%scavenge-enable bits)))
                  (incf (aref sdb 4) free-pointer)))
             (old-space
              ;; Oldspace regions always contribute to oldspace.
              (incf (aref sdb 6) free-pointer)))
        finally (return (setq *most-recent-storage-distribution-block* sdb))))

(defvar *most-recent-storage-distribution-block* (compute-storage-distribution)
  "Last thing returned from GC:COMPUTE-STORAGE-DISTRIBUTION.
This doesn't even pretend to be accurate.  On the other hand, if you don't want
to spend the time to get a correct result, this may be for you.")

;;; %FLIP and support functions.

(defun %flip (volatility &optional areas)
  "Flipping primitive.  Flips all regions that have volatility greater or equal
to VOLATILITY and belong to an area in AREAS.  AREAS must be a list of area numbers,
and defaults to all areas (nil).  Returns T if it actually flips, else NIL."
  (cond ((null inhibit-scheduling-flag)
         (ferror "This function must be called with scheduling inhibited."))
        (*this-process-is-inhibiting-flipping*
         (ferror "Attempt to flip inside a ~S form" 'without-flipping))
        (( *inhibit-flipping-count* 0)
         (wait-until-flipping-is-allowed volatility areas)))
  ;; It's possible that the process-waits above could have allowed somebody else
  ;; to flip while we were waiting for the inhibit to subside.
  (when (null *gc-status*)
    ;; Set up region attributes (flip enables, scavenge enables).  This is moderately
    ;; time-critical -- we don't want to make flipping any slower than it already is.
    ;; (I think the %gc-flip microcode is actually the villain here -- it doesn't hack the PHT
    ;; and the maps as efficiently as it might.)
    (with-quick-region-area-accessors
      (let ((new-space sys:%region-space-new)
            (copy-space sys:%region-space-copy)
            (%%type sys:%%region-space-type)
            (%%volatility sys:%%region-volatility)
            (%%flip-enable sys:%%region-flip-enable)
            (%%scavenge-enable sys:%%region-scavenge-enable)
            (quantum-size sys:%address-space-quantum-size)
            (type)
            (bits))
        (for-every-region (region)
          (setq type (%logldb %%type (setq bits (%region-bits region))))
          (when (or (eq type new-space) (eq type copy-space))
            ;; Now we know it's a dynamic region.  Should we flip it?
            (if (and ( (%logldb %%volatility bits) volatility)
                     (or (null areas) (memq (%region-area region) areas)))
                (let ((free-pointer (%region-free-pointer region)))
                  (setq bits (%logdpb 1 %%flip-enable (%logdpb 0 %%scavenge-enable bits)))
                  (incf *oldspace-words* free-pointer)
                  (when (eq type new-space)
                    (incf *condemned-newspace-words* free-pointer))
                  (unless (eq (%region-length region) quantum-size)
                    (%deallocate-end-of-region region)))
              ;; Every dynamic region that won't be flipped must be scavenged. (This is a
              ;; very important distinction between this GC and the previous one.)
              ;; (when (and (eq type new-space) (eq type static-space))
              ;;    ... don't flip this because it's static ...) LOSES.
              ;; Look at MAKE-AREA and the cold-load builder if you don't think static
              ;; areas will get scavenged.
              (setq bits (%logdpb 0 %%flip-enable (%logdpb 1 %%scavenge-enable bits))))
            ;; Store back the modified region bits.
            (setf (%region-bits region) bits)))))
    ;; Reset all scavenge pointers to zero, quickly.
    (let ((address (%region-origin sys:region-gc-pointer)))
      (%p-store-contents address 0)
      (%blt address (1+ address) (1- sys:number-of-regions) 1))
    ;; All the above work might have determined that there was no storage that needs to be
    ;; flipped, in which case we don't flip.
    (when ( *oldspace-words* 0)
      ;; Communicate the volatility of this flip to everyone who needs to know.
      (setq %gc-switches (setq *gc-status* (setq *flip-volatility* volatility)))
      (setq *flip-start-total-time* (%microsecond-time))
      (setq *flip-start-disk-time* (read-meter %disk-wait-time))
      (%gc-flip (if *all-flips-to-higher-address-space*
                    (without-interrupts (si::find-max-addr))
                  0))
      (incf %gc-generation-number)
      (loop for level from volatility to 3
            do (setf (aref *gc-flip-generations* level) %gc-generation-number))
      (incf (aref *flip-count* volatility))
      (setq inhibit-scavenging-flag ())
      t)))

(defvar *inhibited-flip-sleep-time* 30.)
(defvar *inhibited-flip-complain-threshold* 30.)
(defvar *inhibited-flip-complain-threshold-multiplier* 1.5)
(defvar *error-if-unable-to-determine-process-inhibiting-flipping* t)

(defflavor flip-inhibited-by-dead-stack-groups (how-many-dead-stack-groups volatility areas
                                                wait-time)
           (error)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (flip-inhibited-by-dead-stack-groups :report) (stream)
  (format stream "~S is not zero,
but no processes have ~S set to ~S!
This can be caused by killing a window or a process which was
in an error loop, or by running a co-routine with a ~S form.
In the former case, it is usually safe to set ~S to zero.  If the
process which was inhibiting flipping ever comes back, this course of
action will probably crash the machine.
You have several options:
1) <safest> Go to ZWEI, save your files, and reboot the machine.
2) Stop the garbage collector.
3) Find (by hand) the process that was inhibiting flipping and do
   something about it.
4) If you have just killed a process or a window (in the last ~\\time-interval\\ or so)
   then setting ~S to zero should win.
5) If you think a process just might be inhibiting flipping for a longer
   amount of time than the gc thinks is reasonable, you can increase the
   ~S.
6) <most dangerous>
   If you really think you know what is going on, and you don't want to
   bash the process inhibiting flipping, do a flip anyway."
          '*inhibit-flipping-count*
          '*this-process-is-inhibiting-flipping*
          't
          'without-flipping
          '*inhibit-flipping-count*
          wait-time
          '*inhibit-flipping-count*
          '*inhibited-flip-complain-threshold*))

(defmethod (flip-inhibited-by-dead-stack-groups :case :proceed-asking-user :decrement-inhibit-flipping-count)
           (continuation ignore)
  (decf *inhibit-flipping-count* how-many-dead-stack-groups)
  (if (minusp *inhibit-flipping-count*)
      (setq *inhibit-flipping-count* 0))
  (funcall continuation))

(defmethod (flip-inhibited-by-dead-stack-groups :case :document-proceed-type :decrement-inhibit-flipping-count)
           (stream &optional ignore)
  (format stream "Decrement ~S by ~S and hope that dead stack groups are really dead."
          '*inhibit-flipping-count* how-many-dead-stack-groups))

(defmethod (flip-inhibited-by-dead-stack-groups :case :proceed-asking-user :punt)
           (continuation ignore)
  (funcall continuation))

(defmethod (flip-inhibited-by-dead-stack-groups :case :document-proceed-type :punt)
           (stream &optional ignore)
  (format stream "Proceed in the hopes that the error will go away by itself."))

(defmethod (flip-inhibited-by-dead-stack-groups :case :proceed-asking-user :increase-complain-time)
           (continuation ignore)
  (setq *inhibited-flip-complain-threshold*
        (* *inhibited-flip-complain-threshold-multiplier* *inhibited-flip-complain-threshold*))
  (funcall continuation))

(defmethod (flip-inhibited-by-dead-stack-groups :case :document-proceed-type :increase-complain-time)
           (stream &optional ignore)
  (let ((increase (round (* *inhibited-flip-complain-threshold-multiplier* *inhibited-flip-complain-threshold*))))
    (format stream "Increase ~S to ~\\time-interval\\." '*inhibited-flip-complain-threshold* increase)))

(defmethod (flip-inhibited-by-dead-stack-groups :case :proceed-asking-user :flip-anyway)
           (continuation ignore)
  (without-interrupts
    (let ((*inhibit-flipping-count* 0)
          (*this-process-is-inhibiting-flipping* nil))
      (%flip volatility areas)))
  (funcall continuation))

(defmethod (flip-inhibited-by-dead-stack-groups :case :document-proceed-type :flip-anyway)
           (stream &optional ignore)
  (format stream "Allow a flip even though a without flipping form is active."))

(defun wait-until-flipping-is-allowed (volatility areas)
  (do ((wait-time *inhibited-flip-sleep-time* (+ wait-time *inhibited-flip-sleep-time*)))
      ((process-wait-with-timeout
         "Flip inhibited" (* 60. *inhibited-flip-sleep-time*)
         #'(lambda () (zerop *inhibit-flipping-count*))))
    (when (> wait-time *inhibited-flip-complain-threshold*)
      (find-which-processes-are-inhibiting-flipping
        #'(lambda (process-list how-many)
            (if (and (not (= how-many *inhibit-flipping-count*))
                     *error-if-unable-to-determine-process-inhibiting-flipping*)
                (progn
                  (signal 'flip-inhibited-by-dead-stack-groups
                          :how-many-dead-stack-groups (- *inhibit-flipping-count* how-many)
                          :volatility volatility
                          :areas areas
                          :wait-time wait-time)
                  (si::process-flush-background-stream))
                (process-run-function
                  "GC notify" #'tv:notify ()
                  ;; Know your format directives! (Missing ~< and ~?  Oh well)
                  "GC: Flip inhibited for ~\\time-interval\\~%       ~
                  by ~~:{process \"~A\" which is ~:[runn~;wait~:*~]ing in state \"~A\"~:^,~%~}~"
                  wait-time
                  process-list
                  (loop for p in process-list
                        collect (list (si::process-name p)
                                      (si::process-wait-whostate p)
                                      (si::process-run-whostate p))))))))))

(defun find-which-processes-are-inhibiting-flipping (receiver)
  (let ((processes '())
        (how-many 0))
  (without-interrupts
    (dolist (active-process active-processes)
      (let ((process (car active-process)))
      (when (and process
                 (not (si::process-simple-p process))
                 (symeval-in-stack-group '*this-process-is-inhibiting-flipping*
                                         (process-stack-group process))
                 (symeval-in-stack-group '*this-process-is-inhibiting-flipping*
                                         (process-initial-stack-group process)))
        (push process processes)
        (incf how-many)))))
  (funcall receiver processes how-many)))

;;; RECLAIM-OLDSPACE and support functions.

(defvar *after-scavenge-daemons* '()
  "Initialization which are run after the scavenge but before the reclaim.")

(defun reclaim-oldspace (&optional (reclaim-mode :batch))
  "Finish scavenging all of oldspace right away, then discard it.  RECLAIM-MODE is
either :BATCH or :INCREMENTAL."
  (check-type reclaim-mode (member :batch :incremental))
  (if (not (zerop *oldspace-words*))
      (without-flipping
        (case reclaim-mode
          (:incremental
           (process-wait "Incremental scavenge" (lambda () sys:%gc-flip-ready)))
          (t ;:batch
           (scavenge nil)))
        ;; When we get to here, we know that the scavenging has completed one way or another.
        ;; We also know that oldspace exists, since it existed before and we're in a
        ;; without-flipping.  However, there may be several active invocations of
        ;; reclaim-oldspace -- only one may do the actual reclamation.  The first one to get
        ;; inside the without-interrupts gets to do the work.  Only the invocation that
        ;; actually did the reclaim, as determined by the return values of %reclaim-oldspace,
        ;; makes the report.
        (multiple-value-bind (reclaimed condemned)
            (without-interrupts
              (%reclaim-oldspace))
          (when (not (null reclaimed))
            (report "GC: Reclaimed ~:D words of storage (~D%)"
                    reclaimed
                    (round (/ reclaimed condemned 0.01)))
            reclaimed)))))

(defun scavenge (units)
  "If UNITS is a fixnum, do that many units of scavenger work.  If UNITS is NIL, scavenge
to completion.  If idle-scavenge-p is T, scavenging will be suspended after a page fault."
  (etypecase units
    (fixnum
     (compiler:%better-gc-scavenge () units))
    (null
     ;; Note: somebody else might be doing a :batch reclamation, in which there will
     ;; now be two processes scavenging, which is less efficient.  However, giving
     ;; one of the processes sole responsibility for scavenging is a bad idea,
     ;; because it might be interrupted (and shouldn't be uninterruptible).  So,
     ;; we accept that in this exceedingly rare situation, scavenging efficiency will
     ;; be slightly less than optimal.
     ;; Increment *BATCH-RECLAIM-COUNT* to let everyone know what's going on.
     (let* ((old inhibit-scheduling-flag)
            (inhibit-scheduling-flag t))
       (unwind-protect
           (progn
             (incf *batch-reclaim-count*)
             (setq inhibit-scheduling-flag old)
             (loop until sys:%gc-flip-ready
                   do (compiler:%better-gc-scavenge () 1024)))
         (decf *batch-reclaim-count*))))))

;; called from the scheduler.  Punt if take a page fault
(defun idle-scavenge (units)
  (check-type units fixnum)
  (compiler::%better-gc-scavenge t units))

(defun %reclaim-oldspace ()
  "Reclaim all extant regions of oldspace, if no other process has already done it.  If,
in fact, this invocation does the reclaiming, then this function records the gc meters
and returns the amount of storage condemned and reclaimed by the garbage collection,
otherwise it does nothing and returns nil.  In either case, there can be no oldspace left
after calling this function."
  (declare (values reclaimed condemned))
  (when (null inhibit-scheduling-flag)
    (ferror "This function must be called with scheduling inhibited."))
  (when (not (null *gc-status*))
    (mapc #'funcall *after-scavenge-daemons*)
    (let ((old-space sys:%region-space-old)
          (%%type sys:%%region-space-type))
      (with-quick-region-area-accessors
        (for-every-region (region)
          (when (eq (%logldb %%type (%region-bits region)) old-space)
            ;; Remove oldspace region from its area, and %gc-free-region it.
            (let* ((area (%region-area region))
                   (first (%area-region-list area)))
              ;; If it's the first region in the area, delete from the start of the thread.
              (if (eq region first)
                  (setf (%area-region-list area) (%region-list-thread region))
                ;; Otherwise search for the region and snap it out of the thread.
                ;; Both LOOP and DO generate disgusting code for this loop.  Sorry.
                (prog (this (next first))
                 loop (setq this next)
                      (setq next (%region-list-thread this))
                      (if (not (eq next region)) (go loop))
                 snap (setf (%region-list-thread this) (%region-list-thread next)))))
            (%gc-free-region region)))))
    ;; These two parameters have to be updated atomically with the above operation.
    (setq *oldspace-words* 0)
    (setq *gc-status* ())
    (setq inhibit-scavenging-flag t)
    (values (- *flip-words-condemned* *flip-words-transported*) *flip-words-condemned*)))



;;; GC Metering.

;;; The time spent in the scavenge-queue is included implicitly in the transporter and
;;; scavenger meters, though not explicitly accessible.  It is thought to be fairly evenly
;;; distributed between the scavenger and transporter, and to consume approximately 10% of
;;; those times.  The microcode meters are in microseconds.  Note that Explorers and early
;;; Lambdas don't have usable microsecond clocks, so the meters are meaningless.

(defvar *transporter-total-time* #(0 0 0 0))
(defvar *transporter-disk-time* #(0 0 0 0))
(defvar *scavenger-total-time* #(0 0 0 0))
(defvar *scavenger-disk-time* #(0 0 0 0))
(defvar *ambient-total-time* #(0 0 0 0))
(defvar *ambient-disk-time* #(0 0 0 0))
(defvar *words-transported* #(0 0 0 0))
(defvar *words-condemned* #(0 0 0 0))
(defvar *flip-count* #(0 0 0 0))

;;; Temporaries used to compute above figures.
(defvar *flip-start-total-time* 0)
(defvar *flip-start-disk-time* 0)
(defvar *flip-volatility* 0)
(defvar *flip-transporter-total-time* 0)
(defvar *flip-transporter-disk-time* 0)
(defvar *flip-scavenger-total-time* 0)
(defvar *flip-scavenger-disk-time* 0)
(defvar *flip-ambient-total-time* 0)
(defvar *flip-ambient-disk-time* 0)
(defvar *flip-words-transported* 0)
(defvar *flip-words-condemned* 0)

(defun clear-gc-meters ()
  (array-initialize *transporter-total-time* 0)
  (array-initialize *transporter-disk-time* 0)
  (array-initialize *scavenger-total-time* 0)
  (array-initialize *scavenger-disk-time* 0)
  (array-initialize *ambient-total-time* 0)
  (array-initialize *ambient-disk-time* 0)
  (array-initialize *words-transported* 0)
  (array-initialize *words-condemned* 0)
  (array-initialize *flip-count* 0))

(defun record-gc-meters ()
  (let ((volatility *flip-volatility*))
    (incf (aref *transporter-total-time* volatility)
          (setq *flip-transporter-total-time* (clear-meter sys:%transporter-time)))
    (incf (aref *transporter-disk-time* volatility)
          (setq *flip-transporter-disk-time* (clear-meter sys:%transporter-disk-time)))
    (incf (aref *scavenger-total-time* volatility)
          (setq *flip-scavenger-total-time* (clear-meter sys:%scavenger-time)))
    (incf (aref *scavenger-disk-time* volatility)
          (setq *flip-scavenger-disk-time* (clear-meter sys:%scavenger-disk-time)))
    (incf (aref *ambient-total-time* volatility)
          (setq *flip-ambient-total-time*
                (%32-bit-difference (%microsecond-time) *flip-start-total-time*)))
    (incf (aref *ambient-disk-time* volatility)
          (setq *flip-ambient-disk-time*
                (%32-bit-difference (read-meter sys:%disk-wait-time) *flip-start-disk-time*)))
    (incf (aref *words-transported* volatility)
          (setq *flip-words-transported* (clear-meter sys:%transporter-words-copied)))
    (incf (aref *words-condemned* volatility)
          (setq *flip-words-condemned* *oldspace-words*))))

(pushnew 'record-gc-meters *after-scavenge-daemons*)

(defun words-allocated ()
  (+ *condemned-newspace-words*
     (loop for region from 0 below sys:number-of-regions
           when (or (= (%region-type region) sys:%region-space-new)
                    (= (%region-type region) sys:%region-space-static))
           sum (%region-free-pointer region))))

(defun words-reclaimed (&optional volatility)
  (if (typep volatility 'volatility)
      (- (aref *words-condemned* volatility)
         (aref *words-transported* volatility))
    (- (vector-sum *words-condemned*) (vector-sum *words-transported*))))

(defun words-condemned (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *words-condemned* volatility)
         (if (neq *gc-status* volatility) 0 *oldspace-words*))
    (+ (vector-sum *words-condemned*) *oldspace-words*)))

(defun transporter-total-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *transporter-total-time* volatility)
         (if (neq *gc-status* volatility) 0 (read-meter sys:%transporter-time)))
    (+ (vector-sum *transporter-total-time*) (read-meter sys:%transporter-time))))

(defun transporter-disk-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *transporter-disk-time* volatility)
         (if (neq *gc-status* volatility) 0 (read-meter sys:%transporter-disk-time)))
    (+ (vector-sum *transporter-disk-time*) (read-meter sys:%transporter-disk-time))))

(defun transporter-cpu-time (&optional volatility)
  (- (transporter-total-time volatility) (transporter-disk-time volatility)))

(defun scavenger-total-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *scavenger-total-time* volatility)
         (if (neq *gc-status* volatility) 0 (read-meter sys:%scavenger-time)))
    (+ (vector-sum *scavenger-total-time*) (read-meter sys:%scavenger-time))))

(defun scavenger-disk-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *scavenger-disk-time* volatility)
         (if (neq *gc-status* volatility) 0 (read-meter sys:%scavenger-disk-time)))
    (+ (vector-sum *scavenger-disk-time*) (read-meter sys:%scavenger-disk-time))))

(defun scavenger-cpu-time (&optional volatility)
  (- (scavenger-total-time volatility) (scavenger-disk-time volatility)))

(defun ambient-total-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *ambient-total-time* volatility)
         (if (eq *gc-status* volatility)
             (%32-bit-difference (%microsecond-time) *flip-start-total-time*)
           0))
    (+ (vector-sum *ambient-total-time*)
       (if (not (null *gc-status*))
           (%32-bit-difference (%microsecond-time) *flip-start-total-time*)
         0))))

(defun ambient-disk-time (&optional volatility)
  (if (typep volatility 'volatility)
      (+ (aref *ambient-disk-time* volatility)
         (if (eq *gc-status* volatility)
             (%32-bit-difference (%microsecond-time) *flip-start-disk-time*)
           0))
    (+ (vector-sum *ambient-disk-time*)
       (if (not (null *gc-status*))
           (%32-bit-difference (%microsecond-time) *flip-start-disk-time*)
         0))))

(defun ambient-cpu-time (&optional volatility)
  (- (ambient-total-time volatility) (ambient-disk-time volatility)))

(defun vector-sum (vector)
  (loop for i from 0 below (array-active-length vector) summing (aref vector i)))

(defun percentage (part whole)
  (if ( whole 0) (quotient (float part) (float whole) 0.01) 0))

(defun microseconds-since-cold-boot ()
  (* 1000000 (- (get-universal-time) time::*ut-at-boot-time*)))



;;;; Status routines.

(defun print-statistics (&optional (stream *standard-output*))
  "Prints internal statistics about the operation of the garbage collector.
Most of the statistics are kept on a per-level basis, so both the per-level
values and the total value for all levels are printed out.  Some of the
non-intuitive terms are explained below.

RECLAMATION RATIO: the percentage of condemned storage made available for reuse.
COST PER WORD RECLAIMED: microseconds of processor time spent to make one word
   of storage available for reuse.  There are many variables in this computation,
   and indeed the basic algorithm requires time proportional to the number of words
   NOT reclaimed, but it does give a rough idea of overall GC efficiency.
ACTIVE PROCESSOR UTILIZATION: the percentage of processor time used by the
   garbage collector while there were condemned spaces in existance.
TOTAL PROCESSOR UTILIZATION: the percentage of processor time used by the
   garbage collector since cold boot.

Statistics aren't recorded on Explorers, because I'm not willing to write the
microcode to support the crazy BCD microsecond clock.  Statistics also don't
necessarily mean much on a machine that has been warm-booted."
  ;; These printing functions each take a funarg which they call with all
  ;; volatility numbers (and nil, meaning "all volatilities"), and produce
  ;; nice, formatted output.
  (flet ((fprint (string function)
           (format stream "~%~A~32T~,1F~44T~,1F~56T~,1F~68T~,1F~80T~,1F"
                   string
                   (funcall function ())
                   (funcall function 0)
                   (funcall function 1)
                   (funcall function 2)
                   (funcall function 3)))
         (iprint (string function)
           (format stream "~%~A~32T~:D~44T~:D~56T~:D~68T~:D~80T~:D"
                   string
                   (funcall function ())
                   (funcall function 0)
                   (funcall function 1)
                   (funcall function 2)
                   (funcall function 3)))
         (pprint (string function)
           (format stream "~%~A~32T~,1F%~44T~,1F%~56T~,1F%~68T~,1F%~80T~,1F%"
                   string
                   (funcall function ())
                   (funcall function 0)
                   (funcall function 1)
                   (funcall function 2)
                   (funcall function 3)))
         (seconds (x)
           (quotient x 1000000.0)))
    (select-processor
      (:cadr
        (ferror "This garbage collector doesn't work on CADRs."))
      (:explorer
        (format stream "~%GC statistics involving time are not recorded on Explorer processors.")
        (format stream "~%~32TTotal:~44TLevel 0:~56TLevel 1:~68TLevel 2:~80TLevel 3:")
        (iprint "Flips:" (lambda (v) (if (null v)
                                         (vector-sum *flip-count*)
                                       (aref *flip-count* v))))
        (iprint "Words condemned:" #'words-condemned)
        (iprint "Words reclaimed:" #'words-reclaimed)
        (pprint "Reclamation ratio:"
                (lambda (v) (percentage (words-reclaimed v) (words-condemned v))))
        )
      (:lambda
        (unless ( (aref *transporter-total-time* 3) (aref *transporter-disk-time* 3))
          (ferror "~The GC statistics are inconsistent, because the microsecond clock in~@
                     this processor is broken.  Tell LMI customer service to replace the~@
                     statistics counters on the RG board.~"))
        (format stream "~%~32TTotal:~44TLevel 0:~56TLevel 1:~68TLevel 2:~80TLevel 3:")
        (iprint "Flips:" (lambda (v) (if (null v)
                                         (vector-sum *flip-count*)
                                       (aref *flip-count* v))))
        (iprint "Words condemned:" #'words-condemned)
        (iprint "Words reclaimed:" #'words-reclaimed)
        (pprint "Reclamation ratio:"
                (lambda (v) (percentage (words-reclaimed v) (words-condemned v))))
        (fprint "Transporter CPU time:" (lambda (v) (seconds (transporter-cpu-time v))))
        (fprint "Transporter disk time:" (lambda (v) (seconds (transporter-disk-time v))))
        (fprint "Scavenger CPU time:" (lambda (v) (seconds (scavenger-cpu-time v))))
        (fprint "Scavenger disk time:" (lambda (v) (seconds (scavenger-disk-time v))))
        (fprint "Cost per word reclaimed:"
                (lambda (v)
                  (let ((reclaimed (words-reclaimed v)))
                    (if ( reclaimed 0)
                        (quotient (+ (transporter-total-time v) (scavenger-total-time v))
                                  (float reclaimed))))))
        (pprint "Active processor utilization:"
                (lambda (v)
                  (percentage (+ (transporter-total-time v) (scavenger-total-time v))
                              (ambient-total-time v))))
        (pprint "Total processor utilization:"
                (lambda (v)
                  (percentage (+ (transporter-total-time v) (scavenger-total-time v))
                              (microseconds-since-cold-boot)))))))
  (values))

(defun status (&optional (stream *standard-output*))
  (flet ((out (string &rest args) (apply #'format stream string args)))
    (let ((sdb (compute-storage-distribution))
          (on? (not (memq :gc-stopped (send *gc-process* :arrest-reasons))))
          (*print-base* 10.))

      ;; If the GC ever got in trouble, abuse the silly human and exit.
      (when (memq :gc-suspect (send *gc-process* :arrest-reasons))
        (out "~%The garbage collector has been forcibly disabled because it was in an~@
               inconsistent state when the machine was last booted.  Save your files and reboot!")
        (return-from status (values)))

      (time::print-uptime stream)
      (terpri stream)

      ;; General status of the GC -- on/off, active/inactive.
      (cond ((not on?)
             (if (null *gc-status*)
                 (out "~%The automatic garbage collector is disabled.~%")
               (out "~%The automatic garbage collector is disabled, but someone has recently~@
                        condemned ~:D words of volatility ~D storage.~%"
                    *oldspace-words*
                    *gc-status*)))
            ((null *gc-status*)
             (out "~%The automatic garbage collector is enabled.~%"))
            (t
             (out "~%The automatic garbage collector is enabled,~@
                        and has recently condemned ~:D words of volatility ~D storage.~%"
                  *oldspace-words*
                  *gc-status*)))

      ;; General information about the distribution of storage.  I like this format.
      (out "~%There are ~:D words of dynamic space, ~:D words of static space, and~@
                ~:D words of free space.  Free space consists of ~:D words of unallocated~@
                storage plus ~D/~D of the ~:D words of storage already allocated to specific~@
                regions but not yet used.~%"
           (loop for l from 0 to 3 summing (aref sdb l))
           (aref sdb 4)
           (aref sdb 5)
           (si::unallocated-space)
           ;; We do this in case of losing zetalisp readtable
           (numerator *unused-space-free-fraction*) (denominator *unused-space-free-fraction*)
           (si::unused-space))

      ;; Describe each dynamic level.  I don't like this format.
      (out "~%Dynamic space storage distribution:~%")
      (out "~%~6@TLevel      Current Size     Threshold      Mode")
      (loop for level from 3 downto 0
            for control = (aref *level-control* level)
            for size = (loop for l from level to 3 summing (aref sdb l))
         do (out "~%~8@T~D          ~10:D~@[~ (+ ~:D condemned words)~%~~]    ~10:D      ~A"
                 level
                 (aref sdb level)
                 (and (eq *gc-status* level) *oldspace-words*)
                 (typecase control
                   (fixnum control)
                   (null
                    (if (< (distance-until-flip level :batch sdb) 0) "Passed" "None"))
                   (otherwise
                    (let ((distance (distance-until-flip level :incremental sdb)))
                      (if (> distance 0) (+ distance (aref sdb level)) "Passed"))))
                 (typecase control
                   (fixnum "Flips when size exceeds threshold.")
                   (null "Automatic flipping disabled.")
                   (otherwise "Defer flips as long as possible."))))
      (terpri stream)

      ;; Report on the progress of the scavenger if there is a GC in progress.
      (when (not (null *gc-status*))
        (let ((scavenged 0)
              (scavengeable 0)
              (evacuated (read-meter sys:%transporter-words-copied))
              (condemned *oldspace-words*))
          (with-quick-region-area-accessors
            (for-every-region (region)
              (when (= (%region-scavenge-enable region) 1)
                (incf scavengeable (%region-free-pointer region))
                (incf scavenged (%region-gc-pointer region)))))
          (out "~%Of the ~:D words of condemned storage, ~:D words have already been evacuated~@
              to copyspace.  The scavenger has scanned ~D% of the existing scavengeable storage,~@
              with somewhere between ~:D and ~:D words of storage yet to be scanned.~@
              Up to ~:D words of free space may be required to contain additional evacuated objects.~%"
               condemned
               evacuated
               (truncate (percentage scavenged scavengeable))
               (- scavengeable scavenged)
               (+ (- scavengeable scavenged)
                  (- condemned evacuated))
               (- condemned evacuated))
          (when ( *batch-reclaim-count* 0)
            (out "Some process is doing a batch scavenge.~%"))))

      ;; Don't report anything about processes inhibiting flipping, since by definition
      ;;   at least one process (the gc-process, during a reclaim) is doing it, and other
      ;;   instances are going to be ultra-rare.  Let WAIT-FOR-FLIP-ALLOWED do it.
      ;; Might give a very brief summary of the gc statistics.
      ;; Might report amount of storage allocated/reclaimed since cold boot (clear
      ;;   *condemned-newspace-words* on booting to do this).

      ;; Might report highest virtual address (as in "The minimum size paging partition
      ;;   needed to run this world is 58,000 blocks.")
      (out "~%Estimated size to dump this world is ~D blocks." (si:estimate-dump-size))

      ;; Might say something about the distribution of reference volatilities ("426 pages
      ;;   in WORKING-STORAGE-AREA contain pointers to level 3 storage, 849 to level 2...")
      ;;   I don't know how expensive that is to compute.

      (out "~%")
      (values))))


;;;Following deleted since we want the flip to occur after the whole :before-cold
;;;initialization list is run and everybody has had a chance to clean up.
;;;(disk-save) now directly calls (gc:maybe-flip-level-2)
;;;(add-initialization "Flip level 2" '(maybe-flip-level-2) '(:before-cold))

(defun maybe-flip-level-2 ()
  (format *query-io*
          "~&Flipping level two now will result in better paging and GC behavior after the
   machine boots, by eliminating any garbage in levels two and three.~%")
  (when (yes-or-no-p-with-timeout (* 30 60) t "Flip level two?")
    (flip :volatility 2 :reclaim-mode :batch)))

;;;; User-level incremental flipper.

(defun flip (&key (volatility 3) areas (reclaim-mode :incremental))
  "Flip all regions of volatility  VOLATILITY that belong to an area in AREAS (which defaults
to all areas).  If a garbage collection is already in progress, does nothing and returns
nil, otherwise GC:FLIP returns t.  GC:FLIP signals an error if there is not enough free
space remaining to complete the specified flip.  A RECLAIM-MODE of :BATCH means scavenge
and reclaim immediately in this process; :INCREMENTAL means let the GC process do the
reclamation after the incremental scavenge completes."
  (check-type volatility (integer 0 3) "A volatility level (either 0, 1, 2 or 3")
  (check-type reclaim-mode (member :incremental :batch))
  (unless (null areas)
    ;; Verify that every element of AREAS is a valid area-name or area-number, and
    ;; set AREAS to a corresponding list of area-numbers.
    (setq areas (loop for area in areas
                      collect (etypecase area
                                (area-number area)
                                (area-name (symbol-value area))))))
  (let* ((generation %gc-generation-number)
         (sdb (compute-storage-distribution areas))
         (free (aref sdb 5))
         (committed (committed-free-space volatility reclaim-mode sdb)))
    ;; Make sure there's enough space.
    (if (> committed free)
        (ferror "This flip would require ~:D words of storage to complete.
There are only ~:D words of free space remaining." committed free)
      ;; There's enough space -- do the flip, unless somebody else beats us to it.
      (when (without-interrupts
              (and ;; Garbage collection not in progress.
                   (null *gc-status*)
                   ;; If the gc generation number has changed since we made our storage
                   ;; analysis, someone flipped in the meantime.
                   (= generation %gc-generation-number)
                   (%flip volatility areas)))
        ;; We flipped!  Arrange for reclamation.
        (report "GC: Flipped ~:D words of volatility ~D storage" *oldspace-words* volatility)
        (case reclaim-mode
          (:incremental
           (when (send *gc-process* :arrest-reasons)
             ;; If the GC process is turned off, fork off our own process to do the
             ;; incremental reclamation.  Otherwise the GC process will take care of it.
             (process-run-function "Incremental reclaim" #'reclaim-oldspace :incremental)))
          (t ;:batch
           (reclaim-oldspace :batch)))
        t))))



;;;; Batch garbage collection and hacks for cleaning up the world for system release.

(defun full-gc (&key mode (verbose t))
  "Do a complete batch-style garbage collection in this process.
If MODE is :SYSTEM-RELEASE, magic things happen to clean up the world."
  (check-type mode (member () :system-release))
  (let* ((stream (if verbose *standard-output* #'null-stream)))
    (report-elapsed-time stream 0 "FULL-GC"
      (lambda ()
        (report-elapsed-time stream 2 "reclamation of oldspace"
          #'reclaim-oldspace :batch)
        (report-elapsed-time stream 2 "before full gc initializations"
          #'initializations 'full-gc-initialization-list t)
        (when (eq mode :system-release)
          ;; this should perhaps look at an additional initialization list.
          ;; Do these before linearizing property lists (for obvious reasons).
          (report-elapsed-time stream 4 "removal of previous method definitions"
            #'remove-previous-method-definitions)
          (report-elapsed-time stream 4 "removal of previous symbol function definitions"
            #'remove-previous-symbol-definitions)
          (report-elapsed-time stream 4 "removal of previous function spec definitions"
            #'remove-previous-function-spec-definitions)
          (report-elapsed-time stream 4 "linearization of flavor plists"
            #'linearize-flavor-property-lists)
          (report-elapsed-time stream 4 "linearization of symbol plists"
            #'linearize-symbol-property-lists)
          (when gc-system-release-initialization-list
            (report-elapsed-time stream 4 "random system-release gc initializations"
              #'initializations 'gc-system-release-initialization-list t)))
        (let ((gc-enabled t))
          (unwind-protect
            (let ((*report-stream* stream))
              (report-elapsed-time stream 2 "turning off gc, reclaim oldspace"
                #'(lambda () (gc-off :reset t) (setq gc-enabled nil)))
              (let-if (eq mode :system-release)
                      ((*all-flips-to-higher-address-space* t))
                (report-elapsed-time stream 2 "flip and reclaim level 0"
                  #'flip :volatility 0 :reclaim-mode :batch))
              (when (eq mode :system-release)
                ;; Do another flip to move regions down to low part of address space (maybe).
                 (report-elapsed-time stream 2 "flip and reclaim level 0"
                        #'flip :volatility 0 :reclaim-mode :batch))
              (gc:gc-on)
              (setq gc-enabled t))
            (or gc-enabled
                (format t "~&WARNING: ** GC NOT ENABLED. YOU MUST DO (GC:GC-ON) TO ENABLED IT~%"))))
        (report-elapsed-time stream 2 "after full gc initializations"
          #'initializations 'after-full-gc-initialization-list t)))))

(defun linearize-symbol-property-lists ()
  (si::mapatoms-nr-sym (lambda (symbol)
                         (setf (symbol-plist symbol) (copy-list (symbol-plist symbol))))))

(defun linearize-flavor-property-lists ()
  (loop for flavor-name in si:*all-flavor-names*
        for flavor = (get flavor-name 'si:flavor)
        do (when flavor
             (setf (si::flavor-bindings flavor)
                   (copy-list (si::flavor-bindings flavor)))
             (setf (si::flavor-component-mapping-table-alist flavor)
                   (copy-alist (si::flavor-component-mapping-table-alist flavor)))
             (setf (si::flavor-all-instance-variables flavor)
                   (copy-list (si::flavor-all-instance-variables flavor)))
             (setf (si::flavor-mapped-instance-variables flavor)
                   (copy-list (si::flavor-mapped-instance-variables flavor)))
             (setf (si::flavor-plist flavor)
                   (copy-tree (si::flavor-plist flavor))))))

(defun remove-previous-symbol-definitions ()
  (si::mapatoms-nr-sym (lambda (symbol)
                         (remprop symbol :previous-definition))))

(defun remove-previous-function-spec-definitions ()
  (let ((count 0))
    (maphash #'(lambda (key &rest ignore)
                 (when (eq (second key) :previous-definition)
                   (send si:function-spec-hash-table :rem-hash key)
                   (incf count)))
             si:function-spec-hash-table)
    count))

(defun remove-previous-method-definitions ()
  (loop for flavor-name in si:*all-flavor-names*
        for flavor = (get flavor-name 'si:flavor)
     when flavor
       do (loop for entry in (si::flavor-method-table flavor)
             when (si::meth-plist (fourth entry))
               do (remf (si::meth-plist (fourth entry)) :previous-definition))))



;;; Object hashing

;;; This is a quick way of implementing weak pointers.
;;; (OBJECT-HASH frob) returns an integer.  This integer may be fed to
;;; OBJECT-UNHASH to yield the frob.  If, however, all pointers to the
;;; frob were dropped in-between hashing and unhashing, then OBJECT-UNHASH
;;; has the option of yielding nil (i.e. objects in the hash table will go
;;; away after a garbage collect if no one else points to them).

;;; While this implementation of weak pointers is nowhere near as flexible as
;;; some implementations, it is a good quick working hack.

;;; The *OBJECT->HASH-NUMBER-TABLE* holds the hash numbers keyed by the fixnum
;;; addresses of the objects.  Hashing the same (eq) object twice yields the same
;;; number.

;;; The *HASH-NUMBER->OBJECT-TABLE* holds the data types and the fixnum addresses
;;; of the objects keyed by the hash number.  This is so we can invert the mapping.

;;; After a GC, but before the reclaim, we scavenge the object tables and relink
;;; the addresses of any object which has been moved by the GC.  We drop the links to
;;; objects in oldspace.

;;; During a GC, the keys slowly become invalid.  This makes it impossible to hash
;;; off the keys.  We therefore do a linear search through the table validating and
;;; considering each key.  This is rather slow and it seems that the GC may often finish
;;; before we finish the scan.  Therefore, each step around the scan, we check to see if
;;; the GC is done.  If it is, we do the fast hash.

;;; The GC process is also locked out in a complicated way by using WITHOUT-INTERRUPTS in
;;; strategic places.  This ensures atomicity in searches.

(defvar *this-object-hash-number* 0)
(defvar *object-unhash-table* (make-hash-table)
  "Holds the mapping of hash numbers to objects.")

(defvar *object-hash-tables*
        (make-array (aref #'sys:system-communication-area sys:%sys-com-number-regions))
  "Holds the hash bucket corresponding to each region in the machine.")

(defsubst region-object-hash-table (region)
  (aref *object-hash-tables* region))

(defun for-all-oldspace-region-object-hash-tables (receiver)
  (declare (special %region-space-old))
  (dotimes (scan (length *object-hash-tables*))
    (when (= (%region-type scan) %region-space-old)
      (let ((table (locf (region-object-hash-table scan))))
        (when (contents table) (funcall receiver table))))))

(defun inhibiting-interrupts (receiver)
  (let* ((old inhibit-scheduling-flag)
         (inhibit-scheduling-flag t))
    (labels ((reset-scheduling-flag ()
               (setq inhibit-scheduling-flag old)))
      (unwind-protect
          (funcall receiver #'reset-scheduling-flag)
        (reset-scheduling-flag)))))

(defun object-hash-non-pointer (frob)
  ;; Non pointer frobs are simply turned into bignums with the
  ;; same bits.  They are negative so that object unhash will
  ;; know how to invert them.
  (let ((pointer (locf frob)))
    ;; POINTER is a hack to get around LDB not working on non fixnums
    (- (logior (ash (%data-type frob) (byte-position %%q-data-type))
               (ash (%p-ldb (byte 1 (1- (byte-size %%q-pointer))) pointer) (1- (byte-size %%q-pointer)))
               (%p-ldb (byte (1- %%q-pointer) 0) pointer)))))

(defun object-hash-pointer (frob)
  (catch 'got-hash-code
    (without-flipping                           ; Now frob will not move.
      (let* ((frob-region    (%region-number frob))
             (frob-address   (%pointer       frob))
             (frob-data-type (%data-type     frob))
             (table (region-object-hash-table frob-region)))
        (labels ((return-the-code (value)
                   (throw 'got-hash-code value))
                 (new-hash-code ()
                   (when (null table)
                     (setq table (make-hash-table))
                     (setf (region-object-hash-table frob-region) table))
                   (setf (gethash frob-address table)
                         (incf *this-object-hash-number*))
                   (setf (gethash *this-object-hash-number* *object-unhash-table*)
                         (cons frob-data-type frob-address))
                   *this-object-hash-number*)
                 (quick-search-hash-table (receiver)
                   (when table
                     (multiple-value-bind (value found? ignore)
                         (gethash frob-address table)
                       (when found? (funcall receiver value))))))
          (inhibiting-interrupts
            #'(lambda (re-enable-interrupts)
                (quick-search-hash-table #'return-the-code)
                ;; It would be bad if *gc-status* changed in here.
                (if (or (null *gc-status*) (= (%region-type frob-region) %region-space-new))
                    (new-hash-code)
                    ;; Now we know that there is a scavenge in progress, and the object we got
                    ;; may have been evacuated out of oldspace.  Search all the oldspace buckets
                    ;; for the frob.  Nobody will rehash the oldspace buckets.
                    (progn (funcall re-enable-interrupts)
                           (for-all-oldspace-region-object-hash-tables
                             #'(lambda (hash-table-locative)
                                 (maphash
                                   #'(lambda (test-key value)
                                       (if (null *gc-status*)
                                           (progn
                                             (without-interrupts
                                               (quick-search-hash-table #'return-the-code)
                                               (throw 'got-hash-code (new-hash-code))))
                                           (relink-weak-pointer test-key
                                             #'(lambda (newspace-version)
                                                 (when (= newspace-version frob-address)
                                                   (return-the-code value)))
                                             #'ignore)))
                                   (contents hash-table-locative))))
                           ;; We looked everywhere for it.  But it may be the case that
                           ;; the scavenge completed before we looked anywhere.  We look
                           ;; again and then give up.
                           (without-interrupts
                             (quick-search-hash-table #'return-the-code)
                             (new-hash-code)))))))))))

(defun object-hash (frob)
  "Return a unique number which can be used by OBJECT-UNHASH to get the <frob> back.
OBJECT-UNHASH has the option of returning NIL if no pointers to the <frob> exist elsewhere.
OBJECT-HASH will return the same number on EQ objects."
  (if (%pointerp frob)
      (object-hash-pointer frob)
      (object-hash-non-pointer frob)))

(defun object-unhash-non-pointer (hash-number)
  (let ((frob (ncons nil))
        (data-type (ldb %%q-data-type hash-number)))
    (when (%pointer-type-p data-type)
      (ferror nil "Illegal data to OBJECT-UNHASH.  Hash code is invalid."))
    (%p-store-tag-and-pointer
      frob
      (logior (ash cdr-nil (byte-size %%q-data-type)) data-type)
      (ldb (byte (1- (byte-size %%q-pointer)) 0) hash-number))
    (%p-dpb (ldb (byte 1 (1- (byte-size %%q-pointer))) hash-number)
            (byte 1 (- (byte-size %%q-pointer) 1))
            frob)
    (car frob)))

(defun object-unhash-pointer (hash-number if-found if-not-found)
  (let ((frob (ncons nil))
        (cont nil))
    (without-interrupts
      (let ((object-parts (gethash hash-number *object-unhash-table*)))
        (if object-parts
            ;; CAR will do the transport for us.
            (progn (%p-dpb (car object-parts) %%q-data-type frob)
                   (%p-dpb (cdr object-parts) %%q-pointer frob)
                   (setq cont #'(lambda () (funcall if-found (car frob)))))
            (setq cont if-not-found))))
    (funcall cont)))

(defun maybe-object-unhash (hash-number if-found if-not-found)
  (when (> hash-number *this-object-hash-number*)
    (ferror nil "Hash code ~S has not yet been assigned to any object." hash-number))
  (if (minusp hash-number)
      (funcall if-found (object-unhash-non-pointer (- hash-number)))
      (object-unhash-pointer hash-number if-found if-not-found)))

(defun object-unhash (hash-number)
  "Given an integer created by OBJECT-HASH, return the object associated with that
integer, or NIL, if the object has been reclaimed by the garbage collector."
  (maybe-object-unhash hash-number #'identity #'false))

(defsubst oldspace-pointer? (p)
  (= (%region-type (%region-number p)) %region-space-old))

(defun relink-weak-pointer (pointer keep flush)
  ;; Given a fixnum address of an object, call receiver if the object is in newspace.
  ;; Must be called WITHOUT INTERRUPTS so no one touches the frob while we are
  ;; looking for it.  Actually, I think it could be called with interrupts...
  ;; (It may be the case that we could lose because we are picking up random
  ;; fefs to run and causing transports to happen.  If we NEVER object hash any of the
  ;; object hash routines this is not a concern.)
  (let ((what-i-point-at (follow-structure-forwarding (%make-pointer dtp-locative pointer))))
    (if (oldspace-pointer? what-i-point-at)
        (if (= (%p-data-type what-i-point-at) dtp-gc-forward)
            ;; Garbage collector moved it, relink new address.
            (funcall keep (%p-pointer what-i-point-at))
            (funcall flush))
        (funcall keep (%make-pointer dtp-fix what-i-point-at)))))

(defun relink-object-hash-table ()
  ;; For every entry in the hash tables, relink the objects that have moved
  ;; and remove all the ones in oldspace.  Atomically replace the object hash
  ;; tables with the new version.
  (for-all-oldspace-region-object-hash-tables
    #'(lambda (hash-table-locative)
        (maphash
          #'(lambda (key hash-code)
              (relink-weak-pointer key
                #'(lambda (relink)
                    (let ((move-to-hash-table
                            (or (region-object-hash-table (%region-number relink))
                                (let ((table (make-hash-table)))
                                  (setf (region-object-hash-table (%region-number relink)) table)
                                  table))))
                      (setf (gethash relink move-to-hash-table) hash-code)
                      (setf (cdr (gethash hash-code *object-unhash-table*)) relink)))
                #'(lambda ()
                    (remhash hash-code *object-unhash-table*))))
          (contents hash-table-locative))
        (setf (contents hash-table-locative) nil))))

(pushnew 'relink-object-hash-table *after-scavenge-daemons*)

;;; Populations are weak sets.

;;; (MAKE-POPULATION name)
;;; (ADD-TO-POPULATION population element)
;;; (REMOVE-FROM-POPULATION population element)
;;; (POPULATION->LIST population)
;;; (MAP-OVER-POPULATION procedure population)

(defstruct (population (:conc-name "POPULATION-")
                       (:callable-constructors nil)
                       (:constructor create-population)
                       (:print-function print-population))
  identification
  lock
  members)

(defun print-population (population stream ignore)
  (si:printing-random-object (population stream :type)
    (format stream "~s" (population-identification population))))

(defvar *the-meta-population*
        (create-population
          :identification '*the-meta-population*
          :lock    '()
          :members '()))

(defun add-to-population! (population object)
  (with-lock ((population-lock population))
    (pushnew (object-hash object) (population-members population) :test #'=)
    (values)))

(defun remove-from-population! (population object)
  (with-lock ((population-lock population))
    (setf (population-members population)
          (del #'= (object-hash object) (population-members population)))))

(defun scan-population (population if-exists if-gone)
  (with-lock ((population-lock population))
    (dolist (member (population-members population))
      (maybe-object-unhash member if-exists if-gone))))

(defun collect-population (population)
  (with-lock ((population-lock population))
    (setf (population-members population)
          (remove-if-not #'(lambda (member)
                         (maybe-object-unhash member
                                              #'(lambda (ignore) t)
                                              #'false))
                     (population-members population)))))

(defun map-over-population (procedure population)
  (scan-population population procedure #'ignore))

(defun population->list (population)
  (let ((the-list '()))
    (map-over-population
      #'(lambda (member) (push member the-list))
      population)
    the-list))

(defun make-population (identification)
  (let ((the-population (create-population
                          :identification identification
                          :lock    '()
                          :members '())))
    (add-to-population! *the-meta-population* the-population)
    the-population))

(defun gc-all-populations ()
  (collect-population *the-meta-population*)
  (map-over-population #'collect-population *the-meta-population*))

(pushnew 'gc-all-populations *after-scavenge-daemons*)

;;;; Weak pointers.  ***** Not yet implemented *****

;;; Weak pointers are one-q-forwards to two-word unboxed weak-pointer-links.  The pointers
;;; in these weak-pointer-links are not scavenged, so are left behind by the garbage collector.
;;; Before oldspace is reclaimed after every flip, all the weak-pointer-links in the world are
;;; examined, and those that still point to oldspace are set to nil.

;;; Included in each weak-pointer-link is a thread through all the weak-pointer-links in the
;;; world.  Because we don't want to unnecessarily retain these weak-pointer-links, the thread
;;; pointer is also unboxed, and weak-pointer-links left in oldspace are deleted from the thread.

;;; Note that much weak-pointer manipulation uses fixnums to express addresses.

;; use a new array-type so that %weak-pointer-p is doable? use %make-array?
;(defun make-weak-pointer-internal ()
;  (let ((link (make-array 3 :type art-32b)))
;    ;;the contents of the array has to look like a structure so %find-structure-header will work
;    (dotimes (i 3)
;      (%p-dpb-offset dtp-fix %%q-data-type link (1+ i)))
;    link))

;(defvar *weak-pointer-thread* (make-weak-pointer-internal)
;  "Always contains a pointer to an array that is like a weak-poitner structure,
;but is only used to make the beginning of the chain look like the rest.")

;(defsubst %weak-link-thread (link) (%p-ldb-offset %%q-pointer link 1))
;(defsubst %weak-link-contents (link) (%p-contents-offset link 2))
;(defsubst %weak-link-address (link) (%p-ldb-offset %%q-pointer link 2))

;;remember that DTP-ONE-Q-FORWARD cannot appear in the stack.  in particular,
;;you can't return one, or pass one as an argument

; push-weak-pointer is probably a good macro to have
;(defmacro store-weak-pointer (place value &optional (generations-to-live 1))
;  "For example, do
;     (let ((foo (make-big-structure)))
;       (push () *big-structure-list*)
;       (store-weak-pointer (car *big-structure-list*) foo))"
;  `(store-weak-pointer-1 (locf ,place) ,value))

;(defun store-weak-pointer-1 (location pointer generations-to-live)
;  (check-type location locative)
;  (let ((link (make-weak-pointer-internal)))
;    (without-interrupts
;      (setf (%weak-link-thread link) (%weak-link-thread *weak-pointer-thread*))
;      (setf (%weak-link-thread *weak-pointer-thread*) link)
;      (setf (%weak-link-contents link) pointer)
;      (setf (%weak-link-generations-to-live link) generations-to-live)
;      (%p-store-tag-and-pointer location
;                               (dpb (%p-cdr-code location)
;                                    (byte (byte-size %%q-cdr-code)
;                                          (- (byte-position %%q-cdr-code)
;                                             (byte-position %%q-data-type)))
;                                    dtp-one-q-forward)
;                               (%pointer-plus link 2)))))

;(defun reclaim-weak-pointers ()
;  (without-interrupts
;    (do ((weak-structure (%weak-link-thread *weak-pointer-thread*)
;                        (%weak-link-thread weak-structure))
;        (prev-weak-structure (%pointer *weak-pointer-thread*)
;                             weak-structure))
;       (())
;      ;; skip over all weak structure that are still in old space
;      (do ()
;         ((not (oldspace-pointer? weak-structure)))
;       (setq weak-structure (%weak-link-thread weak-structure)))
;      (setf (%weak-link-thread prev-weak-structure) weak-structure)
;      (if (zerop weak-structure) (return ()))
;      ;; now see if the contents of this weak-structure is still in old space
;      (let ((adr (%weak-link-address weak-structure))
;           (gen (decf (%weak-link-generations-to-live weak-structure))))
;       (when (oldspace-pointer? adr)
;         (cond ((or (eq (%p-data-type adr) dtp-gc-forward)
;                    (plusp gen))
;                ;;reference the cell to snap the GC-FORWARD, or to cause
;                ;;the object to be copied this time
;                (%weak-link-contents weak-structure))
;               (t
;                ;; poor object was left behind,  give up on it
;                (setf (%weak-link-contents weak-structure) ()))))))))

;(defun describe-weak-pointers (&optional print-objects)
;  (without-flipping
;    (do ((weak-structure (%weak-link-thread *weak-pointer-thread*)
;                        (%weak-link-thread weak-structure)))
;       ((zerop weak-structure))
;      (format t "~&***Weak-structure at address #o~o" weak-structure)
;      (if (oldspace-pointer? weak-structure)
;         (format t " (in old space)"))
;      (let ((adr (%weak-link-address weak-structure)))
;       (format t " ~S generations to live" (%weak-link-generations-to-live weak-structure))
;       (format t "~% Object in area ~s" (area-name (%area-number adr)))
;       (cond ((oldspace-pointer? adr)
;              (cond ((eq (%p-data-type adr) dtp-gc-forward)
;                     (format t " (in old space, already forwarded)  ")
;                     (if print-objects
;                         (format t "~&~s" (%find-structure-header (%p-pointer adr)))))
;                    (t
;                     (format t " (in old space)"))))
;             (t
;              (if print-objects
;                  (format t "~&~s" (%find-structure-header adr)))))))))

(eval-when (load)
  (when *gc-disabled-during-loading*
    (setq *gc-disabled-during-loading* t)
    (gc-on)))

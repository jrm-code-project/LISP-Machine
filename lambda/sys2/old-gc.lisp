;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains the Lisp-coded support for the Garbage Collector
;;; Some GC-related functions that need to be in the cold-load can be found in QRAND.

;*** Needs a facility which continuously maintains a second who-line with gc stats?


(DEFVAR GC-REPORT-STREAM T
  "Stream to write GC messages on, or NIL meaning don't, or T meaning make notifications.")

(DEFVAR GC-PROCESS (MAKE-PROCESS "Garbage Collector")
  "Process that runs the gc flipper")

;;; These are lists of forms which are evaluated after reclaiming oldspace
;;; and before flipping newspace into oldspace.
(DEFVAR GC-EVERY-FLIP-LIST NIL
  "Forms to evaluate on every flip")
(DEFVAR GC-NEXT-FLIP-LIST NIL
  "Forms to evaluate just on the next flip")
(DEFVAR GC-SECOND-NEXT-FLIP-LIST NIL
  "Forms to evaluate just on the flip after next")
(DEFVAR GC-AFTER-FLIP-LIST NIL
  "Forms to evaluate after flipping")
(DEFVAR GC-BEFORE-RECLAIM-LIST NIL
  "Forms to eval just before reclaiming oldspace")

(DEFVAR GC-PAGE-CONS-ALARM-MARK 0
  "Value that %PAGE-CONS-ALARM  must be greater than in order to do a flip.
Set by the GC process. If %GC-FLIP-READY is off, this is ignored.")

(DEFVAR GC-FLIP-RATIO 1
  "Fraction of dynamic data expected to be still living (not garbage).
If this parameter is less than 1, GC takes place less often.
But if you underestimate, GC may not be able to finish.")

(DEFVAR GC-FLIP-MINIMUM-RATIO NIL
  "If non-NIL, overrides GC-FLIP-RATIO for deciding whether it is safe to flip.
GC-FLIP-RATIO is still used in deciding when to flip when automatic GC is on.
It only makes sense for this to be a smaller number than GC-FLIP-RATIO.")

(DEFVAR GC-RECLAIM-IMMEDIATELY NIL
  "T means reclaim oldspace immediately after each flip.
NIL means reclaim oldspace gradually (incrementally) as program runs.
T enables a GC to complete with less free space.")

(DEFVAR GC-RECLAIM-IMMEDIATELY-IF-NECESSARY NIL
  "T means reclaim immediately if not enough space for incremental gc.")

(DEFVAR GC-SCAVENGER-WS-SIZE :UNBOUND
  "Physical pages the scavenger may use.
Don't set this variable directly, instead call SET-SCAVENGER-WS.")

(DEFVAR GC-FLIP-LOCK NIL
  "Flipping must be done with this lock locked.")

(DEFVAR INHIBIT-GC-FLIPS NIL
  "Non-NIL prevents flipping from happening.")

(DEFVAR GC-OLDSPACE-EXISTS NIL
  "T after flipping until oldspace is reclaimed.")

(DEFVAR %GC-GENERATION-NUMBER :UNBOUND
  "A number incremented at each flip.
Tells hash tables when they must rehash.") ; Indirected to SYS-COM area.

(DEFVAR GC-BATCH-THIS-TIME NIL
  "T when a nonincremental reclaim is happening.")

(DEFCONST COMMITTED-FREE-SPACE-FUDGE #o1000000
  "Require this much free space for GC in addition to what appears to be needed.
This is to take account of extra space used
because regions are allocated bigger than their data.")

;;; Args like FORMAT, but stream comes from GC-REPORT-STREAM
(DEFUN GC-REPORT (FORMAT-CONTROL &REST FORMAT-ARGS)
  (COND ((NULL GC-REPORT-STREAM))
        ((EQ GC-REPORT-STREAM T)
         (APPLY #'PROCESS-RUN-FUNCTION "GC notification"
                #'TV:NOTIFY NIL FORMAT-CONTROL FORMAT-ARGS))
        (T (SEND GC-REPORT-STREAM :FRESH-LINE)
           (APPLY #'FORMAT GC-REPORT-STREAM FORMAT-CONTROL FORMAT-ARGS))))

(DEFUN GC-WARM-BOOT ()
  ;; %GC-FLIP-READY is always set to NIL by warm boot.
  ;; I think this sets it to what it is supposed to be.
  (SETQ %GC-FLIP-READY (NOT GC-OLDSPACE-EXISTS))
  (SETQ TEMPORARILY-NO-IDLE-SCAVENGING NIL)
  (SETQ INHIBIT-GC-FLIPS NIL))
(ADD-INITIALIZATION 'GC-WARM-BOOT '(GC-WARM-BOOT) '(:WARM))


;;;; Flipper

;;; This function performs a flip only if "a good idea", i.e. if the scavenger
;;; is done and we are anywhere near running out of free space.
;;; How close we are to running out of free space is determined by FREE-SPACE-RATIO.
;;; This number should be greater than or equal to 1 if most of the committed free
;;; space contains meaningful data.  If there is a lot of garbage around, then
;;; this number can be less than 1 to reduce the frequency of flips.  The higher
;;; this number is, the greater the chance of actually doing the flip.
;;; The RECLAIM-IMMEDIATELY parameter will cause the scavenger to take off
;;; as soon as the flip is done and reclaim all oldspace.  If you want to
;;; flush all garbage "immediately", call this function with a large ratio
;;; and with a second argument of T.
;;; Returns T if it flipped and NIL if it didn't.
(DEFUN GC-FLIP-MAYBE (&OPTIONAL (FLIP-RATIO GC-FLIP-RATIO) (RECLAIM-IMMEDIATELY NIL))
  (AND %GC-FLIP-READY
       (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
           (GC-GET-COMMITTED-FREE-SPACE RECLAIM-IMMEDIATELY)
         (WHEN ( (* FLIP-RATIO (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE))
                  FREE-SPACE)
           (GC-FLIP-NOW)
           (IF RECLAIM-IMMEDIATELY (GC-RECLAIM-OLDSPACE))
           T))))
(DEFF GC-FLIP-IF-NECESSARY 'GC-FLIP-MAYBE)

;;; This function performs a flip.  It can be called either by the user
;;; or by the GC process, at any time (much faster if scavenger is done already!)
;;; Must return T for GC-FLIP-MAYBE.
(DEFUN GC-FLIP-NOW ()
  (WITH-LOCK (GC-FLIP-LOCK)
    (IF (NOT %GC-FLIP-READY) (GC-RECLAIM-OLDSPACE))     ;In case not reclaimed already
    (SETQ %PAGE-CONS-ALARM 0 %REGION-CONS-ALARM 0)      ;avoid overflow in these fixnums
    (DOLIST (ELEM GC-DAEMON-QUEUE)
      (GC-DAEMON-QUEUE (FIRST ELEM) (SECOND ELEM) 1 1 ELEM))
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE)
        (GC-GET-SPACE-SIZES)
      (GC-REPORT                        ;separate static from exited when exited exists?
        "GC: About to flip.  Dynamic space=~D., Static space=~D., Free space=~D."
        DYNAMIC-SIZE (+ STATIC-SIZE EXITED-SIZE) FREE-SIZE)
      (WITHOUT-INTERRUPTS
        (PROCESS-WAIT "Flip inhibited" #'(LAMBDA () (NOT INHIBIT-GC-FLIPS)))
        ;; Perform whatever actions other programs need to do on flips
        (MAPC #'EVAL GC-EVERY-FLIP-LIST)
        (MAPC #'EVAL (PROG1 GC-NEXT-FLIP-LIST
                            (SETQ GC-NEXT-FLIP-LIST GC-SECOND-NEXT-FLIP-LIST
                                  GC-SECOND-NEXT-FLIP-LIST NIL)))
        ;; Reset the GC scan pointers of all regions, actually only in static and fixed areas
        ;; is it necessary.
        (DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION)))
            ((MINUSP REGION))
          (%gc-scav-reset region)
          (STORE (REGION-GC-POINTER REGION) 0))
        ;; Invalidate AR-1's cache.
        (SETQ AR-1-ARRAY-POINTER-1 NIL)
        (SETQ AR-1-ARRAY-POINTER-2 NIL)
        ;; Don't forget to actually flip! (Change newspace to oldspace in all dynamic areas)
        (%GC-FLIP T)
        ;; Deallocate space at the end of the oldspace regions, if we can.
        (DOTIMES (REGION SIZE-OF-AREA-ARRAYS)
          (IF (= %REGION-SPACE-OLD
                 (%LOGLDB %%REGION-SPACE-TYPE (REGION-BITS REGION)))
              (DEALLOCATE-END-OF-REGION REGION)))
        (SETQ %GC-GENERATION-NUMBER (1+ %GC-GENERATION-NUMBER))
        (WHEN GC-AFTER-FLIP-LIST
          (GC-REPORT "GC: something is using SI:GC-AFTER-FLIP-LIST; please send a bug report.")
          (MAPC #'EVAL GC-AFTER-FLIP-LIST))
        (INITIALIZATIONS 'AFTER-FLIP-INITIALIZATION-LIST T))
      (SETQ GC-OLDSPACE-EXISTS T)
      T)))

;;;; Compute statistics needed for GC decisions.

(DEFUN GET-FREE-SPACE-SIZE ()
  (MULTIPLE-VALUE-BIND (NIL NIL NIL FREE)
      (GC-GET-SPACE-SIZES)
    FREE))

(DEFUN GC-GET-SPACE-SIZES (&OPTIONAL STATIC-REGIONS-OF-DYNAMIC-AREAS-ARE-DYNAMIC)
  (DECLARE (RETURN-LIST DYNAMIC-SPACE-WORDS STATIC-SPACE-WORDS NIL
                        FREE-SPACE-WORDS OLD-SPACE-WORDS))
  (DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION))
       (SZ)
       (DYNAMIC-SIZE 0)
       (STATIC-SIZE 0)
       (EXITED-SIZE 0)
       (FREE-SIZE (GET-FREE-SPACE-SIZE-1))
       (OLD-SIZE 0))
      ((MINUSP REGION)
       (RETURN DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE OLD-SIZE))
    (SETQ SZ (%POINTER-UNSIGNED (%REGION-FREE-POINTER REGION)))
    (SELECT (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
      ((%REGION-SPACE-NEW %REGION-SPACE-COPY)
       ;; If the area is going to become static at next flip,
       ;; and we are finished scavenging, count it as static now.
       (IF (AND %GC-FLIP-READY (AREA-STATIC-P (%AREA-NUMBER (REGION-ORIGIN REGION))))
           (INCF STATIC-SIZE SZ)
         (INCF FREE-SIZE (FLOOR (- (%POINTER-UNSIGNED (REGION-LENGTH REGION)) SZ)
                                %ADDRESS-SPACE-QUANTUM-SIZE))
         (INCF DYNAMIC-SIZE SZ)))
      (%REGION-SPACE-OLD
        (SETQ OLD-SIZE (+ SZ OLD-SIZE)))
      ;; Count static regions even of dynamic areas as static space, unless arg is T.
      ;; FULL-GC alone sets the arg, since it will make these regions dynamic for its GC.
      ((%REGION-SPACE-STATIC %REGION-SPACE-FIXED)
       (IF (AND STATIC-REGIONS-OF-DYNAMIC-AREAS-ARE-DYNAMIC
                (NOT (ZEROP SZ))                ;Actually happens, due to a bug.
                (NOT (AREA-STATIC-P (%AREA-NUMBER (REGION-ORIGIN REGION)))))
           (INCF DYNAMIC-SIZE SZ)
         (INCF STATIC-SIZE SZ))))))

;;; Returns the number of words of free space
(DEFUN GET-FREE-SPACE-SIZE-1 ()
  (* (LOOP FOR I FROM (TRUNCATE (+ (REGION-ORIGIN INIT-LIST-AREA)
                                   (REGION-LENGTH INIT-LIST-AREA))
                                %ADDRESS-SPACE-QUANTUM-SIZE)
                 BELOW (TRUNCATE VIRTUAL-MEMORY-SIZE %ADDRESS-SPACE-QUANTUM-SIZE)
           COUNT (ZEROP (AREF #'ADDRESS-SPACE-MAP I)))
     %ADDRESS-SPACE-QUANTUM-SIZE))

(DEFUN GET-DIRECT-GC-WORK-REMAINING ()
  "Return the number of words now waiting to be scavenged.
Note: More data may appear and need to be scavanged before we are finished."
  (DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION))
       (WORK 0))
      ((MINUSP REGION)
       (RETURN WORK))
    (UNLESS (ZEROP (LDB %%REGION-SCAVENGE-ENABLE (REGION-BITS REGION)))
      (SETQ WORK (+ WORK (- (%REGION-FREE-POINTER REGION)
                            (REGION-GC-POINTER REGION)))))))

(DEFUN GET-MAX-GC-WORK-REMAINING (&AUX (COPYING 0))
  "Return an upper bound on the number of words to be scavenged before reclamation.
The second value is the part which is not certain (u.b. minus l.b.)."
  (DOLIST (AREA (CURRENT-AREA-LIST))
    (LET ((AREA-NUMBER (SYMEVAL AREA)))
      (DO ((REGION (AREA-REGION-LIST AREA-NUMBER) (REGION-LIST-THREAD REGION))
           (OLD-SIZE 0)
           (COPY-SIZE 0))
          ((MINUSP REGION)
           (SETQ COPYING (+ COPYING (MAX 0 (- OLD-SIZE COPY-SIZE)))))
        (SELECT (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
          (%REGION-SPACE-OLD (SETQ OLD-SIZE (+ OLD-SIZE (%REGION-FREE-POINTER REGION))))
          (%REGION-SPACE-COPY (SETQ COPY-SIZE (+ COPY-SIZE (%REGION-FREE-POINTER REGION))))))))
  (VALUES (+ COPYING (GET-DIRECT-GC-WORK-REMAINING)) COPYING))

;;; If called when %GC-FLIP-READY is true, returns a conservative (over) estimate of
;;; the amount of free space which will be used up during the next cycle before
;;; %GC-FLIP-READY can set again.  This is based on the way consing drives scavenging.
;;; Also returns the current amount of free space since it happens to know it.

;;; In the below, the size of static and dynamic spaces are at the time of the flip,
;;; which is bigger than the current values.  The objective is to compute how much
;;; bigger they can be allowed to grow.

;;; Total scavenger work = amount of static space to be scavenged
;;;                        + 2 x dynamic space (which is both scavenged and copied)
;;;                        + scavenging of static space consed after the flip
;;;                             [dynamic space consed after the flip is newspace
;;;                              rather than copyspace and need not be scavenged]
;;; Total consing (consumption of free space) =
;;;                     (1/K) x scav work
;;;                     + amount of dynamic space which is copied
;;;                     + region breakage
;;; K=4 in the current microcode
;;;
;;; Uncertainties which can use up more free space:
;;;     Consing after the flip in static space rather than dynamic or exited space
;;;     Region breakage
;;;     Consing during GC process wakeup delay
;;; Uncertainties which decrease consumption of free space:
;;;     Scavenging by the idle-loop rather than by CONS
;;;     Certain fixed areas which count as static space but aren't actually scavenged
;;;     Shrinkage of dynamic space (generally some is garbage and will be
;;;             neither copied nor scavenged)
;;;     Consing of additional static space before the flip, which is less
;;;             expensive than additional dynamic space.
;;;     Space already assigned to regions but not yet allocated by CONS
;;;
;;; For maximum delay of fliping, we want to allow enough consing before the
;;; flip so that the remaining free space is exactly equal to the consing after
;;; the flip.  The algebraic manipulation is as follows (incorporating the
;;; worst case assumptions: no garbage, all consing before flip is dynamic,
;;; all consing after flip is static).  Normally I wouldn't bother commenting
;;; this but several people have got it wrong, so it must be hard.
;;;     F0 = free space now
;;;     D0 = dynamic space now
;;;     ND = additional dynamic space consed before the flip
;;;     S0 = static space now
;;;     W  = scavenger work to do after the flip
;;;     C  = consing required after the flip.
;;;
;;; C = D0 + ND + W/k           ;copy all dynamic plus do necessary scavenger work
;;; W = S0 + 2(D0 + ND) + C     ;scav static, scav and copy all dynamic, scav new static
;;; F0 = ND + C                 ;free space divided between before & after consing
;;;
;;; (k-1)C = (k+2)(D0+ND) + S0          ;plugging in for W and collecting C on the left
;;; (k-1)C = (k+2)D0 + S0 + (k+2)(F0-C) ;plugging in for ND
;;; C = [ (k+2)D0 + S0 + (k+2)F0 ] / (2k+1)     ;solving for C
;;; Note that old-space is counted as free.

;Hmmm.  I think that in fact W = S0 + 2(D0 + ND) + W/k.
;I suspect that C was used as a way of saying "all consing after the flip is static",
;but not all of it can be; D0 + ND worth has to be dynamic.
;Only the W/k part is unspecified consing that causes scavenger work to get done.
;The worst case is that all that is static.

;So, solving for W: W * (1 - 1/k) = S0 + 2(D0 + ND)
; W = [S0 + 2(D0 + ND)] / (1 - 1/k)
; C = D0 + ND + [S0 + 2(D0 + ND)] / (k - 1)
; Substitute for ND:
; C = D0 + F - C + [S0 + 2(D0 + F - C)] / (k - 1)
; [2 + 2/(k - 1)]C = D0 + F + [S0 + 2(D0 + F)] / (k - 1)
; C = {D0 + F + [S0 + 2(D0 + F)] / (k - 1)} / [2 + 2/(k - 1)]

; That is if you let scavenging happen due to consing.
; If GC-RECLAIM-IMMEDIATELY is set, or if you do FULL-GC,
; then the W/k term disappears from all equations.
; C = D0 + ND
; W = S0 + 2(D0 + ND)
; F = ND + C.
;Therefore, F = D0 + 2ND, so ND = (F - D0)/2.

;So, we can compute two different values of committed free space (C),
;depending on whether you plan to reclaim immediately or incrementally.
(DEFUN GC-GET-COMMITTED-FREE-SPACE (&OPTIONAL (RECLAIM-IMMEDIATELY GC-RECLAIM-IMMEDIATELY)
                                    (K 4)       ;K is the magic constant
                                    STATIC-REGIONS-OF-DYNAMIC-AREAS-ARE-DYNAMIC)
  (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE OLD-SIZE)
        (GC-GET-SPACE-SIZES STATIC-REGIONS-OF-DYNAMIC-AREAS-ARE-DYNAMIC)
    (SETQ FREE-SIZE (+ FREE-SIZE OLD-SIZE))     ;Old space will be reclaimed
    EXITED-SIZE                                 ;Scavenger never deals with exited space
    (LET ((CONSING (IF RECLAIM-IMMEDIATELY
                       (+ DYNAMIC-SIZE
                          (FLOOR (- FREE-SIZE DYNAMIC-SIZE) 2)
                          #o2000000)            ;Extra fudge, needed for region breakage.
                     (FLOOR (+ DYNAMIC-SIZE FREE-SIZE
                               (FLOOR (+ STATIC-SIZE (* 2 DYNAMIC-SIZE) (* 2 FREE-SIZE))
                                      (1- K)))
                            (* 2 (1+ (FLOOR 1.0S0 (- K 1))))))
                   #| (// (+ (* (+ K 2) (+ DYNAMIC-SIZE FREE-SIZE)) STATIC-SIZE)
                          (+ (* 2 K) 1)) |#
                   ))
      (VALUES (FLOOR CONSING) FREE-SIZE))))

;;; Print various statistics
(DEFUN GC-STATUS (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print various statistics about garbage collection."
  (UNLESS %GC-FLIP-READY
    (FORMAT STREAM "~&~:[Incremental~;Batch~] garbage collection now in progress."
            GC-BATCH-THIS-TIME))
  (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
      (GC-GET-COMMITTED-FREE-SPACE)
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE NIL NIL OLD-SIZE)
        (GC-GET-SPACE-SIZES)
      (FORMAT STREAM "~&Dynamic (new+copy) space ~:D, Old space ~:D, Static ~:D,"
              DYNAMIC-SIZE OLD-SIZE STATIC-SIZE)
      (COND (%GC-FLIP-READY
             (FORMAT STREAM "~%Free space ~:D, with ~:D needed for garbage collection
 assuming ~D% live data (~S = ~D)."
                     FREE-SPACE
                     (FLOOR (* GC-FLIP-RATIO
                               (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE)))
                     (FLOOR (* GC-FLIP-RATIO 100.))
                     'GC-FLIP-RATIO GC-FLIP-RATIO)
             (LET ((DISTANCE
                     (- FREE-SPACE
                        (FLOOR (* GC-FLIP-RATIO
                                  (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE)))))
                   (SAFE-DISTANCE
                     (- FREE-SPACE
                        (FLOOR (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
                                  (+ COMMITTED-FREE-SPACE
                                     COMMITTED-FREE-SPACE-FUDGE)))))
                   (NONINC-DISTANCE
                     (- FREE-SPACE
                        (FLOOR (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
                                  (+ (GC-GET-COMMITTED-FREE-SPACE T)
                                     COMMITTED-FREE-SPACE-FUDGE))))))
               (COND ((PLUSP DISTANCE)
                      (IF (ASSQ GC-PROCESS ACTIVE-PROCESSES)
                          (FORMAT STREAM "~%A")
                          (FORMAT STREAM "~%If GC is turned on, a"))
                      (FORMAT STREAM " flip will happen in ~:D words." DISTANCE))
                     ((MINUSP NONINC-DISTANCE)
                      (FORMAT STREAM "~%It is ~:D words too late to do garbage collection of any sort."
                              (- NONINC-DISTANCE)))
                     ((MINUSP SAFE-DISTANCE)
                      (FORMAT STREAM
                              "~%It is ~:D words too late to do incremental garbage collection
 but batch (~S, or ~S set to ~S)
 is still safe for ~:D more words."
                              (- SAFE-DISTANCE) 'GC-IMMEDIATELY 'GC-RECLAIM-IMMEDIATELY T
                              NONINC-DISTANCE))
                     (T (FORMAT STREAM "~%A flip should have happened ~:D words ago,
 but it is still safe to turn on GC for ~:D more words."
                                (MINUS DISTANCE) SAFE-DISTANCE)))))
            (T
             (MULTIPLE-VALUE-BIND (WORK COPYING)
                 (GET-MAX-GC-WORK-REMAINING)
               (LET ((FREE-SIZE (GET-FREE-SPACE-SIZE)))
                 (FORMAT STREAM "~%Between ~:D and ~:D words of scavenging left to do.
Free space ~:D (of which ~:D might be needed for copying).~:[
Warning: You may require more space for copying than there is freespace, and gc may fail!~;
Ratio scavenging work//free space = ~3F~]"
                         (GET-DIRECT-GC-WORK-REMAINING) WORK FREE-SIZE COPYING
                         (PLUSP (- FREE-SIZE COPYING))
                       (// (FLOAT WORK) (- FREE-SIZE COPYING)))))))))
  (FORMAT STREAM "~&Scavenging during cons ~:[On~;Off~], Idle scavenging ~:[On~;Off~],~%"
          INHIBIT-SCAVENGING-FLAG INHIBIT-IDLE-SCAVENGING-FLAG)
  (FORMAT STREAM "Automatic garbage collection ~:[Off~;On~].~%"
          (ASSQ GC-PROCESS ACTIVE-PROCESSES))
  (FORMAT STREAM "GC Flip Ratio ~D, GC Reclaim Immediately ~:[Off~;On~]~%"
          GC-FLIP-RATIO GC-RECLAIM-IMMEDIATELY)
  (VALUES))

;;; This function gets rid of oldspace.
(DEFUN GC-RECLAIM-OLDSPACE ()
  "Finish scavenging all of oldspace right away, and discard it.
Does nothing if there is no oldspace.  Programs may call this
function every so often to say, /"If you must garbage collect
while running me, do so as a batch process/"."
  (WITH-LOCK (GC-FLIP-LOCK)
    (COND (GC-OLDSPACE-EXISTS
           ;; Make sure all regions are clean (no pointers to oldspace)
           (LET-GLOBALLY ((GC-BATCH-THIS-TIME T))
             (DO ((%SCAVENGER-WS-ENABLE 0))  ;Use all of memory as long as using all of processor
                 (%GC-FLIP-READY)                 ;Stop when scavenger says all is clean
               (%GC-SCAVENGE 10000)))
           ;; Execute certain forms after all is scavanged but before reclamation.
           ;; This is what processes weak links.
           (MAPC 'EVAL GC-BEFORE-RECLAIM-LIST)
           ;; Report oldspace statistics
           (COND (GC-REPORT-STREAM
                  (DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION))
                       (OLD-TOTAL-SIZE 0)
                       (OLD-USED-SIZE 0))
                      ((MINUSP REGION)
                       (GC-REPORT "GC: Flushing oldspace.  allocated=~D., used=~D."
                                  OLD-TOTAL-SIZE OLD-USED-SIZE))
                    (COND ((= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
                              %REGION-SPACE-OLD)
                           (SETQ OLD-TOTAL-SIZE (+ (%POINTER-UNSIGNED (REGION-LENGTH REGION))
                                                   OLD-TOTAL-SIZE)
                                 OLD-USED-SIZE (+ (%POINTER-UNSIGNED
                                                    (%REGION-FREE-POINTER REGION))
                                                  OLD-USED-SIZE)))))))
           ;; Discard oldspace
           (DOLIST (AREA (CURRENT-AREA-LIST))
             (LET ((AREA-NUMBER (SYMBOL-VALUE AREA)))
               (AND (OR (MINUSP AREA-NUMBER) ( AREA-NUMBER SIZE-OF-AREA-ARRAYS))
                    (FERROR NIL "Area-symbol ~S clobbered" AREA)) ;don't get grossly faked out
               (GC-RECLAIM-OLDSPACE-AREA AREA-NUMBER)))
           (SETQ GC-DAEMON-PAGE-CONS-ALARM -1)  ;Wake up daemon process
           (SETQ GC-OLDSPACE-EXISTS NIL)))))

;;; GC-RECLAIM-OLDSPACE-AREA - deletes all old-space regions of a specified area,
;;; unthreading from the lists, and returning the virtual memory to free.
;;; Call this for each area, if %GC-FLIP-READY is true and before calling %GC-FLIP.
;;; Note that if an area has only one oldspace region, we have a problem with
;;; losing the REGION-BITS.  For now just keep around one region.  This only
;;; happens when an area is completely disused.
(DEFUN GC-RECLAIM-OLDSPACE-AREA (AREA)
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
             "an area number")
  (WITHOUT-INTERRUPTS
    (OR %GC-FLIP-READY
        (FERROR NIL "You cannot reclaim oldspace now, there may be pointers to it"))
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))
         (REGION-TO-FREE)
         (PREV-REGION NIL REGION))
        (())
     NEXTLOOP                                   ;May GO here to avoid advancing DO variables
      (WHEN (MINUSP REGION) (RETURN NIL))
      (WHEN (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-OLD)
        ;--no longer true--
        ;; Free this region unless that would leave the area without any regions
        ;; at all, which would lose since there would be no place to remember its bits.
        ;; Before freeing, unthread from area's region list.
        (COND ((OR PREV-REGION (NOT (MINUSP (REGION-LIST-THREAD REGION))))
               (SETQ REGION-TO-FREE REGION
                     REGION (REGION-LIST-THREAD REGION))
               (IF PREV-REGION (STORE (REGION-LIST-THREAD PREV-REGION) REGION)
                 (STORE (AREA-REGION-LIST AREA) REGION))
               (%GC-FREE-REGION REGION-TO-FREE)
               (GO NEXTLOOP))
              (t
               ;; Force region to new space, and reset free,GC pointers.
               (change-region-to-new-space region)
               (setf (%region-free-pointer region) 0)
               (%gc-scav-reset region))))
      (AND (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-COPY)
           ;;; Change this region to NEW space so that it can be used for normal
           ;;; consing
           (change-region-to-new-space region)))))

(defun change-region-to-new-space (region)
  (setf (region-bits region)
        (%logdpb 0 %%region-scavenge-enable
                 (%logdpb %region-space-new %%region-space-type
                          (%logdpb 1 %%region-oldspace-meta-bit (region-bits region)))))
  (invalidate-region-mapping region))

(defun invalidate-region-mapping (region)
  (do ((ra 0 (+ ra page-size))
       (virtual-address))
      ((> ra (region-length region)))
    (setq virtual-address (%make-pointer-offset dtp-fix (region-origin region) ra))
    (%change-page-status virtual-address nil nil)))



;;;; GC Process

;;; This function runs in a separate process.  It wakes up when oldspace needs
;;; to be reclaimed and when a flip is required, and does them.
;;;*** Doesn't yet know about finite number of regions ***
(DEFUN GC-PROCESS ()
  (DO-FOREVER
    (UNLESS (OR GC-RECLAIM-IMMEDIATELY
                ;; Possibly reclaim now if not enough space left to do it incrementally.
                (AND GC-RECLAIM-IMMEDIATELY-IF-NECESSARY
                     (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
                         (GC-GET-COMMITTED-FREE-SPACE NIL)
                       (SETQ COMMITTED-FREE-SPACE
                             (FLOOR (* GC-FLIP-RATIO
                                       (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE))))
                       ( COMMITTED-FREE-SPACE FREE-SPACE))))
      ;; If incremental reclaim ok, wait until microcode gets thru with it.
      (PROCESS-WAIT "Await scavenge" #'SYMBOL-VALUE '%GC-FLIP-READY))
    ;; Then flush oldspace and print "flushing oldspace" message.  A complete
    ;; scavenge will take place here if %GC-FLIP-READY is NIL.
    (GC-RECLAIM-OLDSPACE)
    (DO-FOREVER                                 ;May iterate a few times before flipping
      (OR %GC-FLIP-READY (RETURN NIL))          ;Some other process must have flipped first
      (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
             (GC-GET-COMMITTED-FREE-SPACE)
        ;; Hook to let the user influence how conservative the garbage
        ;; collector will be.  GC-FLIP-RATIO may be a flonum.
        (SETQ COMMITTED-FREE-SPACE
              (FLOOR (* GC-FLIP-RATIO (+ COMMITTED-FREE-SPACE COMMITTED-FREE-SPACE-FUDGE))))
        (COND (( COMMITTED-FREE-SPACE FREE-SPACE)      ;Better flip now
               (WITH-LOCK (GC-FLIP-LOCK)
                 (WHEN %GC-FLIP-READY (GC-FLIP-NOW))
                 (RETURN)))
              (T                        ;Wait a while before flipping, then compute frob again
               (GC-REPORT "GC: Allowing ~D. words more consing before flip."
                          (- FREE-SPACE COMMITTED-FREE-SPACE))
               (SETQ GC-PAGE-CONS-ALARM-MARK
                     (+ %PAGE-CONS-ALARM
                        (TRUNCATE (- FREE-SPACE COMMITTED-FREE-SPACE) PAGE-SIZE)))
               (PROCESS-WAIT "Await need for flip"
                             #'(LAMBDA () (OR (NOT %GC-FLIP-READY)
                                              (> %PAGE-CONS-ALARM
                                                 GC-PAGE-CONS-ALARM-MARK))))))))))

(DEFVAR GC-ON NIL
  "Set to T or NIL by the system when automatic garbage collection is turned on or off.")

(DEFUN GC-ON ()
  "Turn on automatic garbage collection.
It is batch if SI:GC-RECLAIM-IMMEDIATELY is T, incremental if that is NIL."
  (PROG ((FLIP-RATIO
           (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)))
        (COND ((AND %GC-FLIP-READY
                    ( (* FLIP-RATIO (GC-GET-COMMITTED-FREE-SPACE))
                       (GET-FREE-SPACE-SIZE)))
               (FORMAT *QUERY-IO*
                       (IF (AND (NULL GC-RECLAIM-IMMEDIATELY)
                                (< (* FLIP-RATIO
                                      (GC-GET-COMMITTED-FREE-SPACE T))
                                   (GET-FREE-SPACE-SIZE)))
                           "~&There may not be enough free space for incremental garbage collection,
though it may still succeed depending on how much garbage there is,
and what your program does during the garbage collection.

There is certainly enough free space for a batch garbage collection
/(that is, with SI:GC-RECLAIM-IMMEDIATELY = T)."
                         "~&There is probably too little free space for a garbage collection.
Even a batch garbage collection (that is, SI:GC-RECLAIM-IMMEDIATELY = T)
is possible only if there is a lot of garbage; but it has a better chance."))
               (OR (Y-OR-N-P "Try garbage collecting after all? ")
                   (RETURN NIL))
               (IF (Y-OR-N-P "Set SI:GC-RECLAIM-IMMEDIATELY to get a better chance? ")
                   (SETQ GC-RECLAIM-IMMEDIATELY T))))
        (GC-MAYBE-SET-FLIP-READY)               ;if no oldspace regions
        (SEND GC-PROCESS :PRESET 'GC-PROCESS)
        (SETQ GC-ON T)
        (PROCESS-ENABLE GC-PROCESS)             ;Start flipper process
        (SETQ INHIBIT-SCAVENGING-FLAG NIL)      ;Enable scavenging during cons
        (ADD-INITIALIZATION "GC-PROCESS" '(GC-ON) '(:WARM))))   ;Do this on future warm boots

(DEFUN GC-OFF ()
  "Turn off automatic garbage collection."
  (WHEN (OR (NEQ (CAR GC-FLIP-LOCK) GC-PROCESS)
            (YES-OR-NO-P
              "The GC process is currently locking the GC lock.  Turn it off anyway? "))
    (DELETE-INITIALIZATION "GC-PROCESS" '(:WARM))       ;Don't start GC on warm boots anymore
    (PROCESS-DISABLE GC-PROCESS)                ;Disable flipper process
    (IF (EQ (CAR GC-FLIP-LOCK) GC-PROCESS)      ;Unlock the lock so user can SI:FULL-GC.
        (SETQ GC-FLIP-LOCK NIL))
    (SETQ INHIBIT-SCAVENGING-FLAG T)            ;Disable scavenging during cons
    (SETQ GC-ON NIL)))

;;; Set flip-ready if no oldspace anywhere.
(DEFUN GC-MAYBE-SET-FLIP-READY NIL
  (DO ((REGION (1- SIZE-OF-AREA-ARRAYS) (1- REGION)))
      ((MINUSP REGION)
       (WRITE-METER 'SYS:%COUNT-SCAVENGER-WORK 1_30.)
       (SETQ %GC-FLIP-READY T))                 ;No oldspace.
    (WHEN (= %REGION-SPACE-OLD (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)))
      (RETURN NIL))))                           ;really is some old space

(DEFUN MAKE-AREA-STATIC (AREA)
  "Make a dynamic area static.  Takes affect at next flip."
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
             "an area number")
  (PUSH `(MAKE-AREA-STATIC-INTERNAL ,AREA) GC-NEXT-FLIP-LIST)
  T)

(DEFUN MAKE-AREA-STATIC-INTERNAL (AREA)
  (WITHOUT-INTERRUPTS
    (LET ((BITS (AREA-REGION-BITS AREA)))
      (SELECT (LDB %%REGION-SPACE-TYPE BITS)
        (%REGION-SPACE-NEW
         (SETF (AREA-REGION-BITS AREA)
               (%LOGDPB 1 %%REGION-SCAVENGE-ENABLE
                        (%LOGDPB %REGION-SPACE-STATIC %%REGION-SPACE-TYPE BITS))))))
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION)))
        ((MINUSP REGION))
      (LET ((BITS (REGION-BITS REGION)))
        (SELECT (LDB %%REGION-SPACE-TYPE BITS)
          (%REGION-SPACE-NEW
           (SETF (REGION-BITS REGION)
                 (%LOGDPB 1 %%REGION-SCAVENGE-ENABLE
                          (%LOGDPB %REGION-SPACE-STATIC %%REGION-SPACE-TYPE BITS)))))))))

;;; Make a static area dynamic.  This can happen right away, although it really
;;; only takes effect on the next flip, when the area will acquire its first oldspace.
(DEFUN MAKE-AREA-DYNAMIC (AREA)
  "Make a static area dynamic.  It will be garbage collected starting at the next flip.
The area remains dynamic permanently unless you change it back.
To change it temporarily, use SI:CLEAN-UP-STATIC-AREA."
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
             "an area number")
  ;; Cancel any plans to make this area static at next flip,
  ;; for otherwise they would override what we do now.
  (SETQ GC-NEXT-FLIP-LIST (REMOVE `(MAKE-AREA-STATIC-INTERNAL ,AREA)
                                  GC-NEXT-FLIP-LIST))
  (WITHOUT-INTERRUPTS
    (LET ((BITS (AREA-REGION-BITS AREA)))
      (AND (= (LDB %%REGION-SPACE-TYPE BITS) %REGION-SPACE-STATIC)
           (SETF (AREA-REGION-BITS AREA)
                 (%LOGDPB %REGION-SPACE-NEW %%REGION-SPACE-TYPE BITS))))
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION)))
        ((MINUSP REGION))
      (LET ((BITS (REGION-BITS REGION)))
        (AND (= (LDB %%REGION-SPACE-TYPE BITS) %REGION-SPACE-STATIC)
             (SETF (REGION-BITS REGION)
                   (%LOGDPB %REGION-SPACE-NEW %%REGION-SPACE-TYPE BITS)))))))

(DEFUN AREA-STATIC-P (AREA)
  "Returns T if the specified area is currently or normally static.
Areas temporarily made dynamic by (SI:FULL-GC T) still count as static."
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
             "an area number")
  (OR (= (LDB %%REGION-SPACE-TYPE (AREA-REGION-BITS AREA))
         %REGION-SPACE-STATIC)
      (WITH-STACK-LIST (TEM 'MAKE-AREA-STATIC-INTERNAL AREA)
        (MEMBER tem GC-NEXT-FLIP-LIST))))

(DEFVAR AFTER-FULL-GC-INITIZIALIZATION-LIST NIL "Initializations performed after SI:FULL-GC.
But a non-NIL :NO-RECOPYING arg suppresses them.")
(DEFVAR FULL-GC-INITIZIALIZATION-LIST NIL "Initializations performed before SI:FULL-GC.
But a non-NIL :NO-TEMPORARY-AREAS arg suppresses them.")
(DEFVAR AFTER-FLIP-INITIALIZATION-LIST NIL "Initializations performed after every flip.")

(DEFCONST FIRST-FULL-GC-AREA INIT-LIST-AREA)

(DEFUN GC-IMMEDIATELY ()
  "Perform a complete garbage collection right away, running in this process.
It is not necessary to turn on automatic GC to use this function."
  (FULL-GC :NO-PRE-GC-INITIALIZATIONS T :NO-STATIC-REGIONS T :NO-RECOPYING T))

(DEFUN FULL-GC (&KEY NO-PRE-GC-INITIALIZATIONS NO-STATIC-REGIONS NO-RECOPYING DUPLICATE-PNAMES)
  "Do a complete batch-style garbage collection to make minimum size for saved band.
It is best to do this twice in a row to make sure that the used part of memory
is at the bottom of the address space.

If DUPLICATE-PNAMES is T, all equal pnames of symbols are collapsed.
This is only worth doing once for a given system version so it is off
by default.

Unless NO-STATIC-REGIONS is non-NIL, the existing full regions of
WORKING-STORAGE-AREA are marked as static when the GC is done.
Unless NO-RECOPYING is non-NIL, various structures are recopied after
the GC is done to make sure they are compact (to improve paging efficiency).
GC-IMMEDIATELY uses FULL-GC as a subroutine, supplying T for these three args."
  (WHEN ( (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
              (GC-GET-COMMITTED-FREE-SPACE T NIL T))
           (GET-FREE-SPACE-SIZE))
    (FORMAT *QUERY-IO* "~&There is probably not enough free space to garbage collect,
unless there is a lot of garbage to be freed.")
    (UNLESS (Y-OR-N-P "Try garbage collecting anyway? ")
      (RETURN-FROM FULL-GC NIL)))
  (UNLESS NO-PRE-GC-INITIALIZATIONS
    (INITIALIZATIONS 'FULL-GC-INITIALIZATION-LIST T))
  ;; For extra reduction in size of band, reset all temporary areas.
  ;; Do this first, since this may free up other things that they point to.
#|
  This causes lossage since things still point to them!
  (UNLESS NO-TEMPORARY-AREAS
    (DO ((AREA FIRST-FULL-GC-AREA (1+ AREA)))
        ((= AREA SIZE-OF-AREA-ARRAYS))
      (IF (AREA-TEMPORARY-P AREA)
          (RESET-TEMPORARY-AREA AREA))))
|#
  (WHEN DUPLICATE-PNAMES
    (COLLAPSE-DUPLICATE-PNAMES))
  (UNLESS NO-STATIC-REGIONS
    (MAKE-AREA-DYNAMIC WORKING-STORAGE-AREA))
  (WITH-LOCK (GC-FLIP-LOCK)
    (PROCESS-DISABLE GC-PROCESS)
    ;; We assume that incremental GC has not been being used,
    ;; so if oldspace exists, we are already after a (GC-FLIP-NOW).
    (OR GC-OLDSPACE-EXISTS (GC-FLIP-NOW))
    ;; Touch all interned symbols and their pnames,
    ;; to get them in a good order for paging.
    ;; This is really only necessary if NR-SYM and P-N-STRING are being GC'd.
    (LET (TEM)
      (DOLIST (P *ALL-PACKAGES*)
        (DO-LOCAL-SYMBOLS (SYM P)
          (SETQ TEM (LENGTH (SYMBOL-NAME SYM))))))
    (GC-RECLAIM-OLDSPACE)
    (UNLESS NO-RECOPYING
      (INITIALIZATIONS 'AFTER-FULL-GC-INITIALIZATION-LIST T))
    (UNLESS NO-STATIC-REGIONS
      (MAKE-AREA-REGIONS-STATIC WORKING-STORAGE-AREA))))

;(DEFVAR WORTHLESS-SYMBOL-AREA
;       (MAKE-AREA :NAME 'WORTHLESS-SYMBOL-AREA
;                  :REGION-SIZE #o200000
;                  :REPRESENTATION :STRUCTURE
;                  :GC :STATIC)
;  "Area that holds symbols which had no value, function definition or properties.")

;;;; Also moves symbols with no value, function defn or plist
;;;; into worthless-symbol-area.
;(DEFUN COLLAPSE-DUPLICATE-PNAMES ()
;  (LET ((TEM-PKG (OR (FIND-PACKAGE "GC-TEM")
;                    (MAKE-PACKAGE "GC-TEM" :USE () :SIZE 50000.)))
;       (SYMBOLS-MOVED 0)
;       (COUNT 0)
;       (TOTAL-SIZE 0))
;    (PAGE-IN-AREA NR-SYM)
;    (PAGE-IN-AREA P-N-STRING)
;    (MAPATOMS-NR-SYM
;      #'(LAMBDA (SYMBOL &AUX TEM)
;         ;; Collapse the pname if this is not the first symbol with this pname.
;         (WHEN (AND (SYMBOL-PACKAGE SYMBOL) ;Otherwise INTERN would side-effect the symbol.
;                    ( (%P-DATA-TYPE (SYMBOL-NAME SYMBOL)) DTP-HEADER-FORWARD))
;           (WHEN (AND (NEQ SYMBOL (SETQ TEM (INTERN-LOCAL SYMBOL TEM-PKG)))
;                      (NEQ (SYMBOL-NAME SYMBOL) (SYMBOL-NAME TEM)))
;             (LET ((%INHIBIT-READ-ONLY T))
;               (STRUCTURE-FORWARD (SYMBOL-NAME SYMBOL) (SYMBOL-NAME TEM)))
;             (INCF TOTAL-SIZE (%STRUCTURE-TOTAL-SIZE (SYMBOL-NAME TEM)))
;             (INCF COUNT 1)))
;         ;; Forward the symbol into worthless-symbol-area if desired.
;         (WHEN (AND ( (%P-DATA-TYPE SYMBOL) DTP-HEADER-FORWARD)
;                    ( (%AREA-NUMBER SYMBOL) WORTHLESS-SYMBOL-AREA)
;                    (OR (AND (NOT (FBOUNDP SYMBOL))
;                             (OR (NOT (BOUNDP SYMBOL))
;                                 (KEYWORDP SYMBOL))
;                             (NULL (PLIST SYMBOL)))
;                        (NULL (SYMBOL-PACKAGE SYMBOL))))
;           (LET ((NEW (LET ((DEFAULT-CONS-AREA WORTHLESS-SYMBOL-AREA))
;                        (MAKE-SYMBOL (SYMBOL-NAME SYMBOL)))))
;             (%BLT-TYPED SYMBOL NEW (%STRUCTURE-TOTAL-SIZE SYMBOL) 1)
;             (STRUCTURE-FORWARD SYMBOL NEW))
;           (INCF SYMBOLS-MOVED 1))))
;    (KILL-PACKAGE TEM-PKG)
;    (RETURN-STORAGE (PROG1 TEM-PKG (SETQ TEM-PKG NIL)))
;    (CLEAN-UP-STATIC-AREA PKG-AREA)
;    (CLEAN-UP-STATIC-AREA P-N-STRING)
;    (CLEAN-UP-STATIC-AREA NR-SYM)
;    (GC-REPORT "Collapsing duplicate pnames saved ~:D words, for ~:D pnames.
;~:D symbols moved to WORTHLESS-SYMBOL-AREA."
;              TOTAL-SIZE COUNT SYMBOLS-MOVED)))

;;; Used in a patch to system 93 to fix up free pointers munged by following function.
;(DEFUN FIX-AREA-FREE-POINTERS (AREA)
;  (WITHOUT-INTERRUPTS
;    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))) ((MINUSP REGION))
;       (LET ((BITS (REGION-BITS REGION)))
;         (WHEN (= (LDB %%REGION-SPACE-TYPE BITS) %REGION-SPACE-STATIC)
;           (SETF (REGION-FREE-POINTER REGION) (REGION-GC-POINTER REGION))
;           ;; And the following line was added for fixing up after a bug in FILL-UP-REGION.
;           (fill-up-region region))))))

(DEFUN MAKE-AREA-REGIONS-STATIC (AREA)
  "Mark the filled regions of AREA as static."
  (WITHOUT-INTERRUPTS
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION)))
        ((MINUSP REGION))
      (UNLESS (< (%REGION-FREE-POINTER REGION) %ADDRESS-SPACE-QUANTUM-SIZE)
        ;;; Would crash if region completyely empty;
        ;;; if nearly empty it's not worth making static.
        (DEALLOCATE-END-OF-REGION REGION)
        (FILL-UP-REGION REGION)                 ;Prevent further consing in this region.
        ;; Make it static.
        (LET ((BITS (REGION-BITS REGION)))
          (SELECT (LDB %%REGION-SPACE-TYPE BITS)
            ((%REGION-SPACE-NEW %REGION-SPACE-COPY)
             (SETF (REGION-BITS REGION)
                   (%LOGDPB 1 %%REGION-SCAVENGE-ENABLE
                            (%LOGDPB %REGION-SPACE-STATIC %%REGION-SPACE-TYPE BITS))))))))))

(DEFVAR FILL-UP-REGION-ARRAY (MAKE-ARRAY 0 :TYPE ART-32B))

(DEFUN FILL-UP-REGION (REGION)
  "Cons garbage in region REGION until it is exactly full."
  (WITHOUT-INTERRUPTS
    (LET ((FREE-POINTER (%REGION-FREE-POINTER REGION))
          (ORIGIN (REGION-ORIGIN REGION))
          (SIZE (REGION-LENGTH REGION))
          (BITS (REGION-BITS REGION)))
      (UNLESS (= SIZE FREE-POINTER)  ;It's already full?
        (IF (= (%LOGLDB %%REGION-REPRESENTATION-TYPE BITS) %REGION-REPRESENTATION-TYPE-LIST)
            ;; Fill all words of list region with (NIL).
            (PROGN (%P-STORE-CONTENTS-OFFSET NIL ORIGIN FREE-POINTER)
                   (%P-DPB-OFFSET CDR-NIL %%Q-CDR-CODE ORIGIN FREE-POINTER)
                   (UNLESS (= SIZE (1+ FREE-POINTER))
                     (%BLT (%MAKE-POINTER-OFFSET DTP-FIX ORIGIN FREE-POINTER)
                           (%MAKE-POINTER-OFFSET DTP-FIX ORIGIN (1+ FREE-POINTER))
                           (- SIZE FREE-POINTER 1) 1)))
          ;; Fill structure region up to page boundary with zero-length arrays.
          (DO ((I FREE-POINTER (1+ I)))
              ((= (\ I PAGE-SIZE) 0))
            (%BLT FILL-UP-REGION-ARRAY (%MAKE-POINTER-OFFSET DTP-FIX ORIGIN I) 1 1))
          ;; Fill remaining pages with page-long arrays (faster to scavenge).
          (DO ((I (* (CEILING FREE-POINTER PAGE-SIZE) PAGE-SIZE)
                  (+ I PAGE-SIZE)))
              ((= I SIZE))
            (%BLT FILL-UP-REGION-ARRAY (%MAKE-POINTER-OFFSET DTP-FIX ORIGIN I) 1 1)
            (%P-DPB-OFFSET (1- PAGE-SIZE) %%ARRAY-INDEX-LENGTH-IF-SHORT ORIGIN I)))
        (SETF (%REGION-FREE-POINTER REGION) SIZE)))))

(DEFUN DEALLOCATE-END-OF-REGION (REGION)
  "Return as much as possible of unused part of region REGION to free pool.
It can then be allocated into other regions."
  (WITHOUT-INTERRUPTS
    (LET* ((FREE-POINTER (%REGION-FREE-POINTER REGION))
           (ORIGIN (REGION-ORIGIN REGION))
           (SIZE (REGION-LENGTH REGION))
           (N-QUANTA (TRUNCATE SIZE %ADDRESS-SPACE-QUANTUM-SIZE))
           ;; MAX 1 below is so we don't truncate a region to zero length.
           ;; I suspect that that causes crashes.
           (FIRST-FREE-QUANTUM
             (MIN N-QUANTA (MAX 1 (CEILING FREE-POINTER %ADDRESS-SPACE-QUANTUM-SIZE)))))
      (UNLESS (= FIRST-FREE-QUANTUM N-QUANTA)   ;Less than one quantum is free.
        (DO ((I FIRST-FREE-QUANTUM (1+ I))
             ;; (truncate origin %address-space-quantum-size), as unsigned number.
             (ORIGIN-QUANTUM (LSH ORIGIN (- 1 (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE)))))
            ((= I N-QUANTA))
          (SETF (AREF #'ADDRESS-SPACE-MAP (+ ORIGIN-QUANTUM I)) 0))
        (SETF (REGION-LENGTH REGION) (* FIRST-FREE-QUANTUM %ADDRESS-SPACE-QUANTUM-SIZE))
        (DEALLOCATE-PAGES (%MAKE-POINTER-OFFSET DTP-FIX ORIGIN
                                                (REGION-LENGTH REGION))
                          (TRUNCATE (%POINTER-DIFFERENCE SIZE (REGION-LENGTH REGION))
                                    PAGE-SIZE))))
    (invalidate-region-mapping region)))

;;; For N-PAGES pages starting at BASE-ADDR, mark them properly not in core.
(DEFUN DEALLOCATE-PAGES (BASE-ADDR N-PAGES)
  (PAGE-IN-STRUCTURE #'DEALLOCATE-PAGES)        ;Make sure entire fef is in core.
  (DO ((I 0 (1+ I))
       (ADDRESS BASE-ADDR (%MAKE-POINTER-OFFSET DTP-FIX ADDRESS PAGE-SIZE)))
      ((= I N-PAGES))
    ;; Search for this page in the page hash table.
    (DO ((PHTX (%COMPUTE-PAGE-HASH ADDRESS) (+ PHTX 2))
         (PHT-LIMIT (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE))
         (PHT1))
        (())
      (AND ( PHTX PHT-LIMIT) (SETQ PHTX (- PHTX PHT-LIMIT)))
      (LET ((%%VALID-BIT %%PHT1-VALID-BIT)
            (%DUMMY-VIRTUAL-ADDRESS %PHT-DUMMY-VIRTUAL-ADDRESS)
            (%%VIRTUAL-PAGE-NUMBER %%PHT1-VIRTUAL-PAGE-NUMBER)
            (%SWAP-STATUS-FLUSHABLE %PHT-SWAP-STATUS-FLUSHABLE)
            (%%SWAP-STATUS-CODE %%PHT1-SWAP-STATUS-CODE)
            (%%MODIFIED-BIT %%PHT1-MODIFIED-BIT)
            (%MAP-STATUS-READ-ONLY %PHT-MAP-STATUS-READ-ONLY)
            (%%MAP-STATUS-CODE %%PHT2-MAP-STATUS-CODE)
            (%%ACCESS-STATUS-AND-META-BITS %%PHT2-ACCESS-STATUS-AND-META-BITS))
        ;; Above locals used to avoid references to specials in the following.
        ;; Thus there is no chance of a page fault here.
        (SETQ PHT1 (PAGE-TABLE-AREA PHTX))
        (COND ((NOT (LDB-TEST %%VALID-BIT PHT1)) (RETURN NIL))  ;Not found
              ((= (LDB %%VIRTUAL-PAGE-NUMBER PHT1)
                  (LSH ADDRESS -10))            ;Address match
               (SETF (PAGE-TABLE-AREA PHTX)
                     (%LOGDPB %DUMMY-VIRTUAL-ADDRESS %%VIRTUAL-PAGE-NUMBER
                              (%LOGDPB %SWAP-STATUS-FLUSHABLE
                                       %%SWAP-STATUS-CODE
                                       (%LOGDPB 0 %%MODIFIED-BIT PHT1))))
               (SETF (PAGE-TABLE-AREA (1+ PHTX))
                     (%LOGDPB %MAP-STATUS-READ-ONLY %%MAP-STATUS-CODE
                              (%LOGDPB 0 %%ACCESS-STATUS-AND-META-BITS
                                       (PAGE-TABLE-AREA (1+ PHTX)))))
               (RETURN NIL)))))))


(DEFUN AREA-NUMBER-STATIC-P (AREA-NUMBER)
  "Return NIL if area-number is not a static area, else return AREA-NUMBER."
  ;;stolen from clean-up-static-area
  (AND (NUMBERP AREA-NUMBER)
       ( AREA-NUMBER 0)
       ( AREA-NUMBER SIZE-OF-AREA-ARRAYS)
       (AREA-STATIC-P AREA-NUMBER)
       (NOT (AREA-TEMPORARY-P AREA-NUMBER))
       AREA-NUMBER))

(DEFUN CLEAN-UP-STATIC-AREA (AREA)
  "Make a static area dynamic temporarily, for the next flip only.
AREA-STATIC-P will continue to call this area a static area."
  (CHECK-ARG AREA (AREA-NUMBER-STATIC-P AREA) "the area number of a static area")
  (PUSH `(MAKE-AREA-STATIC-INTERNAL ,AREA) GC-SECOND-NEXT-FLIP-LIST)
  (MAKE-AREA-DYNAMIC AREA))

;;This cannot work and would cause bad nonlocality anyway -- RMS.
;(DEFUN CLEAN-UP-ALL-STATIC-AREAS ()
;  "Marks all areas to be cleaned up the next time a flip is done."
;  (DOTIMES (I SIZE-OF-AREA-ARRAYS)
;    (IF (AREA-NUMBER-STATIC-P I) (CLEAN-UP-STATIC-AREA I))))

;;; Find boundary in physical core for scavenger working set.  Scan up until right number
;;; of non-wired pages passed.
(DEFUN SET-SCAVENGER-WS (WS-SIZE)
  "Set the number of pages to be used for scavenging.  NIL means no limit."
  (DO ((PHYS-ADR (- (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE) PAGE-SIZE)
                 (- PHYS-ADR PAGE-SIZE))
       (PAGES-FOUND 0))
      ((OR (ZEROP PHYS-ADR) (>= PAGES-FOUND WS-SIZE))
       (SETQ GC-SCAVENGER-WS-SIZE WS-SIZE
             %SCAVENGER-WS-ENABLE PHYS-ADR))
    (LET ((PPD-ADR (+ (REGION-ORIGIN PHYSICAL-PAGE-DATA)
                      (TRUNCATE PHYS-ADR PAGE-SIZE))))
      (IF (NOT (AND (= (%P-LDB #o0020 PPD-ADR) #o177777)        ;flush if fixed wired
                    ( (%P-LDB #o2020 PPD-ADR) #o177777)))
          (LET ((PHT-ADR (+ (%P-LDB #o0020 PPD-ADR) (REGION-ORIGIN PAGE-TABLE-AREA))))
            (IF (NOT
                  (AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT PHT-ADR)))
                       (= (%P-LDB %%PHT1-SWAP-STATUS-CODE PHT-ADR) %PHT-SWAP-STATUS-WIRED)))
                (SETQ PAGES-FOUND (1+ PAGES-FOUND))))))))

(DEFUN SET-SWAP-RECOMMENDATIONS-OF-AREA (AREA SWAP-RECOMMENDATIONS)
  "Set the number of pages to be swapped in at once in AREA."
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
             "an area number")
  (WITHOUT-INTERRUPTS
    (STORE (SYS:AREA-REGION-BITS AREA)
           (%LOGDPB SWAP-RECOMMENDATIONS %%REGION-SWAPIN-QUANTUM (SYS:AREA-REGION-BITS AREA)))
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION)))
        ((MINUSP REGION))
      (STORE (REGION-BITS REGION)
             (%LOGDPB SWAP-RECOMMENDATIONS %%REGION-SWAPIN-QUANTUM (REGION-BITS REGION))))))

(DEFUN AREA-SWAP-RECOMMENDATIONS (AREA)
  (%LOGLDB %%REGION-SWAPIN-QUANTUM (SYS:AREA-REGION-BITS AREA)))

(DEFUN CHECK-SWAP-RECOMMENDATIONS-OF-AREA (AREA)
  (LET ((SWAP-RECOMMENDATIONS NIL)
        (FIRST-REGION NIL))
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION)))
        ((MINUSP REGION))
      (COND ((NULL SWAP-RECOMMENDATIONS)
             (SETQ SWAP-RECOMMENDATIONS
                   (%LOGLDB %%REGION-SWAPIN-QUANTUM (REGION-BITS REGION))
                   FIRST-REGION REGION))
            ((NOT (= (%LOGLDB %%REGION-SWAPIN-QUANTUM (REGION-BITS REGION))
                     SWAP-RECOMMENDATIONS))
             (FORMAT T "~%region ~S has swapin quantum ~s but regions ~s has ~s."
                     FIRST-REGION
                     SWAP-RECOMMENDATIONS
                     REGION
                     (%LOGLDB %%REGION-SWAPIN-QUANTUM (REGION-BITS REGION))))))))

(DEFVAR LAST-ALL-AREAS-SWAP-RECOMMENDATIONS 0)

(DEFUN SET-ALL-SWAP-RECOMMENDATIONS (N &OPTIONAL REALLY-ALL)
  "Set all areas to swap in N+1 pages at a time."
  (DOLIST (NAME-OF-AREA (CURRENT-AREA-LIST))
    (IF (OR REALLY-ALL (= (AREA-SWAP-RECOMMENDATIONS (SYMEVAL NAME-OF-AREA))
                          LAST-ALL-AREAS-SWAP-RECOMMENDATIONS))
        (SET-SWAP-RECOMMENDATIONS-OF-AREA (SYMEVAL NAME-OF-AREA) N)))
  (SETQ LAST-ALL-AREAS-SWAP-RECOMMENDATIONS N))

(DEFCONST MEMORY-SIZE-SWAP-RECOMMENDATION-ALIST
          '((320. . 2) (384. . 2) (448. . 3) (512. . 4) (576. . 5) (640. . 5) (704. . 6)
            (768. . 6) (832. . 7) (896. . 7) (960. . 8) (1024. . 8))
  "Alist of memory size in K versus default area swap recommendation in pages.
Note that number of pages swapped is one greater than the recommendation.")

(DEFUN DEFAULT-SWAP-RECOMMENDATIONS ()
  "Set all area swap recommendations to the default value for this machine's memory size."
  (SELECT-PROCESSOR
    (:CADR
      (LET ((TEM (ASSQ (FLOOR (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE) #o2000)
                       MEMORY-SIZE-SWAP-RECOMMENDATION-ALIST)))
        (AND TEM (SET-ALL-SWAP-RECOMMENDATIONS (CDR TEM)))))
    (:LAMBDA
      ;; Interphase/Eagle/SDU setup has good latency characteristics but low bandwidth.
      (SET-ALL-SWAP-RECOMMENDATIONS 2))))

(ADD-INITIALIZATION "DEFAULT-SWAP-RECOMMENDATIONS" '(DEFAULT-SWAP-RECOMMENDATIONS) '(:COLD))

;;;; GC-Daemon facility.

;;; A GC-daemon is a set of address-space conditions to wait for, and a
;;; function to run (in a separate process) when conditions are met.

;;; This simple process implements the queue
(DEFVAR GC-DAEMON-PROCESS)

;;; Each element on this queue is a list at least four long:
;;;     (name function region-cons-alarm page-cons-alarm)
;;; If either alarm is  the value in the queue, the function is called
;;; in a background process with the queue element as its argument.
;;; If any oldspace is reclaimed, all entries on the queue go off, since the
;;; allocation of address space has just changed.  This may need improvement
;;; in the future, when oldspace reclamation is more frequent.
(DEFVAR GC-DAEMON-QUEUE NIL)

(DEFVAR GC-DAEMON-PAGE-CONS-ALARM 0)
(DEFVAR GC-DAEMON-REGION-CONS-ALARM 0)

;;; Add to the queue.  Arguments are how many more regions and pages
;;; must be consed before the function goes off.  If you want your
;;; queue element to be more than four long, pre-create it and pass it in
(DEFUN GC-DAEMON-QUEUE (NAME FUNCTION N-REGIONS N-PAGES &OPTIONAL ELEM)
  (OR ELEM (SETQ ELEM (ASSQ NAME GC-DAEMON-QUEUE)) (SETQ ELEM (LIST NAME FUNCTION NIL NIL)))
  (WITHOUT-INTERRUPTS
    (SETF (THIRD ELEM) (+ %REGION-CONS-ALARM N-REGIONS))
    (SETF (FOURTH ELEM) (+ %PAGE-CONS-ALARM N-PAGES))
    (OR (MEMQ ELEM GC-DAEMON-QUEUE)
        (PUSH ELEM GC-DAEMON-QUEUE))
    (SETQ GC-DAEMON-PAGE-CONS-ALARM -1)))       ;Wake up daemon process

;;; This is the function that runs in the scheduler
(DEFUN GC-DAEMON-FUNCTION ()
  ;; Fire off any interesting queue entries
  (LOOP FOR ELEM IN GC-DAEMON-QUEUE
        WHEN (OR ( %REGION-CONS-ALARM (THIRD ELEM))
                 ( %PAGE-CONS-ALARM (FOURTH ELEM)))
          DO (SETQ GC-DAEMON-QUEUE (DELQ ELEM GC-DAEMON-QUEUE))
             (PROCESS-RUN-FUNCTION (STRING (FIRST ELEM)) (SECOND ELEM) ELEM))
  ;; Cause process to sleep until next interesting time
  (IF GC-DAEMON-QUEUE
      (SETQ GC-DAEMON-REGION-CONS-ALARM (LOOP FOR ELEM IN GC-DAEMON-QUEUE
                                              MINIMIZE (THIRD ELEM))
            GC-DAEMON-PAGE-CONS-ALARM (LOOP FOR ELEM IN GC-DAEMON-QUEUE
                                            MINIMIZE (FOURTH ELEM)))
      (SETQ GC-DAEMON-REGION-CONS-ALARM MOST-POSITIVE-FIXNUM
            GC-DAEMON-PAGE-CONS-ALARM MOST-POSITIVE-FIXNUM))
  (SET-PROCESS-WAIT CURRENT-PROCESS
                    #'(LAMBDA ()
                        (OR ( %REGION-CONS-ALARM GC-DAEMON-REGION-CONS-ALARM)
                            ( %PAGE-CONS-ALARM GC-DAEMON-PAGE-CONS-ALARM)))
                    NIL)
  (SETF (PROCESS-WHOSTATE CURRENT-PROCESS) "GC Daemon"))

(DEFUN START-GC-DAEMON ()
  (OR (BOUNDP 'GC-DAEMON-PROCESS)
      (SETQ GC-DAEMON-PROCESS (MAKE-PROCESS "GC Daemon"
                                :SIMPLE-P T
                                :WARM-BOOT-ACTION 'GC-DAEMON-RESTART)))
  (SEND GC-DAEMON-PROCESS :PRESET 'GC-DAEMON-FUNCTION)
  (SEND GC-DAEMON-PROCESS :RUN-REASON 'START-GC-DAEMON))

(DEFUN GC-DAEMON-RESTART (P)
  ;; %REGION-CONS-ALARM and %PAGE-CONS-ALARM have changed unpredictably
  ;; so schedule all gc-daemons to go off almost immediately
  (DOLIST (ELEM GC-DAEMON-QUEUE)
    (GC-DAEMON-QUEUE (FIRST ELEM) (SECOND ELEM) 1 1 ELEM))
  (PROCESS-WARM-BOOT-DELAYED-RESTART P))

(START-GC-DAEMON)

;;;; GC-daemon which watches for exhaustion of address space

;;; Controlling parameters:
;;; Amount of free space at which to start complaining, fraction by which to go down
(DEFCONST ADDRESS-SPACE-WARNING-LOW-WORDS 1000000.)
(DEFCONST ADDRESS-SPACE-WARNING-LOW-REGIONS 50.)
(DEFCONST ADDRESS-SPACE-WARNING-WORDS-RATIO 0.75)
(DEFCONST ADDRESS-SPACE-WARNING-REGIONS-RATIO 0.75)
;; These two are where it last notified the user
(DEFVAR ADDRESS-SPACE-WARNING-WORDS NIL)
(DEFVAR ADDRESS-SPACE-WARNING-REGIONS NIL)

(DEFCONST ADDRESS-SPACE-WARNING-GC-OFF-WORDS 200000.)
(DEFCONST ADDRESS-SPACE-WARNING-GC-OFF-REGIONS 10.)

(DEFUN ADDRESS-SPACE-WARNING (ELEM &AUX (COMPLAIN NIL))
  ;; What is our status now?
  (LET ((FREE-WORDS (GET-FREE-SPACE-SIZE))
        (FREE-REGIONS
          (LOOP FOR REGION = (SYSTEM-COMMUNICATION-AREA %SYS-COM-FREE-REGION#-LIST)
                           THEN (REGION-LIST-THREAD REGION)
                UNTIL (MINUSP REGION)
                COUNT T)))
    ;; What do we do about it?
    (COND ((AND ( FREE-WORDS ADDRESS-SPACE-WARNING-LOW-WORDS)
                ( FREE-REGIONS ADDRESS-SPACE-WARNING-LOW-REGIONS))
           ;; No need to complain at all, reset everything
           (SETQ ADDRESS-SPACE-WARNING-WORDS ADDRESS-SPACE-WARNING-LOW-WORDS)
           (SETQ ADDRESS-SPACE-WARNING-REGIONS ADDRESS-SPACE-WARNING-LOW-REGIONS))
          ((OR (< FREE-WORDS
                  (* ADDRESS-SPACE-WARNING-WORDS ADDRESS-SPACE-WARNING-WORDS-RATIO))
               (< FREE-REGIONS
                  (* ADDRESS-SPACE-WARNING-REGIONS ADDRESS-SPACE-WARNING-REGIONS-RATIO)))
           ;; Time to complain again, space significantly lower than last time
           (SETQ COMPLAIN '<
                 ADDRESS-SPACE-WARNING-WORDS FREE-WORDS
                 ADDRESS-SPACE-WARNING-REGIONS FREE-REGIONS))
          ((AND (> FREE-REGIONS
                   (FLOOR ADDRESS-SPACE-WARNING-REGIONS ADDRESS-SPACE-WARNING-REGIONS-RATIO))
                (> FREE-WORDS
                   (FLOOR ADDRESS-SPACE-WARNING-WORDS ADDRESS-SPACE-WARNING-WORDS-RATIO)))
           ;; Significantly more space than there was before, let user know
           (SETQ COMPLAIN '>
                 ADDRESS-SPACE-WARNING-WORDS FREE-WORDS
                 ADDRESS-SPACE-WARNING-REGIONS FREE-REGIONS)))
    ;; If suppose to complain, do so
    (AND COMPLAIN
         (PROCESS-RUN-FUNCTION
           "GC Notification" #'TV:NOTIFY NIL
           "~:[Address space low!  ~]You have ~D regions and ~DK words of address space left"
           (EQ COMPLAIN '>) FREE-REGIONS (TRUNCATE FREE-WORDS 1024.)))
    (WHEN (AND (OR (< FREE-WORDS ADDRESS-SPACE-WARNING-GC-OFF-WORDS)
                   (< FREE-REGIONS ADDRESS-SPACE-WARNING-GC-OFF-REGIONS))
               (SEND GC-PROCESS :ACTIVE-P))
      (GC-OFF)
      (PROCESS-RUN-FUNCTION "GC Notification"
                            #'TV:NOTIFY NIL "Turning off GC to avoid a crash."))
    ;; Re-queue self
    (GC-DAEMON-QUEUE 'ADDRESS-SPACE-WARNING 'ADDRESS-SPACE-WARNING
                     (- FREE-REGIONS
                        (MIN ADDRESS-SPACE-WARNING-LOW-REGIONS
                             (FLOOR (* FREE-REGIONS ADDRESS-SPACE-WARNING-REGIONS-RATIO))))
                     (TRUNCATE (- FREE-WORDS
                                  (MIN ADDRESS-SPACE-WARNING-LOW-WORDS
                                       (FLOOR (* FREE-WORDS ADDRESS-SPACE-WARNING-WORDS-RATIO))))
                               PAGE-SIZE)
                     ELEM)))

;;; Start
(GC-DAEMON-QUEUE 'ADDRESS-SPACE-WARNING 'ADDRESS-SPACE-WARNING 0 0)

;;; Warn user if he is about to have too little space left to GC.
(DEFVAR GC-TOO-LATE-WARNING-GIVEN NIL
  "T if the user has already been told he has barely enough free space to GC in.")

(DEFUN GC-TOO-LATE-WARNING (ELEM)
  ;; What is our status now?
  (LET ((BATCH-WARNING-FLAG     ;T means warn about batch gc, NIL means warn about incremental
          (OR GC-TOO-LATE-WARNING-GIVEN
              GC-RECLAIM-IMMEDIATELY
              GC-RECLAIM-IMMEDIATELY-IF-NECESSARY)))
    (COND ((AND %GC-FLIP-READY
                (FBOUNDP 'NUMERIC-ONE-ARG)      ;Needed for something we call (I wonder what).
                (NEQ GC-TOO-LATE-WARNING-GIVEN 'BATCH)
                (NOT (SEND GC-PROCESS :ACTIVE-P))
                ( (+ (* (OR GC-FLIP-MINIMUM-RATIO GC-FLIP-RATIO)
                         (GC-GET-COMMITTED-FREE-SPACE BATCH-WARNING-FLAG))
                      COMMITTED-FREE-SPACE-FUDGE)
                   (GET-FREE-SPACE-SIZE)))
           (PROCESS-RUN-FUNCTION
             "GC Notification" #'TV:NOTIFY NIL
             (IF BATCH-WARNING-FLAG
                 "It will soon be too late to start even a batch garbage collection
unless a lot of your consing has been garbage.
Do (GC-STATUS) for more information."
               "It is nearly too late to start incremental garbage collection,
unless a lot of your consing has been garbage.
Batch garbage collection (GC-IMMEDIATELY) will remain possible for a while.
Do (GC-STATUS) for more information."))
           (SETQ GC-TOO-LATE-WARNING-GIVEN
                 (IF BATCH-WARNING-FLAG
                     'BATCH T))))
    ;; Re-queue self
    (GC-DAEMON-QUEUE 'GC-TOO-LATE-WARNING 'GC-TOO-LATE-WARNING
                     100000. (FLOOR COMMITTED-FREE-SPACE-FUDGE 2)
                     ELEM)))

;;;; Start
(GC-DAEMON-QUEUE 'GC-TOO-LATE-WARNING 'GC-TOO-LATE-WARNING 0 0)

;;;; Peek display

;;;; Hash arrays

;;;; Weak links

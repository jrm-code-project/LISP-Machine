;;; -*- Mode: Lisp; Base: 10.; Package: Moby-File-System; Readtable: T -*-
;;;; Basic Structures

(defconstant dtp-moby-eval dtp-symbol-header)  ;see discussion in MOBY-INIT.

;REGION-MOBY-BITS-ARRAY, REGION-NAMESPACE-ORIGIN, AND REGION-SPARE CREATED
; art-inum in system 116.  Should be ART-Q.
(defun fix-region-arrays ()
  (dolist (area-symbol 'sys:(region-moby-bits-array region-namespace-origin region-spare))
    (cond ((= (%p-ldb si:%%array-type-field (symbol-function area-symbol))
              (ldb si:%%array-type-field art-inum))
           (%p-dpb (ldb si:%%array-type-field art-q)
                   si:%%array-type-field
                   (symbol-function area-symbol))
           (dotimes (r si:number-of-regions)
             (setf (aref (symbol-function area-symbol) r) nil))))))


;(DEFSTRUCT (MOBY-DIRECTORY (:TYPE :NAMED-ARRAY)
;                     :CONC-NAME
;                     (:ALTERANT NIL)
;                     (:MAKE-ARRAY (:AREA MOBY-FILE-SYSTEM-AREA)))
;  NAME                                         ;The name of the directory
;  LOCK                                         ;The lock associated with the directory
;  DATA-POINTER                                 ;Not used for directory
;  FILES                                        ;The files specified in the map.
;  )

;this defstruct more or less corresponds to the LISPM AREA in MOBY form.
; "made" by moby-make-section-map.
(defstruct (section-map (:type :array-leader) (:conc-name sm-))
  fill-pointer
  code                  ;code 2 is section-map.
  area-name-string      ; for debugging, sort of.  Cannot be a symbol because symbol
                        ; handling mechanism not bootstrapped.
  area-moby-options     ;see below.
  area-region-bits      ;same as region-bits in QCOM
  area-region-size
  locally-mapped-area   ;NIL for none, otherwise a local area number.  THIS IS A LOCAL CELL.
  section-map-section)  ;a section map can only be pointed at by one mfile- frob, so
                        ; this is ok and represents useful cross-checking.  Valid only on local hosts.

(defconst %%area-moby-option-double-mapped (byte 1 0))
(defconst %%area-moby-option-write-once (byte 1 1))  ;distribute data from this area
        ;without dereconciling it locally.  Used for root;package-root.mby .

(defun moby-options-from-options (options)
  (do ((opt options (cddr opt))
       (ans 0))
      ((null opt) ans)
    (selectq (car opt)
      (:double-mapped (setq ans (dpb 1 %%area-moby-option-double-mapped ans)))
      (:write-once (setq ans (dpb 1 %%area-moby-option-write-once ans))))))

(defstruct (region-map (:type :array-leader) (:conc-name rm-))
  fill-pointer                  ;effectively highest page which has associated dataspace.
  free-pointer                  ;Qs
  size                          ;Qs
  namespace-page-origin         ;moby-namespace-page-number
  consing-host           ;NIL for not committed.
        ;otherwise a fixnum index to *moby-host-index*.  THIS IS A LOCAL CELL.
  locally-mapped-region) ;NIL for none, otherwise a local-region number.  THIS IS A LOCAL CELL.

;exists per area, including partition-header and root-mapping areas.
; eventually, this info probably will be stored directly with the area.
; The MSA basically holds "transient" information which is recreated per BOOT.
; (or, in other words, it is NOT stored in moby-space).  See extensive discussion in MOBY-INIT.
(defstruct (msa-element (:type :named-array) (:conc-name msa-))
  kind          ;partition-header, root-mapping, root, or file
  section     ;for debugging only, probably will be flushed ..
                ;may be nil for root area, otherwise mfile- structure in primary host.
                ; for remote hosts, this has the truename as a pathname object.
  area          ;associated area
  region-list ;For debugging only. Use si:for-every-region-in-area instead of this.
  partition-header
  map   ;section map in root-section for primary host.
        ;for remote hosts, its a locally generated copy of the regions I know.
  map-moby-handle ;moby address of msa-map in primary host, as a bignum (or NIL).
        ;  This is the official "handle" for identifying this map (and thus this file,
        ;  etc) in remote computers.
  mpa   ;mpa defstruct, which is per partition.  Among other things, the mpa defstruct
        ; has the dataspace offset to use for this partition.
  remote-hosts  ;list of remote hosts that know about this section
  primary-host-object ;or NIL if I am primary host.  See discussion in MOBY-INIT.
  host-index    ;of host we reference this thru.
  )

;moby-partition-alist element is:
(defstruct (mpa-element (:type :array) (:conc-name mpa-))
  dataspace-origin              ;dataspace-page-with-offset
  dataspace-highest-+1          ;dataspace-page-with-offset
  physical-unit
  partition-origin
  partition-length

  partition-header
  mapping-code          ;  qs-per-lispm-q
  partition-header-area
  root-mapping-area
  root-area
                                ;array address by usage array element.
  single-mapped-dataspace-usage-occurance-counts ; totals number of tracks with this usage count.
  double-mapped-dataspace-usage-occurance-counts
  local-host-index
  )

(defstruct (usage-count-array (:type :array-leader) (:conc-name uc-))
  fill-pointer          ;constant for checking and consistancy.
  lock
  base-dataspace-page
  ascending-count
  timestamp
  scan-pointer
  )
;The following defstruct is stored starting in word 0 of a moby partition.  Reading this,
; you need to be able to bootstrap everything else!

(defstruct (partition-header (:type :array) (:conc-name ph-))
  ;array-header word is word 0.
          code          ;primitive named-structure-symbol.  code 1 is partition-header.
          mapping-code     ;code with specifies how everything else is decoded.
                           ; (dispatches occur via vectors index of this at various points.)
                           ;note this is first word and a fixnum so it looks the same regardless.
                           ; Qs-per-lispm-Q is implied in this as well as a lot of other stuff.
          magic-number          ;funny number *moby-partition-header-magic-number*

;the following 2 qs constitute the "partition-header-namespace-map".  Once allocated, there is no
;  need for this ever to grow.  For bootstrapping purposes, logically redundant with
;  partition-header-section-map below.
          partition-header-low-namespace-page
          partition-header-high-namespace-page
          partition-header-free-pointer ;This lets us know how much to "wipe" during bootstrapping
                        ;before various maps available.

          partition-header-section-map  ;stored in partition-header following this header.
                ;note that this can only be referenced after you are already partly bootstrapped.
          root-mapping-section-map      ;stored in partition-header following this header
          root-section-map              ;stored in root-mapping-section

          namespace-allocation-structure  ;stored in root-mapping-section
          dataspace-length-in-pages     ;ie number blocks of dataspace in this partition
          number-double-mapped-blocks   ;of these, number stored twice within partition.
          double-mapped-usage-count-array   ;stored in root-mapping-section
          single-mapped-usage-count-array   ;stored in root-mapping-section

          root-section-directory-header ; Points to main section directory.  This is the root for
                                        ;   finding sections, files and things.
          partition-host-name    ;string also stored in partition-header-area.  Having this
        ;available simplifies bootstrapping, since correct area names can be gotten.
          spare1
          spare2
        )

;partition-header
;  fixed allocated, does not grow
; "partition-header-namespace-map" (nonstandard storage) (dataspace is direct mapped to low pages)
; N double-tracked blocks
;
; root-mapping-section-map      (fixed allocated at partition initialization)
; root-mapping-section-region-map (s)

;root-mapping-section
; namespace-allocation-structure
; double mapped usage table
; single mapped usage table
;  root-section-map  (allocated "sufficently large". Theoretically could be reCONSED).
;  root-section-region-map (s)   consed as necessary.

;root-section
; root-section-directory-header
; all other directory info ...



(defstruct (namespace-allocation-structure (:type :array) (:conc-name na-))
  allocation-pointer    ;moby-page-number
  allocation-limit      ;moby-page-number
  allocation-alist      ; element is (moby-page-number number-of-pages) ever allocated to
                        ; this section.
  )



(DEFSTRUCT (MOBY-BASIC-FILE (:TYPE :ARRAY)      ;:named-array when symbols exist in moby space
                       (:CONC-NAME MFILE-)
                       (:CONSTRUCTOR NIL)
                       (:ALTERANT NIL))
  CODE          ;primitive named-structure-symbol.
                ;  code 3 is root-section-directory-header
                ;       4 is directory
                ;       5 is file
  NAME                                          ;A string.
  LOCK  ;THIS IS A LOCAL CELL                   ;A contention lock.
  MAP           ;SECTION-MAP defstruct.
  DATA-POINTER  ;reach the data in the file through this.
                ; if mmoby-mapped? => nil, this points to an array-of-arrays, which hold data.
                ; if mmoby-mapped? => T, this points the root structure of the file.
                ;   The root structure is is the first structure in the file and can be
                ;    either a CONS, an ARRAY, or an ARRAY-WITH-LEADER.
  FILES                                         ;Used only if the file is a directory.
  TYPE                                          ;A string.
  VERSION                                       ;A number (16b).
  )

(DEFSTRUCT (MOBY-FILE (:TYPE :ARRAY)            ;:NAMED-ARRAY when symbols in moby space.
                 (:INCLUDE MOBY-BASIC-FILE)
                 (:CONC-NAME MFILE-)
                 ;(:MAKE-ARRAY (:AREA MOBY-ROOT-AREA))
                 )
  tick        ;tick at which file declared "complete".
  sections-i-reference
  sections-which-reference-me
  DEFAULT-BYTE-SIZE
  OVERWRITE-FILE                                ;Links overwriting/overwritten files.
  AUTHOR-INTERNAL                               ;A string
  CREATION-DATE-INTERNAL                        ;A number (32 bits)
  DIRECTORY                                     ;Mommy.
  OPEN-COUNT                                    ;The number of times this file is open
                                                ;-1 if open for overwrite.
  ATTRIBUTES                                    ;A 16-bit word of common properties
  PLIST                                         ;Arbitrary properties.
  )

;Not really implemented, hack this sometime.
(DEFSTRUCT (MOBY-LINK (:TYPE :ARRAY)            ;:named-array
                 (:INCLUDE MOBY-BASIC-FILE)
                 (:CONC-NAME MFILE-)
                 (:ALTERANT NIL)
                 ;(:MAKE-ARRAY (:AREA MOBY-ROOT-AREA))
                 )
  TO-STRING                                     ;The string linked to, ie "AI: DLA; FOO >"
  )

;;;; Property Bit Definitions

(DEFVAR ATTRIBUTES (MAKE-ARRAY 16.))

(DEFMACRO DEFINE-ATTRIBUTES (&BODY NUMBERS-AND-NAMES)
  `(PROGN 'COMPILE
          . ,(LOOP FOR (NUMBER NAME) ON NUMBERS-AND-NAMES BY #'CDDR
                   NCONCING `((DEFPROP ,NAME ,(BYTE 1 NUMBER) MOBY-ATTRIBUTE)
                              (ASET ',NAME ATTRIBUTES ,NUMBER)))))

(DEFINE-ATTRIBUTES
  0  :DONT-DELETE                               ;File cannot be deleted if on.
  1  :CLOSED                                    ;File is still being written if off.
  2  :DELETED                                   ;File is deleted but still open somewhere.
  3  :DUMPED                                    ;File has been dumped
  4  :DONT-REAP                                 ;Dont reap this file (not used internally)
  5  :CHARACTERS                                ;The file is a CHARACTERS file.
  6  :DIRECTORY                                 ;The file is a subdirectory.

  7  :MOBY-MAPPED                               ;the file is moby-mapped.
                                ;if 0, the DATA-POINTER points to the ARRAY-OF-ARRAYS
                                ;if 1, the DATA-POINTER points at the file-root.
; 10 :QFASLP
; 11 :LISP                                      ;Obsolete
; 12 :TEXT                                      ;Obsolete
; 13 :MAY-BE-REAPED                             ;This bit is used by reaping programs.
                                                ;If it is on, the file is liable to be
                                                ;reaped.  Also, a reaping program may
                                                ;turn it on as a warning that it is liable
                                                ;to be reaped the next time around.
  )

(DEFSUBST MFILE-ATTRIBUTE (FILE INDICATOR)
  (LDB-TEST (GET INDICATOR 'MOBY-ATTRIBUTE) (MFILE-ATTRIBUTES FILE)))

(defsetf mfile-attribute set-mfile-attribute)

(DEFUN SET-MFILE-ATTRIBUTE (FILE INDICATOR VAL)
  (SETF (MFILE-ATTRIBUTES FILE)
        (DPB (IF VAL 1 0) (GET INDICATOR 'MOBY-ATTRIBUTE) (MFILE-ATTRIBUTES FILE)))
  (NOT (NULL VAL)))

;;; Syntax for attributes.

(DEFSUBST MFILE-CLOSED? (FILE)
  (MFILE-ATTRIBUTE FILE :CLOSED))

(DEFSUBST MFILE-DELETED? (FILE)
  (MFILE-ATTRIBUTE FILE :DELETED))

(DEFSUBST MDIRECTORY? (FILE)
  (MFILE-ATTRIBUTE FILE :DIRECTORY))

(DEFSUBST MMOBY-MAPPED? (FILE)
  (MFILE-ATTRIBUTE FILE :MOBY-MAPPED))

(DEFSUBST MROOT-DIRECTORY? (FILE)
  (NULL (MFILE-DIRECTORY FILE)))

(DEFSUBST MFILE-NPAGES (FILE)
  (cond ((null (mmoby-mapped? file))
         (cond ((null (mfile-data-pointer file)) 0)             ;temp
               (t (array-of-arrays-npages (mfile-data-pointer file)))))
        (T (MOBY-MAP-DATA-LENGTH-IN-PAGES (MFILE-MAP FILE)))))

;;;; Maps

;(DEFSUBST MOBY-MAP-NBLOCKS (MAP)
;  (ARRAY-LEADER MAP 0))

(DEFSUBST MOBY-A-OF-A-NBLOCKS (A-OF-A)
  (ARRAY-LEADER A-OF-A 0))

;(DEFSUBST MOBY-MAP-BLOCK-LOCATION (MAP INDEX)
;  (AREF MAP INDEX 0))

(DEFSUBST MOBY-A-OF-A-ARRAY (A-OF-A INDEX)
  (AREF A-OF-A INDEX 0))

;(DEFSUBST MOBY-MAP-BLOCK-SIZE (MAP INDEX)
;  (AREF MAP INDEX 1))

(DEFSUBST MOBY-A-OF-A-BLOCK-SIZE (A-OF-A INDEX)
  (AREF A-OF-A INDEX 1))

;;;; Printers

(DEFSELECT MFILE-NSI
  (:PRINT-SELF (FILE STREAM IGNORE IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (FILE STREAM :NO-POINTER)
      (COND ((MDIRECTORY? FILE)
             (FORMAT STREAM "MBFS-Directory~{ ~A~}" (MFILE-FULL-NAME FILE)))
            (T
             (FORMAT STREAM "MBFS-File ~S"
                     (MLM-NAMESTRING NIL NIL
                                    (MFILE-FULL-NAME (MFILE-DIRECTORY FILE))
                                    (MFILE-NAME FILE)
                                    (MFILE-TYPE FILE)
                                    (MFILE-VERSION FILE))))))))

;(DEFSELECT MOBY-DIRECTORY-NSI
;  (:PRINT-SELF (DIRECTORY STREAM IGNORE IGNORE)
;    (SI:PRINTING-RANDOM-OBJECT (DIRECTORY STREAM :TYPEP :NO-POINTER)
;      (PRIN1 (MFILE-NAME DIRECTORY) STREAM))))

(DEFUN MFILE-FULL-NAME (DIRECTORY)
  (AND (NOT (MROOT-DIRECTORY? DIRECTORY))
       (APPEND (MFILE-FULL-NAME (MFILE-DIRECTORY DIRECTORY))
               (LIST (MFILE-NAME DIRECTORY)))))

;(DEFSELECT MOBY-MAP-NSI
;  (:PRINT-SELF (MAP STREAM IGNORE IGNORE)
;    (SI:PRINTING-RANDOM-OBJECT (MAP STREAM :TYPEP)
;      (FORMAT STREAM "~D block~:P" (MOBY-MAP-NBLOCKS MAP))))
;  (:DESCRIBE (MAP) (DESCRIBE-MAP MAP)))

(DEFPROP MOBY-FILE         MFILE-NSI     NAMED-STRUCTURE-INVOKE)
(DEFPROP MOBY-LINK         MFILE-NSI     NAMED-STRUCTURE-INVOKE)
;(DEFPROP MOBY-DIRECTORY MOBY-DIRECTORY-NSI NAMED-STRUCTURE-INVOKE)
;(DEFPROP MOBY-MAP         MOBY-MAP-NSI  NAMED-STRUCTURE-INVOKE)

;;; Randomness.

(DEFUN MBFS-KEYWORD-OPTION (OPTIONS KEYWORD DEFAULT)
  (COND ((NULL OPTIONS)
         DEFAULT)
        ((EQ KEYWORD (CAR OPTIONS))
         (CADR OPTIONS))
        (T
         (MBFS-KEYWORD-OPTION (CDDR OPTIONS) KEYWORD DEFAULT))))

;;;;; Locking Macros

;(DEFMACRO LOCKING (LOCK &BODY BODY)
;  "Execute BODY with LOCK locked."
;  (LET ((LOCK-CELL (GENSYM)))
;    `(LET ((,LOCK-CELL (LOCF ,LOCK)))
;       (UNWIND-PROTECT
;        (PROGN (PROCESS-LOCK ,LOCK-CELL) . ,BODY)
;        (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL)))))

;;** this unlocks the frob when thru even if you had it originally.
;;  probably a loss!! Should use WITH-LOCK.
;(DEFMACRO LOCKING-RECURSIVELY (LOCK &BODY BODY)
;  "Execute BODY with LOCK locked; don't die if already locked."
;  (LET ((LOCK-CELL (GENSYM))
;       (LOCK-OWNED (GENSYM)))
;    `(LET* ((,LOCK-CELL (LOCF ,LOCK))
;           (,LOCK-OWNED (EQ (CAR ,LOCK-CELL) CURRENT-PROCESS)))
;       (UNWIND-PROTECT
;        (PROGN (IF (NOT ,LOCK-OWNED)
;                   (PROCESS-LOCK ,LOCK-CELL))
;               . ,BODY)
;        (IF (NOT ,LOCK-OWNED)                  ;unlock it only if you locked it.
;            (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL))))))

(DEFMACRO REQUIRE-LOCK (LOCK)
  (LET ((ERSTR (FORMAT NIL "~S should be locked here but is not."
                       (IF (NLISTP LOCK) LOCK (CAR LOCK)))))
    `(IF (NOT (EQ ,LOCK CURRENT-PROCESS))
         (FERROR NIL ,ERSTR))))

;new queue locking scheme.
; "positively timed, with deadly embrace recognition and resolution"
; locks have LOCK-DEFSTRUCTs and processes have PROCESS-LOCKING-INFO defstructs.

;lock-defstruct:
(defstruct (lock (:type :named-array) (conc-name lock-))
  name          ;name of the lock, for identification.
  owner         ;NIL if free, or the process-lock-info of the owner.
  waitees)      ;List of PROCESS-LOCK-INFOs of waitees.



;process-lock-info:
(defstruct (process-lock-info (:type :named-array) (conc-name pli-))
  process       ;associated process
  (type :user)  ;type of this process.  Either :USER or :SERVER.
                ; :SERVER has priority since if we are waiting our user process
                ;on a remote machine must hold  that machine's lock.
  moby-identity ; for servers, is the <moby-host-object> for the machine at the other end.
; gotten by (LISPM-HOST-OF-SERVER-PROCESS <server-process>).
                ; <moby-identity> is nil for :USER processes.
  (locks-held NIL)      ;  list thereof.
  (waiting-on NIL)      ;  if currently waiting, the lock-defstruct for that lock.
  (instruction NIL)     ; examined by process when it comes out of waiting state to see what to do.
                        ;       :THROW
                        ;       :TAKE
  (runstate NIL)        ;  :RUNNING, :QUEUE-LOCK, or :NETWAIT
;     :QUEUE-LOCK if I'm waiting for a lock in this locking system.
;     :NETWAIT if I'm waiting for a respose from the net.
;     :RUNNING otherwise.
  )

;  sketch of the operation:
;    entry.  If lock free seize it, etc.  otherwise make entry on waitee list.
;    wait.  Process waits for its own <instruction> to become non-NIL.
;     When that happens it executes the <instruction> which can be:
;    When the lock becomes free, look down list and give highest priority requestor
;     a :TAKE instruction.

;recognizing and breaking deadly embraces.
; the primary one we worry about is a cross-machine embrace as follows:
;  on machine A,  the <user> has the lock, waiting for the <server> of B to respond.
;  on machine B,  the <user> has the lock, waiting for the <server> of A to respond.
; the only way to break the embrace is for one of the servers to send back a
;  <embrace X> response, instructing its <user> to throw out, and give the lock to
; the <server for X>.  Clearly, it doesnt win for both ends to do this at the
;  same time.  Since any transaction between A and B must grab the locks at both ends,
;  the embrace will eventually be recognized on both ends, altho possibly not
;  simultaneously. Therefore, we need an alternating priority, associated with the link
;  which causes the low priority end to give up and the high priority guy to wait it out.

; it is also possible to have a three way embrace, etc... For now..

;State of the LOCKER.
;  the LOCKER can either be in <running> state or <netwait> state.
; On transition into <netwait>, the locking system gets a chance
; to hack.


(defun process-assign-lock-info (&optional (process current-process))
  (cond ((send process :process-lock-info))
        (t (send process :set-process-lock-info
                 (make-process-lock-info :process process)))))

(defmethod (si:process :process-lock-info) ()
  (if (variable-boundp si:spare-slot-1) si:spare-slot-1
    (setq si:spare-slot-1 (make-process-lock-info :process self))))

(defmethod (si:process :set-process-lock-info) (info)
  (setq si:spare-slot-1 info))


(DEFMACRO WITH-MOBY-LOCK (&ENVIRONMENT ENV
                          (&KEY NORECURSIVE (WHOSTATE "moby-lock" WHOSTATEP) TIMEOUT)
                          &BODY BODY)
  "Execute the BODY with a lock locked.
LOCATOR is an expression whose value is the lock status;
it should be suitable for use inside LOCF.
NORECURSIVE means do not allow locking a lock already locked by this process.
WHOSTATE is what to display if we hang waiting for the lock.
TIMEOUT, if non-NIL, say to signal a SYS:LOCK-TIMEOUT condition if the lock remains
  unavailable for that many 60'ths of a second.  Otherwise, we wait indefinitely."
  (let* ((locator '*fake-moby-lock*)
         (lock (macroexpand `(locf ,locator) env)))
    `(LET* ((.POINTER. ,lock)
            (.ALREADY.MINE. (EQ (CAR .POINTER.) CURRENT-PROCESS)))
       ;; Kludge due to the fact the fact that the compiler knows nothing about types.
       ;; Common cases which are guaranteed to return locatives.
       ,@(if (memq (car-safe lock) '(variable-location aloc locate-in-instance %instance-loc))
             ()
           `((IF (CONSP .POINTER.)
                 (SETQ .POINTER. (CDR-LOCATION-FORCE .POINTER.)))))
       (UNWIND-PROTECT
           (PROGN
             (IF .ALREADY.MINE.
                 ,(IF NORECURSIVE `(FERROR "Attempt to lock ~S recursively."
                                           ',LOCATOR))
               ;; Redundant, but saves time if not locked.
               (OR (%STORE-CONDITIONAL .POINTER. NIL CURRENT-PROCESS)
                   ,(cond (timeout
                           `(process-lock .pointer. nil ,whostate ,timeout))
                          (whostatep
                           `(process-lock .pointer. nil ,whostate))
                          (t
                           `(process-lock .pointer.)))))
             . ,BODY)
         (UNLESS .ALREADY.MINE.
           (%STORE-CONDITIONAL .POINTER. CURRENT-PROCESS NIL))))))

(DEFMACRO MODIFYING-DIRECTORY (DIRECTORY &BODY BODY)
  "Lock DIRECTORY, execute BODY, and then mark DIRECTORY as modified."
  (LET ((LOCK-CELL (GENSYM)))
    `(LET ((,LOCK-CELL (LOCF (MFILE-LOCK ,DIRECTORY))))
       (UNWIND-PROTECT
         (PROGN (PROCESS-LOCK ,LOCK-CELL) . ,BODY)
         (PROGN (SETF (MFILE-CLOSED? DIRECTORY) NIL)
                (%STORE-CONDITIONAL ,LOCK-CELL CURRENT-PROCESS NIL))))))

(DEFMACRO WITH-COMPLETE-ACCESS-PATH (&BODY BODY)
  "Execute the body with a CATCH for FILE-BEING-WRITTEN-OR-SUPERSEDED-CATCH. If that tag is
thrown to, value is a FILE (directory in this case, really), which we need.  Wait for it
to clear, then try again."
 `(BLOCK WITH-COMPLETE-ACCESS-PATH
    (TAGBODY
     RETRY
        (RETURN-FROM WITH-COMPLETE-ACCESS-PATH
          (CATCH-CONTINUATION 'FILE-BEING-WRITTEN-OR-SUPERSEDED
             #'(LAMBDA (FILE)
                 (PROCESS-WAIT "Access Path"
                               #'(LAMBDA (F)
                                   (NOT (MBFS-FILE-BEING-WRITTEN-OR-SUPERSEDED? F)))
                               FILE)
                 (GO RETRY))
             NIL
            . ,BODY)))))

(DEFMACRO OPEN-INPUT-FILE ((PARTITION-HOST FILE PATHNAME) &BODY BODY)
  `(LET ((,FILE NIL))
     (UNWIND-PROTECT
       (PROGN
         (SETQ ,FILE (LOOKUP-FILE ,PARTITION-HOST
                       (FS:PATHNAME-RAW-DIRECTORY ,PATHNAME)
                       (FS:PATHNAME-RAW-NAME ,PATHNAME)
                       (FS:PATHNAME-RAW-TYPE ,PATHNAME)
                       (FS:PATHNAME-RAW-VERSION ,PATHNAME)
                       :ERROR NIL T T   NIL))
         . ,BODY)
       (WHEN ,FILE
         (MBFS-CLOSE-FILE ,FILE)))))

(DEFMACRO OPEN-INPUT-FILE-OR-DIRECTORY ((PARTITION-HOST FILE PATHNAME) &BODY BODY)
  `(LET ((,FILE NIL))
     (UNWIND-PROTECT
       (PROGN
         (SETQ ,FILE (LOOKUP-FILE ,PARTITION-HOST
                       (FS:PATHNAME-RAW-DIRECTORY ,PATHNAME)
                       (FS:PATHNAME-RAW-NAME ,PATHNAME)
                       (FS:PATHNAME-RAW-TYPE ,PATHNAME)
                       (FS:PATHNAME-RAW-VERSION ,PATHNAME)
                       :ERROR NIL :DIRECTORY-OK T  NIL))
         . ,BODY)
       (WHEN ,FILE
         (MBFS-CLOSE-FILE ,FILE)))))

(DEFUN ARRAY-OF-ARRAYS-CREATE (PARTITION-HOST &OPTIONAL (NBLOCKS 32.) INPUTP)
  (let ((A-OF-A (MAKE-ARRAY (LIST NBLOCKS 2)
                           :AREA (ROOT-AREA-OF-PARTITION-HOST PARTITION-HOST)
                           :LEADER-LENGTH 1
                          ;:NAMED-STRUCTURE-SYMBOL 'MOBY-ARRAY-OF-ARRAYS
                        )))
    (SETF (MOBY-A-OF-A-NBLOCKS A-OF-A) (IF INPUTP NBLOCKS 0))
    A-OF-A))

;;; Returns the number of pages used (as opposed to the number of blocks) in the map.
;(DEFUN MAP-NPAGES (MAP)
;  (LOOP WITH NBLOCKS = (MOBY-MAP-NBLOCKS MAP)
;       FOR I FROM 0 BELOW NBLOCKS
;       SUMMING (CEILING (MOBY-MAP-BLOCK-SIZE MAP I) fs:PAGE-SIZE-IN-BITS)))

;;; Returns the number of pages used (as opposed to the number of blocks) in the map.
(DEFUN ARRAY-OF-ARRAYS-NPAGES (A-OF-A)
  (LOOP WITH NBLOCKS = (MOBY-A-OF-A-NBLOCKS A-OF-A)
        FOR I FROM 0 BELOW NBLOCKS
        SUMMING (CEILING (MOBY-A-OF-A-BLOCK-SIZE A-OF-A I) fs:PAGE-SIZE-IN-BITS)))

;; This is used during output, when an array is added onto the end of a file.
(DEFUN ARRAY-OF-ARRAYS-APPEND-ARRAY (A-OF-A ARRAY SIZE
                                     &AUX (NBLOCKS (MOBY-A-OF-A-NBLOCKS A-OF-A)))
  (AND (= NBLOCKS (ARRAY-DIMENSION A-OF-A 0))
       (ARRAY-GROW A-OF-A
                   (COND ((< NBLOCKS 40) 200)
                         (T (* NBLOCKS 2.)))
                   2))
  (WITHOUT-INTERRUPTS
    (SETF (MOBY-A-OF-A-NBLOCKS A-OF-A) (1+ NBLOCKS))
    (SETF (MOBY-A-OF-A-ARRAY A-OF-A NBLOCKS) ARRAY)
    (SETF (MOBY-A-OF-A-BLOCK-SIZE A-OF-A NBLOCKS) SIZE)))

;; Returns the length of the area mapped in bits.
(DEFUN ARRAY-OF-ARRAYS-LENGTH (A-OF-A)
  (LOOP WITH NBLOCKS = (MOBY-A-OF-A-NBLOCKS A-OF-A)
        FOR I FROM 0 BELOW NBLOCKS
        SUMMING (MOBY-A-OF-A-BLOCK-SIZE A-OF-A I)))

(DEFUN DESCRIBE-ARRAY-OF-ARRAYS (A-OF-A &OPTIONAL (STREAM STANDARD-OUTPUT))
  (FORMAT STREAM "~&~S maps out the following arrays:~@
~2@TIndex~3@TLocation~7@TSize~3@TStatus~%" A-OF-A)
  (LET ((NBITS 0))
    (DOTIMES (I (MOBY-A-OF-A-NBLOCKS A-OF-A))
      (FORMAT STREAM "~6O: ~10O ~10O "
              I (MOBY-A-OF-A-ARRAY A-OF-A I) (MOBY-A-OF-A-BLOCK-SIZE A-OF-A I))
      (INCF NBITS (MOBY-A-OF-A-BLOCK-SIZE A-OF-A I)))
    (FORMAT STREAM "~%Total of ~D array~:P, ~D bit~:P.~%"
            (MOBY-A-OF-A-NBLOCKS A-OF-A)
            NBITS)))

(DEFUN FILE-DATA-LENGTH (FILE)
  (COND ((NULL (MMOBY-MAPPED? FILE))
         (COND ((NULL (MFILE-DATA-POINTER FILE)) 0)     ;temp until file always has this.
               (T (ARRAY-OF-ARRAYS-LENGTH (MFILE-DATA-POINTER FILE)))))
        (T (MOBY-MAP-DATA-LENGTH-IN-BITS (MFILE-MAP FILE)))))

;(DEFUN MFILE-NPAGES (FILE)     ;This is a DEFSUBST
;  (ARRAY-OF-ARRAYS-NPAGES (MFILE-ARRAY-OF-ARRAYS FILE)))

(DEFUN FILE-TRUENAME (FILE)
  ;; If at all possible, make the host name be a normal network host name, since these
  ;; names are transportable among machines, whereas "MLM" is not.
  (MAKE-PATHNAME ':HOST ;(IF MOBY-LOCAL-HOST (SEND MOBY-LOCAL-HOST :NAME-AS-FILE-COMPUTER) "MLM")
                 (copy-to-area-if-necessary working-storage-area
                  (send (partition-host-of-section file) :moby-partition-host-name))
                 ':DEVICE "DSK"
                 ':DIRECTORY (OR (copy-to-area-if-necessary working-storage-area
                                  (MFILE-FULL-NAME (MFILE-DIRECTORY FILE)))
                                 :ROOT)
                 ':NAME (copy-to-area-if-necessary working-storage-area (MFILE-NAME FILE))
                 ':TYPE (IF (EQUAL (MFILE-TYPE FILE) "")
                            :UNSPECIFIC
                          (copy-to-area-if-necessary working-storage-area
                           (MFILE-TYPE FILE)))
                 ':VERSION (MFILE-VERSION FILE)))

;; Error handler interface.

(DEFMACRO IDENTIFY-FILE-OPERATION (OPERATION &BODY BODY)
  `(LET ((*CURRENT-FILE-OPERATION* (OR *CURRENT-FILE-OPERATION* ,OPERATION)))
     . ,BODY))

(DEFMACRO IDENTIFY-FILE-OPERATION-WITH-PATHNAME (OPERATION PATHNAME &BODY BODY)
  `(LET ((*CURRENT-FILE-OPERATION* (OR *CURRENT-FILE-OPERATION* ,OPERATION))
         (*CURRENT-FILE-OPERATION-PATHNAME* (OR *CURRENT-FILE-OPERATION-PATHNAME*
                                                ,PATHNAME)))
     . ,BODY))

;; Wrap this around things which want to do things the simple way.
;; If ERROR-P is non-NIL and an error occurs, the error object is returned from this form
(DEFMACRO HANDLING-ERRORS (ERROR-P &BODY BODY)
  `(*CATCH 'MLM-TRAP-ERRORS
     (LET ((MLM-TRAP-ERRORS (NOT ,ERROR-P)))
       (FS:FILE-OPERATION-RETRY
         . ,BODY))))

;; This is bound to T if one is inside MLM-TRAP-ERROR, in which case control
;; is thrown to MLM-TRAP-ERROR
(DEFVAR MLM-TRAP-ERRORS NIL)

(DEFVAR *CURRENT-FILE-OPERATION* NIL)
(DEFVAR *CURRENT-FILE-OPERATION-PATHNAME* NIL)

(DEFPROP MLM-SIGNAL-ERROR T :ERROR-REPORTER)
(DEFUN MLM-SIGNAL-ERROR (SYMBOL &OPTIONAL SOURCE PROCEEDABLE &REST MAKE-CONDITION-ARGS)
  (COND (SOURCE)
        ((OR (TYPEP SELF 'PATHNAME)
             (TYPEP SELF 'SI:FILE-STREAM-MIXIN))
         (SETQ SOURCE SELF))
        ((AND *CURRENT-FILE-OPERATION-PATHNAME*
              (TYPEP *CURRENT-FILE-OPERATION-PATHNAME* 'PATHNAME))
         (SETQ SOURCE *CURRENT-FILE-OPERATION-PATHNAME*)))
  (AND SOURCE (NOT (TYPEP SOURCE 'PATHNAME))
       (SETQ SOURCE (SEND SOURCE ':PATHNAME)))
  (LET ((ERROR
          (LEXPR-FUNCALL 'MAKE-CONDITION
                         (OR (GET SYMBOL 'ERROR-SIGNALER) SYMBOL)
                         (LET ((STRING (GET SYMBOL 'ERROR-STRING)))
                           (IF SOURCE
                               (STRING-APPEND STRING " for "
                                              (SEND SOURCE ':STRING-FOR-PRINTING))
                             STRING))
                         SOURCE MAKE-CONDITION-ARGS)))
    (IF MLM-TRAP-ERRORS
        (*THROW 'MLM-TRAP-ERRORS ERROR))
    (SIGNAL ERROR
            ':PROCEED-TYPES
            (COND ((CONSP PROCEEDABLE) PROCEEDABLE)
                  (PROCEEDABLE '(:RETRY-FILE-OPERATION))))))

;; Use this when an error occurs in file lookup.
(DEFPROP MLM-LOOKUP-ERROR T :ERROR-REPORTER)
(DEFUN MLM-LOOKUP-ERROR (PARTITION-HOST SYMBOL DIR NAM TYP VER &OPTIONAL (OPERATION *CURRENT-FILE-OPERATION*))
  (MLM-SIGNAL-ERROR SYMBOL
                   (MAKE-PATHNAME :HOST
                                  (copy-to-area-if-necessary working-storage-area
                                   (SEND PARTITION-HOST
                                         :MOBY-PARTITION-HOST-NAME))
                                  :DEVICE "DSK"
                                  :DIRECTORY
                                  (COND ((STRINGP DIR) DIR)
                                        ((LISTP DIR) DIR)
                                        (T
                                         (MFILE-NAME DIR)))
                                  :NAME NAM :TYPE TYP :VERSION VER)
                   NIL
                   OPERATION))

;; This is to aid in debugging.  It basically provides a warning at compile time
;; if you haven't defined an error.
(DEFUN MLM-ERROR-SYMBOL-CHECK (FORM)
  (LET ((X (SECOND FORM)))
    (AND (LISTP X)
         (EQ (CAR X) 'QUOTE)
         (PROGN
           (WHEN (NOT (GET (CADR X) 'ERROR-STRING))
             (COMPILER:BARF (CADR X) "is not a defined error symbol" 'COMPILER:WARN))
           (OR (GET (CADR X) 'ERROR-SIGNALER)
               (GET (CADR X) 'EH:MAKE-CONDITION-FUNCTION)
             (COMPILER:BARF (CADR X) "is not a defined signal name" 'COMPILER:WARN))))
    FORM))

(COMPILER:ADD-OPTIMIZER MLM-SIGNAL-ERROR MLM-ERROR-SYMBOL-CHECK)
(COMPILER:ADD-OPTIMIZER MLM-LOOKUP-ERROR MLM-ERROR-SYMBOL-CHECK)
(COMPILER:ADD-OPTIMIZER MLM-RENAME-ERROR MLM-ERROR-SYMBOL-CHECK)

;; Error Definitions
(DEFMACRO DEFINE-ERRORS (&REST SYMBOLS-AND-STRINGS)
  `(PROGN 'COMPILE
          . ,(LOOP FOR (SYMBOL STRING) ON SYMBOLS-AND-STRINGS BY #'CDDR
                   COLLECTING `(DEFPROP ,SYMBOL ,STRING ERROR-STRING))))

(DEFINE-ERRORS
;; Error signalers in SYS:IO;OPEN
  FS:RENAME-TO-EXISTING-FILE    "Attempt to rename to an existing file"
  FS:RENAME-ACROSS-DIRECTORIES  "Attempt to rename across directories"
  FILE-NOT-FOUND                "File not found"
  FILE-ALREADY-EXISTS           "File already exists"
  FS:DIRECTORY-NOT-FOUND                "Directory not found"
  FS:INVALID-BYTE-SIZE          "Illegal byte size"
  FS:NO-MORE-ROOM                       "Disk full"
  FS:NO-FILE-SYSTEM             "File system not mounted"
  FS:DIRECTORY-NOT-EMPTY                "Non-empty directories cannot be deleted"
  FS:DONT-DELETE-FLAG-SET               "File has DONT-DELETE bit set"
  UNIMPLEMENTED-OPTION          "Unimplemented OPEN option"
  FS:FILE-LOCKED                        "File is locked"
  FS:UNKNOWN-OPERATION          "Unknown operation"

;; Error signalers defined below.
  UNDELETE-UNCLOSED-FILE        "Cannot undelete unclosed file"
  OPEN-DELETED-FILE             "File is deleted"
  OPEN-DELETED-DIRECTORY        "Directory is deleted"
  OPEN-OUTPUT-FILE              "File is open for output"
  OPEN-UNFINISHED-DIRECTORY     "Directory still being created"
  FILE-IS-SUBDIRECTORY          "File is a subdirectory"
  UNSETTABLE-PROPERTY           "Unsettable property"
  FS:INVALID-PROPERTY-NAME              "Bad property"
  VERSION-TOO-LARGE             "Version must be smaller than 65536"
  INVALID-DIRECTORY-NAME        "Invalid name given for a directory"
  RENAME-DIRECTORY-INVALID-TYPE "Invalid new type or version for renaming a subdirectory."
  LINKS-NOT-SUPPORTED           "Links not supported"
  BAD-TYPE-FOR-SUBDIRECTORY     "Invalid type for subdirectory"
  SUPERSEDE-DIRECTORY           "Superseding a directory"
  TOP-LEVEL-FILE                "No non-directory files in root"
  )

(DEFPROP UNDELETE-UNCLOSED-FILE FS:FILE-LOCKED ERROR-SIGNALER)

(DEFSIGNAL OPEN-DELETED-FILE FILE-OPERATION-FAILURE (PATHNAME OPERATION)
           "Opening a file that is deleted.")

(DEFSIGNAL OPEN-DELETED-DIRECTORY FILE-LOOKUP-ERROR (PATHNAME OPERATION)
           "Containing directory is deleted.")

(DEFPROP OPEN-OUTPUT-FILE FS:FILE-OPEN-FOR-OUTPUT ERROR-SIGNALER)

(DEFSIGNAL OPEN-UNFINISHED-DIRECTORY FILE-LOOKUP-ERROR (PATHNAME OPERATION)
  "Containing directory is still being created.")

(DEFSIGNAL FILE-IS-SUBDIRECTORY
           (FILE-OPERATION-FAILURE WRONG-KIND-OF-FILE INVALID-OPERATION-FOR-DIRECTORY)
           (PATHNAME OPERATION)
  "Attempt to open a file which is a directory.")

(DEFSIGNAL UNSETTABLE-PROPERTY CHANGE-PROPERTY-FAILURE (PATHNAME PROPERTY)
           "PROPERTY isn't one of the properties this file system allows users to set.")

(DEFSIGNAL VERSION-TOO-LARGE
           (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX VERSION-TOO-LARGE)
           (PATHNAME OPERATION)
  "Version larger that 65536.")

(DEFSIGNAL INVALID-DIRECTORY-NAME
           (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX INVALID-DIRECTORY-NAME)
           (PATHNAME OPERATION)
  "")

(DEFSIGNAL RENAME-DIRECTORY-INVALID-TYPE
           (RENAME-FAILURE RENAME-DIRECTORY-INVALID-TYPE)
           (PATHNAME NEW-PATHNAME)
  "")

(DEFSIGNAL LINKS-NOT-SUPPORTED
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-LINK-FAILURE
                                   LINKS-NOT-SUPPORTED)
           (PATHNAME OPERATION)
  "Links are not supported in this file system.")

(DEFSIGNAL BAD-TYPE-FOR-SUBDIRECTORY
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-DIRECTORY-FAILURE
                                   BAD-TYPE-FOR-SUBDIRECTORY)
           (PATHNAME OPERATION)
  "")

(DEFSIGNAL SUPERSEDE-DIRECTORY
           (FILE-OPERATION-FAILURE CREATION-FAILURE TOP-LEVEL-FILE)
           (PATHNAME OPERATION)
  "Superseding a file which is a subdirectory.")

(DEFSIGNAL TOP-LEVEL-FILE
           (FILE-OPERATION-FAILURE CREATION-FAILURE TOP-LEVEL-FILE)
           (PATHNAME OPERATION)
  "Attempt to create a file not a directory in the root directory.")


;; Binding this variable to non-NIL causes auto creation of directories.
;; This is used internally to create directories, but may be bound by things
;; such as magtape restoration programs.  -- obsolete use :CREATE-DIRECTORIES keyword.
(DEFVAR MLM-AUTOMATICALLY-CREATE-DIRECTORIES NIL)

;; This is a variable with all file streams open since boot time.
;; The :OPEN-STREAMS message to a LOCAL-LISPM-HOST returns COPYLIST of this.
(DEFVAR MLM-FILE-STREAMS-LIST NIL)




(defun copy-to-area-if-necessary (area obj)
  (if (symbolp area) (setq area (symeval area)))
  (cond ((or (null (%pointerp obj))
             (symbolp obj))
         obj)
        ((= area (%area-number obj))
         obj)           ;assume if obj is in right area, all substructure is.
        ((stringp obj)
         (let ((default-cons-area area))
           (string-append obj)))
        ((consp obj)
         (cons-in-area (copy-to-area-if-necessary area (car obj))
                       (copy-to-area-if-necessary area (cdr obj))
                       area))
        (t (ferror nil "Cant handle ~s in copy-to-area-if-necessary" obj))))

(defun putprop-local-cell-in-area (symbol-or-plist value property area)
  (putprop-in-area symbol-or-plist 'placeholder property area)
  (let ((cellp (si:get-location-or-nil symbol-or-plist property)))
    (cond ((null cellp) (ferror "PUTPROP failed"))
          (t (moby-declare-local-cell cellp)
             (setf (car cellp) value)
             value))))

(eval-when (load compile)
(defun get-defstruct-slot-index (x)
  (let ((exp (macroexpand `(,x foo))))
  ;(si:defstruct-expand-ref-macro `(,x foo)) lost at compile time with defstruct on local-macros.
    (cond ((eq (car exp) 'progn)
           (setq exp (car (last exp)))))
    (cond ((and (eq (car exp) 'aref)
                (eq (cadr exp) 'foo))
           (caddr exp))
          (t (ferror nil "Unable to get defstruct index for ~s" x)))))
  )

;moby packet format definitions:

(defstruct (moby-pkt (:constructor nil)
                     (:include chaos:pkt
                      ((chaos:pkt-first-data-word)
                       (command (byte 8 0))
                       (seq (byte 8 8)))     ;in response, matches seq of command.
                      ((chaos:pkt-second-data-word)
                       (arg)
                       (sub-seq (byte 8 0))  ;in response, counts for pkts within reply to
                                             ; one command.
                       (last-flag (byte 1 15.))))
                     (:conc-name mpkt-))
  "Additional fields for pkt used as moby-command, ea 16 bits."
  q-count
  structure-handle-lo
  structure-handle-hi
  base-moby-address-idx
  )

(defun print-moby-pkt (pkt)
  (format t "~%Command ~s, seq ~s, arg ~s, Q-count ~s, structure-handle ~s"
          (mpkt-command pkt)
          (mpkt-seq pkt)
          (mpkt-arg pkt)
          (mpkt-q-count pkt)
          (dpb (mpkt-structure-handle-hi pkt)
               (byte 16. 16.)
               (mpkt-structure-handle-lo pkt)))
  (cond ((not (zerop (mpkt-arg pkt)))
         (format t "~%Arg: ")
         (print-moby-pkt-arg (mpkt-arg pkt))))
  (format t "~%Base-moby-address ~s"
          (moby-base-from-data-response pkt))
  (dotimes (c (mpkt-q-count pkt))
    (multiple-value-bind (q0 q1 q2 q3)
        (moby-fetch-quarters-from-pkt pkt c)
      (multiple-value-bind (format-symbol moby-bits v0 v1)
          (moby-parse-components-and-moby-bits q0 q1 q2 q3)
        (format t "~%  Q ~s, format ~s, moby-bits ~s, v0 ~s, v1 ~s" c
                format-symbol moby-bits v0 v1))))
  (chaos:print-pkt pkt)
  )

(defun print-moby-pkt-arg (arg)
  (if (bit-test 1_15. arg) (format t " LAST"))
  (if (bit-test 1_14. arg) (format t " CONSABLE"))
  (if (bit-test 1_13. arg) (format t " POSITIVE"))
  (format t " Sub-seq ~S" (ldb (byte 8 0) arg)))

(defun print-pkt-data (pkt)
  (let ((c 0) (*print-base* 8) (*read-base* 8))
    (dotimes (idx (// (chaos:pkt-nbytes pkt) 2))
      (print (aref pkt idx))
      (cond ((> (setq c (1+ c)) 10.)
             (setq c 0) (terpri))))))

(defconstant *max-moby-qs-in-pkt* (// (- chaos:max-data-words-per-pkt
                                         (+ 4 (get-defstruct-slot-index 'mpkt-base-moby-address-idx)))
                                      4))
(defconstant *max-clean-bits-in-pkt* (* 64. *max-moby-qs-in-pkt*))

;command in low 8 bits of first data word
(defconstant %moby-pkt-request-data #o1)
(defconstant %moby-pkt-response-data #o2)
(defconstant %moby-pkt-writeout-area #o3) ;similar to request data, but
        ;moby address is map-moby-handle (i.e. moby address of the section-map).
        ;Response is a response-pkt.
(defconstant %moby-pkt-request-consing-region #o4)
        ;given map-moby-handle of section.
        ;reply is a response-region-mapping pkt.
(defconstant %moby-pkt-response #o5)
(defconstant %moby-pkt-request-region-mapping #o6)
  ;given a moby-address request info as to which region its in.
(defconstant %moby-pkt-response-region-mapping #o7)
  ;reply gives:
  ;  section map-moby-handle (moby address of section map in primary host)
  ;  namespace-page-origin
  ;  size in qs
  ;  current free-pointer
  ;  consing-permission
(defconstant %moby-pkt-request-section-mapping #o10)
  ;given a map-moby-handle, request info about this section.
(defconstant %moby-pkt-response-section-mapping #o11)
  ;root-datatype root-pointer-region-offset
  ;area-region-size
  ;area-moby-options
  ;area-region-bits
  ;area-name-string
(defconstant %moby-pkt-request-free-pointer #o12)
        ;response is %moby-pkt-response, -1, ie all ones, is a negative response.
  ;namespace-page-origin
(defconstant %moby-pkt-request-clean-bits #o13)
        ;response is %moby-pkt-clean-bits
  ;namespace-page-origin
  ; (local free pointer in mpkt-structure-handle-lo, hi)
(defconstant %moby-pkt-clean-bits #o14)
  ;reply to request-bits.
  ;Q part of pkt is clean bits, 64. bits per Q.
(defconstant %moby-pkt-give-data #o15)  ;similar to response-data except initiated by user.


(defun moby-base-from-data-response (pkt)
  (get-moby-address-from-pkt pkt -1))

;return untyped moby address
(defun get-moby-address-from-pkt (pkt q-idx)
  (let* ((idx (+ (* (1+ q-idx) 4)
                 #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) ))
         (q0 (aref pkt idx))
         (q1 (aref pkt (1+ idx)))
         (q2 (aref pkt (+ 2 idx)))
         (q3 (aref pkt (+ 3 idx))))
    (dpb-big (dpb q3
                  (byte 9 16.)
                  q2)
             (byte 25. 25.)
             (dpb q1                            ;value of this dpb is bottom 25.
                  (byte 9 16.)
                  q0))))

(defun store-moby-address-in-pkt (pkt moby-address q-idx
                                  &optional (cdr-code-and-data-type 0) (moby-bits 0))
 ;q-idx is -1 for base address, 0 for arg Qs.
  (let ((idx (+ (* (1+ q-idx) 4)
                #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    (aset (ldb (byte 16. 0) moby-address) pkt idx)
    (aset (dpb cdr-code-and-data-type (byte 7 9) (ldb (byte 9 16.) moby-address))
          pkt (1+ idx))
    (aset (ldb (byte 16. 25.) moby-address) pkt (+ idx 2))
    (aset (dpb moby-bits (byte 7 9) (ldb (byte 9 41.) moby-address))
          pkt (+ idx 3))))

(defun store-pointer-bits-in-pkt (pkt pointer-bits q-idx
                                  &optional (cdr-code-and-data-type 0) (moby-bits 0))
  (let ((idx (+ (* (1+ q-idx) 4)
                #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    (aset (ldb (byte 16. 0) pointer-bits) pkt idx)
    (aset (dpb cdr-code-and-data-type (byte 7 9) (ldb (byte 9 16.) pointer-bits))
          pkt (1+ idx))
    (aset 0 pkt (+ idx 2))
    (aset (dpb moby-bits (byte 7 9) 0)
          pkt (+ idx 3))))

(defun store-moby-halves-in-pkt (pkt h0 h1 q-idx
                                  &optional (moby-bits 0))
  (let ((idx (+ (* (1+ q-idx) 4)
                #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    (aset h0 pkt idx)
    (aset 0 pkt (1+ idx))
    (aset h1 pkt (+ idx 2))
    (aset (dpb moby-bits (byte 7 9) 0)
          pkt (+ idx 3))))

(defun store-moby-quarters-in-pkt (pkt q0 q1 q2 q3 q-idx)
  (let ((idx (+ (* (1+ q-idx) 4)
                #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    (aset q0 pkt idx)
    (aset q1 pkt (1+ idx))
    (aset q2 pkt (+ idx 2))
    (aset q3 pkt (+ idx 3))))

(defun moby-fetch-quarters-from-pkt (pkt q-idx)
  ; -1 is base address, 0 etc are data Qs.
  (LET ((IDX (+ (* (1+ q-idx) 4)
                #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    (values (aref pkt idx)
            (aref pkt (1+ idx))
            (aref pkt (+ 2 idx))
            (aref pkt (+ 3 idx)))))

(defun moby-get-clean-bit-from-pkt (pkt bit-no)
  (let* ((idx-no (// bit-no 16.))
         (bit-pos (\  bit-no 16.))
         (idx (+ idx-no
                 #.(* 4 (1+ (get-defstruct-slot-index 'mpkt-base-moby-address-idx))) )))
    (ldb (byte 1 bit-pos) (aref pkt idx))))

(defun moby-set-clean-bit-in-pkt (bit pkt bit-no)
  (let* ((idx-no (// bit-no 16.))
         (bit-pos (\  bit-no 16.))
         (idx (+ idx-no
                 #.(* 4 (1+ (get-defstruct-slot-index 'mpkt-base-moby-address-idx))) )))
    (setf (ldb (byte 1 bit-pos) (aref pkt idx))
          bit)))


(defvar *moby-command-seq* 0)

(defun moby-send-pkt-request-data (conn command arg moby-address)
  (let ((pkt (chaos:get-pkt))
        (seq nil))
    (without-interrupts
      (setf (chaos:pkt-first-data-word pkt)
            (dpb (setq seq *moby-command-seq*) (byte 8 8) command))
      (setq *moby-command-seq* (logand #o377 (1+ seq))))
    (setf (mpkt-arg pkt) arg)
    (setf (mpkt-q-count pkt) 0)
    (setf (mpkt-structure-handle-lo pkt) 0)
    (setf (mpkt-structure-handle-hi pkt) 0)
    (store-moby-address-in-pkt pkt moby-address -1)
    (setf (chaos:pkt-nbytes-on-write pkt)
          (* 2 (+ 4 #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    ;(break "foo")
    (chaos:send-pkt conn pkt fs:%qfile-binary-opcode)
    seq))

(defun moby-send-pkt-request-clean-bits (conn command arg moby-address local-free-pointer)
  (let ((pkt (chaos:get-pkt))
        (seq nil))
    (without-interrupts
      (setf (chaos:pkt-first-data-word pkt)
            (dpb (setq seq *moby-command-seq*) (byte 8 8) command))
      (setq *moby-command-seq* (logand #o377 (1+ seq))))
    (setf (mpkt-arg pkt) arg)
    (setf (mpkt-q-count pkt) 0)
    (setf (mpkt-structure-handle-lo pkt) (ldb (byte 16. 0) local-free-pointer))
    (setf (mpkt-structure-handle-hi pkt) (ldb (byte 16. 16.) local-free-pointer))
    (store-moby-address-in-pkt pkt moby-address -1)
    (setf (chaos:pkt-nbytes-on-write pkt)
          (* 2 (+ 4 #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx) )))
    ;(break "foo")
    (chaos:send-pkt conn pkt fs:%qfile-binary-opcode)
    seq))

(defun moby-send-pkt-command-for-msa (msa command moby-arg)
  (moby-send-pkt-command (moby-get-conn-for-msa msa) command moby-arg))

(defun moby-send-pkt-command (conn command moby-arg &optional seq-num)
  (if (null seq-num)
      (setq seq-num (moby-send-pkt-request-data conn
                     command 0 moby-arg)))
  (let* ((pkt (chaos:get-next-pkt conn)))
    (cond ((not (= seq-num (mpkt-seq pkt)))
           (ferror nil "Response has wrong seq number (~s), should be ~s"
                   (mpkt-seq pkt) seq-num))
          ((not (= 0 (mpkt-sub-seq pkt)))
           (ferror nil "Response has wrong sub-seq number (~s), should be 0"
                   (mpkt-sub-seq pkt)))
          ((zerop (mpkt-last-flag pkt))
           (ferror nil "Response failed to have last-flag")))
    pkt))

(defun moby-send-pkt-response (conn pkt seq response)
  (setf (chaos:pkt-first-data-word pkt) (dpb seq (byte 8 8) %moby-pkt-response))
  (setf (mpkt-arg pkt) (dpb 1_7. (byte 8 8) 0))    ;has sub-seq, and last flag.
  (setf (mpkt-q-count pkt) 0)
  (setf (mpkt-structure-handle-lo pkt) 0)
  (setf (mpkt-structure-handle-hi pkt) 0)
  (store-moby-address-in-pkt pkt response -1)
  (setf (chaos:pkt-nbytes-on-write pkt)
        (* 2 (+ #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx)
                (* 1 4))))
  ;(print-moby-pkt pkt)
  (chaos:send-pkt conn pkt fs:%qfile-binary-opcode))

(defun moby-send-pkt-response-data
       (conn pkt seq arg n-qs structure-handle moby-base-adr pkt-comm-code)
  (setf (chaos:pkt-first-data-word pkt) (dpb seq (byte 8 8) pkt-comm-code))
  (setf (mpkt-arg pkt) arg)    ;has sub-seq, and last flag.
  (setf (mpkt-q-count pkt) n-qs)
  (setf (mpkt-structure-handle-lo pkt) (ldb (byte 16. 0) structure-handle))
  (setf (mpkt-structure-handle-hi pkt) (ldb (byte 16. 16.) structure-handle))
  (store-moby-address-in-pkt pkt moby-base-adr -1)
  (setf (chaos:pkt-nbytes-on-write pkt)
        (* 2 (+ #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx)
                (* (+ 1 n-qs) 4))))
  ;(print-moby-pkt pkt)
  (chaos:send-pkt conn pkt fs:%qfile-binary-opcode))

(defun moby-send-pkt-response-region-mapping
  ;size-in-qs 0 means negative response, other stuff doesnt matter.
  ;consable? (T or NIL if remote host can CONS.)
       (conn seq map-moby-handle namespace-page-origin size-in-qs free-pointer consable?)
  (let ((pkt (chaos:get-pkt))
        (n-qs 4))
    (setf (chaos:pkt-first-data-word pkt) (dpb seq (byte 8 8) %moby-pkt-response-region-mapping))
    (setf (mpkt-arg pkt) (dpb (logior 1_7.
                                      (if consable? 1_6. 0)
                                      (if (not (zerop size-in-qs)) 1_5. 0)) ;positive or negative response.
                              (byte 8 8) 0))    ;has sub-seq, and last flag.
    (setf (mpkt-q-count pkt) n-qs)
    (setf (mpkt-structure-handle-lo pkt) 0)
    (setf (mpkt-structure-handle-hi pkt) 0)
    (store-moby-address-in-pkt pkt 0 -1)        ;null base address
    (store-moby-address-in-pkt pkt map-moby-handle 0)
    (store-moby-address-in-pkt pkt namespace-page-origin 1)
    (store-moby-address-in-pkt pkt size-in-qs 2)
    (store-moby-address-in-pkt pkt free-pointer 3)
    (setf (chaos:pkt-nbytes-on-write pkt)
          (* 2 (+ #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx)
                  (* (+ 1 n-qs) 4))))
    ;(print-moby-pkt pkt)
    (chaos:send-pkt conn pkt fs:%qfile-binary-opcode)))

(defun moby-send-pkt-response-section-mapping
       (conn seq root-datatype root-pointer-region-offset area-region-size area-moby-options
        area-region-bits area-name-string)
  )

(defun moby-send-pkt-response-clean-bits
       (conn pkt seq arg n-bits local-free-pointer)
  (let ((n-qs (ceiling n-bits 64.)))
    (setf (chaos:pkt-first-data-word pkt) (dpb seq (byte 8 8) %moby-pkt-response-data))
    (setf (mpkt-arg pkt) arg)                   ;has sub-seq, and last flag.
    (setf (mpkt-q-count pkt) n-qs)
    (setf (mpkt-structure-handle-lo pkt) (ldb (byte 16. 0) local-free-pointer))
    (setf (mpkt-structure-handle-hi pkt) (ldb (byte 16. 16.) local-free-pointer))
    (store-moby-address-in-pkt pkt 0 -1)        ;moby-base-adr
    (setf (chaos:pkt-nbytes-on-write pkt)
          (* 2 (+ #.(get-defstruct-slot-index 'mpkt-base-moby-address-idx)
                  (* (+ 1 n-qs) 4))))
                                                ;(print-moby-pkt pkt)
    (chaos:send-pkt conn pkt fs:%qfile-binary-opcode)))

(defvar *moby-host-index* (make-array 256.)) ;known hosts, including local ones.
   ;ALL active instances of MOBY-REMOTE-HOST or MOBY-FILE-HOST are assigned indexes here.
(setf (aref *moby-host-index* 0) t)     ;index 0 means simply not reconciled.

;moby-host-object can be the real thing, or it can be a placeholder
; if necessary during booting a partition.
(defun moby-assign-host-index (moby-host-object)
  (dotimes (i (array-length *moby-host-index*)
              (ferror nil "*MOBY-HOST-INDEX* full"))
    (cond ((null (aref *moby-host-index* i))
           (setf (aref *moby-host-index* i) moby-host-object)
           (return i)))))

(defun moby-host-of-lispm-host (lispm-host)
  (dotimes (i (array-length *moby-host-index*)
              (define-moby-host-for-lispm-host lispm-host))
    (let ((h (aref *moby-host-index* i)))
      (cond ((and (typep h 'moby-remote-host)
                  (eq lispm-host (send h :remote-lispm-host)))
             (return h))))))

(defun define-moby-host-for-lispm-host (lispm-host)
  (tv:notify tv:selected-window "Defining moby host for lispm host ~S" lispm-host)
  (let ((package-root-host-name (moby-package-root-of-lispm-host lispm-host)))
    (tv:notify tv:selected-window "Host namestring for lispm host ~s is ~a" lispm-host package-root-host-name)
    (define-remote-moby-host package-root-host-name
                             lispm-host)))

(setq fs:lmfs-debug-server t)

;moved from other files, involve locking.
(defun fs:open-chaos-moby-mapped (host-unit properties string)
  (with-moby-lock ()
    (open-chaos-moby-mapped-internal host-unit properties string)))

(defun moby-process-pkt-request (conn pkt)
  (with-moby-lock ()
    (let ((command (mpkt-command pkt)))
      (select command
        (%moby-pkt-request-data
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-request-data-single-object-local))
        (%moby-pkt-give-data
         (moby-process-pkt-give-data conn pkt))
        (%moby-pkt-writeout-area
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-writeout-area-local))
        (%moby-pkt-request-clean-bits
         (moby-process-pkt-request-clean-bits conn pkt))        ;goes direct.
        (%moby-pkt-request-free-pointer
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-request-free-pointer-local))
        (%moby-pkt-request-consing-region
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-request-consing-region-local))
        (%moby-pkt-request-region-mapping
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-request-region-mapping-local))
        (%moby-pkt-request-section-mapping
         (moby-process-pkt-dispatch conn pkt
          'moby-process-pkt-request-section-mapping-local))
        (otherwise (ferror nil "Unknown moby-pkt-request ~d" command)))
      )))

(defvar *use-reconcile-buffer* nil)

(defun si:resolve-unreconciled-data (local-address md)  md
 ;local address directly from VMA.
 ; (print local-address) (print md) (break)
  (with-moby-lock ()
    ;could have got "given" while waiting for lock.
    (if (= (%p-ldb-offset %%q-data-type 0 local-address)
           dtp-unreconciled)
        (if *use-reconcile-buffer*
            (moby-reconcile-object-via-buffer local-address)
          (moby-reconcile-object local-address)))))

(defun moby-wait-for-data (pntr)
  (cond ((and (= (%p-ldb-offset %%q-data-type 0 pntr) dtp-unreconciled)
              (not (zerop (%p-ldb-offset %%q-pointer 0 pntr))))
         (process-wait "Wait for moby data" 'moby-wait-for-data-waiter pntr))))

(defun moby-wait-for-data-waiter (pntr)
  (or (not (= (%p-ldb-offset %%q-data-type 0 pntr) dtp-unreconciled))
      (zerop (%p-ldb-offset %%q-pointer 0 pntr))))

(defun moby-give-data-to-host (pntr host)
  (let ((conn (moby-get-conn-for-host host))
        (seq-num nil)
        (options 0))
    (with-moby-lock ()
      (without-interrupts
        (setq seq-num *moby-command-seq*)
        (setq *moby-command-seq* (logand #o377 (1+ seq-num))))
      (multiple-value-bind (leader total-size boxed-size)
          (moby-find-structure-info pntr)
        (moby-dereconcile-object-to-net conn seq-num options
                                        leader total-size boxed-size %moby-pkt-give-data))
      (let ((response-pkt (moby-send-pkt-command conn nil nil seq-num)))
        (chaos:return-pkt response-pkt)))))

(DEFUN PARTITION-INDEX-OF-PARTITION-HOST-NAME (PARTITION-HOST-NAME)
  (LET ((H (FS:GET-PATHNAME-HOST PARTITION-HOST-NAME)))
    (SEND H :HOST-INDEX)))

(DEFUN PARTITION-HOST-OF-PARTITION-HOST-NAME (PARTITION-HOST-NAME &optional no-error)
  (FS:GET-PATHNAME-HOST PARTITION-HOST-NAME no-error))

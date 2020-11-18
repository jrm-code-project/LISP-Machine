;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **


(defmacro with-decoded-disk-unit ((decoded encoded use . options) &body body)
  (let ((u (gentemp "unit"))
        (d (gentemp "dont-dispose")))
    `(let (,u ,d)
       (unwind-protect
           (let ((,decoded (multiple-value-setq (,u ,d) (decode-unit-argument ,encoded ,use ,@options))))
             ,@body)
         (and ,u (not ,d) (dispose-of-unit ,u))))))


(DEFMACRO WITH-DISK-RQB ((RQB . OPTIONS) &BODY BODY)
  (LET ((R (GENTEMP "rqb"))
        (RP (GENTEMP "rqbp")))
    `(LET (,R ,RP)
       (UNWIND-PROTECT
           (LET ((,RQB (MULTIPLE-VALUE (,R ,RP) (MAYBE-GET-DISK-RQB ,@OPTIONS))))
             ,@BODY)
         (AND ,RP ,R (RETURN-DISK-RQB ,R))))))

(DEFUN MAYBE-GET-DISK-RQB (&REST L)
  (DECLARE (ARGLIST &OPTIONAL (N-PAGES 1) LEADER-LENGTH)
           (VALUES RQB RETURNP))
  (COND ((TYPEP (CAR L) 'ARRAY)
         (VALUES (CAR L) NIL))
        ('ELSE
         (VALUES (APPLY 'GET-DISK-RQB L) T))))

(defun lambda-or-cadr-only ()
  (if (and (not (= processor-type-code lambda-type-code))
           (not (= processor-type-code cadr-type-code)))
      (ferror "lambda or cadr only")))

(defun lambda-only ()
  (if (not (= processor-type-code lambda-type-code))
      (ferror "lambda only")))

(defconst nupi-disk-rq-halfwords '(
  %nupi-disk-unit
  %nupi-disk-command
  %nupi-disk-status-lo
  %nupi-disk-status-hi
  %nupi-disk-ccw-list-pointer-lo
  %nupi-disk-ccw-list-pointer-hi
  %nupi-disk-transfer-count-lo
  %nupi-disk-transfer-count-hi
  %nupi-disk-logical-block-lo
  %nupi-disk-logical-block-hi
  %nupi-disk-interrupt-address-lo
  %nupi-disk-interrupt-address-hi
  %nupi-disk-reserved-0
  %nupi-disk-reserved-1
  %nupi-disk-reserved-2
  %nupi-disk-reserved-3
  %nupi-disk-ccw-list
  ))
(assign-values nupi-disk-rq-halfwords 0)

;;; "User" Disk I/O routines for Lisp machine -- macrocode portion
;;; See QCOM for documentation on disk rqb's and symbol definitions.
;;; The label-editor and related routines have been moved out of here into DLEDIT

;;;*** Errors should be metered, do this after the microcode is revised ***

;;; The following routines are likely to be of general interest:
;;; DESCRIBE-PARTITION - prints various info about a partition.
;;; FIND-PLAUSIBLE-PARTITIONS - searches the disk for blocks that appear to start Lisp bands
;;; SET-CURRENT-MICROLOAD - choose microload to be booted from  [this is in the DLEDIT file]
;;; SET-CURRENT-BAND - choose world load to be booted from  [this is in the DLEDIT file]
;;; PRINT-DISK-LABEL - print the pack label of a drive  [this is in the DLEDIT file]
;;; PRINT-LOADED-BAND - print information about what system is running
;;; PAGE-{IN,OUT}-{STRUCTURE,WORDS,REGION,AREA,ARRAY} - fast paging: multiple pages at a time
;;; EDIT-DISK-LABEL - edit the pack label of a drive  [this is in the DLEDIT file]
;;; LOAD-MCR-FILE - load microcode from the file-computer onto the disk
;;; COPY-DISK-PARTITION - copy a partition from disk to disk
;;; COPY-DISK-PARTITION-BACKGROUND - same but in separate process and artificially slowed down
;;; COMPARE-DISK-PARTITION - similar to copy, but compares and prints differences.
;;; MEASURED-SIZE-OF-PARTITION - Returns how much of LOD is actually used.

;;; These are interesting if you really want to do I/O
;;; GET-DISK-RQB
;;; RETURN-DISK-RQB
;;; PRINT-RQB
;;; DISK-READ
;;; DISK-WRITE

(DEFVAR *UNIT-CYLINDER-OFFSETS* NIL
  "Alist for dealing with disks with offset origins.
use care! Unit 0 not offset!")

(add-initialization "reset *unit-cylinder-offsets*"
                    '(setq *unit-cylinder-offsets* nil)
                    '(:before-cold))

(add-initialization "reset *unit-cylinder-offsets*"
                    '(setq *unit-cylinder-offsets* nil)
                    '(:system))


;(DEFVAR PAGE-RQB-SIZE (- PAGE-SIZE 1 (FLOOR %DISK-RQ-CCW-LIST 2))) ;NUMBER OF CCWS
(defvar page-rqb-size 100) ;hacked for lambda

;;; Area containing wirable buffers and RQBs
(DEFVAR DISK-BUFFER-AREA (MAKE-AREA ':NAME 'DISK-BUFFER-AREA ':GC ':STATIC :region-size #o200000)
  "Area containing disk RQBs.")

(defvar page-unit :unbound
  "Unit number for PAGE partition.")

(DEFVAR PAGE-OFFSET :UNBOUND
  "Disk address of start of PAGE partition")
(DEFVAR VIRTUAL-MEMORY-SIZE :UNBOUND
  "Size of paging partition in words")

(DEFVAR CURRENT-LOADED-BAND :UNBOUND
  "Remembers %LOADED-BAND through warm-booting")
(DEFVAR DISK-PACK-NAME :UNBOUND
  "Remembers name of pack for PRINT-LOADED-BAND")

(DEFVAR CC-REMOTE-DISK-WRITE-CHECK NIL
  "T => CC remote disk handler does read after write")
(DEFVAR DISK-SHOULD-READ-COMPARE NIL
  "T => read-compare after reads and write done by Lisp programs.")
                                      ;Unfortunately there is a hardware bug with
                                      ;read compares on transfers longer than 1 block.
                                      ;This didn't find any problems while it was on anyway.
                                      ;(Fixed by DC ECO#1)
(DEFVAR DISK-ERROR-RETRY-COUNT 5 "Retry this many times before CERRORing, on disk errors.")
(DEFVAR LET-MICROCODE-HANDLE-DISK-ERRORS T "Use the disk error retry code in the microcode.")

(DEFSUBST RQB-BUFFER (RQB)
  "Returns a 16-bit array whose contents are the data in RQB.
This is an indirect array which overlaps the appropriate portion of RQB."
  (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
(DEFSUBST RQB-8-BIT-BUFFER (RQB)
  "Returns an 8-bit array whose contents are the data in RQB.
This is an indirect array which overlaps the appropriate portion of RQB."
  (ARRAY-LEADER RQB %DISK-RQ-LEADER-8-BIT-BUFFER))
(DEFSUBST RQB-NPAGES (RQB)
  "Returns the data length of RQB, in pages."
  (ARRAY-LEADER RQB %DISK-RQ-LEADER-N-PAGES))

;used to take 2 more arguments: microcode-error-recovery and do-not-offset
(defun disk-read (rqb unit address)
  "Read data from disk UNIT at block ADDRESS into RQB.
The length of data read is simply the number of pages of data in RQB.
UNIT can be either a disk unit number or a function to perform
the transfer.  That is how transfers to other machines' disks are done.
The function receives arguments ':READ, the RQB, and the ADDRESS."
  (cond ((numberp unit)
         (select-processor
           ((:cadr :lambda)
            (wire-disk-rqb rqb (rqb-npages rqb) t t)    ;set modified bits
            (disk-read-wired rqb unit address)
            (unwire-disk-rqb rqb)
            rqb)
           (:explorer
             (nupi-disk-read rqb unit address (io-cmd-n-pages rqb))
             rqb
             )))
        (t
         (send unit :read rqb address))))

(defun disk-read-n-pages (rqb unit address &optional (n-pages (rqb-npages rqb)))
  "Read data from disk UNIT at block ADDRESS into RQB.
The length of data read is simply the number of pages of data in RQB.
UNIT can be either a disk unit number or a function to perform
the transfer.  That is how transfers to other machines' disks are done.
The function receives arguments ':READ, the RQB, and the ADDRESS."
  (if (> n-pages (rqb-npages rqb))
      (ferror "transfer request too large"))
  (cond ((numberp unit)
         (select-processor
           ((:cadr :lambda)
            (wire-disk-rqb rqb n-pages t t)     ;set modified bits
            (disk-read-wired rqb unit address)
            (unwire-disk-rqb rqb)
            rqb)
           (:explorer
             (nupi-disk-read rqb unit address n-pages)
             rqb
             )))
        (t
         (send unit :read rqb address))))

(defun disk-read-physical (rqb unit address)
  (select-processor
    ((:cadr :explorer)
     (disk-read rqb unit address))
    (:lambda
      (cond ((numberp unit)
             (wire-disk-rqb rqb (rqb-npages rqb) t t)   ;set modified bits
             (disk-read-wired rqb unit address t t)
             (unwire-disk-rqb rqb)
             rqb)
            (t
             (send unit :read-physical rqb address))))))

(defun disk-write (rqb unit address)
  "Write data to disk UNIT at block ADDRESS from RQB.
The length of data written is simply the number of pages of data in RQB.
UNIT can be either a disk unit number or a function to perform
the transfer.  That is how transfers to other machines' disks are done.
The function receives arguments ':WRITE, the RQB, and the ADDRESS."
  (cond ((numberp unit)
         (select-processor
           ((:cadr :lambda)
            (wire-disk-rqb rqb)
            (disk-write-wired rqb unit address)
            (unwire-disk-rqb rqb)
            rqb)
           (:explorer
             (nupi-disk-write rqb unit address (io-cmd-n-pages rqb))
             rqb
             )))
        (t
         (send unit :write rqb address))))

(defun disk-write-n-pages (rqb unit address &optional (n-pages (rqb-npages rqb)))
  "Write data to disk UNIT at block ADDRESS from RQB.
The length of data written is simply the number of pages of data in RQB.
UNIT can be either a disk unit number or a function to perform
the transfer.  That is how transfers to other machines' disks are done.
The function receives arguments ':WRITE, the RQB, and the ADDRESS."
  (if (> n-pages (rqb-npages rqb))
      (ferror "transfer request too large"))
  (cond ((numberp unit)
         (select-processor
           ((:cadr :lambda)
            (wire-disk-rqb rqb n-pages)
            (disk-write-wired rqb unit address)
            (unwire-disk-rqb rqb)
            rqb)
           (:explorer
             (nupi-disk-write rqb unit address n-pages)
             rqb
             )))
        (t
         (send unit :write rqb address))))

(defun disk-write-physical (rqb unit address)
  (select-processor
    ((:cadr :explorer)
     (disk-write rqb unit address))
    (:lambda
      (cond ((numberp unit)
             (wire-disk-rqb rqb (rqb-npages rqb))
             (disk-write-wired rqb unit address t t)
             (unwire-disk-rqb rqb)
             rqb)
            (t
             (send unit :write-physical rqb address))))))



(Defun default-disk-unit (unit)
  (if (null unit)
      (select-processor
        ((:cadr :lambda) 0)
        (:explorer (explorer-lod-band-logical-unit)))
    unit)
  )


;;; Internally, RQBs are resources.
(DEFRESOURCE RQB (N-PAGES LEADER-LENGTH)
  :CONSTRUCTOR MAKE-DISK-RQB
  :FREE-LIST-SIZE 50.)

(DEFUN GET-DISK-RQB (&OPTIONAL (N-PAGES 1) (LEADER-LENGTH (LENGTH DISK-RQ-LEADER-QS)))
  "Return an RQB of data length N-PAGES and leader length LEADER-LENGTH.
The leader length is specified only for weird hacks.
Use RETURN-DISK-RQB to release the RQB for re-use."
  (select-processor
    ((:cadr :lambda)
     ;;avoid lossage on consing in
     ;; resource stuff. (specifically parametizer)
     (let ((default-cons-area working-storage-area))
       (allocate-resource 'rqb n-pages leader-length)))
    (:explorer
      (get-io-cmd n-pages))))

;(defun make-disk-rqb (ignore n-pages leader-length)
;  (if (> n-pages page-rqb-size)
;      (ferror 'rqb-too-large "Can't make RQB containing more than ~A pages." page-rqb-size))
;  (let ((rqb) (rqb-data-address) (rqb-buffer-16) (rqb-buffer-8))
;    (setq rqb (make-array (* (1+ n-pages) page-size 2)
;                         :type art-16b
;                         :area disk-buffer-area
;                         :leader-length leader-length
;                         :wireable t))
;    (setq rqb-data-address (%pointer-plus rqb (%pointer-plus (array-data-offset rqb) page-size)))
;    (setq rqb-buffer-16 (make-array (* n-pages page-size 2)
;                                   :type art-16b
;                                   :displaced-to rqb-data-address))
;    (setq rqb-buffer-8 (make-array (* n-pages page-size 4)
;                                  :type art-string
;                                  :displaced-to rqb-data-address))
;    (setf (rqb-npages rqb) n-pages)
;    (setf (rqb-buffer rqb) rqb-buffer-16)
;    (setf (rqb-8-bit-buffer rqb) rqb-buffer-8)
;    rqb))

(DEFUN MAKE-DISK-RQB (IGNORE N-PAGES LEADER-LENGTH)
  (lambda-or-cadr-only)
  (cond ((not (= si:processor-type-code si:cadr-type-code))
         (if (> n-pages page-rqb-size)
             (ferror 'rqb-too-large
                     "rqb's can't be bigger than ~d. pages" page-rqb-size))))
  (LET* ((OVERHEAD (+ 4 4 3 LEADER-LENGTH))
         (ARRAY-LENGTH (* (- (* (1+ N-PAGES) PAGE-SIZE) OVERHEAD) 2))
         RQB-BUFFER
         RQB-8-BIT-BUFFER
         RQB)
    ;; Compute how much overhead there is in the RQB-BUFFER,
    ;; RQB-8-BIT-BUFFER, and in the RQB's leader and header.  4 for the
    ;; RQB-BUFFER indirect-offset array, 4 for the RQB-8-BIT-BUFFER
    ;; indirect-offset array, 3 for the RQB's header, plus the RQB's leader.
    ;; Then set the length (in halfwords) of the array to be sufficient so
    ;; that it plus the overhead is a multiple of the page size, making it
    ;; possible to wire down RQB's.
    (WHEN (> ARRAY-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
      (INCF OVERHEAD 1)
      (DECF ARRAY-LENGTH 2)
      (UNLESS (> ARRAY-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
        (FERROR "Impossible to make this RQB array fit")))
    ;; See if the CCW list will run off the end of the first page, and hence
    ;; not be stored in consecutive physical addresses.
    (IF (> (+ OVERHEAD (FLOOR %DISK-RQ-CCW-LIST 2) N-PAGES) PAGE-SIZE)
        (FERROR 'RQB-TOO-LARGE "CCW list doesn't fit on first RQB page, ~S pages is too many"
                N-PAGES))
    (TAGBODY
     L  (pad-area-to-page-index disk-buffer-area 0)
        (SETQ RQB-BUFFER (MAKE-ARRAY (* PAGE-SIZE 2 N-PAGES)
                                     :TYPE ART-16B
                                     :AREA DISK-BUFFER-AREA
                                     :DISPLACED-TO ""
                                     :DISPLACED-INDEX-OFFSET    ;To second page of RQB
                                        (- ARRAY-LENGTH (* PAGE-SIZE 2 N-PAGES)))
              RQB-8-BIT-BUFFER (MAKE-ARRAY (* PAGE-SIZE 4 N-PAGES)
                                           :TYPE ART-STRING
                                           :AREA DISK-BUFFER-AREA
                                           :DISPLACED-TO ""
                                           :DISPLACED-INDEX-OFFSET
                                                (* 2 (- ARRAY-LENGTH (* PAGE-SIZE 2 N-PAGES))))
              RQB (MAKE-ARRAY ARRAY-LENGTH
                              :AREA DISK-BUFFER-AREA
                              :TYPE ART-16B
                              :LEADER-LENGTH LEADER-LENGTH))
        (WHEN ( (%REGION-NUMBER RQB-BUFFER)    ;make sure a new region
                 (%REGION-NUMBER RQB))          ;didn't screw us completely
          ;; Screwed! Try again.  Make sure don't lose same way again by
          ;;  using up region that didnt hold it.
;         (format t "~&***rqb-screw***~%")
;         (RETURN-ARRAY (PROG1 RQB (SETQ RQB NIL)))
          (LET ((RN (%REGION-NUMBER RQB-BUFFER)))
;           (RETURN-ARRAY (PROG1 RQB-BUFFER (SETQ RQB-BUFFER NIL)))
            (%USE-UP-REGION RN))
          (GO L)))
    (%P-STORE-CONTENTS-OFFSET RQB RQB-BUFFER 1) ;Displace RQB-BUFFER to RQB
    (%P-STORE-CONTENTS-OFFSET RQB RQB-8-BIT-BUFFER 1)
    (SETF (rqb-npages rqb) N-PAGES)
    (SETF (rqb-buffer rqb) RQB-BUFFER)
    (SETF (rqb-8-bit-buffer rqb) RQB-8-BIT-BUFFER)
    RQB))

;;; Use this to recover if the free pointer is off a page boundary.
(DEFUN %USE-UP-REGION (REGION-NUMBER)
  (SETF (%REGION-FREE-POINTER REGION-NUMBER)
        (AREF #'SYSTEM:REGION-LENGTH REGION-NUMBER)))

(defun pad-area-to-page-index (area index)
  (for-every-region-in-area (region area)
    (loop while ( (ldb (BYTE 8. 0.) (%region-free-pointer region)) index)
                ;*** should do CONS-IN-REGION !!  This is at least conservative.
          do (make-array 0 :area area))))

;;; Return a buffer to the free list
(DEFUN RETURN-DISK-RQB (RQB)
  "Release RQB for reuse."
  (select-processor
    (:cadr
      (COND ((NOT (NULL RQB))   ;Allow NIL's to be handed to the function just in case
             (UNWIRE-DISK-RQB RQB)
             (DEALLOCATE-RESOURCE 'RQB RQB))))
    (:lambda
      (COND ((NOT (NULL RQB))   ;Allow NIL's to be handed to the function just in case
             (UNWIRE-DISK-RQB RQB)
             (DEALLOCATE-RESOURCE 'RQB RQB))))
    (:explorer
      (return-io-cmd rqb)))
  NIL)

(DEFCONST DISK-LABEL-RQB-PAGES 3)

(DEFUN COUNT-FREE-RQBS (N-PAGES)
  "Return the number of free RQBs there are whose data length is N-PAGES."
  (lambda-or-cadr-only)
  (WITHOUT-INTERRUPTS
    (LOOP WITH RESOURCE = (GET 'RQB 'DEFRESOURCE)
          WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
          FOR I FROM 0 BELOW N-OBJECTS
          COUNT (= (CAR (RESOURCE-PARAMETERS RESOURCE I)) N-PAGES))))

;;; Set up ccw list, wire down pages
(DEFUN WIRE-DISK-RQB (RQB &OPTIONAL (N-PAGES (rqb-npages rqb))
                                    (WIRE-P T)
                                    SET-MODIFIED
                          &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
                               (LOW (%POINTER-DIFFERENCE
                                      (%POINTER RQB)
                                      (%POINTER-PLUS (ARRAY-LEADER-LENGTH RQB) 2)))
                               (HIGH (%POINTER-PLUS (%POINTER RQB)
                                      (%POINTER-PLUS 1
                                       (%POINTER-PLUS LONG-ARRAY-FLAG
                                                      (FLOOR (ARRAY-LENGTH RQB) 2))))))
  (lambda-or-cadr-only)
  (DO ((LOC (LOGAND LOW (- PAGE-SIZE)) (%POINTER-PLUS LOC PAGE-SIZE)))
      (( (%POINTER-DIFFERENCE LOC HIGH) 0))
    (%WIRE-PAGE LOC WIRE-P SET-MODIFIED))
  ;; Having wired the rqb, if really wiring set up CCW-list N-PAGES long
  ;; and CLP to it, but if really unwiring make CLP point to NXM as err check
  (IF (NOT WIRE-P)
      (SETF (AREF RQB %DISK-RQ-CCW-LIST-POINTER-LOW)  #o177777  ;Just below TV buffer
            (aref RQB %DISK-RQ-CCW-LIST-POINTER-HIGH) #o76)
    (DO ((CCWX 0 (1+ CCWX))
         (VADR (%POINTER-PLUS LOW PAGE-SIZE)
               (%POINTER-PLUS VADR PAGE-SIZE))  ;Start with 2nd page of rqb array
         (PADR))
        (( CCWX N-PAGES)       ;Done, set END in last CCW
         (SETQ PADR (%PHYSICAL-ADDRESS (%POINTER-PLUS (%POINTER RQB)
                                        (%POINTER-PLUS 1
                                         (%POINTER-PLUS LONG-ARRAY-FLAG
                                                        (FLOOR %DISK-RQ-CCW-LIST 2))))))
         (SETF (aref RQB %DISK-RQ-CCW-LIST-POINTER-LOW) PADR)
         (SETF (aref RQB %DISK-RQ-CCW-LIST-POINTER-HIGH) (LSH PADR -16.)))
      (SETQ PADR (%PHYSICAL-ADDRESS VADR))
      (SETF (AREF RQB (+ %DISK-RQ-CCW-LIST (* 2 CCWX)))
            (+ (LOGAND (- PAGE-SIZE) PADR)              ;Low 16 bits
               (IF (= CCWX (1- N-PAGES)) 0 1)))         ;Chain bit
      (SETF (AREF RQB (+ %DISK-RQ-CCW-LIST 1 (* 2 CCWX)))
            (LSH PADR -16.)))))                         ;High 6 bits



(DEFUN UNWIRE-DISK-RQB (RQB)
  (lambda-or-cadr-only)
  (WIRE-DISK-RQB RQB NIL NIL))

;;; Disk geometry is remembered in the following arrays.
;;; The DISK-READ-WIRED function contains a kludge that it notices if you
;;; are reading the label, and automatically adjusts the geometry
;;; for that unit from the label.  You can also store explicitly
;;; in the arrays if you like.

(DEFVAR DISK-SECTORS-PER-TRACK-ARRAY)
(DEFVAR DISK-HEADS-PER-CYLINDER-ARRAY)

;****
(UNLESS (BOUNDP 'DISK-SECTORS-PER-TRACK-ARRAY)
  (SETQ DISK-SECTORS-PER-TRACK-ARRAY (MAKE-ARRAY #o20 :TYPE 'ART-8B :INITIAL-ELEMENT 17.)
        DISK-HEADS-PER-CYLINDER-ARRAY (MAKE-ARRAY #o20 :TYPE 'ART-8B :INITIAL-ELEMENT 5)))

;;; These must be called with the buffer already wired, which specifies the
;;; number of pages implicitly (usually 1 of course)
;;; For now, error-handling is rudimentary, fix later
;;; Note!! If you call this directly, you better make sure the modified bits for the
;;; pages transferred get set!!!
(DEFUN DISK-READ-WIRED (RQB UNIT ADDRESS
                        &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
                                  DO-NOT-OFFSET
                        &AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
                             (HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  (select-processor
    ((:cadr :lambda)
      (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
                (LOGIOR %DISK-COMMAND-READ
                        (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
                "read"
                nil
                do-not-offset))
    (:explorer (ferror "obsolete"))
    ))

(DEFUN DISK-WRITE-WIRED (RQB UNIT ADDRESS
                   &OPTIONAL (MICROCODE-ERROR-RECOVERY LET-MICROCODE-HANDLE-DISK-ERRORS)
                   do-not-offset
                   &AUX (SECTORS-PER-TRACK (AREF DISK-SECTORS-PER-TRACK-ARRAY UNIT))
                        (HEADS-PER-CYLINDER (AREF DISK-HEADS-PER-CYLINDER-ARRAY UNIT)))
  (select-processor
    ((:cadr :lambda)
      (DISK-RUN RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER
                (LOGIOR %DISK-COMMAND-WRITE
                        (IF MICROCODE-ERROR-RECOVERY %DISK-COMMAND-DONE-INTERRUPT-ENABLE 0))
                "write"
                nil
                do-not-offset))
    (:explorer
      (ferror "obsolete"))))

;*** set heads/tracks before disk-read
(defun get-cylinder-offset-for-unit (unit)
  (lambda-only)
  (let ((offset (assq unit *unit-cylinder-offsets*)))
    (cond ((zerop unit) 0)
          ((null offset)
           (WITH-DISK-RQB (RQB)
                                                ;*** change to disk-read-physical
             (let ((*unit-cylinder-offsets* (cons (cons unit 0) *unit-cylinder-offsets*)))
               (disk-read rqb unit 22.))
             (let ((r (rqb-8-bit-buffer rqb)))
               (cond ((null (string-equal r "FOOBLISP" :start1 0 :start2 0 :end1 8 :end2 8))
                      (ferror "disk unit ~d. does not have a mini label" unit)))
               (setq offset (+ (aref r 8)
                               (ash (aref r 9) 8)
                               (ash (aref r 10.) 16.)
                               (ash (aref r 11.) 24.)))))
           (push (cons unit offset) *unit-cylinder-offsets*)
           offset)
          (t
           (cdr offset)))))

(defstruct (mini-label (:type :named-array)
                       (:conc-name mini-label-)
                       :size-symbol)
  mini-string
  mini-label-length                             ;in bytes
  label-block
  backup-label-block
  bad-track-list
  spare1
  number-of-usable-tracks
  disk-type
  number-of-heads
  number-of-sectors
  number-of-cylinders
  gap1
  gap2
  interleave
  skew
  sector-size
  bad-track-list-2
  backup-label-track
  )


(defun print-new-mini-label (unit)
  (with-decoded-disk-unit (unit unit "read")
    (with-disk-rqb (rqb)
      (disk-read-physical rqb unit 10.)
      (when (not (string-equal (rqb-8-bit-buffer rqb) "MINI" :end1 4))
        (ferror nil "no block 10 mini label"))
      (labels ((get-32-bits (rqb index)
                            (dpb (aref (rqb-buffer rqb) (1+ (* index 2)))
                                 (byte 16. 16.)
                                 (aref (rqb-buffer rqb) (* index 2)))))
        (let ((mini-label (make-mini-label)))
          (dotimes (i (min (1- mini-label-size) (floor (get-32-bits rqb 1) 4)))
            (aset (get-32-bits rqb i) mini-label (1+ i)))
          (format t "~&(Numbers in decimal)")
          (format t "~&Label block ~d" (mini-label-label-block mini-label))
          (format t "~&Backup label block ~d" (mini-label-backup-label-block mini-label))
          (format t "~&Bad track list ~d" (mini-label-bad-track-list mini-label))
          (format t "~&Usable tracks ~d" (mini-label-number-of-usable-tracks mini-label))
          (format t "~&Disk type ~d" (mini-label-disk-type mini-label))
          (format t "~&Heads ~d" (mini-label-number-of-heads mini-label))
          (format t "~&Sectors ~d" (mini-label-number-of-sectors mini-label))
          (format t "~&Cylinders ~d" (mini-label-number-of-cylinders mini-label))
          (format t "~&Interleave ~d" (mini-label-interleave mini-label))
          (format t "~&Skew ~d" (mini-label-skew mini-label))
          (format t "~&Sector size ~d" (mini-label-sector-size mini-label))
          (format t "~&Other bad track list ~d" (mini-label-bad-track-list-2 mini-label))
          (format t "~&Backup label track ~d" (mini-label-backup-label-track mini-label))
          )))))


(DEFUN DISK-RUN (RQB UNIT ADDRESS SECTORS-PER-TRACK HEADS-PER-CYLINDER CMD CMD-NAME
                 &OPTIONAL NO-ERROR-CHECKING do-not-offset
                 &AUX ADR CYLINDER SURFACE SECTOR ERROR-COUNT ER-H ER-L)
  (lambda-or-cadr-only)
  (PROG (FINAL-ADDRESS FINAL-CYLINDER FINAL-SURFACE FINAL-SECTOR MICROCODE-ERROR-RECOVERY
         transfer-size)
        (setq transfer-size (disk-transfer-size rqb))
        (cond ((and (= processor-type-code lambda-type-code)
                    (> transfer-size page-rqb-size))
               (ferror 'rqb-too-big
                       "Attempt to do a disk transfer of ~d. pages, but only allowed ~d."
                       transfer-size page-rqb-size)))
        (SETQ FINAL-ADDRESS (+ ADDRESS (1- transfer-size))   ;count length of CCW.
              FINAL-SECTOR (\ FINAL-ADDRESS SECTORS-PER-TRACK)
              ADR (FLOOR FINAL-ADDRESS SECTORS-PER-TRACK)
              FINAL-SURFACE (\ ADR HEADS-PER-CYLINDER)
              FINAL-CYLINDER (FLOOR ADR  HEADS-PER-CYLINDER)
              MICROCODE-ERROR-RECOVERY (BIT-TEST %DISK-COMMAND-DONE-INTERRUPT-ENABLE CMD))
     FULL-RETRY
        (SETQ ERROR-COUNT DISK-ERROR-RETRY-COUNT)
     PARTIAL-RETRY
        (SETQ SECTOR (\ ADDRESS SECTORS-PER-TRACK)
              ADR (FLOOR ADDRESS SECTORS-PER-TRACK)
              SURFACE (\ ADR HEADS-PER-CYLINDER)
              CYLINDER (+ (FLOOR ADR HEADS-PER-CYLINDER)
                          (cond ((zerop unit)
                                 (if do-not-offset #o4000 0))   ;4000 is the physical flag
                                                ;(see %disk-op microcode)
                                (t
                                 (if do-not-offset 0 (get-cylinder-offset-for-unit unit))))))
        (SETF (AREF RQB %DISK-RQ-COMMAND) CMD
              (AREF RQB %DISK-RQ-SURFACE-SECTOR) (+ (LSH SURFACE 8) SECTOR)
              (AREF RQB %DISK-RQ-UNIT-CYLINDER) (+ (LSH UNIT 12.) (LDB (BYTE 12. 0.) CYLINDER))
              (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER) 0
              (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR) 0)
        (DISK-RUN-1 RQB UNIT)
        (WHEN NO-ERROR-CHECKING
          (RETURN NIL))
        (SETQ ER-H (AREF RQB %DISK-RQ-STATUS-HIGH)
              ER-L (AREF RQB %DISK-RQ-STATUS-LOW))
        (AND (= CMD %DISK-COMMAND-READ-COMPARE)
             (LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE ER-H)
             (SETQ ER-H (DPB 0 %%DISK-STATUS-HIGH-INTERNAL-PARITY ER-H)))
        (COND ((OR (BIT-TEST %DISK-STATUS-HIGH-ERROR ER-H)
                   (BIT-TEST %DISK-STATUS-LOW-ERROR ER-L))
               (OR (ZEROP (SETQ ERROR-COUNT (1- ERROR-COUNT)))
                   MICROCODE-ERROR-RECOVERY
                   (GO PARTIAL-RETRY))
               (CERROR ':RETRY-DISK-OPERATION NIL 'SYS:DISK-ERROR
                       "Disk ~A error unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Type ~:C to retry."
                       CMD-NAME UNIT
                       (LDB (BYTE 12. 0.) (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
                       (LDB (BYTE 8. 8.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                       (LDB (BYTE 8. 0.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                       (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
                                           (AREF RQB %DISK-RQ-STATUS-HIGH))
                       #/RESUME)
               (GO FULL-RETRY))
              ((AND ( PROCESSOR-TYPE-CODE LAMBDA-TYPE-CODE)
                    (OR ( FINAL-CYLINDER
                           (LDB (BYTE 12. 0.) (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER)))
                        ( FINAL-SURFACE
                           (LDB (BYTE 8. 8.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR)))
                        ( FINAL-SECTOR
                           (LDB (BYTE 8. 0.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR)))))
               (CERROR ':RETRY-DISK-OPERATION NIL 'SYS:DISK-ERROR
                       "Disk ~A error unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Failed to complete operation, final disk address should be ~D, ~D, ~D.
 Type ~:C to retry."
                       CMD-NAME UNIT
                       (LDB (BYTE 12. 0.) (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
                       (LDB (BYTE 8. 8.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                       (LDB (BYTE 8. 0.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                       (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
                                           (AREF RQB %DISK-RQ-STATUS-HIGH))
                       FINAL-CYLINDER FINAL-SURFACE FINAL-SECTOR
                       #/RESUME)
               (GO FULL-RETRY))
              ((AND DISK-SHOULD-READ-COMPARE
                    (OR (= CMD %DISK-COMMAND-READ) (= CMD %DISK-COMMAND-WRITE)))
               (SETF (AREF RQB %DISK-RQ-COMMAND) %DISK-COMMAND-READ-COMPARE)
               (DISK-RUN-1 RQB UNIT)
               (COND ((OR (BIT-TEST %DISK-STATUS-HIGH-ERROR (AREF RQB %DISK-RQ-STATUS-HIGH))
                          (BIT-TEST %DISK-STATUS-LOW-ERROR (AREF RQB %DISK-RQ-STATUS-LOW)))
                      (OR (ZEROP (SETQ ERROR-COUNT (1- ERROR-COUNT)))
                          (GO PARTIAL-RETRY))
                      (CERROR ':RETRY-DISK-OPERATION NIL 'SYS:DISK-ERROR
                              "Disk error during read//compare after ~A unit ~D, cyl ~D., surf ~D., sec ~D.,~%  status ~A
 Type ~:C to retry."
                              CMD-NAME UNIT
                              (LDB (BYTE 12. 0.) (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
                              (LDB (BYTE 8. 8.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                              (LDB (BYTE 8. 0.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                              (DECODE-DISK-STATUS (AREF RQB %DISK-RQ-STATUS-LOW)
                                                  (AREF RQB %DISK-RQ-STATUS-HIGH))
                              #/RESUME)
                      (GO FULL-RETRY))
                     ((LDB-TEST %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
                                (AREF RQB %DISK-RQ-STATUS-HIGH))
                      ;; A true read/compare difference really shouldn't happen, complain
                      (CERROR ':RETRY-DISK-OPERATION NIL 'SYS:DISK-ERROR
                              "Disk read//compare error unit ~D, cyl ~D., surf ~D., sec ~D.
 Type ~:C to retry."
                              UNIT
                              (LDB (BYTE 12. 0.) (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER))
                              (LDB (BYTE 8. 8.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                              (LDB (BYTE 8. 0.) (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
                              #/RESUME)
                      (GO FULL-RETRY)))))))

;;; This knows about a second disk controller, containing units 10-17,
;;; which is at an XBUS address 4 less than the address of the first controller.
(DEFUN DISK-RUN-1 (RQB UNIT)
  (COND ((< UNIT 8)
         (%DISK-OP RQB)
         (DO () ((NOT (ZEROP (AREF RQB %DISK-RQ-DONE-FLAG)))))) ;Loop until disk op complete
        (T ;; Await disk control ready
         (select-processor
           (:CADR
           (DO () ((BIT-TEST 1 (%XBUS-READ #o377770))))
           ;; Write 4 words into disk control
           (%BLT (%MAKE-POINTER-OFFSET DTP-FIX RQB
                                       (+ (LSH %DISK-RQ-COMMAND -1) (ARRAY-DATA-OFFSET RQB)))
                 (+ IO-SPACE-VIRTUAL-ADDRESS #o377770)
                 4 1)
           ;; Await disk control done
           (DO () ((BIT-TEST 1 (%XBUS-READ #o377770))))
           ;; Read 4 words from disk control
           (%BLT (+ IO-SPACE-VIRTUAL-ADDRESS #o377770)
                 (%MAKE-POINTER-OFFSET DTP-FIX RQB
                                       (+ (LSH %DISK-RQ-STATUS-LOW -1)
                                          (ARRAY-DATA-OFFSET RQB)))
                 4 1))
           ((:lambda :explorer)
            (ferror "Only know about multiple disk controls on CADR"))))))

(DEFUN DISK-TRANSFER-SIZE (RQB)
  (lambda-or-cadr-only)
  (DO ((CCWP %DISK-RQ-CCW-LIST (+ CCWP 2))
       (COUNT 1 (1+ COUNT)))
      ((ZEROP (LOGAND (AREF RQB CCWP) 1)) COUNT)))

;;** even tho the below function is CADRoid, it is still useful since the microcode
;; maps some disk errors into their CADR equivalents.  Should be converted to
;; to a conceptualized disk controller someday.  **
;;; Return a string representation of the disk status register (pair of halfwords)
;;; Put the most relevant error condition bit first, followed by all other 1 bits
;;; Except for Idle, Interrupt, Read Compare Difference (and block-counter) which
;;; are not interesting as errors.
;;; Also if the transfer is aborted, leave out Internal Parity which is always on.
(DEFUN DECODE-DISK-STATUS (LOW HIGH)
  (WITH-OUTPUT-TO-STRING (S)
    (LOOP FOR (NAME PPSS HALF) IN '(("Nonexistent-Memory" %%DISK-STATUS-HIGH-NXM T)
                                    ("Memory-Parity" %%DISK-STATUS-HIGH-MEM-PARITY T)
                                    ("Multiple-Select" %%DISK-STATUS-LOW-MULTIPLE-SELECT)
                                    ("No-Select" %%DISK-STATUS-LOW-NO-SELECT)
                                    ("Fault" %%DISK-STATUS-LOW-FAULT)
                                    ("Off-line"  %%DISK-STATUS-LOW-OFF-LINE)
                                    ("Off-Cylinder" %%DISK-STATUS-LOW-OFF-CYLINDER)
                                    ("Seek-Error" %%DISK-STATUS-LOW-SEEK-ERROR)
                                    ("Start-Block-Error" %%DISK-STATUS-LOW-START-BLOCK-ERROR)
                                    ("Overrun" %%DISK-STATUS-LOW-OVERRUN)
                                    ("Header-Compare" %%DISK-STATUS-HIGH-HEADER-COMPARE T)
                                    ("Header-ECC" %%DISK-STATUS-HIGH-HEADER-ECC T)
                                    ("ECC-Hard" %%DISK-STATUS-HIGH-ECC-HARD T)
                                    ("ECC-Soft" %%DISK-STATUS-LOW-ECC-SOFT)
                                    ("Timeout" %%DISK-STATUS-LOW-TIMEOUT)
                                    ("Internal-Parity" %%DISK-STATUS-HIGH-INTERNAL-PARITY T)
                                    ("Transfer-Aborted" %%DISK-STATUS-LOW-TRANSFER-ABORTED)
                                    ("CCW-Cycle" %%DISK-STATUS-HIGH-CCW-CYCLE T)
                                    ("Read-Only" %%DISK-STATUS-LOW-READ-ONLY)
                                    ("Sel-Unit-Attention"
                                        %%DISK-STATUS-LOW-SEL-UNIT-ATTENTION)
                                    ("Any-Unit-Attention" %%DISK-STATUS-LOW-ATTENTION))
          WITH FLAG = NIL
          WHEN (AND (LDB-TEST (SYMEVAL PPSS) (IF (NULL HALF) LOW HIGH))
                    (OR (NEQ PPSS '%%DISK-STATUS-HIGH-INTERNAL-PARITY)
                        (NOT (LDB-TEST %%DISK-STATUS-LOW-TRANSFER-ABORTED LOW))))
            DO (IF FLAG (SEND S ':STRING-OUT "  "))
               (SEND S ':STRING-OUT NAME)
               (SETQ FLAG T))))

;;; Unit is unit number on local disk controller or a string.
;;; If a string, CC means hack over debug interface
;;;              TEST is a source of test data
;;;              MT is magtape
;;;       otherwise it is assumed to be the host name of a remote machine.

(DEFUN DECODE-UNIT-ARGUMENT (UNIT USE &OPTIONAL (DISK-INIT-P NIL) (WRITE-P NIL)
                             &AUX TEM)
  "First value is decoded unit. If second value is NIL the caller should call
DISPOSE-OF-UNIT eventually. If T then the UNIT must have already been decoded."
  (when (typep unit 'host)
    (setq unit (send unit :name)))
  (COND ((AND (STRINGP UNIT)
              (STRING-EQUAL UNIT "MT" :END1 2))
         (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT))
         (LET ((CC-DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
           (COND ((NOT (ZEROP CC-DISK-UNIT))
                  (FERROR "MT can only talk to unit zero")))
           (values (FS:MAKE-BAND-MAGTAPE-HANDLER WRITE-P) NIL)))
        ((AND (STRINGP UNIT)
              (FBOUNDP 'SI:MAKE-LAM-DISK-UNIT)
              (MEM #'STRING-EQUAL UNIT '("EXP" "LAM")))
         (VALUES (SI:MAKE-LAM-DISK-UNIT UNIT USE DISK-INIT-P WRITE-P) NIL))
        ((STRINGP UNIT)
         (LET ((HOST (SI:PARSE-HOST (SUBSTRING UNIT 0 (SETQ TEM (STRING-SEARCH-CHAR #/SP UNIT)))))
               (DISK-UNIT (IF (NULL TEM) 0 (READ-FROM-STRING UNIT NIL (1+ TEM)))))
           (VALUES (NET:GET-REMOTE-DISK-UNIT HOST DISK-UNIT USE DISK-INIT-P WRITE-P) NIL)))
        ((numberp unit)
         (values (require-online-numeric-unit unit) t))
        ('ELSE
         (VALUES UNIT T))))

; Copied by hand from SYS: SYS2; PATCH on 10/9/86: Avoid site inititialization lossage while
; building cold load.  -rpp
(defun get-boot-tape-version-number ()
  "return boot tape version number, or nil if not available
   version is 32-bits, major.minor"
  (select-processor
    (:lambda
      (when (and (> (%system-configuration-size *sys-conf*)
                    %system-configuration-newboot-version-number)
                 (neq 0 (ldb (BYTE 16. 16.) (%system-configuration-newboot-version-number *sys-conf*))))
        (%system-configuration-newboot-version-number *sys-conf*)))
    ((:explorer :cadr))))

(defun require-online-numeric-unit (unit)
  ;; if sdu boot version is >= 3.12 then
  ;; the info in sys-conf will tell us if the unit is online or not.
  (declare (eh:error-reporter))
  (let ((b (get-boot-tape-version-number)))
    (cond ((null b) unit)
          ((and (not (< (ldb (byte 16. 16.) b) 3))
                (or (> (ldb (byte 16. 16.) b) 3)
                    (> (ldb (byte 16. 0) b) 11.)))
           (check-arg unit (and (fixp unit) (<= 0 unit 7)) "a unit number from 0 through 7")
           (check-arg unit (plusp (funcall (aref #(%system-configuration-disk-unit-0-initialized
                                                    %system-configuration-disk-unit-1-initialized
                                                    %system-configuration-disk-unit-2-initialized
                                                    %system-configuration-disk-unit-3-initialized
                                                    %system-configuration-disk-unit-4-initialized
                                                    %system-configuration-disk-unit-5-initialized
                                                    %system-configuration-disk-unit-6-initialized
                                                    %system-configuration-disk-unit-7-initialized)
                                                 unit)
                                           *sys-conf*))
                      "an online unit")
           unit)
          ('else
           unit))))

(NET:DEFINE-NETWORK-FUNCTION (NET:GET-REMOTE-DISK-UNIT :CHAOS) (HOST DISK-UNIT USE INITP WRITE-P)
  INITP WRITE-P
  (LET ((REMOTE-DISK-CONN (CHAOS:CONNECT HOST "REMOTE-DISK" 25.))
        (REMOTE-DISK-STREAM)
        (REMOTE-DISK-UNIT DISK-UNIT))
    (DECLARE (SPECIAL REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT))
    (AND (STRINGP REMOTE-DISK-CONN)
         (FERROR "Cannot connect to ~S: ~A" HOST REMOTE-DISK-CONN))
    (SETQ REMOTE-DISK-STREAM (CHAOS:MAKE-STREAM REMOTE-DISK-CONN))
    (FORMAT REMOTE-DISK-STREAM "SAY Disk being hacked remotely by ~A@~O -- ~A~%"
            USER-ID
            (GET-HOST-FROM-ADDRESS CHAOS:MY-ADDRESS ':CHAOS)
            USE)
    (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
    (CLOSURE '(REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT)
             'REMOTE-DISK-HANDLER)))


(DEFUN DISPOSE-OF-UNIT (UNIT)
  (if unit (OR (NUMBERP UNIT) (SEND UNIT ':DISPOSE))))

;;; :READ-COMPARE not supported, nothing uses it.
(DEFUN REMOTE-DISK-HANDLER (OP &REST ARGS)
  (DECLARE (SPECIAL REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT))
  (SELECTQ OP
    ((:READ :read-physical)
     (LET ((RQB (CAR ARGS)))
       (LET ((BLOCK (CADR ARGS))
             (N-BLOCKS (rqb-npages rqb)))
         (FORMAT REMOTE-DISK-STREAM "READ~:[~;-PHYSICAL~] ~D ~D ~D~%"
                 (eq op :read-physical)
                 REMOTE-DISK-UNIT BLOCK N-BLOCKS)
         (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
         (DO ((BLOCK BLOCK (1+ BLOCK))
              (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
              (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
              (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
              (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
             ((ZEROP N-BLOCKS))
           ;; Get 3 packets and form a block in the buffer
           ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
           ;; Advance magic strings to next block
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-1 3)
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-2 3)
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-3 3)))))
    (:WRITE (LET ((RQB (CAR ARGS)))
              (LET ((BLOCK (CADR ARGS))
                    (N-BLOCKS (rqb-npages rqb)))
                (FORMAT REMOTE-DISK-STREAM "WRITE ~D ~D ~D~%" REMOTE-DISK-UNIT BLOCK N-BLOCKS)
                (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
                (DO ((BLOCK BLOCK (1+ BLOCK))
                     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
                     (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
                     (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
                     (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
                    ((ZEROP N-BLOCKS))
                  ;; Transmit three packets from block in buffer
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
                  ;; Advance magic strings to next block
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-1 3)
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-2 3)
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-3 3)))))
    (:DISPOSE (CHAOS:CLOSE-CONN REMOTE-DISK-CONN))
    (:UNIT-NUMBER REMOTE-DISK-UNIT)
    (:MACHINE-NAME
     (GET-HOST-FROM-ADDRESS (CHAOS:FOREIGN-ADDRESS REMOTE-DISK-CONN) ':CHAOS))
    (:SAY
      (FORMAT REMOTE-DISK-STREAM "SAY ~A~%" (CAR ARGS))
      (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT))
    (:HANDLES-LABEL NIL)
    ))

(DEFUN RECEIVE-PARTITION-PACKET (CONN INTO)
  (LET ((PKT (CHAOS:GET-NEXT-PKT CONN)))
    (AND (NULL PKT) (FERROR "Connection ~S broken" CONN))
    (SELECT (CHAOS:PKT-OPCODE PKT)
      (CHAOS:DAT-OP
       (COPY-ARRAY-CONTENTS (CHAOS:PKT-STRING PKT) INTO)
       (LET ((CORRECT (AREF PKT (+ (FLOOR (ARRAY-LENGTH INTO) 2) #o10)))
             (ACTUAL (CHECKSUM-STRING INTO)))
         (OR (= CORRECT ACTUAL)
             (FORMAT T "~&Checksum error, correct=~O, actual=~O~%" CORRECT ACTUAL)))
       (CHAOS:RETURN-PKT PKT))
      (CHAOS:EOF-OP
       (CHAOS:RETURN-PKT PKT)
       (THROW 'EOF NIL))
      ;; +++ What about putting these packets back? +++
      (chaos:cls-op  ;; got some kind of error
       (Ferror "Remote disk - ~a" (CHAOS:PKT-STRING PKT)))
      (OTHERWISE
        (FERROR "~S is illegal packet opcode, pkt ~S, received for connection ~S"
                (CHAOS:PKT-OPCODE PKT) PKT CONN)))))

(DEFUN TRANSMIT-PARTITION-PACKET (CONN OUTOF)
  (LET ((PKT (CHAOS:GET-PKT)))
    (COPY-ARRAY-CONTENTS OUTOF (CHAOS:PKT-STRING PKT))
    (SETF (AREF PKT (+ (FLOOR (ARRAY-LENGTH OUTOF) 2) #o10)) (CHECKSUM-STRING OUTOF))
    (SETF (CHAOS:PKT-NBYTES PKT) (+ (ARRAY-LENGTH OUTOF) 2))
    (CHAOS:SEND-PKT CONN PKT)))

(DEFUN CHECKSUM-STRING (STR)
  (DO ((CKSM 0 (+ (AREF STR I) CKSM))
       (I 0 (1+ I))
       (N (ARRAY-LENGTH STR)))
      (( I N) (LOGAND #o177777 CKSM))))

;(DEFUN CC-DISK-HANDLER (OP &REST ARGS)
;  (DECLARE (SPECIAL CC-DISK-UNIT CC-DISK-INIT-P CADR:CC-DISK-LOWCORE CADR:CC-DISK-TYPE))
;  (SELECTQ OP
;    (:READ (LET ((RQB (CAR ARGS)))
;            (LET ((BLOCK (CADR ARGS))
;                  (N-BLOCKS (rqb-npages rqb)))
;              (DO ((BLOCK BLOCK (1+ BLOCK))
;                   (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
;                   (BUF (RQB-BUFFER RQB))
;                   (BUF-IDX -1))
;                  ((ZEROP N-BLOCKS))
;                (CADR:CC-DISK-READ BLOCK CADR:CC-DISK-LOWCORE 1)  ;Was 2 - doesn't that clobber the block above LOWCORE?
;                ;; Following code transmogrified from DBG-READ-XBUS and DBG-READ.
;                ;; You don't really think this would be reasonable via DL11 or debug kludge,
;                ;; do you?
;                ;;  Yes it is reasonable, you total fool!  Hacking the disk label is one of
;                ;;  the most useful things to do.  You really made me do alot of work, and
;                ;;  it was not appreciated one bit.  --HIC
;                (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
;                    (LET ((UBUS-WD-LOC
;                            (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
;                                                  (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
;                      (%UNIBUS-WRITE #o766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
;                      (DOTIMES (W #o400)
;                        (%UNIBUS-WRITE #o766114 UBUS-WD-LOC)
;                        (SETF (AREF BUF (INCF BUF-IDX)) (%UNIBUS-READ #o766100))
;                        (%UNIBUS-WRITE #o766114 (INCF UBUS-WD-LOC))
;                        (SETF (AREF BUF (INCF BUF-IDX)) (%UNIBUS-READ #o766100))
;                        (INCF UBUS-WD-LOC)))
;                    (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
;                         (WORD)
;                         (W 0 (1+ W)))
;                        (( W #o400))
;                      (SETQ WORD (CADR:PHYS-MEM-READ ADR))
;                      (SETF (AREF BUF (INCF BUF-IDX)) (LOGAND #o177777 WORD))
;                      (SETF (AREF BUF (INCF BUF-IDX)) (LDB #o2020 WORD))))))))
;    (:WRITE (LET ((RQB (CAR ARGS)))
;             (LET ((BLOCK (CADR ARGS))
;                   (N-BLOCKS (rqb-npages rqb)))
;               (DO ((BLOCK BLOCK (1+ BLOCK))
;                    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
;                    (BUF (RQB-BUFFER RQB))
;                    (BUF-IDX -1))
;                   ((ZEROP N-BLOCKS))
;                 (IF (EQ CADR:DBG-ACCESS-PATH 'CADR:BUSINT)
;                     (LET ((UBUS-WD-LOC
;                             (LSH (CADR:DBG-SETUP-UNIBUS-MAP 17
;                                                   (ASH CADR:CC-DISK-LOWCORE 8)) -1)))
;                       (%UNIBUS-WRITE #o766110 0)  ;high unibus adr bit and DBG-NXM-INHIBIT
;                       (DOTIMES (W #o400)
;                         (%UNIBUS-WRITE #o766114 UBUS-WD-LOC)
;                         (%UNIBUS-WRITE #o766100 (AREF BUF (INCF BUF-IDX)))
;                         (%UNIBUS-WRITE #o766114 (INCF UBUS-WD-LOC))
;                         (%UNIBUS-WRITE #o766100 (AREF BUF (INCF BUF-IDX)))
;                         (INCF UBUS-WD-LOC)))
;                     (DO ((ADR (ASH CADR:CC-DISK-LOWCORE 8) (1+ ADR))
;                          (WORD)
;                          (W 0 (1+ W)))
;                         (( W #o400))
;                       (SETQ WORD (DPB (AREF BUF (SETQ BUF-IDX (+ 2 BUF-IDX)))
;                                       #o2020
;                                       (AREF BUF (1- BUF-IDX))))
;                       (CADR:PHYS-MEM-WRITE ADR WORD)))
;                 ;; If writing label, init some params such as CC:BLOCKS-PER-CYLINDER.
;                 (IF (ZEROP BLOCK)
;                     (CC:READ-LABEL-1 CADR:CC-DISK-LOWCORE))
;       RETRY     (CADR:CC-DISK-WRITE BLOCK CADR:CC-DISK-LOWCORE 1)
;                 (COND ((AND CC-REMOTE-DISK-WRITE-CHECK
;                             (NULL (CADR:CC-DISK-READ BLOCK
;                                                      (1+ CADR:CC-DISK-LOWCORE)
;                                                      1)))
;                        (GO RETRY)))           ;read it back to let hardware check ECC, etc.
;                 ))))

;    (:DISPOSE (COND ((NULL CC-DISK-INIT-P)
;                    (CADR:CC-DISK-READ 1 CADR:CC-DISK-LOWCORE 1)
;                    (CADR:CC-DISK-READ 3 (1+ CADR:CC-DISK-LOWCORE) 1)) ;Restore saved core
;                   (T (SETQ CADR:CC-DISK-TYPE NIL))))  ;Otherwise read label now that it
;                                                       ; maybe isnt garbage
;    (:UNIT-NUMBER 0)
;    (:MACHINE-NAME "via CC")
;    (:SAY (FORMAT T "CC-SAY ~A~%" (CAR ARGS)))
;    (:HANDLES-LABEL NIL)))

;(DEFUN CC-TEST-HANDLER (OP &REST ARGS)
;  (DECLARE (SPECIAL CC-DISK-UNIT))
;  (SELECTQ OP
;    (:READ (LET ((RQB (CAR ARGS)))
;            (LET ((BLOCK (CADR ARGS))
;                  (N-BLOCKS (rqb-npages rqb)))
;              (DO ((BLOCK BLOCK (1+ BLOCK))
;                   (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
;                   (BUF (RQB-BUFFER RQB))
;                   (BUF-IDX -1))
;                  ((ZEROP N-BLOCKS))
;                (SELECTQ CC-DISK-UNIT
;                  (0 (DOTIMES (W #o400)        ;Unit 0 makes block,,word
;                       (SETF (AREF BUF (INCF BUF-IDX)) BLOCK)
;                       (SETF (AREF BUF (INCF BUF-IDX)) W)))
;                  (1 (DOTIMES (W #o400)        ;Unit 1 makes block#7777,,word
;                       (SETF (AREF BUF (INCF BUF-IDX)) (LOGXOR #o7777 BLOCK))
;                       (SETF (AREF BUF (INCF BUF-IDX)) W))))))))
;    (:WRITE (LET ((RQB (CAR ARGS)))
;             (LET ((BLOCK (CADR ARGS))
;                   (N-BLOCKS (rqb-npages rqb)))
;               (DO ((BLOCK BLOCK (1+ BLOCK))
;                    (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
;                   (BUF (RQB-BUFFER RQB))
;                   ;; Following code is transmogrified from DBG-WRITE-XBUS
;                   (BUF-IDX -1)
;                   (W1) (W2) (ERRS 0) (MAX-ERRS 3))
;                   ((ZEROP N-BLOCKS))
;                (SELECTQ CC-DISK-UNIT
;                  (0 (DOTIMES (W #o400)        ;Unit 0 should be block,,word
;                       (SETQ W1 (AREF BUF (INCF BUF-IDX))
;                             W2 (AREF BUF (INCF BUF-IDX)))
;                       (COND ((OR (NOT (= BLOCK W1))
;                                  (NOT (= W2 W)))
;                              (FORMAT T "~%Block ~O WD ~O should be ~O,,~O is ~O,,~O"
;                                      BLOCK W BLOCK W W1 W2)
;                              (IF (> (INCF ERRS) MAX-ERRS)
;                                  (RETURN NIL))))))
;                  (1 (DOTIMES (W #o400)  ;Unit 1 should be block#7777,,word
;                       (SETQ W1 (AREF BUF (INCF BUF-IDX))
;                             W2 (AREF BUF (INCF BUF-IDX)))
;                       (COND ((OR (NOT (= (LOGXOR #o7777 BLOCK) W1))
;                                  (NOT (= W2 W)))
;                              (FORMAT T "~%Block ~O WD ~O should be ~O,,~O is ~O,,~O"
;                                      BLOCK W (LOGXOR BLOCK #o7777) W W1 W2)
;                              (IF (> (INCF ERRS) MAX-ERRS)
;                                  (RETURN NIL)))))))))))
;    (:DISPOSE NIL)
;    (:UNIT-NUMBER CC-DISK-UNIT)
;    (:MACHINE-NAME "TEST")
;    (:SAY (FORMAT T "CC-TEST-SAY ~A~%" (CAR ARGS)))
;    (:HANDLES-LABEL T)
;    (:FIND-DISK-PARTITION (VALUES  0 #o100000 NIL))
;    (:PARTITION-COMMENT (FORMAT NIL "TEST ~D" CC-DISK-UNIT))
;    (:UPDATE-PARTITION-COMMENT NIL)
;    ))


;;;; Internals

;;; Read the label from specified unit into an RQB, and set up the
;;; disk configuration table if it is a local unit.
;;; The label uses even-numbered pages, with the odd-numbered pages ignored.
;;; That is because booting uses them as scratch pages.
;;; CC-DISK-HANDLER also uses block 1 and 3 as scratch.

(DEFUN READ-DISK-LABEL (RQB UNIT &AUX VERSION)
  (WITH-DISK-RQB (RQB1)
    (DISK-READ RQB1 UNIT 0)
    (let ((string (rqb-8-bit-buffer rqb1)))
      (cond ((not (and (char-equal (aref string 0) #/L)
                       (char-equal (aref string 1) #/A)
                       (char-equal (aref string 2) #/B)
                       (char-equal (aref string 3) #/L)))
             (ferror "bad label check word")))
      (setq version (get-disk-fixnum rqb1 1))
      (cond ((and (not (= version 1))
                  (not (= version 2)))
             (ferror "bad label version ~d" version))))
    (ecase version
      (1
       (COPY-ARRAY-PORTION (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE)
                           (RQB-BUFFER RQB) 0 (* 2 PAGE-SIZE))
       (DISK-READ RQB1 UNIT 2)
       (COPY-ARRAY-PORTION (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE)
                           (RQB-BUFFER RQB) (* 2 PAGE-SIZE) (* 4 PAGE-SIZE))
       (DISK-READ RQB1 UNIT 4)
       (COPY-ARRAY-PORTION (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE)
                           (RQB-BUFFER RQB) (* 4 PAGE-SIZE) (* 6 PAGE-SIZE))
       (IF (NUMBERP UNIT)
           (LET ((BFR (RQB-BUFFER RQB)))
             (ASET (AREF BFR 6) DISK-HEADS-PER-CYLINDER-ARRAY UNIT)
             (ASET (AREF BFR 10) DISK-SECTORS-PER-TRACK-ARRAY UNIT))))
      (2
       (let ((start (get-disk-fixnum rqb1 21.))
             (length (get-disk-fixnum rqb1 22.)))
         (cond ((> length (1- disk-label-rqb-pages))
                (ferror "unreasonable label length ~d" length)))
         (copy-array-portion (rqb-buffer rqb1) 0 (* 2 page-size)
                             (rqb-buffer rqb) 0 (* 2 page-size))
         (dotimes (i length)
           (disk-read rqb1 unit (+ start i))
           (let ((to-adr (* (1+ i) 2 page-size)))
             (copy-array-portion (rqb-buffer rqb1) 0 (* 2 page-size)
                                 (rqb-buffer rqb) to-adr (+ to-adr (* 2 page-size)))))))
      )))


(DEFUN WRITE-DISK-LABEL (RQB UNIT)
  (OR (STRING-EQUAL (GET-DISK-STRING RQB 0 4) "LABL")
      (FERROR "Attempt to write garbage label"))
  (ecase (get-disk-fixnum rqb 1)
    (1 (write-disk-label-v1 rqb unit))
    (2 (write-disk-label-v2 rqb unit))))

(defun write-disk-label-v1 (rqb unit)
  (WITH-DISK-RQB (RQB1)
    (COPY-ARRAY-PORTION (RQB-BUFFER RQB) 0 (* 2 PAGE-SIZE)
                        (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE))
    (DISK-WRITE RQB1 UNIT 0)
    (COPY-ARRAY-PORTION (RQB-BUFFER RQB) (* 2 PAGE-SIZE) (* 4 PAGE-SIZE)
                        (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE))
    (DISK-WRITE RQB1 UNIT 2)
    (COPY-ARRAY-PORTION (RQB-BUFFER RQB) (* 4 PAGE-SIZE) (* 6 PAGE-SIZE)
                        (RQB-BUFFER RQB1) 0 (* 2 PAGE-SIZE))
    (DISK-WRITE RQB1 UNIT 4)))


(defun write-disk-label-v2 (rqb unit)
  (WITH-DISK-RQB (RQB1)
    (let ((start (get-disk-fixnum rqb 21.))
          (length (get-disk-fixnum rqb 22.)))
      (cond ((> length (1- disk-label-rqb-pages))
             (ferror "unreasonable label length ~d" length)))

      (copy-array-portion (rqb-buffer rqb) 0 (* 2 page-size)
                          (rqb-buffer rqb1) 0 (* 2 page-size))
      (disk-write rqb1 unit 0)
      (dotimes (i length)
        (let ((from-adr (* (1+ i) 2 page-size)))
          (copy-array-portion (rqb-buffer rqb) from-adr (+ from-adr (* 2 page-size))
                              (rqb-buffer rqb1) 0 (* 2 page-size))
          (disk-write rqb1 unit (+ start i)))))))


(DEFUN GET-DISK-STRING (RQB WORD-ADDRESS N-CHARACTERS &OPTIONAL (SHARE-P NIL))
  "Return a string containing the contents of a part of RQB's data.
The data consists of N-CHARACTERS characters starting at data word WORD-ADDRESS.
/(The first word of data is WORD-ADDRESS = 0).
SHARE-P non-NIL means return an indirect array that overlaps the RQB,
Otherwise return a string containing the data, except for trailing 0's
 (in which case the string returned may in fact be shorted than N-CHARACTERS.)"
  (IF SHARE-P
      (NSUBSTRING (RQB-8-BIT-BUFFER RQB) (* 4 WORD-ADDRESS)
                                         (+ (* 4 WORD-ADDRESS) N-CHARACTERS))
    (LET* ((STR (SUBSTRING (RQB-8-BIT-BUFFER RQB) (* 4 WORD-ADDRESS)
                                                  (+ (* 4 WORD-ADDRESS) N-CHARACTERS)))
           (IDX (STRING-REVERSE-SEARCH-NOT-CHAR 0 STR)))
      (ADJUST-ARRAY-SIZE STR (IF IDX (1+ IDX) 0))
      STR)))

(DEFUN PUT-DISK-STRING (RQB STR WORD-ADDRESS N-CHARACTERS)
  "Store the contents of string STR into RQB's data at WORD-ADDRESS.
N-CHARACTERS characters are stored, padding STR with zeros if it is not that long."
  (LET ((START (* 4 WORD-ADDRESS))
        (END (+ (* 4 WORD-ADDRESS) N-CHARACTERS)))
    (ARRAY-INITIALIZE (RQB-8-BIT-BUFFER RQB) 0 START END)
    (COPY-ARRAY-PORTION
      STR 0 (STRING-LENGTH STR)
      (RQB-8-BIT-BUFFER RQB) START (MIN END (+ START (STRING-LENGTH STR))))))

(DEFUN GET-DISK-FIXNUM (RQB WORD-ADDRESS)
  "Return the contents of data word WORD-ADDRESS in RQB, as a number."
  (DPB (AREF (RQB-BUFFER RQB) (1+ (* 2 WORD-ADDRESS)))
       (BYTE 16. 16.)
       (AREF (RQB-BUFFER RQB) (* 2 WORD-ADDRESS))))

(DEFUN PUT-DISK-FIXNUM (RQB VAL WORD-ADDRESS)
  "Store VAL into data word WORD-ADDRESS of RQB."
  (SETF (AREF (RQB-BUFFER RQB) (* 2 WORD-ADDRESS)) (LDB (BYTE 16. 0.) VAL))
  (SETF (AREF (RQB-BUFFER RQB) (1+ (* 2 WORD-ADDRESS))) (LDB (BYTE 16. 16.) VAL)))

(DEFUN FIND-DISK-PARTITION (NAME &OPTIONAL RQB UNIT (ALREADY-READ-P NIL) CONFIRM-WRITE)
  "Search the label of disk unit UNIT for a partition named NAME.
Returns three values describing what was found, or NIL if none found.
The values are the first block number of the partition,
the length in disk blocks of the partition,
and the location in the label (in words) of the data for this partition."
  (DECLARE (VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME))
  (setq unit (default-disk-unit unit))
  (IF (AND (CLOSUREP UNIT)
           (FUNCALL UNIT :HANDLES-LABEL))
      (FUNCALL UNIT :FIND-DISK-PARTITION NAME)
    (WITH-DISK-RQB (RQB (OR RQB DISK-LABEL-RQB-PAGES))
      (OR ALREADY-READ-P (READ-DISK-LABEL RQB UNIT))
      (DO ((N-PARTITIONS (GET-DISK-FIXNUM RQB
                                          (ecase (get-disk-fixnum rqb 1)
                                            (1 #o200)
                                            (2 (+ 256. 2)))))
           (WORDS-PER-PART (GET-DISK-FIXNUM RQB
                                            (ecase (get-disk-fixnum rqb 1)
                                              (1 #o201)
                                              (2 (+ 256. 3)))))
           (I 0 (1+ I))
           (LOC (ecase (get-disk-fixnum rqb 1)
                  (1 #o202)
                  (2 (+ 256. 16.)))
                (+ LOC WORDS-PER-PART))
           (words-before-comment
             (ecase (get-disk-fixnum rqb 1)
               (1 3)
               (2 4)))
           )
          ((= I N-PARTITIONS) NIL)
        (WHEN (STRING-EQUAL (GET-DISK-STRING RQB LOC 4) NAME)
          (AND CONFIRM-WRITE
               (NOT (FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS
                            "Do you really want to clobber partition ~A ~
                                 ~:[~*~;on unit ~D ~](~A)? "
                            NAME (NUMBERP UNIT) UNIT
                            (GET-DISK-STRING RQB
                                             (+ LOC words-before-comment)
                                             (* 4 (- words-per-part words-before-comment)))))
               (RETURN-FROM FIND-DISK-PARTITION (VALUES NIL T)))
          (RETURN-FROM FIND-DISK-PARTITION (VALUES (GET-DISK-FIXNUM RQB (+ LOC 1))
                                                   (GET-DISK-FIXNUM RQB (+ LOC 2))
                                                   LOC
                                                   NAME)))))))

(defun disk-partition-comment-string (rqb loc)
  "Get the comment string for a disk partition.
RQB is the one that was passed to FIND-DISK-PARTITION.
LOC is the label location of the partition entry in the
disk label as returned by FIND-DISK-PARTITION."
  (let* ((words-before-comment (ecase (get-disk-fixnum rqb 1)
                                 (1 3)
                                 (2 4)))
         (words-per-part (get-disk-fixnum rqb (ecase (get-disk-fixnum rqb 1)
                                                (1 #o201)
                                                (2 (+ 256. 3))))))
    (get-disk-string rqb
                     (+ loc words-before-comment)
                     (* 4 (- words-per-part words-before-comment))
                     nil)))

(DEFUN FIND-DISK-PARTITION-FOR-READ (NAME &OPTIONAL RQB UNIT (ALREADY-READ-P NIL)
                                                        (NUMBER-PREFIX "LOD"))
  "Like FIND-DISK-PARTITION except there is error checking and coercion.
If NAME is a number, its printed representation is appended to NUMBER-PREFIX
to get the partition name to use."
  (DECLARE (VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME))
  (setq unit (default-disk-unit unit))
  (TYPECASE NAME
    (NUMBER
     (SETQ NAME (FORMAT NIL "~A~D" NUMBER-PREFIX NAME)))
    (SYMBOL
     (SETQ NAME (SYMBOL-NAME NAME)))
    (STRING)
    (T
     (FERROR "~S is not a valid partition name" NAME)))
  (MULTIPLE-VALUE-BIND (FIRST-BLOCK N-BLOCKS LABEL-LOC)
      (FIND-DISK-PARTITION NAME RQB UNIT ALREADY-READ-P)
    (IF (NOT (NULL FIRST-BLOCK))
        (VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME)
      (FERROR "No partition named /"~A/" exists on disk unit ~D." NAME UNIT))))

(DEFUN FIND-DISK-PARTITION-FOR-WRITE (NAME &OPTIONAL RQB UNIT (ALREADY-READ-P NIL)
                                                    (NUMBER-PREFIX "LOD"))
  "Like FIND-DISK-PARTITION except there is error checking, coercion, and confirmation.
If NAME is a number, its printed representation is appended to NUMBER-PREFIX
to get the partition name to use.
Returns NIL if the partition specified is valid but the user refuses to confirm."
  (DECLARE (VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME))
  (setq unit (default-disk-unit unit))
  (COND ((and (closurep unit) (eq (closure-function unit) 'fs:band-magtape-handler))
         (return-from find-disk-partition-for-write (values 0 9999999 nil name)))
        ((NUMBERP NAME) (SETQ NAME (FORMAT NIL "~A~D" NUMBER-PREFIX NAME)))
        ((SYMBOLP NAME) (SETQ NAME (GET-PNAME NAME)))
        ((NOT (STRINGP NAME)) (FERROR "~S is not a valid partition name" NAME)))
  (LET* ((CURRENT-BAND (CURRENT-BAND unit))
         (CURRENT-BAND-BASE-BAND (INC-BAND-BASE-BAND CURRENT-BAND unit))
         (CURRENT-RUNNING-BAND (STRING-APPEND "LOD"
                                              (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND))))
    (COND ((NOT (EQ UNIT 0)))
          ((STRING-EQUAL NAME CURRENT-BAND)
           (FORMAT T "~&It is dangerous to write into the current band.
If there is a disk error saving,
the machine's current band will be invalid
and cold-booting will not work."))
          ((AND CURRENT-BAND-BASE-BAND
                (STRING-EQUAL NAME CURRENT-BAND-BASE-BAND))
           (FORMAT T "~&It is dangerous to write into the current band's base band.
The current band ~A is an incremental band
and requires ~A, its base band, in order to cold-boot.
Overwriting that band makes the current band invalid
and the machine will be unbootable until a valid band is selected."
                   CURRENT-BAND CURRENT-BAND-BASE-BAND))
          ((STRING-EQUAL NAME CURRENT-RUNNING-BAND)
           (FORMAT T "~&It may be unwise to overwrite the currently running band.
Do it only if you are sure the current band for booting (~A)
will work properly."
                   CURRENT-BAND))))
  (MULTIPLE-VALUE-BIND (FIRST-BLOCK N-BLOCKS LABEL-LOC)
      (FIND-DISK-PARTITION NAME RQB UNIT ALREADY-READ-P T)
    (IF (NOT (NULL FIRST-BLOCK))
        (VALUES FIRST-BLOCK N-BLOCKS LABEL-LOC NAME)
      (IF (NULL N-BLOCKS)
          (FERROR "No partition named /"~A/" exists on disk unit ~D." NAME UNIT)
        NIL))))

(DEFUN PARTITION-LIST (&OPTIONAL RQB UNIT ALREADY-READ-P)
  "Returns the data of the disk label on unit UNIT.
The value is a list with one element per partition,
with the format (<name> <base> <size> <comment> <desc-loc>).
RQB is an rqb to use, or NIL meaning allocate one temporarily."
  (setq unit (default-disk-unit unit))
  (WITH-DISK-RQB (RQB (OR RQB DISK-LABEL-RQB-PAGES))
    (UNLESS ALREADY-READ-P
      (READ-DISK-LABEL RQB UNIT))
    (LET ((RESULT (MAKE-LIST (GET-DISK-FIXNUM RQB (ecase (get-disk-fixnum rqb 1)
                                                    (1 #o200)
                                                    (2 (+ 256. 2))))))
          (WORDS-PER-PART (GET-DISK-FIXNUM RQB (ecase (get-disk-fixnum rqb 1)
                                                 (1 #o201)
                                                 (2 (+ 256. 3)))))
          (words-before-comment (ecase (get-disk-fixnum rqb 1)
                                  (1 3)
                                  (2 4)))
          )
      (DO ((LOC (ecase (get-disk-fixnum rqb 1)
                  (1 #o202)
                  (2 (+ 256. 16.)))
                (+ LOC WORDS-PER-PART))
           (R RESULT (CDR R)))
          ((NULL R) RESULT)
        (SETF (CAR R)
              (LIST (GET-DISK-STRING RQB LOC 4)
                    (GET-DISK-FIXNUM RQB (+ LOC 1))
                    (GET-DISK-FIXNUM RQB (+ LOC 2))
                    (GET-DISK-STRING RQB (+ LOC words-before-comment)
                                     (* 4 (- words-per-part words-before-comment)))
                    LOC))))))

;;; This is a hack to allow one to easily find if a partition he wants is available.
(DEFUN PRINT-AVAILABLE-BANDS (&OPTIONAL (WHICH "LOD")
                                        MACHINES
                              &AUX (WL (AND (STRINGP WHICH)
                                            (MIN (ARRAY-ACTIVE-LENGTH WHICH) 4)))
                              PARTITION-LIST PARTITION-LIST-ALIST TEM)
  "Print a summary of the contents of partitions existing on MACHINES.
MACHINES defaults to all free Lisp machines.
Only partitions whose names start with WHICH are mentioned.
WHICH defaults to /"LOD/"."
  (CHECK-TYPE WHICH (OR STRING (MEMBER T)) "a string or T")
  (unless machines
    (multiple-value-bind (free in-use)
        (CHAOS:FINGER-ALL-LMS NIL NIL NIL T   NIL NIL)
      (setq machines (append free in-use))))
  (with-disk-rqb (rqb DISK-LABEL-RQB-PAGES)
    (DOLIST (M MACHINES)
      (catch-error
        (with-decoded-disk-unit (unit m "Examining Label")
          (SETQ PARTITION-LIST (PARTITION-LIST RQB UNIT)))
        (DOLIST (PARTITION PARTITION-LIST)
          (AND (OR (EQ WHICH T)
                   (STRING-EQUAL (CAR PARTITION) WHICH :start1 0 :start2 0 :end1 WL :end2 WL))
               (PLUSP (STRING-LENGTH (FOURTH PARTITION)))
               (IF (SETQ TEM (SI:ASSOC-EQUALP (FOURTH PARTITION) PARTITION-LIST-ALIST))
                   (RPLACD (LAST TEM) (NCONS (LIST M (FIRST PARTITION))))
                 (PUSH (LIST* (FOURTH PARTITION)
                              (LIST M (FIRST PARTITION))
                              NIL)
                       PARTITION-LIST-ALIST)))))))
  (SETQ PARTITION-LIST-ALIST (SORTCAR PARTITION-LIST-ALIST #'STRING-LESSP))
  (DOLIST (P PARTITION-LIST-ALIST)
    (FORMAT T "~%~A:~20T~:{~<~%~20T~2:;~A ~A~>~:^, ~}" (CAR P) (CDR P)))
  PARTITION-LIST-ALIST)


(DEFUN SYS-COM-PAGE-NUMBER (16B-BUFFER INDEX)
  (LSH
    (LOGAND
      (SELECT-PROCESSOR
        (:CADR (1- 1_24.))
        ((:LAMBDA :explorer :falcon) -1)
        )
      (LOGIOR (LSH (AREF 16B-BUFFER (1+ (* 2 INDEX))) #o20)
              (AREF 16B-BUFFER (* 2 INDEX))))
    (- %%Q-POINTER-WITHIN-PAGE)))

(DEFUN DESCRIBE-PARTITION (PART &OPTIONAL UNIT
                           &AUX PART-BASE PART-SIZE
                           COMPRESSED-FORMAT-P INCREMENTAL-BAND-P
                           VALID-SIZE HIGHEST-VIRTUAL-ADDRESS
                           DESIRED-UCODE-VERSION)
  "Print information about partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "describing ~A partition" PART))
    (MULTIPLE-VALUE (PART-BASE PART-SIZE)
      (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT))
    (WITH-DISK-RQB (RQB)
      (SETQ VALID-SIZE
            (COND ((OR (NUMBERP PART) (STRING-EQUAL PART "LOD" :END1 3))
                   (DISK-READ RQB UNIT (1+ PART-BASE))
                   (LET ((BUF (RQB-BUFFER RQB)))
                     (SETQ COMPRESSED-FORMAT-P
                           (= #o1000 (AREF BUF (* 2 %SYS-COM-BAND-FORMAT))))
                     (SETQ INCREMENTAL-BAND-P
                           (= #o1001 (AREF BUF (* 2 %SYS-COM-BAND-FORMAT))))
                     (SETQ VALID-SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE))
                     (SETQ VALID-SIZE (IF (AND (> VALID-SIZE #o10)
                                               ( VALID-SIZE PART-SIZE))
                                          VALID-SIZE
                                        PART-SIZE))
                     (SETQ HIGHEST-VIRTUAL-ADDRESS
                           (SYS-COM-PAGE-NUMBER BUF %SYS-COM-HIGHEST-VIRTUAL-ADDRESS))
                     (SETQ DESIRED-UCODE-VERSION
                           (AREF BUF (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION)))
                     VALID-SIZE))
                  (T PART-SIZE)))
      (FORMAT T "~%Partition ~A starts at ~D and is ~D pages long."
              (STRING-UPCASE PART) PART-BASE PART-SIZE)
      (IF (OR COMPRESSED-FORMAT-P INCREMENTAL-BAND-P)
          (PROGN
            (IF COMPRESSED-FORMAT-P
                (FORMAT T "~%It is a compressed world-load.")
              (FORMAT T "~%It is an incremental band with base band ~A."
                      (INC-BAND-BASE-BAND PART UNIT)))
            (FORMAT T "~%Data length is ~D pages, highest virtual page number is ~D."
                    VALID-SIZE HIGHEST-VIRTUAL-ADDRESS))
        (FORMAT T "~%It is in non-compressed format, data length ~D pages." VALID-SIZE))
      (IF DESIRED-UCODE-VERSION
          (FORMAT T "~%Goes with microcode version ~D." DESIRED-UCODE-VERSION)))))


(DEFUN DESCRIBE-PARTITIONS (&OPTIONAL UNIT)
  "Describes all of the partitions of UNIT, or the standard disk if unit is not supplied."
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT "describing partitions")
    (LOOP FOR (BAND . REST) IN (PARTITION-LIST NIL UNIT)
          DOING
          (FORMAT T "~%")
          (DESCRIBE-PARTITION BAND UNIT))))


(DEFUN FIND-PLAUSIBLE-PARTITIONS (UNIT START END)
  "Search disk unit UNIT from block START to just before block END for valid-looking Lisp worlds.
Use this if a disk label is clobbered.
Each time a block is found that looks like it could be
where the beginning of a partition ought to be,
an entry is printed.  Ignore those whose printed Ucode versions are unreasonable.
If there are two valid-looking entries close together on the disk,
the one with the higher disk address is more likely to be followed
by an actual good Lisp world."
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "searching for partitions"))
    (WITH-DISK-RQB (RQB 20)
      (DO ((RQB-BASE START (+ RQB-BASE 20)))
          ((>= RQB-BASE END))
        (DISK-READ RQB UNIT RQB-BASE)
        (DO ((IDX 0 (1+ IDX)))
            ((= IDX 20))
          (WHEN (= #O1000
                   (AREF (RQB-BUFFER RQB)
                         (+ (* PAGE-SIZE 2 IDX) (* 2 %SYS-COM-BAND-FORMAT))))
            (FORMAT T "~%Possible at block ~d:~%" (+ RQB-BASE IDX -1))
            (FORMAT T "Ucode version ~d~%"
                    (AREF (RQB-BUFFER RQB)
                          (+ (* PAGE-SIZE 2 IDX)
                             (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION))))))))))

(DEFUN GET-UCODE-VERSION-OF-BAND (PART &OPTIONAL UNIT)
  "Return the microcode version number that partition PART on unit UNIT should be run with.
This is only meaningful when used on a LOD partition.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "Finding microcode for ~A partition" PART))
    (WITH-DISK-RQB (RQB)
      (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE)
          (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT)
        (COND ((ZEROP PART-SIZE)
               (FERROR NIL "PARTITION SIZE IS ZERO"))
              ((OR (NUMBERP PART) (STRING-EQUAL PART "LOD" :END1 3))
               (DISK-READ RQB UNIT (1+ PART-BASE))
               (LET ((BUF (RQB-BUFFER RQB)))
                 (AREF BUF (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION)))))))))


(DEFUN MEASURED-SIZE-OF-PARTITION (PART &OPTIONAL UNIT)
  "Return the number of blocks of partition PART on unit UNIT actually containing data.
Except for LOD partitions, this is the total size.
The second value, for LOD partitions, is the required PAGE partition size.
The third value, for LOD partitions, is the desired microcode version.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (DECLARE (VALUES PARTITION-DATA-SIZE VIRTUAL-MEMORY-SIZE MICROCODE-VERSION))
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "sizing ~A partition" PART))
    (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART-NAME)
        (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT)
      (LET ((L (MULTIPLE-VALUE-LIST
                 (MEASURED-FROM-PART-SIZE UNIT PART-NAME PART-BASE PART-SIZE))))
        (IF (CAR L)
            (VALUES-LIST L)
          PART-SIZE)))))



(DEFUN DISK-INIT (&AUX SIZE)
  (select-processor
    ((:cadr :lambda)
     (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
       (READ-DISK-LABEL RQB 0)
       ;;until we know better, assume all disks on this machine are the same
       (dotimes (i (array-length disk-heads-per-cylinder-array))
         (aset (aref disk-heads-per-cylinder-array 0)
               disk-heads-per-cylinder-array
               i)
         (aset (aref disk-sectors-per-track-array 0)
               disk-sectors-per-track-array
               i))
       ;; Update things which depend on the location and size of the paging area
       (MULTIPLE-VALUE (PAGE-OFFSET SIZE)
         (FIND-DISK-PARTITION
           (OR (AND (FBOUNDP 'FIND-PAGING-PARTITION-NAME)
                    (FIND-PAGING-PARTITION-NAME))
               "PAGE") RQB 0 T))
       (SETQ VIRTUAL-MEMORY-SIZE
             (* (MIN
                  (select-processor
                    ;;full address space avail on LAMBDA
                    (:lambda (LDB (BYTE 17. 8.) A-MEMORY-VIRTUAL-ADDRESS))
                    ;;only 24 bits supported by CADR hardware.
                    (:cadr (ldb (BYTE 16. 8.) a-memory-virtual-address))
                    (:explorer nil))
                  SIZE)
                PAGE-SIZE))
       (SETQ DISK-PACK-NAME (GET-DISK-STRING RQB #o20 32.))))
    (:explorer
      (let (page-part-size)
        (multiple-value (page-unit page-offset page-part-size)
          (page-partition-info-for-local-machine))
        (setq virtual-memory-size
              (* (min (ldb (byte 17. 8) a-memory-virtual-address) page-part-size)
                 page-size))
        (setq disk-pack-name (get-pack-name page-unit))))
    ))

(DEFUN PRINT-LOADED-BAND (&OPTIONAL (STREAM T)) ;Can be NIL to return a string
  "Prints on STREAM a description of the loaded band.
This is obsolete -- You probably want PRINT-HERALD"
  (UNLESS (ZEROP %LOADED-BAND)
    (SETQ CURRENT-LOADED-BAND %LOADED-BAND))
  (UNLESS (BOUNDP 'CURRENT-LOADED-BAND)
    (SETQ CURRENT-LOADED-BAND 0))
  (PROG2        ;If STREAM is NIL, want to return a string with no carriage returns in it
    (FRESH-LINE STREAM)
    (FORMAT STREAM "This is band ~C of ~A, with ~A"
            (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND)     ;4th char in string (only high 3 stored)
            DISK-PACK-NAME
            (IF (FBOUNDP 'SYSTEM-VERSION-INFO)  ;For the cold load
                (SYSTEM-VERSION-INFO)
              "[fresh cold load]"))
    (WRITE-CHAR #/NEWLINE STREAM)))

;;;

;;;Set to release 5.0 for system 128.  We are, in effect, forked from
;;;customer release 4 (any patches we send them in future will be to
;;;to system 125). <11nov88 keith>

(DEFVAR *RELEASE-MAJOR-VERSION* 5)
(DEFVAR *RELEASE-MINOR-VERSION* 0)

(DEFVAR *RELEASE-STATUS* :DEVELOPMENT)

(defconstant release-status-keywords  '(:development :released :alpha :beta :obsolete))

(defun release-status ()
  (CASE *RELEASE-STATUS*
    (nil)
    (:DEVELOPMENT "(Development System)")
    (:ALPHA "(Alpha Test)")
    (:BETA "(Beta Test)")
    (:obsolete "(Obsolete System)")
    (:released "")
    (otherwise (if (member *release-status* release-status-keywords)
                   (format nil "(~A System)"*release-status*)
                 (format nil "(~A?)" *release-status*)))))

;;; This is called explicitly by LISP-REINITIALIZE.
(DEFUN PRINT-HERALD (&OPTIONAL (STREAM STANDARD-OUTPUT))
  "Print on STREAM a description of the versions of all software running."
  (UNLESS (ZEROP %LOADED-BAND)
    (SETQ CURRENT-LOADED-BAND %LOADED-BAND))
  (UNLESS (BOUNDP 'CURRENT-LOADED-BAND)
    (SETQ CURRENT-LOADED-BAND 0))
  (SELECT-PROCESSOR
    (:CADR
      ;; Give MIT credit for all the CADR systems.
      (FORMAT STREAM "~&MIT System, band ~C of ~A."
              (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND)
              DISK-PACK-NAME))
    ((:LAMBDA :falcon)
      (FORMAT STREAM "~&GigaMos ~A Release ~D.~D~@[ ~A~]~@[, band ~C of ~A~]."
              (select-processor (:lambda "Lambda") (:falcon "Falcon"))
              *RELEASE-MAJOR-VERSION*
              *RELEASE-MINOR-VERSION*
              (release-status)
              (select-processor
                (:lambda (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND)))
              DISK-PACK-NAME))
    (:EXPLORER
      (FORMAT STREAM "~&LMI Explorer Release ~D.~D~@[ ~A~], band ~C of ~A."
              *RELEASE-MAJOR-VERSION*
              *RELEASE-MINOR-VERSION*
              (release-status)
              (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND)
              DISK-PACK-NAME)))
  (AND (BOUNDP 'SYSTEM-ADDITIONAL-INFO)
       (PLUSP (ARRAY-ACTIVE-LENGTH SYSTEM-ADDITIONAL-INFO))
       (FORMAT STREAM " (~A)" SYSTEM-ADDITIONAL-INFO))
  (IF (NOT (FBOUNDP 'DESCRIBE-SYSTEM-VERSIONS))
      (FORMAT STREAM "~%Fresh Cold Load~%")
    (SELECT-PROCESSOR
      (:CADR
        (FORMAT STREAM "~&~DK physical memory, ~DK virtual memory."
                (TRUNCATE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE) #O2000)
                (TRUNCATE VIRTUAL-MEMORY-SIZE #O2000)))
      ((:LAMBDA :EXPLORER)
        (FORMAT STREAM "~&~DK physical memory, ~DK virtual memory, NuBus slot ~D."
                (TRUNCATE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE) #O2000)
                (TRUNCATE VIRTUAL-MEMORY-SIZE #O2000)
                (LOGXOR #XF0 RG-QUAD-SLOT))))
    (DESCRIBE-SYSTEM-VERSIONS STREAM t)
    (FORMAT STREAM "~%~A ~A, with associated machine ~A.~%"
            (OR (GET-SITE-OPTION :SITE-PRETTY-NAME) SITE-NAME)
            LOCAL-PRETTY-HOST-NAME
            (SEND ASSOCIATED-MACHINE :NAME-AS-FILE-COMPUTER))
    (select-processor
      (:explorer (Check-For-Abnormal-Shutdown))
      ((:lambda :cadr)))))

;;; Must be defined before initialization below
(DEFUN WIRE-PAGE (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete wire-page "use %wire-page with different conventions")

(DEFUN UNWIRE-PAGE (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete unwire-page "use %unwire-page with different conventions")

(DEFUN WIRE-WORDS (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete wire-words "use %wire-words with different conventions")

(DEFUN UNWIRE-WORDS (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete unwire-words "use %unwire-words with different conventions")

(DEFUN WIRE-ARRAY  (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete wire-array "use %wire-structure with different conventions")

(DEFUN UNWIRE-ARRAY (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete unwire-array "use %unwire-structure with different conventions")

(DEFUN WIRE-STRUCTURE (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete wire-structure "use %wire-structure with different conventions")

(DEFUN UNWIRE-STRUCTURE (&rest ignore)
  (ferror "obsolete"))
(compiler:make-obsolete unwire-structure "use %unwire-structure with different conventions")

(defun wire-area (area wire-p)
  (%wire-area area wire-p))
(compiler:make-obsolete wire-area "use %wire-area with different conventions")

;;; Also doesn't work on downwards-consed list regions (which no longer exist).
(defun %wire-area (area wire-p)
  "Takes the number of an area and wires down all the allocated
pages of it, or un-wires, depending on the second argument.
The area had better have only one region!"
  (let ((region (%area-region-list area)))
    (or (minusp (%region-list-thread region)) ;last region in area
        (ferror "area ~a has more than one region" (area-name area)))
    (do ((loc (%region-origin region) (%pointer-plus loc page-size))
         (count (ceiling (%region-free-pointer region) page-size) (1- count)))
        ((zerop count))
      (%wire-page loc wire-p))))

;This must be after main definitions above, but before initialization below!
(ADD-INITIALIZATION "DISK-INIT" '(DISK-INIT) '(SYSTEM))

;;; Put a microcode file onto my own disk.
;;; Note that the cretinous halfwords are out of order
(DEFUN LOAD-MCR-FILE (FILENAME PART &OPTIONAL UNIT)
  "Load microcode from file FILENAME into partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet,
or /"CC/" which refers to the machine being debugged by this one."
  (setq unit (default-disk-unit unit))
  (LET ((DEFAULT (PATHNAME (SELECT-PROCESSOR
                             (:CADR "SYS: UBIN; UCADR MCR >")
                             (:LAMBDA "SYS: UBIN; ULAMBDA MCR >")
                             (:EXPLORER "SYS: UBIN; ULAMBDA MCR >")
                             ))))
    (SETQ FILENAME (COND ((EQ FILENAME 'T)
                          DEFAULT)
                         ((NUMBERP FILENAME)
                          (SEND DEFAULT :NEW-VERSION FILENAME))
                         (T
                          (MERGE-PATHNAMES FILENAME DEFAULT)))))
  (UNLESS (STRING-EQUAL (SEND FILENAME :CANONICAL-TYPE) :MCR)
    (FERROR "~A is not a MCR file." FILENAME))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "Loading ~A into ~A partition" FILENAME PART)
                                NIL T)
    (WITH-DISK-RQB (RQB)
      (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
          (FIND-DISK-PARTITION-FOR-WRITE PART NIL UNIT NIL "MCR")
        (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
          (BLOCK DONE
            (DO ((BUF16 (rqb-buffer rqb))
                 (BLOCK PART-BASE (1+ BLOCK))
                 (N PART-SIZE (1- N)))
                ((ZEROP N) (FERROR "Failed to fit in partition"))
              (DO ((LH) (RH)
                   (I 0 (+ I 2)))
                  ((= I #o1000)
                   (DISK-WRITE RQB UNIT BLOCK))
                (SETQ LH (SEND FILE :TYI)
                      RH (SEND FILE :TYI))
                (WHEN (OR (NULL LH) (NULL RH))
                  (UPDATE-PARTITION-COMMENT
                    PART
                    (LET ((PATHNAME (SEND FILE :TRUENAME)))
                      (FORMAT NIL "~A ~D" (SEND PATHNAME :NAME) (SEND PATHNAME :VERSION)))
                    UNIT)
                  (RETURN-FROM DONE NIL))
                (SETF (AREF BUF16 I) RH)
                (SETF (AREF BUF16 (1+ I)) LH)))))))))


(DEFUN LOAD-EMC-FILE (FILENAME PART &OPTIONAL UNIT)
  "Load microcode from file FILENAME into partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet
or /"CC/" which refers to the machine being debugged by this one."
  (setq unit (default-disk-unit unit))
  (SETQ FILENAME (COND ((NUMBERP FILENAME)
                        (SEND (PATHNAME "SYS:UBIN;ULAMBDA EMC >") :NEW-VERSION
                              FILENAME))
                       ((EQ FILENAME T)
                        (PATHNAME "SYS:UBIN;ULAMBDA EMC >"))
                       (T
                        (MERGE-PATHNAMES FILENAME "SYS: UBIN; ULAMBDA EMC >"))))
  ;;  Do string-equal, not equal, on the canonical-type, not the type
  (if (not (or (STRING-EQUAL (SEND FILENAME :CANONICAL-TYPE) :MCR)
               (STRING-EQUAL (SEND FILENAME :CANONICAL-TYPE) :EMC)))
      (FERROR "~A is not a MCR file." FILENAME))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT
                                (FORMAT NIL "Loading ~A into ~A partiton"
                                        FILENAME PART)
                                NIL
                                T)
    (WITH-DISK-RQB (RQB)
      (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
          (FIND-DISK-PARTITION-FOR-WRITE PART NIL UNIT NIL "MCR")
        (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
          (BLOCK DONE
            (DO ((BUF16 (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
                 (BLOCK PART-BASE (1+ BLOCK))
                 (N PART-SIZE (1- N)))
                ((ZEROP N) (FERROR "Failed to fit in partition"))
              (DO ((LH) (RH)
                   (I 0 (+ I 2)))
                  ((= I #o1000)
                   (DISK-WRITE RQB UNIT BLOCK))
                (SETQ LH (SEND FILE :TYI)
                      RH (SEND FILE :TYI))
                (COND ((OR (NULL LH) (NULL RH))
                       (UPDATE-PARTITION-COMMENT
                         PART
                         (format nil "~A ~D"
                                 (send (truename file) :name)
                                 (send (truename file) :version))
                         UNIT)
                       (RETURN-FROM DONE NIL)))
                (ASET RH BUF16 I)
                (ASET LH BUF16 (1+ I))))))))))


;;; Put a microcode file onto my own disk, LAMBDA style.
;;; Note that the halfwords are IN order in a LMC file (as opposed to a MCR file).
(DEFUN LOAD-LMC-FILE (FILENAME PART &OPTIONAL UNIT)
  "Load microcode from file FILENAME into partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet,
or /"CC/" or /"LAM/" which refers to the machine being debugged by this one.
If FILENAME is T, use SYS:UBIN;ULAMBDA.LMC#>.  If FILENAME is a number,
use SYS:UBIN;ULAMBDA.LMC#FILENAME."
  (setq unit (default-disk-unit unit))
  (SETQ FILENAME (COND ((EQ FILENAME T)
                        (PATHNAME "SYS: UBIN; ULAMBDA LMC >"))
                       ((NUMBERP FILENAME)
                        (SEND (PATHNAME "SYS: UBIN; ULAMBDA LMC >")
                              :NEW-VERSION FILENAME))
                       (T
                        (MERGE-PATHNAMES FILENAME (PATHNAME "SYS: UBIN; ULAMBDA LMC >")))))
  (UNLESS (STRING-EQUAL (SEND FILENAME :TYPE) :LMC)
    (FERROR "~A is not a LMC file." FILENAME))
  (BLOCK USER-ABORT
    (WITH-DECODED-DISK-UNIT (UNIT UNIT
                                  (FORMAT NIL "Loading ~A into ~A partition" FILENAME PART)
                                  NIL
                                  T)
      (WITH-DISK-RQB (RQB)
        (WITH-DISK-RQB (RQB-FOR-LABEL 3)
          (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
              (FIND-DISK-PARTITION-FOR-WRITE PART RQB-FOR-LABEL UNIT NIL "LMC")
            (UNLESS PART-BASE                   ; User declined
              (RETURN-FROM USER-ABORT NIL))
            (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
              (BLOCK DONE
                (DO ((BUF16 (rqb-buffer rqb))
                     (BLOCK PART-BASE (1+ BLOCK))
                     (N PART-SIZE (1- N)))
                    ((ZEROP N) (FERROR "Failed to fit in partition"))
                  (DO ((LH) (RH)
                       (I 0 (+ I 2)))
                      ((= I #o1000)
                       (DISK-WRITE RQB UNIT BLOCK))
                    (SETQ RH (SEND FILE :TYI)   ;note halfwords in "right" order in LMC file
                          LH (SEND FILE :TYI))
                    (WHEN (OR (NULL LH) (NULL RH))
                      (UPDATE-PARTITION-COMMENT
                        PART
                        (LET ((PATHNAME (SEND FILE ':TRUENAME)))
                          (FORMAT NIL "~A ~D" (SEND PATHNAME ':NAME) (SEND PATHNAME :VERSION)))
                        UNIT)
                      (RETURN-FROM DONE NIL))
                    (SETF (AREF BUF16 I) RH)
                    (SETF (AREF BUF16 (1+ I)) LH)))))))))))


(DEFUN LOAD-LMC-FILE-INTO-MICRO-CODE-PAGING-AREA (&OPTIONAL FILENAME)
  (LET ((DEFAULT (PATHNAME "SYS: UBIN; ULAMBDA LMC >")))
    (SETQ FILENAME (COND ((EQ FILENAME 'T)
                          DEFAULT)
                         ((EQ FILENAME 'NIL)
                          (SEND DEFAULT :NEW-VERSION %MICROCODE-VERSION-NUMBER))
                         ((NUMBERP FILENAME)
                          (SEND DEFAULT :NEW-VERSION FILENAME))
                         (T
                          (MERGE-PATHNAMES FILENAME DEFAULT)))))
  (UNLESS (STRING-EQUAL (SEND FILENAME :TYPE) :LMC)
    (FERROR "~A is not a LMC file." FILENAME))
  (WITH-OPEN-FILE (stream FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
    (PROG (HCODE LCODE HADR LADR HCOUNT LCOUNT HD LD
           UDSP-NBLKS UDSP-RELBLK)
       L0
          (SETQ LCODE (SEND STREAM :TYI) HCODE (SEND STREAM :TYI))
          (COND ((OR (NOT (ZEROP HCODE)) (< LCODE 0) (> LCODE 5))
                 (FERROR "BAD CODE HCODE=~O LCODE=~O" HCODE LCODE)))
          (SETQ LADR (SEND STREAM :TYI) HADR (SEND STREAM :TYI))
          (SETQ LCOUNT (SEND STREAM :TYI) HCOUNT (SEND STREAM :TYI))
          (COND ((OR (NOT (ZEROP HADR))
                     (NOT (ZEROP HCOUNT)))
                 (FERROR "BAD HEADER SA ~O,~O COUNT ~O,~O"
                         HADR LADR HCOUNT LCOUNT)))
          (COND ((ZEROP LCODE)
                 (CLOSE STREAM)
                 (RETURN T))
                ((= LCODE 1) (GO LI))           ;I-MEM
                ((= LCODE 3)                    ;IGNORE MAIN MEMORY LOAD
                 (SETQ UDSP-NBLKS LADR)
                 (SETQ UDSP-RELBLK LCOUNT)
                 (SETQ HD (SEND STREAM :TYI) LD (SEND STREAM :TYI))     ;PHYS MEM ADR
                 (GO L0))
                ((= LCODE 4) (GO LA)))          ;A-MEM
       LA
          (COND ((< (SETQ LCOUNT (1- LCOUNT)) 0)
                 (GO L0)))
          (send stream :tyi)
          (send stream :tyi)
          (SETQ LADR (1+ LADR))
          (GO LA)
       LI
          (COND ((< (SETQ LCOUNT (1- LCOUNT)) 0)
                 (GO L0)))
          (let ((q-adr (+ (%region-origin sys:micro-code-paging-area)
                          (* ladr 2))))
                                                ;  (si:mark-not-free q-adr)
            (%p-dpb-offset (send stream :tyi) (byte #o20 0) 0 q-adr)
            (%p-dpb-offset (send stream :tyi) (byte #o20 #o20) 0 q-adr)
            (%p-dpb-offset (send stream :tyi) (byte #o20 0) 0 (1+ q-adr))
            (%p-dpb-offset (send stream :tyi) (byte #o20 #o20) 0 (1+ q-adr)))
          (SETQ LADR (1+ LADR))
          (GO LI))))

(DEFUN LOAD-LMC-FILE-INTO-ARRAYS (&OPTIONAL FILENAME)
  (declare (values i-mem-array a-mem-array main-memory-array macro-ir-dispatch-array
                   lmc-file-pathname lmc-version-number))
  (let ((i-mem-array (make-array 200000))
        (a-mem-array (make-array 10000))
        (main-mem-array (make-array 4000))
        (macro-ir-dispatch-array (make-array 10000))
        (version))
    (cond ((null filename)
           (setq filename %microcode-version-number)))
    (SETQ FILENAME (COND ((EQ FILENAME T)
                          (PATHNAME "SYS: UBIN; ULAMBDA LMC >"))
                         ((NUMBERP FILENAME)
                          (SEND (PATHNAME "SYS: UBIN; ULAMBDA LMC >")
                                :NEW-VERSION "LMC"))
                         (T
                          (FS:MERGE-PATHNAME-DEFAULTS FILENAME))))
    (UNLESS (STRING-EQUAL (SEND FILENAME :TYPE) "LMC")
      (FERROR "~A is not a LMC file." FILENAME))
    (WITH-OPEN-FILE (stream FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
      (PROG (HCODE LCODE HADR LADR HCOUNT LCOUNT HD LD
             UDSP-NBLKS UDSP-RELBLK)
            (let ((tn (send-if-handles stream :truename)))
              (if tn (SETQ VERSION (SEND tn :VERSION))))
         L0     (SETQ LCODE (SEND STREAM :TYI) HCODE (SEND STREAM :TYI))
            (COND ((OR (NOT (ZEROP HCODE)) (< LCODE 0) (> LCODE 6))
                   (FERROR "BAD CODE HCODE=~O LCODE=~O" HCODE LCODE)))
            (SETQ LADR (SEND STREAM :TYI) HADR (SEND STREAM :TYI))
            (SETQ LCOUNT (SEND STREAM :TYI) HCOUNT (SEND STREAM :TYI))
            (COND ((OR (NOT (ZEROP HADR))
                       (NOT (ZEROP HCOUNT)))
                   (FERROR "BAD HEADER SA ~O,~O COUNT ~O,~O"
                           HADR LADR HCOUNT LCOUNT)))
            (COND ((ZEROP LCODE)
                   (COND (UDSP-NBLKS
                          (SEND STREAM :SET-POINTER (* 2 UDSP-RELBLK SI:PAGE-SIZE))
                          (DO ((ADR 0 (1+ ADR))
                               (FIN (* UDSP-NBLKS SI:PAGE-SIZE)))
                              ((= ADR FIN))
                            (aset (LET ((LOW (SEND STREAM :TYI)))
                                    (DPB (SEND STREAM :TYI) (BYTE #o20 #o20) LOW))
                                  main-mem-array
                                  adr))))
                   (CLOSE STREAM)
                   (RETURN (VALUES I-MEM-ARRAY A-MEM-ARRAY
                                   MAIN-MEM-ARRAY MACRO-IR-DISPATCH-ARRAY FILENAME VERSION)))
                  ((= LCODE 1) (GO LI))         ;I-MEM
                  ((= LCODE 3)                  ;IGNORE MAIN MEMORY LOAD
                   (SETQ UDSP-NBLKS LADR)
                   (SETQ UDSP-RELBLK LCOUNT)
                   (SETQ HD (SEND STREAM :TYI) LD (SEND STREAM :TYI))   ;PHYS MEM ADR
                   (GO L0))
                  ((= LCODE 4) (GO LA)) ;A-MEM
                  ((= LCODE 5)
                   (GO LMID))           ;macro-ir-decode memory
                  ((= LCODE 6)
                   (CLOSE STREAM)
                   (RETURN (VALUES I-MEM-ARRAY A-MEM-ARRAY
                                   MAIN-MEM-ARRAY MACRO-IR-DISPATCH-ARRAY FILENAME VERSION)))
                  (T (FERROR "BAD CODE ~S" LCODE)))
         LA (COND ((< (SETQ LCOUNT (1- LCOUNT)) 0)
                       (GO L0)))
            (aset (LET ((LOW (SEND STREAM :TYI)))
                    (DPB (SEND STREAM :TYI) (BYTE #O20 #o20) LOW))
                  a-mem-array
                  ladr)
            (SETQ LADR (1+ LADR))
            (GO LA)
         LI (COND ((< (SETQ LCOUNT (1- LCOUNT)) 0)
                   (GO L0)))
            (LET ((W1 (SEND STREAM :TYI))
                  (W2 (SEND STREAM :TYI))
                  (W3 (SEND STREAM :TYI))
                  (W4 (SEND STREAM :TYI)))
              (aset (dpb w4 (byte #o20 #o60)
                         (dpb w3 (byte #o20 #o40)
                              (dpb w2 (byte #o20 #o20) w1)))
                    i-mem-array
                    ladr))
            (SETQ LADR (1+ LADR))
            (GO LI)
       LMID (COND ((< (SETQ LCOUNT (1- LCOUNT)) 0)
                   (GO L0)))
            (aset (LET ((LOW (SEND STREAM :TYI)))
                    (DPB (SEND STREAM :TYI) (BYTE #o20 #o20) LOW))
                  macro-ir-dispatch-array
                  ladr)
            (SETQ LADR (1+ LADR))
            (GO LMID)))))

;;; Compare a microcode file with a partiion on me, LAMBDA style.
;;; Note that the halfwords are IN order in a LMC file (as opposed to a MCR file).
(DEFUN COMPARE-LMC-FILE (FILENAME PART &OPTIONAL UNIT)
  "Compare microcode from file FILENAME with partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet,
or /"CC/" or /"LAM/" which refers to the machine being debugged by this one."
  (setq unit (default-disk-unit unit))
  (SETQ FILENAME (IF (NUMBERP FILENAME)
                     (SEND (PATHNAME "SYS: UBIN; ULAMBDA LMC >")
                           :NEW-VERSION FILENAME)
                   (FS:MERGE-PATHNAME-DEFAULTS FILENAME)))
  (UNLESS (STRING-EQUAL (SEND FILENAME :TYPE) "LMC")
    (FERROR "~A is not a LMC file." FILENAME))
  (WITH-DECODED-DISK-UNIT (UNIT UNIT
                                (FORMAT NIL "Comparing ~A with ~A partition" FILENAME PART)
                                NIL
                                NIL)
    (WITH-DISK-RQB (RQB)
      (WITH-DISK-RQB (RQB-FOR-LABEL 3)
        (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE)
            (FIND-DISK-PARTITION-FOR-READ PART RQB-FOR-LABEL UNIT NIL "LMC")
          (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
            (BLOCK DONE
              (DO ((BUF16 (rqb-buffer rqb))
                   (BLOCK PART-BASE (1+ BLOCK))
                   (N PART-SIZE (1- N)))
                  ((ZEROP N) (FORMAT T "~&File is longer than partition"))
                (DISK-READ RQB UNIT BLOCK)
                (DO ((LH) (RH)
                     (I 0 (+ I 2)))
                    ((= I #o1000))
                  (SETQ RH (SEND FILE ':TYI)    ;note halfwords in "right" order in LMC file
                        LH (SEND FILE ':TYI))
                  (COND ((OR (NULL LH) (NULL RH))
                         (RETURN-FROM DONE NIL)))
                  (COND ((OR (NOT (= RH (AREF BUF16 I)))
                             (NOT (= LH (AREF BUF16 (1+ I)))))
                         (FORMAT T "~&Compare error:  adr ~O; file ~O-~O; partition ~O-~O"
                                 I LH RH (AREF BUF16 (1+ I)) (AREF BUF16 I)))))))))))))


(DEFUN PARTITION-COMMENT (PART UNIT &AUX DESC-LOC)
  "Return the comment in the disk label for partition PART, unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (COND ((AND (CLOSUREP UNIT)
              (FUNCALL UNIT ':HANDLES-LABEL))
         (FUNCALL UNIT ':PARTITION-COMMENT PART))
        ('ELSE
         (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
           (SETQ DESC-LOC (NTH-VALUE 2 (FIND-DISK-PARTITION PART RQB UNIT)))
           (let ((words-per-part (get-disk-fixnum rqb
                                                  (ecase (get-disk-fixnum rqb 1)
                                                    (1 #o201)
                                                    (2 (+ 256. 3)))))
                 (words-before-comment (ecase (get-disk-fixnum rqb 1)
                                         (1 3)
                                         (2 4))))
             (COND ((NULL DESC-LOC) NIL)
                   (( words-per-part 7)
                    (GET-DISK-STRING RQB
                                     (+ DESC-LOC words-before-comment)
                                     (* 4 (- words-per-part words-before-comment))))
                   (T "")))))))


(DEFUN MAXIMUM-PARTITION-COMMENT-LENGTH (PART UNIT &AUX DESC-LOC)
  "Returns the maximum length in characters of the descriptive partition comments on UNIT"
  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
    (SETQ DESC-LOC (NTH-VALUE 2 (FIND-DISK-PARTITION PART RQB UNIT)))
    (let ((words-per-part (get-disk-fixnum rqb (ecase (get-disk-fixnum rqb 1)
                                                 (1 #o201)
                                                 (2 (+ 256. 3)))))
          (words-before-comment (ecase (get-disk-fixnum rqb 1)
                                  (1 3)
                                  (2 4))))
      (COND ((NULL DESC-LOC) NIL)
            (( words-per-part 7)
             (* 4 (- words-per-part words-before-comment)))
            (T 0)))))


(DEFUN GET-UCODE-VERSION-FROM-COMMENT (PART UNIT &OPTIONAL RQB ALREADY-READ-P)

  "Return the microcode version stored in partition PART on unit UNIT.
This works by parsing the comment in the disk label.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (WITH-DISK-RQB (RQB (OR RQB DISK-LABEL-RQB-PAGES))
    (LET* ((DESC-LOC (NTH-VALUE 2 (FIND-DISK-PARTITION PART RQB UNIT ALREADY-READ-P)))
           (words-per-part (get-disk-fixnum rqb (ecase (get-disk-fixnum rqb 1)
                                                  (1 #o201)
                                                  (2 (+ 256. 3)))))
           (words-before-comment (ecase (get-disk-fixnum rqb 1)
                                   (1 3)
                                   (2 4)))
           (COMMENT (AND DESC-LOC
                         ( words-per-part 7)
                         (GET-DISK-STRING RQB
                                          (+ DESC-LOC words-before-comment)
                                          (* 4 (- words-per-part words-before-comment))))))
      (cond ((null comment) nil)
            (t
             (let ((*read-base* 10.)
                   (*package* (find-package 'si))
                   ucode-name end-of-name)
               (multiple-value (ucode-name end-of-name)
                 (read-from-string comment))
               (cond ((memq ucode-name '(ucadr ulambda control))
                      (read-from-string comment 'no-eof-option end-of-name)))))))))


;;; Change the comment on a partition
(DEFUN UPDATE-PARTITION-COMMENT (PART STRING UNIT &AUX RQB DESC-LOC)
  "Set the comment in the disk label for partition PART, unit UNIT to STRING.
UNIT can be a disk unit number, the name of a machine on the chaos net,
or /"CC/" which refers to the machine being debugged by this one."
  (IF (AND (CLOSUREP UNIT)
           (FUNCALL UNIT ':HANDLES-LABEL))
      (FUNCALL UNIT ':UPDATE-PARTITION-COMMENT PART STRING)
    (WITH-DISK-RQB (RQB (OR RQB DISK-LABEL-RQB-PAGES))
      (SETQ DESC-LOC (NTH-VALUE 2
                       (FIND-DISK-PARTITION-FOR-READ PART RQB UNIT NIL NIL)))
      (let ((words-per-part (get-disk-fixnum rqb (ecase (get-disk-fixnum rqb 1)
                                                   (1 #o201)
                                                   (2 (+ 256. 3)))))
            (words-before-comment (ecase (get-disk-fixnum rqb 1)
                                    (1 3)
                                    (2 4))))
        (AND ( words-per-part 7)
             (PUT-DISK-STRING RQB
                              STRING
                              (+ DESC-LOC words-before-comment)
                              (* 4 (- words-per-part words-before-comment)))))
      (WRITE-DISK-LABEL RQB UNIT))))


;;; Copying a partition from one unit to another
;;; new code 18-Feb-86 11:35:17 -GJC

(DEFUN COPY-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART &OPTIONAL &KEY
                            (PAGES-AT-A-TIME (min page-rqb-size 85.))
                            (VERBOSE T)
                            (STARTING-HUNDRED 0)
                            (WHOLE-THING-P NIL)
                            (DELAY NIL)
                            (subset-start nil)
                            (subset-n-blocks nil))
  "Copy partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT"
  (SERVICE-DISK-PARTITION FROM-UNIT FROM-PART TO-UNIT TO-PART
                          :PAGES-AT-A-TIME PAGES-AT-A-TIME
                          :VERBOSE VERBOSE
                          :STARTING-HUNDRED STARTING-HUNDRED
                          :WHOLE-THING-P WHOLE-THING-P
                          :DELAY DELAY
                          :COMPARE NIL
                          :subset-start subset-start
                          :subset-n-blocks subset-n-blocks))

(DEFUN COMPARE-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART &OPTIONAL &KEY
                               (PAGES-AT-A-TIME (min page-rqb-size 85.))
                               (VERBOSE T)
                               (STARTING-HUNDRED 0)
                               (WHOLE-THING-P NIL)
                               (DELAY NIL)
                               (subset-start nil)
                               (subset-n-blocks nil))
  "Compare partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT.
If the partitions are identical, T is returned, otherwise NIL."
  (SERVICE-DISK-PARTITION FROM-UNIT FROM-PART TO-UNIT TO-PART
                          :PAGES-AT-A-TIME PAGES-AT-A-TIME
                          :VERBOSE VERBOSE
                          :STARTING-HUNDRED STARTING-HUNDRED
                          :WHOLE-THING-P WHOLE-THING-P
                          :DELAY DELAY
                          :COMPARE T
                          :subset-start subset-start
                          :subset-n-blocks subset-n-blocks))


(DEFUN COPY-DISK-PARTITION-BACKGROUND (FROM-UNIT FROM-PART TO-UNIT TO-PART &OPTIONAL &KEY
                                       (STARTING-HUNDRED 0)
                                       (subset-start nil)
                                       (subset-n-blocks nil))
  (PROCESS-RUN-FUNCTION "copy partition"
                        #'SERVICE-DISK-PARTITION
                        FROM-UNIT FROM-PART TO-UNIT TO-PART
                        :VERBOSE NIL
                        :STARTING-HUNDRED STARTING-HUNDRED
                        :subset-start subset-start
                        :subset-n-blocks subset-n-blocks))

(DEFUN SERVICE-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART &OPTIONAL &KEY
                               (PAGES-AT-A-TIME (min page-rqb-size 85.))
                               (VERBOSE T)
                               (STARTING-HUNDRED 0)
                               (WHOLE-THING-P NIL)
                               (DELAY NIL)
                               (SEQUENTIAL-OPTIMIZE T)
                               (COMPARE NIL)
                               (subset-start nil)
                               (subset-n-blocks nil))
  (WITH-DECODED-DISK-UNIT (FROM-UNIT FROM-UNIT (FORMAT NIL "reading ~A partition" FROM-PART))
    (WITH-DECODED-DISK-UNIT (TO-UNIT TO-UNIT
                                     (FORMAT NIL "~:[writing~;reading~] ~A partition"
                                             COMPARE
                                             TO-PART)
                                     NIL
                                     (NOT COMPARE))     ;write-p
      (MULTIPLE-VALUE-BIND (FROM-PART-BASE FROM-PART-SIZE NIL FROM-PART)
          (FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT)
        (MULTIPLE-VALUE-BIND (TO-PART-BASE TO-PART-SIZE NIL TO-PART)
            (IF COMPARE
                (FIND-DISK-PARTITION-FOR-READ TO-PART NIL TO-UNIT)
              (FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
          (OR TO-PART-BASE
              (RETURN-FROM SERVICE-DISK-PARTITION NIL))
          (LET ((PART-COMMENT (PARTITION-COMMENT FROM-PART FROM-UNIT)))
            (COND ((NOT VERBOSE))
                  (COMPARE
                   (FORMAT T "~&Comparing ~S and ~S"
                           PART-COMMENT (PARTITION-COMMENT TO-PART TO-UNIT)))
                  ('ELSE
                   (FORMAT T "~&Copying ~S" PART-COMMENT)))
            (LET ((MEASURED-FROM-PART-SIZE (AND (NOT WHOLE-THING-P)
                                                (MEASURED-FROM-PART-SIZE FROM-UNIT
                                                                         FROM-PART
                                                                         FROM-PART-BASE
                                                                         FROM-PART-SIZE))))
              (COND ((NOT VERBOSE))
                    (MEASURED-FROM-PART-SIZE
                     (FORMAT T "... using measured size of ~D blocks." MEASURED-FROM-PART-SIZE))
                    ('ELSE
                     (FORMAT T "... using total size of ~D blocks." FROM-PART-SIZE)))
              (UNLESS (>= TO-PART-SIZE (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE))
                (CERROR "leave out last part"
                        "Target partition is only ~D blocks long; ~D needed."
                        TO-PART-SIZE (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE)))
              (IF VERBOSE (FORMAT T "~%"))
              (WHEN (NOT COMPARE)
                (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
                (WHEN (AND (CLOSUREP TO-UNIT)
                           (FUNCALL TO-UNIT :HANDLES-LABEL))
                  ;; This is the magtape unit which is sequential
                  (FUNCALL TO-UNIT :PUT PART-COMMENT :COMMENT)
                  (FUNCALL TO-UNIT
                           :PUT
                           (OR MEASURED-FROM-PART-SIZE FROM-PART-SIZE)
                           :SIZE)))
              (PROG1 (let ((offset (or subset-start (* 100. STARTING-HUNDRED))))
                       (COPY-DISK-PORTION
                         FROM-UNIT
                         FROM-PART-BASE
                         (+ FROM-PART-BASE (OR (and subset-n-blocks (+ subset-n-blocks offset))
                                               MEASURED-FROM-PART-SIZE
                                               FROM-PART-SIZE))
                         TO-UNIT
                         TO-PART-BASE
                         (+ TO-PART-BASE   (or (and subset-n-blocks (+ subset-n-blocks offset))
                                               TO-PART-SIZE))
                         :OFFSET offset
                         :PAGES-AT-A-TIME PAGES-AT-A-TIME
                         :COMPARE COMPARE
                         :DELAY DELAY
                         :VERBOSE VERBOSE
                         :SEQUENTIAL-OPTIMIZE (AND SEQUENTIAL-OPTIMIZE
                                                   (NOT (MAGTAPE-UNIT-P FROM-UNIT))
                                                   (NOT (MAGTAPE-UNIT-P TO-UNIT)))))
                     (WHEN (NOT COMPARE)
                       (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))))))))))



(DEFUN MAGTAPE-UNIT-P (FROM-UNIT)
  (AND (CLOSUREP FROM-UNIT)
       (EQ (CLOSURE-FUNCTION FROM-UNIT) 'FS:BAND-MAGTAPE-HANDLER)))


;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:01
(DEFUN MEASURED-FROM-PART-SIZE (FROM-UNIT FROM-PART FROM-PART-BASE FROM-PART-SIZE)
  "measure size, or first value of NIL if measured size cant be trusted"
  (COND ((MAGTAPE-UNIT-P FROM-UNIT)
         ;; PROTOCOL KLUDGE. WE ALREADY KNOW THE SIZE IS CORRECT FROM FIND-DISK-PARTITION-FOR-READ
         ;; AND WE ALSO KNOW THAT WE CANNOT DO RANDOM READS ON SUCH A UNIT.
         ())
        ((STRING-EQUAL FROM-PART "LOD" :END1 3)
         (WITH-DISK-RQB (RQB 1)
           (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
           (LET* ((BUF (RQB-BUFFER RQB))
                  (SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE))
                  (FINAL-SIZE (IF (AND (> SIZE #o10) ( SIZE FROM-PART-SIZE))
                                  SIZE
                                FROM-PART-SIZE))
                  (MEMORY-SIZE
                    (SYS-COM-PAGE-NUMBER BUF %SYS-COM-HIGHEST-VIRTUAL-ADDRESS)))
               (AND (> SIZE #o10)
                    ( SIZE FROM-PART-SIZE)
                    (VALUES FINAL-SIZE
                            (IF (= (AREF BUF (* 2 %SYS-COM-BAND-FORMAT)) #o1000)
                                MEMORY-SIZE FINAL-SIZE)
                            (AREF BUF (* 2 %SYS-COM-DESIRED-MICROCODE-VERSION)))))))
        ((OR (STRING-EQUAL FROM-PART "LMC" :END1 3)
             ;;(STRING-EQUAL FROM-PART "MCR" :END1 3)
             )
         (WITH-DISK-RQB (RQB)
           (LET ((LMC-FORMATP (STRING-EQUAL FROM-PART "LMC" :END1 3))
                 (I-MEM-LOCATIONS 0)
                 (MAIN-MEMORY-PAGES 0)
                 (A//M-LOCATIONS 0)
                 (MID-LOCATIONS 0)
                 (MEASURED-SIZE 0)
                 (DISK-WORD-ADDRESS (* PAGE-SIZE FROM-PART-BASE))
                 (M-B-section-type)
                 (M-C-initial-address)
                 (M-D-n-locs))
             (DO-FOREVER
               (SETQ M-B-section-type (READ-lmc-word disk-WORD-address RQB FROM-unit LMC-FORMATP))
               (SETQ M-C-initial-address (READ-LMC-WORD (1+ disk-WORD-address) RQB FROM-unit LMC-FORMATP))
               (SETQ M-D-n-locs (READ-lmc-word (+ 2 disk-WORD-address) RQB FROM-unit LMC-FORMATP))
               (ECASE M-B-section-type
                 (1                        ;I-MEM
                  (setq I-MEM-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ DISK-WORD-ADDRESS 3 (* 2 M-D-n-locs))))
                 (2                        ;D--MEM obsolete
                  nil)
                 (3                        ;MAIN MEMORY
                  (setq MAIN-MEMORY-PAGES M-C-initial-address
                        DISK-WORD-ADDRESS (+ DISK-WORD-ADDRESS 3 1)
                        MEASURED-SIZE (+ M-C-initial-address M-D-n-locs)))
                 (4                        ;A and M Memory
                  (setq A//M-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ 3 DISK-WORD-ADDRESS M-D-n-locs))
                  (return (VALUES MEASURED-SIZE I-MEM-LOCATIONS A//M-LOCATIONS MID-LOCATIONS
                                  MAIN-MEMORY-PAGES)))
                 (5                        ;Macro-IR-Decode
                  (setq MID-LOCATIONS M-D-n-locs
                        DISK-WORD-ADDRESS (+ 3 M-D-n-locs DISK-WORD-ADDRESS))))))))))


(DEFUN READ-LMC-WORD (WORD-ADDRESS RQB UNIT LMC-FORMATP)
  (multiple-value-bind (page offset)
      (floor word-address page-size)
    (DISK-READ rqb UNIT page)
    (COND (LMC-FORMATP
           (dpb (aref (rqb-buffer RQB) (1+ (* offset 2)))
                (BYTE 16. 16.)
                (aref (rqb-buffer RQB) (* offset 2))))
          ('ELSE
           (dpb (aref (rqb-buffer RQB) (* offset 2))
                (BYTE 16. 16.)
                (aref (rqb-buffer RQB) (1+ (* offset 2))))))))


(DEFUN COPY-DISK-PORTION (FROM-UNIT FROM-START FROM-END TO-UNIT TO-START TO-END &OPTIONAL &KEY
                          (OFFSET 0) (PAGES-AT-A-TIME 1) DELAY (VERBOSE T)
                          (SEQUENTIAL-OPTIMIZE T) COMPARE)
  (LET ((N-BIG-OPS (MIN (FLOOR (- FROM-END FROM-START OFFSET) PAGES-AT-A-TIME)
                        (FLOOR (- TO-END TO-START OFFSET) PAGES-AT-A-TIME)))
        (RQB)(RQB2)
        (COMPARE-OK T)
        (HPRINTER (MAKE-SEQUENCE-PRINTER 100.))
        (TIME (TIME))
        (FROM-TIME 0)
        (TO-TIME 0))
    (WHEN SEQUENTIAL-OPTIMIZE
      (ANTICIPATE-DISK-OPERATIONS FROM-UNIT :READ FROM-START N-BIG-OPS PAGES-AT-A-TIME)
      (ANTICIPATE-DISK-OPERATIONS TO-UNIT (IF COMPARE :READ :WRITE)
                                  TO-START N-BIG-OPS PAGES-AT-A-TIME))
    (UNWIND-PROTECT
        (DO ((FROM-INDEX (+ FROM-START OFFSET))
             (TO-INDEX (+ TO-START OFFSET))
             (AMT))
            ((OR (>= FROM-INDEX FROM-END)
                 (>= TO-INDEX TO-END)
                 (< FROM-INDEX FROM-START)
                 (< TO-INDEX TO-START))
             (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
             (WHEN VERBOSE
               (FORMAT
                 T
                 "~&Took ~$ minutes realtime, ~$ from unit ~$ to unit, ~$ Kbytes per second~%"
                 (QUOTIENT TIME (* 60.0 60.0))
                 (QUOTIENT FROM-TIME (* 60.0 60.0))
                 (QUOTIENT TO-TIME (* 60.0 60.0))
                 (QUOTIENT (* (- FROM-END FROM-START OFFSET)
                              1024.
                              60.0)
                           (* 1000. TIME))))
             COMPARE-OK)
          (SETQ AMT (MIN (- FROM-END FROM-INDEX) (- TO-END TO-INDEX) PAGES-AT-A-TIME))
          (WHEN (NOT (AND RQB (= AMT PAGES-AT-A-TIME)))
            (RETURN-DISK-RQB RQB)
            (SETQ RQB (GET-DISK-RQB AMT)))
          (WHEN (AND COMPARE (NOT (AND RQB2 (= AMT PAGES-AT-A-TIME))))
            (RETURN-DISK-RQB RQB2)
            (SETQ RQB2 (GET-DISK-RQB AMT)))
          (condition-case ()
              (LET ((TM (TIME)))
                (disk-read rqb from-unit FROM-INDEX)
                (INCF FROM-TIME (TIME-DIFFERENCE (TIME) TM)))
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP
                     (- FROM-INDEX
                        (COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME FROM-UNIT
                                                                 FROM-INDEX))))
               (DECF FROM-INDEX BACKUP)
               (DECF TO-INDEX BACKUP))
             (GO RETRY-READ-WRITE)))
          (CONDITION-CASE ()
              (LET ((TM (TIME)))
                (IF COMPARE
                    (DISK-READ RQB2 TO-UNIT TO-INDEX)
                  (DISK-WRITE rqb to-unit TO-INDEX))
                (INCF TO-TIME (TIME-DIFFERENCE (TIME) TM)))
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP (- TO-INDEX
                              (FUNCALL (IF COMPARE
                                           #'COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME
                                         #'COPY-DISK-PORTION-NEXT-WRITE-TAPE-VOLUME)
                                       TO-UNIT
                                       TO-INDEX))))
               (DECF FROM-INDEX BACKUP)
               (DECF TO-INDEX BACKUP))
             (GO RETRY-READ-WRITE)))
          (WHEN COMPARE
            (SETQ COMPARE-OK
                  (COMPARE-DISK-DATA RQB RQB2 AMT (- FROM-INDEX FROM-START))))
          (WHEN VERBOSE (FUNCALL HPRINTER (+ AMT (- FROM-INDEX FROM-START))))
          (IF DELAY
              (PROCESS-SLEEP DELAY)
            (PROCESS-ALLOW-SCHEDULE))
          (INCF FROM-INDEX AMT)
          (INCF TO-INDEX AMT)
          RETRY-READ-WRITE)
      (AND RQB (RETURN-DISK-RQB RQB))
      (AND RQB2 (RETURN-DISK-RQB RQB2)))))


(DEFUN COMPARE-DISK-DATA (RQB1 RQB2 AMT BLOCK)
  (OR (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
        (%STRING-EQUAL (RQB-8-BIT-BUFFER RQB1) 0
                       (RQB-8-BIT-BUFFER RQB2) 0
                       (* 1024 AMT)))
      (DO ((C 0 (1+ C))
           (ERRS 0)
           (LIM (* 512 AMT))
           (BUF1 (RQB-BUFFER RQB1))
           (BUF2 (RQB-BUFFER RQB2)))
          ((OR (= C LIM) (= ERRS 3))
           NIL)
        (WHEN (NOT (= (AREF BUF1 C) (AREF BUF2 C)))
          (FORMAT T "~%ERROR in Block ~D. Halfword ~D., S1: ~O S2: ~O "
                  (+ BLOCK (FLOOR C 512))
                  (REMAINDER C 512)
                  (AREF BUF1 C)
                  (AREF BUF2 C))
          (INCF ERRS)))))


(DEFUN MAKE-SEQUENCE-PRINTER (&OPTIONAL (N 1) &AUX LAST)
  #'(LAMBDA (I)
      (LET ((NEW (FLOOR I N)))
        (COND ((ZEROP NEW))
              ((AND LAST (= NEW LAST)))
              ('ELSE
               (FORMAT T "~D " NEW)
               (SETQ LAST NEW))))))


(DEFUN ANTICIPATE-DISK-OPERATIONS (UNIT OPERATION ADDRESS N-OPERATIONS RQB-NPAGES)
  (AND (INSTANCEP UNIT)
       (SEND-IF-HANDLES UNIT :ANTICIPATE-OPERATIONS OPERATION ADDRESS N-OPERATIONS RQB-NPAGES)))



;;; This stuff for handling magtape is somewhat non-modular.
;;; Maybe magtape unit will get rewritten.

(defun MT-UNIT-MAGTAPE-UNIT (from-unit &AUX WIN)
  ;; a gross violation of modularity.
  (IF (AND (CLOSUREP FROM-UNIT)
           (EQ (CLOSURE-FUNCTION FROM-UNIT) 'FS:BAND-MAGTAPE-HANDLER)
           (SETQ WIN (SYMEVAL-IN-CLOSURE FROM-UNIT 'FS:*BAND-STREAM*)))
      (OR (SEND-IF-HANDLES WIN :UNIT) ;; THE OLD MAGTAPE CODE.
          (SEND (SEND WIN :DEVICE) :UNIT)) ;; dave goodines winning new stuff.
    (FERROR NIL "not an open magtape unit: ~S" from-unit)))

(DEFUN COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME (UNIT EXPECT-INDEX)
  (FS:MT-UNLOAD (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                     :list-choices nil)
          "End of tape reached ... mount next tape and type RESUME ")
  (let ((NEXT-INDEX (funcall unit ':prepare-next-volume-for-read)))
    (OR (<= NEXT-INDEX EXPECT-INDEX)
        (cerror "continue anyway"
                "The last tape ended at block ~d., but this tape starts at ~d."
                EXPECT-INDEX
                NEXT-INDEX))
    NEXT-INDEX))

(DEFVAR *NEXT-WRITE-VOLUME-BACKUP-PAGES* 20.)

(defun COPY-DISK-PORTION-NEXT-WRITE-TAPE-VOLUME (UNIT INDEX &AUX RECORD-SIZE)
  (SETQ RECORD-SIZE (IF (AND (CLOSUREP UNIT)
                             (EQ (CLOSURE-FUNCTION UNIT) 'FS:BAND-MAGTAPE-HANDLER)
                             (SETQ RECORD-SIZE (SYMEVAL-IN-CLOSURE UNIT 'FS:*BAND-STREAM*)))
                        (SEND RECORD-SIZE :RECORD-SIZE)
                      4096.))

  (FS:MT-SPACE-REV (ROUND (* *NEXT-WRITE-VOLUME-BACKUP-PAGES* 1024)
                          RECORD-SIZE)
                   (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:mt-write-eof (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:mt-write-eof (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fs:MT-UNLOAD (MT-UNIT-MAGTAPE-UNIT UNIT))
  (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                     :list-choices nil)
          "End of tape reached ... mount next tape and type RESUME ")
  (funcall UNIT ':prepare-next-volume-for-write
           (- INDEX (* 2 *NEXT-WRITE-VOLUME-BACKUP-PAGES*)))
  (format t "~&Continuing at partition address ~d.~&"
          (- INDEX (* 2 *NEXT-WRITE-VOLUME-BACKUP-PAGES*)))
  ;; WE INDICATE OVERBACKUP FOR SAFETY. ENDS OF TAPES DO GET FRAYED
  (- INDEX (* 2 *NEXT-WRITE-VOLUME-BACKUP-PAGES*)))

;;; Useful to see if a partition has been corrupted.
;;; Run this on release bands etc. Save result in a file to be able to recheck
;;; things later. -gjc


(DEFUN SXHASH-DISK-PARTITION (UNIT &OPTIONAL (PART NIL PARTP) &KEY
                              SAVE-IN-FILE
                              (PAGES-AT-A-TIME (min #+LMI page-rqb-size 85.))
                              (VERBOSE T)
                              (STARTING-HUNDRED 0)
                              (WHOLE-THING-P NIL)
                              (DELAY NIL)
                              (SEQUENTIAL-OPTIMIZE T))
  "Return a list of SXHASH of the blocks in a partition, or save data in a file.
With one argument, a filename, loads saved data from file"
  (COND ((NOT PARTP)
         (LOAD-DISK-PARTITION-SXHASH UNIT))
        ('ELSE
         (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "reading ~A partition" PART))
           (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
               (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT)
             (LET ((PART-COMMENT (PARTITION-COMMENT PART UNIT)))
               (when verbose
                 (format t "~&Computing SXHASH of ~S" PART-COMMENT))
               (LET ((MEASURED-PART-SIZE (AND (NOT WHOLE-THING-P)
                                              (MEASURED-FROM-PART-SIZE UNIT
                                                                       PART
                                                                       PART-BASE
                                                                       PART-SIZE))))
                 (COND ((NOT VERBOSE))
                       (MEASURED-PART-SIZE
                        (FORMAT T "... using measured size of ~D blocks.~%" MEASURED-PART-SIZE))
                       ('ELSE
                        (FORMAT T "... using total size of ~D blocks.~%" PART-SIZE)))
                 (LET ((RESULT (SXHASH-DISK-PORTION
                                 UNIT
                                 PART-BASE
                                 (+ PART-BASE (OR MEASURED-PART-SIZE PART-SIZE))
                                 :OFFSET (* 100. STARTING-HUNDRED)
                                 :PAGES-AT-A-TIME PAGES-AT-A-TIME
                                 :DELAY DELAY
                                 :VERBOSE VERBOSE
                                 :SEQUENTIAL-OPTIMIZE (AND SEQUENTIAL-OPTIMIZE
                                                           (NOT (MAGTAPE-UNIT-P UNIT))))))
                   (COND ((NOT SAVE-IN-FILE)
                          (CONS PART-COMMENT RESULT))
                         ('ELSE
                          (SAVE-DISK-PARTITION-SXHASH PART-COMMENT RESULT SAVE-IN-FILE)))))))))))

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:04
(DEFUN SAVE-DISK-PARTITION-SXHASH (PART-COMMENT RESULT FILENAME)
  (LET ((FNAME (FS:PARSE-PATHNAME FILENAME)))
    (WHEN (MEMQ (SEND FNAME :NAME) '(NIL :UNSPECIFIC))
      (SETQ FNAME (SEND FNAME :NEW-NAME
                        (WITH-INPUT-FROM-STRING (SI PART-COMMENT)
                          (WITH-OUTPUT-TO-STRING (SO)
                            (DO ((C))
                                ((NOT (SETQ C (SEND SI :TYI))))
                              (SETQ C (CHAR-UPCASE C))
                              (COND ((OR (CHAR<= #\A C #\Z)
                                         (CHAR<= #\0 C #\9)
                                         (CHAR= C #\-))
                                     (SEND SO :TYO C))
                                    ((CHAR= C #\.)
                                     (SEND SO :TYO #\P))
                                    ('ELSE
                                     (SEND SO :TYO #\-)))))))))
    (WHEN (MEMQ (SEND FNAME :TYPE) '(NIL :UNSPECIFIC))
      (SETQ FNAME (SEND FNAME :NEW-TYPE "PDATA")))
    (WITH-OPEN-FILE (STREAM FNAME :DIRECTION :OUTPUT :BYTE-SIZE 16. :CHARACTERS NIL)
      (DOLIST (X (GET 'LOAD-DISK-PARTITION-SXHASH T))
        (SEND STREAM :TYO X))
      (SEND STREAM :TYO (LENGTH PART-COMMENT))
      (DOTIMES (J (LENGTH PART-COMMENT))
        (SEND STREAM :TYO (AREF PART-COMMENT J)))
      (SEND STREAM :TYO (LDB (BYTE 16. 0) (LENGTH RESULT)))
      (SEND STREAM :TYO (LDB (BYTE 16. 16.) (LENGTH RESULT)))
      (DOLIST (X RESULT)
        (SEND STREAM :TYO (LDB (BYTE 16. 0) X))
        (SEND STREAM :TYO (LDB (BYTE 16. 16.) X)))
      (SEND STREAM :TRUENAME))))

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:05
(DEFUN LOAD-DISK-PARTITION-SXHASH-1 (STREAM)
  ;; format of file is magic numbers ,PART-COMMENT LENGTH, PART-COMMENT, LENGTH, DATA.
  (DOLIST (X (GET 'LOAD-DISK-PARTITION-SXHASH T))
    (OR (= (SEND STREAM :TYI) X)
        (FERROR NIL "~S is not a disk partition SXHASH file" (SEND STREAM :TRUENAME))))
  (LET ((PART-COMMENT (MAKE-STRING (SEND STREAM :TYI))))
    (DO ((J 0 (1+ J)))
        ((= J (LENGTH PART-COMMENT)))
      (SETF (AREF PART-COMMENT J) (SEND STREAM :TYI)))
    (LET ((L (MAKE-LIST (+ (SEND STREAM :TYI) (ASH (SEND STREAM :TYI) 16.)))))
      (DO ((L L (CDR L)))
          ((NULL L))
        (RPLACA L (+ (SEND STREAM :TYI) (ASH (SEND STREAM :TYI) 16.))))
      (CONS PART-COMMENT L))))

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:06
(DEFUN LOAD-DISK-PARTITION-SXHASH (FILENAME)
  (WITH-OPEN-FILE (STREAM FILENAME :BYTE-SIZE 16. :CHARACTERS NIL)
    (LOAD-DISK-PARTITION-SXHASH-1 STREAM)))

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:06
(DEFPROP LOAD-DISK-PARTITION-SXHASH
         (#x1A2B #xF7E5 #xABCD #x345E #xFA79 #xFAAA
          #x1234 #x5678 #x9AEA #xCDD4 #xFAFA #x987C)
         T)


(DEFUN SXHASH-DISK-PORTION (UNIT START END &KEY (OFFSET 0) (PAGES-AT-A-TIME 1) DELAY
                            VERBOSE (SEQUENTIAL-OPTIMIZE T))
  (LET ((N-BIG-OPS (FLOOR (- END START OFFSET) PAGES-AT-A-TIME))
        (RQB)
        (HPRINTER (MAKE-SEQUENCE-PRINTER 100.))
        (TIME (TIME))
        (READ-TIME 0)
        (RESULT (NCONS NIL)))
    (WHEN SEQUENTIAL-OPTIMIZE
      (ANTICIPATE-DISK-OPERATIONS UNIT :READ START N-BIG-OPS PAGES-AT-A-TIME))
    (UNWIND-PROTECT
        (DO ((INDEX (+ START OFFSET))
             (AMT)
             (SOFAR RESULT))
            ((>= INDEX END)
             (SETQ TIME (TIME-DIFFERENCE (TIME) TIME))
             (WHEN VERBOSE
               (FORMAT
                 T
                 "~&Took ~$ minutes realtime, ~$ reading unit, ~$ Kbytes per second~%"
                 (QUOTIENT TIME (* 60.0 60.0))
                 (QUOTIENT READ-TIME (* 60.0 60.0))
                 (QUOTIENT (* (- END START OFFSET)
                              1024.
                              60.0)
                           (* 1000. TIME))))
             (CDR RESULT))
          (SETQ AMT (MIN (- END INDEX) PAGES-AT-A-TIME))
          (WHEN (NOT (AND RQB (= AMT PAGES-AT-A-TIME)))
            (RETURN-DISK-RQB RQB)
            (SETQ RQB (GET-DISK-RQB AMT)))
          (condition-case ()
              (LET ((TM (TIME)))
                (disk-read rqb unit INDEX)
                (INCF READ-TIME (TIME-DIFFERENCE (TIME) TM)))
            ((fs:end-of-tape si:end-of-file-1)
             (LET ((BACKUP
                     (- INDEX
                        (COPY-DISK-PORTION-NEXT-READ-TAPE-VOLUME UNIT
                                                                 INDEX))))
               (DECF INDEX BACKUP)
               (GO RETRY))))
          (NCONC SOFAR (SXHASH-RQB RQB))
          (SETQ SOFAR (LAST SOFAR))
          (WHEN VERBOSE (FUNCALL HPRINTER (+ AMT (- INDEX START))))
          (IF DELAY
              (PROCESS-SLEEP DELAY)
            (PROCESS-ALLOW-SCHEDULE))
          (INCF INDEX AMT)
          RETRY)
      (AND RQB (RETURN-DISK-RQB RQB)))))

;;; THE OLD STUFF.

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:08
(DEFUN SXHASH-RQB (RQB)
  "Return a list of the SXHASH of the pages in the RQB"
  ;; (%SXHASH-SUBSTRING STRING MASK START END)
  ;; is broken on indirect arrays if START is not 0.
  ;; Also not available on explorer. This is almost as fast.
  ;; Fortunately %STRING-SXHASH is compatible between #+LMI and #+TI.
  (LET ((N (RQB-NPAGES RQB))
        (BUFFER (RQB-8-BIT-BUFFER RQB)))
    (COND ((ZEROP N) NIL)
          ((= N 1)
           (NCONS (%SXHASH-STRING BUFFER #o377)))
          ('ELSE
           (DO ((J 0)
                (STRING (MAKE-ARRAY 1024. :TYPE 'ART-STRING :DISPLACED-TO BUFFER
                                    :DISPLACED-INDEX-OFFSET 0))
                (L NIL))
               (NIL)
             (PUSH (%SXHASH-STRING STRING #o377) L)
             (INCF J)
             (WHEN (= J N)
               (RETURN (NREVERSE L)))
             (CHANGE-INDIRECT-ARRAY STRING 'ART-STRING 1024. BUFFER (* J 1024.)))))))


(DEFUN OLD-COPY-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART
                            &OPTIONAL (N-PAGES-AT-A-TIME (min page-rqb-size 85.)) (DELAY NIL)
                                      (STARTING-HUNDRED 0) (WHOLE-THING-P NIL)
                            &AUX FROM-PART-BASE FROM-PART-SIZE TO-PART-BASE TO-PART-SIZE RQB
                                 PART-COMMENT)
  "Copy partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT.
While names of other machines can be specified as units, this
is not very fast for copying between machines.
Use SI:RECEIVE-BAND or SI:TRANSMIT-BAND for that."
  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT
                                        (FORMAT NIL "reading ~A partition" FROM-PART))
        TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT
                                      (FORMAT NIL "writing ~A partition" TO-PART)
                                      NIL
                                      T))
  (UNWIND-PROTECT
      (PROGN
        (SETQ RQB (GET-DISK-RQB N-PAGES-AT-A-TIME))
        (MULTIPLE-VALUE (FROM-PART-BASE FROM-PART-SIZE NIL FROM-PART)
          (FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT))
        (MULTIPLE-VALUE (TO-PART-BASE TO-PART-SIZE NIL TO-PART)
          (FIND-DISK-PARTITION-FOR-WRITE TO-PART NIL TO-UNIT))
        (SETQ PART-COMMENT (PARTITION-COMMENT FROM-PART FROM-UNIT))
        (FORMAT T "~&Copying ~S" PART-COMMENT)
        (AND (OR (NUMBERP FROM-PART) (STRING-EQUAL FROM-PART "LOD" :END1 3))
             (NOT WHOLE-THING-P)
             (not (and (closurep from-unit)
                       (eq (closure-function from-unit) 'FS:BAND-MAGTAPE-HANDLER)))
             (LET ((RQB NIL) (BUF NIL))
               (UNWIND-PROTECT
                   (PROGN (SETQ RQB (GET-DISK-RQB 1))
                          (SETQ BUF (RQB-BUFFER RQB))
                          (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
                          (LET ((SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE)))
                            (COND ((AND (> SIZE #o10) ( SIZE FROM-PART-SIZE))
                                   (SETQ FROM-PART-SIZE SIZE)
                                   (FORMAT T "... using measured size of ~D blocks." SIZE)))))
                 (RETURN-DISK-RQB RQB))))
        (UNLESS ( TO-PART-SIZE FROM-PART-SIZE)
          (FERROR "Target partition is only ~D blocks long; ~D needed."
                  TO-PART-SIZE FROM-PART-SIZE))
        (FORMAT T "~%")
        (UPDATE-PARTITION-COMMENT TO-PART "Incomplete Copy" TO-UNIT)
        (WHEN (AND (CLOSUREP TO-UNIT)           ;magtape needs to know this stuff before
                   (FUNCALL TO-UNIT ':HANDLES-LABEL))   ;writing file.
          (FUNCALL TO-UNIT ':PUT PART-COMMENT ':COMMENT)
          (FUNCALL TO-UNIT ':PUT FROM-PART-SIZE ':SIZE))
        ;; Old hack which used to move WIRE-DISK-RQB outside loop flushed because
        ;; DISK-READ sets modified bits during WIRE-DISK-RQB, which we may need to do.
        (DO ((FROM-ADR (+ FROM-PART-BASE (* 100. STARTING-HUNDRED)) (+ FROM-ADR AMT))
             (TO-ADR (+ TO-PART-BASE (* 100. STARTING-HUNDRED)) (+ TO-ADR AMT))
             (FROM-HIGH (+ FROM-PART-BASE FROM-PART-SIZE))
             (TO-HIGH (+ TO-PART-BASE TO-PART-SIZE))
             (N-BLOCKS (* 100. STARTING-HUNDRED) (+ N-BLOCKS AMT))
             (N-HUNDRED STARTING-HUNDRED)
             (AMT))
            ((OR ( FROM-ADR FROM-HIGH) (>= TO-ADR TO-HIGH)))
          (SETQ AMT (MIN (- FROM-HIGH FROM-ADR) (- TO-HIGH TO-ADR) N-PAGES-AT-A-TIME))
          (COND ((NOT (= AMT N-PAGES-AT-A-TIME))
                 (RETURN-DISK-RQB RQB)
                 (SETQ RQB (GET-DISK-RQB AMT))))
          retry-copy-after-eot
          (condition-case (condition)
              (disk-read rqb from-unit from-adr)
            ((fs:end-of-tape si:end-of-file-1)
             (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                                :list-choices nil)
                     "End of tape reached ... mount next tape and type RESUME ")
             (let ((new-from-adr (funcall from-unit ':prepare-next-volume-for-read)))
               (if (< from-adr new-from-adr)
                   (cerror ':no-action nil nil
                           "The last tape ended at block ~d., but this tape starts at ~d."
                           (- from-adr from-part-base)
                           (- new-from-adr from-part-base)))
               (setq n-blocks (- n-blocks (- from-adr new-from-adr)))
               (setq n-hundred (floor n-blocks 100.))
               (setq from-adr new-from-adr))
             (setq to-adr (+ to-part-base from-adr))
             (go retry-copy-after-eot)
             ))
          (condition-case (condition)
              (disk-write rqb to-unit to-adr)
            (fs:end-of-tape
             (fs:mt-write-eof)
             (fs:mt-write-eof)
             (fs:mt-rewind)
             (fquery '(:choices (((resume "Resume") #\resume #\space #/y))
                                :list-choices nil)
                     "End of tape reached ... mount next tape and type RESUME ")
             (let ((backup (* 2 amt)))
               (setq from-adr (- from-adr backup))
               (if (< from-adr from-part-base)
                   (ferror "Attempt to back up past base of partition."))
               (setq to-adr (- to-adr backup))
               (setq n-blocks (- n-blocks backup))
               (setq n-hundred (floor n-blocks 100.)))
             (funcall to-unit ':prepare-next-volume-for-write (- from-adr from-part-base))
             (format t "~&Continuing at partition address ~d.~&"
                     (- from-adr from-part-base))
             (go retry-copy-after-eot)
             ))
          (WHEN ( (FLOOR (+ N-BLOCKS AMT) 100.) N-HUNDRED)
            (INCF N-HUNDRED)
            (FORMAT T "~D " N-HUNDRED))
          (IF DELAY
              (PROCESS-SLEEP DELAY)
              (PROCESS-ALLOW-SCHEDULE)))        ;kludge
        (UPDATE-PARTITION-COMMENT TO-PART PART-COMMENT TO-UNIT))
    ;; Unwind-protect forms
    (RETURN-DISK-RQB RQB))
  (DISPOSE-OF-UNIT FROM-UNIT)
  (DISPOSE-OF-UNIT TO-UNIT))

;;; Prints differences
(DEFUN OLD-COMPARE-DISK-PARTITION (FROM-UNIT FROM-PART TO-UNIT TO-PART
                            &OPTIONAL (N-PAGES-AT-A-TIME (min 85. page-rqb-size)) (DELAY NIL)
                                      (STARTING-HUNDRED 0) (WHOLE-THING-P NIL)
                            &AUX FROM-PART-BASE FROM-PART-SIZE TO-PART-BASE TO-PART-SIZE
                                 RQB RQB2 (PARTITION-GOOD? T))
  "Compare partition FROM-PART on FROM-UNIT to partition TO-PART on TO-UNIT.
If the partitions are identical, T is returned, otherwise NIL.
As with other partition transfer functions, names of other machines can be specified as units."
  (lambda-or-cadr-only)
  (SETQ FROM-UNIT (DECODE-UNIT-ARGUMENT FROM-UNIT
                                        (FORMAT NIL "reading ~A partition" FROM-PART))
        TO-UNIT (DECODE-UNIT-ARGUMENT TO-UNIT (FORMAT NIL "reading ~A partition" TO-PART)))
  (UNWIND-PROTECT
      (PROGN
        (SETQ RQB (GET-DISK-RQB N-PAGES-AT-A-TIME))
        (SETQ RQB2 (GET-DISK-RQB N-PAGES-AT-A-TIME))
        (MULTIPLE-VALUE (FROM-PART-BASE FROM-PART-SIZE)
          (FIND-DISK-PARTITION-FOR-READ FROM-PART NIL FROM-UNIT))
        (MULTIPLE-VALUE (TO-PART-BASE TO-PART-SIZE)
          (FIND-DISK-PARTITION-FOR-READ TO-PART NIL TO-UNIT))
        (FORMAT T "~&Comparing ~S and ~S"
                (PARTITION-COMMENT FROM-PART FROM-UNIT)
                (PARTITION-COMMENT TO-PART TO-UNIT))
        (WHEN (STRING-EQUAL FROM-PART "LOD" :END1 3)
          (NOT WHOLE-THING-P)
          (LET (RQB BUF)
            (UNWIND-PROTECT
                (PROGN
                  (SETQ RQB (GET-DISK-RQB 1))
                  (SETQ BUF (RQB-BUFFER RQB))
                  (DISK-READ RQB FROM-UNIT (1+ FROM-PART-BASE))
                  (LET ((SIZE (SYS-COM-PAGE-NUMBER BUF %SYS-COM-VALID-SIZE)))
                    (COND ((AND (> SIZE #o10) ( SIZE FROM-PART-SIZE))
                           (SETQ FROM-PART-SIZE SIZE)
                           (FORMAT T "... using measured size of ~D. blocks." SIZE)))))
              (RETURN-DISK-RQB RQB))))
        (DO ((FROM-ADR (+ FROM-PART-BASE (* 100. STARTING-HUNDRED)) (+ FROM-ADR AMT))
             (TO-ADR (+ TO-PART-BASE (* 100. STARTING-HUNDRED)) (+ TO-ADR AMT))
             (FROM-HIGH (+ FROM-PART-BASE FROM-PART-SIZE))
             (TO-HIGH (+ TO-PART-BASE TO-PART-SIZE))
             (N-BLOCKS (* 100. STARTING-HUNDRED) (+ N-BLOCKS AMT))
             (N-HUNDRED STARTING-HUNDRED)
             (AMT)
             (BUF (RQB-BUFFER RQB))
             (BUF2 (RQB-BUFFER RQB2)))
            ((OR ( FROM-ADR FROM-HIGH)
                 ( TO-ADR TO-HIGH)))
          (SETQ AMT (MIN (- FROM-HIGH FROM-ADR) (- TO-HIGH TO-ADR) N-PAGES-AT-A-TIME))
          (COND ((NOT (= AMT N-PAGES-AT-A-TIME))
                 (RETURN-DISK-RQB RQB)
                 (RETURN-DISK-RQB RQB2)
                 (SETQ RQB (GET-DISK-RQB AMT))
                 (SETQ RQB2 (GET-DISK-RQB AMT))
                 (SETQ BUF (RQB-BUFFER RQB))
                 (SETQ BUF2 (RQB-BUFFER RQB2))))
          (DISK-READ RQB FROM-UNIT FROM-ADR)
          (DISK-READ RQB2 TO-UNIT TO-ADR)
          (UNLESS (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
                    (%STRING-EQUAL (RQB-8-BIT-BUFFER RQB) 0
                                   (RQB-8-BIT-BUFFER RQB2) 0
                                   (* #o2000 AMT)))
            (DO ((C 0 (1+ C))
                 (ERRS 0)
                 (LIM (* #o1000 AMT)))
                ((OR (= C LIM) (= ERRS 3)))
              (COND ((NOT (= (AREF BUF C) (AREF BUF2 C)))
                     (SETQ PARTITION-GOOD? NIL)
                     (FORMAT T "~%ERR Block ~O Halfword ~O, S1: ~O S2: ~O "
                             (+ (- FROM-ADR (+ FROM-PART-BASE (* STARTING-HUNDRED 100.)))
                                (FLOOR C #o1000))
                             (\ C #o1000)
                             (AREF BUF C)
                             (AREF BUF2 C))
                     (INCF ERRS)))))
          (UNLESS (= (FLOOR N-BLOCKS 100.) N-HUNDRED)
            (SETQ N-HUNDRED (FLOOR N-BLOCKS 100.))
            (FORMAT T "~D " N-HUNDRED))
          (IF DELAY (PROCESS-SLEEP DELAY)
            (PROCESS-ALLOW-SCHEDULE)))          ;kludge
        PARTITION-GOOD?)
    ;; Unwind-protect forms
    (RETURN-DISK-RQB RQB)
    (RETURN-DISK-RQB RQB2)
    (DISPOSE-OF-UNIT FROM-UNIT)
    (DISPOSE-OF-UNIT TO-UNIT)))




;;;; User-controlled paging code

;;; We have a special RQB which is not like a normal disk RQB in that
;;; it doesn't contain any buffer.  It is exactly one page long since
;;; everything in DISK-BUFFER-AREA has to be a multiple of a page.
;;; This defines the number of CCWs.

;(DEFVAR PAGE-RQB-SIZE (- PAGE-SIZE 1 (FLOOR %DISK-RQ-CCW-LIST 2))) ;NUMBER OF CCWS
;(defvar page-rqb-size 76)
;PAGE-RQB is an array with a one word header, no leader, no long length q,
;and enough data [(1- page-size) words] to fill up the rest of exactly one page
(DEFVAR PAGE-RQB
        (MAKE-ARRAY (* 2 (1- PAGE-SIZE)) ':TYPE 'ART-16B ':AREA DISK-BUFFER-AREA))

(defun set-max-user-disk-transfer (n-pages)
  (without-interrupts
    (cond ((> page-rqb-size n-pages)
           (cond ((and (boundp 'fs:tapemaster-control-memory)
                       (not (null fs:tapemaster-control-memory)))
                  (return-disk-rqb fs:tapemaster-control-memory)
                  (setq fs:tapemaster-control-memory nil)))
           (clear-resource 'rqb)))
    (setq page-rqb-size n-pages)))

(DEFUN WIRE-PAGE-RQB ()
  (%WIRE-PAGE (%POINTER PAGE-RQB))
  (select-processor
    ((:cadr :lambda)
     (LET ((PADR (+ (%PHYSICAL-ADDRESS PAGE-RQB)
                    1
                    (FLOOR %DISK-RQ-CCW-LIST 2))))
       (SETF (AREF PAGE-RQB %DISK-RQ-CCW-LIST-POINTER-LOW) PADR)
       (SETF (AREF PAGE-RQB %DISK-RQ-CCW-LIST-POINTER-HIGH) (LSH PADR -16.))))
    (:explorer nil)))

(DEFUN UNWIRE-PAGE-RQB ()
  (%UNWIRE-PAGE (%POINTER PAGE-RQB)))

(DEFUN PAGE-IN-AREA (AREA)
  "Swap in the contents of AREA in one disk operation."
  (DO ((REGION (%AREA-REGION-LIST AREA) (%REGION-LIST-THREAD REGION)))
      ((MINUSP REGION))
    (PAGE-IN-REGION REGION)))

(DEFUN PAGE-OUT-AREA (AREA)
  "Put the contents of AREA high on the list for being swapped out."
  (DO ((REGION (%AREA-REGION-LIST AREA) (%REGION-LIST-THREAD REGION)))
      ((MINUSP REGION))
    (PAGE-OUT-REGION REGION)))

(DEFUN PAGE-IN-REGION (REGION)
  "Swap in the contents of region REGION in one disk operation."
  (PAGE-IN-WORDS (%REGION-ORIGIN REGION) (%REGION-FREE-POINTER REGION)))

(DEFUN PAGE-OUT-REGION (REGION)
  "Put the contents of region REGION high on the list for being swapped out."
  (PAGE-OUT-WORDS (%REGION-ORIGIN REGION) (%REGION-FREE-POINTER REGION)))

(DEFUN PAGE-IN-STRUCTURE (OBJ)
  "Swap in the structure STRUCTURE in one disk operation."
  (SETQ OBJ (FOLLOW-STRUCTURE-FORWARDING OBJ))
  (PAGE-IN-WORDS (%FIND-STRUCTURE-LEADER OBJ)
                 (%STRUCTURE-TOTAL-SIZE OBJ)))

(DEFUN PAGE-OUT-STRUCTURE (OBJ)
  "Put the data of structure STRUCTURE high on the list for being swapped out."
  (SETQ OBJ (FOLLOW-STRUCTURE-FORWARDING OBJ))
  (PAGE-OUT-WORDS (%FIND-STRUCTURE-LEADER OBJ)
                  (%STRUCTURE-TOTAL-SIZE OBJ)))

(DEFUN PAGE-ARRAY-CALCULATE-BOUNDS (ARRAY FROM TO)
  "FROM and TO are lists of subscripts.  If too short, zeros are appended.
Returns array, starting address of data, number of Q's of data.
First value is NIL if displaced to an absolute address (probably TV buffer)."
  (DECLARE (VALUES ARRAY DATA-START-ADDRESS ESS DATA-LENGTH))
  (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
  (LET (NDIMS TYPE START END SIZE ELTS-PER-Q)
    (BLOCK DONE
      (SETQ NDIMS (ARRAY-RANK ARRAY)
            TYPE (ARRAY-TYPE ARRAY))
      (UNLESS ( (LENGTH FROM) NDIMS)
        (FERROR "Too many dimensions in starting index ~S" FROM))
      (UNLESS ( (LENGTH TO) NDIMS)
        (FERROR "Too many dimensions in ending index ~S" TO))
      (SETQ START (OR (CAR FROM) 0)
            END (1- (OR (CAR TO) (ARRAY-DIMENSION ARRAY 0))))
      (DO ((I 1 (1+ I))
           DIM)
          ((= I NDIMS))
        (SETQ START (+ (* START (SETQ DIM (ARRAY-DIMENSION ARRAY I)))
                       (OR (NTH I FROM) 0))
              END (+ (* END DIM)
                     (1- (OR (NTH I TO) DIM)))))
      (INCF END)                        ;Convert from inclusive upper bound to exclusive
      (SETQ SIZE (- END START))
      (DO ((P))
          ((ZEROP (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0)))
        (SETQ NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))
        (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE
                                      ARRAY
                                      (+ NDIMS (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG
                                                              ARRAY 0))))
        (IF (ARRAY-INDEXED-P ARRAY)             ;Index offset
            (INCF START (%P-CONTENTS-OFFSET P 2)))
        (SETQ ARRAY (%P-CONTENTS-OFFSET P 0))
        (UNLESS (ARRAYP ARRAY)
          (RETURN-FROM DONE NIL)))
      (SETQ ELTS-PER-Q (CDR (ASSOC TYPE ARRAY-ELEMENTS-PER-Q)))
      (SETQ START (+ (IF (PLUSP ELTS-PER-Q)
                         (FLOOR START ELTS-PER-Q)
                         (* START (MINUS ELTS-PER-Q)))
                     (%POINTER-PLUS ARRAY
                                    (+ NDIMS
                                       (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))))
            SIZE (IF (PLUSP ELTS-PER-Q)
                     (CEILING SIZE ELTS-PER-Q)
                     (* SIZE (MINUS ELTS-PER-Q))))
      (VALUES ARRAY START SIZE))))

(DEFUN PAGE-IN-ARRAY  (ARRAY &OPTIONAL FROM TO &AUX SIZE)
  "Swap in all or part of ARRAY in one disk operation.
FROM and TO are lists of subscripts, or NIL."
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
         ;; Have starting word and number of words.  Page dem words in.
         (PAGE-IN-WORDS FROM SIZE))))

(DEFUN PAGE-OUT-ARRAY (ARRAY &OPTIONAL FROM TO &AUX SIZE)
  "Put all or part of ARRAY high on the list for being swapped out.
FROM and TO are lists of subscripts, or NIL."
  (WITHOUT-INTERRUPTS
    (MULTIPLE-VALUE (ARRAY FROM SIZE)
      (PAGE-ARRAY-CALCULATE-BOUNDS ARRAY FROM TO))
    (AND ARRAY
         ;; Have starting word and number of words.  Page dem words out.
         (PAGE-OUT-WORDS FROM SIZE))))

;;; Just mark pages as good to swap out; don't actually write them.
(DEFUN PAGE-OUT-WORDS (ADDRESS NWDS &OPTIONAL ONLY-IF-UNMODIFIED &AUX STS)
  ONLY-IF-UNMODIFIED
  (WITHOUT-INTERRUPTS
    (SETQ ADDRESS (%POINTER ADDRESS))
    ;; This DO is over the whole frob
    (DO ((ADDR (LOGAND (- PAGE-SIZE) ADDRESS) (%MAKE-POINTER-OFFSET DTP-FIX ADDR PAGE-SIZE))
         (N (+ NWDS (LOGAND (1- PAGE-SIZE) ADDRESS)) (- N PAGE-SIZE)))
        ((NOT (PLUSP N)))
      (OR (NULL (SETQ STS (%PAGE-STATUS ADDR))) ;Swapped out
          ( (LDB %%PHT1-SWAP-STATUS-CODE STS)
             %PHT-SWAP-STATUS-WIRED)            ;Wired
          (%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-FLUSHABLE
                               (LDB %%REGION-MAP-BITS
                                    (%REGION-BITS (%REGION-NUMBER ADDRESS))))))))

(DEFUN REALLY-PAGE-OUT-PAGE (ADDRESS &AUX CCWP PS PHYS-ADR)
  "Write it on the disk, changing in-core page table status to RWF, etc."
  (select-processor
    ((:lambda :cadr)
     (WITHOUT-INTERRUPTS
       (SETQ ADDRESS (%POINTER ADDRESS))
       (UNWIND-PROTECT
           (PROG ()
                 (WIRE-PAGE-RQB)
                 (SETQ CCWP %DISK-RQ-CCW-LIST)
                 ;; We collect some page frames to put them in, remembering the
                 ;; PFNs as CCWs.
                 (COND ((OR (NULL (SETQ PS (%PAGE-STATUS ADDRESS)))
                            (= 0 (LDB %%PHT1-MODIFIED-BIT PS))
                            (NULL (SETQ PHYS-ADR (%PHYSICAL-ADDRESS ADDRESS))))
                        (RETURN NIL))
                       (T (LET ((PFN (LSH PHYS-ADR -8)))
                            (ASET (1+ (LSH PFN 8)) PAGE-RQB CCWP)
                            (ASET (LSH PFN -8) PAGE-RQB (1+ CCWP)))
                          (SETQ CCWP (+ 2 CCWP))
                          (ASET (LOGAND (AREF PAGE-RQB (- CCWP 2)) -2)  ;Turn off chain bit
                                PAGE-RQB (- CCWP 2))
                          (DISK-WRITE-WIRED PAGE-RQB 0 (+ (LSH ADDRESS -8) PAGE-OFFSET))
                          (%CHANGE-PAGE-STATUS ADDRESS (+ 1_23. %PHT-SWAP-STATUS-FLUSHABLE)
                                               (LDB %%REGION-MAP-BITS
                                                    (%REGION-BITS (%REGION-NUMBER ADDRESS))))
                          (RETURN T)
                          )))
         ;; UNWIND-PROTECT forms
         (UNWIRE-PAGE-RQB)
         )))
    (:explorer nil)))

;moved to end for first installation
;(defun page-in-words (address nwds)
;  (select-processor
;    ((:cadr :lambda) (page-in-words-old address nwds))
;    (:explorer (page-in-words-new address nwds))))

(DEFUN PAGE-IN-WORDS-old (ADDRESS NWDS &AUX (CCWX 0) CCWP BASE-ADDR)
  (lambda-or-cadr-only)
  (WITHOUT-INTERRUPTS
    (SETQ ADDRESS (%POINTER ADDRESS))
    (UNWIND-PROTECT
        (PROGN (WIRE-PAGE-RQB)
               ;; This DO is over the whole frob
               (DO ((ADDR (LOGAND (- PAGE-SIZE) ADDRESS)
                          (%MAKE-POINTER-OFFSET DTP-FIX ADDR PAGE-SIZE))
                    (N (+ NWDS (LOGAND (1- PAGE-SIZE) ADDRESS)) (- N PAGE-SIZE)))
                   ((NOT (PLUSP N)))
                 (SETQ CCWX 0
                       CCWP %DISK-RQ-CCW-LIST
                       BASE-ADDR ADDR)
                 ;; This DO is over pages to go in a single I/O operation.
                 ;; We collect some page frames to put them in, remembering the
                 ;; PFNs as CCWs.
                 (DO-FOREVER
                   (OR (EQ (%PAGE-STATUS ADDR) NIL) (RETURN NIL))
                   (LET* ((fast-cache-mode-p (ldb-test %%processor-switch-fast-cache-mode
                                                       (%processor-switches nil)))
                          (PFN (if fast-cache-mode-p
                                   (%FINDCORE-HEXADEC (LDB (BYTE 4 8) ADDR))
                                 (%findcore))))
                     (SETF (AREF PAGE-RQB CCWP) (1+ (LSH PFN 8)))
                     (SETF (AREF PAGE-RQB (1+ CCWP)) (LSH PFN -8)))
                   (INCF CCWX 1)
                   (INCF CCWP 2)
                   (UNLESS (< CCWX PAGE-RQB-SIZE)
                     (RETURN NIL))
                   (SETQ ADDR (%POINTER-PLUS ADDR PAGE-SIZE))
                   (DECF N PAGE-SIZE)
                   (UNLESS (PLUSP N)
                     (RETURN NIL)))
                 (WHEN (PLUSP CCWX)             ;We have something to do, run the I/O op
                   ;; Turn off chain bit
                   (SETF (AREF PAGE-RQB (- CCWP 2)) (LOGAND (AREF PAGE-RQB (- CCWP 2)) -2))
                   (DISK-READ-WIRED PAGE-RQB 0 (+ (LSH BASE-ADDR -8) PAGE-OFFSET))
                   ;; Make these pages in
                   (DO ((I 0 (1+ I))
                        (CCWP %DISK-RQ-CCW-LIST (+ 2 CCWP))
                        (VPN (LSH BASE-ADDR -8) (1+ VPN))
                        (PFN))
                       ((= I CCWX))
                     (SETQ PFN (DPB (AREF PAGE-RQB (1+ CCWP))
                                    (BYTE 8. 8.)
                                    (LDB (BYTE 8. 8.) (AREF PAGE-RQB CCWP))))
                     (UNLESS (%PAGE-IN PFN VPN)
                       ;; Page already got in somehow, free up the PFN
                       (%CREATE-PHYSICAL-PAGE (LSH PFN 8))))
                   (SETQ CCWX 0))))
      ;; UNWIND-PROTECT forms
      (UNWIRE-PAGE-RQB)
;I guess it's better to lose some physical memory than to get two pages
;swapped into the same address, in the event that we bomb out.
;     (DO ((CCWP %DISK-RQ-CCW-LIST (+ CCWP 2))
;          (N CCWX (1- N)))
;         ((ZEROP N))
;       (%CREATE-PHYSICAL-PAGE (DPB (AREF PAGE-RQB (1+ CCWP))
;                                   #o2006
;                                   (AREF PAGE-RQB CCWP))))
      )))

(defun max-pages-per-user-disk-op ()
  (select-processor
    ((:lambda :cadr) page-rqb-size)
    (:explorer (// (- 256. ;the whole page PAGE-RQB is on
                      1    ;size of PAGE-RQB's header
                      8    ;number of words in NUPI control block
                      )
                   2       ;divide by 2 since 2 words per table entry
                   ))))

(defvar page-in-command-block nil)
(defvar page-in-block-map nil)

(defun get-page-in-arrays ()
  (let ((command-block page-in-command-block)
        (map page-in-block-map))
    (if (or (null command-block)
            (null (%store-conditional (locf page-in-command-block)
                                      command-block
                                      nil)))
        (setq command-block (make-array (* 2 (1- page-size)) :type art-16b :area disk-buffer-area)))
    (if (or (null map)
            (null (%store-conditional (locf page-in-block-map)
                                      map
                                      nil)))
        (setq map (make-array (list (max-pages-per-user-disk-op) 2))))
    (values command-block map)))

(defun free-page-in-arrays (command-block map)
  (setq page-in-command-block command-block)
  (setq page-in-block-map map))

(defun object-page-status (obj)
  (do ((adr (%find-structure-leader obj) (%pointer-plus adr page-size))
       (end (%pointer-plus (%find-structure-leader obj)
                           (%structure-total-size (%find-structure-leader obj)))))
      ((> (%pointer-difference adr end) 0))
    (if (%page-status adr)
        (tyo #/+)
      (tyo #/-))))

(defun new-page-in-structure (obj)
  "Swap in the structure STRUCTURE in one disk operation."
  (setq obj (follow-structure-forwarding obj))
  (page-in-words-new (%find-structure-leader obj)
                     (%structure-total-size obj)))

(defun page-in-words-new (from-address n-words)
  (let* ((address (dpb 0 (byte 8 0) (%pointer from-address)))
         (n-pages (ceiling (%pointer-plus (%pointer-difference from-address address)
                                          n-words)
                           page-size))
         command-block map
         (max-pages-per-user-disk-op (max-pages-per-user-disk-op))
         pages-this-time first-page-of-this-chunk
         )

    (multiple-value (command-block map) (get-page-in-arrays))

    (do ()
        ((zerop n-pages))
      ;;find the next page above ADDRESS that is swapped out
      (do ()
          ((or (zerop n-pages)
               (null (%page-status address))))
        (setq address (%pointer-plus address page-size))
        (setq n-pages (1- n-pages)))

      ;;now find how many pages in a row need to be swapped in
      (array-initialize command-block 0)
      (setq pages-this-time 0)
      (setq first-page-of-this-chunk (ldb (byte 17. 8) address))
      (do ((scatter-index 16. (+ scatter-index 4))
           (page-of-this-transfer 0 (1+ page-of-this-transfer))
           pfn
           )
          ((or (zerop n-pages)
               (%page-status address)
               (>= pages-this-time max-pages-per-user-disk-op)
               )
           )
        (LET ((fast-cache-mode-p (ldb-test %%processor-switch-fast-cache-mode
                                           (%processor-switches nil))))
          (setq PFN (if fast-cache-mode-p
                        (%FINDCORE-HEXADEC (LDB (BYTE 4 8) address))
                      (%findcore))))

        (aset address map page-of-this-transfer 0)
        (aset pfn map page-of-this-transfer 1)
        (let ((phys-adr (ash (%nubus-physical-address pfn) 10.)))
          (aset (ldb (byte 16. 0) phys-adr) command-block scatter-index)
          (aset (ldb (byte 16. 16.) phys-adr) command-block (+ 1 scatter-index))
          (aset 1024. command-block (+ 2 scatter-index))
          (aset 0 command-block (+ 3 scatter-index)))
        (setq address (%pointer-plus address page-size))
        (decf n-pages)
        (incf pages-this-time))

      (if (zerop pages-this-time) (return nil))

      (aset (convert-logical-unit-to-physical-unit page-unit) command-block %nupi-disk-unit)
      (aset (dpb #x12 (byte 8 8) #o100) command-block %nupi-disk-command)
      (let ((scatter-phys-adr (vadr-to-nubus-phys (%pointer-plus command-block
                                                                 (+ (array-data-offset command-block)
                                                                    8.)))))
        (aset (ldb (byte 16. 0) scatter-phys-adr) command-block %nupi-disk-ccw-list-pointer-lo)
        (aset (ldb (byte 16. 16.) scatter-phys-adr) command-block %nupi-disk-ccw-list-pointer-hi))
      (let ((byte-count (* pages-this-time 1024.)))
        (aset (ldb (byte 16. 0) byte-count) command-block %nupi-disk-transfer-count-lo)
        (aset (ldb (byte 16. 16.) byte-count) command-block %nupi-disk-transfer-count-hi))
      (let ((disk-address (+ page-offset first-page-of-this-chunk)))
        (aset (ldb (byte 16. 0) disk-address) command-block %nupi-disk-logical-block-lo)
        (aset (ldb (byte 16. 16.) disk-address) command-block %nupi-disk-logical-block-hi))

      (%io-cmd-run command-block)

      (do ()
          ((ldb-test (byte 1 14.) (aref command-block %nupi-disk-status-hi))))
      (cond ((ldb-test (byte 1 13.) (aref command-block %nupi-disk-status-hi))
             (ferror "disk error")))

      (dotimes (page-number pages-this-time)
        (if (null (%page-in (aref map page-number 1)
                            (ldb (byte 17. 8) (aref map page-number 0))))
            (%create-physical-page (lsh (aref map page-number 1) 8.))))

      )

    (free-page-in-arrays command-block map)
    ))


;;;used by the lambda to find the machine name (since there is nothing like the
;;; chaos address set on the IO board)  Must be in this file since is needed when
;;; real chaos routines are initialized after MINI has done its thing.

(defun get-pack-name (&optional unit &aux pack-names)
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (unit unit "reading label")
    (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
      (read-disk-label rqb unit)
      (let ((whole-pack-name (ecase (get-disk-fixnum rqb 1)
                               ;;the fourth arg, t, here means not to stop at the first 0 byte
                               (1 (get-disk-string rqb #o20 32. t))
                               (2 (get-disk-string rqb 12. 16.)))))
        (do  ((i 0 (1+ i))
              (tok-begin nil))
             ((= i (string-length whole-pack-name))
              (when tok-begin
                (push (substring whole-pack-name tok-begin) pack-names)))
          (let ((c (logand 177 (aref whole-pack-name i))))
            (cond ((and (> c (char-int #\space) )
                        (< c 177)
                        (not (char-equal c #/:)))
                    (if (null tok-begin)
                        (setq tok-begin i)))
                   ((null tok-begin))
                   (t
                    (push (substring whole-pack-name tok-begin i) pack-names)
                    (setq tok-begin nil))))))))
  (values-list (reverse pack-names)))

(defun set-pack-name (pack-name &optional unit)
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (unit unit "writing label")
    (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
      (read-disk-label rqb unit)
      (ecase (get-disk-fixnum rqb 1)
        (1 (put-disk-string rqb pack-name #o20 32.))
        (2 (put-disk-string rqb pack-name 12. 16.)))
      (write-disk-label rqb unit)))
  pack-name)

;This is a test function.
(DEFUN READ-ALL-BLOCKS (&OPTIONAL (UNIT 0) &AUX BUF BLOCKS-PER-TRACK N-CYLS N-HEADS)
  (lambda-or-cadr-only)
  (WITH-DECODED-DISK-UNIT (UNIT UNIT "reading all")
    (WITH-DISK-RQB (RQB)
      (DISK-READ RQB UNIT 0)                    ;Get label
      (SETQ BUF (RQB-BUFFER RQB))
      (SETQ N-CYLS (AREF BUF 4)
            N-HEADS (AREF BUF 6)
            BLOCKS-PER-TRACK (AREF BUF 8)))
    (WITH-DISK-RQB (RQB BLOCKS-PER-TRACK)
      (DOTIMES (CYL N-CYLS)
        (DOTIMES (HEAD N-HEADS)
          (DISK-READ RQB UNIT (* (+ (* CYL N-HEADS) HEAD)
                                 BLOCKS-PER-TRACK)))
        (FORMAT T "~D " CYL)))))


(DEFUN INSPECT-BLOCK (BLOCK-NO &OPTIONAL (UNIT 0) &AUX BUF)
  (lambda-or-cadr-only)
  (WITH-DECODED-DISK-UNIT (UNIT UNIT "reading block")
    (WITH-DISK-RQB (RQB)
      (DISK-READ RQB UNIT BLOCK-NO)
      (SETQ BUF (RQB-8-BIT-BUFFER RQB))
      (LET ((FROBS-PER-LINE #o20))
        (DOTIMES (LINE (TRUNCATE #o2000 FROBS-PER-LINE))
          (TERPRI)
          (DOTIMES (CHAR FROBS-PER-LINE)
            (PRIN1-THEN-SPACE (AREF BUF (+ (* LINE FROBS-PER-LINE)
                                           CHAR))))
          (MULTIPLE-VALUE-BIND (X Y)
              (SEND *TERMINAL-IO* ':READ-CURSORPOS)
            (SEND *TERMINAL-IO* ':SET-CURSORPOS (MAX X 500.) Y))
          (DOTIMES (CHAR FROBS-PER-LINE)
            (SEND *TERMINAL-IO*
                  :TYO (AREF BUF (+ (* LINE FROBS-PER-LINE) CHAR)))))))))


;*** get multiple tape copy-disk-partition from release-1
;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:13
; 7/17/86 rpp
(defun string-block-absolute (block)
  (using-resource (rqb rqb 1 4)
    (disk-read rqb 0 block)
    (rqb-8-bit-buffer rqb)))

;; Copied from LAD: RELEASE-3.IO; DISK.LISP#406 on 2-Oct-86 17:27:13
; 7/17/86 rpp
(defun string-in-block (partition offset string-start string-end)
  (let ((part-start (find-disk-partition partition)))
    (if part-start
        (substring (string-block-absolute (+ part-start offset))
                   string-start string-end)
      "")))


(defun page-in-words (address nwds)
  (select-processor
    ((:cadr :lambda) (page-in-words-old address nwds))
    (:explorer (page-in-words-new address nwds))))

;;; Swap recommendations.

(defun set-swap-recommendations-of-area (area swap-recommendations)
  "Set the number of pages to be swapped in at once in AREA."
  (check-type area area-number)
  (setf (%area-swap-recommendations area) swap-recommendations)
  (for-every-region-in-area (region area)
    (setf (%region-swap-recommendations region) swap-recommendations)))

(defun check-swap-recommendations-of-area (area)
  (check-type area area-number)
  (for-every-region-in-area (region area)
    (unless (= (%region-swap-recommendations region) (%area-swap-recommendations area))
      (ferror "Region ~S has swap quantum ~S, but its area has swap quantum ~S."
              region
              (%region-swap-recommendations region)
              (%area-swap-recommendations area)))))

(defun set-all-swap-recommendations (n &optional ignore)
  "Set all areas to swap in N+1 pages at a time."
  (dolist (name-of-area (current-area-list))
    (set-swap-recommendations-of-area (symbol-value name-of-area) n)))

(defun default-swap-recommendations ()
  "Set all area swap recommendations to the default value for this machine's memory size."
  (declare (special tv::sheet-area zwei::zwei-line-area))
  ;; A perfunctory benchmark using JRM's microcode analyzer indicated that swap
  ;; recommendations of 3 and an aging depth of 1 were significantly better than other
  ;; combinations (increasing the swap recommendations to 4 slowed it down significantly).
  (set-all-swap-recommendations 3)
  (write-meter 'sys:%aging-depth 1)
  ;; I'm just guessing that these areas could benefit from an increased swap quantum.
  (set-swap-recommendations-of-area sys:fasl-table-area 6)
  (when (boundp 'tv::sheet-area)
    (set-swap-recommendations-of-area tv::sheet-area 6))
  (when (boundp 'zwei::zwei-line-area)
    (set-swap-recommendations-of-area zwei::zwei-line-area 6)))

(add-initialization "Set swap recommendations" '(default-swap-recommendations) '(:cold))

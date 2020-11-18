;;; -*- Mode:LISP; Package:METER; Base:8; Readtable:ZL -*-
;;; Metering information analyzer

;;; Note:  To support multiple processor machines, use the function
;;;        (find-metr-partition-name) instead of the string "METR"
;;;        when hacking the disk.

(defun find-metr-partition-name ()
  (select-processor
    (:cadr "METR")
    (:lambda (nth si:*my-proc-number* '("METR" "MET1" "MET2" "MET3")))
    (:explorer (ferror "not implemented"))
    ))


(DEFVAR *BUFFER-ARRAY*)                         ;Buffer for microcode to write out of
(DEFVAR *BUFFER*)                               ;Actual read-out buffer
(DEFVAR *BUFFER-ADDRESS*)                       ;Offset of page in meter buffer
(DEFVAR *DISK-PARTITION-LENGTH*)                ;Length of disk count
(DEFVAR *DISK-PARTITION-START*)                 ;Origin of disk address
(DEFVAR *DISK-RQB*)                             ;Disk request block
(DEFVAR *NEXT-DISK-BLOCK*)                      ;Next disk block to return


;variables used by simple anaylzer (as opposed to hairy one)
(defvar *simple-list-data* nil)
(defvar *simple-hash-table* nil)
(defvar *simple-data-analyzed* nil)  ;If T, just print data, since its unchanged sine
                                     ;  last analyzed.


;;; Maps over the data in buffers
(DEFMACRO DO-OVER-DATA (VAR-LIST END-FORMS &BODY BODY)
  `(MULTIPLE-VALUE-BIND (BUF INDEX)
       (FRAME-SETUP)
     (DO ,VAR-LIST
         ((NULL BUF) . ,END-FORMS)
       ,@BODY
       (MULTIPLE-VALUE-SETQ (BUF INDEX)
         (NEXT-FRAME BUF INDEX)))))

;;; Metering enable functions
(DEFUN BUFFER-RESET ()
  (SETQ *BUFFER-ARRAY* (MAKE-ARRAY (* PAGE-SIZE 4) :ELEMENT-TYPE '(MOD #.(^ 2 16.))))
  (MULTIPLE-VALUE-SETQ (*DISK-PARTITION-START* *DISK-PARTITION-LENGTH*)
    (SI:FIND-DISK-PARTITION (find-metr-partition-name)))
  (IF (NULL *DISK-PARTITION-START*)
      (FERROR "No partition named ~S to use for metering" (find-metr-partition-name)))
  (SETQ *BUFFER-ADDRESS* (%POINTER-PLUS (LOGIOR (%POINTER *BUFFER-ARRAY*)       ;This is in Q's here
                                                (1- PAGE-SIZE))
                                        1))
  (SI:%WIRE-PAGE *BUFFER-ADDRESS*)
  ;***bug displaced-index-offset can be negative, bombing if array exactly on page boundary.
  (SETQ *BUFFER* (MAKE-ARRAY (* PAGE-SIZE 2)
                             :ELEMENT-TYPE '(MOD #.(^ 2 16.))
                             :DISPLACED-TO *BUFFER-ARRAY*
                             :DISPLACED-INDEX-OFFSET (* 2 (- *BUFFER-ADDRESS*
                                                             (%POINTER *BUFFER-ARRAY*) 2))))
  )

(DEFUN RESET ()
  "Reset metering and clear metering information."
  (compiler::%set-meter-enables 0)
  (WITHOUT-INTERRUPTS
    (OR (BOUNDP '*BUFFER-ADDRESS*) (BUFFER-RESET))
    (STOP-GC-PROCESS)
    (SETQ %METER-BUFFER-POINTER *BUFFER-ADDRESS*
          %METER-DISK-COUNT *DISK-PARTITION-LENGTH*
          %METER-DISK-ADDRESS *DISK-PARTITION-START*))
  (setq *simple-data-analyzed* nil))

(DEFVAR *METERED-OBJECTS* NIL
  "List of objects being metered.")

(DEFUN ENABLE (&REST THINGS)
  "Turn on metering of THINGS.  Each THING must be a stack group or specify one.
Processes and windows are allowed ways of specifying a stack group.
/(METER:ENABLE T) turns on metering in all stack groups."
  (DOLIST (THING THINGS)
    (IF (EQ THING T)
        (SETQ %METER-GLOBAL-ENABLE T)
      (SETQ THING (ENABLE-STACK-GROUP THING 1)))
    (PUSHNEW THING *METERED-OBJECTS*)))

(DEFUN DISABLE (&REST THINGS)
  "Turn off metering of THINGS. (METER:DISABLE) turns off all metering.
/(METER:DISABLE T) turns off (METER:ENABLE T)."
  (IF (NULL THINGS)
      (SETQ THINGS *METERED-OBJECTS*))
  (DOLIST (THING THINGS)
    (IF (EQ THING T)
        (SETQ %METER-GLOBAL-ENABLE NIL)
      (SETQ THING (ENABLE-STACK-GROUP THING 0)))
    (UNLESS (MEMQ THING *METERED-OBJECTS*)
      (CERROR "Ignore it" "~S was not metered" THING))
    (SETQ *METERED-OBJECTS* (DELQ THING *METERED-OBJECTS*))))

(defvar *aux-sg-list* nil)
(defvar *aux-sg-count* 0)

(defun get-aux-sg ()
  (without-interrupts
    (let ((sg (pop *aux-sg-list*)))
      (or sg (make-stack-group (format nil "Meter AUX SG ~d" (incf *aux-sg-count*)))))))

(defun free-aux-sg (sg)
  (without-interrupts
    (push sg *aux-sg-list*)))

(defun do-stack-group-switch ()
  (without-interrupts
    (let ((sg (get-aux-sg)))
      (stack-group-preset sg  'stack-group-return nil)
      (funcall sg nil)
      (free-aux-sg sg))))

;;;set some bits in the SG-STATE in a stack group
;;;if the stack group we want to hack is the current one, we do the work
;;;in another stack group
(DEFUN ENABLE-STACK-GROUP (THING OFF-OR-ON)
  (COND ((TYPEP THING 'STACK-GROUP))
        ((AND (TYPEP THING 'INSTANCE)
              (LET ((WO (SEND THING :WHICH-OPERATIONS)))
                (COND ((MEMQ :STACK-GROUP WO)
                       (SETQ THING (SEND THING :STACK-GROUP)))
                      ((MEMQ :PROCESS WO)
                       (SETQ THING (SEND (SEND THING :PROCESS) :STACK-GROUP)))
                      (T NIL)))))
        (T (FERROR "Can't meter ~S" THING)))
  (WITHOUT-INTERRUPTS
    (cond ((EQ THING %CURRENT-STACK-GROUP)
           (SETQ %MODE-FLAGS (%LOGDPB OFF-OR-ON %%M-FLAGS-METER-ENABLE %MODE-FLAGS))
           (let ((sg (get-aux-sg)))
             (stack-group-preset sg #'(lambda (sg off-or-on)
                                        (setf (si:sg-inst-disp sg) off-or-on)
                                        (stack-group-return nil))
                                 thing
                                 off-or-on)
             (funcall sg nil)
             (free-aux-sg sg)))
          (t
           (SETF (SI:SG-FLAGS-METER-ENABLE THING) OFF-OR-ON)
           (setf (si:sg-inst-disp thing) off-or-on))))          ;use debugging main loop return
  THING)

(DEFUN SUSPEND ()
  (SETQ %METER-DISK-COUNT 0))

(DEFVAR *WARNING-GIVEN* NIL)
;;; Mustn't let the GC process run while metering since it changes the
;;; meaning of addresses.
;>>
(DEFUN STOP-GC-PROCESS ()
  (UNLESS (MEMQ 'METERING (SEND GC::*GC-PROCESS* :ARREST-REASONS))
    (GC:GC-OFF)
    (IF (SEND GC::*GC-PROCESS* :ACTIVE-P)
        (FORMAT T "~&Turning off GC process, because metering requires it disabled.
Use ~S to allow GC to proceed, once you are done with metering."
                '(RESUME-GC-PROCESS))
      (UNLESS *WARNING-GIVEN*
        (FORMAT T "~&Note: Metering automatically turns off GC.
Use ~S to un-arrest GC, once you are done with metering."
                '(RESUME-GC-PROCESS))
        (SETQ *WARNING-GIVEN* T)))
    (SEND GC:*GC-PROCESS* :ARREST-REASON 'METERING)))

;>>
(DEFUN RESUME-GC-PROCESS ()
  (setq *warning-given* nil)
  (SEND GC::*GC-PROCESS* :REVOKE-ARREST-REASON 'METERING)
  (gc:gc-on))

;;;; General utilities

(DEFUN TIME-DIFF (OLD-HIGH OLD-LOW NEW-HIGH NEW-LOW)
  (IF (> OLD-LOW NEW-LOW)
      (SETQ NEW-LOW (+ 1_16. NEW-LOW)
            NEW-HIGH (1- NEW-HIGH)))
  (DPB (- NEW-HIGH OLD-HIGH) #o2020 (- NEW-LOW OLD-LOW)))

;;; Frame hacking on read in
(DEFUN FRAME-SETUP ()
  (UNLESS (BOUNDP '*DISK-RQB*)
    (SETQ *DISK-RQB* (SI:GET-DISK-RQB)))
  (SETQ *NEXT-DISK-BLOCK* *DISK-PARTITION-START*)
  (GET-NEXT-DISK-BLOCK))

(DEFUN NEXT-FRAME (BUF INDEX)
  (SETQ INDEX (+ INDEX (* (AREF BUF (1+ INDEX)) 2)))
  (IF (EQ BUF *BUFFER*)
      ;; In last buffer, i.e the one in the memory.
      (IF ( (LOGAND %METER-BUFFER-POINTER (1- PAGE-SIZE))      ;Number of Q's in buffer
             (FLOOR INDEX 2))
          (VALUES NIL)
        (VALUES BUF INDEX))
    ;; In another buffer
    (IF (= INDEX (LENGTH BUF))
        (MULTIPLE-VALUE-SETQ (BUF INDEX)
          (GET-NEXT-DISK-BLOCK)))
    (IF (OR (NULL BUF) ( (AREF BUF (1+ INDEX)) 0))
        (VALUES BUF INDEX)
      ;; Here get a new buffer
      (GET-NEXT-DISK-BLOCK))))

(DEFUN GET-NEXT-DISK-BLOCK ()
  (COND ((< *NEXT-DISK-BLOCK* %METER-DISK-ADDRESS)
         (DISK-READ *DISK-RQB* 0 *NEXT-DISK-BLOCK*)
         (INCF *NEXT-DISK-BLOCK*)
         (VALUES (ARRAY-LEADER *DISK-RQB* %DISK-RQ-LEADER-BUFFER) 0))
        (T (IF (ZEROP (LOGAND %METER-BUFFER-POINTER (1- PAGE-SIZE)))
               (VALUES NIL)
             (VALUES *BUFFER* 0)))))

(DEFUN METER-FIX-SIGNED (BUF INDEX)
  (LET* ((HIGH (AREF BUF (1+ INDEX)))
         (ANS (DPB HIGH #o2010 (LDB #o0020 (AREF BUF INDEX)))))
    (IF (LDB-TEST #o1001 HIGH)
        (LOGIOR ANS MOST-NEGATIVE-FIXNUM)
      ANS)))

(DEFUN METER-FIX-UNSIGNED (BUF INDEX)
  (DPB (AREF BUF (1+ INDEX)) #o2011 (LDB #o0020 (AREF BUF INDEX))))

(DEFUN METER-Q (BUF INDEX)
  (LET ((TEMP (AREF BUF (1+ INDEX))))
    (%MAKE-POINTER (LDB #o1105 TEMP)
                   (DPB TEMP #o2011 (AREF BUF INDEX)))))


(DEFSTRUCT (BASIC-INFO :NAMED-ARRAY-LEADER (:ALTERANT NIL))
  INFO-LENGTH
  BASE-STATE
  NEXT-STATE)

;;;; ANALYZE support
(DEFSTRUCT (STACK-STATE :NAMED-ARRAY (:ALTERANT NIL))
  LOW-REAL-TIME
  HIGH-REAL-TIME
  LOW-DISK-TIME
  HIGH-DISK-TIME
  PAGE-FAULTS
  STACK-GROUP
  CURRENT-FUNCTION
  STACK-DEPTH
  )

(defconst event-names-index (make-array 20))

(DEFRESOURCE STACK-STATE () :CONSTRUCTOR (MAKE-STACK-STATE)
             :free-list-cell (aloc object 1))

(DEFMACRO EVENT-ALIAS (SYM EVENT)
  `(progn
     (putprop ',sym  (symeval ',event) 'event-number)
     (setf (aref event-names-index (symeval ',event)) ',sym)))

;;; events in meter data consist of a 6 word header, possibly followwed by additional data.
;;; wd0 length,,event-code
;;; wd1 usec time
;;; wd2 disk-wait-time
;;; wd3 disk-page-read-count
;;; wd4    current-stack-group
;;; wd5 current function (pdl buffer pointed to by m-ap)
;;; wd6    current depth in regular stack (m-ap)
;;;  followed by addtl info, if any.

(EVENT-ALIAS :PAGE-IN %METER-PAGE-IN-EVENT)
;;; 2 wds additional info.
;;;  wd7  field 3 bits, 28. over has <flags for page trace>, rest has micro-return address
;;;  wd8  has VMA of reference
(EVENT-ALIAS :PAGE-OUT %METER-PAGE-OUT-EVENT)
;;; same data as page in.  VMA will always point to page boundary.
(EVENT-ALIAS :CONS %METER-CONS-EVENT)
;;; not implemented in ucode now.
(EVENT-ALIAS :FUNCTION-ENTRY %METER-FUNCTION-ENTRY-EVENT)
;;; one word, function being entered
(EVENT-ALIAS :FUNCTION-EXIT %METER-FUNCTION-EXIT-EVENT)
;;; one word, function being left.
(EVENT-ALIAS :FUNCTION-UNWIND %METER-FUNCTION-UNWIND-EVENT)
;;; no words
(EVENT-ALIAS :STACK-SWITCH %METER-STACK-GROUP-SWITCH-EVENT)
;;; one word, stack group just leaving.
(event-alias :macro-instruction %meter-macro-instruction-event)
 ;one word, LOCATION-COUNTER relative to M-AP (current-function, from wd5)

;;; Also, meter records can be made via macrocode using %RECORD-EVENT

;;; A meter record is always contained entirely within a page, so, if the desired
;;;  record will not fit within the current page, a 0 header is written and
;;;  the record written starting on the next page.

(DEFSTRUCT (EVENT-TABLE :ARRAY-LEADER :NAMED
                        (:MAKE-ARRAY (:LENGTH (1+ (LENGTH METER-EVENTS))))
                        :CONC-NAME (:ALTERANT NIL))
  EVENTS                                        ;Other random events go here
  INIT-FUNCTION                                 ;Called when analyze happens
  EXIT-FUNCTION                                 ;Called when analysis is done
  )

(DEFMACRO DEFTABLE (TABLE-NAME &OPTIONAL (INIT-FUNCTION ''NO-FUNCTION)
                               (EXIT-FUNCTION ''NO-FUNCTION))
  `(PROGN (SI:RECORD-SOURCE-FILE-NAME ',TABLE-NAME 'EVENT-TABLE)
          (PUTPROP ',TABLE-NAME
                   (MAKE-EVENT-TABLE INIT-FUNCTION ,INIT-FUNCTION
                                     EXIT-FUNCTION ,EXIT-FUNCTION)
                   'EVENT-TABLE)))

(DEFUN NO-FUNCTION (&REST IGNORE) NIL)

(DEFMACRO DEFANALYZE (EVENT-TABLE EVENT &BODY BODY)
  (IF (ATOM BODY)
      `(DEFANALYZE-1 ',EVENT-TABLE ',EVENT ',BODY)
    `(PROGN
       (DEFUN (,EVENT-TABLE ,EVENT) (BUF INDEX INFO STREAM)
         BUF INDEX INFO STREAM                  ;are used
         . ,BODY)
       (DEFANALYZE-1 ',EVENT-TABLE ',EVENT (GET ',EVENT-TABLE ',EVENT)))))

(DEFUN DEFANALYZE-1 (TABLE-NAME EVENT FCTN)
  (LET ((EVENT-NUMBER (GET EVENT 'EVENT-NUMBER))
        (TABLE (GET TABLE-NAME 'EVENT-TABLE))
;       (TABLE (SYMEVAL TABLE-NAME))
        (CELL))
    (UNLESS (NUMBERP EVENT-NUMBER)
      (FERROR "~S undefined EVENT"))
    (IF (< EVENT-NUMBER (ARRAY-DIMENSION TABLE 0))
        (SETF (AREF TABLE EVENT-NUMBER) FCTN)
      (SETQ CELL (ASSQ EVENT (EVENT-TABLE-EVENTS TABLE)))
      (IF CELL
          (SET (CDR CELL) FCTN)
        (PUSH (CONS EVENT-NUMBER FCTN) (EVENT-TABLE-EVENTS TABLE))))))

;;; Analysis driver function
(DEFUN ANALYZE (&REST OPTIONS &AUX RET-INFO INFO (FLUSH-INFO T) CLOSE-STREAM
                (STREAM *STANDARD-OUTPUT*))
  "Analyze the information recorded by metering.
:STREAM specifies a stream to print the analysis on;
alternatively, :FILE specifies a filename to write it to,
or :BUFFER specifies an editor buffer to write it to.
:ANALYZER specifies a kind of analysis.
/(:TREE is the default; :LIST-EVENTS is also useful).
If you specify :RETURN T, the intermediate data structure is returned.
Passing that data structure as the :INFO argument, you can save much time.
You can also keep the intermediate data structure while metering other computations.

Particular analyzers allow additional keyword arguments.
:TREE handles these:
 :FIND-CALLERS - arg is function spec or list of them;
   say where each of those function was called from and how often.
   This is instead of the ordinary output.
 :STACK-GROUP - arg is stack group or list of them;
   analyze only activities in those stack group.
 :SORT-FUNCTION - passed to SORT-GROUPED-ARRAY-GROUP-KEY.
   Try MAX-CALLS, MAX-RUN-TIME, MAX-REAL-TIME, MAX-PAGE-FAULTS,
   or MAX-RUN-TIME-PER-CALL (all in METER:).
 :SUMMARIZE - arg is function spec of list of them;
   mention only those functions in the output.
 :INCLUSIVE - non-NIL means include time spent within subroutines
   in the times for each function.

Note: to execute something and meter it, use METER:TEST or METER:RUN."
  (DECLARE (ARGLIST &KEY ANALYZER STREAM FILE BUFFER RETURN INFO
                         FIND-CALLERS STACK-GROUP SORT-FUNCTION SUMMARIZE INCLUSIVE
                    &ALLOW-OTHER-KEYS))
  (MULTIPLE-VALUE-SETQ (*DISK-PARTITION-START* *DISK-PARTITION-LENGTH*)
    (SI:FIND-DISK-PARTITION (find-metr-partition-name)))
  (IF (NULL *DISK-PARTITION-START*)
      (FERROR "No partition named ~S to use for metering" (find-metr-partition-name)))
  (UNWIND-PROTECT
    (LET ((EVENT-TABLE (GET ':TREE 'EVENT-TABLE))
          (OPT-LIST))
      (DO ((L OPTIONS (CDDR L)))
          ((NULL L))
        (CASE (FIRST L)
          (:STREAM (SETQ STREAM (CADR L)))
          (:FILE (SETQ CLOSE-STREAM T
                       STREAM (OPEN (CADR L) :DIRECTION :OUTPUT :CHARACTERS T)))
          (:BUFFER (SETQ STREAM (ZWEI:INTERVAL-STREAM (ZWEI::FIND-BUFFER-NAMED (CADR L) T))))
          (:INFO (SETQ INFO (CADR L) FLUSH-INFO NIL))
          (:ANALYZER (SETQ EVENT-TABLE (IF (TYPEP (CADR L) 'EVENT-TABLE)
                                           (CADR L)
                                         (GET (CADR L) 'EVENT-TABLE))))
          (:RETURN (SETQ RET-INFO (CADR L) FLUSH-INFO NIL))
          (OTHERWISE (PUTPROP (LOCF OPT-LIST) (CADR L) (CAR L)))))
      (IF (ZEROP %METER-DISK-COUNT)
          (FORMAT STREAM "~&Note: ~Either you have not done ~S, or else the disk metering partition has overflowed!
  If that is the case, then some information has been lost,
  and more recent metering events will not have been recorded.~~%"
                  'RESET))
      (IF (NULL INFO)
          (DO-OVER-DATA ((MAX-INDEX (ARRAY-DIMENSION EVENT-TABLE 0))
                         (FCTN (SETQ INFO (FUNCALL (EVENT-TABLE-INIT-FUNCTION EVENT-TABLE)
                                                   OPT-LIST STREAM BUF INDEX)))
                         (EVENT))
                        ()
            (SETQ EVENT (COPY-FRAME-TO-STATE BUF INDEX (NEXT-STATE INFO)))
            (SETQ FCTN (COND ((NULL EVENT) NIL)
                             ((< EVENT MAX-INDEX) (AREF EVENT-TABLE EVENT))
                             (T (CDR (ASSQ EVENT (EVENT-TABLE-EVENTS EVENT-TABLE))))))
            (IF FCTN
                (FUNCALL FCTN BUF INDEX INFO STREAM))))
      (FUNCALL (EVENT-TABLE-EXIT-FUNCTION EVENT-TABLE) INFO STREAM OPT-LIST)
      (AND RET-INFO INFO))
    (AND FLUSH-INFO INFO (ANALYZE-FREE INFO))
    (AND CLOSE-STREAM STREAM (CLOSE STREAM))))

(defun vector-top (array)
  (aref array (1- (fill-pointer array))))


;-- alternative simple analyzer below here

(defun simple-sort-then-print (options sort-comparision &aux (stream *standard-output*) close-stream)
  ;(declare (arglist &key stream file buffer))
  (DO ((L OPTIONS (CDDR L)))
      ((NULL L))
    (CASE (FIRST L)
      (:STREAM (SETQ STREAM (CADR L)))
      (:FILE (SETQ CLOSE-STREAM T
                   STREAM (OPEN (CADR L) :DIRECTION :OUTPUT :CHARACTERS T)))
      (:BUFFER (SETQ STREAM (ZWEI::INTERVAL-STREAM (ZWEI::FIND-BUFFER-NAMED (CADR L) T))))))
  (setq *simple-list-data*
        (sort *simple-list-data* sort-comparision))
  (simple-print-data stream *simple-list-data*)
  (if close-stream (close stream))
  t)

(defun simple-inline-runtime-comparison (a b)
  ( (- (fourth a) (sixth a))
     (- (fourth b) (sixth b))))  ;sort highest first.

(defun simple-inclusive-runtime-comparison (a b)
  ( (- (fifth a) (seventh a))
     (- (fifth b) (seventh b))))

(defun simple-max-calls-comparision (a b)
  ( (third a) (third b)))

(defun simple-max-page-faults-comparision (a b)
  ( (sixth (cddr a)) (sixth (cddr b))))

(defun simple-max-inclusive-page-faults-comparision (a b)
  ( (seventh (cddr a)) (seventh (cddr b))))

(defun simple-max-runtime-per-call-comparision (a b)
  ( (// (- (fourth a) (sixth a)) (third a))
     (// (- (fourth b) (sixth b)) (third b))))

(defun simple-max-inclusive-runtime-per-call-comparison (a b)
  ( (// (- (fifth a) (seventh a)) (third a))
     (// (- (fifth b) (seventh b)) (third b))))

(defun simple-print-data (stream data)
  (Format stream
"~%Functions            # calls   inline-RT  inclusive-RT in-DW  ic-DW in-diskops ic-diskops")
  (dolist (l data)
    (format stream "~%~48A ~5D   ~8D ~8D   ~D ~D   ~D ~D"
            (first l) (third l)
            (- (fourth l) (sixth l))
            (- (fifth l) (seventh l))
            (sixth l) (seventh l)
            (sixth (cddr l)) (seventh (cddr l)))))

;meter-hash-table function recursion-depth number-calls
;                      inline-runtime invoked-runtime
;                      inline-disk-wait-time invoked-disk-wait-time
;                      inline-pgf invoked-pgf
;meter-stack-image <function> <time-originally-entered-low,hish>
;               <disk-wait-time-low,high> <disk-ops-originally-entered>
;global-vars current-function last-usec-time last-disk-time disk-ops

(defun simple-analyze (&rest options)
  (if (null *simple-data-analyzed*)
   (let ((hash-table (make-hash-table :test #'eq
                                     :size (+ 1000. (- %meter-disk-address
                                                       *disk-partition-start*))
                                     :number-of-values 8.))
        (pdl-array (make-array #o4000 :fill-pointer 0))
        (disk-ops 0)       ;ascending count for entire run
        (function-to-charge)   ;normally same as function on top of stack.
        (ftc-time-charge)       ;inline time so far.
        (ftc-disk-time-charge)
        (ftc-disk-ops-charge)
        (ftc-base-runtime-low)     ;used to compute inline time for this "visit"
        (ftc-base-runtime-high)
        (ftc-base-disk-wait-time-low)
        (ftc-base-disk-wait-time-high)
        (ftc-base-disk-ops) ;  ""
        (unwind-events 0)
        (unwind-tick nil)
        (mysterious-returns 0)
        (mysterious-return-tick nil)
        (mrf nil)
        (tick 0)
        (stack-group-switches 0)
        (ignored-stack-group-switches)
        (runtime-in-other-stack-groups 0)
        (disk-wait-in-other-stack-groups 0)
        (metered-stack-group nil)
        (stack-group-out-flag nil)
        )
     (format t "~%Analyzing data ... ")
    (DO-OVER-DATA ((event-number)
                   (arg-function)) ()
      (setq event-number (aref buf index))
      (let ((usec-time-low (AREF BUF (+ INDEX 2)))
            (usec-time-high (AREF BUF (+ INDEX 3)))
            (disk-wait-time-low (AREF BUF (+ INDEX 4)))
            (disk-wait-time-high (AREF BUF (+ INDEX 5)))
         ;   (disk-page-read-count (METER-FIX-UNSIGNED BUF (+ INDEX 6)))
            (current-function (METER-Q BUF (+ INDEX 10.)))
         ;   (current-stack-depth (METER-FIX-UNSIGNED BUF (+ INDEX 12.)))
            )
        (select event-number
          (%meter-function-entry-event
           (if (null metered-stack-group)
               (setq metered-stack-group (meter-q buf (+ index 8.))))  ;current-sg
           (setq tick (1+ tick))
           (setq arg-function (meter-q buf (+ index 14.)))
                ;credit time
           (multiple-value-bind (r-depth foundp hlist)
               (gethash arg-function hash-table)        ;find-hash
             (unless foundp
               (puthash arg-function 0 hash-table 0 0 0 0 0 0 0) ;no-entry -> make-initial-entry
               (multiple-value-setq (r-depth foundp hlist)
                 (gethash arg-function hash-table)))
             (setf (second hlist) (1+ (second hlist)))  ;recursion count
             (setf (third hlist) (1+ (third hlist))))   ;call count
           (when function-to-charge
             (setq ftc-time-charge              ;update inline times
                   (+ ftc-time-charge
                      (time-diff ftc-base-runtime-high ftc-base-runtime-low
                                 usec-time-high usec-time-low)))
             (setq ftc-disk-time-charge
                   (+ ftc-disk-time-charge
                      (time-diff ftc-base-disk-wait-time-high ftc-base-disk-wait-time-low
                                 disk-wait-time-high disk-wait-time-low)))
             (setq ftc-disk-ops-charge
                   (+ ftc-disk-ops-charge
                      (- disk-ops ftc-base-disk-ops)))
             (multiple-value-bind (nil foundp hlist) (gethash function-to-charge hash-table)
               (cond (foundp            ;better be.
                      (incf (fourth hlist) ftc-time-charge)
                      (incf (sixth hlist) ftc-disk-time-charge)
                      (incf (sixth (cddr hlist)) ftc-disk-ops-charge)))))

           (vector-push-extend usec-time-low pdl-array)         ;make-pdl-record
           (vector-push-extend usec-time-high pdl-array)
           (vector-push-extend disk-wait-time-low pdl-array)
           (vector-push-extend disk-wait-time-high pdl-array)
           (vector-push-extend disk-ops pdl-array)
           (vector-push-extend arg-function pdl-array)  ;must be last.

           (setq function-to-charge arg-function                ;update function-to-charge
                 ftc-time-charge 0
                 ftc-disk-time-charge 0
                 ftc-disk-ops-charge 0
                 ftc-base-disk-ops disk-ops
                 ftc-base-runtime-low usec-time-low
                 ftc-base-runtime-high usec-time-high
                 ftc-base-disk-wait-time-low disk-wait-time-low
                 ftc-base-disk-wait-time-high disk-wait-time-high)
           )
          (%meter-function-exit-event  ;current-function is one returnning to. arg is one left.
           (setq tick (1+ tick))
          (prog nil top
           (setq mrf nil)
           (setq arg-function (meter-q buf (+ index 14.)))              ;credit time
  ;all exits metered, but only dtp-fef-pointer entries are, currently.
           (cond ((not (= (%data-type arg-function)
                          dtp-fef-pointer))
                  (go flush-this-event))
                 ((and function-to-charge
                       (not (eq arg-function function-to-charge))) ;compare-pdl-record
                  (cond ((eq arg-function (aref pdl-array (- (fill-pointer pdl-array)
                                                             7)))
                         (setq mrf t
                               mysterious-returns (1+ mysterious-returns)
                               mysterious-return-tick tick)
                         (go pop-one))
                        (t
                         (ferror "Function left ~s, apparently not one running ~s."
                                 arg-function function-to-charge)
                         (go flush-this-event)))))
   pop-one (let ((p-function (vector-pop pdl-array))
                 (p-disk-ops (vector-pop pdl-array))    ;pop-pdl-record
                 (p-dw-h (vector-pop pdl-array))
                 (p-dw-l (vector-pop pdl-array))
                 (p-us-h (vector-pop pdl-array))
                 (p-us-l (vector-pop pdl-array)))
             (cond ((and function-to-charge
                         (not (eq p-function function-to-charge))
                         (null mrf))
                    (ferror "popped function not function-to-charge ~s, ~s"
                            p-function function-to-charge)))
             (multiple-value-bind (r-depth foundp hlist)
                 (gethash p-function hash-table)        ;find-hash
               (cond ((null foundp)
                      (ferror "hash entry for ~s not found" p-function)))
               (setf (second hlist) (1- (second hlist)))        ;recursion count
               (setq ftc-time-charge            ;update inline times
                     (+ ftc-time-charge
                        (time-diff ftc-base-runtime-high ftc-base-runtime-low
                                   usec-time-high usec-time-low)))
               (setq ftc-disk-time-charge
                     (+ ftc-disk-time-charge
                        (time-diff ftc-base-disk-wait-time-high ftc-base-disk-wait-time-low
                                   disk-wait-time-high disk-wait-time-low)))
               (setq ftc-disk-ops-charge
                     (+ ftc-disk-ops-charge
                        (- disk-ops ftc-base-disk-ops)))

               (incf (fourth hlist) ftc-time-charge)
               (incf (sixth hlist) ftc-disk-time-charge)
               (incf (sixth (cddr hlist)) ftc-disk-ops-charge)

               (cond ((= 1 r-depth)             ;r-depth was before decrement.
                      (incf (fifth hlist)
                            (time-diff p-us-h p-us-l    ;recursion-> 0, update invoked times
                                       usec-time-high usec-time-low))
                      (incf (seventh hlist)
                            (time-diff p-dw-h p-dw-l
                                       disk-wait-time-high disk-wait-time-low))
                      (incf (seventh (cddr hlist)) (- disk-ops p-disk-ops))))
               (setq function-to-charge
                     (if (zerop (fill-pointer pdl-array)) nil
                       (vector-top pdl-array))  ;update function-to-charge
                     ftc-time-charge 0
                     ftc-disk-time-charge 0
                     ftc-disk-ops-charge 0
                     ftc-base-disk-ops disk-ops
                     ftc-base-runtime-low usec-time-low
                     ftc-base-runtime-high usec-time-high
                     ftc-base-disk-wait-time-low disk-wait-time-low
                     ftc-base-disk-wait-time-high disk-wait-time-high)
               (cond (mrf (go top)))
               (cond ((and function-to-charge
                           (not (eq function-to-charge current-function)))
                      (break "function-to-charge ~s current-function ~s"
                             function-to-charge current-function)))
               ))
             flush-this-event ))
          (%meter-page-in-event
           (setq disk-ops (1+ disk-ops)))
          (%meter-page-out-event
           (setq disk-ops (1+ disk-ops)))
          (%meter-function-unwind-event
           (setq unwind-events (1+ unwind-events)
                 unwind-tick tick)
           )
          (%meter-stack-group-switch-event      ;arg is stack-group left
           (cond ((eq metered-stack-group (meter-q buf (+ index 14.)))
                  (cond ((null stack-group-out-flag)
                         ;charge running fctn for time so far.
                         (setq ftc-time-charge          ;update inline times
                               (+ ftc-time-charge
                                  (time-diff ftc-base-runtime-high ftc-base-runtime-low
                                             usec-time-high usec-time-low)))
                         (setq ftc-disk-time-charge
                               (+ ftc-disk-time-charge
                                  (time-diff ftc-base-disk-wait-time-high
                                               ftc-base-disk-wait-time-low
                                             disk-wait-time-high disk-wait-time-low)))
                         (setq ftc-disk-ops-charge
                               (+ ftc-disk-ops-charge
                                  (- disk-ops ftc-base-disk-ops)))      ;switching out
                         ;reset base counters so charges in other stack groups accumulated
                         (setq ;ftc-base-disk-ops disk-ops  ;no use setting this since its
                                        ;not metered..
                               ftc-base-runtime-low usec-time-low
                               ftc-base-runtime-high usec-time-high
                               ftc-base-disk-wait-time-low disk-wait-time-low
                               ftc-base-disk-wait-time-high disk-wait-time-high)
                         (setq stack-group-out-flag t))))
                 ((eq metered-stack-group (meter-q buf (+ index 8)))   ;current-sg
                  (cond ((null stack-group-out-flag)
                         (ferror "stack-switch to metered stack group out of sequence"))
                        (t
                         (setq runtime-in-other-stack-groups
                               (+ runtime-in-other-stack-groups
                                  (time-diff ftc-base-runtime-high ftc-base-runtime-low
                                             usec-time-high usec-time-low))
                               disk-wait-in-other-stack-groups
                               (+ disk-wait-in-other-stack-groups
                                  (time-diff ftc-base-disk-wait-time-high
                                             ftc-base-disk-wait-time-low
                                             disk-wait-time-high disk-wait-time-low)))
                         (setq ftc-base-disk-ops disk-ops  ;establish new base
                               ftc-base-runtime-low usec-time-low
                               ftc-base-runtime-high usec-time-high
                               ftc-base-disk-wait-time-low disk-wait-time-low
                               ftc-base-disk-wait-time-high disk-wait-time-high)
                         (setq stack-group-out-flag nil)
                         (incf stack-group-switches))))

                 (t (incf ignored-stack-group-switches)))))
        ))
    (puthash '*other-stack-groups* ignored-stack-group-switches hash-table
             stack-group-switches runtime-in-other-stack-groups 0
             disk-wait-in-other-stack-groups 0 0 0)
    (format t "~%Mysterious returns ~D, unwinds ~D " mysterious-returns unwind-events)
    (setq *simple-hash-table* hash-table)
    (setq *simple-list-data* (analyze-list-data hash-table))
    (setq *simple-data-analyzed* t)
    ))
  (simple-sort-then-print options (simple-analyze-select-sort options)))

(defun simple-analyze-select-sort (options)
  (let ((sort-comparision 'simple-inline-runtime-comparison))
    (do ((l options (cddr l)))
        ((null l) sort-comparision)
      (case (first l)
        (:sort-function
         (setq sort-comparision
               (selectq (cadr l)
                 ((:max-run-time meter:max-run-time)
                  'simple-inline-runtime-comparison)
                 ((:max-inclusive-run-time meter:max-inclusive-run-time)
                  'simple-inclusive-runtime-comparision)
                 ((:max-calls meter:max-calls)  'simple-max-calls-comparison)
                 ((:max-page-faults meter:max-page-faults)
                  'simple-max-page-faults-comparision)
                 ((:max-inclusive-page-faults meter:max-inclusive-page-faults)
                  'simple-max-inclusive-page-faults-comparison)
                 ((:max-run-time-per-call meter:max-run-time-per-call)
                  'simple-max-run-time-per-call-comparision)
                 ((:max-inclusive-run-time-per-call meter:max-inclusive-run-time-per-call)
                  'simple-max-inclusive-run-time-per-call-comparison)
                 (otherwise
                  (ferror nil "Sort function ~s not implemented" (cadr l))))))))))

(defun analyze-list-data (hash-table)
  (maphash-return (lambda (key &rest args)
                    (cons (function-name key) (copy-list args)))
                  hash-table))

(DEFUN PRINT-RAW-DATA (&optional longp)
  (DO-OVER-DATA ((event-number)
                 (last-usec-time-low) (last-usec-time-high)
                 (last-disk-wait-time-low) (last-disk-wait-time-high)
                 (last-disk-page-read-count)) ()
    (declare (ignore last-disk-page-read-count))
    (setq event-number (aref buf index))
    (FORMAT T "~%Event ~S (~S), length ~S"
            (aref event-names-index event-number)
            event-number
            (AREF BUF (1+ INDEX)))
    (when longp
      (let ((usec-time-low (AREF BUF (+ INDEX 2)))
            (usec-time-high (AREF BUF (+ INDEX 3)))
            (disk-wait-time-low (AREF BUF (+ INDEX 4)))
            (disk-wait-time-high (AREF BUF (+ INDEX 5)))
            (disk-page-read-count (METER-FIX-UNSIGNED BUF (+ INDEX 6)))
            (current-sg (METER-Q BUF (+ INDEX 8.)))
            (current-function (METER-Q BUF (+ INDEX 10.)))
            (current-stack-depth (METER-FIX-UNSIGNED BUF (+ INDEX 12.))))
        (declare (ignore current-sg last-disk-page-read-count))
        (format t "~%Function ~S, depth ~D"
                current-function current-stack-depth)
        (format t "~%  Time delta ~D, disk-wait delta ~D, page-read-count ~d"
                (if last-usec-time-low (time-diff last-usec-time-high last-usec-time-low
                                                  usec-time-high usec-time-low))
                (if last-disk-wait-time-low
                    (time-diff last-disk-wait-time-high last-disk-wait-time-low
                               disk-wait-time-high disk-wait-time-low))
                disk-page-read-count)
        (cond ((or (= event-number %meter-function-entry-event)
                   (= event-number %meter-function-exit-event))
               (format t "~%  Function ~S" (meter-q buf (+ index 14.)))))
        (cond ((= event-number %meter-stack-group-switch-event)
               (format t "~%  Leaving stack group ~s" (meter-q buf (+ index 14.)))))
        (setq last-usec-time-high usec-time-high
              last-usec-time-low usec-time-low
              last-disk-wait-time-high disk-wait-time-high
              last-disk-wait-time-low disk-wait-time-low)))))

(DEFUN ANALYZE-FREE (INFO)
  (LET ((SYM (NAMED-STRUCTURE-P INFO)))
    (IF (AND SYM (SETQ SYM (GET SYM 'RETURN-FUNCTION)))
        (FUNCALL SYM INFO))
    NIL))

(DEFUN COPY-FRAME-TO-STATE (BUF INDEX STATE)
  (LET ((EVENT (AREF BUF INDEX))
        (LENGTH (AREF BUF (1+ INDEX))))
    (IF (= LENGTH 0)
        NIL
      (SETF (LOW-REAL-TIME STATE) (AREF BUF (+ INDEX 2)))
      (SETF (HIGH-REAL-TIME STATE) (AREF BUF (+ INDEX 3)))
      (SETF (LOW-DISK-TIME STATE) (AREF BUF (+ INDEX 4)))
      (SETF (HIGH-DISK-TIME STATE) (AREF BUF (+ INDEX 5)))
      (SETF (PAGE-FAULTS STATE) (METER-FIX-UNSIGNED BUF (+ INDEX 6)))
      (SETF (STACK-GROUP STATE) (METER-Q BUF (+ INDEX 8.)))
      (SETF (CURRENT-FUNCTION STATE) (METER-Q BUF (+ INDEX 10.)))
      (SETF (STACK-DEPTH STATE) (METER-FIX-UNSIGNED BUF (+ INDEX 12.)))
      EVENT)))

(DEFUN STATE-RELATIVE-INFO (BASE-STATE NEW-STATE)
  (LET ((REAL-TIME (TIME-DIFF (HIGH-REAL-TIME BASE-STATE) (LOW-REAL-TIME BASE-STATE)
                              (HIGH-REAL-TIME NEW-STATE) (LOW-REAL-TIME NEW-STATE)))
        (DISK-TIME (TIME-DIFF (HIGH-DISK-TIME BASE-STATE) (LOW-DISK-TIME BASE-STATE)
                              (HIGH-DISK-TIME NEW-STATE) (LOW-DISK-TIME NEW-STATE))))
    (VALUES REAL-TIME
            (- REAL-TIME DISK-TIME)
            (%POINTER-DIFFERENCE (PAGE-FAULTS NEW-STATE) (PAGE-FAULTS BASE-STATE)))))

;;; List of events analysis function
(DEFTABLE :LIST-EVENTS 'INIT-LIST-EVENTS)

(DEFANALYZE :LIST-EVENTS :PAGE-IN . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :PAGE-OUT . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :CONS . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :FUNCTION-ENTRY . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :FUNCTION-EXIT . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :FUNCTION-UNWIND . LIST-EVENT-FUNCTION)
(DEFANALYZE :LIST-EVENTS :STACK-SWITCH . LIST-EVENT-FUNCTION)

(DEFUN INIT-LIST-EVENTS (PLIST IGNORE BUF INDEX)
  (LET ((INFO (MAKE-BASIC-INFO)))
    (UNLESS (NULL PLIST)
      (FERROR "~S Bad options" PLIST))
    (SETF (BASE-STATE INFO) (ALLOCATE-RESOURCE 'STACK-STATE))
    (SETF (NEXT-STATE INFO) (ALLOCATE-RESOURCE 'STACK-STATE))
    (IF BUF
        (COPY-FRAME-TO-STATE BUF INDEX (BASE-STATE INFO)))
    INFO))

(DEFUN (BASIC-INFO RETURN-FUNCTION) (INFO)
  (DEALLOCATE-RESOURCE 'STACK-STATE (BASE-STATE INFO))
  (DEALLOCATE-RESOURCE 'STACK-STATE (NEXT-STATE INFO)))

(DEFCONST FIND-CLOSEST-SYM-FUN
          (INTERN "LAM-FIND-CLOSEST-SYM" "LAMBDA"))

  ;(INTERN "CC-FIND-CLOSEST-SYM" "CC")


(DEFCONST RACMO-SYMBOL (INTERN "RACMO" "LAMBDA"))       ;"CC" for cadr.

(DEFUN LIST-EVENT-FUNCTION (BUF INDEX INFO STREAM)
  (LET ((REAL-TIME)
        (RUN-TIME)
        (PAGE-FAULTS)
        (NEW-STATE)
        (EVENT (AREF BUF INDEX)))
  (MULTIPLE-VALUE-SETQ (REAL-TIME RUN-TIME PAGE-FAULTS)
    (STATE-RELATIVE-INFO (BASE-STATE INFO) (SETQ NEW-STATE (NEXT-STATE INFO))))
  (FORMAT STREAM "~&~9D ~9D ~4D ~20A ~20S ~5D "
          REAL-TIME RUN-TIME PAGE-FAULTS
          (SG-NAME (STACK-GROUP NEW-STATE))
          (FUNCTION-NAME (CURRENT-FUNCTION NEW-STATE))
          (STACK-DEPTH NEW-STATE))
  (SELECT EVENT
    ((%METER-FUNCTION-ENTRY-EVENT %METER-FUNCTION-EXIT-EVENT)
     (FORMAT STREAM "~:[RET ~;CALL~] ~S"
             (= EVENT %METER-FUNCTION-ENTRY-EVENT)
             (FUNCTION-NAME (METER-Q BUF (+ INDEX 14.)))))
    (%METER-FUNCTION-UNWIND-EVENT
     (FORMAT STREAM "UNWIND"))
    ((%METER-PAGE-IN-EVENT %METER-PAGE-OUT-EVENT)
     (LET ((VMA (METER-FIX-UNSIGNED BUF (+ INDEX 14.)))
           (UPC (METER-FIX-UNSIGNED BUF (+ INDEX 16.)))
           (MICRO-NAME))
       (SETQ MICRO-NAME
             (FUNCALL FIND-CLOSEST-SYM-FUN
                      (+ (SYMBOL-VALUE RACMO-SYMBOL) (LDB #o0020 UPC))))
       (IF (CONSP MICRO-NAME)
           (SETQ MICRO-NAME (CAR MICRO-NAME)))
       (FORMAT STREAM "~:[PAGI~;PAGO~]~%~10T~8O (~S) ~S"
               (= EVENT %METER-PAGE-OUT-EVENT)
               VMA
               (AREF #'AREA-NAME (%AREA-NUMBER VMA))
               (OR MICRO-NAME (LDB #o0020 UPC)))))
    (%METER-STACK-GROUP-SWITCH-EVENT
     (FORMAT STREAM "~A" (SG-NAME (METER-Q BUF (+ INDEX 14.)))))
    (%meter-macro-instruction-event
     (let ((standard-output stream))
       (compiler:disassemble-instruction (current-function new-state)
                                         (1- (meter-fix-unsigned buf (+ index 14.))))))
    (OTHERWISE
     (FORMAT STREAM "~&Bad event")))))


;;;; Tree hacking info

(DEFSTRUCT (TREE-INFO :NAMED-ARRAY-LEADER (:INCLUDE BASIC-INFO)
                      (:MAKE-ARRAY (:LENGTH (* TREE-SIZE 150.)))
                      :CONC-NAME (:ALTERANT NIL))
  STACK-GROUPS
  CURRENT-FUNCTION
  OUTPUT-FUNCTION
  OUTPUT-PLIST
  )

(DEFSTRUCT (TREE :GROUPED-ARRAY :CONC-NAME :SIZE-SYMBOL (:ALTERANT NIL))
  CALLER
  FUNCTION
  REAL-TIME
  RUN-TIME
  PAGE-FAULTS
  CALLED-FUNCTIONS
  CALLED-REAL-TIME
  CALLED-RUN-TIME
  CALLED-PAGE-FAULTS
  STACK-DEPTH
  NEXT-CALLED
  )

(DEFRESOURCE TREE-INFO () :CONSTRUCTOR (MAKE-TREE-INFO)
             :free-list-cell (aloc object 1))

(DEFTABLE :TREE 'TREE-INIT 'TREE-EXIT)

(DEFUN TREE-INIT (PLIST IGNORE BUF INDEX)
  (LET ((INFO (ALLOCATE-RESOURCE 'TREE-INFO))
        (BASE-STATE (ALLOCATE-RESOURCE 'STACK-STATE)))
    (SETF (INFO-LENGTH INFO) 0)
    (IF BUF
        (COPY-FRAME-TO-STATE BUF INDEX BASE-STATE))
    (SETF (BASE-STATE INFO) BASE-STATE)
    (SETF (NEXT-STATE INFO) (ALLOCATE-RESOURCE 'STACK-STATE))
    (SETF (TREE-INFO-STACK-GROUPS INFO) NIL)
    (SETF (TREE-INFO-CURRENT-FUNCTION INFO) NIL)
    ;; Would process non-output options here if there were any
    ;; There aren't, so ignore plist
    PLIST
    INFO))

(DEFUN (TREE-INFO RETURN-FUNCTION) (INFO)
  (DEALLOCATE-RESOURCE 'STACK-STATE (BASE-STATE INFO))
  (DEALLOCATE-RESOURCE 'STACK-STATE (NEXT-STATE INFO))
  (DEALLOCATE-RESOURCE 'TREE-INFO INFO))

(DEFUN TREE-EXIT (INFO STREAM PLIST)
  ;; Decide output function and its options
  (SETF (TREE-INFO-OUTPUT-FUNCTION INFO) 'SUMMARIZE-TREE)
  (SETF (TREE-INFO-OUTPUT-PLIST INFO) NIL)
  (DO ((L PLIST (CDDR L))
       (OPLIST (GET (TREE-INFO-OUTPUT-FUNCTION INFO) 'OUTPUT-OPTIONS)))
      ((NULL L))
    (CASE (CAR L)
      (:OUTPUT (SETF (TREE-INFO-OUTPUT-FUNCTION INFO) (CADR L))
               (IF (NOT (NULL (TREE-INFO-OUTPUT-PLIST INFO)))
                   (FERROR "Output options specified before output function"))
               (SETQ OPLIST (GET (CADR L) 'OUTPUT-OPTIONS)))
      (:FIND-CALLERS
        (SETF (TREE-INFO-OUTPUT-FUNCTION INFO) 'TREE-FIND-CALLERS)
        (IF (NOT (NULL (TREE-INFO-OUTPUT-PLIST INFO)))
            (FERROR "Output options specified before output function"))
        (PUSH (CADR L) (TREE-INFO-OUTPUT-PLIST INFO))
        (PUSH ':FIND-CALLERS (TREE-INFO-OUTPUT-PLIST INFO))
        (SETQ OPLIST (GET 'TREE-FIND-CALLERS 'OUTPUT-OPTIONS)))
      (OTHERWISE
       (COND ((MEMQ (CAR L) OPLIST)
              (PUSH (CADR L) (TREE-INFO-OUTPUT-PLIST INFO))
              (PUSH (CAR L) (TREE-INFO-OUTPUT-PLIST INFO)))
             (T (FERROR "~S Bad option" (CAR L)))))))
  ;; Finish off tree
  (LET* ((CUR-STACK (STACK-GROUP (BASE-STATE INFO)))
         (INF (ASSQ CUR-STACK (TREE-INFO-STACK-GROUPS INFO))))
    (IF (AND (NULL INF) (NOT (NULL (TREE-INFO-CURRENT-FUNCTION INFO))))
        (PUSH (CONS CUR-STACK (TREE-INFO-CURRENT-FUNCTION INFO))
              (TREE-INFO-STACK-GROUPS INFO))))
  (DOLIST (L (TREE-INFO-STACK-GROUPS INFO))
    (RPLACD L (FIND-ROOT INFO (CDR L))))
  ;; Print
  (FUNCALL (TREE-INFO-OUTPUT-FUNCTION INFO) INFO STREAM))

(DEFUN FIND-ROOT (INFO INDEX)
  (IF INDEX
      (DO ((ROOT INDEX (TREE-CALLER ROOT INFO)))
          ((NULL ROOT) INDEX)
        (SETQ INDEX ROOT))))

(DEFANALYZE :TREE :FUNCTION-ENTRY
  (LET ((CALLEE (GET-TREE INFO))
        (CALLER (TREE-INFO-CURRENT-FUNCTION INFO))
        (NEW-STATE (NEXT-STATE INFO))
        (BASE-STATE (BASE-STATE INFO)))
    (WHEN (NULL CALLER)
      (SETQ CALLER (GET-TREE INFO))
      (SETF (TREE-FUNCTION CALLER INFO) (CURRENT-FUNCTION NEW-STATE)))
    (SETF (TREE-NEXT-CALLED CALLEE INFO) (TREE-CALLED-FUNCTIONS CALLER INFO))
    (SETF (TREE-CALLED-FUNCTIONS CALLER INFO) CALLEE)
    (MULTIPLE-VALUE-BIND (REAL-TIME RUN-TIME PAGE-FAULTS)
        (STATE-RELATIVE-INFO BASE-STATE NEW-STATE)
      (INCF (TREE-REAL-TIME CALLER INFO) REAL-TIME)
      (INCF (TREE-RUN-TIME CALLER INFO) RUN-TIME)
      (INCF (TREE-PAGE-FAULTS CALLER INFO) PAGE-FAULTS))
    (SETF (TREE-CALLER CALLEE INFO) CALLER)
    (SETF (TREE-FUNCTION CALLEE INFO) (METER-Q BUF (+ INDEX 14.)))
    (SETF (TREE-STACK-DEPTH CALLEE INFO) (STACK-DEPTH NEW-STATE))
    (SETF (TREE-INFO-CURRENT-FUNCTION INFO) CALLEE)
    (SETF (BASE-STATE INFO) NEW-STATE)
    (SETF (NEXT-STATE INFO) BASE-STATE)))

(DEFANALYZE :TREE :FUNCTION-EXIT
  (SELECT (%DATA-TYPE (METER-Q BUF (+ INDEX 14.)))
    ((DTP-U-ENTRY DTP-STACK-GROUP) NIL)
    (OTHERWISE
     (TREE-FUNCTION-EXIT BUF INDEX INFO)
     (LET ((NEW-STATE (NEXT-STATE INFO)))
       (SETF (NEXT-STATE INFO) (BASE-STATE INFO))
       (SETF (BASE-STATE INFO) NEW-STATE)))))

(DEFUN TREE-FUNCTION-EXIT (BUF INDEX INFO)
  (LET* ((NEW-STATE (NEXT-STATE INFO))
         (BASE-STATE (BASE-STATE INFO))
         (RETURNER (TREE-INFO-CURRENT-FUNCTION INFO))
         (RETURNEE (AND RETURNER (TREE-CALLER RETURNER INFO))))
    (UNLESS RETURNER
      (SETQ RETURNER (GET-TREE INFO))
      (SETF (TREE-FUNCTION RETURNER INFO) (METER-Q BUF (+ INDEX 14.))))
    (UNLESS RETURNEE
      (SETQ RETURNEE (GET-TREE INFO))
      (SETF (TREE-FUNCTION RETURNEE INFO) (CURRENT-FUNCTION NEW-STATE))
      (SETF (TREE-NEXT-CALLED RETURNER INFO) (TREE-CALLED-FUNCTIONS RETURNEE INFO))
      (SETF (TREE-CALLED-FUNCTIONS RETURNEE INFO) RETURNER))
    (OR (TREE-CALLER RETURNER INFO)
        (SETF (TREE-CALLER RETURNER INFO) RETURNEE))
    (MULTIPLE-VALUE-BIND (REAL-TIME RUN-TIME PAGE-FAULTS)
        (STATE-RELATIVE-INFO BASE-STATE NEW-STATE)
      (INCF (TREE-REAL-TIME RETURNER INFO) REAL-TIME)
      (INCF (TREE-RUN-TIME RETURNER INFO) RUN-TIME)
      (INCF (TREE-PAGE-FAULTS RETURNER INFO) PAGE-FAULTS))
    (INCF (TREE-CALLED-REAL-TIME RETURNEE INFO)
          (+ (TREE-REAL-TIME RETURNER INFO)
             (TREE-CALLED-REAL-TIME RETURNER INFO)))
    (INCF (TREE-CALLED-RUN-TIME RETURNEE INFO)
          (+ (TREE-RUN-TIME RETURNER INFO)
             (TREE-CALLED-RUN-TIME RETURNER INFO)))
    (INCF (TREE-CALLED-PAGE-FAULTS RETURNEE INFO)
          (+ (TREE-PAGE-FAULTS RETURNER INFO)
             (TREE-CALLED-PAGE-FAULTS RETURNER INFO)))
    (SETF (TREE-CALLED-FUNCTIONS RETURNER INFO)
          (REVCALLEES RETURNER INFO))
    (SETF (TREE-INFO-CURRENT-FUNCTION INFO) RETURNEE)))

;;; This reverses the list of callees
(DEFUN REVCALLEES (CALLEES INFO)
  (IF CALLEES
      (DO ((PREV NIL NEXT)
           (NEXT (TREE-CALLED-FUNCTIONS CALLEES INFO) NEXT-NEXT)
           (NEXT-NEXT))
          ((NULL NEXT) PREV)
        (SETQ NEXT-NEXT (TREE-NEXT-CALLED NEXT INFO))
        (SETF (TREE-NEXT-CALLED NEXT INFO) PREV))
    NIL))

(DEFANALYZE :TREE :STACK-SWITCH
  (LET* ((FCTN (TREE-INFO-CURRENT-FUNCTION INFO))
         (NEW-STATE (NEXT-STATE INFO)))
    (IF (NOT (NULL FCTN))
        (MULTIPLE-VALUE-BIND (REAL-TIME RUN-TIME PAGE-FAULTS)
            (STATE-RELATIVE-INFO (BASE-STATE INFO) NEW-STATE)
          (INCF (TREE-REAL-TIME FCTN INFO) REAL-TIME)
          (INCF (TREE-RUN-TIME FCTN INFO) RUN-TIME)
          (INCF (TREE-PAGE-FAULTS FCTN INFO) PAGE-FAULTS)))
    (LET ((OLD-STACK-GROUP (METER-Q BUF (+ INDEX 14.)))
          (INF))
      (IF (SETQ INF (ASSQ OLD-STACK-GROUP (TREE-INFO-STACK-GROUPS INFO)))
          (RPLACD INF FCTN)
          (AND FCTN (PUSH (CONS OLD-STACK-GROUP FCTN) (TREE-INFO-STACK-GROUPS INFO)))))
    (SETF (TREE-INFO-CURRENT-FUNCTION INFO)
          (CDR (ASSQ (STACK-GROUP NEW-STATE) (TREE-INFO-STACK-GROUPS INFO))))
    (SETF (NEXT-STATE INFO) (BASE-STATE INFO))
    (SETF (BASE-STATE INFO) NEW-STATE)))

(DEFUN GET-TREE (TREE)
  (LET* ((IDX (LENGTH TREE))
         (EXTENSION (* TREE-SIZE (FLOOR (FLOOR IDX TREE-SIZE) 3))))
    (VECTOR-PUSH-EXTEND NIL TREE EXTENSION)     ;Caller
    (VECTOR-PUSH NIL TREE)                      ;Function
    (VECTOR-PUSH 0 TREE)                        ;Real time
    (VECTOR-PUSH 0 TREE)                        ;Run time
    (VECTOR-PUSH 0 TREE)                        ;Page faults
    (VECTOR-PUSH NIL TREE)                      ;Called functions
    (VECTOR-PUSH 0 TREE)                        ;Called real time
    (VECTOR-PUSH 0 TREE)                        ;Called run time
    (VECTOR-PUSH 0 TREE)                        ;Called page faults
    (VECTOR-PUSH 0 TREE)                        ;Stack depth
    (VECTOR-PUSH NIL TREE)                      ;Next called
    IDX))

;;;; Output functions for TREE manipulation

(DEFUN TREE-NULL (IGNORE IGNORE))

(DEFPROP TREE-PRINT (:STACK-GROUP) OUTPUT-OPTIONS)
(DEFUN TREE-PRINT (INFO STREAM &AUX STK-GROUP)
  (DO ((PL (TREE-INFO-OUTPUT-PLIST INFO) (CDDR PL)))
      ((NULL PL))
    (CASE (CAR PL)
      (:STACK-GROUP (SETQ STK-GROUP (CADR PL)))
      (OTHERWISE (FERROR "Output option ~S not recognized" (CAR PL)))))
  (DOLIST (L (TREE-INFO-STACK-GROUPS INFO))
    (COND ((AND (OR (NULL STK-GROUP) (EQ (CAR L) STK-GROUP)
                    (AND (LISTP STK-GROUP) (MEMQ (CAR L) STK-GROUP)))
                (NOT (NULL (CDR L))))
           (FORMAT STREAM "~&Stack Group: ~A" (SG-NAME (CAR L)))
           (TREE-PRINT-INTERNAL INFO (CDR L) 0 STREAM)))))

(DEFUN TREE-PRINT-INTERNAL (INFO INDEX LEVEL STREAM)
  (WHEN INDEX
    (FORMAT STREAM "~&[~3D]~30S ~8D ~8D ~8D ~10D ~10D ~8D" LEVEL
            (FUNCTION-NAME (TREE-FUNCTION INDEX INFO))
            (TREE-REAL-TIME INDEX INFO)
            (TREE-RUN-TIME INDEX INFO)
            (TREE-PAGE-FAULTS INDEX INFO)
            (TREE-CALLED-REAL-TIME INDEX INFO)
            (TREE-CALLED-RUN-TIME INDEX INFO)
            (TREE-CALLED-PAGE-FAULTS INDEX INFO))
    (DO ((L (TREE-CALLED-FUNCTIONS INDEX INFO) (TREE-NEXT-CALLED L INFO)))
        ((NULL L))
      (TREE-PRINT-INTERNAL INFO L (1+ LEVEL) STREAM))))


(DEFSTRUCT (SUMMARY-INFO :GROUPED-ARRAY :SIZE-SYMBOL :CONC-NAME (:ALTERANT NIL))
  NAME
  CALLS
  REAL-TIME
  RUN-TIME
  PAGE-FAULTS)

(DEFRESOURCE SUMMARY-INFOS ()
  :CONSTRUCTOR (MAKE-SUMMARY-INFO :TIMES 100. :MAKE-ARRAY (:FILL-POINTER 0)))
(DEFRESOURCE SUMMARY-TABLES ()
  :CONSTRUCTOR (MAKE-HASH-TABLE :SIZE 100.))

(DEFPROP SUMMARIZE-TREE (:STACK-GROUP :SORT-FUNCTION :SUMMARIZE :INCLUSIVE) OUTPUT-OPTIONS)
(DEFUN SUMMARIZE-TREE (INFO STREAM
                       &AUX STK-GROUP (SORT-FCTN 'MAX-RUN-TIME) ONLY-FOR INCLUSIVE)
  (DECLARE (SPECIAL ONLY-FOR INCLUSIVE))
  (DO ((PL (TREE-INFO-OUTPUT-PLIST INFO) (CDDR PL)))
      ((NULL PL))
    (CASE (CAR PL)
      (:STACK-GROUP (SETQ STK-GROUP (CADR PL)))
      (:SORT-FUNCTION (SETQ SORT-FCTN (CADR PL)))
      (:SUMMARIZE (SETQ ONLY-FOR (FUNCTION-LIST (CADR PL))))
      (:INCLUSIVE (SETQ INCLUSIVE (CADR PL)))
      (OTHERWISE (FERROR "Output option ~S not recognized" (CAR PL)))))
  (USING-RESOURCE (HASH SUMMARY-TABLES)
    (DECLARE (SPECIAL HASH))
    (USING-RESOURCE (SUMM SUMMARY-INFOS)
      (DECLARE (SPECIAL SUMM))
      (CLRHASH HASH)
      (SETF (FILL-POINTER SUMM) 0)
      (DOLIST (L (TREE-INFO-STACK-GROUPS INFO))
        (COND ((AND (OR (NULL STK-GROUP) (EQ (CAR L) STK-GROUP)
                    (AND (LISTP STK-GROUP) (MEMQ (CAR L) STK-GROUP)))
                (NOT (NULL (CDR L))))
               (FORMAT STREAM "~&Stack Group: ~A" (SG-NAME (CAR L)))
               (SUMMARY-BRANCH INFO (CDR L)))))
      (PRINT-SUMMARY-INFORMATION SUMM SORT-FCTN STREAM))))

(DEFUN SUMMARY-BRANCH (INFO INDEX)
  (DECLARE (SPECIAL HASH SUMM ONLY-FOR INCLUSIVE))
  (IF (OR (NULL ONLY-FOR) (MEMQ (TREE-FUNCTION INDEX INFO) ONLY-FOR))
      (SUMMARIZE-NODE INFO INDEX))
  (DO ((IND (TREE-CALLED-FUNCTIONS INDEX INFO) (TREE-NEXT-CALLED IND INFO)))
      ((NULL IND))
    (SUMMARY-BRANCH INFO IND)))

(DEFUN SUMMARIZE-NODE (INFO INDEX)
  (DECLARE (SPECIAL HASH SUMM ONLY-FOR INCLUSIVE))
  (MULTIPLE-VALUE-BIND (VALUE FOUND)
      (GETHASH (TREE-FUNCTION INDEX INFO) HASH)
    (UNLESS FOUND
      (SETQ VALUE (LENGTH SUMM))
      (PUTHASH (TREE-FUNCTION INDEX INFO) VALUE HASH)
      (VECTOR-PUSH-EXTEND (TREE-FUNCTION INDEX INFO) SUMM (* 100. SUMMARY-INFO-SIZE))
      (VECTOR-PUSH 0 SUMM)                      ;Calls
      (VECTOR-PUSH 0 SUMM)                      ;Real time
      (VECTOR-PUSH 0 SUMM)                      ;Run time
      (VECTOR-PUSH 0 SUMM))                     ;Page faults
    (INCF (SUMMARY-INFO-CALLS VALUE SUMM))
    (INCF (SUMMARY-INFO-REAL-TIME VALUE SUMM) (TREE-REAL-TIME INDEX INFO))
    (IF INCLUSIVE
        (INCF (SUMMARY-INFO-REAL-TIME VALUE SUMM) (TREE-CALLED-REAL-TIME INDEX INFO)))
    (INCF (SUMMARY-INFO-RUN-TIME VALUE SUMM) (TREE-RUN-TIME INDEX INFO))
    (IF INCLUSIVE
        (INCF (SUMMARY-INFO-RUN-TIME VALUE SUMM) (TREE-CALLED-RUN-TIME INDEX INFO)))
    (INCF (SUMMARY-INFO-PAGE-FAULTS VALUE SUMM) (TREE-PAGE-FAULTS INDEX INFO))
    (IF INCLUSIVE
        (INCF (SUMMARY-INFO-PAGE-FAULTS VALUE SUMM) (TREE-CALLED-PAGE-FAULTS INDEX INFO)))))

(DEFUN PRINT-SUMMARY-INFORMATION (SUMM SORT-FCTN STREAM)
  (SORT-GROUPED-ARRAY-GROUP-KEY SUMM SUMMARY-INFO-SIZE SORT-FCTN)
  (FORMAT STREAM "~&~55A~6@A ~10@A ~6@A ~10@A"
          "Functions" "# calls" "Run T" "Faults" "Real T")
  (DO ((IDX 0 (+ IDX SUMMARY-INFO-SIZE))
       (TOTAL-CALLS 0)
       (TOTAL-REAL-TIME 0)
       (TOTAL-RUN-TIME 0)
       (TOTAL-PAGE-FAULTS 0))
      (( IDX (LENGTH SUMM))
       (FORMAT STREAM "~2%~55A ~6D ~10D ~6D ~10D"
               "Total" TOTAL-CALLS TOTAL-RUN-TIME TOTAL-PAGE-FAULTS TOTAL-REAL-TIME))
    (FORMAT STREAM "~&~55S ~6D ~10D ~6D ~10D"
            (FUNCTION-NAME (SUMMARY-INFO-NAME IDX SUMM))
            (SUMMARY-INFO-CALLS IDX SUMM)
            (SUMMARY-INFO-RUN-TIME IDX SUMM)
            (SUMMARY-INFO-PAGE-FAULTS IDX SUMM)
            (SUMMARY-INFO-REAL-TIME IDX SUMM))
    (SETQ TOTAL-CALLS (+ TOTAL-CALLS (SUMMARY-INFO-CALLS IDX SUMM))
          TOTAL-REAL-TIME (+ TOTAL-REAL-TIME (SUMMARY-INFO-REAL-TIME IDX SUMM))
          TOTAL-RUN-TIME (+ TOTAL-RUN-TIME (SUMMARY-INFO-RUN-TIME IDX SUMM))
          TOTAL-PAGE-FAULTS (+ TOTAL-PAGE-FAULTS
                               (SUMMARY-INFO-PAGE-FAULTS IDX SUMM)))))

;;; Sorting functions
(DEFUN MAX-CALLS (ARY1 IDX1 ARY2 IDX2)
  (> (SUMMARY-INFO-CALLS IDX1 ARY1)
     (SUMMARY-INFO-CALLS IDX2 ARY2)))

(DEFUN MAX-RUN-TIME (ARY1 IDX1 ARY2 IDX2)
  (> (SUMMARY-INFO-RUN-TIME IDX1 ARY1)
     (SUMMARY-INFO-RUN-TIME IDX2 ARY2)))

(DEFUN MAX-REAL-TIME (ARY1 IDX1 ARY2 IDX2)
  (> (SUMMARY-INFO-REAL-TIME IDX1 ARY1)
     (SUMMARY-INFO-REAL-TIME IDX2 ARY2)))

(DEFUN MAX-PAGE-FAULTS (ARY1 IDX1 ARY2 IDX2)
  (> (SUMMARY-INFO-PAGE-FAULTS IDX1 ARY1)
     (SUMMARY-INFO-PAGE-FAULTS IDX2 ARY2)))

(DEFUN MAX-RUN-TIME-PER-CALL (ARY1 IDX1 ARY2 IDX2)
  (> (IF (> (SUMMARY-INFO-CALLS IDX1 ARY1) 0)
         (// (FLOAT (SUMMARY-INFO-RUN-TIME IDX1 ARY1))
             (SUMMARY-INFO-CALLS IDX1 ARY1))
       0)
     (IF (> (SUMMARY-INFO-CALLS IDX2 ARY2) 0)
         (// (FLOAT (SUMMARY-INFO-RUN-TIME IDX2 ARY2))
             (SUMMARY-INFO-CALLS IDX2 ARY2))
       0)))

(DEFPROP TREE-FIND-CALLERS (:STACK-GROUP) OUTPUT-OPTIONS)
(DEFUN TREE-FIND-CALLERS (INFO STREAM &AUX STK-GROUP CALLEES)
  (DECLARE (SPECIAL STREAM))
  (DO ((PL (TREE-INFO-OUTPUT-PLIST INFO) (CDDR PL)))
      ((NULL PL))
    (CASE (CAR PL)
      (:STACK-GROUP (SETQ STK-GROUP (CADR PL)))
      (:FIND-CALLERS
       (SETQ CALLEES (FUNCTION-LIST (CADR PL))))
      (OTHERWISE (FERROR "Output option ~S not recognized" (CAR PL)))))
  (USING-RESOURCE (HASH SUMMARY-TABLES)
    (SEND HASH :CLEAR-HASH)
    (DOLIST (L (TREE-INFO-STACK-GROUPS INFO))
      (COND ((AND (OR (NULL STK-GROUP) (EQ (CAR L) STK-GROUP)
                      (AND (LISTP STK-GROUP) (MEMQ (CAR L) STK-GROUP)))
                  (NOT (NULL (CDR L))))
             (FORMAT STREAM "~&Stack Group: ~A~%" (SG-NAME (CAR L)))
             (FIND-CALLERS-BRANCH INFO (CDR L) HASH CALLEES))))
    (MAPHASH #'(LAMBDA (CALLEE ALIST)
                 (LOOP FOR (CALLER . N-TIMES) IN ALIST
                       DO (FORMAT STREAM "~S called ~S ~D time~:P.~%"
                                  (FUNCTION-NAME CALLER) (FUNCTION-NAME CALLEE) N-TIMES))
                 (SEND STREAM :TYO #/NEWLINE))
             HASH)))

(DEFUN FIND-CALLERS-BRANCH (INFO INDEX HASH CALLEES)
  (LOOP FOR IND = (TREE-CALLED-FUNCTIONS INDEX INFO) THEN (TREE-NEXT-CALLED IND INFO)
            WHILE IND
        AS CALLEE = (TREE-FUNCTION IND INFO)
        WHEN (MEMQ CALLEE CALLEES)
          DO (PUTHASH CALLEE (INCASSQ (TREE-FUNCTION INDEX INFO) (GETHASH CALLEE HASH)) HASH)
        DO (FIND-CALLERS-BRANCH INFO IND HASH CALLEES)))

(DEFUN INCASSQ (KEY ALIST &AUX TEM)
  (IF (SETQ TEM (ASSQ KEY ALIST))
      (INCF (CDR TEM))
    (PUSH (CONS KEY 1) ALIST))
  ALIST)

;;; Spec can be symbol, function spec, or list of such
;;; Returns a list of functions (NOT function names)
(DEFUN FUNCTION-LIST (SPEC)
  (IF (OR (ATOM SPEC)
          (SI:VALIDATE-FUNCTION-SPEC SPEC))
      (LIST (FDEFINITION SPEC))
    (MAPCAR 'FDEFINITION SPEC)))

;;; Random functions
(DEFUN TEST (FORM &OPTIONAL (ENABLES #o17))
  "Execute FORM with metering enabled in this stack group only."
  (compiler::%SET-METER-ENABLES 0)
  (RESET)
  (DISABLE)
  (ENABLE %CURRENT-STACK-GROUP)
  (let ((prev %meter-micro-enables))
   (unwind-protect
       (progn (compiler::%set-meter-enables enables)
              (do-stack-group-switch)
              (eval form))
     (compiler::%set-meter-enables prev)))
  (DISABLE))

(DEFUN RUN (&REST FORMS)
  "Execute FORMS with metering in stack groups you have enabled with METER:ENABLE."
  (RESET)
  (let ((prev %meter-micro-enables))
    (unwind-protect
        (progn (compiler::%set-meter-enables #o14)
               (do-stack-group-switch)
               (DOLIST (FORM FORMS) (EVAL FORM)))
      (compiler::%set-meter-enables prev))))

;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1981 Massachusetts Institute of Technology **

;;; Hairier who-line system

(DEFFLAVOR WHO-LINE-SCREEN () (SCREEN))

(DEFMETHOD (WHO-LINE-SCREEN :USER-VISIBLE) () NIL)

(DEFFLAVOR WHO-LINE-MIXIN ((WHO-LINE-ITEM-STATE NIL)) ()
                                ;WHO-LINE-ITEM-STATE is NIL if the contents of the window
                                ;is unknown and needs to be redrawn.  If non-NIL it
                                ;represents the current contents, to avoid extra redisplay.
  (:INCLUDED-FLAVORS MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :MORE-P NIL :BLINKER-P NIL :FONT-MAP '(FONTS:CPTFONT))
  (:REQUIRED-METHODS :UPDATE)
  (:SELECT-METHOD-ORDER :UPDATE)
  (:INIT-KEYWORDS :FLAVOR)
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES)

(DEFWRAPPER (WHO-LINE-MIXIN :UPDATE) (IGNORE . BODY)
  `(WITHOUT-INTERRUPTS
     (AND (SHEET-CAN-GET-LOCK SELF)
          (NOT (SHEET-OUTPUT-HELD-P SELF))
          (PROGN . ,BODY))))

(DEFMETHOD (WHO-LINE-MIXIN :AFTER :REFRESH) (&OPTIONAL TYPE)
  (UNLESS (AND RESTORED-BITS-P (NEQ TYPE ':SIZE-CHANGED))
    (SEND SELF :CLOBBERED)
    (SEND SELF :UPDATE)))

;;; Should this actually do the updates here??
(DEFMETHOD (WHO-LINE-MIXIN :CLOBBERED) ()
  (SETQ WHO-LINE-ITEM-STATE NIL))

(DEFFLAVOR WHO-LINE-SHEET
        ((WHO-LINE-UPDATE-FUNCTION NIL) (WHO-LINE-EXTRA-STATE NIL))
        (WHO-LINE-MIXIN MINIMUM-WINDOW)
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (WHO-LINE-SHEET :BEFORE :INIT) (PLIST)
  (PUTPROP PLIST (GET PLIST ':WHO-LINE-UPDATE-FUNCTION) ':NAME))

(DEFMETHOD (WHO-LINE-SHEET :UPDATE) ()
  (AND WHO-LINE-UPDATE-FUNCTION
       (SEND WHO-LINE-UPDATE-FUNCTION SELF)))

(DEFUN WHO-LINE-SETUP ()
  (WHEN (NULL WHO-LINE-SCREEN)
    (SETQ WHO-LINE-SCREEN
          (DEFINE-SCREEN 'WHO-LINE-SCREEN "Who Line Screen"
            :AREA WHO-LINE-AREA
            :DEFAULT-FONT FONTS:CPTFONT ;not *DEFAULT-FONT*
            :BUFFER MAIN-SCREEN-BUFFER-ADDRESS
            :CONTROL-ADDRESS #o377760
            :PROPERTY-LIST '(:VIDEO :BLACK-AND-WHITE
                                    :CONTROLLER :SIMPLE
                                    :WHO-LINE T)
            :WIDTH MAIN-SCREEN-WIDTH
            :LOCATIONS-PER-LINE MAIN-SCREEN-LOCATIONS-PER-LINE
            :CHARACTER-HEIGHT 2
            :VSP 0
            :Y NIL                      ;Force this to be calculated
            :BOTTOM MAIN-SCREEN-HEIGHT))
    ;; 18 characters of the date and time
    (SETQ NWATCH-WHO-LINE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'NWATCH-WHO-FUNCTION
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :LEFT 0 :RIGHT 144. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ;; 13 characters of user id or process
    (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                    :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-USER-OR-PROCESS
                    :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                    :LEFT 144. :RIGHT 248. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN))
    ;; 10 characters of package
    (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                    :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-PACKAGE
                    :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                    :LEFT 248. :RIGHT 328. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN))
    ;; 19 characters of process state
    (SETQ WHO-LINE-RUN-STATE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-RUN-STATE
                          :LEFT 328. :RIGHT (SELECT-PROCESSOR
                                              (:CADR 480.)
                                              ((:LAMBDA :EXPLORER) 520.))
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ;; The remaining 36 characters go to the file/idle/boot state
    (SETQ WHO-LINE-FILE-STATE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-FILE-SHEET
                          :LEFT (SELECT-PROCESSOR
                                  (:CADR 480.)
                                  ((:LAMBDA :EXPLORER) 520.))
                          :RIGHT (SELECT-PROCESSOR
                                   (:CADR 768.)
                                   ((:LAMBDA :EXPLORER) 1024.))
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ;; Above those windows is a full line of mouse button documentation
    (SETQ WHO-LINE-DOCUMENTATION-WINDOW
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-DOCUMENTATION-FUNCTION
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :TOP 0 :REVERSE-VIDEO-P T))))

(DEFUN WHO-LINE-UPDATE (&OPTIONAL RUN-STATE-ONLY-P &AUX RL)
  (OR INHIBIT-WHO-LINE
      (NULL WHO-LINE-SCREEN)
      (WITHOUT-INTERRUPTS
        (SETQ RL (SELECT-PROCESSOR
                   (:CADR
                    (%XBUS-READ WHO-LINE-RUN-LIGHT-LOC))        ;Don't clobber run light
                   (:LAMBDA
                    (COMPILER:%IO-SPACE-READ WHO-LINE-RUN-LIGHT-LOC))
                   (:explorer
                    (COMPILER:%IO-SPACE-READ WHO-LINE-RUN-LIGHT-LOC))
                   ))
        (IF RUN-STATE-ONLY-P
            ;; The reason this is here is that this function conspires to do some
            ;; minor nice things for you.  This note is here to remind HIC not to
            ;; clean up this code.  --HIC
            (AND WHO-LINE-RUN-STATE-SHEET
                 (SEND WHO-LINE-RUN-STATE-SHEET :UPDATE))
            (DOLIST (I (SHEET-EXPOSED-INFERIORS WHO-LINE-SCREEN))
              (AND (TYPEP I 'WHO-LINE-MIXIN)
                   (SEND I :UPDATE))))
        (SELECT-PROCESSOR
          (:CADR
           (%XBUS-WRITE WHO-LINE-RUN-LIGHT-LOC RL))
          (:LAMBDA
           (COMPILER:%IO-SPACE-WRITE WHO-LINE-RUN-LIGHT-LOC RL))
          (:explorer
           (COMPILER:%IO-SPACE-WRITE WHO-LINE-RUN-LIGHT-LOC RL))
          )))
  T)

(DEFUN WHO-LINE-CLOBBERED ()
  "Inform the who-line that it must redisplay completely."
  (AND WHO-LINE-SCREEN
       (DOLIST (I (SHEET-INFERIORS WHO-LINE-SCREEN))
         (AND (TYPEP I 'WHO-LINE-MIXIN) (SEND I :CLOBBERED)))))

(DEFUN WHO-LINE-STRING (WHO-SHEET NEW-STRING)
  "Output NEW-STRING on WHO-SHEET, a part of the who line, if it has changed.
The last value is remembered in the WHO-LINE-ITEM-STATE instance variable."
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (SETQ NEW-STRING (STRING NEW-STRING))
  (WHEN (NEQ WHO-LINE-ITEM-STATE NEW-STRING)
    (PREPARE-SHEET (WHO-SHEET)
      (SHEET-CLEAR WHO-SHEET)
      (SHEET-STRING-OUT WHO-SHEET NEW-STRING
                        0 (MIN (LENGTH NEW-STRING)
                               (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
                                         (SHEET-CHAR-WIDTH WHO-SHEET)))))
    (SETQ WHO-LINE-ITEM-STATE NEW-STRING)))

(DEFUN WHO-LINE-USER-OR-PROCESS (WHO-SHEET)
  (WHO-LINE-STRING WHO-SHEET (IF WHO-LINE-PROCESS (PROCESS-NAME WHO-LINE-PROCESS) USER-ID)))

(DEFUN WHO-LINE-RUN-STATE (WHO-SHEET)
  (WHO-LINE-STRING WHO-SHEET WHO-LINE-RUN-STATE))

(DEFUN WHO-LINE-PACKAGE (WHO-SHEET &AUX VAL SG)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (LET ((PKG (COND ((SETQ LAST-WHO-LINE-PROCESS (OR WHO-LINE-PROCESS
                                                    (AND SELECTED-IO-BUFFER
                                                         (IO-BUFFER-LAST-OUTPUT-PROCESS
                                                           SELECTED-IO-BUFFER))))
                    (SETQ SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
                    (COND ((EQ SG %CURRENT-STACK-GROUP) *PACKAGE*)
                          ((TYPEP SG 'STACK-GROUP) (SYMEVAL-IN-STACK-GROUP '*PACKAGE* SG))
                          (T PACKAGE))))))
    (WHEN (AND PKG (PACKAGEP PKG)
               (NEQ WHO-LINE-ITEM-STATE PKG))
      (PREPARE-SHEET (WHO-SHEET)
        (SHEET-CLEAR WHO-SHEET)
        (SETQ VAL (SI:PKG-SHORTEST-NAME PKG))
        (SHEET-STRING-OUT WHO-SHEET VAL
                          0 (MIN (STRING-LENGTH VAL)
                                 (- (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
                                               (SHEET-CHAR-WIDTH WHO-SHEET))
                                    (if si::*read-single-colon-allow-internal-symbol*
                                        1
                                        2)))))
      (SHEET-TYO WHO-SHEET #/:)
      (when (not si::*read-single-colon-allow-internal-symbol*)
        (SHEET-TYO WHO-SHEET #/:))
      (SETQ WHO-LINE-ITEM-STATE PKG))))

(DEFUN WHO-LINE-RUN-STATE-UPDATE (&AUX P)  ;Separate variable since other can be setq'ed
                                           ;asynchronously by other processes
  (SETQ LAST-WHO-LINE-PROCESS
        (SETQ P (OR WHO-LINE-PROCESS
                    (PROGN (AND (NULL SELECTED-IO-BUFFER)
                                (NOT (NULL SELECTED-WINDOW))    ;This can happen
                                (SETQ SELECTED-IO-BUFFER
                                      (SEND SELECTED-WINDOW :IO-BUFFER)))
                           (AND SELECTED-IO-BUFFER
                                (IO-BUFFER-LAST-OUTPUT-PROCESS SELECTED-IO-BUFFER))))))
  (SETQ WHO-LINE-RUN-STATE (COND (*WHO-LINE-RUN-STATE-OVERRIDE*)
                                 ((NULL SELECTED-WINDOW)
                                  "No selected window")
                                 ((NULL P)
                                  "No current process")
                                 ((ASSQ P ACTIVE-PROCESSES)
                                  (or (si:process-wait-whostate p)
                                      (si:process-run-whostate p)))
                                 ((NOT (NULL (SI:PROCESS-ARREST-REASONS P)))
                                  "Arrest")
                                 (T "Stop")))
  (WHO-LINE-UPDATE T))

(DEFUN WHO-LINE-FIELD (&REST ARGS &AUX W)
  ;; Do sheet type consing in special area to increase locality
  (SETQ W (APPLY #'MAKE-INSTANCE (GETF ARGS ':FLAVOR)
                 :AREA WHO-LINE-AREA
                 :SUPERIOR WHO-LINE-SCREEN
                 :VSP 0
                 ARGS))
  (SEND W :ACTIVATE)
  (SEND W :EXPOSE)
  W)

(DEFFLAVOR WHO-LINE-FILE-SHEET
           ((CURRENT-STREAM NIL)                ;The one being displayed
            ;; This is an array rather than a list to avoid consing.
            (OPEN-STREAMS (MAKE-ARRAY 20. :TYPE 'ART-Q-LIST :FILL-POINTER 0))
            ;; A list with elements (chaos-connection from-machine contact-name)
            (SERVERS-LIST NIL)
            DISPLAYED-PERCENT DISPLAYED-COUNT)
           (WHO-LINE-MIXIN MINIMUM-WINDOW))

;;; Take the most recently opened input stream if there is one.  Otherwise
;;; take the most recently opened output stream.
(DEFUN WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM (&OPTIONAL (UPDATE-P T))
  (DECLARE (:SELF-FLAVOR WHO-LINE-FILE-SHEET))
  (DO ((I (1- (FILL-POINTER OPEN-STREAMS)) (1- I))
       (OUTPUT-WINNER NIL) (STREAM) (DIRECTION))
      ((MINUSP I)
       (SETQ CURRENT-STREAM OUTPUT-WINNER))
    (SETQ STREAM (AREF OPEN-STREAMS I)
          DIRECTION (NTH-VALUE 1 (SEND STREAM :WHO-LINE-INFORMATION)))
    (CASE DIRECTION
      ((:INPUT :BIDIRECTIONAL)
       (RETURN (SETQ CURRENT-STREAM STREAM)))
      (:OUTPUT
       (OR OUTPUT-WINNER
           (SETQ OUTPUT-WINNER STREAM)))))
  (AND UPDATE-P (WHO-LINE-UPDATE)))

(DEFMETHOD (WHO-LINE-FILE-SHEET :ADD-STREAM) (STREAM &OPTIONAL (UPDATE-P T))
  (AND (VECTOR-PUSH-EXTEND STREAM OPEN-STREAMS)
       (WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM UPDATE-P)))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-STREAM) (STREAM &AUX POS)
  (WHEN (SETQ POS (FIND-POSITION-IN-LIST STREAM (G-L-P OPEN-STREAMS)))
    (IF (= POS (1- (FILL-POINTER OPEN-STREAMS)))
        (VECTOR-POP OPEN-STREAMS)
        (SETF (AREF OPEN-STREAMS POS) (VECTOR-POP OPEN-STREAMS)))
    (AND (EQ STREAM CURRENT-STREAM)
         (WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM))))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-ALL-STREAMS) ()
  (STORE-ARRAY-LEADER 0 OPEN-STREAMS 0)
  (SETQ CURRENT-STREAM NIL))

(ADD-INITIALIZATION "Fix WHO-LINE-FILE-STATE-SHEET"
                    '(SEND WHO-LINE-FILE-STATE-SHEET :DELETE-ALL-STREAMS)
                    '(SYSTEM))

(DEFMETHOD (WHO-LINE-FILE-SHEET :OPEN-STREAMS) ()
  (G-L-P OPEN-STREAMS))

;;; >> Any of the stuff that records servers is going to have to be rewritten.
(DEFMETHOD (WHO-LINE-FILE-SHEET :ADD-SERVER) (CONNECTION CONTACT-NAME
                                               &OPTIONAL (PROCESS SI:CURRENT-PROCESS)
                                               FUNCTION &REST ARGS
                                               &AUX (INHIBIT-SCHEDULING-FLAG T))
  (SEND SELF :DELETE-SERVER CONNECTION)
  (PUSH (MAKE-SERVER-DESC
          :CONNECTION CONNECTION
          :HOST-NAME (if (typep connection 'chaos:conn)
                         (CHAOS:HOST-SHORT-NAME (CHAOS:FOREIGN-ADDRESS CONNECTION))
                       (ip:host-short-name (send connection :remote-address)))
          :CONTACT-NAME CONTACT-NAME
          :PROCESS PROCESS
          :FUNCTION FUNCTION
          :ARGS (COPY-LIST ARGS))
        SERVERS-LIST))

;;; This isn't usually called; Normally servers are deleted automatically when
;;; it is noticed that the connection has been closed.
(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-SERVER) (CONNECTION
                                                 &AUX (INHIBIT-SCHEDULING-FLAG T))
  (SETQ SERVERS-LIST (DEL #'(LAMBDA (X Y) (EQ X (SERVER-DESC-CONNECTION Y)))
                          CONNECTION SERVERS-LIST)))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-ALL-SERVERS) ()
  (SETQ SERVERS-LIST NIL))

(DEFMETHOD (WHO-LINE-FILE-SHEET :CLOSE-ALL-SERVERS) (REASON)
  (LOOP FOR SERVER IN SERVERS-LIST FINALLY (SETQ SERVERS-LIST NIL) DO
        (let ((connection (server-desc-connection server)))
          (if (typep connection 'chaos:conn)
              (CHAOS:CLOSE-CONN (SERVER-DESC-CONNECTION SERVER) REASON)
            (send connection :close)))))

;;; Remove all servers which aren't current anymore.
(DEFUN PURGE-SERVERS ()
  (DECLARE (:SELF-FLAVOR WHO-LINE-FILE-SHEET))
  (WITHOUT-INTERRUPTS
    (DO ((S SERVERS-LIST (CDR S)))
        ((NULL S)
         (SETQ SERVERS-LIST (DELQ NIL SERVERS-LIST)))
      (let ((connection (server-desc-connection (car s))))
        (cond ((typep connection 'chaos:conn)
               (when (AND (NEQ (CHAOS:STATE (SERVER-DESC-CONNECTION (CAR S))) 'CHAOS:OPEN-STATE)
                          (NEQ (CHAOS:STATE (SERVER-DESC-CONNECTION (CAR S)))
                               'CHAOS:RFC-RECEIVED-STATE))
;                (BACKGROUND-NOTIFY "Server ~A from ~A being purged; state is ~A"
;                                   (SERVER-DESC-CONTACT-NAME (CAR S))
;                                   (SERVER-DESC-HOST-NAME (CAR S))
;                                   (CHAOS:STATE (SERVER-DESC-CONNECTION (CAR S))))
                 (SETF (CAR S) NIL)))
              ((null (send connection :remote-address))
               (setf (car s) nil)))))))


(DEFUN BACKGROUND-NOTIFY (FORMAT-STRING &REST ARGS)
  (APPLY #'PROCESS-RUN-FUNCTION "Notify" 'NOTIFY NIL FORMAT-STRING ARGS))

(DEFMETHOD (WHO-LINE-FILE-SHEET :SERVERS) ()
  (PURGE-SERVERS)
  SERVERS-LIST)

;;;; User level functions
(DEFUN DESCRIBE-SERVERS ()
  "Describe all network servers currently serving."
  (DOLIST (S (SEND TV:WHO-LINE-FILE-STATE-SHEET :SERVERS))
    (FORMAT T "~%~A serving ~A in ~A"
            (SERVER-DESC-CONTACT-NAME S)
            (SERVER-DESC-HOST-NAME S)
            (SERVER-DESC-PROCESS S))))

(DEFUN CLOSE-ALL-SERVERS (&OPTIONAL (REASON "Foo on you"))
  "Disconnect all servers on this machine from their remote users."
  (SEND TV:WHO-LINE-FILE-STATE-SHEET :CLOSE-ALL-SERVERS REASON))

(DEFMETHOD (WHO-LINE-FILE-SHEET :UPDATE) (&AUX (MAX-CHARS (TRUNCATE (SHEET-INSIDE-WIDTH)
                                                                    CHAR-WIDTH))
                                               IDLE STRING)
  (cond (pending-notifications
         ;; each element of pending-notifications is (,time ,string ,window-of-interest)
         (if (eq who-line-item-state pending-notifications)
             nil
           (sheet-clear self)
           (if (cdr pending-notifications)
               (progn (setq string (format nil "~D notifications are pending"
                                           (length pending-notifications)))
                      (send self :string-out string 0 (min (length string) max-chars))
                      (setq string nil))
             (setq string (cadr (car pending-notifications)))
             (let ((string-length (length string))
                   (window (caddr (car pending-notifications))))
               (cond ((and window
                           ;;>> What a kludge
                           (string= string "Process " :end1 #.(length "Process "))
                           (or (string= string "wants to type out"
                                        :start1 (max (- string-length
                                                        #.(length "wants to type out"))
                                                     0))
                               (string= string "wants typein"
                                        :start1 (max (- string-length #.(length "wants typein"))
                                                     0))))
                      (send self :string-out string 0 (min string-length max-chars)))
                     (t ;(null window)
                      (let* ((n (%string-search-char #/newline string 0 string-length))
                             (len (+ #.(length "Notification: ") (or n string-length))))
                        (if (eql n (1- string-length)) (setq string-length n n nil))
                        (cond ((and (null n) ( len max-chars))
                               (send self :string-out "Notification: ")
                               (send self :string-out string 0 string-length))
                              (t
                               (send self :string-out "Notification: ")
                               (send self :string-out string
                                          0 (min (or n string-length)
                                                 (- max-chars #.(length "Notification:     "))))
                               (send self :string-out "     "))
                              #||(t
                               (send self :string-out "Notification pending"
                                     0 (min #.(length "Notification pending"))))||#)))))))
         (setq who-line-item-state pending-notifications))
        (CURRENT-STREAM
         (LET ((OLD-STREAM WHO-LINE-ITEM-STATE))
           (MULTIPLE-VALUE-BIND (PATHNAME DIRECTION COUNT PERCENT)
               (SEND CURRENT-STREAM :WHO-LINE-INFORMATION)
             (UNLESS (AND (EQ OLD-STREAM CURRENT-STREAM)
                          (EQ PERCENT DISPLAYED-PERCENT)
                          (EQ COUNT DISPLAYED-COUNT))
               (IF (EQ OLD-STREAM CURRENT-STREAM)
                   (SHEET-HOME SELF)
                   (SHEET-CLEAR SELF))
               (SETQ WHO-LINE-ITEM-STATE CURRENT-STREAM
                     DISPLAYED-PERCENT PERCENT
                     DISPLAYED-COUNT COUNT)
               (DISPLAY-FILE-TRANSFER SELF PATHNAME DIRECTION COUNT
                                      PERCENT MAX-CHARS)))))
        ((AND (NOT (NULL SERVERS-LIST))
              (PROGN (PURGE-SERVERS)
                     (NOT (NULL SERVERS-LIST))))
         (COND ((= (LENGTH SERVERS-LIST) 1)
                (COND ((NEQ WHO-LINE-ITEM-STATE (CAAR SERVERS-LIST))
                       (SHEET-CLEAR SELF)
                       (SETQ STRING (FORMAT NIL "~A serving ~A"
                                            (CADDAR SERVERS-LIST) (CADAR SERVERS-LIST)))
                       (SEND SELF :STRING-OUT STRING 0 (MIN (STRING-LENGTH STRING) MAX-CHARS))
                       (SETQ STRING NIL)
                       (SETQ WHO-LINE-ITEM-STATE (CAAR SERVERS-LIST)))))
               ((NEQ WHO-LINE-ITEM-STATE (LENGTH SERVERS-LIST))
                (SHEET-CLEAR SELF)
                (SETQ STRING (FORMAT NIL "~D Active Servers" (LENGTH SERVERS-LIST)))
                (SEND SELF :STRING-OUT STRING 0 (MIN (LENGTH STRING) MAX-CHARS))
                (SETQ STRING NIL)
                (SETQ WHO-LINE-ITEM-STATE (LENGTH SERVERS-LIST)))))
        (SI::WHO-LINE-JUST-COLD-BOOTED-P
         (UNLESS (or (EQ WHO-LINE-ITEM-STATE 'COLD)
                     si:*cold-booting*)
           (SHEET-CLEAR SELF)
           (SETQ WHO-LINE-ITEM-STATE 'COLD)
           (SETQ STRING (FORMAT NIL "~A cold-booted" SI::LOCAL-PRETTY-HOST-NAME))
           (SEND SELF :STRING-OUT STRING)))
        ((> (SETQ IDLE (TRUNCATE (TIME-DIFFERENCE (TIME) TV:KBD-LAST-ACTIVITY-TIME) 3600.)) 4)
         ;; Display keyboard idle time
         (LET ((OLD-IDLE WHO-LINE-ITEM-STATE))
           (WHEN (OR (NOT (NUMBERP OLD-IDLE)) ( OLD-IDLE IDLE))
             (SHEET-CLEAR SELF)
             (WITHOUT-INTERRUPTS
               (LET ((STRING (MAKE-IDLE-MESSAGE IDLE)))
                 (SEND SELF :STRING-OUT STRING)
                 (SETQ STRING NIL)))
             (SETQ WHO-LINE-ITEM-STATE IDLE))))
        ((NEQ WHO-LINE-ITEM-STATE 'NULL)
         (SHEET-CLEAR SELF)
         (SETQ WHO-LINE-ITEM-STATE 'NULL))))

(DEFCONST DISPLAY-FILE-TRANSFER-COUNT-STRING (MAKE-STRING 20. :FILL-POINTER 0))

(DEFCONST DISPLAY-FILE-TRANSFER-PERCENT-STRING (MAKE-STRING 5 :FILL-POINTER 0))

(DEFVAR LAST-WHOLINE-PATHNAME NIL
  "The last pathname displayed in the who line.")
(DEFVAR LAST-WHOLINE-PATHNAME-STRING NIL
  "The string we displayed for LAST-WHOLINE-PATHNAME.")
(DEFVAR LAST-WHOLINE-PATHNAME-LENGTH NIL
  "The length we requested, when we obtained LAST-WHOLINE-PATHNAME-STRING.")

;;; Display the who-line-information onto SHEET.  PERCENT may be NIL,
;;; but COUNT is always a fixnum.  DIRECTION is one of the keywords
;;; :INPUT, :OUTPUT, or :BIDIRECTIONAL.  MAX-CHARS is the maximum
;;; number of characters that we may output.
(DEFUN DISPLAY-FILE-TRANSFER (SHEET PATHNAME DIRECTION COUNT PERCENT MAX-CHARS)
  (SEND SHEET :STRING-OUT (CASE DIRECTION
                            (:INPUT " ")
                            (:OUTPUT " ")
                            (:BIDIRECTIONAL " ")
                            (T "? ")))
  (LET* ((FILE-NAME (SEND PATHNAME :STRING-FOR-PRINTING))
         (FILE-NAME-LENGTH (STRING-LENGTH FILE-NAME))
         (FILE-NAME-LIMIT NIL)
         (COUNT-STRING-LENGTH)
         (PERCENT-STRING-LENGTH)
         (DISPLAY-COUNT-P NIL)
         (DISPLAY-PERCENT-P NIL))
    (FIXNUM-INTO-STRING COUNT DISPLAY-FILE-TRANSFER-COUNT-STRING)
    (SETQ COUNT-STRING-LENGTH (LENGTH DISPLAY-FILE-TRANSFER-COUNT-STRING))
    (COND ((NULL PERCENT)
           (SETQ DISPLAY-PERCENT-P NIL DISPLAY-COUNT-P T)
           ;; 4 is two for the direction and two for the spaces after the file name.
           (IF ( (+ FILE-NAME-LENGTH COUNT-STRING-LENGTH 4) MAX-CHARS)
               (SETQ FILE-NAME-LIMIT (- MAX-CHARS 4 COUNT-STRING-LENGTH))))
          (T
           (FIXNUM-INTO-STRING PERCENT DISPLAY-FILE-TRANSFER-PERCENT-STRING)
           (VECTOR-PUSH #/% DISPLAY-FILE-TRANSFER-PERCENT-STRING)
           (SETQ PERCENT-STRING-LENGTH
                 (LENGTH DISPLAY-FILE-TRANSFER-PERCENT-STRING))
           (SETQ DISPLAY-PERCENT-P T)
           (COND (( (+ FILE-NAME-LENGTH COUNT-STRING-LENGTH PERCENT-STRING-LENGTH 5)
                                        ;5 is the above 4 plus 1 space between percent & count
                     MAX-CHARS)
                  (SETQ DISPLAY-COUNT-P T))
                 ((> (+ FILE-NAME-LENGTH PERCENT-STRING-LENGTH 4) MAX-CHARS)
                  (SETQ FILE-NAME-LIMIT (- MAX-CHARS PERCENT-STRING-LENGTH 4))))))
    (WHEN FILE-NAME-LIMIT
      (IF (AND (EQ PATHNAME LAST-WHOLINE-PATHNAME)
               (= FILE-NAME-LIMIT LAST-WHOLINE-PATHNAME-LENGTH))
          (SETQ FILE-NAME LAST-WHOLINE-PATHNAME-STRING)
        (SETQ FILE-NAME (SEND PATHNAME :STRING-FOR-WHOLINE FILE-NAME-LIMIT))
        (SETQ LAST-WHOLINE-PATHNAME-LENGTH FILE-NAME-LIMIT)
        (SETQ LAST-WHOLINE-PATHNAME-STRING FILE-NAME)
        (SETQ LAST-WHOLINE-PATHNAME PATHNAME))
      (AND (= FILE-NAME-LIMIT (LENGTH FILE-NAME))
           (SETQ FILE-NAME-LIMIT NIL)))
    (SHEET-CLEAR-EOL SHEET)
    (SEND SHEET :STRING-OUT FILE-NAME 0
          (IF FILE-NAME-LIMIT (MIN FILE-NAME-LIMIT (LENGTH FILE-NAME))))
    (SEND SHEET :STRING-OUT (IF FILE-NAME-LIMIT "  " "  "))
    (WHEN DISPLAY-PERCENT-P
      (SEND SHEET :STRING-OUT DISPLAY-FILE-TRANSFER-PERCENT-STRING)
      (IF DISPLAY-COUNT-P (SEND SHEET :WRITE-CHAR #/SPACE)))
    (WHEN DISPLAY-COUNT-P
      (SEND SHEET :STRING-OUT DISPLAY-FILE-TRANSFER-COUNT-STRING))))

(DEFUN FIXNUM-INTO-STRING (NUMBER STRING &OPTIONAL (RADIX 10.))
  "Store a printout of NUMBER in RADIX into STRING.
STRING's contents are altered.  STRING is made longer if necessary."
  (SETF (FILL-POINTER STRING) 0)
  (DO ((NUM NUMBER (TRUNCATE NUM RADIX)))
      ((AND (ZEROP NUM) (NOT (ZEROP (LENGTH STRING))))
       (STRING-NREVERSE STRING))
    ;; Keep trying to push until we make array big enough to hold more.
    (DO () ((VECTOR-PUSH (INT-CHAR (+ (CHAR-INT #/0) (CL:REM NUM RADIX))) STRING))
      (ADJUST-ARRAY-SIZE STRING (+ 10. (ARRAY-LENGTH STRING))))))

(DEFUN MAKE-IDLE-MESSAGE (MINUTES)
  (let ((tail (cond ((< MINUTES 60.)
                     (FORMAT NIL "idle ~D minute~:P"
                             MINUTES))
                    (t
                     (MULTIPLE-VALUE-BIND (HOURS MINUTES)
                         (TRUNCATE MINUTES 60.)
                       (FORMAT NIL "idle ~D hr ~D min~:P"
                               HOURS MINUTES))))))
    (let ((whole-thing (format nil "~A's console ~A" si:local-pretty-host-name tail))
          (who-line-length (send tv:who-line-file-state-sheet :size-in-characters))) ; apparently
                                                                                     ; NIL if in cold load
      (cond ((and (numberp who-line-length)
                  (< (string-length whole-thing) who-line-length))
             whole-thing)
            (t
             (format nil "Console ~A" tail))))))

;;;; Date and time in the who-line, continuously updating.

(DEFUN NWATCH-WHO-FUNCTION (WHO-SHEET &AUX LEFTX)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (OR WHO-LINE-EXTRA-STATE
      (LET ((DEFAULT-CONS-AREA WHO-LINE-AREA))
        (SETQ WHO-LINE-EXTRA-STATE (STRING-APPEND "MM//DD//YY HH:MM:SS"))))
                                                  ; Errgghhh! Krazy Backwards Amerikan dates.
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:GET-TIME)
    (COND ((NULL SECONDS)
           (SHEET-CLEAR WHO-SHEET)
           (COPY-ARRAY-CONTENTS "MM//DD//YY HH:MM:SS" WHO-LINE-EXTRA-STATE))
          (T
           (SETQ YEAR (MOD YEAR 100.))
           (SETQ LEFTX (MIN (NWATCH-N MONTH WHO-LINE-EXTRA-STATE 0)
                            (NWATCH-N DAY WHO-LINE-EXTRA-STATE 3)
                            (NWATCH-N YEAR WHO-LINE-EXTRA-STATE 6)
                            (NWATCH-N HOURS WHO-LINE-EXTRA-STATE 9)
                            (NWATCH-N MINUTES WHO-LINE-EXTRA-STATE 12.)
                            (NWATCH-N SECONDS WHO-LINE-EXTRA-STATE 15.)))
           (UNLESS WHO-LINE-ITEM-STATE (SETQ LEFTX 0))  ;was clobbered, redisplay all
           (SHEET-SET-CURSORPOS WHO-SHEET (* LEFTX CHAR-WIDTH) 0)
           (SHEET-CLEAR-EOL WHO-SHEET)
           (SHEET-STRING-OUT WHO-SHEET WHO-LINE-EXTRA-STATE LEFTX)
           (SETQ WHO-LINE-ITEM-STATE T)))))

;;; Returns first character position changed
(DEFUN NWATCH-N (N STR I)
  (LET ((DIG1 (INT-CHAR (+ (TRUNCATE N 10.) (CHAR-INT #/0))))
        (DIG2 (INT-CHAR (+ (CL:REM N 10.) (CHAR-INT #/0)))))
    (PROG1 (COND ((NEQ (CHAR STR I) DIG1) I)
                 ((NEQ (CHAR STR (1+ I)) DIG2) (1+ I))
                 (T (ARRAY-LENGTH STR)))
           (SETF (CHAR STR I) DIG1)
           (SETF (CHAR STR (1+ I)) DIG2))))

;;;; Support for documentation in the who line

(DEFMETHOD (SHEET :WHO-LINE-DOCUMENTATION-STRING) ()
  NIL)

(DEFUN WHO-LINE-DOCUMENTATION (&OPTIONAL (ON-P T))
  "Turn display of the mouse documentation line on or off."
  (COND ((AND ON-P (NOT (SHEET-EXPOSED-P WHO-LINE-DOCUMENTATION-WINDOW)))
         (SET-WHO-LINE-LINES (1+ (TRUNCATE (SHEET-INSIDE-HEIGHT WHO-LINE-SCREEN)
                                           (SHEET-LINE-HEIGHT WHO-LINE-SCREEN))))
         (SEND WHO-LINE-DOCUMENTATION-WINDOW :DEACTIVATE)
         (DOLIST (I (COPY-LIST (SHEET-INFERIORS WHO-LINE-SCREEN)))
           (AND ( (SHEET-Y-OFFSET I) (SHEET-Y-OFFSET WHO-LINE-DOCUMENTATION-WINDOW))
                (SEND I :SET-POSITION
                        (SHEET-X-OFFSET I)
                        (+ (SHEET-Y-OFFSET I)
                           (SHEET-Y-OFFSET WHO-LINE-DOCUMENTATION-WINDOW)
                           (SHEET-HEIGHT WHO-LINE-DOCUMENTATION-WINDOW)))))
         (SEND WHO-LINE-DOCUMENTATION-WINDOW :EXPOSE))
        ((AND (NOT ON-P) WHO-LINE-DOCUMENTATION-WINDOW)
         (COND ((SHEET-EXPOSED-P WHO-LINE-DOCUMENTATION-WINDOW)
                (SEND WHO-LINE-DOCUMENTATION-WINDOW :DEACTIVATE)
                (SET-WHO-LINE-LINES (1- (TRUNCATE (SHEET-INSIDE-HEIGHT WHO-LINE-SCREEN)
                                                  (SHEET-LINE-HEIGHT WHO-LINE-SCREEN))))
                (DOLIST (I (COPY-LIST (SHEET-INFERIORS WHO-LINE-SCREEN)))
                  (AND ( (SHEET-Y-OFFSET I) (SHEET-Y-OFFSET WHO-LINE-DOCUMENTATION-WINDOW))
                       (SEND I :SET-POSITION
                               (SHEET-X-OFFSET I)
                               (- (SHEET-Y-OFFSET I)
                                  (SHEET-Y-OFFSET WHO-LINE-DOCUMENTATION-WINDOW)
                                  (SHEET-HEIGHT WHO-LINE-DOCUMENTATION-WINDOW))))))))))

(DEFUN SET-WHO-LINE-LINES (N-LINES)
  "Reconfigure the screen so that the who line has N-LINES lines."
  (WITH-MOUSE-USURPED
   (LOCK-SHEET (MAIN-SCREEN)
    (LOCK-SHEET (WHO-LINE-SCREEN)
      (WITHOUT-INTERRUPTS
        (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
          (AND (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN)
               (SETQ MOUSE-SHEET NIL))
          (FUNCALL WHO-LINE-SCREEN :DEEXPOSE)
          (FUNCALL MAIN-SCREEN :DEEXPOSE)
;         (FUNCALL WHO-LINE-SCREEN :SET-VSP (IF (= N-LINES 1) 0 2))
          (FUNCALL WHO-LINE-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                   :BOTTOM MAIN-SCREEN-HEIGHT
                   :TOP (- MAIN-SCREEN-HEIGHT
                            (* N-LINES (SHEET-LINE-HEIGHT WHO-LINE-SCREEN))))
          (FUNCALL MAIN-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                   :HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
          (FUNCALL WHO-LINE-SCREEN :EXPOSE)
          (FUNCALL MAIN-SCREEN :EXPOSE)
          (AND SW (FUNCALL SW :SELECT))
          (SETQ MOUSE-SHEET MS)))))))

;;; List of windows waiting for locks to print error notifications.
;;; DEFVAR in SHEET.
(PROCLAIM '(SPECIAL LOCKED-ERROR-WINDOWS))
(PROCLAIM '(SPECIAL PENDING-NOTIFICATIONS))

(DEFUN WHO-LINE-DOCUMENTATION-FUNCTION (WHO-SHEET)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (LET* ((W MOUSE-WINDOW)
         (NEW-STATE
           (COND (LOCKED-ERROR-WINDOWS
                  ;; To attract attention, make this message blink.
                  (LET ((MSG
                          "*** Background error with window locked; try Terminal Control-Clear-Input or Terminal Call ***")
                        (MSG1
                          "    Background error with window locked; try Terminal Control-Clear-Input or Terminal Call"))
                    (IF (EQ WHO-LINE-ITEM-STATE MSG)
                        MSG1 MSG)))
                 (*WHO-LINE-DOCUMENTATION-OVERRIDE*)
;                (PENDING-NOTIFICATIONS
;                 (LET ((MSG
;                         "***** Notifications are pending.  Terminal N is one way to see them. *****")
;                       (MSG1
;                         "      Notifications are pending.  Terminal N is one way to see them."))
;                   (IF (EQ WHO-LINE-ITEM-STATE MSG)
;                       MSG1 MSG)))
                 ((SYMBOLP W)
                  (AND W WHO-LINE-MOUSE-GRABBED-DOCUMENTATION))
                 (T
                  (MULTIPLE-VALUE-BIND (DOC ERROR)
                      (CATCH-ERROR (SEND W :WHO-LINE-DOCUMENTATION-STRING)
                                   NIL)
                    (IF ERROR "Error getting Who-Line documentation string" DOC))))))
    (unless (stringp new-state)
      (setq new-state "Click right to get the System Menu."))
    (WHEN (NEQ WHO-LINE-ITEM-STATE NEW-STATE)
      (SETQ WHO-LINE-ITEM-STATE NEW-STATE)
      (SHEET-CLEAR WHO-SHEET)
      (SHEET-STRING-OUT WHO-SHEET NEW-STATE
                        0 (MIN (OR (STRING-SEARCH-CHAR #/NEWLINE NEW-STATE)
                                   (LENGTH NEW-STATE))
                               (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
                                         (SHEET-CHAR-WIDTH WHO-SHEET)))))))

(DEFUN ADD-WHO-LINE-WINDOW (WINDOW)
  "Takes a window that must be an immediate inferior of the who line screen, and
exposes it at the top of the who-line, making the who-line larger if necessary."
  (OR (EQ (SEND WINDOW :SUPERIOR) WHO-LINE-SCREEN)
      (FERROR "~A is not an immediate inferior of the who line screen" WINDOW))
  (COND ((NOT (SHEET-EXPOSED-P WINDOW))
         (LET ((H (SHEET-HEIGHT WINDOW)))
           (SET-WHO-LINE-HEIGHT (+ H (SHEET-HEIGHT WHO-LINE-SCREEN)))
           (DOLIST (W (COPY-LIST (SHEET-EXPOSED-INFERIORS WHO-LINE-SCREEN)))
             (SEND W :SET-POSITION (SHEET-X-OFFSET W) (+ H (SHEET-Y-OFFSET W))))
           (SEND WINDOW :SET-POSITION (SHEET-X-OFFSET WINDOW) 0)
           (SEND WINDOW :EXPOSE)))))

(DEFUN DELETE-WHO-LINE-WINDOW (WINDOW)
  "Removes WINDOW from display in the who line.
WINDOW must be an inferior of WHO-LINE-SCREEN."
  (OR (EQ (SEND WINDOW :SUPERIOR) WHO-LINE-SCREEN)
      (FERROR "~A is not an immediate inferior of the who line screen" WINDOW))
  (COND ((SHEET-EXPOSED-P WINDOW)
         (SEND WINDOW :DEACTIVATE)
         (LET ((H (SHEET-HEIGHT WINDOW)))
           (DOLIST (W (COPY-LIST (SHEET-EXPOSED-INFERIORS WHO-LINE-SCREEN)))
             (SEND W :SET-POSITION (SHEET-X-OFFSET W) (- (SHEET-Y-OFFSET W) H)))
           (SET-WHO-LINE-HEIGHT (- (SHEET-HEIGHT WHO-LINE-SCREEN) H))))))

(DEFUN SET-WHO-LINE-HEIGHT (H)
  "Set height of WHO-LINE-SCREEN to H lines, updating MAIN-SCREEN as well."
  (WITH-MOUSE-USURPED
   (LOCK-SHEET (MAIN-SCREEN)
    (LOCK-SHEET (WHO-LINE-SCREEN)
      (WITHOUT-INTERRUPTS
        (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
          (AND (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN)
               (SETQ MOUSE-SHEET NIL))
          (SEND WHO-LINE-SCREEN :DEEXPOSE)
          (SEND MAIN-SCREEN :DEEXPOSE)
          (SETQ MOUSE-SHEET MS)
          (SEND WHO-LINE-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                                :BOTTOM MAIN-SCREEN-HEIGHT
                                :TOP (- MAIN-SCREEN-HEIGHT H))
          (SEND MAIN-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                            :HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
          (MOUSE-SET-SHEET MS)
          (SEND MAIN-SCREEN :EXPOSE)
          (SEND WHO-LINE-SCREEN :EXPOSE)
          (AND SW (SEND SW :SELECT))))))))

(DEFFLAVOR WHO-LINE-WINDOW () (WHO-LINE-MIXIN WINDOW))

(DEFMETHOD (WHO-LINE-WINDOW :UPDATE) ()
  )

(DEFUN MAKE-WHO-LINE-WINDOW (&REST ARGS)
  "Create a window to be part of the who line.
ARGS are keyword args passed to MAKE-WINDOW.
The keyword :FLAVOR specifies the window flavor
 (default is TV:WHO-LINE-WINDOW).
The window's superior is always WHO-LINE-SCREEN."
  (APPLY #'MAKE-INSTANCE (GETF ARGS ':FLAVOR 'WHO-LINE-WINDOW)
         :AREA WHO-LINE-AREA
         :SUPERIOR WHO-LINE-SCREEN
         ARGS))

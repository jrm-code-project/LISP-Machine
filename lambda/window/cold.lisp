;;; -*- Mode:LISP; Package:SI; Cold-Load:T; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains those portions of the window system that need
;;; to be in the cold-load, including the basic keyboard software and
;;; the cold load stream.

;;; Note that this file has to be in the SYSTEM-INTERNALS (SI) package
;;; rather than TV because it is part of the cold-load.

;compare these with SI:PROCESSOR-TYPE-CODE to conditionalize code for a specific machine.
(DEFCONSTANT CADR-TYPE-CODE 1
  "The value which SI:PROCESSOR-TYPE-CODE has when you run on a CADR.")
(DEFCONSTANT LAMBDA-TYPE-CODE 2
  "The value which SI:PROCESSOR-TYPE-CODE has when you run on a LAMBDA.")
(DEFCONSTANT EXPLORER-TYPE-CODE 3
  "The value which SI:PROCESSOR-TYPE-CODE has when you run on an Explorer.")
(DEFCONSTANT FALCON-TYPE-CODE 4
  "The value which SI:PROCESSOR-TYPE-CODE has when you run on an Falcon.")

(DEFVAR TV:MORE-PROCESSING-GLOBAL-ENABLE T
  "NIL turns off **MORE**-processing on all windows.")

(DEFVAR TV::DEFAULT-BACKGROUND-STREAM 'TV::BACKGROUND-STREAM)
(DEFPARAMETER TV::WHO-LINE-RUN-LIGHT-LOC #o51765
  "Where the run-light goes, in Xbus I//O space")
(DEFVAR TV::KBD-LAST-ACTIVITY-TIME 0
  "Time user last typed a key or clicked mouse.")

;;;magic BOOLE numbers
(DEFCONSTANT TV:ALU-SETA 5 "Alu function for copying bits to destination.")
(DEFCONSTANT TV:ALU-XOR 6 "Alu function for flipping bits in destination.")
(DEFCONSTANT TV:ALU-ANDCA 2 "Alu function for clearing bits in destination.")
(DEFCONSTANT TV:ALU-IOR 7 "Alu function for setting bits in destination.")
(DEFCONSTANT TV:ALU-SETZ 0 "Alu function for setting bits in destination to zero.")
(DEFCONSTANT TV:ALU-AND 1 "Alu function for anding.")

;;; Call this when the state of a process may have changed.
;;; In the cold-load because called by process stuff, loaded before window stuff.
(DEFUN TV::WHO-LINE-PROCESS-CHANGE (PROC)
  (AND (FBOUNDP 'TV::WHO-LINE-RUN-STATE-UPDATE) (EQ PROC TV::LAST-WHO-LINE-PROCESS)
       (TV::WHO-LINE-RUN-STATE-UPDATE)))

;;; Copy from SYS2;FLAVOR, needed for setting up the sync program
(DEFSUBST SYMEVAL-IN-INSTANCE (INSTANCE PTR)
  "Return the value of instance variable PTR in INSTANCE.
PTR can be a pointer to a value cell instead of a symbol.
Error if PTR does not work out to be a symbol which is an
instance variable in INSTANCE."
  (CONTENTS (LOCATE-IN-INSTANCE INSTANCE PTR)))

;;; Macros for cold-load construction

(DEFMACRO DEFINSTANCE-IMMEDIATE (NAME . INSTANCE-VARIABLES)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES")))
        (METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS"))))
    (SET METHOD-LIST-NAME NIL)
    (SET INSTANCE-VARIABLE-LIST-NAME INSTANCE-VARIABLES)
    `(PROGN 'COMPILE
            (SETQ ,INSTANCE-VARIABLE-LIST-NAME ',INSTANCE-VARIABLES)
            (SETQ ,METHOD-LIST-NAME NIL)
            (DEFPROP ,NAME T SI:FLAVOR)         ;This makes M-. work
            (DEFVAR ,NAME))))

(DEFMACRO DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES ((NAME) . BODY)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES"))))
    `(LOCAL-DECLARE ((SPECIAL . ,(SYMEVAL INSTANCE-VARIABLE-LIST-NAME)))
       . ,BODY)))
(EVAL-WHEN (COMPILE LOAD)
  (DEFPROP DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES T MAY-SURROUND-DEFUN))

(DEFMACRO DEFMETHOD-IMMEDIATE ((NAME MESSAGE) ARGLIST . BODY)
  (LET ((METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS")))
        (METHOD-NAME (INTERN (STRING-APPEND NAME #/- MESSAGE "-METHOD"))))
    (OR (MEMQ MESSAGE (SYMEVAL METHOD-LIST-NAME))
        (PUSH MESSAGE (SYMEVAL METHOD-LIST-NAME)))
    `(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (,NAME)
     (DEFUN ,METHOD-NAME (IGNORE . ,ARGLIST)
       . ,BODY))))

(DEFMACRO MAKE-INSTANCE-IMMEDIATE (NAME INIT-PLIST-GENERATOR)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES")))
        (METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS")))
        (SEND-IF-HANDLES-METHOD-NAME
          (INTERN (STRING-APPEND NAME "-SEND-IF-HANDLES")))
        (OPERATION-HANDLED-P-METHOD-NAME
          (INTERN (STRING-APPEND NAME "-OPERATION-HANDLED-P")))
        (GET-HANDLER-FOR-METHOD-NAME
          (INTERN (STRING-APPEND NAME "-GET-HANDLER-FOR")))
        (METHOD-LIST 'SI:UNCLAIMED-MESSAGE))
    (DOLIST (MESSAGE (SYMEVAL METHOD-LIST-NAME))
      (LET ((METHOD-NAME (INTERN (STRING-APPEND NAME #/- MESSAGE "-METHOD"))))
        (PUSH (CONS MESSAGE METHOD-NAME) METHOD-LIST)))
    (PUSH (CONS ':WHICH-OPERATIONS METHOD-LIST-NAME) METHOD-LIST)
    (PUSH (CONS ':SEND-IF-HANDLES SEND-IF-HANDLES-METHOD-NAME) METHOD-LIST)
    (PUSH (CONS ':OPERATION-HANDLED-P OPERATION-HANDLED-P-METHOD-NAME) METHOD-LIST)
    (PUSH (CONS ':GET-HANDLER-FOR GET-HANDLER-FOR-METHOD-NAME) METHOD-LIST)
    `(PROGN 'COMPILE
       (DEFUN ,SEND-IF-HANDLES-METHOD-NAME (IGNORE OPERATION &REST ARGS)
         (IF (MEMQ OPERATION (,METHOD-LIST-NAME NIL))
             (LEXPR-SEND SELF OPERATION ARGS)))
       (DEFUN ,OPERATION-HANDLED-P-METHOD-NAME (IGNORE OPERATION)
         (MEMQ OPERATION (,METHOD-LIST-NAME NIL)))
       (DEFUN ,GET-HANDLER-FOR-METHOD-NAME (IGNORE OPERATION)
         (IF (MEMQ OPERATION (,METHOD-LIST-NAME NIL))
             (INTERN (STRING-APPEND ',NAME "-" OPERATION)
                     ',(PKG-NAME PACKAGE))))
       (DEFUN ,METHOD-LIST-NAME (IGNORE)
         ',(SYMEVAL METHOD-LIST-NAME))
       (SETQ ,NAME (FAKE-UP-INSTANCE ',NAME ',(SYMEVAL INSTANCE-VARIABLE-LIST-NAME)
                                     ',METHOD-LIST ',INIT-PLIST-GENERATOR)))))

(DEFINSTANCE-IMMEDIATE COLD-LOAD-STREAM
  ARRAY                                         ;The array into which bits go
  LOCATIONS-PER-LINE                            ;Number of words in a screen line
  HEIGHT                                        ;Height of screen
  WIDTH                                         ;Width of screen
  CURSOR-X                                      ;Current x position
  CURSOR-Y                                      ;Current y position
  FONT                                          ;The one and only font
  CHAR-WIDTH                                    ;Width of a character
  LINE-HEIGHT                                   ;Height of line, including vsp
  BUFFER                                        ;The hardward buffer location
  TV::CONTROL-ADDRESS                           ;Hardware controller address
  UNRCHF                                        ;For :UNTYI
  RUBOUT-HANDLER-BUFFER                         ;For :RUBOUT-HANDLER
  )

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :PRINT-SELF) (STREAM &REST IGNORE)
  (FORMAT STREAM "#<~A ~O>" (TYPE-OF SELF) (%POINTER SELF)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :DESCRIBE) ()
  (DECLARE (SPECIAL COLD-LOAD-STREAM-INSTANCE-VARIABLES))
  (FORMAT *STANDARD-OUTPUT* "~&#<~A ~O> is the cold-load stream."
          (TYPE-OF SELF) (%POINTER SELF))
  (DO ((V COLD-LOAD-STREAM-INSTANCE-VARIABLES (CDR V))
       (OFFSET 1 (1+ OFFSET)))
      ((NULL V))
    (FORMAT T "~&~S:~30T~S" (CAR V)
            (IF (NOT (EQ (%P-LDB-OFFSET %%Q-DATA-TYPE SELF OFFSET) DTP-NULL))
                (%P-CONTENTS-OFFSET SELF OFFSET)
              "void"))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :INIT) (PLIST)
  (UNLESS (BOUNDP 'TV:DEFAULT-SCREEN)
    (SETQ TV:DEFAULT-SCREEN SELF))
  (SETQ CURSOR-X 0 CURSOR-Y 0
        FONT (OR (GET PLIST ':FONT) FONTS:CPTFONT)
        UNRCHF NIL
        WIDTH (GET PLIST ':WIDTH)
        HEIGHT (GET PLIST ':HEIGHT)
        BUFFER (GET PLIST ':BUFFER)
        TV::CONTROL-ADDRESS (GET PLIST ':CONTROL-ADDRESS)
        LOCATIONS-PER-LINE (GET PLIST ':LOCATIONS-PER-LINE)
        CHAR-WIDTH (FONT-CHAR-WIDTH FONT)
        LINE-HEIGHT (+ 2 (FONT-CHAR-HEIGHT FONT))
        RUBOUT-HANDLER-BUFFER (MAKE-STRING #o1000 :LEADER-LIST '(0 0 NIL)))
        ;; Leader elements are fill-pointer, scan-pointer, status.
  (SETQ ARRAY (MAKE-ARRAY (LIST (* LOCATIONS-PER-LINE 32.) HEIGHT)
                          :TYPE 'ART-1B :DISPLACED-TO BUFFER)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :READ-CURSORPOS) (&OPTIONAL (UNITS ':PIXEL)
                                               &AUX (X CURSOR-X) (Y CURSOR-Y))
  (IF (EQ UNITS ':CHARACTER)
      (VALUES (CEILING X CHAR-WIDTH)
              (CEILING Y LINE-HEIGHT))
    (VALUES X Y)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :SET-CURSORPOS) (X Y &OPTIONAL (UNITS ':PIXEL))
  (AND (NUMBERP UNITS)                          ;***CROCK***, flush when format fixed
       (PSETQ UNITS X X Y Y UNITS))
  (AND (EQ UNITS ':CHARACTER)
       (SETQ X (* X CHAR-WIDTH)
             Y (* Y LINE-HEIGHT)))
  (SETQ CURSOR-X (MAX 0 (MIN WIDTH X))
        CURSOR-Y (MAX 0 (MIN (- HEIGHT LINE-HEIGHT) Y))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :HOME-CURSOR) ()
  (SETQ CURSOR-X 0 CURSOR-Y 0))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :HANDLE-EXCEPTIONS) ())

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYO) (CH)
  (LET ((CURRENTLY-PREPARED-SHEET SELF))
    (COND ((< CH #o200)
           (LET ((CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
                 (FIT-ENTRY (FONT-INDEXING-TABLE FONT))
                 (DELTA-X))
             (SETQ DELTA-X (IF CHAR-WIDTHS (AREF CHAR-WIDTHS CH) (FONT-CHAR-WIDTH FONT)))
             (AND (> (+ CURSOR-X DELTA-X) WIDTH)        ;End of line exception
                  (SEND SELF ':TYO #/RETURN))
             (IF (NULL FIT-ENTRY)
                 (%DRAW-CHAR FONT CH CURSOR-X CURSOR-Y TV:ALU-IOR SELF)
                 (DO ((CH (AREF FIT-ENTRY CH) (1+ CH))
                      (LIM (AREF FIT-ENTRY (1+ CH)))
                      (XPOS CURSOR-X (+ XPOS (FONT-RASTER-WIDTH FONT))))
                     ((= CH LIM))
                   (%DRAW-CHAR FONT CH XPOS CURSOR-Y TV:ALU-IOR SELF)))
             (SETQ CURSOR-X (+ CURSOR-X DELTA-X))))
          ((= CH #/RETURN)
           (SETQ CURSOR-X 0
                 CURSOR-Y (+ CURSOR-Y LINE-HEIGHT))
           (COND (( (+ CURSOR-Y LINE-HEIGHT) HEIGHT)   ;End-of-page exception
                  (SETQ CURSOR-Y 0))
                 (( (+ CURSOR-Y (* 2 LINE-HEIGHT)) HEIGHT)     ;MORE exception
                  (SEND SELF :CLEAR-REST-OF-LINE)       ;In case wholine is there
                  (WHEN TV:MORE-PROCESSING-GLOBAL-ENABLE
                    (SEND SELF :STRING-OUT "**MORE**")
                    (SEND SELF :TYI)
                    (SETQ CURSOR-X 0)
                    (SEND SELF :CLEAR-REST-OF-LINE))
                  (SETQ CURSOR-Y 0)))
           (SEND SELF :CLEAR-REST-OF-LINE))
          ((= CH #/TAB)
           (DOTIMES (I (- 8 (\ (TRUNCATE CURSOR-X CHAR-WIDTH) 8)))
             (SEND SELF ':TYO #/SPACE)))
          ((AND (< CH 240) (BOUNDP 'FONTS:5X5))
           ;; This won't work in the initial cold-load environment, hopefully no one
           ;; will touch those keys then, but if they do we just type nothing.
           ;; This code is like SHEET-DISPLAY-LOZENGED-STRING
           (LET* ((CHNAME (GET-PNAME (CAR (RASSOC CH XR-SPECIAL-CHARACTER-NAMES))))
                  (CHWIDTH (+ (* (ARRAY-ACTIVE-LENGTH CHNAME) 6) 10.)))
             (AND (> (+ CURSOR-X CHWIDTH) WIDTH)        ;Won't fit on line
                  (SEND SELF ':TYO #/CR))
             ;; Put the string then the box around it
             (LET ((X0 CURSOR-X)
                   (Y0 (1+ CURSOR-Y))
                   (X1 (+ CURSOR-X (1- CHWIDTH)))
                   (Y1 (+ CURSOR-Y 9)))
               (DO ((X (+ X0 5) (+ X 6))
                    (I 0 (1+ I))
                    (N (ARRAY-ACTIVE-LENGTH CHNAME)))
                   (( I N))
                 (%DRAW-CHAR FONTS:5X5 (AREF CHNAME I) X (+ Y0 2) TV:ALU-IOR SELF))
               (%DRAW-RECTANGLE (- CHWIDTH 8) 1 (+ X0 4) Y0 TV:ALU-IOR SELF)
               (%DRAW-RECTANGLE (- CHWIDTH 8) 1 (+ X0 4) Y1 TV:ALU-IOR SELF)
               (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) TV:ALU-IOR T SELF)
               (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) TV:ALU-IOR T SELF)
               (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) TV:ALU-IOR T SELF)
               (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) TV:ALU-IOR T SELF)
               (SETQ CURSOR-X (1+ X1))))))
    CH))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :WRITE-CHAR) (CHAR)
  (SEND SELF :TYO (CHAR-INT CHAR)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-REST-OF-LINE) ()
  (LET ((CURRENTLY-PREPARED-SHEET SELF))
    (%DRAW-RECTANGLE (- WIDTH CURSOR-X) LINE-HEIGHT CURSOR-X CURSOR-Y TV:ALU-ANDCA SELF)))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-EOL) ()
  (SEND SELF :CLEAR-REST-OF-LINE))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-WINDOW) ()
  (SETQ CURSOR-X 0 CURSOR-Y 0)
  (LET ((CURRENTLY-PREPARED-SHEET SELF))
    (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 TV:ALU-ANDCA SELF)))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-SCREEN) ()
  (SEND SELF :CLEAR-WINDOW))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :FRESH-LINE) ()
  (IF (ZEROP CURSOR-X)
      (SEND SELF :CLEAR-REST-OF-LINE)
    (SEND SELF :TYO #/RETURN)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (DO ((I START (1+ I))
       (END (OR END (ARRAY-ACTIVE-LENGTH STRING))))
      (( I END))
    (SEND SELF :TYO (AREF STRING I))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :LINE-OUT) (STRING &OPTIONAL (START 0) END)
  (SEND SELF :STRING-OUT STRING START END)
  (SEND SELF :TYO #/RETURN))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :UNTYI) (CH)
  (IF (EQ RUBOUT-HANDLER SELF)
      (DECF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1))
    (SETQ UNRCHF CH)))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :UNREAD-CHAR) (CH)
  (SEND SELF :UNTYI (CHAR-INT CH)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :LISTEN) ()
  (OR UNRCHF
      (DO () ((NOT (KBD-HARDWARE-CHAR-AVAILABLE)) NIL)
        (AND (SETQ UNRCHF (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR)))
             (RETURN T)))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :ANY-TYI) (&OPTIONAL IGNORE)
  (SEND SELF :TYI))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :ANY-TYI-NO-HANG) ()
  (SEND SELF :TYI-NO-HANG))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :READ-CHAR) ()
  (INT-CHAR (SEND SELF :TYI)))
(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :READ-CHAR-NO-HANG) ()
  (LET ((TEM (SEND SELF :TYI-NO-HANG)))
    (IF (FIXNUMP TEM) (INT-CHAR TEM) TEM)))


(DEFUN WINDOW-SYSTEM-LOADED-P ()
  (GET 'TV:WINDOW 'FLAVOR))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYI) (&OPTIONAL IGNORE
                                                        &AUX IDX (INHIBIT-SCHEDULING-FLAG
                                                                   (WINDOW-SYSTEM-LOADED-P)))
  (COND ((NEQ RUBOUT-HANDLER SELF)
         (IF UNRCHF
             (PROG1 UNRCHF (SETQ UNRCHF NIL))
           (DO-FOREVER
             (COLD-LOAD-STREAM-WAIT-FOR-CHAR)
             (LET ((CHAR (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))
               (SELECTQ CHAR
                 (NIL)                          ;Unreal character
                 (#/BREAK (BREAK "BREAK"))
                 ;; Kludge to make the debugger more usable in the cold-load stream
                 (#/ABORT (IF EH:READING-COMMAND (RETURN CHAR)
                            (SIGNAL EH:ABORT-OBJECT)))
                 (OTHERWISE (RETURN CHAR)))))))
        ((> (FILL-POINTER RUBOUT-HANDLER-BUFFER)
            (SETQ IDX (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1)))
         (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) (1+ IDX))
         (AREF RUBOUT-HANDLER-BUFFER IDX))
        (T
         (COLD-LOAD-STREAM-RUBOUT-HANDLER))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYI-NO-HANG) ()
  (AND (SEND SELF ':LISTEN)
       (SEND SELF ':TYI)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :LINE-IN) (&OPTIONAL ARG1)
  (LET ((BUF (MAKE-STRING #o100 :LEADER-LENGTH (IF (NUMBERP ARG1) ARG1 1))))
    (SETF (FILL-POINTER BUF) 0)
    (VALUES BUF
            (DO ((TEM (SEND SELF :TYI NIL) (SEND SELF :TYI NIL)))
                ((OR (NULL TEM) (= TEM #/CR) (= TEM #/END))
                 (ADJUST-ARRAY-SIZE BUF (LENGTH BUF))
                 (NULL TEM))
              (VECTOR-PUSH-EXTEND TEM BUF)))))

(DEFVAR COLD-LOAD-STREAM-BLINKER-TIME 15.)
(DEFVAR COLD-LOAD-STREAM-WAIT-TIME 1000.)
(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (COLD-LOAD-STREAM)
(DEFUN COLD-LOAD-STREAM-WAIT-FOR-CHAR ()
  (DO ((PHASE NIL)
       (BLINKER-COUNT 0)
       (CURRENTLY-PREPARED-SHEET SELF))
      ((KBD-HARDWARE-CHAR-AVAILABLE)
       (AND PHASE
            (%DRAW-RECTANGLE (FONT-BLINKER-WIDTH FONT) (FONT-BLINKER-HEIGHT FONT) CURSOR-X
                             CURSOR-Y TV:ALU-XOR SELF)))
    (COND ((MINUSP (SETQ BLINKER-COUNT (1- BLINKER-COUNT)))
           (%DRAW-RECTANGLE (FONT-BLINKER-WIDTH FONT) (FONT-BLINKER-HEIGHT FONT) CURSOR-X
                             CURSOR-Y TV:ALU-XOR SELF)
           (SETQ PHASE (NOT PHASE)
                 BLINKER-COUNT COLD-LOAD-STREAM-BLINKER-TIME)))
    (DOTIMES (I COLD-LOAD-STREAM-WAIT-TIME)))))

(defvar rubout-handler-options nil
  "The options supplied as first arg to :RUBOUT-HANDLER operation or to WITH-INPUT-EDITING.")

(defvar cold-load-stream-activation-character)

;;; Give a single character, or do rubout processing, throws to RUBOUT-HANDLER on editing.
(declare-instance-immediate-instance-variables (cold-load-stream)
(defun cold-load-stream-rubout-handler ()
  (declare (special tv::prompt-starting-x tv::prompt-starting-y))
  (when (= (array-leader rubout-handler-buffer 1) most-positive-fixnum)
    (setf (array-leader rubout-handler-buffer 1) 0)
    (throw 'rubout-handler t))
  (if cold-load-stream-activation-character
      (return-from cold-load-stream-rubout-handler
        (prog1 cold-load-stream-activation-character
               (setq cold-load-stream-activation-character nil))))
  (do ((ch)
       (rubbed-out-some)
       (len)
       (rubout-handler nil)
       (pass-through (cdr (assq ':pass-through rubout-handler-options)))
       (editing-command (cdr (assq ':editing-command rubout-handler-options)))
       (do-not-echo (cdr (assq ':do-not-echo rubout-handler-options)))
       (command-handler
         (assq ':command rubout-handler-options))
       (activation-handler
         (assq ':activation rubout-handler-options))
       (initial-input (cadr (assq ':initial-input rubout-handler-options)))
       (status (array-leader rubout-handler-buffer 2) nil))
      (nil)
    (setf (array-leader rubout-handler-buffer 2) nil)
    (when (memq status '(:initial-entry :restored))
      (multiple-value-setq (tv::prompt-starting-x tv::prompt-starting-y)
        (send self :read-cursorpos))
      (let ((prompt-option (assq ':prompt rubout-handler-options)))
        (when (and prompt-option (fboundp 'tv::rubout-handler-prompt))
          (tv::rubout-handler-prompt (cadr prompt-option) self nil)))
      (when initial-input
        (let ((length (length initial-input)))
          (send self ':string-out initial-input)
          (if (< (array-length rubout-handler-buffer) length)
              (setq rubout-handler-buffer (array-grow rubout-handler-buffer
                                                      (+ length length))))
          (copy-array-portion initial-input 0 length rubout-handler-buffer 0 length)
          (setf (fill-pointer rubout-handler-buffer) length)
          (setq rubbed-out-some t))))
    (setq ch (send self ':tyi))
    (cond ((and command-handler
                (apply (cadr command-handler) ch (cddr command-handler)))
             (setf (array-leader rubout-handler-buffer 1) 0)
             (throw 'tv::return-from-rubout-handler
                    (values
                       `(:command ,ch 1)
                       ':command)))
          ;; Don't touch this character, just return it to caller.
          ((or (memq ch editing-command)
               (assq-careful ch editing-command))
           ;; Cause rubout handler rescan next time the user does :TYI.
           (if rubbed-out-some
               (setf (array-leader rubout-handler-buffer 1) most-positive-fixnum))
           (return ch))
          ((and (not (or (memq ch do-not-echo)
                         (memq ch pass-through)
                         (and activation-handler
                              (apply (cadr activation-handler) ch (cddr activation-handler)))))
                (or (not (zerop (char-bits ch)))
                    (memq ch '(#/Rubout #/Clear-input #/Clear-screen #/Delete))))
           (cond
             ((= ch #/Clear)                    ;CLEAR flushes all buffered input
              (setf (fill-pointer rubout-handler-buffer) 0)
              (setq rubbed-out-some t)          ;Will need to throw out
              (send self ':tyo ch)              ;Echo and advance to new line
              (send self ':tyo #/Cr))
             ((or (= ch #/Form) (= ch #/Delete));Retype buffered input
              (send self ':tyo ch)              ;Echo it
              (if (= ch #/Form) (send self :clear-window) (send self :tyo #/Return))
              (let ((prompt (cadr (or (assq ':reprompt rubout-handler-options)
                                      (assq ':prompt rubout-handler-options)))))
                (when (and prompt (fboundp 'tv::rubout-handler-prompt))
                  (tv::rubout-handler-prompt prompt self ch)))
              (send self ':string-out rubout-handler-buffer))
             ((= ch #/Rubout)
              (cond ((not (zerop (setq len (fill-pointer rubout-handler-buffer))))
                     (setq cursor-x (max 0 (- cursor-x char-width)))
                     (send self :clear-rest-of-line)
                     (setf (fill-pointer rubout-handler-buffer) (decf len))
                     (setq rubbed-out-some t)
                     (when (zerop len)
                       (setf (array-leader rubout-handler-buffer 1) 0)
                       (throw 'rubout-handler t)))))
             ((not (zerop (char-bits ch)))
              (kbd-convert-beep)))
           (when (and (zerop (fill-pointer rubout-handler-buffer))
                      (assq ':full-rubout rubout-handler-options))
             (setf (array-leader rubout-handler-buffer 1) 0)
             (throw 'rubout-handler t)))
          (t                                            ;It's a self-inserting character
           (cond ((memq ch do-not-echo)
                  (setq cold-load-stream-activation-character ch))
                 ((and activation-handler
                       (apply (cadr activation-handler) ch (cddr activation-handler)))
                  (setq ch `(:activation ,ch 1))
                  (setq cold-load-stream-activation-character ch))
                 (t
                  (if (not (zerop (char-bits ch)))      ;in :pass-through, but had bucky bits
                      (kbd-convert-beep)
                    (send self ':tyo ch)
                    (array-push-extend rubout-handler-buffer ch))))
           (cond ((and (atom ch)
                       (not (zerop (char-bits ch)))))   ;do nothing
                 (rubbed-out-some
                  (setf (array-leader rubout-handler-buffer 1) 0)
                  (throw 'rubout-handler t))
                 (t
                  (setf (array-leader rubout-handler-buffer 1)
                        (fill-pointer rubout-handler-buffer))
                  (setq cold-load-stream-activation-character nil)
                  (return ch))))))))

(defmethod-immediate (cold-load-stream :rubout-handler)
                     (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq ':nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
        (apply function args))
    (let ((rubout-handler-options options))
      (setf (fill-pointer rubout-handler-buffer) 0)
      (setf (array-leader rubout-handler-buffer 1) 0)
      (setf (array-leader rubout-handler-buffer 2) ':initial-entry)
      (let (tv::prompt-starting-x tv::prompt-starting-y)
        (declare (special tv::prompt-starting-x tv::prompt-starting-y))
        (catch 'tv::return-from-rubout-handler
          (do ((rubout-handler self)                    ;Establish rubout handler
               (inhibit-scheduling-flag (WINDOW-SYSTEM-LOADED-P)) ;Make sure all chars come here
               (cold-load-stream-activation-character nil))
              (nil)
            (catch 'rubout-handler                      ;Throw here when rubbing out
              (condition-case (error)
                  (return (apply function args))        ;Call read type function
                (parse-error
                 (send self :fresh-line)
                 (princ ">>ERROR: " self)
                 (send error :report self)
                 (send self :fresh-line)
                 (send self :string-out rubout-handler-buffer)  ;On error, retype buffered
                 (do-forever (send self ':tyi)))))              ;and force user to edit it
            ;;Maybe return when user rubs all the way back
            (and (zerop (fill-pointer rubout-handler-buffer))
                 (let ((full-rubout-option (assq ':full-rubout rubout-handler-options)))
                   (when full-rubout-option
                     ;; Get rid of the prompt, if any.
                     (send self :set-cursorpos tv::prompt-starting-x tv::prompt-starting-y)
                     (send self :clear-rest-of-line)
                     (return (values nil (cadr full-rubout-option))))))))))))


(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-INPUT) ()
  (SETQ UNRCHF NIL)
  (SETF (FILL-POINTER RUBOUT-HANDLER-BUFFER) 0)
  (SETF (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1) 0)
  (DO () ((NOT (KBD-HARDWARE-CHAR-AVAILABLE)))
    ;;Call the convert routine for up-shifts too
    (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :BEEP) (&OPTIONAL BEEP-TYPE) BEEP-TYPE
  (KBD-CONVERT-BEEP))

(defvar use-keyboard-interrupts nil)

(DEFUN KBD-HARDWARE-CHAR-AVAILABLE ()
  "Returns T if a character is available in the microcode interrupt buffer"
  (cond ((and (= processor-type-code explorer-type-code)
              (null use-keyboard-interrupts))
         (let ((status (%nubus-read #xF5 #xFC0000)))
           (ldb-test (byte 1 1) status)))
        (T ( (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
              (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))))

(DEFUN KBD-GET-HARDWARE-CHAR (&AUX P)
  "Returns the next character in the microcode interrupt buffer, and NIL if there is none"
  (cond ((and (= processor-type-code explorer-type-code)
              (null use-keyboard-interrupts))
         (if (kbd-hardware-char-available)
             (dpb 3 (BYTE 3. 16.) (logand #xFF (%nubus-read #xF5 #xFC0004)))))
        (T (WHEN ( (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
                    (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))
             (PROG1 (%P-LDB %%Q-POINTER P)
                    (INCF P)
                    (WHEN (= P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END)))
                      (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))))
                    (%P-DPB P %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))))))

(defun new-enable-keyboard-ints ()
  (without-interrupts
    (initialize-wired-kbd-buffer)
    ;;write vector
    (%nubus-write #xf5 #xf00018 #xf6e00034)
    ;;initialize 8251
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 #x40)
    (%nubus-write #xf5 #xFC0000 #x7F)
    (dotimes (i 100))
    ;; In the old code there was no timing delay loop,
    ;; and the quantity written here was a #x14. New code
    ;; had it be a #x15. But that didnt work, and switching it
    ;; back to #x14 did work. Obviously we would like it to work.
    ;; Presumably Pace and I can resolve this 8251 initialization
    ;; hair. (Q: What did the TI code do?) -GJC
    (%nubus-write #xf5 #xFC0000 #x14)           ;was, and it now #x14
    (dotimes (i 100))
    ;;enable keyboard ints
    ;; (apparently controlled by 8251 initialization above)
    ;;enable SI bus master
    (%nubus-write #xf5 #xf00040 2)

    (cond ((reset-exp-keyboard)
           (ti-keyboard))
          (t
           (lmi-keyboard)))

    (setq use-keyboard-interrupts t)))

(defun reset-exp-keyboard (&aux got-response)
  (without-interrupts
    (let ((use-keyboard-interrupts nil))
      ;;disable sib interrupts
      (%nubus-write #xf5 #xf00040 0)
      ;;enable transmitter
      (%nubus-write #xf5 #xfc0000 #x11)
      (dotimes (i 100))

      ;;flush any characters in the chip
      (do ((i 0 (1+ i)))
          ((> i 5000.))
        (when (kbd-hardware-char-available)
          (kbd-get-hardware-char)
          (setq i 0)))

      ;;send reset byte
      (%nubus-write #xf5 #xfc0004 0)
      (dotimes (i 100))
      ;;disable transmitter
      (%nubus-write #xf5 #xfc0000 #x14)

      (do ((i 0 (1+ i)))
          ((> i 5000.))
        (when (kbd-hardware-char-available)
          (setq got-response t)
          (kbd-get-hardware-char)
          (setq i 0)))

      (%nubus-write #xf5 #xf00040 2)
      ))
  got-response)

(DEFVAR SHIFT-LOCK-XORS NIL)    ;If T, both SHIFT LOCK and SHIFT is the same as neither
                                ; if the character is alphabetic

(DEFUN KBD-CONVERT-TO-SOFTWARE-CHAR (HARD-CHAR &OPTIONAL (UPCASE-CONTROL-CHARS T))
  "Convert hardware character to software character, or NIL to ignore.
UPCASE-CONTROL-CHARS if NIL means leave Meta-lowercase-letter, etc.,
not converted to upper case."
  (SELECTQ (LDB (BYTE 3. 16.) HARD-CHAR)                        ;Source ID
    (1 (KBD-CONVERT-NEW HARD-CHAR UPCASE-CONTROL-CHARS))                ;New keyboard
    (2 (KBD-CONVERT-NEWER HARD-CHAR UPCASE-CONTROL-CHARS))  ;8 bit serial keyboard
    (3 (kbd-convert-ti hard-char upcase-control-chars)) ; LMI keyboard sending Explorer codes
    (6 (SET-MOUSE-MODE 'VIA-KBD)                ;Mouse via keyboard - turn on remote mouse
       NIL)                                     ; enable bit in IOB
    ))

;; Sys com locations 500-511 are reserved for the wired keyboard buffer:
;; Locations 501 through 511 contain the buffer header; the actual buffer
;; is in locations 200-377 (128. chars, 64. on new-keyboards)
;;
;; This is called when the machine is booted, warm or cold.  It's not an
;; initialization because it has to happen before all other initializations.
(DEFUN INITIALIZE-WIRED-KBD-BUFFER ()
  (DO ((I 500 (1+ I))) ((= I 512))
    (%P-DPB 0 %%Q-LOW-HALF I)
    (%P-DPB 0 %%Q-HIGH-HALF I))
  (DO ((I 200 (1+ I))) ((= I 400))
    (%P-DPB 0 %%Q-LOW-HALF I)
    (%P-DPB 0 %%Q-HIGH-HALF I))
  (%P-DPB (selectq video-board-type
            (:cadr 260)
            (:vcmem 260)
            (:quad 261)
            (:explorer 13.)
            )
          %%Q-POINTER
          (+ 500 %UNIBUS-CHANNEL-VECTOR-ADDRESS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764112) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-ADDRESS))
  (%P-DPB 40 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-BITS))
  (%P-DPB 1 %%UNIBUS-CSR-TWO-DATA-REGISTERS (+ 500 %UNIBUS-CHANNEL-CSR-BITS))
  (%P-DPB 1 %%UNIBUS-CSR-SB-ENABLE (+ 500 %UNIBUS-CHANNEL-CSR-BITS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764100) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-DATA-ADDRESS))
  (%P-DPB 200 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))
  (%P-DPB 400 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END))
  (%P-DPB 200 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
  (%P-DPB 200 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
  (SETF (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST) 500)
  (SET-MOUSE-MODE 'DIRECT))

(DEFUN SET-MOUSE-MODE (MODE)
  (SELECT-PROCESSOR
    (:CADR
     (SELECTQ MODE
       (DIRECT (%UNIBUS-WRITE #o764112 4))      ;Keyboard interrupt enable, local mouse
       (VIA-KBD (%UNIBUS-WRITE #o764112 5))
       (OTHERWISE (FERROR NIL "Unknown mouse mode"))))
    ((:LAMBDA :EXPLORER)
      NIL)))

;; Translate from a Unibus address to a Lisp machine virtual address, returning a fixnum.
(DEFUN VIRTUAL-UNIBUS-ADDRESS (ADR)
  (SELECT-PROCESSOR
    (:CADR
     (%POINTER-PLUS (LSH #o7740 12.) (LSH ADR -1)))
    (:LAMBDA
     (%POINTER-PLUS (LSH #o7737 12.) #o7777))   ;will trap if ref'ed
    (:explorer
     (%POINTER-PLUS (LSH #o7737 12.) #o7777))))


;Support for new keyboard

;These two variables are bit masks for the shifting keys held down.
;Bit numbers are the same as those in the all-keys-up code sent by the
;keyboard, i.e.
;       0 shift         3 caps lock     6 super         9 mode lock
;       1 greek         4 control       7 hyper         10 repeat
;       2 top           5 meta          8 alt lock
;There are two variables so that if both shifts of a given type are pushed
;down. then one is released, we can tell what's going on.

(DEFVAR KBD-LEFT-SHIFTS 0)
(DEFVAR KBD-RIGHT-SHIFTS 0)
(DEFVAR KBD-KEY-STATE-ARRAY                     ;1 if key with that ascii code is down
        (MAKE-ARRAY #o400 :TYPE 'ART-1B :AREA PERMANENT-STORAGE-AREA))
(DEFVAR KBD-KEY-STATE-ARRAY-16B                 ;For fast clearing of above array
        (MAKE-ARRAY #o20 :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA
                         :DISPLACED-TO KBD-KEY-STATE-ARRAY))

;The second dimension is 200 long and indexed by keycode.
;The first dimension is the shifts:
; 0 unshifted
; 1 shift
; 2 top
; 3 greek
; 4 shift greek
;Elements in the table are 16-bit unsigned numbers.
;Bit 15 on and bit 14 on means undefined code, ignore and beep.
;Bit 15 on and bit 14 off means low bits are shift for bit in KBD-SHIFTS
;   (40 octal for right-hand key of a pair)
;Bit 15 off is ordinary code.

;Can return NIL if character wasn't really a character.
(DEFUN KBD-CONVERT-NEW (CH &OPTIONAL (CTL-CHARS-UPPERCASE T))
  (declare (special kdb-new-table))             ;defvared below
  (COND ((BIT-TEST 1_15. CH)            ;An all-keys-up code, just update shifts mask
         (COPY-ARRAY-CONTENTS "" KBD-KEY-STATE-ARRAY-16B)       ;Mark all keys up
         (SETQ CH (LDB (BYTE 15. 0.) CH))       ;Get bits for keys or key-pairs still down
         (SETQ KBD-LEFT-SHIFTS (LOGAND KBD-LEFT-SHIFTS CH)
               KBD-RIGHT-SHIFTS (LOGAND KBD-RIGHT-SHIFTS CH)
               KBD-LEFT-SHIFTS          ;This is for keys that are down that we thought
                 (LOGIOR                ; were up, e.g. caps lock.  Boole 10 is NOR.
                   (LOGAND (BOOLE 10 KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS) CH)
                   KBD-LEFT-SHIFTS)
              TV::KBD-BUTTONS 0)                ;analogous to mouse buttons, set by roman numerals
         NIL)
        (T (LET* ((KBD-SHIFTS (LOGIOR KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS))
                  (NCH (AREF KBD-NEW-TABLE      ;NCH gets translate-table entry
                             (COND ((BIT-TEST 2 KBD-SHIFTS)     ;Greek
                                    (+ (LOGAND 1 KBD-SHIFTS) 3))
                                   ((BIT-TEST 4 KBD-SHIFTS) 2)  ;Top
                                   ((BIT-TEST 1 KBD-SHIFTS) 1)  ;Shift
                                   (T 0))
                             (LDB (BYTE 7. 0.) CH)))
                  (NCH0 (AREF KBD-NEW-TABLE 0 (LDB (BYTE 7. 0.) CH))))
             (COND ((BIT-TEST 1_15. NCH)        ;Not a real character
                    (COND ((BIT-TEST 1_14. NCH) ;Undefined key, beep if key-down
                           (OR (BIT-TEST 1_8 CH)
                               (KBD-CONVERT-BEEP)))
                          (T                    ;A shifting key, update KBD-SHIFTS
                            (LET ((BOOLE (IF (BIT-TEST 1_8 CH) 2 7))  ;Bit off, on
                                  (BIT (LSH 1 (LOGAND NCH 37))))
                              (IF (BIT-TEST 40 NCH)
                                  (SETQ KBD-RIGHT-SHIFTS (BOOLE BOOLE BIT KBD-RIGHT-SHIFTS))
                                  (SETQ KBD-LEFT-SHIFTS (BOOLE BOOLE BIT KBD-LEFT-SHIFTS))))))
                    NIL)
                   ((BIT-TEST 1_8 CH)
                    (setf (aref KBD-KEY-STATE-ARRAY NCH0) 0)
                    (COND ((BIT-TEST 1_9 KBD-SHIFTS)     ;Mode lock
                           (SELECTQ NCH
                             (#/ROMAN-I (SETQ TV::KBD-BUTTONS (BOOLE 4 TV::KBD-BUTTONS 1)))
                             (#/ROMAN-II (SETQ TV::KBD-BUTTONS (BOOLE 4 TV::KBD-BUTTONS 2)))
                             (#/ROMAN-III (SETQ TV::KBD-BUTTONS (BOOLE 4 TV::KBD-BUTTONS 4))))
                           (SETQ MOUSE-WAKEUP T)))
                    NIL)         ;Just an up-code
                   ((AND (BIT-TEST 1_9 KBD-SHIFTS)       ;Mode lock
                         (MEMQ NCH '(#/ROMAN-I #/ROMAN-II #/ROMAN-III)))
                    (setf (aref KBD-KEY-STATE-ARRAY NCH0) 1)
                    (SETQ TV::KBD-BUTTONS (LOGIOR TV::KBD-BUTTONS
                                                  (SELECTQ NCH (#/ROMAN-I 1)
                                                           (#/ROMAN-II 2)
                                                           (T 4))))
                    (SETQ MOUSE-WAKEUP T)
                    NIL)
                   (T ;A real key depression.  Check for caps-lock.
                    (setf (aref KBD-KEY-STATE-ARRAY NCH0) 1)
                    (SETQ NCH0 (LDB (BYTE 4. 4.) KBD-SHIFTS))   ;Hyper, Super, Meta, Control bits
                    (IF (AND CTL-CHARS-UPPERCASE
                             (NOT (ZEROP NCH0)))
                        (IF ( #/a NCH #/z)
                            (DECF NCH 40)       ;Control characters always uppercase,
                          (IF ( #/A NCH #/Z)   ;except if Shift is typed they are lowercase.
                              (INCF NCH 40)))
                      ;; Except for control chars for which Shift is reversed,
                      ;; consider the shift-lock key.
                      (AND (BIT-TEST 10 KBD-SHIFTS)     ;Caps lock
                           (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
                               (AND ( NCH #/A) ( NCH #/Z) (SETQ NCH (+ NCH 40)))
                             (AND ( NCH #/a) ( NCH #/z) (SETQ NCH (- NCH 40))))))
                    (%LOGDPB NCH0 %%KBD-CONTROL-META NCH)))))))

(DEFUN KBD-MAKE-NEW-TABLE ()
  ;; Note: format ~@:C knows about the format of this (format::format-print-top-character)
  ;;  Also, format::ochar-explain-top-character
  (LET ((TBL (MAKE-ARRAY '(5 #o200) :AREA  PERMANENT-STORAGE-AREA :TYPE 'ART-16B)))
    (DO ((J 0 (1+ J))
         (L '(()                                ;0 not used
              #/ROMAN-II                        ;1 Roman II
              #/ROMAN-IV                        ;2 Roman IV
              #o100011                          ;3 Mode lock
              ()                                ;4 not used
              #o100006                          ;5 Left super
              ()                                ;6 not used
              ()                                ;7 not used
              ()                                ;10 not used
              (#/4 #/$ #/$)                     ;11 Four
              (#/r #/R #/)                     ;12 R
              (#/f #/F)                         ;13 F
              (#/v #/V)                         ;14 V
              #o100008                          ;15 Alt Lock
              ()                                ;16 not used
              #/HAND-RIGHT                      ;17 Hand Right
              100004                            ;20 Left control
              (#/: #/ #/)                     ;21 plus-minus
              #/TAB                             ;22 tab
              #/RUBOUT                          ;23 rubout
              #o100000                          ;24 Left Shift
              #o100040                          ;25 Right Shift
              #o100044                          ;26 Right control
              ()                                ;27 not used
              #/HOLD-OUTPUT                     ;30 hold output
              (#/8 #/* #/*)                     ;31 Eight
              (#/i #/I #/)                     ;32 I
              (#/k #/K #/)                     ;33 K
              (#/, #/< #/<)                     ;34 comma
              #o100041                          ;35 Right Greek
              #/LINE                            ;36 Line
              (#/\ #/| #/|)                     ;37 Backslash
              #/TERMINAL                        ;40 terminal
              ()                                ;41 not used
              #/NETWORK                         ;42 network
              ()                                ;43 not used
              #o100001                          ;44 Left Greek
              #o100005                          ;45 Left Meta
              #/STATUS                          ;46 status
              #/RESUME                          ;47 resume
              #/FORM                            ;50 clear screen
              (#/6 #/^ #/^)                     ;51 Six
              (#/y #/Y #/)                     ;52 Y
              (#/h #/H #/)                     ;53 H
              (#/n #/N #/)                     ;54 N
              ()                                ;55 not used
              ()                                ;56 not used
              ()                                ;57 not used
              ()                                ;60 not used
              (#/2 #/@ #/@)                     ;61 Two
              (#/w #/W #/)                     ;62 W
              (#/s #/S)                         ;63 S
              (#/x #/X)                         ;64 X
              #o100046                          ;65 Right Super
              ()                                ;66 not used
              #/ABORT                           ;67 Abort
              ()                                ;70 not used
              (#/9 #/( #/( )                    ;71 Nine
              (#/o #/O #/)                     ;72 O
              (#/l #/L #/ #/)                 ;73 L/lambda
              (#/. #/> #/>)                     ;74 period
              ()                                ;75 not used
              ()                                ;76 not used
              (#/` #/~ #/~ #/)                 ;77 back quote
              #/BACK-NEXT                       ;100 macro
              #/ROMAN-I                         ;101 Roman I
              #/ROMAN-III                       ;102 Roman III
              ()                                ;103 not used
              #o100002                          ;104 Left Top
              ()                                ;105 not used
              #/HAND-UP                         ;106 Up Thumb
              #/CALL                            ;107 Call
              #/CLEAR                           ;110 Clear Input
              (#/5 #/% #/%)                     ;111 Five
              (#/t #/T #/)                     ;112 T
              (#/g #/G #/ 11)                  ;113 G/gamma
              (#/b #/B #/ #/)                 ;114 B
              ()                                ;115 Repeat
              #/HELP                            ;116 Help
              (#/HAND-LEFT #/HAND-LEFT #/HAND-LEFT #/ #/)     ;117 Hand Left
              #/QUOTE                           ;120 Quote
              (#/1 #/! #/!)                     ;121 One
              (#/q #/Q #/)                     ;122 Q
              (#/a #/A #o140000 #/)            ;123 A
              (#/z #/Z)                         ;124 Z
              #o100003                          ;125 Caps Lock
              (#/= #/+ #/+)                     ;126 Equals
              ()                                ;127 not used
              ()                                ;130 not used
              (#/- #/_ #/_)                     ;131 Minus
              (#/( #/[ #/[)                     ;132 Open parenthesis
              (#/' #/" #/" 0)                   ;133 Apostrophe/center-dot
              #/SP                              ;134 Space
              ()                                ;135 not used
              #/CR                              ;136 Return
              (#/) #/] #/])                     ;137 Close parenthesis
              ()                                ;140 not used
              #/SYSTEM                          ;141 system
              ()                                ;142 not used
              #/                               ;143 Alt Mode
              ()                                ;144 not used
              #o100007                          ;145 Left Hyper
              (#/} #o140000 #o140000)           ;146 }
              ()                                ;147 not used
              ()                                ;150 not used
              (#/7 #/& #/&)                     ;151 Seven
              (#/u #/U #/)                     ;152 U
              (#/j #/J #/)                     ;153 J
              (#/m #/M #/)                     ;154 M
              #o100042                          ;155 Right Top
              #/END                             ;156 End
              #/DELETE                          ;157 Delete
              #/OVERSTRIKE                      ;160 Overstrike
              (#/3 #/# #/#)                     ;161 Three
              (#/e #/E #/ #/)                 ;162 E
              (#/d #/D #o140000 #/
)               ;163 D/delta
              (#/c #/C #/)                     ;164 C
              #o100045                          ;165 Right Meta
              (#/{ #o140000 #o140000)           ;166 {
              #/BREAK                           ;167 Break
              #/STOP-OUTPUT                     ;170 Stop Output
              (#/0 #/) #/))                     ;171 Zero
              (#/p #/P #/ #/)                 ;172 P
              (#/; #/: #/:)                     ;173 Semicolon
              (#// #/? #/? #/)                 ;174 Question/Integral
              #o100047                          ;175 Right Hyper
              (#/HAND-DOWN #/HAND-DOWN #/HAND-DOWN #/ #/)     ;176 Down Thumb
              ()                                ;177 Not used
              )
            (CDR L)))
        ((= J #o200) TBL)
      (DO ((I 0 (1+ I))
           (K (CAR L)))
          ((= I 5))
        (setf (aref tbl i j) (COND ((ATOM K) (OR K #o140000))
                                   ((NULL (CAR K)) #o140000)
                                   (T (CAR K))))
        (AND (CONSP K) (SETQ K (CDR K)))))))

(DEFVAR KBD-NEW-TABLE (KBD-MAKE-NEW-TABLE))

(defconst kbd-TABLE-for-lmi
          '(
            ;;0000
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;unused
            (#/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II)
            (#/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV)
            (#o100011 #o100011 #o100011 #o100011 #o100011)      ;3 Mode lock
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;unused
            (#o100006 #o100006 #o100006 #o100006 #o100006)      ;5 Left super
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;6 not used
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;7 not used
            ;;0010
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;10 not used
            (#/4 #/$ #/$ #o140000 #o140000)
            (#/r #/R #/down-horseshoe #o140000 #o140000)
            (#/f #/F #o140000 #o140000)
            (#/v #/V #o140000 #o140000)
            (#o100008 #o100008 #o100008 #o100008 #o100008)      ;15 Alt Lock
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;16 not used
            (#/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT)
            ;;0020
            (#o100004 #o100004 #o100004 #o100004 #o100004)      ;20 Left control
            (#/: #/plus-minus #/plus-minus #o140000 #o140000)
            (#/TAB #/TAB #/TAB #/TAB #/TAB)
            (#/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT)
            (#o100000 #o100000 #o100000 #o100000 #o100000)      ;24 Left Shift
            (#o100040 #o100040 #o100040 #o100040 #o100040)      ;25 Right Shift
            (#o100044 #o100044 #o100044 #o100044 #o100044)      ;26 Right control
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;27 not used
            ;;0030
            (#/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT)
            (#/8 #/* #/* #o140000 #o140000)
            (#/i #/I #/infinity #o140000 #o140000)
            (#/k #/K #/right-arrow #o140000 #o140000)
            (#/, #/< #/< #o140000 #o140000)
            (#o100041 #o100041 #o100041 #o100041 #o100041)
            (#/LINE #/LINE #/LINE #/LINE #/LINE)
            (#/\ #/| #/| #o140000 #o140000)
            ;;0040
            (#/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/NETWORK #/NETWORK #/NETWORK #/NETWORK #/NETWORK)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o100001 #o100001 #o100001 #o100001 #o100001)      ;44 Left Greek
            (#o100005 #o100005 #o100005 #o100005 #o100005)      ;45 Left Meta
            (#/STATUS #/STATUS #/STATUS #/STATUS #/STATUS)
            (#/RESUME #/RESUME #/RESUME #/RESUME #/RESUME)
            ;;0050
            (#/FORM #/FORM #/FORM #/FORM #/FORM)
            (#/6 #/^ #/^ #o140000 #o140000)
            (#/y #/Y #/right-horseshoe #o140000 #o140000)
            (#/h #/H #/down-arrow #o140000 #o140000)
            (#/n #/N #/less-or-equal #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            ;;0060
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/2 #/@ #/@ #o140000 #o140000)
            (#/w #/W #/or #o140000 #o140000)
            (#/s #/S #o140000 #o140000 #o140000)
            (#/x #/X #o140000 #o140000 #o140000)
            (#o100046 #o100046 #o100046 #o100046 #o100046)      ;65 Right Super
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/ABORT #/ABORT #/ABORT #/ABORT #/ABORT)
            ;;0070
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/9 #/( #/( #o140000 #o140000)
            (#/o #/O #/there-exists #o140000 #o140000)
            (#/l #/L #/double-arrow #/lambda #o140000)
            (#/. #/> #/> #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/` #/~ #/~ #/not-sign #o140000 #o140000)
            ;;0100
            (#/BACK-NEXT #/BACK-NEXT #/BACK-NEXT #/BACK-NEXT #/BACK-NEXT)
            (#/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I)
            (#/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;103 not used
            (#o100002 #o100002 #o100002 #o100002 #o100002)      ;104 Left Top
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;105 not used
            (#/HAND-UP #/HAND-UP #/HAND-UP #/HAND-UP #/HAND-UP)
            (#/CALL #/CALL #/CALL #/CALL #/CALL)
            ;;0110
            (#/CLEAR #/CLEAR #/CLEAR #/CLEAR #/CLEAR)
            (#/5 #/% #/% #o140000 #o140000)
            (#/t #/T #/left-horseshoe #o140000 #o140000)
            (#/g #/G #/up-arrow #/gamma #o140000)
            (#/b #/B #/equivalence #/beta #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;115 Repeat
            (#/HELP #/HELP #/HELP #/HELP #/HELP)
            (#/HAND-LEFT #/HAND-LEFT #/HAND-LEFT #/ #/)
            ;;0120
            (#/QUOTE #/QUOTE #/QUOTE #/QUOTE #/QUOTE)
            (#/1 #/! #/! #o140000 #o140000)
            (#/q #/Q #/and-sign #o140000 #o140000)
            (#/a #/A #o140000 #/alpha #o140000)
            (#/z #/Z #o140000 #o140000 #o140000)
            (#o100003 #o100003 #o100003 #o100003 #o100003)
            (#/= #/+ #/+ #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;127 not used
            ;;0130
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;130 not used
            (#/- #/_ #/_ #o140000 #o140000)
            (#/( #/[ #/[ #o140000 #o140000)
            (#/' #/" #/" #/center-dot #o140000)
            (#/SPace #/SPace #/SPace #/SPace #/SPace)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;135 not used
            (#/CR #/CR #/CR #/CR #/CR)
            (#/) #/] #/] #o140000 #o140000)
            ;;0140
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;140 not used
            (#/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;142 not used
            (#/altmode #/altmode #/altmode #/altmode #/altmode)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;144 not used
            (#o100007 #o100007 #o100007 #o100007 #o100007)      ;145 Left Hyper
            (#/} #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;147 not used
            ;;0150
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;150 not used
            (#/7 #/& #/& #o140000 #o140000)
            (#/u #/U #/for-all #o140000 #o140000)
            (#/j #/J #/left-arrow #o140000 #o140000)
            (#/m #/M #/greater-or-equal #o140000 #o140000)
            (#o100042 #o100042 #o100042 #o100042 #o100042)      ;155 Right Top
            (#/END #/END #/END #/END #/END)
            (#/DELETE #/DELETE #/DELETE #/DELETE #/DELETE)
            ;;0160
            (#/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE)
            (#/3 #/# #/# #o140000 #o140000)
            (#/e #/E #/up-horseshoe #/epsilon #o140000)
            (#/d #/D #o140000 #/delta #o140000)
            (#/c #/C #/not-equal #o140000 #o140000)
            (#o100045 #o100045 #o100045 #o100045 #o100045)      ;165 Right Meta
            (#/{ #o140000 #o140000 #o140000 #o140000)
            (#/BREAK #/BREAK #/BREAK #/BREAK #/BREAK)
            ;;0170
            (#/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT)
            (#/0 #/) #/) #o140000 #o140000)
            (#/p #/P #/partial-delta #/pi #o140000)
            (#/; #/: #/: #o140000 #o140000)
            (#// #/? #/? #/integral #o140000)
            (#o100047 #o100047 #o100047 #o100047 #o100047)      ;175 Right Hyper
            (#/HAND-DOWN #/HAND-DOWN #/HAND-DOWN #/ #/)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            ))

(defconst kbd-dvorak-TABLE-for-lmi
          '(
            ;;0000
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;unused
            (#/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II)
            (#/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV)
            (#o100011 #o100011 #o100011 #o100011 #o100011)      ;3 Mode lock
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;unused
            (#o100006 #o100006 #o100006 #o100006 #o100006)      ;5 Left super
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;6 not used
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;7 not used
            ;;0010
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;10 not used
            (#/4 #/$ #/$ #o140000 #o140000)
            (#/p #/P #/partial-delta #/pi #o140000)
            (#/u #/U #/for-all #o140000 #o140000)
            (#/k #/K #/right-arrow #o140000 #o140000)
            (#o100008 #o100008 #o100008 #o100008 #o100008)      ;15 Alt Lock
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;16 not used
            (#/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT #/HAND-RIGHT)
            ;;0020
            (#o100004 #o100004 #o100004 #o100004 #o100004)      ;20 Left control
            (#/: #/plus-minus #/plus-minus #o140000 #o140000)
            (#/TAB #/TAB #/TAB #/TAB #/TAB)
            (#/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT)
            (#o100000 #o100000 #o100000 #o100000 #o100000)      ;24 Left Shift
            (#o100040 #o100040 #o100040 #o100040 #o100040)      ;25 Right Shift
            (#o100044 #o100044 #o100044 #o100044 #o100044)      ;26 Right control
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;27 not used
            ;;0030
            (#/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT)
            (#/8 #/* #/* #o140000 #o140000)
            (#/c #/C #/not-equal #o140000 #o140000)
            (#/t #/T #/left-horseshoe #o140000 #o140000)
            (#/w #/W #/or #o140000 #o140000)
            (#o100041 #o100041 #o100041 #o100041 #o100041)
            (#/LINE #/LINE #/LINE #/LINE #/LINE)
            (#/\ #/| #/| #o140000 #o140000)
            ;;0040
            (#/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/NETWORK #/NETWORK #/NETWORK #/NETWORK #/NETWORK)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o100001 #o100001 #o100001 #o100001 #o100001)      ;44 Left Greek
            (#o100005 #o100005 #o100005 #o100005 #o100005)      ;45 Left Meta
            (#/STATUS #/STATUS #/STATUS #/STATUS #/STATUS)
            (#/RESUME #/RESUME #/RESUME #/RESUME #/RESUME)
            ;;0050
            (#/FORM #/FORM #/FORM #/FORM #/FORM)
            (#/6 #/^ #/^ #o140000 #o140000)
            (#/f #/F #o140000 #o140000)
            (#/d #/D #o140000 #/delta #o140000)
            (#/b #/B #/equivalence #/beta #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            ;;0060
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/2 #/@ #/@ #o140000 #o140000)
            (#/, #/< #/< #o140000 #o140000)
            (#/o #/O #/there-exists #o140000 #o140000)
            (#/q #/Q #/and-sign #o140000 #o140000)
            (#o100046 #o100046 #o100046 #o100046 #o100046)      ;65 Right Super
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/ABORT #/ABORT #/ABORT #/ABORT #/ABORT)
            ;;0070
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/9 #/( #/( #o140000 #o140000)
            (#/r #/R #/down-horseshoe #o140000 #o140000)
            (#/n #/N #/less-or-equal #o140000 #o140000)
            (#/v #/V #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            (#/` #/~ #/~ #/not-sign #o140000 #o140000)
            ;;0100
            (#/BACK-NEXT #/BACK-NEXT #/BACK-NEXT #/BACK-NEXT #/BACK-NEXT)
            (#/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I)
            (#/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;103 not used
            (#o100002 #o100002 #o100002 #o100002 #o100002)      ;104 Left Top
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;105 not used
            (#/HAND-UP #/HAND-UP #/HAND-UP #/HAND-UP #/HAND-UP)
            (#/CALL #/CALL #/CALL #/CALL #/CALL)
            ;;0110
            (#/CLEAR #/CLEAR #/CLEAR #/CLEAR #/CLEAR)
            (#/5 #/% #/% #o140000 #o140000)
            (#/y #/Y #/right-horseshoe #o140000 #o140000)
            (#/i #/I #/infinity #o140000 #o140000)
            (#/x #/X #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;115 Repeat
            (#/HELP #/HELP #/HELP #/HELP #/HELP)
            (#/HAND-LEFT #/HAND-LEFT #/HAND-LEFT #/ #/)
            ;;0120
            (#/QUOTE #/QUOTE #/QUOTE #/QUOTE #/QUOTE)
            (#/1 #/! #/! #o140000 #o140000)
            (#/' #/" #/" #/center-dot #o140000)
            (#/a #/A #o140000 #/alpha #o140000)
            (#/; #/: #/: #o140000 #o140000)
            (#o100003 #o100003 #o100003 #o100003 #o100003)
            (#/= #/+ #/+ #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;127 not used
            ;;0130
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;130 not used
            (#/- #/_ #/_ #o140000 #o140000)
            (#/( #/[ #/[ #o140000 #o140000)
            (#// #/? #/? #/integral #o140000)
            (#/SPace #/SPace #/SPace #/SPace #/SPace)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;135 not used
            (#/CR #/CR #/CR #/CR #/CR)
            (#/) #/] #/] #o140000 #o140000)
            ;;0140
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;140 not used
            (#/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;142 not used
            (#/altmode #/altmode #/altmode #/altmode #/altmode)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;144 not used
            (#o100007 #o100007 #o100007 #o100007 #o100007)      ;145 Left Hyper
            (#/} #o140000 #o140000 #o140000 #o140000)
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;147 not used
            ;;0150
            (#o140000 #o140000 #o140000 #o140000 #o140000)      ;150 not used
            (#/7 #/& #/& #o140000 #o140000)
            (#/g #/G #/up-arrow #/gamma #o140000)
            (#/h #/H #/down-arrow #o140000 #o140000)
            (#/m #/M #/greater-or-equal #o140000 #o140000)
            (#o100042 #o100042 #o100042 #o100042 #o100042)      ;155 Right Top
            (#/END #/END #/END #/END #/END)
            (#/DELETE #/DELETE #/DELETE #/DELETE #/DELETE)
            ;;0160
            (#/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE)
            (#/3 #/# #/# #o140000 #o140000)
            (#/. #/> #/> #o140000 #o140000)
            (#/e #/E #/up-horseshoe #/epsilon #o140000)
            (#/j #/J #/left-arrow #o140000 #o140000)
            (#o100045 #o100045 #o100045 #o100045 #o100045)      ;165 Right Meta
            (#/{ #o140000 #o140000 #o140000 #o140000)
            (#/BREAK #/BREAK #/BREAK #/BREAK #/BREAK)
            ;;0170
            (#/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT #/STOP-OUTPUT)
            (#/0 #/) #/) #o140000 #o140000)
            (#/l #/L #/double-arrow #/lambda #o140000)
            (#/s #/S #o140000 #o140000 #o140000)
            (#/z #/Z #o140000 #o140000 #o140000)
            (#o100047 #o100047 #o100047 #o100047 #o100047)      ;175 Right Hyper
            (#/HAND-DOWN #/HAND-DOWN #/HAND-DOWN #/ #/)
            (#o140000 #o140000 #o140000 #o140000 #o140000)
            ))

(defconst *kbd-lmi-dvorak-table* (kbd-make-bd-table kbd-dvorak-TABLE-for-lmi))

(defun kbd-convert-to-software-char-dvorak-lmi (hard-char &optional (upcase-control-chars t))
  (let ((kbd-new-table *kbd-lmi-dvorak-table*))
    (kbd-convert-to-software-char hard-char upcase-control-chars)))

(defun dvorak-mode (&optional (on-p t) (window terminal-io))
  "Change the keyboard mappings for the current window to use the Dvorak layout.
This function only affects the current window, and doesn't change the physical
keys used by the SYSTEM and TERMINAL commands.  The command also installs
an item in the system menu to undo its effects."
  (let ((plist (locf (tv:io-buffer-plist (send window :io-buffer)))))
    (cond ((null on-p)
           (remprop plist :character-translator))
          (t
           (setq *kbd-lmi-dvorak-table*
                 (kbd-make-bd-table kbd-dvorak-table-for-lmi))
           (putprop plist
                    'kbd-convert-to-software-char-dvorak-lmi
                    :character-translator)
           (tv:add-to-system-menu-programs-column
             "QWERTY Keyboard"
             '(dvorak-mode nil tv:selected-window)
             "Change current window back to QWERTY keyboard mode.")
           (format t "~&Note: The System and Terminal keys still use QWERTY mappings.")
           (format t "~&Use mouse to return to QWERTY mode.")
           t
           ))))



(defvar saved-first-char 0)   ;;; put first char of pair here while waiting for second

;;; do the right thing if the "software character" read out of kbd-new-table had
;;; bit 15 set.  If bit 14 is set then the key is undefined.  Otherwise we should update
;;; kbd-left-shifts and kbd-right-shifts.
(defun kbd-bit-15-on (soft-char down-p)
  (if (bit-test 1_14. soft-char);an undefined key
      (and down-p (kbd-convert-beep))
    ;;; not illegal, must be shift code for kbd-shifts

    (LET ((BOOLE (IF DOWN-P 7 2))
          (BIT (LSH 1 (LOGAND soft-char 37))))
      (IF (BIT-TEST 40 soft-char)
          (SETQ KBD-RIGHT-SHIFTS
                (BOOLE BOOLE BIT KBD-RIGHT-SHIFTS))
        (SETQ KBD-LEFT-SHIFTS
              (BOOLE BOOLE BIT KBD-LEFT-SHIFTS)))))
  nil)

(defun kbd-convert-newer (char &optional (ctl-chars-uppercase t))
  (setq char (logand #o377 char)) ;strip off source bits
  (cond ((bit-test 1_7 char)                    ;is it a second-byte?
         (cond ((bit-test 1_6 char)             ;up or down code?
                (prog1
                 (multiple-value-bind (soft-char unshifted-soft-char)
                     (new-lookup saved-first-char)
                   (cond ((bit-test 1_15. soft-char)
                          (kbd-bit-15-on soft-char t)
                          nil)
                         (t                     ;normal character
                          ;; set bitmap bit
                          (setf (aref kbd-key-state-array unshifted-soft-char) 1)
                          ;; A real key depression.  Check for caps-lock.
                          (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
                            ;; Hyper, Super, Meta, Control bits
                            (SETQ unshifted-soft-char (LDB (BYTE 4. 4.) KBD-SHIFTS))
                            (IF (AND CTL-CHARS-UPPERCASE
                                     (NOT (ZEROP unshifted-soft-char)))
                                (IF (<= #/a SOFT-CHAR #/z)
                                    (DECF SOFT-CHAR 40)         ;Control characters always uppercase,
                                  (IF (<= #/A SOFT-CHAR #/Z)    ;unless  Shift is typed
                                      (INCF SOFT-CHAR 40)))
                                ;; Except for control chars for which Shift is reversed,
                                ;; consider the shift-lock key.
                                (AND (BIT-TEST 10 KBD-SHIFTS)   ;Caps lock
                                     (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
                                         (AND (>= SOFT-CHAR #/A)
                                              (<= SOFT-CHAR #/Z)
                                              (SETQ SOFT-CHAR (+ SOFT-CHAR 40)))
                                       (AND (>= SOFT-CHAR #/a)
                                            (<= SOFT-CHAR #/z)
                                            (SETQ SOFT-CHAR (- SOFT-CHAR 40))))))
                            (%LOGDPB unshifted-soft-char %%KBD-CONTROL-META SOFT-CHAR)))))
                 ;;; this should probably happen before the keycode conversion, not after.  -naha
                 (insure-down-bucky-bit-consistency char)))   ;key-down
               (t                    ;0: key up
                (multiple-value-bind (soft-char unshifted-soft-char)
                   (new-lookup saved-first-char)
                  (cond ((bit-test 1_15. soft-char)
                         (kbd-bit-15-on soft-char nil)
                         nil)
                        (t (setf (aref kbd-key-state-array unshifted-soft-char) 0)))
                  (insure-up-bucky-bit-consistency char)
                  nil))))
        (t (setq saved-first-char (ldb (BYTE 7. 0.) char));its a first-byte
           nil)))

;;; get the software char corresponding to hardware char and bucky bits
(defun new-lookup (char)
  (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
    (values (AREF KBD-NEW-TABLE
                  (COND ((BIT-TEST 2 KBD-SHIFTS)     ;Greek
                         (+ (LOGAND 1 KBD-SHIFTS) 3))
                        ((BIT-TEST 4 KBD-SHIFTS) 2)  ;Top
                        ((BIT-TEST 1 KBD-SHIFTS) 1)  ;Shift
                        (T 0))
                  char)
            (aref kbd-new-table 0 char))))

;;; The magical constants appearing below are derived from what the "newer" keyboard
;;; sends and how the shift keys are encoded in kbd-left-shifts and kbd-right-shifts.
;;; See the code for tv:key-state.  The byte specifiers for ldb are those corresponding
;;; to the bit of the keyboard's "second character" corresponding to the bucky key.
;;; The byte specifiers for dpb correspond to the bits in kbd-right-shifts and
;;; kbd-left-shifts for that key.  Mask1 has ones in the positions associated with those
;;; bucky keys whose states are send with this char.

(defun insure-down-bucky-bit-consistency (char)
  (let* ((mask
          (dpb (ldb (BYTE 1. 0.) char) (BYTE 1. 1.)                             ;greek
               (dpb (ldb (BYTE 1. 1.) char) (BYTE 1. 7.)                        ;hyper
                    (dpb (ldb (BYTE 1. 2.) char) (BYTE 1. 6.)                   ;super
                         (dpb (ldb (BYTE 1. 3.) char) (BYTE 1. 5.)              ;meta
                              (dpb (ldb (BYTE 1. 4.) char) (BYTE 1. 4.)         ;control
                                   (dpb (ldb (BYTE 1. 5.) char) (BYTE 1. 0.) 0) ;shift
                                   ))))))
         (mask1 363)
         (wrong-bits (logxor mask
                             (logand mask1
                                     (logior kbd-left-shifts
                                             kbd-right-shifts)))))
    ;;; change the bits that disagree with the new information
    (setq kbd-left-shifts (logior kbd-left-shifts (logand wrong-bits mask))
          kbd-right-shifts (logior kbd-right-shifts (logand wrong-bits mask)))
    (setq kbd-left-shifts (logand kbd-left-shifts
                                  (lognot (logand wrong-bits (lognot mask))))
          kbd-right-shifts (logand kbd-right-shifts
                                   (lognot (logand wrong-bits (lognot mask)))))))


(defun insure-up-bucky-bit-consistency (char)
  (let* ((mask
          (dpb (ldb (BYTE 1. 0.) char) (BYTE 1. 2.)                             ;top
               (dpb (ldb (BYTE 1. 1.) char) (BYTE 1. 10.)                       ;repeat
                    (dpb (ldb (BYTE 1. 2.) char) (BYTE 1. 3.)                   ;caps-lock
                         (dpb (ldb (BYTE 1. 3.) char) (BYTE 1. 8.)              ;alt-lock
                              (dpb (ldb (BYTE 1. 4.) char) (BYTE 1. 9.) 0)      ;mode-lock
                              )))))
         (mask1 3414)
         (wrong-bits (logxor mask
                             (logand mask1
                                     (logior kbd-left-shifts
                                             kbd-right-shifts)))))
    ;;; change the bits that disagree with the new information
    (setq kbd-left-shifts (logior kbd-left-shifts (logand wrong-bits mask))
          kbd-right-shifts (logior kbd-right-shifts (logand wrong-bits mask)))
    (setq kbd-left-shifts (logand kbd-left-shifts
                                  (lognot (logand wrong-bits (lognot mask))))
          kbd-right-shifts (logand kbd-right-shifts
                                   (lognot (logand wrong-bits (lognot mask)))))
    ))


(defun kbd-convert-beep ()
  (select-processor
    ((:lambda :cadr)
     (%beep 1350 400000))
    (:explorer
      (and (fboundp 'tv:explorer-beep)
           (tv:explorer-beep)))))

(defvar kbd-bd-table-list                       ;lambda keyboard in explorer mode
        '((140000 140000 140000 140000 140000 )
          (206 206 206 206 206 )
          (140000 140000 140000 140000 140000 )
          (100003 100003 100003 100003 100003 )
          (226 226 226 226 226 )
          (230 230 230 230 230 )
          (100011 100011 100011 100011 100011 )
          (100007 100007 100007 100007 100007 )
          (235 235 235 235 235 )
          (236 236 236 236 236 )
          (223 223 223 223 223 )
          (204 204 204 204 204 )
          (100010 100010 100010 100010 100010 )
          (214 214 214 214 214 )
          (202 202 202 202 202 )
          (234 234 234 234 234 )
          (224 224 224 224 224 )
          (217 217 217 217 217 )
          (205 205 205 205 205 )
          (225 225 225 225 225 )
          (227 227 227 227 227 )
          (100002 100002 100002 100002 100002 )
          (231 231 231 231 231 )
          (203 203 203 203 203 )
          (173 140000 140000 140000 140000 )
          (72 14 14 140000 140000 )
          (100006 100006 100006 100006 100006 )
          (100005 100005 100005 100005 100005 )
          (100004 100004 100004 100004 100004 )
          (100044 100044 100044 100044 100044 )
          (100045 100045 100045 100045 100045 )
          (100046 100046 100046 100046 100046 )
          (100047 100047 100047 100047 100047 )
          (222 222 222 222 222 )
          (233 233 233 26 26 )
          (33 33 33 33 33 )
          (61 41 41 140000 140000 )
          (62 100 100 140000 140000 )
          (63 43 43 140000 140000 )
          (64 44 44 140000 140000 )
          (65 45 45 140000 140000 )
          (66 136 136 140000 140000 )
          (67 46 46 140000 140000 )
          (70 52 52 140000 140000 )
          (71 50 50 140000 140000 )
          (60 51 51 140000 140000 )
          (55 137 137 140000 140000 )
          (75 53 53 140000 140000 )
          (140 176 176 5 140000 )
          (175 140000 140000 140000 140000 )
          (216 216 216 216 216 )
          (100042 100042 100042 100042 100042 )
          (40 40 40 40 40 )
          (211 211 211 211 211 )
          (201 201 201 201 201 )
          (213 213 213 213 213 )
          (210 210 210 210 210 )
          (161 121 4 140000 140000 )
          (167 127 37 140000 140000 )
          (145 105 22 6 140000 )
          (162 122 23 140000 140000 )
          (164 124 20 140000 140000 )
          (171 131 21 140000 140000 )
          (165 125 24 140000 140000 )
          (151 111 16 140000 140000 )
          (157 117 25 140000 140000 )
          (160 120 17 7 140000 )
          (50 133 133 140000 140000 )
          (51 135 135 140000 140000 )
          (220 220 220 220 220 )
          (134 174 174 140000 140000 )
          (232 232 232 15 15 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (221 221 221 221 221 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (207 207 207 207 207 )
          (141 101 140000 2 140000 )
          (163 123 140000 140000 140000 )
          (144 104 140000 12 140000 )
          (146 106 140000 140000 140000 )
          (147 107 13 11 140000 )
          (150 110 1 140000 140000 )
          (152 112 30 140000 140000 )
          (153 113 31 140000 140000 )
          (154 114 27 10 140000 )
          (73 72 72 140000 140000 )
          (47 42 42 0 140000 )
          (215 215 215 215 215 )
          (212 212 212 212 212 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (100001 100001 100001 100001 100001 )
          (100000 100000 100000 100000 100000 )
          (172 132 140000 140000 140000 )
          (170 130 140000 140000 140000 )
          (143 103 32 140000 140000 )
          (166 126 140000 140000 140000 )
          (142 102 36 3 140000 )
          (156 116 34 140000 140000 )
          (155 115 35 140000 140000 )
          (54 74 74 140000 140000 )
          (56 76 76 140000 140000 )
          (57 77 77 177 140000 )
          (100040 100040 100040 100040 100040 )
          (140000 140000 140000 140000 140000 )
          (100041 100041 100041 100041 100041 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )
          (140000 140000 140000 140000 140000 )))

(defun make-good-table ()
  (loop for x in kbd-ti-bd-table-list
        collect (let ((entry (if (consp x) x (circular-list x))))
                  (loop for i from 0 below 5
                        collect (cond ((< (nth i entry) #o400)
                                       (int-char (nth i entry)))
                                      (t
                                       (nth i entry)))))))

(defvar kbd-ti-bd-table-list
        '(
          ;;x00
          (140000 140000 140000 140000 140000)      ;none
          (#/HELP #/HELP #/HELP #/HELP #/HELP)     ;HELP
          (140000 140000 140000 140000 140000)  ;none
          (100003 100003 100003 100003 100003)  ;CAPS-LOCK
          (140000 140000 140000 140000 140000)  ;BOLD-LOCK
          (140000 140000 140000 140000 140000)  ;ITALIC-LOCK
          (100011 100011 100011 100011 100011)  ;MODE-LOCK
          (100007 100007 100007 100007 100007)  ;left HYPER
          ;;x08
          (#/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM)
          (#/NETWORK #/NETWORK #/NETWORK #/NETWORK #/NETWORK)
          (#/STATUS #/STATUS #/STATUS #/STATUS #/STATUS)
          (#/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL)
          (140000 140000 140000 140000 140000)  ;none
          (#/PAGE #/PAGE #/PAGE #/PAGE #/PAGE)     ;CLEAR-SCREEN
          (#/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT)
          (140000 140000 140000 140000 140000)  ;UNDO
          ;;x10
          (#/END #/END #/END #/END #/END)
          (#/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I)      ;LEFT
          (#/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II) ;MIDDLE
          (#/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III)    ;RIGHT
          (140000 140000 140000 140000 140000)  ;F1
          (140000 140000 140000 140000 140000)  ;F2
          (140000 140000 140000 140000 140000)  ;F3
          (140000 140000 140000 140000 140000)  ;F4
          ;;x18
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          ;(140000 140000 140000 140000 140000) ;none
          (100006 100006 100006 100006 100006)
          (100005 100005 100005 100005 100005)  ;LEFT META
          (100004 100004 100004 100004 100004)  ;LEFT CONTROL
          (100044 100044 100044 100044 100044)  ;RIGHT CONTROL
          (100045 100045 100045 100045 100045)  ;RIGHT META
          (100046 100046 100046 100046 100046)  ;RIGHT SUPER
          ;;x20
          (100047 100047 100047 100047 100047)  ;RIGHT HYPER
          (#/RESUME #/RESUME #/RESUME #/RESUME #/RESUME)
          (140000 140000 140000 140000 140000)  ;none
          (#/ALTMODE #/ALTMODE #/ALTMODE #/ALTMODE #/ALTMODE)      ;ESCAPE
          (#/1 #/! #/! 140000 140000)
          (#/2 #/@ #/@ 140000 140000)
          (#/3 #/# #/# 140000 140000)
          (#/4 #/$ #/$ 140000 140000)
          ;;x28
          (#/5 #/% #/% 140000 140000)
          (#/6 #/^ #/^ 140000 140000)
          (#/7 #/& #/& 140000 140000)
          (#/8 #/* #/* 140000 140000)
          (#/9 #/( #/( 140000 140000)
          (#/0 #/) #/) 140000 140000)
          (#/- #/_ #/_ 140000 140000)
          (#/= #/+ #/+ 140000 140000)
          ;;x30
          (#/` #/{ #/{ 140000 140000)
          (#/~ #/} #/} 140000 140000)
          (#/= #/= #/= 140000 140000)        ;= on keypad
          (#/+ #/+ #/+ 140000 140000)        ;+ on keypad
          (#/SPACE #/SPACE #/SPACE #/SPACE #/SPACE)        ;space on keypad
          (#/TAB #/TAB #/TAB #/TAB #/TAB)  ;tab on keypad
          (#/BREAK #/BREAK #/BREAK #/BREAK #/BREAK)
          (140000 140000 140000 140000 140000)  ;none
          ;;x38
          (#/TAB #/TAB #/TAB #/TAB #/TAB)
          (#/q #/Q #/ 140000 140000)
          (#/w #/W #/ 140000 140000)
          (#/e #/E #/ #/ 140000)
          (#/r #/R #/ 140000 140000)
          (#/t #/T #/ 140000 140000)
          (#/y #/Y #/ 140000 140000)
          (#/u #/U #/ 140000 140000)
          ;;x40
          (#/i #/I #/ 140000 140000)
          (#/o #/O #/ 140000 140000)
          (#/p #/P #/ #/ 140000)
          (#/( #/[ #/[ 140000 140000)
          (#/) #/] #/] 140000 140000)
          (140000 140000 140000 140000 140000)  ;none
          (#/\ #/| #/| 140000 140000)
          (#/HAND-up #/HAND-up #/HAND-up 140000 140000) ;arrow up
          ;;x48
          (#/7 #/7 #/7 140000 140000)        ;7 on keypad
          (#/8 #/8 #/8 140000 140000)        ;8 on keypad
          (#/9 #/9 #/9 140000 140000)        ;9 on keypad
          (#/- #/- #/- 140000 140000)        ;- on keypad
          (#/ABORT #/ABORT #/ABORT #/ABORT #/ABORT)
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (#/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT)
          ;;x50
          (#/a #/A 140000 #/ 140000)
          (#/s #/S 140000 140000 140000)
          (#/d #/D 140000 #/
 140000)
          (#/f #/F 140000 140000 140000)
          (#/g #/G #/ #/    140000)
          (#/h #/H #/ 140000 140000)
          (#/j #/J #/ 140000 140000)
          (#/k #/K #/ 140000 140000)
          ;;x58
          (#/l #/L #/ #/ 140000)
          (#/; #/: #/: 140000 140000)
          (#/' #/" #/" #/  140000)
          (#/RETURN #/RETURN #/RETURN #/RETURN #/RETURN)
          (#/LINE #/LINE #/LINE #/LINE #/LINE)
          (#/HAND-left #/HAND-left #/HAND-left 140000 140000) ;arrow left
          (140000 140000 140000 140000 140000)  ;arrow middle
          (#/HAND-right #/HAND-right #/HAND-right 140000 140000) ;arrow right
          ;;x60
          (#/4 #/4 #/4 140000 140000)        ;4 on keypad
          (#/5 #/5 #/5 140000 140000)        ;5 on keypad
          (#/6 #/6 #/6 140000 140000)        ;6 on keypad
          (#/, #/, #/, 140000 140000)        ;, on keypad
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (100001 100001 100001 100001 100001)  ;left SYMBOL
          (100000 100000 100000 100000 100000)  ;left SHIFT
          ;;x68
          (#/z #/Z 140000 140000 140000)
          (#/x #/X 140000 140000 140000)
          (#/c #/C #/ 140000 140000)
          (#/v #/V 140000 140000 140000)
          (#/b #/B #/ #/ 140000)
          (#/n #/N #/ 140000 140000)
          (#/m #/M #/ 140000 140000)
          (#/, #/< #/< 140000 140000)
          ;;x70
          (#/. #/> #/> 140000 140000)
          (#// #/? #/? #/ 140000)
          (100040 100040 100040 100040 100040)  ;right SHIFT
          (140000 140000 140000 140000 140000)  ;none
          (100041 100041 100041 100041 100041)  ;right SYMBOL
          (#/HAND-down #/HAND-down #/HAND-down 140000 140000) ;arrow down
          (#/1 #/1 #/1 140000 140000)        ;1 on keypad
          (#/2 #/2 #/2 140000 140000)        ;2 on keypad
          ;;x78
          (#/3 #/3 #/3 140000 140000)        ;3 on keypad
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (#/SPACE #/SPACE #/SPACE #/SPACE #/SPACE)
          (140000 140000 140000 140000 140000)  ;none
          (#/0 #/0 #/0 140000 140000)        ;0 on keypad
          (#/. #/. #/. 140000 140000)        ;. on keypad
          (#/return #/return #/return 140000 140000) ;enter on keypad
          )
  )

;;reg shfit top greek ??
(defvar kbd-lmi-bd-table-list
        '(
          ;;x00
          (140000 140000 140000 140000 140000)      ;none
          (#/HELP #/HELP #/HELP #/HELP #/HELP)     ;HELP
          (140000 140000 140000 140000 140000)  ;repeat
          (100003 100003 100003 100003 100003)  ;CAPS-LOCK
          (#/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II #/ROMAN-II)
          (#/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV #/ROMAN-IV)
          (100011 100011 100011 100011 100011)  ;MODE-LOCK
          (100007 100007 100007 100007 100007)  ;left HYPER
          ;;x08
          (#/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM #/SYSTEM)
          (#/NETWORK #/NETWORK #/NETWORK #/NETWORK #/NETWORK)
          (#/STATUS #/STATUS #/STATUS #/STATUS #/STATUS)
          (#/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL #/TERMINAL)
          (140000 140000 140000 140000 140000)  ;alt lock
          (#/PAGE #/PAGE #/PAGE #/PAGE #/PAGE)     ;CLEAR-SCREEN
          (#/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT #/CLEAR-INPUT)
          (#/hand-right #/hand-right #/hand-right #/hand-right #/hand-right)
          ;;x10
          (#/END #/END #/END #/END #/END)
          (#/hold-output #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT #/HOLD-OUTPUT)
          (#/MACRO #/MACRO #/MACRO #/MACRO #/MACRO)
          (#/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I #/ROMAN-I)
          (#/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III #/ROMAN-III)
          (100002 100002 100002 100002 100002)  ;left top
          (#/hand-up #/hand-up #/hand-up 140000 140000)
          (#/call #/call #/call #/call #/call)
          ;;x18
          (#/{ #/< #/< #/< #/<)
          (#/: #/ #/ #/ #/)
          (100006 100006 100006 100006 100006)  ;left super
          (100005 100005 100005 100005 100005)  ;LEFT META
          (100004 100004 100004 100004 100004)  ;LEFT CONTROL
          (100044 100044 100044 100044 100044)  ;RIGHT CONTROL
          (100045 100045 100045 100045 100045)  ;RIGHT META
          (100046 100046 100046 100046 100046)  ;RIGHT SUPER
          ;;x20
          (100047 100047 100047 100047 100047)  ;RIGHT HYPER
          (#/RESUME #/RESUME #/RESUME #/RESUME #/RESUME)
          (#/hand-left #/hand-left #/hand-left #/circle-cross #/circle-cross)
          (#/ALTMODE #/ALTMODE #/ALTMODE #/ALTMODE #/ALTMODE)      ;ESCAPE
          (#/1 #/! #/! 140000 140000)
          (#/2 #/@ #/@ 140000 140000)
          (#/3 #/# #/# 140000 140000)
          (#/4 #/$ #/$ 140000 140000)
          ;;x28
          (#/5 #/% #/% 140000 140000)
          (#/6 #/^ #/^ 140000 140000)
          (#/7 #/& #/& 140000 140000)
          (#/8 #/* #/* 140000 140000)
          (#/9 #/( #/( 140000 140000)
          (#/0 #/) #/) 140000 140000)
          (#/- #/_ #/_ 140000 140000)
          (#/= #/+ #/+ 140000 140000)
          ;;x30
          (#/` #/~ #/~ #/not-sign #/not-sign)
          (#/} #/> #/> #/> #/>)
          (#/quote #/quote #/quote #/quote #/quote)
          (100042 100042 100042 100042 100042)  ;right top
          (#/SPACE #/SPACE #/SPACE #/SPACE #/SPACE)        ;space on keypad
          (#/TAB #/TAB #/TAB #/TAB #/TAB)  ;tab on keypad
          (#/BREAK #/BREAK #/BREAK #/BREAK #/BREAK)
          (#/delete #/delete #/delete #/delete #/delete)
          ;;x38
          (#/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE #/OVERSTRIKE)
          (#/q #/Q #/and-sign 140000 140000)
          (#/w #/W #/or-sign 140000 140000)
          (#/e #/E #/up-horseshoe #/ 140000)
          (#/r #/R #/down-horseshoe 140000 140000)
          (#/t #/T #/left-horseshoe 140000 140000)
          (#/y #/Y #/right-horseshoe 140000 140000)
          (#/u #/U #/universal-quantifier 140000 140000)
          ;;x40
          (#/i #/I #/infinity 140000 140000)
          (#/o #/O #/existential-quantifier 140000 140000)
          (#/p #/P #/partial-delta #/pi 140000)
          (#/( #/[ #/[ 140000 140000)
          (#/) #/] #/] 140000 140000)
          (#/stop-output #/stop-output #/stop-output #/stop-output #/stop-output)
          (#/\ #/| #/| 140000 140000)
          (#/HAND-down #/HAND-down #/hand-down #/circle-plus #/circle-plus)
          ;;x48
          (#/7 #/7 #/7 140000 140000)        ;7 on keypad
          (#/8 #/8 #/8 140000 140000)        ;8 on keypad
          (#/9 #/9 #/9 140000 140000)        ;9 on keypad
          (#/- #/- #/- 140000 140000)        ;- on keypad
          (#/ABORT #/ABORT #/ABORT #/ABORT #/ABORT)
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (#/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT #/RUBOUT)
          ;;x50
          (#/a #/A 140000 #/alpha 140000)
          (#/s #/S 140000 140000 140000)
          (#/d #/D 140000 #/delta 140000)
          (#/f #/F 140000 140000 140000)
          (#/g #/G #/up-arrow #/gamma 140000)
          (#/h #/H #/down-arrow 140000 140000)
          (#/j #/J #/left-arrow 140000 140000)
          (#/k #/K #/right-arrow 140000 140000)
          ;;x58
          (#/l #/L #/double-arrow #/lambda 140000)
          (#/; #/: #/: 140000 140000)
          (#/' #/" #/" #/center-dot 140000)
          (#/RETURN #/RETURN #/RETURN #/RETURN #/RETURN)
          (#/LINE #/LINE #/LINE #/LINE #/LINE)
          (#/HAND-left #/HAND-left #/HAND-left 140000 140000) ;arrow left
          (140000 140000 140000 140000 140000)  ;arrow middle
          (#/HAND-right #/HAND-right #/HAND-right 140000 140000) ;arrow right
          ;;x60
          (#/4 #/4 #/4 140000 140000)        ;4 on keypad
          (#/5 #/5 #/5 140000 140000)        ;5 on keypad
          (#/6 #/6 #/6 140000 140000)        ;6 on keypad
          (#/, #/, #/, 140000 140000)        ;, on keypad
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (100001 100001 100001 100001 100001)  ;left SYMBOL (= greek)
          (100000 100000 100000 100000 100000)  ;left SHIFT
          ;;x68
          (#/z #/Z 140000 140000 140000)
          (#/x #/X 140000 140000 140000)
          (#/c #/C #/not-equals 140000 140000)
          (#/v #/V 140000 140000 140000)
          (#/b #/B #/equivalence #/beta 140000)
          (#/n #/N #/less-or-equal 140000 140000)
          (#/m #/M #/greater-or-equal 140000 140000)
          (#/, #/< #/< 140000 140000)
          ;;x70
          (#/. #/> #/> 140000 140000)
          (#// #/? #/? #/integral 140000)
          (100040 100040 100040 100040 100040)  ;right SHIFT
          (140000 140000 140000 140000 140000)  ;none
          (100041 100041 100041 100041 100041)  ;right SYMBOL = greek
          (#/HAND-down #/HAND-down #/HAND-down 140000 140000) ;arrow down
          (#/1 #/1 #/1 140000 140000)        ;1 on keypad
          (#/2 #/2 #/2 140000 140000)        ;2 on keypad
          ;;x78
          (#/3 #/3 #/3 140000 140000)        ;3 on keypad
          (140000 140000 140000 140000 140000)  ;none
          (140000 140000 140000 140000 140000)  ;none
          (#/SPACE #/SPACE #/SPACE #/SPACE #/SPACE)
          (140000 140000 140000 140000 140000)  ;none
          (#/0 #/0 #/0 140000 140000)        ;0 on keypad
          (#/. #/. #/. 140000 140000)        ;. on keypad
          (#/return #/return #/return 140000 140000) ;enter on keypad
          )
  )

(defvar production-explorer-keyboard
        '(
          ;;lists are (unshifted shifted top greek greek-shift)
          ;;x00
          NIL
          #/HELP
          NIL
          :CAPS-LOCK
          :BOLD-LOCK
          :ITALIC-LOCK
          :MODE-LOCK
          :LEFT-HYPER
          ;;x08
          #/SYSTEM
          #/NETWORK
          #/STATUS
          #/TERMINAL
          NIL
          #/PAGE                               ;CLEAR-SCREEN
          #/CLEAR-INPUT
          NIL                                   ;UNDO
          ;;x10
          #/END
          #/ROMAN-I                            ;LEFT
          #/ROMAN-II                           ;MIDDLE
          #/ROMAN-III                          ;RIGHT
          NIL                                   ;F1
          NIL                                   ;F2
          NIL                                   ;F3
          NIL                                   ;F4
          ;;x18
          NIL
          NIL
          :left-super
          :LEFT-META
          :LEFT-CONTROL
          :RIGHT-CONTROL
          :RIGHT-META
          :RIGHT-SUPER
          ;;x20
          :RIGHT-HYPER
          #/RESUME
          NIL
          #/ALTMODE                            ;ESCAPE
          (#/1 #/! #/!)
          (#/2 #/@ #/@)
          (#/3 #/# #/#)
          (#/4 #/$ #/$)
          ;;x28
          (#/5 #/% #/%)
          (#/6 #/^ #/^)
          (#/7 #/& #/&)
          (#/8 #/* #/*)
          (#/9 #/( #/()
          (#/0 #/) #/))
          (#/- #/_ #/_)
          (#/= #/+ #/+)
          ;;x30
          (#/` #/{ #/{)
          (#/~ #/} #/})
          #/=                                  ;= on keypad
          #/+                                  ;+ on keypad
          #/SPACE                              ;space on keypad
          #/TAB                                ;tab on keypad
          #/BREAK
          NIL
          ;;x38
          #/TAB
          (#/q #/Q #/)
          (#/w #/W #/)
          (#/e #/E #/ #/)
          (#/r #/R #/)
          (#/t #/T #/)
          (#/y #/Y #/)
          (#/u #/U #/)
          ;;x40
          (#/i #/I #/)
          (#/o #/O #/)
          (#/p #/P #/ #/)
          (#/( #/[ #/[)
          (#/) #/] #/])
          NIL
          (#/\ #/| #/|)
          #/HAND-UP                            ;ARROW UP
          ;;X48
          #/7                                  ;7 on keypad
          #/8                                  ;8 on keypad
          #/9                                  ;9 on keypad
          #/-                                  ;- on keypad
          #/ABORT
          NIL
          NIL
          #/RUBOUT
          ;;x50
          (#/a #/A nil #/)
          (#/s #/S)
          (#/d #/D nil #/
)
          (#/f #/F)
          (#/g #/G #/ #/   )
          (#/h #/H #/)
          (#/j #/J #/)
          (#/k #/K #/)
          ;;x58
          (#/l #/L #/ #/)
          (#/; #/: #/:)
          (#/' #/" #/" #/ )
          #/RETURN
          #/LINE
          #/HAND-LEFT                          ;arrow left
          NIL                                   ;arrow middle
          #/HAND-RIGHT                         ;arrow right
          ;;x60
          #/4                                  ;4 on keypad
          #/5                                  ;5 on keypad
          #/6                                  ;6 on keypad
          #/,                                  ;, on keypad
          NIL
          NIL
          :left-symbol
          :left-shift
          ;;x68
          (#/z #/Z)
          (#/x #/X)
          (#/c #/C #/)
          (#/v #/V)
          (#/b #/B #/ #/)
          (#/n #/N #/)
          (#/m #/M #/)
          (#/, #/< #/<)
          ;;x70
          (#/. #/> #/>)
          (#// #/? #/? #/)
          :right-shift
          NIL
          :right-symbol
          #/HAND-DOWN                          ;arrow down
          #/1                                  ;1 on keypad
          #/2                                  ;2 on keypad
          ;;x78
          #/3                                  ;3 on keypad
          NIL
          NIL
          #/SPACE
          NIL
          #/0                                  ;0 on keypad
          #/.                                  ;. on keypad
          #/return                             ;enter on keypad
          )
  )

(defvar explorer-production-kbd-state 0)


;These two variables are bit masks for the shifting keys held down.
;Bit numbers are the same as those in the all-keys-up code sent by the
;keyboard, i.e.
;       0 shift         3 caps lock     6 super         9 mode lock
;       1 greek         4 control       7 hyper         10 repeat
;       2 top           5 meta          8 alt lock
;There are two variables so that if both shifts of a given type are pushed
;down. then one is released, we can tell what's going on.

;KBD-LEFT-SHIFTS
; KBD-RIGHT-SHIFTS
; KBD-KEY-STATE-ARRAY                   ;1 if key with that ascii code is down
;       (MAKE-ARRAY 400 ':TYPE 'ART-1B ':AREA PERMANENT-STORAGE-AREA))
;(DEFVAR KBD-KEY-STATE-ARRAY-16B                        ;For fast clearing of above array
;       (MAKE-ARRAY 20 ':TYPE 'ART-16B ':AREA PERMANENT-STORAGE-AREA
;                   ':DISPLACED-TO KBD-KEY-STATE-ARRAY))

(defun explorer-production-kbd-convert (hard-char
                                        &optional (upcase-control-chars t)
                                        )
  upcase-control-chars
  (setq hard-char (logand #o377 hard-char))
  (selectq explorer-production-kbd-state
    ;;normal
    (0
     (cond ((zerop (ldb (byte 1 7) hard-char))
            ;;key going up
            )
           )
     )
    )
  )


(DEFUN KBD-MAKE-BD-TABLE (&optional (table kbd-bd-table-list))
;This table was modified 08/20/84 to indicate which keys are repeatable if that key was held down.
;The 6th slot must have a positive number to indicate that it is repeatable.
  (LET ((TBL (MAKE-ARRAY '(6 200) ':AREA  PERMANENT-STORAGE-AREA ':TYPE 'ART-16B)))
    (DO ((J 0 (1+ J))
         (L table (CDR L)))
        ((= J 200) TBL)
      (DO ((I 0 (1+ I))
           (K (CAR L)))
          ((= I 6))
        (ASET (COND ((= I 5) (COND ((OR (ATOM K) (NULL (CAR K))) 0)
                                   (T (CAR K))))
                    ((ATOM K) (OR K 140000))
                    ((NULL (CAR K)) 140000)
                    (T (CAR K)))
              TBL I J)
        (AND (CONSP K) (SETQ K (CDR K)))))))

(defvar kbd-bd-table (kbd-make-bd-table))

(defun ti-keyboard ()
  (select-processor
    (:explorer
      (setq kbd-bd-table (kbd-make-bd-table kbd-ti-bd-table-list)))
    ((:lambda :cadr)
     (ferror nil "can't change keyboard types on this processor"))))

(defun lmi-keyboard ()
  (select-processor
    (:explorer
      (setq kbd-bd-table (kbd-make-bd-table kbd-lmi-bd-table-list)))
    ((:lambda :cadr)
     (ferror nil "can't change keyboard types on this processor"))))

(defun bd-lookup (char)
  (let ((kbd-shifts (logior kbd-left-shifts kbd-right-shifts)))
    (values (aref kbd-bd-table
                  (cond ((or (bit-test 4 kbd-shifts)    ;top when using LMI kbd.
                             (and (bit-test 2 kbd-shifts)    ;generate top
                                  (bit-test 1 kbd-shifts)))  ;on shift-greek on TI kbd.
                         2)
                        ((bit-test 2 kbd-shifts)        ;Greek
                         (+ (logand 1 kbd-shifts) 3))
                        ((bit-test 1 kbd-shifts) 1)     ;shift
                        (t 0))
                  char)
            (aref kbd-bd-table 0 char))))

(defvar ti-lock-bits 0)
(defvar ti-escape-flag nil)

;The sequence of hardware codes for a key, a, depressed and released is the following:
;   400320 400120 400000 400200
(defun kbd-convert-ti (char &optional (ctl-chars-uppercase t))
  "Keystroke conversion routine to support the TI keyboard."
;; Changed 08/20/84 to return the ascii character plus a repeatable flag (nil or t)
  (SETQ CHAR (LOGAND 377 CHAR))                 ;strip off source bits
  (COND
    ((NULL ti-ESCAPE-FLAG)                              ;is it not after an escape char?
     (COND ((BIT-TEST 1_7 CHAR)                 ;up or down code?
            (SETQ CHAR (LOGAND 177 CHAR))       ;strip off down bit
            (MULTIPLE-VALUE-BIND (SOFT-CHAR UNSHIFTED-SOFT-CHAR)
                (BD-LOOKUP CHAR)
              (COND ((BIT-TEST 1_15. SOFT-CHAR)
                     (KBD-BIT-15-ON SOFT-CHAR T)
                     (SETQ SAVED-FIRST-CHAR NIL))       ;clear previously-saved characters
                    (T                          ;normal character
                     (ASET 1 KBD-KEY-STATE-ARRAY UNSHIFTED-SOFT-CHAR)   ;set bitmap bit
                                                ;A real key depression.  Check for caps-lock.
                     (LET ((KBD-SHIFTS (LOGIOR KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS)))
                                                ;Hyper, Super, Meta, Control bits
                       (SETQ UNSHIFTED-SOFT-CHAR (LDB (BYTE 4. 4.) KBD-SHIFTS))
                       (IF (AND CTL-CHARS-UPPERCASE
                                (NOT (ZEROP UNSHIFTED-SOFT-CHAR)))
                           (IF (<= #/a SOFT-CHAR #/z)
                               (DECF SOFT-CHAR 40)      ;Control characters always uppercase,
                               (IF (<= #/A SOFT-CHAR #/Z)       ;unless  Shift is typed
                                   (INCF SOFT-CHAR 40)))
                           ;; Except for control chars for which Shift is reversed,
                           ;; consider the shift-lock key.
                           (AND (BIT-TEST 10 ti-LOCK-BITS)      ;Caps lock
                                (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
                                    (AND (>= SOFT-CHAR #/A)
                                         (<= SOFT-CHAR #/Z)
                                         (SETQ SOFT-CHAR (+ SOFT-CHAR 40)))
                                    (AND (>= SOFT-CHAR #/a)
                                         (<= SOFT-CHAR #/z)
                                         (SETQ SOFT-CHAR (- SOFT-CHAR 40))))))
                       (SETQ SAVED-FIRST-CHAR
                             (%LOGDPB UNSHIFTED-SOFT-CHAR %%KBD-CONTROL-META SOFT-CHAR)))))))
           (T                                   ;0: key up or escape code
            (COND ((ZEROP CHAR)
                   (SETQ ti-ESCAPE-FLAG T)
                   NIL)
                  (T
                   (MULTIPLE-VALUE-BIND (SOFT-CHAR UNSHIFTED-SOFT-CHAR)
                       (bd-LOOKUP CHAR)
                     (COND ((BIT-TEST 1_15. SOFT-CHAR)
                            (KBD-BIT-15-ON SOFT-CHAR NIL)
                            NIL)
                           (T (ASET 0 KBD-KEY-STATE-ARRAY UNSHIFTED-SOFT-CHAR)))
                     NIL))))))
    (T
     (PROG1
       (SETQ ti-ESCAPE-FLAG NIL)                        ;its a second byte (after escape byte)
       (IF (BIT-TEST 1_7 CHAR)                  ;all keys up?
           (PROG1
             (SETQ ti-LOCK-BITS (LOGAND 17 CHAR))
             (SETQ KBD-LEFT-SHIFTS 0)           ;all keys up, clear the status
             (SETQ KBD-RIGHT-SHIFTS 0))))
     NIL)))


;;;; Hardware primitives
;;;; Support for "Simple TV" (32-bit TV system)
;;;;Some special variables used by the hardware routines
;(DECLARE (SPECIAL CPT-SYNC2 COLOR-SYNC CPT-SYNC-60HZ))

;;;; Read and write the sync program
;(DEFUN READ-SYNC (ADR &OPTIONAL (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;  (%XBUS-WRITE (+ TV-ADR 2) ADR)               ;Set pointer
;  (LOGAND 377 (%XBUS-READ (+ TV-ADR 1))))

;(DEFUN WRITE-SYNC (ADR DATA &OPTIONAL (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;  (%XBUS-WRITE (+ TV-ADR 2) ADR)               ;Set pointer
;  (%XBUS-WRITE (+ TV-ADR 1) DATA))

;;;; Start and stop the sync program
;(DEFUN START-SYNC (CLOCK BOW VSP
;                  &OPTIONAL (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;  (%XBUS-WRITE TV-ADR (+ (LSH BOW 2) CLOCK))
;  (%XBUS-WRITE (+ TV-ADR 3) (+ 200 VSP)))

;(DEFUN STOP-SYNC (&OPTIONAL (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;  (%XBUS-WRITE (+ TV-ADR 3) 200))              ;Disable output of sync

;;;; Write into the sync program from a list with repeat-counts
;;;; Sub-lists are repeated <car> times.
;(DEFUN FILL-SYNC (L &OPTIONAL (ADR 0) (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN))
;                   &AUX X)
;  (DO ((L L (CDR L))) ((NULL L) ADR)
;    (SETQ X (CAR L))
;    (COND ((ATOM X) (WRITE-SYNC ADR X TV-ADR) (SETQ ADR (1+ ADR)))
;         (T (DO N (CAR X) (1- N) (ZEROP N)
;                (SETQ ADR (FILL-SYNC (CDR X) ADR TV-ADR)))))))

;(DEFUN CHECK-SYNC (L &OPTIONAL (ADR 0)  (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN))
;                  &AUX X)
;  (DO ((L L (CDR L))) ((NULL L) ADR)
;    (SETQ X (CAR L))
;    (COND ((ATOM X)
;          (CHECK-SYNC-WD ADR X TV-ADR)
;          (SETQ ADR (1+ ADR)))
;         (T (DO N (CAR X) (1- N) (ZEROP N)
;              (SETQ ADR (CHECK-SYNC (CDR X) ADR TV-ADR)))))))

;(DEFUN CHECK-SYNC-WD (ADR DATA TV-ADR &AUX MACH)
;  (COND ((NOT (= DATA (SETQ MACH (READ-SYNC ADR TV-ADR))))
;        (FORMAT T "~%ADR:~S MACH: ~S should be ~S" ADR MACH DATA))))


;;Initialize the TV.  Name of this function is obsolete.
;;If FORCE-P is T, then SYNC-PROG is always loaded.
;;Otherwise, it is a default to be loaded only if there is no prom.
;(DEFUN SETUP-CPT (&OPTIONAL (SYNC-PROG CPT-SYNC2)
;                           (TV-ADR NIL)
;                           (FORCE-P NIL))
;  FORCE-P
;  (SELECT-PROCESSOR
;    (:CADR
;     (IF (NULL TV-ADR) (SETQ TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;     ;; Always turn on vertical sync interrupts if this is the first TV controller.
;     ;; The microcode relies on these as a periodic clock for various purposes.
;     ;; If not the first controller, leave the interrupt enable the way it is.
;     (WITHOUT-INTERRUPTS
;      (LET ((INTERRUPT-ENABLE (IF (= TV-ADR 377760) 10 ;This is the number UCADR knows
;                                 (LOGAND (%XBUS-READ TV-ADR) 10)))
;           (STATUS (%XBUS-READ TV-ADR)))
;       STATUS
;       (COND (NIL
;              ;never using the prom, so as to assure software and hardware agree as to
;              ; what the screen geometry is.
;              (COMMENT
;               (AND (NOT FORCE-P)                      ;Unless forced, try to use the PROM
;                    (OR (ZEROP (LOGAND STATUS 200))    ;Good, PROM already on
;                        (PROGN (PROM-SETUP TV-ADR)     ;Try turning it on
;                               (ZEROP (LOGAND (%XBUS-READ TV-ADR) 200)))))  ;On now?
;               ;; The hardware at least claims the PROM is turned on.  Actually
;               ;; checking for working sync does not work for some reason, so just
;               ;; assume that any board which can have a PROM does have one, and
;               ;; always use the PROM if it is there, since it is more likely to
;               ;; be right than the default sync program.
;               ;; Now turn on black-on-white mode, and interrupt enable if desired.
;               (%XBUS-WRITE TV-ADR (+ 4 INTERRUPT-ENABLE))))
;             (T ;; Must be an ancient TV board at MIT, or else forced
;              ;; Use default (or forced) sync program
;              (STOP-SYNC TV-ADR)
;              (FILL-SYNC SYNC-PROG 0 TV-ADR)
;              (START-SYNC INTERRUPT-ENABLE 1 0 TV-ADR))))))    ;Clock 0, BOW 1, VSP 0
;    ((:LAMBDA :EXPLORER) NIL)))

;(DEFUN PROM-SETUP (&OPTIONAL (TV-ADR TV::(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
;  (%XBUS-WRITE (+ TV-ADR 3) 0))


;;sync program bits
;;1  HSYNC
;;2  VSYNC
;;4  COMPOSITE - (not used really, encode on HSYNC)
;;10  BLANKING
;;     0  PROC CYC
;;     20  REFRESH
;;     40  INC TVMA
;;     60  STEP TVMA
;;     0    --
;;     100  CLR TVMA
;;     200  EOL
;;     300  EOP
;;Assume 60MHZ bit clock, therefore 15Mhz (66.7 ns) TTL clock, 533ns SYNC clk
;; 30. sync clks per line, 10. for horz sync,
;;41.6 lines for 666 usec vertical
;;1037 lines per 16.66 ms frame


;; 640. X 896.
;;(SETQ CPT-SYNC '(
;;   1.  (2 33) (8 13) (18. 12) 212 112
;;   45.  (2 33) (8 13) (18. 12) 212 12
;;   8.  (2 33)  (6 13) 13 12 (18. 2) 202 2
;;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;;   131. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;;   8. (2 31) (6 11) 11 10 (8. 0 0) 0 0 300 0
;;))

;;704. x 896.
;;(SETQ CPT-SYNC1 '(
;;   1.  (1 33) (5 13) 12 12 (10. 12 12) 212 113                        ;VERT SYNC, CLEAR TVMA
;;   53.  (1 33) (5 13) 12 12 (10. 12 12) 212 13                        ;VERT RETRACE
;;   8.  (1 31)  (5 11) 11 10 (10. 0 0) 200 21          ;8 LINES OF MARGIN
;;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;;   131. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;;   7. (1 31) (5 11) 11 10 (10. 0 0) 200 21
;;   1. (1 31) (5 11) 11 10 (10. 0 0) 300 23
;;))

;;Sync for 64 Mhz crystal,  768. x 963.  (was 896 for CPT)
;; This is the default thing.  The vertical repetition rate is 60Hz,
;;and the extra screen space is provided to the user
;;(as compared to the original CPT sync load).

;(DEFCONST CPT-SYNC2 '(
;   1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113                 ;VERT SYNC, CLEAR TVMA
;   53.  (1 33) (5 13) 12 12 (11. 12 12) 212 13                 ;VERT RETRACE
;   8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21           ;8 LINES OF MARGIN
;   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
;   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
;   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
;   198. (1 31) (5 11) 11 50 (11. 0 40) 200 21
;   7. (1 31) (5 11) 11 10 (11. 0 0) 200 21
;   1. (1 31) (5 11) 11 10 (11. 0 0) 300 23
;))

;;;; This is the CPT-SYNC2 program (locations are in octal, repeats in decimal)
;;Loc   Rpt     Hsync   Vsync   Comp    Blank   Other1          Other2
;;0     1
;;1             X       X               X       Refresh
;;2-6           X       X               X
;;7-36                  X               X
;;37                    X               X                       Eol
;;40            X       X               X                       Clr MA
;;41    53.
;;42            X       X               X       Refresh
;;43-47         X       X               X
;;50-77                 X               X
;;100                   X               X                       Eol
;;101           X       X               X                       Clr MA
;;102   8.
;;103           X                       X       Refresh
;;104

;This is used for making an instance in the cold-load environment,
;so that we can display on the TV in the cold-load stream.
;The instance variable slots get initialized to NIL.  Note that
;the method-alist gets used as the dtp-select-method, so it must have
;the unknown-message handler in cdr of its last.
(DEFUN FAKE-UP-INSTANCE (TYPENAME INSTANCE-VARIABLES METHOD-ALIST INIT-PLIST-GENERATOR
                         &AUX INIT-OPTIONS SIZE BINDINGS INSTANCE DESCRIPTOR)
  (SETQ INIT-OPTIONS (FUNCALL INIT-PLIST-GENERATOR))
  ;; Note that the length of this array must agree with INSTANCE-DESCRIPTOR-OFFSETS in QCOM
  (SETQ DESCRIPTOR (MAKE-ARRAY 10 ':AREA PERMANENT-STORAGE-AREA))
  (setf (aref DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-SIZE))
        (SETQ SIZE (1+ (LENGTH INSTANCE-VARIABLES))))
  (SETF (AREF DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-ALL-INSTANCE-VARIABLES))
        INSTANCE-VARIABLES)
  (SETQ BINDINGS (MAKE-LIST (LENGTH INSTANCE-VARIABLES) :AREA PERMANENT-STORAGE-AREA))
  (DO ((B BINDINGS (CDR B))
       (L INSTANCE-VARIABLES (CDR L)))
      ((NULL L))
    (RPLACA B (VALUE-CELL-LOCATION (CAR L))))
  (SETF (AREF DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-BINDINGS)) BINDINGS)
  (SETF (AREF DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-FUNCTION))
        (%MAKE-POINTER DTP-SELECT-METHOD METHOD-ALIST))
  (SETF (AREF DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-TYPENAME)) TYPENAME)
  (SETQ INSTANCE (%make-structure dtp-instance
                                  dtp-instance-header
                                  descriptor
                                  nil
                                  permanent-storage-area
                                  size
                                  size))
  (SEND INSTANCE ':INIT (LOCF INIT-OPTIONS))
  INSTANCE)

(DEFUN COLD-LOAD-STREAM-INIT-PLIST-GENERATOR ()
  `(:WIDTH ,(SELECT-PROCESSOR
              (:CADR 1400)
              (:LAMBDA 1440)
              (:explorer 1024.)
              )
    ;; Lambda height (800.) is desirable in case we ever try to build a system on a
    ;; landscape monitor.  It is set to a more reasonable value later.  Note that a
    ;; microcode bug (%draw-rectangle can write 1 word out of bounds) precludes using
    ;; the entire physical screen.
    :HEIGHT ,(SELECT-PROCESSOR
               (:CADR 1600)
               ;(:LAMBDA 1000.)
               (:LAMBDA 770.)
               (:explorer 754.)
               )
    :LOCATIONS-PER-LINE ,(SELECT-PROCESSOR
                           (:CADR 24.)
                           (:LAMBDA 32.)
                           (:explorer 32.))
    :BUFFER ,IO-SPACE-VIRTUAL-ADDRESS
    :CONTROL-ADDRESS 377760))

(MAKE-INSTANCE-IMMEDIATE COLD-LOAD-STREAM COLD-LOAD-STREAM-INIT-PLIST-GENERATOR)

(DEFUN SET-COLD-LOAD-STREAM-HEIGHT (HEIGHT)
  ;; Error check.  Can't make height greater than what it was created with (above).
  (UNLESS ( HEIGHT 1000.)
    (%P-STORE-CONTENTS-OFFSET HEIGHT COLD-LOAD-STREAM 3)))

(DEFUN SET-COLD-LOAD-STREAM-WIDTH (WIDTH)
  ;; Must be less than 32 (bits) times the number of locations per line.
  (UNLESS ( WIDTH (* 32. (%P-CONTENTS-OFFSET COLD-LOAD-STREAM 2)))
    (%P-STORE-CONTENTS-OFFSET WIDTH COLD-LOAD-STREAM 4)))

;Avoid lossage when processes are in use but window system is not loaded yet.
(OR (FBOUNDP 'TV::BACKGROUND-STREAM)
    (FSET 'TV::BACKGROUND-STREAM COLD-LOAD-STREAM))

;:NORMAL to not do when the ADD-INITIALIZATION is executed, only when it is
;time to do the system-initializations
; SETUP-CPT is now called directly from LISP-REINITIALIZE.  Problem was, on a cold boot,
;references to the video buffer could happen before the sync program was set up.
;This caused main memory parity errors even though the TV sends back ignore parity!
;One path that caused trouble was PROCESS-INITIALIZE, (PROCESS-CLASS :RESET),
;TV:WHO-LINE-PROCESS-CHANGE, etc.
;(ADD-INITIALIZATION "CPT-SYNC" '(SETUP-CPT CPT-SYNC2) '(:SYSTEM :NORMAL))

;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; The editor stream

;;;??? The current way of printing BARFs isn't all THAT good.

(DEFVAR *STREAM-COMTAB* :UNBOUND
  "The comtab used by the Lisp edit window.")

(DEFVAR *INSIDE-EDITOR-STREAM* NIL
  "T when inside the editor stream for ordinary I/O
but not when inside the editor itself.")

(DEFVAR *STREAM-IBEAM-SHOULD-BLINK* NIL
  "T inside the editor within the editor stream when i-beam blinker should blink.
This is true while in the /"editing;/" state or when waiting for activation.")

(DEFVAR *EDITOR-STREAM-REQUIRE-ACTIVATION* NIL
  "T in an editor stream if should require an activate command before letting input be read.")
(DEFPROP *EDITOR-STREAM-REQUIRE-ACTIVATION* T MODE-SETTABLE-P)

(DEFCONST *STREAM-IBEAM-BLINKER-P* T
  "If T, we give editor streams ibeam blinkers.")

(DEFVAR *STREAM-DEFER-OUTPUT-NOT-AT-END* T
  "Non-NIL defers all output to editor stream which is not at the end of the buffer
until the next attempt to read input, which will redisplay it all.")

(DEFVAR EDITOR-STREAM-DEFER-REDISPLAY NIL
  "Non-NIL causes output to editor stream not to come out on screen for the moment.
Causes redisplay to be done all at once, later.")

(ADD-INITIALIZATION 'EDITOR-STREAM
                    '(INITIALIZE-EDITOR-STREAM-FLAGS)
                    '(:WARM))

(DEFUN INITIALIZE-EDITOR-STREAM-FLAGS ()
  (SETQ *INSIDE-EDITOR-STREAM* NIL)
  (SETQ EDITOR-STREAM-DEFER-REDISPLAY NIL)
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* NIL))

(DEFFLAVOR EDITOR-STREAM-MIXIN
       (TV:IO-BUFFER
        (*INSIDE-EDITOR-STREAM* T)
        *STREAM-SHEET*
        *STREAM-START-BP*
        (*STREAM-COMMAND-POINT* NIL)
        (*STREAM-INPUT-HISTORY* NIL)
        *STREAM-BP*
        *STREAM-BLINKER*)
       ()
  (:DEFAULT-INIT-PLIST :IBEAM-BLINKER-P T
    :EDITOR-CLOSURE-VARIABLES EDITOR-STREAM-CLOSURE-VARIABLES
    :COMTAB *STREAM-COMTAB*
    :MODE-LINE-LIST '("EDITOR-STREAM " "(" *MODE-NAME-LIST* ")"
                      (*EDITOR-STREAM-ACTIVATION-NEEDED* " {Not activating}"
                                                         :ELSE "")))
  (:REQUIRED-FLAVORS TOP-LEVEL-DISPLAYER-MIXIN DISPLAYER)
  (:INITABLE-INSTANCE-VARIABLES *STREAM-SHEET*)
  (:GETTABLE-INSTANCE-VARIABLES *STREAM-SHEET* *STREAM-INPUT-HISTORY*)
  (:SETTABLE-INSTANCE-VARIABLES *STREAM-COMMAND-POINT*)
  (:SPECIAL-INSTANCE-VARIABLES *INSIDE-EDITOR-STREAM*)
  (:INIT-KEYWORDS :IBEAM-BLINKER-P))

(DEFVAR *EDITOR-STREAM-START-BP* :UNBOUND)
(DEFVAR *EDITOR-STREAM* :UNBOUND)
(DEFVAR *EDITOR-STREAM-ACTIVATION-NEEDED* NIL)

(DEFCONST EDITOR-STREAM-CLOSURE-VARIABLES
          `((*EDITOR-STREAM-START-BP* NIL)
            (*EDITOR-STREAM* NIL)
            (*EDITOR-STREAM-ACTIVATION-NEEDED* NIL)
            . ,TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(defmethod (editor-stream-mixin :before :init) (ignore)
  ;;When the Interval is created, make it a zwei:file-buffer
  (setq interval 'file-buffer))

(DEFMETHOD (EDITOR-STREAM-MIXIN :AFTER :INIT) (INIT-PLIST)
  (SETQ *STREAM-INPUT-HISTORY* (MAKE-HISTORY (STRING-APPEND "input history of "
                                                            (SEND *STREAM-SHEET* ':NAME))))
  (SETQ TV:IO-BUFFER (SEND *STREAM-SHEET* ':IO-BUFFER))
  (SETQ *STREAM-BLINKER* (WINDOW-POINT-BLINKER *STREAM-SHEET*))
  (AND (GET INIT-PLIST ':IBEAM-BLINKER-P)
       (LET ((BLINKER (TV:MAKE-BLINKER *STREAM-SHEET* 'STREAM-IBEAM-BLINKER
                                       ':EDITOR-STREAM SELF ':VISIBILITY NIL )))
         (SETF (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)
               (DELQ (ASSQ 'STREAM-BLINK-IBEAM (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))
                     (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)))
         (PUSH `(STREAM-BLINK-IBEAM . ,BLINKER)
               (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))))
  (let ((name (tv:sheet-name *stream-sheet*)))
    (setf (symeval-in-instance interval 'name) name)
    (setf (symeval-in-instance interval 'pathname) (make-pathname :host si:local-host :name (string-upcase name))))
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (SETQ *EDITOR-STREAM* SELF
        *WINDOW* *STREAM-SHEET*
        *EDITOR-STREAM-START-BP* *STREAM-START-BP*)
  (PUSH 'STREAM-PRE-COMMAND-HOOK *COMMAND-HOOK*)
  (PUSH 'STREAM-COMMAND-HOOK *POST-COMMAND-HOOK*))

(DEFMETHOD (EDITOR-STREAM-MIXIN :AFTER :SET-INTERVAL-INTERNAL) (IGNORE)
  (LET ((INT (WINDOW-INTERVAL *STREAM-SHEET*)))
    (IF (VARIABLE-BOUNDP *STREAM-START-BP*)
        (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP INT))
      (SETQ *STREAM-START-BP* (COPY-BP (INTERVAL-LAST-BP INT) ':NORMAL)))
    (SETQ *STREAM-BP* (WINDOW-POINT *STREAM-SHEET*))
    (MOVE-BP *STREAM-BP* *STREAM-START-BP*)))

;; The window used by an editor stream must support the operations
;; :ENTER-EDITOR, :EXIT-EDITOR and :ACTIVATION-NEEDED.  These tell the window
;; what state the editor stream is in, so the window can display something
;; to tell the user.  :ENTER-EDITOR says the stream is now running the editor.
;; :EXIT-EDITOR says the stream is no longer inside the editor.
;; :ACTIVATION-NEEDED says the stream is inside the editor
;; and will not leave the editor until the user gives an explicit
;; Activate command.

;; ZWEI windows, which are used by ZTOP streams, just ignore these operations
;; since ZTOP can use the ordinary editor mode line to display the information.
;; EDITOR-STREAM-WINDOWs, since they do not have a mode line permanently displayed,
;; use those operations to display that information in the label.

;;; This is an editor window which is also an editor stream instead
;;; of an ordinary sort of stream.
(DEFFLAVOR EDITOR-STREAM-WINDOW
        ((LABEL-STATE NIL)
         (BASE-TICK *TICK*))
        (EDITOR-STREAM-MIXIN TOP-LEVEL-DISPLAYER-MIXIN
         EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN ZWEI-WINDOW)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :MORE-P T :RIGHT-MARGIN-CHARACTER-FLAG 0))

(DEFMETHOD (EDITOR-STREAM-WINDOW :AFTER :INIT) (IGNORE)
  (FUNCALL-SELF ':EXIT-EDITOR)
  (EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN-POSITION-MODE-LINE-WINDOW))

(DEFMETHOD (EDITOR-STREAM-WINDOW :BEFORE :INIT) (IGNORE)
  (SETQ *STREAM-SHEET* SELF))

(DEFMETHOD (EDITOR-STREAM-WINDOW :AROUND :PRINT-NOTIFICATION)
           (CONTIN MAP CONTIN-ARGS &REST IGNORE)
  (WITH-BP (TEM *STREAM-BP* ':MOVES)
    (WITH-BP (OLD-POINT POINT ':MOVES)
      (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
      (LET ((EDITOR-STREAM-DEFER-REDISPLAY T))
        (SI:AROUND-METHOD-CONTINUE CONTIN MAP CONTIN-ARGS))
      (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
      (MOVE-BP POINT OLD-POINT)
      (MOVE-BP *STREAM-BP* TEM)))
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-WINDOW :ENTER-EDITOR) ()
  (COND ((EQ LABEL-STATE ':READING)
         (SEND SELF ':DELAYED-SET-LABEL
               (FORMAT NIL "~A  Editing; insert text at end to resume reading" TV:NAME))
         (SETQ LABEL-STATE ':EDITOR))))

(DEFMETHOD (EDITOR-STREAM-WINDOW :EXIT-EDITOR) ()
  (COND ((NEQ LABEL-STATE ':READING)
         (IF TV:LABEL-NEEDS-UPDATING
             ;; An :ENTER-EDITOR has been done but label hasn't redisplayed yet.
             ;; Just cancel the label redisplay, since it is really still right.
             (SETQ TV:LABEL-NEEDS-UPDATING NIL)
           ;; Label is really wrong.
           (SEND SELF ':SET-LABEL
                 (FORMAT NIL "~A  Actively reading and evaluating" TV:NAME)))
         (SETQ LABEL-STATE ':READING))))

(DEFMETHOD (EDITOR-STREAM-WINDOW :ACTIVATION-NEEDED) ()
  (COND ((NEQ LABEL-STATE ':ACTIVATION-NEEDED)
         (SETQ TV:LABEL-NEEDS-UPDATING NIL)
         (FUNCALL-SELF ':SET-LABEL (FORMAT NIL "~A  Type ~A to resume reading of input"
                                           (FUNCALL-SELF ':NAME)
                                           (KEY-FOR-COMMAND 'COM-ACTIVATE
                                                            (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE
                                                                                *COMTAB*)
                                                            NIL NIL #/END)))
         (SETQ LABEL-STATE ':ACTIVATION-NEEDED))))

;Inhibit ZWEI-FRAME's tendency to put the label into the name in the select menu.
(DEFMETHOD (EDITOR-STREAM-WINDOW :NAME-FOR-SELECTION) ()
  TV:NAME)

;;; Basic I/O operations on editor streams.
;;; The output operations here type directly on the sheet
;;; as well as updating the buffer.

;;; The input operations here have special hacks to detect when they are called
;;; inside the rubout handler.  Note that RUBOUT-HANDLER is non-nil when
;;; input is being done with rubout handling (editing, in our case);
;;; but it is NIL when input is being done on behalf of the rubout handler.
;;; *EDITOR-STREAM-ALREADY-KNOWS* is T at that time; it is how you can
;;; distinguish the two reasons why RUBOUT-HANDLER might be NIL.
(DEFVAR *EDITOR-STREAM-ALREADY-KNOWS* NIL)

;;; *INSIDE-EDITOR-STREAM* is NIL when inside the editor proper or editor redisplay
;;; (This is not quite the same as when *EDITOR-STREAM-ALREADY-KNOWS* is T);
;;; T at all other times while inside operations on the editor stream.
;;; It enables more processing and glitching in ZWEI windows,
;;; which must not happen in the editor redisplay.

(DEFMACRO STREAM-IMMEDIATE-OUTPUT (&BODY BODY)
  "Perform BODY if no redisplay is pending and direct output is feasible.
It may also do the redisplay and then perform BODY.
BODY should be code to do output to have the effect of redisplay
for changes already made to the buffer."
  `(OR (STREAM-MAYBE-REDISPLAY)
       (PROGN . ,BODY)))

(DEFMETHOD (EDITOR-STREAM-MIXIN :TYO) (CH)
  (LET ((*WINDOW* *STREAM-SHEET*))
    (INSERT-MOVING *STREAM-BP* CH)
    (STREAM-IMMEDIATE-OUTPUT
      (TV:SHEET-TYO *STREAM-SHEET* CH))))

(defmethod (EDITOR-STREAM-MIXIN :STRING-OUT) (STR &OPTIONAL (START 0) END)
  (LET ((*WINDOW* *STREAM-SHEET*))
    (INSERT-MOVING *STREAM-BP* (IF (AND (ZEROP START) (NULL END)) STR
                                 (NSUBSTRING STR START END)))
    (STREAM-IMMEDIATE-OUTPUT
      (tv:sheet-string-out *stream-sheet* str start end))))

(DEFMETHOD (EDITOR-STREAM-MIXIN :LINE-OUT) (STR &OPTIONAL (START 0) END)
  (LET ((*WINDOW* *STREAM-SHEET*))
    (INSERT-MOVING *STREAM-BP* (IF (AND (ZEROP START) (NULL END)) STR
                                 (NSUBSTRING STR START END)))
    (INSERT-MOVING *STREAM-BP* #/RETURN)
    (STREAM-IMMEDIATE-OUTPUT
      (TV:SHEET-STRING-OUT *STREAM-SHEET* STR START END)
      (TV:SHEET-CRLF *STREAM-SHEET*))))

(DEFMETHOD (EDITOR-STREAM-MIXIN :AROUND :LISTEN) (CONT MT ARGS)
  (OR (NOT (OR (neq rubout-handler self)
               (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
      (AROUND-METHOD-CONTINUE CONT MT ARGS)))

(DEFMETHOD (EDITOR-STREAM-MIXIN :BEFORE :CLEAR-INPUT) ()
  (UNLESS (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
    (LET ((*WINDOW* *STREAM-SHEET*)
          (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
      (KILL-INTERVAL *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)) T))
    (FORMAT SELF
            "~&Killing the rest of the buffer to clear input.  It is on the kill ring.")))

(DEFMETHOD (EDITOR-STREAM-MIXIN :ANY-TYI-NO-HANG) (&OPTIONAL IGNORE)
  (when (eq rubout-handler self)
    (FERROR NIL ":ANY-TYI-NO-HANG while inside RUBOUT-HANDLER"))
  (TV:KBD-IO-BUFFER-GET TV:IO-BUFFER T))

;;; Actual rubout handling.

(DEFMETHOD (EDITOR-STREAM-MIXIN :ANY-TYI) (&OPTIONAL IGNORE)
  (IF (eq rubout-handler self)
      ;; If input is being edited...
      (IF (AND (NULL *SRE-INPUT-POINT*)
               (NOT (BP-= *STREAM-BP*
                          (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
          ;; If we have more to fetch from the buffer, just fetch it.
          (PROG1 (LDB %%CH-CHAR (BP-CHAR *STREAM-BP*))  ;Give buffered character if any
                 (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)        ;We are moving point, so...
                 (LET ((*INTERVAL* INTERVAL))
                   (IBP *STREAM-BP*)))
        ;; We have to get more input, probably invoking the editor.
        (LET ((*EDITOR-STREAM-ALREADY-KNOWS* T))
          (SEND SELF ':STREAM-RUBOUT-HANDLER)))
    ;; If input is not being edited
    ;; Then we read it directly, as if were not an editor stream.
    ;; Make sure screen is right before we read it.
    ;; But if came from within the rubout handler, that is already done.
    (OR *EDITOR-STREAM-ALREADY-KNOWS*
        (STREAM-REDISPLAY))
    (cond ((TV:KBD-IO-BUFFER-GET TV:IO-BUFFER T))
          (t
           (FUNCALL-SELF ':NOTICE ':INPUT-WAIT)
           (TV:KBD-IO-BUFFER-GET TV:IO-BUFFER)))))

(DEFMETHOD (EDITOR-STREAM-MIXIN :UNTYI) (CH)
  (IF (eq rubout-handler self)
      (LET ((*INTERVAL* (SEND *STREAM-SHEET* ':INTERVAL)))
        (DBP *STREAM-BP*))
    (TV:IO-BUFFER-UNGET TV:IO-BUFFER CH)))

(DEFVAR *SRE-STREAM-BP*)
(DEFVAR *SRE-STREAM-START-BP*)
(DEFVAR *SRE-WINDOW*)

;; These are set to copied bps to the end of the interval and to point,
;; whenever a :EDITING-COMMAND character is encountered.
;; This is so that the character can do output and then do a :REFRESH-RUBOUT-HANDLER.
(DEFVAR *SRE-INPUT-END-BP* NIL)
(DEFVAR *SRE-INPUT-POINT* NIL)

;; If we encounter a :ACTIVATION or :DO-NOT-ECHO, we put here the value
;; to return for that character and force a rescan (all in the pre-command hook).
;; When the input is all rescanned, the :STREAM-RUBOUT-HANDLER operation
;; sees this is non-NIL; then it returns this value to its caller (:ANY-TYI)
;; and clears it out.
(DEFVAR *SRE-ACTIVATION-CHARACTER* NIL)

;; This is the "rubout handler" itself.  We call the supplied function
;; after binding RUBOUT-HANDLER to SELF.  That function, which might be READ,
;; does its input as usual, but the :ANY-TYI operation acts differently
;; because RUBOUT-HANDLER is non-NIL.  Specifically, it may invoke :STREAM-RUBOUT-HANDLER.
(DEFMETHOD (EDITOR-STREAM-MIXIN :RUBOUT-HANDLER) (options FUNCTION
                                                  &REST ARGS &AUX TEM
                                                  (*WINDOW* *STREAM-SHEET*)
                                                  *STREAM-DEFER-OUTPUT-NOT-AT-END*
                                                  COMMAND-POINT)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq ':nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
        (apply function args))
    (let ((rubout-handler-options options))
      (IF *STREAM-COMMAND-POINT*
          (PROGN
            (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
            (SETQ COMMAND-POINT *STREAM-COMMAND-POINT*)
            (FLUSH-BP COMMAND-POINT)
            (SETQ *STREAM-COMMAND-POINT* NIL))
        (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
      (LET ((PROMPT-OPTION (ASSQ ':PROMPT rubout-handler-options)))
        (WHEN PROMPT-OPTION
          (IF (STRINGP (CADR PROMPT-OPTION))
              (PRINC (CADR PROMPT-OPTION) SELF)
            (FUNCALL (CADR PROMPT-OPTION) SELF NIL))
          (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))))
      (LET ((INITIAL-INPUT (CADR (ASSQ ':INITIAL-INPUT rubout-handler-options)))
            (INITIAL-INPUT-POINTER (CADR (ASSQ ':INITIAL-INPUT-POINTER rubout-handler-options)))
            (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))      ;need this in forward-char
        (WHEN INITIAL-INPUT
          (INSERT *STREAM-START-BP* INITIAL-INPUT)
          (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
          (OR COMMAND-POINT
              (NOT INITIAL-INPUT-POINTER)
              (SETQ COMMAND-POINT (FORWARD-CHAR *STREAM-START-BP* INITIAL-INPUT-POINTER)))))
      (STREAM-MAYBE-REDISPLAY)
      (*CATCH 'TV:RETURN-FROM-RUBOUT-HANDLER
        (DO ((RUBOUT-HANDLER self)                      ;Establish rubout handler
             (*SRE-ACTIVATION-CHARACTER* NIL))
            (())
          (WITH-BP (START-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
            (WITH-BP (END-OF-MSG-BP *STREAM-START-BP* ':NORMAL)
              (*CATCH 'RUBOUT-HANDLER
                (CONDITION-CASE (ERROR)
                    (LET ((*SRE-STREAM-BP* *STREAM-BP*)
                          (*SRE-STREAM-START-BP* *STREAM-START-BP*)
                          (*SRE-WINDOW* *STREAM-SHEET*)
                          *SRE-INPUT-END-BP*
                          (*SRE-INPUT-POINT* COMMAND-POINT))
                      (CONDITION-BIND ((ERROR 'STREAM-READ-ERROR-HANDLER))
                        (RETURN
                          (multiple-value-prog1
                            (APPLY FUNCTION ARGS)
                            (LET ((*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
                              (DELETE-INTERVAL *STREAM-BP*
                                               (INTERVAL-LAST-BP *INTERVAL*)))))))
                  (SYS:PARSE-ERROR
                   (LET ((*STREAM-DEFER-OUTPUT-NOT-AT-END* T))
                     (fresh-line SELF)
                     (PRINC ">>ERROR: " SELF)
                     (SEND ERROR ':REPORT SELF)
                     (fresh-line SELF))
                   (MOVE-BP *STREAM-START-BP* *STREAM-BP*)
                   (MOVE-BP END-OF-MSG-BP *STREAM-START-BP*)
                   (MOVE-BP *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
                   (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
                   (STREAM-REDISPLAY)
                   (do-forever (FUNCALL-SELF ':TYI)))))
              ;; Here if editor throws to RUBOUT-HANDLER
              ;; to cause the input we have to be read over again.
              ;; First, delete any error message we got from a previous parsing.
              (COND ((NOT (BP-= START-OF-MSG-BP END-OF-MSG-BP))
                     (DELETE-INTERVAL START-OF-MSG-BP END-OF-MSG-BP T)
                     (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
                     (STREAM-REDISPLAY T)))
              ;; Now start over again reading from the front of the input.
              (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
              (SETQ COMMAND-POINT NIL)
              (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)))
          ;; When a rubout or other editing operation is done, throws back to that
          ;; catch to reread the input.  But if the :FULL-RUBOUT option was specified
          ;; and everything was rubbed out, we return NIL and the specified value.
          (AND (BP-= *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
               (SETQ TEM (ASSQ ':FULL-RUBOUT rubout-handler-options))
               (RETURN (VALUES NIL (CADR TEM)))))))))

;;; Catch all errors from inside read and make a copy of the text so far so the error message
;;; looks reasonable
(DEFUN STREAM-READ-ERROR-HANDLER (IGNORE &REST IGNORE)
  (when (eq rubout-handler self)
    (INSERT-INTERVAL *SRE-STREAM-BP* *SRE-STREAM-START-BP* *SRE-STREAM-BP*)
    (MUST-REDISPLAY *SRE-WINDOW* DIS-TEXT))
  NIL)

;; This is a separate demon method so that ZTOP-STREAM-MIXIN will not override it.
(DEFMETHOD (EDITOR-STREAM-MIXIN :AFTER :RUBOUT-HANDLER) (OPTIONS &REST IGNORE)
  (UNLESS (OR (CADR (ASSQ ':NO-INPUT-SAVE OPTIONS))
              (CADR (ASSQ ':DONT-SAVE OPTIONS)))
    (PUSH-ON-HISTORY (STRING-INTERVAL *STREAM-START-BP* (WINDOW-POINT *STREAM-SHEET*))
                     *STREAM-INPUT-HISTORY*)))

(DEFMETHOD (EDITOR-STREAM-MIXIN :REFRESH-RUBOUT-HANDLER) (&REST IGNORE)
  (UNLESS (BP-< *STREAM-START-BP* *SRE-INPUT-POINT*)
    (MOVE-BP *SRE-INPUT-POINT* *STREAM-START-BP*))
  (LET ((CONTENTS
          (STRING-INTERVAL *STREAM-START-BP* *SRE-INPUT-END-BP*))
        (POINT-OFFSET (BP-DIFFERENCE *SRE-INPUT-POINT* *STREAM-START-BP*))
        (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
    (MOVE-BP *STREAM-START-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
    (INSERT *STREAM-START-BP* CONTENTS)
    (MOVE-BP *STREAM-BP*
             (FORWARD-CHAR *STREAM-START-BP* POINT-OFFSET T))
    (SETQ *SRE-INPUT-POINT* (COPY-BP *STREAM-BP*))
    (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
    (STREAM-REDISPLAY T)))

(DEFUN BP-DIFFERENCE (BP2 BP1)
  (LET ((BP (COPY-BP BP1))
        (*INTERVAL* (BP-TOP-LEVEL-NODE BP1)))
    (DO ((I 0 (1+ I)))
        ((BP-= BP BP2) I)
      (IBP BP))))

;Do one :ANY-TYI for a read being done with rubout handling.
(DEFMETHOD (EDITOR-STREAM-MIXIN :STREAM-RUBOUT-HANDLER)
           (&AUX (RUBOUT-HANDLER NIL) CHAR)
  ;; If there is a saved-up activation character, return it from :ANY-TYI now.
  ;; This happens at the end of a rescan of the input.
  (IF *SRE-ACTIVATION-CHARACTER*
      (PROG1 *SRE-ACTIVATION-CHARACTER*
             (SETQ *SRE-ACTIVATION-CHARACTER* NIL))
    ;; We could just call the editor, but we must pass certain characters (editing-commands)
    ;; that the program doing the read is handling, and we also want to
    ;; save some time for alphabetic characters.
    (IF *SRE-INPUT-POINT*
        (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
      (SETQ CHAR (FUNCALL-SELF ':ANY-TYI)))
    ;; Just type out and insert self-inserting printing characters
    ;; but not if they are the caller's editing-commands or activations or commands,
    ;; or if they have been redefined in the editor itself.
    (let ((editing-command (cdr (assq ':editing-command rubout-handler-options)))
          (command-handler (cdr (assq ':command rubout-handler-options)))
          (activation-handler (cdr (assq ':activation rubout-handler-options)))
          (do-not-echo (cdr (assq ':do-not-echo rubout-handler-options)))
          (pass-through (cdr (assq ':pass-though rubout-handler-options))))
      (IF (AND (NUMBERP CHAR)
               (NOT (OR (MEMQ CHAR editing-command)
                        (SI:ASSQ-CAREFUL CHAR editing-command)))
               (NOT (AND command-handler
                         (APPLY (car command-handler)
                                CHAR (cdr command-handler))))
               (NOT (AND activation-handler
                         (APPLY (car activation-handler)
                                CHAR (cdr activation-handler))))
               (NOT (MEMQ CHAR DO-NOT-ECHO))
               (OR (AND (graphic-char-p CHAR)
                        (EQ 'COM-ORDINARILY-SELF-INSERT
                            (COMMAND-LOOKUP CHAR *STREAM-COMTAB*)))
                   (AND (< CHAR 400)
                        (MEMQ CHAR pass-through))))
          (LET ((*WINDOW* *STREAM-SHEET*))
            (INSERT-MOVING *STREAM-BP* CHAR)
            (STREAM-IMMEDIATE-OUTPUT
              (TV:SHEET-TYO *STREAM-SHEET* CHAR))
            CHAR)
        ;; Otherwise, run the editor till COM-ACTIVATE throws to us,
        ;; then throw to RUBOUT-HANDLER to restart the read using the buffer contents.
        ;; Move editor point to where we are reading.
        (MOVE-BP *STREAM-BP*
                 (OR *SRE-INPUT-POINT* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*))))
        (SETQ *SRE-INPUT-POINT* NIL)
        ;; Update buffer display and window data so editor gets consistent data.
        (STREAM-REDISPLAY T)
        ;; Unread this character so editor will execute it.
        (WHEN CHAR (TV:IO-BUFFER-UNGET (SEND *STREAM-SHEET* ':IO-BUFFER) CHAR))
        ;; Edit.
        (SET-IN-CLOSURE EDITOR-CLOSURE '*EDITOR-STREAM-ACTIVATION-NEEDED* NIL)
        ;; PASS-ON characters throw here
        ;; to return from the :ANY-TYI method.
        (CATCH 'RETURN-FROM-ANY-TYI
          (LET ((*STREAM-IBEAM-SHOULD-BLINK* NIL)
                (*INSIDE-EDITOR-STREAM* NIL))
            (SI:%BIND (LOCF (TV:SHEET-MORE-VPOS *STREAM-SHEET*)) NIL)
            (UNLESS (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
              (SEND SELF ':ENTER-EDITOR)
              (send self :enable-ibeam))
            (UNWIND-PROTECT
                (FUNCALL *STREAM-SHEET* ':EDIT EDITOR-CLOSURE)
              (FUNCALL *STREAM-SHEET* ':EXIT-EDITOR)
              (send self :disable-ibeam)
              ;; Put blinker into ordinary stream mode instead of editor mode.
              (MULTIPLE-VALUE-BIND (X Y) (TV:BLINKER-READ-CURSORPOS *STREAM-BLINKER*)
                (TV:SHEET-SET-CURSORPOS *STREAM-SHEET* X Y))
              (FUNCALL *STREAM-BLINKER* ':SET-FOLLOW-P T)       ;Make the blinker follow again
              (TV:BLINKER-SET-VISIBILITY *STREAM-BLINKER*
                                         (IF (EQ *STREAM-SHEET* TV:SELECTED-WINDOW)
                                             ':BLINK ':ON)))
            ;; Tell the :RUBOUT-HANDLER method to restart the read.
            (THROW 'RUBOUT-HANDLER T)))))))

;;; Call editor redisplay from the editing stream.

(DEFMETHOD (EDITOR-STREAM-WINDOW :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR TV:RESTORED-BITS-P
      (NOT (TV:SHEET-EXPOSED-P *STREAM-SHEET*))
      (STREAM-REDISPLAY T)))

;Fix up the PLINE data of a window to correspond to the text in its interval,
;on the assumption that we updated the screen by hand already.
(DEFUN FAKE-OUT-TOP-LINE (WINDOW BUFFER &AUX START-LINE START-INDEX START-PLINE LAST-BP SHEET)
  (SETQ LAST-BP (INTERVAL-LAST-BP BUFFER)
        SHEET (WINDOW-SHEET WINDOW))
  (SETQ START-PLINE (DO ((PLINE 0 (1+ PLINE))
                         (N-PLINES (WINDOW-N-PLINES WINDOW))
                         (LINE))
                        (( PLINE N-PLINES) (1- N-PLINES))
                      (SETQ LINE (PLINE-LINE WINDOW PLINE))
                      (AND (OR (NULL LINE)
                               (eq (line-tick line) 'deleted)
                               (> (LINE-TICK LINE) (PLINE-TICK WINDOW PLINE)))
                           (RETURN (1- PLINE)))))
  ;; If the screen was glitched, the pline structure is all wrong.
  (IF (OR (NEQ (BP-LINE (WINDOW-START-BP WINDOW))
               (PLINE-LINE WINDOW 0))
          ( (BP-INDEX (WINDOW-START-BP WINDOW))
             (PLINE-FROM-INDEX WINDOW 0)))
      (SETQ START-PLINE -1))
  (IF (MINUSP START-PLINE)
      (LET ((BP (OR (WINDOW-START-BP WINDOW) (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW)))))
        (SETQ START-LINE (BP-LINE BP)
              START-INDEX (BP-INDEX BP)
              START-PLINE 0))
      (SETQ START-LINE (PLINE-LINE WINDOW START-PLINE)
            START-INDEX (PLINE-TO-INDEX WINDOW START-PLINE))
      (AND (>= START-INDEX (LINE-LENGTH START-LINE))    ;Includes CR
           (SETQ START-LINE (LINE-NEXT START-LINE)
                 START-INDEX 0
                 START-PLINE (1+ START-PLINE))))
  (DO-NAMED LINES
      ((LINE START-LINE (LINE-NEXT LINE))
       (FROM-INDEX START-INDEX 0)
       (TO-INDEX)
       (PLINE START-PLINE)
       (N-PLINES (WINDOW-N-PLINES WINDOW))
       (STOP-LINE (BP-LINE LAST-BP))
       (LH (TV:SHEET-LINE-HEIGHT SHEET))
       (I) (TW))
      ;; If we exhaust the test there is to display, mark remaining screen lines as empty.
      ((NULL LINE)
       (DO ((PLINE PLINE (1+ PLINE)))
           ((= PLINE N-PLINES))
         (SETF (PLINE-LINE WINDOW PLINE) NIL)
         (SETF (PLINE-TICK WINDOW PLINE) NIL)
         (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL)))
    (IF (= PLINE N-PLINES) (RETURN NIL))
    (SETQ TO-INDEX (IF (EQ LINE STOP-LINE) (BP-INDEX LAST-BP)
                       (LINE-LENGTH LINE)))
    (DO NIL (NIL)
      (MULTIPLE-VALUE (TW NIL I)
        (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE FROM-INDEX TO-INDEX NIL 0 LH))
      (OR (NUMBERP I)
          (SETQ I (1+ (LINE-LENGTH LINE))))
      (SETF (PLINE-LINE WINDOW PLINE) LINE)
      (SETF (PLINE-FROM-INDEX WINDOW PLINE) FROM-INDEX)
      (SETF (PLINE-TO-INDEX WINDOW PLINE) I)
      (SETF (PLINE-TICK WINDOW PLINE) *TICK*)
      (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL)
      (SETF (PLINE-TEXT-WIDTH WINDOW PLINE)
            (IF ( I (LINE-LENGTH LINE)) TW
                (+ TW (TV:SHEET-CHAR-WIDTH SHEET))))
      (SETQ FROM-INDEX I)
      (AND ( (SETQ PLINE (1+ PLINE)) N-PLINES) (RETURN-FROM LINES NIL))
      (AND (> FROM-INDEX TO-INDEX) (RETURN NIL)))
    ;; This is another way of exhausting the text we have to display.
    (IF (EQ LINE STOP-LINE)
        (RETURN
          (DO ((PLINE PLINE (1+ PLINE)))
              ((= PLINE N-PLINES))
            (SETF (PLINE-LINE WINDOW PLINE) NIL)
            (SETF (PLINE-TICK WINDOW PLINE) NIL)
            (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL))))))

(DEFUN STREAM-MAYBE-REDISPLAY ()
  "Maybe redisplay the buffer; then return NIL if caller should update screen explicitly.
This function may redisplay and return T,
may refrain from redisplay and return T.
If explicit updating will work, always does nothing and returns NIL
because explicit updating is certainly fast."
  (DECLARE (:SELF-FLAVOR EDITOR-STREAM-MIXIN))
  (TV:PREPARE-SHEET (*STREAM-SHEET*) NIL)
  (COND (EDITOR-STREAM-DEFER-REDISPLAY
         ;; If redisplay is deferred, don't do it, but arrange to do it later.
         ;; Say we did redisplay, to prevent direct output.
         (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
         T)
        ((NOT (WINDOW-READY-P *STREAM-SHEET* NIL))
         (FUNCALL *STREAM-SHEET* ':PREPARE-FOR-REDISPLAY)
         (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
         (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
             (STREAM-REDISPLAY))
         T)
        ((> (WINDOW-REDISPLAY-DEGREE *STREAM-SHEET*) DIS-BPS)
         (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
             (STREAM-REDISPLAY))
         T)
        ((AND (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
              (neq rubout-handler self))        ;Always redisplay on typein
         ;; Turn off editor blinkers if faking redisplay
         (DOLIST (BL (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))
           (TV:BLINKER-SET-VISIBILITY (CDR BL) NIL))
         NIL)
        (T
         (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
         (OR *STREAM-DEFER-OUTPUT-NOT-AT-END*
             (STREAM-REDISPLAY))
         T)))

(DEFUN STREAM-REDISPLAY (&OPTIONAL FORCE-TO-COMPLETION &AUX (*TERMINAL-IO* *STREAM-SHEET*))
  "Invoke editor redisplay while in an editor stream output operation, not inside the editor.
Used by stream output operations that are not doing direct output
to the sheet, after they update the buffer."
  (DECLARE (:SELF-FLAVOR EDITOR-STREAM-MIXIN))
  (TV:PROCESS-TYPEAHEAD TV:IO-BUFFER
                        #'(LAMBDA (CH)
                            (COND ((ATOM CH) CH)
                                  ((EQ (CAR CH) 'REDISPLAY) NIL)
                                  (T CH))))
  (SI:%BIND (LOCF (TV:SHEET-MORE-VPOS *STREAM-SHEET*)) NIL)
  ;; If editor redisplay is required, do it.
  ;; Otherwise, there may have been immediate output by the stream.
  ;; If there is any, we update the editor's redisplay data here.
  (IF ( (WINDOW-REDISPLAY-DEGREE *STREAM-SHEET*) DIS-BPS)
      (FAKE-OUT-TOP-LINE *STREAM-SHEET* (WINDOW-INTERVAL *STREAM-SHEET*)))
  (FUNCALL EDITOR-CLOSURE 'REDISPLAY *STREAM-SHEET* ':POINT NIL NIL FORCE-TO-COMPLETION)
  (SEND *STREAM-BLINKER* ':SET-FOLLOW-P T)
  (TV:BLINKER-SET-VISIBILITY *STREAM-BLINKER* (IF (EQ *STREAM-SHEET* TV:SELECTED-WINDOW)
                                                  ':BLINK ':ON)))

;;; Editor hooks and editor commands for interfacing to the EDITOR-STREAM-MIXIN.

;Make sure that STREAM-PRE-COMMAND-HOOK-1 is not called inside the minibuffer.
(DEFUN STREAM-PRE-COMMAND-HOOK (CHAR)
  (IF (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
      (STREAM-PRE-COMMAND-HOOK-1 CHAR)))

;;; Command hook, throw out if character is to be passed through as an editing command
(DEFUN STREAM-PRE-COMMAND-HOOK-1 (CHAR)
  (let ((editing-command (cdr (assq ':editing-command rubout-handler-options)))
        (command-handler (cdr (assq ':command rubout-handler-options)))
        (activation-handler (cdr (assq ':activation rubout-handler-options)))
        (do-not-echo (cdr (assq ':do-not-echo rubout-handler-options)))
        (CHAR (CHAR-INT CHAR)))
    (COND ((OR (MEMQ CHAR editing-command)
               (SI:ASSQ-CAREFUL CHAR editing-command))
           (SETQ *SRE-INPUT-END-BP* (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))
           (SETQ *SRE-INPUT-POINT* (COPY-BP *SRE-STREAM-BP*))
           (MOVE-BP *SRE-STREAM-BP* (INTERVAL-LAST-BP *INTERVAL*))
           (THROW 'RETURN-FROM-ANY-TYI CHAR))
          ((AND command-handler
                (APPLY (car command-handler) CHAR (cdr command-handler)))
           (SEND *SRE-WINDOW* ':SET-*STREAM-COMMAND-POINT*
                 (COPY-BP *SRE-STREAM-BP* ':NORMAL))
           (MOVE-BP *SRE-STREAM-BP* *SRE-STREAM-START-BP*)
           (THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
                   (VALUES
                     `(:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))
                     ':COMMAND)))
          ((eq char #/End)
           ;;Subsume rubout-handler caller's desire for End to be an Activation character --
           ;;we are inside the editor and will interpret End as activation-with-cleanup.
           )
          ((OR (MEMQ CHAR do-not-echo)
               (AND activation-handler
                    (APPLY (car activation-handler)
                           CHAR (cdr activation-handler))))
           (SETQ *SRE-ACTIVATION-CHARACTER*
                 (IF (MEMQ CHAR do-not-echo) CHAR
                   `(:ACTIVATION ,CHAR ,(OR *NUMERIC-ARG* 1))))
           (THROW 'RUBOUT-HANDLER T)))
  ;; Tell label to change at redisplay after this command finishes,
  ;; so that if this command activates, there will be no change.
  (send *WINDOW* ':ENTER-EDITOR)
  ;; Tell the ibeam blinker to start blinking at next redisplay, also.
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* T)))

;I'm not really sure what value is right; this will prevent blowouts comparing priorities.
(DEFPROP STREAM-PRE-COMMAND-HOOK 0 COMMAND-HOOK-PRIORITY)

(DEFUN STREAM-COMMAND-HOOK (CHAR)
  (IF (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
      (STREAM-COMMAND-HOOK-1 CHAR)))

;;; Command hook.
;;; If at the end of the buffer after the command, send through the buffered input.
;;; *EDITOR-STREAM-ACTIVATION-NEEDED* means we were elsewhere than the end of the buffer
;;;   once upon a time, and shouldnt activate until the user requests it.
;;; *EDITOR-STREAM-REQUIRE-ACTIVATION* is the user-settable flag the enables this mode
(DEFUN STREAM-COMMAND-HOOK-1 (CHAR)
  (LET ((CHAR (CHAR-INT CHAR)))
    (COND ((BP-= (POINT) (INTERVAL-LAST-BP *INTERVAL*))
           (OR *EDITOR-STREAM-ACTIVATION-NEEDED*
               (NOT (OR (< CHAR 200)
                        (MEMQ CHAR '(#/TAB #/RETURN))
                        (AND (< CHAR 400)
                             (MEMQ CHAR (cdr (assq ':pass-through rubout-handler-options))))))
               (EQ *LAST-COMMAND-TYPE* 'INDENT-NEW-LINE)
               (COM-ACTIVATE)))                 ;Automatically activate
          (*EDITOR-STREAM-REQUIRE-ACTIVATION*
           (SETQ *EDITOR-STREAM-ACTIVATION-NEEDED* T)))))

;I'm not really sure what value is right; this will prevent blowouts comparing priorities.
(DEFPROP STREAM-COMMAND-HOOK 0 COMMAND-HOOK-PRIORITY)


;;; Special editor commands for editor streams.

(DEFUN INITIALIZE-STREAM-COMTAB ()
  (COND ((NOT (VARIABLE-BOUNDP *STREAM-COMTAB*))
         (SETQ *STREAM-COMTAB* (SET-COMTAB NIL '(#/END COM-ACTIVATE
                                                 #/C-RETURN COM-ACTIVATE
                                                 #/CLEAR COM-STREAM-CLEAR
                                                 #/PAGE COM-RECENTER-TO-TOP
                                                 #/C-M-Y COM-YANK-INPUT-HISTORY
                                                 #/C-C COM-YANK-INPUT-HISTORY
                                                 #/M-C COM-YANK-POP
                                                 #/C-SH-A COM-QUICK-ARGLIST-INTO-BUFFER)
                                           '(("Require Activation Mode"
                                              . COM-REQUIRE-ACTIVATION-MODE-1))))
         (SET-COMTAB-INDIRECTION *STREAM-COMTAB* *STANDARD-COMTAB*))))

(ADD-INITIALIZATION "INITIALIZE-STREAM-COMTAB" '(INITIALIZE-STREAM-COMTAB)
                    '(:NORMAL) '*EDITOR-INITIALIZATION-LIST*)

;;; The C-CR command
(DEFCOM COM-ACTIVATE "Begin execution of buffered input." ()
  (COND ((WINDOW-MARK-P *WINDOW*)               ;If there is a region
         (WITH-BP (BP (INTERVAL-LAST-BP *INTERVAL*) ':NORMAL)
           (INSERT-INTERVAL BP (POINT) (MARK))  ;copy it to the end
           (DELETE-INTERVAL *EDITOR-STREAM-START-BP* BP T))
         (AND (= (BP-CHAR-BEFORE (INTERVAL-LAST-BP *INTERVAL*)) #/CR)
              (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP *INTERVAL*) -1)
                               (INTERVAL-LAST-BP *INTERVAL*) T))
         (SETF (WINDOW-MARK-P *WINDOW*) NIL)
         (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
  (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*))
  (SETQ *EDITOR-STREAM-ACTIVATION-NEEDED* NIL)          ;So that mode line updated right
  (MUST-REDISPLAY *WINDOW* DIS-BPS)
  (FUNCALL *WINDOW* ':EXIT-EDITOR)      ;Prevent redisplay from updating label now
                                                ;to say we are in the editor,
                                                ;if it hasn't already.
  (REDISPLAY *WINDOW* :point NIL NIL T)         ;Force completion so state is consistent
                                                ;for direct output by the stream.
  (REDISPLAY-MODE-LINE)
  (LET ((IBEAM-BLINKER
          (CDR (ASSQ 'STREAM-BLINK-IBEAM (WINDOW-SPECIAL-BLINKER-LIST *WINDOW*)))))
    (TV:BLINKER-SET-VISIBILITY IBEAM-BLINKER NIL))
  (*THROW 'EXIT-TOP-LEVEL T))

(DEFMINOR COM-REQUIRE-ACTIVATION-MODE-1 REQUIRE-ACTIVATION-MODE-1 "Require Activation" 1
          "Require an End command to resume reading input
if you once move the cursor away from the end of the buffer."
          ()
  (SETQ *EDITOR-STREAM-REQUIRE-ACTIVATION* T))

(DEFCOM COM-STREAM-CLEAR "Delete the form being typed in" ()
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (INTERVAL-LAST-BP *INTERVAL*))
    (KILL-INTERVAL *EDITOR-STREAM-START-BP* POINT T NIL))
  DIS-TEXT)

(DEFCOM COM-RECENTER-TO-TOP "Glitch screen to show start of form at top" ()
  (RECENTER-WINDOW *WINDOW* ':START *EDITOR-STREAM-START-BP*)
  DIS-NONE)

(DEFCOM COM-QUICK-ARGLIST-INTO-BUFFER "Insert arglist result into buffer" ()
  (LET ((STRING ;(QUICK-ARGLIST NIL)    ;Someone broke this beyond repair
          (WITH-OUTPUT-TO-STRING (S) (QUICK-ARGLIST S)))
        (BP (SKIP-OVER-BLANK-LINES-AND-COMMENTS *EDITOR-STREAM-START-BP* T)))
    (INSERT-MOVING BP *COMMENT-BEGIN*)
    (INSERT-MOVING BP STRING)
    (INSERT-MOVING BP #/CR))
  DIS-TEXT)

(DEFCOM COM-YANK-INPUT-HISTORY "Insert the previous input string." ()
  (HISTORY-YANK (SEND *WINDOW* ':*STREAM-INPUT-HISTORY*)))

;;; Special ibeam blinker
(DEFFLAVOR STREAM-IBEAM-BLINKER (EDITOR-STREAM) (TV:IBEAM-BLINKER)
  (:DEFAULT-INIT-PLIST :HALF-PERIOD 32)
  (:INITABLE-INSTANCE-VARIABLES EDITOR-STREAM))

(DEFMETHOD (STREAM-IBEAM-BLINKER :COMPUTE-BLINKER-POSITION) (POINT)
  (FUNCALL EDITOR-STREAM ':COMPUTE-BLINKER-POSITION POINT))

;;; Blink the ibeam while we are in the editor.
(DEFUN STREAM-BLINK-IBEAM (BLINKER WINDOW POINT IGNORE &AUX X Y)
  WINDOW
  (AND *STREAM-IBEAM-BLINKER-P*
       (MULTIPLE-VALUE (X Y)
         (FUNCALL BLINKER ':COMPUTE-BLINKER-POSITION POINT)))
  (COND (X
         (TV:BLINKER-SET-CURSORPOS BLINKER X Y)
         (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK))
        (T (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))

;Note: There is a different method for ZTOP streams.
(DEFMETHOD (EDITOR-STREAM-MIXIN :COMPUTE-BLINKER-POSITION) (BP)
  (AND (EQ (BP-TOP-LEVEL-NODE BP) (WINDOW-INTERVAL *STREAM-SHEET*))
       *STREAM-IBEAM-SHOULD-BLINK*
       (FIND-BP-IN-WINDOW-COORDS *STREAM-START-BP* *STREAM-SHEET*)))

(defmethod (editor-stream-mixin :enable-ibeam) ()
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* T)
  (LET ((IBEAM-BLINKER
          (CDR (ASSQ 'STREAM-BLINK-IBEAM
                     (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)))))
    (WHEN IBEAM-BLINKER
      (STREAM-BLINK-IBEAM IBEAM-BLINKER *STREAM-SHEET* *STREAM-BP* NIL))))

(defmethod (editor-stream-mixin :disable-ibeam) ()
  (SETQ *STREAM-IBEAM-SHOULD-BLINK* nil)
  (LET ((IBEAM-BLINKER
          (CDR (ASSQ 'STREAM-BLINK-IBEAM (WINDOW-SPECIAL-BLINKER-LIST *WINDOW*)))))
    (TV:BLINKER-SET-VISIBILITY IBEAM-BLINKER NIL)))


;;;The actual editor top level, a lisp listener in an editor window
(DEFFLAVOR EDITOR-TOP-LEVEL ()
           (TV:LISTENER-MIXIN TV:NOTIFICATION-MIXIN
            EDITOR-STREAM-WINDOW))

;;;Now that Lisp/edit really works, let's make them easy to get to. -KmC
(tv:add-system-key #/TOP-L 'editor-top-level "LISP(Edit)" T)

(DEFVAR *ZDT-WINDOW* :UNBOUND
  "An editor-top-level window, used by the function ZDT.")

(DEFUN ZDT (&OPTIONAL (ON-P (NOT (TYPEP *TERMINAL-IO* 'EDITOR-TOP-LEVEL))))
  "Enter or leave editor top level.  ON-P is T to enter, NIL to leave."
  (COND (ON-P
         (INITIALIZE-STREAM-COMTAB)
         (OR (VARIABLE-BOUNDP *ZDT-WINDOW*)
             (SETQ *ZDT-WINDOW* (TV:MAKE-WINDOW 'EDITOR-TOP-LEVEL)))
         (SEND *ZDT-WINDOW* ':SELECT T))
        (T
         (SEND *ZDT-WINDOW* ':DESELECT T))))

;;; Additional standard stream operations that editor streams support.

(DEFMETHOD (EDITOR-STREAM-MIXIN :FRESH-LINE) ()
  (IF (ZEROP (BP-INDEX *STREAM-BP*))
      NIL
    (SEND SELF ':TYO #/RETURN)
    T))

(DEFMETHOD (EDITOR-STREAM-MIXIN :UNTYO-MARK) ()
  (FUNCALL-SELF ':READ-BP))

(DEFMETHOD (EDITOR-STREAM-MIXIN :READ-BP) ()
  (COPY-BP *STREAM-BP*))

(DEFMETHOD (EDITOR-STREAM-MIXIN :UNTYO) (-MARK-)
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DELETE-INTERVAL -MARK- *STREAM-BP* T))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :SET-BP) (BP)
  (MOVE-BP *STREAM-BP* BP)
  (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :DELETE-TEXT) ()
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DELETE-INTERVAL *INTERVAL*))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :DELETE-INTERVAL) (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DELETE-INTERVAL FROM-BP TO-BP IN-ORDER-P))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :TEXT-DELETED) ()
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

;;; Clear the screen by scrolling everything off of it
(DEFMETHOD (EDITOR-STREAM-MIXIN :CLEAR-SCREEN) ()
  (RECENTER-WINDOW *STREAM-SHEET* ':RELATIVE (- (WINDOW-N-PLINES *STREAM-SHEET*) 1))
  (STREAM-REDISPLAY))

(defmethod (editor-stream-mixin :clear-window) ()
  (send self :clear-screen))

(DEFMETHOD (EDITOR-STREAM-MIXIN :READ-CURSORPOS) (&OPTIONAL (UNITS ':PIXEL))
  (STREAM-REDISPLAY)   ;Get window data structure up to date for any immediate output.
  (MULTIPLE-VALUE-BIND (X Y)
      (FIND-BP-IN-WINDOW-COORDS *STREAM-BP* *STREAM-SHEET*)
    (SELECTQ UNITS
      (:PIXEL)
      (:CHARACTER
       (IF (AND X Y)
           (SETQ X (TRUNCATE X (TV:SHEET-CHAR-WIDTH (WINDOW-SHEET *STREAM-SHEET*)))
                 Y (TRUNCATE Y (TV:SHEET-LINE-HEIGHT (WINDOW-SHEET *STREAM-SHEET*))))
         (SETQ X (BP-INDEX *STREAM-BP*) Y 0)))
      (OTHERWISE
       (FERROR NIL "~S is not a known unit." UNITS)))
    (VALUES X Y)))

(DEFMETHOD (EDITOR-STREAM-MIXIN :SET-CURSORPOS) (X Y &OPTIONAL (UNITS ':PIXEL))
  (SELECTQ UNITS
    (:PIXEL)
    (:CHARACTER
      (AND X (SETQ X (* X (TV:SHEET-CHAR-WIDTH (WINDOW-SHEET *STREAM-SHEET*)))))
      (AND Y (SETQ Y (* Y (TV:SHEET-LINE-HEIGHT (WINDOW-SHEET *STREAM-SHEET*))))))
    (OTHERWISE
      (FERROR NIL "~S is not a known unit." UNITS)))
  (STREAM-REDISPLAY)
  (let ((*window* *stream-sheet*))
    (MOVE-BP *STREAM-BP* (BP-FROM-COORDS *STREAM-SHEET* X Y)))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :INCREMENT-CURSORPOS) (DX DY &OPTIONAL (UNITS ':PIXEL))
  (when (plusp (tv:sheet-more-flag *stream-sheet*))
    (send *stream-sheet* :more-exception))
  (multiple-value-bind (x y)
      (send self :read-cursorpos units)
    (send self :set-cursorpos (+ x dx) (+ y dy) units)))

(DEFUN BP-FROM-COORDS (WINDOW X Y &AUX SHEET LINE PLINE CHAR-POS LH REAL-PLINE START END)
  (SETQ SHEET (WINDOW-SHEET WINDOW))
  (SETQ LH (TV:SHEET-LINE-HEIGHT SHEET)
        PLINE (SETQ REAL-PLINE (TRUNCATE Y LH)))
  (COND ((MINUSP PLINE)
         (SETQ PLINE 0))
        (( PLINE (WINDOW-N-PLINES WINDOW))
         (SETQ PLINE (WINDOW-N-PLINES WINDOW))))
  (DO NIL ((OR (NULL (PLINE-LINE WINDOW PLINE))
               (ZEROP (PLINE-FROM-INDEX WINDOW PLINE))))
    (AND (ZEROP PLINE) (RETURN))
    (SETQ PLINE (1- PLINE)))
  ;; If there is no line there, extend the buffer until there is one
  (OR (SETQ LINE (PLINE-LINE WINDOW PLINE))
      (DO ((I 0 (1+ I))
           (P PLINE (1- P)))
          ((PLINE-LINE WINDOW P)
           (LET ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
             (DOTIMES (J I)
               (INSERT LAST-BP #/CR))
             (SETQ LINE (BP-LINE LAST-BP))))))
  (SETQ START (PLINE-FROM-INDEX WINDOW PLINE))
  (LET ((BP (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))))
    (AND (EQ LINE (BP-LINE BP)) (SETQ START (MIN START (BP-INDEX BP)))))
  (LET ((BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW))))
    (AND (EQ LINE (BP-LINE BP)) (SETQ END (BP-INDEX BP))))
  (MULTIPLE-VALUE (NIL NIL CHAR-POS)            ;Find character to right
    (TV:SHEET-COMPUTE-MOTION SHEET 0 (* PLINE LH) LINE START END NIL
                             (MAX 0 X)
                             (* REAL-PLINE LH)))
  ;; If there is no such index, extend the line
  (IF CHAR-POS
      (CREATE-BP LINE CHAR-POS)
      (INDENT-TO (CREATE-BP LINE (LINE-LENGTH LINE)) X SHEET)))

(DEFMETHOD (EDITOR-STREAM-MIXIN :HOME-CURSOR) ()
  (FUNCALL-SELF ':SET-CURSORPOS 0 0))

(DEFMETHOD (EDITOR-STREAM-MIXIN :CLEAR-EOL) ()
  (LET ((*WINDOW* *STREAM-SHEET*)
        (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
    (DELETE-INTERVAL *STREAM-BP* (END-LINE *STREAM-BP* 0 T) T))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(defmethod (editor-stream-mixin :clear-rest-of-line) ()
  (send self :clear-eol))

(DEFMETHOD (EDITOR-STREAM-MIXIN :CLEAR-EOF) ()
  (LET ((*WINDOW* *STREAM-SHEET*)
        (*INTERVAL* (WINDOW-INTERVAL *STREAM-SHEET*)))
    (DELETE-INTERVAL *STREAM-BP* (INTERVAL-LAST-BP *INTERVAL*) T))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :CLEAR-CHAR) ()
  (LET ((LINE (BP-LINE *STREAM-BP*))
        (IDX (BP-INDEX *STREAM-BP*)))
    (ASET #/SPACE LINE IDX)
    (MUNG-LINE LINE))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :INSERT-LINE) (&OPTIONAL (NLINES 1))
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DOTIMES (I NLINES)
      (INSERT *STREAM-BP* #/RETURN)))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :DELETE-LINE) (&OPTIONAL (NLINES 1))
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DELETE-INTERVAL *STREAM-BP* (FORWARD-LINE *STREAM-BP* NLINES T) T))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :INSERT-CHAR) (&OPTIONAL (NCHARS 1))
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DOTIMES (I NCHARS)
      (INSERT *STREAM-BP* #/SPACE)))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

(DEFMETHOD (EDITOR-STREAM-MIXIN :DELETE-CHAR) (&OPTIONAL (NCHARS 1))
  (LET ((*WINDOW* *STREAM-SHEET*))
    (DELETE-INTERVAL *STREAM-BP* (FORWARD-CHAR *STREAM-BP* NCHARS T) T))
  (MUST-REDISPLAY *STREAM-SHEET* DIS-TEXT)
  (STREAM-REDISPLAY))

;;; Editors always insert, this should be close, therefore.
(DEFMETHOD (EDITOR-STREAM-MIXIN :INSERT-STRING) (STRING &OPTIONAL (START 0) END)
  (FUNCALL-SELF ':STRING-OUT STRING START END))

(DEFFLAVOR EDITOR-STREAM-WITHOUT-WINDOW-MIXIN () ()
  (:REQUIRED-FLAVORS EDITOR-STREAM-MIXIN)
  (:DOCUMENTATION :MIXIN "Mix this into kinds of EDITOR-STREAM-MIXINs
that are not also windows."))

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :TYI) (&REST ARGS)
  (DO-FOREVER
    (LET ((CHAR (LEXPR-SEND SELF ':ANY-TYI ARGS)))
      (UNLESS (LISTP CHAR) (RETURN CHAR)))))

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :HOME-DOWN)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :SIZE)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :INSIDE-SIZE)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :REFRESH)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :PREPARE-FOR-REDISPLAY)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :TYPEOUT-WINDOW)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :MODE-LINE-WINDOW)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :PUT-POINT-AT-PLINE)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :NEW-SCROLL-POSITION)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :REDISPLAY)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :BEEP)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :CLEAR-INPUT)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :SIZE-IN-CHARACTERS)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :COMPUTE-MOTION)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :STRING-LENGTH)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :CHARACTER-WIDTH)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :FORCE-KBD-INPUT)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :HANDLE-EXCEPTIONS)
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :NOTICE) ;for :INPUT-WAIT
           EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW)

(DEFUN EDITOR-STREAM-WITHOUT-WINDOW-MIXIN-PASS-ON-MESSAGE-TO-WINDOW (&REST REST)
  (DECLARE (:SELF-FLAVOR EDITOR-STREAM-MIXIN))
  (LEXPR-FUNCALL *STREAM-SHEET* REST))

(DEFMETHOD (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN :BEFORE :INIT) (IGNORE)
  (SETQ TV:IO-BUFFER (SEND *STREAM-SHEET* ':IO-BUFFER)))

;;; Streams that type into a specified window.
;;; This is so that, given any ZWEI window at all, you can create
;;; an editor stream which will use that window.

(DEFFLAVOR EDITOR-STREAM-FROM-WINDOW ()
           (EDITOR-STREAM-WITHOUT-WINDOW-MIXIN EDITOR-STREAM-MIXIN
            TOP-LEVEL-DISPLAYER-MIXIN DISPLAYER))

;List of all streams made so far by the following function.
(DEFVAR *STREAMS-FROM-WINDOWS* NIL)

;Given a ZWEI-WINDOW structure, return an editor stream that uses it.
(DEFUN MAKE-EDITOR-STREAM-FROM-WINDOW (WINDOW)
  (OR (DOLIST (STREAM *STREAMS-FROM-WINDOWS*)
        (AND (EQ WINDOW (FUNCALL STREAM ':*STREAM-SHEET*))
             (RETURN STREAM)))
      (LET ((INIT-PLIST (LIST ':*STREAM-SHEET* WINDOW)))
        (LET ((STREAM (INSTANTIATE-FLAVOR 'EDITOR-STREAM-FROM-WINDOW (LOCF INIT-PLIST)
                                          T NIL TV:SHEET-AREA)))
          (PUSH STREAM *STREAMS-FROM-WINDOWS*)
          STREAM))))

;;; Editor top level major mode for ZMACS.

;;; ZTOP streams differ from ordinary EDITOR-STREAM-MIXINs in that
;;; they work with ZMACS-WINDOWS (and are based on EDITOR-STREAM-FROM-WINDOW)
;;; and the stream is used in a separate stack group while ZMACS runs in
;;; its normal stack group.  This causes the way of getting into and out of
;;; the rubout processor to be different.

;;; The evaluation stack group has a special binding of PRIN1
;;; to put semicolons before output.

(DEFVAR *ZTOP-PRIN1* NIL
  "Function for ZTOP mode top level to use instead of PRIN1.
ZTOP-EVALUATION-PRIN1 is suggested.   See also
*ZTOP-GRIND-EVALUATION-RESULT-P* and *ZTOP-COMMENT-EVALUATION-RESULT-P*.")

(DEFVAR *ZTOP-GRIND-EVALUATION-RESULT-P* T
  "T => ZTOP-EVALUATION-PRIN1 grinds rather than just printing.")
(DEFVAR *ZTOP-COMMENT-EVALUATION-RESULT-P* "; "
  "String for ZTOP-EVALUATION-PRIN1 to insert in front of line.")

(DEFVAR *ZTOP-EVALUATION-PRIN1-STREAM* :UNBOUND
  "The stream given to ZTOP-EVALUATION-PRIN1.")

;;; Something suitable for binding PRIN1 to
(DEFUN ZTOP-EVALUATION-PRIN1 (EXP &OPTIONAL (*ZTOP-EVALUATION-PRIN1-STREAM* *STANDARD-OUTPUT*))
  (WHEN *ZTOP-COMMENT-EVALUATION-RESULT-P*
    (SEND *ZTOP-EVALUATION-PRIN1-STREAM* ':STRING-OUT *ZTOP-COMMENT-EVALUATION-RESULT-P*))
  (IF *ZTOP-GRIND-EVALUATION-RESULT-P*
      (GRIND-TOP-LEVEL EXP 90. 'ZTOP-EVALUATION-PRIN1-IO NIL 'DISPLACED NIL)
    (PRIN1 EXP 'ZTOP-EVALUATION-PRIN1-IO)))

(DEFUN ZTOP-EVALUATION-PRIN1-IO (OP &REST REST)
  (PROG1 (LEXPR-FUNCALL *ZTOP-EVALUATION-PRIN1-STREAM* OP REST)
         (AND (EQ OP ':TYO) (= (CAR REST) #/RETURN) *ZTOP-COMMENT-EVALUATION-RESULT-P*
              (SEND *ZTOP-EVALUATION-PRIN1-STREAM* ':STRING-OUT
                    *ZTOP-COMMENT-EVALUATION-RESULT-P*))))

(DEFVAR *ZTOP-REQUIRE-ACTIVATION* NIL
  "T when ZTOP buffer is in Require Activation mode.
Used by streams and by mode line.")
(DEFPROP *ZTOP-REQUIRE-ACTIVATION* T MODE-SETTABLE-P)

;One at most of these variables is T.
;They records the state of rubout processing and activation,
;and are used to display the mode line.
(DEFVAR *ZTOP-ACTIVATION-NEEDED* NIL
  "T if ZTOP input will be read when user types an activation command.")
(DEFVAR *ZTOP-READING-INPUT* NIL
  "T if ZTOP input is being read now.")
(DEFVAR *ZTOP-EDITING* NIL
  "T if ZTOP input cannot be read because cursor is not at end.")

(DEFVAR *ZTOP-PACKAGE* :UNBOUND
  "Current PACKAGE at entry to :RUBOUT-HANDLER on a Ztop stream.
Used to pass changes in current package from editor to execution sg and vice versa.")

(DEFVAR *LAST-ZTOP-BUFFER* NIL)

(DEFMAJOR COM-ZTOP-MODE ZTOP-MODE "ZTOP"
          "Sets things up for zmacs buffer editor top level." ()
  (COMMAND-HOOK (MAKE-ZTOP-COMMAND-HOOK *INTERVAL* *WINDOW*) *POST-COMMAND-HOOK*)
  (COMMAND-HOOK (MAKE-ZTOP-PRE-COMMAND-HOOK *INTERVAL* *WINDOW*) *COMMAND-HOOK*)
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
  (SET-COMTAB *MODE-COMTAB* '(#/END COM-FINISH-ZTOP-EVALUATION
                              #/C-CR COM-FINISH-ZTOP-EVALUATION
                              #/ABORT COM-ZTOP-ABORT
                              #/META-ABORT COM-ZTOP-ABORT-ALL
                              #/C-M-Y COM-ZTOP-YANK-INPUT-HISTORY
                              #/TAB COM-INDENT-FOR-LISP
                              #/RUBOUT COM-TAB-HACKING-RUBOUT
                              #/C-RUBOUT COM-RUBOUT)
              '(("Require Activation Mode" . COM-REQUIRE-ACTIVATION-MODE)))
  (SETQ *MODE-LINE-LIST*
        (APPEND *MODE-LINE-LIST*
                '((*ZTOP-ACTIVATION-NEEDED* "  Type End to resume reading input")
                  (*ZTOP-READING-INPUT* "  Reading input")
                  (*ZTOP-EDITING* "  Editing"))))
  (PROGN (AND (TYPEP *INTERVAL* 'FILE-BUFFER)
              (setq *last-ztop-buffer*
                    (cons *INTERVAL*
                          (delete *INTERVAL* *LAST-ZTOP-BUFFER*))))))

(DEFPROP ZTOP-MODE :LISP EDITING-TYPE)

;Create a buffer and put it in ZTOP mode.
(DEFUN MAKE-ZTOP-BUFFER (&OPTIONAL (NAME "ZTOP") &AUX BUFFER)
  (SETQ BUFFER (CREATE-ONE-BUFFER-TO-GO NAME))
  (SETF (BUFFER-SAVED-MAJOR-MODE BUFFER) 'ZTOP-MODE)
  (SI:%BIND (LOCF (WINDOW-INTERVAL *WINDOW*)) BUFFER)
  (MAKE-ZTOP-COMMAND-HOOK BUFFER *WINDOW*)
  (MAKE-ZTOP-PRE-COMMAND-HOOK BUFFER *WINDOW*)
  BUFFER)

(DEFVAR *ZTOP-COMMAND-HOOK-ALIST* NIL)

(DEFVAR *ZTOP-PRE-COMMAND-HOOK-ALIST* NIL)

;Make a ztop stream and a command hook for a specified ZMACS buffer.
;If we already made them for this buffer, reuse them.
;The command hook used for ZTOP is a gensym
;whose function definition is closure of an explicit lambda in this function.
(DEFUN MAKE-ZTOP-COMMAND-HOOK (BUFFER WINDOW &AUX ZTOP-STREAM HOOK-CLOSURE HOOK)
  (SETQ *CURRENT-COMMAND-TYPE* 'ZTOP-MODE)
  (COND ((SETQ HOOK (ASSQ BUFFER *ZTOP-COMMAND-HOOK-ALIST*))
         (SETQ HOOK (CDR HOOK) HOOK-CLOSURE (FSYMEVAL HOOK)
               ZTOP-STREAM (SYMEVAL-IN-CLOSURE HOOK-CLOSURE '*ZTOP-STREAM*))
         (FUNCALL ZTOP-STREAM ':SET-STREAM-WINDOW WINDOW))
        (T
         (SETQ ZTOP-STREAM (LET ((INIT-PLIST (LIST ':*STREAM-SHEET* WINDOW
                                                   ':*ZMACS-SG* CURRENT-STACK-GROUP
                                                   ':BUFFER BUFFER)))
                             (INSTANTIATE-FLAVOR 'ZTOP-STREAM-FROM-WINDOW (LOCF INIT-PLIST)
                                                 T NIL TV:SHEET-AREA)))
         (SEND BUFFER ':PUTPROP ZTOP-STREAM 'ZTOP-STREAM)
         (SETQ HOOK-CLOSURE (LET-CLOSED ((*ZTOP-INTERVAL* BUFFER)
                                         (*ZTOP-STREAM* ZTOP-STREAM))
                                        #'(LAMBDA (IGNORE)
                                            (AND (EQ *INTERVAL* *ZTOP-INTERVAL*)
                                                 (FUNCALL *ZTOP-STREAM* ':COMMAND-HOOK
                                                          *CURRENT-COMMAND-TYPE*))))
               HOOK (GENSYM))
         (FSET HOOK HOOK-CLOSURE)
         (PUTPROP HOOK 1000 'COMMAND-HOOK-PRIORITY)
         (PUSH (CONS BUFFER HOOK) *ZTOP-COMMAND-HOOK-ALIST*)
         (SETQ *ZTOP-EDITING* NIL *ZTOP-READING-INPUT* T
               *ZTOP-ACTIVATION-NEEDED* NIL)))
  HOOK)

;; We assume that MAKE-ZTOP-COMMAND-HOOK has already been called
;; on the same window and buffer.
(DEFUN MAKE-ZTOP-PRE-COMMAND-HOOK (BUFFER WINDOW &AUX ZTOP-STREAM HOOK-CLOSURE HOOK)
  WINDOW
  (SETQ *CURRENT-COMMAND-TYPE* 'ZTOP-MODE)
  (UNLESS (SETQ HOOK (CDR (ASSQ BUFFER *ZTOP-PRE-COMMAND-HOOK-ALIST*)))
    (SETQ ZTOP-STREAM (GET BUFFER 'ZTOP-STREAM))
    (SETQ HOOK-CLOSURE (LET-CLOSED ((*ZTOP-INTERVAL* BUFFER)
                                    (*ZTOP-STREAM* ZTOP-STREAM))
                         #'(LAMBDA (CHAR)
                             (AND (EQ *INTERVAL* *ZTOP-INTERVAL*)
                                  (FUNCALL *ZTOP-STREAM* ':PRE-COMMAND-HOOK CHAR))))
          HOOK (GENSYM))
    (FSET HOOK HOOK-CLOSURE)
    (PUTPROP HOOK 1000 'COMMAND-HOOK-PRIORITY)
    (PUSH (CONS BUFFER HOOK) *ZTOP-PRE-COMMAND-HOOK-ALIST*))
  HOOK)

;;; Special commands used while in ZTOP mode.

(DEFMINOR COM-REQUIRE-ACTIVATION-MODE REQUIRE-ACTIVATION-MODE "Require Activation" 1
          "Require an End command in ZTOP to resume reading input
if you once move the cursor away from the end of the buffer."
          ()
  (SETQ *ZTOP-REQUIRE-ACTIVATION* T))

;;; This is like the DO-IT command in HENRY's ZTOP
(DEFCOM COM-FINISH-ZTOP-EVALUATION "Begin execution of buffered input." ()
  (LET ((ZTOP-BUFFER *INTERVAL*) ZTOP-STREAM STREAM-START-BP)
    (OR (SETQ ZTOP-STREAM (FUNCALL *INTERVAL* ':GET 'ZTOP-STREAM))
        (SETQ ZTOP-BUFFER (if *LAST-ZTOP-BUFFER* (first *last-ztop-buffer*) (MAKE-ZTOP-BUFFER))
              ZTOP-STREAM (FUNCALL ZTOP-BUFFER ':GET 'ZTOP-STREAM)))
    (SETQ STREAM-START-BP (FUNCALL ZTOP-STREAM ':*STREAM-START-BP*))
    (COND ((WINDOW-MARK-P *WINDOW*)             ;If there is a region
           (SETF (WINDOW-MARK-P *WINDOW*) NIL)
           (WITH-BP (BP (INTERVAL-LAST-BP ZTOP-BUFFER) ':NORMAL)
             (INSERT-INTERVAL BP (POINT) (MARK))        ;copy it to the end
             (DELETE-INTERVAL STREAM-START-BP BP T))
           (COND ((NEQ *INTERVAL* ZTOP-BUFFER)
                  (FUNCALL ZTOP-BUFFER ':SET-ATTRIBUTE ':PACKAGE *PACKAGE*)
                  (SETF (BUFFER-PACKAGE ZTOP-BUFFER) *PACKAGE*)
                  (SEND ZTOP-STREAM ':SET-PACKAGE *PACKAGE*)
                  (DO-IT-SELECT-WINDOW-BUFFER ZTOP-BUFFER))))
          ((NEQ *INTERVAL* ZTOP-BUFFER)
           (BARF "There is no region"))))
  (LET ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (LET ((CH (BP-CHAR-BEFORE LAST-BP)))
      (COND ((= CH #/CR)
             (DELETE-INTERVAL (FORWARD-CHAR LAST-BP -1) LAST-BP T))
            ((= (LIST-SYNTAX CH) LIST-ALPHABETIC)
             (INSERT LAST-BP #/SPACE))))
    (MOVE-BP (POINT) LAST-BP))
  (SETQ *CURRENT-COMMAND-TYPE* 'ACTIVATE-ZTOP)
  DIS-TEXT)

;;; Henry's more winning remote-evaluation ztop buffer selector (slightly modified)
(DEFUN DO-IT-SELECT-WINDOW-BUFFER (BUFFER)
  ;; If there's a window with this buffer in it, switch to that window.
  ;; otherwise, switch to the buffer in the current window.
  (DO ((REST-WINDOWS (FRAME-EXPOSED-WINDOWS) (CDR REST-WINDOWS))
       (WINDOW))
      ((COND ((NULL REST-WINDOWS) (MAKE-BUFFER-CURRENT BUFFER) T)
             ((EQ (WINDOW-INTERVAL (SETQ WINDOW (CAR REST-WINDOWS))) BUFFER)
              (MAKE-WINDOW-CURRENT WINDOW)
              T)))))

(DEFCOM COM-SELECT-LAST-ZTOP-BUFFER "Move to the most recently used ZTOP mode buffer." ()
  (make-buffer-current (if *last-ztop-buffer*
                           (first *last-ztop-buffer*)
                         (create-one-buffer-to-go "ZTOP")))
  (com-ztop-mode)
  DIS-TEXT)

(DEFCOM COM-ZTOP-ABORT "Abort program that is running in the Ztop buffer." ()
  (FUNCALL (FUNCALL *INTERVAL* ':GET 'ZTOP-STREAM)
           ':ZTOP-ABORT)
  DIS-TEXT)

(DEFCOM COM-ZTOP-ABORT-ALL "Abort the running program all the way to top level." ()
  (FUNCALL (FUNCALL *INTERVAL* ':GET 'ZTOP-STREAM)
           ':ZTOP-ABORT-ALL)
  DIS-TEXT)

(DEFCOM COM-ZTOP-YANK-INPUT-HISTORY "Yank the previous input." ()
  (HISTORY-YANK (SEND (GET *INTERVAL* 'ZTOP-STREAM) ':*STREAM-INPUT-HISTORY*)))

;;; Define the flavors for the actual streams used by ZTOP.

(DEFFLAVOR ZTOP-STREAM-FROM-WINDOW () (ZTOP-STREAM-MIXIN EDITOR-STREAM-FROM-WINDOW))

(DEFFLAVOR ZTOP-STREAM-MIXIN
        (*ZMACS-SG*
         *ZTOP-SG*
         *STREAM-START-BP*
         (*STREAM-ACTIVATION-NEEDED* NIL)
;not used after 98
;        (rubout-handler-options NIL)
         (*RUBOUT-HANDLER-STATE* ':NORMAL))
        ()
  (:REQUIRED-FLAVORS EDITOR-STREAM-FROM-WINDOW)
  (:INIT-KEYWORDS :BUFFER)
  (:INITABLE-INSTANCE-VARIABLES *ZMACS-SG*)
  (:GETTABLE-INSTANCE-VARIABLES *STREAM-START-BP*))

(DEFMETHOD (ZTOP-STREAM-MIXIN :AFTER :INIT) (INIT-PLIST)
  (SETQ *ZTOP-SG* (MAKE-STACK-GROUP "ZTOP" ':REGULAR-PDL-SIZE 40000
                                           ':SPECIAL-PDL-SIZE 4000))
  (SETQ *STREAM-INPUT-HISTORY*
        (MAKE-HISTORY (STRING-APPEND "input history of "
                                     (BUFFER-NAME (GET INIT-PLIST ':BUFFER)))))
  (INITIALIZE-ZTOP-SG SELF))

(DEFMETHOD (ZTOP-STREAM-MIXIN :SET-PACKAGE) (PKG)
  ;; Tell the execution SG about a new package.
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SYMEVAL-IN-STACK-GROUP '*PACKAGE* *ZTOP-SG*)
    (SETF (CONTENTS LOC) PKG))
  (MULTIPLE-VALUE-BIND (NIL LOC)
      (SYMEVAL-IN-STACK-GROUP '*ZTOP-PACKAGE* *ZTOP-SG*)
    (SETF (CONTENTS LOC) PKG)))

(DEFUN INITIALIZE-ZTOP-SG (STREAM)
  (DECLARE (:SELF-FLAVOR ZTOP-STREAM-MIXIN))
  (STACK-GROUP-PRESET *ZTOP-SG* `ZTOP-TOP-LEVEL STREAM)
  (FUNCALL *ZTOP-SG*)
  (STREAM-REDISPLAY T))

;This is the top level loop of the execution stack group.
(DEFUN ZTOP-TOP-LEVEL (ZTOP-STREAM &AUX (PRIN1 *ZTOP-PRIN1*)
                                        (*PACKAGE* *PACKAGE*) (*ZTOP-PACKAGE* *PACKAGE*))
  (DO-FOREVER
    (*CATCH 'ZTOP-TOP-LEVEL
      (SI:LISP-TOP-LEVEL1 ZTOP-STREAM))))

;Unlike other editor streams, a ZTOP stream must be able to be told to
;change which sheet it prints on, if you visit the ZTOP buffer in a different ZMACS window.
(DEFMETHOD (ZTOP-STREAM-MIXIN :SET-STREAM-WINDOW) (WINDOW)
  (MULTIPLE-VALUE-BIND (NIL TEM) (SYMEVAL-IN-STACK-GROUP 'PRIN1 *ZTOP-SG*)
    (SETF (CDR TEM) *ZTOP-PRIN1*))
  (COND ((NEQ WINDOW *STREAM-SHEET*)
         (SETQ *STREAM-SHEET* WINDOW
               *STREAM-BLINKER* (WINDOW-POINT-BLINKER *STREAM-SHEET*)
               *STREAM-BP* (WINDOW-POINT *STREAM-SHEET*))
         (LET ((BLINKER (TV:MAKE-BLINKER *STREAM-SHEET* 'STREAM-IBEAM-BLINKER
                                         ':EDITOR-STREAM SELF ':VISIBILITY NIL )))
           (SETF (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)
                 (DELQ (ASSQ 'STREAM-BLINK-IBEAM (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))
                       (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*)))
           (PUSH `(STREAM-BLINK-IBEAM . ,BLINKER)
                 (WINDOW-SPECIAL-BLINKER-LIST *STREAM-SHEET*))))))

(DEFMETHOD (ZTOP-STREAM-MIXIN :BEFORE :RUBOUT-HANDLER) (ignore &REST IGNORE)
  (SETQ *STREAM-ACTIVATION-NEEDED* NIL
        *ZTOP-PACKAGE* *PACKAGE*
;       rubout-handler-options args     ;ARGS used to be the first arg supplied. Flushed.
        *RUBOUT-HANDLER-STATE*
        (IF (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
            ':NORMAL ':EDITING)
        *ZTOP-READING-INPUT* (EQ *RUBOUT-HANDLER-STATE* ':NORMAL)
        *ZTOP-EDITING* (NOT *ZTOP-READING-INPUT*)
        *ZTOP-ACTIVATION-NEEDED* NIL))

(DEFMETHOD (ZTOP-STREAM-MIXIN :AFTER :RUBOUT-HANDLER) (&REST IGNORE)
  ;;Get package for evaling in
  (SETQ *PACKAGE* (SYMEVAL-IN-STACK-GROUP '*PACKAGE* *ZMACS-SG*))
  (STREAM-REDISPLAY T))                         ;Redisplay typeahead

;;; This method gets called when the buffer is empty, co-call the other stack group
(DEFMETHOD (ZTOP-STREAM-MIXIN :STREAM-RUBOUT-HANDLER) ()
  ;; If everything has been typed out correctly, update the window datastructure
  (AND (< (WINDOW-REDISPLAY-DEGREE *STREAM-SHEET*) DIS-TEXT)
       (FAKE-OUT-TOP-LINE *STREAM-SHEET* (WINDOW-INTERVAL *STREAM-SHEET*)))
  ;; Avoid confusing the :RUBOUT-HANDLER method after a EDITING-COMMAND character.
  (SETQ *SRE-INPUT-END-BP* NIL *SRE-INPUT-POINT* NIL)
  (IF *SRE-ACTIVATION-CHARACTER*
      (PROG1 *SRE-ACTIVATION-CHARACTER*
             (SETQ *SRE-ACTIVATION-CHARACTER* NIL))
    (SETQ *ZTOP-SG* SYS:%CURRENT-STACK-GROUP)
    (WITH-BP (OLD-STREAM-BP *STREAM-BP* ':NORMAL)
      (LET ((RESUME-INFORMATION (FUNCALL *ZMACS-SG*)))
        (COND ((EQ RESUME-INFORMATION ':RESCAN)
               (*THROW 'RUBOUT-HANDLER T))
              ((EQ RESUME-INFORMATION ':KEEP-READING)
               (MOVE-BP *STREAM-BP* OLD-STREAM-BP)
               (FUNCALL-SELF ':ANY-TYI))
              ((EQ (CAR RESUME-INFORMATION) ':EDITING-COMMAND)
               (SETQ *SRE-INPUT-END-BP* (CADR RESUME-INFORMATION)
                     *SRE-INPUT-POINT* (CADDR RESUME-INFORMATION))
               (CADDDR RESUME-INFORMATION))
              ((EQ (CAR RESUME-INFORMATION) ':COMMAND)
               (SETQ *STREAM-COMMAND-POINT*
                     (COPY-BP *STREAM-BP* ':NORMAL))
               (MOVE-BP *STREAM-BP* *STREAM-START-BP*)
               (*THROW 'TV:RETURN-FROM-RUBOUT-HANDLER
                       (VALUES (CADR RESUME-INFORMATION) ':COMMAND)))
              ((EQ (CAR RESUME-INFORMATION) ':ACTIVATION)
               (SETQ *SRE-ACTIVATION-CHARACTER*
                     (CADR RESUME-INFORMATION))
               (*THROW 'RUBOUT-HANDLER T)))))))

;;; Command hook, called by editor before each command.
;;; The purpose is to detect editing-commands, activations and commands
;;; as requested by the invocation of :RUBOUT-HANDLER.
;;; If this character is any of those, we cause it to be ignored
;;; by the editor, after the resuming the execution stack group
;;; and giving it full information about the character we encountered.
(DEFMETHOD (ZTOP-STREAM-MIXIN :PRE-COMMAND-HOOK)
           (CHAR &AUX
            (EDITING-COMMAND (CDR (ASSQ ':EDITING-COMMAND rubout-handler-options)))
            (DO-NOT-ECHO (CDR (ASSQ ':DO-NOT-ECHO rubout-handler-options)))
            (COMMAND-HANDLER (cdr (ASSQ ':COMMAND rubout-handler-options)))
            (ACTIVATION-HANDLER (cdr (ASSQ ':ACTIVATION rubout-handler-options))))
  (SETQ CHAR (CHAR-INT CHAR))
  (COND ((OR (MEMQ CHAR EDITING-COMMAND)
             (SI:ASSQ-CAREFUL CHAR EDITING-COMMAND))
         (RESUME-ZTOP-SG
           (PROG1
             `(:EDITING-COMMAND
                ,(COPY-BP (INTERVAL-LAST-BP *INTERVAL*))
                ,(COPY-BP (POINT))
                ,CHAR)
             (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*))))
         (SETQ *CURRENT-COMMAND-TYPE* 'EDITING-COMMAND)
         (*THROW 'COMMAND-EXECUTE T))
        ((AND COMMAND-HANDLER
              (APPLY (car COMMAND-HANDLER) CHAR (cdr COMMAND-HANDLER)))
         (RESUME-ZTOP-SG
           `(:COMMAND
              (:COMMAND ,CHAR ,(OR *NUMERIC-ARG* 1))))
         (SETQ *CURRENT-COMMAND-TYPE* 'EDITING-COMMAND)
         (*THROW 'COMMAND-EXECUTE T))
        ((OR (MEMQ CHAR DO-NOT-ECHO)
             (AND ACTIVATION-HANDLER
                  (APPLY (car ACTIVATION-HANDLER)
                         CHAR (cdr ACTIVATION-HANDLER))))
         (SETQ *RUBOUT-HANDLER-STATE* ':NORMAL)
         (SETQ *ZTOP-ACTIVATION-NEEDED* NIL
               *ZTOP-EDITING* NIL
               *ZTOP-READING-INPUT* T)
         (REDISPLAY-MODE-LINE)
         (RESUME-ZTOP-SG
           `(:ACTIVATION
              ,(IF (MEMQ CHAR DO-NOT-ECHO) CHAR
                 `(:ACTIVATION ,CHAR ,(OR *NUMERIC-ARG* 1)))))
         (SETQ *CURRENT-COMMAND-TYPE* 'ACTIVATE-ZTOP)
         (*THROW 'COMMAND-EXECUTE T))))

;;; This gets called by the editor after each command
(DEFMETHOD (ZTOP-STREAM-MIXIN :COMMAND-HOOK) (TYPE &AUX (OLD-STATE *RUBOUT-HANDLER-STATE*))
  (UNLESS (EQ TYPE 'EDITING-COMMAND)
    (AND (ASSQ ':FULL-RUBOUT rubout-handler-options) (BP-= *STREAM-START-BP* *STREAM-BP*)
         (SETQ OLD-STATE ':EDITING TYPE ':FULL-RUBOUT))
    (SETQ *RUBOUT-HANDLER-STATE*
          (IF (AND (BP-= *STREAM-BP* (INTERVAL-LAST-BP (WINDOW-INTERVAL *STREAM-SHEET*)))
                   (OR (NOT *STREAM-ACTIVATION-NEEDED*)
                       (EQ TYPE 'ACTIVATE-ZTOP))
                   (MEMQ TYPE '(SELF-INSERT INSERT-CR ACTIVATE-ZTOP ZTOP-MODE :FULL-RUBOUT)))
              ':NORMAL
            ':EDITING))
    (IF (EQ *RUBOUT-HANDLER-STATE* ':EDITING)
        (SETQ *STREAM-ACTIVATION-NEEDED* *ZTOP-REQUIRE-ACTIVATION*)
      (AND (NEQ OLD-STATE ':NORMAL)             ;If we were editing
           (MOVE-BP *STREAM-BP* *STREAM-START-BP*))
      (SETQ *ZMACS-SG* SYS:%CURRENT-STACK-GROUP)
      (RESUME-ZTOP-SG (IF (EQ OLD-STATE ':EDITING) ':RESCAN ':KEEP-READING))
      (AND (NEQ OLD-STATE ':NORMAL)
           (MUST-REDISPLAY *STREAM-SHEET* DIS-BPS))))
  (SETQ *ZTOP-ACTIVATION-NEEDED* *STREAM-ACTIVATION-NEEDED*)
  (SETQ *ZTOP-EDITING*
        (AND (NOT *ZTOP-ACTIVATION-NEEDED*)
             (EQ *RUBOUT-HANDLER-STATE* ':EDITING)))
  (SETQ *ZTOP-READING-INPUT* (AND (NOT *ZTOP-ACTIVATION-NEEDED*) (NOT *ZTOP-EDITING*))))

(DEFUN RESUME-ZTOP-SG (ARG)
  (DECLARE (:SELF-FLAVOR ZTOP-STREAM-MIXIN))
  (FUNCALL CURRENT-PROCESS ':ADD-COROUTINE-STACK-GROUP *ZTOP-SG*)
  ;; If the editor has changed our package, tell the execution SG.
  (MULTIPLE-VALUE-BIND (VALUE LOC)
      (SYMEVAL-IN-STACK-GROUP '*ZTOP-PACKAGE* *ZTOP-SG*)
    (UNLESS (EQ VALUE *PACKAGE*)
      (SETF (CONTENTS LOC) *PACKAGE*)
      (MULTIPLE-VALUE-BIND (NIL LOC)
          (SYMEVAL-IN-STACK-GROUP '*PACKAGE* *ZTOP-SG*)
        (SETF (CONTENTS LOC) *PACKAGE*))))
  (LET ((NORMAL-EXIT-P NIL))
    (UNWIND-PROTECT
        (PROG1
          (FUNCALL *ZTOP-SG* ARG)
          (SETQ NORMAL-EXIT-P T)
          ;; If the execution SG has changed its package, propagate to editor.
          (SETQ *PACKAGE* (SYMEVAL-IN-STACK-GROUP '*ZTOP-PACKAGE* *ZTOP-SG*))
          (SETF (BUFFER-PACKAGE *INTERVAL*) *PACKAGE*))
      (UNLESS NORMAL-EXIT-P (INITIALIZE-ZTOP-SG SELF)))))

;Decide whether and where to blink the ibeam blinker.
(DEFMETHOD (ZTOP-STREAM-MIXIN :COMPUTE-BLINKER-POSITION) (BP)
  (AND (EQ (BP-TOP-LEVEL-NODE BP) (BP-TOP-LEVEL-NODE *STREAM-START-BP*))
      (NOT *ZTOP-READING-INPUT*)
       (FIND-BP-IN-WINDOW-COORDS *STREAM-START-BP* *STREAM-SHEET*)))

;Executed in the editor sg to abort the execution sg.
(DEFMETHOD (ZTOP-STREAM-MIXIN :ZTOP-ABORT) ()
  (EH:SG-ABORT *ZTOP-SG*))

;Executed in the editor sg to abort the execution sg.
(DEFMETHOD (ZTOP-STREAM-MIXIN :ZTOP-ABORT-ALL) ()
  (EH:SG-APPLY-NO-TRAP *ZTOP-SG* 'TV:KBD-INTERCEPT-ABORT-ALL '(#/M-ABORT) T T))

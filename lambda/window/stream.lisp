;;; -*- Mode:LISP; Package:TV; Base:8; Lowercase:T; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;;This is overridden by loading SYS:WINDOW;RH, which is now standardly loaded.
(DEFVAR STREAM-MIXIN-DEFAULT-RUBOUT-HANDLER 'DEFAULT-RUBOUT-HANDLER
  "Default rubout-handler to use for input from windows")

;;; Io stream stuff
(DEFFLAVOR STREAM-MIXIN
           (;; I/O buffer for this stream
            (IO-BUFFER NIL)
            (RUBOUT-HANDLER-BUFFER NIL)
            ;; Used for :PREEMPTABLE-READ.
            (OLD-TYPEAHEAD NIL)
            ;; nil => use stream-mixin-default-rubout-handler
            (stream-rubout-handler)
            (displayer nil)
            stream-spare-1
            stream-spare-2
            stream-spare-3
            stream-spare-4
            stream-spare-5
            stream-spare-6)
           ()
  (:REQUIRED-FLAVORS SHEET ESSENTIAL-WINDOW)    ;Explicit presence of SHEET
                                                ;helps init flavor-unmapped-instance-variables
  (:SELECT-METHOD-ORDER :TYO :STRING-OUT :LINE-OUT :TYI :TYI-NO-HANG :LISTEN)
  (:GETTABLE-INSTANCE-VARIABLES IO-BUFFER stream-rubout-handler displayer)
  (:INITABLE-INSTANCE-VARIABLES IO-BUFFER RUBOUT-HANDLER-BUFFER)
  (:SETTABLE-INSTANCE-VARIABLES OLD-TYPEAHEAD stream-rubout-handler displayer)
  (:INIT-KEYWORDS :ASYNCHRONOUS-CHARACTERS)
  (:DOCUMENTATION :MIXIN "Ordinary tv stream operations.
Gives all the meaningful stream operations for a display, such as :TYO, :TYI, :RUBOUT-HANDLER,
:STRING-OUT, etc.  Include this flavor someplace so that the window can be passed to functions
that take streams as arguments, and especially if *TERMINAL-IO* is going to be bound to the
window."))


(DEFMETHOD (STREAM-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SEND SELF :WHICH-OPERATIONS)         ;Pre-create this, certainly going to be used
  (UNLESS (TYPEP IO-BUFFER 'IO-BUFFER)
    (LET (SIZE INPUT-FUNCTION OUTPUT-FUNCTION)
      (IF (NUMBERP IO-BUFFER)
          (SETQ SIZE IO-BUFFER
                INPUT-FUNCTION NIL
                OUTPUT-FUNCTION 'KBD-DEFAULT-OUTPUT-FUNCTION)
        (SETQ SIZE (OR (FIRST IO-BUFFER) 100)
              INPUT-FUNCTION (SECOND IO-BUFFER)
              OUTPUT-FUNCTION (OR (THIRD IO-BUFFER) 'KBD-DEFAULT-OUTPUT-FUNCTION)))
      (SETQ IO-BUFFER (MAKE-IO-BUFFER SIZE INPUT-FUNCTION OUTPUT-FUNCTION))))
  (IF (GETL INIT-PLIST '(:ASYNCHRONOUS-CHARACTERS))
      (SETF (GETF (IO-BUFFER-PLIST IO-BUFFER) :ASYNCHRONOUS-CHARACTERS)
            (GET INIT-PLIST :ASYNCHRONOUS-CHARACTERS)))
  (UNLESS RUBOUT-HANDLER-BUFFER
    (SETQ RUBOUT-HANDLER-BUFFER (MAKE-RUBOUT-HANDLER-BUFFER))))

(DEFMETHOD (STREAM-MIXIN :ADD-ASYNCHRONOUS-CHARACTER) (CHARACTER FUNCTION &REST ARGS)
;character lossage
  (IF (CHARACTERP CHARACTER) (SETQ CHARACTER (CHAR-INT CHARACTER)))
  (CHECK-TYPE CHARACTER FIXNUM "a character")
  (CHECK-TYPE FUNCTION (SATISFIES FUNCTIONP) "a function")
  (LET ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER))))
    (PUSH (LIST* CHARACTER FUNCTION (COPYLIST ARGS))
          (GET PLIST :ASYNCHRONOUS-CHARACTERS))))

(DEFMETHOD (STREAM-MIXIN :ASYNCHRONOUS-CHARACTER-P) (CHARACTER)
;character lossage
  (IF (CHARACTERP CHARACTER) (SETQ CHARACTER (CHAR-INT CHARACTER)))
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
         (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS)))
    (ASSQ CHARACTER ALIST)))

(DEFMETHOD (STREAM-MIXIN :HANDLE-ASYNCHRONOUS-CHARACTER) (CHARACTER)
;character lossage
  (IF (CHARACTERP CHARACTER) (SETQ CHARACTER (CHAR-INT CHARACTER)))
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
         (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS))
         (TEM (ASSQ CHARACTER ALIST)))
    (WHEN TEM (APPLY (CADR TEM) (CAR TEM) SELF (CDDR TEM)))))

(DEFMETHOD (STREAM-MIXIN :REMOVE-ASYNCHRONOUS-CHARACTER) (CHARACTER)
;character lossage
  (IF (CHARACTERP CHARACTER) (SETQ CHARACTER (CHAR-INT CHARACTER)))
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
         (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS)))
    (SETF (GET PLIST :ASYNCHRONOUS-CHARACTERS)
          (DELQ (ASSQ CHARACTER ALIST) ALIST))))

(DEFMETHOD (STREAM-MIXIN :DIRECTION) () :BIDIRECTIONAL)

(DEFMETHOD (STREAM-MIXIN :BEFORE :SELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :BEFORE :DESELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :SET-IO-BUFFER) (NEW-BUFFER)
  (WITHOUT-INTERRUPTS
    (KBD-CLEAR-SELECTED-IO-BUFFER)
    (SETQ IO-BUFFER NEW-BUFFER)))

(DEFMETHOD (STREAM-MIXIN :PUSH-INPUT) (INPUT)
  (IF (STRINGP INPUT)
      (DO ((I (1- (STRING-LENGTH INPUT)) (1- I)))
          ((MINUSP I))
        (IO-BUFFER-PUSH IO-BUFFER (AREF INPUT I)))
    (IO-BUFFER-PUSH IO-BUFFER INPUT)))


(DEFMETHOD (STREAM-MIXIN :UNTYI) (CH)
  (IF (AND (eq rubout-handler self)
           ;; RUBOUT-HANDLER added as conjunct 6/1/83
           ;; to avoid lossage entering editor rubout handler
           ;; by typing (= 1 2) then stray ) while inside BREAK.
           ( 1 (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
           (EQ CH (AREF RUBOUT-HANDLER-BUFFER (1- (RHB-SCAN-POINTER)))))
      (DECF (RHB-SCAN-POINTER))
    (IO-BUFFER-UNGET IO-BUFFER CH))
  CH)

(DEFMETHOD (STREAM-MIXIN :UNREAD-CHAR) (CH)
  (IF (CHARACTERP CH) (SETQ CH (CHAR-INT CH)))
  (SEND SELF :UNTYI CH))

(DEFMETHOD (STREAM-MIXIN :LISTEN) ()
  (NOT (AND ( (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
            (IO-BUFFER-EMPTY-P IO-BUFFER)
            (WITHOUT-INTERRUPTS
              (IF (NEQ IO-BUFFER (KBD-GET-IO-BUFFER))
                  T
                (AND (KBD-HARDWARE-CHAR-AVAILABLE)
                     (KBD-PROCESS-MAIN-LOOP-INTERNAL))
                (IO-BUFFER-EMPTY-P KBD-IO-BUFFER))))))

(DEFMETHOD (STREAM-MIXIN :WAIT-FOR-INPUT-WITH-TIMEOUT) (TIMEOUT)
  (KBD-WAIT-FOR-INPUT-WITH-TIMEOUT IO-BUFFER TIMEOUT))

(DEFMETHOD (STREAM-MIXIN :CLEAR-INPUT) ()
  (SETF (RHB-FILL-POINTER) 0)
  (SETF (RHB-SCAN-POINTER) 0)
  (IO-BUFFER-CLEAR IO-BUFFER)
  (AND (EQ IO-BUFFER (KBD-GET-IO-BUFFER))
       (KBD-CLEAR-IO-BUFFER)))

(DEFMETHOD (STREAM-MIXIN :TYI) (&OPTIONAL IGNORE &AUX CH)
  (DO-FOREVER
    (SETQ CH (SEND SELF :ANY-TYI))
    (WHEN (NUMBERP CH)
      (RETURN CH))
    (WHEN (AND (CONSP CH)
               (EQ (CAR CH) :MOUSE-BUTTON)
               (MEMQ (CADR CH) '(#/MOUSE-3-1 #.(CHAR-INT #/MOUSE-3-1))))
      (MOUSE-CALL-SYSTEM-MENU))))

(DEFMETHOD (STREAM-MIXIN :READ-CHAR) (&OPTIONAL IGNORE IGNORE)
  (INT-CHAR (SEND SELF :TYI)))

(DEFMETHOD (STREAM-MIXIN :TYI-NO-HANG) (&OPTIONAL IGNORE &AUX CH)
  (DO-FOREVER
    (SETQ CH (SEND SELF :ANY-TYI-NO-HANG))
    (WHEN (OR (NULL CH)
              (NUMBERP CH))
      (RETURN CH))
    (WHEN (AND (CONSP CH)
               (EQ (CAR CH) :MOUSE-BUTTON)
               (MEMQ (CADR CH) '(#/MOUSE-3-1 #.(CHAR-INT #/MOUSE-3-1))))
      (MOUSE-CALL-SYSTEM-MENU))))

(DEFMETHOD (STREAM-MIXIN :READ-CHAR-NO-HANG) (&OPTIONAL IGNORE IGNORE)
  (INT-CHAR (SEND SELF :TYI-NO-HANG)))

(DEFMETHOD (STREAM-MIXIN :ANY-TYI) (&OPTIONAL IGNORE &AUX IDX)
  (COND ((> (RHB-FILL-POINTER) (SETQ IDX (RHB-SCAN-POINTER)))
         (SETF (RHB-SCAN-POINTER) (1+ IDX))
         (OR (AREF RUBOUT-HANDLER-BUFFER IDX)
             (FERROR "EOF on input from a window.")))
        ((neq rubout-handler self)
;        (SETF (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
         (LET ((CHAR
                 (COND ((KBD-IO-BUFFER-GET IO-BUFFER T))
                       (T
                        (SEND SELF :NOTICE :INPUT-WAIT)
                        (KBD-IO-BUFFER-GET IO-BUFFER)))))
           (IF (AND (eq rubout-handler-inside self)
                    (EQ OLD-TYPEAHEAD T)
                    (CONSP CHAR)
                    (NEQ (CAR CHAR) 'REDISPLAY-RUBOUT-HANDLER))
               ;; If inside the rubout handler in a :PREEMPTABLE-READ
               ;; and we just got a blip that isn't intended for the rubout handler.
               (PROGN
                 (MULTIPLE-VALUE-BIND (STRING INDEX)
                     (SEND SELF :SAVE-RUBOUT-HANDLER-BUFFER)
                   (SETQ OLD-TYPEAHEAD (LIST STRING INDEX)))
                 ;; Save the text, rub it all out, and unread the blip.
                 ;; The :FULL-RUBOUT option will cause the RH to return to the caller
                 ;; who will then read the blip.
                 (SEND SELF :UNTYI CHAR)
                 (CHAR-INT #/CLEAR))
             CHAR)))
        (T
         (OR (FUNCALL (or stream-rubout-handler stream-mixin-default-rubout-handler))
             (FERROR "EOF on input from a window.")))))

(DEFMETHOD (STREAM-MIXIN :ANY-READ-CHAR) (&OPTIONAL IGNORE IGNORE &AUX CH)
  (SETQ CH (SEND SELF :TYI-NO-HANG))
  (IF (FIXNUMP CH) (INT-CHAR CH) CH))

(DEFMETHOD (STREAM-MIXIN :ANY-TYI-NO-HANG) (&OPTIONAL IGNORE)
  (if (> (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
      (send self :any-tyi))
  (if (neq rubout-handler self)
      (KBD-IO-BUFFER-GET IO-BUFFER T)
    (FERROR "~S from inside a rubout handler." :ANY-TYI-NO-HANG)))

(DEFMETHOD (STREAM-MIXIN :ANY-READ-CHAR-NO-HANG) (&OPTIONAL IGNORE IGNORE &AUX CH)
  (SETQ CH (SEND SELF :ANY-TYI-NO-HANG))
  (IF (FIXNUMP CH) (INT-CHAR CH) CH))

;;; For things only prepared to deal with fixnums
(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI) (&AUX CH)
  (DO-FOREVER
    (SETQ CH (SEND SELF :ANY-TYI))
    (WHEN (NUMBERP CH)
      (RETURN (VALUES CH CH)))
    (WHEN (AND (CONSP CH) (EQ (CAR CH) :MOUSE-BUTTON))
      (RETURN (VALUES (SECOND CH) CH)))))

(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI-NO-HANG) (&AUX CH)
  (DO-FOREVER
    (SETQ CH (SEND SELF :ANY-TYI-NO-HANG))
    (WHEN (OR (NULL ch) (NUMBERP CH))
      (RETURN (VALUES CH CH)))
    (WHEN (AND (CONSP CH) (EQ (CAR CH) :MOUSE-BUTTON))
      (RETURN (VALUES (SECOND CH) CH)))))

(DEFMETHOD (STREAM-MIXIN :LIST-TYI) (&AUX CH)
  "Only return lists"
  (DO-FOREVER
    (SETQ CH (SEND SELF :ANY-TYI))
    (AND (CONSP CH) (RETURN CH))))

;;; Return a circular buffer array describing the last however many input characters.
;;; The (array-leader array 1) points at the last slot stored into.
(DEFMETHOD (STREAM-MIXIN :PLAYBACK) ()
  (IO-BUFFER-RECORD IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :FORCE-KBD-INPUT) (CH-OR-STRING)
  (IF (STRINGP CH-OR-STRING)
      (DOTIMES (N (LENGTH CH-OR-STRING))
        (IO-BUFFER-PUT IO-BUFFER (AREF CH-OR-STRING N)))
    (IO-BUFFER-PUT IO-BUFFER CH-OR-STRING)))    ;all of the old cases.

(DEFFLAVOR LIST-TYI-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Makes :TYI discard non-keyboard input."))

(DEFFLAVOR ANY-TYI-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Obsolete flavor"))

(DEFFLAVOR PREEMPTABLE-READ-ANY-TYI-MIXIN () ())

(DEFVAR RUBOUT-HANDLER-STARTING-X :UNBOUND
  "Within rubout handler, X position of beginning of input.")
(DEFVAR RUBOUT-HANDLER-STARTING-Y :UNBOUND
  "Within rubout handler, Y position of beginning of input.")
(DEFVAR RUBOUT-HANDLER-RE-ECHO-FLAG :UNBOUND
  "Within rubout handler, T when there are error messages in the middle of the input.
Set to NIL when the input is reprinted and they are gone.")
(DEFVAR RUBOUT-HANDLER-INSIDE NIL
  "Non-NIL while inside the rubout handler.")
(DEFVAR RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL
  "The character or blip that is activating this invocation of :RUBOUT-HANDLER.")

(DEFVAR PROMPT-STARTING-X :UNBOUND
  "Within rubout handler, X position of beginning of printed prompt string.")
(DEFVAR PROMPT-STARTING-Y :UNBOUND
  "Within rubout handler, Y position of beginning of printed prompt string.")

(defmethod (stream-mixin :rubout-handler) (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq :nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
        (apply function args))
    (let ((rubout-handler-options options))
      (if ( (rhb-fill-pointer) (rhb-scan-pointer))
          (setf (rhb-fill-pointer) 0)
        (copy-array-portion rubout-handler-buffer (rhb-scan-pointer) (rhb-fill-pointer)
                            rubout-handler-buffer 0 (array-length rubout-handler-buffer))
        (if (numberp (rhb-typein-pointer))
            (decf (rhb-typein-pointer) (rhb-scan-pointer)))
        (decf (rhb-fill-pointer) (rhb-scan-pointer)))
      (setf (rhb-scan-pointer) 0 (rhb-status) :initial-entry)
      (catch 'return-from-rubout-handler
        (let (prompt-starting-x prompt-starting-y
              rubout-handler-starting-x rubout-handler-starting-y
              (rubout-handler self)
              (rubout-handler-inside self)
              (rubout-handler-re-echo-flag nil)
              (rubout-handler-activation-character nil))
          (multiple-value (prompt-starting-x prompt-starting-y) (send self :read-cursorpos))
          (setq rubout-handler-starting-x prompt-starting-x
                rubout-handler-starting-y prompt-starting-y)
          (do-forever
            (setq rubout-handler-re-echo-flag nil)
            (catch 'rubout-handler                      ;Throw here when rubbing out
              (condition-case (error)
                  (return
                   (multiple-value-prog1
                     (apply function args)              ;Call READ or whatever.
                     (setf (rhb-fill-pointer) (rhb-scan-pointer))
                     (and (rhb-typein-pointer)
                          (> (rhb-typein-pointer) (rhb-fill-pointer))
                          (setf (rhb-typein-pointer) (rhb-fill-pointer)))))
                (sys:parse-error
                 (send self :fresh-line)
                 (princ ">>ERROR: " self)
                 (send error :report self)
                 (send self :fresh-line)
                 (setq rubout-handler-re-echo-flag t)
                 (do-forever (send self :tyi)))))       ;If error, force user to rub out
            ;;Maybe return when user rubs all the way back
            (and (zerop (rhb-fill-pointer))
                 (let ((full-rubout-option (assq :full-rubout rubout-handler-options)))
                   (when full-rubout-option
                     ;; Get rid of the prompt, if any.
                     (send self :clear-between-cursorposes
                                prompt-starting-x prompt-starting-y
                                (- cursor-x left-margin-size) (- cursor-y top-margin-size))
                     (send self :set-cursorpos prompt-starting-x prompt-starting-y)
                     (return (values nil (cadr full-rubout-option))))))))))))

(defmethod (stream-mixin :rescanning-p) ()
  (or (< (rhb-scan-pointer) (rhb-fill-pointer))
      rubout-handler-activation-character))

(defmethod (stream-mixin :force-rescan) ()
  (setf (rhb-scan-pointer) 0)
  (throw 'rubout-handler t))

(defmethod (stream-mixin :read-bp) ()
  (rhb-scan-pointer))

;;; Foo.
(defmethod (stream-mixin :replace-input) (n string &optional (start 0) end)
  (declare (ignore n string start end))
  (ferror "Foo. I haven't written this yet.")
  )
;;; Foo. This is hair implemented by Brand S which seems to be a real crock.
(defmethod (stream-mixin :start-typeout) (type &optional spacing)
  type spacing
  ;(ferror "Foo. I haven't written this yet.")
  )
(defmethod (stream-mixin :finish-typeout) (&optional spacing erase-p)
  spacing erase-p
  ;(ferror "Foo. I haven't written this yet.")
  )


(DEFMETHOD (STREAM-MIXIN :PREEMPTABLE-READ) (OPTIONS FUN &REST ARGS)
  (DO ((TYPEAHEAD OLD-TYPEAHEAD NIL)
       (RESULT) (FLAG))
      (())
    (SETQ OLD-TYPEAHEAD T)
    (UNWIND-PROTECT
      (MULTIPLE-VALUE (RESULT FLAG)
        (with-stack-list (initial-input :initial-input
                                        (if (consp typeahead) (car typeahead)))
          (with-stack-list (initial-input-pointer :initial-input-pointer
                                          (if (consp typeahead) (cadr typeahead)))
            (with-stack-list* (options initial-input
                                       initial-input-pointer
                                       '((:full-rubout :full-rubout))
                                       options)
              (unless (consp typeahead) (setq options (cddr options)))
              (lexpr-send self :rubout-handler options FUN ARGS)))))
      (AND (EQ OLD-TYPEAHEAD T)
           (SETQ OLD-TYPEAHEAD NIL)))
    (AND (NEQ FLAG :FULL-RUBOUT)
         (RETURN (VALUES RESULT NIL)))
    ;; Determine whether a mouse character caused the full-rubout
    (SETQ RESULT (SEND SELF :ANY-TYI-NO-HANG))
    (COND (RESULT
           (OR (NUMBERP RESULT)
               (RETURN (VALUES RESULT :MOUSE-CHAR)))
           (SEND SELF :UNTYI RESULT)))
    (AND (SETQ FLAG (CADR (ASSQ :FULL-RUBOUT OPTIONS)))
         (RETURN (VALUES NIL FLAG)))
    ;; Presumably after this point, the user didn't call us with :FULL-RUBOUT
    ;; option, so we should retry. We have to fix up the notion of :PROMPT
    ;; and :REPROMPT first though.
    (LET ((PROMPT (ASSQ :PROMPT OPTIONS)))
      (WHEN PROMPT
        (SETQ OPTIONS (REMQ PROMPT OPTIONS))
        ;This next condition may be unnecessary, but just in case. --kmp
        (UNLESS (NOT (ASSQ :REPROMPT OPTIONS))
          ;; make fake reprompt info. our old prompt should still
          ;; be there --kmp
          (PUSH `(:REPROMPT . ,(CDR PROMPT)) OPTIONS))))))

;;; Give a single character, or do rubout processing, throws to RUBOUT-HANDLER on editing.
(defun default-rubout-handler ()
  (declare (:self-flavor stream-mixin))
  (setf (rhb-typein-pointer) nil)               ;Mark that old rubout handler is in use.
  (when (= (rhb-scan-pointer) most-positive-fixnum)
    (setf (rhb-scan-pointer) 0)
    (throw 'rubout-handler t))
  (let ((status (rhb-status))
        (rubbed-out-some nil)
        (rubout-handler nil))
    (setf (rhb-status) nil)
    (when (memq status '(:restored :initial-entry))
      ;;Prompt if desired
      (let ((prompt-option (assq :prompt rubout-handler-options)))
        (when prompt-option
          (rubout-handler-prompt (cadr prompt-option) self nil)))
      (multiple-value (rubout-handler-starting-x rubout-handler-starting-y)
        (send self :read-cursorpos))
      ;; Output any "typeahead"
      (when (plusp (rhb-fill-pointer))
        (send self :string-out rubout-handler-buffer))
      ;;no point looking for :initial-input-pointer since this rh can't do anything with it
      (let ((initial-input (cadr (assq :initial-input rubout-handler-options))))
        (when initial-input
          (string-nconc rubout-handler-buffer initial-input))))
    (or (prog1 rubout-handler-activation-character
               (setq rubout-handler-activation-character nil))
        (do ((editing-command (cdr (assq :editing-command rubout-handler-options)))
             (do-not-echo (cdr (assq :do-not-echo rubout-handler-options)))
             (pass-through (cdr (assq :pass-through rubout-handler-options)))
             (command-handler
               (assq :command rubout-handler-options))
             (preemptable (assq :preemptable rubout-handler-options))
             (activation-handler
               (assq :activation rubout-handler-options))
             ch len)
            (nil)
          (setq ch (send self :any-tyi))
          (cond ((eq (car-safe ch) 'redisplay-rubout-handler)
                 (send self :set-cursorpos prompt-starting-x prompt-starting-y)
                 (send self :clear-rest-of-line)
                 (and (setq len (or (assq :reprompt rubout-handler-options)
                                    (assq :prompt rubout-handler-options)))
                      (rubout-handler-prompt (cadr len) self ch))
                 (multiple-value (rubout-handler-starting-x rubout-handler-starting-y)
                   (send self :read-cursorpos))
                 (send self :string-out rubout-handler-buffer))
                ((consp ch)
                 (when preemptable
                   (setf (rhb-scan-pointer) 0)
                   (throw 'return-from-rubout-handler
                          (values ch (cadr preemptable)))))
                ((and command-handler
                      (apply (cadr command-handler) ch (cddr command-handler)))
                 (setf (rhb-scan-pointer) 0)
                 (throw 'return-from-rubout-handler
                         (values
                           `(:command ,ch 1)
                           :command)))
                ;; Don't touch this character, just return it to caller.
                ((or (memq ch editing-command)
                     (si:assq-careful ch editing-command))
                 ;; Cause rubout handler rescan next time the user does :TYI.
                 (if rubbed-out-some (setf (rhb-scan-pointer) most-positive-fixnum))
                 (return ch))
                ;; Is it an editing character?
                ((and (not (or (memq ch do-not-echo)
                               (and activation-handler
                                    (apply (cadr activation-handler) ch (cddr activation-handler)))))
                      (or (ldb-test %%kbd-control-meta ch)
                          (and (memq ch '(#/Rubout #/Clear-input #/Clear-screen #/Delete))
                               (not (memq ch pass-through)))))
                 (cond ((memq ch '(#/Clear-screen #/Delete))    ;Retype buffered input
                        (send self :tyo ch)             ;Echo it
                        (if (= ch #/Clear-screen) (send self :clear-window)
                          (send self :tyo #/Newline))
                        (multiple-value (prompt-starting-x prompt-starting-y)
                          (send self :read-cursorpos))
                        (and (setq len (or (assq :reprompt rubout-handler-options)
                                           (assq :prompt rubout-handler-options)))
                             (rubout-handler-prompt (cadr len) self ch))
                        (multiple-value (rubout-handler-starting-x rubout-handler-starting-y)
                          (send self :read-cursorpos))
                        (send self :string-out rubout-handler-buffer))
                       ((memq ch '(#/Rubout #/M-rubout #/Clear-input)) ;Delete some characters
                        (cond ((not (zerop (setq len (rhb-fill-pointer))))
                               (setf (rhb-fill-pointer)
                                     (setq len (selectq ch
                                                 (#/Rubout (1- len))
                                                 (#/M-rubout (string-backward-word
                                                               rubout-handler-buffer len))
                                                 (#/Clear-input 0))))
                               (setf rubbed-out-some t
                                     (rhb-status) :rubout)
                               (multiple-value-bind (x y)
                                   (send self :compute-motion rubout-handler-buffer 0 len
                                                 rubout-handler-starting-x rubout-handler-starting-y)
                                 (if rubout-handler-re-echo-flag
                                     (setq x rubout-handler-starting-x y rubout-handler-starting-y))
                                 (multiple-value-bind (cx cy) (send self :read-cursorpos)
                                   (send self :clear-between-cursorposes x y cx cy))
                                 (send self :set-cursorpos x y)
                                 (and rubout-handler-re-echo-flag
                                      (send self :string-out rubout-handler-buffer))))))
                       (t (beep)))                              ;Undefined editing character
                 (cond ((and (zerop (rhb-fill-pointer))
                             (assq :full-rubout rubout-handler-options))
                        (setf (rhb-scan-pointer) 0)
                        (throw 'rubout-handler t))))
                (t
                 ;; It's a self-inserting character.
                 ;; If this is first character typed in, re-get starting cursorpos since while
                 ;; waiting for input a notification may have been typed out.
                 (and (zerop (rhb-fill-pointer))
                      (multiple-value (rubout-handler-starting-x rubout-handler-starting-y)
                        (send self :read-cursorpos)))
                 (cond ((memq ch do-not-echo)
                        (setq rubout-handler-activation-character ch))
                       ((and activation-handler
                             (apply (cadr activation-handler) ch (cddr activation-handler)))
                        (setq ch `(:activation ,ch 1))
                        (setq rubout-handler-activation-character ch))
                       (t
                        (send self :tyo ch)
                        (array-push-extend rubout-handler-buffer ch)))
                 (cond (rubbed-out-some
                        (setf (rhb-scan-pointer) 0)
                        (throw 'rubout-handler t))
                       (t
                        (setf (rhb-scan-pointer) (rhb-fill-pointer))
                        (setq rubout-handler-activation-character nil)
                        (return ch)))))))))

;;; Use ZWEI's syntax table if ZWEI is around...
(DEFUN STRING-BACKWARD-WORD (STRING INDEX &AUX ALPHA-P-FCN)
  (SETQ ALPHA-P-FCN
        (IF (BOUNDP 'ZWEI:*WORD-SYNTAX-TABLE*)
            #'(LAMBDA (X) (EQ (ZWEI:CHAR-SYNTAX X ZWEI:*WORD-SYNTAX-TABLE*)
                              ZWEI:WORD-ALPHABETIC))
          'ALPHA-CHAR-P))
  (DO ((I (1- INDEX) (1- I))
       (INSIDE-WORD NIL))
      ((MINUSP I) 0)
    (IF (FUNCALL ALPHA-P-FCN (AREF STRING I))
        (SETQ INSIDE-WORD T)
      (AND INSIDE-WORD (RETURN (1+ I))))))

(DEFUN RUBOUT-HANDLER-PROMPT (PROMPT-OPTION STREAM CH)
  (LET ((RUBOUT-HANDLER NIL))   ;In case of **more**
    (IF (STRINGP PROMPT-OPTION)
        (FUNCALL STREAM :STRING-OUT PROMPT-OPTION)
      (FUNCALL PROMPT-OPTION STREAM CH))))

(defmethod (stream-mixin :save-rubout-handler-buffer) ()
  (when (eq rubout-handler-inside self)
    ;; Give rubout handler function a chance to put its internal data
    ;; into RUBOUT-HANDLER-BUFFER where we look for it.
; not patched in 98.
    (let ((prop (get (or stream-rubout-handler
                         stream-mixin-default-rubout-handler)
                     'save-rubout-handler-buffer)))
      (when prop (funcall prop self)))
    (values (copy-seq rubout-handler-buffer) (rhb-typein-pointer))))

(defmethod (stream-mixin :restore-rubout-handler-buffer) (string &optional pointer)
  (let ((length (array-active-length string)))
    (or ( (array-length rubout-handler-buffer) length)
        (adjust-array-size rubout-handler-buffer length))
    (copy-array-contents string rubout-handler-buffer)
    (setf (rhb-fill-pointer) length))
  (setf (rhb-typein-pointer) pointer)
  (send self :refresh-rubout-handler)
  (setf (rhb-scan-pointer) 0)
  ;(setf (rhb-status) :restored)
  (throw 'rubout-handler t))

(defmethod (stream-mixin :refresh-rubout-handler) (&optional discard-last-character)
  (if discard-last-character
      (setf (rhb-fill-pointer) (max 0 (1- (rhb-fill-pointer)))))
  (if (rhb-typein-pointer)
      (setf (rhb-typein-pointer)
            (min (rhb-typein-pointer) (rhb-fill-pointer))))
  (send self :fresh-line)
  (let ((prompt (or (assq :reprompt rubout-handler-options)
                    (assq :prompt rubout-handler-options))))
    (when prompt (rubout-handler-prompt (cadr prompt) self #/Delete)))
  (multiple-value (rubout-handler-starting-x rubout-handler-starting-y)
    (send self :read-cursorpos))
  (send self :string-out rubout-handler-buffer))

;;; Stream operations which all streams are required to support or ignore
;;; I'm afraid these will appear in the :WHICH-OPERATIONS even though they
;;; aren't "really supported"

;;; These 3 are ignored since we don't have buffered output
(DEFMETHOD (STREAM-MIXIN :CLEAR-OUTPUT) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :FORCE-OUTPUT) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :FINISH) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :CLOSE) (&OPTIONAL IGNORE)
  NIL)

(DEFMETHOD (STREAM-MIXIN :LINE-IN) (&OPTIONAL LEADER)
  (STREAM-DEFAULT-HANDLER SELF :LINE-IN LEADER NIL))

(DEFMETHOD (STREAM-MIXIN :STRING-IN) (EOF &REST REST)
  (DECLARE (ARGLIST EOF STRING &OPTIONAL START END))
  (STREAM-DEFAULT-HANDLER SELF :STRING-IN EOF REST))

(DEFMETHOD (STREAM-MIXIN :STRING-LINE-IN) (EOF &REST REST)
  (DECLARE (ARGLIST EOF STRING &OPTIONAL START END))
  (STREAM-DEFAULT-HANDLER SELF :STRING-LINE-IN EOF REST))


(DEFFLAVOR LINE-TRUNCATING-MIXIN () ()
  (:REQUIRED-FLAVORS STREAM-MIXIN)
  (:DOCUMENTATION :MIXIN "Causes stream output functions to truncate if the
SHEET-TRUNCATE-LINE-OUT-FLAG in the window is set."))

(DEFWRAPPER (LINE-TRUNCATING-MIXIN :TYO) (IGNORE . BODY)
  `(CATCH 'LINE-OVERFLOW
     . ,BODY))

(DEFMETHOD (LINE-TRUNCATING-MIXIN :BEFORE :END-OF-LINE-EXCEPTION) ()
  (OR (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG))
      (THROW 'LINE-OVERFLOW T)))

(DEFWHOPPER (LINE-TRUNCATING-MIXIN :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (DO ((I START (1+ CR-IDX))
       (CR-IDX))
      (( I END))
    (SETQ CR-IDX (POSITION #/NEWLINE STRING :START I :END END))
    (CATCH 'LINE-OVERFLOW
      (CONTINUE-WHOPPER STRING I (OR CR-IDX END)))
    (OR CR-IDX (RETURN NIL))
    (SHEET-CRLF SELF)))

(DEFFLAVOR TRUNCATING-WINDOW () (LINE-TRUNCATING-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :TRUNCATE-LINE-OUT-FLAG 1)
  (:DOCUMENTATION :COMBINATION "A window that truncates line of output."))

(DEFFLAVOR AUTOEXPOSING-MORE-MIXIN () ()
  (:REQUIRED-FLAVORS WINDOW)
  (:DOCUMENTATION :MIXIN
   "Makes a window expose itself if output on it stops at a **MORE**."))

(DEFMETHOD (AUTOEXPOSING-MORE-MIXIN :BEFORE :MORE-EXCEPTION) ()
  (SEND SELF :EXPOSE))

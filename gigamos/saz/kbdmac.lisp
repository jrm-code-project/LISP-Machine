;;; ZWEI keyboard macros -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFVAR MACRO-ESCAPE-CHAR #/MACRO
  "The character that tells a macro stream to define//execute a macro.")
(DEFVAR MACRO-STREAM :UNBOUND
  "The stream to do I//O through, in a macro stream.")
(DEFVAR MACRO-LEVEL :UNBOUND
  "Current depth in definition//execution of keyboard macros, in a macro stream.")
(DEFVAR MACRO-UNTYI :UNBOUND
  "Holds an untyi'd character, in a macro stream.")
(DEFVAR MACRO-LEVEL-ARRAY :UNBOUND
  "Array of macro being defined//executed at each level, in a macro stream.")
(DEFVAR MACRO-CURRENT-ARRAY :UNBOUND
  "Macro currently being executed.")
(DEFVAR MACRO-PREVIOUS-ARRAY)
(DEFVAR MACRO-READING NIL
  "T when executing a macro, in the macro stream.")
(DEFVAR MACRO-REDIS-LEVEL -1
  "Macro level last displayed, in the macro stream.")
(DEFVAR MACRO-OPERATIONS :UNBOUND
  "List of operations implemented by this macro stream.")
(DEFVAR MACRO-INPUT-RECORD NIL
  "This is a circular buffer array holding the last 60. characters read from this macro stream.")
(defvar macro-alist nil
  "Alist of all (user-defined) non-macro-a keyboard macros defined")



(DEFSTRUCT (MACRO-ARRAY ARRAY-LEADER (:MAKE-ARRAY (:LENGTH #o100)) (:ALTERANT NIL))
  (MACRO-POSITION 0 :DOCUMENTATION "Current position reading or writing")
  (MACRO-LENGTH 0 :DOCUMENTATION "Length of macro")
  (MACRO-COUNT NIL :DOCUMENTATION "Current repeat count for macro")
  (MACRO-DEFAULT-COUNT NIL :DOCUMENTATION "Initial value of MACRO-COUNT, or NIL if writing")
  (MACRO-NAME NIL :DOCUMENTATION "Name of macro as a string, or NIL if temporary."))

;;; The following structure is used for the MACRO-A command.
;;; It is important that it be a LIST since that is how it is identified.
(DEFSTRUCT (MACRO-A LIST (:ALTERANT NIL))
  (MACRO-A-NAME '*A* :DOCUMENTATION "Symbol by which this is recognized.")
  (MACRO-A-VALUE NIL :DOCUMENTATION "Current value of the character.")
  (MACRO-A-STEP NIL :DOCUMENTATION "Number to increase VALUE by on each step.")
  (MACRO-A-INITIAL-VALUE NIL :DOCUMENTATION "Initial current-value given by user."))

(DEFUN MAKE-MACRO-STREAM (STREAM)
  "Create a keyboard-macro stream that does its I//O through STREAM.
The keyboard macro stream support definition and replay of macros."
  (LET-CLOSED ((MACRO-STREAM STREAM)
               (MACRO-LEVEL -1)
               (MACRO-UNTYI NIL)
               (MACRO-LEVEL-ARRAY (MAKE-ARRAY 16.))
               (MACRO-CURRENT-ARRAY NIL)
               (MACRO-PREVIOUS-ARRAY NIL)
               (MACRO-INPUT-RECORD (MAKE-ARRAY 60. ':LEADER-LIST '(60. 0)))
               (MACRO-OPERATIONS
                 (LET ((OPS (COPY-LIST (SEND STREAM ':WHICH-OPERATIONS))))
                   (MAPC #'(LAMBDA (X) (SETQ OPS (DELQ X OPS)))
                         '(:TYI :UNTYI :LISTEN :CLEAR-INPUT :MACRO-LEVEL :MACRO-ERROR
                           :MACRO-EXECUTE :LINE-IN :RUBOUT-HANDLER))
                   `(:TYI :UNTYI :LISTEN :CLEAR-INPUT :MACRO-LEVEL :MACRO-ERROR
                     :MACRO-EXECUTE :MACRO-PUSH :MACRO-POP :MACRO-QUERY :MACRO-PREVIOUS-ARRAY
                     . ,OPS))))
    'MACRO-STREAM-IO))

(DEFVAR MACRO-ERROR-HOOK NIL
  "If non-NIL, funcalled by macro stream after :MACRO-ERROR operation.")

(defun remember-last-keyboard-macro ()
  (do* ((name (typein-line-read "Type a name for this macro:"
                                 (or macro-alist (list (list "No keyboard macros defined."))))
              (typein-line-read "Type a name for this macro:"
                                 (or macro-alist (list (list "No keyboard macros defined.")))))
        (repeat-p (assq name macro-alist)  (assq name macro-alist)))
       ((not repeat-p) (pushnew (cons name macro-current-array)
                                macro-alist)
        (return (putprop name macro-current-array 'current-macro-array)))
    (and (y-or-n-p "~S is already the name of a macro; overwrite it?" name)
         (progn (setq macro-alist (delq (car (assq name macro-alist)) macro-alist))
                (pushnew (cons name macro-current-array)
                         macro-alist)
                (return (putprop name macro-current-array 'current-macro-array))))))





(defvar MACRO-POP-HOOK 'remember-last-keyboard-macro
  "funcalled by macro stream after :MACRO-POP operation.")

(DEFSELECT (MACRO-STREAM-IO MACRO-STREAM-DEFAULT-HANDLER T)
  (:WHICH-OPERATIONS ()
   MACRO-OPERATIONS)
  (:UNTYI (CH)
    (DECF (ARRAY-LEADER MACRO-INPUT-RECORD 1))
    (IF (MINUSP (ARRAY-LEADER MACRO-INPUT-RECORD 1))
        (SETF (ARRAY-LEADER MACRO-INPUT-RECORD 1)
              (1- (ARRAY-LENGTH MACRO-INPUT-RECORD))))
    (SETQ MACRO-UNTYI CH))
  ((:TYI :ANY-TYI :MOUSE-OR-KBD-TYI
    :TYI-NO-HANG :ANY-TYI-NO-HANG :MOUSE-OR-KBD-TYI-NO-HANG)
   . MACRO-STREAM-IO-TYI)
  (:PLAYBACK ()
    MACRO-INPUT-RECORD)
  (:WAIT-FOR-INPUT-WITH-TIMEOUT (TIMEOUT)
   (COND (MACRO-UNTYI T)
         ((OR MACRO-READING
              (NULL MACRO-CURRENT-ARRAY)
              (NULL (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY))
              (MEMQ (AREF MACRO-CURRENT-ARRAY (MACRO-POSITION MACRO-CURRENT-ARRAY))
                    '(*SPACE* *MOUSE* *MICE* NIL)))
          (SEND MACRO-STREAM ':WAIT-FOR-INPUT-WITH-TIMEOUT TIMEOUT))
         (T T)))
  (:LISTEN ()
   (COND (MACRO-UNTYI T)
         ((OR MACRO-READING
              (NULL MACRO-CURRENT-ARRAY)
              (NULL (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY))
              (MEMQ (AREF MACRO-CURRENT-ARRAY (MACRO-POSITION MACRO-CURRENT-ARRAY))
                    '(*SPACE* *MOUSE* *MICE* NIL)))
          (SEND MACRO-STREAM ':LISTEN))
         (T T)))
  (:MACRO-LEVEL ()
   (1+ MACRO-LEVEL))
  (:MACRO-ERROR ()
    ;; If executing a macro, stop and return T.  Else return NIL.
   (PROG1 (AND MACRO-CURRENT-ARRAY (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY))
          (MACRO-STOP NIL)
          (IF MACRO-ERROR-HOOK (FUNCALL MACRO-ERROR-HOOK))))
  (:CLEAR-INPUT ()
   (MACRO-STOP NIL)
   (SETQ MACRO-UNTYI NIL)
   (SEND MACRO-STREAM ':CLEAR-INPUT))
  (:MACRO-EXECUTE (&OPTIONAL ARRAY TIMES)
    ;; Execute the macro ARRAY, TIMES times.
    (COND ((OR ARRAY (SETQ ARRAY MACRO-PREVIOUS-ARRAY))
           (MACRO-PUSH-LEVEL (MACRO-STORE ARRAY))
           (AND TIMES
                (SETF (MACRO-COUNT ARRAY) TIMES)))
          (T
           (BEEP))))
  (:MACRO-PUSH (&OPTIONAL N APPEND-TO)
    ;Start defining another macro.
    ;; N is number of most recent chars that were the command to do so.
    ;; APPEND-TO is a macro to append to, or NIL.
   (AND MACRO-CURRENT-ARRAY             ;Erase the command that caused this to happen
        N
        (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY)
              (- (MACRO-POSITION MACRO-CURRENT-ARRAY) N)))
   (COND (APPEND-TO
          (SETF (MACRO-DEFAULT-COUNT APPEND-TO) NIL)
          (INCF (MACRO-POSITION APPEND-TO))))
   (MACRO-PUSH-LEVEL (MACRO-STORE (OR APPEND-TO T)) (NULL APPEND-TO)))
  (:MACRO-POP (&OPTIONAL N TIMES)
   ;; Finished defining the innermost macro.
   ;; N is number of most recent chars that were the command to do so.
   ;; TIMES is number of times to execute (counting definition as 1).
   (AND MACRO-CURRENT-ARRAY
        N
        (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY)
              (- (MACRO-POSITION MACRO-CURRENT-ARRAY) N)))
   (PROG1 (MACRO-REPEAT TIMES)
          (IF MACRO-POP-HOOK (FUNCALL MACRO-POP-HOOK))))
  (:MACRO-QUERY (&OPTIONAL N)
    ;; Record that user is being queried now.
    (AND MACRO-CURRENT-ARRAY
         N
         (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY)
               (- (MACRO-POSITION MACRO-CURRENT-ARRAY) N)))
    (MACRO-STORE '*SPACE*))
  (:MACRO-PREVIOUS-ARRAY ()
   MACRO-PREVIOUS-ARRAY))

(DEFCONST RECORD-ALL-INPUT T
  "Non-NIL means record all input that comes through the macro stream, per buffer.")

(DEFUN PRINT-BUFFER-INPUT (&OPTIONAL (NBUFS 10.) (INT *INTERVAL*))
  "Print the last NBUFS buffers of saved input that applies to interval INT.
Each buffer is 400. characters worth.  NBUFS defaults to 10."
  (DOLIST (ARRAY (REVERSE (FIRSTN NBUFS (GET INT 'INPUT-LIST))))
    (DOTIMES (I (LENGTH ARRAY))
      (IF (LISTP (AREF ARRAY I))
          (PRIN1 (AREF ARRAY I))
        (FORMAT:OCHAR (AREF ARRAY I) ':EDITOR))
      (TYO #/SP))))

(DEFVAR RECORD-INPUT-INTERVAL NIL)

(DEFUN MACRO-STREAM-IO-TYI (OP &OPTIONAL IGNORE)
  (LET ((CHAR
          (COND (MACRO-UNTYI (PROG1 MACRO-UNTYI (SETQ MACRO-UNTYI NIL)))
                (MACRO-READING
                 (MACRO-UPDATE-LEVEL)
                 (SEND MACRO-STREAM OP))
                (T (MACRO-TYI OP)))))
    (WHEN CHAR
      (WHEN RECORD-ALL-INPUT
        (UNLESS (EQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
          (SETQ RECORD-INPUT-INTERVAL *INTERVAL*))
        (LET ((INPUT-LIST (GET RECORD-INPUT-INTERVAL 'INPUT-LIST)))
          (WHEN (OR (NULL INPUT-LIST)
                    ( (LENGTH (CAR INPUT-LIST)) 400.))
            (PUTPROP RECORD-INPUT-INTERVAL
                     (SETQ INPUT-LIST (CONS (MAKE-ARRAY 400. ':FILL-POINTER 0)
                                            INPUT-LIST))
                     'INPUT-LIST))
          (ARRAY-PUSH (CAR INPUT-LIST) CHAR)))
      (SETF (ARRAY-LEADER MACRO-INPUT-RECORD 1)
            (\ (1+ (ARRAY-LEADER MACRO-INPUT-RECORD 1))
               (ARRAY-LENGTH MACRO-INPUT-RECORD)))
      (SETF (AREF MACRO-INPUT-RECORD (ARRAY-LEADER MACRO-INPUT-RECORD 1))
            CHAR))
    CHAR))

(DEFUN MACRO-STREAM-DEFAULT-HANDLER (OP &REST REST)
  (IF (AND (MEMQ OP MACRO-OPERATIONS)
           (NOT (MEMQ OP '(:SEND-IF-HANDLES :OPERATION-HANDLED-P))))
      (LEXPR-SEND MACRO-STREAM OP REST)
    (STREAM-DEFAULT-HANDLER 'MACRO-STREAM-IO OP (CAR REST) (CDR REST))))

(DEFUN MACRO-TYI (&OPTIONAL (OP ':MOUSE-OR-KBD-TYI))
  (DO ((CH) (TEM) (NUMARG) (FLAG) (TEM2) (SUPPRESS))
      (())
   (*CATCH 'MACRO-LOOP
    (COND ((AND MACRO-CURRENT-ARRAY (SETQ TEM2 (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY)))
           (SETQ TEM (MACRO-POSITION MACRO-CURRENT-ARRAY)
                 CH (AREF MACRO-CURRENT-ARRAY TEM))
           (COND ((EQ CH '*SPACE*)
                  (SELECTQ (SEND MACRO-STREAM ':MOUSE-OR-KBD-TYI)
                   (#/SP
                    (SETQ CH '*IGNORE*))
                   ((#/? #/HELP)
                    (FORMAT T "~&You are in an interactive macro.
Space continues on, Rubout skips this one, Form refreshes the screen,
Control-R enters a typein macro level (~:C R exits), anything else exits." MACRO-ESCAPE-CHAR)
                    (*THROW 'MACRO-LOOP NIL))
                   (#/RUBOUT
                    (SETQ TEM (MACRO-LENGTH MACRO-CURRENT-ARRAY)
                          CH '*IGNORE*))
                   ((#/C-R #/C-/r)
                    (SETQ CH NIL))
                   (#/FF
                    (RETURN #/FF))
                   (#/.
                    (SETF (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY) 0)
                    (SETF (MACRO-COUNT MACRO-CURRENT-ARRAY) 0)
                    (SETQ CH '*IGNORE*))
                   (#/!
                    (ASET '*RUN* MACRO-CURRENT-ARRAY TEM)
                    (SETQ CH '*IGNORE*))
                   (OTHERWISE
                    (MACRO-STOP 1)
                    (*THROW 'MACRO-LOOP NIL))))
                 ((MEMQ CH '(*MOUSE* *MICE*))
                  (AND (EQ CH '*MOUSE*) (FORMAT T "~&Use the mouse.~%"))
                  (SETQ CH (SEND MACRO-STREAM ':MOUSE-OR-KBD-TYI))
                  (COND ((TV:CHAR-MOUSE-P CH)
                         (ASET '*MICE* MACRO-CURRENT-ARRAY TEM)
                         (RETURN CH))
                        (T
                         (ASET '*MOUSE* MACRO-CURRENT-ARRAY TEM)
                         (SETQ CH '*IGNORE*)))))
           (COND ((AND (ZEROP TEM)
                       (EQ TEM2 '*REPEAT*)
                       (MEMQ ':MACRO-TERMINATE MACRO-OPERATIONS)
                       (SEND MACRO-STREAM ':MACRO-TERMINATE))
                  (COND (( (SETQ MACRO-LEVEL (1- MACRO-LEVEL)) 0)
                         (SETQ MACRO-CURRENT-ARRAY
                               (AREF MACRO-LEVEL-ARRAY MACRO-LEVEL)))
                        (T
                         (SETQ MACRO-CURRENT-ARRAY NIL))))
                 ((< TEM (MACRO-LENGTH MACRO-CURRENT-ARRAY))
                  (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY) (1+ TEM)))
                 ((EQ TEM2 '*REPEAT*)
                  (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY) 0))
                 ((> (SETQ TEM (1- (MACRO-COUNT MACRO-CURRENT-ARRAY))) 0)
                  (SETF (MACRO-COUNT MACRO-CURRENT-ARRAY) TEM)
                  (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY) 0))
                 (( (SETQ MACRO-LEVEL (1- MACRO-LEVEL)) 0)
                  (SETQ MACRO-CURRENT-ARRAY (AREF MACRO-LEVEL-ARRAY MACRO-LEVEL)))
                 (T
                  (SETQ MACRO-CURRENT-ARRAY NIL)))
           (COND ((NUMBERP CH) (OR SUPPRESS (RETURN CH)))
                 ((MEMQ CH '(*RUN* *IGNORE*)))
                 ((AND (CONSP CH) (EQ (CAR CH) '*A*))
                  (LET ((X (MACRO-A-VALUE CH)))
                    (SETF (MACRO-A-VALUE CH) (+ X (MACRO-A-STEP CH)))
                    (OR SUPPRESS (RETURN X))))
                 (T (MACRO-PUSH-LEVEL CH))))
          (T
           (MACRO-UPDATE-LEVEL)
           (MULTIPLE-VALUE (CH TEM) (SEND MACRO-STREAM OP))
           (COND (FLAG
                  (OR (NUMBERP CH) (MACRO-BARF))
                  (SETQ CH (CHAR-UPCASE CH))
                  (COND ((AND ( #/0 CH #/9)
                         (SETQ NUMARG (+ (- CH #/0) (* (OR NUMARG 0) 10.)))))
                        (T
                         (SETQ FLAG NIL)
                         (SELECTQ CH
                           (#/C
                            (SETQ TEM (MACRO-DO-READ "Macro to call: "))
                            (OR (SETQ TEM (GET TEM 'MACRO-STREAM-MACRO)) (MACRO-BARF))
                            (MACRO-STORE TEM)
                            (OR SUPPRESS (MACRO-PUSH-LEVEL TEM)))
                           (#/D
                            (SETQ SUPPRESS MACRO-LEVEL)
                            (MACRO-PUSH-LEVEL (MACRO-MAKE-NAMED-MACRO)))
                           (#/M
                            (MACRO-PUSH-LEVEL (MACRO-STORE (MACRO-MAKE-NAMED-MACRO))))
                           (#/P
                            (MACRO-PUSH-LEVEL (MACRO-STORE)))
                           (#/R
                            (MACRO-REPEAT NUMARG)
                            (AND (EQ SUPPRESS MACRO-LEVEL) (SETQ SUPPRESS NIL)))
                           (#/S
                            (MACRO-STOP NUMARG))
                           (#/T
                            (MACRO-PUSH-LEVEL (MACRO-STORE NIL)))
                           (#/U
                            (MACRO-PUSH-LEVEL NIL))
                           (#/SP
                            (MACRO-STORE '*SPACE*))
                           (#/A
                            (LET ((STR (MACRO-READ-STRING
                                         "Initial character (type a one-character string):")))
                              (OR (= (STRING-LENGTH STR) 1) (MACRO-BARF))
                              (LET ((VAL (AREF STR 0))
                                    (NUM (MACRO-READ-NUMBER
                                  "Amount by which to increase it (type a decimal number):")))
                                (MACRO-STORE (MAKE-MACRO-A :MACRO-A-VALUE (+ VAL NUM)
                                                           :MACRO-A-STEP NUM
                                                           :MACRO-A-INITIAL-VALUE VAL))
                                (OR SUPPRESS (RETURN VAL)))))
                           (#/HELP
                            (FORMAT T "~&Macro commands are:
P push a level of macro, R end and repeat arg times, C call a macro by name,
S stop macro definition, U allow typein now only, T allow typein in expansion too.
/(terminate typein with ~:C R/)
M define a named macro, D define a named macro but don't execute as building.
Space enter macro query, A store an increasing character string.
Now type a macro command: "
                                    MACRO-ESCAPE-CHAR)
                            (SETQ FLAG T))
                           (OTHERWISE
                            (MACRO-BARF))))))
                 ((EQ CH MACRO-ESCAPE-CHAR)
                  (SETQ FLAG T NUMARG NIL))
                 (T
                  (AND (NUMBERP CH) (MACRO-STORE (IF (TV:CHAR-MOUSE-P CH) '*MOUSE* CH)))
                  (OR SUPPRESS (RETURN (VALUES CH TEM))))))))))

(DEFUN MACRO-PUSH-LEVEL (MAC &OPTIONAL (RESET-POSITION T))
  (COND (MAC
          (AND (SYMBOLP MAC) (SETQ MAC (GET MAC 'MACRO-STREAM-MACRO)))
          (OR (ARRAYP MAC) (MACRO-BARF))))
  (SETQ MACRO-LEVEL (1+ MACRO-LEVEL)
        MACRO-CURRENT-ARRAY MAC)
  (AND ( MACRO-LEVEL (ARRAY-LENGTH MACRO-LEVEL-ARRAY))
       (ADJUST-ARRAY-SIZE MACRO-LEVEL-ARRAY (FLOOR (* MACRO-LEVEL 3) 2)))
  (ASET MAC MACRO-LEVEL-ARRAY MACRO-LEVEL)
  (COND (MAC
          (AND RESET-POSITION (SETF (MACRO-POSITION MAC) 0))
          (SETF (MACRO-COUNT MAC) (MACRO-DEFAULT-COUNT MAC))
          (DO ((I 0 (1+ I))
               (X)
               (LIM (MACRO-LENGTH MAC)))
              ((> I LIM))
            (SETQ X (AREF MAC I))
            (COND ((EQ '*RUN* X)
                   (ASET '*SPACE* MAC I))
                  ((EQ '*MICE* X)
                   (ASET '*MOUSE* MAC I))
                  ((AND (CONSP X) (EQ (CAR X) '*A*))
                   (SETF (MACRO-A-VALUE X) (MACRO-A-INITIAL-VALUE X)))
                  )))))

(DEFUN MACRO-STORE (&OPTIONAL (THING T))
  (AND (EQ THING T) (SETQ THING (MAKE-MACRO-ARRAY)))
  (AND MACRO-CURRENT-ARRAY (ARRAY-PUSH-EXTEND MACRO-CURRENT-ARRAY THING))
  THING)

(DEFUN MACRO-BARF ()
  (BEEP)
  (*THROW 'MACRO-LOOP NIL))

(DEFUN MACRO-REPEAT (ARG &AUX (TEM -1))
  (AND (< MACRO-LEVEL 0) (MACRO-BARF))
  (COND (MACRO-CURRENT-ARRAY
          (OR ARG (SETQ ARG '*REPEAT*))
          (SETF (MACRO-DEFAULT-COUNT MACRO-CURRENT-ARRAY) ARG)
          (SETQ TEM (1- (MACRO-POSITION MACRO-CURRENT-ARRAY)))
          (SETF (MACRO-LENGTH MACRO-CURRENT-ARRAY) TEM)
          (SETQ MACRO-PREVIOUS-ARRAY MACRO-CURRENT-ARRAY)))
  (COND ((AND ( TEM 0) (NUMBERP ARG) (> ARG 1))
         (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY) 0)
         (SETF (MACRO-COUNT MACRO-CURRENT-ARRAY) (1- ARG)))
        ((EQ ARG '*REPEAT*)
         (SETF (MACRO-POSITION MACRO-CURRENT-ARRAY) 0))
        (( (SETQ MACRO-LEVEL (1- MACRO-LEVEL)) 0)
         (SETQ MACRO-CURRENT-ARRAY
               (AREF MACRO-LEVEL-ARRAY MACRO-LEVEL)))
        (T (SETQ MACRO-CURRENT-ARRAY NIL))))

(DEFUN MACRO-MAKE-NAMED-MACRO (&AUX TEM MAC)
  (SETQ TEM (MACRO-DO-READ "Name of macro to define: "))
  (OR (SYMBOLP TEM) (MACRO-BARF))
  (SETQ MAC (MAKE-MACRO-ARRAY))
  (PUTPROP TEM MAC 'MACRO-STREAM-MACRO)
  (SETF (MACRO-NAME MAC) (STRING TEM))
  MAC)

(DEFUN MACRO-READ-STRING (STR &AUX (MACRO-READING T) (MACRO-REDIS-LEVEL -1))
  (IF (MEMQ ':READ-MACRO-LINE MACRO-OPERATIONS)
      (SEND MACRO-STREAM ':READ-MACRO-LINE STR)
    (PRINC STR MACRO-STREAM)
    (READLINE MACRO-STREAM)))

(DEFUN MACRO-DO-READ (STR)
  (INTERN (STRING-UPCASE (STRING-TRIM '(#/SP #/TAB) (MACRO-READ-STRING STR)))
          ""))

(DEFUN MACRO-READ-NUMBER (STR)
  (LET ((NUM (READ-FROM-STRING (MACRO-READ-STRING STR))))
    (OR (NUMBERP NUM) (MACRO-BARF))
    NUM))

(DEFUN MACRO-STOP (NUM)
  "Stop defining or executing the innermost NUM macros."
  (SETQ MACRO-LEVEL (MAX -1 (- MACRO-LEVEL (OR NUM 200000)))
        MACRO-CURRENT-ARRAY (AND ( MACRO-LEVEL 0)
                                 (AREF MACRO-LEVEL-ARRAY MACRO-LEVEL))))

(DEFUN MACRO-UPDATE-LEVEL ()
  "Maybe tell the i//o stream to redisplay the macro level (if it has changed)."
  (COND ((AND ( MACRO-LEVEL MACRO-REDIS-LEVEL) (MEMQ ':SET-MACRO-LEVEL MACRO-OPERATIONS))
         (SETQ MACRO-REDIS-LEVEL MACRO-LEVEL)
         (SEND MACRO-STREAM ':SET-MACRO-LEVEL
               (AND (NOT (MINUSP MACRO-LEVEL))
                    (FORMAT NIL "~D" (1+ MACRO-LEVEL)))))))

;;; Handy things for saving out macros on disk and editing them
(DEFMACRO DEFINE-KEYBOARD-MACRO (NAME (COUNT) . EXPANSION)
  `(DEFINE-KEYBOARD-MACRO-1 ',NAME ,(OR COUNT 1) ',(COPYLIST EXPANSION)))

(DEFUN DEFINE-KEYBOARD-MACRO-1 (NAME COUNT EXPANSION &AUX MACRO-ARRAY (LEN 0) STRING)
  (SETQ STRING (STRING NAME)
        NAME (INTERN STRING ""))
  (DOLIST (THING EXPANSION)
    (IF (STRINGP THING)
        (SETQ LEN (+ LEN (STRING-LENGTH THING)))
        (SETQ LEN (1+ LEN))))
  (SETQ MACRO-ARRAY (MAKE-MACRO-ARRAY :MAKE-ARRAY (:LENGTH LEN)
                                      :MACRO-LENGTH (1- LEN)
                                      :MACRO-DEFAULT-COUNT COUNT
                                      :MACRO-NAME STRING))
  (DOLIST (THING EXPANSION)
    (IF (STRINGP THING)
        (APPEND-TO-ARRAY MACRO-ARRAY THING)
        (COND ((NUMBERP THING))
              ((STRING-EQUAL THING '*INPUT*)
               (SETQ THING NIL))
              ((STRING-EQUAL THING '*SPACE*)
               (SETQ THING '*SPACE*))
              ((STRING-EQUAL THING '*MOUSE*)
               (SETQ THING '*MOUSE*))
              ((STRING-EQUAL THING '*MICE*)
               (SETQ THING '*MICE*))
              (T
               (FERROR NIL "~S is not a known macro expansion element." THING)))
        (ARRAY-PUSH MACRO-ARRAY THING)))
  (PUTPROP NAME MACRO-ARRAY 'MACRO-STREAM-MACRO)
  NAME)

(DEFUN PRINT-KEYBOARD-MACRO-DEFINITION (STREAM NAME &OPTIONAL MACRO-ARRAY)
  (LET ((*PACKAGE* (SYMBOL-PACKAGE 'FOO))
        (*PRINT-BASE* ':CHARACTER))
    (SI:GRIND-TOP-LEVEL (GET-KEYBOARD-MACRO-DEFINITION NAME MACRO-ARRAY) 95. STREAM)))

(DEFUN GET-KEYBOARD-MACRO-DEFINITION (NAME MACRO-ARRAY)
  "Return a Lisp form that would define NAME as a keyboard macro.
The definition is MACRO-ARRAY or what NAME is actually defined as."
  (OR MACRO-ARRAY (SETQ MACRO-ARRAY (GET NAME 'MACRO-STREAM-MACRO)))
  (SETQ NAME (INTERN (STRING NAME) (SYMBOL-PACKAGE 'FOO)))
  (DO ((I 0 (1+ I))
       (LEN (1+ (MACRO-LENGTH MACRO-ARRAY)))
       (THING)
       (STATE NIL)
       (LIST NIL)
       (STRING (MAKE-STRING 10. :FILL-POINTER 0)))
      (( I LEN)
       `(DEFINE-KEYBOARD-MACRO ,NAME () . ,(NREVERSE LIST)))
    (SETQ THING (AREF MACRO-ARRAY I))
    (COND ((OR (SYMBOLP THING) (LDB-TEST %%KBD-CONTROL-META THING))
           (COND (STATE
                  (PUSH (STRING-APPEND STRING) LIST)
                  (SETQ STATE NIL)))
           (COND ((NUMBERP THING))
                 ((NULL THING)
                  (SETQ THING '*INPUT*)))
           (PUSH THING LIST))
          (T
           (COND ((NOT STATE)
                  (STORE-ARRAY-LEADER 0 STRING 0)
                  (SETQ STATE T)))
           (ARRAY-PUSH-EXTEND STRING THING)))))

;;; Define CHARACTER as a radix. What a crock! Of course, now we have character objects...
(DEFPROP CHARACTER PRINC-BASE-CHARACTER SI:PRINC-FUNCTION)
;;; Let's be completely gratuitous
(DEFPROP CLI:CHARACTER PRINC-BASE-CHARACTER SI:PRINC-FUNCTION)
(DEFUN PRINC-BASE-CHARACTER (-N STREAM)
  (FORMAT STREAM "~@C" (- -N)))

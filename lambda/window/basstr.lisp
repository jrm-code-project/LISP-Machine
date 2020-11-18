;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains:  IO buffers, keyboard process

;;; IO buffers (definition in TVDEFS)

(DEFSELECT ((:PROPERTY IO-BUFFER NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (SELF STREAM IGNORE &OPTIONAL IGNORE)
    (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :NO-POINTER :TYPE)
      (FORMAT STREAM "~O: " (%POINTER SELF))
      (IF (= (IO-BUFFER-INPUT-POINTER SELF)
             (IO-BUFFER-OUTPUT-POINTER SELF))
          (PRINC "empty, " STREAM)
        (FORMAT STREAM "~D entr~:@P, "
                (LET ((DIFF (- (IO-BUFFER-INPUT-POINTER SELF)
                               (IO-BUFFER-OUTPUT-POINTER SELF))))
                  (IF (< DIFF 0)
                      (+ DIFF (IO-BUFFER-SIZE SELF))
                    DIFF))))
      (FORMAT STREAM "State: ~A" (IO-BUFFER-STATE SELF))))
  ((:GET :GET-LOCATION-OR-NIL :GET-LOCATION :GETL :PUTPROP :REMPROP :PUSH-PROPERTY :PLIST
    :PLIST-LOCATION :PROPERTY-LIST-LOCATION :SETPLIST :SET)
   . IO-BUFFER-PROPERTY-LIST-HANDLER))
(DEFUN IO-BUFFER-PROPERTY-LIST-HANDLER (OP SELF &REST ARGS)
  (APPLY #'SI:PROPERTY-LIST-HANDLER OP (LOCF (IO-BUFFER-PLIST SELF)) ARGS))

(DEFUN MAKE-IO-BUFFER (SIZE &OPTIONAL IN-FUN OUT-FUN PLIST STATE &AUX BUFFER)
  "Create a new IO buffer of specified size.
The actual size is 2 greater, since that 2 elements must always be empty."
  (SETQ BUFFER (MAKE-ARRAY (+ 2 SIZE)
                           :LEADER-LENGTH IO-BUFFER-LEADER-SIZE
                           :NAMED-STRUCTURE-SYMBOL 'IO-BUFFER))
  (SETF (IO-BUFFER-FILL-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-SIZE BUFFER) SIZE)
  (SETF (IO-BUFFER-INPUT-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-INPUT-FUNCTION BUFFER) IN-FUN)
  (SETF (IO-BUFFER-OUTPUT-FUNCTION BUFFER) OUT-FUN)
  (SETF (IO-BUFFER-STATE BUFFER) STATE)
  (SETF (IO-BUFFER-PLIST BUFFER) PLIST)
  (SETF (IO-BUFFER-RECORD BUFFER) (MAKE-ARRAY IO-BUFFER-RECORD-LENGTH
                                              :LEADER-LIST `(,IO-BUFFER-RECORD-LENGTH 0)))
  BUFFER)

(DEFUN MAKE-DEFAULT-IO-BUFFER ()
  (MAKE-IO-BUFFER #o100 NIL 'KBD-DEFAULT-OUTPUT-FUNCTION))

(DEFUN IO-BUFFER-PUT (BUFFER ELT &OPTIONAL (NO-HANG-P NIL))
  "Store a new element in an IO buffer, at the end (FIFO)."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (IGNORE-P)
       (INPUT-POINTER)
       (IN-FUN (IO-BUFFER-INPUT-FUNCTION BUFFER)))
      (())
    (COND ((OR (NULL (IO-BUFFER-STATE BUFFER))
               (EQ (IO-BUFFER-STATE BUFFER) ':INPUT))
           (COND (IN-FUN
                  ;; Call function with INHIBIT-SCHEDULING-FLAG turned on and bound.
                  ;; Since this function may change the state of the buffer either directly
                  ;; or indirectly, loop in order to check the state.  Set the function to
                  ;; NIL, though, so it won't be run again
                  (MULTIPLE-VALUE (ELT IGNORE-P)
                    (FUNCALL IN-FUN BUFFER ELT))
                  (AND IGNORE-P (RETURN T))
                  (SETQ IN-FUN NIL))
                 (T
                  (COND ((NOT (IO-BUFFER-FULL-P BUFFER))
                         (SETF (IO-BUFFER-LAST-INPUT-PROCESS BUFFER) CURRENT-PROCESS)
                         (SETQ INPUT-POINTER (IO-BUFFER-INPUT-POINTER BUFFER))
                         (ASET ELT BUFFER INPUT-POINTER)
                         (SETF (IO-BUFFER-INPUT-POINTER BUFFER)
                               (\ (1+ INPUT-POINTER) (IO-BUFFER-SIZE BUFFER)))
                         (RETURN T))
                        (NO-HANG-P (RETURN NIL))
                        (T
                          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                          (PROCESS-WAIT "Buffer full" #'(LAMBDA (BUF)
                                                          (NOT (IO-BUFFER-FULL-P BUF)))
                                        BUFFER))))))
          (NO-HANG-P (RETURN NIL))
          (T
           (SETQ INHIBIT-SCHEDULING-FLAG NIL)
           (PROCESS-WAIT "Buffer state" #'(LAMBDA (BUF)
                                            (OR (NULL (IO-BUFFER-STATE BUF))
                                                (EQ (IO-BUFFER-STATE BUF) ':INPUT)))
                         BUFFER)))))

(DEFUN IO-BUFFER-GET (BUFFER &OPTIONAL (NO-HANG-P NIL))
  "Get an element from an IO buffer.  First value is elt, second is T if got one, else NIL"
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS BUFFER) CURRENT-PROCESS)
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (ELT)
       (IGNORE-P)
       (OUTPUT-POINTER)
       (OUT-FUN (IO-BUFFER-OUTPUT-FUNCTION BUFFER)))
      (())
    (COND ((OR (NULL (IO-BUFFER-STATE BUFFER))
               (EQ (IO-BUFFER-STATE BUFFER) ':OUTPUT))
           (COND ((NOT (IO-BUFFER-EMPTY-P BUFFER))
                  (SETQ OUTPUT-POINTER (IO-BUFFER-OUTPUT-POINTER BUFFER))
                  (SETQ ELT (AREF BUFFER OUTPUT-POINTER))
                  (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER)
                        (\ (1+ OUTPUT-POINTER) (IO-BUFFER-SIZE BUFFER)))
                  ;; Stick this element into the record of the last few input chars.
                  (AND (IO-BUFFER-RECORD BUFFER)
                       (LET ((INPUT-RECORD (IO-BUFFER-RECORD BUFFER)))
                         (INCF (IO-BUFFER-RECORD-POINTER INPUT-RECORD))
                         (AND (= (IO-BUFFER-RECORD-POINTER INPUT-RECORD)
                                 (ARRAY-LENGTH INPUT-RECORD))
                              (SETF (IO-BUFFER-RECORD-POINTER INPUT-RECORD) 0))
                         (SETF (AREF INPUT-RECORD (IO-BUFFER-RECORD-POINTER INPUT-RECORD))
                               ELT)))
                  (COND ((AND OUT-FUN
                              ;; Call function with INHIBIT-SCHEDULING-FLAG on and bound.
                              ;; If element is to be ignored, loop back, else return element
                              (PROG2
                                (MULTIPLE-VALUE (ELT IGNORE-P)
                                  (FUNCALL OUT-FUN BUFFER ELT))
                                IGNORE-P)))
                        (T (RETURN (VALUES ELT T)))))
                 (NO-HANG-P (RETURN (VALUES NIL NIL)))
                 (T
                  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                  (PROCESS-WAIT "Buffer empty" #'(LAMBDA (BUF)
                                                   (NOT (IO-BUFFER-EMPTY-P BUF)))
                                BUFFER))))
          (NO-HANG-P (RETURN (VALUES NIL NIL)))
          (T
           (SETQ INHIBIT-SCHEDULING-FLAG NIL)
           (PROCESS-WAIT "Buffer state" #'(LAMBDA (BUF)
                                            (OR (NULL (IO-BUFFER-STATE BUF))
                                                (EQ (IO-BUFFER-STATE BUF) ':OUTPUT)))
                         BUFFER)))))

(DEFUN IO-BUFFER-UNGET (BUFFER ELT)
  "Reinsert ELT into the IO-BUFFER by backing up the pointer.
ELT should be the last thing read from the buffer."
  (WITHOUT-INTERRUPTS
    (LET ((OUTPUT-POINTER (1- (IO-BUFFER-OUTPUT-POINTER BUFFER))))
      (AND (< OUTPUT-POINTER 0)
           (SETQ OUTPUT-POINTER (1- (IO-BUFFER-SIZE BUFFER))))
      (OR (EQ ELT (AREF BUFFER OUTPUT-POINTER))
          (FERROR
            "Attempt to un-get something different from last element gotten from IO-BUFFER"))
      ;; Remove this element from the record of the last few input chars.
      (AND (IO-BUFFER-RECORD BUFFER)
           (LET ((INPUT-RECORD (IO-BUFFER-RECORD BUFFER)))
             (DECF (IO-BUFFER-RECORD-POINTER INPUT-RECORD))
             (AND (MINUSP (IO-BUFFER-RECORD-POINTER INPUT-RECORD))
                  (SETF (IO-BUFFER-RECORD-POINTER INPUT-RECORD)
                        (1- (ARRAY-LENGTH INPUT-RECORD))))))
      (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) OUTPUT-POINTER))))

(DEFUN IO-BUFFER-PUSH (BUFFER ELT)
  "Insert ELT into the IO-BUFFER at the front (LIFO fashion)."
  (WITHOUT-INTERRUPTS
    (LET ((OUTPUT-POINTER (1- (IO-BUFFER-OUTPUT-POINTER BUFFER))))
      (AND (< OUTPUT-POINTER 0)
           (SETQ OUTPUT-POINTER (1- (IO-BUFFER-SIZE BUFFER))))
      (IF (= OUTPUT-POINTER (IO-BUFFER-INPUT-POINTER BUFFER))
          (FERROR "IO-BUFFER ~S is full." BUFFER))
      (SETF (AREF BUFFER OUTPUT-POINTER) ELT)
      (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) OUTPUT-POINTER))))

(DEFUN IO-BUFFER-CLEAR (BUFFER)
  "Clears out an IO buffer"
  (WITHOUT-INTERRUPTS
    (SETF (IO-BUFFER-INPUT-POINTER BUFFER) 0)
    (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) 0)
    T))

(DEFUN PROCESS-TYPEAHEAD (IO-BUFFER FUNCTION)
  "Apply FUNCTION to each element of IO-BUFFER.
The value returned by FUNCTION is put back in place of the original element,
except that if the value is NIL, the element is simply deleted."
  (DO ((INPUT-POINTER (IO-BUFFER-INPUT-POINTER IO-BUFFER))
       (CH))
      ((OR (= INPUT-POINTER (IO-BUFFER-OUTPUT-POINTER IO-BUFFER))
           (NULL (SETQ CH (IO-BUFFER-GET IO-BUFFER T)))))
    (AND (SETQ CH (FUNCALL FUNCTION CH))
         (IO-BUFFER-PUT IO-BUFFER CH T))))

(DEFVAR KBD-IO-BUFFER (MAKE-IO-BUFFER #o1000))  ;Intermediate buffer so char is read out of
                                                ; hardware immediatly
(DEFVAR KBD-ESC-HAPPENED NIL)                   ;An escape was typed
(DEFVAR KBD-ESC-TIME NIL)       ;If non-NIL, this is the time we started processing
                                ;an escape (Terminal or System) which is still in process.
                                ;We try not to look at the keyboard while one is still
                                ;in process to provide more predictable behavior with
                                ;typeahead.  However, we don't wait forever so that if
                                ;the process hangs forever the system doesn't "die".
(DEFVAR COLD-LOAD-STREAM-OWNS-KEYBOARD NIL
  "Non-NIL means something reading from cold load stream, so turn off KBD-PROCESS.")

(DEFUN KBD-PROCESS-MAIN-LOOP ()
  "This function runs in the keyboard process.  It is responsible for reading characters
from the hardware, and performing any immediate processing associated with the character."
  (ERROR-RESTART-LOOP ((SYS:ABORT ERROR) "Return to top level of KBD-PROCESS.")
    (IO-BUFFER-CLEAR KBD-IO-BUFFER)
    (SETQ KBD-ESC-HAPPENED NIL)
    (DO-FOREVER
      (PROCESS-WAIT "Keyboard"
                    #'(LAMBDA ()
                        (OR KBD-ESC-HAPPENED
                            (AND (NOT COLD-LOAD-STREAM-OWNS-KEYBOARD)
                                 (NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
                                 (KBD-HARDWARE-CHAR-AVAILABLE)))))
      (WHEN KBD-ESC-HAPPENED
        (APPLY (CADR KBD-ESC-HAPPENED)
               (CAR KBD-ESC-HAPPENED)
               SELECTED-WINDOW
               (CDDR KBD-ESC-HAPPENED))
        (PROCESS-WAIT-WITH-TIMEOUT "ESC Finish" 600.    ;wait at most 10 sec
                                   #'(LAMBDA () (NULL KBD-ESC-TIME)))
        (SETQ KBD-ESC-HAPPENED NIL))
      (KBD-PROCESS-MAIN-LOOP-INTERNAL))))

;;; Note that KBD-CONVERT-TO-SOFTWARE-CHAR must be called in order,
;;; since for the new keyboards it does shifts and keeps state.

(DEFCONST KBD-STANDARD-ASYNCHRONOUS-CHARACTERS
          '((#/C-ABORT KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
             (:NAME "Abort" :PRIORITY 50.)
             KBD-INTERCEPT-ABORT)
            (#/C-M-ABORT KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
             (:NAME "Abort All" :PRIORITY 50.)
             KBD-INTERCEPT-ABORT-ALL)
            (#/C-BREAK KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
             (:NAME "Break" :PRIORITY 40.)
             KBD-INTERCEPT-BREAK)
            (#/C-M-BREAK KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
             (:NAME "Error Break" :PRIORITY 40.)
             KBD-INTERCEPT-ERROR-BREAK))
  "Default alist of asynchronous characters for a window's keyboard input.")

(DEFCONST KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS
          '((#/TERMINAL KBD-ESC)
            (#/SYSTEM KBD-SYS)
            (#/CONTROL-CLEAR-INPUT KBD-ESC-CLEAR))
  "Default alist of keys handled like Terminal and System.")

(defvar *default-character-translator* 'kbd-convert-to-software-char
  "Hook for Dvorak hack.  Can be overridden by the :character-translator
property of io-buffers.")

(DEFUN KBD-PROCESS-MAIN-LOOP-INTERNAL (&AUX BUFFER PLIST RAW-P ASYNCH-CHARS TEM
                                       LOWERCASE-CONTROL-CHARS CHAR SOFT-CHAR
                                       character-translator)
  (WITHOUT-INTERRUPTS
    (SETQ BUFFER (KBD-GET-IO-BUFFER))
    (IF (NULL BUFFER)
        (SETQ ASYNCH-CHARS KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)
      (SETQ PLIST (LOCF (IO-BUFFER-PLIST BUFFER)))
      (SETQ ASYNCH-CHARS
            (IF (GET PLIST ':SUPER-IMAGE)
                NIL
              (GET PLIST ':ASYNCHRONOUS-CHARACTERS KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)))
      (SETQ LOWERCASE-CONTROL-CHARS
            (GET PLIST ':DONT-UPCASE-CONTROL-CHARACTERS))
      (SETQ RAW-P (GET PLIST ':RAW))
      (setq character-translator (get plist :character-translator))
      )
    (DO ()
        ((OR KBD-ESC-HAPPENED
             (NOT (KBD-HARDWARE-CHAR-AVAILABLE))))
      (SETQ CHAR (KBD-GET-HARDWARE-CHAR))
      (IF RAW-P
          (OR (IO-BUFFER-FULL-P BUFFER)
              (IO-BUFFER-PUT BUFFER CHAR))
        (SETQ SOFT-CHAR
              (funcall (or character-translator
                           *default-character-translator*
                           ;'KBD-CONVERT-TO-SOFTWARE-CHAR
                           )
                       CHAR
                       (NOT LOWERCASE-CONTROL-CHARS)))
        (IF (NULL SOFT-CHAR)
            NIL                                 ;unreal character
          (SETQ CHAR (CHAR-CODE SOFT-CHAR));No bucky bits
          ;; Don't count the Terminal key as keyboard activity.
          (OR (= SOFT-CHAR #/TERMINAL)
              (SETQ KBD-LAST-ACTIVITY-TIME (TIME)
                    SI::WHO-LINE-JUST-COLD-BOOTED-P NIL))
          (COND ((SETQ KBD-ESC-HAPPENED (ASSQ SOFT-CHAR KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS)))
                ((SETQ TEM (ASSQ SOFT-CHAR ASYNCH-CHARS))
                 (APPLY (CADR TEM) (CAR TEM) TV:SELECTED-WINDOW (CDDR TEM)))
                ((NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
                 (IO-BUFFER-PUT KBD-IO-BUFFER SOFT-CHAR))))))))

(DEFUN KBD-IO-BUFFER-GET (BUFFER &OPTIONAL (NO-HANG-P NIL) (WHOSTATE "Keyboard"))
  "Get the next element from io-buffer BUFFER or from the keyboard.
The keyboard is checked for input only if this buffer is the selected one.
NO-HANG-P says return NIL immediately if no input is available.
WHOSTATE appears in the who line if we have to wait.
Note that BUFFER's output function is executed even if the input
comes from the keyboard /"directly/"."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (UPDATE-STATE-P (NEQ CURRENT-PROCESS (IO-BUFFER-LAST-OUTPUT-PROCESS BUFFER)))
       (OK)
       (ELT))
      (())
    (MULTIPLE-VALUE (ELT OK)
      (IO-BUFFER-GET BUFFER T))
    ;; If new process reading, better update wholine run state
    (AND UPDATE-STATE-P (EQ BUFFER SELECTED-IO-BUFFER)
         (WHO-LINE-RUN-STATE-UPDATE))
    ;; Got something from the normal buffer, just return it
    (IF OK (RETURN ELT))
    ;; OK is NIL here.  If we aren't selected, don't look at system's io buffer
    (WHEN (EQ BUFFER SELECTED-IO-BUFFER)
      (MULTIPLE-VALUE (ELT OK)
        (IO-BUFFER-GET KBD-IO-BUFFER T)))
    (IF OK
        ;; Got something from the kbd buffer, put it into the normal buffer and loop
        (IO-BUFFER-PUT BUFFER ELT T)            ;Can't hang, but...
      ;; Nothing for baby!!!  What should we do?
      (AND (NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
           (KBD-HARDWARE-CHAR-AVAILABLE)
           ;; If there is a possibility that a character of interest exists in
           ;; the hardware, get it
           (KBD-PROCESS-MAIN-LOOP-INTERNAL))
      (IF (OR (NOT (IO-BUFFER-EMPTY-P BUFFER))
              (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
                   (NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER))))
          NIL                                   ;Have a character, so loop and get it
        (AND NO-HANG-P (RETURN NIL))
        (SETQ INHIBIT-SCHEDULING-FLAG NIL)
        (PROCESS-WAIT WHOSTATE #'(LAMBDA (BUFFER)
                                   (OR (NOT (IO-BUFFER-EMPTY-P BUFFER))
                                       (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
                                            (NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER)))))
                      BUFFER)))))

(DEFUN KBD-WAIT-FOR-INPUT-WITH-TIMEOUT (BUFFER TIMEOUT &OPTIONAL (WHOSTATE "Keyboard"))
  "Wait until there is input available in BUFFER or the keyboard, or until TIMEOUT.
The keyboard is checked for input only if this buffer is the selected one.
TIMEOUT is in units of 60'ths of a second, like (TIME).
WHOSTATE appears in the who line while we wait."
  (PROCESS-WAIT WHOSTATE #'(LAMBDA (BUFFER START-TIME INTERVAL)
                             (OR ( (TIME-DIFFERENCE (TIME) START-TIME)
                                    INTERVAL)
                                 (NOT (IO-BUFFER-EMPTY-P BUFFER))
                                 (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
                                      (NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER)))))
                BUFFER
                (TIME) TIMEOUT))

(DEFUN KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE (BUFFER WINDOW &OPTIONAL (WHOSTATE "Keyboard"))
  "Wait until BUFFER or the keyboard has input, or WINDOW is not exposed.
The keyboard is checked for input only if this buffer is the selected one.
WHOSTATE appears in the who line while we are waiting."
  (PROCESS-WAIT WHOSTATE #'(LAMBDA (BUFFER WINDOW)
                             (OR (NOT (SEND WINDOW :EXPOSED-P))
                                 (NOT (IO-BUFFER-EMPTY-P BUFFER))
                                 (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
                                      (NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER)))))
                BUFFER WINDOW))

(DEFUN KBD-SNARF-INPUT (BUFFER &OPTIONAL NO-HARDWARE-CHARS-P)
  "Transfer any input that the keyboard has into BUFFER.
The keyboard is checked for input only if this buffer is the selected one."
  (WITHOUT-INTERRUPTS
    (COND ((NULL BUFFER))                       ;This can happen due to timing error
          ((EQ BUFFER (KBD-GET-IO-BUFFER))
           ;; There is potentially input for us
           (OR NO-HARDWARE-CHARS-P (KBD-PROCESS-MAIN-LOOP-INTERNAL))
           (DO ((OK)
                (ELT))
               ((IO-BUFFER-EMPTY-P KBD-IO-BUFFER))
             (MULTIPLE-VALUE (ELT OK)
               (IO-BUFFER-GET KBD-IO-BUFFER T))
             (OR OK (RETURN NIL))               ;Some ignored characters, we are done
             (AND ELT (IO-BUFFER-PUT BUFFER ELT T)))))))

(DEFVAR KBD-TYI-HOOK NIL)  ;This is a crock, but I suppose someone might want to...
(DEFCONST KBD-STANDARD-INTERCEPTED-CHARACTERS
          '((#/ABORT KBD-INTERCEPT-ABORT)
            (#/M-ABORT KBD-INTERCEPT-ABORT-ALL)
            (#/BREAK KBD-INTERCEPT-BREAK)
            (#/M-BREAK KBD-INTERCEPT-ERROR-BREAK)))
(DEFVAR KBD-INTERCEPTED-CHARACTERS KBD-STANDARD-INTERCEPTED-CHARACTERS
  "List of characters to be processed by the low levels of reading keyboard input.
Each element is a list (character function).  The function is called with
the character as its argument, and should return two values:
the character (or another character, to translate it), and
a flag saying whether to ignore the character.")
(ADD-INITIALIZATION "Don't Ignore Abort"
                    '(SETQ KBD-INTERCEPTED-CHARACTERS KBD-STANDARD-INTERCEPTED-CHARACTERS
                           COLD-LOAD-STREAM-OWNS-KEYBOARD NIL)
                    '(SYSTEM))

(DEFUN KBD-DEFAULT-OUTPUT-FUNCTION (IGNORE CHAR)
  "System standard IO-BUFFER output function.
Intercepts those characters in KBD-INTERCEPTED-CHARACTERS.
Must be called with INHIBIT-SCHEDULING-FLAG bound to T, and this may SETQ it to NIL."
  (IF (AND KBD-TYI-HOOK (FUNCALL KBD-TYI-HOOK CHAR))
      (VALUES CHAR T)
      ;; Note, this must not use =, since the character may not be a number
    (LET ((TEM (ASSQ CHAR TV:KBD-INTERCEPTED-CHARACTERS)))
      (IF TEM (FUNCALL (CADR TEM) CHAR)
        CHAR))))

(DEFUN KBD-INTERCEPT-ABORT (CHAR &REST IGNORE)
  "Perform the action normally associated with the Abort character.
This function is intended to be called from IO-BUFFER output functions."
  CHAR
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (OR (AND (TYPEP *TERMINAL-IO* 'SHEET)         ;Kludge to avoid being unable to abort
           (SHEET-OUTPUT-HELD-P *TERMINAL-IO*))
      (SEND *TERMINAL-IO* :SEND-IF-HANDLES :INHIBIT-OUTPUT-FOR-ABORT-P)
      (PROGN
        (SEND *TERMINAL-IO* :CLEAR-REST-OF-LINE)
        (SEND *TERMINAL-IO* :STRING-OUT "[Abort]")))
  (SIGNAL-CONDITION EH:ABORT-OBJECT))

(DEFUN KBD-INTERCEPT-ABORT-ALL (CHAR &REST IGNORE)
  "Perform the action normally associated with the Meta-Abort character.
This function is intended to be called from IO-BUFFER output functions."
  CHAR
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (OR (AND (TYPEP *TERMINAL-IO* 'SHEET)         ;Kludge to avoid being unable to abort
           (SHEET-OUTPUT-HELD-P *TERMINAL-IO*))
      (SEND *TERMINAL-IO* :SEND-IF-HANDLES :INHIBIT-OUTPUT-FOR-ABORT-P)
      (PROGN
        (SEND *TERMINAL-IO* :CLEAR-REST-OF-LINE)
        (SEND *TERMINAL-IO* :STRING-OUT "[Abort all]")))
  (SEND CURRENT-PROCESS :RESET :ALWAYS))

(DEFUN KBD-INTERCEPT-BREAK (CHAR &REST IGNORE)
  "Perform the action normally associated with the Break character.
This function is intended to be called from IO-BUFFER output functions."
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (BREAK "BREAK")
  (VALUES CHAR T))

(DEFUN KBD-INTERCEPT-ERROR-BREAK (CHAR &REST IGNORE)
  "Perform the action normally associated with the Meta-Break character.
This function is intended to be called from IO-BUFFER output functions."
  (declare (dbg:error-reporter))
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (MULTIPLE-VALUE-BIND (BUFFER POSITION)
      (SEND *STANDARD-INPUT* :SEND-IF-HANDLES :SAVE-RUBOUT-HANDLER-BUFFER)
    (UNWIND-PROTECT
        (SIGNAL-CONDITION
          (MAKE-CONDITION 'BREAK :FORMAT-STRING "Keyboard break.")
          '(:NO-ACTION) T)
      (UNLESS (EQ *DEBUG-IO* COLD-LOAD-STREAM)
        (IF BUFFER (SEND *STANDARD-INPUT* :RESTORE-RUBOUT-HANDLER-BUFFER BUFFER POSITION)))))
  (VALUES CHAR T))

;;; This function is called, possibly in the keyboard process, when one of the
;;; standard asynchronous intercepted characters, of the sort that mungs over the
;;; process, is typed.  Scheduling is inhibited.
;;; This does the actual munging of the process in a separate process, in case
;;; it has to wait for the process' stack-group to get out of some weird state.
(DEFUN KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER (CHAR WINDOW &OPTIONAL PROCESS-RUN-OPTIONS
                                             FUNCTION &AUX P)
  (KBD-ESC-CLEAR NIL)  ;Forget chars typed before "CTRL-abort", even those inside window's iob
  (IF (EQ FUNCTION 'KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER)
      (SETQ FUNCTION NIL))
  (AND WINDOW                   ;Find process to be hacked
       (SETQ P (SEND WINDOW :PROCESS))
       (IF FUNCTION (PROCESS-RUN-FUNCTION PROCESS-RUN-OPTIONS
                                          P :INTERRUPT FUNCTION CHAR WINDOW)
         (SELECTQ CHAR
           ((#/C-ABORT #/C-M-ABORT)
            (PROCESS-RUN-FUNCTION '(:NAME "Abort" :PRIORITY 50.) P :INTERRUPT
                                  (SELECTQ CHAR
                                    (#/C-ABORT #'KBD-INTERCEPT-ABORT)
                                    (#/C-M-ABORT #'KBD-INTERCEPT-ABORT-ALL))
                                  (DPB 0 %%KBD-CONTROL CHAR)))
           (#/C-BREAK
            (PROCESS-RUN-FUNCTION '(:NAME "Break" :PRIORITY 40.) P :INTERRUPT 'BREAK 'BREAK))
           (#/C-M-BREAK
            (PROCESS-RUN-FUNCTION '(:NAME "Break" :PRIORITY 40.)
                                  P :INTERRUPT %ERROR-HANDLER-STACK-GROUP
                                  '(BREAK)))))))

(DEFUN KBD-GET-SOFTWARE-CHAR (&OPTIONAL (WHOSTATE "Keyboard"))
  "Returns the next char from the hardware converted to software codes.  This
is meant to be used only by things that run in the keyboard process, and not by
any user code."
  (DO ((CH)) (NIL)
    (PROCESS-WAIT WHOSTATE #'KBD-HARDWARE-CHAR-AVAILABLE)
    (AND (SETQ CH (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR)))
         (RETURN CH))))

(DEFUN KBD-CHAR-TYPED-P (&AUX (BUFFER (KBD-GET-IO-BUFFER)))
  "Kludge to return T when a character has been typed.  First checks the selected window's
IO buffer, and if it is empty then checks the microcode's buffer.  This is useful for
programs which want to stop when a character is typed, but don't want to allow
interrupts and scheduling."
  (OR (AND BUFFER (NOT (IO-BUFFER-EMPTY-P BUFFER)))
      (KBD-HARDWARE-CHAR-AVAILABLE)))

(DEFUN KBD-CLEAR-IO-BUFFER ()
  "Clear the keyboard buffer and the hardware buffer"
  (IO-BUFFER-CLEAR KBD-IO-BUFFER)
  (DO ((CH))
      ((NULL (SETQ CH (KBD-GET-HARDWARE-CHAR))))
    ;; Call this to process shifts
    (KBD-CONVERT-TO-SOFTWARE-CHAR CH)))

(DEFUN KBD-CLEAR-SELECTED-IO-BUFFER ()
  "Flush the selected io buffer"
  (SETQ SELECTED-IO-BUFFER NIL))

(DEFUN KBD-GET-IO-BUFFER ()
  "Returns the selected IO buffer -- the one that is allowed to read from the keyboard.
If there is no current buffer, the selected window is interrogated.
If there is no selected window, or the window has no buffer, returns NIL."
  (COND ((NULL SELECTED-WINDOW)
         ;; This shouldn't be necessary, but try not to lose too big
         (KBD-CLEAR-SELECTED-IO-BUFFER))
        (SELECTED-IO-BUFFER SELECTED-IO-BUFFER)
        (T (PROG1 (SETQ SELECTED-IO-BUFFER (SEND SELECTED-WINDOW :IO-BUFFER))
                  (WHO-LINE-RUN-STATE-UPDATE)))))       ;May have just switched processes

;(DEFUN KBD-CALL (BUFFER)
;  "Handle the CALL character."
;  BUFFER                                       ;Not used
;  (IO-BUFFER-CLEAR KBD-IO-BUFFER)              ;Forget chars typed before "call"
;  (PROCESS-RUN-FUNCTION "Call" #'(LAMBDA (WINDOW)
;                                  (IF WINDOW
;                                      (SEND WINDOW :CALL)
;                                    (SETQ WINDOW (KBD-DEFAULT-CALL-WINDOW))
;                                    (SEND WINDOW :MOUSE-SELECT)))
;                       SELECTED-WINDOW))

;; someone commented this out... I'm putting it back for
;; (:method tv:select-mixin :call) -dg

(DEFUN KBD-DEFAULT-CALL-WINDOW (&OPTIONAL (SCREEN DEFAULT-SCREEN) &AUX PREVIOUS-WINDOW)
  "Return a suitable window for the CALL character to select."
  (IF (AND (SETQ PREVIOUS-WINDOW (AREF PREVIOUSLY-SELECTED-WINDOWS 0))
           (EQ (SEND PREVIOUS-WINDOW :LISP-LISTENER-P) :IDLE))
      ;; CALL should always get a Lisp Listener, but try to be smart about
      ;; the one that it really gets
      PREVIOUS-WINDOW
    (SEND SCREEN :IDLE-LISP-LISTENER)))

(DEFUN KEY-STATE (KEY &AUX TEM)
  "T if the specified key is now depressed.
KEY is the name of a shift key, or the character for a non-shift key.
Names allowed are :SHIFT, :GREEK, :TOP, :CONTROL, :META, :SUPER, :HYPER,
or any of those with LEFT- or RIGHT, as in :LEFT-SHIFT,
and four other names that do not allow LEFT- or RIGHT- :
:REPEAT, :ALT-LOCK, :MODE-LOCK and :SHIFT-LOCK.
:SHIFT is T if either shift key is depressed;
 :LEFT-SHIFT and :RIGHT-SHIFT test one individual shift key."
  (KBD-PROCESS-MAIN-LOOP-INTERNAL)
  (COND ((NUMBERP KEY) (NOT (ZEROP (AREF SI::KBD-KEY-STATE-ARRAY KEY))))
        ((SETQ TEM (ASSQ KEY '#o((:SHIFT 100) (:LEFT-SHIFT 0) (:RIGHT-SHIFT 40)
                                 (:GREEK 101) (:LEFT-GREEK 1) (:RIGHT-GREEK 41)
                                 (:TOP 102) (:LEFT-TOP 2) (:RIGHT-TOP 42)
                                 (:CONTROL 104) (:LEFT-CONTROL 4) (:RIGHT-CONTROL 44)
                                 (:META 105) (:LEFT-META 5) (:RIGHT-META 45)
                                 (:SUPER 106) (:LEFT-SUPER 6) (:RIGHT-SUPER 46)
                                 (:HYPER 107) (:LEFT-HYPER 7) (:RIGHT-HYPER 47)
                                 (:CAPS-LOCK 3) (:ALT-LOCK 10) (:MODE-LOCK 11)
                                 (:REPEAT 12))))
         (BIT-TEST (LSH 1 (LOGAND (SETQ TEM (CADR TEM)) #o37))
                   (COND ((< TEM #o40) SI::KBD-LEFT-SHIFTS)
                         ((< TEM #o100) SI::KBD-RIGHT-SHIFTS)
                         (T (LOGIOR SI::KBD-LEFT-SHIFTS SI::KBD-RIGHT-SHIFTS)))))
        (T (FERROR "~S illegal key; must be character or symbol for shift key" KEY))))

;;;; "Escape key" (ie #/terminal)

;;; Unknown or misspelled keywords are ignored.
(DEFVAR *ESCAPE-KEYS*
     '((#/CLEAR KBD-ESC-CLEAR "Discard type-ahead" :KEYBOARD-PROCESS)
       (#/RESUME (KBD-ESC-RESUME)
        "Allow deexposed typeout in window that Terminal-0-S would select.")
       (#/ SYSTEM-MENU-SET-MOUSE-SCREEN "Set Mouse screen")
       (#/FORM (KBD-SCREEN-REDISPLAY)
        "Clear and redisplay all windows (Page = Clear Screen)")
       (#/A KBD-ESC-ARREST
        "Arrest process in who-line (minus means unarrest)" :KEYBOARD-PROCESS)
       (#/B KBD-BURY
        "Bury the selected window" :TYPEAHEAD)
       (#/C KBD-COMPLEMENT
        '("Complement video black-on-white state"
          "With an argument, complement the who-line documentation window")
        :KEYBOARD-PROCESS)
;      (#/D (SI:BUZZ-DOOR) (AND (SI:TECH-SQUARE-FLOOR-P 9) "Open the door"))
;      (#/E (SI:CALL-ELEVATOR) (AND (OR (SI:TECH-SQUARE-FLOOR-P 8)
;                                       (SI:TECH-SQUARE-FLOOR-P 9))
;                                   "Call the elevator"))
       (#/F KBD-FINGER (FINGER-ARG-PROMPT)
        :TYPEAHEAD)
       (#/G (KBD-GC-STATUS) "Show the current state of all garbage collection.")
       (#/H (KBD-HOSTAT) "Show status of CHAOSnet hosts" :TYPEAHEAD)
       (#/I KBD-ESC-I
        "Selected window deexposed input notify flag (complement, or arg=1 on, 0 off)")
       (#/M KBD-ESC-MORE "Selected window **MORE** enable (complement, or arg=1 on, 0 off, 3 on with timeout)"
        :KEYBOARD-PROCESS)
       (#/N KBD-ESC-NOTIFICATIONS "Allow notifications to come out."
        "Terminal 1 N  print all notifications (even old ones)"
        "Terminal 2 N  defer notifications, reset who-line"
        :TYPEAHEAD)
       (#/O KBD-OTHER-EXPOSED-WINDOW "Select another exposed window" :TYPEAHEAD)
       (#/Q KBD-ESC-Q
        (LET ((PRINTER (OR SI:*DEFAULT-BIT-ARRAY-PRINTER* SI:*DEFAULT-PRINTER*)))
          (IF (STRINGP PRINTER) (SETQ PRINTER (SI:EXPAND-PRINTER-NAME PRINTER)))
          (AND (GET (IF (CONSP PRINTER) (CAR PRINTER) PRINTER)
                    'SI:PRINT-BIT-ARRAY)
               (FORMAT NIL "Hardcopy the screen on the ~A.  With an argument of 1, hardcopies only
the current window; with an argument of 3, does not include screen borders."
                       (IF (CONSP PRINTER)
                           (CAR PRINTER)
                         PRINTER)))))
       (#/S KBD-SWITCH-WINDOWS
        '("Select the most recently selected window.  With an argument, select the nth"
          "previously selected window and rotate the top n windows.  (Default arg is 2)."
          "With an arg of 1, rotate through all the windows."
          "With a negative arg rotate in the other direction."
          "With an argument of 0, select a window that wants to type out.")
        :TYPEAHEAD)
       (#/T KBD-ESC-T
        "Selected window deexposed typeout action.  0 - wait, 1 - notify, 2 - permit.")
       (#/V KBD-VIEW-MAIL "View new mail. Terminal 1 V - view any file."
        :TYPEAHEAD)
       (#/W KBD-ESC-W
        '("Switch which process the wholine looks at.  Default is just to refresh it"
          " 1 means selected-window's process, 2 means freeze on this process,"
          " 3 means rotate among all processes, 4 means rotate other direction,"
          " 0 gives a menu of all processes"))
       (#/HOLD-OUTPUT KBD-ESC-OUTPUT-HOLD "Expose window on which we have /"Output Hold/"")
       (#/? KBD-ESC-HELP NIL :TYPEAHEAD)
       (#/HELP KBD-ESC-HELP NIL :TYPEAHEAD)
       (NIL) ;Ones after here are "for wizards"
       (#/CALL (KBD-USE-COLD-LOAD-STREAM) "Get to cold-load stream" :TYPEAHEAD)
       (#/C-T KBD-CLEAR-TEMPORARY-WINDOWS "Flush temporary windows")
       (#/C-CLEAR KBD-CLEAR-LOCKS "Clear window-system locks")
       (#/C-A KBD-ESC-ARREST-ALL "Arrest nearly all processes" :KEYBOARD-PROCESS))
  "Determines what to do with characters typed after the Terminal character.
A list of elements (CHAR FUNCTION DOCUMENTATION . OPTIONS).
CHAR is what character this element applies to.
If FUNCTION is a list, it is evaluated; otherwise, it is called with one arg,
 which is either NIL or the numeric arg (1 in Terminal 1 F).
The evaluation or calling is normally done in a separate process.
DOCUMENTATION can be a string, a function that returns a string, or NIL.
 NIL means the this entry will not appear in the help message.
 It can also be a list of strings that go on separate lines.
OPTIONS are keywords (with no values).  Defined options are:
    :TYPEAHEAD - copy the contents of the
        software buffer into the currently selected IO-BUFFER.  This has the
        effect of treating everything typed before the TERMINAL as typeahead to
        the currently selected window.  Useful for TERMINAL commands that
        change the selected window.  These commands should set KBD-ESC-TIME to NIL
        as soon as they change the selected window, unless they complete quickly
        (input should never be done with KBD-ESC-TIME non-NIL).
    :KEYBOARD-PROCESS - run the function in the keyboard process instead of starting
        a new process for it.")

(DEFUN REMOVE-ESCAPE-KEY (CHAR &REST IGNORE)
  "Remove the character CHAR from the list of terminal- keys."
  (if (characterp char) (setq char (char-int char)))
  (SETQ *ESCAPE-KEYS* (DELQ (ASSQ (CHAR-UPCASE CHAR) *ESCAPE-KEYS*) *ESCAPE-KEYS*)))


(DEFVAR *USER-DEFINED-ESCAPE-KEYS* NIL
  "List of escape keys that the user has added, so the user won't lose his definitions.")

(DEFUN ADD-ESCAPE-KEY (CHAR FUNCTION &OPTIONAL DOCUMENTATION &REST OPTIONS
                       &AUX C ENTRY BEFORE DURING AFTER (STATE :BEFORE))
  "Add CHAR to the list of actions to be performed and are prefaced by typing TERMINAL.
FUNCTION should be the function to be called when that key is depressed,
DOCUMENTATION is what to show up when the user types Terminal Help.
OPTIONS can include either :TYPEAHEAD or :KEYBOARD-PROCESS,
 or :SYSTEM meaning this is a redefinition of system code
 rather than a user overriding the system code."
  (CHECK-TYPE DOCUMENTATION STRING "a valid documentation string")
  (if (characterp char) (setq char (char-int char)))
  (SETQ ENTRY (LIST* CHAR FUNCTION DOCUMENTATION (COPY-LIST OPTIONS)))  ;what to store
  (UNLESS (MEMQ ':SYSTEM OPTIONS)
    (PUSH ENTRY *USER-DEFINED-ESCAPE-KEYS*))
  ;;remove character from list.
  (REMOVE-ESCAPE-KEY CHAR)
  ;;logic: before means we haven't found the alphabetics yet, during means we're hacking them
  ;;       now and after means that we are hacking the post alphabetics.
  ;; we also invert the order, so we are really hacking the ones at the end first.
  (DOLIST (ITEM (NREVERSE *ESCAPE-KEYS*))
    (SETQ C (CAR ITEM))
    (AND (EQ STATE ':DURING) (NOT (NULL C)) (NOT (ALPHA-CHAR-P C))
         (SETQ STATE ':AFTER))
    (AND (EQ STATE ':BEFORE) (NOT (NULL C)) (ALPHA-CHAR-P C)
         (SETQ STATE ':DURING))
    (CASE STATE
      (:AFTER
       (PUSH ITEM AFTER))
      (:DURING
       (PUSH ITEM DURING))
      (:BEFORE
       (PUSH ITEM BEFORE))))
  ;; We're all done; now where does that key go?
  (IF (ALPHA-CHAR-P CHAR)
      (PUSH ENTRY DURING)
      (PUSH ENTRY BEFORE))
  (SETQ *ESCAPE-KEYS*                           ;alphabatize
        (APPEND AFTER (SORT DURING #' :KEY #'CAR) BEFORE))
  NIL)

;;; when you patch sys code, be sure to call this or call add-escape-key with 2nd arg of T.
;;; Put on initialization list ?
(DEFUN REDO-USER-ESCAPE-MODIFICATIONS ()
  "Make sure the user gets the changes he wants on the *ESCAPE-KEYS*"
  (IF (NOT (NULL *USER-DEFINED-ESCAPE-KEYS*))
      (DOLIST (ENTRY *USER-DEFINED-ESCAPE-KEYS*)
        (APPLY #'ADD-ESCAPE-KEY ENTRY))))

(DEFUN KBD-ESC-RESUME ()
  "Handle a terminal-resume typed on the keyboard by allowing interesting window to type out."
  (LET ((W (FIND-INTERESTING-WINDOW)))
    (COND (W
           (SEND W :SET-DEEXPOSED-TYPEOUT-ACTION :PERMIT)
           (SETF (TV:SHEET-OUTPUT-HOLD-FLAG W) 0))
          (T
           (BEEP)))))

(DEFVAR *SAVED-COLD-BOOTED-P* NIL
  "Used to tell if we should consider ourselves to be cold booted.")

(DEFVAR *SAVED-LAST-ACTIVITY-TIME* 0. ;;a very long time ago indeed
  "Used to record how long we were idle at some point back in time.")

(DEFUN SAVE-IDLE-INFO ()
  "Save away the idle info that is currently valid."
  (SETQ *SAVED-COLD-BOOTED-P* SI::WHO-LINE-JUST-COLD-BOOTED-P
        *SAVED-LAST-ACTIVITY-TIME* TV:KBD-LAST-ACTIVITY-TIME))

(DEFUN RESTORE-IDLE-INFO ()
  "Restore the idle info that was saved away earlier."
  (AND (> *SAVED-LAST-ACTIVITY-TIME* 0) ;;this means we never called save-idle-info
       (SETQ *SAVED-COLD-BOOTED-P* SI::WHO-LINE-JUST-COLD-BOOTED-P
             *SAVED-LAST-ACTIVITY-TIME* TV:KBD-LAST-ACTIVITY-TIME)))


;;; The keyboard process wants to be permanent, to have medium-high priority, and
;;; to restart sooner than other processes during booting.
(DEFVAR KBD-PROCESS (PROCESS-RUN-FUNCTION '(:NAME "Keyboard" :PRIORITY 40.
                                            :RESTART-AFTER-RESET T
                                            :WARM-BOOT-ACTION SI:PROCESS-WARM-BOOT-RESTART)
                                          'KBD-PROCESS-MAIN-LOOP))

(DEFUN KBD-ESC (&REST IGNORE &AUX CH ARG MINUS FCN ENT TEM)
  "Handle Terminal typed on keyboard"
  (SAVE-IDLE-INFO) ;;but something was probably already clobbered, sigh
  (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
    (WHO-LINE-RUN-STATE-UPDATE)                 ;Necessary to make above take effect
    (DO-FOREVER
      (SETQ CH (CHAR-UPCASE (KBD-GET-SOFTWARE-CHAR "Terminal-")))
      (COND (( #/0 CH #/9)
             (SETQ ARG (+ (* (OR ARG 0) 10.) (- CH #/0))))
            ((= CH #/-) (SETQ MINUS T))
            (T (RETURN)))))
  (WHO-LINE-RUN-STATE-UPDATE)                   ;Switch LAST-WHO-LINE-PROCESS back
  (AND MINUS (SETQ ARG (MINUS (OR ARG 1))))
  (COND ((SETQ ENT (ASSQ CH *ESCAPE-KEYS*))
         (WITHOUT-INTERRUPTS
           (WHEN (MEMQ ':TYPEAHEAD (CDDDR ENT))
             (KBD-GET-IO-BUFFER)
             (KBD-SNARF-INPUT SELECTED-IO-BUFFER T)
             (SETQ KBD-ESC-TIME (TIME))))
         (SETQ FCN (SECOND ENT))
         (IF (CONSP FCN) (SETQ ARG FCN FCN #'EVAL))
         (COND ((MEMQ ':KEYBOARD-PROCESS (CDDDR ENT))
                (FUNCALL FCN ARG)
                (SETQ KBD-ESC-TIME NIL))
               (T (PROCESS-RUN-FUNCTION (FORMAT NIL "Handle Terminal-~:C" CH)
                                        #'(LAMBDA (FCN ARG)
                                            (LET ((KBD-LAST-ACTIVITY-TIME
                                                    KBD-LAST-ACTIVITY-TIME))
                                              (FUNCALL FCN ARG))
                                            (SETQ KBD-ESC-TIME NIL)
                                            (RESTORE-IDLE-INFO))
                                        FCN ARG))))
        ;; quote asynchronous characters
        ((OR (AND (SETQ TEM (KBD-GET-IO-BUFFER))
                  (ASSQ CH (GETF (IO-BUFFER-PLIST TEM)
                                 ':ASYNCHRONOUS-CHARACTERS
                                 KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)))
             (ASSQ CH KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS))
         (UNLESS (IO-BUFFER-FULL-P KBD-IO-BUFFER)
           (IO-BUFFER-PUT KBD-IO-BUFFER CH)))
        ;((SETQ TEM (ASSQ (DPB 1 %%KBD-CONTROL (LDB %%KBD-CHAR CH))
        ;           KBD-STANDARD-ASYNCHRONOUS-CHARACTERS))
        ; (APPLY (CADR TEM) (CAR TEM) TV:SELECTED-WINDOW (CDDR TEM)))
        ((NEQ CH #/RUBOUT) (BEEP)))
  ;; No unwind-protect needed -- this is for good measure
  (RESTORE-IDLE-INFO))

(DEFUN KBD-COMPLEMENT (ARG)                     ;terminal C
  (IF ARG
      (SEND WHO-LINE-DOCUMENTATION-WINDOW :SET-REVERSE-VIDEO-P
            (NOT (SEND WHO-LINE-DOCUMENTATION-WINDOW :REVERSE-VIDEO-P)))
    (COMPLEMENT-BOW-MODE)))

(DEFUN KBD-ESC-MORE (ARG)                       ;terminal M
  (COND (SELECTED-WINDOW
         (let ((new-more
                 (COND ((NULL ARG) (NOT (SEND SELECTED-WINDOW :MORE-P)))
                        ((bit-test 1 ARG) T)            ;terminal <odd> M:  more proc on
                        (T NIL))))                      ;terminal <even> M: more proc off
           (SEND SELECTED-WINDOW :SET-MORE-P new-more)
           (if arg (send selected-window :set-time-out-on-more (bit-test 2 arg)))))))

(DEFUN KBD-ESC-CLEAR (TEM &OPTIONAL IGNORE)     ;terminal clear-input / c-clear-input
  (AND (SETQ TEM (KBD-GET-IO-BUFFER))
       (IO-BUFFER-CLEAR TEM))
  (IO-BUFFER-CLEAR KBD-IO-BUFFER))

(DEFUN KBD-ESC-ARREST (ARG &AUX P)
  (COND ((NULL (SETQ P LAST-WHO-LINE-PROCESS)) (BEEP))
        ((AND ARG (MINUSP ARG))
         (DOLIST (R (SEND P :ARREST-REASONS))
           (SEND P :REVOKE-ARREST-REASON R)))
        (T (SEND P :ARREST-REASON :USER))))

(DEFUN KBD-ESC-ARREST-ALL (ARG)
  (DOLIST (P ALL-PROCESSES)
    (OR (NULL P)
        (EQ P KBD-PROCESS)
        (EQ P MOUSE-PROCESS)
        (EQ P SCREEN-MANAGER-BACKGROUND-PROCESS)
        (EQ P LAST-WHO-LINE-PROCESS)
        (SEND P (IF (AND ARG (MINUSP ARG)) :REVOKE-ARREST-REASON :ARREST-REASON)
              :USER))))

(DEFUN KBD-BURY (ARG)                           ;terminal B
  ARG ;unused for now
  (IF SELECTED-WINDOW
      (SEND (SEND SELECTED-WINDOW :ALIAS-FOR-SELECTED-WINDOWS) :BURY))
  (SETQ KBD-ESC-TIME NIL))

(DEFUN KBD-OTHER-EXPOSED-WINDOW (IGNORE)        ;terminal O
  ;; terminal O selects the least recently-selected window that is exposed.
  ;; Thus repeated terminal O cycles among all the selectable exposed windows
  ;; on all the screens.  Real useful with split-screen!
  (DO ((I 0 (1+ I))
       (N (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
       (TEM)
       (WINDOW NIL))
      (( I N)
       (IF WINDOW (SEND WINDOW :MOUSE-SELECT)
           (BEEP)))
    (AND (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I))
         (EQ (SEND TEM :STATUS) ':EXPOSED)
         (NOT (NULL (SEND TEM :NAME-FOR-SELECTION)))
         (SETQ WINDOW TEM))))

(DEFUN KBD-SWITCH-WINDOWS (ARG &AUX TEM)        ;terminal S
  ;; terminal n S rotates the n most recently selected windows, selecting the nth
  ;; terminal S = terminal 2 S
  ;; terminal 1 S selects the next most recent window but rotates all the windows
  ;; terminal -n S rotates the same set of windows in the other direction
  ;; terminal 0 S selects a window which has an error pending (or otherwise wants attention)
  (OR ARG (SETQ ARG 2))
  (COND ((= ARG 0) (SELECT-INTERESTING-WINDOW))
        (T (DELAYING-SCREEN-MANAGEMENT          ;Inhibit auto-selection
             (COND ((SETQ TEM SELECTED-WINDOW)  ;Put current window on front of array
                    (SEND TEM :DESELECT NIL)
                    (AND (SETQ TEM (SEND TEM :IO-BUFFER))
                         (KBD-SNARF-INPUT TEM T))))
             (WITHOUT-INTERRUPTS                ;Get rid of any non-mouse-selectable ones
               (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
                 (OR (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I)) (RETURN))
                 (COND ((NOT (SEND TEM :NAME-FOR-SELECTION))
                        (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS TEM)
                        (SETQ I (1- I)))))
               (ROTATE-TOP-OF-ARRAY PREVIOUSLY-SELECTED-WINDOWS ARG))
             (AND (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS 0))
                  (SEND TEM :MOUSE-SELECT))))))

#|
(DEFUN KBD-STATUS (IGNORE &AUX W TEM)
  "Display what the effects of typing terminal-N-s would be."
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (SEND WINDOW :SET-LABEL "Information on recently selected windows.")
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
    (WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW))
        (SETQ KBD-ESC-TIME NIL)
        (FORMAT WINDOW "To select one of the windows below, type  <number> S~&")
        (DO ((I 1 (1+ I)))
            ((= I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS)))
          (OR (SETQ W (AREF PREVIOUSLY-SELECTED-WINDOWS I)) (RETURN))
          (IF (SETQ TEM (SEND W :NAME-FOR-SELECTION))
              (FORMAT WINDOW "~2D: ~A~&" (1+ I) TEM)
            (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS W)
            (SETQ I (1- I))))
        (IF (NULL (SETQ W (FIND-INTERESTING-WINDOW)))
            (FORMAT WINDOW "~%[There is currently no /"interesting/" window]~&")
          (FORMAT WINDOW
                  "~%~A is an /"interesting/" window~%   Type -0-S to select it.~&"
                  (SEND W :NAME-FOR-SELECTION))
          (LET (WW)
            (DOLIST (P ACTIVE-PROCESSES)
              (WHEN (AND (SETQ P (CAR P))
                         (TYPEP (PROCESS-STACK-GROUP P) 'STACK-GROUP)
                         (NEQ (SETQ WW (SI::PROCESS-IS-IN-ERROR-P P)) W))
                (FORMAT WINDOW
                        "Process ~A is in error,
     and is awaiting selection of window ~A for typeout" (PROCESS-NAME P) WW))))
          (DOLIST (WW BACKGROUND-INTERESTING-WINDOWS)
            (WHEN (NEQ (CAR WW) W)
              (FORMAT WINDOW "Window ~A is awaiting selection"))))
        (SETQ W (AREF PREVIOUSLY-SELECTED-WINDOWS 0))
        (IF (NULL W)
            (FORMAT WINDOW "~2&There is currently no window selected.")
          (FORMAT WINDOW "~2&The currently selected window is ~:[~S~;~:*~A~]"
                  (SEND W :NAME-FOR-SELECTION) W))
        (FORMAT WINDOW "~2%Type a space to flush: ")
        (PROCESS-WAIT "Keyboard" #'(LAMBDA (WINDOW)
                                     (NEQ SELECTED-WINDOW WINDOW)
                                     (SEND WINDOW :LISTEN))
                      WINDOW)
        (IF (EQ WINDOW SELECTED-WINDOW) (SEND WINDOW :TYI-NO-HANG))))))
|#

;;; This is like ZWEI:ROTATE-TOP-OF-LIST but for a NIL-padded array
;;; Rotate nth (1-origin!) element to the front of the array, rotating the
;;; part of the array before it.  With a negative arg rotate the same amount
;;; backwards.  With an arg of 1 rotate the whole array BACKWARDS, i.e. bring
;;; up the same element as with an arg of 2 but store the old front at the back.
;;; Zero arg is undefined, do nothing I guess.  Note that 2 and -2 do the same thing.
;;; Doesn't barf if N is too big.
(DEFUN ROTATE-TOP-OF-ARRAY (ARRAY N &AUX (LENGTH (ARRAY-LENGTH ARRAY)))
  (DO () ((ZEROP LENGTH))
    (AND (AREF ARRAY (1- LENGTH)) (RETURN))
    (SETQ LENGTH (1- LENGTH)))
  (AND (= (ABS N) 1) (SETQ N (* N -1 LENGTH)))
  (COND ((PLUSP N)
         (SETQ N (MIN LENGTH N))
         (DO ((I 0 (1+ I))
              (NTH (AREF ARRAY (1- N)) OLD)
              (OLD))
             (( I N))
           (SETQ OLD (AREF ARRAY I))
           (ASET NTH ARRAY I)))
        ((MINUSP N)
         (SETQ N (MIN LENGTH (MINUS N)))
         (DO ((I 1 (1+ I))
              (FRONT (AREF ARRAY 0)))
             (( I N) (ASET FRONT ARRAY (1- I)))
           (ASET (AREF ARRAY I) ARRAY (1- I)))))
  ARRAY)

(DEFUN KBD-SCREEN-REDISPLAY ()
  "Like SCREEN-REDISPLAY, but goes over windows by hand, and never waits for a lock."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (WHEN (SHEET-EXPOSED-P SCREEN)
      (DOLIST (I (SHEET-EXPOSED-INFERIORS SCREEN))
        (AND (SHEET-CAN-GET-LOCK I)
             (SEND I :REFRESH)))
      (SEND SCREEN :SCREEN-MANAGE)))
  (WHO-LINE-CLOBBERED))

(DEFUN KBD-CLEAR-LOCKS (IGNORE)                 ;terminal c-clear
  (KBD-CLEAR-TEMPORARY-WINDOWS NIL)             ;First flush any temporary windows
  (SHEET-CLEAR-LOCKS))

(DEFUN KBD-CLEAR-TEMPORARY-WINDOWS (IGNORE)     ;terminal c-T
  (MAP-OVER-SHEETS (LAMBDA (SHEET)
                     (AND (SHEET-TEMPORARY-P SHEET)
                          (SHEET-EXPOSED-P SHEET)
                          (SHEET-CAN-GET-LOCK SHEET)
                          (CATCH-ERROR (SEND SHEET :DEEXPOSE) NIL)))))

(DEFUN KBD-USE-COLD-LOAD-STREAM ()
  (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
    (BLOCK TOP
      (EH::SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
      (DOLIST (W LOCKED-ERROR-WINDOWS)
        (LET ((*QUERY-IO* COLD-LOAD-STREAM))
          (WHEN (FQUERY NIL "Handle error in ~S in cold load stream?" W)
            ;; Deleting the window from the list wakes up the process that
            ;; is waiting for an unlocked window.
            (EH::RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)
            (RETURN-FROM TOP (SETQ LOCKED-ERROR-WINDOWS (DELQ W LOCKED-ERROR-WINDOWS))))))
      (UNWIND-PROTECT
          (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit from the cold-load-stream breakpoint.")
            (LET ((INHIBIT-SCHEDULING-FLAG NIL) ;NIL or BREAK would complain
                  (*TERMINAL-IO* COLD-LOAD-STREAM))
              (FORMAT *TERMINAL-IO* "~&Package ~A." (PACKAGE-NAME *PACKAGE*))
              (BREAK "using cold load stream.")))
        (EH::RESTORE-SCREEN-FOR-COLD-LOAD-STREAM)))))

(DEFUN KBD-ESC-OUTPUT-HOLD (IGNORE)
  (PROG (P W LOCKED ANS)
    (COND ((AND (SETQ P LAST-WHO-LINE-PROCESS)
                (MEM #'EQUALP (SI::PROCESS-WAIT-WHOSTATE P) '("Output Hold" "Lock" "Window Lock"))
                (TYPEP (SETQ W (CAR (PROCESS-WAIT-ARGUMENT-LIST P))) 'SHEET))
           ;; Bludgeon our way past any deadlocks, e.g. due to the process P holding
           ;; the lock on the window we are trying to expose, or on something we need
           ;; to de-expose in order to expose it.  This code probably doesn't do a good
           ;; enough job explaining what is going on to the user.
           (COND ((AND (CONSP (SHEET-LOCK W))   ;Only temp-locked?
                       (ZEROP (SHEET-LOCK-COUNT W))
                       (LOOP FOR TW IN (SHEET-LOCK W)
                             ALWAYS (SHEET-CAN-GET-LOCK TW)))
                  (SHEET-FREE-TEMPORARY-LOCKS W))
                 ((OR (NOT (SHEET-CAN-GET-LOCK (SETQ LOCKED W)))
                      (AND (SHEET-SUPERIOR W)
                           (LOOP FOR I IN (SHEET-EXPOSED-INFERIORS (SHEET-SUPERIOR W))
                                 THEREIS (AND (SHEET-OVERLAPS-SHEET-P W I)
                                              (NOT (SHEET-CAN-GET-LOCK (SETQ LOCKED I)))))))
                  (LET-GLOBALLY ((COLD-LOAD-STREAM-OWNS-KEYBOARD T))
                    (EH::SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
                    (LET ((*QUERY-IO* COLD-LOAD-STREAM))
                      (FORMAT *QUERY-IO* "Cannot expose ~S because~@
                                        ~:[~S~;~*it~] is locked by ~S."
                              W (EQ W LOCKED) LOCKED (SHEET-LOCK LOCKED))
                      (WHEN (AND (TYPEP (SHEET-LOCK LOCKED) 'SI:PROCESS)
                                 (NOT (SHEET-EXPOSED-P W))
                                 (FQUERY NIL "Attempt to expose, pretending to be ~S? "
                                         (SHEET-LOCK LOCKED)))
                        (EH::RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)
                        (LET ((RESULT (EXPOSE-LOCKED-WINDOW W (SHEET-LOCK LOCKED))))
                          (IF (EQ RESULT T) (RETURN NIL))
                          (EH::SAVE-SCREEN-FOR-COLD-LOAD-STREAM T)
                          (IF RESULT
                              (FORMAT *QUERY-IO* "~&That got an error:~%~A" RESULT)
                            (FORMAT *QUERY-IO* "~&That did not finish in 10 seconds."))))
                      (SETQ ANS (FQUERY '(:CHOICES (((T "Yes.") #/Y #/SP #/T)
                                                    ((NIL "No.") #/N #/RUBOUT)
                                                    ((EH "To debugger.") #/D #/E))
                                          :BEEP T)
                                        "Forcibly unlock all window-system locks? "))
                      (EH::RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)))
                  (COND ((EQ ANS 'EH)
                         (SETQ *DEBUG-IO* COLD-LOAD-STREAM)
                         (SEND P :INTERRUPT %ERROR-HANDLER-STACK-GROUP '(BREAK))
                         (RETURN NIL))  ;Don't try to expose
                        (ANS (SHEET-CLEAR-LOCKS))))
                 ((AND (SHEET-EXPOSED-P W)      ;This can happen, I don't know how
                       (NOT (SHEET-LOCK W))
                       (SHEET-OUTPUT-HELD-P W))
                  (EH::SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
                  (IF (LET ((*QUERY-IO* COLD-LOAD-STREAM))
                        (FQUERY '(:BEEP T)
                                "~S is output-held for no apparent reason.~@
                                 If you know the circumstances that led to this, please~@
                                 mail in a bug report describing them.  ~
                                 Do you want to forcibly clear output-hold? "
                                W))
                      (SETF (SHEET-OUTPUT-HOLD-FLAG W) 0))
                  (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T)))
           (SEND W :EXPOSE))
          ((BEEP)))))

(DEFUN EXPOSE-LOCKED-WINDOW (WINDOW LOCKING-PROCESS)
  (LET* ((RESULT (LIST NIL))
         (PROCESS
           (PROCESS-RUN-FUNCTION "Expose locked window"
                                 #'(LAMBDA (WINDOW CURRENT-PROCESS RESULT)
                                     (CONDITION-CASE (ERROR)
                                         (SEND WINDOW :EXPOSE)
                                       (ERROR (SETF (CAR RESULT) ERROR))
                                       (:NO-ERROR (SETF (CAR RESULT) T))))
                                 WINDOW LOCKING-PROCESS RESULT)))
    (PROCESS-WAIT-WITH-TIMEOUT "Expose Window" 600. 'CAR RESULT)
    (UNLESS (CAR RESULT) (SEND PROCESS :RESET))
    (CAR RESULT)))

(DEFINE-SITE-VARIABLE *FINGER-ARG-ALIST* :ESC-F-ARG-ALIST)

(DEFVAR *LAST-PRINTED-FINGER-ARG-ALIST* NIL
  "The last *FINGER-ARG-ALIST* value from which a prompt string was made.")

(DEFVAR *LAST-PRINTED-FINGER-ARG-STRING* NIL
  "The last value returned by FINGER-ARG-PROMPT.")

(DEFUN FINGER-ARG-PROMPT ()
  "Return a string describing the possible arguments to Terminal F."
  (IF (EQUAL *FINGER-ARG-ALIST* *LAST-PRINTED-FINGER-ARG-ALIST*)
      *LAST-PRINTED-FINGER-ARG-STRING*
    (UPDATE-FINGER-ARG-PROMPT)))

(DEFUN UPDATE-FINGER-ARG-PROMPT ()
  "Force the varables concerned to update the state of the finger arg prompt."
  (SETQ *LAST-PRINTED-FINGER-ARG-ALIST* *FINGER-ARG-ALIST*
        *LAST-PRINTED-FINGER-ARG-STRING* (NEW-FINGER-ARG-PROMPT)))

(DEFUN FINGER-INITIALIZE-PROMPT ()
  "Set up the finger arg prompt to its initial state, based on site info."
  (SETQ *FINGER-ARG-ALIST* (GET-SITE-OPTION :ESC-F-ARG-ALIST))
  (UPDATE-FINGER-ARG-PROMPT))

(defun write-kbd-finger-action (for-label-p thing stream)
  (labels ((spec (x)
             (typecase x
               (symbol (case x
                         (:login
                          ;; Temporary until LISP-REINITIALIZE clears this on cold boot.
                          (if (typep fs:user-login-machine 'fs:logical-host)
                              (setq fs:user-login-machine nil))
                          (send (or fs:user-login-machine si:associated-machine)
                                (if for-label-p :name :short-name)))
                         (:associated (send si:associated-machine
                                            (if for-label-p :name :short-name)))
                         ((:lisp-machines :local-lisp-machines) "Lisp Machines")
                         (:all-lisp-machines   "All Lisp Machines")
                         (:read "ask")
                         (otherwise (format nil "~S (keyword)" x))))
               (string
                (condition-case () (let ((host (si:parse-host x t)))
                                     (if host (send host (if for-label-p :name :short-name))
                                       (format nil "~A (bad host name)" x)))
                  (error (format nil "~S (caused error)" x))))
               (null "nothing (check your site files)")
               (t (format nil "~S (illegal finger arg spec)" x))))
           (arg-out (x) (write-string (spec x) stream)))
    (when for-label-p (write-string "Who's on " stream))
    (IF (consp thing)
        (loop for spec in thing
              with plus-p = nil
              do (if plus-p (send stream :tyo #/+) (setq plus-p t))
              do (arg-out spec))
      (arg-out thing))))

(defun new-finger-arg-prompt ()
  "Return a string that describe the arguments to Terminal-F"
  (format:output nil
    (format t "Show users: ")
    (let ((arg-printed nil)
          (comma-p nil)
          (width (- (// (or (send-if-handles default-screen :inside-width)
                            (send-if-handles default-screen :width)
                            800.)
                        (or (send-if-handles default-screen :char-width)
                            10.))
                    48.))
          )
      (loop for (arg . val) in *finger-arg-alist*
            do (if comma-p
                   (if (> (send *standard-output* :read-cursorpos :character) width)
                       (format t ",~%  ")
                     (princ ", " *standard-output*))
                 (setq comma-p t))
            when arg
            do (unless arg-printed
                 (send *standard-output* :string-out "or prefix argument of ")
                 (setq arg-printed t))
            do (when arg (format t "~D: " arg))
            DO (write-kbd-finger-action nil val *standard-output*)))
    (format t ".")))

(ADD-INITIALIZATION "Finger arg prompt" '(FINGER-INITIALIZE-PROMPT)
                    '(:SITE-OPTION))

(ADD-INITIALIZATION "Finger arg prompt" '(FINGER-INITIALIZE-PROMPT)
                    '(:LOGIN))

(DEFUN AWAIT-USER-TYPEAHEAD (WINDOW)
  (SEND WINDOW :CLEAR-INPUT)                    ;I prefer to lose this way.
  (FORMAT WINDOW "~&~%Type a character to flush: ") ;well any character, anyway
  (SEND WINDOW :TYI))

(DEFUN KBD-FINGER (arg)
  "This function fingers various hosts and displays them on a temporary window.
It is called when the user types Terminal-F"
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (let* ((MODE (OR (CDR (ASSQ ARG *FINGER-ARG-ALIST*))
                     (CDR (ASSQ 'T *FINGER-ARG-ALIST*))
                     :login))
           hosts lispms-p all-lispms-p)
      (send window :set-label
            (if (eq mode :read) "Finger" (with-output-to-string (s)
                                           (write-kbd-finger-action t mode s))))
      (send window :set-process current-process)
      (flet ((check-mode-atom (thing)
               (case thing
                 (:login
                  (pushnew (or fs:user-login-machine si:associated-machine) hosts))
                 (:associated
                  (pushnew si:associated-machine hosts))
                 ((:local-lisp-machines :lisp-machines)
                  (setq lispms-p t))
                 (:all-lisp-machines
                  (setq all-lispms-p t)
                  (setq lispms-p t))
                 (otherwise ; it's a string
                  (let ((host (si:parse-host thing t)))
                    (when host
                      (pushnew host hosts)))))))
        (window-call (window :deactivate)
          (let ((*terminal-io* window))         ;In case of [Abort] printout and the like
            (setq kbd-esc-time nil)                     ;Window configuration stable now;
                                                ; let kbd process proceed
            (if (eq mode :read) ; don't need to set hosts or Lisp Machines to finger
                (progn
                  (send window :clear-input)  ;don't lose!
                  (format window
                          "~&Finger (type NAME@HOST or just @HOST, followed by Return):~%")
                  (chaos-finger (readline window) window))
              (if (listp mode)
                  (dolist (frob mode) (check-mode-atom frob))
                (check-mode-atom mode))
              (cond (all-lispms-p
                     (chaos::finger-all-lispms window))
                    (lispms-p
                     (chaos::finger-local-lispms window))
                    ((null hosts)
                     (format window "~&~S is invalid as what to finger." mode)))
              (dolist (host hosts)
                (chaos-finger (string-append "@" host) window t))))
          (AWAIT-USER-TYPEAHEAD WINDOW))))))

(DEFUN CHAOS-FINGER (SPEC WINDOW &OPTIONAL HACK-BRACKETS-P &AUX RESULT)
  (WHEN (AND (ERRORP (SETQ RESULT (CHAOS:FINGER SPEC WINDOW HACK-BRACKETS-P)))
             (CONDITION-TYPEP RESULT 'SYS:NETWORK-ERROR))
    (SEND WINDOW :FRESH-LINE)
    (SEND RESULT :REPORT WINDOW)))

(DEFUN KBD-ESC-NOTIFICATIONS (ARG &AUX NN TEM)  ;terminal n
  "Display notifications in a pop-up window.
With no argument, pending notifications only are printed. If there are none, we say so.
With an argument of T, pending notifications only are printed.
  If there are none, we return immediately.
With an argument of 1, displays all notifications.
With an argument of 2, prints nothing, but marks pending
 notifications as no longer pending."
  ;;do this before selecting new window -- don't get screwed by the wait-to-notify process
  (WITHOUT-INTERRUPTS
    (SETQ NN (APPEND PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS)
          DEFERRED-NOTIFICATIONS NIL
          PENDING-NOTIFICATIONS NIL))
  (IF (CASE ARG
        ((NIL) T)
        ((T) NN)
        (1 T)
        (2 (SETQ DEFERRED-NOTIFICATIONS NN)
           NIL))
      (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
        (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
        (SEND WINDOW :SET-LABEL "Notifications -- most recent first")
        (WINDOW-CALL (WINDOW :DEACTIVATE)
          (SETQ KBD-ESC-TIME NIL)
          ;;kludge to let us catch more notifications while this window is exposed
          (DO-FOREVER
            (WITHOUT-INTERRUPTS
              (SETQ NN (NCONC PENDING-NOTIFICATIONS NN) PENDING-NOTIFICATIONS NIL))
            (SEND WINDOW :CLEAR-WINDOW)
            (FORMAT WINDOW "~:[[There are no pending notifications]~;New notifications:~]~%"
                    NN)
            (SETQ TEM NOTIFICATION-HISTORY)
            (IF NN
                (DOLIST (N NN)
                  (TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N) WINDOW)
                  (FORMAT WINDOW ": ~A~&" (SECOND N))
                  (POP TEM)))   ;don't duplicate messages -- unseen messages are also in tem
            (WHEN ARG
              (FORMAT WINDOW "~%~:[[No old Notifications]~;Previous notifications:~]~%" TEM)
              (DOLIST (N TEM)
                (TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N) WINDOW)
                (FORMAT WINDOW ": ~A~&" (SECOND N))))
            (FORMAT WINDOW "~%Type a space to flush: ")
            (PROCESS-WAIT "Keyboard" #'(LAMBDA (WINDOW)
                                         (OR PENDING-NOTIFICATIONS
                                             (NEQ SELECTED-WINDOW WINDOW)
                                             (SEND WINDOW :LISTEN)))
                          WINDOW)
            (OR PENDING-NOTIFICATIONS
                (NEQ SELECTED-WINDOW WINDOW)
                (SEND WINDOW :TYI-NO-HANG))
            (OR PENDING-NOTIFICATIONS (RETURN T)))))))

;;; nil -- view own mail. 0 -- view random person's mail. 1 -- view random file
(DEFUN KBD-VIEW-MAIL (ARG &AUX (LUSER USER-ID) (HOST FS:USER-LOGIN-MACHINE) DIR FILE ERROR)
  (USING-RESOURCE (WINDOW TV:POP-UP-FINGER-WINDOW)
    (SETF (TV:SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (SEND WINDOW :SET-LABEL (IF (EQ ARG 1) "View file" "View mail"))
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
    (TV:WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW) (*QUERY-IO* WINDOW))
        (SETQ TV:KBD-ESC-TIME NIL)              ;Window configuration stable.
        (SELECTQ ARG
          (0 (MULTIPLE-VALUE (LUSER HOST)
               (SI::PARSE-USER-NAME (READ-LINE T NIL NIL NIL '((:PROMPT "View whose mail: ")))
                                    :DEFAULT-HOST FS:USER-LOGIN-MACHINE))
             (COND ((ERRORP HOST)
                    (TERPRI WINDOW)
                    (SEND HOST :REPORT WINDOW)
                    (TERPRI WINDOW)
                    (SETQ ERROR T))
                   ((NOT (FS:GET-PATHNAME-HOST HOST T))
                    (FORMAT T "~%~A is not a file server host; one can't read mail from it.~%"
                            HOST)
                    (SETQ ERROR T))))
          (1 (SETQ FILE (PROMPT-AND-READ :PATHNAME "View File: "))
             (SEND WINDOW :SET-LABEL (FORMAT NIL "Viewing ~A" FILE))))
        (IF ERROR
            (AWAIT-USER-TYPEAHEAD WINDOW)
          (UNLESS (EQ ARG 1)
            (SEND WINDOW :SET-LABEL (FORMAT NIL "Viewing ~A's new mail on ~A" LUSER HOST))
            (SETQ DIR (FS:USER-HOMEDIR HOST NIL LUSER)) ;this still isn't right, but...
            (SETQ FILE (SEND DIR :NEW-MAIL-PATHNAME)))
          (WITH-OPEN-FILE (S FILE :ERROR NIL :CHARACTERS T)
            (COND ((ERRORP S)
                   (IF (EQ ARG 1)
                       (FORMAT WINDOW "~&Error opening ~A: ~A" FILE S)
                     (IF (CONDITION-TYPEP S 'FS:FILE-NOT-FOUND)
                         (FORMAT WINDOW "~&~A's mail file appears to be empty." LUSER)
                       (FORMAT WINDOW "~&Unable to view ~A's mail because ~A." LUSER S)))
                   (AWAIT-USER-TYPEAHEAD WINDOW))
                  (T
                   (STREAM-COPY-UNTIL-EOF S WINDOW)
                   (SEND WINDOW :CLEAR-INPUT)   ;just to be safe
                   (FORMAT WINDOW
                           "~3&Type ~@[~C to delete ~A's mail, or ~]a space to flush: "
                           (NEQ ARG 1) #/DELETE LUSER)
                   (LET ((RESPONSE (SEND WINDOW :TYI)))
                     (WHEN (AND (EQ RESPONSE #/DELETE)
                                (NEQ ARG 1)
                                (YES-OR-NO-P "Do you REALLY want to delete this mail ? "))
                       (SEND S :DELETE))))))))))
  NIL)

(DEFUN KBD-GC-STATUS ()
  "Show the user the state of the garbage collector."
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (SEND WINDOW :SET-LABEL "GC Status")
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
    (WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW))
        (SETQ KBD-ESC-TIME NIL)
        (GC:STATUS WINDOW)
        (GC:PRINT-STATISTICS WINDOW)
        (explain-run-bars window)
        (AWAIT-USER-TYPEAHEAD WINDOW)))))

(defun explain-run-bars (stream)
  (format stream "~3%                                Run Bar Definitions")
  (format stream "~2% --------             ----//----      ----------     ---------")
  (format stream "~1% SB defer             Scav Trans        Disk           Run"))




(DEFUN KBD-HOSTAT ()                            ;terminal h
  "In a temporary window, show the user the status of various Chaosnet hosts."
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (SEND WINDOW :SET-LABEL "Hostat")
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
    (WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW))
        (SETQ KBD-ESC-TIME NIL)                 ;Window configuration stable.
        (HOSTAT)
        (AWAIT-USER-TYPEAHEAD WINDOW)))))

(DEFUN KBD-ESC-I (ARG)                          ;terminal i
  "Control the selected window's deexposed type-IN action.
0 => wait, 1 => notify."
  (COND ((NOT (OR (NULL ARG) ( 0 ARG 1)))
         (BEEP))
        ((NULL SELECTED-WINDOW) (BEEP))
        (T
         (LET ((CURRENT-ACTION (SEND SELECTED-WINDOW :DEEXPOSED-TYPEIN-ACTION)))
           (SELECTQ ARG
             ((NIL)
              (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEIN-ACTION
                    (IF (EQ CURRENT-ACTION :NOTIFY) :NORMAL :NOTIFY)))
             (0 (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEIN-ACTION :NORMAL))
             (1 (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEIN-ACTION :NOTIFY)))))))

(DEFUN KBD-ESC-T (ARG)                          ;terminal t
  "Control the selected window's deexposed typeout action.
0 => wait, 1 => notify, 2 => permit."
  (COND ((NOT (OR (NULL ARG) ( 0 ARG 2)))
         (BEEP))
        ((NULL SELECTED-WINDOW) (BEEP))
        (T
         (LET ((CURRENT-ACTION (SEND SELECTED-WINDOW :DEEXPOSED-TYPEOUT-ACTION)))
           (IF (NOT (MEMQ CURRENT-ACTION '(:NOTIFY :NORMAL :PERMIT)))
               (BEEP)   ;If it's something funny, don't change it!
             (CASE ARG
               ((NIL)
                (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION
                      (IF (EQ CURRENT-ACTION :NOTIFY) :NORMAL :NOTIFY)))
               (0 (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION :NORMAL))
               (1 (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION :NOTIFY))
               (2 (SEND SELECTED-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION :PERMIT))))))))

(DEFUN KBD-ESC-W (ARG &AUX PROC)                ;terminal w
  (SETQ PROC LAST-WHO-LINE-PROCESS)
  (CASE ARG
    (NIL (SEND WHO-LINE-SCREEN :REFRESH))
    (0 (SETQ WHO-LINE-PROCESS
             (LET ((ALIST (MAPCAR #'(LAMBDA (P) (CONS (PROCESS-NAME P) P)) ALL-PROCESSES)))
               (MENU-CHOOSE ALIST "Who-line process:" '(:MOUSE) (RASSOC PROC ALIST)))))
    (1 (SETQ WHO-LINE-PROCESS NIL))
    (2 (SETQ WHO-LINE-PROCESS PROC))
    (3 (SETQ WHO-LINE-PROCESS (DO ((L ALL-PROCESSES (CDR L)))
                                  ((NULL L) (CAR ALL-PROCESSES))
                                (AND (EQ (CAR L) PROC)
                                     (RETURN (OR (CADR L) (CAR ALL-PROCESSES)))))))
    (4 (SETQ WHO-LINE-PROCESS (OR (DO ((L ALL-PROCESSES (CDR L))
                                       (OL NIL L))
                                      ((NULL L) NIL)
                                    (AND (EQ (CAR L) PROC)
                                         (RETURN (CAR OL))))
                                  (CAR (LAST ALL-PROCESSES))))))
  (WHO-LINE-RUN-STATE-UPDATE)
  (WHO-LINE-UPDATE))

(DEFRESOURCE HARDCOPY-BIT-ARRAY-RESOURCE ()
  :CONSTRUCTOR (MAKE-PIXEL-ARRAY (* (send main-screen :locations-per-line) 32.)
                                 (+ MAIN-SCREEN-HEIGHT 100)     ;Big enough for
                                 :TYPE 'ART-1B)
  :INITIAL-COPIES 0)                            ; for (SET-TV-SPEED 60.)

(defvar *terminal-3-q-margin* 1
  "Number of bits (dimension) to clip from screen dump,
If this is a list, the dimensions are (<horizontal> <vertical>)")

;;; An argument of 1 does only the selected window; 0 does the default screen; 3 clips
;;; the main screen and wholine as specified in *TERMINAL-3-Q-MARGIN*
;;; 4 does the current FRAME, if there is one.

(DEFUN KBD-ESC-Q (ARG &AUX (PRINTER (OR SI:*DEFAULT-BIT-ARRAY-PRINTER* SI:*DEFAULT-PRINTER*)))
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (SI:EXPAND-PRINTER-NAME PRINTER)))
  (IF (GET (IF (CONSP PRINTER) (CAR PRINTER) PRINTER) 'SI:PRINT-BIT-ARRAY)
      (USING-RESOURCE (ARRAY HARDCOPY-BIT-ARRAY-RESOURCE)
        (MULTIPLE-VALUE-BIND (NIL WIDTH HEIGHT)
            (SNAPSHOT-SCREEN (SELECTQ ARG
                               (1 SELECTED-WINDOW)
                               (0 DEFAULT-SCREEN)
                               (4 (DO ((WINDOW SELECTED-WINDOW (SEND WINDOW :SUPERIOR)))
                                      ((OR (NULL WINDOW) (TYPEP WINDOW 'BASIC-FRAME))
                                       (OR WINDOW
                                           (RETURN-FROM KBD-ESC-Q
                                             (TV:NOTIFY NIL "Window or superior not a frame"))))))
                               (OTHERWISE
                                (main-screen-and-who-line)))
                             ARRAY (if (not (or (equal arg 0) (equal arg 1) (equal arg 4)))
                                       (+ main-screen-width
                                          (if (and (boundp 'fancy-landscape) fancy-landscape)
                                              control-panel-width
                                            0))))
          (BEEP)
          (let ((left 0)
                (top 0)
                (right width)
                (bottom height))
            (when (eq arg 3)
              (let ((h (if (atom *terminal-3-q-margin*) *terminal-3-q-margin*
                         (car *terminal-3-q-margin*)))
                    (v (if (atom *terminal-3-q-margin*) *terminal-3-q-margin*
                         (cadr *terminal-3-q-margin*))))
                (setq left h right (- right h)
                      top v bottom (- bottom v))))
            (HARDCOPY-BIT-ARRAY ARRAY left top right bottom ':PRINTER PRINTER))))
    (TV:NOTIFY NIL "I don't know a printer that can hardcopy the screen")))

(DEFUN SNAPSHOT-SCREEN (FROM-ARRAY TO-ARRAY &OPTIONAL WIDTH HEIGHT)
  ;; (set 'w width) ; Why was this here?  -smh 24aug88
  (WITHOUT-INTERRUPTS
    (COND ((ARRAYP FROM-ARRAY)
           (OR WIDTH (SETQ WIDTH (PIXEL-ARRAY-WIDTH FROM-ARRAY)))
           (OR HEIGHT (SETQ HEIGHT (PIXEL-ARRAY-HEIGHT FROM-ARRAY))))
          (T
           (OR WIDTH (SETQ WIDTH (SHEET-WIDTH FROM-ARRAY)))
           (OR HEIGHT (SETQ HEIGHT (SHEET-HEIGHT FROM-ARRAY)))
           (SETQ FROM-ARRAY (OR (SHEET-SCREEN-ARRAY FROM-ARRAY)
                                (FERROR "Window ~S does not have an array" FROM-ARRAY)))))
    (WHO-LINE-UPDATE)
    (BITBLT ALU-SETZ (PIXEL-ARRAY-WIDTH TO-ARRAY) (PIXEL-ARRAY-HEIGHT TO-ARRAY)
            TO-ARRAY 0 0 TO-ARRAY 0 0)
    (BITBLT ALU-SETA WIDTH HEIGHT FROM-ARRAY 0 0 TO-ARRAY 0 0))
  (VALUES TO-ARRAY WIDTH HEIGHT))

(DEFUN KBD-ESC-HELP (IGNORE &AUX DOC (INDENT 15.))      ;terminal help
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
    (SEND WINDOW :SET-LABEL "Keyboard documentation")
    (WINDOW-MOUSE-CALL (WINDOW :DEACTIVATE)
      (SETQ KBD-ESC-TIME NIL)
      (FORMAT WINDOW "~25TType ~:C followed by:

~:C~VTDo nothing. (Use this if you typed ~:C by accident and want to cancel it.)
0-9, -~VTNumeric argument to following command~%"
              #/TERMINAL #/RUBOUT INDENT #/TERMINAL INDENT)
      (DOLIST (X *ESCAPE-KEYS*)
        (COND ((NULL (CAR X))
               (SETQ INDENT 20.)
               (FORMAT WINDOW "~%~5@TThese are for wizards:~2%"))
              ((SETQ DOC (EVAL (CADDR X)))
               (FORMAT WINDOW "~:C" (CAR X))
               (PRINT-STRING-WITH-INDENTATION
                 WINDOW
                 (IF (ATOM DOC) DOC (CAR DOC))
                 INDENT)
               (OR (ATOM DOC) (DOLIST (LINE (CDR DOC))
                                (PRINT-STRING-WITH-INDENTATION WINDOW LINE INDENT))))))
      (FORMAT WINDOW "~3%~25TKeyboard function keys:

Abort           Throw to command level          Break           Get read-eval-print loop
Control-Abort   To command level immediately    Control-Break   BREAK immediately
Meta-Abort      Throw out of all levels         Meta-Break      Get to error-handler
C-M-Abort       Out of all levels immediately   C-M-Break       Error-handler immediately
Macro           Keyboard macros (in Zmacs)      Stop-Output     (not used)
Terminal        The above commands              Resume          Continue from break//error
System          Select a Program                Call            (not used)
Network         Supdup//Telnet commands         Status          Print Input History
Quote           (not used)                      C-Status        Print Kill History
Overstrike      /"backspace/"                   Delete          (not used)
Clear-Input     Clear type-in                   End             Terminate input
C-Clear-Input   Forget typeahead                Help            Print documentation
Clear-Screen    Refresh screen                  Return          Carriage return
Hold-Output     (not used)                      Line            Next line and indent (Zmacs)
")
      (FORMAT WINDOW "~%Type a space to flush: ")
      (SEND WINDOW :TYI))))

(DEFUN PRINT-STRING-WITH-INDENTATION (STREAM STRING INDENT &AUX POS)
  "Print STRING on STREAM, indenting each line in STRING by INDENT."
  (DO ((START 0) (END (STRING-LENGTH STRING)))
      (( START END))
    (FORMAT STREAM "~VT" INDENT)
    (SETQ POS (STRING-SEARCH-CHAR #/RETURN STRING START))
    (PRINC (NSUBSTRING STRING START POS) STREAM)
    (TERPRI STREAM)
    (SETQ START (IF POS (1+ POS) END))))

(DEFVAR *SYSTEM-KEYS* NIL
  "Defines keys you can type after the System key.
Each element is a list (CHARACTER FLAVOR DOCUMENTATION-STRING CREATE-FLAVOR)
CHARACTER is the character this applies to.
DOCUMENTATION-STRING says what kind of window the character selects.
FLAVOR is a flavor name, meaning look for a window of that flavor,
 a window, meaning select that window, or else an expression
 which returns either a window to select or a flavor name.
CREATE-FLAVOR can be NIL meaning do not create windows ever,
 T meaning instantiate FLAVOR, or a some other flavor name to instantiate,
 or a list of forms to evaluate to produce the new window.")

(DEFUN REMOVE-SYSTEM-KEY (CHAR)
  "Remove any definition for CHAR typed after the System key."
  (SETQ *SYSTEM-KEYS*
        (DELETE-IF #'(LAMBDA (ELT) (CHAR-EQUAL (CAR ELT) CHAR))
                   *SYSTEM-KEYS* :COUNT 1)))

(DEFUN ADD-SYSTEM-KEY (CHAR WINDOW-OR-FLAVOR DOCUMENTATION &OPTIONAL (CREATE T))
  "Make typing the System key followed by CHAR select the window WINDOW-OR-FLAVOR.
WINDOW-OR-FLAVOR may be:
  an actual window
  a name of a flavor of window
  a list to evaluate to get a window or a flavor name.
CREATE says whether and how to create a new window if Control-char is pressed.
  It may be:
  T meaning create a window of flavor PROGRAM, or
  the name of a flavor to create, or
  a list to evaluate for effect, to create and select a window.
If CHAR is already defined to select a flavor window, then the old version is
  remembered. To restore the former definition, use (TV:REMOVE-SYSTEM-KEY CHAR)"
  (PUSHNEW (LIST (CHAR-UPCASE (GLOBAL:CHARACTER CHAR)) WINDOW-OR-FLAVOR DOCUMENTATION CREATE)
           *SYSTEM-KEYS*
           :TEST #'EQUAL)
  (SETQ *SYSTEM-KEYS* (STABLE-SORTCAR *SYSTEM-KEYS* #'ALPHALESSP)))

(DEFUN KBD-SYS (&REST IGNORE &AUX CH)
  (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
    (WHO-LINE-RUN-STATE-UPDATE)  ;Necessary to make above take effect
    (SETQ CH (CHAR-UPCASE (KBD-GET-SOFTWARE-CHAR "System-"))))
  (WHO-LINE-RUN-STATE-UPDATE)   ;Switch LAST-WHO-LINE-PROCESS back
  ;; Anything typed before the System belongs to the currently selected window
  ;; Anything typed after this belongs to the new window we are going to get to.
  (WITHOUT-INTERRUPTS
    (AND (KBD-GET-IO-BUFFER)
         (KBD-SNARF-INPUT SELECTED-IO-BUFFER T)))
  (SETQ KBD-ESC-TIME (TIME))
  (PROCESS-RUN-FUNCTION "KBD SYS" #'KBD-SYS-1 CH))

(DEFUN KBD-SYS-1 (CH &AUX E W SW MAKENEW FLAVOR-OR-WINDOW SW-ALIAS)
  (SETQ MAKENEW (CHAR-BIT CH :CONTROL)
        CH (CHAR-CODE CH))
  (COND ((OR (= CH #/?) (= CH #/HELP))
         (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
           (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
           (SEND WINDOW :SET-LABEL "Keyboard system commands")
           (WINDOW-CALL (WINDOW :DEACTIVATE)
             (FORMAT WINDOW
                     "Type ~:@C followed by one of these characters to select the ~
                      corresponding program:~2%" #/SYSTEM)
             (LET ((LIST (SORTCAR (COPY-LIST *SYSTEM-KEYS*) #'ALPHALESSP)) (TEM #/?))
               (DOLIST (X LIST)
                 (OR (CHAR-EQUAL TEM (SETQ TEM (first X)))      ;Skip duplicates
                     (and (atom (second x))                     ;Skip invalid flavors
                          (not (get (second x) 'si:flavor)))
                     (FORMAT WINDOW "~&~:@C~16T~A" TEM (third X)))))
             (FORMAT WINDOW
               "~2&Type ~:@C control-<character> to create a new window of a particular type.~@
                Type ~:@C after ~:@C to do nothing (if you typed ~:@C by accident).~%~@
                Type a space to flush: " #/SYSTEM #/RUBOUT #/SYSTEM #/SYSTEM)
             (SETQ KBD-ESC-TIME NIL)            ;Let kbd process proceed before we TYI.
             (SEND WINDOW :TYI))))
        ((SETQ E (ASSQ CH *SYSTEM-KEYS*))
         ;; Find the most recently selected window of the desired type.
         ;; If it is the same type as the selected window, make that the
         ;; least recently selected so as to achieve the cycling-through effect.
         ;; Otherwise the currently selected window becomes the most recently
         ;; selected as usual, and terminal S will return to it.
         ;; In any case, we must fake out :MOUSE-SELECT's typeahead action since
         ;; that has already been properly taken care of and we don't want to snarf
         ;; any characters already typed after the [SYSTEM] command.
         (SETQ FLAVOR-OR-WINDOW
               (IF (CONSP (SECOND E))
                   (EVAL (SECOND E))
                 (SECOND E)))
         (DELAYING-SCREEN-MANAGEMENT    ;Inhibit auto selection
           (SETQ SW SELECTED-WINDOW)
           (WHEN SW (SETQ SW-ALIAS (SEND SW :ALIAS-FOR-SELECTED-WINDOWS)))
           (COND ((TYPEP FLAVOR-OR-WINDOW 'ESSENTIAL-WINDOW)
                  ;; If the *SYSTEM-KEYS* list has a specific window indicated, use that.
                  (AND SW (SEND SW :DESELECT NIL))
                  (SEND FLAVOR-OR-WINDOW :MOUSE-SELECT))
                 ((NULL FLAVOR-OR-WINDOW) NIL)  ;NIL means he already did whatever he wanted.
                 ((not (get flavor-or-window 'si:flavor)) (beep))       ;Invalid flavor
                 ((AND (NOT MAKENEW)
                       (SETQ W (FIND-WINDOW-OF-FLAVOR FLAVOR-OR-WINDOW)))
                  ;; Cycle through other windows of this flavor.
                  (WHEN SW (SEND SW :DESELECT (IF (TYPEP SW-ALIAS FLAVOR-OR-WINDOW) :END)))
                  (SEND W :MOUSE-SELECT))
                 ((AND (NOT MAKENEW) SW
                       (TYPEP SW-ALIAS FLAVOR-OR-WINDOW))
                  ;; There is only one window of this flavor, and this is it.
                  (BEEP))
                 ((NULL (FOURTH E)) (BEEP))     ;Cannot create
                 ((ATOM (FOURTH E))
                  ;; Create a new window of this flavor.
                  ;; We create on the default screen.
                  (WHEN SW (SEND SW :DESELECT (IF (TYPEP SW-ALIAS FLAVOR-OR-WINDOW) :END)))
                  (LET ((FLAVOR (IF (EQ (FOURTH E) T) FLAVOR-OR-WINDOW (FOURTH E))))
                    (if (get flavor 'si:flavor)
                        (SEND (MAKE-INSTANCE FLAVOR :SUPERIOR DEFAULT-SCREEN) :MOUSE-SELECT)
                      (beep))))
                 (T (EVAL (FOURTH E))))))
        (( CH #/RUBOUT) (BEEP)))
  (SETQ KBD-ESC-TIME NIL))

(DEFUN FIND-WINDOW-OF-FLAVOR (FLAVOR)
  "Find a previously selected window whose flavor includes FLAVOR."
  (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
    (LET ((W (AREF PREVIOUSLY-SELECTED-WINDOWS I)))
      (AND W (TYPEP W FLAVOR) (SEND W :NAME-FOR-SELECTION)
           (RETURN W)))))

;;;; Notifications (call side)

(DEFVAR NOTIFICATION-HISTORY NIL)       ;Each entry is list of time and string
(ADD-INITIALIZATION "Forget old notifications"
                    '(SETQ NOTIFICATION-HISTORY NIL)
                    '(:BEFORE-COLD))

(DEFUN PRINT-NOTIFICATIONS (&OPTIONAL (FROM 0) TO)
  "Reprint all notifications that have happened, newest first.
If FROM is nonzero, that many of the most recent notificatiosn are skipped.
If TO is non-NIL, we stop after printing the TO'th most recent notification."
  (FORMAT T "~&~:[No notifications.~;Notifications, most recent first:~]~%"
            NOTIFICATION-HISTORY)
  (DO ((NLIST NOTIFICATION-HISTORY (CDR NLIST))
       (I 0 (1+ I)))
      ((OR (NULL NLIST)
           (AND TO ( I TO))))
    (UNLESS (< I FROM)
      (LET ((N (CAR NLIST)))
        (TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST N))
        (FORMAT T " ~A~%" (SECOND N))))))

(DEFUN NOTIFY (WINDOW-OF-INTEREST FORMAT-CONTROL &REST FORMAT-ARGS)
  "Notify the user with an unsolicited message.
The message is generated from FORMAT-CONTROL and FORMAT-ARGS.
If WINDOW-OF-INTEREST is non-NIL, it is a window to be made available to
Terminal-0-S and maybe another way depending on who prints the notification"
  (APPLY #'CAREFUL-NOTIFY WINDOW-OF-INTEREST NIL FORMAT-CONTROL FORMAT-ARGS))

(DEFUN CAREFUL-NOTIFY (WINDOW-OF-INTEREST CAREFUL-P FORMAT-CONTROL &REST FORMAT-ARGS)
  "Like NOTIFY but will not hang up waiting for locks if CAREFUL-P is T.
If locks are locked or there is no selected-window, returns NIL.  If succeeds
in printing the notification, returns T."
  (LET ((TIME (TIME:GET-UNIVERSAL-TIME))
        (MESSAGE (APPLY #'FORMAT NIL FORMAT-CONTROL FORMAT-ARGS)))
    (PUSH (LIST TIME MESSAGE) NOTIFICATION-HISTORY)
    (WHEN WINDOW-OF-INTEREST                    ;Make this window "interesting"
      (WITHOUT-INTERRUPTS
        (OR (ASSQ WINDOW-OF-INTEREST BACKGROUND-INTERESTING-WINDOWS)
            (PUSH (CONS WINDOW-OF-INTEREST NIL) BACKGROUND-INTERESTING-WINDOWS)))
      (IF (SHEET-CAN-GET-LOCK WINDOW-OF-INTEREST)       ;Try to make available to sys menu
          (SEND WINDOW-OF-INTEREST :ACTIVATE))) ;but don't bother if locked
    ;; Get a selected-window to which to send the :print-notification message
    (IF (NOT CAREFUL-P)
        (IF WAIT-FOR-NOTIFICATIONS-FLAG
            ;; It's going to wait anyway, so don't lock anything!
            (DO ((SW SELECTED-WINDOW SELECTED-WINDOW))
                (SW (SEND SW :PRINT-NOTIFICATION TIME MESSAGE WINDOW-OF-INTEREST)
                    T)
              (PROCESS-WAIT "A selected window" 'SYMEVAL 'SELECTED-WINDOW))
          ;; What this piece of hair is all about is that we don't want to pick a window
          ;; to print the notification on and then have that window deexposed out from
          ;; under us, causing us to hang forever.  So we lock the window while printing
          ;; the notification, which is assumed is going to be on either the window itself
          ;; or one of its direct or indirect inferiors.  Any windows which don't print
          ;; their notification this way must spawn a separate process to do the printing.
          (LOOP AS INHIBIT-SCHEDULING-FLAG = T AS SW = SELECTED-WINDOW
                WHEN (AND (NOT (NULL SW))
                          (SHEET-CAN-GET-LOCK SW))
                RETURN (LOCK-SHEET (SW)
                         (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                         (SEND SW :PRINT-NOTIFICATION TIME MESSAGE WINDOW-OF-INTEREST))
                DO (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                   (PROCESS-WAIT "A selected window"
                                 (LAMBDA (SW) (OR (NEQ SELECTED-WINDOW SW)
                                                  (AND SW (SHEET-CAN-GET-LOCK SW))))
                                 SW)))
      ;; In this case, we simply want to punt if we don't seem to be able to acquire
      ;; the necessary locks.
      (WITHOUT-INTERRUPTS
        (LET ((SW SELECTED-WINDOW))
          (COND ((OR (NULL SW)                          ;No one in charge
                     (SHEET-OUTPUT-HELD-P SW)           ;Guy in charge locked or broken
                     (NOT (SHEET-CAN-GET-LOCK           ;Anything locked, even by this process,
                            (SHEET-GET-SCREEN SW) T)))  ; that would hang Terminal-0-S
                 NIL)                                   ;Lose, don't try to notify
                (T                                      ;Win, go ahead
                 (LOCK-SHEET (SW)
                    (LET ((INHIBIT-SCHEDULING-FLAG NIL)
                          (WAIT-FOR-NOTIFICATIONS-FLAG NIL))
                      (SEND SW :PRINT-NOTIFICATION TIME MESSAGE WINDOW-OF-INTEREST)))
                 T)))))))

;;;; Background stream

;(DEFVAR DEFAULT-BACKGROUND-STREAM 'BACKGROUND-STREAM)  ;in COLD
(DEFVAR PROCESS-IS-IN-ERROR NIL)
(DEFVAR BACKGROUND-INTERESTING-WINDOWS NIL)

(DEFFLAVOR BACKGROUND-LISP-INTERACTOR () (LISP-INTERACTOR)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION :NOTIFY
                       :DEEXPOSED-TYPEIN-ACTION :NOTIFY))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :BEFORE :INIT) (PLIST)
  (SETF (GET PLIST :SAVE-BITS) T))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :SET-PROCESS) (NP)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) NP)
  (SETQ PROCESS NP))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :SELECT) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (SETQ BACKGROUND-INTERESTING-WINDOWS
          (DELQ (ASSQ SELF BACKGROUND-INTERESTING-WINDOWS)
                BACKGROUND-INTERESTING-WINDOWS))))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :DEACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (SETQ BACKGROUND-INTERESTING-WINDOWS
          (DELQ (ASSQ SELF BACKGROUND-INTERESTING-WINDOWS)
                BACKGROUND-INTERESTING-WINDOWS))))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :WAIT-UNTIL-SEEN) ()
  ;; If we have typed out since we were selected last, then wait until we get seen
  (WHEN (ASSQ SELF BACKGROUND-INTERESTING-WINDOWS)
    (PROCESS-WAIT "Seen" (LAMBDA (S)
                           (NOT (ASSQ S BACKGROUND-INTERESTING-WINDOWS)))
                  SELF)
    ;; Then wait until we are deselected
    (PROCESS-WAIT "No Longer Seen" (LAMBDA (S) (NEQ S SELECTED-WINDOW)) SELF)))

(DEFVAR BACKGROUND-STREAM-WHICH-OPERATIONS)

(DEFUN BACKGROUND-STREAM (OP &REST ARGS)
  "This function is defaultly used as *TERMINAL-IO* for all processes.  If it gets called
at all, it turns *TERMINAL-IO* into a lisp listener window, and notifies the user that
the process wants the terminal."
  (cond ((NOT (AND (VARIABLE-BOUNDP INITIAL-LISP-LISTENER)
                   INITIAL-LISP-LISTENER))
         ;; Window system not fully turned on yet.
         (LEXPR-SEND COLD-LOAD-STREAM OP ARGS))
        ((EQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
         (CASE OP
           (:operation-handled-p
            (memq (first args) (background-stream :which-operations)))
           (:WHICH-OPERATIONS
            ;; Get the which-operations once, but after the flavor has been compiled
            (OR (BOUNDP 'BACKGROUND-STREAM-WHICH-OPERATIONS)
                (USING-RESOURCE (WINDOW BACKGROUND-LISP-INTERACTORS)
                  (SETQ BACKGROUND-STREAM-WHICH-OPERATIONS
                        (SEND WINDOW :WHICH-OPERATIONS))
                  (mapcar #'(lambda (operation)
                              (pushnew operation background-stream-which-operations))
                          ;;; Include the operations handled here.
                          '(:operation-handled-p
                             :which-operations
                             :send-if-handles
                             :beep
                             :listen
                             :inhibit-output-for-abort-p
                             :await-exposure))))
            BACKGROUND-STREAM-WHICH-OPERATIONS)
           ;; If the stream hasn't changed since the process was started, do default action
           (:SEND-IF-HANDLES
            (IF (MEMQ (CAR ARGS) (BACKGROUND-STREAM :WHICH-OPERATIONS))
                (APPLY #'BACKGROUND-STREAM ARGS)))
           (:BEEP
            (LET ((W (WITHOUT-INTERRUPTS
                       (IF SELECTED-WINDOW
                           (SHEET-GET-SCREEN SELECTED-WINDOW)
                         DEFAULT-SCREEN))))
              (LEXPR-SEND W :BEEP ARGS)))
           (:LISTEN NIL)
           (:INHIBIT-OUTPUT-FOR-ABORT-P T)
           (:AWAIT-EXPOSURE NIL)
           (OTHERWISE
            (IF (EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
                (FERROR "Attempt to create a background window while in the scheduler."))
            (SETQ *TERMINAL-IO* (ALLOCATE-RESOURCE 'BACKGROUND-LISP-INTERACTORS))
            (SHEET-FORCE-ACCESS (*TERMINAL-IO* :NO-PREPARE)
              (let ((new-name (string-append (process-name current-process) " Background Stream")))
                (SEND *TERMINAL-IO* :SET-LABEL new-name)
                (SEND *TERMINAL-IO* :SET-PROCESS CURRENT-PROCESS)
                (SEND *TERMINAL-IO* :CLEAR-WINDOW)))
            (SEND *TERMINAL-IO* :ACTIVATE)
            (LEXPR-SEND *TERMINAL-IO* OP ARGS))))
        ((streamp *terminal-io*)
         (LEXPR-SEND *TERMINAL-IO* OP ARGS))
        (t
         (SETQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
         (LEXPR-SEND *TERMINAL-IO* OP ARGS))))

;;; Note that this stuff is completely different from the LOCKED-ERROR-WINDOWS hack.
;;; A window is on there when the window system is locked up against it
;;; and its notification cannot be printed.
;;; A process is "in-error-p" when its notification HAS been printed.

(DEFUN FIND-PROCESS-IN-ERROR (&AUX WINDOW SG)
  "Return a process waiting for window exposure for error handling.
The value is a process, or NIL."
  (DECLARE (VALUES PROCESS ASSOCIATED-WINDOW))
  (WITHOUT-INTERRUPTS
    (DOLIST (P ACTIVE-PROCESSES)
      (AND (SETQ P (CAR P))
           (TYPEP (SETQ SG (PROCESS-STACK-GROUP P)) 'STACK-GROUP)
           (SETQ WINDOW (SI::PROCESS-IS-IN-ERROR-P P))
           (RETURN (VALUES P WINDOW))))))

(DEFUN CHOOSE-PROCESS-IN-ERROR (&AUX SG WINDOW)
  "Ask user to choose among processes waiting for window exposure for error handling.
The value is a process, or NIL."
  (DECLARE (VALUES (PROCESS ASSOCIATED-WINDOW)))
  (DOLIST (P ACTIVE-PROCESSES)
    (AND (SETQ P (CAR P))
         (TYPEP (SETQ SG (PROCESS-STACK-GROUP P)) 'STACK-GROUP)
         (SETQ WINDOW (SI::PROCESS-IS-IN-ERROR-P P))
         (Y-OR-N-P (FORMAT NIL "Use process ~A? " (PROCESS-NAME P)))
         (RETURN (VALUES P WINDOW)))))

;;;Select an "interesting" background window.
(DEFUN SELECT-INTERESTING-WINDOW (&AUX INTERESTING NOTIFICATION-WINDOW)
  (WHEN (SETQ INTERESTING (FIND-INTERESTING-WINDOW))
    (SETQ NOTIFICATION-WINDOW (CDR (ASSQ INTERESTING BACKGROUND-INTERESTING-WINDOWS)))
    (AND NOTIFICATION-WINDOW
         (EQ (SEND NOTIFICATION-WINDOW :WINDOW-OF-INTEREST) INTERESTING)
         (SEND NOTIFICATION-WINDOW :EXPOSED-P)
         (SEND NOTIFICATION-WINDOW :DEEXPOSE))
    (SEND INTERESTING :MOUSE-SELECT)
    (SETQ BACKGROUND-INTERESTING-WINDOWS
          (DELQ (ASSQ INTERESTING BACKGROUND-INTERESTING-WINDOWS)
                BACKGROUND-INTERESTING-WINDOWS))))

(DEFUN FIND-INTERESTING-WINDOW ()
  (OR (NTH-VALUE 1 (FIND-PROCESS-IN-ERROR))
      (CAAR BACKGROUND-INTERESTING-WINDOWS)))

;;; More or less innocuous functions from the old window system that are called all over the
;;; place.
(DEFUN KBD-TYI (&REST IGNORE)
  "The same thing as (SEND *TERMINAL-IO* :TYI)."
  (SEND *TERMINAL-IO* :TYI))

(DEFUN KBD-TYI-NO-HANG (&REST IGNORE)
  "The same thing as (SEND *TERMINAL-IO* :TYI-NO-HANG)."
  (SEND *TERMINAL-IO* :TYI-NO-HANG))

(DEFUN KBD-CHAR-AVAILABLE (&REST IGNORE)
  "The same thing as (SEND *TERMINAL-IO* :LISTEN)."
  (SEND *TERMINAL-IO* :LISTEN))

(compiler:make-obsolete kbd-tyi "send *TERMINAL-IO* a :TYI or :READ-CHAR message
or use READ-CHAR")
(compiler:make-obsolete kbd-tyi-no-hang "send *TERMINAL-IO* a :TYI-NO-HANG or :READ-CHAR-NO-HANG
message or use READ-CHAR-NO-HANG")
(compiler:make-obsolete kbd-char-available "use LISTEN or send *TERMINAL-IO* a :LISTEN message")

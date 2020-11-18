;;; -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-
;;; Zwei commands, see ZWEI;COMA for comments
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Q-Register Commands.

(DEFUN GET-REGISTER-NAME (PROMPT &OPTIONAL PURPOSE &AUX CHAR ECHO-FLAG)
  "Read a register name in the echo area.
Puts PROMPT in the mode line.  Returns a symbol in the utility package.
The ZWEI:TEXT property of that symbol is the text contents.
The ZWEI:POINT property of it is a location saved on it."
;character lossage
  (SETQ CHAR (READ-CHAR-NO-HANG *STANDARD-INPUT*))
  (DO-FOREVER
    (WHEN (NULL CHAR)
      (FORMAT *QUERY-IO* "~&~A " PROMPT)
      (TYPEIN-LINE-ACTIVATE
        (SETQ CHAR (READ-CHAR *STANDARD-INPUT*)))
      (SETQ ECHO-FLAG T))
    (IF ( CHAR #/HELP)
        (RETURN)
      (SETQ CHAR NIL)
      (SEND *QUERY-IO* :CLEAR-WINDOW)
      (FORMAT *QUERY-IO* "You are typing the name of a ZWEI register~A.
A name is just a character sans meta bits; case is ignored."
              (OR PURPOSE ""))))
  (IF (OR (> (CHAR-CODE CHAR) #O177)
          (EQ CHAR #/C-G))
      (SIGNAL EH:ABORT-OBJECT))
  (SETQ CHAR (CHAR-UPCASE (CHAR-CODE CHAR)))
  (IF ECHO-FLAG (FORMAT *QUERY-IO* "~C" CHAR))
  (MAKE-REGISTER-NAME CHAR))

(DEFUN MAKE-REGISTER-NAME (CHAR &AUX SYM STR OLDP)
  "Return the symbol that represents the ZWEI register named CHAR."
  (SETQ STR (STRING-APPEND CHAR))
  (MULTIPLE-VALUE (SYM OLDP)
    (INTERN-LOCAL STR *UTILITY-PACKAGE*))
  (UNLESS OLDP
    (PUSH SYM *Q-REG-LIST*))
  SYM)

(DEFCOM COM-OPEN-GET-REGISTER "Insert text from a specified register, gobbling blank lines.
The register name, a character with no meta bits, is read from the keyboard.
Uses up blank lines the way Return does (calling the definition of Return).
Leaves the point after, and the mark before, the text.
With an argument, puts point before and mark after." ()
  (LET ((QREG (GET-REGISTER-NAME "Get text from register:" " containing text")))
    (LET ((POINT (POINT))
          (MARK (MARK))
          (THING (GET QREG 'TEXT)))
      (OR THING (BARF "The register ~A does not contain any text." QREG))
      (WITH-UNDO-SAVE ("Get register" POINT POINT T)
        (MOVE-BP MARK (INSERT-INTERVAL POINT THING))
        (FIXUP-FONTS-INTERVAL (GET QREG 'TEXT-FONTS) POINT MARK T)
        (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
        (LET ((SAVE-PT (COPY-BP POINT))
              (NL (1- (COUNT-LINES POINT MARK))))
          (AND (BEG-LINE-P (MARK))
               (MOVE-BP MARK (FORWARD-CHAR MARK -1)))
          (MOVE-BP POINT MARK)
          (DOTIMES (I NL)
            (KEY-EXECUTE #/CR))
          (DELETE-INTERVAL POINT MARK)
          (MOVE-BP (POINT) SAVE-PT)))
      (OR *NUMERIC-ARG-P*
          (SWAP-BPS POINT MARK))))
  DIS-TEXT)

(DEFCOM COM-GET-REGISTER "Insert text containied in specified register.
The register name, a character with no meta bits, is read from the keyboard.
Leaves the pointer before, and the mark after, the text.
With argument, puts point after and mark before." ()
  (LET ((QREG (GET-REGISTER-NAME "Get text from register:" " containing text")))
    (LET ((THING (GET QREG 'TEXT)))
      (OR THING (BARF "The register ~A does not contain any text." QREG))
      (WITH-UNDO-SAVE ("Get register" (POINT) (POINT) T)
        (MOVE-BP (MARK) (INSERT-INTERVAL (POINT) THING))
        (FIXUP-FONTS-INTERVAL (GET QREG 'TEXT-FONTS) (POINT) (MARK) T))
      (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
      (OR *NUMERIC-ARG-P*
          (SWAP-BPS (POINT) (MARK)))))
  DIS-TEXT)

(DEFCOM COM-PUT-REGISTER "Put text from point to mark into a register.
The register name, a character with no meta bits, is read from the keyboard.
With an argument, the text is also deleted." ()
  (REGION (BP1 BP2)
    (LET ((QREG (GET-REGISTER-NAME "Put text into register:")))
      (PUTPROP QREG (COPY-INTERVAL BP1 BP2 T) 'TEXT)
      (COND (*NUMERIC-ARG-P*
             (DELETE-INTERVAL (POINT) (MARK))
             DIS-TEXT)
            (T DIS-NONE)))))

(DEFCOM COM-VIEW-REGISTER "Display the contents of a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((SYM (GET-REGISTER-NAME "View register:")))
    (FORMAT T "~&REGISTER ~A:" SYM)
    (LET ((BP (CAR (GET SYM 'POINT))))
      (WHEN BP
        (FORMAT T "~& Position: Buffer ~A,~%~8T~A -|- ~A~%"
                (CDR (GET SYM 'POINT))
                (OR (SUMMARIZE-INTERVAL (FORWARD-LINE BP -1 T) BP) "")
                (OR (SUMMARIZE-INTERVAL BP (FORWARD-LINE BP 1 T)) ""))))
    (WHEN (GET SYM 'TEXT)
      (FORMAT T "~& Text:~%~A" (GET SYM 'TEXT)))
    (AND (NULL (PLIST SYM))
         (FORMAT T "~& No text or saved position assigned.")))
  (FORMAT T "~&")
  DIS-NONE)

(DEFUN VIEW-REGISTER (SYM)
  (LET ((TEXT (GET SYM 'TEXT)))
    (FORMAT T "~&~10,5,2A~A~%" SYM
            (IF (NULL TEXT)
                "[EMPTY]"
              (SUMMARIZE-INTERVAL TEXT))))
  (LET* ((BP (CAR (GET SYM 'POINT))))
    (WHEN BP
      (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
        (FORMAT T "~& Position: Buffer ~A,~%~8T~A -|- ~A~%"
                (CDR (GET SYM 'POINT))
                (OR (SUMMARIZE-INTERVAL (FORWARD-LINE BP -1 T) BP) "")
                (OR (SUMMARIZE-INTERVAL BP (FORWARD-LINE BP 1 T)) ""))))))

(DEFCOM COM-LIST-REGISTERS "List and display the contents of all defined registers." ()
  (FORMAT T "List of all registers:")
  (DOLIST (L *Q-REG-LIST*)
    (VIEW-REGISTER L))
  (FORMAT T "Done.")
  DIS-NONE)

(DEFCOM COM-KILL-REGISTER "Kill a register.
The register name, a character with no meta bits, is read from the keyboard." ()
  (LET ((Q-REG (GET-REGISTER-NAME "Kill register:")))
    (COND ((GET Q-REG 'TEXT)
           (SETQ *Q-REG-LIST* (DELQ Q-REG *Q-REG-LIST*))
           (REMPROP Q-REG 'TEXT))
          (T (BARF "The register ~A does not contain anything." Q-REG))))
  DIS-NONE)

(DEFCOM COM-SAVE-POSITION-IN-REGISTER "Save the current point in a register.
The register name, a character with no meta bits, is read from the keyboard." ()
  (LET ((Q-REG (GET-REGISTER-NAME "Point to register:")))
    (SAVE-POSITION-IN-REGISTER Q-REG (POINT)))
  DIS-NONE)

(DEFUN SAVE-POSITION-IN-REGISTER (REGISTER BP)
  (LET ((PT (GET REGISTER 'POINT)))
    (COND (PT
           (MOVE-BP (CAR PT) BP)
           (RPLACD PT (BP-TOP-LEVEL-NODE BP)))
          (T
           (SETQ PT (CONS (COPY-BP BP :NORMAL)  (BP-TOP-LEVEL-NODE BP)))))
    (PUTPROP REGISTER PT 'POINT)))

(DEFCOM COM-JUMP-TO-REGISTER-POSITION "Restore a saved position from a register.
The register name, a character with no meta bits, is read from the keyboard." (KM)
  (LET ((Q-REG (GET-REGISTER-NAME "Register to point:" " containing a location")))
    (LET ((PT (GET Q-REG 'POINT)))
      (WHEN (NULL PT)
        (BARF "The register ~A doesn't point anywhere." Q-REG))
      (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
      (MAKE-BUFFER-CURRENT (CDR PT))
      (MOVE-BP (POINT) (CAR PT))))
  DIS-BPS)

;;;; Completing-reader and other mini-buffer stuff

(DEFCOM COM-END-OF-MINI-BUFFER "Terminate input from the typein line." ()
  (*THROW 'RETURN-FROM-COMMAND-LOOP NIL))

;;; The c-G command in the minibuffer.
(DEFCOM COM-MINI-BUFFER-BEEP "Quit out of the mini buffer.
If there is text in the mini buffer, delete it all.
If the mini buffer is empty, quit out of it." ()
  (BEEP)
  (COND (*NUMERIC-ARG-P* DIS-NONE)
        ((WINDOW-MARK-P *WINDOW*)
         (SETQ *MARK-STAYS* NIL)
         DIS-NONE)
        ((BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
         (*THROW 'NON-MINIBUFFER-LEVEL T))
        (T
         (DELETE-INTERVAL *INTERVAL*)
         DIS-TEXT)))

(DEFCOM COM-YANK-DEFAULT-STRING "Insert the default string into the mini buffer." ()
  (IF (NULL *MINI-BUFFER-DEFAULT-STRING*)
      (BARF "There is no default in this context.")
    (INSERT-MOVING (POINT)
                   (typecase *MINI-BUFFER-DEFAULT-STRING*
                     ((or character string) *mini-buffer-default-string*)
                     (symbol (string *mini-buffer-default-string*))
                     (list (format nil "~S" *mini-buffer-default-string*))
                     (t (format nil "~A" *mini-buffer-default-string*)))))
  DIS-TEXT)

(DEFCOM COM-YANK-SEARCH-STRING "Insert the last search string into the mini buffer." ()
  (IF (NULL *SEARCH-RING*)
      (BARF "No search string is recorded.")
    (INSERT-MOVING (POINT) (CAAR *SEARCH-RING*)))
  DIS-TEXT)

(DEFCOM COM-RECURSIVE-EDIT-ABORT "Quit out of recursive edit right away" ()
  (*THROW 'TOP-LEVEL T))

(DEFUN EDIT-IN-MINI-BUFFER (&OPTIONAL (COMTAB *MINI-BUFFER-COMTAB*)
                                      INITIAL-CONTENTS INITIAL-CHAR-POS MODE-LINE-LIST)
  "Read input using a mini buffer, and return a string.
COMTAB is the comtab to use while in the mini buffer; the default is usually right.
INITIAL-CONTENTS is a string to initialize the mini buffer from
and INITIAL-CHAR-POS is where in that string to start the cursor.
MODE-LINE-LIST is what to display in the mode line during."
  (AND *MINI-BUFFER-COMMAND-IN-PROGRESS*        ;Recursive mini-buffers don't work
       (BARF "Mini-buffer entered recursively"))
;Removed because it can go off if a repeated command uses more
;minibuffers that were recorded, for any reason,
;and then you can no longer keep rotating the minibuffer ring with C-M-Y
;after you get back to the command's first minibuffer.
;  ;; Prevent C-M-Y from being confused if we repeated a command
;  ;; previously but are not repeating one now.
;  (OR *MINI-BUFFER-REPEATED-COMMAND*
;      (SETQ *MINI-BUFFER-ENTIRE-REPEATED-COMMAND* NIL))
  (AND *MINI-BUFFER-REPEATED-COMMAND*
       (POP *MINI-BUFFER-REPEATED-COMMAND* INITIAL-CONTENTS)
       (SETQ INITIAL-CHAR-POS (STRING-LENGTH INITIAL-CONTENTS)))
  (MUST-REDISPLAY *MINI-BUFFER-WINDOW* DIS-TEXT)
  ;; Set up the initial contents of the mini buffer, and discard previous undo records.
  (LET ((*INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
        (*BATCH-UNDO-SAVE* T)
        (BP (WINDOW-POINT *MINI-BUFFER-WINDOW*)))
    (DISCARD-UNDO-INFORMATION *INTERVAL*)
    (DELETE-INTERVAL *INTERVAL*)
    (AND INITIAL-CONTENTS
         (INSERT BP (SETQ INITIAL-CONTENTS (STRING INITIAL-CONTENTS))))
    (AND INITIAL-CHAR-POS
         (MOVE-BP BP (FORWARD-CHAR BP INITIAL-CHAR-POS))))
  (OR *MINI-BUFFER-DONT-RECORD* (RECORD-MINI-BUFFER-VALUE T))
  (PROG* KLUDGE (VAL SUCCESSFUL
                     (TOP-W (SEND (TV:SHEET-SUPERIOR *MINI-BUFFER-WINDOW*)
                                  :TOP-OF-EDITOR-HIERARCHY))
                     (OSUBST (SEND TOP-W :SELECTION-SUBSTITUTE)))
    ;; Prevent a delay when mini buffer window is selected
    (SEND (WINDOW-SHEET *MINI-BUFFER-WINDOW*) :MINI-BUFFER-ENTERED)
    (UNWIND-PROTECT
      (BIND-MODE-LINE MODE-LINE-LIST
        (LET ((*MINI-BUFFER-COMMAND-IN-PROGRESS* *CURRENT-COMMAND*)
              (PACKAGE PACKAGE)
              (*COMTAB* COMTAB)
              (*OUTER-LEVEL-MINI-BUFFER-COMMAND* *MINI-BUFFER-COMMAND*)
              (INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
          ;; Actually do the editing.
          (SETQ VAL (SEND *MINI-BUFFER-WINDOW* :EDIT))
          (SETQ *MINI-BUFFER-COMMAND* *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
          (OR *MINI-BUFFER-DONT-RECORD*
              (RECORD-MINI-BUFFER-VALUE NIL (STRING-INTERVAL INTERVAL)))
          ;; If we are repeating a command, re-input any non-mini-buffer
          ;; characters that followed this mini-buffer.
          (DOLIST (CHAR (REVERSE (CAR *MINI-BUFFER-REPEATED-COMMAND*)))
            (SEND *TERMINAL-IO* :PUSH-INPUT CHAR))
          (POP *MINI-BUFFER-REPEATED-COMMAND*)
          (SETQ SUCCESSFUL T)
          (RETURN-FROM KLUDGE (VALUES VAL *MINI-BUFFER-WINDOW* INTERVAL))))
;If we quit out of a minibuffer, tell (:method editor :edit)
;not to push this command on the ring.
;Likewise if this is being run from a mouse click, since then
;*CURRENT-COMMAND* is NIL and we could not reexecute this properly.
      (OR (AND *CURRENT-COMMAND* SUCCESSFUL) (SETQ *MINI-BUFFER-COMMAND* NIL))
      ;; Don't reselect a typeout window that has been popped down.
      (IF (AND OSUBST (NOT (MEMQ OSUBST (TV:SHEET-INFERIORS (TV:SHEET-SUPERIOR OSUBST)))))
          (SETQ OSUBST (TV:SHEET-SUPERIOR OSUBST)))
      (SEND TOP-W :SET-SELECTION-SUBSTITUTE OSUBST)
      (UNLESS (EQ *WINDOW* *MINI-BUFFER-WINDOW*)
        (DISAPPEAR-MINI-BUFFER-WINDOW)))))

(DEFUN DISAPPEAR-MINI-BUFFER-WINDOW ()
  ;; Turn off the mini buffer's blinkers, so that
  ;; when it appears next time, the blinkers do not initially appear
  ;; in the old, no longer correct positions.
;  (TV:BLINKER-SET-DESELECTED-VISIBILITY (WINDOW-POINT-BLINKER *MINI-BUFFER-WINDOW*) NIL)
;  (DO L (WINDOW-SPECIAL-BLINKER-LIST *MINI-BUFFER-WINDOW*) (CDR L) (NULL L)
;    (TV:BLINKER-SET-VISIBILITY (CDAR L) NIL))
;;Is this needed at all?  I think everything wins anyway.
;  ;; If typein-window is already exposed,
;  ;; that caused the whole zwei window to be deselected,
;  ;; so must reselect it now that its substitute is not the mini buffer.
;  (IF (TV:SHEET-EXPOSED-P *TYPEIN-WINDOW*)
;      (SEND (SEND *WINDOW* :TOP-OF-EDITOR-HIERARCHY) :SET-SELECTION-SUBSTITUTE *WINDOW*))
  ;; Bring the typein window back, and hence get rid of the mini-buffer
  (SEND *TYPEIN-WINDOW* :EXPOSE)
  (SEND (WINDOW-SHEET *MINI-BUFFER-WINDOW*) :DEACTIVATE)
  (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW))

;;; Record a string returned by a mini buffer as part of the sequence
;;; of arguments of the current command.
;;; With no string, records all characters typed in since the last
;;; mini buffer was exited.
(DEFUN RECORD-MINI-BUFFER-VALUE (START-OK &OPTIONAL STRING)
  (OR *MINI-BUFFER-COMMAND*
      (IF START-OK
          (SETQ *MINI-BUFFER-COMMAND*
                `((,*CURRENT-COMMAND*
                   ,*NUMERIC-ARG-P* ,*NUMERIC-ARG*
                   . ,(CURRENT-PROMPTS))))
        (FERROR NIL "No mini-buffer command is in progress.")))
  (LET* ((RECORD (SEND *STANDARD-INPUT* :PLAYBACK))
         (LEN (ARRAY-LENGTH RECORD))
         (RECORD-POINTER (\ (1+ (TV:IO-BUFFER-RECORD-POINTER RECORD)) LEN)))
    (IF STRING
        (SETQ *MINI-BUFFER-END-POINTER* RECORD-POINTER)
      (IF (CDR *MINI-BUFFER-COMMAND*)
          (DO ((PTR *MINI-BUFFER-END-POINTER*
                    (\ (1+ PTR) LEN))
               CHARS)
              ((= PTR RECORD-POINTER)
               (RETURN (RPLACA (LAST *MINI-BUFFER-COMMAND*) (NREVERSE CHARS))))
;charcter lossage?
            (IF (NUMBERP (AREF RECORD PTR))
                (PUSH (AREF RECORD PTR) CHARS))))))
  (IF STRING (RPLACD (LAST *MINI-BUFFER-COMMAND*) (LIST STRING NIL))))

;;;; The mini buffer command history.

;;; The top level loop pushes *MINI-BUFFER-COMMAND* onto this history list.

(DEFUN MINI-BUFFER-RING-PUSH (THING)
  "Push THING on the ring of saved mini buffers."
  (PUSH-ON-HISTORY THING *MINI-BUFFER-HISTORY*))

(DEFUN SUMMARIZE-MINI-BUFFER-COMMAND (COMMAND-DESC)
  (LET ((ARG-P (CADAR COMMAND-DESC))
        (ARG (CADDAR COMMAND-DESC))
        (PROMPTS (CDDDAR COMMAND-DESC))
        (COMMAND (CAAR COMMAND-DESC))
        (CONTENTS (CDR COMMAND-DESC))
        STR)
    (IF PROMPTS
        (SETQ STR
              (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
                (DOLIST (PROMPT PROMPTS)
                  (IF (STRINGP PROMPT)
                      (PRINC PROMPT)
                    (FORMAT:OCHAR PROMPT :EDITOR))
                  (WRITE-CHAR #/SP))))
      (SETQ STR (OR (KEY-FOR-COMMAND COMMAND)
                    (GET COMMAND 'COMMAND-NAME)))
      (AND ARG-P
           (SETQ STR (STRING-APPEND (FORMAT-ARGUMENT ARG-P ARG) #/SPACE STR)))
      (SETQ STR (STRING-APPEND STR #/SPACE)))
    (DO ((CONTENTS CONTENTS (CDDR CONTENTS)))
        ((NULL CONTENTS))
      (LET ((STRING (CAR CONTENTS))
            (CHARS (CADR CONTENTS)))
        (SETQ STR (STRING-APPEND STR STRING #/SPACE))
        (DOLIST (CHAR CHARS)
          (OR (CONSP CHAR)
              (SETQ STR (STRING-APPEND STR
                                       (FORMAT:OUTPUT NIL
                                         (FORMAT:OCHAR CHAR :EDITOR))
                                       #/SPACE))))))
    STR))


;;;; Re-executing mini buffer commands.

(DEFCOM COM-REPEAT-MINI-BUFFER-COMMAND "Repeat a recent mini-buffer command.
A numeric argument does the nth previous one.
An argument of 0 lists which ones are remembered." ()
  (HISTORY-YANK *MINI-BUFFER-HISTORY*))

;;; This is the YANK-METHOD for the *MINI-BUFFER-HISTORY*
(DEFUN MINI-BUFFER-HISTORY-YANK (COMMAND &OPTIONAL KILL-PREVIOUS LEAVE-POINT-BEFORE)
  KILL-PREVIOUS LEAVE-POINT-BEFORE
  (UNLESS COMMAND (BEEP))
  (APPLY 'MUST-REDISPLAY *WINDOW*
                         (MULTIPLE-VALUE-LIST (RE-EXECUTE-MINI-BUFFER-COMMAND COMMAND))))

(DEFCOM COM-POP-MINI-BUFFER-HISTORY "Back up to previous minibuffer.
Backs up to the previous minibuffer argument of this command.
If this is the first minibuffer argument in this command,
backs up to the last saved minibuffer command,
and then to the minibuffer command before that one, and so on." ()
  (LET ((COMMAND
          ;; If not the first minibuffer in this command,
          ;; back up to previous minibuffer in same command.
          (IF (CDR *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
              (PROGN (SETF (CAR (LAST *OUTER-LEVEL-MINI-BUFFER-COMMAND*)) NIL)
                     *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
            ;; Otherwise use the last command in the ring,
            ;; unless we are already using that one.
            ;; In that case, rotate it to the back and use the next one.
            (ROTATE-HISTORY-YANK-POINTER *MINI-BUFFER-HISTORY* *NUMERIC-ARG*))))
    ;; Setup to repeat the one before this
    (SEND *STANDARD-INPUT* :UNTYI `(:EXECUTE POP-RING-RE-EXECUTE-MINI-BUFFER-COMMAND
                                             ,COMMAND)))
  (*THROW 'TOP-LEVEL T))

;;; M-sh-Y pushes a blip that makes the command loop call this funtion.
;;; NIL as value flushes typeout.
;;; Must bind *MINI-BUFFER-DONT-RECORD* to NIL since the :EXECUTE mechanism binds it to T.
(DEFUN POP-RING-RE-EXECUTE-MINI-BUFFER-COMMAND (MINI-BUFFER-COMMAND)
  (LET ((*MINI-BUFFER-DONT-RECORD* NIL))
    (LET ((VALUES (MULTIPLE-VALUE-LIST (RE-EXECUTE-MINI-BUFFER-COMMAND MINI-BUFFER-COMMAND))))
      (APPLY 'MUST-REDISPLAY *WINDOW* VALUES)))
  ;; Return NIL so typeout gets flushed.
  NIL)

;;; This is called from the top level command loop
;;; and does the actual work of reexecuting the command.
(DEFUN RE-EXECUTE-MINI-BUFFER-COMMAND (*MINI-BUFFER-REPEATED-COMMAND*
                                       &AUX *MINI-BUFFER-ENTIRE-REPEATED-COMMAND*
                                       PROMPTS)
  "Re-execute a mini-buffer command (such as an element of *MINI-BUFFER-HISTORY*)."
  (SETQ *MINI-BUFFER-ENTIRE-REPEATED-COMMAND*
        *MINI-BUFFER-REPEATED-COMMAND*)
  (OR *MINI-BUFFER-REPEATED-COMMAND* (BARF "No previous command"))
  (POP *MINI-BUFFER-REPEATED-COMMAND* `(,*CURRENT-COMMAND* ,*NUMERIC-ARG-P*
                                        ,*NUMERIC-ARG* . ,PROMPTS))
  (OR *MINI-BUFFER-COMMAND*
      (SETQ *MINI-BUFFER-COMMAND*
            `((,*CURRENT-COMMAND*
               ,*NUMERIC-ARG-P* ,*NUMERIC-ARG*
               . ,PROMPTS))))
  (FUNCALL *CURRENT-COMMAND*))

(DEFVAR *COMPLETING-ALIST*)
(DEFVAR *COMPLETING-IMPOSSIBLE-IS-OK-P*)
(DEFVAR *COMPLETING-HELP-MESSAGE*)
(DEFVAR *COMPLETING-DOCUMENTER*)
;character lossage
(DEFVAR *COMPLETING-DELIMS* '(#/SP #/-))

(DEFUN COMPLETING-READ-FROM-MINI-BUFFER (PROMPT *COMPLETING-ALIST*
                                         &OPTIONAL
                                         *COMPLETING-IMPOSSIBLE-IS-OK-P* INITIAL-COMPLETE
                                         *COMPLETING-HELP-MESSAGE* *COMPLETING-DOCUMENTER*
                                         &AUX CONTENTS CHAR-POS)
  "Read a string from the mini buffer with completion.
PROMPT is displayed in the mode line.
*COMPLETING-ALIST* is an alist, or sorted array of pairs, to complete from.
*COMPLETING-IMPOSSIBLE-IS-OK-P* non-NIL means allow inputs
 that are not on the alist and don't complete.
INITIAL-COMPLETE if non-NIL is a string to start out with,
 or T meaning try completing the empty string.
*COMPLETING-HELP-MESSAGE* is a string to print if Help is typed, etc.
*COMPLETING-DOCUMENTER* is a function which, given an alist element,
 prints documentation of its meaning on *STANDARD-OUTPUT*.

The value is an element of the alist, or just a string
 (if it was an impossible completion).
 The null string is always a possible value."
  (AND INITIAL-COMPLETE
       (MULTIPLE-VALUE (CONTENTS NIL NIL NIL CHAR-POS)
         (COMPLETE-STRING (IF (EQ INITIAL-COMPLETE T) ""
                            INITIAL-COMPLETE)
                          *COMPLETING-ALIST* *COMPLETING-DELIMS* T
                          (LENGTH (IF (EQ INITIAL-COMPLETE T) ""
                                    INITIAL-COMPLETE)))))
  (EDIT-IN-MINI-BUFFER *COMPLETING-READER-COMTAB* CONTENTS CHAR-POS
                       (IF PROMPT `(,PROMPT (:RIGHT-FLUSH " (Completion)"))
                         '(:RIGHT-FLUSH "(Completion)"))))

;; Note that WINDOW is a window system type window, not a ZWEI-WINDOW
(DEFUN COMPLETING-READ (WINDOW *COMPLETING-ALIST*
                        &OPTIONAL PROMPT *COMPLETING-IMPOSSIBLE-IS-OK-P*
                        INITIAL-COMPLETE *COMPLETING-HELP-MESSAGE*
                        *COMPLETING-DOCUMENTER*
                        &AUX ZWEI-WINDOW CONTENTS CHAR-POS)
  "Read a string with completion using the specified editor window.
WINDOW should be a window instance, not a defstruct.
The remaining args are as for COMPLETING-READ-FROM-MINI-BUFFER.
You must make WINDOW's ZWEI-WINDOW contain an editor closure
made from TOP-LEVEL-EDITOR-CLOSURE-VARIABLES, and made with
*COMTAB* equal to *COMPLETING-READER-COMTAB*."
  (AND INITIAL-COMPLETE
       (MULTIPLE-VALUE (CONTENTS NIL NIL NIL CHAR-POS)
         (COMPLETE-STRING "" *COMPLETING-ALIST* *COMPLETING-DELIMS* T 0)))
  (AND PROMPT (SEND WINDOW :SET-LABEL PROMPT))
  (SETQ ZWEI-WINDOW (SEND WINDOW :ZWEI-WINDOW))
  (LET ((INTERVAL (WINDOW-INTERVAL ZWEI-WINDOW)))
    (IF INTERVAL (DELETE-INTERVAL INTERVAL)
      (SEND ZWEI-WINDOW :SET-INTERVAL (CREATE-INTERVAL NIL NIL T))))
  (SETF (WINDOW-REDISPLAY-DEGREE ZWEI-WINDOW) DIS-ALL)
  (AND CONTENTS (NOT (EQUAL CONTENTS ""))
       (LET ((*INTERVAL* (WINDOW-INTERVAL ZWEI-WINDOW))
             (BP (WINDOW-POINT ZWEI-WINDOW)))
         (INSERT BP CONTENTS)
         (AND CHAR-POS (MOVE-BP BP (FORWARD-CHAR BP CHAR-POS)))))
  (LET ((OLD-STATUS (SEND WINDOW :STATUS)))
    (UNWIND-PROTECT
      (TV:WINDOW-CALL (WINDOW)
        (SEND ZWEI-WINDOW :EDIT))
      (SEND WINDOW :SET-STATUS OLD-STATUS))))

(DEFCOM COM-COMPLETE "Attempt to complete the current line." ()
  (OR (COMPLETE-LINE T T) (BEEP 'NO-COMPLETION))
  DIS-TEXT)

(DEFCOM COM-SELF-INSERT-AND-COMPLETE "Attempt to complete after inserting break character." ()
  (OR (END-LINE-P (POINT)) (INSERT-MOVING (POINT) *LAST-COMMAND-CHAR*))
  (OR (COMPLETE-LINE NIL NIL *LAST-COMMAND-CHAR*)
      *COMPLETING-IMPOSSIBLE-IS-OK-P*
      (BEEP 'NO-COMPLETION))
  DIS-TEXT)

(DEFCOM COM-COMPLETE-AND-EXIT "Attempt to complete and return if unique." ()
  (COM-COMPLETE-AND-EXIT-1 NIL))

(DEFUN COM-COMPLETE-AND-EXIT-1 (EXPLICIT-P)
  (PROG ((LINE (BP-LINE (WINDOW-START-BP *WINDOW*)))
         COMPLETION VAL
         (IMPOSSIBLE-OK (AND (NOT EXPLICIT-P) *COMPLETING-IMPOSSIBLE-IS-OK-P*)))
    (SETQ VAL (COND ((ZEROP (LINE-LENGTH LINE)) ;Allow typing just CR
                     "")
                    ((NOT IMPOSSIBLE-OK) ;Not allowed to type new things,
                     (SETQ COMPLETION (COMPLETE-LINE T NIL NIL (NOT EXPLICIT-P)))
                     (COND ((NULL COMPLETION)
                            ;; No completions at all.
                            (BEEP 'NO-COMPLETION)
                            (RETURN NIL))
                           ((NULL (CDR COMPLETION))
                            ;; It is unique.
                            (SETQ VAL (CAR COMPLETION)))
                           ((NULL (SETQ VAL (SYS:ASSOC-EQUALP LINE COMPLETION)))
                            ;; Multiple completions, and no exact match.
                            (RETURN NIL)))
                     ;; Completes uniquely somehow.
                     (MUST-REDISPLAY *WINDOW* DIS-TEXT) ;Typed something good
                     (AND (WINDOW-READY-P *WINDOW*) (REDISPLAY *WINDOW* :NONE))
                     VAL)
                    ((AND (EQ IMPOSSIBLE-OK 'MAYBE)
                          ;; If allowed one failure
                          (NEQ *LAST-COMMAND-TYPE* 'FAILING-COMPLETION)
                          (TYPEP *LAST-COMMAND-CHAR* '(OR NUMBER CHARACTER))
                          (ZEROP (CHAR-BITS (COMTAB-CHAR-INDIRECTION *LAST-COMMAND-CHAR*))))
                     (SETQ COMPLETION (COMPLETE-LINE T NIL NIL (NOT EXPLICIT-P)))
                     (SETQ COMPLETION (IF (= (LENGTH COMPLETION) 1) (CAR COMPLETION)
                                        (SYS:ASSOC-EQUALP LINE COMPLETION)))
                     (COND ((NULL COMPLETION)   ;This is no good
                            (SETQ *CURRENT-COMMAND-TYPE* 'FAILING-COMPLETION)
                            (BEEP 'NO-COMPLETION)
                            (RETURN NIL))
                           (T
                            (MUST-REDISPLAY *WINDOW* DIS-TEXT)
                            (AND (WINDOW-READY-P *WINDOW*) (REDISPLAY *WINDOW* :NONE))
                            COMPLETION)))
                    ((AND (NEQ IMPOSSIBLE-OK 'ALWAYS-STRING)
                          (SETQ COMPLETION (ASS 'STRING-EQUAL LINE
                                                (IF (ARRAYP *COMPLETING-ALIST*)
                                                    (G-L-P *COMPLETING-ALIST*)
                                                  *COMPLETING-ALIST*))))
                     COMPLETION)
                    (T
                     (STRING-APPEND LINE))))
    (*THROW 'RETURN-FROM-COMMAND-LOOP VAL))
  DIS-TEXT)

(DEFCOM COM-COMPLETE-AND-EXIT-IF-UNIQUE "Attempt to complete and return only if unique." ()
  (COM-COMPLETE-AND-EXIT-1 T))

(DEFCOM COM-LIST-COMPLETIONS "Give a menu of possible completions for string so far." ()
  (LET (POSS)
    (MULTIPLE-VALUE (NIL POSS)
      (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*))
    (OR POSS (BARF))
    (AND *COMPLETING-HELP-MESSAGE* (FORMAT T "~&~A" *COMPLETING-HELP-MESSAGE*))
    (LIST-COMPLETIONS-INTERNAL POSS))
  DIS-NONE)

(DEFUN LIST-COMPLETIONS-INTERNAL (POSS &AUX LEN)
  (SETQ LEN (LENGTH POSS))
  (COND ((ZEROP LEN)
         (FORMAT T
                 "~&There are no possible completions of the text you have typed.~%"))
        ((= LEN 1)
         (FORMAT T
                 "~&The only possible completion of the text you have typed is ")
         (SEND *STANDARD-OUTPUT* :ITEM 'COMPLETION (CAAR POSS))
         (FORMAT T ":~%")
         (COND (*COMPLETING-DOCUMENTER*
                (TERPRI *STANDARD-OUTPUT*)
                (FUNCALL *COMPLETING-DOCUMENTER* (CAR POSS)))))
        ((OR (< LEN 50.)
             (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
               (FQUERY NIL "There are ~D possibilities, do you really want to see them all? "
                       LEN)))
         (FORMAT T "~&These are the possible completions of the text you have typed:~2%")
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'COMPLETION
               (SORT (MAPCAR #'CAR POSS) #'STRING-LESSP))
         (TERPRI *STANDARD-OUTPUT*))))

(DEFCOM COM-COMPLETION-APROPOS "Do apropos within the completions of what has been typed." ()
  (LET ((LINE (BP-LINE (POINT)))
        FUNCTION)
    (LET (IDX)
      (IF (SETQ IDX (STRING-SEARCH-SET *COMPLETING-DELIMS* LINE))
          (SETQ LINE (DO ((I 0)
                          (J IDX)
                          (LIST))
                         (NIL)
                       (OR (= (OR J (STRING-LENGTH LINE)) I)
                           (PUSH (SUBSTRING LINE I J) LIST))
                       (OR J
                           (RETURN (NREVERSE LIST)))
                       (SETQ I (1+ J)
                             J (STRING-SEARCH-SET *COMPLETING-DELIMS* LINE I)))
                FUNCTION 'FSM-STRING-SEARCH)
        (SETQ FUNCTION 'STRING-SEARCH)))
    (AND *COMPLETING-HELP-MESSAGE*
         (FORMAT T "~&~A" *COMPLETING-HELP-MESSAGE*))
    (FORMAT T
            "~&These are the completions matching~:[ /"~A/"~;~{ /"~A/"~^ or~}~]:"
            (CONSP LINE) LINE)
    (AND (CONSP LINE)
         (SETQ LINE (LIST LINE NIL NIL)))
    (DO ((ALIST (IF (ARRAYP *COMPLETING-ALIST*) (G-L-P *COMPLETING-ALIST*)
                  *COMPLETING-ALIST*)
                (CDR ALIST))
         (POSS NIL))
        ((NULL ALIST)
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'COMPLETION
               (SORT (MAPCAR #'CAR POSS) #'STRING-LESSP)))
      (DO NIL ((CONSP ALIST)) (SETQ ALIST (CAR ALIST))) ;Indirect through multiple alists
      (AND (FUNCALL FUNCTION LINE (CAAR ALIST))
           (PUSH (CAR ALIST) POSS))))
  (TERPRI *STANDARD-OUTPUT*)
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* COMPLETION "Select" SELECT-COMPLETION T
                          "Use this completion.")

;Called if the user mouses one of the completions
(DEFUN SELECT-COMPLETION (STRING)
  (OR (EQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)) (BARF))
  (OR (VARIABLE-BOUNDP *COMPLETING-ALIST*) (BARF))
  (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)       ;Only one completion can be meaningful
  (DELETE-INTERVAL *INTERVAL*)
  (INSERT-MOVING (POINT) STRING)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COM-COMPLETE-AND-EXIT))

;;; This command is on the HELP key when the user is in the completing reader.
;;; The caller of the completing reader can pass this two implicit arguments
;;; through the specal variables *COMPLETING-HELP-MESSAGE* and *COMPLETING-DOCUMENTER*.
;;; The command first prints the value of *COMPLETING-HELP-MESSAGE*, if non-NIL;
;;; otherwise it prints "You are in the completing reader."  The top-level value
;;; of this variable is NIL.  Then it explains how completion works, and tells
;;; the user what options he can complete to.  If there is only one option,
;;; and *COMPLETING-DOCUMENTER* is non-NIL, then *COMPLETING-DOCUMENTER* is
;;; applied to the one element of the ALIST that the user is indicating;
;;; the function should output helpful cruft to *STANDARD-OUTPUT*.
(DEFCOM COM-DOCUMENT-COMPLETING-READ "Explain how the completing reader works.
Also tell you what you are currently doing." ()
  (LET (POSS)
    (FORMAT T "~&~A"
            (OR *COMPLETING-HELP-MESSAGE* "You are in the completing reader."))
    (unless *inhibit-explanation-of-completion*
      (progn
        (FORMAT T
                "~%~%You are typing to a mini-buffer, with the following commands redefined:
Altmode causes as much of the string as can be determined to be inserted
into the mini-buffer (this is called command completion).  Space and )
are similar; they complete up to the next Space or ) respectively.
")
        (FORMAT T
                (IF (CDR *OUTER-LEVEL-MINI-BUFFER-COMMAND*)
                    "
Use Control-Meta-Y to go back to editing the previous argument if you
are no longer satisfied with what you typed.
"
                  "
Use Control-Meta-Y to cancel this command and begin to re-execute
the previous command that read mini-buffer arguments.
"))
        (FORMAT T "
Control-? lists all the strings that complete what you have typed so far,
without the rest of this HELP display.  Control-// lists all the strings
that contain what you have typed anywhere within them.

End will complete as much as possible and return if that gives a unique result.
Return will complete as much as possible, and ")
        (FORMAT T
                (IF *COMPLETING-IMPOSSIBLE-IS-OK-P*
                    "return the result."
                  "if that is a valid string it
will return it."))))
    (MULTIPLE-VALUE (NIL POSS)
      (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*))
    (TERPRI) (TERPRI)
    (LIST-COMPLETIONS-INTERNAL POSS))
  DIS-NONE)

(DEFUN COMPLETE-LINE (FORWARD-OK IGNORE &OPTIONAL INSERT
                      IGNORE-TRAILING-SPACE
                      &AUX NSTR POSS WINP LINE POINT
                      CHAR-POS EOLP MAGIC-POS)
  (SETQ POINT (POINT))
  (SETQ LINE (BP-LINE POINT)
        CHAR-POS (BP-INDEX POINT))
  (SETQ EOLP (= CHAR-POS (LINE-LENGTH LINE)))
  (MULTIPLE-VALUE (NSTR POSS WINP CHAR-POS MAGIC-POS)
    (COMPLETE-STRING LINE *COMPLETING-ALIST* *COMPLETING-DELIMS* T CHAR-POS
                     (NOT FORWARD-OK) IGNORE-TRAILING-SPACE))
  (AND MAGIC-POS FORWARD-OK
       (SETQ CHAR-POS MAGIC-POS))
  (COND (POSS
         (DELETE-INTERVAL (BEG-LINE POINT) (END-LINE POINT))
         (INSERT-MOVING POINT NSTR)))
  ;; Insert the given character, unless we have fully completed only one completion.
  ;; But don't insert spaces at beginning of line.
  (AND INSERT EOLP (OR (NEQ WINP 'NOSPACE)
                       (AND (SYS:ASSOC-EQUALP LINE POSS) (NOT (NULL (CDR POSS)))))
       (OR ( INSERT #/SPACE)
           (NOT (BEG-LINE-P POINT)))
       (INSERT-MOVING POINT INSERT))
  (COND (WINP)
        (FORWARD-OK
         (COND (MAGIC-POS
                (MOVE-BP POINT LINE MAGIC-POS)))))
  POSS)

(DEFUN COMPLETE-STRING (STRING ALIST DELIMS &OPTIONAL DONT-NEED-LIST CHAR-POS TRUNC
                                                      IGNORE-TRAILING-SPACE
                        &AUX NCHUNKS CHUNKS CHUNK-DELIMS FILLS CHAMB TEMS RETS
                             RCHUNKS TEM LEN COMPLETED-P CHAR-CHUNK CHAR-OFFSET MAGIC-POS
                             TAIL ONE-BEFORE-TAIL TEMSTRING)
  "Complete a given STRING from an ALIST of strings.
DELIMS is a list of delimiter characters that delimit chunks.
 Each chunk is matched against the chunks of strings in the ALIST.
DONT-NEED-LIST says we don't want all the possibilities, just not NIL.
CHAR-POS is position in STRING to be relocated with new things inserted.
TRUNC says don't complete more than one chunk at end.
IGNORE-TRAILING-SPACE non-NIL says ignore a trailing space character if any.
/
Returns multiple values:
 0) NEWSTRING - the (first) matching completion string.
 1) COMPLETIONS - list of matching completions, a subset of ALIST.
 2) COMPLETED-P - non-NIL if some completion was done;
    value is 'ZWEI:NOSPACE if proper delimiter is already at end of string.
 3) CHAR-POS - the new character position in STRING with new things inserted.
 4) MAGIC-POS - location of first point of ambiguity.
/
For efficiency, if ALIST is an ART-Q-LIST array, it is assumed to be alphabetically
sorted."
  (declare (values newstring completions completed-p char-pos magic-pos))
  (SETQ CHUNKS (MAKE-ARRAY 20. :FILL-POINTER 0)
        CHUNK-DELIMS (MAKE-ARRAY 20. :FILL-POINTER 0))
  (SETQ LEN (STRING-LENGTH STRING))
  (AND IGNORE-TRAILING-SPACE
       (> LEN 0)
       (= (AREF STRING (1- LEN)) #/SP)
       (DECF LEN))
  (DO ((I 0 (1+ I))
       (J 0))
      ((> I LEN))
    (when (if (= I LEN)
              (SETQ TEM -1)             ;Last character delimits a chunk unless it is empty.
            (lisp:member (setq tem (char string i)) delims :test #'char-equal))
      (AND CHAR-POS (> CHAR-POS J)      ;Keep track of relative position
           (SETQ CHAR-CHUNK (ARRAY-LEADER CHUNKS 0)
                 CHAR-OFFSET (- CHAR-POS J)))
      (VECTOR-PUSH-EXTEND (NSUBSTRING STRING J I) CHUNKS)
      (VECTOR-PUSH-EXTEND TEM CHUNK-DELIMS)
      (SETQ J I)))
  (SETQ NCHUNKS (ARRAY-ACTIVE-LENGTH CHUNKS)
        FILLS (MAKE-ARRAY NCHUNKS)
        TEMS (MAKE-ARRAY NCHUNKS)
        RCHUNKS (MAKE-ARRAY NCHUNKS)
        CHAMB (MAKE-ARRAY NCHUNKS :TYPE 'ART-1B))
  (AND (ARRAYP ALIST)
       (MULTIPLE-VALUE (ALIST TAIL ONE-BEFORE-TAIL)
         (COMPLETE-STRING-BOUNDS ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS)))
  (AND ONE-BEFORE-TAIL
       (N-CHUNKS-MATCH-P (CAAR ALIST) (CAAR ONE-BEFORE-TAIL) NCHUNKS DELIMS)
       ;; The first and last possibilities are the same, for as many chunks as we need,
       ;; so all in between must also be the same.
       DONT-NEED-LIST
       ;; So if we don't need all the possibilities,
       ;; keep just the first one and the last one.
       (SETQ ALIST (LIST (CAR ALIST) (CAR ONE-BEFORE-TAIL))
             TAIL NIL))
  (DO ((L ALIST (CDR L))
       (ALL-AMBIG))
      ((EQ L TAIL))
    (COND ((ATOM L))                            ;Indirect through multiple alists
          ((NULL (COMPLETE-CHUNK-COMPARE (CAAR L) DELIMS NCHUNKS CHUNKS CHUNK-DELIMS TEMS
                                         (AND (NULL RETS) RCHUNKS)))
           (OR RETS (SETQ CHUNKS RCHUNKS))      ;First winner determines case of result
           (PUSH (CAR L) RETS)          ;add to list of partial matches
           (SETQ ALL-AMBIG DONT-NEED-LIST)
           (DO ((I 0 (1+ I))
                (FILL))
               (( I NCHUNKS))
             (SETQ TEM (AREF TEMS I)
                   FILL (AREF FILLS I))
             (COND ((NULL FILL)                 ;First one to complete a chunk
                    (SETF (AREF FILLS I) TEM)   ;save for later ones
                    (AND (PLUSP (STRING-LENGTH TEM))
                         (SETQ ALL-AMBIG NIL))) ;This chunk not ambiguous yet
                   ((AND (EQUAL FILL "")
                         (ZEROP (AREF CHAMB I))
                         (NOT (EQUAL TEM "")))
                    ;; If there was an exact match found for this chunk,
                    ;; ignore everything that is NOT an exact match in this chunk.
                    (SETQ ALL-AMBIG NIL)
                    (RETURN NIL))
                   ((AND (EQUAL TEM "")
                         (NOT (AND (EQUAL FILL "")
                                   (ZEROP (AREF CHAMB I)))))
                    ;; The first time we find an exact match for this chunk,
                    ;; from now on consider only exact matches for it,
                    ;; and forget anything we concluded about later chunks
                    ;; from completions that were inexact in this chunk.
                    (SETF (AREF FILLS I) "")
                    (SETF (AREF CHAMB I) 0)
                    (DO ((I (1+ I) (1+ I))) ((= I NCHUNKS))
                      (SETF (AREF FILLS I) NIL)
                      (SETF (AREF CHAMB I) 0))
                    (SETQ ALL-AMBIG NIL))
                   (T
                    (SETQ LEN (STRING-LENGTH FILL))
                    (DO ((J 0 (1+ J))
                         (LEN1 (STRING-LENGTH TEM)))
                        (( J LEN)
                         (OR (ZEROP LEN)
                             (AND (= I (1- NCHUNKS))
                                  (= LEN 1)
                                  (MEM '= (AREF FILL 0) DELIMS))
                             (SETQ ALL-AMBIG NIL)))
                      (WHEN (OR ( J LEN1)
                                (NOT (CHAR-EQUAL (AREF FILL J) (AREF TEM J))))
                        ;; Not the same completion, shorten final version
                        (ASET (NSUBSTRING FILL 0 J) FILLS I)
                        (SETF (AREF CHAMB I) 1) ;Remember this was ambiguous
                        (OR (ZEROP J) (SETQ ALL-AMBIG NIL))
                        (RETURN NIL))))))
           ;;If not going to complete and don't need actual list, finish up now.
           (AND ALL-AMBIG (NULL (AREF FILLS (1- NCHUNKS))) (RETURN NIL)))))
  (WHEN (AND TRUNC (SETQ TEMSTRING (AREF FILLS (1- NCHUNKS))))
    (SETQ LEN (STRING-LENGTH TEMSTRING))
    (AND (ZEROP (AREF CHAMB (1- NCHUNKS)))      ;If last chunk wasn't ambigous,
         (SETQ TRUNC 'NOSPACE))                 ;shouldn't have delimiter there
    (DO ((I 0 (1+ I)))
        (( I LEN))
      (WHEN (MEM '= (AREF TEMSTRING I) DELIMS)
        (ASET (NSUBSTRING TEMSTRING 0 (1+ I)) FILLS (1- NCHUNKS))
        (SETQ TRUNC 'NOSPACE)                   ;Already gave a delimiter
        (RETURN NIL))))
  (SETQ TEMSTRING "")
  (DO ((I 0 (1+ I)))
      (( I NCHUNKS))
    (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)   ;In case inside chunk not completed,
         (SETQ CHAR-POS (+ (STRING-LENGTH TEMSTRING) CHAR-OFFSET)))     ;relocate
    (SETQ TEMSTRING (STRING-APPEND TEMSTRING (AREF CHUNKS I)))
    (WHEN (AND (SETQ TEM (AREF FILLS I)) (> (STRING-LENGTH TEM) 0))
      (SETQ TEMSTRING (STRING-APPEND TEMSTRING TEM)
            COMPLETED-P T)
      (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK) ;If inside completed chunk,
           (SETQ CHAR-POS (STRING-LENGTH TEMSTRING))))  ;move to end of it
    (OR MAGIC-POS (ZEROP (AREF CHAMB I))                ;Remember end of leftmost ambigous chunk
        (SETQ MAGIC-POS (STRING-LENGTH TEMSTRING))))
  (AND COMPLETED-P (EQ TRUNC 'NOSPACE)
       (SETQ COMPLETED-P 'NOSPACE))
  (WHEN (OR (AND (ARRAY-HAS-LEADER-P TEMSTRING)
                 (MINUSP (FILL-POINTER TEMSTRING)))
            (AND CHAR-POS (MINUSP CHAR-POS))
            (AND MAGIC-POS (MINUSP MAGIC-POS)))
    (FERROR "Internal error in completion.  Report a bug."))
  (VALUES TEMSTRING (NREVERSE RETS) COMPLETED-P CHAR-POS MAGIC-POS))

;;;Compare a STR with the given chunks and return NIL if it is a possible completion,
;;;else LESS or GREATER according as it is less or greater than the CHUNKS.
;;;T is returned for the indeterminate case, for the sake of the binary search in the
;;;array case.  The actual completer only checks NULL.
;;;If no ordering is found in the first NCHUNKS-FOR-ORDERING chunks,
;;;the value is always T unless we have an exact match.
(DEFUN COMPLETE-CHUNK-COMPARE (STR DELIMS NCHUNKS CHUNKS CHUNK-DELIMS &OPTIONAL TEMS RCHUNKS
                               (NCHUNKS-FOR-ORDERING 1)
                               &AUX LEN2)
  (SETQ LEN2 (STRING-LENGTH STR))
  (DO ((I 0 (1+ I))
       (J 0)
       (K)
       (LEN1)
       (CHUNK)
       (EXACT-SO-FAR T)
       (DELIM))
      (( I NCHUNKS) NIL)               ;Aligns with each chunk, a winner
    (SETQ CHUNK (AREF CHUNKS I)
          LEN1 (STRING-LENGTH CHUNK))
    (SETQ K (DO ((J1 0 (1+ J1))
                 (K1 J (1+ K1))
                 (CH1)
                 (CH2))
                (( J1 LEN1)
                 (UNLESS (= K1 LEN2)
                   (SETQ EXACT-SO-FAR NIL))
                 K1)
              (AND ( K1 LEN2) (RETURN 'LESS))
              (SETQ CH1 (CHAR-CODE (CHAR-UPCASE (AREF CHUNK J1)))
                    CH2 (CHAR-CODE (CHAR-UPCASE (AREF STR K1))))
              (COND ((= CH1 CH2))
                    ((< CH1 CH2) (RETURN 'GREATER))
                    (T (RETURN 'LESS)))))
    (OR (NUMBERP K)
        ;; If comparison of first chunks yields an ordering, return it.
        ;; Also if ordering is LESS on this chunk, and previous chunks are exact, return it.
        ;; If later chunks don't match, just return T.
        (COND ((< I NCHUNKS-FOR-ORDERING) (RETURN K))
              ((AND EXACT-SO-FAR (EQ K 'LESS)) (RETURN K))
              (T (RETURN T))))
    (AND RCHUNKS (ASET (NSUBSTRING STR J K) RCHUNKS I))
    (COND ((MINUSP (SETQ DELIM (AREF CHUNK-DELIMS I)))
           (SETQ J NIL))                ;For the last chunk, use rest of string
          ((AND (SETQ J (STRING-SEARCH-SET DELIMS STR K LEN2))
                (= DELIM (AREF STR J))))
          (T (RETURN T)))       ;Compare fails if can't find chunk when expected.
    (AND TEMS (SETF (AREF TEMS I) (NSUBSTRING STR K J)))))

;;; Given an ART-Q-LIST array and the chunks to match, compute the subset of that array
;;; that could possibly be a completion of the string, and return an NTHCDR of the G-L-P
;;; and the appropriate tail to stop with.
(DEFUN COMPLETE-STRING-BOUNDS (ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS
                               &OPTIONAL (NCHUNKS-FOR-ORDERING 1)
                               (LO 0) (HIHI (ARRAY-ACTIVE-LENGTH ALIST))
                               (HI LO))
  (DECF LO)
  (DO ((HILO HIHI)
       (IDX)
       (VAL T))
      (NIL)
    (AND (ZEROP (SETQ IDX (TRUNCATE (- HILO LO) 2)))    ;binary search
         (RETURN NIL))
    (SETQ IDX (+ LO IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX)) DELIMS
                                      NCHUNKS CHUNKS CHUNK-DELIMS NIL NIL
                                      NCHUNKS-FOR-ORDERING))
    (COND ((EQ VAL 'LESS)
           (SETQ LO IDX)
           (SETQ HI IDX))
          (T
           (SETQ HILO IDX)
           (COND ((NEQ VAL 'GREATER)
                  (SETQ HI IDX))
                 (T (SETQ HIHI IDX))))))
  (DO ((IDX)
       (VAL))
      (NIL)
    (AND (ZEROP (SETQ IDX (TRUNCATE (- HIHI HI) 2)))
         (RETURN NIL))
    (SETQ IDX (+ HI IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX)) DELIMS
                                      NCHUNKS CHUNKS CHUNK-DELIMS NIL NIL
                                      NCHUNKS-FOR-ORDERING))
    (COND ((NEQ VAL 'GREATER)
           (SETQ HI IDX))
          (T (SETQ HIHI IDX))))
  ;; At this point, HI is the last one not greater,
  ;; but LO is the last one that is less.  Increment LO to exclude that one.
  (INCF LO)
;  ;; I think problems can happen if HI is too close to LO.
;  (WHEN (<= HI (1+ LO))
;    (WHEN (< HI (LENGTH ALIST))
;      (INCF HI)))
  (IF (OR (= LO (LENGTH ALIST)) (< HI LO))
      (VALUES NIL NIL NIL)
    ;; Do the lowest and highest match for all the chunks we considered?
    (IF (AND (> HI (+ LO 2))
             (N-CHUNKS-MATCH-P (CAR (AREF ALIST LO)) (CAR (AREF ALIST HI))
                               NCHUNKS-FOR-ORDERING DELIMS))
        ;; Yes => search again, considering one more chunk,
        ;; and search only the range not yet eliminated.
        (COMPLETE-STRING-BOUNDS ALIST DELIMS NCHUNKS CHUNKS CHUNK-DELIMS
                                (1+ NCHUNKS-FOR-ORDERING) LO (1+ HI))
      (VALUES (%MAKE-POINTER DTP-LIST (ALOC ALIST LO))
              (CDR (%MAKE-POINTER DTP-LIST (ALOC ALIST HI)))
              (%MAKE-POINTER DTP-LIST (ALOC ALIST HI))))))

(DEFUN N-CHUNKS-MATCH-P (STRING1 STRING2 N DELIMS &AUX (POS -1))
  "T if the first N chunks of the two strings are identical."
  (DOTIMES (I N)
    (SETQ POS (STRING-SEARCH-SET DELIMS STRING1 (1+ POS)))
    (UNLESS POS (RETURN NIL)))
  (AND POS
       (PLUSP POS)
       (STRING-EQUAL STRING1 STRING2 :START1 0 :START2 0 :END1 POS :END2 POS)))

(DEFUN SORT-COMPLETION-AARRAY (AARRAY)
  "Sort AARRAY, an ART-Q-LIST array of conses, by the cars of the conses.
If array leader element is T, it means the array is already sorted,
so we do not sort it again."
  (COND ((NOT (ARRAY-LEADER AARRAY 1))  ;If not sorted right now
         (SORT AARRAY (FUNCTION (LAMBDA (X Y)
                                  (STRING-LESSP (CAR X) (CAR Y)))))
         (STORE-ARRAY-LEADER T AARRAY 1))))

(DEFCONST MERGE-COMPLETION-AARRAY-FUDGE 20.
  "Amount of space to leave for insertion of new aarray elements in MERGE-COMPLETION-ARRAY.")

(DEFUN FIND-AARRAY-INSERTION-INDEX (AARRAY STRING)
  "Given an AARRAY, find the index of the element for STRING, or where to insert one.
Assumes that AARRAY is sorted.  Uses a binary search.
AARRAY should be an ART-Q-LIST array of conses whose cars are strings."
  (DO ((LO 0) (HI (ARRAY-ACTIVE-LENGTH AARRAY)))
      ((= LO HI) (RETURN LO))
    (LET ((IDX (LSH (+ LO HI) -1)))
      (COND ((STRING-EQUAL STRING (CAR (AREF AARRAY IDX)))
             (RETURN IDX))
            ((STRING-LESSP STRING (CAR (AREF AARRAY IDX)))
             (SETQ HI IDX))
            ((SETQ LO (1+ IDX)))))))

(DEFUN MERGE-COMPLETION-AARRAY (AARRAY ADDITIONAL-AARRAY &AUX OLD-MAX ADDED-MAX NEW-AARRAY)
  "Merge the elements of ADDITIONAL-AARRAY into AARRAY.
An aarray is an ART-Q-LIST array of conses whose cars are strings.
If AARRAY was sorted, it remains sorted."
  (COND ((ZEROP (SETQ ADDED-MAX (ARRAY-ACTIVE-LENGTH ADDITIONAL-AARRAY)))
         AARRAY)
        ((ZEROP (SETQ OLD-MAX (ARRAY-ACTIVE-LENGTH AARRAY)))
         (SETQ NEW-AARRAY ADDITIONAL-AARRAY)
         (STORE-ARRAY-LEADER T NEW-AARRAY 1)
         (STRUCTURE-FORWARD AARRAY NEW-AARRAY))
        ((AND (ARRAY-LEADER AARRAY 1)
              (< ADDED-MAX 4))
         ;; If number being added is small,  do it by inserting in the old array.
         ;; Make AARRAY big enough to hold all the new elements.
         (IF (> (+ OLD-MAX ADDED-MAX) (ARRAY-LENGTH AARRAY))
             (ADJUST-ARRAY-SIZE AARRAY (+ OLD-MAX ADDED-MAX MERGE-COMPLETION-AARRAY-FUDGE)))
         (DOLIST (NEWELT (G-L-P ADDITIONAL-AARRAY))
           (LET* ((AARRAY (FOLLOW-STRUCTURE-FORWARDING AARRAY))
                  (OLDIDX (FIND-AARRAY-INSERTION-INDEX AARRAY (CAR NEWELT)))
                  (OLDELT (AREF AARRAY OLDIDX)))
             (IF (AND (< OLDIDX OLD-MAX)
                      (STRING-EQUAL (CAR OLDELT) (CAR NEWELT)))
                 (SETF (CDR OLDELT)
                       (MERGE-AND-ELIMINATE-DUPLICATES (CDR OLDELT) (CDR NEWELT)))
               (SYS:%BLT-TYPED
                 (ALOC AARRAY (1- OLD-MAX))
                 (ALOC AARRAY OLD-MAX)
                 (- OLD-MAX OLDIDX) -1)
               (SETF (FILL-POINTER AARRAY) (INCF OLD-MAX))
               (SETF (AREF AARRAY OLDIDX) NEWELT))))
         (FOLLOW-STRUCTURE-FORWARDING AARRAY))
        (T
         ;; Make a new AARRAY big enough to hold both.
         (SETQ NEW-AARRAY (MAKE-ARRAY (+ OLD-MAX ADDED-MAX MERGE-COMPLETION-AARRAY-FUDGE)
                                      :TYPE 'ART-Q-LIST
                                      :LEADER-LENGTH 2
                                      :LEADER-LIST '(0)))
         ;; Now merge the two inputs into it.
         (DO ((OLD 0) (ADDED 0)
              (OLD-ELEM) (ADDED-ELEM)
              (ELEM-TO-BE-ADDED)
              (LAST-ELEM-ADDED NIL ELEM-TO-BE-ADDED))
             ;; Done when both inputs are empty.
             ((AND (= OLD OLD-MAX) (= ADDED ADDED-MAX)))
           ;; Find which input aarray's next element is least.  Remove it
           (SETQ ADDED-ELEM (AND ( ADDED ADDED-MAX) (AREF ADDITIONAL-AARRAY ADDED))
                 OLD-ELEM (AND ( OLD OLD-MAX) (AREF AARRAY OLD)))
           (IF (AND OLD-ELEM
                    (OR (NULL ADDED-ELEM)
                        (STRING-LESSP (CAR OLD-ELEM) (CAR ADDED-ELEM))))
               (SETQ ELEM-TO-BE-ADDED OLD-ELEM
                     OLD (1+ OLD))
             (SETQ ELEM-TO-BE-ADDED ADDED-ELEM
                   ADDED (1+ ADDED)))
           ;; and insert it into the new aarray.  But flush duplicate strings.
           (COND ((AND LAST-ELEM-ADDED
                       (%STRING-EQUAL (CAR ELEM-TO-BE-ADDED) 0
                                      (CAR LAST-ELEM-ADDED) 0 NIL))
                  (SETF (CDR LAST-ELEM-ADDED)
                        (MERGE-AND-ELIMINATE-DUPLICATES (CDR ELEM-TO-BE-ADDED)
                                                        (CDR LAST-ELEM-ADDED)))
                  (SETQ ELEM-TO-BE-ADDED LAST-ELEM-ADDED))
                 ((ARRAY-PUSH NEW-AARRAY ELEM-TO-BE-ADDED))
                 (T                             ;This ought to never happen
                  (ARRAY-PUSH-EXTEND NEW-AARRAY ELEM-TO-BE-ADDED))))
         (STORE-ARRAY-LEADER T NEW-AARRAY 1)
         (STRUCTURE-FORWARD AARRAY NEW-AARRAY))))

(DEFUN MERGE-AND-ELIMINATE-DUPLICATES (L1 L2 &AUX LIST)
  (SETQ LIST (IF (ATOM L1) (NCONS L1) (NREVERSE L1)))
  (IF (ATOM L2)
      (PUSH* L2 LIST)
    (DOLIST (X L2)
      (PUSH* X LIST)))
  (SETQ LIST (NREVERSE LIST))
  (IF (CDR LIST) LIST (CAR LIST)))

(DEFUN STRING-IN-AARRAY-P (STRING AARRAY)
  "T if STRING is the car of one of the elements of AARRAY.
Assumes AARRAY is sorted by the cars of its elements."
  (SETQ STRING (STRING STRING))
  (DO ((LO 0)
       (HI (ARRAY-ACTIVE-LENGTH AARRAY))
       IDX INC TEM)
      (NIL)
    (AND (ZEROP (SETQ INC (TRUNCATE (- HI LO) 2)))
         (RETURN NIL))
    (SETQ IDX (+ LO INC))
    (COND ((ZEROP (SETQ TEM (STRING-COMPARE STRING (CAR (AREF AARRAY IDX)))))
           (RETURN T))
          ((PLUSP TEM)
           (SETQ LO IDX))
          (T
           (SETQ HI IDX)))))

;;;; Variables.

;;; Given a variable and a stream, prints the variable's name and value to that stream.
(DEFUN PRINT-VARIABLE (VAR &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Describe the ZWEI variable VAR on STREAM.
Prints the name, value and documentation."
  (LET ((*PACKAGE* (PKG-FIND-PACKAGE "ZWEI"))
        (*PRINT-BASE* 10.)
        (*NOPOINT T) (*PRINT-RADIX* NIL)
        (VAL (SYMBOL-VALUE VAR))
        (TYPE (GET VAR 'VARIABLE-TYPE)))
    (FORMAT STREAM "~25,4,2A " (STRING-APPEND (GET VAR 'VARIABLE-NAME) ":"))
    (CASE TYPE
      ((:BOOLEAN :KEYWORD :STRING :FIXNUM-OR-NIL :FIXNUM :ANYTHING)
       (PRIN1 VAL STREAM))
      ((:PIXEL :PIXEL-OR-NIL)
       (IF VAL
           (FORMAT STREAM "~D pixels (~D spaces in current font, plus ~D pixels)"
                   VAL (TRUNCATE VAL (FONT-SPACE-WIDTH)) (\ VAL (FONT-SPACE-WIDTH)))
         (PRIN1 VAL STREAM)))
      (:CHAR (TYO VAL STREAM))
      (:CHAR-LIST
       (WRITE-CHAR #/" STREAM)
       (DOLIST (L VAL) (TYO L STREAM))
       (WRITE-CHAR #/" STREAM)))
    (TERPRI STREAM)))

(DEFUN PRINT-VARIABLE-DOC (VAR &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the short doc for ZWEI variable VAR on STREAM.
Adds some leading spaces and a trailing newline."
  (LET ((DOC (DOCUMENTATION VAR 'VARIABLE)))
    (LET ((FIRST-CR (STRING-SEARCH-CHAR #/NEWLINE DOC)))
      (FORMAT STREAM "    ~A~&" (IF FIRST-CR
                                    (NSUBSTRING DOC 0 FIRST-CR)
                                  DOC)))))

(DEFCOM COM-LIST-VARIABLES "List all editor user option variables and their values.
With an argument, print out documentation as well." ()
  (FORMAT T "~%ZWEI variables:~2%")
  (SETQ *VARIABLE-ALIST* (SORTCAR *VARIABLE-ALIST* #'STRING-LESSP))
  (DO L *VARIABLE-ALIST* (CDR L) (NULL L)
      (PRINT-VARIABLE (CDAR L))
      (AND *NUMERIC-ARG-P*
           (PRINT-VARIABLE-DOC (CDAR L))))
  (FORMAT T "~%Done.~%")
  DIS-NONE)

(DEFCOM COM-VARIABLE-APROPOS "List all editor options whose names contain a given substring.
With an argument, print documentation as well." ()
  (MULTIPLE-VALUE-BIND (FUNCTION ARG STR)
      (GET-EXTENDED-SEARCH-STRINGS "Variable Apropos (substring):")
    (FORMAT T "~%ZWEI variables containing /"~A/":~2%" STR)
    (DO L *VARIABLE-ALIST* (CDR L) (NULL L)
        (COND ((FUNCALL FUNCTION ARG (CAAR L))
               (PRINT-VARIABLE (CDAR L))
               (AND *NUMERIC-ARG-P*
                    (PRINT-VARIABLE-DOC (CDAR L))))))
    (FORMAT T "~%Done.~%"))
  DIS-NONE)

(DEFCOM COM-DESCRIBE-VARIABLE "Print documentation of editor user option variable.
Reads the name of a variable (using completion),
and prints its documentation string." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
             "Variable name:" *VARIABLE-ALIST* NIL NIL
             "You are typing the name of a variable to document.")))
    (COND ((EQUAL X "") (BARF))
          (T (PRINT-VARIABLE (CDR X))
             (FORMAT T "~A~&"
                     (DOCUMENTATION (CDR X) 'VARIABLE)))))
  DIS-NONE)

(DEFCOM COM-SET-VARIABLE "Set editor user option variable, checking type.
Read the name of a variable (with completion), display current value
and documentation, and read a new variable.  Some checking is done
that the variable is the right type." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
            "Variable name:" *VARIABLE-ALIST* NIL NIL
            "You are typing the name of a variable to be set."
            #'(LAMBDA (X)
                   (PRINT-VARIABLE (CDR X))
                   (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
     (AND (EQUAL X "") (BARF))
     (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))
     (PRINT-VARIABLE (CDR X))
     (LET* ((VAR (CDR X))
            (*MINI-BUFFER-DEFAULT-STRING* (VARIABLE-STRING VAR))
            (DEFAULT *MINI-BUFFER-DEFAULT-STRING*)
            (*PACKAGE* (PKG-FIND-PACKAGE "ZWEI"))
            (TYPE (GET VAR 'VARIABLE-TYPE))
            (*READ-BASE* 10.) (*PRINT-BASE* 10.))
       (SET VAR
            (CASE TYPE
              (:CHAR
               (LET ((V (TYPEIN-LINE-READLINE "New value (one character)")))
                 (OR (= (STRING-LENGTH V) 1) (BARF "~A is not one character." V))
                 (CHAR-INT (AREF V 0))))
              (:CHAR-LIST
               (LET ((V (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)")))
                 (DO ((I 0 (1+ I))
                      (RET)
                      (LIM (STRING-LENGTH V)))
                     (( I LIM) (NREVERSE RET))
                   (PUSH (CHAR-INT (AREF V I)) RET))))
              (:STRING
               (TYPEIN-LINE-READLINE-WITH-DEFAULT DEFAULT "New value (a string)"))
              ((:PIXEL :FIXNUM)
               (LET ((V (TYPEIN-LINE-READ "New value (a fixnum)")))
                 (UNLESS (FIXNUMP V) (BARF "~S is not a fixnum." V))
                 V))
              ((:FIXNUM-OR-NIL :PIXEL-OR-NIL)
               (LET ((V (TYPEIN-LINE-READ "New value (NIL or a fixnum)")))
                 (UNLESS (OR (FIXNUMP V) (NULL V)) (BARF "~S is neither a fixnum not NIL." V))
                 V))
              (:SMALL-FRACTION
               (LET ((V (TYPEIN-LINE-READ "New value (a flonum between 0.0 and 1.0")))
                 (OR (FLOATP V) (BARF "~S is not a floating-point number." V))
                 (OR (AND ( V 0.0s0) ( V 1.0s0))
                     (BARF "~S is not between 0.0 and 1.0" V))
                 (SMALL-FLOAT V)))
              (:BOOLEAN
               (LET ((V (TYPEIN-LINE-READ "New value (T or NIL)")))
                 (OR (MEMQ V '(T NIL)) (BARF "~S is neither T nor NIL." V))
                 V))
              (:KEYWORD
               (LET ((V (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value (a symbol)")))
                 (OR (SYMBOLP V) (BARF "~S is not a symbol." V))
                 V))
              (:ANYTHING
               (TYPEIN-LINE-READ-WITH-DEFAULT DEFAULT "New value"))))
       ;; If variable is local, record its new value in the buffer
       ;; in case the buffer is selected in another window.
       (IF (MEMQ VAR *LOCAL-VARIABLES*)
           (MAKE-LOCAL-VARIABLE VAR (SYMEVAL VAR)))))
  DIS-NONE)

(DEFUN VARIABLE-STRING (VAR)
  "Return a string representing the value of ZWEI variable VAR."
  (LET ((*PACKAGE* (SYMBOL-PACKAGE 'FOO))
        (*PRINT-BASE* 10.)
        (*PRINT-RADIX* NIL)
        (*NOPOINT NIL)
        (VAL (SYMBOL-VALUE VAR))
        (TYPE (GET VAR 'VARIABLE-TYPE)))
    (CASE TYPE
      ((:BOOLEAN :KEYWORD :FIXNUM-OR-NIL :FIXNUM :ANYTHING :PIXEL :PIXEL-OR-NIL)
       (PRIN1-TO-STRING VAL))
      (:STRING (STRING VAL))
      (:CHAR (FORMAT NIL "~C" VAL))
      (:CHAR-LIST
       (DO ((VAL VAL (CDR VAL))
            (STRING (MAKE-STRING (MAX (LENGTH VAL) 10.) :FILL-POINTER 0)))
           ((NULL VAL) STRING)
         (VECTOR-PUSH-EXTEND (CAR VAL) STRING))))))

(DEFCOM COM-MAKE-LOCAL-VARIABLE "Make editor user option variable local to this buffer.
Reads the name of a variable (with completion)
and makes the variable local to the current buffer
so that if you set it you will not affect any other buffer." ()
  (LET ((VARNAME (COMPLETING-READ-FROM-MINI-BUFFER
                   "Variable name:" *VARIABLE-ALIST* NIL NIL
                   "You are typing the name of a variable to be made local to this buffer."
                   #'(LAMBDA (X)
                       (PRINT-VARIABLE (CDR X))
                       (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
    (UNLESS (CONSP VARNAME) (BARF))
    (MAKE-LOCAL-VARIABLE (CDR VARNAME)))
  DIS-NONE)

(DEFCOM COM-KILL-LOCAL-VARIABLE "Make editor user option variable global in this buffer.
Reads the name of a variable (with completion)
and makes the variable no longer be local to this buffer,
so that this buffer will share the value with most other buffers." ()
  (LET ((VARNAME (COMPLETING-READ-FROM-MINI-BUFFER
                   "Variable name:" *VARIABLE-ALIST* NIL NIL
                   "You are typing the name of a variable to be made local to this buffer."
                   #'(LAMBDA (X)
                       (PRINT-VARIABLE (CDR X))
                       (FORMAT T "~A~&" (DOCUMENTATION (CDR X) 'VARIABLE))))))
    (UNLESS (CONSP VARNAME) (BARF))
    (KILL-LOCAL-VARIABLE (CDR VARNAME)))
  DIS-NONE)

(DEFCOM COM-LIST-LOCAL-VARIABLES "List editor user option variables local in this buffer." ()
  (DOLIST (VARNAME *LOCAL-VARIABLES*)
    (PRINT-VARIABLE VARNAME)
    (FORMAT T "~A~&" (DOCUMENTATION VARNAME 'VARIABLE)))
  DIS-NONE)

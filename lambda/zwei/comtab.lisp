;;;    -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;;Command dispatch table functions for ZWEI.
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; The functions in this file implement COMTABs.
;;; This file also contains the basic editor command loop.

(DEFUN EXTENDED-COMMAND-ALIST (COMTAB)
  "Returns a funny alist of all extended commands in COMTAB and its indirections.
The funny alist can have as its ultimate CDR a locative
which points to a cell containing some more of the alist."
  (DO ((CI (COMTAB-INDIRECT-TO COMTAB) (COMTAB-INDIRECT-TO CI))
       (C COMTAB CI)
       (EC))
      ((NULL CI))
    (COND ((CONSP (SETQ EC (COMTAB-EXTENDED-COMMANDS C)))
           (RPLACD (LAST EC)                    ;Put in alist indirections
                   (LOCF (COMTAB-EXTENDED-COMMANDS CI))))
          ((OR (NULL EC) (LOCATIVEP EC))
           (SETF (COMTAB-EXTENDED-COMMANDS C)
                 (LOCF (COMTAB-EXTENDED-COMMANDS CI))))))
  (let ((ans (COMTAB-EXTENDED-COMMANDS COMTAB)))
    (if (locativep ans)
        (car ans)
      ans)))

(DEFUN COMMAND-LOOKUP (CHAR COMTAB &OPTIONAL NO-ALIASES NO-INDIRECTION)
  "Return the command in COMTAB has for character CHAR.
NO-ALIASES means do not follow aliases (such as #\c-h-A => #\c-sh-A)
 Instead, return a list of the CHAR-BITS and the CHAR-CODE of the character
 for which it is an alias.
NO-INDIRECTION means only for a command associated with CHAR in COMTAB itself;
 ie the COMTAB-INDIRECT-TO of COMTAB is not followed.
The second value is the comtab the command was found in.
This will be COMTAB or a comtab that COMTAB indirects to."
  (DECLARE (VALUES COMMAND COMTAB))
;character lossage
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (DO ((CTB COMTAB (COMTAB-INDIRECT-TO CTB))
       (CH CHAR) KEYBOARD-ARRAY COMMAND)
      ((NULL CTB) NIL)
    (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
          COMMAND (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
                         (CDR (ASSQ CH KEYBOARD-ARRAY)))
                        ((TV:CHAR-MOUSE-P CH)
                         (IF (ARRAYP (COMTAB-MOUSE-ARRAY CTB))
                             (AREF (COMTAB-MOUSE-ARRAY CTB)
                                   (MIN (LDB %%KBD-MOUSE-N-CLICKS CH) 1)
                                   (LDB %%KBD-MOUSE-BUTTON CH)
                                   (CHAR-BITS CH))))
                        (T
                         (AREF KEYBOARD-ARRAY (CHAR-CODE CH) (CHAR-BITS CH)))))
    (COND ((AND (CONSP COMMAND) (NOT NO-ALIASES))
           (RETURN (COMMAND-LOOKUP (MAKE-CHAR (CADR COMMAND) (CAR COMMAND))
                                   CTB NO-ALIASES NO-INDIRECTION)))
          (COMMAND (RETURN (VALUES COMMAND CTB)))
          (NO-INDIRECTION (RETURN NIL)))))

(DEFUN COMMAND-STORE (COMMAND CHAR COMTAB &AUX KEYBOARD-ARRAY)
  "Store COMMAND into COMTAB for character CHAR."
;character lossage
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB))
  (COND ((NOT (ARRAYP KEYBOARD-ARRAY))
         (LET ((ELEMENT (ASSQ CHAR KEYBOARD-ARRAY)))
           (IF ELEMENT
               (SETF (CDR ELEMENT) COMMAND)
             (PUSH (CONS CHAR COMMAND) (COMTAB-KEYBOARD-ARRAY COMTAB)))))
        ((TV:CHAR-MOUSE-P CHAR)
         (SETF (AREF (COMTAB-MOUSE-ARRAY COMTAB)
                     (MIN (LDB %%KBD-MOUSE-N-CLICKS CHAR) 1)
                     (LDB %%KBD-MOUSE-BUTTON CHAR)
                     (CHAR-BITS CHAR))
               COMMAND))
        (T
         (SETF (AREF KEYBOARD-ARRAY
                     (CHAR-CODE CHAR)
                     (CHAR-BITS CHAR))
               COMMAND))))

(DEFVAR *ILLEGAL-COMMAND-BARF-STRING-ALIST*
        '((#/m- . "  (Do Dah, Do Dah)")
          (#/h-space . "  Perhaps you should engage warp drive first.")))
(DEFUN COMMAND-EXECUTE (COMMAND CHAR &OPTIONAL PREFIX-CHAR HOOK-LIST)
  "Execute an editor command (function name) as obtained from COMMAND-LOOKUP.
Undefined commands carefully.
Evaluates the elements of HOOK-LIST before calling the function.
If a hook throws to COMMAND-EXECUTE, the command itself is skipped.
CHAR should be the character that was typed, in case the command is /"self insert/".
PREFIX-CHAR is the prefix char by which this command was reached, or NIL.
It is used only in error reporting."
;character lossage
  (IF (NUMBERP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (LET (HOOK-SUCCESS)
    (CATCH 'COMMAND-EXECUTE
      (DOLIST (HOOK HOOK-LIST)
        (FUNCALL HOOK CHAR))
      (SETQ HOOK-SUCCESS T))
    (IF HOOK-SUCCESS
        (COND ((MEMQ COMMAND '(NIL :UNDEFINED))
               (SEND *STANDARD-INPUT* :CLEAR-INPUT)     ;More randomness may follow
               (BARF "~:[~:@C ~;~*~]~:@C is not a defined key.~A"
                     (NOT PREFIX-CHAR) PREFIX-CHAR CHAR
                     (OR (CDR (ASSQ CHAR *ILLEGAL-COMMAND-BARF-STRING-ALIST*)) "")))
              ((AND (SYMBOLP COMMAND)
                    (NOT (FBOUNDP COMMAND)))
               (BARF "~S is not implemented." COMMAND))
              (T
               (FUNCALL COMMAND)))
      DIS-NONE)))

(DEFUN COMMAND-HOOK (HOOK HOOK-SYMBOL)
  "Add the hook HOOK onto the hook-list HOOK-SYMBOL.
HOOK should be a symbol defined as a function of one arg, a character,
 and it should have a COMMAND-HOOK-PRIORITY property
 which is a fixnum that controls the placement in the hook-list
 (Low priority numbers come first).
Returns a form to undo what it did."
  (LET ((PRI (GET HOOK 'COMMAND-HOOK-PRIORITY)))
    (OR PRI (FERROR "~S is not a valid command hook." HOOK))
    (DO ((L (SYMBOL-VALUE HOOK-SYMBOL) (CDR L))
         (PREV NIL L))
        ((OR (NULL L)
             (> (GET (CAR L) 'COMMAND-HOOK-PRIORITY) PRI))
         (LET ((X (CONS HOOK L)))
           (IF PREV
               (SETF (CDR PREV) X)
               (SET HOOK-SYMBOL X))))))
  `(SETQ ,HOOK-SYMBOL (DELQ ',HOOK ,HOOK-SYMBOL)))

(DEFUN CREATE-COMTAB (&OPTIONAL NAME)
  "Return a new, empty comtab with name NAME."
  (MAKE-COMTAB :COMTAB-KEYBOARD-ARRAY (MAKE-ARRAY '(#o240 16.)) ;nkbdchars char-bits-limit
               :COMTAB-NAME NAME
               :COMTAB-MOUSE-ARRAY (MAKE-ARRAY '(2 3 16.))))    ;nbuttons nclicks bits-limit

(DEFUN CREATE-SPARSE-COMTAB (&OPTIONAL NAME)
  "Return a new, empty sparse-comtab with name NAME.
A sparse comtab stores its command list as a list rather than an array.
It is good if the comtab will have very few commands in it."
  (MAKE-COMTAB :COMTAB-KEYBOARD-ARRAY NIL
               :COMTAB-NAME NAME))

;;; Copy a COMTAB.  Copies the arrays, and the list structure of the extended command alist.
;;; NOTE: this function doesn't make the comtab occupy fewer pages, since the array part
;;; and the alist part can't be on the same page anyway, and the alist part is all on one
;;; page since it was consed up all at once.  Furthermore the new copy is not EQ to the
;;; old copy, which screws up comtab indirection.
(DEFUN COPY-COMTAB (OLD-COMTAB &OPTIONAL NEW-NAME)
  "Returns a comtab which is a copy of OLD-COMTAB, with name NEW-NAME.
The keyboard array and extended command alist are both copied."
  (LET ((OKBD (COMTAB-KEYBOARD-ARRAY OLD-COMTAB))
        (OMSE (COMTAB-MOUSE-ARRAY OLD-COMTAB)))
    (MAKE-COMTAB
      :COMTAB-NAME           NEW-NAME
      :COMTAB-KEYBOARD-ARRAY (COND ((CONSP OKBD)
                                    (COPY-TREE OKBD))
                                   ((ARRAYP OKBD)
                                    (COPY-ARRAY-CONTENTS OKBD
                                      (MAKE-ARRAY (ARRAY-DIMENSIONS OKBD)))))
      :COMTAB-MOUSE-ARRAY    (COND ((CONSP OMSE)
                                    (COPY-TREE OMSE))
                                   ((ARRAYP OMSE)
                                    (COPY-ARRAY-CONTENTS OMSE
                                      (MAKE-ARRAY (ARRAY-DIMENSIONS OMSE)))))
      :COMTAB-EXTENDED-COMMANDS (COPYTREE (COMTAB-EXTENDED-COMMANDS OLD-COMTAB))
      :COMTAB-INDIRECT-TO    (COMTAB-INDIRECT-TO OLD-COMTAB))))

(DEFUN SET-COMTAB (COMTAB SPECS &OPTIONAL EXTENDED-COMMANDS)
  "Set the definitions of some characters and extended commands in COMTAB.
SPECS is an alternating list of characters (or character names) and definitions.
A list may also appear instead of a character; this means set several
 successive characters.  The car of the list is the first character,
 and the cdr is an iteration count.  The definition that follows the list
 is either a symbol to store in all the characters, or a function to call
 given the character.  What the function returns is stored.
EXTENDED-COMMAND is an alist of additional extended command names vs. definitions.

If COMTAB a symbol, a new comtab is created with that name,
with all lower-case characters indirected to the upper-case characters,
and then the specified definitions are stored in it.

In any case, the comtab is returned."
  (WHEN (SYMBOLP COMTAB)
    (SETQ COMTAB (CREATE-COMTAB COMTAB))
    (SET-COMTAB-UPPERCASE-INDIRECTION COMTAB))
  (DO ((L SPECS (CDDR L))) ((NULL L))
    (COND ((CONSP (CAR L))
           (DO ((CHAR (COMMAND-CHAR-FROM-NAME (CAAR L)) (1+ CHAR))
                (I 0 (1+ I))
                (TO (CADAR L))
                (COMMAND (CADR L)))
               (( I TO))
             (COMMAND-STORE (IF (SYMBOLP COMMAND)
                                COMMAND
                                (FUNCALL COMMAND CHAR))
                            CHAR
                            COMTAB)))
          (T (COMMAND-STORE (CADR L) (COMMAND-CHAR-FROM-NAME (CAR L)) COMTAB))))
  (SETF (COMTAB-EXTENDED-COMMANDS COMTAB)
        (APPEND EXTENDED-COMMANDS (COMTAB-EXTENDED-COMMANDS COMTAB)))
  COMTAB)

(DEFUN MAKE-COMTAB-NON-SPARSE (COMTAB &AUX KBD)
  "Modify COMTAB to be a non-sparse comtab, if it is sparse.
This replaces the alist of commands with an actual array."
  (UNLESS (ARRAYP (SETQ KBD (COMTAB-KEYBOARD-ARRAY COMTAB)))
    (LET ((ARRAY (COMTAB-KEYBOARD-ARRAY (CREATE-COMTAB))))
      (DOLIST (ELT KBD)
        (SETF (AREF ARRAY (CHAR-CODE (CAR ELT)) (CHAR-BITS (CAR ELT)))
              (CDR ELT)))
      (SETF (COMTAB-KEYBOARD-ARRAY COMTAB) ARRAY))))

(defun set-comtab-uppercase-indirection (comtab)
  "Make all lower case chars in COMTAB indirect appropriately.
Those with no meta bits indirect to the corresponding upper case character.
Uppercase characters with hyper bit set indirect to to the corresponding
 lowercase character with no hyper bit, for compatibility with Ye Olden Days
 in which C-Sh-a was synonymous with C-H-A. Sigh.
Note: characters which are already defined are not redefined."
  (make-comtab-non-sparse comtab)
  (let ((array (comtab-keyboard-array comtab))
        bits code)
    ;; make h-foo-cokebottle indirect to sh-foo-cokebottle
    (dolist (start-char '(#/h-c-A #/h-m-A #/h-s-A
                          #/h-c-m-A #/h-c-s-A #/h-m-s-A
                          #/h-c-m-s-A))
      (setq bits (char-bits start-char)
            code (char-code start-char))
      (dotimes (x 26.)
        (unless (aref array (+ code x) bits)
          (setf (aref array (+ code x) bits)
                `(,(- bits char-hyper-bit)
                  ,(+ code x (- #/a #/A)))))))
    ;; make lowercase with no bucky bits indirect to uppercase
    (dotimes (x 26.)
      (unless (aref array (+ #/a x) 0)
        (setf (aref array (+ #/a x) 0)
              `(0 ,(+ #/A x)))))))

(DEFUN SET-COMTAB-CONTROL-INDIRECTION (COMTAB)
  "Make EMACS-like indirections in COMTAB.
Tab, Return, etc. are indirected to C-I, C-M.
Meta-anything is indirected to that anything.
Non-ascii control characters indirect to the non-control character.
However, commands that are already defined are not clobbered."
  (MAKE-COMTAB-NON-SPARSE COMTAB)
  (LET ((ARRAY (COMTAB-KEYBOARD-ARRAY COMTAB)))
    ;; Indirect things like Tab to things like control-I
    (DOLIST (CHAR '(#/CR #/LF #/TAB #/BS #/FF #/VT))
      (WHEN (NULL (AREF ARRAY CHAR 0))
        (SETF (AREF ARRAY CHAR 0) (LIST 1 (- CHAR #o100)))))
    ;; Indirect all meta things through the corresponding non-meta thing
    (DO ((I 2 (1+ I))) ((= I 4))
      (DOTIMES (CHAR (ARRAY-DIMENSION ARRAY 0))
        (WHEN (NULL (AREF ARRAY CHAR I))
          (SETF (AREF ARRAY CHAR I) (LIST (- I 2) CHAR)))))
    ;; Indirect controls other than atsign through underscore to non-controls
    (DOTIMES (CHAR (ARRAY-DIMENSION ARRAY 0))
      (WHEN (AND (NOT ( #/@ CHAR #/_))
                 (NULL (AREF ARRAY CHAR 1)))
        (SETF (AREF ARRAY CHAR 1) (LIST 0 (INT-CHAR CHAR)))))))

(DEFPROP SET-COMTAB MAKE-SET-COMTAB-UNDO-LIST :UNDO-FUNCTION)

(DEFUN MAKE-SET-COMTAB-UNDO-LIST (FORM)
  "Given FORM (a call to SET-COMTAB), return a form that will undo what it did."
  (LET ((COMTAB (EVAL (SECOND FORM)))
        (UN-SPECS))
    (WHEN (SYMBOLP COMTAB)
      (FERROR "A (~S symbol ...) form cannot be undone." 'SET-COMTAB))
    (DO ((S (EVAL (THIRD FORM)) (CDDR S)))
        ((NULL S))
      (LET ((CHAR-NAME (FIRST S)))
        (WHEN (CONSP CHAR-NAME)
          (FERROR "A ~S with a List character name cannot be undone." 'SET-COMTAB))
        (PUSH (COMMAND-LOOKUP (COMMAND-CHAR-FROM-NAME CHAR-NAME) COMTAB) UN-SPECS)
        (PUSH CHAR-NAME UN-SPECS)))
    `(PROGN (REMOVE-EXTENDED-COMMANDS ',(EVAL (FOURTH FORM)) ',COMTAB)
            (SET-COMTAB ',COMTAB ',UN-SPECS))))

(DEFUN SET-COMTAB-RETURN-UNDO (&REST &QUOTE FORM &AUX UNDO)
  "Like SET-COMTAB, but returns a form to undo what it did.
This is a historical crock.
What you probably really want is ZWEI:SET-COMTAB inside of LOGIN-FORMS instead."
  (SETQ FORM (CONS 'SET-COMTAB FORM)
        UNDO (MAKE-SET-COMTAB-UNDO-LIST FORM))
  (EVAL FORM)
  UNDO)

(DEFUN REMOVE-EXTENDED-COMMANDS (COMMAND-LIST COMTAB)
  "Delete some extended commands from COMTAB's list of them.
COMMAND-LIST's elements are symbols (commands) or conses whose cars are commands."
  (LET ((EC (COMTAB-EXTENDED-COMMANDS COMTAB)))
    (DOLIST (COMMAND COMMAND-LIST)
      (OR (SYMBOLP COMMAND)
          (SETQ COMMAND (CAR COMMAND)))
      (SETQ EC (DELQ (ASSQ COMMAND EC) EC 1)))
    (SETF (COMTAB-EXTENDED-COMMANDS COMTAB) EC)))

(DEFUN SET-COMTAB-INDIRECTION (OF TO)
  "Set the indirection-comtab of comtab OF to comtab TO."
  (SETF (COMTAB-INDIRECT-TO OF) TO))

;;; This takes a string specification of a command char (the numeric code for
;;; a keystroke), and returns the command char.  Specifications may look like:
;;;   "A", "B", "V", "", etc., referring to keyboard characters.
;;;   "MOUSE-n-m", where n and m are numbers relating which button and how many clicks.
;;; Note: this function is semi-obselete, since the reader allows specification of all
;;; characters.
(DEFUN COMMAND-CHAR-FROM-NAME (NAME)
  (TYPECASE NAME
    (CHARACTER NAME)
    (FIXNUM (INT-CHAR NAME))
    (T
     (LET ((I (STRING-MATCH "MOUSE" NAME)))
       (IF (NULL I)
           ;; The name does not start with MOUSE.
           (LET ((CHAR1 (CHAR NAME 0)))
             (LET ((X (ASSQ CHAR1 '((#/ . 0) (#/ . 1) (#/ . 2) (#/ . 3)))))
               (IF (NULL X)
                   CHAR1
                 (MAKE-CHAR (AREF NAME 1) (CDR X)))))
         (MULTIPLE-VALUE-BIND (BUTTON J)
             (PARSE-NUMBER NAME (1+ I))
           (LET ((N-CLICKS (PARSE-NUMBER NAME (1+ J))))
             (COND ((OR (> 1 BUTTON 3)
                        (> 1 N-CLICKS 2))
                    (FERROR "Invalid mouse specification: ~A" NAME))
                   (T (TV:MAKE-MOUSE-CHAR (1- BUTTON) (1- N-CLICKS)))))))))))

(DEFUN KEY-EXECUTE (KEY &OPTIONAL (*NUMERIC-ARG-P* NIL) (*NUMERIC-ARG* 1))
  (PROCESS-COMMAND-CHAR KEY)
  DIS-NONE)

;; This is the Meta-X command.
;; Note that numeric arg is transmitted via dynamic scoping etc.
(DEFCOM COM-EXTENDED-COMMAND "" ()
  (LET ((ANS (GET-EXTENDED-COMMAND (FORMAT NIL "Extended command:~:[  (Arg = ~A.)~]"
                                            (NOT *NUMERIC-ARG-P*)
                                            (FORMAT-ARGUMENT *NUMERIC-ARG-P* *NUMERIC-ARG*))
                                   *COMTAB*)))
    (COND ((EQUAL ANS "")
           (BEEP)
           DIS-NONE)
          (T
           (LET ((*CURRENT-COMMAND* (CDR ANS)))
             (FUNCALL *CURRENT-COMMAND*))))))

(DEFUN FORMAT-ARGUMENT (ARG-P ARG)
  "Return a string representing numeric argument status according to ARG-P and ARG.
ARG-P should be a value of *NUMERIC-ARG-P* and ARG a value of *NUMERIC-ARG*."
  (CASE ARG-P
    (NIL "")
    (:SIGN (IF (MINUSP ARG) "-" "+"))
    (:DIGITS (FORMAT NIL "~D" ARG))
    (:CONTROL-U
      (DO ((STR "Control-U" (STRING-APPEND "Control-U " STR))
           (A (ABS ARG) (TRUNCATE A 4)))
          ((= A 4) (IF (MINUSP ARG) (STRING-APPEND "- " STR) STR))))))

(DEFPROP COM-EXTENDED-COMMAND DOCUMENT-EXTENDED-COMMAND DOCUMENTATION-FUNCTION)
(DEFUN DOCUMENT-EXTENDED-COMMAND (COMMAND CHAR OP)
  (COND ((EQ OP ':NAME) "a prefix for extended commands")
        ((MEMQ OP '(:FULL :SHORT))
         (FORMAT T "Completing reads and executes a command from the mini buffer.~%")
         (WHEN (EQ OP ':FULL)
           (SETQ COMMAND (GET-EXTENDED-COMMAND "Type a command to document:" *COMTAB*))
           (UNLESS (EQUAL COMMAND "")
             (FORMAT T "~:C ~A is implemented by ~S:~%" CHAR (CAR COMMAND) (CDR COMMAND))
             (PRINT-DOC OP (CDR COMMAND) CHAR))))))

(DEFUN GET-EXTENDED-COMMAND (PROMPT COMTAB)
  "Read an extended command name from the mini buffer, printing PROMPT.
COMTAB controls the set of extended commands to complete from.
The value is a cons of a string (command name) and symbol (the command itself)."
  (COMPLETING-READ-FROM-MINI-BUFFER PROMPT
                                    (EXTENDED-COMMAND-ALIST COMTAB)
                                    NIL ;Impossible is not OK.
                                    NIL ;Don't do initial completion (it won't work anyway)
                                    "You are typing an extended command."
                                    (LAMBDA (X)
                                      (PRINT-DOC ':FULL (CDR X)))))

(DEFCOM COM-ANY-EXTENDED-COMMAND DOCUMENT-ANY-EXTENDED-COMMAND ()
  (LET ((FROM-META-X (EQ *MINI-BUFFER-COMMAND-IN-PROGRESS* 'COM-EXTENDED-COMMAND))
        (INITIAL-COMPLETE NIL))
    (WHEN FROM-META-X
      ;; This turns M-X into C-M-X
      (SETQ INITIAL-COMPLETE (BP-LINE (POINT))) ;Start with what was typed already
      (AND (ZEROP (STRING-LENGTH INITIAL-COMPLETE))
           (SETQ INITIAL-COMPLETE NIL))
      (SETQ *MINI-BUFFER-COMMAND* NIL
            *MINI-BUFFER-COMMAND-IN-PROGRESS* NIL)      ;Prevent barfage
      (SETF (CAAR *OUTER-LEVEL-MINI-BUFFER-COMMAND*) 'COM-ANY-EXTENDED-COMMAND)
      ;; Flush any saved prompts so that C-0 C-X  will look through the
      ;; comtab to find a key for com-any-extended-command.
      (SETF (CDDDAR *OUTER-LEVEL-MINI-BUFFER-COMMAND*) (CURRENT-PROMPTS))
      (SETF `((COMMAND ,*NUMERIC-ARG-P* ,*NUMERIC-ARG*))
            *OUTER-LEVEL-MINI-BUFFER-COMMAND*))
    (LET ((ANS (GET-ANY-EXTENDED-COMMAND
                 (FORMAT NIL "Command:~:[  (Arg = ~A)~]"
                         (NOT *NUMERIC-ARG-P*)
                         (FORMAT-ARGUMENT *NUMERIC-ARG-P* *NUMERIC-ARG*))
                 INITIAL-COMPLETE)))
      (COND (FROM-META-X
             (THROW 'RETURN-FROM-COMMAND-LOOP ANS))
            ((EQUAL ANS "")
             (BEEP)
             DIS-NONE)
            (T
             (LET ((*CURRENT-COMMAND* (CDR ANS)))
               (FUNCALL *CURRENT-COMMAND*)))))))

(DEFUN DOCUMENT-ANY-EXTENDED-COMMAND (COMMAND CHAR OP)
  (COND ((EQ OP ':NAME) "a prefix for any Zmacs extended commands")
        ((MEMQ OP '(:FULL :SHORT))
         (FORMAT T "May be used to access commands not present in the current comtab~%")
         (FORMAT T "Completing reads and executes a command from the mini buffer.~%")
         (WHEN (EQ OP ':FULL)
           (SETQ COMMAND (GET-ANY-EXTENDED-COMMAND "Type a command to document:"))
           (UNLESS (EQUAL COMMAND "")
             (FORMAT T "~:C ~A is implemented by ~S:~%" CHAR (CAR COMMAND) (CDR COMMAND))
             (PRINT-DOC OP (CDR COMMAND) CHAR))))))

(DEFUN GET-ANY-EXTENDED-COMMAND (PROMPT &OPTIONAL INITIAL-COMPLETE)
  "Read an extended command name from the mini buffer, printing PROMPT.
All defined extended commands are allowed; no comtab is relevant.
INITIAL-COMPLETE if non-NIL is a string for the command to start with."
  (COMPLETING-READ-FROM-MINI-BUFFER
    PROMPT
    *COMMAND-ALIST* NIL INITIAL-COMPLETE
    "You are typing the name of a ZWEI command."
    (LAMBDA (X) (PRINT-DOC ':FULL (CDR X)))))

(DEFUN MAKE-EXTENDED-COMMAND (THE-COMTAB)
  "Create and return a prefix command, which will use THE-COMTAB as its comtab.
THE-COMTAB is used to look up the subcommands of the prefix command."
  (LET-CLOSED ((COMTAB THE-COMTAB))
    #'MAKE-EXTENDED-COMMAND-INTERNAL))

(DEFUN MAKE-EXTENDED-COMMAND-INTERNAL (&AUX (PREFIX-CHAR *LAST-COMMAND-CHAR*))
  (DECLARE (SPECIAL COMTAB))
  (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
    (LET ((CHAR (INPUT-WITH-PROMPTS *STANDARD-INPUT* :MOUSE-OR-KBD-TYI)))
      (AND (TV:CHAR-MOUSE-P CHAR) (BARF))
      (SETQ *LAST-COMMAND-CHAR* CHAR)))
  (SETQ *CURRENT-COMMAND* (COMMAND-LOOKUP *LAST-COMMAND-CHAR* COMTAB))
  (COMMAND-EXECUTE *CURRENT-COMMAND* *LAST-COMMAND-CHAR* PREFIX-CHAR))


;;; Handle printing prompts in the typein window.
;;; The PROMPT-ARRAY (bound inside (:method window :edit))
;;; remembers all the prompts that are currently active.
;;; It is updated with VECTOR-PUSH-EXTEND.
;;; Its PROMPTS-PRINTED slot is T if the prompts are actually being displayed
;;; (because the user has paused for longer than PROMPT-TIMEOUT)

(DEFSUBST PROMPTS-PRINTED (PROMPT-ARRAY)
  "T if the argument prompts are currently being displayed."
  (ARRAY-LEADER PROMPT-ARRAY 1))

(DEFVAR PROMPT-ARRAY NIL
  "An array that holds the current argument prompts.")
(DEFCONST PROMPT-TIMEOUT 40.
  "Time in 60'ths to wait before deciding to start displaying argument prompts.")

(DEFUN MAKE-PROMPT-ARRAY () (MAKE-ARRAY 5 :LEADER-LIST '(0 NIL)))

(DEFUN CLEAR-PROMPTS ()
  "Get rid of any argument prompts, and say that we are not printing them."
  (SETF (ARRAY-LEADER PROMPT-ARRAY 0) 0)
  (SETF (PROMPTS-PRINTED PROMPT-ARRAY) NIL))

(DEFUN CURRENT-PROMPTS ()
  "Return a list of the current argument prompts."
  (AND PROMPT-ARRAY
       (LISTARRAY PROMPT-ARRAY)))

(DEFUN ADD-PROMPT (STRING)
  "Add STRING an argument prompt, to the end of the array.
STRING can also be a character; then it is printed verbosely, with a space after.
The new prompt is printed now if prompts are already being printed."
  (VECTOR-PUSH-EXTEND STRING PROMPT-ARRAY)
  (IF (PROMPTS-PRINTED PROMPT-ARRAY)
      (PRINT-PROMPTS)))

(DEFUN DISCARD-LAST-PROMPT ()
  "Throw away the last prompt from the array of them.
Useful if you want to start printing your input manually,
or for processing a ? which is ignored except for printing help."
  (SETF (ARRAY-LEADER PROMPT-ARRAY 0)
        (MAX 0 (1- (ARRAY-LEADER PROMPT-ARRAY 0)))))

(DEFUN MAYBE-DISPLAY-PROMPTS ()
  "Start displaying argument prompts if no input arrives for a while."
  (IF (PROMPTS-PRINTED PROMPT-ARRAY)
      NIL
    (SEND *STANDARD-INPUT* :WAIT-FOR-INPUT-WITH-TIMEOUT PROMPT-TIMEOUT)
    (UNLESS (SEND *STANDARD-INPUT* :LISTEN)
      (SETF (PROMPTS-PRINTED PROMPT-ARRAY) T)
      (PRINT-PROMPTS))))

(DEFUN ALWAYS-DISPLAY-PROMPTS ()
  "Unconditionally start displaying the prompts."
  (SETF (PROMPTS-PRINTED PROMPT-ARRAY) T)
  (PRINT-PROMPTS))

(DEFUN PRINT-PROMPTS ()
  "Display all the argument prompts that are in PROMPT-ARRAY."
  (LET ((LEN (ARRAY-ACTIVE-LENGTH PROMPT-ARRAY)))
    (SEND *QUERY-IO* :CLEAR-WINDOW)
    (SEND *QUERY-IO* :FRESH-LINE)
    (DOTIMES (I LEN)
      (TYPEIN-LINE-CHAR-OR-STRING (AREF PROMPT-ARRAY I))
      (OR (STRINGP (AREF PROMPT-ARRAY I))
          (TYPEIN-LINE-CHAR-OR-STRING " ")))
    (SEND *QUERY-IO* :MAKE-COMPLETE)))

(DEFUN INPUT-WITH-PROMPTS (FUNCTION &REST ARGS &AUX CH)
  "Read an input character, maintaining and possibly printing the argument prompts.
FUNCTION is applied to ARGS to do the reading.
The character read is added to the prompts, and printed if appropriate."
  (IF (NOT (ZEROP (ARRAY-ACTIVE-LENGTH PROMPT-ARRAY)))
      (MAYBE-DISPLAY-PROMPTS))
  (IF (PROMPTS-PRINTED PROMPT-ARRAY)
      (TYPEIN-LINE-ACTIVATE
        (SETQ CH (APPLY FUNCTION ARGS)))
    (SETQ CH (APPLY FUNCTION ARGS)))
  (UNLESS (CONSP CH) (ADD-PROMPT CH))
  CH)

(DEFUN GET-PREFIX-COMMAND-COMTAB (X)
  "Given a prefix command X made by MAKE-EXTENDED-COMMAND, return its comtab."
  (SYMEVAL-IN-CLOSURE X 'COMTAB))

(DEFUN PREFIX-COMMAND-P (X)
  "T if X is a prefix command (a closure made by MAKE-EXTENDED-COMMAND)"
  (AND (CLOSUREP X)
       (EQ (%FIND-STRUCTURE-HEADER (CAR (CLOSURE-BINDINGS X))) 'COMTAB)))

(DEFUN MACRO-COMMAND-P (X)
  "T if X is a macro command made from a keyboard macro with MAKE-MACRO-COMMAND."
  (AND (CLOSUREP X)
       (let ((cb (closure-bindings x)))
         (and (cdr cb)          ;flush lexical closures
              (EQ (%FIND-STRUCTURE-HEADER (CAR cb)) 'SYMBOL)))))

(DEFUN MAKE-MACRO-COMMAND (THE-SYMBOL &OPTIONAL MOUSE-P)
  (LET-CLOSED ((SYMBOL (typecase the-symbol
                         (string (read-from-string THE-SYMBOL))
                         (otherwise the-symbol)))
               (MOVE-TO-MOUSE-P MOUSE-P)
               (PREVIOUS-COMMAND NIL))
    (LAMBDA ()
      (LET ((MAC (GET SYMBOL 'MACRO-STREAM-MACRO)))
        (OR MAC (BARF "The macro ~A is not defined." SYMBOL))
        (OR (OPERATION-HANDLED-P *STANDARD-INPUT* :MACRO-EXECUTE)
            (BARF "The input stream does not support macros."))
        (AND MOVE-TO-MOUSE-P
             (MOVE-BP (POINT) (MOUSE-BP *WINDOW* *MOUSE-X* *MOUSE-Y*)))
        (SEND *STANDARD-INPUT* :MACRO-EXECUTE MAC (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))
        DIS-NONE))))

(DEFUN MOUSE-MACRO-COMMAND-LAST-COMMAND (COMMAND)
  (OR (MACRO-COMMAND-P COMMAND)
      (BARF "That command is not a keyboard macro"))
  (SYMEVAL-IN-CLOSURE COMMAND 'PREVIOUS-COMMAND))

(DEFUN SET-MOUSE-MACRO-COMMAND-LAST-COMMAND (COMMAND OLD-VALUE)
  (OR (MACRO-COMMAND-P COMMAND)
      (BARF "That command is not a keyboard macro"))
  (SET-IN-CLOSURE COMMAND 'PREVIOUS-COMMAND OLD-VALUE))

;;; If we look up CHAR in COMTAB, would it be found in the comtab COMTAB-FOUND-IN-COMPARISON?
(DEFUN IN-THIS-COMTAB-P (COMTAB CHAR COMTAB-FOUND-IN-COMPARISON)
  (MULTIPLE-VALUE-BIND (NIL COMTAB-FOUND-IN)
      (COMMAND-LOOKUP CHAR COMTAB)
    (EQ COMTAB-FOUND-IN COMTAB-FOUND-IN-COMPARISON)))

;;>> bug -- this does not hack mouse commands yet. foo.
(DEFUN KEY-FOR-COMMAND (COMMAND &OPTIONAL (COMTAB *COMTAB*)
                        STARTING-CHAR STARTING-COMTAB
                        SUGGESTED-CHAR &AUX TEM)
  "Return a string describing the character to invoke COMMAND in COMTAB.
Returns NIL if there is no way.
The second value is the comtab that COMMAND was actually found in;
this is COMTAB or one of the comtabs it indirects to.
STARTING-CHAR and STARTING-COMTAB say where, in the sequence
to be searched for COMTAB, to start looking.  This is so you
can use the character and comtab values to resume the search.
You can use the SUGGESTED-CHAR to save time
by suggesting the place where the command standardly goes."
 (DECLARE (VALUES STRING CHARACTER COMTAB))
 (OR STARTING-CHAR (SETQ STARTING-CHAR 0))
 (OR STARTING-COMTAB (SETQ STARTING-COMTAB COMTAB))
 (BLOCK FOUND
   (IF SUGGESTED-CHAR
       (MULTIPLE-VALUE-BIND (COMMAND1 COMTAB1)
           (COMMAND-LOOKUP SUGGESTED-CHAR COMTAB)
         (IF (EQ COMMAND1 COMMAND)
             (RETURN-FROM FOUND
               (VALUES (FORMAT NIL "~@:C" SUGGESTED-CHAR)
                       SUGGESTED-CHAR
                       COMTAB1)))))
   (DO ((CTB STARTING-COMTAB (COMTAB-INDIRECT-TO CTB))
        (STARTING-CHAR STARTING-CHAR 0)
        KEYBOARD-ARRAY LENGTH MOUSE-ARRAY)
       ((NULL CTB))
     (SETQ KEYBOARD-ARRAY (COMTAB-KEYBOARD-ARRAY CTB)
           MOUSE-ARRAY (COMTAB-MOUSE-ARRAY CTB))
     (IF (NOT (ARRAYP KEYBOARD-ARRAY))
         (DOLIST (ELT KEYBOARD-ARRAY)
           (COND ((< (CAR ELT) STARTING-CHAR))
                 ((AND (EQ (CDR ELT) COMMAND)
                       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
                  (RETURN-FROM FOUND
                    (VALUES
                      (FORMAT NIL "~@:C" (CAR ELT))
                      (CAR ELT)
                      CTB)))
                 ((AND (EQ (CDR ELT) 'COM-DOCUMENTATION)
                       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB)
                       (SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
                  (RETURN-FROM FOUND
                    (VALUES
                      (FORMAT NIL "~:@C ~:@C" (CAR ELT) (CAR TEM))
                      (CAR ELT)
                      CTB)))
                 ((AND (TYPEP (CDR ELT) 'CLOSURE)       ;Redundant but should speed things up.
                       (PREFIX-COMMAND-P (CDR ELT))
                       (SETQ TEM
                             (KEY-FOR-COMMAND COMMAND
                                              (GET-PREFIX-COMMAND-COMTAB (CDR ELT))))
                       (IN-THIS-COMTAB-P COMTAB (CAR ELT) CTB))
                  (RETURN-FROM FOUND
                    (VALUES
                      (FORMAT NIL "~:@C ~A" (CAR ELT) TEM)
                      (CAR ELT)
                      CTB)))))
       (SETQ LENGTH (ARRAY-DIMENSION KEYBOARD-ARRAY 0))
       (DO ((BITS (CHAR-BITS STARTING-CHAR) (1+ BITS))
            (INCREMENT (ARRAY-DIMENSION KEYBOARD-ARRAY 1)))
           ((= BITS CHAR-BITS-LIMIT))
         (DO ((CH (IF (= BITS (CHAR-BITS STARTING-CHAR))
                      (CHAR-CODE STARTING-CHAR)
                    0)
                  (+ 1 CH))
              (OFFSET (IF (= BITS (CHAR-BITS STARTING-CHAR))
                          (* INCREMENT (CHAR-CODE STARTING-CHAR))
                          0)
                      (+ OFFSET INCREMENT))
              (PTR (ALOC KEYBOARD-ARRAY 0 BITS)))
             ((= CH LENGTH))
           (LET ((THIS-COM (%P-CONTENTS-OFFSET PTR OFFSET)))    ;Faster than AREF on 2d array!
             (COND ((AND (EQ THIS-COM COMMAND)
                         (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB))
                    (SETQ CH (MAKE-CHAR CH BITS))
                    (RETURN-FROM FOUND
                      (VALUES
                        (FORMAT NIL "~@:C" CH)
                        CH
                        CTB)))
                   ((AND (EQ THIS-COM 'COM-DOCUMENTATION)
                         (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB)
                         (SETQ TEM (RASSQ COMMAND *COM-DOCUMENTATION-ALIST*)))
                    (SETQ CH (MAKE-CHAR CH BITS))
                    (RETURN-FROM FOUND
                      (VALUES
                        (FORMAT NIL "~:@C ~:@C" CH (CAR TEM))
                        CH
                        CTB)))
                   ((AND (TYPEP THIS-COM 'CLOSURE)      ;Redundant but should speed things up.
                         (PREFIX-COMMAND-P THIS-COM)
                         (SETQ TEM
                               (KEY-FOR-COMMAND COMMAND
                                                (GET-PREFIX-COMMAND-COMTAB THIS-COM)))
                         (IN-THIS-COMTAB-P COMTAB (MAKE-CHAR CH BITS) CTB))
                    (SETQ CH (MAKE-CHAR CH BITS))
                    (RETURN-FROM FOUND
                      (VALUES
                        (FORMAT NIL "~:@C ~A" CH TEM)
                        CH
                        CTB)))))))))))

(DEFUN COMTAB-CHAR-INDIRECTION (CHAR &OPTIONAL (COMTAB *COMTAB*))
  "Return the character that CHAR indirects to in COMTAB.
This may be CHAR itself, if it is not indirect."
  (DO ((CH CHAR (MAKE-CHAR (CADR NCH) (CAR NCH)))
       (NCH))
      ((ATOM (SETQ NCH (COMMAND-LOOKUP CH COMTAB T)))
       CH)))

;;;; The command loop.

(DEFMETHOD (DISPLAYER :AROUND :EDIT) (CONT MT ARGS &OPTIONAL EDITOR-CLOSURE-1 &REST IGNORE)
  ;; Let the continuation find its own mapping table.
  ;; It saves hair, and the time is insignificant here.
  (DECLARE (IGNORE MT))
  (APPLY (OR EDITOR-CLOSURE-1 EDITOR-CLOSURE) CONT ARGS))

;character lossage
(DEFCONST EDITOR-INTERCEPTED-CHARACTERS
          '((#/ABORT EDITOR-INTERCEPT-ABORT)
            (#/M-ABORT EDITOR-INTERCEPT-ABORT-ALL)
            (#/BREAK EDITOR-INTERCEPT-BREAK)
            (#/M-BREAK EDITOR-INTERCEPT-ERROR-BREAK)))

(DEFUN EDITOR-INTERCEPT-BREAK (CHAR &REST IGNORE)
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
  (LET ((TYPEOUT-SELECTED
          (EQ TV:SELECTED-WINDOW *TYPEOUT-WINDOW*)))
    (LET ((*INSIDE-BREAK* T)
          (*EDITOR-IDLE* NIL))
      (TV:KBD-INTERCEPT-BREAK CHAR))
    (OR *INSIDE-BREAK*
        TYPEOUT-SELECTED
        (PROGN
          ;; Don't flush the typeout window if this is a recursive Break.
          (SEND-IF-HANDLES *STANDARD-OUTPUT* :MAKE-COMPLETE)
          (REDISPLAY-ALL-WINDOWS))))
  (VALUES CHAR T))

(DEFUN EDITOR-INTERCEPT-ERROR-BREAK (CHAR &REST IGNORE)
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
  (LET ((TYPEOUT-SELECTED
          (EQ TV:SELECTED-WINDOW *TYPEOUT-WINDOW*)))
    (LET ((*INSIDE-BREAK* T)
          (*EDITOR-IDLE* NIL))
      (TV:KBD-INTERCEPT-ERROR-BREAK CHAR))
    (OR *INSIDE-BREAK*
        TYPEOUT-SELECTED
        (PROGN
          (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)
          (REDISPLAY-ALL-WINDOWS))))
  (VALUES CHAR T))

(DEFUN EDITOR-INTERCEPT-ABORT-ALL (CHAR &REST IGNORE)
  (SEND-IF-HANDLES *QUERY-IO* :MAKE-COMPLETE)
  (TV:KBD-INTERCEPT-ABORT-ALL CHAR))

(DEFUN EDITOR-INTERCEPT-ABORT (CHAR &REST IGNORE)
  (SEND-IF-HANDLES *QUERY-IO* :MAKE-COMPLETE)
  (TV:KBD-INTERCEPT-ABORT CHAR))

;;; This is the function that does the actual work
(DEFMETHOD (WINDOW :EDIT) (&OPTIONAL IGNORE (*COMTAB* *COMTAB*)
                           (*MODE-LINE-LIST* *MODE-LINE-LIST*)
                           &OPTIONAL (TOP-LEVEL-P (SEND SELF :TOP-LEVEL-P))
                           &AUX
                           (PROMPT-ARRAY (MAKE-PROMPT-ARRAY))
                           (TV:KBD-INTERCEPTED-CHARACTERS EDITOR-INTERCEPTED-CHARACTERS))
  (TV:PROCESS-TYPEAHEAD (SEND SELF :IO-BUFFER)
                        (LAMBDA (CH)
                          (declare (sys:downward-function))
                          (COND ((ATOM CH) CH)
                                ((EQ (CAR CH) 'SELECT-WINDOW)
                                 (LEXPR-SEND SELF :PROCESS-SPECIAL-COMMAND CH)
                                 NIL)
                                ((MEMQ (CAR CH) '(CONFIGURATION-CHANGED REDISPLAY))
                                 NIL)
                                (T CH))))
  (UNWIND-PROTECT
      (PROGN
        (SEND (WINDOW-SHEET *WINDOW*)   ;Don't expose yet, but on first redisplay
              :START-DELAYED-SELECT)
        (SETQ *WINDOW-LIST* (FRAME-EXPOSED-WINDOWS))
        (REDISPLAY-MODE-LINE)           ;Do this once since may change size
        ;; Flush any typeout hiding this window.
        (IF (SEND (WINDOW-SHEET *WINDOW*) :EXPOSED-P)
            (PREPARE-WINDOW-FOR-REDISPLAY *WINDOW*))
        (CATCH 'RETURN-FROM-COMMAND-LOOP
          (CATCH (IF (EQ TOP-LEVEL-P T) 'EXIT-TOP-LEVEL 'EXIT-CONTROL-R)
            (DO-FOREVER
              (UNLESS
                (CATCH-ERROR-RESTART ((SYS:ABORT ERROR)
                                      (IF TOP-LEVEL-P
                                          "Return to top level editor command loop."
                                          "Return to editor command loop."))
                  (CATCH 'ZWEI-COMMAND-LOOP
                    (CATCH (IF (EQ TOP-LEVEL-P T) 'TOP-LEVEL 'DUMMY-TAG)
                      (CATCH (IF (EQ *WINDOW* *MINI-BUFFER-WINDOW*)
                                 'DUMMY-TAG 'NON-MINIBUFFER-LEVEL)
                        (PROG (CH)
                              (SETQ *LAST-COMMAND-TYPE* *CURRENT-COMMAND-TYPE*
                                    *CURRENT-COMMAND-TYPE* NIL
                                    *NUMERIC-ARG* 1
                                    *NUMERIC-ARG-P* NIL
                                    *NUMERIC-ARG-N-DIGITS* 0
                                    *MARK-STAYS* NIL
                                    *MINI-BUFFER-COMMAND* NIL)
                              (CLEAR-PROMPTS)
                              (REDISPLAY-ALL-WINDOWS)
                              (SEND *TYPEIN-WINDOW* :COMMAND-LOOP-REDISPLAY)
                              (SETQ *CENTERING-FRACTION* *CENTER-FRACTION*)
                           UNREAL-COMMAND               ;arguments loop back here
                              (LET ((*EDITOR-IDLE* TOP-LEVEL-P))
                                (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
                                  (SETQ CH (INPUT-WITH-PROMPTS *STANDARD-INPUT* :ANY-TYI))))
                              (TYPECASE CH
                                (NULL           ;If EOF, return
                                 (RETURN NIL))
                                (CONS           ;Handle mouse, etc
                                 (SETQ *LAST-COMMAND-CHAR* CH)
                                 (COND ((NOT (LEXPR-SEND SELF :PROCESS-SPECIAL-COMMAND CH))
                                        ;; Here to NOT flush typeout!
                                        ;; First redisplay any windows not covered by typeout.
                                        (REDISPLAY-ALL-WINDOWS NIL NIL)
                                        (IF *NUMERIC-ARG-P* (GO UNREAL-COMMAND))
                                        ;; If no numeric arg, must flush next char if space.
                                        ;; Now wait for a char if there is any typeout
                                        ;;  even if it is "complete".
                                        ;; Aside from that, we do the same redisplay-related
                                        ;; things that real commands do.
                                        (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT T)
                                        (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
                                        (REDISPLAY-ALL-WINDOWS)
                                        (GO UNREAL-COMMAND))))
                                ((OR FIXNUM CHARACTER)  ;Keyboard or mouse character
;; character lossage
                                 (IF (FIXNUMP CH) (SETQ CH (INT-CHAR CH)))
                                 (WHEN (EQ :ARGUMENT (SEND SELF :PROCESS-COMMAND-CHAR CH))
                                   (INCF *NUMERIC-ARG-N-DIGITS*)
                                   (REDISPLAY-ALL-WINDOWS NIL NIL)
                                   ;; If there is typeout, the windows under it
                                   ;; have not been redisplayed.
                                   ;; Now redisplay will not happen until (at least)
                                   ;; the next input char, but Space will not be ignored
                                   ;; since it ought to execute with this numeric arg.
                                   (GO UNREAL-COMMAND))))
                              ;; If there is typeout (window-typeout-stream style) that the user
                              ;; hasn't finished reading, wait for a character, and unread it
                              ;; unless it is a space.
                              (LET ((*EDITOR-IDLE* TOP-LEVEL-P))
                                (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)))))
                    T))
                (SEND-IF-HANDLES *STANDARD-INPUT* :MACRO-ERROR))
              ;; If we Control-Z from BREAK or an error, and after every command,
              ;; say it is ok to flush the typeout window.
              (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
              (WHEN *MINI-BUFFER-COMMAND*
                (MINI-BUFFER-RING-PUSH *MINI-BUFFER-COMMAND*))))))
    ;; unwind-protect cleanup
    (SEND (WINDOW-SHEET *WINDOW*) :FLUSH-DELAYED-SELECT)))

(DEFMETHOD (WINDOW :PROCESS-COMMAND-CHAR) (CH)
  (PROCESS-COMMAND-CHAR CH))

(DEFMETHOD (WINDOW :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY #'PROCESS-SPECIAL-COMMAND ARGS))

(DEFUN PROCESS-COMMAND-CHAR (CH &AUX VALUE LINE INDEX)
  "Process the character CH as an editor command.
CH should be a keyboard character or mouse character, not a list."
;character lossage
  (IF (NUMBERP CH) (SETQ CH (INT-CHAR CH)))
  (SETQ *LAST-COMMAND-CHAR* CH)
  ;; Look up the command in the table.
  (LET ((*CURRENT-COMMAND* (COMMAND-LOOKUP *LAST-COMMAND-CHAR* *COMTAB*)))
    ;; Execute the command.
    (MULTIPLE-VALUE (VALUE LINE INDEX)
      (COMMAND-EXECUTE *CURRENT-COMMAND* *LAST-COMMAND-CHAR* NIL *COMMAND-HOOK*))
    ;; This command is creating the argument to a subsequent command.
    (COND ((EQ VALUE ':ARGUMENT)
           VALUE)
          (T
           ;; If the mark is not being preserved, make it go away.
           (WHEN (AND (NOT *MARK-STAYS*) (WINDOW-MARK-P *WINDOW*))
             (SETF (WINDOW-MARK-P *WINDOW*) NIL)
             (MUST-REDISPLAY *WINDOW* DIS-MARK-GOES))
           ;; Report the returned value of the command to the window.
           (MUST-REDISPLAY *WINDOW* VALUE LINE INDEX)
           ;; Call the post-command hooks
           (DOLIST (HOOK *POST-COMMAND-HOOK*)
             (FUNCALL HOOK *LAST-COMMAND-CHAR*))))))

;;; This handles special commands from the window system
;;; returns non-NIL if the typeout window should be flushed like normal commands.
;;; Note: a mouse sensitive item typeout-execute blip function
;;; must return NIL to cause the typeout to be flushed!
(DEFSELECT (PROCESS-SPECIAL-COMMAND UNKNOWN-SPECIAL-COMMAND)
  (REDISPLAY ()
    ;The window is presumably on our list of windows and will get redisplayed
    ;in the normal course of events when buffered input had been processed.
    NIL)
  (SELECT-WINDOW (WINDOW)
    (PROG1 (NEQ WINDOW *WINDOW*)
           (MAKE-WINDOW-CURRENT WINDOW)))
  (CONFIGURATION-CHANGED ()
    (LET ((FEW (FRAME-EXPOSED-WINDOWS)))
      (UNLESS (MEMQ *WINDOW* FEW)
        (MAKE-WINDOW-CURRENT (CAR FEW))))
    NIL)
  (SCROLL (WINDOW NLINES TYPE)
    (IF (EQ TYPE ':RELATIVE)
        (RECENTER-WINDOW-RELATIVE WINDOW NLINES)
        (RECENTER-WINDOW WINDOW :START
                         (FORWARD-LINE (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))
                                       NLINES T)))
    (UNLESS (EQ WINDOW *WINDOW*)
      ;; Scrolling nonselected window => flush typeout on it
      ;; because the main loop won't do it except for the selected window.
      (PREPARE-WINDOW-FOR-REDISPLAY WINDOW))
    T)
  (:MOUSE-BUTTON (CH WINDOW *MOUSE-X* *MOUSE-Y*)
    (IF (NOT (TYPEP WINDOW 'ZWEI))
        (WHEN (CHAR= CH #/MOUSE-R)
          (TV:MOUSE-CALL-SYSTEM-MENU))
      (DECF *MOUSE-X* (TV:SHEET-INSIDE-LEFT (WINDOW-SHEET WINDOW)))
      (DECF *MOUSE-Y* (TV:SHEET-INSIDE-TOP (WINDOW-SHEET WINDOW)))
      (SEND-IF-HANDLES *STANDARD-INPUT* :RECORD CH)
      (IF *MOUSE-HOOK*
          (FUNCALL *MOUSE-HOOK* WINDOW CH *MOUSE-X* *MOUSE-Y*)
        (IF (NEQ WINDOW *WINDOW*)               ;Given in another window,
            (LET ((*COMTAB* (IF (EQ *WINDOW* *MINI-BUFFER-WINDOW*) *STANDARD-COMTAB* *COMTAB*))
                  (*LAST-COMMAND-TYPE* NIL)     ;dont confuse mouse mark thing, and
                  *CURRENT-COMMAND-TYPE*
                  (*WINDOW* WINDOW)
                  (*INTERVAL* (WINDOW-INTERVAL WINDOW)))        ;temporarily act there (mini-buffer)
              (PROCESS-COMMAND-CHAR CH))
          (PROCESS-COMMAND-CHAR CH)))
      T))
  ((:TYPEOUT-EXECUTE :EXECUTE) (FUNCTION &REST ARGS)
   (LET ((*MINI-BUFFER-DONT-RECORD* T))
     ;; We would not be able to repeat the command anyway.
     (NOT (APPLY FUNCTION ARGS)))))

(defvar *throw-unknown-special-command* nil)

(DEFUN UNKNOWN-SPECIAL-COMMAND (TYPE &REST REST)
; (DECLARE (IGNORE REST))                       ;not needed
  (if (and *throw-unknown-special-command*
           (eq type 'summary-mouse))
      (*throw *throw-unknown-special-command* (cadar rest))
    (CERROR "Ignore it" "ZMACS error: ~~S is not a valid special editor command~" TYPE)))

(DEFUN TYI-WITH-SCROLLING (&OPTIONAL MOUSE-OR-KBD-TYI-P ANY-TYI-P)
  "Read a character, allowing scroll bar commands to be given and executed.
We do not return when scrolling is done, but keep waiting for
some other sort of input.
ANY-TYI-P non-NIL says return anything but scroll commands;
Otherwise MOUSE-OR-KBD-TYI-P non-NIL says return mouse characters;
otherwise ignore them."
  (DO-FOREVER
    (LET ((CH (SEND *STANDARD-INPUT* :ANY-TYI)))
      (COND ((NUMBERP CH)
             (RETURN (VALUES CH CH)))
; character lossage
            ((characterp ch)
             (return (values (int-char ch) ch)))
            ((ATOM CH))
            ((EQ (CAR CH) 'SCROLL)
             (APPLY #'PROCESS-SPECIAL-COMMAND CH))
            (ANY-TYI-P (RETURN CH))
            ((AND MOUSE-OR-KBD-TYI-P (EQ (CAR CH) ':MOUSE-BUTTON))
             (RETURN (VALUES (SECOND CH) CH)))))))

(DEFUN TYI-WITH-SCROLLING-AND-MOUSING ()
  "Read a keyboard character, processing any mouse commands that are given."
  (LOOP
    (MULTIPLE-VALUE-BIND (CH REAL-CH)
        (TYI-WITH-SCROLLING T)
      (IF (TV:CHAR-MOUSE-P CH)
          (APPLY #'PROCESS-SPECIAL-COMMAND REAL-CH)
        (RETURN CH)))))

(DEFUN CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT (&OPTIONAL WAIT-IF-EXPOSED)
  "If the typeout window is incomplete, wait until an input character is available.
WAIT-IF-EXPOSED non-NIL says do so if typeout window is exposed
even if it is not incomplete."
  (COND ((SEND-IF-HANDLES *STANDARD-OUTPUT* :NEVER-FLUSH-TYPEOUT))
        ((IF WAIT-IF-EXPOSED
             (SEND *STANDARD-OUTPUT* :EXPOSED-P)
           (TYPEOUT-WINDOW-INCOMPLETE-P *STANDARD-OUTPUT*))
         (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
           (DO ((CHAR (SEND *STANDARD-INPUT* :ANY-TYI) (SEND *STANDARD-INPUT* :ANY-TYI)))
               ((NOT (AND ;; Ignore requests to select current window
                          (EQ (CAR-SAFE CHAR) 'SELECT-WINDOW)
                          (EQ (CADR CHAR) *WINDOW*)))
                (OR (EQ CHAR (CHAR-INT #/SPACE))
                    ;; If it's not a space, unread it.  That will
                    ;; prevent immediate redisplay.
                    (SEND *STANDARD-INPUT* :UNTYI CHAR))))))))

(DEFUN CONTROL-R (&AUX (COMTAB *COMTAB*))
  "Do a recursive edit on the same buffer."
  (UNLESS (EQ COMTAB *CONTROL-R-COMTAB*)
    (SET-COMTAB-INDIRECTION *CONTROL-R-COMTAB* COMTAB)
    (SETQ COMTAB *CONTROL-R-COMTAB*))
  (SEND *WINDOW* :EDIT NIL COMTAB `("[" ,@*MODE-LINE-LIST* "]") NIL))

(DEFUN RECURSIVE-EDIT (INTERVAL MODE &OPTIONAL POINT
                                     &AUX (WINDOW (CREATE-OVERLYING-WINDOW *WINDOW*)))
  "Do a recursive edit on INTERVAL in MODE, starting at POINT.
MODE is a string to put in the mode line.
Uses another ZWEI window which overlies the selected one."
  (SEND WINDOW :SET-INTERVAL INTERVAL)
  (AND POINT (MOVE-BP (WINDOW-POINT WINDOW) POINT))
  (TEMPORARY-WINDOW-SELECT (WINDOW)
    (BIND-MODE-LINE `("[" ,MODE "]")
      (LET ((*COMTAB* *RECURSIVE-EDIT-COMTAB*))
        (SEND WINDOW :EDIT)))))

(defflavor barf () (dbg:format-condition-mixin condition)
  (:documentation "All calls to ZWEI:BARF signal this condition, normally ignored."))

(DEFUN BARF (&OPTIONAL CTL-STRING &REST ARGS)
  "Report an error to the editor user.  Throws to the command loop.
The args are a FORMAT string and args for FORMAT.
Signals the condition ZWEI:BARF, passing along the args to BARF."
  (IF (NOT (AND (VARIABLE-BOUNDP *WINDOW*) *WINDOW*)) ;There might not be a window, if preloading
      (FERROR CTL-STRING ARGS)
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)          ;May have altered the text before erring
    (IF (EH:CONDITION-NAME-HANDLED-P 'BARF)     ;>> Why bother doing this?
        (MULTIPLE-VALUE-CALL (LAMBDA (TEM1 &REST TEM2)
                               (IF TEM1 (RETURN-FROM BARF (VALUES-LIST TEM2))))
                             (SIGNAL 'BARF :FORMAT-STRING CTL-STRING
                                           :FORMAT-ARGS (COPY-LIST ARGS))))
    (WHEN (SEND-IF-HANDLES *STANDARD-INPUT* :MACRO-ERROR)
      (FORMAT *QUERY-IO* "~&Keyboard macro terminated by error."))
    (BEEP NIL *QUERY-IO*)
    (WHEN CTL-STRING
      (SEND *QUERY-IO* :FRESH-LINE)
      (APPLY #'FORMAT *QUERY-IO* CTL-STRING ARGS))
    (THROW 'ZWEI-COMMAND-LOOP T)))


;;;; The actual command tables used by the implemented ZWEI subsystems.

(DEFUN INITIALIZE-STANDARD-COMTABS ()
  (SETQ *STANDARD-COMTAB*
         (SET-COMTAB 'STANDARD-COMTAB
          '((0 #o200) COM-ORDINARILY-SELF-INSERT
            #/BS COM-ORDINARILY-SELF-INSERT
            #/C-F COM-FORWARD
            #/C-B COM-BACKWARD
            #/C-N COM-DOWN-REAL-LINE
            #/C-P COM-UP-REAL-LINE
            #/C-V COM-NEXT-SCREEN
            #/M-V COM-PREVIOUS-SCREEN
            #/C-M-V COM-SCROLL-OTHER-WINDOW
            #/C-A COM-BEGINNING-OF-LINE
            #/C-E COM-END-OF-LINE
            #/M-R COM-MOVE-TO-SCREEN-EDGE
            #/M-< COM-GOTO-BEGINNING
            #/M-> COM-GOTO-END
            #/C-SP COM-SET-POP-MARK
            #/C-@ COM-SET-POP-MARK
            #/M-SP COM-PUSH-POP-POINT-EXPLICIT
            #/C-M-SP COM-MOVE-TO-PREVIOUS-POINT
            #/CR COM-INSERT-CRS
            #/C-O COM-MAKE-ROOM
            #/C-M-O COM-SPLIT-LINE
            #/M-O COM-THIS-INDENTATION
            #/M-^ COM-DELETE-INDENTATION
            #/C-M-^ COM-DELETE-INDENTATION
            #/C-D COM-DELETE-FORWARD
            #/RUBOUT COM-RUBOUT
            #/C-RUBOUT COM-TAB-HACKING-RUBOUT
            #/C-K COM-KILL-LINE
            #/CLEAR COM-CLEAR
            #/BREAK COM-BREAK
            #/M-W COM-SAVE-REGION
            #/C-W COM-KILL-REGION
            #/C-M-W COM-APPEND-NEXT-KILL
            #/C-Y COM-YANK
            #/M-Y COM-YANK-POP
            #/C-L COM-RECENTER-WINDOW
            #/FF COM-COMPLETE-REDISPLAY
            #/C-M-! COM-COMPLETE-REDISPLAY
            #/C-U COM-UNIVERSAL-ARGUMENT
            #/C-- COM-NEGATE-NUMERIC-ARG
            #/M-- COM-NEGATE-NUMERIC-ARG
            #/M-C-- COM-NEGATE-NUMERIC-ARG
            #/S-- COM-NEGATE-NUMERIC-ARG
            #/S-C-- COM-NEGATE-NUMERIC-ARG
            #/S-M-- COM-NEGATE-NUMERIC-ARG
            #/S-M-C-- COM-NEGATE-NUMERIC-ARG
            #/H-- COM-NEGATE-NUMERIC-ARG
            #/C-- COM-NEGATE-NUMERIC-ARG
            #/H-M-- COM-NEGATE-NUMERIC-ARG
            #/H-M-C-- COM-NEGATE-NUMERIC-ARG
            #/H-S-- COM-NEGATE-NUMERIC-ARG
            #/H-S-C-- COM-NEGATE-NUMERIC-ARG
            #/H-S-M-- COM-NEGATE-NUMERIC-ARG
            #/H-S-M-C-- COM-NEGATE-NUMERIC-ARG
            (#/C-0 10.) COM-NUMBERS
            (#/M-0 10.) COM-NUMBERS
            (#/C-M-0 10.) COM-NUMBERS
            (#/S-0 10.) COM-NUMBERS
            (#/S-C-0 10.) COM-NUMBERS
            (#/S-M-0 10.) COM-NUMBERS
            (#/S-M-C-0 10.) COM-NUMBERS
            (#/H-0 10.) COM-NUMBERS
            (#/H-C-0 10.) COM-NUMBERS
            (#/H-M-0 10.) COM-NUMBERS
            (#/H-M-C-0 10.) COM-NUMBERS
            (#/H-S-0 10.) COM-NUMBERS
            (#/H-S-C-0 10.) COM-NUMBERS
            (#/H-S-M-0 10.) COM-NUMBERS
            (#/H-S-M-C-0 10.) COM-NUMBERS
            #/C-T COM-EXCHANGE-CHARACTERS
            #/M-T COM-EXCHANGE-WORDS
            #/C-M-T COM-EXCHANGE-SEXPS
            #/M-F COM-FORWARD-WORD
            #/M-B COM-BACKWARD-WORD
            #/M-K COM-KILL-SENTENCE
            #/M-D COM-KILL-WORD
            #/M-RUBOUT COM-BACKWARD-KILL-WORD
            #/M-@ COM-MARK-WORD
            #/C-M-F COM-FORWARD-SEXP
            #/C-M-N COM-FORWARD-LIST
            #/C-M-B COM-BACKWARD-SEXP
            #/C-M-P COM-BACKWARD-LIST
            #/C-M-K COM-KILL-SEXP
            #/C-M-RUBOUT COM-BACKWARD-KILL-SEXP
            #/C-M-@ COM-MARK-SEXP
            #/C-M-/) COM-FORWARD-UP-LIST
            #/C-M-/( COM-BACKWARD-UP-LIST
            #/C-M-U COM-BACKWARD-UP-LIST
            #/C-M-[ COM-BEGINNING-OF-DEFUN
            #/C-M-] COM-END-OF-DEFUN
            #/C-M-A COM-BEGINNING-OF-DEFUN
            #/C-M-E COM-END-OF-DEFUN
            #/C-M-D COM-DOWN-LIST
            #/C-/( COM-FIND-UNBALANCED-PARENTHESES
            #/C-/) COM-SHOW-LIST-START
            #/M-/( COM-MAKE-/(/)
            #/M-/) COM-MOVE-OVER-/)
            #/C-M-Shift-K COM-DELETE-/(/)
            #/C-M-Shift-F COM-GROW-LIST-FORWARD
            #/C-M-Shift-B COM-GROW-LIST-BACKWARD
            #/M-] COM-FORWARD-PARAGRAPH
            #/M-[ COM-BACKWARD-PARAGRAPH
            #/M-H COM-MARK-PARAGRAPH
            #/M-E COM-FORWARD-SENTENCE
            #/M-A COM-BACKWARD-SENTENCE
            #/C-G COM-BEEP
            #/TAB COM-INSERT-TAB
            #/C-M-TAB COM-INDENT-FOR-LISP
            #/C-TAB COM-INDENT-DIFFERENTLY
            #/LF COM-INDENT-NEW-LINE
            #/C-M-Q COM-INDENT-SEXP
            #/C-/; COM-INDENT-FOR-COMMENT
            #/M-/; COM-INDENT-FOR-COMMENT
            #/C-M-/; COM-KILL-COMMENT
            #/M-N COM-DOWN-COMMENT-LINE
            #/M-P COM-UP-COMMENT-LINE
            #/c-sh-M com-macro-expand-expression
            #/m-sh-M com-macro-expand-expression-all
            #/M-Q COM-FILL-PARAGRAPH
            #/M-G COM-FILL-REGION
            #/C-/\ COM-JUST-ONE-SPACE
            #/M-/\ COM-DELETE-HORIZONTAL-SPACE
            #/M-CR COM-BACK-TO-INDENTATION
            #/M-M COM-BACK-TO-INDENTATION
            #/C-M-CR COM-BACK-TO-INDENTATION
            #/C-M-M COM-BACK-TO-INDENTATION
            #/M-U COM-UPPERCASE-WORD
            #/M-L COM-LOWERCASE-WORD
            #/M-C COM-UPPERCASE-INITIAL
            #/C-M-/\ COM-INDENT-REGION
            #/M-FF COM-INSERT-FF
            #/M-TAB COM-INSERT-TAB
            #/M-S COM-CENTER-LINE
            #/M-= COM-COUNT-LINES-REGION
            #/C-= COM-FAST-WHERE-AM-I
            #/C-S COM-INCREMENTAL-SEARCH
            #/C-R COM-REVERSE-INCREMENTAL-SEARCH
            #/M- COM-EVALUATE-MINI-BUFFER
            #/C- COM-COMPILE-REGION
            #/C-SH-C COM-COMPILE-REGION
            #/C-SH-E COM-EVALUATE-REGION
            #/M-SH-E COM-EVALUATE-REGION-VERBOSE
            #/C-M-SH-E COM-EVALUATE-REGION-HACK
;           #/C-? COM-SELF-DOCUMENT
            #/M-? COM-SELF-DOCUMENT
            #/C-M-? COM-DOCUMENTATION
            #/HELP COM-DOCUMENTATION
            #/C-HELP COM-DOCUMENTATION
            #/C-Q COM-QUOTED-INSERT
            #/M-X COM-EXTENDED-COMMAND
            #/C-M-X COM-ANY-EXTENDED-COMMAND
            #/c-sh-x com-various-quantities
            #/C-< COM-MARK-BEGINNING
            #/C-> COM-MARK-END
            #/M-LF COM-INDENT-NEW-COMMENT-LINE
            #/C-% COM-REPLACE-STRING
            #/M-% COM-QUERY-REPLACE
            #/C-M-H COM-MARK-DEFUN
            #/C-M-R COM-REPOSITION-WINDOW
            #/M-/' COM-UPCASE-DIGIT
            #/C-SH-S COM-LISP-MATCH-SEARCH
            #/C-Z COM-QUIT
;           #/END COM-QUIT
            #/ABORT COM-ABORT-AT-TOP-LEVEL
            #/C-M-& COM-FROB-LISP-CONDITIONAL
            #/C-M-$ COM-FROB-DO
            #/C-SH-A COM-QUICK-ARGLIST
            #/C-SH-D COM-QUICK-DOCUMENTATION
            #/M-SH-D COM-LONG-DOCUMENTATION
            #/C-SH-V COM-DESCRIBE-VARIABLE-AT-POINT
            #/C-J COM-CHANGE-FONT-CHAR
            #/M-J COM-CHANGE-FONT-WORD
            #/C-M-J COM-CHANGE-DEFAULT-FONT
            #/C-SH-J COM-CHANGE-FONT-REGION
            #/M-SH-J COM-CHANGE-ONE-FONT-REGION
            #/M-/# COM-TEXT-JUSTIFIER-CHANGE-FONT-WORD
            #/M-_ COM-TEXT-JUSTIFIER-UNDERLINE-WORD
            #/M-$ COM-CORRECT-WORD-SPELLING
            #/C-M-/# COM-GOTO-CHARACTER
            #/M-SH-P COM-QUICK-PRINT-BUFFER
            #/C-SH-U COM-QUICK-UNDO
            #/C-SH-R COM-QUICK-REDO
            #/MOUSE-1-1 COM-MOUSE-MARK-REGION
            #/MOUSE-1-2 COM-MOUSE-MOVE-REGION
            #/MOUSE-2-1 COM-MOUSE-MARK-THING
            #/MOUSE-2-2 COM-MOUSE-KILL-YANK
            )
          (MAKE-COMMAND-ALIST
           '(;; COM*:
             COM-INSTALL-COMMAND COM-SET-KEY COM-DEFINE-CHARACTER COM-UNDEFINE-CHARACTER COM-ARGLIST
             COM-COUNT-LINES COM-COUNT-WORDS COM-COUNT-CHARACTERS
             COM-QUERY-REPLACE COM-REPLACE-STRING
             COM-HOW-MANY COM-COUNT-OCCURRENCES COM-OCCUR COM-LIST-MATCHING-LINES
             COM-KEEP-LINES COM-DELETE-NON-MATCHING-LINES
             COM-FLUSH-LINES COM-DELETE-MATCHING-LINES
             COM-COMPILE-REGION COM-COMPILE-BUFFER COM-EVALUATE-REGION COM-EVALUATE-BUFFER
             COM-VIEW-REGISTER COM-LIST-REGISTERS COM-KILL-REGISTER
             COM-GET-REGISTER COM-PUT-REGISTER
             COM-LIST-VARIABLES COM-VARIABLE-APROPOS COM-DESCRIBE-VARIABLE COM-SET-VARIABLE
             COM-GRIND-DEFINITION COM-GRIND-EXPRESSION
             COM-EVALUATE-INTO-BUFFER com-evaluate-and-replace-into-buffer com-evaluate-and-print-into-buffer
             COM-TRACE COM-UNTRACE
             COM-VIEW-MAIL
             COM-ATOM-QUERY-REPLACE COM-FORMAT-CODE COM-MULTIPLE-QUERY-REPLACE
             COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER COM-QUERY-EXCHANGE
             COM-QUERY-REPLACE-LAST-KILL COM-QUERY-REPLACE-LET-BINDING
             COM-FIND-UNBALANCED-PARENTHESES
             COM-MACRO-EXPAND-EXPRESSION COM-MACRO-EXPAND-EXPRESSION-ALL
             COM-UNDO COM-REDO COM-DISCARD-UNDO-INFORMATION
             COM-FILL-LONG-COMMENT
             COM-KILL-COMMENTS-IN-REGION
             COM-INDENT-UNDER COM-INDENT-NESTED COM-INDENT-RELATIVE
             COM-SORT-LINES COM-SORT-PAGES COM-SORT-PARAGRAPHS COM-SORT-VIA-KEYBOARD-MACROS
             COM-EXECUTE-COMMAND-INTO-BUFFER COM-INSERT-DATE COM-DISASSEMBLE
             COM-UPPERCASE-LISP-CODE-IN-REGION COM-LOWERCASE-LISP-CODE-IN-REGION
             COM-REVERSE-LINES COM-REVERSE-FOLLOWING-LIST COM-STACK-LIST-VERTICALLY
             COM-PRINT-BUFFER COM-PRINT-REGION COM-PRINT-ALL-BUFFERS COM-DISSOCIATED-PRESS
             COM-MAKE-LOCAL-VARIABLE COM-KILL-LOCAL-VARIABLE COM-LIST-LOCAL-VARIABLES
             com-correct-spelling
             ;; DOC:
             COM-LIST-COMMANDS COM-APROPOS COM-WHERE-IS COM-DESCRIBE-COMMAND
             COM-GENERATE-WALLCHART
             com-long-documentation
             ;; FILES:
             COM-INSERT-FILE COM-INSERT-FILE-NO-FONTS
             com-write-region-to-file
             COM-APPEND-TO-FILE COM-PREPEND-TO-FILE
             COM-VIEW-FILE COM-LIST-FILES COM-PRINT-FILE
             COM-RENAME-FILE COM-DELETE-FILE COM-UNDELETE-FILE
             COM-COPY-FILE COM-COPY-TEXT-FILE COM-COPY-BINARY-FILE
             COM-LOAD-FILE
             ;; DIRS:
             COM-LIST-ALL-DIRECTORY-NAMES COM-VIEW-DIRECTORY COM-EXPUNGE-DIRECTORY
             COM-VIEW-LOGIN-DIRECTORY
             ;; MODES:
             COM-LISP-MODE com-common-lisp-mode
             COM-TEXT-MODE COM-FUNDAMENTAL-MODE COM-PL1-MODE COM-BOLIO-MODE
             COM-TEX-MODE COM-TECO-MODE COM-MACSYMA-MODE
             COM-ELECTRIC-PL1-MODE COM-ATOM-WORD-MODE COM-EMACS-MODE COM-OVERWRITE-MODE
             COM-ANY-BRACKET-MODE COM-AUTO-FILL-MODE COM-WORD-ABBREV-MODE
             COM-INSERT-WORD-ABBREVS COM-KILL-ALL-WORD-ABBREVS COM-LIST-WORD-ABBREVS
             COM-KILL-MODE-WORD-ABBREV COM-KILL-GLOBAL-WORD-ABBREV
             COM-DEFINE-WORD-ABBREVS COM-EDIT-WORD-ABBREVS COM-LIST-SOME-WORD-ABBREVS
             COM-WRITE-WORD-ABBREV-FILE COM-READ-WORD-ABBREV-FILE COM-MAKE-WORD-ABBREV
             COM-ADD-MODE-WORD-ABBREV COM-ADD-GLOBAL-WORD-ABBREV
             COM-EDIT-TAB-STOPS COM-MIDAS-MODE COM-RETURN-INDENTS-MODE
             COM-ELECTRIC-SHIFT-LOCK-MODE COM-ELECTRIC-FONT-LOCK-MODE
             ;; FONT, KBDMAC, DIRED
             COM-SET-FONTS
             COM-INSTALL-MACRO COM-INSTALL-MOUSE-MACRO
             COM-VIEW-KEYBOARD-MACRO COM-NAME-LAST-KEYBOARD-MACRO
             ))))
  (SETQ *STANDARD-CONTROL-X-COMTAB*
        (SET-COMTAB 'STANDARD-CONTROL-X-COMTAB
                    '(#/C-G COM-PREFIX-BEEP
                      #/C-D COM-DISPLAY-DIRECTORY
                      #/C-N COM-SET-GOAL-COLUMN
                      #/C-P COM-MARK-PAGE
                      #/C-X COM-SWAP-POINT-AND-MARK
                      #/G COM-GET-REGISTER
                      #/X COM-PUT-REGISTER
                      #/S COM-SAVE-POSITION-IN-REGISTER
                      #/J COM-JUMP-TO-REGISTER-POSITION
                      #/L COM-COUNT-LINES-PAGE
                      #/RUBOUT COM-BACKWARD-KILL-SENTENCE
                      #/C-/; COM-COMMENT-OUT-REGION
                      #/; COM-SET-COMMENT-COLUMN
                      #/. COM-SET-FILL-PREFIX
                      #/F COM-SET-FILL-COLUMN
                      #/C-U COM-UPPERCASE-REGION
                      #/C-L COM-LOWERCASE-REGION
                      #/C-O COM-DELETE-BLANK-LINES
                      #/C-I COM-INDENT-RIGIDLY
                      #/= COM-WHERE-AM-I
                      #/[ COM-PREVIOUS-PAGE
                      #/] COM-NEXT-PAGE
                      #/H COM-MARK-WHOLE
                      #/C-C COM-QUIT
                      #/C-J COM-CHANGE-FONT-REGION
                      #/( COM-START-KEYBOARD-MACRO
                      #/) COM-END-KEYBOARD-MACRO
                      #/E COM-CALL-LAST-KEYBOARD-MACRO
                      #/Q COM-KEYBOARD-MACRO-QUERY
                      #/ COM-REPEAT-MINI-BUFFER-COMMAND
                      #/C-T COM-EXCHANGE-LINES
                      #/T COM-EXCHANGE-REGIONS
                      #/U COM-UNEXPAND-LAST-WORD
                      #/# COM-TEXT-JUSTIFIER-CHANGE-FONT-REGION
                      #/_ COM-TEXT-JUSTIFIER-UNDERLINE-REGION
                      #/C-M-SP COM-MOVE-TO-DEFAULT-PREVIOUS-POINT
                      #/HELP COM-DOCUMENT-CONTAINING-PREFIX-COMMAND
                      #/ABORT COM-PREFIX-ABORT
                      )))
  (SET-COMTAB-CONTROL-INDIRECTION *STANDARD-CONTROL-X-COMTAB*)
  (SET-COMTAB *STANDARD-COMTAB*
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *STANDARD-CONTROL-X-COMTAB*)))
  (SETQ *COMPLETING-READER-COMTAB*
        (SET-COMTAB 'COMPLETING-READER-COMTAB
                    '(#/ COM-COMPLETE
                      #/SP COM-SELF-INSERT-AND-COMPLETE
                      #/. COM-SELF-INSERT-AND-COMPLETE
                      #/C-? COM-LIST-COMPLETIONS
                      #/C-Q COM-QUOTED-INSERT
                      #/HELP COM-DOCUMENT-COMPLETING-READ
                      #/C-// COM-COMPLETION-APROPOS
                      #/CR COM-COMPLETE-AND-EXIT
                      #/C-G COM-MINI-BUFFER-BEEP
                      #/ABORT COM-RECURSIVE-EDIT-ABORT
                      #/C-CR COM-COMPLETE-AND-EXIT
                      #/END COM-COMPLETE-AND-EXIT-IF-UNIQUE
                      #/MOUSE-1-1 COM-MOUSE-END-OF-MINI-BUFFER
                      #/MOUSE-3-1 COM-MOUSE-LIST-COMPLETIONS
                      #/C-SH-Y COM-YANK-DEFAULT-STRING
                      #/C-M-Y COM-YANK-PREVIOUS-INPUT
                      #/M-SH-Y COM-POP-MINI-BUFFER-HISTORY
                      #/C-SH-F COM-SPECIFY-FILE-BUFFER
                      #/C-Z :UNDEFINED
                      #/M-Z :UNDEFINED
                      #/C-M-Z :UNDEFINED)))
  (SET-COMTAB-INDIRECTION *COMPLETING-READER-COMTAB* *STANDARD-COMTAB*)
  (SETQ *CONTROL-R-COMTAB*
        (SET-COMTAB 'CONTROL-R-COMTAB
                    '(#/C- COM-EXIT-CONTROL-R
                      #/END COM-EXIT-CONTROL-R
                      #/ABORT COM-EXIT-CONTROL-R)))
  (SET-COMTAB-INDIRECTION *CONTROL-R-COMTAB* *STANDARD-COMTAB*)
  (SETQ *RECURSIVE-EDIT-COMTAB*
        (SET-COMTAB 'RECURSIVE-EDIT-COMTAB
                    '(#/C- COM-EXIT-CONTROL-R
                      #/END COM-EXIT-CONTROL-R
                      #/C-G COM-RECURSIVE-EDIT-BEEP
                      #/ABORT COM-RECURSIVE-EDIT-ABORT)))
  (SET-COMTAB-INDIRECTION *RECURSIVE-EDIT-COMTAB* *STANDARD-COMTAB*)
  (SETQ *STANDALONE-COMTAB*
        (SET-COMTAB 'STANDALONE-COMTAB
                    '(#/END COM-QUIT
                      #/ABORT COM-STANDALONE-ABORT
                      #/C- COM-QUIT)))
  (SET-COMTAB-INDIRECTION *STANDALONE-COMTAB* *STANDARD-COMTAB*)
  )

(DEFCONST MAKE-COMMAND-ALIST-FLAG NIL
  "T => MAKE-COMMAND-LIST does not complain about commands that are not defined.")

(DEFUN MAKE-COMMAND-ALIST (COMMAND-LIST)
  "Given a list of commands (symbols), return an alist of command name vs. command.
The command name for COM-ARGLIST is /"Arglist/", a string.
The specified ordering is reversed."
  (DO ((CL COMMAND-LIST (CDR CL))
       (RET NIL (LET ((NAME (OR (GET (CAR CL) 'COMMAND-NAME)
                                (AND MAKE-COMMAND-ALIST-FLAG
                                     (MAKE-COMMAND-NAME (CAR CL)))
                                (CERROR T NIL NIL "No command-name string is recorded for command ~S.  Continue with a string or NIL" (CAR CL)))))
                  (IF NAME
                      (CONS (CONS NAME (CAR CL))
                            RET)
                    RET))))
      ((NULL CL) (NREVERSE RET))))

;;;; Mouse prompting stuff
(DEFUN COMTAB-MOUSE-PROMPT (COMTAB STRING &AUX (INHIBIT-SCHEDULING-FLAG T))
  (SETF (FILL-POINTER STRING) 0)
  (DO ((BUTTON 0 (1+ BUTTON))
       (NAMES '(#/L #/M #/R) (CDR NAMES))
       (FIRST-P T))
      (( BUTTON 3)
       (OR FIRST-P (VECTOR-PUSH-EXTEND #/. STRING)))
    (DO ((CLICKS 0 (1+ CLICKS))
         (COMMAND) (PROMPT))
        (( CLICKS 2))
      (COND ((OR (AND (SETQ COMMAND (COMMAND-LOOKUP (TV:MAKE-MOUSE-CHAR BUTTON CLICKS)
                                                    COMTAB))
                      (OR (SETQ PROMPT (GET COMMAND ':MOUSE-SHORT-DOCUMENTATION))
                          (AND (MENU-COMMAND-P COMMAND)
                               (SETQ PROMPT "Menu"))))
                 (AND (= BUTTON 2) (= CLICKS 1)
                      (SETQ PROMPT "System menu")))
             (IF FIRST-P
                 (SETQ FIRST-P NIL)
                 (APPEND-TO-ARRAY STRING ", "))
             (VECTOR-PUSH-EXTEND (CAR NAMES) STRING)
             (AND (> CLICKS 0) (VECTOR-PUSH-EXTEND #/2 STRING))
             (VECTOR-PUSH-EXTEND #/: STRING)
             (APPEND-TO-ARRAY STRING PROMPT)))))
  STRING)

;;;; Syntax table stuff.

(DEFUN CHAR-SYNTAX (CHAR SYNTAX-TABLE)
  "Return the syntax code for CHAR in SYNTAX-TABLE."
  (SETQ CHAR (CHAR-CODE CHAR))
  (COND ((ARRAYP SYNTAX-TABLE)
         (AREF SYNTAX-TABLE CHAR))
        ((CDR (ASSQ CHAR (CDR SYNTAX-TABLE))))
        ((CHAR-SYNTAX CHAR (CAR SYNTAX-TABLE)))))

(DEFUN SET-CHAR-SYNTAX (SYNTAX SYNTAX-TABLE CHAR &AUX TEM)
  "Set the syntax code for CHAR in SYNTAX-TABLE to SYNTAX."
  (SETQ CHAR (CHAR-CODE CHAR))
  (COND ((ARRAYP SYNTAX-TABLE)
         (SETF (AREF SYNTAX-TABLE CHAR) SYNTAX))
        ((SETQ TEM (ASSQ CHAR (CDR SYNTAX-TABLE)))
         (SETF (CDR TEM) SYNTAX))
        (T
         (PUSH (CONS CHAR SYNTAX) (CDR SYNTAX-TABLE)))))

(DEFUN MAKE-SPARSE-SYNTAX-TABLE (INDIRECT-TO)
  "Create a syntax table whose data are represented as changes from INDIRECT-TO.
INDIRECT-TO is another syntax table."
  (NCONS INDIRECT-TO))

(DEFUN MAKE-SYNTAX-TABLE (SPECS)
  "Create a syntax table.  SPECS specifies the initial contents.
The format of SPECS is like the second arg to SET-COMTAB."
  (DO ((SPECS SPECS (CDR SPECS))
       (SPEC)
       (I 0)
       (TABLE (MAKE-ARRAY #o400 :ELEMENT-TYPE '(MOD 16.))))
      ((NULL SPECS)
       (IF (NOT (= I #o400))
           (FERROR "Wrong number (~S) of elements in the specs" I))
       TABLE)
    (SETQ SPEC (CAR SPECS))
    (COND ((SYMBOLP SPEC)
           (SETF (AREF TABLE I) (SYMBOL-VALUE SPEC))
           (INCF I))
          (T (DO ((J 0 (1+ J))
                  (VALUE (SYMEVAL (SECOND SPEC)))
                  (LIMIT (FIRST SPEC)))
                 (( J LIMIT))
               (SETF (AREF TABLE I) VALUE)
               (INCF I))))))

(DEFUN INITIALIZE-SYNTAX-TABLES ()
  (SETQ *WORD-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE
                              '((#o40 WORD-ALPHABETIC)
                                WORD-DELIMITER          ;040 space
                                WORD-DELIMITER          ;041 !
                                WORD-DELIMITER          ;042 "
                                WORD-DELIMITER          ;043 #
                                WORD-ALPHABETIC         ;044 $
                                WORD-ALPHABETIC         ;045 %
                                WORD-DELIMITER          ;046 &
                                WORD-DELIMITER          ;047 '
                                WORD-DELIMITER          ;050 (
                                WORD-DELIMITER          ;051 )
                                WORD-DELIMITER          ;052 *
                                WORD-DELIMITER          ;053 +
                                WORD-DELIMITER          ;054 ,
                                WORD-DELIMITER          ;055 -
                                WORD-DELIMITER          ;056 .
                                WORD-DELIMITER          ;057 /
                                (10. WORD-ALPHABETIC)   ;Digits
                                WORD-DELIMITER          ;072 :
                                WORD-DELIMITER          ;073 ;
                                WORD-DELIMITER          ;074 <
                                WORD-DELIMITER          ;075 =
                                WORD-DELIMITER          ;076 >
                                WORD-DELIMITER          ;077 ?
                                WORD-DELIMITER          ;100 @
                                (26. WORD-ALPHABETIC)   ;Uppercase letters
                                WORD-DELIMITER          ;133 [
                                WORD-DELIMITER          ;134 \
                                WORD-DELIMITER          ;135 ]
                                WORD-DELIMITER          ;136 ^
                                WORD-DELIMITER          ;137 _
                                WORD-DELIMITER          ;140 `
                                (26. WORD-ALPHABETIC)   ;Lowercase letters
                                (#o205 WORD-DELIMITER)))
        *LIST-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE
                              '((#o40 LIST-ALPHABETIC)
                                LIST-DELIMITER          ;040 space
                                LIST-ALPHABETIC         ;041 !
                                LIST-DOUBLE-QUOTE       ;042 "
                                LIST-SINGLE-QUOTE       ;043 #
                                LIST-ALPHABETIC         ;044 $
                                LIST-ALPHABETIC         ;045 %
                                LIST-ALPHABETIC         ;046 &
                                LIST-SINGLE-QUOTE       ;047 '
                                LIST-OPEN               ;050 (
                                LIST-CLOSE              ;051 )
                                LIST-ALPHABETIC         ;052 *
                                LIST-ALPHABETIC         ;053 +
                                LIST-SINGLE-QUOTE       ;054 ,
                                LIST-ALPHABETIC         ;055 -
                                LIST-ALPHABETIC         ;056 .
                                LIST-SLASH              ;057 /
                                (10. LIST-ALPHABETIC)   ;Digits
                                LIST-COLON              ;072 :
                                LIST-COMMENT            ;073 ;
                                LIST-ALPHABETIC         ;074 <
                                LIST-ALPHABETIC         ;075 =
                                LIST-ALPHABETIC         ;076 >
                                LIST-ALPHABETIC         ;077 ?
                                LIST-SINGLE-QUOTE       ;100 @
                                (26. LIST-ALPHABETIC)   ;Uppercase letters
                                LIST-ALPHABETIC         ;133 [
                                LIST-ALPHABETIC         ;134 \
                                LIST-ALPHABETIC         ;135 ]
                                LIST-ALPHABETIC         ;136 ^
                                LIST-ALPHABETIC         ;137 _
                                LIST-SINGLE-QUOTE       ;140 `
                                (26. LIST-ALPHABETIC)   ;Lowercase letters
                                LIST-ALPHABETIC         ;173 {
                                LIST-DOUBLE-QUOTE       ;174 |
                                LIST-ALPHABETIC         ;175 }
                                LIST-ALPHABETIC         ;176 ~
                                LIST-ALPHABETIC         ;177 

                                LIST-ALPHABETIC         ;200 null
                                LIST-DELIMITER          ;201 break
                                LIST-DELIMITER          ;202 clear
                                LIST-DELIMITER          ;203 call
                                LIST-DELIMITER          ;204 escape
                                LIST-DELIMITER          ;205 backnext
                                LIST-DELIMITER          ;206 help
                                LIST-DELIMITER          ;207 rubout
                                LIST-ALPHABETIC         ;210 bs
                                LIST-DELIMITER          ;211 tab
                                LIST-DELIMITER          ;212 line
                                LIST-DELIMITER          ;213 vt
                                LIST-DELIMITER          ;214 form
                                LIST-DELIMITER          ;215 return
                                (#o162 LIST-ALPHABETIC)))
        *ATOM-WORD-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE
                                   '((#o40 LIST-ALPHABETIC)
                                     WORD-DELIMITER     ;040 space
                                     WORD-ALPHABETIC    ;041 !
                                     WORD-ALPHABETIC    ;042 "
                                     WORD-ALPHABETIC    ;043 #
                                     WORD-ALPHABETIC    ;044 $
                                     WORD-ALPHABETIC    ;045 %
                                     WORD-ALPHABETIC    ;046 &
                                     WORD-DELIMITER     ;047 '
                                     WORD-DELIMITER     ;050 (
                                     WORD-DELIMITER     ;051 )
                                     WORD-ALPHABETIC    ;052 *
                                     WORD-ALPHABETIC    ;053 +
                                     WORD-DELIMITER     ;054 ,
                                     WORD-ALPHABETIC    ;055 -
                                     WORD-ALPHABETIC    ;056 .
                                     WORD-ALPHABETIC    ;057 /
                                     (10. WORD-ALPHABETIC)      ;Digits
                                     WORD-ALPHABETIC    ;072 :
                                     WORD-DELIMITER     ;073 ;
                                     WORD-ALPHABETIC    ;074 <
                                     WORD-ALPHABETIC    ;075 =
                                     WORD-ALPHABETIC    ;076 >
                                     WORD-ALPHABETIC    ;077 ?
                                     WORD-DELIMITER     ;100 @
                                     (26. WORD-ALPHABETIC)      ;Uppercase letters
                                     WORD-ALPHABETIC    ;133 [
                                     WORD-ALPHABETIC    ;134 \
                                     WORD-ALPHABETIC    ;135 ]
                                     WORD-ALPHABETIC    ;136 ^
                                     WORD-ALPHABETIC    ;137 _
                                     WORD-DELIMITER     ;140 `
                                     (31. WORD-ALPHABETIC)      ;Lowercase letters
                                     (#o200 WORD-DELIMITER)))))

;;;; Initialization stuff

;;; This initializes all ZWEI globals.  This hacks the ones common to all
;;; ZWEI, but not the ZMACS ones.  It first sets some unusual things, then
;;; initializes the ZWEI variables (the ones defined with DEFVAR in the MACROS file),
;;; then sets up the incremental search command (which has some magic globals).
;;; Finally it sets up the comtabs and syntax tables, and the minibuffer window.
(DEFUN INITIALIZE-ZWEI-GLOBALS ()
  (UNLESS (FIND-PACKAGE "ZWEI Utility Package")
    (SETQ *UTILITY-PACKAGE* (MAKE-PACKAGE "ZWEI Utility Package" :USE ())))
  (DOLIST (VAR *GLOBAL-INITIALIZATION-LIST*)    ;Reset other variables defined by DEFGLOBAL
    (SET (CAR VAR) (CDR VAR)))
  (SETQ-ZWEI-VARIABLES)
  (INITIALIZE-WORD-ABBREV-TABLE)
  (INITIALIZE-INCREMENTAL-SEARCH-GLOBALS)
  (INITIALIZE-STANDARD-COMTABS)
  (INITIALIZE-SYNTAX-TABLES)
  (INITIALIZE-MINI-BUFFER)
  (INITIALIZE-TAB-STOP-BUFFER)
  (INITIALIZE-MOUSE)
  (SETQ *PATHNAME-DEFAULTS* (FS:MAKE-PATHNAME-DEFAULTS)
        *AUX-PATHNAME-DEFAULTS* (FS:MAKE-PATHNAME-DEFAULTS)))

(DEFUN INITIALIZE-MINI-BUFFER ()
   (SETQ *MINI-BUFFER-HISTORY*
        (MAKE-HISTORY "mini buffer command history"
                      :ELEMENT-STRING-FUNCTION 'SUMMARIZE-MINI-BUFFER-COMMAND
                      :YANK-METHOD 'MINI-BUFFER-HISTORY-YANK))
  (SETQ *MINI-BUFFER-REPEATED-COMMAND* NIL)
  (SETQ *MINI-BUFFER-MULTI-LINE-COMTAB*
        (SET-COMTAB 'MINI-BUFFER-MULTI-LINE-COMTAB
                    '(#/HELP COM-DOCUMENT-CONTAINING-COMMAND
                      #/C-CR COM-END-OF-MINI-BUFFER
                      #/END COM-END-OF-MINI-BUFFER
                      #/C-G COM-MINI-BUFFER-BEEP
                      #/ABORT COM-RECURSIVE-EDIT-ABORT
                      #/C-SH-Y COM-YANK-DEFAULT-STRING
                      #/C-M-Y COM-YANK-PREVIOUS-INPUT
                      #/C-SH-S COM-YANK-SEARCH-STRING
                      #/C-Z :UNDEFINED
                      #/M-Z :UNDEFINED
                      #/C-M-Z :UNDEFINED
                      #/M-SH-Y COM-POP-MINI-BUFFER-HISTORY
                      #/MOUSE-1-2 COM-MOUSE-END-OF-MINI-BUFFER
                      )))
  (SET-COMTAB-INDIRECTION *MINI-BUFFER-MULTI-LINE-COMTAB* *STANDARD-COMTAB*)
  (SETQ *MINI-BUFFER-COMTAB* (SET-COMTAB 'MINI-BUFFER-COMTAB '(#/RETURN COM-END-OF-MINI-BUFFER)))
  (SET-COMTAB-INDIRECTION *MINI-BUFFER-COMTAB* *MINI-BUFFER-MULTI-LINE-COMTAB*)
  (SETQ *PATHNAME-READING-COMTAB*
        (SET-COMTAB 'PATHNAME-READING-COMTAB
                    '(#/ COM-PATHNAME-COMPLETE
                      #/C-? COM-PATHNAME-LIST-COMPLETIONS
                      #/C-SH-Y COM-YANK-DEFAULT-PATHNAME
                      #/END COM-PATHNAME-COMPLETE-AND-EXIT-IF-UNIQUE
                      #/HELP COM-DOCUMENT-PATHNAME-READ)))
  (SET-COMTAB-INDIRECTION *PATHNAME-READING-COMTAB* *MINI-BUFFER-COMTAB*))

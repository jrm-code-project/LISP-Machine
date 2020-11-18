;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;;      ZWEI Definitions (ZWEI Was EINE Initially)

;;; This history stuff must be here,
;;; since MAKE-HISTORY is used by some of the following DEFGLOBALs.
(DEFSTRUCT (HISTORY :NAMED-ARRAY (:CONC-NAME HISTORY-) (:CONSTRUCTOR MAKE-HISTORY-INTERNAL)
                    (:ALTERANT NIL)
                    (:PRINT-FUNCTION
                      (LAMBDA (HISTORY STREAM IGNORE)
                        (SYS:PRINTING-RANDOM-OBJECT (HISTORY STREAM :TYPE :NO-POINTER)
                          (FORMAT STREAM "~a. Length ~S Yank pointer ~S"
                                  (HISTORY-NAME HISTORY) (HISTORY-LENGTH HISTORY)
                                  (HISTORY-YANK-POINTER HISTORY))))))
  (NAME NIL :DOCUMENTATION "Name of this history. A string.")
  (YANK-POINTER NIL :DOCUMENTATION "Current position for yanking from history")
  (LENGTH 0 :DOCUMENTATION "Length of history.")
  (LIST NIL :DOCUMENTATION "Actual elements of history.")
  (ELEMENT-STRING-FUNCTION NIL
   :DOCUMENTATION "Function to extract an element of HISTORY (as a string)")
  (YANK-METHOD NIL
   :DOCUMENTATION "Function to yank element from HISTORY"))

(DEFUN MAKE-HISTORY (NAME &KEY
                     (ELEMENT-STRING-FUNCTION 'IDENTITY)
                     (YANK-METHOD 'YANK-AS-TEXT)
                     (CLEAR-P T))
  "Return a new history list, which records objects and which you can yank from.
NAME is a string, in which all words are normally lower case.
ELEMENT-STRING-FUNCTION is a function to describe elements in the history, briefly.
It should return some Lisp object, which will be handed to PRINC.
YANK-METHOD is a function to be used to yank an object recorded in the history.
It should take arguments like ZWEI:YANK-AS-TEXT (the default yank-function).
If CLEAR-P is NIL, this history will not be recorded to be cleared on cold boot."
  (LET ((HISTORY (MAKE-HISTORY-INTERNAL :NAME NAME
                                        :ELEMENT-STRING-FUNCTION ELEMENT-STRING-FUNCTION
                                        :YANK-METHOD YANK-METHOD)))
    (IF CLEAR-P (PUSH HISTORY *HISTORIES-TO-CLEAR*))
    HISTORY))

;;; Global parameters.  These variables will have global values,
;;; and any editor may bind or not bind them as it pleases.
;;; NOTE: Those specials that are ZWEI variables are defined with DEFVARIABLE
;;; in the MACROS file.  They are of the same status as these variables.
(DEFCONST *GLOBAL-INITIALIZATION-LIST* ()
  "Alist of variable names and initial value forms, for ZWEI DEFGLOBALs.")

(DEFMACRO DEFGLOBAL (VAR &OPTIONAL INITIAL-VALUE DOCUMENTATION)
  "Define a ZWEI global variable.  Like DEFVAR, but reinitting ZWEI resets the variable."
  `(PROGN 'COMPILE
     (DEFVAR ,VAR ,INITIAL-VALUE
       ,DOCUMENTATION)
     . ,(AND (NEQ INITIAL-VALUE ':UNBOUND)
             `((PUSH (CONS ',VAR ,INITIAL-VALUE) *GLOBAL-INITIALIZATION-LIST*)))))

;;; This kludge, instead of a DEFGLOBAL, causes INITIALIZE-ZWEI-GLOBALS to be called within
;;; INITIALIZE-ZMACS-IF-NECESSARY.
(DEFVAR *TICK* :UNBOUND "A time-stamp, incremented whenever text is changed.")
(PUSH '(*TICK* . 0) *GLOBAL-INITIALIZATION-LIST*)

(DEFPARAMETER *ZWEI-AREA* ()
  "The area in which ZWEI makes lines, nodes, and intervals.")
(DEFPARAMETER *ZWEI-LINE-AREA* () "The area in which to make lines.")
(ADD-INITIALIZATION
  "Create ZWEI-LINE-AREA."
  '(SETQ *ZWEI-LINE-AREA* (MAKE-AREA :NAME 'ZWEI-LINE-AREA :REGION-SIZE #o1000000 :VOLATILITY 2)
         *ZWEI-AREA* *ZWEI-LINE-AREA*)
  '(:ONCE :NOW))

(DEFGLOBAL *WORD-SYNTAX-TABLE* :UNBOUND
  "Array of length 256. which is the word syntax of all basic characters.
Possible values are the values of WORD-ALPHABETIC and WORD-DELIMITER.")
(DEFGLOBAL *LIST-SYNTAX-TABLE* :UNBOUND
  "Array of length 256. which is the list syntax of all basic characters.
The possible values of array elements have names: the variables
LIST-ALPHABETIC, LIST-DELIMITER, LIST-SLASH, LIST-DOUBLE-QUOTE, LIST-SINGLE-QUOTE,
 LIST-CLOSE, LIST-OPEN, LIST-COMMENT, and LIST-COLON.")
(DEFGLOBAL *ATOM-WORD-SYNTAX-TABLE* :UNBOUND
  "This is like *WORD-SYNTAX-TABLE*, and is put there while in Atom Word Mode.")
(DEFGLOBAL *UTILITY-PACKAGE* :UNBOUND
  "A package used for symbols which are used as internal data structure by ZWEI.")
(DEFGLOBAL *Q-REG-LIST* NIL "List of q-registers currently defined.")
(DEFGLOBAL *STANDARD-COMTAB* :UNBOUND
  "A comtab which defines the standard set of harmless ZWEI commands.")
(DEFGLOBAL *STANDARD-CONTROL-X-COMTAB* :UNBOUND
  "A comtab which defines the simple control-X commands.")
(DEFGLOBAL *COMPLETING-READER-COMTAB* :UNBOUND
  "Comtab for the completing reader environment.")
(DEFGLOBAL *PATHNAME-READING-COMTAB* :UNBOUND
  "Comtab for reading pathnames in mini-buffer.")
(DEFGLOBAL *CONTROL-R-COMTAB* :UNBOUND
  "Comtab for recursive edits on the same buffer.")
(DEFGLOBAL *RECURSIVE-EDIT-COMTAB* :UNBOUND
  "Comtab for recursive edits on a new buffer.")
(DEFGLOBAL *STANDALONE-COMTAB* :UNBOUND
  "Comtab for simple standalone editors.")
(DEFGLOBAL *WORD-ABBREV-TABLE* :UNBOUND
  "Array of length 256., indexed by basic character.
Nonzero means the character expands abbrevs.")
(DEFGLOBAL *PREVIOUS-MODE-LINE* NIL
  "List of strings that make up the state of mode line.")
(DEFGLOBAL *TAB-STOP-BUFFER* :UNBOUND
  "Bufferof tab-stop definitions, used by Tab to Tab Stop.")

;;;We don't want the following four things to be DEFGLOBAL's because we don't want them reset
;;;to their initial values when (INITIALIZE-ZWEI-GLOBALS) is called.  The three histories are
;;;initialized here and never changed.  The list of *HISTORIES-TO-CLEAR* is initialized to NIL,
;;;but we don't want to forget things added to it before (INITIALIZE-ZWEI-GLOBALS) is called
;;;by (INITIALIZE-ZMACS)
(DEFVAR *HISTORIES-TO-CLEAR* NIL "List of histories to clear on saving band.")
(DEFVAR *KILL-HISTORY*
            (MAKE-HISTORY "kill history"
                          :ELEMENT-STRING-FUNCTION
                          'SUMMARIZE-KILL-HISTORY-INTERVAL)
  "History of intervals of killed text.")
(DEFVAR *DEFINITION-NAME-HISTORY*
           (MAKE-HISTORY "definition name argument history"
                         :ELEMENT-STRING-FUNCTION 'PRIN1-TO-STRING)
  "History of definition names read by READ-FUNCTION-NAME.")
(DEFVAR *PATHNAME-ARGUMENT-HISTORY*
           (MAKE-HISTORY "pathname argument history")
  "History of pathnames read in the mini buffer.")

(DEFGLOBAL *MINI-BUFFER-HISTORY* :UNBOUND
  "History of previous mini-buffer commands.")
(DEFGLOBAL *SEARCH-RING* NIL "List of strings searched for recently.")
(DEFGLOBAL *LISP-PARSE-PREPARSED-FLAG* NIL
  "If this is T, LISP-PARSE-FROM-DEFUN assumes that the lines are already parsed.")
(DEFGLOBAL *MOUSE-X* :UNBOUND
  "Position of the mouse for the previous command, relative to *WINDOW*'s margins.")
(DEFGLOBAL *MOUSE-Y* :UNBOUND
  "Position of the mouse for the previous command, relative to *WINDOW*'s margins.")
(DEFGLOBAL *BATCH-UNDO-SAVE* NIL
  "T while inside WITH-UNDO-SAVE.")
(DEFGLOBAL *PATHNAME-DEFAULTS* :UNBOUND
  "A defaults-alist for FS:MERGE-PATHNAME-DEFAULTS for important things like C-X C-F.")
(DEFGLOBAL *AUX-PATHNAME-DEFAULTS* :UNBOUND
  "A defaults-alist for FS:MERGE-PATHNAME-DEFAULTS for minor things like Insert File.")
(DEFGLOBAL *WORD-ABBREV-FILE-NAME* NIL
  "Last file used for reading or saving word abbrevs.")
(DEFGLOBAL *WORD-ABBREV-FILE-TICK* -1
  "*TICK* at which word abbrevs were last read or saved.")
(DEFGLOBAL *WORD-ABBREV-TICK* -1
  "*TICK* at which word abbrevs were last modified.")
(DEFGLOBAL *IN-COM-DOC-P* NIL
  "T => We are inside COM-DOCUMENTATION.")
(DEFGLOBAL *REPEAT-DOC-P* NIL
  "T => COM-DOCUMENTATION is repeating what it did last.")
(DEFVAR *QUANTITY-MODE* :UNBOUND
  "Kind of quantity to operate on, inside COM-VARIOUS-QUANTITIES.")
(DEFVAR *READ-ONLY-SUPPRESSED-INTERVAL* NIL
  "If non-NIL, it's an interval that temporarily may be modified even if read-only.")
(DEFVAR *INSIDE-BREAK* NIL
  "T when inside BREAK inside an editor window.")
(DEFVAR *TAB-WIDTH* NIL
  "Non-NIL => specifies width of tab, in spaces, for indentation calculations.")

(DEFVAR SYN-TYPEIN-WINDOW-IO (MAKE-SYN-STREAM '*TYPEIN-WINDOW*))

(PROCLAIM '(SPECIAL *COM-DOCUMENTATION-ALIST*)) ;DEFVAR in DOC.
(PROCLAIM '(SPECIAL *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*))  ;DEFVAR in MOUSE.
(PROCLAIM '(SPECIAL *COMPLETING-DELIMS*))       ;DEFVAR in COMD.

(DEFVAR *EDITOR-IDLE* NIL
  "Bound to T inside an editor that is reading commands at top level.")

(DEFMACRO COMMAND-BUFFER-PUSH (THING)
  `(TV:IO-BUFFER-PUT TV:IO-BUFFER ,THING))

;;; Redisplay levels.  These are symbolic constants.
;;; They have global values and should never be bound.
(DEFCONSTANT DIS-NONE 0
  "As value from a command, or to MUST-REDISPLAY, means: No redisplay needed.")
(DEFCONSTANT DIS-MARK-GOES 1
  "As value from a command, or to MUST-REDISPLAY, means: Consider flushing region-marking")
(DEFCONSTANT DIS-BPS 2
  "As value from a command, or to MUST-REDISPLAY, means: Point and Mark may be changed.")
(DEFCONSTANT DIS-LINE 3
  "To MUST-REDISPLAY, means: Changes within one line.
Should be accompanied by additional arguments to MUST-REDISPLAY
giving the line, and the index at which changes start.
If a command returns DIS-LINE, it should return a line and index as additional values.")
(DEFCONSTANT DIS-TEXT 4
  "As value from a command, or to MUST-REDISPLAY, means: Text may have changed.")
(DEFCONSTANT DIS-ALL 5
  "As value from a command, or to MUST-REDISPLAY, means: Window parameters may have changed.
The window must be completely redisplayed.")

;;; Syntax codes in *LISP-SYNTAX-TABLE*
(DEFCONSTANT LIST-ALPHABETIC 0
  "In an element of *LIST-SYNTAX-TABLE*, means: this char is part of an atom.")
(DEFCONSTANT LIST-DELIMITER 1
  "In an element of *LIST-SYNTAX-TABLE*, means: this char separates things but has no other significance.")
(DEFCONSTANT LIST-SLASH 2
  "In an element of *LIST-SYNTAX-TABLE*, means: this char quotes the following character.")
(DEFCONSTANT LIST-DOUBLE-QUOTE 3
  "In an element of *LIST-SYNTAX-TABLE*, means: this char starts a grouping terminated by another of itself.")
(DEFCONSTANT LIST-SINGLE-QUOTE 4
  "In an element of *LIST-SYNTAX-TABLE*, means: this char is part of whatever follows it.")
(DEFCONSTANT LIST-CLOSE 5
  "In an element of *LIST-SYNTAX-TABLE*, means: this char acts like a close parenthesis.")
(DEFCONSTANT LIST-OPEN 6
  "In an element of *LIST-SYNTAX-TABLE*, means: this char acts like an open parenthesis.")
(DEFCONSTANT LIST-COMMENT 7
  "In an element of *LIST-SYNTAX-TABLE*, means: this char starts a comment.")
(DEFCONSTANT LIST-COLON 10
  "In an element of *LIST-SYNTAX-TABLE*, means: this char ends a package prefix.")

;;; Syntax codes in *WORD-SYNTAX-TABLE*.
(DEFCONSTANT WORD-ALPHABETIC 0
  "In an element of *WORD-SYNTAX-TABLE*, means: this char is part of words.")
(DEFCONSTANT WORD-DELIMITER 1
  "In an element of *WORD-SYNTAX-TABLE*, means: this char separates words.")


(DEFVAR *MINI-BUFFER-COMMAND* NIL
  "Accumulates the mini-buffer values read by this editor command.
This is actually an editor closure variable.  It
Starts from NIL for each editor command,
 but can accumulate multiple minibuffers within one command.
The value looks like ((command-function-symbol *numeric-arg-p* *numeric-arg* . prompts)
                       mini-buffer-string1 chars1 mini-buffer-string2 chars2 ...)
The mini-buffer-strings are the strings returned by the mini buffers
invoked by the command.  They are pushed on as the mini buffers return.
The CHARS lists are lists of characters that are read in between the minibuffers,
that are not part of the mini buffers.  They are present in the list in the
order they were typed.")

(DEFVAR *MINI-BUFFER-DONT-RECORD* NIL
  "T means do not record mini buffer input on *MINI-BUFFER-COMMAND*.
Bound to T by the pop-up mini buffer.")

(DEFVAR *MINI-BUFFER-END-POINTER* NIL
  "The IO-BUFFER-RECORD-POINTER (plus one) when the last mini buffer was exited.
Used if another mini buffer is entered, to collect the characters
typed in between the two mini buffers out of the IO-BUFFER-RECORD.")

(DEFVAR *OUTER-LEVEL-MINI-BUFFER-COMMAND* :UNBOUND
  "While inside the mini buffer, this saves the outer value of *MINI-BUFFER-COMMAND*.
The latter variable is rebound by EDIT-IN-MINI-BUFFER.")

(DEFVAR *MINI-BUFFER-COMMAND-IN-PROGRESS* NIL
  "In this value can be found the symbol for the command that invoked the minibuffer.")

(DEFVAR *MINI-BUFFER-ARG-DOCUMENTER* NIL
  "This, if not NIL, should be a function to document the current minibuffer.
It should print what the argument is for or how to type it.")

(DEFVAR *MINI-BUFFER-DEFAULT-STRING* NIL
  "String for C-Shift-Y to yank.")

(DEFVAR *MINI-BUFFER-VALUE-HISTORY* NIL
  "NIL, or history for C-M-Y to yank from.")

(DEFVAR *MINI-BUFFER-MULTI-LINE-COMTAB* :UNBOUND
  "Comtab for mini buffers where Return does not exit.")
(DEFVAR *MINI-BUFFER-COMTAB* :UNBOUND
  "Default comtab for mini buffers.")

(DEFVAR *MINI-BUFFER-REPEATED-COMMAND* :UNBOUND
  "Rest (part not yet repeated) of *MINI-BUFFER-ENTIRE-REPEATED-COMMAND*.")
(DEFVAR *MINI-BUFFER-ENTIRE-REPEATED-COMMAND* NIL
  "If non-NIL, this is a mini buffer command being repeated.
See the comments in SYS:ZWEI;COMC.")

(DEFVAR *ZMACS-BUFFER-LIST* :UNBOUND
  "List of live buffers selectable in ZMACS windows.
Changes to this list should be made inside WITHOUT-INTERRUPTS.")
(DEFVAR *ZMACS-BUFFER-COUNTER* 0
  "Counter used in generating buffer names such as Buffer-1.")
(DEFVAR *ZMACS-COMTAB* :UNBOUND
  "Main comtab for ZMACS")
(DEFVAR *ZMACS-CONTROL-X-COMTAB* :UNBOUND
  "Comtab for Control-X prefix in ZMACS.  Indirects to *STANDARD-CONTROL-X-COMTAB*.")
(DEFVAR *ZMACS-BUFFER-NAME-READING-COMTAB* :UNBOUND
  "Comtab for mini buffer when reading a buffer name.")
(DEFVAR *ZMACS-BUFFER-NAME-ALIST* :UNBOUND
  "Alist of ZMACS buffer names vs buffers, for completion in C-X B.
Changes to this list should be made inside WITHOUT-INTERRUPTS.")
(DEFVAR *ZMACS-COMPLETION-AARRAY* :UNBOUND
  "Aarray for completion of function names.")
(DEFVAR *ZMACS-TAG-TABLE-ALIST* "List of tag tables read in.")

(DEFVAR *ALL-ZMACS-WINDOWS* NIL
  "List of all ZMACS windows created.")

(DEFVAR *ZMACS-CURRENT-TAG-TABLE* NIL
  "The selected tag table for Tags Search, etc., or NIL if none.
A tag table is a pathname, which has a ZMACS-TAG-TABLE-FILE-SYMBOLS property
which is a list of pathnames of files included.")

(DEFVAR *TYPEOUT-COMMAND-ALIST* NIL
  "The item-type-alist for typeout windows of editor windows.")

;;;; Definitions of the ZWEI data structures.

;;; An editor is a command loop and the associated variables.
;;; Since almost all the code in the editor runs within one, all their
;;; variables are declared special.

(DEFVAR *INTERVAL* NIL
  "The interval which is being edited.")
(DEFVAR *WINDOW* :UNBOUND
  "The selected window in this editor.")
(DEFVAR *WINDOW-LIST* :UNBOUND
  "List of windows to be displayed by this editor.")
(DEFVAR *POINT* :UNBOUND
  "The point BP of *WINDOW*.")
(DEFVAR *MARK* :UNBOUND
  "The mark BP of *WINDOW*.")
(DEFVAR *COMTAB* :UNBOUND
  "Current comtab for looking up command characters.")
(DEFVAR *FONT* 0
  "Current font for self-inserting characters.")
(DEFVAR *NUMERIC-ARG* 1
  "The value of the numeric argument, or 1.")
(DEFVAR *NUMERIC-ARG-P* NIL
  "T if there is a numeric argument to this or the following command.")
(DEFVAR *NUMERIC-ARG-N-DIGITS* :UNBOUND
  "Number of characters typed to get this command's arg.")
(DEFVAR *LAST-COMMAND-CHAR* :UNBOUND
  "The character typed to get this command.")
(DEFVAR *CURRENT-COMMAND* NIL
  "The command (symbol) now being executed.")
(DEFVAR *CURRENT-COMMAND-TYPE* NIL
  "The /"type/" (a symbol) of the current command.")
(DEFVAR *LAST-COMMAND-TYPE* NIL
  "The /"type/" of the previous command executed.")
(DEFVAR *REAL-LINE-GOAL-XPOS* 0
  "Hpos in pixels used as the goal by real-line up and down commands.")
(DEFVAR *MARK-STAYS* NIL
  "Tells command loop whether to preserve region.")
(DEFVAR *CENTERING-FRACTION* :UNBOUND
  "Tells redisplay where to recenter, if needed.")
(DEFVAR *COM-DOC-LAST-CHAR* #/B
  "Last char typed to COM-DOCUMENTATION.")
(DEFVAR *FONT-NAME* NIL
  "Current self-insertion font, as a string, for the mode line.")
(DEFVAR *LOCAL-VARIABLES* NIL
  "List of variables given local values in this editor.
These variables are also on *LOCAL-BOUND-VARIABLES*
unless they are bound in the editor closure initially.")
(DEFVAR *LOCAL-BOUND-VARIABLES* NIL
  "List of variables added by user to this editor closure.")
(DEFVAR *EDITOR* NIL
  "Within an editor closure, the value of this is the editor closure itself.")

(DEFUN MAKE-EDITOR-CLOSURE (BINDINGS WINDOW)
  "Create a closure over BINDINGS of the function FUNCALL.
Such closures are used to bind editor variables and then
execute the editor within.
BINDINGS is a list good for PROGW (or what you could put in a LET)."
  (LET ((*WINDOW* WINDOW)
        (*EDITOR* NIL))
    (PROGW (SUBSET #'CONSP BINDINGS)
      (SETQ *EDITOR*
            (CLOSURE (MAPCAR (LAMBDA (ELT) (IF (SYMBOLP ELT) ELT (CAR ELT)))
                             BINDINGS)
                     #'FUNCALL)))))

(DEFCONST EDITOR-CLOSURE-VARIABLES
          '((*INTERVAL* (AND *WINDOW* (WINDOW-INTERVAL *WINDOW*)))
            (*WINDOW* *WINDOW*)
            (*WINDOW-LIST* (AND *WINDOW* (LIST *WINDOW*)))
            (*POINT* (AND *WINDOW* (WINDOW-POINT *WINDOW*)))
            (*MARK* (AND *WINDOW* (WINDOW-MARK *WINDOW*)))
            (*FONT* 0)
            (*NUMERIC-ARG* NIL)
            (*NUMERIC-ARG-P* NIL)
            (*NUMERIC-ARG-N-DIGITS* NIL)
            (*LAST-COMMAND-CHAR* NIL)
            (*CURRENT-COMMAND* NIL)
            (*CURRENT-COMMAND-TYPE* NIL)
            (*LAST-COMMAND-TYPE* NIL)
            (*REAL-LINE-GOAL-XPOS* 0)
            (*MARK-STAYS* NIL)
            (*CENTERING-FRACTION* *CENTER-FRACTION*)
            (*COM-DOC-LAST-CHAR* #/B)
;           (*LAST-FILE-NAME-TYPED* "")
            (*FONT-NAME* NIL)
            (*MINI-BUFFER-COMMAND* NIL)
            (*MINI-BUFFER-END-POINTER* NIL)
            (*LOCAL-VARIABLES* NIL)
            (*LOCAL-BOUND-VARIABLES* NIL)
            (*EDITOR* NIL)
            ))

(DEFUN MERGE-CLOSURE-VARIABLE-LISTS (LIST1 LIST2)
  (APPEND LIST1
          (SUBSET #'(LAMBDA (ELT) (NOT (ASSQ (CAR ELT) LIST1)))
                  LIST2)))

(DEFVAR *MODE-LINE-LIST* :UNBOUND
  "List used by *MODE-LINE-WINDOW* to display the mode line.")
(DEFVAR *MODE-LIST* NIL
  "List of modes in effect.")
(DEFVAR *MAJOR-MODE* :UNBOUND
  "Current major mode name keyword.")
(DEFVAR *MODE-NAME-LIST* NIL
  "This is for the mode line")
(DEFVAR *MORE-ABOVE-BELOW* NIL
  "Says whether there is text above or below the screen.
Either NIL, or a string containing ,  or  plus a preceding space.
For the mode line.")
(DEFVAR *MODE-COMTAB* :UNBOUND
  "A sparse comtab for mode redefinitions.")
(DEFVAR *MODE-WORD-SYNTAX-TABLE* :UNBOUND
  "A sparse syntax table for mode redefinitions.")
(DEFVAR *MODE-LIST-SYNTAX-TABLE* :UNBOUND
  "A sparse syntax table for mode redefinitions.")
(DEFVAR *COMMAND-HOOK* NIL
  "List of functions to be applied to command char before the command itself.")
(DEFVAR *POST-COMMAND-HOOK* NIL
  "List of functions to be applied after the command itself.")
(DEFVAR *TYPEOUT-WINDOW* :UNBOUND
  "The menu-like typeout window of the selected editor window.")
(DEFVAR *MODE-LINE-WINDOW* :UNBOUND
  "The window where the mode line is displayed.  The minibuffer is its inferior.")
(DEFVAR *TYPEIN-WINDOW* :UNBOUND
  "Window for prompts, an inferior of *MODE-LINE-WINDOW*.")
(DEFVAR *MINI-BUFFER-WINDOW* :UNBOUND
  "The mini buffer window.")
(DEFVAR *INITIALIZED-FOR-USER* NIL
  "T if this top-level editor closure has initialized its modes since the user logged in.")

(DEFVAR *STANDARD-COMMAND* 'COM-SELF-INSERT
  "Command function to use for /"ordinary/" printing characters (COM-STANDARD).")
(DEFVAR *LAST-EXPANDED* NIL
  "Last word-abbrev expanded.")
(DEFVAR *LAST-EXPANSION* NIL
  "Expansion of last word-abbrev expanded.")
(DEFVAR *LAST-EXPANSION-BP* NIL
  "BP to last word-abbrev expansion.")
(DEFVAR *LAST-EXPANSION-SYMBOL* :UNBOUND)
(DEFVAR *LAST-EXPANSION-USAGE-PROP* :UNBOUND)
(DEFVAR *WORD-ABBREV-PREFIX-MARK* NIL)

;(DEFVAR *IO-BUFFER* NIL
;  "Bound by top level editor closures to a window-system io buffer to do input on.")

(DEFCONST MEDIUM-LEVEL-EDITOR-CLOSURE-VARIABLES
          `((*MODE-LIST* NIL)
            (*MAJOR-MODE* (GET-FILE-MAJOR-MODE *DEFAULT-MAJOR-MODE*))
            (*MODE-NAME-LIST* NIL)
            (*MORE-ABOVE-BELOW* NIL)
            (*MODE-WORD-SYNTAX-TABLE* (MAKE-SPARSE-SYNTAX-TABLE *WORD-SYNTAX-TABLE*))
            (*MODE-LIST-SYNTAX-TABLE* (MAKE-LIST-SYNTAX-TABLE))
            (*COMMAND-HOOK* NIL)
            (*POST-COMMAND-HOOK* NIL)

            ;;These are for modes SETQing to work right
            (*SPACE-INDENT-FLAG* *SPACE-INDENT-FLAG*)
            (*PARAGRAPH-DELIMITER-LIST* *PARAGRAPH-DELIMITER-LIST*)
            (*COMMENT-START* *COMMENT-START*)
            (*COMMENT-BEGIN* *COMMENT-BEGIN*)
            (*COMMENT-END* *COMMENT-END*)
            (*STANDARD-COMMAND* 'COM-SELF-INSERT)
            (*COMMENT-COLUMN* *COMMENT-COLUMN*)
            (*LAST-EXPANDED* NIL)
            (*LAST-EXPANSION* NIL)
            (*LAST-EXPANSION-BP* NIL)
            (*LAST-EXPANSION-SYMBOL* NIL)
            (*LAST-EXPANSION-USAGE-PROP* NIL)
            (*WORD-ABBREV-PREFIX-MARK* NIL)
            (*FILL-COLUMN* *FILL-COLUMN*)
            . ,EDITOR-CLOSURE-VARIABLES))

;;; Here's something you can call from outside
(DEFCONST TOP-LEVEL-EDITOR-CLOSURE-VARIABLES
          `((*MODE-LINE-LIST* *MODE-LINE-LIST*)
            (*COMTAB* *COMTAB*)
            (*MODE-COMTAB* NIL)
            (*MODE-LINE-WINDOW* NIL)
            (*TYPEIN-WINDOW* NIL)
            (*MINI-BUFFER-WINDOW* NIL)
;           (*MINI-BUFFER-RING* NIL)
            (*INITIALIZED-FOR-USER* NIL)
;           (*IO-BUFFER* NIL)
            (*TYPEOUT-WINDOW* NIL)
            ;; This value lasts only during initialization.
            (*TERMINAL-IO* *TERMINAL-IO*)
            (*STANDARD-OUTPUT* *STANDARD-OUTPUT*)
            (*STANDARD-INPUT* *STANDARD-INPUT*)
            (*QUERY-IO* *QUERY-IO*)
            . ,MEDIUM-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFVAR *EDITORS-WHOSE-MODES-TO-RESET* NIL)

(DEFUN MAKE-LIST-SYNTAX-TABLE ()
  (LET ((TABLE
          (MAKE-ARRAY #o400 ':TYPE ART-4B)))
    (COPY-ARRAY-CONTENTS *LIST-SYNTAX-TABLE* TABLE)
    TABLE))

;;; Call this inside a top-level editor to initialize it.
(DEFUN INITIALIZE-TOP-LEVEL-EDITOR (WINDOW &OPTIONAL NO-RESET-MODES)
  (UNLESS *WINDOW*
    (OR NO-RESET-MODES
        (MEMQ *EDITOR* *EDITORS-WHOSE-MODES-TO-RESET*)
        (PUSH *EDITOR* *EDITORS-WHOSE-MODES-TO-RESET*))
    (SETQ *MODE-COMTAB* (CREATE-SPARSE-COMTAB 'MODE-COMTAB))
    (SET-COMTAB-INDIRECTION *MODE-COMTAB* *COMTAB*)
    (SETQ *COMTAB* *MODE-COMTAB*)
    (SETQ *WINDOW-LIST* NIL)
    (MAKE-WINDOW-CURRENT WINDOW NIL)
    (AND *MAJOR-MODE* (TURN-ON-MODE *MAJOR-MODE*))))

(DEFUN RESET-ALL-EDITOR-MODES ()
  (DOLIST (EDITOR *EDITORS-WHOSE-MODES-TO-RESET*)
    (FUNCALL EDITOR #'(LAMBDA () (SETQ *INITIALIZED-FOR-USER* NIL)))))

(ADD-INITIALIZATION "Reset-All-Editor-Modes"
                    '(RESET-ALL-EDITOR-MODES)
                    :LOGIN)

;;; allow-setting-instance-variables-inside-mode flushed.
(DEFUN ALLOW-SETTING-VARIABLES-INSIDE-MODE (BINDINGS)
  (DOLIST (ELT BINDINGS)
    (LET ((VAR (IF (SYMBOLP ELT) ELT (CAR ELT))))
      (PUTPROP VAR T 'MODE-SETTABLE-P))))

(ALLOW-SETTING-VARIABLES-INSIDE-MODE TOP-LEVEL-EDITOR-CLOSURE-VARIABLES)

(DEFVAR *BUFFER-MODIFIED-P* NIL
  "Inside a ZMACS editor, a string containing * or (RO) or something like that, or NIL.")
(DEFVAR *ZMACS-BUFFER-NAME* :UNBOUND
  "The name of the selected buffer in the selected ZMACS window.
This is for the ZMACS mode line")
(DEFVAR *ZMACS-BUFFER-VERSION-STRING* :UNBOUND
  "A string containing the version of the visited file
in the selected ZMACS buffer in the selected ZMACS window.
This is for the ZMACS mode line")
(DEFVAR *MACRO-LEVEL* NIL
  "Current macro level, for the mode line.")

(DEFCONST ZMACS-TOP-LEVEL-EDITOR-CLOSURE-VARIABLES
          `((*PACKAGE* SI:PKG-USER-PACKAGE)
            (*PRINT-BASE* *PRINT-BASE*)
            (*READ-BASE* *READ-BASE*)
            (*READTABLE* *READTABLE*)
            (*BUFFER-MODIFIED-P* NIL)
            (*MACRO-LEVEL* NIL)
            (*ZMACS-BUFFER-NAME* NIL)
            (*ZMACS-BUFFER-VERSION-STRING* NIL)
            . ,TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFSTRUCT (LINE :ARRAY-LEADER (:ALTERANT NIL) (:SIZE-SYMBOL LINE-LEADER-SIZE))
  (LINE-LENGTH 0 :DOCUMENTATION "Number of characters in line")
  (LINE-NEXT NIL :DOCUMENTATION "Following line in buffer, or NIL if none.")
  (LINE-PREVIOUS NIL :DOCUMENTATION "Preceeding line in buffer, or NIL if none.")
  (LINE-BP-LIST NIL :DOCUMENTATION "List of permanent BPs pointing into this line.")
  (LINE-TICK NIL :DOCUMENTATION "Tick at which this line was last modified.")
  (LINE-NODE NIL :DOCUMENTATION
     "The bottommost node in the node heirarchy containing this line.")
  (LINE-CONTENTS-PLIST NIL :DOCUMENTATION "Plist which is cleared out by MUNG-LINE.
Holds properties of the text in the line. Contrast this with LINE-PLIST")
  (LINE-PLIST NIL :DOCUMENTATION
     "Plist of properties of this line itself, rather than the text in it.
This is not cleared out by MUNG-LINE.")
  )

(DEFSTRUCT (TEMP-BP :LIST (:ALTERANT NIL))
  (BP-LINE NIL :DOCUMENTATION "Line to which this bp points.")
  (BP-INDEX NIL :DOCUMENTATION "Character position in line at which this bp points.")
  )

(DEFSTRUCT (BP :LIST (:INCLUDE TEMP-BP) (:ALTERANT NIL))
  (BP-STATUS NIL :DOCUMENTATION ":NORMAL or :MOVES")
  )

(DEFSTRUCT (COMTAB :NAMED (:ALTERANT NIL))
  "A COMTAB is a command table.  It conceptually is indexed by possible user /"keystrokes/"
/(a keystroke may also be a mouse push or anything else like that), and contains
a COMMAND for each one.  A COMMAND may be any of:
  NIL    -- The keystroke is unassigned.  If there is a COMTAB-INDIRECT-TO,
            look in that COMTAB.
  :UNDEFINED -- The keystroke is REALLY unassigned, regardless of indirecting.
  A list -- The command is a synonym, pointing at some other slot in the COMTAB.
            the list should have two elements, the control-meta and the char parts
            of a keystroke on the keyboard.
  Some other symbol -- A command as defined by DEFCOM."
  (COMTAB-NAME NIL :DOCUMENTATION "Name of this comtab. A symbol.")
  (COMTAB-KEYBOARD-ARRAY NIL :DOCUMENTATION "Commands gotten by typing on the keyboard.")
  (COMTAB-MOUSE-ARRAY NIL :DOCUMENTATION "Commands gotten by pushing mouse buttons.")
  (COMTAB-EXTENDED-COMMANDS NIL :DOCUMENTATION "Alist of long-named (/"m-X/") commands.")
  (COMTAB-INDIRECT-TO NIL :DOCUMENTATION "A COMTAB to try when you find NIL in this COMTAB.")
  (COMTAB-DOCUMENTATION NIL :DOCUMENTATION
    "A string describing this comtabs meaning in life.")
  )

(DEFSELECT ((:PROPERTY COMTAB NAMED-STRUCTURE-INVOKE))
  (:PRINT-SELF (SELF STREAM IGNORE &OPTIONAL IGNORE)
    (IF *PRINT-ESCAPE*
        (SYS:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE)
          (PRIN1 (COMTAB-NAME SELF) STREAM))
      (PRINC (COMTAB-NAME SELF) STREAM))))

(DEFSTRUCT (UNDO-STATUS :LIST (:CONC-NAME UNDO-STATUS-) (:ALTERANT NIL)
                        (:CONSTRUCTOR MAKE-UNDO-STATUS (NODE)))
  "The undo-status of a node is a list of this form:
/(node undo-list range-start-bp range-end-bp)
The start-bp and end-bp delimit the current batch of small changes,
not yet recorded in the undo-list.  The interval and saved-bp-values
are data about that batch of small changes.
The redo-list is like the undo-list but has to do with
changes made by Undo itself.  Redo undoes from that list."
  NODE
  UNDO-LIST
  START-BP
  END-BP
  INTERVAL
  SAVED-BP-VALUES
  (INTERVAL-NUMBER-OF-LINES NIL :DOCUMENTATION
    "Number of lines of old data we have recorded.")
  (END-ORIGINAL-INDEX NIL :DOCUMENTATION
    "UNDO-STATUS-END-BP's original position-within-line.")
  REDO-LIST
  MODIFIED-FLAG)

(DEFSTRUCT (UNDO-ITEM :LIST (:CONSTRUCTOR MAKE-UNDO-ITEM
                                          (TYPE START-BP END-BP INTERVAL SAVED-BP-VALUES
                                                MODIFIED-FLAG))
                      (:ALTERANT NIL)
                      (:CONC-NAME UNDO-ITEM-))
  "An individual atomic undo item."
  (TYPE NIL :DOCUMENTATION "A string describing the reason for this set of changes")
  (START-BP NIL :DOCUMENTATION "BP to start of where interval came from (:moves)")
  (END-BP NIL :DOCUMENTATION "BP to end of where interval came from (:normal)")
  (INTERVAL NIL :DOCUMENTATION "The innterval this UNDO-ITEM records information about.
Used to be delimited by UNDO-ITEM-START-BP and UNDO-ITEM-END-BP.")
  (SAVED-BP-VALUES NIL :DOCUMENTATION
    "Alist recording all permanent bps that were in that interval
elements are (permanent-bp . copy), where the copy is a nonrelocating bp.")
  (MODIFIED-FLAG NIL :DOCUMENTATION "Whether the interval was modified before this change."))

;;;; Editor window structures.
(DEFFLAVOR DISPLAYER
           (N-PLINES                    ;Number of screen lines in length.
            (INTERVAL 'NODE)            ;Interval being displayed.
            (REDISPLAY-DEGREE DIS-ALL)  ;DIS-xxx.  Says how much redisplay needed (max)
            (REDISPLAY-LINE NIL)        ;If ^ is DIS-LINE, this is the line needing display.
            (REDISPLAY-INDEX 0)         ;If it is DIS-LINE, this is index in line to start at
            START-BP                    ;BP to put at top of window.
            (LAST-POINT-PLINE 0)        ;Hint for redisplay computation.
            ;; Some vectors with one element per pline.
            PLINE-LINE-ARRAY            ;LINE on each pline.
            PLINE-FROM-INDEX-ARRAY      ;Index of start of part of LINE that's on this pline.
            PLINE-TO-INDEX-ARRAY        ;Index of end of that part of the line.
            PLINE-TICK-ARRAY            ;Tick this pline was last updated on.

            EDITOR-CLOSURE              ;it binds many variables and then does FUNCALL.
            TV:IO-BUFFER
            BASE-TICK                   ;Used to tell whether "interval is modified"
                                        ;in non-ZMACS windows.
            (POINT NIL)
            (MARK NIL)
            (MARK-P NIL)                ;T if there is a region now.
            (POINT-PDL NIL)
            (LAST-BP-DISPLAYED-P NIL))  ;T if end of interval is "on screen".
                                        ;Up to date only after redisplay is done.
           ()
  (:REQUIRED-METHODS :REDISPLAY :PUT-POINT-AT-PLINE
                     :STRING-LENGTH :COMPUTE-MOTION
                     :NEW-SCROLL-POSITION :IO-BUFFER :MODE-LINE-WINDOW
                     :TAB-NCHARS-LOCATION)
  (:INIT-KEYWORDS :COMTAB :EDITOR-CLOSURE-VARIABLES :MODE-LINE-LIST)
  :GETTABLE-INSTANCE-VARIABLES
  :INITTABLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES BASE-TICK)
  (:ACCESSOR-PREFIX WINDOW-)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFSUBST WINDOW-OVERPRINTING-FLAG (WINDOW)
  (SEND WINDOW ':OVERPRINTING-FLAG))

;;; Return the typeout stream associated with the given window
(DEFSUBST WINDOW-TYPEOUT-WINDOW (WINDOW)
  (SEND WINDOW ':TYPEOUT-WINDOW))

(DEFSUBST WINDOW-IO-BUFFER (WINDOW)
  (SEND WINDOW ':IO-BUFFER))

;;; A displayer that is also a window-system window.
(DEFFLAVOR WINDOW
           ((SPECIAL-BLINKER-LIST NIL)
            (FONT-ALIST NIL)
            (CURRENT-FONT 0)
            POINT-BLINKER

            ;; Next come some vectors with one element per pline.
            PLINE-MARKING-LEFT-ARRAY    ;Hpos of left edge of region-marking on this pline.
            PLINE-MARKING-WIDTH-ARRAY   ;width in pixels of region-marking.
            PLINE-TEXT-WIDTH-ARRAY)     ;width in pixels of text.
           (DISPLAYER)
  (:REQUIRED-FLAVORS TV:WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES
    SPECIAL-BLINKER-LIST FONT-ALIST POINT-BLINKER  ;:CURRENT-FONT defined in METH
    PLINE-MARKING-LEFT-ARRAY PLINE-MARKING-WIDTH-ARRAY PLINE-TEXT-WIDTH-ARRAY)
  :INITTABLE-INSTANCE-VARIABLES
  (:ACCESSOR-PREFIX WINDOW-)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFSUBST WINDOW-SHEET (WINDOW) WINDOW)

;;; These accessor macros normally go through the ones that use %INSTANCE-REF
;;; but if WINDOW is SELF they refer to the instance variable directly.
(DEFMACRO WINDOW-OR-SELF-PLINE-LINE-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-LINE-ARRAY
    `(WINDOW-PLINE-LINE-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-FROM-INDEX-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-FROM-INDEX-ARRAY
    `(WINDOW-PLINE-FROM-INDEX-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-TO-INDEX-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-TO-INDEX-ARRAY
    `(WINDOW-PLINE-TO-INDEX-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-TICK-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-TICK-ARRAY
    `(WINDOW-PLINE-TICK-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-MARKING-LEFT-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-MARKING-LEFT-ARRAY
    `(WINDOW-PLINE-MARKING-LEFT-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-MARKING-WIDTH-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-MARKING-WIDTH-ARRAY
    `(WINDOW-PLINE-MARKING-WIDTH-ARRAY ,WINDOW)))

(DEFMACRO WINDOW-OR-SELF-PLINE-TEXT-WIDTH-ARRAY (WINDOW)
  (IF (EQ WINDOW 'SELF)
      'PLINE-TEXT-WIDTH-ARRAY
    `(WINDOW-PLINE-TEXT-WIDTH-ARRAY ,WINDOW)))

;;; There will be problems in the indentation functions
;;; for non-sheet windows, since they all use sheet-compute-motion, etc.
;;; but sending a message to the window may be too slow?  I wonder.

(DEFSUBST PLINE-LINE (WINDOW &OPTIONAL PLINE)
  "The LINE last displayed on pline number PLINE in WINDOW."
  (AREF (WINDOW-OR-SELF-PLINE-LINE-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-FROM-INDEX (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-FROM-INDEX-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-TO-INDEX (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-TO-INDEX-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-TICK (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-TICK-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-MARKING-LEFT (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-MARKING-LEFT-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-MARKING-WIDTH (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-MARKING-WIDTH-ARRAY WINDOW) PLINE))

(DEFSUBST PLINE-TEXT-WIDTH (WINDOW PLINE)
  (AREF (WINDOW-OR-SELF-PLINE-TEXT-WIDTH-ARRAY WINDOW) PLINE))

(DEFFLAVOR TOP-LEVEL-DISPLAYER-MIXIN
           ()
           ()
  (:REQUIRED-FLAVORS DISPLAYER)
  (:REQUIRED-METHODS :TYPEOUT-WINDOW))

;;;; Define basic kinds of intervals.

(DEFFLAVOR INTERVAL (FIRST-BP LAST-BP) ()
  :SETTABLE-INSTANCE-VARIABLES
  :ORDERED-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFUN MAKE-INTERVAL (&OPTIONAL FIRST-BP LAST-BP)
  (LET ((DEFAULT-CONS-AREA *ZWEI-AREA*))
    (SI:%MAKE-INSTANCE 'INTERVAL FIRST-BP LAST-BP)))

(DEFMETHOD (INTERVAL :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP FIRST-BP)
    (LET ((LINE (CREATE-LINE 'ART-STRING 0 NIL)))
      (SETF FIRST-BP (CREATE-BP LINE 0 ':NORMAL))
      (SETF LAST-BP (CREATE-BP LINE 0 ':MOVES))
      (SETF (LINE-NODE LINE) SELF))))

(DEFFLAVOR NODE
           ((TICK *TICK*)               ;*TICK* this buffer was last munged at.
            (NEXT NIL)                  ;Following node at same level.
            (PREVIOUS NIL)              ;previous node at same level.
            (SUPERIOR NIL)              ;Node which contains this whole level.
            (INFERIORS NIL)             ;List of nodes which are inside of this one.
            (UNDO-STATUS NIL)           ;UNDO-STATUS list, see defstruct above.
            (READ-ONLY-P NIL)
            (SI:PROPERTY-LIST NIL))     ;For SI:PROPERTY-LIST-MIXIN.
                                        ;Mentioned so it gets made ordered/accessible
           ;; Properties include:
           ;;  :KILLED -- T means this is a killed buffer.
           ;;  :SPECIAL-TYPE -- non-NIL means it's a special buffer, and identifies the type.
           ;;   Some examples are :DIRED, :MAIL, :EDIT-BUFFERS, etc.
           ;; :DONT-SECTIONIZE -- T means created with Find File No Sectionize.
           (INTERVAL SI:PROPERTY-LIST-MIXIN)
  :SETTABLE-INSTANCE-VARIABLES
  (:ORDERED-INSTANCE-VARIABLES
    FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
    UNDO-STATUS READ-ONLY-P SI:PROPERTY-LIST)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFUN MAKE-NODE (&OPTIONAL FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
                  UNDO-STATUS READ-ONLY-P PLIST)
  (LET ((DEFAULT-CONS-AREA *ZWEI-AREA*))
    (SI:%MAKE-INSTANCE 'NODE FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
                       UNDO-STATUS READ-ONLY-P PLIST)))

(DEFSUBST NODE-SPECIAL-TYPE (NODE)
  (GET (LOCF (NODE-PROPERTY-LIST NODE)) ':SPECIAL-TYPE))

(DEFSUBST NODE-UNDO-STATUS-OR-NIL (NODE)
  (IF (NEQ (NODE-UNDO-STATUS NODE) ':DONT) (NODE-UNDO-STATUS NODE)))

(DEFFLAVOR SECTION-NODE
           ((NAME NIL)                          ;A function spec, or a string.
            (DEFUN-LINE NIL)                    ;A LINE on which the definition in this
                                                ;section appears (appeared) to live.
            (COMPILE-TICK *TICK*)               ;Time last compiled this section.
            (SECTIONIZE-TICK *TICK*))           ;Time last checked sectionization here.
           (NODE)
  :SETTABLE-INSTANCE-VARIABLES
  (:ORDERED-INSTANCE-VARIABLES
    FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
    UNDO-STATUS READ-ONLY-P SI:PROPERTY-LIST
    NAME DEFUN-LINE COMPILE-TICK SECTIONIZE-TICK)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFMETHOD (SECTION-NODE :PRINT-SELF) (STREAM IGNORE &OPTIONAL IGNORE)
  (IF *PRINT-ESCAPE*
      (SYS:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE)
        (PRINC NAME STREAM))
    (PRINC NAME STREAM)))

(DEFUN MAKE-SECTION-NODE (&OPTIONAL FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
                          UNDO-STATUS READ-ONLY-P PLIST
                          NAME DEFUN-LINE COMPILE-TICK SECTIONIZE-TICK)
  (LET ((DEFAULT-CONS-AREA *ZWEI-AREA*))
    (SI:%MAKE-INSTANCE 'SECTION-NODE FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
                       UNDO-STATUS READ-ONLY-P PLIST
                       NAME DEFUN-LINE COMPILE-TICK SECTIONIZE-TICK)))

(DEFFLAVOR NAMED-BUFFER
           (NAME)                               ;A string.
           (NODE)
  :SETTABLE-INSTANCE-VARIABLES
  (:ACCESSOR-PREFIX BUFFER-)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFMETHOD (NAMED-BUFFER :BEFORE :INIT) (IGNORE)
  (IF (VARIABLE-BOUNDP NAME)
      (SETQ NAME (STRING NAME))))

(DEFMETHOD (NAMED-BUFFER :PRINT-SELF) (STREAM IGNORE &OPTIONAL IGNORE)
  (IF *PRINT-ESCAPE*
      (SYS:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPE)
        (PRINC NAME STREAM))
    (PRINC NAME STREAM)))

(DEFFLAVOR FILE-BUFFER
           ((PATHNAME NIL)                      ;Pathname being visited.
            (GENERIC-PATHNAME NIL)              ;Holds file attributes.
            (FILE-ID NIL)                       ;file version/date when last read or written
            (FILE-TICK *TICK*)                  ;tick at which file was last read or written
            (FILE-READ-TICK *TICK*)             ;tick at which file was last read
            (VERSION-STRING NIL))
           (NAMED-BUFFER)
  :SETTABLE-INSTANCE-VARIABLES
  (:ACCESSOR-PREFIX BUFFER-)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFMETHOD (FILE-BUFFER :WRITE-FILE-INTERNAL) (PATH)
  "Save the buffer in file PATH and mark it as visiting that file.
This method is always called when a buffer is written to a file, so it is
a prime candidate for :before and :after daemons."
  "Save BUFFER in file PATHNAME and mark it as visiting that file."
  ;; Examine the buffer's current mode line.
  ;; If the user has edited in a Fonts: property,
  ;; save the font information, even if he has failed to do
  ;; Reparse Mode Line.
  (LET ((PLIST (FS:FILE-EXTRACT-PROPERTY-LIST (INTERVAL-STREAM SELF))))
    (WHEN (CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES PLIST SELF)
      (MUST-REDISPLAY-BUFFER SELF DIS-TEXT))
    (WITH-OPEN-FILE-RETRY (STREAM (PATH FS:FILE-ERROR) :DIRECTION :OUTPUT)
      (STREAM-OUT-INTERVAL STREAM SELF NIL T
                           (OR (GETL (LOCF PLIST) '(:FONTS :DIAGRAM))
                               (SEND SELF :GET-ATTRIBUTE :FONTS)
                               (SEND SELF :GET-ATTRIBUTE :DIAGRAM)))
      (CLOSE STREAM)
      (SET-BUFFER-PATHNAME PATH SELF)
      (SET-BUFFER-FILE-ID SELF (SEND STREAM ':INFO))
      (SETF (BUFFER-TICK SELF) (TICK))
      (send self :remprop :cts-done)            ; $$$ cts control <17-Nov-88 smh>
      (WHEN *DISCARD-UNDO-INFO-ON-SAVING*
        (DISCARD-UNDO-INFORMATION SELF))
      (PRINT-FILE-WRITTEN STREAM))))

(DEFSUBST BUFFER-TICK (BUFFER) (BUFFER-FILE-TICK BUFFER))

;;; This probably wants to be changed.
(DEFFLAVOR ZMACS-BUFFER
           (SAVED-POINT                         ;POINT the last time this was on a window.
            SAVED-MARK                          ;Same for MARK.
            SAVED-MODE-LIST                     ;Saved *MODE-LIST* for this buffer.
            SAVED-MAJOR-MODE                    ;Saved *MAJOR-MODE* for this buffer.
            (SAVED-FONT-ALIST NIL)              ;Saved font mapping for this buffer
            SAVED-WINDOW-START-BP               ;BP to top of window
;; This variable is not patched in system 98!
            (SAVED-LOCAL-VARIABLES NIL)         ;Elts are (VARIABLE . LOCAL-VALUE)
            (SAVED-PACKAGE NIL))                ;The package to go to for editing this buffer
           (FILE-BUFFER)
  :SETTABLE-INSTANCE-VARIABLES
  (:METHOD-COMBINATION
    (:DAEMON-WITH-AND :BASE-FLAVOR-LAST :MUNGED-P :NEEDS-SAVING-P))
  (:ACCESSOR-PREFIX BUFFER-)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

(DEFSUBST BUFFER-PACKAGE (BUFFER) (BUFFER-SAVED-PACKAGE BUFFER))

;; This is used only in system 98.
;(DEFSUBST BUFFER-SAVED-LOCAL-VARIABLES (BUFFER)
;  (GET BUFFER 'SAVED-LOCAL-VARIABLES))

(DEFMETHOD (ZMACS-BUFFER :BEFORE :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP NAME)
    (setq name (generate-buffer-name))))

;; As of system 123, there is only one place where anonymous buffer names
;; are computed, and they'll all look alike.

(DEFUN GENERATE-BUFFER-NAME ()
  "Generate a new Buffer-n name."
    (LOOP AS POSSIBLE-NAME =
             (FORMAT:OUTPUT NIL
               "*Buffer-"
               (LET ((*PRINT-BASE* 10.) (*NOPOINT T) (*PRINT-RADIX* NIL))
                 (PRIN1 (INCF *ZMACS-BUFFER-COUNTER*)))
               "*")
          WHEN (NULL (FIND-BUFFER-NAMED POSSIBLE-NAME))
          RETURN POSSIBLE-NAME))

(DEFMETHOD (ZMACS-BUFFER :AFTER :INIT) (IGNORE)
  (DECLARE (SPECIAL *DEFAULT-MAJOR-MODE*))      ;DEFVAR is in MACROS.
  (UNLESS SAVED-PACKAGE
    (SETQ SAVED-PACKAGE (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*))))
  (SEND SELF ':SET-ATTRIBUTE ':MODE *DEFAULT-MAJOR-MODE*)
  (LET ((LINE (BP-LINE FIRST-BP)))
    (SETQ SAVED-MAJOR-MODE (GET-FILE-MAJOR-MODE *DEFAULT-MAJOR-MODE*))
    (SETF SAVED-POINT (CREATE-BP LINE 0 ':NORMAL))
    (SETF SAVED-MARK (CREATE-BP LINE 0 ':NORMAL))
    (SETF SAVED-WINDOW-START-BP (CREATE-BP LINE 0 ':NORMAL))
    (SETF SAVED-MODE-LIST (STICKY-MODE-LIST))))

(DEFFLAVOR INTERVAL-STREAM
           (**INTERVAL** *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* (*EOF* NIL)
            NO-UNDO-SAVING)
           (SI:BIDIRECTIONAL-STREAM)
  :INITABLE-INSTANCE-VARIABLES)

(DEFFLAVOR INTERVAL-STREAM-FAT
           ()
           (INTERVAL-STREAM)
  :INITABLE-INSTANCE-VARIABLES)

(DEFFLAVOR INTERVAL-STREAM-WITH-FONTS
           ((*FONT-FLAG* NIL)
            (**FONT** 0)
            ;;;Changed *FONT-STACK* from ART-Q-LIST to normal array. -Keith 9/24/88
            (*FONT-STACK* (MAKE-ARRAY 50. ':TYPE 'ART-16B ':FILL-POINTER 0)))
           (INTERVAL-STREAM)
  :INITABLE-INSTANCE-VARIABLES)

(DEFUN RESET-EDITOR-VARIABLES ()
  (DECLARE (SPECIAL *READ-BUFFER-KLUDGE*))      ;DEFVAR in ZMACS.
  ;; Make sure that these variables are not left screwed up
  ;; if a warm boot is done while inside an editor.
  ;; There may be a few other variables needed by other
  ;; kinds of editors, but this gets all the vital ones.
  (DOLIST (X ZMACS-TOP-LEVEL-EDITOR-CLOSURE-VARIABLES)
    (SI:UNCLOSUREBIND (CAR X)))
  (SETQ *BATCH-UNDO-SAVE* NIL)
  (SETQ *LISP-PARSE-PREPARSED-FLAG* NIL)
  (SETQ *IN-COM-DOC-P* NIL)
  (SETQ *REPEAT-DOC-P* NIL)
  (SETQ *READ-ONLY-SUPPRESSED-INTERVAL* NIL)
  (SETQ *INSIDE-BREAK* NIL)
  (SETQ *EDITOR-IDLE* NIL)
  (SETQ *TAB-WIDTH* NIL)
  (SETQ *READ-BUFFER-KLUDGE* NIL))

(ADD-INITIALIZATION 'RESET-EDITOR-VARIABLES
                    '(RESET-EDITOR-VARIABLES)
                    '(:SYSTEM))

;;; Self-Documentation. -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Worry about whether DOCUMENTATION-FUNCTION stuff will get called
;;; with the right streams set up.

;;; A command (a COM- symbol) either has "smart" or "normal" handling of
;;; documentation.  All commands should have a COMMAND-NAME property, whose
;;; value is the nice-looking string form of the command's name.
;;; It should also have a DOCUMENTATION property, whose value is the
;;; string which is the full documentation.  If short documentation
;;; (the one-line kind produced by List Commands) is needed, it is just
;;; the first line of the full documentation.

;;; A command with "smart" handling is detected by the presence of
;;; a DOCUMENTATION-FUNCTION property.  The value of this property
;;; should be a function, which is called with three arguments:
;;;    First, the command symbol itself.
;;;    Secondly, the character typed to get this command, or NIL.  If the second
;;;       argument is NIL, that means that the caller does not have any particular
;;;       character in mind (e.g. in List Commands).  The documentation-function
;;;       should be prepared to deal with this case.
;;;    Thirdly, an operation which tells the function what to do.  They are:
;;;       :NAME  Return your name as a string, (e.g. "Self Insert")
;;;       :FULL  Type out full documentation to *standard-output*.
;;;       :SHORT Type out short documentation to *standard-output*.

;;; The symbols on the *COMMAND-HOOK* may also want to document
;;; themselves when the user asks for self-documentation.  Any hook
;;; which does should have an HOOK-DOCUMENTATION-FUNCTION property,
;;; whose value is a function of two arguments, the command which the
;;; hook is preceeding, and the character typed.  (The second argument
;;; will never be NIL.) This function will only be called on the user's
;;; request for FULL (not SHORT) documentation. The function should print
;;; out stuff to *standard-output*.  It may assume the cursor is at the left
;;; edge, and it should leave the cursor there.

(DEFMACRO DEFINE-COMMAND-DOCUMENTATION (COMMAND &BODY BODY)
  "Define a documentation function for a ZWEI command.
There are three arguments provided for you:
    COM, the command symbol itself.
    CHAR, the character typed to get this command, or NIL.  If the second
       argument is NIL, that means that the caller does not have any particular
       character in mind (e.g. in List Commands).  The documentation-function
       should be prepared to deal with this case.
    OP, an operation which tells the function what to do.  They are:
       :NAME  Return your name as a string, (e.g. /"Self Insert/")
       :FULL  Type out full documentation to *STANDARD-OUTPUT*.
       :SHORT Type out short documentation to *STANDARD-OUTPUT*."
  `(DEFUN (:PROPERTY ,COMMAND DOCUMENTATION-FUNCTION) (COM CHAR OP)
     COM CHAR OP ; in case not used in body
     ,@BODY))

(DEFCONST *COM-DOCUMENTATION-ALIST*
          '((#/C COM-SELF-DOCUMENT)
            (#/D COM-DESCRIBE-COMMAND)
            (#/L COM-WHAT-LOSSAGE
             (OPERATION-HANDLED-P *STANDARD-INPUT* :PLAYBACK))
            (#/A COM-current-mode-APROPOS)
            (#/U COM-UNDO)
            (#/V COM-VARIABLE-APROPOS)
            (#/W COM-WHERE-IS))
  "Alist defining Help options.
Each element is (CHARACTER COMMAND COND-FORM).
The option is available only if COND-FORM evals non-NIL
 (but if the list has only two elements, the option is unconditional).
COMMAND is what is run to do the work.  It should be a DEFCOM name.")

(DEFCOM COM-DOCUMENTATION "Run a specified documentation command.
You type a character.  To find out what a certain character does, type C and that character.
To find out what a named command does, type D and the command name.
To find all commands whose names contain a certain substring,
  type A and then the substring.
To find out the last 60 characters you typed, if you are confused, type L.
More advanced options:
   U - Undo; V - run Variable Apropos; W - run Where Is;
   SPACE repeats the previous documentation request, if any." ()
  (DO ((CHAR 0)
       (*IN-COM-DOC-P* T)
       (*REPEAT-DOC-P* NIL))
      (NIL)
    ;; Print a list of available options.
    (FORMAT *QUERY-IO* "~&Help.  Options are ")
    (DOLIST (ELT *COM-DOCUMENTATION-ALIST*)
      (IF (OR (NULL (THIRD ELT))
              (EVAL (THIRD ELT)))
          (FORMAT *QUERY-IO* "~C," (CAR ELT))))
    (FORMAT *QUERY-IO* "~\LOZENGED-CHARACTER\,~\LOZENGED-CHARACTER\: " #/SPACE #/HELP)
    ;; Read input chars till we get a valid one.
    (TYPEIN-LINE-ACTIVATE
      (SETQ CHAR (DO ((CHAR (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))
                            (CHAR-UPCASE (READ-CHAR *STANDARD-INPUT*))))
                     ((OR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*)
;character lossage
                          (ASSQ (CHAR-INT CHAR) *COM-DOCUMENTATION-ALIST*)
                          (MEMQ CHAR '(#/SP #/HELP)))
                      CHAR)
                   (WHEN (MEMQ CHAR '(#/C-G #/RUBOUT))
                     (SEND *QUERY-IO* :MAKE-COMPLETE)
                     (BARF))
                   (BEEP))))
    (SEND *QUERY-IO* :MAKE-COMPLETE)
    ;; Execute the character we got.
    (COND ((EQL CHAR #/SPACE)
           (SETQ *REPEAT-DOC-P* T)
           (SETQ CHAR *COM-DOC-LAST-CHAR*))
          (T (SETQ *COM-DOC-LAST-CHAR* CHAR)))
    (IF (MEMQ CHAR '(#/? #/HELP))
        (PROGN (FORMAT T "You have entered the Documentation command.~%")
               (PRINT-DOC :FULL 'COM-DOCUMENTATION *LAST-COMMAND-CHAR*))
      (LET ((FUNCTION (CADR (OR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*)
;character lossage
                                (ASSQ (CHAR-INT CHAR) *COM-DOCUMENTATION-ALIST*)))))
        (AND FUNCTION (RETURN (FUNCALL FUNCTION)))))))

(DEFINE-COMMAND-DOCUMENTATION COM-DOCUMENTATION ()
  (IF (NEQ OP :FULL)
      (IF (EQ OP :NAME)
          "Documentation"
        (PRINT-DOC OP COM CHAR T))
    (FORMAT T "This prints various sorts of editor documentation.
You type an option -- one character -- saying what kind of documentation you want.
For some options, you then type more; for example, which command to document.

Here are the options available now:~%")
    (DOLIST (ELT *COM-DOCUMENTATION-ALIST*)
      (IF (OR (NULL (THIRD ELT)) (EVAL (THIRD ELT)))
          (PROGN
            (FORMAT T "~C -- " (CAR ELT))
            (PRINT-DOC :SHORT (CADR ELT)))))
    (FORMAT T "~%In addition, ~\LOZENGED-CHARACTER\ repeats the previous Help request."
            #/SPACE)))

(DEFCOM COM-DOCUMENT-CONTAINING-COMMAND "Print documentation on the command that you
are in the middle of." ()
  (FORMAT T "~&You are typing in the mini-buffer.
Type Control-Meta-Y to go back to editing the previous mini-buffer ~A.

~@[The command in progress is ~A:~]~%"
          (IF (CDR *OUTER-LEVEL-MINI-BUFFER-COMMAND*) "argument of this command" "command")
          (COMMAND-NAME *MINI-BUFFER-COMMAND-IN-PROGRESS* T))
  (PRINT-DOC :FULL *MINI-BUFFER-COMMAND-IN-PROGRESS*)
  (SEND *STANDARD-OUTPUT* :FRESH-LINE)
  (WHEN *MINI-BUFFER-ARG-DOCUMENTER*
    (FUNCALL *MINI-BUFFER-ARG-DOCUMENTER*))
  DIS-NONE)

(DEFCOM COM-SELF-DOCUMENT "Print out documentation for the command on a given key." (KM)
  (LET (CHAR)
    (FORMAT *QUERY-IO* "~&Document command: ")
    (TYPEIN-LINE-ACTIVATE
      (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
        (SETQ CHAR (SEND *QUERY-IO* :ANY-TYI))))
    (DO-FOREVER
      (IF (EQ (CAR-SAFE CHAR) :MOUSE-BUTTON)
          (SETQ CHAR (CADR CHAR)))
      (COND ((ATOM CHAR)
             (FORMAT *QUERY-IO* "~:@C" CHAR)
             (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)
             (DOCUMENT-KEY CHAR *COMTAB*)
             (RETURN))
            ((EQ (CAR CHAR) 'SCROLL)
             (FORMAT *QUERY-IO* "Mouse-Scroll")
             (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)
             (FORMAT T
                     "Mouse Scrolling:
  When the mouse cursor is an up-and-down arrow, near the left edge,
it is in the /"scroll bar/".  Clicking the mouse in the scroll bar
scrolls the text in the window.

When the mouse is near the top or bottom edge and the cursor is a thick arrow,
that too is a place you can scroll, by pushing the mouse against the edge.

In the scroll bar, click left to scroll the line the mouse is on to the
top of the window.  Click right scrolls the same amount in the opposite
direction; the line at the top of the window moves down to the mouse.
Click middle uses the position of the mouse along the edge to choose
a portion of the buffer to view, so that if the mouse is near the bottom
you see something near the end of the file.

A portion of the left edge is thickened to show you what part of the
buffer is currently on the screen.")
             (RETURN)))))
  DIS-NONE)

(DEFUN DOCUMENT-KEY (CHAR COMTAB)
  "Print full documentation of character CHAR's definition in COMTAB, on *STANDARD-OUTPUT*."
  (IF (OR (ATOM CHAR)
          (AND (EQ (CAR-SAFE CHAR) :MOUSE-BUTTON)
               (SETQ CHAR (CADR CHAR))))
      (FORMAT T "~&~:@C" CHAR)
      (FORMAT T "~&~S" CHAR))
  (PROG (TEM PREFIX)
     L  (SETQ TEM (COMMAND-LOOKUP CHAR COMTAB T))
        (COND ((NULL TEM)
               (FORMAT T " is undefined.~%"))
              ((SYMBOLP TEM)
               (IF (NOT (GET TEM 'COMMAND-NAME))
                   (FORMAT T " is ~A, which is not implemented.~%" TEM)
                   (FORMAT T " is ~A, implemented by " (COMMAND-NAME TEM))
                   (if (get-handler-for *standard-output* :item)
                       (SEND-IF-HANDLES *STANDARD-OUTPUT* :ITEM 'FUNCTION-NAME TEM)
                     (format t "~a" tem))
                   (FORMAT T ":~%")
                   (DO L *COMMAND-HOOK* (CDR L) (NULL L)
                       (LET ((DOCFN (GET (CAR L) 'HOOK-DOCUMENTATION-FUNCTION)))
                         (AND DOCFN
                              (FUNCALL DOCFN TEM CHAR))))
                   (PRINT-DOC :FULL TEM CHAR)))
              ((CONSP TEM)
               (FORMAT T " is an alias for ~@[~:@C ~]~:@C.~%~@[~:@C ~]~:@C"
                       PREFIX
                       (SETQ CHAR (%LOGDPB (FIRST TEM) %%KBD-CONTROL-META (SECOND TEM)))
                       PREFIX CHAR)
               (GO L))
              ((MACRO-COMMAND-P TEM)
               (FORMAT T " is a user defined macro named ~A.
With no argument, run the macro with the repeat count in its definition.
With an argument, ignore the repeat count in its definition and use
the argument instead.~%"
                       (SYMEVAL-IN-CLOSURE TEM 'SYMBOL)))
              ((PREFIX-COMMAND-P TEM)
               (FORMAT T " is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
Type a subcommand to document (or * for all):~%")
               (SETQ PREFIX CHAR
                     CHAR (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
                            (READ-CHAR *STANDARD-INPUT*)))
               (FORMAT T "~%~:@C" PREFIX)
               (COND ((CHAR= CHAR #/*)
                      (FORMAT T " has these subcommands:~%")
                      (DOCUMENT-PREFIX-CHAR-TABLE (GET-PREFIX-COMMAND-COMTAB TEM)))
                     (T
                      (FORMAT T " ~:@C" CHAR)
                      (SETQ COMTAB (GET-PREFIX-COMMAND-COMTAB TEM))
                      (GO L))))
              ((MENU-COMMAND-P TEM)
               (FORMAT T " is a menu command with the following subcommands:~%")
               (DO ((L (GET-MENU-COMMAND-COMMANDS TEM) (CDR L))
                    (FLAG T NIL))
                   ((NULL L) (TERPRI))
;>> should makes these items
                 (FORMAT T "~:[, ~]~A" FLAG (CAAR L))))
              (T (FORMAT T " is garbage!?~%")))))

(DEFUN DOCUMENT-PREFIX-CHAR-TABLE (COMTAB &AUX KBD CHARS)
  (IF (DO ((CT COMTAB (COMTAB-INDIRECT-TO CT)))
          ((NULL CT) T)
        (IF (ARRAYP (SETQ KBD (COMTAB-KEYBOARD-ARRAY CT)))
            (RETURN NIL))
        (SETQ CHARS (SI:UNION-EQ CHARS (MAPCAR #'CAR KBD))))
      ;; Every level of comtab we indirect to is a sparse one.
      (DOLIST (CHAR CHARS)
        (PRINT-SHORT-DOC-FOR-TABLE CHAR COMTAB 3))
    (DOTIMES (I (ARRAY-DIMENSION KBD 1))
      (DOTIMES (J (ARRAY-DIMENSION KBD 0))
        (PRINT-SHORT-DOC-FOR-TABLE (%LOGDPB I %%KBD-CONTROL-META J) COMTAB 3)))))

(DEFUN PRINT-DOC (OP COMMAND &OPTIONAL (CHAR NIL) OVERRIDE-DOCUMENTATION-FUNCTION &AUX DOC)
  "Print documentation of COMMAND (a symbol defined with DEFCOM).
OP is the type of documentation wanted: :FULL or :SHORT.
CHAR is the character COMMAND was supposedly invoked thru;
 it is used only to pass to the documentation function if any.
OVERRIDE-DOCUMENTATION-FUNCTION means ignore any documentation function
and just use the documentation string.
A documentation function is the ZWEI:DOCUMENTATION prop of COMMAND,
and it gets as arguments COMMAND, CHAR and OP."
  (COND ((NULL COMMAND)
         (FORMAT T "The command is undefined.~%"))
        ((SYMBOLP COMMAND)
         (COND ((AND (NOT OVERRIDE-DOCUMENTATION-FUNCTION)
                     (GET COMMAND 'DOCUMENTATION-FUNCTION))
                (FUNCALL (GET COMMAND 'DOCUMENTATION-FUNCTION)
                         COMMAND CHAR OP)
                (FRESH-LINE))
               ((SETQ DOC (GET COMMAND 'DOCUMENTATION))
                (FORMAT T "~A~&"
                        (ECASE OP
                          (:FULL DOC)
                          (:SHORT
                           (IF DOC
                               (LET ((FIRST-CR (STRING-SEARCH-CHAR #/NEWLINE DOC)))
                                 (IF FIRST-CR
                                     (NSUBSTRING DOC 0 FIRST-CR)
                                     DOC))
                               "")))))))
        ((PREFIX-COMMAND-P COMMAND)
         (FORMAT T "The command is an escape-prefix for more commands.~%"))))

(DEFUN COMMAND-NAME (COMMAND &OPTIONAL NO-ERROR-P &AUX FN)
  "Return the pretty name of COMMAND (a symbol defined with a DEFCOM).
NO-ERROR-P means return NIL if data is not present (no DEFCOM was done)."
  (CHECK-TYPE COMMAND SYMBOL)
  (COND ((SETQ FN (GET COMMAND 'DOCUMENTATION-FUNCTION))
         (FUNCALL FN COMMAND NIL :NAME))
        ((GET COMMAND 'COMMAND-NAME))
        (NO-ERROR-P NIL)
        (T (FERROR "~S does not have a name" COMMAND))))

(DEFCOM COM-LIST-COMMANDS "List all extended commands." ()
  (FORMAT T "~%   Extended commands:~2%")
  (DO ((L (EXTENDED-COMMAND-ALIST *COMTAB*) (CDR L))) ((NULL L))
    (WHEN (CONSP L)
      (FORMAT T "~30,5,2A" (CAAR L))
      (PRINT-DOC :SHORT (CDAR L))
      (FRESH-LINE)))
  (FORMAT T "~%Done.~%")
  DIS-NONE)

(DEFCONST *EXTENDED-COMMAND-COMMAND* 'COM-EXTENDED-COMMAND
  "Symbol for command that gets an extended command from this comtab.
Used by APROPOS")
(DEFCONST *ANY-EXTENDED-COMMAND-COMMAND* 'COM-ANY-EXTENDED-COMMAND
  "Symbol for command that gets any extended command.  Used by APROPOS")

(DEFCOM COM-APROPOS "List commands whose names contain a given string.
Tell on which key(s) each command can be found.
Leading and trailing spaces in the substring are NOT ignored - they
must be matched by spaces in the command name." ()
  (apropos-internal t "Command Apropos. (Substring:)"))

(defcom com-current-mode-apropos
  "List currently accessible commands whose names contain a given string." ()
  (apropos-internal nil "Accessible commands Apropos. (Substring:)"))

(defun apropos-internal (use-any-extended-command-p prompt)
  (multiple-value-bind (function key)
      (get-extended-search-strings prompt)
    (let ((extended-cmd (key-for-command *extended-command-command*))
          (any-extended-cmd (key-for-command *any-extended-command-command*)))
      (dolist (x *command-alist*)
        (let* ((cmd (cdr x))
               (name (command-name cmd))
               (type 'any-extended))
          (flet ((document-command ()
                   (format t "~&~30,5,2A" name)
                   (print-doc :short cmd)
                   (send *terminal-io* :fresh-line)))
            (when (funcall function key name)
              (and (extended-command-p cmd)
                   (setq type 'extended))
              ;; Check key second so we get a key if one exists.
              (and (key-for-command cmd) ; just use for predicate here...
                   (setq type 'key))
              (case type
                (key
                 (document-command)
                 (when (> (find-command-on-keys cmd 4 "  which can be invoked via: ") 0)
                   (terpri)))
                (extended
                 (document-command)
                 (format t "  which can be invoked via: ~A ~A~%" extended-cmd name))
                (any-extended
                 (when use-any-extended-command-p
                   (document-command)
                   (format t "  which can be invoked via: ~A ~A~%" any-extended-cmd name))))))))))
  (format t "~%Done.~%")
  dis-none)



(DEFCOM COM-WHERE-IS "List all characters that invoke a given command.
Reads the command name with completion from the mini-buffer." ()
  (LET ((CMD (COMPLETING-READ-FROM-MINI-BUFFER
               "Where is:" *COMMAND-ALIST* NIL NIL
               "You are typing the name of a command, and you will be told
all characters that invoke the command."
               )))
    (IF (EQUAL CMD "") (BARF)
      (FORMAT T (CAR CMD))
      (IF (ZEROP (FIND-COMMAND-ON-KEYS (CDR CMD) #o77777 " can be invoked via: "))
          (FORMAT T " is not on any keys.~%")
        (TERPRI))))
  DIS-NONE)

(DEFUN FIND-COMMAND-ON-KEYS (COMMAND LIMIT MESSAGE
                             &OPTIONAL (COMTAB *COMTAB*))
  "Print a description of characters that would invoke COMMAND in COMTAB.
LIMIT is the maximum number of characters to mention.
MESSAGE is printed before the first character, if there are any.
Returns the number of characters found and printed."
  (DO ((STARTING-CHAR 0 (1+ CHAR-NUMBER))
       (STARTING-COMTAB COMTAB) CHAR-STRING CHAR-NUMBER
       (COUNT 0 (1+ COUNT)))
      (())
    (SETF (VALUES CHAR-STRING CHAR-NUMBER STARTING-COMTAB)
          (KEY-FOR-COMMAND COMMAND COMTAB STARTING-CHAR STARTING-COMTAB))
    (AND (NULL CHAR-STRING) (RETURN COUNT))
    (COND ((> COUNT LIMIT)
           (FORMAT T ", etc.")
           (RETURN COUNT)))
    (FORMAT T (IF (= COUNT 0)
                  MESSAGE
                ", "))
    (PRINC CHAR-STRING)))


;;; Unfortunately the mode-specific comtab is shared, can't win on those
(DEFVAR ALL-COMTABS '(*STANDARD-COMTAB* *STANDARD-CONTROL-X-COMTAB* *COMPLETING-READER-COMTAB*
                      *CONTROL-R-COMTAB* *RECURSIVE-EDIT-COMTAB* *STANDALONE-COMTAB*
                      *ZMACS-COMTAB* *ZMACS-CONTROL-X-COMTAB*))

;;; Returns a list of the commands which are in *COMMAND-ALIST* but not reachable
;;; from the current comtab.
(DEFUN UNREACHABLE-COMMAND-LIST (&AUX (L (MAPCAR #'CDR *COMMAND-ALIST*)))
  (DOLIST (COMTAB ALL-COMTABS)
    (SETQ L (UNREACHABLE-COMMAND-LIST-INTERNAL (SYMEVAL COMTAB) L)))
  (SORT L #'STRING-LESSP))

(DEFUN UNREACHABLE-COMMAND-LIST-INTERNAL (*COMTAB* L &AUX CHAR TEM KBD)
  (DO ((CT *COMTAB* (COMTAB-INDIRECT-TO CT)))
      ((ARRAYP (SETQ KBD (COMTAB-KEYBOARD-ARRAY CT)))))
  (DOTIMES (I (ARRAY-DIMENSION KBD 1))
    (DOTIMES (J (ARRAY-DIMENSION KBD 0))
      (SETQ CHAR (%LOGDPB I %%KBD-CONTROL-META J))
      (SETQ TEM (COMMAND-LOOKUP CHAR *COMTAB* T))
      (COND ((AND TEM (SYMBOLP TEM))
             (SETQ L (DELQ TEM L)))
            ((PREFIX-COMMAND-P TEM)
             (SETQ L (UNREACHABLE-COMMAND-LIST-INTERNAL
                       (GET-PREFIX-COMMAND-COMTAB TEM) L))))))
  (DOLIST (C L)
    (AND (EXTENDED-COMMAND-P C) (SETQ L (DELQ C L))))
  L)

(DEFUN EXTENDED-COMMAND-P (SYMBOL &OPTIONAL (COMTAB *COMTAB*))
  "T if SYMBOL (a DEFCOM name) is reachable thru M-X using COMTAB."
  (DO-NAMED EXTENDED-COMMAND-P
       C COMTAB (COMTAB-INDIRECT-TO C) (NULL C)
    (DOLIST (X (EXTENDED-COMMAND-ALIST C))
      (AND (EQ (CDR X) SYMBOL) (RETURN-FROM EXTENDED-COMMAND-P T)))))

(DEFCOM COM-DESCRIBE-COMMAND "Describe a command, specified by name.
Prints the full documentation for a command.  The command
may be a function name or an extended command name, and you
need only type enough to be unique." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
             "Describe command:"
             *COMMAND-ALIST*
             NIL
             NIL
             "You are typing the name of a command, which will be described."
             )))
    (IF (EQUAL X "") (BARF)
      (PRINT-DOC :FULL (CDR X))))
  DIS-NONE)

;;; This command goes on keys which are normally self-inserting.
;;; *STANDARD-COMMAND* is usually COM-SELF-INSERT.
(DEFCOM COM-ORDINARILY-SELF-INSERT DOCUMENT-STANDARD-COMMAND ()
  (FUNCALL *STANDARD-COMMAND*))

;;; This is the documentation function for *STANDARD-COMMAND*.
(DEFUN DOCUMENT-STANDARD-COMMAND (COMMAND CHAR OP)
  (CASE OP
    (:FULL
     (FORMAT T "Ordinarily a self-inserting character.  Currently, these characters do: ")
     (PRINT-DOC :FULL  *STANDARD-COMMAND* CHAR))
    (:SHORT
     (PRINC "Ordinarily self-inserting character.  Currently: ")
     (PRINT-DOC :SHORT *STANDARD-COMMAND* CHAR))
    (:NAME
     "Ordinarily Self Insert")
    (OTHERWISE
     (FERROR "Unknown operation ~A; ~S ~S" OP COMMAND CHAR))))

(DEFUN PRINT-SHORT-DOC-FOR-TABLE (CHAR COMTAB INDENTATION)
  "Document what CHAR does in COMTAB, for subcommands of prefix characters.
It prints one or two lines of stuff, with the given INDENTATION."
  (LET ((X (COMMAND-LOOKUP CHAR COMTAB T)))
    (COND ((MEMQ X '(NIL :UNDEFINED)))          ;undefined
          ((CONSP X))                   ;alias
          ((MACRO-COMMAND-P X)
           (FORMAT T "~&~V@T~:C is a user defined macro.~%" INDENTATION CHAR))
          ((PREFIX-COMMAND-P X)
           (FORMAT T "~&~V@T~:C reads another character and dispatches.~%"
                   INDENTATION CHAR))
          ((NOT (SYMBOLP X)))           ;??
          (T
           (FORMAT T "~&~V@T~:C is ~A:~%~@T"
                   INDENTATION CHAR (COMMAND-NAME X) (+ 5 INDENTATION))
           (PRINT-DOC :SHORT X CHAR)))))

(DEFCOM COM-DOCUMENT-CONTAINING-PREFIX-COMMAND "Document this command" ()
  (DECLARE (SPECIAL COMTAB))
  (DOCUMENT-PREFIX-CHAR-TABLE COMTAB)
  DIS-NONE)


;;;; Generate a ZMACS Wall chart.

(DEFUN WALLCHART (&OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*) &OPTIONAL COMTAB)
  (IF COMTAB
      (WALLCHART-COMTAB COMTAB)
    (LET ((COMMANDS (MAPCAR #'CDR *COMMAND-ALIST*)))
      (DOLIST (COMTAB ALL-COMTABS)
        (SETQ COMMANDS (WALLCHART-COMTAB COMTAB COMMANDS))
        (TERPRI))
      (FORMAT T "~|~%Not on any key: ~%~%")
      (MAPC #'(LAMBDA (X)
                (FORMAT T "~%~A" (MAKE-COMMAND-NAME X)))
            COMMANDS)))
  (TERPRI))

(DEFUN WALLCHART-COMTAB (COMTAB &OPTIONAL COMMANDS)
  (LET ((TABLE (SYMEVAL COMTAB)))
    (FORMAT T "~|~%Command chart of ~A:~%~%" (MAKE-COMMAND-NAME COMTAB))
    (DO ((LETTER 0 (1+ LETTER)))
        ((= LETTER #o237))
      (unless (lower-case-p letter)             ;Lowercase letters are just aliased.
          (DO ((BUCKY 0 (1+ BUCKY)))
              ((= BUCKY 16.))
            (LET* ((KEY (%LOGDPB BUCKY %%KBD-CONTROL-META LETTER))
                   (COMMAND (COMMAND-LOOKUP KEY TABLE)))
              (WHEN (AND COMMAND
                         (NEQ COMMAND 'COM-STANDARD)
                         (NEQ COMMAND 'COM-NUMBERS)
                         (NEQ COMMAND 'COM-ORDINARILY-SELF-INSERT)
                         (NEQ COMMAND 'COM-NEGATE-NUMERIC-ARG))
                (TERPRI)
                (SEND *STANDARD-OUTPUT* :STRING-OUT     ;So ~T works on all streams.
                      (FORMAT NIL "~:C~32,1T~A" KEY (IF (SYMBOLP COMMAND)
                                                        (MAKE-COMMAND-NAME COMMAND)
                                                      "Extended command")))
                (SETQ COMMANDS (DELQ COMMAND COMMANDS)))))))
    (TERPRI)
    COMMANDS))

(DEFUN GENERATE-WALLCHART (&OPTIONAL (FILENAME "ZWEI-COMMANDS.TABLE") COMTAB)
  (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :OUTPUT :CHARACTERS T)
    (WALLCHART FILE COMTAB)))

(DEFCOM COM-GENERATE-WALLCHART "Generates a Wallchart a la emacs for one or all comtabs.
The comtabs and the destination file are read from the minbuffer
Organised into keyboard and extended (i.e. not on a key) commands. Mouse
commands are ignored because they are not generally useful. Numeric and
self-inserting commands are not mentioned because they are obvious." ()
  (LET ((COMPLETION-ARRAY (MAKE-ARRAY (1+ (LENGTH ALL-COMTABS))
                                      :TYPE :ART-Q-LIST
                                      :LEADER-LENGTH 2)))
    (DOTIMES (ELT (1- (LENGTH COMPLETION-ARRAY)))
      (ASET (LET ((NAME (NTH ELT ALL-COMTABS)))
              (CONS (MAKE-COMMAND-NAME NAME) NAME)) COMPLETION-ARRAY ELT))
    (ASET (NCONS "All") COMPLETION-ARRAY (1- (LENGTH COMPLETION-ARRAY)))
    (SORT-COMPLETION-AARRAY COMPLETION-ARRAY)
    (LET ((COMTAB (COMPLETING-READ-FROM-MINI-BUFFER "Wallchart of:" COMPLETION-ARRAY
                                                    NIL NIL "You are typing the name of a comtab, from which a wallchart will be generated.
/"All/" will list all commands, including those not on any key.")))
      (AND (ATOM COMTAB) (BARF))
      (GENERATE-WALLCHART (READ-DEFAULTED-AUX-PATHNAME "Put wallchart into:") (CDR COMTAB))))
  DIS-NONE)

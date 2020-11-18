;;; Zwei commands, see ZWEI;COMA for comments -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; This is SYS: ZWEI; COMG
;;;
;;; ** (c) Copyright 1981, 1984 Massachusetts Institute of Technology **

(DEFUN ASSURE-MACRO-STREAM (OPERATION)
  "BARF unless the standard input is a macro stream (by testing for handling OPERATION)"
  (UNLESS (SEND *STANDARD-INPUT* :OPERATION-HANDLED-P OPERATION)
    (BARF "This stream does not support macros")
    NIL))

(DEFCOM COM-SET-KEY "Install a specified editor command on a specified character.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
  (LET ((COMMAND (COMPLETING-READ-FROM-MINI-BUFFER "Command to install" *COMMAND-ALIST*)))
    (UNLESS (LISTP COMMAND) (BARF))
    (INSTALL-COMMAND-INTERNAL (CDR COMMAND))))

(DEFCOM COM-DEFINE-CHARACTER "Install a specified editor command on a specified character.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
  (LET ((COMMAND (COMPLETING-READ-FROM-MINI-BUFFER "Command to install" *COMMAND-ALIST*)))
    (UNLESS (LISTP COMMAND) (BARF))
    (INSTALL-COMMAND-INTERNAL (CDR COMMAND))))

(DEFCOM COM-INSTALL-COMMAND "Install a specified function on a specified editor command character.
The name of the function is read from the mini-buffer (the top of the kill ring
contains the name of the current defun), and a character from the echo area.
Two-character sequences starting with prefixes such as Control-X are also allowed;
however, a numeric argument means to regard the prefix character itself as the
character to be redefined." ()
  (loop
    (let ((name (READ-FUNCTION-NAME "Name of function to install"
                                    (RELEVANT-FUNCTION-NAME (POINT)) NIL 'ALWAYS-READ))
          new-name)
      (AND (OR (FBOUNDP NAME)
               (setq new-name (si:dwimify-package-0 name 'fdefinedp)) ;in case we are not in the zwei package at the moment
               (FQUERY () "~A is not defined, ok to install anyway? " NAME))
           (RETURN (INSTALL-COMMAND-INTERNAL (or new-name NAME)))))))

(DEFCOM COM-INSTALL-MACRO "Install a specified user macro on a specifed key.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer, and the keystroke on which
to install it is read in the echo area.
If the key is currently holding a command prefix (like Control-X), it will ask
you for another character, so that you can redefine Control-X commands.  However,
with a numeric argument, it will assume you want to redefine Control-X itself,
and will not ask for another character." ()
  (COM-INSTALL-MACRO-INTERNAL NIL))

(DEFCOM COM-INSTALL-MOUSE-MACRO
        "Like Install Macro, but moves the mouse to where clicked first." ()
  (COM-INSTALL-MACRO-INTERNAL T))

(DEFCOM COM-UNDEFINE-CHARACTER "Remove the editor command definition of a character.
Reads the key to undefine using the echo area.
Two-character sequences starting with Control-X are also allowed." ()
  (INSTALL-COMMAND-INTERNAL NIL NIL T))

(DEFUN COM-INSTALL-MACRO-INTERNAL (MOUSE-P)
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
        (name (COMPLETING-READ-FROM-MINI-BUFFER
                "Type name of macro to install, or name the most recently defined macro:"
                (MAPCAR (LAMBDA (X) (CONS (SI::PATCH-NAME X) X))
                  SI::PATCH-SYSTEMS-LIST))))
        (NAME (TYPEIN-LINE-READ "Type name of macro to install, or name the most recently defined macro:")))
    (COND ((EQ NAME '*EOF*)
           (SETQ MAC (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)
                 NAME (GENSYM))
           (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
          ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
           (BARF "~A is not a defined macro." NAME)))
    (INSTALL-COMMAND-INTERNAL (MAKE-MACRO-COMMAND NAME MOUSE-P) T)))
    (pushnew (cons name mac) *keyboard-macro-alist*)
    (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))

(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND &OPTIONAL REMEMBER-OLD-P DEINSTALL-P
                                 &AUX KEY-LIST)
  (PROMPT-LINE (IF DEINSTALL-P "Key to deinstall:" "Key to get it:"))
  (CLEAR-PROMPTS)
  (ALWAYS-DISPLAY-PROMPTS)
  (DO ((COMTAB *COMTAB*)
       (KEY (INPUT-WITH-PROMPTS *STANDARD-INPUT* :MOUSE-OR-KBD-TYI)
            (INPUT-WITH-PROMPTS *STANDARD-INPUT* :TYI)))
      (NIL)
    (PUSH KEY KEY-LIST)
    (RECORD-MINI-BUFFER-VALUE NIL)
    (LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB)))
      (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
                  (NOT *NUMERIC-ARG-P*))
             (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
            ((Y-OR-N-P "Install command ~S on~{ ~:@C~}? " COMMAND (REVERSE KEY-LIST))
             ;; >>This change seems to be broken, and I don't think it can actually do the
             ;; >>right thing anyway. (RpK, 9-May-86 16:17:20)
             ;; Now we know the key sequence, so decide which comtab to really start from.
             #|
             (DO ((COMTAB (SYMEVAL (FQUERY '(:CHOICES (((*ZMACS-COMTAB* "Zmacs") #\Z)
                                                       ((*STANDARD-COMTAB* "All Zwei editors") #\A)
                                                       ((*COMTAB* "Just this window") #\W #\T))
                                                      :TYPE :TYI)
                                           "Define for Zmacs, all editors, or just this window? ")))
                  (KEYS KEY-LIST (CDR KEY-LIST)))
                 ((NULL (CDR KEY-LIST)) ... clauses below)
               (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB))) |#
             (AND DEINSTALL-P
                  (SETQ COMMAND (MOUSE-MACRO-COMMAND-LAST-COMMAND
                                  (COMMAND-LOOKUP KEY COMTAB))))
             (AND REMEMBER-OLD-P
                  (SET-MOUSE-MACRO-COMMAND-LAST-COMMAND COMMAND
                                                        (COMMAND-LOOKUP KEY COMTAB)))
             (COMMAND-STORE COMMAND KEY COMTAB)
             (FORMAT *QUERY-IO* "~&Installed.")
             (RETURN NIL))
            (T (FORMAT *QUERY-IO* "~&Not installed.")
               (RETURN NIL)))))
  DIS-NONE)

;;; EMACS compatible macro commands
(DEFCOM COM-START-KEYBOARD-MACRO "Begin defining a keyboard macro.
A numeric argument means append to the previous keyboard macro." (KM)
  (ASSURE-MACRO-STREAM :MACRO-PUSH)
  (SEND *STANDARD-INPUT* :MACRO-PUSH (+ 2 *NUMERIC-ARG-N-DIGITS*)
        (AND *NUMERIC-ARG-P* (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
  DIS-NONE)

(DEFCOM COM-END-KEYBOARD-MACRO "Terminate the definition of a keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-POP)
  (*CATCH 'MACRO-LOOP                           ;In case no macro running
     (SEND *STANDARD-INPUT* :MACRO-POP (+ 2 *NUMERIC-ARG-N-DIGITS*)
           (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*)))
  DIS-NONE)

(DEFCOM COM-CALL-LAST-KEYBOARD-MACRO "Repeat the last keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-EXECUTE)
  (SEND *STANDARD-INPUT* :MACRO-EXECUTE NIL (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*))
  DIS-NONE)

(DEFCOM COM-KEYBOARD-MACRO-QUERY "Interactive keyboard macro" ()
  (ASSURE-MACRO-STREAM :MACRO-QUERY)
  (SEND *STANDARD-INPUT* :MACRO-QUERY (+ 2 *NUMERIC-ARG-N-DIGITS*))
  DIS-NONE)

(DEFCOM COM-VIEW-KEYBOARD-MACRO "Typeout the specified keyboard macro.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer.
Just Return means the last one defined, even if temporary." ()
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
        NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to view (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
           (SETQ MAC (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
          ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
           (BARF "~A is not a defined macro." NAME)))
    (DO ((I 0 (1+ I))
         (LEN (MACRO-LENGTH MAC))
         (CH))
        ((> I LEN))
      (FORMAT T (SELECTQ (SETQ CH (AREF MAC I))
                  (*MOUSE* "Mouse command ~*")
                  (*SPACE* "Macro query ~*")
                  (*RUN* "Repeat ~*")
                  (NIL "Input ~*")
                  (OTHERWISE "~:C "))
              CH)))
  DIS-NONE)

(DEFCOM COM-NAME-LAST-KEYBOARD-MACRO "Make the last temporary macro permanent.
The new name for the macro is read from the mini-buffer." ()
  (ASSURE-MACRO-STREAM :MACRO-PREVIOUS-ARRAY)
  (LET* ((MAC (OR (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)
                  (BARF "There is no previous keyboard macro")))
         (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
         (NAME (TYPEIN-LINE-READ "Name for macro:")))
    (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
  DIS-NONE)

;;; Sorting commands
(DEFCOM COM-SORT-LINES "Sort the region alphabetically by lines." ()
  (REGION (BP1 BP2)
    (SORT-LINES-INTERVAL #'STRING-LESSP BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-SORT-PARAGRAPHS "Sort the region alphabetically by paragraphs." ()
  (REGION (BP1 BP2)
    (SORT-INTERVAL-FUNCTIONS #'FORWARD-OVER-BLANK-OR-TEXT-JUSTIFIER-LINES
                             #'(LAMBDA (BP) (FORWARD-PARAGRAPH BP 1 T))
                             #'(LAMBDA (BP) BP)
                             #'INTERVAL-WITH-SORT-INTERVAL-LESSP
                             BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-SORT-PAGES
  "Sort the region alphabetically by pages.
A page separator is inserted at the end of the region if there
was not already one there.  For proper results, you may wish to
have an empty line at the beginning of the region, to match the
empty line usually placed at the start of pages." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Sort" BP1 BP2 T)
      (LET ((NEWDELIM (BEG-LINE-P BP2)))
        (IF NEWDELIM
            (INSERT-MOVING BP2 (CAR *PAGE-DELIMITER-LIST*)))
        (SORT-INTERVAL-FUNCTIONS #'(LAMBDA (BP)
                                     (LOOP
                                       WHILE
                                       (AND (NOT (BP-= BP (INTERVAL-LAST-BP *INTERVAL*)))
                                            (OR (MEMQ (BP-CHAR BP) *PAGE-DELIMITER-LIST*)
                                                (END-LINE-P BP)))
                                       DO
                                       (IBP BP))
                                     BP)
                                 #'(LAMBDA (BP) (FORWARD-PAGE BP 1 T))
                                 #'(LAMBDA (BP) BP)
                                 #'INTERVAL-WITH-SORT-INTERVAL-LESSP
                                 BP1 BP2 T)
        ;; If originally was no separator at the end, make it so again.
        (IF NEWDELIM
            (REGION (BP3 BP4)
              (DELETE-INTERVAL (FORWARD-CHAR BP4 -1) BP4))))))
  DIS-TEXT)

(DEFVAR *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*)

;;; This returns a function which takes a BP and returns a resultant BP after performing
;;; the given keyboard-macro operation.
(DEFUN MAKE-KEYBOARD-MACRO-MOVER (PROMPT)
  "Returns a function which takes a BP, moves, and returns a BP.
The function is defined to perform the ZWEI commands that you type
while MAKE-KEYBOARD-MACRO-MOVER is running.  Prompts with PROMPT."
  (COM-START-KEYBOARD-MACRO)
  (FORMAT *QUERY-IO* "~&Defining a keyboard macro to ~A~@[; type ~A to finish it~]"
          PROMPT (KEY-FOR-COMMAND 'COM-END-KEYBOARD-MACRO))
  (LET ((MACRO-ERROR-HOOK #'(LAMBDA ()
                              (*THROW 'EXIT-MAKE-KEYBOARD-MACRO-MOVER :MACRO-ERROR)))
        (MACRO-POP-HOOK #'(LAMBDA ()
                            (*THROW 'EXIT-MAKE-KEYBOARD-MACRO-MOVER T))))
    (AND (EQ (*CATCH 'EXIT-MAKE-KEYBOARD-MACRO-MOVER
               (SEND SELF :EDIT))
             :MACRO-ERROR)
         (*THROW 'ZWEI-COMMAND-LOOP T)))
  (COND ((NOT (BOUNDP '*MAKE-KEYBOARD-MACRO-MOVER-COMTAB*))
         (SETQ *MAKE-KEYBOARD-MACRO-MOVER-COMTAB* (CREATE-SPARSE-COMTAB 'MACRO-MOVER-COMTAB))
         (SETF (COMTAB-KEYBOARD-ARRAY *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*)
               '((-1 . COM-EXIT-KEYBOARD-MACRO-MOVER)))))
  (SET-COMTAB-INDIRECTION *MAKE-KEYBOARD-MACRO-MOVER-COMTAB* *COMTAB*)
  (LET-CLOSED ((OLD-MACRO-PREVIOUS-ARRAY (SEND *STANDARD-INPUT* :MACRO-PREVIOUS-ARRAY)))
    (ARRAY-PUSH-EXTEND OLD-MACRO-PREVIOUS-ARRAY -1)
    (SETF (MACRO-LENGTH OLD-MACRO-PREVIOUS-ARRAY)
          (1- (MACRO-POSITION OLD-MACRO-PREVIOUS-ARRAY)))
    #'(LAMBDA (BP &AUX (POINT (POINT)) OLD-POINT
               (MACRO-ERROR-HOOK #'(LAMBDA () (*THROW 'EXIT-KEYBOARD-MACRO-MOVER :MACRO-ERROR))))
        (SETQ OLD-POINT (COPY-BP POINT :NORMAL))
        (MOVE-BP (POINT) BP)
        (UNWIND-PROTECT
          (PROGN
            (SEND *STANDARD-INPUT* :MACRO-EXECUTE OLD-MACRO-PREVIOUS-ARRAY 1)
            (AND (EQ (*CATCH 'EXIT-KEYBOARD-MACRO-MOVER
                       (SEND *WINDOW* :EDIT NIL *MAKE-KEYBOARD-MACRO-MOVER-COMTAB*))
                     :MACRO-ERROR)
                 (*THROW 'ZWEI-COMMAND-LOOP T))
            (COPY-BP POINT))
          (MOVE-BP (POINT) OLD-POINT)
          (FLUSH-BP OLD-POINT)))))

(DEFUN COM-EXIT-KEYBOARD-MACRO-MOVER ()
  (*THROW 'EXIT-KEYBOARD-MACRO-MOVER T))

(DEFCOM COM-SORT-VIA-KEYBOARD-MACROS "Sort the region alphabetically.
You are prompted to enter three keyboard macros.
 one that moves from the beginning of a sort record to the start of its key,
 one that moves from the start of the key to the end of the key,
 one that moves from the end of the key to the end of the containing record.E
End each macro with C-X ).
These macros are used to divide the region up into records and find their sort keys.
Then the records are rearranged to put their sort keys in alphabetical order." ()
  (REGION (BP1 BP2)
    (WITH-BP (FIRST-BP BP1 :NORMAL)
      (WITH-BP (LAST-BP BP2 :MOVES)
        (SETF (WINDOW-MARK-P *WINDOW*) NIL)
        (MOVE-BP (POINT) FIRST-BP)
        (MUST-REDISPLAY *WINDOW* DIS-BPS)
        (LET ((MOVE-TO-KEY-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move to the start of the sort key"))
              (MOVE-OVER-KEY-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move over the sort key"))
              (MOVE-TO-NEXT-MACRO (MAKE-KEYBOARD-MACRO-MOVER "move to the end of the record")))
          (SORT-INTERVAL-FUNCTIONS MOVE-TO-KEY-MACRO MOVE-OVER-KEY-MACRO MOVE-TO-NEXT-MACRO
                                   #'INTERVAL-WITH-SORT-INTERVAL-LESSP FIRST-BP LAST-BP T)))))
  DIS-TEXT)

(DEFUN READ-FLAVOR-NAME (PROMPT HELP)
  "Reads a flavor name using the mini buffer, prompting with PROMPT.
HELP is a string to print to say what the flavor name is for."
  (SORT-COMPLETION-AARRAY SI:*ALL-FLAVOR-NAMES-AARRAY*)
  (LET ((FLAVOR (COMPLETING-READ-FROM-MINI-BUFFER PROMPT SI:*ALL-FLAVOR-NAMES-AARRAY*
                                                  NIL NIL HELP))
        STRING TEM)
    (AND (ATOM FLAVOR) (BARF))
    (SETQ STRING (CAR FLAVOR))
    (SETQ FLAVOR (CDR FLAVOR))
    ;; If have flavors of the same name in more than one package,
    ;; and a package prefix was not given, the one we get is random.
    ;; So use the one in the current package, if there is such a one.
    (IF (AND (EQUAL STRING (SYMBOL-NAME FLAVOR))        ;No prefix specified.
             (NEQ (SYMBOL-PACKAGE FLAVOR) PACKAGE)
             (SETQ TEM (INTERN-SOFT (SYMBOL-NAME FLAVOR) *PACKAGE*))
             (NEQ TEM FLAVOR)                   ;And the sym is not this package's
             (GET TEM 'SI:FLAVOR))              ;and this package's sym is a defined flavor.
        TEM
      FLAVOR)))

(DEFCOM COM-DESCRIBE-FLAVOR "Describe the specified flavor." ()
  (DESCRIBE-FLAVOR-INTERNAL
    (READ-FLAVOR-NAME "Describe flavor:"
                      "You are typing the name of a flavor, to be described."))
  DIS-NONE)

(DEFUN DESCRIBE-FLAVOR-INTERNAL (FLAVOR &AUX FL TEM)
  (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
      (BARF "~S is not the name of a flavor" FLAVOR))
  (FORMAT T "~&Flavor ")
  (SEND *STANDARD-OUTPUT* :ITEM 'FLAVOR-NAME FLAVOR)
  (COND ((SETQ TEM (DONT-OPTIMIZE (SI:FLAVOR-DEPENDS-ON FL)))
         (FORMAT T " directly depends on flavor~P:~%" (LENGTH TEM))
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FLAVOR-NAME TEM))
        (T
         (FORMAT T " does not directly depend on any other flavors~%")))
  (COND ((SETQ TEM (DONT-OPTIMIZE (SI:FLAVOR-INCLUDES FL)))
         (FORMAT T "~& and directly includes flavor~P:~%"
                 (LENGTH TEM))
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FLAVOR-NAME TEM)))
  (COND ((SETQ TEM (DONT-OPTIMIZE (SI:FLAVOR-DEPENDED-ON-BY FL)))
         (FORMAT T "~& and is directly depended on by flavor~P:~%"
                 (LENGTH TEM))
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FLAVOR-NAME TEM)))
  (IF (NULL (DONT-OPTIMIZE (SI:FLAVOR-DEPENDS-ON-ALL FL)))
      (SI:COMPOSE-FLAVOR-COMBINATION FL))
  (COND ((SETQ TEM (CDR (DONT-OPTIMIZE (SI:FLAVOR-DEPENDS-ON-ALL FL))))
         (FORMAT T "~&Its entire list of components is:~%")
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FLAVOR-NAME TEM)))
  (LET ((LIV (DONT-OPTIMIZE (SI:FLAVOR-LOCAL-INSTANCE-VARIABLES FL))))
    (SETQ LIV (LOOP FOR V IN LIV COLLECT (IF (ATOM V) V (CAR V))))
    (IF (NULL LIV)
        (FORMAT T "~&~S has no local instance variables~%" FLAVOR)
      (FORMAT T "~&Instance variable~P of ~S: "
              (LENGTH LIV) FLAVOR )
      (FORMAT:PRINT-LIST *STANDARD-OUTPUT* "~S" LIV)
      (TERPRI *STANDARD-OUTPUT*))
    (COND ((SETQ TEM (DONT-OPTIMIZE (SI:FLAVOR-INSTANCE-SIZE FL)))
           (FORMAT T "Flavor ~S has instance size ~D"
                   FLAVOR TEM)
           (LET ((IIV (REM-IF #'(LAMBDA (X) (MEMQ X LIV))
                              (DONT-OPTIMIZE (SI:FLAVOR-ALL-INSTANCE-VARIABLES FL)))))
             (COND (IIV
                    (FORMAT T ",
 with inherited instance variables: ")
                    (FORMAT:PRINT-LIST *STANDARD-OUTPUT* "~S" IIV))
                   (T (TYO #/. *STANDARD-OUTPUT*))))
           (TERPRI *STANDARD-OUTPUT*))))
  (LET ((STATE (LIST NIL NIL)))
    (DOLIST (F (DONT-OPTIMIZE (SI:FLAVOR-DEPENDS-ON-ALL FL)))
      (DESCRIBE-FLAVOR-1 F STATE FLAVOR)))
  (DO ((PLIST (DONT-OPTIMIZE (SI:FLAVOR-PLIST FL)) (CDDR PLIST))
       (FLAG NIL))
      ((NULL PLIST))
    (COND ((NOT (MEMQ (CAR PLIST) '(:DEFAULT-INIT-PLIST
                                     :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)))
           (COND ((NOT FLAG)
                  (FORMAT T "Random properties:~%")
                  (SETQ FLAG T)))
           (FORMAT T "~5X~S:    ~S~%" (CAR PLIST) (CADR PLIST)))))
  NIL)

(DEFUN DESCRIBE-FLAVOR-1 (FLAVOR STATE TOP-FLAVOR-NAME &AUX FL (FLAVOR-FLAG NIL))
  (COND ((NULL (SETQ FL (GET FLAVOR 'SI:FLAVOR))))
        ((NOT (MEMQ FL (SECOND STATE)))
         (DO ((MTES (SORTCAR (COPYLIST (DONT-OPTIMIZE (SI:FLAVOR-METHOD-TABLE FL)))
                             'ALPHALESSP)
                    (CDR MTES))
              (MTE)
              (ELEM)
              (MSG)
              (MSG-FLAG NIL NIL)
              (TEM))
             ((NULL MTES))
           (SETQ MTE (CAR MTES) MSG (FIRST MTE) MTE (CDDDR MTE))
           (OR (SETQ ELEM (ASSQ MSG (FIRST STATE)))
               (PUSH (SETQ ELEM (LIST MSG NIL NIL)) (FIRST STATE)))
           (COND ((AND (SETQ TEM (SI:METH-LOOKUP MTE :BEFORE))
                       (SI:METH-DEFINEDP TEM)
                       (NOT (MEMQ TEM (SECOND ELEM))))
                  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
                    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "before"
                                               MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
                  (PUSH TEM (SECOND ELEM))))
           ;; This assumes the combination type is daemon.  Hard to check at this level.
           (COND ((AND (SETQ TEM (SI:METH-LOOKUP MTE 'NIL))
                       (SI:METH-DEFINEDP TEM)
                       (NULL (THIRD ELEM)))
                  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
                    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "primary"
                                               MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
                  (SETF (THIRD ELEM) TEM)))
           (COND ((AND (SETQ TEM (SI:METH-LOOKUP MTE :AFTER))
                       (SI:METH-DEFINEDP TEM)
                       (NOT (MEMQ TEM (SECOND ELEM))))
                  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
                    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "after"
                                               MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
                  (PUSH TEM (SECOND ELEM))))
           (COND ((AND (SETQ TEM (SI:METH-LOOKUP MTE :WRAPPER))
                       (SI:METH-DEFINEDP TEM)
                       (NOT (MEMQ TEM (SECOND ELEM))))
                  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
                    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM) "wrapper"
                                               MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
                  (PUSH TEM (SECOND ELEM))))
           ;In case there are any other method types
           (LOOP FOR TEM IN MTE
                 UNLESS (MEMQ (SI:METH-METHOD-TYPE TEM)
                              '(:BEFORE NIL :AFTER :WRAPPER :COMBINED))
                   UNLESS (OR (NOT (SI:METH-DEFINEDP TEM)) (MEMQ TEM (SECOND ELEM)))
                     DO (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
                          (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (SI:METH-FUNCTION-SPEC TEM)
                                                     (STRING-DOWNCASE
                                                       (SI:METH-METHOD-TYPE TEM))
                                                     MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
                        (PUSH TEM (SECOND ELEM)))
           (AND MSG-FLAG (TERPRI *STANDARD-OUTPUT*)))
         (SETQ FLAVOR-FLAG
               (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
                 FL (SORT (COPYLIST (DONT-OPTIMIZE (SI:FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)))
                          'ALPHALESSP)
                 "automatically-generated methods to get instance variable" ""
                 FLAVOR-FLAG TOP-FLAVOR-NAME))
         (SETQ FLAVOR-FLAG
               (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
                 FL (SORT (COPYLIST (DONT-OPTIMIZE (SI:FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)))
                          'ALPHALESSP)
                 "automatically-generated methods to set instance variable" ""
                 FLAVOR-FLAG TOP-FLAVOR-NAME))
         (SETQ FLAVOR-FLAG
               (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
                 FL
                 (SORT (MAPCAR #'CDR (DONT-OPTIMIZE
                                       (SI:FLAVOR-INITTABLE-INSTANCE-VARIABLES FL)))
                       'ALPHALESSP)
                 "instance variable" " that may be set by initialization"
                 FLAVOR-FLAG TOP-FLAVOR-NAME))
         (SETQ FLAVOR-FLAG
               (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
                 FL (SORT (COPYLIST (DONT-OPTIMIZE (SI:FLAVOR-INIT-KEYWORDS FL))) 'ALPHALESSP)
                 "keyword" " in the :INIT message"
                 FLAVOR-FLAG TOP-FLAVOR-NAME))
         (SETQ FLAVOR-FLAG
               (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
                 FL (SORT (COPYLIST (SI:FLAVOR-GET FL :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
                          'ALPHALESSP)
                 "macros to access variable" ""
                 FLAVOR-FLAG TOP-FLAVOR-NAME))
         (LET ((DEFAULT-PLIST (SI:FLAVOR-GET FL :DEFAULT-INIT-PLIST)))
           (COND (DEFAULT-PLIST
                  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
                  (FORMAT T " Plus default init plist: ")
                  (DO ((L DEFAULT-PLIST (CDDR L))
                       (FLAG T NIL))
                      ((NULL L))
                    (FORMAT T "~:[, ~]~S ~S" FLAG (CAR L) (CADR L)))
                  (TERPRI *STANDARD-OUTPUT*))))
         (PUSH FL (SECOND STATE))))
  STATE)

(DEFUN DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME (FL FLAG TOP-FLAVOR-NAME &AUX FLAVOR-NAME)
  (COND ((NOT FLAG)                             ;If not already printed
         (SETQ FLAVOR-NAME (DONT-OPTIMIZE (SI:FLAVOR-NAME FL)))
         (FORMAT T "Method(s) ~:[inherited from~;of~] ~S:~%"
                 (EQ FLAVOR-NAME TOP-FLAVOR-NAME) FLAVOR-NAME)))
  T)                                            ;New value of flag

(DEFUN DESCRIBE-FLAVOR-PRINT-MSG (FL MSG FUNCTION TYPE MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME)
  ;; If method type is :CASE, mention suboperation too.
  (IF (FIFTH FUNCTION)
      (SETQ TYPE
            (FORMAT:OUTPUT NIL
              (PRINC TYPE) " "
;             (COND ((EQUAL (SYMBOL-PACKAGE (FIFTH FUNCTION))
;                           PKG-GLOBAL-PACKAGE)
;                    (PRINC ":")
;                    (PRIN1 (FIFTH FUNCTION)))
;                   (T
                     (LET ((*PACKAGE* NIL)) (PRIN1 (FIFTH FUNCTION)))
;                    ))
                     )))
  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
  (IF MSG-FLAG
      (PRINC ", ")
    (FORMAT:OUTPUT T
      "   "
      (COND ((EQUAL (SYMBOL-PACKAGE MSG)
                    PKG-GLOBAL-PACKAGE)
             (PRINC ":")
             (PRIN1 MSG))
            (T
             (LET ((*PACKAGE* NIL)) (PRIN1 MSG))))
      " "))
  (WHEN (> (+ (LENGTH TYPE) (SEND *STANDARD-OUTPUT* :READ-CURSORPOS :CHARACTER))
           (SEND *STANDARD-OUTPUT* :SIZE-IN-CHARACTERS))
    (TERPRI)
    (PRINC "     "))
  (SEND *STANDARD-OUTPUT* :ITEM 'FUNCTION-NAME FUNCTION TYPE)
  (VALUES T T))                         ;New values for the flags

(DEFUN DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST (FL LIST STR1 STR2 FLAG TOP-FLAVOR-NAME)
  (COND (LIST                                   ;If there is something there
         (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAG TOP-FLAVOR-NAME)
         (FORMAT T " Plus ~A~P~A: ~{~<~%  ~2:;~:S~>~^, ~}~%"
                                  STR1 (LENGTH LIST) STR2 LIST)
         T)                                     ;New value of the flag
        (T FLAG)))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Edit"
                          EDIT-DEFINITION-FOR-MOUSE T
                          "Edit the definition of this flavor.")

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Describe"
                          DESCRIBE-FLAVOR-INTERNAL NIL
                          "Describe this flavor.")

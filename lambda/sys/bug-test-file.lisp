

Babyl Options:
Version:5
Labels:doc.prob,unreproducible,fixed,issues,3,valid,2,4,indeterminate,critical,cosmetic

0,, 3, valid,
*** EOOH ***
Date: Thursday, 8 May 1986, 19:19-EDT
From: Robert Putnam <robert>
Message-ID: <8605082319.AA02589@lmi-angel.ARPA>
To: bug-lispm

To: BUG-LISPM@LMI-Angel
--Text Follows This Line--
In System 110.232, Lambda-Diag 7.17, Experimental Local-File 68.7,
FILE-Server 18.4, Unix-Interface 9.1, ZMail 65.14, Object Lisp 3.4,
Tape 6.39, Site Data Editor 3.3, Tiger 24.0, KERMIT 31.3,
Window-Maker 1.1, Gateway 4.8, TCP-Kernel 39.7, TCP-User 62.7,
TCP-Server 45.5, MEDIUM-RESOLUTION-COLOR 3.4,
MICRO-COMPILATION-TOOLS 3.2, System Revision Level 3.10, microcode 1511,
SDU ROM 102, on Claude Debussy (LAMBDA):


Insert your description of the circumstances here:

  Did M-x Install Macro.  Prompt in mini-buffer was

      Name of macro to install (

         instead of

   Name of macro to install (#\return for last macro defined):

robert



; release 3.0
(DEFMETHOD (SHEET :STRING-OUT) (STRING &OPTIONAL START STOP)
  (WHEN (SYMBOLP STRING) (SETQ STRING (SYMBOL-NAME STRING)))
  (WHEN (NULL START) (SETQ START 0))
  (WHEN (NULL STOP) (SETQ STOP (ARRAY-ACTIVE-LENGTH STRING)))
  (DO ((XLIM (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG))
                 (SHEET-INSIDE-RIGHT)
               (- (SHEET-INSIDE-RIGHT) CHAR-WIDTH)))
       (STOPPED))
      (())
    (PREPARE-SHEET (SELF)
      (SHEET-HANDLE-EXCEPTIONS-IF-NECESSARY)
      (MULTIPLE-VALUE-SETQ (STOPPED CURSOR-X)
        (%DRAW-STRING SELF CHAR-ALUF CURSOR-X CURSOR-Y STRING CURRENT-FONT START STOP XLIM))
      (COND ((> STOPPED STOP)                          ;All characters drawn.
             (RETURN NIL))
            ((EQ (ZL:AREF STRING STOPPED) #/NEWLINE)    ;Stopped at newline.
             (SEND SELF :END-OF-LINE-EXCEPTION)
             (SETQ START (1+ STOPPED)))
            (T                                         ;Stopped at horizontal limit.
             (SEND SELF :END-OF-LINE-EXCEPTION)
             (SETQ START STOPPED))))))

(DEFUN SHEET-STRING-OUT (SHEET STRING &OPTIONAL (START 0) (STOP NIL))
  "Output STRING or portion thereof on SHEET."
  (SEND SHEET :STRING-OUT STRING START STOP))

;;; *** SHEET-STRING-OUT now does the right thing with all kinds of strings ***

;; from release 2.0
(defmethod (sheet :string-out) (string &optional (start 0) end)
  (sheet-string-out self string start end))

(defun sheet-string-out (sheet string &optional (start 0) (stop nil))
  "Output STRING or portion thereof on SHEET."
  (prepare-sheet (sheet)
    (when (symbolp string) (setq string (symbol-name string)))
    (when (null stop) (setq stop (array-active-length string)))
    (do ((aluf (sheet-char-aluf sheet))
         (xlim (if (zerop (sheet-right-margin-character-flag sheet))
                   (sheet-inside-right sheet)
                   (- (sheet-inside-right sheet) (sheet-char-width sheet))))
         (font (sheet-current-font sheet))
         (stop-index)
         (stop-xpos))
        (nil)
      (multiple-value (stop-index stop-xpos)
        (%draw-string
          sheet aluf (sheet-cursor-x sheet) (sheet-cursor-y sheet) string font start stop xlim))
      (setf (sheet-cursor-x sheet) stop-xpos)
      (cond ((> stop-index stop)                        ;All characters drawn.
             (return nil))
            ((eq (aref string stop-index) #/Newline)    ;Stopped at newline.
             (send sheet :end-of-line-exception)
             (setq start (1+ stop-index)))
            (t                                          ;Stopped at horizontal limit.
             (send sheet :end-of-line-exception)
             (setq start stop-index))))))

;;; *** SHEET-STRING-OUT now does the right thing with all kinds of strings ***

From rpk Fri May  9 16:38:11 1986
Received: by lmi-angel.ARPA (4.12/4.7)  id AA03851; Fri, 9 May 86 16:37:59 edt
Date: Fri, 9 May 86 16:37:59 edt
From: Bob Krajewski <rpk>
Message-Id: <8605092037.AA03851@lmi-angel.ARPA>
To: robert
Cc: bug-lispm
Cc: rms
In-Reply-To: Robert Putnam's message of Thu, 8 May 86 19:19:17 edt
Subject: m-X Install Macro (prompt bug)
Status: R

There was actually more than one bug here: the \LOZENGED-CHAR\ directive
that ZWEI::TYPEIN-LINE-READLINE was using should have been using the
CHAR-NAME function, but then it also turned out that you can't get lozenged
characters in the minibuffer prompt anyway.

And command installation was broken anyway: I tried it with a named keyboard
macro, and constantly kept being thrown in the error handler, when I tried
to install a command on a key sequence like c-X 7, although it did work with
single-stroke keys like h-m-F.  It turned out that somebody (RMS ?  MLY ?)
changed the code to give the user a choice of installing the macro on
different comtabs (for all editors, Zmacs, or just the current window).
This cannot really work once you try to install a command on an extended
command, so I took out the loop that implemented it.  If somebody wants to
reinstate the feature and make it work in all cases, they are welcome to.

Anyway, I changed the sources on DJ (IO; FORMAT and ZWEI; COMG), and the
patch is in LAD: RPK.L.FIX; INSTALL-MACRO-FIX.LISP.






0,, valid, 2,
*** EOOH ***
Date: Thursday, 8 May 1986, 16:34-EDT
From: Robert Putnam <robert>
Message-ID: <8605082034.AA01609@lmi-angel.ARPA>
To: bug-lispm

Here's an unwonderful fix to the compile-file problem (where compile-file tries to
compile QFASL files).

;original
(DEFUN COMPILE-FILE (INPUT-FILE
                     &KEY OUTPUT-FILE
                          output-filename
                          (SET-DEFAULT-PATHNAME T)
                          LOAD
                          ((:PACKAGE PACKAGE-SPEC)))
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the file after compiling it."
  (LET* ((FILE (MERGE-PATHNAMES (OR INPUT-FILE "") *DEFAULT-PATHNAME-DEFAULTS*))
         (RESULT (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on compiling ~A." FILE)
                   (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry compiling ~A." FILE)
                     (QC-FILE FILE (or OUTPUT-FILE output-filename)
                              NIL NIL PACKAGE-SPEC NIL
                              (NOT SET-DEFAULT-PATHNAME))))))
    (AND RESULT LOAD (LOAD RESULT :SET-DEFAULT-PATHNAME NIL))
    RESULT))

; use fs:merge-pathname-defaults instead of merge-pathnames.
(DEFUN COMPILE-FILE (INPUT-FILE
                     &KEY OUTPUT-FILE
                          output-filename
                          (SET-DEFAULT-PATHNAME T)
                          LOAD
                          ((:PACKAGE PACKAGE-SPEC)))
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the file after compiling it."
  (LET* ((FILE (FS:MERGE-PATHNAME-DEFAULTS (OR INPUT-FILE "") *DEFAULT-PATHNAME-DEFAULTS*))
         (RESULT (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on compiling ~A." FILE)
                   (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry compiling ~A." FILE)
                     (QC-FILE FILE (or OUTPUT-FILE output-filename)
                              NIL NIL PACKAGE-SPEC NIL
                              (NOT SET-DEFAULT-PATHNAME))))))
    (AND RESULT LOAD (LOAD RESULT :SET-DEFAULT-PATHNAME NIL))
    RESULT))


; From discussion of fs:merge-pathname-defaults in greyual:

   ...  This is the function that most programs should call to process a file name
supplied by the user. ...

; however:

(documentation 'fs:merge-pathname-defaults)
"If I were you I wouldn't use this function:
  Try MERGE-PATHNAMES and FS:MERGE-PATHNAME-COMPONENTS instead.

Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
the type and version components, iff a name was specified
and FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is NIL.
Otherwise, the type and version are obtained from DEFAULTS,
and DEFAULT-TYPE and DEFAULT-VERSION are not used.
If ALWAYS-MERGE-TYPE is non-NIL, that forces the type component
to be merged like the name, directory, etc. but has no effect on the version."

robert


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 6 May 1986, 18:42-EDT
From: Bob Krajewski <rpk>
Message-ID: <8605062242.AA20058@lmi-angel.ARPA>
To: ayesha!debbie
CC: BUG-LISPM
In-reply-to: The message of 6 May 1986 14:36-EDT from Debbie Ellerin <NIL>

   Date: Tuesday, 6 May 1986, 14:36-EDT
   From: Debbie Ellerin <ayesha!debbie@ayesha.ARPA>

   In System 110.232, Lambda-Diag 7.17, Experimental Local-File 68.7,
   FILE-Server 18.4, Unix-Interface 9.1, ZMail 65.14, Object Lisp 3.4,
   Tape 6.39, Site Data Editor 3.3, Tiger 24.0, KERMIT 31.3,
   Window-Maker 1.1, Gateway 4.8, TCP-Kernel 39.7, TCP-User 62.7,
   TCP-Server 45.5, MEDIUM-RESOLUTION-COLOR 3.4,
   MICRO-COMPILATION-TOOLS 3.2, System Revision Level 3.10, microcode 1511,
   SDU Boot Tape 3.12, SDU ROM 8, on Thing (LAMBDA):

   In zmail, I moved into the bottom pane to edit a mail message,
   and then I tried to mouse on a line in the summary pane, and I got
   this error.
   Also, I was unable to choose anything from the command pane.

Sigh.  Looks like it will have to ignore commands like this in edit mode...

   >>ERROR: ZMACS error: ZWEI::SUMMARY-MOUSE is not a valid special editor command
   Backtrace from the debugger:

   ZWEI::UNKNOWN-SPECIAL-COMMAND (P.C. = 24)

    Arg 0 (TYPE): ZWEI::SUMMARY-MOUSE
    Rest arg (REST): ((ZWEI::SUMMARY-MOUSE #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 42540573> :INTERVAL ...)) #<ZWEI::ZMAIL-SUMMARY-SCROLL-WINDOW Zmail Summary Scroll Window 1 26345346 exposed> 1048576)


   (:METHOD ZWEI:WINDOW :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
     (SELF is #<ZWEI::ZMAIL-WINDOW Zmail Window 1 26321451 exposed>)

    Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
    Rest arg (ARGS): (ZWEI::SUMMARY-MOUSE (ZWEI::SUMMARY-MOUSE #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 42540573> :INTERVAL ...)) #<ZWEI::ZMAIL-SUMMARY-SCROLL-WINDOW Zmail Summary Scroll Window 1 26345346 exposed> 1048576)


0,, valid, 2,
*** EOOH ***
Message-ID: <8605052201.AA00228@ayesha.ARPA>
Date: Monday, 5 May 1986, 20:14-EDT
From: ayesha!keith@ayesha.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV,site 110.197,
on Cousin It (LAMBDA):

from rpk, re this report: The problem with the pathname instance
variable is that the hardcopy command is always assuming that
ZWEI:*INTERVAL* is a ZMacs buffer, which it ain't.

Insert your description of the circumstances here:

Attempted M-shift-P to print a zmail message buffer...

>>TRAP 5770 (INSTANCE-LACKS-INSTANCE-VARIABLE M-C M-A)
There is no instance variable PATHNAME in #<ZWEI::NODE 22131352>.
Backtrace from the debugger:

ZWEI::PRINT-BUFFER-1 (P.C. = 105)

 Arg 0 (INTERVAL): #<ZWEI::NODE 22131352>
   --Defaulted args:--
 Arg 1 (*STANDARD-OUTPUT*): #<DTP-SELECT-METHOD 24140134>
 Local 0 (FONTS): NIL
 Local 1 (STREAM): #<ZWEI:INTERVAL-STREAM 40173265>
 Local 2 (LEN): NIL
 Local 3 (NEW): NIL


ZWEI::COM-QUICK-PRINT-BUFFER (P.C. = 31)



Additional information supplied with call:
 Expecting 3 values

ZWEI::COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI::COM-QUICK-PRINT-BUFFER
 Arg 1 (CHAR): #/m-/p
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
 Local 0 (HOOK-SUCCESS): T
 Local 1: NIL
 Local 2 (HOOK): NIL


ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)

 Arg 0 (CH): #/m-/p
 Local 0 (VALUE): NIL
 Local 1 (LINE): NIL
 Local 2 (INDEX): NIL
 Local 3: NIL
 Local 4 (HOOK): NIL


(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
  (SELF is #<ZWEI::ZMAIL-WINDOW Zmail Window 1 26446576 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-COMMAND-CHAR
 Arg 1 (CH): #/m-/p


Remainder of stack:

(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) 0) (P.C. = 58)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) (P.C. = 39)
(:METHOD ZWEI::ZMAIL-WINDOW :AROUND :EDIT) (P.C. = 59)
(:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) (P.C. = 37)
ZWEI::COM-EDIT-CURRENT-MSG (P.C. = 162)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MOUSE-BUTTON) (P.C. = 59)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, valid, 2,
*** EOOH ***
Message-ID: <8605052235.AA04144@lmi-angel.ARPA>
Date: Monday, 5 May 1986, 18:31-EDT
From: rjpi@ANGEL
Sender: pld@LMI-ALADDIN
Subject: SI:DEF-A-OR-AN won't work?
To: BUG-LISPM@LMI-Angel

In System 110.232, Lambda-Diag 7.17, Experimental Local-File 68.7,
FILE-Server 18.4, Unix-Interface 9.1, ZMail 65.14, Object Lisp 3.4,
Tape 6.39, Site Data Editor 3.3, Tiger 24.0, KERMIT 31.3,
Window-Maker 1.1, Gateway 4.8, TCP-Kernel 39.7, TCP-User 62.7,
TCP-Server 45.5, MEDIUM-RESOLUTION-COLOR 3.4,
MICRO-COMPILATION-TOOLS 3.2, System Revision Level 3.12, microcode 1508,
SDU Boot Tape 3.12, SDU ROM 8, communications group,
on Nyarlathotep (LAMBDA):


Insert your description of the circumstances here:



(si:def-a-or-an :a hom :an homb)

>>TRAP 12192 (WRITE-IN-READ-ONLY VMA)
 There was an attempt to write into #o1567161, which is a read-only address.
 #o1567161 is in area #34, SYSTEM:INIT-LIST-AREA
Backtrace from the debugger:

SI::HACK-A-OR-AN-TABLE (P.C. = 111)

 Arg 0 (ARG): "HOM"
 Arg 1 (INSERTP): NIL
 Arg 2 (UNDO-LIST): NIL
   --Defaulted args:--
 Arg 3 (PATTERN): ("" ("A" "E" # "F" ...))
 Arg 4 (A-OR-AN): :A
 Arg 5 (NA-RO-A): :AN
 Local 0 (HOLDER): (NIL "A" "E" ("EU") ...)
 Local 1 (PATPTR): (("FA" "FE" "FI" "FJ" ...) "H" ("HA" # "HE" # ...) "I" ...)
 Local 2 (COMPARE): -2
 Local 3: NIL


SYSTEM:EVAL1 (P.C. = 419)

 Arg 0 (FORM): (SI::HACK-A-OR-AN-TABLE (QUOTE HOM) NIL NIL)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER SI::HACK-A-OR-AN-TABLE 12432711>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER SI::HACK-A-OR-AN-TABLE 12432711>
 Local 5 (ARG-DESC): 131270
 Local 6 (NUM-ARGS): 3
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ARG): NIL


SYSTEM:EVAL1 (P.C. = 416)

 Arg 0 (FORM): (SI::HACK-A-OR-AN-TABLE (QUOTE HOMB) T (SI::HACK-A-OR-AN-TABLE # NIL NIL))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER SI::HACK-A-OR-AN-TABLE 12432711>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER SI::HACK-A-OR-AN-TABLE 12432711>
 Local 5 (ARG-DESC): 131270
 Local 6 (NUM-ARGS): 3
 Local 7: NIL
 Local 8: ((SI::HACK-A-OR-AN-TABLE # NIL NIL))
 Local 9 (ARGL): NIL
 Local 10 (ARG): (SI::HACK-A-OR-AN-TABLE (QUOTE HOM) NIL NIL)


SYSTEM:EVAL1 (P.C. = 416)

 Arg 0 (FORM): (CONS (QUOTE SI:DEF-A-OR-AN) (SI::HACK-A-OR-AN-TABLE # T #))
 Arg 1 (NOHOOK): T
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-U-ENTRY CONS 443>
 Local 4 (CALL-FUNCTION): #<DTP-U-ENTRY CONS 443>
 Local 5 (ARG-DESC): 130
 Local 6 (NUM-ARGS): 2
 Local 7: NIL
 Local 8: ((SI::HACK-A-OR-AN-TABLE # T #))
 Local 9 (ARGL): NIL
 Local 10 (ARG): (SI::HACK-A-OR-AN-TABLE (QUOTE HOMB) T (SI::HACK-A-OR-AN-TABLE # NIL NIL))


SYSTEM:EVAL1 (P.C. = 276)

 Arg 0 (FORM): (SI::DISPLACED (SI::XR-BQ-CONS # #) (CONS # #))
 Arg 1 (NOHOOK): T
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): (MACRO . #<DTP-FEF-POINTER SI::XR-BQ-CONS 44542225>)
 Local 4 (CALL-FUNCTION): (MACRO . #<DTP-FEF-POINTER SI::XR-BQ-CONS 44542225>)
 Local 5 (ARG-DESC): 262207
 Local 6 (NUM-ARGS): NIL
 Local 7: ((CONS # #) T)
 Local 8: ((#) NIL (SI::HACK-A-OR-AN-TABLE # T #) NIL ...)
 Local 9 (ARGL): NIL
 Local 10 (ARG): NIL


Remainder of stack:

SYSTEM:EVAL1 (P.C. = 276)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::DRIBBLE-START (P.C. = 77)
DRIBBLE (P.C. = 22)
SYSTEM:EVAL1 (P.C. = 419)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, valid, critical,
*** EOOH ***
Message-ID: <8604111850.AA03282@Ayesha.ARPA>
Date: Sunday, 4 May 1986, 02:01-EDT
From: Robert Putnam <Ayesha!robert@ayesha.ARPA>
To: bug-lispm@ayesha.ARPA

Special forms in 3.0

C-Sh-D no longer works for special forms in the editor.  Also, M-. with
the cursor next to a special form doesn't default to that form.

Does lots of LMI code assume that special forms are fboundp?  If so, we
might inadvertantly be breaking lots of formerly "stable" things (like
the above, not to mention user code).


robert



0,, 3, valid,
*** EOOH ***
Date: Friday, 2 May 1986, 20:19-EDT
From: Dave Goodine <dg@LMI-ANGEL>
To: ayesha!debbie@LMI-ANGEL
CC: bug-lispm@LMI-ANGEL
In-reply-to: <8605021827.AA02813@ayesha.ARPA>
Message-ID: <[LMI-MAURICE-RAVEL].2-May-86 20:19:04.dg>

    Date: Friday, 2 May 1986, 14:27-EDT
    From: Debbie Ellerin <ayesha!debbie>
    Message-ID: <8605021827.AA02813@ayesha.ARPA>
    To: dg

    To: BUG-LISPM@AYESHA
    --Text Follows This Line--
    In Experimental System 110.197, Experimental Lambda-Diag 7.5,
    Experimental Local-File 68.7, Experimental FILE-Server 18.3,
    Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
    Experimental Object Lisp 3.1, Experimental Tape 6.38,
    Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
    Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
    Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
    Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
    Experimental MEDIUM-RESOLUTION-COLOR 3.4,
    Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
    SDU Boot Tape 3.12, SDU ROM 8, Alpha IV,site, on Thing (LAMBDA):

    In a ztop buffer, I tried to evaluate an expression which was supposed
    to print out some text. It printed out the numbers but no letters.
    To see this, try (print-herald)  -  it just prints the version numbers,
    or try (setq bar "does this work") -> it returns "" .

    debbie

you should send all bug reports to angel!bug-lispm.
Ztop is really broken. this may be a minor fix, but I think
we should discourage customers from using Ztop mode.

-dg


0,, cosmetic, valid,
*** EOOH ***
Message-ID: <8604292304.AA01445@LMI-CAPRICORN.ARPA>
Date: Tuesday, 29 April 1986, 18:59-EDT
From: sam@angel
Subject: <CTRL>-<HELP> is lost.
To: BUG-LISPM@LMI-Angel

In Experimental System 110.220, Experimental Lambda-Diag 7.11,
Experimental Local-File 68.7, Experimental FILE-Server 18.4,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.3, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.8, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481, SDU ROM 8,
3And^.220+u1481, on Emma Willard (LAMBDA):

<CTRL>-<HELP> no longer gets you a display of
"a list of commands for editing input."
It instead responds as to the <HELP> key unadorned.
That response claims that <CTRL>-<HELP> should work as described.



0,, 3, valid,
*** EOOH ***
Message-ID: <8604291645.AA01190@LMI-CAPRICORN.ARPA>
Date: Tuesday, 29 April 1986, 12:44-EDT
From: sam@angel
Subject: Zwei fails on special-case search for directory hogs.
To: BUG-LISPM@LMI-Angel

In Experimental System 110.220, Experimental Lambda-Diag 7.11,
Experimental Local-File 68.7, Experimental FILE-Server 18.4,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.3, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.8, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481, SDU ROM 8,
3And^.220+u1481, on Emma Willard (LAMBDA):


Insert your description of the circumstances here:

With cursor beyond last directory entry, typing N for "next hog" throws you into
the error handler.  If it is not intended to work backwards, it probably should
just gracefully say the usual:  <beep> No More Hogs .

P.S.  I know that Zwei is riddled with more bugs than one could possibly want to
report (or read a bug report about), but here's a tiny example anyway.


>>TRAP 6864 (ARGTYP ARRAY M-ARRAY-POINTER 0 (GAHDR RESTORE-ARRAY-REGISTERS) GAHDR) ->  AP-LEADER
The first argument to EH::GAHDR, NIL, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

ZWEI::COM-DIRED-NEXT-HOG (P.C. = 56)

 Local 0 (HOG): 1
 Local 1 (LINE): NIL
 Local 2 (PATHNAME): NIL
 Local 3 (LINE): NIL
 Local 4 (STOP-LINE): NIL
 Local 5 (NAME): NIL
 Local 6 (TYPE): NIL
 Local 7 (SKIP-P): NIL
 Local 8 (FIRST-LINE): NIL
 Local 9 (N-VERSIONS): NIL


Additional information supplied with call:
 Expecting 3 values

ZWEI::COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI::COM-DIRED-NEXT-HOG
 Arg 1 (CHAR): #/n
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
 Local 0 (HOOK-SUCCESS): T
 Local 1: NIL
 Local 2 (HOOK): NIL


ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)

 Arg 0 (CH): #/n
 Local 0 (VALUE): NIL
 Local 1 (LINE): NIL
 Local 2 (INDEX): NIL
 Local 3: NIL
 Local 4 (HOOK): NIL


(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
  (SELF is #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 1 26437121 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-COMMAND-CHAR
 Arg 1 (CH): #/n


(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
  (SELF is #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 1 26437121 exposed>)

 Arg 0 (.OPERATION.): :EDIT
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
 Arg 2 (*COMTAB*): #<ZWEI::COMTAB ZWEI::MODE-COMTAB 64146470>
 Arg 3 (*MODE-LINE-LIST*): ("ZMACS " "(" ZWEI::*MODE-NAME-LIST* ") " ...)
 Arg 4 (TOP-LEVEL-P): T
 Local 0: ("Return to top level editor command loop.")
 Local 1: ((ABORT ERROR) ("Return to top level editor command loop.") T ("Return to top level editor command loop.") ...)
 Local 2 (CH): #/n


Remainder of stack:

(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Date: Friday, 25 April 1986, 16:09-EST
From: Debbie Ellerin <ayesha!debbie@angel>
Message-ID: <8604252109.AA03643@ayesha.ARPA>
To: bug-lispm@angel

 To: BUG-LISPM@AYESHA
--Text Follows This Line--
In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV, on Thing (LAMBDA):


Insert your description of the circumstances here:

In the hostat window, after breaking into a read-eval-print loop,
I typed ctrl-shift-d on a function. It printed the documentation
string, but then got this error while trying to reprint
the input.

(the same thing happens when I hit the help key in that window.)

debbie.

>>ERROR: Cannot convert #<ART-Q 512 leader-length 7 57507443> into a string.
Backtrace from the debugger:

STRING-LENGTH (P.C. = 60)

 Arg 0 (STRING): #<ART-Q 512 leader-length 7 57507443>
 Local 0 (STRING): NIL


(:METHOD TV:LINE-TRUNCATING-MIXIN :AROUND :STRING-OUT) (P.C. = 56)
  (SELF is #<TV:TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET Truncating Pop Up Text Window With Reset 1 30670650 exposed>)

 Arg 0 (.OPERATION.): :STRING-OUT
 Arg 1 (.CONTINUATION.): #<DTP-FEF-POINTER (:INTERNAL (:METHOD TV:TRUNCATING-POP-UP-TEXT-WINDOW :COMBINED :STRING-OUT) 0) 13157044>
 Arg 2 (.MAPPING-TABLE.): #<ART-16B 5 leader-length 10 25527742>
 Arg 3 (.AROUND-ARGS.): (:STRING-OUT #<ART-Q 512 leader-length 7 57507443>)
 Arg 4 (STRING): #<ART-Q 512 leader-length 7 57507443>
   --Defaulted args:--
 Arg 5 (START): 0
 Arg 6 (END): NIL
 Local 0 (I): NIL
 Local 1 (CR-IDX): NIL


(:METHOD TV:TRUNCATING-POP-UP-TEXT-WINDOW :COMBINED :STRING-OUT) (P.C. = 39)
  (SELF is #<TV:TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET Truncating Pop Up Text Window With Reset 1 30670650 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:STRING-OUT #<ART-Q 512 leader-length 7 57507443>)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 5 leader-length 10 25527742>


TV::RH-REPRINT-INPUT (P.C. = 78)

 Arg 0 (CHAR): 134
   --Defaulted args:--
 Arg 1 (DONT-SET-PROMPT-CURSORPOS): NIL
 Local 0 (PROMPT): NIL


TV::RH-DISPLAY-INFO-INTERNAL (P.C. = 54)

 Arg 0 (PRINTER): #<CLOSURE (:INTERNAL TV::RH-COM-DOCUMENTATION 0) (Lexical environment) 40220040>


Remainder of stack:

TV::RH-COM-DOCUMENTATION (P.C. = 34)
TV::ALTERNATE-RUBOUT-HANDLER (P.C. = 534)
(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 113)
(:METHOD TV:STREAM-MIXIN :TYI) (P.C. = 27)
SI::XR-XRTYI (P.C. = 67)
SI::XR-READ-THING (P.C. = 77)
SI::INTERNAL-READ (P.C. = 168)
SI::XR-BACKQUOTE-MACRO (P.C. = 44)
SI::INVOKE-READER-MACRO (P.C. = 29)
SI::XR-READ-LIST (P.C. = 205)
...
PROCESS-WAIT-WITH-TIMEOUT (P.C. = 34)
CHAOS:POLL-HOSTS (P.C. = 294)
HOSTAT (P.C. = 42)
TV::KBD-HOSTAT (P.C. = 95)
SYSTEM:EVAL1 (P.C. = 419)
EVAL (P.C. = 96)
(:INTERNAL TV::KBD-ESC 0) (P.C. = 24)
SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 64)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, cosmetic, valid,
*** EOOH ***
Date: Tuesday, 22 April 1986, 14:26-EST
From: Debbie Ellerin <ayesha!debbie@angel>
Message-ID: <8604221926.AA01790@ayesha.ARPA>
To: bug-lispm@angel

All of the arguments to Terminal-Q should be documented in
tv:*escape-keys* so that the user can get to them through
Terminal-Help.



0,, 3, valid,
*** EOOH ***
Message-ID: <8604181902.AA00634@LMI-CAPRICORN.ARPA>
Date: Friday, 18 April 1986, 12:55-EST
From: sam@angel
Subject: ZWEI:COM-DIRED-VIEW-FILE fails on large-font files.
To: BUG-LISPM@LMI-Angel

In Experimental System 110.201, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.4,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.7, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1408, SDU ROM 8,
III Andvr 041586, on McGuffey (LAMBDA):


Insert your description of the circumstances here:



In DIRED, upon trying to view a file having the following mode line,

;;; -*- Mode:LISP; Package:ICON; Fonts:(BIGFNT) -*-



>>TRAP 11261 (TV-ERASE-OFF-SCREEN)
An attempt was made to do graphics past the end of the screen.
Backtrace from the debugger:

TV:SHEET-LINE-OUT (P.C. = 136)

 Arg 0 (SHEET): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Arg 1 (STRING): "--More--"
 Arg 2 (START): 0
 Arg 3 (STOP): 8
 Arg 4 (SET-XPOS): 0
 Arg 5 (SET-YPOS): 682
   --Defaulted args:--
 Arg 6 (DWIDTH): NIL
 Local 0 (INSIDE-RIGHT): 1020
 Local 1 (INSIDE-LEFT): 3
 Local 2 (MARGIN-FLAG): T
 Local 3 (XPOS): 3
 Local 4 (YPOS): 684
 Local 5 (STOP-INDEX): NIL
 Local 6 (STOP-XPOS): NIL


Additional information supplied with call:
 Expecting 2 values

ZWEI::VIEW-WINDOW-DISPLAY (P.C. = 163)

 Arg 0 (WINDOW): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Arg 1 (STREAM): #<FS::LM-CHARACTER-INPUT-STREAM "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1" 42533634>
 Arg 2 (FORCE-P): T
 Arg 3 (FONTS-P): NIL
 Local 0 (AT-END-P): NIL
 Local 1 (N-PLINES): 31
 Local 2 (SHEET): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Local 3 (LAST-BP): ("" 0 :MOVES)
 Local 4 (PLINE): 0
 Local 5 (X): NIL
 Local 6 (Y): 682
 Local 7 (Y-POS): NIL
 Local 8 (ISTREAM): #<INTERVAL-STREAM 42534325>
 Local 9 (I): 31
 Local 10 (LINE): "    ':panes"
 Local 11 (EOF): NIL
 Local 12 (BLINKER): NIL


ZWEI::VIEW-WINDOW (P.C. = 165)

 Arg 0 (WINDOW): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Arg 1 (STREAM): #<FS::LM-CHARACTER-INPUT-STREAM "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1" 42533634>
   --Defaulted args:--
 Arg 2 (RETURN-IF-NO-MORE): NIL
 Local 0 (CH): NIL
 Local 1 (ATTRIBUTE-LIST): (:MODE :LISP :PACKAGE :ICON ...)
 Local 2 (FONTSP): NIL
 Local 3 (N-LINES): 30
 Local 4 (FIRST-P): T
 Local 5 (AT-END-P): NIL
 Local 6: NIL
 Local 7: NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

ZWEI::VIEW-STREAM (P.C. = 132)

 Arg 0 (STREAM): #<FS::LM-CHARACTER-INPUT-STREAM "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1" 42533634>
   --Defaulted args:--
 Arg 1 (WINDOW): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Local 0 (INTERVAL): #<ZWEI::NODE 42534022>
 Local 1 (NEW-WINDOW): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Local 2 (.WINDOW.): #<ZWEI::ZWEI-OVERLYING-WINDOW Zwei Overlying Window 1 26537771 exposed>
 Local 3 (.FOR-WINDOW.): #<ZMACS-FRAME Zmacs Frame 1 25740035 exposed>
 Local 4 (.OSTATUS.): :DEACTIVATED
 Local 5 (.OSUBST.): #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 1 26437121 deexposed>
 Local 6 (.QUEUE-LEFT.): NIL
 Local 7: NIL
 Local 8 (E): NIL


ZWEI::VIEW-FILE (P.C. = 64)

 Arg 0 (PATHNAME): #FS::LM-PATHNAME "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1"
 Arg 1 (DELETED-P): NIL
 Local 0: #<FS::LM-CHARACTER-INPUT-STREAM "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1" 42533634>
 Local 1 (.FILE-ABORTED-FLAG.): :ABORT
 Local 2 (STREAM): #<FS::LM-CHARACTER-INPUT-STREAM "GUFF: LISP2-CIRCULATION; TURTLE-HACKS.LISP#1" 42533634>


Remainder of stack:

ZWEI::COM-DIRED-VIEW-FILE (P.C. = 88)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, valid, 4,
*** EOOH ***
Message-ID: <8604172324.AA00322@ayesha.ARPA>
Date: Thursday, 17 April 1986, 18:27-EST
From: ayesha!debbie@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In System 102.187, Local-File 56.16, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.16,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0,
Experimental IRIS 1.0, microcode 782, prol patch obj, on Gomez:


Insert your description of the circumstances here:

I tried a print-disk-label over the network,
(print-disk-label 'mike) didn't work, but
(print-disk-label "mike") does.

>>TRAP 9735 (TRANS-TRAP)
The function MIKE is undefined.
Backtrace from the debugger:

MIKE:
   Arg 0: :READ
   Arg 1: #<ART-16B-994 5443016>
   Arg 2: 0


SYSTEM:DISK-READ (P.C. = 67)

 Arg 0 (RQB): #<ART-16B-994 5443016>
 Arg 1 (UNIT): MIKE
 Arg 2 (ADDRESS): 0
   --Defaulted args:--
 Arg 3 (MICROCODE-ERROR-RECOVERY): T
 Arg 4 (DO-NOT-OFFSET): NIL


SI::READ-DISK-LABEL (P.C. = 44)

 Arg 0 (RQB): #<ART-16B-2016 5440016>
 Arg 1 (UNIT): MIKE
Local 0 (RQB1): #<ART-16B-994 5443016>
Local 1 (BFR): NIL


PRINT-DISK-LABEL (P.C. = 55)

 Arg 0 (UNIT): MIKE
   --Defaulted args:--
 Arg 1 (STREAM): #:*TERMINAL-IO*-SYN-STREAM
Local 0 (RQB): #<ART-16B-2016 5440016>


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (PRINT-DISK-LABEL (QUOTE MIKE))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER PRINT-DISK-LABEL 23336042>
Local 6 (ARG-DESC): 131074
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


Remainder of stack:

SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
APPLY (P.C. = 24)
SYSTEM:EVAL1 (P.C. = 547)
COND (P.C. = 58)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
...
APPLY (P.C. = 24)
SYSTEM:EVAL1 (P.C. = 547)
LET (P.C. = 274)
SYSTEM:EVAL1 (P.C. = 547)
SYSTEM:APPLY-LAMBDA (P.C. = 1307)
SI:LISP-TOP-LEVEL1
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)




0,, 3, valid,
*** EOOH ***
Date: Thursday, 17 April 1986, 16:53-EST
From: ayesha!debbi@AYESHA.ARPA
Sender: ayesha!debbie@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[THING].17-Apr-86 16:53:37.debbie>

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV, on Thing (LAMBDA):

There may be bugs associated with setting the font for
instances of tv:command-menu and for other flavors based on it.

   o  If each element of :item-list has a font specified then everything
      works fine.

   o  Define a flavor which inherits from tv:command-menu, and give
      it :font-map (fonts:bigfnt).
      ((and the elements of :item-list do not  specify any font.))

     --  Then instantiate that flavor and expose it. You will see that
      the strings printed for some of the elements of the :item-list
      have been truncated to fit in the too small menu.

     --  If this flavor is used as a pane in a constraint frame, then the
      strings are printed correctly, but the mouse sensitivity boxes
      are drawn too small for the string.





0,, cosmetic, valid,
*** EOOH ***
Date: Thursday, 17 April 1986, 16:52-EST
From: ayesha!debbi@AYESHA.ARPA
Sender: ayesha!debbie@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[THING].17-Apr-86 16:52:39.debbie>

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV, on Thing (LAMBDA):


In the editor-typeout-window
(send terminal-io :set-font-map (fonts:bigfnt fonts:cptfont))
doesn't erase the previous line being drawn at the bottom of the
editor typeout window before it proceeds.





0,, 3, valid,
*** EOOH ***
Message-ID: <8604171926.AA02276@ayesha.ARPA>
Date: Thursday, 17 April 1986, 14:30-EST
From: ayesha!debbie@AYESHA.ARPA
Sender: ayesha!softserv@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV, on Thing (LAMBDA):


I was using window-maker, and tried to split the screen into two panes
(evenly) . One was tv:window and the other was tv:command-pane.
I got this error after providing the information for these two panes.

>>ERROR: ETYPECASE failure; the value of (CAR SI::S-TAIL), #<DTP-FEF-POINTER (:INTERNAL WM::GROUP-THE-ITEMS-BY-SUBGROUP 0) 12516173>,
         is not a vector, a cons or NIL.
Backtrace from the debugger:

LISP:EVERY (P.C. = 62)

 Arg 0 (PREDICATE): (:EVEN :EVEN)
 Rest arg (SEQUENCES): (#<DTP-FEF-POINTER (:INTERNAL WM::GROUP-THE-ITEMS-BY-SUBGROUP 0) 12516173>)
 Local 1 (INDEX): 0
 Local 2 (S-TAIL): (#<DTP-FEF-POINTER (:INTERNAL WM::GROUP-THE-ITEMS-BY-SUBGROUP 0) 12516173>)


WM::GROUP-THE-ITEMS-BY-SUBGROUP (P.C. = 43)

 Arg 0 (LIST-OF-ITEMS): (#<WM::PANE 54027104> #<WM::PANE 54027121>)
 Local 0 (ITEM): NIL
 Local 1: (#<WM::PANE 54027104> #<WM::PANE 54027121>)
 Local 2 (LIST-OF-SUBGROUP): NIL
 Local 3: (:EVEN :EVEN)
 Local 4: (:EVEN)
 Local 5: NIL
 Local 6 (X): #<WM::PANE 54027121>
 Local 7 (FLAG): NIL
 Local 8 (LAST-ITEM): NIL
 Local 9 (SUBGROUP-OF-PANES): NIL


WM::GENERATE-CONSTRAINT-LIST-FOR-FRAME (P.C. = 122)

   --Defaulted args:--
 Arg 0 (FRAME): #<WM::FRAME 54025222>
 Local 0 (LIST-OF-PANES-OR-FRAMES-UNSORTED): (#<WM::PANE 54027104> #<WM::PANE 54027121>)
 Local 1 (DIRECTION-OF-SLICE): :VERTICAL
 Local 2 (DIRECTION-OF-STACKING): :HORIZONTAL
 Local 3 (SORTED-LIST-BY-STACKING): (#<WM::PANE 54027104> #<WM::PANE 54027121>)
 Local 4 (SORTED-LIST-BY-KEYWORD): (#<WM::PANE 54027104> #<WM::PANE 54027121>)
 Local 5 (OWNER): NIL
 Local 6 (OLD-STACKING-DIRECTION): NIL
 Local 7 (DIRECTION-OF-OWNER-STACKING): :VERTICAL
 Local 8 (LIST-OF-SUBGROUP): NIL
 Local 9 (CONSTRAINTS): NIL
 Local 10 (SUBGROUP): NIL
 Local 11: NIL
 Local 12 (LIST-OF-NAMES): NIL
 Local 13 (ITEM): NIL
 Local 14: NIL
 Local 15 (OPERATION): NIL
 Local 16 (CONSTRAINT): NIL


WM::GENERATE-CODE-TO-USE (P.C. = 115)

 Local 0 (LIST-OF-PANES): NIL
 Local 1 (CONSTRAINTS): NIL
 Local 2 (NAME): NIL
 Local 3: NIL
 Local 4 (STREAM): NIL
 Local 5 (OUTPUT-STRING): NIL
 Local 6 (INDEX): NIL
 Local 7 (MAX-INDEX): NIL
 Local 8 (EXPRESSION-TO-OUTPUT): NIL


SYSTEM:EVAL1 (P.C. = 419)

 Arg 0 (FORM): (WM::GENERATE-CODE-TO-USE)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL NIL NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER WM::GENERATE-CODE-TO-USE 12405377>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER WM::GENERATE-CODE-TO-USE 12405377>
 Local 5 (ARG-DESC): 0
 Local 6 (NUM-ARGS): 0
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ARG): NIL


Remainder of stack:

EVAL (P.C. = 96)
WM::WINDOW-EDITOR (P.C. = 102)
WM::PROCESS-FUNCTION (P.C. = 188)
SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 64)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, valid, 4,
*** EOOH ***
Date: Wednesday, 16 April 1986, 16:26-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: +
To: pecann@angel
CC: bug-lispm@angel
In-reply-to: <8604112028.AA04179@LMI-CAPRICORN.ARPA>
Message-ID: <[LMI-LAMB-CHOP].16-Apr-86 16:26:39.RpK>

    No matter what position you place a non-numeric arg in,
    you get a complaint about the second arg.

This is a consequence of the way + is implemented in the interpreter.





0,, cosmetic, valid,
*** EOOH ***
Date: Wednesday, 16 April 1986, 08:53-EST
From: Debbie Ellerin <ayesha!debbie@ayesha.ARPA>
Subject: zmail configure option
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[COUSIN-IT].16-Apr-86 08:53:40.debbie>

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV,site, on Cousin It (LAMBDA):

In zmail, mousing right on configure to change the window configuration
gives 4 options, message only, summary only, both and experimental.

1. The summary only configuration does not include the command pane.
   The message only configuration does, and thus it is a lot easier to use.
   I think the command pane should be included in all configurations.

2. Why is the "experimental" configuration  experimental?
   Will this experimental option be in the released band?

debbie



0,, cosmetic, valid,
*** EOOH ***
Date: Wednesday, 16 April 1986, 06:45-EST
From: Debbie Ellerin <ayesha!debbie@ayesha.ARPA>
Subject: zmail configure option
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[COUSIN-IT].16-Apr-86 06:45:17.debbie>

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV,site, on Cousin It (LAMBDA):

In zmail, mousing right on configure to change the window configuration
gives 4 options, message only, summary only, both and experimental.

1. The summary only configuration does not include the command pane.
   The message only configuration does, and thus it is a lot easier to use.
   I think the command pane should be included in all configurations.

2. Why is the "experimental" configuration  experimental?
   Will this experimental option be in the released band?

debbie



0,, cosmetic, valid,
*** EOOH ***
Date: Wednesday, 16 April 1986, 05:47-EST
From: ayesha!debbie@ayesha.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[COUSIN-IT].16-Apr-86 05:47:24.debbie>

In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.12, SDU ROM 8, Alpha IV,site, on Cousin It (LAMBDA):

In zmacs, the documentation for C-X 3  says "Show a second window
but leave the first one selected."
However, if I already have two windows shown, this does not leave the
first one selected; instead, it selects the second one.



0,, 3, valid,
*** EOOH ***
Date: Monday, 14 April 1986, 16:15-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!softserv@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[THING].14-Apr-86 16:15:45.softserv>


In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.8, SDU ROM 8, Alpha IV, on Thing (LAMBDA):


Insert your description of the circumstances here:

I tried to bring up gateway from the editor by typing,
Ctrl-x g.

>>TRAP 9805 (TRANS-TRAP)
The function NIL is undefined.
Backtrace from the debugger:

NIL:
   Arg 0: :SET-ITEM-LIST
   Arg 1: (("" :NO-SELECT NIL) ("LMI-GATEWAY" :VALUE # :FONT ...))


ZWEI::SET-UP-GATEWAY (P.C. = 32)



ZWEI::COM-GATE (P.C. = 21)



ZWEI::COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI::COM-GATE
 Arg 1 (CHAR): #/g
 Arg 2 (PREFIX-CHAR): #/c-X
   --Defaulted args:--
 Arg 3 (HOOK-LIST): NIL
 Local 0 (HOOK-SUCCESS): T
 Local 1: NIL
 Local 2 (HOOK): NIL


ZWEI::MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)

 Local 0 (PREFIX-CHAR): #/c-X
 Local 1 (CHAR): 103


Remainder of stack:

ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, cosmetic, valid,
*** EOOH ***
Date: Monday, 14 April 1986, 16:10-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!softserv@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[THING].14-Apr-86 16:10:55.softserv>

To: BUG-LISPM@AYESHA
--Text Follows This Line--
In Experimental System 110.197, Experimental Lambda-Diag 7.5,
Experimental Local-File 68.7, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.14,
Experimental Object Lisp 3.1, Experimental Tape 6.38,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.3, Experimental Window-Maker 1.1,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.7,
Experimental TCP-User 62.7, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.4,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1481,
SDU Boot Tape 3.8, SDU ROM 8, Alpha IV, on Thing (LAMBDA):

The default mouse documentation string in the editor
comes up as "Click left on node name to display that node".





0,, cosmetic, valid, issues,
*** EOOH ***
Date: Monday, 14 April 1986, 16:08-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!softserv@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[THING].14-Apr-86 16:08:49.softserv>

To: BUG-LISPM@AYESHA
--Text Follows This Line--
In Experimental System 110.169, Experimental Lambda-Diag 7.3,
Experimental Local-File 68.5, Experimental FILE-Server 18.3,
Experimental Unix-Interface 9.1, Experimental ZMail 65.11,
Experimental Object Lisp 3.1, Experimental Tape 6.33,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.6, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.6, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.1,
Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1408,
SDU Boot Tape 3.8, SDU ROM 8, Alpha III Andover, on Thing (LAMBDA):

In the tape software pane, I think "restore-files" should be
changed to "restore", or something else that would indicate
that this can be used to restore partitions as well as files.



0,, valid, 2,
*** EOOH ***
Date: Tuesday, 8 April 1986, 17:37-EST
From: Dave Goodine <dg@LMI-ANGEL>
To: BUG-LISPM@LMI-Angel
In-reply-to: <8604082102.AA05285@lmi-angel.ARPA>
Message-ID: <[LMI-MAURICE-RAVEL].8-Apr-86 17:37:02.dg>

    Message-ID: <8604082102.AA05285@lmi-angel.ARPA>
    Date: Tuesday, 8 April 1986, 15:55-EST
    From: Bob Powell <bobp@LMI-CAPRICORN>
    To: BUG-LISPM@LMI-Angel

    In Experimental System 110.180, Experimental ZMail 65.13,
    Experimental Unix-Interface 9.1, Experimental Local-File 66.1,
    Experimental MagTape 4.1, Experimental FILE-Server 18.3,
    Experimental IMicro 10.0, Experimental Lambda-Diag 7.3,
    Experimental Object Lisp 3.1, microcode 1408, SDU Boot Tape 3.9,
    SDU ROM 103, Nifty+, on Poindexter (LAMBDA):

    In the last few weeks, I have heard (and made) a few complaints
    that the system was suddenly taking much longer to do simple
    things like compile 10 lines of code.  The microsecond clock
    doesn't work on this machine, so it is obvious when the machine
    isn't taking interrupts ... the clock will lose 10 seconds at
    a time.  This didn't happen before; whatever the lambda is
    doing that it didn't use to do, is inhibiting the clock.

    Latest symptom is that removing one 4-meg board (reducing ram
    from 11MB to 7MB) has apparently "cured" the machine.  I
    sure didn't change anything else!

    - Bob

I have also noticed that reducing the memory seems to fix the problem.
I turned on the microsecond clock before I saved the Alpha III band,
but I've noticed that it doesn't seem to stay enabled between boots,
but haven't figured out for sure what the circumstances of lossage
are.

George is looking into a bug in the hash table code that the compiler
depends on.

This is something we must fix before we send the band out to MCC
on April 15th.

-dg



0,, 3, valid,
*** EOOH ***
Message-ID: <8604062254.AA02487@LMI-CAPRICORN.ARPA>
Date: Sunday, 6 April 1986, 17:53-EST
From: sam@Angel
Subject: LOAD may bomb on a physical pathname for which there is a logical host.
To: BUG-LISPM@LMI-Angel

In Experimental System 110.121, Experimental Lambda-Diag 7.0,
Experimental Local-File 68.1, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Object Lisp 3.0, Experimental Tape 6.0,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408, SDU ROM 8,
Alpha III Cambridge, on Emma Willard (LAMBDA):


Insert your description of the circumstances here:

The following fails.  Loading "zzz:string-processing.lisp#1" instead succeeds.

(fs:set-logical-pathname-host
  "zzz"
  :physical-host "emma"
  :translations '(("" "sam.cl;")))
#FS::LOGICAL-HOST "zzz"

(load "emma:sam.cl;string-processing.lisp#1")
>>TRAP 4440 (ARGTYP CONS M-T T CAR CAR)
The argument to CAR, T, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

FS::PATHNAME-TRANSLATE-WILD-COMPONENT (P.C. = 24)

 Arg 0 (TARGET-PATTERN): :WILD
 Arg 1 (DATA): ("SAM" "CL")
 Arg 2 (SPECS): T
 Arg 3 (WILD-ANY): #/*
 Arg 4 (WILD-ONE): -1
 Arg 5 (REVERSIBLE-P): T
 Local 0: NIL
 Local 1: NIL
 Local 2 (ELT): NIL
 Local 3: NIL
 Local 4 (NEW-ELT): NIL
 Local 5 (SPECS-LEFT): NIL


(:METHOD PATHNAME :TARGET-TRANSLATE-WILD-PATHNAME) (P.C. = 107)
  (SELF is #FS::LOGICAL-PATHNAME "zzz: *; * * *")

 Arg 0 (.OPERATION.): :TARGET-TRANSLATE-WILD-PATHNAME
 Arg 1 (SOURCE-PATTERN): #FS::LM-PATHNAME "PEEL: SAM.CL; *.*#*"
 Arg 2 (DATA-PATHNAME): #FS::LM-PATHNAME "PEEL: SAM.CL; STRING-PROCESSING.#"
 Arg 3 (REVERSIBLE-P): T
 Local 0 (W*): #/*
 Local 1 (W1): -1
 Local 2 (CASE-CONVERTER): #<DTP-FEF-POINTER (:INTERNAL (:METHOD PATHNAME :TRANSLATION-CASE-CONVERTER) 0) 61647271>
 Local 3 (DEV-SPECS): T
 Local 4 (DIR-SPECS): T
 Local 5 (NAME-SPECS): ("STRING-PROCESSING")
 Local 6 (TYPE-SPECS): (:UNSPECIFIC)


(:METHOD PATHNAME :TRANSLATE-WILD-PATHNAME) (P.C. = 25)
  (SELF is #FS::LM-PATHNAME "PEEL: SAM.CL; *.*#*")

 Arg 0 (.OPERATION.): :TRANSLATE-WILD-PATHNAME
 Arg 1 (TARGET-PATTERN): #FS::LOGICAL-PATHNAME "zzz: *; * * *"
 Arg 2 (DATA-PATHNAME): #FS::LM-PATHNAME "PEEL: SAM.CL; STRING-PROCESSING.#"
 Arg 3 (REVERSIBLE-P): T


(:METHOD FS::LOGICAL-PATHNAME :BACK-TRANSLATED-PATHNAME) (P.C. = 51)
  (SELF is #FS::LOGICAL-PATHNAME "zzz: ")

 Arg 0 (.OPERATION.): :BACK-TRANSLATED-PATHNAME
 Arg 1 (PATHNAME): #FS::LM-PATHNAME "PEEL: SAM.CL; STRING-PROCESSING.#"
 Local 0: ((#FS::LOGICAL-PATHNAME "zzz: *; * * *" #FS::LM-PATHNAME "PEEL: SAM.CL; *.*#*"))
 Local 1 (TRANS): (#FS::LOGICAL-PATHNAME "zzz: *; * * *" #FS::LM-PATHNAME "PEEL: SAM.CL; *.*#*")


(:METHOD PATHNAME :GENERIC-PATHNAME) (P.C. = 108)
  (SELF is #FS::LM-PATHNAME "PEEL: SAM.CL; STRING-PROCESSING.LISP#1")

 Arg 0 (.OPERATION.): :GENERIC-PATHNAME
 Local 0 (TYP): :LISP
 Local 1 (NEW-TYPE): :UNSPECIFIC
 Local 2 (DEV): "DSK"
 Local 3 (DEV1): NIL
 Local 4 (STAGE1): #FS::LM-PATHNAME "PEEL: SAM.CL; STRING-PROCESSING.#"
 Local 5: (#FS::LOGICAL-HOST "zzz" #FS::LOGICAL-HOST "GATEWAY" #FS::LOGICAL-HOST "SYS")
 Local 6 (H): #FS::LOGICAL-HOST "zzz"
 Local 7 (BTPN): NIL


Remainder of stack:

SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE (P.C. = 24)
(:INTERNAL FS::LOAD-1 FS::KLUDGE) (P.C. = 102)
FS::LOAD-1 (P.C. = 217)
LOAD (P.C. = 68)
SYSTEM:EVAL1 (P.C. = 419)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)
SI:LISP-TOP-LEVEL (P.C. = 39)



0,, valid, 2,
*** EOOH ***
Message-ID: <8604041456.AA00967@LMI-CAPRICORN.ARPA>
Date: Friday, 4 April 1986, 09:53-EST
From: Pace Willisson <pace@cap>
To: BUG-LISPM@LMI-Angel

In Experimental System 110.166, Experimental Lambda-Diag 7.3,
Experimental Local-File 68.5, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.1, Experimental ZMail 65.11,
Experimental Object Lisp 3.1, Experimental Tape 6.30,
Experimental Site Data Editor 3.3, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.5, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.6, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.1,
Experimental MICRO-COMPILATION-TOOLS 3.2, Experimental IMicro 10.0,
microcode 1408, SDU ROM 102, Alpha III Cambridge,
on Lene Lovich (LAMBDA):

I ran a function that spent a long time in a without-flipping form.
After a while, I got a (big) notification that the inhibit-flipping
count was not zero, but no one had bound the special variable to T.
I looked at the inhibit-flipping variable and it was 0.
I typed Super-C to "hope the problem goes away", and everything was OK.

This probably means there is a race condition between the guy that is
upset that inhibit-flipping has been non zero for a long time, and the
guy who tries to find out why.  In this case, it was set back to zero
by my funciton just after it was checked, so when the searcher ran,
he could find nothing.

We should at least make the search recheck the value of inhibit-flipping
before signalling the error, but it seems like a little more will be
required.

Pace



0,, valid, 2,
*** EOOH ***
Message-ID: <8603300036.AA00641@LMI-CAPRICORN.ARPA>
Date: Sunday, 30 March 1986, 21:04-EST
From: John Mann <jcm@angel>
To: bug-lispm@angel

Topic: the zmacs menu split-screen option.

If there are enough buffers around, the list of buffers-to-choose-from
takes up two columns, extending completely across the screen if the
names in it are long enough. Then there is no room beside the list to
display the map of how the screen is being split. Instead of putting
the map elsewhere, or forgoing it, the command crashes into the cold
load stream when any buffer is selected, apparently because it is trying
to display the map in a position that is occupied (or too small, or
nonexistent). Forcible unlocking lets a message come out, the screen
can be restored, but trying to abort from the too-wide list doesn't
work: it remains on display, though it is no longer mouse-sensitive.
x
x
x



0,, 3, valid,
*** EOOH ***
Date: Friday, 28 March 1986, 13:51-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Dump catching errors
To: dg@LMI-ANGEL
CC: BUG-LISPM@LMI-Angel
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: <8603272241.AA03440@LMI-CAPRICORN.ARPA>
Message-ID: <[LMI-DAVID-BOWIE].28-Mar-86 13:51:09.RpK>

    Date: Thursday, 27 March 1986, 17:42-EST
    From: dg@LMI-ANGEL

    In Experimental System 110.157, Experimental Lambda-Diag 7.3,
    Experimental Local-File 68.5, Experimental FILE-Server 18.2,
    Experimental Unix-Interface 9.1, Experimental ZMail 65.10,
    Experimental Object Lisp 3.0, Experimental Tape 6.14,
    Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
    Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
    Experimental Gateway 4.3, Experimental TCP-Kernel 39.5,
    Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
    Experimental MEDIUM-RESOLUTION-COLOR 3.1,
    Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1408, SDU ROM 8,
    Alpha III Cambridge, on Mary had a little Lambda (LAMBDA):

    File being dumped by backup software ... -dg

    >>ERROR: File is open for output

I suppose it's only reasonable for the dumper to catch
FS:FILE-OPERATION-FAILURE (with this happens to be), since these things
are bound to happen if somebody else is using the file system.



0,, 3, valid,
*** EOOH ***
Date: Monday, 24 March 1986, 18:12-EST
From: Debbie Ellerin <Ayesha!debbie@angel>
Message-ID: <8603242312.AA01218@Ayesha.ARPA>
To: bug-lispm@angel

To: BUG-LISPM@LURCH
--Text Follows This Line--
In Experimental System 110.121, Experimental Lambda-Diag 7.0,
Experimental Local-File 68.2, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Object Lisp 3.0, Experimental Tape 6.6,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408,
SDU Boot Tape 3.8, SDU ROM 8, Alpha III Andover,
on Cousin It (LAMBDA):


Insert your description of the circumstances here:
I think this is another lambda list bug.

(defun foo (a &rest args &key b c &allow-other-keys)
  (copy-list args))

(compile 'foo)
(foo 1 2)

>>ERROR: Odd number of keyword args: (2)
Backtrace from the debugger:

FOO (P.C. = 27) (from file MIKE: DEBBIE; LAMBDA-LIST.#)

 Arg 0 (A): 1
 Rest arg (ARGS): (2)
 Local 1 (B): NIL
 Local 2 (C): NIL


SYSTEM:EVAL1 (P.C. = 419)

 Arg 0 (FORM): (FOO 1 2)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER FOO 20616141>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER FOO 20616141>
 Local 5 (ARG-DESC): 1048641
 Local 6 (NUM-ARGS): 2
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ARG): 2


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (FOO 1 2)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)

 Arg 0 (TOP-LEVEL-FORM): (FOO 1 2)
 Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
 Local 1: ((# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 238)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 25400000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
 Local 0 (OLD-PACKAGE): #<Package USER 20062737>
 Local 1 (W-PKG): #<Package USER 20062737>
 Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 47702351>
 Local 3 (THROW-FLAG): T
 Local 4: ("Return to top level in ~A." "Lisp Listener 1")
 Local 5: ((SYSTEM:ABORT EH:DEBUGGER-CONDITION) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
 Local 6 (VALUES): NIL
 Local 7: NIL
 Local 8 (VALUE): FOO


Remainder of stack:

SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, cosmetic, valid,
*** EOOH ***
Date: Monday, 24 March 1986, 12:02-EST
From: Robert Ingria <rjpi@LMI-ANGEL>
Subject: <HELP> in ZMail
To: BUG-ZMail@LMI-Angel
Message-ID: <[LMI-BORIS].24-Mar-86 12:02:45.rjpi>

In ZMAIL in Experimental System 110.152, Experimental Lambda-Diag 7.3,
Experimental Local-File 68.4, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.1, Experimental ZMail 65.10,
Experimental Object Lisp 3.0, Experimental Tape 6.9,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408, SDU ROM 102,
Alpha III (3/20 mrc), on Boris Badinoff (LAMBDA):

        Typing <HELP> in ZMail STILL prints out:

C,C,D,L,A,U,V,W,<SPACE>,<HELP>

as the options.  Note the repetition of ``C''.



0,, 3, valid,
*** EOOH ***
Message-ID: <8603241240.AA12231@LMI-CAPRICORN.ARPA>
Date: Monday, 24 March 1986, 07:38-EST
From: rjpi@LMI-ANGEL
Sender: Ingria@LMI-ANGEL
Subject: System Menu Programs Column
To: BUG-LISPM@LMI-Angel

In Experimental System 110.152, Experimental Lambda-Diag 7.3,
Experimental Local-File 68.4, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.1, Experimental ZMail 65.9,
Experimental Object Lisp 3.0, Experimental Tape 6.9,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408, SDU ROM 102,
Alpha III (3/20 mrc), on Boris Badinoff (LAMBDA):

[Mail] used to be one of the options in the Programs column of the
System Menu; it's not there in the current version.  Was this a
deliberate removal or did it get removed accidentally?



0,, 3, valid,
*** EOOH ***
Message-ID: <8603220511.AA10643@LMI-CAPRICORN.ARPA>
Date: Saturday, 22 March 1986, 00:11-EST
From: Mark Nahabedian <naha@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Angel, sam@LMI-ANGEL
Subject: frame option to split screen is broken

Frame option in split screen is broken with two windows (lisp and edit).

I looked into this a bit the other day.  What I found out is that
when you select the frame option, a choose-variable-vaules menu
is supposed to pop up to ask you about the name of the frame and
what system key to put it on.  This menu blows out during initialization.

The specific case in which these menu's fail is when they are required
to compute their size rather than being told it.  The code that is there
in the file now could not possibly ever work.  No obveous fix has
come to mind yet.  Whoever is responsible for breaking this should
go home and eat some drano.

        -naha



0,, 3, valid,
*** EOOH ***
Date: Thursday, 20 March 1986, 15:45-EST
From: software-release@LMI-ANGEL
Sender: BUGS@LMI-ANGEL
Subject: Adding keywords to messages in ZMail.
To: BUG-ZMAIL@LMI-Angel
Message-ID: <[LMI-MAURICE-RAVEL].20-Mar-86 15:45:40.BUGS>

In ZMAIL in Experimental System 110.142, Experimental Lambda-Diag 7.3,
Experimental Local-File 68.4, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.1, Experimental ZMail 65.7,
Experimental Object Lisp 3.0, Experimental Tape 6.9,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408, SDU ROM 102,
Alpha III Andover, on Maurice Ravel (LAMBDA):

In ZMail, if you add a new keyword to a message, two menu items
get created for the item.

-dg




0,, fixed, valid,
*** EOOH ***
Message-ID: <8603180733.AA03940@LMI-CAPRICORN.ARPA>
Date: Tuesday, 18 March 1986, 02:31-EST
From: SAM@Emma
Subject: Pretty-printing Barfs on Code-Like DATA
To: BUG-LISPM@LMI-Angel

In Experimental System 110.121, Experimental Lambda-Diag 7.0,
Experimental Local-File 68.1, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Object Lisp 3.0, Experimental Tape 6.0,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408, SDU ROM 8,
Alpha III Cambridge, on Emma Willard (LAMBDA):


Insert your description of the circumstances here:

I was trying to pretty-print some Lisp DATA (which happens to be Prolog CODE).
Here is a small piece that fails:

(let ((*print-pretty* t)) (print '(or ?first . ?rest)))



>>TRAP 4466 (ARGTYP CONS M-T T CDR CDR)
The argument to CDR, ?REST, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

SI::GRIND-AND (P.C. = 77)

 Arg 0 (EXP): (OR ?FIRST . ?REST)
 Arg 1 (LOC): ((OR ?FIRST . ?REST))


SI::GRIND-FORM (P.C. = 76)

 Arg 0 (EXP): (OR ?FIRST . ?REST)
 Arg 1 (LOC): ((OR ?FIRST . ?REST))
 Local 0 (TEM): SI::GRIND-AND
 Local 1 (GMF): NIL


SI::GRIND-TRY (P.C. = 113)

 Arg 0 (FORM): SI::GRIND-FORM
 Arg 1 (EXP): (OR ?FIRST . ?REST)
 Arg 2 (LOC): ((OR ?FIRST . ?REST))
 Rest arg (ARGS): NIL
 Local 1 (MARK): NIL
 Local 2 (VP): NIL
 Local 3 (HP): NIL


SI::GRIND-OPTI-MISER (P.C. = 47)

 Arg 0 (EXP): (OR ?FIRST . ?REST)
 Arg 1 (LOC): ((OR ?FIRST . ?REST))


GRIND-TOP-LEVEL (P.C. = 131)

 Arg 0 (EXP): (OR ?FIRST . ?REST)
 Arg 1 (GRIND-WIDTH): 127
 Arg 2 (GRIND-REAL-IO): #:*TERMINAL-IO*-SYN-STREAM
 Arg 3 (GRIND-UNTYO-P): NIL
 Arg 4 (GRIND-DISPLACED): SI::DISPLACED
 Arg 5 (TERPRI-P): NIL
   --Defaulted args:--
 Arg 6 (GRIND-NOTIFY-FUN): NIL
 Arg 7 (LOC): ((OR ?FIRST . ?REST))
 Arg 8 (GRIND-FORMAT): SI::GRIND-OPTI-MISER
 Arg 9 (INITIAL-INDENTATION): 0
 Local 0 (I): NIL


Remainder of stack:

SI:PRINT-OBJECT (P.C. = 210)
SI::PRINT-CIRCLE (P.C. = 80)
PRINT (P.C. = 51)
SYSTEM:EVAL1 (P.C. = 419)
(:SPECIAL-FORM LET) (P.C. = 271)
SYSTEM:EVAL1 (P.C. = 372)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
BREAK (P.C. = 394)
ZWEI::COM-BREAK (P.C. = 30)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, fixed,
*** EOOH ***
Message-ID: <8603180428.AA03436@LMI-CAPRICORN.ARPA>
Date: Monday, 17 March 1986, 23:27-EST
From: bobp@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.105, Experimental ZMail 65.7,
Experimental Unix-Interface 9.0, Experimental Local-File 66.1,
Experimental MagTape 4.1, Experimental FILE-Server 18.1, microcode 1408,
SDU Boot Tape 3.7, SDU ROM 102, Nifty+, on Poindexter (LAMBDA):


Insert your description of the circumstances here:

While doing (apropos "") ...
don't know if this is machine flake or effect of recent patches.


>>ERROR: Obsolete special form, recompile the definition of #<DTP-FEF-POINTER LAMBDA::DEFMIC 46370771>
Backtrace from the debugger:

ARGLIST (P.C. = 677)

 Arg 0 (FUNCTION): #<DTP-FEF-POINTER LAMBDA::DEFMIC 46370771>
 Arg 1 (REAL-FLAG): NIL
 Local 0 (TEM): NIL
 Local 1 (DEBUG-INFO): ((COMPILER::LOCAL-MAP #))
 Local 2 (ARG-MAP): NIL
 Local 3 (LOCAL-MAP): ((LAMBDA::X) (LAMBDA::NAME) (LAMBDA::OPCODE) (ARGLIST) ...)
 Local 4 (TEM): NIL
 Local 5 (I): NIL
 Local 6 (L): NIL
 Local 7 (ADL): NIL
 Local 8 (ARGNUM): NIL
 Local 9 (ARGNAME): NIL
 Local 10 (OPTIONALP): NIL
 Local 11 (SPECIAL): NIL
 Local 12 (INIT): NIL
 Local 13 (INITP): NIL
 Local 14 (ADLWORD): NIL
 Local 15 (ARGLIS): NIL
 Local 16 (.SELECTQ.ITEM.): NIL
 Local 17 (.SELECTQ.ITEM.): NIL
 Local 18 (SYM): NIL
 Local 19 (CELL-FUNCTION): NIL
 Local 20 (FAST-OPT): 2097152
 Local 21 (RES): NIL
 Local 22 (MIN-ARGS): 0
 Local 23 (MAX-ARGS): 0
 Local 24 (EVALED-REST): 0
 Local 25: NIL


ARGLIST (P.C. = 201)

 Arg 0 (FUNCTION): LAMBDA::DEFMIC
   --Defaulted args:--
 Arg 1 (REAL-FLAG): NIL
 Local 0 (TEM): NIL
 Local 1 (DEBUG-INFO): NIL
 Local 2 (ARG-MAP): NIL
 Local 3 (LOCAL-MAP): NIL
 Local 4 (TEM): NIL
 Local 5 (I): NIL
 Local 6 (L): NIL
 Local 7 (ADL): NIL
 Local 8 (ARGNUM): NIL
 Local 9 (ARGNAME): NIL
 Local 10 (OPTIONALP): NIL
 Local 11 (SPECIAL): NIL
 Local 12 (INIT): NIL
 Local 13 (INITP): NIL
 Local 14 (ADLWORD): NIL
 Local 15 (ARGLIS): NIL
 Local 16 (.SELECTQ.ITEM.): NIL
 Local 17 (.SELECTQ.ITEM.): NIL
 Local 18 (SYM): NIL
 Local 19 (CELL-FUNCTION): NIL
 Local 20 (FAST-OPT): NIL
 Local 21 (RES): NIL
 Local 22 (MIN-ARGS): NIL
 Local 23 (MAX-ARGS): NIL
 Local 24 (EVALED-REST): NIL
 Local 25: NIL


SI::APROPOS-1 (P.C. = 117)

 Arg 0 (SYMBOL): LAMBDA::DEFMIC
 Local 0 (P): "DEFMIC"
 Local 1: NIL
 Local 2 (S): NIL


APROPOS (P.C. = 221)

 Arg 0 (SUBSTRING): ""
   --Defaulted args:--
 Arg 1 (PKG): (#<Package TOPS20 6215354> #<Package ANSI 2675316> #<Package LMI-UTILS 2676046> #<Package LAMBDA 2500020> ...)
 Rest arg: NIL
 Local 1 (INHERITORS): NIL
 Local 2 (INHERITED): T
 Local 3 (DONT-PRINT): NIL
 Local 4 (PREDICATE): NIL
 Local 5 (BOUNDP): NIL
 Local 6 (FBOUNDP): NIL
 Local 7 (APROPOS-PACKAGES): (#<Package KEYWORD 2140020> #<Package LISP 6277345> #<Package COMPILER 6235433> #<Package USER 6222142> ...)
 Local 8: 192
 Local 9 (P): #<Package LAMBDA 2500020>
 Local 10: (#<Package LAMBDA 2500020> #<Package ZWEI Utility Package 2670065> #<Package SMTP 2665005> #<Package NETWORK 2662354> ...)
 Local 11 (U): NIL
 Local 12: #<Package LAMBDA 2500020>
 Local 13: 20161
 Local 14 (SYMBOL): LAMBDA::DEFMIC
 Local 15 (IGNORE): 2117097


SYSTEM:EVAL1 (P.C. = 410)

 Arg 0 (FORM): (APROPOS "")
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER APROPOS 46470462>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER APROPOS 46470462>
 Local 5 (ARG-DESC): 1179714
 Local 6 (NUM-ARGS): 1
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ARG): ""


Remainder of stack:

SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)
SI:LISP-TOP-LEVEL (P.C. = 39)



0,, 3, indeterminate,
*** EOOH ***
Date: Monday, 17 March 1986, 14:03-EST
From: Debbie Ellerin <Ayesha!debbie@angel>
Message-ID: <8603171903.AA00145@Ayesha.ARPA>
To: bug-lispm@angel

0, unseen, recent,,
*** EOOH ***
From: uucp
Date: Monday, 1 January 1900, -5:00-EST

>From MIT-CCC!KRALL%mcc-pp@mcc.arpa  Fri Mar 14 14:01:26 1986 remote from angel
Received: by angel.ARPA (4.12/4.7)  id AA08626; Fri, 14 Mar 86 14:01:26 est
Received: from MIT-XX.ARPA by MIT-CCC via duct; 14 Mar 86 13:47-EST
Received: from MCC.ARPA by XX.LCS.MIT.EDU with TCP; Fri 14 Mar 86 13:00:35-EST
Received: from mcc-pp by MCC.ARPA with TCP; Fri 14 Mar 86 12:00:52-CST
Posted-Date: Friday, 14 March 1986, 12:03-CST
Message-Id: <8603141800.AA09989@mcc-pp>
Received: from amber by mcc-pp (4.12/RKA.851124)
        id AA09989; Fri, 14 Mar 86 12:00:49 cst
Date: Friday, 14 March 1986, 12:03-CST
From: <angel!krall%amethyst%mcc-pp@mcc.arpa>
Sender: angel!KRALL%mcc-pp@mcc.arpa
Subject: Imagen software bug
To: lmi-angel!debbie%mit-ccc%mit-xx@mcc.arpa
Cc: krall%amethyst%mcc-pp@mcc.arpa, gjc%mit-mc.ARPA@mcc.arpa


The latest version of Imagen software contains

(DEFVAR *PRINTER-OPTIONS* '(:FONT :FONT-LIST :HEADING-FONT :PAGE-HEADINGS :VSP
                            :COPIES :SPOOL )
  "A list of keyword options that can be defaulted on a per-printer-type basis.
This is looked at by SI:GET-PRINTER-DEFAULT-OPERATIONS.")

I think it should contain

(DEFVAR si:*PRINTER-OPTIONS* '(:FONT :FONT-LIST :HEADING-FONT :PAGE-HEADINGS :VSP
                            :COPIES :SPOOL :print-mode)
  "A list of keyword options that can be defaulted on a per-printer-type basis.
This is looked at by SI:GET-PRINTER-DEFAULT-OPERATIONS.")

and

        (SET-PRINTER-DEFAULT-OPTION :IMAGEN :PRINT-MODE :LPR)

WIthout this fix, I get the error that
        (GETF OPTIONS PRINT-MODE) is NIL,
since si:get-printer-default-options will not look for a
print-mode.






0,, 3, valid,
*** EOOH ***
Message-ID: <8603171511.AA02362@LMI-CAPRICORN.ARPA>
Date: Monday, 17 March 1986, 10:08-EST
From: act@LMI-ANGEL
To: BUG-LISPM@LMI-Angel

In Experimental System 110.114, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.12,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.2, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.4, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102,
Alpha-1 Release (mrc 3/14 Hopefully good site Info--mildly dirty),
on Boris Badinoff (LAMBDA):

While playing with fonts:
        I set the font map to (tr12 bigfnt).  Everything changed to tr12, right on schedule.
BUT, when :set-current-font was changed to bigfnt, keyboard entries were still printed as
tr12 while machine return was printed in bigfnt.



0,, 3, indeterminate,
*** EOOH ***
Date: Friday, 14 March 1986, 16:39-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: ZMail Bizareness
To: BUG-ZMAIL@LMI-Angel
Message-ID: <[LMI-BORIS].14-Mar-86 16:39:25.Ingria>

In ZMAIL in Experimental System 110.114, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.12,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.2, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.4, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102,
Alpha-1 Release (mrc 3/14 Hopefully good site Info--mildly dirty),
on Boris Badinoff (LAMBDA):

        Selected ZMail with <SYSTEM> M on a freshly booted machine.
Typed "G" to get my new mail.  There was only one new message in my
inbox (on Cap).  ZMail did NOT replace the Help information that is
displayed in (above?) the message pane area after new mail had been read
in.  Typing <SPACE> caused a beep and put me back at the FIRST message
in my primary mail file.

        This never happened to me before (including in this version of



0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8603141953.AA04649@Ayesha.ARPA>
Date: Friday, 14 March 1986, 14:59-EST
From: Ayesha!keith@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Evaluating (setf(subseq "Foobar" 3) "gle") .
Note that subseq accepts null end arg, means "to end of sequence";
so should subseq-setf.

No error is now (110.232) signalled.

"Foobar" becomes
#(70 111 111 103 108 101)

Is this right?

robert

0,, 3, valid,
*** EOOH ***
Message-ID: <8603251945.AA02299@Ayesha.ARPA>
Date: Friday, 14 March 1986, 14:20-EST
From: Robert Putnam <Ayesha!robert@ayesha.ARPA>
Subject: [lmi-angel!acosta at MCC.ARPA: common lisp defstruct bug]
To: bug-lispm@ayesha.ARPA

Message-ID: <8603251427.AA06353@angel.ARPA>
Date: Monday, 24 March 1986, 17:10-EST
From: lmi-angel!acosta@MCC.ARPA
Subject: common lisp defstruct bug
To: lmi-angel!robert%mit-ccc%mit-xx@MCC.ARPA
CC: acosta@MCC.ARPA

        Here is a copy of the file documenting the bug I just talked to you about.

                Thanks,
                -ramon


;Reading at top level in Lisp Listener 1.
;Reading in base 10 in package USER with standard Zetalisp readtable.

(common-lisp t)
;Reading in base 10 in package USER with standard Common-Lisp readtable.

(defstruct (entity) (dummy nil))
ENTITY
(setq foo (make-entity))
#S(ENTITY)
(entity-p foo)
NIL
(type-of foo)
ENTITY
(defun my-entity-p (thing)
        (eq (type-of thing) 'entity))
MY-ENTITY-P
(my-entity-p foo)
T
(compile 'my-entity-p)
MY-ENTITY-P
(my-entity-p foo)
NIL
(defun new-entity-p (thing)
        (let ((check-type (type-of thing)))
                (eq check-type 'entity)))
NEW-ENTITY-P
(new-entity-p foo)
T
(compile 'new-entity-p)
NEW-ENTITY-P
(new-entity-p foo)
T
(disassemble 'entity-p)
 14 MOVE D-PDL ARG|0          ;X
 15 (MISC) %DATA-TYPE D-PDL
 16 PUSH-NUMBER 25
 17 (MISC) M-= D-RETURN

ENTITY-P
(disassemble 'my-entity-p)
 14 MOVE D-PDL ARG|0          ;THING
 15 (MISC) %DATA-TYPE D-PDL
 16 PUSH-NUMBER 25
 17 (MISC) M-= D-RETURN

MY-ENTITY-P
(disassemble 'new-entity-p)
 18 CALL D-PDL FEF|6          ;#'TYPE-OF
 19 MOVE D-LAST ARG|0         ;THING
 20 MOVEM LOCAL|0             ;CHECK-TYPE
 21 MOVE D-PDL FEF|7          ;'ENTITY
 22 (MISC) M-EQ D-RETURN

NEW-ENTITY-P
(dribble)






0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8603132227.AA03507@Ayesha.ARPA>
Date: Thursday, 13 March 1986, 17:26-EST
From: Ayesha!keith@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Just booted up (bare alpha 2 band). Entered <system>-s to supdup,
entered host name, and bang. Note that inhibit-scheduling-flag was
set and default cons area was format-area...

>>TRAP 12625 (PDL-OVERFLOW REGULAR)
The regular push-down list has overflown.
Backtrace from the debugger:

(:PROPERTY FORMAT::FORMAT-PARAMS SI::RESOURCE-FREE-LIST-CELL) (P.C. = 32602)

 Arg 0 (OBJECT): #<ART-Q-LIST 10 fill-pointer 0 10015523>


ALLOCATE-RESOURCE (P.C. = 158)

 Arg 0 (RESOURCE-NAME): FORMAT::FORMAT-PARAMS
 Rest arg (PARAMETERS): NIL
 Local 1 (RESOURCE): #S(SI::RESOURCE :NAME FORMAT::FORMAT-PARAMS :N-OBJECTS ...)
 Local 2 (PARAMS): NIL
 Local 3 (TEM): NIL
 Local 4 (INDEX): NIL
 Local 5 (OLD): NIL
 Local 6 (INITIALIZER): NIL
 Local 7 (CHECKER): NIL
 Local 8 (MATCHER): NIL
 Local 9 (CELL): #<DTP-LOCATIVE 7024351>
 Local 10 (OBJ): #<ART-Q-LIST 10 fill-pointer 0 10015523>
 Local 11 (N-OBJECTS): NIL
 Local 12 (N): NIL
 Local 13 (IN-USE-P): NIL


FORMAT::FORMAT-PARSE-CLAUSES (P.C. = 107)

 Arg 0 (CLOSECHAR): FORMAT::}
 Arg 1 (SEMIP): NIL
 Local 0: #<ART-Q-LIST 10 fill-pointer 0 13150454>
 Local 1 (START): 5
 Local 2 (CLAUSES): #<ART-Q-LIST 30 fill-pointer 0 13150412>
 Local 3 (STACK): #<ART-Q-LIST 10 fill-pointer 0 13150454>
 Local 4 (I): 2
 Local 5 (J): 2
 Local 6 (TEM): NIL
 Local 7 (COMMAND): NIL


(:PROPERTY FORMAT::{ FORMAT:FORMAT-CTL-MULTI-ARG) (P.C. = 58)

 Arg 0 (ARGS): (("TESTS") "TEST-HELPERS" "QFASL" ">")
 Arg 1 (PARAMS): NIL
 Local 0 (*FORMAT-OUTPUT*): FORMAT::FORMAT-STRING-STREAM
 Local 1 (LIMIT): NIL
 Local 2 (CLAUSES): NIL
 Local 3 (STR): NIL
 Local 4 (OKAY-TO-EXIT): NIL


FORMAT::FORMAT-CTL-OP (P.C. = 63)

 Arg 0 (OP): FORMAT::{
 Arg 1 (ARGS): (("TESTS") "TEST-HELPERS" "QFASL" ">")
 Arg 2 (PARAMS): NIL
 Local 0 (TEM): #<DTP-FEF-POINTER (:PROPERTY FORMAT::{ FORMAT:FORMAT-CTL-MULTI-ARG) 17145233>


Remainder of stack:

FORMAT::FORMAT-CTL-STRING (P.C. = 94)
(:PROPERTY FORMAT::[ FORMAT:FORMAT-CTL-MULTI-ARG) (P.C. = 211)
FORMAT::FORMAT-CTL-OP (P.C. = 63)
FORMAT::FORMAT-CTL-STRING (P.C. = 94)
FORMAT (P.C. = 177)
(:METHOD FS::LOGICAL-PATHNAME :STRING-FOR-PRINTING) (P.C. = 88)
(:METHOD FS::LOGICAL-PATHNAME :COMBINED :STRING-FOR-PRINTING) (P.C. = 41)
(:METHOD SI:VANILLA-FLAVOR :SEND-IF-HANDLES) (P.C. = 55)
STRING (P.C. = 70)
FS::FILE-PROCESS-ERROR (P.C. = 60)
...
FS::LOAD-1 (P.C. = 217)
LOAD (P.C. = 64)
LOGIN (P.C. = 192)
FS:FORCE-USER-TO-LOGIN (P.C. = 125)
(:METHOD SUPDUP::BASIC-NVT :BEFORE :CONNECT) (P.C. = 28)
(:METHOD SUPDUP :COMBINED :CONNECT) (P.C. = 37)
(:METHOD SUPDUP::BASIC-NVT :TYPEIN-TOP-LEVEL) (P.C. = 414)
SUPDUP::TYPEIN-TOP-LEVEL (P.C. = 20)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, 3, valid,
*** EOOH ***
Message-ID: <8603121703.AA00839@LMI-CAPRICORN.ARPA>
Date: Wednesday, 12 March 1986, 11:47-EST
From: mrc@LMI-ANGEL
Subject: FED
To: BUG-LISPM@LMI-Angel

In Experimental System 110.105, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.11,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.2, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102, Alpha-1 Release (rjpi: 3/7/86; used band),
on Natasha Nogoodnik (LAMBDA):


I clicked right after invoking DRAW SPLINE but before specifying any points with left clicks.
This put me in the error handler. If you ask to draw a spline without any points you should get
message "Can't draw spline before points are specified.  Click left to specify points; click
right when you are done."


>>TRAP 6009 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS) M-ARRAY-POINTER)
The subscript 0 for #() was out of range in AS-1.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 3 values

TV:SPLINE (P.C. = 253)

 Arg 0 (PX): #<ART-Q 100 fill-pointer 0 15640751>
 Arg 1 (PY): #<ART-Q 100 fill-pointer 0 15641121>
 Arg 2 (Z): 10
 Arg 3 (CX): #<ART-Q 56 (simple) 15642033>
 Arg 4 (CY): #<ART-Q 56 (simple) 15642124>
   --Defaulted args:--
 Arg 5 (C1): :RELAXED
 Arg 6 (C2): :RELAXED
 Arg 7 (P1-PRIME-X): NIL
 Arg 8 (P1-PRIME-Y): NIL
 Arg 9 (PN-PRIME-X): NIL
 Arg 10 (PN-PRIME-Y): NIL
 Local 0 (N): 0
 Local 1 (N-1): -1
 Local 2 (N-2): -2
 Local 3 (N-3): -3
 Local 4 (BX): NIL
 Local 5 (BY): NIL
 Local 6 (L): #()
 Local 7 (UX): NIL
 Local 8 (UY): NIL
 Local 9 (N1): #()
 Local 10 (N2): #()
 Local 11 (N3): #()
 Local 12 (N4): NIL
 Local 13 (SIGN): NIL
 Local 14 (ZUNDERFLOW): T
 Local 15 (CLEN): -10
 Local 16 (J): 0
 Local 17: -2
 Local 18 (TEM): NIL
 Local 19 (S3): NIL
 Local 20 (L0): NIL
 Local 21 (L1): NIL
 Local 22 (PX0): NIL
 Local 23 (PX1): NIL
 Local 24 (PX2): NIL
 Local 25 (PY0): NIL
 Local 26 (PY1): NIL
 Local 27 (PY2): NIL
 Local 28 (Q): NIL
 Local 29 (I): NIL
 Local 30 (N1I): NIL
 Local 31 (D): NIL
 Local 32 (N3J): NIL
 Local 33 (UXN-2): NIL
 Local 34 (UYN-2): NIL
 Local 35 (N4J): NIL


FED::COM-MOUSE-DRAW-SPLINE (P.C. = 195)

 Local 0 (I): NIL
 Local 1 (Y): (:MOUSE-BUTTON 1048578 #<FED Fed 1 35676317 exposed> 116 ...)
 Local 2: 0
 Local 3: 0
 Local 4 (X): NIL
 Local 5 (I): 0


(:METHOD FED :COMMAND-LOOP) (P.C. = 130)
  (SELF is #<FED Fed 1 35676317 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): T
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): NIL


SI::PROCESS-TOP-LEVEL (P.C. = 113)

 Arg 0 (IGNORE): NIL
 Local 0: ("Reset and arrest process ~A." "Fed 1")
 Local 1: (CONDITION ("Reset and arrest process ~A." "Fed 1") T ("Reset and arrest process ~A." "Fed 1") ...)
 Local 2: ("Restart process ~A." "Fed 1")
 Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "Fed 1") T ("Restart process ~A." "Fed 1") ...)




0,, 3, valid,
*** EOOH ***
Message-ID: <8603121647.AA00809@LMI-CAPRICORN.ARPA>
Date: Wednesday, 12 March 1986, 11:34-EST
From: mrc@LMI-ANGEL
Subject: FED
To: BUG-LISPM@LMI-Angel

In Experimental System 110.105, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.11,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.2, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102, Alpha-1 Release (rjpi: 3/7/86; used band),
on Natasha Nogoodnik (LAMBDA):


Two different DISPLAY SCALE problems:
1) If the scale is too small, you run out of dots and the screen looks funny.

2) If the scale is too big you get the following problem:


>>TRAP 11412 (BITBLT-DESTINATION-TOO-SMALL)
The destination of a BITBLT was too small.
Backtrace from the debugger:

(:METHOD FED::GRAY-GRID-MIXIN :REDISPLAY-POINT) (P.C. = 78)
  (SELF is #<FED Fed 2 44116734 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY-POINT
 Arg 1 (I): 37
 Arg 2 (J): 26
 Arg 3 (NEW-VALUE): 3
 Arg 4 (OLD-VALUE): 0


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD FED::GRID-MIXIN :REDISPLAY) (P.C. = 216)
  (SELF is #<FED Fed 2 44116734 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
   --Defaulted args:--
 Arg 1 (FORCE-TO-COMPLETION): NIL
 Local 0 (PLANE-EDGES): (-10 0 17 18)
 Local 1 (J): 26
 Local 2 (I): 37
 Local 3 (OLD-VALUE): 0
 Local 4 (NEW-VALUE): 3


(:METHOD FED :COMBINED :REDISPLAY) (P.C. = 50)
  (SELF is #<FED Fed 2 44116734 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:REDISPLAY)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 37 leader-length 19 63614750>


(:METHOD FED :COMMAND-LOOP) (P.C. = 107)
  (SELF is #<FED Fed 2 44116734 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): T
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): NIL


SI::PROCESS-TOP-LEVEL (P.C. = 113)

   --Defaulted args:--
 Arg 0 (IGNORE): NIL
 Local 0: ("Reset and arrest process ~A." "Fed 2")
 Local 1: (CONDITION ("Reset and arrest process ~A." "Fed 2") T ("Reset and arrest process ~A." "Fed 2") ...)
 Local 2: ("Restart process ~A." "Fed 2")
 Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "Fed 2") T ("Restart process ~A." "Fed 2") ...)




0,, 3, valid,
*** EOOH ***
Message-ID: <8603121633.AA00782@LMI-CAPRICORN.ARPA>
Date: Wednesday, 12 March 1986, 11:20-EST
From: mrc@LMI-ANGEL
Subject: FED
To: BUG-LISPM@LMI-Angel

In Experimental System 110.105, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.11,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.2, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102, Alpha-1 Release (rjpi: 3/7/86; used band),
on Natasha Nogoodnik (LAMBDA):


When you click right on FONT and there is not yet a loaded font, FED goes off to look for
font NIL.  and then of course complains.  FED should test if a font is loaded and simply
print a message saying "Can't display current font, no font loaded".

>>ERROR: Font NIL not found
Backtrace from the debugger:

TV::SCREEN-PARSE-FONT-DESCRIPTOR (P.C. = 214)

 Arg 0 (FD): NIL
 Arg 1 (TYPE): FONTS:CPT-FONT
   --Defaulted args:--
 Arg 2 (DONT-LOAD-P): NIL
 Local 0 (FONT): NIL
 Local 1: (NIL NIL "Font ~D not found" NIL)
 Local 2: #SYSTEM:CONNECTION-ERROR :PROPERTY-LIST (:CONNECTION #<CHAOS Connection to LMI-DJINN FILE 1 61757001>) :CONDITION-NAMES (SYSTEM:CONNECTION-ERROR SYSTEM:REMOTE-NETWORK-ERROR SYSTEM:NETWORK-ERROR ERROR ...) :FORMAT-STRING "Host ~1@*~A not respond
ing." :FORMAT-ARGS (#<CHAOS Connection to LMI-DJINN FILE 1 61757001> #FS::LISPM-HOST "LMI-DJINN") :CONNECTION #<CHAOS Connection to LMI-DJINN FILE 1 61757001> :FOREIGN-HOST #FS::LISPM-HOST "LMI-DJINN"


(:METHOD TV:SCREEN :PARSE-FONT-DESCRIPTOR) (P.C. = 23)
  (SELF is #<TV::STANDARD-SCREEN Main Screen 22500073 exposed>)

 Arg 0 (.OPERATION.): :PARSE-FONT-DESCRIPTOR
 Arg 1 (FD): NIL


FED:DISPLAY-FONT (P.C. = 124)

 Arg 0 (FONT): NIL
 Arg 1 (WINDOW): #<FED::FED-TYPEOUT-WINDOW Fed Typeout Window 2 44117152 exposed>
 Arg 2 (CLEAR-FIRST-P): T
 Arg 3 (FROM-FED): T
 Local 0 (FONT-MAP): NIL
 Local 1 (CURRENT-FONT): NIL
 Local 2 (NAME): NIL
 Local 3 (FD): NIL
 Local 4 (DF): NIL
 Local 5 (CH): NIL
 Local 6 (OCH): NIL
 Local 7 (LEN): NIL
 Local 8 (CH1): NIL


FED::COM-DISPLAY-FONT (P.C. = 62)

   --Defaulted args:--
 Arg 0 (FONT): NIL
 Arg 1 (WINDOW): #<FED::FED-TYPEOUT-WINDOW Fed Typeout Window 2 44117152 exposed>
 Arg 2 (FROM-FED): T
 Arg 3 (CLEAR-FIRST-P): T


(:METHOD FED :COMMAND-LOOP) (P.C. = 130)
  (SELF is #<FED Fed 2 44116734 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): T
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): NIL


Remainder of stack:

SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, cosmetic, valid,
*** EOOH ***
Message-ID: <8603191653.AA00150@Ayesha.ARPA>
Date: Wednesday, 12 March 1986, 07:25-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!hotchkiss@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0,
Experimental IRIS 1.0, microcode 768, prol patch obj, on Morticia:


The documentation for rewind/unload says:

This command rewinds the tape to load point if the left mouse button
is used.  If the middle button is used, then the tape is unloaded.
If the tape is unloaded, all subsequent operations will get an error
until another tape is unloaded."

It should say  "if the tape is unloaded, ... until another tape is loaded."




0,, valid, 2, critical,
*** EOOH ***
Message-ID: <8603182339.AA00774@Ayesha.ARPA>
Date: Tuesday, 11 March 1986, 14:14-EST
From: Ayesha!robert@ayesha.ARPA
To: bug-lispm@ayesha.ARPA

In Experimental System 110.121, Experimental Lambda-Diag 7.0,
Experimental Local-File 68.2, Experimental FILE-Server 18.2,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Object Lisp 3.0, Experimental Tape 6.6,
Experimental Site Data Editor 3.1, Experimental Tiger 24.0,
Experimental KERMIT 31.2, Experimental Window-Maker 1.0,
Experimental Gateway 4.0, Experimental TCP-Kernel 39.5,
Experimental TCP-User 62.5, Experimental TCP-Server 45.5,
Experimental MEDIUM-RESOLUTION-COLOR 3.0,
Experimental MICRO-COMPILATION-TOOLS 3.0, microcode 1408,
SDU Boot Tape 3.8, SDU ROM 8, Alpha III Andover, on Thing (LAMBDA):

Is zz dynamically scoped in the following (interpreted) example?
[I think this was introduced with Objectlisp in release 2.0.]

(makunbound 'zz)

(setf (plist 'zz) nil)

(defun foo (zz)
  "Evaluate zz, call bar, then evaluate it again."
  (format t "~%Zz's initial value inside foo: ~s" zz)
  (bar)
  (format t "~%Zz's value has changed inside foo: ~s" zz))

(defun bar ()
  "Make free reference to zz, then setq it."
  (format t "~%Bar sees zz: ~s"  zz)
  (setq zz 'inside))

(foo 'outside)
Zz's initial value inside foo: OUTSIDE
Bar sees zz: OUTSIDE
Zz's value has changed inside foo: INSIDE
NIL

robert




0,, 3, valid,
*** EOOH ***
Date: Monday, 10 March 1986, 16:50-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Checking for Owner of BABYL File
To: BUG-ZMAIL@LMI-Angel
Message-ID: <[LMI-BORIS].10-Mar-86 16:50:49.Ingria>

In ZMAIL in Experimental System 110.109, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.11,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.2, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1368, SDU ROM 102, Alpha-1 Release (rjpi: 3/7/86; used band),
on Boris Badinoff (LAMBDA):

ZMail will warn if you try to save a Babyl mail file whose author name
is the same as your user name BUT has leading or trailing white space.
ZMail should trim the whitespace and THEN do the comparison.



0,, fixed, valid,
*** EOOH ***
Message-ID: <8603120130.AA00762@Ayesha.ARPA>
Date: Monday, 10 March 1986, 12:26-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Pressing middle button while using a side scroll cursor on a zmail msg:

>>ERROR: Position of POINT on window is screwed up.
Backtrace from the debugger:

ZWEI::WINDOW-REDISPLAY-DIS-BPS (P.C. = 179)

 Arg 0 (IGNORE): 16234
 Arg 1 (POINT-PLINE): 0
 Arg 2 (RECENTER-TYPE): :START
 Arg 3 (INITIAL-DEGREE): 0
 Local 0 (POINT-LINE): ">From mrc@LMI-CAPRICORN.ARPA  Wed Mar  5 16:46:36 1986 remote from angel"
 Local 1 (POINT-INDEX): 0
 Local 2 (POINT-NODE): #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/keith/ ENG: 15404103>
 Local 3 (START-BP-NODE): #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/keith/ ENG: 15404103>
 Local 4 (BUF): #<ZWEI::NODE 25064216>
 Local 5 (FROM-INDEX): NIL
 Local 6 (DISPLAYER): NIL
 Local 7: NIL
 Local 8 (BL): NIL


(:METHOD ZWEI:WINDOW :REDISPLAY) (P.C. = 333)
  (SELF is #<ZWEI::ZMAIL-WINDOW Zmail Window 1 15662605 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
 Arg 1 (RECENTER-TYPE): :START
 Arg 2 (RC1): ("" 0)
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL
 Local 0 (LH): 14
 Local 1 (NOW): 16234
 Local 2 (POINT-PLINE): 0
 Local 3 (POINT-LINE): ">From mrc@LMI-CAPRICORN.ARPA  Wed Mar  5 16:46:36 1986 remote from angel"
 Local 4 (POINT-INDEX): 0
 Local 5 (TOP-LINE): ""
 Local 6 (TOP-INDEX): 0
 Local 7 (INITIAL-DEGREE): 0
 Local 8 (NEW-TOP-INDEX): NIL
 Local 9 (Y): NIL
 Local 10 (I): NIL


ZWEI::REDISPLAY (P.C. = 56)

 Arg 0 (WINDOW): #<ZWEI::ZMAIL-WINDOW Zmail Window 1 15662605 exposed>
 Arg 1 (RECENTER-TYPE): :START
 Arg 2 (RC1): ("" 0)
   --Defaulted args:--
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL


(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST-DEFAULT ZWEI::SCROLL) (P.C. = 44)

 Arg 0 (IGNORE): ZWEI::SCROLL
 Arg 1 (WINDOW): #<ZWEI::ZMAIL-WINDOW Zmail Window 1 15662605 exposed>
 Arg 2 (NLINES): ("" 0)
 Arg 3 (TYPE): :START


(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
  (SELF is #<ZWEI::ZMAIL-FRAME Main ZMail window 11030601 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
 Rest arg (ARGS): (ZWEI::SCROLL #<ZWEI::ZMAIL-WINDOW Zmail Window 1 15662605 exposed> 2 :ABSOLUTE)


Remainder of stack:

(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, fixed, valid,
*** EOOH ***
Message-ID: <8603120127.AA00753@Ayesha.ARPA>
Date: Monday, 10 March 1986, 12:23-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Successfully read my mail, but selecting 'save files' caused:

>>TRAP 6892 (ARGTYP ARRAY M-ARRAY-POINTER 0 (DECODE-1D-ARRAY-RESTART RESTORE-ARRAY-REGISTERS))
The first argument to AR-1, NIL, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 2 values

(:METHOD ZWEI::MAIL-FILE-BUFFER :LAST-LINE-FOR-APPEND) (P.C. = 41)
  (SELF is #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>)

 Arg 0 (.OPERATION.): :LAST-LINE-FOR-APPEND
   --Defaulted args:--
 Arg 1 (NMSGS): 100
 Local 0 (BP): NIL
 Local 1 (BP1): NIL
 Local 2 (LINE): NIL
 Local 3 (PREV-END-BPS): NIL
 Local 4 (MSG): NIL


ZWEI::INSERT-NEW-MAIL (P.C. = 145)

 Arg 0 (OLD-FILE): #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>
 Arg 1 (NEW-FILE): #<ZWEI::UNIX-INBOX-BUFFER ENG: /lmi/debbie/mbox 15400137>
 Local 0 (APPEND-P): T
 Local 1 (OLD-INT): #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>
 Local 2 (NEW-INT): #<ZWEI::UNIX-INBOX-BUFFER ENG: /lmi/debbie/mbox 15400137>
 Local 3 (INT-APPEND-P): T
 Local 4 (NMSGS): NIL
 Local 5: #<DTP-LOCATIVE 15400017>
 Local 6: NIL
 Local 7: #<DTP-LOCATIVE 15400156>
 Local 8: T
 Local 9 (END-LINE): NIL
 Local 10 (PREV-MSG-END-BPS): NIL
 Local 11 (START-LINE): NIL
 Local 12: NIL
 Local 13 (BP): NIL
 Local 14 (PREV): NIL
 Local 15 (NEW-START-LINE): NIL
 Local 16 (NEW-END-LINE): NIL
 Local 17 (NEW-INFS): NIL
 Local 18 (OLD-INFS): NIL
 Local 19 (LAST-INT): NIL
 Local 20 (FIRST-INT): NIL
 Local 21 (INT): NIL
 Local 22 (LAST-INT-END): NIL
 Local 23 (FIRST-INT-START): NIL
 Local 24 (LAST-INT-END-1): NIL
 Local 25 (MOVE-1-P): NIL
 Local 26 (NEW-ARRAY): NIL
 Local 27 (OLD-ARRAY): NIL
 Local 28 (OLDLEN): NIL
 Local 29 (NEWLEN): NIL
 Local 30 (I): NIL
 Local 31 (J): NIL
 Local 32 (MSG): NIL
 Local 33 (LAST-BP): NIL
 Local 34 (MSG-LAST-BP): NIL
 Local 35 (AT-END-P): NIL


(:METHOD ZWEI::MAIL-FILE-BUFFER :LOADING-DONE) (P.C. = 85)
  (SELF is #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>)

 Arg 0 (.OPERATION.): :LOADING-DONE
 Local 0 (SORT): NIL
 Local 1: NIL
 Local 2: NIL


(:METHOD ZWEI::UNIX-MAIL-FILE-BUFFER :COMBINED :LOADING-DONE) (P.C. = 70)
  (SELF is #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:LOADING-DONE)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 4 leader-length 12 7516225>


(:METHOD ZWEI::ZMAIL-DISK-BUFFER :READ-NEXT-MSG) (P.C. = 461)
  (SELF is #<ZWEI::UNIX-MAIL-FILE-BUFFER mbox /lmi/debbie/ ENG: 15400000>)

 Arg 0 (.OPERATION.): :READ-NEXT-MSG
 Arg 1 (NMSGS): 65535
 Local 0 (EOF): NIL
 Local 1: #<DTP-LOCATIVE 15400017>
 Local 2: NIL
 Local 3 (START): NIL
 Local 4 (LINE-WAITING-FLAG): NIL
 Local 5 (TEST-FUNCTION): NIL
 Local 6 (END-LINE): NIL
 Local 7 (LINE): NIL
 Local 8 (LENGTH): NIL
 Local 9 (END-IDX): NIL
 Local 10 (MSG-REAL-START-BP): NIL
 Local 11 (STATE): NIL
 Local 12 (I): NIL
 Local 13 (MSG-REAL-INTERVAL): NIL
 Local 14 (MSG-INTERVAL): NIL
 Local 15: NIL
 Local 16 (LINE): NIL
 Local 17 (LAST): NIL
 Local 18 (INFS): NIL
 Local 19 (LAST-BP-0): NIL
 Local 20 (LAST-BP-1): NIL


Remainder of stack:

ZWEI::LOAD-ALL-MSGS (P.C. = 32)
ZWEI::ASSURE-ZMAIL-BUFFER-FULLY-LOADED (P.C. = 53)
ZWEI::FOREGROUND-BACKGROUND-FINISH (P.C. = 129)
ZWEI::EXPUNGE-ZMAIL-BUFFER (P.C. = 105)
ZWEI::ZMAIL-SAVE-ALL (P.C. = 35)
ZWEI::COM-ZMAIL-SAVE (P.C. = 53)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, fixed,
*** EOOH ***
Message-ID: <8603120121.AA00730@Ayesha.ARPA>
Date: Monday, 10 March 1986, 12:16-EST
From: Ayesha!keith@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Just logged in. Went into Zmail, selected 'get new mail', and:

>>TRAP 7968 (ARGTYP NUMBER PP 0 QIEQL)
The first argument to =, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 3 values

ZWEI::READ-LEXEME (P.C. = 199)

 Arg 0 (RDTBL): #<READTABLE 5740236>
 Arg 1 (START-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 2 (START-INDEX): 17
 Arg 3 (END-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 4 (END-INDEX): 21
 Arg 5 (ERROR-P): :RECURSIVE
 Arg 6 (BACKSLASH-P): T
 Local 0 (CH): NIL
 Local 1 (CODE): 13
 Local 2 (STATE): (ZWEI::UNTYI . ATOM)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 5740740>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Local 6 (INDEX): 21
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): NIL
 Local 10 (ACTION): NIL


Additional information supplied with call:
 Expecting 3 values

(:PROPERTY COMMENT ZWEI::RFC733) (P.C. = 51)

 Arg 0 (TYPE): COMMENT
 Arg 1 (RDTBL): #<READTABLE 5740236>
 Arg 2 (START-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 3 (START-INDEX): 16
 Arg 4 (END-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 5 (END-INDEX): 21
 Local 0 (STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Local 1 (INDEX): 17
 Local 2 (TEM): NIL


Additional information supplied with call:
 Expecting 4 values

ZWEI::READ-LEXEME (P.C. = 157)

 Arg 0 (RDTBL): #<READTABLE 5740236>
 Arg 1 (START-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 2 (START-INDEX): 15
 Arg 3 (END-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 4 (END-INDEX): 21
 Arg 5 (ERROR-P): NIL
   --Defaulted args:--
 Arg 6 (BACKSLASH-P): NIL
 Local 0 (CH): 40
 Local 1 (CODE): 12
 Local 2 (STATE): (ZWEI::START . COMMENT)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 5740740>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Local 6 (INDEX): 16
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): ZWEI::START
 Local 10 (ACTION): COMMENT


ZWEI::RDTBL-LEXER (P.C. = 33)

 Arg 0 (RDTBL): #<READTABLE 5740236>
 Arg 1 (START-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 2 (START-INDEX): 0
 Arg 3 (END-STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 4 (END-INDEX): 21
 Arg 5 (ERROR-P): NIL
 Local 0: ((ATOM "Alpha-1" # #) (ATOM "Release" # #))
 Local 1: ((ATOM "Release" # #))
 Local 2 (STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Local 3 (INDEX): 15
 Local 4 (TEM): (ATOM "Release" ("Alpha-1 Release (rjpi: 2//14//86), on Natasha Nogoodnik (LAMBDA):" 8) ("Alpha-1 Release (rjpi: 2//14//86), on Natasha Nogoodnik (LAMBDA):" 15))
 Local 5 (ERRMES): NIL


ZWEI::RFC733-LEXER (P.C. = 44)

 Arg 0 (STRING): "Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):"
 Arg 1 (START): 0
 Arg 2 (END): 21
 Arg 3 (ERROR-P): NIL


Remainder of stack:

ZWEI::PROBABLE-ITS-HEADER-P (P.C. = 86)
ZWEI::FIRST-TEXT-LINE (P.C. = 91)
(:PROPERTY :SUBJECT ZWEI::SUMMARY-PRINTER) (P.C. = 37)
ZWEI::SET-MSG-SUMMARY-LINE (P.C. = 79)
ZWEI::SET-PARSED-MSG-HEADERS (P.C. = 111)
(:METHOD ZWEI::ZMAIL-DISK-BUFFER :PARSE-MSG) (P.C. = 21)
(:METHOD ZWEI::UNIX-INBOX-BUFFER :COMBINED :PARSE-MSG) (P.C. = 40)
ZWEI::ASSURE-MSG-PARSED (P.C. = 64)
ZWEI::MSG-PUT (P.C. = 27)
ZWEI::INSERT-NEW-MAIL (P.C. = 363)
...
(:METHOD ZWEI::UNIX-MAIL-FILE-BUFFER :COMBINED :LOADING-DONE) (P.C. = 70)
(:SELECT-METHOD ZWEI::ZMAIL-BACKGROUND-REQUEST ZWEI::FILE-LOADED) (P.C. = 18)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 149)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, valid, 4,
*** EOOH ***
Message-ID: <8603132159.AA03420@Ayesha.ARPA>
Date: Monday, 10 March 1986, 10:18-EST
From: Robert Putnam <Ayesha!robert@ayesha.ARPA>
To: bug-lispm@ayesha.ARPA

In System 102.176, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, TCP-Kernel 30.12, TCP-User 57.12,
TCP-Server 33.5, microcode 782, REL2.0TCP, on Cousin It:

The prompt for the P option in dired ...

  Print? (Q, E, Y, or N)  Yes, then expunge.

robert




0,, 3, valid,
*** EOOH ***
Message-ID: <8603132132.AA03378@Ayesha.ARPA>
Date: Monday, 10 March 1986, 09:51-EST
From: Robert Putnam <Ayesha!robert@ayesha.ARPA>
To: bug-lispm@ayesha.ARPA

In System 102.176, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, TCP-Kernel 30.12, TCP-User 57.12,
TCP-Server 33.5, microcode 782, REL2.0TCP, on Cousin It:

; Compilation of the following code,

(defvar thing7)

(defflavor flavor7 (thing7) ()
  (:gettable-instance-variables)
  (:settable-instance-variables)
  (:initable-instance-variables))

; generates the following compiler warning

  << While compiling (:METHOD FLAVOR7 :THING7) >>
   The special variable THING7 is an instance variable of FLAVOR7
  but was not mentioned in a :SPECIAL-INSTANCE-VARIABLES in that flavor.
  This function will not execute correctly unless the DEFFLAVOR is fixed.

; and causes the following warning when the flavor is instantiated:

  Instance variable THING7 of FLAVOR7 being made special
  because that variable is globally special

Bug, feature?

robert



0,, doc.prob, 3, valid,
*** EOOH ***
Message-ID: <8603110858.AA00600@Ayesha.ARPA>
Date: Sunday, 9 March 1986, 19:53-EST
From: Debbie Ellerin <Ayesha!debbie@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):


Does tv:use-kbd-buttons do anything? The documentation claims
that if it is non nil, and the mode-lock key is down, then the
Roman numeral keys I through III are to be treated as mouse clicks.
But, this doesn't appear to be happening.

debbie



0,, issues, 3, valid,
*** EOOH ***
Message-ID: <8603110735.AA00454@Ayesha.ARPA>
Date: Sunday, 9 March 1986, 18:28-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!SOFTSERV@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Cousin It (LAMBDA):

When I use metering I get a warning that metering turns off
garbage collection and that I should use (meter:resume-gc-process)
to un-arrest gc.
I noticed these 2 arrest reasons were put on the gc process
 (meter:metering  :gc-stopped) by metering.
However, (meter:resume-gc-process) only removes the
first of these arrest reasons; and so when I do terminal-g
I see that the automatic garbage collector is still disabled.

Why can't meter:disable restart the gc anyway ?

debbie




0,, 4, indeterminate,
*** EOOH ***
Message-ID: <8603072228.AA01174@LMI-CAPRICORN.ARPA>
Date: Friday, 7 March 1986, 17:27-EST
From: pecann@LMI-ANGEL
Sender: SKY@LMI-ANGEL
To: BUG-LISPM@LMI-Angel

In Experimental System 110.105, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0, Experimental Tape 1.11,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.4,
Experimental TCP-User 62.2, Experimental TCP-Server 45.5,
Experimental Unix-Interface 9.0, Experimental ZMail 65.7,
Experimental Window-Maker 1.0, Experimental Local-File 67.0,
microcode 1380, SDU ROM 8, dirty alpha1     waiting for a new release.,
on Mary had a little Lambda (LAMBDA):


Insert your description of the circumstances here:

Clicked on Abort in window attribute cvv menu.

>>ERROR: The value of SI::OPERATION, (*THROW (QUOTE TV::ABORT-EDIT) NIL), is not a symbol.
Backtrace from the debugger:

NAMED-STRUCTURE-INVOKE (P.C. = 46)

 Arg 0 (OPERATION): (*THROW (QUOTE TV::ABORT-EDIT) NIL)
 Arg 1 (STRUCTURE): #S(SI::INTERPRETER-SPECIAL-FORM :NAME THROW :HANDLER ...)
 Rest arg (ARGS): NIL
 Local 1 (C): NIL


SI::CALL-NAMED-STRUCTURE (P.C. = 26)

 Rest arg (ARGS): ((*THROW # NIL))
 Local 1 (TEM): ((*THROW # NIL))


SYSTEM:EVAL1 (P.C. = 363)

 Arg 0 (FORM): (*THROW (QUOTE TV::ABORT-EDIT) NIL)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): NIL
 Local 1 (ENV): (NIL NIL NIL NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #S(SI::INTERPRETER-SPECIAL-FORM :NAME *THROW :HANDLER ...)
 Local 4 (CALL-FUNCTION): #S(SI::INTERPRETER-SPECIAL-FORM :NAME *THROW :HANDLER ...)
 Local 5 (ARG-DESC): 65
 Local 6 (NUM-ARGS): NIL
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ARG): NIL


EVAL (P.C. = 96)

 Arg 0 (FORM): (*THROW (QUOTE TV::ABORT-EDIT) NIL)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


TV:CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE (P.C. = 49)

 Arg 0 (WINDOW): #<TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW Temporary Choose Variable Values Window 1 13013145 deactivated>
 Arg 1 (MSG): (*THROW (QUOTE TV::ABORT-EDIT) NIL)


Remainder of stack:

TV:CHOOSE-VARIABLE-VALUES (P.C. = 352)
TV::SCREEN-EDITOR-EDIT-ATTRIBUTES (P.C. = 273)
TV::SYSTEM-MENU-EDIT-WINDOW-ATTRIBUTES (P.C. = 20)
(:METHOD TV:WINDOW-HACKING-MENU-MIXIN :EXECUTE-WINDOW-OP) (P.C. = 26)
(:METHOD TV:MENU-EXECUTE-MIXIN :EXECUTE) (P.C. = 120)
(:METHOD TV:MOMENTARY-MENU :COMBINED :EXECUTE) (P.C. = 42)
(:METHOD TV:BASIC-MENU :CHOOSE) (P.C. = 52)
(:INTERNAL (:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) 0) (P.C. = 60)
(:METHOD TV:BASIC-MOMENTARY-MENU :AROUND :CHOOSE) (P.C. = 50)
(:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) (P.C. = 39)
(:INTERNAL TV:MOUSE-CALL-SYSTEM-MENU 0) (P.C. = 34)
SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 64)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8603052137.AA03830@LMI-CAPRICORN.ARPA>
Date: Wednesday, 5 March 1986, 16:35-EST
From: mrc@LMI-CAPRICORN
Subject: Another Zmail Bug
To: BUG-LISPM@LMI-Angel

In Experimental System 110.79, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.11, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.4, Experimental Window-Maker 1.0, microcode 1380,
SDU ROM 8, dirty alpha1     waiting for a new release.,
on Mary had a little Lambda (LAMBDA):


Insert your description of the circumstances here:

ZMAIL--just tried to go to a new message.

>>ERROR: The object #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 13100137> received a :UPDATE-OPTIONS-IN-FILE message, which went unclaimed.
 The rest of the message was ().
Backtrace from the debugger:

#<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 13100137>:
   Arg 0: :UPDATE-OPTIONS-IN-FILE


SI::INSTANCE-HASH-FAILURE (P.C. = 162)

 Arg 0 (OP): :UPDATE-OPTIONS-IN-FILE
 Rest arg (ARGS): NIL
 Local 1 (HARRY): #<SI::HASH-ARRAY 129/177 :test EQ (Funcallable) 6361344>
 Local 2 (FN-LOCATION): NIL
 Local 3 (FUNC): NIL
 Local 4 (.POINTER.): #<DTP-LOCATIVE 6361331>
 Local 5 (.ALREADY.MINE.): NIL
 Local 6 (TEM): 0
 Local 7 (NEW): NIL
 Local 8: NIL


(:METHOD ZWEI::ZMAIL-BUFFER :MODIFIED-P) (P.C. = 61)
  (SELF is #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 13100137>)

 Arg 0 (.OPERATION.): :MODIFIED-P
 Local 0 (.ARRAY.): #<ART-Q 64 fill-pointer 13 13100201>
 Local 1 (.I.): 13
 Local 2 (.NMSGS.): 13
 Local 3 (MSG): #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 63051645> :INTERVAL ...)


ZWEI::BUFFER-MODIFIED-P (P.C. = 18)

 Arg 0 (BUFFER): #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 13100137>


ZWEI::UNDO-SAVE-NEW-SMALL-CHANGE (P.C. = 330)

 Arg 0 (BP1): ("I just found a customer report that there" 0 :NORMAL)
 Arg 1 (BP2): ("I just found a customer report that there" 0 :NORMAL)
 Local 0 (UNDO-STATUS): (#<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 13100137> NIL ("I just found a customer report that there" 0 :NORMAL) ("I just found a customer report that there" 0 :MOVES) ...)
 Local 1 (BP1-INSIDE): NIL
 Local 2 (BP2-INSIDE): NIL
 Local 3 (LINE): NIL
 Local 4 (END-LINE): NIL
 Local 5 (ALIST): ((# 0 0))
 Local 6 (COUNT): 0
 Local 7: NIL
 Local 8 (INDEX): 0


Remainder of stack:

ZWEI::INSERT (P.C. = 127)
ZWEI::INSERT-MOVING (P.C. = 35)
ZWEI::COM-SELF-INSERT (P.C. = 61)
ZWEI::COM-ORDINARILY-SELF-INSERT (P.C. = 17)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) 0) (P.C. = 58)
FUNCALL (P.C. = 21)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MOUSE-BUTTON) (P.C. = 59)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, valid, 2,
*** EOOH ***
Message-ID: <8603052022.AA03633@LMI-CAPRICORN.ARPA>
Date: Wednesday, 5 March 1986, 15:22-EST
From: Mark Nahabedian <naha@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Test Lambda A (LAMBDA):

The arrays si:*global-shared-memory-8*, 16 and 32 as setup by

(defun set-up-shared-memory ()
  (setq *global-shared-memory-size* (%system-configuration-global-shared-size *sys-conf*))
  (setq *global-shared-memory-8*
        (make-array *global-shared-memory-size*
                    :type :art-8b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*))))
  (setq *global-shared-memory-16*
        (make-array (// *global-shared-memory-size* 2)
                    :type :art-16b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*))))
  (setq *global-shared-memory-32*
        (make-array (// *global-shared-memory-size* 4)
                    :type :art-32b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*)))))

in SYS:SYS;CONFIG are too big.  Referencing a sufficiently high element in one of these
arrays causes the machine to ILLOP in PGF-MM0.

;REFERENCE TO UNIBUS OR X-BUS IO VIRTUAL ADDRESS (on CADR).  FAKE UP PAGE HASH TABLE ENTRY
PGF-MM0 (JUMP-LESS-THAN M-T (A-CONSTANT 177100000) MAP-VIDEO-BUFFER)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT LOWEST-UNIBUS-VIRTUAL-ADDRESS) MAP-MULTIBUS)
        (CALL-GREATER-OR-EQUAL M-T (A-CONSTANT 177377400) ILLOP)  ;CADR control registers.
                        ;this page left blank so they will trap.
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177377000) MAP-MULTIBUS-IO)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177371000) MAP-NU-MULTI-MAP-REGS)  ;See below.
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177370400) MAP-TV-CONTROL-REGS)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177370000) MAP-SDU-CONTROL-REGS)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177360000) MAP-SHARED-PAGES-1)
 +-->   (CALL-GREATER-OR-EQUAL M-T (A-CONSTANT 177340000) ILLOP)                  ;Scratch block.
 |      (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177300000) MAP-SHARED-PAGES-2)
 |
 +----<<< THIS IS THE PLACE.

Simple calculation shows that the value of

(+ (// si:*global-shared-memory-size* 4)
   (si:%pointer-unsigned (sdu-phys-to-virtual
                            (%system-configuration-global-shared-base *sys-conf*))))

177344400

Falls in the ILLOP region of the virtual address space.

A quick solution is to hack the definition of SET-UP-SHARED-MEMORY to adjust
*GLOBAL-SHARED-MEMORY-SIZE* to make sure it does not exceed the boundary.
This would be adequate if the shared memory arrays allways fall at the end
of the "MAP-SHARED-PAGES-2" space.

si:
(defun set-up-shared-memory ()
  (setq *global-shared-memory-size* (%system-configuration-global-shared-size *sys-conf*))
  (setq *global-shared-memory-size*
        (min *global-shared-memory-size*
             (* 4                       ;4 bytes per virtual address
                (- #o177340000 (%pointer-unsigned
                                 (sdu-phys-to-virtual
                                   (%system-configuration-global-shared-base *sys-conf*)))))))
  (setq *global-shared-memory-8*
        (make-array *global-shared-memory-size*
                    :type :art-8b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*))))
  (setq *global-shared-memory-16*
        (make-array (// *global-shared-memory-size* 2)
                    :type :art-16b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*))))
  (setq *global-shared-memory-32*
        (make-array (// *global-shared-memory-size* 4)
                    :type :art-32b
                    :displaced-to (sdu-phys-to-virtual
                                    (%system-configuration-global-shared-base *sys-conf*)))))

GJC suggests that it might be time to think about physical displaced arrays.

Let me know if I should patch the above definition of SET-UP-SHARED-MEMORY into
the system.

        -naha



0,, 3, valid,
*** EOOH ***
Date: Wednesday, 5 March 1986, 15:16-EST
From: Debbie Ellerin <Ayesha!debbie@angel>
Message-ID: <8603052016.AA03511@Ayesha.ARPA>
To: bug-lispm@angel

To: BUG-LISPM@AYESHA
--Text Follows This Line--
In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0,
Experimental IRIS 1.0, Experimental MICRO-COMPILATION-TOOLS 3.0,
microcode 768, prol patch obj, on Morticia:


Insert your description of the circumstances here:

I was trying to micro-compile the functions seed and random.
I micro-compiled them , and then tried to load them.
Seed loaded , but random got the following error:

;;; -*- Mode:LISP; Package:USER -*-
;;; here is the file  --- taken from "ql.verify.bench;traverse"
;;; (I get the same error at a different point in "ql.verify.bench;traverseu")

(defmacro mod (x n) `(remainder ,x ,n))

(declare (special rand)#-LISPM (FIXNUM rand))

(setq rand 21.)

(compiler:define-micro-properties seed ())
(defun seed () (setq rand 21.) (mod 4. 2.))

(compiler:define-micro-properties random ())
(defun random () (setq rand (mod 17. 251.)))



;;; this is the output from microcompiling random
(zwei:com-microcompile-region )
(COMPILER::MA-HOOK-UP-STATES)
(COMPILER::MA-HOOK-UP-OPERANDS)
(COMPILER::MA-BRANCH-TENSION)
(COMPILER::MA-OPTIMIZE)
(COMPILER::MA-HOOK-UP-STATES)
(COMPILER::MA-HOOK-UP-OPERANDS)
(COMPILER::MA-BRANCH-TENSION)
(COMPILER::MA-OPTIMIZE)
(COMPILER::MA-HOOK-UP-STATES)
(COMPILER::MA-HOOK-UP-OPERANDS)
(COMPILER::MA-BRANCH-TENSION)
(COMPILER::MA-OPTIMIZE)
(COMPILER::MA-HOOK-UP-STATES)
(COMPILER::MA-HOOK-UP-OPERANDS)
(COMPILER::MA-BRANCH-TENSION)
(COMPILER::MA-OPTIMIZE)
(COMPILER::MA-HOOK-UP-STATES)
(COMPILER::MA-HOOK-UP-OPERANDS)
(COMPILER::MA-BRANCH-TENSION)
(COMPILER::MA-OPTIMIZE)
(COMPILER::MA-CONVERT)
Combine dest hack! 42974839320 (164392 ((2508 (COMPILER::MCLAP-GET-A-CONSTANT 167772177))))

;;; here is the error generated from (compiler:ma-load 'random)
>>ERROR: A-CONSTANT memory full
Backtrace from the debugger:

COMPILER::MCLAP-LOAD (P.C. = 128)

 Arg 0 (LOAD-P): T
 Arg 1 (MCLAP): ((** ** ** ** ...) (COMPILER::PROGSA ** 1586194548544 ** ...))
   --Defaulted args:--
 Arg 2 (MICRO-PC-LIST): NIL
Local 0 (PARAM-LIST): ((COMPILER::%MAX-IP-PDL-LEVEL 2) (COMPILER::DEBUG-INFO **) (COMPILER::ALLVARS NIL) (COMPILER::%MAXARGS 0) ...)
Local 1 (MCLAP-CODE): (COMPILER::PROGSA (3221225472 **) 1586194548544 (2147484231 **) ...)
Local 2 (FUNCTION-NAME): RANDOM
Local 3 (NEW-C-LOC): 17273
Local 4 (RTN-ACT): (167772177 167772411)
Local 5 (RTN-EVT): ((SPECIAL RAND))
Local 6 (RTN-MM-LINKAGE-LIST): NIL
Local 7: (167772177 167772411)
Local 8 (C): 167772177
Local 9 (Q): NIL


COMPILER::MA-LOAD (P.C. = 104)

 Rest arg (FUNCTIONS): (RANDOM)
Local 1: (RANDOM)
Local 2 (FUNCTION-NAME): RANDOM


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (COMPILER::MA-LOAD (QUOTE RANDOM))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER COMPILER::MA-LOAD 13320767>
Local 6 (ARG-DESC): 1048576
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (COMPILER::MA-LOAD (QUOTE RANDOM))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (COMPILER::MA-LOAD (QUOTE RANDOM))
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


Remainder of stack:

APPLY (P.C. = 24)
SYSTEM:EVAL1 (P.C. = 547)
COND (P.C. = 58)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
MULTIPLE-VALUE-LIST (P.C. = 21)
SYSTEM:EVAL1 (P.C. = 547)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
|#



0,, fixed, valid,
*** EOOH ***
Message-ID: <8603041430.AA00173@Ayesha.ARPA>
Date: Tuesday, 4 March 1986, 11:34-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@LURCH.ARPA

In System 102.176, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, TCP-Kernel 30.12, TCP-User 57.12,
TCP-Server 33.5, microcode 768, REL2.0TCP, on Cousin It:


Insert your description of the circumstances here:

Selected 'quit' from ZMail window, with a message displayed
on top.

>>ERROR: The object #<TV::STANDARD-SCREEN Main Screen 3200073 exposed> received a :SELECT message, which went unclaimed.
The rest of the message was NIL.
Backtrace from the debugger:

#<TV::STANDARD-SCREEN Main Screen 3200073 exposed>:
   Arg 0: :SELECT


SI::INSTANCE-HASH-FAILURE (P.C. = 116)

 Arg 0 (OP): :SELECT
 Rest arg (ARGS): NIL
Local 1 (HT): #<EQ-SI::HASH-ARRAY (Funcallable) 22706145>
Local 2 (FN-LOCATION): NIL
Local 3 (FUNC): NIL
Local 4 (NEWHT): NIL
Local 5: NIL


TV::SELECT-PREVIOUS-WINDOW (P.C. = 165)

 Arg 0 (WINDOW): #<TV::STANDARD-SCREEN Main Screen 3200073 exposed>
 Arg 1 (MOUSE-P): NIL
 Arg 2 (DEFAULT-TO-LISP-LISTENER): NIL
   --Defaulted args:--
 Arg 3 (MOUSE-SELECT): NIL
Local 0 (I): 0
Local 1: 30
Local 2 (.QUEUE-LEFT.): T
Local 3 (SW): NIL
Local 4 (E): NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD TV:SELECT-MIXIN :DESELECT) (P.C. = 74)
  (SELF is #<ZWEI::ZMAIL-FRAME Main ZMail window 3212156 exposed>)

 Arg 0 (.OPERATION.): :DESELECT
 Arg 1 (RESTORE-SELECTED): :LAST
Local 0 (SEL-P): T


(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :DESELECT) (P.C. = 109)
  (SELF is #<ZWEI::ZMAIL-FRAME Main ZMail window 3212156 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:DESELECT :LAST)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-13 6352374>
Local 2 (ARGS): (:LAST)
Local 3 (.QUEUE-LEFT.): T
Local 4: NIL
Local 5 (E): NIL


Remainder of stack:

TV:DESELECT-AND-MAYBE-BURY-WINDOW (P.C. = 72)
ZWEI::COM-ZMAIL-QUIT (P.C. = 89)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)





0,, fixed,
*** EOOH ***
Message-ID: <8603041421.AB00133@Ayesha.ARPA>
Date: Tuesday, 4 March 1986, 11:25-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA

In System 102.176, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, TCP-Kernel 30.12, TCP-User 57.12,
TCP-Server 33.5, microcode 768, REL2.0TCP, on Cousin It:

In System 102.176, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, TCP-Kernel 30.12, TCP-User 57.12,
TCP-Server 33.5, microcode 768, REL2.0TCP, on Cousin It:


Insert your description of the circumstances here:

Compiling a reference to a THROW with no value causes
compiler to barf, but it evaluates ok...

E.g.  (defun foo()(catch 'foo (print 'ok) (throw 'foo)))

Should be a more informative error, or more lax and assume NIL value.

>>ERROR: Not on special var list: NIL
Backtrace from the debugger:

COMPILER::BARF (P.C. = 66)

 Arg 0 (EXP): NIL
 Arg 1 (REASON): "Not on special var list"
 Arg 2 (SEVERITY): COMPILER::BARF


COMPILER::LAP-SPECIAL-ADR (P.C. = 50)

 Arg 0 (VAR): NIL
Local 0 (TM): NIL


COMPILER::LAP-WORD-EVAL (P.C. = 112)

 Arg 0 (WD): ((SPECIAL NIL))
Local 0 (VL): 17408
Local 1 (TM): (SPECIAL NIL)
Local 2 (INDEX): NIL


COMPILER::QLP2-U (P.C. = 93)

 Arg 0 (WD): (COMPILER::MOVE COMPILER::D-PDL (SPECIAL NIL))
Local 0 (TEM): NIL


COMPILER::QLAP-PASS2 (P.C. = 43)

 Arg 0 (PNTR): (COMPILER::PROGSA (COMPILER::ADI-CALL CALL COMPILER::D-RETURN ** ...) (COMPILER::MOVE COMPILER::D-PDL **) (CALL COMPILER::D-IGNORE **) ...)
Local 0 (P): ((COMPILER::MOVE COMPILER::D-PDL **) (COMPILER::MISC COMPILER::D-IGNORE *THROW) (COMPILER::RESTART-TAG #:G0725) (COMPILER::PARAM COMPILER::MXPDL 15))


Remainder of stack:

COMPILER::QLAPP (P.C. = 256)
COMPILER::QC-TRANSLATE-FUNCTION (P.C. = 391)
COMPILER:COMPILE-1 (P.C. = 67)
(:INTERNAL COMPILE COMPILER::FOO) (P.C. = 162)
COMPILE (P.C. = 126)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
BREAK (P.C. = 437)
ZWEI::COM-BREAK (P.C. = 36)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)







0,, fixed,
*** EOOH ***
Message-ID: <8603032253.AA06495@LMI-CAPRICORN.ARPA>
Date: Monday, 3 March 1986, 17:52-EST
From: rjpi@LMI-ANGEL
Sender: Ingria@LMI-ANGEL
Subject: ????
To: BUG-ZMail@LMI-Angel

In Experimental System 110.79, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.11, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.6, Experimental Window-Maker 1.0, microcode 1380,
SDU ROM 8, dirty alpha1     waiting for a new release.,
on Mary had a little Lambda (LAMBDA):


Insert your description of the circumstances here:

Got this after I tried to select [Profile] from the ZMail menu.
(Selecting this option originally tried to toss me into the Cold Load
Stream.  I typed U; then <TERMINAL> 0S.)

>>TRAP 5587 (ARGTYP LIST PP T LENGTH)
The argument to LENGTH, #FS::UNIX-HOST "LMI-ANGEL", was of the wrong type.
The function expected a list.
Backtrace from the debugger:

(:SELECT-METHOD FORMAT::FORMAT-STRING-STREAM :STRING-OUT) (P.C. = 40)

 Arg 0 (IGNORE): :STRING-OUT
 Arg 1 (STRING): #FS::UNIX-HOST "LMI-ANGEL"
   --Defaulted args:--
 Arg 2 (FIRST): 0
 Arg 3 (LAST): NIL
 Local 0 (NEW-LENGTH): NIL


ZWEI::PRINT-STRING-OR-NIL (P.C. = 21)

 Arg 0 (STRING): #FS::UNIX-HOST "LMI-ANGEL"
 Arg 1 (STREAM): #<DTP-SELECT-METHOD 5040060>


(:METHOD TV:BASIC-CHOOSE-VARIABLE-VALUES :ITEM-WIDTH) (P.C. = 196)
  (SELF is #<ZWEI::ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 27435576 deexposed>)

 Arg 0 (.OPERATION.): :ITEM-WIDTH
 Arg 1 (ITEM): ("If non-NIL, this is the host whose name goes in From: fields of mail, by default." :STRING-OR-NIL)
 Arg 2 (EXTRA-WIDTH): 0
 Arg 3 (ITEM-NO): 0
 Local 0 (VAR): ZWEI::*FROM-HOST*
 Local 1 (VAL): #FS::UNIX-HOST "LMI-ANGEL"
 Local 2 (STR): "If non-NIL, this is the host whose name goes in From: fields of mail, by default."
 Local 3 (FONTNO): 1
 Local 4 (CHOICES): NIL
 Local 5 (PF): ZWEI::PRINT-STRING-OR-NIL
 Local 6 (RF): ZWEI::READ-STRING-OR-NIL
 Local 7 (K&A): (:STRING-OR-NIL)
 Local 8 (GPVF): NIL
 Local 9 (GVVF): NIL
 Local 10 (PVAL): NIL
 Local 11 (X): 664
 Local 12 (LEN): NIL
 Local 13 (NEW): NIL
 Local 14: NIL
 Local 15 (CHOICE): NIL


(:METHOD TV:BASIC-CHOOSE-VARIABLE-VALUES :APPROPRIATE-WIDTH) (P.C. = 85)
  (SELF is #<ZWEI::ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 27435576 deexposed>)

 Arg 0 (.OPERATION.): :APPROPRIATE-WIDTH
   --Defaulted args:--
 Arg 1 (EXTRA-WIDTH): 0
 Local 0 (NITEMS): 61
 Local 1 (I): 0
 Local 2 (ITEM): (ZWEI::*FROM-HOST* "If non-NIL, this is the host whose name goes in From: fields of mail, by default." :STRING-OR-NIL)
 Local 3: 0
 Local 4: 0
 Local 5: T


(:METHOD TV:BASIC-CHOOSE-VARIABLE-VALUES :PANE-SIZE) (P.C. = 54)
  (SELF is #<ZWEI::ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 27435576 deexposed>)

 Arg 0 (.OPERATION.): :PANE-SIZE
 Arg 1 (REM-WIDTH): 1018
 Arg 2 (REM-HEIGHT): 258
 Arg 3 (IGNORE): 1018
 Arg 4 (IGNORE): 258
 Arg 5 (STACKING): :HORIZONTAL


Remainder of stack:

(:INTERNAL (:METHOD ZWEI::ZMAIL-CHOOSE-VARIABLE-VALUES-PANE :COMBINED :PANE-SIZE) 0) (P.C. = 33)
(:METHOD TV:SCROLL-STUFF-ON-OFF-MIXIN :AROUND :PANE-SIZE) (P.C. = 80)
(:METHOD ZWEI::ZMAIL-CHOOSE-VARIABLE-VALUES-PANE :COMBINED :PANE-SIZE) (P.C. = 39)
TV::CONSTRAINT-FRAME-DO-A-CONSTRAINT (P.C. = 98)
TV::CONSTRAINT-FRAME-DO-SIZES-INTERNAL (P.C. = 67)
TV::CONSTRAINT-FRAME-DO-SIZES (P.C. = 64)
TV::CONSTRAINT-FRAME-DO-SIZES-INTERNAL (P.C. = 94)
TV::CONSTRAINT-FRAME-DO-SIZES (P.C. = 64)
TV::CONSTRAINT-FRAME-DO-SIZES-INTERNAL (P.C. = 99)
TV::CONSTRAINT-FRAME-DO-SIZES (P.C. = 64)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, indeterminate,
*** EOOH ***
Date: Monday, 3 March 1986, 17:49-EST
From: rjpi@LMI-ANGEL
Sender: Ingria@LMI-ANGEL
Subject: ZMail losing last line of messages?
To: BUG-ZMail@LMI-Angel
CC: dexter@angel
Message-ID: <[LMI-LAMB-CHOP].3-Mar-86 17:49:01.Ingria>

In ZMAIL in Experimental System 110.79, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.11, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.6, Experimental Window-Maker 1.0, microcode 1380,
SDU ROM 8, dirty alpha1     waiting for a new release.,
on Mary had a little Lambda (LAMBDA):

Just noticed that the last lines of these two messages seems to have
fallen prey to a line-eater.  Is ZMail/ZWEI eating the final line of
messages?

N.B. Both messages are given in their entirety.

======================================================================

Date: Thursday, 27 February 1986, 09:20-EST
From: dexter@LMI-ANGEL
To: BUG-LISPM@LMI-Angel
Message-ID: <[LMI-LAURIE-ANDERSON].27-Feb-86 09:20:10.DEXTER>

In Don't-dump-a-band! Experimental System 110.72,
Inconsistent (unreleased patches loaded) ZMail 65.6,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1,
Experimental ObjectLISP 2.0, Experimental IMicro 4.0, microcode 1371,
SDU Newboot 227, SDU ROM 102, paint/obl, on Laurie Anderson (LAMBDA):

Is there a new version of Meta-x Update Mode Line?  It seems to

======================================================================

Date: Friday, 28 February 1986, 16:21-EST
From: dexter@LMI-ANGEL
To: bug-lispm@LMI-ANGEL
Message-ID: <[LMI-LAURIE-ANDERSON].28-Feb-86 16:21:57.DEXTER>

Sorry about that last - the note said
REMOVE-DUPLICATE, but I see they obviously
mean REMOVE-DUPLICATES, which seems to

======================================================================



0,, unreproducible,
*** EOOH ***
Message-ID: <8603030500.AA05146@LMI-CAPRICORN.ARPA>
Date: Sunday, 2 March 1986, 23:59-EST
From: Dave Goodine <dg@LMI-ANGEL>
To: BUG-LISPM@LMI-Angel

In Experimental System 110.89, Experimental Local-File 66.1,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.6, Experimental Unix-Interface 9.0,
Experimental Tape 2.0, microcode 1408, SDU ROM 8,
on Lambda Two (LAMBDA):

compilation of a DO (not do*) no longer warns about free
variable references within the bindings:

        (do ((count 0 (add1 count))
             (copy-start 0 (* count adjusted-record-size)))
            ((= count number-of-records) count)
        ...

sorry... the interesting variable reference is COUNT, not ADJUSTED-RECORD-SIZE.

-dg

Note: not a bug.
5/9/86 rpp


0,, fixed, valid, 2,
*** EOOH ***
Message-ID: <8603010442.AA03767@LMI-CAPRICORN.ARPA>
Date: Friday, 28 February 1986, 23:40-EST
From: bobp@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.87, Experimental ZMail 65.6,
Experimental Unix-Interface 9.0, Experimental Local-File 66.1,
Experimental MagTape 4.1, Experimental FILE-Server 18.1, microcode 1408,
SDU Boot Tape 3.6, SDU ROM 103, Nifty+, on Poindexter (LAMBDA):

Just mentioning this again as release nears ...

While this machine is set up with gc on at boot, the microsecond
clock is not on by default.  The gc (I assume) makes the machine
lose time (up to 25% slow) if the microsecond clock is off (?);
this is kindof important.

Also, the <terminal>-G window is too narrow on this portrait monitor;
someone mentioned that other windows of similar type also have this
problem as of about two weeks ago.

- Bob



0,, valid, 2, indeterminate,
*** EOOH ***
Date: Friday, 28 February 1986, 16:14-EST
From: dexter@LMI-ANGEL
To: bug-lispm@LMI-ANGEL
Message-ID: <[LMI-LAURIE-ANDERSON].28-Feb-86 16:14:34.DEXTER>

I just found a customer report that there
were problems in our commonlisp:

        1. REMOVE-DUPLICATES is broken
        (no symptoms given)

        2. COERCE is broken
        (no symptoms given)

        3. SUBST is broken
        (no symptoms given)

        4. EVENP does not exist


EVENP certainly exists now.  Were there
problems with the others?  Are they fixed?




0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8602272013.AA00611@LMI-CAPRICORN.ARPA>
Date: Thursday, 27 February 1986, 15:14-EST
From: pecann@LMI-ANGEL
Sender: ERRIC@LMI-ANGEL
To: BUG-LISPM@LMI-Capricorn

In System 102.180, Local-File 56.13, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.10,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental ObjectLISP 2.0, Experimental vista 1.0,
Experimental IRIS 1.0, TCP-Kernel 30.12, TCP-User 57.11, TCP-Server 33.5,
microcode 783, LAM, on Lambda Fourteen:


Insert your description of the circumstances here:

Erric S. was doing a dired and had typed ahead randomness accidentally when this happened.  Interesting?

>>TRAP 6793 (ARGTYP ARRAY M-ARRAY-POINTER (GAHDR RESTORE-ARRAY-REGISTERS) GAHDR) ->  AP-LEADER
The NIL argument to AP-LEADER, NIL, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

ZWEI::DIRED-MAP-OVER-LINES (P.C. = 69)

 Arg 0 (N-TIMES): -1
 Arg 1 (FUNCTION): #<DTP-FEF-POINTER (:INTERNAL ZWEI::COM-DIRED-UNDELETE 0) 26637002>
Local 0 (BP): (NIL 0)
Local 1 (BOTTOM): ("" 0 :MOVES)
Local 2 (I): 0
Local 3: 1


ZWEI::COM-DIRED-UNDELETE (P.C. = 56)



ZWEI::COM-DIRED-REVERSE-UNDELETE (P.C. = 22)



Additional information supplied with call:
 Expecting 3 values

ZWEI::COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI::COM-DIRED-REVERSE-UNDELETE
 Arg 1 (CHAR): #/RUBOUT
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
Local 0 (HOOK-SUCCESS): T
Local 1: NIL
Local 2 (HOOK): NIL


ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)

 Arg 0 (CH): #/RUBOUT
Local 0 (VALUE): NIL
Local 1 (LINE): NIL
Local 2 (INDEX): NIL
Local 3: NIL
Local 4 (HOOK): NIL


Remainder of stack:

(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8602261642.AA04022@LMI-CAPRICORN.ARPA>
Date: Wednesday, 26 February 1986, 11:40-EST
From: pecann@LMI-ANGEL
Sender: GJC@LMI-ANGEL
To: BUG-LISPM@LMI-Angel

In Experimental System 110.75, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.0, Experimental Gateway 2.0,
Experimental Tape 1.11, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.0, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
SDU ROM 103, Alpha-2 Release, on Moe (LAMBDA):


Insert your description of the circumstances here:

Was copying partition from moe to chop, running copy-disk-partition on chas, when "foreign host died".
This is what was going on on the foreign host.  The server seems to have blown it.


>>ERROR: SI::RQB-TOO-LARGE is not a known condition flavor or signal name
Backtrace from the debugger:

MAKE-CONDITION (P.C. = 44)

 Arg 0 (SIGNAL-NAME): SI::RQB-TOO-LARGE
 Rest arg (ARGS): ("rqb's can't be bigger than ~d. pages" 64)


FERROR (P.C. = 42)

 Arg 0 (SIGNAL-NAME): SI::RQB-TOO-LARGE
 Arg 1 (FORMAT-STRING): "rqb's can't be bigger than ~d. pages"
 Rest arg (ARGS): (64)


SI::MAKE-DISK-RQB (P.C. = 79)

 Arg 0 (IGNORE): #S(SI::RESOURCE :NAME SI::RQB :N-OBJECTS ...)
 Arg 1 (N-PAGES): 85
 Arg 2 (LEADER-LENGTH): 4
 Local 0 (OVERHEAD): NIL
 Local 1 (ARRAY-LENGTH): NIL
 Local 2 (RQB-BUFFER): NIL
 Local 3 (RQB-8-BIT-BUFFER): NIL
 Local 4 (RQB): NIL
 Local 5 (RN): NIL


ALLOCATE-RESOURCE (P.C. = 260)

 Arg 0 (RESOURCE-NAME): SI::RQB
 Rest arg (PARAMETERS): (85 4)
 Local 1 (RESOURCE): #S(SI::RESOURCE :NAME SI::RQB :N-OBJECTS ...)
 Local 2 (PARAMS): (85 4)
 Local 3 (TEM): NIL
 Local 4 (INDEX): NIL
 Local 5 (OLD): NIL
 Local 6 (INITIALIZER): NIL
 Local 7 (CHECKER): NIL
 Local 8 (MATCHER): NIL
 Local 9 (CELL): NIL
 Local 10 (OBJ): #<ART-16B 2016 leader-length 4 12040016>
 Local 11 (N-OBJECTS): 8
 Local 12 (N): -1
 Local 13 (IN-USE-P): NIL


SYSTEM:GET-DISK-RQB (P.C. = 53)

 Arg 0 (N-PAGES): 85
   --Defaulted args:--
 Arg 1 (LEADER-LENGTH): 4


Remainder of stack:

CHAOS::REMOTE-DISK-SERVER (P.C. = 211)
SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 64)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, indeterminate,
*** EOOH ***
Message-ID: <8602261615.AA03931@LMI-CAPRICORN.ARPA>
Date: Wednesday, 26 February 1986, 11:14-EST
From: act@LMI-ANGEL
To: BUG-LISPM@LMI-Angel

In Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:

First I tried to load the not-standard font prt12b and got the message that the file was
left over from system 97 and there might be problems.  We went ahead and tried anyway; got
the "displaced array" message.  We gave up then, and it started trying to load font NIL.
>>ERROR: Font NIL not found
Backtrace from the debugger:

TV::SCREEN-PARSE-FONT-DESCRIPTOR (P.C. = 214)

 Arg 0 (FD): FONTS:NIL
 Arg 1 (TYPE): FONTS:CPT-FONT
 Arg 2 (DONT-LOAD-P): NIL
 Local 0 (FONT): FONTS:NIL
 Local 1: (NIL NIL "Font ~D not found" FONTS:NIL)
 Local 2: (FONTS:NIL)


TV::SCREEN-PARSE-FONT-DESCRIPTOR (P.C. = 147)

 Arg 0 (FD): NIL
 Arg 1 (TYPE): FONTS:CPT-FONT
   --Defaulted args:--
 Arg 2 (DONT-LOAD-P): NIL
 Local 0 (FONT): FONTS:NIL
 Local 1: NIL
 Local 2: NIL


(:METHOD TV:SCREEN :PARSE-FONT-SPECIFIER) (P.C. = 23)
  (SELF is #<TV::STANDARD-SCREEN Main Screen 7440073 exposed>)

 Arg 0 (.OPERATION.): :PARSE-FONT-SPECIFIER
 Arg 1 (FD): NIL


(:METHOD TV:SHEET :SET-CURRENT-FONT) (P.C. = 61)
  (SELF is #<FED::FED-LABEL-WINDOW Fed Label Window 2 35621576 exposed>)

 Arg 0 (.OPERATION.): :SET-CURRENT-FONT
 Arg 1 (NEW-FONT): NIL
 Arg 2 (OK-IF-NOT-IN-FONT-MAP): T
 Local 0 (FONT): NIL
 Local 1 (TEM): NIL
 Local 2 (BL): NIL


TV:SHEET-SET-FONT (P.C. = 20)

 Arg 0 (SHEET): #<FED::FED-LABEL-WINDOW Fed Label Window 2 35621576 exposed>
 Arg 1 (FONT): NIL


Remainder of stack:

FED::DISPLAY-LABEL (P.C. = 340)
FED::REDISPLAY-LABELS (P.C. = 27)
(:METHOD FED :BEFORE :REDISPLAY) (P.C. = 65)
(:METHOD FED :COMBINED :REDISPLAY) (P.C. = 43)
(:METHOD FED :COMMAND-LOOP) (P.C. = 107)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Message-ID: <8602252223.AA03464@LMI-CAPRICORN.ARPA>
Date: Tuesday, 25 February 1986, 17:23-EST
From: mrc@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):


Insert your description of the circumstances here:

I clicked R on "font" (display entire font) when there was no font loaded.  This
should error gracefully, ie "There is no current font" rather than throw the user
into the debugger.

>>ERROR: Font NIL not found
Backtrace from the debugger:

TV::SCREEN-PARSE-FONT-DESCRIPTOR (P.C. = 214)

 Arg 0 (FD): NIL
 Arg 1 (TYPE): FONTS:CPT-FONT
   --Defaulted args:--
 Arg 2 (DONT-LOAD-P): NIL
 Local 0 (FONT): NIL
 Local 1: (NIL NIL "Font ~D not found" NIL)
 Local 2: #SYSTEM:CONNECTION-ERROR :PROPERTY-LIST (:CONNECTION #<CHAOS Connection to LMI-DJINN FILE 1 33045347> :FOREIGN-HOST #FS::LISPM-HOST "LMI-DJINN" ...) :CONDITION-NAMES (SYSTEM:CONNECTION-ERROR SYSTEM:REMOTE-NETWORK-ERROR SYSTEM:NETWORK-ERROR ER
ROR ...) :FORMAT-STRING "Connection to ~1@*~A refused: ~A." :FORMAT-ARGS (#<CHAOS Connection to LMI-DJINN FILE 1 33045347> #FS::LISPM-HOST "LMI-DJINN" "No server for this contact name [LISPM]") :CONNECTION #<CHAOS Connection to LMI-DJINN FILE 1 33045347
> :FOREIGN-HOST #FS::LISPM-HOST "LMI-DJINN"


(:METHOD TV:SCREEN :PARSE-FONT-DESCRIPTOR) (P.C. = 23)
  (SELF is #<TV::STANDARD-SCREEN Main Screen 7440073 exposed>)

 Arg 0 (.OPERATION.): :PARSE-FONT-DESCRIPTOR
 Arg 1 (FD): NIL


FED:DISPLAY-FONT (P.C. = 124)

 Arg 0 (FONT): NIL
 Arg 1 (WINDOW): #<FED::FED-TYPEOUT-WINDOW Fed Typeout Window 2 47127306 exposed>
 Arg 2 (CLEAR-FIRST-P): T
 Arg 3 (FROM-FED): T
 Local 0 (FONT-MAP): NIL
 Local 1 (CURRENT-FONT): NIL
 Local 2 (NAME): NIL
 Local 3 (FD): NIL
 Local 4 (DF): NIL
 Local 5 (CH): NIL
 Local 6 (OCH): NIL
 Local 7 (LEN): NIL
 Local 8 (CH1): NIL


FED::COM-DISPLAY-FONT (P.C. = 62)

   --Defaulted args:--
 Arg 0 (FONT): NIL
 Arg 1 (WINDOW): #<FED::FED-TYPEOUT-WINDOW Fed Typeout Window 2 47127306 exposed>
 Arg 2 (FROM-FED): T
 Arg 3 (CLEAR-FIRST-P): T


(:METHOD FED :COMMAND-LOOP) (P.C. = 130)
  (SELF is #<FED Fed 2 47127070 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): NIL
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): 32


Remainder of stack:

SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Message-ID: <8602252211.AA03452@LMI-CAPRICORN.ARPA>
Date: Tuesday, 25 February 1986, 17:10-EST
From: mrc@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):


Insert your description of the circumstances here:

Set "display scale" to 30 on a rather large, but not super large character.

>>TRAP 11462 (BITBLT-DESTINATION-TOO-SMALL)
The destination of a BITBLT was too small.
Backtrace from the debugger:

(:METHOD FED::GRAY-GRID-MIXIN :REDISPLAY-POINT) (P.C. = 78)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY-POINT
 Arg 1 (I): 26
 Arg 2 (J): 26
 Arg 3 (NEW-VALUE): 2
 Arg 4 (OLD-VALUE): 0


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD FED::GRID-MIXIN :REDISPLAY) (P.C. = 216)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
   --Defaulted args:--
 Arg 1 (FORCE-TO-COMPLETION): NIL
 Local 0 (PLANE-EDGES): (0 -8 17 27)
 Local 1 (J): 26
 Local 2 (I): 26
 Local 3 (OLD-VALUE): 0
 Local 4 (NEW-VALUE): 2


(:METHOD FED :COMBINED :REDISPLAY) (P.C. = 50)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:REDISPLAY)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 37 leader-length 19 27344745>


(:METHOD FED :COMMAND-LOOP) (P.C. = 107)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): T
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): NIL


SI::PROCESS-TOP-LEVEL (P.C. = 113)

   --Defaulted args:--
 Arg 0 (IGNORE): NIL
 Local 0: ("Reset and arrest process ~A." "Fed 1")
 Local 1: (CONDITION ("Reset and arrest process ~A." "Fed 1") T ("Reset and arrest process ~A." "Fed 1") ...)
 Local 2: ("Restart process ~A." "Fed 1")
 Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "Fed 1") T ("Restart process ~A." "Fed 1") ...)



0,, 3, valid,
*** EOOH ***
Message-ID: <8602252202.AA03444@LMI-CAPRICORN.ARPA>
Date: Tuesday, 25 February 1986, 17:01-EST
From: mrc@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):


Insert your description of the circumstances here:

This occurred when I clicked R to get the register menu and then didn't pick anything
from it.

>>ERROR: ECASE failure; the value of (CAR FED::SUBOP), NIL,
         is not EQL one of :CLEAR-REG, :LOAD-REG, :LOAD-REG-GRAY, :LOAD-BLACK, :MERGE-BLACK or :MERGE-GRAY.
Backtrace from the debugger:

(:METHOD FED :REGISTER-CLICK) (P.C. = 173)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Arg 0 (.OPERATION.): :REGISTER-CLICK
 Arg 1 (REGISTER): #<FED::REGISTER-PANE Register Pane 2 46640067 exposed>
 Arg 2 (CLICK): 1048578
 Local 0 (SUBOP): NIL


(:METHOD FED :COMMAND-LOOP) (P.C. = 123)
  (SELF is #<FED Fed 1 46636553 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
 Local 0 (PROMPT-LINE-WAS-USED): NIL
 Local 1 (COMMAND): NIL
 Local 2 (NEXTCH): (:TYPEOUT-EXECUTE :SELECT-CHAR 100)


SI::PROCESS-TOP-LEVEL (P.C. = 113)

   --Defaulted args:--
 Arg 0 (IGNORE): NIL
 Local 0: ("Reset and arrest process ~A." "Fed 1")
 Local 1: (CONDITION ("Reset and arrest process ~A." "Fed 1") T ("Reset and arrest process ~A." "Fed 1") ...)
 Local 2: ("Restart process ~A." "Fed 1")
 Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "Fed 1") T ("Restart process ~A." "Fed 1") ...)



0,, fixed, valid,
*** EOOH ***
Message-ID: <8603072010.AA00861@Ayesha.ARPA>
Date: Tuesday, 25 February 1986, 16:44-EST
From: Ayesha!debbie@AYESHA.ARPA
Sender: Ayesha!SOFTSERV@AYESHA.ARPA
To: BUG-LISPM@AYESHA.ARPA

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 3.0,
microcode 768, site uc, on Morticia:


Loading the following file produces strange results:

If the readtable is zetalisp or if I load it in a lisp listener
I correctly get the error message:
Error: Ship1 is referenced as a free variable, not declared special.

However,  If I load it from an editor typeout window, with readtable common-lisp,
my machine crashes.

;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defstruct ship
  x-pos y-pos x-vel y-vel :mass )
(setq ship1 (make-ship :x-pos 10))





0,, valid, 2,
*** EOOH ***
Message-ID: <8602251722.AA03201@LMI-CAPRICORN.ARPA>
Date: Tuesday, 25 February 1986, 12:18-EST
From: Dave Goodine <dg@LMI-ANGEL>
To: BUG-LISPM@LMI-Angel

In Experimental System 110.29, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.0, Experimental Gateway 2.0,
Experimental Tape 1.5, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.0, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1408,
Alpha-2 Release, on Moe (LAMBDA):


Insert your description of the circumstances here:

Did C-X C-F of the following file when it was already
in the editor.  (looks like it was probing for update info)

>>ERROR: Unknown OPEN option. for ANGEL: /usr/lib/aliases
Backtrace from the debugger:

OPEN-CHAOS (P.C. = 603)

 Arg 0 (ACCESS): #<QFILE-ACCESS Chaos FILE access to LMI-ANGEL 56021671>
 Arg 1 (FILE): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Arg 2 (PATHNAME): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Rest arg (OPTIONS): (:DIRECTION :PROBE)
 Local 1 (DIRECTION): NIL
 Local 2 (CHARACTERS): T
 Local 3 (ERROR): T
 Local 4: (NIL)
 Local 5: (NIL)
 Local 6: (NIL)
 Local 7 (BYTE-SIZE): :DEFAULT
 Local 8 (MOBY-MAPPED): NIL
 Local 9 (ELEMENT-TYPE-P): NIL
 Local 10 (IF-EXISTS-P): NIL
 Local 11 (IF-DOES-NOT-EXIST-P): NIL
 Local 12 (ELEMENT-TYPE): STRING-CHAR
 Local 13 (IF-EXISTS): :NEW-VERSION
 Local 14 (IF-DOES-NOT-EXIST): NIL
 Local 15 (HOST-UNIT): #<QFILE-HOST-UNIT for Chaos FILE access to LMI-ANGEL 56021677>
 Local 16 (DATA-CONN): NIL
 Local 17 (PKT): #<CHAOS packet :STRING "T6423  ERROR UOO C Unknown OPEN option." :STATUS NIL 57326261>
 Local 18 (NOT-ABORTED): T
 Local 19 (PHONY-CHARACTERS): NIL
 Local 20 (SIGN-EXTEND-BYTES): NIL
 Local 21 (.SELECTQ.ITEM.): :UNSPECIFIC
 Local 22: #<CLOSURE (:INTERNAL OPEN-CHAOS MAKE-STREAM) (Lexical environment) 52647633>
 Local 23: #<CLOSURE (:INTERNAL OPEN-CHAOS DATA-CONNECTION-HANDLE) (Lexical environment) 52647637>
 Local 24: ((FILE-NOT-FOUND EH::CONDITION-CASE-THROW G1906))
 Local 25: (FILE-NOT-FOUND EH::CONDITION-CASE-THROW G1906)
 Local 26 (ERROR-OBJECT): #<QFILE-HOST-UNIT for Chaos FILE access to LMI-ANGEL 56021677>
 Local 27 (SUCCESS): NIL
 Local 28 (STRING): " ERROR UOO C Unknown OPEN option."
 Local 29: (FILE-ALREADY-EXISTS EH::CONDITION-CASE-THROW G1913)
 Local 30: ((FILE-ALREADY-EXISTS EH::CONDITION-CASE-THROW G1913) (FILE-NOT-FOUND EH::CONDITION-CASE-THROW G1906))


(:METHOD BASIC-QFILE-ACCESS :OPEN) (P.C. = 26)
  (SELF is #<QFILE-ACCESS Chaos FILE access to LMI-ANGEL 56021671>)

 Arg 0 (.OPERATION.): :OPEN
 Arg 1 (FILE): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Arg 2 (PATHNAME): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Rest arg (OPTIONS): (:DIRECTION :PROBE)


(:METHOD FILE-HOST-MIXIN :ACCESS-OPERATION) (P.C. = 25)
  (SELF is #UNIX-HOST "LMI-ANGEL")

 Arg 0 (.OPERATION.): :ACCESS-OPERATION
 Arg 1 (OP): :OPEN
 Rest arg (ARGS): (#UNIX-PATHNAME "ANGEL: //usr//lib//aliases" #UNIX-PATHNAME "ANGEL: //usr//lib//aliases" :DIRECTION :PROBE)


(:METHOD HOST-PATHNAME :OPEN) (P.C. = 31)
  (SELF is #UNIX-PATHNAME "ANGEL: //usr//lib//aliases")

 Arg 0 (.OPERATION.): :OPEN
 Arg 1 (PATHNAME): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Rest arg (OPTIONS): (:DIRECTION :PROBE)


OPEN (P.C. = 151)

 Arg 0 (FILENAME): #UNIX-PATHNAME "ANGEL: //usr//lib//aliases"
 Rest arg (KEYWORD-ARGS): (:DIRECTION :PROBE)
 Local 1: ((FILE-ERROR) SI::FILE-RETRY-HANDLER #UNIX-PATHNAME "ANGEL: //usr//lib//aliases" G9268)
 Local 2: ((# SI::FILE-RETRY-HANDLER #UNIX-PATHNAME "ANGEL: //usr//lib//aliases" G9268))
 Local 3 (KEYL): NIL
 Local 4 (KEY): NIL
 Local 5 (CHARACTERS): NIL
 Local 6 (DIRECTION): NIL
 Local 7 (BYTE-SIZE): NIL
 Local 8 (ERRORP): NIL
 Local 9 (ERRORP-SPECD): NIL
 Local 10 (DELETED-P): NIL
 Local 11 (TEMPORARY-P): NIL
 Local 12 (RAW-P): NIL
 Local 13 (SUPER-IMAGE-P): NIL


Remainder of stack:

ZWEI::FIND-FILE (P.C. = 180)
ZWEI::COM-FIND-FILE (P.C. = 39)
ZWEI::RE-EXECUTE-MINI-BUFFER-COMMAND (P.C. = 60)
ZWEI::MINI-BUFFER-HISTORY-YANK (P.C. = 30)
ZWEI::HISTORY-YANK (P.C. = 86)
ZWEI::COM-REPEAT-MINI-BUFFER-COMMAND (P.C. = 20)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, fixed, valid,
*** EOOH ***
Message-ID: <8602251611.AA03062@LMI-CAPRICORN.ARPA>
Date: Tuesday, 25 February 1986, 11:11-EST
From: naha@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Test Lambda A (LAMBDA):


Insert your description of the circumstances here:

I typed System-U and UNIX was in single user mode.  This is the error
I got instead of the one I should have gotten.

>>ERROR: UNIX::NO-UNIX-STREAM is not a known condition flavor or signal name
Backtrace from the debugger:

MAKE-CONDITION (P.C. = 44)

 Arg 0 (SIGNAL-NAME): UNIX::NO-UNIX-STREAM
 Rest arg (ARGS): ("couldn't find unix stream with~:[out~] login" T)


FERROR (P.C. = 42)

 Arg 0 (SIGNAL-NAME): UNIX::NO-UNIX-STREAM
 Arg 1 (FORMAT-STRING): "couldn't find unix stream with~:[out~] login"
 Rest arg (ARGS): (T)


Additional information supplied with call:
 Expecting 2 values

UNIX::FIND-UNIX-STREAM (P.C. = 63)

 Arg 0 (WITH-LOGIN-P): T
 Local 0 (STR): NIL
 Local 1: ("Try again.")
 Local 2: (UNIX::NO-UNIX-STREAM NIL T ("Try again.") ...)
 Local 3: ((UNIX::NO-UNIX-STREAM NIL T # ...) (# # T # ...) (CONDITION # T # ...))


UNIX::SIMPLE-UNIX-TYPEIN-TOP-LEVEL (P.C. = 84)

 Arg 0 (WINDOW): #<UNIX::SIMPLE-UNIX-WINDOW Simple Unix Window 1 37100000 exposed>
 Local 0 (PATHNAME): NIL
 Local 1 (PORT-NUMBER): NIL


SI::PROCESS-TOP-LEVEL (P.C. = 113)

   --Defaulted args:--
 Arg 0 (IGNORE): NIL
 Local 0: ("Reset and arrest process ~A." "Simple Unix Stream Typein")
 Local 1: (CONDITION ("Reset and arrest process ~A." "Simple Unix Stream Typein") T ("Reset and arrest process ~A." "Simple Unix Stream Typein") ...)
 Local 2: ("Restart process ~A." "Simple Unix Stream Typein")
 Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "Simple Unix Stream Typein") T ("Restart process ~A." "Simple Unix Stream Typein") ...)



0,, fixed, valid,
*** EOOH ***
Message-ID: <8602250203.AA01504@Ayesha.ARPA>
Date: Monday, 24 February 1986, 21:01-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
andover site, on Cousin It (LAMBDA):


Insert your description of the circumstances here:

Selected 'sort' in Zmail.

>>TRAP 7986 (ARGTYP NUMBER PP 0 QILSP)
The first argument to SYSTEM:M-<, "TIME:PARSE-ERROR is not a known condition flavor or signal name", was of the wrong type.
The function expected a number.
Backtrace from the debugger:

ZWEI::MSG-DATE-SORT-LESSP (P.C. = 31)

 Arg 0 (MSG-1): #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...)
 Arg 1 (MSG-2): #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156501> :INTERVAL ...)


SI::SORT-SHORT-LIST (P.C. = 46)

 Arg 0 (L): (#S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156501> :INTERVAL ...) #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...))
 Arg 1 (LPRED): ZWEI::MSG-DATE-SORT-LESSP
 Arg 2 (KEYFUN): NIL
 Local 0 (I): 1
 Local 1 (SWITCH): T
 Local 2 (LP): (#S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156501> :INTERVAL ...) #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...))


SORT (P.C. = 65)

 Arg 0 (X): (#S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156501> :INTERVAL ...) #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...))
 Arg 1 (SORT-LESSP-PREDICATE): ZWEI::MSG-DATE-SORT-LESSP
 Rest arg: (:KEY NIL)
 Local 1 (KEY): NIL
 Local 2 (TEM): NIL


SI::SORT-ARRAY-STABLE (P.C. = 94)

 Arg 0 (A): #<ART-Q 64 fill-pointer 2 47557607>
 Arg 1 (LESSP-PREDICATE): ZWEI::MSG-DATE-SORT-LESSP
 Arg 2 (KEY): NIL
 Local 0 (TEMP): #(#S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156501> :INTERVAL ...) (#S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...)) #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 24156640> :INTERVAL ...) NIL)
 Local 1 (J): 1
 Local 2 (I): 2
 Local 3 (LEN): 2


STABLE-SORT (P.C. = 65)

 Arg 0 (X): #<ART-Q 64 fill-pointer 2 47557607>
 Arg 1 (LESSP-PREDICATE): ZWEI::MSG-DATE-SORT-LESSP
 Rest arg: NIL
 Local 1 (KEY): NIL
 Local 2 (TEM): NIL


Remainder of stack:

ZWEI::SORT-ZMAIL-BUFFER (P.C. = 148)
ZWEI::COM-ZMAIL-SORT (P.C. = 112)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, issues, 3, valid,
*** EOOH ***
Date: Monday, 24 February 1986, 19:44-EST
From: Keith Corbett <Ayesha!keith@ayesha.ARPA>
To: BUG-LISPM@AYESHA.ARPA
Message-ID: <[COUSIN-IT].24-Feb-86 19:44:23.keith>

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
andover site, on Cousin It (LAMBDA):

Old bug, still in 3.0, in ZMail.

When 'Get New Mail' finds so much as one new message in my Unix
inbox, it copies *all* my previously found new msgs back in...

  -- Keith



0,, fixed,
*** EOOH ***
Date: Monday, 24 February 1986, 16:55-EST
From: Debbie Ellerin <Ayesha!debbie@angel>
Message-ID: <8602242155.AA01042@Ayesha.ARPA>
To: bug-lispm@angel

;;; -*- Mode:LISP; Package:USER; Base:10 -*-


--Text Follows This Line--
In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
andover site, on Thing (LAMBDA):


Insert your description of the circumstances here:

This 2.0 bug is still broken in 3.0  .
It works evaluated but not compiled.

(defun test-multiple-choose ()
  (let ((item-list '((:eat "eat" (:add :make-permanent))
                     ( :drink "drink" (:add :make-permanent))))
        (keyword-alist  '((:add "add"  nil   t nil nil)
                        (:make-permanent "make permanent" nil t nil nil  ))))
    (tv:multiple-choose "*wd*" item-list keyword-alist )))



>>TRAP 12106 (WRITE-IN-READ-ONLY VMA)
There was an attempt to write into #o34522065, which is a read-only address.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 3 values

(:METHOD TV:BASIC-MULTIPLE-CHOICE :SET-CHOICES) (P.C. = 79)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 12441570 deactivated>)

 Arg 0 (.OPERATION.): :SET-CHOICES
 Arg 1 (NEW-CHOICES): ((:EAT "eat" #) (:DRINK "drink" #))
 Local 0 (NAME-LENGTH): NIL
 Local 1 (CHOICE-BOXES): NIL
 Local 2 (MAX-X): NIL
 Local 3 (NITEMS): NIL
 Local 4 (NEW-LABEL): NIL
 Local 5 (ALLTYPES): (:ADD :MAKE-PERMANENT)
 Local 6: ((:ADD "add" NIL T ...) (:MAKE-PERMANENT "make permanent" NIL NIL ...))
 Local 7 (CHOICE-TYPE): (:ADD "add" NIL T ...)
 Local 8 (CHOICE): NIL
 Local 9 (X): NIL
 Local 10 (TYPES): NIL
 Local 11 (TYPE): NIL
 Local 12 (TYPE-WIDTH): NIL
 Local 13 (MAXIMUM-POSSIBLE-MAX-X): NIL
 Local 14 (BOX): NIL
 Local 15: NIL
 Local 16 (I): NIL
 Local 17 (LIM): NIL
 Local 18 (CHOICES): NIL
 Local 19 (MAX-NAME-CHARS): NIL
 Local 20 (CHOICE-ITEM): NIL
 Local 21 (BOXES): NIL
 Local 22 (INITIAL-STATE): NIL


(:METHOD TV:BASIC-MULTIPLE-CHOICE :SETUP) (P.C. = 90)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 12441570 deactivated>)

 Arg 0 (.OPERATION.): :SETUP
 Arg 1 (NEW-ITEM-NAME): "*wd*"
 Arg 2 (NEW-CHOICE-TYPES): ((:ADD "add" NIL T ...) (:MAKE-PERMANENT "make permanent" NIL NIL ...))
 Arg 3 (NEW-FINISHING-CHOICES): (("Do It" NIL TV::MULTIPLE-CHOICE-DONE 50 ...) ("Abort" NIL TV::MULTIPLE-CHOICE-ABORT 154 ...))
 Arg 4 (NEW-CHOICES): ((:EAT "eat" #) (:DRINK "drink" #))
 Arg 5 (MAXLINES): 20
 Local 0 (WID): NIL
 Local 1 (HGT): NIL
 Local 2 (LBL): NIL
 Local 3 (.OLD.OUTPUT.HOLD.): NIL


TV:MULTIPLE-CHOOSE (P.C. = 119)

 Arg 0 (ITEM-NAME): "*wd*"
 Arg 1 (ITEM-LIST): ((:EAT "eat" #) (:DRINK "drink" #))
 Arg 2 (KEYWORD-ALIST): ((:ADD "add" NIL T ...) (:MAKE-PERMANENT "make permanent" NIL NIL ...))
   --Defaulted args:--
 Arg 3 (NEAR-MODE): (:MOUSE)
 Arg 4 (MAXLINES): 20
 Arg 5 (SUP): #<TV::STANDARD-SCREEN Main Screen 10440073 exposed>
 Local 0 (X): (:MAKE-PERMANENT "make permanent" NIL NIL ...)
 Local 1: NIL
 Local 2 (L): NIL
 Local 3 (WINDOW): #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 12441570 deactivated>


TEST-KRALL (P.C. = 30) (from file TISH: DEBBIE; KR-TEST.#)

 Local 0 (ITEM-LIST): ((:EAT "eat" #) (:DRINK "drink" #))
 Local 1 (KEYWORD-ALIST): ((:ADD "add" NIL T ...) (:MAKE-PERMANENT "make permanent" NIL NIL ...))


SYSTEM:EVAL1 (P.C. = 538)

 Arg 0 (FORM): (TEST-KRALL)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): 0
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER TEST-KRALL 34522023>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER TEST-KRALL 34522023>
 Local 5 (ARG-DESC): 0
 Local 6 (NUM-ARGS): 0
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ADL): NIL
 Local 11 (ITEM): NIL
 Local 12 (.SELECTQ.ITEM.): NIL


Remainder of stack:

PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 538)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
BREAK (P.C. = 394)
ZWEI::COM-BREAK (P.C. = 30)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, issues, valid, 4,
*** EOOH ***
Message-ID: <8602242015.AA01604@LMI-CAPRICORN.ARPA>
Date: Monday, 24 February 1986, 15:14-EST
From: mrc@LMI-ANGEL
Sender: %LMI-ANGEL@angel
To: BUG-LISPM@LMI-Angel

In Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):

The mouse documentation line on a newly booted machine ought to say
"Click right to get the system menu"
Currently it is blank.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Sunday, 23 February 1986, 20:55-EST
From: rjpi@LMI-ANGEL
Sender: Ingria@LMI-ANGEL
Subject: File Options Menus
To: BUG-ZMail@LMI-Angel
Message-ID: <[LMI-NATASHA].23-Feb-86 20:55:49.Ingria>

In ZMAIL in Experimental System 110.47, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.2, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):

        The Choose-Variable-Values windows you get on ZMail when you
create a new mail file have the following weird behavior:

the first menu that pops up will remain until you click on ``Do It'' or
``Abort''; however, if you change the format of the file, the second
menu will just pop up and then disappear, without giving you a chance to
confirm or abort.



0,, fixed, 3, indeterminate,
*** EOOH ***
Date: Sunday, 23 February 1986, 19:26-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Too Small Temporary Windows
To: BUG-LISPM@LMI-Angel
In-reply-to: <8602210552.AA04130@LMI-CAPRICORN.ARPA>
Message-ID: <[LMI-NATASHA].23-Feb-86 19:26:12.Ingria>


    Date: Friday, 21 February 1986, 00:55-EST
    From: Bob Powell <bobp@LMI-CAPRICORN>

    In Experimental System 110.54, Experimental ZMail 65.3,
    Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
    Experimental MagTape 4.1, Experimental FILE-Server 18.1, microcode 1371,
    SDU Newboot 227, SDU ROM 103, Nifty+, on Poindexter (LAMBDA):

    On a portrait monitor, <terminal>-G appears in a window too narrow
    to show the right-most column.  It is fine on a landscape.
    Either it is just messed up now for portrait, or perhaps the
    window got too small when it was scaled to landscape and then
    back to portrait as this band moved around.


It seems that <TERMINAL> commands, in general, that pop up temporary
windows make them too small on portrait monitors; look at, for example,
<TERMINAL> <HELP>, F, N, etc.




0,, valid, 4,
*** EOOH ***
Message-ID: <8602220305.AA00683@LMI-CAPRICORN.ARPA>
Date: Friday, 21 February 1986, 22:04-EST
From: Pace Willisson <pace@cap>
To: BUG-LISPM@LMI-Angel

In Experimental System 110.64, Experimental ZMail 65.3,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1,
Experimental ObjectLISP 2.0, Experimental IMicro 1.0, microcode 1371,
SDU Newboot 227, SDU ROM 102, paint/obl, on Lene Lovich (LAMBDA):


Insert your description of the circumstances here:

Here is the current arglist for compile-file:
  (COMPILER::INPUT-FILE &KEY COMPILER::OUTPUT-FILE (COMPILER::SET-DEFAULT-PATHNAME T) LOAD :PACKAGE)
As you can see, it is compatable with common lisp, but not zetalisp.
Pace

Note: compile-file now (5/9/86) accepts :output-file or :output-filename, but
the documentation string needs to be updated.



>>ERROR: Keyword arg keyword :OUTPUT-FILENAME unrecognized.
Backtrace from the debugger:

COMPILE-FILE (P.C. = 81)

 Arg 0 (INPUT-FILE): #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >"
 Rest arg: (:OUTPUT-FILENAME #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO QFASL" :PACKAGE NIL)
 Local 1 (OUTPUT-FILE): NIL
 Local 2 (SET-DEFAULT-PATHNAME): T
 Local 3 (LOAD): NIL
 Local 4 (PACKAGE-SPEC): NIL
 Local 5 (FILE): NIL
 Local 6: NIL
 Local 7: NIL
 Local 8: NIL
 Local 9: NIL
 Local 10 (RESULT): NIL


SI::QC-FILE-1 (P.C. = 42)

 Arg 0 (INFILE): #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >"
 Arg 1 (OUTFILE): #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO QFASL >"


SI::DO-FILE-TRANSFORMATIONS (P.C. = 171)

 Local 0: ((:PENDING # NIL #<SI::SYSTEM IMicro 61506742> ...) (:NOT-NEEDED # NIL #<SI::SYSTEM IMicro 61506742> ...) (:PROBABLY # NIL #<SI::SYSTEM IMicro 61506742> ...) (:PENDING # NIL #<SI::SYSTEM IMicro 61506742> ...) ...)
 Local 1 (FILE-TRANSFORMATION): (:PENDING (:COMPILE # SI::QC-FILE-1 # ...) NIL #<SI::SYSTEM IMicro 61506742> ...)
 Local 2 (STATE): :PENDING
 Local 3 (TYPE): (:COMPILE ("Compile" "Compiling" "compiled") SI::QC-FILE-1 (:LISP) ...)
 Local 4 (ARGS): (#FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO QFASL >")
 Local 5: ("Give up ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >")
 Local 6: (ERROR ("Give up ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >") T ("Give up ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >") ...)
 Local 7: ("Retry ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >")
 Local 8: (ERROR ("Retry ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >") T ("Retry ~(~A~) ~A." "Compiling" #FS::LOGICAL-PATHNAME "SYS: IMICRO; IMICRO LISP >") ...)
 Local 9 (PATHNAME): NIL
 Local 10 (FILE-XFORM): NIL
 Local 11 (L): NIL
 Local 12 (TAIL): NIL


SI::PERFORM-TRANSFORMATIONS (P.C. = 81)

 Arg 0 (TRANSFORMATION-LIST): ((#<TRANSFORMATION COMPILE 61507416> NIL NIL) (#<TRANSFORMATION COMPILE 61507441> NIL NIL) (#<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464> NIL NIL))
 Local 0: NIL
 Local 1: #<DTP-LOCATIVE 10040337>
 Local 2 (ELEM): (#<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464> NIL NIL)
 Local 3: NIL
 Local 4 (XFORM): #<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464>
 Local 5 (PKG): NIL
 Local 6 (FORCE): NIL
 Local 7 (INPUT): NIL
 Local 8 (INPUTS): NIL


SI::PERFORM-TRANSFORMATIONS (P.C. = 62)

 Arg 0 (TRANSFORMATION-LIST): ((#<TRANSFORMATION READFILE 61507131> NIL NIL) (#<TRANSFORMATION FASLOAD 61507407> NIL NIL) (#<TRANSFORMATION FASLOAD 61507432> NIL NIL) (#<TRANSFORMATION INCREMENT-LOADED-VERSION 61507455> NIL NIL))
 Local 0: ((#<TRANSFORMATION COMPILE 61507416> NIL NIL) (#<TRANSFORMATION COMPILE 61507441> NIL NIL) (#<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464> NIL NIL))
 Local 1: ((#<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464> NIL NIL))
 Local 2 (ELEM): (#<TRANSFORMATION INCREMENT-LOADED-VERSION 61507455> NIL NIL)
 Local 3: NIL
 Local 4 (XFORM): #<TRANSFORMATION INCREMENT-LOADED-VERSION 61507455>
 Local 5 (PKG): NIL
 Local 6 (FORCE): NIL
 Local 7 (INPUT): #<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464>
 Local 8 (INPUTS): ((#<TRANSFORMATION COMPILE 61507416> NIL NIL) (#<TRANSFORMATION COMPILE 61507441> NIL NIL) (#<TRANSFORMATION INCREMENT-COMPILED-VERSION 61507464> NIL NIL))


Remainder of stack:

MAKE-SYSTEM (P.C. = 155)
SYSTEM:EVAL1 (P.C. = 538)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)
SI:LISP-TOP-LEVEL (P.C. = 39)



0,, fixed,
*** EOOH ***
Message-ID: <8602212143.AA00248@LMI-CAPRICORN.ARPA>
Date: Friday, 21 February 1986, 16:38-EST
From: naha@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.37, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental MagTape 4.1,
Experimental ZMail 65.3, Experimental Unix-Interface 9.0, microcode 1371,
A-2 for CS, on Test Lambda A (LAMBDA):


Insert your description of the circumstances here:

I clicked right for the system menu then selected split-screen.
clicked "lisp" "edit" "frame" "doit" then got this:

see further comments after backtrace

>>TRAP 7989 (ARGTYP NUMBER M-T 1 QILSP0)
The second argument to <, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

(:METHOD TV:BASIC-CHOOSE-VARIABLE-VALUES :SET-VARIABLES) (P.C. = 117)
  (SELF is #<TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW Split Screen Choose Values 37231141 deactivated>)

 Arg 0 (.OPERATION.): :SET-VARIABLES
 Arg 1 (ELEMS): ((TV::*FRAME-NAME* "Name of frame" :STRING) (TV::*SYSTEM-KEY* "[SYSTEM] <char> selects it" :CHARACTER-OR-NIL))
 Arg 2 (NO-SET-HEIGHT): T
   --Defaulted args:--
 Arg 3 (-WIDTH-): NIL
 Arg 4 (EXTRA-WIDTH): NIL
 Local 0 (NELEM): 2
 Local 1: NIL
 Local 2 (ELEM): (TV::*SYSTEM-KEY* "[SYSTEM] <char> selects it" :CHARACTER-OR-NIL)
 Local 3 (DESIRED-HEIGHT): 28


(:METHOD TV:BASIC-CHOOSE-VARIABLE-VALUES :AFTER :INIT) (P.C. = 30)
  (SELF is #<TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW Split Screen Choose Values 37231141 deactivated>)

 Arg 0 (.OPERATION.): :INIT
 Arg 1 (PLIST): #<DTP-LOCATIVE 36650525>
 Local 0 (ELEMS): ((TV::*FRAME-NAME* "Name of frame" :STRING) (TV::*SYSTEM-KEY* "[SYSTEM] <char> selects it" :CHARACTER-OR-NIL))


(:INTERNAL (:METHOD TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW :COMBINED :INIT) 0) (P.C. = 143)

 Rest arg (.DAEMON-CALLER-ARGS.): (:INIT #<DTP-LOCATIVE 36650525>)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 6 leader-length 16 7547523>


(:METHOD TV:SHEET :INVERSE-AROUND :INIT) (P.C. = 96)
  (SELF is #<TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW Split Screen Choose Values 37231141 deactivated>)

 Arg 0 (.OPERATION.): :INIT
 Arg 1 (CONT): #<DTP-FEF-POINTER (:INTERNAL (:METHOD TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW :COMBINED :INIT) 0) 3651170>
 Arg 2 (MT): #<ART-16B 6 leader-length 16 7547523>
 Arg 3 (ARGS): (:INIT #<DTP-LOCATIVE 36650525>)
 Arg 4 (INIT-PLIST): #<DTP-LOCATIVE 36650525>
 Local 0 (.QUEUE-LEFT.): T
 Local 1 (.OLD.OUTPUT.HOLD.): NIL
 Local 2 (EXPOSE-P): NIL
 Local 3: NIL
 Local 4 (E): NIL


(:METHOD TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW :COMBINED :INIT) (P.C. = 39)
  (SELF is #<TV:TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW Split Screen Choose Values 37231141 deactivated>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:INIT #<DTP-LOCATIVE 36650525>)
 Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B 6 leader-length 16 7547523>


Remainder of stack:

INSTANTIATE-FLAVOR (P.C. = 335)
TV:MAKE-WINDOW (P.C. = 22)
(:PROPERTY TV::SPLIT-SCREEN-CHOOSE-VALUES SI::RESOURCE-CONSTRUCTOR) (P.C. = 74)
ALLOCATE-RESOURCE (P.C. = 260)
TV::SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS (P.C. = 409)
(:METHOD TV:MENU-EXECUTE-MIXIN :EXECUTE) (P.C. = 108)
(:METHOD TV:MOMENTARY-MENU :COMBINED :EXECUTE) (P.C. = 42)
(:METHOD TV:BASIC-MENU :CHOOSE) (P.C. = 52)
(:INTERNAL (:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) 0) (P.C. = 60)
(:METHOD TV:BASIC-MOMENTARY-MENU :AROUND :CHOOSE) (P.C. = 50)
(:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) (P.C. = 39)
(:INTERNAL TV:MOUSE-CALL-SYSTEM-MENU 0) (P.C. = 34)
SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 64)
SI::PROCESS-TOP-LEVEL (P.C. = 113)


 ***************** THE OFFENDING FUNCTION: ****************

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SET-VARIABLES) (ELEMS &OPTIONAL NO-SET-HEIGHT
                                                          -WIDTH- EXTRA-WIDTH
                                                          &AUX (NELEM (LENGTH ELEMS)))
  (SETQ TOP-ITEM 0)                     ;Unscroll
  (AND (< (ARRAY-LENGTH ITEMS) NELEM)
       (SETQ ITEMS (ADJUST-ARRAY-SIZE ITEMS NELEM)))
  (SETF (ARRAY-LEADER ITEMS 0) 0)
  (DOLIST (ELEM ELEMS)
    (ARRAY-PUSH ITEMS ELEM))
  ;; -WIDTH- can be a string, a number of chars, or T meaning look at the variable specs.
  (COND ((STRINGP -WIDTH-)
         (SETQ -WIDTH- (SEND SELF :STRING-LENGTH -WIDTH-)))
        ((NUMBERP -WIDTH-)
         (SETQ -WIDTH- (* (SHEET-CHAR-WIDTH SELF) -WIDTH-)))
        ((EQ -WIDTH- T)
         (SETQ -WIDTH- (SEND SELF :APPROPRIATE-WIDTH EXTRA-WIDTH))))
  (SETQ NELEM (LENGTH ITEMS))
  (LET ((DESIRED-HEIGHT (* (MIN 25. NELEM) LINE-HEIGHT)))
    (AND (or ( (SHEET-INSIDE-HEIGHT) DESIRED-HEIGHT)
;;; ***************** AND THE OFFENDING LINE OF CODE: ****************
             (< (sheet-inside-width) -width-)) ;maybe should be  ... wasn't in at all before
         (NOT NO-SET-HEIGHT)
         (SEND SELF :ADJUSTABLE-SIZE-P)
         ( (+ DESIRED-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
            (SHEET-INSIDE-HEIGHT SUPERIOR))
         (SEND SELF :SET-INSIDE-SIZE (OR -WIDTH- (SHEET-INSIDE-WIDTH)) DESIRED-HEIGHT))
    (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)
    (SEND SELF :SET-ITEMS ITEMS)))




0,, fixed, valid,
*** EOOH ***
Date: Friday, 21 February 1986, 13:38-EST
From: Debbie Ellerin <Ayesha!debbie@angel>
Message-ID: <8602211838.AA02756@Ayesha.ARPA>
To: bug-lispm@angel

Trying to create a zwei:editor-top-level window doesn't work.
It can be instantiated, but when I try to select it I get an
error in the window system (I can't get a backtrace of it - sorry.)



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8602210457.AA04087@LMI-CAPRICORN.ARPA>
Date: Friday, 21 February 1986, 00:00-EST
From: pace@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Experimental System 110.29, Experimental ZMail 65.3,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1, microcode 1368,
Nifty, on David Byrne (LAMBDA):

The funciton mail does not work from lisp if it is worried that a
lisp machine can't appear in the from field.



0,, fixed, valid,
*** EOOH ***
Message-ID: <8602200252.AA02978@LMI-CAPRICORN.ARPA>
Date: Wednesday, 19 February 1986, 22:07-EST
From: chuck@LMI-CAPRICORN
To: BUG-LISPM@LMI-Angel

In Don't-dump-a-band! Experimental System 110.36,
Experimental Local-File 66.0, Experimental FILE-Server 18.1,
Experimental ObjectLISP 1.0, Experimental Site-Editor 1.1,
Experimental Gateway 2.0,
Inconsistent (unreleased patches loaded) Tape 1.7,
Experimental Tiger 23.0, Experimental Lambda-Diag 3.0,
Experimental KERMIT 31.1, Experimental TCP-Kernel 39.1,
Experimental TCP-User 62.1, Experimental TCP-Server 45.2,
Experimental Unix-Interface 9.0, Experimental ZMail 65.3,
Experimental Window-Maker 1.0, microcode 1368, Alpha-1 Release,
on Djinn (LAMBDA):


Insert your description of the circumstances here:

This should not die:  (y-or-n-p "")

-chuck kollar
-carnegie group inc (CGI)


>>TRAP 6009 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS) M-ARRAY-POINTER)
The subscript -1 for "" was out of range in SYSTEM:COMMON-LISP-AR-1.
Backtrace from the debugger:

Y-OR-N-P (P.C. = 30)

 Arg 0 (FORMAT-STRING): ""
 Rest arg (FORMAT-ARGS): NIL


SYSTEM:EVAL1 (P.C. = 538)

 Arg 0 (FORM): (Y-OR-N-P "")
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): 1
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER Y-OR-N-P 3047425>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER Y-OR-N-P 3047425>
 Local 5 (ARG-DESC): 1048577
 Local 6 (NUM-ARGS): 1
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ADL): NIL
 Local 11 (ITEM): NIL
 Local 12 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (Y-OR-N-P "")
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)

 Arg 0 (TOP-LEVEL-FORM): (Y-OR-N-P "")
 Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
 Local 1: ((# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 238)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 7440000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
 Local 0 (OLD-PACKAGE): #<Package USER 7122113>
 Local 1 (W-PKG): #<Package USER 7122113>
 Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 5042370>
 Local 3 (THROW-FLAG): T
 Local 4: ("Return to top level in ~A." "Lisp Listener 1")
 Local 5: ((SYSTEM:ABORT EH:DEBUGGER-CONDITION) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
 Local 6 (VALUES): NIL
 Local 7: NIL
 Local 8 (VALUE): NIL


Remainder of stack:

SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)
SI:LISP-TOP-LEVEL (P.C. = 39)



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8602180454.AA00882@LMI-CAPRICORN.ARPA>
Date: Monday, 17 February 1986, 23:53-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: YAERIAMF (Yet Another Error Reading in a Mail File)
To: BUG-ZMail@LMI-Angel

In Experimental System 110.49, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):


Insert your description of the circumstances here:

Got this by trying to move a message to cap:/lmi/rjpi/mail/bug-zwei.

>>TRAP 6893 (ARGTYP ARRAY M-ARRAY-POINTER 0 (DECODE-1D-ARRAY-RESTART RESTORE-ARRAY-REGISTERS))
The first argument to AS-1, ("From dg@LMI-ANGELDate: Saturday, 25 January 1986, 14:37-EST" 0 :NORMAL), was of the wrong type.
The function expected an array.
Backtrace from the debugger:

ZWEI::CREATE-INTERVAL (P.C. = 85)

 Arg 0 (ARG1): ("From dg@LMI-ANGELDate: Saturday, 25 January 1986, 14:37-EST" 0 :NORMAL)
 Arg 1 (ARG2): ("From ingria%LMI-CAPRICORN@LMI-NOSFERATU, rjpi@LMI-CAPRICORNDate: Monday, 27 January 1986, 02:21-EST" 0 :MOVES)
   --Defaulted args:--
 Arg 2 (NODE-P): T
 Local 0 (INTERVAL): #<ZWEI::NODE 60331400>
 Local 1 (*BATCH-UNDO-SAVE*): T
 Local 2 (LINE): NIL


(:METHOD ZWEI::ZMAIL-DISK-BUFFER :READ-NEXT-MSG) (P.C. = 326)
  (SELF is #<ZWEI::UNIX-MAIL-FILE-BUFFER bug-zwei /lmi/rjpi/mail/ CAP: 13100072>)

 Arg 0 (.OPERATION.): :READ-NEXT-MSG
 Arg 1 (NMSGS): 65448
 Local 0 (EOF): NIL
 Local 1: #<DTP-LOCATIVE 13100111>
 Local 2: T
 Local 3 (START): "From dg@LMI-ANGELDate: Saturday, 25 January 1986, 14:37-EST"
 Local 4 (LINE-WAITING-FLAG): NIL
 Local 5 (TEST-FUNCTION): #<DTP-FEF-POINTER (:METHOD ZWEI::UNIX-MAIL-FILE-MIXIN :LINE-END-OF-MSG-P) 37277033>
 Local 6 (END-LINE): ""
 Local 7 (LINE): "From ingria%LMI-CAPRICORN@LMI-NOSFERATU, rjpi@LMI-CAPRICORNDate: Monday, 27 January 1986, 02:21-EST"
 Local 8 (LENGTH): 99
 Local 9 (END-IDX): :START-NEXT
 Local 10 (MSG-REAL-START-BP): ("From dg@LMI-ANGELDate: Saturday, 25 January 1986, 14:37-EST" 0 :NORMAL)
 Local 11 (STATE): T
 Local 12 (I): 0
 Local 13 (MSG-REAL-INTERVAL): #<ZWEI::NODE 60330277>
 Local 14 (MSG-INTERVAL): #<ZWEI::NODE 60330312>
 Local 15: #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 60330277> :INTERVAL ...)
 Local 16 (LINE): ""
 Local 17 (LAST): #<ZWEI::NODE 60327604>
 Local 18 (INFS): (#<ZWEI::NODE 60145130> #<ZWEI::NODE 60150450> #<ZWEI::NODE 60151457> #<ZWEI::NODE 60152500> ...)
 Local 19 (LAST-BP-0): ("From rjpi@LMI-CAPRICORNDate: Friday, 24 January 1986, 20:35-EST" 0 :MOVES)
 Local 20 (LAST-BP-1): ("From rjpi@LMI-CAPRICORNDate: Friday, 24 January 1986, 20:35-EST" 0 :MOVES)


ZWEI::LOAD-ALL-MSGS (P.C. = 32)

 Arg 0 (ZMAIL-BUFFER): #<ZWEI::UNIX-MAIL-FILE-BUFFER bug-zwei /lmi/rjpi/mail/ CAP: 13100072>
 Local 0 (MAIL-FILE-BUFFER): NIL


ZWEI::START-LOADING-ZMAIL-BUFFER (P.C. = 65)

 Arg 0 (ZMAIL-BUFFER): #<ZWEI::UNIX-MAIL-FILE-BUFFER bug-zwei /lmi/rjpi/mail/ CAP: 13100072>
 Arg 1 (STREAM): #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//bug-zwei" 57150601>
 Arg 2 (BACKGROUND-P): NIL
   --Defaulted args:--
 Arg 3 (TRUENAME): NIL
 Local 0: NIL
 Local 1: NIL


ZWEI::GET-ZMAIL-FILE (P.C. = 128)

 Arg 0 (STREAM): #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//bug-zwei" 57150601>
 Arg 1 (PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//rjpi//mail//bug-zwei"
 Arg 2 (BACKGROUND-P): NIL
 Arg 3 (FLAVOR): ZWEI::UNIX-MAIL-FILE-BUFFER
 Local 0 (INFO): (#FS::UNIX-PATHNAME "CAP: //lmi//rjpi//mail//bug-zwei" . 2717275094)
 Local 1 (SUCCESS): NIL
 Local 2 (ZMAIL-BUFFER): #<ZWEI::UNIX-MAIL-FILE-BUFFER bug-zwei /lmi/rjpi/mail/ CAP: 13100072>
 Local 3: NIL
 Local 4: NIL
 Local 5 (APPEND-P): T


Remainder of stack:

ZWEI::ZMAIL-FIND-FILE-NOSELECT (P.C. = 90)
ZWEI::GET-MOVE-ZMAIL-BUFFER (P.C. = 302)
ZWEI::GET-DEFAULTED-MOVE-ZMAIL-BUFFER (P.C. = 66)
ZWEI::COM-ZMAIL-MOVE (P.C. = 48)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 170)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 17 February 1986, 23:41-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Another LEXEME Bug
To: rpk@LMI-CAPRICORN
CC: BUG-ZMail@LMI-Angel
In-reply-to: <[LMI-DAVID-BYRNE].8-Feb-86 20:15:39.RpK>
Message-ID: <[LMI-NATASHA].17-Feb-86 23:41:53.Ingria>

    Date: Saturday, 8 February 1986, 20:15-EST
    From: Robert P. Krajewski <rpk@LMI-CAPRICORN>

        Date: Saturday, 8 February 1986, 17:33-EST
        From: rjpi@LMI-CAPRICORN

        In Experimental System 109.99, Experimental Local-File 64.1,
        Experimental FILE-Server 17.1, Experimental MagTape 3.5,
        Experimental ZMail 64.2, Experimental Unix-Interface 7.1,
        Experimental Tiger 22.0, microcode 1361, (with Flexi-Screen!),
        on Boris Badinoff (LAMBDA):

        Got this by reading in a mail file.

    OK, looks like a bug -- what is the mail file ?  (Can't tell from the
    backtrace.)

cap:/lmi/rjpi/mail/bug-wsm.

    You can see that what's actually happening here is that ZMail thought it
    was reading a header line -- parens are one way of delimited things to
    be ignored in a address header line.

I belive this is in a message that had no Subject: field.  I believe
that the LEXEME bugs I've seen before have all involved messages without
Subject: fields.



0,, valid, 4, indeterminate,
*** EOOH ***
Message-ID: <8602180433.AA00842@LMI-CAPRICORN.ARPA>
Date: Monday, 17 February 1986, 23:31-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Schmutz in the Who Line
To: BUG-LISPM@LMI-Angel

In Experimental System 110.49, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Natasha Nogoodnik (LAMBDA):

        I have noticed on various occasions that one or two characters
appear in the file state sheet.  I haven't noticed exactly under what
conditions this happens.  (It happened just now after I logged in and
loaded patches; "es" appeared in the file state sheet.)  I thought I
would mention this, since I find it disconcerting having random crud in
the who line and I'm sure our customers will, too.



0,, fixed, valid,
*** EOOH ***
Message-ID: <8602180309.AA00773@LMI-CAPRICORN.ARPA>
Date: Monday, 17 February 1986, 21:50-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: More Twiddle Twaddle
To: BUG-LISPM@LMI-Angel

In Experimental System 110.49, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:

I typed Meta-T.  May The Goddess forgive me!

>>ERROR: Cannot convert #(105 110 116) into a string.
Backtrace from the debugger:

STRING-LENGTH (P.C. = 60)

 Arg 0 (STRING): #(105 110 116)
 Local 0 (STRING): NIL


TV::RH-INSERT-STRING (P.C. = 103)

 Arg 0 (STRING): #(105 110 116)
 Arg 1 (BEGIN): 0
 Arg 2 (END): NIL
 Arg 3 (RUBBED-OUT-SOME): T
 Arg 4 (RETURN-FROM-RH): NIL
   --Defaulted args:--
 Arg 5 (DO-NOT-ECHO): NIL
 Local 0 (NEWLINE-POS): NIL
 Local 1 (BEGIN-X): NIL
 Local 2 (BEGIN-Y): NIL
 Local 3 (*WIDTH): NIL
 Local 4 (RECOMPUTE-CURSOR-FLAG): NIL
 Local 5 (TYPEIN-POINTER): NIL
 Local 6 (FILL-POINTER): NIL
 Local 7 (IGNORE): NIL
 Local 8 (NEWLINE-Y): NIL
 Local 9 (END-X): NIL
 Local 10 (END-Y): NIL


TV::RH-EXCHANGE-WORDS (P.C. = 133)

 Arg 0 (TYPE): :FORWARD
 Local 0 (THISWORDBEG): 1
 Local 1 (THISWORDEND): 4
 Local 2 (THISWORD): #(105 110 116)
 Local 3 (OTHERWORDBEG): 5
 Local 4 (OTHERWORDEND): 9
 Local 5 (OTHERWORD): #(99 104 97 114)


TV::RH-COM-EXCHANGE-WORDS (P.C. = 60)

 Arg 0 (N): 1
 Local 0 (I): 0
 Local 1: 1


TV::ALTERNATE-RUBOUT-HANDLER (P.C. = 524)

 Local 0 (CH): 4194388
 Local 1 (CH-CHAR): 84
 Local 2 (CH-CONTROL-META): 2
 Local 3 (COMMAND): (4194388 . TV::RH-COM-EXCHANGE-WORDS)
 Local 4 (FILL-POINTER): 9
 Local 5 (TYPEIN-POINTER): 9
 Local 6 (STATUS): NIL
 Local 7 (RUBBED-OUT-SOME): NIL
 Local 8 (NUMERIC-ARG): NIL
 Local 9 (NUMERIC-ARG-NEGATIVE): NIL
 Local 10 (PROMPT-OPTION): NIL
 Local 11 (INITIAL-INPUT): NIL
 Local 12 (INITIAL-INPUT-POINTER): NIL
 Local 13 (EDITING-COMMAND): NIL
 Local 14 (DO-NOT-ECHO): NIL
 Local 15 (PASS-THROUGH): NIL
 Local 16 (COMMAND-HANDLER): NIL
 Local 17 (PREEMPTABLE): NIL
 Local 18 (BLIP-HANDLER): NIL
 Local 19 (ACTIVATION-HANDLER): (:ACTIVATION CHAR= #/END)
 Local 20 (VALUE): NIL


Remainder of stack:

(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 113)
(:METHOD TV:STREAM-MIXIN :TYI) (P.C. = 27)
SI::XR-XRTYI (P.C. = 67)
SI::XR-READ-THING (P.C. = 240)
SI::XR-READ-LIST (P.C. = 169)
SI::XR-OPENPAREN-MACRO (P.C. = 20)
SI::INVOKE-READER-MACRO (P.C. = 29)
SI::INTERNAL-READ (P.C. = 180)
(:INTERNAL READ-FOR-TOP-LEVEL SI::.WITH-INPUT-EDITING.) (P.C. = 25)
(:METHOD TV:STREAM-MIXIN :RUBOUT-HANDLER) (P.C. = 196)
READ-FOR-TOP-LEVEL (P.C. = 67)
SI:LISP-TOP-LEVEL1 (P.C. = 227)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Message-ID: <8602180215.AA00739@LMI-CAPRICORN.ARPA>
Date: Monday, 17 February 1986, 20:57-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Giving Numerical Argument to m-X List Fonts
To: BUG-ZWEI@LMI-Angel

In Experimental System 110.49, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.1, Experimental Gateway 2.0,
Experimental Tape 1.6, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.4, Experimental TCP-User 62.2,
Experimental TCP-Server 45.5, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1368,
Alpha-1 Release (rjpi: 2/14/86), on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:

Got this by typing c-U m-X List Fonts.  A numeric arg is supposed to
present the fonts on the file computer, as well as those loaded.

>>ERROR: No translation for SYS: DSK: FONTS; * QFASL >.
Backtrace from the debugger:

(:METHOD FS::LOGICAL-PATHNAME :TRANSLATED-PATHNAME) (P.C. = 112)
  (SELF is #FS::LOGICAL-PATHNAME "SYS: DSK: FONTS; * QFASL >")

 Arg 0 (.OPERATION.): :TRANSLATED-PATHNAME
 Local 0: NIL
 Local 1 (TRANS): (#FS::LOGICAL-PATHNAME "SYS: *; * * *" #FS::LM-PATHNAME "DJ: L.*; *.*#*")
 Local 2 (NEWDIR): NIL


FS::LOGICAL-PATHNAME-PASS-ON (P.C. = 20)

 Rest arg (REST): (:DIRECTORY-LIST (:FAST))


FS:DIRECTORY-LIST (P.C. = 32)

 Arg 0 (FILENAME): #FS::LOGICAL-PATHNAME "SYS: DSK: FONTS; * QFASL >"
 Rest arg (OPTIONS): (:FAST)


ZWEI::COM-LIST-FONTS (P.C. = 144)

 Local 0 (LIST): (FONTS:25FR3 FONTS:40VR FONTS:40VSHD FONTS:43VXMS ...)
 Local 1: 251
 Local 2: NIL
 Local 3: #<DTP-LOCATIVE 10151360>
 Local 4 (X): FONTS:HELVETICA12B
 Local 5 (IGNORE): -14323700
 Local 6 (FILE): NIL


ZWEI::COM-EXTENDED-COMMAND (P.C. = 55)

 Local 0 (ANS): ("List Fonts" . ZWEI::COM-LIST-FONTS)


Remainder of stack:

ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 323)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, fixed, valid,
*** EOOH ***
Date: Friday, 14 February 1986, 13:13-EST
From: Scott lander <Ayesha!scott@angel>
Message-ID: <8602141813.AA00153@Ayesha.ARPA>
To: bug-lispm@angel, bug-unix@angel

As of R2.0 when either unix or lisp trys to read the
SDU date it does a 32bit word op. This is wrong.  It
should be doing a byte op.  Could someone fix this in both
places for R3.0

Scott



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 11 February 1986, 22:31-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Another reason SI::ANALYZE-CHANGED-FILES doesn't work in 110
To: BUG-LISPM@LMI-Angel
Message-ID: <[LMI-DAVID-BYRNE].11-Feb-86 22:31:18.RpK>

In Experimental System 110.38, Experimental ZMail 65.3,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1,
Experimental Lambda-Diag 3.0, microcode 1368, Nifty,
on David Byrne (LAMBDA):

SI::ANALYZE-CHANGED-FILES will barf in the current system when walking
down the debugging info of certain :INTERNAL functions; usually the
error is trying to take the CAR of :MACROS-EXPANDED while looking at the
function (:INTERNAL FORMAT::FORMAT-CTL-CHARACTER FORMAT::PRINT-BITS).
Probably the unmodular compiler builds debugging info correctly for
normal functions, but not for :INTERNAL ones.

Of course, there's always CAR-SAFE, but a compiler bug like this could
have other unforeseen and painful consequences (in the debugger,
perhaps).



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 11 February 1986, 22:15-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: ZWEI (?) Lossage
To: mrc@angel
CC: BUG-LISPM@LMI-Angel, mt@cap
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: <8602112229.AA01507@lmi-cap.ARPA>,
             Your message
Message-ID: <[LMI-DAVID-BYRNE].11-Feb-86 22:15:29.RpK>

    Date: Tuesday, 11 February 1986, 17:28-EST
    From: mrc@angel

    In Experimental System 110.27... Alpha-1 Release, on Mary had a
    little Lambda (LAMBDA):

    This happened after doing C-X, C-F to read in a file from
    angel.  Trying to read it in from dired didn't work either.
    The file is a botex file.

    >>ERROR: Cannot convert #2/i into a string.
    Backtrace from the debugger:

    STRING (P.C. = 73)

     Arg 0 (X): #2/i

    Additional information supplied with call:
     Expecting 3 values

    (:PROPERTY :LISP ZWEI::GET-SECTION-NAME) (P.C. = 84) (from file LAD: MT; ZWOBL.#)

There's your (or maybe Mike Travers') problem.  See the filename above ?
This is a redefined version of a system function; it's not the default
of the system software as far as it can be determined -- does this file
work in 102 ?  (I noticed that STRING has a character with a font in
it.)

How did this file get loaded ?  Did you load it yourself ?  This better
not be something the system loads in, because you're not supposed to use
physical pathnames for systems that go outside LMI.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 11 February 1986, 21:44-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: File server bug
To: BUG-LISPM@LMI-Angel
Message-ID: <[LMI-DAVID-BYRNE].11-Feb-86 21:44:00.RpK>

In Experimental System 110.38, Experimental ZMail 65.3,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1,
Experimental Lambda-Diag 3.0, microcode 1368, Nifty,
on David Byrne (LAMBDA):

The file server processes are sensitive to the value of *print-case*
when they print out transaction ID's.  The other machine will think it's
got an invalid transaction ID in the reply as a result.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Saturday, 8 February 1986, 20:15-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Another LEXEME Bug
To: rjpi@LMI-CAPRICORN
CC: BUG-ZMail@LMI-Angel
In-reply-to: <8602082235.AA04405@lmi-cap.ARPA>
Message-ID: <[LMI-DAVID-BYRNE].8-Feb-86 20:15:39.RpK>

    Date: Saturday, 8 February 1986, 17:33-EST
    From: rjpi@LMI-CAPRICORN

    In Experimental System 109.99, Experimental Local-File 64.1,
    Experimental FILE-Server 17.1, Experimental MagTape 3.5,
    Experimental ZMail 64.2, Experimental Unix-Interface 7.1,
    Experimental Tiger 22.0, microcode 1361, (with Flexi-Screen!),
    on Boris Badinoff (LAMBDA):

    Got this by reading in a mail file.

OK, looks like a bug -- what is the mail file ?  (Can't tell from the
backtrace.)

You can see that what's actually happening here is that ZMail thought it
was reading a header line -- parens are one way of delimited things to
be ignored in a address header line.

    >>TRAP 7954 (ARGTYP NUMBER PP 0 QIEQL)
    The first argument to =, NIL, was of the wrong type.
    The function expected a number.
    Backtrace from the debugger:

    Additional information supplied with call:
     Expecting 3 values

    ZWEI::READ-LEXEME (P.C. = 199)

     Arg 0 (RDTBL): #<READTABLE 53453101>
     Arg 1 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Arg 2 (START-INDEX): 5
     Arg 3 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Arg 4 (END-INDEX): 7
     Arg 5 (ERROR-P): :RECURSIVE
     Arg 6 (BACKSLASH-P): T
     Local 0 (CH): NIL
     Local 1 (CODE): 13
     Local 2 (STATE): (ZWEI::UNTYI . ATOM)
     Local 3 (FSM): #<ART-Q 4x14 (simple) 53453534>
     Local 4 (PROPNAME): ZWEI::RFC733
     Local 5 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Local 6 (INDEX): 7
     Local 7 (VALUE): NIL
     Local 8 (ERRMES): NIL
     Local 9 (FLAG): NIL
     Local 10 (ACTION): NIL


    Additional information supplied with call:
     Expecting 3 values

    (:PROPERTY COMMENT ZWEI::RFC733) (P.C. = 51)

     Arg 0 (TYPE): COMMENT
     Arg 1 (RDTBL): #<READTABLE 53453101>
     Arg 2 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Arg 3 (START-INDEX): 4
     Arg 4 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Arg 5 (END-INDEX): 7
     Local 0 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
     Local 1 (INDEX): 5
     Local 2 (TEM): NIL



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8602082235.AA04405@lmi-cap.ARPA>
Date: Saturday, 8 February 1986, 17:33-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Another LEXEME Bug
To: BUG-ZMail@LMI-Angel

In Experimental System 109.99, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.5,
Experimental ZMail 64.2, Experimental Unix-Interface 7.1,
Experimental Tiger 22.0, microcode 1361, (with Flexi-Screen!),
on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:

Got this by reading in a mail file.

>>TRAP 7954 (ARGTYP NUMBER PP 0 QIEQL)
The first argument to =, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 3 values

ZWEI::READ-LEXEME (P.C. = 199)

 Arg 0 (RDTBL): #<READTABLE 53453101>
 Arg 1 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 2 (START-INDEX): 5
 Arg 3 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 4 (END-INDEX): 7
 Arg 5 (ERROR-P): :RECURSIVE
 Arg 6 (BACKSLASH-P): T
 Local 0 (CH): NIL
 Local 1 (CODE): 13
 Local 2 (STATE): (ZWEI::UNTYI . ATOM)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 53453534>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Local 6 (INDEX): 7
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): NIL
 Local 10 (ACTION): NIL


Additional information supplied with call:
 Expecting 3 values

(:PROPERTY COMMENT ZWEI::RFC733) (P.C. = 51)

 Arg 0 (TYPE): COMMENT
 Arg 1 (RDTBL): #<READTABLE 53453101>
 Arg 2 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 3 (START-INDEX): 4
 Arg 4 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 5 (END-INDEX): 7
 Local 0 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Local 1 (INDEX): 5
 Local 2 (TEM): NIL


Additional information supplied with call:
 Expecting 4 values

ZWEI::READ-LEXEME (P.C. = 157)

 Arg 0 (RDTBL): #<READTABLE 53453101>
 Arg 1 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 2 (START-INDEX): 0
 Arg 3 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 4 (END-INDEX): 7
 Arg 5 (ERROR-P): NIL
   --Defaulted args:--
 Arg 6 (BACKSLASH-P): NIL
 Local 0 (CH): 40
 Local 1 (CODE): 12
 Local 2 (STATE): (ZWEI::START . COMMENT)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 53453534>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Local 6 (INDEX): 4
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): ZWEI::START
 Local 10 (ACTION): COMMENT


ZWEI::RDTBL-LEXER (P.C. = 33)

 Arg 0 (RDTBL): #<READTABLE 53453101>
 Arg 1 (START-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 2 (START-INDEX): 0
 Arg 3 (END-STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 4 (END-INDEX): 7
 Arg 5 (ERROR-P): NIL
 Local 0: NIL
 Local 1: #<DTP-LOCATIVE 33102363>
 Local 2 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Local 3 (INDEX): 0
 Local 4 (TEM): NIL
 Local 5 (ERRMES): NIL


ZWEI::RFC733-LEXER (P.C. = 44)

 Arg 0 (STRING): "    (tv:multiple-menu-choose '(rice spinach water coke)"
 Arg 1 (START): 0
 Arg 2 (END): 7
 Arg 3 (ERROR-P): NIL


Remainder of stack:

ZWEI::PROBABLE-ITS-HEADER-P (P.C. = 86)
ZWEI::FIRST-TEXT-LINE (P.C. = 91)
(:PROPERTY :SUBJECT ZWEI::SUMMARY-PRINTER) (P.C. = 37)
ZWEI::SET-MSG-SUMMARY-LINE (P.C. = 77)
ZWEI::SET-PARSED-MSG-HEADERS (P.C. = 111)
(:METHOD ZWEI::ZMAIL-DISK-BUFFER :PARSE-MSG) (P.C. = 21)
ZWEI::ASSURE-MSG-PARSED (P.C. = 64)
ZWEI::ZMAIL-BACKGROUND-PARSE-MSGS (P.C. = 63)
ZWEI::ZMAIL-BACKGROUND-PERFORM (P.C. = 83)
ZWEI::ZMAIL-BACKGROUND (P.C. = 132)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, fixed, valid,
*** EOOH ***
Message-ID: <8602071907.AA03504@lmi-cap.ARPA>
Date: Friday, 7 February 1986, 14:06-EST
From: mrc@angel
To: BUG-LISPM@LMI-Angel

In Experimental System 110.27, Experimental Local-File 66.0,
Experimental FILE-Server 18.1, Experimental ObjectLISP 1.0,
Experimental Site-Editor 1.0, Experimental Gateway 2.0,
Experimental Tape 1.1, Experimental Tiger 23.0,
Experimental Lambda-Diag 3.0, Experimental KERMIT 31.1,
Experimental TCP-Kernel 39.1, Experimental TCP-User 62.1,
Experimental TCP-Server 45.0, Experimental Unix-Interface 9.0,
Experimental ZMail 65.3, Experimental Window-Maker 1.0, microcode 1365,
Alpha-1 Release, on Mary had a little Lambda (LAMBDA):

Choosing the Frame option in Split screen causes an "error in process



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8602050126.AA00949@lmi-cap.ARPA>
Date: Tuesday, 4 February 1986, 20:26-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: LOAD bug with Unix files (can't recognise QFASL's)
To: BUG-LISPM@LMI-Angel

In Experimental System 110.27, Experimental ZMail 65.3,
Experimental Unix-Interface 9.0, Experimental Local-File 66.0,
Experimental MagTape 4.1, Experimental FILE-Server 18.1, microcode 1365,
Nifty, on Laurie Anderson (LAMBDA):

I tried to load cap:/lmi/rpk/foo.qf (with the function LOAD) and this
happened:

>>ERROR: Free reference made to symbol HS
Backtrace from the debugger:

EVAL (P.C. = 91)

 Arg 0 (FORM): HS
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


SI::READFILE-INTERNAL (P.C. = 144)

 Arg 0 (*STANDARD-INPUT*): #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rpk//foo.qf" 54112456>
 Arg 1 (PKG): NIL
 Arg 2 (NO-MSG-P): NIL
 Local 0 (FILE-ID): (#FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo.qf" . 24174231727)
 Local 1 (PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo.qf"
 Local 2 (GENERIC-PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo"
 Local 3 (PLIST): NIL
 Local 4 (VARS): NIL
 Local 5 (VALS): NIL
 Local 6: NIL
 Local 7: NIL
 Local 8 (EOF): (NIL)
 Local 9 (FORM): HS


(:INTERNAL FS::LOAD-1 FS::KLUDGE) (P.C. = 169)

 Local 0: (FS::MULTIPLE-FILE-NOT-FOUND EH::CONDITION-CASE-THROW FS::G0547)
 Local 1: ((FS::MULTIPLE-FILE-NOT-FOUND EH::CONDITION-CASE-THROW FS::G0547) (# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))
 Local 2: ((#FS::UNIX-HOST "LMI-CAPRICORN" . #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo.qf") (#FS::LISPM-HOST "LMI-JAMES-BROWN" . #FS::LM-PATHNAME "JB: RPK;") (#FS::LISPM-HOST "LMI-ALADDIN" . #FS::LM-PATHNAME "LAD: RPK;") (#FS::LISPM-HOST "LMI-

DJINN" . #FS::LM-PATHNAME "DJ: RPK;") ...)
 Local 3: T
 Local 4: (:QFASL :LISP)
 Local 5: #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo"
 Local 6: ((FS:FILE-ERROR) SI::FILE-RETRY-HANDLER #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo" FS::G0560)
 Local 7: ((# SI::FILE-RETRY-HANDLER #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo" FS::G0560) (# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))
 Local 8: #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rpk//foo.qf" 54112456>
 Local 9 (.FILE-ABORTED-FLAG.): :ABORT
 Local 10 (STREAM): #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rpk//foo.qf" 54112456>


FS::LOAD-1 (P.C. = 217)

 Arg 0 (FILE): "cap:/lmi/rpk/foo"
   --Defaulted args:--
 Arg 1 (PKG): NIL
 Arg 2 (NONEXISTENT-OK-FLAG): NIL
 Arg 3 (DONT-SET-DEFAULT-P): NIL
 Arg 4 (NO-MSG-P): NIL
 Local 0: (EH:DEBUGGER-CONDITION ("Retry loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo") T ("Retry loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo") ...)
 Local 1: ("Retry loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo")
 Local 2: (EH:DEBUGGER-CONDITION ("Give up on loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo") T ("Give up on loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo") ...)
 Local 3: ("Give up on loading ~A." #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo")
 Local 4 (PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//rpk//foo"
 Local 5: #<CLOSURE (:INTERNAL FS::LOAD-1 FS::KLUDGE) (Lexical environment) 54112615>


LOAD (P.C. = 68)

 Arg 0 (FILE): "cap:/lmi/rpk/foo"
 Rest arg (KEY-OR-POSITIONAL-ARGS): NIL


Remainder of stack:

SYSTEM:EVAL1 (P.C. = 538)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Message-ID: <8602032304.AA01016@lmi-cap.ARPA>
Date: Monday, 3 February 1986, 18:02-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Dribble File and *terminal-io*
To: BUG-LISPM@LMI-Angel

In Experimental System 109.97, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.5,
Experimental ZMail 64.2, Experimental Unix-Interface 7.1,
Experimental Tiger 22.0, microcode 1361, (with Flexi-Screen!),
on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:


Got this by evaluating

        (tv:sheet-get-screen *terminal-io*)

while a dribble file opened with DRIBBLE-ALL was still open.
I can understand the problem, but it's still pretty obnoxious.

>>TRAP 6087 (ARGTYP INSTANCE PP 0 AREFI-INSTANCE-RESTART-TYPE %INSTANCE-REF)
The first argument to SYSTEM:%INSTANCE-REF, #<CLOSURE SI::DRIBBLE-STREAM-IO 4 33736254>, was of the wrong type.
The function expected an instance.
Backtrace from the debugger:

TV::SHEET-SCREEN (P.C. = 20)

 Arg 0 (SHEET): #<CLOSURE SI::DRIBBLE-STREAM-IO 4 33736254>
   --Defaulted args:--
 Arg 1 (HIGHEST): NIL
 Local 0 (SHEET): #<CLOSURE SI::DRIBBLE-STREAM-IO 4 33736254>
 Local 1 (SUPERIOR): #<CLOSURE SI::DRIBBLE-STREAM-IO 4 33736254>


SYSTEM:EVAL1 (P.C. = 542)

 Arg 0 (FORM): (TV:SHEET-GET-SCREEN *TERMINAL-IO*)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): 1
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER TV::SHEET-SCREEN 30760134>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER TV::SHEET-SCREEN 30760134>
 Local 5 (ARG-DESC): 66
 Local 6 (NUM-ARGS): 1
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ADL): NIL
 Local 11 (ITEM): NIL
 Local 12 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (TV:SHEET-GET-SCREEN *TERMINAL-IO*)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)

 Arg 0 (TOP-LEVEL-FORM): (TV:SHEET-GET-SCREEN *TERMINAL-IO*)
 Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
 Local 1: ((# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER) (# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 238)

 Arg 0 (*TERMINAL-IO*): #<CLOSURE SI::DRIBBLE-STREAM-IO 4 33736254>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
 Local 0 (OLD-PACKAGE): #<Package USER 7022142>
 Local 1 (W-PKG): #<Package USER 7022142>
 Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 36042226>
 Local 3 (THROW-FLAG): T
 Local 4: ("Return to top level in ~A." "Lisp Listener 1")
 Local 5: ((SYSTEM:ABORT EH:DEBUGGER-CONDITION) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
 Local 6 (VALUES): NIL
 Local 7: NIL
 Local 8 (VALUE): #<TV::STANDARD-SCREEN Main Screen 13140073 exposed>


Remainder of stack:

DRIBBLE-ALL (P.C. = 76)
SYSTEM:EVAL1 (P.C. = 542)
SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)
SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, fixed, valid,
*** EOOH ***
Message-ID: <8601312159.AA10581@lmi-cap.ARPA>
Date: Friday, 31 January 1986, 17:02-EST
From: SAM@LMI-CAPRICORN
Subject: In compiler, bad interaction between MULTIPLE-VALUE-BIND and VALUES
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.85, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.2, Experimental Unix-Interface 7.1, microcode 1356,
on Poindexter (LAMBDA):


Insert your description of the circumstances here:


(defun silence? ()
  (multiple-value-bind (a b) (values 1 2)
    a
    b
    (values)))
SILENCE?

(silence?)


(compile 'silence?)
SILENCE?

(silence?)
NIL

;;; I.e., MULTIPLE-VALUE-BIND doesn't know how to return no values
;;;       after it has bound more than one symbol to values.
;;; Only the compiler has this failure.



0,, issues, 3, valid,
*** EOOH ***
Message-ID: <8601292338.AA08885@lmi-cap.ARPA>
Date: Wednesday, 29 January 1986, 18:38-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 783, LMI Site, on Lambda Ten:

If you say
  (send pathname ':new-name <symbol>)
pathname's name becomes FOO.

E.g. (send (fs:parse-pathname "PICON: PICON;") ':new-name 'petro)

--> #FS:LOGICAL-PATHNAME "PICON: PICON; FOO"

Is this a bug?  Aren't pathnames no fun?

-mhd


0,, 3, valid,
*** EOOH ***
Date: Sunday, 19 January 1986, 19:45-EST
From: Dave Goodine <dg@LMI-ANGEL>
To: BUG-LISPM@angel
Message-ID: <[LMI-MAURICE-RAVEL].19-Jan-86 19:45:20.dg>

In Don't-dump-a-band! Inconsistent (unreleased patches loaded) System 109.85,
Experimental Local-File 64.1, Experimental FILE-Server 17.1,
Experimental MagTape 3.4, Experimental ZMail 64.2,
Experimental Unix-Interface 7.1, microcode 1336, LMI Pre-Alpha Test Band,
on Maurice Ravel (LAMBDA):


Insert your description of the circumstances here:

did the following

(defflavor oooh (a b c) () (:default-handler biz))
OOOH
(compile 'oooh)
;;; this works fine in the editor using C-Sh-C.


>>ERROR: Can't find LAMBDA expression for OOOH
Backtrace from the debugger:

Additional information supplied with call:
 Multiple values being passed to frame at 123

(:INTERNAL COMPILE COMPILER::.CONTINUATION.) (P.C. = 159)

 Local 0 (NEW-FILE-THIS-LEVEL): T
 Local 1: T
 Local 2 (TEM): NIL


COMPILE (P.C. = 118)

 Arg 0 (NAME): OOOH
   --Defaulted args:--
 Arg 1 (LAMBDA-EXP): NIL
 Arg 2 (PROCESSING-MODE): COMPILER:MACRO-COMPILE
 Local 0: #<CLOSURE (:INTERNAL COMPILE COMPILER::.CONTINUATION.) (Lexical environment) 55231075>
 Local 1 (TEMPS): (65 #<SI::EQL-HASH-TABLE 0/26320 37625324> #<SI::EQUAL-HASH-TABLE 0/262 2 values 37625326> #<ART-Q-LIST 1536 fill-pointer 0 40441427> ...)


SYSTEM:EVAL1 (P.C. = 542)

 Arg 0 (FORM): (COMPILE (QUOTE OOOH))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (ARGNUM): 1
 Local 1 (ENV): (NIL NIL T NIL ...)
 Local 2 (TEM): NIL
 Local 3 (FINAL-FUNCTION): #<DTP-FEF-POINTER COMPILE 4216002>
 Local 4 (CALL-FUNCTION): #<DTP-FEF-POINTER COMPILE 4216002>
 Local 5 (ARG-DESC): 400103
 Local 6 (NUM-ARGS): 1
 Local 7: NIL
 Local 8: NIL
 Local 9 (ARGL): NIL
 Local 10 (ADL): NIL
 Local 11 (ITEM): NIL
 Local 12 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (COMPILE (QUOTE OOOH))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
 Local 0 (TEM): NIL
 Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 38)

 Arg 0 (TOP-LEVEL-FORM): (COMPILE (QUOTE OOOH))
 Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
 Local 1: ((# SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


Remainder of stack:

SI:LISP-TOP-LEVEL1 (P.C. = 238)
SI::LISP-TOP-LEVEL2 (P.C. = 25)
SI::PROCESS-TOP-LEVEL (P.C. = 113)




0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8601171801.AA02497@lmi-cap.ARPA>
Date: Friday, 17 January 1986, 13:01-EST
From: mhd@LMI-CAPRICORN
Sender: jam@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI Site, on Lambda Ten:


Insert your description of the circumstances here:

Using tiger to print a unix file (was using tiger from lam5; It's also very poor
that the tiger "server" asks the local user to login rather than the tiger "user"!)

>>ERROR: QFILE protocol violated, unknown transaction id in "T2331  ERROR UNK C Login incorrect."
Backtrace from the debugger:

(:INTERNAL FS:HOST-CHAOS-INTERRUPT-FUNCTION 0) (P.C. = 31)

 Arg 0 (PKT): #<CHAOS packet :STRING "T2331  ERROR UNK C Login incorrect." :STATUS CHAOS:RELEASED 37032735>


SI:PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 66)

 Arg 0 (RESTART-ON-RESET): NIL
 Arg 1 (FUNCTION): #<DTP-FEF-POINTER (:INTERNAL FS:HOST-CHAOS-INTERRUPT-FUNCTION 0) 25200101>
 Rest arg (ARGS): (#<CHAOS packet :STRING "T2331  ERROR UNK C Login incorrect." :STATUS CHAOS:RELEASED 37032735>)
Local 1: ("Terminate and free process ~A." "QFILE Protocol Error")
Local 2: ((SYSTEM:ABORT ERROR) ("Terminate and free process ~A." "QFILE Protocol Error") T ("Terminate and free process ~A." "QFILE Protocol Error") ...)


SI:PROCESS-TOP-LEVEL (P.C. = 115)

 Arg 0 (IGNORE): NIL
Local 0: ("Reset and arrest process ~A." "QFILE Protocol Error")
Local 1: (CONDITION ("Reset and arrest process ~A." "QFILE Protocol Error") T ("Reset and arrest process ~A." "QFILE Protocol Error") ...)
Local 2: ("Restart process ~A." "QFILE Protocol Error")
Local 3: ((SYSTEM:ABORT CONDITION) ("Restart process ~A." "QFILE Protocol Error") T ("Restart process ~A." "QFILE Protocol Error") ...)
Local 4 (IGNORE): NIL




0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8601161809.AA01304@lmi-cap.ARPA>
Date: Thursday, 16 January 1986, 13:07-EST
From: pecann@LMI-ANGEL
To: BUG-ZWEI@LMI-Capricorn

In ZWEI in System 102.117, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental ObjectLISP 1.0,
Experimental vista 1.0, Experimental IRIS 1.0, microcode 768,
on Huh ? [No Chaos Address]:

Completion failure in find file: "chop:vista;4t<altmode>" completes to "4trans".
The only correct completion is 4trans-4point.  Perhaps "-4" is being taken as a version number.

-pecann, user interface




0,, fixed, valid,
*** EOOH ***
Message-ID: <8601082341.AA01518@lmi-cap.ARPA>
Date: Wednesday, 8 January 1986, 18:40-EST
From: debbie@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, 102.117a 121985,
on Emma Willard:


Insert your description of the circumstances here:

Listarray of a 3-dimensional array gets the following error.
If listarray is evaluated it works fine .

>>TRAP 4729 (ARGTYP CONS M-S 0 RPLACA)
The first argument to SYSTEM:SETCAR, #<ART-Q-2-2-2 64661271>, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

LISTARRAY (P.C. = 206)

 Arg 0 (ARRAY): #<ART-Q-2-2-2 64661271>
   --Defaulted args:--
 Arg 1 (LIMIT): NIL
Local 0 (NDIMS): 3
Local 1 (ELEMENTS): 8
Local 2 (TIMES): 8
Local 3 (LIST): (NIL NIL NIL NIL ...)
Local 4 (L): (NIL NIL NIL NIL ...)
Local 5 (COUNT): 1
Local 6 (X): 0
Local 7: 2
Local 8 (Y): 0
Local 9: 2
Local 10 (Z): 0
Local 11: 2
Local 12 (INDEX-ARRAY): NIL
Local 13 (I): NIL


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (LISTARRAY AR-1)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER LISTARRAY 23441706>
Local 6 (ARG-DESC): 66
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (LISTARRAY AR-1)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (LISTARRAY AR-1)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 272)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 1700000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
Local 0 (OLD-PACKAGE): #<Package USER 10054742>
Local 1 (W-PKG): #<Package USER 10054742>
Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 30267261>
Local 3 (THROW-FLAG): T
Local 4: ("Return to top level in ~A." "Lisp Listener 1")
Local 5: ((SYSTEM:ABORT ERROR) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
Local 6 (VALUES): NIL
Local 7: NIL
Local 8 (VALUE): NIL


Remainder of stack:

SI:LISP-TOP-LEVEL2 (P.C. = 23)
SI:PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)




0,, fixed, valid,
*** EOOH ***
Message-ID: <8601031755.AA03452@lmi-cap.ARPA>
Date: Friday, 3 January 1986, 12:53-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork (11/16/85)., on Lambda Five:


Insert your description of the circumstances here:

Do (apropos <string> 'fs).  Why is package FILE-SYSTEM a 2d array; or why
should this cause a bombout?

-mhd

>>TRAP 6849 (ARRAY-NUMBER-DIMENSIONS M-ARRAY-LENGTH 1 M-ARRAY-POINTER (DECODE-1D-ARRAY-RESTART RESTORE-ARRAY-REGISTERS)) ->  %STRING-SEARCH-CHAR
The 2-dimensional array #<Package FILE-SYSTEM 7574513> was given 1 subscript: (0).
Backtrace from the debugger:

STRING-SEARCH (P.C. = 104)

 Arg 0 (KEY): "delete"
 Arg 1 (STRING): #<Package FILE-SYSTEM 7574513>
   --Defaulted args:--
 Arg 2 (FROM): 0
 Arg 3 (TO): 9377
 Arg 4 (KEY-FROM): 0
 Arg 5 (KEY-TO): 6
 Arg 6 (CONSIDER-CASE): NIL
Local 0 (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON): NIL
Local 1 (KEY-LEN): 6
Local 2 (CH1): 100


SI::APROPOS-1 (P.C. = 70)

 Arg 0 (SYMBOL): #:
Local 0 (P): #<Package FILE-SYSTEM 7574513>
Local 1: NIL
Local 2 (S): NIL


APROPOS (P.C. = 214)

 Arg 0 (SUBSTRING): "delete"
 Arg 1 (PKG): (FS)
 Rest arg: NIL
Local 1 (INHERITORS): NIL
Local 2 (INHERITED): T
Local 3 (DONT-PRINT): NIL
Local 4 (PREDICATE): NIL
Local 5 (BOUNDP): NIL
Local 6 (FBOUNDP): NIL
Local 7 (APROPOS-PACKAGES): (#<Package SYSTEM 7470446> #<Package GLOBAL 7440020> #<Package FILE-SYSTEM 7574513>)
Local 8: 109
Local 9 (P): #<Package FILE-SYSTEM 7574513>
Local 10: (#<Package FILE-SYSTEM 7574513> #<Package GLOBAL 7440020> #<Package SYSTEM 7470446>)
Local 11 (U): #<Package SYSTEM 7470446>
Local 12: #<Package FILE-SYSTEM 7574513>
Local 13: 4691
Local 14 (SYMBOL): #:...error printing #<SYMBOL 35230451>...
Local 15 (IGNORE): 7458799


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (APROPOS "delete" (QUOTE FS))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 2
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER APROPOS 23437674>
Local 6 (ARG-DESC): 1179714
Local 7 (NUM-ARGS): 2
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (APROPOS "delete" (QUOTE FS))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Remainder of stack:

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
BREAK (P.C. = 437)
ZWEI::COM-BREAK (P.C. = 36)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)




0,, fixed, valid,
*** EOOH ***
Message-ID: <8512310549.AA00233@lmi-cap.ARPA>
Date: Tuesday, 31 December 1985, 00:47-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.171, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.10,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, TCP-Kernel 30.5,
TCP-User 57.8, TCP-Server 33.1, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Fifteen B:


Insert your description of the circumstances here:

Changed a variable in profile mode and tried to save.
-mhd


>>ERROR: The object #<ZMAIL-PROFILE-BUFFER ZMail.in /lmi/mhd/ CAP: 60035077> received a :WRITE-FILE-INTERNAL message, which went unclaimed.
The rest of the message was (#FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in").
Backtrace from the debugger:

#<ZMAIL-PROFILE-BUFFER ZMail.in /lmi/mhd/ CAP: 60035077>:
   Arg 0: :WRITE-FILE-INTERNAL
   Arg 1: #FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in"


SI::INSTANCE-HASH-FAILURE (P.C. = 116)

 Arg 0 (OP): :WRITE-FILE-INTERNAL
 Rest arg (ARGS): (#FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in")
Local 1 (HT): #<EQ-SI::HASH-ARRAY (Funcallable) 6455651>
Local 2 (FN-LOCATION): NIL
Local 3 (FUNC): NIL
Local 4 (NEWHT): NIL
Local 5: NIL


WRITE-FILE-INTERNAL (P.C. = 27)

 Arg 0 (PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in"
 Arg 1 (BUFFER): #<ZMAIL-PROFILE-BUFFER ZMail.in /lmi/mhd/ CAP: 60035077>


SAVE-BUFFER (P.C. = 116)

 Arg 0 (BUFFER): #<ZMAIL-PROFILE-BUFFER ZMail.in /lmi/mhd/ CAP: 60035077>
Local 0 (FILE-ID): (#FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in" . 2713843353)
Local 1 (PATHNAME): #FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in"
Local 2 (FILE-FILE-ID): (#FS::UNIX-PATHNAME "CAP: //lmi//mhd//ZMail.in" . 2713843353)
Local 3: #<FS::QFILE-PROBE-STREAM "CAP: //lmi//mhd//ZMail.in" 61702361>
Local 4 (.FILE-ABORTED-FLAG.): NIL
Local 5 (S): #<FS::QFILE-PROBE-STREAM "CAP: //lmi//mhd//ZMail.in" 61702361>


COM-SAVE-FILE (P.C. = 41)



Remainder of stack:

PROFILE-SAVE-BUTTON (P.C. = 133)
(:SELECT-METHOD ZMAIL-PROFILE-COMMAND-LIST :MOUSE-BUTTON) (P.C. = 389)
(:METHOD ZMAIL-PROFILE-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
COM-ZMAIL-PROFILE (P.C. = 188)
COMMAND-EXECUTE (P.C. = 88)
...
(:SELECT-METHOD ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)




0,, fixed, valid,
*** EOOH ***
Message-ID: <8512240422.AA27662@lmi-cap.ARPA>
Date: Monday, 23 December 1985, 23:16-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.171, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.10,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, TCP-Kernel 30.5,
TCP-User 57.8, TCP-Server 33.1, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Fifteen B:


Insert your description of the circumstances here:

yanked (listf "lam15:pic;junk") in the rubout handler and
then typed <meta-d>.
 -mhd

>>TRAP 5057 (ARGTYP NUMBER PP 1 XLDB)
The second argument to LDB, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

ZWEI:CHAR-SYNTAX (P.C. = 21)

 Arg 0 (CHAR): NIL
 Arg 1 (SYNTAX-TABLE): #<ART-4B-256 30405244>


TV::RH-ALPHABETIC? (P.C. = 36)

 Arg 0 (I): 24
Local 0 (C): NIL


TV::RH-SEARCH-FORWARD-WORD (P.C. = 42)

 Arg 0 (N): 1
   --Defaulted args:--
 Arg 1 (POS): 24
Local 0 (SEARCH-POS): NIL
Local 1 (I): 0


TV::RH-COM-DELETE-WORD (P.C. = 27)

 Arg 0 (N): 1


TV::ALTERNATE-RUBOUT-HANDLER (P.C. = 511)

Local 0 (CH): 4194372
Local 1 (CH-CHAR): 68
Local 2 (CH-CONTROL-META): 2
Local 3 (COMMAND): (4194372 . TV::RH-COM-DELETE-WORD)
Local 4 (FILL-POINTER): 0
Local 5 (TYPEIN-POINTER): 0
Local 6 (STATUS): :INITIAL-ENTRY
Local 7 (RUBBED-OUT-SOME): T
Local 8 (NUMERIC-ARG): NIL
Local 9 (NUMERIC-ARG-NEGATIVE): NIL
Local 10 (PROMPT-OPTION): NIL
Local 11 (INITIAL-INPUT): NIL
Local 12 (INITIAL-INPUT-POINTER): NIL
Local 13 (EDITING-COMMAND): NIL
Local 14 (DO-NOT-ECHO): NIL
Local 15 (PASS-THROUGH): NIL
Local 16 (COMMAND-HANDLER): NIL
Local 17 (PREEMPTABLE): NIL
Local 18 (ACTIVATION-HANDLER): (:ACTIVATION = #/END)
Local 19 (VALUE): NIL


Remainder of stack:

(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 114)
(:METHOD AI-GENERAL-PANE :COMBINED :ANY-TYI) (P.C. = 40) (from file PICON: PICON; AI-WINDOW  )
SI::XR-XRTYI (P.C. = 64)
SI::XR-READ-THING (P.C. = 77)
SI::INTERNAL-READ (P.C. = 234)
READ-FOR-TOP-LEVEL (P.C. = 31)
(:INTERNAL BREAK SI::.DO.IT.) (P.C. = 15)
(:METHOD TV:STREAM-MIXIN :RUBOUT-HANDLER) (P.C. = 198)
BREAK (P.C. = 396)
AI-WINDOW-TYI-HOOK (P.C. = 226) (from file PICON: PICON; AI-WINDOW  )
...
LET-USER-EDIT-SLOT-TABLE (P.C. = 179) (from file PICON: PICON; CAPTURE  )
LET-USER-EDIT-FRAME (P.C. = 70) (from file PICON: PICON; CAPTURE  )
LET-USER-REVIEW-KNOWLEDGE-BASE-NAME (P.C. = 68) (from file PICON: PICON; CAPTURE  )
SAVE-KNOWLEDGE (P.C. = 27) (from file PICON: PICON; CAPTURE  )
HANDLE-TOP-LEVEL-CHARACTER-FOR-CAPTURE (P.C. = 523) (from file PICON: PICON; CAPTURE  )
TOP-LEVEL-COMMAND-LOOP (P.C. = 109) (from file PICON: PICON; CAPTURE  )
(:METHOD PETRO-WINDOW :TOP-LEVEL-FUNCTION) (P.C. = 823) (from file PICON: PICON; PETRO-WINDOW  )
AI-INITIAL-FUNCTION (P.C. = 121) (from file PICON: PICON; AI-WINDOW  )
SI::PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid,
*** EOOH ***
Date: Thursday, 19 December 1985, 14:09-EST
From: Debbie Ellerin <debbie>
Message-ID: <8512191909.AA06276@lmi-cap.ARPA>
To: bug-lispm

adjust-array doesn't work properly for an art-1b array (2-dimensional).
It grows correctly on the first try, but subsequent attempts get an error
The word #<dtp-trap 0> was read from location ...
(you may have to grow the array a few times to get the error)  .


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8512190326.AA05742@lmi-cap.ARPA>
Date: Wednesday, 18 December 1985, 22:36-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: LEXEME Problems
To: BUG-ZMail@LMI-Capricorn

In Experimental System 109.42, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.1, microcode 1311, GC5,
on Boris Badinoff (LAMBDA):


Insert your description of the circumstances here:

        I get this problem when ZMail is reading in my mail file and it
reaches the message included here.  This bug didn't occur in 102.

======================================================================

From: wer
Date: Sunday, 24 March 1985, 16:06-EST
To: bug-lispm
CC: tex

If you have any requests for documents (favorite tcp/ip or chaos
documents from MIT, etc) that will cost us legwork but not a fortune,
send mail to "library" about the name and (if you have any leads)
source for the document.  Rich Mabbitt and I will try to track some
of these down. Mail will collect in /lmi/library-requests, if you
want to look at it.


======================================================================

>>TRAP 7937 (ARGTYP NUMBER PP 0 QIEQL)
The first argument to =, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 3 values

ZWEI::READ-LEXEME (P.C. = 199)

 Arg 0 (RDTBL): #<READTABLE 17452371>
 Arg 1 (START-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 2 (START-INDEX): 59
 Arg 3 (END-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 4 (END-INDEX): 64
 Arg 5 (ERROR-P): :RECURSIVE
 Arg 6 (BACKSLASH-P): T
 Local 0 (CH): NIL
 Local 1 (CODE): 13
 Local 2 (STATE): (ZWEI::UNTYI . ATOM)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 17451744>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Local 6 (INDEX): 64
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): NIL
 Local 10 (ACTION): NIL


Additional information supplied with call:
 Expecting 3 values

(:PROPERTY COMMENT ZWEI::RFC733) (P.C. = 51)

 Arg 0 (TYPE): COMMENT
 Arg 1 (RDTBL): #<READTABLE 17452371>
 Arg 2 (START-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 3 (START-INDEX): 39
 Arg 4 (END-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 5 (END-INDEX): 64
 Local 0 (STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Local 1 (INDEX): 58
 Local 2 (TEM): (ATOM "or" ("If you have any requests for documents (favorite tcp//ip or chaos" 56) ("If you have any requests for documents (favorite tcp//ip or chaos" 58))


Additional information supplied with call:
 Expecting 4 values

ZWEI::READ-LEXEME (P.C. = 157)

 Arg 0 (RDTBL): #<READTABLE 17452371>
 Arg 1 (START-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 2 (START-INDEX): 38
 Arg 3 (END-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 4 (END-INDEX): 64
 Arg 5 (ERROR-P): NIL
   --Defaulted args:--
 Arg 6 (BACKSLASH-P): NIL
 Local 0 (CH): 40
 Local 1 (CODE): 12
 Local 2 (STATE): (ZWEI::START . COMMENT)
 Local 3 (FSM): #<ART-Q 4x14 (simple) 17451744>
 Local 4 (PROPNAME): ZWEI::RFC733
 Local 5 (STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Local 6 (INDEX): 39
 Local 7 (VALUE): NIL
 Local 8 (ERRMES): NIL
 Local 9 (FLAG): ZWEI::START
 Local 10 (ACTION): COMMENT


ZWEI::RDTBL-LEXER (P.C. = 33)

 Arg 0 (RDTBL): #<READTABLE 17452371>
 Arg 1 (START-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 2 (START-INDEX): 0
 Arg 3 (END-STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 4 (END-INDEX): 64
 Arg 5 (ERROR-P): NIL
 Local 0: ((ATOM "If" # #) (ATOM "you" # #) (ATOM "have" # #) (ATOM "any" # #) ...)
 Local 1: ((ATOM "documents" # #))
 Local 2 (STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Local 3 (INDEX): 38
 Local 4 (TEM): (ATOM "documents" ("If you have any requests for documents (favorite tcp//ip or chaos" 29) ("If you have any requests for documents (favorite tcp//ip or chaos" 38))
 Local 5 (ERRMES): NIL


ZWEI::RFC733-LEXER (P.C. = 44)

 Arg 0 (STRING): "If you have any requests for documents (favorite tcp//ip or chaos"
 Arg 1 (START): 0
 Arg 2 (END): 64
 Arg 3 (ERROR-P): NIL


Remainder of stack:

ZWEI::PROBABLE-ITS-HEADER-P (P.C. = 86)
ZWEI::FIRST-TEXT-LINE (P.C. = 91)
(:PROPERTY :SUBJECT ZWEI::SUMMARY-PRINTER) (P.C. = 37)
ZWEI::SET-MSG-SUMMARY-LINE (P.C. = 77)
ZWEI::SET-PARSED-MSG-HEADERS (P.C. = 111)
ZWEI::COM-EDIT-CURRENT-MSG (P.C. = 204)
ZWEI::COM-ZMAIL-EXTENDED-COMMAND (P.C. = 82)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-COMMAND-CHAR) (P.C. = 32)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 175)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 113)



0,, 3, valid,
*** EOOH ***
Message-ID: <8512190031.AA05630@lmi-cap.ARPA>
Date: Wednesday, 18 December 1985, 19:34-EST
From: Dave Goodine <dg@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.26, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.0, microcode 1311, GC5, on Guinea Pig (LAMBDA):


Insert your description of the circumstances here:

This code caused it.

(defflavor protocol-violation ()
           (user-error)
  :gettable-instance-variables
  :initable-instance-variables
  (:required-init-keywords :format-string))

(defmethod (protocol-violation :report) (stream)
  (format stream "Protocol Violation: ~A"
          (lexpr-funcall 'format nil
                         (send self :format-string)
                         (send self :format-args))))

(signal (make-condition 'protocol-violation :format-string "foo"))


I had previously been playing with this flavor in a fairly obscene way, so it may
not be a real bug.

>>ERROR: A EH::NEW-NONEXISTENT-INSTANCE-VARIABLE trap was received from the microcode, which does not have a make-ucode-error-function associated with it!
Backtrace from the debugger:

EH::MAKE-UCODE-ERROR (P.C. = 28)

 Arg 0 (ERROR-NAME): EH::NEW-NONEXISTENT-INSTANCE-VARIABLE
 Arg 1 (SG): #<DTP-STACK-GROUP "SECOND-LEVEL-ERROR-HANDLER-1" 16340056>
 Arg 2 (ETE): (EH::NEW-NONEXISTENT-INSTANCE-VARIABLE)
 Local 0 (ERROR-FCTN): NIL


EH::PREPARE-TO-SIGNAL-MICROCODE-CONDITION (P.C. = 177)

 Arg 0 (SG): #<DTP-STACK-GROUP "SECOND-LEVEL-ERROR-HANDLER-1" 16340056>
 Arg 1 (ETE): (EH::NEW-NONEXISTENT-INSTANCE-VARIABLE)
   --Defaulted args:--
 Arg 2 (IGNORE): T
 Local 0 (INHIBIT-SCHEDULING-FLAG): T
 Local 1 (*ERROR-HANDLER-REPRINT-ERROR*): NIL
 Local 2 (*ERROR-HANDLER-RUNNING*): T
 Local 3: ("Abort from where a microcode error is being signaled.")
 Local 4: ((SYSTEM:ABORT EH:DEBUGGER-CONDITION) ("Abort from where a microcode error is being signaled.") T ("Abort from where a microcode error is being signaled.") ...)
 Local 5 (SAVED-MICRO-PCS): NIL
 Local 6 (ERROR-OBJECT): NIL
 Local 7 (I): NIL
 Local 8 (PC): NIL
 Local 9 (CONDITION-RESULT): NIL



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8512122046.AA00550@lmi-cap.ARPA>
Date: Thursday, 12 December 1985, 15:46-EST
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.9, KERMIT 26.21,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.5, TCP-User 57.7, TCP-Server 33.1,
Experimental LMI Laser Printer II 11.2, DOE-Macsyma 9.15, microcode 768,
Education 2x2, on Emma Willard:


Insert your description of the circumstances here:

This happened when I tried to set the first keyword in zmail.
Now almost all zmail commands keep getting this same error.


>>TRAP 6812 (ARGTYP ARRAY M-ARRAY-POINTER (GAHDR RESTORE-ARRAY-REGISTERS) GAHDR) ->  ARRAY-ACTIVE-LENGTH
The NIL argument to ARRAY-ACTIVE-LENGTH, NIL, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

ZWEI::STRING-FROM-KEYWORDS (P.C. = 33)

 Arg 0 (KEYWORDS): (:AAA)
Local 0 (STR): NIL
Local 1 (KEYS): (:AAA)
Local 2 (LENGTH): 1
Local 3 (I0): NIL
Local 4 (I1): NIL
Local 5 (KEY): NIL
Local 6 (LEN): NIL


(:PROPERTY ZWEI::COM-ZMAIL-KEYWORDS ZWEI::WHO-LINE-DOCUMENTATION-UPDATER) (P.C. = 64)

 Arg 0 (STRING): "Change keywords on this message: L: add "
Local 0 (ON): (:AAA)
Local 1 (OFF): NIL
Local 2 (KEYS): NIL


ZWEI::UPDATE-COMMAND-WHO-LINE-DOCUMENTATION (P.C. = 78)

 Arg 0 (COMMAND): ZWEI::COM-ZMAIL-KEYWORDS
   --Defaulted args:--
 Arg 1 (TELL-WHO-LINE): T
 Arg 2 (RECURSIVE): NIL
Local 0 (STRING): "Change keywords on this message: L: add "
Local 1 (FUNCTION): #<DTP-FEF-POINTER (:PROPERTY ZWEI::COM-ZMAIL-KEYWORDS ZWEI::WHO-LINE-DOCUMENTATION-UPDATER) 5505047>


ZWEI::COMPUTE-CURRENT-MSG-NAME (P.C. = 68)

Local 0 (STRING): NIL
Local 1 (STATUS): NIL
Local 2 (NMSGS): NIL
Local 3 (LIST): NIL
Local 4 (FLAG): NIL
Local 5 (KEY): NIL


ZWEI::ZMAIL-SELECT-MSG (P.C. = 182)

 Arg 0 (MSG): 77
 Arg 1 (NO-ERROR-P): NIL
 Arg 2 (SAVE-POINT-P): NIL
Local 0 (OLD-CURRENT-MSG): #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 54302030> :INTERVAL ...)
Local 1 (INDEX): NIL
Local 2 (ARRAY): #<ART-Q-107 43064623>
Local 3 (NMSGS): 86
Local 4 (FLAG): NIL
Local 5 (I): NIL
Local 6 (BP): NIL


Remainder of stack:

ZWEI::ZMAIL-SELECT-PREVIOUS-MSG (P.C. = 90)
ZWEI::COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL (P.C. = 103)
ZWEI::COM-ZMAIL-PREVIOUS (P.C. = 58)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8512111008.AA00359@lmi-cap.ARPA>
Date: Wednesday, 11 December 1985, 05:14-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Deexposed typein action :notify in Supdup
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.33, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.1, microcode 1311, GC5 FS LAM,
on Mary had a little Lambda (LAMBDA):

I set the deexposed typein action of a Supdup window connected to Angel
to :NOTIFY.  Deexposed typeout action was :PERMIT.  This configuration
was to allow me to run BoTeX.  This would allow it to print out without
the window being selected but would, or so I thought, notify me if it
got an error and was waiting for input.  Or so I thought!  After setting
BoTeX running, I selected another window.  Later, I re-selected Supdup
to see how things were going.  Well, BoTeX had gotten an error and was
waiting for input, but there had been no notification.  What happened?



0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8512091525.AA02586@lmi-cap.ARPA>
Date: Monday, 9 December 1985, 10:25-EST
From: mrc@angel
To: BUG-ZMAIL@LMI-Capricorn

In System 102.174, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12,
KERMIT 26.25, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, TCP-Kernel 30.8, TCP-User 57.11,
TCP-Server 33.3, DOE-Macsyma 9.17, Macsyma-Help-Database 1.1,
microcode 778, GJC MADE THIS BAND, on Boris Badinoff:


Insert your description of the circumstances here:
I got this error when trying to CTRL-N past the end of a mail message.

>>ERROR: The object #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 67343700> received a :UPDATE-OPTIONS-IN-FILE message, which went unclaimed.
The rest of the message was NIL.
Backtrace from the debugger:

#<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 67343700>:
   Arg 0: :UPDATE-OPTIONS-IN-FILE


SI::INSTANCE-HASH-FAILURE (P.C. = 116)

 Arg 0 (OP): :UPDATE-OPTIONS-IN-FILE
 Rest arg (ARGS): NIL
Local 1 (HT): #<EQ-SI::HASH-ARRAY (Funcallable) 6442160>
Local 2 (FN-LOCATION): NIL
Local 3 (FUNC): NIL
Local 4 (NEWHT): NIL
Local 5: NIL


(:METHOD ZWEI::ZMAIL-BUFFER :MODIFIED-P) (P.C. = 61)
  (SELF is #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 67343700>)

 Arg 0 (.OPERATION.): :MODIFIED-P
Local 0 (.ARRAY.): #<ART-Q-64 67343755>
Local 1 (.I.): 6
Local 2 (.NMSGS.): 6
Local 3 (MSG): #S(ZWEI::MSG :REAL-INTERVAL #<ZWEI::NODE 67351745> :INTERVAL ...)


ZWEI::BUFFER-MODIFIED-P (P.C. = 18)

 Arg 0 (BUFFER): #<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 67343700>


ZWEI::UNDO-SAVE-NEW-SMALL-CHANGE (P.C. = 330)

 Arg 0 (BP1): ("From Ingria@LMI-CAPRICORN  Mon Dec  9 03:34:54 1985" 0 :MOVES)
 Arg 1 (BP2): ("From Ingria@LMI-CAPRICORN  Mon Dec  9 03:34:54 1985" 0 :MOVES)
Local 0 (UNDO-STATUS): (#<ZWEI::UNIX-INBOX-BUFFER CAP: /usr/mail/mrc 67343700> NIL ("From Ingria@LMI-CAPRICORN  Mon Dec  9 03:34:54 1985" 0 :NORMAL) ("From Ingria@LMI-CAPRICORN  Mon Dec  9 03:34:54 1985" 0 :MOVES) ...)
Local 1 (BP1-INSIDE): NIL
Local 2 (BP2-INSIDE): NIL
Local 3 (LINE): NIL
Local 4 (END-LINE): NIL
Local 5 (ALIST): ((** 0 0) (** 0 0) (** 0 0) (** 0 0) ...)
Local 6 (COUNT): 0
Local 7: NIL
Local 8 (INDEX): 0


Remainder of stack:

ZWEI::INSERT (P.C. = 127)
ZWEI::DOWN-REAL-LINE (P.C. = 83)
ZWEI::COM-DOWN-REAL-LINE (P.C. = 37)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) 0) (P.C. = 58)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MOUSE-BUTTON) (P.C. = 59)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)









0,, unreproducible, 3, valid,
*** EOOH ***
Message-ID: <8512090822.AA02074@lmi-cap.ARPA>
Date: Monday, 9 December 1985, 03:24-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Yanking form from Kill List into Mini-Buffer
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.33, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.1, microcode 1311, GC5 FS LAM,
on Mary had a little Lambda (LAMBDA):


Insert your description of the circumstances here:

Got this by defining a region containing a LISP form, then typing c-U
m-X Evaluate into Buffer and typing c-Y to yank it into the mini-buffer.

>>TRAP 5748 (INSTANCE-LACKS-INSTANCE-VARIABLE M-C M-A)
There is no instance variable ZWEI::SAVED-FONT-ALIST in #<ZWEI::NODE 13414713>.
Backtrace from the debugger:

ZWEI::FIXUP-FONTS-INTERVAL (P.C. = 60)

 Arg 0 (FONTS): (:TR12 :TR12B :HL12I :HL12B ...)
 Arg 1 (FROM-BP): ("(pprint-character-alist (list (assoc #\Clear-screen tv:*escape-keys*)))" 0 :NORMAL)
 Arg 2 (TO-BP): ("(pprint-character-alist (list (assoc #\Clear-screen tv:*escape-keys*)))" 71 :MOVES)
 Arg 3 (IN-ORDER-P): T
 Local 0 (PERMUTATION): NIL
 Local 1 (I): NIL
 Local 2: NIL
 Local 3 (J): NIL
 Local 4 (LIST): NIL


ZWEI::INSERT-KILL-RING-THING (P.C. = 81)

 Arg 0 (BP): ("(pprint-character-alist (list (assoc #\Clear-screen tv:*escape-keys*)))" 0 :NORMAL)
 Arg 1 (THING): #<ZWEI::NODE 33554142>
 Local 0 (BP1): ("(pprint-character-alist (list (assoc #\Clear-screen tv:*escape-keys*)))" 71 :MOVES)


ZWEI::YANK-AS-TEXT (P.C. = 201)

 Arg 0 (THING): #<ZWEI::NODE 33554142>
 Arg 1 (KILL-PREVIOUS): NIL
 Arg 2 (LEAVE-POINT-BEFORE): NIL
 Local 0 (BP1): NIL
 Local 1 (BP2): NIL
 Local 2 (UNDO-ITEM): NIL


ZWEI::HISTORY-YANK (P.C. = 86)

 Arg 0 (HISTORY): #<ZWEI::HISTORY kill history. Length 79 Yank pointer 1>
 Local 0 (ELEMENT): #<ZWEI::NODE 33554142>


ZWEI::COM-YANK (P.C. = 22)



Remainder of stack:

ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 322)
(:INTERNAL (:METHOD ZWEI::ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) 0) (P.C. = 51)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI::ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) (P.C. = 39)
ZWEI::EDIT-IN-MINI-BUFFER (P.C. = 216)
ZWEI::TYPEIN-LINE-MULTI-LINE-READ-WITH-DEFAULT (P.C. = 48)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 322)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)


0,, 3, valid,
*** EOOH ***
Date: Monday, 9 December 1985, 03:13-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Terminal Resume
To: BUG-LISPM@LMI-Capricorn
Message-ID: <[LMI-LAMB-CHOP].9-Dec-85 03:13:31.Ingria>

In Experimental System 109.33, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.1, microcode 1311, GC5 FS LAM,
on Mary had a little Lambda (LAMBDA):

The documentation string for Terminal Resume states the following:

"Allow deexposed typeout in window that Terminal-0-S would select."

However, "OUTPUT HOLD" still appears in the run state sheet, even after
Terminal Resume is typed.  (If the deeexposed typeout action of the
window is :NOTIFY or :NORMAL.)

Terminal Resume calls TV::KBD-ESC-RESUME, whose definition is as follows:

(DEFUN KBD-ESC-RESUME ()
  "Handle a terminal-resume typed on the keyboard by allowing interesting window to type out."
  (LET ((W (FIND-INTERESTING-WINDOW)))
    (IF W
        (SEND W :SET-DEEXPOSED-TYPEOUT-ACTION :PERMIT)
      (BEEP))))

As far as I can figure out, the problem is that, even though this sets
the window's deexposed typeout action to :PERMIT, it still has not
cleared the output hold flag of the window.  The following re-definition
seems to do the right thing:

(DEFUN KBD-ESC-RESUME ()
  "Handle a terminal-resume typed on the keyboard by allowing interesting window to type out."
  (LET ((W (FIND-INTERESTING-WINDOW)))
    (IF W
        (progn
          (SEND W :SET-DEEXPOSED-TYPEOUT-ACTION :PERMIT)
          (setf (tv:sheet-output-hold-flag w) 0))
      (BEEP))))

I haven't installed this, since I don't know whether this is an
oversight in the defintion of TV::KBD-ESC-RESUME or whether setting the
deexposed typeout action to :PERMIT should take care of clearing this
exception flag directly.


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8512061905.AA01976@lmi-cap.ARPA>
Date: Friday, 6 December 1985, 13:58-EST
From: Dave Goodine <dg@LMI-ANGEL>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.26, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.0, microcode 1311, GC5,
on Maurice Ravel (LAMBDA):

The :IF-EXISTS option for FS:LMFS-OPEN-FILE provided as NIL, creates the
file if it doesn't exists, but still returns NIL.

-dg


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 5 December 1985, 14:27-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Window problems in 2.1
To: BUG-LISPM@LMI-Capricorn
Message-ID: <[LMI-DAVID-BOWIE].5-Dec-85 14:27:00.RpK>

In Experimental System 109.31, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.4,
Experimental ZMail 64.0, microcode 1312, 109.27+ZMail,
on David Bowie (LAMBDA):

. On a portrait monitor, the Peek menu is deexposed.

. After sending a Lisp Listener window

  :SET-EDGES #o4 #o66 #o1360 #o733
  :SET-BORDERS 3
  :SET-BORDER-MARGIN-WIDTH 3

the appearance changed accordingly, but after it was deexposed (because
I selected another window), and then reexposed, it would not draw about
#o20 pixels on right hand edge, which includes the right hand border,
and other #o15 pixels or so of the right hand extremities of the top and
bottom borders.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 5 December 1985, 13:48-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Brain Death
To: jer@LMI-CAPRICORN
CC: BUG-Lispm@LMI-Capricorn
In-reply-to: The message of 30 Sep 1985 17:48-EDT from jer@LMI-CAPRICORN
Message-ID: <[LMI-DAVID-BOWIE].5-Dec-85 13:48:10.RpK>

    Date: Monday, 30 September 1985, 17:48-EDT
    From: Janet E. Ressler <jer@LMI-CAPRICORN>

    In System 102.162, Local-File 56.12, FILE-Server 13.2,
    Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.7, KERMIT 26.20,
    MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
    Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
    Experimental ObjectLISP 1.0, Experimental vista 1.0,
    Experimental IRIS 1.0, DOE-Macsyma 9.9, Macsyma-Help-Database 1.1,
    microcode 778, on The Importance of Being Earnest:

    Got this by clicking right on [Save All Files]

    >>TRAP 10602 (ILLEGAL-INSTRUCTION)
    There was an attempt to execute an invalid instruction: 15724.
    Backtrace from the debugger:

    TV::DRAW-CHOICE-BOX (P.C. = 131)

This wasn't a ZMail bug. I notice that TV::DRAW-CHOICE-BOX uses the
%DRAW-TRIANGLE instruction, which had problems in 2.0.


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8511220457.AA00284@lmi-cap.ARPA>
Date: Thursday, 21 November 1985, 23:57-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: SYSTEM:FUNCALL-MACRO condition
To: BUG-LISPM@LMI-Capricorn

In Experimental System 109.19, Experimental Local-File 64.1,
Experimental FILE-Server 17.1, Experimental MagTape 3.1,
Experimental ZMail 63.1, microcode 1303, GC4 FS LAM,
on David Bowie (LAMBDA):

I must've screwed up in my compilation somewhere, so this happened...

>>ERROR: SYSTEM:FUNCALL-MACRO is not a known condition flavor or signal name
Backtrace from the debugger:

MAKE-CONDITION (P.C. = 44)

 Arg 0 (SIGNAL-NAME): SYSTEM:FUNCALL-MACRO
 Rest arg (ARGS): ("Funcalling the macro ~S." IN-FONT)


FERROR (P.C. = 42)

 Arg 0 (SIGNAL-NAME): SYSTEM:FUNCALL-MACRO
 Arg 1 (FORMAT-STRING): "Funcalling the macro ~S."
 Rest arg (ARGS): (IN-FONT)


SYSTEM:APPLY-LAMBDA (P.C. = 166)

 Arg 0 (FCTN): (MACRO . #<DTP-FEF-POINTER IN-FONT 6007711>)
 Arg 1 (A-VALUE-LIST): (:ROMAN :ROMAN NIL 30)
   --Defaulted args:--
 Arg 2 (ENVIRONMENT): NIL
 Local 0 (TEM): NIL
 Local 1: NIL
 Local 2: NIL
 Local 3: NIL
 Local 4 (ARG): NIL


IN-FONT: (from file DOCTOOL: GRUNCH; GRUNCH  )
   Arg 0: :ROMAN
   Arg 1: :ROMAN
   Arg 2: NIL
   Arg 3: 30


OUTPUT-SPACING (P.C. = 54) (from file DOCTOOL: GRUNCH; GRUNCH-OUT  )

 Arg 0 (PIXELS): 30
 Local 0 (NSPACES): 5
 Local 1 (N): 5
 Local 2: 5


Remainder of stack:

OUTPUT-CHAR (P.C. = 77) (from file DOCTOOL: GRUNCH; GRUNCH-OUT  )
PROCESS-TEXT-CHAR (P.C. = 116) (from file DOCTOOL: GRUNCH; GRUNCH  )
PROCESS-STREAM (P.C. = 69) (from file DOCTOOL: GRUNCH; GRUNCH  )
PROCESS-IN-ENVIRONMENT (P.C. = 282) (from file DOCTOOL: GRUNCH; GRUNCH  )
PROCESS-STREAM-TOP-LEVEL (P.C. = 66) (from file DOCTOOL: GRUNCH; GRUNCH  )
(:INTERNAL GRUNCH-FILE DO-IT) (P.C. = 160)
GRUNCH-FILE (P.C. = 153) (from file DOCTOOL: GRUNCH; GRUNCH  )
GRUNCH-FILE-FOR-GATEWAY (P.C. = 55) (from file DOCTOOL: GRUNCH; GRUNCH  )
SYSTEM:EVAL1 (P.C. = 542)
SI:EVAL-SPECIAL-OK (P.C. = 81)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 322)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 113)


0,, issues, 3, valid,
*** EOOH ***
Message-ID: <8511162153.AA00912@lmi-cap.ARPA>
Date: Saturday, 16 November 1985, 16:47-EST
From: Michael Travers <MT@lmi-capricorn>
To: BUG-LISPM@LMI-Capricorn

In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.10,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental ObjectLISP 2.0, Experimental vista 1.0,
Experimental IRIS 1.0, microcode 778,
on The Importance of Being Earnest:


The proceed for this error is labelled "simply proceed".  This says nothing about
what will happen if you do, which apparently is to do nothing.  What I usually
want in this situation is to forcibly flush the using packagages symbol and
replace it with the exported symbol.  There should be another proceed to do this,
and the old one should have a better label.

>>ERROR: Name conflicts created by EXPORT in package OBIE:
"TOOLBOX-MENU-ICON" causes a conflict in package BW.
"OWNER" causes a conflict in package BW.
"PORT" causes a conflict in package BW.
"ADD-OBJECT" causes a conflict in package BW.
"WINDOW" causes a conflict in package BW.
"VALUE" causes a conflict in package BW.
"PORTED-ICON" causes a conflict in package BW.
"TEXT" causes a conflict in package BW.
"DYNAMIC-ARRAY-ICON" causes a conflict in package BW.
"ARRAY-ICON" causes a conflict in package BW.
"TEXT-ICON" causes a conflict in package BW.
"MOVE" causes a conflict in package BW.
"WINDOW" causes a conflict in package BW.
"Y" causes a conflict in package BW.
"X" causes a conflict in package BW.

Backtrace from the debugger:

EXPORT (P.C. = 125)

 Arg 0 (SYMBOLS): ("DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT" ...)
 Arg 1 (PKG): #<Package OBIE 7377561>
   --Defaulted args:--
 Arg 2 (FORCE-FLAG): NIL
Local 0 (CONFLICTS): (("TOOLBOX-MENU-ICON" #<Package BW 7404226> ** **) ("OWNER" #<Package BW 7404226> ** **) ("PORT" #<Package BW 7404226> ** **) ("ADD-OBJECT" #<Package BW 7404226> ** **) ...)
Local 1: NIL
Local 2 (P): #<Package BW 7404226>
Local 3: NIL
Local 4 (SYMBOL): "ICON-CLASS"
Local 5 (CANDIDATES): NIL
Local 6 (SYM): NIL
Local 7 (INDEX): NIL
Local 8: NIL


SI::ALTER-PACKAGE (P.C. = 232)

 Arg 0 (NAME): OBIE
 Rest arg: (:IMPORT NIL :USE (OBJ GLOBAL) ...)
Local 1 (NICKNAMES): NIL
Local 2 (USE): (OBJ GLOBAL)
Local 3 (IGNORE): NIL
Local 4 (SHADOW): NIL
Local 5 (EXPORT): ("DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT" ...)
Local 6 (PREFIX-NAME): NIL
Local 7 (AUTO-EXPORT-P): NIL
Local 8 (IMPORT): NIL
Local 9 (SHADOWING-IMPORT): NIL
Local 10 (IMPORT-FROM): NIL
Local 11 (RELATIVE-NAMES): NIL
Local 12 (RELATIVE-NAMES-FOR-ME): NIL
Local 13 (IGNORE): NIL
Local 14 (PROPERTIES): NIL
Local 15 (NEW-SYMBOL-FUNCTION): NIL
Local 16 (IGNORE): NIL
Local 17 (IGNORE): NIL
Local 18: (NIL)
Local 19 (EXTERNAL-ONLY-P): NIL
Local 20 (EXTERNAL-ONLY): NIL
Local 21 (PKG): #<Package OBIE 7377561>
Local 22 (PROP): NIL
Local 23 (VAL): NIL
Local 24: NIL
Local 25 (DESIRED-USE): NIL
Local 26 (ELT): NIL
Local 27 (NAME): NIL
Local 28 (SYM): NIL
Local 29: NIL
Local 30 (NICK): NIL
Local 31 (P): NIL
Local 32: NIL


SI::DEFPACKAGE-INTERNAL (P.C. = 154)

 Arg 0 (NAME): OBIE
 Arg 1 (ALIST-OF-OPTIONS): ((:USE OBJ GLOBAL) (:EXPORT "DISPLAY-OBJECT" "POINT" "RECT" ...))
Local 0 (IMPORT): NIL
Local 1 (L): (("DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT" ...))
Local 2 (TAIL): NIL
Local 3 (K): :EXPORT
Local 4 (ARGS): ("DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT" ...)
Local 5: NIL
Local 6 (S): NIL
Local 7 (P): (:USE (OBJ GLOBAL) :EXPORT ("DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT" ...))


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (SI::DEFPACKAGE-INTERNAL (QUOTE OBIE) (QUOTE **))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 2
Local 1 (ENV): (NIL NIL NIL NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER SI::DEFPACKAGE-INTERNAL 6054611>
Local 6 (ARG-DESC): 130
Local 7 (NUM-ARGS): 2
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


Additional information supplied with call:
 SYSTEM:ADI-FEXPR-CALL

LET (P.C. = 202)

 Arg 0 (VARLIST): ((SI::SYM **) (SI::PKG **))
 Rest arg (BODY): ((RECORD-SOURCE-FILE-NAME SI::SYM **) SI::PKG)
Local 1 (VARS-LEFT): ((SI::PKG **))
Local 2 (L): NIL
Local 3 (VARS-ENV): (NIL)
Local 4 (BINDFRAME): (#<DTP-LOCATIVE 2322500> OBIE #<DTP-LOCATIVE 2322505> 71447 . 547)
Local 5 (VALS-LEFT): NIL
Local 6 (THISVARLOC): #<DTP-LOCATIVE 2322505>
Local 7: NIL
Local 8: NIL
Local 9: NIL
Local 10 (VAR): NIL
Local 11 (TEM): (#<DTP-LOCATIVE 2322500> OBIE #<DTP-LOCATIVE 2322505> 71447 . 547)
Local 12 (MUMBLE): NIL


Remainder of stack:

SYSTEM:EVAL1 (P.C. = 547)
ZWEI::EVAL-PRINT (P.C. = 33)
ZWEI::COMPILE-INTERVAL-PROCESS-BASIC-FORM (P.C. = 35)
COMPILER:COMPILE-DRIVER (P.C. = 599)
ZWEI::COMPILE-INTERVAL-PROCESS-FN (P.C. = 24)
COMPILER:COMPILE-STREAM (P.C. = 588)
(:INTERNAL ZWEI::COMPILE-INTERVAL ZWEI::DO-IT) (P.C. = 45)
ZWEI::COMPILE-INTERVAL (P.C. = 284)
ZWEI::COMPILE-PRINT-INTERVAL (P.C. = 123)
ZWEI::COMPILE-DEFUN-INTERNAL (P.C. = 115)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI::ZMACS-WINDOW-PANE :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI::ZMACS-WINDOW-PANE :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
Message-ID: <8511150016.AA02415@lmi-cap.ARPA>
Date: Thursday, 14 November 1985, 18:26-EST
From: Michael Travers <MT@lmi-capricorn>
Subject: :default-handler option to defflavor
To: BUG-LISPM@LMI-Capricorn

In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.10,
KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental ObjectLISP 2.0, Experimental vista 1.0,
Experimental IRIS 1.0, microcode 778, on Waiting for Godot:

If you change the :default-handler option to defflavor and recompile, it doesn't
get changed in the flavor structure.



0,, fixed, valid,
*** EOOH ***
Message-ID: <8510291951.AA00320@lmi-cap.ARPA>
Date: Tuesday, 29 October 1985, 16:15-EST
From: sam@guff
Subject: DEFINE-MODIFY-MACRO reverses order of additional args.
To: BUG-LISPM@LMI-Capricorn

In System 102.170, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.9, KERMIT 26.21,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.5, TCP-User 57.7, TCP-Server 33.1,
Experimental LMI Laser Printer II 11.2, DOE-Macsyma 9.15, microcode 768,
Education 2x2, on Emma Willard:


Insert your description of the circumstances here:

(define-modify-macro do-f (a b c) get-new-val "testing")
DO-F

(macroexpand '(do-f place 1 2 3))
(SETQ PLACE (GET-NEW-VAL PLACE 3 2 1))

(macroexpand '(do-f (get 'x 'pname) 1 2 3))
(SI::SETPROP 'X 'PNAME (GET-NEW-VAL (GET 'X 'PNAME NIL) 3 2 1))

;;; In the above cases, I would expect the arguments in the expanded form
;;; to remain in the order 1 2 3 .


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8510290126.AA02491@lmi-cap.ARPA>
Date: Monday, 28 October 1985, 20:27-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: :add-asyncrhonous-character and :remove-asynchronous-character
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 778, LMI Site--mrc 9/12/85,
on Boris Badinoff:

        Remember that little fragment of code from a previous bug
report?

(DEFUN KBD-PROCESS-MAIN-LOOP-INTERNAL (&AUX BUFFER PLIST RAW-P ASYNCH-CHARS TEM
                                       LOWERCASE-CONTROL-CHARS CHAR SOFT-CHAR)
  (WITHOUT-INTERRUPTS
    (SETQ BUFFER (KBD-GET-IO-BUFFER))
    (IF (NULL BUFFER)
        (SETQ ASYNCH-CHARS KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)
      (SETQ PLIST (LOCF (IO-BUFFER-PLIST BUFFER)))
      (SETQ ASYNCH-CHARS
            (IF (GET PLIST ':SUPER-IMAGE)
                NIL
              (GET PLIST ':ASYNCHRONOUS-CHARACTERS KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)))
...

Well, this interacts interestingly with the :add-asynchronous-character
and :remove-asynchronous-character messages.  If you send an
:add-asynchronous-character messsage to a window, only the asynchronous
character you have just added (as well as any other characters you
subsequently add with :add-asynchronous-character) will be handled
asynchronously.  (Since the window's io-buffer now has an
:asynchronous-characters property,
tv:kbd-standard-asynchronous-characters won't be consulted at all.) This
means that CTRL-ABORT, CTRL-META-ABORT, CTRL-BREAK, and CTRL-META-BREAK
no longer work, either asynchronously or synchronously.

Moreover, if you use :remove-asynchronous-character to remove all the
asynchronous character you have added yourself, you will still lose,
since the io-buffer will still have an :asynchronous-characters
property.  At this point, the only way to win is to do something like
the following:

(send (send violated-window :io-buffer) :remprop :asynchronous-characters)

Of course, you could preserve the functionality of the asynchronous
characters specified by tv:kbd-standard-asynchronous-characters by doing
the following:

(send (send window-to-be-munged :io-buffer) :set :get :asynchronous-characters
(cons (new-asynchronous-character-alist-element)
(copy-tree tv:kbd-standard-asynchronous-characters)))

However, the :add-asynchronous-character and
:remove-asynchronous-character operations are supposed to allow you to
add or remove individual characters to the set of asynchronous
characters to be handled by a particular window.  I.e, you're supposed
to be able to say

(send window-to-be-munged :add-asynchronous-character new-asynchronous-character-alist-element)

But this will clobber the asynchronous characters specified by
tv:kbd-standard-asynchronous-characters, as we have just seen.

So, anybody going to fix this?



0,, issues, 3, valid,
*** EOOH ***
Message-ID: <8510290001.AA02352@lmi-cap.ARPA>
Date: Monday, 28 October 1985, 18:54-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Suoer Image Mode redundancy?
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 778, LMI Site--mrc 9/12/85,
on Boris Badinoff:

Are these two pieces of code redundant, or am I missing something?

In package SUPDUP:

(DEFMETHOD (BASIC-NVT :SET-SUPER-IMAGE-MODE) (FLAG)
  (COND (FLAG
         (SETF (TV:IO-BUFFER-OUTPUT-FUNCTION TV:IO-BUFFER) NIL)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER))
                  NIL
                  ':ASYNCHRONOUS-CHARACTERS)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER)) T ':SUPER-IMAGE)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER)) T ':DONT-UPCASE-CONTROL-CHARACTERS))
        (T
         (SETF (TV:IO-BUFFER-OUTPUT-FUNCTION TV:IO-BUFFER) 'TV:KBD-DEFAULT-OUTPUT-FUNCTION)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER))
                  TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS
                  ':ASYNCHRONOUS-CHARACTERS)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER)) NIL ':SUPER-IMAGE)
         (PUTPROP (LOCF (TV:IO-BUFFER-PLIST TV:IO-BUFFER)) NIL
                  ':DONT-UPCASE-CONTROL-CHARACTERS))))

In package TV:

(DEFUN KBD-PROCESS-MAIN-LOOP-INTERNAL (&AUX BUFFER PLIST RAW-P ASYNCH-CHARS TEM
                                       LOWERCASE-CONTROL-CHARS CHAR SOFT-CHAR)
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
...

:SET-SUPER-IMAGE-MODE with a non-nil argument both puts the :SUPER-IMAGE
property on the window's io buffer plist AND sets the
:ASYNCHRONOUS-CHARACTERS property to NIL.  However, if
TV:KBD-PROCESS-MAIN-LOOP-INTERNAL finds :SUPER-IMAGE on the plist, it
will not look for any asynchronous characters anyway.

Similarly, :SET-SUPER-IMAGE-MODE with an argument of NIL both sets the
:SUPER-IMAGE property to NIL and sets the :ASYNCHRONOUS-CHARACTERS
property to TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS.  However, if
TV:KBD-PROCESS-MAIN-LOOP-INTERNAL doesn't find the :SUPER-IMAGE property
on the plist, it will take the characters to be handled asynchronously
from TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS if there is no
:ASYNCHRONOUS-CHARACTERS property.

(1) Is this as redundant as it seems to me?

(2) If it is redundant, was this deliberate or just a case of
reinventing the wheel?


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Mon, 28 Oct 85 08:06:58 est
From: mhd
Posted-Date: Mon, 28 Oct 85 08:06:58 est
Message-Id: <8510281306.AA00351@lmi-cap.ARPA>
To: bug-lispm

To: BUG-LISPM@LMI-Capricorn
--Text Follows This Line--
In Experimental System 107.15, Experimental Local-File 63.0,
Experimental FILE-Server 16.0, Experimental ZMail 62.0,
Experimental KERMIT 27.0, microcode 1290, GC Landscape,
on Maurice Ravel (LAMBDA):

Spurious warnings about binding special instance variables.  These are instance
variables that are declared :special-instance-variables.

Compiling KERMIT: KERMIT; CALLS LISP >
<< While compiling (:METHOD KSTATE :SIMPLE-RECEIVE) >>
 Binding *TTYFD*, which has the same name as an instance variable of flavor KSTATE
<< While compiling (:METHOD KSTATE :SIMPLE-SEND) >>
 Binding *FILNAM*, which has the same name as an instance variable of flavor KSTATE
 Binding *FILELIST*, which has the same name as an instance variable of flavor KSTATE
 Binding *TTYFD*, which has the same name as an instance variable of flavor KSTATE
...


-mhd


0,, issues, 3, valid,
*** EOOH ***
Date: Monday, 14 October 1985, 10:51-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn


Forwarded from Rob Pettengill at Mcc

Since this is loaded in your released system you might as well have it work ...

-rob

;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by rcp
;;; Reason:
;;;  Bug fixes for hacks:alarm
;;;  Calls to DELETE-ELEMENT have N replaced by (1- N) so that the number is
;;;  as printed.
;;;  (HOSTS PRINT) was sending :name to a chaos address - printing the chaos
;;;  address is ugly but keeps it from blowing up ...
;;;  VIEW-ALARM and REMOVE-ALARM are fixed to properly print the alarm contents
;;; Written 1-Oct-85 16:17:38 by rcp,
;;; while running on Rob's friend Zonker from band 2
;;; with System 102.159, Local-File 56.11, FILE-Server 13.2, Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.6, KERMIT 26.20, MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0, Experimental window-maker 1.0, Experimental MCC 3.7, Experimental MICRO-COMPILATION-TOOLS 3.0, Experimental LM-Prolog 1.0, Experimental ObjectLISP 1.0, TCP-Kernel 30.3, TCP-User 57.1, TCP-Server 33.0, microcode 773, CAD Base Band with the Microcompiler, Prolog, Oblisp, and TCP.



; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN (FILE REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT (1- N) FILES-TO-BE-MONITORED)))
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN (MAIL REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT (1- N) MAIL-CHECK-USERS)))
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN (TIME REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT (1- N) ALARM-TIMES)))
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN (HOSTS PRINT) (STREAM &AUX (N 0))
  (FORMAT STREAM "~%HOSTS Alarms:")
  (IF (NULL HOSTS-TO-CHECK)
      (FORMAT STREAM "  You are not monitoring the status of any hosts.~%")
    (DOLIST (HOST HOSTS-TO-CHECK)
      (FORMAT STREAM "~%[~A] You will be notified if the status of host ~A changes."
;             (INCF N) (SEND HOST ':NAME)))))
              (INCF N) HOST))))
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN (HOSTS REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT (1- N) HOSTS-TO-CHECK)))
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN VIEW-ALARM (ALARM)
;  ALARM)
  (FUNCALL (GET ALARM 'PRINT) STANDARD-OUTPUT))

;;greatly improve the user interface
)
))

; From file ALARM.LISP#> QL.DEMO; ZONK: (52)
#8R HACKS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "HACKS")))
  (COMPILER#:PATCH-SOURCE-FILE "ZONK: QL.DEMO; ALARM.#"


(compiler-let ((prolog:*file-default-options* '(:OPTIONS (:WORLD :EST))))

(DEFUN REMOVE-ALARM (&OPTIONAL ALARM ALARM-NUMBER CONFIRM)
  "Remove a specific alarm.  Asks the user for confirmation."
  ;;cond-every !
  (COND ((NULL ALARM)
         (FORMAT QUERY-IO "~%Please type in the name an alarm (or just return to quit).
Valid alarms are ~A." (PRINT-LIST ALARM-TYPE-LIST QUERY-IO))
         (SETQ ALARM (READLINE QUERY-IO))))
  (COND ((AND (NOT (NULL ALARM)) (NULL ALARM-NUMBER))
         (FUNCALL (GET ALARM 'PRINT) STANDARD-OUTPUT)
         (FORMAT QUERY-IO "Please type the number of the ~A alarm that you want to be rid of." ALARM)
         (SETQ ALARM-NUMBER (PARSE-NUMBER (READLINE QUERY-IO)))))
  (COND ((AND (NOT (NULL ALARM)) ( 0 ALARM-NUMBER))
         (IF (NULL CONFIRM)
             (SETQ CONFIRM (Y-OR-N-P
                             (FORMAT NIL "Do you really want to remove yourself of alarm number ~A?" ALARM-NUMBER))))
         (IF CONFIRM (REMOVE-ALARM-INTERNAL ALARM ALARM-NUMBER)))))

)
))




0,, 3, valid, indeterminate,
*** EOOH ***
Date: Saturday, 12 October 1985, 18:48-EDT
From: Sam@Guff
Subject: Formatting Backquote Macros
To: BUG-LISPM@LMI-Capricorn

In System 102.168, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.8, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.5, TCP-User 57.2, TCP-Server 33.0,
Experimental LMI Laser Printer II 11.2, DOE-Macsyma 9.11, microcode 773,
Education 2x2, on McGuffey:


Insert your description of the circumstances here:
[  >>Keyboard break.  ]


The value of

        `(`(f ,g ,@h . beef))

is

        ((SI::XR-BQ-LIST* (QUOTE F) G (SI::XR-BQ-APPEND H (QUOTE BEEF))))

which formatted (using m-X FORMAT CODE) is

        (`(F ,G ,@H) )

.

Where's the BEEF?
According to CommonLisp, it should be there!

The following variant is OK:

(setq beef "Where's the BEEF?")

        `(`(f ,g ,@h . ,beef))
   -->  ((SI::XR-BQ-LIST* (QUOTE F) G (SI::XR-BQ-APPEND H BEEF)))
   ===  (`(F ,G ,@H . ,BEEF) )



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Saturday, 5 October 1985, 18:38-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: ZMail SAPFu in saving primary mail file (2 bugsd for the price of 1 message)
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Ten:


Insert your description of the circumstances here:

I clicked R on [Get New Mail] and specified CAP:
/lmi/rjpi/mail/3-85-bug-lispm.  ZMail hung at 0% read so I tried to
abort out of that state.  After enormous difficulties I succeeded and I
got a notification saying [Chaosnet trouble CAP:
//lmi//rjpi//mail//3-85-bug-lispm closed].  I then tried to save my mail
buffers, and got into this error state when ZMail tried to save my
primary mail file.  (That's bug one)

Bug two is multifaceted.  ZMail hung, so, to save my mail buffers, I got
into a LISP Listener and evaluated (zwei:zmail-save-all-files).
Unfortunatley, the output from this function, querying me about each
buffer, was directed to a typeout window in the ZMail frame.  Smartly
done!

>>ERROR: Attempt to read from #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//3-85-bug-lispm" 77702657>, which is closed.
Backtrace from the debugger:

FS::QFILE-NEXT-READ-PKT (P.C. = 150)

 Arg 0 (NO-HANG-P): NIL
 Arg 1 (FOR-SYNC-MARK-P): NIL
Local 0 (.SELECTQ.ITEM.): :CLOSED
Local 1 (PKT): NIL
Local 2 (.SELECTQ.ITEM.): NIL


(:METHOD FS::QFILE-INPUT-STREAM-MIXIN :GET-NEXT-INPUT-PKT) (P.C. = 31)
  (SELF is #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//3-85-bug-lispm" 77702657>)

 Arg 0 (.OPERATION.): :GET-NEXT-INPUT-PKT
 Arg 1 (NO-HANG-P): NIL
Local 0: NIL


Additional information supplied with call:
 Expecting 3 values

(:METHOD CHAOS:CHARACTER-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (P.C. = 24)
  (SELF is #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//3-85-bug-lispm" 77702657>)

 Arg 0 (.OPERATION.): :NEXT-INPUT-BUFFER
 Arg 1 (NO-HANG-P): NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD SI:BASIC-BUFFERED-INPUT-STREAM :SETUP-NEXT-INPUT-BUFFER) (P.C. = 35)
  (SELF is #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//3-85-bug-lispm" 77702657>)

 Arg 0 (.OPERATION.): :SETUP-NEXT-INPUT-BUFFER
   --Defaulted args:--
 Arg 1 (NO-HANG-P): NIL


(:METHOD FS::QFILE-INPUT-CHARACTER-STREAM :COMBINED :SETUP-NEXT-INPUT-BUFFER) (P.C. = 39)
  (SELF is #<FS::QFILE-INPUT-CHARACTER-STREAM "CAP: //lmi//rjpi//mail//3-85-bug-lispm" 77702657>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:SETUP-NEXT-INPUT-BUFFER)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-4 22643566>


Remainder of stack:

(:METHOD SI::BUFFERED-LINE-INPUT-STREAM :LINE-IN) (P.C. = 40)
(:METHOD ZWEI::ZMAIL-DISK-BUFFER :READ-NEXT-MSG) (P.C. = 210)
ZWEI::LOAD-ALL-MSGS (P.C. = 32)
ZWEI::ASSURE-ZMAIL-BUFFER-FULLY-LOADED (P.C. = 67)
ZWEI::FOREGROUND-BACKGROUND-FINISH (P.C. = 129)
ZWEI::SAVE-ZMAIL-BUFFER (P.C. = 54)
(:INTERNAL ZWEI::ZMAIL-SAVE-ALL-FILES 0) (P.C. = 51)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-FRAME :FUNCALL-INSIDE-YOURSELF) (P.C. = 22)
ZWEI::ZMAIL-SAVE-ALL-FILES (P.C. = 23)
...
(:METHOD ZWEI::ZMAIL-SUMMARY-SCROLL-WINDOW :REDISPLAY-AS-NECESSARY) (P.C. = 51)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN-WITH-SUMMARY :REDISPLAY) (P.C. = 39)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 141)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
Date: Saturday, 5 October 1985, 17:56-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Loading ZMAIL.INIT
To: BUG-ZMAIL@LMI-Capricorn

In ZMAIL in System 102.122, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Ten:

If you use ZMail once (causing it to read in your init file), then kill
off your file buffers, and logout, then later log back in to the same
machine and type G or click L on [Get New Mail], ZMail will look for
your mail file on the associated machine, rather than on the mail server
specified in your .INIT file.  Either ZMail should reload your
ZMAIL.INIT file or it should be less brain damaged about remembering
your mail file.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 3 October 1985, 11:44-EDT
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Fifteen B:

If you send a :primitive-item-outside message to a tv:basic-mouse-sensitive-items
type window, and the top argument is > the bottom argument (i.e. the message wraps
around), you neither get an error nor do you get a mouse sensitive region which
wraps around.  You get nothing.  This behaviour is undocumented.

-mhd


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 3 October 1985, 00:39-EDT
From: bobp@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 104.112, Experimental MagTape 2.0,
Experimental FILE-Server 15.0, Experimental Local-File 58.1,
Experimental ZMail 58.0, Experimental Unix-Interface 6.0, microcode 1287,
104.111 Zmail, on BobP:


Insert your description of the circumstances here:
When warm-booting, if the lambda doesn't "own" the ethernet, it
fails trying to get the time over the net, but after it prints
the herald, (time:initialize-timebase) works fine.

>>Keyboard break.
Backtrace from the debugger:

PROCESS-WAIT (P.C. = 36)

 Arg 0 (WHOSTATE): "Keyboard"
 Arg 1 (FUNCTION): #<DTP-FEF-POINTER (:INTERNAL TV:KBD-IO-BUFFER-GET 0) 34016062>
 Rest arg (ARGUMENTS): (#<TV:IO-BUFFER 36004773: empty, State: NIL>)
Local 1 (STATE): 7


TV:KBD-IO-BUFFER-GET (P.C. = 132)

 Arg 0 (BUFFER): #<TV:IO-BUFFER 36004773: empty, State: NIL>
   --Defaulted args:--
 Arg 1 (NO-HANG-P): NIL
 Arg 2 (WHOSTATE): "Keyboard"
Local 0 (UPDATE-STATE-P): NIL
Local 1 (OK): NIL
Local 2 (ELT): NIL


(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 81)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 1 41500000 exposed>)

 Arg 0 (.OPERATION.): :ANY-TYI
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
Local 0 (IDX): 0
Local 1 (CHAR): NIL
Local 2 (STRING): NIL
Local 3 (INDEX): NIL


TV::ALTERNATE-RUBOUT-HANDLER (P.C. = 351)

Local 0 (CH): 134
Local 1 (CH-CHAR): 134
Local 2 (CH-CONTROL-META): 0
Local 3 (COMMAND): (134 . TV::RH-COM-BASIC-HELP)
Local 4 (FILL-POINTER): 0
Local 5 (TYPEIN-POINTER): 0
Local 6 (STATUS): NIL
Local 7 (RUBBED-OUT-SOME): NIL
Local 8 (NUMERIC-ARG): NIL
Local 9 (NUMERIC-ARG-NEGATIVE): NIL
Local 10 (PROMPT-OPTION): NIL
Local 11 (INITIAL-INPUT): NIL
Local 12 (INITIAL-INPUT-POINTER): NIL
Local 13 (EDITING-COMMAND): NIL
Local 14 (DO-NOT-ECHO): NIL
Local 15 (PASS-THROUGH): NIL
Local 16 (COMMAND-HANDLER): NIL
Local 17 (PREEMPTABLE): NIL
Local 18 (BLIP-HANDLER): NIL
Local 19 (ACTIVATION-HANDLER): (:ACTIVATION MEMQ (141 148))
Local 20 (VALUE): NIL


(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 114)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 1 41500000 exposed>)

 Arg 0 (.OPERATION.): :ANY-TYI
 Arg 1 (IGNORE): T
Local 0 (IDX): 0
Local 1 (CHAR): NIL
Local 2 (STRING): NIL
Local 3 (INDEX): NIL


Remainder of stack:

(:INTERNAL READ-DELIMITED-STRING SI::.DO.IT.) (P.C. = 51)
(:METHOD TV:STREAM-MIXIN :RUBOUT-HANDLER) (P.C. = 198)
READ-DELIMITED-STRING (P.C. = 183)
READLINE (P.C. = 49)
TIME:INITIALIZE-TIMEBASE (P.C. = 117)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
EVAL (P.C. = 82)
INITIALIZATIONS (P.C. = 95)
LISP-REINITIALIZE (P.C. = 994)
SI:LISP-TOP-LEVEL (P.C. = 30)


0,, fixed, 3, valid,
*** EOOH ***
From: debbie
Date: Wednesday, 2 October 1985, 14:32-EDT
To: bug-lispm

Writing two lmc partitions to tape [using fs:mt-write-partition]
and then restoring them [using fs:restore-magtape] does not
work properly. If you respond yes to restore the first partition,
it doesn't ask if you want to restore the second partition
It just skips over it.  [fs:magtape-list-files] lists both
partitions.
.
.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 30 September 1985, 17:48-EDT
From: Janet E. Ressler <jer@LMI-CAPRICORN>
Subject: Brain Death
To: BUG-ZMail@LMI-Capricorn

In System 102.162, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.7, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0,
Experimental IRIS 1.0, DOE-Macsyma 9.9, Macsyma-Help-Database 1.1,
microcode 778, on The Importance of Being Earnest:


Insert your description of the circumstances here:

Got this by clicking right on [Save All Files]

>>TRAP 10602 (ILLEGAL-INSTRUCTION)
There was an attempt to execute an invalid instruction: 15724.
Backtrace from the debugger:

TV::DRAW-CHOICE-BOX (P.C. = 131)

 Arg 0 (SHEET): #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 1732005 exposed>
 Arg 1 (X): 276
 Arg 2 (Y): 18
 Arg 3 (ON-P): T
   --Defaulted args:--
 Arg 4 (SIZE): 12
Local 0 (WIDTH): 3
Local 1 (CHAR-ALUF): 7
Local 2 (ERASE-ALUF): 2
Local 3 (TEM): 6
Local 4 (X1): 279
Local 5 (Y1): 21
Local 6 (X2): 285
Local 7 (Y2): 27


(:METHOD TV:BASIC-MULTIPLE-CHOICE :PRINT-ITEM) (P.C. = 42)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 1732005 exposed>)

 Arg 0 (.OPERATION.): :PRINT-ITEM
 Arg 1 (ITEM): (#<ZWEI::BABYL-MAIL-FILE-BUFFER jer.bb /lmi/jer/ CAP: 67316372> "jer.bb //lmi//jer// CAP:" (** ** **))
 Arg 2 (LINE-NO): 0
 Arg 3 (ITEM-NO): 0
Local 0: ((:SAVE T TV::MULTIPLE-CHOICE-CHOOSE 276 ...) (:EXPUNGE T TV::MULTIPLE-CHOICE-CHOOSE 224 ...))
Local 1 (BOX): (:SAVE T TV::MULTIPLE-CHOICE-CHOOSE 276 ...)


(:METHOD TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW :COMBINED :PRINT-ITEM) (P.C. = 44)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 1732005 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:PRINT-ITEM (#<ZWEI::BABYL-MAIL-FILE-BUFFER jer.bb /lmi/jer/ CAP: 67316372> "jer.bb //lmi//jer// CAP:" **) 0 0)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-7 23106731>


(:METHOD TV:TEXT-SCROLL-WINDOW :REDISPLAY) (P.C. = 57)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 1732005 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
 Arg 1 (START): 0
 Arg 2 (END): 2
Local 0 (I): 0
Local 1 (J): 0
Local 2 (LIM): 2


(:METHOD TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW :COMBINED :REDISPLAY) (P.C. = 42)
  (SELF is #<TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW Temporary Multiple Choice Window 1 1732005 exposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:REDISPLAY 0 2)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-7 23106731>


Remainder of stack:

(:METHOD TV:TEXT-SCROLL-WINDOW :AFTER :REFRESH) (P.C. = 47)
(:METHOD TV:TEMPORARY-MULTIPLE-CHOICE-WINDOW :COMBINED :REFRESH) (P.C. = 51)
(:METHOD TV:SHEET :EXPOSE) (P.C. = 408)
(:INTERNAL (:METHOD TV:WINDOW :COMBINED :EXPOSE) 0) (P.C. = 46)
TV::SHEET-EXPOSE (P.C. = 112)
(:METHOD TV:WINDOW :COMBINED :EXPOSE) (P.C. = 31)
TV:EXPOSE-WINDOW-NEAR (P.C. = 317)
(:METHOD TV:BASIC-MULTIPLE-CHOICE :CHOOSE) (P.C. = 56)
TV:MULTIPLE-CHOOSE (P.C. = 126)
ZWEI::ZMAIL-SAVE-MENU (P.C. = 106)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
Date: Monday, 30 September 1985, 12:16-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.168, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.8, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.5, TCP-User 57.2, TCP-Server 33.0,
Experimental LMI Laser Printer II 11.2, DOE-Macsyma 9.11, microcode 773,
Education 2x2, on Emma Willard:


Insert your description of the circumstances here:

Allocating the following resource fails.
Should defresource be able to handle &key arguments?

(defresource foo (&key bar)
        :constructor (make-array bar))

>>ERROR: No argument after keyword 3
Backtrace from the debugger:

SYSTEM:APPLY-LAMBDA (P.C. = 963)

 Arg 0 (FCTN): (NAMED-LAMBDA (** **) (IGNORE &KEY BAR) (DECLARE) ...)
 Arg 1 (A-VALUE-LIST): (#S(SI::RESOURCE :NAME FOO :PARAMETIZER ...) 3)
   --Defaulted args:--
 Arg 2 (ENVIRONMENT): NIL
Local 0 (TEM): NIL
Local 1 (OPTIONALF): NIL
Local 2 (QUOTEFLAG): NIL
Local 3 (TEM): (NIL)
Local 4 (RESTF): NIL
Local 5 (INIT): NIL
Local 6 (THIS-RESTF): NIL
Local 7 (SPECIALF): NIL
Local 8 (FCTN1): ((** **) (IGNORE &KEY BAR) (DECLARE) BAR ...)
Local 9 (LAMBDA-LIST): NIL
Local 10 (BODY): ((DECLARE) BAR (MAKE-ARRAY BAR))
Local 11 (VALUE-LIST): (3)
Local 12 (THISVAL): (13878 . 44)
Local 13 (KEYNAMES): (BAR)
Local 14 (KEYINITS): NIL
Local 15 (KEYKEYS): (:BAR)
Local 16 (KEYFLAGS): (NIL)
Local 17 (KEYNAMES1): NIL
Local 18 (KEYKEYS1): NIL
Local 19 (KEYFLAGS1): NIL
Local 20 (UNSPECIFIED): (NIL)
Local 21 (ALLOW-OTHER-KEYS): NIL
Local 22 (THISVAR): IGNORE
Local 23 (VARS-ENV): (NIL)
Local 24 (TAIL): NIL
Local 25 (TEM1): (13878 . 44)
Local 26 (VARS-ENV): (NIL NIL)
Local 27: NIL
Local 28 (ARGS): NIL
Local 29 (MUMBLE): NIL
Local 30 (X): (3)
Local 31 (KEYWORD): NIL
Local 32: NIL
Local 33 (L): NIL
Local 34: NIL
Local 35 (ARG): NIL


(:PROPERTY FOO SI::RESOURCE-CONSTRUCTOR):
   Arg 0 (IGNORE): #S(SI::RESOURCE :NAME FOO :PARAMETIZER ...)
   Arg 1: 3


ALLOCATE-RESOURCE (P.C. = 263)

 Arg 0 (RESOURCE-NAME): FOO
 Rest arg (PARAMETERS): (:BAR 3)
Local 1 (RESOURCE): #S(SI::RESOURCE :NAME FOO :PARAMETIZER ...)
Local 2 (PARAMS): (3)
Local 3 (TEM): NIL
Local 4 (INDEX): NIL
Local 5 (OLD): NIL
Local 6 (INITIALIZER): NIL
Local 7 (CHECKER): NIL
Local 8 (MATCHER): NIL
Local 9 (CELL): NIL
Local 10 (OBJ): NIL
Local 11 (N-OBJECTS): 0
Local 12 (N): -1
Local 13 (IN-USE-P): NIL


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (ALLOCATE-RESOURCE (QUOTE FOO) :BAR 3)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 3
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER ALLOCATE-RESOURCE 23461023>
Local 6 (ARG-DESC): 1179713
Local 7 (NUM-ARGS): 3
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (ALLOCATE-RESOURCE (QUOTE FOO) :BAR 3)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Remainder of stack:

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Sunday, 29 September 1985, 10:20-EDT
From: Janet E. Ressler <jer@LMI-CAPRICORN>
Subject: Saving Out Profile From ZMail
To: BUG-LISPM@LMI-Capricorn

In System 102.162, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.7, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0,
Experimental IRIS 1.0, DOE-Macsyma 9.9, Macsyma-Help-Database 1.1,
microcode 778, on The Importance of Being Earnest:


Insert your description of the circumstances here:

Got this by clicking left on the bottom pane in the ZMail profile, then
clicking right on [Save] and selecting [Save File].

>>ERROR: The object #<ZMAIL-PROFILE-BUFFER ZMAIL.INIT#> JER; LAD: 33253353> received a :WRITE-FILE-INTERNAL message, which went unclaimed.
The rest of the message was (#FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#>").
Backtrace from the debugger:

#<ZMAIL-PROFILE-BUFFER ZMAIL.INIT#> JER; LAD: 33253353>:
   Arg 0: :WRITE-FILE-INTERNAL
   Arg 1: #FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#>"


SI::INSTANCE-HASH-FAILURE (P.C. = 116)

 Arg 0 (OP): :WRITE-FILE-INTERNAL
 Rest arg (ARGS): (#FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#>")
Local 1 (HT): #<EQ-SI::HASH-ARRAY (Funcallable) 6455651>
Local 2 (FN-LOCATION): NIL
Local 3 (FUNC): NIL
Local 4 (NEWHT): NIL
Local 5: NIL


WRITE-FILE-INTERNAL (P.C. = 27)

 Arg 0 (PATHNAME): #FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#>"
 Arg 1 (BUFFER): #<ZMAIL-PROFILE-BUFFER ZMAIL.INIT#> JER; LAD: 33253353>


SAVE-BUFFER (P.C. = 116)

 Arg 0 (BUFFER): #<ZMAIL-PROFILE-BUFFER ZMAIL.INIT#> JER; LAD: 33253353>
Local 0 (FILE-ID): (#FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#1" . 2689181524)
Local 1 (PATHNAME): #FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#>"
Local 2 (FILE-FILE-ID): (#FS::LM-PATHNAME "LAD: JER; ZMAIL.INIT#1" . 2689181524)
Local 3: #<FS::QFILE-PROBE-STREAM "LAD: JER; ZMAIL.INIT#>" 36710457>
Local 4 (.FILE-ABORTED-FLAG.): NIL
Local 5 (S): #<FS::QFILE-PROBE-STREAM "LAD: JER; ZMAIL.INIT#>" 36710457>


COM-SAVE-FILE (P.C. = 41)



Remainder of stack:

PROFILE-SAVE-BUTTON (P.C. = 90)
(:SELECT-METHOD ZMAIL-PROFILE-COMMAND-LIST :MOUSE-BUTTON) (P.C. = 389)
(:METHOD ZMAIL-PROFILE-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
COM-ZMAIL-PROFILE (P.C. = 188)
COMMAND-EXECUTE (P.C. = 88)
...
(:SELECT-METHOD ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, doc.prob, issues, valid,
*** EOOH ***
Date: Friday, 27 September 1985, 02:26-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: :initial-input-index
To: BUG-LISPM@LMI-Capricorn
CC: rjpi@angel

In System 102.133, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.10, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI Site--mrc 9/12/85,
on Natasha Nogoodnik:

The :initial-input-index option to the with-input-editing function and
the :rubout-handler operation on tv:stream-mixin doesn't appear to do
what is advertised.  Take the trivial case:

(with-input-editing (*terminal-io* '((:initial-input "(") (:initial-input-index 1)))
(read))

( is inserted in the input buffer and echoed, but the cursor appears
over it, rather than to the right of it, just as it is if the
:initial-input-index option is omitted.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Wednesday, 25 September 1985, 00:14-EDT
From: rjpi@CAP
Sender: Ingria@LMI-LAMBDA-3
Subject: tv:sheet-bounds-within-sheet-p
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI Site--mrc 9/12/85,
on Natasha Nogoodnik:

(send ll2 :edges) =>
185
486
578
846

(tv:sheet-bounds-within-sheet-p 200 500 560 800 ll2) =>
NIL

If ll3 is bound to a window with those edges:

(tv:sheet-within-sheet-p ll3 ll2) =>
NIL

This is a bug.  (These are bugs?)


0,, issues, 3, valid,
*** EOOH ***
Date: Monday, 23 September 1985, 15:46-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Current Package in ZMACS
To: BUG-ZWEI@LMI-Capricorn

In ZWEI in System 102.117, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI Site--mrc 9/12/85,
on Boris Badinoff:

If I have read a file, with a Package: specification in its file
attribute list and then read in a file (say a text---non-source
code---file) the current package in ZMACS is set in the new buffer to
the package of the previous buffer.   Example: the ZMACS I am using had
the file FED.LISP in the (then) currently selected buffer.  I read in a
BoTeX file and the buffer it was read into also (and unbeknownst to me)
had FED as its package.  I got into a BREAK loop several times, defined
variables, etc.  When I selected a LISP Listener and tried to use the
values of the variables, they were listed as unbound. I entered the
debugger and was offered the opportunity to use the correspondingly
named variables in the FED package.

This is not the first time I've gotten screwed by this.  Is there any
reason why a buffer created by reading in a file should take its package
from the previously selected buffer, rather than defaulting to USER?



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 19 September 1985, 21:14-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.166, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.8, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.5, TCP-User 57.2, TCP-Server 33.0,
Experimental LMI Laser Printer II 11.2, Experimental Object Lisp 4.0,
Experimental ObjectLISP 1.0, Experimental vista 1.0, DOE-Macsyma 9.9,
microcode 772, Education Division 2x2+, on Bedlam:


Insert your description of the circumstances here:

In the font editor, if you select a big font (like 43vxms)
and then select a character to edit, the following error
results.

There is also a problem in the way output is displayed in the
label window , when the following sequence is executed.
Bring up a font editor window and shape it to be as big
as the whole screen (I'm on a landscape). Then mouse
on FONT, and then on BIGFNT . You should see that parts of the
label are missing.

>>TRAP 25560 (TV-ERASE-OFF-SCREEN)
An attempt was made to do graphics past the end of the screen.
Backtrace from the debugger:

(:METHOD FED::BASIC-FED :TYO-EDITED-CHAR) (P.C. = 128)
  (SELF is #<FED Fed 1 1733372 exposed>)

 Arg 0 (.OPERATION.): :TYO-EDITED-CHAR
 Arg 1 (SHEET): #<FED::FED-LABEL-WINDOW Fed Label Window 1 1733737 exposed>
Local 0 (LEFT): 62
Local 1 (TOP): 2
Local 2 (PLANE-WIDTH): 61
Local 3 (PLANE-TOP): 0
Local 4 (PLANE-BOTTOM): 53
Local 5 (HPOS): 2
Local 6: 61
Local 7 (VPOS): 41


FED::FED-TYO (P.C. = 36)

 Arg 0 (SHEET): #<FED::FED-LABEL-WINDOW Fed Label Window 1 1733737 exposed>
 Arg 1 (CH): 113
 Arg 2 (FONTNAME): FONTS:43VXMS
Local 0 (ELTS): ((FONTS:43VXMS 113 #<FED Fed 1 1733372 exposed>))
Local 1 (ELT): (FONTS:43VXMS 113 #<FED Fed 1 1733372 exposed>)


FED::DISPLAY-LABEL (P.C. = 263)

Local 0 (SECOND-LINE-HEIGHT): 53
Local 1 (FD): #S(FED::FONT-DESCRIPTOR :FD-FILL-POINTER 200 :FD-NAME ...)
Local 2 (OLD-X): 42
Local 3 (I): NIL
Local 4: NIL


FED::REDISPLAY-LABELS (P.C. = 27)

Local 0: NIL
Local 1 (ELT): NIL


(:METHOD FED :BEFORE :REDISPLAY) (P.C. = 64)
  (SELF is #<FED Fed 1 1733372 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
 Rest arg (IGNORE): NIL


Remainder of stack:

(:METHOD FED :COMBINED :REDISPLAY) (P.C. = 43)
(:METHOD FED :COMMAND-LOOP) (P.C. = 107)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, fixed, 3, valid,
*** EOOH ***
Date: Tuesday, 17 September 1985, 13:40-EDT
From: wilde@bedlam
Sender: @LMI-MCGUFFEY
Subject: graphic(s) operations bug
To: BUG-LISPM@LMI-Capricorn

In System 102.165, Local-File 56.12, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.8, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
TCP-Kernel 30.3, TCP-User 57.1, TCP-Server 33.0,
Experimental LMI Laser Printer II 11.2, Experimental Object Lisp 4.0,
Experimental vista 2.0, microcode 778, Education Division 2x2+,
on Bedlam:


Insert your description of the circumstances here:
:draw-triangle and other select graphics operations (eg. :draw-regular-polygon) do
not seem to be supported by microcode 778.  These operations worked as expected
up through atleast 102.157 which goes with ucode 772.

(send *terminal-io* :draw-triangle 100 100 100 200 300 200)

>>TRAP 10602 (ILLEGAL-INSTRUCTION)
There was an attempt to execute an invalid instruction: 0.
Backtrace from the debugger:

(:METHOD TV:GRAPHICS-MIXIN :DRAW-TRIANGLE) (P.C. = 89)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 1 1700000 exposed>)

 Arg 0 (.OPERATION.): :DRAW-TRIANGLE
 Arg 1 (X1): 100
 Arg 2 (Y1): 100
 Arg 3 (X2): 100
 Arg 4 (Y2): 200
 Arg 5 (X3): 300
 Arg 6 (Y3): 200
   --Defaulted args:--
 Arg 7 (ALU): 7
Local 0 (INHIBIT-SCHEDULING-FLAG): T


FUNCALL (P.C. = 21)

 Arg 0 (FN): #<TV:LISP-LISTENER Lisp Listener 1 1700000 exposed>
 Rest arg (ARGS): (:DRAW-TRIANGLE 100 100 200 ...)


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (FUNCALL *TERMINAL-IO* :DRAW-TRIANGLE 100 ...)
 Arg 1 (NOHOOK): T
Local 0 (ARGNUM): 8
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER FUNCALL 2545325>
Local 6 (ARG-DESC): 1048641
Local 7 (NUM-ARGS): 8
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SYSTEM:EVAL1 (P.C. = 305)

 Arg 0 (FORM): (SI::DISPLACED (SEND *TERMINAL-IO* :DRAW-TRIANGLE 100 ...) (FUNCALL *TERMINAL-IO* :DRAW-TRIANGLE 100 ...))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): NIL
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): (MACRO . #<DTP-FEF-POINTER SEND 23271326>)
Local 6 (ARG-DESC): 262207
Local 7 (NUM-ARGS): NIL
Local 8: ((FUNCALL *TERMINAL-IO* :DRAW-TRIANGLE 100 ...) T)
Local 9: (NIL)
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (SI::DISPLACED (SEND *TERMINAL-IO* :DRAW-TRIANGLE 100 ...) (FUNCALL *TERMINAL-IO* :DRAW-TRIANGLE 100 ...))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Remainder of stack:

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 16 September 1985, 20:18-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: ``Node inferiors messed up, please report to BUG-ZMAIL''
To: BUG-ZMail@LMI-Capricorn

In ZMail in System 102.117, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI Site--mrc 9/12/85,
on Boris Badinoff:


Insert your description of the circumstances here:


>>ERROR: Node inferiors messed up, please report to BUG-ZMAIL
Backtrace from the debugger:

ZWEI:ZMAIL-ERROR (P.C. = 33)

 Arg 0 (FORMAT-STRING): "Node inferiors messed up, please report to BUG-ZMAIL"
 Rest arg (ARGS): NIL


ZWEI:EXPUNGE-ZMAIL-BUFFER (P.C. = 158)

 Arg 0 (ZMAIL-BUFFER): #<ZWEI:UNIX-MAIL-FILE-BUFFER typography /lmi/rjpi/mail/ CAP: 64674134>
   --Defaulted args:--
 Arg 1 (DELETED-MSGS): T
Local 0 (ARRAY): #<ART-Q-64 64674205>
Local 1 (INFS): #<DTP-LOCATIVE 64674143>
Local 2 (*INTERVAL*): #<ZWEI:UNIX-MAIL-FILE-BUFFER typography /lmi/rjpi/mail/ CAP: 64674134>
Local 3 (NMSGS): 20
Local 4 (I): 0
Local 5 (MSG): #S(ZWEI:MSG :REAL-INTERVAL #<ZWEI:NODE 65132006> :INTERVAL ...)
Local 6 (REAL-INT): #<ZWEI:NODE 65132006>
Local 7 (INT): #<ZWEI:NODE 64674525>


ZWEI:ZMAIL-SAVE-ALL (P.C. = 35)

Local 0: (#<ZWEI:UNIX-MAIL-FILE-BUFFER typography /lmi/rjpi/mail/ CAP: 64674134> #<ZWEI:BABYL-MAIL-FILE-BUFFER xmail.bb /lmi/rjpi/mail/ CAP: 64502277> #<ZWEI:UNIX-MAIL-FILE-BUFFER macintosh /lmi/rjpi/mail/ CAP: 65114454> #<ZWEI:BABYL-MAIL-FILE-BUFFER humor.bb /lmi/rjpi/mail/ CAP: 65170136> ...)
Local 1 (ZMAIL-BUFFER): #<ZWEI:UNIX-MAIL-FILE-BUFFER typography /lmi/rjpi/mail/ CAP: 64674134>


ZWEI:COM-ZMAIL-SAVE (P.C. = 53)



ZWEI:COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI:COM-ZMAIL-SAVE
 Arg 1 (CHAR): (:MENU ("Save Files" . ZWEI:COM-ZMAIL-SAVE) 1 #<ZWEI:ZMAIL-MAIN-COMMAND-MENU-PANE Zmail Main Command Menu Pane 1 1706553 exposed>)
   --Defaulted args:--
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
Local 0 (HOOK-SUCCESS): T
Local 1: NIL
Local 2 (HOOK): NIL


Remainder of stack:

ZWEI:ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZWEI:ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI:ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI:ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Thursday, 5 September 1985, 12:05-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn


(send terminal-io :string-out-x-y-centered-explicit "string" 0 200 800 500
        fonts:cptfontb tv:alu-xor)
is supposed to center the string horizontally (which works)
and vertically (which doesn't). In the above example it should be centered
vertically between 200 and 500, but it is written at 243.
The following change appears to fix this, (it is now output at 343.)

(DEFUN SHEET-DISPLAY-X-Y-CENTERED-STRING (SHEET STRING
                                          &OPTIONAL
                                          (LEFT (SHEET-INSIDE-LEFT SHEET))      ;
                                          (TOP (SHEET-INSIDE-TOP SHEET))
                                          (RIGHT (SHEET-INSIDE-RIGHT SHEET))
                                          (BOTTOM (SHEET-INSIDE-BOTTOM SHEET))
                                          (FNT (SHEET-CURRENT-FONT SHEET))
                                          (ALU (SHEET-CHAR-ALUF SHEET))
                                          (START 0) END
                                          (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET)))
  "Display STRING on SHEET centered in both X and Y, in font FNT.
It is centered horizontally between LEFT and RIGHT,
vertically between TOP and BOTTOM.
All four coordinates are relative to SHEET's outside edges.
SHEET's cursor is not used or moved."
  (LET ((WID (- RIGHT LEFT)))
    (MULTIPLE-VALUE-BIND (NIL SHEI SLEN SWID)
        (SHEET-COMPUTE-MOTION SHEET LEFT TOP STRING START END T
                              0 (+ top (sheet-inside-height sheet))
                              30000000 NIL FNT MULTI-LINE-LINE-HEIGHT)
;       (SHEET-STRING-LENGTH SHEET STRING START END WID FNT)
      (UNLESS (NUMBERP SLEN) (SETQ SLEN NIL))
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING
                (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0))
                (MAX (- (TRUNCATE (+ TOP BOTTOM) 2)
;;;                     (TRUNCATE SHEI 2))          ;changed here: removed this and
                        (TRUNCATE (- SHEI top) 2))      ;added this ...
                                        TOP)
                                   RIGHT BOTTOM
                                   FNT ALU START SLEN MULTI-LINE-LINE-HEIGHT))))


0,, 3, valid,
*** EOOH ***
Date: Wednesday, 31 July 1985, 19:25-EDT
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Fifteen B:

Meta-break is undefined in zmacs.


0,, 3, valid,
*** EOOH ***
Date: Monday, 29 July 1985, 19:26-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Problem While Sending Message
To: BUG-ZMail@LMI-Capricorn

In System 102.145, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.4, microcode 770,
Boris' Working Band, on Natasha Nogoodnik:


Insert your description of the circumstances here:

        I've had this happen before.  I type  to send a message and
get this error message.  Usually aborting out and typing  again
sends the message.

>>TRAP 5952 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS))
The subscript 0 for "" was out of range in AR-1.
Backtrace from the debugger:

ZWEI::CHECK-CHAOS-MAIL-RESPONSE (P.C. = 48)

 Arg 0 (STREAM): #<CHAOS:CHARACTER-STREAM 65731543>
 Arg 1 (ERRMES): "the body of the message"
 Arg 2 (EOF-P): T
Local 0 (LINE): ""
Local 1 (.SELECTQ.ITEM.): NIL


ZWEI::CHAOS-SEND-IT-1 (P.C. = 116)

 Arg 0 (HOST): #FS::LISPM-HOST "LMI-LAMBDA-3"
 Arg 1 (RECIPIENTS): ((:NAME "wish-lispm" :HOST ** ...))
 Arg 2 (PLIST): #<DTP-LOCATIVE 4063724>
 Arg 3 (INTERVAL): #<ZWEI::ZMAIL-SENDING-INTERVAL Mail 65727604>
 Arg 4 (TEMPLATE): (:DATE :FROM :SENDER :REPLY-TO ...)
Local 0: #<CHAOS:CHARACTER-STREAM 65731543>
Local 1 (.FILE-ABORTED-FLAG.): :ABORT
Local 2 (STREAM): #<CHAOS:CHARACTER-STREAM 65731543>
Local 3: NIL
Local 4 (RCPT): "wish-lispm@cap"


ZWEI::CHAOS-SEND-IT (P.C. = 81)

 Arg 0 (PLIST): #<DTP-LOCATIVE 4063724>
 Arg 1 (INTERVAL): #<ZWEI::ZMAIL-SENDING-INTERVAL Mail 65727604>
 Arg 2 (TEMPLATE): (:DATE :FROM :SENDER :REPLY-TO ...)
Local 0 (HOSTS): (#FS::LISPM-HOST "LMI-LAMBDA-3" #FS::UNIX-HOST "LMI-CAPRICORN" #FS::UNIX-HOST "LMI-NOSFERATU" #FS::LISPM-HOST "LMI-LAMBDA-3" ...)
Local 1 (UP): NIL
Local 2 (RECIPIENTS): ((:NAME "wish-lispm" :HOST ** ...))
Local 3 (HOST): #FS::LISPM-HOST "LMI-LAMBDA-3"
Local 4: (#FS::UNIX-HOST "LMI-CAPRICORN" #FS::UNIX-HOST "LMI-NOSFERATU" #FS::LISPM-HOST "LMI-LAMBDA-3" #FS::VMS-HOST "LMI-VAX")
Local 5 (BLIND-RECIPIENTS): NIL
Local 6 (BLIND-TEMPLATE): NIL


ZWEI::SEND-IT (P.C. = 329)

 Arg 0 (PLIST): #<DTP-LOCATIVE 4063724>
Local 0 (*QUOTE-HOSTS-FOR-XMAILR*): NIL
Local 1 (FCC): NIL
Local 2 (FTO): NIL
Local 3 (BFCC): NIL
Local 4 (FCC-PATHNAMES): NIL
Local 5 (FTO-PATHNAMES): NIL
Local 6 (BFCC-PATHNAMES): NIL
Local 7 (FCC-NAMES): NIL
Local 8 (FTO-NAMES): NIL
Local 9 (BFCC-NAMES): NIL
Local 10: (ZWEI::MAIL-ERROR SI::CONDITION-CASE-THROW ZWEI::G0233)
Local 11 (ELT): NIL
Local 12 (PATHNAME): NIL
Local 13 (MSG): NIL
Local 14 (BUFFER): NIL
Local 15: ((ZWEI::MAIL-ERROR SI::CONDITION-CASE-THROW ZWEI::G0233) (ZWEI::UNKNOWN-SPECIAL-COMMAND ZWEI::ZMAIL-COMMAND-LOOP-UNKNOWN-SPECIAL-COMMAND))
Local 16 (ERROR): NIL
Local 17 (.SELECTQ.ITEM.): NIL


ZWEI::COM-SEND-MESSAGE (P.C. = 41)

Local 0 (LIST): (:SENDER (**) :FROM (**) ...)
Local 1 (PLIST): #<DTP-LOCATIVE 4063724>


Remainder of stack:

ZWEI::COM-MAIL-END (P.C. = 23)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) 0) (P.C. = 58)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:INTERNAL (:METHOD ZWEI::ZMAIL-WINDOW :COMBINED :EDIT) 0) (P.C. = 39)
(:METHOD ZWEI::ZMAIL-WINDOW :AROUND :EDIT) (P.C. = 61)
...
(:SELECT-METHOD ZWEI::ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZWEI::ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI::ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI::ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Sunday, 28 July 1985, 13:33-EDT
From: SAM@LMI-MCGUFFEY
To: BUG-LISPM@LMI-Capricorn

In System 102.157, Local-File 56.11, FILE-Server 13.2,
Unix-Interface 5.6, MagTape 40.22, ZMail 57.10, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental TCP-Kernel 29.0, Experimental TCP-User 57.0,
Experimental TCP-Server 33.0, Experimental LMI Laser Printer II 11.1,
Experimental Object Lisp 4.0, Experimental vista 2.0, microcode 772,
Education 2x2+, on McGuffey:


Insert your description of the circumstances here:

The first time IN-PACKAGE is invoked to create a new package, it fails upon trying to
return it, because either the final PKG-GOTO should have the argument NAME , or the
final clause of the COND should setq PKG .

(DEFUN IN-PACKAGE (NAME &REST OPTIONS &KEY USE NICKNAMES)
  (DECLARE (ARGLIST NAME &KEY NICKNAMES (USE '("GLOBAL")) (SIZE #O200) SHADOW EXPORT))
  (LET ((PKG (FIND-PACKAGE NAME)))
    (COND ((AND PKG OPTIONS)
           (PROGN (USE-PACKAGE USE PKG)
                  (PKG-ADD-NICKNAMES PKG NICKNAMES)))
          ;; IF NO OPTIONS ARE SUPPLIED, AND THE PACKAGE ALREADY EXISTS, JUST DO A PKG-GOTO
          (PKG)
          (T (APPLY #'MAKE-PACKAGE NAME OPTIONS)))
    (PKG-GOTO PKG)))


>>ERROR: No package named "NIL"
Backtrace from the debugger:

PKG-FIND-PACKAGE (P.C. = 98)

 Arg 0 (THING): NIL
   --Defaulted args:--
 Arg 1 (CREATE-P): NIL
 Arg 2 (USE-LOCAL-NAMES-PACKAGE): NIL
Local 0: NIL
Local 1 (NEW-NAME): NIL
Local 2 (STRING1): NIL


PKG-GOTO (P.C. = 38)

 Arg 0 (PKG): NIL
   --Defaulted args:--
 Arg 1 (GLOBALLYP): NIL
Local 0 (PK): NIL


IN-PACKAGE (P.C. = 58)

 Arg 0 (NAME): GEORGE
 Rest arg (OPTIONS): NIL
Local 1 (USE): NIL
Local 2 (NICKNAMES): NIL
Local 3 (PKG): NIL


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (IN-PACKAGE (QUOTE GEORGE))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER IN-PACKAGE 2672033>
Local 6 (ARG-DESC): 1048641
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (IN-PACKAGE (QUOTE GEORGE))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Remainder of stack:

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
BREAK (P.C. = 437)
ZWEI::COM-BREAK (P.C. = 36)
ZWEI::COMMAND-EXECUTE (P.C. = 88)
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Friday, 26 July 1985, 15:58-EDT
From: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.157, Local-File 56.11, FILE-Server 13.2,
Unix-Interface 5.4, MagTape 40.22, ZMail 57.10, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc landscape patched, on Customer Service 2:


Insert your description of the circumstances here:

trying to compile the function sg from the following
results in the following backtrace

the function works evaluated however
;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(defmacro-displace df ($fname $params &body $body)
  `(defun ,$fname ,(append (list '&quote '&rest) $params) . ,$body))



(defmacro de ($name $body &aux
              ($type (first $body))
              ($fcn-args (second $body))
              ($defn (rest2 $body)))
  (cond ((null $fcn-args) nil)
        ((listp $fcn-args)
         (setq $defn (append (list (cons 'declare (list (cons 'special $fcn-args))))    ;
                             $defn)))
        ((atom $fcn-args)
         (setq $defn (append (list (list 'declare (list  'special $fcn-args)))
                             $defn)))
        (t nil))
  (cond ((eq $type 'lambda)
         (cond ((or (listp $fcn-args)
                   (listp $fcn-args))
                `(defun ,$name ,$fcn-args ,@$defn))
               (t (print "illegal DE type in glondon file ..."))))
        ((eq $type 'nlambda)
         (cond ((listp $fcn-args)
                `(df ,$name
                     ,`($_args_$ &AUX
                                 ,@(global:do (($n (length $fcn-args) (sub1 $n))
                                               ($aux-lst nil))
                                              ((zerop $n)
                                               (reverse $aux-lst))
                                     (setq $aux-lst
                                           (cons (list (nth $fcn-args $n)
                                                       `(nth $_args_$ ,$n))
                                                 $aux-lst))))
                     ,@$defn))
               (t `(df ,$name ,(list $fcn-args) ,@$defn))))))

(de sg
    (nlambda body
             (prog ()
                   (print "hello")
                   (prog ()
                         #'(lambda (x)
                             (print x))))))


>>ERROR: Missing breakoff-function, position: NIL
Backtrace from the debugger:

COMPILER::BARF (P.C. = 66)

 Arg 0 (EXP): NIL
 Arg 1 (REASON): "Missing breakoff-function, position"
 Arg 2 (SEVERITY): 0


COMPILER::QLP2-Q (P.C. = 243)

 Arg 0 (WD): (COMPILER::BREAKOFFS (NIL))
Local 0: NIL
Local 1 (CONST-ELT): NIL
Local 2 (OFFSET): NIL


COMPILER::QLAP-PASS2 (P.C. = 31)

 Arg 0 (PNTR): ((COMPILER::BREAKOFFS **) (COMPILER::DEBUG-INFO ** ** ** ...) COMPILER::PROGSA (CALL COMPILER::D-IGNORE **) ...)
Local 0 (P): NIL


COMPILER::QLAPP (P.C. = 256)

 Arg 0 (FCTN): ((COMPILER::MFEF SG T ** ...) (COMPILER::QTAG COMPILER::S-V-BASE) (COMPILER::S-V-BLOCK) (COMPILER::QTAG COMPILER::DESC-LIST-ORG) ...)
 Arg 1 (LAP-MODE): COMPILER:COMPILE-TO-CORE
Local 0 (NBR): 0
Local 1 (TEM): ((COMPILER::QUOTE-BASE COMPILER::TDEF 8) (COMPILER::PROGSA COMPILER::TDEF 22))


COMPILER::QC-TRANSLATE-FUNCTION (P.C. = 391)

 Arg 0 (FUNCTION-SPEC): SG
 Arg 1 (EXP): (NAMED-LAMBDA (SG **) (&QUOTE &REST BODY) (DECLARE **) ...)
 Arg 2 (QC-TF-PROCESSING-MODE): COMPILER:MACRO-COMPILE
 Arg 3 (QC-TF-OUTPUT-MODE): COMPILER:COMPILE-TO-CORE
 Arg 4 (NAME-FOR-FUNCTION): SG
Local 0 (NEW-OBJECT-THIS-LEVEL): T
Local 1 (VAL): NIL
Local 2 (VARIABLES-LISTS): ((**))
Local 3 (L): ((SG SG ** ** ...) (** ** ** ** ...))
Local 4 (FUNCTION-TO-DEFINE): SG
Local 5 (EXP): (NAMED-LAMBDA (SG **) (&QUOTE &REST BODY) (DECLARE **) ...)
Local 6 (NAME-FOR-FUNCTION): SG
Local 7 (NEW-OBJECT-THIS-LEVEL): NIL
Local 8: ("Give up on compiling ~S" SG)
Local 9: (ERROR ("Give up on compiling ~S" SG) T ("Give up on compiling ~S" SG) ...)
Local 10 (VL): NIL
Local 11 (V): NIL


Remainder of stack:

COMPILER:COMPILE-1 (P.C. = 67)
ZWEI::COMPILE-BUFFER-FORM (P.C. = 76)
ZWEI::COMPILE-BUFFER-FORM (P.C. = 46)
ZWEI::COMPILE-INTERVAL-PROCESS-BASIC-FORM (P.C. = 28)
COMPILER:COMPILE-DRIVER (P.C. = 588)
ZWEI::COMPILE-INTERVAL-PROCESS-FN (P.C. = 24)
COMPILER:COMPILE-STREAM (P.C. = 588)
(:INTERNAL ZWEI::COMPILE-INTERVAL ZWEI::DO-IT) (P.C. = 45)
(:INTERNAL ZWEI::COMPILE-INTERVAL COMPILER::FOO) (P.C. = 17)
ZWEI::COMPILE-INTERVAL (P.C. = 268)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, unreproducible, valid, 2,
*** EOOH ***
Date: Thursday, 25 July 1985, 17:22-EDT
From: Dave Goodine <dg@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.145, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.4, microcode 770,
Boris' Working Band, on Boris Badinoff:

If you C-M-<BREAK> inside a #'LOAD frame and C-R out of
that frame, you are allowed to type <END> at VALUE 0,
which crashes the machine.

-dg


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 23 July 1985, 16:20-EDT
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi site info,
on Lambda Five B:


Insert your description of the circumstances here:

Had recursively entered my own tv:kbd-tyi-hook function exactly 6 (I think) times.

(-mhd)

>>TRAP 9754 (TRANS-TRAP)
The instance variable TV:STREAM-SPARE-6 is unbound in #<PICON:AI-GENERAL-PANE Ai General Pane 3 1732526 exposed>.
Backtrace from the debugger:

EH:RELOCATE-LOCATIVES-TO-SPECPDL (P.C. = 30)

 Arg 0 (REGPDL): #<ART-REG-PDL-9000 12155543>
 Arg 1 (REGPDL-POINTER): 5664
 Arg 2 (OLD-SPECPDL-START): #<DTP-LOCATIVE 65307213>
 Arg 3 (NEW-SPECPDL-START): #<DTP-LOCATIVE 66035750>
 Arg 4 (SPECPDL-POINTER): 795
Local 0 (I): 417
Local 1 (INDEX): -8520306


EH:SG-MAYBE-GROW-PDLS (P.C. = 194)

 Arg 0 (SG): #<DTP-STACK-GROUP "Petro Window 1" 65307205>
 Arg 1 (MESSAGE-P): NIL
 Arg 2 (REGULAR-ROOM): 1024
 Arg 3 (SPECIAL-ROOM): 256
 Arg 4 (INHIBIT-ERROR): T
Local 0 (RPP): 5664
Local 1 (RPL): 5936
Local 2 (SPP): 795
Local 3 (SPL): 992
Local 4 (TEM): 1051
Local 5 (NEW-SIZE): 1536
Local 6 (NEW-LIMIT): 1472
Local 7 (DID-GROW): :REGULAR
Local 8 (OSP): #<DTP-LOCATIVE 65307213>


EH:SIGNAL-MICROCODE-ERROR (P.C. = 343)

 Arg 0 (SG): #<DTP-STACK-GROUP "Petro Window 1" 65307205>
 Arg 1 (ETE): (SYSTEM:PDL-OVERFLOW EH:REGULAR)
   --Defaulted args:--
 Arg 2 (IGNORE): T
Local 0 (INHIBIT-SCHEDULING-FLAG): T
Local 1 (ERROR-HANDLER-RUNNING): T
Local 2 (ERROR-HANDLER-REPRINT-ERROR): NIL
Local 3: ("Abort from where a microcode error is being signaled.")
Local 4: ((SYSTEM:ABORT ERROR) ("Abort from where a microcode error is being signaled.") T ("Abort from where a microcode error is being signaled.") ...)
Local 5 (SAVED-MICRO-PCS): (1706)
Local 6 (ERROR-OBJECT): #EH:PDL-OVERFLOW-ERROR :CONDITION-NAMES (EH:PDL-OVERFLOW-ERROR ERROR CONDITION SYSTEM:PDL-OVERFLOW) :PDL-NAME :REGULAR
Local 7 (I): 795
Local 8 (PC): 1706
Local 9: NIL
Local 10: NIL
Local 11: NIL
Local 12: NIL
Local 13: NIL
Local 14 (ERROR): NIL
Local 15 (.SELECTQ.ITEM.): NIL
Local 16 (CONDITION-RESULT): NIL


0,, doc.prob, 3, valid,
*** EOOH ***
Date: Sunday, 21 July 1985, 02:31-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Tail-recursion.
To: Mark Henry David <mhd@LMI-CAPRICORN>, BUG-LISPM@LMI-Capricorn
In-reply-to: The message of 20 Jul 1985 23:37-EDT from mhd@LMI-CAPRICORN
Message-ID: <[LMI-DJINN].7/21/85 02:31:40.khs>

    (setq tail-recursion-flag t)                        ; OK?

This doesn't work (as was documented in the 2.0 notes), and won't work
until the new function-calling mechanism is done, and perhaps not even
then.

Ken.


0,, 3, valid,
*** EOOH ***
Date: Saturday, 20 July 1985, 23:37-EDT
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi site info,
on Lambda Five A:


Insert your description of the circumstances here:
(setq tail-recursion-flag t)                    ; OK?

;;; Dummiest tail-recursive function...
(DEFUN FOO ()
  (FOO))

(compile 'foo)

(disassemble #'foo)
16 CALL0 D-RETURN FEF|6      ;#'FOO

(foo) produces :

>>TRAP 30302 (PDL-OVERFLOW REGULAR)
The regular push-down list has overflown.
Backtrace from the debugger:

FOO (P.C. = 17)



FOO (P.C. = 17)



FOO (P.C. = 17)



FOO (P.C. = 17)



FOO (P.C. = 17)



Remainder of stack:

FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
...
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
FOO (P.C. = 17)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


;; Renaud@Oz


0,, valid, 2,
*** EOOH ***
Date: Friday, 19 July 1985, 16:54-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: DESCRIBE
To: BUG-LISPM@LMI-Capricorn

In System 102.157, Local-File 56.11, FILE-Server 13.2,
Unix-Interface 5.4, MagTape 40.22, ZMail 57.10, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.5,
Experimental Spud 1.0, Experimental Object Lisp 17.0, microcode 770,
Boris' Working Band, on Natasha Nogoodnik:


Insert your description of the circumstances here:

Got this by evaluating (DESCRIBE least-positive-short-float).

>>TRAP 7558 (FLOATING-EXPONENT-UNDERFLOW SFL)
MINUS produced a result too small in magnitude to be a short float.
Backtrace from the debugger:

SI::PRINT-FLONUM (P.C. = 72)

 Arg 0 (X):
 Arg 1 (STREAM): #:*TERMINAL-IO*-SYN-STREAM
 Arg 2 (FASTP): (:STRING-OUT)
 Arg 3 (SMALL): T
   --Defaulted args:--
 Arg 4 (MAX-DIGITS): NIL
 Arg 5 (FORCE-E-FORMAT): NIL
Local 0 (EXPT): NIL
Local 1 (PLACE-MOVED): NIL


SI:PRINT-OBJECT (P.C. = 353)

 Arg 0 (EXP):
 Arg 1 (I-PRINDEPTH): 0
 Arg 2 (STREAM): #:*TERMINAL-IO*-SYN-STREAM
   --Defaulted args:--
 Arg 3 (WHICH-OPERATIONS): (:STRING-OUT)
Local 0 (NSS): NIL
Local 1 (FASTP): (:STRING-OUT)
Local 2: NIL
Local 3: NIL


PRIN1 (P.C. = 50)

 Arg 0 (OBJECT):
   --Defaulted args:--
 Arg 1 (STREAM): NIL
Local 0 (OLD): NIL


FORMAT::FORMAT-CTL-ASCII (P.C. = 82)

 Arg 0 (ARG):
 Arg 1 (PARAMS): NIL
 Arg 2 (PRIN1P): T
Local 0 (EDGE): NIL
Local 1 (PERIOD): NIL
Local 2 (MIN): NIL
Local 3 (PADCHAR): #/SPACE
Local 4 (WIDTH): NIL


FORMAT::FORMAT-CTL-SEXP (P.C. = 20)

 Arg 0 (ARG):
 Arg 1 (PARAMS): NIL


Remainder of stack:

FORMAT::FORMAT-CTL-OP (P.C. = 43)
FORMAT::FORMAT-CTL-STRING (P.C. = 94)
FORMAT (P.C. = 146)
SI::DESCRIBE-SYMBOL (P.C. = 134)
DESCRIBE (P.C. = 211)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 49)
SYSTEM:EVAL1 (P.C. = 547)
SI::TAGBODY-INTERNAL (P.C. = 78)
PROG (P.C. = 406)
...
ZWEI::PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, valid, 2, indeterminate,
*** EOOH ***
From: colpitts
Date: Friday, 19 July 1985, 11:18-EDT
To: bug-lispm



Sperry , Minnesota has 2 2x2+ they can send mail from Unix-A to Unix-B
but they cannot send mail in the reverse direction.

Mail sent from B to A with a to line of
        john@unix-a
will have the unix-a stripped off and end up going to Unix-B

The file /usr/lib/chaos/deliverm.log on Unix-A is empty while the
same file on Unix-B is not empty.

All other chaosnet transfers seem to work without any problems;
i.e. cftp works both ways with both get and send.

On both machines the value of :default-mail-mode is :chaos ;
the value of :chaos-mail-server-hosts is '("Unix-B" "Unix-A")

/etc/netmailer is in both crontabs

Any ideas as to the cause of this problem ?




0,, 3, valid, indeterminate,
*** EOOH ***
From: mly
Date: Thursday, 18 July 1985, 21:51-EDT
To: bug-lispm khs
Subject: select-match (macrocode in sys:sys2;selev) pushing extra nils

In Experimental System 104.48, Experimental MagTape 2.0,
Experimental FILE-Server 15.0, Experimental Local-File 58.0,
microcode 1268, 104.30 LD,LFS, on Oliver Twist:

(frob-keywords '(:test #'eq) '((:test eql t) (:test-not nil t) (:key identity t)))

>>TRAP 4663 (ARGTYP CONS M-S 0 RPLACD)
The first argument to RPLACD, NIL, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

FROB-KEYWORDS (P.C. = 146)

 Arg 0 (ARG): (:TEST (FUNCTION EQ))
 Arg 1 (KEY-INFO): ((:TEST EQL T) (:TEST-NOT NIL T) (:KEY IDENTITY T))
Local 0 (TEM): 2
Local 1 (KEYWORDS): (:TEST)
Local 2: ((:KEY IDENTITY T))
Local 3: ((FUNCTION EQ))
Local 4 (X): (:TEST-NOT NIL T)
Local 5: (NIL)
Local 6 (IDX): NIL
Local 7 (VALUE): NIL
Local 8: NIL
Local 9 (TEM): NIL

;;; -*- Mode:LISP; Package:USER -*-
(defun frob-keywords (arg key-info &aux tem keywords)
  (declare (values losep keywords &rest key-values))
  (cond ((atom arg))
        ((null (setq tem (list-length arg))))
        ((oddp tem))
        ((cdr (last arg)))
        (t
         (apply #'values
                nil
                (setq keywords
                      (loop for x in arg by 'cddr
                         do (list-match-p x `(quote ,x))
                         if (or (not (keywordp x))
                                (not (assq x key-info))
                                (memq x keywords))
                            do (return-from frob-keywords t)
                         else collect x))
                (loop for x in key-info
                      as idx = (find-position-in-list (car x) keywords)
                      as value = (if idx (elt arg (+ idx idx 1)) nil)
                   collect
                     (if (null (third x))
                         (if idx value (second x))
                       (select-match value
                         (`(quote ,tem) t
                          (if (eq tem (second x)) nil value))
                         (`(function ,tem) #||(not (fsymeval-in-function-environment tem))||# t
                          (if (eq tem (second x)) nil value))
                         (t
                            value))))))))

FROB-KEYWORDS:
 24 MOVE D-PDL ARG|0          ;ARG
 25 (MISC) ATOM D-PDL
 26 BR-NOT-NIL-POP 149
 27 CALL D-PDL FEF|7          ;#'LIST-LENGTH
 28 MOVE D-LAST ARG|0         ;ARG
 29 MOVEM LOCAL|0             ;TEM
 30 (MISC) NOT D-PDL
 31 BR-NOT-NIL-POP 149
 32 MOVE D-PDL LOCAL|0        ;TEM
 33 (MISC) ODDP D-PDL
 34 BR-NOT-NIL-POP 149
 35 MOVE D-PDL ARG|0          ;ARG
 36 (MISC) LAST D-PDL
 37 CDR D-PDL PDL-POP
 38 BR-NOT-NIL-POP 149
 39 CALL D-RETURN FEF|8       ;#'VALUES
 40 MOVE D-PDL 'NIL
 41 PUSH-E LOCAL|2
 42 POP LOCAL|3
 43 MOVE D-PDL ARG|0          ;ARG
 44 POP LOCAL|5
 45 BR-NIL 77
 46 CAR D-PDL LOCAL|5
 47 MOVEM LOCAL|4             ;X
 48 PUSH-CDR-IF-CAR-EQUAL  FEF|9      ;'QUOTE
 49 BR-NIL 53
 50 PUSH-CDR-STORE-CAR-IF-CONS  LOCAL|4       ;X
 51 BR-NIL 53
 52 (MISC) NOT D-IGNORE
 53 MOVE D-PDL LOCAL|4        ;X
 54 (MISC) SYMBOLP D-IGNORE
 55 BR-NIL 69
 56 MOVE D-PDL LOCAL|4        ;X
 57 (MISC) PACKAGE-CELL-LOCATION D-PDL
 58 CDR D-PDL PDL-POP
 59 EQ FEF|6                  ;PKG-KEYWORD-PACKAGE
 60 BR-NIL 69
 61 MOVE D-PDL LOCAL|4        ;X
 62 MOVE D-PDL ARG|1          ;KEY-INFO
 63 (MISC) ASSQ D-IGNORE
 64 BR-NIL 69
 65 MOVE D-PDL LOCAL|4        ;X
 66 MOVE D-PDL LOCAL|1        ;KEYWORDS
 67 (MISC) MEMQ D-IGNORE
 68 BR-NIL 70
 69 MOVE D-RETURN 'T
 70 MOVE D-PDL LOCAL|3
 71 MOVE D-PDL LOCAL|4        ;X
 72 (MISC) NCONS D-PDL
 73 MOVEM LOCAL|3
 74 (MISC) RPLACD D-IGNORE
 75 SETE-CDDR LOCAL|5
 76 BR-NOT-NIL 46
 77 MOVE D-PDL LOCAL|2
 78 MOVEM LOCAL|1             ;KEYWORDS
 79 SET-NIL LOCAL|5
 80 SET-NIL LOCAL|3
 81 PUSH-E LOCAL|3
 82 POP LOCAL|5
 83 MOVE D-PDL ARG|1          ;KEY-INFO
 84 POP LOCAL|2
 85 SET-NIL LOCAL|4           ;X
 86 SET-NIL LOCAL|6           ;IDX
 87 SET-NIL LOCAL|7           ;VALUE
 88 MOVE D-IGNORE LOCAL|2
 89 BR-NIL 147
 90 CAR D-PDL LOCAL|2
 91 POP LOCAL|4               ;X
 92 SETE-CDR LOCAL|2
 93 CAR D-PDL LOCAL|4         ;X
 94 MOVE D-PDL LOCAL|1        ;KEYWORDS
 95 (MISC) FIND-POSITION-IN-LIST D-PDL
 96 POP LOCAL|6               ;IDX
 97 BR-NIL 104
 98 MOVE D-PDL ARG|0          ;ARG
 99 MOVE D-PDL LOCAL|6        ;IDX
100 + LOCAL|6                 ;IDX
101 1+ PDL-POP
102 (MISC) ELT D-PDL
103 BR 105
104 MOVE D-PDL 'NIL
105 POP LOCAL|7               ;VALUE
106 MOVE D-PDL LOCAL|5
107 MOVE D-PDL LOCAL|4        ;X
108 (MISC) CADDR D-IGNORE
109 BR-NOT-NIL 114
110 MOVE D-IGNORE LOCAL|6     ;IDX
111 BR-NOT-NIL 142
112 CADR D-PDL LOCAL|4        ;X
113 BR 143
114 MOVE D-PDL LOCAL|7        ;VALUE
115 SET-NIL LOCAL|9           ;TEM
116 MOVEM LOCAL|8
117 PUSH-CDR-IF-CAR-EQUAL  FEF|9      ;'QUOTE
118 BR-NIL 129
119 PUSH-CDR-STORE-CAR-IF-CONS  LOCAL|9       ;TEM
120 BR-NIL 129
121 MOVE D-IGNORE PDL-POP
122 BR-NOT-NIL 129
123 MOVE D-PDL LOCAL|9        ;TEM
124 CADR D-PDL LOCAL|4        ;X
125 EQ PDL-POP
126 BR-NIL 142
127 MOVE D-PDL 'NIL
128 BR 143
129 MOVE D-PDL LOCAL|8
130 PUSH-CDR-IF-CAR-EQUAL  FEF|10     ;'FUNCTION
131 BR-NIL 142
132 PUSH-CDR-IF-CAR-EQUAL  LOCAL|9    ;TEM
133 BR-NIL 142
134 MOVE D-IGNORE PDL-POP
135 BR-NOT-NIL 142
136 MOVE D-PDL LOCAL|9        ;TEM
137 CADR D-PDL LOCAL|4        ;X
138 EQ PDL-POP
139 BR-NIL 142
140 MOVE D-PDL 'NIL
141 BR 143
142 MOVE D-PDL LOCAL|7        ;VALUE
143 (MISC) NCONS D-PDL
144 MOVEM LOCAL|5
145 (MISC) RPLACD D-IGNORE
146 BR 88
147 MOVE D-PDL LOCAL|3
148 (MISC) %SPREAD D-LAST
149 MOVE D-RETURN PDL-POP


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Thursday, 18 July 1985, 15:25-EDT
From: george@LMI-CAPRICORN
Sender: ROBDFD@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, site files 2.0,
on Customer Service 2:


Insert your description of the circumstances here:

Inside Zmacs type
        Meta-X List Fonts
mouse on Grey5x5 (the display is screwy
mouse on Big Fonts ***more*** processing is indicated
hitting the space bar gives the following


>>TRAP 25564 (TV-ERASE-OFF-SCREEN)
An attempt was made to do graphics past the end of the screen.
Backtrace from the debugger:

FED:DISPLAY-FONT (P.C. = 422)

 Arg 0 (FONT): #<FONT BIGFNT 30304342>
 Arg 1 (WINDOW): #<ZWEI:EDITOR-TYPEOUT-WINDOW Editor Typeout Window 1 1705074 exposed>
 Arg 2 (CLEAR-FIRST-P): NIL
   --Defaulted args:--
 Arg 3 (FROM-FED): NIL
Local 0 (FONT-MAP): #<ART-Q-32 31547560>
Local 1 (CURRENT-FONT): #<FONT CPTFONT 30242576>
Local 2 (NAME): FONTS:BIGFNT
Local 3 (FD): #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 200 :FD-NAME ...)
Local 4 (DF): #<FONT CPTFONT 30242576>
Local 5 (CH): 140
Local 6 (OCH): 100
Local 7 (LEN): 200
Local 8 (CH1): 100


ZWEI:DISPLAY-FONT (P.C. = 51)

 Arg 0 (FONT-SYMBOL): FONTS:BIGFNT
Local 0 (FONT): #<FONT BIGFNT 30304342>


(:SELECT-METHOD ZWEI:PROCESS-SPECIAL-COMMAND :TYPEOUT-EXECUTE) (P.C. = 31)

 Arg 0 (IGNORE): :TYPEOUT-EXECUTE
 Arg 1 (FUNCTION): ZWEI:DISPLAY-FONT
 Rest arg (ARGS): (FONTS:BIGFNT)
Local 1 (*MINI-BUFFER-DONT-RECORD*): T


(:METHOD ZWEI:WINDOW :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
  (SELF is #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1704261 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
 Rest arg (ARGS): (:TYPEOUT-EXECUTE ZWEI:DISPLAY-FONT FONTS:BIGFNT)


(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 284)
  (SELF is #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1704261 exposed>)

 Arg 0 (.OPERATION.): :EDIT
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
 Arg 2 (*COMTAB*): #<ZWEI:COMTAB ZWEI:MODE-COMTAB 33132061>
 Arg 3 (*MODE-LINE-LIST*): ("ZMACS " "(" ZWEI:*MODE-NAME-LIST* ") " ...)
 Arg 4 (TOP-LEVEL-P): T
Local 0: ("Return to top level editor command loop.")
Local 1: ((SYSTEM:ABORT ERROR) ("Return to top level editor command loop.") T ("Return to top level editor command loop.") ...)
Local 2 (CH): (:TYPEOUT-EXECUTE ZWEI:DISPLAY-FONT FONTS:BIGFNT)


Remainder of stack:

(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 15 July 1985, 15:13-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Starting ZMAIL from (zmail)
To: dg@LMI-CAPRICORN
CC: BUG-zmail@LMI-Capricorn
In-reply-to: The message of 15 Jul 1985 10:29-EDT from dg@LMI-CAPRICORN
Message-ID: <[LMI-OLIVER-TWIST].7/15/85 15:13:22.RpK>

Hmm.  I can't get this to happen, and I start up ZMail like this a lot.
When you are getting new mail while reading in a mail file (which is
what happens at start up), the mode line is supposed to show you the name of
the inbox file.  (This is somewhat gratuitous, but is informative and
harmless.)  The mode line changes back to normal as soon as the new mail
has been read in and parsed.  Hitting a key should have nothing to do
with it, unless some comment forces all messages to be parsed, in which
case there is no bug.


0,, 3, valid,
*** EOOH ***
Date: Monday, 15 July 1985, 13:23-EDT
From: robert@LMI-CAPRICORN
Sender: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc, on Customer Service 2:

The following code runs differently evaluated than compiled.
The evaluated version seems to be buggy.


;;; -*- Mode:LISP; Package:USER; Tab-Width:70; Fonts:(MEDFNT HL12I MEDFNB); Base:10 -*-



;first type
;(ask rule goal)
;then hit the help key
;then (ask '(not a list) '(my fat rat)
;then hit the help key
;the results are different depending on whether you run the code compiled or
;interpreted

(defvar rule '(a list))
(defvar goal '(my fat cat))

(defun help-fun (stream ignore1 ignore2)
  (format stream "~%The rule is ~{ ~S~}" rule))

(defun ask (rule goal)
  (fquery '( :fresh-line t :help-function help-fun

                         )
                       "Is this true: ~S?  " goal))



0,, 3, valid,
*** EOOH ***
Date: Monday, 15 July 1985, 12:14-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc landscape, on Customer Service 1:

I'm trying to meter a compiled function, by using meter:enable,
meter:test or meter:run , then meter:analyze.
If I use Meter:run, then my function only
appears if I have explicitly enabled %current-stack-group,
but not if I use (meter:enable t) which is supposed to
enable all stack groups.
Evaluating the form using meter:test works fine,
(this does a (meter:enable %current-stack-group)) .


0, answered,, 3, valid,
*** EOOH ***
Date: Monday, 15 July 1985, 11:48-EDT
From: mhd@LMI-CAPRICORN
Sender: LH@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Fifteen B:

M-x Macro Expand Expression is broken.  It will sometimes
get confused and say "Beep. The special token close was
read at top level" when there is absolutely no such problem.
For example, in the following definition marking the form
that begins with with-refreshable-pane and then doing M-x
Macro Expand Expression gets this lossage to happen. The
closely related command, M-x Macro Expand Expression All,
seems to work in this case, however.

(defun top-level-command-loop ()
  (error-restart-loop ((sys:abort error) "Restart top-level loop of PICON")
    (handle-top-level-character-for-capture
      (or (send interaction-pane ':any-tyi-no-hang)
          (with-refreshable-pane (menu-pane)
            (draw-on-pane-with-redoable-funcall
              menu-pane
              'put-up-subcommands
              '((customize-ai-base   "   Customize          ")
                (modify-parameters   "   Modify Parameters  ")
                (load-application-kb "   Load Knowledge     ")
                (save-application-kb "   Save Knowledge     ")
                (run-ai-base         "   Run                ")
                (retrieve-rules      "   Retrieve Rules     ")
                (add-icon            "   Add Icon           "))
              menu-pane)
            (loop as character = (send interaction-pane ':any-tyi)
                  when (interactive-command-character-p character)
                  return character
                  do (handle-top-level-character-for-capture character)))))))


-mhd


0,, 3, valid,
*** EOOH ***
Date: Monday, 15 July 1985, 10:29-EDT
From: Dave Goodine <dg@LMI-CAPRICORN>
Subject:
To: BUG-zmail@LMI-Capricorn

In zmail in System 102.145, Local-File 56.9, FILE-Server 13.2,
Unix-Interface 5.4, MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.4, microcode 770,
Boris' Working Band, on Boris Badinoff:

I've just notced a new problem with ZMAIL.  If I do (ZMAIL) from
lisp, which automatically loads my mail files, I get left in the
buffer CAP:/usr/spool/mail/dg until I type a character (like ^L)
which then causes the right buffer (/lmi/dg/mbox) to be shown.


-dg


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 12 July 1985, 15:59-EDT
From: Dave Goodine <dg@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.145, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.4, microcode 770,
Boris' Working Band, on Boris Badinoff:

Calling a compiled function with a (LEXPR-FUNCALL NIL ...) in it crashes
the machine.  For example:


(defun crash-me ()
        (lexpr-funcall nil 'foo '(bar)))

which produces the following compiled code

 18 CALL D-RETURN 'NIL
 19 MOVE D-PDL FEF|6          ;'FOO
 20 MOVE D-LAST FEF|7         ;'BAR



0,, 3, valid,
*** EOOH ***
Date: Thursday, 11 July 1985, 22:46-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: :draw-filled-in-circle
To: BUG-LISPM@LMI-Capricorn

In System 102.156, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental Object Lisp 4.0, Experimental iris 2.0,
Experimental LMI Laser Printer II 11.0, microcode 770,
on The Importance of Being Earnest:

:draw-filled-in-circle leaves a couple of lines blank
when the radius argument is 4 (and not in any other
cases, as far as I can tell).


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 11 July 1985, 12:47-EDT
From: rdm@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.148, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
Experimental Object Lisp 4.0, Experimental iris 2.0, microcode 770,
on Lambda Fourteen:

%multibus-write-16 fails to pop one of its arguments before
exiting.  Watch the pdl pointer increase when it has no right to
when you run

(defun foo ()
  (dotimes (i 300.)
    (format t "~&~d" (sys:sg-regular-pdl-pointer sys:%current-stack-group))
    (tyi)
    (%multibus-write-16 0 0)))

See the bug in the code:


xmultibus-write-16 (misc-inst-entry %multibus-write-16)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop) ; word to write
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
        ((md) a-map-scratch-block)
        ((m-a) a-sdu-quad-slot)         ;give L1 map time to settle..
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((m-i) dpb m-a (byte-field 8 14.) a-zero)
        ((m-i) dpb c-pdl-buffer-pointer (byte-field 2 22.) a-i)  ;get bit 1 of adr into
                                                                 ;map.phys.1
        ((m-i) dpb m-minus-one (byte-field 1 22.) a-i) ;turn on map.phys.0
        ((l2-map-physical-page) ldb c-pdl-buffer-pointer (byte-field 13. 10.) a-i)
                        ;page number bits to page number section of map.
        ((m-a) ldb (byte-field 1 1) c-pdl-buffer-pointer) ; get bit 1 of adr
        ((m-a) dpb m-a (byte-field 1 4) a-zero) ; multiply by 16
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        ((md) dpb (byte-field 16. 0) m-t a-zero)
        ((vma-start-write) ldb (byte-field 8 2) c-pdl-buffer-pointer a-map-scratch-block)
        (illop-if-page-fault)
        (popj)          ;no popj after next, since return to main loop could start mem cycle

Other multibus functions might also have this problem.

Respectfully submitted
        rdm, naha


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 11 July 1985, 12:29-EDT
From: george@LMI-CAPRICORN
Subject: compiler:fasd-file-symbols-properties of large lists of large arrays
To: BUG-LISPM@LMI-CAPRICORN

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, site files 2.0,
on Customer Service 2:


Evaluating the following code does not result in preserving the value of
b and a. According to Graphael the problem is

'the function "fasd-op-storein-symbol-value" uses an array called
"fasd table" whose index is coded on 16 bits. so when we reach an index over
16 bits everything gets mixed up.'

;;; -*- Mode:LISP; Package:USER; Base:10 -*-




(setq a (make-array 70000))
(setf (aref a 69000) 69000)
(length (setq b (make-list 70000)))
(setf (nth 69000 b) a)
(aref (nth 69000 b) 69000)

(compiler:fasd-file-symbols-properties "robert:george;graphael#1" '(b a) nil t nil nil)
(setq a 2 b 3)
(load "robert:george;graphael#1")
(aref (nth 69000 b) 69000)



0,, valid, 2, indeterminate,
*** EOOH ***
Date: Thursday, 11 July 1985, 12:18-EDT
From: dg@cap
Sender: george@LMI-LAMBDA-CS1
To: BUG-LISPM@LMI-CAPRICORN

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, site files 2.0,
on Customer Service 2:

----------------------------------------------------------------------
Compiling the following code crashes the machine.
Evaluating is ok, though.

Crash info follows.

(defvar UpdateStatus nil)


(defun f () (add-imparityname))

(defun add-imparityname ()
  (PROG (ABORTDATA)
        (LET ((Updates NIL) (Requirements NIL))
          (SETQ ABORTDATA
                (MULTIPLE-VALUE-LIST
                  (CATCH  'OuterAtomic
                    (LET-IF (NULL UpdateStatus)
                            ((UpdateStatus 'ATOMIC))
                      (PROG
                        (ABORTDAT)
                        (LET-IF
                          (NULL UpdateStatus)
                          ((Updates NIL) (Requirements NIL))
                          (SETQ
                            ABORTDAT
                            (MULTIPLE-VALUE-LIST
                              (CATCH
                                (COND (UpdateStatus 'InnerAtomic)
                                      (T 'OuterAtomic))
                                (LET ((UpdateStatus 'ATOMIC))
                                  NIL))))
                          (RETURN (progn
                                    (print "going to return values in")
                                    (print abortdat)
                                    (VALUES-LIST ABORTDAT)))
                          ))))))
;; *** it gets this far
; *** but not theis far
          (print "end of ++'s")
          ))
  )


; compiled code:

 36 MOVE D-PDL FEF|7          ;'76
 37 (MISC) %CATCH-OPEN-MV-LIST D-IGNORE
 38 MOVE D-PDL FEF|8          ;'OUTERATOMIC
 39 (MISC) SPECIAL-PDL-INDEX D-PDL
 40 MOVE D-IGNORE FEF|6       ;UPDATESTATUS
 41 BR-NOT-NIL 46
 42 MOVE D-PDL FEF|9          ;'UPDATESTATUS
 43 (MISC) %EXTERNAL-VALUE-CELL D-PDL
 44 MOVE D-PDL FEF|10         ;'ATOMIC
 45 (MISC) %BIND D-IGNORE
 46 (MISC) SPECIAL-PDL-INDEX D-PDL
 47 MOVE D-IGNORE FEF|6       ;UPDATESTATUS
 48 BR-NOT-NIL 55
 49 PUSH-E LOCAL|1            ;UPDATES
 50 MOVE D-PDL 'NIL
 51 PUSH-E LOCAL|2            ;REQUIREMENTS
 52 MOVE D-PDL 'NIL
 53 (MISC) %BIND D-IGNORE
 54 (MISC) %BIND D-IGNORE
 55 MOVE D-PDL FEF|11         ;'66
 56 (MISC) %CATCH-OPEN-MV-LIST D-IGNORE
 57 MOVE D-IGNORE FEF|6       ;UPDATESTATUS
 58 BR-NIL 61
 59 MOVE D-PDL FEF|12         ;'INNERATOMIC
 60 BR 62
 61 MOVE D-PDL FEF|8          ;'OUTERATOMIC
 62 MOVE D-PDL FEF|10         ;'ATOMIC
 63 BIND-POP FEF|6            ;UPDATESTATUS
 64 MOVE D-PDL 'NIL
 65 (MISC) UNBIND 1 binding
 66 POP LOCAL|3               ;ABORTDAT
 67 CALL D-IGNORE FEF|13      ;#'PRINT
 68 MOVE D-LAST FEF|14        ;'"going to return values in"
 69 CALL D-IGNORE FEF|13      ;#'PRINT
 70 MOVE D-LAST LOCAL|3       ;ABORTDAT
 71 MOVE D-PDL LOCAL|3        ;ABORTDAT
 72 (MISC) UNBIND-TO-INDEX-MOVE D-PDL
 73 (MISC) UNBIND-TO-INDEX-MOVE D-PDL
 74 MOVE D-PDL FEF|15         ;'12
 75 (MISC) SHRINK-PDL-SAVE-TOP D-PDL
 76 POP LOCAL|0               ;ABORTDATA
 77 CALL D-IGNORE FEF|13      ;#'PRINT
 78 MOVE D-LAST FEF|16        ;'"end of ++'s"
 79 MOVE D-RETURN 'NIL

;;; PC history (WHY-PC output) (elipses mean contiguous in pc history)

Running Microcode 768

At PC=#o000005 machine is at (ILLOP + 1)
At PC=#o000004 machine is at ILLOP

At PC=#o006477 machine is at (XUNBIND-TO-INDEX-0 + 2)
At PC=#o006476 machine is at (XUNBIND-TO-INDEX-0 + 1)
At PC=#o006475 machine is at XUNBIND-TO-INDEX-0

At PC=#o006434 machine is at (BBLKP2 + 2)
...
At PC=#o006432 machine is at BBLKP2

At PC=#o006426 machine is at (BBLKP3 + 1)
...
At PC=#o006424 machine is at (BBLKP1 + 17)
...
At PC=#o006407 machine is at (BBLKP1 + 2)
...
At PC=#o006404 machine is at QUNBND
At PC=#o006403 machine is at (BBLKP + 1)
At PC=#o006402 machine is at BBLKP



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Wednesday, 10 July 1985, 20:29-EDT
From: robert@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Ten:


Insert your description of the circumstances here:

(sxhash '(a b . c))    => a reasonable value
(sxhash '(a b c d .e)) => ditto

But,

(sxhash '(a b c . d))  =>
>>TRAP 5180 (ARGTYP FIXNUM PP 1 XDPB0)
The second argument to DPB, 536870747, was of the wrong type.
The function expected a fixnum.
Backtrace from the debugger:

SXHASH (P.C. = 157)

 Arg 0 (X): (A B C . D)
   --Defaulted args:--
 Arg 1 (RANDOM-OBJECT-ACTION): NIL
Local 0 (ROT): 1
Local 1 (HASH): 279429
Local 2 (Y): C
Local 3 (X): D
Local 4: -3
Local 5: 8704


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (SXHASH (QUOTE **))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER SXHASH 2541045>
Local 6 (ARG-DESC): 66
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (SXHASH (QUOTE **))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (SXHASH (QUOTE **))
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 272)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 3200000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
Local 0 (OLD-PACKAGE): #<Package USER 10054742>
Local 1 (W-PKG): #<Package USER 10054742>
Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 30265237>
Local 3 (THROW-FLAG): T
Local 4: ("Return to top level in ~A." "Lisp Listener 1")
Local 5: ((SYSTEM:ABORT ERROR) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
Local 6 (VALUES): NIL
Local 7: NIL
Local 8 (VALUE): T


Remainder of stack:

SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 9 July 1985, 17:28-EDT
From: Ken Sinclair <mly@LMI-CAPRICORN>
Sender: khs@LMI-CAPRICORN
Subject: QFASL.
To: colpitts@LMI-CAPRICORN, bug-lispm@LMI-CAPRICORN
In-reply-to: The message of 9 Jul 1985 12:34-EDT from colpitts
Message-ID: <[LMI-DJINN].7/09/85 17:28:10.khs>

    From: colpitts
    Date: Tuesday, 9 July 1985, 12:34-EDT

    graphael has huge lists of huge art-q arrays which they store to
    a file with
    compiler:fasd-file-symbols-properties when they load the file in
    they say the data is incoherent and that the problem is caused by
    the function fasd-op-storein-symbol-value uses an array called
    fasd table whose index is coded on 16 bits so if they exceed 16
    bits everything is screwed up.

    Can we fix this in the next release ?
    (sorry about the vagueness of the description but its what they
    gave us)

I think so.  I'll look at it.

Ken.


0,, valid, 2, indeterminate,
*** EOOH ***
Message-ID: <8507092035.AA15480@isi-vaxa.ARPA>
Date: Tuesday, 9 July 1985, 16:34-EDT
To: LMI-CAPRICORN!BUG-LISPM%MITCCC@MIT-MC
From: Donc@ISI-VAXA
Subject: compiled code causing crash

By the way, please let me know when you recieve this (so I'll know
the msg arrived).

;;; -*- Mode:LISP; Package:user; Base:10 -*-

;; Instructions:
; compile and load this file and then do something like
; (add-imparityname t t t t)
; You'll see several tracing messages printed, ending with the one
; flagged below (***), after which the machine crashes.
; If you load the source and do the same thing, there's no crash.

#m ; this is the original function
(defun add-imparityname (rel imp arity name)
  (atomic (++ relationarity rel arity)
          (++ relationname rel name)
          (++ relationimplementation rel imp)))
#m
(macroexpand
 '(atomic (++ relationarity rel arity)
          (++ relationname rel name)
          (++ relationimplementation rel imp)))
#m ;=>
(PROG (ABORTDATA ABORTDATA2 ACTIVATIONS)
      (LET-IF (NULL |UpdateStatus |)
              ((|Updates | NIL) (|Requirements | NIL))
              (SETQ ABORTDATA
                    (MULTIPLE-VALUE-LIST
                       (CATCH (COND (|UpdateStatus | '|InnerAtomic |)
                                    (T '|OuterAtomic |))
                              (LET-IF (NULL |UpdateStatus |)
                                      ((|UpdateStatus | 'ATOMIC))
                                      ;(++ RELATIONARITY REL ARITY)
                                      ;(++ RELATIONNAME REL NAME)
                                      ;(++ RELATIONIMPLEMENTATION REL IMP)
                                ))))
              (COND ((AND (NULL |UpdateStatus |)
                          (NEQ (CAR ABORTDATA) '|Abort |)
                          (OR |Updates | |Requirements |))
                       (SETQ ABORTDATA2
                             (MULTIPLE-VALUE-LIST
                                (CATCH '|OuterAtomic |
                                       (SETQ ACTIVATIONS
                                             (ATOMICUPDATE
                                                |Updates | |Requirements |)))))))
              (COND ((EQ (CAR ABORTDATA2) '|Abort |)
                       (SETQ ABORTDATA ABORTDATA2)))
              (DOAUTOMATIONDEMONS ACTIVATIONS)
              (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                               (SETQ ABORTDATA (CDR ABORTDATA))
                               (LEXPR-FUNCALL 'FERROR ABORTDATA))
                            (T (VALUES-LIST ABORTDATA))))))
; by the way, this expansion, when compiled, does not crash the machine

; unfortunately, that's not all the macroexpansion to be done ...

;(macroexpand '(++ RELATIONARITY REL ARITY)) =>
#m
(PROG
   (ABORTDATA ABORTDATA2 ACTIVATIONS)
   (LET-IF
      (NULL |UpdateStatus |)
      ((|Updates | NIL) (|Requirements | NIL))
      (SETQ
         ABORTDATA
         (MULTIPLE-VALUE-LIST
            (CATCH
               (COND (|UpdateStatus | '|InnerAtomic |)
                     (T '|OuterAtomic |))
               (LET-IF
                  (NULL |UpdateStatus |)
                  ((|UpdateStatus | 'ATOMIC))
                  (CHECKUPDATEALLOWED
                     '(RELATIONARITY REL ARITY)
                     'T)
                  (PUSH
                     (MAKE-UPDATEREC
                        :UPDTUPLE
                        (MAKE-TUPLE
                           TUPREL
                           (MEMO (RELATIONP 'RELATIONARITY))
                           TUPARGS
                           (LIST REL ARITY)
                           TUPTV
                           'T)
                        :UPDREASON
                        '(RELATIONARITY REL ARITY)
                        :UPDCX CURRENTCX :UPDCODE
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             #'(LAMBDA
                                  (RELARITY REL ARITY &AUX TUPLE)
                                  (SETQ TUPLE (LIST REL ARITY))
                                  (PUTBASEDATA
                                     RELARITY
                                     (CONS
                                        TUPLE
                                        (GETBASEDATA RELARITY)))
                                  (ADD-STRUCTURE-PROPERTY
                                     REL ARITY
                                     'RELATIONARITY)))
                        :UPDCXEVAL
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             'TESTREL))
                     |Updates |)
                  NIL))))
      (COND ((AND (NULL |UpdateStatus |)
                  (NEQ (CAR ABORTDATA) '|Abort |)
                  (OR |Updates | |Requirements |))
               (SETQ ABORTDATA2
                     (MULTIPLE-VALUE-LIST
                        (CATCH '|OuterAtomic |
                               (SETQ ACTIVATIONS
                                     (ATOMICUPDATE |Updates | |Requirements |)))))))
      (COND ((EQ (CAR ABORTDATA2) '|Abort |) (SETQ ABORTDATA ABORTDATA2)))
      (DOAUTOMATIONDEMONS ACTIVATIONS)
      (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                       (SETQ ABORTDATA (CDR ABORTDATA))
                       (LEXPR-FUNCALL 'FERROR ABORTDATA))
                    (T (VALUES-LIST ABORTDATA))))))

; and similarly for the others
;
; The net result is:

(defun add-imparityname (rel imp arity name)

 (PROG (ABORTDATA ABORTDATA2 ACTIVATIONS)
      (LET-IF (NULL |UpdateStatus |)
              ((|Updates | NIL) (|Requirements | NIL))
              (SETQ ABORTDATA
                    (MULTIPLE-VALUE-LIST
                       (CATCH (COND (|UpdateStatus | '|InnerAtomic |)
                                    (T '|OuterAtomic |))
                              (LET-IF (NULL |UpdateStatus |)
                                      ((|UpdateStatus | 'ATOMIC))
; (macroexpand '(++ RELATIONARITY REL ARITY))
 (PROG
   (ABORTDATA ABORTDATA2 ACTIVATIONS)
   (LET-IF
      (NULL |UpdateStatus |)
      ((|Updates | NIL) (|Requirements | NIL))
      (SETQ
         ABORTDATA
         (MULTIPLE-VALUE-LIST
            (CATCH
               (COND (|UpdateStatus | '|InnerAtomic |)
                     (T '|OuterAtomic |))
               (LET-IF
                  (NULL |UpdateStatus |)
                  ((|UpdateStatus | 'ATOMIC))
                  (CHECKUPDATEALLOWED
                     '(RELATIONARITY REL ARITY)
                     'T)
                  (PUSH
                     (MAKE-UPDATEREC
                        :UPDTUPLE
                        (MAKE-TUPLE
                           :TUPREL
                           (MEMO (RELATIONP 'RELATIONARITY))
                           :TUPARGS
                           (LIST REL ARITY)
                           :TUPTV
                           'T)
                        :UPDREASON
                        '(RELATIONARITY REL ARITY)
                        :UPDCX CURRENTCX :UPDCODE
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             #'(LAMBDA
                                  (RELARITY REL ARITY &AUX TUPLE)
                                  (SETQ TUPLE (LIST REL ARITY))
                                  (PUTBASEDATA
                                     RELARITY
                                     (CONS
                                        TUPLE
                                        (GETBASEDATA RELARITY)))
                                  (ADD-STRUCTURE-PROPERTY
                                     REL ARITY
                                     'RELATIONARITY)))
                        :UPDCXEVAL
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             'TESTREL))
                     |Updates |)
                  NIL))))
      (COND ((AND (NULL |UpdateStatus |)
                  (NEQ (CAR ABORTDATA) '|Abort |)
                  (OR |Updates | |Requirements |))
               (SETQ ABORTDATA2
                     (MULTIPLE-VALUE-LIST
                        (CATCH '|OuterAtomic |
                               (SETQ ACTIVATIONS
                                     (ATOMICUPDATE |Updates | |Requirements |)))))))
      (COND ((EQ (CAR ABORTDATA2) '|Abort |) (SETQ ABORTDATA ABORTDATA2)))
      (DOAUTOMATIONDEMONS ACTIVATIONS)
      (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                       (SETQ ABORTDATA (CDR ABORTDATA))
                       (LEXPR-FUNCALL 'FERROR ABORTDATA))
                    (T (VALUES-LIST ABORTDATA))))))
; (macroexpand '(++ RELATIONNAME REL NAME))
 (PROG
   (ABORTDATA ABORTDATA2 ACTIVATIONS)
   (LET-IF
      (NULL |UpdateStatus |)
      ((|Updates | NIL) (|Requirements | NIL))
      (SETQ
         ABORTDATA
         (MULTIPLE-VALUE-LIST
            (CATCH
               (COND (|UpdateStatus | '|InnerAtomic |)
                     (T '|OuterAtomic |))
               (LET-IF
                  (NULL |UpdateStatus |)
                  ((|UpdateStatus | 'ATOMIC))
                  (CHECKUPDATEALLOWED
                     '(RELATIONNAME REL NAME)
                     'T)
                  (PUSH
                     (MAKE-UPDATEREC
                        :UPDTUPLE
                        (MAKE-TUPLE
                           :TUPREL
                           (MEMO (RELATIONP 'RELATIONNAME))
                           :TUPARGS
                           (LIST REL NAME)
                           :TUPTV
                           'T)
                        :UPDREASON
                        '(RELATIONNAME REL NAME)
                        :UPDCX CURRENTCX :UPDCODE
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             #'(LAMBDA
                                  (RELNAME REL NAME &AUX TUPLE)
                                  (SETQ TUPLE (LIST REL NAME))
                                  (PUTBASEDATA
                                     RELNAME
                                     (CONS
                                        TUPLE
                                        (GETBASEDATA RELNAME)))
                                  (PUTRELATIONOFNAME NAME REL)))
                        :UPDCXEVAL
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             'TESTREL))
                     |Updates |)
                  NIL))))
      (COND ((AND (NULL |UpdateStatus |)
                  (NEQ (CAR ABORTDATA) '|Abort |)
                  (OR |Updates | |Requirements |))
               (SETQ ABORTDATA2
                     (MULTIPLE-VALUE-LIST
                        (CATCH '|OuterAtomic |
                               (SETQ ACTIVATIONS
                                     (ATOMICUPDATE |Updates | |Requirements |)))))))
      (COND ((EQ (CAR ABORTDATA2) '|Abort |) (SETQ ABORTDATA ABORTDATA2)))
      (DOAUTOMATIONDEMONS ACTIVATIONS)
      (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                       (SETQ ABORTDATA (CDR ABORTDATA))
                       (LEXPR-FUNCALL 'FERROR ABORTDATA))
                    (T (VALUES-LIST ABORTDATA))))))

; (macroexpand '(++ RELATIONIMPLEMENTATION REL IMP))
 (PROG
   (ABORTDATA ABORTDATA2 ACTIVATIONS)
   (LET-IF
      (NULL |UpdateStatus |)
      ((|Updates | NIL) (|Requirements | NIL))
      (SETQ
         ABORTDATA
         (MULTIPLE-VALUE-LIST
            (CATCH
               (COND (|UpdateStatus | '|InnerAtomic |)
                     (T '|OuterAtomic |))
               (LET-IF
                  (NULL |UpdateStatus |)
                  ((|UpdateStatus | 'ATOMIC))
                  (CHECKUPDATEALLOWED
                     '(RELATIONIMPLEMENTATION REL IMP)
                     'T)
                  (PUSH
                     (MAKE-UPDATEREC
                        :UPDTUPLE
                        (MAKE-TUPLE
                           :TUPREL
                           (MEMO (RELATIONP 'RELATIONIMPLEMENTATION))
                           :TUPARGS
                           (LIST REL IMP)
                           :TUPTV
                           'T)
                        :UPDREASON
                        '(RELATIONIMPLEMENTATION REL IMP)
                        :UPDCX CURRENTCX :UPDCODE
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             #'(LAMBDA
                                  (RELIMP REL IMP &AUX TUPLE)
                                  (SETQ TUPLE (LIST REL IMP))
                                  (PUTBASEDATA
                                     RELIMP
                                     (CONS
                                        TUPLE
                                        (GETBASEDATA RELIMP)))
                                  (ADD-STRUCTURE-PROPERTY
                                     REL IMP
                                     'RELATIONIMPLEMENTATION)))
                        :UPDCXEVAL
                        (AND (NEQ |UpdateStatus | 'TRIGGER)
                             'TESTREL))
                     |Updates |)
                  NIL))))
      (COND ((AND (NULL |UpdateStatus |)
                  (NEQ (CAR ABORTDATA) '|Abort |)
                  (OR |Updates | |Requirements |))
               (SETQ ABORTDATA2
                     (MULTIPLE-VALUE-LIST
                        (CATCH '|OuterAtomic |
                               (SETQ ACTIVATIONS
                                     (ATOMICUPDATE |Updates | |Requirements |)))))))
      (COND ((EQ (CAR ABORTDATA2) '|Abort |) (SETQ ABORTDATA ABORTDATA2)))
      (DOAUTOMATIONDEMONS ACTIVATIONS)
      (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                     (print "going to abort")
                       (SETQ ABORTDATA (CDR ABORTDATA))
                       (LEXPR-FUNCALL 'FERROR ABORTDATA))
                    (T
                     (print "going to return values in")
                     (print abortdata)
; *** it gets this far
                     (VALUES-LIST ABORTDATA))))))))))
; *** but not theis far
              (print "end of ++'s")
              (COND ((AND (NULL |UpdateStatus |)
                          (NEQ (CAR ABORTDATA) '|Abort |)
                          (OR |Updates | |Requirements |))
                     (print "about to do atomicupdate")
                       (SETQ ABORTDATA2
                             (MULTIPLE-VALUE-LIST
                                (CATCH '|OuterAtomic |
                                       (SETQ ACTIVATIONS
                                             (ATOMICUPDATE
                                                |Updates | |Requirements |)))))))
              (COND ((EQ (CAR ABORTDATA2) '|Abort |)
                       (SETQ ABORTDATA ABORTDATA2)))
              (DOAUTOMATIONDEMONS ACTIVATIONS)
              (RETURN (COND ((EQ (CAR ABORTDATA) '|Abort |)
                               (SETQ ABORTDATA (CDR ABORTDATA))
                               (LEXPR-FUNCALL 'FERROR ABORTDATA))
                            (T (VALUES-LIST ABORTDATA))))))
)

; and now for a little environment to allow this all to run
;
(defvar |UpdateStatus | nil)
(defun checkupdateallowed (x y) (print (list 'checkupdateallowed x y)))
(defun MAKE-UPDATEREC (&rest ignore) (prin1 'make-updaterec) 'updaterec)
(defun MAKE-TUPLE (&rest ignore) 'tuple)
(defun memo (&rest ignore) 'relation)
(defun relationp (&rest ignore) 'relation)
(defvar currentcx 'currentcx)
(defun DOAUTOMATIONDEMONS (&rest ignore) (prin1 'doautomationdemons))



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 9 July 1985, 12:33-EDT
From: robert@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Ten:

Mitre's VAX keeps only the five most recent versions of files around.
When a file is to be saved, if five versions already exist, the oldest
version is deleted first.  If the owner of the earliest version is
different from the current user, however, the file won't be deleted, and
an appropriate error message is displayed.

Unfortunately, the release 2.0 software doesn't appear to recognize this
situation.  In spite of the VAX's having refused to save the new file,
the user is told that the file was written successfully.

robert


0,, valid, 4,
*** EOOH ***
Date: Saturday, 6 July 1985, 15:52-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Site options when using ZMail
To: colpitts@LMI-CAPRICORN
CC: bug-zmail@LMI-CAPRICORN
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: The message of 3 Jul 1985 10:34-EDT from colpitts
Message-ID: <[LMI-BORIS].7/06/85 15:52:16.RpK>

Don't you mean :LOCAL-MAIL-HOSTS, not :LOCAL-MAIL-SERVER-HOSTS ?  That
particular option is actually not absolutely neccessary; it just makes
the ZMail summary window display a little less cluttered.

The base site file ought to have :DEFAULT-MAIL-MODE be :CHAOS anyway.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Thursday, 4 July 1985, 16:08-EDT
From: James M. Turner <jmturn@LMI-CAPRICORN>
Subject: Is there a microcode hacker in the house?
To: bug-lispm@cap
In-reply-to: The message of 25 Jun 1985 10:30-EDT from george@LMI-CAPRICORN
Message-ID: <[LMI-NATASHA].7/04/85 16:08:13.JMTurn>

    Date: Tuesday, 25 June 1985, 10:30-EDT
    From: george at LMI-CAPRICORN

    In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
    MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
    MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
    Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
    DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
    si pr vi ma gc, on Customer Service 2:


    Insert your description of the circumstances here:

    the previous bug report on this was slightly inaccurate. The following
    must be compiled and setf must have two arguments the second being nil


    (defvar *array* (make-array '(3 3 2187)))

    (defun s1 (n)
      (dotimes (i n)
        (setf (aref *array*  0 0 i) nil)))

    (s1 1100)
    will work
    but
    (s1 2000)
    gets thrown into a trap

Actually, I got remarkably similar results with 0 as the argument. Also,
since SETF expands into a microcoded function, one SYSTEM:SET-AREF, one
is forced to assume that it must be something in the calling sequence
that changes. This is backed up by the fact that I tried this goodie out
in the mail breakpoint and got my stack group screwed.

By the way, not a trick to try on a machine you care about, it really
does tend to break the universe pretty badly.

                        James


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Wednesday, 3 July 1985, 14:27-EDT
From: James M. Turner <jmturn@LMI-CAPRICORN>
Subject: Grump.
To: BUG-LISPM@LMI-Capricorn

In System 102.152, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.21, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental TCP-Kernel 28.1,
Experimental TCP-User 55.6, Experimental TCP-Server 32.2,
Experimental LMI Laser Printer II 8.4, microcode 768, Laser2, on Dale:


Insert your description of the circumstances here:

I got this after telling the error handler to resume with the last argument dropped.

>>TRAP 5952 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS))
The subscript -2578953 for #<ART-REG-PDL-6656 11655010> was out of range in AR-1.
Backtrace from the debugger:

EH:SG-FRAME-VALUE-LIST (P.C. = 120)

 Arg 0 (SG): #<DTP-STACK-GROUP "Laser2 Server Process" 65427477>
 Arg 1 (FRAME): -2578950
 Arg 2 (NEW-NUMBER-OF-VALUES): 0
 Arg 3 (ORIGINAL-FRAME): 164
Local 0 (RP): #<ART-REG-PDL-6656 11655010>
Local 1 (IDX): NIL
Local 2 (TYPE): NIL
Local 3 (MORE-P): NIL
Local 4 (STORING-OPTION): NIL
Local 5 (NUM-TOTAL): NIL
Local 6 (NUM-ALREADY): NIL
Local 7 (POINTER): NIL
Local 8: NIL
Local 9 (I): NIL
Local 10 (IDX1): NIL
Local 11 (LIST-SLOT-IDX): NIL
Local 12 (EXTRA): NIL


EH:SG-FRAME-VALUE-LIST (P.C. = 272)

 Arg 0 (SG): #<DTP-STACK-GROUP "Laser2 Server Process" 65427477>
 Arg 1 (FRAME): 164
 Arg 2 (NEW-NUMBER-OF-VALUES): 0
   --Defaulted args:--
 Arg 3 (ORIGINAL-FRAME): 164
Local 0 (RP): #<ART-REG-PDL-6656 11655010>
Local 1 (IDX): 160
Local 2 (TYPE): 1
Local 3 (MORE-P): 0
Local 4 (STORING-OPTION): SYSTEM:ADI-ST-INDIRECT
Local 5 (NUM-TOTAL): NIL
Local 6 (NUM-ALREADY): NIL
Local 7 (POINTER): NIL
Local 8: NIL
Local 9 (I): NIL
Local 10 (IDX1): NIL
Local 11 (LIST-SLOT-IDX): NIL
Local 12 (EXTRA): NIL


EH::SG-UNWIND-TO-FRAME-AND-REINVOKE (P.C. = 279)

 Arg 0 (SG): #<DTP-STACK-GROUP "Laser2 Server Process" 65427477>
 Arg 1 (FRAME): 164
 Arg 2 (FORM): (#<DTP-FEF-POINTER (:METHOD LASER2::LASER2-STREAM :PRINT-STREAM) 11355355> :PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364>)
Local 0 (RP): #<ART-REG-PDL-6656 11655010>
Local 1 (PP): 163
Local 2 (LEXPR-CALL): NIL
Local 3 (OTOC): 0
Local 4 (BOT): 46
Local 5 (SP): #<ART-SPECIAL-PDL-2048 65427503>
Local 6 (SPP): 48
Local 7 (P): #<DTP-LOCATIVE 65427567>
Local 8: #<ART-REG-PDL-6656 11655010>
Local 9: 162
Local 10 (ARGS): NIL
Local 11 (COUNT): 1


(:METHOD EH::FUNCTION-ENTRY-ERROR :CASE :PROCEED-UCODE-WITH-ARGS :NEW-ARGUMENT-LIST) (P.C. = 56)
  (SELF is #EH::FUNCTION-ENTRY-ERROR :CONDITION-NAMES (EH::FUNCTION-ENTRY-ERROR ERROR CONDITION SYSTEM:TOO-MANY-ARGUMENTS) :FUNCTION #<DTP-FEF-POINTER (:METHOD LASER2::LASER2-STREAM :PRINT-STREAM) 11355355> :ARGUMENT-LIST (:PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364> NIL) :NARGS 3)

 Arg 0 (.OPERATION.): :PROCEED-UCODE-WITH-ARGS
 Arg 1 (.SUBOPERATION.): :NEW-ARGUMENT-LIST
 Arg 2 (SG): #<DTP-STACK-GROUP "Laser2 Server Process" 65427477>
 Arg 3 (ARGUMENTS): (:PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364>)
Local 0 (FORM): (#<DTP-FEF-POINTER (:METHOD LASER2::LASER2-STREAM :PRINT-STREAM) 11355355> :PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364>)
Local 1 (FRAME): 164


(:METHOD EH::FUNCTION-ENTRY-ERROR :COMBINED :PROCEED-UCODE-WITH-ARGS) (P.C. = 60)
  (SELF is #EH::FUNCTION-ENTRY-ERROR :CONDITION-NAMES (EH::FUNCTION-ENTRY-ERROR ERROR CONDITION SYSTEM:TOO-MANY-ARGUMENTS) :FUNCTION #<DTP-FEF-POINTER (:METHOD LASER2::LASER2-STREAM :PRINT-STREAM) 11355355> :ARGUMENT-LIST (:PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364> NIL) :NARGS 3)

 Rest arg (.DAEMON-CALLER-ARGS.): (:PROCEED-UCODE-WITH-ARGS :NEW-ARGUMENT-LIST #<DTP-STACK-GROUP "Laser2 Server Process" 65427477> (:PRINT-STREAM #<FS::QFILE-INPUT-CHARACTER-STREAM "SYS: LASER2; TEXT LISP >" 36113364>))
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-7 1651323>


Remainder of stack:

EH::SIGNAL-MICROCODE-ERROR (P.C. = 369)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 2 July 1985, 20:24-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: omitted keyword arg should be an error
To: BUG-LISPM@LMI-Capricorn

In System 102.154, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.21, ZMail 57.8, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LMI Laser Printer II 10.4,
microcode 768, on Mary had a little Lambda:

(defun foo (&key a b c)
  )

(foo :a)

The above causes an error (the correct behavior) when foo is interpreted, but just
leaves all three args nil when foo is compiled.


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 2 July 1985, 20:16-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: Rubout handler arglist has package problems
To: BUG-LISPM@LMI-Capricorn

In System 102.154, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.21, ZMail 57.8, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.4, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LMI Laser Printer II 10.4,
microcode 768, on Mary had a little Lambda:

At a lisp listner, type

(open :foo

then C-Shift-A, without any space after the :foo.  You'll be told "Can't find a
definition for :OPEN", indicating it is erroneously trying to intern the open in
the keyword package.  If you replace open by something that isn't a global symbol,
it will just beep instead.  It doesn't happen if you leave a space after the :foo,
and it doesn't happen with other package prefixes (ie, "(open tv:foo C-Shift-a"
will do the right thing.


0,, valid, 4, indeterminate,
*** EOOH ***
Date: Tuesday, 2 July 1985, 17:12-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
Subject: nrl -  mouse documentation line
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi, landscape,
on Customer Service 1:


Certainly after cold boot and, it seems, other times as well the mouse
documentation line is missing.  No real big deal since it is properly
displayed after a mouse click.

In PEEK MODE with "window hierarchy" selected (at other times as well)
and mouse not on text, mouse documentation line says "Click right to
get System Menu". It lies.  Actually, double click right wins.
Single click gets a bleat.






0,, unreproducible, valid, 4, indeterminate,
*** EOOH ***
Date: Monday, 1 July 1985, 14:21-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: addendum
To: bug-zmail@lmi-capricorn
FCC: CAP: /lmi/mt/cc.bb

Sending the previous message appears to have unstuck the background process.
Wierd.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Monday, 1 July 1985, 14:19-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: handling of aborts
To: bug-zmail@cap
FCC: CAP: /lmi/mt/cc.bb

I hit R to make a reply while the background process was still reading in the
Babyl file.  Then I noticed another relevant message, so I hit (I think) C-Abort
to cancel going into reply composition mode.  What happened that I was stuck in
the composition buffer but with the mail-reading comtab still active, and the
background process stopped reading in the file, and the new messages were never
put into their proper place at the end of the list, and once again I've probably
lost the contents of my mail file.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Sunday, 30 June 1985, 12:11-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Modifying rest arg in frame broken.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:


Insert your description of the circumstances here:

I reinvoked the MAKE-PACKAGE frame specifying additional rest arguments.
Note that the rest-arg has one extra level of indirection (that is the
cell with cdr-error).

Ken.

>>TRAP 4490 (BAD-CDR-CODE VMA)
A bad cdr-code was found in memory (at address 10247653).
Backtrace from the debugger:

MAKE-PACKAGE (P.C. = 181)

 Arg 0 (NAME): :LANGUAGE-TOOLS
 Rest arg: ((:USE ** :PREFIX-NAME :LT ...)
Local 1 (NICKNAMES): NIL
Local 2 (USE): ("GLOBAL")
Local 3 (SUPER): NIL
Local 4 (SIZE): 128
Local 5 (IGNORE): NIL
Local 6 (SHADOW): NIL
Local 7 (EXPORT): NIL
Local 8 (PREFIX-NAME): NIL
Local 9 (AUTO-EXPORT-P): NIL
Local 10 (INVISIBLE): NIL
Local 11 (IMPORT): NIL
Local 12 (SHADOWING-IMPORT): NIL
Local 13 (IMPORT-FROM): NIL
Local 14 (RELATIVE-NAMES): NIL
Local 15 (RELATIVE-NAMES-FOR-ME): NIL
Local 16 (IGNORE): NIL
Local 17 (PROPERTIES): NIL
Local 18 (NEW-SYMBOL-FUNCTION): NIL
Local 19 (IGNORE): NIL
Local 20 (IGNORE): NIL
Local 21 (EXTERNAL-ONLY): NIL
Local 22 (TABLE-SIZE): NIL
Local 23 (PKG): NIL
Local 24: NIL
Local 25 (NICK): NIL
Local 26 (SUCCESS): NIL
Local 27 (NAME): NIL
Local 28: NIL
Local 29 (NAME): NIL
Local 30 (SYM): NIL
Local 31 (P): NIL
Local 32: NIL
Local 33 (ELT): NIL


FIND-PACKAGE (P.C. = 147)

 Arg 0 (NAME): (:LANGUAGE-TOOLS :USE (:GLOBAL) :PREFIX-NAME ...)
 Arg 1 (USE-LOCAL-NAMES-PACKAGE): NIL
Local 0 (P): NIL
Local 1 (ELT): NIL
Local 2: NIL
Local 3 (PKG): NIL


PKG-FIND-PACKAGE (P.C. = 78)

 Arg 0 (THING): (:LANGUAGE-TOOLS :USE (:GLOBAL) :PREFIX-NAME ...)
 Arg 1 (CREATE-P): :ERROR
   --Defaulted args:--
 Arg 2 (USE-LOCAL-NAMES-PACKAGE): NIL
Local 0: NIL
Local 1 (NEW-NAME): NIL
Local 2 (STRING1): NIL


Additional information supplied with call:
 Expecting 2 values

(:PROPERTY :PACKAGE FS:FILE-ATTRIBUTE-BINDINGS) (P.C. = 25)

 Arg 0 (IGNORE): #<ZWEI:ZMACS-BUFFER MAPFORMS.LISP#> MLY.L.T; DJ: 60043560>
 Arg 1 (IGNORE): :PACKAGE
 Arg 2 (PKG): (:LANGUAGE-TOOLS :USE (:GLOBAL) :PREFIX-NAME ...)


FS:ATTRIBUTE-BINDINGS-FROM-LIST (P.C. = 34)

 Arg 0 (ATTLIST): (:BASE 10 :READTABLE :COMMON-LISP ...)
 Arg 1 (PATHNAME): #<ZWEI:ZMACS-BUFFER MAPFORMS.LISP#> MLY.L.T; DJ: 60043560>
Local 0 (ATTLIST): (:PACKAGE (:LANGUAGE-TOOLS :USE ** :PREFIX-NAME ...) :MODE :LISP ...)
Local 1 (VARS): (*READTABLE* *READ-BASE* *PRINT-BASE*)
Local 2 (VALS): (#<READTABLE standard Common-Lisp -67313457> 10 10)
Local 3 (BINDING-FUNCTION): #<DTP-FEF-POINTER (:PROPERTY :PACKAGE FS:FILE-ATTRIBUTE-BINDINGS) 40772265>
Local 4 (VARS1): (*READTABLE* *READ-BASE* *PRINT-BASE*)
Local 5 (VALS1): (#<READTABLE standard Common-Lisp -67313457> 10 10)


Remainder of stack:

FS:FILE-ATTRIBUTE-BINDINGS (P.C. = 30)
(:METHOD ZWEI:NODE :ATTRIBUTE-BINDINGS) (P.C. = 22)
ZWEI:INITIALIZE-BUFFER-PACKAGE (P.C. = 60)
ZWEI:REVERT-FILE-BUFFER (P.C. = 358)
(:METHOD ZWEI:ZMACS-BUFFER :REVERT) (P.C. = 36)
ZWEI:REVERT-BUFFER (P.C. = 54)
ZWEI:FIND-FILE (P.C. = 168)
ZWEI:COM-FIND-FILE (P.C. = 39)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Sunday, 30 June 1985, 08:08-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: macpar
To: Moby Lisp Yukko <khs@LMI-CAPRICORN>, BUG-LISPM@LMI-Capricorn
In-reply-to: The message of 29 Jun 1985 05:28-EDT from khs@LMI-CAPRICORN
Message-ID: <[LMI-DJINN].6/30/85 08:08:03.khs>

    Date: Saturday, 29 June 1985, 05:28-EDT
    From: Moby Lisp Yukko <khs at LMI-CAPRICORN>

    In Experimental System 102.80, Experimental Local-File 55.5,
    Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
    Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
    Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:

    I love %internal-mapcar.

Sigh.  The 104 compiler won't use it, and we can punt the ucode in 3.0.

Ken.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Saturday, 29 June 1985, 05:28-EDT
From: Moby Lisp Yukko <khs@LMI-CAPRICORN>
Sender: Mly@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:

I love %internal-mapcar.

(defun make-xpackage (name &key nicknames (use '("LISP")) prefix-name
                               (size 100) shadow export
                               import shadowing-import import-from
                               relative-names relative-names-for-me
                               auto-export-p new-symbol-function
                               invisible properties)
  (setq name (delete-duplicates (mapcar #'simple-stringify
                                        (cons name (xpkg-listify nicknames)))
                                :test #'equal :from-end t))
  ...)
Arg 0 (NAME) is required, local, not initialized.
Rest arg (NIL) is local, not initialized.
Local 1 (NICKNAMES) is local, not initialized.
Local 2 (USE) is local, initialized to '("LISP").
Local 3 (PREFIX-NAME) is local, not initialized.
Local 4 (SIZE) is local, initialized to '100.
Local 5 (SHADOW) is local, not initialized.
Local 6 (EXPORT) is local, not initialized.
Local 7 (IMPORT) is local, not initialized.
Local 8 (SHADOWING-IMPORT) is local, not initialized.
Local 9 (IMPORT-FROM) is local, not initialized.
Local 10 (RELATIVE-NAMES) is local, not initialized.
Local 11 (RELATIVE-NAMES-FOR-ME) is local, not initialized.
Local 12 (AUTO-EXPORT-P) is local, not initialized.
Local 13 (NEW-SYMBOL-FUNCTION) is local, not initialized.
Local 14 (INVISIBLE) is local, not initialized.
Local 15 (PROPERTIES) is local, not initialized.

168 MOVE D-IGNORE LOCAL|0
169 BR-NIL 177
170 CALL D-IGNORE FEF|48      ;#'STORE-KEYWORD-ARG-VALUES
171 (MISC) %STACK-FRAME-POINTER D-PDL
172 MOVE D-PDL LOCAL|0
173 MOVE D-PDL FEF|49         ;'(:NICKNAMES :USE :PREFIX-NAME :SIZE :SHADOW :EXPORT :IMPORT :SHADOWING-IMPORT :IMPORT-FROM :RELATIVE-NAMES :RELATIVE-NAMES-FOR-ME :AUTO-EXPORT-P :NEW-SYMBOL-FUNCTION :INVISIBLE :PROPERTIES)
174 MOVE D-PDL 'NIL
175 PUSH-E LOCAL|1            ;NICKNAMES
176 MOVE D-LAST PDL-POP
177 CALL D-PDL FEF|50         ;#'DELETE-DUPLICATES
178 MOVE D-PDL FEF|51         ;#'SIMPLE-STRINGIFY
179 MOVE D-PDL ARG|0          ;NAME
180 MOVE D-PDL LOCAL|1        ;NICKNAMES
181 (MISC) COMMON-LISP-LISTP D-IGNORE
182 BR-NIL 185
183 MOVE D-PDL LOCAL|1        ;NICKNAMES
184 BR 188
185 MOVE D-PDL LOCAL|1        ;NICKNAMES
186 PUSH-NUMBER 1
187 (MISC) %INTERNAL-LIST D-PDL
188 (MISC) CONS D-PDL
189 (MISC) %INTERNAL-MAPCAR D-PDL
190 MOVE D-PDL FEF|52         ;':TEST
191 MOVE D-PDL FEF|53         ;#'EQUAL
192 MOVE D-PDL FEF|54         ;':FROM-END
193 MOVE D-LAST 'T
194 POP ARG|0                 ;NAME
...

(make-xpackage "FOO" :nicknames () :use () :size 20)

>>TRAP 14043 (ARGTYP FIXNUM M-J NIL (NIL RESTORE-ARRAY-REGISTERS))
Some argument to "FOO", NIL, was an invalid array subscript.
Use a fixnum.
Backtrace from the debugger:

"FOO":
   Arg 0: NIL


MAKE-XPACKAGE (P.C. = 190) (from file DJ: MLY.L.T; PKPK.#)

 Arg 0 (NAME): "FOO"
 Rest arg: (:NICKNAMES NIL :USE NIL ...)
Local 1 (NICKNAMES): NIL
Local 2 (USE): NIL
Local 3 (PREFIX-NAME): NIL
Local 4 (SIZE): 20
Local 5 (SHADOW): NIL
Local 6 (EXPORT): NIL
Local 7 (IMPORT): NIL
Local 8 (SHADOWING-IMPORT): NIL
Local 9 (IMPORT-FROM): NIL
Local 10 (RELATIVE-NAMES): NIL
Local 11 (RELATIVE-NAMES-FOR-ME): NIL
Local 12 (AUTO-EXPORT-P): NIL
Local 13 (NEW-SYMBOL-FUNCTION): NIL
Local 14 (INVISIBLE): NIL
Local 15 (PROPERTIES): NIL
Local 16 (TABLE-SIZE): NIL
Local 17 (PKG): NIL
Local 18 (TEM): NIL
Local 19: NIL
Local 20 (N): NIL
Local 21 (S): NIL
Local 22 (SUCCESS): NIL
Local 23 (SYM): NIL
Local 24: NIL
Local 25 (NICK): NIL
Local 26 (P): NIL
Local 27: NIL
Local 28 (ELT): NIL


EVAL1 (P.C. = 547)

 Arg 0 (FORM): (MAKE-XPACKAGE "FOO" :NICKNAMES NIL ...)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 7
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER MAKE-XPACKAGE 7062626>
Local 6 (ARG-DESC): 4400101
Local 7 (NUM-ARGS): 7
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (MAKE-XPACKAGE "FOO" :NICKNAMES NIL ...)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (MAKE-XPACKAGE "FOO" :NICKNAMES NIL ...)
Local 0: ((TOO-FEW-ARGUMENTS TOO-MANY-ARGUMENTS CELL-CONTENTS-ERROR WRONG-TYPE-ARGUMENT ...) EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


Remainder of stack:

BREAK (P.C. = 437)
ZWEI:COM-BREAK (P.C. = 36)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Friday, 28 June 1985, 11:15-EDT
From: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi,
on Customer Service 2:


Insert your description of the circumstances here:
the following function will work compiled but if it is evaluated
you get thrown into the debugger with the error message

ERROR: DEVICE is referenced as a free variable but not declared special
(defun foo ()
  (let ((device terminal-io))
    (mapc '(lambda (x) (format device "msg ~s" x))
          '(a b c d))))

when you then do a cntrl-m you get thrown into another level of the debugger with
the following backtrace.
Of course if you put #' in front of the lambda expression it will work properly
compiled and evaluated

>>ERROR: The function spec (LAMBDA (X) (FORMAT DEVICE "msg ~s" X)) is invalid.
Backtrace from the debugger:

FUNCTION-SPEC-GET (P.C. = 67)

 Arg 0 (FUNCTION-SPEC): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
 Arg 1 (PROPERTY): :SOURCE-FILE-NAME
   --Defaulted args:--
 Arg 2 (DEFAULT): NIL
Local 0 (HFUN): NIL


GET-ALL-SOURCE-FILE-NAMES (P.C. = 25)

 Arg 0 (FUNCTION-SPEC): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
Local 0 (PROPERTY): NIL


EH:DESCRIBE-FUNCTION-SOURCE-FILE (P.C. = 47)

 Arg 0 (FUNCTION): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
Local 0 (NAME): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
Local 1 (FILE): NIL


EH:PRINT-FUNCTION-AND-ARGS (P.C. = 119)

 Arg 0 (SG): #<DTP-STACK-GROUP "Zmacs Frame 1" 30554602>
 Arg 1 (FRAME): 735
 Arg 2 (DESCRIBE-FUNCTION-SOURCE-FILE): T
Local 0 (FUNCTION): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
Local 1 (FUNCTION-NAME): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))
Local 2 (RP): #<ART-REG-PDL-4096 12047650>
Local 3: (25 . 258)
Local 4: (NIL . 67078)


EH:SHOW-FRAME-FOR-BUG-MESSAGE (P.C. = 62)

 Arg 0 (SG): #<DTP-STACK-GROUP "Zmacs Frame 1" 30554602>
 Arg 1 (FRAME): 735
Local 0 (RP): #<ART-REG-PDL-4096 12047650>
Local 1 (FUNCTION): (LAMBDA (X) (FORMAT DEVICE "msg ~s" X))


Remainder of stack:

(:METHOD CONDITION :BUG-REPORT-DESCRIPTION) (P.C. = 79)
EH:COM-BUG-REPORT (P.C. = 111)
EH:COMMAND-LOOP (P.C. = 447)
(:METHOD CONDITION :DEBUGGER-COMMAND-LOOP) (P.C. = 23)
EH:SECOND-LEVEL-ERROR-HANDLER (P.C. = 712)


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Friday, 28 June 1985, 10:26-EDT
From: khs@LMI-CAPRICORN
Sender: Mly@LMI-CAPRICORN
Subject: Is this ever supposed to work?
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:

(si:debug-warm-booted-process)
after the machine hung trying to get a backtrace after a
thrwo to the non-existent catch tag si::process-wait-in-scheduler whilst in
(:internal chaos::allocate-int-pkt 0) whilst running without-interrupts inside
a long computation inside a a tv:prepare-sheet.

>>TRAP 6812 (ARGTYP ARRAY M-ARRAY-POINTER (GAHDR RESTORE-ARRAY-REGISTERS) GAHDR)
The NIL argument to AR-1, CHAOS:LAMBDA-RECEIVE-ANY-FUNCTION, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

SYMEVAL-IN-STACK-GROUP (P.C. = 54)

 Arg 0 (SYM): EH:ERROR-HANDLER-RUNNING
 Arg 1 (SG): CHAOS:LAMBDA-RECEIVE-ANY-FUNCTION
   --Defaulted args:--
 Arg 2 (FRAME): NIL
 Arg 3 (AS-IF-CURRENT): NIL
Local 0 (VCL): NIL
Local 1 (SP): NIL
Local 2 (SPP): NIL
Local 3 (I): NIL
Local 4 (P): NIL
Local 5 (.SELECTQ.ITEM.): NIL
Local 6 (LOCATION): NIL
Local 7 (INNERMOST-BINDING): NIL
Local 8 (FRAMEP): NIL
Local 9 (VALUE): NIL
Local 10 (BOUNDP): NIL


EH:DEBUG (P.C. = 149)

 Arg 0 (PROCESS): #<SI:SIMPLE-PROCESS Chaos Receiver -67334351>
Local 0 (PKG): NIL
Local 1 (SG): NIL
Local 2 (ARREST-REASON): NIL
Local 3 (CURRENT-FRAME): NIL
Local 4 (INNERMOST-VISIBLE-FRAME): NIL
Local 5 (ERROR-LOCUS-FRAME): NIL
Local 6 (INNERMOST-FRAME-IS-INTERESTING): NIL
Local 7 (EH-ERROR): NIL
Local 8 (ERROR-HANDLER-RUNNING): NIL
Local 9 (CELL): NIL
Local 10: NIL
Local 11: NIL
Local 12: NIL
Local 13: NIL


EH (P.C. = 18)

 Arg 0 (PROCESS): #<SI:SIMPLE-PROCESS Chaos Receiver -67334351>


SI:DEBUG-WARM-BOOTED-PROCESS (P.C. = 24)



SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (SI:DEBUG-WARM-BOOTED-PROCESS)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 0
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER SI:DEBUG-WARM-BOOTED-PROCESS 37430540>
Local 6 (ARG-DESC): 0
Local 7 (NUM-ARGS): 0
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


Remainder of stack:

SI:EVAL-SPECIAL-OK (P.C. = 81)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI:LISP-TOP-LEVEL2 (P.C. = 23)
SI:PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Thursday, 27 June 1985, 22:55-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Common Lisp array borderline cases
To: george@LMI-CAPRICORN
CC: BUG-LISPM@LMI-Capricorn
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: The message of 13 Jun 1985 11:33-EDT from george@LMI-CAPRICORN
Message-ID: <[LMI-EXPLORER-2].6/27/85 22:55:10.RpK>

    Date: Thursday, 13 June 1985, 11:33-EDT
    From: george at LMI-CAPRICORN

    In System 102.117...

    Typing the following at the Lisp Listener results in the following backtrace
    (make-array  (1- array-dimension-limit) :element-type 'bit)

    the value of array-dimension-limit is (^ 2 25) which is not a fixnum which is
    what make-array seems to want

Looks like somebody goofed.  It should be 2^24 (also bignum, but one
less is a fixnum).  By the way, the number is a exclusive limit anyway,
so the correct dimension type is actually the type

        (number 0 (#.array-dimension-limit))

since you can still have (theoretically) that many slots which can
enumerated by fixnums.  But then again, you won't have any space in
which to cons this monster.

By the way, we allow strange things to be created by saying
  (make-array '(0 4))


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 25 June 1985, 10:30-EDT
From: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc, on Customer Service 2:


Insert your description of the circumstances here:

the previous bug report on this was slightly inaccurate. The following
must be compiled and setf must have two arguments the second being nil


(defvar *array* (make-array '(3 3 2187)))

(defun s1 (n)
  (dotimes (i n)
    (setf (aref *array*  0 0 i) nil)))

(s1 1100)
will work
but
(s1 2000)
gets thrown into a trap


0,, issues, 3, valid,
*** EOOH ***
Date: Saturday, 22 June 1985, 06:02-EDT
From: khs@LMI-CAPRICORN
Sender: PECANN@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.149, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.20, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental Object Lisp 2.0,
Experimental vista 1.0, Experimental IRIS 1.0, microcode 770,
on Waiting for Godot:

The compiler lets you compile

   (defun foo (a) (nth-value 1 a))

into code which could concievably crash the machine.

KHS.


0,, issues, 3, valid,
*** EOOH ***
Date: Friday, 21 June 1985, 07:14-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Funky proceed option.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:


Insert your description of the circumstances here:

This error has a really wierd proceed option that prompts you for which
symbol you want.  It should be replaced by two separate proceed options, one
to import the new one, the other to leave the old one.

Ken.

>>ERROR: A symbol named "%REGION-GC-POINTER" is already accessible in package GC.
Backtrace from the debugger:

IMPORT (P.C. = 100)

 Arg 0 (SYMBOLS): (%REGION-ORIGIN %REGION-AREA %REGION-BITS %REGION-LENGTH ...)
   --Defaulted args:--
 Arg 1 (PKG): #<Package GC 5622475>
Local 0: (SI:%REGION-GC-POINTER SI:%REGION-TYPE SI:%REGION-FLIP-ENABLE SI:%REGION-SCAVENGE-ENABLE ...)
Local 1 (SYM): SI:%REGION-GC-POINTER
Local 2 (TEM): %REGION-GC-POINTER
Local 3 (FOUNDP): :INTERNAL



0,, 3, valid,
*** EOOH ***
Date: Thursday, 20 June 1985, 18:39-EDT
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.148, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.20, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi site info,
on Lambda Five A:

Insert your description of the circumstances here:

     Doing copy directory. One file is "1.lisp", which is/was legal
     as far as I know.

     -mark

* * *   N O   * * *


The problem was that (copy-directory "lam10:mhd;" "lm:") used to
do the right thing. Now it does the wrong thing (the one and only
wrong thing, no less), i.e. translating the second argument roughly
into "lm:*;" rather than "lm:mhd;".

-mark


>>TRAP 9754 (TRANS-TRAP)
The function :WILD is undefined.
Backtrace from the debugger:

:WILD:
   Arg 0: :PATHNAME


FS::LM-SIGNAL-ERROR (P.C. = 64)

 Arg 0 (SYMBOL): FS::INVALID-DIRECTORY-NAME
 Arg 1 (SOURCE): :WILD
   --Defaulted args:--
 Arg 2 (PROCEEDABLE): NIL
 Rest arg (MAKE-CONDITION-ARGS): NIL
Local 1 (STRING): NIL
Local 2 (ERROR): NIL


FS::LOOKUP-DIRECTORY (P.C. = 82)

 Arg 0 (NAME): :WILD
   --Defaulted args:--
 Arg 1 (OK-IF-NOT-THERE): NIL
Local 0 (FILE): NIL


FS::LMFS-CREATE-DIRECTORY (P.C. = 22)

 Arg 0 (NAME): :WILD


(:METHOD FS::LOCAL-FILE-ACCESS :CREATE-DIRECTORY) (P.C. = 75)
  (SELF is #<FS::LOCAL-FILE-ACCESS Direct access to LMI-LAMBDA-5-A 65032755>)

 Arg 0 (.OPERATION.): :CREATE-DIRECTORY
 Arg 1 (PATHNAME): #FS::LM-PATHNAME "LAM5: *; 1.LOG#2"
 Arg 2 (ERROR): NIL
Local 0 (IGNORE): NIL


Remainder of stack:

(:METHOD FS::FILE-HOST-MIXIN :ACCESS-OPERATION) (P.C. = 25)
(:METHOD FS::HOST-PATHNAME :CREATE-DIRECTORY) (P.C. = 50)
FS:CREATE-DIRECTORY (P.C. = 154)
FS::FS-COPY-FILE (P.C. = 623)
FS::COPY-DIRECTORY (P.C. = 469)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 20 June 1985, 03:09-EDT
From: Ken Sinclair <mly@LMI-CAPRICORN>
Sender: khs@LMI-CAPRICORN
Subject: LPARSE caching.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:

After interrupting a long Indent Region or C-M-Q, some of the lisp
parser's cached information is invalid, causing (at least) the
matching-paren blinker to appear in the wrong place.

Ken.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 18 June 1985, 22:45-EDT
From: mly@LMI-CAPRICORN
Sender: khs@LMI-CAPRICORN
Subject: eh blowout: %instance-ref
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 770, R2.0 Macsyma, on Djinn:

(%instance-ref <instance> 0)

>>TRAP 3471 (ARGTYP PLUSP M-1 1 NIL %INSTANCE-LOC)
The second argument to %INSTANCE-LOC, #<DTP-SYMBOL-HEADER 1540000>, was of the wrong type.
The function expected a positive number.
Backtrace from the debugger:

%INSTANCE-REF:
   Arg 0 (INSTANCE): -16776188
   Arg 1 (INDEX): 18


EVAL1 (P.C. = 547)

 Arg 0 (FORM): (%INSTANCE-REF ** 0)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 2
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-U-ENTRY %INSTANCE-REF 331>
Local 6 (ARG-DESC): 130
Local 7 (NUM-ARGS): 2
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (%INSTANCE-REF ** 0)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (%INSTANCE-REF ** 0)
Local 0: ((TOO-FEW-ARGUMENTS TOO-MANY-ARGUMENTS CELL-CONTENTS-ERROR WRONG-TYPE-ARGUMENT ...) EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


LISP-TOP-LEVEL1 (P.C. = 272)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 3040000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
Local 0 (OLD-PACKAGE): #<Package SYSTEM-INTERNALS 13453056>
Local 1 (W-PKG): #<Package SYSTEM-INTERNALS 13453056>
Local 2 (LAST-TIME-READTABLE): #<READTABLE standard traditional syntax -67314602>
Local 3 (THROW-FLAG): T
Local 4: ("Return to top level in ~A." "Lisp Listener 1")
Local 5: ((ABORT ERROR) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
Local 6 (VALUES): NIL
Local 7: NIL
Local 8 (VALUE): #<XHASH-ARRAY 1/90 :test EQ -44545705>


Remainder of stack:

LISP-TOP-LEVEL2 (P.C. = 23)
PROCESS-TOP-LEVEL (P.C. = 115)
LISP-TOP-LEVEL (P.C. = 39)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Friday, 14 June 1985, 11:17-EDT
From: debbie@LMI-CAPRICORN
Sender: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-CAPRICORN

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc, on Customer Service 1:


Insert your description of the circumstances here:
  In the system menu,  I moused on split screen, split the screen
into two lisp listeners and created a frame. Then I tried to edit the screen. I Moused on reshape,
choose a lisp listener, placed the edges completely within the lisp listener window, and
got this error.

>>ERROR: Attempt to expose #<TV:LISP-LISTENER Lisp Listener 2 1735131 deexposed> outside of its superior
Backtrace from the debugger:

TV:SHEET-EXPOSE (P.C. = 129)

 Arg 0 (DAEMON-ARGS): (:EXPOSE T)
 Arg 1 (INTERNALS): #<DTP-FEF-POINTER (:INTERNAL (:METHOD TV:LISP-LISTENER :COMBINED :EXPOSE) 0) 25364016>
Local 0 (*SHEETS-MADE-INVISIBLE-TO-MOUSE*): NIL
Local 1 (VAL1): NIL
Local 2 (VAL2): NIL
Local 3 (VAL3): NIL
Local 4 (.QUEUE-LEFT.): T
Local 5 (DONE): NIL
Local 6 (ERROR): (NIL "Attempt to expose ~S outside of its superior" #<TV:LISP-LISTENER Lisp Listener 2 1735131 deexposed>)
Local 7: NIL
Local 8 (SHEET): NIL
Local 9 (E): NIL


(:METHOD TV:LISP-LISTENER :COMBINED :EXPOSE) (P.C. = 31)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 2 1735131 deexposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:EXPOSE T)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-5 23007547>


(:METHOD TV:SELECT-MIXIN :BEFORE :SELECT) (P.C. = 54)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 2 1735131 deexposed>)

 Arg 0 (.OPERATION.): :SELECT
   --Defaulted args:--
 Arg 1 (SAVE-SELECTED): T
Local 0 (OSW): NIL
Local 1 (SHEET): NIL


(:METHOD TV:LISP-LISTENER :COMBINED :SELECT) (P.C. = 128)
  (SELF is #<TV:LISP-LISTENER Lisp Listener 2 1735131 deexposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:SELECT)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-5 23007547>
Local 2 (ARGS): NIL
Local 3 (.QUEUE-LEFT.): T
Local 4: NIL
Local 5 (E): NIL


(:METHOD TV:SPLIT-SCREEN-FRAME :COMBINED :SELECT) (P.C. = 74)
  (SELF is #<TV:SPLIT-SCREEN-FRAME Split-screen frame 1735056 deexposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:SELECT)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-4 6633421>
Local 2 (ARGS): NIL
Local 3 (.QUEUE-LEFT.): NIL
Local 4: NIL
Local 5 (E): NIL


Remainder of stack:

(:METHOD TV:ESSENTIAL-ACTIVATE :SET-STATUS) (P.C. = 54)
TV:SYSTEM-SET-EDGES (P.C. = 405)
(:METHOD TV:ESSENTIAL-SET-EDGES :SET-EDGES) (P.C. = 21)
(:METHOD TV:BASIC-FRAME :COMBINED :SET-EDGES) (P.C. = 83)
TV:EDIT-SCREEN (P.C. = 226)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
(:METHOD TV:MENU-EXECUTE-MIXIN :EXECUTE) (P.C. = 102)
(:METHOD TV:MOMENTARY-MENU :COMBINED :EXECUTE) (P.C. = 42)
(:METHOD TV:BASIC-MENU :CHOOSE) (P.C. = 52)
(:INTERNAL (:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) 0) (P.C. = 60)
(:METHOD TV:BASIC-MOMENTARY-MENU :AROUND :CHOOSE) (P.C. = 50)
(:METHOD TV:DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU :COMBINED :CHOOSE) (P.C. = 39)
(:INTERNAL TV:MOUSE-CALL-SYSTEM-MENU 0) (P.C. = 34)
SI:PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 66)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, fixed, 3, valid,
*** EOOH ***
Date: Friday, 14 June 1985, 10:42-EDT
From: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-CAPRICORN

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1,
Experimental MICRO-COMPILATION-TOOLS 4.0, microcode 768, si pr vi ma gc,
on Customer Service 2:


Insert your description of the circumstances here:

microcompile

(defun r ()
        (loop (return)))

the actual function that caused the probelm was more interesting.
Could we fix this limitation or at least document it.


>>ERROR: UNKNOWN-MISC: (RETURN-LIST)
Backtrace from the debugger:

COMPILER:BARF (P.C. = 66)

 Arg 0 (EXP): (RETURN-LIST)
 Arg 1 (REASON): COMPILER:UNKNOWN-MISC
 Arg 2 (SEVERITY): COMPILER:BARF


COMPILER:MC-MISC (P.C. = 153)

 Arg 0 (DEST): COMPILER:D-RETURN
 Arg 1 (TAIL): (RETURN-LIST)
Local 0 (MISC-FCTN): RETURN-LIST
Local 1 (NARGS): NIL


COMPILER:MC-3 (P.C. = 248)

 Arg 0 (WD): (COMPILER:MISC COMPILER:D-RETURN RETURN-LIST)
Local 0 (TEM): NIL
Local 1 (TEM1): NIL


COMPILER:MC-PROCESS-CODE (P.C. = 37)

 Arg 0 (END-TAG): NIL


COMPILER:MICRO-COMPILE0 (P.C. = 192)

 Arg 0 (FCTN): ((COMPILER:MFEF R NIL NIL ...) (COMPILER:QTAG COMPILER:S-V-BASE) (COMPILER:S-V-BLOCK) (COMPILER:QTAG COMPILER:DESC-LIST-ORG) ...)
 Arg 1 (MC-MODE): STORE


Remainder of stack:

COMPILER:MICRO-COMPILE-INTERNAL (P.C. = 25)
COMPILER:QC-TRANSLATE-FUNCTION (P.C. = 406)
COMPILER:COMPILE-1 (P.C. = 67)
ZWEI:COMPILE-BUFFER-FORM (P.C. = 76)
ZWEI:COMPILE-BUFFER-FORM (P.C. = 46)
ZWEI:COMPILE-INTERVAL-PROCESS-BASIC-FORM (P.C. = 28)
COMPILER:COMPILE-DRIVER (P.C. = 588)
ZWEI:COMPILE-INTERVAL-PROCESS-FN (P.C. = 24)
COMPILER:COMPILE-STREAM (P.C. = 580)
(:INTERNAL ZWEI:COMPILE-INTERVAL ZWEI:DO-IT) (P.C. = 45)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0, forwarded,, fixed, 3, valid,
*** EOOH ***
Date: Thursday, 13 June 1985, 15:26-EDT
From: george@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0,
DOE-Macsyma 9.9, Macsyma-Help-Database 1.1, microcode 768,
si pr vi ma gc, on Customer Service 2:


Insert your description of the circumstances here:

(cli:atan 1) results in the following but according to the orangual and
Steele's CLTL the second argument is optional

>>TRAP 3030 (FUNCTION-ENTRY)
Function CLI:ATAN called with only 1 argument.
Backtrace from the debugger:

CLI:ATAN (P.C. = 16)

 Arg 0 (Y): 1
   --Missing args:--
 Arg 1 (X)


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (CLI:ATAN 1)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER CLI:ATAN 23426136>
Local 6 (ARG-DESC): 130
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (CLI:ATAN 1)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (CLI:ATAN 1)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


APPLY (P.C. = 24)

 Arg 0 (FUNCTION): SI:EVAL-ABORT-TRIVIAL-ERRORS
 Rest arg (ARGS): ((**))
Local 1 (ARGL): NIL
Local 2 (RESTARGL): NIL


Remainder of stack:

SYSTEM:EVAL1 (P.C. = 547)
COND (P.C. = 58)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
MULTIPLE-VALUE-LIST (P.C. = 21)
SYSTEM:EVAL1 (P.C. = 547)
SETQ (P.C. = 86)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 11 June 1985, 21:17-EDT
From: mt@LMI-CAPRICORN
Sender: @LMI-CAPRICORN
Subject: handling of THROW-TRAP
To: BUG-LISPM@LMI-Capricorn

In System 102.144, Local-File 56.9, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental TCP-Kernel 28.1,
Experimental TCP-User 55.5, Experimental TCP-Server 32.2, microcode 770,
Official working band for Boris//Natasha -dg, on Lambda Six:


I typed (throw 'foo 'bar) to a lisp listener.  The report of the value
being thrown is wrong.  This seems related to my earlier bug report,
that proceeding this error causes DTP-TRAP words to be read.

>>TRAP 2471 (THROW-TRAP)
There was no pending *CATCH for the tag FOO.
The value being thrown was #<DTP-FEF-POINTER SYSTEM:EVAL1 35021666>.

Backtrace from the debugger:

Additional information supplied with call:
 SYSTEM:ADI-FEXPR-CALL

THROW (P.C. = 33)

 Arg 0 (TAG): FOO
 Rest arg (VALUE-EXPRESSION): ((QUOTE BAR))


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (THROW (QUOTE FOO) (QUOTE BAR))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER THROW 2610404>
Local 6 (ARG-DESC): 2621505
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): ((QUOTE BAR))
Local 12 (ADL): (288)
Local 13 (ITEM): 0
Local 14 (.SELECTQ.ITEM.): 0


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (THROW (QUOTE FOO) (QUOTE BAR))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (THROW (QUOTE FOO) (QUOTE BAR))
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI::EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


SI:LISP-TOP-LEVEL1 (P.C. = 272)

 Arg 0 (*TERMINAL-IO*): #<TV:LISP-LISTENER Lisp Listener 1 1700000 exposed>
   --Defaulted args:--
 Arg 1 (TOP-LEVEL-P): T
Local 0 (OLD-PACKAGE): #<Package USER 10054742>
Local 1 (W-PKG): #<Package USER 10054742>
Local 2 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 30267261>
Local 3 (THROW-FLAG): T
Local 4: ("Return to top level in ~A." "Lisp Listener 1")
Local 5: ((SYSTEM:ABORT ERROR) ("Return to top level in ~A." "Lisp Listener 1") T ("Return to top level in ~A." "Lisp Listener 1") ...)
Local 6 (VALUES): NIL
Local 7: NIL
Local 8 (VALUE): NIL


Remainder of stack:

SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 11 June 1985, 19:50-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: MITRE VAX file server problem
To: robert@LMI-CAPRICORN
CC: BUG-LISPM@LMI-Capricorn
In-reply-to: The message of 9 Jul 1985 12:33-EDT from robert@LMI-CAPRICORN
Message-ID: <[LMI-OLIVER-TWIST].6/11/85 19:50:32.RpK>

    Date: Tuesday, 9 July 1985, 12:33-EDT
    From: robert@LMI-CAPRICORN

    In System 102.122...

    Mitre's VAX keeps only the five most recent versions of files
    around.

This must be VMS.  Is our Chaosnet software running on that machine, or
are they using TCP ?

    When a file is to be saved, if five versions already exist, the oldest
    version is deleted first.  If the owner of the earliest version is
    different from the current user, however, the file won't be deleted, and
    an appropriate error message is displayed.

Who displays the error message ?

    Unfortunately, the release 2.0 software doesn't appear to recognize this
    situation.  In spite of the VAX's having refused to save the new file,
    the user is told that the file was written successfully.

It won't recognise the error if it never gets an error message from the
file (Chaos FILE, Chaos GETFIL, TCP FTP) server.  If the file still gets
written, there really is no error.  (Kind of a strange case, really.)


0,, doc.prob, 3, valid,
*** EOOH ***
Date: Tuesday, 11 June 1985, 19:35-EDT
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: bug-lispm@LMI-CAPRICORN

    From dg@LMI-CAPRICORN Thu May 30 17:01:55 1985
    Date: Thursday, 30 May 1985, 15:49-EDT
    From: Dave Goodine <dg at LMI-CAPRICORN>
    To: mhd at LMI-CAPRICORN
    In-reply-to: The message of 20 Mar 1985 19:03-EST from mhd at LMI-CAPRICORN
    Status: O

        Date: Wednesday, 20 March 1985, 19:03-EST
        From: mhd at LMI-CAPRICORN

        In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
        MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
        MEDIUM-RESOLUTION-COLOR 17.3, microcode 753, on Lambda Fifteen:


        Insert your description of the circumstances here:

        Apparently there's an upper limit on the size of a window sheet.
        This is undocumented, and Picon graphics assumes no such limit, hence
        this error.

        -mhd


        >>TRAP 11507 (BITBLT-DESTINATION-TOO-SMALL)
        The destination of a BITBLT was too small.
        Backtrace from the debugger:

        TV:GROW-BIT-ARRAY (P.C. = 161)

         Arg 0 (ARRAY): #<ART-1B-200-1024 61324532>
         Arg 1 (WIDTH): 1024

    I'd like to see this if you can reproduce it.
    -dg


Ok, here:

(defun dare ()
 (let ((w (make-instance 'tv:window ':save-bits t)))
   (loop with x = 100
         do
           (cond
             ((y-or-n-p "Give up?")
              (return w))
             (t (format t "~%Ok. ~D pixels wide..." x)
                (send w ':set-inside-size x 100)
                (setq x (* x 10.)))))))

Ken Sinclair has already figured out what the problem
is, I believe, and will be able to fix it soon.

-mark


0,, doc.prob, 3, valid,
*** EOOH ***
Date: Tuesday, 11 June 1985, 17:18-EDT
From: debbie@LMI-CAPRICORN
Sender: @LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, site files 2.0,
on Customer Service 1:


Insert your description of the circumstances here:

I defined the following defstructs in common-lisp and
zeta-lisp. In common-lisp  the :print-function option can only be
compiled if the structure is named, and in zeta-lisp it only works if
the structure is not named.
The only restriction I could find on :print-function is in Steele (p.314)
it says it may be used only if the :type option is not specified.
I didn't specify the :type option, but it seems as if :named has some
bearing upon this.

;;; -*- Mode:LISP; Readtable:CL -*-

(defstruct (cl-not-1
             (:print-function
               (lambda (struct stream depth)
                 (format stream "test ~S"
                         (zap struct))))) zap ab)

(defstruct (cl-named-1 :named
             (:print-function
               (lambda (struct stream depth)
                 (format stream "test ~S"
                         (zap struct))))) zap ab)

;;;--------  zeta lisp -----------
(defstruct (zl-not-1
             (:print-function
               (lambda (struct stream depth)
                 (format stream "test ~S"
                         (zap struct))))) zap ab)

(defstruct (zl-named-1 :named
             (:print-function
               (lambda (struct stream depth)
                 (format stream "test ~S"
                         (zap struct))))) zap ab)

>>ERROR: :PRINT or :PRINT-FUNCTION is allowed only for recognizable named structures: CL-NAMED-1
Backtrace from the debugger:

SI:DEFSTRUCT-PARSE-OPTIONS (P.C. = 812)

 Arg 0 (OPTIONS): (CL-NAMED-1 :NAMED (:PRINT-FUNCTION **))
 Arg 1 (CLIP): T
Local 0 (NAME): CL-NAMED-1
Local 1 (TYPE): :PHONY-NAMED-VECTOR
Local 2 (CONSTRUCTORS): ((MAKE-CL-NAMED-1))
Local 3 (ALTERANT): NIL
Local 4 (INCLUDED): NIL
Local 5 (NAMED-P): :PHONY
Local 6 (BUT-FIRST): NIL
Local 7 (DESCRIPTION): (SI:ONE :PHONY-NAMED-VECTOR NIL NIL ...)
Local 8 (OLD): NIL
Local 9 (OP): :PRINT-FUNCTION
Local 10 (VAL): (LAMBDA (STRUCT STREAM DEPTH) (FORMAT STREAM "test ~S" **))
Local 11 (VALS): ((LAMBDA ** **))
Local 12 (OPTIONS): NIL
Local 13: NIL
Local 14: NIL
Local 15 (NEW): NIL
Local 16 (TYPE-DESCRIPTION): (SI:PHONY-NAMED-VECTOR-DEFSTRUCT-REF 1 SI:PHONY-NAMED-VECTOR-DEFSTRUCT-CONS :ALIST ...)
Local 17 (D): NIL
Local 18 (X): NIL


SI:DEFSTRUCT-1 (P.C. = 79)

 Arg 0 (OPTIONS): (CL-NAMED-1 :NAMED (:PRINT-FUNCTION **))
 Arg 1 (ITEMS): (ZAP AB)
 Arg 2 (CLIP): T
Local 0 (DESCRIPTION): NIL
Local 1 (TYPE-DESCRIPTION): NIL
Local 2 (NAME): NIL
Local 3 (DOC): NIL
Local 4 (NEW-SLOTS): NIL
Local 5 (RETURNS): NIL
Local 6 (ALTERANT): NIL
Local 7 (SIZE-MACRO): NIL
Local 8 (SIZE-SYMBOL): NIL
Local 9 (PREDICATE): NIL
Local 10 (COPIER): NIL
Local 11 (COPY-FUN): NIL
Local 12 (I): NIL
Local 13 (L): NIL


CLI:DEFSTRUCT (P.C. = 34)

 Arg 0 (*MACROARG*): (CLI:DEFSTRUCT (CL-NAMED-1 :NAMED **) ZAP AB)
 Arg 1 (*MACROENVIRONMENT*): NIL
Local 0 (OPTIONS): (CL-NAMED-1 :NAMED (:PRINT-FUNCTION **))
Local 1 (ITEMS): (ZAP AB)


FUNCALL (P.C. = 21)

 Arg 0 (FN): #<DTP-FEF-POINTER CLI:DEFSTRUCT 23274224>
 Rest arg (ARGS): ((CLI:DEFSTRUCT ** ZAP AB) NIL)


MACROEXPAND-1 (P.C. = 195)

 Arg 0 (MACRO-CALL): (CLI:DEFSTRUCT (CL-NAMED-1 :NAMED **) ZAP AB)
   --Defaulted args:--
 Arg 1 (ENVIRONMENT): NIL
Local 0 (LOCAL-MACROS): NIL
Local 1 (TM): (MACRO . #<DTP-FEF-POINTER CLI:DEFSTRUCT 23274224>)
Local 2 (TAIL): NIL
Local 3 (FRAME): NIL
Local 4 (AINF): 66


Remainder of stack:

COMPILER:COMPILE-DRIVER (P.C. = 241)
ZWEI:COMPILE-INTERVAL-PROCESS-FN (P.C. = 24)
COMPILER:COMPILE-STREAM (P.C. = 580)
(:INTERNAL ZWEI:COMPILE-INTERVAL ZWEI:DO-IT) (P.C. = 45)
ZWEI:COMPILE-INTERVAL (P.C. = 284)
ZWEI:COMPILE-PRINT-INTERVAL (P.C. = 123)
ZWEI:COMPILE-DEFUN-INTERNAL (P.C. = 115)
ZWEI:COM-EVALUATE-REGION (P.C. = 34)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, doc.prob, 3, valid,
*** EOOH ***
Date: Monday, 10 June 1985, 11:52-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Hardcopy/Font Lossage; Design Inadequacies
To: dg@LMI-CAPRICORN
CC: MT@lmi-capricorn, BUG-LISPM@LMI-Capricorn, bug-lmman@cap
In-reply-to: The message of 30 May 1985 19:48-EDT from dg@LMI-CAPRICORN
Message-ID: <[LMI-NATASHA].6/10/85 11:52:57.RpK>

    Date: Thursday, 30 May 1985, 19:48-EDT
    From: Dave Goodine <dg at LMI-CAPRICORN>

        Date: Thursday, 30 May 1985, 17:23-EDT
        From: Michael Travers <MT at lmi-capricorn>

        In System 102.139 ...

        I tried to print a file (via M-X Print File in ZWEI).  I believe this error is
        due to the presence of epsilons that aren't font change codes.  The losing line
        was:
         (defconstant font-char #\)
        This is perfectly legitimate as far as the editor is concerned, since there are no
        fonts specified in the file's attribute list.  The hardcopy software ought to be
        able to handle it more gracefully.

    Ok on the bug, but you really should use #/epsilon.

But it is not required of him to do so.

    Fixing it in PRINT-BUFFER may not be easy.

This is a half-truth.  The problem is the way it's using ZWEI:INTERVAL-STREAM.
If there is just one font for the buffer, then a stream which yields
only font-0 characters (never epsilons) is passed to SI:HARDCOPY-STREAM.
(I find this rather pointless.)  If there is more than one font, then a
ZWEI:INTERVAL-STREAM is called with a fourth argument of T, which makes
a stream which yields the font-change codes as epsilons.  There is no
:FONTS or similar keyword passed to the hardcopy function; the software
must figure the presence  of fonts out for itself with
FS:READ-FILE-ATTRIBUTE-LIST.  (Or somesuch function.)  Whatever
printer's software that MT was using did not make this check.  (Another
loss of doing things this way is that the stream must be ``rewindable,''
i.e., handle :SET-POINTER 0.)

I think it would be clearer to do something like

  (si:hardcopy-stream (interval-stream *interval* () () :tyi)
                      :font-change :character
                      :lispm-fonts (send *interval* :get-attribute :fonts))

where :LISPM-FONTS would always mean the fonts to use if a hardcopy
device could emulate it.  (This is different from the :FONT-LIST option
in that is not printer-specific and does not have to be strictly
supported.)  The :FONT-CHANGE keyword would tell the hardcopy function
how the font changes were encoded; :CHARACTER would mean to pay
attention to the font codes of the characters. Currently, the epsilon
convention is supposed to supported by the hardcopy functions if the
:FONT-LIST option is supplied; I suppose this would have to be the
default, still.

This, of course, would break everything.

Bug-LMMan: The documentation for ZWEI:INTERVAL-STREAM is wrong, the
hack-font argument value is :TYI, NOT :TYO.  Maybe it would just be
easier to change the code.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Sunday, 9 June 1985, 22:24-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: zwei:standalone-eidtor-window
To: BUG-LISPM@LMI-Capricorn
CC: bug-lmman@cap

In System 102.143, Local-File 56.7, FILE-Server 13.2, Unix-Interface 5.4,
MagTape 40.19, ZMail 57.7, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental BURN-IN 2.0, microcode 768,
Boris' Working Band, on Natasha Nogoodnik:


Insert your description of the circumstances here:

How the hell is somebody supposed to use this flavor?  Anytime I try to
make an instance of it or do anything to it, it dumps me into the cold
load stream or otherwise performs some insult or indignity on me or my
machine.

>>TRAP 9754 (TRANS-TRAP)
The instance variable ZWEI::MODE-LINE-WINDOW is unbound in #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>.
Backtrace from the debugger:

(:METHOD ZWEI::EDITOR-WINDOW-WITH-POP-UP-MINI-BUFFER-MIXIN :BEFORE :PREPARE-FOR-REDISPLAY) (P.C. = 69)
  (SELF is #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>)

 Arg 0 (.OPERATION.): :PREPARE-FOR-REDISPLAY
Local 0 (.CURRENT-WINDOW.): NIL
Local 1 (.QUEUE-LEFT.): NIL
Local 2 (.FLAG.): NIL
Local 3: NIL
Local 4 (E): NIL
Local 5 (CH): NIL


(:METHOD ZWEI:STANDALONE-EDITOR-WINDOW :COMBINED :PREPARE-FOR-REDISPLAY) (P.C. = 37)
  (SELF is #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:PREPARE-FOR-REDISPLAY)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-5 27345015>


ZWEI::PREPARE-WINDOW-FOR-REDISPLAY (P.C. = 18)

 Arg 0 (WINDOW): #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>


(:METHOD ZWEI::ZWEI :AFTER :REFRESH) (P.C. = 41)
  (SELF is #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>)

 Arg 0 (.OPERATION.): :REFRESH
 Arg 1 (TYPE): :COMPLETE-REDISPLAY


(:METHOD ZWEI::ZWEI-WITHOUT-TYPEOUT :COMBINED :REFRESH) (P.C. = 51)
  (SELF is #<ZWEI:STANDALONE-EDITOR-WINDOW Standalone Editor Window 1 1731122 deexposed>)

 Rest arg (.DAEMON-CALLER-ARGS.): (:REFRESH :COMPLETE-REDISPLAY)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-5 27344651>


Remainder of stack:

(:METHOD TV:SHEET :EXPOSE) (P.C. = 408)
(:INTERNAL (:METHOD ZWEI:STANDALONE-EDITOR-WINDOW :COMBINED :EXPOSE) 0) (P.C. = 54)
TV::SHEET-EXPOSE (P.C. = 112)
(:METHOD ZWEI:STANDALONE-EDITOR-WINDOW :COMBINED :EXPOSE) (P.C. = 59)
(:METHOD TV:SELECT-MIXIN :BEFORE :SELECT) (P.C. = 61)
(:METHOD TV:TYPEOUT-WINDOW :COMBINED :SELECT) (P.C. = 123)
(:METHOD ZWEI::ZWEI-WITHOUT-TYPEOUT :COMBINED :SELECT) (P.C. = 88)
FUNCALL (P.C. = 21)
SYSTEM:EVAL1 (P.C. = 547)
SYSTEM:EVAL1 (P.C. = 305)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Saturday, 8 June 1985, 22:45-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: [CJL at OZ: ZMAIL BUG]
To: bug-lispm@cap
Message-ID: <[LMI-DJINN].6/08/85 22:45:20.khs>

We have this same bug.

Date: Friday, 7 June 1985, 23:20-EDT
From: Chris Lindblad <CJL at OZ>
Subject: ZMAIL BUG
To: BUG-LISPM at OZ
Message-ID: <850607232043.3.CJL@ELVIS>

In Symbolics 3600 Release 6.0, IP-TCP 29.0, AISite 9.3,
microcode TMC5-MIC 319, FEP 22, on Lisp Machine Elvis Presley:

Tk's logged into elvis. CJL logs in. He types system-m, gets zmail,
which has nothing loaded in. Types g, and CJL's mail is loaded into the
machine. Great. He deals with his mail, and types s. IT THEN PROCEEDS TO
SAVE CJL's MAIL IN TK's MAIL FILE. Bad computer.

(This bug trace was produced typing s a second time, after I quickly
typed c-abort for the first s. I typed s the second time to make a
backtrace to show you.)

>>Error: File system bug on host MC:
         Channel not open
         For MC: TK; TK BABYL
While in the function (:METHOD FS:HOST-UNIT :COMMAND)  (:METHOD FS:FILE-DATA-STREAM-MIXIN :COMMAND)  (:METHOD FS:FILE-OUTPUT-STREAM-MIXIN :REAL-CLOSE)

(:METHOD FS:HOST-UNIT :COMMAND):  (P.C. = 236)
   Arg 0 (SELF): #<HOST-UNIT 722105>
   Arg 1 (SELF-MAPPING-TABLE): FS:HOST-UNIT
   Arg 2 (OPERATION): :COMMAND
   Arg 3 (MARK-P): :OUTPUT
   Arg 4 (STREAM-OR-HANDLE): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Arg 5 (SIMPLE-P): NIL
   Arg 6 (WHOSTATE): "Close"
   Rest arg (COMMANDS): ("CLOSE")
   Local 8 (HANDLE): "O2509"
   Local 9 (STREAM): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Local 10 (PKT): #<Chaos Packet 6004067>
   Local 11 (SUCCESS): NIL
   Local 12 (STRING): "O2509 ERROR BUG F Channel not open"
   Local 13 (TRANSACTION-ID): "T2516"
   Local 14 (CREATE-P): NIL
   Local 15 (CONDITION): NIL
   Local 16: T
   Local 17: #<CASE-HANDLER 120711625>
   Local 18: (#<CASE-HANDLER 121426367> #<LEXICAL-CLOSURE (:INTERNAL ** 0.) 43640056>)
   Local 19: (#<CASE-HANDLER 120711625> #<CASE-HANDLER 121426367> #<LEXICAL-CLOSURE (:INTERNAL ** 0.) 43640056>)

(:METHOD FS:FILE-DATA-STREAM-MIXIN :COMMAND):  (P.C. = 14)
   Arg 0 (SELF): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor FS:FILE-DATA-STREAM-MIXIN -- 6. IV's, 0. FL's 104532157>
   Arg 2 (OPERATION): :COMMAND
   Arg 3 (MARK-P): :OUTPUT
   Arg 4 (WHOSTATE): "Close"
   Arg 5 (COM): "CLOSE"
   Rest arg (STRINGS): NIL

(:METHOD FS:FILE-OUTPUT-STREAM-MIXIN :REAL-CLOSE):  (P.C. = 56)
   Arg 0 (SELF): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor FS:FILE-OUTPUT-STREAM-MIXIN -- 6. IV's, 1. FL's 104532117>
   Arg 2 (OPERATION): :REAL-CLOSE
   Arg 3 (ABORT-P): :ABORT

(:METHOD FS:FILE-DATA-STREAM-MIXIN :CLOSE):  (P.C. = 26)
   Arg 0 (SELF): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor FS:FILE-DATA-STREAM-MIXIN -- 6. IV's, 0. FL's 104532157>
   Arg 2 (OPERATION): :CLOSE
   Arg 3 (ABORTP): :ABORT

(:METHOD FS:FILE-OUTPUT-CHARACTER-STREAM :COMBINED :CLOSE):  (P.C. = 21)
   Arg 0 (SELF): #<FILE-OUTPUT-CHARACTER-STREAM "MC: TK; TK BABYL" 27271726>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor FS:FILE-OUTPUT-CHARACTER-STREAM -- 1. IV's, 8. FL's 104532127>
   Arg 2 (.OPERATION.): :CLOSE
   Rest arg (.DAEMON-CALLER-ARGS.): (:ABORT)

(:METHOD ZWEI:FILE-MAIL-BUFFER :ABORT-SAVE):  (P.C. = 23)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:FILE-MAIL-BUFFER -- 9. IV's, 1. FL's 142630564>
   Arg 2 (OPERATION): :ABORT-SAVE
   Arg 3 (TWICE): NIL

(:INTERNAL (:METHOD ZWEI:BABYL-BUFFER :COMBINED :ABORT-SAVE) 0.):  (P.C. = 11)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:BABYL-BUFFER -- 3. IV's, 6. FL's 142630520>
   Arg 2 (.OPERATION.): :ABORT-SAVE
   Rest arg (.DAEMON-CALLER-ARGS.): (NIL)

(:METHOD ZWEI:MSG-BUFFER :WHOPPER :ABORT-SAVE):  (P.C. = 30)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:MSG-BUFFER -- 15. IV's, 0. FL's 142630543>
   Arg 2 (.WHOPPER-CONTINUATION.): #<DTP-COMPILED-FUNCTION (:INTERNAL (:METHOD ZWEI:BABYL-BUFFER :COMBINED :ABORT-SAVE) 0.) 47714400>
   Arg 3 (.OLD-SELF-MAPPING-TABLE.): #<Map to flavor ZWEI:BABYL-BUFFER -- 3. IV's, 6. FL's 142630520>
   Arg 4 (.OPERATION.): :ABORT-SAVE
   --Defaulted args:--
   Arg 5 (TWICE): NIL

(:METHOD ZWEI:BABYL-BUFFER :COMBINED :ABORT-SAVE):  (P.C. = 13)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:BABYL-BUFFER -- 3. IV's, 6. FL's 142630520>
   Arg 2 (.OPERATION.): :ABORT-SAVE
   Rest arg (.DAEMON-CALLER-ARGS.): NIL

ZWEI:FOREGROUND-BACKGROUND-FINISH:  (P.C. = 106)
   Arg 0 (BUFFER): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   --Defaulted args:--
   Arg 1 (ABORT-P): T

(:METHOD ZWEI:MSG-BUFFER :BEFORE :EXPUNGE):  (P.C. = 4)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:MSG-BUFFER -- 15. IV's, 0. FL's 142630543>
   Arg 2 (OPERATION): :EXPUNGE
   Arg 3 (IGNORE): T

(:METHOD ZWEI:ITS-BUFFER-MIXIN :COMBINED :EXPUNGE):  (P.C. = 31)
   Arg 0 (SELF): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ITS-BUFFER-MIXIN -- 0. IV's, 3. FL's 142630534>
   Arg 2 (.OPERATION.): :EXPUNGE
   Rest arg (.DAEMON-CALLER-ARGS.): (T)

ZWEI:EXPUNGE-SEQUENCE:  (P.C. = 6)
   Arg 0 (SEQUENCE): #<BABYL-BUFFER "MC: TK; TK BABYL" 723110>
   --Defaulted args:--
   Arg 1 (DELETED-MSGS): T

ZWEI:ZMAIL-SAVE-ALL:  (P.C. = 21)

ZWEI:COM-ZMAIL-SAVE:  (P.C. = 16)

ZWEI:COMMAND-EXECUTE:  (P.C. = 57)
   Arg 0 (COMMAND): ZWEI:COM-ZMAIL-SAVE
   Arg 1 (CHAR): 115.
   --Defaulted args:--
   Arg 2 (PREFIX-CHAR): NIL
   Arg 3 (HOOK-LIST): NIL

ZWEI:ZMAIL-COMMAND-EXECUTE:  (P.C. = 6)
   Arg 0: ZWEI:COM-ZMAIL-SAVE

(:METHOD ZWEI:ZMAIL-FRAME :PROCESS-COMMAND-CHAR):  (P.C. = 7)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (OPERATION): :PROCESS-COMMAND-CHAR
   Arg 3 (CH): 115.

(:INTERNAL (:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :PROCESS-COMMAND-CHAR) 0.) 0.):  (P.C. = 10)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (.OPERATION.): :PROCESS-COMMAND-CHAR
   Rest arg (.DAEMON-CALLER-ARGS.): (115.)

(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :WHOPPER :PROCESS-COMMAND-CHAR):  (P.C. = 21)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-COMMAND-LOOP-MIXIN -- 6. IV's, 0. FL's 142611503>
   Arg 2 (.WHOPPER-CONTINUATION.): #<DTP-COMPILED-FUNCTION (:INTERNAL (:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :PROCESS-COMMAND-CHAR) 0.) 0.) 47674121>
   Arg 3 (.OLD-SELF-MAPPING-TABLE.): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 4 (.OPERATION.): :PROCESS-COMMAND-CHAR
   Arg 5 (CHAR): 115.

(:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :PROCESS-COMMAND-CHAR) 0.):  (P.C. = 13)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (.OPERATION.): :PROCESS-COMMAND-CHAR
   Rest arg (.DAEMON-CALLER-ARGS.): (115.)

(:METHOD ZWEI:ZMAIL-FRAME :WHOPPER :PROCESS-COMMAND-CHAR):  (P.C. = 21)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (.WHOPPER-CONTINUATION.): #<DTP-COMPILED-FUNCTION (:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :PROCESS-COMMAND-CHAR) 0.) 47674106>
   Arg 3 (.OLD-SELF-MAPPING-TABLE.): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 4 (.OPERATION.): :PROCESS-COMMAND-CHAR
   Arg 5 (CHAR): 115.

(:METHOD ZWEI:ZMAIL-FRAME :COMBINED :PROCESS-COMMAND-CHAR):  (P.C. = 12)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (.OPERATION.): :PROCESS-COMMAND-CHAR
   Rest arg (.DAEMON-CALLER-ARGS.): (115.)

(:METHOD ZWEI:ZMAIL-FRAME :COMMAND-LOOP):  (P.C. = 143)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. IV's, 20. FL's 142611405>
   Arg 2 (OPERATION): :COMMAND-LOOP

(:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP):  (P.C. = 70)
   Arg 0 (SELF): #<ZMAIL-FRAME Main Zmail Window 43200617 exposed>
   Arg 1 (SELF-MAPPING-TABLE): #<Map to flavor ZWEI:ZMAIL-FRAME -- 65. I




0,, issues, valid, 2,
*** EOOH ***
Date: Friday, 7 June 1985, 20:17-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Restoring Files on Cap
To: rjpi@LMI-CAPRICORN
CC: RPK@CAP, bug-lispm@cap, bug-unix@cap
In-reply-to: The message of 29 May 1985 19:52-EDT from rjpi@LMI-CAPRICORN
Message-ID: <[LMI-BORIS].6/07/85 20:17:45.RpK>

    Date: Wednesday, 29 May 1985, 19:52-EDT
    From: rjpi at LMI-CAPRICORN

    My LISPM crashed while I was in ZMail, and it wiped out my primary mail
    file.  Is there backup on Cap?  (I hadn't read the file in a while, so
    it should be on tape in there is backup.)  If there is, who do I contact
    about retrieving the file?

    Thanks,
    Bob

The best you can do is to ask whoever does backups (i.e., cd /; tar c ...)
on Capricorn get a recent version back for you....

The way that LMI Unix FILE servers do file output is WRONG.  If the data
connection associated with writing a file goes away, the older version
of the file goes away.  Forever.  How can LMI distribute a product that
loses data in such a catastrophic manner ?

Files written for output should be put somewhere in /tmp and renamed
upon successfull reception of the CLOSE command.  This is a simple for a
UNIX hacker to implement...

Another bug with the Unix file server is that it does character
translation the wrong way on certain lesser-used characters.  (Peter
Wolf and I can probably tell you more about this bug if we're prodded
enough.  I can't recall all the details about it at the moment.)

Long files written to Angel via Chaosnet tend to get data-corrupted near
the end.  (The usual sympton is lack of character set translation,
though other strange things can happen.)  It's a good thing RMS had a
version of the ZMacs manual in a buffer in DJ a few hours after he wrote
out the file, or else we'd be out a good chunk of a badly-needed and
well-written ZMacs manual.  (And don't forget the frustration factor.)

It is important that Chaosnet work on Angel properly; it can become
LMI's main mail/file machine, and networking node (it's already on the
Internet), if the Chaosnet implementation is followed through.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Friday, 7 June 1985, 19:59-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Your Feature Report of 29 May 1985 15:32-EDT
To: mly@cap
CC: BUG-LISPM@LMI-CAPRICORN
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: The message of 29 May 1985 15:32-EDT from mly@cap
Message-ID: <[LMI-BORIS].6/07/85 19:59:39.RpK>

    Date: Wednesday, 29 May 1985, 15:32-EDT
    From: mly at cap

    In Experimental System 102.84, Experimental Local-File 56.0,
    Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
    Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 762,
    102 16Mb, on Lambda Two:

    Is this a feature?

    >>TRAP 9750 (TRANS-TRAP)
    The function SI:SYMBOLIC-CHAOS-ADDRESS is undefined.
    Backtrace from the debugger:

I can't this to happen in the latest 2.0 (102.145).

Part of the lossage is that Lambda Two, Djinn, and Guinea Pig look for
things in DJ: L;.  For some reason, one of the hackers has deleted all
the patch paraphenilia for 102 and related systems on that tree...


0,, issues, 3, valid,
*** EOOH ***
Date: Thursday, 6 June 1985, 20:20-EDT
From: Michael Travers <MT@lmi-capricorn>
Subject: Edit Flavor disappeared
To: BUG-ZWEI@LMI-Capricorn

In ZWEI in System 102.143, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.19, ZMail 57.7, Tiger 20.5, KERMIT 26.16,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, 102.129 vista portrait,
on Test Lambda A:

There are Edit Flavor Components and other hairy commands, but Edit
Flavor itself seems to have vanished.


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 4 June 1985, 18:15-EDT
From: Michael Travers <MT@lmi-capricorn>
To: BUG-LISPM@LMI-Capricorn

In System 102.143, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.19, ZMail 57.4, Tiger 20.5, KERMIT 26.18,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768,
Official working band for Boris//Natasha -dg, on Boris Badinoff:


Insert your description of the circumstances here:

I got a "no pending CATCH error" and tried to resume it by giving
another tag.  I think the new tag didn't have a catch either.  This
was the result.

>>TRAP 9754 (TRANS-TRAP)
The word #<DTP-TRAP 0> was read from location 2240007 (in CONTROL-TABLES).
Backtrace from the debugger:

(:PROPERTY EH::THROW-TRAP EH::MAKE-UCODE-ERROR-FUNCTION) (P.C. = 54)

 Arg 0 (IGNORE): EH::THROW-TRAP
 Arg 1 (SG): #<DTP-STACK-GROUP SI::MAIN-STACK-GROUP 2240053>
 Arg 2 (IGNORE): (EH::THROW-TRAP)


EH::MAKE-UCODE-ERROR (P.C. = 23)

 Arg 0 (ERROR-NAME): EH::THROW-TRAP
 Arg 1 (SG): #<DTP-STACK-GROUP SI::MAIN-STACK-GROUP 2240053>
 Arg 2 (ETE): (EH::THROW-TRAP)


EH::SIGNAL-MICROCODE-ERROR (P.C. = 224)

 Arg 0 (SG): #<DTP-STACK-GROUP SI::MAIN-STACK-GROUP 2240053>
 Arg 1 (ETE): (EH::THROW-TRAP)
   --Defaulted args:--
 Arg 2 (IGNORE): T
Local 0 (INHIBIT-SCHEDULING-FLAG): T
Local 1 (ERROR-HANDLER-RUNNING): T
Local 2 (ERROR-HANDLER-REPRINT-ERROR): NIL
Local 3: ("Abort from where a microcode error is being signaled.")
Local 4: ((SYSTEM:ABORT ERROR) ("Abort from where a microcode error is being signaled.") T ("Abort from where a microcode error is being signaled.") ...)
Local 5 (SAVED-MICRO-PCS): NIL
Local 6 (ERROR-OBJECT): NIL
Local 7 (I): NIL
Local 8 (PC): NIL
Local 9: NIL
Local 10: NIL
Local 11: NIL
Local 12: NIL
Local 13: NIL
Local 14 (ERROR): NIL
Local 15 (.SELECTQ.ITEM.): NIL
Local 16 (CONDITION-RESULT): NIL


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 4 June 1985, 10:49-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: name of compiler:locblock changed
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 751, R2.0 Macsyma, on Djinn:

(disassemble 'string-search-char)

>>TRAP 7901 (ARGTYP NUMBER M-T 1 QIEQL0)
The second argument to =, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

SI:DESCRIBE-FEF-ADL (P.C. = 332)

 Arg 0 (FEF): #<DTP-FEF-POINTER STRING-SEARCH-CHAR 2457154>
Local 0 (ADL): (388)
Local 1 (OPT-Q): 16438
Local 2 (INIT-OPTION): SYSTEM:FEF-INI-EFF-ADR
Local 3 (ARGNUM): 5
Local 4 (LOCALNUM): 1
Local 5 (ARGP): NIL
Local 6 (ARG-SYNTAX): SYSTEM:FEF-ARG-AUX
Local 7 (LOC): NIL
Local 8 (STR): NIL
Local 9 (.SELECTQ.ITEM.): NIL
Local 10 (SLOT): 4


DISASSEMBLE (P.C. = 101)

 Arg 0 (FUNCTION): STRING-SEARCH-CHAR
Local 0 (FEF): #<DTP-FEF-POINTER STRING-SEARCH-CHAR 2457154>
Local 1 (LIM-PC): NIL
Local 2 (ILEN): NIL
Local 3 (DISASSEMBLE-OBJECT-OUTPUT-FUN): NIL
Local 4 (FUNCTION): #<DTP-FEF-POINTER STRING-SEARCH-CHAR 2457154>
Local 5 (PC): NIL


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 4 June 1985, 10:15-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: xsetcarcdr-instance fukt
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, DOE-Macsyma 9.9,
Macsyma-Help-Database 1.1, microcode 751, R2.0 Macsyma, on Djinn:

(rplacd tv:main-screen 'foo)

>>TRAP 5262 (STACK-FRAME-TOO-LARGE)
Attempt to make a stack frame larger than 256. words.
Backtrace from the debugger:

SI:INSTANCE-HASH-FAILURE (P.C. = 116)

 Arg 0 (OP): :SET-CDR
 Rest arg (ARGS): (FOO FOO FOO FOO ...)
Local 1 (HT): #<EQ-SI:HASH-ARRAY (Funcallable) 36106145>
Local 2 (FN-LOCATION): NIL
Local 3 (FUNC): NIL
Local 4 (NEWHT): NIL
Local 5: NIL


RPLACD:
   Arg 0 (CONS): #<TV:STANDARD-SCREEN Main Screen 3040073 exposed>
   Arg 1 (NEW-CDR): -76775373


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (RPLACD TV:MAIN-SCREEN (QUOTE FOO))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 2
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-U-ENTRY RPLACD 473>
Local 6 (ARG-DESC): 202
Local 7 (NUM-ARGS): 2
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 3 June 1985, 18:43-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
Subject: defstructs in common lisp
To: debbie@LMI-CAPRICORN, BUG-LISPM@LMI-Capricorn


   In common lisp , I defined  the following structure binop-c,

       (defstruct (binop-c (:type list) :named)
                  operator
                  operand-1
                  operand-2)

    Then I tried to use the constructor function , make-binop-c ,

       (make-binop-c :operator '+ :operand-1 'x :operand-2 5)

    And got the following error. This example comes from Steele , p.318.

    ( This works ok in zeta-lisp. In common lisp if all of the arguments to the defstruct
     are given default values, then it will work.)


Oops, I take back the part about working in common lisp if given default values to all
arguments.  I can't get it to work in common lisp at all.


0,, issues, 3, valid,
*** EOOH ***
Date: Friday, 31 May 1985, 12:03-EDT
From: Robert Putnam <robert@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.135, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.19, ZMail 57.5, Tiger 20.5, KERMIT 26.18,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental MICRO-COMPILATION-TOOLS 4.0,
microcode 768, Education 2x2+ Ucomp Idle Oblisp, on Test Lambda D:

Tried to hardcopy unix files on DEBRA from education machine with
si:*default-printer* set to (:TI855 "george").  The files spooled to
GEORGE as expected but could not be printed until user SELECTed Tiger
Operater Window and logged in to DEBRA.

        1. If the Tiger Operator Window on the print server requires the
user to log in to unix, the familiar "Notifications are pending" message
should appear at the bottom of the screen.

        2. Actually, the login message should appear on the terminal of
the user trying to print the file, not the file server terminal.

        3. Why not just dispense with logging in altogether (especially
in cases where the unix protection bits don't limit access, i.e.,
rw-rw-rw)?

robert


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Thursday, 30 May 1985, 16:25-EDT
From: SAM@LMI-LAMBDA-10
To: BUG-LISPM@LMI-CAPRICORN

In System 102.122, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.3, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768,
2.0 Process Systems 5/2/85 Fork., on Lambda Ten:


Insert your description of the circumstances here:

In Zmacs, I tried out "Two Windows Showing Region" [c-X 8] immediately after
c-<space>, therefore handing it a 0-line region.  It doesn't even recover well
from aborting to top level, because it again tries to redisplay the no-length
window.  <system> c-E recovers.

                                Sam Pilato

>>TRAP 5952 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS))
The subscript -1 for #<ART-Q-96 51646117> was out of range in AR-1.
Backtrace from the debugger:

(:METHOD ZWEI:WINDOW :REDISPLAY) (P.C. = 343)
  (SELF is #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 5 3237055 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
 Arg 1 (RECENTER-TYPE): :ABSOLUTE
 Arg 2 (RC1): NIL
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL
Local 0 (LH): 14
Local 1 (NOW): 123016
Local 2 (POINT-PLINE): -1
Local 3 (POINT-LINE): "    nil))"
Local 4 (POINT-INDEX): 0
Local 5 (TOP-LINE): ""
Local 6 (TOP-INDEX): 0
Local 7 (LAST-BP): ("" 0 :MOVES)
Local 8 (INITIAL-DEGREE): 5
Local 9 (POINT-NODE): NIL
Local 10 (START-BP-NODE): NIL
Local 11 (BUF): NIL
Local 12 (NEW-TOP-INDEX): NIL
Local 13 (Y): NIL
Local 14 (LINE): NIL
Local 15 (INDEX): NIL
Local 16 (P): NIL
Local 17 (LINE-LENGTH): NIL
Local 18 (LEN): NIL
Local 19 (DWID): NIL
Local 20 (CH): NIL
Local 21 (FONT): NIL
Local 22 (CWT): NIL
Local 23 (CWID): NIL
Local 24 (RWID): NIL
Local 25 (I): NIL
Local 26 (TW): NIL
Local 27 (L): NIL
Local 28 (FROM-INDEX): NIL
Local 29 (TO-INDEX): NIL
Local 30 (PLINE): NIL
Local 31 (STOP-LINE): NIL
Local 32 (FROB): NIL
Local 33 (PLINE): NIL
Local 34: NIL
Local 35 (BL): NIL


ZWEI::REDISPLAY (P.C. = 56)

 Arg 0 (WINDOW): #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 5 3237055 exposed>
   --Defaulted args:--
 Arg 1 (RECENTER-TYPE): :POINT
 Arg 2 (RC1): NIL
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL


ZWEI::REDISPLAY-ALL-WINDOWS (P.C. = 61)

 Arg 0 (FORCE-TO-COMPLETION-P): NIL
 Arg 1 (SELECT-P): NIL
Local 0: (#<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 5 3237055 exposed> #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 4 3236515 exposed>)
Local 1 (WINDOW): #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 5 3237055 exposed>


(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 288)
  (SELF is #<ZWEI::ZMACS-WINDOW-PANE Zmacs Window Pane 4 3236515 exposed>)

 Arg 0 (.OPERATION.): :EDIT
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
 Arg 2 (*COMTAB*): #<ZWEI::COMTAB ZWEI::MODE-COMTAB 51644001>
 Arg 3 (*MODE-LINE-LIST*): ("ZMACS " "(" ZWEI::*MODE-NAME-LIST* ") " ...)
 Arg 4 (TOP-LEVEL-P): T
Local 0: ("Return to top level editor command loop.")
Local 1: ((SYSTEM:ABORT ERROR) ("Return to top level editor command loop.") T ("Return to top level editor command loop.") ...)
Local 2 (CH): (ZWEI::CONFIGURATION-CHANGED)


(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)

 Rest arg (.DAEMON-CALLER-ARGS.): (:EDIT)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-15 27314175>


Remainder of stack:

FUNCALL (P.C. = 21)
(:METHOD ZWEI::DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI::ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI::PROCESS-TOP-LEVEL (P.C. = 115)



0,, doc.prob, unreproducible, 3, valid,
*** EOOH ***
Date: Thursday, 30 May 1985, 15:21-EDT
From: Dave Goodine <dg@LMI-CAPRICORN>
To: pace@LMI-CAPRICORN, BUG-LISPM@LMI-CAPRICORN
In-reply-to: The message of 19 Mar 1985 10:44-EST from pace@LMI-CAPRICORN

    Date: Tuesday, 19 March 1985, 10:44-EST
    From: pace at LMI-CAPRICORN

    In System 102.94, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
    MagTape 40.16, ZMail 57.1, Experimental LM-Prolog 1.0, Tiger 20.4,
    microcode 758, 102 16Mb, on Lambda Two:


    Insert your description of the circumstances here:

    F  was defined as

    (defun f (random)
      (let-closed ((random random)) (incf random)))

    There is apparently some ambiguity as to in which environment the values of LET-bindings
    should be executed.

    >>TRAP 9750 (TRANS-TRAP)
    The variable RANDOM is unbound.

This behavior is documented in the orangual in the definition of LET-CLOSED.

-dg


0,, issues, 3, valid,
*** EOOH ***
Date: Thursday, 30 May 1985, 15:07-EDT
From: colpitts@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0, microcode 768,
LMI site info, plg, picon, vista, on Customer Service 2:

the following is the code for character in sys: sys2; string

(DEFUN GLOBAL:CHARACTER (X)
  "Convert X to a fixnum representing character if possible.
        This is the same as (CHAR-INT (CLI:CHARACTER X))"
  (COND ((NUMBERP X)
         X)
        ((CHARACTERP X)
         (CHAR-INT X))
        ((AND (STRINGP X) (= (LENGTH X) 1))
         (CHAR-INT (AREF X 0)))
        ((AND (SYMBOLP X) (= (LENGTH (GET-PNAME X)) 1))
         (CHAR-INT (AREF (SYMBOL-NAME X) 0)))
        (T (FERROR NIL "Cannot convert ~S into a character." X))))

shouldn't it be

(DEFUN GLOBAL:CHARACTER (X)
  "Convert X to a fixnum representing character if possible.
        This is the same as (CHAR-INT (CLI:CHARACTER X))"
  (COND ((NUMBERP X)
         X)
        ((CHARACTERP X)
         (CHAR-INT X))
        ( (STRINGP X)
         (CHAR-INT (AREF X 0)))
        ( (SYMBOLP X)
         (CHAR-INT (AREF (SYMBOL-NAME X) 0)))
        (T (FERROR NIL "Cannot convert ~S into a character." X))))


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 30 May 1985, 14:19-EDT
From: robert@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.137, Local-File 56.7, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.19, ZMail 57.6, Tiger 20.6, KERMIT 26.20,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, 102.129 vista,
on Waiting for Godot:

On systems with the medium resolution color board, grey:grey-array and
grey:grey-array0-7 should be bound to screen arrays but are bound to NIL
instead.  (See p. 12 of the Medium Res documentation.) The following form,

      (send grey:grey-screen :tv:screen-array)

can be used in place of grey:grey-array, but there doesn't seem to be any
way to access the eight single-bit arrays.

robert


0,, 3, valid,
*** EOOH ***
Date: Thursday, 30 May 1985, 14:13-EDT
From: colpitts@LMI-CAPRICORN
Sender: @LMI-CAPRICORN
To: BUG-LISPM@LMI-CAPRICORN

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, Experimental LM-Prolog 1.0, microcode 768,
LMI site info, plg, picon, vista, on Customer Service 2:


Insert your description of the circumstances here:

typed
(character 'abc)
this worked in 1.2 and according to the documentation
should work in 2.0

>>ERROR: Cannot convert ABC into a character.
Backtrace from the debugger:

CHARACTER (P.C. = 53)

 Arg 0 (X): ABC


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (CHARACTER (QUOTE ABC))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER CHARACTER 2663232>
Local 6 (ARG-DESC): 65
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (CHARACTER (QUOTE ABC))
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (CHARACTER (QUOTE ABC))
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER) (** #<DTP-FEF-POINTER ** 13044771>) (** #<DTP-FEF-POINTER ** 13045010>) (** #<DTP-FEF-POINTER ** 13045024>) ...)


PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS (P.C. = 142) (from file LMP: KERNEL; TOPLEVEL  )

 Arg 0 (PREDICATION): (CHARACTER (QUOTE ABC))
Local 0: ((SYSTEM:UNDEFINED-FUNCTION) #<DTP-FEF-POINTER (:INTERNAL PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS 0) 13044771>)
Local 1: ((SYSTEM:STACK-FRAME-TOO-LARGE) #<DTP-FEF-POINTER (:INTERNAL PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS 1) 13045010>)
Local 2: ((SYSTEM:UNCLAIMED-MESSAGE) #<DTP-FEF-POINTER (:INTERNAL PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS 2) 13045024>)
Local 3: ((SYSTEM:WRONG-TYPE-ARGUMENT) #<DTP-FEF-POINTER (:INTERNAL PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS 3) 13045045>)
Local 4: ((** #<DTP-FEF-POINTER ** 13044771>) (** #<DTP-FEF-POINTER ** 13045010>) (** #<DTP-FEF-POINTER ** 13045024>) (** #<DTP-FEF-POINTER ** 13045045>) ...)
Local 5 (OLD-WHO-STATE): "Run"
Local 6: NIL
Local 7: #<CLOSURE (:INTERNAL PROLOG:TOP-LEVEL-PROVE-AND-SHOW-RESULTS 4) (Lexical environment) 71767170>


Remainder of stack:

SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
(:INTERNAL PROLOG:EVAL-IN-WINDOW 0) (P.C. = 30)
PROLOG:EVAL-IN-WINDOW (P.C. = 80) (from file LMP: KERNEL; TOPLEVEL  )
SYSTEM:EVAL1 (P.C. = 547)
COND (P.C. = 58)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 54)
SYSTEM:EVAL1 (P.C. = 547)
...
APPLY (P.C. = 24)
SYSTEM:EVAL1 (P.C. = 547)
LET (P.C. = 274)
SYSTEM:EVAL1 (P.C. = 547)
SYSTEM:APPLY-LAMBDA (P.C. = 1307)
SI:LISP-TOP-LEVEL1
SI:LISP-TOP-LEVEL2 (P.C. = 23)
SI:PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Wednesday, 29 May 1985, 15:32-EDT
From: mly@cap
Sender: naha@LMI-ALADDIN
To: BUG-LISPM@LMI-CAPRICORN

In Experimental System 102.84, Experimental Local-File 56.0,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 762,
102 16Mb, on Lambda Two:

Is this a feature?


>>TRAP 9750 (TRANS-TRAP)
The function SI:SYMBOLIC-CHAOS-ADDRESS is undefined.
Backtrace from the debugger:

SI:DECODE-UNIT-ARGUMENT (P.C. = 349)

 Arg 0 (UNIT): "DJ"
 Arg 1 (USE): "reading label"
   --Defaulted args:--
 Arg 2 (CC-DISK-INIT-P): NIL
 Arg 3 (WRITE-P): NIL
Local 0 (TEM): NIL


PRINT-DISK-LABEL (P.C. = 45)

 Arg 0 (UNIT): "DJ"
   --Defaulted args:--
 Arg 1 (STREAM): #:*TERMINAL-IO*-SYN-STREAM
Local 0 (RQB): NIL


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (PRINT-DISK-LABEL "DJ")
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER PRINT-DISK-LABEL 23336042>
Local 6 (ARG-DESC): 131074
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (PRINT-DISK-LABEL "DJ")
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (PRINT-DISK-LABEL "DJ")
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


Remainder of stack:

SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI:LISP-TOP-LEVEL2 (P.C. = 23)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, unreproducible, issues, 3, valid,
*** EOOH ***
Date: Wednesday, 29 May 1985, 11:52-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: [Moon at SCRC-STONY-BROOK: Can't compile closures.]
To: Bug-LispM@Capricorn
Message-ID: <[LMI-DJINN].5/29/85 11:52:33.khs>

This should be on our list too.  (It doesn't work).

Date: Monday, 15 April 1985, 15:44-EST
From: David A. Moon <Moon at SCRC-STONY-BROOK>
Subject: Can't compile closures.
To: Ken Haase <KWH at OZ>, Bill Gosper <rwg at SPA-RUSSIAN>
CC: BUG-LISPM at OZ
In-reply-to: <850412124156.1.KWH@ROBOT-5.MIT>,
             <850413012729.1.RWG@RUSSIAN.SPA.Symbolics.COM>
Message-ID: <850415154422.4.MOON@EUPHRATES.SCRC.Symbolics.COM>

    Date: Friday, 12 April 1985, 12:41-EST
    From: Ken Haase <KWH at OZ>

    (setq foo #'(lambda (x) (+ x 3)))
    (compile 'foo foo)

    errors out in the following way.  It seems that it would be reasonable to
    be able to compile interpreter closures into compiler closures.

    >>Trap: The argument given to the CAR instruction, #<LEXICAL-CLOSURE (LAMBDA ** **) 23211767>, was not a locative, a list, or NIL.

    (:INTERNAL COMPILER:COMPILE-TO-CORE 1 (:DUMP-LAMBDA-EXPRESSION)):  (P.C. = 3)
       Arg 0 (**DEFSELECT-OP**): :DUMP-LAMBDA-EXPRESSION
       Arg 1 (FSPEC): FOO
       Arg 2 (LAMBDA-EXP): #<LEXICAL-CLOSURE (LAMBDA ** **) 23211767>


    Date: Sat, 13 Apr 85 01:27 PST
    From: Bill Gosper <rwg@RUSSIAN.SPA.Symbolics.COM>

    At the binding of GO-P, I'm (spastically) trying to compile a closure referring
    to LINENUM.  How do you actually do this? ....

    >>Error: (FUNCTION (LAMBDA NIL (DECLARE (SYS:DOWNWARD-FUNCTION)) (< LINENUM (SETQ LINENUM MACSYMA:$LINENUM)))) is not a valid form
    While in the function COMPILER:COMPILE-LAMBDA-EXP

Odd that you both came up with this the same day.  Maybe the earth passed through
a cloud of cosmic gas.

This is a reasonable suggestion for a future feature.  I added it to a file
of suggestions I am maintaining.  In the meantime, you need to know that
the second argument to COMPILE is required (and documented) to be a LA




0,, 3, valid, indeterminate,
*** EOOH ***
Date: Friday, 24 May 1985, 16:28-EDT
From: wilde@LMI-CAPRICORN
Sender: @LMI-LAMBDA-3
To: BUG-LISPM@LMI-Capricorn
CC: youcef@LMI-CAPRICORN

In System 102.129, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.19, ZMail 57.4, Tiger 20.5, KERMIT 26.16,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, 102.129 vista portrait,
on Test Lambda C:


Insert your description of the circumstances here:
Process window editor got an error in this sequence of choices:
Vertical Split -> Absolute Size -> Pixels (525) -> Remaining Space

Duplicated this error in Window Maker 3 out of 4 times.

>>TRAP 7723 (ARGTYP NUMBER PP 0 QIADD)
The first argument to +, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

(:METHOD WINDOW-MAKER::PANE :SET-MOUSE-REGION) (P.C. = 32)
  (SELF is #<WINDOW-MAKER::PANE 37722320>)

 Arg 0 (.OPERATION.): :SET-MOUSE-REGION


(:METHOD WINDOW-MAKER::FRAME :SLICE) (P.C. = 574)
  (SELF is #<WINDOW-MAKER::FRAME 37714754>)

 Arg 0 (.OPERATION.): :SLICE
 Arg 1 (DIRECTION): :VERTICAL
   --Defaulted args:--
 Arg 2 (LOWER-LIMIT): 2
 Arg 3 (UPPER-LIMIT): 790
Local 0 (POINT-OF-SLICING1): 523
Local 1 (POINT-OF-SLICING2): NIL
Local 2 (POINT-OF-SLICING): 523
Local 3 (PANE1): #<WINDOW-MAKER::PANE 37722320>
Local 4 (PANE2): #<WINDOW-MAKER::PANE 37722335>
Local 5 (PANE3): NIL
Local 6 (LINE1): #<WINDOW-MAKER::LINE 37722352>
Local 7 (LINE2): NIL
Local 8 (KEY): :ABSOLUTE
Local 9 (KEY1): :PIXELS
Local 10 (KEY2): :EVEN
Local 11 (NUMBER1): NIL
Local 12 (NUMBER2): NIL
Local 13 (LEFT1): NIL
Local 14 (LEFT2): NIL
Local 15 (TOP1): NIL
Local 16 (TOP2): NIL
Local 17 (RIGHT1): NIL
Local 18 (RIGHT2): NIL
Local 19 (BOTTOM1): NIL
Local 20 (BOTTOM2): NIL
Local 21 (LINE): NIL
Local 22 (X): NIL
Local 23 (Y): NIL
Local 24 (Z): NIL
Local 25 (S): NIL


WINDOW-MAKER::WINDOW-EDITOR (P.C. = 121)

Local 0: ("aborting computation")
Local 1: ((SYSTEM:ABORT ERROR) ("aborting computation") T ("aborting computation") ...)
Local 2 (BLIP): (:TYPEOUT-EXECUTE :VERTICAL-SPLIT #<WINDOW-MAKER::FRAME 37714754>)
Local 3 (OBJECT): #<WINDOW-MAKER::FRAME 37714754>
Local 4 (.SELECTQ.ITEM.): :VERTICAL-SPLIT


WINDOW-MAKER::PROCESS-FUNCTION (P.C. = 188)

 Arg 0 (WINDOW): #<WINDOW-MAKER::WINDOW-MAKER-FRAME Window Maker Frame 3 1736714 deactivated>
Local 0 (X): 2
Local 1 (Y): 2
Local 2 (Z): 790
Local 3 (S): 883


SI::PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 66)

 Arg 0 (RESTART-ON-RESET): T
 Arg 1 (FUNCTION): WINDOW-MAKER::PROCESS-FUNCTION
 Rest arg (ARGS): (#<WINDOW-MAKER::WINDOW-MAKER-FRAME Window Maker Frame 3 1736714 deactivated>)
Local 1: ("Terminate and free process ~A." "window editor")
Local 2: ((SYSTEM:ABORT ERROR) ("Terminate and free process ~A." "window editor") T ("Terminate and free process ~A." "window editor") ...)


Remainder of stack:

SI::PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
From: colpitts
Date: Friday, 24 May 1985, 11:21-EDT
To: bug-lispm

>From daemon Mon May 20 16:00:16 1985
Status: R

>From hoey@nrl-aic.ARPA Mon May 20 15:44:38 1985 remote from MIT-CCC
Received: from MIT-MC by MIT-CCC via Chaosnet; 20 May 85 15:44-EST
Received: from nrl-aic by MIT-MC.ARPA; 20 May 85 15:43:56 EST
Date: 20 May 1985 15:06:01 EDT (Mon)
From: Dan Hoey <hoey@nrl-aic.ARPA>
Subject: READ screws up on one-line windows
To: "lmi-capricorn!bug-cs%mit-ccc"@mit-mc
Message-Id: <485463961/hoey@nrl-aic>

In Rel2 Beta

Read, readline, etc. have bugs associated with wraparound on one line
windows.  For a quick example, go to a lisp listener and move the mouse
to the middle of the screen on a line with some text (like the current
input line) and type

        (defvar *foo* nil)
        (tv:choose-variable-values '((*foo*)))

Mouse left on NIL, then enter keys

        ( C-U C-U C-U a b b b

You should see funny displays, including a funky sort of half-cursor at
the right margin.  Move the cursor around typing C-L and see if you can
find your left parenthesis any more.  But it gets better, continuing

        C-E C-U C-U C-U RUBOUT

And it draws garbage outside the menu.

Slight variations of this technique can create an error break inside
the menu.

Sounds like TV:STREAM-MIXIN still needs work.

Dan




0,, valid, 2, indeterminate,
*** EOOH ***
Date: Thursday, 23 May 1985, 11:37-EDT
From: debbie@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI site info,
on Customer Service 1:

MCC has reported two problems using read-delimited-string and the rubout-handler when they
give the rubout-handler the :preemptable option . They type a string of characters
and then hit a mouse button , which stuffs all the input into a buffer.

The first is that (send x :untyi (send x :tyi)) gets an error saying "attempt to unget
something different from the last element gotten from IO-BUFFER..."
(x is an instance of tv:window) .

The second is that (send x :any-tyi-no-hang) will return nil , whereas (send x :listen)
would return T, and (send x :any-tyi) will return the first character they typed earlier.

My uneducated guess is that   the characters are in the rubout-handler-buffer, tyi looks
at the rubout-handler-buffer, but untyi and any-tyi-no-hang dont look there.??

Here is the code they used:
(defflavor trash
        ()
        (tv:window)
        :settable-instance-variables)

(defmethod (trash :read-input) (&optional (input-string ""))
        (read-delimited-string '(#return #/end) self nil
        (list '(:preemptable nil)
        (list ':initial-input input-string)
        (list ':initial-input-index (string-length input-string)))))

(setq x (make-instance 'trash :edges-from ':mouse))
(progn (send x :select)(send x :read-input))

Then type in a string and hit a mouse click to preempt the read-input method.
At this point the above problems will be reproduceable.


0,, issues, 3, valid,
*** EOOH ***
Date: Friday, 17 May 1985, 23:31-EDT
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi site info,
on Lambda Five A:

more on eh:*inhibit-debugger-proceed-prompt*...and *mode-lock-means-clear-screen*.

The source file for eh:command-loop is in a totally random state.
In fact, from what I can tell it does not appear to really be the
source that is loaded on this system.

As for

(defconst *mode-lock-means-clear-screen* t
  "Clear the screen before printing info (on ^N, ^P and entering error-handler
   if the [MODE-LOCK] key is depressed.")

, did I mention that I vote NO for this feature?  (If I were for anything,
I would be for having one behavior for Return/Line and the other for
Ctrl-N/Ctrl-P. Presently, they are the same, respectively.)

Anyway, *mode-lock-means-clear-screen* does not have a top level binding
in this system, but it is in the source, which is one more reason why I
shall refrain from hacking the source.

-mhd


0,, 3, valid,
*** EOOH ***
Date: Thursday, 16 May 1985, 14:08-EDT
From: LH@LMI-LAMBDA-3
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, lmi site info,
on Lambda Five A:


Insert your description of the circumstances here:

Did a restart-program. This should cause the old serial
stream to be thrown away, but it doesn't do this. Then this happens any
time you try to connect again.

Perhaps there shouldn't be a restart-program since it's kind
of a hold-over from the days when there could only be one kermit
window in the world.  Now you can simply do <system> ctrl-k.

-mhd


>>ERROR: The argument CHAN was NIL, which is not an array (a Unibus channel).
Backtrace from the debugger:

SI:UNIBUS-CHANNEL-NOT-EMPTY (P.C. = 44)

 Arg 0 (CHAN): NIL


(:METHOD SI:SDU-SERIAL-STREAM-MIXIN :LISTEN) (P.C. = 28)
  (SELF is #<SI:SDU-SERIAL-STREAM 65142532>)

 Arg 0 (.OPERATION.): :LISTEN


KERMIT:PROCESS-WAIT-LISTEN (P.C. = 26)

 Rest arg (STREAMS): (#<SI:SDU-SERIAL-STREAM 65142532> #<KERMIT:KERMIT-TERMINAL-PANE Kermit Terminal Pane 1 1734450 exposed>)
Local 1 (STREAM1): #<SI:SDU-SERIAL-STREAM 65142532>
Local 2 (RETURN-VALUE): NIL


KERMIT:CONNECT (P.C. = 59)

 Arg 0 (*SERIAL-STREAM*): #<SI:SDU-SERIAL-STREAM 65142532>
 Arg 1 (*TERMINAL*): #<KERMIT:KERMIT-TERMINAL-PANE Kermit Terminal Pane 1 1734450 exposed>
Local 0 (RETURN-VALUE): NIL
Local 1 (CHAR-ALUF): 6
Local 2 (WINNER): NIL


(:METHOD KERMIT:KTERM-STATE :MAKE-CONNECTION) (P.C. = 21)
  (SELF is #<KERMIT:KTERM-STATE 65431314>)

 Arg 0 (.OPERATION.): :MAKE-CONNECTION
 Arg 1 (SERIAL-STREAM): #<SI:SDU-SERIAL-STREAM 65142532>
 Arg 2 (TERMINAL-STREAM): #<KERMIT:KERMIT-TERMINAL-PANE Kermit Terminal Pane 1 1734450 exposed>


Remainder of stack:

KERMIT:MAKE-CONNECTION (P.C. = 189)
(:METHOD TV:MENU-EXECUTE-MIXIN :EXECUTE) (P.C. = 106)
(:INTERNAL (:METHOD KERMIT:KERMIT-COMMAND-PANE :COMBINED :EXECUTE) 0) (P.C. = 33)
(:METHOD KERMIT:KERMIT-COMMAND-PANE :AROUND :EXECUTE) (P.C. = 41)
(:METHOD KERMIT:KERMIT-COMMAND-PANE :COMBINED :EXECUTE) (P.C. = 37)
(:METHOD KERMIT:KERMIT-FRAME :TOP-LEVEL) (P.C. = 211)
KERMIT:KERMIT-INITIAL-FUNCTION (P.C. = 21)
KERMIT:RUN-KERMIT-PROCESS (P.C. = 22)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
From: colpitts
Date: Tuesday, 14 May 1985, 12:34-EDT
To: bug-lispm

you don't get a warning from the garbage collector when you've
used up half of memory.
(102.117 ucode 768)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Wednesday, 8 May 1985, 19:43-EDT
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: DIREDing Angel
To: BUG-LISPM@LMI-Capricorn

In System 102.116, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 768, on Natasha Nogoodnik:


Insert your description of the circumstances here:

I got this error while doing Meta-X Dired of ANGEL:/lmi/.
I've also had other directory access problems.  Trying to Dired one of
my subdirectories on Angel gives a file not found error.  Also, trying
to fs:balance-directories a Lambda directory and an Angel directory also
gives a file not found error. What gives?

>>TRAP 9754 (TRANS-TRAP)
The function NIL is undefined.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 6 values

NIL:
   Arg 0: :SORT-COMPONENTS


(:METHOD PATHNAME :SORT-LESSP) (P.C. = 36)
  (SELF is #FS:UNIX-PATHNAME "ANGEL: //lmi//dest")

 Arg 0 (.OPERATION.): :SORT-LESSP
 Arg 1 (OTHER-PATHNAME): NIL
Local 0 (TEM): NIL
Local 1 (OHOST): NIL
Local 2 (ODEVICE): NIL
Local 3 (ODIRECTORY): NIL
Local 4 (ONAME): NIL
Local 5 (OTYPE): NIL
Local 6 (OVERSION): NIL


FS:PATHNAME-LESSP (P.C. = 19)

 Arg 0 (PATHNAME-1): #FS:UNIX-PATHNAME "ANGEL: //lmi//dest"
 Arg 1 (PATHNAME-2): NIL


SI:SORT-LIST-MERGE (P.C. = 54)

 Arg 0 (L1): ((NIL :PATHNAME #FS:UNIX-PATHNAME "ANGEL: //lmi//*" :|//lmi//colpitts| ...))
 Arg 1 (L2): ((#FS:UNIX-PATHNAME "ANGEL: //lmi//dest" :PHYSICAL-VOLUME-FREE-BLOCKS NIL :BLOCK-SIZE ...))
Local 0 (R): NIL
Local 1 (P): #<DTP-LOCATIVE 4041570>
Local 2 (LAST1): ((NIL :PATHNAME #FS:UNIX-PATHNAME "ANGEL: //lmi//*" :|//lmi//colpitts| ...))
Local 3 (LENGTH1): 0
Local 4 (LAST2): ((#FS:UNIX-PATHNAME "ANGEL: //lmi//dest" :PHYSICAL-VOLUME-FREE-BLOCKS NIL :BLOCK-SIZE ...))
Local 5 (LENGTH2): 0
Local 6 (HIGH1): (NIL :PATHNAME #FS:UNIX-PATHNAME "ANGEL: //lmi//*" :|//lmi//colpitts| ...)
Local 7 (HIGH2): (#FS:UNIX-PATHNAME "ANGEL: //lmi//dest" :PHYSICAL-VOLUME-FREE-BLOCKS NIL :BLOCK-SIZE ...)


SI:SORT-LIST-PREFIX (P.C. = 56)

 Arg 0 (HEIGHT): 1
Local 0 (LENGTH): NIL
Local 1 (LAST): NIL


Remainder of stack:

SI:SORT-LIST-PREFIX (P.C. = 53)
SI:SORT-LIST (P.C. = 31)
SORT (P.C. = 69)
SORTCAR (P.C. = 25)
(:METHOD FS:DIRECTORY-STREAM-ACCESS-MIXIN :DIRECTORY-LIST) (P.C. = 96)
(:METHOD FS:FILE-HOST-MIXIN :ACCESS-OPERATION) (P.C. = 25)
(:METHOD FS:HOST-PATHNAME :DIRECTORY-LIST) (P.C. = 29)
FS:DIRECTORY-LIST (P.C. = 32)
ZWEI:DIRECTORY-EDIT-REVERT (P.C. = 192)
ZWEI:DIRECTORY-EDIT (P.C. = 134)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
Date: Wednesday, 8 May 1985, 16:23-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Bugs in ZMail
To: dg@LMI-CAPRICORN
CC: jmturn@LMI-CAPRICORN, bug-zmail@LMI-CAPRICORN
In-reply-to: The message of 8 May 1985 11:06-EDT from dg@LMI-CAPRICORN
Message-ID: <[LMI-BORIS].5/08/85 16:23:32.RpK>

    Date: Wednesday, 8 May 1985, 11:06-EDT
    From: Dave Goodine <dg at LMI-CAPRICORN>

    In zmail in System 102.116, Local-File 56.6, FILE-Server 13.1,
    Unix-Interface 5.3, MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
    MEDIUM-RESOLUTION-COLOR 17.3, microcode 768, on Boris Badinoff:

    There are several problems with ZMail:

    1) Using the Move command makes CR's get output to the typeout window
       under (over) the message box...

This doesn't happen to me.

    2) Saving your profile makes a question come up in the same typeout window
       rather than  in the minibuffer area... probably a silly oversight.

Noted...

    3) In PROFILE, setting the keywords associated with a mail file loses...
       forget the exact circumstances, it should be obvious.

This is an easily reproducable bug, true.

    4) There several bugs and stupidities in the Profile code... using it
       enough will make them obvious (the exposure problem with the filter
       definition window, etc...).

Yep.  Some of this has to do with the hairiness of the window system.
In the current system, however, things should still be usable.

    Suggestion:

    When I click right on select, the presentation of each buffer's full file name
    (especially unix filenames) is pretty gross.  Maybe we could do
    something neat with the documentation line like have the buffer menu items
    just be the file name (w/o directory and host), and have its mouse documentation
    be the full filename.  Therefore we can find the full pathname if necessary,
    withough having to look at it all the time.  It could even be a ZMail option
    if people really love to make big mouse boxes appear.

This issue has been addressed; first, there is a profile option so that
you can tell ZMail where most of your mail files appear.  I beleive this
variable is documented in the Release 2 Notes.  This option must be
explicitly set by the user.  When this is set, buffers will appear in
certain places to be denoted only by the name of the associated file
(not the directory, extension, etc.).  This has been done in the Select
menu for quite a while.  (Note that if a mail buffer is not yet actually
loaded, the full name will still appear.)  Second, while I was fixing
the who-line documentation bug, I made the Select and Move item
documentation updaters use this new feature.  Another fix was to make
filenames be PRINC'ed instead of PRINTed -- why would anyone need to see
the name slashified ?  And yes, it did look really ugly with Unix
pathnames.


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 7 May 1985, 13:11-EDT
From: robert@LMI-CAPRICORN
Sender: BRQ@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.127, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.17, ZMail 57.3, Tiger 20.5, KERMIT 26.16,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental MICRO-COMPILATION-TOOLS 4.0,
microcode 768, Education 2x2+ 102.120 Idle UCompiler,
on Test Lambda A:


Insert your description of the circumstances here:

Tried

   (fs:set-host-working-directory  (si:parse-host "george") "robert;")


>>ERROR: Attempt to print NIL, which is not a valid component.
Backtrace from the debugger:

FS::LM-PRINT-COMPONENT (P.C. = 97)

 Arg 0 (SPEC): NIL
 Arg 1 (STREAM): #<CLOSURE FORMAT::FORMAT-STRING-STREAM 1 41202765>
   --Defaulted args:--
 Arg 2 (VERSION-P): NIL
Local 0 (TEM): NIL
Local 1 (I): NIL
Local 2: NIL


FS::LM-PRINT-DIRECTORY (P.C. = 68)

 Arg 0 (DEVICE): "DSK"
 Arg 1 (DIRECTORY): ("ROBERT" NIL)
 Arg 2 (S): #<CLOSURE FORMAT::FORMAT-STRING-STREAM 1 41202765>
 Arg 3 (SPACE): T
Local 0 (D): (NIL)


FS::LM-NAMESTRING (P.C. = 38)

 Arg 0 (HOST): #FS::LISPM-HOST "LMI-LAMBDA-CS1"
 Arg 1 (DEVICE): "DSK"
 Arg 2 (DIRECTORY): ("ROBERT" NIL)
 Arg 3 (NAME): :UNSPECIFIC
 Arg 4 (TYPE): :UNSPECIFIC
 Arg 5 (VERSION): :UNSPECIFIC
Local 0 (S): #<CLOSURE FORMAT::FORMAT-STRING-STREAM 1 41202765>


(:METHOD FS::LM-PARSING-MIXIN :STRING-FOR-PRINTING) (P.C. = 37)
  (SELF is #FS::LM-PATHNAME ...error printing #<FS::LM-PATHNAME 3466163>...)

 Arg 0 (.OPERATION.): :STRING-FOR-PRINTING


(:METHOD FS::LM-PATHNAME :COMBINED :STRING-FOR-PRINTING) (P.C. = 43)
  (SELF is #FS::LM-PATHNAME ...error printing #<FS::LM-PATHNAME 3466163>...)

 Rest arg (.DAEMON-CALLER-ARGS.): (:STRING-FOR-PRINTING)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-8 22615605>


Remainder of stack:

(:METHOD PATHNAME :PRINT-SELF) (P.C. = 50)
SI:PRINT-OBJECT (P.C. = 276)
PRIN1 (P.C. = 50)
SI:LISP-TOP-LEVEL1 (P.C. = 304)
SI::LISP-TOP-LEVEL2 (P.C. = 23)
SI::PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 7 May 1985, 13:08-EDT
From: mhd@LMI-CAPRICORN
Sender: @LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.109, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 4/15/85 Fork., on Lambda Five B:

Soon after running hacks:qix (which uses hacks:with-real-time), the screen
flashes. Is this the sdu thinking that the processor has crashed?

-mhd


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Sunday, 5 May 1985, 02:01-EDT
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Reposition Window
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 751,
on Djinn:

c-m-R can leave your function off the screen if there is a huge comment
block preceding it.

Ken.


0,, valid, 2,
*** EOOH ***
Date: Friday, 3 May 1985, 10:03-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-ZMAIL@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI site info,
on Customer Service 1:


Insert your description of the circumstances here:
I was trying to print out mail messages from zmail. I tried both Meta-x Hardcopy Msg and
Meta-x Hardcopy All. Both of them did this:

>>ERROR: The argument FLAVOR-NAME was NIL, which is not the name of an instantiable flavor, or alias thereof.
Backtrace from the debugger:

INSTANTIATE-FLAVOR (P.C. = 96)

 Arg 0 (FLAVOR-NAME): NIL
 Arg 1 (INIT-PLIST): #<DTP-LOCATIVE 4063307>
 Arg 2 (SEND-INIT-MESSAGE-P): T
   --Defaulted args:--
 Arg 3 (RETURN-UNHANDLED-KEYWORDS-P): NIL
 Arg 4 (AREA-TO-CONS-INSTANCE-IN): NIL
Local 0 (FL): NIL
Local 1 (UNHANDLED-KEYWORDS): NIL
Local 2 (INSTANCE): NIL
Local 3 (VARS): NIL
Local 4 (NEW-PLIST): NIL
Local 5 (TEM): NIL
Local 6 (MAP-RESULT): NIL
Local 7 (MAP-TEMP): NIL
Local 8 (.MAP-LOCAL-0.): NIL
Local 9 (KEYWORD): NIL
Local 10 (MISSING-KEYWORDS): NIL
Local 11 (V): NIL
Local 12 (I): NIL
Local 13 (VAR-KEYWORDS): NIL
Local 14 (REMAINING-KEYWORDS): NIL
Local 15 (PL): NIL
Local 16 (INDEX): NIL
Local 17: NIL
Local 18 (D): NIL


MAKE-INSTANCE (P.C. = 20)

 Arg 0 (FLAVOR-NAME): NIL
 Rest arg (INIT-OPTIONS): NIL


ZWEI:MAKE-HARDCOPY-ZMAIL-BUFFER (P.C. = 32)

 Arg 0 (CHOOSE-OPTIONS-P): NIL
 Arg 1 (FOR-WHOLE-FILE-P): T
 Arg 2 (NEAR-MODE): (:MOUSE)


ZWEI:COM-ZMAIL-HARDCOPY-ALL (P.C. = 52)



ZWEI:COM-ZMAIL-EXTENDED-COMMAND (P.C. = 82)

Local 0 (.LOCKED-P.): T
Local 1 (ANS): ("Hardcopy All" . ZWEI:COM-ZMAIL-HARDCOPY-ALL)


Remainder of stack:

ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:METHOD ZWEI:ZMAIL-FRAME :PROCESS-COMMAND-CHAR) (P.C. = 32)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 177)
(:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI:ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, valid, 4, cosmetic,
*** EOOH ***
From: robert
Date: Wednesday, 1 May 1985, 12:37-EDT
To: bug-lispm

Under 102.95 (i.e., with the Symbolics version number patch
loaded), if an extension is not specified, the load command will
not find files on S machines with .LISP extensions.  That is, you
lose with

     (load "s:mydir;glond")

and win with

     (load "s:mydir;glond.lisp")

Shouldn't the "look for .QFASL, then .LISP" rule be applied for
Symbolics files?

robert


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 30 April 1985, 15:56-EDT
From: debbie@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI site info,
on Customer Service 1:

MCC reported a bug where using read-delimited-string and giving it options to pass to
the rubout-handler, leaves the cursor at the  beginning of the string, instead of the end.
This happens when passing the options :initial-input and :initial-input-index to the
rubout handler. They say that this works on symbolics (i.e. leaves cursor at end of string),
and works ok in 1.2 - although that was using read-line instead of read-delimited-string.




0,, issues, 3, valid,
*** EOOH ***
Date: Tuesday, 30 April 1985, 11:43-EDT
From: Dave Goodine <dg@LMI-CAPRICORN>
To: BUG-ZWEI@LMI-Capricorn

In ZWEI in System 102.117, Local-File 56.6, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, on Boris Badinoff:

It seems that when I do:

^X^F CAP:/lmi/dg/ZMail.in

alter the file, save it and then do M-X Load,

the default file to load becomes

        CAP:/lmi/dg/ZMail.'

This is bizarre....

-dg


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 29 April 1985, 20:52-EDT
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Problem with copy-file to unix host
To: mitccc!PETTENGILL%MCC.ARPA%MIT-MC@cap
CC: bug-lispm@LMI-CAPRICORN, bug-unix@LMI-CAPRICORN
In-reply-to: The message of 29 Apr 1985 18:57-EDT from PETTENGILL@MCC.ARPA
Message-ID: <[LMI-EARNEST].4/29/85 20:52:24.RpK>

    Date: Monday, 29 April 1985, 18:57-EDT
    From: PETTENGILL at MCC.ARPA

    In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
    MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
    MEDIUM-RESOLUTION-COLOR 17.3, Experimental MCC 3.5, microcode 761,
     Release 2.0 - MCC Site (New Host Tables)(chaos ucode enabled)bold fonts,
    on Rob's friend Zonker:

    Insert your description of the circumstances here:

    I was trying to copy a file from my lmfs to my local unix host -
    (copy-file "local:..." "j:...")
    If I try exactly the same operation to a vax unix host running the
    symbolics chaos software there is no problem

This is somewhat puzzling.  There has been a bug (seen at MIT) where the
Lisp Machine was sending an OPEN option which was defined at the Lisp
level, but it was not supposed to be sent to the file server if it was
the default for the direction.  The command string for the file opening
here should definitely not send any options with IF-EXISTS or
IF-DOES-NOT-EXIST in this case:

    >>ERROR: Unknown OPEN option. for J: HOME; ZMAIL INIT 6
    Backtrace from the debugger:

    FS:FILE-PROCESS-ERROR (P.C. = 89)

     Arg 0 (CONDITION-NAME): FS:UNIMPLEMENTED-OPTION
     Arg 1 (ERROR-STRING): "Unknown OPEN option. for J: HOME; ZMAIL INIT 6"
     Arg 2 (PATHNAME-OR-STREAM): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"

    ... from OPEN-CHAOS ...
--> Local 20 (IF-EXISTS): :ERROR           ; these are the default
--> Local 21 (IF-DOES-NOT-EXIST): :CREATE  ; actions for output

I tried using COPY-FILE here to a Nu machine, and I did not get an error
-- I think that would rule out the bug of sending these OPEN options.
Perhaps you need a new version of the Unix file server.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 29 April 1985, 18:57-EDT
From: PETTENGILL@MCC.ARPA
Sender: ROB@MCC.ARPA
Subject: [PETTENGILL at MCC.ARPA: Problem with copy-file to unix host]
To: lmi-cap!bug-lispm%mit-ccc%mit-mc@mcc

>From ROB@MCC.ARPA Mon Apr 29 17:52:05 1985
Received: by mcc-bell with CHAOS id AA20303; Mon, 29 Apr 85 17:50:25 cdt
Date: Monday, 29 April 1985, 17:49-CDT
From: <PETTENGILL@MCC.ARPA>
Sender: ROB@MCC.ARPA
Subject: Problem with copy-file to unix host
To: BUG-LISPM@MCC-BELL

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental MCC 3.5, microcode 761,
 Release 2.0 - MCC Site (New Host Tables)(chaos ucode enabled)bold fonts,
on Rob's friend Zonker:


Insert your description of the circumstances here:

I was trying to copy a file from my lmfs to my local unix host -
(copy-file "local:..." "j:...")
If I try exactly the same operation to a vax unix host running the
symbolics chaos software there is no problem

>>ERROR: Unknown OPEN option. for J: HOME; ZMAIL INIT 6
Backtrace from the debugger:

FS:FILE-PROCESS-ERROR (P.C. = 89)

 Arg 0 (CONDITION-NAME): FS:UNIMPLEMENTED-OPTION
 Arg 1 (ERROR-STRING): "Unknown OPEN option. for J: HOME; ZMAIL INIT 6"
 Arg 2 (PATHNAME-OR-STREAM): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
 Arg 3 (PROCEEDABLE): NIL
 Arg 4 (NOERROR): NIL
 Rest arg (MAKE-CONDITION-ARGS): (:OPEN)
Local 1 (WHO-FOR): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
Local 2 (CONDITION): #FS:FILE-OPERATION-FAILURE :PROPERTY-LIST (:PATHNAME #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6" :OPERATION :OPEN) :CONDITION-NAMES (FS:FILE-OPERATION-FAILURE FS:FILE-ERROR ERROR CONDITION ...) :FORMAT-STRING "~A" :FORMAT-ARGS ("U

nknown OPEN option. for J: HOME; ZMAIL INIT 6") :PATHNAME #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6" :OPERATION :OPEN


FS:QFILE-PROCESS-ERROR-NEW (P.C. = 129)

 Arg 0 (STRING): "O1507 ERROR UOO C Unknown OPEN option."
 Arg 1 (PATHNAME-OR-STREAM): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
 Arg 2 (PROCEEDABLE): NIL
 Arg 3 (NOERROR): NIL
 Rest arg (MAKE-CONDITION-ARGS): (:OPEN)
Local 1 (S-P): 17
Local 2 (ERROR-CODE): "UOO"
Local 3 (ERROR-SEVERITY): "C"
Local 4 (ERROR-STRING): "Unknown OPEN option."
Local 5 (WHO-FOR): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
Local 6 (DEFAULT-CONS-AREA): 33


FS:OPEN-CHAOS (P.C. = 800)

 Arg 0 (ACCESS): #<FS:QFILE-ACCESS Chaos FILE access to JOANIE 62116057>
 Arg 1 (FILE): #FS:UNIX-PATHNAME "JOANIE: //usr//mcc//cad//rcp//zmail.in"
 Arg 2 (PATHNAME): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
 Rest arg (OPTIONS): (:DIRECTION :OUTPUT :CHARACTERS T ...)
Local 1 (DIRECTION): :OUTPUT
Local 2 (CHARACTERS): T
Local 3 (ERROR): T
Local 4: (NIL)
Local 5: (NIL)
Local 6: (NIL)
Local 7: (NIL)
Local 8 (TEMPORARY): NIL
Local 9 (DELETED): NIL
Local 10 (RAW): NIL
Local 11 (SUPER-IMAGE): NIL
Local 12 (BYTE-SIZE): :DEFAULT
Local 13 (PRESERVE-DATES): NIL
Local 14 (INHIBIT-LINKS): NIL
Local 15 (SUBMIT): NIL
Local 16 (ESTIMATED-LENGTH): NIL
Local 17 (ELEMENT-TYPE-P): NIL
Local 18 (ACCESS-ERROR): NIL
Local 19 (ELEMENT-TYPE): STRING-CHAR
Local 20 (IF-EXISTS): :ERROR
Local 21 (IF-DOES-NOT-EXIST): :CREATE
Local 22 (HOST-UNIT): #<FS:QFILE-HOST-UNIT for Chaos FILE access to JOANIE 62116062>
Local 23 (DATA-CONN): (#<CHAOS Connection 62032400> "I1506" "O1507" 3619203 ...)
Local 24 (PKT): #<CHAOS packet :STRING "Rob's friend Zonker
Local 25 (SUCCESS): NIL
Local 26 (STRING): "O1507 ERROR UOO C Unknown OPEN option."
Local 27 (NOT-ABORTED): T
Local 28 (PHONY-CHARACTERS): NIL
Local 29 (SIGN-EXTEND-BYTES): NIL
Local 30 (IF-EXISTS-P): T
Local 31 (.SELECTQ.ITEM.): 6
Local 32 (ERROR-OBJECT): (#<CHAOS Connection 62032400> "I1506" "O1507" 3619203 ...)
Local 33: ((FS:FILE-NOT-FOUND SI:CONDITION-CASE-THROW FS:G8325) (FS:DIRECTORY-NOT-FOUND #<DTP-STACK-CLOSURE 1154400>) (** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))
Local 34: (FS:FILE-NOT-FOUND SI:CONDITION-CASE-THROW FS:G8325)
Local 35: (FS:FILE-ALREADY-EXISTS SI:CONDITION-CASE-THROW FS:G8336)
Local 36: ((FS:FILE-ALREADY-EXISTS SI:CONDITION-CASE-THROW FS:G8336) (FS:DIRECTORY-NOT-FOUND #<DTP-STACK-CLOSURE 1154400>) (** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))
Local 37 (PROPERTIES): NIL
Local 38 (ERROR-OBJECT): NIL
Local 39 (IGNORE): NIL


(:METHOD FS:QFILE-ACCESS :OPEN) (P.C. = 26)
  (SELF is #<FS:QFILE-ACCESS Chaos FILE access to JOANIE 62116057>)

 Arg 0 (.OPERATION.): :OPEN
 Arg 1 (FILE): #FS:UNIX-PATHNAME "JOANIE: //usr//mcc//cad//rcp//zmail.in"
 Arg 2 (PATHNAME): #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6"
 Rest arg (OPTIONS): (:DIRECTION :OUTPUT :CHARACTERS T ...)


(:METHOD FS:FILE-HOST-MIXIN :ACCESS-OPERATION) (P.C. = 25)
  (SELF is #FS:UNIX-HOST "JOANIE")

 Arg 0 (.OPERATION.): :ACCESS-OPERATION
 Arg 1 (OP): :OPEN
 Rest arg (ARGS): (#FS:UNIX-PATHNAME "JOANIE: //usr//mcc//cad//rcp//zmail.in" #FS:LOGICAL-PATHNAME "J: HOME; ZMAIL INIT 6" :DIRECTION :OUTPUT ...)


Remainder of stack:

(:METHOD FS:HOST-PATHNAME :OPEN) (P.C. = 31)
FS:LOGICAL-PATHNAME-PASS-ON (P.C. = 22)
OPEN (P.C. = 153)
FS:PRIMITIVE-COPY-FILE (P.C. = 419)
(:METHOD PATHNAME :WILDCARD-MAP) (P.C. = 104)
COPY-FILE (P.C. = 150)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
SI:LISP-TOP-LEVEL2 (P.C. = 23)
SI:PROCESS-TOP-LEVEL (P.C. = 115)
SI:LISP-TOP-LEVEL (P.C. = 39)




0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 29 April 1985, 12:25-EDT
From: Debbie Ellerin <debbie@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.117, Local-File 56.6, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental Sited 1.0,
Experimental window-maker 1.0, microcode 768, LMI site info,
on Customer Service 1:

In 2.0 , unix host filenames which include a "~" are not correctly parsed.
This works in 1.2, where something like  "unix-a:~debbie//a.out"
                     is the same as      "unix-a://usr//debbie//a.out"



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Friday, 26 April 1985, 18:20-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: lmi - symbolics pathname problems
To: mitccc!Donc%ISI-VAXA%MIT-MC@lmi-capricorn
CC: bug-lispm@cap
In-reply-to: <8504260048.AA09837@isi-vaxa.ARPA>
Message-ID: <[LMI-LAMBDA-6].4/26/85 18:20:56.RpK>

    Date: Thursday, 25 April 1985, 19:48-EST
    From: Donc at ISI-VAXA

    If I do (load file),where the file is a symbolics pathname
    without an extension, the result is a complaint that load could
    not find any file related to a filename that looks like the same
    file with an unspecific extension.  In fact, there is a .lisp file
    (but currently no qfasl), which I'd like it to find.

Hmm.  If that fails, then the following situation should also fail:

  1. There is a file called FOO:>bar>baz.lisp
  2. If you enter the editor by typing (ed "foo:>bar>baz"), it should
     not find the file, and instead say (New File) in the echo area.

Other possible lossages:

  1. Perhaps the Symbolics file server does not like the .newest version
     spec in the file opening string.  Try loading the file with a
     specific version number, or try using the READFILE function, which
     only uses text files, or FASLOAD, which only looks for QFASLs.

  2. Your site information is wrong.  I doubt this, but you should
     check...

Also, you should include the version of software when you send us bug
reports.   I assume you are running at least version 102.95 of System.
The current version here at LMI is 102.118, although there are not any
changes that could affect LMFS pathnames.


0,, 3, valid, indeterminate,
*** EOOH ***
Message-ID: <8504260048.AA09837@isi-vaxa.ARPA>
Date: Thursday, 25 April 1985, 19:48-EST
To: LMI-CAPRICORN!BUG-LISPM%MITCCC@MIT-MC
From: Donc@ISI-VAXA
Subject: lmi - symbolics pathname problems

I got the tape that you (George Colpitts) sent, and it solves the
problem we were having with unspecific version numbers.  Now I've
run into something that looks very similar.  If I do (load file),
where the file is a symbolics pathname without an extension, the
result is a complaint that load could not find any file related
to a filename that looks like the same file with an unspecific
extension.  In fact, there is a .lisp file (but currently no
qfasl), which I'd like it to find.  The idea is that I should be
able to use this same command on either a lambda or 3600 - on the
lambda it should get a .qfasl if there is one, and otherwise look
for a .lisp, while the 3600 will look for a .bin and then a .lisp.

Please let me know that you got this msg.  You were going to send
me the patches over the arpanet, but I never got them.  Thanks.



0,, valid, 2,
*** EOOH ***
Date: Thursday, 25 April 1985, 14:16-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.109, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.5, KERMIT 26.15,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 4/15/85 Fork., on Lambda Five A:


Insert your description of the circumstances here:

Another example of logging-in-while-transferring-a-file-causes
an-error-in-that-xfer. To recreate: 1) login, 2) c-x, c-f a big file into
zmacs, 3) quickly switch to lisp and log in. You should then get a notification
of an error in zmacs similar to the one below.

If logging in is going to take forever (doing who knows what...), it
might as well lock up and wait till it's safe.

-mhd


>>ERROR: Attempt to receive from #<CHAOS Connection 30246515>,
which got a LOS: No such index exists
Backtrace from the debugger:

CHAOS:GET-NEXT-PKT (P.C. = 87)

 Arg 0 (CONN): #<CHAOS Connection 30246515>
 Arg 1 (NO-HANG-P): NIL
 Arg 2 (WHOSTATE): "File Input"
   --Defaulted args:--
 Arg 3 (CHECK-CONN-STATE): T
Local 0 (PKT): NIL


FS:QFILE-NEXT-READ-PKT (P.C. = 87)

 Arg 0 (NO-HANG-P): NIL
 Arg 1 (FOR-SYNC-MARK-P): NIL
Local 0 (.SELECTQ.ITEM.): :OPEN
Local 1 (PKT): NIL
Local 2 (.SELECTQ.ITEM.): NIL


(:METHOD FS:QFILE-INPUT-STREAM-MIXIN :GET-NEXT-INPUT-PKT) (P.C. = 31)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; TEXT.LISP#>" 61731756>)

 Arg 0 (.OPERATION.): :GET-NEXT-INPUT-PKT
 Arg 1 (NO-HANG-P): NIL
Local 0: NIL


Additional information supplied with call:
 Expecting 3 values

(:METHOD CHAOS:CHARACTER-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (P.C. = 24)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; TEXT.LISP#>" 61731756>)

 Arg 0 (.OPERATION.): :NEXT-INPUT-BUFFER
 Arg 1 (NO-HANG-P): NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD SI:BASIC-BUFFERED-INPUT-STREAM :SETUP-NEXT-INPUT-BUFFER) (P.C. = 35)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; TEXT.LISP#>" 61731756>)

 Arg 0 (.OPERATION.): :SETUP-NEXT-INPUT-BUFFER
   --Defaulted args:--
 Arg 1 (NO-HANG-P): NIL


Remainder of stack:

(:METHOD FS:QFILE-INPUT-CHARACTER-STREAM :COMBINED :SETUP-NEXT-INPUT-BUFFER) (P.C. = 39)
(:METHOD SI:BUFFERED-LINE-INPUT-STREAM :LINE-IN) (P.C. = 40)
ZWEI:SECTIONIZE-FILE-BUFFER (P.C. = 259)
ZWEI:REVERT-FILE-BUFFER (P.C. = 501)
(:METHOD ZWEI:ZMACS-BUFFER :REVERT) (P.C. = 36)
ZWEI:REVERT-BUFFER (P.C. = 54)
ZWEI:FIND-FILE (P.C. = 168)
ZWEI:COM-FIND-FILE (P.C. = 39)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid,
*** EOOH ***
Date: Tuesday, 23 April 1985, 19:11-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Updating Displays in Peek
To: rjpi@LMI-CAPRICORN
CC: BUG-LISPM@LMI-Capricorn
FCC: CAP: /lmi/rpk/Mail/cc.bb
In-reply-to: The message of 5 Apr 1985 20:02-EST from rjpi@LMI-CAPRICORN
Message-ID: <[LAMBDA-TEST-C].4/23/85 19:11:20.RpK>

    Date: Friday, 5 April 1985, 20:02-EST
    From: rjpi at LMI-CAPRICORN

    In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
    MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
    MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
    2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Fifteen:

    I just killed a process in the [Active Processes] display in Peek and a
    window in the [Window Hierarchy] display.  What seems to be a bug is
    that normally killing a window or process will cause it to be removed
    from the display.  In this case they weren't removed.  What's happened?

This might be a bug in Peek via the scroll-window updater stuff.  I have
a fairly trivial use of peek in the file access code (it is the default
peek method for accesses when they don't define their own), and have run
across a similar problem.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Wednesday, 17 April 1985, 10:51-EST
From: GJC@LMI-EXPLORER-2
To: BUG-LISPM@lam3

In Experimental HAL 1.100, Experimental Error Handler 1.9,
Experimental Font Editor 1.1, Experimental ZMACS 1.32,
Experimental Universal Command Loop 6.44, Experimental Compiler 2.21,
Experimental ZMail 1.14, Experimental Suggestions 1.52,
Experimental Local-File 5.31, Experimental Explorer Streamer Tape 5.48,
Experimental Glossary 2.7, Experimental VT100 1.7,
Experimental Explorer Serial & Parallel Ports 1.6,
Experimental PRINTER 1.3, Experimental Window System 1.44,
Experimental Utilities 1.23, Experimental Input Editor 1.13,
Experimental HAL-A 1.6, Experimental Net-Config 4.11,
Experimental Formatter 2.13, Experimental Grasper 2.6,
Experimental Graphics Window 2.10, Experimental Graphics Editor 2.11,
Experimental Tree Drawing Utility 2.2, Experimental NLMenu 2.21,
Experimental Relational Table Management System 1.0,
Experimental NLMenu-RTMS-Interface 4.8, Experimental PROLOG 1.6,
Experimental User Profile Utility 3.0, microcode 186, Hal1.100atoolkit,
on LMI-EXPLORER-2:


Insert your description of the circumstances here:

Compilation got errors and asked if I wanted to continue, and
I answered No. (LMI's make-system does not do this (ask).)

>>TRAP 6084 (THROW-TRAP)
There was no pending *CATCH for the tag SI::EXIT-MAKE-SYSTEM.
The value being thrown was NIL.Backtrace from the debugger:

SI::QC-FILE-1 (P.C. = 143)

 Arg 0 (INFILE): #FS::LOGICAL-PATHNAME "MACSYMA-SOURCE: MAXSRC; MTRACE LISP >"
 Arg 1 (OUTFILE): #FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: MAXSRC; MTRACE XFASL >"
 Local 0 (ARGS): (#FS::LOGICAL-PATHNAME "MACSYMA-SOURCE: MAXSRC; MTRACE LISP >" :OUTPUT-FILE #FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: MAXSRC; MTRACE XFASL" :PACKAGE ...)
 Local 1 (OUTFILE): #FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: MAXSRC; MTRACE XFASL 2"
 Local 2 (ERROR-CODE): 20


SI::DO-FILE-TRANSFORMATIONS (P.C. = 253)

 Local 0: ((:PENDING # MACSYMA #<SI::SYSTEM DOE-Macsyma 65004633> ...) (:PENDING # MACSYMA #<SI::SYSTEM DOE-Macsyma 65004633> ...) (:PENDING # MACSYMA #<SI::SYSTEM DOE-Macsyma 65004633> ...) (:PENDING # MACSYMA #<SI::SYSTEM DOE-Macsyma 65004633> ...) ...)
 Local 1 (FILE-TRANSFORMATION): (:PENDING (:COMPILE # SI::QC-FILE-1 # ...) MACSYMA #<SI::SYSTEM DOE-Macsyma 65004633> ...)
 Local 2 (STATE): :PENDING
 Local 3 (TYPE): (:COMPILE ("Compile" "Compiling" "compiled") SI::QC-FILE-1 (#) ...)
 Local 4 (ARGS): (#FS::LOGICAL-PATHNAME "MACSYMA-SOURCE: MAXSRC; MTRACE LISP >" #FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: MAXSRC; MTRACE XFASL >")
 Local 5: ("Give up on all the ~(~A~)." "Compiling")
 Local 6: (GLOBAL:ERROR ("Give up on all the ~(~A~)." "Compiling") T ("Give up on all the ~(~A~)." "Compiling") ...)
 Local 7: ("Retry all the ~(~A~)." "Compiling")
 Local 8: (GLOBAL:ERROR ("Retry all the ~(~A~)." "Compiling") T ("Retry all the ~(~A~)." "Compiling") ...)
 Local 9 (IGNORE): NIL
 Local 10 (PATHNAME): #FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: CFFK; CPOLY XFASL >"
 Local 11 (FILE-XFORM): (:PENDING (SI::INCREMENT-COMPILED-VERSION # SI::INCREMENT-COMPILED-VERSION-1 NIL ...) NIL #<SI::SYSTEM DOE-Macsyma 65004633> ...)
 Local 12 (L): (#FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: RAT; POLYRZ XFASL >")
 Local 13 (TAIL): (#FS::LOGICAL-PATHNAME "MACSYMA-OBJECT: RAT; POLYRZ XFASL >")


SI::PERFORM-TRANSFORMATIONS (P.C. = 122)

 Arg 0 (TRANSFORMATION-LIST): ((#<TRANSFORMATION COMPILE 65015016> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015034> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015052> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015070> MACSYMA NIL) ...)
 Local 0 (ELEM): (#<TRANSFORMATION INCREMENT-COMPILED-VERSION 65015464> MACSYMA NIL)
 Local 1: NIL
 Local 2 (XFORM): #<TRANSFORMATION INCREMENT-COMPILED-VERSION 65015464>
 Local 3 (PKG): MACSYMA
 Local 4 (FORCE): NIL
 Local 5 (INPUT): NIL
 Local 6: #<DTP-LOCATIVE 475042>
 Local 7: NIL
 Local 8 (INPUTS): NIL


SI::PERFORM-TRANSFORMATIONS (P.C. = 77)

 Arg 0 (TRANSFORMATION-LIST): ((#<TRANSFORMATION FASLOAD 65015025> MACSYMA NIL) (#<TRANSFORMATION FASLOAD 65015043> MACSYMA NIL) (#<TRANSFORMATION FASLOAD 65015061> MACSYMA NIL) (#<TRANSFORMATION FASLOAD 65015077> MACSYMA NIL) ...)
 Local 0 (ELEM): (#<TRANSFORMATION INCREMENT-LOADED-VERSION 65015473> MACSYMA NIL)
 Local 1: NIL
 Local 2 (XFORM): #<TRANSFORMATION INCREMENT-LOADED-VERSION 65015473>
 Local 3 (PKG): MACSYMA
 Local 4 (FORCE): NIL
 Local 5 (INPUT): #<TRANSFORMATION INCREMENT-COMPILED-VERSION 65015464>
 Local 6: ((#<TRANSFORMATION INCREMENT-COMPILED-VERSION 65015464> MACSYMA NIL))
 Local 7: ((#<TRANSFORMATION COMPILE 65015016> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015034> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015052> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015070> MACSYMA NIL) ...)
 Local 8 (INPUTS): ((#<TRANSFORMATION COMPILE 65015016> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015034> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015052> MACSYMA NIL) (#<TRANSFORMATION COMPILE 65015070> MACSYMA NIL) ...)


MAKE-SYSTEM (P.C. = 401)

 Arg 0 (SYSTEM): DOE-MACSYMA
 Rest arg (KEYWORDS): (:COMPILE)
 Local 1: NIL
 Local 2: T
 Local 3 (KEYWORD): :COMPILE
 Local 4 (FUNCTION): #<DTP-FEF-POINTER (:PROPERTY :COMPILE SI::MAKE-SYSTEM-KEYWORD) 13676221>
 Local 5 (TARGET): NIL
 Local 6 (FORM): NIL


Remainder of stack:

SI::EVAL1 (P.C. = 1266)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 44)
(:METHOD UCL::TOP-LEVEL-FUNCTIONS :EXECUTE) (P.C. = 103)
(:METHOD UCL::BASIC-COMMAND-LOOP :EXECUTE-COMMAND) (P.C. = 62)
(:METHOD TV:LISP-LISTENER :COMBINED :EXECUTE-COMMAND) (P.C. = 55)
(:METHOD UCL::BASIC-COMMAND-LOOP :HANDLE-TYPEIN-INPUT) (P.C. = 540)
(:METHOD UCL::BASIC-COMMAND-LOOP :HANDLE-KEY-INPUT) (P.C. = 116)
(:METHOD UCL::BASIC-COMMAND-LOOP :FETCH-AND-EXECUTE) (P.C. = 117)
(:METHOD TV:LISP-LISTENER :LOOP) (P.C. = 152)
(:METHOD UCL::COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 75)
SI::PROCESS-TOP-LEVEL (P.C. = 244)
SI:LISP-TOP-LEVEL (P.C. = 47)


0,, 3, valid,
*** EOOH ***
Date: Saturday, 13 April 1985, 14:34-EST
From: Michael Travers <MT@lmi-capricorn>
Subject: error report for wrong type arg
To: BUG-LISPM@LMI-Capricorn

In System 102.107, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Fifteen:

(= 0 nil) => The first arg to =, NIL, was of the wrong type
(= nil 0) => The second arg to =, NIL, was of the wrong type

(+ 0 nil) => The second arg to +, NIL, was of the wrong type (correct)
(+ nil 0) => The second arg to +, NIL, was of the wrong type




0,, unreproducible, 3, valid,
*** EOOH ***
From: robert
Date: Friday, 12 April 1985, 13:44-EST
To: bug-lispm

>From daemon Thu Apr 11 20:30:24 1985
Status: R

>From keira@Think Thu Apr 11 20:27:29 1985 remote from MIT-CCC
Received: from MIT-MC by MIT-CCC via Chaosnet; 11 Apr 85 20:27-EST
Received: from Think.ARPA by MIT-MC.ARPA; 11 APR 85 15:37:55 EST
Received: by Think.ARPA id AA12080; Thu, 11 Apr 85 15:35:06 est
To: lmi-capricorn!robert%mitccc@mit-mc
Cc: keira@Think
Subject: bugs
Date: 11 Apr 85 15:35:00 EST (Thu)
From: Keira Bromberg <keira@Think>

From: Bill St. Clair <Bill@AQUINAS>
Subject: BUG in LET-CLOSED due to improper scoping of SPECIAL declaration.
To: BUG-LAMBDA@GODOT

In LAMBDA in System 102.95, Local-File 56.0, FILE-Server 13.1,
Unix-Interface 5.3, MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, Zeta-C 3.4, Experimental CDL 11.0,
microcode 753, patches 93-95 loaded 2 April 1985,
on Mr. Wilbur Wright:

    I found this bug while building a big system that uses closures for
SYSTEM 2. It worked fine in the old system. I have reduced the problem
to three simple examples to illustrate.

(defun foo (x)
  (let-closed ((x x)) #'(lambda (y) (+ x y))))

(defun bar (x)
  (let-closed ((z x)) #'(lambda (y) (+ z y))))

(defun baz (x)
  (declare (special x))
  (let-closed ((x x)) #'(lambda (y) (+ x y))))


The following error is generated when FOO is compiled. BAR & BAZ compile
without error.

        << While compiling FOO >>
         The variable X is bound but never used.

FOO also gets a fatal error (traceback below) when invoked. BAR & BAZ
work correctly.  The problem is in the scoping of SPECIAL declarations
over the binding list of the LET-CLOSED:

        (let-closed ((x x)) #'(lambda (y) (+ x y))))

expands into:

        (LET ((X x))         ;I inserted CASE difference here for reference
          (DECLARE (SPECIAL X))
          (CLOSURE (QUOTE (X))
                   (PROGN (FUNCTION (LAMBDA (Y) (+ X Y))))))

The second "x" in the binding list of the LET should refer to the
lexically bound parameter "x" to the function FOO. The variable "X" in
the binding list, should be SPECIAL as declared in the body of the LET.
Instead, the SPECIAL declaration is scoping the entire binding list.
This causes the compiler to complain that the lexically bound parameter
"x" to FOO is bound but never used. It also causes an error when FOO is
invoked because "x" is not bound in the global environment (were there a
global value for "x", that value would be the one used).  The error
works the same whether FOO is compiled or interpreted. I have
temprorarily avoided this problem by insuring that my code is of the
form of BAZ. I wish I didn't have to do this.  Thank you for your
attention.

-- Bill

--------------------------------------------------------------------------

The following is the error generated when (FOO 3) is evaluated:

>>TRAP 23025 (TRANS-TRAP)
The variable X is unbound.
Backtrace from the debugger:

FOO (P.C. = 21)

 Arg 0 (X): 3


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (FOO 3)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER FOO 13211612>
Local 6 (ARG-DESC): 101
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 73)

 Arg 0 (FORM): (FOO 3)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (FOO 3)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


BREAK (P.C. = 437)

 Arg 0 (FORMAT-STRING): "ZMACS."
 Rest arg (ARGS): NIL
Local 1 (SAVED-BUFFER): NIL
Local 2 (SAVED-BUFFER-POSITION): NIL
Local 3: NIL
Local 4 (LAST-TIME-READTABLE): #<READTABLE standard Zetalisp 30240210>
Local 5 (CHAR): 50
Local 6 (THROW-FLAG): T
Local 7: ("Return to BREAK ~?" "ZMACS." NIL)
Local 8: ((SYSTEM:ABORT ERROR) ("Return to BREAK ~?" "ZMACS." NIL) T ("Return to BREAK ~?" "ZMACS." NIL) ...)
Local 9: #<DTP-FEF-POINTER (:INTERNAL BREAK SI:.DO.IT.) 2632372>
Local 10: #:*TERMINAL-IO*-SYN-STREAM
Local 11 (TEM1): (FOO 3)
Local 12 (TEM): NIL
Local 13 (VALUES): NIL
Local 14 (VALUE): NIL


Remainder of stack:

ZWEI:COM-BREAK (P.C. = 36)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)







0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 8 April 1985, 01:39-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Ten:


Insert your description of the circumstances here:

Hey, do we support Ztop mode.  I think it is very useful.  This
just happened randomly, as far as I can tell.

CORRECTION:

I think this happened the first time a line was longer than the width of the
window and had to wrap around.



>>ERROR: The negative array length -58 is illegal.
Backtrace from the debugger:

SI:SIMPLE-MAKE-ARRAY (P.C. = 156)

 Arg 0 (DIMENSIONS): -58
 Arg 1 (TYPE): ART-STRING
   --Defaulted args:--
 Arg 2 (AREA): NIL
 Arg 3 (LEADER-LENGTH): NIL
 Arg 4 (INITIAL-ELEMENT): NIL
Local 0 (INITIAL-ELEMENT-P): NIL
Local 1 (DATA-LENGTH): NIL
Local 2 (LONG-ARRAY-P): NIL
Local 3 (ARRAY): NIL
Local 4: NIL
Local 5 (ENTRIES-PER-Q): NIL
Local 6 (HEADER-WORD): NIL


ZWEI:INDENT-TO (P.C. = 111)

 Arg 0 (BP): ("c-M  Mail a bug report.   Entering the editor..." 49 :NORMAL)
 Arg 1 (GOAL): 400
   --Defaulted args:--
 Arg 2 (SHEET): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 3201000 exposed>
Local 0 (SPACES): NIL
Local 1 (N): -58
Local 2 (M): 0
Local 3 (BPI): 864
Local 4 (SW): 8
Local 5 (TW): 64
Local 6 (TAB): NIL
Local 7 (SPACE): NIL
Local 8 (I): NIL
Local 9 (J): NIL


(:METHOD ZWEI:EDITOR-STREAM-MIXIN :INCREMENT-CURSORPOS) (P.C. = 120)
  (SELF is #<ZWEI:ZTOP-STREAM-FROM-WINDOW 3230264>)

 Arg 0 (.OPERATION.): :INCREMENT-CURSORPOS
 Arg 1 (DX): 320
 Arg 2 (DY): 0
 Arg 3 (UNITS): :CHARACTER
Local 0 (STARTING-INDENTATION): 80


FORMAT:FORMAT-CTL-TAB (P.C. = 114)

 Arg 0 (PARAMS): (50)
Local 0 (DEST): 50
Local 1 (EXTRA): 1
Local 2 (OPS): (:*STREAM-COMMAND-POINT* :*STREAM-INPUT-HISTORY* :*STREAM-SHEET* :*STREAM-START-BP* ...)
Local 3 (INCR-OK): (:INCREMENT-CURSORPOS :INIT :INSERT-CHAR :INSERT-LINE ...)
Local 4 (FLAVOR): :CHARACTER
Local 5 (X): 10
Local 6 (Y): 61
Local 7 (NEW-X): 50
Local 8 (I): NIL
Local 9: NIL


FORMAT:FORMAT-CTL-OP (P.C. = 51)

 Arg 0 (OP): T
 Arg 1 (ARGS): NIL
 Arg 2 (PARAMS): (50)
Local 0 (TEM): FORMAT:FORMAT-CTL-TAB


Remainder of stack:

FORMAT:FORMAT-CTL-STRING (P.C. = 94)
FORMAT (P.C. = 146)
FS:FS-COPY-FILE (P.C. = 285)
FS:COPY-DIRECTORY (P.C. = 457)
SYSTEM:EVAL1 (P.C. = 547)
PROGN (P.C. = 49)
SYSTEM:EVAL1 (P.C. = 547)
SI:EVAL-SPECIAL-OK (P.C. = 73)
SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)
SI:LISP-TOP-LEVEL1 (P.C. = 272)
ZWEI:ZTOP-TOP-LEVEL (P.C. = 39)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 5 April 1985, 13:38-EST
From: rjpi@cap
Sender: ml@LMI-LAMBDA-3
Subject: Altmode doesn't seem to work in Change Font Region
To: BUG-ZWEI@LMI-Capricorn

In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Fifteen:


Insert your description of the circumstances here:

        I typed CTRL-X CTRL-J and then <ALT MODE> to enter the name of a
font.  I entered TR12B, and received this error message.

>>ERROR: Cannot convert ("TR12B" . #<FONT TR12B 30364016>) into a string.
Backtrace from the debugger:

STRING (P.C. = 74)

 Arg 0 (X): ("TR12B" . #<FONT TR12B 30364016>)


STRING-EQUAL (P.C. = 76)

 Arg 0 (STRING1): ("TR12B" . #<FONT TR12B 30364016>)
 Arg 1 (STRING2): "TR12"
 Rest arg (ARGS): NIL
Local 1 (IDX1): 0
Local 2 (IDX2): 0
Local 3 (LIM1): NIL
Local 4 (LIM2): NIL
Local 5: NIL


Additional information supplied with call:
 Expecting 2 values

FIND (P.C. = 185)

 Arg 0 (ITEM): ("TR12B" . #<FONT TR12B 30364016>)
 Arg 1 (SEQUENCE): (("TR12" . #<FONT TR12 30362171>) ("TR12B" . #<FONT TR12B 30364016>) ("TR12I" . #<FONT TR12I 30366043>) ("HL12" . #<FONT HL12 30340622>) ...)
 Rest arg (KEYARGS): (:TEST STRING-EQUAL :KEY CAR)
Local 1 (FROM-END): NIL
Local 2 (TEST): STRING-EQUAL
Local 3 (TEST-NOT): NIL
Local 4 (START): 0
Local 5 (END): NIL
Local 6 (KEY): CAR
Local 7 (ONE-ARG-PREDICATE): NIL
Local 8 (INDEX): (("TR12" . #<FONT TR12 30362171>) ("TR12B" . #<FONT TR12B 30364016>) ("TR12I" . #<FONT TR12I 30366043>) ("HL12" . #<FONT HL12 30340622>) ...)
Local 9 (I): 0
Local 10 (STOP-INDEX): NIL
Local 11 (LAST-ELT): NIL
Local 12 (LAST-POS): NIL


POSITION (P.C. = 23)

 Arg 0 (ITEM): ("TR12B" . #<FONT TR12B 30364016>)
 Arg 1 (SEQUENCE): (("TR12" . #<FONT TR12 30362171>) ("TR12B" . #<FONT TR12B 30364016>) ("TR12I" . #<FONT TR12I 30366043>) ("HL12" . #<FONT HL12 30340622>) ...)
 Rest arg (KEYARGS): (:TEST STRING-EQUAL :KEY CAR)
Local 1 (POS): NIL


ZWEI:INPUT-FONT-NAME-FROM-MINI-BUFFER (P.C. = 76)

Local 0 (FONT): ("TR12B" . #<FONT TR12B 30364016>)
Local 1 (NEW-P): NIL


Remainder of stack:

ZWEI:INPUT-FONT-NAME (P.C. = 233)
ZWEI:COM-CHANGE-FONT-REGION (P.C. = 67)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 4 April 1985, 16:43-EST
From: Debbie Ellerin <debbie@LMI-UNIX-CS>
To: BUG-LISPM@lmi-capricorn

In System 1.129, Local-File 1.9, FILE-Server 1.2, Tiger 6.38,
MagTape 32.15, Unix-Interface 12.7, MEDIUM-RESOLUTION-COLOR 6.4,
KERMIT 6.0, LMI-System 1.5, Experimental LM-Prolog 2.0, microcode 154,
3/28/85 rpp prolog, on Customer Service 1:

I ran a function which heavily exercised nested calls to with-file-open-case, where unix files
are being opened. This hangs . I tried to use process-sleep to give the with-file-open-cases
enough time to close properly before the next connection was made. This ran for a little
longer before hanging , but not much. Any ideas?
The code looks like this:
(defun loop-in-with-open-file-case ()
  (do-forever
    (with-open-file-case (unixfile1 "debra://etc//passwd")
      (fs:file-operation-failure (print 'error-in-loop-1))
      (:no-error (with-open-file-case (unixf2 "debra://etc//sysnames")
                   (fs:file-operation-failure (print 'err-in-loop2))
                   (:no-error (print 'inside-inner-loop)))))))



0,, issues, 3, valid,
*** EOOH ***
Date: Thursday, 4 April 1985, 11:23-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.101, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Fifteen:


Insert your description of the circumstances here:


You always get this or a similar (chaosnet) error in the process with an open
file when you login in another process.  It seems the process where
you're logging in should wait or get an error instead.


- mhd


>>ERROR: Attempt to receive from #<CHAOS Connection 31205065>,
which got a LOS: No such index exists
Backtrace from the debugger:

CHAOS:GET-NEXT-PKT (P.C. = 87)

 Arg 0 (CONN): #<CHAOS Connection 31205065>
 Arg 1 (NO-HANG-P): NIL
 Arg 2 (WHOSTATE): "File Input"
   --Defaulted args:--
 Arg 3 (CHECK-CONN-STATE): T
Local 0 (PKT): NIL


FS:QFILE-NEXT-READ-PKT (P.C. = 87)

 Arg 0 (NO-HANG-P): NIL
 Arg 1 (FOR-SYNC-MARK-P): NIL
Local 0 (.SELECTQ.ITEM.): :OPEN
Local 1 (PKT): NIL
Local 2 (.SELECTQ.ITEM.): NIL


(:METHOD FS:QFILE-INPUT-STREAM-MIXIN :GET-NEXT-INPUT-PKT) (P.C. = 31)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; AI-WINDOW.LISP#>" 47324321>)

 Arg 0 (.OPERATION.): :GET-NEXT-INPUT-PKT
 Arg 1 (NO-HANG-P): NIL
Local 0: NIL


Additional information supplied with call:
 Expecting 3 values

(:METHOD CHAOS:CHARACTER-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (P.C. = 24)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; AI-WINDOW.LISP#>" 47324321>)

 Arg 0 (.OPERATION.): :NEXT-INPUT-BUFFER
 Arg 1 (NO-HANG-P): NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD SI:BASIC-BUFFERED-INPUT-STREAM :SETUP-NEXT-INPUT-BUFFER) (P.C. = 35)
  (SELF is #<FS:QFILE-INPUT-CHARACTER-STREAM "LAM10: PIC; AI-WINDOW.LISP#>" 47324321>)

 Arg 0 (.OPERATION.): :SETUP-NEXT-INPUT-BUFFER
   --Defaulted args:--
 Arg 1 (NO-HANG-P): NIL


Remainder of stack:

(:METHOD FS:QFILE-INPUT-CHARACTER-STREAM :COMBINED :SETUP-NEXT-INPUT-BUFFER) (P.C. = 39)
(:METHOD SI:BUFFERED-LINE-INPUT-STREAM :LINE-IN) (P.C. = 40)
ZWEI:SECTIONIZE-FILE-BUFFER (P.C. = 259)
ZWEI:REVERT-FILE-BUFFER (P.C. = 501)
(:METHOD ZWEI:ZMACS-BUFFER :REVERT) (P.C. = 36)
ZWEI:REVERT-BUFFER (P.C. = 54)
ZWEI:FIND-FILE (P.C. = 168)
ZWEI:COM-FIND-FILE (P.C. = 39)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:MAKE-EXTENDED-COMMAND-INTERNAL (P.C. = 58)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Wednesday, 3 April 1985, 13:58-EST
From: Debbie Ellerin <debbie@LMI-LAMBDA-3>
To: BUG-LISPM@lmi-capricorn

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental MICRO-COMPILATION-TOOLS 3.0,
Experimental LM-Prolog 1.0, microcode 762, 2.0c site plg,
on Customer Service 1:


Insert your description of the circumstances here:

While in a font editor window in the top half of the screen, I tried to display the font
5X5. If its worth anything, in this case *MORE* processing was turned off.
>>TRAP 7922 (ARGTYP NUMBER M-T 1 QILSP0)
The second argument to <, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

FED:DISPLAY-FONT (P.C. = 242)

 Arg 0 (FONT): #<FONT 5X5 30267111>
 Arg 1 (WINDOW): #<FED:FED-TYPEOUT-WINDOW Fed Typeout Window 5 7401470 exposed>
 Arg 2 (CLEAR-FIRST-P): T
 Arg 3 (FROM-FED): T
Local 0 (FONT-MAP): #<ART-Q-26 62635437>
Local 1 (CURRENT-FONT): #<FONT CPTFONT 30242471>
Local 2 (NAME): FONTS:5X5
Local 3 (FD): #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 128 :FD-NAME ...)
Local 4 (DF): #<FONT CPTFONT 30242471>
Local 5 (CH): 0
Local 6 (OCH): NIL
Local 7 (LEN): 128
Local 8 (CH1): 0


FED:COM-DISPLAY-FONT (P.C. = 72)

   --Defaulted args:--
 Arg 0 (FONT): #<FONT 5X5 30267111>
 Arg 1 (WINDOW): #<FED:FED-TYPEOUT-WINDOW Fed Typeout Window 5 7401470 exposed>
 Arg 2 (FROM-FED): T
 Arg 3 (CLEAR-FIRST-P): T


FED:SELECT-FONT (P.C. = 103)

 Arg 0 (NEW-FONT): FONTS:5X5
Local 0 (TEM): NIL


(:METHOD FED :SELECT-FONT) (P.C. = 44)
  (SELF is #<FED Fed 5 7401325 exposed>)

 Arg 0 (.OPERATION.): :SELECT-FONT
 Arg 1 (NEW-FONT): FONTS:5X5


(:METHOD FED :COMMAND-LOOP) (P.C. = 123)
  (SELF is #<FED Fed 5 7401325 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
Local 0 (PROMPT-LINE-WAS-USED): NIL
Local 1 (COMMAND): NIL
Local 2 (NEXTCH): (:TYPEOUT-EXECUTE :SELECT-FONT FONTS:5X5)


Remainder of stack:

SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Wednesday, 3 April 1985, 11:57-EST
From: Mark Nahabedian <custer@LMI-CAPRICORN>
Sender: naha@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.100, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 21.0, Experimental NewDraw 17.1,
microcode 762, Landscape ND fixed, on Lambda Twelve:

In lisp listener 1 I did

(with-open-file (str  "sdu-serial-a:" ':direction  ':output)
        (format str "hello wendy."))

because wendy and I were trying to figure out why we couldn't talk to port A.
Instead of writing to port A it wrote to the file lam3:naha.burn-in;utils.lisp.
I'm sure glad the old useful version was still around.  Note the following:

        (fs:parse-pathname "sdu-serial-b:")
        #SI:SHARED-DEVICE-PATHNAME "SDU-SERIAL-B:"
        NIL

        (fs:parse-pathname "sdu-serial-a:")
        #FS:LM-PATHNAME "LAM3: ~; SDU-SERIAL-A:"
        NIL

I suggest that if we refuse to make port A work that the minimum acceptable
behavior is to signal an error saying that you can't use port A.  Clobbering
a completely random file is perhaps the definitive pessimal behavior.

        -naha


0,, issues, 3, valid,
*** EOOH ***
Date: Wednesday, 3 April 1985, 10:28-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.101, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.13,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Five B:


Insert your description of the circumstances here:

Tried to open a second serial-stream on port b in kermit frame 2.
Kermit frame 1 owned the lock. But lock state didn't go away even
after I went and kill kermit frame 1 (with the system menu).  Also,
why doesn't it signal an error when two processes try to use port
b, just like when two processors in a 2x2 try to use it?

--mhd

>>Keyboard break.
Backtrace from the debugger:

PROCESS-WAIT (P.C. = 36)

 Arg 0 (WHOSTATE): "SDU-SERIAL-B locked"
 Arg 1 (FUNCTION): #<DTP-FEF-POINTER (:INTERNAL PROCESS-LOCK 1) 23432175>
 Rest arg (ARGUMENTS): (#<SI:PROCESS Kermit Frame 1 62340265> #<DTP-LOCATIVE 37432525>)
Local 1 (STATE): 7


PROCESS-LOCK (P.C. = 83)

 Arg 0 (LOCATIVE-POINTER): #<DTP-LOCATIVE 37432525>
 Arg 1 (LOCK-VALUE): #<SI:PROCESS Kermit Frame 2 62516366>
 Arg 2 (WHOSTATE): "SDU-SERIAL-B locked"
   --Defaulted args:--
 Arg 3 (TIMEOUT): NIL
Local 0 (LOCKER): #<SI:PROCESS Kermit Frame 1 62340265>


(:METHOD SI:BASIC-SHARED-DEVICE :GET-LOCK) (P.C. = 53)
  (SELF is #SI:SDU-SERIAL-B-SHARED-DEVICE "SDU-SERIAL-B" "allocated by slot 4.")

 Arg 0 (.OPERATION.): :GET-LOCK
   --Defaulted args:--
 Arg 1 (WAIT-IF-NECESSARY): T


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

(:METHOD SI:SHARED-DEVICE :OPEN) (P.C. = 50)
  (SELF is #SI:SDU-SERIAL-B-SHARED-DEVICE "SDU-SERIAL-B" "allocated by slot 4.")

 Arg 0 (.OPERATION.): :OPEN
 Arg 1 (FLAVOR-AND-INIT-OPTIONS): (SI:SDU-SERIAL-STREAM)
 Arg 2 (SHARED-DEVICE-PATHNAME): #SI:SHARED-DEVICE-PATHNAME "SDU-SERIAL-B:"
Local 0 (FLAVOR): SI:SDU-SERIAL-STREAM
Local 1 (INIT-OPTIONS): (:SHARED-DEVICE #SI:SHARED-DEVICE-PATHNAME "SDU-SERIAL-B:")


(:METHOD SI:SDU-SERIAL-B-SHARED-DEVICE :COMBINED :OPEN) (P.C. = 37)
  (SELF is #SI:SDU-SERIAL-B-SHARED-DEVICE "SDU-SERIAL-B" "allocated by slot 4.")

 Rest arg (.DAEMON-CALLER-ARGS.): (:OPEN NIL #SI:SHARED-DEVICE-PATHNAME "SDU-SERIAL-B:")
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-5 22672032>


Remainder of stack:

(:METHOD SI:SHARED-DEVICE-PATHNAME :OPEN) (P.C. = 41)
OPEN (P.C. = 153)
SYSTEM:EVAL1 (P.C. = 547)
EVAL (P.C. = 82)
(:METHOD KERMIT:KERMIT-FRAME :TOP-LEVEL) (P.C. = 201)
KERMIT:KERMIT-INITIAL-FUNCTION (P.C. = 21)
KERMIT:RUN-KERMIT-PROCESS (P.C. = 22)
SI:PROCESS-TOP-LEVEL (P.C. = 115)


0,, issues, 3, valid,
*** EOOH ***
Date: Tuesday, 2 April 1985, 03:40-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Common Lisp.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 751,
on Djinn:

CASE uses EQ for comparison.  This must be fixed in R2.

Ken.


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 29 March 1985, 18:10-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Fifteen:

When the lisp version of a file with the following form at top-level is
loaded, it does the right thing.  The compiled version gets an error.

(pkg-find-package 'picon t)


-mhd


0,, doc.prob, 3, valid,
*** EOOH ***
Date: Friday, 29 March 1985, 10:10-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn, BUG-DOC@LMI-Capricorn

In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Five A:

si:system-source-files (system)

, which returns a list of the source files for system, should be a documented function
(and I guess in the sys or user package).

-mhd


0,, 3, valid, indeterminate,
*** EOOH ***
From: mwt
Date: Thursday, 28 March 1985, 21:21-EST
To: bug-lispm


Rob Pettengill reported that defsystem modules that depend
on other system's module don't get their dependencies
processed correctly.   He said that (make-system xxx :recompile)
did the right thing, but (make-system xxx :compile) just
seemed to ignore the foreign system's dependencies.

E.g.

in the transformation (:compile-load module-a (:fasload (system-1 module-1)))
the dependency just seems to be ignored.


Mark


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Wednesday, 27 March 1985, 21:49-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.97, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.10,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 762,
2.0 Beta. Process Systems 3/26/85 Fork., on Lambda Ten:

;Reading at top level.
;Reading in base 10 in package USER with standard Zetalisp readtable.

(fs:merge-pathname-defaults "lam3:pic;ai-base" "*.*#*")
#FS:LM-PATHNAME "LAM3: PIC; AI-BASE.LISP#>"
T


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 25 March 1985, 18:12-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: MCC Bugs found in 2.0 Beta Release
To: dg@LMI-CAPRICORN
CC: bug-lispm@LMI-CAPRICORN
In-reply-to: <[LMI-BORIS].3/24/85 01:28:52.dg>
Message-ID: <[LMI-BORIS].3/25/85 18:12:00.RpK>

    Date: Sunday, 24 March 1985, 01:28-EST
    From: Dave Goodine <dg at LMI-CAPRICORN>
    Message-ID: <[LMI-BORIS].3/24/85 01:28:52.dg>

    2)  to undo
            (trace (:function (:method foo :plugh-me))), one must do
            (untrace (:method foo :plugh-me))
            which is not logical.

Trace and untrace have crockish interfaces.  People should use the Trace
item in the System Menu, which also allows untracing.

    3)  Itwouldbeniceif BREAKON worked on methods...

Seems to in this system (102.97/u761).

    4)  Rob Pettengill reports that in the old system, if he logged into a specific
            UNIX host, edited some files from other machines (one being the current buffer)
            and did a file/dired operation specifying just "~rcp...", that Zwei would
            do the right thing with the defaults, whereas now it merges with the current buffer
            paying no special attention to "~".

The Lisp Machine does not do anything special with the ~ character.  The
Unix file servers (LMI/CCC and Symbolics') both snarfed code from the C
shell so that * would work -- I guess ~ was put in, too.  If the Unix
file server does in fact support ~, the Lisp Machine will win -- but if
it doesn't there's no way for the Lambda to figure out the home
directory of an arbitary user.

Hmm -- when I try (viewf "cap:~rpk//.login"), I get the error

  Directory doesn't exist for CAP: /lmi/rpk/~rpk/.login

Maybe somebody put in a clever hack in Release 1.2 to pass ~ unscathed
through pathname defaulting.  It can be fixed somewhat simply, but first
I'd like to see if anything else in the system broke this feature.

    5)  In some instances, ZMAIL comes up with "CPTFONT" in the top little pane (label)
            instead of the usual "  No. Lines Date FromTo...".

Pretty bizarre.

    6)  Specifying :CHAOS-DIRECT in the site option :DEFAULT-MAIL-MODE breaks
            ZMAIL...

Back burner material.  There's really no good reason to specify it
anymore.


0,, 3, valid, indeterminate,
*** EOOH ***
Date: Sunday, 24 March 1985, 01:28-EST
From: Dave Goodine <dg@LMI-CAPRICORN>
Subject: MCC Bugs found in 2.0 Beta Release
To: bug-lispm@LMI-CAPRICORN
Message-ID: <[LMI-BORIS].3/24/85 01:28:52.dg>


For the most part, these are not critical bugs that need to be addressed
for the Final Release of 2.0.

1)      CREATEing a Lisp Listener (or any window) seems have problems with
        some irregular sizes... basically the first column is considered to be
        outside the border of the window.  This doesn't error for some reason though,
        the left border disappears.  I haven't re-created it yet, but I witnessed it.

2)      to undo
        (trace (:function (:method foo :plugh-me))), one must do
        (untrace (:method foo :plugh-me))
        which is not logical.

3)      Itwouldbeniceif BREAKON worked on methods...

4)      Rob Pettengill reports that in the old system, if he logged into a specific
        UNIX host, edited some files from other machines (one being the current buffer)
        and did a file/dired operation specifying just "~rcp...", that Zwei would
        do the right thing with the defaults, whereas now it merges with the current buffer
        paying no special attention to "~".  When I saw this happen, the
        current buffer had a logical pathname.  I saw it happen a few times
        but didn't note whether the default was a logical pathame, though I suspect
        it was.

5)      In some instances, ZMAIL comes up with "CPTFONT" in the top little pane (label)
        instead of the usual "  No. Lines Date FromTo...".

6)      Specifying :CHAOS-DIRECT in the site option :DEFAULT-MAIL-MODE breaks
        ZMAIL...

7)      #'SI:LOAD-LMC-FILE uses "LAM" as the default UNIT argument.  This might
        crash machines in the field if the neither user nor the software catch it.

-dave



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Thursday, 21 March 1985, 13:02-EST
From: mhd@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.96, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.2, Tiger 20.4, KERMIT 26.8,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 753, on Lambda Five A:


Insert your description of the circumstances here:

All I did was hit altmode after typing in NODEDEF in mini
buffer in zwei.  The default was PICON: PICON; RTIME-COM LISP >....
which doesn't matter, since completion in Zwei should definitely be
a no-error thing.
-mhd

>>ERROR: No translation for PICON: NODEDEF.
Backtrace from the debugger:

(:METHOD FS:LOGICAL-PATHNAME :TRANSLATED-PATHNAME) (P.C. = 112)
  (SELF is #FS:LOGICAL-PATHNAME "PICON: NODEDEF")

 Arg 0 (.OPERATION.): :TRANSLATED-PATHNAME
Local 0: NIL
Local 1 (TRANS): (#FS:LOGICAL-PATHNAME "PICON: PICON; * * *" #FS:LM-PATHNAME "LAM10: PIC; *.*#*")
Local 2 (NEWDIR): NIL


(:METHOD FS:LOGICAL-PATHNAME :COMPLETE-STRING) (P.C. = 53)
  (SELF is #FS:LOGICAL-PATHNAME "PICON: PICON; RTIME-COM LISP >")

 Arg 0 (.OPERATION.): :COMPLETE-STRING
 Arg 1 (STRING): "nodedef"
 Arg 2 (OPTIONS): (:NEW-OK)
Local 0 (STRING1): NIL
Local 1 (FOO): NIL
Local 2 (SUCCESS): NIL
Local 3 (TRANSLATED): NIL
Local 4 (BASE-PATHNAME): NIL
Local 5: NIL
Local 6: NIL
Local 7 (.SELECTQ.ITEM.): NIL


Additional information supplied with call:
 Expecting 2 values

FS:COMPLETE-PATHNAME (P.C. = 59)

 Arg 0 (DEFAULTS): ((#FS:LOGICAL-HOST "PICON" . #FS:LOGICAL-PATHNAME "PICON: PICON; RTIME-COM LISP >") (NIL . #FS:LOGICAL-PATHNAME "PICON: PICON; RTIME-COM LISP >"))
 Arg 1 (STRING): "nodedef"
 Arg 2 (TYPE): NIL
 Arg 3 (VERSION): NIL
 Rest arg (OPTIONS): (:NEW-OK)
Local 1 (PATHNAME): #FS:LOGICAL-PATHNAME "PICON: PICON; RTIME-COM LISP >"
Local 2 (HOST): NIL
Local 3 (START): 0
Local 4 (END): 7


ZWEI:PATHNAME-COMPLETE (P.C. = 50)

Local 0 (STRING): "nodedef"
Local 1 (VALUE): NIL


ZWEI:COM-PATHNAME-COMPLETE (P.C. = 21)

Local 0 (TEM): NIL


Remainder of stack:

ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) 0) (P.C. = 51)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) (P.C. = 39)
ZWEI:EDIT-IN-MINI-BUFFER (P.C. = 216)
ZWEI:READ-UNDEFAULTED-PATHNAME-STRING (P.C. = 92)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid,
*** EOOH ***
Date: Wednesday, 20 March 1985, 12:03-EST
From: rpk@cap
Sender: JMTurn@LMI-LAMBDA-3
Subject: Losing error message (Rel2)
To: BUG-LISPM@LMI-Capricorn

In System 102.94, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, Experimental DOE-Macsyma 9.0,
microcode 753, on Lambda Six:

(defun lose (x) (aref x 0))
(compile 'lose)

Now, try (lose nil).

>>TRAP 6827 (BAD-ARRAY-TYPE M-ARRAY-HEADER)
The array type, 7, was invalid in AR-1.
Backtrace from the debugger:

LOSE (P.C. = 16)

 Arg 0 (X): NIL


SYSTEM:EVAL1 (P.C. = 547)

 Arg 0 (FORM): (LOSE NIL)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (ENV): (NIL NIL T NIL)
Local 2 (TEM): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER LOSE 11437676>
Local 6 (ARG-DESC): 65
Local 7 (NUM-ARGS): 1
Local 8: NIL
Local 9: NIL
Local 10 (IGNORE): NIL
Local 11 (ARGL): NIL
Local 12 (ADL): NIL
Local 13 (ITEM): NIL
Local 14 (.SELECTQ.ITEM.): NIL



0,, valid, 2,
*** EOOH ***
Date: Tuesday, 19 March 1985, 23:13-EST
From: rjpi@cap
Sender: Ovid@LMI-LAMBDA-3
Subject: ZMail Profile Editor Bug
To: BUG-ZMAIL@LMI-Capricorn

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 753,
Education 2 x 2 Plus 102.92 16m Idle, on Test Lambda D:



Insert your description of the circumstances here:

There is something seriously wrong with the way the Profile Editor keeps
track of where to store information.  It keeps trying to store variables
that belong in ZMail.INIT in the last mail file whose options you edited.
This must be fixed ASAP or we will have lots of angry ZMail users!

>>TRAP 13135 (INSTANCE-LACKS-INSTANCE-VARIABLE M-C M-A)
There is no instance variable VARIABLE-TICK in #<BABYL-MAIL-FILE-BUFFER mbox.12.3 /lmi/rjpi/ CAP: 61732507>.
Backtrace from the debugger:

(:SELECT-METHOD ZMAIL-PROFILE-COMMAND-LIST :VARIABLE-CHOICE) (P.C. = 94)

 Arg 0 (IGNORE): :VARIABLE-CHOICE
 Arg 1 (WINDOW): #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed>
 Arg 2 (ITEM): (*ZMAIL-USUAL-MAIL-FILE-DIRECTORY* "Directory where most of your mail files live" :PATHNAME-OR-NIL)
 Arg 3 (CHOICE): NIL
 Arg 4 (LINE-NO): 11
 Arg 5 (BUTTON): 4000000
Local 0 (.WINDOW.): #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed>
Local 1 (.FOR-WINDOW.): #<ZMAIL-FRAME Main ZMail window 3212156 exposed>
Local 2 (.OSTATUS.): :EXPOSED
Local 3 (.OSUBST.): NIL
Local 4: NIL
Local 5 (COM): NIL
Local 6 (.QUEUE-LEFT.): NIL
Local 7 (E): NIL


(:METHOD ZMAIL-PROFILE-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
  (SELF is #<ZMAIL-PROFILE-FRAME Zmail Profile Frame 1 3217232 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
 Rest arg (ARGS): (:VARIABLE-CHOICE #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed> (*ZMAIL-USUAL-MAIL-FILE-DIRECTORY* "Directory where most of your mail files live" :PATHNAME-OR-NIL) NIL ...)


(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
  (SELF is #<ZMAIL-PROFILE-FRAME Zmail Profile Frame 1 3217232 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
Local 0: ("Return to ZMAIL command loop.")
Local 1: ((SYSTEM:ABORT ERROR) ("Return to ZMAIL command loop.") T ("Return to ZMAIL command loop.") ...)
Local 2 (RESPONSE): NIL
Local 3 (DEGREE): NIL


(:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)

 Rest arg (.DAEMON-CALLER-ARGS.): (:COMMAND-LOOP)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-4 6346665>


FUNCALL (P.C. = 21)

 Arg 0 (FN): #<DTP-FEF-POINTER (:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) 5571433>
 Rest arg (ARGS): (:COMMAND-LOOP)


Remainder of stack:

(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
COM-ZMAIL-PROFILE (P.C. = 184)
COMMAND-EXECUTE (P.C. = 88)
ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 18 March 1985, 22:27-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Bug destination.
To: Bug-LispM@LMI-CAPRICORN
Message-ID: <[LMI-DJINN].3/18/85 22:27:44.khs>

ZMail has been sending my bugs (presumably this one, too) to
Dracula, because at one point in this session Capricorn would
not accept a connection.  It does now though, so ZMail is
caching something improperly somewhere.

Ken.



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 18 March 1985, 13:50-EST
From: Michael Travers <MT@lmi-capricorn>
Subject: Filename completion problem
To: BUG-LISPM@LMI-Capricorn

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 753, on Lambda Five A:

While in a buffer visiting the file LAM3:QL.ZWEI;FONT.LISP, I did
C-X C-F DISPL <altmode>, and recieved this error.

>>ERROR: Attempt to print NIL, which is not a valid component.
Backtrace from the debugger:

FS:LM-PRINT-COMPONENT (P.C. = 97)

 Arg 0 (SPEC): NIL
 Arg 1 (STREAM): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 65536722>
   --Defaulted args:--
 Arg 2 (VERSION-P): NIL
Local 0 (TEM): NIL
Local 1 (I): NIL
Local 2: NIL


FS:LM-PRINT-DIRECTORY (P.C. = 68)

 Arg 0 (DEVICE): "DSK"
 Arg 1 (DIRECTORY): ("QL" NIL)
 Arg 2 (S): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 65536722>
 Arg 3 (SPACE): T
Local 0 (D): (NIL)


FS:LM-NAMESTRING (P.C. = 38)

 Arg 0 (HOST): NIL
 Arg 1 (DEVICE): "DSK"
 Arg 2 (DIRECTORY): ("QL" NIL)
 Arg 3 (NAME): "DISPL"
 Arg 4 (TYPE): NIL
 Arg 5 (VERSION): NIL
Local 0 (S): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 65536722>


(:METHOD FS:LM-PARSING-MIXIN :STRING-FOR-HOST) (P.C. = 35)
  (SELF is #FS:LM-PATHNAME ...error printing #<FS:LM-PATHNAME 7207225>...)

 Arg 0 (.OPERATION.): :STRING-FOR-HOST


(:METHOD FS:LM-PATHNAME :COMBINED :STRING-FOR-HOST) (P.C. = 43)
  (SELF is #FS:LM-PATHNAME ...error printing #<FS:LM-PATHNAME 7207225>...)

 Rest arg (.DAEMON-CALLER-ARGS.): (:STRING-FOR-HOST)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-10 22615605>


Remainder of stack:

(:METHOD FS:LOGICAL-PATHNAME :COMPLETE-STRING) (P.C. = 56)
FS:COMPLETE-PATHNAME (P.C. = 59)
PATHNAME-COMPLETE (P.C. = 50)
COM-PATHNAME-COMPLETE (P.C. = 21)
COMMAND-EXECUTE (P.C. = 88)
PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) 0) (P.C. = 51)
FUNCALL (P.C. = 21)
...
PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 18 March 1985, 13:46-EST
From: Michael Travers <MT@lmi-capricorn>
Subject: List Fonts problem
To: BUG-LISPM@LMI-Capricorn

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 753, on Lambda Five A:


>From ZWEI, I did a list fonts with an argument, to see the file computer
fonts.  (Incidentally, this system doesn't have a translation for
SYS:FONTS:) I then moused a couple of font names to see them.  When one
of the font displays ran off the bottom I got this error.

>>TRAP 26033 (TV-ERASE-OFF-SCREEN)
An attempt was made to do graphics past the end of the screen.
Backtrace from the debugger:

FED:DISPLAY-FONT (P.C. = 422)

 Arg 0 (FONT): #<FONT 25FR3 61435455>
 Arg 1 (WINDOW): #<EDITOR-TYPEOUT-WINDOW Editor Typeout Window 1 3202017 exposed>
 Arg 2 (CLEAR-FIRST-P): NIL
   --Defaulted args:--
 Arg 3 (FROM-FED): NIL
Local 0 (FONT-MAP): #<ART-Q-32 31173741>
Local 1 (CURRENT-FONT): #<FONT CPTFONT 30242471>
Local 2 (NAME): FONTS:25FR3
Local 3 (FD): #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 200 :FD-NAME ...)
Local 4 (DF): #<FONT CPTFONT 30242471>
Local 5 (CH): 40
Local 6 (OCH): 0
Local 7 (LEN): 200
Local 8 (CH1): 0


DISPLAY-FONT (P.C. = 51)

 Arg 0 (FONT-SYMBOL): FONTS:25FR3
Local 0 (FONT): #<FONT 25FR3 61435455>


(:SELECT-METHOD PROCESS-SPECIAL-COMMAND :TYPEOUT-EXECUTE) (P.C. = 31)

 Arg 0 (IGNORE): :TYPEOUT-EXECUTE
 Arg 1 (FUNCTION): DISPLAY-FONT
 Rest arg (ARGS): (FONTS:25FR3)
Local 1 (*MINI-BUFFER-DONT-RECORD*): T


(:METHOD WINDOW :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
  (SELF is #<ZMACS-WINDOW-PANE Zmacs Window Pane 1 3201000 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
 Rest arg (ARGS): (:TYPEOUT-EXECUTE DISPLAY-FONT FONTS:25FR3)


(:METHOD WINDOW :EDIT) (P.C. = 284)
  (SELF is #<ZMACS-WINDOW-PANE Zmacs Window Pane 1 3201000 exposed>)

 Arg 0 (.OPERATION.): :EDIT
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
 Arg 2 (*COMTAB*): #<COMTAB MODE-COMTAB 31207004>
 Arg 3 (*MODE-LINE-LIST*): ("ZMACS " "(" *MODE-NAME-LIST* ") " ...)
 Arg 4 (TOP-LEVEL-P): T
Local 0: ("Return to top level editor command loop.")
Local 1: ((SYSTEM:ABORT ERROR) ("Return to top level editor command loop.") T ("Return to top level editor command loop.") ...)
Local 2 (CH): (:TYPEOUT-EXECUTE DISPLAY-FONT FONTS:25FR3)


Remainder of stack:

(:INTERNAL (:METHOD ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid, indeterminate,
*** EOOH ***
From: robert
Date: Monday, 18 March 1985, 11:17-EST
To: bug-lispm rpk

Thinking Machines reports that pathnames with ">" as a delimiter
are not parsed correctly in their system 99 software (vintage
12/84).  Is this a 99 bug?


3/18/85 Update:

Running under 102.92, ucode 753, it is not possible to ^X^F from
ZMACS, (fs:copy-file ____ ____), etc. if the file in question is
on a Symbolics machine.  The system complains about a "missing
version number."  Even if a version number is supplied, it is
stripped from the pathname before it reaches the Symbolics machine.
(The problem was first reported by TMI, but Dave Goodine says
that MCC is similarly afflicted.)

robert


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 15 March 1985, 16:54-EST
From: mrc@LMI-CAPRICORN
Subject: Problem in Profile Editor
To: BUG-ZMail@LMI-Capricorn

In System 102.92, Local-File 56.0, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.14, ZMail 57.1, Tiger 20.4, KERMIT 26.4,
MEDIUM-RESOLUTION-COLOR 17.3, microcode 753,
Education 2 x 2 Plus 102.92 16m Idle, on Test Lambda A:


Insert your description of the circumstances here:

In the Profile Editor, I changed the variable ``Delete message after
moving to new buffer'' from Yes to No and got this error.

>>TRAP 5725 (INSTANCE-LACKS-INSTANCE-VARIABLE M-C M-A)
There is no instance variable VARIABLE-TICK in #<BABYL-MAIL-FILE-BUFFER random /lmi/mrc/mail/ CAP: 62633550>.
Backtrace from the debugger:

(:SELECT-METHOD ZMAIL-PROFILE-COMMAND-LIST :VARIABLE-CHOICE) (P.C. = 94)

 Arg 0 (IGNORE): :VARIABLE-CHOICE
 Arg 1 (WINDOW): #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed>
 Arg 2 (ITEM): (*DELETE-AFTER-MOVE-TO-FILE* "Delete message when moved into another buffer" :BOOLEAN)
 Arg 3 (CHOICE): NIL
 Arg 4 (LINE-NO): 9
 Arg 5 (BUTTON): 1048576
Local 0 (.WINDOW.): #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed>
Local 1 (.FOR-WINDOW.): #<ZMAIL-FRAME Main ZMail window 3212156 exposed>
Local 2 (.OSTATUS.): :EXPOSED
Local 3 (.OSUBST.): NIL
Local 4: NIL
Local 5 (COM): NIL
Local 6 (.QUEUE-LEFT.): NIL
Local 7 (E): NIL


(:METHOD ZMAIL-PROFILE-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
  (SELF is #<ZMAIL-PROFILE-FRAME Zmail Profile Frame 1 3217232 exposed>)

 Arg 0 (.OPERATION.): :PROCESS-SPECIAL-COMMAND
 Rest arg (ARGS): (:VARIABLE-CHOICE #<ZMAIL-CHOOSE-VARIABLE-VALUES-PANE Zmail Choose Variable Values Pane 1 3220063 exposed> (*DELETE-AFTER-MOVE-TO-FILE* "Delete message when moved into another buffer" :BOOLEAN) NIL ...)


(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
  (SELF is #<ZMAIL-PROFILE-FRAME Zmail Profile Frame 1 3217232 exposed>)

 Arg 0 (.OPERATION.): :COMMAND-LOOP
Local 0: ("Return to ZMAIL command loop.")
Local 1: ((SYSTEM:ABORT ERROR) ("Return to ZMAIL command loop.") T ("Return to ZMAIL command loop.") ...)
Local 2 (RESPONSE): NIL
Local 3 (DEGREE): 0


(:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)

 Rest arg (.DAEMON-CALLER-ARGS.): (:COMMAND-LOOP)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-4 6346665>


FUNCALL (P.C. = 21)

 Arg 0 (FN): #<DTP-FEF-POINTER (:INTERNAL (:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) 0) 5571433>
 Rest arg (ARGS): (:COMMAND-LOOP)


Remainder of stack:

(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-UTILITY-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
COM-ZMAIL-PROFILE (P.C. = 184)
COMMAND-EXECUTE (P.C. = 88)
ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:SELECT-METHOD ZMAIL-COMMAND-LIST :MENU) (P.C. = 26)
(:METHOD ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 172)
(:INTERNAL (:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, unreproducible, valid, 4,
*** EOOH ***
From: robert
Date: Friday, 15 March 1985, 12:38-EST
To: bug-lispm

Our "si:unfasl" is documented on p. 319 of the Orangual as
"si:unfasl-file."  Either the documentation or the function name
should be changed so that an unfoolish consistency is maintained.

robert


0,, fixed, 3, valid,
*** EOOH ***
Date: Wednesday, 13 March 1985, 14:48-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Get off my Case.
To: khs@LMI-CAPRICORN
CC: helen@LMI-CAPRICORN, BUG-LISPM@LMI-Capricorn
In-reply-to: <[LMI-DJINN].3/08/85 22:34:06.khs>
Message-ID: <[LMI-LAMBDA-5-A].3/13/85 14:48:05.RpK>

    Date: Friday, 8 March 1985, 22:34-EST
    From: Ken Sinclair <khs at LMI-CAPRICORN>

    Message-ID: <[LMI-DJINN].3/08/85 22:34:06.khs>

        Date: Friday, 8 March 1985, 19:31-EST
        From: helen at LMI-CAPRICORN

        (let ((alphabetic-case-affects-string-comparison t))
          (string-search "L" "l"))
        this returns 0
        it should return nil

    There is a CONSIDER-CASE argument to STRING-SEARCH and related functions.
    Presently, if you do not supply the argument, the functions default to
    being case-insensitive.

Yep, this is a change for Common Lisp.  Quite reasonable, too.

    In the development sources, the case-sensitivity of these functions
    defaults to the value of ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON.

NO NO NO.  This is a regressive loss.  Case-sensitivity is something
that should be made explicit at the point of the function call, not
controlled by some special variable.  This is incompatible with Common
Lisp and what has been documented in the latest Lisp Machine Manual and
the Release 2.0 notes.



0,, 3, valid, indeterminate,
*** EOOH ***
From: mwt
Date: Wednesday, 13 March 1985, 12:15-EST
To: bug-lispm

12-Mar-85 17:02:44-EST,1079;000000000001
Received: from MIT-MC by MIT-OZ via Chaosnet; 12 Mar 85 16:59-EST
Received: from MCC.ARPA by MIT-MC.ARPA; 12 MAR 85 16:59:21 EST
Received: from MCC-BELL by MCC.ARPA via Chaosnet; Tue 12 Mar 85 15:59:04-CST
Received: by mcc-bell with CHAOS id AA11907; Tue, 12 Mar 85 15:45:31 cst
Date: Tuesday, 12 March 1985, 15:45-CST
From: Robert C. Pettengill <rcp@MCC-BELL>
Re: Note for GJC
To: mwt%mit-mc@mcc


Mark,

Would you please forward this to George -

George the problem is this:

Both

(COPY-FILE "star:sys;* * >" "bell:~rcp//lispm//star//sys//")

;and

(COPY-FILE (SEND (FS:PARSE-PATHNAME "star:sys;* *") ':NEW-VERSION ':NEWEST)
           "bell:~rcp//lispm//star//sys//")

end up copying all versions - newest first - from the VMS host to
the unix host.  Since the oldest version is copied last you also end
up with the oldest version of each file on the unix host.  This is
a double looser.

Wildcards in copy-file are one nice feature that LMI has that Symbolics
doesn't but with behavior like that above they can do more damage than
good.

-Rob


0,, 3, indeterminate,
*** EOOH ***
Date: Monday, 11 March 1985, 15:53-EST
From: pace@LMI-CAPRICORN
Sender: LISPM@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In System 102.93, Local-File 56.4, FILE-Server 13.1, Unix-Interface 5.3,
MagTape 40.16, ZMail 57.1, Experimental LM-Prolog 3.0, microcode 753,
102 16Mb, on Lambda Two:


Insert your description of the circumstances here:

I wanted to disassemble a flavor method.
The cursor was at the top of the DEFETHOD.
I said M-X Disassemble followed by c-sh-Y to insert the defaulted argument into the
minibuffer, and it bombed.


>>ERROR: Cannot convert (:METHOD CONSTRAINT :UNIFY) into a string.
Backtrace from the debugger:

STRING (P.C. = 74)

 Arg 0 (X): (:METHOD CONSTRAINT :UNIFY)


ZWEI:INSERT (P.C. = 579)

 Arg 0 (BP): ("" 0 :NORMAL)
 Arg 1 (STRING): (:METHOD CONSTRAINT :UNIFY)
 Arg 2 (START): 0
 Arg 3 (END): NIL
Local 0 (LINE): ""
Local 1 (INDEX): 0
Local 2 (LINE-LENGTH): 0
Local 3 (FIRST-NEWLINE): NIL
Local 4 (FIRST-LINE): NIL
Local 5 (LAST-LINE): NIL
Local 6 (LAST-NEWLINE): NIL
Local 7 (ARRAY-TYPE): NIL
Local 8 (LCHARS): NIL
Local 9: NIL
Local 10 (BP): NIL
Local 11 (I): NIL
Local 12 (PREV-LINE): NIL
Local 13 (THIS-LINE): NIL
Local 14 (PREV-NEWLINE): NIL
Local 15 (NEWLINE): NIL
Local 16 (THE-LINE-BEYOND): NIL
Local 17 (LENGTH): NIL
Local 18 (LF): NIL
Local 19 (LT): NIL


ZWEI:INSERT-MOVING (P.C. = 35)

 Arg 0 (BP): ("" 0 :NORMAL)
 Arg 1 (STRING): (:METHOD CONSTRAINT :UNIFY)
   --Defaulted args:--
 Arg 2 (START): 0
 Arg 3 (END): NIL
Local 0 (NBP): NIL


ZWEI:COM-YANK-DEFAULT-STRING (P.C. = 39)



Additional information supplied with call:
 Expecting 3 values

ZWEI:COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): ZWEI:COM-YANK-DEFAULT-STRING
 Arg 1 (CHAR): #/c-/y
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
Local 0 (HOOK-SUCCESS): T
Local 1: NIL
Local 2 (HOOK): NIL


Remainder of stack:

ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) 0) (P.C. = 51)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) (P.C. = 39)
ZWEI:EDIT-IN-MINI-BUFFER (P.C. = 216)
ZWEI:COMPLETING-READ-FROM-MINI-BUFFER (P.C. = 69)
ZWEI:READ-FUNCTION-NAME (P.C. = 180)
...
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Sunday, 10 March 1985, 17:42-EST
From: pace@LMI-CAPRICORN
Sender: LISPM@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.84, Experimental Local-File 56.0,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 753,
102 16Mb, on Lambda Two:



;;Gets error while compiling, "negative number of pops: -1"
(defun zot (m)
  (prog ()
        label
           (return
             (let ((n 0))
               (bind m n)
               (go label)))))

;;Doesnt get error.
(defun zot (m)
  (prog ()
        label
           (let ((n 0))
             (bind m n)
             (go label))))
;;-- Mats Carlsson



0,, issues, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 5 March 1985, 13:39-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Rubout handler
To: naha@LMI-CAPRICORN, bug-lispm@LMI-CAPRICORN
In-reply-to: The message of 5 Mar 1985 12:39-EST from naha
Message-ID: <[LMI-DJINN].3/05/85 13:39:25.khs>

    From: naha
    Date: Tuesday, 5 March 1985, 12:39-EST

    I seem to remember a brief period of time a few months abck when
    C-M-b and some of the other sexp things worked in the rubout handler.
    I think it may have been on some of the 99 bands.  Would that we
    could return to that golden age.

            -naha

They do work, but they're stupid, meaning they don't use the same rules
as Zwei does.  For instance, atoms aren't considered sexps.

MLY is working on a new rubout handler, primarily to facilitate a supdup
server, but i bet i could convince him to fix up some of these other
things.

Ken.



0,, valid, 2,
*** EOOH ***
Date: Tuesday, 5 March 1985, 11:19-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.50, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.6, Experimental ZMail 57.0,
Experimental KERMIT 24.0, Experimental Tiger 11.2, microcode 729,
R2.0 102.50 gc5 + kitchen sink (site), on Lambda Fifteen:


Insert your description of the circumstances here:

I said

 (car 5)

in the interpreter. This is after bypassing abort-trivial-errors.
Look at what it thinks the arg was: -16776188.

I tried this first yesterday, and did (setf (eh-arg 0) '(x)),
then did c-m-r (re-evaluate), and the machine crashed.

-mhd

>>TRAP 4419 (ARGTYP CONS M-T T CAR CAR)
The argument to CAR, 5, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

CAR:
   Arg 0 (X): -16776188


SYSTEM:EVAL1 (P.C. = 710)

 Arg 0 (FORM): (CAR 5)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 1
Local 1 (TEM): NIL
Local 2 (ENV): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-U-ENTRY CAR 555>
Local 6 (ARG-DESC): 65
Local 7 (NUM-ARGS): 1
Local 8 (CLOSURE-PASSED): NIL
Local 9 (LAMBDA-LIST): NIL
Local 10 (LL): NIL
Local 11 (QUOTE-STATUS): NIL
Local 12 (REST-FLAG): NIL
Local 13 (ARGL): NIL
Local 14: NIL
Local 15: NIL
Local 16 (IGNORE): NIL
Local 17 (ADL): NIL
Local 18 (ITEM): NIL
Local 19 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (CAR 5)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (CAR 5)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


BREAK (P.C. = 437)

 Arg 0 (FORMAT-STRING): "ZMail."
 Rest arg (ARGS): NIL
Local 1 (SAVED-BUFFER): NIL
Local 2 (SAVED-BUFFER-POSITION): NIL
Local 3: NIL
Local 4 (LAST-TIME-READTABLE): #<READTABLE standard traditional syntax 25740210>
Local 5 (CHAR): 40
Local 6 (THROW-FLAG): T
Local 7: ("Return to BREAK ~?" "ZMail." NIL)
Local 8: ((SYSTEM:ABORT ERROR) ("Return to BREAK ~?" "ZMail." NIL) T ("Return to BREAK ~?" "ZMail." NIL) ...)
Local 9: #<DTP-FEF-POINTER (:INTERNAL BREAK SI:.DO.IT.) 2431603>
Local 10: #:*TERMINAL-IO*-SYN-STREAM
Local 11 (TEM1): (CAR 5)
Local 12 (TEM): NIL
Local 13 (VALUES): NIL
Local 14 (VALUE): NIL


Remainder of stack:

ZWEI:COM-ZMAIL-BREAK (P.C. = 40)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:ZMAIL-COMMAND-EXECUTE (P.C. = 23)
(:METHOD ZWEI:ZMAIL-FRAME :PROCESS-COMMAND-CHAR) (P.C. = 32)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) (P.C. = 177)
(:INTERNAL (:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) 0) (P.C. = 40)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP) (P.C. = 47)
(:METHOD ZWEI:ZMAIL-FRAME :COMBINED :COMMAND-LOOP) (P.C. = 39)
ZWEI:ZMAIL-PROCESS-TOP-LEVEL (P.C. = 79)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 25 February 1985, 19:19-EST
From: rjpi@LMI-CAPRICORN
Sender: ingria@LMI-CAPRICORN
Subject: Old Mail Files Hanging Around
To: BUG-ZMAIL@LMI-Capricorn
CC: rjpi@cap

In ZMAIL in Experimental System 102.50, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.6, Experimental ZMail 57.0,
Experimental KERMIT 24.0, Experimental Tiger 11.2, microcode 729,
R2.0 102.50 gc5 + kitchen sink (site), on Lambda Fifteen:

Two of the past three times I have used ZMail I have discovered somebody
else's mail files hanging around in my ZMail, and accessable in the
menus popped up by Save Files or Move.  This is NOT good.  When someone
logs out, hir mail file should not be accessible in ZMail to the next
person who logs in.



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 25 February 1985, 06:28-EST
From: James M Turner <jmturn@LMI-CAPRICORN>
Subject: Problem with EH
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.87, Experimental Local-File 56.0,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0,
Experimental Imagen Printing System 2.5, microcode 753, 102 16Mb,
on Test Lambda G:


Insert your description of the circumstances here:

I'm getting this error trying to Control-R in the error handler. Is this something
generic in 102? This is the error handler error, the stack I was trying to debug
follows.

>>TRAP 5948 (SUBSCRIPT-OOB M-Q M-ARRAY-LENGTH (NIL RESTORE-ARRAY-REGISTERS))
The subscript -1018894 for #<ART-REG-PDL-6656 3706015> was out of range in AR-1.
Backtrace from the debugger:

EH:SG-FRAME-VALUE-LIST (P.C. = 120)

 Arg 0 (SG): #<DTP-STACK-GROUP "Imagen Server Process" 30247601>
 Arg 1 (FRAME): -1018891
 Arg 2 (NEW-NUMBER-OF-VALUES): NIL
 Arg 3 (ORIGINAL-FRAME): 145
Local 0 (RP): #<ART-REG-PDL-6656 3706015>
Local 1 (IDX): NIL
Local 2 (TYPE): NIL
Local 3 (MORE-P): NIL
Local 4 (STORING-OPTION): NIL
Local 5 (NUM-TOTAL): NIL
Local 6 (NUM-ALREADY): NIL
Local 7 (POINTER): NIL
Local 8: NIL
Local 9 (I): NIL
Local 10 (IDX1): NIL
Local 11 (LIST-SLOT-IDX): NIL
Local 12 (EXTRA): NIL


EH:SG-FRAME-VALUE-LIST (P.C. = 272)

 Arg 0 (SG): #<DTP-STACK-GROUP "Imagen Server Process" 30247601>
 Arg 1 (FRAME): 138
 Arg 2 (NEW-NUMBER-OF-VALUES): NIL
 Arg 3 (ORIGINAL-FRAME): 145
Local 0 (RP): #<ART-REG-PDL-6656 3706015>
Local 1 (IDX): 134
Local 2 (TYPE): 1
Local 3 (MORE-P): 0
Local 4 (STORING-OPTION): SYSTEM:ADI-ST-INDIRECT
Local 5 (NUM-TOTAL): NIL
Local 6 (NUM-ALREADY): NIL
Local 7 (POINTER): NIL
Local 8: NIL
Local 9 (I): NIL
Local 10 (IDX1): NIL
Local 11 (LIST-SLOT-IDX): NIL
Local 12 (EXTRA): NIL


Additional information supplied with call:
 Expecting 2 values

EH:SG-FRAME-VALUE-LIST (P.C. = 137)

 Arg 0 (SG): #<DTP-STACK-GROUP "Imagen Server Process" 30247601>
 Arg 1 (FRAME): 145
   --Defaulted args:--
 Arg 2 (NEW-NUMBER-OF-VALUES): NIL
 Arg 3 (ORIGINAL-FRAME): 145
Local 0 (RP): #<ART-REG-PDL-6656 3706015>
Local 1 (IDX): NIL
Local 2 (TYPE): NIL
Local 3 (MORE-P): NIL
Local 4 (STORING-OPTION): NIL
Local 5 (NUM-TOTAL): NIL
Local 6 (NUM-ALREADY): NIL
Local 7 (POINTER): NIL
Local 8: NIL
Local 9 (I): NIL
Local 10 (IDX1): NIL
Local 11 (LIST-SLOT-IDX): NIL
Local 12 (EXTRA): NIL


EH:COM-RETURN-A-VALUE (P.C. = 128)

 Arg 0 (SG): #<DTP-STACK-GROUP "Imagen Server Process" 30247601>
 Arg 1 (IGNORE): #EH:ARG-TYPE-ERROR :CONDITION-NAMES (EH:ARG-TYPE-ERROR ERROR CONDITION SYSTEM:WRONG-TYPE-ARGUMENT) :ARG-NUMBER 0 :FUNCTION < :ARG-POINTER NIL :ARG-DATA-TYPE 3 :DESCRIPTION NUMBER :ARG-LOCATION-IN-SG EH:PP :RESTART-TAG EH:QILSP
 Rest arg (IGNORE): NIL
Local 1 (VALUE): NIL
Local 2 (FN): (:METHOD IMAGEN:IMAGEN-STREAM :PRINT-STREAM1)
Local 3 (NUMBER-LOC-OR-NIL): NIL
Local 4: NIL
Local 5: NIL
Local 6 (ACCUM): NIL
Local 7 (I): NIL
Local 8 (VALUE): NIL
Local 9 (FLAG): NIL


EH:COMMAND-LOOP (P.C. = 447)

 Arg 0 (ERROR-SG): #<DTP-STACK-GROUP "Imagen Server Process" 30247601>
 Arg 1 (ERROR-OBJECT): #EH:ARG-TYPE-ERROR :CONDITION-NAMES (EH:ARG-TYPE-ERROR ERROR CONDITION SYSTEM:WRONG-TYPE-ARGUMENT) :ARG-NUMBER 0 :FUNCTION < :ARG-POINTER NIL :ARG-DATA-TYPE 3 :DESCRIPTION NUMBER :ARG-LOCATION-IN-SG EH:PP :RESTART-TAG EH:QILSP
Local 0 (FUNCTION): EH:COM-RETURN-A-VALUE
Local 1 (SEXP): 2097234
Local 2 (EVALHOOK): NIL
Local 3 (SPECIAL-COMMANDS): NIL
Local 4 (WINDOW-ERROR-HANDLER-OLD-WINDOW): NIL
Local 5 (IO-BUFFER): #<TV:IO-BUFFER 4454273: empty, State: NIL>
Local 6 (READING-COMMAND): NIL
Local 7 (.L.): NIL
Local 8 (.VAR.): *READTABLE*
Local 9 (.VAL.): #<READTABLE standard Zetalisp 30265237>
Local 10: (SYSTEM:ABORT ("Return to debugger command loop") T ("Return to debugger command loop") ...)
Local 11: ("Return to debugger command loop")
Local 12 (NUMERIC-ARG): NIL
Local 13: ("Return to debugger command loop.")
Local 14: ((SYSTEM:ABORT ERROR) ("Return to debugger command loop.") T ("Return to debugger command loop.") ...)
Local 15 (I): 1
Local 16: 1
Local 17 (VALUES): NIL
Local 18 (VALUE): NIL
Local 19: NIL
Local 20: NIL
Local 21 (IGNORE): NIL


Remainder of stack:

(:METHOD CONDITION :DEBUGGER-COMMAND-LOOP) (P.C. = 23)
EH:SECOND-LEVEL-ERROR-HANDLER (P.C. = 712)

THIS IS THE STACK I WAS TRYING TO DEBUG

>>TRAP 7918 (ARGTYP NUMBER PP 0 QILSP)
The first argument to <, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

(:METHOD IMAGEN:IMAGEN-STREAM :PRINT-STREAM1) (P.C. = 206)
  (SELF is #<IMAGEN:IMAGEN-STREAM 60565121>)

 Arg 0 (.OPERATION.): :PRINT-STREAM1
 Arg 1 (IN-STREAM): #<FS:QFILE-INPUT-CHARACTER-STREAM "SYS: IMAGEN; PACKET LISP >" 61432451>
 Arg 2 (OUT-STREAM): #<IMAGEN:IMAGEN-STREAM 60565121>
Local 0 (FILE): "SYS: IMAGEN; PACKET LISP >"
Local 1 (PATHNAME): #FS:LOGICAL-PATHNAME "SYS: IMAGEN; PACKET LISP >"
Local 2 (FC): 0
Local 3 (TEMP): NIL
Local 4 (V-POS): NIL
Local 5 (PAGE-COUNT): 1
Local 6 (CHAR-COUNT): 0
Local 7 (FONTS): (:CPTFONT :HL12B :CPTFONTB)
Local 8 (FONT-STACK): NIL
Local 9 (HEADER-FONT-NUMBER): 2
Local 10 (FONT-DESCS): (#S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 128 :FD-NAME ...) #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 128 :FD-NAME ...) #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 128 :FD-NAME ...))
Local 11 (FD): #S(FED:FONT-DESCRIPTOR :FD-FILL-POINTER 128 :FD-NAME ...)
Local 12 (LINE-SPACE): 480
Local 13: NIL
Local 14 (FONT): :CPTFONTB
Local 15 (CHAR): 59
Local 16 (N): NIL


Additional information supplied with call:
 Multiple values passed to frame, but frame pointer is NIL.
 This means that we were going to pass multiple values
 to a frame that did not want them.

IMAGEN:IMAGEN-PRINT-STREAM1 (P.C. = 30)

 Arg 0 (STREAM): #<FS:QFILE-INPUT-CHARACTER-STREAM "SYS: IMAGEN; PACKET LISP >" 61432451>
 Arg 1 (ORIENTATION): :PORTRAIT
   --Defaulted args:--
 Arg 2 (OUT-STREAM): #<IMAGEN:IMAGEN-STREAM 60565121>


IMAGEN:IMAGEN-PRINT-FILE1 (P.C. = 50)

 Arg 0 (FILE): "sys:imagen;packet lisp"
 Arg 1 (ORIENTATION): :PORTRAIT
Local 0: #<FS:QFILE-INPUT-CHARACTER-STREAM "SYS: IMAGEN; PACKET LISP >" 61432451>
Local 1 (.FILE-ABORTED-FLAG.): :ABORT
Local 2 (STREAM): #<FS:QFILE-INPUT-CHARACTER-STREAM "SYS: IMAGEN; PACKET LISP >" 61432451>


(:METHOD IMAGEN:IMAGEN-STREAM :RUN-SERVER) (P.C. = 93)
  (SELF is #<IMAGEN:IMAGEN-STREAM 60565121>)

 Arg 0 (.OPERATION.): :RUN-SERVER
Local 0: NIL
Local 1 (.FILE-ABORTED-FLAG.): NIL
Local 2 (STR1): NIL


IMAGEN:IMAGEN-SERVER (P.C. = 20)



Remainder of stack:

SI:PROCESS-RUN-FUNCTION-INTERNAL (P.C. = 66)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, valid, 2, indeterminate,
*** EOOH ***
From: robert
Date: Friday, 22 February 1985, 20:24-EST
To: bug-lispm

Problems with the tv:choose-variable-values function (see pp.
196-8 of the Window System Manual):

   1) If the left mouse button is depressed while the mouse
cursor is outside the variable-window, the window disappears, but
the function does not return.  User must control-abort to regain
control of keyboard and mouse.

   2) Windows are not resized unless the function is called with
a variable list which has a different length than on the previous
invocation.  Consequently, windows may be too wide (if variables
have decreased in size) or too narrow (if variables have
increased in size).

   3) Setting the :function option to t causes the function to
hang, forcing a control-abort.

robert



0,, unreproducible, 3,
*** EOOH ***
Date: Friday, 22 February 1985, 04:22-EST
From: Pace Willisson <bobp@LMI-CAPRICORN>
Sender: pace@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.66, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 751,
almost Beta Test, on Lambda Two:


Insert your description of the circumstances here:

OK, this is random, but in mis-spelling :displaced-to as :displace-to,
the error is reported as :type being random instead of :displace-to.

>>ERROR: :TYPE is not a known MAKE-ARRAY keyword.
Backtrace from the debugger:

MAKE-ARRAY (P.C. = 291)

 Arg 0 (DIMENSIONS): 5138
 Rest arg (OPTIONS): (:TYPE :ART-8B :DISPLACE-TO #<ART-8B-65536 61554755> ...)
Local 1 (LENGTH-OF-OPTIONS): 6
Local 2 (ENTRIES-PER-Q): NIL
Local 3 (LEADER-LIST): NIL
Local 4 (FILL-POINTER): NIL
Local 5 (DISPLACED-TO): NIL
Local 6 (DISPLACED-INDEX-OFFSET): NIL
Local 7 (NAMED-STRUCTURE-SYMBOL): NIL
Local 8 (AREA): NIL
Local 9 (TYPE): ART-8B
Local 10 (TYPE-P): T
Local 11 (ELEMENT-TYPE-P): NIL
Local 12 (INITIAL-ELEMENT): NIL
Local 13 (INITIAL-ELEMENT-P): NIL
Local 14 (INITIAL-CONTENTS): NIL
Local 15 (INITIAL-CONTENTS-P): NIL
Local 16 (ARRAY): NIL
Local 17 (N-DIMENSIONS): NIL
Local 18 (INDEX-LENGTH): NIL
Local 19 (LONG-ARRAY-P): NIL
Local 20 (LEADER-QS): NIL
Local 21 (DATA-LENGTH): NIL
Local 22 (LEADER-LENGTH): NIL
Local 23 (O): (:DISPLACE-TO #<ART-8B-65536 61554755> :DISPLACED-INDEX-OFFSET 45136)
Local 24 (VALUE): #<ART-8B-65536 61554755>
Local 25: NIL
Local 26 (DIM): NIL
Local 27 (HEADER-WORD): NIL
Local 28 (DIMLIST): NIL
Local 29 (I): NIL
Local 30 (IDX): NIL
Local 31 (LEADER-LIST): NIL


COMPARE-FILES (P.C. = 53) (from file LAM3: BOBP; SDUROM.#)



SYSTEM:EVAL1 (P.C. = 710)

 Arg 0 (FORM): (COMPARE-FILES)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 0
Local 1 (TEM): NIL
Local 2 (ENV): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER COMPARE-FILES 46673445>
Local 6 (ARG-DESC): 0
Local 7 (NUM-ARGS): 0
Local 8 (CLOSURE-PASSED): NIL
Local 9 (LAMBDA-LIST): NIL
Local 10 (LL): NIL
Local 11 (QUOTE-STATUS): NIL
Local 12 (REST-FLAG): NIL
Local 13 (ARGL): NIL
Local 14: NIL
Local 15: NIL
Local 16 (IGNORE): NIL
Local 17 (ADL): NIL
Local 18 (ITEM): NIL
Local 19 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (COMPARE-FILES)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (COMPARE-FILES)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


Remainder of stack:

BREAK (P.C. = 437)
ZWEI:COM-BREAK (P.C. = 36)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, doc.prob, issues, 3, valid, cosmetic,
*** EOOH ***
Date: Thursday, 21 February 1985, 09:58-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: TRACE documentation.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.80, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 751,
on Djinn:

TRACE needs a real documentation string.

Ken.



0,, doc.prob, issues, 3, valid,
*** EOOH ***
Date: Wednesday, 20 February 1985, 19:01-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: Displaced macro documentation.
To: Bug-LispM@Capricorn
Message-ID: <[LMI-DJINN].2/20/85 19:01:17.khs>

The Release 2.0 notes should have more detail about when displaced macros
don't work.  Also, SI:INIHIBIT-DISPLACING-FLAG should be discussed.

Ken.



0,, valid, 4, indeterminate,
*** EOOH ***
Date: Wednesday, 20 February 1985, 18:03-EST
From: ml@LMI-CAPRICORN
Sender: LH@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.50, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.6, Experimental ZMail 57.0,
Experimental KERMIT 24.0, Experimental Tiger 11.2, microcode 729,
R2.0 102.50 gc5 + kitchen sink (mhd), on Lambda Ten:

In listings on the Toshiba, if there's a tab at the beginning of a line, then the line
is printed one position to the right of where it should be.  This has been true for some
time on more than one machine....  Lowell



0,, issues, valid, 2,
*** EOOH ***
Date: Monday, 18 February 1985, 19:06-EST
From: khs@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.61, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 741,
on Djinn:

The tail-recursion flag has been broken in Release 2.0 and should not
be documented.  It is unlikely that it will ever be reinstated; perhaps
we should document and provide the compile-time tail-recursion
eliminator instead.  George?

Ken.



0,, 3, valid,
*** EOOH ***
Date: Monday, 18 February 1985, 18:32-EST
From: bobp@LMI-CAPRICORN
Sender: khs@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.73, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.2,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 748,
102.61 new apply-lambda, on Lambda Nine:


Insert your description of the circumstances here:

parse-integer is documented to accept hex, but doesn't.

>>ERROR: " 3F3 " does not contain simply a number surrounded by whitespace.
Backtrace from the debugger:

PARSE-INTEGER (P.C. = 147)

 Arg 0 (STRING): " 3F3 "
 Rest arg: (:RADIX 16)
Local 1 (START): 0
Local 2 (END): NIL
Local 3 (RADIX): 16
Local 4 (JUNK-ALLOWED): NIL
Local 5 (INDEX): 2
Local 6 (STOP): 5
Local 7 (NUM): 3
Local 8 (SIGN): NIL
Local 9 (TEM): NIL
Local 10 (CH): #/F
Local 11 (CH): #/F


SYSTEM:EVAL1 (P.C. = 710)

 Arg 0 (FORM): (PARSE-INTEGER " 3F3 " :RADIX 16)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (ARGNUM): 3
Local 1 (TEM): NIL
Local 2 (ENV): NIL
Local 3 (MUMBLE): NIL
Local 4 (TAIL): NIL
Local 5 (FCTN): #<DTP-FEF-POINTER PARSE-INTEGER 2462560>
Local 6 (ARG-DESC): 1179713
Local 7 (NUM-ARGS): 3
Local 8 (CLOSURE-PASSED): NIL
Local 9 (LAMBDA-LIST): NIL
Local 10 (LL): NIL
Local 11 (QUOTE-STATUS): NIL
Local 12 (REST-FLAG): NIL
Local 13 (ARGL): NIL
Local 14: NIL
Local 15: NIL
Local 16 (IGNORE): NIL
Local 17 (ADL): NIL
Local 18 (ITEM): NIL
Local 19 (.SELECTQ.ITEM.): NIL


SI:EVAL-SPECIAL-OK (P.C. = 81)

 Arg 0 (FORM): (PARSE-INTEGER " 3F3 " :RADIX 16)
   --Defaulted args:--
 Arg 1 (NOHOOK): NIL
Local 0 (TEM): NIL
Local 1 (ENV): NIL


Additional information supplied with call:
 Values to be collected for MULTIPLE-VALUE-LIST

SI:EVAL-ABORT-TRIVIAL-ERRORS (P.C. = 36)

 Arg 0 (TOP-LEVEL-FORM): (PARSE-INTEGER " 3F3 " :RADIX 16)
Local 0: ((SYSTEM:TOO-FEW-ARGUMENTS SYSTEM:TOO-MANY-ARGUMENTS SYSTEM:CELL-CONTENTS-ERROR SYSTEM:WRONG-TYPE-ARGUMENT ...) SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER)
Local 1: ((** SI:EVAL-ABORT-TRIVIAL-ERRORS-HANDLER))


BREAK (P.C. = 437)

 Arg 0 (FORMAT-STRING): "ZMACS."
 Rest arg (ARGS): NIL
Local 1 (SAVED-BUFFER): NIL
Local 2 (SAVED-BUFFER-POSITION): NIL
Local 3: NIL
Local 4 (LAST-TIME-READTABLE): #<READTABLE standard traditional syntax 25740212>
Local 5 (CHAR): 2097219
Local 6 (THROW-FLAG): T
Local 7: ("Return to BREAK ~?" "ZMACS." NIL)
Local 8: ((SYSTEM:ABORT ERROR) ("Return to BREAK ~?" "ZMACS." NIL) T ("Return to BREAK ~?" "ZMACS." NIL) ...)
Local 9: #<DTP-FEF-POINTER (:INTERNAL BREAK SI:.DO.IT.) 2431603>
Local 10: #:*TERMINAL-IO*-SYN-STREAM
Local 11 (TEM1): (PARSE-INTEGER " 3F3 " :RADIX 16)
Local 12 (TEM): NIL
Local 13 (VALUES): NIL
Local 14 (VALUE): 4


Remainder of stack:

ZWEI:COM-BREAK (P.C. = 36)
ZWEI:COMMAND-EXECUTE (P.C. = 88)
ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD ZWEI:WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, doc.prob, 3, valid,
*** EOOH ***
Date: Sunday, 17 February 1985, 14:29-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: array fill pointers
To: mrc@LMI-CAPRICORN, bug-lispm@LMI-CAPRICORN, rpk@LMI-CAPRICORN
In-reply-to: The message of 18 Jan 1985 15:00-EST from mrc


    From mrc Fri Jan 18 15:00:40 1985
    To: bug-lispm

    The function (fill-pointer arrayname) p.166 of the orange LMM
    is documented to return NIL if the array does not have a fill
    pointer.  Currently, it throws you into the debugger instead.
                    mrc


The documentation is wrong.  Common LISP says that "it is an error ...
if the array does not have a fill pointer ..."

Ken.



0,, 3, valid,
*** EOOH ***
Date: Sunday, 17 February 1985, 02:05-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: ZEROP optimizer.
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.67, Experimental Local-File 55.5,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 741,
on Djinn:

(= FOO 0) optimizes into (ZEROP FOO), but (= 0 FOO) doesn't.

Ken.



0,, valid, 2, indeterminate,
*** EOOH ***
From: robert
Date: Friday, 15 February 1985, 18:50-EST
To: bug-lispm

Font problems:
   1) After the following form is evaluated, the cursor position
is about 5 pixels too high:

   (send terminal-io ':set-font-map '(cptfont medfnt bigfnt))

   The font for the window is correctly set to the first font in
the list.  Oddly enough, if "bigfnt" is first, the cursor
position is fine.

   2) The following form really mucks things up:

   (send terminal-io ':set-current-font 'medfnt)

   Characters are displayed in the new font, but the rubout
handler seems to be thinking in terms of the previous one (the
rubout key erases portions of characters, cursor movement is
bizarre, etc.).  One sees the same behavior if the font is
changed via the Attributes choice in the System menu.

   These problems exist in both 1.2 and 2.0.


0,, valid, 2, indeterminate,
*** EOOH ***
Date: Tuesday, 12 February 1985, 15:21-EST
From: vicky@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.61, Experimental Local-File 55.4,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 740,
on Lambda Two:

IMPORT doesn't import symbols.  It should be calling intern-local instead of intern.



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 11 February 1985, 20:28-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Completion of Logical pathnames
To: BUG-LISPM@LMI-Capricorn
Message-ID: <[LMI-LAMBDA-9].2/11/85 20:28:51.RpK>

In Experimental System 102.61, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.12, Experimental ZMail 57.0, microcode 740,
102.61 new apply-lambda, on Lambda Nine:

Now that ZWEI gives logical pathnames as defaults when it can (which is
actually a Good Thing), some bugs are occurring in logical pathname
components.  This happened with Zwei c-X c-F:

Given the defaults SYS: DOC; MCDOC TEXT >, the string "REL2"

>>ERROR: Attempt to print NIL, which is not a valid component.
Backtrace from the debugger:

FS:LM-PRINT-COMPONENT (P.C. = 97)

 Arg 0 (SPEC): NIL
 Arg 1 (STREAM): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 71271374>
   --Defaulted args:--
 Arg 2 (VERSION-P): NIL
Local 0 (TEM): NIL
Local 1 (I): NIL
Local 2: NIL


FS:LM-PRINT-DIRECTORY (P.C. = 68)

 Arg 0 (DEVICE): "DSK"
 Arg 1 (DIRECTORY): ("QL" NIL)
 Arg 2 (S): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 71271374>
 Arg 3 (SPACE): T
Local 0 (D): (NIL)


FS:LM-NAMESTRING (P.C. = 38)

 Arg 0 (HOST): NIL
 Arg 1 (DEVICE): "DSK"
 Arg 2 (DIRECTORY): ("QL" NIL)
 Arg 3 (NAME): "FOO"
 Arg 4 (TYPE): NIL
 Arg 5 (VERSION): NIL
Local 0 (S): #<CLOSURE FORMAT:FORMAT-STRING-STREAM 1 71271374>


(:METHOD FS:LM-PARSING-MIXIN :STRING-FOR-HOST) (P.C. = 35)
  (SELF is #FS:LM-PATHNAME ...error printing #<FS:LM-PATHNAME 3035221>...)

 Arg 0 (.OPERATION.): :STRING-FOR-HOST


(:METHOD FS:LM-PATHNAME :COMBINED :STRING-FOR-HOST) (P.C. = 43)
  (SELF is #FS:LM-PATHNAME ...error printing #<FS:LM-PATHNAME 3035221>...)

 Rest arg (.DAEMON-CALLER-ARGS.): (:STRING-FOR-HOST)
Local 1 (.DAEMON-MAPPING-TABLE.): #<ART-16B-8 36015605>


Remainder of stack:

(:METHOD FS:LOGICAL-PATHNAME :COMPLETE-STRING) (P.C. = 56)
FS:COMPLETE-PATHNAME (P.C. = 59)
PATHNAME-COMPLETE (P.C. = 50)
COM-PATHNAME-COMPLETE (P.C. = 21)
COMMAND-EXECUTE (P.C. = 88)
PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZWEI-WITHOUT-TYPEOUT :COMBINED :EDIT) 0) (P.C. = 51)
FUNCALL (P.C. = 21)
...
PROCESS-COMMAND-CHAR (P.C. = 59)
(:METHOD WINDOW :PROCESS-COMMAND-CHAR) (P.C. = 20)
(:METHOD WINDOW :EDIT) (P.C. = 307)
(:INTERNAL (:METHOD ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, valid, 2, indeterminate,
*** EOOH ***
Date: Saturday, 9 February 1985, 04:01-EST
From: khs@LMI-LAMBDA-3
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.57, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.8, Experimental ZMail 57.0, microcode 735,
on Djinn:

Some of the bignum microcode runs for long periods of time
without taking sequence breaks.  I think this can be fixed.

Ken.



0,, 3, valid, indeterminate,
*** EOOH ***
From: colpitts
Date: Wednesday, 6 February 1985, 16:14-EST
To: bug-lispm

acos and other inverse trig functions are not defined in 2.0 for
real arguments. rpk's notes state that transcendental functions
are not defined for complex arguments but doesn't mention real
arguments.


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Wednesday, 6 February 1985, 10:55-EST
From: Dave Goodine <dg@LMI-CAPRICORN>
Subject: Trying to FS:CREATE-DIRECTORY on CAP
To: bug-unix@LMI-Capricorn, bug-lispm@cap

In unix in Experimental System 102.48, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.6, Experimental ZMail 57.0, microcode 729,
Software Release work band, on Test Lambda H:

Tried doing (fs:create-directory "cap://lmi//dg//foo") and
            (fs:create-directory "cap://lmi//dg//foo//")

but lost.  Both times CAP closed the connection, seemingly
when it received the create directory message
(wholine said "Create Directory" right before the connection got closed).

-dg



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Wednesday, 6 February 1985, 10:07-EST
From: Dave Goodine <dg@LMI-CAPRICORN>
Subject: Zmail and GMSGS
To: bug-zmail@cap
Message-ID: <[LAMBDA-TEST-H].2/06/85 10:07:02.dg>


Zmail should think that messages taken from the GMSGS server are
new mail and correctly go to the first unseen msg, instead of to the
beginning of all messages (which happens if nothing was found in /usr/spool/mail.
-dg



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Tuesday, 29 January 1985, 21:13-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: m-. Bug, CLI:MAP
To: BUG-LISPM@LMI-Capricorn
Message-ID: <[LAMBDA-TEST-C].1/29/85 21:13:21.RpK>

In Experimental System 102.39, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Test Lambda C:

While in an editor buffer with -*-Readtable:CL-*-, I did a m-. of
CLI:MAP by using the mouse.  Of course, in my code, it just looks like
MAP.  First, it read in GENRIC, but instead of stopping there it then
started to read in QFCTNS, and stopped at the definition of GLOBAL:MAP.

Also, it would be really nice if CLI:MAP would open code a little if the
result type was NIL.  Actually, you can do better even if the result
type is known at compile time.



0,, valid, 4, cosmetic,
*** EOOH ***
Date: Monday, 28 January 1985, 20:04-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Indent Under in ZMACS menu
To: BUG-ZWEI@LMI-CAPRICORN
CC: rjpi@LMI-CAPRICORN, lmc@LMI-CAPRICORN

In ZWEI in Experimental System 102.24, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Test Lambda A:

        Indent Under prompts for a string to indent under.  If it is
invoked by C-M-X it prints out the prompt on the Mode Line.  If it is
invoked from the ZMACS Menu, it prints out the prompt only if you type
some character after clicking on [Indent Under]; i.e. when you slect
this option from the ZMACS Menu, nothing seems to be happening, and only
if you type another character will the promt appear.



0,, unreproducible, 3, valid, indeterminate,
*** EOOH ***
Date: Monday, 28 January 1985, 19:55-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Indent...
To: BUG-ZWEI@LMI-CAPRICORN
CC: BUG-LMMAN@LMI-CAPRICORN, lmc@LMI-CAPRICORN

In ZWEI in Experimental System 102.24, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Test Lambda A:

        A question and a bug:

The question:

        What is the difference between Indent Region and Indent Rigidly?


The bug:

        Indent Rigidly does not seem to work from the ZMACS menu
(Indent Region did work; DON'T tell me that's the difference between the
two of them!)



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Monday, 28 January 1985, 19:38-EST
From: rjpi@LMI-CAPRICORN
Sender: Ingria@LMI-CAPRICORN
Subject: Change Default Font
To: BUG-ZWEI@LMI-CAPRICORN
CC: rjpi@LMI-CAPRICORN, lmc@LMI-CAPRICORN

In Experimental System 102.24, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Test Lambda A:


Insert your description of the circumstances here:

        I invoked Change Default Font and typed Altmode to add a new
font specification (it was BIGFNT).  It queried me about adding the
change to the file attributes list.  I typed Y and got this error.

>>TRAP 7732 (ARGTYP NUMBER PP 0 QIADD)
The first argument to +, NIL, was of the wrong type.
The function expected a number.
Backtrace from the debugger:

ZWEI:INPUT-FONT-NAME (P.C. = 306)

 Arg 0 (USE-PREVIOUS-P): NIL
 Arg 1 (PROMPT): "Set default font to (Font ID): "
Local 0 (NUM): NIL
Local 1 (CH): 27
Local 2 (ALIST): (("CPTFONT" . #<FONT CPTFONT 25743131>) ("MEDFNT" . #<FONT MEDFNT 26002631>))
Local 3: NIL
Local 4: #<DTP-LOCATIVE 40541001>
Local 5: #<DTP-LOCATIVE 6140377>
Local 6 (OLD-SELECTED-WINDOW): #<ZWEI:ZMACS-FRAME Zmacs Frame 1 1340371 exposed>
Local 7 (OLD-SUBSTITUTE): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 8 1375005 exposed>
Local 8 (I): NIL
Local 9 (L): NIL


ZWEI:COM-CHANGE-DEFAULT-FONT (P.C. = 27)



ZWEI:MAKE-MENU-COMMAND-DRIVER (P.C. = 58)

Local 0 (COMMAND): ZWEI:COM-CHANGE-DEFAULT-FONT
Local 1 (MENU): #<ZWEI:MENU-COMMAND-MOMENTARY-MENU Menu Command Momentary Menu 2 1370064 deactivated>


Additional information supplied with call:
 Expecting 3 values

ZWEI:COMMAND-EXECUTE (P.C. = 88)

 Arg 0 (COMMAND): #<CLOSURE ZWEI:MAKE-MENU-COMMAND-DRIVER 2 51422656>
 Arg 1 (CHAR): #/Mouse-Right-1
 Arg 2 (PREFIX-CHAR): NIL
 Arg 3 (HOOK-LIST): NIL
Local 0 (HOOK-SUCCESS): T
Local 1: NIL
Local 2 (HOOK): NIL


ZWEI:PROCESS-COMMAND-CHAR (P.C. = 59)

 Arg 0 (CH): #/Mouse-Right-1
Local 0 (VALUE): NIL
Local 1 (LINE): NIL
Local 2 (INDEX): NIL
Local 3: NIL
Local 4 (HOOK): NIL


Remainder of stack:

(:SELECT-METHOD ZWEI:PROCESS-SPECIAL-COMMAND :MOUSE-BUTTON) (P.C. = 111)
(:METHOD ZWEI:WINDOW :PROCESS-SPECIAL-COMMAND) (P.C. = 21)
(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 284)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, 3, valid,
*** EOOH ***
Date: Thursday, 24 January 1985, 20:02-EST
From: Robert P. Krajewski <rpk@LMI-CAPRICORN>
Subject: Common Lisp/String referencing bugs
To: BUG-LISPM@LMI-Capricorn
FCC: CAP: /lmi/rpk/Mail/cc.bb
Message-ID: <[LMI-LAMBDA-8].1/24/85 20:02:26.RpK>

In Experimental System 102.25, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 715,
on Lambda Eight:

First, not even CLI:AREF (d/b/a the COMMON-LISP-AR instructions) seem to
do anything special about ART-FAT-STRINGS.

(defvar *a* (make-array 10 :type art-fat-string :initial-element #\A))

(aref *a* 0) => 65

[BUG !] (char *a* 0) => 65
[BUG !] (elt *a* 0) => 65

Secondly, ELT should use CLI:AREF, like CHAR does, so that strings will
yield character objects:

(aref "A" 0) => 65
(char "A" 0) => #/A

[BUG !] (elt "A" 0) => 65

An element type of CHARACTER for MAKE-ARRAY also seems to do the wrong
thing.  In Common Lisp, strings are arrays whose element type is
STRING-CHAR -- if the user explicit wants an array of ``real'' character
objects, he should be able to do

   (make-array n :element-type 'character)

On the Lisp Machine, this returns an ART-STRING array.  It should
actually be returning an ART-Q array.

It's too bad that Common Lisp doesn't have (VECTOR SI:FAT-CHAR), which
is what I need for an application (a Bolio processor for the Lisp
Machine and NIL).



0,, 3, valid, indeterminate,
*** EOOH ***
Date: Thursday, 24 January 1985, 16:22-EST
From: Michael Travers <MT@lmi-capricorn>
Subject: inspector problems
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.24, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.1, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Lambda Six:

Is this a problem between landscape and protrait monitors? -Custer-

The inspector does not grind lists well.  Sometimes it places parts of
the structure off the screen so they cannot be seen or moused.  The list
below will illustrate the problem.  Here it was ground via the ZMACS
Grind Expression command.  I suggest that the inspector use whatever
method or parameters this command does, since it seems to work.

The inspector also loses with circular DEFSTRUCTs (ie, slot A of FOO
points to BAR, slot B of BAR points to FOO).  Since it handles circular
lists tolerably well, perhaps this is also fixable.  I guess it only
became a problem with the common lisp #S(...) notation.

((PROPERTY TIGER)
 -6
 6
 ((RULE I10 (IF (CARNIVORE TAWNY BLACK-STRIPES) THEN TIGER))
  -20
  20
  ((PROPERTY CARNIVORE)
   -6
   6
   ((RULE I6 (IF (MAMMAL POINTED-TEETH CLAWS FORWARD-EYES) THEN CARNIVORE))
    -20
    20
    ((PROPERTY MAMMAL) -6
                       6
                       ((RULE I2 (IF (MILK) THEN MAMMAL)) -20 20 ((PROPERTY MILK) -6 6))
                       ((RULE I1 (IF (HAIR) THEN MAMMAL)) -20 20 ((PROPERTY HAIR) -6 6)))
    ((PROPERTY POINTED-TEETH) -6 6)
    ((PROPERTY CLAWS) -6 6)
    ((PROPERTY FORWARD-EYES) -6 6))
   ((RULE I5 (IF (MAMMAL EATS-MEAT) THEN CARNIVORE))
    -20
    20
    ((PROPERTY MAMMAL) -6
                       6
                       ((RULE I2 (IF (MILK) THEN MAMMAL)) -20 20 ((PROPERTY MILK) -6 6))
                       ((RULE I1 (IF (HAIR) THEN MAMMAL)) -20
                                                          20
                                                          ((PROPERTY HAIR) -6 6)))
    ((PROPERTY EATS-MEAT) -6 6)))
  ((PROPERTY TAWNY) -6 6)
  ((PROPERTY BLACK-STRIPES) -6 6)))



0,, 3, valid, indeterminate,
*** EOOH ***
From: robert
Date: Wednesday, 23 January 1985, 16:15-EST
To: bug-lispm

If an unbound variable is given to the Kermit login server's
read-eval-print loop, the following is sent repeatedly to the
remote terminal:

        >>ERROR: The object #<PS-TERMINAL 34562442> received a
      READ-CURSORPOS message, which went unclaimed.
        The rest of the message was (CHARACTER).
        >>ERROR: The object #<PS-TERMINAL 345...


Nothing entered at the remote terminal has any effect; the
process must be killed from the Lambda.  (Testing was done on
LAMA, Release 1.2.)

robert


0,, unreproducible, 3, valid,
*** EOOH ***
Date: Tuesday, 22 January 1985, 18:00-EST
From: Michael Travers <mt@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.18, Experimental Local-File 55.3,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.1,
Experimental MagTape 40.0, Experimental ZMail 57.0, microcode 715,
on Lambda Eight:

LOOP is broken in this system.  The following returns NIL, it should
return a list of numbers:

(loop for foo from 0 to 10
      with bar
      collect foo into bar
      finally (return bar))

This, on the other hand, works properly.

(loop for foo from 0 to 10
      collect foo)


--{}



0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 18 January 1985, 16:47-EST
From: Ken Sinclair <khs@LMI-CAPRICORN>
Subject: %DRAW-STRING understands brain-damaged fonts.
To: Mark Henry David <mhd@LMI-CAPRICORN>, BUG-LISPM@LMI-Capricorn
In-reply-to: The message of 18 Jan 1985 15:14-EST from mhd@LMI-CAPRICORN


    From mhd@LMI-CAPRICORN Fri Jan 18 15:16:03 1985
    Date: Friday, 18 January 1985, 15:14-EST
    From: Mark Henry David <mhd at LMI-CAPRICORN>
    To: BUG-LISPM at LMI-Capricorn

    In Experimental System 102.10, Experimental Local-File 55.1,
    Experimental FILE-Server 13.1, Experimental Unix-Interface 5.0,
    Experimental MagTape 40.0, Experimental ZMail 57.0, microcode 714,
    R2.0 102.3 gc3 + ucode ethernet, on Test Lambda B:


    Insert your description of the circumstances here:

    Changed to bigfnt in the editor.
    -mhd

Hmm.  I didn't know fixed-width fonts could have kerning tables.  Seems kind
of silly to me.  SYSTEM-102-18 has a new %DRAW-STRING that's more defensive
about these things.

Ken.

    >>TRAP 13571 (ARGTYP ARRAY M-A 0 FALL-THROUGH AREF)
    The first argument to AREF, NIL, was of the wrong type.
    The function expected an array.
    Backtrace from the debugger:

    Additional information supplied with call:
     Expecting 2 values

    TV:%DRAW-STRING (P.C. = 234)

     Arg 0 (SHEET): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
     Arg 1 (ALU): 7
     Arg 2 (XPOS): 3
     Arg 3 (YPOS): 2
     Arg 4 (STRING): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
     Arg 5 (FONT): #<FONT BIGFNT 25777757>
     Arg 6 (START): 0
     Arg 7 (STOP): 60
     Arg 8 (XLIM): 1420
    Local 0 (C): 50
    Local 1 (I): 0
    Local 2 (WIDTH): NIL
    Local 3 (TAB-WIDTH): 140
    Local 4 (BASE-YPOS): 2
    Local 5 (NPOS): 3
    Local 6 (FONT-INDEX): NIL
    Local 7 (FONT-NEXT): NIL
    Local 8 (FONT-MAP): #<ART-Q-32 60104524>
    Local 9 (FONT-INDEX-TABLE): NIL
    Local 10 (FONT-WIDTH-TABLE): NIL
    Local 11 (FONT-KERN-TABLE): #<ART-32B-200 27717314>
    Local 12 (LOZENGE-STRING): NIL
    Local 13 (INSIDE-LEFT): 3


    Additional information supplied with call:
     Expecting 2 values

    TV:SHEET-LINE-OUT (P.C. = 161)

     Arg 0 (SHEET): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
     Arg 1 (STRING): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
     Arg 2 (START): 0
     Arg 3 (STOP): 60
     Arg 4 (SET-XPOS): 0
     Arg 5 (SET-YPOS): 0
       --Defaulted args:--
     Arg 6 (DWIDTH): NIL
    Local 0 (INSIDE-RIGHT): 1434
    Local 1 (INSIDE-LEFT): 3
    Local 2 (MARGIN-FLAG): T
    Local 3 (XPOS): 3
    Local 4 (YPOS): 2
    Local 5 (STOP-INDEX): NIL
    Local 6 (STOP-XPOS): NIL


    (:METHOD ZWEI:WINDOW :REDISPLAY) (P.C. = 729)
      (SELF is #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>)

     Arg 0 (.OPERATION.): :REDISPLAY
     Arg 1 (RECENTER-TYPE): :POINT
     Arg 2 (RC1): NIL
     Arg 3 (RC2): NIL
     Arg 4 (FORCE-TO-COMPLETION-P): NIL
    Local 0 (LH): 26
    Local 1 (NOW): 45410
    Local 2 (POINT-PLINE): NIL
    Local 3 (POINT-LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
    Local 4 (POINT-INDEX): 0
    Local 5 (TOP-LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
    Local 6 (TOP-INDEX): 0
    Local 7 (LAST-BP): ("" 0 :MOVES)
    Local 8 (INITIAL-DEGREE): 5
    Local 9 (POINT-NODE): NIL
    Local 10 (START-BP-NODE): NIL
    Local 11 (BUF): NIL
    Local 12 (NEW-TOP-INDEX): NIL
    Local 13 (Y): NIL
    Local 14 (LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
    Local 15 (INDEX): NIL
    Local 16 (P): NIL
    Local 17 (LINE-LENGTH): NIL
    Local 18 (LEN): NIL
    Local 19 (DWID): NIL
    Local 20 (CH): NIL
    Local 21 (FONT): NIL
    Local 22 (CWT): NIL
    Local 23 (CWID): NIL
    Local 24 (RWID): NIL
    Local 25 (I): NIL
    Local 26 (TW): NIL
    Local 27 (L): NIL
    Local 28 (FROM-INDEX): 0
    Local 29 (TO-INDEX): 60
    Local 30 (PLINE): 0
    Local 31 (STOP-LINE): ""
    Local 32 (FROB): NIL
    Local 33 (PLINE): NIL
    Local 34: NIL
    Local 35 (BL): NIL


    ZWEI:REDISPLAY (P.C. = 56)

     Arg 0 (WINDOW): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
       --Defaulted args:--
     Arg 1 (RECENTER-TYPE): :POINT
     Arg 2 (RC1): NIL
     Arg 3 (RC2): NIL
     Arg 4 (FORCE-TO-COMPLETION-P): NIL


    ZWEI:REDISPLAY-ALL-WINDOWS (P.C. = 61)

       --Defaulted args:--
     Arg 0 (FORCE-TO-COMPLETION-P): NIL
     Arg 1 (SELECT-P): T
    Local 0: (#<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>)
    Local 1 (WINDOW): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>


    Remainder of stack:

    (:METHOD ZWEI:WINDOW :EDIT) (P.C. = 259)
    (:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
    FUNCALL (P.C. = 21)
    (:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
    (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
    ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
    SI:PROCESS-TOP-LEVEL (P.C. = 115)




0,, unreproducible, 3, valid,
*** EOOH ***
Date: Friday, 18 January 1985, 15:14-EST
From: Mark Henry David <mhd@LMI-CAPRICORN>
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.10, Experimental Local-File 55.1,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.0,
Experimental MagTape 40.0, Experimental ZMail 57.0, microcode 714,
R2.0 102.3 gc3 + ucode ethernet, on Test Lambda B:


Insert your description of the circumstances here:

Changed to bigfnt in the editor.
-mhd


>>TRAP 13571 (ARGTYP ARRAY M-A 0 FALL-THROUGH AREF)
The first argument to AREF, NIL, was of the wrong type.
The function expected an array.
Backtrace from the debugger:

Additional information supplied with call:
 Expecting 2 values

TV:%DRAW-STRING (P.C. = 234)

 Arg 0 (SHEET): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
 Arg 1 (ALU): 7
 Arg 2 (XPOS): 3
 Arg 3 (YPOS): 2
 Arg 4 (STRING): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
 Arg 5 (FONT): #<FONT BIGFNT 25777757>
 Arg 6 (START): 0
 Arg 7 (STOP): 60
 Arg 8 (XLIM): 1420
Local 0 (C): 50
Local 1 (I): 0
Local 2 (WIDTH): NIL
Local 3 (TAB-WIDTH): 140
Local 4 (BASE-YPOS): 2
Local 5 (NPOS): 3
Local 6 (FONT-INDEX): NIL
Local 7 (FONT-NEXT): NIL
Local 8 (FONT-MAP): #<ART-Q-32 60104524>
Local 9 (FONT-INDEX-TABLE): NIL
Local 10 (FONT-WIDTH-TABLE): NIL
Local 11 (FONT-KERN-TABLE): #<ART-32B-200 27717314>
Local 12 (LOZENGE-STRING): NIL
Local 13 (INSIDE-LEFT): 3


Additional information supplied with call:
 Expecting 2 values

TV:SHEET-LINE-OUT (P.C. = 161)

 Arg 0 (SHEET): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
 Arg 1 (STRING): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
 Arg 2 (START): 0
 Arg 3 (STOP): 60
 Arg 4 (SET-XPOS): 0
 Arg 5 (SET-YPOS): 0
   --Defaulted args:--
 Arg 6 (DWIDTH): NIL
Local 0 (INSIDE-RIGHT): 1434
Local 1 (INSIDE-LEFT): 3
Local 2 (MARGIN-FLAG): T
Local 3 (XPOS): 3
Local 4 (YPOS): 2
Local 5 (STOP-INDEX): NIL
Local 6 (STOP-XPOS): NIL


(:METHOD ZWEI:WINDOW :REDISPLAY) (P.C. = 729)
  (SELF is #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>)

 Arg 0 (.OPERATION.): :REDISPLAY
 Arg 1 (RECENTER-TYPE): :POINT
 Arg 2 (RC1): NIL
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL
Local 0 (LH): 26
Local 1 (NOW): 45410
Local 2 (POINT-PLINE): NIL
Local 3 (POINT-LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
Local 4 (POINT-INDEX): 0
Local 5 (TOP-LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
Local 6 (TOP-INDEX): 0
Local 7 (LAST-BP): ("" 0 :MOVES)
Local 8 (INITIAL-DEGREE): 5
Local 9 (POINT-NODE): NIL
Local 10 (START-BP-NODE): NIL
Local 11 (BUF): NIL
Local 12 (NEW-TOP-INDEX): NIL
Local 13 (Y): NIL
Local 14 (LINE): "(defstruct (rtime-shared-memory (:conc-name /"/"))"
Local 15 (INDEX): NIL
Local 16 (P): NIL
Local 17 (LINE-LENGTH): NIL
Local 18 (LEN): NIL
Local 19 (DWID): NIL
Local 20 (CH): NIL
Local 21 (FONT): NIL
Local 22 (CWT): NIL
Local 23 (CWID): NIL
Local 24 (RWID): NIL
Local 25 (I): NIL
Local 26 (TW): NIL
Local 27 (L): NIL
Local 28 (FROM-INDEX): 0
Local 29 (TO-INDEX): 60
Local 30 (PLINE): 0
Local 31 (STOP-LINE): ""
Local 32 (FROB): NIL
Local 33 (PLINE): NIL
Local 34: NIL
Local 35 (BL): NIL


ZWEI:REDISPLAY (P.C. = 56)

 Arg 0 (WINDOW): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>
   --Defaulted args:--
 Arg 1 (RECENTER-TYPE): :POINT
 Arg 2 (RC1): NIL
 Arg 3 (RC2): NIL
 Arg 4 (FORCE-TO-COMPLETION-P): NIL


ZWEI:REDISPLAY-ALL-WINDOWS (P.C. = 61)

   --Defaulted args:--
 Arg 0 (FORCE-TO-COMPLETION-P): NIL
 Arg 1 (SELECT-P): T
Local 0: (#<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>)
Local 1 (WINDOW): #<ZWEI:ZMACS-WINDOW-PANE Zmacs Window Pane 1 1344014 exposed>


Remainder of stack:

(:METHOD ZWEI:WINDOW :EDIT) (P.C. = 259)
(:INTERNAL (:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) 0) (P.C. = 60)
FUNCALL (P.C. = 21)
(:METHOD ZWEI:DISPLAYER :AROUND :EDIT) (P.C. = 25)
(:METHOD ZWEI:ZMACS-WINDOW :COMBINED :EDIT) (P.C. = 39)
ZWEI:ZMACS-WINDOW-TOP-LEVEL (P.C. = 38)
SI:PROCESS-TOP-LEVEL (P.C. = 115)



0,, valid, 4, indeterminate,
*** EOOH ***
Date: Tuesday, 15 January 1985, 09:31-EST
From: gjc@LMI-CAPRICORN
Sender: IRIS@LMI-CAPRICORN
To: BUG-LISPM@LMI-Capricorn

In Experimental System 102.12, Experimental Local-File 55.2,
Experimental FILE-Server 13.1, Experimental Unix-Interface 5.0,
Experimental MagTape 40.0, Experimental ZMail 57.0,
Experimental MEDIUM-RESOLUTION-COLOR 17.0,
Experimental MICRO-COMPILATION-TOOLS 1.0, Experimental iris 10.0,
microcode 714, R2.0 2x2 test band, on Waiting for Godot:


Insert your description of the circumstances here:
We warm booted the machine while it was in this window. Evidently rubout handler is not expecting
:WARM-BOOT to be put into the ZWEI:HISTORY. Is this the fault of the person pushing that into the history?

>>TRAP 4398 (ARGTYP CONS M-T T CAR CAR)
The argument to CAR, :WARM-BOOT, was of the wrong type.
The function expected a cons.
Backtrace from the debugger:

MISMATCH (P.C. = 179)

 Arg 0 (SEQUENCE1): #<ART-Q-512 61256062>
 Arg 1 (SEQUENCE2): :WARM-BOOT
 Rest arg (KEYARGS): NIL
Local 1 (FROM-END): NIL
Local 2 (TEST): NIL
Local 3 (TEST-NOT): NIL
Local 4 (KEY): NIL
Local 5 (START1): 0
Local 6 (END1): NIL
Local 7 (START2): 0
Local 8 (END2): NIL
Local 9 (INDEX1): 0
Local 10 (INDEX2): :WARM-BOOT
Local 11 (I): 0
Local 12 (STOP1): 17
Local 13 (STOP2): NIL


TV:ALTERNATE-RUBOUT-HANDLER (P.C. = 225)

Local 0 (CH): NIL
Local 1 (CH-CHAR): NIL
Local 2 (CH-CONTROL-META): NIL
Local 3 (COMMAND): NIL
Local 4 (FILL-POINTER): 0
Local 5 (TYPEIN-POINTER): 17
Local 6 (STATUS): :INITIAL-ENTRY
Local 7 (RUBBED-OUT-SOME): NIL
Local 8 (NUMERIC-ARG): NIL
Local 9 (NUMERIC-ARG-NEGATIVE): NIL
Local 10 (PROMPT-OPTION): NIL
Local 11 (INITIAL-INPUT): NIL
Local 12 (INITIAL-INPUT-POINTER): NIL
Local 13 (EDITING-COMMAND): NIL
Local 14 (DO-NOT-ECHO): NIL
Local 15 (PASS-THROUGH): NIL
Local 16 (COMMAND-HANDLER): NIL
Local 17 (PREEMPTABLE): NIL
Local 18 (ACTIVATION-HANDLER): NIL
Local 19 (VALUE): NIL


(:METHOD TV:STREAM-MIXIN :ANY-TYI) (P.C. = 114)
  (SELF is #<IRIS-SERIAL-STREAM-LISP-LISTENER Iris Serial Stream Lisp Listener 1 1370463 exposed>)

 Arg 0 (.OPERATION.): :ANY-TYI
   --Defaulted args:--
 Arg 1 (IGNORE): NIL
Local 0 (IDX): 0
Local 1 (CHAR): NIL
Local 2 (STRING): NIL
Local 3 (INDEX): NIL




0,, issues, 3, valid,
*** EOOH ***
Date: Monday, 14 January 1985, 17:23-EST
From: Pace Willisson <pace@cap>
Subject: grinder
To: bug-lispm@cap
Message-ID: <[LMI-LAMBDA-9].1/14/85 17:23:00.pace>


make the grinder work on circular structures like *print-circle* does




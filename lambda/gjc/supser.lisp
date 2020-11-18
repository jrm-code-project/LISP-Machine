;; -*- Mode:LISP; Package:SUPDUP; Base:10 -*-

;; SUPDUP-TIMEOUT is time in which the LISTEN must win.  Set high for debugging.
(DEFVAR SUPDUP-TIMEOUT 100000.)

;; ITS Output Buffer codes -- 0-177 are graphic characters which are output as is.
;; The %TD codes control cursor movement and other terminal functions.  Comments
;; describe code function, arguments, and conditions in which the code can be used.

(DEFVAR %TDEOF #o202)       ;Clear to end of page
(DEFVAR %TDEOL #o203)       ;Clear to end of line
(DEFVAR %TDDLF #o204)       ;Delete character after cursor, without moving cursor
(DEFVAR %TDCRL #o207)       ;Newline character (CRLF)
(DEFVAR %TDNOP #o210)       ;No-op
(DEFVAR %TDBS  #o211)       ;Backspace
                            ; use only if %TORAW set
(DEFVAR %TDLF  #o212)       ;Linefeed
                            ; use only if %TORAW set
(DEFVAR %TDRCR #o213)       ;Carriage return
                            ; use only if %TORAW set
(DEFVAR %TDQOT #o215)       ;Quote next character
(DEFVAR %TDFS  #o216)       ;Move cursor forward one column
(DEFVAR %TDMV0 #o217)       ;Move cursor
                            ; arguments are vertical and horizontal position
(DEFVAR %TDCLR #o220)       ;Clear screen and home cursor
(DEFVAR %TDBEL #o221)       ;Ring bell
(DEFVAR %TDILP #o223)       ;Insert Lines
                            ; argument is number of lines
                            ; use only if %TOLID set
(DEFVAR %TDDLP #o224)       ;Delete Lines
                            ; argument is number of lines
                            ; use only if %TOLID set
(DEFVAR %TDICP #o225)       ;Insert Characters
                            ; argument is number of characters
                            ; use only if %TOCID set
(DEFVAR %TDDCP #o226)       ;Delete Characters
                            ; argument is number of characters
                            ; use only if %TOCID set

;; ITS TTYOPT word -- describes terminal capabilities.
;; Left half fields begin with %TO, right hand with %TP.

(DEFVAR %TOSAI #o4000_18.)
(DEFVAR %TOOVR #o1000_18.)
(DEFVAR %TOMVU  #o400_18.)
(DEFVAR %TORAW   #o40_18.)    ;Suppress cursor motion optimization
(DEFVAR %TOFCI   #o10_18.)    ;Can generate ITS 12-bit character set
                            ;%TPCBS (control-back-slash) will also be on
(DEFVAR %TOLID    #o2_18.)    ;Can insert/delete lines
(DEFVAR %TOCID    #o1_18.)    ;Can insert/delete characters

;; ITS 12-bit character representation -- low 7 bits are

(DEFVAR %TXTOP #o4000)        ;Top key
;; These are useless.
;; (DEFVAR %TXSFL 2000)     ;Shift lock key
;; (DEFVAR %TXSFT 1000)     ;Shift key
(DEFVAR %TXMTA  #o400)      ;Meta key
(DEFVAR %TXCTL  #o200)      ;Control key
(DEFVAR %TXASC  #o177)      ;Ascii part of character

;; Enable supdup connections to this machine.

(DEFUN SUPDUP-LISTEN ()
  (ADD-INITIALIZATION "SUPDUP"
                      '(PROCESS-RUN-FUNCTION "Supdup server" #'SERVE-SUPDUP)
                      NIL
                      'CHAOS:SERVER-ALIST))

;; For debugging

(DEFVAR SUPSER-PROCESSES NIL)
(DEFVAR SUPSER-STREAMS NIL)

;; This is the top level function of the server process.

(DEFUN SERVE-SUPDUP (&AUX (CONN (CHAOS:LISTEN "SUPDUP")))
  (COND ((CHAOS:WAIT CONN 'CHAOS:LISTENING-STATE SUPDUP-TIMEOUT "Supdup wait")
         (COND ((EQ (CHAOS:STATE CONN) 'CHAOS:RFC-RECEIVED-STATE)
                (CHAOS:ACCEPT CONN)
                (CONDITION-CASE ()
                    (UNWIND-PROTECT
                        (*CATCH 'EXIT (SUPDUP-INITIALIZE CONN))
                      (CHAOS:CLOSE-CONN CONN))
                  (SI:REMOTE-NETWORK-ERROR)))
               (T (CHAOS:CLOSE-CONN CONN
                                    (FORMAT NIL "Connection went into ~S after listening."
                                            (CHAOS:STATE CONN))))))
        (T (CHAOS:CLOSE-CONN CONN "Timeout on Listen."))))


;; Take an open connection and be a Supdup server.  Build a chaos stream on top
;; of the connection, and build a supdup server stream on top of the chaos stream.
;; This used to bind RUBOUT-HANDLER to NIL in the current stack group, but I don't
;; think this is necessary.

(DEFUN SUPDUP-INITIALIZE (CONN &AUX CHAOS-STREAM SUPDUP-STREAM W-O EDITOR-STREAM)
  (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN "SUPDUP")
  (SETQ CHAOS-STREAM (CHAOS:MAKE-STREAM CONN))
  ;; This is apparently a part of the supdup protocol
  (FUNCALL CHAOS-STREAM ':STRING-OUT (CHAOS:HOST-DATA CHAOS:MY-ADDRESS))
  (FUNCALL CHAOS-STREAM ':TYO #o15)
  (FUNCALL CHAOS-STREAM ':TYO #o12)
  (FUNCALL CHAOS-STREAM ':TYO %TDNOP)
  (FUNCALL CHAOS-STREAM ':FORCE-OUTPUT)
  (SETQ SUPDUP-STREAM (MAKE-SUPDUP-STREAM CHAOS-STREAM))
  (SETQ W-O (FUNCALL SUPDUP-STREAM ':WHICH-OPERATIONS))
  (SETQ EDITOR-STREAM
        (MAKE-EDITOR-STREAM SUPDUP-STREAM
                            (COND ((MEMQ ':SET-CURSORPOS W-O) :DISPLAY-EDITOR)
                                  (T :PRINTING-EDITOR))))
  (PROCESS-SLEEP 120.)
  (FUNCALL EDITOR-STREAM ':CLEAR-SCREEN)
  (PRINT-HERALD EDITOR-STREAM)
  (FUNCALL EDITOR-STREAM ':TYO #\RETURN)
  (FUNCALL EDITOR-STREAM ':FORCE-OUTPUT)
  ;; For debugging
  (PUSH EDITOR-STREAM SUPSER-STREAMS)
  (PUSH CURRENT-PROCESS SUPSER-PROCESSES)
  ;; note: Should set up a watchdog process to look for asyncronoush characters
  ;; and also to do :FORCE-OUTPUT every once in a while
  (SUPDUP-LISP-TOPLEVEL EDITOR-STREAM))


(DEFUN SUPDUP-LISP-TOPLEVEL (STREAM)
  ;; SI:LISP-TOP-LEVEL1, BINDS *TERMINAL-IO*, WHICH IS DANGEROUS (ERROR ENTERING ERROR HANDLER) UNTIL THIS GETS DEBUGGED.
  (LET ((*STANDARD-INPUT* STREAM)
        (*STANDARD-OUTPUT* STREAM)
        (*PACKAGE* (FIND-PACKAGE "USER"))
        (*READTABLE* (SI:FIND-READTABLE-NAMED "CL"))
        (*QUERY-IO* STREAM))
    (DO ((FORM))
        (NIL)
      (SETQ FORM (PROMPT-AND-READ :READ "~&FORM> "))
      (SEND STREAM :FORCE-OUTPUT)
      (PRIN1 (EVAL FORM)))))

(DEFUN 18BIT-IN (STREAM)
    (DPB (FUNCALL STREAM ':TYI)
         #o1406
         (DPB (FUNCALL STREAM ':TYI) #o0606 (FUNCALL STREAM ':TYI))))

(DEFUN RECEIVE-TTY-VARIABLES (CHAOS-STREAM)
    (DO ((COUNT (LOGIOR -1_18. (PROG1 (18BIT-IN CHAOS-STREAM)
                                      (18BIT-IN CHAOS-STREAM))) (1+ COUNT))
         (L NIL))
        (NIL)
      (IF (= COUNT 0) (RETURN-LIST (NREVERSE L)))
      (PUSH (DPB (18BIT-IN CHAOS-STREAM) #o2222 (18BIT-IN CHAOS-STREAM)) L)))



;; MAKE-SUPDUP-STREAM takes a chaos stream and returns a stream which looks
;; something like a pc-ppr stream.  It can understand the Lisp Machine
;; character set and does the appropriate conversions.  The supdup stream
;; returned is designed to sit behind the editor stream in MC:SCE;EDITST
;; and thus doesn't handle the :UNTYI or :RUBOUT-HANDLER messages.
;; Should precede these variable names with SS- or something.

(SPECIAL SUPDUP-CHAOS-STREAM SUPDUP-WHICH-OPERATIONS
         MORE-PROCESSING-FLAG MORE-PROCESSING-IN-PROGRESS MORE-PROCESSING-LINE
         TCTYPE TTYOPT HEIGHT WIDTH TTYROL SMARTS ISPEED OSPEED
         XPOS YPOS SUPDUP-FINGER-STRING
         META-BITS-SEEN)

;; Terminal capabilities should be checked for by looking on the :WHICH-OPERATIONS
;; list.  The stream operations assume that if called, the terminal must have
;; that capability.  This may be the wrong thing for simple programs, but higher
;; level programs know best how to approximate missing capabilities.

;; Some approximations are made here in the output of sail characters and input of
;; control and meta characters, but Lisp Machine code shouldn't stoop that low,
;; and there's now way to check for the capability via :WHICH-OPERATIONS.

(DEFUN MAKE-SUPDUP-STREAM (SUPDUP-CHAOS-STREAM)
  (MULTIPLE-VALUE-BIND (TCTYPE TTYOPT HEIGHT WIDTH TTYROL SMARTS ISPEED OSPEED)
      (RECEIVE-TTY-VARIABLES SUPDUP-CHAOS-STREAM)
    (LET ((SUPDUP-FINGER-STRING NIL)
          (SUPDUP-WHICH-OPERATIONS '(:TYI :TYI-NO-HANG :LISTEN :TYO :FRESH-LINE :BEEP))
          (XPOS 0)
          (YPOS 0)
          (MORE-PROCESSING-FLAG T)
          (MORE-PROCESSING-IN-PROGRESS NIL)
          (MORE-PROCESSING-LINE (1- HEIGHT))
          (META-BITS-SEEN NIL))
      ;; Set the WHICH-OPERATIONS parameter of the supdup stream according
      ;; to the terminal capabilities.
      (IF (BIT-TEST %TOMVU TTYOPT)
          (SETQ SUPDUP-WHICH-OPERATIONS (APPEND '(:TRIGGER-MORE :READ-CURSORPOS :SET-CURSORPOS
                                                                :SET-CURSORPOS-RELATIVE :HOME-CURSOR
                                                                :COMPUTE-MOTION :CURSOR-MOTION)
                                                SUPDUP-WHICH-OPERATIONS)))
      (IF (BIT-TEST %TOCID TTYOPT)
          (SETQ SUPDUP-WHICH-OPERATIONS (APPEND '(:INSERT-CHAR :DELETE-CHAR) SUPDUP-WHICH-OPERATIONS)))
      (IF (BIT-TEST %TOLID TTYOPT)
          (SETQ SUPDUP-WHICH-OPERATIONS (APPEND '(:INSERT-LINE :DELETE-LINE) SUPDUP-WHICH-OPERATIONS)))
      (IF (BIT-TEST (+ %TOCID %TOLID) TTYOPT)
          (SETQ SUPDUP-WHICH-OPERATIONS (APPEND '(:INSERT-STRING :DELETE-STRING) SUPDUP-WHICH-OPERATIONS)))
      (IF (BIT-TEST %TOOVR TTYOPT)
          (SETQ SUPDUP-WHICH-OPERATIONS (APPEND '(:UNDERLINE) SUPDUP-WHICH-OPERATIONS)))

      (CLOSURE '(TCTYPE TTYOPT HEIGHT WIDTH TTYROL SMARTS ISPEED OSPEED
                        XPOS YPOS MORE-PROCESSING-FLAG MORE-PROCESSING-IN-PROGRESS
                        MORE-PROCESSING-LINE META-BITS-SEEN
                        SUPDUP-CHAOS-STREAM SUPDUP-FINGER-STRING SUPDUP-WHICH-OPERATIONS)
               #'SUPDUP-STREAM))))

(DEFMACRO SUPSER-RAW-TYO (CHAR)
    `(FUNCALL SUPDUP-CHAOS-STREAM ':TYO ,CHAR))

(DEFMACRO SUPSER-RAW-TYI ()
    `(FUNCALL SUPDUP-CHAOS-STREAM ':TYI))

;; Add the :STRING-OUT and :LINE-OUT operations to this stream when CHAOS:STREAM
;; understands them.
;; Use the :WHICH-OPERATIONS list to transmit the capabilities of the terminal.
;; The third argument in the DEFSELECT function specification means
;; that we're handling :WHICH-OPERATIONS ourselves.

(DEFUN SUPDUP-STREAM (OPERATION &REST L)
  (LET ((F (GET OPERATION 'SUPDUP-STREAM)))
    (COND (F
           (APPLY F L))
          ('ELSE
           (STREAM-DEFAULT-HANDLER #'SUPDUP-STREAM OPERATION (CAR L) (CDR L))))))

(DEFUN (:WHICH-OPERATIONS SUPDUP-STREAM) () SUPDUP-WHICH-OPERATIONS)

;; Input operations
(DEFUN (:TYI SUPDUP-STREAM)         () (SUPSER-TYI))
(DEFUN (:TYI-NO-HANG SUPDUP-STREAM) () (SUPSER-TYI-NO-HANG))
(DEFUN (:LISTEN SUPDUP-STREAM)  () (FUNCALL SUPDUP-CHAOS-STREAM ':LISTEN))

;; Figure out how to make this work.  Probably have to hack CHAOS:STREAM.
;; (:CLEAR-INPUT)

;; Output operations
(DEFUN (:TYO SUPDUP-STREAM) (CHAR) (SUPSER-TYO CHAR))
;; If at the beginning of a line, then clear to end of line.  Otherwise,
;; go down to the next line.
(DEFUN (:FRESH-LINE SUPDUP-STREAM) ()
    (IF (ZEROP XPOS)
        (SUPSER-RAW-TYO %TDEOL)
        (SUPDUP-STREAM ':TYO #\RETURN)))

(DEFUN (:TRIGGER-MORE SUPDUP-STREAM) (&OPTIONAL ARG1) (SETQ MORE-PROCESSING-FLAG ARG1))
(DEFUN (:FORCE-OUTPUT SUPDUP-STREAM) () (FUNCALL SUPDUP-CHAOS-STREAM ':FORCE-OUTPUT))
  ;; Ignore frequency and duration args for now.
(DEFUN (:BEEP SUPDUP-STREAM) (&OPTIONAL IGNORE IGNORE) (SUPSER-RAW-TYO %TDBEL))

(DEFUN (:READ-CURSORPOS SUPDUP-STREAM) (&OPTIONAL (UNIT ':CHARACTER))
  (SELECTQ UNIT
    (:CHARACTER (VALUES XPOS YPOS))
    (OTHERWISE
     ;;(FERROR NIL "~S is not a known unit." UNIT)
     ;; I KNOW THAT THE ERROR HANDLER NOW DOES THIS. SO RETURN NIL
     NIL)))


(DEFUN (:SET-CURSORPOS SUPDUP-STREAM) (X Y &OPTIONAL (UNIT ':CHARACTER))
  (SELECTQ UNIT
    (:CHARACTER (SUPSER-SET-CURSORPOS X Y))
    (OTHERWISE (FERROR NIL "~S is not a known unit." UNIT))))

(DEFUN (:SET-CURSORPOS-RELATIVE SUPDUP-STREAM) (X Y &OPTIONAL (UNIT ':CHARACTER))
  (SELECTQ UNIT
    (:CHARACTER (SUPSER-SET-CURSORPOS-RELATIVE X Y))
    (OTHERWISE (FERROR NIL "~S is not a known unit." UNIT))))

(DEFUN (:HOME-CURSOR SUPDUP-STREAM) () (SUPSER-SET-CURSORPOS 0 0))

(DEFUN (:COMPUTE-MOTION SUPDUP-STREAM) (X-POS Y-POS STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
  (SUPSER-COMPUTE-MOTION X-POS Y-POS STRING BEGIN END))

(DEFUN (:CURSOR-MOTION SUPDUP-STREAM) (X-POS Y-POS STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
  (MULTIPLE-VALUE-BIND (X Y)
      (SUPSER-COMPUTE-MOTION X-POS Y-POS STRING BEGIN END)
    (SUPSER-SET-CURSORPOS X Y)))

;; These entries shouldn't be called unless the terminal can insert and
;; delete characters.  It is up to the calling function to look at the
;; :WHICH-OPERATIONS list.  :INSERT-CHAR will be present if %TOCID is set.
;; :INSERT-STRING and :DELETE-STRING currently assume that the string contains
;; no newlines.

(DEFUN (:INSERT-CHAR SUPDUP-STREAM) (&OPTIONAL (CHAR #\SPACE) (COUNT 1))
    (SUPSER-RAW-TYO %TDICP)
    (SUPSER-RAW-TYO (+ #/0 COUNT))
    (DOTIMES (I COUNT) (SUPDUP-STREAM ':TYO CHAR)))

(DEFUN (:INSERT-STRING SUPDUP-STREAM) (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    (SUPSER-RAW-TYO %TDICP)
    (SUPSER-RAW-TYO (+ #/0 (- END BEGIN)))
    (SUPDUP-STREAM ':STRING-OUT STRING BEGIN END))

(DEFUN (:DELETE-CHAR SUPDUP-STREAM) (&OPTIONAL (CHAR #\SPACE) (COUNT 1))
    CHAR ;Ignored, since only fixed width fonts now.
    (SUPSER-RAW-TYO %TDDCP)
    (SUPSER-RAW-TYO (+ #/0 COUNT)))

(DEFUN (:DELETE-STRING SUPDUP-STREAM) (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
    STRING ;Ignored, since only fixed width fonts now.
    (SUPSER-RAW-TYO %TDDCP)
    (SUPSER-RAW-TYO (- END BEGIN)))

(DEFUN (:CLEAR-SCREEN SUPDUP-STREAM) ()
    (SUPSER-RAW-TYO %TDCLR)
    (SETQ XPOS 0 YPOS 0))

(DEFUN (:CLEAR-EOL SUPDUP-STREAM) ()
    (SUPSER-RAW-TYO %TDEOL))

  ;; Clear to end of page -- who uses this?  Why not %TDEOF?
(DEFUN (:CLEAR-EOC SUPDUP-STREAM) ()
    (DO ((I YPOS (1+ I)))
        ((= I HEIGHT) (SUPSER-SET-CURSORPOS XPOS YPOS))
      (SUPSER-RAW-TYO %TDCRL)))

  ;; Operations particular to this type of stream
(DEFUN (:CONSOLE-LOCATION SUPDUP-STREAM)   () SUPDUP-FINGER-STRING)
(DEFUN (:TTYOPT SUPDUP-STREAM)         () TTYOPT)
(DEFUN (:SIZE-IN-CHARACTERS SUPDUP-STREAM) () (VALUES WIDTH HEIGHT))
(DEFUN (:CLOSE SUPDUP-STREAM)          () (FUNCALL SUPDUP-CHAOS-STREAM ':CLOSE))




;; Input methods of supdup server streams.

;; The tyi hook checks for control and meta characters if necessary.
;; Note that the checking for the tyi hook belongs in the editor stream.
;; If it were in the editor stream, it could go to the end of the input buffer
;; before printing "Quit".  In the new window system, presumably there will be
;; a process stuffing characters into an IO-BUFFER.

;; User can set this to change control/meta prefixes.

(DEFVAR SUPSER-TYI-HOOK 'DEFAULT-SUPSER-TYI-HOOK)

(DEFUN DEFAULT-SUPSER-TYI-HOOK (CHAR OP)
  (COND ((= CHAR #\BREAK) (BREAK "BREAK") NIL)
        ((OR (= CHAR #/Z) (= CHAR #/Z))
         (PRINC "Z Quit")
         (SIGNAL 'SI:ABORT NIL))
        ;; 12-bit keyboard available
        ((BIT-TEST %TOFCI TTYOPT) CHAR)
        ;; Ascii keyboard.  Accept C-B as break.
        ((= CHAR #/B) (BREAK "BREAK") NIL)
        ;; C-^ is control prefix,  is meta prefix, C-C is control-meta prefix.
        ;; Any prefix typed twice transmits the prefix directly.  This doesn't
        ;; address the entire character set from Ascii, but its good enough for now.
        ((= CHAR #/^)
         (SETQ CHAR (FUNCALL SUPDUP-CHAOS-STREAM OP))
         (IF (= CHAR #/^) #/^ (DPB 1 %%KBD-CONTROL CHAR)))
        ((= CHAR #/)
         (SETQ CHAR (FUNCALL SUPDUP-CHAOS-STREAM OP))
         (IF (= CHAR #/) #/ (DPB 1 %%KBD-META CHAR)))
        ((= CHAR #/C)
         (SETQ CHAR (FUNCALL SUPDUP-CHAOS-STREAM OP))
         (IF (= CHAR #/C) #/C (DPB 3 %%KBD-CONTROL-META CHAR)))
        (T CHAR)))

(DEFUN SUPSER-TYI () (SUPSER-TYI-CHECK-HOOK ':TYI))
(DEFUN SUPSER-TYI-NO-HANG () (SUPSER-TYI-CHECK-HOOK ':TYI-NO-HANG))

(DEFUN SUPSER-TYI-CHECK-HOOK (OP &AUX CHAR)
  (DO () (NIL)
    (SETQ CHAR (SUPSER-TYI-LM-CHAR OP))
    ;; Update idle time
    ;;(SETQ SI:KBD-LAST-ACTIVITY-TIME (TIME))
    ;; If :TYI-NO-HANG and no input available, return immediately.
    (IF (NULL CHAR) (RETURN NIL))
    ;; Otherwise, let the tyi-hook have it and loop until it decides to
    ;; return something.
    (SETQ CHAR (FUNCALL SUPSER-TYI-HOOK CHAR OP))
    (IF CHAR (RETURN CHAR))))

;; Reads a single character in Lisp Machine format.
;; Interprets the intelligent terminal protocol, and assumes that it is on all
;; the time.  C-\ (34) is the command escape.

(DEFUN SUPSER-TYI-LM-CHAR (OP &AUX CHAR)
  (DO () (NIL)
    (SETQ CHAR (FUNCALL SUPDUP-CHAOS-STREAM OP))
    (COND ((NULL CHAR) (RETURN))
          (( CHAR #O300) (SUPDUP-ESCAPE CHAR))
          (( CHAR #O34) (RETURN))
          (T (SETQ CHAR (FUNCALL SUPDUP-CHAOS-STREAM ':TYI))
             (COND
               ;; C-\ C-C means the screens messed up.  Ignore for now and try again.
               ;; Hack output reset and C-\ C-P later.
               ((= CHAR 3))
               ;; Transmitting C-\ twice sends one through.
               ((= CHAR #O34) (RETURN))
               ;; The low five bits of characters @ through _ transmit meta bits.
               (( CHAR #/@)
                (SETQ CHAR (DPB (LOGAND CHAR 37) 0705 (FUNCALL SUPDUP-CHAOS-STREAM ':TYI)))
                (RETURN))
               ;; Bomb out on anything else.
               (T (FERROR NIL "Unimplemented intelligent terminal code -- ~C" CHAR))))))
  ;; If no input available, return nil.  If we have a character, then translate
  ;; according to character set type.
  (COND ((NULL CHAR) NIL)
        ((BIT-TEST %TOFCI TTYOPT) (12-BIT-TO-LM-CHAR CHAR))
        (T (ASCII-TO-LM-CHAR CHAR))))

;; Convert C-M to RETURN, C-H to BS, etc. as special cases since it is most likely
;; that the user typed RETURN and BS keys on his keyboard.  Don't convert VT to
;; C-K since it is unlikely that a VT key is present.

(DEFUN ASCII-TO-LM-CHAR (CHAR)
  "Convert a character in the Ascii character set to one in the Lisp Machine
character set.  This function does not make it possible to address all of the
Lisp Machine characters from Ascii.  That is up to the individual program."
  (COND ((= CHAR #O33)   #\ALT)
        ((= CHAR #O177)  #\RUBOUT)
        ((= CHAR #O10)   #\BS)
        ((= CHAR #O11)   #\TAB)
        ((= CHAR #O12)   #\LINE)
        ((= CHAR #O13)   #\RETURN)
        ;; Can be accessed by typing C-_ H from an Ascii terminal
        ((= CHAR #O4110) #\HELP)
        ;; C-A (not C-a) through C-Z, C-\, C-], C-^, C-_
        ;; Algorithm:  turn  into B, then turn on control bit.
        ((< CHAR #\SPACE) (DPB 1 %%KBD-CONTROL (+ #O100 CHAR)))
        (T CHAR)))

;; ITS representation is confusing -- some examples:
;; BS is 10 (Ascii Control-H), Control-H is %TXCTL+110 (Ascii H),
;;  Lambda is %TXTOP+10 [NOT %TXTOP+V!] and HELP is %TXTOP+110 (4110).
;; CALL is 32 (Ascii Control-Z), Control-Z is %TXCTL+132 (Ascii Z),
;;  Not-Equal is %TXTOP+32, Control-CALL is %TXCTL+32,
;;  and Control-Not-Equal is %TXCTL+%TXTOP+32.
;; See page 153 of Lisp Machine manual for more info.

;; Note that it is impossible to distinguish Back-Next from Control-_
;; in this representation.

(DEFUN 12-BIT-TO-LM-CHAR (CHAR &AUX ASC TOP)
  (SETQ TOP (BIT-TEST %TXTOP CHAR))
  (SETQ ASC (LOGAND %TXASC CHAR))
  (SETQ ASC
        (COND (TOP (COND ((< ASC #\SPACE) ASC)
                         ((= ASC #/A) #\ESC)
                         ((= ASC #/B) #\BREAK)
                         ((= ASC #/C) #\CLEAR)
                         ((= ASC #/H) #\HELP)
                         (T (FERROR NIL "Random character with Top bit set -- ~D (~O)"
                                    ASC ASC))))
              (T (COND
                   ;; BS, TAB, LINE, VT, FORM, RETURN
                   ((AND ( ASC 8) ( ASC #O15))
                    (+ ASC #O200))
                   ((= ASC #O32) #\CALL)
                   ((= ASC #O177) #\RUBOUT)
                   (T ASC)))))
  (DPB (LDB #O702 CHAR) #O1002 ASC))


;; Output methods of supdup server streams.
;; They assume to be running inside of the stream so that all the specials are bound.
;; Many are analagous to the TV- functions in LMIO;TV and LMIO;TVWARM
;; and try to follow the same naming conventions.

;; Printing characters which have special widths.  Needed only for
;; terminals which can't hack multiple fonts.

(DEFVAR SUPSER-MAGIC-CHAR-LIST
        '((#\BREAK     . "<BREAK>")
          (#\CLEAR     . "<CLEAR>")
          (#\CALL      . "<CALL>")
          (#\ESC       . "<ESC>")
          (#\BACK-NEXT . "<BACKNEXT>")
          (#\HELP      . "<HELP>")
          (#\RUBOUT    . "<RUBOUT>")
          (#\LINE      . "<LINE>")
          (#\VT        . "<VT>")
          (#\FORM      . "<FORM>")))

;; Printing characters which aren't in the ASCII character set.  Needed only
;; for terminals which don't have the full Sail character set.
;;  is translated to ^ for echoing control characters.  This is a loss.  The
;; :TYO operation should be applied directly to the control character and the
;; stream should figure out how to echo it.

(DEFVAR SUPSER-NON-ASCII-CHAR-LIST
        '((#/ . "^")
          (#/ . "<-")
          (#/ . "->")
          (#/ . "<=")
          (#/ . ">=")))

(DEFUN SUPSER-TYO (CHAR &AUX TEMP)
  (COND
    ;; Standard graphic characters.  Check for these early to speed the
    ;; most frequent case.
    ((< CHAR #O200)
     (COND
       ;; Sail console -- this can print anything.
       ((BIT-TEST %TOSAI TTYOPT)
        (SUPSER-TYO-CHECK-EXCEPTIONS CHAR))
       ;; Not in the Ascii character set.  Translate to something printable.
       ((SETQ TEMP (ASSQ CHAR SUPSER-NON-ASCII-CHAR-LIST))
        (SUPSER-STRING-OUT (CDR TEMP)))
       ;; Standard Ascii graphic.
       (T (SUPSER-TYO-CHECK-EXCEPTIONS CHAR))))
    ;; "Control" characters of one sort or another.  Ideally, these should
    ;; be stream operations and characters should be printing only.
    ((= CHAR #\BS)     (IF (> XPOS 0) (SUPSER-SET-CURSORPOS (1- XPOS) YPOS)))
    ((= CHAR #\TAB)    (SUPSER-SET-CURSORPOS (* (// (+ XPOS 8) 8) 8) YPOS))
    ((= CHAR #\RETURN) (SUPSER-CRLF))
    ;; Magic graphic characters.  When the protocol exists, check here if the console
    ;; can hack it.
    ((< CHAR #O240)
     (SETQ TEMP (ASSQ CHAR SUPSER-MAGIC-CHAR-LIST))
     (IF TEMP (SUPSER-STRING-OUT (CDR TEMP))))
    ;; Font change character.  Ignore for now.
    ((< CHAR #O250))
    (T (FERROR NIL "Random character -- ~O" CHAR))))

;; This guy triggers line continuation and more processing.
;; Make this print ! or something at the end of the line.

(DEFUN SUPSER-TYO-CHECK-EXCEPTIONS (CHAR)
  (IF (AND (= XPOS 0)
           (= YPOS MORE-PROCESSING-LINE)
           MORE-PROCESSING-FLAG
           (NOT MORE-PROCESSING-IN-PROGRESS))
      (HANDLE-MORE-INTERRUPT))
  (INCF XPOS)
  (COND (( XPOS WIDTH)
         (SUPSER-CRLF)
         (SUPSER-TYO-CHECK-EXCEPTIONS CHAR))
        (T (SUPSER-RAW-TYO CHAR))))

(DEFUN SUPSER-CHAR-WIDTH (CHAR &OPTIONAL (X XPOS) &AUX TEMP)
  "Returns the width of a character, in character units.
For backspace, it can return a negative number.
For tab, the number returned depends on second arg or the current cursor position.
For return, the result is zero."
  (COND
    ;; Standard graphic
    ((< CHAR #O200)
     (COND
       ;; Sail console
       ((BIT-TEST %TOSAI TTYOPT) 1)
       ;; Non-Ascii graphic
       ((SETQ TEMP (ASSQ CHAR SUPSER-NON-ASCII-CHAR-LIST))
        (STRING-LENGTH (CDR TEMP)))
       ;; Standard Ascii graphic
       (T 1)))
    ;; Control character
    ((= CHAR #\BS) -1)
    ((= CHAR #\TAB) (- (* (// (+ X 8) 8) 8) X))
    ((= CHAR #\RETURN) 0)
    ;; Magic graphic character
    ((< CHAR #O240)
     (SETQ TEMP (ASSQ CHAR SUPSER-MAGIC-CHAR-LIST))
     (IF TEMP (STRING-LENGTH (CDR TEMP)) 0))
    ;; Font change character
    ((< CHAR #O250) 0)
    (T (FERROR NIL "Random character -- ~O" CHAR))))

;; Worry about making this efficient later.

(DEFUN SUPSER-STRING-OUT (STRING &OPTIONAL (BEGIN 0) (END (STRING-LENGTH STRING)))
  (DO I BEGIN (1+ I) (= I END)
      (SUPSER-TYO (AREF STRING I))))

;; This guy triggers more processing.

(DEFUN SUPSER-CRLF ()
    (SETQ XPOS 0)
    (SETQ YPOS (\ (1+ YPOS) HEIGHT))
    (COND (( YPOS 0) (SUPSER-RAW-TYO %TDCRL))
          (T (SUPSER-SET-CURSORPOS XPOS YPOS)
             (SUPSER-RAW-TYO %TDEOL))))

;; Handles line and screen wrap-around.

(DEFUN SUPSER-SET-CURSORPOS (X Y)
    (SETQ XPOS (\ X WIDTH))
    (SETQ YPOS (\ Y HEIGHT))
    (SUPSER-RAW-TYO %TDMV0)
    (SUPSER-RAW-TYO YPOS)
    (SUPSER-RAW-TYO XPOS))

(DEFUN SUPSER-SET-CURSORPOS-RELATIVE (X Y)
  (SUPSER-SET-CURSORPOS (+ XPOS X) (+ YPOS Y)))

;; Use MORE-PROCESSING-IN-PROGRESS flag to avoid recursion.
;; This should be handled higher up.

(DEFUN HANDLE-MORE-INTERRUPT (&AUX CHAR (MORE-PROCESSING-IN-PROGRESS T))
  (SUPSER-STRING-OUT ':STRING-OUT "--More--")
  (SETQ CHAR (SUPSER-RAW-TYI))
  (IF ( CHAR #\SPACE)
      (FUNCALL SUPDUP-CHAOS-STREAM ':UNTYI CHAR))
  ;; Clear out the --More--, home cursor up, and clear the top line.
  (SUPSER-SET-CURSORPOS 0 YPOS)
  (SUPSER-RAW-TYO %TDEOL)
  (SUPSER-SET-CURSORPOS 0 0)
  (SUPSER-RAW-TYO %TDEOL))

;; Assume fixed width fonts for now.  Add hair for going off end of line
;; and end of page later.  Fix special var kludges later.

(DEFUN SUPSER-COMPUTE-MOTION (X Y STRING BEGIN END &AUX CHAR)
    (IF (NULL X) (SETQ X XPOS))
    (IF (NULL Y) (SETQ Y YPOS))
    (DO ((I BEGIN (1+ I)))
        ((= I END))
      (SETQ CHAR (AREF STRING I))
      (COND ((= CHAR #\RETURN) (SETQ X 0) (INCF Y))
            (T (SETQ X (SUPSER-CHAR-WIDTH CHAR X)))))
    (VALUES X Y))


(defun supdup-escape (char)
  (selectq char
    (#O301 (do ((i 0 (add1 i))) ((= i 4) nil) (tv:COMPLEMENT-BOW-MODE))
           (*throw 'EXIT nil))
    (#O302 (receive-finger-string))))

(defun receive-finger-string ()
  (setq supdup-finger-string
        (do ((s (make-array 100 :TYPE 'art-string :FILL-POINTER 0))
             (ch (supser-raw-tyi) (supser-raw-tyi)))
            ((zerop ch) s)
          (array-push-extend s ch))))

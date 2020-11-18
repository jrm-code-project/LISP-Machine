;;; -*- Mode:LISP; Package:KERMIT; Syntax:COMMON-LISP; Base:10 -*-

;;; ITS KERMIT - a Rule-Based Expert System

; Authorship Information:
;      Mark David (LMI)           Original version, using KERMIT.C as a guide
;      George Carrette (LMI)      Various enhancements
;      Mark Ahlstrom (Honeywell)  Port to 3600 (edits marked with ";1;")
;      Jonathan Rees (MIT)        Almost total rewrite for ITS

;;; [It ain't really an expert system.  That was an attempt at humor.]

;;; Don't make any attempt to understand this code without having access
;;; to the Kermit Protocol Manual, Frank Da Cruz, Columbia University,
;;; 5th edition.

;;; In an attempt to allow this code to outlive its usefulness on the
;;; ITS machines, I've tried to make the code as close to Common Lisp as
;;; seems reasonable, without going overboard.  So a port to some
;;; Common Lisp implementation should be relatively straightforward.

;;;+ Bugs
;;; What to do about the control-C's at the end of ITS files?
;;; There's apparently no way to distinguish them from legitimate control-C's.
;;; Right now they're interpreted exactly the same as end of file.
;;;
;;; The receive packet size rightly ought to be about 93 or so, but
;;; for some reason can't RECEIVE packets bigger than about 64 bytes.
;;; For the life of me I can't figure out why.  Debug this some day.
;;;  [And due to a bug in Kermit-9000, I have set the packet size down
;;;  to 50 (probably should be 32) to try to make SURE...]
;;;-

#+maclisp
(EVAL-WHEN (EVAL LOAD COMPILE)
  (OR (GET 'COMMON 'LOADEDP) (LOAD '((DSK MATH) COMMON))))
#+maclisp
(FORMAT NIL "T")                                       Make sure FORMAT is loaded
#+maclisp
(DECLARE (*LEXPR SEND-ERROR-PACKET INIT-SERIAL-I/O STRING-APPEND MERGE-PATHNAMES))
#+maclisp
(DECLARE (FIXNUM (SERIAL-WRITE-CHAR FIXNUM)) (FIXNUM (SERIAL-READ-CHAR)))
#+maclisp
(SETQ GC-OVERFLOW NIL)

;;; Fundamental constants
(DEFCONSTANT *SOH*     1    "start of header")
(DEFCONSTANT *MAX-PACKET-SIZE* (- 128. 32.) "maximum packet size")

;;; The following three values merely need to answer true to FAILP
;;; and not be legal packet types.
(DEFCONSTANT *TIMED-OUT* 0)
(DEFCONSTANT *LENGTH-ERROR* 1)
(DEFCONSTANT *CHECKSUM-ERROR* 2)

;;; Global parameters
(DEFVAR *IMAGE?*          NIL "T means 8-bit mode")
(DEFVAR *DEBUG?*          NIL "T means supply debugging info as you run")
(DEFVAR *VERSION-NUMBERS?* T  "T means include version numbers in file names")
(DEFVAR *MAX-RETRY-COUNT* 10. "times to retry a packet")
(DEFVAR *MIN-TIMEOUT*     2   "minimum timeout interval in seconds")
(DEFVAR *SERVER-TIMEOUT*  30  "timeout interval when in server mode")
(DEFVAR *SERIAL-INPUT*    NIL "input stream for serial line")
(DEFVAR *SERIAL-OUTPUT*   NIL "output stream for serial line")
(DEFVAR *INTERACTION-STREAM* *TERMINAL-IO* "stream for messages to user")

;;; State machine variables
(DEFVAR *SERVER-MODE* NIL "true if in server mode")
(DEFVAR *FILE-TRANSFER-LIST* '() "list of source/dest filename pairs")
(DEFVAR *COMMAND* NIL "generic or other command to execute")
(DEFVAR *SEQUENCE-NUMBER* 0 "the packet number")
(DEFVAR *RETRY-COUNT* 0 "times this packet retried")
(DEFVAR *STREAM*  NIL "file pointer for current disk file")
(DEFVAR *DISCARD* NIL "T if current file is to be discarded")

;;; Packet reception parameters (see KPM page 26):
(DEFVAR *RECEIVE-PACKET-SIZE* 50 "maximum packet size")  ;see bug remark, above
(DEFVAR *MY-TIMEOUT*  10   "seconds after which I time out")
(DEFVAR *MY-PAD*      0    "number of padding characters I need")
(DEFVAR *MY-PAD-CHAR* 0    "char I want as a padding char")
(DEFVAR *MY-EOL*      #o15 "my kind of return char")
(DEFVAR *MY-QUOTE*    #\#  "my quote character")

;;; Packet transmission parameters:
(DEFVAR *SEND-PACKET-SIZE* 80 "maximum send packet size")       ;MAXL
(DEFVAR *TIMEOUT*  15   "timeout for remote host on sends")     ;TIME
(DEFVAR *PAD*      0    "how much padding to send")             ;NPAD
(DEFVAR *PAD-CHAR* 0    "padding character to send")            ;PADC
(DEFVAR *EOL*      #o15 "end-of-line character to send")        ;EOL
(DEFVAR *QUOTE*    #\#  "quote character in outgoing data")     ;QCTL

;;; Packet buffers, globally allocated for no very good reason
(DEFVAR *RECEIVE-PACKET*
  (MAKE-ARRAY *MAX-PACKET-SIZE* :ELEMENT-TYPE 'CHARACTER :FILL-POINTER 0)
  "receive packet buffer")

(DEFVAR *SEND-PACKET*
  (MAKE-ARRAY *MAX-PACKET-SIZE* :ELEMENT-TYPE 'CHARACTER :FILL-POINTER 0)
  "send packet buffer")

;;; Utility routines & macros

(DEFMACRO MESSAGE (&REST CRUFT)
  `(IF (OR (NOT *SERVER-MODE*) *DEBUG?*)
       (FORMAT *INTERACTION-STREAM* ,@CRUFT)))

(DEFMACRO DEBUG-MESSAGE (&REST CRUFT)
  `(IF *DEBUG?*
       (FORMAT *DEBUG-IO* ,@CRUFT)))

;;; TOCHAR: converts a control character to a printable one by adding a space.
;;; UNCHAR: undoes tochar.
;;; CTL:    converts between control characters and printable characters by
;;;         toggling the control bit (i.e. ^a becomes a and a becomes ^a).

(DEFSUBST TOCHAR (CH)
  (+ CH #\SPACE))

(DEFSUBST UNCHAR (CH)
  (- CH #\SPACE))

(DEFSUBST CTL (CH)
  (LOGXOR CH #o100))

(DEFSUBST COMPUTE-FINAL-CHECKSUM (CHKSUM)
  (LOGAND (+ (ASH (LOGAND CHKSUM #o0300) -6) CHKSUM) #o077))

;;; ACKP, NACKP, ERRP, FAILP
;;; Predicates applied to packet type returned by RECEIVE-PACKET:
;;; an ACK (Y), a NACK (N), an ERRORMESSAGE (E), or a failed
;;; packet transmission (timeout, bad checksum, or illegal length).

(DEFSUBST ACKP (TYPE)
  (CHAR= TYPE #\Y))

(DEFSUBST NACKP (TYPE)
  (CHAR= TYPE #\N))

(DEFSUBST ERRP (TYPE)
  (CHAR= TYPE #\E))

(DEFSUBST FAILP (TYPE)
  (CHAR< TYPE #\SPACE))

;;; Top-level entry routines

;;; Server mode:

(DEFUN SERVE ()                         ;See LIBDOC; TTY >
  (FORMAT *INTERACTION-STREAM* "~&Entering KERMIT server mode.~%")
  (LET ((STATUS (RUN-STATE-MACHINE #'RECEIVE-SERVER-IDLE T '() NIL)))
    (IF (EQ STATUS 'LOGOUT) (QUIT) STATUS)))

;;; Send a file:

(DEFUN SEND-FILE (LOCAL-FILESPEC &OPTIONAL REMOTE-FILESPEC)
  (LET ((LOCAL-FILE-LIST (FILES-MATCHING LOCAL-FILESPEC)))
    (COND ((NULL LOCAL-FILE-LIST)
           (MESSAGE "~&No files match this specification: ~S" LOCAL-FILESPEC))
          (T
           (RUN-STATE-MACHINE #'SEND-INIT
                              NIL
                              (MAKE-TRANSFER-LIST LOCAL-FILE-LIST
                                                  REMOTE-FILESPEC)
                              NIL)))))

;;; Get a file:
;;; --- This routine is untested.  Use at your own risk. ---
;;; [What about sending parameters?  We ought to do an I / Y packet exchange!]

(DEFUN GET-FILE (REMOTE-SPEC &OPTIONAL LOCAL-SPEC)
  (RUN-STATE-MACHINE #'SEND-MISC
                     NIL
                     (LIST (CONS REMOTE-SPEC (OR LOCAL-SPEC (LOCALIZE REMOTE-SPEC))))
                     'GET))

;;; Tell remote server to go away:

(DEFUN FINISH ()
  (RUN-STATE-MACHINE #'SEND-MISC NIL '() 'FINISH))

(DEFUN BYE ()
  (RUN-STATE-MACHINE #'SEND-MISC NIL '() 'BYE))

;;; The KERMIT state machine

(DEFUN RUN-STATE-MACHINE (STATE *SERVER-MODE* *FILE-TRANSFER-LIST* *COMMAND*)
  (IF (NULL *SERIAL-INPUT*) (INIT-SERIAL-I/O))
  (LET ((TEMP (SERIAL-STATE)))
    (UNWIND-PROTECT
      (PROGN (DISABLE-ECHOING TEMP)
             (FLUSH-INPUT)
             (*CATCH 'DONE
               (DO ()
                   (NIL)
                 (*CATCH 'ABORT
                   (UNWIND-PROTECT
                     (LET ((*SEQUENCE-NUMBER* 0)
                           (*RETRY-COUNT* 0)
                           (*DISCARD* NIL)
                           (*TIMEOUT* *TIMEOUT*))
                       (DO ((STATE STATE (FUNCALL STATE)))
                           (NIL)
                         (DEBUG-MESSAGE "~&State = ~S~&" STATE)))
                     (COND (*STREAM*
                            (CLOSE *STREAM*)
                            (SETQ *STREAM* NIL)))))
                 (IF (NOT *SERVER-MODE*) (RETURN 'ABORTED)))))
      (SET-SERIAL-STATE TEMP))))

(DEFMACRO DEFSTATE (NAME &REST BODY)
  `(DEFUN ,NAME () ,@BODY))

(DEFMACRO GOTO-STATE (STATE)
  `#',STATE)

(DEFUN ABORT ()
  (DEBUG-MESSAGE "~&Aborting.")
  (*THROW 'ABORT NIL))

(DEFUN COMPLETE ()
  (IF *SERVER-MODE*
      (GOTO-STATE RECEIVE-SERVER-IDLE)
      (*THROW 'DONE 'COMPLETE)))

(DEFSUBST NEXT-SEQUENCE-NUMBER ()
  (LOGAND (1+ *SEQUENCE-NUMBER*) #o77))

(DEFSUBST PREVIOUS-SEQUENCE-NUMBER ()
  (LOGAND (+ *SEQUENCE-NUMBER* #o77) #o77))

(DEFUN BUMP-SEQUENCE-NUMBER ()
  (SETQ *SEQUENCE-NUMBER* (NEXT-SEQUENCE-NUMBER))
  (SETQ *RETRY-COUNT* 0))

(DEFUN BUMP-RETRY-COUNT ()
  (INCF *RETRY-COUNT*)
  (COND ((> *RETRY-COUNT* *MAX-RETRY-COUNT*)
         (SEND-ERROR-PACKET "aborting after ~D retries" *MAX-RETRY-COUNT*)
         (ABORT))))

;;; Rec_Server_Idle -- Server idle, waiting for a message

(DEFSTATE RECEIVE-SERVER-IDLE
  (SETQ *SEQUENCE-NUMBER* 0)
  (SETQ *TIMEOUT* *SERVER-TIMEOUT*)  ;will get reset later
  (MULTIPLE-VALUE-BIND (TYPE NUM DATA)
                       (RECEIVE-PACKET)
    (COND ((FAILP TYPE)
           (SEND-NACK 0)
           (GOTO-STATE RECEIVE-SERVER-IDLE))
          ((NOT (= NUM 0))
           (SEND-ERROR-PACKET "bad sequence number - ~S" NUM))
          ((CHAR= TYPE #\I)
           ;; Initialize & send ACK
           (INITIALIZE-PARAMETERS DATA)
           (GOTO-STATE RECEIVE-SERVER-IDLE))
          ((CHAR= TYPE #\S)
           ;; Remote host wants to send a file; we should prepare to receive.
           (INITIALIZE-PARAMETERS DATA)
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          ((CHAR= TYPE #\R)
           ;; Remote host wants to receive a file, i.e. we should send it.
           (LET* ((FILESPEC (UNKLUDGIFY-STRING DATA))
                  (XFER-LIST (FILES-MATCHING FILESPEC)))
             (COND ((NULL XFER-LIST)
                    (SEND-ERROR-PACKET "no files matching this specification")
                    (GOTO-STATE RECEIVE-SERVER-IDLE))
                   (T
                    (SETQ *FILE-TRANSFER-LIST*
                          (MAKE-TRANSFER-LIST XFER-LIST FILESPEC))
                    (DEBUG-MESSAGE "~&Files to send: ~S"
                                   *FILE-TRANSFER-LIST*)
                    (GOTO-STATE SEND-INIT)))))
          ((CHAR= TYPE #\G)
           (COND ((< (STRING-LENGTH DATA) 1)
                  (SEND-ERROR-PACKET "ill-formed generic command packet")
                  (GOTO-STATE RECEIVE-SERVER-IDLE))
                 ((CHAR= (STRING-ELT DATA 0) #\F)
                  (SEND-ACK *SEQUENCE-NUMBER*)
                  (*THROW 'DONE 'FINISHED))
                 ((CHAR= (STRING-ELT DATA 0) #\L)
                  (SEND-ACK *SEQUENCE-NUMBER*)
                  (*THROW 'DONE 'LOGOUT))
                 ;; Unimplemented: Cwd, Directory, diskUsage,
                 ;; dEletefile, Type, Rename, Kopy, Whoisloggedin,
                 ;; sendMessage, runProgram, Journal, Variable
                 (T
                  (SEND-ERROR-PACKET "unimplemented generic command - ~A"
                                     (UNKLUDGIFY-STRING DATA))
                  (GOTO-STATE RECEIVE-SERVER-IDLE))))
          (T
           (SEND-ERROR-PACKET "unimplemented server command - ~C" TYPE)
           (GOTO-STATE RECEIVE-SERVER-IDLE)))))

;;; Rec_Init - entry point for non-server RECEIVE command (pretty useless)

(DEFSTATE RECEIVE-INIT
  (SETQ *SEQUENCE-NUMBER* 0)
  (MULTIPLE-VALUE-BIND (TYPE NUM DATA)
                       (RECEIVE-PACKET)
    (COND ((AND (CHAR= TYPE #\S) (= NUM 0))
           (INITIALIZE-PARAMETERS DATA)
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          ((FAILP TYPE)
           (SEND-NACK 0)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE RECEIVE-INIT))
          (T
           (SEND-NACK 0)
           (ABORT)))))

;;; Rec_File -- look for a file header or EOT message

(DEFSTATE RECEIVE-FILE-HEADER
  (MULTIPLE-VALUE-BIND (TYPE NUM PACKET)
                       (RECEIVE-PACKET)
    (COND ((AND (CHAR= TYPE #\F) (= NUM *SEQUENCE-NUMBER*))
           ;; File header; ignore it if this is a GET command.
           (IF (NULL *FILE-TRANSFER-LIST*)
               (LET ((REMOTE (UNKLUDGIFY-STRING PACKET)))
                 (PUSH (CONS REMOTE (PATHNAME REMOTE)) *FILE-TRANSFER-LIST*)))
           (LET ((LOCAL-PATHNAME (CDAR *FILE-TRANSFER-LIST*)))
             (COND ((SETQ *STREAM* (MAYBE-OPEN-FOR-OUTPUT LOCAL-PATHNAME))
                    (MESSAGE "~&Receiving ~A as ~A"
                             (CAAR *FILE-TRANSFER-LIST*)
                             (NAMESTRING (PATHNAME->NAMELIST LOCAL-PATHNAME)))
                    (SEND-ACK NUM)
                    (BUMP-SEQUENCE-NUMBER)
                    (GOTO-STATE RECEIVE-DATA))
                   (T
                    (MESSAGE "~&Can't create ~S" LOCAL-PATHNAME)
                    (SEND-ERROR-PACKET "Can't create ~S" LOCAL-PATHNAME)
                    (ABORT)))))
          ((AND (CHAR= TYPE #\B) (= NUM *SEQUENCE-NUMBER*))
           ;; End of transmission
           (SEND-ACK NUM)
           (COMPLETE))
          ((AND (CHAR= TYPE #\S) (= NUM (PREVIOUS-SEQUENCE-NUMBER)))
           ;; Send init
           (SEND-PARAMETERS #\Y NUM)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          ((AND (CHAR= TYPE #\Z) (= NUM (PREVIOUS-SEQUENCE-NUMBER)))
           ;; End of file
           (SEND-ACK NUM)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          ((FAILP TYPE)
           ;; Timeout
           (SEND-NACK *SEQUENCE-NUMBER*)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          (T
           (SEND-ERROR-PACKET "error receiving file header")
           (ABORT)))))

;;; Rec_Data - Receive data up to end of file

(DEFSTATE RECEIVE-DATA
  (MULTIPLE-VALUE-BIND (TYPE NUM DATA)
                       (RECEIVE-PACKET)
    (COND ((AND (CHAR= TYPE #\D) (= NUM *SEQUENCE-NUMBER*))
           (EMPTY-BUFFER DATA)
           (SEND-ACK NUM)
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE RECEIVE-DATA))
          ((AND (OR (CHAR= TYPE #\D) (CHAR= TYPE #\F))
                (= NUM (PREVIOUS-SEQUENCE-NUMBER)))
           (SEND-ACK NUM)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE RECEIVE-DATA))
          ((AND (CHAR= TYPE #\Z) (= NUM *SEQUENCE-NUMBER*))
           (LET ((FILENAME (PATHNAME->NAMELIST (CDAR *FILE-TRANSFER-LIST*)))
                 (IO-LOSSAGE #'(LAMBDA IGNORE
                                 (SEND-ERROR-PACKET "error closing file")
                                 (ABORT))))
             (COND ((OR (< (STRING-LENGTH DATA) 1)
                        (NOT (CHAR= (STRING-ELT DATA 0) #\D)))
                    (RENAMEF *STREAM* FILENAME)
                    (MESSAGE "~&File received successfully: ~A"
                             (NAMESTRING FILENAME)))
                   (T
                    ;; Discard
                    (DELETEF *STREAM*)
                    (MESSAGE "~&File discarded: ~A" (NAMESTRING FILENAME))))
             (SEND-ACK NUM)
             (SETQ *STREAM* NIL)
             (POP *FILE-TRANSFER-LIST*)
             (BUMP-SEQUENCE-NUMBER)
             (GOTO-STATE RECEIVE-FILE-HEADER)))
          ((FAILP TYPE)
           (SEND-NACK *SEQUENCE-NUMBER*)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE RECEIVE-DATA))
          (T
           (IF (NOT (ERRP TYPE))
               (SEND-ERROR-PACKET "error during data reception"))
           (ABORT)))))

;;; Send_Init - entry for SEND command

(DEFSTATE SEND-INIT
  (SETQ *SEQUENCE-NUMBER* 0)
  (SEND-PARAMETERS #\S 0)
  (MULTIPLE-VALUE-BIND (REPLY NUM PACKET)
                       (RECEIVE-PACKET)
    (COND ((AND (ACKP REPLY) (= *SEQUENCE-NUMBER* NUM))
           (SETQ *EOL* 0 *QUOTE* 0)     ;Initialize questionable params.
           (DECODE-PARAMETERS PACKET)           ;Check and set defaults
           (IF (ZEROP *EOL*) (SETQ *EOL* *MY-EOL*))
           (IF (ZEROP *QUOTE*) (SETQ *QUOTE* *MY-QUOTE*))
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE OPEN-FILE))
          ((ERRP REPLY)
           (ABORT))
          (T
           (GOTO-STATE SEND-INIT)))))

;;; Open_File

(DEFSTATE OPEN-FILE
  (COND ((NULL *FILE-TRANSFER-LIST*)
         (GOTO-STATE SEND-BREAK))
        (T
         (LET ((LOCAL-FILENAME (CAAR *FILE-TRANSFER-LIST*)))
           (COND ((NOT (SETQ *STREAM* (MAYBE-OPEN-FOR-INPUT LOCAL-FILENAME)))
                  ;; This ain't right, but what's to do for it??
                  (MESSAGE "~&Can't open file ~A" LOCAL-FILENAME)
                  (POP *FILE-TRANSFER-LIST*)
                  (SETQ *DISCARD* T)
                  (GOTO-STATE SEND-EOF))
                 (T
                  (GOTO-STATE SEND-FILE-HEADER)))))))

;;; Send_File

(DEFUN NORMAL-ACKP (TYPE NUM)
  (OR (AND (ACKP TYPE) (= NUM *SEQUENCE-NUMBER*))
      (AND (NACKP TYPE) (= NUM (NEXT-SEQUENCE-NUMBER)))))

(DEFUN NORMAL-NACKP (TYPE)
  (OR (FAILP TYPE)
      (NACKP TYPE)))

(DEFSTATE SEND-FILE-HEADER
  (SEND-PACKET #\F *SEQUENCE-NUMBER*
               (KERMITIFY-PATHNAME (CDAR *FILE-TRANSFER-LIST*)
                                   *VERSION-NUMBERS?*))
  (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE)
                       (RECEIVE-PACKET)
    (COND ((NORMAL-ACKP REPLY NUM)
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE SEND-DATA))
          ((NORMAL-NACKP REPLY)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE SEND-FILE-HEADER))
          (T
           (IF (NOT (ERRP REPLY))
               (SEND-ERROR-PACKET "error sending file header"))
           (ABORT)))))

;;; Send_Data -- Send contents of file

(DEFSTATE SEND-DATA
  (IF (= *RETRY-COUNT* 0)
      (FILL-BUFFER *SEND-PACKET*))
  (COND ((= (STRING-LENGTH *SEND-PACKET*) 0)
         (GOTO-STATE SEND-EOF))
        (T
         (SEND-PACKET #\D *SEQUENCE-NUMBER* *SEND-PACKET*)
         (MULTIPLE-VALUE-BIND (REPLY NUM PACKET)
                              (RECEIVE-PACKET)
           (COND ((NORMAL-ACKP REPLY NUM)
                  (BUMP-SEQUENCE-NUMBER)
                  (COND ((AND (> (STRING-LENGTH PACKET) 0)
                              (OR (CHAR= (STRING-ELT PACKET 0) #\X)
                                  (CHAR= (STRING-ELT PACKET 0) #\Z)))
                         (SETQ *DISCARD* T)
                         (GOTO-STATE SEND-EOF))
                        (T
                         (GOTO-STATE SEND-DATA))))
                 ((NORMAL-NACKP REPLY)
                  (BUMP-RETRY-COUNT)
                  (GOTO-STATE SEND-DATA))
                 (T
                  (IF (NOT (ERRP REPLY))
                      (SEND-ERROR-PACKET "error during data transmission"))
                  (ABORT)))))))

;;; Send_EOF -- Send end of file indicator

(DEFSTATE SEND-EOF
  (SEND-PACKET #\Z *SEQUENCE-NUMBER* (IF *DISCARD* "Z" NIL))
  (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE)
                       (RECEIVE-PACKET)
    (COND ((NORMAL-ACKP REPLY NUM)
           (MESSAGE "~&File sent successfully: ~A~%"
                    (TRUENAME *STREAM*))
           (CLOSE *STREAM*)
           (SETQ *STREAM* NIL)
           (POP *FILE-TRANSFER-LIST*)
           (BUMP-SEQUENCE-NUMBER)
           (SETQ *DISCARD* NIL)
           (GOTO-STATE OPEN-FILE))
          ((NORMAL-NACKP REPLY)
           (GOTO-STATE SEND-EOF))
          (T
           (SETQ *DISCARD* NIL)
           (IF (NOT (ERRP REPLY))
               (SEND-ERROR-PACKET "error during EOF transmission"))
           (ABORT)))))

;;; Send_Break - End of transaction

(DEFSTATE SEND-BREAK
  (SEND-PACKET #\B *SEQUENCE-NUMBER* NIL)
  (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE)
                       (RECEIVE-PACKET)
    (COND ((OR (AND (ACKP REPLY) (= NUM *SEQUENCE-NUMBER*))
               (AND (NACKP REPLY) (= NUM 0)))
           (COMPLETE))
          ((OR (AND (NACKP REPLY) (= NUM *SEQUENCE-NUMBER*))
               (FAILP REPLY))
           (GOTO-STATE SEND-BREAK))
          (T
           (IF (NOT (ERRP TYPE))
               (SEND-ERROR-PACKET "error during break transmission"))
           (ABORT)))))

;;; Send_Gen_Cmd - Command to server

(DEFSTATE SEND-MISC
  (CASEQ *COMMAND*
    ((FINISH) (SEND-PACKET #\G 0 "F"))
    ((LOGOUT) (SEND-PACKET #\G 0 "L"))
    ((GET)
     (SEND-PACKET #\R 0 (KERMITIFY-PATHNAME (CDAR *FILE-TRANSFER-LIST*)
                                            *VERSION-NUMBERS?*)))
    (T (ERROR "weird command in SEND-MISC" *COMMAND* 'FAIL-ACT)))
  (MULTIPLE-VALUE-BIND (TYPE NUM DATA)
                       (RECEIVE-PACKET)
    (COND ((AND (ACKP TYPE) (= NUM 0))
           (MESSAGE "~&OK")
           (COMPLETE))
          ((AND (CHAR= TYPE #\S) (= NUM 0) (>= (STRING-LENGTH DATA) 6))
           (DECODE-PARAMETERS DATA)
           (SEND-PARAMETERS #\Y 0)
           (BUMP-SEQUENCE-NUMBER)
           (GOTO-STATE RECEIVE-FILE-HEADER))
          ((NORMAL-NACKP TYPE)
           (BUMP-RETRY-COUNT)
           (GOTO-STATE SEND-MISC))
          (T
           (IF (NOT (ERRP TYPE))
               (SEND-ERROR-PACKET "error during command transmission"))
           (ABORT)))))

;;; Packet sending and receiving

(DEFUN SEND-ERROR-PACKET (&REST CRUFT)
  (LET* ((FROB (LEXPR-FUNCALL #'FORMAT NIL CRUFT))
         (FROB (FORMAT NIL "Kermit-ITS: ~A" FROB)))
    (DEBUG-MESSAGE "~&Sending error packet: ~A" FROB)
    (SEND-PACKET #\E *SEQUENCE-NUMBER* FROB)))

(DEFUN SEND-ACK (NUM)
  (SEND-PACKET #\Y NUM NIL))

(DEFUN SEND-NACK (NUM)
  (SEND-PACKET #\N NUM NIL))

;;; Remember remote parameters and ackowledge with ours

(DEFUN INITIALIZE-PARAMETERS (DATA)
  (DECODE-PARAMETERS DATA)
  (SEND-PARAMETERS #\Y *SEQUENCE-NUMBER*))

(DEFUN DECODE-PARAMETERS (DATA)
  (SETQ *SEND-PACKET-SIZE* (UNCHAR (STRING-ELT DATA 0)))
  (SETQ *TIMEOUT*          (UNCHAR (STRING-ELT DATA 1)))
  (SETQ *PAD*              (UNCHAR (STRING-ELT DATA 2)))
  (SETQ *PAD-CHAR*            (CTL (STRING-ELT DATA 3)))
  (SETQ *EOL*              (UNCHAR (STRING-ELT DATA 4)))
  (SETQ *QUOTE*                    (STRING-ELT DATA 5)))

(DEFUN SEND-PARAMETERS (TYPE NUM)
  (LET ((PACKET *SEND-PACKET*))
    (SET-STRING-ELT PACKET 0 (TOCHAR *RECEIVE-PACKET-SIZE*))
    (SET-STRING-ELT PACKET 1 (TOCHAR *MY-TIMEOUT*))
    (SET-STRING-ELT PACKET 2 (TOCHAR *MY-PAD*))
    (SET-STRING-ELT PACKET 3 (CTL *MY-PAD-CHAR*))
    (SET-STRING-ELT PACKET 4 (TOCHAR *MY-EOL*))
    (SET-STRING-ELT PACKET 5 *MY-QUOTE*)
    (SETF (FILL-POINTER PACKET) 6)
    (SEND-PACKET TYPE NUM PACKET)))


;;; SEND-PACKET
;;;  TYPE -- a number, the type of packet this is.
;;;  NUM  -- a number, the packet-number of this packet.
;;;  DATA -- a string, i.e. an art-string type of array, the data of this pkt.

(DEFUN SEND-PACKET (TYPE NUM DATA)
  (LET ((CHKSUM 0)
        (LEN (IF DATA (STRING-LENGTH DATA) 0)))
    (DECLARE (FIXNUM CHKSUM LEN))
    (DEBUG-MESSAGE "~&Sent packet:     type = ~3C, num = ~3D~&  Data = ~A"
                   TYPE NUM (IF DATA (UNKLUDGIFY-STRING DATA) ""))
    (DO ((I 0 (1+ I)))
        ((>= I *PAD*))
      (SERIAL-WRITE-CHAR *PAD-CHAR*))
    (SERIAL-WRITE-CHAR *SOH*)
    (INCF CHKSUM (SERIAL-WRITE-CHAR (TOCHAR (+ LEN 3))))
    (INCF CHKSUM (SERIAL-WRITE-CHAR (TOCHAR NUM)))
    (INCF CHKSUM (SERIAL-WRITE-CHAR TYPE))
    (IF DATA
        (DO ((I 0 (1+ I)))
            ((>= I LEN))
          (DECLARE (FIXNUM I))
          (INCF CHKSUM (SERIAL-WRITE-CHAR (STRING-ELT DATA I)))))
    (SERIAL-WRITE-CHAR (TOCHAR (COMPUTE-FINAL-CHECKSUM CHKSUM)))
    (SERIAL-WRITE-CHAR *EOL*)
    (FORCE-OUTPUT *SERIAL-OUTPUT*)))

;;; RECEIVE-PACKET
;;;  Values returned are, in order:
;;;  TYPE -- a character (fixnum), in {#\A, #\S, ...}.  E.g. #\A means "abort".
;;;  NUM  -- a number, the packet-number of this packet.
;;;  DATA -- a string, the data of this packet, which is as many
;;;             characters as appropriate/desired for this type of packet.
;;; Many callers need only one (usually the type) value.

(DEFUN RECEIVE-PACKET ()
  (*CATCH 'TIMEOUT
    (UNWIND-PROTECT
      (PROGN
        (ALARMCLOCK 'TIME (MAX *TIMEOUT* *MIN-TIMEOUT*))
        (REALLY-RECEIVE-PACKET))
      (ALARMCLOCK 'TIME NIL))))

(DEFUN ALARMCLOCK-HANDLER (IGNORE)
  (DEBUG-MESSAGE "~&RECEIVE-PACKET timed out")
  (*THROW 'TIMEOUT *TIMED-OUT*))

(SETQ ALARMCLOCK #'ALARMCLOCK-HANDLER)

(DEFUN REALLY-RECEIVE-PACKET ()
  (LET ((CH 0) (TYPE 0) (I 0) (CCHKSUM 0) (RCHKSUM 0) (LEN 0) (NUM 0)
        (DATA *RECEIVE-PACKET*))
    (DECLARE (FIXNUM CH TYPE I CCHKSUM RCHKSUM LEN NUM))
    (PROG ()
     CONTINUE

      (SETQ CH (SERIAL-READ-CHAR))
      (COND ((NOT (CHAR= CH *SOH*))
             (GO CONTINUE)))

     (SETQ CH (SERIAL-READ-CHAR))
     (IF (CHAR= CH *SOH*) (GO CONTINUE))
     (SETQ CCHKSUM CH)                                  ;OK, start checksum
     (SETQ LEN (- (UNCHAR CH) 3))                       ;Get character count
     ;; (if *debug?* (format t "<~d>" len))

     (COND ((OR (< LEN 0) (> LEN (- *MAX-PACKET-SIZE* 3)))
            (DEBUG-MESSAGE "~&RECEIVE-PACKET got illegal packet length: ~D"
                           LEN)
            (RETURN (VALUES *LENGTH-ERROR* NUM DATA))))

     (SETQ CH (SERIAL-READ-CHAR))
     (IF (CHAR= CH *SOH*) (GO CONTINUE))
     (INCF CCHKSUM CH)                                  ;OK, update checksum
     (SETQ NUM (UNCHAR CH))                             ;Get packet number

     (SETQ CH (SERIAL-READ-CHAR))
     (IF (CHAR= CH *SOH*) (GO CONTINUE))
     (INCF CCHKSUM CH)                                  ;OK, update checksum
     (SETQ TYPE CH)                                     ;Get packet type

      (SETQ I 0)
     READ-THE-PACKET
      (COND ((< I LEN)
             (SETQ CH (SERIAL-READ-CHAR))
             (IF (CHAR= CH *SOH*) (GO CONTINUE))
             (INCF CCHKSUM CH)                          ;OK, update checksum
             (SET-STRING-ELT DATA I CH)
             (INCF I)
             (GO READ-THE-PACKET)))

      (SET-STRING-ELT DATA LEN 0)                       ;Mark end of data
      (SETF (FILL-POINTER DATA) LEN)

     (SETQ CH (SERIAL-READ-CHAR))
     (SETQ RCHKSUM (UNCHAR CH))                         ;OK, get checksum

     (SETQ CH (SERIAL-READ-CHAR))
     (IF (CHAR= CH *SOH*) (GO CONTINUE))        ;OK, get eol char and toss it
                                                        ;Safe!
      (DEBUG-MESSAGE "~&Received packet: type = ~3C, num = ~3D, len = ~3D~%  Data = ~A"
                     TYPE NUM LEN (UNKLUDGIFY-STRING DATA))

     (SETQ CCHKSUM (COMPUTE-FINAL-CHECKSUM CCHKSUM))

     (COND ((NOT (EQUAL CCHKSUM RCHKSUM))
            (DEBUG-MESSAGE "RECEIVE-PACKET received bad checksum (expected ~A, got ~A)"
                           RCHKSUM CCHKSUM)
            ;; Corruption, oh no!
            (RETURN (VALUES *CHECKSUM-ERROR* NUM DATA)))
           (T
            ;; Else checksum ok, 'uncorrupted'.
            (IF (ERRP TYPE)
                (MESSAGE "~&Aborting with following error from remote host:~%  ~S~%"
                         (UNKLUDGIFY-STRING DATA)))
            (RETURN (VALUES TYPE NUM DATA)))))))


;;; Serial I/O

(DEFUN INIT-SERIAL-I/O (&OPTIONAL (TTYNAME 'TTY))
  (IF *SERIAL-INPUT* (TERMINATE-SERIAL-I/O))
  (SETQ *SERIAL-INPUT* (OPEN `((,TTYNAME)) '(TTY SINGLE IMAGE IN)))
  (SETQ *SERIAL-OUTPUT* (OPEN `((,TTYNAME)) '(TTY BLOCK IMAGE OUT))))

(DEFUN TERMINATE-SERIAL-I/O ()
  (CLOSE *SERIAL-INPUT*)
  (CLOSE *SERIAL-OUTPUT*)
  (SETQ *SERIAL-INPUT* NIL *SERIAL-OUTPUT* NIL))

(DEFUN SERIAL-STATE ()
  (SYSCALL 3. 'TTYGET *SERIAL-INPUT*))

(DEFUN SET-SERIAL-STATE (TEMP)
  (SYSCALL 0. 'TTYSET *SERIAL-INPUT* (CAR  TEMP) (CADR TEMP)))

(DEFUN DISABLE-ECHOING (TEMP)
  (SYSCALL 0. 'TTYSET *SERIAL-INPUT*
           (LOGAND (CAR  TEMP) #o070707070707)
           (LOGAND (CADR TEMP) #o070707070707)))

;(DEFUN ENABLE-ECHOING (TEMP)
;  (SYSCALL 0. 'TTYSET *SERIAL-INPUT*
;          (LOGIOR (CAR  TEMP) #o202020202020)
;          (LOGIOR (CADR TEMP) #o202020200020)))

(DEFUN SERIAL-READ-CHAR ()
  (LET ((CH (+TYI *SERIAL-INPUT*)))
    (IF *IMAGE?* CH (LOGAND CH #o177))))

(DEFUN SERIAL-WRITE-CHAR (CH)
  (+TYO CH *SERIAL-OUTPUT*)
  CH)

(DEFUN FLUSH-INPUT ()
  (CLEAR-INPUT *SERIAL-INPUT*))

;;; File I/O

;;; Put a pathname into Kermit standard form.

(DEFUN KERMITIFY-PATHNAME (PATHNAME VERSIONP)
  (LET* ((NAME    (STRINGIFY-COMPONENT (PATHNAME-NAME PATHNAME)))
         (TYPE    (STRINGIFY-COMPONENT (PATHNAME-TYPE PATHNAME)))
         (VERSION (STRINGIFY-COMPONENT (PATHNAME-VERSION PATHNAME))))
    (COND ((AND VERSIONP VERSION)
           (STRING-APPEND NAME "." (OR TYPE "") "." VERSION))
          (TYPE
           (STRING-APPEND NAME "." TYPE))
          (T NAME))))

(DEFUN STRINGIFY-COMPONENT (X)
  (COND ((EQ X ':WILD) "*")
        ((EQ X ':NEWEST) "0")
        ((EQ X ':OLDEST) "-1")
        ((NUMBERP X) (COERCE (EXPLODEN X) 'STRING))
        (T X)))

(DEFUN MAKE-TRANSFER-LIST (LOCAL-FILE-LIST REMOTE-FILESPEC)
  (DECLARE (SPECIAL REMOTE-FILESPEC))
  (MAPCAR #'(LAMBDA (F)
              (CONS F (MERGE-PATHNAMES REMOTE-FILESPEC F (PATHNAME-VERSION F))))
          LOCAL-FILE-LIST))

;;; Opening files...

(DEFUN MAYBE-OPEN-FOR-INPUT (FILESPEC)
  (*CATCH 'FNF
    (LET ((IO-LOSSAGE #'(LAMBDA IGNORE (*THROW 'FNF NIL))))
      (OPEN (PATHNAME->NAMELIST FILESPEC) '(IN IMAGE BLOCK)))))

(DEFUN MAYBE-OPEN-FOR-OUTPUT (FILESPEC)
  (*CATCH 'FNF
    (LET ((IO-LOSSAGE #'(LAMBDA IGNORE (*THROW 'FNF NIL))))
      (OPEN (MERGEF '((* *) _KERM_ >) (PATHNAME->NAMELIST FILESPEC)) '(OUT IMAGE BLOCK)))))

;;; Write a packet buffer into the currently open local output file.

(DEFUN EMPTY-BUFFER (BUFFER)
  "Put data from an incoming packet into a local disk file."
  (LET ((LEN (STRING-LENGTH BUFFER)))
    (DO ((I 0 (1+ I)))
        ((>= I LEN) BUFFER)
      (LET ((CH (STRING-ELT BUFFER I)))
        (DECLARE (FIXNUM CH))
        (COND ((CHAR= CH *MY-QUOTE*)
               (SETQ CH (STRING-ELT BUFFER (INCF I)))
               (IF (NOT (CHAR= (LOGAND CH #o177) *MY-QUOTE*))
                   (SETQ CH (CTL CH)))))
        (IF (NOT *IMAGE?*) (SETQ CH (LOGAND CH #o177)))
        (+TYO CH *STREAM*)))))

;;; Fill a packet buffer from the currently open local input file.
;;; Returns the number of characters stored into the packet buffer.
;;; A return value of zero means that the end of the file has been reached.

;;; There are four ways to fill a buffer:
;;; 1. kermit default: 7-bit, quote all control characters.
;;; 3. image mode: send everything through with no conversion, except for
;;;    quoting the quote character.
;;; 2, 4: excised by JAR.

(DEFUN FILL-BUFFER (BUFFER)
  "fill buffer with the outgoing data from the file *STREAM* points to.
Only control quoting is done; 8-bit and repeat count prefixes are not handled."
  ;;1; Changed 6 to 7!! See lmbugs.doc file item #14.
  (LET ((INDEX 0)
        (LIMIT (- *SEND-PACKET-SIZE* 7)))
    (DECLARE (FIXNUM LIMIT INDEX))
    (DO ()
        ((>= INDEX LIMIT))
      (LET ((C (+TYI *STREAM*)))
        (DECLARE (FIXNUM C))
        (COND ((OR (CHAR< C 0)
                   (AND (NOT *IMAGE?*) (CHAR= C #^C)))  ;???
               ;; ITS end-of-file
               (RETURN NIL))
              ((AND (CHAR>= C #\SPACE)
                    (CHAR< C (CODE-CHAR #o177))
                    (NOT (CHAR= C *QUOTE*)))
               ;; Regular character
               (SET-STRING-ELT BUFFER INDEX C)
               (INCF INDEX))
              ((AND (NOT *IMAGE?*)
                    (CHAR>= C (CODE-CHAR #o177)))
               ;; Weird character.  Don't send anything for it.
               (DEBUG-MESSAGE "~&The character ~C [~O octal] couldn't be translated to ASCII."
                              C C))
              (T (SET-STRING-ELT BUFFER INDEX *QUOTE*)
                 (INCF INDEX)
                 (SET-STRING-ELT BUFFER
                                 INDEX
                                 (IF (OR *IMAGE?* (CHAR>= C #\SPACE))
                                     C
                                     (CTL C)))
                 (INCF INDEX)))))
    (SETF (FILL-POINTER BUFFER) INDEX)
    INDEX))

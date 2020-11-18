#|| -*- Mode:LISP; Package:(TUBE GLOBAL); Base:10; Fonts:(CPTFONTB) -*-

TUBES ARE BIDRECTIONAL COMMUNICATIONS STREAMS. YOU CAN TALK
IN ONE END AND LISTEN ON THE OTHER. AKA TIN-CAN TELEPHONE.

simple utility functions for interprocess communication streams
6/28/85 17:09:39 -george carrette



                                    TUBES

 There are two functions for setting up tubes:

 (TUBE:GET-REMOTE-TUBE "<tube-name>")

  Will return all tubes of "<tube-name>" that have been opened up to this machine.

 (TUBE:OPEN-TUBE "<hostname>" "<tube-name>")

 Will open up a tube named "<tube-name>" to the host "<hostname>".


 (TUBE:REMOTE-TUBE-NAMES) returns a list of names for which TUBE:GET-REMOTE-TUBE will
  return a non-null value.

 Tubes may or may not need unique names depending on the setting of the variable

  TUBE:*REMOTE-TUBE-UNIQUE-CHECK-P*, which defaults to T. If this is T then it
  is an error to open more than one tube of a given name to a given host.

 When outputing to a TUBE, use :TYO or any of the usual IO functions such as PRINT.
 A :FORCE-OUTPUT may need to be done on a TUBE to make sure that the output gets to
 the other side (machine).

 When inputting from a TUBE, use :TYI or any of the usual IO functions such as READ.


 A demonstration/test program, TUBE-TALK.
  On machine "LAM-A" do:
    (TUBE:TUBE-TALK "test" "LAM-B")
  On machine "LAM-B" do:
    (TUBE:TUBE-TALK "test")

  This simple demo is like a telephone in that it is confusing to both talk at the same time.


 Debugging functions:
  (TUBE:PRINT-TUBES) prints out information about tubes.
  (TUBE:RESET-TUBES) closes in abort mode all tubes and removes them from the list of tubes.


||#






(DEFVAR *REMOTE-TUBES* NIL)

(ADD-INITIALIZATION "TUBE"
                    '(PROCESS-RUN-FUNCTION "TUBE SERVER" 'SETUP-REMOTE-TUBE)
                    NIL
                    'CHAOS:SERVER-ALIST)

(defvar *remote-TUBE-unique-check-p* t)

(DEFUN SETUP-REMOTE-TUBE (&AUX STREAM CONN PKT ST N)
  (SETQ STREAM (CHAOS:OPEN-STREAM NIL "TUBE"))
  (SETQ CONN (SEND STREAM :CONNECTION))
  (SETQ PKT (CHAOS:CONN-READ-PKTS CONN))
  (COND ((NULL PKT)
         (CHAOS:REJECT CONN "INTERNAL ERROR"))
        ((NOT (SETQ N (STRING-SEARCH-CHAR #\SPACE (SETQ ST (CHAOS:PKT-STRING PKT)))))
         (CHAOS:REJECT CONN "NO TUBE NAME GIVEN"))
        ((and (GET-REMOTE-TUBE (SETQ ST (string-trim " " (SUBSTRING ST N))))
              *remote-TUBE-unique-check-p*)
         (CHAOS:REJECT CONN "Already have a TUBE by that name here"))
        ('ELSE
         (PUSH (LIST ST STREAM) *REMOTE-TUBES*)
         (CHAOS:ACCEPT CONN))))


(DEFUN GET-REMOTE-TUBE (NAME)
  (if *remote-TUBE-unique-check-p*
      (CADR (ASS #'STRING-EQUAL NAME *REMOTE-TUBES*))
    (VALUES-LIST (MAPCAR #'CADR (SUBSET #'(LAMBDA (X) (STRING-EQUAL (CAR X) NAME)) *REMOTE-TUBES*)))))

(DEFUN REMOTE-TUBE-NAMES ()
  "Returns the names of all open remote tubes"
  (DO ((NAMES NIL (IF (NOT (MEM #'(LAMBDA (TN E) (STRING-EQUAL TN (CADR E))) (CAAR L) (CDR L)))
                      (CONS (CAAR L) NAMES)
                    NAMES))
       (L *REMOTE-TUBES* (CDR L)))
      ((NULL L) NAMES)))


(DEFVAR *LOCAL-TUBES* NIL)

(DEFUN OPEN-TUBE (HOST NAME)
  (CHECK-TYPE NAME STRING)
  (LET ((S (IF (EQ SI:LOCAL-HOST (SI:PARSE-HOST HOST))
               (OPEN-TUBE-TO-SELF NAME)
             (CHAOS:OPEN-STREAM HOST (STRING-APPEND "TUBE " NAME)))))
    (PUSH (LIST HOST NAME S) *LOCAL-TUBES*)
    S))

(DEFUN GET-LOCAL-TUBE (HOST NAME)
  (CADDR (CAR (MEM #'(LAMBDA (PS X)
                       (AND (EQ (SI:PARSE-HOST (CAR X))
                                PS)
                            (STRING-EQUAL NAME (CADR X))))
                   (SI:PARSE-HOST HOST)
                   *LOCAL-TUBES*))))

(DEFFLAVOR LOCAL-TUBE
           (INPUT-IO-BUFFER
            (UNTYI-CHAR NIL)
            OUTPUT-IO-BUFFER
            (OPEN T))
           ()
  :INITABLE-INSTANCE-VARIABLES)

(DEFUN MAKE-LOCAL-TUBE ()
  (LET ((I (TV:MAKE-IO-BUFFER 100))
        (O (TV:MAKE-IO-BUFFER 100)))
    (VALUES (MAKE-INSTANCE 'LOCAL-TUBE
                           :INPUT-IO-BUFFER I
                           :OUTPUT-IO-BUFFER O)
            (MAKE-INSTANCE 'LOCAL-TUBE
                           :INPUT-IO-BUFFER O
                           :OUTPUT-IO-BUFFER I))))


(DEFUN TUBE-NOT-OPEN ()
  (FERROR NIL "tube is closed for I//O operations"))

(DEFMETHOD (LOCAL-TUBE :TYI) (&OPTIONAL IGNORE)
  (OR OPEN (TUBE-NOT-OPEN))
  (COND (UNTYI-CHAR
         (PROG1 UNTYI-CHAR (SETQ UNTYI-CHAR NIL)))
        ('ELSE
         (TV:IO-BUFFER-GET INPUT-IO-BUFFER))))

(DEFMETHOD (LOCAL-TUBE :TYI-NO-HANG) ()
  (OR OPEN (TUBE-NOT-OPEN))
  (COND (UNTYI-CHAR
         (PROG1 UNTYI-CHAR (SETQ UNTYI-CHAR NIL)))
        ('ELSE
         (TV:IO-BUFFER-GET INPUT-IO-BUFFER T))))

(DEFMETHOD (LOCAL-TUBE :LISTEN) ()
  (OR OPEN (TUBE-NOT-OPEN))
  (LET ((C (SEND SELF :TYI-NO-HANG)))
    (COND ((NULL C)
           NIL)
          ('ELSE
           (SEND SELF :UNTYI C)
           T))))

(DEFMETHOD (LOCAL-TUBE :UNTYI) (C)
  (OR OPEN (TUBE-NOT-OPEN))
  (SETQ UNTYI-CHAR C))

(DEFMETHOD (LOCAL-TUBE :TYO) (C)
  (OR OPEN (TUBE-NOT-OPEN))
  (TV:IO-BUFFER-PUT OUTPUT-IO-BUFFER C))

(DEFMETHOD (LOCAL-TUBE :STRING-OUT) (&OPTIONAL ARG1 &REST ARGS)
  (OR OPEN (TUBE-NOT-OPEN))
  (STREAM-DEFAULT-HANDLER SELF :STRING-OUT ARG1 ARGS))

(DEFMETHOD (LOCAL-TUBE :CLOSE) (&OPTIONAL IGNORE)
  (SETQ OPEN NIL))

(DEFMETHOD (LOCAL-TUBE :FORCE-OUTPUT) ()
  T)

(DEFUN OPEN-TUBE-TO-SELF (NAME)
  (IF (AND *remote-TUBE-unique-check-p*
           (GET-REMOTE-TUBE NAME))
      (FERROR NIL "Already have a TUBE by that name"))
  (MULTIPLE-VALUE-BIND (A B)
      (MAKE-LOCAL-TUBE)
    (PUSH (LIST NAME B) *REMOTE-TUBES*)
    A))


(COMPILE-FLAVOR-METHODS LOCAL-TUBE)


(DEFUN PRINT-TUBES ()
  (FORMAT T "~&Tubes connected to us:~%")
  (dolist (tube *REMOTE-TUBES*)
    (format t "Tube named: ~S, Stream: ~S~%" (car tube) (cadr tube)))
  (FORMAT T "~&Tube connections we have initiated:~%")
  (dolist (tube *LOCAL-TUBES*)
    (format t "To host: ~S, tube named: ~S, Stream: ~S~%"
            (car tube) (cadr tube) (caddr tube)))

  NIL)

(DEFUN RESET-TUBES ()
  (FORMAT T "~&Tubes connected to us:~%")
  (dolist (tube *REMOTE-TUBES*)
    (format t "Tube named: ~S, Stream: ~S~%" (car tube) (cadr tube))
    (CLOSE (CADR TUBE) T)
    (SETQ *REMOTE-TUBES* (DELQ TUBE *REMOTE-TUBES*)))
  (FORMAT T "~&Tube connections we have initiated:~%")
  (dolist (tube *LOCAL-TUBES*)
    (format t "To host: ~S, tube named: ~S, Stream: ~S~%"
            (car tube) (cadr tube) (caddr tube))
    (CLOSE (CADDR TUBE) T)
    (SETQ *LOCAL-TUBES* (DELQ TUBE *LOCAL-TUBES*)))
  NIL)

(DEFUN TUBE-TALK (TUBE-NAME &OPTIONAL HOST)
  (COND ((NULL HOST)
         (TALK-ON-TUBE (OR (GET-REMOTE-TUBE TUBE-NAME)
                           (RETURN-FROM TUBE-TALK "NOBODY CONNECTED TO THAT TUBE"))))
        ('ELSE
         (TALK-ON-TUBE (OR (GET-LOCAL-TUBE HOST TUBE-NAME)
                           (OPEN-TUBE HOST TUBE-NAME))))))


(DEFUN TALK-ON-TUBE (X)
  (DO ((C))
      (NIL)
    (PROCESS-WAIT "keyboard or Tube" #'(LAMBDA (A B) (OR (SEND A :LISTEN) (send b :listen))) X TERMINAL-IO)
    (WHEN (SETQ C (SEND TERMINAL-IO :TYI-NO-HANG))
      (COND ((NOT (ZEROP (CHAR-BITS C)))
             (FORMAT TERMINAL-IO "<BITS>"))
            ('ELSE
             (SEND TERMINAL-IO :TYO C)
             (SEND X :TYO C)
             (SEND X :FORCE-OUTPUT))))
    (WHEN (SETQ C (SEND X :TYI-NO-HANG))
      (SEND TERMINAL-IO :TYO C))))


(DEFUN UNIX-TUBE-TALK (&OPTIONAL (TUBE-NAME "UNX"))
  (DO ((LINE)
       (X (or (get-remote-tube TUBE-NAME) (return-from unix-tube-talk "no unix connected"))))
      (nil)
    (setq line (read-delimited-string (tiger:ascii-code #\line) x))
    (send terminal-io :line-out line)
    (setq line (prompt-and-read :string "~&To Unix>"))
    (send x :string-out line)
    (send x :tyo (tiger:ascii-code #\line))
    (send x :force-output)))

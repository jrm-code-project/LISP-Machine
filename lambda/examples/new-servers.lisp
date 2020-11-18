#|| -*- Mode:LISP; Package:(NEWSRV GLOBAL); Fonts:(CPTFONTB); Base:10 -*-

  Copyright LISP Machine, Inc. 1985, 196
   See filename "Copyright.Text" for
  licensing and release information.

 *** THIS EXAMPLE IS OFFICIALLY BROKEN 22-Feb-86 12:33:08 -GJC

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

Some new chaosnet servers: (-GJC)
 (OPEN-REMOTE-SERIAL-STREAM "<hostname>" &optional (baud 9600))
 (REMOTE-SERIAL-STREAM-STATUS "<hostname>")

If the remote stream is stuck in serial finish then that is probably
because the data-set-ready line is not enabled or jumpered and there
is pending output, xmit buffer not empty.

 (REMOTE-MAGTAPE "<hostname>")
 (FREE-MAGTAPE-FOR-REMOTE) ; use on host if needed.

The REMOTE-MAGTAPE is an ugly hack, particularly the violation of
lexical closure abstaction in REVERSE-ENGINEER-RUBOUT-HANDLER-PARSER.
A real implementation of remote-magtape protocol subsumes this functionality,
but the example remains one of interest to remote-procedure-call fans.

||#


;; GENERIC CHAOSNET FUNCTIONS.

(DEFUN VANILLA-CHAOS-SERVER-FUNCTION (CONTACT FUNCTION REJECTP)
  (WITH-OPEN-STREAM (STREAM (CHAOS:OPEN-STREAM NIL CONTACT))
    (LET ((CONN (SEND STREAM :CONNECTION)))
      (LET ((ST (SUBSTRING (CHAOS:PKT-STRING (CHAOS:CONN-READ-PKTS CONN))
                           (STRING-LENGTH CONTACT))))
        (LET ((REJECT? (AND REJECTP (FUNCALL REJECTP ST))))
          (IF REJECT?
              (RETURN-FROM VANILLA-CHAOS-SERVER-FUNCTION
                (CHAOS:REJECT CONN REJECT?))))
        (CHAOS:ACCEPT CONN)
        (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN CONTACT)
        (CONDITION-CASE ()
            (FUNCALL FUNCTION STREAM ST)
          (SYS:REMOTE-NETWORK-ERROR NIL))))))


(DEFUN ADD-VANILLA-CHAOS-SERVER (CONTACT FUNCTION &KEY REJECTP)
  (check-type contact string)
  (ADD-INITIALIZATION CONTACT
                      `(PROCESS-RUN-FUNCTION ,(FORMAT NIL
                                                      "~A server"
                                                      contact)
                                             'vanilla-chaos-server-function
                                             ,contact
                                             ',function
                                             ',REJECTP)
                      nil
                      'CHAOS:SERVER-ALIST))


(DEFUN TEST-RUN-CHAOS-SERVER (CONTACT)
  (LET ((FORM (ASS #'STRING-EQUAL CONTACT CHAOS:SERVER-ALIST)))
    (COND ((OR (ATOM FORM) (ATOM (CADR FORM)))
           "NO SERVER")
          ((EQ (CAR (CADR FORM)) 'PROCESS-RUN-FUNCTION)
           (LET ((ARGS (MAPCAR 'EVAL (CDR (CADR FORM)))))
             (PRINT ARGS)
             (APPLY (CADR ARGS) (CDDR ARGS))))
          ('ELSE
           (PRINT (CADR FORM))
           (EVAL FORM)))))

;; REMOTE SERIAL STREAM SUPPORT

(defun open-remote-serial-stream (host &optional (baud 9600))
  (make-auto-force-output-stream
    (chaos:open-stream host
                       (format nil "SDU-SERIAL-B ~D"
                               baud))))

(add-vanilla-chaos-server "SDU-SERIAL-B"
                          'sdu-serial-b-server
                          :REJECTP 'SDU-SERIAL-B-IN-USE)

(DEFUN SDU-SERIAL-B-IN-USE (IGNORE)
  (LET ((DEVICE (SEND (FS:PARSE-PATHNAME "SDU-SERIAL-B:") :HOST)))
    (COND ((NOT (SEND DEVICE :ALLOCATE-IF-EASY))
           "device not available")
          ((SEND device :GET-LOCK NIL)
           NIL)
          ('else
           "Serial port B in use by on host"))))


(defvar *serial-b-buffer* (make-string 1000))

(defun sdu-serial-b-server (remote extra)
  (with-open-file (local "SDU-SERIAL-B:")
    (let ((baud (parse-integer extra :junk-allowed t)))
      (if baud
          (send local :set-baud-rate baud)))
    (let ((proc)
          (name "serial b remote -> local"))
      (unwind-protect
          (*catch 'remote-closed
            (setq proc (process-run-function name
                                             #'serial-b-remote->local
                                             remote local
                                             current-process))
            (do-forever
              (send remote :tyo (send local :tyi))
              (do ((n (send local :INPUT-CHARACTERS-AVAILABLE))
                   (j 0 (1+ j)))
                  ((= j n)
                   (OR (ZEROP J)
                       (send remote :string-out *serial-b-buffer* 0 J)))
                (setf (aref *serial-b-buffer* j) (send local :tyi)))
              (send remote :force-output)))
        (and proc (eq (si:process-name proc) name)
             (send proc :kill))))))

(defun serial-b-remote->local (remote local superior)
  (CONDITION-CASE ()
      (DO ((C))
          ((null (setq c (send remote :tyi))))
        (send local :tyo c))
    (SYS:REMOTE-NETWORK-ERROR NIL))
  (send superior :interrupt #'(lambda ()
                                (*throw 'remote-closed nil)))
  (si:process-wait-forever))


(add-vanilla-chaos-server "SDU-SERIAL-B-STATUS"
                          'sdu-serial-b-status-server)


(DEFUN REMOTE-SERIAL-STREAM-STATUS (host)
  (format t "~&Getting status of SDU-SERIAL-B on ~S~%" host)
  (with-open-stream (s (chaos:open-stream host "SDU-SERIAL-B-STATUS"))
    (stream-copy-until-eof s standard-output)))


(defun sdu-serial-b-status-server (standard-output ignore)
  (let ((dev (send (fs:parse-pathname "sdu-serial-b:") :host))
        (p))
    (format t "Device: ~S,~%" dev)
    (cond ((setq p (car (send dev :lock)))
           (format t "Locked by ~S state: ~A~%"
                   p (send p :whostate))))
    (si:sdu-serial-status)))

(defun make-auto-force-output-stream (substream)
  #'(lambda (op &rest args)
      (multiple-value-prog1 (LEXPR-SEND substream op args)
                            (if (memq op '(:tyo :string-out))
                                (send substream :force-output)))))

;; magnetic tape

(add-vanilla-chaos-server "MAGTAPE-FUNCTIONS"
                          'MAGTAPE-FUNCTIONS-SERVER
                          :REJECTP 'MAGTAPE-FUNCTIONS-IN-USE)

(DEFUN MAGTAPE-FUNCTIONS-IN-USE (IGNORE)
  (LET ((DEVICE (SEND (FS:PARSE-PATHNAME "HALF-INCH-TAPE:") :HOST)))
    (COND ((NOT (SEND DEVICE :ALLOCATE-IF-EASY))
           "device not available")
          ((SEND device :GET-LOCK NIL)
           (FS:TM-INIT)
           NIL)
          ('else
           "magtape in use on host"))))

(defun FREE-MAGTAPE-FOR-REMOTE ()
  (with-open-file (s "HALF-INCH-TAPE:")
    s
    "half inch tape now free"))


;; THIS IS IN FACT A GENERAL EVAL-SERVER CAPABILITY.

(DEFUN REMOTE-MAGTAPE (HOST)
  (*CATCH 'REMOTE-READ
    (WITH-OPEN-STREAM (S (CHAOS:OPEN-STREAM HOST "MAGTAPE-FUNCTIONS"))
      (DISPLAY-REMOTE-RESULT S)
      (DO ()
          ((NULL (EXECUTE-REMOTE-MAGTAPE-COMMAND
                   S
                   (PROMPT-AND-READ :READ "~&Magtape Function on ~A:"
                                    (send (send (send s :connection) :foreign-host)
                                          :name)))))))))


(DEFUN EXECUTE-REMOTE-MAGTAPE-COMMAND (S C)
  (SEND STANDARD-OUTPUT :FRESH-LINE)
  (COND ((SYMBOLP C)
         (COND ((STRING-EQUAL "HELP" C)
                (FORMAT T "~&Magtape functions allowed:~
                           ~%(FS:MT-REWIND) (FS:MT-WRITE-EOF) (FS:RESTORE-MAGTAPE)~
                           ~%(FS:COPY-DIRECTORY), (FS:MAGTAPE-LIST-FILES)~
                           ~%and the atoms HELP and EXIT~%"))
               ((STRING-EQUAL "EXIT" C)
                (RETURN-FROM EXECUTE-REMOTE-MAGTAPE-COMMAND NIL))
               ('ELSE
                (FORMAT T "~&Unknown magtape command, try HELP: ~S~%" C))))
        ((OR (ATOM C) (NOT (SYMBOLP (CAR C))))
         (FORMAT T "~&Illegal magtape command, try HELP: ~S~%" C))
        ((NOT (MEMQ (CAR C) '(FS:MT-REWIND FS:MT-WRITE-EOF FS:RESTORE-MAGTAPE
                                           FS:COPY-DIRECTORY
                                           FS:MAGTAPE-LIST-FILES
                                           INTERNAL-TEST)))
         (FORMAT T "~&Unknown magtape command, try HELP: ~S~%" C))
        ('ELSE
         (WHEN (AND (EQ (CAR C) 'FS:RESTORE-MAGTAPE)
                    (NOT (GET C :HOST)))
           (SETF (GET C :HOST) (SEND SI:LOCAL-HOST :NAME)))
         (STANDARD-REMOTE-PRINT C S)
         (SEND S :FORCE-OUTPUT)
         (DISPLAY-REMOTE-RESULT S)))
  T)

(defun internal-test (&quote form)
  (format t "~&Internal test on ~S~%" form)
  (eval form))

(DEFUN DISPLAY-REMOTE-RESULT (S)
  (DO ((C))
      ((NULL (SETQ C (SEND S :TYI)))
       (*THROW 'REMOTE-READ "CONNECTION BROKEN"))
    (COND ((AND (= C #\NETWORK) (NOT (= (SETQ C (SEND S :TYI))
                                        #\NETWORK)))
           (COND ((= C #\BREAK)
                  (RETURN))
                 ((= C #\RETURN)
                  (STANDARD-REMOTE-PRINT
                    (SEND STANDARD-INPUT :TYI)
                    S)
                  (SEND S :FORCE-OUTPUT))
                 ((= C #\O)
                  (SEND STANDARD-OUTPUT (STANDARD-REMOTE-READ S)))
                 ((= C #\F)
                  (LET ((OP (STANDARD-REMOTE-READ S))
                        (L (STANDARD-REMOTE-READ S)))
                    (STANDARD-REMOTE-PRINT
                      (MULTIPLE-VALUE-LIST (LEXPR-SEND STANDARD-OUTPUT
                                                       OP
                                                       L))
                      S))
                  (SEND S :FORCE-OUTPUT))
                 ('ELSE
                  (FUNCALL (standard-remote-read s) s))))
          ('ELSE
           (SEND STANDARD-OUTPUT :TYO C)))))


(defun standard-remote-read (stream)
  (LET ((READTABLE SI:INITIAL-READTABLE)
        (PACKAGE (FIND-PACKAGE "NEWSRV"))
        (base 10)
        (ibase 10))
    (LET ((RESULT (READ STREAM STREAM)))
      (IF (EQ RESULT STREAM)
          (*THROW 'REMOTE-READ "CONNECTION BROKEN"))
      (DO-FOREVER
        (IF (AND (SEND STREAM :LISTEN) (= #\SPACE (SEND STREAM :TYIPEEK)))
            (SEND STREAM :TYI)
          (RETURN)))
      RESULT)))

(defun standard-remote-print (obj stream)
  (IF (NULL OBJ)
      (PRINC "()" STREAM)
    (LET ((READTABLE SI:INITIAL-READTABLE)
          (PACKAGE (FIND-PACKAGE "NEWSRV"))
          (base 10)
          (ibase 10)
          (SI:PRINT-READABLY t))
      (prin1 obj stream)
      (IF (ATOM OBJ) (PRINC " " STREAM)))))

(DEFVAR *MAGTAPE-FUNCTIONS-SERVER-DEBUG* NIL)

(DEFUN MAGTAPE-FUNCTIONS-SERVER (STREAM IGNORE)
  (LET ((REMOTE-HACKING-STREAM (MAKE-INSTANCE'REMOTE-HACKING-STREAM
                                 :SUBSTREAM STREAM)))
    (LET ((*PACKAGE* (PKG-FIND-PACKAGE "NEWSRV"))
          (*STANDARD-INPUT* REMOTE-HACKING-STREAM)
          (*STANDARD-OUTPUT* REMOTE-HACKING-STREAM)
          (*ERROR-OUTPUT* REMOTE-HACKING-STREAM)
          (*QUERY-IO* REMOTE-HACKING-STREAM)
          (BASE 10)
          (IBASE 10))
      (*CATCH 'REMOTE-READ
        (WITH-OPEN-FILE (MAGTAPE "HALF-INCH-TAPE:")
          (FORMAT T "~&Remote service on ~A of ~S:~%"
                  (send si:local-host :name)
                  magtape)
          (SEND-NETBREAK STREAM)
          (DO-FOREVER
            (CONDITION-CASE-IF (NOT *MAGTAPE-FUNCTIONS-SERVER-DEBUG*) (ERR)
                (EVAL (STANDARD-REMOTE-READ STREAM))
              (SYS:REMOTE-NETWORK-ERROR
               (RETURN))
              (ERROR
               (FORMAT *ERROR-OUTPUT* "~%##ERROR>> ")
               (SEND ERR :REPORT *ERROR-OUTPUT*)))
            (SEND-NETBREAK STREAM)))))))


(DEFUN SEND-NETBREAK (STREAM)
  (SEND STREAM :TYO #\NETWORK)
  (SEND STREAM :TYO #\BREAK)
  (SEND STREAM :FORCE-OUTPUT))

(DEFFLAVOR REMOTE-HACKING-STREAM
           (SUBSTREAM
            (PEEK-CHAR NIL))
           ()
  :INITABLE-INSTANCE-VARIABLES)

(DEFMETHOD (REMOTE-HACKING-STREAM :TYI) (&REST IGNORED)
  (SEND SELF :ANY-TYI))

(DEFMETHOD (REMOTE-HACKING-STREAM :ANY-TYI) (&REST IGNORED)
  (COND (PEEK-CHAR
         (PROG1 PEEK-CHAR
                (SETQ PEEK-CHAR NIL)))
        ('ELSE
         (SEND SUBSTREAM :TYO #\NETWORK)
         (SEND SUBSTREAM :TYO #\RETURN)
         (SEND SUBSTREAM :FORCE-OUTPUT)
         (STANDARD-REMOTE-READ SUBSTREAM))))

(DEFMETHOD (REMOTE-HACKING-STREAM :TYIPEEK) ()
  (COND (PEEK-CHAR)
        ('ELSE
         (SEND SELF (SEND SELF :TYI) :UNTYI))))

(DEFMETHOD (REMOTE-HACKING-STREAM :UNTYI) (C)
  (SETQ PEEK-CHAR C))

(DEFMETHOD (REMOTE-HACKING-STREAM :TYO) (C)
  (Cond ((= C #\NETWORK)
         (SEND SUBSTREAM :TYO #\NETWORK)
         (SEND SUBSTREAM :TYO #\NETWORK))
        ((= C #\RETURN)
         (SEND SUBSTREAM :TYO #\RETURN)
         (SEND SUBSTREAM :FORCE-OUTPUT))
        ('ELSE
         (SEND SUBSTREAM :TYO C))))

(DEFMETHOD (REMOTE-HACKING-STREAM :STRING-OUT) (STRING &OPTIONAL START END)
  (OR START (SETQ START 0))
  (OR END (SETQ END (LENGTH STRING)))
  (COND ((STRING-SEARCH-CHAR #\NETWORK STRING START END)
         (DO ((J START (1+ J)))
             ((= J END))
           (SEND SELF :TYO (AREF STRING J))))
        ('ELSE
         (SEND SUBSTREAM :STRING-OUT STRING START END)
         (IF (STRING-SEARCH-CHAR #\RETURN STRING START END)
             (SEND SUBSTREAM :FORCE-OUTPUT)))))

(DEFMETHOD (REMOTE-HACKING-STREAM :FORCE-OUTPUT) ()
  (SEND SUBSTREAM :FORCE-OUTPUT))


(DEFMETHOD (REMOTE-HACKING-STREAM :CLEAR-INPUT) ()
  (SETQ PEEK-CHAR NIL)
  (SEND SELF :REMOTE-SINGLE-OP :CLEAR-INPUT)
  (SEND SUBSTREAM :CLEAR-INPUT))

(DEFMETHOD (REMOTE-HACKING-STREAM :REMOTE-SINGLE-OP) (OP)
  (SEND SUBSTREAM :TYO #\NETWORK)
  (SEND SUBSTREAM :TYO #\O)
  (STANDARD-REMOTE-PRINT OP SUBSTREAM)
  (SEND SUBSTREAM :FORCE-OUTPUT))


(DEFMETHOD (REMOTE-HACKING-STREAM :REMOTE-GENERAL-OP) (OP ARGS)
  (SEND SUBSTREAM :TYO #\NETWORK)
  (SEND SUBSTREAM :TYO #\F)
  (STANDARD-REMOTE-PRINT OP SUBSTREAM)
  (STANDARD-REMOTE-PRINT ARGS SUBSTREAM)
  (SEND SUBSTREAM :FORCE-OUTPUT)
  (VALUES-LIST (STANDARD-REMOTE-READ SUBSTREAM)))

(DEFMETHOD (REMOTE-HACKING-STREAM :REMOTE-CALL) (F)
  (SEND SUBSTREAM :TYO #\NETWORK)
  (SEND SUBSTREAM :TYO #\SPACE)
  (STANDARD-REMOTE-PRINT F SUBSTREAM)
  (SEND SUBSTREAM :FORCE-OUTPUT))

(DEFMETHOD (REMOTE-HACKING-STREAM :CLEAR-SCREEN) ()
  (SEND SELF :REMOTE-SINGLE-OP :CLEAR-SCREEN))

(DEFMETHOD (REMOTE-HACKING-STREAM :FRESH-LINE) ()
  (SEND SELF :REMOTE-SINGLE-OP :FRESH-LINE))


(DEFMETHOD (REMOTE-HACKING-STREAM :CLEAR-EOL) ()
  (SEND SELF :REMOTE-SINGLE-OP :CLEAR-EOL))


;; When we fix WITH-INPUT-EDITING to handle the case of a simple body
;; which is a single function call in the obvious way this kludge wont be needed.
;; If only the formalization in terms of a SI:READ-APPLY that handled both
;; the decoding of read arguments and rubout handler call in a more straightforward
;; function-level modularization instead of this macro-level transformation
;; things would be easier. In any case for this package to work in general
;; we most formalize our INPUT-PARSERS still further.


(DEFVAR *A-RANDOM-GLOBAL-VALUE-CELL* NIL)

(DEFMETHOD (REMOTE-HACKING-STREAM :RUBOUT-HANDLER) (OPTIONS
                                                    *A-RANDOM-GLOBAL-VALUE-CELL*
                                                    &REST ARGS)
  ;; the function entry sequence makes sure that the DTP-STACK-CLOSURE function gets
  ;; consed into the heap as a CLOSURE if the argument variable is a special cell.
  (LET ((FUNCTION *A-RANDOM-GLOBAL-VALUE-CELL*)
        (F))
    (MULTIPLE-VALUE (F ARGS) (REVERSE-ENGINEER-RUBOUT-HANDLER-PARSER
                               FUNCTION ARGS))
    (OR (AND F (SYMBOLP F))
        (FERROR NIL "can't externalize parsing function: ~S" function))
    ;; one good thing to do might be to calculate the prompt and reprompt
    ;; *now* and send that over as a string.
    ;; A GOOD REASON FOR THIS besides possible efficiency
    ;; is that in the remote process the SPECIAL VARIABLE BINDINGS
    ;; will not be in effect. This screws the use of PROMPT-AND-READ
    ;; for example.
    (SEND SELF :REMOTE-CALL 'HANDLE-REMOTE-RUBOUT-HANDLER)
    (STANDARD-REMOTE-PRINT OPTIONS SUBSTREAM)
    (STANDARD-REMOTE-PRINT F SUBSTREAM)
    (STANDARD-REMOTE-PRINT (mapcar #'(lambda (arg)
                                       (if (eq arg self)
                                           'remote-hacking-stream-self
                                         arg))
                                   ARGS)
                           substream)
    (SEND SUBSTREAM :FORCE-OUTPUT)
    (VALUES-LIST (STANDARD-REMOTE-READ SUBSTREAM))))


(DEFUN REVERSE-ENGINEER-RUBOUT-HANDLER-PARSER (FUNCTION ARGS)
  ;; I WONDER HOW YOU WOULD DO SOMETHING LIKE THIS IN CLU?
  (VALUES (TYPECASE FUNCTION
            (SYMBOL FUNCTION)
            (MICROCODE-FUNCTION (FUNCTION-NAME FUNCTION))
            (COMPILED-FUNCTION
             (LET ((NAME (FUNCTION-NAME FUNCTION)))
               (COND ((NOT (AND (NOT (ATOM NAME))
                                (EQ (CAR NAME) :INTERNAL)
                                (EQ (CADDR NAME) 'SI::.DO.IT.)))
                      NIL)
                     ((MEMQ (CADR NAME) '(EH::command-loop-read
                                           EH::com-print-variable-frame-value
                                           EH::com-eval-in-error-handler))
                      ;; ALL EASY CASES, A MERE CALL TO THIS FUNCTION:
                      'si::read-for-top-level))))
            (CLOSURE
             (LET ((NAME (FUNCTION-NAME FUNCTION)))
               (COND ((NOT (AND (NOT (ATOM NAME))
                                (EQ (CAR NAME) :INTERNAL)
                                (EQ (CADDR NAME) 'SI::.DO.IT.)))
                      NIL)
                     ((EQ (CADR NAME) 'SI::INTERNAL-READ)
                      ;; THIS WOULD BE ONE OF THE SIMPLE CASES.
                      (SETQ ARGS (CAAR (CLOSURE-BINDINGS FUNCTION)))
                      'SI::INTERNAL-READ)
                     ((EQ (CADR NAME) 'READ-DELIMITED-STRING)
                      ;; it might actually be more pleasant, certainly more modular
                      ;; to go up the stack looking for the original call
                      ;; to READ-DELIMITED-STRING
                      ;; as a way to reconstruct the original argument list.
                      ;; Even though we would have to switch to another
                      ;; stack group to do it
                      (SETQ ARGS (CAAR (CLOSURE-BINDINGS FUNCTION)))
                      'SOMEWHAT-MUNGED-READ-DELIMITED-STRING)))))
          ARGS))




(DEFPROP SOMEWHAT-MUNGED-READ-DELIMITED-STRING T SPECIALLY-MUNGED)
(DEFPROP SI:INTERNAL-READ T BACKWARD-MUNGED-ARGUMENTS)

(DEFUN SOMEWHAT-MUNGED-READ-DELIMITED-STRING (OPTIONS L &AUX
                                              BUFFER-SIZE EOF-ERROR-P STREAM DELIMITER)
  (SETF `(,BUFFER-SIZE ,EOF-ERROR-P ,STREAM ,DELIMITER) L)
  ;; THIS DEPENDS ON THE ORDER AND USAGE OF THE CLOSURE VARIABLES AND THE LAYOUT OF
  ;; CODE IN READ-DELIMITED-STRING.
  (READ-DELIMITED-STRING DELIMITER STREAM EOF-ERROR-P (CDR OPTIONS) BUFFER-SIZE))

(defun handle-remote-rubout-handler (stream)
  ;; this protocol brings up the problem of abort handling.
  ;; However, an abort would exit the entire REMOTE-MAGTAPE function
  ;; closing the connection. REMOTE-MAGTAPE may be called again.
  (LET ((HAPPY-HACKING (LIST (STANDARD-REMOTE-READ STREAM)
                             (STANDARD-REMOTE-READ STREAM)
                             (MAPCAR #'(lambda (arg)
                                         (if (eq arg 'remote-hacking-stream-self)
                                             standard-input
                                           arg))
                                     (STANDARD-REMOTE-READ STREAM)))))
    (STANDARD-REMOTE-PRINT
      (MULTIPLE-VALUE-LIST
        (IF (AND (SYMBOLP (CADR HAPPY-HACKING))
                 (GET (CADR HAPPY-HACKING) 'SPECIALLY-MUNGED))
            (APPLY (CADR HAPPY-HACKING) (CAR HAPPY-HACKING) (CDDR HAPPY-HACKING))
          (LEXPR-SEND standard-input :rubout-handler
                      (CAR HAPPY-HACKING)
                      (CADR HAPPY-HACKING)
                      (IF (AND (SYMBOLP (CADR HAPPY-HACKING))
                               (GET (CADR HAPPY-HACKING) 'BACKWARD-MUNGED-ARGUMENTS))
                          (REVERSE (CAR (CDDR HAPPY-HACKING)))
                        (CDDR HAPPY-HACKING)))))
      STREAM)
    (SEND STREAM :FORCE-OUTPUT)))

(DEFMETHOD (REMOTE-HACKING-STREAM :PROMPT-AND-READ) (OPTION ST &REST ARGS)
  ;; we handle this specially because it is the easiest way of dealing
  ;; with the fact that special variable binding is used by prompt-and-read
  (SEND SELF :REMOTE-CALL 'HANDLE-REMOTE-PROMT-AND-READ)
  (STANDARD-REMOTE-PRINT OPTION SUBSTREAM)
  (STANDARD-REMOTE-PRINT ST SUBSTREAM)
  (STANDARD-REMOTE-PRINT ARGS SUBSTREAM)
  (SEND SUBSTREAM :FORCE-OUTPUT)
  (VALUES-LIST (STANDARD-REMOTE-READ SUBSTREAM)))


(DEFUN HANDLE-REMOTE-PROMT-AND-READ (STREAM)
  (STANDARD-REMOTE-PRINT
    (MULTIPLE-VALUE-LIST (APPLY 'PROMPT-AND-READ
                                (STANDARD-REMOTE-READ STREAM)
                                (STANDARD-REMOTE-READ STREAM)
                                (STANDARD-REMOTE-READ STREAM)))
    STREAM)
  (SEND STREAM :FORCE-OUTPUT))

(DEFMETHOD (REMOTE-HACKING-STREAM :BEEP) (&REST IGNORED)
  (SEND SELF :REMOTE-SINGLE-OP :BEEP))


(DEFMETHOD (REMOTE-HACKING-STREAM :READ-CURSORPOS) (&REST L)
  (SEND SELF :REMOTE-GENERAL-OP :READ-CURSORPOS L))

(DEFMETHOD (REMOTE-HACKING-STREAM :SET-CURSORPOS) (&REST L)
  (SEND SELF :REMOTE-GENERAL-OP :SET-CURSORPOS L))

(DEFMETHOD (REMOTE-HACKING-STREAM :SIZE) (&REST L)
  (SEND SELF :REMOTE-GENERAL-OP :SIZE L))

(DEFMETHOD (REMOTE-HACKING-STREAM :EDGES) (&REST L)
  (SEND SELF :REMOTE-GENERAL-OP :EDGES L))

(DEFMETHOD (REMOTE-HACKING-STREAM :DRAW-LINE) (&REST L)
  (SEND SELF :REMOTE-GENERAL-OP :DRAW-LINE L))

(COMPILE-FLAVOR-METHODS REMOTE-HACKING-STREAM)

(DEFUN TEST-FQUERY ()
  (FQUERY '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
                      ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
                      ((:PROCEED "Proceed.") #/p #\RESUME)))
          "~A : Restore ? "
          "<FILENAME-FOO>"))

(defun test-prompt-and-read ()
  (prompt-and-read :read "~&Hello ~A: " 'dolly))

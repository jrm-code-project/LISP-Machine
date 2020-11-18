;;; -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:10 -*-
;;; Network message facility
;;;     ** (c) Copyright 1982 Cliff Lasser **

;;; To Do:
;;; Commands for moving between major conversations (control-shift-[)
;;; xxx qsends-off and qsends-on in converse.
;;; Just refuse messages from certain people
;;; xxx Remove HDTisms, some Chaosnet dependencies.

(DEFVAR *CONVERSE-RECEIVE-MODE-DOCUMENTATION*
        "This variable controls what occurs when you receive a new interactive message.
        It has four possible values:
        :AUTO means to automatically enter Converse when a message arrives.
        :NOTIFY means print a short message on your current window
          when a message arrives, or pop up a small window to tell
          you to Type Terminal 0 S to go to Converse.
        :NOTIFY-WITH-MESSAGE is similar to :NOTIFY, but the notification is included
          in the message as well as the just the sender's name.  This is the default.
        :SIMPLE means to pop up a small window with which you can read
          the message, reply, enter Converse, or do nothing at all."
 "This variable is used for saving space and for modularity in the documentation.")

;;; User options

(DEFVAR *CONVERSE-RECEIVE-MODE* :NOTIFY-WITH-MESSAGE
  ;This one is least obnoxious -- RMS
        "This variable controls what occurs when you receive a new interactive message.
         It has four possible values:

        :AUTO means to automatically enter Converse when a message arrives.
        :NOTIFY means print a short message on your current window
          when a message arrives, or pop up a small window to tell
          you to Type Terminal 0 S to go to Converse.
        :NOTIFY-WITH-MESSAGE is similar to :NOTIFY, but the notification is included
          in the message as well as the just the sender's name.  This is the default.
        :SIMPLE means to pop up a small window with which you can read
          the message, reply, enter Converse, or do nothing at all."
        )

(DEFVAR *CONVERSE-END-EXITS* NIL
  "If T, typing END in Converse send and exit if NIL (the default), Converse will just send.")

(DEFVAR *CONVERSE-APPEND-P* NIL
  "NIL means to prepend incoming messages into the Converse buffer, T means to append them.")
(DEFVAR *CONVERSE-BEEP-COUNT* 2  "Number of beeps to do when a new message arrives.")

(DEFVAR *CONVERSE-EXTRA-HOSTS-TO-CHECK* NIL
  "A list of hosts other than Lisp Machines that are checked to determine if a user is logged in.
This variable only is checked when a username is specified as a destination without a host.")

(DEFVAR *CONVERSE-WAIT-P* T "T means that we are willing to wait to determine the status of the message we are sending.")

(DEFVAR *CONVERSE-GAGGED* NIL
  "If this variable is NIL, then you will receive all incoming Converse messages.
If it is not T, then it is assumed to be a string that will be sent to any user
who attempts to send you a message.
If the value of this variable is T, then all incoming messages are simply rejected.
Usage of the functions QSENDS-ON and QSENDS-OFF is encouraged instead.")

(ADD-INITIALIZATION "Converse is initially enabled"
                    '(SETQ *CONVERSE-GAGGED* NIL)
                    NIL 'si:cold-initialization-list)

(DEFVAR *END-ABORT-MESSAGE* "For some weird reason, the Converse who line hasn't updated."
  "Please report bugs to bug-converse.")

(DEFVAR *CONVERSE-END-EXITS-MESSAGE*
        "   End sends and exits, Abort just exits, Control-End just sends")

(DEFVAR *CONVERSE-END-JUST-SENDS-MESSAGE*
        "   End just sends, Abort just exits, Control-End sends and exits")

;Next two are obsolete and should be flushed at some point.
(DEFVAR *CONVERSE-AUTO-EXPOSE-P* 'OBSOLETE
  "T=automatic exposure, NIL=notification")
(DEFVAR *CONVERSE-NOTIFY-WITH-MESSAGE* 'OBSOLETE
  "T=notification includes message, NIL=just sender")

(DEFVAR *Y-OR-N-CONVERSE-OPTIONS* '(
                                    *CONVERSE-APPEND-P*
                                    *CONVERSE-END-EXITS*
                                    *CONVERSE-WAIT-P*
                                    *CONVERSE-GAGGED*
                                    )
  "Used by CONVERSE-PROFILE to set user's converse options.")

(DEFVAR *HAIRY-CONVERSE-OPTIONS*
        '(
          *CONVERSE-RECEIVE-MODE*
          *CONVERSE-BEEP-COUNT*
          *CONVERSE-EXTRA-HOSTS-TO-CHECK*)
    "Used by CONVERSE-PROFILE to set user's converse options.")

;;; Internal variables
(DEFVAR *AWAITING-EXPOSURE* NIL "Internal variable to tell if we are awaiting exposure.")
(DEFVAR *CONVERSE-LIST* :UNBOUND "Used interally to store the conversations.")
(DEFVAR *CONVERSE-FRAME* :UNBOUND "Used internally and should be initially unbound.")
(DEFVAR *CONVERSE-COMTAB* NIL
  "Used for creating a comtab for Converse with the proper extra commands.")

(DEFVAR *BUFFER-MUNGED-SAVED-CONVERSATION* ""
  "Used to store the conversation that regenerate buffer gets rid of.")

(DEFVAR *SYSTEMS-DONT-UPCASE-FOR* '(:unix :multics)
  "List of types of host whose send servers
care about case, and therefore we won't uppercasify what we send them.")

(DEFVAR *SAVED-SENDS* (MAKE-ARRAY 100 :TYPE 'ART-STRING :LEADER-LIST '(0)))
(DEFVAR *LAST-CONVERSE-SENDER* NIL "Internally used so that we can reply.")

(ADD-INITIALIZATION "Clear saved sends" '(SETQ *SAVED-SENDS* NIL) '(:BEFORE-COLD))

(DEFUN QSENDS-OFF (&OPTIONAL (GAG-MESSAGE T))
  "Refuse messages from other users.
GAG-MESSAGE can be a string which is sent automatically as a reply
to anyone who sends a message here."
  (SETQ *CONVERSE-GAGGED* GAG-MESSAGE))

(DEFUN QSENDS-ON ()
  "Accept messages from other users."
  (SETQ *CONVERSE-GAGGED* NIL))


;;This must fit into the three lines normally provided.
(DEFPARAMETER *CONVERSE-BUFFER-MUNGED-MESSAGE*
          "Buffer damaged!  Do Meta-X Regenerate Buffer to continue using Converse.
This will destroy the text of the current message being edited.  To save that
text you must kill it now and yank it back after regenerating the buffer.")
#|  This is the non-three-line message that used to be here in defiance of the above comment. -naha
(DEFPARAMETER *CONVERSE-BUFFER-MUNGED-MESSAGE*
     "Buffer damaged!  You must do Meta-X Regenerate Buffer to continue using Converse.
This will destroy any text that you have edited in the buffer
other than messages already sent and received.
To save that text, you must kill in now, and yank it back later.")
 |#

(DEFPARAMETER *CONVERSE-POINT-INVALID-MESSAGE*
          "You're not inside a conversation, please move outside the black lines.")

(DEFFLAVOR CONVER
          (FIRST-LINE                           ;first line in conversation
           LAST-LINE                            ;last line in conversation
           (WHO "Whomever")                     ;with whom are we conversing
           MY-NODE                              ;node in which this is happening
           TO-LINE-DEL                          ;points to diagram after To: area
           (WITH-HEADER-P T)                    ;whether or not conversation has header
           (APPEND-MODE *CONVERSE-APPEND-P*)
           (OLDMSGS NIL)                        ;all the msgs in this conversation
           )
          ()
  (:INITABLE-INSTANCE-VARIABLES WHO APPEND-MODE WITH-HEADER-P)
  (:INIT-KEYWORDS :BEFORE-LINE :AFTER-LINE)
  :GETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (CONVER :LINE-MINE?) (LINE)
  (EQ (LINE-NODE LINE) MY-NODE))

(DEFMETHOD (CONVER :MY-NAME?) (NAME)
  (EQUALP WHO NAME))

;;; Return a bp to the line immediately after "To:".  In the case of a headerless
;;; conversation, the bp is to the end of the "To:" line.
(DEFMETHOD (CONVER :AFTER-TO-LINE-BP) ()
  (LET ((L1 (IF WITH-HEADER-P (LINE-NEXT FIRST-LINE) FIRST-LINE)))
    (IF WITH-HEADER-P
        (BEG-OF-LINE (LINE-NEXT L1))
      (WHEN (AND (> (LINE-LENGTH L1) 3)
               (STRING-EQUAL L1 "To:" :start1 0 :start2 0 :end1 3 :end2 3))
        (LET ((BP (CREATE-BP L1 3)))
          (IF (EQ (BP-CHAR BP) #/SP)
              (IBP BP))
          BP)))))

(DEFUN INIT-CONVERSATION-WITHOUT-H (AFTER-LINE BEFORE-LINE &AUX LINE)
  "Create the node and initial text for the first, special conversation.
It is inserted between AFTER-LINE and BEFORE-LINE,
which should be successive lines."
  (DECLARE (:SELF-FLAVOR CONVER))
  (SETQ MY-NODE (MAKE-INSTANCE 'NODE :SUPERIOR *INTERVAL*))
  (SETF (LINE-NODE AFTER-LINE) MY-NODE)
  (SETF (LINE-NODE BEFORE-LINE) MY-NODE)
  (SETQ LINE (MAKE-DIAGRAM-LINE 'BLACK-LINE-DIAGRAM))
  (SETF (LINE-NODE LINE) MY-NODE)
  (INSERT-LINE-WITH-LEADER LINE BEFORE-LINE)
  (INSERT-WITHIN-LINE AFTER-LINE 0 "To: " 0 4)  ;put in To: line
  (SETQ TO-LINE-DEL LINE)
; (INSERT (CREATE-BP LINE 0 :MOVES) #/CR)
  (SETQ FIRST-LINE AFTER-LINE
        LAST-LINE LINE))

(DEFUN INIT-CONVERSATION-WITH-H (AFTER-LINE BEFORE-LINE -WHO- &AUX LINE)
  "Create the node and initial text for a normal conversation with user -WHO-.
It is inserted between AFTER-LINE and BEFORE-LINE,
which should be successive lines."
  (DECLARE (:SELF-FLAVOR CONVER))
  (SETQ MY-NODE (MAKE-INSTANCE 'NODE :SUPERIOR *INTERVAL*))
  ;; splice in MY-NODE (must check for node type)
  (IF (AND (TYPEP (LINE-NODE AFTER-LINE) 'NODE)
           (NODE-NEXT (LINE-NODE AFTER-LINE)))
      (SETF (NODE-NEXT (LINE-NODE AFTER-LINE)) MY-NODE))
  (IF (AND (TYPEP (LINE-NODE BEFORE-LINE) 'NODE)
           (NODE-PREVIOUS (LINE-NODE BEFORE-LINE)))
      (SETF (NODE-PREVIOUS (LINE-NODE BEFORE-LINE)) MY-NODE))
  ;; make end of conversation delimiter
  (SETQ LINE (MAKE-DIAGRAM-LINE 'BLACK-LINE-DIAGRAM))
  ;; splice this diagram between AFTER-LINE and BEFORE-LINE
  ;; reason am doing this is to avoid setting nodes for To: line
  (SETQ LAST-LINE LINE)
  (INSERT-LINE-WITH-LEADER LINE BEFORE-LINE)
  ;; must set node after inserting, otherwise wrong node will be used
  (SETF (LINE-NODE LINE) MY-NODE)
  (SETQ TO-LINE-DEL LINE)
  ;; put To: line before end delimiter
  (INSERT (CREATE-BP LINE 0 :NORMAL)
          (STRING-APPEND "To: " -WHO- #/CR #/CR))
  ;; make header diagram
  (SETQ LINE (MAKE-DIAGRAM-LINE 'CONVERSE-HEADER-DIAGRAM :NAME -WHO-))
  (SETF (LINE-NODE LINE) MY-NODE)
  ;; splice header between AFTER-LINE and To: line
  (INSERT-LINE-WITH-LEADER LINE (LINE-NEXT AFTER-LINE))
  ;; tell header what name to display
  (SETQ FIRST-LINE (LINE-NEXT AFTER-LINE))
  )

;;; Initializes a conversation with correct To: line and diagrams.
;;; Must put itself between AFTER-LINE and BEFORE-LINE.
;;; If HEADER is nil then use AFTER-LINE for first-line and do not generate a
;;; conversation header diagram. Don't even bother with nodes for BEFORE-LINE.
(DEFMETHOD (CONVER :INIT) (INIT-PLIST)
  (LET ((BEFORE-LINE (GET INIT-PLIST :BEFORE-LINE))
        (AFTER-LINE (GET INIT-PLIST :AFTER-LINE)))
    (IF (OR (NULL BEFORE-LINE) (NULL AFTER-LINE))
        (FERROR NIL "You must supply a before-line and an after-line when creating a CONVER"))
    (IF WITH-HEADER-P
        (INIT-CONVERSATION-WITH-H AFTER-LINE BEFORE-LINE WHO)
        (INIT-CONVERSATION-WITHOUT-H AFTER-LINE BEFORE-LINE))))

(DEFMACRO CHECK-CONVERSATION-INTEGRITY ()
  ;check to see that the current conversation is ok, warning if not.
  `(IF (NULL (SEND SELF :CONVERSATION-GOOD?))
       (CONVERSE-BARF *CONVERSE-BUFFER-MUNGED-MESSAGE*)))

(DEFMACRO CHECK-BUFFER-INTEGRITY ()
  ;check to see that entire buffer is ok, warning if not.
  `(IF (NULL (CONVERSE-BUFFER-GOOD?))
       (CONVERSE-BARF *CONVERSE-BUFFER-MUNGED-MESSAGE*)))

;;; takes MSG and adds it in the right place to the conversation.
(DEFMETHOD (CONVER :ADD-MSG) (MSG &AUX LINE)
  ;;inserts all the new text before the old To-line-del
  ;;and creates a new to-line-del
  (PUSH MSG OLDMSGS)
  (CHECK-BUFFER-INTEGRITY)
  (SETQ LINE (MAKE-DIAGRAM-LINE 'BLACK-LINE-DIAGRAM))
  (COND ((OR (EQ LAST-LINE TO-LINE-DEL) (NOT APPEND-MODE));prepending mode
         (INSERT-LINE-WITH-LEADER LINE TO-LINE-DEL)       ;or no msgs yet
         (SETF (LINE-NODE LINE) MY-NODE)
         (SETQ TO-LINE-DEL LINE))
        (T                                ;appending mode
         (INSERT-LINE-WITH-LEADER LINE LAST-LINE)
         (SETF (LINE-NODE LINE) MY-NODE)))
  (INSERT (CREATE-BP (LINE-NEXT LINE) 0) (STRING-APPEND MSG #/CR)))

;;; returns the string in the To: line area of a conversation
(DEFMETHOD (CONVER :GET-TO-MSG) ()
  (CHECK-CONVERSATION-INTEGRITY)
  ;; must put L1 and L2 around the text to return
  (LET ((L1 (IF WITH-HEADER-P (LINE-NEXT FIRST-LINE) FIRST-LINE))
        (L2 (LINE-PREVIOUS TO-LINE-DEL)))
    (STRING-INTERVAL (BEG-OF-LINE L1) (END-OF-LINE L2) T)))

(DEFUN RESTORE-TO-MSG-WITH-H ()
  "Cleans out the To: line and following lines, after a message is sent.
This function is called with SELF set to a normal conversation."
  (DECLARE (:SELF-FLAVOR CONVER))
  ;; splice out lines between the header and the To: line delimiter
  (DELETE-INTERVAL (CREATE-BP (LINE-NEXT FIRST-LINE) 0)
                   (CREATE-BP TO-LINE-DEL 0))
  ;; now put in a new To: line
  (INSERT (CREATE-BP TO-LINE-DEL 0)
          (STRING-APPEND "To: " WHO #/CR #/CR)))

(DEFUN RESTORE-TO-MSG-WITHOUT-H ()
  "Cleans out the To: line and following lines, after a message is sent.
This function is called with SELF set to a special conversation
/(one that is with nobody in particular)."
  (DECLARE (:SELF-FLAVOR CONVER))
  (WITH-BP (BP (CREATE-BP FIRST-LINE 0) :NORMAL)
    (DELETE-INTERVAL BP (END-OF-LINE (LINE-PREVIOUS TO-LINE-DEL)))
    (SETQ FIRST-LINE (BP-LINE BP)))
  (INSERT-WITHIN-LINE FIRST-LINE 0 "To: " 0 4)
  ;;  will have to reset the interval-first-bp of converse buffer to start-line
; (INSERT (CREATE-BP TO-LINE-DEL 0 :MOVES) #/CR)
  )

;;; returns the To: line area to is empty state
(DEFMETHOD (CONVER :RESTORE-TO-MSG) ()
  (CHECK-CONVERSATION-INTEGRITY)
  ;; what we do depends on whether or not there is a header for this conversation
  (IF WITH-HEADER-P (RESTORE-TO-MSG-WITH-H) (RESTORE-TO-MSG-WITHOUT-H)))

;;; The following stuff will restore a damaged conversation or conversation buffer

(DEFUN REGEN-CONVERSE-BUFFER  (&AUX OLDLIST)
  "Regenerate the entire contents of the Converse buffer from saved messages."
   ;;try to save what we throw away
  (SETQ *BUFFER-MUNGED-SAVED-CONVERSATION* (CAR *CONVERSE-LIST*))
  (SETQ OLDLIST (REVERSE (CDR *CONVERSE-LIST*))) ;;old list minus first conversation
  ;; clear everything out of the buffer

  (DELETE-INTERVAL *INTERVAL*)
  (INSERT (INTERVAL-FIRST-BP *INTERVAL*) #/CR)
  ;; make a headerless To: line conversation
  (LET ((C (MAKE-INSTANCE 'CONVER :WITH-HEADER-P NIL
                          :AFTER-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))
                          :BEFORE-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))))
    ;; create the conversation list
    (SETQ *CONVERSE-LIST* (LIST C)))
  ;; now regenerate all the conversations in order
  (DOLIST (CONVERSATION OLDLIST)
    (REGEN-SETUP-CONVERSATION CONVERSATION))
  ;; move the point to beginning of new buffer
  (MOVE-BP (POINT) (END-LINE (INTERVAL-FIRST-BP *INTERVAL*))))

(DEFUN REGEN-FIRST-CONVER ()
  "Try to regain lost text from regenerating the whole buffer."
  (IF (VARIABLE-BOUNDP *BUFFER-MUNGED-SAVED-CONVERSATION*)
      (REGEN-SETUP-CONVERSATION *BUFFER-MUNGED-SAVED-CONVERSATION*)))

(DEFUN SALVAGE-CONVERSE (&OPTIONAL SEVERELY)
  "Try to get Converse working again.  You may lose many things in the process, though.
If SEVERELY is T, then regenerate the buffers as well."
  ;; (if ... ask about saving buffer.
  (SEND (FIND-CONVERSE-WINDOW) :CLEAR-REQUEST-QUEUE)
  (IF SEVERELY (SEND (FIND-CONVERSE-WINDOW) :REGEN-YOURSELF)))

;;; for regenerating a conversation after main to-line conversation
(DEFUN REGEN-SETUP-CONVERSATION (CONVERSATION &AUX LINE)
  "Regenerate the text for CONVERSATION from its saved messages.
CONVERSATION should be a normal conversation -- one with a particular other user."
  (IF (CDR *CONVERSE-LIST*)
      (SETQ LINE (SEND (CADR *CONVERSE-LIST*) :FIRST-LINE))
      (SETQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ *CONVERSE-LIST* (APPEND (LIST (CAR *CONVERSE-LIST*)) (LIST CONVERSATION)
                                (CDR *CONVERSE-LIST*)))
  (SEND CONVERSATION :REGEN-YOURSELF (LINE-PREVIOUS LINE) LINE)
  CONVERSATION)

;;; This is almost the same thing as :INIT except go through all the OLDMSGS and add
;;; them to the buffer.
(DEFMETHOD (CONVER :REGEN-YOURSELF) (AFTER-LINE BEFORE-LINE &AUX (TOLDMSGS OLDMSGS))
  (SETQ APPEND-MODE *CONVERSE-APPEND-P*)
  (IF WITH-HEADER-P (INIT-CONVERSATION-WITH-H AFTER-LINE BEFORE-LINE WHO)
    (INIT-CONVERSATION-WITHOUT-H AFTER-LINE BEFORE-LINE))
  (SETQ OLDMSGS NIL)
  (DOLIST (MSG (REVERSE TOLDMSGS))
    (SEND SELF :ADD-MSG MSG)))

;;;; Diagram stuff

;;; the following create a medium width black line diagram
(DEFFLAVOR BLACK-LINE-DIAGRAM () (LINE-DIAGRAM-MIXIN))

(DEFMETHOD (BLACK-LINE-DIAGRAM :DRAW) (IGNORE SHEET &AUX HEIGHT)
  (SETQ HEIGHT (FLOOR (TV:SHEET-LINE-HEIGHT SHEET) 4))
  (TV:PREPARE-SHEET (SHEET)
    (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET)
                         HEIGHT
                         (TV:SHEET-INSIDE-LEFT SHEET)
                         (+ (TV:SHEET-CURSOR-Y SHEET)
                            (FLOOR (* HEIGHT 3) 2))
                         (TV:SHEET-CHAR-ALUF SHEET) SHEET)))

(DEFMETHOD (BLACK-LINE-DIAGRAM :STRING-FOR-FILE) (LINE)
  LINE
  "-----------------------------------------------------------------------")

(COMPILE-FLAVOR-METHODS BLACK-LINE-DIAGRAM)

;;; the following creates a converse header diagram
(DEFFLAVOR CONVERSE-HEADER-DIAGRAM
        (NAME)
        (LINE-DIAGRAM-MIXIN)
  :SETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (CONVERSE-HEADER-DIAGRAM :DRAW) (IGNORE SHEET &AUX W W1)
  (SETQ W (+ (TV:SHEET-STRING-LENGTH SHEET NAME 0 NIL NIL FONTS:CPTFONTB) #o100))
  (SETQ W1 (FLOOR (- (TV:SHEET-INSIDE-WIDTH SHEET) W) 2))
  (SEND SHEET :DRAW-RECTANGLE W1
                               (- (TV:SHEET-LINE-HEIGHT SHEET) 3)
                               0
                               (TV:SHEET-CURSOR-Y SHEET))
  (SEND SHEET :DRAW-RECTANGLE W1
                               (- (TV:SHEET-LINE-HEIGHT SHEET) 3)
                               (+ W W1)
                               (TV:SHEET-CURSOR-Y SHEET))
  (SEND SHEET :STRING-OUT-EXPLICIT
              NAME (+ W1 50) (TV:SHEET-CURSOR-Y SHEET)
              (+ (TV:SHEET-INSIDE-LEFT SHEET) (+ W W1)) NIL
              FONTS:CPTFONTB
              (TV:SHEET-CHAR-ALUF SHEET)))


(DEFMETHOD (CONVERSE-HEADER-DIAGRAM :STRING-FOR-FILE) (LINE)
  LINE
  (STRING-APPEND "============================== " NAME "============================== "))

(COMPILE-FLAVOR-METHODS CONVERSE-HEADER-DIAGRAM)

(DEFFLAVOR CONVERSE
        (*CONVERSE-LIST*                        ;list of all ongoing conversations
         (CONVERSE-REQUEST-QUEUE NIL)           ;requests from other processes
                ;This is a list of requests, each is cons of function and list of args
         )
        ()
  (:SPECIAL-INSTANCE-VARIABLES *CONVERSE-LIST*))

(DEFMETHOD (CONVERSE :CLEAR-REQUEST-QUEUE) ()
  (SETQ CONVERSE-REQUEST-QUEUE NIL))

(ADD-INITIALIZATION 'CLEAR-CONVERSE-QUEUE '(SEND (FIND-CONVERSE-WINDOW) :CLEAR-REQUEST-QUEUE)
                    '(:BEFORE-COLD))

;;; Use this message to send a request to the Converse process.
;;; The request is placed on the request queue and the process is awakened
;;; by forcing keyboard input.
(DEFMETHOD (CONVERSE :ENTER-REQUEST) (FUNCTION &REST ARGS)
  (LET ((REQ (CONS FUNCTION (COPYLIST ARGS))))
    (WITHOUT-INTERRUPTS
      ;; Put the new request at the end so they are processed in
      ;; the order they are requested.
      (SETQ CONVERSE-REQUEST-QUEUE
            (NCONC CONVERSE-REQUEST-QUEUE (NCONS REQ))))
    (SEND SELF :FORCE-KBD-INPUT '(:EXECUTE CONVERSE-EXECUTE-QUEUE))))

;;; Similar if the request does not need to be processed unless/until
;;; Converse is awakened for some other reason.
(DEFMETHOD (CONVERSE :ENTER-DELAYED-REQUEST) (FUNCTION &REST ARGS)
  (LET ((REQ (CONS FUNCTION (COPYLIST ARGS))))
    (WITHOUT-INTERRUPTS
      ;; Put the new request at the end so they are processed in
      ;; the order they are requested.
      (SETQ CONVERSE-REQUEST-QUEUE
            (NCONC CONVERSE-REQUEST-QUEUE (NCONS REQ))))))

;;; Process the delayed requests when converse is exposed.
;;; Note: delayed requests should not be made when converse is exposed.
(DEFMETHOD (CONVERSE :AFTER :EXPOSE) (&REST IGNORE)
  (IF CONVERSE-REQUEST-QUEUE
      (SEND SELF :FORCE-KBD-INPUT '(:EXECUTE CONVERSE-EXECUTE-QUEUE))))

(DEFUN CONVERSE-EXECUTE-QUEUE ()
  (SEND *CONVERSE-FRAME* :EXECUTE-QUEUED-REQUESTS))

(DEFMETHOD (CONVERSE :EXECUTE-QUEUED-REQUESTS) ()
  (DOLIST (REQ CONVERSE-REQUEST-QUEUE)
    (APPLY (CAR REQ) (CDR REQ))
    (WITHOUT-INTERRUPTS (SETQ CONVERSE-REQUEST-QUEUE (DELQ REQ CONVERSE-REQUEST-QUEUE)))))

(DEFUN SETUP-CONVERSATION (WHO &AUX CONVERSATION LINE)
  "Create a conversation with user WHO and insert its text in the buffer."
  (CHECK-BUFFER-INTEGRITY)
  (IF (CDR *CONVERSE-LIST*)
      (SETQ LINE (SEND (CADR *CONVERSE-LIST*) :FIRST-LINE))
      (SETQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ CONVERSATION (MAKE-INSTANCE 'CONVER
                                    :WHO WHO
                                    :AFTER-LINE (LINE-PREVIOUS LINE)
                                    :BEFORE-LINE LINE))
  (SETQ *CONVERSE-LIST* (APPEND (LIST (CAR *CONVERSE-LIST*)) (LIST CONVERSATION)
                                (CDR *CONVERSE-LIST*)))
  CONVERSATION)

;;; The following stuff will allow a conversation, or buffer to check its
;;; integrity.  For a buffer, that means being able to reach the first and
;;; last lines of every conversation, in the same order that they were put
;;; in the buffer.  Also, the last line of one conversation must be the first
;;; line of another conversation.  A conversation, however, only cares that
;;; its to-line-delimiter is between its first line and last line, and that
;;; its first line can be reached from the first line in the buffer.

;;; the present definition is a little too stringent: all it should really
;;; check for is a good 1st conversation and a good neighboring conversation
;;; (depending on the direction of the buffer) - more trickieness to come!
(DEFUN CONVERSE-BUFFER-GOOD? ()
  "T if the Converse buffer is consistently structured as nodes for conversations."
  ;; first line in buffer must be first line of first message
  (OR (NOT (VARIABLE-BOUNDP *CONVERSE-LIST*)) ;;may be checking before we got any messages
      (AND (EQ (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))
               (SEND (CAR *CONVERSE-LIST*) :FIRST-LINE))
           ;; must be able to get to the last line

           ;; check each last line for next line being first line of next conversation
           (DO ((C *CONVERSE-LIST* (CDR C)))
               ((NULL (CDR C)) T)
             (IF (NOT (LINE-REACHABLE? (SEND (CAR C) :FIRST-LINE)
                                       (SEND (CAR C) :LAST-LINE)))
                 (RETURN NIL))
             (IF (NEQ (LINE-NEXT (SEND (CAR C) :LAST-LINE))
                      (SEND (CADR C) :FIRST-LINE))
                 (RETURN NIL))))))

;;; returns whether or not the TO-LINE-DEL is in between the FIRST-LINE and
;;; LAST-LINE and also if the FIRST-LINE is reachable in the *CONVERSE-WINDOW*
(DEFMETHOD (CONVER :CONVERSATION-GOOD?) ()
  (AND (LINE-REACHABLE? (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)) FIRST-LINE)
       (LINE-REACHABLE? FIRST-LINE TO-LINE-DEL)
       (LINE-REACHABLE? TO-LINE-DEL LAST-LINE)))

;;; will find out if TO-REACH-LINE is somewhere after START-LINE
(DEFUN LINE-REACHABLE? (START-LINE TO-REACH-LINE)
  (DO () (())
    (IF (NULL START-LINE) (RETURN NIL))
    (IF (AND (LINE-NEXT START-LINE)
             (NEQ (LINE-PREVIOUS (LINE-NEXT START-LINE)) START-LINE))
        (RETURN NIL))             ;not a broken link?
    (IF (EQ START-LINE TO-REACH-LINE) (RETURN T))
    (SETQ START-LINE (LINE-NEXT START-LINE))))

;;;; Commands

(DEFCOM COM-CONVERSE-PREVIOUS-CONVERSATION "Move to the previous conversation." ()
  ;;go through all the conversations and find out which one the cursor is in
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (IF (BP-< (SEND CONVERSATION :AFTER-TO-LINE-BP) (POINT))
        (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP))
      (SETQ CONVERSATION (DO ((CLIST *CONVERSE-LIST* (CDR CLIST)))
                             ((OR (NULL CLIST) (EQ (CADR CLIST) CONVERSATION))
                              (CAR CLIST))))
      (IF CONVERSATION (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP)))))
  DIS-BPS)

(DEFCOM COM-CONVERSE-NEXT-CONVERSATION "Move to the next conversation." ()
  ;;go through all the conversations and find out which one the cursor is in
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (IF (BP-< (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP))
        (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP))
      (SETQ CONVERSATION (DO ((CLIST *CONVERSE-LIST* (CDR CLIST)))
                             ((OR (NULL CLIST) (EQ (CAR CLIST) CONVERSATION))
                              (CADR CLIST))))
      (IF CONVERSATION (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP)))))
  DIS-BPS)

(DEFCOM COM-CONVERSE-REGENERATE-BUFFER "Restore the Converse buffer to working condition." ()
  (REGEN-CONVERSE-BUFFER)
  DIS-ALL)

(DEFCOM COM-CONVERSE-ABORT "Exit from CONVERSE." ()
  (CONVERSE-QUIT)
  DIS-NONE)

(DEFCOM COM-CONVERSE-HANDLE-END
        "Send the current message; default is not to exit Converse.
Exits Converse if ZWEI:*CONVERSE-END-EXITS* is set to T." ()
  (IF *CONVERSE-END-EXITS*
      (CONVERSE-SEND-WITH-EXIT)
    (CONVERSE-SEND-WITHOUT-EXIT))
  DIS-TEXT)

(DEFCOM COM-CONVERSE-HANDLE-CONTROL-END
        "Send the current message and (by default) exit Converse.
Doesn't exit if ZWEI:*CONVERSE-END-EXITS* is set to T."
        ()
        (IF (NOT *CONVERSE-END-EXITS*)
            (CONVERSE-SEND-WITH-EXIT)
          (CONVERSE-SEND-WITHOUT-EXIT))
  DIS-TEXT)

(DEFUN (*END-ABORT-MESSAGE* MODE-LINE-RECALCULATE) ()
       (SETQ *END-ABORT-MESSAGE* (IF *CONVERSE-END-EXITS*
                                     *CONVERSE-END-EXITS-MESSAGE*
                                   *CONVERSE-END-JUST-SENDS-MESSAGE*)))

(DEFUN CONVERSE-SEND-WITHOUT-EXIT ()
  "Transmit the message without exiting Converse."
  (CONVERSE-SEND-MSG)
  DIS-TEXT)

(DEFUN CONVERSE-SEND-WITH-EXIT ()
  "Transmit the message and exit Converse."
  (IF (CONVERSE-SEND-MSG) ;;successful
      (CONVERSE-QUIT))
  DIS-TEXT)

(DEFCOM COM-CONVERSE-DELETE-CONVERSATION "Delete the conversation from the buffer." ()
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (DELQ CONVERSATION *CONVERSE-LIST*)
    (DELETE-INTERVAL (CREATE-BP (SEND CONVERSATION :FIRST-LINE) 0)
                     (CREATE-BP (LINE-NEXT (SEND CONVERSATION :LAST-LINE)) 0)))
  DIS-TEXT)

(DEFCOM COM-CONVERSE-WRITE-BUFFER "Write the entire buffer (all conversations) into a file."
        ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Write entire buffer to file:" (PATHNAME-DEFAULTS))))
    (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :OUTPUT :ERROR :REPROMPT)
      (STREAM-OUT-INTERVAL STREAM *INTERVAL* NIL NIL T)))
  DIS-NONE)

  ;;say which conversation !!!

(DEFCOM COM-CONVERSE-WRITE-CONVERSATION "Write a single conversation into a file." ()
  ;;go through all the conversations and find out which one the cursor is in
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (LET ((PATHNAME (READ-DEFAULTED-PATHNAME
                      (FORMAT NIL "Write conversation with ~A to file:"
                              (SEND CONVERSATION :WHO))
                      (PATHNAME-DEFAULTS))))
      (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :OUTPUT :ERROR :REPROMPT)
        (STREAM-OUT-INTERVAL
          STREAM
          (CREATE-BP (LINE-PREVIOUS (SEND CONVERSATION :FIRST-LINE)) 0)
          (CREATE-BP (SEND CONVERSATION :LAST-LINE) 0)
          T T))))
  DIS-NONE)

(DEFCOM COM-CONVERSE-APPEND-BUFFER
        "Append the entire buffer (all conversations) to the end of a file." ()
  (LET ((PATHNAME (READ-DEFAULTED-PATHNAME
                    "Append Converse buffer to:" (PATHNAME-DEFAULTS))))
    (WITH-OPEN-FILE-RETRY (OSTREAM (PATHNAME FS:FILE-ERROR) '(:OUT))
      (WITH-OPEN-FILE-CASE (ISTREAM PATHNAME)
        (FS:FILE-NOT-FOUND (TYPEIN-LINE "(New File)"))
        (ERROR (BARF "Error: ~A" ISTREAM))
        (:NO-ERROR
          (STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM)))
      (STREAM-OUT-INTERVAL OSTREAM *INTERVAL* NIL NIL T)))
  DIS-NONE)

(DEFCOM COM-CONVERSE-APPEND-CONVERSATION
        "Appends the conversation to the end of a file." ()
  ;;go through all the conversations and find out which one the cursor is in
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (LET ((PATHNAME (READ-DEFAULTED-PATHNAME
                      "Append Converse conversation to:" (PATHNAME-DEFAULTS)))
          (BP1 (CREATE-BP (LINE-PREVIOUS (SEND CONVERSATION :FIRST-LINE)) 0))
          (BP2 (CREATE-BP (SEND CONVERSATION :LAST-LINE) 0)))
      (WITH-OPEN-FILE-RETRY (OSTREAM (PATHNAME FS:FILE-ERROR) '(:OUT))
        (WITH-OPEN-FILE-CASE (ISTREAM PATHNAME)
          (FS:FILE-NOT-FOUND (TYPEIN-LINE "(New File)"))
          (ERROR (BARF "Error: ~A" ISTREAM))
          (:NO-ERROR
            (STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM)))
        (STREAM-OUT-INTERVAL OSTREAM BP1 BP2 T T))))
  DIS-NONE)



(DEFCOM COM-CONVERSE-HELP "Explain how to use Converse." ()
  (FORMAT T "~%     You are in Converse, the interactive message editor.

Messages are separated by black lines.  All the messages you send to or
receive from a particular user are grouped into a /"conversation/", with
that user's name at the top.  The thick black lines separate
conversations.  DON'T DELETE THE BLACK LINES; it will cause lossage.

Within the conversation, messages are ordered chronologically.
Users with different user names or at different hosts get distinct
conversations.  If you begin communicating with a user there is no
conversation for, a new conversation is created.

To send a message to FRED@HOST, move to after the /"To:/" at the
beginning of the buffer, type FRED@HOST, type Return, and the message.
Then use End or Control-End to send it.  This creates a new
conversation, which has a template message at the front that you can use
to send FRED more messages.  Then you can use the /"To:/" at the
beginning of the buffer to start new conversations, or use the templates
at the beginning of each conversation to continue that conversation.

You can send a message to more than one person at once.  Use the syntax
FRED@HOST,NOT-FRED@HOST-1,YOU@NAUSEAM.  A separate copy of your message
goes into the conversation for each of the recipients.

If instead of saying FRED@HOST you just say FRED, Converse will try to
find a Lisp Machine that FRED is logged into and send the message there.
If you set the variable ZWEI:*CONVERSE-EXTRA-HOSTS-TO-CHECK* to a list of hosts,
those hosts will be checked when Converse tries to determine where Fred is
logged in.  If Fred is actually logged in somewhere else, you will be given
a menu of hosts to choose from as a destination for your message.

Most, if not all of the usual editor commands can be used to edit your messages.

The following additional commands are provided:
   End                      Send the current message and exit from Converse.
   Control-End              Send the current, without leaving Converse.
   Abort                    Get rid of the Converse window.
   Control-M                Mail the current message instead of sending it.
   Control-Meta-Y           Yank in the text of the last message received.
   Meta-{                   Move to the previous To: line.
   Meta-}                   Move to the next To: line.
   M-X Delete Conversation  Delete the current conversation.
   M-X Disable Converse     Reject incoming messages.
   M-X Enable Converse      Accept incoming messages.
   M-X Write Buffer         Write the entire buffer (all conversations) to a file.
   M-X Write Conversation   Write only the current conversation to a file.
   M-X Append Buffer        Append all conversations (buffer) to the end of a file.
   M-X Append Conversation  Append the current conversation to the end of a file.
   M-X Regenerate Buffer    This may be necessary if, during your editing, you
                            damage the structure of the buffer, by editing in
                            or across the black lines.  This command rebuilds
                            the structure.  Some error messages may ask you to
                            give this command and try again.
                            **NOTE** It will throw away anything you have inserted
                            but not sent!  To avoid losing it, kill it now
                            and yank it back after you regenerate.

The following commands can be given from a Lisp Listener.
   (qsend)                  Enter Converse.
   (qsend dest)             Read a message from the keyboard and send it to dest.
   (qsend dest message)     Send message to dest.  Note: dest is not evaluated.
   (qsends-off)             Reject all incoming messages.
   (qsends-off message)     Same, but /"message/" is told to anyone who tries sending to you.
   (qsends-on)              Accept further messages.  This commaand un-does (qsends-off) .
   (reply message)          Send message to last user who sent you a message.

The following are user options that you may set in your Lispm init file:
   zwei:*converse-receive-mode*
~A~%
   zwei:*converse-append-p*
        If true, new messages are appended to the ends of
        conversations.  If false (this is the default), they
        are prepended to the beginnings of conversations.

   zwei:*converse-beep-count*
        The number of times to beep when a message arrives.
        The default value is 2.

   zwei:*converse-extra-hosts-to-check*
        A list of other hosts to look for users when no host name is
        specified.  Defaults to NIL.

Type any character to return to Converse top level.
~%" *converse-receive-mode-documentation*)
  DIS-NONE)


(DEFUN CONVERSE-QUIT ()
  "Exist Converse and return to previous window."
  (TV:DESELECT-AND-MAYBE-BURY-WINDOW *CONVERSE-FRAME*))

(DEFUN CONVERSE-SEND-MSG ()
  "Figures out what is the message to transmit, sends it and updates the buffer
Returns non-NIL if the message is successfully sent."
  (MULTIPLE-VALUE-BIND (DEST MSG)
      (CONVERSE-GET-DEST-AND-MSG)
    (CONVERSE-SEND-MSG-INTERNAL DEST MSG)))


(DEFUN CONVERSE-GET-DEST-AND-MSG ()
  "Find the message that point is in.  Return the parsed destination and
 the body of the message."
  (CHECK-BUFFER-INTEGRITY)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :LINE-MINE? (BP-LINE (POINT))) (RETURN C)))))
    (IF (NULL CONVERSATION)
        (CONVERSE-BARF *CONVERSE-POINT-INVALID-MESSAGE*))
    (LET ((MSG-WITH-TO (SEND CONVERSATION :GET-TO-MSG)))
      (LET ((DEST (IF (STRING-EQUAL MSG-WITH-TO "To:" :start1 0 :start2 0 :end1 3 :end2 3)
                      (STRING-TRIM " "
                                   (SUBSTRING MSG-WITH-TO 3 (STRING-SEARCH-CHAR #/CR
                                                                                MSG-WITH-TO)))
                    (CONVERSE-BARF "This message doesn't start with /"To:/".")))
            (MSG (IF (STRING-SEARCH-CHAR #/CR MSG-WITH-TO)
                     (SUBSTRING MSG-WITH-TO (1+ (STRING-SEARCH-CHAR #/CR MSG-WITH-TO)))
                     (CONVERSE-BARF "This message has no contents, only a /"To:/" line."))))
        (OR (STRING-SEARCH-NOT-SET '(#/RETURN #/SPACE) MSG)
            (CONVERSE-BARF "This message has no contents, only a /"To:/" line."))
        ;; Parse now enough to get error for non-ex host
        (CONDITION-CASE (ERROR)
            (DOLIST (DEST (PARSE-COMMAS-INTO-LIST DEST))
              (PARSE-SINGLE-DEST DEST T))
          (SYS:LOCAL-NETWORK-ERROR
           (CONVERSE-BARF "~A" (SEND ERROR :REPORT-STRING))))
        ;; It's valid enough to record on a conversation,
        ;; so remove it from the TO-MSG and try sending.
        (SEND CONVERSATION :RESTORE-TO-MSG)
        (VALUES DEST MSG)))))

;Within a Converse command, send MESSAGE to DESTINATION, returning T if successful.
;MAIL-P says mail the message instead of sending it interactively.
; DESTINATION is a string, and it can contain commas, in which case we
;will send to multiple people.  We return the list of successful recipients."
(DEFUN CONVERSE-SEND-MSG-INTERNAL (DESTINATION MESSAGE &OPTIONAL MAIL-P
                                   &AUX USER HOST LOSSAGE)
  (LET ((DEST-LIST (PARSE-COMMAS-INTO-LIST DESTINATION))
         LOSSAGE-REASON)
    (OR DEST-LIST (SETQ LOSSAGE T))
    (DOLIST (DEST DEST-LIST)
      (MULTIPLE-VALUE (DEST USER HOST)  ;;this now treats each user sperately
        (PARSE-SINGLE-DEST DEST))
      #| (COND ((AND (NOT (NULL HOST)) (NOT MAIL-P) (NOT (CHAOS:ON-CHAOSNET-P HOST)))
               (SETQ MAIL-P T)
               (CONVERSE-PROBLEM
                 (FORMAT NIL "Message for ~A is being mailed, as host ~A is not on the ~
chaosnet." USER (SEND (SI:PARSE-HOST HOST) :NAME))))) |#
      (COND ((NULL HOST)
             (SETQ LOSSAGE T)
             (CONVERSE-PROBLEM
               (STRING-APPEND "Message not sent because: "
                 (SETQ LOSSAGE-REASON
                       (FORMAT NIL "Converse could not find /"~A/" logged into any host." USER)))))
            (T
             (IF MAIL-P
                 (SEND-MESSAGE-STRING DEST MESSAGE)
               (SETQ LOSSAGE-REASON (SEND-MSG DEST MESSAGE)
                     LOSSAGE (OR LOSSAGE LOSSAGE-REASON)))))
      (CONVERSE-RECORD-MSG-SENT DEST MESSAGE MAIL-P LOSSAGE-REASON LOSSAGE-REASON T)))
  (NOT LOSSAGE))

(DEFUN CONVERSE-RECORD-MSG-SENT (DEST MESSAGE MAIL-P ERROR-P LOSSAGE-REASON
                                 &OPTIONAL INSIDE-CONVERSE-P)
  (LET ((CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                        (IF (SEND C :MY-NAME? DEST) (RETURN C)))))
    (IF (NULL CONVERSATION) (SETQ CONVERSATION (SETUP-CONVERSATION DEST)))
    (SEND CONVERSATION :ADD-MSG
          (FORMAT NIL
                  "Message ~:[~;NOT ~]~:[sent~;mailed~] to ~A (~\DATIME\)~@[ because~% ~A~]~%~A"
                  ERROR-P MAIL-P DEST LOSSAGE-REASON MESSAGE))
    (WHEN INSIDE-CONVERSE-P
      ;; move the point to the To: line so user can type there
      (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT))))

(DEFUN CONVERSE-EDIT-AND-SEND-MSG (DESTINATION MESSAGE)
  "Initialize a message to DESTINATION with MESSAGE and select Converse to edit it.
DESTINATION is a list of strings; we initialize a message to each string
in the list if there is more than one."
  (LET ((DEST (PARSE-SINGLE-DEST (CAR (PARSE-COMMAS-INTO-LIST DESTINATION)))))
    (LET ((CONVERSATION
            (OR (DOLIST (C *CONVERSE-LIST*)
                  (IF (SEND C :MY-NAME? DEST) (RETURN C)))
                (SETUP-CONVERSATION DEST))))
      ;; move the point to the To: line so user can type there
      (MOVE-BP (POINT) (BEG-LINE (SEND CONVERSATION :AFTER-TO-LINE-BP)))
      (INSERT-MOVING (POINT) MESSAGE)))
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (SEND (FIND-CONVERSE-WINDOW) :SELECT))

(DEFCOM COM-CONVERSE-MAIL-MESSAGE
        "Mail the current message to the specified destination instead of sending it." ()
  (MULTIPLE-VALUE-BIND (DEST MSG)
      (CONVERSE-GET-DEST-AND-MSG)
    (CONVERSE-SEND-MSG-INTERNAL DEST MSG T))
  DIS-TEXT)

(DEFCOM COM-CONVERSE-YANK-LAST-MSG-TEXT
        "Insert the text of the last message we received into the conversation."
        ()
  (INSERT-MOVING (POINT) (LAST-MESSAGE-TEXT))
  DIS-TEXT)

(defcom com-disable-converse "Disable /(/"gag/") Converse" ()
  (SETQ *CONVERSE-GAGGED* T)
  (format *query-io* "~&Converse is disabled.")
  dis-none)

(defcom com-enable-converse  "Enable Converse" ()
  (SETQ *CONVERSE-GAGGED* NIL)
  (format *query-io* "~&Converse is enabled.")
  dis-none)

(DEFCOM COM-GAG-CONVERSE
        "Gag Converse if it is not already gagged and ungag it if it is not."
        ()
  (if (typep *numeric-arg* '(integer 2))
      (com-disable-converse)
    (if *CONVERSE-GAGGED* (com-enable-converse) (com-disable-converse)))
  DIS-NONE)

(DEFUN INIT-CONVERSE-COMTAB ()
  (IF *CONVERSE-COMTAB*
      (FERROR "A *CONVERSE-COMTAB* already exists."))
  (SETQ *CONVERSE-COMTAB*
        (SET-COMTAB '*CONVERSE-COMTAB*
                    '(#/END COM-CONVERSE-HANDLE-END
                      #/CONTROL-Z COM-CONVERSE-SEND-WITH-EXIT
                      #/ABORT COM-CONVERSE-ABORT
                      #/CONTROL-END COM-CONVERSE-HANDLE-CONTROL-END
                      #/META-{ COM-CONVERSE-PREVIOUS-CONVERSATION
                      #/META-} COM-CONVERSE-NEXT-CONVERSATION
;; #/meta-shift-{ for real.
                      #/CONTROL-M COM-CONVERSE-MAIL-MESSAGE
                      #/CONTROL-META-Y COM-CONVERSE-YANK-LAST-MSG-TEXT
                      )
                    '(("Regenerate Buffer" . COM-CONVERSE-REGENERATE-BUFFER)
                      ("Delete Conversation" . COM-CONVERSE-DELETE-CONVERSATION)
                      ("Write Buffer" . COM-CONVERSE-WRITE-BUFFER)
                      ("Write Conversation" . COM-CONVERSE-WRITE-CONVERSATION)
                      ("Append Buffer" . COM-CONVERSE-APPEND-BUFFER)
                      ("Append Conversation" . COM-CONVERSE-APPEND-CONVERSATION)
                      ("Disable Converse" . COM-DISABLE-CONVERSE)
                      ("Enable Converse" . COM-ENABLE-CONVERSE)
                      ("Gag Converse" . COM-GAG-CONVERSE)
                      )))
  (SET-COMTAB-INDIRECTION *CONVERSE-COMTAB* *STANDARD-COMTAB*))


(DEFPARAMETER CONVERSE-EDITOR-CLOSURE-VARIABLES
          (MERGE-CLOSURE-VARIABLE-LISTS
            '((*COM-DOCUMENTATION-ALIST*
                (CONS '(#/M COM-CONVERSE-HELP) *COM-DOCUMENTATION-ALIST*))
              (*MAJOR-MODE* 'TEXT-MODE)
              (*POST-COMMAND-HOOK* '(CONVERSE-POST-COMMAND-HOOK)))
            TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

;;; Post-command hook to dump out any pending requests
;;; (They might have come in while in a break loop)
(DEFUN CONVERSE-POST-COMMAND-HOOK (&OPTIONAL IGNORE)
  (SEND *CONVERSE-FRAME* :EXECUTE-QUEUED-REQUESTS))
(DEFPROP CONVERSE-POST-COMMAND-HOOK 64. COMMAND-HOOK-PRIORITY)

(DEFFLAVOR CONVERSE-FRAME
        ()
        (CONVERSE ZWEI-FRAME
                ;These wouldn't be here at all if there was an activity system.
                ;Put them at the end for their daemons, don't let them shadow anything.
         TV:PROCESS-MIXIN TV:SELECT-MIXIN)
  (:DEFAULT-INIT-PLIST :PROCESS '(CONVERSE-WINDOW-TOP-LEVEL :SPECIAL-PDL-SIZE #o4000
                                                            :REGULAR-PDL-SIZE 4000.)
                       :SAVE-BITS T
                       :MODE-LINE-LIST
                       '("Converse " (*CONVERSE-GAGGED* "[Disabled] ") "(" *MODE-NAME-LIST* ") "
                         *END-ABORT-MESSAGE*)
                       :EDITOR-CLOSURE-VARIABLES CONVERSE-EDITOR-CLOSURE-VARIABLES
                       :COMTAB *CONVERSE-COMTAB*))

;;; This is the top-level function for the process
(DEFUN CONVERSE-WINDOW-TOP-LEVEL (*CONVERSE-FRAME*)
;  (LET ((*COM-DOCUMENTATION-ALIST*
;         (CONS '(#/M COM-CONVERSE-HELP) *COM-DOCUMENTATION-ALIST*)))
  (DO-FOREVER
    (SEND *CONVERSE-FRAME* :EDIT)))

(DEFMETHOD (CONVERSE-FRAME :AFTER :INIT) (IGNORE)
  (LET ((PANE
          (SEND SELF :CREATE-WINDOW 'ZWEI-WINDOW-PANE :NAME "Converse"
                        :EXPOSE-P T)))
    (SEND SELF :SELECT-PANE PANE)
    (SEND PANE :SET-BASE-TICK *TICK*)
    (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
    (LET ((*BATCH-UNDO-SAVE* T))
      (INSERT (INTERVAL-LAST-BP *INTERVAL*) #/CR))
    ;; make a headerless To: line conversation
    (LET ((C (MAKE-INSTANCE 'CONVER :WITH-HEADER-P NIL
                            :AFTER-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))
                            :BEFORE-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))))
      ;; create the conversation list
      (SETQ *CONVERSE-LIST* (LIST C)))
    (MOVE-BP (POINT) (END-LINE (INTERVAL-FIRST-BP *INTERVAL*)))))

(DEFMETHOD (CONVERSE-FRAME :FORCE-KBD-INPUT) (INPUT)
  (SEND TV:SELECTION-SUBSTITUTE :FORCE-KBD-INPUT INPUT))

(DEFMETHOD (CONVERSE-FRAME :BEFORE :EXPOSE) (&REST IGNORE)
  (UNLESS (TV:SHEET-EXPOSED-P SELF)
    (SEND SELF :FORCE-KBD-INPUT '(:EXECUTE INITIALIZE-FOR-USER))))

(COMPILE-FLAVOR-METHODS CONVER CONVERSE-FRAME)

;;; initialize the command loop and window for converse
(DEFUN INITIALIZE-CONVERSE-COMMAND-LOOP ()
  (INIT-CONVERSE-COMTAB)
  (LET ((W (TV:MAKE-WINDOW 'CONVERSE-FRAME :ACTIVATE-P T)))
    (SEND (SEND W :PROCESS)
          :RUN-REASON W)))

(ADD-INITIALIZATION 'START-CONVERSE '(INITIALIZE-CONVERSE-COMMAND-LOOP) '(:ONCE))

;;;; Chaos stuff

;;; When a msg comes in, this is called to put it in the buffer
(DEFUN CONVERSE-RECEIVE-MSG (SENDER MSG &OPTIONAL ALREADY-EXPOSED &AUX CONVERSATION)
  ;; If the user has set one of the obsolete flags, gobble it down.
  (IF (NEQ *CONVERSE-AUTO-EXPOSE-P* 'OBSOLETE)
      (SETQ *CONVERSE-RECEIVE-MODE*
            (IF *CONVERSE-AUTO-EXPOSE-P* :AUTO :NOTIFY)
            *CONVERSE-AUTO-EXPOSE-P* 'OBSOLETE))
  (IF (NEQ *CONVERSE-NOTIFY-WITH-MESSAGE* 'OBSOLETE)
      (SETQ *CONVERSE-RECEIVE-MODE*
            (IF *CONVERSE-NOTIFY-WITH-MESSAGE* :NOTIFY-WITH-MESSAGE :NOTIFY)
            *CONVERSE-NOTIFY-WITH-MESSAGE* 'OBSOLETE))
  ;; Beep as desired.
  ;; If not in :AUTO mode and not exposed, the beeping was done already!
  (IF (OR ALREADY-EXPOSED (EQ *CONVERSE-RECEIVE-MODE* :AUTO))
      (DOTIMES (I *CONVERSE-BEEP-COUNT*)
        (BEEP 'CONVERSE-MESSAGE-RECEIVED *converse-frame*)))
  ;;find or create the proper conversation for this to go in
  (SETQ CONVERSATION (DOLIST (C *CONVERSE-LIST*)
                       (IF (SEND C :MY-NAME? SENDER) (RETURN C))))
  (IF (NULL CONVERSATION) (SETQ CONVERSATION (SETUP-CONVERSATION SENDER)))
  ;; add the message to the conversation and set up the point for easy reply
  (SEND CONVERSATION :ADD-MSG MSG)
  (UNLESS ALREADY-EXPOSED
    (IF *CONVERSE-APPEND-P*
        (MOVE-BP (POINT) (CREATE-BP (SEND CONVERSATION :LAST-LINE) 0))
      (MOVE-BP (POINT) (SEND CONVERSATION :AFTER-TO-LINE-BP))))
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COND ((MEMQ (SEND *CONVERSE-FRAME* :STATUS) '(:EXPOSED :SELECTED)))
        ((EQ *CONVERSE-RECEIVE-MODE* :AUTO)
         (SEND *CONVERSE-FRAME* :SELECT)))
  (TYPEIN-LINE "Latest message from ~A at ~A" SENDER (TIME:PRINT-CURRENT-TIME NIL)))

(DEFFLAVOR CONVERSE-SIMPLE-REPLY-WINDOW
     (CONVERSE-FRAME)
     (TV:WINDOW)
  :SETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :HEIGHT 400. :SAVE-BITS T
                       :NAME "Incoming Message"))

(DEFMETHOD (CONVERSE-SIMPLE-REPLY-WINDOW :WHO-LINE-DOCUMENTATION-STRING) ()
  "L: Enter Converse.  M: Return to previous activities.")

(DEFMETHOD (CONVERSE-SIMPLE-REPLY-WINDOW :AFTER :DEACTIVATE) ()
  (ARRAY-INITIALIZE TV:BIT-ARRAY 0)
  (SEND SELF :REFRESH-MARGINS))

(DEFMETHOD (CONVERSE-SIMPLE-REPLY-WINDOW :MOUSE-CLICK) (BUTTONS IGNORE IGNORE)
  (case buttons
    (#/MOUSE-1-1
      (PROCESS-RUN-FUNCTION "Select Converse"
                            #'(LAMBDA (CFRAME REPLY-WINDOW)
                                (SEND REPLY-WINDOW :DEACTIVATE)
                                (SEND CFRAME :SELECT))
                            CONVERSE-FRAME SELF)
      T)
    (#/MOUSE-2-1
     (PROCESS-RUN-FUNCTION "Kill Reply Window"
                           SELF :DEACTIVATE)
     T)))

(COMPILE-FLAVOR-METHODS CONVERSE-SIMPLE-REPLY-WINDOW)

(DEFWINDOW-RESOURCE CONVERSE-SIMPLE-REPLY-WINDOW ()
  :MAKE-WINDOW (CONVERSE-SIMPLE-REPLY-WINDOW)
  :REUSABLE-WHEN :DEACTIVATED)


(DEFUN CONVERSE-SIMPLE-REPLY (CONVERSE-FRAME SENDER MESSAGE)
  (USING-RESOURCE (REPLY-WINDOW CONVERSE-SIMPLE-REPLY-WINDOW)
    (SEND REPLY-WINDOW :SET-LABEL (FORMAT NIL "Message from ~A" SENDER))
    (SEND REPLY-WINDOW :EXPOSE NIL :CLEAN)
    (SEND REPLY-WINDOW :SELECT)
    (UNWIND-PROTECT
      (LET ((TERMINAL-IO REPLY-WINDOW))
        (SEND REPLY-WINDOW :SET-CONVERSE-FRAME CONVERSE-FRAME)
        (FORMAT REPLY-WINDOW "Message from ~A~%~A" SENDER MESSAGE)
        (FORMAT REPLY-WINDOW "~&Type Y to Reply, N to do Nothing or C to enter Converse.")
        (SELECTQ (FQUERY '(:CHOICES
                           (((YES "Reply.") #/Y #/R #/T #/HAND-UP #/SPACE)
                            ((NO "Nothing.") #/N #/RUBOUT #/HAND-DOWN)
                            ((CONVERSE "Go to Converse to read and send messages.") #/C))
                           :STREAM TERMINAL-IO
                           :CLEAR-INPUT T)
                         "Reply? ")
          (NO)
          (YES
           (FORMAT T "~&Type message to send to ~A" SENDER)
           (REPLY NIL SENDER NIL NIL)) ;;make it fast, avoid timing errors
          (CONVERSE
           (SEND REPLY-WINDOW :DEACTIVATE)
           (SEND CONVERSE-FRAME :SELECT))))
      (SEND REPLY-WINDOW :DEACTIVATE))))


;;; This makes up for lack of an activity system, in several ways
(DEFUN FIND-CONVERSE-WINDOW ()
  "Returns the Converse frame."
  (LET* ((FRAME (COND ((DO S TV:SELECTED-WINDOW (TV:SHEET-SUPERIOR S) (NULL S)
                           (AND (TYPEP S 'CONVERSE-FRAME) (RETURN S))))
                      ((TV:FIND-WINDOW-OF-FLAVOR 'CONVERSE-FRAME))
                      (T (TV:MAKE-WINDOW 'CONVERSE-FRAME))))
         (PROCESS (SEND FRAME :PROCESS)))
    (OR (SEND PROCESS :RUN-REASONS)
        (SEND PROCESS :RUN-REASON FRAME))
    FRAME))

(DEFUN LAST-MESSAGE-SENDER ()
  "Return the username of the last person who sent us a message."
; (AND (NOT (= 0. (LENGTH *SAVED-SENDS*)))
;      (SUBSTRING *SAVED-SENDS* 0 (STRING-SEARCH-CHAR  #/SPACE *SAVED-SENDS*))))
  *LAST-CONVERSE-SENDER*)

(DEFUN LAST-MESSAGE-TEXT ()
  "Return the text of the last Converse message that we have received."
  (AND (NOT (= 0. (LENGTH *SAVED-SENDS*)))
       (SUBSTRING *SAVED-SENDS*
                  (1+ (STRING-SEARCH-CHAR  #/RETURN *SAVED-SENDS*))
                  (STRING-SEARCH
                    (STRING-APPEND #/CR #/CR) *SAVED-SENDS*))))

;;; Sending messages outside of Converse.
(DEFUN REPLY (&OPTIONAL MESSAGE (DESTINATION *LAST-CONVERSE-SENDER*) MAIL-P
              (WAIT-P *CONVERSE-WAIT-P*))
  "Send the message MESSAGE to the person who last sent this machine a message.
With no arguments, reads a message from the terminal."
  (QSEND DESTINATION MESSAGE MAIL-P WAIT-P))

(DEFUN QSEND (&OPTIONAL DESTINATION MESSAGE (MAIL-P NIL) (WAIT-P *CONVERSE-WAIT-P*)
              &AUX SWITCH-TO-CONVERSE DESTINATION-LIST)
  "Send an interactive message, MESSAGE, to the people in DESTINATION.
If MESSAGE is empty, you will be prompted for it.
If DESTINATION is empty, Converse will be selected.
If MAIL-P is NIL (the default), the message will be sent interactively,
   otherwise the message will be mailed.
If WAIT-P is NIL, then queue up to message to be soon, but return NIL.
If WAIT-P is T, then wait until we determine the status of the messages sent,
and return a list of the successful recipients.  The default value of the variable
is contained in the init variable ZWEI:*CONVERSE-WAIT-P*.

This function is expected to be called by a user.
Programs which call it are guaranteed to do something useful only if MESSAGE and
DESTINATION are non-NIL."
  (SETQ DESTINATION-LIST (PARSE-COMMAS-INTO-LIST DESTINATION))
  (COND ((NULL DESTINATION)  ;; Person wants to select Converse.
         (SEND (FIND-CONVERSE-WINDOW) :SELECT)) ;;do your thing
        ((NULL MESSAGE)
         (FORMAT T
                 "~%Please enter a message for ~{~A~^, ~}:~%
  To terminate the message ~40Ttype ~:@C
  To quit ~40Ttype ~:@C
  To switch to a Converse Frame ~40Ttype ~:@C~@%"
                 DESTINATION-LIST
                 #/END #/ABORT #/C-M-E)
         (SETF (VALUES MESSAGE SWITCH-TO-CONVERSE)
               (QSEND-GET-MESSAGE *STANDARD-INPUT*))
         (COND (SWITCH-TO-CONVERSE
                (SETQ *AWAITING-EXPOSURE* T) ;in case of converse-problem  ;;???? -hdt
                (SEND (FIND-CONVERSE-WINDOW) :ENTER-REQUEST
                      'CONVERSE-EDIT-AND-SEND-MSG DESTINATION MESSAGE)
                (PROCESS-WAIT "Expose Converse"
                              (FIND-CONVERSE-WINDOW) :EXPOSED-P)
                (TV:AWAIT-WINDOW-EXPOSURE)
                (SETQ *AWAITING-EXPOSURE* NIL))
               (T  ;;didn't select converse, just send it.
                (QSEND-FORCE-MESSAGE DESTINATION MESSAGE MAIL-P WAIT-P))))
        (T  ;;we got the message without querying user, now just send it
         (QSEND-FORCE-MESSAGE DESTINATION MESSAGE MAIL-P WAIT-P))))

(DEFUN QSEND-GET-MESSAGE (&OPTIONAL (STREAM *STANDARD-INPUT*) IGNORE END-WITH-RETURN-OK)
  (IF (AND (NOT RUBOUT-HANDLER)
           (MEMQ :RUBOUT-HANDLER (SEND STREAM :WHICH-OPERATIONS)))
      (SEND STREAM :RUBOUT-HANDLER
               '((:EDITING-COMMAND #/END #/C-Z
                                   (#/C-M-Y "Yank last msg received")
                                   (#/C-M-E "Switch to Converse")))
               #'QSEND-GET-MESSAGE STREAM NIL END-WITH-RETURN-OK)
      (DO ((MSG (MAKE-ARRAY 100 :TYPE 'ART-STRING :LEADER-LIST '(0)))
           (CH))
          (NIL)
        (SETQ CH (SEND STREAM :TYI))
        (AND
          (OR (AND (MEMQ CH '(#/END #/C-Z NIL)))
              (AND END-WITH-RETURN-OK (EQ CH #/RETURN))) ;;why doesn't this work?
          (RETURN MSG))
        (AND (EQ CH #/C-M-E) (RETURN (VALUES MSG T)))
        (COND ((EQ CH #/C-M-Y)   ;;selectq ify?
               (LET ((TEXT (LAST-MESSAGE-TEXT)))
                 (IF (NULL TEXT) (SETQ TEXT ""))
                 (STRING-NCONC MSG TEXT)
                 (SEND STREAM :FORCE-KBD-INPUT TEXT)))
              ;; Normal case for a vanilla character
              (T (ARRAY-PUSH-EXTEND MSG CH))))))

(DEFUN QSEND-FORCE-MESSAGE (DESTINATION MESSAGE &OPTIONAL MAIL-P (WAIT-P T) &AUX RCPTS HOST)
  "Send a Converse message from outside of Converse.
DESTINATION is a string specifying where to send it,
MESSAGE is a string giving the text,
MAIL-P is non-NIL to mail rather than send interactivelty.
WAIT-P if NIL means return NIL right away while sending in background.
Otherwise we return the list of successful recipients."
  (LET ((DEST-LIST (PARSE-COMMAS-INTO-LIST DESTINATION)))
    (DOLIST (DEST DEST-LIST)
      (MULTIPLE-VALUE (DEST NIL HOST)
        (PARSE-SINGLE-DEST DEST))
      (IF HOST
          (PUSH DEST RCPTS)
        (FORMAT T "~&No host found for recipient ~A." DEST))))
  (IF (NULL WAIT-P)
      (PROGN (PROCESS-RUN-FUNCTION "Qsend" 'QSEND-FORCE-MESSAGE-1 RCPTS MESSAGE MAIL-P)
             NIL)
    (QSEND-FORCE-MESSAGE-1 RCPTS MESSAGE MAIL-P)))

(DEFUN QSEND-FORCE-MESSAGE-1 (RCPTS MESSAGE MAIL-P &AUX SUCCESS-RCPTS)
  (DOLIST (DEST RCPTS)
    (LET (LOSSAGE-REASON)
      (IF MAIL-P
          (SEND-MESSAGE-STRING DEST MESSAGE)
        (SETQ LOSSAGE-REASON (SEND-MSG DEST MESSAGE)))
      (IF (NOT LOSSAGE-REASON)
          (PUSH DEST SUCCESS-RCPTS))
      (SEND (FIND-CONVERSE-WINDOW)
            :ENTER-DELAYED-REQUEST
            'CONVERSE-RECORD-MSG-SENT
            DEST MESSAGE MAIL-P LOSSAGE-REASON LOSSAGE-REASON)))
  SUCCESS-RCPTS)

(DEFUN PARSE-SINGLE-DEST (DEST &OPTIONAL JUST-VERIFY &AUX HOST)
  "Parse the string DEST as a single destination for a QSEND message.
First value is a string of the form USERNAME@HOSTNAME,
where hostname is the official name of a host,
or NIL if DEST did not specify a host and the person cannot be found.
Also returns just the username as the second value
and just the host as a third value.

If JUST-VERIFY is non-NIL, we do only enough to get an error
if the host specified is nonexistent."
  ;;this code is slightly gross, most parsing has already been done.
  (LET ((@-POS (STRING-SEARCH-CHAR #/@ DEST)))
    (COND ((NULL @-POS)
           (UNLESS JUST-VERIFY
             (SETQ DEST (STRING-TRIM " " DEST))
             (SETQ HOST (DECIDE-HOST
                          (CHAOS:FIND-HOSTS-OR-LISPMS-LOGGED-IN-AS-USER
                            DEST *CONVERSE-EXTRA-HOSTS-TO-CHECK*)
                          DEST))))
          (T  (SETQ HOST (SI:PARSE-HOST (STRING-TRIM " " (SUBSTRING DEST (1+ @-POS))))
                    DEST (STRING-TRIM " " (SUBSTRING DEST 0 @-POS)))))
    (VALUES (STRING-APPEND DEST #/@ (if host (STRING HOST) "<undetermined-host>"))
            DEST
            (IF (NUMBERP HOST) NIL HOST))))

(DEFUN PARSE-COMMAS-INTO-LIST (STRING)
  "Given a string such as /"a,b,c/" return the list (/"a/" /"b/" /"c/").
If given a list, return the list."
  (IF (OR (NULL STRING) (LISTP STRING))
      STRING
    (LET ((COMMA-POS (STRING-SEARCH-CHAR #/, STRING)))
      (COND ((NOT COMMA-POS)
             (LIST (STRING-TRIM " " STRING)))
            (T
             (APPEND
               (LIST (STRING-TRIM " " (SUBSTRING STRING 0 COMMA-POS)))
               (PARSE-COMMAS-INTO-LIST (SUBSTRING STRING (1+ COMMA-POS)))))))))

(DEFUN GET-OFFICIAL-HOST-NAME (HOST)
  "Return the official name (a string) of a host specified in any way you like."
  (SEND (SI:PARSE-HOST HOST T) :NAME))

(DEFUN DECIDE-HOST (HOSTS USER)
  "Ask the user to pick one of HOSTS, the hosts on which USER is logged in.
If there is only one, the user is not asked.
The selected host is returned."
  (COND ((NULL HOSTS)
         (CONVERSE-PROBLEM
           (FORMAT NIL "Converse could not find /"~A/" logged into any host."
                   (IF (CONSP USER) (CAR USER) USER))))
        ((NULL (CADR HOSTS)) (CAR HOSTS))
        (T
         (TV:MENU-CHOOSE (LOOP FOR HOST IN HOSTS
                               COLLECT (CONS (STRING-APPEND USER #\@ (STRING HOST))
                                             HOST))
                         "Choose a host for outgoing message"))))

(DEFUN CONVERSE-BARF (FORMAT-STRING &REST ARGS)
  "Print an error message from converse and abort current activity."
  (APPLY 'CONVERSE-PROBLEM FORMAT-STRING ARGS)
  (THROW 'ZWEI-COMMAND-LOOP NIL))

(DEFUN CONVERSE-PROBLEM (FORMAT-STRING &REST ARGS)
  "Print a warning message from converse, possibly in background (for qsend).
Message goes to *QUERY-IO* (typein window) or as a notification."
  (BEEP 'CONVERSE-PROBLEM *query-io*)
  (IF (SEND (FIND-CONVERSE-WINDOW) :EXPOSED-P)
      (APPLY 'FORMAT *QUERY-IO* FORMAT-STRING ARGS)
    (APPLY 'TV:NOTIFY NIL (STRING-APPEND "Converse is reporting a problem: ~&" FORMAT-STRING)
           ARGS)))

;;; This is does the work of figuring out what kind of request to enter
;;; The arglist is keyworded so that someday we can pass in some user/host object
;;; or the like.
(defun converse-receive-from-network (message &key sender)
  "Enter a MESSAGE (a string) from SENDER (a user@host address) into Converse."
  (when sender
    ;; Don't bother recording a message if the connection
    ;; dropped before we even got the sender's name.
    (setq *saved-sends* (string-append message #\Newline #\Newline *saved-sends*))
    ;; Save the username for the reply macro.
    (setq *last-converse-sender* sender)
    ;; If user wants a simple reply window, get it now.
    ;; Don't wake up CONVERSE.
    (let* ((cframe (find-converse-window))
           (exposedp (send cframe :exposed-p)))
      (cond ((and (not exposedp)
                  (memq *converse-receive-mode*
                        '(:simple :pop-up :notify :notify-with-message)))
             (dotimes (i *converse-beep-count*) (beep 'converse-message-received cframe))
             (send cframe :enter-delayed-request 'converse-receive-msg sender message)
             (case *converse-receive-mode*
               ((:simple :pop-up)
                (process-run-function "Reply to Message" 'converse-simple-reply
                                      cframe sender message))
               ((:notify :notify-with-message)
                (tv:notify cframe "Converse message received from ~A"
                           (if (eq *converse-receive-mode* :notify-with-message)
                               (string-right-trim '(#\Newline #\Space) message)
                             sender)))))
            (t
             (send cframe :enter-request 'converse-receive-msg sender message exposedp))))))

(DEFUN RECEIVE-MSG-CONDITION-HANDLER (&REST IGNORE)
  (THROW 'CONNECTION-CLOSED NIL))

(DEFUN PRINT-SENDS (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print out all messages received from other users."
  (SEND STREAM :FRESH-LINE)
  (SEND STREAM :STRING-OUT *SAVED-SENDS*))

(defun send-msg (destination message &aux host (person "anyone"))
  "Actually send MSG to DESTINATION.  This is the internal function that does the work.
DESTINATION must be username@hostname or a host name or number;
 multiple destinations are not allowed here.
Does not record the message sent for Converse display.
Returns NIL if successful, or an error object."
  (when *converse-gagged*
    (format *error-output*
            "~&[Note: You have gagged incoming messages.  Do (QSENDS-ON) to ungag.]~%"))
  (etypecase destination
    ((integer #o200 #o200000) ; Chaos address.  Gletch
     (setq host (SI:PARSE-HOST (FORMAT NIL "CHAOS|~O" destination))))
    (string
     (cond ((setq host (do ((@-pos (string-search "@" destination)
                             (string-search "@" destination (1+ @-pos)))
                            (last-@-pos nil @-pos))
                           ((null @-pos) last-@-pos)))
            (setq person (nsubstring destination 0 host)
                  host (si:parse-host
                         (nsubstring destination (1+ host) (string-length destination))))
            (if (not (memq (send host :system-type) *systems-dont-upcase-for*))
                (setq person (string-upcase person))))  ; other hosts want it in uppercase
           (t (setq host (SI:PARSE-HOST destination))))))
  (NET:SEND-TERMINAL-MESSAGE HOST PERSON
                             #'(LAMBDA (STREAM)
                                 (format stream "~A@~A ~\DATIME\~%" user-id si:local-host)
                                 (send stream :string-out message))))


(defun chaos-receive-converse-message (&AUX CONN FLINE TO SENDER TEM (MSG ""))
  (setq conn (chaos:listen "SEND"))
  (IF (CHAOS:UNWANTED-CONNECTION-REJECTED-P CONN)
      (RETURN-FROM chaos-receive-converse-message NIL))
  (if *converse-gagged*
      (chaos:reject conn
                    (if (stringp *converse-gagged*)
                        *converse-gagged*
                      (format nil "~A is not accepting messages."
                              (if (member user-id '("" nil)) "The user" user-id))))
    (let* ((rfc (chaos:get-next-pkt conn)))
      (if (> (length (chaos:pkt-string rfc)) 4)
          (setq to (substring (chaos:pkt-string rfc) 5))
        (setq to "unspecified"))
      (chaos:return-pkt rfc))
    (chaos:accept conn)
    (with-open-stream (cstream (chaos:make-stream conn :direction :input))
      (condition-bind ((sys:remote-network-error
                         'receive-msg-condition-handler))
        (catch 'connection-closed
          (setq fline (send cstream :line-in))
          (cond ((setq tem (string-search-char #\@ fline))
                 (setq sender (nsubstring fline 0 tem)))
                ((setq tem (string-search "from " fline))
                 (setq sender (nsubstring fline (+ tem 5)
                                          (string-search-set
                                            '(#\] #\Sp) fline (+ tem 5)))))
                (t
                 (setq sender "")))
          (let ((host (si:get-host-from-address
                        (chaos:foreign-address conn) :chaos)))
            (setq sender (string-append
                           sender #/@
                           (if host (send host :name)
                             (format nil "CHAOS|~O" (chaos:foreign-address conn))))))
          (setq msg
                (format nil "~A~%~:[~*~;To: ~A~%~]~A"
                        fline (not (string-equal user-id to)) to
                        (with-output-to-string (stream)
                          (or (catch 'connection-closed
                                (stream-copy-until-eof cstream stream)
                                t)
                              (format stream "~%... chaos connection trouble.~%"))))))))
    (when sender
      (converse-receive-from-network msg :sender sender))))

;;; Now install it
(add-initialization "SEND"
                    '(process-run-function "Converse Chaos Receiver"
                                           #'chaos-receive-converse-message)
                    nil
                    'chaos:server-alist)

(tv:add-system-key #/C 'ZWEI:CONVERSE-FRAME "Converse" T)

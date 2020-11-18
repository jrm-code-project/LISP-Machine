;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Mailing commands and routines, definition are in DEFS
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Send a message
(DEFINE-ZMAIL-GLOBAL *LAST-MAIL-TYPE-ITEM* NIL
  "Last mail-type keyword used in Mail command.
Possibilities are :LOCAL, :BUG, :FORWARD, :REDISTRIBUTE, :MAIL.")

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-MAIL (STRING)
  (FORMAT STRING "Send a message: L: normal; M: ~A; R: menu."
          (NAME-FROM-MENU-VALUE *MAIL-MIDDLE-MODE* *ZMAIL-MAIL-MENU-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *MAIL-MIDDLE-MODE* COM-ZMAIL-MAIL)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MAIL "Send a message.
Left gives normal mail.  Middle is normally Bug, but controlled by *MAIL-MIDDLE-MODE*.
Right gives a menu of Bug, Forward, Redistribute or Local." (NO-ZMAIL-BUFFER-OK)
  (SET-ZMAIL-USER)
  (LET ((TYPE (CHOOSE-MAIL-MODE)))
    (CASE TYPE
      (:LOCAL
       (COM-ZMAIL-LOCAL-MAIL-INTERNAL))
      (:REDISTRIBUTE
       (COM-ZMAIL-REDISTRIBUTE-MSG))
      (OTHERWISE
       (COM-ZMAIL-MAIL-INTERNAL TYPE)))))

(DEFUN CHOOSE-MAIL-MODE (&OPTIONAL (TYPE :MAIL))
  "Ask user to choose between :LOCAL, :BUG, :FORWARD, :REDISTRIBUTE and :MAIL.
Decides based on button used, or puts up a menu."
  (IF (MEMQ *ZMAIL-COMMAND-BUTTON* '(:MIDDLE :RIGHT))
      (MULTIPLE-VALUE (TYPE *LAST-MAIL-TYPE-ITEM*)
        (ZMAIL-MENU-CHOOSE NIL *ZMAIL-MAIL-MENU-ALIST* *LAST-MAIL-TYPE-ITEM*
                           NIL *MAIL-MIDDLE-MODE*))
    TYPE))

;;; Forwarding version
(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-FORWARD "Forward current message" ()
  (COM-ZMAIL-MAIL-INTERNAL :FORWARD))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-BUG "Send a bug report" (NO-ZMAIL-BUFFER-OK)
  (COM-ZMAIL-MAIL-INTERNAL :BUG))

(DEFVAR *SENDING-BUG-REPORT* NIL
  "While composing a draft message, T if the mail is a bug report.")

(DEFUN COM-ZMAIL-MAIL-INTERNAL (MODE &AUX POINT WHO WHAT (STARTING-WINDOW :HEADER)
                                          (*SENDING-BUG-REPORT* NIL))
  (LET (*DRAFT-MSG* *DRAFT-HEADER-INTERVAL* *DRAFT-TEXT-INTERVAL*)
    (CASE MODE
      (:FORWARD
       (OR *MSG* (BARF "There is no current message")))
      (:BUG
       (SETQ *SENDING-BUG-REPORT* T)
       (MULTIPLE-VALUE (WHO WHAT)
         (PARSE-BUG-ARG (GET-BUG-ARG)))))
    (INITIALIZE-FOR-MAIL (MAKE-DRAFT-MSG) (IF (EQ MODE :FORWARD) T :SUBJECT-TOO))
    (LET ((*INTERVAL* *DRAFT-MSG*))
      (SETQ POINT (WINDOW-POINT *DRAFT-HEADER-WINDOW*))
      (INSERT-MOVING POINT "To: ")
      (INSERT POINT #/CR)
      (CASE MODE
        (:MAIL
         (WHEN *DEFAULT-MAIL-TEMPLATE*
           (FUNCALL *DEFAULT-MAIL-TEMPLATE* *INTERVAL* NIL)))
        (:FORWARD
         (SETF (DRAFT-MSG-MSGS-BEING-FORWARDED *DRAFT-MSG*) (NCONS *MSG*))
         (AND *FORWARDED-ADD-SUBJECT*
              (LET* ((STREAM (INTERVAL-STREAM-INTO-BP POINT))
                     (STATUS (ASSURE-MSG-PARSED *MSG*))
                     (FROM (CAR (GET STATUS :FROM)))
                     (SUBJECT (GET STATUS :SUBJECT)))
                (FORMAT STREAM "~%Subject: [~A: ~A]"
                        (STRING-FROM-HEADER FROM :LONG)
                        (OR SUBJECT "Forwarded"))))
         (INSERT-MSGS-INTO-WINDOW *DRAFT-TEXT-WINDOW* NIL *MSG*)
         (OR (AND (STRING-EQUAL *FORWARDED-MESSAGE-BEGIN* "")
                  (STRING-EQUAL *FORWARDED-MESSAGE-END* ""))
             (LET* ((BP (WINDOW-POINT *DRAFT-TEXT-WINDOW*))
                    (STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))))
               (FORMAT STREAM *FORWARDED-MESSAGE-BEGIN* 1)
               (SEND STREAM :FRESH-LINE)
               (SEND STREAM :SET-BP BP)
               (FORMAT STREAM *FORWARDED-MESSAGE-END* 1)
               (SEND STREAM :FRESH-LINE)
               (MOVE-BP BP (SEND STREAM :READ-BP))))
         (WHEN *DEFAULT-FORWARDING-TEMPLATE*
           (FUNCALL *DEFAULT-FORWARDING-TEMPLATE* *INTERVAL* (LIST *MSG*))))
        (:BUG
         (INSERT-MOVING POINT WHO)
         (INSERT-MOVING (WINDOW-POINT *DRAFT-TEXT-WINDOW*) WHAT)
         (SETQ STARTING-WINDOW :TEXT)
         (WHEN *DEFAULT-BUG-TEMPLATE*
           (FUNCALL *DEFAULT-BUG-TEMPLATE* *INTERVAL* NIL))))
      ;; Avoid an extra blank line at end of headers.
      (WHEN (char= (BP-CHAR-BEFORE (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)) #/RETURN)
        (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) -1)
                         (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) T))
      (ZMAIL-MAIL :MAIL STARTING-WINDOW))))

(DEFVAR *ZMAIL-BUG-LIST*
        '(("ZWEI" :VALUE "ZWEI" :DOCUMENTATION
           "Report a bug in the Lisp machine editing software.")
          ("ZMAIL" :VALUE "ZMAIL" :DOCUMENTATION
           "Report a bug in the Lisp machine mail-reading software.")
          ("LISPM" :VALUE "LISPM" :DOCUMENTATION
           "Report a bug in any other part of the Lisp machine software system.")
          ("LMMAN" :VALUE "LMMAN" :DOCUMENTATION
           "Report an inaccuracy, omission or unclarity in the Lisp machine manual.")
          ("Hardware" :VALUE "Hardware" :DOCUMENTATION
           "Report a malfunction in a particular Lisp machine.")
          ("Other" :VALUE :OTHER :FONT FONTS:HL12I
           :DOCUMENTATION "You specify the mailing list to send the report to."))
  "Menu alist for chosing a bug report destination.
The value of each menu item should be a string or the symbol :OTHER.")

(DEFUN ADD-BUG-RECIPIENT (NAME &OPTIONAL DOCUMENTATION)
  "Add an entry to *ZMAIL-BUG-ALIST*.
NAME is a topic, such as /"LISPM/", and DOCUMENTATION
is a string saying what to use this list for."
  (SETQ NAME (STRING NAME))
  (OR (DOLIST (ELT *ZMAIL-BUG-LIST*)
        (IF (EQUALP NAME (IF (CONSP ELT) (CAR ELT) ELT))
            (RETURN T)))
      (PUSH (LIST NAME :VALUE NAME
                  :DOCUMENTATION
                  (OR DOCUMENTATION (STRING-APPEND "Report a bug in " NAME)))
            *ZMAIL-BUG-LIST*)))

(DEFMACRO (:BUG-REPORTS SI:DEFSYSTEM-MACRO) (&OPTIONAL ADDRESS DOCUMENTATION)
  (ADD-BUG-RECIPIENT (OR ADDRESS (SI:SYSTEM-NAME SI:*SYSTEM-BEING-DEFINED*))
                     DOCUMENTATION)
  NIL)

(DEFINE-ZMAIL-GLOBAL *LAST-BUG-TYPE* NIL
  "The entry in *ZMAIL-BUG-ALIST* for the last bug report started.")

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION GET-BUG-ARG *BUG-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER GET-BUG-ARG (STRING)
  (FORMAT STRING "Send a bug report:~@[ L: BUG-~A; ~] R: menu."
          (CAR *LAST-BUG-TYPE*)))

(DEFUN GET-BUG-ARG (&AUX WHO)
  "Ask the user to select a bug-report list.
Returns a string not containing the word /"BUG-/"."
  (OR (EQ *ZMAIL-COMMAND-BUTTON* :KBD)
      (MULTIPLE-VALUE (WHO *LAST-BUG-TYPE*)
        (ZMAIL-MENU-CHOOSE NIL *ZMAIL-BUG-LIST* *LAST-BUG-TYPE*)))
  (PROG1 (COND ((EQ *ZMAIL-COMMAND-BUTTON* :KBD)
                (SETQ WHO (TYPEIN-LINE-READLINE
                            "Send a message to BUG-~@[ (default ~A)~]"
                            (OR (CAR *LAST-BUG-TYPE*)
                                (CAAR *ZMAIL-BUG-LIST*))))
                (IF (EQUAL WHO "")
                    (SETQ WHO (CAR (OR *LAST-BUG-TYPE*
                                       (SETQ *LAST-BUG-TYPE* (CAR *ZMAIL-BUG-LIST*)))))
                  (ADD-BUG-RECIPIENT WHO)
                  (SETQ *LAST-BUG-TYPE* (ASS 'EQUALP WHO *ZMAIL-BUG-LIST*))
                  WHO))
               ((STRINGP WHO)
                WHO)
               ((EQ WHO :OTHER)
                (SETQ WHO (CALL-POP-UP-MINI-BUFFER-EDITOR
                            :MOUSE 'TYPEIN-LINE-READLINE "Send a message to BUG-"))
                (AND (EQUAL WHO "") (BARF))
                (ADD-BUG-RECIPIENT WHO)
                (SETQ *LAST-BUG-TYPE* (ASS 'EQUALP WHO *ZMAIL-BUG-LIST*))
                WHO))
         (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-BUG-ARG)))

(DEFUN MAKE-DRAFT-MSG (&REST MSGS-BEING-REPLIED-TO)
  "Create a DRAFT-MSG for replying to the messages supplied as args."
  (MAKE-INSTANCE 'DRAFT-MSG :MSGS-BEING-REPLIED-TO MSGS-BEING-REPLIED-TO))

(DEFUN VALIDATE-DRAFT-MSG-SUMMARY-STRING (DRAFT-MSG &AUX TEM HEADERS RECIPIENTS SUBJECT
                                          SUMMARY-STRING
                                          (*INTERVAL* DRAFT-MSG))
  "Return a string summarizing DRAFT-MSG.
Used in giving the user a menu for the Continue command."
  (IF ( (NODE-TICK DRAFT-MSG) (DRAFT-MSG-SUMMARY-STRING-TICK DRAFT-MSG))
      (DRAFT-MSG-SUMMARY-STRING DRAFT-MSG)
    (CONDITION-CASE ()
        (PROGN
          (SETQ TEM (PARSE-HEADERS-INTERVAL (DRAFT-MSG-HEADER-INTERVAL DRAFT-MSG))
                HEADERS (LOCF TEM))
          (AND (SETQ RECIPIENTS (GET HEADERS :TO))
               (SETQ SUMMARY-STRING (STRING-APPEND "To: " (SUMMARIZE-RECIPIENTS RECIPIENTS 20.))))
          (AND (OR (NULL RECIPIENTS)
                   (< (STRING-LENGTH SUMMARY-STRING) 20.))
               (SETQ RECIPIENTS (GET HEADERS :CC))
               (SETQ SUMMARY-STRING (STRING-APPEND (IF SUMMARY-STRING
                                                       (STRING-APPEND SUMMARY-STRING "; CC: ")
                                                     "CC: ")
                                                   (SUMMARIZE-RECIPIENTS RECIPIENTS 20.))))
          (AND (> (STRING-LENGTH SUMMARY-STRING) 30.)
               (SETQ SUMMARY-STRING (SUBSTRING SUMMARY-STRING 0 30.)))
          (AND (COND ((SETQ SUBJECT (GET HEADERS :SUBJECT))
                      (AND (CONSP SUBJECT)
                           (SETQ SUBJECT (CAR SUBJECT)))
                      (SETQ SUBJECT (STRING-APPEND "Re: " SUBJECT))
                      T)
                     ((NOT (EQUAL (SETQ SUBJECT (FIRST-TEXT-LINE (DRAFT-MSG-REPLY-INTERVAL
                                                                   DRAFT-MSG)))
                                  ""))))
               (SETQ SUMMARY-STRING (IF SUMMARY-STRING
                                        (STRING-APPEND SUMMARY-STRING "; " SUBJECT)
                                      SUBJECT)))
          (COND (SUMMARY-STRING
                 (AND (DRAFT-MSG-MSGS-BEING-REPLIED-TO DRAFT-MSG)
                      (SETQ SUMMARY-STRING (STRING-APPEND "Reply: " SUMMARY-STRING)))
                 (OR (DRAFT-MSG-SENT-P DRAFT-MSG)
                     (SETQ SUMMARY-STRING (STRING-APPEND (IF (DRAFT-MSG-MSGS-BEING-REPLIED-TO DRAFT-MSG)
                                                             "Unsent "
                                                           "Unsent: ")
                                                         SUMMARY-STRING))))
                (T
                 (SETQ SUMMARY-STRING "Empty")))
          (SETF (DRAFT-MSG-SUMMARY-STRING DRAFT-MSG) SUMMARY-STRING)
          (SETF (DRAFT-MSG-SUMMARY-STRING-TICK DRAFT-MSG) (TICK))
          SUMMARY-STRING)
      (ERROR "??"))))

;;; Send some more
(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-CONTINUE (STRING)
  (APPEND-TO-ARRAY STRING "Resume sending message:")
  (AND *DRAFT-LIST*
       (LET* ((LEFT-DRAFT-MSG (CAR *DRAFT-LIST*))
              (MIDDLE-DRAFT-MSG (LOOP FOR DM IN *DRAFT-LIST*
                                      UNLESS (DRAFT-MSG-SENT-P DM)
                                      RETURN DM))
              (SAME-P (EQ LEFT-DRAFT-MSG MIDDLE-DRAFT-MSG)))
         (APPEND-TO-ARRAY STRING (IF SAME-P " L,M: " " L: "))
         (LET ((SUMMARY (VALIDATE-DRAFT-MSG-SUMMARY-STRING LEFT-DRAFT-MSG)))
           (APPEND-TO-ARRAY STRING SUMMARY 0
                            (MIN (COND (SAME-P 56.)
                                       ((NULL MIDDLE-DRAFT-MSG) 58.)
                                       (T 27.))
                                 (STRING-LENGTH SUMMARY))))
         (AND MIDDLE-DRAFT-MSG (NOT SAME-P)
              (LET ((SUMMARY (VALIDATE-DRAFT-MSG-SUMMARY-STRING MIDDLE-DRAFT-MSG)))
                (APPEND-TO-ARRAY STRING "; M: ")
                (APPEND-TO-ARRAY STRING SUMMARY 0 (MIN 27. (STRING-LENGTH SUMMARY)))))
         (VECTOR-PUSH-EXTEND #/; STRING)))
  (APPEND-TO-ARRAY STRING " R: menu."))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-CONTINUE "Resume sending message.
Left continues the last message.  Middle continues the last unsent message.
Right for menu of drafts or from a message or file." (NO-ZMAIL-BUFFER-OK)
  (LET ((DRAFT-MSG (GET-DRAFT-MSG-FOR-CONTINUE)))
    (COND ((NULL DRAFT-MSG)
           (ABORT-CURRENT-COMMAND))
          ((EQ DRAFT-MSG :RESTORE-DRAFT)
           (SETQ DRAFT-MSG (MAKE-DRAFT-MSG-FROM-RESTORED-DRAFT)))
          ((MEMQ DRAFT-MSG '(:FROM-THIS-MSG :FROM-SOME-MSG))
           (SETQ DRAFT-MSG (IF (EQ DRAFT-MSG :FROM-THIS-MSG)
                               (OR *MSG* (BARF "There is no current message"))
                               (CHOOSE-MSG-FROM-SUMMARY "a draft message")))
           (SETQ DRAFT-MSG (MAKE-DRAFT-MSG-FROM-MSG DRAFT-MSG))))
    (CONTINUE-DRAFT-MSG DRAFT-MSG)))

;;; Decide what message to continue sending, using a menu if appropriate.
;;; Return a DRAFT-MSG object.
(DEFUN GET-DRAFT-MSG-FOR-CONTINUE ()
  (CASE *ZMAIL-COMMAND-BUTTON*
    ((:LEFT :KBD)
     (OR (CAR *DRAFT-LIST*)
         (BARF "There are no messages to continue sending")))
    (:MIDDLE
     (OR (LOOP FOR DM IN *DRAFT-LIST*
               UNLESS (DRAFT-MSG-SENT-P DM)
               RETURN DM)
         (BARF "There are no unsent messages to continue sending")))
    (:RIGHT
     (TV:MENU-CHOOSE (NCONC (MAPCAR #'(LAMBDA (DRAFT-MSG)
                                        (CONS (VALIDATE-DRAFT-MSG-SUMMARY-STRING DRAFT-MSG)
                                              DRAFT-MSG))
                                    *DRAFT-LIST*)
                            '(("Restore draft file"
                               :VALUE :RESTORE-DRAFT :FONT FONTS:HL12I
                               :DOCUMENTATION
                               "Continue sending a message draft that was saved in a file.")
                              ("Restore draft message"
                               :BUTTONS (:FROM-THIS-MSG NIL :FROM-SOME-MSG) :FONT FONTS:HL12I
                               :DOCUMENTATION
  "Continue sending message draft saved as message: L: this message; R: specify from summary."
                               )))
                     NIL
                     (RECTANGLE-NEAR-COMMAND-MENU TV:MOUSE-SHEET)))))

(DEFUN CONTINUE-DRAFT-MSG (DRAFT-MSG)
  (LET (*DRAFT-MSG* *DRAFT-HEADER-INTERVAL* *DRAFT-TEXT-INTERVAL*)
    (INITIALIZE-FOR-MAIL DRAFT-MSG NIL)
    (ZMAIL-MAIL (DRAFT-MSG-LAST-WINDOW-CONFIGURATION DRAFT-MSG)
                (OR (CAAR (DRAFT-MSG-WINDOW-POINTS DRAFT-MSG))
                    :TEXT))))

;;; This is the guts of composing mail.
;;; The previous stuff is just multiple command interfaces to this.

(DEFUN ZMAIL-MAIL (CONFIGURATION STARTING-WINDOW
                   &AUX (OLD-CONFIGURATION *WINDOW-CONFIGURATION*)
                   (*INTERVAL* *DRAFT-MSG*)
                   (*INSIDE-MAIL* T)
                   *END-SENDS-MESSAGE-P*)
  "Enter composition of *DRAFT-MSG*, exiting on End or Abort.
INITIALIZE-FOR-MAIL should have been called already to specify
the draft to be edited and set up some things based on it.
CONFIGURATION is a keyword specifying the window configuration to use;
we switch to it and switch back on exit.
STARTING-WINDOW is the window in that configuratio to select to start with."
  (TV:WITH-SELECTION-SUBSTITUTE (NIL *ZMAIL-WINDOW*)
    (LET ((W
            (SEND *MSG-WINDOW* :FUNCALL-INSIDE-YOURSELF
                  'SET-MAIL-WINDOW-CONFIGURATION CONFIGURATION STARTING-WINDOW NIL)))
      (UNWIND-PROTECT
          (UNWIND-PROTECT
              (PROGN
                (LOCK-BACKGROUND-PROCESS)
                (IF (TV:SHEET-EXPOSED-P *MSG-WINDOW*)
                    (PREPARE-WINDOW-FOR-REDISPLAY *MSG-WINDOW*))
                (*CATCH 'SEND-IT
                  (LET ((*COMTAB* *REPLY-COMTAB*)
                        (*PARAGRAPH-DELIMITER-LIST*
                          (CONS #/- *PARAGRAPH-DELIMITER-LIST*))
                        (*MODE-LINE-LIST*
                          `("ZMail " "Mail " "(" *MODE-NAME-LIST*
                            ") " *ZMAIL-INTERVAL-NAME*
                            (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*)
                            ,(FORMAT NIL "     ~:@C " #/END)
                            (*END-SENDS-MESSAGE-P* "mails" :ELSE "adds more text")
                            ,(FORMAT NIL ", ~:@C aborts" #/ABORT))))
                    (SYS:%BIND (LOCF (TV:BLINKER-DESELECTED-VISIBILITY
                                       (WINDOW-POINT-BLINKER *DRAFT-HEADER-WINDOW*)))
                               :ON)
                    (SYS:%BIND (LOCF (TV:BLINKER-DESELECTED-VISIBILITY
                                  (WINDOW-POINT-BLINKER *DRAFT-TEXT-WINDOW*)))
                               :ON)
                    (SYS:%BIND (LOCF (TV:BLINKER-DESELECTED-VISIBILITY
                                  (WINDOW-POINT-BLINKER *MSG-WINDOW*)))
                               :ON)
                    (SEND *ZMAIL-WINDOW* :SET-SELECTION-SUBSTITUTE W)
                    (SEND W :EDIT))))
            (RESEPARATE-HEADER-AND-TEXT)
            (SAVE-DRAFT-MSG-WINDOW-STATE *DRAFT-MSG*)
            (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-CONTINUE))
        ;; These are the vital ones; do them even if the previous ones blow out.
        (IF (NEQ OLD-CONFIGURATION *WINDOW-CONFIGURATION*)
            (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION OLD-CONFIGURATION))
        (MUST-REDISPLAY *MSG-WINDOW* DIS-TEXT)  ;Was DIS-ALL.  Think this should do it.
        (ZMAIL-SELECT-MSG *MSG* T NIL)
        (PROCESS-UNLOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*))))
  DIS-NONE)

(DEFUN SAVE-DRAFT-MSG-WINDOW-STATE (DRAFT-MSG)
  "When exiting editing DRAFT-MSG, record POINT of various windows.
Record the current window configuration, and the POINT and START-BP
of each editor window in use in it, for use in Continuing editing."
  (SETF (DRAFT-MSG-LAST-WINDOW-CONFIGURATION DRAFT-MSG) *WINDOW-CONFIGURATION*)
  (LET ((W (SYMEVAL-IN-CLOSURE (WINDOW-EDITOR-CLOSURE *WINDOW*) '*WINDOW*)))
    (SETF (DRAFT-MSG-WINDOW-POINTS DRAFT-MSG)
          (LOOP FOR WINDOW IN (CONS W (REMQ W (SEND *ZMAIL-WINDOW* :EDITOR-WINDOWS)))
                WHEN (SEND WINDOW :EXPOSED-P)
                COLLECT (LIST WINDOW
                              (COPY-BP (WINDOW-POINT WINDOW) :NORMAL)
                              (COPY-BP (WINDOW-START-BP WINDOW) :NORMAL))))))

(DEFVAR *MSGS-BEING-REPLIED-TO-INTERVAL* :UNBOUND
  "While in ZMAIL-MAIL, this interval is the text of MSGs being replied to.
If replying to one MSG, it is that MSG's INTERVAL.
If replying to more than one at once, it is an interval
containing a concatenation of the MSGs.
It is NIL if not replying.")

;;; INSERT-DEFAULTS means put in the Default CC list and Default Fcc list
(DEFUN INITIALIZE-FOR-MAIL (&OPTIONAL (DRAFT-MSG (MAKE-DRAFT-MSG)) (INSERT-DEFAULTS T))
  "Set various variables for composing mail.
Sets *DRAFT-HEADER-INTERVAL* and *DRAFT-TEXT-INTERVAL*
to the inferiors of DRAFT-MSG.  Each caller MUST bind those variables.
Also puts those intervals into the corresponding windows."
  (SETQ *DRAFT-HEADER-INTERVAL* (DRAFT-MSG-HEADER-INTERVAL DRAFT-MSG))
  (SEND *DRAFT-HEADER-WINDOW* :SET-INTERVAL *DRAFT-HEADER-INTERVAL*)
  (MUST-REDISPLAY *DRAFT-HEADER-WINDOW* DIS-ALL)
  (SETQ *DRAFT-TEXT-INTERVAL* (DRAFT-MSG-REPLY-INTERVAL DRAFT-MSG))
  (SEND *DRAFT-TEXT-WINDOW* :SET-INTERVAL *DRAFT-TEXT-INTERVAL*)
  (MUST-REDISPLAY *DRAFT-TEXT-WINDOW* DIS-ALL)
  (SEND *MSG-WINDOW* :SET-INTERVAL DRAFT-MSG)

  (AND INSERT-DEFAULTS
       (OR *DEFAULT-CC-LIST* *DEFAULT-FCC-LIST* *REQUIRE-SUBJECTS*)
       (LET* ((LIST `(:CC ,*DEFAULT-CC-LIST*
                      :FCC ,*DEFAULT-FCC-LIST*))
              (PLIST (LOCF LIST))
              (STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*))))
         (AND (EQ INSERT-DEFAULTS :SUBJECT-TOO)
              *REQUIRE-SUBJECTS*
              (OR (NEQ *REQUIRE-SUBJECTS* :BUG)
                  *SENDING-BUG-REPORT*)
              (SETQ LIST (LIST* :SUBJECT "" LIST)))
         (OUTPUT-HEADER STREAM PLIST '(:SUBJECT :CC :FCC))))
  (SETF (DRAFT-MSG-MSGS-BEING-REPLIED-TO DRAFT-MSG)
        (DEL-IF #'(LAMBDA (MSG) (EQ (MSG-PARSED-P MSG) :KILLED))
                (DRAFT-MSG-MSGS-BEING-REPLIED-TO DRAFT-MSG)))
  (SETF (DRAFT-MSG-MSGS-BEING-FORWARDED DRAFT-MSG)
        (DEL-IF #'(LAMBDA (MSG) (EQ (MSG-PARSED-P MSG) :KILLED))
                (DRAFT-MSG-MSGS-BEING-FORWARDED DRAFT-MSG)))
  (LET ((MSGS (DRAFT-MSG-MSGS-BEING-REPLIED-TO DRAFT-MSG)))
    (COND ((NULL MSGS)
           (SETQ *MSGS-BEING-REPLIED-TO-INTERVAL* NIL))
          ((NULL (CDR MSGS))
           (SETQ *MSGS-BEING-REPLIED-TO-INTERVAL* (MSG-INTERVAL (CAR MSGS))))
          (T
           (SETQ *MSGS-BEING-REPLIED-TO-INTERVAL* (CREATE-INTERVAL))
           (DO ((BP (INTERVAL-LAST-BP *MSGS-BEING-REPLIED-TO-INTERVAL*))
                (MSGS MSGS (CDR MSGS))
                (*BATCH-UNDO-SAVE* T)
                (FIRST-P T NIL))
               ((NULL MSGS))
            (OR FIRST-P (INSERT BP #/CR))
            (INSERT-INTERVAL BP (MSG-INTERVAL (CAR MSGS)))))))
  (LOOP FOR ZOT IN (DRAFT-MSG-WINDOW-POINTS DRAFT-MSG)
        AS WINDOW = (POP ZOT)
        DO (MOVE-BP (WINDOW-POINT WINDOW) (POP ZOT))
           (RECENTER-WINDOW WINDOW :START (POP ZOT)))
  (SETQ *DRAFT-LIST* (CONS DRAFT-MSG (DELQ DRAFT-MSG *DRAFT-LIST*)))
  (SETQ *DRAFT-MSG* DRAFT-MSG))

(DEFUN SET-MAIL-WINDOW-CONFIGURATION (CONFIGURATION &OPTIONAL STARTING-WINDOW (SWITCHING-P T)
                                      &AUX (OLD-*WINDOW* *WINDOW*)
                                      (DRAFT-WINDOWS-EXPOSED-P
                                        (TV:SHEET-EXPOSED-P *DRAFT-HEADER-WINDOW*)))
  "Set the window configuration when entering mail composition, or inside it.
CONFIGURATION is either a configuration name,
or :MAIL meaning use *DEFAULT-MAIL-WINDOW-CONFIGURATION*,
or :NORMAL meaning use *DEFAULT-INITIAL-WINDOW-CONFIGURATION*.

STARTING-WINDOW is or identifies a window to select, or to take POINT from.
If switching from separate header//text windows to a single draft window,
that is where we take the POINT from.  If the reverse, that is what we select.
It can be a window, or :HEADER or :TEXT.

SWITCHING-P is NIL only on initial entry to mail composition."
  (AND STARTING-WINDOW (SYMBOLP STARTING-WINDOW)
       (SETQ STARTING-WINDOW
             (CASE STARTING-WINDOW
               (:HEADER *DRAFT-HEADER-WINDOW*)
               (:TEXT *DRAFT-TEXT-WINDOW*))))
  (COND ((NULL CONFIGURATION)
         (SETQ CONFIGURATION *WINDOW-CONFIGURATION*))
        ((EQ CONFIGURATION :MAIL)
         (SETQ CONFIGURATION *DEFAULT-MAIL-WINDOW-CONFIGURATION*)))
  (IF (EQ CONFIGURATION :NORMAL)
      (IF (NOT (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P))
          (SETQ CONFIGURATION *WINDOW-CONFIGURATION*)
        (SETQ CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*)))
  ;; Swap out msg window
  (AND SWITCHING-P
       ;; If already inside mail composition, and header window is not in use
       ;; (and therefore draft-text-window is not either)
       ;; then transfer the buffer pointers properly into
       ;; those two windows in case we are about to start using them.
       (NOT (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P))
       (LET* ((MSG-POINT (WINDOW-POINT *MSG-WINDOW*))
              WINDOW-TO-MOVE)
         (RESEPARATE-HEADER-AND-TEXT)
         (SETQ WINDOW-TO-MOVE (IF (BP-< MSG-POINT (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))
                                  *DRAFT-HEADER-WINDOW*
                                *DRAFT-TEXT-WINDOW*))
         (MOVE-BP (WINDOW-POINT WINDOW-TO-MOVE) MSG-POINT)
         (MUST-REDISPLAY WINDOW-TO-MOVE DIS-BPS)
         ;; If new *WINDOW* isn't specified, use the one that contains where POINT is now.
         (UNLESS STARTING-WINDOW (SETQ STARTING-WINDOW WINDOW-TO-MOVE))))
  (COND ((EQ CONFIGURATION :REPLY)
         (SEND *MSG-WINDOW* :SET-INTERVAL
               (OR *MSGS-BEING-REPLIED-TO-INTERVAL*
                   (MSG-INTERVAL *MSG*))))
        (T
         (SEND *MSG-WINDOW* :SET-INTERVAL *DRAFT-MSG*)))
  ;; Change configuration if necessary.
  (COND ((NEQ CONFIGURATION *WINDOW-CONFIGURATION*)
         (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION CONFIGURATION NIL)))
  ;; Make sure *typeout-window* gets set to correct value by MAKE-WINDOW-CURRENT.
  (SETQ *WINDOW* NIL)
  ;; Make the right window current; select it if already within ZMAIL-MAIL.
  (COND ((NOT (TV:SHEET-EXPOSED-P *DRAFT-TEXT-WINDOW*))
         (SETQ *ZMAIL-INTERVAL-NAME* "Message")
         (UNLESS (AND SWITCHING-P (NOT DRAFT-WINDOWS-EXPOSED-P))
           ;; If switching TO *MSG-WINDOW*, copy POINT into it from appropriate other window.
           ;; But not if we already were displaying the draft in *MSG-WINDOW*.
           (MOVE-BP (WINDOW-POINT *MSG-WINDOW*)
                    (WINDOW-POINT
                      (COND ((OR (EQ OLD-*WINDOW* *DRAFT-HEADER-WINDOW*)
                                 (EQ OLD-*WINDOW* *DRAFT-TEXT-WINDOW*))
                             OLD-*WINDOW*)
                            (SWITCHING-P *DRAFT-TEXT-WINDOW*)
                            (STARTING-WINDOW)
                            ((BP-= (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                                   (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*))
                             *DRAFT-TEXT-WINDOW*)
                            (T *DRAFT-HEADER-WINDOW*)))))
         (MAKE-WINDOW-CURRENT *MSG-WINDOW* NIL)
         (MUST-REDISPLAY *MSG-WINDOW* DIS-BPS))
        (T
         ;; We are using separate header and text windows.
         ;; Recenter header window to show as much stuff as possible.
         (WHEN (BP-= (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                     (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*))
           (REDISPLAY-POINT-ON-PLINE (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                                     *DRAFT-HEADER-WINDOW*
                                     (1- (WINDOW-N-PLINES *DRAFT-HEADER-WINDOW*))
                                     NIL)
           (MUST-REDISPLAY *DRAFT-HEADER-WINDOW* DIS-TEXT))
         (MAKE-WINDOW-CURRENT (OR STARTING-WINDOW *DRAFT-TEXT-WINDOW*) NIL)))
  (IF SWITCHING-P (SELECT-WINDOW *WINDOW*))
  (SETQ *END-SENDS-MESSAGE-P* (NEQ *WINDOW* *DRAFT-HEADER-WINDOW*))
  *WINDOW*)

(DEFUN RESEPARATE-HEADER-AND-TEXT ()
  "Recalculate division of *DRAFT-MSG* into headers and text.
Moves the boundaries of *DRAFT-HEADER-INTERVAL* and *DRAFT-TEXT-INTERVAL*.
Does nothing those two windows are now in use, since then there
was no way for the boundary to have moved."
  (WHEN (NOT (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P))
    (LET* ((*INTERVAL* *DRAFT-MSG*)
           (BOUNDARY (ZWEI-SEARCH (INTERVAL-FIRST-BP *DRAFT-MSG*)
                                  (STRING-APPEND #/RETURN
                                                 *MAIL-HEADER-DELIMITER*
                                                 #/RETURN))))
      (COND (BOUNDARY
             (MOVE-BP (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)
                      (END-LINE BOUNDARY -2))
             (MOVE-BP (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*)
                      BOUNDARY))
            (T
             (MOVE-BP (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)
                      (INTERVAL-LAST-BP *DRAFT-MSG*))
             (MOVE-BP (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*)
                      (INTERVAL-LAST-BP *DRAFT-MSG*)))))
    (MUST-REDISPLAY *DRAFT-HEADER-WINDOW* DIS-TEXT)
    (MUST-REDISPLAY *DRAFT-TEXT-WINDOW* DIS-TEXT)
    ;; Make sure text window's start-bp is at start of text or after.
    (IF (BP-< (WINDOW-START-BP *DRAFT-TEXT-WINDOW*)
              (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))
        (MOVE-BP (WINDOW-START-BP *DRAFT-TEXT-WINDOW*)
                 (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*)))
    ;; Make sure each window's POINT is inside its text.
    (IF (BP-< (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)
              (WINDOW-POINT *DRAFT-HEADER-WINDOW*))
        (MOVE-BP (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                 (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)))
    (IF (BP-< (WINDOW-POINT *DRAFT-TEXT-WINDOW*)
              (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))
        (MOVE-BP (WINDOW-POINT *DRAFT-TEXT-WINDOW*)
                 (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*)))))

;;; Send a reply
(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-REPLY (STRING &OPTIONAL RECURSIVE)
  (OR RECURSIVE (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'NORMAL-REPLY NIL T))
  (STRING-NCONC STRING "Reply to current message: "
                (GET 'NORMAL-REPLY :WHO-LINE-DOCUMENTATION)))

(DEFVAR *ZMAIL-REPLY-PROCESSING-LIST* '(DRAFT-REPLY COMSAT-REPLY XMAILR-REPLY NORMAL-REPLY))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REPLY "Reply to current message.
Left controlled by *REPLY-MODE* and *REPLY-WINDOW-MODE*.
Middle controlled by *MIDDLE-REPLY-MODE* and *MIDDLE-REPLY-WINDOW-MODE*.
Right gives a menu to specify recipients and window configuration.
Numeric argument of 1 replies according to *1R-REPLY-MODE*.
Numeric argument of 3 or 4 yanks in message.
Messages from COMSAT or draft messages are treated specially." (NUMERIC-ARG-OK)
  (LET (*DRAFT-MSG* *DRAFT-TEXT-INTERVAL* *DRAFT-HEADER-INTERVAL*)
    (DO ((L *ZMAIL-REPLY-PROCESSING-LIST* (CDR L))
         (MODE) (STARTING-WINDOW))
        ((NULL L) (ZMAIL-ERROR "Reply was not processed"))
      (MULTIPLE-VALUE (MODE STARTING-WINDOW)
        (FUNCALL (CAR L) *MSG*))
      (WHEN MODE
        (MOVE-BP (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                 (INTERVAL-FIRST-BP (WINDOW-INTERVAL *DRAFT-HEADER-WINDOW*)))
        ;; Avoid an extra blank line at end of headers.
        (WHEN (= (BP-CHAR-BEFORE (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)) #/RETURN)
          (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) -1)
                           (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) T))
        (WHEN *DEFAULT-REPLY-TEMPLATE*
          (FUNCALL *DEFAULT-REPLY-TEMPLATE* *DRAFT-MSG*
                   (DRAFT-MSG-MSGS-BEING-REPLIED-TO *DRAFT-MSG*)))
        (RETURN (ZMAIL-MAIL MODE STARTING-WINDOW))))))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER NORMAL-REPLY (STRING &OPTIONAL RECURSIVE)
  (APPEND-MULTIPLE-MENU-DOCUMENTATION STRING *REPLY-OLD-MODES-ALIST* "//"
                                      #/L *REPLY-MODE* *REPLY-WINDOW-MODE*)
  (APPEND-MULTIPLE-MENU-DOCUMENTATION STRING *REPLY-OLD-MODES-ALIST* "//"
                                      #/M *MIDDLE-REPLY-MODE* *MIDDLE-REPLY-WINDOW-MODE*)
  (APPEND-TO-ARRAY STRING " R: menu.")
  (OR RECURSIVE
      (DOLIST (COM '(COM-ZMAIL-REPLY COM-ZMAIL-REPLY-ALL SUMMARY-REPLY-DOCUMENTATION))
        (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM NIL T))))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *REPLY-MODE* NORMAL-REPLY)
(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *REPLY-WINDOW-MODE* NORMAL-REPLY)
(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *MIDDLE-REPLY-MODE* NORMAL-REPLY)
(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *MIDDLE-REPLY-WINDOW-MODE* NORMAL-REPLY)

(DEFUN NORMAL-REPLY (&REST MSGS
                     &AUX (REPLY-MODE *REPLY-MODE*)
                          (REPLY-WINDOW-MODE *REPLY-WINDOW-MODE*))
  (COND (*NUMERIC-ARG-P*
         (CASE *NUMERIC-ARG*
           (1 (SETQ REPLY-MODE *1R-REPLY-MODE*))
           ((3 4) (SETQ REPLY-WINDOW-MODE :YANK))))
        ((EQ *ZMAIL-COMMAND-BUTTON* :MIDDLE)
         (SETQ REPLY-MODE *MIDDLE-REPLY-MODE*
               REPLY-WINDOW-MODE *MIDDLE-REPLY-WINDOW-MODE*))
        ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
         (MULTIPLE-VALUE (REPLY-MODE REPLY-WINDOW-MODE)
           (DEFAULTED-MULTIPLE-MENU-CHOOSE-NEAR-MENU *REPLY-MODES-ALIST*
                                                     REPLY-MODE REPLY-WINDOW-MODE))))
  (SETUP-FOR-REPLY MSGS REPLY-MODE REPLY-WINDOW-MODE))

;;; For use in building user commands to do special replying.
(DEFUN SETUP-FOR-REPLY (MSGS REPLY-MODE REPLY-WINDOW-MODE)
  (INITIALIZE-FOR-MAIL (APPLY 'MAKE-DRAFT-MSG MSGS))
  (INSERT-REPLY-HEADERS REPLY-MODE)
  (COND ((EQ REPLY-WINDOW-MODE :YANK)
         (INSERT-MSGS-INTO-WINDOW *DRAFT-TEXT-WINDOW* T)
         (AND *PRUNE-HEADERS-AFTER-YANKING*
              (COM-PRUNE-YANKED-HEADERS))))
  (VALUES (IF (MEMQ REPLY-WINDOW-MODE '(:TWO-WINDOWS :SHOW-ORIGINAL))
              :REPLY :MAIL)
          :TEXT))

(DEFUN DRAFT-REPLY (MSG &AUX DRAFT-MSG)
  (COND ((MSG-DRAFT-MSG-P MSG)
         (SETQ DRAFT-MSG (MAKE-DRAFT-MSG-FROM-MSG MSG))
         (INITIALIZE-FOR-MAIL DRAFT-MSG NIL)
         (VALUES (DRAFT-MSG-LAST-WINDOW-CONFIGURATION DRAFT-MSG) :TEXT))))

(DEFUN COMSAT-REPLY (MSG &AUX START-BP END-BP FAILED-MSG-START-BP FAILED-RECIPIENTS)
  (COND ((AND (LET ((FROM (CAR (MSG-GET MSG :FROM))))
                (EQUAL (GET (LOCF FROM) :NAME) "COMSAT"))
              (SETQ FAILED-MSG-START-BP (ZWEI-SEARCH (SETQ START-BP (MSG-START-BP MSG))
                                                     "
 Failed message follows:
-------
"
                                                     NIL NIL NIL
                                                     (SETQ END-BP (MSG-END-BP MSG)))))
         (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
              (END-LINE (BP-LINE FAILED-MSG-START-BP))
              (IDX))
             ((EQ LINE END-LINE))
           (COND ((AND (STRING-EQUAL-START LINE "FAILED: ")
                       (DO ((IDX1 NIL)) (NIL)
                         (OR (AND (SETQ IDX (STRING-REVERSE-SEARCH-CHAR #/; LINE IDX1))
                                  (SETQ IDX1 (STRING-REVERSE-SEARCH-CHAR #/SP LINE IDX)))
                             (RETURN NIL))
                         (AND (STRING-EQUAL LINE " at " :start1 (- IDX1 3) :start2 0 :end1 (1+ IDX1))
                              (RETURN T))))
                  (SETQ FAILED-RECIPIENTS
                        (APPEND FAILED-RECIPIENTS (PARSE-ADDRESSES LINE 7 IDX))))
                 ((SETQ IDX (STRING-SEARCH " is an unknown recipient." LINE))
                  (SETQ FAILED-RECIPIENTS
                        (APPEND FAILED-RECIPIENTS (PARSE-ADDRESSES LINE 0 IDX))))))
         (FAILED-MAILER-RETRY MSG FAILED-MSG-START-BP END-BP FAILED-RECIPIENTS))))

(DEFUN XMAILR-REPLY (MSG &AUX FAILED-RECIPIENTS-START-BP FAILED-MSG-START-BP
                              FAILED-RECIPIENTS FAILED-MSG-END-BP)
  (COND ((AND (LET ((FROM (CAR (MSG-GET MSG :FROM))))
                (EQUAL (GET (LOCF FROM) :PERSONAL-NAME) "The Mailer Daemon"))
              (SETQ FAILED-RECIPIENTS-START-BP (ZWEI-SEARCH (MSG-START-BP MSG)
                                                            "
Message failed for the following:
"
                                                            NIL NIL NIL (MSG-END-BP MSG)))
              (SETQ FAILED-MSG-START-BP (ZWEI-SEARCH FAILED-RECIPIENTS-START-BP
                                                     "
            ------------
"
                                                     NIL NIL NIL (MSG-END-BP MSG))))
         (DO ((LINE (BP-LINE FAILED-RECIPIENTS-START-BP) (LINE-NEXT LINE))
              (END-LINE (LINE-PREVIOUS (BP-LINE FAILED-MSG-START-BP))))
             ((EQ LINE END-LINE))
           (SETQ FAILED-RECIPIENTS
                 (APPEND FAILED-RECIPIENTS (PARSE-ADDRESSES LINE 0
                                                            (STRING-SEARCH-CHAR #/: LINE)))))
         (DO ((LINE (BP-LINE (MSG-END-BP MSG)) PLINE)
              (PLINE))
             ((NOT (STRING-EQUAL (SETQ PLINE (LINE-PREVIOUS LINE)) "-------"))
              (SETQ FAILED-MSG-END-BP (CREATE-BP LINE 0))))
         (FAILED-MAILER-RETRY MSG FAILED-MSG-START-BP FAILED-MSG-END-BP FAILED-RECIPIENTS))))

(DEFUN FAILED-MAILER-RETRY (MSG START-BP END-BP RECIPIENTS &AUX HEADERS)
  (MULTIPLE-VALUE (HEADERS START-BP)
    (PARSE-ITS-MSG-HEADERS START-BP END-BP T))
  (INITIALIZE-FOR-MAIL (MAKE-DRAFT-MSG MSG) NIL)
  (DELETE-INTERVAL *DRAFT-HEADER-INTERVAL*)
  (LET* ((POINT (WINDOW-POINT *DRAFT-HEADER-WINDOW*))
         (LINE (BP-LINE POINT))
         (SUBJECT))
    (INSERT-REPLY-HEADER-LIST POINT RECIPIENTS :TO)
    (COND ((SETQ SUBJECT (GET (LOCF HEADERS) :SUBJECT))
           (AND (CONSP SUBJECT) (SETQ SUBJECT (CAR SUBJECT)))
           (OR (BEG-LINE-P POINT) (INSERT-MOVING POINT #/CR))
           (INSERT-MOVING POINT "Subject: ")
           (INSERT POINT SUBJECT)))
    (MOVE-BP POINT LINE 5))
  (INSERT-INTERVAL-MOVING (WINDOW-POINT *DRAFT-TEXT-WINDOW*) START-BP END-BP T)
  (VALUES :MAIL :HEADER))

;;; Make headers right for this message
(DEFUN INSERT-REPLY-HEADERS (REPLY-MODE
                             &AUX (POINT (WINDOW-POINT *DRAFT-HEADER-WINDOW*)) MSGS FROM TO)
  (SETQ MSGS (DRAFT-MSG-MSGS-BEING-REPLIED-TO *DRAFT-MSG*))
  (LOOP FOR MSG IN MSGS
        AS STATUS = (ASSURE-MSG-PARSED MSG)
        DO (SETQ FROM (APPEND FROM
                              (REPLY-HEADER-TRIM (OR (GET STATUS :REPLY-TO)
                                                     (GET STATUS :FROM))
                                                 FROM))))
  (LOOP FOR MSG IN MSGS
        AS STATUS = (ASSURE-MSG-PARSED MSG)
        DO (SETQ TO (APPEND TO (REPLY-HEADER-TRIM (GET STATUS :TO) TO))))
  (AND (MEMQ REPLY-MODE '(:ALL :ALL-CC :TO :TO-CC :CC-TO :CC-ALL))
       (INSERT-REPLY-HEADER-LIST POINT (REPLY-HEADER-TRIM (REPLY-HEADER-TRIM TO FROM)
                                                          *DONT-REPLY-TO* T)
                                 (IF (MEMQ REPLY-MODE '(:ALL :TO :CC-TO :CC-ALL))
                                     :TO :CC)))
  (AND (MEMQ REPLY-MODE '(:ALL :ALL-CC :CC-ALL))
       (INSERT-REPLY-HEADER-LIST
         POINT (LOOP FOR MSG IN MSGS
                     AS STATUS = (ASSURE-MSG-PARSED MSG)
                     WITH CC
                     DO (SETQ CC (APPEND CC (REPLY-HEADER-TRIM (GET STATUS :CC) CC)))
                     FINALLY (RETURN (REPLY-HEADER-TRIM (REPLY-HEADER-TRIM CC
                                                                           (APPEND FROM TO))
                                                        *DONT-REPLY-TO* T)))
                           :CC))
  (LET (SUBJECT)
    (LOOP FOR MSG IN MSGS
          UNTIL (SETQ SUBJECT (GET (ASSURE-MSG-PARSED MSG) :SUBJECT)))
    (COND (SUBJECT
           (AND (CONSP SUBJECT) (SETQ SUBJECT (CAR SUBJECT)))
           (OR (BEG-LINE-P POINT) (INSERT-MOVING POINT #/CR))
           (INSERT-MOVING POINT "Subject: ")
           (INSERT-MOVING POINT SUBJECT)
           (INSERT POINT #/CR))))
  (AND *GENERATE-IN-REPLY-TO-FIELD*
       (LET ((IN-REPLY-TO (GENERATE-IN-REPLY-TO-HEADER)))
         (COND (IN-REPLY-TO
                (OR (BEG-LINE-P POINT) (INSERT-MOVING POINT #/CR))
                (INSERT-MOVING POINT (WITH-OUTPUT-TO-STRING (STREAM)
                                       (PRINT-HEADER STREAM IN-REPLY-TO :IN-REPLY-TO)))))))
  ;; Go back to beginning and put in the original msg's From as recip, if appropriate.
  (MOVE-BP POINT (INTERVAL-FIRST-BP *DRAFT-HEADER-INTERVAL*))
  (AND FROM
       (INSERT-REPLY-HEADER-LIST POINT FROM (IF (MEMQ REPLY-MODE '(:CC-ALL :CC-TO))
                                                :CC :TO))))

(DEFUN REPLY-HEADER-TRIM (LIST-TO-TRIM LIST-TO-REMOVE &OPTIONAL STAR-SPECIAL)
  "Return a list of all elements of LIST-TO-TRIM not in LIST-TO-REMOVE.
The elements are recipients; that is, alternating-lists.

Meanwhile, any recipient in LIST-TO-TRIM that does not contain sufficient
information to be replied to causes a warning message and is dropped.

Recipients which are distribution-lists are dropped unless
*REPLY-HEADER-FORMAT* is :USE-ORIGINAL.

If STAR-SPECIAL is non-NIL, then LIST-TO-REMOVE is a list of strings
to compare against just the name of each recipient of LIST-TO-TRIM.
Furthermore, a * at the end of one of those strings matches anything."
  (LOOP FOR NEW IN LIST-TO-TRIM
        UNLESS (OR (WARN-OF-UNANSWERABLE-HEADER NEW)
                   (MEMQ NEW INFERIORS)
                   (LOOP FOR OLD IN LIST-TO-REMOVE
                         THEREIS (REPLY-HEADER-TRIM-EQUAL NEW OLD STAR-SPECIAL)))
        COLLECT NEW
        WHEN (AND (GETL (LOCF NEW) '(:DISTRIBUTION-LIST :BRACKETED-LIST))
                  (EQ *REPLY-HEADER-FORMAT* :USE-ORIGINAL))
        NCONC (GET-HEADER-INFERIORS NEW) INTO INFERIORS))

;;; Subroutine of reply-header-trim.
(DEFUN GET-HEADER-INFERIORS (HEADER)
  (LOOP FOR INF IN (GET (LOCF HEADER) :INFERIORS)
        NCONC (CONS INF (GET-HEADER-INFERIORS INF))))

;;; Subroutine of reply-header-trim.
(DEFUN REPLY-HEADER-TRIM-EQUAL (NEW OLD STAR-SPECIAL &AUX SL NL)
  (COND (STAR-SPECIAL
         (SETQ SL (STRING-LENGTH OLD))
         (AND (> SL 0)
              (char= (char OLD (1- SL)) #/*)
              (SETQ SL (1- SL) NL SL)))
        (T
         (SETQ OLD (GET (LOCF OLD) :NAME))))
  (SETQ NEW (GET (LOCF NEW) :NAME))
  (STRING-EQUAL NEW OLD :start1 0 :start2 0 :end1 NL :end2 SL))

;;; Subroutine of reply-header-trim.
(DEFUN WARN-OF-UNANSWERABLE-HEADER (HEADER &AUX (PLIST (LOCF HEADER)))
  (COND ((GET PLIST :NAME)
         NIL)                                   ;OK, normal header
        ((AND (GETL PLIST '(:DISTRIBUTION-LIST :BRACKETED-LIST))
              (GET PLIST :INFERIORS))
         (NEQ *REPLY-HEADER-FORMAT* :USE-ORIGINAL))     ;Use or ignore silently
        (T
         (FORMAT T "~&Cannot reply to /"~A/", address flushed~%"
                 (STRING-FROM-HEADER HEADER :USE-ORIGINAL))
         T)))

(DEFVAR *QUOTE-HOSTS-FOR-XMAILR* NIL)

(DEFUN STRING-FROM-HEADER (HEADER FORMAT &AUX (PLIST (LOCF HEADER)) TEM)
  (IF (AND (EQ FORMAT :USE-ORIGINAL)
           (SETQ TEM (GET PLIST :INTERVAL)))
      (STRING-INTERVAL (FIRST TEM) (SECOND TEM) T)
    (LET ((STRING (GET PLIST :NAME)))
      ;; If talking to the server, strip off any quoting
      (AND (EQ FORMAT :HOST)
           (QUOTED-RECIPIENT-P STRING)
           (SETQ STRING (SUBSTRING STRING 1 (1- (STRING-LENGTH STRING)))))
      (LET ((HOST (GET PLIST :HOST))
            (AT (IF (MEMQ FORMAT '(:INCLUDE-PERSONAL :SHORT :HOST)) #/@ " at ")))
        (IF (AND HOST *QUOTE-HOSTS-FOR-XMAILR*)
            (SETQ STRING (STRING-APPEND STRING AT #/ (CAR HOST) #/))
          (DO ((HS HOST (CDR HS)))
              ((NULL HS))
            (SETQ STRING (STRING-APPEND STRING (IF (CDR HS) "%" AT) (CAR HS))))))
      (AND (MEMQ FORMAT '(:USE-ORIGINAL :INCLUDE-PERSONAL))
           (SETQ TEM (GET PLIST :PERSONAL-NAME))
           (PROGN
             (AND (NOT (QUOTED-RECIPIENT-P TEM))
                  (STRING-SEARCH-SET "/"," TEM)
                  (SETQ TEM (FORMAT:OUTPUT NIL (PRINT TEM))))
             (SETQ STRING (STRING-APPEND TEM " <" STRING #/>))))
      STRING)))

(DEFUN QUOTED-RECIPIENT-P (STRING)
  (LET ((LEN (STRING-LENGTH STRING)))
    (AND (> LEN 1)
         (char= (char STRING 0) #/")
         (char= (char STRING (1- LEN)) #/"))))

(DEFUN INSERT-REPLY-HEADER-LIST (BP LIST TYPE &AUX NAME)
  (SETQ NAME (HEADER-TYPE-NAME TYPE))
  (OR (BEG-LINE-P BP) (INSERT-MOVING BP #/CR))
  (DO ((L LIST (CDR L))
       (FLAG NIL T)
       (BP1 (COPY-BP BP)))
      ((NULL L) (AND FLAG (INSERT-MOVING BP #/CR)))
    (IF FLAG
        (INSERT-MOVING BP #/,)
        (INSERT-MOVING BP NAME)
        (INSERT-MOVING BP ": "))
    (MOVE-BP BP1 BP)
    (INSERT-MOVING BP (STRING-FROM-HEADER (CAR L) *REPLY-HEADER-FORMAT*))
    (INSERT BP1 (IF (< (BP-INDEX BP) 72.) #/Space "
    "))))

;;; Editor commands in *REPLY-COMTAB*.

;;; Mail from inside mail
(DEFCOM COM-ZMAIL-RECURSIVE-MAIL "Start composing another message" ()
  (LET ((OLD-DRAFT-MSG *DRAFT-MSG*))
    (SAVE-DRAFT-MSG-WINDOW-STATE OLD-DRAFT-MSG) ;Save point
    (LET (*DRAFT-MSG* *DRAFT-HEADER-INTERVAL* *DRAFT-TEXT-INTERVAL*)
      (INITIALIZE-FOR-MAIL (MAKE-DRAFT-MSG) :SUBJECT-TOO)       ;Start a new message
      (SET-MAIL-WINDOW-CONFIGURATION NIL :HEADER)
      (INSERT-MOVING (POINT) "To: ")
      (INSERT (POINT) #/RETURN)
      ;; Avoid an extra blank line at end of headers.
      (WHEN (char= (BP-CHAR-BEFORE (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)) #/RETURN)
        (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) -1)
                         (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) T))
      (*CATCH 'SEND-IT
        (SEND *WINDOW* :EDIT))                  ;Edit it
      (RESEPARATE-HEADER-AND-TEXT))
    (INITIALIZE-FOR-MAIL OLD-DRAFT-MSG)         ;Restore old one
    (MAKE-WINDOW-CURRENT (CAAR (DRAFT-MSG-WINDOW-POINTS OLD-DRAFT-MSG))))
  DIS-NONE)

;;; Abort it
(DEFCOM COM-ABORT-SEND "Abort sending of this message
If you really want to forget about this message after aborting, use s-Abort. " ()
  (FORMAT QUERY-IO "~&Aborting, use the /"Continue/" command to continue.")
  (*THROW 'SEND-IT NIL))

(DEFCOM COM-REALLY-ABORT-SEND "Abort this message, and forget about its draft" ()
  (FORMAT QUERY-IO "~&Aborting.")
  (SETQ *DRAFT-LIST* (DELQ *DRAFT-MSG* *DRAFT-LIST*))
  (*THROW 'SEND-IT NIL))

(DEFCOM COM-MAIL-END "Send message unless in header area" ()
  (IF *END-SENDS-MESSAGE-P*
      (COM-SEND-MESSAGE)
      (COM-ADD-MORE-TEXT)))

(DEFCOM COM-ZMAIL-OTHER-WINDOW "Move to another other window" ()
  (IF *NUMERIC-ARG-P*
      (SELECT-NUMBERED-WINDOW *NUMERIC-ARG*)
    (MAKE-WINDOW-CURRENT (OR (OTHER-WINDOW) (BARF))))
  (SETQ *END-SENDS-MESSAGE-P* (NEQ *WINDOW* *DRAFT-HEADER-WINDOW*))
  DIS-NONE)

(DEFCOM COM-ZMAIL-REPLY-TWO-WINDOWS "Switch to separate header and text windows." ()
  (SET-MAIL-WINDOW-CONFIGURATION :SEND)
  (AND (EQ *WINDOW* *MSG-WINDOW*)
       (MAKE-WINDOW-CURRENT *DRAFT-TEXT-WINDOW*))
  DIS-NONE)

(DEFCOM COM-ZMAIL-REPLY-ONE-WINDOW "Use one window for headers and text together." ()
  (SET-MAIL-WINDOW-CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*
                                 *WINDOW*)
  DIS-ALL)

(DEFCOM COM-ZMAIL-REPLY-THREE-WINDOWS
        "Draft headers, draft text, and message being replied to on top." ()
  (OR *MSGS-BEING-REPLIED-TO-INTERVAL* *MSG*
      (BARF "There is no current message."))
  (SET-MAIL-WINDOW-CONFIGURATION :REPLY)
  DIS-NONE)

;;;; Redistribution

;; Copied from LAD: RELEASE-3.ZMAIL; MAIL.LISP#320 on 2-Oct-86 03:08:24
(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REDISTRIBUTE-MSG "Redistribute this message" ()
  (REDISTRIBUTE-MSG *MSG* (GET-REDISTRIBUTE-RECIPIENTS))
        (ZMAIL-SELECT-MSG *MSG*)        ;Fixes bug 339 "attributes not being updated immediately after redistribution"
  DIS-NONE)

;; Copied from LAD: RELEASE-3.ZMAIL; MAIL.LISP#320 on 2-Oct-86 03:08:25
(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REDISTRIBUTE-ALL "Redistribute these messages" ()
  (LOOP WITH RECIPIENTS = (GET-REDISTRIBUTE-RECIPIENTS)
        FOR MSG BEING THE MSGS IN *ZMAIL-BUFFER*
        DO (REDISTRIBUTE-MSG MSG RECIPIENTS))
        (ZMAIL-SELECT-MSG *MSG*)        ;Fixes bug 339 "attributes not being updated immediately after redistribution"
  DIS-NONE)

(DEFUN GET-REDISTRIBUTE-RECIPIENTS (&AUX RECIPIENTS)
  (SETQ RECIPIENTS (TYPEIN-LINE-READLINE "Redistribute to:"))
  (SETQ RECIPIENTS (PARSE-ADDRESSES RECIPIENTS))
  (OR (CONSP RECIPIENTS) (BARF "Cannot parse recipients: ~A" RECIPIENTS))
  RECIPIENTS)

(DEFUN REDISTRIBUTE-MSG (MSG RECIPIENTS &AUX (SENDING-MODE *MAIL-SENDING-MODE*)
                                             (*INTERVAL* (MSG-INTERVAL MSG))
                                             (START-BP (INTERVAL-FIRST-BP *INTERVAL*))
                                             (END-BP (INTERVAL-LAST-BP *INTERVAL*))
                                             HEADER-INTERVAL BODY-INTERVAL
                                             HEADERS-END-BP LIST
                                             (PLIST (LOCF LIST)))
  (SETQ HEADERS-END-BP (OR (GET (ASSURE-MSG-PARSED MSG) 'HEADERS-END-BP)
                           (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
                                (END-LINE (BP-LINE END-BP)))
                               (NIL)
                             (AND (OR (EQ LINE END-LINE)
                                      (LINE-BLANK-P LINE))
                                  (RETURN (CREATE-BP LINE 0))))))
  (DO ((LINE (BP-LINE HEADERS-END-BP) PREV)
       (PREV)
       (BEG-LINE (BP-LINE START-BP)))
      ((OR (EQ LINE BEG-LINE)
           (NOT (LINE-BLANK-P (SETQ PREV (LINE-PREVIOUS LINE)))))
       (SETQ HEADERS-END-BP (CREATE-BP LINE 0))))
  (SETQ HEADER-INTERVAL (CREATE-INTERVAL (MSG-START-BP MSG) HEADERS-END-BP))
  (DO ((LINE (BP-LINE HEADERS-END-BP) (LINE-NEXT LINE))
       (END-LINE (BP-LINE END-BP)))
      ((OR (EQ LINE END-LINE)
           (NOT (LINE-BLANK-P LINE)))
       (SETQ HEADERS-END-BP (CREATE-BP LINE 0))))
  (SETQ BODY-INTERVAL (CREATE-INTERVAL HEADERS-END-BP (MSG-END-BP MSG)))
  (SETQ LIST `(:REDISTRIBUTED-TO ,RECIPIENTS
                                 REDISTRIBUTED-HEADERS ,HEADER-INTERVAL))
  (AND (EQ SENDING-MODE 'FILE-SEND-IT) (SETQ SENDING-MODE 'CHAOS-SEND-IT))
  (FUNCALL SENDING-MODE PLIST BODY-INTERVAL
           '(REDISTRIBUTED-HEADERS
              :REDISTRIBUTED-TO :REDISTRIBUTED-BY :REDISTRIBUTED-DATE))
  (MSG-PUT MSG T 'REDISTRIBUTED))

;;;; Special mail composing commands to do editing.

(DEFCOM COM-ADD-TO-FIELD "Add another to recipient.
With a negative argument, removes the to field
With a zero argument, only selects the start of that field." ()
  (ADD-RECIPIENT-FIELD :TO)
  DIS-TEXT)

(DEFCOM COM-ADD-CC-FIELD "Add another cc recipient.
With a negative argument, removes the cc field
With a zero argument, only selects the start of that field." ()
  (ADD-RECIPIENT-FIELD :CC)
  DIS-TEXT)

(DEFUN ADD-RECIPIENT-FIELD (TYPE &AUX BP BP2)
  (MULTIPLE-VALUE (BP BP2)
    (ADD-HEADER-FIELD TYPE (MINUSP *NUMERIC-ARG*) (MINUSP *NUMERIC-ARG*)))
  (AND BP2 (PLUSP *NUMERIC-ARG*) (NOT (END-LINE-P BP))
       (LET ((BP3 (END-LINE BP2 -1)))
         (IF (< (BP-INDENTATION BP3) *FILL-COLUMN*)
             (SETQ BP2 (INSERT BP3 ", "))
             (INSERT BP3 #/,)
             (COND ((NOT (LINE-BLANK-P (BP-LINE BP2)))
                    (INSERT BP2 #/CR)
                    (SETQ BP2 (BEG-LINE BP2 -1))))
             (INSERT-MOVING BP2 "    "))
         (MOVE-BP (POINT) BP2))))

(DEFVAR *SUBJECT-PRONOUN-FROM-LIST*)
(DEFVAR *SUBJECT-PRONOUN-TO-LIST*)
(DEFUN INITIALIZE-SUBJECT-PRONOUN-LISTS (&AUX L1 L2)
  (DOLIST (ELEM '(("i" "you") ("me" "you") ("my" "your")
                  ("mine" "yours") ("here" "there")))
    (PUSH (CAR ELEM) L1)
    (PUSH (CADR ELEM) L2)
    (PUSH (CADR ELEM) L1)
    (PUSH (CAR ELEM) L2))
  (SETQ *SUBJECT-PRONOUN-FROM-LIST* (NREVERSE L1)
        *SUBJECT-PRONOUN-TO-LIST* (NREVERSE L2)))

(INITIALIZE-SUBJECT-PRONOUN-LISTS)

(DEFCOM COM-CHANGE-SUBJECT-PRONOUNS "Correct pronouns the subject field (me <-> you)." ()
  (ADD-HEADER-FIELD :SUBJECT NIL NIL)
  (LET ((*INTERVAL* (CREATE-INTERVAL (COPY-BP (POINT) :NORMAL)
                                     (COPY-BP (BEG-LINE (POINT) 1 T) :MOVES))))
    (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
                        *SUBJECT-PRONOUN-FROM-LIST* *SUBJECT-PRONOUN-TO-LIST* T))
  DIS-TEXT)

(DEFCOM COM-ADD-SUBJECT-FIELD "Add a subject field.
With a negative argument, removes the subject field
With a zero argument, only selects the start of that field." ()
  (ADD-HEADER-FIELD :SUBJECT (NOT (ZEROP *NUMERIC-ARG*)) (MINUSP *NUMERIC-ARG*))
  DIS-TEXT)

(DEFCOM COM-ADD-FROM-FIELD "Add a from field.
With a negative argument, removes the from field.
With a zero argument, only selects the start of that field." ()
  (ADD-HEADER-FIELD :FROM (NOT (ZEROP *NUMERIC-ARG*)) (MINUSP *NUMERIC-ARG*))
  DIS-TEXT)

(DEFUN ADD-HEADER-FIELD (TYPE DELETE-P DELETE-HEADER-TOO-P
                         &OPTIONAL ALWAYS-CREATE-NEW DONT-MOVE-POINT JUST-FIND
                         &AUX NAME FIRST-BP LAST-BP
                         STOP-AT-BLANK BP BP2)
  (SETQ NAME (HEADER-TYPE-NAME TYPE))
  (COND (*INSIDE-MAIL*
         (SET-MAIL-WINDOW-CONFIGURATION NIL :HEADER)
         (SETQ FIRST-BP (INTERVAL-FIRST-BP *DRAFT-HEADER-INTERVAL*)
               LAST-BP (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)))
        (T
         (SETQ FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)
               LAST-BP (INTERVAL-LAST-BP *INTERVAL*)
               STOP-AT-BLANK T)))
  (OR (BEG-LINE-P LAST-BP) (INSERT LAST-BP #/CR))
  (DO ((LINE (BP-LINE FIRST-BP) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE LAST-BP))
       (LEN (STRING-LENGTH NAME)))
      (NIL)
    (AND STOP-AT-BLANK
         (OR (LINE-BLANK-P LINE)
             (STRING-EQUAL LINE *MAIL-HEADER-DELIMITER*))
         (RETURN (SETQ LAST-BP (CREATE-BP LINE 0))))
    (AND (> (LINE-LENGTH LINE) LEN)
         (NOT ALWAYS-CREATE-NEW)
         (STRING-EQUAL LINE NAME :start1 0 :start2 0 :end1 LEN :end2 LEN)
         (char= (char LINE LEN) #/:)
         (RETURN (SETQ BP (CREATE-BP LINE (1+ LEN)))))
    (AND (EQ LINE LAST-LINE)
         (RETURN NIL)))
  (COND (JUST-FIND)
        ((NULL BP)
         (SETQ BP (COPY-BP LAST-BP))
         (COND ((NOT DELETE-HEADER-TOO-P)
                (INSERT-MOVING BP NAME)
                (INSERT-MOVING BP ": ")
                (INSERT BP #/CR))))
        (T
         (IF DELETE-HEADER-TOO-P
             (SETQ BP (BEG-LINE BP))
             (SETQ BP (FORWARD-OVER *BLANKS* BP)))
         (SETQ BP2 (CREATE-BP (DO ((LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE))
                                   (LAST-LINE (BP-LINE LAST-BP)))
                                  ((EQ LINE LAST-LINE) LINE)
                                (OR (AND (NOT (ZEROP (LINE-LENGTH LINE)))
                                         (MEMQ (AREF LINE 0) *BLANKS*))
                                    (RETURN LINE)))
                              0))

         (COND (DELETE-P
                (AND
                  (NOT DELETE-HEADER-TOO-P)
                  (SETQ BP2 (END-LINE BP2 -1)))
                (WITH-BP (SAVE-BP BP :NORMAL)
                  (KILL-INTERVAL BP BP2 T)
                  (MOVE-BP BP SAVE-BP)
                  (UNLESS DONT-MOVE-POINT
                    (SETQ *CURRENT-COMMAND-TYPE* 'KILL)))))))
  (UNLESS DONT-MOVE-POINT
    (POINT-PDL-PUSH (POINT) *WINDOW*)
    (MOVE-BP (POINT) BP))
  (VALUES BP BP2))

(DEFCOM COM-ADD-MORE-TEXT "Reselect the text portion of the message" ()
  (COND ((AND *INSIDE-MAIL*
              (TV:SHEET-EXPOSED-P *DRAFT-HEADER-WINDOW*))
         (SET-MAIL-WINDOW-CONFIGURATION NIL :TEXT)
         DIS-NONE)
        (T
         (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*))
         DIS-BPS)))

;;; Primitives for use in templates defined by DEFINE-MAIL-TEMPLATE.
;;; *INTERVAL* may be a draft message or a message or a ZMACS buffer.

(DEFUN ADD-FIELD (TYPE CONTENTS)
  "Add a new TYPE header field with contents CONTENTS.
TYPE should be a keyword, such as :CC or :SUBJECT, and CONTENTS a string.
A new field is added even if other fields of the same type exist already."
  (LET ((BP1 (ADD-HEADER-FIELD TYPE NIL NIL T T)))
    (INSERT BP1 CONTENTS)))

(DEFUN DEFAULT-FIELD (TYPE CONTENTS)
  "Default the TYPE header field to contents CONTENTS.
TYPE should be a keyword, such as :CC or :SUBJECT, and CONTENTS a string.
If there is already a TYPE field with nonempty contents, nothing is done."
  (MULTIPLE-VALUE-BIND (BP1 BP2)
      (ADD-HEADER-FIELD TYPE NIL NIL NIL T)
    (UNLESS (AND BP2 (NOT (BP-= BP1 (FORWARD-CHAR BP2 -1))))
      ;; BP2 is non-NIL if we found an existing field.
      (INSERT BP1 CONTENTS))))

(DEFUN DELETE-FIELD (TYPE)
  "Delete any TYPE header fields.  TYPE should be a keyword such as :CC or :SUBJECT."
  (DO-FOREVER
    (MULTIPLE-VALUE-BIND (NIL VALUE)
        (ADD-HEADER-FIELD TYPE T T NIL T)
      (UNLESS VALUE (RETURN NIL)))))

(DEFUN ADD-TEXT-END (TEXT)
  "Add TEXT, a string, to the text of the message at the end."
  (INSERT (INTERVAL-LAST-BP *INTERVAL*) TEXT))

(DEFUN ADD-TEXT-START (TEXT)
  "Add TEXT, a string, to the text of the message at the start of the text."
  (LET ((BP (ADD-HEADER-FIELD 'THISWONTBEFOUND T T NIL T)))
    (INSERT (FORWARD-LINE BP 1 T) TEXT)))

(DEFUN FIND-FIELD (TYPE)
  "Return a BP to the first header field of type TYPE, or NIL if there is none."
  (ADD-HEADER-FIELD TYPE NIL NIL NIL T T))

;;; Pruning headers of yanked messages.

(DEFVAR *PRUNE-YANKED-HEADERS-KEEP-HEADERS* '(:DATE :FROM :ITS :UNKNOWN))

(DEFCOM COM-PRUNE-YANKED-HEADERS "Shorten up the headers on a yanked message" ()
  (PROG TOP ((BP (COPY-BP (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))))
    (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
         (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *DRAFT-TEXT-INTERVAL*)))
         (STATE NIL)
         (TYPE))
        (NIL)
      (SETQ TYPE (HEADER-LINE-TYPE LINE))
      (COND ((MEMQ TYPE '(:BLANK :MALFORMATTED))                ;End of headers
             (COND (STATE
                    (KILL-INTERVAL BP (CREATE-BP LINE 0) T)     ;Things to be flushed
                    (SETQ *LAST-COMMAND-TYPE* 'KILL     ;Append any more kills
                          *CURRENT-COMMAND-TYPE* 'KILL)))
             (MOVE-BP (POINT) LINE 0)
             (UNLESS (EQ TYPE :BLANK)
               (INSERT-MOVING (POINT) #/RETURN))
             (RETURN-FROM TOP NIL))
            ((MEMQ TYPE *PRUNE-YANKED-HEADERS-KEEP-HEADERS*)
             (COND (STATE                       ;Things to be flushed
                    (DELETE-INTERVAL BP (CREATE-BP LINE 0) T)
                    (SETQ STATE NIL))))         ;No more
            (T
             (COND ((NOT STATE)                         ;Start flushing here
                    (MOVE-BP BP LINE 0)
                    (SETQ STATE T)))))
      (AND (EQ LINE LAST-LINE) (RETURN-FROM TOP NIL))))
  DIS-TEXT)

(DEFUN HEADER-LINE-TYPE (LINE &AUX LEXEMES)
  (IF (ATOM (SETQ LEXEMES (RFC733-LEXER LINE 0 NIL NIL)))
      :MALFORMATTED
      (CASE (CAAR LEXEMES)
        (EOF :BLANK)
        (ATOM
         (CASE (CAADR LEXEMES)
           ((ATSIGN AT-ATOM)
            (IF (EQ (CAADDR LEXEMES) 'ATOM)
                :ITS :MALFORMATTED))
           (COLON
            (OR (CDR (ASS 'EQUALP (CADAR LEXEMES) *HEADER-NAME-ALIST*))
                :UNKNOWN))
           (OTHERWISE
            :MALFORMATTED)))
        (OTHERWISE
         :MALFORMATTED))))

(DEFCOM COM-ADD-FCC-FIELD "Add another file CC recipient.
You specify the file with a menu." ()
  (LET ((ZMAIL-BUFFER (AND (NOT *NUMERIC-ARG-P*)
                           (OR (GET-MOVE-ZMAIL-BUFFER)
                               (ABORT-CURRENT-COMMAND)))))
    (ADD-RECIPIENT-FIELD :FCC)
    (AND ZMAIL-BUFFER (INSERT-MOVING (POINT) (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))))
  DIS-TEXT)

(DEFCOM COM-ADD-FTO-FIELD "Add another file To recipient.
You specify the file with a menu." ()
  (LET ((ZMAIL-BUFFER (AND (NOT *NUMERIC-ARG-P*)
                           (OR (GET-MOVE-ZMAIL-BUFFER)
                               (ABORT-CURRENT-COMMAND)))))
    (ADD-RECIPIENT-FIELD :FTO)
    (AND ZMAIL-BUFFER (INSERT-MOVING (POINT) (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))))
  DIS-TEXT)

(DEFCOM COM-ADD-IN-REPLY-TO-FIELD "Add an in-reply-to field" ()
  (LET ((IN-REPLY-TO (GENERATE-IN-REPLY-TO-HEADER)))
    (OR IN-REPLY-TO (BARF))
    (ADD-HEADER-FIELD :IN-REPLY-TO T T)
    (INSERT-MOVING (POINT)
                   (WITH-OUTPUT-TO-STRING (STREAM)
                     (PRINT-HEADER STREAM IN-REPLY-TO :IN-REPLY-TO))))
  DIS-TEXT)

(DEFUN GENERATE-IN-REPLY-TO-HEADER ()
  (APPLY 'GENERATE-REFERENCE-HEADER (DRAFT-MSG-MSGS-BEING-REPLIED-TO *DRAFT-MSG*)))

(DEFUN GENERATE-REFERENCE-HEADER (&REST MSGS &AUX TEM)
  (LOOP FOR MSG IN MSGS
        AS STATUS = (ASSURE-MSG-PARSED MSG)
        COLLECT (IF (SETQ TEM (GET STATUS :MESSAGE-ID))
                    `(:MESSAGE-ID ,TEM)
                    `(:DATE ,(GET STATUS :DATE)
                      :FROM ,(CAR (GET STATUS :FROM))))))

(DEFCOM COM-ADD-REFERENCES-FIELD "Add a References field" ()
  (LET ((REFERENCES (REFERENCES-FOR-ZMAIL-BUFFER *ZMAIL-BUFFER*)))
    (OR REFERENCES (BARF))
    (ADD-HEADER-TYPE-FIELD REFERENCES :REFERENCES))
  DIS-TEXT)

(DEFUN ADD-HEADER-TYPE-FIELD (VAL TYPE)
  (ADD-HEADER-FIELD TYPE T T)
  (INSERT-MOVING (POINT)
                 (WITH-OUTPUT-TO-STRING (STREAM)
                   (PRINT-HEADER STREAM VAL TYPE))))

(DEFUN REFERENCES-FOR-ZMAIL-BUFFER (ZMAIL-BUFFER)
  (APPLY-ARRAY 'GENERATE-REFERENCE-HEADER (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))

(DEFCOM COM-ADD-EXPIRATION-DATE-FIELD "Add an Expiration-date field" ()
  (ADD-DATE-FIELD :EXPIRATION-DATE))

(DEFUN ADD-DATE-FIELD (FIELD &AUX DATE)
  (SETQ DATE (TYPEIN-LINE-READLINE "Add ~A:" (HEADER-TYPE-NAME FIELD)))
  (CONDITION-CASE (ERROR)
      (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME DATE))
    (ERROR (BARF "Bad date: ~A" ERROR)))
  (ADD-HEADER-TYPE-FIELD DATE FIELD)
  DIS-TEXT)

(DEFCOM COM-ZMAIL-YANK "Insert the message being replied to" ()
  (LET ((CONFIG *WINDOW-CONFIGURATION*))
    (AND *ONE-WINDOW-AFTER-YANK*
         (EQ CONFIG :REPLY)
         (SET-MAIL-WINDOW-CONFIGURATION :SEND)))
  (if (not *msg*)
      (barf "There is no current message to yank.")
    (progn (INSERT-MSGS-INTO-WINDOW (IF (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P)
                                        *DRAFT-TEXT-WINDOW* *MSG-WINDOW*)
                                    (NOT *NUMERIC-ARG-P*))
           (AND *PRUNE-HEADERS-AFTER-YANKING*
                (COM-PRUNE-YANKED-HEADERS))
           (SETQ *CURRENT-COMMAND-TYPE* 'YANK)))
  DIS-TEXT)

(DEFCOM COM-ZMAIL-YANK-CURRENT-MSG "Insert the current message" ()
  (LET ((*MSGS-BEING-REPLIED-TO-INTERVAL* NIL)) ;Force use of *MSG*
    (COM-ZMAIL-YANK)))

(DEFUN INSERT-MSGS-INTO-WINDOW (WINDOW INDENT-P &OPTIONAL MSG)
  (LET ((POINT (WINDOW-POINT WINDOW))
        (MARK (WINDOW-MARK WINDOW)))
    (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
    (MOVE-BP MARK POINT)
    (POINT-PDL-PUSH POINT WINDOW NIL NIL)
    (AND (NULL MSG)
         (NULL *MSGS-BEING-REPLIED-TO-INTERVAL*)
         (SETQ MSG *MSG*))
    (INSERT-INTERVAL-MOVING POINT (IF (NULL MSG)
                                      *MSGS-BEING-REPLIED-TO-INTERVAL*
                                      (MSG-INTERVAL MSG)))
    (AND INDENT-P
         (LET ((NON-BLANK-POINT (BACKWARD-OVER '(#/CR) POINT)))
           (INTERVAL-LINES (MARK NON-BLANK-POINT) (START-LINE STOP-LINE)
             (DO ((LINE START-LINE (LINE-NEXT LINE))
                  (DELTA (* 4 (FONT-SPACE-WIDTH))))
                 ((EQ LINE STOP-LINE))
               (OR (LINE-BLANK-P LINE)
                   (INDENT-LINE (CREATE-BP LINE 0) (+ DELTA (LINE-INDENTATION LINE))))))))
    POINT))

;;; Saving and restoring draft messages

(DEFCOM COM-RESTORE-DRAFT-FILE "Restore the saved text of the message from the given file." ()
  (READ-DEFAULT-DRAFT-PATHNAME "Restore draft" (DRAFT-MSG-PATHNAME *DRAFT-MSG*))
  (RESTORE-DRAFT-INTERNAL *DRAFT-HEADER-INTERVAL* *DRAFT-TEXT-INTERVAL*)
  (IF (NOT (SEND *DRAFT-HEADER-WINDOW* :EXPOSED-P))
      (MUST-REDISPLAY *MSG-WINDOW* DIS-TEXT)
    (MUST-REDISPLAY *DRAFT-HEADER-WINDOW* DIS-TEXT)
    (MUST-REDISPLAY *DRAFT-TEXT-WINDOW* DIS-TEXT))
  (SET-MAIL-WINDOW-CONFIGURATION NIL :TEXT)
  (MOVE-BP (POINT) (INTERVAL-LAST-BP *DRAFT-TEXT-INTERVAL*))
  DIS-NONE)

(DEFUN MAKE-DRAFT-MSG-FROM-RESTORED-DRAFT (&AUX *DRAFT-MSG*)
  (CALL-POP-UP-MINI-BUFFER-EDITOR :MOUSE 'READ-DEFAULT-DRAFT-PATHNAME "Restore draft")
  (SETQ *DRAFT-MSG* (MAKE-DRAFT-MSG))
  (RESTORE-DRAFT-INTERNAL (DRAFT-MSG-HEADER-INTERVAL *DRAFT-MSG*)
                          (DRAFT-MSG-REPLY-INTERVAL *DRAFT-MSG*))
  (SETF (DRAFT-MSG-PATHNAME *DRAFT-MSG*) *DEFAULT-DRAFT-FILE-NAME*)
  *DRAFT-MSG*)

(DEFUN RESTORE-DRAFT-INTERNAL (HEADER-INTERVAL REPLY-INTERVAL)
  (WITH-OPEN-FILE (STREAM *DEFAULT-DRAFT-FILE-NAME* '(:IN))
    (DELETE-INTERVAL HEADER-INTERVAL)
    (DO ((INT-STREAM (INTERVAL-STREAM HEADER-INTERVAL))
         (LINE))
        (NIL)
      (AND (OR (NULL (SETQ LINE (SEND STREAM :LINE-IN LINE-LEADER-SIZE)))
               (ZEROP (ARRAY-ACTIVE-LENGTH LINE)))
           (RETURN NIL))
      (SEND INT-STREAM :LINE-OUT LINE))
    (DELETE-INTERVAL REPLY-INTERVAL)
    (STREAM-INTO-BP STREAM (INTERVAL-LAST-BP REPLY-INTERVAL))
    (SETF (DRAFT-MSG-STARTING-TICK *DRAFT-MSG*) *TICK*)))

(DEFCOM COM-SAVE-DRAFT-FILE "Save the text of the message being composed." ()
  (AND (NULL (DRAFT-MSG-PATHNAME *DRAFT-MSG*))
       (READ-DEFAULT-DRAFT-PATHNAME "Save draft" NIL :WRITE))
  (SAVE-DRAFT-FILE-INTERNAL))

(DEFCOM COM-WRITE-DRAFT-FILE "Save the text of the message being composed to the given file."
        ()
  (READ-DEFAULT-DRAFT-PATHNAME "Write draft" (DRAFT-MSG-PATHNAME *DRAFT-MSG*) :WRITE)
  (SAVE-DRAFT-FILE-INTERNAL))

(DEFUN SAVE-DRAFT-FILE-INTERNAL ()
  (WITH-OPEN-FILE (STREAM *DEFAULT-DRAFT-FILE-NAME* '(:OUT))
    (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP *DRAFT-HEADER-INTERVAL*)) (LINE-NEXT LINE))
         (END-LINE (BP-LINE (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*))))
        ((NULL LINE))
      (OR (ZEROP (LINE-LENGTH LINE))            ;Don't write blank lines here
          (SEND STREAM :LINE-OUT LINE))
      (AND (EQ LINE END-LINE)
           (RETURN NIL)))
    (SEND STREAM :TYO #/CR)                     ;Separate by blank line
    (COPY-INTERVAL-TO-STREAM STREAM *DRAFT-TEXT-INTERVAL*)
    (CLOSE STREAM)
    (FORMAT QUERY-IO "~&Written: ~A" (SEND STREAM :TRUENAME)))
  (SETF (DRAFT-MSG-PATHNAME *DRAFT-MSG*) *DEFAULT-DRAFT-FILE-NAME*)
  DIS-NONE)

(DEFUN READ-DEFAULT-DRAFT-PATHNAME (PROMPT &OPTIONAL DEFAULT (DIRECTION :READ))
  (SETQ *DEFAULT-DRAFT-FILE-NAME* (READ-DEFAULTED-PATHNAME
                                    PROMPT
                                    (OR DEFAULT
                                        *DEFAULT-DRAFT-FILE-NAME*
                                        (SEND (FS:USER-HOMEDIR) :NEW-NAME "DRAFT"))
                                    NIL NIL DIRECTION)))

(DEFVAR *DRAFT-MSG-INTERNAL-HEADERS* '(:DATE :DRAFT-COMPOSITION-DATE))

(DEFUN MAKE-DRAFT-MSG-FROM-MSG (MSG &AUX DRAFT-MSG)
  (OR (MSG-DRAFT-MSG-P MSG)
      (BARF "This is not a draft message"))
  (COND ((AND (SETQ DRAFT-MSG (MSG-GET MSG 'DRAFT-MSG))
              (= (NODE-TICK (MSG-INTERVAL MSG))
                 (NODE-TICK DRAFT-MSG))))       ;Already one and not modified
        (T
         (SETQ DRAFT-MSG (MAKE-DRAFT-MSG))
         (SETF (DRAFT-MSG-MSG DRAFT-MSG) MSG)
         (SETF (DRAFT-MSG-STARTING-TICK DRAFT-MSG) *TICK*)
         (MSG-PUT MSG DRAFT-MSG 'DRAFT-MSG)
         (LET ((MSG-INT (MSG-INTERVAL MSG))
               (START-BP (INTERVAL-FIRST-BP DRAFT-MSG)))
           ;;Copy the text of the message, except for the composition-date
           (INSERT-INTERVAL START-BP
                            (DO ((LINE (BP-LINE (INTERVAL-FIRST-BP MSG-INT))
                                       (LINE-NEXT LINE)))
                                ((NOT (GETL (LOCF (GET (LOCF (LINE-CONTENTS-PLIST LINE))
                                                       'PARSED-HEADERS))
                                            *DRAFT-MSG-INTERNAL-HEADERS*))
                                 (CREATE-BP LINE 0)))
                            (INTERVAL-LAST-BP MSG-INT)
                            T)
           ;;Separate the headers from the text by finding the first blank line
           (DO ((LINE (BP-LINE START-BP) (LINE-NEXT LINE))
                (END-LINE (BP-LINE (INTERVAL-LAST-BP DRAFT-MSG))))
               ((EQ LINE END-LINE))
             (COND ((LINE-BLANK-P LINE)
                    (MOVE-BP (INTERVAL-LAST-BP (DRAFT-MSG-HEADER-INTERVAL DRAFT-MSG))
                             LINE 0)
                    (MOVE-BP (INTERVAL-FIRST-BP (DRAFT-MSG-REPLY-INTERVAL DRAFT-MSG))
                             (LINE-NEXT LINE) 0)
                    (RETURN))))
           (SETF (NODE-TICK DRAFT-MSG) (TICK))
           (SETF (NODE-TICK MSG-INT) *TICK*))))
  DRAFT-MSG)

(DEFCOM COM-SAVE-DRAFT-AS-MSG "Save the text of the message being composed as a message" ()
  (LET ((MSG (AND (NOT *NUMERIC-ARG-P*) (DRAFT-MSG-MSG *DRAFT-MSG*)))
        (NEW-ZMAIL-BUFFER NIL))
    (COND ((AND MSG (NEQ (MSG-PARSED-P MSG) :KILLED))
           (SETF (MSG-STATUS MSG) (SOME-PLIST (MSG-STATUS MSG) *INTERNAL-TYPE-PROPERTIES*))
           (SETF (MSG-TICK MSG) (TICK))
           (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG MSG))
          (T
           (SETQ NEW-ZMAIL-BUFFER
                 (COND ((NULL *MAIL-FILE-FOR-DRAFTS*)
                        (OR *PRIMARY-ZMAIL-BUFFER*
                            (LOOP FOR MF IN *ZMAIL-BUFFER-LIST*
                                  WHEN (ZMAIL-BUFFER-DISK-P MF)
                                  RETURN MF)
                            (BARF
                              "There is no file buffer to put a draft message in."
                              )))
                       ((GET-ZMAIL-BUFFER-FROM-PATHNAME
                          (FS:MERGE-PATHNAME-DEFAULTS *MAIL-FILE-FOR-DRAFTS*
                                                      *ZMAIL-PATHNAME-DEFAULTS*)
                          T)))
                 MSG (MAKE-EMPTY-MSG))
           (SETF (DRAFT-MSG-MSG *DRAFT-MSG*) MSG)
           (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)))
    (LET ((END-BP (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)))
      (OR (BEG-LINE-P END-BP)
          (INSERT END-BP #/CR)))
    (LET ((END-BP (INTERVAL-LAST-BP *DRAFT-TEXT-INTERVAL*)))
      (OR (BEG-LINE-P END-BP)
          (INSERT END-BP #/CR)))
    (LET ((INTERVAL (MSG-INTERVAL MSG)))
      (DELETE-INTERVAL INTERVAL)
      (LET ((END-BP (INTERVAL-LAST-BP INTERVAL))
            (PLIST (LOCF (MSG-STATUS MSG)))
            (NOW (TIME:GET-UNIVERSAL-TIME)))
        (OR (GET PLIST :DATE) (PUTPROP PLIST NOW :DATE))
        (PUTPROP PLIST NOW :DRAFT-COMPOSITION-DATE)
        (INSERT-MOVING END-BP
                       (WITH-OUTPUT-TO-STRING (STREAM)
                         (OUTPUT-HEADER STREAM PLIST *DRAFT-MSG-INTERNAL-HEADERS*)))
        (INSERT-INTERVAL END-BP *DRAFT-MSG*))
      (SETF (NODE-TICK INTERVAL) (TICK))
      (SETF (NODE-TICK *DRAFT-MSG*) *TICK*))
    (AND NEW-ZMAIL-BUFFER (SEND NEW-ZMAIL-BUFFER :ADD-MSG MSG))
    (FORMAT QUERY-IO "~&Saved in ~A" (ZMAIL-BUFFER-NAME (MSG-MAIL-FILE-BUFFER MSG)))
    (IF NEW-ZMAIL-BUFFER
        (MSG-PUT MSG *DRAFT-MSG* 'DRAFT-MSG)
        (SET-PARSED-MSG-HEADERS MSG))
    (AND (EQ MSG *MSG*)
         (MUST-REDISPLAY *MSG-WINDOW* DIS-TEXT)))
  DIS-NONE)

;;; Sending the composed draft.

(DEFCOM COM-SEND-MESSAGE "Finish the current message" ()
  (RESEPARATE-HEADER-AND-TEXT)
  (LET* ((*REFERENCE-TYPE-HEADERS* NIL)         ;Use text that is there
         (LIST (GET-SEND-HEADERS *DRAFT-HEADER-INTERVAL*))
         (PLIST (LOCF LIST)))
    (SEND-IT PLIST)
    ;; Mark draft as "not changed".
    (SETF (DRAFT-MSG-STARTING-TICK *DRAFT-MSG*) *TICK*)
    (*THROW 'SEND-IT T)))

(DEFUN GET-SEND-HEADERS (BP1 &OPTIONAL BP2 IN-ORDER-P
                         &AUX LIST PLIST TEM SUBJECT)
  (SETQ LIST (PARSE-HEADERS-INTERVAL BP1 BP2 IN-ORDER-P)
        PLIST (LOCF LIST))
  (COND ((SETQ TEM (GET PLIST 'LOSING-HEADERS))
         (BARF "Unrecognized entry in headers: ~A" TEM))
        ((AND (NULL (GET PLIST :TO))
              (NULL (GET PLIST :FTO)))
         (BARF "There are no /"To/" or /"FTo/" recipients")))
  (WHEN (SETQ TEM (GETL PLIST ':(F)))
    (SETF (FIRST TEM) :FROM)
    (SETF (SECOND TEM) (PARSE-ADDRESSES (SECOND TEM))))
  (IF (SETQ TEM (GETL PLIST ':(S))) (SETF (FIRST TEM) :SUBJECT))
  (COND ((NOT (OR (NOT (MEMBER (GET PLIST :SUBJECT) '(NIL "")))
                  (CASE *REQUIRE-SUBJECTS*
                    ((NIL :INIT) T)
                    (:BUG (NOT *SENDING-BUG-REPORT*))
                    (OTHERWISE NIL))
                  (EQUAL "" (SETQ SUBJECT (TYPEIN-LINE-READLINE "Subject for message (or just Return):")))))
         (SETQ LIST (LIST* :SUBJECT SUBJECT LIST))
         (INSERT-MOVING (ADD-HEADER-FIELD :SUBJECT NIL NIL) SUBJECT)))
  LIST)

(DEFINE-ZMAIL-GLOBAL *DEFAULT-SEND-TEMPLATE*
  '(:DATE
    :FROM :SENDER :REPLY-TO
    :SUBJECT
    :TO :FTO :CC
    :FCC
    :IN-REPLY-TO
    :REFERENCES
    :MESSAGE-ID))

;;; Top-level handling of sending a message.  Expands abbreviations and does Fcc: itself
(DEFUN SEND-IT (PLIST &AUX *QUOTE-HOSTS-FOR-XMAILR*)
  (IF *GENERATE-MESSAGE-ID-FIELD*
      (PUTPROP PLIST  (FORMAT () "<[~A].~\time\.~A>"
                              SI:LOCAL-HOST (TIME:GET-UNIVERSAL-TIME) USER-ID)
               :MESSAGE-ID))
  (LET ((FCC (GET PLIST :FCC))
        (FTO (GET PLIST :FTO))
        (BFCC (GET PLIST :BFCC)))
    (WHEN (OR BFCC FCC FTO)
      (LET* ((FCC-PATHNAMES (GET-FCC-PATHNAMES FCC))
             (FTO-PATHNAMES (GET-FCC-PATHNAMES FTO))
             (BFCC-PATHNAMES (GET-FCC-PATHNAMES BFCC))
             FCC-NAMES FTO-NAMES BFCC-NAMES)
        ;; Eliminate duplicates between lists.  FTo beats FCC, FCC beats BFCC.
        (DOLIST (ELT FTO-PATHNAMES)
          (SETQ FCC-PATHNAMES (DELQ ELT FCC-PATHNAMES))
          (SETQ BFCC-PATHNAMES (DELQ ELT BFCC-PATHNAMES)))
        (DOLIST (ELT FCC-PATHNAMES)
          (SETQ BFCC-PATHNAMES (DELQ ELT BFCC-PATHNAMES)))
        (SETQ FCC-NAMES (MAPCAR 'STRING FCC-PATHNAMES)
              FTO-NAMES (MAPCAR 'STRING FTO-PATHNAMES)
              BFCC-NAMES (MAPCAR 'STRING BFCC-PATHNAMES))
        ;; Fill in the defaults in the pathnames before composing message to go out.
        (PUTPROP PLIST FCC-NAMES :FCC)
        (PUTPROP PLIST FTO-NAMES :FTO)
        (PUTPROP PLIST BFCC-NAMES :BFCC)
        (WHEN FTO-PATHNAMES
          (LOOP FOR PATHNAME IN FTO-PATHNAMES
                WITH MSG = (CONSTRUCT-FCC-MSG PLIST *DRAFT-TEXT-INTERVAL*)
                AS BUFFER = (GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME T)
                DO (SEND BUFFER :ADD-MSG MSG)
                (IF (EQ BUFFER *ZMAIL-BUFFER*)
                    (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY))))
        (WHEN FCC-PATHNAMES
          (LOOP FOR PATHNAME IN FCC-PATHNAMES
                WITH MSG = (CONSTRUCT-FCC-MSG PLIST *DRAFT-TEXT-INTERVAL*)
                AS BUFFER = (GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME T)
                DO (SEND BUFFER :ADD-MSG MSG)
                (IF (EQ BUFFER *ZMAIL-BUFFER*)
                    (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY))))
        (WHEN BFCC-PATHNAMES
          (LOOP FOR PATHNAME IN BFCC-PATHNAMES
                WITH MSG = (CONSTRUCT-FCC-MSG PLIST *DRAFT-TEXT-INTERVAL* T)
                AS BUFFER = (GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME T)
                DO (SEND BUFFER :ADD-MSG MSG)
                (IF (EQ BUFFER *ZMAIL-BUFFER*)
                    (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY))))
        (FORMAT QUERY-IO "~&Message moved to buffer~P "
                (+ (LENGTH FTO-PATHNAMES)
                   (LENGTH FCC-PATHNAMES)
                   (LENGTH BFCC-PATHNAMES)))
        (FORMAT:PRINT-LIST
          QUERY-IO "~A"
          (MAPCAR 'FUNCALL (APPEND FTO-PATHNAMES FCC-PATHNAMES BFCC-PATHNAMES)
                  (CIRCULAR-LIST :STRING-FOR-EDITOR))))))
  (CONDITION-CASE (ERROR)
      (FUNCALL *MAIL-SENDING-MODE* PLIST *DRAFT-TEXT-INTERVAL* *DEFAULT-SEND-TEMPLATE*)
    (MAIL-ERROR
      (BARF ERROR)))
  (SETF (DRAFT-MSG-SENT-P *DRAFT-MSG*) T)
  (SETF (DRAFT-MSG-SUMMARY-STRING-TICK *DRAFT-MSG*) -1)
  (DOLIST (MSG (DRAFT-MSG-MSGS-BEING-REPLIED-TO *DRAFT-MSG*))   ;Mark if it was a reply
    (MSG-PUT MSG T 'ANSWERED))
  (DOLIST (MSG (DRAFT-MSG-MSGS-BEING-FORWARDED *DRAFT-MSG*))
    (MSG-PUT MSG T 'FORWARDED))
  (FORMAT QUERY-IO "~&Message sent at ~\time\." (time:get-universal-time)))     ;Erase any aborting message

;;; This is the interface from ZMACS C-X M and (MAIL).
(DEFUN SEND-MESSAGE (HEADER-BP1 HEADER-BP2 HEADER-IN-ORDER-P
                     TEXT-BP1 &OPTIONAL TEXT-BP2 TEXT-IN-ORDER-P
                     &AUX LIST (PLIST (LOCF LIST))
                     *QUOTE-HOSTS-FOR-XMAILR*)
  (SETQ LIST (GET-SEND-HEADERS HEADER-BP1 HEADER-BP2 HEADER-IN-ORDER-P))
  (GET-INTERVAL TEXT-BP1 TEXT-BP2 TEXT-IN-ORDER-P)
  (CONDITION-CASE (ERROR)
      (FUNCALL *MAIL-SENDING-MODE* PLIST (CREATE-INTERVAL TEXT-BP1 TEXT-BP2)
               *DEFAULT-SEND-TEMPLATE*)
    (MAIL-ERROR
      (BARF ERROR))))

;;; This is for failing QSEND and friends
(DEFUN SEND-MESSAGE-STRING (TO MESSAGE &AUX LIST PLIST *QUOTE-HOSTS-FOR-XMAILR*)
  (AND (ERRORP (SETQ TO (PARSE-ADDRESSES TO)))
       (BARF TO))
  (SETQ LIST `(:TO ,TO)
        PLIST (LOCF LIST))
  (FUNCALL *MAIL-SENDING-MODE* PLIST (CREATE-INTERVAL MESSAGE) *DEFAULT-SEND-TEMPLATE*))

(DEFVAR *MAIL-QUEUE-FILE* "DSK: .MAIL.; MAIL >")

;;; If non-NIL, this host is tried first
(DEFINE-SITE-HOST-LIST *MAIL-FILE-HOSTS* :FILE-MAIL-SERVER-HOSTS)

(DEFUN FILE-SEND-IT (PLIST INTERVAL TEMPLATE &AUX FILE-NAME TEM USER HOSTS HOST)
  TEMPLATE                                      ;COMSAT is in charge
  (WHEN (OR (GET PLIST :TO) (GET PLIST :CC))
    (SETQ HOSTS (MAPCAR 'FS:GET-PATHNAME-HOST (OR *MAIL-FILE-HOSTS* '("AI" "ML" "MC"))))
    ;; If the login machine is one of the mail servers, try it first, since it likely offers
    ;; the shortest path.
    (AND (MEMQ FS:USER-LOGIN-MACHINE HOSTS)
         (PUSH FS:USER-LOGIN-MACHINE HOSTS))
    (DOLIST (HOST1 HOSTS)
      (IF (SEND HOST1 :GET-HOST-UNIT T)
          (RETURN (SETQ HOST HOST1))))
    (OR HOST (ZMAIL-ERROR "Cannot connect to a file server"))
    (SETQ FILE-NAME (FS:PARSE-PATHNAME *MAIL-QUEUE-FILE* HOST))
    ;; Use the UNAME for ITS in the file
    (SEND FILE-NAME :HOMEDIR)
    (AND (OR (NULL (SETQ USER (CDR (ASSQ 'FS:ITS FS:USER-UNAMES))))
             (STRING-EQUAL USER USER-ID))
         (SETQ USER USER-ID))
    (IF (NULL (SETQ TEM (GETL PLIST '(:FROM))))
        (PUTPROP PLIST USER :AUTHOR)
      (SETF (CAR TEM) :AUTHOR)
      (SETF (CADR TEM)
            (LET ((FROM (CADR TEM)))
              ;; This is somewhat of a kludge, but COMSAT likes to put in the @MIT-MC itself.
              (STRING-INTERVAL (CAR (GET (LOCF (CAR FROM)) :INTERVAL))
                               (CADR (GET (LOCF (CAR (LAST FROM))) :INTERVAL))))))
    (OR (GET PLIST :HEADER-FORCE) (EQ *DEFAULT-HEADER-FORCE* :NONE)
        (PUTPROP PLIST *DEFAULT-HEADER-FORCE* :HEADER-FORCE))
    (PUTPROP PLIST 'ZMAIL :FROM-PROGRAM)
    (PUTPROP PLIST USER :FROM-UNAME)
    (PUTPROP PLIST USER :FROM-XUNAME)
    (WITH-OPEN-FILE (STREAM FILE-NAME '(OUT))
      (DO ((LIST (CAR PLIST) (CDDR LIST))
           (TYPE) (ITEMS)
           (CC-P NIL NIL))
          ((NULL LIST))
        (SETQ TYPE (CAR LIST)
              ITEMS (CADR LIST))
        (AND (EQ TYPE :CC) (SETQ TYPE :TO CC-P T))
        (OR (CONSP ITEMS) (SETQ ITEMS (NCONS ITEMS)))
        (DO ((ITEMS ITEMS (CDR ITEMS))
             (ITEM) (HOST)
             (OPEN-P NIL NIL))
            ((NULL ITEMS))
          (SETQ ITEM (CAR ITEMS))
          (IF (NEQ TYPE :TO)
              (FORMAT STREAM "~A:~A~%" TYPE ITEM)
            (LET ((PL (LOCF ITEM)))
              (PSETQ ITEM (GET PL :NAME)
                     HOST (GET PL :HOST)))
            (AND (NOT (NULL (CDR HOST)))
                 (SETQ ITEM (FORMAT NIL "~A~{@~A~^~}" ITEM (CDR HOST))))
            (SETQ HOST (CAR HOST))
            ;; There is no consistent way to send structured recipients.
            ;; Also, quoting doesn't work right.
            (LET ((LEN-1 (1- (STRING-LENGTH ITEM))))
              (COND ((MINUSP LEN-1))
                    ((AND (char= (char ITEM 0) #/()
                          (char= (char ITEM LEN-1) #/)))
                     (SETQ ITEM (SUBSTRING ITEM 1 LEN-1)
                           OPEN-P T))
                    ((AND (char= (char ITEM 0) #/")
                          (char= (char ITEM LEN-1) #/"))
                     (SETQ OPEN-P T))))
            (FORMAT STREAM "~A:(~:[/"~A/"~;~A~]~@[ ~A~]~:[~; (R-OPTION CC)~])~%"
                    TYPE OPEN-P ITEM HOST CC-P))))
      (SEND STREAM :LINE-OUT "TEXT;-1")
      (COPY-INTERVAL-TO-STREAM STREAM INTERVAL))))

;;;; Sending CHAOS and DIRECT-CHAOS.

(DEFINE-SITE-HOST-LIST *MAIL-CHAOS-HOSTS* :CHAOS-MAIL-SERVER-HOSTS)

(DEFUN CHAOS-SEND-IT (PLIST INTERVAL TEMPLATE &AUX (HOSTS (COPYLIST *MAIL-CHAOS-HOSTS*)))
  (CANONICALIZE-HEADERS PLIST)
  ;; If the first on the list doesn't seem up in a short test,
  ;; ask all the hosts and find one that is up, as quickly as possible.
  (UNLESS (CHAOS:HOST-UP-P (CAR HOSTS) 120.)
    (LET ((UP (CHAOS:UP-HOSTS *MAIL-CHAOS-HOSTS* 1)))
      (WHEN UP
        (PUSH (CAR UP) HOSTS))))
  (LET ((RECIPIENTS (GET-MAIL-RECIPIENTS PLIST)))
    (AND RECIPIENTS
         (LOOP FOR HOST IN HOSTS
               ALWAYS (CHAOS-SEND-IT-1 HOST RECIPIENTS PLIST INTERVAL TEMPLATE))
         (ZMAIL-ERROR "Cannot connect to a MAIL server")))
  (LET ((BLIND-RECIPIENTS (GET-MAIL-RECIPIENTS PLIST T)))
    (WHEN BLIND-RECIPIENTS
      (AND (LOOP WITH BLIND-TEMPLATE = (APPEND TEMPLATE '(:BCC :BFCC))
                 FOR HOST IN HOSTS
                 ALWAYS (CHAOS-SEND-IT-1 HOST BLIND-RECIPIENTS PLIST INTERVAL BLIND-TEMPLATE))
           (ZMAIL-ERROR "Cannot connect to a MAIL server")))))

(DEFUN GET-MAIL-RECIPIENTS (PLIST &OPTIONAL BLIND)
  ;; BLIND = T means ONLY BCC; otherwise don't include them.
  (LOOP FOR FIELD IN *RECIPIENT-TYPE-HEADERS*
        WHEN (EQ (NOT (NULL BLIND))
                 (NOT (NULL (EQ FIELD :BCC))))
        NCONC (LOOP FOR RECIPIENT IN (GET PLIST FIELD)
                    COLLECT RECIPIENT)))

(defsignal mail-error (error) (message-part error-string)
  "Signalled when queuing a message fails")

(DEFUN CHAOS-SEND-IT-1 (HOST RECIPIENTS PLIST INTERVAL TEMPLATE)
  (WITH-OPEN-STREAM (STREAM (CHAOS:OPEN-STREAM HOST "MAIL" :ERROR NIL))
    (IF (ERRORP STREAM)
        (PROGN
          ;; When a host can't be reached, move it to end of list.
          (SETQ *MAIL-CHAOS-HOSTS*
                (NCONC (DELQ HOST *MAIL-CHAOS-HOSTS*)
                       (LIST HOST)))
          STREAM)
        ;; Output the recipients
        (DOLIST (RCPT RECIPIENTS)
          (SETQ RCPT (STRING-FROM-HEADER RCPT :HOST))
          (SEND STREAM :STRING-OUT RCPT)
          (SEND STREAM :TYO #/CR)
          (CHECK-CHAOS-MAIL-RESPONSE STREAM RCPT))
        (SEND STREAM :TYO #/CR)         ;Mark end of recipients
        (LET ((*QUOTE-HOSTS-FOR-XMAILR* (MEMQ (SEND HOST :SYSTEM-TYPE)
                                              '(:TOPS-20 :TENEX))))
          (OUTPUT-HEADER-AND-MSG STREAM PLIST INTERVAL TEMPLATE))
        (CHECK-CHAOS-MAIL-RESPONSE STREAM "the body of the message" T)
        (SEND STREAM :CLOSE :ABORT)     ;Non-abort would send another EOF and wait for it.
        NIL)))

(DEFUN CHECK-CHAOS-MAIL-RESPONSE (STREAM ERRMES &OPTIONAL EOF-P)
  (SEND STREAM :FORCE-OUTPUT)
  (AND EOF-P (SEND STREAM :EOF))
  (LET ((LINE (SEND STREAM :LINE-IN)))
    (case (char LINE 0)
      (#/+) ;AOK
      (#/-
       (FERROR 'MAIL-ERROR "Negative response from host for ~A: ~A"
               ERRMES (NSUBSTRING LINE 1)))
      (#/%
       (FERROR 'MAIL-ERROR "Temporary error from host for ~A: ~A" ERRMES (NSUBSTRING LINE 1)))
      (OTHERWISE
       (FERROR 'MAIL-ERROR "Unknown response from host for ~A: ~A" ERRMES LINE)))))

;;; This connects separately to each host that is getting mail
(DEFUN CHAOS-DIRECT-SEND-IT (PLIST INTERVAL TEMPLATE)
  (CANONICALIZE-HEADERS PLIST)
  (LET ((RECIPIENTS (GET-MAIL-RECIPIENTS PLIST)))
    (WHEN RECIPIENTS
      (CHAOS-DIRECT-SEND-IT-1 PLIST INTERVAL TEMPLATE RECIPIENTS)))
  ;; Now handle any BCC recipients.
  (LET ((BLIND-RECIPIENTS (GET-MAIL-RECIPIENTS PLIST T)))
    (WHEN BLIND-RECIPIENTS
      (CHAOS-DIRECT-SEND-IT-1 PLIST INTERVAL (APPEND TEMPLATE '(:BCC :BFCC))
                              BLIND-RECIPIENTS))))

(DEFUN CHAOS-DIRECT-SEND-IT-1 (PLIST INTERVAL TEMPLATE RECIPIENTS
                               &AUX RCPTS FAILED-HOST-RECIPIENTS)
  (LOOP WITH RPLIST = (LOCF RCPTS) AND GATEWAY = (CAR *MAIL-CHAOS-HOSTS*) AND HOST
        FOR RECIPIENT IN RECIPIENTS
        DO (AND (NOT (AND (SETQ HOST (CAR (LAST (GET (LOCF RECIPIENT) :HOST))))
                          (SETQ HOST (SI:PARSE-HOST HOST T))
                          (SEND HOST :NETWORK-TYPEP :CHAOS)))
                (SETQ HOST GATEWAY))
           (PUSH RECIPIENT (GET RPLIST HOST)))
  ;; First try sending each host its own recipients.
  (LOOP FOR (HOST THIS-HOST-RECIPIENTS) ON RCPTS BY 'CDDR
        WHEN (CHAOS-SEND-IT-1 HOST THIS-HOST-RECIPIENTS PLIST INTERVAL TEMPLATE)
        DO (SETQ FAILED-HOST-RECIPIENTS
                 (APPEND THIS-HOST-RECIPIENTS FAILED-HOST-RECIPIENTS)))
  ;; Now take care of all the recipients whose own hosts could not be reached.
  (WHEN FAILED-HOST-RECIPIENTS
    (LET ((HOSTS (COPYLIST *MAIL-CHAOS-HOSTS*)))
      ;; If the first on the list doesn't seem up in a short test,
      ;; ask all the hosts and find one that is up, as quickly as possible.
      (UNLESS (CHAOS:HOST-UP-P (CAR HOSTS) 120.)
        (LET ((UP (CHAOS:UP-HOSTS *MAIL-CHAOS-HOSTS* 1)))
          (WHEN UP
            (PUSH (CAR UP) HOSTS))))
      (AND (LOOP FOR HOST IN HOSTS
                 ALWAYS (CHAOS-SEND-IT-1 HOST FAILED-HOST-RECIPIENTS
                                         PLIST INTERVAL TEMPLATE))
           (ZMAIL-ERROR "Cannot connect to a MAIL server")))))


(DEFINE-SITE-HOST-LIST *MAIL-SMTP-HOSTS* :SMTP-MAIL-SERVER-HOSTS)

(DEFUN SMTP-SEND-IT (PLIST INTERVAL TEMPLATE)
  ;; The actual function is in the FTP package, due to similarities with the FTP protocol.
  ;; We could move the function here, maybe after some service opening utility such as
  ;; (OPEN-SERVER "HOST" "SMTP") is implemented, or perhaps some hairier generic
  ;; network scheme.
  (FUNCALL (INTERN "SMTP-SEND-IT" "FTP") PLIST INTERVAL TEMPLATE))


(DEFINE-SITE-VARIABLE *ALLOW-LM-IN-FROM-FIELD* :ALLOW-LM-IN-FROM-FIELD)

(DEFUN CANONICALIZE-HEADERS (PLIST &AUX HOST HOST1 (USER (OR *FROM-USER-ID* USER-ID "Not-logged-in")))
  (SETQ HOST (OR *FROM-HOST* FS:USER-LOGIN-MACHINE))
  (LET ((DATE-PROP :DATE)
        (FROM-PROP :FROM))
    (AND (GET PLIST 'REDISTRIBUTED-HEADERS)
         (SETQ DATE-PROP :REDISTRIBUTED-DATE
               FROM-PROP :REDISTRIBUTED-BY))
    (OR (GET PLIST DATE-PROP)
        (PUTPROP PLIST (TIME:GET-UNIVERSAL-TIME) DATE-PROP))
    (SETQ HOST1 (NCONS (STRING HOST)))
    (LET ((FROM (GET PLIST FROM-PROP)))
      (COND ((NULL FROM)
             (UNLESS *FROM-HOST*
               ;; Make sure we have a host that is not a Lisp machine, to be in the From.
               (DO ((TEM (FORMAT NIL "~A@~A" USER (CAR HOST1)))
                    (TEM1 '(FOO))
                    (FIRST-TIME T NIL))
                   (())
                 (COND ((EQUAL USER "")
                        (SETQ TEM
                              (TYPEIN-LINE-READLINE "You are not logged in.  Who are you?  (Type a valid mailing address for replies) ")))
                       ((AND (CONSP TEM1) (= (LENGTH TEM1) 1)
                             (OR *ALLOW-LM-IN-FROM-FIELD*
                                 (NEQ (SEND HOST :SYSTEM-TYPE) :LISPM)))
                        (UNLESS FIRST-TIME
                          (SETQ *FROM-HOST* HOST
                                *FROM-USER-ID* USER))
                        (RETURN))
                       (T
                        (SETQ TEM
                              (if (boundp 'zwei:*mini-buffer-window*)
                                  (TYPEIN-LINE-READLINE
                                    "~A, a Lisp machine, cannot be the From field.  Type User@host: "
                                    TEM)
                                (prompt-and-read :string-trim
                                                 "~&~A, a Lisp machine, cannot be the From field.  Type User@host: "
                                                 TEM)))
                        ;; Just Return means go ahead and put the Lispm in the From field.
                        (IF (EQUAL TEM "")
                            (RETURN (SETQ *FROM-HOST* HOST *FROM-USER-ID* USER)))))
                 ;; Otherwise try parsing the input and use that.
                 (SETQ TEM1 (PARSE-ADDRESSES TEM))
                 (AND (CONSP TEM1) (= (LENGTH TEM1) 1)
                      (SETQ USER (GET (LOCF (CAR TEM1)) :NAME)
                            HOST (SI:PARSE-HOST (CAR (GET (LOCF (CAR TEM1)) :HOST)))))))
             ;; Get a list of a string of its name.
             (SETQ HOST1 (NCONS (STRING HOST)))
             (PUTPROP PLIST `((:PERSONAL-NAME ,FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
                               :NAME ,USER :HOST ,HOST1))
                      FROM-PROP))
            ((NOT (AND (= (LENGTH FROM) 1)
                       (EQUALP USER-ID (GET (LOCF (CAR FROM)) :NAME))))
             (PUTPROP PLIST `((:NAME ,USER-ID :HOST ,HOST1)) :SENDER)))))
  ;; Make sure everyone has a host who should
  (LOOP FOR (IND PROPS) ON (CAR PLIST) BY 'CDDR
        WHEN (MEMQ IND *ADDRESS-TYPE-HEADERS*)
        DO (LOOP FOR PROP ON PROPS
                 AS PL = (LOCF (CAR PROP))
                 UNLESS (GET PL :HOST)
                 DO (PUTPROP PL HOST1 :HOST)
                    (REMPROP PL :INTERVAL))))   ;This can no longer be used

;;; Outputting of headers.  This is used for sending the draft,
;;; for formatting its text, for operating on text of messages, etc.

(DEFUN OUTPUT-HEADER-AND-MSG (STREAM PLIST INTERVAL TEMPLATE)
  (CANONICALIZE-HEADERS PLIST)
  (OUTPUT-HEADER STREAM PLIST TEMPLATE)
  (TERPRI STREAM)
  (COPY-INTERVAL-TO-STREAM STREAM INTERVAL))

(DEFUN COPY-INTERVAL-TO-STREAM (STREAM INTERVAL)
  (STREAM-COPY-UNTIL-EOF (INTERVAL-STREAM (INTERVAL-FIRST-BP INTERVAL)
                                          (INTERVAL-LAST-BP INTERVAL)
                                          T)
                         STREAM NIL))

(DEFUN OUTPUT-HEADER (STREAM PLIST TEMPLATE &OPTIONAL CR-AT-END)
  "Output to STREAM some header properties from PLIST according to TEMPLATE.
TEMPLATE is a list of property names; those properties' values are
output, in that order.  Each property has its own way of being output.
CR-AT-END requests a final blank line."
  (LOOP FOR IND IN TEMPLATE
        WITH HEADER
        DO (AND (SETQ HEADER (GET PLIST IND))
                (PRINT-HEADER STREAM HEADER IND)))
  ;;Finish up
  (AND CR-AT-END (SEND STREAM :TYO #/CR)))

;;; Same calling sequence as OUTPUT-HEADER, but outputs in different format.
(DEFUN OUTPUT-ITS-HEADER (STREAM PLIST IGNORE IGNORE &AUX TEM)
  (SEND STREAM :STRING-OUT (STRING-FROM-HEADER (CAR (GET PLIST :FROM)) :SHORT))
  (SEND STREAM :TYO #/SP)
  (COND ((SETQ TEM (GET PLIST :SENDER))
         (SEND STREAM :STRING-OUT "(Sent by ")
         (SEND STREAM :STRING-OUT (STRING-FROM-HEADER (CAR TEM) :SHORT))
         (SEND STREAM :STRING-OUT ") ")))
  (TIME:PRINT-UNIVERSAL-TIME (GET PLIST :DATE) STREAM)
  (COND ((SETQ TEM (GET PLIST :SUBJECT))
         (SEND STREAM :STRING-OUT " Re: ")
         (SEND STREAM :STRING-OUT TEM)))
  (SEND STREAM :TYO #/CR)
  (OR (AND (NULL (GET PLIST :CC))
           (NULL (CDR (GET PLIST :TO))))        ;One or fewer recipients
      (LOOP FOR (IND . NAME) IN '((:TO . "To") (:CC . "CC"))
            WITH RECIPIENTS
            DO (AND (SETQ RECIPIENTS (GET PLIST IND))
                    (PRINT-ADDRESS-HEADER STREAM RECIPIENTS NAME)))))

(DEFUN PRINT-HEADER (STREAM HEADER TYPE &AUX NAME)
  "Print a header item with field name TYPE and value HEADER, on STREAM.
TYPE controls the manner in which the printing is done,
and also appears in the output (usually)."
  (SETQ NAME (HEADER-TYPE-NAME TYPE))
  (COND ((EQ TYPE 'REDISTRIBUTED-HEADERS)
         (COPY-INTERVAL-TO-STREAM STREAM HEADER))
        ((MEMQ TYPE *ADDRESS-TYPE-HEADERS*)
         (PRINT-ADDRESS-HEADER STREAM HEADER NAME))
        ((MEMQ TYPE *DATE-TYPE-HEADERS*)
         (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS DAY MONTH YEAR
                               DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P)
             (TIME:DECODE-UNIVERSAL-TIME HEADER)
           (FORMAT STREAM "~A: ~A, ~D ~A ~D, ~2,'0D:~2,'0D-~A~%" NAME
                   (TIME:DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK) DAY (TIME:MONTH-STRING MONTH)
                   YEAR HOURS MINUTES
                   (TIME:TIMEZONE-STRING TIME:*TIMEZONE* DAYLIGHT-SAVINGS-P))))
        ((CONSP HEADER)
         (DO ((STRS HEADER (CDR STRS))
              (STR)
              (LEN (+ (STRING-LENGTH NAME) 2))
              (COMMA-P (MEMQ TYPE *SINGLE-LINE-TYPE-HEADERS*))
              (REFERENCE-P (MEMQ TYPE *REFERENCE-TYPE-HEADERS*))
              (FIRST-P T NIL))
             ((NULL STRS))
           (SETQ STR (CAR STRS))
           (COND (FIRST-P
                  (SEND STREAM :STRING-OUT NAME)
                  (SEND STREAM :STRING-OUT ": "))
                 (T
                  (DOTIMES (I LEN)
                    (SEND STREAM :TYO #/SP))))
           (IF REFERENCE-P
               (PRINT-REFERENCE STREAM STR)
               (SEND STREAM :STRING-OUT STR))
           (AND COMMA-P (CDR STRS)
                (SEND STREAM :TYO #/,))
           (SEND STREAM :TYO #/CR)))
        (T
         (FORMAT STREAM "~A: ~A~%" NAME HEADER))))

(DEFUN HEADER-TYPE-NAME (TYPE)
  (OR (CAR (RASSQ TYPE *HEADER-NAME-ALIST*))
      (STRING TYPE)))

(DEFUN PRINT-ADDRESS-HEADER (STREAM LIST NAME &OPTIONAL (MAX-COL 72.) &AUX PADLEN)
  (SETQ PADLEN (+ (STRING-LENGTH NAME) 2))      ;Account for ": "
  (SEND STREAM :STRING-OUT NAME)
  (SEND STREAM :STRING-OUT ": ")
  (cond ((listp list)
         (DO ((L LIST (CDR L))
              (X PADLEN (+ X LEN))
              (LEN) (STR)
              (EOL-P T NIL))
             ((NULL L))
           (SETQ STR (STRING-FROM-HEADER (CAR L) *SEND-HEADER-FORMAT*)
                 LEN (STRING-LENGTH STR))
           (COND ((NOT EOL-P)
                  (SEND STREAM :TYO #/,)
                  (COND ((OR *QUOTE-HOSTS-FOR-XMAILR* (> (+ X LEN 2) MAX-COL))
                         (SEND STREAM :TYO #/CR)
                         (DOTIMES (I PADLEN)
                           (SEND STREAM :TYO #/SP))
                         (SETQ X PADLEN
                               EOL-P T)))
                  (COND ((NOT EOL-P)
                         (SEND STREAM :TYO #/SP)
                         (INCF X 2)))))
           (SEND STREAM :STRING-OUT STR)))
        (t (format stream "Bad List: ~S" list)))        ;defensive, in case things screwwed.
  (SEND STREAM :TYO #/CR))

(DEFUN PRINT-REFERENCE (STREAM REF &OPTIONAL (VERBOSE-P T) &AUX (PLIST (LOCF REF)) TEM)
  (IF (SETQ TEM (GET PLIST :MESSAGE-ID))
      (SEND STREAM :STRING-OUT TEM)
      (LET* ((DATE (GET PLIST :DATE))
             (FROM (GET PLIST :FROM)))
        (COND (FROM
               (SETQ TEM (OR (STRING-FROM-HEADER FROM
                                                 (IF VERBOSE-P :INCLUDE-PERSONAL :SHORT))
                             (GET (LOCF FROM) :PERSONAL-NAME)))
               (SETQ FROM TEM)))
        (AND VERBOSE-P (SEND STREAM :STRING-OUT (IF FROM "The " "Your ")))
        (SEND STREAM :STRING-OUT "message")
        (AND DATE
             (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS DAY MONTH YEAR NIL DAYLIGHT-SAVINGS-P)
                 (TIME:DECODE-UNIVERSAL-TIME DATE)
               (FORMAT STREAM " of ~D ~A ~D ~2,'0D:~2,'0D-~A"
                       DAY (TIME:MONTH-STRING MONTH :SHORT) YEAR HOURS MINUTES
                       (TIME:TIMEZONE-STRING TIME:*TIMEZONE* DAYLIGHT-SAVINGS-P))))
        (COND (FROM
               (SEND STREAM :STRING-OUT " from ")
               (SEND STREAM :STRING-OUT FROM))))))

(DEFUN COM-ZMAIL-LOCAL-MAIL-INTERNAL (&AUX ZMAIL-BUFFER END-BP MSG)
  (SETQ ZMAIL-BUFFER (COND (*MSG* (MSG-MAIL-FILE-BUFFER *MSG*))
                           ((LOOP FOR MF IN *ZMAIL-BUFFER-LIST*
                                  WHEN (ZMAIL-BUFFER-DISK-P MF)
                                  RETURN MF))
                           (T (BARF "There is no file buffer to put a local message in."))))
  (SETQ MSG (MAKE-EMPTY-MSG)
        END-BP (MSG-END-BP MSG))
  (LET* ((HEADERS `(:TO ((:NAME ,USER-ID))))
         (PLIST (LOCF HEADERS)))
    (AND *LOCAL-MAIL-INCLUDE-SUBJECT*
         (PUTPROP PLIST "" :SUBJECT))
    (CANONICALIZE-HEADERS PLIST)
    (FUNCALL (IF (EQ *LOCAL-MAIL-HEADER-FORCE* :ITS) 'OUTPUT-ITS-HEADER 'OUTPUT-HEADER)
             (INTERVAL-STREAM-INTO-BP END-BP) PLIST *DEFAULT-SEND-TEMPLATE* T))
  (SEND ZMAIL-BUFFER :ADD-MSG MSG)
  (OR (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
      (SEND *ZMAIL-BUFFER* :ADD-MSG MSG))
  (ZMAIL-SELECT-MSG MSG)
  (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
  (LET* ((START-LINE (BP-LINE (MSG-START-BP MSG)))
         (SUBJ-LINE (AND *LOCAL-MAIL-INCLUDE-SUBJECT*
                         (IF (EQ *LOCAL-MAIL-HEADER-FORCE* :ITS)
                             START-LINE
                             (DO ((LINE (BP-LINE END-BP) (LINE-PREVIOUS LINE)))
                                 ((EQ LINE START-LINE) NIL)
                               (AND (STRING-EQUAL-START LINE "Subject:")
                                    (RETURN LINE)))))))
    (MOVE-BP (POINT) (IF SUBJ-LINE (END-OF-LINE SUBJ-LINE) END-BP)))
  (MUST-REDISPLAY *WINDOW* DIS-ALL)
  (COM-EDIT-CURRENT-MSG))

(DEFUN GET-FCC-PATHNAMES (FCC)
  (IF (CONSP FCC)
      (LOOP FOR STR IN FCC
            NCONC (GET-FCC-PATHNAMES FCC))
    (AND FCC
         (TV:PARSE-PATHNAME-LIST FCC *ZMAIL-PATHNAME-DEFAULTS*))))

(DEFUN CONSTRUCT-FCC-MSG (PLIST INTERVAL &OPTIONAL BLIND-P &AUX MSG)
  (SETQ MSG (MAKE-EMPTY-MSG))
  (LET ((STREAM (INTERVAL-STREAM-INTO-BP (MSG-END-BP MSG))))
    (OUTPUT-HEADER-AND-MSG STREAM PLIST (COPY-INTERVAL INTERVAL)
                           (IF BLIND-P
                               (APPEND *DEFAULT-SEND-TEMPLATE*
                                       '(:BCC :BFCC))
                             *DEFAULT-SEND-TEMPLATE*))
    (SEND STREAM :FRESH-LINE))
  MSG)

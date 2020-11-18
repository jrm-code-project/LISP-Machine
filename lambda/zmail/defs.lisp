;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable: ZL -*-
;;; This is SYS: ZMAIL; DEFS
;;; Definitions for ZMail
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFMACRO DEFINE-ZMAIL-TOP-LEVEL-COMMAND (FN DOC OPTIONS &BODY DEF)
  `(PROGN
     (ZMAIL-TOP-LEVEL-COMMAND-DEFINE ',FN ',DOC ',OPTIONS)
     (DEFUN ,FN ()
       ,@(PROCESS-ZMAIL-TOP-LEVEL-COMMAND-OPTIONS OPTIONS)
       . ,DEF)))

(DEFVAR *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST* NIL)

(DEFUN ZMAIL-TOP-LEVEL-COMMAND-DEFINE (COMMAND DOC IGNORE)
  (COND ((STRINGP DOC)
         (PUTPROP COMMAND DOC 'DOCUMENTATION))
        ((OR (SYMBOLP DOC)
             (AND (NOT (ATOM DOC))
                  (EQ (CAR DOC) 'LAMBDA)))
         (PUTPROP COMMAND DOC 'DOCUMENTATION-FUNCTION))
        (T
         (ZMAIL-ERROR "The command ~S has invalid self-documentation ~S" COMMAND DOC)))
  (LET ((NAME (MAKE-ZMAIL-TOP-LEVEL-COMMAND-NAME COMMAND)))
    (PUTPROP COMMAND NAME 'COMMAND-NAME)
    (OR (ASSOC NAME *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)
        (PUSH (CONS NAME COMMAND) *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*))))

(DEFUN PROCESS-ZMAIL-TOP-LEVEL-COMMAND-OPTIONS (OPTIONS
                                                &AUX (CONTEXT-CONDITION 'MUST-HAVE-MSG)
                                                     (ARGUMENT-CONDITION 'NO-ARG))
  (DOLIST (OP OPTIONS)
    (CASE OP
      ((NO-ZMAIL-BUFFER-OK NO-MSG-OK MUST-HAVE-MSG)
       (SETQ CONTEXT-CONDITION OP))
      ((NUMERIC-ARG-OK NO-ARG)
       (SETQ ARGUMENT-CONDITION OP))
      (OTHERWISE
       (ZMAIL-ERROR "~S is not a recognized option" OP))))
  (LIST (CADR (ASSQ CONTEXT-CONDITION '((MUST-HAVE-MSG
                                         (OR *MSG*
                                             (BARF "There is no current message")))
                                        (NO-MSG-OK
                                         (OR *ZMAIL-BUFFER*
                                             (BARF "There is no current zmail buffer")))
                                        (NO-ZMAIL-BUFFER-OK))))
        (CADR (ASSQ ARGUMENT-CONDITION '((NO-ARG
                                          (AND *NUMERIC-ARG-P*
                                               (BARF "This command does not take an argument")
                                               ))
                                         (NUMERIC-ARG-OK))))
        ))

;;; Convert a string into human-readable form.  Remove leading COM-, or leading
;;; and trailing *'s.  Conver hyphens into spaces, and capitalize each word.
;;; This is used both for command names and variable names.
(DEFUN MAKE-ZMAIL-TOP-LEVEL-COMMAND-NAME (COMMAND)
  (SETQ COMMAND (STRING COMMAND))
  (LET ((CLEN (STRING-LENGTH COMMAND)))
    (STRING-SUBST-CHAR
      #\Space #\-
      (STRING-CAPITALIZE (SUBSTRING COMMAND
                                    (COND ((STRING= "COM-ZMAIL-" COMMAND :END1 12 :END2 12)
                                           12)
                                          ((STRING= "COM-MOUSE-" COMMAND :END1 12 :END2 12)
                                           12)
                                          ((STRING= "COM-" COMMAND :END1 4 :END2 4)
                                           4)
                                          ((STRING= "*" COMMAND :END1 1 :END2 1)
                                           1)
                                          (T 0))
                                    (COND ((CHAR= #\* (CHAR COMMAND (1- CLEN))) (1- CLEN))
                                          (T CLEN)))))))

;;; Top-level-commands
(DEFVAR *ZMAIL-COMMAND-ALIST* '(("Profile" . COM-ZMAIL-PROFILE)
                                ("Quit" . COM-ZMAIL-QUIT)
                                ("Delete" . COM-ZMAIL-DELETE)
                                ("Undelete" . COM-ZMAIL-UNDELETE)
                                ("Reply" . COM-ZMAIL-REPLY)
                                ("Configure" . COM-ZMAIL-CONFIGURE)
                                ("Save Files" . COM-ZMAIL-SAVE)
                                ("Next" . COM-ZMAIL-NEXT)
                                ("Previous" . COM-ZMAIL-PREVIOUS)
                                ("Continue" . COM-ZMAIL-CONTINUE)
                                ("Survey" . COM-ZMAIL-SURVEY)
                                ("Get New Mail" . COM-GET-NEW-MAIL)
                                ("Jump" . COM-ZMAIL-GOTO)
                                ("Keywords" . COM-ZMAIL-KEYWORDS)
                                ("Mail" . COM-ZMAIL-MAIL)
                                ("Sort" . COM-ZMAIL-SORT)
                                ("Map Over" . COM-ZMAIL-MAP)
                                ("Move" . COM-ZMAIL-MOVE)
                                ("Select" . COM-ZMAIL-SELECT)
                                ("Other" . COM-ZMAIL-OTHER-COMMANDS)))

;;; Commands without message arguments
(DEFVAR *ZMAIL-NO-FILTER-COMMAND-ALIST* '(("Configure" . COM-ZMAIL-CONFIGURE)
                                          ("Get New Mail" . COM-GET-NEW-MAIL)
                                          ("Save Files" . COM-ZMAIL-SAVE)
                                          ("Sort" . COM-ZMAIL-SORT)
                                          ("Profile" . COM-ZMAIL-PROFILE)
                                          ("Quit" . COM-ZMAIL-QUIT)))

(DEFVAR *ZMAIL-FILTER-COMMAND-ALIST* '(("Concatenate" . COM-ZMAIL-CONCATENATE)
                                       ("Survey" . COM-ZMAIL-SURVEY)
                                       ("Delete" . COM-ZMAIL-DELETE)
                                       ("Undelete" . COM-ZMAIL-UNDELETE)
                                       ("Reply" . COM-ZMAIL-REPLY)
                                       ("Other" . COM-ZMAIL-OTHER-COMMANDS)
                                       ("Type" . COM-ZMAIL-TYPE)
                                       ("Next" . COM-ZMAIL-NEXT)
                                       ("Previous" . COM-ZMAIL-PREVIOUS)
                                       ("Continue" . COM-ZMAIL-CONTINUE)
                                       ("" :NO-SELECT T)
                                       ("Select" . COM-ZMAIL-SELECT)
                                       ("Keywords" . COM-ZMAIL-KEYWORDS)
                                       ("Move" . COM-ZMAIL-MOVE)
                                       ("Mail" . COM-ZMAIL-MAIL)))

(DEFVAR *OTHER-COMMAND-ALIST* NIL "List of commands for the OTHER menu.")

;;; Send definite ZMail bugs to BUG-ZMAIL.
(DEFFLAVOR ZMAIL-ERROR () (ERROR))
(DEFMETHOD (ZMAIL-ERROR :BUG-REPORT-RECIPIENT-SYSTEM) () "ZMail")
(DEFUN ZMAIL-ERROR (FORMAT-STRING &REST ARGS)
  (DECLARE (EH:ERROR-REPORTER))
  (SIGNAL-CONDITION (MAKE-INSTANCE 'ZMAIL-ERROR :FORMAT-STRING FORMAT-STRING
                                                :FORMAT-ARGS ARGS)
                    () T))

(DEFVAR *ZMAIL-WINDOW*)

(DEFVAR *ZMAIL-TYPEOUT-ITEM-ALIST* NIL)

;;; Most messages tend to stick around for more than the average level 3 flip interval...
(DEFVAR *ZMAIL-MSG-AREA* (MAKE-AREA :NAME 'ZMAIL-MSG-AREA :VOLATILITY 2)
  "The zmail message area.")

;;; Used as the :INSTANCE-AREA-FUNCTION for several kinds of ZMail data structures
(defun zmail-buffer-cons-area (init-plist)
  (declare (ignore init-plist))
  *zmail-msg-area*)

;;; This used to be a separate area, but now there's *ZWEI-AREA* which is the right thing.
(DEFVAR *ZMAIL-MSG-LINE-AREA* *ZWEI-AREA* "The ZMail message line area.")

(DEFVAR *HEADER-NAME-ALIST*
  '(
    ("Backward-references" . :backward-references)
    ("BCC" . :BCC)
    ("BFCC" . :BFCC)
    ("CC" . :CC)
    ("Date" . :DATE)
    ("Draft-Composition-Date" . :DRAFT-COMPOSITION-DATE)
    ("Encrypted" . :encrypted) ; String contains description of encryption algorithm
    ("Expiration-date" . :EXPIRATION-DATE)
    ("Expires" . :EXPIRATION-DATE)
    ("FCC" . :FCC)
    ("Fonts" . :FONTS)
    ("Forward-references" . :forward-references)
    ("From" . :FROM)
    ("FTo" . :FTO)
    ("In-reply-to" . :IN-REPLY-TO)
    ("Included-messages" . :included-messages)
    ("Included-references" . :included-references)
    ("Mail-from" . :MAIL-FROM)
    ("Message-ID" . :MESSAGE-ID)
    ;; Re goes at the end so that RASSQ sees "Subject" first as the :SUBJECT header
    ("Redistributed-by" . :REDISTRIBUTED-BY)
    ("Redistributed-date" . :REDISTRIBUTED-DATE)
    ("Redistributed-to" . :REDISTRIBUTED-TO)
    ("References" . :REFERENCES)
    ("Remailed-by" . :REMAILED-BY)
    ("Remailed-date" . :REMAILED-DATE)
    ("Remailed-to" . :REMAILED-TO)
    ("Reply-to" . :REPLY-TO)
    ("Resent-cc" . :RESENT-CC)
    ("Resent-date" . :RESENT-DATE)
    ("Resent-from" . :RESENT-FROM)
    ("Resent-reply-to" . :RESENT-REPLY-TO)
    ("Resent-sender" . :RESENT-SENDER)
    ("Resent-to" . :RESENT-TO)
    ("Return-path" . :RETURN-PATH)
    ("Return-Path" . :RETURN-PATH)  ;;I don't think this is official, but its legal
    ("Sender" . :SENDER)
    ("Subject" . :SUBJECT)
    ("Supersedes" . :SUPERSEDES)
    ("To" . :TO)
    ("Re" . :SUBJECT) ;;This is also valid
    )
  "The list of all the valid fields in a header.")

(DEFVAR *RECIPIENT-TYPE-HEADERS* '(:TO :CC :BCC :REDISTRIBUTED-TO :FORWARDED-TO-TO
                                       :FORWARDED-TO-CC :REDISTRIBUTED-TO-CC
                                       :REMAILED-TO
                                       :RESENT-TO :RESENT-CC)
  "The list of all the fields valid as recipient headers.")
(DEFVAR *SENDER-TYPE-HEADERS* '(:FROM :SENDER :REPLY-TO
                                      :REDISTRIBUTED-BY :FORWARDED-TO-BY
                                      :REMAILED-BY :RESENT-FROM :RESENT-SENDER
                                      :RESENT-REPLY-TO)
  "The list of all the fields valid as sender headers.")
(DEFVAR *SENDER-OR-RECIPIENT-TYPE-HEADERS* `(,@*SENDER-TYPE-HEADERS*
                                             . ,*RECIPIENT-TYPE-HEADERS*)
  "The list of all the fields valid as sender or recipient headers.")
;;; Text of these headers is parsed as RFC733 headers
(DEFVAR *ADDRESS-TYPE-HEADERS* (LIST* :RETURN-PATH
                                      *SENDER-OR-RECIPIENT-TYPE-HEADERS*))
;;; Text of these headers is parsed as a date/time specification
(DEFVAR *DATE-TYPE-HEADERS* '(:DATE :DRAFT-COMPOSITION-DATE :EXPIRATION-DATE
                              :REDISTRIBUTED-DATE :FORWARDED-TO-DATE
                              :RESENT-DATE))
;;; These reference other messages in some way
(DEFVAR *REFERENCE-TYPE-HEADERS* '(:backward-references :forward-references
                                   :IN-REPLY-TO :included-messages :included-references :REFERENCES :SUPERSEDES))
;;; Several instances within these properties are separated by ",<CR>".
(DEFVAR *SINGLE-LINE-TYPE-HEADERS* *REFERENCE-TYPE-HEADERS*)

;;; These are properties which are not dependent on the text of the message
;;; Other properties which gotten from the text indirectly and hence not in the
;;; keyword package are: LOSING-HEADERS, REFERENCES, ITS-HEADER-P, HEADERS-END-BP, LENGTH,
;;; HASH-ID
(DEFVAR *INTERNAL-TYPE-PROPERTIES*
        '(DELETED UNSEEN REFORMATTED ANSWERED FILED PROCESSED MARKED FORWARDED
          RECENT LENGTH KEYWORDS KEYWORDS-STRING DRAFT-MSG)
  "Properties which are not dependent on the text of the message.")

(DEFVAR *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED* T)

(DEFVAR *SAVED-INTERNAL-PROPERTIES-ALIST*
        '(("last" . LAST)                       ;This is really treated differently
          ("unseen" . UNSEEN)
          ("deleted" . DELETED)
          ("bad-header" . LOSING-HEADERS)
          ("answered" . ANSWERED)
          ("forwarded" . FORWARDED)
          ("redistributed" . REDISTRIBUTED)
          ("filed" . FILED)
          ("recent" . RECENT))
  "The properties a message could have.  They are one or more of the following:
last, unseen, deleted, bad-header, answered, forwarded, redistributed, filed, recent.")

(DEFVAR *SORT-KEY-ALIST-1*
        '(("Date" :VALUE MSG-DATE-SORT-LESSP :DOCUMENTATION "Chronologically by date.")
          ("To" :VALUE MSG-TO-STRING-LESSP :DOCUMENTATION "Alphabetically by To: field.")
          ("From" :VALUE MSG-FROM-STRING-LESSP :DOCUMENTATION
           "Alphabetically by From: field.")
          ("Subject" :VALUE MSG-SUBJECT-STRING-LESSP :DOCUMENTATION
           "Alphabetically by Subject: field.")
          ("Keywords" :VALUE MSG-KEYWORD-LESSP :DOCUMENTATION
           "Alphabetically by keywords present.")
          ("Text" :VALUE MSG-TEXT-STRING-LESSP :DOCUMENTATION
           "Alphabetically by actual message text.")
          ("Length" :VALUE MSG-LENGTH-LESSP
                    :DOCUMENTATION "Numerically by length of message in characters."))
  "The items in the SORT menu.")

(DEFINE-USER-OPTION-ALIST *ZMAIL-HARDCOPY-OPTION-ALIST* DEFINE-ZMAIL-HARDCOPY-OPTION
  "The items for the HARDCOPY menu.")

;;; Mail file/buffer, actual array contains messages themselves
;;; The inherited instance variables are not actually used.
;;; They are here because ZMAIL-DISK-BUFFER wants them
;;; and therefore they must be here so they can be :ORDERED right.
(DEFFLAVOR ZMAIL-BUFFER
        (NAME                                   ;Name of the mail file
         (UNDO-STATUS :DONT)                    ;Normally don't record changes.
         ARRAY                                  ;Where actual messages live
         (OPTIONS NIL)                          ;Property list
         (SAVED-CURRENT-MSG NIL))               ;When switching back
        (FILE-BUFFER)
  (:ORDERED-INSTANCE-VARIABLES
    ;; These are the ordered ivars of FILE-BUFFER.
    FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
    UNDO-STATUS READ-ONLY-P SI:PROPERTY-LIST
    ;; This flavor's
    NAME ARRAY OPTIONS SAVED-CURRENT-MSG)
  :SETTABLE-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:INIT-KEYWORDS :APPEND-P)
  (:instance-area-function zmail-buffer-cons-area))

;; This is used for actual files, both mail files and inboxes.
(DEFFLAVOR ZMAIL-DISK-BUFFER
        (;; Those really inherited
         NAME
         ARRAY
         OPTIONS
         SAVED-CURRENT-MSG
         INFERIORS                              ;List of the real-intervals of the messages.
         ;; Our own
         (LOCK NIL)
         MSG-UPDATE-TICK
         (STATUS NIL)                           ;Special state or NIL
                                                ;States for old mail are :LOADING and :SAVING
                                                ;and :SAVE-REQUIRED and :AWAIT-NEW-MAIL.
                                                ;for new mail :NEW-MAIL, :LOADING-NEW-MAIL,
                                                ;and :AWAITING-SAVE.
         (STREAM NIL))
        (ZMAIL-BUFFER)
  (:ORDERED-INSTANCE-VARIABLES
    FIRST-BP LAST-BP TICK NEXT PREVIOUS SUPERIOR INFERIORS
    UNDO-STATUS READ-ONLY-P SI:PROPERTY-LIST
    ;; ZMAIL-BUFFER's
    NAME ARRAY OPTIONS SAVED-CURRENT-MSG
    ;; This flavor's
    LOCK MSG-UPDATE-TICK STATUS STREAM)
  :SETTABLE-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)


(DEFSUBST ZMAIL-DISK-BUFFER-INTERVAL (BUF) BUF)

;;; Temporary buffers
(DEFFLAVOR TEMP-ZMAIL-BUFFER (FULL-NAME) (ZMAIL-BUFFER)
  (:SETTABLE-INSTANCE-VARIABLES FULL-NAME))

(DEFRESOURCE TEMP-ZMAIL-BUFFER ()
  :INITIAL-COPIES 0
  :CONSTRUCTOR MAKE-TEMP-ZMAIL-BUFFER
  :CHECKER TEMP-ZMAIL-BUFFER-AVAILABLE-P)

(DEFFLAVOR MAIL-FILE-BUFFER
        ((ASSOCIATED-INBOX-BUFFER NIL)
         (REFERENCE-HASH-TABLE NIL))
        (ZMAIL-DISK-BUFFER)
  :SETTABLE-INSTANCE-VARIABLES
  (:INIT-KEYWORDS :NEW-PRIMARY-P))

(DEFFLAVOR INBOX-BUFFER
        (FILE-LIST
         PENDING-FILE-LIST
         FILE-LIST-MAIL-CHECK-INFO
         (NEXT-PENDING-FILE-LIST NIL)
         (PENDING-DELETION-LIST NIL)
         (ASSOCIATED-MAIL-FILE-BUFFER NIL))
        (ZMAIL-DISK-BUFFER)
  :SETTABLE-INSTANCE-VARIABLES)

(DEFMACRO ZMAIL-BUFFER-NMSGS (ZMAIL-BUFFER)
  `(ARRAY-LEADER (ZMAIL-BUFFER-ARRAY ,ZMAIL-BUFFER) 0))

(DEFMACRO ZMAIL-BUFFER-START-BP (ZMAIL-BUFFER)
  `(INTERVAL-FIRST-BP ,ZMAIL-BUFFER))

(DEFMACRO ZMAIL-BUFFER-END-BP (ZMAIL-BUFFER)
  `(INTERVAL-LAST-BP ,ZMAIL-BUFFER))


(DEFMACRO ZMAIL-BUFFER-COERCE (VARIABLE)
  "Cause the value of VARIABLE to become a ZMAIL-BUFFER object if it is a string or pathname."
  `(AND (OR (STRINGP ,VARIABLE) (TYPEP ,VARIABLE 'FS:PATHNAME))
        (SETQ ,VARIABLE (ZMAIL-FIND-FILE-NOSELECT ,VARIABLE))))

;;; Lock a buffer around BODY
(DEFMACRO LOCK-ZMAIL-BUFFER ((ZMAIL-BUFFER) &BODY BODY)
  (LET ((LOCK (GENSYM)) (LOCKED-P (GENSYM)))
    `(LET ((,LOCK (LOCF (ZMAIL-DISK-BUFFER-LOCK ,ZMAIL-BUFFER)))
           (,LOCKED-P NIL))
       (UNWIND-PROTECT
         (PROGN
           (COND ((NEQ (CAR ,LOCK) CURRENT-PROCESS)
                  (PROCESS-LOCK ,LOCK)
                  (SETQ ,LOCKED-P T)))
           . ,BODY)
         (AND ,LOCKED-P (PROCESS-UNLOCK ,LOCK))))))

(DEFMACRO ZMAIL-BUFFER-DISK-P (ZMAIL-BUFFER)
  `(TYPEP ,ZMAIL-BUFFER 'ZMAIL-DISK-BUFFER))

(DEFMACRO ZMAIL-BUFFER-APPEND-P (ZMAIL-BUFFER)
  `(GET (LOCF (ZMAIL-BUFFER-OPTIONS ,ZMAIL-BUFFER)) :APPEND))

(DEFMACRO DOMSGS ((MSG ZMAIL-BUFFER) &BODY BODY)
  `(LET ((.ARRAY. (ZMAIL-BUFFER-ARRAY ,ZMAIL-BUFFER)))
     (DO ((.I. 0 (1+ .I.))
          (.NMSGS. (ARRAY-ACTIVE-LENGTH .ARRAY.))
          (,MSG))
         (( .I. .NMSGS.))
       (SETQ ,MSG (AREF .ARRAY. .I.))
       . ,BODY)))

(DEFINE-LOOP-PATH MSGS MSG-PATH (IN))

(DEFUN MSG-PATH (PATH-NAME VARIABLE IGNORE PREP-PHRASES INCLUSIVE-P IGNORE IGNORE)
  (OR PREP-PHRASES
      (ZMAIL-ERROR "Missing IN between ~S and ~S" PATH-NAME VARIABLE))
  (AND INCLUSIVE-P
       (ZMAIL-ERROR "Inclusive not supported"))
  (LET ((ARRAY (SI:LOOP-NAMED-VARIABLE 'ARRAY))
        (SIZE (SI:LOOP-NAMED-VARIABLE 'SIZE))
        (INDEX (SI:LOOP-NAMED-VARIABLE 'INDEX)))
    (LIST `((,VARIABLE NIL)
            (,ARRAY (ZMAIL-BUFFER-ARRAY ,(CADAR PREP-PHRASES)))
            (,SIZE NIL)
            (,INDEX 0))
          `((SETQ ,SIZE (ARRAY-ACTIVE-LENGTH ,ARRAY)))
          `( ,INDEX ,SIZE)
          NIL
          NIL
          `(,VARIABLE (AREF ,ARRAY ,INDEX)
            ,INDEX (1+ ,INDEX)))))

;;; Messages
(DEFSTRUCT (MSG :ARRAY :NAMED :CONC-NAME (:ALTERANT NIL)
                (:MAKE-ARRAY (:AREA *ZMAIL-MSG-AREA*)))
  (REAL-INTERVAL NIL :DOCUMENTATION "Where the message starts in the file itself")
  (INTERVAL NIL :DOCUMENTATION "Displayed portion of message")
  (TICK NIL :DOCUMENTATION "Last time something munged in some way")
  (MAIL-FILE-BUFFER NIL :DOCUMENTATION "The file this lives in")
  (SUMMARY-LINE NIL :DOCUMENTATION "String displayed in summary window")
  (DISPLAYED-INDEX 0 :DOCUMENTATION "Number for when displayed in summary window")
  (STATUS NIL :DOCUMENTATION "Alist of keywords for message")
  (PARSED-P NIL :DOCUMENTATION "NIL, T, or :IN-PROGRESS"))

(DEFMACRO MSG-REAL-START-BP (MSG)
  `(INTERVAL-FIRST-BP (MSG-REAL-INTERVAL ,MSG)))

(DEFMACRO MSG-REAL-END-BP (MSG)
  `(INTERVAL-LAST-BP (MSG-REAL-INTERVAL ,MSG)))

(DEFMACRO MSG-START-BP (MSG)
  `(INTERVAL-FIRST-BP (MSG-INTERVAL ,MSG)))

(DEFMACRO MSG-END-BP (MSG)
  `(INTERVAL-LAST-BP (MSG-INTERVAL ,MSG)))

;;; Get something off a message's "property list"
(DEFMACRO MSG-GET (MSG PROPNAME)
  `(GET (ASSURE-MSG-PARSED ,MSG) ,PROPNAME))

(DEFMACRO MSG-DRAFT-MSG-P (MSG)
  `(NOT (NULL (MSG-GET ,MSG :DRAFT-COMPOSITION-DATE))))

;; Represents a message being sent.
(DEFFLAVOR DRAFT-MSG
           (HEADER-INTERVAL                     ;Headers of message
            REPLY-INTERVAL                      ;Body of text
            SUMMARY-STRING                      ;For continue command
            SUMMARY-STRING-TICK                 ;When last valid
            (MSGS-BEING-REPLIED-TO NIL)         ;If from reply command
            (MSGS-BEING-FORWARDED NIL)          ;If from forward command
            (SENT-P NIL)                        ;Sent successfully
            (LAST-WINDOW-CONFIGURATION NIL)     ;Value of *WINDOW-CONFIGURATION* when done
            (PATHNAME NIL)                      ;When saved out in a file
            (MSG NIL)                           ;In which it is saved
            (WINDOW-POINTS NIL)                 ;Saved positions in the various windows
            STARTING-TICK                       ;Tick at which draft was created or sent.
            )
           (NODE)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:instance-area-function zmail-buffer-cons-area))

(DEFMETHOD (DRAFT-MSG :AFTER :INIT) (IGNORE)
  (SETQ SUMMARY-STRING "Empty"
        SUMMARY-STRING-TICK *TICK*
        STARTING-TICK *TICK*
        MSGS-BEING-REPLIED-TO (COPYLIST MSGS-BEING-REPLIED-TO)
        LAST-WINDOW-CONFIGURATION :MAIL)
  (INSERT LAST-BP #\CR)
  (INSERT LAST-BP *MAIL-HEADER-DELIMITER*)
  (INSERT LAST-BP #\CR)
  (SETQ HEADER-INTERVAL (MAKE-INSTANCE 'ZMAIL-SENDING-INTERVAL
                                       :SUPERIOR SELF
                                       :FIRST-BP (COPY-BP FIRST-BP :NORMAL)
                                       :LAST-BP (COPY-BP FIRST-BP :MOVES)
                                       :NAME "Headers")
        REPLY-INTERVAL (MAKE-INSTANCE 'ZMAIL-SENDING-INTERVAL
                                      :SUPERIOR SELF
                                      :FIRST-BP (COPY-BP LAST-BP :NORMAL)
                                      :LAST-BP (COPY-BP LAST-BP :MOVES)
                                      :NAME "Mail"))
  (SETF (LINE-NODE (BP-LINE FIRST-BP)) HEADER-INTERVAL)
  (SETF (LINE-NODE (BP-LINE LAST-BP)) REPLY-INTERVAL)
  (SETF INFERIORS (LIST HEADER-INTERVAL REPLY-INTERVAL)))

(DEFMETHOD (DRAFT-MSG :MODIFIED-P) ()
  (> TICK STARTING-TICK))

(DEFMETHOD (DRAFT-MSG :NOT-MODIFIED) () NIL)

;; The HEADER-INTERVAL and REPLY-INTERVAL of a DRAFT-MSG
;; are of this type.
(DEFFLAVOR ZMAIL-SENDING-INTERVAL () (NAMED-BUFFER))

(DEFMETHOD (ZMAIL-SENDING-INTERVAL :MODIFIED-P) ()
  (SEND SUPERIOR :MODIFIED-P))

(DEFMETHOD (ZMAIL-SENDING-INTERVAL :NOT-MODIFIED) () NIL)

(DEFSTRUCT (SUMMARY-LINE :ARRAY-LEADER :CONC-NAME
                         (:MAKE-ARRAY (:LENGTH 140 :TYPE 'ART-STRING :area *zmail-msg-area*)))
  (LENGTH 0 :DOCUMENTATION "The length of the summary line.")
  TEMPLATE)

(DEFMACRO ZMAIL-BACKGROUND-REQUEST-PUSH (THING)
  "Transmit THING from main process to background process."
  `(WITHOUT-INTERRUPTS
     (PUSH ,THING (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*))))

(DEFMACRO ZMAIL-BACKGROUND-RESPONSE-PUSH (THING)
  "Transmit THING from background process to main process.
It goes at the end of *BACKGROUND-RESPONSE-QUEUE*."
  `(LOCAL-DECLARE ((SPECIAL TV:IO-BUFFER))
;    (COMMAND-BUFFER-PUSH (CONS 'BACKGROUND ,THING))
     (SETF (CONTENTS *BACKGROUND-RESPONSE-QUEUE*)
           (NCONC (CONTENTS *BACKGROUND-RESPONSE-QUEUE*) (NCONS (CONS 'BACKGROUND ,THING))))
     (COMMAND-BUFFER-PUSH '(READ-BACKGROUND-RESPONSE-QUEUE))))

(DEFMACRO USING-OVERLYING-WINDOW (&BODY BODY)
  `(TV:WITH-SELECTION-SUBSTITUTE (*OVERLYING-WINDOW* *ZMAIL-WINDOW*)
     (SEND *OVERLYING-WINDOW* :DELETE-TEXT)
     (LET ((TERMINAL-IO *OVERLYING-WINDOW*)
           (STANDARD-INPUT SI:SYN-TERMINAL-IO)
           (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
           (QUERY-IO SI:SYN-TERMINAL-IO))
       . ,BODY)))

;     (SEND *ZMAIL-WINDOW* :DEEXPOSE NIL :NOOP)
;       (SEND *OVERLYING-WINDOW* :DEACTIVATE)
;       (SEND *ZMAIL-WINDOW* :SELECT NIL))

(DEFMACRO WITH-WINDOW-CONFIGURATION ((CONFIGURATION) &BODY BODY)
  "Execute BODY with window configuration set to CONFIGURATION."
  `(LET ((.CONFIG. *WINDOW-CONFIGURATION*)
         (.NEWCONFIG. ,CONFIGURATION))
     (UNWIND-PROTECT
       (PROGN
         (UNLESS (EQ *WINDOW-CONFIGURATION* .NEWCONFIG.)
           (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION .NEWCONFIG.))
         . ,BODY)
       (UNLESS (EQ .CONFIG. .NEWCONFIG.)
         (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION .CONFIG.)))))

(DEFMACRO WITH-BACKGROUND-PROCESS-LOCKED (&BODY BODY)
  "Execute BODY with background process locked."
  `(LET ((.LOCKED-P. NIL))
     (UNWIND-PROTECT
       (PROGN
         (SETQ .LOCKED-P. (LOCK-BACKGROUND-PROCESS))
         . ,BODY)
       (AND .LOCKED-P. (PROCESS-UNLOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*)))))

(DEFMACRO MAKE-EMPTY-STRING (LENGTH)
  `(MAKE-STRING ,LENGTH :FILL-POINTER 0))


(DEFVAR *TEMPLATE-LIST* NIL
  "List of names (symbols) of user-defined mail templates.")
(DEFVAR *TEMPLATE-COMMAND-ALIST* NIL
  "Alist of editor commands made from user-defined mail templates,
suitable for passing to SET-COMTAB.")
(DEFVAR *MSGS* NIL
  "In a mail template, the list of msg options it is applied to.")

(DEFMACRO DEFINE-MAIL-TEMPLATE (NAME DOCUMENTATION &BODY BODY)
  "Define a mail template named NAME.
DOCUMENTATION is a string whose first line is brief documentation
 and whose entire text is the full documentation.
BODY does the work.
The template defines an editor command named COM-<NAME>,
made available through Meta-X while editing messages and composing mail.
Thus, a template whose NAME is FOO-BAR would be Meta-X Foo Bar.

Good functions to call from the body of the mail template are
ZWEI:ADD-FIELD, ZWEI:DEFAULT-FIELD, ZWEI:DELETE-FIELD, ZWEI:FIND-FIELD,
ZWEI:ADD-TEXT-START and ZWEI:ADD-TEXT-END.  See their documentation.

If invoked from ZMAIL, the local variable ZWEI:MSGS will be a list
/(possibly NIL) of ZMAIL MSG structures for messages,
 including this message if you are editing one,
 or the messages you are forwarding or replying to.
If not invoked from ZMAIL, that local variable will be NIL."
  (LET ((COMMAND-NAME (INTERN (STRING-APPEND "COM-" NAME) (SYMBOL-PACKAGE 'FOO))))
    `(PROGN
            (DEFUN ,NAME (*INTERVAL* *MSGS*)
              ,DOCUMENTATION
              . ,BODY)
            (DEFCOM ,COMMAND-NAME
                    ,DOCUMENTATION ()
              (,NAME (TEMPLATE-INTERVAL) (TEMPLATE-MSGS))
              DIS-TEXT)
            (UNLESS (MEMQ ',NAME *TEMPLATE-LIST*)
              (PUSH ',NAME *TEMPLATE-LIST*))
            (UNLESS (ASSQ ,(GET-PNAME COMMAND-NAME) *TEMPLATE-COMMAND-ALIST*)
              (SETQ *TEMPLATE-COMMAND-ALIST*
                    (APPEND *TEMPLATE-COMMAND-ALIST*
                            (MAKE-COMMAND-ALIST '(,COMMAND-NAME)))))
            (LET ((ALIST (MAKE-COMMAND-ALIST '(,COMMAND-NAME))))
              (UNLESS (ASSQ ',COMMAND-NAME *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)
                (SETQ *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*
                      (APPEND ALIST *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)))
              (SET-COMTAB *MSG-COMTAB* () ALIST)))))

(DEFUN TEMPLATE-INTERVAL ()
  (COND ((TYPEP *INTERVAL* 'ZMAIL-BUFFER)
         (UNLESS *MSG*
           (BARF "There is no current message."))
         (MSG-INTERVAL *MSG*))
        ((TYPEP *INTERVAL* 'DRAFT-MSG) *INTERVAL*)
        ((TYPEP (NODE-SUPERIOR *INTERVAL*) 'DRAFT-MSG) (NODE-SUPERIOR *INTERVAL*))
        (T *INTERVAL*)))

(DEFUN TEMPLATE-MSGS ()
  (LET ((INT (TEMPLATE-INTERVAL)))
    (COND ((TYPEP INT 'DRAFT-MSG) (OR (DRAFT-MSG-MSGS-BEING-REPLIED-TO INT)
                                      (DRAFT-MSG-MSGS-BEING-FORWARDED INT)))
          ((AND (NODE-SUPERIOR INT)
                (NODE-SUPERIOR (NODE-SUPERIOR INT))
                (TYPEP (NODE-SUPERIOR (NODE-SUPERIOR INT)) 'ZMAIL-BUFFER))
           (LIST *MSG*)))))

(DEFFLAVOR ZMAIL-UTILITY-FRAME
           (MODE-LINE-WINDOW EDITOR-CLOSURE)
           (ZMAIL-FRAME-MIXIN ZMAIL-COMMAND-LOOP-MIXIN
            TV:ANY-TYI-MIXIN TV:STREAM-MIXIN TV:BORDERS-MIXIN
            TV:ITEM-LIST-PANE-KLUDGE TV:FRAME-WITH-XOR-BUTTONS
            TV:CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER TV:MINIMUM-WINDOW)
  :ABSTRACT-FLAVOR)

;; The variables bound by the ZMAIL frame's editor closure.

(DEFVAR *DRAFT-HEADER-WINDOW* :UNBOUND "Headers when sending")
(DEFVAR *DRAFT-TEXT-WINDOW* :UNBOUND "Text when sending")
(DEFVAR *MSG-WINDOW* :UNBOUND
  "Text of message being read, or headers and text when sending.
Note that each mail composition configuration includes uses
*MSG-WINDOW* or both *DRAFT-HEADER-WINDOW* and *DRAFT-TEXT-WINDOW*
for composing the message.  In the latter case, it may use
*MSG-WINDOW* for the message being replied to.")
(DEFVAR *SUMMARY-WINDOW* :UNBOUND "Summary of messages")
(DEFVAR *FILTER-WINDOW* :UNBOUND "Hairy menu pane for filter mode")
(DEFVAR *PROFILE-WINDOW* :UNBOUND "Frame for changing profile")
(DEFVAR *PROFILE-EDITOR* :UNBOUND "Editor that goes with above and below")
(DEFVAR *PROFILE-EDITOR-WINDOW* :UNBOUND)
(DEFVAR *COMMAND-MENU* :UNBOUND "The main command menu")
(DEFVAR *KEYWORD-WINDOW* :UNBOUND "Menu for hacking keywords")
(DEFVAR *WINDOW-CONFIGURATION* :UNBOUND "The present configuration")
(DEFVAR *CURRENT-MSG-NAME* :UNBOUND "The number of the current msg as a string")
(DEFVAR *ZMAIL-INTERVAL-NAME* :UNBOUND "Name of interval in reply for mode line")
(DEFVAR *ZMAIL-BUFFER* :UNBOUND "The buffer being hacked")
(DEFVAR *ZMAIL-BUFFER-LIST* :UNBOUND "The list of ones known about")
(DEFVAR *PRIMARY-ZMAIL-BUFFER* :UNBOUND "The one associated with new mail")
(DEFVAR *ZMAIL-FILE-NAME* :UNBOUND "Name of that file for the mode line")
(DEFVAR *MSG* :UNBOUND "The current message")
(DEFVAR *MSG-NO* :UNBOUND "Numerical index of above")
(DEFVAR *MSG-POINT-PDL* :UNBOUND "Saved positions")
(DEFVAR *ZMAIL-BACKGROUND-PROCESS* :UNBOUND "Handles asynchronous tasks")
(DEFVAR *ZMAIL-BACKGROUND-PROCESS-LOCK* :UNBOUND "Lock for synchronizing")
(DEFVAR *ZMAIL-BACKGROUND-REQUEST-CELL* :UNBOUND "Locative for communication")
(DEFVAR *BACKGROUND-RESPONSE-QUEUE* :UNBOUND
  "Locative to list of responses from background process.")
(DEFVAR *CURRENT-MSG-KEYWORDS-STRING* :UNBOUND "String of current messages keywords")
(DEFVAR *SELECTABLE-MODE-LINE-ELEMENTS* :UNBOUND "Alist of modeline element and command")
(DEFVAR *MSG-MORE-STRING* :UNBOUND "When can scroll message from mode line")
(DEFVAR *DRAFT-LIST* :UNBOUND "List of drafts for continue")
(DEFVAR *DEFAULT-MOVE-ZMAIL-BUFFER* :UNBOUND "When clicking left on move command")
(DEFVAR *MOVE-ZMAIL-BUFFER-MENU* :UNBOUND "Pop-up for choosing where")
(DEFVAR *ZMAIL-MAP-COMMAND-MENU* :UNBOUND "Things you can do to all messages")
(DEFVAR *SELECT-ZMAIL-BUFFER-MENU* :UNBOUND "For select command")
(DEFVAR *FILTER-SELECTION-FRAME* :UNBOUND "Frame for choosing a filter")
(DEFVAR *UNIVERSE-SELECTION-MENU* :UNBOUND "Menu for choosing a universe for filter")
(DEFVAR *UNIVERSE-DEFINITION-FRAME* :UNBOUND "Frame for defining a new universe")
(DEFVAR *OVERLYING-WINDOW* :UNBOUND "For scrolling typeout of message texts")
(DEFVAR *POP-UP-MINI-BUFFER-EDITOR* :UNBOUND "For asking temporary questions")


;; These are setq'd by INITIALIZE-FOR-MAIL,
;; Each caller of that function must bind them.

(DEFVAR *DRAFT-MSG* :UNBOUND "DRAFT-MSG being edited.  Bound only while composing.")

(DEFVAR *DRAFT-HEADER-INTERVAL* :UNBOUND
  "An interval holding just the headers of the draft being edited.
It is the current interval of *DRAFT-HEADER-WINDOW*.")
(DEFVAR *DRAFT-TEXT-INTERVAL* :UNBOUND
  "An interval holding just the text of the draft being edited.
It is the current interval of *DRAFT-TEXT-WINDOW*")

;;; Other widely-used variables.

(DEFVAR *INSIDE-MAIL* NIL
  "T while composing mail (ie, within ZMAIL-MAIL).")

(DEFVAR *END-SENDS-MESSAGE-P* T
  "T if the End key now will send the draft being composed.
Value is used only within ZMAIL-MAIL.")

(DEFVAR *WINDOW-CONFIGURATION-ALIST* '(("Summary only" :VALUE :SUMMARY
                                        :DOCUMENTATION "Just the summary window.")
                                       ("Both" :VALUE :BOTH
                                        :DOCUMENTATION
                                       "Summary window at the top and message at the bottom.")
                                       ("Message only" :VALUE :MSG
                                        :DOCUMENTATION "Just display current message.")
                                       ("Experimental" :VALUE :NEW
                                        :DOCUMENTATION "Every command takes a filter."))

  "List of the different window configurations available.")

(DEFVAR *DELETE-DIRECTION-ALIST* '(("Backward" :VALUE :BACKWARD :DOCUMENTATION
                                    "Move backward after deleting this message.")
                                   ("Forward" :VALUE :FORWARD :DOCUMENTATION
                                    "Move forward after deleting this message.")
                                   ("Remove" :VALUE :REMOVE :DOCUMENTATION
                               "Actually remove this message from this temporary buffer.")
                                   ("No" :VALUE :NO :DOCUMENTATION
                                    "Do not move after the deletion.")))

(DEFVAR *REPLY-MODES-ALIST*
        '((("All" :VALUE :ALL :DOCUMENTATION "To Sender and original To, Cc original Cc.")
           ("All-Cc" :VALUE :ALL-CC :DOCUMENTATION "To Sender, Cc original To and Cc.")
           ("Cc-All" :VALUE :CC-ALL
                     :DOCUMENTATION "To original To, Cc Sender and original Cc.")
           ("To" :VALUE :TO :DOCUMENTATION "To Sender and original To.")
           ("To-Cc" :VALUE :TO-CC :DOCUMENTATION "To Sender, Cc original To.")
           ("Cc-To" :VALUE :CC-TO :DOCUMENTATION "To original To, Cc Sender.")
           ("Sender" :VALUE :SENDER :DOCUMENTATION "To Sender."))
          (("Like Mail" :VALUE :LIKE-MAIL :DOCUMENTATION
            "Just text of reply, like Mail command.")
           ("Show Original" :VALUE :SHOW-ORIGINAL
            :DOCUMENTATION "Message being replied to on top and reply below.")
           ("Yank" :VALUE :YANK
            :DOCUMENTATION "Just text of reply with message being replied to yanked in.")))
  "List of all the reply modes available.")

;; Like the previous, but has entries for the obsolete mode keywords.
;; Used in creating documentation lines from keywords.
(DEFVAR *REPLY-OLD-MODES-ALIST*
        (LIST (CAR *REPLY-MODES-ALIST*)
              (APPEND (CADR *REPLY-MODES-ALIST*)
                      '(("Like Mail" :VALUE :ONE-WINDOW)
                        ("Show Original" :VALUE :TWO-WINDOWS))))
  "Like *REPLY-MODES-ALIST* but it has entries for the obsolete mode keywords.  Used in
creating documentation lines from keywords.")

(DEFVAR *YES-NO-ASK-ALIST*
        '(("Yes" :VALUE T) ("No" :VALUE NIL) ("Ask" . :ASK))
  "List of things for the YES-NO-ASK menu that pops up sometimes.")

(DEFVAR *MAIL-SENDING-MODE-ALIST*
        '(("COMSAT" :VALUE FILE-SEND-IT :SITE-KEYWORD :COMSAT
           :DOCUMENTATION "Write a request file for the mailer via the file job.")
          ("Chaos" :VALUE CHAOS-SEND-IT :SITE-KEYWORD :CHAOS
           :DOCUMENTATION "Mail via the chaosnet mail protocol to some MAIL server.")
          ("Chaos Direct" :VALUE CHAOS-DIRECT-SEND-IT :SITE-KEYWORD :CHAOS
                                                      :DEFAULT-SITE-KEYWORD :DIRECT-CHAOS
           :DOCUMENTATION "Mail via the chaosnet mail protocol to each host specified.")
          ("Ether" :VALUE ETHER-SEND-IT :SITE-KEYWORD :ETHER
           :DOCUMENTATION "Mail via the ethernet mail protocol.")
          ("SMTP" :value smtp-send-it :SITE-KEYWORD :SMTP
           :documentation "Send mail via the SMTP protocol.")
          ))

(DEFVAR *HEADER-FORCE-ALIST* '(("None" :VALUE :NONE
                                :DOCUMENTATION "No special header force, COMSAT chooses.")
                               ("RFC733" :VALUE :RFC733
                                :DOCUMENTATION "RFC733 standard headers.")
                               ("Network" :VALUE :NETWORK
                                :DOCUMENTATION "Network standard headers.")
                               ("ITS" :VALUE :ITS
                                :DOCUMENTATION "Single line ITS headers."))
  "List of the ways headers could be.")

(DEFVAR *HEADER-FORMAT-ALIST* '(("Short" :VALUE :SHORT :DOCUMENTATION
  "Use /"@/" to separate user and host, no personal names.")
                                ("Long" :VALUE :LONG :DOCUMENTATION
  "Use /" at /" to separate user and host, no personal names.")
                                ("Include personal" :VALUE :INCLUDE-PERSONAL :DOCUMENTATION
  "Include the user's personal name if any.")
                                ("Use original" :VALUE :USE-ORIGINAL :DOCUMENTATION
  "Use the exact text of the address from the original."))
  "A list of the header formats available.")

(DEFVAR *BUG-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation for sending a bug report using the BUG option on the MAIL menu.")

(DEFVAR *ZMAIL-MAIL-MENU-ALIST*
        `(("Bug" :VALUE :BUG
           :DOCUMENTATION ,*BUG-DOCUMENTATION*)
          ("Mail" :VALUE :MAIL :DOCUMENTATION "Normal mail.")
          ("Forward" :VALUE :FORWARD :DOCUMENTATION
  "Forward this message.  Starts sending a message with this as its text.")
          ("Redistribute" :VALUE :REDISTRIBUTE :DOCUMENTATION
  "Redistribute this message.  Sends a message with the original headers plus Redistributed.")
          ("Local" :VALUE :LOCAL
           :DOCUMENTATION "Create new message in current buffer and edit it.")))

(DEFVAR *MOVE-TO-NEXT-MENU-ALIST*
        '(("Next undeleted" :VALUE :NEXT-UNDELETED :DOCUMENTATION
           "Move to next undeleted message.")
          ("Next unseen" :VALUE :NEXT-UNSEEN :DOCUMENTATION
           "Move to next message you have not seen the text of")
          ("Next" :VALUE :NEXT :DOCUMENTATION
           "Move to next message, even if deleted.")
          ("Last undeleted" :VALUE :LAST-UNDELETED :DOCUMENTATION
           "Move to last undeleted message in the buffer.")
          ("Last unseen" :VALUE :LAST-UNSEEN :DOCUMENTATION
           "Move to last message in buffer you have not seen the text of")
          ("Last" :VALUE :LAST :DOCUMENTATION
           "Move to last message in buffer, even if deleted"))
  "List of items for the NEXT menu of zmail.")

(DEFVAR *MOVE-TO-PREVIOUS-MENU-ALIST*
        '(("Previous undeleted" :VALUE :PREVIOUS-UNDELETED :DOCUMENTATION
           "Move to previous undeleted message.")
          ("Previous unseen" :VALUE :PREVIOUS-UNSEEN :DOCUMENTATION
           "Move to previous message you have not seen the text of")
          ("Previous" :VALUE :PREVIOUS :DOCUMENTATION
           "Move to previous message, even if deleted.")
          ("First undeleted" :VALUE :FIRST-UNDELETED :DOCUMENTATION
           "Move to first undeleted message in the buffer.")
          ("First unseen" :VALUE :FIRST-UNSEEN :DOCUMENTATION
           "Move to first message in buffer you have not seen the text of")
          ("First" :VALUE :FIRST :DOCUMENTATION
           "Move to first message in buffer, even if deleted"))
  "List of items for the PREVIOUS menu of zmail.")

(DEFVAR *ZMAIL-MAP-COMMAND-ALIST*
        '(("Delete" . COM-ZMAIL-DELETE-ALL)
          ("Undelete" . COM-ZMAIL-UNDELETE-ALL)
          ("Type" . COM-ZMAIL-TYPE-ALL)
          ("Find string" . COM-ZMAIL-OCCUR)
          ("Keywords" . COM-ZMAIL-KEYWORDS-ALL)
          ("Unkeywords" . COM-ZMAIL-UNKEYWORDS-ALL)
          ("Move" . COM-ZMAIL-MOVE-ALL-TO-FILE)
          ("Forward" . COM-ZMAIL-FORWARD-ALL)
          ("Redistribute" . COM-ZMAIL-REDISTRIBUTE-ALL)
          ("Reply" . COM-ZMAIL-REPLY-ALL)
          ("Concatenate" . COM-ZMAIL-CONCATENATE-ALL)))

(DEFVAR *KEYWORDS-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "The documentation on commands for the KEYWORD menu")
(DEFVAR *SUMMARY-MOVE-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation on commands for SUMMARY move.")
(DEFVAR *SUMMARY-REPLY-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation on SUMMARY replying.")
(DEFVAR *EDIT-MSG-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation for the editting messages.")

(DEFVAR *UNIVERSE-BUTTON-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation for universe buttons.")
(DEFVAR *FILTER-BUTTON-DOCUMENTATION* (MAKE-EMPTY-STRING 95.)
  "Documentation for FILTER buttons.")

;;; If you change this list, look at ZMAIL-SUMMARY-MOUSE, which knows the order of the
;;; elements
(DEFVAR *SUMMARY-MOUSE-MENU-ALIST*
        `(("Continue" :VALUE :REPLY :DOCUMENTATION "Continue sending this draft message.")
          ("Keywords" :VALUE :KEYWORDS
           :DOCUMENTATION ,*KEYWORDS-DOCUMENTATION*)
          ("Delete" :VALUE :DELETE
           :DOCUMENTATION "Delete this message.")
          ("Undelete" :VALUE :UNDELETE
           :DOCUMENTATION "Undelete this message.")
          ("Remove" :VALUE :REMOVE
           :DOCUMENTATION "Remove this message from this temporary buffer.")
          ("Reply" :VALUE :REPLY :DOCUMENTATION ,*SUMMARY-REPLY-DOCUMENTATION*)
          ("Move" :VALUE :MOVE
           :DOCUMENTATION ,*SUMMARY-MOVE-DOCUMENTATION*)
          ("Append" :VALUE :APPEND :DOCUMENTATION
  "Append this message to the end of another:  L: current message; R: specify from summary.")
          ("Filter" :VALUE :FILTER
                    :DOCUMENTATION "Filter according to some attribute of this message."))
  "The list of things in the summary menu.")

(DEFVAR *SUMMARY-MOUSE-MIDDLE-MENU-ALIST*
        `(("Delete//Undelete" :VALUE :DELETE-OR-UNDELETE :DOCUMENTATION
           "Delete if not deleted, else undelete.")
          ("Delete//Remove" :VALUE :DELETE-OR-REMOVE :DOCUMENTATION
           "Delete if from file buffer, else remove.")
          . ,(CDR *SUMMARY-MOUSE-MENU-ALIST*))
  "Menu list for the middle button of the SUMMARY menu.")

(DEFVAR *REQUIRE-SUBJECTS-ALIST*
        '(("Yes" :VALUE T)
          ("No" :VALUE NIL)
          ("On bug reports" :VALUE :BUG)
          ("Initial but not required" :VALUE :INIT)))

;;; This is for defining things that should be reset when the user changes
(DEFVAR *ZMAIL-GLOBAL-INITIALIZATION-LIST* NIL)
(DEFMACRO DEFINE-ZMAIL-GLOBAL (VAR &OPTIONAL (INITIAL-VALUE ':UNBOUND IVP) DOCUMENTATION)
  `(PROGN 'COMPILE
     (DEFVAR ,VAR ,INITIAL-VALUE ,DOCUMENTATION)
     (SETQ *ZMAIL-GLOBAL-INITIALIZATION-LIST*
           (DELQ (ASSQ ',VAR *ZMAIL-GLOBAL-INITIALIZATION-LIST*)
                 *ZMAIL-GLOBAL-INITIALIZATION-LIST*))
     ,(AND IVP `(PUSH (CONS ',VAR ,INITIAL-VALUE) *ZMAIL-GLOBAL-INITIALIZATION-LIST*))))

(DEFVAR *ZMAIL-WHO-LINE-DOCUMENTATION-SYMBOLS* NIL)
(DEFMACRO DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER (COMMAND ARGLIST &BODY BODY)
  `(PROGN 'COMPILE
     (PUSH* ',COMMAND *ZMAIL-WHO-LINE-DOCUMENTATION-SYMBOLS*)
     (DEFUN (,COMMAND WHO-LINE-DOCUMENTATION-UPDATER) ,ARGLIST
       . ,BODY)))

(DEFMACRO DEFINE-COMMAND-WHO-LINE-DOCUMENTATION (COMMAND STRING)
  `(PUTPROP ',COMMAND ,STRING :WHO-LINE-DOCUMENTATION))

(DEFVAR *OPTIONS-NOT-IN-ALIST* NIL "List of options not in *ZMAIL-USER-OPTION-ALIST*.")
(DEFMACRO ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION (OPTION COMMAND)
  `(PROGN 'COMPILE
     (PUSH ',COMMAND (GET ',OPTION 'DOCUMENTATION-ASSOCIATED-COMMANDS))
     (OR (ASSQ ',OPTION *ZMAIL-USER-OPTION-ALIST*)
         (PUSH ',OPTION *OPTIONS-NOT-IN-ALIST*))))

;;; Random variables
(DEFVAR *ZMAIL-PATHNAME-DEFAULTS* nil
  "Defaults for pathname parsing")
(DEFVAR *ZMAIL-COMTAB* nil
  "Main keyboard comtab")
(DEFVAR *MSG-COMTAB* nil
  "COMTAB in the message window")
(DEFVAR *MSG-CONTROL-X-COMTAB* nil
  "C-X comtab in message window")
(DEFVAR *REPLY-COMTAB* nil
  "COMTAB in the sending window")
(DEFVAR *REPLY-CONTROL-X-COMTAB* nil
  "C-X comtab in the sending window")
(DEFVAR *ZMAIL-COMMAND-BUTTON*)                 ;Extended commands
(DEFVAR *ZMAIL-BACKGROUND-P* NIL "T if within background process.")
(DEFVAR *MY-ADDRESS*)                           ;String of network address
(DEFINE-ZMAIL-GLOBAL *KEYWORD-ALIST* NIL "Currently defined keywords")
(DEFINE-ZMAIL-GLOBAL *USER-FILTER-ALIST* NIL "Currently defined user filters")
(DEFINE-ZMAIL-GLOBAL *UNIVERSE-LIST* NIL "User defined mapping for universe.")

;; This line goes in the summary to contain the column headings.
(DEFVAR *SUMMARY-WINDOW-LABEL* (MAKE-SUMMARY-LINE))

;; This is bound by each :COMMAND-LOOP.
(DEFVAR *LAST-SUMMARY-MOUSE-ITEM* NIL)

(DEFVAR *ALL-ZMAIL-WINDOWS* NIL "All windows of flavor ZMAIL-WINDOW.")

(DEFVAR *EXPLICIT-OPTION-UPDATE* NIL
  "T if updating options because user did something in the CVV window.
Causes some options to be willing to ask questions about what to
do about the change.")

;;; User options
(DEFINE-USER-OPTION-ALIST *ZMAIL-USER-OPTION-ALIST* DEFINE-ZMAIL-USER-OPTION)

;;; These are user options, in that they are automatically written into the file,
;;; but they are modified by special means
(DEFINE-ZMAIL-USER-OPTION *OTHER-MAIL-FILE-NAMES* NIL :PATHNAME-LIST)
(TV:RESTRICT-USER-OPTION *OTHER-MAIL-FILE-NAMES* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-KEYWORDS-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-KEYWORDS-ALIST* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-MOVE-MAIL-FILE-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-MOVE-MAIL-FILE-ALIST* :NEVER)
(DEFINE-ZMAIL-USER-OPTION *FILTER-REFERENCE-UNIVERSE-ALIST* NIL :SEXP)
(TV:RESTRICT-USER-OPTION *FILTER-REFERENCE-UNIVERSE-ALIST* :NEVER)

;;; Real user options
(DEFINE-ZMAIL-USER-OPTION *FILTER-SUMMARY-WINDOW-FRACTION* NIL :NUMBER-OR-NIL
                          "Fraction of the frame occupied by the summary in filter mode")
(DEFINE-ZMAIL-USER-OPTION *SUMMARY-WINDOW-FRACTION* 0.45s0 :NUMBER
                          "Fraction of the frame occupied by the summary")
(DEFINE-ZMAIL-USER-OPTION *SUMMARY-SCROLL-FRACTION* 0.2s0 :NUMBER
                          "Amount by which to glitch summary window")
(DEFINE-ZMAIL-USER-OPTION *NEW-MAIL-FILE-APPEND-P* :STICKY :MENU-ALIST
                          "Should newly created mail files append"
                          '(("Append" :VALUE :APPEND
                             :DOCUMENTATION "New mail files append messages.")
                            ("Prepend" :VALUE :PREPEND
                             :DOCUMENTATION "New mail files prepend messages.")
                            ("Sticky" :VALUE :STICKY :DOCUMENTATION
  "New mail files inherit whether they append messages from the current mail file.")
                            ("Ask" :VALUE :ASK :DOCUMENTATION
  "User is always queried when creating a new mail file as to whether it appends messages.")))
;;; This can only be useful for Twenex and Symbolics LMFS currently
(define-zmail-user-option *default-mail-buffer-generation-retention-count* nil :number-or-nil
                          "Generation retention count for newly created mail files")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-SUMMARY-TEMPLATE* T :SEXP
                          "Default summary display format")
;;; Old name which may be in some init files
(FORWARD-VALUE-CELL '*SUMMARY-INCLUDE-DATE* '*DEFAULT-SUMMARY-TEMPLATE*)

(DEFINE-ZMAIL-USER-OPTION *GMSGS-OTHER-SWITCHES* "//Z" :STRING
                          "Other switches to supply to GMSGS server")
(TV:RESTRICT-USER-OPTION *GMSGS-OTHER-SWITCHES* :IF :GMSGS)
(DEFINE-ZMAIL-USER-OPTION *RUN-GMSGS-P* :NO :ASSOC
                          "Run GMSGS before getting new mail"
                          '(("Yes" . :YES) ("No" . :NO) ("Once only" . :ONCE-ONLY)))
(TV:RESTRICT-USER-OPTION *RUN-GMSGS-P* :IF :GMSGS)

(DEFINE-ZMAIL-USER-OPTION *MAIL-FILE-FOR-DRAFTS* NIL :PATHNAME-OR-NIL
                          "Mail file to store drafts in")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-DRAFT-FILE-NAME* NIL :PATHNAME-OR-NIL
                           "Default file for saving draft")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-MOVE-MAIL-FILE-NAME* NIL :PATHNAME-OR-NIL
                          "Default file for moving a message to a new file")
(DEFINE-ZMAIL-USER-OPTION *MOVE-FILE-NAME-STICKY-FN2* T :BOOLEAN
                          "Take file type for moving to a new file from default")
(DEFINE-ZMAIL-USER-OPTION *TEXT-MAIL-FILE-SEPARATOR* "" :STRING
                          "Line between messages in text mail file")
(DEFINE-ZMAIL-USER-OPTION *ZMAIL-STARTUP-FILE-NAME* NIL :PATHNAME-OR-NIL
                          "File read in at startup")
(DEFINE-ZMAIL-USER-OPTION *ZMAIL-USUAL-MAIL-FILE-DIRECTORY* () :PATHNAME-OR-NIL
                          "Directory where most of your mail files live")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-END* "" :STRING
                          "Format line after forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-SEPARATOR* "" :STRING
                          "Format line between forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-MESSAGE-BEGIN* "" :STRING
                          "Format line before forwarded messages")
(DEFINE-ZMAIL-USER-OPTION *DONT-REPLY-TO* '("INFO-*") :STRING-LIST  "People not to reply to")

(DEFINE-ZMAIL-USER-OPTION *MIDDLE-REPLY-MODE* :SENDER :MENU-ALIST
                          "Default reply to for middle button"
                          (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *MIDDLE-REPLY-WINDOW-MODE* :SHOW-ORIGINAL :MENU-ALIST
                          "Default reply window setup for middle button"
                          (CADR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *1R-REPLY-MODE* :SENDER :MENU-ALIST
                          "Default reply with argument of 1 to"
                          (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *REPLY-MODE* :ALL :MENU-ALIST
                          "Default reply to"
                          (CAR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *REPLY-WINDOW-MODE* :LIKE-MAIL :MENU-ALIST
                          "Default reply window setup"
                          (CADR *REPLY-MODES-ALIST*))
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-MAIL-WINDOW-CONFIGURATION* :NORMAL :MENU-ALIST
                          "Default window configuration when mailing"
                          `(("Normal" :VALUE :NORMAL
                             :DOCUMENTATION "Draft message replaces message being read.")
                            ("Send" :VALUE :SEND
                             :DOCUMENTATION "Separate headers and text windows.")
                            . ,(CDR *WINDOW-CONFIGURATION-ALIST*)))
(DEFINE-ZMAIL-USER-OPTION *SEND-HEADER-FORMAT* :INCLUDE-PERSONAL :MENU-ALIST
                          "Format of recipients in headers sent (except via COMSAT)"
                          *HEADER-FORMAT-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *REPLY-HEADER-FORMAT* :SHORT :MENU-ALIST
                          "Format of recipients inserted for reply"
                          *HEADER-FORMAT-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-HEADER-FORCE* :NONE :MENU-ALIST
                          "Default header force (via COMSAT)"
                          *HEADER-FORCE-ALIST*)
(TV:RESTRICT-USER-OPTION *DEFAULT-HEADER-FORCE* :IF :COMSAT)
(DEFINE-ZMAIL-USER-OPTION *LOCAL-MAIL-HEADER-FORCE* :ITS :MENU-ALIST
                          "Header force for local messages" *HEADER-FORCE-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *LOCAL-MAIL-INCLUDE-SUBJECT* T :BOOLEAN
                          "Local mail starts out with a subject")
(DEFINE-SITE-ALIST-USER-OPTION (*MAIL-SENDING-MODE* *ZMAIL-USER-OPTION-ALIST*)
                               "Mail sending mode" *MAIL-SENDING-MODE-ALIST*
                               :DEFAULT-MAIL-MODE)
(DEFINE-ZMAIL-USER-OPTION *DELETE-EXPIRED-MSGS* :PER-FILE :MENU-ALIST
                          "Automatically delete expired messages"
                          `(,@*YES-NO-ASK-ALIST*
                            ("Per file" . :PER-FILE)))
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-FCC-LIST* NIL :PATHNAME-LIST
                          "Default initial Fcc list")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-CC-LIST* NIL :ADDRESS-LIST
                          "Default initial Cc list")
(DEFINE-ZMAIL-USER-OPTION *REQUIRE-SUBJECTS* NIL :MENU-ALIST
                          "Require subjects on outgoing messages"
                          *REQUIRE-SUBJECTS-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *GENERATE-IN-REPLY-TO-FIELD* T :BOOLEAN
                          "Automatically generate In-reply-to fields")

(DEFINE-ZMAIL-USER-OPTION *GENERATE-MESSAGE-ID-FIELD* T :BOOLEAN
                          "Automatically generate Message-ID fields")
;;; This is not implemented yet.
(define-zmail-user-option *preserve-msg-references-across-expunge* T :boolean
  "Preserve conversations inspite of intervening expunged messages")

(DEFINE-ZMAIL-USER-OPTION *SUMMARY-MOUSE-MIDDLE-MODE* :DELETE-OR-UNDELETE :MENU-ALIST
                          "Middle button on summary window"
                          *SUMMARY-MOUSE-MIDDLE-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *NEXT-MIDDLE-MODE* :LAST-UNDELETED :MENU-ALIST
                          "Middle button on Next command"
                          *MOVE-TO-NEXT-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *PREVIOUS-MIDDLE-MODE* :FIRST-UNDELETED :MENU-ALIST
                          "Middle button on Previous command"
                          *MOVE-TO-PREVIOUS-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *MAP-MIDDLE-MODE* NIL :ASSOC
                          "Middle button on Map command"
                          *ZMAIL-MAP-COMMAND-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *MAIL-MIDDLE-MODE* :BUG :MENU-ALIST
                          "Middle button on Mail command"
                          *ZMAIL-MAIL-MENU-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-INITIAL-WINDOW-CONFIGURATION* :BOTH :MENU-ALIST
                          "Default startup window setup"
                          *WINDOW-CONFIGURATION-ALIST*)
(DEFINE-ZMAIL-USER-OPTION *DELETE-MIDDLE-MODE* :BACKWARD :MENU-ALIST
                          "Direction to move for click middle on delete"
                          *DELETE-DIRECTION-ALIST*)
(DEFINE-ZMAIL-USER-OPTION  *NEXT-AFTER-DELETE* :FORWARD :MENU-ALIST
                           "Direction to move after delete"
                           *DELETE-DIRECTION-ALIST*)

(DEFINE-ZMAIL-USER-OPTION *PRUNE-HEADERS-AFTER-YANKING* NIL :BOOLEAN
                          "Prune headers of yanked messages")

(DEFINE-ZMAIL-USER-OPTION *INHIBIT-BACKGROUND-MAIL-CHECKS* NIL :BOOLEAN
                          "Do not check for new mail in the background")
(define-zmail-user-option *notify-on-new-mail-in-background* nil :assoc
                          "Whether and how to notify user of new mail received in background"
                          '(("Converse" . :converse) ("Notify" . :notify) ("Never" . nil)))
(DEFINE-ZMAIL-USER-OPTION *INHIBIT-BACKGROUND-SAVES* NIL :BOOLEAN
                          "Do not automatically save after get new mail")

(DEFINE-ZMAIL-USER-OPTION *ONE-WINDOW-AFTER-YANK* T :BOOLEAN
                          "Just show headers and text after yanking in message")
(DEFINE-ZMAIL-USER-OPTION *ALWAYS-JUMP-AFTER-GET-NEW-MAIL* NIL :BOOLEAN
                          "Move to first message even when no new mail")
(DEFINE-ZMAIL-USER-OPTION *FORWARDED-ADD-SUBJECT* T :BOOLEAN
                          "Forwarded messages are supplied with a subject")
(DEFINE-ZMAIL-USER-OPTION *QUERY-BEFORE-EXPUNGE* NIL :BOOLEAN
                          "Show headers and ask before expunging deleted messages")
(DEFINE-ZMAIL-USER-OPTION *DELETE-AFTER-MOVE-TO-FILE* T :BOOLEAN
                          "Delete message when moved into another buffer")
(DEFINE-ZMAIL-USER-OPTION *MAIL-HEADER-DELIMITER* "--Text Follows This Line--" :STRING
                          "Separator between headers and text in draft messages.")

(DEFINE-ZMAIL-USER-OPTION *DELETE-UNDIGESTIFIED-MESSAGE* T :BOOLEAN
                          "Delete original message after undigestification")
(DEFINE-ZMAIL-USER-OPTION *CLIP-UNDIGESTIFIED-MESSAGE* () :BOOLEAN
                          "Clip contents of original message after undigestification")
(DEFINE-ZMAIL-USER-OPTION *INHERIT-SUBJECT-FIELD* T :BOOLEAN
                          "Messages from a digest show which one in Subject: line")

(DEFINE-ZMAIL-USER-OPTION *DEFAULT-MAIL-TEMPLATE* NIL :SEXP
  "Name (a symbol) of DEFINE-MAIL-TEMPLATE template for ordinary mailing.")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-REPLY-TEMPLATE* NIL :SEXP
                          "Name (a symbol) of DEFINE-MAIL-TEMPLATE template for replies.")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-BUG-TEMPLATE* NIL :SEXP
                          "Name (a symbol) of DEFINE-MAIL-TEMPLATE template for bug reports.")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-FORWARDING-TEMPLATE* NIL :SEXP
                          "Name (a symbol) of DEFINE-MAIL-TEMPLATE template for forwarding.")
(DEFINE-ZMAIL-USER-OPTION *DEFAULT-REFORMATTING-TEMPLATE* NIL :SEXP
  "Name (a symbol) of DEFINE-MAIL-TEMPLATE template for reformatting incoming mail headers.")

(define-zmail-user-option *from-user-id* nil :string-or-nil
                          "If non-NIL, this is the user-name that goes in From: fields of mail, by default.")
(define-zmail-user-option *FROM-HOST* NIL :host-or-nil
                          "If non-NIL, this is the host whose name goes in From: fields of mail, by default.")

;;; Call this to update the item list of the Profile CVV window.  Needs to be done when patches
;;; call DEFINE-ZMAIL-USER-OPTION.
;;;>>>Then hope patches don't get loaded in world where ZMail is not running
;;;   (maybe user killed it).  So, at least check first.  If user says to make
;;;   a window, do so.

(defun update-zmail-profile-choice-window ()
  "Update the ZMail profile window from the current user option (variables) list.
If ZMail is not up and running, the user is asked whether to start it up (default is no)."
  (let ((zmail (find-or-maybe-initialize-zmail-window "to update profile window")))
    (if zmail
        (prog2 (send
                 (send (symeval-in-closure (send zmail :editor-closure) '*profile-window*)
                       :get-pane 'choose-window)
                 :set-items *zmail-user-option-alist*)
               t)
      (warn "No ZMail window running -- cannot update profile window"))))

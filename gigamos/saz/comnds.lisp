;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL  -*- machine mail reader
;;; ZMail Commands
;;; Definitions in DEFS
;;; When we go to real character stuff, this file needs to be hacked a bit.
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; An invalid Enhancements copyright notice on AI:ZMAIL; COMNDS 495 removed on 3/31/82 by RG
;;;  This file had been installed as the official MIT source in direct contravention
;;;  to instructions to Symbolics personnel acting on MIT's behalf.

;;; Set up main command table
(DEFUN INITIALIZE-ZMAIL-COMTABS (MODE-COMTAB)
  (SET-COMTAB MODE-COMTAB '(#/H-F COM-FORWARD-ADDRESS
                            #/H-B COM-BACKWARD-ADDRESS
                            #/H-K COM-KILL-ADDRESS
                            #/H-RUBOUT COM-BACKWARD-KILL-ADDRESS
                            #/H-T COM-EXCHANGE-ADDRESSES))

  (SETQ *ZMAIL-COMTAB* (SET-COMTAB '*ZMAIL-COMTAB*
                                   '(#/C-D COM-ZMAIL-DELETE-AND-UP
                                     #/C-G COM-BEEP
                                     #/C-F COM-ZMAIL-FIND-STRING
                                     #/C-L COM-RECENTER-WINDOW
                                     #/C-N COM-ZMAIL-DOWN-TO-NEXT
                                     #/C-P COM-ZMAIL-UP-TO-PREVIOUS
                                     #/C-R COM-EDIT-CURRENT-MSG
                                     #/C-U COM-UNIVERSAL-ARGUMENT
                                     #/C-V COM-NEXT-SCREEN
                                     #/C-Z COM-QUIT
                                     #/C-SP COM-ZMAIL-SET-POP-MARK
                                     #/M-V COM-PREVIOUS-SCREEN
                                     #/M-X COM-ZMAIL-EXTENDED-COMMAND
                                     #/M-? COM-ZMAIL-SELF-DOCUMENT
                                     #/M-~ COM-ZMAIL-NOT-MODIFIED
                                     #/M-< COM-ZMAIL-START-OF-MSG
                                     #/M-> COM-ZMAIL-END-OF-MSG
                                     #/C-M-V COM-SCROLL-SUMMARY-WINDOW
                                     #/C-M-SP COM-ZMAIL-MOVE-TO-PREVIOUS-POINT
                                     #/. COM-ZMAIL-START-OF-MSG
                                     #/? COM-ZMAIL-DOCUMENTATION
                                     #/C COM-ZMAIL-CONTINUE
                                     #/D COM-ZMAIL-DELETE
                                     #/E COM-ZMAIL-EXPUNGE
                                     #/F COM-ZMAIL-FORWARD
                                     #/G COM-GET-NEW-MAIL
                                     #/J COM-ZMAIL-JUMP
                                     #/L COM-ZMAIL-KEYWORDS
                                     #/M COM-ZMAIL-MAIL
                                     #/O COM-ZMAIL-MOVE
                                     #/N COM-ZMAIL-NEXT
                                     #/P COM-ZMAIL-PREVIOUS
                                     #/Q COM-ZMAIL-QUIT
                                     #/R COM-ZMAIL-REPLY
                                     #/S COM-ZMAIL-SAVE
                                     #/U COM-ZMAIL-UNDELETE
                                     #/X COM-ZMAIL-EXTENDED-COMMAND
                                     #/Z COM-ZMAIL-LARGE-ARGUMENT
                                     #/BREAK COM-ZMAIL-BREAK
                                     #/BS COM-PREVIOUS-SCREEN
                                     #/RUBOUT COM-PREVIOUS-SCREEN
                                     #/HAND-DOWN COM-NEXT-SCREEN
                                     #/HAND-UP COM-PREVIOUS-SCREEN
                                     #/HELP COM-ZMAIL-DOCUMENTATION
                                     #/FF COM-ZMAIL-REFRESH
                                     #/RESUME COM-ZMAIL-CONTINUE
                                     #/SP COM-NEXT-SCREEN
                                     #/- COM-NEGATE-NUMERIC-ARG
                                     #/C-- COM-NEGATE-NUMERIC-ARG
                                     #/M-- COM-NEGATE-NUMERIC-ARG
                                     #/C-M-- COM-NEGATE-NUMERIC-ARG
                                     (#/0 10.) COM-NUMBERS
                                     (#/C-0 10.) COM-NUMBERS
                                     (#/M-0 10.) COM-NUMBERS
                                     (#/C-M-0 10.) COM-NUMBERS
                                     )))

  (SETQ *MSG-COMTAB* (SET-COMTAB '*MSG-COMTAB*
                                 '(#/END COM-QUIT-ZMAIL-EDIT
                                   #/C- COM-QUIT-ZMAIL-EDIT
                                   #/ABORT COM-QUIT-ZMAIL-EDIT)))
  (SET-COMTAB-INDIRECTION *MSG-COMTAB* MODE-COMTAB)

  (SETQ *MSG-CONTROL-X-COMTAB*
        (SET-COMTAB '*MSG-CONTROL-X-COMTAB*
                    '(#/A COM-ADD-MORE-TEXT
                      #/C COM-ADD-CC-FIELD
                      #/S COM-ADD-SUBJECT-FIELD
                      #/T COM-ADD-TO-FIELD)))
  (SET-COMTAB-INDIRECTION *MSG-CONTROL-X-COMTAB* *STANDARD-CONTROL-X-COMTAB*)

  (SET-COMTAB MODE-COMTAB
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *MSG-CONTROL-X-COMTAB*)))

  (SETQ *REPLY-CONTROL-X-COMTAB*
        (SET-COMTAB '*REPLY-CONTROL-X-COMTAB*
                    '(#/2 COM-ZMAIL-REPLY-TWO-WINDOWS
                      #/3 COM-ZMAIL-REPLY-THREE-WINDOWS
                      #/1 COM-ZMAIL-REPLY-ONE-WINDOW
                      #/M COM-ZMAIL-RECURSIVE-MAIL
                      #/O COM-ZMAIL-OTHER-WINDOW
                      #/Y COM-PRUNE-YANKED-HEADERS
                      #/C-R COM-RESTORE-DRAFT-FILE
                      #/C-S COM-SAVE-DRAFT-FILE
                      #/C-W COM-WRITE-DRAFT-FILE
                      #/C-M-S COM-SAVE-DRAFT-AS-MSG)))
  (SET-COMTAB-INDIRECTION *REPLY-CONTROL-X-COMTAB* *MSG-CONTROL-X-COMTAB*)

  (SETQ *REPLY-COMTAB* (SET-COMTAB '*REPLY-COMTAB*
                                   '(#/END COM-MAIL-END
                                     #/C- COM-SEND-MESSAGE
                                     #/C-M-Y COM-ZMAIL-YANK
                                     #/ABORT COM-ABORT-SEND
                                     #/SUPER-ABORT COM-REALLY-ABORT-SEND
                                     #/C-] COM-ABORT-SEND)
                                   (MAKE-COMMAND-ALIST '(COM-ADD-TO-FIELD COM-ADD-CC-FIELD
                                                         COM-ADD-FTO-FIELD
                                                         COM-ADD-FCC-FIELD
                                                         COM-ADD-SUBJECT-FIELD
                                                         COM-ADD-IN-REPLY-TO-FIELD
                                                         COM-ADD-MORE-TEXT COM-ADD-FROM-FIELD
                                                         COM-ZMAIL-YANK-CURRENT-MSG
                                                         COM-PRUNE-YANKED-HEADERS
                                                         COM-SEND-MESSAGE COM-ABORT-SEND
                                                         COM-RESTORE-DRAFT-FILE
                                                         COM-WRITE-DRAFT-FILE
                                                         COM-SAVE-DRAFT-FILE
                                                         COM-SAVE-DRAFT-AS-MSG
                                                         COM-CHANGE-SUBJECT-PRONOUNS))))
  (SET-COMTAB *REPLY-COMTAB*
              (LIST #/C-X (MAKE-EXTENDED-COMMAND *REPLY-CONTROL-X-COMTAB*)))
  (SET-COMTAB *REPLY-COMTAB* (LIST #/MOUSE-3-1
                                   (MAKE-MENU-COMMAND '(COM-ADD-TO-FIELD COM-ADD-CC-FIELD
                                                        COM-ADD-FCC-FIELD
                                                        COM-ADD-SUBJECT-FIELD
                                                        COM-ADD-IN-REPLY-TO-FIELD
                                                        COM-ADD-MORE-TEXT COM-ADD-FROM-FIELD
                                                        COM-PRUNE-YANKED-HEADERS
                                                        COM-SEND-MESSAGE COM-ABORT-SEND
                                                        COM-RESTORE-DRAFT-FILE
                                                        COM-WRITE-DRAFT-FILE
                                                        COM-SAVE-DRAFT-FILE
                                                        COM-SAVE-DRAFT-AS-MSG
                                                        COM-CHANGE-SUBJECT-PRONOUNS))))
  (SET-COMTAB-INDIRECTION *REPLY-COMTAB* MODE-COMTAB)
  (SETQ *OTHER-COMMAND-ALIST* (MAKE-COMMAND-ALIST '(COM-ZMAIL-VIEW-FILE COM-ZMAIL-WHOIS))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SELF-DOCUMENT
                                "ZWEI line self help"
                                (NO-ZMAIL-BUFFER-OK)
  (LET ((*COMTAB* *ZMAIL-COMTAB*))
    (COM-SELF-DOCUMENT)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-APROPOS
                                "List commands whose names contain a given string."
                                (NO-ZMAIL-BUFFER-OK)
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-SEARCH-STRINGS "Apropos. (Substring:)")
    (DOLIST (X *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)
      (LET ((COMMAND (CDR X)))
        (AND (FUNCALL FUNCTION KEY (COMMAND-NAME COMMAND))
             (DOCUMENT-ZMAIL-COMMAND COMMAND))))
    (FORMAT T "~%Done.~%"))
  DIS-NONE)

;; Copied from LAD: RELEASE-3.ZMAIL; COMNDS.LISP#592 on 2-Oct-86 04:02:25
(DEFVAR *COM-ZMAIL-DOCUMENTATION-ALIST*
        '((#/Z COM-ZMAIL-HELP)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DOCUMENTATION "Minimal self help" (NO-ZMAIL-BUFFER-OK)
  (SETF (COMTAB-EXTENDED-COMMANDS *ZMAIL-COMTAB*) *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*)
  (LET ((*COM-DOCUMENTATION-ALIST*
          (APPEND *COM-ZMAIL-DOCUMENTATION-ALIST*
                  (REMQ (ASSQ #/C *COM-DOCUMENTATION-ALIST*) *COM-DOCUMENTATION-ALIST*)))
        (*COMTAB* *ZMAIL-COMTAB*)
        (*EXTENDED-COMMAND-COMMAND* 'NOT-A-COMMAND)
        (*ANY-EXTENDED-COMMAND-COMMAND*
          'COM-ZMAIL-EXTENDED-COMMAND)
        (*COMMAND-ALIST* *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*))
    (COM-DOCUMENTATION)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-HELP "Minimal self help" (NO-ZMAIL-BUFFER-OK)
  (WITH-BACKGROUND-PROCESS-LOCKED
    (TYPEIN-LINE
      "~&Select command by typing character or mousing menu, or type /"*/" for all: ")
;    (TV:WINDOW-CALL (*TYPEOUT-WINDOW*)
    (LET ((CH (SEND *STANDARD-INPUT* :ANY-TYI))
          COMMAND)
      (IF (EQ CH #/*)
          (DOLIST (CMD (SETQ *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST*
                             (SORTCAR *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST* #'STRING-LESSP)))
            (DOCUMENT-ZMAIL-COMMAND (CDR CMD)))
        (COND ((NUMBERP CH)
               (COND ((SETQ COMMAND (COMMAND-LOOKUP CH *ZMAIL-COMTAB*))
                      (FORMAT T "~:C is " CH))
                     (T
                      (FORMAT T "~:C is not defined" CH))))
              ((AND (CONSP CH) (EQ (CAR CH) :MENU))
               (SET-COMMAND-BUTTON (THIRD CH))
               (SETQ COMMAND (CDADR CH))))
        (ZMAIL-PRINT-DOC COMMAND))))
  DIS-NONE)

(DEFUN ZMAIL-PRINT-DOC (COMMAND)
  (DO () ((NULL COMMAND))
    (FORMAT T "~A:~%" (COMMAND-NAME COMMAND))
    (PRINT-DOC :FULL COMMAND)
    (SETQ COMMAND (COND ((EQ COMMAND 'COM-ZMAIL-EXTENDED-COMMAND)
                         (CDR (COMPLETING-READ-FROM-MINI-BUFFER
                                "Command to document:"
                                *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST* NIL NIL
                                "You are typing the name of a ZMAIL command.")))
                        ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
                         (CASE COMMAND
                           (COM-ZMAIL-MAP
                            (ZMAIL-MENU-CHOOSE *ZMAIL-MAP-COMMAND-MENU*))
                           (COM-ZMAIL-OTHER-COMMANDS
                            (ZMAIL-MENU-CHOOSE 'ZMAIL-MOMENTARY-COMMAND-MENU
                                               *OTHER-COMMAND-ALIST*))
                           (OTHERWISE NIL)))))))

(DEFUN DOCUMENT-ZMAIL-COMMAND (COMMAND)
  (FORMAT T "~&~30,5,2A" (COMMAND-NAME COMMAND))
  (PRINT-DOC :SHORT COMMAND)
  (FORMAT T "~&")
  (AND (RASSQ COMMAND *ZMAIL-COMMAND-ALIST*)
       (FORMAT T "~&  which can be invoked from the main command menu~%"))
  (AND (RASSQ COMMAND *OTHER-COMMAND-ALIST*)
       (FORMAT T "~&  which can be invoked from the /"Other/" command menu~%"))
  (AND (RASSQ COMMAND *ZMAIL-MAP-COMMAND-ALIST*)
       (FORMAT T "~&  which can be invoked from the /"Map over/" command menu~%"))
  (AND (> (FIND-COMMAND-ON-KEYS COMMAND 4 "  which can be invoked via: "
                                *ZMAIL-COMTAB*)
          0)
       (TERPRI)))

;;; Recursive means updating a substring
(DEFUN UPDATE-COMMAND-WHO-LINE-DOCUMENTATION (COMMAND &OPTIONAL (TELL-WHO-LINE T) RECURSIVE
                                                      &AUX STRING FUNCTION)
  (let ((doc-string (GET COMMAND :WHO-LINE-DOCUMENTATION)))
    (IF (and (array-has-leader-p doc-string)
             (SETQ STRING doc-string))
        (SETF (ARRAY-LEADER STRING 0) 0)
      (progn (SETQ STRING (MAKE-EMPTY-STRING 95.))
             (PUTPROP COMMAND STRING :WHO-LINE-DOCUMENTATION)))
    (SETQ FUNCTION (GET COMMAND 'WHO-LINE-DOCUMENTATION-UPDATER))
    (IF RECURSIVE
        (FUNCALL FUNCTION STRING T)
      (FUNCALL FUNCTION STRING))
    (AND TELL-WHO-LINE
         (SEND TV:WHO-LINE-DOCUMENTATION-WINDOW :SET-WHO-LINE-ITEM-STATE NIL))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-EXTENDED-COMMAND
                                "Get at any top-level command"
                                (NO-ZMAIL-BUFFER-OK NUMERIC-ARG-OK)
  (LET ((ANS (WITH-BACKGROUND-PROCESS-LOCKED
               (COMPLETING-READ-FROM-MINI-BUFFER
                 (FORMAT NIL "Command:~:[  (Arg = ~A)~]"
                         (NOT *NUMERIC-ARG-P*)
                         (FORMAT-ARGUMENT *NUMERIC-ARG-P* *NUMERIC-ARG*))
                 *ZMAIL-TOP-LEVEL-COMMAND-NAME-ALIST* NIL NIL
                 "You are typing the name of a ZMAIL command."
                 #'(LAMBDA (X)
                     (LET ((*STANDARD-OUTPUT* *TYPEOUT-WINDOW*))
                       (PRINT-DOC :FULL (CDR X))))))))
    (COND ((EQUAL ANS "")
           (BEEP)
           DIS-NONE)
          (T
           (LET ((*CURRENT-COMMAND* (CDR ANS)))
             (FUNCALL *CURRENT-COMMAND*))))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-LARGE-ARGUMENT
                                "A large number for argument to command." (NO-ZMAIL-BUFFER-OK)
  (SETQ *NUMERIC-ARG* MOST-POSITIVE-FIXNUM
        *NUMERIC-ARG-P* :INFINITY)
  :ARGUMENT)

(DEFINE-ZMAIL-GLOBAL *MOVE-TO-NEXT-MENU-LAST-ITEM* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-NEXT (STRING)
  (FORMAT STRING "Move forward: L: ~A; M: ~A; R: menu."
          (NAME-FROM-MENU-VALUE :NEXT-UNDELETED *MOVE-TO-NEXT-MENU-ALIST*)
          (NAME-FROM-MENU-VALUE *NEXT-MIDDLE-MODE* *MOVE-TO-NEXT-MENU-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *NEXT-MIDDLE-MODE* COM-ZMAIL-NEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-NEXT "Move to next message.
Skips deleted messages.
Middle normally moves to the end, but is controlled by *NEXT-MIDDLE-MODE*.
Right for a menu." (NUMERIC-ARG-OK)
  (LET (MODE)
    (IF (MEMQ *ZMAIL-COMMAND-BUTTON* '(:KBD :LEFT))
        (SETQ MODE :NEXT-UNDELETED)
        (MULTIPLE-VALUE (MODE *MOVE-TO-NEXT-MENU-LAST-ITEM*)
          (ZMAIL-MENU-CHOOSE NIL *MOVE-TO-NEXT-MENU-ALIST* *MOVE-TO-NEXT-MENU-LAST-ITEM*
                             NIL *NEXT-MIDDLE-MODE*)))
    (COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL MODE)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DOWN-TO-NEXT
                                "Move to next message, including deleted messages."
                                (NUMERIC-ARG-OK)
  (COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL :NEXT))

(DEFINE-ZMAIL-GLOBAL *MOVE-TO-PREVIOUS-MENU-LAST-ITEM* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-PREVIOUS (STRING)
  (FORMAT STRING "Move backward: L: ~A; M: ~A; R: menu."
          (NAME-FROM-MENU-VALUE :PREVIOUS-UNDELETED *MOVE-TO-PREVIOUS-MENU-ALIST*)
          (NAME-FROM-MENU-VALUE *PREVIOUS-MIDDLE-MODE* *MOVE-TO-PREVIOUS-MENU-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *PREVIOUS-MIDDLE-MODE* COM-ZMAIL-PREVIOUS)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-PREVIOUS "Move to previous message.
Skips deleted messages.
Middle normally moves to the beginning, but is controlled by *PREVIOUS-MIDDLE-MODE*.
Right for a menu." (NUMERIC-ARG-OK)
  (LET (MODE)
    (IF (MEMQ *ZMAIL-COMMAND-BUTTON* '(:KBD :LEFT))
        (SETQ MODE :PREVIOUS-UNDELETED)
        (MULTIPLE-VALUE (MODE *MOVE-TO-PREVIOUS-MENU-LAST-ITEM*)
          (ZMAIL-MENU-CHOOSE NIL *MOVE-TO-PREVIOUS-MENU-ALIST*
                             *MOVE-TO-PREVIOUS-MENU-LAST-ITEM*
                             NIL *PREVIOUS-MIDDLE-MODE*)))
    (COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL MODE)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-UP-TO-PREVIOUS
                                "Move to previous message, including deleted messages."
                                (NUMERIC-ARG-OK)
  (COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL :PREVIOUS))

(DEFUN COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL (KEY)
  (LET ((UNDELETED-KEYWORDS
          '(:FIRST-UNDELETED :NEXT-UNDELETED :LAST-UNDELETED :PREVIOUS-UNDELETED))
        (FIRST-KEYWORDS '(:FIRST :FIRST-UNSEEN :FIRST-UNDELETED))
        (LAST-KEYWORDS '(:LAST :LAST-UNSEEN :LAST-UNDELETED))
        (FORWARD-KEYWORDS '(:NEXT :NEXT-UNDELETED :FIRST :FIRST-UNDELETED
                            :NEXT-UNSEEN :FIRST-UNSEEN))
        (UNSEEN-KEYWORDS '(:NEXT-UNSEEN :FIRST-UNSEEN :LAST-UNSEEN :PREVIOUS-UNSEEN)))
    (COND ((MEMQ KEY FIRST-KEYWORDS)
           (MSG-POINT-PDL-PUSH *MSG* *ZMAIL-BUFFER*)
           (SETQ *MSG-NO* -1))
          ((MEMQ KEY LAST-KEYWORDS)
           (MSG-POINT-PDL-PUSH *MSG* *ZMAIL-BUFFER*)
           (SETQ *MSG-NO* (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*))))
    (FUNCALL (IF (MEMQ KEY FORWARD-KEYWORDS)
                 'ZMAIL-SELECT-NEXT-MSG
               'ZMAIL-SELECT-PREVIOUS-MSG)
             (COND ((MEMQ KEY UNDELETED-KEYWORDS) T)
                   ((MEMQ KEY UNSEEN-KEYWORDS) :UNSEEN))
             (OR *NUMERIC-ARG-P*
                 (MEMQ KEY FIRST-KEYWORDS)
                 (MEMQ KEY LAST-KEYWORDS))
             *NUMERIC-ARG*)))

(DEFVAR *LAST-DELETED-MSG*)     ;Most recently deleted message, for the YANK command.

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-DELETE (STRING)
  (FORMAT STRING "Delete current message: L: ~A; M: ~A; R: menu."
          (NAME-FROM-MENU-VALUE *NEXT-AFTER-DELETE* *DELETE-DIRECTION-ALIST*)
          (NAME-FROM-MENU-VALUE *DELETE-MIDDLE-MODE* *DELETE-DIRECTION-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *NEXT-AFTER-DELETE* COM-ZMAIL-DELETE)
(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *DELETE-MIDDLE-MODE* COM-ZMAIL-DELETE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE "Delete current message.
Normally moves to the next message, but is controlled by *NEXT-AFTER-DELETE*.
Middle normally moves back after deleting, but is controlled by *DELETE-MIDDLE-MODE*.
Right gives a menu, including Remove.  With an argument, deletes the message with that
number." (NUMERIC-ARG-OK)
  (LET (MSG MODE)
    (IF *NUMERIC-ARG-P*
        (SETQ MSG (GET-MSG-FROM-ARG)
              MODE :NONE)
        (SETQ MSG *MSG*
              MODE (CHOOSE-DELETE-MODE)))
    (COM-ZMAIL-DELETE-INTERNAL MODE MSG)))

(DEFUN CHOOSE-DELETE-MODE ()
  (CASE *ZMAIL-COMMAND-BUTTON*
    (:MIDDLE *DELETE-MIDDLE-MODE*)
    (:RIGHT (OR (TV:MENU-CHOOSE
                  *DELETE-DIRECTION-ALIST* NIL
                  (RECTANGLE-NEAR-COMMAND-MENU TV:MOUSE-SHEET))
                (ABORT-CURRENT-COMMAND)))
    (OTHERWISE *NEXT-AFTER-DELETE*)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE-AND-UP
                                 "Delete the current message and move to previous undeleted"
                                 ()
  (COM-ZMAIL-DELETE-INTERNAL :BACKWARD))

(DEFUN COM-ZMAIL-DELETE-INTERNAL (MODE &OPTIONAL (MSG *MSG*))
  (IF (EQ MODE :REMOVE)
      (REMOVE-MSG *ZMAIL-BUFFER* *MSG* (LOCATE-MSG-IN-ZMAIL-BUFFER *MSG* *ZMAIL-BUFFER*))
    (MSG-PUT MSG T 'DELETED)
    (SETQ *LAST-DELETED-MSG* MSG)
    (MOVE-AFTER-DELETE MODE)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REMOVE
                                "Remove this message from the selected buffer.
Like deleting and expunging at once, but allowed only on temporary buffers."
                                ()
  (REMOVE-MSG *ZMAIL-BUFFER* *MSG* (LOCATE-MSG-IN-ZMAIL-BUFFER *MSG* *ZMAIL-BUFFER*)))

(DEFUN MOVE-AFTER-DELETE (MODE)
  (CASE MODE
    (:BACKWARD
     (ZMAIL-SELECT-PREVIOUS-MSG T T))
    (:FORWARD
     (ZMAIL-SELECT-NEXT-MSG T T))
    (OTHERWISE
     (ZMAIL-SELECT-MSG *MSG* NIL NIL))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-UNDELETE
                                 "Undelete this or the previous deleted message.
With numeric arg, undelete message with specified number."
                                 (NUMERIC-ARG-OK)
  (IF *NUMERIC-ARG-P*
      (LET ((MSG (GET-MSG-FROM-ARG)))
        (OR (MSG-GET MSG 'DELETED)
            (BARF "Message not deleted."))
        (ZMAIL-UNDELETE-MSG MSG))
      (LET ((ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*)))
        (DO ((N *MSG-NO* (1- N))
             (MSG))
            ((< N 0) (BARF "No preceding deleted messages."))
          (COND ((MSG-GET (SETQ MSG (AREF ARRAY N)) 'DELETED)
                 (MSG-PUT MSG NIL 'DELETED)
                 (RETURN (ZMAIL-SELECT-MSG N))))))))

(DEFUN REMOVE-MSG (ZMAIL-BUFFER MSG INDEX &AUX ARRAY LEN)
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (BARF "Cannot remove from a file buffer."))
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (OR (EQ MSG (AREF ARRAY INDEX))
      (ZMAIL-ERROR "~S not in ~Dth position of ~S" MSG INDEX ZMAIL-BUFFER))
  (MSG-POINT-PDL-PURGE MSG ZMAIL-BUFFER)
  (SETQ LEN (ARRAY-ACTIVE-LENGTH ARRAY))
  (COPY-ARRAY-PORTION ARRAY (1+ INDEX) LEN ARRAY INDEX (SETQ LEN (1- LEN)))
  (SETF (ARRAY-LEADER ARRAY 0) LEN)
  (COND ((EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
         (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
         (IF (ZEROP LEN)
             (ZMAIL-SELECT-MSG NIL T)
           (ZMAIL-SELECT-MSG (MIN INDEX (1- LEN)))))))

(DEFUN ZMAIL-SELECT-NEXT-MSG (&OPTIONAL NO-DELETED NO-ERROR-P (TIMES 1) &AUX ARRAY)
  "NO-DELETED is T for undeleted messages, :UNSEEN to allow only unseen messages."
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((N (1+ *MSG-NO*) (1+ N))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (NTIMES TIMES)
       (OK-IDX))
      (NIL)
    (AND ( N NMSGS)
         (OR (SEND *ZMAIL-BUFFER* :READ-NEXT-MSG)
             (IF (NOT NO-ERROR-P)
                 (BARF "Already at end")
               (OR OK-IDX (SETQ OK-IDX *MSG*))
               (RETURN (IF OK-IDX
                           (ZMAIL-SELECT-MSG OK-IDX)
                         (COMPUTE-CURRENT-MSG-NAME)
                         (SETQ *CURRENT-MSG-KEYWORDS-STRING* NIL)
                         DIS-NONE)))))
    (OR (AND NO-DELETED (MSG-GET (AREF ARRAY N) 'DELETED))
        (AND (EQ NO-DELETED :UNSEEN) (NOT (MSG-GET (AREF ARRAY N) 'UNSEEN)))
        (PLUSP (SETQ OK-IDX N
                     NTIMES (1- NTIMES)))
        (RETURN (ZMAIL-SELECT-MSG N NIL ( TIMES 1))))))

(DEFUN ZMAIL-SELECT-PREVIOUS-MSG (&OPTIONAL NO-DELETED NO-ERROR-P (TIMES 1) &AUX ARRAY)
  "NO-DELETED is T for undeleted messages, :UNSEEN to allow only unseen messages."
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((N (1- *MSG-NO*) (1- N))
       (NTIMES TIMES)
       (OK-IDX))
      ((< N 0) (IF NO-ERROR-P
                   (ZMAIL-SELECT-MSG (OR OK-IDX *MSG*))
                   (BARF "Already at beginning")))
    (OR (AND NO-DELETED (MSG-GET (AREF ARRAY N) 'DELETED))
        (AND (EQ NO-DELETED :UNSEEN) (NOT (MSG-GET (AREF ARRAY N) 'UNSEEN)))
        (PLUSP (SETQ OK-IDX N
                     NTIMES (1- NTIMES)))
        (RETURN (ZMAIL-SELECT-MSG N NIL ( TIMES 1))))))

(DEFVAR *EMPTY-MSG-INTERVAL* (CREATE-INTERVAL))

;;; Go to a specified message, specified either as the message itself or a number
(DEFUN ZMAIL-SELECT-MSG (MSG &OPTIONAL NO-ERROR-P (SAVE-POINT-P T)
                             &AUX (OLD-CURRENT-MSG *MSG*) INDEX ARRAY NMSGS)
  (IF (EQ MSG :NO-SELECT)
      DIS-NONE
      (IF (NULL *ZMAIL-BUFFER*)
          (SETQ *MSG-NO* -1
                *MSG* NIL)
          (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*)
                NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
          (COND ((NUMBERP MSG)
                 (SETQ *MSG-NO* MSG
                       *MSG* (AREF ARRAY *MSG-NO*)))
                ;; First try a hint from the summary window's display
                ((AND MSG
                      (SETQ INDEX (MSG-DISPLAYED-INDEX MSG))
                      (< INDEX NMSGS)
                      (EQ MSG (AREF ARRAY INDEX)))
                 (SETQ *MSG-NO* INDEX
                       *MSG* MSG))
                (T
                 (DO-NAMED FOO ((FLAG NIL T)) (NIL)
                   (DO ((I 0 (1+ I)))
                       (( I NMSGS)
                        (OR NO-ERROR-P
                            (ZMAIL-ERROR "Cannot find ~S in current buffer." MSG))
                        (COND ((OR FLAG
                                   (NULL (SETQ MSG (ZMAIL-BUFFER-SAVED-CURRENT-MSG
                                                     *ZMAIL-BUFFER*))))
                               (IF (PLUSP NMSGS)
                                   (SETQ *MSG-NO* 0     ;If not erring, choose one at random
                                         *MSG* (AREF ARRAY 0))
                                   (SETQ *MSG-NO* -1
                                         *MSG* NIL))
                               (RETURN-FROM FOO))))
                     (COND ((EQ MSG (AREF ARRAY I))
                            (SETQ *MSG-NO* I
                                  *MSG* MSG)
                            (RETURN-FROM FOO))))))))
      (SEND *SUMMARY-WINDOW* :SET-CURRENT-MSG *MSG*)
      (COMPUTE-CURRENT-MSG-NAME)
      (SETQ *CURRENT-MSG-KEYWORDS-STRING* (AND *MSG*
                                               (OR (MSG-GET *MSG* 'KEYWORDS-STRING) "{}")))
      (AND SAVE-POINT-P OLD-CURRENT-MSG
           (NEQ *MSG* OLD-CURRENT-MSG) *ZMAIL-BUFFER*
           (MSG-POINT-PDL-PUSH OLD-CURRENT-MSG *ZMAIL-BUFFER*))
      (COND (*MSG*
             (AND (MSG-GET *MSG* 'UNSEEN) (MSG-PUT *MSG* NIL 'UNSEEN))
             (SEND *MSG-WINDOW* :SET-INTERVAL (MSG-INTERVAL *MSG*))
             ;; FIx things up if parsing a message has caused these
             ;; bps to get relocated outside the user-visible part of the text.
             (LET ((BP (INTERVAL-FIRST-BP (MSG-INTERVAL *MSG*))))
               (MOVE-BP (WINDOW-START-BP *MSG-WINDOW*) BP)
               (MOVE-BP (WINDOW-POINT *MSG-WINDOW*) BP)
               (MOVE-BP (WINDOW-MARK *MSG-WINDOW*) BP)))
            (T
             (SEND *MSG-WINDOW* :SET-INTERVAL *EMPTY-MSG-INTERVAL*)))
      ;;This DIS-ALL is not right, but since there are so many bugs in REDISPLAY-BLT having
      ;;to do with virtual bounds, moving backwards would redisplay incorrectly otherwise.
      (IF (EQ *MSG* OLD-CURRENT-MSG) DIS-TEXT DIS-ALL)))

(DEFUN COMPUTE-CURRENT-MSG-NAME ()
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-EDIT-CURRENT-MSG)
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-KEYWORDS)
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-MOVE)
  (SETQ *CURRENT-MSG-NAME*
        (AND *ZMAIL-BUFFER*
             (IF *MSG*
                 (LET ((STRING (MAKE-EMPTY-STRING 40))
                       (STATUS (ASSURE-MSG-PARSED *MSG*))
                       (NMSGS (AND (NOT (AND (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
                                             (MEMQ (ZMAIL-DISK-BUFFER-STATUS *ZMAIL-BUFFER*)
                                                   '(:NEW-MAIL :LOADING :LOADING-NEW-MAIL))))
                                   (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*))))
                   (FORMAT STRING "  Msg #~D//~:[??~*~;~D~]" (1+ *MSG-NO*) NMSGS NMSGS)
                   (DO ((LIST *SAVED-INTERNAL-PROPERTIES-ALIST* (CDR LIST))
                        (FLAG NIL)
                        (KEY))
                       ((NULL LIST)
                        (AND FLAG (VECTOR-PUSH-EXTEND #/) STRING)))
                     (SETQ KEY (CDAR LIST))
                     (COND ((IF (EQ KEY 'LAST)
                                (AND NMSGS (= *MSG-NO* (1- NMSGS)))
                                (GET STATUS KEY))
                            (APPEND-TO-ARRAY STRING (IF FLAG ", " " ("))
                            (APPEND-TO-ARRAY STRING (CAAR LIST))
                            (SETQ FLAG T))))
                   STRING)
                 "    Empty buffer"))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SET-POP-MARK "Sets or pops the mark.
With no U's, sets the mark at the point, and pushes point onto the point pdl.
With one U, pops the point pdl.
With two U's, pops the point pdl and throws it away" (NUMERIC-ARG-OK)
  (COND (( *NUMERIC-ARG* 3)
         (MSG-POINT-PDL-PUSH *MSG* *ZMAIL-BUFFER*)
         DIS-NONE)
        (( *NUMERIC-ARG* 17.)
         (MSG-POINT-PDL-MOVE (MSG-POINT-PDL-POP)))
        (T
         (MSG-POINT-PDL-POP)
         DIS-NONE)))

(DEFUN MSG-POINT-PDL-PUSH (MSG ZMAIL-BUFFER &AUX START-BP)
  (COND ((NEQ MSG :NO-SELECT)
         (LET ((INT (WINDOW-INTERVAL *MSG-WINDOW*)))
           (AND (BP-= (INTERVAL-FIRST-BP INT) (MSG-START-BP MSG))
                (BP-= (INTERVAL-LAST-BP INT) (MSG-END-BP MSG))
                (SETQ START-BP (COPY-BP (WINDOW-START-BP *MSG-WINDOW*) :NORMAL))))
         (PUSH (LIST MSG ZMAIL-BUFFER START-BP) *MSG-POINT-PDL*)
         (AND (> (LENGTH *MSG-POINT-PDL*) *POINT-PDL-MAX*)
              (LET ((ENTRY (DELETE-LAST-ELEMENT *MSG-POINT-PDL*)))
                (AND ENTRY (THIRD ENTRY)
                     (FLUSH-BP (THIRD ENTRY))))))))

(DEFUN MSG-POINT-PDL-POP ()
  (OR *MSG-POINT-PDL* (BARF))
  (PROG1 (CAR *MSG-POINT-PDL*)
         (SETQ *MSG-POINT-PDL* (NCONC (CDR *MSG-POINT-PDL*) (RPLACD *MSG-POINT-PDL* NIL)))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* POINT-PDL-ELEMENT "Select"
                          MSG-POINT-PDL-MOVE T "Select this message.")

(DEFUN MSG-POINT-PDL-MOVE (ENTRY &AUX MSG ZMAIL-BUFFER START-BP)
  (SETF `(,MSG ,ZMAIL-BUFFER ,START-BP) ENTRY)
  (OR (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
      (SELECT-ZMAIL-BUFFER ZMAIL-BUFFER NIL NIL))
  (ZMAIL-SELECT-MSG MSG NIL NIL)
  (AND START-BP
       (RECENTER-WINDOW *MSG-WINDOW* :START START-BP))
  DIS-ALL)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOVE-TO-PREVIOUS-POINT
                                "Exchange point and top of point pdl.
A numeric argument rotates top arg entries of the point pdl (the default
numeric argument is 2).  An argument of 1 rotates the whole point pdl
and a negative argument rotates the other way." (NUMERIC-ARG-OK)
  (ROTATE-MSG-POINT-PDL (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 2)))

(DEFVAR *DEFAULT-PREVIOUS-MSG-POINT-ARG* 3)
(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOVE-TO-DEFAULT-PREVIOUS-POINT
                                "Rotate the point pdl.
A numeric argument specifies the number of entries to rotate, and sets the new default."
                                (NUMERIC-ARG-OK)
  (AND *NUMERIC-ARG-P*
       (SETQ *DEFAULT-PREVIOUS-MSG-POINT-ARG* *NUMERIC-ARG*))
  (ROTATE-MSG-POINT-PDL *DEFAULT-PREVIOUS-MSG-POINT-ARG*))

(DEFUN ROTATE-MSG-POINT-PDL (N &AUX ENTRY LIST)
  (SETQ ENTRY (LIST *MSG* *ZMAIL-BUFFER* (COPY-BP (WINDOW-START-BP *MSG-WINDOW*) :NORMAL))
        LIST (CONS ENTRY *MSG-POINT-PDL*))
  (ROTATE-TOP-OF-LIST LIST N)
  (SETQ ENTRY (CAR LIST))
  (MSG-POINT-PDL-MOVE ENTRY)
  DIS-BPS)

(DEFUN MSG-POINT-PDL-PURGE (*MSG* *ZMAIL-BUFFER*)
  (SETQ *MSG-POINT-PDL* (DEL-IF #'(LAMBDA (X)
                                    (AND (OR (NULL *MSG*)
                                             (EQ (FIRST X) *MSG*))
                                         (OR (NULL *ZMAIL-BUFFER*)
                                             (EQ (SECOND X) *ZMAIL-BUFFER*))))
                                *MSG-POINT-PDL*)))

(DEFUN MSG-POINT-PDL-FORWARD-ZMAIL-BUFFER (FROM TO)
  (DOLIST (ELEM *MSG-POINT-PDL*)
    (COND ((EQ (SECOND ELEM) FROM)
           (SETF (SECOND ELEM) TO)
           (AND (BP-< (THIRD ELEM) (MSG-START-BP (FIRST ELEM)))
                (MOVE-BP (THIRD ELEM) (MSG-START-BP (FIRST ELEM))))))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOUSE-POINT-PDL "Give menu of message point pdl" ()
  (LET (*TYPEOUT-WINDOW* (N 0))
    (IF (TV:SHEET-EXPOSED-P *SUMMARY-WINDOW*)
        (SETQ *TYPEOUT-WINDOW* (SEND *SUMMARY-WINDOW* :TYPEOUT-WINDOW))
        (SETQ *TYPEOUT-WINDOW* (WINDOW-TYPEOUT-WINDOW *WINDOW*))
        (SEND *TYPEOUT-WINDOW* :LINE-OUT *SUMMARY-WINDOW-LABEL*))
    (DOLIST (ELEM *MSG-POINT-PDL*)
      (LET* ((MSG (CAR ELEM))
             (STATUS (ASSURE-MSG-PARSED MSG)))
        (SEND *TYPEOUT-WINDOW* :TRUNCATED-ITEM 'POINT-PDL-ELEMENT ELEM "~~3D~C~A~"
              (EQ MSG *MSG*) (INCF N) (STATUS-LETTER STATUS) (MSG-SUMMARY-LINE MSG))
        (SEND *TYPEOUT-WINDOW* :TYO #/CR)))
    (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* MSG-LINE "Select" SELECT-MSG-FROM-LINE T
                          "Select the message containing this line.")

;;; Select the message as pointed to by the typeout window
(DEFUN SELECT-MSG-FROM-LINE (MSG-LINE &AUX ARRAY)
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (MSG))
      (( I NMSGS)
       (ZMAIL-ERROR "Cannot find /"~A/" in ~S" MSG-LINE *ZMAIL-BUFFER*))
    (SETQ MSG (AREF ARRAY I))
    (AND (DO ((LINE (BP-LINE (MSG-START-BP MSG)) (LINE-NEXT LINE))
              (END (BP-LINE (MSG-END-BP MSG))))
             ((EQ LINE END) NIL)
           (AND (EQ LINE MSG-LINE) (RETURN T)))
         (RETURN (ZMAIL-SELECT-MSG MSG)))))

;;; Select a msg, possibly changing to its primary mail file
(DEFUN SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER (MSG &AUX I)
  (IF (SETQ I (MSG-IN-ZMAIL-BUFFER-P MSG *ZMAIL-BUFFER*))
      (ZMAIL-SELECT-MSG I)
      (SELECT-ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG) NIL NIL)
      (ZMAIL-SELECT-MSG MSG NIL NIL)))

(DEFUN ABORT-CURRENT-COMMAND ()
  (*THROW 'ZWEI-COMMAND-LOOP T))

(DEFVAR *ZMAIL-QUIT-MENU-ALIST*
        '((("Don't Save" :VALUE :NOSAVE :DOCUMENTATION "Don't save out any files.")
           ("Ask" :VALUE :ASK :DOCUMENTATION "Give a menu for saving of files.")
           ("Save" :VALUE :SAVE :DOCUMENTATION "Save out all changed files."))
          (("Quit" :VALUE :QUIT :DOCUMENTATION "Select calling window.")
           ("Logout" :VALUE :LOGOUT :DOCUMENTATION "Logout when done writing."))))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-ZMAIL-QUIT
  "Save and exit:  L: save all; R: menu for Save mode // Logout.")

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-QUIT "Save and exit.
Expunge deleted messages, write out changes and exit.
Right gives menu of options." (NO-ZMAIL-BUFFER-OK)
  (LET ((SAVE-MODE :SAVE)
        (LOGOUT-MODE :QUIT))
    (AND (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
         (MULTIPLE-VALUE (SAVE-MODE LOGOUT-MODE)
           (DEFAULTED-MULTIPLE-MENU-CHOOSE-NEAR-MENU *ZMAIL-QUIT-MENU-ALIST*
                                                     SAVE-MODE LOGOUT-MODE)))
    (CASE SAVE-MODE
      (:NOSAVE)
      (:SAVE (ZMAIL-SAVE-ALL))
      (:ASK (ZMAIL-SAVE-MENU)))
    (TV:DESELECT-AND-MAYBE-BURY-WINDOW *ZMAIL-WINDOW*)
    (CASE LOGOUT-MODE
      (:QUIT )
      (:LOGOUT (LOGOUT)
               (TV:SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'TV:LISTENER-MIXIN 'TV:LISP-LISTENER))))
  DIS-TEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-NOT-MODIFIED
                                "Mark this buffer as not needing saving"
                                (NO-MSG-OK)
  (SEND *ZMAIL-BUFFER* :NOT-MODIFIED)
  (FORMAT *QUERY-IO* "~&Not modified")
  DIS-NONE)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-ZMAIL-SAVE
"L: Expunge & save all buffers; M: Expunge this buffer; R: Expunge // Save // Kill menu.")

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SAVE  "Expunge and write out all files.
Right gives a menu for Expunge, Save or Kill for each file." (NO-MSG-OK)
  (CASE *ZMAIL-COMMAND-BUTTON*
    ((:KBD :LEFT)
     (ZMAIL-SAVE-ALL))
    (:MIDDLE
     (COM-ZMAIL-EXPUNGE))
    (:RIGHT
     (ZMAIL-SAVE-MENU)))
  DIS-TEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-KILL-CURRENT-BUFFER "Kill the current buffer."
                                (NO-MSG-OK)
  (AND (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
       (ZMAIL-BUFFER-SAVE-P *ZMAIL-BUFFER*)
       (FQUERY NIL "Save mail file ~A before killing ? " (ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*))
       (SAVE-ZMAIL-BUFFER *ZMAIL-BUFFER*))
  (KILL-ZMAIL-BUFFER *ZMAIL-BUFFER*)
  DIS-TEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-EXPUNGE "Expunge the current buffer."
                                (NO-MSG-OK)
  (EXPUNGE-ZMAIL-BUFFER *ZMAIL-BUFFER*)
  (AND (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
       (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
         (AND (NOT (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER))
              (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER NIL))))
  DIS-TEXT)

(DEFUN ZMAIL-SAVE-ALL ()
  "Expunge all buffers, and save all file buffers that have unsaved changes."
  ;; Expunge them all first, since this can ask questions.
  (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
    (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER))
  (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
    (WHEN (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
      (SAVE-ZMAIL-BUFFER ZMAIL-BUFFER)))
  (FORMAT *QUERY-IO* "~&Saving done."))

(DEFUN ZMAIL-SAVE-ALL-FILES ()
  "Offer to save each ZMAIL file buffer that has unsaved changes."
  (let ((window tv:selected-window))
    (SEND *ZMAIL-WINDOW* :FUNCALL-INSIDE-YOURSELF
          #'(LAMBDA (&AUX (*QUERY-IO* *TERMINAL-IO*))
              (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
                (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
                     (ZMAIL-BUFFER-SAVE-P ZMAIL-BUFFER)
                     (FQUERY `(:stream ,window) "Save mail file ~A ? "
                             (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))
                     (SAVE-ZMAIL-BUFFER ZMAIL-BUFFER)))))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* ZMAIL-BUFFER "Expunge & Save"
                          EXPUNGE-AND-SAVE-ZMAIL-BUFFER NIL
                          "Expunge and then save this buffer.")

(DEFUN EXPUNGE-AND-SAVE-ZMAIL-BUFFER (ZMAIL-BUFFER)
  "Expunge ZMAIL-BUFFER, and save it if it is a file buffer with unsaved changes."
  (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER)
  (WHEN (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
    (SAVE-ZMAIL-BUFFER ZMAIL-BUFFER)
    T))

(DEFUN ZMAIL-SAVE-MENU (&AUX CHOICES EXPUNGED-SOME)
  "Expunge, save or kill ZMAIL buffers, asking the user with a menu which ones."
  (SETQ CHOICES (DO ((L *ZMAIL-BUFFER-LIST* (CDR L))
                     (ZMAIL-BUFFER)
                     (NL NIL)
                     (SAVE-P) (EXPUNGE-P))
                    ((NULL L) (NREVERSE NL))
                  (SETQ ZMAIL-BUFFER (CAR L))
                  (MULTIPLE-VALUE (SAVE-P EXPUNGE-P)
                    (ZMAIL-BUFFER-SAVE-P ZMAIL-BUFFER))
                  (PUSH `(,ZMAIL-BUFFER ,(ZMAIL-BUFFER-NAME ZMAIL-BUFFER)
                          ,(IF (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
                               `((:EXPUNGE ,EXPUNGE-P) (:SAVE ,(OR SAVE-P EXPUNGE-P)) :KILL)
                               `((:EXPUNGE ,EXPUNGE-P) :KILL)))
                        NL)))
  (SETQ CHOICES (TV:MULTIPLE-CHOOSE "Mail file"
                                    (SORT CHOICES
                                          #'(LAMBDA (X Y)
                                              (STRING-LESSP (CADR X) (CADR Y))))
                                    '((:EXPUNGE "Expunge" NIL NIL NIL NIL)
                                      (:SAVE "Save" NIL NIL NIL NIL)
                                      (:KILL "Kill" NIL NIL NIL NIL))
                                    '(:MOUSE) 50.))
  (DOLIST (CHOICE CHOICES)
    (LET ((ZMAIL-BUFFER (CAR CHOICE))
          (KEYWORDS (CDR CHOICE)))
      ;; Make sure things are done in the right order
      (COND ((MEMQ :EXPUNGE KEYWORDS)
             (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER)
             (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
                  (SETQ EXPUNGED-SOME T))))
      (AND (MEMQ :SAVE KEYWORDS)
           (SAVE-ZMAIL-BUFFER ZMAIL-BUFFER))
      (AND (MEMQ :KILL KEYWORDS)
           (KILL-ZMAIL-BUFFER ZMAIL-BUFFER))))
  ;;If some file buffers were expunged, expunge any temporary ones that weren't
  (DOLIST (CHOICE CHOICES)
    (LET ((ZMAIL-BUFFER (CAR CHOICE)))
      (AND (NOT (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER))
           (NOT (MEMQ :EXPUNGE (CDR CHOICE)))
           (EXPUNGE-ZMAIL-BUFFER ZMAIL-BUFFER NIL)))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* ZMAIL-BUFFER "Expunge" EXPUNGE-ZMAIL-BUFFER
                          NIL "Expunge this buffer.")

(DEFUN EXPUNGE-ZMAIL-BUFFER (ZMAIL-BUFFER &OPTIONAL (DELETED-MSGS T)
                                    &AUX ARRAY INFS (*INTERVAL* *INTERVAL*))
  "Expunge ZMAIL-BUFFER, removing all messages marked as deleted.
If DELETED-MSGS is NIL, only remove messages that have been removed from their owning buffers.
After you expunge a file buffer, you should expunge all temporary buffers
 with DELETED-MSGS = NIL, but this function does not do that for you."
  (COND ((ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
         (SETQ *INTERVAL* (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER))
         (SETQ INFS (LOCF (NODE-INFERIORS *INTERVAL*)))
         (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER)
         (ZMAIL-BUFFER-DELETE-EXPIRED ZMAIL-BUFFER *DELETE-EXPIRED-MSGS*)
         (AND *QUERY-BEFORE-EXPUNGE*
              (ZMAIL-BUFFER-EXPUNGE-QUERY ZMAIL-BUFFER))))
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (DO ((NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (I 0 (1+ I))
       (MSG))
      (( I NMSGS)
       (COND ((AND (PLUSP NMSGS) (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER))
              ;; Kludge for babyl, make the last line of the file correct maybe.
              (SEND ZMAIL-BUFFER :UPDATE-MSG-END (AREF ARRAY (1- NMSGS))))
             ((AND (ZEROP NMSGS) (NOT (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)))
              ;; A temporary buffer goes away when it becomes empty.
              (SEND ZMAIL-BUFFER :KILL))))
    ;; For speed, this does not call MSG-GET, which would parse the
    ;; message if need be.  If the message has never been parsed, it
    ;; probably isn't deleted.  Likewise, below it cannot be recent.
    (COND ((OR (EQ (MSG-PARSED-P (SETQ MSG (AREF ARRAY I))) :KILLED)
               (AND DELETED-MSGS (GET (LOCF (MSG-STATUS MSG)) 'DELETED)))
           (MSG-POINT-PDL-PURGE MSG (AND (NOT (EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG)))
                                         ZMAIL-BUFFER))
           (IF (EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG))
               (LET ((REAL-INT (MSG-REAL-INTERVAL MSG)))
                 (UNLESS (EQ REAL-INT (CADR INFS))
                   (ZMAIL-ERROR "Node inferiors messed up, please report to BUG-ZMAIL"))
                 (DELETE-INTERVAL REAL-INT)
                 (UNCACHE-MSG-REFERENCES MSG)
                 ;; Now start really killing it.
                 (WITHOUT-INTERRUPTS
                   (RPLACD INFS (CDDR INFS))
                   (SETF (MSG-PARSED-P MSG) :KILLED)
                   (UNLESS (= (1+ I) NMSGS)
                     (%BLT (ALOC ARRAY (1+ I)) (ALOC ARRAY I)
                           (- NMSGS I 1) 1))
                   (DECF (FILL-POINTER ARRAY))
                   (DECF NMSGS)
                   (DECF I))    ;Compensate for increment that DO-step will do.
                 ;; It's no disaster if these don't get done due to an abort.
                 (FLUSH-BP (INTERVAL-FIRST-BP REAL-INT))
                 (FLUSH-BP (INTERVAL-LAST-BP REAL-INT))
                 (LET ((INT (MSG-INTERVAL MSG)))
                   (FLUSH-BP (INTERVAL-FIRST-BP INT))
                   (FLUSH-BP (INTERVAL-LAST-BP INT))))
             ;; Just remove the message from our array.
             (WITHOUT-INTERRUPTS
               (UNLESS (= (1+ I) NMSGS)
                 (%BLT (ALOC ARRAY (1+ I)) (ALOC ARRAY I)
                       (- NMSGS I 1) 1))
               (DECF (FILL-POINTER ARRAY))
               (DECF NMSGS)
               (DECF I))))
          (T
           (COND ((EQ ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG))
                  (AND (REMPROP (LOCF (MSG-STATUS MSG)) 'RECENT)
                       (SETF (MSG-TICK MSG) (TICK)))
                  (SETQ INFS (CDR INFS)))))))
  (COND ((EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
         (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
         (ZMAIL-SELECT-MSG (COND ((ZEROP (ARRAY-ACTIVE-LENGTH ARRAY)) NIL)
                                 ((OR (EQ (MSG-PARSED-P *MSG*) :KILLED)
                                      (MSG-GET *MSG* 'DELETED))
                                  (do* ((msg-no *msg-no* (1- msg-no))
                                        (msg *msg* (aref array msg-no)))
                                       ((zerop msg-no)
                                        ;;in this case, all msgs before *msg*
                                        ;;were deleted prior to the expunge,
                                        ;;next clause searches forward thru msgs.
                                        nil)
                                    (when (not (OR (EQ (MSG-PARSED-P MSG) :KILLED)
                                                   (MSG-GET MSG 'DELETED)))
                                      (return msg))))
                                 ;;note slightly differing logic here; from first
                                 ;;clause we know that there are msgs, and from
                                 ;;second clause we know that there are no previous
                                 ;;msgs; therefore there must be future msgs...
                                 (t (do* ((msg-no (1+ *msg-no*) (1+ msg-no))
                                          (msg (aref array msg-no) (aref array msg-no)))
                                       ((not (OR (EQ (MSG-PARSED-P *MSG*) :KILLED)
                                                 (MSG-GET *MSG* 'DELETED)))
                                        *msg*))))
                           T NIL))))

(DEFUN ZMAIL-BUFFER-EXPUNGE-QUERY (ZMAIL-BUFFER)
  "Ask the user whether to expunge ZMAIL-BUFFER, and throw to the command loop if No."
  (AND (NOT (ZMAIL-BUFFER-PREDICATE-QUERY ZMAIL-BUFFER
                                       #'(LAMBDA (MSG)
                                           (GET (LOCF (MSG-STATUS MSG)) 'DELETED))
                                       "expunge" "Deleted"))
       (ABORT-CURRENT-COMMAND)))

(DEFUN ZMAIL-BUFFER-PREDICATE-QUERY (ZMAIL-BUFFER PREDICATE ACTION ATTRIBUTE &AUX ARRAY)
  "Ask the user whether to perform ACTION on some messages in ZMAIL-BUFFER; return T or NIL.
PREDICATE is a function used to select the messages we ask about.
We offer to print a list those messages.
ACTION is a lower case word saying what the operation is.
ATTRIBUTE is a capitalized word describing messages that satisfy the predicate."
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (MSG)
       (FLAG 0))
      (( I NMSGS)
       (OR (ZEROP FLAG)
           (LET ((TV:KBD-TYI-HOOK
                   #'(LAMBDA (CH)
                       (AND (CONSP CH) (EQ (CAR CH) :TYPEOUT-EXECUTE)
                            (*THROW 'RETURN-TO-COMMAND-LOOP NIL))
                       NIL)))
             ;; The KBD-TYI-HOOK arranges that if one of the items is clicked on
             ;; we throw to the command loop so it can interpret the click.
             (TYPEOUT-YES-OR-NO-P "Ok to ~A ~:[it~;them~]? " ACTION (> FLAG 1)))))
    (COND ((FUNCALL PREDICATE (SETQ MSG (AREF ARRAY I)))
           (AND (ZEROP FLAG)
                (FORMAT *TYPEOUT-WINDOW* "~&~A messages to be ~Ad from ~A:~%"
                        ATTRIBUTE ACTION (ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))
           (INCF FLAG)
           (SEND *TYPEOUT-WINDOW* :TRUNCATED-ITEM 'SUMMARY-LINE MSG "~~3D~C~A~"
                 (+ (msg-displayed-index msg) 1) ; because this is zero based
                 (STATUS-LETTER (ASSURE-MSG-PARSED MSG)) (MSG-SUMMARY-LINE MSG))
           (SEND *TYPEOUT-WINDOW* :TYO #/CR)))))

(LOCAL-DECLARE ((SPECIAL *NOW*))
(DEFUN ZMAIL-BUFFER-DELETE-EXPIRED (ZMAIL-BUFFER WHEN &AUX (*NOW* (TIME:GET-UNIVERSAL-TIME)))
  "Maybe mark as deleted all expired messages in ZMAIL-BUFFER, according to WHEN.
WHEN can be T, meaning do delete them, or NIL, meaning do nothing,
 or :ASK, meaning query the user, or :PER-FILE, which means do delete
 if the ZMAIL-BUFFER has the :DELETE-EXPIRED option."
  (AND (EQ WHEN :PER-FILE)
       (SETQ WHEN (GET (LOCF (ZMAIL-BUFFER-OPTIONS ZMAIL-BUFFER)) :DELETE-EXPIRED)))
  (AND (OR (EQ WHEN T)
           (AND (EQ WHEN :ASK)
                (ZMAIL-BUFFER-PREDICATE-QUERY ZMAIL-BUFFER #'MSG-EXPIRED-P "delete" "Expired")))
       (DOMSGS (MSG ZMAIL-BUFFER)
         (AND (MSG-EXPIRED-P MSG)
              (MSG-PUT MSG T 'DELETED)))))

(DEFUN MSG-EXPIRED-P (MSG &AUX STATUS EXPIRATION-DATE)
  "T if MSG has an expiration date and it is passed."
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (AND (NOT (GET STATUS 'DELETED))
       (SETQ EXPIRATION-DATE (GET STATUS :EXPIRATION-DATE))
       (> *NOW* EXPIRATION-DATE)))
)

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* ZMAIL-BUFFER "Save" SAVE-ZMAIL-BUFFER
                          NIL "Save this buffer.")

(DEFUN SAVE-ZMAIL-BUFFER (ZMAIL-BUFFER)
  "Save ZMAIL-BUFFER, in the foreground, if it has unsaved changes."
  (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER)
  (COND ((ZMAIL-BUFFER-SAVE-P ZMAIL-BUFFER)
         (LET ((OPTIONS (LOCF (ZMAIL-BUFFER-OPTIONS ZMAIL-BUFFER))))
           (LET ((OWNER (GET OPTIONS :OWNER)))
             (AND OWNER (NOT (STRING-EQUAL OWNER USER-ID))
                  (OR (FQUERY '(:SELECT T
                                :BEEP T)
                              "The file ~A is owned by ~A.  Save it anyway? "
                              (ZMAIL-BUFFER-NAME ZMAIL-BUFFER) OWNER)
                      (ABORT-CURRENT-COMMAND)))))
         (UNWIND-PROTECT
           (PROGN
             (ZMAIL-BUFFER-SAVE-SETUP ZMAIL-BUFFER)
             (STREAM-OUT-INTERVAL (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER)
                                  (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER))
             (SEND ZMAIL-BUFFER :SAVING-DONE))
           ;; If we have not finished the save, we must have aborted it.
           (AND (EQ (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) :SAVING)
                (SEND ZMAIL-BUFFER :SAVING-ABORTED))))
        (T
         (FORMAT *QUERY-IO* "~&No changes need to be written in ~A"
                 (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))))
  DIS-NONE)

;;; Does this file need saving out?  Works by updating the start of each message that
;;; has changed and seeing if that mungs the buffer's interval.
(DEFUN ZMAIL-BUFFER-SAVE-P (ZMAIL-BUFFER &AUX SAVE-P EXPUNGE-P)
  "T if ZMAIL-BUFFER has unsaved changes.
Second value is T if it has any deleted messages."
  (DECLARE (RETURN-LIST SAVE-P EXPUNGE-P))
  (DOMSGS (MSG ZMAIL-BUFFER)
    (SEND ZMAIL-BUFFER :UPDATE-MSG-OPTIONS-IN-FILE-IF-NECESSARY MSG)
    (AND (GET (LOCF (MSG-STATUS MSG)) 'DELETED)
         (SETQ EXPUNGE-P T)))
  (COND ((ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
         (SETF (ZMAIL-DISK-BUFFER-MSG-UPDATE-TICK ZMAIL-BUFFER) (TICK))
         ;; This may require that the file be saved out.
         (SEND ZMAIL-BUFFER :UPDATE-OPTIONS-IN-FILE)
         (AND (OR (EQ (BUFFER-FILE-ID ZMAIL-BUFFER) T)
                   (> (NODE-TICK (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER))
                      (BUFFER-TICK ZMAIL-BUFFER)))
              (SETQ SAVE-P T))))
  (VALUES SAVE-P EXPUNGE-P))

;;; This forces the rest of the file to be read in immediately, aborts any saving in
;;; progress, etc.  ABORT-P means abort the saving rather than finishing it, as
;;; another one is about to be begun.
(DEFUN FOREGROUND-BACKGROUND-FINISH (ZMAIL-BUFFER &OPTIONAL (ABORT-P T) STATUS)
  (IF (TYPEP ZMAIL-BUFFER 'INBOX-BUFFER)
      (SETQ ZMAIL-BUFFER (SEND ZMAIL-BUFFER :ASSOCIATED-MAIL-FILE-BUFFER)))
  (LOCK-ZMAIL-BUFFER (ZMAIL-BUFFER)
    (CASE (SETQ STATUS (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER))
      ((:LOADING :SAVING-REQUIRED :AWAITING-NEW-MAIL)
       (LET ((*ZMAIL-BACKGROUND-P* :DISABLE))
         (ASSURE-ZMAIL-BUFFER-FULLY-LOADED ZMAIL-BUFFER))
       (COND (ABORT-P
              (SETQ STATUS :SAVING-REQUIRED))
             (T
              (ZMAIL-BUFFER-SAVE-SETUP ZMAIL-BUFFER)
              (STREAM-OUT-INTERVAL (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER)
                                   (ZMAIL-DISK-BUFFER-INTERVAL ZMAIL-BUFFER))
              (SEND ZMAIL-BUFFER :SAVING-DONE T)
              (SETQ STATUS NIL))))
      (:SAVING
       (IF ABORT-P
           (SEND (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER) :CLOSE :ABORT)
         (LET ((INTERVAL-STREAM (DOLIST (REQUEST (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*))
                                  (AND (EQ (CAR REQUEST) 'ZMAIL-BACKGROUND-SAVE-FILE)
                                       (EQ (CADR REQUEST) ZMAIL-BUFFER)
                                       (RETURN (THIRD REQUEST))))))
           (COND (INTERVAL-STREAM
                  (STREAM-COPY-UNTIL-EOF INTERVAL-STREAM (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER))
                  (SEND ZMAIL-BUFFER :SAVING-DONE)))))
       (SETQ STATUS NIL)))
    (SETF (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) STATUS)
    (WITHOUT-INTERRUPTS
      (WHEN (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*)
        (SETF (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*)
              (LET ((*ZMAIL-BUFFER* ZMAIL-BUFFER)
                    (*INBOX-BUFFER* (AND (TYPEP ZMAIL-BUFFER 'MAIL-FILE-BUFFER)
                                         (SEND ZMAIL-BUFFER :ASSOCIATED-INBOX-BUFFER))))
                (DECLARE (SPECIAL *INBOX-BUFFER*))
                (DEL-IF #'(LAMBDA (X)
                            (AND (MEMQ (CAR X)
                                       '(ZMAIL-BACKGROUND-SAVE-FILE ZMAIL-BACKGROUND-LOAD-FILE))
                                 (OR (EQ (CADR X) *ZMAIL-BUFFER*)
                                     (EQ (CADR X) *INBOX-BUFFER*))))
                        (CAR *ZMAIL-BACKGROUND-REQUEST-CELL*))))))))

(DEFUN ASSURE-ZMAIL-BUFFER-FULLY-LOADED (ZMAIL-BUFFER)
  "Read in the rest of ZMAIL-BUFFER if not already finished.
Do the same thing for ZMAIL-BUFFER's associated inbox if it has one.
 /(This will result in merging the new mail.)"
  (LOCK-ZMAIL-BUFFER (ZMAIL-BUFFER)
    (WHEN (EQ (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) :LOADING)
      (LOAD-ALL-MSGS ZMAIL-BUFFER))
    (WHEN (TYPEP ZMAIL-BUFFER 'MAIL-FILE-BUFFER)
      (LET ((INBOX (SEND ZMAIL-BUFFER :ASSOCIATED-INBOX-BUFFER)))
        (WHEN (AND INBOX (EQ (ZMAIL-DISK-BUFFER-STATUS INBOX) :LOADING-NEW-MAIL))
          (LOAD-ALL-MSGS INBOX))))))

(DEFUN ZMAIL-BUFFER-SAVE-SETUP (ZMAIL-BUFFER)
  (SETF (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER)
        (OPEN (BUFFER-PATHNAME ZMAIL-BUFFER) '(:OUT)))
  (SETF (ZMAIL-DISK-BUFFER-STATUS ZMAIL-BUFFER) :SAVING))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* ZMAIL-BUFFER "Kill" KILL-ZMAIL-BUFFER NIL
                          "Kill this buffer.")

(DEFUN KILL-ZMAIL-BUFFER (ZMAIL-BUFFER)
  (SEND ZMAIL-BUFFER :KILL)
  DIS-NONE)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-EDIT-CURRENT-MSG *EDIT-MSG-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-EDIT-CURRENT-MSG (STRING)
  (AND *MSG*
       (APPEND-TO-ARRAY STRING "L: edit this message.")))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-EDIT-CURRENT-MSG "Edit the current message" ()
;  (MAKE-WINDOW-CURRENT *MSG-WINDOW*)
;  (SELECT-WINDOW *MSG-WINDOW*)
  (MUST-REDISPLAY *MSG-WINDOW* DIS-BPS)
  (TV:WITH-SELECTION-SUBSTITUTE (*MSG-WINDOW* *ZMAIL-WINDOW*)
    (UNWIND-PROTECT
        (PROGN
          (LOCK-BACKGROUND-PROCESS)
          (SETF (NODE-UNDO-STATUS *ZMAIL-BUFFER*)
                (NODE-UNDO-STATUS (MSG-INTERVAL *MSG*)))
          (LET ((*COMTAB* *MSG-COMTAB*)
                (*MODE-LINE-LIST* '("ZMail " "Editing message " "(" *MODE-NAME-LIST*
                                    ") " *ZMAIL-FILE-NAME* *CURRENT-MSG-NAME*)))
            (SEND *MSG-WINDOW* :EDIT)
            DIS-NONE))
      ;; Keep undo info on a per-message basis;
      ;; however, during editing, the undo info must
      ;; be on the top-level node.
      (SETF (NODE-UNDO-STATUS (MSG-INTERVAL *MSG*))
            (NODE-UNDO-STATUS *ZMAIL-BUFFER*))
      (SETF (NODE-UNDO-STATUS *ZMAIL-BUFFER*) :DONT)
      ;;Make sure the message is still separated ok
      (LET* ((*INTERVAL* (WINDOW-INTERVAL *MSG-WINDOW*))
             (BP (INTERVAL-LAST-BP *INTERVAL*)))
        (OR (BEG-LINE-P BP) (INSERT BP #/CR))
        (SEND (MSG-MAIL-FILE-BUFFER *MSG*) :UPDATE-MSG-END *MSG*))
      (SETF (MSG-TICK *MSG*) (TICK))            ;Munge message
      (UNCACHE-MSG-REFERENCES *MSG*)
      (SETF (MSG-STATUS *MSG*) (SOME-PLIST (MSG-STATUS *MSG*) *INTERNAL-TYPE-PROPERTIES*))
      (SET-PARSED-MSG-HEADERS *MSG*)
      (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG *MSG*)
      (SETF (WINDOW-MARK-P *MSG-WINDOW*) NIL)
      (LET ((BP (COPY-BP (WINDOW-POINT *MSG-WINDOW*))))
        (ZMAIL-SELECT-MSG *MSG*)                        ;May not have losing headers any more, say
        (MOVE-BP (WINDOW-POINT *MSG-WINDOW*) BP))
      (PROCESS-UNLOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*))))

(defun cleanup-message-window ()
  ;; in case user exited via the mouse while a region was highlighted...
  (setf (window-mark-p *window*) nil)
  (send *window* :redisplay dis-all nil nil nil))

(DEFCOM COM-QUIT-ZMAIL-EDIT "Exit editing the message." ()
  (progn (cleanup-message-window)
         (*THROW 'RETURN-FROM-COMMAND-LOOP T)))

;;; Keyword stuff

;;; List of keywords on, keywords off
(DEFINE-ZMAIL-GLOBAL *DEFAULT-KEYWORDS* NIL)

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *FILTER-KEYWORDS-ALIST* COM-ZMAIL-KEYWORDS)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-ZMAIL-KEYWORDS *KEYWORDS-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-KEYWORDS (STRING)
  (APPEND-TO-ARRAY STRING "Change keywords on this message: ")
  (COND (*DEFAULT-KEYWORDS*
         (APPEND-TO-ARRAY STRING "L:")
         (LET ((ON (CAR *DEFAULT-KEYWORDS*))
               (OFF (CADR *DEFAULT-KEYWORDS*)))
           (COND (ON
                  (APPEND-TO-ARRAY STRING " add ")
                  (APPEND-TO-ARRAY STRING (STRING-FROM-KEYWORDS ON))))
           (COND (OFF
                  (AND ON (VECTOR-PUSH-EXTEND #/, STRING))
                  (APPEND-TO-ARRAY STRING " remove ")
                  (APPEND-TO-ARRAY STRING (STRING-FROM-KEYWORDS OFF)))))
         (APPEND-TO-ARRAY STRING "; ")))
  (AND *FILTER-KEYWORDS-ALIST* *MSG*
       (LET ((KEYS (GET-KEYWORDS-FROM-MSG-BY-FILTERING *MSG*)))
         (COND (KEYS
                (FORMAT STRING "M: ~A;" (STRING-FROM-KEYWORDS KEYS))))))
  (APPEND-TO-ARRAY STRING "R: menu."))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-KEYWORDS "Change keywords for the current message.
Left does same as last keywords operation.  Right gives a menu." ()
  (ZMAIL-KEYWORDS-MSG *MSG*)
  DIS-NONE)

(DEFUN ZMAIL-KEYWORDS-MSG (MSG &AUX OLD-KEYWORDS NEW-KEYWORDS)
  (SETQ OLD-KEYWORDS (MSG-GET MSG 'KEYWORDS))
  (COND ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
         (SETQ NEW-KEYWORDS (CHOOSE-KEYWORDS "Set keywords for this message:" OLD-KEYWORDS))
         ;;; Removed incorrect and unnecessary code for updating *KEYWORD-ALIST*
         (SETQ *DEFAULT-KEYWORDS*
               (LIST (DO ((NEW NEW-KEYWORDS (CDR NEW))
                          (NEW-NEW NIL)
                          (KEY))
                         ((NULL NEW) (NREVERSE NEW-NEW))
                       (SETQ KEY (CAR NEW))
                       (OR (MEMQ KEY OLD-KEYWORDS)
                           (PUSH KEY NEW-NEW)))
                     (DO ((OLD OLD-KEYWORDS (CDR OLD))
                          (OLD-OLD NIL)
                          (KEY))
                         ((NULL OLD) (NREVERSE OLD-OLD))
                       (SETQ KEY (CAR OLD))
                       (OR (MEMQ KEY NEW-KEYWORDS)
                           (PUSH KEY OLD-OLD)))))
         (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-KEYWORDS))
        ((MEMQ *ZMAIL-COMMAND-BUTTON* '(:KBD :MIDDLE))
         (SETQ NEW-KEYWORDS (GET-KEYWORDS-FROM-MSG-BY-FILTERING MSG OLD-KEYWORDS)))
        (*DEFAULT-KEYWORDS*
         (SETQ NEW-KEYWORDS OLD-KEYWORDS)
         (DOLIST (KEY (CAR *DEFAULT-KEYWORDS*)) ;Keywords on
           (PUSH* KEY NEW-KEYWORDS))
         (DOLIST (KEY (CADR *DEFAULT-KEYWORDS*))        ;Keywords off
           (SETQ NEW-KEYWORDS (REMQ KEY NEW-KEYWORDS))))
        (T
         (BARF "There are no default keywords yet")))
  (CHANGE-MSG-KEYWORDS MSG NEW-KEYWORDS OLD-KEYWORDS))

(DEFUN CHOOSE-KEYWORDS (&OPTIONAL LABEL OLD-KEYWORDS &AUX NEW-KEYWORDS)
  (SEND *KEYWORD-WINDOW* :SET-LABEL LABEL)
  (MULTIPLE-VALUE (*KEYWORD-ALIST* NEW-KEYWORDS)
    (SEND *KEYWORD-WINDOW* :MULTIPLE-CHOOSE *KEYWORD-ALIST* OLD-KEYWORDS
             (RECTANGLE-NEAR-COMMAND-MENU)))
  NEW-KEYWORDS)

(DEFUN GET-KEYWORDS-FROM-MSG-BY-FILTERING (MSG &OPTIONAL OLD-KEYWORDS &AUX KEYWORDS)
  (SETQ KEYWORDS (LOOP FOR (FILTER . KEYWORDS) IN *FILTER-KEYWORDS-ALIST*
                       WHEN (MSG-FITS-FILTER-P MSG FILTER)
                       APPEND KEYWORDS))
  (SETQ KEYWORDS (SI:ELIMINATE-DUPLICATES (APPEND KEYWORDS OLD-KEYWORDS)))
  (LOOP FOR KEYWORD IN KEYWORDS
        UNLESS (RASSQ KEYWORD *KEYWORD-ALIST*)
        DO (PUSH (CONS (STRING-DOWNCASE KEYWORD) KEYWORD) *KEYWORD-ALIST*))
  KEYWORDS)

(DEFUN CHANGE-MSG-KEYWORDS (MSG NEW-KEYWORDS OLD-KEYWORDS &AUX MSG-MAIL-FILE-BUFFER ZMAIL-BUFFER-KEYS)
  ;; Canonicalize the order of keywords for the msg to the order for the file.
  (SETQ MSG-MAIL-FILE-BUFFER (MSG-MAIL-FILE-BUFFER MSG)
        ZMAIL-BUFFER-KEYS (GET (LOCF (ZMAIL-BUFFER-OPTIONS MSG-MAIL-FILE-BUFFER)) :KEYWORDS)
        NEW-KEYWORDS (LET ((*KEYWORD-ALIST* ZMAIL-BUFFER-KEYS))
                       (SORT NEW-KEYWORDS #'(LAMBDA (KEY1 KEY2)
                                              (DO ((L *KEYWORD-ALIST* (CDR L))
                                                   (KEY))
                                                  ((NULL L)
                                                   (STRING-LESSP KEY1 KEY2))
                                                (SETQ KEY (CDAR L))
                                                (COND ((EQ KEY KEY1) (RETURN T))
                                                      ((EQ KEY KEY2) (RETURN NIL))))))))
  (COND ((EQUAL OLD-KEYWORDS NEW-KEYWORDS))     ;Did not change
        (T
         (DO ((KEYS NEW-KEYWORDS (CDR KEYS))
              (ELEM) (FLAG NIL))
             ((NULL KEYS)
              (AND FLAG (SETF (GET (LOCF (ZMAIL-BUFFER-OPTIONS MSG-MAIL-FILE-BUFFER)) :KEYWORDS)
                              ZMAIL-BUFFER-KEYS)))
           (OR (MEMQ (SETQ ELEM (RASSQ (CAR KEYS) *KEYWORD-ALIST*)) ZMAIL-BUFFER-KEYS)
               (SETQ ZMAIL-BUFFER-KEYS (NCONC ZMAIL-BUFFER-KEYS (NCONS ELEM))
                     FLAG T)))
         (MSG-PUT MSG NEW-KEYWORDS 'KEYWORDS)
         (LET ((STRING (AND NEW-KEYWORDS (STRING-FROM-KEYWORDS NEW-KEYWORDS))))
           (MSG-PUT MSG STRING 'KEYWORDS-STRING)
           (UPDATE-MSG-SUMMARY-LINE MSG :KEYWORDS)
           (COND ((EQ MSG *MSG*)
                  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-KEYWORDS)
                  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-MOVE)
                  (SETQ *CURRENT-MSG-KEYWORDS-STRING* (OR STRING "{}"))))))))

(DEFUN STRING-FROM-KEYWORDS (KEYWORDS &AUX STR)
  (DO ((KEYS KEYWORDS (CDR KEYS))
       (LENGTH 1))
      ((NULL KEYS)
       (SETQ STR (MAKE-ARRAY LENGTH :TYPE 'ART-STRING)))
    (SETQ LENGTH (+ LENGTH (ARRAY-ACTIVE-LENGTH (CAR (RASSQ (CAR KEYS) *KEYWORD-ALIST*)))
                    1)))
  (ASET #/{ STR 0)
  (DO ((KEYS KEYWORDS (CDR KEYS))
       (I0 1 (1+ I1))
       (I1) (KEY) (LEN))
      ((NULL KEYS)
       (ASET #/} STR I1))
    (SETQ KEY (CAR (RASSQ (CAR KEYS) *KEYWORD-ALIST*))
          LEN (ARRAY-ACTIVE-LENGTH KEY)
          I1 (+ I0 LEN))
    (COPY-ARRAY-PORTION KEY 0 LEN STR I0 I1)
    (ASET #/SP STR I1))
  STR)

;;; Some file commands
(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-MOVE (STRING &OPTIONAL RECURSIVE)
  (OR RECURSIVE (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER NIL T))
  (STRING-NCONC STRING "Move current message into buffer: "
                (GET 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER :WHO-LINE-DOCUMENTATION)))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION ZMAIL-SUMMARY-MOVE *SUMMARY-MOVE-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER ZMAIL-SUMMARY-MOVE (STRING &OPTIONAL RECURSIVE)
  (OR RECURSIVE (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER NIL T))
  (STRING-NCONC STRING "Move this message into buffer: "
                (GET 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER :WHO-LINE-DOCUMENTATION)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOVE "Move message into buffer.
Click right for menu of existing buffers, to specify a filename,
or for special destinations, such as Hardcopy." ()
  (LET ((BUFFER (GET-DEFAULTED-MOVE-ZMAIL-BUFFER *MSG*)))
    (SEND BUFFER :ADD-MSG *MSG*)
    (FORMAT *QUERY-IO* "~&Moved to ~A" (SEND BUFFER :NAME)))
  DIS-NONE)

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *FILTER-MOVE-MAIL-FILE-ALIST*
                                             GET-DEFAULTED-MOVE-ZMAIL-BUFFER)
(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *DEFAULT-MOVE-MAIL-FILE-NAME*
                                             GET-DEFAULTED-MOVE-ZMAIL-BUFFER)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER GET-DEFAULTED-MOVE-ZMAIL-BUFFER
                                               (STRING &OPTIONAL RECURSIVE)
  (FORMAT STRING "~@[L: /"~A/"; ~]"
          (COND (*DEFAULT-MOVE-ZMAIL-BUFFER*
                 (zmail-buffer-name-for-buffer-alist *DEFAULT-MOVE-ZMAIL-BUFFER*))
                (*DEFAULT-MOVE-MAIL-FILE-NAME*
                 (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                   (SEND (FS:MERGE-PATHNAME-DEFAULTS *DEFAULT-MOVE-MAIL-FILE-NAME*
                                                        *ZMAIL-PATHNAME-DEFAULTS*)
                            :STRING-FOR-PRINTING)))))
  (COND (*MSG*
         (STRING-NCONC STRING "M: By filters; ")))
;  (LET ((ZMAIL-BUFFER (AND *MSG* (GET-MAIL-FILE-FROM-MSG-BY-FILTERING *MSG*))))
;    (COND (ZMAIL-BUFFER
;          (FORMAT STRING "M: ~A; "
;                  (ZMAIL-BUFFER-DOCUMENTATION-STRING ZMAIL-BUFFER)))))
  (FORMAT STRING "R: menu.")
  (OR RECURSIVE
      (DOLIST (COM '(COM-ZMAIL-MOVE COM-ZMAIL-MOVE-ALL-TO-FILE ZMAIL-SUMMARY-MOVE))
        (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM NIL T))))

(DEFUN ZMAIL-BUFFER-DOCUMENTATION-STRING (ZMAIL-BUFFER)
  "Document a buffer, string, or keyword signifying a way of choosing one.
Returns a string to be used in printing documentation about the buffer or keyword."
  (OR (CADR (ASSQ ZMAIL-BUFFER '((:NEW-TEMP "New temporary")
                                 (:OLD-TEMP "Generated temporary")
                                 (:BY-FILTERS "By filters")
                                 (:FIND-FILE "Find file")
                                 (:TEXT-FILE "Text mail file")
                                 (:HARDCOPY "Hardcopy")
                                 (:HARDCOPY-WITH-OPTIONS "Hardcopy options"))))
      (IF (TYPEP ZMAIL-BUFFER 'ZMAIL-BUFFER)
          (SEND ZMAIL-BUFFER :NAME)
        ZMAIL-BUFFER)))

;*DEFAULT-MOVE-ZMAIL-BUFFER* is a ZMAIL-BUFFER instance to move to for click-left.
;If that is NIL, it means no move has been done yet.  In that case, possibly use
;*DEFAULT-MOVE-MAIL-FILE-NAME*, which is a pathname object, and create a ZMAIL-BUFFER.
;FOR-WHOLE-FILE-P is T when called from COM-ZMAIL-MOVE-ALL-TO-FILE, which
;means that a group of messages are being moved.  This makes click middle
;return something to move each message based on filtering that message.
(DEFUN GET-DEFAULTED-MOVE-ZMAIL-BUFFER (&OPTIONAL MSG FOR-WHOLE-FILE-P &AUX NEW-P)
  (IF (MEMQ *ZMAIL-COMMAND-BUTTON* '(:MIDDLE :RIGHT))
      (LET (BUFFER)
        (MULTIPLE-VALUE (BUFFER NEW-P)
          (GET-MOVE-ZMAIL-BUFFER (AND (NEQ *ZMAIL-COMMAND-BUTTON* :RIGHT) MSG)
                                 FOR-WHOLE-FILE-P))
        (OR BUFFER (ABORT-CURRENT-COMMAND))
        (WHEN (OR (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
                  (NOT FOR-WHOLE-FILE-P))
          ;; Except for middle on multiple messages, set default and update doc.
          (SETQ *DEFAULT-MOVE-ZMAIL-BUFFER* BUFFER)
          (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER))
        BUFFER)
    ;; Here if click-left or keyboard command.  For the latter, read a filename.
    (COND ((EQ *ZMAIL-COMMAND-BUTTON* :KBD)
           (SETQ *DEFAULT-MOVE-MAIL-FILE-NAME*
                 (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* *MOVE-FILE-NAME-STICKY-FN2*))
                   (READ-DEFAULTED-PATHNAME "File to move to:"
                                            (DEFAULT-ZMAIL-MOVE-PATHNAME)))
                 *DEFAULT-MOVE-ZMAIL-BUFFER* NIL)
           (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER)))
    ;; For click-left, use the default mail file or mail file name.
    (COND (*DEFAULT-MOVE-ZMAIL-BUFFER*)
          (*DEFAULT-MOVE-MAIL-FILE-NAME*
           (OR (SETQ *DEFAULT-MOVE-ZMAIL-BUFFER*
                     (GET-ZMAIL-BUFFER-FROM-PATHNAME *DEFAULT-MOVE-MAIL-FILE-NAME* NIL))
               (MULTIPLE-VALUE (*DEFAULT-MOVE-ZMAIL-BUFFER* NEW-P)
                 (ZMAIL-FIND-FILE-NOSELECT *DEFAULT-MOVE-MAIL-FILE-NAME* NIL NIL))))
          (T
           (BARF "There is no default buffer or mail file to move to")))
    *DEFAULT-MOVE-ZMAIL-BUFFER*))

;;; MSG is a message to check with filters and look for a mail file from
;;; *FILTER-MOVE-MAIL-FILE-ALIST*.
(DEFUN GET-MOVE-ZMAIL-BUFFER (&OPTIONAL MSG FOR-WHOLE-FILE-P &AUX ITEM-LIST NEW-P)
  (DECLARE (VALUES ZMAIL-BUFFER NEW-P))
  (MULTIPLE-VALUE-BIND (ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)
      (GET-ZMAIL-BUFFER-ALISTS T)
    (SETQ ITEM-LIST (TV:APPEND-ITEM-LISTS ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)))
  (SETQ ITEM-LIST (APPEND ITEM-LIST
                          (AND (ODDP (LENGTH ITEM-LIST)) '(("" :NO-SELECT T)))
                          (IF FOR-WHOLE-FILE-P
                              '(("By Individual Filters" :VALUE :BY-FILTERS
                                 :FONT FONTS:HL12I :DOCUMENTATION
  "Move each msg into a mail file chosen by matching it against filter-mail-file associations"))
                            '(("By Filters" :VALUE :BY-FILTERS
                                 :FONT FONTS:HL12I :DOCUMENTATION
  "Move into a mail file chosen by matching the message against filter-mail-file associations")))
                          '(("New Temporary" :VALUE :NEW-TEMP
                             :FONT FONTS:HL12I
                             :DOCUMENTATION
  "Move into a new temporary buffer, whose name is read from the keyboard.")
                            ("Generated Temporary" :VALUE :OLD-TEMP
                             :FONT FONTS:HL12I
                             :DOCUMENTATION
  "Move into a new temporary buffer with an automatically generated name.")
                            ("Find file" :VALUE :FIND-FILE
                             :FONT FONTS:HL12I
                             :DOCUMENTATION
  "Move into a new file buffer; pathname is read from the keyboard.")
                            ("Text Mail File" :VALUE :TEXT-FILE
                             :FONT FONTS:HL12I
                             :DOCUMENTATION
  "Move into a new write-only file buffer; messages are separated just by blank lines")
                            ("Abort" :VALUE :ABORT
                             :FONT FONTS:HL12I
                             :DOCUMENTATION "Abort this command.")
                            ("Hardcopy" :BUTTONS (:HARDCOPY NIL :HARDCOPY-WITH-OPTIONS)
                             :FONT FONTS:HL12I
                             :DOCUMENTATION
  "Print on a hardcopy device.  Click right to get hardcopy options."))))
  (OR (EQUAL ITEM-LIST (SEND *MOVE-ZMAIL-BUFFER-MENU* :ITEM-LIST))
      (SEND *MOVE-ZMAIL-BUFFER-MENU* :SET-ITEM-LIST ITEM-LIST))
  (UNWIND-PROTECT
    (PROGN
      (DO ((ZMAIL-BUFFER)
           (ALWAYS-NEW-FLAVOR NIL NIL))
          (NIL)
        (IF MSG
            (SETQ ZMAIL-BUFFER :BY-FILTERS)
          ;; This is a noop if already exposed
          (TV:EXPOSE-WINDOW-NEAR *MOVE-ZMAIL-BUFFER-MENU* (RECTANGLE-NEAR-COMMAND-MENU))
          (SETQ ZMAIL-BUFFER (SEND *MOVE-ZMAIL-BUFFER-MENU* :CHOOSE))
          (SET-COMMAND-BUTTON (SEND *MOVE-ZMAIL-BUFFER-MENU* :LAST-BUTTONS)))
        (AND (EQ ZMAIL-BUFFER :BY-FILTERS)
             ;; Click middle, or "by filters" chosen from menu after click right.
             ;; Let the user get out anything that could appear in the menu, including
             ;; hardcopy :FIND-FILE, :MENU-CHOOSE, etc.
             (IF FOR-WHOLE-FILE-P
                 (SETQ ZMAIL-BUFFER 'MOVE-AND-FILTER-EACH-MSG)
               (SETQ ZMAIL-BUFFER (OR (GET-MAIL-FILE-FROM-MSG-BY-FILTERING MSG)
                                      (BARF "Cannot find suitable filter")))))
        (AND (EQ ZMAIL-BUFFER :MENU-CHOOSE) (SETQ ZMAIL-BUFFER NIL MSG NIL))
        (AND (EQ ZMAIL-BUFFER :ABORT) (ABORT-CURRENT-COMMAND))
        (AND (EQ ZMAIL-BUFFER :NEW-TEMP)
             (LET ((NAME (CALL-POP-UP-MINI-BUFFER-EDITOR
                           *MOVE-ZMAIL-BUFFER-MENU* #'TYPEIN-LINE-READLINE "New temporary")))
               (AND (NOT (SYMBOLP NAME))
                    (SETQ ZMAIL-BUFFER (MAKE-NEW-TEMP-ZMAIL-BUFFER NAME)))))
        (AND (EQ ZMAIL-BUFFER :OLD-TEMP)
             (SETQ ZMAIL-BUFFER (GET-RECYCLED-TEMP-ZMAIL-BUFFER "Temp")))
        (AND (MEMQ ZMAIL-BUFFER '(:HARDCOPY :HARDCOPY-WITH-OPTIONS))
             (SETQ ZMAIL-BUFFER (MAKE-HARDCOPY-ZMAIL-BUFFER
                                  (EQ ZMAIL-BUFFER :HARDCOPY-WITH-OPTIONS) FOR-WHOLE-FILE-P
                                  `(:WINDOW ,*MOVE-ZMAIL-BUFFER-MENU*))))
        (AND (MEMQ ZMAIL-BUFFER '(:FIND-FILE :TEXT-FILE))
             (*CATCH 'ZWEI-COMMAND-LOOP         ;In case of G
               (LET ((DEFAULT (IF (AND *DEFAULT-MOVE-ZMAIL-BUFFER*
                                       (ZMAIL-BUFFER-DISK-P *DEFAULT-MOVE-ZMAIL-BUFFER*))
                                  (BUFFER-PATHNAME *DEFAULT-MOVE-ZMAIL-BUFFER*)
                                  (DEFAULT-ZMAIL-MOVE-PATHNAME))))
                 (SETQ ALWAYS-NEW-FLAVOR (AND (EQ ZMAIL-BUFFER :TEXT-FILE)
                                              'TEXT-MAIL-FILE-BUFFER)
                       ZMAIL-BUFFER NIL)
                 (SETQ ZMAIL-BUFFER (MAYBE-CALL-POP-UP-MINI-BUFFER-EDITOR
                                      *MOVE-ZMAIL-BUFFER-MENU*
                                      #'READ-DEFAULTED-PATHNAME
                                      (IF ALWAYS-NEW-FLAVOR "Make text mail file"
                                        "Find mail file")
                                      DEFAULT
                                      (AND *MOVE-FILE-NAME-STICKY-FN2*
                                           (SEND DEFAULT :TYPE))
                                      NIL
                                      (IF ALWAYS-NEW-FLAVOR :WRITE :NEW-OK))))))
        (AND (OR (STRINGP ZMAIL-BUFFER) (TYPEP ZMAIL-BUFFER 'FS:PATHNAME))
             (LET ((PATHNAME ZMAIL-BUFFER))
               (OR (SETQ ZMAIL-BUFFER (GET-ZMAIL-BUFFER-FROM-PATHNAME ZMAIL-BUFFER NIL))
                   (MULTIPLE-VALUE (ZMAIL-BUFFER NEW-P)
                     (ZMAIL-FIND-FILE-NOSELECT PATHNAME `(:WINDOW ,*MOVE-ZMAIL-BUFFER-MENU*)
                                               ALWAYS-NEW-FLAVOR ALWAYS-NEW-FLAVOR)))))
        (AND ZMAIL-BUFFER (RETURN (VALUES ZMAIL-BUFFER NEW-P)))))
    (SEND *MOVE-ZMAIL-BUFFER-MENU* :DEACTIVATE)))

;; This function is returned as the "Zmail buffer to move to"
;; in order to move a bunch of messages to individual destinations.
;; Attempting to move them into this "buffer"
;; actually looks over the messages and moves each one as appropriate.
(DEFUN MOVE-AND-FILTER-EACH-MSG (OP &REST ARGS)
  (CASE OP
    (:ADD-ZMAIL-BUFFER
     (LET ((BUFFER (CAR ARGS)))
       (DOMSGS (M BUFFER)
         (LET ((REALLY-TO-BUFFER (GET-MAIL-FILE-FROM-MSG-BY-FILTERING M)))
           (WHEN REALLY-TO-BUFFER
             (SEND (OR (GET-ZMAIL-BUFFER-FROM-PATHNAME REALLY-TO-BUFFER NIL)
                       (ZMAIL-FIND-FILE-NOSELECT REALLY-TO-BUFFER))
                   :ADD-MSG M))))))
    (:NAME "files by individual filters")
    (OTHERWISE (ZMAIL-ERROR "Internal error in ZMail."))))

(DEFUN GET-MAIL-FILE-FROM-MSG-BY-FILTERING (MSG)
  (LOOP FOR (FILTER . MAIL-FILE) IN *FILTER-MOVE-MAIL-FILE-ALIST*
        WHEN (OR (EQ FILTER T)
                 (AND (GET FILTER 'FILTER-FUNCTION)  ;Avoid bombing out updating who line doc.
                      (MSG-FITS-FILTER-P MSG FILTER)))
        DO (RETURN MAIL-FILE)))

(DEFUN ZMAIL-BUFFER-NAME-FOR-BUFFER-ALIST (ZMAIL-BUFFER)
  (IF (OR (NULL *ZMAIL-USUAL-MAIL-FILE-DIRECTORY*)
          (NOT (TYPEP ZMAIL-BUFFER 'MAIL-FILE-BUFFER)))
      (ZMAIL-BUFFER-NAME ZMAIL-BUFFER)
    (LET ((BUFFER-PATHNAME (SEND ZMAIL-BUFFER :PATHNAME))
          (USUAL-PATHNAME (FS:PARSE-PATHNAME *ZMAIL-USUAL-MAIL-FILE-DIRECTORY*)))
      (IF (AND (EQ (SEND BUFFER-PATHNAME :HOST) (SEND USUAL-PATHNAME :HOST))
               (EQUAL (SEND BUFFER-PATHNAME :DEVICE) (SEND USUAL-PATHNAME :DEVICE))
               (EQUAL (SEND BUFFER-PATHNAME :DIRECTORY) (SEND USUAL-PATHNAME :DIRECTORY)))
          (SEND BUFFER-PATHNAME :RAW-NAME)
        (ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))))

(DEFUN GET-ZMAIL-BUFFER-ALISTS (&OPTIONAL OTHERS-TOO &AUX MFL TMFL MFL-PATHNAMES)
  (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
    (LET ((ELEM (CONS (ZMAIL-BUFFER-NAME-FOR-BUFFER-ALIST ZMAIL-BUFFER) ZMAIL-BUFFER)))
      (IF (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
          (PUSH ELEM MFL)
          (PUSH ELEM TMFL))))
  (SETQ MFL (SORTCAR MFL #'STRING-LESSP)
        TMFL (SORTCAR TMFL #'STRING-LESSP))
  (SETQ MFL-PATHNAMES
        (MAPCAR 'BUFFER-PATHNAME
                *ZMAIL-BUFFER-LIST*))
  (IF (FIND-IF 'STRINGP *OTHER-MAIL-FILE-NAMES*)
      (SETQ *OTHER-MAIL-FILE-NAMES*
            (MAPCAR 'PARSE-NAMESTRING *OTHER-MAIL-FILE-NAMES*)))
  (AND OTHERS-TOO
       (DOLIST (FILE-NAME (SORT *OTHER-MAIL-FILE-NAMES* 'STRING-LESSP))
         (OR (MEMBER (SEND FILE-NAME :NEW-VERSION :NEWEST) MFL-PATHNAMES)
             (SETQ MFL (NCONC MFL (NCONS (CONS FILE-NAME FILE-NAME)))))))
  (VALUES MFL TMFL))

(DEFUN SOME-PLIST (PLIST PROPERTIES)
  (DO ((PLIST PLIST (CDDR PLIST))
       (LIST NIL))
      ((NULL PLIST) (NREVERSE LIST))
    (COND ((MEMQ (CAR PLIST) PROPERTIES)
           (PUSH (CAR PLIST) LIST)
           (PUSH (CADR PLIST) LIST)))))

(DEFUN SOME-PLIST-NOT (PLIST PROPERTIES)
  (DO ((PLIST PLIST (CDDR PLIST))
       (LIST NIL))
      ((NULL PLIST) (NREVERSE LIST))
    (COND ((NOT (MEMQ (CAR PLIST) PROPERTIES))
           (PUSH (CAR PLIST) LIST)
           (PUSH (CADR PLIST) LIST)))))

(DEFUN DEFAULT-ZMAIL-MOVE-PATHNAME ()
  (SEND (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
          (COND ((AND *DEFAULT-MOVE-ZMAIL-BUFFER*
                      (ZMAIL-BUFFER-DISK-P *DEFAULT-MOVE-ZMAIL-BUFFER*))
                 (BUFFER-PATHNAME *DEFAULT-MOVE-ZMAIL-BUFFER*))
                (*DEFAULT-MOVE-MAIL-FILE-NAME*
                 (FS:MERGE-PATHNAME-DEFAULTS *DEFAULT-MOVE-MAIL-FILE-NAME*
                                             *ZMAIL-PATHNAME-DEFAULTS*))
                (T (FS:MERGE-PATHNAME-DEFAULTS USER-ID (FS:USER-HOMEDIR) :XMAIL))))
        ;; Make version be NIL just in case it used to be :UNSPECIFIC
        ;; (such as in an ITS pathname with TYPE = "BABYL")
        ;; because it is going to be used with FS:*ALWAYS-MERGE-TYPE-AND-VERSION* non-NIL.
        :NEW-VERSION NIL))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-RENAME-BUFFER (STRING &AUX ELEM)
  (SETQ ELEM (ASSQ '*ZMAIL-FILE-NAME* *SELECTABLE-MODE-LINE-ELEMENTS*))
  (IF (NULL *ZMAIL-BUFFER*)
      (AND ELEM
           (SETQ *SELECTABLE-MODE-LINE-ELEMENTS* (DELQ ELEM *SELECTABLE-MODE-LINE-ELEMENTS*)))
      (OR ELEM
          (PUSH '(*ZMAIL-FILE-NAME* . COM-ZMAIL-RENAME-BUFFER)
                *SELECTABLE-MODE-LINE-ELEMENTS*))
      (FORMAT STRING "Change the ~:[file~]name of this buffer."
              (NOT (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-RENAME-BUFFER
                                 "Change the name of this buffer."
                                 (NO-MSG-OK)
  (LET ((NEW-NAME (WITH-BACKGROUND-PROCESS-LOCKED
                    (IF (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
                        (LET ((FS:*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                          (READ-DEFAULTED-PATHNAME "New filename"
                                                   (BUFFER-PATHNAME *ZMAIL-BUFFER*)
                                                   NIL NIL :WRITE))
                        (TYPEIN-LINE-READLINE "New name for ~A:"
                                              (ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*))))))
    (WHEN (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
      (SETF (BUFFER-PATHNAME *ZMAIL-BUFFER*) NEW-NAME)
      (SETQ NEW-NAME (STRING NEW-NAME))
      (SETF (BUFFER-FILE-ID *ZMAIL-BUFFER*) T)) ;Also means it must be saved out
    (SEND *ZMAIL-BUFFER* :RENAME NEW-NAME))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REFRESH
                                 "Complete redisplay"
                                 (NO-ZMAIL-BUFFER-OK)
  (DOLIST (WINDOW (SEND *ZMAIL-WINDOW* :EDITOR-WINDOWS))
    (IF (SEND WINDOW :EXPOSED-P)
        (SEND WINDOW :PREPARE-FOR-REDISPLAY)))
  (SEND *ZMAIL-WINDOW* :REFRESH)
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-START-OF-MSG
                                 "Scroll back to start of message"
                                 (NO-ZMAIL-BUFFER-OK)
  (MOVE-BP (WINDOW-POINT *WINDOW*)
           (INTERVAL-FIRST-BP (WINDOW-INTERVAL *WINDOW*)))
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (REDISPLAY *WINDOW* :POINT)
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-END-OF-MSG
                                 "Scroll to end of message"
                                 (NO-ZMAIL-BUFFER-OK)
  (MOVE-BP (WINDOW-POINT *WINDOW*)
           (INTERVAL-LAST-BP (WINDOW-INTERVAL *WINDOW*)))
  (MOVE-BP (WINDOW-START-BP *WINDOW*)
           (INTERVAL-LAST-BP (WINDOW-INTERVAL *WINDOW*)))
  ;; Putting point and start-bp at end of interval
  ;; makes redisplay scroll up a suitable amount.
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (REDISPLAY *WINDOW* :POINT)
  DIS-NONE)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-ZMAIL-MODE-LINE-SCROLL
  "Scroll message: L: forward; M: backward; R: menu."
  )

(DEFINE-ZMAIL-GLOBAL *LAST-MODE-LINE-SCROLL-ITEM* NIL)

(DEFVAR *MODE-LINE-SCROLL-MENU-ALIST*
        '(("Forward" :VALUE :FORWARD :DOCUMENTATION "Scroll message forward a screenful.")
          ("Backward" :VALUE :BACKWARD :DOCUMENTATION "Scroll message backward a screenful.")
          ("Beginning" :VALUE :BEGINNING :DOCUMENTATION "Scroll to top of message.")
          ("End" :VALUE :END :DOCUMENTATION "Scroll to end of message")))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MODE-LINE-SCROLL "Scroll the message window.
Left scrolls forward, middle scrolls backward, right gives a menu." (NO-ZMAIL-BUFFER-OK)
  (LET ((MODE :FORWARD)
        (N-PLINES (WINDOW-N-PLINES *WINDOW*)))
    (AND (MEMQ *ZMAIL-COMMAND-BUTTON* '(:MIDDLE :RIGHT))
         (MULTIPLE-VALUE (MODE *LAST-MODE-LINE-SCROLL-ITEM*)
           (ZMAIL-MENU-CHOOSE NIL *MODE-LINE-SCROLL-MENU-ALIST* *LAST-MODE-LINE-SCROLL-ITEM*
                              NIL :BACKWARD)))
    (CASE MODE
      (:FORWARD (RECENTER-WINDOW-RELATIVE *WINDOW* (- N-PLINES 1)))
      (:BACKWARD (RECENTER-WINDOW-RELATIVE *WINDOW* (- 1 N-PLINES)))
      (:BEGINNING (MOVE-BP (WINDOW-POINT *WINDOW*)
                           (INTERVAL-FIRST-BP (WINDOW-INTERVAL *WINDOW*)))
                  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
                  (REDISPLAY *WINDOW* :POINT))
      (:END (MOVE-BP (WINDOW-POINT *WINDOW*)
                     (INTERVAL-LAST-BP (WINDOW-INTERVAL *WINDOW*)))
            (MOVE-BP (WINDOW-START-BP *WINDOW*)
                     (INTERVAL-LAST-BP (WINDOW-INTERVAL *WINDOW*)))
            (MUST-REDISPLAY *WINDOW* DIS-TEXT)
            (REDISPLAY *WINDOW* :POINT))))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-SCROLL-SUMMARY-WINDOW
                                 "Scroll the summary window"
                                 (NO-MSG-OK NUMERIC-ARG-OK)
  (OR (SEND *SUMMARY-WINDOW* :EXPOSED-P) (BARF))
  (LET* ((TYPE :RELATIVE)
         (ARG (COND ((ZEROP *NUMERIC-ARG*)
                     (SETQ TYPE :ABSOLUTE)      ;Arg of zero put current msg at top
                     (MSG-DISPLAYED-INDEX *MSG*))
                    ((MEMQ *NUMERIC-ARG-P* '(:DIGITS :CONTROL-U))       ;Explicit arg
                     *NUMERIC-ARG*)             ;Scroll that many lines
                    (T
                     (* *NUMERIC-ARG*           ;Else scroll a screenful
                        (1- (TV:SHEET-NUMBER-OF-INSIDE-LINES *SUMMARY-WINDOW*)))))))
    (SEND *SUMMARY-WINDOW* :SCROLL-TO ARG TYPE))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-JUMP
                                 "Move to a specific message"
                                 (NUMERIC-ARG-OK)
  (IF *NUMERIC-ARG-P*
      (ZMAIL-SELECT-MSG (1- (RANGE *NUMERIC-ARG* 1 (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*))))
      (COM-ZMAIL-NEXT-OR-PREVIOUS-INTERNAL :FIRST-UNDELETED)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-BREAK
                                 "Break loop"
  (NO-ZMAIL-BUFFER-OK)
;  (SEND *TYPEOUT-WINDOW* :OUTPUT-HOLD-EXCEPTION)
;  (TV:WINDOW-CALL (*TYPEOUT-WINDOW*)
;  (UNWIND-PROTECT
  (LET ((*INSIDE-BREAK* T))
    (BREAK "ZMail"))
;;;??? What did this accomplish?
;      (SEND SELF :EXPOSE-MODE-LINE-WINDOW)
;      )
  (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
  DIS-NONE)

(DEFINE-ZMAIL-GLOBAL *LAST-MAP-MENU-ITEM* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-MAP (STRING)
  (FORMAT STRING "Operate on all messages: ~@[L: ~A; ~]~@[M: ~A; ~]R: menu."
          (CAR *LAST-MAP-MENU-ITEM*)
          (NAME-FROM-MENU-VALUE *MAP-MIDDLE-MODE* *ZMAIL-MAP-COMMAND-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *MAP-MIDDLE-MODE* COM-ZMAIL-MAP)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MAP "Perform operation on these messages.
Left defaults to same as last Map command.  Middle is a user option.
Right gives menu of operations."
                                 (NO-MSG-OK NUMERIC-ARG-OK)
  (COND ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
         (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-MOVE-ALL-TO-FILE)))
  (LET (COMMAND)
    (MULTIPLE-VALUE (COMMAND *LAST-MAP-MENU-ITEM*)
      (ZMAIL-MENU-CHOOSE *ZMAIL-MAP-COMMAND-MENU* NIL *LAST-MAP-MENU-ITEM*
                         NIL *MAP-MIDDLE-MODE*))
    (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-MAP)
    (FUNCALL COMMAND)))

(DEFPROP COM-ZMAIL-DELETE COM-ZMAIL-DELETE-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE-ALL
                                "Delete these messages."
                                ()
  (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
  (DOMSGS (MSG *ZMAIL-BUFFER*)
    (OR (MSG-GET MSG 'DELETED)
        (MSG-PUT MSG T 'DELETED)))
  (ZMAIL-SELECT-MSG *MSG* NIL NIL))

;;; This is separate so that it can move to the next undeleted message
(DEFUN (COM-ZMAIL-DELETE ASSOCIATED-MAP-COMMAND) (MAP-FUNCTION MAP-ARG
                                                  FILTER-FUNCTION FILTER-ARG)
  (FUNCALL MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
           #'(LAMBDA (MSG IGNORE)
               (OR (MSG-GET MSG 'DELETED)
                   (MSG-PUT MSG T 'DELETED)))
           NIL)
  (IF (AND *MSG* (MSG-GET *MSG* 'DELETED))
      (MOVE-AFTER-DELETE (CHOOSE-DELETE-MODE))
      DIS-NONE))

(DEFPROP COM-ZMAIL-UNDELETE COM-ZMAIL-UNDELETE-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-UNDELETE-ALL
                                 "Undelete all these messages."
                                 ()
  (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
  (DOMSGS (MSG *ZMAIL-BUFFER*)
    (AND (MSG-GET MSG 'DELETED)
         (MSG-PUT MSG NIL 'DELETED)))
  (ZMAIL-SELECT-MSG *MSG* NIL NIL))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-TYPE "Type this message." ()
  (USING-OVERLYING-WINDOW
    (SEND *OVERLYING-WINDOW* :VIEW-STREAM (INTERVAL-STREAM (MSG-INTERVAL *MSG*))))
  DIS-NONE)

(DEFPROP COM-ZMAIL-TYPE COM-ZMAIL-TYPE-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-TYPE-ALL
                                 "Type out all these messages."
                                 ()
  (USING-OVERLYING-WINDOW
    (SEND *OVERLYING-WINDOW* :VIEW-STREAM (MAKE-ZMAIL-BUFFER-STREAM *ZMAIL-BUFFER*)))
  DIS-NONE)

(DEFUN MAKE-ZMAIL-BUFFER-STREAM (ZMAIL-BUFFER)
  (LET-CLOSED ((*ARRAY* (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)) (*I* -1)
               (*LINE*) (*END-LINE*))
    'ZMAIL-BUFFER-IO))

;; Copied from LAD: RELEASE-3.ZMAIL; COMNDS.LISP#593 on 26-Mar-87 17:31:14
;;; Bare minimum for typing out purposes
(LOCAL-DECLARE ((SPECIAL *ARRAY* *I* *LINE* *END-LINE*))
(DEFSELECT ZMAIL-BUFFER-IO
  (:LINE-IN (&OPTIONAL SIZE EOF)
   (COND ((NULL *LINE*)
          (IF ( (SETQ *I* (1+ *I*)) (ARRAY-ACTIVE-LENGTH *ARRAY*))
              (IF EOF (ERROR EOF) (VALUES (CANONICALIZE-LINE "" SIZE) T))
              (LET ((MSG (AREF *ARRAY* *I*)))
                (SETQ *LINE* (BP-LINE (MSG-START-BP MSG))
                      *END-LINE* (BP-LINE (MSG-END-BP MSG))))
              (ZMAIL-BUFFER-IO :LINE-IN SIZE EOF)))
         ((EQ *LINE* *END-LINE*)
          (SETQ *LINE* NIL)
          (CANONICALIZE-LINE "" SIZE))
         (T
          (PROG1 (CANONICALIZE-LINE *LINE* SIZE)
                 (SETQ *LINE* (LINE-NEXT *LINE*))))))
  (:CLOSE (&REST IGNORE)
   T)
  (:CHARACTERS  (&REST IGNORE)
   T)
  (:CURRENT-MSG-NO ()
   *I*)))

(DEFUN CANONICALIZE-LINE (LINE SIZE &AUX LEN RET-LINE)
  (IF (NULL SIZE) LINE
      (SETQ LEN (ARRAY-ACTIVE-LENGTH LINE)
            RET-LINE (MAKE-ARRAY LEN :TYPE 'ART-STRING
                                     :LEADER-LENGTH (IF (NUMBERP SIZE) SIZE NIL)))
      (COPY-ARRAY-CONTENTS LINE RET-LINE)
      (AND (NUMBERP SIZE) (STORE-ARRAY-LEADER LEN RET-LINE 0))
      RET-LINE))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-FIND-STRING
                                 "Move to next message containing specified string"
                                 (NUMERIC-ARG-OK)
  (LET ((DELTA (IF (MINUSP *NUMERIC-ARG*) -1 +1))
        (ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
        FUN STR)
    (MULTIPLE-VALUE (FUN STR)
      (ZMAIL-READ-FIND-SEARCH-STRING (IF (= DELTA +1) "Find string" "Find string reverse")))
    (DO ((I (+ *MSG-NO* DELTA) (+ I DELTA))
         (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
         (MSG))
        ((OR (< I 0) ( I NMSGS)) (BARF "Search failed: ~A" (STRING-APPEND "" STR)))
      (SETQ MSG (AREF ARRAY I))
      (AND (NOT (MSG-GET MSG 'DELETED))         ;Not if deleted (is this right?)
           (FUNCALL FUN (MSG-START-BP MSG) STR NIL NIL NIL (MSG-END-BP MSG))
           (RETURN (ZMAIL-SELECT-MSG I))))))

(DEFINE-ZMAIL-GLOBAL *INTERVAL-TO-BE-YANKED* NIL)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE-AND-SAVE-MSG
                                "Delete the current msg and save its text to be yanked back"
                                ()
  (LET ((BP (INTERVAL-LAST-BP (OR *INTERVAL-TO-BE-YANKED*
                                  (SETQ *INTERVAL-TO-BE-YANKED* (CREATE-INTERVAL))))))
    (INSERT-INTERVAL BP (MSG-INTERVAL *MSG*))
    (ONE-BLANK-LINE BP))
  (COM-ZMAIL-DELETE))

(DEFUN ONE-BLANK-LINE (BP &AUX (*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
  (DELETE-AROUND '(#/CR) BP)
  (OR (BEG-LINE-P BP) (INSERT-MOVING BP #/CR))
  (OR (LINE-BLANK-P (BP-LINE BP)) (INSERT BP #/CR)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-YANK-MSG "Appends message to end of current msg.
Messages is either specified by a numeric argument or the last message deleted is used."
                                (NUMERIC-ARG-OK)
  (LET ((INTERVAL (OR (PROG1 *INTERVAL-TO-BE-YANKED* (SETQ *INTERVAL-TO-BE-YANKED* NIL))
                      (LET ((MSG (IF *NUMERIC-ARG-P*
                                     (GET-MSG-FROM-ARG)
                                     *LAST-DELETED-MSG*)))
                        (AND (EQ MSG *MSG*) (BARF "Can't yank a message into itself"))
                        (MSG-PUT MSG T 'DELETED)
                        (MSG-INTERVAL MSG)))))
    ;;This uses (POINT) because it really wants to end up between the messages
    (LET ((POINT (POINT)))
      (MOVE-BP POINT (MSG-END-BP *MSG*))
      (INSERT-INTERVAL POINT INTERVAL)
      (ONE-BLANK-LINE POINT)))
  DIS-TEXT)

(DEFUN GET-MSG-FROM-ARG ()
  (AND (OR (< *NUMERIC-ARG* 1)
           (> *NUMERIC-ARG* (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*)))
       (BARF "Argument out of range"))
  (AREF (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*) (1- *NUMERIC-ARG*)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-OCCUR
  "Show lines within messages containing the given search string."
                                 ()
  (SETQ *TYPEOUT-WINDOW* (IF (TV:SHEET-EXPOSED-P *SUMMARY-WINDOW*)
                             (SEND *SUMMARY-WINDOW* :TYPEOUT-WINDOW)
                           (WINDOW-TYPEOUT-WINDOW *WINDOW*)))
  (MULTIPLE-VALUE-BIND (FUN STR)
      (ZMAIL-READ-FIND-STRING-SEARCH-STRING "Message lines containing")
    (DO ((STREAM (MAKE-ZMAIL-BUFFER-STREAM *ZMAIL-BUFFER*))
         (LINE) (EOF))
        (NIL)
      (MULTIPLE-VALUE (LINE EOF)
        (SEND STREAM :LINE-IN NIL))
      (AND EOF (RETURN NIL))
      (COND ((SEND FUN STR LINE)
             (SEND *TYPEOUT-WINDOW* :TRUNCATED-ITEM 'MSG-LINE LINE)
             (SEND *TYPEOUT-WINDOW* :TYO #/CR)))))
  DIS-NONE)

(DEFINE-ZMAIL-GLOBAL *ZMAIL-FIND-DEFAULT* NIL)
(DEFVAR *ZMAIL-FIND-STRING-SEARCH-DEFAULT*)
(DEFVAR *ZMAIL-FIND-SEARCH-DEFAULT*)

(DEFUN ZMAIL-READ-FIND-STRING-SEARCH-STRING (PROMPT)
  (DECLARE (RETURN-LIST FUNCTION KEY))
  (ZMAIL-READ-FIND-STRING PROMPT)
  (PROG () (RETURN-LIST *ZMAIL-FIND-STRING-SEARCH-DEFAULT*)))

(DEFUN ZMAIL-READ-FIND-SEARCH-STRING (PROMPT)
  (DECLARE (RETURN-LIST FUNCTION KEY))
  (ZMAIL-READ-FIND-STRING PROMPT)
  (PROG () (RETURN-LIST *ZMAIL-FIND-SEARCH-DEFAULT*)))

(DEFUN ZMAIL-READ-FIND-STRING (PROMPT &AUX STR)
  (SETQ STR (WITH-BACKGROUND-PROCESS-LOCKED
              (GET-EXTENDED-SEARCH-16B-STRING
                (FORMAT NIL "~A~@[ (Default: ~A)~]"
                        PROMPT *ZMAIL-FIND-DEFAULT* *ZMAIL-FIND-DEFAULT*))))
  (COND ((STRING-EQUAL STR ""))                 ;Leave defaults alone
        (T
         (SETQ *ZMAIL-FIND-DEFAULT* STR)        ;Save for next time
         (MULTIPLE-VALUE-BIND (STRINGS EXPR CR-P)
             (PARSE-EXTENDED-SEARCH-STRING STR)
           (SETQ *ZMAIL-FIND-SEARCH-DEFAULT*
                   (COND ((ATOM STRINGS)
                          `(SEARCH ,STRINGS))
                         (EXPR
                          `(FSM-EXPR-SEARCH (,STRINGS ,EXPR)))
                         (T
                          `(FSM-SEARCH ,STRINGS)))
                 *ZMAIL-FIND-STRING-SEARCH-DEFAULT*
                   (IF (OR (CONSP STRINGS) CR-P)
                       `(FSM-STRING-SEARCH (,(IF (CONSP STRINGS) STRINGS `(,STRINGS))
                                            ,EXPR ,CR-P))
                       `(STRING-SEARCH ,STRINGS)))))))

(DEFUN CALL-POP-UP-MINI-BUFFER-EDITOR (WHERE FUNCTION &REST ARGS &AUX VAL)
  (WITH-BACKGROUND-PROCESS-LOCKED
    (SETQ VAL (LEXPR-SEND *POP-UP-MINI-BUFFER-EDITOR* :CALL-MINI-BUFFER-NEAR-WINDOW
                          WHERE FUNCTION ARGS)))
  (AND (SYMBOLP VAL) (ABORT-CURRENT-COMMAND))
  VAL)

(DEFUN MAYBE-CALL-POP-UP-MINI-BUFFER-EDITOR (WHERE FUNCTION &REST ARGS)
  (WITH-BACKGROUND-PROCESS-LOCKED
    (IF (TV:SHEET-EXPOSED-P WHERE)
        (APPLY 'CALL-POP-UP-MINI-BUFFER-EDITOR WHERE FUNCTION ARGS )
      (APPLY FUNCTION ARGS))))

(DEFPROP COM-ZMAIL-KEYWORDS COM-ZMAIL-KEYWORDS-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-KEYWORDS-ALL
                                 "Put some keywords on all these messages."
                                 ()
  (ZMAIL-KEYWORDS-ALL T))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-UNKEYWORDS-ALL
                                 "Remove some keywords from all these messages."
                                 ()
  (ZMAIL-KEYWORDS-ALL NIL))

(DEFUN ZMAIL-KEYWORDS-ALL (ON-P &AUX KEYWORDS)
  (SETQ KEYWORDS (CASE *ZMAIL-COMMAND-BUTTON*
                   ((:LEFT :KBD)
                    (BARF))
                   (:MIDDLE
                    (LET ((MSG (FIRST-MSG-IF-ANY)))
                      (OR MSG (BARF))
                      (GET-KEYWORDS-FROM-MSG-BY-FILTERING MSG)))
                   (:RIGHT
                    (CHOOSE-KEYWORDS
                      (FORMAT NIL "Keywords to ~:[remove from~;add to~] all messages:"
                              ON-P)))))
  (DOMSGS (MSG *ZMAIL-BUFFER*)
    (LET* ((OLD-KEYWORDS (MSG-GET MSG 'KEYWORDS))
           (NEW-KEYWORDS (IF ON-P
                             (DO ((L KEYWORDS (CDR L))
                                  (NL (REVERSE OLD-KEYWORDS)))
                                 ((NULL L) (NREVERSE NL))
                               (OR (MEMQ (CAR L) NL)
                                   (PUSH (CAR L) NL)))
                             (DO ((L OLD-KEYWORDS (CDR L))
                                  (NL NIL))
                                 ((NULL L) (NREVERSE NL))
                               (OR (MEMQ (CAR L) KEYWORDS)
                                   (PUSH (CAR L) NL))))))
      (CHANGE-MSG-KEYWORDS MSG NEW-KEYWORDS OLD-KEYWORDS)))
  (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
  (ZMAIL-SELECT-MSG *MSG* NIL NIL))             ;Update mode line, etc.

;;; Useful for things that decide based on attributes of a message, pick one
;;; at random.
(DEFUN FIRST-MSG-IF-ANY (&OPTIONAL (ZMAIL-BUFFER *ZMAIL-BUFFER*))
  (AND ZMAIL-BUFFER
       (PLUSP (ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER))
       (AREF (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER) 0)))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-MOVE-ALL-TO-FILE
                                               (STRING &OPTIONAL RECURSIVE)
  (OR RECURSIVE (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER NIL T))
  (STRING-NCONC STRING "Move these messages into buffer: "
                (GET 'GET-DEFAULTED-MOVE-ZMAIL-BUFFER :WHO-LINE-DOCUMENTATION)))

(DEFPROP COM-ZMAIL-MOVE COM-ZMAIL-MOVE-ALL-TO-FILE ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOVE-ALL-TO-FILE "Move messages into a buffer.
Click right for menu of destinations." ()
  (LET ((BUFFER (GET-DEFAULTED-MOVE-ZMAIL-BUFFER (FIRST-MSG-IF-ANY) T)))
;  (LET ((*MSG* :NO-SELECT))
    (SEND BUFFER :ADD-ZMAIL-BUFFER *ZMAIL-BUFFER*)
    (FORMAT *QUERY-IO* "~&Moved to ~A" (SEND BUFFER :NAME)))
  (ZMAIL-SELECT-MSG *MSG* NIL NIL)
  DIS-NONE)

(DEFVAR *MSG-SORT-KEY-ALIST*
        `((,@*SORT-KEY-ALIST-1*
           ("Position" :VALUE MSG-POSITION-LESSP
            :DOCUMENTATION "Numerically by position in containing mail file."))
          (("Forward" :VALUE :FORWARD
                      :DOCUMENTATION "Perform sort forwards (least first).")
           ("Backward" :VALUE :BACKWARD
                       :DOCUMENTATION "Perform sort backwards (greatest first)."))))

(DEFINE-ZMAIL-GLOBAL *LAST-SORT-MODE* 'MSG-DATE-SORT-LESSP)
(DEFINE-ZMAIL-GLOBAL *LAST-SORT-DIRECTION* :FORWARD)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-SORT (STRING &AUX ZMAIL-BUFFER-MODE)
  (APPEND-TO-ARRAY STRING "Sort buffer:")
  (APPEND-MULTIPLE-MENU-DOCUMENTATION STRING *MSG-SORT-KEY-ALIST* "by"
                                      #/L *LAST-SORT-MODE* *LAST-SORT-DIRECTION*)
  (AND *ZMAIL-BUFFER*
       (SETQ ZMAIL-BUFFER-MODE (GET (LOCF (ZMAIL-BUFFER-OPTIONS *ZMAIL-BUFFER*)) :SORT))
       (APPEND-MULTIPLE-MENU-DOCUMENTATION STRING *MSG-SORT-KEY-ALIST* "by"
                                           #/M ZMAIL-BUFFER-MODE
                                           (IF (ZMAIL-BUFFER-APPEND-P *ZMAIL-BUFFER*)
                                               :FORWARD :BACKWARD)))
  (APPEND-TO-ARRAY STRING " R: menu."))

(DEFUN APPEND-MULTIPLE-MENU-DOCUMENTATION (STRING ALIST NAME BUTTON MODE DIRECTION
                                           &AUX NAME-1 NAME-2)
  (COND ((AND (SETQ NAME-1 (NAME-FROM-MENU-VALUE DIRECTION (CADR ALIST)))
              (SETQ NAME-2 (NAME-FROM-MENU-VALUE MODE (CAR ALIST))))
         (VECTOR-PUSH-EXTEND #/SP STRING)
         (VECTOR-PUSH-EXTEND BUTTON STRING)
         (APPEND-TO-ARRAY STRING ": ")
         (APPEND-TO-ARRAY STRING NAME-1)
         (VECTOR-PUSH-EXTEND #/SP STRING)
         (APPEND-TO-ARRAY STRING NAME)
         (VECTOR-PUSH-EXTEND #/SP STRING)
         (APPEND-TO-ARRAY STRING NAME-2)
         (VECTOR-PUSH-EXTEND #/; STRING))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SORT "Sort the current buffer.
Click right to specify sort key and//or direction." ()
  (LET ((MODE *LAST-SORT-MODE*)
        (DIRECTION *LAST-SORT-DIRECTION*))
    (CASE *ZMAIL-COMMAND-BUTTON*
      (:MIDDLE
       (SETQ MODE (OR (GET (LOCF (ZMAIL-BUFFER-OPTIONS *ZMAIL-BUFFER*)) :SORT)
                      'MSG-NOOP-SORT-LESSP)
             DIRECTION (IF (ZMAIL-BUFFER-APPEND-P *ZMAIL-BUFFER*) :FORWARD :BACKWARD)))
      (:RIGHT
       (MULTIPLE-VALUE (MODE DIRECTION)
         (DEFAULTED-MULTIPLE-MENU-CHOOSE-NEAR-MENU *MSG-SORT-KEY-ALIST*
                                                   *LAST-SORT-MODE* *LAST-SORT-DIRECTION*))
       (SETQ *LAST-SORT-MODE* MODE
             *LAST-SORT-DIRECTION* DIRECTION)
       (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-SORT)))
    (SORT-ZMAIL-BUFFER *ZMAIL-BUFFER* MODE (EQ DIRECTION :FORWARD))))

(DEFUN SORT-ZMAIL-BUFFER (ZMAIL-BUFFER MODE FORWARD-P &OPTIONAL NO-BACKGROUND-FINISH)
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (NOT NO-BACKGROUND-FINISH)
       (FOREGROUND-BACKGROUND-FINISH ZMAIL-BUFFER NIL))
  ;; Parse everything now, just in case.
  (DOMSGS (MSG ZMAIL-BUFFER)
    (ASSURE-MSG-PARSED MSG))
  (LET ((TEMPARRAY
          (SI:COPY-OBJECT (FOLLOW-STRUCTURE-FORWARDING (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))))
    ;; If the sort itself bombs out, we have not clobbered the buffer.
    (COND ((EQ MODE 'MSG-POSITION-LESSP)
           ;; For sorting by position, record each message's mail file and position in advance.
           ;; Then comparison is much faster.
           (DOTIMES (I (LENGTH TEMPARRAY))
             (SETF (AREF TEMPARRAY I)
                   (LIST (MSG-MAIL-FILE-BUFFER (AREF TEMPARRAY I))
                         (LOCATE-MSG-IN-ZMAIL-BUFFER (AREF TEMPARRAY I)
                                                     (MSG-MAIL-FILE-BUFFER (AREF TEMPARRAY I)))
                         (AREF TEMPARRAY I))))
           (FUNCALL (IF FORWARD-P 'STABLE-SORT 'REVERSE-STABLE-SORT)
                    TEMPARRAY 'MSG-POSITION-LESSP-CACHED)
           (DOTIMES (I (LENGTH TEMPARRAY))
             (SETF (AREF TEMPARRAY I)
                   (CADDR (AREF TEMPARRAY I)))))
          (T
           (FUNCALL (IF FORWARD-P 'STABLE-SORT 'REVERSE-STABLE-SORT)
                    TEMPARRAY MODE)))
    (COPY-ARRAY-CONTENTS TEMPARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))
  (AND (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
       (RESPLICE-ZMAIL-BUFFER ZMAIL-BUFFER))
  ;; Sorting modifies the buffer even though not any one message.
  (SETF (NODE-TICK ZMAIL-BUFFER) (TICK))
  (COND ((EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
         (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
         (ZMAIL-SELECT-MSG *MSG* NIL NIL))))

;;; Keep the status quo
(DEFUN MSG-NOOP-SORT-LESSP (MSG-1 MSG-2)
  MSG-1 MSG-2
  NIL)

(DEFUN MSG-DATE-SORT-LESSP (MSG-1 MSG-2)
  (< (OR (MSG-GET MSG-1 :DATE) -1) (OR (MSG-GET MSG-2 :DATE) -1)))

(DEFUN MSG-TO-STRING-LESSP (MSG-1 MSG-2)
  (MSG-RECIPIENT-LESSP :TO MSG-1 MSG-2))

(DEFUN MSG-FROM-STRING-LESSP (MSG-1 MSG-2)
  (MSG-RECIPIENT-LESSP :FROM MSG-1 MSG-2))

(DEFUN MSG-RECIPIENT-LESSP (TYPE MSG-1 MSG-2)
  (MSG-RECIPIENT-LESSP-1 (MSG-GET MSG-1 TYPE) (MSG-GET MSG-2 TYPE)))

(DEFUN MSG-RECIPIENT-LESSP-1 (FIELD-1 FIELD-2)
  (DO ((PL1) (PL2) (STR1) (STR2))
      (NIL)
    (COND ((NULL FIELD-1) (RETURN (NOT (NULL FIELD-2))))
          ((NULL FIELD-2) (RETURN NIL))
          ((NOT (STRING-EQUAL (SETQ STR1 (GET (SETQ PL1 (LOCF (CAR FIELD-1))) :NAME))
                              (SETQ STR2 (GET (SETQ PL2 (LOCF (CAR FIELD-2))) :NAME))))
           (RETURN (STRING-LESSP STR1 STR2)))
          ((NULL (SETQ STR1 (GET PL1 :HOST))) (RETURN (NOT (NULL (GET PL2 :HOST)))))
          ((NULL (SETQ STR2 (GET PL2 :HOST))) (RETURN NIL))
          ((NOT (STRING-EQUAL (SETQ STR1 (CAR STR1)) (SETQ STR2 (CAR STR2))))
           (RETURN (STRING-LESSP STR1 STR2))))
    (SETQ FIELD-1 (CDR FIELD-1)
          FIELD-2 (CDR FIELD-2))))

(DEFUN MSG-SUBJECT-STRING-LESSP (MSG-1 MSG-2 &AUX SUB1 SUB2)
  (SETQ SUB1 (MSG-GET MSG-1 :SUBJECT)
        SUB2 (MSG-GET MSG-2 :SUBJECT))
  (COND ((NULL SUB1) (NOT (NULL SUB2)))
        ((NULL SUB2) NIL)
        (T (STRING-LESSP SUB1 SUB2))))

(DEFUN MSG-KEYWORD-LESSP (MSG-1 MSG-2 &AUX KEY-1 KEY-2)
  (SETQ KEY-1 (MSG-GET MSG-1 'KEYWORDS-STRING)
        KEY-2 (MSG-GET MSG-2 'KEYWORDS-STRING))
  (COND ((NULL KEY-1) (NOT (NULL KEY-2)))
        ((NULL KEY-2) NIL)
        (T (STRING-LESSP KEY-1 KEY-2))))

(DEFUN MSG-TEXT-STRING-LESSP (MSG-1 MSG-2)
  (INTERVAL-LESSP (MSG-INTERVAL MSG-1) NIL T (MSG-INTERVAL MSG-2) NIL T))

(DEFUN MSG-LENGTH-LESSP (MSG-1 MSG-2)
  (< (COMPUTE-MSG-LENGTH MSG-1) (COMPUTE-MSG-LENGTH MSG-2)))

(DEFUN COMPUTE-MSG-LENGTH (MSG)
  (OR (MSG-GET MSG 'LENGTH)
      (LET ((LENGTH (COUNT-CHARS (MSG-INTERVAL MSG)) ))
        (MSG-PUT MSG LENGTH 'LENGTH)
        LENGTH)))

(DEFUN MSG-POSITION-LESSP (MSG-1 MSG-2 &AUX ZMAIL-BUFFER-1 ZMAIL-BUFFER-2)
  (SETQ ZMAIL-BUFFER-1 (MSG-MAIL-FILE-BUFFER MSG-1)
        ZMAIL-BUFFER-2 (MSG-MAIL-FILE-BUFFER MSG-2))
  (IF (NEQ ZMAIL-BUFFER-1 ZMAIL-BUFFER-2)
      (STRING-LESSP (ZMAIL-BUFFER-NAME ZMAIL-BUFFER-1) (ZMAIL-BUFFER-NAME ZMAIL-BUFFER-2))
      (DOMSGS (MSG ZMAIL-BUFFER-1)
        (COND ((EQ MSG MSG-1) (RETURN T))       ;Found first message, it is less
              ((EQ MSG MSG-2) (RETURN NIL))))))

;Here, each argument is not just a msg, but a list (MAIL-FILE POSITION-IN-IT MSG).
;Computing these lists in advance and then sorting them
;is much faster than sorting just the messages.
(DEFUN MSG-POSITION-LESSP-CACHED (MSG-1-AND-POSITION MSG-2-AND-POSITION)
  (IF (NEQ (CAR MSG-1-AND-POSITION) (CAR MSG-2-AND-POSITION))
      (STRING-LESSP (ZMAIL-BUFFER-NAME (CAR MSG-1-AND-POSITION))
                    (ZMAIL-BUFFER-NAME (CAR MSG-2-AND-POSITION)))
    (< (CADR MSG-1-AND-POSITION) (CADR MSG-2-AND-POSITION))))

(LOCAL-DECLARE ((SPECIAL SORT-GREATERP-PREDICATE))
(DEFUN REVERSE-STABLE-SORT (OBJECT SORT-GREATERP-PREDICATE)
  (STABLE-SORT OBJECT #'(LAMBDA (X Y) (FUNCALL SORT-GREATERP-PREDICATE Y X)))))

(DEFUN (COM-ZMAIL-MAIL ASSOCIATED-ALL-COMMAND) ()
  (CASE (CHOOSE-MAIL-MODE)
    (:FORWARD (COM-ZMAIL-FORWARD-ALL))
    (:REDISTRIBUTE (COM-ZMAIL-REDISTRIBUTE-ALL))
    (OTHERWISE (BARF "That command does not take a filter argument"))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-FORWARD-ALL
                                 "Forward all these messages to someone."
                                 ()
  (LET (*DRAFT-MSG* *DRAFT-TEXT-INTERVAL* *DRAFT-HEADER-INTERVAL*)
    (INITIALIZE-FOR-MAIL)
    (INSERT (INSERT-MOVING (WINDOW-POINT *DRAFT-HEADER-WINDOW*) "To: ") #/CR)
    (LET* ((ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
           (BP (INSERT-MSGS-INTO-WINDOW *DRAFT-TEXT-WINDOW* NIL (AREF ARRAY 0)))
           (STREAM (INTERVAL-STREAM-INTO-BP BP))
           (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
           (LIST NIL))
      (SEND STREAM :SET-BP (INTERVAL-FIRST-BP *DRAFT-TEXT-INTERVAL*))
      (FORMAT STREAM *FORWARDED-MESSAGE-BEGIN* NMSGS)
      (SEND STREAM :FRESH-LINE)
      (DO ((I 1 (1+ I))
           (MSG))
          (( I NMSGS))
        (SETQ MSG (AREF ARRAY I))
        (PUSH MSG LIST)
        (SEND STREAM :SET-BP BP)
        (FORMAT STREAM *FORWARDED-MESSAGE-SEPARATOR* I)
        (SEND STREAM :TYO #/CR)
        (MOVE-BP BP (INSERT-INTERVAL (SEND STREAM :READ-BP) (MSG-INTERVAL MSG))))
      (SETF (DRAFT-MSG-MSGS-BEING-FORWARDED *DRAFT-MSG*) (NREVERSE LIST))
      (SEND STREAM :SET-BP BP)
      (FORMAT STREAM *FORWARDED-MESSAGE-END* NMSGS)
      (SEND STREAM :FRESH-LINE)
      (MOVE-BP BP (SEND STREAM :READ-BP)))
    (ZMAIL-MAIL :MAIL :HEADER)))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-REPLY-ALL
                                               (STRING &OPTIONAL RECURSIVE)
  (OR RECURSIVE (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'NORMAL-REPLY NIL T))
  (STRING-NCONC STRING "Reply to these messages: "
                (GET 'NORMAL-REPLY :WHO-LINE-DOCUMENTATION)))

(DEFPROP COM-ZMAIL-REPLY COM-ZMAIL-REPLY-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REPLY-ALL
                                "Reply to all these messages at once."
                                ()
  (LET (*DRAFT-MSG* *DRAFT-TEXT-INTERVAL* *DRAFT-HEADER-INTERVAL*)
    (MULTIPLE-VALUE-BIND (MODE STARTING-WINDOW)
        (APPLY #'NORMAL-REPLY (LISTARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*)))
      (WHEN MODE
        (MOVE-BP (WINDOW-POINT *DRAFT-HEADER-WINDOW*)
                 (INTERVAL-FIRST-BP (WINDOW-INTERVAL *DRAFT-HEADER-WINDOW*)))
        ;; Avoid an extra blank line at end of headers.
        (WHEN (= (BP-CHAR-BEFORE (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*)) #\RETURN)
          (DELETE-INTERVAL (FORWARD-CHAR (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) -1)
                           (INTERVAL-LAST-BP *DRAFT-HEADER-INTERVAL*) T))
        (WHEN *DEFAULT-REPLY-TEMPLATE*
          (FUNCALL *DEFAULT-REPLY-TEMPLATE* *DRAFT-MSG*
                   (DRAFT-MSG-MSGS-BEING-REPLIED-TO *DRAFT-MSG*)))
        (ZMAIL-MAIL MODE STARTING-WINDOW)))))

;;; This might be useful enough to be someplace
(DEFUN APPLY-ARRAY (FUNCTION ARRAY &AUX LEN)
  (SETQ LEN (ARRAY-ACTIVE-LENGTH ARRAY))
  (%OPEN-CALL-BLOCK FUNCTION 0 4)               ;No ADI, D-RETURN
  (%ASSURE-PDL-ROOM LEN)
  (DO ((I 0 (1+ I))) (( I LEN))
    (%PUSH (AREF ARRAY I)))
  (%ACTIVATE-OPEN-CALL-BLOCK))

(DEFPROP COM-ZMAIL-CONCATENATE COM-ZMAIL-CONCATENATE-ALL ASSOCIATED-ALL-COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-CONCATENATE-ALL "Append these messages together.
The text of the first message becomes the concatenation of the text of all messages.
All but resultant first message are then marked as deleted." ()
  (LET* ((ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
         (FIRST-MSG (AREF ARRAY 0))
         (BP (MSG-END-BP FIRST-MSG)))
    (UNCACHE-MSG-REFERENCES FIRST-MSG)
    (DO ((I 1 (1+ I))
         (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
         (MSG))
        (( I NMSGS))
      (SETQ MSG (AREF ARRAY I))
      (INSERT BP #/CR)
      (INSERT-INTERVAL BP (MSG-INTERVAL MSG))
      (MSG-PUT MSG T 'DELETED))
    (ZMAIL-SELECT-MSG FIRST-MSG)
    (UPDATE-MSG-SUMMARY-LINE FIRST-MSG :SIZE))
  DIS-TEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-CONCATENATE
                                 "Append the current message to another message"
                                 ()
  (ZMAIL-CONCATENATE-MSG *MSG* T))

(DEFUN ZMAIL-CONCATENATE-MSG (MSG &OPTIONAL CHOOSE-P &AUX OTHER-MSG)
  (SETQ OTHER-MSG (IF (OR CHOOSE-P (EQ *ZMAIL-COMMAND-BUTTON* :RIGHT))
                      (CHOOSE-MSG-FROM-SUMMARY "another message")
                      *MSG*))
  (CONCATENATE-MSG-TO-MSG MSG OTHER-MSG)
  DIS-NONE)

(DEFUN CONCATENATE-MSG-TO-MSG (MSG OTHER-MSG)
  (AND (EQ MSG OTHER-MSG) (BARF "Cannot concatenate message to itself"))
  (UNCACHE-MSG-REFERENCES MSG)
  (LET ((BP (MSG-END-BP OTHER-MSG)))
    (INSERT BP #/CR)
    (INSERT-INTERVAL BP (MSG-INTERVAL MSG)))
  (UPDATE-MSG-SUMMARY-LINE OTHER-MSG :SIZE)
  (ZMAIL-DELETE-MSG MSG))

(DEFUN ZMAIL-DELETE-MSG (MSG)
  (IF (EQ MSG *MSG*)
      (MUST-REDISPLAY *WINDOW*
                      (LET ((*ZMAIL-COMMAND-BUTTON* :KBD)
                            (*NUMERIC-ARG-P* NIL))
                        (COM-ZMAIL-DELETE)))
      (MSG-PUT MSG T 'DELETED)))

(DEFUN ZMAIL-UNDELETE-MSG (MSG)
  (MSG-PUT MSG NIL 'DELETED)
  (IF (EQ MSG *MSG*)
      (ZMAIL-SELECT-MSG *MSG* NIL NIL)
      DIS-NONE))

(DEFUN CHOOSE-MSG-FROM-SUMMARY (PROMPT)
  (OR (SEND *SUMMARY-WINDOW* :EXPOSED-P)
      (BARF "Summary is not visible"))
  (AND (OR (NULL *ZMAIL-BUFFER*)
           (ZEROP (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*)))
       (BARF "There are no messages to choose from"))
  (PROMPT-LINE "Select ~A with the mouse, or type any character to abort" PROMPT)
  (LET ((CH (SEND *STANDARD-INPUT* :ANY-TYI)))
    (OR (AND (CONSP CH) (EQ (CAR CH) 'SUMMARY-MOUSE))
        (BARF))
    (CADADR CH)))

;;; Undigestify

;; These macros are local to ZMAIL-UNDIGESTIFY-MSG-TO-MSGS
(DEFCONSTANT DASHES-AND-BLANK "-----------------------------215215") ; Awkward in code
(DEFSUBST UM-SEARCH-FOR-SEPARATOR (&OPTIONAL (CURRENT-BP CURRENT-BP))
  (ZWEI-SEARCH CURRENT-BP DASHES-AND-BLANK () () () END-BP))


(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-UNDIGESTIFY-MESSAGE
                                "Undigestify the current message." () ; Not interesting
  (LET ((RESULT (ZMAIL-UNDIGESTIFY-MSG-TO-MSGS *MSG* (MSG-MAIL-FILE-BUFFER *MSG*))))
    (COND (RESULT (FORMAT *QUERY-IO* "Split into ~D messages." RESULT) DIS-TEXT)
          (T (BARF "This message doesn't seem to be a digest.") DIS-NONE))))

(DEFUN ZMAIL-UNDIGESTIFY-MSG-TO-MSGS
       (MSG BUFFER &AUX (INT (MSG-INTERVAL MSG)) (END-BP (INTERVAL-LAST-BP INT))
                        TO-INSERT NEW-MSG (CURRENT-BP (INTERVAL-FIRST-BP INT))
                        (IDX (LOCATE-MSG-IN-ZMAIL-BUFFER MSG BUFFER))
                        (DIGEST-NAME (MSG-GET MSG :SUBJECT))
                        TO-INSERT-PROP
                        (N-MSGS 0) FOUND) ; A BP of success
  "Returns either (), if MSG doesn't seem to be a digest, or how many messages
MSG has been split into."
  ;; Search for the end of the header and contents.
  (LOCK-ZMAIL-BUFFER (BUFFER)
    (SETQ FOUND (UM-SEARCH-FOR-SEPARATOR))
    (WHEN (AND FOUND (BP-IN-INTERVAL-P (UM-SEARCH-FOR-SEPARATOR FOUND) INT))
      (SETQ TO-INSERT (OR (MSG-GET MSG (SETQ TO-INSERT-PROP :REPLY-TO))
                          (MSG-GET MSG (SETQ TO-INSERT-PROP :TO))))
      ;; Now to grovel through the rest of the interval
      ;; Delete the body of text (this applies to MSG)
      (IF *CLIP-UNDIGESTIFIED-MESSAGE*  (DELETE-INTERVAL FOUND END-BP T))
      (LET ((*BATCH-UNDO-SAVE* T))
        (DO-FOREVER
          (SETQ CURRENT-BP FOUND)
          (SETQ FOUND (UM-SEARCH-FOR-SEPARATOR))
          (IF (NOT FOUND) (RETURN))                     ; Might want to do something else later.
          (INCF IDX) (INCF N-MSGS)
          (SETQ NEW-MSG (MAKE-EMPTY-MSG))
          (INSERT-INTERVAL (MSG-END-BP NEW-MSG)
                           CURRENT-BP FOUND T)
          (SETQ NEW-MSG (SEND BUFFER :ADD-MSG NEW-MSG IDX))
          (ADD-HEADER-TO-MSG NEW-MSG TO-INSERT-PROP TO-INSERT)
          (IF *INHERIT-SUBJECT-FIELD*
              (ADD-HEADER-TO-MSG NEW-MSG :SUBJECT
                   (FORMAT () "~A [~A]" (MSG-GET NEW-MSG :SUBJECT) DIGEST-NAME)))
          (SETF (MSG-TICK NEW-MSG) (TICK))
          (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG NEW-MSG)))
      (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
      (IF *DELETE-UNDIGESTIFIED-MESSAGE* (ZMAIL-DELETE-MSG MSG))
      (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG MSG)
      N-MSGS)))

;;; Digestify support
;;;
;;; A digest file looks like:
;;; Culinary Snobs Digest
;;; :MAIL-ADDRESS CULINARY-SNOBS@OZ
;;; Other options are slots in the DIGEST-STATUS structure after the
;;; :MAIL-ADDRESS (except for the last four).
;;;
;;; A digest status file looks like:
;;; current-volume number (decimal)
;;; current-issue-number (decimal)
;;; volume issue date (date like 1/26/84 03:30:23)

(FS:DEFINE-CANONICAL-TYPE :DIGEST "DIGEST"
  (:UNIX "dg")
  (:VMS "DIG"))

(FS:DEFINE-CANONICAL-TYPE :DIGEST-STATUS "DSTATUS"
  (:UNIX "ds")
  (:VMS "DST")
  (:ITS "DSTAT"))

(DEFSTRUCT (DIGEST-STATUS (:CONC-NAME DS-) (:TYPE :LIST) (:ALTERANT NIL)
                          (:CALLABLE-CONSTRUCTORS T))
  ; All values are strings or NIL.  Some values default to the right thing
  NAME                  ; "Culinary Snobs Digest"
  FILE                  ; "OZ:PS:<JBEARD.CS>CS.DIGEST" (actually, a pathname object)
  STATUS-FILE           ; "OZ:PS:<JBEARD.CS>CD.DSTATUS" (actually, a pathname object)
  MAIL-ADDRESS          ; "CULINARY-SNOBS@OZ" (required)
  REPLY-TO              ; "CULINARY-SNOBS@OZ" or ()
  MODERATOR-NAME        ; "The Moderator" or () means personal name
  MODERATOR-ADDRESS     ; "CULINARY-SNOBS-REQUEST@OZ"
  FCC                   ; FCC: for ZMAIL or ()
  CURRENT-VOLUME        ; When composing
  CURRENT-NUMBER        ; When composing
  CURRENT-TOPICS        ; A list of strings (will be centered by command)
  DRAFT-STATUS          ; One of {NIL (haven't begun), :COMPOSING}
  )

(DEFPROP :MAIL-ADDRESS T DS-ADDRESS-SLOT)
(DEFPROP :REPLY-TO T DS-ADDRESS-SLOT)
(DEFPROP :MODERATOR-ADDRESS T DS-ADDRESS-SLOT)

(DEFVAR *DIGEST-FILE-ALIST* () "An alist of (digest-name digest-file)")
(DEFVAR *DIGESTS* () "A list of digest descriptions.")

(DEFVAR *DIGEST-TOPIC-HEADER* "Today's topics:")

(DEFUN GET-DIGEST-DESCRIPTION-FILE (DIGEST-NAME &AUX FILE)
  "Usually the type field is not specified."
  (AND (SETQ FILE (ASS #'STRING-EQUAL DIGEST-NAME *DIGEST-FILE-ALIST*))
       (SEND (FS:PARSE-PATHNAME FILE) :NEW-CANONICAL-TYPE :STATUS)))

(DEFSUBST DS-STATUS-FILE (DS) (SEND (DS-FILE DS) :NEW-CANONICAL-TYPE :DIGEST-STATUS))

(DEFSUBST TRIM-WHITESPACE (STRING) (STRING-TRIM '(#/Space #/Tab) STRING))

(DEFUN READ-DIGEST-DESCRIPTION (F &AUX MAKE-DS-ARGS)
  (WITH-OPEN-FILE (S F :DIRECTION :INPUT)
    (SETQ MAKE-DS-ARGS (LIST* :NAME (TRIM-WHITESPACE (SEND S :LINE-IN))
                              :FILE F
                              :STATUS-FILE (SEND F :NEW-CANONICAL-TYPE :DIGEST-STATUS)
                              ())) ; NCONC more args here
    (DO ((KEY (READ S 'EOF) (READ S 'EOF))
         VAL)
        ((EQ KEY 'EOF))
      (IF (STRING-EQUAL (SETQ VAL (TRIM-WHITESPACE (SEND S :LINE-IN))) "")
          (SETQ VAL ()))
      (AND VAL (GET KEY 'DS-ADDRESS-SLOT) (SETQ VAL (PARSE-ADDRESSES VAL)))
      (NCONC MAKE-DS-ARGS (LIST* KEY VAL ()))))
  (LET* ((DS (APPLY #'MAKE-DIGEST-STATUS MAKE-DS-ARGS)))
    (UNLESS (DS-REPLY-TO DS) (SETF (DS-REPLY-TO DS) (DS-MAIL-ADDRESS DS)))
    (UNLESS (DS-MODERATOR-NAME DS)
      (SETF (DS-MODERATOR-NAME DS) FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST))
    (UNLESS (DS-MODERATOR-ADDRESS DS)
      (SETF (DS-MODERATOR-ADDRESS DS)
            (OR (DS-REPLY-TO DS)
                (PARSE-ADDRESSES (FORMAT () "~A@~A" USER-ID FS:USER-LOGIN-MACHINE)))))
    DS))

(DEFUN DS-READ-LAST-EDITION (DS)
  (WITH-OPEN-FILE-CASE (S (DS-STATUS-FILE DS) :DIRECTION :INPUT)
    (FS:FILE-NOT-FOUND
     (SETF (DS-CURRENT-VOLUME DS) 1)
     (SETF (DS-CURRENT-NUMBER DS) 0))
    (SETF (DS-CURRENT-VOLUME DS) (PARSE-NUMBER (SEND S :LINE-IN) 0 () 10.))
    (SETF (DS-CURRENT-NUMBER DS) (PARSE-NUMBER (SEND S :LINE-IN) 0 () 10.))))

(DEFUN MAKE-DIGEST-DRAFT-MSG (DS MSGS
                              &AUX (DRAFT (MAKE-MSG))
                                   (INUM (1+ (DS-CURRENT-NUMBER DS)))
                                   TEXT-INTERVAL)
  "Make a draft msg for the digest DS out of MSGS."
  (MSG-PUT DRAFT :TO (DS-MAIL-ADDRESS DS))
  (MSG-PUT DRAFT :FROM (DS-MODERATOR-ADDRESS DS)) ; check this out later
  (MSG-PUT DRAFT :SUBJECT (FORMAT T "~A Volume ~D No. ~D"
                                  (DS-NAME DS) (DS-CURRENT-VOLUME DS) INUM))
  (MSG-PUT DRAFT :REPLY-TO (DS-REPLY-TO DRAFT))
  (AND (DS-FCC DS) (MSG-PUT DRAFT :FCC (DS-FCC DS)))
  (SETQ TEXT-INTERVAL (CREATE-INTERVAL))
  (WITH-OPEN-STREAM (S (INTERVAL-STREAM TEXT-INTERVAL () () () T))
    (SEND S :LINE-OUT *DIGEST-TOPIC-HEADER*)
    (SEND S :LINE-OUT "")
    (DOLIST (MSG MSGS)
      (LET ((SUBJECT (MSG-GET MSG :SUBJECT)))
        (DOTIMES (I (MAX 0 (- 38. (// (STRING-LENGTH SUBJECT) 2))))
          (SEND S :TYO #/Space))
        (SEND S :LINE-OUT SUBJECT)))
    (SEND S :LINE-OUT "")
    (DOTIMES (I 76.) (SEND S :TYO #/-))
    (SEND S :LINE-OUT "")
    (SEND S :LINE-OUT ""))
  ;;; Add in text of messages
  (DOLIST (MSG MSGS)
    )
  ;;; Make this MSG into a DRAFT-MSG and return
  )

(DEFUN DIGEST-DESCRIPTION (DIGEST-NAME &AUX F)
  "Returns either () (no such digest) or a digest status object.
A non-() value implies that the object can be found on *DIGESTS*"
  (OR (ASS #'STRING-EQUAL DIGEST-NAME *DIGESTS*)
      (WHEN (SETQ F (GET-DIGEST-DESCRIPTION-FILE DIGEST-NAME))
        (LET ((DS (READ-DIGEST-DESCRIPTION F)))
          (PUSH DS *DIGESTS*)
          DS))))


;;; Conversation/Reference stuff
(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SELECT-REFERENCES
                                "Select messages which this message refers to.
Makes a new temporary buffer of all the messages referred to directly
or indirectly by the selected message, including the selected message,
and selects that buffer." ()
  (OR (MSG-REFERENCES *MSG*)
      (BARF "Cannot find any message references in this message."))
  (SELECT-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER-OF-REFERENCES *MSG* T NIL)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SELECT-CONVERSATION-BY-REFERENCES
  "Select messages which this message refers to or which refer to it.
Makes a new temporary buffer containing the entire conversation
which contains the selected message, and selects that buffer."
 ()
  (SELECT-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER-OF-REFERENCES *MSG* T T)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SELECT-REFERENCED-MSG
                                "Select the message this message refers to." ()
  (SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER (FIND-MSG-FROM-CURRENT-REFERENCES)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE-REFERENCED-MSGS
                                "Delete the referenced messages.
Deletes all messages referenced directly or indirectly by the selected message,
This includes the selected message itself." ()
  (LOOP FOR MSG BEING THE MSGS IN (MAKE-ZMAIL-BUFFER-OF-REFERENCES *MSG* T NIL)
        DO (ZMAIL-DELETE-MSG MSG))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-DELETE-CONVERSATION-BY-REFERENCES
  "Delete the conversation containing the selected message.
This is all messages which this message refers to or which refer to it,
either directly or indirectly.  This includes the selected message." ()
  (LOOP FOR MSG BEING THE MSGS IN (MAKE-ZMAIL-BUFFER-OF-REFERENCES *MSG* T T)
        DO (ZMAIL-DELETE-MSG MSG))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-APPEND-TO-REFERENCED-MSG
                                "Append this message into the referenced message" ()
  (LET ((MSG (FIND-MSG-FROM-CURRENT-REFERENCES)))
    (CONCATENATE-MSG-TO-MSG *MSG* MSG)
    (FORMAT *QUERY-IO* "~&Appended to message in ~A"
            (SEND (MSG-MAIL-FILE-BUFFER MSG) :NAME)))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-MOVE-IN-PLACE-OF-REFERENCED-MSG
                                "Move this message where referenced message is.
A message is found in the specified universe as referenced by this message.
This message is then moved into the mail file that message occupies in the
same place and that message deleted." ()
  (LET* ((MSG (FIND-MSG-FROM-CURRENT-REFERENCES NIL))
         (ZMAIL-BUFFER (MSG-MAIL-FILE-BUFFER MSG))
         (INDEX (MSG-IN-ZMAIL-BUFFER-P MSG ZMAIL-BUFFER)))
    (MSG-PUT MSG T 'DELETED)
    (SEND ZMAIL-BUFFER :ADD-MSG *MSG* INDEX)
    (FORMAT *QUERY-IO* "~&Moved to ~A" (SEND ZMAIL-BUFFER :NAME)))
  DIS-NONE)

;;; ASK-FOR-UNIVERSE means give the menu before bothering to look in current mail file.
(DEFUN FIND-MSG-FROM-CURRENT-REFERENCES (&OPTIONAL (TRY-HERE-FIRST-P (NOT *NUMERIC-ARG-P*))
                                         &AUX REFS)
  (OR (SETQ REFS (MSG-REFERENCES *MSG*))
      (BARF "Cannot find any message references in this message"))
  (OR (FIND-MSG-FROM-REFERENCE (CAR REFS) *MSG* TRY-HERE-FIRST-P)
      (BARF "Cannot find ~A" (STRING-FOR-MSG-REFERENCE (CAR REFS)))))

(DEFUN STRING-FOR-MSG-REFERENCE (REF)
  (WITH-OUTPUT-TO-STRING (STREAM)
    (PRINT-REFERENCE STREAM REF NIL)))

(DEFUN FIND-MSG-FROM-REFERENCE (REF MSG &OPTIONAL (TRY-HERE-FIRST-P T))
  (COND ((AND TRY-HERE-FIRST-P
              (FIND-MSG-FROM-REFERENCE-IN-UNIVERSE
                REF #'MAP-OVER-SINGLE-ZMAIL-BUFFER *ZMAIL-BUFFER*)))
        ((LOOP FOR (FILTER . UNIVERSE) IN *FILTER-REFERENCE-UNIVERSE-ALIST*
               WHEN (MSG-FITS-FILTER-P MSG FILTER)
               THEREIS (FIND-MSG-FROM-REFERENCE-IN-UNIVERSE
                         REF #'MAP-OVER-DEFINED-UNIVERSE UNIVERSE)))
        (T
         (MULTIPLE-VALUE-BIND (MAP-FUNCTION MAP-ARG)
             (GET-UNIVERSE-FUNCTION
               '(:MOUSE)
               (FORMAT NIL "Where is ~A?" (STRING-FOR-MSG-REFERENCE REF)))
           (AND MAP-FUNCTION
                (FIND-MSG-FROM-REFERENCE-IN-UNIVERSE REF MAP-FUNCTION MAP-ARG))))))

(DEFUN FIND-MSG-FROM-REFERENCE-IN-UNIVERSE (REF MAP-FUNCTION MAP-ARG)
  (IF (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
      (CAR (SEND MAP-ARG :REFERENCED-MSGS (LIST REF)))
    (FIND-MSG-FROM-FILTER MAP-FUNCTION MAP-ARG #'MSG-REFERENCE-EQUAL REF)))

(DEFMACRO ADD-OTHER-COMMANDS (&REST COMMANDS)
  `(SETQ *OTHER-COMMAND-ALIST*
         (APPEND *OTHER-COMMAND-ALIST*
                 (MAKE-COMMAND-ALIST ',(COPYLIST COMMANDS)))))

(DEFINE-ZMAIL-GLOBAL *LAST-OTHER-MENU-ITEM* NIL)

;;; Let the user specify what this does if (s)he wants
(DEFINE-ZMAIL-GLOBAL *MIDDLE-OTHER-COMMAND* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-OTHER-COMMANDS (STRING)
  (FORMAT STRING "Execute auxiliary command: ~@[L: ~A; ~]~@[M: ~A; ~]R: menu."
          (CAR *LAST-OTHER-MENU-ITEM*)
          (NAME-FROM-MENU-VALUE *MIDDLE-OTHER-COMMAND* *OTHER-COMMAND-ALIST*)))

(ASSOCIATE-OPTION-WITH-COMMAND-DOCUMENTATION *MIDDLE-OTHER-COMMAND* COM-ZMAIL-OTHER-COMMANDS)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-OTHER-COMMANDS "Execute an auxiliary command.
Left defaults to last command used.  Middle is a user option.
Right gives a menu of these commands." (NO-ZMAIL-BUFFER-OK NUMERIC-ARG-OK)
  (FUNCALL (CHOOSE-OTHER-COMMAND)))

(DEFUN CHOOSE-OTHER-COMMAND (&AUX COMMAND)
  (OR *OTHER-COMMAND-ALIST* (BARF "No other commands"))
  (MULTIPLE-VALUE (COMMAND *LAST-OTHER-MENU-ITEM*)
    (ZMAIL-MENU-CHOOSE 'ZMAIL-MOMENTARY-COMMAND-MENU *OTHER-COMMAND-ALIST*
                       *LAST-OTHER-MENU-ITEM* NIL *MIDDLE-OTHER-COMMAND*))
  (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-OTHER-COMMANDS)
  COMMAND)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-VIEW-FILE
                                "View a specified file."
  (NO-ZMAIL-BUFFER-OK)
  (LET ((PATHNAME (WITH-BACKGROUND-PROCESS-LOCKED
                    (READ-DEFAULTED-PATHNAME
                      "View file:"
                      (COND ((AND *ZMAIL-BUFFER* (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*))
                             (BUFFER-PATHNAME *ZMAIL-BUFFER*))
                            (*PRIMARY-ZMAIL-BUFFER*
                             (BUFFER-PATHNAME *PRIMARY-ZMAIL-BUFFER*))
                            (T
                             (DEFAULT-PATHNAME)))))))
    (ZMAIL-VIEW-FILE PATHNAME))
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* FILE "View" ZMAIL-VIEW-FILE NIL
                          "View this file.")

(DEFUN ZMAIL-VIEW-FILE (PATHNAME)
  (WITH-OPEN-FILE (STREAM PATHNAME '(:IN))
    (USING-OVERLYING-WINDOW
      (SEND *OVERLYING-WINDOW* :VIEW-STREAM STREAM)))
  NIL)

(DEFINE-ZMAIL-GLOBAL *ZMAIL-KEYBOARD-MACROS* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-KEYBOARD-MACRO-ITEM* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-REPLAY-KEYBOARD-MACRO (STRING)
  (FORMAT STRING "Replay a keyboard macro:  ~@[L: ~A; ~]R: menu."
          *LAST-KEYBOARD-MACRO-ITEM*))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-REPLAY-KEYBOARD-MACRO "Replay a keyboard macro.
Left defaults to last keyboard macro used.
Right gives menu of possibilities." (NO-ZMAIL-BUFFER-OK)
  (OR (MEMQ :MACRO-EXECUTE (SEND *STANDARD-INPUT* :WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (LET (MACRO)
    (MULTIPLE-VALUE (MACRO *LAST-KEYBOARD-MACRO-ITEM*)
      (ZMAIL-MENU-CHOOSE NIL *ZMAIL-KEYBOARD-MACROS* *LAST-KEYBOARD-MACRO-ITEM*))
    (OR (AND MACRO
             (SETQ MACRO (GET MACRO 'MACRO-STREAM-MACRO)))
        (BARF))
    (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-REPLAY-KEYBOARD-MACRO)
    (SEND *STANDARD-INPUT* :MACRO-EXECUTE MACRO 1)))

(DEFMACRO ADD-ZMAIL-KEYBOARD-MACROS NAMES
  `(SETQ *ZMAIL-KEYBOARD-MACROS*
         (APPEND *ZMAIL-KEYBOARD-MACROS*
                 ',(MAPCAR #'(LAMBDA (X) (INTERN (GET-PNAME X) "")) NAMES))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-KILL-RING-SAVE-MSG
                                "Save the current message on the kill ring"
                                ()
  (KILL-RING-SAVE-INTERVAL (MSG-INTERVAL *MSG*))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-LIST-BUFFERS
                                "List the buffers in ZMAIL."
                                (NO-ZMAIL-BUFFER-OK)
  (FORMAT *TYPEOUT-WINDOW* "~& Name:                     Expunge Save Number of messages~%")
  (MULTIPLE-VALUE-BIND (MAIL-FILE-BUFFERS TEMP-ZMAIL-BUFFERS)
      (GET-ZMAIL-BUFFER-ALISTS)
    (LIST-ZMAIL-BUFFERS-INTERNAL MAIL-FILE-BUFFERS "File")
    (LIST-ZMAIL-BUFFERS-INTERNAL TEMP-ZMAIL-BUFFERS "Temporary")
    (DO ((LIST *OTHER-MAIL-FILE-NAMES* (CDR LIST))
         (FIRST-P T) (NAME))
        ((NULL LIST)
         (OR FIRST-P (SEND *TYPEOUT-WINDOW* :TYO #/CR)))
      (SETQ NAME (CAR LIST))
      (COND ((NOT (ASSOC NAME MAIL-FILE-BUFFERS))
             (COND (FIRST-P
                    (FORMAT *TYPEOUT-WINDOW* "~&Mail files not yet read in:~%")
                    (SETQ FIRST-P NIL)))
             (SEND *TYPEOUT-WINDOW* :FRESH-LINE)
             (SEND *TYPEOUT-WINDOW* :ITEM 'NONLOADED-MAIL-FILE NAME)
             (SEND *TYPEOUT-WINDOW* :TYO #/CR)))))
  DIS-NONE)

(DEFUN LIST-ZMAIL-BUFFERS-INTERNAL (LIST PROMPT)
  (COND (LIST
         (FORMAT *TYPEOUT-WINDOW* "~&~A buffers:~%" PROMPT)
         (DO L LIST (CDR L) (NULL L)
           (LET ((ZMAIL-BUFFER (CDAR L)))
             (MULTIPLE-VALUE-BIND (SAVE-P EXPUNGE-P)
                 (ZMAIL-BUFFER-SAVE-P ZMAIL-BUFFER)
               (SEND *TYPEOUT-WINDOW* :FRESH-LINE)
               (SEND *TYPEOUT-WINDOW* :ITEM 'ZMAIL-BUFFER ZMAIL-BUFFER "~A" (CAAR L))
               (FORMAT *TYPEOUT-WINDOW* "~30T~C~36T~C~46T~3D~%"
                       (IF EXPUNGE-P #/* #/SP) (IF SAVE-P #/* #/SP)
                       (ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER)))))
         (SEND *TYPEOUT-WINDOW* :TYO #/CR))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* NONLOADED-MAIL-FILE "Read in"
                          SELECT-ZMAIL-BUFFER-FROM-PATHNAME T
                          "Read in this file.")

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-ENABLE-BACKGROUND-PROCESS-WHEN-DEEXPOSED
                                "Allow the background process to run when deexposed"
                                (NO-ZMAIL-BUFFER-OK NUMERIC-ARG-OK)
  (FORMAT *QUERY-IO* "~&Background process ~:[en~;dis~]abled when deexposed"
          (SETQ *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED*
                (IF *NUMERIC-ARG-P*
                    (ZEROP *NUMERIC-ARG*)
                  (NOT *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED*))))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-DELETE-DUPLICATE-MSGS
                                "Delete any duplicate messages in the current buffer."
                                ()
  (LET ((HASH-TABLE (MAKE-EQUAL-HASH-TABLE :SIZE (TRUNCATE
                                                    (* (ZMAIL-BUFFER-NMSGS *ZMAIL-BUFFER*)
                                                       5)
                                                    4)))
        (NDELETED 0)
        OMSG)
    (DOMSGS (MSG *ZMAIL-BUFFER*)
      (COND ((AND (SETQ OMSG (SWAPHASH (MSG-HASH-ID MSG) MSG HASH-TABLE))
                  (NOT (MSG-GET OMSG 'DELETED)))
             (ZMAIL-DELETE-MSG OMSG)
             (INCF NDELETED))))
    (FORMAT *QUERY-IO* "~&~D message~:P deleted." NDELETED))
  DIS-NONE)

;;; This intentionally ignores things like Redistributed-by which might be different.
;;; I think this is probably more right
(DEFUN MSG-HASH-ID (MSG &AUX STATUS)
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (OR (GET STATUS 'HASH-ID)
      (LET ((HASH-ID (OR (GET STATUS :MESSAGE-ID)
                         (SOME-PLIST (CAR STATUS) '(:DATE :FROM :TO :CC :SUBJECT)))))
        (PUTPROP STATUS HASH-ID 'HASH-ID)
        HASH-ID)))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-SET-EXPIRATION-DATE
                                "Set the expiration date on this message."
                                ()
  (LET ((DATE (TYPEIN-LINE-READLINE "SET EXPIRATION DATE:")))
    (CONDITION-CASE (ERROR)
        (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME DATE))
      (ERROR (BARF ERROR)))
    (ADD-HEADER-TO-MSG *MSG* :EXPIRATION-DATE DATE))
  DIS-TEXT)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-WHOIS
                                "Print whois information on a user" (NO-ZMAIL-BUFFER-OK)
  (MULTIPLE-VALUE-BIND (NIL USER)
      (CHOOSE-OR-READLINE-ADDRESS "Whois" NIL T
                                  (LET ((FROM (AND *MSG* (CAR (MSG-GET *MSG* :FROM)))))
                                    (AND FROM (STRING-FROM-HEADER FROM :SHORT))))
    (CHAOS:WHOIS USER))
  DIS-NONE)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-SELECT-ARBITRARY-FORMAT-MAIL-FILE
                                "Read in a file and a specified format"
                                (NO-ZMAIL-BUFFER-OK)
  (SET-ZMAIL-USER)
  (SELECT-ARBITRARY-FORMAT-ZMAIL-BUFFER (READ-ZMAIL-BUFFER-FILENAME :MOUSE)))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* FILE "Arbitrary format"
                          SELECT-ARBITRARY-FORMAT-ZMAIL-BUFFER NIL
                          "Select this file, specifying the format.")

(DEFUN SELECT-ARBITRARY-FORMAT-ZMAIL-BUFFER (PATHNAME &AUX FLAVOR ZMAIL-BUFFER)
  (SETQ *ZMAIL-COMMAND-BUTTON* :RIGHT)          ;Always ask for flavor
  (SETQ FLAVOR (ZMAIL-MENU-CHOOSE NIL (LOOP FOR X IN *ZMAIL-BUFFER-FLAVOR-ALIST*
                                            WHEN (FUNCALL
                                                   (SI:GET-FLAVOR-HANDLER-FOR
                                                     (CDR X) :MAIL-FILE-REPARSABLE-P)
                                                   :MAIL-FILE-REPARSABLE-P)
                                            COLLECT X))
        ZMAIL-BUFFER (ZMAIL-FIND-FILE-NOSELECT PATHNAME '(:MOUSE) FLAVOR))
  (SELECT-ZMAIL-BUFFER ZMAIL-BUFFER))

(define-zmail-top-level-command com-view-original-header
                                "View the original header"
                                ()
  (stream-copy-until-eof
    (interval-stream (interval-first-bp (msg-real-interval *msg*))
                     (interval-first-bp (msg-interval *msg*)))
    *standard-output*)
  dis-none)

;; -*- Mode: Lisp; Base: 8.; Package: Hacks -*-

;; Alarm system for Lispms.
;; Written by Dave Andre in August 1981.
;; Additional features added by Howard Trachtman, 1982.

                                                ;future plans:
  ;;win more when error occurs (user can restart alarm)
  ;; Allow user to delete alarms easily.

  ;; provide much more user flexibility in things to be searched for.
  ;; allow memos which are not to be gotten until very far in the
  ;; future be mailed.

  ;; implement something like a PCAL, this may have be better suited
  ;; for zwei see sys:zmail;calender

  ;; maybe a mouse interface, but I don't know for what.

  ;; provide more flexibility in how often to search for something.
  ;; impliment some kind of mini-scheduler (or use the real one)

  ;look for lisp machines which are free (or not so free) and have a particular band on them.

  ;;   Can we have clock driven interrupts?
  ;; Provide some of the functionality of process-run-function

  ;;Merge mail and file checks.

  ;;usage from other machines, like notify when a lispm frees up.

  ;;force checking of particular alarms
  ;;look at bradst's stuff.

  ;(tv:notify-with-query)

  ;save up notifications.  use some kind of alarm-notify function.
 ;periodic notifications.

;; Make alarm go off when the status of somebody logged in somewhere changes.

;; Make alarm go off if the dover status changes.

  ;; I had some other ideas, but forgot them.  Suggestions welcome.


  ;;Documentation

;; The system is driven by the symbols on ALARM-LIST.
;; Any symbol on the list should have the following properties defined as functions:
;;
;;  CHECK:  This function is called periodically and should return a boolean which
;;  states whether an alarm condition exists.
;;
;;  NOTIFY &optional dont-notify-p:  This function is called if the CHECK function
;;  has notified the alarm process that an alarm condition exists.  This function
;;  should update its knowledge to state that the user has been notified, and unless
;;  dont-notify-p is true, should notify the user of the occurance.  If it can also
;;  be determined that an alarm condition will no longer exist, this function may
;;  remove its associated alarm from ALARM-LIST.
;;
;;  RESET:  This function should reset the alarm's knowledge to its initial state.
;;  For example, the MAIL alarm checks for mail to various people.  This function
;;  would then remove any people from the alarm's knowledge.
;;
;;  ADD-ALARM new-condition:  This function's argument uniquely specifies a condition
;;  to check for.  Calling this function adds this condition to the alarm's knowledge.


(DEFVAR ALARM-LIST NIL "A list of symbols which represnt which things we might ALARM about.")
(DEFVAR ALARM-LIST-LOCK NIL "Used internally to lock out the alarm process while we
are hacking up the alarm database.")

(DEFVAR ALARM-INHIBIT-NOTIFICATION-LIST NIL "This is a list of alarms which should not
signal a notification the next time they are checked.")

(DEFUN SET-ALARM-INHIBIT-NOTIFY (ALARM)
  "Inhibit an alarm from bothering the user the next time that event occurs."
  (WITHOUT-INTERRUPTS
    (OR (MEMQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST)
        (PUSH ALARM ALARM-INHIBIT-NOTIFICATION-LIST))))


(DEFVAR ALARM-SLEEP-TIME 7200. "The alarm process sleeps for this amount of time
(in 60ths of a second) between checking ALARM-LIST. Defaults to two minutes.")

(DEFVAR ALARM-PROCESS NIL "This variable keeps track of the actual alarm process.
 We normally have a lower priority than other processes.")

(DEFVAR ALARM-PROCESS-PRIORITY -1 "The priority of the alarm process.")

;;; Entry functions

(DEFUN ADD-ALARM (ALARM)
  "Add a new alarm to the list of alarms.  Don't call this yourself!"
  (CHECK-ARG ALARM
             (GET ALARM 'CHECK)
             "an alarm")
  (ACTIVATE-ALARM-PROCESS)
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
           (COND ((NOT (MEMQ ALARM ALARM-LIST))
                  (PUSH ALARM ALARM-LIST))))
    (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL)))

(DEFUN DELETE-ALARM (ALARM)
  "Remove an alarm from the list of alarms.  Don't call this yourself!"
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
           (FUNCALL (GET ALARM 'RESET))
           (SETQ ALARM-LIST (DELQ ALARM ALARM-LIST)))
    (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL)))

(DEFUN ACTIVATE-ALARM-PROCESS ()
  "Activate an alarm in the list of alarms.  Don't call this yourself!"
  (OR ALARM-PROCESS
      (SETQ ALARM-PROCESS
            (MAKE-PROCESS "Alarm Background" ':PRIORITY ALARM-PROCESS-PRIORITY)))
  (COND ((NULL (SI:PROCESS-RUN-REASONS ALARM-PROCESS))
         (SEND ALARM-PROCESS :PRESET 'ALARM-BACKGROUND-TOP-LEVEL)
         (PROCESS-ENABLE ALARM-PROCESS))))

(DEFUN DEACTIVATE-ALARM-PROCESS (&OPTIONAL RESET-ALARMS)
  "Deactivate an alarm in the list of alarms.  Don't call this yourself!"
  (COND ((NOT (NULL ALARM-PROCESS))
         (IF (EQ CURRENT-PROCESS ALARM-PROCESS)
             (PROCESS-RUN-FUNCTION "Alarm Temp" 'DEACTIVATE-ALARM-PROCESS)
             (COND (RESET-ALARMS
                    (SETQ ALARM-INHIBIT-NOTIFICATION-LIST NIL)
                    (DOLIST (ALARM ALARM-LIST)
                      (FUNCALL (GET ALARM 'RESET)))
                    (SETQ ALARM-LIST NIL)))
             (FUNCALL ALARM-PROCESS ':KILL)))))

(ADD-INITIALIZATION "Deactivate Alarms"
                    '(DEACTIVATE-ALARM-PROCESS T)
                    '(LOGOUT))

;;; Internal workings.

  ;better way?
(DEFMACRO DELETE-ELEMENT (N ELEMENT)
  `(SETF (NTHCDR ,N ,ELEMENT) (CDR (NTHCDR ,N ,ELEMENT))))

(DEFVAR *CURRENT-ALARM* NIL "Internal variable used from the alarm presently being checked.")

(DEFUN ALARM-BACKGROUND-TOP-LEVEL ()
  "Top level function of the alarm background process.  Is smart about internal errors."
  (ERROR-RESTART-LOOP ((SYS:ABORT ERROR) "Return to top level of ALARM-BACKGROUND.")
    (DO ((NOTIFICATION-QUEUE NIL NIL)) (NIL)
      (LET ((USER-ID USER-ID) VAL ERR)
        (AND (EQUAL USER-ID "")
             (SETQ USER-ID "Alarm-Background"))
        (UNWIND-PROTECT
          (PROGN (PROCESS-LOCK (LOCF ALARM-LIST-LOCK) NIL "Alarm Lock")
                 (CONDITION-BIND ((ERROR 'ALARM-CONDITION-HANDLER))
                   (DOLIST (*CURRENT-ALARM* ALARM-LIST)
                     (MULTIPLE-VALUE (VAL ERR)
                       (*CATCH 'ALARM (FUNCALL (GET *CURRENT-ALARM* 'CHECK))))
                     (COND (ERR (SETQ ALARM-LIST
                                      (DELQ *CURRENT-ALARM* ALARM-LIST))
                                (FUNCALL (GET *CURRENT-ALARM* 'RESET)))
                           (VAL (PUSH *CURRENT-ALARM* NOTIFICATION-QUEUE))))))
          (%STORE-CONDITIONAL (LOCF ALARM-LIST-LOCK) CURRENT-PROCESS NIL))
        (DOLIST (ALARM (NREVERSE NOTIFICATION-QUEUE))
          (COND ((MEMQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST)
                 (SETQ ALARM-INHIBIT-NOTIFICATION-LIST
                       (DELQ ALARM ALARM-INHIBIT-NOTIFICATION-LIST))
                 (FUNCALL (GET ALARM 'NOTIFY) T))
                (T (FUNCALL (GET ALARM 'NOTIFY)))))
        (PROCESS-SLEEP ALARM-SLEEP-TIME "Alarm Wait")
        (OR ALARM-LIST (DEACTIVATE-ALARM-PROCESS))))))

(DEFUN ALARM-CONDITION-HANDLER (CONDITION)
  "Notify the user of an error while processing an alarm."
  (TV:NOTIFY NIL
             (FORMAT NIL "Error in the alarm process while checking ~A alarms.
Removing ~:*~A from the active alarm list.
The error was: ~A" *CURRENT-ALARM* CONDITION))
  (*THROW 'ALARM NIL))

;;; Alarm definitions.

;; File checks.  Notifies whenever the INFO of a file on this list changes.
(DEFVAR FILES-TO-BE-MONITORED NIL "List of files to check for creation date changes.")
(DEFVAR FILES-TO-BE-MONITORED-PREVIOUS-PLIST NIL "Plist of info about FILES-TO-BE-MONITERED.")
(DEFVAR FILES-TO-BE-NOTIFIED "List of files which for we will notify when they change.")

(DEFUN PLIST-INFO (PLIST)
  "Returns a cons which returns useful info about the creation date of a file."
  (CONS (GET PLIST ':TRUENAME) (GET PLIST ':CREATION-DATE)))

(DEFUN (FILE CHECK) ()
  (LET ((PLIST (FS:MULTIPLE-FILE-PLISTS FILES-TO-BE-MONITORED)))
    (DOLIST (ENTRY PLIST)
      (COND ((NOT (EQUAL (PLIST-INFO (ASSOC (CAR ENTRY) FILES-TO-BE-MONITORED-PREVIOUS-PLIST))
                         (PLIST-INFO ENTRY)))
             (PUSH (LIST (CAR ENTRY) (GET ENTRY ':CREATION-DATE))
                   FILES-TO-BE-NOTIFIED))))
    (SETQ FILES-TO-BE-MONITORED-PREVIOUS-PLIST PLIST)
    FILES-TO-BE-NOTIFIED))

(DEFUN (FILE NOTIFY) (&OPTIONAL DONT-NOTIFY-P)
  (OR DONT-NOTIFY-P
      (DOLIST (ENTRY FILES-TO-BE-NOTIFIED)
        (LEXPR-FUNCALL 'TV:NOTIFY NIL "File ~A modified at ~\TIME\" ENTRY)))
  (SETQ FILES-TO-BE-NOTIFIED NIL))

(DEFUN (FILE PRINT) (STREAM &AUX (N 0))
  (FORMAT STREAM "~&FILE Alarms:")
  (IF (NULL FILES-TO-BE-MONITORED)
      (FORMAT STREAM "   There are no files being monitored.~%")
    (DOLIST (FILE FILES-TO-BE-MONITORED)
      (FORMAT STREAM "~%[~A] The file ~A is being monitored for changes." (INCF N) FILE))))

(DEFUN (FILE REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT N FILES-TO-BE-MONITORED)))

(DEFUN (FILE RESET) ()
  (SETQ FILES-TO-BE-MONITORED NIL
        FILES-TO-BE-MONITORED-PREVIOUS-PLIST NIL
        FILES-TO-BE-NOTIFIED NIL))

(DEFUN (FILE ADD-ALARM) (ALARM)
  (SETQ ALARM (FS:MERGE-PATHNAME-DEFAULTS ALARM))
  (OR (MEMQ ALARM FILES-TO-BE-MONITORED)
      (PUSH ALARM FILES-TO-BE-MONITORED)))

;; Mail checks.  Notifies whenever the mail file of a user on this list is updated, but not
;; if it's deleted.
(DEFVAR MAIL-CHECK-USERS NIL "List of info on which users we are searching for mail.
Each entry must be of the form (user host filename).")
(DEFVAR MAIL-CHECK-USERS-WITH-NEW-MAIL NIL "List of users' files that there is new mail for.")
(DEFVAR MAIL-CHECK-USERS-CREATION-DATE-ALIST NIL "An alist of info on the creation date
of the mail files of the people we care about.")
(DEFVAR MAIL-CHECK-USERS-AUTHOR-LIST NIL "List of last writer of the mail files.")

(DEFUN (MAIL CHECK) ()
  (DO ((U MAIL-CHECK-USERS (CDR U))
       (USER) (HOST) (FILENAME) (OLD-ENTRY) (PROBE) (CREATION-DATE))
      ((NULL U))
    (SETQ USER (CAAR U)
          HOST (CADAR U)
          FILENAME (CADDAR U)
          OLD-ENTRY (ASSOC (CAR U) MAIL-CHECK-USERS-CREATION-DATE-ALIST))
    (COND ((NOT (ERRORP (SETQ PROBE (OPEN FILENAME '(:PROBE)))))
           (SETQ CREATION-DATE (FUNCALL PROBE ':CREATION-DATE))
           (PUSH (OR (FUNCALL PROBE ':GET ':AUTHOR) "an unknown person")
                 MAIL-CHECK-USERS-AUTHOR-LIST)
           (COND ((OR (NULL (CDR OLD-ENTRY))
                      ( CREATION-DATE (CDR OLD-ENTRY)))
                  (IF OLD-ENTRY
                      (SETF (CDR OLD-ENTRY) CREATION-DATE)
                      (PUSH (SETQ OLD-ENTRY (CONS (CAR U) CREATION-DATE))
                            MAIL-CHECK-USERS-CREATION-DATE-ALIST))
                  (PUSH OLD-ENTRY MAIL-CHECK-USERS-WITH-NEW-MAIL))))
          (T (IF OLD-ENTRY (SETF (CDR OLD-ENTRY) NIL)))))
  MAIL-CHECK-USERS-WITH-NEW-MAIL)

(DEFUN (MAIL NOTIFY) (&OPTIONAL DONT-NOTIFY-P &AUX AUTHOR ENTRY PERSON)
  (OR DONT-NOTIFY-P
      (LOOP FOR N FROM 0 TO (1- (LENGTH MAIL-CHECK-USERS-WITH-NEW-MAIL))
            DOING
            (SETQ AUTHOR (NTH N MAIL-CHECK-USERS-AUTHOR-LIST))
            (SETQ ENTRY (NTH N MAIL-CHECK-USERS-WITH-NEW-MAIL))
            (SETQ PERSON (COND ((AND (EQUAL (CAAR ENTRY) USER-ID)
                                     (EQ (CADAR ENTRY) FS:USER-LOGIN-MACHINE)) ;Hosts are EQ.
                                "You have")
                               (T (FORMAT NIL "~A~:[@~A~] has"
                                          (CAAR ENTRY)
                                          (EQUAL (CADAR ENTRY) FS:USER-LOGIN-MACHINE)
                                          (CADAR ENTRY)))))
            (TV:NOTIFY NIL
                       (WITH-OUTPUT-TO-STRING (S)
                         (FORMAT S "~A new mail from ~A at " PERSON AUTHOR)
                         (TIME:PRINT-BRIEF-UNIVERSAL-TIME
                           (CDR (NTH N MAIL-CHECK-USERS-CREATION-DATE-ALIST)) S)))))
  (SETQ MAIL-CHECK-USERS-WITH-NEW-MAIL NIL)
  (SETQ MAIL-CHECK-USERS-AUTHOR-LIST NIL))

(DEFUN (MAIL PRINT) (STREAM &AUX (N 0))
  (FORMAT STREAM "~%MAIL Alarms:")
  (IF (NULL MAIL-CHECK-USERS)
      (FORMAT STREAM "  Nobody's mail file is being monitored.~%")
    (DOLIST (ENTRY MAIL-CHECK-USERS)
      (FORMAT STREAM "~%[~A] ~A's mail file ~A on host ~A is being monitored."
              (INCF N) (FIRST ENTRY) (THIRD ENTRY) (SEND (SECOND ENTRY) ':NAME)))))

(DEFUN (MAIL RESET) ()
  (SETQ MAIL-CHECK-USERS NIL
        MAIL-CHECK-USERS-CREATION-DATE-ALIST NIL
        MAIL-CHECK-USERS-AUTHOR-LIST NIL
        MAIL-CHECK-USERS-WITH-NEW-MAIL NIL))

(DEFUN (MAIL REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT N MAIL-CHECK-USERS)))

(DEFUN (MAIL ADD-ALARM) (ALARM)
  (PUSH ALARM MAIL-CHECK-USERS))

;; Make alarm go off at a certain time.
(DEFVAR ALARM-TIMES NIL
  "An alist of what times the alarms should go off.
Each entry is a list of the arguments provided from SET-ALARM:
TIME MESSAGE INTERVAL REPEAT-END-TIME ALSO-SHOW-MESSAGE-P FUNCTION ARGS")

(DEFUN (TIME CHECK) ()
  (> (TIME:GET-UNIVERSAL-TIME) (CAAR ALARM-TIMES)))

(DEFUN (TIME NOTIFY) (&OPTIONAL IGNORE &AUX REPEAT-ALARMS)
  (DO ((A ALARM-TIMES (CDR A)))
      ((NULL A) ;out of alarms
       (SETQ ALARM-TIMES NIL)
       (DELETE-ALARM 'TIME))
    ;Recontruct args as created by SET-ALARM
    (LET* ((ENTRY (CAR A))
           (TIME (FIRST ENTRY))
           (MESSAGE (SECOND ENTRY))
           (INTERVAL (THIRD ENTRY))
           (REPEAT-END-TIME (FOURTH ENTRY))
           (ALSO-SHOW-MESSAGE-P (FIFTH ENTRY))
           (FUNCTION (SIXTH ENTRY))
           (ARGS (SEVENTH ENTRY)))
    (COND (( (TIME:GET-UNIVERSAL-TIME) TIME) ;trigger an alarm
           (IF ALSO-SHOW-MESSAGE-P (TV:NOTIFY NIL MESSAGE))
           (COND ((NOT (NULL FUNCTION)) ;gotta call a function
                  (IF (NULL ARGS)
                      (FUNCALL FUNCTION)
                    (APPLY FUNCTION ARGS))))
           (COND ((AND (NOT (NULL INTERVAL)) ;repeatable alarm
                       (OR (NULL REPEAT-END-TIME) ;we always care
                           ( (+ TIME INTERVAL) REPEAT-END-TIME))
                       (PUSH (RPLACA ENTRY (+ TIME INTERVAL)) REPEAT-ALARMS))))) ;mung ENTRY
          (T
           (RETURN (SETQ ALARM-TIMES A)))))) ;out of alarms to notify
  (COND ((NOT (NULL REPEAT-ALARMS))  ;gotta add repeatable alarms
         (DOLIST (ALARM REPEAT-ALARMS)
           (FUNCALL (GET 'TIME 'ADD-ALARM) ALARM))))
  ALARM-TIMES) ;we used to return this, guess we still should

(DEFUN (TIME RESET) ()
  (SETQ ALARM-TIMES NIL))

(DEFUN (TIME PRINT) (STREAM &AUX (N 0))
  (FORMAT STREAM "~%TIME Alarms:")
  (IF (NULL ALARM-TIMES)
      (FORMAT STREAM "  You have no scheduled alarm notifications.~%")
    (DOLIST (ALARM ALARM-TIMES)
      (FORMAT STREAM "~%[~A] You have an alarm scheduled to go off at ~A.~%"
              (INCF N) (TIME:PRINT-BRIEF-UNIVERSAL-TIME (FIRST ALARM) NIL)))))
;more info should be given

(DEFUN (TIME REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT N ALARM-TIMES)))

(DEFUN (TIME ADD-ALARM) (ALARM)
  (WITHOUT-INTERRUPTS
    (SETQ ALARM-TIMES (SORTCAR (CONS ALARM ALARM-TIMES) #'<))))


;; Make alarm go off when the status of a specified host changes.
;; As far as this program is concerned, there are three possible statuses for hosts:
;; UP, DOWN, or going down in a certain amount of time.  The status on the alist is
;; therefore always UP, DOWN, or a universal time of a planned shutdown.
;; Unfortunately no code exists to check for planned shutdowns, but when it does,
;; the only thing which should have to be modified is the CHECK function.


(DEFVAR ALWAYS-NOTIFY-IF-HOST-NOT-UP T
  "This us a user variable, and it overrides the inhibit notification stuff.")

(DEFVAR HOSTS-TO-CHECK NIL
  "A list of chaos addresses of hosts to check for a change in their up or down state.")
(DEFVAR HOSTS-TO-CHECK-LOCK NIL "Used to lock out processing of the HOSTS alarm.")
(DEFVAR HOSTS-CURRENT-STATUS NIL "List of hosts for which we know their present status.")
(DEFVAR HOSTS-WITH-NEW-STATUS NIL "List of hosts which have just gone up or down.")

(DEFUN (HOSTS CHECK) (&AUX CONNECTIONS CURRENT-STATUS)
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF HOSTS-TO-CHECK-LOCK))
           (SETQ CONNECTIONS (MAKE-LIST (LENGTH HOSTS-TO-CHECK)))
           (CHAOS:ASSURE-ENABLED)
           (DO ((H HOSTS-TO-CHECK (CDR H))
                (C CONNECTIONS (CDR C)))
               ((NULL C))
             (SETF (CAR C) (CHAOS:OPEN-CONNECTION (CAR H) "STATUS" 1)))
           (SETQ HOSTS-TO-CHECK-LOCK NIL)
           ;; Wait a maximum of 5 seconds for the replys to come in.
           (PROCESS-WAIT-WITH-TIMEOUT "Host Status" 600.
             #'(LAMBDA (CONNS)
                 (DO ((C CONNS (CDR C)))
                     ((NULL C) T)
                   (AND (EQ (CHAOS:STATE (CAR C)) 'CHAOS:RFC-SENT-STATE)
                        (RETURN NIL))))
             CONNECTIONS)
           (DO ((C CONNECTIONS (CDR C)))
               ((NULL C))
             (SETQ CURRENT-STATUS (CDR (ASSQ (CHAOS:FOREIGN-ADDRESS (CAR C))
                                             HOSTS-CURRENT-STATUS)))
             (SELECTQ (CHAOS:STATE (CAR C))
               ((CHAOS:RFC-SENT-STATE CHAOS:HOST-DOWN-STATE)
                (COND ((NEQ CURRENT-STATUS 'DOWN)
                       (PUSH (CONS (CHAOS:FOREIGN-ADDRESS (CAR C)) 'DOWN)
                             HOSTS-WITH-NEW-STATUS))))
               (OTHERWISE
                (COND ((NEQ CURRENT-STATUS 'UP)
                       (PUSH (CONS (CHAOS:FOREIGN-ADDRESS (CAR C)) 'UP)
                             HOSTS-WITH-NEW-STATUS)))))))
    (%STORE-CONDITIONAL (LOCF HOSTS-TO-CHECK-LOCK) CURRENT-PROCESS NIL)
    (DOLIST (C CONNECTIONS)
      (CHAOS:REMOVE-CONN C)))
  HOSTS-WITH-NEW-STATUS)

 ;;macro!  (defun chaos:remove-connections ...   work with erros.

(DEFUN (HOSTS NOTIFY) (&OPTIONAL DONT-NOTIFY-P &AUX TEM)
  (DOLIST (H HOSTS-WITH-NEW-STATUS)
    (AND (OR (NOT DONT-NOTIFY-P)
             (AND ALWAYS-NOTIFY-IF-HOST-NOT-UP
                  (NEQ (CDR H) 'UP)))
         (TV:NOTIFY NIL
           (WITH-OUTPUT-TO-STRING (S)
             (FUNCALL S ':STRING-OUT (CHAOS:HOST-SHORT-NAME (CAR H)))
             (COND ((NUMBERP (CDR H))
                    (FUNCALL S ':STRING-OUT " is going down at ")
                    (TIME:PRINT-UNIVERSAL-TIME (CDR H) S))
                   (T (FUNCALL S ':STRING-OUT
                               (COND ((EQ (CDR H) 'UP) " is up.")
                                     (T " is down."))))))))
    (COND ((SETQ TEM (ASSOC (CAR H) HOSTS-CURRENT-STATUS))
           (RPLACD TEM (CDR H)))
          (T (PUSH H HOSTS-CURRENT-STATUS))))
  (SETQ HOSTS-WITH-NEW-STATUS NIL))

 (DEFUN (HOSTS RESET) ()
  (SETQ HOSTS-TO-CHECK NIL
        HOSTS-TO-CHECK-LOCK NIL
        HOSTS-CURRENT-STATUS NIL
        HOSTS-WITH-NEW-STATUS NIL))

(DEFUN (HOSTS PRINT) (STREAM &AUX (N 0))
  (FORMAT STREAM "~%HOSTS Alarms:")
  (IF (NULL HOSTS-TO-CHECK)
      (FORMAT STREAM "  You are not monitoring the status of any hosts.~%")
    (DOLIST (HOST HOSTS-TO-CHECK)
      (FORMAT STREAM "~%[~A] You will be notified if the status of host ~A changes."
              (INCF N) (SEND HOST ':NAME)))))

(DEFUN (HOSTS REMOVE-ALARM) (N)
  (WITHOUT-INTERRUPTS
    (DELETE-ELEMENT N HOSTS-TO-CHECK)))

(DEFUN (HOSTS ADD-ALARM) (NEW-ALARM &AUX HOST)
  (COND ((NUMBERP NEW-ALARM) (SETQ HOST NEW-ALARM))
        ((SETQ HOST (CHAOS:ADDRESS-PARSE NEW-ALARM)))
        (T (FERROR NIL "~S is not a known host." NEW-ALARM)))
  (UNWIND-PROTECT
    (PROGN (PROCESS-LOCK (LOCF HOSTS-TO-CHECK-LOCK))
           (OR (MEMQ HOST HOSTS-TO-CHECK)
               (PUSH HOST HOSTS-TO-CHECK)))
    (%STORE-CONDITIONAL (LOCF HOSTS-TO-CHECK-LOCK) CURRENT-PROCESS NIL)))

;; Make alarm go off when a Lispm frees up.
;; Here CHAOS:FINGER-ALL-LMS does all the work for us.
(DEFVAR FREE-LISPMS NIL  "List of free lisp machines.  Initially, NIL")
(DEFVAR NEW-FREE-LISPMS NIL "List of lisp machines which have just become free.")

(DEFUN (LISPM CHECK) ()
  (LET ((LISPMS (CHAOS:FINGER-ALL-LMS  'IGNORE NIL T)))
    (DOLIST (LISPM LISPMS)
      (COND ((NOT (MEMBER LISPM FREE-LISPMS))
             (PUSH LISPM NEW-FREE-LISPMS))))
    (SETQ FREE-LISPMS LISPMS)))

(DEFUN (LISPM NOTIFY) (&OPTIONAL DONT-NOTIFY-P)
  (DOLIST (LISPM NEW-FREE-LISPMS)
    (OR DONT-NOTIFY-P
        (TV:NOTIFY NIL "~A is free." LISPM)))
  (SETQ NEW-FREE-LISPMS NIL))

(DEFUN (LISPM PRINT) (STREAM)
  (FORMAT STREAM "~%LISPM Alarms: (unknown)"))

;; These properties are inappropriate, because this alarm only searches
;; for one type of alarm.
(DEFPROP LISPM IGNORE RESET)
(DEFPROP LISPM IGNORE NEW-ALARM)
(DEFPROP LISPM IGNORE REMOVE-ALARM) ;foo


;;; User interface functions.
;;; These are the functions that a user would normally call in an init file.
;;; Someday write a mouse interface.

(DEFF BACKGROUND-MAIL-CHECK 'BACKGROUND-CHECK-MAIL) ;humaness

(DEFUN BACKGROUND-CHECK-MAIL (&OPTIONAL (USER USER-ID)(HOST FS:USER-LOGIN-MACHINE)
                              FILENAME NOTIFY-INITIALLY-P)
"In a background process, check every so often to see if a particular
user at a particular host has new mail and who the mail is from.
With no arguments, it will check for your mail on the  host that
you logged into.  NOTIFY-INITIALLY-P if NIL will not bother to notify
you when new mail arrives, if T it will.  It defualts to T"
  (IF (NULL FILENAME) (SETQ FILENAME (SEND (FS:USER-HOMEDIR) ':NEW-MAIL-PATHNAME)))
  (SETQ HOST (SI:PARSE-HOST HOST))
  (FUNCALL (GET 'MAIL 'ADD-ALARM) (LIST USER HOST (FS:MERGE-PATHNAME-DEFAULTS FILENAME)))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'MAIL))
  (ADD-ALARM 'MAIL)
  T)

(DEFUN BACKGROUND-CHECK-FILES (NOTIFY-INITIALLY-P &REST FILES)
  "In a background process, notify when a file is modified."
  (DOLIST (FILE FILES)
    (FUNCALL (GET 'FILE 'ADD-ALARM) FILE))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'FILE))
  (ADD-ALARM 'FILE)
  T)

(DEFUN SET-ALARM (TIME &OPTIONAL (MESSAGE "It is now the time that you scheduled an alarm.")
                  (REPEAT-INTERVAL "never") REPEAT-END-TIME ALSO-SHOW-MESSAGE-P
                  FUNCTION &REST ARGS)
  "Set an alarm, so that you will be notified when the specified TIME arrives,
by having the string MESSAGE forcably displayed on your terminal.

If REPEAT-INTERVAL is specified, then repeat that alarm every time the amount of time
in REPEAT-INTERVAL passes.  Stop repeating the alarm after REPEAT-END-TIME, or
continue forever if REPEAT-END-TIME is NIL (the default).

If FUNCTION is supplied, the FUNCTION will be called with the arguments
of ARGS instead of you being notified of the MESSAGE, unless ALSO-SHOW-MESSAGE-P is T."
  (LET ((TIME (TIME:PARSE-UNIVERSAL-TIME TIME))
        (INTERVAL (TIME:PARSE-INTERVAL-OR-NEVER REPEAT-INTERVAL))
        (ARGS (COPYLIST ARGS)))
    (AND (NOT (NULL INTERVAL))
         (< (* 60. INTERVAL) ALARM-SLEEP-TIME) ;boy are you trying to lose1
         (SETQ INTERVAL (TRUNCATE ALARM-SLEEP-TIME 60.)) ;minutes vs. seconds
         (TV:NOTIFY NIL "Coercing INTERVAL to be ~A, the ALARM-SLEEP-TIME."
                    (TIME:PRINT-INTERVAL-OR-NEVER INTERVAL NIL)))
    (IF (NOT (NULL REPEAT-END-TIME))
        (SETQ REPEAT-END-TIME (TIME:PARSE-UNIVERSAL-TIME REPEAT-END-TIME)))
    (IF (NULL FUNCTION) (SETQ ALSO-SHOW-MESSAGE-P T))
    (FUNCALL (GET 'TIME 'ADD-ALARM)
             (LIST TIME MESSAGE INTERVAL REPEAT-END-TIME ALSO-SHOW-MESSAGE-P FUNCTION ARGS))
  (ADD-ALARM 'TIME)
  T))

;; By default, the background host stuff will always notify if the host is not up.
;; See the variable ALWAYS-NOTIFY-IF-HOST-NOT-UP.   It's a kludge, but...

(DEFUN BACKGROUND-CHECK-HOSTS (NOTIFY-INITIALLY-P &REST HOSTS)
  "Notify me when one of the hosts specified goes up or down."
  (DO ((H HOSTS (CDR H))) ((NULL H))
    (FUNCALL (GET 'HOSTS 'ADD-ALARM) (CAR H)))
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'HOSTS))
  (ADD-ALARM 'HOSTS)
  T)

;; Note that this also updates the variable FREE-LISPMS
(DEFUN CHECK-FREE-LISPMS (&OPTIONAL NOTIFY-INITIALLY-P)
"In a background process, check every so often a change
in free lisp machines.  With NOTIFY-INITALLY-P of T,
don't bother to notify me.  Defaults so that you
will be be notified."
  (OR NOTIFY-INITIALLY-P
      (SET-ALARM-INHIBIT-NOTIFY 'LISPM))
  (ADD-ALARM 'LISPM))

(DEFVAR ALARM-TYPE-LIST '(TIME FILE HOSTS LISPM MAIL) "A list of know types of alarm.")

(DEFUN PRINT-ALARM (&OPTIONAL ALARM (STREAM STANDARD-OUTPUT))
  "Display information about a particular alarm."
  (PRINT-ALARMS STREAM (LIST ALARM)))

(DEFUN PRINT-ALARMS (&OPTIONAL (STREAM STANDARD-OUTPUT) (ALARM-LIST ALARM-TYPE-LIST))
  "Display information on all alarms."
  (FORMAT STREAM "~%Alarms are checked every ~A.~%"
          (TIME:PRINT-INTERVAL-OR-NEVER (TRUNCATE ALARM-SLEEP-TIME 60.) NIL))
  (DOLIST (ALARM ALARM-TYPE-LIST)
    (FUNCALL (GET ALARM 'PRINT) STREAM)))

 ;;this function is dangerous!!
(DEFUN REMOVE-ALARM-INTERNAL (ALARM ALARM-NUMBER)
  "Remove a particular alarm those alarms to be checked."
  (FUNCALL (GET ALARM 'REMOVE-ALARM) ALARM-NUMBER))

  ;users want to call this

(DEFUN VIEW-ALARMS ()
  "Supply a menu with a list of alarms to delete."
  ;;;Lock alarm database first!!un
  (LET* ((ALIST (LOOP FOR ALARM IN ALARM-TYPE-LIST
                      COLLECT (LIST ALARM (STRING ALARM) '(:VIEW :REMOVE))))
         (BLIST (LOOP FOR THING IN '(:VIEW :REMOVE)
                      COLLECT (LIST THING (STRING-CAPITALIZE-WORDS (STRING THING)))))
         (RESPONSE (TV:MULTIPLE-CHOOSE "Operate on some alarms." ALIST BLIST)))
    (DOLIST (ELT RESPONSE)
      (LET ((ALARM (CAR ELT)))
        (IF (MEMQ ':VIEW ELT)
            (VIEW-ALARM ALARM))
        (IF (MEMQ ':REMOVE ELT)
            (REMOVE-ALARM ALARM))))))

(DEFUN VIEW-ALARM (ALARM)
  ALARM)

;;greatly improve the user interface

(DEFUN REMOVE-ALARM (&OPTIONAL ALARM ALARM-NUMBER CONFIRM)
  "Remove a specific alarm.  Asks the user for confirmation."
  ;;cond-every !
  (COND ((NULL ALARM)
         (FORMAT QUERY-IO "~%Please type in the name an alarm (or just return to quit).
Valid alarms are ~A." (PRINT-LIST ALARM-TYPE-LIST QUERY-IO))
         (SETQ ALARM (READLINE QUERY-IO))))
  (COND ((AND (NOT (NULL ALARM)) (NULL ALARM-NUMBER))
         (FORMAT QUERY-IO "Please type the number of the ~A alarm that you want to be rid of." ALARM)
         (SETQ ALARM-NUMBER (PARSE-NUMBER (READLINE QUERY-IO)))))
  (COND ((AND (NOT (NULL ALARM)) ( 0 ALARM-NUMBER))
         (IF (NULL CONFIRM)
             (SETQ CONFIRM (Y-OR-N-P
                             (FORMAT NIL "Do you really want to remove yourself of alarm number ~A?" ALARM-NUMBER))))
         (IF CONFIRM (REMOVE-ALARM-INTERNAL ALARM ALARM-NUMBER)))))


(DEFUN PRINT-LIST (LIST STREAM)
  "A simpler version of format:print list."
  (COND ((NULL LIST)
         (FORMAT STREAM "none"))
        ((= (LENGTH LIST) 1)
          (FORMAT STREAM "~A"))
        ((= (LENGTH LIST) 2)
         (FORMAT STREAM "~A and ~A" (FIRST LIST) (SECOND LIST)))
        (T
         (DOTIMES (N (1- (LENGTH LIST)))
           (FORMAT STREAM "~A, " (NTH N LIST)))
         (FORMAT STREAM "and ~A" (NTH (LENGTH LIST) LIST)))))

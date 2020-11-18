;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; PEEK -- displays status information about the Lisp Machine

;; Define the Peek display modes, and how to select them.

(DEFVAR *PEEK-DEFAULT-MODE-ALIST* NIL
  "This variable has one element for each Peek display mode.
Elements are added by calls to DEFINE-PEEK-MODE which appear in this file
and the other source files of PEEK, and are executed at load time.
Each element looks like:
/(character display-function menu-item-string no-updating-flag long-documentation).
That is also what the five arguments to DEFINE-PEEK-MODE look like.
The menu-item-string is a short phrase of documentation which is also
what appears in the menu proper.
The display function is what is called to update the display.
NO-UPDATING-FLAG is either T or NIL.  If T, it means simply call the
function once and leave what it prints on the screen.  Normally it is NIL,
which means that the updating function returns a set of scroll items.")

(DEFVAR *PEEK-MENU-ITEM-ALIST* NIL
  "This is the item alist for the command menu.
The first element of each element is the menu item string.
The third element is the corresponding element of *PEEK-DEFAULT-MODE-ALIST*.")

(DEFMACRO DEFINE-PEEK-MODE (FUNCTION CHARACTER DOCUMENTATION &OPTIONAL FUNCTION-P
                            &BODY LONG-DOCUMENTATION)   ;this is to get the indentation right
  (DECLARE (ARGLIST FUNCTION CHARACTER DOCUMENTATION &OPTIONAL FUNCTION-P LONG-DOCUMENTATION))
  `(DEFINE-PEEK-MODE-1 ',FUNCTION ,CHARACTER ,DOCUMENTATION ,FUNCTION-P ,(CAR LONG-DOCUMENTATION)))

(DEFUN DEFINE-PEEK-MODE-1 (FUNCTION CHARACTER DOCUMENTATION FUNCTION-P LONG-DOCUMENTATION)
;character lossage
  (IF (NUMBERP CHARACTER) (SETQ CHARACTER (INT-CHAR CHARACTER)))
  (WITHOUT-INTERRUPTS
    (SETQ *PEEK-DEFAULT-MODE-ALIST*
          (NCONC (DELQ (ASSQ CHARACTER *PEEK-DEFAULT-MODE-ALIST*) *PEEK-DEFAULT-MODE-ALIST*)
                 (NCONS (LIST CHARACTER FUNCTION DOCUMENTATION
                              FUNCTION-P LONG-DOCUMENTATION))))
    (SETQ *PEEK-MENU-ITEM-ALIST*
          (MAPCAR (LAMBDA (ELT)
                    `(,(THIRD ELT) :VALUE ,ELT
                      :DOCUMENTATION ,(OR (FIFTH ELT) "This PEEK mode is undocumented.")))
                  *PEEK-DEFAULT-MODE-ALIST*))))

;;; This is meant to be called inside the PEEK-TOP-LEVEL,
;;; and MODE should be a character and the first arg can be a numeric argument.
(DEFUN PEEK-SET-MODE (WINDOW MODE &REST ARGS &AUX OLD-MODE)
;character lossage
  (IF (NUMBERP MODE) (SETQ MODE (INT-CHAR MODE)))
  (WHEN (SETQ MODE (ASSQ MODE *PEEK-DEFAULT-MODE-ALIST*))
    (SETQ OLD-MODE (LABEL-STRING (SEND WINDOW :LABEL)))
    (SEND (SEND (SEND WINDOW :SUPERIOR) :GET-PANE 'MENU)
          :SET-HIGHLIGHTED-ITEMS
          (LIST (SYS:ASSOC-EQUALP (THIRD MODE) *PEEK-MENU-ITEM-ALIST*)))
    (SEND WINDOW :SET-LABEL (THIRD MODE))
    (IF (FOURTH MODE)
        ;; If you want to execute only once,
        (UNWIND-PROTECT
            (APPLY (SECOND MODE) WINDOW ARGS)
          ;; Then on exit restore the old mode in the menu and label.
          ;; Since probably the typeout window is going away now
          ;; and that old mode's data is reappearing.
          (PEEK-ASSURE-NO-TYPEOUT WINDOW)
          (DOLIST (ELT *PEEK-DEFAULT-MODE-ALIST*)
            (IF (EQUALP OLD-MODE (THIRD ELT))
                (RETURN (SETQ MODE ELT))))
          (SEND (SEND (SEND WINDOW :SUPERIOR) :GET-PANE 'MENU)
                :SET-HIGHLIGHTED-ITEMS
                (LIST (SYS:ASSOC-EQUALP (THIRD MODE) *PEEK-MENU-ITEM-ALIST*)))
          (SEND WINDOW :SET-LABEL (THIRD MODE)))
      ;; Here if we are entering a mode that really does update.
      ;; We stay in it semipermanently.
      (PEEK-ASSURE-NO-TYPEOUT WINDOW)
      (SEND WINDOW :SET-DISPLAY-ITEM (APPLY (SECOND MODE) ARGS)))
    T))

;;;; Windows for PEEK.

(DEFFLAVOR BASIC-PEEK ((NEEDS-REDISPLAY NIL))
   (SCROLL-MOUSE-MIXIN SCROLL-WINDOW-WITH-TYPEOUT FULL-SCREEN-HACK-MIXIN)
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :SAVE-BITS T
                       :LABEL "Peek"
                       :TRUNCATION T)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The actual peek window.  This has the capability
to display in a PEEK display mode."))

(DEFMETHOD (BASIC-PEEK :NAME-FOR-SELECTION) ()
  (STRING-APPEND "Peek: " (LABEL-STRING LABEL)))

(DEFFLAVOR PEEK-WINDOW () (PROCESS-MIXIN TV:INITIALLY-INVISIBLE-MIXIN BASIC-PEEK)
  (:DEFAULT-INIT-PLIST :PROCESS
                       '(PEEK-STANDALONE-TOP-LEVEL :SPECIAL-PDL-SIZE #o4000
                                                   :REGULAR-PDL-SIZE #o10000))
  (:DOCUMENTATION :COMBINATION "Peek window with a process.
Usable as a stand-alone window that does PEEK display,
but with no menu -- only keyboard commands will be available."))

(DEFUN PEEK-MOUSE-CLICK (ITEM LEADER-TO-COMPLEMENT)
  (DECLARE (:SELF-FLAVOR BASIC-PEEK))
  (SETQ NEEDS-REDISPLAY T)
  (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET LEADER-TO-COMPLEMENT))
        (NOT (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET LEADER-TO-COMPLEMENT)))))

;This is the top level function that runs in the process of a window of flavor PEEK.
(DEFUN PEEK-STANDALONE-TOP-LEVEL (WINDOW)
  (PEEK-TOP-LEVEL WINDOW #/HELP)
  (DO-FOREVER
    (DESELECT-AND-MAYBE-BURY-WINDOW (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS) :FIRST)
    (PEEK-TOP-LEVEL WINDOW NIL)))

;;; Peek frames that have command menus of Peek modes, as well as a peek-window
;;; to do the actual displaying in.
(DEFFLAVOR PEEK-FRAME ()
           (PROCESS-MIXIN
            FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
            BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER)
  (:DEFAULT-INIT-PLIST :SAVE-BITS :DELAYED :PROCESS ))

(DEFMETHOD (PEEK-FRAME :BEFORE :INIT) (INIT-PLIST)
  (SETQ PANES (LIST
                (IF (GETL INIT-PLIST '(:PROCESS))
                    `(PEEK PEEK-WINDOW :SAVE-BITS NIL :PROCESS ,(GET INIT-PLIST :PROCESS))
                    '(PEEK PEEK-WINDOW :SAVE-BITS NIL))
                `(MENU DYNAMIC-HIGHLIGHTING-COMMAND-MENU-PANE
                       :FONT-MAP ,(LIST FONTS:CPTFONT)  ;not the usual large menu font
                       :LABEL "Peek Modes"
                       :ITEM-LIST-POINTER *PEEK-MENU-ITEM-ALIST*)))
  (SETQ CONSTRAINTS `((MAIN . ((MENU PEEK)
                               ((MENU :ASK :PANE-SIZE))
                               ((PEEK :EVEN)))))))

(DEFMETHOD (PEEK-FRAME :AFTER :INIT) (IGNORE)
  (LET ((PANE (SEND SELF :GET-PANE 'PEEK)))
    (SEND SELF :SELECT-PANE PANE)
    (SETQ PROCESS (SEND PANE :PROCESS))))

(DEFMETHOD (PEEK-FRAME :BEFORE :EXPOSE) (&REST IGNORE)
  (OR EXPOSED-P
      (EQUAL *PEEK-MENU-ITEM-ALIST*
             (SEND (SEND SELF :GET-PANE 'MENU) :ITEM-LIST))
      (SEND SELF :SET-CONFIGURATION 'MAIN)))

(DEFFLAVOR DYNAMIC-HIGHLIGHTING-COMMAND-MENU-PANE ()
           (DYNAMIC-ITEM-LIST-MIXIN MENU-HIGHLIGHTING-MIXIN COMMAND-MENU))

(COMPILE-FLAVOR-METHODS PEEK-FRAME PEEK-WINDOW DYNAMIC-HIGHLIGHTING-COMMAND-MENU-PANE)

(DEFUN PEEK (&OPTIONAL INITIAL-MODE)
  "Select a new or old Peek window.  An argument sets the Peek display mode."
  (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'PEEK-FRAME)
  (IF INITIAL-MODE
      (SEND SELECTED-WINDOW :FORCE-KBD-INPUT
                            (TYPECASE INITIAL-MODE
                              (STRING (CHAR INITIAL-MODE 0))
                              (SYMBOL (CHAR (SYMBOL-NAME INITIAL-MODE) 0))
                              (T INITIAL-MODE))))
  (AWAIT-WINDOW-EXPOSURE))

(DEFVAR PEEK-SLEEP-TIME 120.
  "This is how long, in 60'ths of a second, to wait between updates of the screen in PEEK.")

;;; This is the command reading loop.
(DEFUN PEEK-TOP-LEVEL (WINDOW MODE)
  (COND-EVERY
    ((AND MODE (SYMBOLP MODE)) (SETQ MODE (SYMBOL-NAME MODE)))
    ((STRINGP MODE) (SETQ MODE (CHAR MODE 0)))
    ((CHARACTERP MODE) (SETQ MODE (CHAR-INT MODE)))
;character lossage
    ((NUMBERP MODE) (SEND WINDOW :FORCE-KBD-INPUT MODE)))
  (BLOCK PEEK
    (DO-FOREVER
      (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to PEEK command level.")
        (DO ((SLEEP-TIME PEEK-SLEEP-TIME)
             (WAKEUP-TIME (TIME-DIFFERENCE (TIME) (- PEEK-SLEEP-TIME)))
             (wakeup-time-passed nil)
             (*TERMINAL-IO* (SEND WINDOW :TYPEOUT-WINDOW))
             (ARG)
             (CHAR))
            (())
          (when (or (TIME-LESSP WAKEUP-TIME (TIME)) wakeup-time-passed)
            (SETQ WAKEUP-TIME (TIME-DIFFERENCE (TIME) (- SLEEP-TIME)))
            (setq wakeup-time-passed nil))
          (OR (= SLEEP-TIME 0)
              (PROCESS-WAIT "Peek Timeout or TYI"
                            (LAMBDA (TIME FLAG-LOC STREAM PEEK-WINDOW)
                                (if (SHEET-EXPOSED-P PEEK-WINDOW)
                                    (OR wakeup-time-passed
                                        (TIME-LESSP TIME (TIME))
                                        (CONTENTS FLAG-LOC)
                                        (SEND STREAM :LISTEN))
                                  ;;If sheet not exposed, notice when wakeup-time is passed
                                  ;;If we don't do this and sheet is deexposed for a long time, peek
                                  ;;stops redisplaying
                                  (when (time-lessp time (time))
                                    (setq wakeup-time-passed t)
                                    nil)))
                            WAKEUP-TIME
                            (LOCATE-IN-INSTANCE WINDOW 'NEEDS-REDISPLAY)
                            *TERMINAL-IO*
                            (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)))
          (DO ()
              ((PROGN (PEEK-ASSURE-NO-TYPEOUT WINDOW)
                      (NULL (SETQ CHAR (SEND *TERMINAL-IO* :ANY-TYI-NO-HANG)))))
            (COND-EVERY
              ((CONSP CHAR)
               ;; A special command (forced input, no doubt)
               (CASE (CAR CHAR)
                 (SUPDUP (SUPDUP (CADR CHAR)))
                 (SUPDUP:TELNET (TELNET (CADR CHAR)))
                 (QSEND (QSEND (CADR CHAR))
                        (SEND WINDOW :SET-NEEDS-REDISPLAY T)
                        (SEND *TERMINAL-IO* :MAKE-COMPLETE))
                 (EH (EH (CADR CHAR)))
                 (INSPECT (INSPECT (CADR CHAR)))
                 (DESCRIBE (DESCRIBE (CADR CHAR)))
                 (:eval (eval (cadr char)))
                 (:MENU (SETQ CHAR (FIRST (THIRD (SECOND CHAR)))))
                 (:mouse-button
                  (mouse-call-system-menu))
                 (OTHERWISE (BEEP)))
               (SETQ ARG NIL))
              ((NUMBERP CHAR)
               (SETQ CHAR (INT-CHAR CHAR)))
              ((CHARACTERP CHAR)
               ;; Standard character, either accumulate arg or select new mode
               (SETQ CHAR (CHAR-UPCASE CHAR))
               (IF (DIGIT-CHAR-P CHAR)
                   (SETQ ARG (+ (* 10. (OR ARG 0)) (DIGIT-CHAR-P CHAR)))
                 (IF (PEEK-SET-MODE WINDOW CHAR ARG)
                     (SETQ ARG NIL)
                   ;; Check for standard character assignments
                   (CASE CHAR
                     (#/HELP
                      (SEND *STANDARD-OUTPUT* :CLEAR-WINDOW)
                      (LET (INPUT)
                        (SETQ INPUT (SEND *STANDARD-INPUT* :LISTEN))
                        (UNLESS INPUT
                          (FORMAT T "The Peek program shows a continuously updating status display.
  There are several modes that display different status.
  Here is a list of modes.  Select a mode by typing the character
  or by clicking on the corresponding menu item.~2%"))
                        (DOLIST (E *PEEK-DEFAULT-MODE-ALIST*)
                          (WHEN (OR INPUT (SETQ INPUT (SEND *STANDARD-INPUT* :LISTEN)))
                            (RETURN))
                          (FORMAT T "~:@C~5T~A~%~@[~6T~A~%~]"
                                  (FIRST E) (THIRD E) (FIFTH E)))
                        (UNLESS INPUT
                          (FORMAT T "~%Q~5TQuit.~%")
                          (FORMAT T "nZ~5TSets sleep time between updates to n seconds.~2%")
                          (FORMAT T "[Help]  Prints this message.~2%")))
                      (SETQ ARG NIL))
                     (#/Q
                      (RETURN-FROM PEEK NIL))
                     (#/Z
                      (AND ARG (SETQ SLEEP-TIME (* 60. ARG)))
                      (SEND WINDOW :SET-NEEDS-REDISPLAY T)
                      (SETQ ARG NIL))
                     (#/SPACE (SEND WINDOW :SET-NEEDS-REDISPLAY T))
                     (OTHERWISE (BEEP))))))))
          (WHEN (OR (SEND WINDOW :NEEDS-REDISPLAY) (TIME-LESSP WAKEUP-TIME (TIME)))
            ;; We want to redisplay.  If have typeout, hang until user confirms.
            (SEND WINDOW :SET-NEEDS-REDISPLAY NIL)
            (SEND WINDOW :REDISPLAY)))))))

(DEFUN PEEK-ASSURE-NO-TYPEOUT (WINDOW)
  (WHEN (SEND (SETQ WINDOW (SEND WINDOW :TYPEOUT-WINDOW)) :INCOMPLETE-P)
    (FORMAT T "~&Type any character to flush:")
    (LET ((CHAR (SEND *TERMINAL-IO* :ANY-TYI)))
      (SEND WINDOW :MAKE-COMPLETE)
      (UNLESS (EQ CHAR #/SPACE)
        (SEND *TERMINAL-IO* :UNTYI CHAR)))))

;;;; Processes, meters

(DEFINE-PEEK-MODE PEEK-PROCESSES #/P "Active Processes" NIL
  "List status of every process -- why waiting, how much run recently.")
(DEFUN PEEK-PROCESSES (IGNORE)
  "Shows state of all active processes."
  (LIST ()
        ;; 30 of process name, 25 of state, 5 of priority, 10 of quantum left/quantum,
        ;; 8 of percentage, followed by idle time (11 columns)
        (SCROLL-PARSE-ITEM (FORMAT NIL "~30A~21A~10A~10A~8A~8A"
                                   "Process Name" "State" "Priority" "Quantum"
                                   " %" "Idle"))
        (SCROLL-PARSE-ITEM "")
        (SCROLL-MAINTAIN-LIST #'(LAMBDA () ALL-PROCESSES)
                              #'(LAMBDA (PROCESS)
                                  (SCROLL-PARSE-ITEM
                                    `(:MOUSE-ITEM
                                       (NIL :EVAL (PEEK-PROCESS-MENU ',PROCESS 'ITEM 0)
                                            :DOCUMENTATION
                                            "Menu of useful things to do to this process.")
                                       :STRING ,(PROCESS-NAME PROCESS) 30.)
                                    `(:FUNCTION ,#'PEEK-WHOSTATE ,(NCONS PROCESS) 25.)
                                    `(:FUNCTION ,PROCESS (:PRIORITY) 5. ("~D."))
                                    `(:FUNCTION ,PROCESS (:QUANTUM-REMAINING) 5. ("~4D//"))
                                    `(:FUNCTION ,PROCESS (:QUANTUM) 5. ("~D."))
                                    `(:FUNCTION ,PROCESS (:PERCENT-UTILIZATION) 8.
                                                ("~1,1,4$%"))
                                    `(:FUNCTION ,PROCESS (:IDLE-TIME) NIL
                                                ("~\TV::PEEK-PROCESS-IDLE-TIME\"))))
                              NIL
                              NIL)
        (SCROLL-PARSE-ITEM "")
        (SCROLL-PARSE-ITEM "Clock Function List")
        (SCROLL-MAINTAIN-LIST #'(LAMBDA () SI::CLOCK-FUNCTION-LIST)
                              #'(LAMBDA (FUNC)
                                  (SCROLL-PARSE-ITEM
                                    `(:STRING ,(WITH-OUTPUT-TO-STRING (STR)
                                                 (PRINC FUNC STR))))))))

(FORMAT:DEFFORMAT PEEK-PROCESS-IDLE-TIME (:ONE-ARG) (ARG IGNORE)
  (COND ((NULL ARG) (SEND *STANDARD-OUTPUT* :STRING-OUT "forever"))     ; character too small
        ((ZEROP ARG))                           ;Not idle
        ((< ARG 60.) (FORMAT T "~D sec" ARG))
        ((< ARG 3600.) (FORMAT T "~D min" (TRUNCATE ARG 60.)))
        (T (FORMAT T "~D hr" (TRUNCATE ARG 3600.)))))

(DEFUN PEEK-WHOSTATE (PROCESS)
  (COND ((SI::PROCESS-ARREST-REASONS PROCESS) "Arrest")
        ((SI::PROCESS-RUN-REASONS PROCESS)
         (or (si::process-wait-whostate process)
             (string-append "Running: " (si::process-run-whostate process))))
        (T "Stop")))

(DEFINE-PEEK-MODE PEEK-RATES #/R "Counter Rates" NIL
  "Display the rate of change of the counters.")

(defstruct (peek-rate (:type :list))
  peek-rate-counter-name
  peek-rate-counter-type
  peek-rate-last-value
  peek-rate-average
  )

(defconst peek-rate-list nil)

(defun put-meters-on-peek-rate-list (meter-list function-list)
  (dolist (m meter-list)
    (cond ((null (memq m si:a-memory-counter-block-names)))
          ((null (assq m peek-rate-list))
           (let ((pr (make-peek-rate)))
             (setf (peek-rate-counter-type pr) :meter)
             (setf (peek-rate-counter-name pr) m)
             (setf (peek-rate-last-value pr) (read-meter m))
             (setf (peek-rate-average pr) 0)
             (without-interrupts
               (setq peek-rate-list (nconc peek-rate-list (list pr))))))))
  (dolist (f function-list)
    (cond ((null (assq f peek-rate-list))
           (let ((pr (make-peek-rate)))
             (setf (peek-rate-counter-type pr) :function)
             (setf (peek-rate-counter-name pr) f)
             (setf (peek-rate-last-value pr) (funcall f))
             (setf (peek-rate-average pr) 0)
             (without-interrupts
               (setq peek-rate-list (nconc peek-rate-list (list pr)))))))))

(put-meters-on-peek-rate-list '(si:%count-first-level-map-reloads
                                 si:%count-second-level-map-reloads
                                 %COUNT-PDL-BUFFER-READ-FAULTS
                                 %COUNT-PDL-BUFFER-WRITE-FAULTS
                                 %COUNT-PDL-BUFFER-MEMORY-FAULTS
                                 %COUNT-DISK-PAGE-READS
                                 %COUNT-DISK-PAGE-WRITES
                                 %COUNT-FRESH-PAGES
                                 %COUNT-AGED-PAGES
                                 %COUNT-AGE-FLUSHED-PAGES
                                 %COUNT-META-BITS-MAP-RELOADS
                                 %COUNT-CONS-WORK
                                 %COUNT-SCAVENGER-WORK
                                 %AGING-DEPTH
                                 %COUNT-FINDCORE-STEPS
                                 %COUNT-FINDCORE-EMERGENCIES
                                 %COUNT-DISK-PAGE-READ-OPERATIONS
                                 %COUNT-DISK-PAGE-WRITE-OPERATIONS
                                 %COUNT-DISK-PAGE-WRITE-WAITS
                                 %COUNT-DISK-PAGE-WRITE-BUSYS
                                 %COUNT-DISK-PREPAGES-USED
                                 %COUNT-DISK-PREPAGES-NOT-USED
                                 %DISK-WAIT-TIME
                                 %COUNT-DISK-PAGE-WRITE-APPENDS
                                 %COUNT-DISK-PAGE-READ-APPENDS
                                 %COUNT-ILLOP-DEBUG
                                 %COUNT-MICRO-FAULTS
                                 )
                              '(
;                               si:read-main-stat-counter
;                               si:read-aux-stat-counter
                                time:microsecond-time
                                ))

(defconst peek-maintain-rates-time-constant 16.)
(defconst peek-maintain-rates-number-of-sixtieths 30.)

(defvar peek-maintain-rates-ticks-to-go 0)

(defun peek-maintain-rates (time-delta)
  (condition-case ()
      (progn
       (decf peek-maintain-rates-ticks-to-go time-delta)
       (cond ((<= peek-maintain-rates-ticks-to-go 0)
              (setq peek-maintain-rates-ticks-to-go peek-maintain-rates-number-of-sixtieths)
              (dolist (pr peek-rate-list)
                (let* ((new-value (selectq (peek-rate-counter-type pr)
                                    (:meter (read-meter (peek-rate-counter-name pr)))
                                    (:function (funcall (peek-rate-counter-name pr)))))
                       (delta (- new-value (peek-rate-last-value pr))))
                  (decf (peek-rate-average pr)
                        (// (peek-rate-average pr) peek-maintain-rates-time-constant))
                  (incf (peek-rate-average pr) delta)
                  (setf (peek-rate-last-value pr) new-value)))
              )))
    (error (without-interrupts
             (setq clock-function-list (delq 'peek-maintain-rates si:clock-function-list))))))

(defun activate-peek-rate-maintainer (on-p)
  (peek-maintain-rates 1)
  (without-interrupts
    (cond ((null on-p)
           (setq si:clock-function-list (delq 'peek-maintain-rates si:clock-function-list)))
          ((memq 'peek-maintain-rates si:clock-function-list))
          (t
           (setq peek-maintain-rates-ticks-to-go 0)
           (push 'peek-maintain-rates si:clock-function-list)))))

(defmethod (peek-frame :after :deexpose) (&rest ignore)
  (activate-peek-rate-maintainer nil))

(DEFUN PEEK-RATES (IGNORE)
  "Statistics counter rates"
  (activate-peek-rate-maintainer t)
  (SCROLL-MAINTAIN-LIST
    #'(LAMBDA () peek-rate-list)
    #'(LAMBDA (pr)
        (SCROLL-PARSE-ITEM
          `(:STRING ,(STRING (peek-rate-counter-name pr)) 35.)
          `(:function ,#'(lambda (pr)
                           (case (peek-rate-counter-type pr)
                             (:meter (read-meter (peek-rate-counter-name pr)))
                             (:function (funcall (peek-rate-counter-name pr)))))
                      (,pr) nil ("~15:d" 10. T))
          `(:function ,#'(lambda (pr)
;                          (// (* (// 60. peek-maintain-rates-number-of-sixtieths)
;                                 (peek-rate-average pr))
;                              peek-maintain-rates-time-constant)
                           (fix (* 0.12 (peek-rate-average pr)))
                           )
                      (,pr)
                      nil ("~15:d" 10. T))))))


(DEFINE-PEEK-MODE PEEK-COUNTERS #/% "Statistics Counters" NIL
  "Display the values of all the microcode meters.")

(DEFUN PEEK-COUNTERS (IGNORE)
  "Statistics counters"
  (cond (t ;(not (fboundp 'si::read-main-stat-counter))
         (LIST ()
               (SCROLL-MAINTAIN-LIST #'(LAMBDA () SYS:A-MEMORY-COUNTER-BLOCK-NAMES)
                                     #'(LAMBDA (COUNTER)
                                         (SCROLL-PARSE-ITEM
                                           `(:STRING ,(STRING COUNTER) 35.)
                                           `(:FUNCTION READ-METER (,COUNTER) NIL
                                                       ("~@15A" 10. T)))))))

;       (t
;        (LIST ()
;              (SCROLL-MAINTAIN-LIST #'(LAMBDA () SYS:A-MEMORY-COUNTER-BLOCK-NAMES)
;                                    #'(LAMBDA (COUNTER)
;                                        (SCROLL-PARSE-ITEM
;                                          `(:STRING ,(STRING COUNTER) 35.)
;                                          `(:FUNCTION READ-METER (,COUNTER) NIL
;                                                      ("~@15A" 10. T)))))
;              (SCROLL-PARSE-ITEM "")
;              (scroll-parse-item
;                `(:mouse-item
;                   (nil :eval (peek-stat-counter-controls-menu)
;                        :documentation
;                        "Change stat counter controls.")
;                   :string "Statistics Counters" 20.))
;              (scroll-parse-item "Main: "
;                                 `(:function ,#'(lambda ()
;                                                  (nth (ldb si::%%main-stat-clock-control
;                                                            (si::read-rg-mode))
;                                                       stat-clock-values))
;                                             () nil ("~a"))
;                                 "; "
;                                 `(:function ,#'(lambda ()
;                                                  (nth (ldb si::%%main-stat-count-control
;                                                            (si::read-rg-mode))
;                                                       stat-count-values))
;                                             () nil ("~a")))
;              (scroll-parse-item "Aux : "
;                                 `(:function ,#'(lambda ()
;                                                  (nth (ldb si::%%aux-stat-clock-control
;                                                            (si::read-rg-mode))
;                                                       stat-clock-values))
;                                             () nil ("~a"))
;                                 "; "
;                                 `(:function ,#'(lambda ()
;                                                  (nth (ldb si::%%aux-stat-count-control
;                                                            (si::read-rg-mode))
;                                                       stat-count-values))
;                                             () nil ("~a")))

;              (scroll-parse-item "Main counter: "
;                                 `(:function si::read-main-stat-counter () nil ("~20:d")))
;              (scroll-parse-item "Aux  counter: "
;                                 `(:function si::read-aux-stat-counter () nil ("~20:d")))
;              (scroll-parse-item "Main - Aux:   "
;                                 `(:function ,#'(lambda ()
;                                                  (- (si::read-main-stat-counter)
;                                                     (si::read-aux-stat-counter)))
;                                             () nil ("~20:d")))
;              (scroll-parse-item "Main // Aux:   "
;                                 `(:function ,#'(lambda ()
;                                                  (let ((main (si::read-main-stat-counter))
;                                                        (aux (si::read-aux-stat-counter)))
;                                                    (cond ((zerop aux)
;                                                           "***")
;                                                          (t
;                                                           (// (float main) aux)))))
;                                             () nil ("~20f")))
;              (scroll-parse-item "Aux // Main:   "
;                                 `(:function ,#'(lambda ()
;                                                  (let ((main (si::read-main-stat-counter))
;                                                        (aux (si::read-aux-stat-counter)))
;                                                    (cond ((zerop main)
;                                                           "***")
;                                                          (t
;                                                           (// (float aux) main)))))
;                                             () nil ("~20f")))
;              ))
;
  ))

;(defun peek-stat-counter-controls-menu ()
;  (process-run-function "Change Stat Counters" 'peek-stat-counter-controls-function))

;(defvar peek-main-stat-clock :sm.clock)
;(defvar peek-main-stat-count :hi)
;(defvar peek-aux-stat-clock :sm.clock)
;(defvar peek-aux-stat-count :hi)

;(defconst stat-clock-values '(:SM.CLOCK :UINST.CLOCK))

;(defconst stat-count-values '(:VALID.STATISTICS.BIT
;                                :MEMORY.START.NEXT.CYCLE
;                                :CSM.STATISTICS.BIT
;                                :INCREMENT.LC
;                                :T.HOLD
;                                :T.STATISTICS.BIT
;                                :1.MHZ.CLOCK
;                                :HI))

;(defun make-atom-list-into-menu-alist (atom-list)
;  (loop for a in atom-list
;       collect (list (string a) a)))

;(defun peek-stat-counter-controls-function ()
;  (tv:choose-variable-values `((peek-main-stat-clock
;                                "Main stat clock"
;                                :menu-alist ,(make-atom-list-into-menu-alist stat-clock-values))
;                              (peek-main-stat-count
;                                "Main stat count control"
;                                :menu-alist ,(make-atom-list-into-menu-alist stat-count-values))
;                              (peek-aux-stat-clock
;                                "Aux stat clock"
;                                :menu-alist ,(make-atom-list-into-menu-alist stat-clock-values))
;                              (peek-aux-stat-count
;                                "Aux stat count control"
;                                :menu-alist ,(make-atom-list-into-menu-alist stat-count-values))
;                              "foo"
;                              "bar"
;                              )
;                            )
;  (si::write-main-stat-control (find-position-in-list peek-main-stat-clock stat-clock-values)
;                             (find-position-in-list peek-main-stat-count stat-count-values))
;  (si::write-aux-stat-control (find-position-in-list peek-aux-stat-clock stat-clock-values)
;                            (find-position-in-list peek-aux-stat-count stat-count-values))
;;  (si::reset-stat-counters)
;  )

;;;; Memory

(defun peek-memory-header ()
  (scroll-parse-item
      "Physical memory: "
      `(:function ,(lambda (&aux (val (aref (symbol-function 'system-communication-area)
                                            %sys-com-memory-size)))
                     (setf (value 0) (truncate val #o2000))
                     val)
                  nil nil (nil 8.))
      `(:value 0 nil (" (~DK), "))
      "Free space: "
      `(:function ,(lambda (&aux (val (si::free-space)))
                     (setf (value 0) (truncate val #o2000))
                     val)
                  nil nil (nil 8.))
      `(:value 0 nil (" (~DK)"))
      ", Wired pages "
      `(:function ,(lambda ()
                     (multiple-value-bind (n-wired-pages n-fixed-wired-pages)
                         (si::count-wired-pages)
                       (setf (value 0) (- n-wired-pages n-fixed-wired-pages))
                       (setf (value 1) (truncate n-wired-pages (truncate 2000 page-size)))
                       (setf (value 2) (\ n-wired-pages (truncate 2000 page-size)))
                       n-fixed-wired-pages))
                  nil nil ("~D"))
      `(:value 0 nil ("+~D "))
      `(:value 1 nil ("(~D"))
      `(:value 2 nil ("~[~;.25~;.5~;.75~]K)"))))

(define-peek-mode peek-areas #/A "Areas" nil
  "Display status of areas, including how much memory allocated and used.")

(defconst *first-interesting-area* working-storage-area
  "All areas  this are uninteresting crufty system frobs and kludges.")

(defun peek-areas (ignore)
  "Areas"
  (list ()
    (peek-memory-header)
    (scroll-parse-item "")
    (scroll-maintain-list
      (lambda () 'boring)
      (lambda (area)
        (if (eq area 'boring)
            (list
              '(:pre-process-function peek-areas-boring-display)
              (scroll-parse-item
                :mouse-self '(nil :eval (peek-mouse-click 'self 0)
                                  :documentation
                                  "Insert//remove display of boring areas.")
                :leader `(nil)
                `(:function ,(lambda () (1- *first-interesting-area*))
                            ()
                            nil (" 0-~D /"Uninteresting/" system-internal areas"))))
          (list
            '(:pre-process-function peek-areas-region-display)
            (scroll-parse-item
              :mouse-self '(nil :eval (peek-mouse-click 'self 0)
                                :documentation
                                "Insert//remove display of all regions in this area.")
              :leader `(nil ,area)
              `(:function identity (,area) 4 ("~3D"))
              `(:string ,(string (area-name area)) 40.)
              `(:function ,(lambda (area)
                             (multiple-value-bind (length used n-regions)
                                 (si::room-get-area-length-used area)
                               (setf (value 0) used)
                               (setf (value 1) length)
                               (setf (value 2)
                                     (cond ((zerop length) 0)
                                           ((< length #o40000)
                                            (truncate (* 100. (- length used)) length))
                                           (t
                                            (truncate (- length used) (truncate length 100.)))))
                               n-regions))
                          (,area) 15. ("(~D region~0@*~P)"))
              `(:value 2 nil ("~@3A% free, " 10. t))
              `(:value 0 nil ("~8O"))
              `(:value 1 nil ("//~8O used"))))))
      nil
      (lambda (state &aux next-one this-one (len (array-length (symbol-function 'area-name))))
        (if (eq state 'boring)
            (values 'boring *first-interesting-area* nil)
          (do ((i state (1+ i)))
              (( i len) nil)
            (cond ((and (null this-one) (aref (symbol-function 'area-name) i))
                   (setq this-one i))
                  ((and this-one (aref (symbol-function 'area-name) i))
                   (setq next-one i)
                   (return t))))
          (values this-one next-one (null next-one)))))))

(defun peek-areas-boring-display (item)
  "Handles adding/deleting of the boring areas when a mouse button is clicked."
  (cond ((null (array-leader (cadr item) scroll-item-leader-offset)))
         ;; Clicked on this item, need to complement state
        ((= (length item) 2)
         ;; If aren't displaying boredom now, display it
         (setf (cddr item)
               (ncons
                 (scroll-maintain-list
                   (lambda () 0)
                   (lambda (area)
                     (list
                       '(:pre-process-function peek-areas-region-display)
                       (scroll-parse-item
                         :mouse-self '(nil :eval (peek-mouse-click 'self 0)
                                           :documentation
                                           "Insert//remove display of all regions in this boring area.")
                         :leader `(nil ,area)
                         `(:function identity (,area) 5 (" ~3D"))
                         `(:string ,(string (area-name area)) 39.)
                         `(:function ,(lambda (area)
                                        (multiple-value-bind (length used n-regions)
                                            (si::room-get-area-length-used area)
                                          (setf (value 0) used)
                                          (setf (value 1) length)
                                          (setf (value 2)
                                                (cond ((zerop length) 0)
                                                      ((< length #o40000)
                                                       (truncate (* 100. (- length used))
                                                                 length))
                                                      (t
                                                       (truncate (- length used)
                                                                 (truncate length 100.)))))
                                          n-regions))
                                     (,area) 15. ("(~D region~0@*~P)"))
                         `(:value 2 nil ("~@3A% free, " 10. t))
                         `(:value 0 nil ("~O"))
                         `(:value 1 nil ("//~O used")))))
                   nil
                   (lambda (state &aux next-one this-one)
                     (do ((i state (1+ i)))
                         (( i *first-interesting-area*) nil)
                       (cond ((and (null this-one) (aref (symbol-function 'area-name) i))
                              (setq this-one i))
                             ((and this-one (aref (symbol-function 'area-name) i))
                              (setq next-one i)
                              (return t))))
                     (values this-one next-one (null next-one)))))))
        (t (setf (cddr item) nil)))
  (setf (array-leader (cadr item) scroll-item-leader-offset) nil))


(defun peek-areas-region-display (item)
  "Handles adding/deleting of the region display when a mouse button is clicked."
  (cond ((null (array-leader (cadr item) scroll-item-leader-offset)))
         ;; Clicked on this item, need to complement state
        ((= (length item) 2)
         ;; If aren't displaying regions now, display them
         (setf (cddr item)
               (ncons
                 (scroll-maintain-list
                   (lambda ()
                     (si:%area-region-list (array-leader (first (scroll-items item))
                                                     (1+ scroll-item-leader-offset))))
                   (lambda (region)
                     (if (minusp region)
                         (scroll-parse-item `(:string "     No regions in this area"))
                       (scroll-parse-item
                         `(:string
                            ,(format nil "     ~3O: Origin ~8O, Length ~8O, "
                                     region
                                     (si:%pointer-unsigned (si::%region-origin region))
                                     (si::%region-length region)))
                         `(:function ,#'si::%region-free-pointer (,region) nil ("Used ~8O, "))
                         `(:function
                            ,(lambda (region)
                               (let ((used (si::%region-free-pointer region))
                                     (scavenged (si::%region-gc-pointer region)))
                                 (cond ((not (si::%region-scavenge-enable region))
                                        "Scavenger off, ")
                                       ((= used scavenged)
                                        "Scavenge done, ")
                                       ((= used 0)
                                        "Scavenged  0%, ")
                                       (t
                                        (format nil "Scavenged ~D%, "
                                                (round
                                                  (* 100. (// (float scavenged)
                                                              (float used)))))))))
                            (,region))
                         `(:string
                            ,(format nil "~A space, Vol=~D."
                                     (nth (si::%region-type region)
                                          '(free old new new1 new2 new3 new4 new5 new6
                                                 static fixed extra-pdl copy))
                                     (si::%region-volatility region))))))
                   nil
                   (lambda (state)
                     (if (minusp state)
                         ;; no regions at all in this area
                         (values -1 -1 t)
                         (values state
                                 (si:%region-list-thread state)
                                 (minusp (si:%region-list-thread state)))))))))
        (t (setf (cddr item) nil)))
  (setf (array-leader (cadr item) scroll-item-leader-offset) nil))

(DEFINE-PEEK-MODE PEEK-page-status #/M "Memory Usage" NIL
  "Percent of each area paged in.")

(DEFUN PEEK-page-status (IGNORE)
  "Page Status"
  (LIST ()
        (scroll-maintain-list
          #'(lambda ()
              (loop for i from si:VIRTUAL-PAGE-DATA below (array-length #'area-name)
                    when (aref #'area-name i)
                    collect (aref #'area-name i)))
          #'(lambda (area)
              (scroll-parse-item
                `(:string ,(string area) 40.)
                `(:function page-status-function (,area) nil
                            ("~@15A" 10. t)))))))

(defun page-status-function (area &aux (total-pages 0) (paged-in-pages 0))
  (gc:without-flipping
    (si:for-every-region-in-area (region (symbol-value area))
      (select (ldb %%region-space-type (si:%region-bits region))
        ((%REGION-SPACE-FREE
           %REGION-SPACE-OLD
           %REGION-SPACE-EXTRA-PDL
           %REGION-SPACE-MOBY-FIXED
           %REGION-SPACE-MOBY-NEW))
        ((%REGION-SPACE-NEW
           %REGION-SPACE-STATIC
           %REGION-SPACE-FIXED
           %REGION-SPACE-COPY)
         (let ((pages-in-this-region (ceiling (si:%region-free-pointer region) page-size)))
           (incf total-pages pages-in-this-region)
           (dotimes (page-number pages-in-this-region)
             (if (%page-status (ash page-number 8))
                 (incf paged-in-pages)))))
        (t
         (ferror nil "unknown region space type")))))
  (when (not (zerop total-pages))
    (format nil "~2d% of ~5d pages"
            (round (* 100. (// (float paged-in-pages) total-pages)))
            total-pages)))


;;;; File system status

(DEFINE-PEEK-MODE PEEK-FILE-SYSTEM #/F "File System Status" NIL
  "Display status of FILE protocol connections to remote file systems.")

(DEFUN NEXT-INTERESTING-FILE-HOST-STEPPER (LIST)
  (LET ((GOOD (SOME LIST (LAMBDA (X) (SEND-IF-HANDLES X :ACCESS)))))
    (VALUES (CAR GOOD) (CDR GOOD) (NULL GOOD))))

(defun file-host-item-function (host)
  (when host
    (list (list :pre-process-function 'peek-file-host-pre-process
                :host host
                :access nil
                :host-units nil)
            '(nil)
            '(nil))))

(defun peek-file-host-pre-process (item)
  (let* ((host (getf (tv:scroll-item-plist item) :host))
         (access (getf (tv:scroll-item-plist item) :access))
         (host-units (getf (tv:scroll-item-plist item) :host-units))
         (new-access (send host :access))
         (new-host-units (send-if-handles new-access :host-units)))
    (unless (and (eq access new-access) (equal host-units new-host-units))
      (setf (getf (tv:scroll-item-plist item) :access) new-access)
      (setf (getf (tv:scroll-item-plist item) :host-units) new-host-units)
      (setf (cdr (tv:scroll-item-component-items item))
            (append (send host :peek-file-system-header)
                    (send host :peek-file-system))))))

(defun peek-file-system (ignore)
  "Display status of file system"
  (scroll-maintain-list
    #'(lambda () fs:*pathname-host-list*)
    'file-host-item-function
    nil
    'next-interesting-file-host-stepper))

(DEFMETHOD (SI::FILE-DATA-STREAM-MIXIN :PEEK-FILE-SYSTEM) (&OPTIONAL (INDENT 0) &AUX DIRECTION)
  "Returns a scroll item describing a stream"
  (TV:SCROLL-PARSE-ITEM
    :MOUSE `(NIL :EVAL (PEEK-FILE-SYSTEM-STREAM-MENU ',SELF)
                  :DOCUMENTATION "Menu of useful things to do to this open file.")
    (AND ( INDENT 0) (FORMAT NIL "~V@T" INDENT))
    (CASE (SETQ DIRECTION (SEND SELF :DIRECTION))
      (:INPUT "Input ")
      (:OUTPUT "Output ")
      (OTHERWISE "?Direction? "))
    (SEND (SEND SELF :PATHNAME) :STRING-FOR-PRINTING)
    (IF (SEND SELF :CHARACTERS)
        ", Character, " ", Binary, ")
    `(:FUNCTION ,#'(LAMBDA (STREAM)
                     (SETF (TV:VALUE 0) (SEND STREAM :READ-POINTER))
                     (TV:VALUE 0))
                (,SELF) NIL ("~D"))
    (AND (EQ DIRECTION :INPUT)
         `(:FUNCTION ,#'(LAMBDA (STREAM)
                          (LET ((LENGTH (SEND STREAM :LENGTH)))
                            (AND LENGTH (NOT (ZEROP LENGTH))
                                 (TRUNCATE (* 100. (TV:VALUE 0)) LENGTH))))
                     (,SELF) NIL ("~@[ (~D%)~]")))
    " bytes"))

(DEFUN PEEK-FILE-SYSTEM-STREAM-MENU (STREAM)
  (APPLY #'PROCESS-RUN-FUNCTION "Peek File System Menu"
                                SELF :PEEK-FILE-SYSTEM-MENU
                                (LIST STREAM)))

(DEFMETHOD (BASIC-PEEK :PEEK-FILE-SYSTEM-MENU) (STREAM)
  (LET ((*TERMINAL-IO* TYPEOUT-WINDOW))
    (MENU-CHOOSE `(("Close" :EVAL (SEND ',STREAM :CLOSE)
                    :DOCUMENTATION "Close selected file (normally).")
                   ("Abort" :EVAL (SEND ',STREAM :CLOSE :ABORT)
                    :DOCUMENTATION "Close selected file (aborts writing).")
                   ("Delete" :EVAL (SEND ',STREAM :DELETE)
                    :DOCUMENTATION "Delete selected file, but don't close it.")
                   ("Describe" :EVAL (DESCRIBE ',STREAM)
                    :DOCUMENTATION "Describe the file's stream.")
                   ("Inspect" :EVAL (INSPECT ',STREAM)
                    :DOCUMENTATION "Inspect the file's stream.")
                   )
                 (STRING (SEND STREAM :TRUENAME)))))

(DEFUN PEEK-PROCESS-MENU (&REST ARGS)
  (APPLY #'PROCESS-RUN-FUNCTION "Peek Process Menu"
                                SELF :PEEK-PROCESS-MENU ARGS))

(DEFMETHOD (BASIC-PEEK :PEEK-PROCESS-MENU) (PROCESS &REST IGNORE &AUX CHOICE)
  "Menu for interesting operations on processes in a peek display"
  (LET ((*TERMINAL-IO* TYPEOUT-WINDOW)
        (CHOICES '(("Debugger" :VALUE PROCESS-EH
                    :DOCUMENTATION
                    "Call the debugger to examine the selected process.")
                   ("Arrest" :VALUE PROCESS-ARREST
                    :DOCUMENTATION "Arrest the selected process.  Undone by Un-Arrest.")
                   ("Un-Arrest" :VALUE PROCESS-UN-ARREST
                    :DOCUMENTATION "Un-Arrest the selected process.  Complement of Arrest.")
                   ("Flush" :VALUE PROCESS-FLUSH
                    :DOCUMENTATION
                    "Unwind the selected process' stack and make it unrunnable.  Ask for confirmation.")
                   ("Reset" :VALUE PROCESS-RESET
                    :DOCUMENTATION "Reset the selected process.  Ask for confirmation.")
                   ("Kill" :VALUE PROCESS-KILL
                    :DOCUMENTATION
                    "Kill the selected process.  Ask for confirmation.")
                   ("Describe" :VALUE PROCESS-DESCRIBE
                    :DOCUMENTATION
                    "Call DESCRIBE on this process.")
                   ("Inspect" :VALUE PROCESS-INSPECT
                    :DOCUMENTATION
                    "Call INSPECT on this process."))))
    ;; Don't offer EH for a simple process.
    (OR (TYPEP (PROCESS-STACK-GROUP PROCESS) 'STACK-GROUP)
        (POP CHOICES))
    (SETQ CHOICE (MENU-CHOOSE CHOICES (PROCESS-NAME PROCESS)))
    (CASE CHOICE
      (PROCESS-ARREST (SEND PROCESS :ARREST-REASON))
      (PROCESS-UN-ARREST (SEND PROCESS :REVOKE-ARREST-REASON))
      (PROCESS-FLUSH (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Flush ~A" PROCESS))
                         (SEND PROCESS :FLUSH)))
      (PROCESS-RESET (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Reset ~A" PROCESS))
                         (SEND PROCESS :RESET)))
      (PROCESS-KILL (IF (MOUSE-Y-OR-N-P (FORMAT NIL "Kill ~A" PROCESS))
                        (SEND PROCESS :KILL)))
      (PROCESS-EH (SEND SELF :FORCE-KBD-INPUT `(EH ,PROCESS)))
      (PROCESS-DESCRIBE (SEND SELF :FORCE-KBD-INPUT `(DESCRIBE ,PROCESS)))
      (PROCESS-INSPECT (SEND SELF :FORCE-KBD-INPUT `(INSPECT ,PROCESS)))
      (NIL)
      (OTHERWISE (BEEP)))))

;;;; Peeking at windows

;; Copied from LAD: RELEASE-3.WINDOW; PEEK.LISP#191 on 2-Oct-86 04:12:43
(DEFINE-PEEK-MODE PEEK-WINDOW-HIERARCHY #/W "Window Hierarchy" NIL
  "Display the hierarchy of window inferiors, saying which are exposed.")

(DEFUN PEEK-WINDOW-HIERARCHY (IGNORE)
  (SCROLL-MAINTAIN-LIST #'(LAMBDA () ALL-THE-SCREENS)
                        #'(LAMBDA (SCREEN)
                            (LIST ()
                              (SCROLL-PARSE-ITEM (FORMAT NIL "Screen ~A" SCREEN))
                              (PEEK-WINDOW-INFERIORS SCREEN 2)
                              (SCROLL-PARSE-ITEM "")))))

(DEFUN PEEK-WINDOW-INFERIORS (WINDOW INDENT)
  (SCROLL-MAINTAIN-LIST #'(LAMBDA () (SHEET-INFERIORS WINDOW))
                        #'(LAMBDA (SHEET)
                            (LIST ()
                                  (SCROLL-PARSE-ITEM
                                    (FORMAT NIL "~V@T" INDENT)
                                    `(:MOUSE
                                       (NIL :EVAL (PEEK-WINDOW-MENU ',SHEET)
                                            :DOCUMENTATION
                                            "Menu of useful things to do to this window.")
                                       :STRING
                                       ,(SEND SHEET :NAME)))
                                  (PEEK-WINDOW-INFERIORS SHEET (+ INDENT 4))))))

(DEFUN PEEK-WINDOW-MENU (&REST ARGS)
  (APPLY #'PROCESS-RUN-FUNCTION "Peek Window Menu"
                                #'PEEK-WINDOW-MENU-INTERNAL SELF ARGS))

(DEFUN PEEK-WINDOW-MENU-INTERNAL (PEEK-WINDOW SHEET &REST IGNORE &AUX CHOICE)
  "Menu for interesting operations on sheets in a peek display"
  (SETQ CHOICE
        (MENU-CHOOSE
          '(("Deexpose" :VALUE :DEEXPOSE :DOCUMENTATION "Deexpose the window.")
            ("Expose" :VALUE :EXPOSE :DOCUMENTATION "Expose the window.")
            ("Select" :VALUE :SELECT :DOCUMENTATION "Select the window.")
            ("Deselect" :VALUE :DESELECT :DOCUMENTATION "Deselect the window.")
            ("Deactivate" :VALUE :DEACTIVATE :DOCUMENTATION "Deactivate the window.")
            ("Kill" :VALUE :KILL :DOCUMENTATION "Kill the window.")
            ("Bury" :VALUE :BURY :DOCUMENTATION "Bury the window.")
            ("Inspect" :VALUE :INSPECT :DOCUMENTATION "Look at window data structure."))
          (SEND SHEET :NAME)))
  (AND CHOICE
       (OR (NEQ CHOICE :KILL)
           (MOUSE-Y-OR-N-P (FORMAT NIL "Kill ~A" (SEND SHEET :NAME))))
       (IF (EQ CHOICE :INSPECT)
           (SEND PEEK-WINDOW :FORCE-KBD-INPUT `(INSPECT ,SHEET))
           (SEND SHEET CHOICE))))

(DEFINE-PEEK-MODE PEEK-SERVERS #/S "Active Servers" NIL
  "List all servers, who they are serving, and their status.")

(DEFUN PEEK-SERVERS (IGNORE)
    (LIST ()
          (SCROLL-PARSE-ITEM "Active Servers")
          (SCROLL-PARSE-ITEM "Contact Name        Host                Process // State")
          (SCROLL-PARSE-ITEM "                                                  Connection")
          (SCROLL-PARSE-ITEM "")
          (SCROLL-MAINTAIN-LIST
            #'(LAMBDA () (SEND TV:WHO-LINE-FILE-STATE-SHEET :SERVERS))
            #'(LAMBDA (SERVER-DESC)
                (LET* ((PROCESS (SERVER-DESC-PROCESS SERVER-DESC))
                       (CONN (SERVER-DESC-CONNECTION SERVER-DESC))
                       (contact (server-desc-contact-name server-desc))
                       (HOST (if (typep conn 'chaos:conn)
                                 (SI:GET-HOST-FROM-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN) :CHAOS)
                               (nth-value 1 (ip:parse-internet-address (send conn :remote-address))))))
                  (LIST '(:PRE-PROCESS-FUNCTION PEEK-SERVER-PREPROCESS)
                        (SCROLL-PARSE-ITEM
                          :LEADER '(NIL NIL NIL)
                          `(:FUNCTION ,#'values (,contact) 20. ("~A"))
                          `(:MOUSE-ITEM
                             (NIL :EVAL (CHAOS:PEEK-CHAOS-HOST-MENU ',HOST 'TV:ITEM 0)
                                  :DOCUMENTATION "Menu of useful things to do to this host.")
                             :FUNCTION ,#'VALUES (,HOST) 20. ("~A"))
                          `(:MOUSE
                             (NIL :EVAL (PEEK-PROCESS-MENU ',PROCESS)
                                  :DOCUMENTATION
                                  "Menu of useful things to do to this process.")
                             :STRING
                             ,(FORMAT NIL "~S" PROCESS))
                          "    "
                          `(:FUNCTION ,#'PEEK-WHOSTATE ,(NCONS PROCESS)))
                        (SCROLL-PARSE-ITEM
                          :LEADER '(NIL NIL NIL NIL NIL NIL)    ;6
                          "                                                  "
                          `(:MOUSE-ITEM
                             (NIL :EVAL (PEEK-CONNECTION-MENU ',CONN 'ITEM ',host ',contact)
                                  :DOCUMENTATION
                                  "Menu of useful things to do this connection")
                             :STRING ,(FORMAT NIL "~S" CONN)))
                        NIL                     ;Connection stat
                        NIL                     ;hostat
                        (AND (SERVER-DESC-FUNCTION SERVER-DESC)
                             (APPLY (SERVER-DESC-FUNCTION SERVER-DESC)
                                    (SERVER-DESC-ARGS SERVER-DESC)))))))))

(DEFUN PEEK-CONNECTION-MENU (CONN ITEM host contact)
  (APPLY #'PROCESS-RUN-FUNCTION "Peek Server Connection Menu"
                                SELF :PEEK-SERVER-CONNECTION-MENU
                                (LIST CONN ITEM host contact)))

(DEFMETHOD (BASIC-PEEK :PEEK-SERVER-CONNECTION-MENU) (CONN ITEM host contact)
  (LET ((*TERMINAL-IO* TYPEOUT-WINDOW))
    (LET ((CHOICE
            (MENU-CHOOSE (append '(("Close" :VALUE :CLOSE
                                    :DOCUMENTATION "Close connection forcibly."))
                                 (when (typep conn 'chaos:conn)
                                   '(("Insert Detail" :VALUE :DETAIL
                                      :DOCUMENTATION
                                      "Insert detailed info about chaos connection.")))
                                 (when (typep conn 'chaos:conn)
                                   '(("Remove Detail" :VALUE :UNDETAIL
                                      :DOCUMENTATION
                                      "Remove detailed info from Peek display.")))
                                 '(("Inspect" :VALUE :INSPECT
                                    :DOCUMENTATION "Inspect the connection")))
                         (STRING-APPEND host "//" contact))))
      (CASE CHOICE
        (:CLOSE    (if (typep conn 'chaos:conn)
                       (CHAOS:CLOSE-CONN CONN "Manual Close from PEEK")
                     (send conn :close)))
        (:INSPECT  (INSPECT CONN))
        (:DETAIL   (SETF (ARRAY-LEADER ITEM (+ 4 TV:SCROLL-ITEM-LEADER-OFFSET)) CONN)
                   (SETF (ARRAY-LEADER ITEM (+ 5 TV:SCROLL-ITEM-LEADER-OFFSET)) T))
        (:UNDETAIL (SETF (ARRAY-LEADER ITEM (+ 4 TV:SCROLL-ITEM-LEADER-OFFSET)) NIL)
                   (SETF (ARRAY-LEADER ITEM (+ 5 TV:SCROLL-ITEM-LEADER-OFFSET)) NIL))))))

(DEFUN PEEK-SERVER-PREPROCESS (LIST-ITEM &AUX HOST)
  (LET* ((LINE-ITEM (THIRD LIST-ITEM))
         (HOST-ITEM (SECOND LIST-ITEM))
         (WANTED (ARRAY-LEADER LINE-ITEM (+ 4 TV:SCROLL-ITEM-LEADER-OFFSET)))
         (GOT (ARRAY-LEADER LINE-ITEM (+ 5 TV:SCROLL-ITEM-LEADER-OFFSET))))
    (COND ((NULL WANTED)
           (SETF (ARRAY-LEADER LINE-ITEM (+ 5 TV:SCROLL-ITEM-LEADER-OFFSET)) NIL)
           (SETF (FOURTH LIST-ITEM) NIL))
          ((EQ WANTED GOT))
          (T
           (SETF (FOURTH LIST-ITEM) (CHAOS:PEEK-CHAOS-CONN WANTED))
           (SETF (ARRAY-LEADER LINE-ITEM (+ 5 TV:SCROLL-ITEM-LEADER-OFFSET)) WANTED)))
    ;; Hack hostat
    (COND ((ARRAY-LEADER HOST-ITEM TV:SCROLL-ITEM-LEADER-OFFSET)
         ;; Want a hostat, make sure it's there and for the right host
           (IF (AND (EQ (SETQ HOST (ARRAY-LEADER HOST-ITEM (1+ TV:SCROLL-ITEM-LEADER-OFFSET)))
                        (ARRAY-LEADER HOST-ITEM  (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)))
                    (FIFTH LIST-ITEM))
               NIL
             (SETF (FIFTH LIST-ITEM) (CONS '() (CHAOS:PEEK-CHAOS-HOSTAT HOST 1)))
             (SETF (ARRAY-LEADER HOST-ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) HOST)))
          (T (SETF (FIFTH LIST-ITEM) NIL)
             (SETF (ARRAY-LEADER HOST-ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) NIL)))))

(define-peek-mode peek-devices #/D "Devices" NIL
  "Display information about devices.")

(defun peek-devices (ignore)
  "Devices"
  (list ()
        (scroll-maintain-list
          #'(lambda ()
              (reverse
                (loop for x in fs:*pathname-host-list*
                      when (typep x 'si:shared-device)
                      collect x)))
          #'(lambda (dev)
              (scroll-parse-item
                `(:mouse-item
                   (nil :eval (peek-device-menu ',dev 'item 0)
                        :documentation
                        "Menu of useful things to do to this device.")
                   :string ,(send dev :name) 20.)
                `(:function peek-dev-lock (,dev) nil ("~30a"))
                `(:function peek-dev-owner (,dev) nil ("~10a"))
                )))))

(defun peek-dev-lock (dev)
  (let ((lock (car (send dev :lock))))
    (cond ((null lock) "not locked")
          (t (send lock :name)))))

(defun peek-dev-owner (dev)
  (let ((owner (send dev :owner)))
    (cond ((null owner) "free")
          ((eq owner :not-on-bus) "not on bus")
          (t (format nil "slot ~d" owner)))))

(defun peek-device-menu (&rest args)
  (apply #'process-run-function "Peek Device Menu"
                                self :peek-device-menu args))

(defmethod (basic-peek :peek-device-menu) (dev &rest ignore &aux choice)
  "Menu for interesting operations on devices in a peek display"
  (let ((*terminal-io* typeout-window)
        (choices '(("Select window with lock" :value select
                    :documentation "Select the window who holds the lock for this device.")
                   ("Clear lock" :value clear
                    :documentation "Clear the lock for this device.")
                   )))
    (setq choice (menu-choose choices (send dev :name)))
    (case choice
      (select
       (let ((proc (car (send dev :lock))))
         (if (null proc)
             (beep)
           (dolist (w (send proc :run-reasons)
                      (beep))
             (when (typep w 'tv:minimum-window)
               (send w :select)
               (return nil))))))
      (clear
       (rplaca (send dev :lock) nil))
      (nil)
      (otherwise (beep)))))

(define-peek-mode peek-general-status #/G "General Status" NIL
  "Random internal processor state.")

(defstruct (peek-general-status-item
             (:type :named-array)
             (:conc-name peek-gs-)
             (:print (format nil "#<Peek Processor Switch ~a>"
                             (peek-gs-pretty-name peek-general-status-item)
                             )))
  name
  pretty-name
  get-function
  set-function
  )

(defun peek-gs-get-from-processor-switches (bit-name)
  (ldb (symeval bit-name) (%processor-switches nil)))

(defun peek-gs-set-processor-switches (bit-name val)
  (%processor-switches (dpb val (symeval bit-name) (%processor-switches nil))))

(defvar peek-processor-switch-items ())

(defun peek-processor-switch-items ()
  (or peek-processor-switch-items
      (setq peek-processor-switch-items
            (loop for x in si::lambda-processor-switches-bits by 'cddr
                  collect (make-peek-general-status-item
                            :name x
                            :pretty-name
                            (string-capitalize-words
                              (if (string-equal x "%%PROCESSOR-SWITCH-" :end1 #o23)
                                  (substring x #o23)
                                x))
                            :get-function 'peek-gs-get-from-processor-switches
                            :set-function 'peek-gs-set-processor-switches)))))

(defun peek-general-status (ignore)
  "General Status"
  (list ()
        (scroll-maintain-list
          #'(lambda () (peek-processor-switch-items))
          #'(lambda (item)
              (scroll-parse-item
                `(:function ,(peek-gs-get-function item) (,(peek-gs-name item)) ()
                            ("~@5A  " 10. T))
                `(:string ,(peek-gs-pretty-name item) 50.)
                )))))

(defun create-first-peek-frame ()
  (or (dolist (x (selectable-windows tv:main-screen))
        (when (typep (second x) 'peek-frame)
          (return t)))
      (make-instance 'peek-frame :activate-p t)))

(add-initialization "Create a Peek Frame" '(create-first-peek-frame) '(:once))

(tv:add-system-key #/P 'PEEK-FRAME "Peek" T)

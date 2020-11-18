;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Command loop and primitives for ZMail
;;; Definitions are in DEFS
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; An invalid Enhancements copyright notice on AI:ZMAIL; TOP 484 removed on 3/31/82 by RG
;;;  This file had been installed as the official MIT source in direct contravention
;;;  to instructions to Symbolics personnel acting on MIT's behalf.


;;; A process with a few communication cells.
(DEFFLAVOR ZMAIL-BACKGROUND-PROCESS
        ((REQUEST-CELL NIL)
         (RESPONSE-QUEUE NIL)
         (LOCK NIL)
         (PRELOAD-QUEUE NIL))
        (SI:PROCESS)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)

;;; The command loops
(DEFFLAVOR ZMAIL-FRAME-MIXIN
        (MODE-LINE-WINDOW)
        ()
  (:REQUIRED-FLAVORS TV:BASIC-CONSTRAINT-FRAME))

(DEFMETHOD (ZMAIL-FRAME-MIXIN :MODE-LINE-WINDOW) ()
  (IF (VARIABLE-BOUNDP MODE-LINE-WINDOW) MODE-LINE-WINDOW
    (SETQ MODE-LINE-WINDOW
          (OR (SEND SELF :GET-PANE :MODE-LINE-WINDOW)
              (SEND TV:SUPERIOR :MODE-LINE-WINDOW)))))

(DEFMETHOD (ZMAIL-FRAME-MIXIN :EDITOR-WINDOWS) ()
  (SUBSET #'(LAMBDA (W) (TYPEP W 'DISPLAYER))
          (APPEND (SORT TV:EXPOSED-INFERIORS
                        #'(LAMBDA (W1 W2)
                            (< (TV:SHEET-X-OFFSET W1) (TV:SHEET-X-OFFSET W2))))
                  (IF (EQ MODE-LINE-WINDOW (SEND *WINDOW* :SUPERIOR))
                      (LIST *WINDOW*)
                    (SEND MODE-LINE-WINDOW :EXPOSED-INFERIORS)))))

(DEFMETHOD (ZMAIL-FRAME-MIXIN :TOP-OF-EDITOR-HIERARCHY) ()
  (SEND TV:SUPERIOR :TOP-OF-EDITOR-HIERARCHY))

;Don't let BASIC-FRAME's method alter our selection substitute.
;We do that manually whenever desired.
(DEFMETHOD (ZMAIL-FRAME-MIXIN :INFERIOR-SELECT) (IGNORE) T)

(DEFMETHOD (ZMAIL-FRAME-MIXIN :PRINT-NOTIFICATION) (TIME STRING WINDOW-OF-INTEREST)
  (WINDOW-TYPEIN-NOTIFICATION SELF TIME STRING WINDOW-OF-INTEREST))

(DEFFLAVOR ZMAIL-COMMAND-LOOP-MIXIN
           (TV:IO-BUFFER
            EDITOR-CLOSURE)
        ()
  (:INIT-KEYWORDS :EDITOR-CLOSURE-VARIABLES :COMTAB :MODE-LINE-LIST)
  :GETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS TV:BASIC-FRAME)
  (:REQUIRED-METHODS :TOP-LEVEL-TAG :PROCESS-COMMAND-CHAR :PROCESS-SPECIAL-COMMAND))

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :BEFORE :INIT) (PLIST)
  (UNLESS (VARIABLE-BOUNDP EDITOR-CLOSURE)
    (LET ((STANDARD-INPUT (SEND TV:SUPERIOR :STANDARD-INPUT-FOR-PANES))
          (*MODE-LINE-LIST* (GET PLIST :MODE-LINE-LIST))
          (*COMTAB* (OR (GET PLIST :COMTAB) *STANDARD-COMTAB*)))
      (SETQ EDITOR-CLOSURE
            (MAKE-EDITOR-CLOSURE (GET PLIST :EDITOR-CLOSURE-VARIABLES) NIL)))))

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :BEFORE :KILL) ()
  (SETQ *EDITORS-WHOSE-MODES-TO-RESET*
        (DELQ EDITOR-CLOSURE *EDITORS-WHOSE-MODES-TO-RESET*)))

(DEFVAR *CURRENT-COMMAND-LOOP* :UNBOUND
  "The window whose :COMMAND-LOOP method we are inside.")

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :AROUND :COMMAND-LOOP)
           (CONT MT ARGS &OPTIONAL EDITOR-CLOSURE-1 &REST IGNORE)
  MT   ;;; Let the continuation find its own mapping table.
       ;;; It saves hair, and the time is insignificant here.
  (CONDITION-BIND ((UNKNOWN-SPECIAL-COMMAND
                     'ZMAIL-COMMAND-LOOP-UNKNOWN-SPECIAL-COMMAND))
    (LET ((*CURRENT-COMMAND-LOOP* SELF))
      (LEXPR-FUNCALL (OR EDITOR-CLOSURE-1 EDITOR-CLOSURE) CONT ARGS))))

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :BEFORE :COMMAND-LOOP) ()
  (TV:PROCESS-TYPEAHEAD TV:IO-BUFFER
                        #'(LAMBDA (CH)
                            (IF (AND (CONSP CH)
                                     (MEMQ (CAR CH) '(REDISPLAY SELECT-WINDOW
                                                      CONFIGURATION-CHANGED)))
                                NIL CH))))

;;; This is the main command loop for zmail, and also for such things`
;;; as editing the profile, creating filters or universes.
;;; But when you actually go into editing the text of a message, init file, etc.
;;; then the :EDIT method is used instead.
(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :COMMAND-LOOP) ()
  (*CATCH (SEND SELF :TOP-LEVEL-TAG)
    (DO ((*NUMERIC-ARG-P* NIL NIL)
         (*NUMERIC-ARG* 1 1)
         (*ZMAIL-COMMAND-BUTTON* NIL NIL)
         (*CURRENT-COMMAND* NIL)
         (*LAST-SUMMARY-MOUSE-ITEM* NIL)
         (*LAST-COMMAND-CHAR*)
         (PROMPT-ARRAY (MAKE-PROMPT-ARRAY)))
        (NIL)
      (*CATCH 'RETURN-TO-COMMAND-LOOP
        (*CATCH 'TOP-LEVEL
          (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to ZMAIL command loop.")
            (*CATCH 'ZWEI-COMMAND-LOOP
              (PROG ()
                    (CLEAR-PROMPTS)
                    (SEND SELF :REDISPLAY)
                    (TICK)
                 GETCHAR
                    ;; Process any responses from the background.
                    ;; Not patched in system 91.
                    (DO () ((NULL (CONTENTS *BACKGROUND-RESPONSE-QUEUE*)))
                      (LET ((RESPONSE (POP (CONTENTS *BACKGROUND-RESPONSE-QUEUE*))))
                        (APPLY #'ZMAIL-BACKGROUND-REQUEST (CDR RESPONSE))))
                    (SETQ *LAST-COMMAND-CHAR*
                          (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
                            (INPUT-WITH-PROMPTS *STANDARD-INPUT* :ANY-TYI)))
                    (AND (NULL *LAST-COMMAND-CHAR*)     ;EOF
                         (*THROW (SEND SELF :TOP-LEVEL-TAG) T))
                    (LET ((DEGREE (IF (CONSP *LAST-COMMAND-CHAR*)
                                      (LEXPR-SEND SELF :PROCESS-SPECIAL-COMMAND
                                                          *LAST-COMMAND-CHAR*)
                                    (SEND SELF :PROCESS-COMMAND-CHAR
                                                  *LAST-COMMAND-CHAR*))))
                      (AND (EQ DEGREE :ARGUMENT) (GO GETCHAR))
                      (MUST-REDISPLAY *WINDOW* DEGREE))
                    (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)
                 NIL)))))
      (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE))))

(DEFUN ZMAIL-COMMAND-LOOP-UNKNOWN-SPECIAL-COMMAND (&REST IGNORE)
  (COND ((MEMQ (CAR *LAST-COMMAND-CHAR*)
               (FUNCALL *CURRENT-COMMAND-LOOP* :PROCESS-SPECIAL-COMMAND :WHICH-OPERATIONS))
         (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
         (*THROW 'RETURN-TO-COMMAND-LOOP NIL))))

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :REDISPLAY) ()
  (REDISPLAY-ALL-WINDOWS)
  (SEND *TYPEIN-WINDOW* :COMMAND-LOOP-REDISPLAY))

(DEFSELECT ZMAIL-COMMAND-LIST-DEFAULT
  ((SUMMARY-EXECUTE :TYPEOUT-EXECUTE :EXECUTE) (FUNCTION &REST ARGS)
    (APPLY FUNCTION ARGS))                      ;Request from typeout or summary menus
  ;; This will do nothing at this level,
  ;; but eventually this command loop will return
  ;; to top level and the queue will be read.
  (READ-BACKGROUND-RESPONSE-QUEUE ()
    DIS-NONE)
  ((REDISPLAY CONFIGURATION-CHANGED) (&OPTIONAL IGNORE)
    DIS-NONE)
  (SCROLL (WINDOW NLINES TYPE)                  ;Scroll bar command
    (OR (EQ TYPE :RELATIVE)
        (SETQ TYPE :START
              NLINES (FORWARD-LINE (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))
                                   NLINES T)))
    (REDISPLAY WINDOW TYPE NLINES)
    DIS-NONE)
  (SELECT-WINDOW (WINDOW)                       ;Moused a window, edit there
    (TV:WITH-SELECTION-SUBSTITUTE (WINDOW *ZMAIL-WINDOW*)
;      (MAKE-WINDOW-CURRENT WINDOW)
      (*CATCH 'ABORT-STANDALONE-EDIT
        (SEND *WINDOW* :EDIT)))
    DIS-NONE)
  (:MOUSE-BUTTON (&REST IGNORE)                 ;Mouse char, edit that window
    (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
    (SEND SELF :PROCESS-SPECIAL-COMMAND 'SELECT-WINDOW *WINDOW*))
  )

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN :PROCESS-COMMAND-CHAR) (CHAR)
  (SELECTOR CHAR CHAR=
    (#\CLEAR-SCREEN
     (SEND SELF :REFRESH))
    (#\CONTROL-R
     (SEND SELF :PROCESS-SPECIAL-COMMAND 'SELECT-WINDOW *WINDOW*))
    (#\BREAK
     (COM-ZMAIL-BREAK))
    (#\ABORT
     (*THROW (SEND SELF :TOP-LEVEL-TAG) NIL))
    (OTHERWISE
     (BARF)))
  DIS-NONE)

(DEFFLAVOR ZMAIL-COMMAND-LOOP-MIXIN-WITH-SUMMARY
           ()
           ()
  (:REQUIRED-FLAVORS ZMAIL-COMMAND-LOOP-MIXIN))

(DEFMETHOD (ZMAIL-COMMAND-LOOP-MIXIN-WITH-SUMMARY :REDISPLAY) (&AUX REDISPLAY-SUPPRESSED)
  (SETQ REDISPLAY-SUPPRESSED (REDISPLAY-ALL-WINDOWS))
  (AND (NOT REDISPLAY-SUPPRESSED)
       (SEND *TYPEIN-WINDOW* :COMMAND-LOOP-REDISPLAY))
  (AND (TV:SHEET-EXPOSED-P *SUMMARY-WINDOW*) (NOT REDISPLAY-SUPPRESSED)
       (SEND *SUMMARY-WINDOW* :REDISPLAY-AS-NECESSARY)))

(defvar *ZMAIL-DEFAULT-BACKGROUND-PRIROITY* -5)

(DEFCONST ZMAIL-FRAME-EDITOR-CLOSURE-VARIABLES
  (MERGE-CLOSURE-VARIABLE-LISTS
    '((*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
      (standard-output si:syn-terminal-io)
      (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
      (QUERY-IO SYN-TYPEIN-WINDOW-IO)
      (*ZMAIL-WINDOW* *ZMAIL-WINDOW*)
      (*DRAFT-HEADER-WINDOW* NIL)
      (*DRAFT-TEXT-WINDOW* NIL)
      (*MSG-WINDOW* NIL)        ;Initialized below
      (*SUMMARY-WINDOW* NIL)
      (*FILTER-WINDOW* NIL)
      (*PROFILE-WINDOW* NIL)
      (*PROFILE-EDITOR-WINDOW* NIL)             ;Initialized below
      (*COMMAND-MENU* NIL)
      (*KEYWORD-WINDOW* NIL)
      (*WINDOW-CONFIGURATION* NIL)
      (*CURRENT-MSG-NAME* NIL)
      (*ZMAIL-INTERVAL-NAME* NIL)
      (*ZMAIL-BUFFER* NIL)
      (*ZMAIL-BUFFER-LIST* NIL)
      (*PRIMARY-ZMAIL-BUFFER* NIL)
      (*ZMAIL-FILE-NAME* NIL)
      (*MSG* NIL)
      (*MSG-NO* NIL)
      (*MSG-POINT-PDL* NIL)
      (*MAJOR-MODE* 'TEXT-MODE)
      (*ZMAIL-BACKGROUND-PROCESS*
        (SI:MAKE-PROCESS "Zmail background"
                         :PRIORITY *ZMAIL-DEFAULT-BACKGROUND-PRIROITY*
                         :FLAVOR 'ZMAIL-BACKGROUND-PROCESS
                         :SPECIAL-PDL-SIZE 4000
                         :REGULAR-PDL-SIZE 4000.))
      (*ZMAIL-BACKGROUND-PROCESS-LOCK* NIL)     ;Initialized below
      (*ZMAIL-BACKGROUND-REQUEST-CELL* NIL)     ;Initialized below
      (*BACKGROUND-RESPONSE-QUEUE* NIL)
      (*CURRENT-MSG-KEYWORDS-STRING* NIL)
      (*MODE-LINE-LIST*
        '("ZMail " *ZMAIL-FILE-NAME* *CURRENT-MSG-NAME*
          (*CURRENT-MSG-KEYWORDS-STRING* "  " *CURRENT-MSG-KEYWORDS-STRING*)
          (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*)
          (*MSG-MORE-STRING* "  " *MSG-MORE-STRING*)))
      (*SELECTABLE-MODE-LINE-ELEMENTS*
        '((*CURRENT-MSG-KEYWORDS-STRING* . COM-ZMAIL-KEYWORDS)
          (*MSG-MORE-STRING* . COM-ZMAIL-MODE-LINE-SCROLL)
          ))
      (*MSG-MORE-STRING* NIL)
      (*DRAFT-LIST* NIL)
      (*DEFAULT-MOVE-ZMAIL-BUFFER* NIL)
      (*MOVE-ZMAIL-BUFFER-MENU* NIL)
      (*SELECT-ZMAIL-BUFFER-MENU* NIL)
      (*ZMAIL-MAP-COMMAND-MENU* NIL)
      (*UNIVERSE-SELECTION-MENU* NIL)
      (*FILTER-SELECTION-FRAME* NIL)
      (*UNIVERSE-DEFINITION-FRAME* NIL)
      (*OVERLYING-WINDOW* NIL)
      (*POP-UP-MINI-BUFFER-EDITOR* NIL)
      )
    TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFUN INITIALIZE-ZMAIL-FRAME-EDITOR-CLOSURE (ZMAIL-FRAME
                                              &AUX (IOB (SEND ZMAIL-FRAME :IO-BUFFER)))
  (SETQ *DRAFT-HEADER-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'DRAFT-HEADER-WINDOW)
        *DRAFT-TEXT-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'DRAFT-TEXT-WINDOW)
        *MSG-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'MSG-WINDOW)
        *SUMMARY-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'SUMMARY-WINDOW)
        *FILTER-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'FILTER-WINDOW)
        *COMMAND-MENU* (SEND ZMAIL-FRAME :GET-PANE 'COMMAND-MENU))
  (SETQ *WINDOW-CONFIGURATION* (SEND ZMAIL-FRAME :CONFIGURATION))
  (SETQ *OVERLYING-WINDOW*
        (TV:MAKE-WINDOW 'ZMAIL-OVERLYING-WINDOW
                        :IO-BUFFER IOB
                        :LABEL "" :SUPERIOR ZMAIL-FRAME))
  (SETQ *POP-UP-MINI-BUFFER-EDITOR*
        (TV:MAKE-WINDOW 'TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS
                        :IO-BUFFER (SEND ZMAIL-FRAME :IO-BUFFER)))
  (SETQ *FILTER-SELECTION-FRAME*
        (TV:MAKE-WINDOW 'FILTER-SELECTION-FRAME
                        :IO-BUFFER IOB
                        :SAVE-BITS T
                        :SUPERIOR ZMAIL-FRAME))
  (SETQ *UNIVERSE-DEFINITION-FRAME*
        (TV:MAKE-WINDOW 'UNIVERSE-DEFINITION-FRAME
                        :IO-BUFFER IOB
                        :SAVE-BITS T
                        :SUPERIOR ZMAIL-FRAME))
  (SETQ *KEYWORD-WINDOW* (TV:MAKE-WINDOW 'POP-UP-ZMAIL-MULTIPLE-MENU
                                         :NEW-FUNCTION 'MULTIPLE-MENU-NEW-KEYWORD
                                         :SUPERIOR ZMAIL-FRAME))
  (SETQ *SELECT-ZMAIL-BUFFER-MENU*
        (TV:MAKE-WINDOW 'CLICK-REMEMBERING-POP-UP-MENU
                        :COLUMNS 2 :ITEM-LIST NIL
                        :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
                        :SUPERIOR ZMAIL-FRAME))
  (SETQ *MOVE-ZMAIL-BUFFER-MENU*
        (TV:MAKE-WINDOW 'CLICK-REMEMBERING-POP-UP-MENU
                        :COLUMNS 2 :ITEM-LIST NIL
                        :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
                        :SUPERIOR ZMAIL-FRAME))
  (SETQ *ZMAIL-MAP-COMMAND-MENU*
        (TV:MAKE-WINDOW 'ZMAIL-DYNAMIC-MOMENTARY-COMMAND-MENU
                        :COLUMNS 2
                        :ITEM-LIST-POINTER
                        '*ZMAIL-MAP-COMMAND-ALIST*
                        :SUPERIOR ZMAIL-FRAME))
  (SETQ *UNIVERSE-SELECTION-MENU*
        (TV:MAKE-WINDOW 'TV:MOMENTARY-MULTIPLE-ITEM-LIST-MENU
                        :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
                        :LABEL "Universe:"
                        :SUPERIOR ZMAIL-FRAME))
  ;; Both of these windows should use the same editor closure.
  (SETQ *PROFILE-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'PROFILE-WINDOW))
  (SETQ *PROFILE-EDITOR-WINDOW* (SEND ZMAIL-FRAME :GET-PANE 'PROFILE-EDITOR-WINDOW))
  (SEND *PROFILE-EDITOR-WINDOW* :SET-EDITOR-CLOSURE
        (SEND *PROFILE-WINDOW* :EDITOR-CLOSURE))
  (SEND *PROFILE-EDITOR-WINDOW* :SET-INTERVAL
        (MAKE-INSTANCE 'ZMAIL-PROFILE-BUFFER))
  ;; These three windows should all use the same EDITOR-CLOSURE
  ;; since when editing mail one switches among the three.
  (SEND *DRAFT-TEXT-WINDOW* :SET-EDITOR-CLOSURE
        (SEND *MSG-WINDOW* :EDITOR-CLOSURE))
  (SEND *DRAFT-HEADER-WINDOW* :SET-EDITOR-CLOSURE
        (SEND *MSG-WINDOW* :EDITOR-CLOSURE))
  (SETQ *ZMAIL-BACKGROUND-REQUEST-CELL*
        (LOCF (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL *ZMAIL-BACKGROUND-PROCESS*))
        *BACKGROUND-RESPONSE-QUEUE*
        (LOCF (ZMAIL-BACKGROUND-PROCESS-RESPONSE-QUEUE *ZMAIL-BACKGROUND-PROCESS*))
        *ZMAIL-BACKGROUND-PROCESS-LOCK*
        (LOCF (ZMAIL-BACKGROUND-PROCESS-LOCK *ZMAIL-BACKGROUND-PROCESS*)))
  (SEND *ZMAIL-BACKGROUND-PROCESS* :PRESET 'ZMAIL-BACKGROUND ZMAIL-FRAME)
  (INITIALIZE-TOP-LEVEL-EDITOR *MSG-WINDOW*))

;;; This is the flavor that does it all, really, instance variables here should just change
;;; defaults.
(DEFFLAVOR ZMAIL-FRAME
           (STANDARD-INPUT-FOR-PANES
;Not removed in system 93.
;           OVERLYING-WINDOW
            )
       (ZMAIL-COMMAND-LOOP-MIXIN-WITH-SUMMARY ZMAIL-COMMAND-LOOP-MIXIN ZMAIL-FRAME-MIXIN
        TV:ANY-TYI-MIXIN TV:STREAM-MIXIN TV:PROCESS-MIXIN TV:SELECT-MIXIN
        TV:FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
        TV:INITIALLY-INVISIBLE-MIXIN TV:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
        ZWEI-MACRO-MIXIN)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BORDER-MARGIN-WIDTH 0))

(DEFVAR *MSG-WINDOW-CONFIGURATIONS* '(:MSG :BOTH :NEW))

(DEFMETHOD (ZMAIL-FRAME :BEFORE :INIT) (PLIST &AUX MODE-LINE-LINE-HEIGHT
                                                   MODE-LINE-HEIGHT EDITOR-MODE-LINE-HEIGHT)
  (PUTPROP PLIST T :SAVE-BITS)                  ;Things depend on working like this
  (SETQ TV:PROCESS '(ZMAIL-PROCESS-TOP-LEVEL :SPECIAL-PDL-SIZE 4000
                                             :REGULAR-PDL-SIZE 4000.))
  (SETQ MODE-LINE-LINE-HEIGHT (+ 2 (MAX (FONT-CHAR-HEIGHT TV:(SCREEN-DEFAULT-FONT
                                                              (SHEET-GET-SCREEN SUPERIOR)))
                                       (FONT-CHAR-HEIGHT FONTS:SEARCH)))
        MODE-LINE-HEIGHT (+ 10 (* 3 MODE-LINE-LINE-HEIGHT))
        EDITOR-MODE-LINE-HEIGHT (+ 10 (* 4 MODE-LINE-LINE-HEIGHT)))
  (SETQ TV:PANES `((MODE-LINE-WINDOW ZMAIL-MOUSE-SENSITIVE-MODE-LINE-PANE
                                     :HEIGHT ,MODE-LINE-HEIGHT
                                     :BLINKER-DESELECTED-VISIBILITY :OFF)
                   (MSG-WINDOW ZMAIL-WINDOW :LABEL "Message"
                                            :BLINKER-DESELECTED-VISIBILITY :OFF
                                            :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
                                              ,*EDIT-MSG-DOCUMENTATION*)
                   (DRAFT-HEADER-WINDOW ZMAIL-WINDOW :LABEL "Headers")
                   (DRAFT-TEXT-WINDOW ZMAIL-WINDOW :LABEL "Mail")
                   (SUMMARY-WINDOW ZMAIL-SUMMARY-SCROLL-WINDOW)
                   (FILTER-WINDOW ZMAIL-FILTER-FRAME)
                   (PROFILE-WINDOW ZMAIL-PROFILE-FRAME)
                   (PROFILE-EDITOR-WINDOW ZMAIL-WINDOW :LABEL "Profile"
                                          :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
                                            "L: edit profile buffer.")
                   (COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                 :COLUMNS 5 :ITEM-LIST ,*ZMAIL-COMMAND-ALIST*)
                   (NO-FILTER-COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                           :COLUMNS 6
                                           :ITEM-LIST ,*ZMAIL-NO-FILTER-COMMAND-ALIST*)
                   (FILTER-COMMAND-MENU ZMAIL-MAIN-COMMAND-MENU-PANE
                                        :COLUMNS 5
                                        :ITEM-LIST ,*ZMAIL-FILTER-COMMAND-ALIST*)
                   (BUTTONS-FRAME TV:BUTTONS-FRAME
                                  :PANES ((UNIVERSE-BUTTON TV:MEDIUM-BUTTON-PANE
                                           :NAME "Just current message"
                                           :DOCUMENTATION ,*UNIVERSE-BUTTON-DOCUMENTATION*)
                                          (FILTER-BUTTON TV:MEDIUM-BUTTON-PANE
                                           :NAME "All"
                                           :DOCUMENTATION ,*FILTER-BUTTON-DOCUMENTATION*))))

        TV:SUBSTITUTIONS `((STANDARD-MODE-LINE . (MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                           (STANDARD-EDITOR-MODE-LINE
                             . (MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                           (STANDARD-HEADER . (DRAFT-HEADER-WINDOW :LIMIT (3 8 :LINES)
                                                                   :ASK :HEADER-WINDOW-HEIGHT))
                           (STANDARD-COMMAND-MENU . (COMMAND-MENU :ASK :PANE-SIZE)))

        TV:CONSTRAINTS '((:MSG . ((MSG-WINDOW COMMAND-MENU MODE-LINE-WINDOW)
                                  (STANDARD-MODE-LINE
                                   STANDARD-COMMAND-MENU)
                                  ((MSG-WINDOW :EVEN))))
                         (:SUMMARY . ((SUMMARY-WINDOW MODE-LINE-WINDOW)
                                      (STANDARD-MODE-LINE)
                                      ((SUMMARY-WINDOW :EVEN))))
                         (:BOTH . ((SUMMARY-WINDOW COMMAND-MENU MSG-WINDOW MODE-LINE-WINDOW)
                                   (STANDARD-MODE-LINE
                                    STANDARD-COMMAND-MENU
                                    (SUMMARY-WINDOW :EVAL (TV:CONSTRAINT-ROUND
                                                            (* *SUMMARY-WINDOW-FRACTION*
                                                               TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                            '(NIL :LINES)
                                                            TV:**CONSTRAINT-NODE**)))
                                   ((MSG-WINDOW :EVEN))))
                         (:NEW . ((SUMMARY-WINDOW NO-FILTER-COMMAND-MENU BUTTONS-FRAME
                                   FILTER-COMMAND-MENU MSG-WINDOW MODE-LINE-WINDOW)
                                  (STANDARD-MODE-LINE
                                   (NO-FILTER-COMMAND-MENU :ASK :PANE-SIZE)
                                   (FILTER-COMMAND-MENU :ASK :PANE-SIZE)
                                   (SUMMARY-WINDOW :EVAL (TV:CONSTRAINT-ROUND
                                                           (* *SUMMARY-WINDOW-FRACTION*
                                                              TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                           '(NIL :LINES)
                                                           TV:**CONSTRAINT-NODE**)))
                                  ((BUTTONS-FRAME :ASK :PANE-SIZE))
                                  ((MSG-WINDOW :EVEN))))
                         ;; mail composing configurations
                         (:SEND . ((DRAFT-HEADER-WINDOW DRAFT-TEXT-WINDOW MODE-LINE-WINDOW)
                                   (STANDARD-EDITOR-MODE-LINE
                                    STANDARD-HEADER)
                                   ((DRAFT-TEXT-WINDOW :EVEN))))
                         (:REPLY . ((MSG-WINDOW DRAFT-HEADER-WINDOW DRAFT-TEXT-WINDOW
                                     MODE-LINE-WINDOW)
                                    (STANDARD-EDITOR-MODE-LINE
                                     (MSG-WINDOW 0.50s0 :LINES)
                                     STANDARD-HEADER)
                                    ((DRAFT-TEXT-WINDOW :EVEN))))
                         (:FILTER . ((SUMMARY-WINDOW FILTER-WINDOW)
                                     ((SUMMARY-WINDOW
                                        :EVAL (TV:CONSTRAINT-ROUND
                                                (* (OR *FILTER-SUMMARY-WINDOW-FRACTION*
                                                       *SUMMARY-WINDOW-FRACTION*)
                                                   TV:**CONSTRAINT-TOTAL-HEIGHT**)
                                                '(NIL :LINES)
                                                TV:**CONSTRAINT-NODE**)))
                                     ((FILTER-WINDOW :EVEN))))
                         (:PROFILE . ((PROFILE-WINDOW
                                       PROFILE-EDITOR-WINDOW MODE-LINE-WINDOW)
                                      (STANDARD-EDITOR-MODE-LINE
                                       (PROFILE-WINDOW 0.425s0))
                                      ((PROFILE-EDITOR-WINDOW :EVEN))))
                         )
        TV:CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*)
  (SETQ STANDARD-INPUT-FOR-PANES (MAKE-MACRO-STREAM SELF))
  (LET ((*STANDARD-INPUT* STANDARD-INPUT-FOR-PANES)
        (*ZMAIL-WINDOW* SELF)
        (*COMTAB* *STANDARD-COMTAB*))
    (SETQ EDITOR-CLOSURE (MAKE-EDITOR-CLOSURE ZMAIL-FRAME-EDITOR-CLOSURE-VARIABLES NIL))))

(DEFMETHOD (ZMAIL-FRAME :TOP-OF-EDITOR-HIERARCHY) () SELF)

(COMMENT
(DEFMETHOD (ZMAIL-FRAME :EDITOR-WINDOWS) ()
  (SUBSET #'TV:SHEET-EXPOSED-P
          (LIST *PROFILE-EDITOR-WINDOW*
                *MSG-WINDOW* *DRAFT-HEADER-WINDOW* *DRAFT-TEXT-WINDOW* *MINI-BUFFER-WINDOW*))))

(DEFMETHOD (ZMAIL-FRAME :AFTER :INIT) (IGNORE)
  (SETQ MODE-LINE-WINDOW (SEND SELF :GET-PANE 'MODE-LINE-WINDOW))
  (FUNCALL EDITOR-CLOSURE 'INITIALIZE-ZMAIL-FRAME-EDITOR-CLOSURE SELF))

;;; Position all the needed windows for this operation
(DEFMETHOD (ZMAIL-FRAME :SET-WINDOW-CONFIGURATION)
           (NEW-CONFIGURATION &OPTIONAL STARTING-WINDOW
                              &AUX EXPOSE-P SELECT-P)
  (SETQ *WINDOW-CONFIGURATION* NEW-CONFIGURATION)
  (TV:DELAYING-SCREEN-MANAGEMENT
    (SI:PAGE-IN-ARRAY TV:SCREEN-ARRAY)
    (SETQ SELECT-P (SEND SELF :SELF-OR-SUBSTITUTE-SELECTED-P))
    (AND (SETQ EXPOSE-P TV:EXPOSED-P)           ;Make things look less spastic
         (SEND SELF :DEEXPOSE :DEFAULT :NOOP))
    (SEND SELF :SET-SELECTION-SUBSTITUTE NIL)
    (SEND SELF :SET-CONFIGURATION NEW-CONFIGURATION)
    (COND ((MEMQ *MSG-WINDOW* TV:EXPOSED-INFERIORS)
           (SETQ STARTING-WINDOW *MSG-WINDOW*))
          ((MEMQ *FILTER-WINDOW* TV:EXPOSED-INFERIORS)
           (SEND SELF :SET-SELECTION-SUBSTITUTE *FILTER-WINDOW*)
           (SEND *FILTER-WINDOW* :INITIALIZE))
          ((MEMQ *PROFILE-EDITOR-WINDOW* TV:EXPOSED-INFERIORS)
           (SEND *PROFILE-WINDOW* :INITIALIZE)
           (SETQ STARTING-WINDOW *PROFILE-EDITOR-WINDOW*)))
    (AND (MEMQ *SUMMARY-WINDOW* TV:EXPOSED-INFERIORS)
         (NOT (STRING-EQUAL (TV:LABEL-STRING (SEND *SUMMARY-WINDOW* :LABEL))
                            *SUMMARY-WINDOW-LABEL*))
         (SEND *SUMMARY-WINDOW* :SET-LABEL (STRING-APPEND *SUMMARY-WINDOW-LABEL*)))
    (AND EXPOSE-P (SEND SELF :EXPOSE))
    (COND (STARTING-WINDOW
           (MAKE-WINDOW-CURRENT STARTING-WINDOW NIL)
           (PREPARE-WINDOW-FOR-REDISPLAY STARTING-WINDOW)
           (SETQ *TYPEOUT-WINDOW* (SEND STARTING-WINDOW :TYPEOUT-WINDOW))
           (SETQ *TERMINAL-IO* *TYPEOUT-WINDOW*)
           (OR (EQ STARTING-WINDOW *MSG-WINDOW*)
               (EQ STARTING-WINDOW *PROFILE-EDITOR-WINDOW*)
               (SEND SELF :SET-SELECTION-SUBSTITUTE STARTING-WINDOW)))
          ((EQ NEW-CONFIGURATION :SUMMARY)
           (SETQ *TYPEOUT-WINDOW* (SEND *SUMMARY-WINDOW* :TYPEOUT-WINDOW)
                 *TERMINAL-IO* *TYPEOUT-WINDOW*)))
    (AND SELECT-P (SEND SELF :SELECT))))

(DEFMETHOD (ZMAIL-FRAME :BEFORE :KILL) ()
  (DOLIST (BUFFER (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*ZMAIL-BUFFER-LIST*))
    (AND (ZMAIL-BUFFER-DISK-P BUFFER)
         (ZMAIL-DISK-BUFFER-STREAM BUFFER)
         (CLOSE (ZMAIL-DISK-BUFFER-STREAM BUFFER) :ABORT))))

(DEFMETHOD (ZMAIL-FRAME :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SEND (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*OVERLYING-WINDOW*) :FULL-SCREEN))

;;; Called by the macro stream system
(DEFMETHOD (ZMAIL-FRAME :READ-MACRO-LINE) (PROMPT &AUX (*CURRENT-COMMAND* 'READ-MACRO-LINE))
  (LET ((*NUMERIC-ARG-P* NIL) (*NUMERIC-ARG* 1))
    (TYPEIN-LINE-READLINE PROMPT)))

(DEFMETHOD (ZMAIL-FRAME :SET-ZMAIL-BUFFER) (PATHNAME)
  (AND (EQ PATHNAME T)                          ;Re-init
       (LET ()
         (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
         (SETQ PATHNAME NIL
               *PRIMARY-ZMAIL-BUFFER* NIL
               *ZMAIL-BUFFER-LIST* NIL)))
  (COMMAND-BUFFER-PUSH `(:EXECUTE STARTUP-ZMAIL-BUFFER ,PATHNAME)))

;;; Initialization and outside functions

(DEFVAR *ZMAIL-USER*)                           ;USER-ID that last ran this

(DEFUN INITIALIZE-ZMAIL ()
  (AND (BOUNDP '*ZMAIL-WINDOW*) (SEND *ZMAIL-WINDOW* :KILL))
  (SETQ *ZMAIL-WINDOW* (TV:MAKE-WINDOW 'ZMAIL-FRAME :NAME "Main ZMail window"))
  (INITIALIZE-ZMAIL-COMTABS (SEND *ZMAIL-WINDOW* :FUNCALL-INSIDE-YOURSELF 'EVAL '*MODE-COMTAB*))
  (OR (ASSQ #/M TV:*SYSTEM-KEYS*)
      (TV:ADD-SYSTEM-KEY #\M 'ZMAIL-FRAME "ZMail" T))
  (TV:ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN "Mail"
    '(TV:SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'ZMAIL-FRAME)
    "Select ZMail, to send or receive mail."
    "Inspect")
  (SETQ *ZMAIL-PATHNAME-DEFAULTS* (FS:MAKE-PATHNAME-DEFAULTS))
  (RESET-ZMAIL-USER)
  (SEND *ZMAIL-WINDOW* :FUNCALL-INSIDE-YOURSELF
           *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*)
  (SEND *ZMAIL-WINDOW* :ACTIVATE))              ;Now ok to call it up

;;; Top level function
(DEFUN ZMAIL (&OPTIONAL SAVED-MAIL-PATHNAME)
  "Calls up a ZMAIL, a mail reading program.
If SAVED-MAIL-PATHNAME is specified it should be either
  - :RELOAD, to reinitialize for user /(reading in mail), or
  - a pathname, to read in a mail file."
  (COND ((memq SAVED-MAIL-PATHNAME '(RELOAD :reload))
         (INITIALIZE-ZMAIL)
         (SETQ SAVED-MAIL-PATHNAME NIL)))
  (FS:FORCE-USER-TO-LOGIN)
  (SEND *ZMAIL-WINDOW* :SET-ZMAIL-BUFFER SAVED-MAIL-PATHNAME)
  (SEND *ZMAIL-WINDOW* :SELECT)
  (TV:AWAIT-WINDOW-EXPOSURE)
  T)

(defun find-or-maybe-initialize-zmail-window (&optional reason (timeout (* 60. 60. 3.)) default)
  (or (tv:find-window-of-flavor 'zmail-frame)
      (and (yes-or-no-p-with-timeout timeout default "Start up ZMail~@[ ~A~]?" reason)
           (prog2 (initialize-zmail) *zmail-window*))))

;;; This is the initial function for the zmail window
(DEFUN ZMAIL-PROCESS-TOP-LEVEL (WINDOW)
  (PROCESS-WAIT "Select"
                #'(LAMBDA (WINDOW)
                    (DO W TV:SELECTED-WINDOW (TV:SHEET-SUPERIOR W) (NULL W)
                        (AND (EQ WINDOW W) (RETURN T))))
                WINDOW)
  (SEND WINDOW :SET-SELECTION-SUBSTITUTE NIL)
  (AND (STRING-EQUAL USER-ID "")
       (LET ((TERMINAL-IO (SEND (SEND WINDOW :GET-PANE 'MSG-WINDOW) :TYPEOUT-WINDOW)))
         (SEND *TERMINAL-IO* :OUTPUT-HOLD-EXCEPTION)
         (FS:FORCE-USER-TO-LOGIN)))
  (AND (NULL *ZMAIL-USER*)
       (SEND WINDOW :NULL-STARTUP-SETUP))
  (SEND WINDOW :COMMAND-LOOP)
  (TV:DESELECT-AND-MAYBE-BURY-WINDOW WINDOW)
  (SI:PROCESS-WAIT-FOREVER))

(DEFVAR *NULL-STARTUP-MSG-INTERVAL* (CREATE-INTERVAL "Type the HELP key for help.
To read your new mail, click Left on /"Get New mail/".
To send a message, click Left on /"Mail/".
To send a bug report, click Middle on /"Mail/"."))

(DEFMETHOD (ZMAIL-FRAME :NULL-STARTUP-SETUP) ()
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (SEND *MSG-WINDOW* :SET-INTERVAL *NULL-STARTUP-MSG-INTERVAL*)
  (MUST-REDISPLAY *WINDOW* DIS-ALL)
  (SETQ *ZMAIL-FILE-NAME* "No current buffer"
        *CURRENT-MSG-NAME* NIL
        *CURRENT-MSG-KEYWORDS-STRING* NIL))

(DEFMETHOD (ZMAIL-FRAME :FUNCALL-INSIDE-YOURSELF) (FUNCTION &REST ARGS)
  (LEXPR-FUNCALL EDITOR-CLOSURE FUNCTION ARGS))

(DEFUN RESET-ZMAIL-USER (&AUX (OLD-FRACT *SUMMARY-WINDOW-FRACTION*))
  (RESET-USER-OPTIONS *ZMAIL-USER-OPTION-ALIST*)
  (DOLIST (VAR *ZMAIL-GLOBAL-INITIALIZATION-LIST*)
    (SET (CAR VAR) (CDR VAR)))
  (SEND *ZMAIL-WINDOW* :FUNCALL-INSIDE-YOURSELF
           #'(LAMBDA (LIST OLD-FRACT)
               (SELECT-ZMAIL-BUFFER NIL T)
               (ZMAIL-BACKGROUND-REQUEST-PUSH
                 '(ZMAIL-BACKGROUND-SET-INBOX-BUFFER NIL))
               (DOLIST (COM LIST)
                 (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM NIL))
               (SEND *PROFILE-WINDOW* :RESET-PROFILE)
               (SET-MAIN-WINDOW-CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*
                                              ( *SUMMARY-WINDOW-FRACTION* OLD-FRACT)))
           *ZMAIL-WHO-LINE-DOCUMENTATION-SYMBOLS* OLD-FRACT)
  (TV:SHEET-FORCE-ACCESS (*ZMAIL-WINDOW*)
    (SEND *ZMAIL-WINDOW* :REFRESH))
  (SETQ *zmail-init-loaded* nil
        *initialized-for-user* nil
        *ZMAIL-USER* NIL))

(ADD-INITIALIZATION 'RESET-ZMAIL-USER '(RESET-ZMAIL-USER) '(LOGOUT))

(DEFVAR *ZMAIL-PROFILE-LOADING-LOCK-CELL* ()
  "Lock cell to prevent two loadings of the ZMAIL profile")
(DEFVAR *ZMAIL-INIT-LOADED* NIL
  "Allows preloading of Zmail profile")

(DEFUN SET-ZMAIL-USER (&OPTIONAL SILENT &AUX (OLD-FRACT *SUMMARY-WINDOW-FRACTION*))
  (WITH-LOCK (*ZMAIL-PROFILE-LOADING-LOCK-CELL*)
    (UNLESS (EQUAL *ZMAIL-USER* USER-ID)
      (UNLESS *INITIALIZED-FOR-USER*
        (LET ((*INTERVAL*
                (OR *INTERVAL*                  ;Must not be NIL; would cause errors.
                    (WINDOW-INTERVAL *MSG-WINDOW*))))
          (TURN-ON-MODE *MAJOR-MODE*))
        (SETQ *INITIALIZED-FOR-USER* T))
      (LOAD-ZMAIL-INIT-FILE SILENT)
      (UPDATE-ALL-COMMANDS-ASSOCIATED-WITH-OPTIONS-DOCUMENTATION)
      (SET-MAIN-WINDOW-CONFIGURATION *DEFAULT-INITIAL-WINDOW-CONFIGURATION*
                                     ( *SUMMARY-WINDOW-FRACTION* OLD-FRACT))
      (SETQ *ZMAIL-USER* USER-ID))))

(DEFUN LOAD-ZMAIL-INIT-FILE (&OPTIONAL SILENT)
  (UNLESS (EQUAL *ZMAIL-INIT-LOADED* USER-ID)
    (WITH-LOCK (*ZMAIL-PROFILE-LOADING-LOCK-CELL*)
      (CONDITION-CASE ()
          (WITH-OPEN-FILE (STREAM (ZMAIL-INIT-FILE-PATHNAME)
                                  :CHARACTERS :DEFAULT)
            (UNLESS SILENT (FORMAT QUERY-IO "~&Loading ZMail init file ~A"
                                   (SEND STREAM :TRUENAME)))
            (FUNCALL (IF (SEND STREAM :CHARACTERS)
                         #'SI:READFILE-INTERNAL #'SI:FASLOAD-INTERNAL)
                     STREAM "ZWEI" T))
        (FS:FILE-NOT-FOUND
         NIL)
        (SYS:REMOTE-NETWORK-ERROR
         (FORMAT QUERY-IO "~&Network trouble encountered while looking for a ZMail init file.
It is impossible to tell whether you even have one.")
         (UNLESS (Y-OR-N-P "Proceed without loading the init file, if any? ")
           (FORMAT QUERY-IO "~&Pausing and trying again.")
           (SLEEP 30.)
           (LOAD-ZMAIL-INIT-FILE SILENT))))
      (SETQ *ZMAIL-INIT-LOADED* USER-ID))))

(DEFUN UPDATE-ALL-COMMANDS-ASSOCIATED-WITH-OPTIONS-DOCUMENTATION ()
  (DO L *ZMAIL-USER-OPTION-ALIST* (CDR L) (NULL L)
    (DOLIST (COM (GET (CAAR L) 'DOCUMENTATION-ASSOCIATED-COMMANDS))
      (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM)))
  (DOLIST (OPT *OPTIONS-NOT-IN-ALIST*)
    (DOLIST (COM (GET OPT 'DOCUMENTATION-ASSOCIATED-COMMANDS))
      (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM))))

(DEFVAR *ZMAIL-INIT-FILE-HOST* NIL)

(DEFUN ZMAIL-INIT-FILE-PATHNAME (&OPTIONAL (HOST *ZMAIL-INIT-FILE-HOST*))
  (FS:INIT-FILE-PATHNAME "ZMail" (OR HOST FS:USER-LOGIN-MACHINE)))

;;; Process a top-level ZMAIL command
(DEFMETHOD (ZMAIL-FRAME :PROCESS-COMMAND-CHAR) (CH)
  (SETQ *ZMAIL-COMMAND-BUTTON* :KBD)
  (ZMAIL-COMMAND-EXECUTE (COMMAND-LOOKUP CH *ZMAIL-COMTAB*)))

(DEFUN ZMAIL-COMMAND-EXECUTE (*CURRENT-COMMAND*)
  (COMMAND-EXECUTE *CURRENT-COMMAND* *LAST-COMMAND-CHAR*))

(DEFMETHOD (ZMAIL-FRAME :TOP-LEVEL-TAG) () 'EXIT-TOP-LEVEL)

(DEFMETHOD (ZMAIL-FRAME :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY #'ZMAIL-COMMAND-LIST ARGS))

(DEFSELECT (ZMAIL-COMMAND-LIST ZMAIL-COMMAND-LIST-DEFAULT)
  (:MENU (ITEM CH WINDOW)                       ;Request from the main menu
    (SET-COMMAND-BUTTON CH)
    (ZMAIL-COMMAND-EXECUTE (SEND WINDOW :EXECUTE-NO-SIDE-EFFECTS ITEM)))
  (:MOUSE-BUTTON (IGNORE WINDOW IGNORE IGNORE)
    (COND ((EQ (TV:SHEET-SUPERIOR WINDOW)
              (SEND *ZMAIL-WINDOW* :GET-PANE 'BUTTONS-FRAME))
           (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
           (COMMAND-WITH-UNIVERSE-OR-FILTER))
          ((EQ WINDOW *WINDOW*)
           (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
           (SEND SELF :PROCESS-SPECIAL-COMMAND 'SELECT-WINDOW *WINDOW*))
          (T
           ;; Get here if user clicks on a button in some other frame
           ;; after that level is already exiting;
           ;; such as, clicking twice on Exit in Profile mode.
           DIS-NONE)))
  (SUMMARY-MOUSE (ITEM IGNORE CHAR &AUX (MSG (CADR ITEM)))
    (SET-COMMAND-BUTTON CHAR)                   ;Clicking on mouse in summary window
    (ZMAIL-SUMMARY-MOUSE MSG))
  (SELECT-WINDOW (WINDOW)                       ;Moused a window, edit it
    (OR *MSG* (SEND *STANDARD-INPUT* :CLEAR-INPUT))     ;There is something UNTYI'ed
    (MAKE-WINDOW-CURRENT WINDOW)
    (ZMAIL-COMMAND-EXECUTE 'COM-EDIT-CURRENT-MSG))
  (GET-NEW-MAIL ()                              ;Clicked on the new mail ***
    (COM-GET-NEW-MAIL))
  (MODE-LINE (COMMAND BUTTON)
    (SET-COMMAND-BUTTON BUTTON)
    (ZMAIL-COMMAND-EXECUTE COMMAND))
  (READ-BACKGROUND-RESPONSE-QUEUE ()
    DIS-NONE))

;;; Parse message, this is called whenever we first care about a message, it returns a pointer
;;; to the status plist
(DEFUN ASSURE-MSG-PARSED (MSG &AUX PARSED-P STATUS)
  (SETQ PARSED-P (LOCF (MSG-PARSED-P MSG))
        STATUS (LOCF (MSG-STATUS MSG)))
  (LET ((DONE-P NIL))
    (%STORE-CONDITIONAL PARSED-P CURRENT-PROCESS NIL)
    (UNWIND-PROTECT
      (COND ((%STORE-CONDITIONAL PARSED-P NIL CURRENT-PROCESS)
             (LET ((*INTERVAL* (MSG-REAL-INTERVAL MSG)))
               (SEND (MSG-MAIL-FILE-BUFFER MSG) :PARSE-MSG MSG STATUS)
               (SETQ DONE-P T)))
            ((EQ (CDR PARSED-P) :KILLED)
             (ZMAIL-ERROR "Attempt to parse a dead message."))
            (T
             (PROCESS-WAIT "Parse" #'(LAMBDA (PARSED-P)
                                       (EQ (CDR PARSED-P) T))
                           PARSED-P)))
      (IF (EQ (CDR PARSED-P) SI:CURRENT-PROCESS)
          (RPLACD PARSED-P DONE-P))))
  STATUS)

(DEFUN SET-PARSED-MSG-HEADERS (MSG &OPTIONAL (STATUS (ASSURE-MSG-PARSED MSG))
                                   &AUX NEWSTAT)
  (CATCH-ERROR
    (UNWIND-PROTECT
      (LET (HEADERS-END-BP)
        (MULTIPLE-VALUE (NEWSTAT HEADERS-END-BP)
          (SEND (MSG-MAIL-FILE-BUFFER MSG) :PARSE-MSG-TEXT MSG STATUS))
        (PUTPROP (LOCF NEWSTAT) HEADERS-END-BP 'HEADERS-END-BP))
      (OR NEWSTAT (SETQ NEWSTAT '(LOSING-HEADERS "Error during parsing"))))
    NIL)
  (SETF (MSG-STATUS MSG) (APPEND (CAR STATUS) NEWSTAT))
  (MSG-PARSE-HOOK MSG STATUS)
  (SET-MSG-SUMMARY-LINE MSG STATUS))

;;; This function exists so that it can be advised
(DEFUN MSG-PARSE-HOOK (MSG STATUS)
  MSG STATUS)

(DEFUN ADD-HEADER-TO-MSG (MSG TYPE PROP &AUX STATUS HEADERS-END-BP LOSE-P STREAM)
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (OR (SETQ HEADERS-END-BP (GET STATUS 'HEADERS-END-BP))
      (BARF "Cannot find end of headers"))
  (COND ((GET STATUS 'ITS-HEADER-P)
         ;;Need to reformat
         (AND (SETQ LOSE-P (GET STATUS 'LOSING-HEADERS))
              (BARF "Cannot parse headers: ~A" LOSE-P))
         (DELETE-INTERVAL (MSG-START-BP MSG) HEADERS-END-BP)
         (SETQ STREAM (INTERVAL-STREAM-INTO-BP HEADERS-END-BP))
         (OUTPUT-HEADER STREAM STATUS '(:DATE :FROM :SENDER :SUBJECT :TO :CC))
         (TERPRI STREAM)
         (SETQ HEADERS-END-BP (SEND STREAM :READ-BP))))
  ;; Move back over blank lines
  (DO ((LINE (BP-LINE HEADERS-END-BP) PREV)
       (PREV)
       (BEG-LINE (BP-LINE (MSG-START-BP MSG))))
      ((OR (EQ LINE BEG-LINE)
           (NOT (LINE-BLANK-P (SETQ PREV (LINE-PREVIOUS LINE)))))
       (SETQ HEADERS-END-BP (CREATE-BP LINE 0))))
  ;; Delete any instances of the old header
  (DO ((LINE (BP-LINE (MSG-START-BP MSG)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE HEADERS-END-BP))
       (START-LINE NIL)
       (TEM))
      (NIL)
    (IF (SETQ TEM (AND (NEQ LINE END-LINE)
                       (LET ((PH (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'PARSED-HEADERS)))
                         (GET (LOCF PH) TYPE))))
        (OR START-LINE (SETQ START-LINE LINE))
        (COND (START-LINE
               (DELETE-INTERVAL (CREATE-BP START-LINE 0) (CREATE-BP LINE 0) T)
               (SETQ START-LINE NIL))))
    (AND (EQ LINE END-LINE) (RETURN)))
  (IF STREAM
      (SEND STREAM :SET-BP HEADERS-END-BP)
      (SETQ STREAM (INTERVAL-STREAM-INTO-BP HEADERS-END-BP)))
  (PRINT-HEADER STREAM PROP TYPE)
  (SET-PARSED-MSG-HEADERS MSG))

;;; Change a property
(DEFUN MSG-PUT (MSG PROP INDICATOR)
  (PUTPROP (ASSURE-MSG-PARSED MSG) PROP INDICATOR)
  (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG MSG)
  (SETF (MSG-TICK MSG) (TICK)))

;;; Background stuff
(DEFMETHOD (ZMAIL-FRAME :AFTER :SELECT) (&OPTIONAL IGNORE)
  (FUNCALL EDITOR-CLOSURE 'MAYBE-RESET-ZMAIL-BACKGROUND-PROCESS SELF))

;; Not patched in 94 due to SI:FLUSHED-PROCESS
(DEFUN MAYBE-RESET-ZMAIL-BACKGROUND-PROCESS (REASON)
  (AND (EQ (PROCESS-WAIT-FUNCTION *ZMAIL-BACKGROUND-PROCESS*) 'SI:FLUSHED-PROCESS)
       (SEND *ZMAIL-BACKGROUND-PROCESS* :RESET))
  (SEND *ZMAIL-BACKGROUND-PROCESS* :RUN-REASON REASON))

;;; Information from the background process.
;;; This function is called in the main process to process a (BACKGROUND ...) blip
;;; put on by the background process.

(DEFSELECT ZMAIL-BACKGROUND-REQUEST
  (NEW-MAIL (&REST ARGS)
    (case *notify-on-new-mail-in-background*
      (:notify (apply #'tv:notify nil args))
      ((:converse T)
       (qsend (format nil "~A@~A" *zmail-user* si:local-host)
              (apply #'format nil args)
              nil nil))
      (nil))
    (APPLY #'TYPEIN-LINE ARGS))
  (FILE-LOADED (ZMAIL-BUFFER)
    (SEND ZMAIL-BUFFER :LOADING-DONE))
  (MSGS-LOADED (ZMAIL-BUFFER START END)
    ZMAIL-BUFFER START END
    (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY T))
  (FILE-SAVE-ABORTED (ZMAIL-BUFFER)
    (SEND ZMAIL-BUFFER :SAVING-ABORTED))
  (FILE-SAVED (ZMAIL-BUFFER)
    (SEND ZMAIL-BUFFER :SAVING-DONE)))

(DEFVAR *BACKGROUND-MOUSE-SPEED-THRESHOLD* 2.5s0)
(DEFVAR *MAIL-CHECK-PERIOD* 1800.)
(DEFVAR *LAST-MAIL-CHECK-TIME*)
;; This is the INBOX-BUFFER which is used for checking for new mail.
(DEFVAR *BACKGROUND-INBOX-BUFFER*)

(DEFVAR *BACKGROUND-PRELOAD-BUFFER*)

;;; This is the top level of the background process
(DEFUN ZMAIL-BACKGROUND (*ZMAIL-WINDOW*)
  (DO ((*ZMAIL-BACKGROUND-P* T)
       (TV:IO-BUFFER (SEND *ZMAIL-WINDOW* :IO-BUFFER))
       (*BACKGROUND-INBOX-BUFFER* NIL)
       (*LAST-MAIL-CHECK-TIME* (TIME))
       (*BACKGROUND-RESPONSE-QUEUE*
         (LOCF (ZMAIL-BACKGROUND-PROCESS-RESPONSE-QUEUE CURRENT-PROCESS)))
       (LOCK (LOCF (ZMAIL-BACKGROUND-PROCESS-LOCK CURRENT-PROCESS)))
       (COMMAND-BUFFER-POINTER (LOCF (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS)))
       (PRELOAD-QUEUE-POINTER (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE CURRENT-PROCESS)))
       *BACKGROUND-PRELOAD-BUFFER*)
      (NIL)
    (DECLARE (SPECIAL TV:IO-BUFFER))
    (DO () ((< TV:MOUSE-SPEED *BACKGROUND-MOUSE-SPEED-THRESHOLD*))
      (PROCESS-SLEEP 600.))                     ;Try not to interfere with mousing commands
    ;; Only run when main window exposed or preload requested
    (AND *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED*
         (CONTENTS PRELOAD-QUEUE-POINTER)       ;Have preload requests.
         (NULL *BACKGROUND-PRELOAD-BUFFER*)                     ;In the middle of preloading
         (ZMAIL-BACKGROUND-WAIT-FOR-EXPOSURE))
    (UNWIND-PROTECT
      (LET ((CURRENT-REQUEST (CAAR COMMAND-BUFFER-POINTER)))
        (PROCESS-LOCK LOCK)
        (ZMAIL-BACKGROUND-PERFORM CURRENT-REQUEST)      ;Perform one step
        (OR *INHIBIT-BACKGROUND-MAIL-CHECKS*
            (ZMAIL-BACKGROUND-CHECK-FOR-NEW-MAIL)))
      (IGNORE-ERRORS (PROCESS-UNLOCK LOCK)))
    ;; Wait 1 second if there are still pending requests, or a long time if none.
    ;; Stop waiting if any new requests appear.
    (LET ((CURRENT-STATE (CDR COMMAND-BUFFER-POINTER))
          (PRELOAD-STATE (CDR PRELOAD-QUEUE-POINTER)))
      (PROCESS-WAIT "Zmail Background"
                    #'ZMAIL-BACKGROUND-PAUSE
                    (TIME) (IF (OR CURRENT-STATE *BACKGROUND-PRELOAD-BUFFER*
                                   (CDR PRELOAD-QUEUE-POINTER))
                               60. *MAIL-CHECK-PERIOD*)
                    CURRENT-STATE COMMAND-BUFFER-POINTER
                    PRELOAD-STATE PRELOAD-QUEUE-POINTER))))

(DEFUN ZMAIL-BACKGROUND-PAUSE (START-TIME INTERVAL OLD-CONTENTS POINTER
                               PRELOAD-OLD PRELOAD-POINTER)
  (OR ( (TIME-DIFFERENCE (TIME) START-TIME) INTERVAL)
      (NEQ (CAR POINTER) OLD-CONTENTS)
      (NEQ (CDR PRELOAD-POINTER) PRELOAD-OLD)))


;; Wait for exposure or preload request.  Also wait as long as foreground is not waiting.
(DEFUN ZMAIL-BACKGROUND-WAIT-FOR-EXPOSURE ()
  (DO ((CELL (LOCF (TV:SHEET-EXPOSED-P *ZMAIL-WINDOW*)))
       (PRELOAD-QUEUE-POINTER
         (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE CURRENT-PROCESS))))
      ((OR (CAR CELL) (CDR PRELOAD-QUEUE-POINTER)))
    (PROCESS-WAIT "Expose" #'(LAMBDA (CELL PRELOAD-QUEUE-POINTER FOREGROUND-PROCESS)
                               (AND (NEQ (PROCESS-WAIT-FUNCTION FOREGROUND-PROCESS) #'TRUE)
                                    (OR (CAR CELL)
                                        (CDR PRELOAD-QUEUE-POINTER))))
                  CELL PRELOAD-QUEUE-POINTER
                  (SEND *ZMAIL-WINDOW* :PROCESS))))

;Do some or all of the pending request or the pending preload file.
;Advance the queues if appropriate.
(DEFUN ZMAIL-BACKGROUND-PERFORM (CURRENT-REQUEST)
  (LET ((COMMAND-BUFFER-POINTER
          (LOCF (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS)))
        (*BATCH-UNDO-SAVE* T)
        (PRELOAD-QUEUE-POINTER
          (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE CURRENT-PROCESS)))
        (PRELOAD-FILENAME))
    (COND (CURRENT-REQUEST
           (AND (SEND (CAR CURRENT-REQUEST) CURRENT-REQUEST)
                (WITHOUT-INTERRUPTS
                  (SETF (CAR COMMAND-BUFFER-POINTER)
                        (DELQ CURRENT-REQUEST (CAR COMMAND-BUFFER-POINTER))))))
          (*BACKGROUND-PRELOAD-BUFFER*
           (LET ((THROWING T)
                 (FILENAME (BUFFER-PATHNAME *BACKGROUND-PRELOAD-BUFFER*)))
             (CONDITION-CASE ()
                 (UNWIND-PROTECT
                     (PROGN
                       (IF (ZMAIL-DISK-BUFFER-STREAM *BACKGROUND-PRELOAD-BUFFER*)
                           (WHEN (NOT (SEND *BACKGROUND-PRELOAD-BUFFER* :READ-NEXT-MSG 5))
                             (CLOSE (ZMAIL-DISK-BUFFER-STREAM *BACKGROUND-PRELOAD-BUFFER*))
                             (SETF (ZMAIL-DISK-BUFFER-STREAM *BACKGROUND-PRELOAD-BUFFER*) NIL)
                             (SETQ *BACKGROUND-PRELOAD-BUFFER* NIL))
                         (SETQ *BACKGROUND-PRELOAD-BUFFER* NIL))
                       (SETQ THROWING NIL))
                   (WHEN THROWING
                     (CLOSE (ZMAIL-DISK-BUFFER-STREAM *BACKGROUND-PRELOAD-BUFFER*))
                     (SETF (ZMAIL-DISK-BUFFER-STREAM *BACKGROUND-PRELOAD-BUFFER*) NIL)
                     (UNLESS (ZMAIL-BUFFER-SAVED-CURRENT-MSG *BACKGROUND-PRELOAD-BUFFER*)
                       ;; Don't kill if has been selected in ZMail ever.
                       (SEND *ZMAIL-WINDOW* :KILL-ZMAIL-BUFFER *BACKGROUND-PRELOAD-BUFFER*))
                     (SETQ *BACKGROUND-PRELOAD-BUFFER* NIL)))
               ((SYS:REMOTE-NETWORK-ERROR
                 SYS:STREAM-INVALID)
                (TV:NOTIFY NIL "Chaosnet trouble in ZMail; ~A not preloaded."
                           FILENAME)))))
          ((SETQ PRELOAD-FILENAME
                 (ZMAIL-BACKGROUND-PRELOAD-POP PRELOAD-QUEUE-POINTER))
           (IF (CONSP PRELOAD-FILENAME)
               (APPLY #'ZMAIL-PRELOAD-PERFORM PRELOAD-FILENAME)
             (*CATCH 'PRELOAD-ERROR
               (SETQ *BACKGROUND-PRELOAD-BUFFER*
                     (BACKGROUND-OPEN-ZMAIL-FILE PRELOAD-FILENAME))))))))

(DEFSELECT ZMAIL-PRELOAD-PERFORM
  (:LOAD-FILE (FILENAME)
    (*CATCH 'PRELOAD-ERROR
      (SETQ *BACKGROUND-PRELOAD-BUFFER*
            (BACKGROUND-OPEN-ZMAIL-FILE FILENAME))))
  (:HANG-WHEN-DEEXPOSED (&OPTIONAL (FLAG T))
    (SETQ *HANG-BACKGROUND-PROCESS-WHEN-DEEXPOSED* FLAG))
  (:SET-ZMAIL-USER ()
    (SEND *ZMAIL-WINDOW* :SET-ZMAIL-USER T)))   ;Be sure we're logged in


(DEFUN ZMAIL-BACKGROUND-CHECK-FOR-NEW-MAIL ()
  (COND (( (TIME-DIFFERENCE (TIME) *LAST-MAIL-CHECK-TIME*)
            *MAIL-CHECK-PERIOD*)
         (AND *BACKGROUND-INBOX-BUFFER*
              (SEND *BACKGROUND-INBOX-BUFFER* :BACKGROUND-CHECK-FOR-NEW-MAIL))
         (SETQ *LAST-MAIL-CHECK-TIME* (TIME)))))

;(ZMAIL-BACKGROUND-LOAD-FILE zmail-buffer) is a request on the queue.
;Load some more of a mail file, then tell main process some more has been loaded.
;Returns T if loading of file is finished, or if there is an error,
;so that the request is removed from the queue.
(DEFUN ZMAIL-BACKGROUND-LOAD-FILE (REQUEST &AUX (ZMAIL-BUFFER (SECOND REQUEST))
                                                (START (ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER)))
  (UNWIND-PROTECT
    (CONDITION-CASE ()
        (NOT (SEND ZMAIL-BUFFER :READ-NEXT-MSG 5))
      ((SYS:REMOTE-NETWORK-ERROR
        SYS:STREAM-INVALID)
       (TV:NOTIFY NIL "Chaosnet trouble, file ~A closed."
                  (BUFFER-PATHNAME ZMAIL-BUFFER))
       T))
    (ZMAIL-BACKGROUND-RESPONSE-PUSH
      `(MSGS-LOADED ,ZMAIL-BUFFER ,START ,(ZMAIL-BUFFER-NMSGS ZMAIL-BUFFER)))))

;(ZMAIL-BACKGROUND-SAVE-FILE zmail-buffer) is a request on the queue.
(DEFUN ZMAIL-BACKGROUND-SAVE-FILE (REQUEST &AUX (ZMAIL-BUFFER (SECOND REQUEST)) SUCCESS)
  (LOCK-ZMAIL-BUFFER (ZMAIL-BUFFER)
    (AND ;; Prevent timing problems
         (MEMQ REQUEST (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS))
         (UNWIND-PROTECT
           (CONDITION-CASE ()
               (PROG1
                ;; Write up to 50 lines.  Return T if saving finished.
                (DO ((I 0 (1+ I))
                     (INTERVAL-STREAM (THIRD REQUEST))
                     (FILE-STREAM (ZMAIL-DISK-BUFFER-STREAM ZMAIL-BUFFER))
                     (LINE)
                     (EOF))
                    (( I 50.) NIL)
                  (MULTIPLE-VALUE (LINE EOF)
                    (SEND INTERVAL-STREAM :LINE-IN))
                  (IF (NOT EOF)
                      (SEND FILE-STREAM :LINE-OUT LINE)
                    (SEND FILE-STREAM :STRING-OUT LINE)
                    (SEND ZMAIL-BUFFER :SAVING-DONE)
                    (RETURN T)))
                ;; No abort; prevent execution of unwind forms.
                (SETQ SUCCESS T))
             ((SYS:REMOTE-NETWORK-ERROR
               SYS:STREAM-INVALID)
              (TV:NOTIFY NIL "Chaosnet trouble, ~A not saved."
                         (BUFFER-PATHNAME ZMAIL-BUFFER))))
           (IF SUCCESS NIL
             ;; If we got an error, flush this request to avoid repeated lossage.
             (SEND ZMAIL-BUFFER :SAVING-ABORTED)
             (SETF (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS)
                   (DELQ REQUEST (ZMAIL-BACKGROUND-PROCESS-REQUEST-CELL CURRENT-PROCESS))))))))

(DEFUN ZMAIL-BACKGROUND-SET-INBOX-BUFFER (REQUEST)
  (SETQ *BACKGROUND-INBOX-BUFFER* (SECOND REQUEST))
  (SETQ *LAST-MAIL-CHECK-TIME* (TIME-DIFFERENCE (TIME) (1+ *MAIL-CHECK-PERIOD*)))
  T)

(DEFUN ZMAIL-BACKGROUND-PARSE-MSGS (REQUEST &AUX (ZMAIL-BUFFER (SECOND REQUEST))
                                                 (START (THIRD REQUEST))
                                                 (ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))
  (LOCK-ZMAIL-BUFFER (ZMAIL-BUFFER)
    (DO ((INDEX START (1+ INDEX))
         (MAX (ARRAY-ACTIVE-LENGTH ARRAY))
         (COUNT 0)
         MSG)
        ((OR ( INDEX MAX) ( COUNT 5))
         (COND (( INDEX MAX)
                T)
               (T
                (SETF (THIRD REQUEST) INDEX)
                NIL)))
      (SETQ MSG (AREF ARRAY INDEX))
      (COND ((NULL (MSG-PARSED-P MSG))
             (ASSURE-MSG-PARSED MSG)
             (INCF COUNT))))))

;; Add things to the request queue as a background request.
;; This can be used to request things be preloaded, but not until
;; the ZMAIL window is selected.

(DEFUN ZMAIL-BACKGROUND-PRELOAD-FILES (REQUEST)
  (LET ((QUEUE (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE CURRENT-PROCESS))))
    (WITHOUT-INTERRUPTS
      (SETF (CDR QUEUE)
            (APPEND (CDR QUEUE)
                    (CDR REQUEST)
                    NIL)))))                    ;Copy the request list

(DEFUN LOCK-BACKGROUND-PROCESS ()
  (COND ((NEQ CURRENT-PROCESS (CAR *ZMAIL-BACKGROUND-PROCESS-LOCK*))
         (PROCESS-LOCK *ZMAIL-BACKGROUND-PROCESS-LOCK*)
         ;; There may be some junk from it in the response queue already
         (DO () ((NULL (CONTENTS *BACKGROUND-RESPONSE-QUEUE*)))
           (LET ((RESPONSE (POP (CONTENTS *BACKGROUND-RESPONSE-QUEUE*))))
             (APPLY #'ZMAIL-BACKGROUND-REQUEST (CDR RESPONSE))))
         T)))


;;; Setup for loading a mail file from stream, does not actually read any messages.
;;; Notifies if error, throwing to PRELOAD-ERROR, otherwise returning a newly consed
;;; mail file, or a mail file already associated with this file.
(DEFUN BACKGROUND-OPEN-ZMAIL-FILE (PATHNAME)
  (SETQ PATHNAME
        (FS:MERGE-PATHNAME-DEFAULTS PATHNAME *ZMAIL-PATHNAME-DEFAULTS*))
  (LET ((STREAM (OPEN PATHNAME '(:IN :NOERROR)))
        INFO ZMAIL-BUFFER)
    (COND ((ERRORP STREAM)
           (TV:CAREFUL-NOTIFY NIL T "Could not pre-load ZMAIL file ~A:  ~A"
                              (SEND PATHNAME :STRING-FOR-PRINTING) STREAM)
           (*THROW 'PRELOAD-ERROR ())))
    (SETQ INFO (SEND STREAM :INFO))
    (SETQ ZMAIL-BUFFER (SEND *ZMAIL-WINDOW* :GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME))
    (COND (ZMAIL-BUFFER
           (CLOSE STREAM)
           NIL)
          (T (MULTIPLE-VALUE-BIND (FLAVOR APPEND-P)
                 (SEND PATHNAME :MAIL-FILE-FORMAT-COMPUTER STREAM)
               (SEND *ZMAIL-WINDOW* :MAKE-ZMAIL-BUFFER FLAVOR
                        :PATHNAME PATHNAME :STREAM STREAM :FILE-ID INFO
                        :APPEND-P APPEND-P))))))

(DEFUN ZMAIL-BACKGROUND-PRELOAD-POP (POINTER)
  (PROG1 (CADR POINTER)
         (SETF (CDR POINTER)
               (CDDR POINTER))))

(DEFMETHOD (ZMAIL-FRAME :BACKGROUND-PRELOAD) (FILES)
  (LET ((*ZMAIL-BACKGROUND-PROCESS*
          (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*ZMAIL-BACKGROUND-PROCESS*)))
    (LET ((POINTER (LOCF (ZMAIL-BACKGROUND-PROCESS-PRELOAD-QUEUE *ZMAIL-BACKGROUND-PROCESS*))))
      (WITHOUT-INTERRUPTS
        (SETF (CDR POINTER)
              (APPEND (CDR POINTER) FILES NIL))))))

(DEFMETHOD (ZMAIL-FRAME :AFTER :BACKGROUND-PRELOAD) (&OPTIONAL IGNORE)
  (LET ((*ZMAIL-BACKGROUND-PROCESS*
          (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*ZMAIL-BACKGROUND-PROCESS*)))
    (MAYBE-RESET-ZMAIL-BACKGROUND-PROCESS CURRENT-PROCESS)))

(DEFMETHOD (ZMAIL-FRAME :SET-ZMAIL-USER) (&OPTIONAL SILENT)
  (FUNCALL EDITOR-CLOSURE 'SET-ZMAIL-USER SILENT))

(DEFMETHOD (ZMAIL-FRAME :GET-ZMAIL-BUFFER-FROM-PATHNAME) (PATHNAME &OPTIONAL CREATE-P)
  (FUNCALL EDITOR-CLOSURE 'GET-ZMAIL-BUFFER-FROM-PATHNAME PATHNAME CREATE-P))

(DEFMETHOD (ZMAIL-FRAME :MAKE-ZMAIL-BUFFER) (TYPE &REST OPTIONS)
  (LEXPR-FUNCALL EDITOR-CLOSURE #'MAKE-ZMAIL-BUFFER TYPE OPTIONS))

(DEFMETHOD (ZMAIL-FRAME :KILL-ZMAIL-BUFFER) (BUFFER)
  (FUNCALL EDITOR-CLOSURE BUFFER :KILL))

(DEFUN PRELOAD-ZMAIL (&REST FILES)
  (SEND *ZMAIL-WINDOW* :BACKGROUND-PRELOAD `((:SET-ZMAIL-USER) . ,FILES)))

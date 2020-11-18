;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; These are the frames used by filtering and their commands
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-SELECT (STRING)
  (FORMAT STRING "Create//Select buffer: ~@[L: /"~A/"; ~]M: filter; R: menu."
          (DOLIST (MF *ZMAIL-BUFFER-LIST*)
            (OR (EQ MF *ZMAIL-BUFFER*)
                (RETURN (ZMAIL-BUFFER-NAME-for-buffer-alist MF))))))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SELECT "Select another buffer.
Left selects most recently selected other buffer.
Middle creates a new subset buffer by filtering.
Right gives a menu listing all existing buffers,
 and creation techniques such as reading in a mail file
 and marking the summary window." (NO-ZMAIL-BUFFER-OK)
  (SET-ZMAIL-USER)
  (SELECT-ZMAIL-BUFFER (CASE *ZMAIL-COMMAND-BUTTON*
                         (:RIGHT (MENU-GET-ZMAIL-BUFFER-FOR-SELECTION))
                         (:MIDDLE (READ-SUBSET-ZMAIL-BUFFER))
                         (OTHERWISE (OR (DOLIST (MF *ZMAIL-BUFFER-LIST*)
                                          (OR (EQ MF *ZMAIL-BUFFER*)
                                              (RETURN MF)))
                                        (BARF "This is the only buffer"))))))

(DEFUN MENU-GET-ZMAIL-BUFFER-FOR-SELECTION (&AUX ITEM-LIST)
  "Return a ZMAIL-BUFFER chose by the user.  We offer the menu."
  (MULTIPLE-VALUE-BIND (ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)
      (GET-ZMAIL-BUFFER-ALISTS T)
     (IF (OR ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)
         (SEND *SELECT-ZMAIL-BUFFER-MENU* :SET-GEOMETRY 2 NIL)
       (SEND *SELECT-ZMAIL-BUFFER-MENU* :SET-GEOMETRY NIL 1))
    (SETQ ITEM-LIST (TV:APPEND-ITEM-LISTS ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)))
  (SETQ ITEM-LIST (APPEND ITEM-LIST
                          (AND (ODDP (LENGTH ITEM-LIST)) '(("" :NO-SELECT T)))
                          '(("Read or create file" :VALUE :READ-FILE :FONT FONTS:HL12I
                             :DOCUMENTATION
                             "Read in and select a mail file, creating it if necessary.")
                            ("Mark summary" :VALUE :MARKING :FONT FONTS:HL12I
                             :DOCUMENTATION
                       "Select a temporary buffer made by clicking on the summary window.")
                            ("Abort" :VALUE :ABORT :FONT FONTS:HL12I
                                     :DOCUMENTATION "Abort this command.")
                            ("Subset" :VALUE :SUBSET :FONT FONTS:HL12I
                             :DOCUMENTATION "Select a subset buffer made by filtering.")
                            )))
  (OR (EQUAL ITEM-LIST (SEND *SELECT-ZMAIL-BUFFER-MENU* :ITEM-LIST))
      (SEND *SELECT-ZMAIL-BUFFER-MENU* :SET-ITEM-LIST ITEM-LIST))
  (UNWIND-PROTECT
    (PROGN
      (TV:EXPOSE-WINDOW-NEAR *SELECT-ZMAIL-BUFFER-MENU* (RECTANGLE-NEAR-COMMAND-MENU))
      (DO ((ZMAIL-BUFFER)) (NIL)
        (SETQ ZMAIL-BUFFER (SEND *SELECT-ZMAIL-BUFFER-MENU* :CHOOSE))
        (SET-COMMAND-BUTTON (SEND *SELECT-ZMAIL-BUFFER-MENU* :LAST-BUTTONS))
        (CASE ZMAIL-BUFFER
          (:ABORT
           (ABORT-CURRENT-COMMAND))
          (:SUBSET
           (SEND *SELECT-ZMAIL-BUFFER-MENU* :DEACTIVATE)
           (SETQ ZMAIL-BUFFER (READ-SUBSET-ZMAIL-BUFFER)))
          (:MARKING
           (SEND *SELECT-ZMAIL-BUFFER-MENU* :DEACTIVATE)
           (SETQ ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER-BY-MARKING)))
          (:READ-FILE
           (SETQ ZMAIL-BUFFER (READ-ZMAIL-BUFFER-FILENAME *SELECT-ZMAIL-BUFFER-MENU*))))
        (COND ((OR (STRINGP ZMAIL-BUFFER) (TYPEP ZMAIL-BUFFER 'FS:PATHNAME))
               (SEND *SELECT-ZMAIL-BUFFER-MENU* :DEACTIVATE)
               (SETQ ZMAIL-BUFFER (ZMAIL-FIND-FILE-NOSELECT ZMAIL-BUFFER))))
        (AND ZMAIL-BUFFER (RETURN ZMAIL-BUFFER))))
    (SEND *SELECT-ZMAIL-BUFFER-MENU* :DEACTIVATE)))

(DEFUN READ-ZMAIL-BUFFER-FILENAME (NEAR-WINDOW &AUX ZMAIL-BUFFER)
  (LET ((PN (IF *ZMAIL-BUFFER*
                (BUFFER-PATHNAME (IF (ZMAIL-BUFFER-DISK-P *ZMAIL-BUFFER*)
                                     *ZMAIL-BUFFER*
                                   *PRIMARY-ZMAIL-BUFFER*))
              (DEFAULT-ZMAIL-MOVE-PATHNAME))))
    (*CATCH 'ZWEI-COMMAND-LOOP                  ;In case of G
      (SETQ ZMAIL-BUFFER (CALL-POP-UP-MINI-BUFFER-EDITOR
                           NEAR-WINDOW
                           'READ-DEFAULTED-PATHNAME "Find file"
                           PN (SEND PN :TYPE) NIL :NEW-OK))))
  ZMAIL-BUFFER)

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* FILE "Select"
                          SELECT-ZMAIL-BUFFER-FROM-PATHNAME T "Select this file.")

(DEFUN SELECT-ZMAIL-BUFFER-FROM-PATHNAME (PATHNAME)
  (SELECT-ZMAIL-BUFFER (ZMAIL-FIND-FILE-NOSELECT PATHNAME)))

(DEFUN READ-SUBSET-ZMAIL-BUFFER ()
  "Ask the user to specify a universe and filter; create and return a subset buffer."
  (MULTIPLE-VALUE-BIND (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
      (GET-FILTER-FUNCTION)
    (MAKE-ZMAIL-BUFFER-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))

(DEFUN MAKE-ZMAIL-BUFFER-FROM-FILTER (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
                                      &OPTIONAL ZMAIL-BUFFER)
  "Create and return a subset buffer using specified mapping and filtering."
  (OR ZMAIL-BUFFER
      (MULTIPLE-VALUE-BIND (NAME FULL-NAME)
          (GENERATE-SUBSET-BUFFER-NAME MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
        (SETQ ZMAIL-BUFFER (GET-RECYCLED-TEMP-ZMAIL-BUFFER NAME FULL-NAME))))
  (LET ((ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER))
        (*N* 0))
    (DECLARE (SPECIAL *N*))
    (FUNCALL MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
             #'(LAMBDA (MSG ARRAY)
                 (AND ( *N* (ARRAY-LENGTH ARRAY))
                      (ADJUST-ARRAY-SIZE ARRAY (TRUNCATE (* *N* 5) 4)))
                 (ASET MSG ARRAY *N*)
                 (SETQ *N* (1+ *N*)))
             ARRAY)
    (SETF (ARRAY-LEADER ARRAY 0) *N*))
  ZMAIL-BUFFER)

(DEFUN (COM-ZMAIL-SELECT ASSOCIATED-MAP-COMMAND) (MAP-FUNCTION MAP-ARG
                                                  FILTER-FUNCTION FILTER-ARG)
  (SELECT-ZMAIL-BUFFER (IF (AND (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
                                (EQ FILTER-FUNCTION 'MSG-TRUE-FILTER))
                           MAP-ARG
                         (MAKE-ZMAIL-BUFFER-FROM-FILTER MAP-FUNCTION MAP-ARG
                                                        FILTER-FUNCTION FILTER-ARG))))

(DEFCONST *MAX-NAME-LENGTH* 50.)
(DEFUN GENERATE-SUBSET-BUFFER-NAME (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
                                    &AUX FULL-NAME NAME ADDED-NAME)
  (SETQ FULL-NAME (FUNCALL (GET MAP-FUNCTION 'MAP-FUNCTION-BUFFER-NAME-FUNCTION) MAP-ARG)
        NAME FULL-NAME
        ADDED-NAME (FILTER-FUNCTION-BUFFER-NAME FILTER-FUNCTION FILTER-ARG))
  (DO ((I 0)
       (LEN (STRING-LENGTH NAME))
       (MAXL (MAX (- *MAX-NAME-LENGTH* (STRING-LENGTH ADDED-NAME) 4) 0)))
      (( (- LEN I) MAXL)
       (OR (ZEROP I)
           (SETQ NAME (STRING-APPEND "<...>" (SUBSTRING NAME I)))))
    (IF (SETQ I (STRING-SEARCH-SET '(#/> #/) #/} #/] #/) NAME I))
        (SETQ I (1+ I))
      (SETQ I LEN)))
  (LET ((SAME (EQ FULL-NAME NAME)))
    (SETQ NAME (STRING-APPEND NAME ADDED-NAME)
          FULL-NAME (IF SAME NAME (STRING-APPEND FULL-NAME ADDED-NAME))))
  (VALUES NAME FULL-NAME))

(DEFUN FILTER-FUNCTION-BUFFER-NAME (FILTER-FUNCTION FILTER-ARG &AUX TEM)
  (COND ((SETQ TEM (GET FILTER-FUNCTION 'FILTER-FUNCTION-BUFFER-NAME-FUNCTION))
         (FUNCALL TEM FILTER-ARG))
        ((SETQ TEM (GET FILTER-FUNCTION 'FILTER-FUNCTION-OPPOSITE-FUNCTION))
         (STRING-APPEND #/~ (FILTER-FUNCTION-BUFFER-NAME TEM FILTER-ARG)))
        (T
         (STRING-APPEND #/< FILTER-FUNCTION #/>))))

(DEFUN GET-RECYCLED-TEMP-ZMAIL-BUFFER (NAME &OPTIONAL (FULL-NAME NAME))
  ;; Make sure the name is unique
  (DO ((ORIGINAL-NAME NAME)
       (COUNT 1 (1+ COUNT)))
      ((NOT (GET-ZMAIL-BUFFER-FROM-NAME NAME)))
    (SETQ NAME (FORMAT NIL "~A-~D" ORIGINAL-NAME COUNT)))
  (MAKE-NEW-TEMP-ZMAIL-BUFFER NAME FULL-NAME))

(DEFUN MAKE-ZMAIL-BUFFER-BY-MARKING (&AUX OLD-CONFIG OLD-DOC)
  (OR *ZMAIL-BUFFER* (BARF "There is no current buffer"))
  (SETQ OLD-CONFIG *WINDOW-CONFIGURATION*
        OLD-DOC (SEND *SUMMARY-WINDOW* :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING))
  (UNWIND-PROTECT
    (LET ((*MODE-LINE-LIST* `("ZMail " "Marking " *ZMAIL-FILE-NAME*
                              ,(FORMAT NIL " ~:@C to finish; ~:@C to abort." #/END #/ABORT)
                              (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*))))
      (SEND *SUMMARY-WINDOW* :SET-WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
            "Click left to complement marked state of message.")
      (UNMARK-ALL-MESSAGES)
      (OR (SEND *SUMMARY-WINDOW* :EXPOSED-P)
          (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION :SUMMARY))
      (DO ((LIST NIL)
           (CH))
          (NIL)
        (REDISPLAY-MODE-LINE)
        (SEND *SUMMARY-WINDOW* :REDISPLAY-AS-NECESSARY)
        (SETQ CH (SEND *STANDARD-INPUT* :ANY-TYI))
        (COND ((AND (CONSP CH) (EQ (CAR CH) 'SUMMARY-MOUSE))
               (LET* ((MSG (CADADR CH))
                      (STATUS (ASSURE-MSG-PARSED MSG)))
                 (IF (PUTPROP STATUS (NOT (GET STATUS 'MARKED)) 'MARKED)
                     (PUSH MSG LIST)
                   (SETQ LIST (DELQ MSG LIST)))
                 (SEND *SUMMARY-WINDOW* :NEED-TO-REDISPLAY-MSG MSG)))
              ((OR (CONSP CH) (EQ CH #/END))
               (OR (EQ CH #/END) (SEND *STANDARD-INPUT* :UNTYI CH))
               (LET ((ZMAIL-BUFFER (GET-RECYCLED-TEMP-ZMAIL-BUFFER
                                     (STRING-APPEND (SINGLE-ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*)
                                                    "<Marked>"))))
                 (LET ((ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)))
                   (DOLIST (MSG (NREVERSE LIST))
                     (VECTOR-PUSH-EXTEND MSG ARRAY)))
                 (RETURN ZMAIL-BUFFER)))
              ((MEMQ CH '(#/ABORT #/C-]))
               (ABORT-CURRENT-COMMAND))
              (T
               (BEEP)))))
    (SEND *SUMMARY-WINDOW* :SET-WHO-LINE-OVERRIDE-DOCUMENTATION-STRING OLD-DOC)
    (UNMARK-ALL-MESSAGES)
    (SEND *SUMMARY-WINDOW* :NEED-FULL-REDISPLAY)
    (OR (EQ OLD-CONFIG *WINDOW-CONFIGURATION*)
        (SEND *ZMAIL-WINDOW* :SET-WINDOW-CONFIGURATION OLD-CONFIG))))

(DEFUN UNMARK-ALL-MESSAGES (&AUX ARRAY)
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (TEM))
      (( I NMSGS))
    ;; Avoid ASSURE-MSG-PARSED, since messages that haven't been cannot be marked.
    (AND (SETQ TEM (GETL (LOCF (MSG-STATUS (AREF ARRAY I))) '(MARKED)))
         (SETF (CADR TEM) NIL))))

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION COM-ZMAIL-SURVEY
  "Survey messages in typeout window:  L: all messages; M: last predicate; R: predicate menu.")

(DEFINE-ZMAIL-GLOBAL *LAST-SURVEY-FILTER-DATA* NIL)

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-SURVEY "Survey set of messages in typeout window.
Click right to give filter." (NO-MSG-OK)
  (LET ((MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
        (MAP-ARG *ZMAIL-BUFFER*)
        (FILTER-FUNCTION 'MSG-TRUE-FILTER)
        (FILTER-ARG NIL))
    (COND ((EQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
           (MULTIPLE-VALUE (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
             (GET-FILTER-FUNCTION (RECTANGLE-NEAR-COMMAND-MENU)))
           (SETQ *LAST-SURVEY-FILTER-DATA*
                 (LIST MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))
          ((EQ *ZMAIL-COMMAND-BUTTON* :MIDDLE)
           (UNLESS *LAST-SURVEY-FILTER-DATA*
             (BARF))
           (SETF (LIST MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
                 *LAST-SURVEY-FILTER-DATA*)))
    (SURVEY-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))

(DEFPROP COM-ZMAIL-SURVEY SURVEY-FROM-FILTER ASSOCIATED-MAP-COMMAND)

(DEFUN SURVEY-FROM-FILTER (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
                           &AUX *TERMINAL-IO*)
  (IF (TV:SHEET-EXPOSED-P *SUMMARY-WINDOW*)
      (SETQ *TERMINAL-IO* (SEND *SUMMARY-WINDOW* :TYPEOUT-WINDOW))
    (SETQ *TERMINAL-IO* (WINDOW-TYPEOUT-WINDOW *WINDOW*))
    (SEND *STANDARD-OUTPUT* :LINE-OUT *SUMMARY-WINDOW-LABEL*))
;  (TV:WINDOW-CALL (*TERMINAL-IO*)              ;For **MORE** blinking
     (LET ((*N* 0))
       (FUNCALL MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
                #'(LAMBDA (MSG STREAM &AUX STATUS)
                    (SETQ STATUS (ASSURE-MSG-PARSED MSG))
                    (SEND STREAM :TRUNCATED-ITEM 'SUMMARY-LINE MSG "~\ARROW\~3D~C~A"
                          (EQ MSG *MSG*) (SETQ *N* (1+ *N*)) (STATUS-LETTER STATUS)
                          (MSG-SUMMARY-LINE MSG))
                    (SEND STREAM :TYO #/CR))
                *STANDARD-OUTPUT*))
    (SEND *STANDARD-OUTPUT* :LINE-OUT "Done.")
  (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)
  DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* SUMMARY-LINE "Select"
                          SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER T
                          "Select this message.")

(DEFINE-ZMAIL-GLOBAL *LAST-GOTO-FILTER-FUNCTION* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-GOTO-FILTER-ARG* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER COM-ZMAIL-GOTO (STRING)
  (APPEND-TO-ARRAY STRING "Move to message from filter: ")
  (COND (*LAST-GOTO-FILTER-FUNCTION*
         (APPEND-TO-ARRAY STRING "L: ")
         (APPEND-TO-ARRAY STRING (FILTER-FUNCTION-BUFFER-NAME
                                   *LAST-GOTO-FILTER-FUNCTION*
                                   *LAST-GOTO-FILTER-ARG*))
         (APPEND-TO-ARRAY STRING "; ")))
  (APPEND-TO-ARRAY STRING "M: point pdl; R: specify filter."))

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-GOTO
  "Move to next message fitting a particular filter.
Left default to last filter used.  Middle gives a menu of recent messages.
Right to specify the filter." ()
  (IF (EQ *ZMAIL-COMMAND-BUTTON* :MIDDLE)
      (COM-ZMAIL-MOUSE-POINT-PDL)
    (LET ((MAP-FUNCTION 'MAP-OVER-REST-OF-ZMAIL-BUFFER)
          (MAP-ARG *ZMAIL-BUFFER*)
          (FILTER-FUNCTION *LAST-GOTO-FILTER-FUNCTION*)
          (FILTER-ARG *LAST-GOTO-FILTER-ARG*))
      (IF (NEQ *ZMAIL-COMMAND-BUTTON* :RIGHT)
          (OR FILTER-FUNCTION (BARF "There is no default for this command yet"))
        (MULTIPLE-VALUE (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
          (GET-FILTER-FUNCTION-1 MAP-FUNCTION MAP-ARG
                                 (FORMAT NIL "Rest of ~A" (ZMAIL-BUFFER-NAME MAP-ARG))
                                 (RECTANGLE-NEAR-COMMAND-MENU)))
        (SETQ *LAST-GOTO-FILTER-FUNCTION* FILTER-FUNCTION
              *LAST-GOTO-FILTER-ARG* FILTER-ARG)
        (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION 'COM-ZMAIL-GOTO))
      (LET ((MSG (FIND-MSG-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))
        (OR MSG (BARF "No more messages of this type"))
        (SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER MSG)))))

(DEFUN FIND-MSG-FROM-FILTER (MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)
  "Return the first message among those mapped over which fits the filter."
  (*CATCH 'FOUND
    (FUNCALL MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG
             #'(LAMBDA (MSG IGNORE) (*THROW 'FOUND MSG)) NIL)
    NIL))

(DEFUN (COM-ZMAIL-NEXT ASSOCIATED-MAP-COMMAND) (MAP-FUNCTION MAP-ARG
                                                FILTER-FUNCTION FILTER-ARG)
  (AND (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
       (SETQ MAP-FUNCTION 'MAP-OVER-REST-OF-ZMAIL-BUFFER))
  (LET ((MSG (FIND-MSG-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))
    (OR MSG (BARF "No more messages of this type"))
    (SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER MSG)))

(DEFUN (COM-ZMAIL-PREVIOUS ASSOCIATED-MAP-COMMAND) (MAP-FUNCTION MAP-ARG
                                                    FILTER-FUNCTION FILTER-ARG)
  (AND (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
       (SETQ MAP-FUNCTION 'MAP-OVER-BEGINNING-OF-ZMAIL-BUFFER))
  (LET ((MSG (FIND-MSG-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG)))
    (OR MSG (BARF "No more messages of this type"))
    (SELECT-MSG-AND-POSSIBLY-ZMAIL-BUFFER MSG)))

(DEFVAR *SYSTEM-FILTER-ALIST* '(("Deleted" :VALUE DELETED
                                           :DOCUMENTATION "Messages marked as deleted.")
                                ("Unseen" :VALUE UNSEEN
                                          :DOCUMENTATION "Messages never displayed before.")
                                ("Recent" :VALUE RECENT
                                          :DOCUMENTATION
                                          "Messages read in since last expunge.")
                                ("Answered" :VALUE ANSWERED
                                            :DOCUMENTATION
                                            "Messages to which replies have been sent.")
                                ("Filed" :VALUE FILED
                                         :DOCUMENTATION
                                         "Messages that have been moved into another file.")
                                ("Search" :VALUE :SEARCH
                                          :DOCUMENTATION
                                          "Messages containing a given string.")))

(DEFFLAVOR ZMAIL-COMMAND-MENU-PANE
        ()
        TV:(WHITESPACE-PANE-MIXIN COMMAND-MENU-MIXIN BASIC-MENU
            TOP-LABEL-MIXIN BORDERS-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST :COLUMNS 1 :BORDERS 2 :LABEL NIL
                       :FONT-MAP '(FONTS:HL12B FONTS:HL12BI)))

(DEFFLAVOR FILTER-SELECTION-FRAME () (TV:TEMPORARY-WINDOW-MIXIN TV:ANY-TYI-MIXIN
                                      TV:STREAM-MIXIN TV:BORDERS-MIXIN
                                      TV:ITEM-LIST-PANE-KLUDGE TV:FRAME-WITH-XOR-BUTTONS
                                      TV:CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
                                      TV:MINIMUM-WINDOW))

;;;; This page is entirely concerned with arranging the panes of the filter selection frame.

(DEFMETHOD (FILTER-SELECTION-FRAME :BEFORE :INIT) (IGNORE)
  (SETQ TV:PANES `((UNIVERSE-BUTTON TV:BIG-BUTTON-WITH-TOP-OUTSIDE-LABEL-PANE
                                    :LABEL "Universe:"
                                    :DOCUMENTATION
  "Give a menu of universes, buffers, and universe creation techniques.")
                   (NOT-BUTTON TV:BUTTON-PANE :NAME "Not"
                                              :DOCUMENTATION "Toggle negation of filter.")
                   (KEYWORD-MENU ZMAIL-COMMAND-MENU-PANE
                                 :ITEM-LIST NIL :LABEL "Keywords:")
                   (SYSTEM-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                     :ITEM-LIST ,`(("All" :VALUE :ALL
                                          :DOCUMENTATION "All messages in this universe.")
                                   ,@*SYSTEM-FILTER-ALIST*
                                   ("From//To" :VALUE :FROM-TO
                                               :DOCUMENTATION
 "Messages with a given From or To field, read from the keyboard or from message in summary.")
                                   ("Subject" :VALUE :SUBJECT
                                              :DOCUMENTATION
 "Messages with a given Subject field, read from the keyboard or from message in summary.")))
                   (USER-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                                     :ITEM-LIST NIL :LABEL "Filters:")
                   (ABORT-BUTTON TV:BUTTON-PANE :NAME "Abort"
                                                :DOCUMENTATION "Abort this command."))
        TV:CONSTRAINTS
              (LIST (FILTER-SELECTION-FRAME-MAKE-CONSTRAINT 'WITH T)
                    (FILTER-SELECTION-FRAME-MAKE-CONSTRAINT 'WITHOUT NIL))))

(DEFUN FILTER-SELECTION-FRAME-MAKE-CONSTRAINT (NAME UNIVERSE-P)
  `(,NAME . ((WHOLE-THING)
             ((WHOLE-THING :HORIZONTAL (:EVEN)
               (WHOLE)
               ((WHOLE TV:WHITE-INCLUDE-WHITESPACE      ;Vert
                 (0.95) (:EVEN)
                 (,@(AND UNIVERSE-P '(UNIVERSAL)) MENUS CONTROLS)
                 ,@(AND UNIVERSE-P
                        '(((UNIVERSAL TV:SINGLE-PANE-IN-WHITESPACE UNIVERSE-BUTTON))))
                 ((CONTROLS TV:SINGLE-PANE-IN-WHITESPACE ABORT-BUTTON))
                 ((MENUS TV:WHITE-INCLUDE-WHITESPACE    ;Horiz
                         (:ASK-WINDOW SELF :MENUS-SIZE) (:EVEN)
                         (KEYWORD-MENUX SYSTEM-FILTER-AND-BUTTON USER-FILTER-MENUX)
                         ((KEYWORD-MENUX TV:SINGLE-PANE-IN-WHITESPACE KEYWORD-MENU))
                         ((USER-FILTER-MENUX TV:SINGLE-PANE-IN-WHITESPACE USER-FILTER-MENU))
                         ((SYSTEM-FILTER-AND-BUTTON TV:PANES-IN-WHITESPACE
                           (:ASK-WINDOW SYSTEM-FILTER-MENU :PANE-SIZE)
                           (NOT-BUTTON SYSTEM-FILTER-MENU))))))))))))

(DEFMETHOD (FILTER-SELECTION-FRAME :MENUS-SIZE) (&REST ARGS)
  (MAX (LEXPR-SEND SELF :SEND-PANE 'KEYWORD-MENU :PANE-SIZE ARGS)
       (TRUNCATE (* (+ (LEXPR-SEND SELF :SEND-PANE 'NOT-BUTTON :PANE-SIZE ARGS)
                       (LEXPR-SEND SELF :SEND-PANE 'SYSTEM-FILTER-MENU :PANE-SIZE ARGS))
                    12.)
                 10.)
       (LEXPR-SEND SELF :SEND-PANE 'USER-FILTER-MENU :PANE-SIZE ARGS)))

(DEFMETHOD (FILTER-SELECTION-FRAME :COMPUTE-GEOMETRY) (UNIVERSE-NAME KEYWORD-ALIST
                                                       USER-FILTER-ALIST
                                                       &AUX MAX-WIDTH MAX-HEIGHT CHANGED-P
                                                            (CONFIG 'WITH))
  (SETQ MAX-WIDTH TV:(- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
        MAX-HEIGHT TV:(- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
  (IF UNIVERSE-NAME
      (SEND SELF :SET-PANES-NAME 'UNIVERSE-BUTTON UNIVERSE-NAME)
    (SETQ CONFIG 'WITHOUT))
  (SETQ CHANGED-P (NEQ CONFIG TV:CONFIGURATION))
  (SETQ CHANGED-P (OR (SEND SELF :SET-PANES-ITEM-LIST 'KEYWORD-MENU KEYWORD-ALIST)
                      CHANGED-P))
  (SETQ CHANGED-P (OR (SEND SELF :SET-PANES-ITEM-LIST 'USER-FILTER-MENU USER-FILTER-ALIST)
                      CHANGED-P))
  (AND CHANGED-P
       (LET ((WID (MIN MAX-WIDTH
                       (TRUNCATE
                         (* (MAX (SEND SELF :SEND-PANE 'UNIVERSE-BUTTON
                                            :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                            NIL NIL :HORIZONTAL)
                                 (+ (SEND SELF :SEND-PANE 'KEYWORD-MENU
                                               :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                               NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'SYSTEM-FILTER-MENU
                                               :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                               NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'USER-FILTER-MENU
                                               :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                               NIL NIL :HORIZONTAL))
                                 (SEND SELF :SEND-PANE 'ABORT-BUTTON
                                            :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                            NIL NIL :HORIZONTAL))
                            15.)
                         10.)))
             (HEI (MIN MAX-HEIGHT
                       (TRUNCATE
                         (* (+ (SEND SELF :SEND-PANE 'UNIVERSE-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :VERTICAL)
                               (SEND SELF :MENUS-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :VERTICAL)
                               (SEND SELF :SEND-PANE 'ABORT-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :VERTICAL))
                            12.)
                         10.))))
         (IF (AND (= WID (TV:SHEET-INSIDE-WIDTH))
                  (= HEI (TV:SHEET-INSIDE-HEIGHT)))
             (SEND SELF :SET-CONFIGURATION CONFIG)
           (OR (EQ CONFIG TV:CONFIGURATION)
               (SEND SELF :SET-CONFIGURATION CONFIG))
           (SEND SELF :SET-INSIDE-SIZE WID HEI)))))

(DEFUN GET-FILTER-FUNCTION (&OPTIONAL (NEAR-MODE '(:MOUSE)))
  "Ask user to choose a filter, using the filter selection frame.
The user also specifies a domain to map over -- possibly a defined universe.
Values are map function and arg, and filter function and arg."
  (DECLARE (RETURN-LIST MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG))
  (OR *ZMAIL-BUFFER* (BARF "There is no current buffer"))
  (GET-FILTER-FUNCTION-1 'MAP-OVER-SINGLE-ZMAIL-BUFFER *ZMAIL-BUFFER*
                         (ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*)
                         NEAR-MODE))

(DEFUN GET-FILTER-FUNCTION-1 (MAP-FUNCTION MAP-ARG NAME NEAR-MODE
                              &AUX FILTER-FUNCTION FILTER-ARG NOT-P)
  (DECLARE (RETURN-LIST MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG))
  (SEND *FILTER-SELECTION-FRAME* :COMPUTE-GEOMETRY NAME
        (APPEND '(("Any" :VALUE ANY
                   :FONT FONTS:HL12BI
                   :DOCUMENTATION "Messages with any keyword on them."))
                *KEYWORD-ALIST*
                NIL)                            ;Use a copy of the keyword-alist.
        (APPEND *USER-FILTER-ALIST*
                '(("New filter" :VALUE :NEW-FILTER
                   :FONT FONTS:HL12BI
                   :DOCUMENTATION "Define and use a new filter."))))
  (SEND *FILTER-SELECTION-FRAME* :TURN-OFF-ACCENTS)
  (UNWIND-PROTECT
    (PROGN
      (TV:EXPOSE-WINDOW-NEAR *FILTER-SELECTION-FRAME* NEAR-MODE)
      (DO ((CHAR)) (NIL)
        (SETQ CHAR (SEND *FILTER-SELECTION-FRAME* :ANY-TYI))
        (IF (ATOM CHAR)
            (TV:BEEP)
          (CASE (FIRST CHAR)
            (:MOUSE-BUTTON
             (IF (SEND (THIRD CHAR) :OPERATION-HANDLED-P :SET-ACCENT)
                 (LET* ((WINDOW (THIRD CHAR))
                        (WINDOW-NAME (SEND *FILTER-SELECTION-FRAME* :PANE-NAME WINDOW)))
                   (UNWIND-PROTECT
                       (CASE WINDOW-NAME
                         (ABORT-BUTTON (ABORT-CURRENT-COMMAND))
                         (NOT-BUTTON)
                         (UNIVERSE-BUTTON
                          (MULTIPLE-VALUE-BIND (NEW-MAP-FUNCTION NEW-MAP-ARG NEW-NAME)
                              (GET-UNIVERSE-FUNCTION `(:WINDOW ,*FILTER-SELECTION-FRAME*))
                            (AND NEW-MAP-FUNCTION
                                 (SETQ MAP-FUNCTION NEW-MAP-FUNCTION
                                       MAP-ARG NEW-MAP-ARG
                                       NAME NEW-NAME)))
                          (SEND *FILTER-SELECTION-FRAME* :SET-PANES-NAME
                                'UNIVERSE-BUTTON NAME)
                          (SEND *FILTER-SELECTION-FRAME* :EXPOSE)
                          )
                         (OTHERWISE (ZMAIL-ERROR "~S is not a known window" (THIRD CHAR))))
                     (SEND WINDOW :SET-ACCENT (AND (EQ WINDOW-NAME 'NOT-BUTTON)
                                                   (SETQ NOT-P (NOT NOT-P))))))))
              (:MENU
               (SETQ FILTER-ARG (SEND (FOURTH CHAR) :EXECUTE-NO-SIDE-EFFECTS
                                      (SECOND CHAR)))
               (CASE (SEND *FILTER-SELECTION-FRAME* :PANE-NAME (FOURTH CHAR))
                 (KEYWORD-MENU
                  (SETQ FILTER-FUNCTION (IF (EQ FILTER-ARG 'ANY)
                                            (IF NOT-P
                                                'MSG-DOES-NOT-HAVE-KEYWORDS-P
                                              'MSG-HAS-KEYWORDS-P)
                                          (IF NOT-P
                                              'MSG-DOES-NOT-HAVE-KEYWORD-P
                                            'MSG-HAS-KEYWORD-P))))
                 (SYSTEM-FILTER-MENU
                  (SETQ FILTER-FUNCTION (COND ((EQ FILTER-ARG :ALL)
                                               (IF NOT-P 'MSG-FALSE-FILTER 'MSG-TRUE-FILTER))
                                              ((EQ FILTER-ARG :SEARCH)
                                               (SEND *FILTER-SELECTION-FRAME* :DEACTIVATE)
                                               (MULTIPLE-VALUE-BIND (FUN KEY)
                                                   (ZMAIL-READ-FIND-SEARCH-STRING
                                                     "Messages containing string")
                                                 (SETQ FILTER-ARG KEY)
                                                 (CASE FUN
                                                   (SEARCH
                                                    (IF NOT-P
                                                        'MSG-DOES-NOT-HAVE-SEARCH-STRING
                                                      'MSG-HAS-SEARCH-STRING))
                                                   (FSM-SEARCH
                                                    (IF NOT-P
                                                        'MSG-DOES-NOT-HAVE-FSM-SEARCH-STRING
                                                      'MSG-HAS-FSM-SEARCH-STRING))
                                                   (FSM-EXPR-SEARCH
                                                    (IF NOT-P
                                                        'MSG-DOES-NOT-HAVE-FSM-EXPR-SEARCH-STRING
                                                      'MSG-HAS-FSM-EXPR-SEARCH-STRING)))))
                                              ((EQ FILTER-ARG :FROM-TO)
                                               (SEND *FILTER-SELECTION-FRAME* :DEACTIVATE)
                                               (LET (X)
                                                 (MULTIPLE-VALUE (X FILTER-ARG)
                                                   (CHOOSE-OR-READLINE-ADDRESS "From//To"
                                                                               NOT-P))
                                                 X))
                                              ((EQ FILTER-ARG :SUBJECT)
                                               (SEND *FILTER-SELECTION-FRAME* :DEACTIVATE)
                                               (LET ((X (CHOOSE-MSG-OR-READLINE "Subject")))
                                                 (OR (STRINGP X)
                                                     (SETQ X (GET-MSG-SUBJECT-CLEVERLY X)))
                                                 (SETQ FILTER-ARG X))
                                               (IF NOT-P
                                                   'MSG-DOES-NOT-HAVE-SUBJECT-STRING
                                                 'MSG-HAS-SUBJECT-STRING))
                                              (T
                                               (IF NOT-P
                                                   'MSG-DOES-NOT-HAVE-ATTRIBUTE-P
                                                 'MSG-HAS-ATTRIBUTE-P)))))
                 (USER-FILTER-MENU
                  (COND ((EQ FILTER-ARG :NEW-FILTER)
                         (SETQ NOT-P NIL)
                         (SEND *FILTER-SELECTION-FRAME* :DEACTIVATE)
                         (SETQ FILTER-ARG (DEFINE-NEW-FILTER))
                         (OR FILTER-ARG (ABORT-CURRENT-COMMAND))))
                  (SETQ FILTER-FUNCTION (IF NOT-P
                                            'MSG-DOES-NOT-FIT-FILTER-P
                                          'MSG-FITS-FILTER-P)))
                 (OTHERWISE (ZMAIL-ERROR "~S is not a known window" (THIRD CHAR))))
               (AND FILTER-FUNCTION (RETURN NIL)))))))
    (SEND *FILTER-SELECTION-FRAME* :DEACTIVATE))
  (VALUES MAP-FUNCTION MAP-ARG FILTER-FUNCTION FILTER-ARG))

;;; Map functions that implement that standard, simple domains to map over.
;;; A map function is always called with five arguments:
;;; an argument "for tha map function" which is computed
;;; at the same time as the map function is (eg, which buffer to map over),
;;; a filter function and an argument for it,
;;; and a processing function and an argument for it.
;;; MAP-OVER-SINGLE-MSG shows what these arguments are for.
;;; THe map function also has a MAP-FUNCTION-BUFFER-NAME-FUNCTION property.
;;; This function, given the argument "for the map function",
;;; returns a name to use for a subset buffer if that is what you are making.

(DEFUN (MAP-OVER-SINGLE-MSG MAP-FUNCTION-BUFFER-NAME-FUNCTION) (MSG)
  (STRING-APPEND #/$ (MSG-SUMMARY-LINE MSG) #/$))

(DEFUN MAP-OVER-SINGLE-MSG (MSG FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION PROCESSING-ARG)
  (AND (FUNCALL FILTER-FUNCTION MSG FILTER-ARG)
       (FUNCALL PROCESSING-FUNCTION MSG PROCESSING-ARG)))

(DEFPROP MAP-OVER-SINGLE-ZMAIL-BUFFER SINGLE-ZMAIL-BUFFER-NAME MAP-FUNCTION-BUFFER-NAME-FUNCTION)

(DEFUN SINGLE-ZMAIL-BUFFER-NAME (ZMAIL-BUFFER)
  (IF (ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
      (STRING-APPEND #/[ (ZMAIL-BUFFER-NAME ZMAIL-BUFFER) #/])
    (SEND ZMAIL-BUFFER :FULL-NAME)))

(DEFUN MAP-OVER-SINGLE-ZMAIL-BUFFER
       (ZMAIL-BUFFER FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION PROCESSING-ARG)
  (DOMSGS (MSG ZMAIL-BUFFER)
    (AND (FUNCALL FILTER-FUNCTION MSG FILTER-ARG)
         (FUNCALL PROCESSING-FUNCTION MSG PROCESSING-ARG))))

(DEFUN (MAP-OVER-REST-OF-ZMAIL-BUFFER MAP-FUNCTION-BUFFER-NAME-FUNCTION) (ZMAIL-BUFFER)
  (STRING-APPEND #/ (SINGLE-ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))

(DEFUN MAP-OVER-REST-OF-ZMAIL-BUFFER
       (ZMAIL-BUFFER FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION PROCESSING-ARG
        &AUX INDEX ARRAY)
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)
        INDEX (IF (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*) (1+ *MSG-NO*) 0))
  (DO ((INDEX INDEX (1+ INDEX))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (MSG))
      (NIL)
    (AND ( INDEX NMSGS)
         (OR (SEND ZMAIL-BUFFER :READ-NEXT-MSG)
             (RETURN NIL)))
    (SETQ MSG (AREF ARRAY INDEX))
    (AND (FUNCALL FILTER-FUNCTION MSG FILTER-ARG)
         (FUNCALL PROCESSING-FUNCTION MSG PROCESSING-ARG))))

(DEFUN (MAP-OVER-BEGINNING-OF-ZMAIL-BUFFER MAP-FUNCTION-BUFFER-NAME-FUNCTION)
       (ZMAIL-BUFFER)
  (STRING-APPEND #/ (SINGLE-ZMAIL-BUFFER-NAME ZMAIL-BUFFER)))

(DEFUN MAP-OVER-BEGINNING-OF-ZMAIL-BUFFER
       (ZMAIL-BUFFER FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION PROCESSING-ARG &AUX ARRAY)
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((N (1- (IF (EQ ZMAIL-BUFFER *ZMAIL-BUFFER*)
                  *MSG-NO*
                (ARRAY-ACTIVE-LENGTH ARRAY))) (1- N))
       (MSG))
      ((< N 0) NIL)
    (SETQ MSG (AREF ARRAY N))
    (AND (FUNCALL FILTER-FUNCTION MSG FILTER-ARG)
         (FUNCALL PROCESSING-FUNCTION MSG PROCESSING-ARG))))

(DEFUN (MAP-OVER-LOADED-ZMAIL-BUFFERS MAP-FUNCTION-BUFFER-NAME-FUNCTION) (IGNORE)
  "[*]")

(DEFUN MAP-OVER-LOADED-ZMAIL-BUFFERS (IGNORE FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION
                                      PROCESSING-ARG)
  (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
    (COND ((ZMAIL-BUFFER-DISK-P ZMAIL-BUFFER)
           (ASSURE-ZMAIL-BUFFER-FULLY-LOADED ZMAIL-BUFFER)
           (MAP-OVER-SINGLE-ZMAIL-BUFFER
             ZMAIL-BUFFER FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION
             PROCESSING-ARG)))))

(DEFUN (MAP-OVER-ALL-ZMAIL-BUFFERS MAP-FUNCTION-BUFFER-NAME-FUNCTION) (IGNORE)
  "[**]")

(DEFUN MAP-OVER-ALL-ZMAIL-BUFFERS (IGNORE FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION
                                   PROCESSING-ARG &AUX ZMAIL-BUFFER)
  ;; First all that are loaded
  (MAP-OVER-LOADED-ZMAIL-BUFFERS NIL FILTER-FUNCTION FILTER-ARG
                                 PROCESSING-FUNCTION PROCESSING-ARG)
  (DOLIST (ALIST-ELEM (GET-ZMAIL-BUFFER-ALISTS T))
    (SETQ ZMAIL-BUFFER (CDR ALIST-ELEM))
    (COND ((OR (STRINGP ZMAIL-BUFFER) (TYPEP ZMAIL-BUFFER 'FS:PATHNAME))
           (SETQ ZMAIL-BUFFER (ZMAIL-FIND-FILE-NOSELECT ZMAIL-BUFFER))
           (ASSURE-ZMAIL-BUFFER-FULLY-LOADED ZMAIL-BUFFER)
           (MAP-OVER-SINGLE-ZMAIL-BUFFER ZMAIL-BUFFER FILTER-FUNCTION FILTER-ARG
                                         PROCESSING-FUNCTION PROCESSING-ARG)))))

;;; Some built-in filter-functions.
;;; A filter function is called with two arguments:
;;; first a message, and then another arg whose meaning
;;; is specific to the particular filter function.
;;; This is the "filter function argument" that is passed to map functions, etc.

;;; Each filter function may have a FILTER-FUNCTION-BUFFER-NAME-FUNCTION property
;;; which is a function of one arg, the filter function argument,
;;; to return the second half of a name for a subset buffer.

;;; A filter defined by the user is not a filter-function.
;;; It serves as the filter-function-argument
;;; for the filter function MSG-FITS-FILTER-P.

(DEFUN (MSG-HAS-KEYWORDS-P FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (IGNORE)
  "{*}")

(DEFUN MSG-HAS-KEYWORDS-P (MSG IGNORE)
  (NOT (NULL (MSG-GET MSG 'KEYWORDS))))

(DEFPROP MSG-DOES-NOT-HAVE-KEYWORDS-P MSG-HAS-KEYWORDS-P FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-KEYWORDS-P (MSG IGNORE)
  (NULL (MSG-GET MSG 'KEYWORDS)))

(DEFUN (MSG-HAS-KEYWORD-P FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (KEYWORD)
  (STRING-APPEND #/{ (CAR (RASSQ KEYWORD *KEYWORD-ALIST*)) #/}))

(DEFUN MSG-HAS-KEYWORD-P (MSG KEYWORD)
  (MEMQ KEYWORD (MSG-GET MSG 'KEYWORDS)))

(DEFPROP MSG-DOES-NOT-HAVE-KEYWORD-P MSG-HAS-KEYWORD-P FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-KEYWORD-P (MSG KEYWORD)
  (NOT (MEMQ KEYWORD (MSG-GET MSG 'KEYWORDS))))

(DEFUN (MSG-TRUE-FILTER FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (IGNORE)
  "<*>")


(DEFUN MSG-TRUE-FILTER (IGNORE IGNORE)
  T)

(DEFPROP MSG-FALSE-FILTER MSG-TRUE-FILTER FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-FALSE-FILTER (IGNORE IGNORE)
  NIL)

(DEFUN (MSG-HAS-ATTRIBUTE-P FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (ATTRIBUTE)
  (STRING-APPEND #/< (SYSTEM-ATTRIBUTE-NAME ATTRIBUTE) #/>))

(DEFUN SYSTEM-ATTRIBUTE-NAME (ATTRIBUTE)
  (OR (DOLIST (X *SYSTEM-FILTER-ALIST*)
        (AND (EQ ATTRIBUTE (GET X :VALUE))
             (RETURN (CAR X))))
      (STRING ATTRIBUTE)))

(DEFUN MSG-HAS-ATTRIBUTE-P (MSG ATTRIBUTE)
  (NOT (NULL (MSG-GET MSG ATTRIBUTE))))

(DEFPROP MSG-DOES-NOT-HAVE-ATTRIBUTE-P MSG-HAS-ATTRIBUTE-P FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-ATTRIBUTE-P (MSG ATTRIBUTE)
  (NOT (MSG-GET MSG ATTRIBUTE)))

(DEFUN (MSG-FITS-FILTER-P FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (FILTER)
  (STRING-APPEND #/< FILTER #/>))

(DEFUN MSG-FITS-FILTER-P (MSG FILTER)
  (NOT (NULL (FUNCALL (OR (GET FILTER 'FILTER-FUNCTION)
                          (ZMAIL-ERROR "~S is not the name of a filter" FILTER))
                      MSG))))

(DEFPROP MSG-DOES-NOT-FIT-FILTER-P MSG-FITS-FILTER-P FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-FIT-FILTER-P (MSG FILTER)
  (NOT (FUNCALL (OR (GET FILTER 'FILTER-FUNCTION)
                    (ZMAIL-ERROR "~S is not the name of a filter" FILTER))
                MSG)))

(DEFUN (MSG-HAS-SEARCH-STRING FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (KEY)
  (STRING-APPEND "(Search: " KEY ")"))

(DEFUN MSG-HAS-SEARCH-STRING (MSG KEY)
  (NOT (NULL (ZWEI-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG)))))

(DEFPROP MSG-DOES-NOT-HAVE-SEARCH-STRING MSG-HAS-SEARCH-STRING
         FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-SEARCH-STRING (MSG KEY)
  (NULL (ZWEI-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG))))

(DEFUN MSG-HAS-FSM-SEARCH-STRING (MSG KEY)
  (NOT (NULL (FSM-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG)))))

(DEFUN MSG-DOES-NOT-HAVE-FSM-SEARCH-STRING (MSG KEY)
  (NULL (FSM-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG))))

(DEFUN MSG-HAS-FSM-EXPR-SEARCH-STRING (MSG KEY)
  (NOT (NULL (FSM-EXPR-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG)))))

(DEFUN MSG-DOES-NOT-HAVE-FSM-EXPR-SEARCH-STRING (MSG KEY)
  (NULL (FSM-EXPR-SEARCH (MSG-START-BP MSG) KEY NIL NIL NIL (MSG-END-BP MSG))))

(DEFUN (MSG-HAS-RECIPIENT-FIELD FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (FIELD)
  (STRING-APPEND "(Recipient: " FIELD #/)))

(DEFUN MSG-HAS-RECIPIENT-FIELD (MSG FIELD &AUX (STATUS (ASSURE-MSG-PARSED MSG)))
  (DOLIST (F *RECIPIENT-TYPE-HEADERS*)
    (AND (MSG-HEADER-RECIPIENT-MATCH (GET STATUS F) FIELD)
         (RETURN T))))

(DEFPROP MSG-DOES-NOT-HAVE-RECIPIENT-FIELD MSG-HAS-RECIPIENT-FIELD
         FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-RECIPIENT-FIELD (MSG FIELD &AUX (STATUS (ASSURE-MSG-PARSED MSG)))
  (NOT (DOLIST (F *RECIPIENT-TYPE-HEADERS*)
         (AND (MSG-HEADER-RECIPIENT-MATCH (GET STATUS F) FIELD)
              (RETURN T)))))

(DEFUN (MSG-HAS-FROM-FIELD FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (FIELD)
  (STRING-APPEND "(From: " FIELD #/)))

(DEFUN MSG-HAS-FROM-FIELD (MSG FIELD)
  (MSG-HEADER-RECIPIENT-MATCH (MSG-GET MSG :FROM) FIELD))

(DEFPROP MSG-DOES-NOT-HAVE-FROM-FIELD MSG-HAS-FROM-FIELD FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-FROM-FIELD (MSG FIELD)
  (NOT (MSG-HEADER-RECIPIENT-MATCH (MSG-GET MSG :FROM) FIELD)))

(DEFUN (MSG-HAS-SENDER-OR-RECIPIENT-FIELD FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (FIELD)
  (STRING-APPEND "(From//To: " FIELD #/)))

(DEFUN MSG-HAS-SENDER-OR-RECIPIENT-FIELD (MSG FIELD &AUX (STATUS (ASSURE-MSG-PARSED MSG)))
  (DOLIST (F *SENDER-OR-RECIPIENT-TYPE-HEADERS*)
    (AND (MSG-HEADER-RECIPIENT-MATCH (GET STATUS F) FIELD)
         (RETURN T))))

(DEFPROP MSG-DOES-NOT-HAVE-SENDER-OR-RECIPIENT-FIELD MSG-HAS-SENDER-OR-RECIPIENT-FIELD
         FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-SENDER-OR-RECIPIENT-FIELD (MSG FIELD
                                                    &AUX (STATUS (ASSURE-MSG-PARSED MSG)))
  (NOT (DOLIST (F *SENDER-OR-RECIPIENT-TYPE-HEADERS*)
         (AND (MSG-HEADER-RECIPIENT-MATCH (GET STATUS F) FIELD)
              (RETURN T)))))

(DEFUN (MSG-HAS-SUBJECT-STRING FILTER-FUNCTION-BUFFER-NAME-FUNCTION) (KEY)
  (STRING-APPEND "(Subject: " KEY #/)))

(DEFUN MSG-HAS-SUBJECT-STRING (MSG KEY &AUX SUBJECT)
  (NOT (NULL (AND (SETQ SUBJECT (MSG-GET MSG :SUBJECT))
                  (IF (CONSP SUBJECT)
                      (LOOP FOR STRING IN SUBJECT
                            THEREIS (STRING-SEARCH KEY STRING))
                    (STRING-SEARCH KEY SUBJECT))))))

(DEFPROP MSG-DOES-NOT-HAVE-SUBJECT-STRING MSG-HAS-SUBJECT-STRING
         FILTER-FUNCTION-OPPOSITE-FUNCTION)

(DEFUN MSG-DOES-NOT-HAVE-SUBJECT-STRING (MSG KEY &AUX SUBJECT)
  (NOT (AND (SETQ SUBJECT (MSG-GET MSG :SUBJECT))
            (IF (CONSP SUBJECT)
                (LOOP FOR STRING IN SUBJECT
                      THEREIS (STRING-SEARCH KEY STRING))
              (STRING-SEARCH KEY SUBJECT)))))

;;; The filter definition frame, which appears if you ask to define a new filter
;;; when in the filter selection frame (and in other ways).

(DEFFLAVOR ZMAIL-FILTER-FRAME
           ()
           (ZMAIL-COMMAND-LOOP-MIXIN-WITH-SUMMARY
            TV:SELECT-MIXIN ZMAIL-UTILITY-FRAME)
  (:DEFAULT-INIT-PLIST :EDITOR-CLOSURE-VARIABLES ZMAIL-FILTER-FRAME-EDITOR-CLOSURE-VARIABLES
    :COMTAB *STANDALONE-COMTAB*))

(DEFVAR *EDITOR-WINDOW* :UNBOUND)
(DEFVAR *EDITOR-INTERVAL* :UNBOUND)
(DEFVAR *EDITOR-ISTREAM* :UNBOUND)
(DEFVAR *EDITOR-INSERT-BP* :UNBOUND)

(DEFCONST ZMAIL-FILTER-FRAME-EDITOR-CLOSURE-VARIABLES
          (MERGE-CLOSURE-VARIABLE-LISTS
            '((*MODE-LINE-LIST* '("ZMail " "Filter"))
              (*MAJOR-MODE* NIL)
              (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
              (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
              (*SELECTABLE-MODE-LINE-ELEMENTS* NIL)
              (*EDITOR-WINDOW* NIL)
              (*EDITOR-INTERVAL* NIL)
              (*EDITOR-ISTREAM* NIL)
              (*EDITOR-INSERT-BP* NIL))
            TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFMETHOD (ZMAIL-FILTER-FRAME :TOP-OF-EDITOR-HIERARCHY) () SELF)

(DEFMETHOD (ZMAIL-FILTER-FRAME :AFTER :INIT) (IGNORE)
  (SETQ MODE-LINE-WINDOW (SEND SELF :GET-PANE 'MODE-LINE-WINDOW))
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (SETQ *EDITOR-WINDOW* (SEND SELF :GET-PANE 'EDITOR-WINDOW))
  (SEND *EDITOR-WINDOW* :SET-EDITOR-CLOSURE EDITOR-CLOSURE)
  (INITIALIZE-TOP-LEVEL-EDITOR *EDITOR-WINDOW* T)
  (SETQ *EDITOR-INTERVAL* *INTERVAL*)
  (SETQ *EDITOR-ISTREAM* (INTERVAL-STREAM *EDITOR-INTERVAL* NIL NIL :TYI))
  (SETQ *EDITOR-INSERT-BP* (COPY-BP (POINT) :MOVES))
  (SET-COMTAB *COMTAB* (LIST #/C-H (COMMAND-LOOKUP #/C-H *SEARCH-MINI-BUFFER-COMTAB*))))

(DEFVAR *HEADER-FILTER-MENU-ALIST*
        '(("To" :VALUE :TO :DOCUMENTATION "Messages to a recipient in the To: line")
          ("To//Cc" :VALUE :TO//CC
           :DOCUMENTATION "Messages to a recipient in TO:, CC:, Forwarded to:, etc.")
          ("From" :VALUE :FROM :DOCUMENTATION "Messages from a sender")
          ("Subject" :VALUE :SUBJECT :DOCUMENTATION
           "Messages with a given string anywhere in the subject line")
          ("Other" :VALUE :OTHER
           :DOCUMENTATION "messages with a string in an arbitrary header field")))

(DEFVAR *DATE-FILTER-MENU-ALIST*
        '(("Before" :VALUE :BEFORE :DOCUMENTATION "Messages before a given constant date")
          ("On" :VALUE :ON :DOCUMENTATION "Messages on a specific date")
          ("After" :VALUE :AFTER :DOCUMENTATION "Messages after a specific date")))

;;; Arrange the panes of the filter definition frame.

(DEFMETHOD (ZMAIL-FILTER-FRAME :BEFORE :INIT) (IGNORE
                                               &AUX MODE-LINE-LINE-HEIGHT MODE-LINE-HEIGHT)
  (SETQ MODE-LINE-LINE-HEIGHT (+ 2 (MAX (FONT-CHAR-HEIGHT TV:(SCREEN-DEFAULT-FONT
                                                              (SHEET-GET-SCREEN SUPERIOR)))
                                       (FONT-CHAR-HEIGHT FONTS:SEARCH)))
        MODE-LINE-HEIGHT (+ 11 (* 3 MODE-LINE-LINE-HEIGHT)))
  (SETQ TV:PANES `((NOT-BUTTON TV:BUTTON-PANE :NAME "Not"
                                              :DOCUMENTATION "Negate a clause.")
                   (AND-BUTTON TV:BUTTON-PANE :NAME "And"
                               :DOCUMENTATION "Logical and of several clauses.")
                   (OR-BUTTON TV:BUTTON-PANE :NAME "Or"
                                             :DOCUMENTATION "Logical or of several clauses.")
                   (CLOSE-BUTTON TV:BUTTON-PANE :NAME "Close"
                                                :DOCUMENTATION
  "Add clauses to the next higher AND or OR.")
                   (SAMPLE-BUTTON TV:BUTTON-PANE :NAME "Sample"
                                                 :DOCUMENTATION
  "Show messages matching the filter as so far defined in the typeout window.")
                   (DONE-BUTTON TV:BUTTON-PANE :NAME "Done"
                                               :DOCUMENTATION "Use this filter definition.")
                   (ABORT-BUTTON TV:BUTTON-PANE :NAME "Abort"
                                                :DOCUMENTATION "Abort this command.")
                   (KEYWORD-COMMAND-MENU ZMAIL-COMMAND-MENU-PANE
                                         :ITEM-LIST NIL :LABEL "Keywords:")
                   (USER-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                                     :ITEM-LIST NIL :LABEL "Filters:")
                   (SYSTEM-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                                       :ITEM-LIST ,*SYSTEM-FILTER-ALIST*)
                   (HEADER-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                                       :ITEM-LIST ,*HEADER-FILTER-MENU-ALIST*)
                   (DATE-FILTER-MENU ZMAIL-COMMAND-MENU-PANE
                                     :ITEM-LIST ,*DATE-FILTER-MENU-ALIST*)
                   (NAME-BUTTON TV:BIG-BUTTON-PANE :NAME "Name"
                                                   :BORDERS 3
                                                   :DOCUMENTATION
  "Specify a new name for this filter.  Click right for a menu of existing filters to edit.")
                   (EDITOR-WINDOW ZMAIL-WINDOW :LABEL NIL :BORDERS (2 2 2 1) :SAVE-BITS NIL
                                  :FONT-MAP (FONTS:CPTFONT FONTS:SEARCH))
                   (MODE-LINE-WINDOW ZMAIL-MOUSE-SENSITIVE-MODE-LINE-PANE
                                     :HEIGHT ,MODE-LINE-HEIGHT
                                     :MORE-P NIL :BORDERS (2 1 2 2)
                                     :BLINKER-DESELECTED-VISIBILITY :OFF))
        TV:CONSTRAINTS
               `((ONLY . ((WHOLE-THING)
                           ((WHOLE-THING TV:WHITE-INCLUDE-WHITESPACE    ;Horiz
                             (0.9) (:EVEN)
                             (MENUS FORM)
                             ((FORM TV:WHITE-INCLUDE-WHITESPACE ;Vert
                               (0.625) (:EVEN)
                               (NAMEX EDITORSS)
                               ((NAMEX TV:SINGLE-PANE-IN-WHITESPACE NAME-BUTTON))
                               ((EDITORSS :HORIZONTAL (0.85)
                                 (EDITORS)
                                 ((EDITORS :VERTICAL (:EVEN)
                                   (EDITOR-WINDOW MODE-LINE-WINDOW)
                                   ((MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                                   ((EDITOR-WINDOW :EVEN))))))))
                             ((MENUS TV:WHITE-INCLUDE-WHITESPACE        ;Vert
                               (0.95) (:EVEN)
                               (CONDITIONALS CONTROLS SYSTEM-MENUS USER-MENUS)
                               ((CONDITIONALS TV:FLOATING-BUTTONS
                                 (NOT-BUTTON AND-BUTTON OR-BUTTON CLOSE-BUTTON)))
                               ((CONTROLS TV:FLOATING-BUTTONS
                                 (SAMPLE-BUTTON DONE-BUTTON ABORT-BUTTON)))
                               ((SYSTEM-MENUS TV:FLOATING-MENUS
                                 (:ASK-WINDOW SYSTEM-FILTER-MENU :PANE-SIZE-WITH-WHITESPACE)
                                 (SYSTEM-FILTER-MENU HEADER-FILTER-MENU DATE-FILTER-MENU)))
                               ((USER-MENUS TV:FLOATING-MENUS
                                 (:ASK-WINDOW SELF :USER-MENUS-SIZE)
                                 (KEYWORD-COMMAND-MENU USER-FILTER-MENU))))))))))))

(DEFMETHOD (ZMAIL-FILTER-FRAME :USER-MENUS-SIZE) (&REST ARGS)
  (MAX (LEXPR-SEND SELF :SEND-PANE 'KEYWORD-COMMAND-MENU :PANE-SIZE-WITH-WHITESPACE ARGS)
       (LEXPR-SEND SELF :SEND-PANE 'USER-FILTER-MENU :PANE-SIZE-WITH-WHITESPACE ARGS)))

;; This is sent when the window configuration is changed to :FILTER.
(DEFMETHOD (ZMAIL-FILTER-FRAME :INITIALIZE) (&AUX NEW-NAME CHANGED-P)
  (SETQ NEW-NAME (GENERATE-UNIQUE-NAME *USER-FILTER-ALIST*))
  (SETQ CHANGED-P (SEND SELF :SET-PANES-ITEM-LIST 'KEYWORD-COMMAND-MENU
                             (APPEND '(("Any" :VALUE ANY
                                        :FONT FONTS:HL12BI :DOCUMENTATION
                                        "Messages with any keyword on them."))
                                     *KEYWORD-ALIST*
                                     NIL)))
  (SETQ CHANGED-P (OR (SEND SELF :SET-PANES-ITEM-LIST
                                 'USER-FILTER-MENU (COPYLIST *USER-FILTER-ALIST*))
                      CHANGED-P))
  (AND CHANGED-P (SEND SELF :SET-CONFIGURATION 'ONLY))
  (SEND SELF :TURN-OFF-ACCENTS)
  (SEND SELF :SET-PANES-NAME 'NAME-BUTTON NEW-NAME)
  (SEND SELF :SET-SELECTION-SUBSTITUTE NIL)
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (UNLESS (EQ *MAJOR-MODE* 'LISP-MODE)
    (COM-LISP-MODE))
  (LET ((*BATCH-UNDO-SAVE* T))
    (DELETE-INTERVAL *EDITOR-INTERVAL*)
    (INSERT-FORM-INTO-WINDOW
      `(DEFINE-FILTER ,(INTERN (STRING-UPCASE NEW-NAME)) (MSG)) -1))
  (SETF (WINDOW-BASE-TICK *EDITOR-WINDOW*) (TICK))
  (DISCARD-UNDO-INFORMATION *EDITOR-INTERVAL*)
  (MUST-REDISPLAY *EDITOR-WINDOW* DIS-ALL)
  (FORMAT *QUERY-IO* "~&")
  (SEND SELF :SET-SELECTION-SUBSTITUTE NIL))

;;; These are the methods that :COMMAND-LOOP expects us to provide.

(DEFMETHOD (ZMAIL-FILTER-FRAME :TOP-LEVEL-TAG) () 'EXIT-FILTER-DEFINITION)

(DEFMETHOD (ZMAIL-FILTER-FRAME :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY 'ZMAIL-FILTER-COMMAND-LIST ARGS))

(DEFSELECT (ZMAIL-FILTER-COMMAND-LIST ZMAIL-COMMAND-LIST-DEFAULT)
  (:MENU (ITEM IGNORE WINDOW &AUX WINDOW-NAME ITEM-NAME)
   (SETQ WINDOW-NAME (SEND SELF :PANE-NAME WINDOW)
         ITEM-NAME (IF (ATOM ITEM) ITEM (CAR ITEM))
         ITEM (SEND WINDOW :EXECUTE-NO-SIDE-EFFECTS ITEM))
   (CASE WINDOW-NAME
     (KEYWORD-COMMAND-MENU
      (INSERT-FORM-INTO-WINDOW (IF (EQ ITEM 'ANY)
                                   'KEYWORDS
                                 `(MEMQ ',ITEM KEYWORDS))))
     (USER-FILTER-MENU
      (INSERT-FORM-INTO-WINDOW `(MSG-FITS-FILTER-P MSG ',ITEM)))
     (OTHERWISE
      (INSERT-FILTER ITEM-NAME ITEM)))
   DIS-NONE)
  (SELECT-WINDOW (WINDOW)                       ;Moused a window, edit there
    (TV:WITH-SELECTION-SUBSTITUTE (WINDOW SELF)
;      (MAKE-WINDOW-CURRENT WINDOW)
      (*CATCH 'ABORT-STANDALONE-EDIT
        (SEND *WINDOW* :EDIT)))
    DIS-NONE)
  (:MOUSE-BUTTON (CH WINDOW IGNORE IGNORE &AUX WINDOW-NAME)
    (COND ((EQ WINDOW *WINDOW*)
           (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
           (SEND SELF :PROCESS-SPECIAL-COMMAND 'SELECT-WINDOW *WINDOW*))
          ((SEND WINDOW :OPERATION-HANDLED-P :SET-ACCENT)
           (SETQ WINDOW-NAME (SEND SELF :PANE-NAME WINDOW))
           (UNWIND-PROTECT
               (CASE WINDOW-NAME
                 (ABORT-BUTTON
                  (*THROW 'EXIT-FILTER-DEFINITION NIL))
                 (DONE-BUTTON
                  (*THROW 'EXIT-FILTER-DEFINITION (GET-AND-COMPILE-FILTER)))
                 (SAMPLE-BUTTON
                  (LET ((MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
                        (MAP-ARG *ZMAIL-BUFFER*))
                    (AND (= CH #/MOUSE-3-1)
                         (MULTIPLE-VALUE (MAP-FUNCTION MAP-ARG)
                           (GET-UNIVERSE-FUNCTION `(:WINDOW ,SELF))))
                    (OR MAP-FUNCTION (ABORT-CURRENT-COMMAND))
                    (SURVEY-FROM-FILTER MAP-FUNCTION MAP-ARG
                                        'MSG-FITS-FILTER-P (GET-AND-COMPILE-FILTER)))
                  (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT))
                 (NAME-BUTTON
                  (READ-NEW-NAME WINDOW CH *USER-FILTER-ALIST* 'GET-FILTER-DEFINITION))
                 (CLOSE-BUTTON
                  (EDITOR-WINDOW-CLOSE-BUTTON))
                 (OTHERWISE
                  (INSERT-FORM-INTO-WINDOW `(,(CASE WINDOW-NAME
                                                (NOT-BUTTON 'NOT)
                                                (AND-BUTTON 'AND)
                                                (OR-BUTTON 'OR)))
                                           -1)))
             (SEND WINDOW :SET-ACCENT NIL)))
          (T NIL))                              ;random window
    DIS-NONE)
  (SUMMARY-MOUSE (ITEM IGNORE IGNORE &AUX (MSG (CADR ITEM)))    ;Mouse in summary window
   (EXTRACT-FILTERS-FROM-MSG MSG)
   DIS-NONE)
  ((:TYPEOUT-EXECUTE SUMMARY-EXECUTE) (&REST IGNORE)
    (BARF)))

(DEFUN INSERT-FILTER (NAME TYPE)
  (INSERT-FORM-INTO-WINDOW
    (LET ((*TYPE* TYPE))
      (DECLARE (SPECIAL *TYPE*))
      (CONDITION-BIND
        ((UNKNOWN-SPECIAL-COMMAND 'ZMAIL-FILTER-MINI-BUFFER-UNKNOWN-SPECIAL-COMMAND))
        (CASE TYPE
          ((DELETED UNSEEN ANSWERED RECENT FILED)
           `(GET STATUS ',TYPE))
          (:SEARCH
           (LET ((KEY (READ-SEARCH-KEY-FROM-EDITOR-WINDOW "String to search for:")))
             `(SEARCH-WITHIN-MSG ,KEY)))
          ((:TO :TO//CC :FROM :SUBJECT :OTHER)
           (COND ((EQ TYPE :OTHER)
                  (MULTIPLE-VALUE (NAME TYPE)
                    (READ-HEADER-NAME-FROM-EDITOR-WINDOW "Header name:"))))
           (LET ((KEY (READ-SEARCH-KEY-FROM-EDITOR-WINDOW (STRING-APPEND NAME #/:))))
             (IF (ZEROP (STRING-LENGTH KEY))
                 `(NOT (NULL (GET STATUS ',TYPE)))
               `(,(IF (OR (MEMQ TYPE *ADDRESS-TYPE-HEADERS*)
                          (EQ TYPE :TO//CC))
                      'MSG-HEADER-RECIPIENT-SEARCH 'MSG-HEADER-SEARCH)
                 ,(IF (EQ TYPE :TO//CC) '*RECIPIENT-TYPE-HEADERS* `',TYPE)
                 ,KEY))))
          ((:ON :BEFORE :AFTER)
           (MULTIPLE-VALUE-BIND (DATE RELATIVE-P)
               (READ-DATE-FROM-EDITOR-WINDOW "~A date:" NAME)
             (IF (NOT RELATIVE-P)
                 `(,(CASE TYPE
                      (:ON 'MSG-SAME-DATE)
                      (:BEFORE 'MSG-DATE-LESSP)
                      (:AFTER 'MSG-DATE-GREATERP))
                   ,DATE)
               `(,(CASE TYPE
                    (:ON 'MSG-SAME-RELATIVE-DATE)
                    (:BEFORE 'MSG-RELATIVE-DATE-LESSP)
                    (:AFTER 'MSG-RELATIVE-DATE-GREATERP))
                 ,DATE
                 ,(FORMAT-CURRENT-DATE-FOR-FILTER)))))
          )))))

(DEFUN INSERT-FORM-INTO-WINDOW (FORM &OPTIONAL (NCHARS 0) &AUX BP (POINT (POINT)))
  (SEND *EDITOR-ISTREAM* :SET-BP *EDITOR-INSERT-BP*)
  (LET ((*READTABLE* (INITIALIZE-SPECIAL-/#/"-READTABLE)))
    (GRIND-TOP-LEVEL FORM (SEND (WINDOW-SHEET *WINDOW*) :SIZE-IN-CHARACTERS)
                     *EDITOR-ISTREAM* T 'SI:DISPLACED NIL))
  (SETQ BP (SEND *EDITOR-ISTREAM* :READ-BP))
  (MOVE-BP POINT (COND ((= NCHARS 0) BP)
                       ((< NCHARS 0) (FORWARD-CHAR BP NCHARS))
                       (T (FORWARD-CHAR POINT NCHARS))))
  (DO ((N)
       (FLAG NIL))
      (FLAG)
    (DELETE-BACKWARD-OVER *WHITESPACE-CHARS* POINT)
    (SETQ FLAG T
          N (LET ((BP (FORWARD-SEXP POINT -1 NIL 1)))
              (IF BP (COUNT-LIST-ELEMENTS BP) 0)))
    (COND ((ZEROP N))
          ((= N 1) (INSERT-MOVING POINT #/SP))
          ((MEMQ (RELEVANT-FUNCTION-NAME POINT NIL NIL) '(NOT ))
           (MOVE-BP POINT (FORWARD-CHAR POINT 1))
           (SETQ FLAG NIL))
          (T (LET ((*NUMERIC-ARG-P* NIL) (*NUMERIC-ARG* 1))
               (COM-INDENT-NEW-LINE)))))
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (MOVE-BP *EDITOR-INSERT-BP* POINT))

(DEFUN READ-SEARCH-KEY-FROM-EDITOR-WINDOW (PROMPT)
  (GET-EXTENDED-SEARCH-16B-STRING PROMPT))

(DEFUN READLINE-FROM-EDITOR-WINDOW (&REST PROMPT)
  (APPLY 'TYPEIN-LINE-READLINE PROMPT))

(DEFUN READ-HEADER-NAME-FROM-EDITOR-WINDOW (PROMPT)
  (LET ((KEY (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *HEADER-NAME-ALIST* T)))
    (IF (STRINGP KEY)
        (VALUES KEY (INTERN (STRING-UPCASE KEY) ""))
      (VALUES (CAR KEY) (CDR KEY)))))

(DEFUN FORMAT-DATE-FOR-FILTER (STRING)
  (DECLARE (RETURNS TIME-STRING RELATIVE-P))
  (MULTIPLE-VALUE-BIND (TIME RELATIVE-P)
      (CONDITION-CASE (ERROR)
          (TIME:PARSE-UNIVERSAL-TIME STRING 0 NIL NIL)  ;Parse it, assuming not in future
        (ERROR (BARF ERROR)))
    (IF (AND RELATIVE-P
             (Y-OR-N-P "Do you want that time relative to when the filter is run?"))
        (VALUES STRING RELATIVE-P)
      (MULTIPLE-VALUE-BIND (SECONDS-OR-ERRMES MINUTES HOURS DAY MONTH YEAR)
          (TIME:DECODE-UNIVERSAL-TIME TIME)
        (AND ( YEAR 1900.) (< YEAR 2000.)
             (SETQ YEAR (- YEAR 1900.)))
        (FORMAT-DATE-FOR-FILTER-INTERNAL SECONDS-OR-ERRMES MINUTES HOURS DAY MONTH YEAR)))))

(DEFUN FORMAT-DATE-FOR-FILTER-INTERNAL (SECONDS MINUTES HOURS DAY MONTH YEAR)
  (FORMAT NIL "~D-~A-~D~:[ ~D:~2,48D~:[:~2,48D~]~]"
          DAY (TIME:MONTH-STRING MONTH :SHORT)
          YEAR (AND (ZEROP HOURS) (ZEROP MINUTES) (ZEROP SECONDS))
          HOURS MINUTES (ZEROP SECONDS) SECONDS))

(DEFUN FORMAT-CURRENT-DATE-FOR-FILTER ()
  (MULTIPLE-VALUE-BIND (SECONDS-OR-ERRMSG MINUTES HOURS DAY MONTH YEAR)
      (TIME:DECODE-UNIVERSAL-TIME (TIME:GET-UNIVERSAL-TIME))
    (IF (ERRORP SECONDS-OR-ERRMSG)
        (BARF SECONDS-OR-ERRMSG)
      (FORMAT-DATE-FOR-FILTER-INTERNAL SECONDS-OR-ERRMSG MINUTES HOURS DAY MONTH YEAR))))

(DEFUN READ-DATE-FROM-EDITOR-WINDOW (&REST PROMPT)
  (LET ((STRING (APPLY 'TYPEIN-LINE-READLINE PROMPT)))
    (FORMAT-DATE-FOR-FILTER STRING)))

;; Condition handler function for UNKNOWN-SPECIAL-COMMAND condition.
(DEFUN ZMAIL-FILTER-MINI-BUFFER-UNKNOWN-SPECIAL-COMMAND (&REST IGNORE &AUX MSG STRING)
  (DECLARE (SPECIAL *TYPE*))
  (COND ((EQ (CAR *LAST-COMMAND-CHAR*) 'SUMMARY-MOUSE)
         (SETQ MSG (CADADR *LAST-COMMAND-CHAR*))
         (SETQ STRING (MSG-HEADER-FILTER-STRING MSG *TYPE*))
         (DELETE-INTERVAL *INTERVAL*)
         (INSERT-MOVING (POINT) STRING)
         (MUST-REDISPLAY *WINDOW* DIS-TEXT)
         (VALUES :NEW-VALUE T))))

(DEFUN MSG-HEADER-FILTER-STRING (MSG TYPE &AUX STATUS PROP)
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (AND (MEMQ TYPE '(:ON :BEFORE :AFTER))
       (SETQ TYPE :DATE))
  (SETQ PROP (IF (EQ TYPE :TO//CC)
                 (DO ((TYPES *RECIPIENT-TYPE-HEADERS* (CDR TYPES))
                      (L NIL))
                     ((NULL TYPES) L)
                   (SETQ L (APPEND L (GET STATUS (CAR TYPES)))))
               (GET STATUS TYPE)))
  (COND ((NULL PROP) (BARF))
        ((EQ TYPE :DATE)
         (OR PROP (BARF))
         (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
             (TIME:DECODE-UNIVERSAL-TIME PROP)
           (AND ( YEAR 1900.) (< YEAR 2000.)
                (SETQ YEAR (- YEAR 1900.)))
           (FORMAT NIL "~D-~A-~D ~D:~2,48D:~2,48D" DAY (TIME:MONTH-STRING MONTH :SHORT) YEAR
                   HOURS MINUTES SECONDS)))
        ((STRINGP PROP) PROP)
        ((NULL (CDR PROP))
         (CANONICALIZE-RECIPIENT-FILTER-STRING (CAR PROP)))
        (T
         (UNWIND-PROTECT
           (PROGN
             (PRINT-TYPEOUT-FILTER (CAR (RASSQ TYPE *HEADER-FILTER-MENU-ALIST*))
                                   'MINI-BUFFER-STRING
                                   (MAPCAR 'CANONICALIZE-RECIPIENT-FILTER-STRING PROP))
             (LET ((CH (SEND *STANDARD-INPUT* :ANY-TYI)))
               (OR (AND (CONSP CH) (EQ (CAR CH) :TYPEOUT-EXECUTE))
                   (BARF))
               (CADDR CH)))
           (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)))))

(DEFUN CANONICALIZE-RECIPIENT-FILTER-STRING (STRING)
  (COND ((STRINGP STRING) STRING)
        ((NULL (CDR STRING)) (CAR STRING))
        (T (STRING-FROM-HEADER STRING :SHORT))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* MINI-BUFFER-STRING "Insert" STRING T
                          "Insert this string.")

(DEFUN EDITOR-WINDOW-CLOSE-BUTTON (&AUX (*NUMERIC-ARG* 1) (*NUMERIC-ARG-P* NIL)
                                        (*LAST-COMMAND-CHAR* #/)))
  (MUST-REDISPLAY *WINDOW* (COM-MOVE-OVER-/)))
  (MOVE-BP *EDITOR-INSERT-BP* (POINT)))

(DEFUN READ-FROM-EDITOR-WINDOW (&AUX (FORM '*EOF*))
  (SEND *EDITOR-ISTREAM* :SET-BP (INTERVAL-FIRST-BP *EDITOR-INTERVAL*))
  (CATCH-ERROR
    (LET ((*READTABLE* (INITIALIZE-SPECIAL-/#/"-READTABLE)))
      (SETQ FORM (READ *EDITOR-ISTREAM* '*EOF*)))
    NIL)
  (AND (EQ FORM '*EOF*) (BARF "Unbalanced parentheses"))
  FORM)

(DEFUN GET-AND-COMPILE-FILTER (&AUX FILTER-PROP FILTER)
  (SETQ *TYPEOUT-WINDOW* (WINDOW-TYPEOUT-WINDOW *WINDOW*)
        *TERMINAL-IO* *TYPEOUT-WINDOW*)
  (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
  (SETQ FILTER-PROP (EVAL (READ-FROM-EDITOR-WINDOW)))
  (OR (AND (EQ (CAR FILTER-PROP) :PROPERTY)
           (EQ (CADDR FILTER-PROP) 'FILTER-FUNCTION))
      (BARF "Does not look like a filter definition"))
  (SETQ FILTER (CADR FILTER-PROP))
  (PUTPROP FILTER (GET FILTER 'FILTER-FUNCTION) 'EXPR-FILTER-FUNCTION)
  (COMPILE FILTER-PROP)
  (AND (SEND *TYPEOUT-WINDOW* :INCOMPLETE-P)    ;If there are warning messages,
       (NOT (LET ((*QUERY-IO* *TYPEOUT-WINDOW*))
              (Y-OR-N-P "Ok? ")))               ;user has chance to not exit
       (ABORT-CURRENT-COMMAND))
  FILTER)

(DEFUN READ-NEW-NAME (NAME-BUTTON CHAR CHOICE-LIST DEFINITION-ACCESS-FUNCTION &AUX STRING)
  (COND ((= CHAR #/MOUSE-1-1)                   ;Left button gets new one
         (SETQ STRING (READLINE-FROM-EDITOR-WINDOW "New name:"))
         (LET ((BP (FORWARD-OVER *BLANKS* (FORWARD-ATOM (INTERVAL-FIRST-BP *INTERVAL*)))))
           (SETQ BP (DELETE-INTERVAL BP (FORWARD-ATOM BP) T))
           (INSERT BP (FORMAT NIL "~S" (INTERN STRING))))
         (MUST-REDISPLAY *WINDOW* DIS-TEXT))
        (T
         (SETQ STRING (TV:MENU-CHOOSE CHOICE-LIST NIL `(:WINDOW ,NAME-BUTTON)))
         (OR STRING (ABORT-CURRENT-COMMAND))
         (DELETE-INTERVAL *INTERVAL*)
         (INSERT-FORM-INTO-WINDOW (FUNCALL DEFINITION-ACCESS-FUNCTION STRING))
         (SETQ STRING (STRING STRING))))
  (SEND (TV:SHEET-SUPERIOR NAME-BUTTON) :SET-PANES-NAME 'NAME-BUTTON STRING))

(DEFVAR *FILTER-DEFINITION-SUMMARY-DOCUMENTATION* "Select filters based on this message.")

(DEFUN DEFINE-NEW-FILTER (&AUX OLD-DOC)
  (WITH-WINDOW-CONFIGURATION (:FILTER)
    (WITH-BACKGROUND-PROCESS-LOCKED
      (SETQ OLD-DOC (SEND *SUMMARY-WINDOW* :WHO-LINE-OVERRIDE-DOCUMENTATION-STRING))
      (PKG-BIND (SYMBOL-PACKAGE 'FOO)
        (UNWIND-PROTECT
          (PROGN
            (SEND *SUMMARY-WINDOW* :SET-WHO-LINE-OVERRIDE-DOCUMENTATION-STRING
                                   *FILTER-DEFINITION-SUMMARY-DOCUMENTATION*)
            (SEND *FILTER-WINDOW* :COMMAND-LOOP))
          (SEND *SUMMARY-WINDOW* :SET-WHO-LINE-OVERRIDE-DOCUMENTATION-STRING OLD-DOC))))))

(DEFUN GENERATE-UNIQUE-NAME (LIST &OPTIONAL (NAME "Noname"))
  (DO ((I 1 (1+ I))
       (STRING))
      (NIL)
    (SETQ STRING (FORMAT NIL "~A-~D" NAME I))
    (OR (MEM #'(LAMBDA (X Y)
                 (IF (NOT (ATOM Y))
                     (SETQ Y (CAR Y)))
                 (STRING-EQUAL X Y))
             STRING LIST)
        (RETURN STRING))))

(DEFUN EXTRACT-FILTERS-FROM-MSG (MSG)
  (DO-NAMED TOP
      ((*TYPEOUT-WINDOW*))
      (NIL)
    (PRINT-MSG-TYPEOUT-FILTERS MSG)
    (DO ((CH) (TYPE))
        (NIL)
      (REDISPLAY-ALL-WINDOWS)
      (SETQ CH (SEND *STANDARD-INPUT* :ANY-TYI))
      (COND ((OR (ATOM CH) (NOT (MEMQ (SETQ TYPE (CAR CH))
                                        '(:TYPEOUT-EXECUTE SUMMARY-MOUSE))))
             (OR (EQ CH #/SP) (SEND *STANDARD-INPUT* :UNTYI CH))
             (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
             (RETURN-FROM TOP NIL))
            ((EQ TYPE 'SUMMARY-MOUSE)
             (SETQ MSG (CADADR CH))
             (RETURN))
            ((EQ TYPE :TYPEOUT-EXECUTE)
             (APPLY (CADR CH) (CDDR CH)))))))

(DEFUN PRINT-MSG-TYPEOUT-FILTERS (MSG &OPTIONAL JUST-FROM-TO INCLUDE-SITE &AUX STATUS)
  (SETQ *TYPEOUT-WINDOW* (SEND *SUMMARY-WINDOW* :TYPEOUT-WINDOW))
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (COND (JUST-FROM-TO
         (PRINT-MSG-TYPEOUT-FILTERS-1 "From//To: " 'SENDER-OR-RECIPIENT-FIELD
                                      STATUS *SENDER-OR-RECIPIENT-TYPE-HEADERS*
                                      INCLUDE-SITE))
        (T
         (PRINT-MSG-TYPEOUT-FILTERS-1 "From: " 'FROM-FIELD
                                      STATUS *SENDER-TYPE-HEADERS* INCLUDE-SITE)
         (PRINT-MSG-TYPEOUT-FILTERS-1 "Recipients: " 'RECIPIENT-FIELD
                                      STATUS *RECIPIENT-TYPE-HEADERS* INCLUDE-SITE)
         (PRINT-TYPEOUT-FILTER "Subject: " 'SUBJECT-FIELD
                               (GET-MSG-SUBJECT-CLEVERLY MSG NIL)))))

(DEFUN PRINT-MSG-TYPEOUT-FILTERS-1 (NAME TYPE STATUS LIST INCLUDE-SITE)
  (PRINT-TYPEOUT-FILTER NAME TYPE
                        (LOOP FOR IND IN LIST
                              NCONC (MAKE-RECIPIENT-TYPEOUT-ALIST STATUS IND INCLUDE-SITE))))

(DEFUN PRINT-TYPEOUT-FILTER (NAME TYPE ELEMENTS)
  (COND ((NOT (NULL ELEMENTS))
         (SEND *TYPEOUT-WINDOW* :FRESH-LINE)
         (SEND *TYPEOUT-WINDOW* :STRING-OUT NAME)
         (IF (OR (ATOM ELEMENTS) (NULL (CDR ELEMENTS))) ;Only one element
             (SEND *TYPEOUT-WINDOW* :ITEM TYPE
                                    (IF (CONSP ELEMENTS) (CAR ELEMENTS) ELEMENTS))
           (SEND *TYPEOUT-WINDOW* :ITEM-LIST TYPE ELEMENTS)))))

(DEFUN MAKE-RECIPIENT-TYPEOUT-ALIST (STATUS TYPE INCLUDE-SITE)
  (LOOP FOR HEADER IN (GET STATUS TYPE)
        COLLECT (IF INCLUDE-SITE
                    (STRING-FROM-HEADER HEADER :SHORT)
                  (GET (LOCF HEADER) :NAME))))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* FROM-FIELD "Insert" INSERT-FROM-FIELD T
                          "Insert this from field.")

(DEFUN INSERT-FROM-FIELD (FIELD)
  (INSERT-FORM-INTO-WINDOW `(MSG-HEADER-RECIPIENT-SEARCH :FROM ,FIELD)))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* RECIPIENT-FIELD "Insert"
                          INSERT-RECIPIENT-FIELD T
                          "Insert this recipient field.")

(DEFUN INSERT-RECIPIENT-FIELD (FIELD)
  (INSERT-FORM-INTO-WINDOW `(MSG-HEADER-RECIPIENT-SEARCH *RECIPIENT-TYPE-HEADERS*
                                                                ,FIELD)))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* SENDER-OR-RECIPIENT-FIELD "Insert"
                          INSERT-SENDER-OR-RECIPIENT-FIELD T
                          "Insert this recipient field.")

(DEFUN INSERT-SENDER-OR-RECIPIENT-FIELD (FIELD)
  (INSERT-FORM-INTO-WINDOW
    `(MSG-HEADER-RECIPIENT-SEARCH *SENDER-OR-RECIPIENT-TYPE-HEADERS* ,FIELD)))

(TV:ADD-TYPEOUT-ITEM-TYPE *ZMAIL-TYPEOUT-ITEM-ALIST* SUBJECT-FIELD "Insert"
                          INSERT-SUBJECT-FIELD T "Insert this subject field.")

(DEFUN INSERT-SUBJECT-FIELD (FIELD)
  (INSERT-FORM-INTO-WINDOW `(MSG-HEADER-SEARCH :SUBJECT ,FIELD)))

(DEFUN CHOOSE-MSG-OR-READLINE (PROMPT &OPTIONAL DEFAULT &AUX RESULT)
  (let ((*throw-unknown-special-command* 'choose-msg-or-readline))
    (*CATCH *throw-unknown-special-command*
      (WITH-BACKGROUND-PROCESS-LOCKED
        (CONDITION-BIND ((UNKNOWN-SPECIAL-COMMAND
                           'CHOOSE-MSG-OR-READLINE-UNKNOWN-SPECIAL-COMMAND))
          (SETQ RESULT (TYPEIN-LINE-READLINE
                         "~A:~@[ (Default: ~A)~]~:[ (Or select message with mouse)~]"
                         PROMPT DEFAULT
                         (NOT (SEND *SUMMARY-WINDOW* :EXPOSED-P))))))
      (AND DEFAULT (EQUAL RESULT "")
           (SETQ RESULT DEFAULT))
      RESULT)))

(DEFUN CHOOSE-MSG-OR-READLINE-UNKNOWN-SPECIAL-COMMAND (&REST IGNORE)
  (AND (EQ (CAR *LAST-COMMAND-CHAR*) 'SUMMARY-MOUSE)
       (*THROW 'CHOOSE-MSG-OR-READLINE (CADADR *LAST-COMMAND-CHAR*))))  ;Return the msg

(DEFUN CHOOSE-OR-READLINE-ADDRESS (PROMPT &OPTIONAL NOT-P INCLUDE-SITE DEFAULT &AUX X)
  (SETQ X (CHOOSE-MSG-OR-READLINE PROMPT DEFAULT))
  (IF (STRINGP X)                               ;Typed by user
      (VALUES (IF NOT-P
                  'MSG-DOES-NOT-HAVE-SENDER-OR-RECIPIENT-FIELD
                'MSG-HAS-SENDER-OR-RECIPIENT-FIELD)
              X)
    (GET-FILTERS-FROM-MSG X NOT-P T INCLUDE-SITE)))

(DEFUN GET-FILTERS-FROM-MSG (MSG NOT-P &OPTIONAL JUST-FROM-TO INCLUDE-SITE
                                       &AUX CH FUN *TYPEOUT-WINDOW*)
  (PRINT-MSG-TYPEOUT-FILTERS MSG JUST-FROM-TO INCLUDE-SITE)
  (SEND *TYPEOUT-WINDOW* :FRESH-LINE)
  (SETQ CH (SEND *STANDARD-INPUT* :ANY-TYI))
  (SEND *TYPEOUT-WINDOW* :MAKE-COMPLETE)
  (OR (AND (CONSP CH) (EQ (CAR CH) :TYPEOUT-EXECUTE))
      (BARF))
  (SETQ FUN (CASE (CADR CH)
              (INSERT-FROM-FIELD
               (IF NOT-P 'MSG-DOES-NOT-HAVE-FROM-FIELD 'MSG-HAS-FROM-FIELD))
              (INSERT-RECIPIENT-FIELD
               (IF NOT-P 'MSG-DOES-NOT-HAVE-RECIPIENT-FIELD 'MSG-HAS-RECIPIENT-FIELD))
              (INSERT-SENDER-OR-RECIPIENT-FIELD
               (IF NOT-P 'MSG-DOES-NOT-HAVE-SENDER-OR-RECIPIENT-FIELD
                         'MSG-HAS-SENDER-OR-RECIPIENT-FIELD))
              (INSERT-SUBJECT-FIELD
               (IF NOT-P 'MSG-DOES-NOT-HAVE-SUBJECT-STRING 'MSG-HAS-SUBJECT-STRING))
              (OTHERWISE
               (BARF))))
  (VALUES FUN (CADDR CH)))

(DEFUN GET-MSG-SUBJECT-CLEVERLY (MSG &OPTIONAL (ERROR-P T) &AUX SUBJECT START END)
  (COND ((SETQ SUBJECT (MSG-GET MSG :SUBJECT))
         (SETQ START 0 END (STRING-LENGTH SUBJECT))
         (DO () ((NOT (%STRING-EQUAL SUBJECT START "Re: " 0 4)))
           (SETQ START (+ START 4)))
         (DO ((TEM)) ((NOT (AND (PLUSP END) (= (AREF SUBJECT (1- END)) #/]))))
           (OR (SETQ TEM (STRING-SEARCH-CHAR #/: SUBJECT START END))
               (RETURN))
           (SETQ TEM (1+ TEM))
           (DO () ((NOT (MEMQ (AREF SUBJECT TEM) '(#/SP #/TAB))))
             (SETQ TEM (1+ TEM)))
           (SETQ START TEM
                 END (1- END)))
         (IF (AND (= START 0) (= END (STRING-LENGTH SUBJECT)))
             SUBJECT (SUBSTRING SUBJECT START END)))
        (ERROR-P
         (BARF "This message has no subject"))))

(DEFUN MAKE-ZMAIL-BUFFER-FROM-FILTER-FROM-MSG (MSG)
  (MULTIPLE-VALUE-BIND (FILTER-FUNCTION FILTER-ARG)
      (GET-FILTERS-FROM-MSG MSG NIL)
    (MAKE-ZMAIL-BUFFER-FROM-FILTER 'MAP-OVER-SINGLE-ZMAIL-BUFFER *ZMAIL-BUFFER*
                                   FILTER-FUNCTION FILTER-ARG)))

;;; Choosing and defining universes.

(DEFUN GET-UNIVERSE-FUNCTION (&OPTIONAL (NEAR-MODE '(:MOUSE)) LABEL
                              &AUX CHOICE MAP-FUNCTION MAP-ARG NAME)
  "Ask the user to choose a universe, using the universe menu.
Return digested information about the universe:
a map-function and an argument for it, which can be used to map over that universe.
The third value is a name for the universe."
  (DECLARE (RETURN-LIST MAP-FUNCTION MAP-ARG NAME))
  (SEND *UNIVERSE-SELECTION-MENU* :SET-LABEL LABEL)
  (MULTIPLE-VALUE-BIND (ZMAIL-BUFFER-ITEM-LIST TEMP-ZMAIL-BUFFER-ITEM-LIST)
      (GET-ZMAIL-BUFFER-ALISTS T)
    (SEND *UNIVERSE-SELECTION-MENU* :SET-ITEM-LISTS
          ZMAIL-BUFFER-ITEM-LIST
          TEMP-ZMAIL-BUFFER-ITEM-LIST
          (APPEND *UNIVERSE-LIST*
                  ;; "Built-in" universes
                  '(("Find file" :VALUE :FIND-FILE :FONT FONTS:HL12I
                     :DOCUMENTATION "Map over messages in a specified file.")
                    ("All" :VALUE :ALL :FONT FONTS:HL12I :DOCUMENTATION
                     "All messages, including those in files not yet read in.")
                    ("Loaded files" :VALUE :LOADED :FONT FONTS:HL12I
                     :DOCUMENTATION "All messages currently read in.")
                    ("New universe" :VALUE :NEW-UNIVERSE :FONT FONTS:HL12I
                     :DOCUMENTATION "Create a new universe by set operations."))
                  (AND *ZMAIL-BUFFER*
                       '(("Rest of current" :VALUE :REST :FONT FONTS:HL12I
                          :DOCUMENTATION "Messsages after this one in this buffer")
                         ("Beginning of current" :VALUE :BEGINNING :FONT FONTS:HL12I
                          :DOCUMENTATION "Messages before this one in this buffer"))))))
  (TV:EXPOSE-WINDOW-NEAR *UNIVERSE-SELECTION-MENU* NEAR-MODE)
  (SETQ CHOICE (SEND *UNIVERSE-SELECTION-MENU* :CHOOSE))
  (AND (EQ CHOICE :FIND-FILE)
       (SETQ CHOICE (READ-ZMAIL-BUFFER-FILENAME :MOUSE)))
  (AND (OR (STRINGP CHOICE) (TYPEP CHOICE 'FS:PATHNAME))
       (SETQ CHOICE (ZMAIL-FIND-FILE-NOSELECT CHOICE)))
  (COND ((NULL CHOICE)
         (SETQ MAP-FUNCTION NIL))
        ((EQ CHOICE :ALL)
         (SETQ MAP-FUNCTION 'MAP-OVER-ALL-ZMAIL-BUFFERS
               NAME "All"))
        ((EQ CHOICE :LOADED)
         (SETQ MAP-FUNCTION 'MAP-OVER-LOADED-ZMAIL-BUFFERS
               NAME "Loaded files"))
        ((EQ CHOICE :REST)
         (SETQ MAP-FUNCTION 'MAP-OVER-REST-OF-ZMAIL-BUFFER
               MAP-ARG *ZMAIL-BUFFER*
               NAME (FORMAT NIL "Rest of ~A" (ZMAIL-BUFFER-NAME MAP-ARG))))
        ((EQ CHOICE :BEGINNING)
         (SETQ MAP-FUNCTION 'MAP-OVER-BEGINNING-OF-ZMAIL-BUFFER
               MAP-ARG *ZMAIL-BUFFER*
               NAME (FORMAT NIL "Beginning of ~A" (ZMAIL-BUFFER-NAME MAP-ARG))))
        ((EQ CHOICE :NEW-UNIVERSE)
         (MULTIPLE-VALUE (MAP-FUNCTION MAP-ARG NAME)
           (DEFINE-NEW-UNIVERSE NEAR-MODE)))
        ((SYMBOLP CHOICE)
         (SETQ MAP-FUNCTION 'MAP-OVER-DEFINED-UNIVERSE
               MAP-ARG CHOICE
               NAME (STRING CHOICE)))
        (T
         (SETQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER
               MAP-ARG CHOICE
               NAME (ZMAIL-BUFFER-NAME MAP-ARG))
         (AND (ZMAIL-BUFFER-DISK-P CHOICE) (ASSURE-ZMAIL-BUFFER-FULLY-LOADED CHOICE))))
  (VALUES MAP-FUNCTION MAP-ARG NAME))

(DEFFLAVOR UNIVERSE-DEFINITION-FRAME
           ()
           (TV:TEMPORARY-WINDOW-MIXIN TV:SELECT-MIXIN ZMAIL-UTILITY-FRAME)
  (:DEFAULT-INIT-PLIST
    :EDITOR-CLOSURE-VARIABLES UNIVERSE-DEFINITION-FRAME-EDITOR-CLOSURE-VARIABLES
    :COMTAB *STANDALONE-COMTAB*))

(DEFCONST UNIVERSE-DEFINITION-FRAME-EDITOR-CLOSURE-VARIABLES
          (MERGE-CLOSURE-VARIABLE-LISTS
            '((*MODE-LINE-LIST* '("ZMail " "Universe"))
              (*MAJOR-MODE* NIL)
              (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
              (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
              (*SELECTABLE-MODE-LINE-ELEMENTS* NIL)
              (*EDITOR-WINDOW* NIL)
              (*EDITOR-INTERVAL* NIL)
              (*EDITOR-ISTREAM* NIL)
              (*EDITOR-INSERT-BP* NIL))
            TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :AFTER :INIT) (IGNORE)
  (SETQ MODE-LINE-WINDOW (SEND SELF :GET-PANE 'MODE-LINE-WINDOW))
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (SETQ *EDITOR-WINDOW* (SEND SELF :GET-PANE 'EDITOR-WINDOW))
  (SEND *EDITOR-WINDOW* :SET-EDITOR-CLOSURE EDITOR-CLOSURE)
  (INITIALIZE-TOP-LEVEL-EDITOR *EDITOR-WINDOW* T)
  (SETQ *EDITOR-INTERVAL* *INTERVAL*)
  (SETQ *EDITOR-ISTREAM* (INTERVAL-STREAM *INTERVAL*))
  (SETQ *EDITOR-INSERT-BP* (COPY-BP (POINT) :MOVES)))

;;; This page is concerned only with arranging the layout of the panes
;;; of the universe definition frame.

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :BEFORE :INIT) (IGNORE &AUX MODE-LINE-LINE-HEIGHT
                                                                  MODE-LINE-HEIGHT)
  (SETQ MODE-LINE-LINE-HEIGHT (+ 2 (MAX (FONT-CHAR-HEIGHT TV:(SCREEN-DEFAULT-FONT
                                                              (SHEET-GET-SCREEN SUPERIOR)))
                                       (FONT-CHAR-HEIGHT FONTS:SEARCH)))
        MODE-LINE-HEIGHT (+ 11 (* 2 MODE-LINE-LINE-HEIGHT)))
  (SETQ TV:PANES `((UNION-BUTTON TV:BUTTON-PANE :NAME "Union"
                                 :DOCUMENTATION "Set union of several universes.")
                   (INTERSECTION-BUTTON TV:BUTTON-PANE :NAME "Intersection"
                                                       :DOCUMENTATION
                                                    "Set intersection of several universes.")
                   (NOT-BUTTON TV:BUTTON-PANE :NAME "Not"
                                              :DOCUMENTATION
                                              "Set of all messages not in a universe.")
                   (CLOSE-BUTTON TV:BUTTON-PANE :NAME "Close"
                                                :DOCUMENTATION
                                                "Move to next higher Union or Intersection.")
                   (ZMAIL-BUFFER-MENU ZMAIL-COMMAND-MENU-PANE
                                      :ITEM-LIST NIL :LABEL "File buffers:")
                   (TEMP-ZMAIL-BUFFER-MENU ZMAIL-COMMAND-MENU-PANE
                                           :ITEM-LIST NIL :LABEL "")
                   (UNIVERSE-MENU ZMAIL-COMMAND-MENU-PANE
                                  :ITEM-LIST NIL :LABEL "Universes:")
                   (NAME-BUTTON TV:BIG-BUTTON-PANE :NAME "Name" :BORDERS 3
                                :DOCUMENTATION
 "Specify a new name for this universe.  Click right for a menu of existing filters to edit.")
                   (EDITOR-WINDOW ZMAIL-WINDOW :LABEL NIL :BORDERS (2 2 2 1) :SAVE-BITS NIL
                                  :CHARACTER-HEIGHT 10.)
                   (MODE-LINE-WINDOW ZMAIL-MOUSE-SENSITIVE-MODE-LINE-PANE
                                     :HEIGHT ,MODE-LINE-HEIGHT
                                     :MORE-P NIL :BORDERS (2 1 2 2)
                                     :BLINKER-DESELECTED-VISIBILITY :OFF)
                   (DONE-BUTTON TV:BUTTON-PANE :NAME "Done"
                                :DOCUMENTATION "Use this universe definition.")
                   (ABORT-BUTTON TV:BUTTON-PANE :NAME "Abort"
                                 :DOCUMENTATION "Abort this command."))
        TV:CONSTRAINTS
              `((ONLY . ( (WHOLE-THING)
                          ((WHOLE-THING :HORIZONTAL (:EVEN)
                            (WHOLE)
                            ((WHOLE TV:WHITE-INCLUDE-WHITESPACE ;Vert
                              (0.95) (:EVEN)
                              (OPERATIONS MENUS NAME EDITOR CONTROLS)
                              ((OPERATIONS TV:FLOATING-BUTTONS
                                (UNION-BUTTON INTERSECTION-BUTTON NOT-BUTTON CLOSE-BUTTON)))
                              ((NAME TV:SINGLE-PANE-IN-WHITESPACE NAME-BUTTON))
                              ((CONTROLS TV:FLOATING-BUTTONS
                                (DONE-BUTTON ABORT-BUTTON)))
                              ((EDITOR TV:WHITE-INCLUDE-WHITESPACE      ;Horiz
                                (:ASK-WINDOW SELF :EDITOR-SIZE) (:EVEN)
                                (EDITORS)
                                ((EDITORS :VERTICAL (0.8)
                                  (EDITOR-WINDOW MODE-LINE-WINDOW)
                                  ((MODE-LINE-WINDOW ,MODE-LINE-HEIGHT))
                                  ((EDITOR-WINDOW :EVEN))))))
                              ;; This comes last since it can afford a scroll bar
                              ((MENUS TV:FLOATING-MENUS
                                (:ASK-WINDOW SELF :MENUS-SIZE)
                                (ZMAIL-BUFFER-MENU TEMP-ZMAIL-BUFFER-MENU UNIVERSE-MENU))))))))))))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :MENUS-SIZE) (&REST ARGS)
  (MAX (LEXPR-SEND SELF :SEND-PANE 'ZMAIL-BUFFER-MENU :PANE-SIZE ARGS)
       (LEXPR-SEND SELF :SEND-PANE 'TEMP-ZMAIL-BUFFER-MENU :PANE-SIZE ARGS)
       (LEXPR-SEND SELF :SEND-PANE 'UNIVERSE-MENU :PANE-SIZE ARGS)))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :EDITOR-SIZE) (&REST IGNORE)
  (+ (TV:SHEET-HEIGHT (SEND SELF :GET-PANE 'MODE-LINE-WINDOW))
     (LET ((EDITOR-WINDOW (SEND SELF :GET-PANE 'EDITOR-WINDOW)))
       (+ (TV:SHEET-TOP-MARGIN-SIZE EDITOR-WINDOW)
          (TV:SHEET-BOTTOM-MARGIN-SIZE EDITOR-WINDOW)
          (* 10. (TV:SHEET-LINE-HEIGHT EDITOR-WINDOW))))))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :COMPUTE-GEOMETRY) (UNIVERSE-NAME ZMAIL-BUFFER-ALIST
                                                         TEMP-ZMAIL-BUFFER-ALIST UNIVERSE-ALIST
                                                         &AUX MAX-WIDTH MAX-HEIGHT CHANGED-P)
  (SETQ MAX-WIDTH TV:(- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
        MAX-HEIGHT TV:(- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
  (SEND SELF :SET-PANES-NAME 'NAME-BUTTON UNIVERSE-NAME)
  (SETQ CHANGED-P (SEND SELF :SET-PANES-ITEM-LIST 'ZMAIL-BUFFER-MENU ZMAIL-BUFFER-ALIST))
  (SETQ CHANGED-P (OR (SEND SELF :SET-PANES-ITEM-LIST 'TEMP-ZMAIL-BUFFER-MENU
                            TEMP-ZMAIL-BUFFER-ALIST)
                      CHANGED-P))
  (SETQ CHANGED-P (OR (SEND SELF :SET-PANES-ITEM-LIST 'UNIVERSE-MENU UNIVERSE-ALIST)
                      CHANGED-P))
  (AND CHANGED-P
       (LET ((WID (MIN MAX-WIDTH
                       (TRUNCATE
                         (* (MAX (+ (SEND SELF :SEND-PANE 'UNION-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'INTERSECTION-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'NOT-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'CLOSE-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL))
                                 (+ (SEND SELF :SEND-PANE 'ZMAIL-BUFFER-MENU
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'TEMP-ZMAIL-BUFFER-MENU
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'UNIVERSE-MENU
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL))
                                 (SEND SELF :SEND-PANE 'NAME-BUTTON
                                       :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                       NIL NIL :HORIZONTAL)
                                 (+ (SEND SELF :SEND-PANE 'DONE-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)
                                    (SEND SELF :SEND-PANE 'ABORT-BUTTON
                                          :PANE-SIZE MAX-WIDTH MAX-HEIGHT
                                          NIL NIL :HORIZONTAL)))
                            15.)
                         10.)))
             (HEI (MIN MAX-HEIGHT
                       (TRUNCATE
                         (* (+ (SEND SELF :SEND-PANE 'UNION-BUTTON
                                     :PANE-SIZE MAX-WIDTH MAX-HEIGHT NIL NIL
                                     :VERTICAL)
                               (SEND SELF :MENUS-SIZE MAX-WIDTH MAX-HEIGHT NIL NIL
                                     :VERTICAL)
                               (SEND SELF :SEND-PANE 'NAME-BUTTON
                                     :PANE-SIZE MAX-WIDTH MAX-HEIGHT NIL NIL
                                     :VERTICAL)
                               (SEND SELF :EDITOR-SIZE MAX-WIDTH MAX-HEIGHT NIL NIL
                                     :VERTICAL)
                               (SEND SELF :SEND-PANE 'DONE-BUTTON
                                     :PANE-SIZE MAX-WIDTH MAX-HEIGHT NIL NIL
                                     :VERTICAL))
                            12.)
                         10.))))
         (IF (AND (= WID (TV:SHEET-INSIDE-WIDTH))
                  (= HEI (TV:SHEET-INSIDE-HEIGHT)))
             (SEND SELF :SET-CONFIGURATION 'ONLY)
           (SEND SELF :SET-INSIDE-SIZE WID HEI)))))

;;; Cosmogony
(DEFUN DEFINE-NEW-UNIVERSE (&OPTIONAL (NEAR-MODE '(:MOUSE)))
  "Ask user to define a universe.
Switches to the universe definition frame and back out.
Returns a map function and argument, for use in filtering,
 and the universe name.
Also, the fourth value is the universe itself."
  (PKG-BIND (SYMBOL-PACKAGE 'FOO)
    (SEND *UNIVERSE-DEFINITION-FRAME* :INITIALIZE)
    (TV:WITH-SELECTION-SUBSTITUTE (*UNIVERSE-DEFINITION-FRAME* *ZMAIL-WINDOW*)
      (LET (UNIVERSE)
        (AND (SETQ UNIVERSE (SEND *UNIVERSE-DEFINITION-FRAME* :GET-UNIVERSE NEAR-MODE))
             (VALUES
               'MAP-OVER-DEFINED-UNIVERSE UNIVERSE (STRING UNIVERSE) UNIVERSE))))))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :INITIALIZE) (&AUX NEW-NAME)
  (SETQ NEW-NAME (GENERATE-UNIQUE-NAME *UNIVERSE-LIST*))
  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
  (UNLESS (EQ *MAJOR-MODE* 'LISP-MODE)
    (COM-LISP-MODE))
  (LET ((*BATCH-UNDO-SAVE* T))
    (DELETE-INTERVAL *INTERVAL*)
    (INSERT-FORM-INTO-WINDOW
      `(DEFINE-UNIVERSE ,(INTERN (STRING-UPCASE NEW-NAME)) ()) -1))
  (SETF (WINDOW-BASE-TICK *EDITOR-WINDOW*) (TICK))
  (DISCARD-UNDO-INFORMATION *INTERVAL*)
  (MUST-REDISPLAY *WINDOW* DIS-ALL)
  (MULTIPLE-VALUE-BIND (ZMAIL-BUFFER-ALIST TEMP-ZMAIL-BUFFER-ALIST)
      (GET-ZMAIL-BUFFER-ALISTS T)
    (SEND SELF :COMPUTE-GEOMETRY NEW-NAME
          (NCONC ZMAIL-BUFFER-ALIST
                 '(("Primary" :VALUE PRIMARY :FONT FONTS:HL12BI
                    :DOCUMENTATION "The primary mail file buffer.")))
          (NCONC TEMP-ZMAIL-BUFFER-ALIST
                 '(("Current" :VALUE CURRENT :FONT FONTS:HL12BI
                    :DOCUMENTATION "The current buffer.")))
          (APPEND *UNIVERSE-LIST*
                  '(("All" :VALUE ALL :FONT FONTS:HL12BI
                     :DOCUMENTATION "All messages in Zmail.")))))
  (SEND *MINI-BUFFER-WINDOW* :DEACTIVATE)
  (SEND SELF :TURN-OFF-ACCENTS))

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :GET-UNIVERSE) (&OPTIONAL (NEAR-MODE '(:MOUSE)))
  ;; Some domain specific knowledge here
  (AND (EQ (CAR NEAR-MODE) :WINDOW)
       (LET ((MIN-BOTTOM (+ (TV:SHEET-INSIDE-TOP TV:SUPERIOR) TV:HEIGHT))
             (BROTHER (CADR NEAR-MODE)))
         (AND (< (TV:SHEET-Y-OFFSET BROTHER) MIN-BOTTOM)        ;If we won't fit on top
              ( MIN-BOTTOM (- (TV:SHEET-INSIDE-BOTTOM TV:SUPERIOR)     ;and moving will help
                               (TV:SHEET-HEIGHT BROTHER)))
              (SEND BROTHER :SET-POSITION (TV:SHEET-X-OFFSET BROTHER) MIN-BOTTOM))))
  (TV:EXPOSE-WINDOW-NEAR SELF NEAR-MODE)
  (SEND SELF :COMMAND-LOOP))

;;; Here are the methods that :COMMAND-LOOP requires us to define.

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :TOP-LEVEL-TAG) ()
  'EXIT-UNIVERSE-DEFINITION)

(DEFMETHOD (UNIVERSE-DEFINITION-FRAME :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY 'ZMAIL-UNIVERSE-COMMAND-LIST ARGS))

(DEFSELECT (ZMAIL-UNIVERSE-COMMAND-LIST ZMAIL-COMMAND-LIST-DEFAULT)
  (SELECT-WINDOW (WINDOW)                       ;Moused a window, edit there
    (TV:WITH-SELECTION-SUBSTITUTE (WINDOW SELF)
      (LET ((*COMTAB* *STANDALONE-COMTAB*)
            (*MODE-LINE-LIST* '("ZMail " "Editing Filter")))
        (*CATCH 'ABORT-STANDALONE-EDIT
          (SEND *WINDOW* :EDIT))))
    DIS-NONE)
  (:MENU (ITEM IGNORE WINDOW &AUX WINDOW-NAME)
   (SETQ WINDOW-NAME (SEND SELF :PANE-NAME WINDOW))
   (SETQ ITEM (SEND WINDOW :EXECUTE-NO-SIDE-EFFECTS ITEM))
   (IF (EQ WINDOW-NAME 'UNIVERSE-MENU)
       (SETQ ITEM `(,ITEM))
     (AND (TYPEP ITEM 'ZMAIL-BUFFER)
          (SETQ ITEM (ZMAIL-BUFFER-NAME ITEM))))
   (INSERT-FORM-INTO-WINDOW ITEM)
   DIS-NONE)
  (:MOUSE-BUTTON (CH WINDOW IGNORE IGNORE &AUX WINDOW-NAME)
    (COND ((SEND WINDOW :OPERATION-HANDLED-P :SET-ACCENT)
           (SETQ WINDOW-NAME (SEND SELF :PANE-NAME WINDOW))
           (UNWIND-PROTECT
               (CASE WINDOW-NAME
                 (ABORT-BUTTON
                  (*THROW 'EXIT-UNIVERSE-DEFINITION NIL))
                 (DONE-BUTTON
                  (*THROW 'EXIT-UNIVERSE-DEFINITION (EVAL (READ-FROM-EDITOR-WINDOW))))
                 (NAME-BUTTON
                  (READ-NEW-NAME WINDOW CH *UNIVERSE-LIST* 'GET-UNIVERSE-DEFINITION))
                 (CLOSE-BUTTON
                  (EDITOR-WINDOW-CLOSE-BUTTON))
                 (OTHERWISE
                  (INSERT-FORM-INTO-WINDOW `(,(CASE WINDOW-NAME
                                                (NOT-BUTTON ')
                                                (UNION-BUTTON ')
                                                (INTERSECTION-BUTTON ')))
                                           -1)))
             (SEND WINDOW :SET-ACCENT NIL)))
          (T NIL))                              ;random window
   DIS-NONE)
  )

;;; Filter definition components
(DEFUN MACRO-EXPAND-SEARCH-KEY (KEY &AUX FUNCTION (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  (DECLARE (RETURN-LIST FUNCTION KEY))
  (COND ((STRINGP KEY)
         (SETQ FUNCTION 'SEARCH))
        ((EQ (ARRAY-TYPE KEY) 'ART-16B)
         (MULTIPLE-VALUE (FUNCTION KEY)
           (PARSE-EXTENDED-SEARCH-16B-STRING KEY)))
        (T
         (ZMAIL-ERROR "~S not a valid search key" KEY)))
  (VALUES FUNCTION KEY))

(DEFMACRO SEARCH-WITHIN-MSG (KEY)
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (MACRO-EXPAND-SEARCH-KEY KEY)
    `(,FUNCTION (MSG-START-BP MSG) ',KEY NIL NIL NIL (MSG-END-BP MSG))))

(DEFMACRO MSG-HEADER-SEARCH (TYPE KEY)
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (MACRO-EXPAND-SEARCH-KEY KEY)
    (SETQ FUNCTION (CASE FUNCTION
                     (SEARCH 'STRING-SEARCH)
                     (FSM-SEARCH 'FSM-STRING-SEARCH)))
    `(LET ((.HEADER. (GET STATUS ,TYPE)))
       (AND .HEADER.
            (IF (CONSP .HEADER.)
                (LOOP FOR .STRING. IN .HEADER.
                      THEREIS (,FUNCTION ',KEY .STRING.))
              (,FUNCTION ',KEY .HEADER.))))))

(DEFMACRO MSG-HEADER-RECIPIENT-SEARCH (TYPE KEY)
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (MACRO-EXPAND-SEARCH-KEY KEY)
    (SETQ FUNCTION (COND ((EQ FUNCTION 'FSM-SEARCH)
                          'MSG-HEADER-RECIPIENT-FSM-SEARCH)
                         ((AND (= (AREF KEY 0) #/)
                               (= (AREF KEY (1- (STRING-LENGTH KEY))) #/))
                          (SETQ KEY (SUBSTRING KEY 1 (1- (STRING-LENGTH KEY))))
                          'MSG-HEADER-RECIPIENT-PARTIAL-MATCH)
                         (T
                          'MSG-HEADER-RECIPIENT-MATCH)))
    (IF (AND (CONSP TYPE) (EQ (CAR TYPE) 'QUOTE) (SYMBOLP (CADR TYPE)))
        `(,FUNCTION (GET STATUS ,TYPE) ',KEY)
      `(DO .L. ,TYPE (CDR .L.) (NULL .L.)
           (AND (,FUNCTION (GET STATUS (CAR .L.)) ',KEY)
                (RETURN T))))))

(DEFUN MSG-HEADER-RECIPIENT-FSM-SEARCH (RECIPIENTS KEY &AUX INTERVAL)
  (DOLIST (RECIPIENT RECIPIENTS)
    (AND (SETQ INTERVAL (GET (LOCF RECIPIENT) :INTERVAL))
         (FSM-SEARCH (CAR INTERVAL) KEY NIL NIL NIL (CADR INTERVAL))
         (RETURN T))))

(DEFUN MSG-HEADER-RECIPIENT-PARTIAL-MATCH (RECIPIENTS KEY)
  (DO L RECIPIENTS (CDR L) (NULL L)
    (AND (STRING-SEARCH KEY (GET (LOCF (CAR L)) :NAME))
         (RETURN T))))

(DEFUN MSG-HEADER-RECIPIENT-MATCH (RECIPIENTS KEY &AUX END-1 START-2 PLIST)
  (AND (SETQ END-1 (STRING-SEARCH-CHAR #/@ KEY))
       (SETQ START-2 (1+ END-1)))
  (DOLIST (RECIPIENT RECIPIENTS)
    (SETQ PLIST (LOCF RECIPIENT))
    (AND (STRING-EQUAL (GET PLIST :NAME) KEY :start1 0 :start2 0 :end1 NIL :end2 END-1)
         (OR (NULL START-2) (STRING-EQUAL (CAR (GET PLIST :HOST)) KEY :start1 0 :start2 START-2))
         (RETURN T))))

(DEFMACRO MSG-HEADER-RECIPIENT-EQUAL (TYPE KEY)
  `(LET ((.RECIPIENTS. (GET STATUS ,TYPE)))
     (AND (NULL (CDR .RECIPIENTS.))
          (MSG-HEADER-RECIPIENT-MATCH .RECIPIENTS. ',KEY))))

(DEFMACRO DEFINE-FILTER (FILTER (MSG) . BODY)
  "Define a FILTER with argument MSG to compute BODY.
This function is used in ZMAIL init files."
  (LET ((DOCUMENTATION))
    (IF (STRINGP (CAR BODY))
        (SETQ DOCUMENTATION (CAR BODY)
              BODY (CDR BODY)))
    `(PROGN 'COMPILE
       (DEFINE-FILTER-1 ',FILTER ',DOCUMENTATION)
       (DEFUN (:PROPERTY ,FILTER FILTER-FUNCTION) (,MSG &AUX STATUS KEYWORDS)
         (SETQ STATUS (ASSURE-MSG-PARSED ,MSG)
               KEYWORDS (GET STATUS 'KEYWORDS))
         . ,BODY))))

;;; Add a new filter-name, with optional mouse documentation
(DEFUN DEFINE-FILTER-1 (FILTER DOCUMENTATION)
  (IF DOCUMENTATION (SETQ DOCUMENTATION `(:DOCUMENTATION ,DOCUMENTATION)))
  (LET ((ALIST-ENTRY (ASSQ FILTER *USER-FILTER-ALIST*))
        (ALIST-DATA `(:VALUE ,FILTER ,@DOCUMENTATION)))
    (IF ALIST-ENTRY
        (RPLACD ALIST-ENTRY ALIST-DATA)
      (SETQ *USER-FILTER-ALIST*
            (NCONC *USER-FILTER-ALIST*
                   (NCONS `(,FILTER ,@ALIST-DATA)))))))

(DEFUN GET-FILTER-DEFINITION (FILTER &AUX DEF)
  "Given a filter name, return a DEFINE-FILTER form that could define it."
  (SETQ DEF (GET FILTER 'FILTER-FUNCTION))
  (AND (ATOM DEF)
       (SETQ DEF (GET FILTER 'EXPR-FILTER-FUNCTION)))
  (OR (AND DEF (EQ (CAR DEF) 'NAMED-LAMBDA)
           (NOT (ATOM (CADR DEF)))
           (EQ (CAR (CAADR DEF)) :PROPERTY)
           (EQ (CADR (CAADR DEF)) FILTER))
      (BARF "~A is compiled" FILTER))
  `(DEFINE-FILTER ,FILTER (,(CAADDR DEF))
     . ,(CDDDDR DEF)))

(DEFUN MACRO-EXPAND-DATE (DATE)
  (OR (STRINGP DATE)
      (ZMAIL-ERROR "~S is not a valid date" DATE))
  (TIME:PARSE-UNIVERSAL-TIME DATE))


;; Currently the "NOW" argument to MACRO-EXPAND-RELATIVE-DATE is useless.
;; However, if anything special is to be done with things like "A week after
;; my birthday" or "January", besides forcing them to be absolute, the current
;; date is required.

(DEFUN MACRO-EXPAND-RELATIVE-DATE (DATE NOW &REST OTHERS
                                   &AUX (DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
                                   RELATIVE-P)
  (MULTIPLE-VALUE (DATE RELATIVE-P)
    (MACRO-EXPAND-DATE DATE))
  (SETQ OTHERS (COPYLIST OTHERS))
  (IF (EQ RELATIVE-P :RELATIVE)
      `(- (TIME:GET-UNIVERSAL-TIME)
          ,(- (TIME:GET-UNIVERSAL-TIME) DATE)   ;Relative:  Compute offset from now
          ,@OTHERS)
    `(- (TIME:GET-UNIVERSAL-TIME)
        ,(- (MACRO-EXPAND-DATE NOW) DATE)       ;Absolute:  Compute offset from then
        ,@OTHERS)))


(DEFMACRO MSG-SAME-DATE (DATE)
  (SETQ DATE (MACRO-EXPAND-DATE DATE))
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          ( ',DATE .DATE.)
          (> ',(LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
                 (+ DATE 86400.))
             .DATE.))))

(DEFMACRO MSG-SAME-RELATIVE-DATE (DATE NOW)
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          ( ,(MACRO-EXPAND-RELATIVE-DATE DATE NOW) .DATE.)
          (> ,(MACRO-EXPAND-RELATIVE-DATE DATE NOW -86400.) .DATE.))))

(DEFMACRO MSG-DATE-LESSP (DATE)
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          (< .DATE. ',(MACRO-EXPAND-DATE DATE)))))

(DEFMACRO MSG-RELATIVE-DATE-LESSP (DATE NOW)
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          (< .DATE. ,(MACRO-EXPAND-RELATIVE-DATE DATE NOW)))))

(DEFMACRO MSG-DATE-GREATERP (DATE)
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          ( .DATE. ',(MACRO-EXPAND-DATE DATE)))))

(DEFMACRO MSG-RELATIVE-DATE-GREATERP (DATE NOW)
  `(LET ((.DATE. (GET STATUS :DATE)))
     (AND (NOT (NULL .DATE.))
          ( .DATE. ,(MACRO-EXPAND-RELATIVE-DATE DATE NOW)))))

;;;; Defining universes in the init file, and writing definitions there.

(DEFMACRO DEFINE-UNIVERSE (UNIVERSE IGNORE EXPANSION)
  (CHECK-EXPANSION EXPANSION)
  `(PROGN
     (OR (MEMQ ',UNIVERSE *UNIVERSE-LIST*)
         (SETQ *UNIVERSE-LIST* (NCONC *UNIVERSE-LIST* (NCONS ',UNIVERSE))))
     (DEFPROP ,UNIVERSE ,EXPANSION UNIVERSE)))

(DEFUN GET-UNIVERSE-DEFINITION (UNIVERSE)
  `(DEFINE-UNIVERSE ,UNIVERSE ()
     ,(GET UNIVERSE 'UNIVERSE)))

(DEFUN CHECK-EXPANSION (EXPANSION)
  (COND ((STRINGP EXPANSION))
        ((PATHNAMEP EXPANSION))
        ((NULL EXPANSION))
        ((MEMQ EXPANSION '(PRIMARY CURRENT)))
        ((AND (SYMBOLP EXPANSION) (GET EXPANSION 'UNIVERSE)))
        ((ATOM EXPANSION)
         (ZMAIL-ERROR "~S is not a valid universe component" EXPANSION))
        ((NULL (CDR EXPANSION))
         (OR (SYMBOLP (CAR EXPANSION))
             (ZMAIL-ERROR "~S is not a valid universe component" EXPANSION)))
        ((EQ (CAR EXPANSION) ')
         (OR (= (LENGTH EXPANSION) 2)
             (ZMAIL-ERROR "~S wrong number of argument to " EXPANSION)))
        ((NOT (MEMQ (CAR EXPANSION) '( )))
         (ZMAIL-ERROR "~S is not a known set operator" (CAR EXPANSION)))
        (T
         (DOLIST (EXP (CDR EXPANSION))
           (CHECK-EXPANSION EXP)))))

;;;; Implementation of mapping over a universe.

(DEFUN (MAP-OVER-DEFINED-UNIVERSE MAP-FUNCTION-BUFFER-NAME-FUNCTION) (UNIVERSE)
  (STRING-APPEND "" UNIVERSE ""))

(DEFUN MAP-OVER-DEFINED-UNIVERSE (UNIVERSE FILTER-FUNCTION FILTER-ARG PROCESSING-FUNCTION
                                  PROCESSING-ARG)
  (SETQ UNIVERSE (EXPAND-UNIVERSE UNIVERSE))
  (DOMSGS (MSG UNIVERSE)
    (AND (FUNCALL FILTER-FUNCTION MSG FILTER-ARG)
         (FUNCALL PROCESSING-FUNCTION MSG PROCESSING-ARG))))

;;; This takes a universe and returns an array with the appropriate messages in it.
(DEFUN EXPAND-UNIVERSE (UNIVERSE)
  (COND ((NULL UNIVERSE) NIL)
        ((SYMBOLP UNIVERSE)
         (CASE UNIVERSE
           (PRIMARY *PRIMARY-ZMAIL-BUFFER*)
           (CURRENT *ZMAIL-BUFFER*)
           (ALL (EXPAND-UNIVERSE-INTERSECTION NIL))
           (OTHERWISE (EXPAND-UNIVERSE (GET UNIVERSE 'UNIVERSE)))))
        ((STRINGP UNIVERSE)
         (GET-ZMAIL-BUFFER-FROM-NAME UNIVERSE T))
        ((EQ (CAR UNIVERSE) ')
         (EXPAND-UNIVERSE-NOT (EXPAND-UNIVERSE (CADR UNIVERSE))))
        ((EQ (CAR UNIVERSE) ')
         (EXPAND-UNIVERSE-UNION (MAPCAR 'EXPAND-UNIVERSE (CDR UNIVERSE))))
        ((EQ (CAR UNIVERSE) ')
         (EXPAND-UNIVERSE-INTERSECTION (MAPCAR 'EXPAND-UNIVERSE (CDR UNIVERSE))))
        ((NULL (CDR UNIVERSE))
         (EXPAND-UNIVERSE (GET (CAR UNIVERSE) 'UNIVERSE)))
        (T
         (ZMAIL-ERROR "~S is not a valid universe" UNIVERSE))))

(DEFUN EXPAND-UNIVERSE-NOT (ZMAIL-BUFFER &AUX NEW-ZMAIL-BUFFER ARRAY)
  (SETQ NEW-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER)
        ARRAY (ZMAIL-BUFFER-ARRAY NEW-ZMAIL-BUFFER))
  (DOLIST (MF *ZMAIL-BUFFER-LIST*)
    (AND (ZMAIL-BUFFER-DISK-P MF)
         (DOMSGS (MSG MF)
           (OR (MSG-IN-ZMAIL-BUFFER-P MSG ZMAIL-BUFFER)
               (VECTOR-PUSH-EXTEND MSG ARRAY)))))
  NEW-ZMAIL-BUFFER)

(DEFUN EXPAND-UNIVERSE-UNION (ZMAIL-BUFFERS &AUX NEW-ZMAIL-BUFFER ARRAY)
  ;; Move the larger buffers to the start of the list for speed
  (SETQ ZMAIL-BUFFERS (SORT ZMAIL-BUFFERS
                            #'(LAMBDA (MF1 MF2)
                                (> (ZMAIL-BUFFER-NMSGS MF1) (ZMAIL-BUFFER-NMSGS MF2))))
        NEW-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER)
        ARRAY (ZMAIL-BUFFER-ARRAY NEW-ZMAIL-BUFFER))
  (DOLIST (MF ZMAIL-BUFFERS)
    (DOMSGS (MSG MF)
            (OR (MSG-IN-ZMAIL-BUFFER-P MSG NEW-ZMAIL-BUFFER)
                (VECTOR-PUSH-EXTEND MSG ARRAY))))
  NEW-ZMAIL-BUFFER)

(DEFUN EXPAND-UNIVERSE-INTERSECTION (ZMAIL-BUFFERS &AUX NEW-ZMAIL-BUFFER ZMAIL-BUFFER ARRAY)
  (IF (NULL ZMAIL-BUFFERS)                      ;Intersection of no args is everything
      (EXPAND-UNIVERSE-UNION *ZMAIL-BUFFER-LIST*)
    ;; Move the smaller buffers to the start of the list for speed
    (SETQ ZMAIL-BUFFERS (SORT ZMAIL-BUFFERS
                              #'(LAMBDA (MF1 MF2)
                                  (< (ZMAIL-BUFFER-NMSGS MF1) (ZMAIL-BUFFER-NMSGS MF2))))
          NEW-ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER)
          ARRAY (ZMAIL-BUFFER-ARRAY NEW-ZMAIL-BUFFER))
    (POP ZMAIL-BUFFERS ZMAIL-BUFFER)
    (DOMSGS (MSG ZMAIL-BUFFER)
            (OR (DOLIST (MF ZMAIL-BUFFERS)
                  (OR (MSG-IN-ZMAIL-BUFFER-P MSG MF)
                      (RETURN T)))
                (VECTOR-PUSH-EXTEND MSG ARRAY)))
    NEW-ZMAIL-BUFFER))

;;;; Implementation of the "experimental" window configuration.

;;;Execute a command - a blip read from the input stream -
;;;using a universe or filter or both, which will be specified by other blips.
;;;The blip specifying the command comes last and is of type :MENU.
;;;The blips specifying the universe and/or filter are assumed to be coming
;;;from the universe and filter "button" windows.

;;;It is quite possible that, when this function is entered,
;;;the last blip is not yet read in, and we will wait for the user to click it.
(DEFUN COMMAND-WITH-UNIVERSE-OR-FILTER (&AUX BUTTON-FRAME UNIVERSE-BUTTON FILTER-BUTTON)
  (SETQ BUTTON-FRAME (SEND *ZMAIL-WINDOW* :GET-PANE 'BUTTONS-FRAME)
        UNIVERSE-BUTTON (SEND BUTTON-FRAME :GET-PANE 'UNIVERSE-BUTTON)
        FILTER-BUTTON (SEND BUTTON-FRAME :GET-PANE 'FILTER-BUTTON))
  (UNWIND-PROTECT
    (DO ((FILTER-FUNCTION 'MSG-TRUE-FILTER)
         (FILTER-ARG NIL)
         ;; Assume by default that there is no universe, no filter.
         (MAP-FUNCTION 'MAP-OVER-SINGLE-MSG)
         (MAP-ARG *MSG*)
         (CHAR))
        (NIL)
      ;; Read the next blip.
      (SETQ CHAR (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
                   (SEND *STANDARD-INPUT* :ANY-TYI)))
      (SETQ *LAST-COMMAND-CHAR* CHAR)
      (COND ((AND (CONSP CHAR)
                  (EQ (FIRST CHAR) :MENU))
             ;; If the blip is a command, execute it
             ;; using universe and filter already specified.
             (LET* ((COMMAND (SEND (FOURTH CHAR) :EXECUTE-NO-SIDE-EFFECTS (SECOND CHAR)))
                    (ALL-COMMAND (GET COMMAND 'ASSOCIATED-ALL-COMMAND))
                    (MAP-COMMAND (GET COMMAND 'ASSOCIATED-MAP-COMMAND)))
               ;; Record which button was typed to invoke command we will now do.
               (SET-COMMAND-BUTTON (THIRD CHAR))
               (DO () ((NEQ COMMAND 'COM-ZMAIL-OTHER-COMMANDS))
                 (SETQ COMMAND (CHOOSE-OTHER-COMMAND)))
               ;; How to execute the command depends on whether the command
               ;; provides a MAP-FUNCTION or an ALL-FUNCTION, and what universe/filter.
               ;; The MAP-FUNCTION does mapping itself; the ALL-FUNCTION can only
               ;; operate on an entire buffer.
               (RETURN (COND ((EQ MAP-FUNCTION 'MAP-OVER-SINGLE-MSG)
                              (FUNCALL COMMAND))
                             ((AND ALL-COMMAND
                                   (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
                                   (EQ MAP-ARG *ZMAIL-BUFFER*)
                                   (EQ FILTER-FUNCTION 'MSG-TRUE-FILTER))
                              (FUNCALL ALL-COMMAND))
                             ((AND ALL-COMMAND
                                   (EQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
                                   (TYPEP MAP-ARG 'ZMAIL-BUFFER)
                                   (EQ FILTER-FUNCTION 'MSG-TRUE-FILTER))
                              (LET ((*ZMAIL-BUFFER* MAP-ARG)
                                    (*MSG* :NO-SELECT))
                                (FUNCALL ALL-COMMAND)))
                             ((AND (NULL MAP-COMMAND)
                                   ALL-COMMAND)
                              (LET ((*ZMAIL-BUFFER*
                                      (MAKE-ZMAIL-BUFFER-FROM-FILTER
                                        MAP-FUNCTION MAP-ARG
                                        FILTER-FUNCTION FILTER-ARG))
                                    (*MSG* :NO-SELECT))
                                (FUNCALL ALL-COMMAND))
                              (ZMAIL-SELECT-MSG *MSG* NIL NIL))
                             (MAP-COMMAND
                              (FUNCALL MAP-COMMAND MAP-FUNCTION MAP-ARG
                                       FILTER-FUNCTION FILTER-ARG))
                             (T
                              (BARF "That command does not take a filter argument"))))))
            ;; If the blip is a :MOUSE-BUTTON blip, it contains data from the
            ;; universe or filter button.  Look at them to specify the
            ((AND (CONSP CHAR)
                  (EQ (FIRST CHAR) :MOUSE-BUTTON))
             (SET-COMMAND-BUTTON (SECOND CHAR))
             (LET ((WINDOW (THIRD CHAR)))
               (COND ((SEND WINDOW :OPERATION-HANDLED-P :SET-ACCENT)
                      (UNWIND-PROTECT
                          (*CATCH 'ZWEI-COMMAND-LOOP
                            (COND ((EQ WINDOW UNIVERSE-BUTTON)
                                   (MULTIPLE-VALUE (MAP-FUNCTION MAP-ARG)
                                     (GET-UNIVERSE-OR-FILTER-FOR-COMMAND
                                       'GET-UNIVERSE-FUNCTION-FOR-COMMAND WINDOW BUTTON-FRAME
                                       '*LAST-COMMAND-UNIVERSE-FUNCTION*
                                       '*LAST-COMMAND-UNIVERSE-ARG*
                                       '*LAST-COMMAND-UNIVERSE-NAME*
                                       'MAP-OVER-SINGLE-ZMAIL-BUFFER *ZMAIL-BUFFER*
                                       (AND *ZMAIL-BUFFER*
                                            (ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*)))))
                                  ((EQ WINDOW FILTER-BUTTON)
                                   (MULTIPLE-VALUE (FILTER-FUNCTION FILTER-ARG)
                                     (GET-UNIVERSE-OR-FILTER-FOR-COMMAND
                                       'GET-FILTER-FUNCTION-FOR-COMMAND WINDOW BUTTON-FRAME
                                       '*LAST-COMMAND-FILTER-FUNCTION*
                                       '*LAST-COMMAND-FILTER-ARG*
                                       '*LAST-COMMAND-FILTER-NAME*
                                       NIL NIL NIL))
                                   (COND ((EQ MAP-FUNCTION 'MAP-OVER-SINGLE-MSG)
                                          (SETQ MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER
                                                MAP-ARG *ZMAIL-BUFFER*)
                                          (SEND BUTTON-FRAME :CHANGE-BUTTONS UNIVERSE-BUTTON
                                                (ZMAIL-BUFFER-NAME *ZMAIL-BUFFER*)))))
                                  (T (ZMAIL-ERROR "~S is not a known window" WINDOW))))
                        (SEND WINDOW :SET-ACCENT NIL)))
                     (T NIL))))                 ;random window
            (T
             (SEND *STANDARD-INPUT* :UNTYI CHAR)
             (RETURN NIL))))
    (SEND BUTTON-FRAME :CHANGE-BUTTONS UNIVERSE-BUTTON "Just current message"
          FILTER-BUTTON "All")))

;;;This function allows a command to either use its own default filter or universe
;;;or get a filter or universe from a menu and set the command's default.
;;;Which one depends on the value of *ZMAIL-COMMAND-BUTTON*; that is,
;;;which button the user clicked on to invoke the command this time.
;;;Args are: the function to read the universe/filter using a menu,
;;; the "button" window for universes of filters (so its default can be set),
;;; the "button" window's superior window,
;;; three symbols which are variables describing the default for the left button
;;;  (as three separate arguments),
;;; and three values which describe the default for the middle button.
(DEFUN GET-UNIVERSE-OR-FILTER-FOR-COMMAND (FUNCTION WINDOW SUPERIOR
                                           FUNVAR ARGVAR NAMVAR MIDFUN MIDARG MIDNAM
                                           &AUX FV AV NAME)
  (COND ((EQ *ZMAIL-COMMAND-BUTTON* :LEFT)
         ;; Left button => use values of the variables which contain the default.
         (SETQ FV (OR (SYMEVAL FUNVAR) (BARF "There is no default for this yet."))
               AV (SYMEVAL ARGVAR)
               NAME (SYMEVAL NAMVAR)))
        (T
         ;; Otherwise, set the default either from middle-button values
         ;; or by reading it in using FUNCTION.
         (IF (NEQ *ZMAIL-COMMAND-BUTTON* :MIDDLE)
             (MULTIPLE-VALUE (FV AV NAME)
               (FUNCALL FUNCTION))
           (OR MIDFUN (BARF))
           (SETQ FV MIDFUN AV MIDARG NAME MIDNAM))
         (COND (FV
                (SET FUNVAR FV)
                (SET ARGVAR AV)
                (SET NAMVAR NAME)
                ;; Update documentation for the universe or filter button,
                ;; about defaults for clicking on those buttons,
                (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION FUNCTION)))))
  ;; Update what appears in the universe or filter button
  ;; to show you what universe or filter is being used in this command.
  ;; It will get changed back at the end of the command.
  (IF FV
      (SEND SUPERIOR :CHANGE-BUTTONS WINDOW NAME))
  (VALUES FV AV))

(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-UNIVERSE-FUNCTION* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-UNIVERSE-ARG* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-UNIVERSE-NAME* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION GET-UNIVERSE-FUNCTION-FOR-COMMAND
                                       *UNIVERSE-BUTTON-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER GET-UNIVERSE-FUNCTION-FOR-COMMAND (STRING)
  (FORMAT STRING
          "Change universe for next command: ~@[L: ~A; ~]M: Current buffer; R: menu."
          *LAST-COMMAND-UNIVERSE-NAME*))

(DEFF GET-UNIVERSE-FUNCTION-FOR-COMMAND 'GET-UNIVERSE-FUNCTION)

(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-FILTER-FUNCTION* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-FILTER-ARG* NIL)
(DEFINE-ZMAIL-GLOBAL *LAST-COMMAND-FILTER-NAME* NIL)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION GET-FILTER-FUNCTION-FOR-COMMAND
                                       *FILTER-BUTTON-DOCUMENTATION*)

(DEFINE-COMMAND-WHO-LINE-DOCUMENTATION-UPDATER GET-FILTER-FUNCTION-FOR-COMMAND (STRING)
  (FORMAT STRING "Change filter for next command: ~@[L: ~A; ~]R: menu."
          *LAST-COMMAND-FILTER-NAME*))

(DEFUN GET-FILTER-FUNCTION-FOR-COMMAND (&AUX FUN ARG)
  (MULTIPLE-VALUE (NIL NIL FUN ARG)
    (GET-FILTER-FUNCTION-1 NIL NIL NIL '(:MOUSE)))
  (VALUES FUN ARG (FILTER-FUNCTION-BUFFER-NAME FUN ARG)))

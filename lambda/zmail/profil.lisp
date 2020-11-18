;;; Lisp Machine mail reader -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Profile mode and commands, definition are in DEFS
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFFLAVOR ZMAIL-PROFILE-BUFFER
           (;; Tick at which editor text was last updated by ZMAIL from actual values.
            (EDITOR-VARIABLE-TICK *TICK*)
            ;; Tick at which variable values were last modified in core.
            (VARIABLE-TICK *TICK*)
            ;; Tick at which we last compiled the init file, or NIL if wasn't compiled.
            ;; May get set to -1 to say that it was compiled but QFASL is out of date.
            (SAVED-PACKAGE NIL)
            (PROFILE-COMPILE-TICK *TICK*))
           (FILE-BUFFER)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:ACCESSOR-PREFIX PROFILE-BUFFER-)
  (:DOCUMENTATION "The kind of buffer that lives in *PROFILE-EDITOR-WINDOW*"))

(DEFMETHOD (ZMAIL-PROFILE-BUFFER :SECTIONIZE) ()
  (SECTIONIZE-FILE-BUFFER SELF NIL 'ZMAIL-PROFILE-BUFFERS))

(DEFMETHOD (ZMAIL-PROFILE-BUFFER :RESECTIONIZE) (&REST IGNORE)
  (RESECTIONIZE-FILE-BUFFER SELF NIL 'ZMAIL-PROFILE-BUFFERS))

(DEFMETHOD (ZMAIL-PROFILE-BUFFER :RENAME) (NEW-NAME)
  (SETQ NAME NEW-NAME)
  (IF (EQ SELF *INTERVAL*)
      (SETQ *ZMAIL-INTERVAL-NAME* NEW-NAME)))

(DEFMETHOD (ZMAIL-PROFILE-BUFFER :AFTER :INIT) (IGNORE)
  (SETQ NAME "Profile"))

;; This flavor is used for *PROFILE-WINDOW*, a frame which
;; includes the CVV window and the buttons, but not the *PROFILE-EDITOR-WINDOW*.
(DEFFLAVOR ZMAIL-PROFILE-FRAME
        ()
        (ZMAIL-UTILITY-FRAME)
  (:DEFAULT-INIT-PLIST :EDITOR-CLOSURE-VARIABLES
                       ZMAIL-PROFILE-FRAME-EDITOR-CLOSURE-VARIABLES))

(DEFCONST ZMAIL-PROFILE-FRAME-EDITOR-CLOSURE-VARIABLES
          (MERGE-CLOSURE-VARIABLE-LISTS
            '((*MODE-LINE-LIST* '("ZMail " "Profile"))
              (*MAJOR-MODE* NIL)
              (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
              (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
              (*COMTAB* *MSG-COMTAB*)
              (*SELECTABLE-MODE-LINE-ELEMENTS* NIL))
            TOP-LEVEL-EDITOR-CLOSURE-VARIABLES))

(DEFMETHOD (ZMAIL-PROFILE-FRAME :BEFORE :INIT) (IGNORE)
  (SETQ TV:PANES `((FILTERS-BUTTON TV:BUTTON-PANE :NAME "Filters" :DOCUMENTATION
  "L: Menu of filters saved in init file;  M: Filter associations;  R: menu.")
                   (UNIVERSES-BUTTON TV:BUTTON-PANE :NAME "Universes" :DOCUMENTATION
  "L: Menu of universes saved in init file;  M: Universe-Filter associations;  R: menu.")
                   (MAIL-FILES-BUTTON TV:BUTTON-PANE :NAME "Mail files" :DOCUMENTATION
  "L: Menu of non-primary disk mail files saved in init file;  M: Move associations;  R: menu."
                    )
                   (KEYWORDS-BUTTON TV:BUTTON-PANE :NAME "Keywords" :DOCUMENTATION
  "L: Edit mail file-keyword associations;  M: Keywords-Filter associations;  R: menu.")
                   (HARDCOPY-BUTTON TV:BUTTON-PANE :NAME "Hardcopy" :DOCUMENTATION
  "Give choose variable values window for hardcopy user options.")
                   (FILE-OPTIONS-BUTTON TV:BUTTON-PANE :NAME "File options" :DOCUMENTATION
  "Give menu for mail file and alter the attributes of that mail file.")
                   (CHOOSE-WINDOW ZMAIL-CHOOSE-VARIABLE-VALUES-PANE
                                  :STACK-GROUP ,SYS:%CURRENT-STACK-GROUP
                                  :LABEL "User options:")
                   (DONE-BUTTON TV:BUTTON-PANE :NAME "Exit" :DOCUMENTATION
                                "Return to main command level.")
                   (RESET-BUTTON TV:BUTTON-PANE :NAME "Reset" :DOCUMENTATION
                                 "Set all options back to values in init file.")
                   (DEFAULTS-BUTTON TV:BUTTON-PANE :NAME "Defaults" :DOCUMENTATION
                                    "Set all options back to normal system default values.")
                   (SAVE-BUTTON TV:BUTTON-PANE :NAME "Save"
                    :DOCUMENTATION "L: Save file;  M: Make init file compiled;  R: menu.")
                   (EDIT-BUTTON TV:BUTTON-PANE :NAME "Edit" :DOCUMENTATION
                                "Edit init file buffer."))

        TV:CONSTRAINTS
          `((ONLY . ( (WHOLE-THING)
                     ((WHOLE-THING :HORIZONTAL (:EVEN)
                       (WHOLE)
                       ((WHOLE TV:WHITE-INCLUDE-WHITESPACE      ;Vert
                         (1.0) (:EVEN)
                         (EXTENDED CHOOSE CONTROLS)
                         ((EXTENDED TV:FLOATING-BUTTONS
                           (FILTERS-BUTTON UNIVERSES-BUTTON MAIL-FILES-BUTTON
                            FILE-OPTIONS-BUTTON KEYWORDS-BUTTON HARDCOPY-BUTTON)))
                         ((CONTROLS TV:FLOATING-BUTTONS
                           (DONE-BUTTON RESET-BUTTON DEFAULTS-BUTTON SAVE-BUTTON
                            EDIT-BUTTON)))
                         ((CHOOSE TV:WHITE-INCLUDE-WHITESPACE   ;Horiz
                           (0.8 :LINES CHOOSE-WINDOW) (:EVEN)
                           (CHOOSE-WINDOW)
                           ((CHOOSE-WINDOW :ASK :PANE-SIZE)))))))))))))

(DEFMETHOD (ZMAIL-PROFILE-FRAME :AFTER :INIT) (IGNORE)
;  (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
;  (INITIALIZE-TOP-LEVEL-EDITOR *PROFILE-EDITOR-WINDOW*)
  (ADD-INITIALIZATION "Reset zmail displayed user options"
                      `(SEND ',(SEND SELF :GET-PANE 'CHOOSE-WINDOW) :SET-VARIABLES
                             (TV:PRUNE-USER-OPTION-ALIST *ZMAIL-USER-OPTION-ALIST*))
                      '(SITE-OPTION)))

(DEFMETHOD (ZMAIL-PROFILE-FRAME :INITIALIZE) ()
;  (UNLESS (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*WINDOW*)
;    (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
;    (INITIALIZE-TOP-LEVEL-EDITOR *PROFILE-EDITOR-WINDOW* T))
  (SEND SELF :SEND-PANE 'CHOOSE-WINDOW :SET-STACK-GROUP SYS:%CURRENT-STACK-GROUP)
  (SEND SELF :TURN-OFF-ACCENTS))

;;; Here we have the command to enter profile mode, and its subroutines.

(DEFVAR *EDITING-PROFILE* :UNBOUND
  "Bound while in the profile frame, T if in the editor window.")

(DEFINE-ZMAIL-TOP-LEVEL-COMMAND COM-ZMAIL-PROFILE
                                "Alter user options and edit ZMail init file."
                                (NO-ZMAIL-BUFFER-OK)
  (SET-ZMAIL-USER)
  (LET ((EDITOR-CLOSURE (SEND *PROFILE-WINDOW* :EDITOR-CLOSURE)))
    (UNLESS (SYMEVAL-IN-CLOSURE EDITOR-CLOSURE '*WINDOW*)
      (SYS:%USING-BINDING-INSTANCES (CLOSURE-BINDINGS EDITOR-CLOSURE))
      (INITIALIZE-TOP-LEVEL-EDITOR *PROFILE-EDITOR-WINDOW* T)
      (set-in-closure editor-closure '*comtab* *standalone-comtab*)))
  (WITH-BACKGROUND-PROCESS-LOCKED
    (WITH-WINDOW-CONFIGURATION (:PROFILE)
      (LET ((*EDITING-PROFILE* NIL)
            (*ZMACS-BUFFER-VERSION-STRING* NIL)
            (*ZMAIL-INTERVAL-NAME* "")
            (*MODE-LINE-LIST*
              `("ZMail " (*EDITING-PROFILE* "Editing ") "Profile "
                *ZMAIL-INTERVAL-NAME* *ZMACS-BUFFER-VERSION-STRING*
                *MORE-ABOVE-BELOW*
                (*EDITING-PROFILE* ,(FORMAT NIL "     ~:@C ends." #\End ))))
            (*PACKAGE* (SYMBOL-PACKAGE 'FOO)))
        ;; Read in the init file if not up to date.
        (VISIT-ZMAIL-PROFILE (WINDOW-INTERVAL *PROFILE-EDITOR-WINDOW*) *PACKAGE*)
        (SETQ *ZMAIL-INTERVAL-NAME*
              (BUFFER-NAME (WINDOW-INTERVAL *PROFILE-EDITOR-WINDOW*)))
        (SEND *PROFILE-WINDOW* :COMMAND-LOOP))))
  DIS-NONE)

;; Used only from preceding function.
(DEFUN VISIT-ZMAIL-PROFILE (*INTERVAL* PKG &AUX FILE-ID PATHNAME)
  (SET-ZMAIL-USER)
  (SETQ FILE-ID (BUFFER-FILE-ID *INTERVAL*))
  (IF (NULL FILE-ID)
      (SETQ PATHNAME (ZMAIL-INIT-FILE-PATHNAME))
      (SETQ PATHNAME (BUFFER-PATHNAME *INTERVAL*))
      ;; See if everything is still ok
      (WITH-OPEN-FILE (STREAM PATHNAME '(:PROBE :NOERROR))
        (IF (ERRORP STREAM)
            (AND (NEQ FILE-ID T)
                 (FORMAT *QUERY-IO* "~&Note: file has been deleted on the file computer"))
            (AND (NOT (EQUAL FILE-ID (SEND STREAM :INFO)))
                 (FQUERY '(:SELECT T
                           :BEEP T)
                         "There is a different version of this file on the file computer,~@
                          your version has~:[ not~] been modified.~@
                          Do you want the new version instead? "
                         (BUFFER-needs-saving-P *INTERVAL*))
                 (SETQ FILE-ID NIL)))))
  (UNLESS FILE-ID
    ;; Does not use WITH-OPEN-FILE
    ;; because it may open a new STREAM midway through.
    (LET ((STREAM (OPEN PATHNAME :DIRECTION :INPUT :CHARACTERS :DEFAULT
                        :ERROR NIL))
          ;; Avoid recording the deletion for undo.
          (*BATCH-UNDO-SAVE* T)
          (package-name (package-name pkg))
          (*readtable* si:common-lisp-readtable)
          (readtable-name (second (si:rdtbl-names si:common-lisp-readtable)))
          (*print-base* 10.)
          (*read-base*  10.))
      (SETF (BUFFER-PACKAGE *INTERVAL*) PKG)
      (send *interval* :set-attribute :base *read-base*)
      (send *interval* :set-attribute :package package-name)
      (send *interval* :set-attribute :readtable readtable-name) ; acts as a default
      (DISCARD-UNDO-INFORMATION *INTERVAL*)
      (DELETE-INTERVAL *INTERVAL*)
      (UNWIND-PROTECT
        (PROGN
          (COND ((ERRORP STREAM)
                 (FORMAT *QUERY-IO* "~&Creating init file ~A" PATHNAME)
                 (FORMAT (INTERVAL-STREAM *INTERVAL*)
                         ";~A's ZMAIL init file -*-Mode:Lisp;Package:~A;Readtable:~A;Base:~D-*-~%"
                         USER-ID package-name readtable-name *read-base*)
                 (INSERT-CHANGED-VARIABLES T)
                 (MOVE-BP (WINDOW-POINT *PROFILE-EDITOR-WINDOW*)
                          (INTERVAL-LAST-BP *INTERVAL*))
                 (SETF (BUFFER-FILE-ID *INTERVAL*) T)
                 (SETF (BUFFER-TICK *INTERVAL*) (TICK)))
                (T
                 (SETQ PATHNAME (SEND STREAM :PATHNAME))
                 ;; If file is compiled, open the source file instead.
                 (IF (NOT (SEND STREAM :CHARACTERS))
                     (LET ((PLIST (FS:EXTRACT-ATTRIBUTE-LIST STREAM)))
                       (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) 0)
                       (SETQ PATHNAME
                             (SEND (GET (LOCF PLIST) :QFASL-SOURCE-FILE-UNIQUE-ID)
                                   :NEW-VERSION :NEWEST))
                       (CLOSE STREAM)
                       (SETQ STREAM (OPEN PATHNAME)))
                   (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) NIL)
                   (SETQ PATHNAME (SEND PATHNAME :NEW-VERSION :NEWEST)))
                 (FORMAT *QUERY-IO* "~&Reading init file ~A" (SEND STREAM :TRUENAME))
                 (SETF (BUFFER-TICK *INTERVAL*) (TICK))
                 ;; The following also sets *ZMACS-BUFFER-VERSION-STRING*
                 (SET-BUFFER-FILE-ID *INTERVAL* (SEND STREAM :INFO))
                 (SETF (BUFFER-GENERIC-PATHNAME *INTERVAL*) (SEND PATHNAME :GENERIC-PATHNAME))
                 (STREAM-COPY-UNTIL-EOF STREAM
                                        (INTERVAL-STREAM-INTO-BP
                                          (INTERVAL-LAST-BP *INTERVAL*)))
                 (SECTIONIZE-BUFFER *INTERVAL*)
                 (FS:READ-ATTRIBUTE-LIST *INTERVAL* (INTERVAL-STREAM *INTERVAL*))
                 ;; Old init files are assumed to have Zetalisp syntax, Base 10
                 (unless (send *interval* :get-attribute :readtable)
                   (format *query-io* "~&Updating Readtable attribute of ZMail init file.")
                   (send *interval* :set-attribute :readtable "ZL" t))
                 (unless (send *interval* :get-attribute :base)
                   (format *query-io* "~&Updating Base attribute of ZMail init file.")
                   (send *interval* :set-attribute :base *read-base* t))
                 (DECIDE-IF-SOURCE-MATCHES-QFASL STREAM)))
          (SEND *INTERVAL* :RENAME (SEND PATHNAME :STRING-FOR-EDITOR))
          (reparse-buffer-attribute-list *interval*)
          (SETF (BUFFER-PATHNAME *INTERVAL*) PATHNAME)
          (SETF (BUFFER-GENERIC-PATHNAME *INTERVAL*)
                (SEND PATHNAME :GENERIC-PATHNAME))
          (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))
          (SETF (PROFILE-BUFFER-EDITOR-VARIABLE-TICK *INTERVAL*) (TICK))
          (MUST-REDISPLAY *PROFILE-EDITOR-WINDOW* DIS-TEXT))
        (OR (ERRORP STREAM)
            (CLOSE STREAM))))))

(DEFUN DECIDE-IF-SOURCE-MATCHES-QFASL (STREAM &AUX QFASL-FILE SRC-FILE)
  ;; We decide if the QFASL file is the one corresponding with
  ;; the source file we found by checking both the specific pathname
  ;; and the creation dates.  Our argument is a stream for the source
  ;; file.
  (COND ((NULL (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*)))  ;; Was not compiled.
        ((NEQ (SETQ SRC-FILE (SEND STREAM :TRUENAME))
              (SETQ QFASL-FILE (SEND (BUFFER-GENERIC-PATHNAME *INTERVAL*)
                                     :GET :QFASL-SOURCE-FILE-UNIQUE-ID)))
         (TYPEIN-LINE
           "(Profile compiled from ~A, current source = ~A.)
 (Click SAVE to recompile.)"
           QFASL-FILE SRC-FILE)
         (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) -1))
        ((CONDITION-CASE (ERROR)
             (SETQ QFASL-FILE (OPEN (ZMAIL-INIT-FILE-PATHNAME) :DIRECTION :PROBE
                                                               :IF-DOES-NOT-EXIST :ERROR))
           (ERROR (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) -1)
                  (ZMAIL-ERROR "Init file suddenly missing:  ~A" QFASL-FILE)
                  T)
           (:NO-ERROR NIL)))
        ((> (SEND STREAM :CREATION-DATE)
            (SEND QFASL-FILE :CREATION-DATE))
         (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) -1)
         (FORMAT *QUERY-IO* "~&~
/(Your Compiled profile is older than its source file.  Click SAVE to recompile)"))
        (T (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*)
                 (BUFFER-TICK *INTERVAL*)))))

;;; Here we define the methods that ZMAIL-COMMAND-LOOP-MIXIN requires
;;; to make the *PROFILE-WINDOW*'s command loop do the right stuff.

(DEFMETHOD (ZMAIL-PROFILE-FRAME :TOP-LEVEL-TAG) () 'EXIT-PROFILE-EDITOR)

(DEFMETHOD (ZMAIL-PROFILE-FRAME :RESET-PROFILE) ()
  (SET-BUFFER-FILE-ID (WINDOW-INTERVAL *PROFILE-EDITOR-WINDOW*) NIL))

(DEFMETHOD (ZMAIL-PROFILE-FRAME :PROCESS-SPECIAL-COMMAND) (&REST ARGS)
  (APPLY #'ZMAIL-PROFILE-COMMAND-LIST ARGS))

(DEFSELECT (ZMAIL-PROFILE-COMMAND-LIST ZMAIL-COMMAND-LIST-DEFAULT)
  (:VARIABLE-CHOICE (WINDOW ITEM CHOICE LINE-NO BUTTON)
    (TV:WITH-SELECTION-SUBSTITUTE
      ((SEND *PROFILE-WINDOW* :GET-PANE 'CHOOSE-WINDOW)
       *ZMAIL-WINDOW*)
      (TV:CHOOSE-VARIABLE-VALUES-CHOICE WINDOW ITEM CHOICE LINE-NO BUTTON)
      (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))
      (LET ((*EXPLICIT-OPTION-UPDATE* T))
        (DOLIST (COM (GET (CAR ITEM) 'DOCUMENTATION-ASSOCIATED-COMMANDS))
          (UPDATE-COMMAND-WHO-LINE-DOCUMENTATION COM))))
    DIS-NONE)
  (:MOUSE-BUTTON (CH WINDOW IGNORE IGNORE &AUX WINDOW-NAME *ZMAIL-COMMAND-BUTTON* NEAR-MODE)
    (COND ((EQ WINDOW *PROFILE-EDITOR-WINDOW*)  ;click on zmacs window
           (SEND *STANDARD-INPUT* :UNTYI *LAST-COMMAND-CHAR*)
           (SEND SELF :PROCESS-SPECIAL-COMMAND 'SELECT-WINDOW *WINDOW*))
          ((SEND WINDOW :OPERATION-HANDLED-P :SET-ACCENT)
           (SEND WINDOW :SET-ACCENT T)          ;may have gotten turned off
           (SET-COMMAND-BUTTON CH)
           (SETQ WINDOW-NAME (SEND *PROFILE-WINDOW* :PANE-NAME WINDOW)
                 NEAR-MODE `(:WINDOW ,WINDOW))
           (UNWIND-PROTECT
               (CASE WINDOW-NAME
                 (FILTERS-BUTTON
                  (PROFILE-FILTERS-BUTTON NEAR-MODE))
                 (UNIVERSES-BUTTON
                  (PROFILE-UNIVERSES-BUTTON NEAR-MODE))
                 (MAIL-FILES-BUTTON
                  (PROFILE-MAIL-FILES-BUTTON NEAR-MODE))
                 (KEYWORDS-BUTTON
                  (PROFILE-KEYWORDS-BUTTON NEAR-MODE))
                 (HARDCOPY-BUTTON
                  (CHOOSE-HARDCOPY-OPTIONS NEAR-MODE :BOTH))
                 (FILE-OPTIONS-BUTTON
                  (LET* ((ALIST (OR (GET-ZMAIL-BUFFER-ALISTS T) (BARF "No buffers to choose")))
                         (ZMAIL-BUFFER (TV:MENU-CHOOSE ALIST NIL NEAR-MODE)))
                    (COND (ZMAIL-BUFFER
                           (ZMAIL-BUFFER-COERCE ZMAIL-BUFFER)
                           (CHOOSE-MAIL-FILE-OPTIONS ZMAIL-BUFFER)))))
                 (DONE-BUTTON
                  (*THROW 'EXIT-PROFILE-EDITOR T))
                 ((RESET-BUTTON DEFAULTS-BUTTON)
                  (RESET-USER-OPTIONS *ZMAIL-USER-OPTION-ALIST*)
                  (IF (EQ WINDOW-NAME 'RESET-BUTTON)
                      (PROGN (LOAD (ZMAIL-INIT-FILE-PATHNAME)
                                   (BUFFER-PACKAGE *INTERVAL*)
                                   T)
                             (WITH-OPEN-FILE (SRC-FILE (BUFFER-PATHNAME *INTERVAL*) :PROBE)
                               (DECIDE-IF-SOURCE-MATCHES-QFASL SRC-FILE)))
                    (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) NIL))
                  (SEND *PROFILE-WINDOW* :SEND-PANE 'CHOOSE-WINDOW :REFRESH)
                  (UPDATE-ALL-COMMANDS-ASSOCIATED-WITH-OPTIONS-DOCUMENTATION)
                  (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))
                  (AND (BUFFER-NEEDS-SAVING-P *INTERVAL*)
                       (IF (AND (EQ WINDOW-NAME 'RESET-BUTTON)
                                (TYPEOUT-BEEP-YES-OR-NO-P "Revert file buffer too? "))
                           (REVERT-BUFFER *INTERVAL*)
                         (INSERT-CHANGED-VARIABLES :ASK))))
                 (SAVE-BUTTON
                  (PROFILE-SAVE-BUTTON NEAR-MODE))
                 ;; "Edit" button -- edit the text of the init file.
                 (EDIT-BUTTON
                  ;; Make sure text buffer reflects changes made in the CVV window.
                  (INSERT-CHANGED-VARIABLES NIL)
                  (TV:WITH-SELECTION-SUBSTITUTE (*PROFILE-EDITOR-WINDOW* *ZMAIL-WINDOW*)
                    (*CATCH 'EXIT-TOP-LEVEL
                      (*CATCH 'ABORT-STANDALONE-EDIT
                        (LET ((*EDITING-PROFILE* NIL))
                          (SEND *PROFILE-EDITOR-WINDOW* :EDIT))))
                    (RESECTIONIZE-BUFFER *INTERVAL*)
                    ;; Now read in whatever the user changed in the text
                    (RESET-USER-OPTIONS *ZMAIL-USER-OPTION-ALIST*)
                    (SI:READFILE-INTERNAL (INTERVAL-STREAM *INTERVAL*)
                                          (BUFFER-PACKAGE *INTERVAL*) T)
                    ;; Update things based on new values.
                    (SEND *PROFILE-WINDOW* :SEND-PANE 'CHOOSE-WINDOW :REFRESH)
                    (UPDATE-ALL-COMMANDS-ASSOCIATED-WITH-OPTIONS-DOCUMENTATION)
                    ;; Remember that CVV window and text buffer now match each other.
                    (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))
                    (SETF (PROFILE-BUFFER-EDITOR-VARIABLE-TICK *INTERVAL*)
                          *TICK*)))
                 (OTHERWISE
                  (BARF "~S is not a recognized window" WINDOW)))
             (SEND WINDOW :SET-ACCENT NIL)))
          (T NIL))                              ;clicked at some random place.
     DIS-NONE)
  (SELECT-WINDOW (IGNORE)
    ;; Clicking on the editor window sends this blip.
    ;; Pretend it was a click on the "Edit" button.
    (ZMAIL-PROFILE-COMMAND-LIST :MOUSE-BUTTON :LEFT
                                (SEND *PROFILE-WINDOW* :GET-PANE 'EDIT-BUTTON)
                                NIL NIL))
  )

;;; Saving and compilation of init files.

(DEFVAR *PROFILE-SAVE-MENU-ALIST*
  '(("Save file" :VALUE :SAVE
     :DOCUMENTATION "Save out the init file, changed variables are NOT inserted first.")
    ("Make init compiled" :VALUE :COMPILE-INIT
     :DOCUMENTATION "Make init file be compiled.  This will speed up its loading somewhat.")
    ("Insert changes" :VALUE :INSERT-CHANGED
     :DOCUMENTATION "Insert the changed variables, file is NOT saved.")
    ("Reap file" :VALUE :REAP
     :DOCUMENTATION "Offer to delete old versions of init file.")
    ("Recompile file" :VALUE :RECOMPILE
     :DOCUMENTATION "Recompile init file.")))

(DEFUN PROFILE-SAVE-BUTTON (NEAR-MODE &AUX MODE)
  (SETQ MODE (CASE *ZMAIL-COMMAND-BUTTON*
               ((:LEFT :KBD) :SAVE-ALL)
               (:MIDDLE :COMPILE-INIT)
               (:RIGHT
                (TV:MENU-CHOOSE *PROFILE-SAVE-MENU-ALIST* NIL NEAR-MODE))))
  (CASE MODE
    (:SAVE
     (COM-SAVE-FILE))
    (:COMPILE-INIT
     (MAKE-INIT-FILE-BE-COMPILED))
    (:INSERT-CHANGED
     (INSERT-CHANGED-VARIABLES T))
    (:REAP
     (REAP-FILE (SEND (ZMAIL-INIT-FILE-PATHNAME) :NEW-PATHNAME :VERSION :WILD))
     (AND (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*)
          (REAP-FILE (SEND (BUFFER-PATHNAME *INTERVAL*) :NEW-PATHNAME :VERSION :WILD))))
    (:RECOMPILE
     (COMPILE-ZMAIL-INIT-FILE))
    (:SAVE-ALL
     (INSERT-CHANGED-VARIABLES :ASK)
     (COM-SAVE-FILE)
     (MAYBE-COMPILE-ZMAIL-INIT-FILE))))

;;; Compilation of init files.
;;; When the init file is compiled, the normal init file pathname
;;; is used for the compiled version, and the source is stored
;;; in another file, which is asked for when the user asks for compilation.

;;; The BUFFER-PATHNAME and BUFFER-GENERIC-PATHNAME always refer to a text
;;; file, whether that is the actual init file or not.

;;; The PROFILE-BUFFER-PROFILE-COMPILE-TICK is non-NIL if a compiled init file is in use.
;;; If it matches the BUFFER-TICK, then the compiled version matches the source.

(DEFUN MAYBE-COMPILE-ZMAIL-INIT-FILE ()
  (IF (AND (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*)
           (NEQ (BUFFER-TICK *INTERVAL*) (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*))
           (YES-OR-NO-P "Recompile ZMAIL init file? "))
      (COMPILE-ZMAIL-INIT-FILE)))

;;;TYPEOUT-BEEP-YES-OR-NO-P moved to ZWEI;DISPLA -Keith

(DEFUN COMPILE-ZMAIL-INIT-FILE ()
  "Recompile the init file.  Barf if it has not been compiled so far."
  (IF (NULL (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*))
      (BARF "Your init file is not compiled.")
    (SAVE-BUFFER-IF-NECESSARY *INTERVAL* T)
    (QC-FILE (BUFFER-PATHNAME *INTERVAL*) (ZMAIL-INIT-FILE-PATHNAME))
    (LOAD (ZMAIL-INIT-FILE-PATHNAME))
    (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) (BUFFER-TICK *INTERVAL*))))

(DEFUN MAKE-INIT-FILE-BE-COMPILED ()
  "Switch to using a compiled init file if not already so, and recompile if needed."
  (UNLESS (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*)
    (LET* ((DEFAULT (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME
                          :NAME "ZMAIL" :TYPE :LISP :VERSION :NEWEST))
           (PATHNAME))
      (DO () (())
        (SETQ PATHNAME
              (READ-DEFAULTED-PATHNAME "Pathname for source file: " DEFAULT))
        (UNLESS (EQ PATHNAME (ZMAIL-INIT-FILE-PATHNAME))
          (RETURN))
        (BEEP)
        (FORMAT *QUERY-IO* "~&That is where the compiled version must go.  Try again.")
        (SEND *QUERY-IO* :WAIT-FOR-INPUT-WITH-TIMEOUT 30.))
      (SETF (BUFFER-PATHNAME *INTERVAL*) PATHNAME)
      (SETF (BUFFER-GENERIC-PATHNAME *INTERVAL*) (SEND PATHNAME :GENERIC-PATHNAME))
      (SEND *INTERVAL* :RENAME (SEND PATHNAME :STRING-FOR-EDITOR))
      ;; Say source needs to be saved.
      (SETF (BUFFER-FILE-ID *INTERVAL*) T)
      ;; Say compilation is very out-of-date.
      (SETF (PROFILE-BUFFER-PROFILE-COMPILE-TICK *INTERVAL*) -1)))
  (COMPILE-ZMAIL-INIT-FILE))

(DEFUN INSERT-CHANGED-VARIABLES (WHEN)
  "Update the text form of the init file to correspond to actual variable values.
We assume that the PROFILE-BUFFER-VARIABLE-TICK of the init file buffer
is updated when a variable's value is changed, and we use the
PROFILE-BUFFER-EDITOR-VARIABLE-TICK to record when we put the changes in.

WHEN says how to decide whether to do updating.
T means always.  NIL means only if we think there are changed values.
:ASK means only if there are changes, and also ask the user to confirm."
  (WHEN (OR (EQ WHEN T)
            (AND (> (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*)
                    (PROFILE-BUFFER-EDITOR-VARIABLE-TICK *INTERVAL*))
                 (OR (NEQ WHEN :ASK)
                     (TYPEOUT-BEEP-YES-OR-NO-P "Insert changed variables? "))))
    (UNLESS (ZEROP (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))
      (INSERT (INTERVAL-LAST-BP *INTERVAL*) #/CR))
    (LET* ((BP (DO ((BP (INTERVAL-FIRST-BP *INTERVAL*))
                    (START-BP) (LAST-END-BP))
                   (NIL)
                 (OR (STRING-EQUAL (BP-LINE BP) "(" :start1 0 :start2 0 :end1 1 :end2 1)
                     (SETQ BP (FORWARD-DEFUN BP 1 T)))
                 (COND ((STRING-EQUAL (BP-LINE BP) "(LOGIN-SETQ" :start1 0 :start2 0 :end1 13 :end2 13)
                        (OR START-BP (SETQ START-BP BP))
                        (SETQ BP (FORWARD-SEXP BP 1 T))
                        (SETQ BP (BEG-LINE BP 1 T))
                        (SETQ LAST-END-BP BP))
                       (T
                        (AND START-BP (DELETE-INTERVAL START-BP LAST-END-BP T))
                        (RETURN (OR LAST-END-BP BP))))))
           (STREAM (INTERVAL-STREAM-INTO-BP BP)))
      (multiple-value-bind (vars vals)
          (fs:extract-attribute-bindings (interval-stream *interval*))
        (progv vars vals
          (WRITE-USER-OPTIONS *ZMAIL-USER-OPTION-ALIST* STREAM)
          (WRITE-USER-OPTIONS *ZMAIL-HARDCOPY-OPTION-ALIST* STREAM)
          (SEND STREAM :TYO #\NewLine)
          (MUST-REDISPLAY *WINDOW* DIS-TEXT)
          (SETF (PROFILE-BUFFER-EDITOR-VARIABLE-TICK *INTERVAL*)
                (TICK)))))))

;; Anybody who can change values of ZMAIL options should send this.
(DEFMETHOD (ZMAIL-PROFILE-FRAME :VARIABLE-TICK) ()
  (SETF (PROFILE-BUFFER-VARIABLE-TICK (WINDOW-INTERVAL *PROFILE-EDITOR-WINDOW*))
        (TICK)))

(DEFFLAVOR ZMAIL-CHOOSE-VARIABLE-VALUES-PANE
        ()
        TV:(CHOOSE-VARIABLE-VALUES-PANE-MIXIN BASIC-CHOOSE-VARIABLE-VALUES
            BORDERS-MIXIN TOP-BOX-LABEL-MIXIN SCROLL-STUFF-ON-OFF-MIXIN ANY-TYI-MIXIN WINDOW))

;;; The stuff on this page is not called from anywhere.

;;; This function is not used.
(DEFUN MAKE-ZMAIL-BUFFER-OF-RECENT-MSGS (&AUX ZMAIL-BUFFER NEW-ARRAY ARRAY)
  (SETQ ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER :NAME "Recent")
        NEW-ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)
        ARRAY (ZMAIL-BUFFER-ARRAY *ZMAIL-BUFFER*))
  (DO ((I 0 (1+ I))
       (NMSGS (ARRAY-ACTIVE-LENGTH ARRAY))
       (MSG))
      (( I NMSGS))
    (SETQ MSG (AREF ARRAY I))
    (IF (GET (LOCF (MSG-STATUS MSG)) 'RECENT)   ;Cannot be recent if not parsed
        (VECTOR-PUSH-EXTEND MSG NEW-ARRAY)
        (RETURN T)))                            ;Can stop when find first non-recent
  ZMAIL-BUFFER)

;;; Quick processing of a set of messages
(DEFUN PROCESS-FILTER (FILTER &REST OPTIONS &AUX NAME NAME-BEFORE NAME-AFTER)
  (MULTIPLE-VALUE (FILTER NAME NAME-BEFORE NAME-AFTER)
    (PARSE-FILTER-SPEC FILTER))
  (USING-OVERLYING-WINDOW
    (APPLY 'PROCESS-FILTER-1 'MSG-FITS-FILTER-P FILTER
           NAME NAME-BEFORE NAME-AFTER OPTIONS))
  (ZMAIL-SELECT-MSG *MSG* NIL NIL))

(DEFUN PARSE-FILTER-SPEC (FILTER &AUX NAME NAME-BEFORE NAME-AFTER)
  (DECLARE (RETURN-LIST FILTER NAME NAME-BEFORE NAME-AFTER))
  (IF (ATOM FILTER)
      (SETQ NAME FILTER NAME-AFTER NAME)
      (SETQ NAME (CADR FILTER) FILTER (CAR FILTER))
      (IF (ATOM NAME)
          (SETQ NAME-AFTER NAME)
          (SETQ NAME-BEFORE (CAR NAME) NAME-AFTER (CADR NAME)
                NAME (OR NAME-BEFORE NAME-AFTER))))
  (VALUES FILTER NAME NAME-BEFORE NAME-AFTER))

(DEFUN PROCESS-FILTER-1 (FILTER FILTER-ARG NAME NAME-BEFORE NAME-AFTER
                                &REST OPTIONS
                                &AUX (MAP-FUNCTION 'MAP-OVER-SINGLE-ZMAIL-BUFFER)
                                     (MAP-ARG *ZMAIL-BUFFER*) (LAST-P T)
                                     (COUNT-P :ENGLISH) (SURVEY-P T)
                                     (TYPE-P :ASK) (DELETE-P NIL) (KEYWORDS NIL) (SAVE-P NIL)
                                     (MARKING-FUNCTION NIL) (NOT-IF-MARKED-P NIL)
                                     ZMAIL-BUFFER ARRAY NMSGS N-ALREADY-MARKED PRONOUN)
  (TV:DOPLIST (OPTIONS VAL KEY)
    (CASE KEY
      (:MAP-FUNCTION
       (SETQ MAP-FUNCTION VAL))
      (:MAP-ARG
       (SETQ MAP-ARG VAL))
      (:COUNT-P
       (SETQ COUNT-P VAL))
      (:SURVEY-P
       (SETQ SURVEY-P VAL))
      (:TYPE-P
       (SETQ TYPE-P VAL))
      (:DELETE-P
       (SETQ DELETE-P VAL))
      (:SAVE-P
       (SETQ SAVE-P VAL))
      (:KEYWORDS
       (SETQ KEYWORDS VAL))
      (:MARKING-FUNCTION
       (SETQ MARKING-FUNCTION VAL))
      (:NOT-IF-MARKED-P
       (SETQ NOT-IF-MARKED-P VAL))
      (:LAST-P
       (SETQ LAST-P VAL))
      (OTHERWISE
       (ZMAIL-ERROR "Unknown keyword: ~S" KEY))))
  (SETQ ZMAIL-BUFFER (MAKE-ZMAIL-BUFFER 'TEMP-ZMAIL-BUFFER :NAME NAME))
  (MAKE-ZMAIL-BUFFER-FROM-FILTER MAP-FUNCTION MAP-ARG FILTER FILTER-ARG ZMAIL-BUFFER)
  (SETQ ARRAY (ZMAIL-BUFFER-ARRAY ZMAIL-BUFFER)
        NMSGS (ARRAY-ACTIVE-LENGTH ARRAY)
        N-ALREADY-MARKED 0)
  (AND MARKING-FUNCTION
       (DO ((I 0 (1+ I))
            (MSG))
           (( I NMSGS))
         (SETQ MSG (AREF ARRAY I))
         (COND ((AND (NOT (SEND MARKING-FUNCTION MSG))
                     NOT-IF-MARKED-P)           ;Already done once
                (REMOVE-MSG ZMAIL-BUFFER MSG I)
                (SETQ N-ALREADY-MARKED (1+ N-ALREADY-MARKED)
                      NMSGS (1- NMSGS)
                      I (1- I))))))
  (AND COUNT-P
       (LET ((BASE (IF (EQ COUNT-P T) 10. COUNT-P)))
         (FORMAT T "~&~: ~@[~A ~]message~P~@[ ~A~]~
                    ~:[ (not counting ~A message~:P already done)~].~%"
                 NMSGS NAME-BEFORE NMSGS NAME-AFTER
                 (ZEROP N-ALREADY-MARKED) N-ALREADY-MARKED)))
  (COND ((NOT (ZEROP NMSGS))
         (SETQ PRONOUN (IF (= NMSGS 1) "it" "them"))
         (COND ((OR (EQ SURVEY-P T)
                    (AND (EQ SURVEY-P :ASK)
                         (SETQ SURVEY-P (FQUERY NIL "Survey ~A? " PRONOUN))))
                (SEND *STANDARD-OUTPUT* :FRESH-LINE)
                (DO ((I 0 (1+ I))
                     (MSG) (STATUS))
                    (( I NMSGS))
                  (SETQ MSG (AREF ARRAY I)
                        STATUS (ASSURE-MSG-PARSED MSG))
                  (SEND *STANDARD-OUTPUT* :TRUNCATED-FORMAT
                        " ~3D~C~A" (1+ I) (STATUS-LETTER STATUS) (MSG-SUMMARY-LINE MSG))
                  (SEND *STANDARD-OUTPUT* :TYO #/CR))))
         (AND (OR (EQ TYPE-P T)
                  (AND (EQ TYPE-P :ASK)
                       (SETQ TYPE-P (FQUERY NIL "Type ~A? " PRONOUN))))
              (LET ((STREAM (MAKE-ZMAIL-BUFFER-STREAM ZMAIL-BUFFER)))
                (SEND *STANDARD-OUTPUT* :FRESH-LINE)
                (SEND *STANDARD-OUTPUT* :VIEW-STREAM STREAM T)
                (SEND *STANDARD-OUTPUT* :MOVE-TO-END)
                (DO ((I 0 (1+ I))
                     (LIM (SEND STREAM :CURRENT-MSG-NO)))
                    (( I LIM))
                  (MSG-PUT (AREF ARRAY I) NIL 'UNSEEN))))
         (AND (OR (EQ DELETE-P T)
                  (AND (EQ DELETE-P :ASK)
                       (SETQ DELETE-P (FQUERY NIL "Delete ~A? " PRONOUN))))
              (DO I 0 (1+ I) ( I NMSGS)
                (MSG-PUT (AREF ARRAY I) T 'DELETED)))
         (COND ((NOT DELETE-P)
                (AND KEYWORDS
                     (DO ((KEYS KEYWORDS (CDDR KEYS))
                          (KEY-P) (KEYWORDS))
                         ((NULL KEYS))
                       (SETQ KEY-P (CAR KEYS)
                             KEYWORDS (CADR KEYS))
                       (COND ((EQ KEY-P :ASK)
                              (FORMAT T "~&Add keyword~P" (LENGTH KEYWORDS))
                              (DOLIST (KEY KEYWORDS)
                                (SEND *STANDARD-OUTPUT* :TYO #/SP)
                                (SEND *STANDARD-OUTPUT* :STRING-OUT
                                      (OR (CAR (RASSQ KEY *KEYWORD-ALIST*))
                                          (STRING KEY))))
                              (SEND *STANDARD-OUTPUT* :STRING-OUT "? ")
                              (SETQ KEY-P (Y-OR-N-P))))
                       (AND KEY-P
                            (DOMSGS (MSG ZMAIL-BUFFER)
                                    (LET* ((OLD-KEYWORDS (MSG-GET MSG 'KEYWORDS))
                                           (NEW-KEYWORDS (DO ((L KEYWORDS (CDR L))
                                                              (NL (REVERSE OLD-KEYWORDS)))
                                                             ((NULL L) (NREVERSE NL))
                                                           (OR (MEMQ (CAR L) NL)
                                                               (PUSH (CAR L) NL)))))
                                      (CHANGE-MSG-KEYWORDS MSG NEW-KEYWORDS OLD-KEYWORDS))))))
                (AND (OR (EQ SAVE-P T)
                         (AND (EQ SAVE-P :ASK)
                              (SETQ SAVE-P (FQUERY NIL "Save ~A? " PRONOUN))))
                     (SETQ *ZMAIL-BUFFER-LIST*
                           (NCONC *ZMAIL-BUFFER-LIST* (NCONS ZMAIL-BUFFER))))))))
  (COND (LAST-P
         (FORMAT T "~&Type any character to flush:")
         (SEND *STANDARD-INPUT* :TYI))))

(DEFUN PROCESS-FILTER-ALIST (ZMAIL-BUFFER ALIST &OPTIONAL MENU-P)
  (USING-OVERLYING-WINDOW
    (PROCESS-FILTER-ALIST-1 ZMAIL-BUFFER ALIST MENU-P))
  (ZMAIL-SELECT-MSG *MSG* NIL NIL))

(DEFUN PROCESS-FILTER-ALIST-1 (ZMAIL-BUFFER ALIST &OPTIONAL MENU-P &AUX LIST MARK-P)
  (DOMSGS (MSG ZMAIL-BUFFER)
    (MSG-PUT MSG NIL 'PROCESSED))
  (IF MENU-P
      (DO ((AL ALIST (CDR AL))
           (L NIL)
           (FILTER) (NAME) (ITEM))
          ((NULL AL) (SETQ LIST (NREVERSE L)))
        (SETQ FILTER (CAR AL))
        (SETQ NAME (IF (ATOM (CAR FILTER)) (STRING (CAR FILTER)) (CADR FILTER)))
        (AND (CONSP NAME) (SETQ NAME (CAR NAME)))
        (SETQ ITEM `(,NAME :VALUE ,FILTER))
        (AND (EQ (IF (ATOM (CAR FILTER)) (CAR FILTER) (CAAR FILTER)) 'NOT-PROCESSED)
             (SETQ MARK-P T
                   ITEM (NCONC ITEM '((:FONT FONTS:HL12I)))))
        (PUSH ITEM L))
      (SETQ LIST ALIST)
      (DO ((AL ALIST (CDR AL))
           (FILTER))
          ((NULL AL))
        (SETQ FILTER (CAAR AL))
        (AND (EQ (IF (ATOM FILTER) FILTER (CAR FILTER)) 'NOT-PROCESSED)
             (SETQ MARK-P T))))
  (AND MARK-P (SETQ MARK-P 'MARK-MSG-AS-PROCESSED))
  (DO ((ITEM) (FILTER) (NAME) (NAME-BEFORE) (NAME-AFTER) (FILTER-ARG))
      (NIL)
    (SETQ ITEM (IF MENU-P (TV:MENU-CHOOSE LIST) (POP LIST)))
    (AND (NULL ITEM) (RETURN T))
    (MULTIPLE-VALUE (FILTER NAME NAME-BEFORE NAME-AFTER)
      (PARSE-FILTER-SPEC (CAR ITEM)))
    (IF (EQ FILTER 'NOT-PROCESSED)
        (SETQ FILTER 'MSG-DOES-NOT-HAVE-ATTRIBUTE-P
              FILTER-ARG 'PROCESSED)
      (PSETQ FILTER 'MSG-FITS-FILTER-P
             FILTER-ARG FILTER))
    (APPLY 'PROCESS-FILTER-1 FILTER FILTER-ARG NAME NAME-BEFORE NAME-AFTER
           :MAP-ARG ZMAIL-BUFFER :MARKING-FUNCTION MARK-P :LAST-P (NULL LIST)
           (CDR ITEM))
    (FORMAT T "~2&")))

;;; Return T if processed for the first time
(DEFUN MARK-MSG-AS-PROCESSED (MSG &AUX (STATUS (ASSURE-MSG-PARSED MSG)))
  (AND (NOT (GET STATUS 'PROCESSED))
       (PUTPROP STATUS T 'PROCESSED)))

;;; >> Babyl knowledge shouldn't be wired in; neither should internal keyword storage
;;; format.
(DEFUN EDIT-MAIL-FILES-KEYWORDS (NEAR-MODE &AUX STRING)
  "Let user edit the keywords mail files remember, using a pop up editor window.
NEAR-MODE is used in TV:EXPOSE-WINDOW-NEAR, in exposing the window."
  (DOLIST (ZMAIL-BUFFER *ZMAIL-BUFFER-LIST*)
    (AND (TYPEP ZMAIL-BUFFER 'BABYL-MAIL-FILE-BUFFER)           ;Only kind that can store these
         (LET ((OPTIONS (LOCF (ZMAIL-BUFFER-OPTIONS ZMAIL-BUFFER))))
           (SEND ZMAIL-BUFFER :UPDATE-OPTIONS-IN-FILE)  ;Make sure string correct
           (OR STRING (SETQ STRING (MAKE-EMPTY-STRING 100.)))
           (APPEND-TO-ARRAY STRING (ZMAIL-BUFFER-NAME ZMAIL-BUFFER))
           (APPEND-TO-ARRAY STRING "")
           (APPEND-TO-ARRAY STRING (OR (GET OPTIONS :KEYWORDS-STRING) ""))
           (VECTOR-PUSH-EXTEND #\Newline STRING))))
  (OR STRING (BARF "No mail files able to remember keywords"))
  (AND (SETQ STRING (POP-UP-EDSTRING STRING NEAR-MODE
                                     `("ZMail " "Keywords"
                                       ,(FORMAT NIL "     ~:@C ends, ~:@C aborts"
                                                #/END #/ABORT))
                                     800 200
                                     "Format is Mail filekey1,key2,..."))
       (DO ((I 0 (1+ I))
            (J) (ZMAIL-BUFFER) (OPTIONS))
           (NIL)
         (OR (SETQ I (STRING-SEARCH-NOT-SET *WHITESPACE-CHARS* STRING I))
             (RETURN NIL))
         (OR (SETQ J (STRING-SEARCH "" STRING I))
             (BARF "Returned string in bad format, no arrows found"))
         (SETQ ZMAIL-BUFFER (SUBSTRING STRING I J))
         (SETQ ZMAIL-BUFFER (OR (GET-ZMAIL-BUFFER-FROM-NAME ZMAIL-BUFFER)
                                (BARF "No mail file named ~A" ZMAIL-BUFFER)))
         (SETQ OPTIONS (LOCF (ZMAIL-BUFFER-OPTIONS ZMAIL-BUFFER)))
         (SETQ J (+ J 2)
               I (STRING-SEARCH-CHAR #/CR STRING J))
         (LOOP FOR (PROPNAME PROPVAL) ON (PARSE-KEYWORDS-LIST NIL STRING J I) BY 'CDDR
               DO (PUTPROP OPTIONS PROPVAL PROPNAME))
         (OR I (RETURN NIL)))))

(DEFUN PROFILE-MAIL-FILES-BUTTON (NEAR-MODE &AUX MODE)
  (SETQ MODE (CASE *ZMAIL-COMMAND-BUTTON*
               ((:LEFT :KBD) :PROFILE-REMEMBERED-MAIL-FILES)
               (:MIDDLE :FILTER-ASSOCIATIONS)
               (:RIGHT
                (TV:MENU-CHOOSE '(("Other mail files" :VALUE :PROFILE-REMEMBERED-MAIL-FILES
                                   :DOCUMENTATION
 "Give a menu of non-primary disk mail files to be remembered in init file.")
                                  ("Filter associations" :VALUE :FILTER-ASSOCIATIONS
                                   :DOCUMENTATION "Associate a mail file with filters."))
                                NIL NEAR-MODE))))
  (CASE MODE
    (:PROFILE-REMEMBERED-MAIL-FILES (EDIT-PROFILE-REMEMBERED-MAIL-FILES NEAR-MODE))
    (:FILTER-ASSOCIATIONS (EDIT-MAIL-FILE-FILTER-ASSOCIATIONS NEAR-MODE))))

(DEFUN EDIT-PROFILE-REMEMBERED-MAIL-FILES (NEAR-MODE)
  (LET ((ZMAIL-BUFFER-ALIST (GET-ZMAIL-BUFFER-ALISTS T))
        (ACTIVE-LIST NIL))
    (SETQ ZMAIL-BUFFER-ALIST (DELQ (RASSQ *PRIMARY-ZMAIL-BUFFER* ZMAIL-BUFFER-ALIST)
                                   ZMAIL-BUFFER-ALIST))
    ;; ACTIVE-LIST gets all the buffers (or pathnames, if file is not loaded)
    ;; that are remembered in the init file as of now.
    (DOLIST (FILE-NAME *OTHER-MAIL-FILE-NAMES*)
      (PUSHNEW (CDR (OR (RASSQ FILE-NAME ZMAIL-BUFFER-ALIST)
                        (GLOBAL:FIND (SEND FILE-NAME :NEW-VERSION NIL)
                                     ZMAIL-BUFFER-ALIST
                                  :KEY #'(LAMBDA (X) (IF (TYPEP (CDR X) 'INTERVAL)
                                                          (BUFFER-PATHNAME (CDR X))
                                                        (CDR X))))))
               ACTIVE-LIST))
    (SETQ ACTIVE-LIST (DELQ NIL ACTIVE-LIST))
    (MULTIPLE-VALUE (ZMAIL-BUFFER-ALIST ACTIVE-LIST)
      (ZMAIL-MULTIPLE-MENU-CHOOSE ZMAIL-BUFFER-ALIST (NREVERSE ACTIVE-LIST)
                                  'MULTIPLE-MENU-NEW-PATHNAME NEAR-MODE
                                  "Mail files to be remembered in init file"))
    (SETQ ACTIVE-LIST (LOOP FOR (name . buffer) IN ZMAIL-BUFFER-ALIST
                            WHEN (MEMQ buffer ACTIVE-LIST)
                            COLLECT (send (if (typep buffer 'interval)
                                              (buffer-pathname buffer)
                                            buffer)
                                          :string-for-printing)))
    (COND ((NOT (EQUAL ACTIVE-LIST *OTHER-MAIL-FILE-NAMES*))
           (SETQ *OTHER-MAIL-FILE-NAMES* ACTIVE-LIST)
           (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))))))

(DEFUN PROFILE-KEYWORDS-BUTTON (NEAR-MODE &AUX MODE)
  (SETQ MODE (CASE *ZMAIL-COMMAND-BUTTON*
               ((:LEFT :KBD) :EDIT-KEYWORDS)
               (:MIDDLE :FILTER-ASSOCIATIONS)
               (:RIGHT
                (TV:MENU-CHOOSE '(("Mail file keywords" :VALUE :EDIT-KEYWORDS
                                   :DOCUMENTATION
                                   "Edit the keywords associated with a mail file.")
                                  ("Filter associations" :VALUE :FILTER-ASSOCIATIONS
                                   :DOCUMENTATION "Associate a keyword with filters."))
                                NIL NEAR-MODE))))
  (CASE MODE
    (:EDIT-KEYWORDS (EDIT-MAIL-FILES-KEYWORDS NEAR-MODE))
    (:FILTER-ASSOCIATIONS (GET-KEYWORD-FILTER-ASSOCIATIONS NEAR-MODE))))

(DEFUN PROFILE-FILTERS-BUTTON (NEAR-MODE &AUX MODE)
  (SETQ MODE (CASE *ZMAIL-COMMAND-BUTTON*
               ((:LEFT :KBD) :EDIT-FILTERS)
               (:MIDDLE :FILTER-ASSOCIATIONS)
               (:RIGHT
                (TV:MENU-CHOOSE '(("Edit filter list" :VALUE :EDIT-FILTERS :DOCUMENTATION
                                   "Give a menu of filters to be remembered in init file.")
                                  ("Filter associations" :VALUE :FILTER-ASSOCIATIONS
                                   :DOCUMENTATION
  "Associate a filter with keywords, move mail files, or universes."))
                                NIL NEAR-MODE))))
  (CASE MODE
    (:EDIT-FILTERS
      (PROFILE-FILTERS-OR-UNIVERSES *USER-FILTER-ALIST* NEAR-MODE
                                    "Filters to be remembered in init file"
                                    'PROFILE-NEW-FILTER 'GET-FILTER-DEFINITION))
    (:FILTER-ASSOCIATIONS
     (GET-FILTER-ASSOCIATIONS NEAR-MODE))))

(DEFUN PROFILE-UNIVERSES-BUTTON (NEAR-MODE &AUX MODE)
  (SETQ MODE (CASE *ZMAIL-COMMAND-BUTTON*
               ((:LEFT :KBD) :EDIT-UNIVERSES)
               (:MIDDLE :FILTER-ASSOCIATIONS)
               (:RIGHT
                (TV:MENU-CHOOSE '(("Edit universe list" :VALUE :EDIT-UNIVERSES :DOCUMENTATION
                                   "Give a menu of universes to be remembered in init file.")
                                  ("Filter associations" :VALUE :FILTER-ASSOCIATIONS
                                   :DOCUMENTATION "Associate a universe with filters."))
                                NIL NEAR-MODE))))
  (CASE MODE
    (:EDIT-UNIVERSES
     (PROFILE-FILTERS-OR-UNIVERSES *UNIVERSE-LIST* NEAR-MODE
                                   "Universes to be remembered in init file"
                                   'PROFILE-NEW-UNIVERSE 'GET-UNIVERSE-DEFINITION))
    (:FILTER-ASSOCIATIONS
     (GET-UNIVERSE-FILTER-ASSOCIATIONS NEAR-MODE))))

;;; Implement editing of list of filters or universes to record in the init file.

;;; NEW-FUNCTION is the function to call if user says he wants to define a new one.
;;; DEFINITION-FUNCTION is the function that returns the definition of one.
(DEFUN PROFILE-FILTERS-OR-UNIVERSES (SYMBOL-LIST NEAR-MODE LABEL
                                     NEW-FUNCTION DEFINITION-FUNCTION
                                     &AUX ACTIVE-LIST TEM)
  (DOLIST (SYM SYMBOL-LIST)
    (SETQ SYM (TV:MENU-EXECUTE-NO-SIDE-EFFECTS SYM))
    (AND (ASSQ *INTERVAL* (GET SYM 'ZMAIL-PROFILE-BUFFERS))
         (PUSH SYM ACTIVE-LIST)))
  (MULTIPLE-VALUE (SYMBOL-LIST ACTIVE-LIST)
    (ZMAIL-MULTIPLE-MENU-CHOOSE (COPYLIST SYMBOL-LIST) (NREVERSE ACTIVE-LIST) NEW-FUNCTION
                                NEAR-MODE LABEL))
  (DOLIST (SYM SYMBOL-LIST)                     ;Delete sections that aren't wanted
    (SETQ SYM (TV:MENU-EXECUTE-NO-SIDE-EFFECTS SYM))
    (AND (SETQ TEM (ASSQ *INTERVAL* (GET SYM 'ZMAIL-PROFILE-BUFFERS)))
         (IF (MEMQ SYM ACTIVE-LIST)
             (SETQ ACTIVE-LIST (DELQ SYM ACTIVE-LIST))
             (DELETE-INTERVAL (DEFUN-INTERVAL (CREATE-BP (CDR TEM) 0) 1 T T T)))))
  (AND ACTIVE-LIST
       (LET ((BP (INTERVAL-LAST-BP *INTERVAL*)))
         (DOLIST (SYM ACTIVE-LIST)                      ;New ones to add
           (INSERT BP #/CR)
           (GRIND-INTO-BP BP (SEND DEFINITION-FUNCTION SYM))
           (INSERT BP #/CR))
         (MOVE-BP (WINDOW-POINT *WINDOW*) BP)))
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (RESECTIONIZE-BUFFER *INTERVAL*))

;;; These are the things to use as the NEW-FUNCTION.

(DEFUN PROFILE-NEW-FILTER (WINDOW &OPTIONAL IGNORE IGNORE &AUX FILTER)
  (SEND WINDOW :DEACTIVATE)
  (SETQ FILTER (DEFINE-NEW-FILTER))
  (ASSQ FILTER *USER-FILTER-ALIST*))            ;Return a menu item

(DEFUN PROFILE-NEW-UNIVERSE (WINDOW &OPTIONAL IGNORE IGNORE &AUX UNIVERSE)
  (SEND WINDOW :DEACTIVATE)
  (MULTIPLE-VALUE (NIL NIL NIL UNIVERSE)
    (DEFINE-NEW-UNIVERSE))
  (SEND *ZMAIL-WINDOW* :SELECT NIL)
  UNIVERSE)

;;; Implement editing of associations.

;;; Allow user to edit filter associations of some mail file.
;;; This function does choice of mail file, and the editing and the alteration.
(DEFUN EDIT-MAIL-FILE-FILTER-ASSOCIATIONS (NEAR-MODE &AUX MAIL-FILE OLD-FILTERS NEW-FILTERS)
  (SETQ MAIL-FILE
        (GET-ASSOCIATED-MAIL-FILE-NAME NEAR-MODE
                                       "Select a mail file whose filter associations to edit"
                                       ))
  (SETQ OLD-FILTERS (LOOP FOR (FILTER . MF) IN *FILTER-MOVE-MAIL-FILE-ALIST*
                          WHEN (EQUAL MF MAIL-FILE)
                          COLLECT FILTER))
  (MULTIPLE-VALUE (NIL NEW-FILTERS)
    (ZMAIL-MULTIPLE-MENU-CHOOSE *USER-FILTER-ALIST* OLD-FILTERS 'PROFILE-NEW-FILTER
                                NEAR-MODE
                                (FORMAT NIL "Filters associated with ~A:" MAIL-FILE)))
  (COND ((NOT (EQUAL OLD-FILTERS NEW-FILTERS))
         (DOLIST (ELEM *USER-FILTER-ALIST*)
           (LET* ((FILTER (TV:MENU-EXECUTE-NO-SIDE-EFFECTS ELEM)))
             (SET-MAIL-FILE-FILTER-ASSOCIATION FILTER (AND (MEMQ FILTER NEW-FILTERS)
                                                           MAIL-FILE)))))))

;;; Allow user to edit filter associations of some universe.
;;; This function does choice of universe, and the editing and the alteration.
(DEFUN GET-UNIVERSE-FILTER-ASSOCIATIONS (NEAR-MODE &AUX UNIVERSE OLD-FILTERS NEW-FILTERS)
  (SETQ UNIVERSE (MENU-CHOOSE-WITH-NEW *UNIVERSE-LIST* 'PROFILE-NEW-UNIVERSE NEAR-MODE
                                       "Select a universe whose filter associations to edit"))
  (SETQ OLD-FILTERS (LOOP FOR (FILTER . UV) IN *FILTER-REFERENCE-UNIVERSE-ALIST*
                          WHEN (EQUAL UV UNIVERSE)
                          COLLECT FILTER))
  (MULTIPLE-VALUE (NIL NEW-FILTERS)
    (ZMAIL-MULTIPLE-MENU-CHOOSE *USER-FILTER-ALIST* OLD-FILTERS 'PROFILE-NEW-FILTER
                                NEAR-MODE
                                (FORMAT NIL "Filters associated with ~A:" UNIVERSE)))
  (COND ((NOT (EQUAL OLD-FILTERS NEW-FILTERS))
         (DOLIST (ELEM *USER-FILTER-ALIST*)
           (LET* ((FILTER (TV:MENU-EXECUTE-NO-SIDE-EFFECTS ELEM)))
             (SET-UNIVERSE-FILTER-ASSOCIATION FILTER (AND (MEMQ FILTER NEW-FILTERS)
                                                           UNIVERSE))))
         (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK)))))

;;; Allow user to edit filter associations of some keyword.
;;; This function does choice of keyword, and the editing and the alteration.
(DEFUN GET-KEYWORD-FILTER-ASSOCIATIONS (NEAR-MODE &AUX KEYWORD OLD-FILTERS NEW-FILTERS)
  (SETQ KEYWORD (MENU-CHOOSE-WITH-NEW *KEYWORD-ALIST* 'MULTIPLE-MENU-NEW-KEYWORD NEAR-MODE
                                      "Select a keyword whose filter associations to edit"))
  (WHEN (CONSP KEYWORD)  ;If user specifies a new one, we get back (string . symbol).
    (PUSH KEYWORD *KEYWORD-ALIST*)
    (SETQ KEYWORD (CDR KEYWORD)))
  (SETQ OLD-FILTERS (LOOP FOR (FILTER . KEYWORDS) IN *FILTER-KEYWORDS-ALIST*
                          WHEN (MEMQ KEYWORD KEYWORDS)
                          COLLECT FILTER))
  (MULTIPLE-VALUE (NIL NEW-FILTERS)
    (ZMAIL-MULTIPLE-MENU-CHOOSE *USER-FILTER-ALIST* OLD-FILTERS 'PROFILE-NEW-FILTER
                                NEAR-MODE
                                (FORMAT NIL "Filters associated with ~A:"
                                        (CAR (RASSQ KEYWORD *KEYWORD-ALIST*)))))
  (COND ((NOT (EQUAL OLD-FILTERS NEW-FILTERS))
         (DOLIST (ELEM *USER-FILTER-ALIST*)
           (LET* ((FILTER (TV:MENU-EXECUTE-NO-SIDE-EFFECTS ELEM))
                  (ITEM (ASSQ FILTER *FILTER-KEYWORDS-ALIST*)))
             (IF (MEMQ FILTER NEW-FILTERS)
                 (IF ITEM (PUSH* KEYWORD (CDR ITEM))
                     (SETQ *FILTER-KEYWORDS-ALIST* (NCONC *FILTER-KEYWORDS-ALIST*
                                                          (NCONS (LIST FILTER KEYWORD)))))
                 (AND ITEM (SETF (CDR ITEM) (DELQ KEYWORD (CDR ITEM)))))))
         (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK)))))

;;; Allow user to edit associations (of any sort) of some filter.
;;; This function does choice of filter, choice of what to edit,
;;; and the editing and the alteration.
(DEFUN GET-FILTER-ASSOCIATIONS (NEAR-MODE)
  (LET ((FILTER (MENU-CHOOSE-WITH-NEW *USER-FILTER-ALIST* 'PROFILE-NEW-FILTER NEAR-MODE
                                      "Select a filter whose associations to edit")))
    (CASE (OR (TV:MENU-CHOOSE '(("Keywords" :VALUE :KEYWORDS :DOCUMENTATION
                                    "Edit keywords associated with this filter.")
                                   ("Mail files" :VALUE :MAIL-FILES :DOCUMENTATION
                                    "Set mail file associated with this filter.")
                                   ("Universes" :VALUE :UNIVERSES :DOCUMENTATION
                                    "Set universe associated with this filter.")))
                 (ABORT-CURRENT-COMMAND))
      (:KEYWORDS
       (LET* ((ITEM (ASSQ FILTER *FILTER-KEYWORDS-ALIST*))
              (OLD-KEYWORDS (CDR ITEM))
              NEW-KEYWORDS)
         (SETQ NEW-KEYWORDS (CHOOSE-KEYWORDS (FORMAT NIL "Keywords associated with ~A:"
                                                     FILTER)
                                             OLD-KEYWORDS))
         (COND ((NOT (EQUAL OLD-KEYWORDS NEW-KEYWORDS))
                (IF (NULL NEW-KEYWORDS)
                    (AND ITEM
                         (SETQ *FILTER-KEYWORDS-ALIST* (DELQ ITEM *FILTER-KEYWORDS-ALIST*)))
                    (IF ITEM (SETF (CDR ITEM) NEW-KEYWORDS)
                        (SETQ *FILTER-KEYWORDS-ALIST*
                              (NCONC *FILTER-KEYWORDS-ALIST*
                                     (NCONS (CONS FILTER NEW-KEYWORDS))))))
                (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK))))))
      (:MAIL-FILES
       (LET ((MAIL-FILE (GET-ASSOCIATED-MAIL-FILE-NAME
                             NEAR-MODE
                             (FORMAT NIL "Mail file associated with ~A:"
                                     FILTER)
                             T)))
         (SET-MAIL-FILE-FILTER-ASSOCIATION FILTER MAIL-FILE)))
      (:UNIVERSES
       (LET ((UNIVERSE (MENU-CHOOSE-WITH-NEW (CONS '("None" :VALUE :NONE :FONTS TR12I
                                                      :DOCUMENTATION
                                                      "Remove any association.")
                                                    *UNIVERSE-LIST*)
                                             'PROFILE-NEW-UNIVERSE NEAR-MODE
                                             (FORMAT NIL "Universe associated with ~A:"
                                                     FILTER))))
         (AND (EQ UNIVERSE :NONE) (SETQ UNIVERSE NIL))
         (SET-UNIVERSE-FILTER-ASSOCIATION FILTER UNIVERSE))))))

;;; Subroutines of the above.
(DEFUN GET-ASSOCIATED-MAIL-FILE-NAME
       (NEAR-MODE LABEL &OPTIONAL NONE-OK &AUX ALIST ZMAIL-BUFFER-NAME)
  "Ask user to specify a mail file.  Value is a string.
NONE-OK says that /"none/" is a valid alternative; for that we return NIL.
NEAR-MODE controls positioning of menu and LABEL goes in it."
  (SETQ ALIST (GET-ZMAIL-BUFFER-ALISTS T))
  (AND NONE-OK (PUSH '("None" :VALUE :NONE :FONTS TR12I
                       :DOCUMENTATION "Remove any association.")
                     ALIST))
  (SETQ ZMAIL-BUFFER-NAME
        (MENU-CHOOSE-WITH-NEW ALIST 'MULTIPLE-MENU-NEW-PATHNAME NEAR-MODE LABEL))
  (COND ((EQ ZMAIL-BUFFER-NAME :NONE) NIL)
        ((STRINGP ZMAIL-BUFFER-NAME) ZMAIL-BUFFER-NAME)
        ((TYPEP ZMAIL-BUFFER-NAME 'ZMAIL-BUFFER)
         (SEND ZMAIL-BUFFER-NAME :NAME))
        (T (STRING ZMAIL-BUFFER-NAME))))

(DEFUN SET-MAIL-FILE-FILTER-ASSOCIATION (FILTER NAMESTRING &AUX ITEM)
  "Associate FILTER with mail-file NAMESTRING.
NAMESTRING = NIL removes association for FILTER."
  (SETQ ITEM (ASSQ FILTER *FILTER-MOVE-MAIL-FILE-ALIST*))
  (IF NAMESTRING
      (IF ITEM (SETF (CDR ITEM) NAMESTRING)
          (SETQ *FILTER-MOVE-MAIL-FILE-ALIST* (NCONC *FILTER-MOVE-MAIL-FILE-ALIST*
                                                     (NCONS (CONS FILTER NAMESTRING)))))
    (AND ITEM
         (SETQ *FILTER-MOVE-MAIL-FILE-ALIST* (DELQ ITEM *FILTER-MOVE-MAIL-FILE-ALIST*))))
  (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK)))

(DEFUN SET-UNIVERSE-FILTER-ASSOCIATION (FILTER UNIVERSE-NAME &AUX ITEM)
  "Associate FILTER with universe UNIVERSE-NAME (a string).
UNIVERSE-NAME = NIL removes association for FILTER."
  (SETQ ITEM (ASSQ FILTER *FILTER-REFERENCE-UNIVERSE-ALIST*))
  (IF UNIVERSE-NAME
      (IF ITEM (SETF (CDR ITEM) UNIVERSE-NAME)
          (SETQ *FILTER-REFERENCE-UNIVERSE-ALIST* (NCONC *FILTER-REFERENCE-UNIVERSE-ALIST*
                                                         (NCONS (CONS FILTER UNIVERSE-NAME)))))
      (AND ITEM
           (SETQ *FILTER-REFERENCE-UNIVERSE-ALIST*
                 (DELQ ITEM *FILTER-REFERENCE-UNIVERSE-ALIST*))))
  (SETF (PROFILE-BUFFER-VARIABLE-TICK *INTERVAL*) (TICK)))

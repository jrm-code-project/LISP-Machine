;;; Buffer and window handling for ZWEI. -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-

;>> note: things in this file know about art-string/art-fat-string

;;; Make a window select a certain interval.
;;; This is for callers outside the editor.
(DEFMETHOD (DISPLAYER :SET-INTERVAL) (NEW-INTERVAL)
  (FUNCALL EDITOR-CLOSURE #'SET-INTERVAL-1 SELF NEW-INTERVAL))

(DEFUN SET-INTERVAL-1 (WINDOW NEW-INTERVAL &OPTIONAL IGNORE) ;REAL-*WINDOW*
  (DECLARE (:SELF-FLAVOR DISPLAYER))
  (OR (EQ (WINDOW-INTERVAL WINDOW) NEW-INTERVAL)
      (SEND WINDOW :SET-INTERVAL-INTERNAL NEW-INTERVAL))
  ;; Used to say REAL-*WINDOW* instead of *WINDOW* below
  (IF (EQ WINDOW *WINDOW*)
      (SEND NEW-INTERVAL :SELECT)))

(DEFUN MAKE-WINDOW-CURRENT (WINDOW &OPTIONAL (SELECT-P T) &AUX INTERVAL)
  "Make WINDOW be the selected editor window.
Sets *WINDOW* to it, and sets various other variables based on it.
Selects WINDOW's current interval also.
If SELECT-P is NIL, a :SELECT message is not actually sent."
  (COND ((AND (NEQ WINDOW *WINDOW*)
              (OR (NULL *WINDOW*)
                  (NEQ *WINDOW* *MINI-BUFFER-WINDOW*)))
         (SETQ *WINDOW* WINDOW
               INTERVAL (WINDOW-INTERVAL WINDOW))
;        (SETQ *IO-BUFFER* (WINDOW-IO-BUFFER *WINDOW*))
         (SETQ *POINT* (WINDOW-POINT WINDOW)
               *MARK* (WINDOW-MARK WINDOW))
         (SEND INTERVAL :SELECT)
         (SETQ *TYPEOUT-WINDOW* (WINDOW-TYPEOUT-WINDOW WINDOW)
               *TERMINAL-IO* *TYPEOUT-WINDOW*)
         (SETQ *MODE-LINE-WINDOW* (SEND WINDOW :MODE-LINE-WINDOW))
         (SETQ *TYPEIN-WINDOW* (SEND *MODE-LINE-WINDOW* :TYPEIN-WINDOW))
         (SETQ *MINI-BUFFER-WINDOW* (SEND *MODE-LINE-WINDOW* :MINI-BUFFER-WINDOW))
;        (SEND *MODE-LINE-WINDOW* :SET-IO-BUFFER *IO-BUFFER*)
         (AND SELECT-P (SELECT-WINDOW WINDOW))))
  (SETQ *WINDOW-LIST* (FRAME-EXPOSED-WINDOWS)))

;;; Called when a window is selected by editor command, to inform the
;;; sheet.  Note that this must NOT send a SELECT-WINDOW request back to
;;; the editor!  (Consider interaction with macros, type-ahead)
(DEFUN SELECT-WINDOW (&OPTIONAL (WINDOW *WINDOW*))
  "Reselect *WINDOW* if some other window has been selected.
This is used if some other window has been selected in the window system
but *WINDOW* has not been changed."
  (UNLESS (EQ WINDOW *WINDOW*)
    (FERROR "~S is not the editor's selected window." WINDOW))
  (SEND WINDOW :RESELECT))

(DEFMETHOD (DISPLAYER :DEFAULT :RESELECT) ()
  NIL)

(DEFMETHOD (WINDOW :RESELECT) ()
  (AND (NEQ SELF TV:SELECTED-WINDOW)
       (LET ((TOP-W (SEND SELF :TOP-OF-EDITOR-HIERARCHY)))
         (IF (EQ SELF TOP-W)
             (SEND SELF :SELECT)
           (SEND TOP-W :SET-SELECTION-SUBSTITUTE SELF)))))

;;; This makes WINDOW edit NEW-INTERVAL,
;;; as part of selecting buffers, etc.
;This only does things relevant to changing WINDOW's selected buffer;
;it does not do anything about possibly making BUFFER be the editor's selected buffer
;if WINDOW is the selected window.  This must be done by the caller.
(DEFMETHOD (DISPLAYER :SET-INTERVAL-INTERNAL) (NEW-INTERVAL)
  (SET-WINDOW-INTERVAL-1 SELF NEW-INTERVAL))

(DEFUN SET-WINDOW-INTERVAL-1 (WINDOW INTERVAL)
  (SETF (WINDOW-INTERVAL WINDOW) INTERVAL)
  (WHEN (WINDOW-POINT WINDOW)
    (FLUSH-BP (WINDOW-POINT WINDOW))
    (FLUSH-BP (WINDOW-MARK WINDOW))
    (FLUSH-BP (WINDOW-START-BP WINDOW)))
  (LET ((FIRST-BP (INTERVAL-FIRST-BP INTERVAL)))
    (SETF (WINDOW-POINT WINDOW) (COPY-BP FIRST-BP :NORMAL))
    (SETF (WINDOW-MARK WINDOW) (COPY-BP FIRST-BP :NORMAL))
    (SETF (WINDOW-START-BP WINDOW) (COPY-BP FIRST-BP :NORMAL)))
  (WHEN (EQ WINDOW *WINDOW*)
    (SETQ *POINT* (WINDOW-POINT WINDOW)
          *MARK* (WINDOW-MARK WINDOW))))

(DEFMETHOD (ZMACS-WINDOW :SET-INTERVAL-INTERNAL) (BUFFER)
  (IF (NULL POINT)
      (SET-WINDOW-INTERVAL-1 SELF BUFFER)
    (MOVE-BP (BUFFER-SAVED-POINT INTERVAL) POINT)
    (MOVE-BP (BUFFER-SAVED-MARK INTERVAL) MARK)
    (MOVE-BP (BUFFER-SAVED-WINDOW-START-BP INTERVAL) START-BP)
    (SETF (BUFFER-SAVED-FONT-ALIST INTERVAL) FONT-ALIST))
  (SETF (WINDOW-INTERVAL SELF) BUFFER)
  (MOVE-BP POINT (BUFFER-SAVED-POINT BUFFER))
  (MOVE-BP MARK (BUFFER-SAVED-MARK BUFFER))
  (MOVE-BP START-BP (BUFFER-SAVED-WINDOW-START-BP BUFFER))
  (REDEFINE-WINDOW-OVERPRINTING-FLAG SELF (SEND BUFFER :GET-ATTRIBUTE ':BACKSPACE))
  (REDEFINE-WINDOW-TAB-NCHARS SELF (SEND BUFFER :GET-ATTRIBUTE ':TAB-WIDTH))
  (REDEFINE-FONTS SELF (BUFFER-SAVED-FONT-ALIST BUFFER) (SEND BUFFER :GET-ATTRIBUTE ':VSP))
  (MUST-REDISPLAY SELF DIS-TEXT)
  (CHANGE-WINDOW-LABEL SELF))

(DEFMETHOD (DISPLAYER :FUNCALL-EDITOR-CLOSURE) (&REST ARGS)
  (APPLY EDITOR-CLOSURE ARGS))

(DEFMETHOD (DISPLAYER :ZWEI-WINDOW) () SELF)

(DEFMETHOD (DISPLAYER :EDITOR-WINDOWS) () (LIST SELF))

(DEFMETHOD (DISPLAYER :FONT-ALIST) () NIL)

(DEFMETHOD (DISPLAYER :AFTER :INIT) (IGNORE)
;;;Should initialize PLINE-LINE-ARRAY, etc., here,
;;;but N-PLINES has not been computed properly yet.
  (FUNCALL EDITOR-CLOSURE SELF :SET-INTERVAL-INTERNAL INTERVAL))

(DEFMETHOD (DISPLAYER :FINISH-DELAYED-SELECT) ()
  NIL)

(DEFMETHOD (DISPLAYER :BEFORE :EDIT) (&REST IGNORE)
  (SETQ BASE-TICK *TICK*))

(DEFMETHOD (DISPLAYER :TOP-LEVEL-P) () NIL)

(DEFMETHOD (DISPLAYER :BEFORE :INIT) (INIT-PLIST)
  (WHEN (SYMBOLP INTERVAL)
    (SETQ INTERVAL (MAKE-INSTANCE INTERVAL))
    (SEND INTERVAL :SEND-IF-HANDLES :ACTIVATE))
  (OR (VARIABLE-BOUNDP EDITOR-CLOSURE)
      (SETQ EDITOR-CLOSURE
            (LET ((VARS (OR (GET INIT-PLIST ':EDITOR-CLOSURE-VARIABLES)
                            EDITOR-CLOSURE-VARIABLES)))
              (MAKE-EDITOR-CLOSURE VARS
                                   (UNLESS (ASSQ '*TYPEOUT-WINDOW* VARS) SELF))))))

(DEFMETHOD (WINDOW :AFTER :INIT) (IGNORE)
  (SETQ N-PLINES (FLOOR (TV:SHEET-INSIDE-HEIGHT) TV:LINE-HEIGHT))
  (LET ((PAREN-BLINKER (TV:MAKE-BLINKER SELF 'TV:CHARACTER-BLINKER
                                        :VISIBILITY NIL :HALF-PERIOD 8
                                        :DESELECTED-VISIBILITY :OFF
                                        :FONT (TV:SHEET-CURRENT-FONT SELF) :CHAR #/()))
    (SETQ POINT-BLINKER (CAR (LAST TV:BLINKER-LIST))
          SPECIAL-BLINKER-LIST `((BLINK-MATCHING-PAREN . ,PAREN-BLINKER))))
  (SETQ PLINE-LINE-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-FROM-INDEX-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-TO-INDEX-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-TICK-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-MARKING-LEFT-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-MARKING-WIDTH-ARRAY (MAKE-ARRAY N-PLINES))
  (SETQ PLINE-TEXT-WIDTH-ARRAY (MAKE-ARRAY N-PLINES)))

(DEFMETHOD (TOP-LEVEL-DISPLAYER-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (UNLESS (VARIABLE-BOUNDP EDITOR-CLOSURE)
    (LET ((*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
          (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
          (*QUERY-IO* SYN-TYPEIN-WINDOW-IO)
          (*COMTAB* (GET INIT-PLIST ':COMTAB))
          (*MODE-LINE-LIST* (OR (GET INIT-PLIST ':MODE-LINE-LIST)
                                '("ZWEI " "(" *MODE-NAME-LIST* ")"))))
      (SETQ EDITOR-CLOSURE
            (MAKE-EDITOR-CLOSURE (OR (GET INIT-PLIST ':EDITOR-CLOSURE-VARIABLES)
                                     TOP-LEVEL-EDITOR-CLOSURE-VARIABLES)
                                 NIL)))))

(DEFMETHOD (TOP-LEVEL-DISPLAYER-MIXIN :AFTER :INIT) (IGNORE)
  (FUNCALL EDITOR-CLOSURE #'INITIALIZE-TOP-LEVEL-EDITOR SELF))

(DEFMETHOD (TOP-LEVEL-DISPLAYER-MIXIN :BEFORE :EDIT) (&REST IGNORE)
  (INITIALIZE-FOR-USER))

;; See (:METHOD CONVERSE-FRAME :BEFORE :EXPOSE) for the use of this function.
(DEFUN INITIALIZE-FOR-USER ()
  (UNLESS *INITIALIZED-FOR-USER*
    (TURN-ON-MODE *MAJOR-MODE*)
    (SETQ *INITIALIZED-FOR-USER* T)))

(DEFMETHOD (TOP-LEVEL-DISPLAYER-MIXIN :BEFORE :KILL) ()
  (SETQ *EDITORS-WHOSE-MODES-TO-RESET*
        (DELQ EDITOR-CLOSURE *EDITORS-WHOSE-MODES-TO-RESET*)))

(DEFMETHOD (TOP-LEVEL-DISPLAYER-MIXIN :TOP-LEVEL-P) ()
  T)

(DEFUN WINDOW-LIST ()
  *WINDOW-LIST*)

(DEFUN FRAME-EXPOSED-WINDOWS ()
  "Return a list of the exposed editor windows in this frame, in descending order."
  (SEND *WINDOW* :EDITOR-WINDOWS))

(DEFUN WINDOW-READY-P (WINDOW &OPTIONAL (CURRENT-WINDOW-SPECIAL T))
  "T if WINDOW is ready for redisplay now.
The value does not depend on whether WINDOW needs redisplay.
Unless CURRENT-WINDOW-SPECIAL is NIL, the current window is always considered ready."
  (SEND WINDOW :READY-FOR-REDISPLAY-P CURRENT-WINDOW-SPECIAL))

(DEFMETHOD (DISPLAYER :READY-FOR-REDISPLAY-P) (IGNORE)
  T)

;;; Make a window ready for redisplay.
;;; This deactivates the window's typeout stream.
(DEFUN PREPARE-WINDOW-FOR-REDISPLAY (WINDOW)
  (SEND WINDOW :PREPARE-FOR-REDISPLAY))

(DEFMETHOD (DISPLAYER :PREPARE-FOR-REDISPLAY) ()
  NIL)

;;; Called when the label of the window might have changed
(DEFUN CHANGE-WINDOW-LABEL (WINDOW)
  "Tells WINDOW to recompute what should go in its label."
  (SEND WINDOW :RECOMPUTE-LABEL))

(DEFMETHOD (DISPLAYER :DEFAULT :RECOMPUTE-LABEL) ()
  NIL)

(DEFUN MODE-LINE-LIST ()
  "Return the list being used to drive the mode line window."
  *MODE-LINE-LIST*)

(DEFUN SET-MODE-LINE-LIST (NEW-MODE-LINE-LIST)
  "Set the list being used to drive the mode line window."
  (SETQ *MODE-LINE-LIST* NEW-MODE-LINE-LIST))

;;; This takes a window, and creates a new window on top of it in the default font.
(DEFVAR *OVERLYING-WINDOW-LIST* NIL)            ;This buys a little speed

(DEFUN CREATE-OVERLYING-WINDOW (WINDOW &AUX NEW-WINDOW)
  "Create another ZWEI window whose screen area is the same as WINDOW's.
Reuses an old one if one was ever made before for this window."
  (IF (SETQ NEW-WINDOW (CDR (ASSQ WINDOW *OVERLYING-WINDOW-LIST*)))
      (SEND NEW-WINDOW :REUSE-OVERLYING-WINDOW WINDOW)
    (SETQ NEW-WINDOW (SEND WINDOW :CREATE-OVERLYING-WINDOW))
    (PUSH (CONS WINDOW NEW-WINDOW) *OVERLYING-WINDOW-LIST*))
  NEW-WINDOW)

(DEFMETHOD (WINDOW :REUSE-OVERLYING-WINDOW) (OVER-WINDOW)
  (LEXPR-SEND SELF :SET-EDGES (MULTIPLE-VALUE-LIST (SEND OVER-WINDOW :EDGES))))

(DEFMETHOD (WINDOW :CREATE-OVERLYING-WINDOW) ()
  (MAKE-INSTANCE 'ZWEI-OVERLYING-WINDOW :FOR-WINDOW SELF
                                        :EDGES-FROM SELF
                                        :SAVE-BITS NIL
                                        :IO-BUFFER TV:IO-BUFFER
                                        :SUPERIOR TV:SUPERIOR))

(DEFUN BUFFER-POINT (BUFFER)
  "Return the value of POINT for BUFFER.
If BUFFER is in some window, that window's POINT is used.
Otherwise BUFFER's saved point is used."
  (SEND BUFFER :POINT))

(DEFUN BUFFER-MARK (BUFFER)
  "Return the value of MARK for BUFFER.
If BUFFER is in some window, that window's MARK is used.
Otherwise BUFFER's saved mark is used."
  (SEND BUFFER :MARK))

(DEFUN MUST-REDISPLAY-BUFFER (BUFFER DEGREE &OPTIONAL LINE INDEX)
  "Redisplay all windows that are displaying BUFFER."
  (DOLIST (WINDOW (SEND BUFFER :WINDOWS))
    (MUST-REDISPLAY WINDOW DEGREE LINE INDEX))
  NIL)

(DEFMETHOD (INTERVAL :POINT) () (POINT))

(DEFMETHOD (INTERVAL :MARK) () (MARK))

(DEFMETHOD (INTERVAL :WINDOWS) ()
  (LIST *WINDOW*))

(DEFMETHOD (INTERVAL :POSSIBLE-WINDOWS) ()
  (LIST *WINDOW*))

(DEFMETHOD (INTERVAL :OTHER-WINDOWS) (WINDOW) WINDOW NIL)

(DEFMETHOD (INTERVAL :PATHNAME) () NIL)

(DEFMETHOD (INTERVAL :GENERIC-PATHNAME) () NIL)

(DEFMETHOD (ZMACS-BUFFER :WINDOWS) ()
  (SUBSET #'(LAMBDA (WINDOW) (EQ SELF (WINDOW-INTERVAL WINDOW)))
          *ALL-ZMACS-WINDOWS*))

(DEFMETHOD (ZMACS-BUFFER :OTHER-WINDOWS) (WINDOW)
  (DECLARE (SPECIAL WINDOW))
  (SUBSET #'(LAMBDA (W) (AND (NEQ W WINDOW) (EQ SELF (WINDOW-INTERVAL W))))
          *ALL-ZMACS-WINDOWS*))

(DEFMETHOD (ZMACS-BUFFER :POSSIBLE-WINDOWS) ()
  *ALL-ZMACS-WINDOWS*)

(DEFMETHOD (ZMACS-BUFFER :POINT) ()
  (DOLIST (WINDOW (SEND SELF :WINDOWS) SAVED-POINT)
    (IF (EQ (WINDOW-INTERVAL WINDOW) SELF)
        (RETURN (WINDOW-POINT WINDOW)))))

(DEFMETHOD (ZMACS-BUFFER :MARK) ()
  (DOLIST (WINDOW (SEND SELF :WINDOWS) SAVED-MARK)
    (IF (EQ (WINDOW-INTERVAL WINDOW) SELF)
        (RETURN (WINDOW-MARK WINDOW)))))

(DEFMETHOD (ZMACS-BUFFER :ACTIVATE) (&OPTIONAL ASK-FOR-NEW-NAME)
  (WITHOUT-INTERRUPTS
    ;; First, if buffer is not already on name alist, put it on,
    ;; getting a new name if necessary and appropriate.
    (UNLESS (RASSQ SELF *ZMACS-BUFFER-NAME-ALIST*)
      (DO ()
          ((NOT (ASS #'EQUALP NAME *ZMACS-BUFFER-NAME-ALIST*)))
        (IF ASK-FOR-NEW-NAME
            (WITHOUT-INTERRUPTS
              (IF (AND PATHNAME
                       (NOT (BUFFER-PATHNAME
                              (CDR (ASS #'EQUALP NAME *ZMACS-BUFFER-NAME-ALIST*)))))
                  ;; This is visiting a file and the other is not.
                  (SEND (CDR (ASS #'EQUALP NAME *ZMACS-BUFFER-NAME-ALIST*))
                        :RENAME (DO ((NAME1 NAME)
                                     (FIRST T NIL))
                                    (())
                                  (SETQ NAME1
                                        (TYPEIN-LINE-READLINE
                                          (IF FIRST
                                              "There is a non-file buffer ~A.  Rename it to: "
                                              "~A is in use too.  Try again.")
                                          NAME1))
                                  (UNLESS (ASS #'EQUALP NAME1 *ZMACS-BUFFER-NAME-ALIST*)
                                    (RETURN NAME1))))
                (SETQ NAME
                      (TYPEIN-LINE-READLINE
                        "There is already a buffer named ~A.  Specify another name:"
                        NAME))))
          (BARF "There is already a buffer named ~A." NAME)))
      (PUSH (CONS NAME SELF) *ZMACS-BUFFER-NAME-ALIST*)
;>> I don't understand what use this is. Mly
;      (SETQ *ZMACS-BUFFER-NAME-ALIST*
;           (COPY-ALIST *ZMACS-BUFFER-NAME-ALIST*))
;     (DOLIST (ELT *ZMACS-BUFFER-NAME-ALIST*)
;       (SETF (CAR ELT) (SI:COPY-OBJECT (CAR ELT))))
      )
    ;; Put the buffer on the other lists, if not already there.
    (UNLESS (MEMQ SELF *ZMACS-BUFFER-LIST*)
      (SETQ *ZMACS-BUFFER-LIST* (APPEND *ZMACS-BUFFER-LIST* (LIST SELF)))
      ;;Append to the histories of all the windows
      ;; The histories contain the same elements as *ZMACS-BUFFER-LIST*, though in different
      ;; orders. Thus they are not strictly historical, since they include buffers
      ;; which have never been selected. This is more convenient, though.
      (DOLIST (W *ALL-ZMACS-WINDOWS*)
        (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
          (APPEND-REMOVE-ON-HISTORY SELF HISTORY))))))

;; Should only be used inside an editor closure.
(DEFMETHOD (INTERVAL :SELECT) ()
  (SETQ *INTERVAL* SELF))

(DEFMETHOD (ZMACS-BUFFER :SELECT) ()
  (MAKE-BUFFER-CURRENT SELF))

(DEFMETHOD (INTERVAL :KILL) () NIL)

(DEFMETHOD (ZMACS-BUFFER :AFTER :KILL) ()
  (POINT-PDL-PURGE SELF)
  (WITHOUT-INTERRUPTS
    (LET ((ELEMENT (SI:RASSOC-EQUAL SELF *ZMACS-BUFFER-NAME-ALIST*)))
      (WHEN ELEMENT
        (SETQ *ZMACS-BUFFER-NAME-ALIST* (DELQ ELEMENT *ZMACS-BUFFER-NAME-ALIST*))))
    (SETQ *ZMACS-BUFFER-LIST* (REMQ SELF *ZMACS-BUFFER-LIST*)))
  (DOLIST (W *ALL-ZMACS-WINDOWS*)
    (WITHOUT-INTERRUPTS
      (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
        (DELETE-FROM-HISTORY SELF HISTORY))))
  (DOLIST (NODE INFERIORS)
    (LET ((SYM (SECTION-NODE-NAME NODE)))
      (UNLESS (STRINGP SYM)
        (SETF (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS)
              (DEL #'(LAMBDA (BUF PROP)
                       (EQ BUF (CAR PROP)))
                   SELF (SI:FUNCTION-SPEC-GET SYM 'ZMACS-BUFFERS))))))
  ;; Any other windows lying around should not have pointers to this window.
  ;; Make them redisplay; that will check for this.
  (DOLIST (WINDOW (SEND SELF :WINDOWS))
    (SEND WINDOW :FORCE-KBD-INPUT '(REDISPLAY)))
  (SETF (GETF SI:PROPERTY-LIST :KILLED) T)
  (setq *last-ztop-buffer* (delete self *last-ztop-buffer*))
  T)

(DEFMETHOD (ZMACS-BUFFER :FIX-WINDOW-INTERVAL) (WINDOW)
  (SEND WINDOW :SET-INTERVAL (CAR *ZMACS-BUFFER-LIST*)))

(DEFMETHOD (ZMACS-BUFFER :RENAME) (NEW-NAME)
  (AND (NOT (STRING-EQUAL NEW-NAME (BUFFER-NAME SELF)))     ;Allow "foo" -> "FOO"
       (FIND-BUFFER-NAMED NEW-NAME)
       (BARF "There is already another buffer named ~A" NEW-NAME))
  (SETQ NAME NEW-NAME)
  (LET ((ELEMENT (SI:RASSOC-EQUAL SELF *ZMACS-BUFFER-NAME-ALIST*)))
    (WHEN ELEMENT (SETF (CAR ELEMENT) NEW-NAME)))
  (IF (EQ SELF *INTERVAL*)
      (UPDATE-BUFFER-NAMES SELF))
  (DOLIST (WINDOW (SEND SELF :WINDOWS))
    (CHANGE-WINDOW-LABEL WINDOW)))

(DEFMETHOD (INTERVAL :REVERT) (&REST IGNORE)
  NIL)

(DEFMETHOD (FILE-BUFFER :REVERT) (&OPTIONAL NEW-PATHNAME CONNECT-FLAG
                                  SELECT-FLAG QUIETLY-FLAG)
  (REVERT-FILE-BUFFER SELF NEW-PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG))

(DEFMETHOD (ZMACS-BUFFER :REVERT) (&OPTIONAL NEW-PATHNAME CONNECT-FLAG
                                   SELECT-FLAG QUIETLY-FLAG)
  (FUNCALL (OR (GET (BUFFER-MODE SELF) 'MAJOR-MODE-REVERT-FUNCTION)
               #'REVERT-FILE-BUFFER)
           SELF NEW-PATHNAME CONNECT-FLAG SELECT-FLAG QUIETLY-FLAG))

(DEFVAR *ZWEI-BUFFER-LIST-NAMES* '(*ZMACS-BUFFER-LIST*)
  "List of symbols whose values are lists of editor buffers to possibly save.
Each element of each symbol's value must handle operations :MODIFIED-P and :SAVE.")

(DEFUN SAVE-ALL-FILES (&AUX MODIFIED-BUFFERS)
  "Call this while not in the editor to offer to save each modified buffer."
  (DOLIST (LIST-NAME *ZWEI-BUFFER-LIST-NAMES*)
    (SETQ MODIFIED-BUFFERS ())
    (DOLIST (BUFFER (SYMEVAL LIST-NAME))
      (IF (BUFFER-MODIFIED-P BUFFER) (PUSH BUFFER MODIFIED-BUFFERS)))
    (WHEN MODIFIED-BUFFERS
      (FORMAT T "~&Buffers in ~S:" LIST-NAME)
      (DOLIST (BUFFER MODIFIED-BUFFERS)
        (WHEN (FQUERY NIL "Save buffer ~A ? " (BUFFER-NAME BUFFER))
          (LET ((*WINDOW* NIL)
                (*INTERVAL* NIL)
                (*TYPEOUT-WINDOW* *STANDARD-OUTPUT*)
                (*TYPEIN-WINDOW* *STANDARD-OUTPUT*)
                (*NUMERIC-ARG-P* NIL))
            (SEND BUFFER :SAVE)))))))

(DEFMETHOD (ZMACS-BUFFER :SAVE) ()
  (SAVE-BUFFER SELF))

(DEFUN SECTIONIZE-BUFFER (BUFFER)
  "Compute sectionization of BUFFER.
Ignores and discards any old sectionization."
  (SEND BUFFER :SECTIONIZE))

(DEFUN RESECTIONIZE-BUFFER (BUFFER &OPTIONAL START-NODE END-NODE)
  "Updates sectionization of changed parts of BUFFER.
If START-NODE and END-NODE are specified, we update only that range."
  (SEND BUFFER :RESECTIONIZE START-NODE END-NODE))

(DEFMETHOD (INTERVAL :SECTIONIZE) ()
  NIL)

(DEFMETHOD (INTERVAL :RESECTIONIZE) (&OPTIONAL IGNORE IGNORE)
  NIL)

(DEFMETHOD (ZMACS-BUFFER :SECTIONIZE) ()
  (IF (GETF SI:PROPERTY-LIST :DONT-SECTIONIZE)
      NIL
    (SECTIONIZE-FILE-BUFFER SELF *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS)))

(DEFMETHOD (ZMACS-BUFFER :RESECTIONIZE) (&OPTIONAL START-NODE END-NODE)
  (IF (GETF SI:PROPERTY-LIST :DONT-SECTIONIZE)
      NIL
    (RESECTIONIZE-FILE-BUFFER SELF *ZMACS-COMPLETION-AARRAY* 'ZMACS-BUFFERS
                              START-NODE END-NODE)))


;; Is a buffer modified?
;(DEFUN BUFFER-MUNGED-P (BUFFER)
;  "T if BUFFER has been changed since last read or written.
;Always returns NIL for special buffers.
;An error if BUFFER is not visiting a file."
;  (SEND BUFFER :MUNGED-P))

;(DEFMETHOD (FILE-BUFFER :MUNGED-P) ()
;  (AND (SYMBOLP FILE-ID)
;       (FERROR ":MUNGED-P on a buffer that has not been read or written."))
;  (AND (NOT (NODE-SPECIAL-TYPE SELF))
;       (> TICK FILE-TICK)))

(DEFUN BUFFER-NEEDS-SAVING-P (BUFFER)
  "T if BUFFER is a file buffer and has been changed since last read, written or created.
Always NIL for special buffers and buffers not visiting files."
  (SEND BUFFER :NEEDS-SAVING-P))

(DEFUN BUFFER-MODIFIED-P (BUFFER)
  "T if BUFFER has been changed since last read, written or created.
Changing the buffer's visited pathname counts as changing it."
  (SEND BUFFER :MODIFIED-P))

(DEFMETHOD (FILE-BUFFER :NEEDS-SAVING-P) ()
  (AND PATHNAME FILE-ID
       (NOT (NODE-SPECIAL-TYPE SELF))
       (SEND SELF :MODIFIED-P)))

(DEFMETHOD (FILE-BUFFER :MODIFIED-P) ()
  (OR (AND PATHNAME (EQ FILE-ID T))
      (> TICK FILE-TICK)))

(DEFMETHOD (NODE :MODIFIED-P) ()
  (> TICK (SEND *WINDOW* :BASE-TICK)))

(DEFUN BUFFER-READ-ONLY-P (BUFFER)
  "T if BUFFER is read-only."
  (NODE-READ-ONLY-P BUFFER))

(DEFUN MAKE-BUFFER-READ-ONLY (BUFFER)
  "Make BUFFER be read-only."
  (SEND BUFFER :SET-READ-ONLY-P T))

(DEFUN MAKE-BUFFER-NOT-READ-ONLY (BUFFER)
  "Make BUFFER not be read-only."
  (SEND BUFFER :SET-READ-ONLY-P NIL))


(DEFUN NOT-MODIFIED (BUFFER)
  "Mark BUFFER and its sections as not modified since read or saved."
  (SEND BUFFER :NOT-MODIFIED)
  DIS-NONE)

(DEFMETHOD (INTERVAL :NOT-MODIFIED) ()
  NIL)

(DEFMETHOD (NODE :NOT-MODIFIED) ()
  (DOLIST (SUBNODE INFERIORS)
    (SEND SUBNODE :NOT-MODIFIED)))

(DEFMETHOD (SECTION-NODE :AFTER :NOT-MODIFIED) ()
  (SETQ COMPILE-TICK *TICK*))

(DEFMETHOD (FILE-BUFFER :AFTER :NOT-MODIFIED) ()
  (send self :remprop :cts-done)                ; $$$ cts control <17-Nov-88 smh>
  (SETQ FILE-TICK *TICK*))

(DEFUN GET-ATTRIBUTE (PLIST ATTRIBUTE)
  (GET PLIST ATTRIBUTE (EVAL (GET ATTRIBUTE 'DEFAULT-ATTRIBUTE-VALUE))))

(DEFMETHOD (NODE :GET-ATTRIBUTE) (ATTRIBUTE &OPTIONAL (DEFAULT NIL DEFAULT-SPECIFIED-P))
  (GETF SI:PROPERTY-LIST ATTRIBUTE
        (IF DEFAULT-SPECIFIED-P DEFAULT (EVAL (GET ATTRIBUTE 'DEFAULT-ATTRIBUTE-VALUE)))))

(DEFMETHOD (NODE :SET-ATTRIBUTE) (ATTRIBUTE VALUE &OPTIONAL SET-TEXT-TOO)
  (DECLARE (IGNORE SET-TEXT-TOO))
  (SETF (GETF SI:PROPERTY-LIST ATTRIBUTE) VALUE)
  ;; Make sure, if the entire list of attributes is reinitialized,
  ;; that this one gets flushed.
  (SETF (GETF (GETF SI:PROPERTY-LIST 'FS:LAST-FILE-PLIST) ATTRIBUTE) VALUE)
  ;; Re-run the mode hook, which may act depending on value of this attribute.
  (IF (EQ SELF *INTERVAL*)
      (LET ((HOOK (GET *MAJOR-MODE* 'MODE-HOOK-SYMBOL)))
        (AND HOOK (BOUNDP HOOK) (FUNCALL (SYMBOL-VALUE HOOK))))))

(DEFMETHOD (FILE-BUFFER :AFTER :SET-ATTRIBUTE) (ATTRIBUTE VALUE &OPTIONAL SET-TEXT-TOO)
  (LET ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM SELF))))
    (AND (NOT (EQUAL VALUE (GETF ATTRIBUTES ATTRIBUTE)))
         ;; Ok, the new value doesn't match what's in the text.
         (OR (EQ SET-TEXT-TOO T)
             (AND (EQ SET-TEXT-TOO ':QUERY)
                  (FQUERY NIL "Change the -*- line of the text as well? ")))
         (PROGN
           ;; Put the new value in with what we got from the text;
           ;; if the new value is the default, delete it instead.
           (IF (OR (MEMQ ATTRIBUTE '(:BASE :MODE :PACKAGE :SYNTAX :READTABLE))
                   (NOT (EQUAL VALUE (EVAL (GET ATTRIBUTE 'DEFAULT-ATTRIBUTE-VALUE)))))
               (SETF (GETF ATTRIBUTES ATTRIBUTE) VALUE)
             (REMF ATTRIBUTES ATTRIBUTE)
             ;; Cause Update Attribute List to forget this one too.
             (REMF (GETF SI:PROPERTY-LIST 'FS::LAST-FILE-PLIST) ATTRIBUTE))
           ;; Now we have an attribute list to store in the file.
           (STORE-ATTRIBUTE-LIST SELF ATTRIBUTES)
           (MUST-REDISPLAY-BUFFER SELF DIS-TEXT)))))

(DEFMETHOD (NODE :ATTRIBUTE-BINDINGS) ()
  (FS:FILE-ATTRIBUTE-BINDINGS SELF))

(DEFPROP :TAB-WIDTH 8 DEFAULT-ATTRIBUTE-VALUE)
(DEFPROP :VSP *VSP* DEFAULT-ATTRIBUTE-VALUE)

(DEFMETHOD (NODE :UPDATE-COMPILE-TICK) () NIL)

(DEFMETHOD (SECTION-NODE :UPDATE-COMPILE-TICK) ()
  (SETQ COMPILE-TICK *TICK*))

(DEFF BUFFER-MAJOR-MODE 'BUFFER-MODE)

(DEFUN BUFFER-MODE (BUFFER)
  "Return the keyword for BUFFER's major mode."
  (IF (EQ BUFFER *INTERVAL*)
      *MAJOR-MODE*
    (SEND BUFFER :MAJOR-MODE)))

(DEFMETHOD (INTERVAL :MAJOR-MODE) ()
  *MAJOR-MODE*)

(DEFMETHOD (ZMACS-BUFFER :MAJOR-MODE) ()
  (IF (AND *WINDOW* (EQ (WINDOW-INTERVAL *WINDOW*) SELF))
      *MAJOR-MODE*
    SAVED-MAJOR-MODE))

(DEFMETHOD (INTERVAL :SET-MAJOR-MODE) (NEW-MODE)
  (WHEN (EQ SELF *INTERVAL*)
    (TURN-OFF-MODE *MAJOR-MODE*)
    (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
      (TURN-OFF-MODE MODE))
    (TURN-ON-MODE NEW-MODE)))

(DEFMETHOD (ZMACS-BUFFER :BEFORE :SET-MAJOR-MODE) (NEW-MODE)
  (SETQ SAVED-MODE-LIST
        (DELQ (ASSQ SAVED-MAJOR-MODE
                    SAVED-MODE-LIST)
              SAVED-MODE-LIST))
  (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
    (SETQ SAVED-MODE-LIST
          (DELQ (ASSQ MODE SAVED-MODE-LIST)
                SAVED-MODE-LIST)))
  (SETQ SAVED-MAJOR-MODE NEW-MODE))

(DEFMETHOD (INTERVAL :SET-SAVED-FONT-ALIST) (IGNORE) NIL)

;; :FIND-SPECIAL-BUFFER is used by commands such as DIRED and MAIL.
;; It is sent to the window to ask it to select a buffer of the
;; appropriate kind for the kind of editing to be done.
;; A stand-alone window will do nothing, since it always points
;; at the suitable buffer.
;; a ZMACS window will switch buffers.

;; :FIND-SPECIAL-BUFFER's arguments are:
;; TYPE - the type of window desired.  This is a keyword, normally designating a mode.
;;   For example, :DIRED, :EDIT-BUFFERS, :MAIL
;; NEW-P - T means a "clean" buffer is desired, NIL means an "in use" buffer is desired.
;;   C-U C-X M would ask for an "in use" buffer while C-X M would ask for a clean one.
;;   A "clean" special buffer is just one which is not MODIFIED-P.
;;   NEW-P can be :ALWAYS meaning always make a new buffer.
;; NAME is a prefix to generate a name from.
;; MAKE-CURRENT (default T) says select the buffer.
;;   Otherwise just return it.
;; The final arg is *DEFAULT-MAJOR-MODE*.
;;   This controls the major mode to use if a new buffer needs to be made.
;;   It defaults to TYPE.

;; :EXIT-SPECIAL-BUFFER is what to do in an "exit" command
;; such as Q in DIRED or End in Mail.
;; In ZMACS it deselects the special buffer and optionally marks it "clean".
;; The arguments are:
;; MARK-CLEAN - T => NOT-MODIFIED this buffer.
;; BUFFER-BEING-EXITED - the special buffer.
;;   The default for this is the buffer that is now current.
;; In other kinds of windows this operation probably
;; does a throw to exit the function such as MAIL or DIRED.

(DEFMETHOD (WINDOW :FIND-SPECIAL-BUFFER) (&REST IGNORE)
  *INTERVAL*)

(DEFMETHOD (ZMACS-WINDOW :FIND-SPECIAL-BUFFER) (TYPE NEW-P NAME
                                                &OPTIONAL (MAKE-CURRENT T)
                                                (MAJOR-MODE TYPE)
                                                &AUX BUFFER PROSPECT)
  (DECLARE (IGNORE MAJOR-MODE))
  (SETQ BUFFER
        (OR (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
              (WHEN (EQ (NODE-SPECIAL-TYPE BUFFER) TYPE)
                (SETQ PROSPECT BUFFER)
                (AND (NEQ NEW-P ':ALWAYS)
                     (EQ (NOT NEW-P) (SEND BUFFER :MODIFIED-P))
                     (RETURN BUFFER))))
            (IF (AND PROSPECT (NOT NEW-P))
                PROSPECT
              (SETQ PROSPECT (MAKE-INSTANCE 'ZMACS-BUFFER
                               :NAME (LOOP FOR I FROM 1
                                           AS BUFNAM = (FORMAT NIL "*~A-~D*" NAME I)
                                           UNLESS (FIND-BUFFER-NAMED BUFNAM)
                                           RETURN BUFNAM)))
              (SETF (NODE-SPECIAL-TYPE PROSPECT) TYPE)
              PROSPECT)))
  (IF MAKE-CURRENT (MAKE-BUFFER-CURRENT BUFFER))
;  (IF MAKE-CURRENT (FUNCALL (WINDOW-EDITOR-CLOSURE SELF) #'MAKE-BUFFER-CURRENT BUFFER))
  BUFFER)


(DEFMETHOD (ZMACS-WINDOW :EXIT-SPECIAL-BUFFER) (&OPTIONAL MARK-CLEAN BUFFER-BEING-EXITED)
  (LET ((SPECIAL-BUFFER (OR BUFFER-BEING-EXITED *INTERVAL*)))
    (AND MARK-CLEAN (NOT-MODIFIED SPECIAL-BUFFER))
    (IF (EQ SPECIAL-BUFFER *INTERVAL*)
        (MAKE-BUFFER-CURRENT (OR (CAR (MEM 'NEQ SPECIAL-BUFFER (HISTORY-LIST BUFFER-HISTORY)))
                                 *INTERVAL*)))
    (WITHOUT-INTERRUPTS
      (DOLIST (W *ALL-ZMACS-WINDOWS*)
        (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
          (APPEND-REMOVE-ON-HISTORY SPECIAL-BUFFER HISTORY)))
      (SETQ *ZMACS-BUFFER-LIST* (APPEND (REMQ SPECIAL-BUFFER *ZMACS-BUFFER-LIST*)
                                        (LIST SPECIAL-BUFFER))))
    (POINT-PDL-PURGE SPECIAL-BUFFER))
  DIS-TEXT)

;;;; Overprinting flag and tab width of windows.

;In DEFS
;(DEFSUBST WINDOW-OVERPRINTING-FLAG (WINDOW)
;  (SEND WINDOW :OVERPRINTING-FLAG))

(DEFUN REDEFINE-WINDOW-OVERPRINTING-FLAG (WINDOW OVERPRINTING-FLAG)
  (SEND WINDOW :SET-OVERPRINTING-FLAG OVERPRINTING-FLAG))

(DEFMETHOD (DISPLAYER :DEFAULT :OVERPRINTING-FLAG) () NIL)

(DEFMETHOD (DISPLAYER :DEFAULT :SET-OVERPRINTING-FLAG) () NIL)

(DEFMETHOD (WINDOW :OVERPRINTING-FLAG) ()
  (ZEROP (TV:SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG)))

(DEFMETHOD (WINDOW :SET-OVERPRINTING-FLAG) (NEW-OVERPRINTING-FLAG)
  (LET* ((OLD (TV:SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG))
         (NEW (IF NEW-OVERPRINTING-FLAG 0 1)))
    (UNLESS (EQ OLD NEW)
      (SETF (TV:SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG) NEW)
      (MUNG-LINES-WITH-CHAR #/OVERSTRIKE SELF))))

(DEFUN WINDOW-TAB-NCHARS (WINDOW)
  (SEND WINDOW :TAB-NCHARS))

(DEFUN REDEFINE-WINDOW-TAB-NCHARS (WINDOW TAB-NCHARS)
  (SEND WINDOW :SET-TAB-NCHARS TAB-NCHARS))

(DEFMETHOD (DISPLAYER :DEFAULT :TAB-NCHARS) ()
  8)

(DEFMETHOD (DISPLAYER :DEFAULT :SET-TAB-NCHARS) ()
  NIL)

(DEFMETHOD (DISPLAYER :TAB-NCHARS-LOCATION) ()
  NIL)

(DEFMETHOD (WINDOW :TAB-NCHARS) ()
  (TV:SHEET-TAB-NCHARS))

(DEFMETHOD (WINDOW :SET-TAB-NCHARS) (TAB-NCHARS)
  (LET* ((OLD (TV:SHEET-TAB-NCHARS SELF)))
    (UNLESS (= OLD TAB-NCHARS)
      (SETF (TV:SHEET-TAB-NCHARS) TAB-NCHARS)
      (MUNG-LINES-WITH-CHAR #/TAB SELF))))

(DEFUN MUNG-LINES-WITH-CHAR (CH WINDOW)
  (DO ((I 0 (1+ I))
       (NPLINES (WINDOW-N-PLINES WINDOW))
       (LINE)
       (FLAG NIL))
      (( I NPLINES)
       (AND FLAG (MUST-REDISPLAY WINDOW DIS-TEXT)))
    (WHEN (AND (SETQ LINE (PLINE-LINE WINDOW I))
               (STRING-SEARCH-CHAR CH LINE (PLINE-FROM-INDEX WINDOW I)
                                   (MIN (LINE-LENGTH LINE)
                                        (PLINE-TO-INDEX WINDOW I))))
      (SETF (PLINE-TICK WINDOW I) 0)
      (SETQ FLAG T))))

(DEFMETHOD (WINDOW :CURRENT-FONT) (&OPTIONAL (FONT *FONT*))
  (CURRENT-FONT SELF FONT))

(DEFUN TELL-EDITOR-TO-REDISPLAY (DEGREE)
  "Request redisplay in window SELF according to DEGREE, and send a blip to make it happen."
  (DECLARE (:SELF-FLAVOR DISPLAYER))
  (SETQ REDISPLAY-DEGREE (MAX REDISPLAY-DEGREE DEGREE))
  (COMMAND-BUFFER-PUSH '(REDISPLAY)))

(DEFMETHOD (DISPLAYER :INTERVAL-STRING) ()
  (STRING-INTERVAL INTERVAL))

(DEFMETHOD (DISPLAYER :INTERVAL-STREAM) ()
  (INTERVAL-STREAM INTERVAL))

(DEFMETHOD (DISPLAYER :SET-INTERVAL-STRING) (STRING)
  (LET ((*BATCH-UNDO-SAVE* T)
        (*WINDOW* SELF)
        (*INTERVAL* INTERVAL))
    (DISCARD-UNDO-INFORMATION INTERVAL)
    (DELETE-INTERVAL INTERVAL)
    (INSERT (INTERVAL-LAST-BP INTERVAL) STRING))
  (TELL-EDITOR-TO-REDISPLAY DIS-ALL))

(DEFMETHOD (DISPLAYER :ADD-TO-INTERVAL-STRING) (STRING)
  (LET ((*BATCH-UNDO-SAVE* T)
        (*WINDOW* SELF)
        (*INTERVAL* INTERVAL))
    (INSERT (INTERVAL-LAST-BP INTERVAL) STRING))
  (TELL-EDITOR-TO-REDISPLAY DIS-TEXT))

(DEFMETHOD (DISPLAYER :ADD-STRING-AT-POINT) (STRING)
  (LET ((*BATCH-UNDO-SAVE* T)
        (*WINDOW* SELF)
        (*INTERVAL* INTERVAL))
    (INSERT-MOVING POINT STRING))
  (TELL-EDITOR-TO-REDISPLAY DIS-TEXT))

;;;; Interval I/O

;;; Note: DEFFLAVORs related to this stuff are now found in SYS: ZWEI; DEFS.
;;; DEFMETHODS are found in SYS: ZWEI; METH.

;;; Edited to use flavors by Dulcey 10-Jan-83
;;; Variable used by stream renamed to **INTERVAL** to avoid compiler error messages
;;; caused by *INTERVAL* being a special variable in Zwei
;;;
;;; HACK-FONTS T means return 's for font changes
;;; HACK-FONTS :TYI means return 16 bit characters

;;; *LINE*, *INDEX* point to the next character to be returned.
;;; *STOP-INDEX* is the place on the current line at which to stop (usually the end).
;;; *LAST-LINE*, *LAST-INDEX* is where the interval ends.
;;; If *INDEX* is NIL, we are at the end-of-file.

;;; Font hacking stream
;;; Edited to use flavors by Dulcey 10-Jan-83
;;; Instance variable renamed to **FONT** to avoid compiler errors (since *FONT* is special)

;;; *FONT-FLAG* is normally NIL.  After a  which starts a font change, it is T.
;;; On input, it can also be a string of characters to read before
;;;  the next character from the interval.  This is used in describing diagram lines.
;;; On output, it can also be various things such as DIAG-1, DIAG-2 or an array
;;;  which are used in creating diagram lines.

;;; *FONT-STACK* is an art-q-list array used for doing ^F*.
;;; Numeric font changes push the previous font, and ^F*'s pop from it.
;;; It is initially empty.  If it gets full, the bottom 20. elements are flushed.

(DEFUN INTERVAL-STREAM (FROM-BP &OPTIONAL TO-BP IN-ORDER-P HACK-FONTS NO-UNDO-SAVING)
  "Return a stream that does I//O to the specified interval.
Input reads that text, and output inserts wherever input had got to.
If only output is done, it inserts at the beginning of the interval.
HACK-FONTS = T means return  prefixes if the text contains multiple fonts.
HACK-FONTS = :TYI means return characters with fonts if the text contains them.
NO-UNDO-SAVING non-NIL means do not record stream output to be undone."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (MAKE-INSTANCE (CASE HACK-FONTS
                   ((NIL) 'INTERVAL-STREAM)
                   (:TYI 'INTERVAL-STREAM-FAT)
                   (T 'INTERVAL-STREAM-WITH-FONTS))
                 :**INTERVAL** (CREATE-INTERVAL FROM-BP TO-BP)
                 :NO-UNDO-SAVING NO-UNDO-SAVING
                 :*LINE* (BP-LINE FROM-BP)
                 :*INDEX* (BP-INDEX FROM-BP)
                 :*LAST-LINE* (BP-LINE TO-BP)
                 :*LAST-INDEX* (BP-INDEX TO-BP)
                 :*STOP-INDEX* (IF (EQ (BP-LINE FROM-BP) (BP-LINE TO-BP))
                                   (BP-INDEX TO-BP)
                                   (LINE-LENGTH (BP-LINE FROM-BP)))))

;;; Return an interval stream outputing at BP
(DEFUN INTERVAL-STREAM-INTO-BP (BP &OPTIONAL HACK-FONTS)
  "Return a stream that outputs text at BP.
HACK-FONTS = T means return  prefixes if the text contains multiple fonts.
HACK-FONTS = :TYO means return characters with fonts if the text contains them."
  (INTERVAL-STREAM BP BP T HACK-FONTS))

(DEFUN REST-OF-INTERVAL-STREAM (BP)
  "Return a stream that reads all of the interval BP points at, after BP."
  (INTERVAL-STREAM BP (INTERVAL-LAST-BP *INTERVAL*) T))

(DEFUN STREAM-INTO-BP (STREAM BP &OPTIONAL HACK-FONTS)
  "Copy from the stream into the interval until EOF.
Returns a BP to where the end of the inserted text is.
HACK-FONTS means interpret 's in the file as font-change characters."
  (LET ((INT-STREAM (INTERVAL-STREAM-INTO-BP BP HACK-FONTS)))
    (STREAM-COPY-UNTIL-EOF STREAM INT-STREAM LINE-LEADER-SIZE)
    (SEND INT-STREAM :READ-BP)))

;;; Copy from the interval into the stream.
(DEFUN STREAM-OUT-INTERVAL (STREAM FROM-BP &OPTIONAL TO-BP IN-ORDER-P HACK-FONTS)
  "Copy from the specified interval to STREAM.
HACK-FONTS means interpret 's in the file as font-change characters."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
;  (STREAM-COPY-UNTIL-EOF
;    (INTERVAL-STREAM FROM-BP TO-BP T HACK-FONTS)
;    STREAM
;    NIL)
  (LET ((DEPTH 0)
        IN-STRING
        (CHECK-PARENS (AND *CHECK-UNBALANCED-PARENTHESES-WHEN-SAVING*
                           ;(EQ (GET (SEND (BP-TOP-LEVEL-NODE FROM-BP) :MAJOR-MODE) 'EDITING-TYPE) ':LISP)
                           (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                           )))
    (DO ((LINE) (EOF)
         (START-INDEX (BP-INDEX FROM-BP) 0)
         (INT-LINE (BP-LINE FROM-BP))
         (INT-STREAM (INTERVAL-STREAM FROM-BP TO-BP T HACK-FONTS)))
        (())
      (MULTIPLE-VALUE-SETQ (LINE EOF)
        (SEND INT-STREAM :LINE-IN NIL))
      (WHEN CHECK-PARENS
        (LET ((SYNTAX (LISP-PARSE-LINE INT-LINE IN-STRING START-INDEX))
              MINIMUM)
          (SETQ MINIMUM (IF (CONSP SYNTAX) (+ DEPTH (SECOND SYNTAX)) (+ DEPTH SYNTAX)))
          (WHEN (MINUSP MINIMUM)
            (BEEP)
            (FORMAT *QUERY-IO* "~&Unbalanced parentheses in ~A." (BP-TOP-LEVEL-NODE FROM-BP))
            (SETQ DEPTH 0 CHECK-PARENS NIL))
          (SETQ DEPTH (+ DEPTH (IF (NUMBERP SYNTAX) SYNTAX (CAR SYNTAX))))
          (SETQ IN-STRING (AND (CONSP SYNTAX) (FOURTH SYNTAX))))
        (SETQ INT-LINE (LINE-NEXT INT-LINE)))
      (IF (NOT EOF)
          (SEND STREAM :LINE-OUT LINE)
        (SEND STREAM :STRING-OUT LINE)
        (RETURN NIL)))
    (WHEN (AND CHECK-PARENS (OR IN-STRING (NOT (ZEROP DEPTH))))
      (BEEP)
      (FORMAT *QUERY-IO*
              "~&~:[Unbalanced parentheses in ~A~;Text of ~A ends inside a string~]"
              IN-STRING
              (BP-TOP-LEVEL-NODE FROM-BP)))))

(DEFUN OPEN-EDITOR-STREAM (&KEY INTERVAL BUFFER-NAME PATHNAME WINDOW START END
                           (CREATE-P :WARN) DEFAULTS HACK-FONTS KILL LOAD-P ORDERED-P
                           UNDO-SAVING)
  "Open and return a stream to read or write an editor interval.
The stream is bidirectional, with a single pointer into
 its interval, used for both reading and writing.
Specify the interval using :INTERVAL, :BUFFER-NAME, :PATHNAME, or :WINDOW.
:INTERVAL specifies the interval directly.
:BUFFER-NAME specifies a ZMACS buffer name.
:PATHNAME specifies a file.  The ZMACS buffer visiting it is used.
:WINDOW specifies a window.  The interval it is displaying is used.
If none of those is specified, :START must be a BP.
 The interval it points into us used.

These keywords modify the specification of the interval:
:CREATE-P says what to do if there is no ZMACS buffer
 for the specified :BUFFER-NAME or :PATHNAME.
 Possibilities are :ASK (query user), T (just create one),
 :WARN (print message on *ERROR-OUTPUT* and create one),
 or :ERROR (get an error).  The default is :WARN.
:LOAD-P says what to do if creating a buffer for a pathname.
 T means read in the file if it exists.  NIL means create it empty.
:DEFAULTS specifies a defaults-list for use in defaulting :PATHNAME.

These keywords specify precisely the portion of the interval
 to read or write:
:START specifies where to start.
 It can be :BEGINNING (beginning of interval), :END (end of interval),
 :POINT (the POINT of the specified :WINDOW),
 :MARK (the MARK of the specified :WINDOW),
 :REGION (same as using :POINT for :START and :MARK for :END),
 or a BP.
 :START initializes the stream's pointer, which is advanced
 over all texta read or written.
:END specifies where to stop reading (get eof).
 It can be :END, :POINT, :MARK or a BP.
:ORDERED-P non-NIL says assume that :END follows :START.
 If this is NIL, then the two are compared and whichever
 comes earlier in the interval is actually used as the start,
 the other becoming the end.  If :START is :BEGINNING or
 if :END is :END, the two are automatically known to be ordered.

:KILL if non-NIL says to delete the text of the interval
 or the portion of it between :START and :END.
:UNDO-SAVING if non-NIL says that insertions done by stream output
 should be recorded for the Undo command.
:HACK-FONTS can be NIL meaning discard font information
 of text in the buffer, or T meaning convert to epsilons
 (that is, the text you read will contain epsilons, and
 if you write epsilons they will convert into font changes)."
  (COND (INTERVAL)
        (BUFFER-NAME
         (OR (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME))
             (IF (SELECTQ CREATE-P
                   ((T) T)
                   (:ASK (FQUERY NIL "Create ZMACS buffer ~A? " BUFFER-NAME))
                   (:WARN
                    (FORMAT *ERROR-OUTPUT* "~&[Creating ZMACS buffer ~A]" BUFFER-NAME)
                    T))
                 (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME T)))
             (LET ((TEM (CERROR '(:NO-ACTION :NEW-VALUE) NIL NIL
                                "Buffer ~A does not exist." BUFFER-NAME)))
               (IF TEM (SETQ BUFFER-NAME TEM))
               (SETQ INTERVAL (FIND-BUFFER-NAMED BUFFER-NAME T)))))
        (PATHNAME
         (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME DEFAULTS))
         (OR (SETQ INTERVAL (FIND-FILE-BUFFER PATHNAME))
             (IF (SELECTQ CREATE-P
                   ((T) T)
                   (:ASK (FQUERY NIL "Create ZMACS buffer for ~A? " PATHNAME))
                   (:WARN
                    (FORMAT *ERROR-OUTPUT* "~&[Creating ZMACS buffer for ~A]" PATHNAME)
                    T))
                 (SETQ INTERVAL (FIND-FILE PATHNAME NIL NIL LOAD-P)))
             (LET ((TEM (CERROR '(:NO-ACTION :NEW-VALUE) NIL NIL
                                "There is no ZMACS buffer for ~A." PATHNAME)))
               (IF TEM (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS TEM DEFAULTS)))
               (SETQ INTERVAL
                     (OR (FIND-FILE-BUFFER PATHNAME)
                         (FIND-FILE PATHNAME NIL NIL LOAD-P))))))
        (WINDOW
         (SETQ INTERVAL (WINDOW-INTERVAL WINDOW)))
        (START
         (SETQ INTERVAL (BP-TOP-LEVEL-NODE START)))
        (T
         (FERROR "No interval specified.")))
  (CASE START
    ((:END :APPEND NIL)
     (SETQ START (INTERVAL-LAST-BP INTERVAL)))
    (:BEGINNING
     (SETQ ORDERED-P T)
     (SETQ START (INTERVAL-FIRST-BP INTERVAL)))
    (:POINT
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR "No WINDOW specified with START = :POINT."))
     (SETQ START (WINDOW-POINT WINDOW)))
    (:MARK
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR "No WINDOW specified with START = :MARK."))
     (SETQ START (WINDOW-MARK WINDOW)))
    (:REGION
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR "No WINDOW specified with START = :REGION."))
     (SETQ START (WINDOW-POINT WINDOW))
     (SETQ END (WINDOW-MARK WINDOW)))
    (T
     (UNLESS (CONSP START)
       (FERROR "START is ~S, which is not valid." START))))
  (CASE END
    ((:END NIL)
     (SETQ ORDERED-P T)
     (SETQ END (INTERVAL-LAST-BP INTERVAL)))
    (:POINT
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR "No WINDOW specified with END = :POINT."))
     (SETQ END (WINDOW-POINT WINDOW)))
    (:MARK
     (UNLESS (TYPEP WINDOW 'DISPLAYER)
       (FERROR "No WINDOW specified with END = :MARK."))
     (SETQ END (WINDOW-MARK WINDOW)))
    (T
     (UNLESS (CONSP END)
       (FERROR "END is not a BP or :END, :POINT or :MARK."))))
  (WHEN KILL
    (LET ((*BATCH-UNDO-SAVE* T))
      (DELETE-INTERVAL START END)))
  (INTERVAL-STREAM START END ORDERED-P HACK-FONTS (NOT UNDO-SAVING)))

;;;; Methods for INTERVAL-STREAM, INTERVAL-STREAM-FAT, and INTERVAL-STREAM-WITH-FONTS

(DEFMACRO INTERVAL-WITH-FONTS-IO-PUSH-FONT ()
  '(OR (VECTOR-PUSH **FONT** *FONT-STACK*)
       (PROGN (COPY-ARRAY-PORTION *FONT-STACK* 20. (ARRAY-LENGTH *FONT-STACK*)
                                  *FONT-STACK* 0 (- (ARRAY-LENGTH *FONT-STACK*) 20.))
              (DECF (FILL-POINTER *FONT-STACK*) 20.)
              (VECTOR-PUSH **FONT** *FONT-STACK*))))

(DEFMETHOD (INTERVAL-STREAM :CHARACTERS) ()
  T)

(DEFMETHOD (INTERVAL-STREAM :ELEMENT-TYPE) ()
  'STRING-CHAR)

(DEFMETHOD (INTERVAL-STREAM :TYI) (&OPTIONAL EOF)
  (COND (*EOF* (AND EOF (FERROR "~A" EOF)))
         ((< *INDEX* *STOP-INDEX*)
          (PROG1 (CHAR-CODE (CHAR *LINE* *INDEX*))
                 (INCF *INDEX*)))
         ((EQ *LINE* *LAST-LINE*)
          (SETQ *EOF* T)
          (AND EOF (FERROR "~A" EOF)))
         (T
          (SETQ *LINE* (LINE-NEXT *LINE*)
                *INDEX* 0
                *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                 *LAST-INDEX*
                                 (LINE-LENGTH *LINE*)))
          (CHAR-INT #/NEWLINE))))

(DEFMETHOD (INTERVAL-STREAM-FAT :TYI) (&OPTIONAL EOF)
  (COND (*EOF* (AND EOF (FERROR "~A" EOF)))
         ((< *INDEX* *STOP-INDEX*)
          (PROG1 (CHAR-INT (CHAR *LINE* *INDEX*)) (INCF *INDEX*)))
         ((EQ *LINE* *LAST-LINE*)
          (SETQ *EOF* T)
          (AND EOF (FERROR "~A" EOF)))
         (T
          (SETQ *LINE* (LINE-NEXT *LINE*)
                *INDEX* 0
                *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                 *LAST-INDEX*
                                 (LINE-LENGTH *LINE*)))
          (CHAR-INT #/NEWLINE))))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :TYI) (&OPTIONAL EOF &AUX CH)
  (COND ((STRINGP *FONT-FLAG*)
         (SETQ CH (CHAR-INT (CHAR *FONT-FLAG* *INDEX*)))
         (AND ( (SETQ *INDEX* (1+ *INDEX*)) *STOP-INDEX*)
              (SETQ *FONT-FLAG* NIL
                    *INDEX* 0
                    *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*) *LAST-INDEX*
                                   (LINE-LENGTH *LINE*))))
         CH)
        ((NULL *INDEX*)
         (AND EOF (ERROR EOF)))
        ((EQ *FONT-FLAG* T)
         (SETQ *FONT-FLAG* NIL)
         (+ #/0 **FONT**))
        ((NUMBERP *FONT-FLAG*)
         (PROG1 *FONT-FLAG*
                (SETQ *FONT-FLAG* NIL)))
        ((< *INDEX* *STOP-INDEX*)
         (SETQ CH (CHAR *LINE* *INDEX*))
         (COND (( **FONT** (CHAR-FONT CH))
                (COND ((find (CHAR-FONT CH) *FONT-STACK*)
                       (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
                       (SETQ *FONT-FLAG* #/*))
                      (T
                       (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
                       (SETQ **FONT** (CHAR-FONT CH))
                       (SETQ *FONT-FLAG* T)))
                (CHAR-INT #/))
               (T
                (SETQ *INDEX* (1+ *INDEX*))
                (IF ( (CHAR-CODE CH) (CHAR-CODE #/))
                    (CHAR-CODE CH)
                  (SETQ *FONT-FLAG* (CHAR-INT CH))))))
        ((EQ *LINE* *LAST-LINE*)
         (SETQ *INDEX* NIL)
         (AND EOF (ERROR EOF)))
        ((ANTICIPATE-FONT-POP))
        (T
         (SETQ *LINE* (LINE-NEXT *LINE*))
         (IF (SETQ CH (GETF (LINE-PLIST *LINE*) ':DIAGRAM))
             (LET* ((STRING (SEND CH :STRING-FOR-FILE *LINE*))
                    (LENGTH (STRING-LENGTH STRING)))
               (AND (PLUSP LENGTH)
                    (SETQ *FONT-FLAG* STRING
                          *INDEX* 0 *STOP-INDEX* LENGTH)))
           (SETQ *INDEX* 0 *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                            *LAST-INDEX*
                                          (LINE-LENGTH *LINE*))))
         (CHAR-INT #/NEWLINE))))

(DEFMETHOD (INTERVAL-STREAM :LINE-IN) (&OPTIONAL SIZE EOF)
  (LET ((RET-LINE)
        (AT-END-P (EQ *LINE* *LAST-LINE*)))
    (COND ((AND (NULL SIZE)
                (ZEROP *INDEX*)
                (NOT AT-END-P))
           ;; Easy case, just return the line and advance the pointer.
           (SETQ RET-LINE *LINE*)
           (SETQ *LINE* (LINE-NEXT *LINE*))
           (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                  *LAST-INDEX*
                                (LINE-LENGTH *LINE*))))
          (*EOF*
           ;; End of file.
           (AND EOF (ERROR EOF)))
          (T
           ;; Hard case, make a copy.
           (SETQ RET-LINE
                 (MAKE-ARRAY (- *STOP-INDEX* *INDEX*)
                             :TYPE (ARRAY-TYPE *LINE*)
                             :LEADER-LENGTH (IF (NUMBERP SIZE) SIZE NIL)))
           (DO ((LF *INDEX* (1+ LF))
                (RT 0 (1+ RT)))
               (( LF *STOP-INDEX*))
             (SETF (CHAR RET-LINE RT) (CHAR *LINE* LF)))
           (IF (NUMBERP SIZE)
               (SETF (ARRAY-LEADER RET-LINE 0) (- *STOP-INDEX* *INDEX*)))
           ;; Now advance the pointer.
           (COND (AT-END-P
                  (SETQ *EOF* T))
                 (T
                  (SETQ *LINE* (LINE-NEXT *LINE*))
                  (SETQ *INDEX* 0)
                  (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                         *LAST-INDEX*
                                       (LINE-LENGTH *LINE*)))))))
    (VALUES RET-LINE AT-END-P)))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :LINE-IN) (&OPTIONAL SIZE EOF)
  (LET ((RET-LINE)
        (AT-END-P (EQ *LINE* *LAST-LINE*)))
    (COND ((AND (NULL SIZE)
                (NULL *FONT-FLAG*)
                (EQ 0 *INDEX*)
                (NOT AT-END-P))
           (IF (AND (ZEROP **FONT**)
                    (DOTIMES (I (LINE-LENGTH *LINE*) T)
                      (OR (ZEROP (CHAR-FONT (CHAR *LINE* I)))
                          (RETURN NIL))))
               ;; Easy case, line is all in font 0 and that font is current.
               (SETQ RET-LINE *LINE*)
             ;; Otherwise, look through actual line
             ;; storing font changes into RET-LINE when needed.
             (SETQ RET-LINE (MAKE-ARRAY (+ 8. (STRING-LENGTH *LINE*)) :FILL-POINTER 0
                                        :TYPE ART-STRING))
             (DOTIMES (I (LINE-LENGTH *LINE*))
               (LET ((CH (CHAR *LINE* I)))
                 (UNLESS (= (CHAR-FONT CH) **FONT**)
                   (COND ((find (CHAR-FONT CH) *FONT-STACK*)
                          (DO ()
                              ((= (CHAR-FONT CH) **FONT**))
                            (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
                            (VECTOR-PUSH-EXTEND #/ RET-LINE)
                            (VECTOR-PUSH-EXTEND #/* RET-LINE)))
                         (T
                          (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
                          (SETQ **FONT** (CHAR-FONT CH))
                          (VECTOR-PUSH-EXTEND #/ RET-LINE)
                          (VECTOR-PUSH-EXTEND (+ #/0 **FONT**) RET-LINE))))
                 (SETQ CH (CHAR-CODE CH))
                 (IF (= CH (CHAR-CODE #/))
                     (VECTOR-PUSH-EXTEND #/ RET-LINE))
                 (VECTOR-PUSH-EXTEND CH RET-LINE)))
             ;; Do one or more * now if would otherwise happen after the next few Returns.
             (DO ()
                 ((NOT (ANTICIPATE-FONT-POP)))
               (VECTOR-PUSH-EXTEND #/ RET-LINE)
               (VECTOR-PUSH-EXTEND #/* RET-LINE)
               (SETQ *FONT-FLAG* NIL)))
           (SETQ *LINE* (LINE-NEXT *LINE*))
           (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                                  *LAST-INDEX*
                                (LINE-LENGTH *LINE*))))
          ((NULL *INDEX*)
           ;; End of file.
           (SETQ RET-LINE (MAKE-STRING 0))
           (AND EOF (ERROR EOF)))
          (T
           ;; Hard case; do it using our :TYI method.
           (SETF (VALUES RET-LINE AT-END-P)
                 (STREAM-DEFAULT-HANDLER SELF :LINE-IN SIZE (LIST EOF)))
           ;; If we are now processing the last line, set *INDEX* to NIL
           ;; so the next :LINE-IN can get an error if desired.
           (IF AT-END-P (SETQ *INDEX* NIL))))
    (VALUES RET-LINE AT-END-P)))

(DEFMETHOD (INTERVAL-STREAM :UNTYI) (IGNORE)
  (IF (ZEROP *INDEX*)
      (SETQ *LINE* (LINE-PREVIOUS *LINE*)
            *INDEX* (STRING-LENGTH *LINE*)
            *STOP-INDEX* (STRING-LENGTH *LINE*))
    (DECF *INDEX*)))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :UNTYI) (CH)
  (IF (CHARACTERP CH) (SETQ CH (CHAR-INT CH)))
  (IF (ZEROP *INDEX*)
      (SETQ *LINE* (LINE-PREVIOUS *LINE*) *INDEX* (LINE-LENGTH *LINE*))
    (IF (CHAR= (IN-CURRENT-FONT CH **FONT**) (CHAR *LINE* (1- *INDEX*)))
        (DECF *INDEX*)
      (SETQ *FONT-FLAG* CH))))

(DEFMETHOD (INTERVAL-STREAM :TYO) (CH)
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) CH)))
      (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP)))))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :TYO) (CH)
  (IF (CHARACTERP CH) (SETQ CH (CHAR-INT CH)))
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (COND ((EQ *FONT-FLAG* T)
           ;; Character after a ^F.
           (SETQ *FONT-FLAG* NIL)
           (COND ((= CH (CHAR-INT #/))
                  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
                                    (IN-CURRENT-FONT CH **FONT**))))
                    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
                 ((= CH (CHAR-INT #/#))
                  (SETQ *FONT-FLAG* 'DIAG-1))
                 ((= CH (CHAR-INT #/*))
                  (OR (ZEROP (ARRAY-LEADER *FONT-STACK* 0))
                      (SETQ **FONT** (VECTOR-POP *FONT-STACK*))))
                 (T
                  (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
                  (SETQ **FONT** (- CH #/0)))))
          ((NULL *FONT-FLAG*)
           ;; Character in normal text state.
           (COND ((= CH (CHAR-INT #/))
                  (SETQ *FONT-FLAG* T))
                 (T
                  (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*)
                                    (IN-CURRENT-FONT CH **FONT**))))
                    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
          ((EQ *FONT-FLAG* 'DIAG-1)
           ;; Character after a ^F#
           (SETQ *FONT-FLAG* 'DIAG-2 *STOP-INDEX* 0))
          ((EQ *FONT-FLAG* 'DIAG-2)
           (IF (= CH (CHAR-INT #/SPACE))
               (SETQ *FONT-FLAG* (MAKE-STRING 10. :FILL-POINTER 0))
             (SETQ *STOP-INDEX* (+ (* *STOP-INDEX* 10.) (- CH #/0)))))
          ((STRINGP *FONT-FLAG*)
           (IF (= CH (CHAR-INT #/NEWLINE))
               (SETQ *INDEX* NIL
                     *FONT-FLAG* (MAKE-INSTANCE (READ-FROM-STRING *FONT-FLAG*)
                                                :NUMBER-OF-LINES *STOP-INDEX*))
             (VECTOR-PUSH-EXTEND CH *FONT-FLAG*)))
          ((TYPEP *FONT-FLAG* 'RESTORABLE-LINE-DIAGRAM-MIXIN)
           (BLOCK NIL
             (OR *INDEX*
                 (COND ((< (SETQ *STOP-INDEX* (1- *STOP-INDEX*)) 0)
                        (SETQ *INDEX* 0 *FONT-FLAG* (= CH (CHAR-INT #/)))
                        (RETURN NIL))
                       (T
                        (SETQ *INDEX* (CREATE-LINE ART-STRING 0 NIL))
                        (INSERT-LINE-WITH-LEADER *INDEX* *LINE*))))
             (COND ((= CH (CHAR-INT #/NEWLINE))
                    (SETF (GETF (LINE-PLIST *INDEX*) ':DIAGRAM) *FONT-FLAG*)
                    (SEND *FONT-FLAG* :ADD-LINE *INDEX* *INDEX*)
                    (SETF (LINE-LENGTH *INDEX*) 0)
                    (SETQ *INDEX* NIL))
                   (T
                    (VECTOR-PUSH-EXTEND CH *INDEX*)))))
          (T (FERROR "~S has a value not understood here" '*FONT-FLAG*)))))

(DEFMETHOD (INTERVAL-STREAM :LINE-OUT) (LINE)   ;Bleagh, really should take two optional args
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (COND ((AND (ZEROP *INDEX*) (EQ (ARRAY-LEADER-LENGTH LINE) LINE-LEADER-SIZE))
           (INSERT-LINE-WITH-LEADER LINE *LINE*))       ;Optimize case for file readin
          ((ZEROP *INDEX*)                      ;Optimize case where it's not already a line
           (LET ((NEW-LINE (CREATE-LINE 'ART-STRING (LENGTH LINE) **INTERVAL**)))
             (COPY-ARRAY-CONTENTS LINE NEW-LINE)
             (INSERT-LINE-WITH-LEADER NEW-LINE *LINE*)))
          (T
           (LET ((BP (INSERT (INSERT (CREATE-BP *LINE* *INDEX*) LINE) #/NEWLINE)))
             (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))))
  LINE)

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :LINE-OUT) (LINE)        ;Bleagh, really should take
                                                ;two optional args
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (WHEN (ZEROP *INDEX*)
      ;; At start of line in buffer, we can just make a line and insert it here.
      (IF (EQ (ARRAY-TYPE LINE) 'ART-FAT-STRING)
          ;; If it's already a fat string, just make sure its leader is the right size.
          (OR (EQ (ARRAY-LEADER-LENGTH LINE) LINE-LEADER-SIZE)
              (LET ((NEW-LINE (CREATE-LINE 'ART-STRING (LENGTH LINE) **INTERVAL**)))
                (COPY-ARRAY-CONTENTS LINE NEW-LINE)
                (SETQ LINE NEW-LINE)))
        ;; Otherwise do font processing to make a new string.
        (SETQ LINE (MAKE-MULTI-FONT-LINE SELF LINE **INTERVAL**))))
    ;; Now test for MAKE-MULTI-FONT-LINE returning NIL, or inserting some stuff
    ;; so that we are no longer at the start of a line in the buffer.
    (AND LINE
         (IF (ZEROP *INDEX*)
             (INSERT-LINE-WITH-LEADER LINE *LINE*)
           (SEND SELF :STRING-OUT LINE)
           (SEND SELF :TYO #/NEWLINE)))
    (LINE-PREVIOUS *LINE*)))

(DEFMETHOD (INTERVAL-STREAM :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
   (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) STRING START END)))
     (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP)))))

(DEFMETHOD (INTERVAL-STREAM-WITH-FONTS :STRING-OUT)
           (STRING &OPTIONAL (START 0) END (ORIGINAL-STRING STRING))
  (LET-IF NO-UNDO-SAVING ((*BATCH-UNDO-SAVE* T))
    (OR (EQ (ARRAY-TYPE STRING) 'ART-FAT-STRING)
        (SETQ STRING (MAKE-MULTI-FONT-LINE SELF STRING NIL START END) START 0 END NIL))
    (AND STRING
         (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) STRING START END)))
           (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
    ORIGINAL-STRING))

(DEFMETHOD (INTERVAL-STREAM :READ-BP) ()
  (CREATE-BP *LINE* *INDEX*))

(DEFMETHOD (INTERVAL-STREAM :UNTYO-MARK) ()
  (CREATE-BP *LINE* *INDEX*))

(DEFMETHOD (INTERVAL-STREAM :UNTYO) (MARK)
  (DELETE-INTERVAL MARK (CREATE-BP *LINE* *INDEX*) T)
  (SETQ *LINE* (BP-LINE MARK) *INDEX* (BP-INDEX MARK)))

(DEFMETHOD (INTERVAL-STREAM :SET-BP) (BP)
  (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))
  (LET ((LAST-BP (INTERVAL-LAST-BP **INTERVAL**)))      ;Take account of inserted changes
    (SETQ *LAST-LINE* (BP-LINE LAST-BP)
          *LAST-INDEX* (BP-INDEX LAST-BP)))
  (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
                         *LAST-INDEX*
                       (LINE-LENGTH *LINE*))))

(DEFMETHOD (INTERVAL-STREAM :DELETE-TEXT) ()
  (DELETE-INTERVAL **INTERVAL**))

(DEFMETHOD (INTERVAL-STREAM :FRESH-LINE) ()
  (IF (ZEROP *INDEX*) NIL
    (SEND SELF :TYO #/NEWLINE)
    T))

(DEFMETHOD (INTERVAL-STREAM :READ-UNTIL-EOF) ()
  (SETQ *LINE* *LAST-LINE*
        *INDEX* *LAST-INDEX*
        *STOP-INDEX* *LAST-INDEX*))

(DEFMETHOD (INTERVAL-STREAM :SET-POINTER) (POINTER)
  (OR (ZEROP POINTER) (FERROR "Attempt to set pointer other than to beginning."))
  (LET ((BP (INTERVAL-FIRST-BP **INTERVAL**)))
    (SETQ *LINE* (BP-LINE BP)
          *INDEX* (BP-INDEX BP)
          *STOP-INDEX* (IF (EQ (BP-LINE BP) *LAST-LINE*)
                           *LAST-INDEX*
                         (LINE-LENGTH *LINE*)))))

(DEFMETHOD (INTERVAL-STREAM :READ-CURSORPOS) (&OPTIONAL (UNITS :PIXEL))
  (DO ((I 0 (1+ I))
       (X 0))
      (( I *INDEX*)
       (IF (EQ UNITS :PIXEL)
           (VALUES (* X (TV:SHEET-CHAR-WIDTH (WINDOW-SHEET *WINDOW*))) 0)
           (VALUES X 0)))                       ;Y position always zero
    (CASE (CHAR-CODE (CHAR *LINE* I))
      (#.(CHAR-CODE #/OVERSTRIKE) (SETQ X (MAX (1- X) 0)))
      (#.(CHAR-CODE #/TAB) (SETQ X (* (1+ (FLOOR X 8)) 8)))
      (OTHERWISE (INCF X)))))

(DEFMETHOD (INTERVAL-STREAM :INCREMENT-CURSORPOS) (X Y &OPTIONAL (UNITS :PIXEL))
  (DECLARE (IGNORE Y))                          ;This is a bit fraudulent, for FORMAT ~T.
  (UNLESS (EQ UNITS :CHARACTER) (FERROR "~S unknown cursor-position unit" UNITS))
  ;; Can't use the regular indent stuff since we don't have a window.
  (DOTIMES (I X)
    (SEND SELF :TYO #/SPACE)))

(DEFMETHOD (INTERVAL-STREAM :SET-CURSORPOS) (X Y &OPTIONAL (UNITS :PIXEL))
  (declare (ignore Y))                          ;This is a bit fraudulent, for FORMAT ~T.
  ;; Can't use the regular indent stuff since we don't have a window.
  (LET ((FROM (SEND SELF :READ-CURSORPOS :CHARACTER))
        (TO (IF (EQ UNITS :CHARACTER)
                X
              (1+ (TRUNCATE (1- X) (TV:SHEET-CHAR-WIDTH (WINDOW-SHEET *WINDOW*)))))))
    (DO FROM FROM (1+ FROM) ( FROM TO)
        (SEND SELF :TYO #/SPACE))))

(DEFMETHOD (INTERVAL-STREAM :CLEAR-SCREEN) (&REST IGNORE))

(DEFMETHOD (INTERVAL-STREAM :FINISH) (&REST IGNORE))

(DEFMETHOD (INTERVAL-STREAM :FORCE-OUTPUT) (&REST IGNORE))

(DEFMETHOD (INTERVAL-STREAM :TRUENAME) (&AUX INT)
  (AND (TYPEP (SETQ INT (BP-TOP-LEVEL-NODE (CREATE-BP *LINE* *INDEX*))) 'FILE-BUFFER)
       (FS:PARSE-PATHNAME (BUFFER-NAME INT) 'ED-BUFFER)))

;;These are for the compiler (also FILE-READ-PROPERTY-LIST)
(DEFMETHOD (INTERVAL-STREAM :PATHNAME) (&AUX INT)
  (AND (TYPEP (SETQ INT (BP-TOP-LEVEL-NODE (CREATE-BP *LINE* *INDEX*))) 'FILE-BUFFER)
       (BUFFER-PATHNAME INT)))

(DEFMETHOD (INTERVAL-STREAM :GENERIC-PATHNAME) (&AUX INT)
  (AND (TYPEP (SETQ INT (BP-TOP-LEVEL-NODE (CREATE-BP *LINE* *INDEX*))) 'FILE-BUFFER)
       (BUFFER-GENERIC-PATHNAME INT)))

(DEFMETHOD (INTERVAL-STREAM :INFO) (&REST IGNORE)
  NIL)

;; This is rather bogus.  But it doesn't cause an undefined-function error in WHO-CALLS.
(DEFMETHOD (INTERVAL-STREAM :COMPILER-WARNINGS-NAME) (&REST IGNORE)
  (SEND SELF :PATHNAME))

;; This is a separate message so that simulated BP of *LINE*,*INDEX* will be kept up to date
(DEFMETHOD (INTERVAL-STREAM :DELETE-INTERVAL) (START-BP &OPTIONAL END-BP IN-ORDER-P)
  (WITH-BP (BP (CREATE-BP *LINE* *INDEX*) :NORMAL)
    (DELETE-INTERVAL START-BP END-BP IN-ORDER-P)
    (SETQ *LINE* (BP-LINE BP)
          *INDEX* (BP-INDEX BP))))

(DEFMETHOD (INTERVAL-STREAM :TEXT-DELETED) (&REST IGNORE)
  NIL)

(DEFMETHOD (INTERVAL-STREAM :SAFE-TO-USE-P) ()
  T)

(DEFMETHOD (INTERVAL-STREAM :LINE-PUT) (PROPERTY VALUE)
  (SETF (GETF (LINE-PLIST *LINE*) PROPERTY) VALUE))

(DEFMETHOD (INTERVAL-STREAM :NEXT-LINE-GET) (PROPERTY)
  (AND (NEQ *LINE* (BP-LINE (INTERVAL-LAST-BP **INTERVAL**)))
       (GETF (LINE-PLIST (LINE-NEXT *LINE*)) PROPERTY)))

(DEFMETHOD (INTERVAL-STREAM :FORCE-REDISPLAY) ()
  (LET ((*WINDOW* 'IGNORE))
    (MUST-REDISPLAY-OTHER-WINDOWS (BP-TOP-LEVEL-NODE (CREATE-BP *LINE* *INDEX*))
                                  *WINDOW* DIS-TEXT)))

(COMPILE-FLAVOR-METHODS INTERVAL-STREAM INTERVAL-STREAM-FAT INTERVAL-STREAM-WITH-FONTS)

;;;; Functions used by INTERVAL-STREAM-WITH-FONTS

(DEFUN ANTICIPATE-FONT-POP ()
  (DECLARE (:SELF-FLAVOR INTERVAL-STREAM-WITH-FONTS))
  (DO ((LINE (LINE-NEXT *LINE*) (LINE-NEXT LINE))
       F)
      ((EQ LINE *LAST-LINE*))
    (OR (GETF (LINE-PLIST LINE) ':DIAGRAM)
        (ZEROP (LINE-LENGTH LINE))
        (RETURN (UNLESS (= **FONT** (SETQ F (CHAR-FONT (CHAR LINE 0))))
                  (WHEN (find F *FONT-STACK*)
                    (SETQ **FONT** (VECTOR-POP *FONT-STACK*))
                    (SETQ *FONT-FLAG* #/*)
                    #/))))))

;Take STRING, which contains ^F's, and convert it into a fat-string
;in which each character contains its font.  At least, that is what we normally do.
;If INTERVAL-OR-NIL is NIL, we make the leader length 1.
;If it is an interval, we make a line, with that as its node.
;In the latter case, we consider that there is an implicit Return at the end of the string.

;We cannot handle diagrams at this level, so we pass them along to the :TYO method.
;We do this by 1) taking the converted string so far and doing :STRING-OUT on it,
;2) passing the remaining characters of STRING to :TYO until the diagram is over,
;and 3) starting over with what is left of the string.
;If the string is exhausted in step 2) we return nil.
(DEFUN MAKE-MULTI-FONT-LINE (STREAM STRING INTERVAL-OR-NIL &OPTIONAL (START 0) END)
  (DECLARE (:SELF-FLAVOR INTERVAL-STREAM-WITH-FONTS))
  (LET* ((LENGTH (OR END (STRING-LENGTH STRING)))
         (NEWSTRING))
    (DO ((I START (1+ I)))
        ((= I LENGTH)
         ;; Check for case of null line with implicit Return at end.
         (OR NEWSTRING
             (AND INTERVAL-OR-NIL
                  (SEND STREAM :TYO #/RETURN))))
      (COND ((NOT (MEMQ *FONT-FLAG* '(T NIL)))
             ;; If inside a diagram, first output converted string so far.
             (IF NEWSTRING (SEND STREAM :STRING-OUT NEWSTRING))
             (SETQ NEWSTRING NIL)
             ;; Then TYO characters from the input string till end of string or end of diagram
             (DO ((I1 I (1+ I1)))
                 ((OR (= I1 LENGTH)
                      (MEMQ *FONT-FLAG* '(T NIL)))
                  (SETQ I I1))
               (SEND SELF :TYO (CHAR-INT (CHAR STRING I1))))
             ;; If it's end of string, return NIL, TYOing the implicit Return if there is one.
             (AND (= I LENGTH)
                  (RETURN
                    (IF INTERVAL-OR-NIL
                        (SEND STREAM :TYO #/RETURN))))))
      ;; If this is first char or first char after a diagram, make an output string.
      (OR NEWSTRING
          (PROGN
            (SETQ NEWSTRING
                  (IF INTERVAL-OR-NIL
                      (CREATE-LINE ART-FAT-STRING (- LENGTH I) INTERVAL-OR-NIL)
                    (MAKE-ARRAY (- LENGTH I)
                                :TYPE ART-FAT-STRING
                                :FILL-POINTER 0)))
            (SETF (ARRAY-LEADER NEWSTRING 0) 0)))
      ;; Process the non-diagram character.
      (LET ((CH (CHAR STRING I)))
        (COND ((EQ *FONT-FLAG* T)
               ;; After a ^F (epsilon).
               (COND ((CHAR= CH #/)
                      (SETQ *FONT-FLAG* NIL)
                      (VECTOR-PUSH (IN-CURRENT-FONT CH **FONT**) NEWSTRING))
                     ((CHAR= CH #/#)
                      (SETQ *FONT-FLAG* 'DIAG-1))
                     ((CHAR= CH #/*)
                      (SETQ *FONT-FLAG* NIL)
                      (OR (ZEROP (FILL-POINTER *FONT-STACK*))
                          (SETQ **FONT** (VECTOR-POP *FONT-STACK*))))
                     (T
                      (INTERVAL-WITH-FONTS-IO-PUSH-FONT)
                      (SETQ *FONT-FLAG* NIL
                            **FONT** (- CH #/0)))))
              ((CHAR= CH #/)
               (SETQ *FONT-FLAG* T))
              (T
               (VECTOR-PUSH (IN-CURRENT-FONT CH **FONT**) NEWSTRING)))))
    NEWSTRING))

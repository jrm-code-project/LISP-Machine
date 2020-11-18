;;The broken, hairy, useless window error handler -*- Mode:LISP; Package:EH; Base:8; Readtable:ZL -*-

(DEFFLAVOR ERROR-HANDLER-LISP-LISTENER-PANE ()
           (TV:INTERACTION-PANE)
  :ALIAS-FLAVOR
  (:DOCUMENTATION :COMBINATION "The read-eval-print window in the window error handler"))

(DEFFLAVOR ERROR-HANDLER-TEXT-SCROLL-PANE ()
           TV:(FUNCTION-TEXT-SCROLL-WINDOW MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
               FLASHY-SCROLLING-MIXIN STREAM-MIXIN SELECT-MIXIN
               BORDERS-MIXIN TOP-LABEL-MIXIN BASIC-SCROLL-BAR
               DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
           (:DOCUMENTATION :COMBINATION "Scroll windows for the window error handler"))

(DEFMETHOD (ERROR-HANDLER-TEXT-SCROLL-PANE :WHO-LINE-DOCUMENTATION-STRING) ()
  "L: Inspect the selected item, and set * to it.")

(DEFFLAVOR GRAY-ERROR-HANDLER-TEXT-SCROLL-PANE () (TV:TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK
                                                   ERROR-HANDLER-TEXT-SCROLL-PANE)
  (:DOCUMENTATION :COMBINATION "Args and locals windows in window error handler"))

(DEFFLAVOR STACK-SCROLL-PANE
        ((*PRINT-LENGTH* ERROR-MESSAGE-PRINLENGTH)
         (*PRINT-LEVEL* ERROR-MESSAGE-PRINLEVEL))
        (TV:CURRENT-ITEM-MIXIN TV:FLASHY-MARGIN-SCROLLING-MIXIN
         TV:FUNCTION-TEXT-SCROLL-WINDOW
         TV:LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN
         TV:MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
         TV:MARGIN-REGION-MIXIN
         ERROR-HANDLER-TEXT-SCROLL-PANE)
  :SPECIAL-INSTANCE-VARIABLES
  (:DOCUMENTATION :COMBINATION "Stack window in the window error handler"))

(DEFMETHOD (STACK-SCROLL-PANE :LINE-AREA-MOUSE-DOCUMENTATION) ()
  "L: Select the stack frame the mouse is pointing at.")

(DEFFLAVOR ERROR-HANDLER-HISTORY-PANE () (TV:INSPECT-HISTORY-PANE))
(DEFMETHOD (ERROR-HANDLER-HISTORY-PANE :WHO-LINE-DOCUMENTATION-STRING) ()
  "L: Inspect object pointed at.  R: Set * to object.")

(DEFFLAVOR ERROR-HANDLER-FRAME
         (INSPECT-WINDOW                        ;Where the disassembled code goes
          INSPECT-HISTORY-WINDOW                ;History for the inspector
          ARGS-WINDOW                           ;The arguments
          LOCALS-WINDOW                         ;The locals
          STACK-WINDOW                          ;Backtrace
          COMMAND-MENU-WINDOW                   ;The command menu
          LISP-WINDOW                           ;A read-eval-print loop window
          FRAME-ALIST                           ;Saved frame layout
          )
        (TV:INFERIORS-NOT-IN-SELECT-MENU-MIXIN
         TV:BORDERED-CONSTRAINT-FRAME)
  (:GETTABLE-INSTANCE-VARIABLES LISP-WINDOW INSPECT-WINDOW INSPECT-HISTORY-WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :SPECIAL-PURPOSE "Controls layout of window error handler panes"))

(DEFVAR COMMAND-ALIST
        '(("Proceed"
           :VALUE COMW-PROCEED
           :DOCUMENTATION "Try to proceed from the error.")
          ("Exit Window EH" :VALUE COMW-EXIT
           :DOCUMENTATION "Return to regular debugger, but don't leave error context.")
          ("What Error" :VALUE COMW-WHAT-ERROR
           :DOCUMENTATION "Re-print error message.")
          ("Retry" :VALUE COM-RETURN-REINVOCATION
           :DOCUMENTATION "Retry the function call represented by the current frame.")
          ("Set Arg" :VALUE COMW-SET-ARG
           :DOCUMENTATION "Modify an argument in the current frame.")
          ("T" :VALUE T :DOCUMENTATION "Like typing T.  Also means Yes.")
          ("Inspect" :VALUE COMW-INSPECT
           :DOCUMENTATION "Inspects a value that's either typed or pointed to.")
          ("Return a Value" :VALUE COM-RETURN-A-VALUE
           :DOCUMENTATION "Return a value as the result of the current frame.")
          ("Search" :VALUE COMW-SEARCH :DOCUMENTATION "Search in stack for a string.")
          ("NIL" :VALUE NIL-VALUE
           :DOCUMENTATION "Like typing NIL.  Also means No.")
          ("Abort Program" :VALUE COM-TOP-LEVEL-THROW
           :DOCUMENTATION "Like typing Abort in regular debugger.")
          ("Arglist" :VALUE COMW-ARGLIST
           :DOCUMENTATION "Display argument list of function in the current frame.")
          ("Edit" :VALUE COMW-EDIT
           :DOCUMENTATION "Edit the function in the current frame.")
          ("Throw" :VALUE COM-THROW
           :DOCUMENTATION "Throw a value to a catch tag.")
          ("DeCache" :VALUE COMW-FLUSH-CACHE
           :DOCUMENTATION
           "Delete saved display info.  Useful if you are looking at objects that have changed.")))

(DEFMETHOD (ERROR-HANDLER-FRAME :BEFORE :INIT) (IGNORE &AUX IO-BUFFER)
  (SETQ IO-BUFFER (TV:MAKE-DEFAULT-IO-BUFFER))
  (SETQ TV:PANES `((LISP-WINDOW ERROR-HANDLER-LISP-LISTENER-PANE :LABEL NIL
                                                                 :IO-BUFFER ,IO-BUFFER)
                   (ARGS-WINDOW GRAY-ERROR-HANDLER-TEXT-SCROLL-PANE :LABEL "Args:"
                                :IO-BUFFER ,IO-BUFFER)
                   (LOCALS-WINDOW GRAY-ERROR-HANDLER-TEXT-SCROLL-PANE
                                  :LABEL "Locals:" :IO-BUFFER ,IO-BUFFER)
                   (STACK-WINDOW STACK-SCROLL-PANE
                                 :MARGIN-SCROLL-REGIONS ((:TOP "Bottom of stack")
                                                         (:BOTTOM "Top of stack"))
                                 :FLASHY-SCROLLING-REGION ((20 0.40s0 0.60s0)
                                                           (20 0.40s0 0.60s0))
                                 :LABEL NIL :IO-BUFFER ,IO-BUFFER)
                   (COMMAND-MENU-WINDOW TV:COMMAND-MENU-PANE :ITEM-LIST ,COMMAND-ALIST
                                        :IO-BUFFER ,IO-BUFFER)
                   (INSPECT-WINDOW TV:INSPECT-PANE :IO-BUFFER ,IO-BUFFER
                                                   :LABEL FONTS:CPTFONT
                                                   :BUTTON-DOCUMENTATION
                                                   ("Right gets object into error handler."))
                   (INSPECT-HISTORY-WINDOW ERROR-HANDLER-HISTORY-PANE
                                           :IO-BUFFER ,IO-BUFFER))


        TV:CONSTRAINTS
        '((ERROR-HANDLER-CONFIGURATION . ((INSPECT-WINDOW INSPECT-HISTORY-WINDOW ARGS-LOCS
                                           STACK-WINDOW COMMAND-MENU-WINDOW LISP-WINDOW)
                                          ((LISP-WINDOW 0.10s0 :LINES)
                                           (COMMAND-MENU-WINDOW :ASK :PANE-SIZE)
                                           (INSPECT-HISTORY-WINDOW 3 :LINES))
                                          ((INSPECT-WINDOW 0.3s0 :LINES)
                                           (ARGS-LOCS :HORIZONTAL (0.33s0 :LINES ARGS-WINDOW)
                                                      (ARGS-WINDOW LOCALS-WINDOW)
                                                      ((ARGS-WINDOW :EVEN)
                                                       (LOCALS-WINDOW :EVEN))))
                                          ((STACK-WINDOW :EVEN))))
          (ERROR-HANDLER-OLD-CONFIGURATION . ((INSPECT-WINDOW ARGS-LOCS STACK-WINDOW
                                                              COMMAND-MENU-WINDOW LISP-WINDOW)
                                              ((LISP-WINDOW 0.25s0 :LINES)
                                               (COMMAND-MENU-WINDOW :ASK :PANE-SIZE))
                                              ((INSPECT-WINDOW 0.33s0)
                                               (ARGS-LOCS :HORIZONTAL (0.33s0)
                                                          (ARGS-WINDOW LOCALS-WINDOW)
                                                          ((ARGS-WINDOW :EVEN)
                                                           (LOCALS-WINDOW :EVEN))))
                                              ((STACK-WINDOW :EVEN)))))))

(DEFMETHOD (ERROR-HANDLER-FRAME :AFTER :INIT) (IGNORE)
  ;;>>>>*****************
  (WITH-SELF-VARIABLES-BOUND
    (DOLIST (PANE TV:INTERNAL-PANES)
      (SET (CAR PANE) (CDR PANE)))
    (SEND SELF :SELECT-PANE LISP-WINDOW))
  (send self :set-selection-substitute lisp-window))


(DEFMETHOD (ERROR-HANDLER-FRAME :INSPECT-WINDOW-P) (W)
  (OR (EQ W INSPECT-HISTORY-WINDOW) (EQ W INSPECT-WINDOW)))

;(DEFMETHOD (ERROR-HANDLER-FRAME :SELECT) (&REST ARGS)
;  (LEXPR-SEND LISP-WINDOW :SELECT ARGS)
;  (SEND SELF :EXPOSE))

;(DEFMETHOD (ERROR-HANDLER-FRAME :DESELECT) (&REST ARGS)
;  (LEXPR-SEND LISP-WINDOW :DESELECT ARGS))

(DEFMETHOD (ERROR-HANDLER-FRAME :NAME-FOR-SELECTION) () TV:NAME)

(DEFMETHOD (ERROR-HANDLER-FRAME :SET-SENSITIVE-ITEM-TYPES) (VAL)
  (SEND ARGS-WINDOW :SET-SENSITIVE-ITEM-TYPES VAL)
  (SEND LOCALS-WINDOW :SET-SENSITIVE-ITEM-TYPES VAL)
  (SEND STACK-WINDOW :SET-SENSITIVE-ITEM-TYPES VAL))

(DEFMETHOD (ERROR-HANDLER-FRAME :SENSITIVE-ITEM-TYPES) ()
  (SEND LOCALS-WINDOW :SENSITIVE-ITEM-TYPES))

(DEFMETHOD (ERROR-HANDLER-FRAME :INSPECT-OBJECT) (THING)
  (SEND INSPECT-HISTORY-WINDOW :INSPECT-OBJECT THING INSPECT-WINDOW))

(DEFMETHOD (ERROR-HANDLER-FRAME :SETUP-SG) (SG AP)
  (SETQ FRAME-ALIST NIL)
  (SEND INSPECT-HISTORY-WINDOW :FLUSH-CONTENTS)
  (SETUP-STACK-FRAME-WINDOW STACK-WINDOW SG)
  (SEND SELF :SETUP-FRAME SG AP))

(DEFMETHOD (ERROR-HANDLER-FRAME :SETUP-FRAME) (SG AP &OPTIONAL FORCE-P ARG-CHANGED-FLAG
                                                     &AUX CODE ARGS LOCALS TEM)
  (OR TV:EXPOSED-P
      ;; If window not exposed, get its bit array in core so setup will go faster
      (SI:PAGE-IN-ARRAY TV:SCREEN-ARRAY))
  (SETQ TEM (ASSQ AP FRAME-ALIST))
  (COND (FORCE-P
         (SETQ FRAME-ALIST (DELQ TEM FRAME-ALIST))
         (SETQ TEM NIL)))
  ;; Set stuff up in most interesting order: args, then locals, then code
  (COND (TEM                                    ;Displayed this before
         (SEND ARGS-WINDOW :SETUP (THIRD TEM))
         (SEND LOCALS-WINDOW :SETUP (FOURTH TEM))
         (SEND INSPECT-HISTORY-WINDOW :INSPECT-OBJECT (SECOND TEM) INSPECT-WINDOW))
        (T
         (MULTIPLE-VALUE-SETQ (ARGS TEM) (SETUP-ARGS-WINDOW ARGS-WINDOW SG AP))
         (SETQ LOCALS (SETUP-LOCALS-WINDOW LOCALS-WINDOW SG AP TEM))
         (SETQ CODE (SETUP-INSPECT-WINDOW INSPECT-WINDOW SG AP INSPECT-HISTORY-WINDOW))
         (PUSH (LIST AP CODE ARGS LOCALS) FRAME-ALIST)))
  (IF ARG-CHANGED-FLAG
      (SEND STACK-WINDOW :REFRESH))
  (SEND STACK-WINDOW :PUT-ITEM-IN-WINDOW AP)
  (SEND STACK-WINDOW :SET-CURRENT-ITEM AP))

;;;Support routines for the stack frame window, a stack frame entry is just an AP.
;;;The common argument is the stack group.
(DEFUN SETUP-STACK-FRAME-WINDOW (WINDOW SG &AUX LIST)
  (DO ((AP (SG-AP SG) (SG-NEXT-ACTIVE SG AP)))
      ((NULL AP))
    (PUSH AP LIST))     ;No NREVERSE below, note.
  (SEND WINDOW :SETUP (LIST 'PRINT-STACK-FRAME SG LIST)))

;;; Given an SG and an AP, return the function and first and last+1 arg index into the RP
;;; (3 values).
(DEFUN STACK-FRAME-FUNCTION-AND-ARGS (SG AP)
  (DECLARE (VALUES FUNCTION ARGS-START ARGS-END))
  (LET* ((RP (SG-REGULAR-PDL SG))
         (FUNCTION (RP-FUNCTION-WORD RP AP))
         ;;If SELF is bound by this frame to an object whose handler for the first argument
         ;; to this frame is the function of this frame, print that object instead.
         (IDX (SG-FRAME-SPECIAL-PDL-RANGE SG AP)))
    (AND IDX
         (> IDX 0)
         (LET ((SP (SG-SPECIAL-PDL SG)))
           (AND (EQ (AREF SP (1+ IDX))
                    (%MAKE-POINTER DTP-LOCATIVE (LOCF (SYMBOL-VALUE 'SELF))))
                (LET* ((OBJECT (AREF SP IDX))
                       (HANDLER (GET-HANDLER-FOR OBJECT (AREF RP (1+ AP)))))
                  (AND (IF HANDLER
                           (EQ FUNCTION (IF (SYMBOLP HANDLER) (FSYMEVAL HANDLER) HANDLER))
                         (MEMQ (FUNCTION-NAME FUNCTION)
                               '(SI::INSTANCE-HASH-FAILURE
                                 SI::FLAVOR-UNCLAIMED-MESSAGE
                                 SI::REPORT-UNCLAIMED-MESSAGE)))
                       (SETQ FUNCTION OBJECT))))))
    (VALUES FUNCTION
            (1+ AP)
            (+ AP (RP-NUMBER-ARGS-SUPPLIED RP AP) 1))))

;get here if guy mouses "list" form of stack item in window-oriented error window.
(defprop stack-frame stack-frame-value-function tv:value-function)
(defun stack-frame-value-function (slot)
  (let* ((sg (send (third slot) :print-function-arg))
         (rp (sg-regular-pdl sg)))
    (multiple-value-bind (function first-arg last-arg)
        (stack-frame-function-and-args sg (cadr slot))
      (list function
            (loop for idx from first-arg below last-arg
                  collect (aref rp idx))))))

;;;Print a frame  (ITEM is AP)
(DEFUN PRINT-STACK-FRAME (ITEM SG STREAM IGNORE)
  (SEND STREAM :ITEM1 ITEM 'STACK-FRAME #'PRINT-STACK-FRAME-1 SG))

(DEFUN PRINT-STACK-FRAME-1 (AP STREAM SG)
  (MULTIPLE-VALUE-BIND (FUNCTION ARGS-START ARGS-END) (STACK-FRAME-FUNCTION-AND-ARGS SG AP)
    (SEND STREAM :TYO (SI:PTTBL-OPEN-PAREN *READTABLE*))
    (SEND STREAM :ITEM1 FUNCTION :FUNCTION (LAMBDA (FUNCTION STREAM)
                                             (PRIN1 (FUNCTION-NAME FUNCTION) STREAM)))
    (DO ((I ARGS-START (1+ I))
         (L 1 (1+ L))
         (RP (SG-REGULAR-PDL SG)))
        (( I ARGS-END)
         (SEND STREAM :TYO (SI:PTTBL-CLOSE-PAREN *READTABLE*)))
      (SEND STREAM :TYO (SI:PTTBL-SPACE *READTABLE*))
      (SEND STREAM :ITEM1 (AREF RP I) :VALUE #'TV:PRINT-ITEM-CONCISELY)
      (WHEN (AND *PRINT-LENGTH* ( L *PRINT-LENGTH*))
        (SEND STREAM :STRING-OUT (SI:PTTBL-PRINLENGTH *READTABLE*))
        (RETURN NIL)))))

;;;Support routines for the args, locals, and specials windows
;;;Entries are fixed strings, or lists of name, val, and number
;;;Common arg is the type of entries present
(DEFUN SETUP-ARGS-WINDOW (WINDOW SG AP &AUX (RP (SG-REGULAR-PDL SG)) LIST
                                            FUNCTION NARGS-SUPPLIED NARGS-TO-PRINT
                                            NARGS-EXPECTED NARGS-REQUIRED
                                            LEXPR-CALL REST-ARG-P REST-ARG-VALUE)
  (SETQ FUNCTION (RP-FUNCTION-WORD RP AP)
        NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))
  (COND ((OR (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER) (CONSP FUNCTION))
         (SETQ NARGS-REQUIRED
               (LDB %%ARG-DESC-MIN-ARGS (ARGS-INFO FUNCTION)))
         (SETQ NARGS-EXPECTED
               (LDB %%ARG-DESC-MAX-ARGS (ARGS-INFO FUNCTION)))))
  (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-P LEXPR-CALL)
    (SG-REST-ARG-VALUE SG AP))
  (SETQ NARGS-TO-PRINT (SG-NUMBER-OF-SPREAD-ARGS SG AP))
  ;; Store the individual args.
  (DOTIMES (I NARGS-TO-PRINT)
    (AND (= I NARGS-SUPPLIED)                   ;These "args" weren't supplied
         (PUSH (IF (AND NARGS-REQUIRED (< I NARGS-REQUIRED))
                   "   --Missing args:--" "   --Defaulted args:--")
               LIST))
    (AND NARGS-EXPECTED (= I NARGS-EXPECTED)    ;Called with too many args
         (PUSH "   --Extraneous args:--" LIST))
    (LET ((MISSING (AND NARGS-REQUIRED
                        (> NARGS-REQUIRED NARGS-SUPPLIED)
                        ( I NARGS-SUPPLIED))))
      (PUSH (LIST (ARG-NAME FUNCTION I) ;Arg name
                  (OR MISSING (AREF RP (+ AP I 1)))     ;Value
                  (IF (NOT MISSING) I (LIST ':NOVALUE I))) ;Number
            LIST)))
  ;; Print the rest arg if any.
  (AND (OR REST-ARG-P LEXPR-CALL)
       (PUSH (LIST (AND REST-ARG-P (LOCAL-NAME FUNCTION 0))     ;Name
                   REST-ARG-VALUE       ;Value
                   (IF REST-ARG-P "Rest arg" "Extraneous rest arg"))
             LIST))
  (VALUES (SEND WINDOW :SETUP (LIST 'PRINT-ARG-OR-LOCAL '(ARG "Arg")
                                    (NREVERSE LIST)))
          REST-ARG-P))

;;;REST-ARG-P means that local 0 is in the other window and should not be duplicated
(DEFUN SETUP-LOCALS-WINDOW (WINDOW SG AP REST-ARG-P)
  (LET* ((RP (SG-REGULAR-PDL SG))
         (SP (SG-SPECIAL-PDL SG))
         (FUNCTION (RP-FUNCTION-WORD RP AP))
         self-value
         (list ()))
    (WHEN (TYPEP FUNCTION 'COMPILED-FUNCTION)
      ;; Print the locals if this is a fef
      (DO ((N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION))
           (I 0 (1+ I))
           (J (+ AP (RP-LOCAL-BLOCK-ORIGIN RP AP)) (1+ J)))
          (( I N-LOCALS))
        (UNLESS (AND REST-ARG-P (ZEROP I))      ;Don't show rest arg twice
          (PUSH (LIST (LOCAL-NAME FUNCTION I)   ;Name
                      (AREF RP J)               ;Value
                      I)                        ;Number
                LIST))))
    (MULTIPLE-VALUE-BIND (START END)
        (SG-FRAME-SPECIAL-PDL-RANGE SG AP)
      (WHEN START
        (PUSH "" LIST)
        (PUSH "Specials:" LIST)
        (DO ((I START (+ I 2)))
            (( I END))
          (IF (EQ 'SELF (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I))))
              (SETQ SELF-VALUE (AREF SP I)))
          (PUSH (LIST (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))        ;Name
                      (if (location-boundp (locf (aref sp i)))                  ;value
                        (AREF SP I)
                      "void"))
                LIST))))
    ;; If SELF is mentioned in this frame, include its instance variables:
    (IF (AND SELF-VALUE
             (TYPEP SELF-VALUE 'INSTANCE))
        (LET* ((SELF-FLAVOR
                 (SI:INSTANCE-FLAVOR SELF-VALUE))
               (SELF-VARS (SI:FLAVOR-ALL-INSTANCE-VARIABLES-SLOW SELF-FLAVOR)))
          (PUSH "" LIST)
          (PUSH "Instance variables of SELF:" LIST)
          (DO ((SV SELF-VARS (CDR SV))
               (I 1 (1+ I)))
              ((NULL SV))
            (UNLESS (SI:ASSQ-CAREFUL (CAR SV) LIST)     ;specials already mentioned
              (PUSH (LIST (CAR SV)
                          (if (location-boundp (locf (%instance-ref self-value i)))
                              (%INSTANCE-REF SELF-VALUE I)
                            "void"))
                    LIST)))))
    (SEND WINDOW :SETUP `(PRINT-ARG-OR-LOCAL (LOCAL "Local") ,(NREVERSE LIST)))))

;(DEFUN PRINT-SPECIAL-PDL-RANGE (SG START END &OPTIONAL (STREAM *STANDARD-OUTPUT*))
;  (DO ((SP (SG-SPECIAL-PDL SG))
;       (I START (+ I 2)))
;      (( I END))
;    (FORMAT STREAM "~&~S: ~S~%" (SYMBOL-FROM-VALUE-CELL-LOCATION (AREF SP (1+ I)))
;                               (AREF SP I))))

(DEFUN PRINT-ARG-OR-LOCAL (ITEM TYPE STREAM IGNORE
                           &AUX NUMBER NAME VALUE TYPE-NAME NOVALUE ERROR)
  (IF (STRINGP ITEM)
      (SEND STREAM :STRING-OUT ITEM)
      (SETQ TYPE-NAME (SECOND TYPE)
            TYPE (FIRST TYPE)
            NAME (FIRST ITEM)
            VALUE (SECOND ITEM)
            NUMBER (THIRD ITEM))
      (AND (CONSP NUMBER) (SETQ NOVALUE (FIRST NUMBER) NUMBER (SECOND NUMBER)))
      (COND ((NULL NUMBER))
            ((STRINGP NUMBER) (SEND STREAM :STRING-OUT NUMBER))
            (T (FORMAT STREAM "~A ~D" TYPE-NAME NUMBER)))
      (AND NAME
           (COND (NUMBER
                  (SEND STREAM :STRING-OUT " (")
                  (SEND STREAM :ITEM1 (LIST NAME NUMBER) TYPE
                                      (LAMBDA (X STREAM)
                                        (SEND STREAM :STRING-OUT (SYMBOL-NAME (CAR X)))))
                  (SEND STREAM :TYO #/)))
                 (T
                  (SEND STREAM :ITEM1 NAME TYPE))))
      (COND ((NEQ NOVALUE :NOVALUE)
             (SEND STREAM :STRING-OUT ": ")
             (MULTIPLE-VALUE (NIL ERROR)
               (CATCH-ERROR (SEND STREAM :ITEM1 VALUE :VALUE) NIL))
             (IF ERROR (SEND STREAM :STRING-OUT "<<unprintable>>"))))))

;;;Support routines for the code window
(DEFUN SETUP-INSPECT-WINDOW (INSPECT-WINDOW SG AP INSPECT-HISTORY-WINDOW
                             &AUX FUNCTION (LABEL "") CODE)
  (SETQ FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) AP))
  (AND (ATOM FUNCTION)                  ;Print nothing for interpreted code
       (LET ((NAME (FUNCTION-NAME FUNCTION)))
         (SETQ LABEL (COND ((STRINGP NAME) NAME)
                           ((SYMBOLP NAME) (SYMBOL-NAME NAME))
                           (T (FORMAT NIL "~S" NAME))))))
  (SEND INSPECT-HISTORY-WINDOW :INSPECT-OBJECT
        (SETQ CODE (TV:MAKE-STACK-FRAME TV:STACK-FRAME-SG SG
                                        TV:STACK-FRAME-AP AP
                                        TV:STACK-FRAME-FUNCTION-NAME LABEL))
        INSPECT-WINDOW)
  CODE)

;;;Entry from the other error handler
(DEFUN COM-WINDOW-ERROR-HANDLER (SG ETE)
  "Use a window-oriented error handler to debug the stack."
  (IF (EQ *TERMINAL-IO* SI:COLD-LOAD-STREAM)
      (FORMAT T "~&The window-based debugger cannot be invoked since we are using the cold load stream.")
    (FORMAT T "~&Transfer to Window-Oriented Debugger!~%")
    (USING-RESOURCE (WINDOW ERROR-HANDLER-FRAME TV:DEFAULT-SCREEN)
      (SEND WINDOW :SET-SENSITIVE-ITEM-TYPES '(:VALUE :FUNCTION STACK-FRAME))
      (WINDOW-COMMAND-LOOP SG ETE WINDOW))))

(COMPILE-FLAVOR-METHODS ERROR-HANDLER-FRAME
                        ERROR-HANDLER-TEXT-SCROLL-PANE GRAY-ERROR-HANDLER-TEXT-SCROLL-PANE
                        STACK-SCROLL-PANE ERROR-HANDLER-HISTORY-PANE)

(TV:DEFWINDOW-RESOURCE ERROR-HANDLER-FRAME ()
  :MAKE-WINDOW (ERROR-HANDLER-FRAME)
  :REUSABLE-WHEN :DEACTIVATED
  :initial-copies 0
  :initializer (when (or ( (send object :width) (send tv:main-screen :inside-width))
                         ( (send object :height) (send tv:main-screen :inside-height))
                         ( (send object :x-offset) 0)
                         ( (send object :y-offset) 0))
                 (send object :change-of-size-or-margins
                       :left 0
                       :top 0
                       :width (send tv:main-screen :inside-width)
                       :height (send tv:main-screen :inside-height))))

;;;The actual window.  Bound in the error handler stack group
;;;to the error handler window being run by that stack group.
(DEFVAR ERROR-HANDLER-WINDOW)

;;;The command loop
(DEFUN WINDOW-COMMAND-LOOP (*ERROR-SG* ERROR-OBJECT ERROR-HANDLER-WINDOW
                            &AUX (*EVALHOOK* NIL) PKG
                                 (*PACKAGE* *PACKAGE*)
                                 (WINDOW-ERROR-HANDLER-OLD-WINDOW T)
                                 (*TERMINAL-IO* (SEND ERROR-HANDLER-WINDOW :LISP-WINDOW)))
  (SEND ERROR-HANDLER-WINDOW :SETUP-SG *ERROR-SG* *CURRENT-FRAME*)
  (SEND *TERMINAL-IO* :CLEAR-WINDOW)
  (TV:WINDOW-CALL (ERROR-HANDLER-WINDOW :DEACTIVATE)
    (SETQ WINDOW-ERROR-HANDLER-OLD-WINDOW (OR TV::.CURRENT-WINDOW. T))
    (SEND ERROR-OBJECT :PRINT-ERROR-MESSAGE *ERROR-SG* T *STANDARD-OUTPUT*)
    (SEND *TERMINAL-IO* :CLEAR-INPUT)
    (CATCH 'COMW-EXIT
      (DO ((-)
           (+ (SYMEVAL-IN-STACK-GROUP '- *ERROR-SG*))
           (* (SYMEVAL-IN-STACK-GROUP '* *ERROR-SG*)))
          (NIL)
        (SETQ PKG (SYMEVAL-IN-STACK-GROUP '*PACKAGE* *ERROR-SG*))
        (SETQ *PACKAGE* (IF (TYPEP PKG 'PACKAGE) PKG (PKG-FIND-PACKAGE "USER")))
        (CATCH 'QUIT
          (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to window debugger command loop.")
            (SEND *STANDARD-OUTPUT* :FRESH-LINE)
            (MULTIPLE-VALUE-BIND (SPECIAL-CHAR SEXP)
                (WINDOW-COMMAND-LOOP-READ T)
              (IF SPECIAL-CHAR
                  (PROCESS-SPECIAL-COMMAND SPECIAL-CHAR *ERROR-SG* ERROR-OBJECT)
                (LET ((RESULTS (SG-EVAL-IN-FRAME *ERROR-SG* (SETQ - SEXP) *CURRENT-FRAME* T)))
                  (SETQ + -)
                  (WHEN (NEQ RESULTS ERROR-FLAG)
                    (SETQ * (CAR RESULTS))
                    (MAPC #'PRINT RESULTS)))))))))))

(DEFUN COMW-EXIT (IGNORE IGNORE)
  (THROW 'COMW-EXIT NIL))

(DEFUN PROCESS-SPECIAL-COMMAND (LIST SG ERROR-OBJECT &AUX OPERATION VALUE WINDOW)
  (SETQ OPERATION (FIRST LIST)
        VALUE (SECOND LIST)
        WINDOW (THIRD LIST))
  (WHEN (EQ OPERATION ':MENU)
    (SETQ WINDOW (FOURTH LIST)
          VALUE (SEND WINDOW :EXECUTE VALUE)))
  (AND (NOT (MEMQ OPERATION '(:MOUSE-BUTTON :APPLY)))
       (SEND ERROR-HANDLER-WINDOW :INSPECT-WINDOW-P WINDOW)
       (IF (= (FOURTH LIST) #/MOUSE-1-1)
           (SETQ OPERATION ':INSPECT)
         (SETQ OPERATION ':VALUE
                 VALUE (TV:INSPECT-REAL-VALUE LIST))))
  (WHEN (AND (EQ OPERATION ':MENU) (MEMQ VALUE '(T NIL-VALUE)))
    (SEND *STANDARD-OUTPUT* :LINE-OUT (IF (SETQ VALUE (EQ VALUE T)) "T" "()"))
    (SETQ OPERATION ':VALUE + VALUE))
  (case operation
    (:APPLY
     (APPLY (CAR VALUE) (CDR VALUE)))
    (:LINE-AREA
     (SETQ *CURRENT-FRAME* VALUE)
     (SEND ERROR-HANDLER-WINDOW :SETUP-FRAME SG *CURRENT-FRAME*))
    (:MENU
     (SEND VALUE SG ERROR-OBJECT))              ;Execute a regular menu command
    (:INSPECT
     (SEND ERROR-HANDLER-WINDOW :INSPECT-OBJECT (TV:INSPECT-REAL-VALUE LIST)))
    (stack-frame
     (send error-handler-window :inspect-object (stack-frame-function-and-args sg value)))
    ((:VALUE :FUNCTION SPECIAL ARG LOCAL)
     (SETQ +++ ++ ++ +)
     (when (MEMQ OPERATION '(SPECIAL ARG LOCAL))
       (case operation
         ((ARG LOCAL)
          (PRIN1 (FIRST VALUE))
          (LET ((IDX (SECOND VALUE)))
            (IF (NOT (NUMBERP IDX))
                (AND (EQUALP IDX "Rest arg")
                     (SETQ VALUE (SG-REST-ARG-VALUE SG *CURRENT-FRAME*)))
              (LET ((RP (SG-REGULAR-PDL SG)))
                (SETQ + (ALOC RP
                              (+ *CURRENT-FRAME* IDX
                                 (IF (EQ OPERATION 'ARG) 1
                                   (RP-LOCAL-BLOCK-ORIGIN
                                     RP *CURRENT-FRAME*))))))
              (SETQ VALUE (CAR +)))))
         (T
          (SETQ + (PRIN1 VALUE))
          (SETQ VALUE (SYMEVAL VALUE))))
       (TERPRI))
     (SEND ERROR-HANDLER-WINDOW :INSPECT-OBJECT (TV:INSPECT-REAL-VALUE LIST))
     (SETQ *** ** ** * * (PRIN1 VALUE)))
    (T (TV:BEEP))))

;;;This reads a form or special command (a list in the input stream)
(DEFUN WINDOW-COMMAND-LOOP-READ (&OPTIONAL PREEMPTABLE)
  (DO ((CHAR -1) (TYPEAHEAD)) (())
    (UNWIND-PROTECT
      (PROGN
        (UNLESS PREEMPTABLE
          (SETQ TYPEAHEAD (SEND *TERMINAL-IO* :OLD-TYPEAHEAD))
          (SEND *TERMINAL-IO* :SET-OLD-TYPEAHEAD NIL))
        (UNLESS (SEND *TERMINAL-IO* :OLD-TYPEAHEAD)
          (SETQ CHAR (SEND *TERMINAL-IO* :ANY-TYI)))
        (COND ((CONSP CHAR) (RETURN CHAR))
              ((= CHAR #/FORM) (SEND *TERMINAL-IO* :CLEAR-WINDOW))
              ((= CHAR #/RUBOUT))
              ((= CHAR #/END)
                ;Unfortunately, this has to look like a command menu frob, sort of.
               (RETURN (values `(:APPLY (COMW-EXIT NIL NIL) NIL ,*TERMINAL-IO*) nil)))
              (T
               (AND ( CHAR 0) (SEND *TERMINAL-IO* :UNTYI CHAR))
               (MULTIPLE-VALUE-BIND (SEXP FLAG)
                   (SEND *TERMINAL-IO* :PREEMPTABLE-READ
                                       '((:FULL-RUBOUT :FULL-RUBOUT)) #'SI:READ-FOR-TOP-LEVEL)
                 (COND ((EQ FLAG :MOUSE-CHAR)
                        (RETURN SEXP))
                       ((EQ FLAG :FULL-RUBOUT))
                       (T
                        (RETURN (VALUES NIL SEXP))))))))
      (OR PREEMPTABLE
          (SEND *TERMINAL-IO* :SET-OLD-TYPEAHEAD TYPEAHEAD)))))

;;;This gets an object to return or something
(DEFUN WINDOW-READ-OBJECT (KEYWORD &REST FORMAT-STRING-AND-ARGS
                           &AUX SPECIAL SEXP ASK-P OLD-SI-TYPES)
  (COND ((EQ KEYWORD ':EVAL-READ)
         (SETQ OLD-SI-TYPES (SEND ERROR-HANDLER-WINDOW :SENSITIVE-ITEM-TYPES))
         (UNWIND-PROTECT
             (PROG ()
                   (SEND ERROR-HANDLER-WINDOW :SET-SENSITIVE-ITEM-TYPES
                                              '(:VALUE :FUNCTION STACK-FRAME))
                RETRY
                   (APPLY #'FORMAT T FORMAT-STRING-AND-ARGS)
                   (MULTIPLE-VALUE (SPECIAL SEXP)
                     (WINDOW-COMMAND-LOOP-READ))
                   (COND ((CONSP SPECIAL)
                          (IF (SEND ERROR-HANDLER-WINDOW :INSPECT-WINDOW-P (THIRD SPECIAL))
                              (SETQ SEXP (TV:INSPECT-REAL-VALUE SPECIAL) ASK-P T)
                            (LET ((TYPE (FIRST SPECIAL)))
                              (COND ((EQ TYPE ':APPLY)
                                     (APPLY (CAR (CADR SPECIAL)) (CDR (CADR SPECIAL))))
                                    ((EQ TYPE ':VALUE)
                                     (SETQ SEXP (SECOND SPECIAL) ASK-P T))
                                    ((AND (EQ TYPE ':MENU)
                                          (MEMQ (SETQ SEXP (SEND (FOURTH SPECIAL) :EXECUTE
                                                                 (SECOND SPECIAL)))
                                                '(T NIL-VALUE)))
                                     (SETQ SEXP (EQ SEXP T) ASK-P NIL))
                                    (T
                                     (TV:BEEP)
                                     (GO RETRY))))))
                         (T
                          (SETQ ASK-P (SI:TRIVIAL-FORM-P SEXP)
                                SEXP (LET ((OTOC (SG-FLAGS-TRAP-ON-CALL *ERROR-SG*)))
                                       (UNWIND-PROTECT
                                           (PROGN (SETF (SG-FLAGS-TRAP-ON-CALL *ERROR-SG*) 0)
                                                  (CAR (SG-EVAL-IN-FRAME *ERROR-SG* SEXP
                                                                         *CURRENT-FRAME* T)))
                                         (SETF (SG-FLAGS-TRAP-ON-CALL *ERROR-SG*) OTOC))))))
                   (AND ASK-P (COND ((NOT (WINDOW-Y-OR-N-P "The object is ~S, ok? " SEXP))
                                     (TERPRI)
                                     (GO RETRY))))
                   (RETURN SEXP))
           (SEND ERROR-HANDLER-WINDOW :SET-SENSITIVE-ITEM-TYPES OLD-SI-TYPES)))
        ((EQUAL KEYWORD '(:FQUERY))
         (APPLY #'WINDOW-Y-OR-N-P FORMAT-STRING-AND-ARGS))
        (T (APPLY #'PROMPT-AND-READ KEYWORD FORMAT-STRING-AND-ARGS))))

(DEFUN WINDOW-Y-OR-N-P (STRING &REST FORMAT-ARGS)
  (APPLY #'FORMAT T STRING FORMAT-ARGS)
  (DO ((CH)) (NIL)
    (SETQ CH (SEND *STANDARD-INPUT* :ANY-TYI))
    (AND (EQ (CAR-SAFE CH) ':MENU)
         (MEMQ (SETQ CH (SEND (FOURTH CH) :EXECUTE (SECOND CH))) '(T NIL-VALUE))
         (SETQ CH (IF (EQ CH T) #/Y #/N)))
    (when (eq (car-safe ch) ':apply)
      (apply (car (cadr ch)) (cdr (cadr ch))))
    (COND ((MEMQ CH '(#/Y #/y #/SP))
           (PRINC "yes")
           (RETURN T))
          ((MEMQ CH '(#/N #/n #/RUBOUT))
           (PRINC "no")
           (RETURN NIL))
          (T
           (PRINC "(Y or N)")))))

(DEFUN WINDOW-READ-FUNCTION (ACTION &OPTIONAL ALLOW-T RETURN-STACK-FRAMES)
  (FORMAT T "~&Type or mouse a function ~A, or mouse NIL to abort~:[, or T for nothing~]:~%"
          ACTION (NOT ALLOW-T))
  (MULTIPLE-VALUE-BIND (SPECIAL FUNCTION)
      (WINDOW-COMMAND-LOOP-READ)
    (AND SPECIAL
         (SETQ FUNCTION
               (ECASE (FIRST SPECIAL)
                 (:MENU (AND (EQ (SEND (FOURTH SPECIAL) :EXECUTE (SECOND SPECIAL)) T)
                             ALLOW-T))
                 (STACK-FRAME (IF RETURN-STACK-FRAMES SPECIAL
                                (STACK-FRAME-FUNCTION-AND-ARGS *ERROR-SG* (SECOND SPECIAL))))
                 (:LINE-AREA (IF RETURN-STACK-FRAMES (LIST 'STACK-FRAME (SECOND SPECIAL))
                               (STACK-FRAME-FUNCTION-AND-ARGS *ERROR-SG* (SECOND SPECIAL))))
                 ((SPECIAL ARG LOCAL) (FIRST (SECOND SPECIAL)))
                 ((:VALUE :FUNCTION SPECIAL) (SECOND SPECIAL))
                 (:APPLY
                  (APPLY (CAR (CADR SPECIAL)) (CDR (CADR SPECIAL))))
                 ((symbol-function symbol-value)
                  (third (second special))))))
    (AND (CLOSUREP FUNCTION) (SETQ FUNCTION (CAR (%MAKE-POINTER DTP-LIST FUNCTION))))
    (COND ((MEMQ (DATA-TYPE FUNCTION) '(DTP-ENTITY DTP-INSTANCE DTP-SELECT-METHOD))
           (SETQ SPECIAL (WINDOW-READ-THING "~&Type or mouse a message name for ~S:~%"
                                            FUNCTION))
           (LET ((HANDLER (GET-HANDLER-FOR FUNCTION SPECIAL)))
             (OR HANDLER (FORMAT T "~&~S does not handle the ~S message.~%" FUNCTION SPECIAL))
             (SETQ FUNCTION HANDLER)))
          ((NULL FUNCTION)
           (FORMAT T "~&Aborted.~%")))
    FUNCTION))

(DEFUN WINDOW-READ-THING (PROMPT &REST FORMAT-ARGS)
  (APPLY #'FORMAT T PROMPT FORMAT-ARGS)
  (MULTIPLE-VALUE-BIND (SPECIAL THING)
      (WINDOW-COMMAND-LOOP-READ)
    (IF SPECIAL
        (IF (SEND ERROR-HANDLER-WINDOW :INSPECT-WINDOW-P (THIRD SPECIAL))
            (TV:INSPECT-REAL-VALUE SPECIAL)
          (ECASE (FIRST SPECIAL)
            (:MENU
             (EQ (SEND (FOURTH SPECIAL) :EXECUTE (SECOND SPECIAL)) T))
            ((SPECIAL ARG LOCAL)
             (FIRST (SECOND SPECIAL)))
            ((:VALUE :FUNCTION SPECIAL)
             (SECOND SPECIAL))
            (:APPLY
             (APPLY (CAR (CADR SPECIAL)) (CDR (CADR SPECIAL))))))
      (CAR (SG-EVAL *ERROR-SG* THING)))))

;;;The commands

(DEFUN COMW-PROCEED (*ERROR-SG* ERROR-OBJECT &REST IGNORE
                     &AUX PROCEED-TYPES PROCEED-TYPE
                     (RESUME-HANDLERS (SYMEVAL-IN-STACK-GROUP 'CONDITION-RESUME-HANDLERS *ERROR-SG*)))
  (ERROR-HANDLER-MUST-BE-RUNNING)
  (SETQ PROCEED-TYPES (SEND ERROR-OBJECT :USER-PROCEED-TYPES
                            (SG-CONDITION-PROCEED-TYPES *ERROR-SG* ERROR-OBJECT)))
  (IF (NOT PROCEED-TYPES)
      (FORMAT T "There is no way to proceed from this error.~%")
    (WHEN
      (SETQ PROCEED-TYPE
            (TV:MENU-CHOOSE (APPEND
                              (MAPCAR (LAMBDA (PROCEED-TYPE)
                                        (LIST (FORMAT:OUTPUT NIL
                                                (SEND ERROR-OBJECT :DOCUMENT-PROCEED-TYPE
                                                      PROCEED-TYPE *STANDARD-OUTPUT*
                                                      RESUME-HANDLERS))
                                              PROCEED-TYPE))
                                      PROCEED-TYPES)
                              (MAPCAR (LAMBDA (SPECIAL-COMMAND)
                                        (LIST (FORMAT:OUTPUT NIL
                                                (SEND ERROR-OBJECT :DOCUMENT-SPECIAL-COMMAND
                                                      SPECIAL-COMMAND *STANDARD-OUTPUT*))
                                              SPECIAL-COMMAND))
                                      *SPECIAL-COMMANDS*))
                            "Which type of proceed?"))
      (SEND ERROR-OBJECT :PROCEED-ASKING-USER PROCEED-TYPE
            'PROCEED-ERROR-SG
            'WINDOW-READ-OBJECT)))
  NIL)

(DEFUN COMW-WHAT-ERROR (SG ERROR-OBJECT)
  (SEND ERROR-OBJECT :PRINT-ERROR-MESSAGE SG T *STANDARD-OUTPUT* ))

(DEFUN COMW-SEARCH (SG IGNORE &AUX KEY AP)
  (FORMAT T "String to search for (end with RETURN):~%")
  (SETQ KEY (READLINE))
  (SETQ AP (DO ((AP *CURRENT-FRAME* (SG-NEXT-ACTIVE SG AP))
                (RP (SG-REGULAR-PDL SG))
                (NAME))
               ((NULL AP) NIL)
             (SETQ NAME (FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))
             (SETQ NAME (COND ((STRINGP NAME) NAME)
                              ((SYMBOLP NAME) (STRING NAME))
                              (T (FORMAT NIL "~S" NAME))))
             (AND (STRING-SEARCH KEY NAME) (RETURN AP))))
  (COND ((NULL AP)
         (FORMAT T "Search failed.~%"))
        (T
         (SETQ *CURRENT-FRAME* AP)
         (SEND ERROR-HANDLER-WINDOW :SETUP-FRAME SG *CURRENT-FRAME*))))

;(DEFUN COM-PRINT-SPECIALS (SG IGNORE &AUX START END)
;  (MULTIPLE-VALUE (START END)
;    (SG-FRAME-SPECIAL-PDL-RANGE SG *CURRENT-FRAME*))
;  (IF START
;      (PRINT-SPECIAL-PDL-RANGE SG START END)
;      (PRINC "This frame has no special variable bindings.")))

(DEFUN COMW-DESCRIBE (IGNORE IGNORE &AUX THING)
  (AND (SETQ THING (WINDOW-READ-THING "~&Type or mouse something to describe:~%"))
       (DESCRIBE THING)))                               ;This should go to a typeout stream

(DEFUN COMW-INSPECT (IGNORE IGNORE &AUX THING)
  (AND (SETQ THING (WINDOW-READ-THING "~&Type or mouse something to inspect:~%"))
       (SEND ERROR-HANDLER-WINDOW :INSPECT-OBJECT THING)))

(DEFUN COMW-ARGLIST (SG IGNORE &AUX FUNCTION)
  (AND (SETQ FUNCTION (WINDOW-READ-FUNCTION "for arglist" NIL T))
       (COND ((AND (SYMBOLP FUNCTION) (NOT (FBOUNDP FUNCTION)))
              (FORMAT T "~&~S is not defined." FUNCTION))
             ((AND (CONSP FUNCTION) (EQ (FIRST FUNCTION) 'STACK-FRAME))
              (PRINT-FRAME-ARGLIST SG (SECOND FUNCTION)))
             (T
              (SETQ FUNCTION (FUNCTION-NAME FUNCTION))
              (FORMAT T "~&~S: ~:A~%" FUNCTION (ARGLIST FUNCTION))))))

(DEFUN PRINT-FRAME-ARGLIST (SG AP)
  (LET ((STR1 (MAKE-STRING 50. :FILL-POINTER 0))
        (STR2  (MAKE-STRING 50. :FILL-POINTER 0)))
    (VECTOR-PUSH-EXTEND (SI:PTTBL-OPEN-PAREN *READTABLE*) STR1)
    (MULTIPLE-VALUE-BIND (FUNCTION ARGS-START ARGS-END)
        (STACK-FRAME-FUNCTION-AND-ARGS SG AP)
      (FORMAT STR1 "~S" (FUNCTION-NAME FUNCTION))
      (VECTOR-PUSH-EXTEND (SI:PTTBL-SPACE *READTABLE*) STR1)
      (VECTOR-PUSH-EXTEND (SI:PTTBL-SPACE *READTABLE*) STR2)
      (DO ((ARGLIST (ARGLIST FUNCTION) (CDR ARGLIST))
           (RP (SG-REGULAR-PDL SG))
           (I ARGS-START)
           (FLAG NIL T))
          ((AND (NULL ARGLIST) ( I ARGS-END)))
        (LET ((I1 (IF FLAG 1 0)) (I2 (IF FLAG 1 0)))
          (LET ((LEN1 (FILL-POINTER STR1))
                (LEN2 (FILL-POINTER STR2)))
            (COND ((> LEN1 LEN2)
                   (SETQ I2 (1+ (- LEN1 LEN2))))
                  ((< LEN1 LEN2)
                   (SETQ I1 (1+ (- LEN2 LEN1))))))
          (WHEN ARGLIST
            (DOTIMES (I I1) (VECTOR-PUSH-EXTEND #/SPACE STR1))
            (format str1 "~S" (car arglist))
            (AND (MEMQ (CAR ARGLIST) '(&OPTIONAL &REST))
                 (SETQ FLAG '&MUMBLE)))
          (WHEN (< I ARGS-END)
            (DOTIMES (I I2) (VECTOR-PUSH-EXTEND #/SPACE STR2))
            (UNLESS (EQ FLAG '&MUMBLE)
              (format str2 "~S" (aref rp i))
              (SETQ I (1+ I)))))))
    (VECTOR-PUSH-EXTEND (SI:PTTBL-CLOSE-PAREN *READTABLE*) STR2)
    (VECTOR-PUSH-EXTEND (SI:PTTBL-CLOSE-PAREN *READTABLE*) STR1)
    (SEND *STANDARD-OUTPUT* :FRESH-LINE)
    (SEND *STANDARD-OUTPUT* :LINE-OUT STR1)
    (SEND *STANDARD-OUTPUT* :LINE-OUT STR2)))

(DEFUN STACK-FRAME-INTO-LIST (AP SG &AUX LIST)
  (MULTIPLE-VALUE-BIND (FUNCTION ARGS-START ARGS-END)
      (STACK-FRAME-FUNCTION-AND-ARGS SG AP)
    (SETQ LIST (NCONS (FUNCTION-NAME FUNCTION)))
    (DO ((I ARGS-START (1+ I))
         (RP (SG-REGULAR-PDL SG)))
        (( I ARGS-END))
      (PUSH (AREF RP I) LIST))
    (NREVERSE LIST)))

(DEFUN COMW-EDIT (IGNORE IGNORE &AUX THING)
  (AND (SETQ THING (WINDOW-READ-FUNCTION "to edit" T))
       (PRIN1 (ED (IF (EQ THING T) NIL (FUNCTION-NAME THING))))))

(DEFUN COMW-SET-ARG (SG IGNORE &AUX CHAR)
  (FORMAT T "~&Mouse an argument or local to modify:~%")
  (LET ((OLD-SI-TYPES (SEND ERROR-HANDLER-WINDOW :SENSITIVE-ITEM-TYPES)))
    (SEND ERROR-HANDLER-WINDOW :SET-SENSITIVE-ITEM-TYPES '(ARG LOCAL))
    (UNWIND-PROTECT
        (SETQ CHAR (SEND *STANDARD-INPUT* :ANY-TYI))
      (SEND ERROR-HANDLER-WINDOW :SET-SENSITIVE-ITEM-TYPES OLD-SI-TYPES)))
  (IF (NOT (MEMQ (CAR-SAFE CHAR) '(LOCAL ARG)))
      (FORMAT T "~&That is not an argument or local~%")
    (LET ((IDX (CADADR CHAR)))
      (IF (NOT (NUMBERP IDX))
          (FORMAT T "~&Cannot set rest arg.")
        (LET ((NEW-OBJ (READ-OBJECT :EVAL-READ
                                    (FORMAT NIL "Value to substitute for ~A: "
                                            (CAADR CHAR)))))
          (LET ((RP (SG-REGULAR-PDL SG)))
            (ASET NEW-OBJ RP
                  (+ *CURRENT-FRAME* IDX (IF (EQ (CAR CHAR) 'ARG) 1
                                         (RP-LOCAL-BLOCK-ORIGIN RP *CURRENT-FRAME*))))))))
    (SEND ERROR-HANDLER-WINDOW :SETUP-FRAME SG *CURRENT-FRAME* T T)))

(defun comw-flush-cache (sg ignore) sg
  (let ((ihw (send error-handler-window :inspect-history-window))
        (iw  (send error-handler-window :inspect-window)))
    (send ihw :set-cache nil)
    (tv::inspect-setup-object (send iw :current-object) iw)))

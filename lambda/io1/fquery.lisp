;;;-*- Mode:LISP; Package:FORMAT; Base:8; Readtable:T -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; (FQUERY OPTIONS FORMAT-STRING &REST FORMAT-ARGS)
;;; OPTIONS is a PLIST.  Defined indicators are:
;;; :MAKE-COMPLETE boolean.  Send a :MAKE-COMPLETE message to the stream if it understands it.
;;; :TYPE one of :TYI, :READLINE.  How typing is gathered and echoed.
;;; :CHOICES a list of choices.
;;;   A choice is either the symbol :ANY or a list.
;;;   If a list, its car is either a possible return value,
;;;     or a list of a possible return value and how to echo it.
;;;   The remaining things in the list are input items that select that return value.
;;;   For a :READLINE type call, they should be strings.
;;;   For a :TYI type call, they should be characters.
;;;   Example choice: ((:foo "Foo") #\F #\space)
;;; :FRESH-LINE boolean.  Send a FRESH-LINE to the stream initially.
;;; :CONDITION symbol.  Signalled before asking.
;;; :LIST-CHOICES boolean.  After prompting in parentheses.
;;; :BEEP boolean.  Before printing message.
;;; :CLEAR-INPUT boolean.  Before printing message.
;;; :HELP-FUNCTION function.  Called with STREAM, CHOICES and TYPE-FUNCTION as arguments.
;;; :STREAM stream or expression.  Specifies the stream to use.
;;;     If it is a symbol (which is not an io-stream) or a list it is evaluated.
;;;     Default is to use *QUERY-IO*.
;;; :DEFAULT-VALUE <value>. to return if defaulted or timed out.
;;; :TIMEOUT <internval-in-60ths>.

;;; Modified printing of timeout values to use TIME:PRINT-INTERVAL-OR-NEVER,
;;; for more intelligible intervals. -KmC 6/88

(DEFVAR Y-OR-N-P-CHOICES '(((T "Yes.") #/Y #/T #/SP #/HAND-UP)
                           ((NIL "No.") #/N #/RUBOUT #/HAND-DOWN)))
(DEFVAR YES-OR-NO-P-CHOICES '((T "Yes") (NIL "No")))

(DEFVAR FQUERY-FORMAT-STRING)
(DEFVAR FQUERY-FORMAT-ARGS)
(DEFVAR FQUERY-LIST-CHOICES)
(DEFVAR FQUERY-CHOICES)
(DEFVAR FQUERY-HELP-FUNCTION)
(DEFVAR FQUERY-STREAM)
(defvar fquery-default-value)
(defvar fquery-timeout)

(DEFUN FQUERY (OPTIONS FQUERY-FORMAT-STRING &REST FQUERY-FORMAT-ARGS
                                            &AUX MAKE-COMPLETE
                                            TYPE TYPE-FUNCTION
                                            FQUERY-CHOICES
                                            STREAM FQUERY-STREAM
                                            FRESH-LINE
                                            CONDITION
                                            FQUERY-LIST-CHOICES
                                            FQUERY-HELP-FUNCTION
                                            BEEP-P CLEAR-INPUT
                                            HANDLED-P VAL
                                            FQUERY-DEFAULT-VALUE FQUERY-TIMEOUT)
  "Ask a multiple-choice question on *QUERY-IO*.
FQUERY-FORMAT-STRING and FQUERY-FORMAT-ARGS are used to print the question.
Ending the string with /"? /" is often appropriate.
OPTIONS is a PLIST.  Defined indicators are:
:MAKE-COMPLETE boolean.  Send a :MAKE-COMPLETE message to the stream if it understands it.
:TYPE one of :TYI, :READLINE, :MINI-BUFFER-OR-READLINE.
  It says how the answer is gathered and echoed.
:CHOICES a list of choices.
  A choice is either the symbol :ANY or a list.
  If a list, its car is either a possible return value,
    or a list of a possible return value and how to echo it.
  The remaining things in the list are input items that select that return value.
  For a :READLINE type call, they should be strings.
  For a :TYI type call, they should be characters.
  Example choice (for :READLINE): ((:foo /"Foo/") #\F #\space)
:FRESH-LINE boolean.  Send a :FRESH-LINE to the stream initially.
:CONDITION symbol.  Signalled before asking.
:LIST-CHOICES boolean.  If T, a list of choices is printed after the question.
:BEEP boolean.  If T, we beep before printing the message.
:CLEAR-INPUT boolean.  If T, we discard type-ahead before printing the message.
:HELP-FUNCTION specifies a function to be called
  if the user types Help.
  It is called with STREAM, CHOICES and TYPE-FUNCTION as arguments.
:STREAM stream or expression.  Specifies the stream to use.
  If it is a symbol (which is not an io-stream) or a list it is evaluated.
  Default is to use *QUERY-IO*.
:DEFAULT-VALUE value.  Return this if defaulted or timed out.
:TIMEOUT <interval-in-60ths>."
  (SETF (VALUES MAKE-COMPLETE TYPE FQUERY-CHOICES STREAM BEEP-P CLEAR-INPUT
                FRESH-LINE CONDITION FQUERY-LIST-CHOICES FQUERY-HELP-FUNCTION
                FQUERY-DEFAULT-VALUE FQUERY-TIMEOUT)
        (APPLY #'FQUERY-DECODE-OPTIONS OPTIONS))
  (SETQ FQUERY-STREAM (IF STREAM
                          (IF (OR (AND (SYMBOLP STREAM) (NOT (GET STREAM 'SI:IO-STREAM-P)))
                                  (CONSP STREAM))
                              (EVAL STREAM)
                            STREAM)
                        *QUERY-IO*))
  (SETQ TYPE-FUNCTION (OR (GET TYPE 'FQUERY-FUNCTION)
                          (FERROR NIL "~S is not a valid :TYPE for FQUERY" TYPE)))
  (AND CONDITION
       (OR (NEQ CONDITION 'FQUERY)
           (EH:CONDITION-NAME-HANDLED-P CONDITION))
       (MULTIPLE-VALUE (HANDLED-P VAL)
         (SIGNAL-CONDITION
           (APPLY #'MAKE-CONDITION CONDITION OPTIONS FQUERY-FORMAT-STRING FQUERY-FORMAT-ARGS)
           '(:NEW-VALUE))))
  (IF HANDLED-P VAL
;     (UNWIND-PROTECT
        (PROGN
;         (COND ((AND SELECT
;                     (MEMQ :SELECT (SEND FQUERY-STREAM :WHICH-OPERATIONS)))
;                (SEND FQUERY-STREAM :OUTPUT-HOLD-EXCEPTION)
;                (SETQ OLD-SELECTED-WINDOW TV:SELECTED-WINDOW)
;                (SEND FQUERY-STREAM :SELECT)))
          (BLOCK TOP
            (DO-FOREVER
              (AND BEEP-P (SEND FQUERY-STREAM :BEEP 'FQUERY))
              (AND CLEAR-INPUT (SEND FQUERY-STREAM :CLEAR-INPUT))
              (AND FRESH-LINE (SEND FQUERY-STREAM :FRESH-LINE))
              (MULTIPLE-VALUE-BIND (TYPEIN TIMEOUT-P)
                  (FUNCALL TYPE-FUNCTION :READ FQUERY-STREAM)
                (cond (timeout-p
                       (format fquery-stream "~A -- timed out." (find-fquery-default))
                       (AND MAKE-COMPLETE
                            (SEND FQUERY-STREAM :SEND-IF-HANDLES :MAKE-COMPLETE))
                       (RETURN-FROM TOP TYPEIN))
                      (t
                       (DOLIST (CHOICE FQUERY-CHOICES)
                         (COND ((EQ CHOICE :ANY)
                                (FUNCALL TYPE-FUNCTION :ECHO TYPEIN FQUERY-STREAM)
                                (AND MAKE-COMPLETE
                                     (SEND FQUERY-STREAM :SEND-IF-HANDLES :MAKE-COMPLETE))
                                (RETURN-FROM TOP TYPEIN))
                               ((FUNCALL TYPE-FUNCTION :MEMBER TYPEIN (CDR CHOICE))
                                (SETQ CHOICE (CAR CHOICE))
                                (WHEN (CONSP CHOICE)
                                  (FUNCALL TYPE-FUNCTION :ECHO (CADR CHOICE) FQUERY-STREAM)
                                  (SETQ CHOICE (CAR CHOICE)))
                                (AND MAKE-COMPLETE
                                     (SEND FQUERY-STREAM :SEND-IF-HANDLES :MAKE-COMPLETE))
                                (RETURN-FROM TOP CHOICE)))))))
              (SETQ BEEP-P T
                    CLEAR-INPUT T
                    FRESH-LINE T                ;User spazzed, will need fresh line
                    FQUERY-LIST-CHOICES T))))   ;and should list options
;       (AND OLD-SELECTED-WINDOW (SEND OLD-SELECTED-WINDOW :SELECT NIL)))
        ))


(DEFVAR *ALLOW-FQUERY-TIMEOUTS* T
  "T if you want the :TIMEOUT option to be noticed.")

(DEFUN FQUERY-DECODE-OPTIONS (&KEY (MAKE-COMPLETE T)
                                   (TYPE :TYI)
                                   (CHOICES Y-OR-N-P-CHOICES)
                                   STREAM
                                   BEEP
                                   CLEAR-INPUT
                                   (FRESH-LINE T)
                                   (CONDITION 'FQUERY)
                                   SIGNAL-CONDITION
                                   (LIST-CHOICES T)
                                   SELECT     ;no longer used
                                   (HELP-FUNCTION 'DEFAULT-FQUERY-HELP)
                                   DEFAULT-VALUE
                                   TIMEOUT)
  SIGNAL-CONDITION SELECT
  (LET((CHOICES (IF (EQ CHOICES :ANY) '(:ANY) CHOICES)))
    (VALUES MAKE-COMPLETE TYPE CHOICES STREAM BEEP CLEAR-INPUT
            FRESH-LINE CONDITION LIST-CHOICES HELP-FUNCTION
            DEFAULT-VALUE
            (IF *ALLOW-FQUERY-TIMEOUTS* TIMEOUT))))

(defun string-to-handle-any-as-choice (remaining-choices &optional first-p)
  (cond (remaining-choices "anything")
        (first-p "anything")
        (t "anything else")))

(DEFUN FQUERY-PROMPT (STREAM &REST IGNORE)
  (AND FQUERY-FORMAT-STRING
       (APPLY #'FORMAT STREAM FQUERY-FORMAT-STRING FQUERY-FORMAT-ARGS))
  (AND FQUERY-LIST-CHOICES
       (DO ((CHOICES FQUERY-CHOICES (CDR CHOICES))
            (FIRST-P T NIL)
            (MANY (> (LENGTH FQUERY-CHOICES) 2))
            (CHOICE))
           ((NULL CHOICES)
            (OR FIRST-P
                (SEND STREAM :STRING-OUT ") ")))
         (SEND STREAM :STRING-OUT (COND (FIRST-P "(")
                                        ((NOT (NULL (CDR CHOICES))) ", ")
                                        (MANY ", or ")
                                        (T " or ")))
         (IF (EQ (CAR CHOICES) :ANY)
             (SEND STREAM :STRING-OUT
                   (string-to-handle-any-as-choice (cdr choices) first-p))
           (progn
             (SETQ CHOICE (CADAR CHOICES))
; character lossage
             (COND ((TYPEP CHOICE '(OR NUMBER CHARACTER))
                    (FORMAT STREAM "~:@C" CHOICE))
                   ((EQUAL CHOICE "")
                    (PRINC "nothing" STREAM))
                   (T
                    (SEND STREAM :STRING-OUT CHOICE)))))))
  (AND FQUERY-TIMEOUT
                                                ;attempt to print out what will return default-value.
       (DO ((CHOICES FQUERY-CHOICES (CDR CHOICES))
            (choice))
           ((NULL CHOICES)
            (FORMAT STREAM "(Automatic default returns ~s after ~a) "
                    fquery-default-value
                    (time:print-interval-or-never
                      (// fquery-timeout 60.)
                      nil)))
         (setq choice (car choices))
         (cond ((eq choice :any))
               ((equal fquery-default-value
                       (if (consp (car choice))
                           (caar choice)
                         (car choice)))
                (return
                  (format stream "(Automatic default after ~a, ~a) "
                          (time:print-interval-or-never
                            (// fquery-timeout 60.)
                            nil)
                          (cadr choice)))))))
  )

(DEFUN DEFAULT-FQUERY-HELP (STREAM CHOICES TYPE)
  (declare(ignore TYPE))                        ;Not used
  (DO ((CHOICES CHOICES (CDR CHOICES))
       (FIRST-P T NIL)
       (CHOICE))
      ((NULL CHOICES)
       (OR FIRST-P
           (SEND STREAM :STRING-OUT ") ")))
    (SEND STREAM :STRING-OUT (COND (FIRST-P "(Type ")
                                   ((NOT (NULL (CDR CHOICES))) ", ")
                                   (T " or ")))
    (SETQ CHOICE (CAR CHOICES))
    (COND ((EQ CHOICE :ANY)
           (PRINC  (string-to-handle-any-as-choice (cdr choices) first-p) STREAM))
          (T
           ;;Print the first input which selects this choice.
           ;;Don't confuse the user by mentioning possible alternative inputs.
; character lossage
           (COND ((TYPEP (CADR CHOICE) '(OR NUMBER CHARACTER))
                  (FORMAT STREAM "~:@C" (CADR CHOICE)))
                 ((EQUAL (CADR CHOICE) "")
                  (PRINC "nothing" STREAM))
                 (T
                  (SEND STREAM :STRING-OUT (CADR CHOICE))))
           ;; If that would echo as something else, say so
           (IF (CONSP (CAR CHOICE))
               (FORMAT STREAM " (~A)" (CADAR CHOICE)))))))

(defun find-fquery-default ()
  (dolist (element fquery-choices)
    (let ((choice (car element)))
      (when (eq fquery-default-value (if (consp choice) (car choice) choice))
        ;;We found the default that will be returned.  How does it echo?
        (let ((value (if (consp choice) (cadr choice) (cadr element))))
          (return (if (and (stringp value)
                           (char= (char value (1- (string-length value))) #/.))
                      ;;Kludge -- strip off period at end.
                      (substring value 0 (1- (string-length value)))
                    value)))))))

(DEFPROP :TYI TYI-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFSELECT TYI-FQUERY-FUNCTION
  (:READ (STREAM)
    (labels ((read-it ()
              (DO ((CH)) (NIL)
                (FQUERY-PROMPT STREAM)
                (SETQ CH (READ-CHAR STREAM))
                (cond ((AND (CHAR= CH #/HELP) FQUERY-HELP-FUNCTION)
                       (SEND FQUERY-HELP-FUNCTION STREAM FQUERY-CHOICES 'TYI-FQUERY-FUNCTION)
                       (SEND STREAM :FRESH-LINE))
                      ((char= ch #/clear-screen)
                       (send-if-handles stream :clear-screen))
                      (t
                       (RETURN CH))))))

      (if (null fquery-timeout)
          (read-it)
        (with-timeout (fquery-timeout
                        (values fquery-default-value t))
          (read-it)))))
  (:ECHO (ECHO STREAM)
    (SEND STREAM :STRING-OUT (STRING ECHO)))
  (:MEMBER (CHAR LIST)
; character lossage
    (MEM #'(LAMBDA (X Y) (CHAR-EQUAL X (COERCE Y 'CHARACTER))) CHAR LIST)))

(DEFPROP :READLINE READLINE-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFSELECT READLINE-FQUERY-FUNCTION
  (:READ (STREAM &AUX STRING)
    (labels ((read-it ()
              (if (operation-handled-p stream :rubout-handler)
                  (SETQ STRING (SEND STREAM :RUBOUT-HANDLER `((:EDITING-COMMAND ,(char-int #/Help)
                                                                                #/HELP)        ;Just in case
                                                              (:PROMPT FQUERY-PROMPT)
                                                              (:DONT-SAVE T))
                                     'FQUERY-READLINE-WITH-HELP STREAM))
                (setq string (send stream :line-in)))
              (STRING-TRIM '(#/SP) STRING)))
      (if (null fquery-timeout)
          (read-it)
        (with-timeout (fquery-timeout
                         (values fquery-default-value t))
          (read-it)))))
  (:ECHO (ECHO STREAM)
    ECHO STREAM)
  (:MEMBER (STRING LIST)
    (MEM #'STRING-EQUAL STRING LIST)))

(DEFUN FQUERY-READLINE-WITH-HELP (STREAM)
  (DO ((STRING (MAKE-STRING 20. :FILL-POINTER 0))
       (CH))
      (NIL)
    (SETQ CH (READ-CHAR STREAM))
    (COND ((OR (NULL CH) (CHAR= CH #/CR))
           (RETURN STRING))
          ((AND (CHAR= CH #/HELP) FQUERY-HELP-FUNCTION)
           (FRESH-LINE STREAM)
           (SEND FQUERY-HELP-FUNCTION STREAM FQUERY-CHOICES 'READLINE-FQUERY-FUNCTION)
           (SEND STREAM :SEND-IF-HANDLES :REFRESH-RUBOUT-HANDLER))
          ((NOT (ZEROP (CHAR-BITS CH))))
          (T (VECTOR-PUSH-EXTEND CH STRING)))))

(PROCLAIM '(SPECIAL ZWEI:*MINI-BUFFER-ARG-DOCUMENTER*))  ;DEFVAR is in ZWEI.

(DEFPROP :MINI-BUFFER-OR-READLINE MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFUN MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION (&REST ARGS &AUX STRING)
  (COND ((AND (EQ (CAR ARGS) :READ)
              (EQ (CADR ARGS) 'ZWEI:*TYPEIN-WINDOW*-SYN-STREAM))
         (LET ((ZWEI:*MINI-BUFFER-ARG-DOCUMENTER* 'MINI-BUFFER-OR-READLINE-HELP-FUNCTION))
           (FUNCALL (CADR ARGS) :SEND-IF-HANDLES :MAKE-COMPLETE)
           (SETQ STRING (APPLY #'ZWEI:TYPEIN-LINE-READLINE
                               FQUERY-FORMAT-STRING FQUERY-FORMAT-ARGS)))
         (STRING-TRIM '(#/SP) STRING))
        (T
         (APPLY #'READLINE-FQUERY-FUNCTION ARGS))))

(DEFUN MINI-BUFFER-OR-READLINE-HELP-FUNCTION ()
  (FORMAT *TERMINAL-IO* "~&~%You are now typing an answer to a query.~&")
  (FUNCALL FQUERY-HELP-FUNCTION *TERMINAL-IO* FQUERY-CHOICES
           'MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION))

(DEFCONST Y-OR-N-P-OPTIONS `(:FRESH-LINE NIL))

(DEFUN Y-OR-N-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Ask the user a question he can answer with Y or N.
Passes the arguments to FORMAT.
With no args, asks the question without printing anything but the /"(Y or N)/".
Returns T if the answer was yes."
  (FQUERY Y-OR-N-P-OPTIONS
          (AND FORMAT-STRING
               (> (length format-string) 0)
               (IF (CHAR= (CHAR FORMAT-STRING (max 0 (1- (LENGTH FORMAT-STRING)))) #/SP)
                   "~&~?" "~&~? "))
          FORMAT-STRING
          FORMAT-ARGS))

(DEFUN Y-OR-N-P-WITH-TIMEOUT (&OPTIONAL (TIMEOUT (* 15. 60.)) DEFAULT-VALUE FORMAT-STRING &REST FORMAT-ARGS)
  "Like Y-OR-N-P, but times out and returns DEFAULT-VALUE if
the user has not answered in (// TIMEOUT 60.) seconds."
  (FQUERY `(:timeout ,timeout
            :default-value ,default-value
            ,@Y-OR-N-P-OPTIONS)
          (AND FORMAT-STRING
               (> (length format-string) 0)
               (IF (CHAR= (CHAR FORMAT-STRING (max 0 (1- (LENGTH FORMAT-STRING)))) #/SP)
                   "~&~?" "~&~? "))
          FORMAT-STRING
          FORMAT-ARGS))

(DEFCONST YES-OR-NO-P-OPTIONS `(:FRESH-LINE NIL
                                :BEEP T
                                :TYPE :READLINE
                                :CHOICES ,YES-OR-NO-P-CHOICES))

(DEFUN YES-OR-NO-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Ask the user a question he can answer with Yes or No.
Beeps and discards type-ahead.
Passes the arguments to FORMAT.
With no args, asks the question without printing anything but the /"(Yes or No)/".
Returns T if the answer was yes."
  (FQUERY YES-OR-NO-P-OPTIONS
          (AND FORMAT-STRING
               (> (length format-string) 0)
               (IF (CHAR= (CHAR FORMAT-STRING (max 0 (1- (LENGTH FORMAT-STRING)))) #/SP)
                   "~&~?" "~&~? "))
          FORMAT-STRING
          FORMAT-ARGS))

(DEFUN YES-OR-NO-P-WITH-TIMEOUT (&OPTIONAL (TIMEOUT (* 15. 60.)) DEFAULT-VALUE FORMAT-STRING &REST FORMAT-ARGS)
  "Like YES-OR-NO-P, but times out and returns DEFAULT-VALUE if
the user has not answered in (// TIMEOUT 60.) seconds."
  (FQUERY `(:timeout ,timeout
            :default-value ,default-value
            ,@YES-OR-NO-P-OPTIONS)
          (AND FORMAT-STRING
               (> (length format-string) 0)
               (IF (CHAR= (CHAR FORMAT-STRING (max 0 (1- (LENGTH FORMAT-STRING)))) #/SP)
                   "~&~?" "~&~? "))
          FORMAT-STRING
          FORMAT-ARGS))

(DEFCONST YES-OR-NO-QUIETLY-P-OPTIONS `(:TYPE :READLINE
                                        :CHOICES ,YES-OR-NO-P-CHOICES))

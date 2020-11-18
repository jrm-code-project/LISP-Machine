;;;-*- Mode:LISP; Package:FORMAT; Readtable:ZL; Base:8 -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFPARAMETER Y-OR-N-P-CHOICES '(((T "Yes.") #/Y #/T #/SP #/HAND-UP)
                                 ((NIL "No.") #/N #/RUBOUT #/HAND-DOWN)))
(DEFPARAMETER YES-OR-NO-P-CHOICES '((T "Yes") (NIL "No")))

(DEFVAR FQUERY-FORMAT-STRING)
(DEFVAR FQUERY-FORMAT-ARGS)
(DEFVAR FQUERY-LIST-CHOICES)
(DEFVAR FQUERY-CHOICES)
(DEFVAR FQUERY-HELP-FUNCTION)
(DEFVAR FQUERY-STREAM)

(DEFUN FQUERY (OPTIONS FQUERY-FORMAT-STRING &REST FQUERY-FORMAT-ARGS)
  "Ask a multiple-choice question on *QUERY-IO*.
FQUERY-FORMAT-STRING and FQUERY-FORMAT-ARGS are used to print the question.
Ending the string with /"? /" is often appropriate.
OPTIONS is an &key-style list of keywords and values.  Defined keys are:
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
:STREAM specifies the stream to use.  Default is to use *QUERY-IO*."
  (MULTIPLE-VALUE-BIND (MAKE-COMPLETE TYPE FQUERY-CHOICES STREAM BEEP-P CLEAR-INPUT
                        FRESH-LINE CONDITION FQUERY-LIST-CHOICES FQUERY-HELP-FUNCTION)
      (APPLY (LAMBDA (&KEY (MAKE-COMPLETE T)
                           (TYPE :TYI)
                           (CHOICES Y-OR-N-P-CHOICES)
                           STREAM
                           BEEP
                           CLEAR-INPUT
                           (FRESH-LINE T)
                           (CONDITION 'FQUERY)
                           (LIST-CHOICES T)
                           SELECT SIGNAL-CONDITION      ;not used
                           (HELP-FUNCTION 'DEFAULT-FQUERY-HELP))
               (DECLARE (IGNORE SIGNAL-CONDITION SELECT))
               (VALUES MAKE-COMPLETE TYPE CHOICES STREAM BEEP CLEAR-INPUT
                       FRESH-LINE CONDITION LIST-CHOICES HELP-FUNCTION))
             OPTIONS)
    (LET ((FQUERY-STREAM (OR STREAM *QUERY-IO*))
          (TYPE-FUNCTION (OR (GET TYPE 'FQUERY-FUNCTION)
                             (FERROR "~S is not a valid ~S for ~S" TYPE :TYPE 'FQUERY))))
      (AND CONDITION
           (OR (NEQ CONDITION 'FQUERY)
               (EH:CONDITION-NAME-HANDLED-P CONDITION))
           (MULTIPLE-VALUE-BIND (HANDLEDP VAL)
               (SIGNAL-CONDITION
                 (APPLY #'MAKE-CONDITION CONDITION
                        OPTIONS FQUERY-FORMAT-STRING FQUERY-FORMAT-ARGS)
                 '(:NEW-VALUE))
             (IF HANDLEDP (RETURN-FROM FQUERY VAL))))
      (FLET ((THUNK ()
               (LOOP
                 (AND BEEP-P (SEND FQUERY-STREAM :BEEP 'FQUERY))
                 (AND CLEAR-INPUT (SEND FQUERY-STREAM :CLEAR-INPUT))
                 (AND FRESH-LINE (SEND FQUERY-STREAM :FRESH-LINE))
                 (LET ((TYPEIN (FUNCALL TYPE-FUNCTION :READ FQUERY-STREAM)))
                   (DOLIST (CHOICE FQUERY-CHOICES)
                     (COND ((EQ CHOICE ':ANY)
                            (FUNCALL TYPE-FUNCTION :ECHO TYPEIN FQUERY-STREAM)
                            (AND MAKE-COMPLETE (SEND-IF-HANDLES FQUERY-STREAM :MAKE-COMPLETE))
                            (RETURN-FROM FQUERY TYPEIN))
                           ((FUNCALL TYPE-FUNCTION :MEMBER TYPEIN (CDR CHOICE))
                            (SETQ CHOICE (CAR CHOICE))
                            (WHEN (CONSP CHOICE)
                              (FUNCALL TYPE-FUNCTION :ECHO (CADR CHOICE) FQUERY-STREAM)
                              (SETQ CHOICE (CAR CHOICE)))
                            (AND MAKE-COMPLETE (SEND-IF-HANDLES FQUERY-STREAM :MAKE-COMPLETE))
                            (RETURN-FROM FQUERY CHOICE)))))
                 (SETQ BEEP-P T
                       CLEAR-INPUT T
                       FRESH-LINE T             ;User spazzed, will need fresh line
                       FQUERY-LIST-CHOICES T))))
        (IF (AND (TYPEP (SI::FOLLOW-SYN-STREAM FQUERY-STREAM) 'TV:SHEET)
                 (OPERATION-HANDLED-P TV:SELECTED-WINDOW :SET-SELECTION-SUBSTITUTE)
                 (OPERATION-HANDLED-P FQUERY-STREAM :SELECT))
            (TV:WITH-SELECTION-SUBSTITUTE (FQUERY-STREAM TV:SELECTED-WINDOW)
              (SEND FQUERY-STREAM :OUTPUT-HOLD-EXCEPTION)
              (THUNK))
          (THUNK))))))

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
             (SEND STREAM :STRING-OUT "anything else")
           (SETQ CHOICE (CADAR CHOICES))
; character lossage
           (COND ((TYPEP CHOICE '(OR NUMBER CHARACTER))
                  (FORMAT STREAM "~:@C" CHOICE))
                 ((EQUAL CHOICE "")
                  (PRINC "nothing" STREAM))
                 (T
                  (SEND STREAM :STRING-OUT CHOICE)))))))

(DEFUN DEFAULT-FQUERY-HELP (STREAM CHOICES TYPE)
  (DECLARE (IGNORE TYPE))                       ;Not relevant
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
           (PRINC "anything else" STREAM))
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

(DEFPROP :TYI TYI-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFSELECT TYI-FQUERY-FUNCTION
  (:READ (STREAM)
    (DO ((CH)) (NIL)
      (FQUERY-PROMPT STREAM)
      (SETQ CH (READ-CHAR STREAM))
      (UNLESS (AND (CHAR= CH #/HELP) FQUERY-HELP-FUNCTION)
        (RETURN CH))
      (SEND FQUERY-HELP-FUNCTION STREAM FQUERY-CHOICES 'TYI-FQUERY-FUNCTION)
      (SEND STREAM :FRESH-LINE)))
  (:ECHO (ECHO STREAM)
    (SEND STREAM :STRING-OUT (STRING ECHO)))
  (:MEMBER (CHAR LIST)
; character lossage
    (MEM (LAMBDA (X Y) (CHAR-EQUAL X (COERCE Y 'CHARACTER))) CHAR LIST)))

(DEFPROP :READLINE READLINE-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFSELECT READLINE-FQUERY-FUNCTION
  (:READ (STREAM &AUX STRING)
    (SETQ STRING (SEND STREAM :RUBOUT-HANDLER '((:EDITING-COMMAND #/HELP)      ;Just in case
                                                (:PROMPT FQUERY-PROMPT)
                                                (:DONT-SAVE T))
                              #'FQUERY-READLINE-WITH-HELP STREAM))
    (STRING-TRIM '(#/SP) STRING))
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
           (SEND-IF-HANDLES STREAM :REFRESH-RUBOUT-HANDLER))
          ((NOT (ZEROP (CHAR-BITS CH))))
          (T
           (VECTOR-PUSH-EXTEND CH STRING)))))

(PROCLAIM '(SPECIAL ZWEI::*MINI-BUFFER-ARG-DOCUMENTER*))  ;DEFVAR is in ZWEI.

(DEFPROP :MINI-BUFFER-OR-READLINE MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION FQUERY-FUNCTION)
(DEFUN MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION (&REST ARGS &AUX STRING)
  (COND ((AND (EQ (CAR ARGS) :READ)
              (EQ (CADR ARGS) 'ZWEI::*TYPEIN-WINDOW*-SYN-STREAM))
         (LET ((ZWEI::*MINI-BUFFER-ARG-DOCUMENTER* 'MINI-BUFFER-OR-READLINE-HELP-FUNCTION))
           (SEND-IF-HANDLES (CADR ARGS) :MAKE-COMPLETE)
           (SETQ STRING (APPLY #'ZWEI::TYPEIN-LINE-READLINE
                               FQUERY-FORMAT-STRING FQUERY-FORMAT-ARGS)))
         (STRING-TRIM '(#/SP) STRING))
        (T
         (APPLY #'READLINE-FQUERY-FUNCTION ARGS))))

(DEFUN MINI-BUFFER-OR-READLINE-HELP-FUNCTION ()
  (FORMAT *TERMINAL-IO* "~&~%You are now typing an answer to a query.~&")
  (FUNCALL FQUERY-HELP-FUNCTION *TERMINAL-IO* FQUERY-CHOICES
           'MINI-BUFFER-OR-READLINE-FQUERY-FUNCTION))


(DEFPARAMETER Y-OR-N-P-OPTIONS `(:FRESH-LINE NIL))

(DEFUN Y-OR-N-P (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Ask the user a question he can answer with Y or N.
Passes the arguments to FORMAT.
With no args, asks the question without printing anything but the /"(Y or N)/".
Returns T if the answer was yes."
  (FQUERY Y-OR-N-P-OPTIONS
          (AND FORMAT-STRING
               (IF (CHAR= (CHAR FORMAT-STRING (1- (LENGTH FORMAT-STRING))) #/SP)
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
               (IF (CHAR= (CHAR FORMAT-STRING (1- (LENGTH FORMAT-STRING))) #/SP)
                   "~&~?" "~&~? "))
          FORMAT-STRING
          FORMAT-ARGS))

(DEFPARAMETER YES-OR-NO-QUIETLY-P-OPTIONS `(:TYPE :READLINE
                                                  :CHOICES ,YES-OR-NO-P-CHOICES))

;;; -*- Mode:LISP; Package:SI; Cold-Load:T; Base:8; Readtable:ZL -*-

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; A BONUS DRIVEN READER!

(DEFVAR *READTABLE* :UNBOUND
  "Syntax table which controls operation of READ (and also PRINT, in limited ways).")
(DEFVAR READTABLE :UNBOUND
  "Syntax table which controls operation of READ (and also PRINT, in limited ways).")
(FORWARD-VALUE-CELL 'READTABLE '*READTABLE*)

(DEFVAR *ALL-READTABLES* NIL
  "A list of all the named readtables in the world")

(DEFVAR-RESETTABLE *READ-BASE* 10. 10.
  "Default radix for reading rational numbers.")
(DEFVAR IBASE :UNBOUND
  "Default radix for reading rational numbers.")
(FORWARD-VALUE-CELL 'IBASE '*READ-BASE*)

(DEFVAR INITIAL-READTABLE :UNBOUND
  "A readtable defining the standard Zetalisp syntax and symbols.
This is a copy of the readtable that was current when the system was built.
It does not contain any changes you have made to SI:STANDARD-READTABLE.")

(DEFVAR COMMON-LISP-READTABLE :UNBOUND
  "A readtable for Common Lisp syntax.")

(DEFVAR INITIAL-COMMON-LISP-READTABLE :UNBOUND
  "A readtable defining the standard Common Lisp syntax and symbols.
This is a copy of the readtable that defined when the system was built.
It does not contain any changes you have made to SI:COMMON-LISP-READTABLE.")

(DEFVAR STANDARD-READTABLE :UNBOUND
  "A readtable defining the standard traditional Zetalisp syntax.")

(DEFVAR READ-PRESERVE-DELIMITERS NIL
  "If NIL, syntatically useless characters that terminate symbols and numbers are discarded.
If T, they are untyi'ed so that they can be seen by the caller of READ.
This variable should always be globally NIL so that
interactive READ-EVAL-PRINT loops work right, but certain reader macros
or other specialized applications may want to bind it to T.")

(DEFVAR *READ-SUPPRESS* NIL
  "T means do not actually intern symbols that are read.
Used by read-time conditionals to skip what is not wanted.")
;(DEFVAR READ-DONT-INTERN :UNBOUND
;  "T means do not actually intern symbols that are read.
;Used by read-time conditionals to skip what is not wanted.")
;(FORWARD-VALUE-CELL 'READ-DONT-INTERN '*READ-SUPPRESS*)

(DEFVAR READ-DISCARD-FONT-CHANGES NIL
  "T means input stream contains 's that signify font changes.
READ ignores the font information.")

(DEFVAR READ-CHECK-INDENTATION NIL
  "If T, do not allow an internal list to start in column 0.
It will get a SYS:MISSING-CLOSEPAREN error.  If you proceed
with :NO-ACTION, closeparens will be invented and then the
openparen will be re-read.")

(DEFVAR INSIDE-COLUMN-0-LIST NIL
  "T while we are reading the insides of a list that starts in column 0.")

(ADD-INITIALIZATION "READ Check Indentation"
                    '(SETQ READ-CHECK-INDENTATION NIL INSIDE-COLUMN-0-LIST NIL
                           *READ-SUPPRESS* NIL READ-DISCARD-FONT-CHANGES NIL
                           READ-PRESERVE-DELIMITERS NIL
                           READ-INSIDE-MULTIPLE-ESCAPE NIL
                           READ-INTERN-FUNCTION 'INTERN)
                    '(:WARM))

(DEFVAR MISSING-CLOSEPAREN-REPORTED NIL
  "T after an invalid open paren in column 0 has been reported (as missing closeparens)
 until that open paren is successfully digested.
Used to cause the problem to be reported only once.")

(DEFVAR READ-INSIDE-MULTIPLE-ESCAPE NIL
  "T if inside vertical bars while reading a token.")

(DEFVAR READ-INTERN-FUNCTION 'INTERN
  "Function READ uses to get a symbol given a pname.")

(defvar *read-single-colon-allow-internal-symbol* t
  "T means that a single colon prefix will find an internal symbol in a package.
:WARN means to print a message on *ERROR-OUTPUT* but still find the symbol.
NIL means to get an error.")

(defvar *read-barf-if-auto-export?* T
  "T means that the reader is not allowed to intern a symbol in
a package that is auto exporting.  NIL makes the reader pay attention
only to the READ-LOCK feature of the package.")

;;; One peculiar feature of this reader is the ability to construct a table of correspondences
;;; between the s-expression read in and positions in the stream it was read from.
;;; This assumes that the stream responds to a :READ-BP operation with a position.
;;; The feature is activated by reading with XR-CORRESPONDENCE-FLAG set non-nil.
;;; The table is accumulated in XR-CORRESPONDENCE.
;;; The table is a list containing three elements for each list in the s-expression
;;; (not counting cdrs of other lists).
;;; The first of the three is the list that the three elements pertain to.
;;; The second is the position at which that list started.
;;; The third is a list of positions of the elements of that list which are atoms.
;;; NIL appears in that list for elements which are not atoms, since they have
;;; their own sets of three elements.
(DEFVAR-RESETTABLE XR-CORRESPONDENCE-FLAG NIL NIL);T if inside READ-ESTABLISH-CORRESPONDENCE.
(DEFVAR-RESETTABLE XR-CORRESPONDENCE NIL NIL)   ;Each list we read puts its correspondence
                                                ;entry on this list.

(DEFVAR XR-SHARP-ARGUMENT)

(SETQ RDTBL-ARRAY-SIZE #.RDTBL-ARRAY-SIZE)      ;Have a reasonable value in the cold load

(DEFVAR-RESETTABLE READ-AREA NIL NIL
  "Area in which most data read are consed (NIL means use DEFAULT-CONS-AREA).")

(defun read-error (format-string &rest format-args)
  (declare (dbg:error-reporter))
  (apply #'cerror :no-action nil 'read-error-1 format-string format-args))

(defun read-fatal-error (format-string &rest format-args)
  (declare (dbg:error-reporter))
  (apply #'ferror 'read-error-1 format-string format-args))

(defun current-read-base (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *read-base*)
    (cond ((and (fixnump val) ( 2 val 36.))
           (return val))
          (t
           (signal-proceed-case (() 'eh:bad-system-parameter
                                    :place '*read-base*
                                    :value val
                                    :type-specifier '(integer 2 36.)
                                    :reset-value (setq *read-base* 10.))
             (:no-action))))))

;;;; Low levels of READ

;;; These hold the last and next-to-last characters returned by XR-XRTYI.
;;; A circle-cross and its args are treated as a single character.
(DEFVAR XR-XRTYI-LAST-CHAR NIL)
(DEFVAR XR-XRTYI-PREV-CHAR)

(DEFUN XR-XRTYI (STREAM &OPTIONAL IGNORE-WHITESPACE NO-CHARS-SPECIAL NO-MULTIPLE-ESCAPES)
  "Read a character from STREAM, processing escapes (// and /) and multiple-escapes (/|).
IGNORE-WHITESPACE non-NIL means skip over whitespace characters.
NO-CHARS-SPECIAL means do not process escapes specially.
NO-MULTIPLE-ESCAPES means do not process multiple-escape characters specially.

The first value is the translated character.
The second is the index for looking in READ's FSM.
The third is the original, nontranslated character.
The fourth is T if the character was preceded by one or more
 multi-character escape characters that were passed over.

Has a kludge for *READ-BASE* > 10. where letters that should be digits
return the readtable code for EXTENDED-DIGIT rather than their own codes."
  (DECLARE (VALUES TRANSLATED-CHAR FSM-INDEX ACTUAL-CHAR FOUND-MULTI-ESCAPES))
  (PROG (CH BITS CODE CH-CHAR FOUND-MULTI-ESCAPES)
        (SETQ XR-XRTYI-PREV-CHAR XR-XRTYI-LAST-CHAR)
     L
        (DO-FOREVER
          (SETQ CH (SEND STREAM (IF (EQ RUBOUT-HANDLER STREAM) :ANY-TYI :TYI)))
          ;; fixnump as opposed to blip --- we never get character objects from :tyi
          (if (fixnump ch) (setf (char-font ch) 0))
          (COND ((NULL CH)
                 (RETURN-FROM XR-XRTYI (VALUES CH
                                               (RDTBL-EOF-CODE *READTABLE*)
                                               CH)))
                ((CONSP CH)
                 (AND (EQ (CAR CH) ':ACTIVATION)
                      ;; Ignore activations except in top-level context.
                      (NOT IGNORE-WHITESPACE)
                      (NOT NO-CHARS-SPECIAL)
                      (NOT NO-MULTIPLE-ESCAPES)
                      (LET ((CH1 (CAR (RDTBL-WHITESPACE *READTABLE*))))
                        (RETURN-FROM XR-XRTYI (VALUES CH1
                                                      (RDTBL-CODE *READTABLE* CH1)
                                                      CH)))))
                ((AND READ-DISCARD-FONT-CHANGES
                      (EQ CH #/))
                 (IF (EQ #/ (SEND STREAM :TYI))
                     (RETURN)))
                ((NOT (> CH RDTBL-ARRAY-SIZE))
                 (RETURN))))
        (SETQ CH-CHAR (CHAR-CODE CH))
        (SETQ BITS (RDTBL-BITS *READTABLE* CH-CHAR))
        (SETQ CODE (RDTBL-CODE *READTABLE* CH-CHAR))
        (COND ((AND (NOT NO-CHARS-SPECIAL)
                    (NOT NO-MULTIPLE-ESCAPES)
                    (= CODE
                       (RDTBL-MULTIPLE-ESCAPE-CODE *READTABLE*)))
               ;; Vertical bar.
               (SETQ FOUND-MULTI-ESCAPES T)
               (SETQ READ-INSIDE-MULTIPLE-ESCAPE
                     (IF READ-INSIDE-MULTIPLE-ESCAPE NIL
                       CH-CHAR))
               (GO L))
              ((AND (NOT NO-CHARS-SPECIAL)
                    (= CODE
                       (RDTBL-ESCAPE-CODE *READTABLE*)))
               ;; Slash
               (SETQ XR-XRTYI-PREV-CHAR CH)
               (DO-FOREVER
                 (SETQ CH (SEND STREAM :TYI))
                 (COND ((AND READ-DISCARD-FONT-CHANGES
                             (EQ CH #/))
                        (IF (EQ #/ (SEND STREAM :TYI))
                            (RETURN)))
                       (T (RETURN))))
               (SETQ XR-XRTYI-LAST-CHAR CH)
               (RETURN (VALUES (OR CH (PROGN
                                        (CERROR :NO-ACTION NIL 'SYS:READ-END-OF-FILE
                                                "EOF on ~S after a ~S." STREAM
                                                (STRING XR-XRTYI-PREV-CHAR))
                                        #/SPACE))
                               (RDTBL-SLASH-CODE *READTABLE*)
                               CH)))
              ((AND (NOT NO-CHARS-SPECIAL)
                    (= CODE
                       (RDTBL-CHARACTER-CODE-ESCAPE-CODE *READTABLE*)))
               ;; circlecross
               (SETQ XR-XRTYI-LAST-CHAR (XR-READ-CIRCLECROSS STREAM))
               (RETURN (VALUES XR-XRTYI-LAST-CHAR
                               (RDTBL-SLASH-CODE *READTABLE*)
                               XR-XRTYI-LAST-CHAR)))
              (READ-INSIDE-MULTIPLE-ESCAPE
               ;; Ordinary character but within vertical bars.
               (SETQ XR-XRTYI-LAST-CHAR CH)
               (RETURN (VALUES (OR CH (PROGN
                                        (CERROR :NO-ACTION NIL 'READ-END-OF-FILE
                                                "EOF on ~S inside a ~C-quoted token." STREAM
                                                READ-INSIDE-MULTIPLE-ESCAPE)
                                        #/SPACE))
                               (RDTBL-SLASH-CODE *READTABLE*)
                               CH)))
              (T
               ;; Ordinary character.
               (COND ((AND IGNORE-WHITESPACE (NOT FOUND-MULTI-ESCAPES)
                           (BIT-TEST 1 BITS))
                      ;; Here if whitespace char to be ignored.
                      (SETQ XR-XRTYI-PREV-CHAR CH)
                      (GO L)))
               ;; Here for ordinary, significant input char.
               (SETQ XR-XRTYI-LAST-CHAR CH)
               (RETURN (VALUES (RDTBL-TRANS *READTABLE* CH-CHAR)
                               ;; If not doing slashes, caller must not really want the
                               ;;  RDTBL-CODE, so return a value which, if passed to
                               ;;  XR-XRUNTYI, will prevent barfing.
                               (IF NO-CHARS-SPECIAL 0
                                 (IF (AND (NUMBERP *READ-BASE*)
                                          ( #/A (CHAR-UPCASE CH) (+ *READ-BASE* #/A -11.)))
                                     (CDR (GETF (RDTBL-PLIST *READTABLE*) 'EXTENDED-DIGIT))
                                   (RDTBL-CODE *READTABLE* CH-CHAR)))
                               CH
                               T))))))

(DEFUN XR-READ-CIRCLECROSS (STREAM &AUX CH1 CH2 CH3)
  (IF (NOT (AND (SETQ CH1 (XR-XRTYI STREAM))
                (SETQ CH2 (XR-XRTYI STREAM))
                (SETQ CH3 (XR-XRTYI STREAM))))
      (PROGN (CERROR :NO-ACTION NIL 'READ-END-OF-FILE "EOF during a circlecross.")
             #/SPACE)
    (IF (OR (< CH1 #/0) (> CH1 #/7)
            (< CH2 #/0) (> CH2 #/7)
            (< CH3 #/0) (> CH3 #/7))
        ;; The lack of an explicit  character here is to get around
        ;; a stupid bug in the cold-load generator.
        (PROGN
          (READ-ERROR
            "The three characters immediately following a circlecross must be octal -- ~C~C~C~C."
            #/ CH1 CH2 CH3)
          #/SPACE)
      (+ (* 100 (- CH1 #/0))
         (+ (* 10 (- CH2 #/0))
            (- CH3 #/0))))))

;;; XR-XRUNTYI takes a stream to untyi to, a character to untyi (third result of XR-XRTYI
;;; please) and the magic number that character was read in with.
;;; This is where READ-PRESERVE-DELIMITERS is implemented.
(DEFUN XR-XRUNTYI (STREAM CH NUM)
  "XR-XRUNTYI is to XR-XRTYI as the :UNTYI operation is to :TYI.
CH and NUM should be the third and second values returned by XR-XRTYI."
  (WHEN (= NUM (RDTBL-SLASH-CODE *READTABLE*))
    (FERROR 'READ-ERROR-1 "The character /"~C/" was slashified and cannot be UNTYIed." CH))
  (WHEN (AND CH (NOT (CONSP CH))
             (OR READ-PRESERVE-DELIMITERS (ZEROP (LOGAND 1 (RDTBL-BITS *READTABLE* CH)))))
    (SETQ XR-XRTYI-LAST-CHAR XR-XRTYI-PREV-CHAR)
    (SEND STREAM ':UNTYI CH)))

;(defun xr-xrtyipeek (&optional peek-type &rest read-args)
;  "If PEEK-TYPE is NIL, the default, returns the next character to be
;read from STREAM, part of READ-ARGS, without removing the character from
;the input stream.  If PEEK-TYPE is a fixnum less than 1000 octal, this
;reads characters until it gets one equal to PEEK-TYPE.  That character
;is not removed from the input stream.  If PEEK-TYPE is T, it skips over
;all the input characters until the start of the printed representation
;of a Lisp object is reached.  Characters passed over by TYIPEEK are echo
;if STREAM is interactive.."
;  (declare (arglist peek-type stream eof-option))
;  (multiple-value-bind (stream eof-option)
;      (decode-read-args read-args)
;    (and (numberp peek-type) ( peek-type #o1000)
;        (ferror "The ~S flavor of ~S is not implemented." peek-type 'tyipeek))
;    (do ((ch))       ;Pass over characters until termination condition reached
;       (())
;      (or (setq ch (send stream :tyi))
;         (if (eq eof-option 'no-eof-option)
;             (ferror 'sys:end-of-file-1 "End of file encountered on stream ~S." stream)
;             (return eof-option)))
;      (send stream :untyi ch)                  ;Put it back
;      (and (cond ((null peek-type))            ;Break on every
;                ((eq ch peek-type))            ;Break on specified character
;                ((eq peek-type t)              ;Break on start-of-object
;                 (and (< ch rdtbl-array-size)
;                      (zerop (logand (rdtbl-bits *readtable* ch) 1)))))
;          (return ch))                         ;Break here
;      (xr-xrtyi stream))))


;;;; Middle level of READ - the FSM processor.

(DEFVAR READ-TEMP-STRING NIL
  "String used for accumulating token by XR-READ-THING
and passed to read routines for types of tokens.
NIL means no string available for re-use now.")

(DEFVAR READ-STRING-COUNT 0
  "Counts each time READ makes a new temporary string.")

;; These save the time used by MAKE-ARRAY and also that used by RETURN-ARRAY.
(DEFMACRO RETURN-READ-STRING (STRING)
  `(SETQ READ-TEMP-STRING ,STRING))

(DEFMACRO GET-READ-STRING ()
  '(OR
     ;; WITHOUT-INTERRUPTS seems to take some time.
     (DO (OLD)
         ((%STORE-CONDITIONAL (LOCF READ-TEMP-STRING) (SETQ OLD READ-TEMP-STRING) NIL)
          OLD))
;     (WITHOUT-INTERRUPTS
;       (PROG1 READ-TEMP-STRING (SETQ READ-TEMP-STRING NIL)))
     (PROGN (INCF READ-STRING-COUNT)
            (MAKE-STRING #o100 :FILL-POINTER 0))))

;;; The specific functions called by XR-READ-THING can return anything as a second value
;;; if that thing is the symbol "READER-MACRO" then the first thing is called as a
;;; standard reader macro.
;;; If a read function is going to call READ, it should do a RETURN-READ-STRING
;;; on its second arg if that is actually a string.
;;; Then it should return T as its third arg
;;; to tell XR-READ-THING not to do a RETURN-READ-STRING itself.
;;; (that could cause the same string to be used by two processes at once).
(DEFUN XR-READ-THING (STREAM)
  (PROG (CH NUM A B STRING (INDEX 0) STLEN REAL-CH ALREADY-RETURNED
         (READTABLE-FSM (RDTBL-FSM *READTABLE*))
         (FNPROP (RDTBL-READ-FUNCTION-PROPERTY *READTABLE*))
         (STATE (RDTBL-STARTING-STATE *READTABLE*))
         READ-INSIDE-MULTIPLE-ESCAPE FOUND-MULTI-ESCAPES)
        (when (symbolp fnprop) (setq fnprop (list fnprop)))
        (MULTIPLE-VALUE-SETQ (CH NUM REAL-CH FOUND-MULTI-ESCAPES) (XR-XRTYI STREAM T))
        (SETQ STATE (AREF READTABLE-FSM STATE NUM))
        ;; Compensate for bad readtable in system 99.
        ;; Detect the case of a whitespace char following a pair of vertical bars.
        (AND (NULL STATE) FOUND-MULTI-ESCAPES
             (SETQ STATE '(UNTYI-FUNCTION . SYMBOL)))
        (UNLESS (NUMBERP STATE)
          (LET ((FLAG (CAR STATE))
                (TODO (CDR STATE)))
            (labels ((get-reader-function ()
                       (dolist (prop fnprop)
                         (let ((function (get todo prop)))
                           (when function
                             (return-from get-reader-function function))))))

            (CASE FLAG
              (NO-UNTYI-QUOTE
               (SETQ A TODO)
               (SETQ B 'SPECIAL-TOKEN))
              (LAST-CHAR
               (MULTIPLE-VALUE-SETQ (A B)
                 (FUNCALL (GET-READER-FUNCTION) STREAM NIL CH)))
              (NO-UNTYI-FUNCTION
               (IF *READ-SUPPRESS*
                   (SETQ A NIL B NIL)
                 (SETQ STRING (GET-READ-STRING))
                 (SETF (FILL-POINTER STRING) 1)
                 (SETF (CHAR STRING 0) CH)
                 (MULTIPLE-VALUE-SETQ (A B ALREADY-RETURNED)
                   (FUNCALL (GET-READER-FUNCTION) STREAM STRING))
                 (UNLESS ALREADY-RETURNED
                   (RETURN-READ-STRING STRING))))
              (UNTYI-QUOTE
               (FERROR 'READ-ERROR-1 "Reader in infinite loop reading character: /"~C/"."
                                     REAL-CH))
              (UNTYI-FUNCTION
               (IF (NOT FOUND-MULTI-ESCAPES)
                   (FERROR 'READ-ERROR-1 "Reader in infinite loop reading character: /"~C/"."
                                         REAL-CH)
                 (XR-XRUNTYI STREAM REAL-CH NUM)
                 (IF *READ-SUPPRESS*
                     (SETQ A NIL B NIL)
                   (MULTIPLE-VALUE-SETQ (A B)
                     (FUNCALL (GET-READER-FUNCTION) STREAM "")))))
              (OTHERWISE
               (FERROR 'READ-ERROR-1 "The reader found ~S in the finite state machine."
                                     FLAG)))
            (RETURN (VALUES A B)))))
        (SETQ STRING (GET-READ-STRING))
        (SETQ STLEN (ARRAY-LENGTH STRING))
     L  (SETF (CHAR STRING INDEX) CH)
        (INCF INDEX)
        (MULTIPLE-VALUE-SETQ (CH NUM REAL-CH) (XR-XRTYI STREAM))
        (SETQ STATE (AREF READTABLE-FSM STATE NUM))
        (WHEN (NUMBERP STATE)
          (WHEN (= INDEX STLEN)
            (SETQ STLEN (+ 32. STLEN))
            (ADJUST-ARRAY-SIZE STRING STLEN)
            (SETQ STRING (FOLLOW-STRUCTURE-FORWARDING STRING)))
          (GO L))
        (LET ((FLAG (CAR STATE))
              (TODO (CDR STATE)))
            (labels ((get-reader-function ()
                       (dolist (prop fnprop)
                         (let ((function (get todo prop)))
                           (when function
                             (return-from get-reader-function function))))))

          (CASE FLAG
            (UNTYI-FUNCTION
             (XR-XRUNTYI STREAM REAL-CH NUM)
             (SETF (FILL-POINTER STRING) INDEX)
             (IF *READ-SUPPRESS*
                 (SETQ A NIL B NIL)
               (MULTIPLE-VALUE (A B ALREADY-RETURNED)
                 (FUNCALL (GET-READER-FUNCTION) STREAM STRING))))
            (LAST-CHAR
             (SETF (FILL-POINTER STRING) INDEX)
             (MULTIPLE-VALUE (A B ALREADY-RETURNED)
               (FUNCALL (GET-READER-FUNCTION)
                        STREAM STRING CH)))
            (NO-UNTYI-FUNCTION
             (SETF (FILL-POINTER STRING) (1+ INDEX))
             (SETF (AREF STRING INDEX) CH)
             (IF *READ-SUPPRESS*
                 (SETQ A NIL B NIL)
               (MULTIPLE-VALUE (A B ALREADY-RETURNED)
                 (FUNCALL (GET-READER-FUNCTION) STREAM STRING))))
            (UNTYI-QUOTE
             (XR-XRUNTYI STREAM REAL-CH NUM)
             (SETQ A TODO)
             (SETQ B 'SPECIAL-TOKEN))
            (NO-UNTYI-QUOTE
             (SETQ A TODO)
             (SETQ B 'SPECIAL-TOKEN))
            (OTHERWISE
             (FERROR 'READ-ERROR-1 "The reader found ~S in the finite state machine." FLAG)))
          (UNLESS ALREADY-RETURNED
            (RETURN-READ-STRING STRING))
          (RETURN (VALUES A B))))))

(DEFVAR XR-LIST-SO-FAR :UNBOUND
  "When a reader macro is called, this contains the list so far at the current level.")

(DEFVAR XR-SPLICE-P :UNBOUND
  "A reader macro can set this to NIL to indicate that it has modified XR-LIST-SO-FAR.")

(DEFVAR XR-MACRO-ALIST-ENTRY-CDR :UNBOUND)

(DEFUN INVOKE-READER-MACRO (MACRO STREAM)
  (LET ((XR-MACRO-ALIST-ENTRY-CDR MACRO))
    (MULTIPLE-VALUE-LIST (FUNCALL (IF (CONSP MACRO) (CAR MACRO) MACRO)
                                  STREAM XR-XRTYI-LAST-CHAR))))

(DEFUN XR-DISPATCH-MACRO-DRIVER (STREAM MACRO-CHAR)
  (DO (ARG TEM CHAR)
      (())
    (SETQ CHAR (XR-XRTYI STREAM NIL T))
    (COND ((NULL CHAR)
           (CERROR :NO-ACTION NIL 'READ-END-OF-FILE
                   "End of file after dispatch macro character ~C." MACRO-CHAR)
           (RETURN NIL))
          ((SETQ TEM (CDR (ASSQ CHAR '((#/ . 1) (#/ . 2) (#/ . 3) (#/ . 4) (#/ . 8)))))
           (SETQ ARG (LOGIOR (OR ARG 0) TEM)))
          (( #/0 CHAR #/9)
           (SETQ ARG (+ (* 10. (OR ARG 0)) (- CHAR #/0))))
          (T
           (LET ((FN (CADR (ASSQ (CHAR-UPCASE CHAR)
                                 (CDDR XR-MACRO-ALIST-ENTRY-CDR)))))
             (IF (NULL FN)
                 (RETURN (READ-ERROR "Undefined dispatch macro combination ~C~C."
                                     MACRO-CHAR CHAR))
               (RETURN (FUNCALL FN STREAM CHAR ARG))))))))

;;;; Top levels of READ
;;; (-really- (ie user-callable) top levels of read are in io;input-readers)

;;; This variable helps implement the labeled object reader macros #n# and #n=.
;;; See the Common Lisp manual for how they are supposed to work.
(DEFVAR XR-LABEL-BINDINGS)

(DEFVAR READ-STREAM :UNBOUND
  "Within READ, the stream being read from.  For creating error objects.")

(DEFUN INTERNAL-READ (&OPTIONAL (STREAM *STANDARD-INPUT*)
                      (EOF-ERRORP T) EOF-VALUE RECURSIVE-P
                      PRESERVE-WHITESPACE DISCARD-CLOSEPARENS CHECK-INDENTATION
                      &AUX W-O)
  "Read an s-expression from STREAM and return it.
End of file within an s-expression is an error.
End of file with no s-expression seen is controlled by EOF-ERRORP.
T means it is an error then too.  NIL means that end of file
with no s-expression returns EOF-VALUE.

RECURSIVE-P non-NIL is used for recursive calls, e.g. from read macro definitions.
Recursive calls must be distinguished to make READ-PRESERVING-WHITESPACE
and #n= /"labels/" work properly.

PRESERVE-WHITESPACE if non-NIL says do not discard the terminating delimiter even
if it is whitespace.  This argument is ignored if RECURSIVE-P is non-NIL,
and the outer, nonrecursive call gets to control the matter.

DISCARD-CLOSEPARENS if non-NIL says if we see a close paren
just keep reading past it, with no error.

CHECK-INDENTATION controls whether indentation is checked within this
s-expression.  If RECURSIVE-P is non-NIL, this argument is ignored
and the outer, nonrecursive call gets to control the matter."
  (COND ((EQ STREAM T) (SETQ STREAM *TERMINAL-IO*))
        ((EQ STREAM NIL) (SETQ STREAM *STANDARD-INPUT*)))
  (LET-IF (NOT RECURSIVE-P)
          ((XR-LABEL-BINDINGS NIL)
           (READ-PRESERVE-DELIMITERS PRESERVE-WHITESPACE)
           (READ-CHECK-INDENTATION CHECK-INDENTATION)
           (XR-XRTYI-LAST-CHAR #/RETURN)
           (XR-XRTYI-PREV-CHAR NIL)
           (READ-STREAM STREAM)
           (MISSING-CLOSEPAREN-REPORTED NIL))
    (SETQ W-O (SEND STREAM ':WHICH-OPERATIONS))
    (COND ((MEMQ ':READ W-O)
           (SEND STREAM ':READ NIL))
;         ((AND (NOT RECURSIVE-P) (NEQ RUBOUT-HANDLER STREAM) (MEMQ ':RUBOUT-HANDLER W-O))
;          (WITH-INPUT-EDITING (STREAM '((:ACTIVATION CHAR= #/END)))
;            (INTERNAL-READ STREAM EOF-ERRORP EOF-VALUE T)))
          ((PROG (THING TYPE XR-SHARP-ARGUMENT)
               A (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
                 (COND ((EQ TYPE 'READER-MACRO)
                        (LET ((XR-LIST-SO-FAR ':TOPLEVEL)
                              (XR-SPLICE-P NIL)
                              VALUES)
                          (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
                          (IF (OR XR-SPLICE-P
                                  ( (LENGTH VALUES) 1))
                              (GO A))
                          (RETURN (CAR VALUES))))
                       ((EQ TYPE 'SPECIAL-TOKEN)
                        (COND ((EQ THING 'EOF)
                               (IF EOF-ERRORP
                                   (CERROR :NO-ACTION NIL 'READ-END-OF-FILE
                                     "End of file encountered by the reader on stream ~S."
                                     STREAM)
                                 (RETURN EOF-VALUE)))
                              ((AND DISCARD-CLOSEPARENS
                                    (EQ THING 'CLOSE))
                               (GO A))
                              (T
                               (READ-ERROR "The special token ~S was read in at top level."
                                           THING))))
                       (T (RETURN THING))))))))

(DEFUN READ-RECURSIVE (&OPTIONAL (STREAM *STANDARD-INPUT*))
  "Readmacros that wish to read an expression should use this funtion instead of READ."
  (INTERNAL-READ STREAM T NIL T))

(DEFUN READ-DELIMITED-LIST (STOP-CHAR &OPTIONAL (STREAM *STANDARD-INPUT*) RECURSIVE-P)
  "Read objects from STREAM until STOP-CHAR is seen; return a list of the objects read.
STOP-CHAR should not be a whitespace character in the current Readtable;
for best results, its syntax should be the standard syntax of closeparen.
RECURSIVE-P should be supplied non-NIL when this is called from a reader macro."
  (LET-IF (NOT RECURSIVE-P)
          ((XR-LABEL-BINDINGS NIL)
           (XR-XRTYI-LAST-CHAR #/RETURN)
           (XR-XRTYI-PREV-CHAR NIL)
           (READ-CHECK-INDENTATION NIL)
           (READ-PRESERVE-DELIMITERS NIL))
    (IF (AND (NOT RECURSIVE-P) (NEQ RUBOUT-HANDLER STREAM)
             (SEND STREAM :OPERATION-HANDLED-P :RUBOUT-HANDLER))
        ;;We must get inside the rubout handler's top-level CATCH
        (WITH-INPUT-EDITING (STREAM '((:ACTIVATION CHAR= #/END)))
          (READ-DELIMITED-LIST STOP-CHAR STREAM))
      (PROG (LIST THING TYPE END-OF-LIST BP CORRESPONDENCE-ENTRY
             (INSIDE-COLUMN-0-LIST INSIDE-COLUMN-0-LIST)
             (THIS-IS-COLUMN-0-LIST (AND READ-CHECK-INDENTATION
                                         (EQ XR-XRTYI-PREV-CHAR #/RETURN))))
            (AND THIS-IS-COLUMN-0-LIST
                 (NOT *READ-SUPPRESS*)
                 (IF (NOT INSIDE-COLUMN-0-LIST)
                     (SETQ INSIDE-COLUMN-0-LIST T)
                   ;; ( in column 0 when not allowed.
                   ;; Report it (but only report each occurrence once).
                   (OR MISSING-CLOSEPAREN-REPORTED
                       (PROGN (SETQ MISSING-CLOSEPAREN-REPORTED T)
                              (SIGNAL-PROCEED-CASE (() 'MISSING-CLOSEPAREN
                                                       "Open paren found in column zero; missing closeparens assumed.")
                                (:NO-ACTION))))
                   ;; Unread it and pretend it was a ).
                   (XR-XRUNTYI STREAM XR-XRTYI-LAST-CHAR -1)
                   (RETURN (VALUES 'CLOSE 'SPECIAL-TOKEN))))
            (SETQ MISSING-CLOSEPAREN-REPORTED NIL)
            (SETQ END-OF-LIST (LOCF LIST))
            (WHEN XR-CORRESPONDENCE-FLAG
              (SEND STREAM :UNTYI XR-XRTYI-LAST-CHAR)
              (SETQ CORRESPONDENCE-ENTRY
                    `(NIL ,(SEND STREAM :READ-BP) NIL . ,XR-CORRESPONDENCE))
              (SETQ XR-CORRESPONDENCE CORRESPONDENCE-ENTRY)
              (SEND STREAM :TYI))
         A
            (WHEN XR-CORRESPONDENCE-FLAG
              (PUSH (SEND STREAM :READ-BP) (CADDR CORRESPONDENCE-ENTRY)))
            ;; Peek ahead to look for the terminator we expect.
            (MULTIPLE-VALUE-BIND (CHAR NUM ACTUAL-CHAR)
                (XR-XRTYI STREAM T T)
              (WHEN (= CHAR STOP-CHAR)
                (RETURN LIST))
              (XR-XRUNTYI STREAM ACTUAL-CHAR NUM))
            ;; Read the next token, or a macro character.
            (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
            ;; If this is the first element of a list starting in column 0,
            ;; and it is EVAL-WHEN or something like that,
            ;; say it is ok for our sublists to start in column 0.
            (AND THIS-IS-COLUMN-0-LIST
                 (EQ END-OF-LIST (LOCF LIST))
                 (SYMBOLP THING)
                 ;; It is usually dangerous for READ to look at properties of symbols,
                 ;; but this will only happen for the symbol after a paren in column 0
                 ;; and never in lists read in interactively.
                 (GET THING 'MAY-SURROUND-DEFUN)
                 (SETQ INSIDE-COLUMN-0-LIST NIL))
            (COND ((EQ TYPE 'READER-MACRO)
                   (WHEN XR-CORRESPONDENCE-FLAG
                     (SEND STREAM :UNTYI XR-XRTYI-LAST-CHAR)
                     (SETQ BP (SEND STREAM :READ-BP))
                     (SEND STREAM :TYI))
                   (LET ((XR-LIST-SO-FAR LIST)
                         (XR-SPLICE-P NIL)
                         VALUES)
                     (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
                     (COND (XR-SPLICE-P
                            (SETQ LIST XR-LIST-SO-FAR)
                            (AND XR-CORRESPONDENCE-FLAG
                                 (SETF (CADDR CORRESPONDENCE-ENTRY)
                                       (FIRSTN (LENGTH LIST) (CADDR CORRESPONDENCE-ENTRY))))
                            (SETQ END-OF-LIST (IF (ATOM LIST) (LOCF LIST) (LAST LIST))))
                           (VALUES
                            (SETF (CDR END-OF-LIST)
                                  (SETQ VALUES (COPY-LIST VALUES READ-AREA)))
                            (SETQ END-OF-LIST (LAST VALUES))
                            (WHEN XR-CORRESPONDENCE-FLAG
                              (SETQ XR-CORRESPONDENCE
                                    `(,(CAR VALUES) ,BP NIL . ,XR-CORRESPONDENCE))))))
                   (GO A))
                  ((EQ TYPE 'SPECIAL-TOKEN)
                   (COND ((EQ THING 'EOF)
                          (OR (AND READ-CHECK-INDENTATION MISSING-CLOSEPAREN-REPORTED)
                              (SIGNAL-PROCEED-CASE (() 'READ-LIST-END-OF-FILE
                                                       "End of file on ~S in the middle of the list ~:S."
                                                       STREAM LIST)
                                (:NO-ACTION
                                 (IF READ-CHECK-INDENTATION
                                     (SETQ MISSING-CLOSEPAREN-REPORTED T)))))
                          (RETURN (VALUES LIST 'LIST)))
                         (T (READ-ERROR "Incorrect terminator: /"~C/" not /"~C/"."
                                        XR-XRTYI-LAST-CHAR
                                        STOP-CHAR))))
                  (T
                   (SETF (CDR END-OF-LIST)
                         (SETQ END-OF-LIST (NCONS-IN-AREA THING READ-AREA)))
                   (GO A)))))))


;;; This ends the reader proper. The things from here on are called only if they appear in
;;; the readtable itself.  However, XR-READ-LIST is somewhat special in that it handles
;;; splicing macros.

;;; Symbols which can head lists which go around defuns
;;; are given the MAY-SURROUND-DEFUN property.
(DEFPROP EVAL-WHEN T MAY-SURROUND-DEFUN)
(DEFPROP PROGN T MAY-SURROUND-DEFUN)
(DEFPROP LOCAL-DECLARE T MAY-SURROUND-DEFUN)
(DEFPROP DECLARE-FLAVOR-INSTANCE-VARIABLES T MAY-SURROUND-DEFUN)
(DEFPROP COMPILER-LET T MAY-SURROUND-DEFUN)
(DEFPROP COMMENT T MAY-SURROUND-DEFUN)
(DEFPROP QUOTE T MAY-SURROUND-DEFUN)
(DEFPROP SETQ T MAY-SURROUND-DEFUN)

;;; Note that the second arg (FIFTY) should be a number (50) rather than a string ("(")
;;; due to the LAST-CHAR hack.
(DEFUN XR-READ-LIST (STREAM SHOULD-BE-NIL FIFTY)
  (DECLARE (IGNORE SHOULD-BE-NIL))              ;Ignored.  This would be the string if there were one
  (PROG (LIST THING TYPE END-OF-LIST BP CORRESPONDENCE-ENTRY
         (INSIDE-COLUMN-0-LIST INSIDE-COLUMN-0-LIST)
         (THIS-IS-COLUMN-0-LIST (AND READ-CHECK-INDENTATION
                                     (= XR-XRTYI-PREV-CHAR #/RETURN))))
        (AND THIS-IS-COLUMN-0-LIST
             (NOT *READ-SUPPRESS*)
             (IF (NOT INSIDE-COLUMN-0-LIST)
                 (SETQ INSIDE-COLUMN-0-LIST T)
               ;; ( in column 0 when not allowed.
               ;; Report it (but only report each occurrence once).
               (OR MISSING-CLOSEPAREN-REPORTED
                   (PROGN (SETQ MISSING-CLOSEPAREN-REPORTED T)
                          (SIGNAL-PROCEED-CASE (() 'MISSING-CLOSEPAREN
                                                   "Open paren found in column zero; missing closeparens assumed.")
                            (:NO-ACTION))))
               ;; Unread it the char.  The -1 prevents barfage in XR-XRUNTYI, that's all.
               (XR-XRUNTYI STREAM XR-XRTYI-LAST-CHAR -1)
               ;; Set a signal for the XR-READ-LIST frame that called us.
               (SETQ XR-SPLICE-P 'MISSING-CLOSEPAREN)
               (RETURN NIL)))
        (SETQ MISSING-CLOSEPAREN-REPORTED NIL)
        (SETQ END-OF-LIST (LOCF LIST))
        (WHEN XR-CORRESPONDENCE-FLAG
          (SEND STREAM :UNTYI FIFTY)
          (SETQ CORRESPONDENCE-ENTRY
                `(NIL ,(SEND STREAM :READ-BP) NIL . ,XR-CORRESPONDENCE))
          (SETQ XR-CORRESPONDENCE CORRESPONDENCE-ENTRY)
          (SEND STREAM :TYI))
     A
        (WHEN XR-CORRESPONDENCE-FLAG
          (PUSH (SEND STREAM :READ-BP) (CADDR CORRESPONDENCE-ENTRY)))
        (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
        ;; If this is the first element of a list starting in column 0,
        ;; and it is EVAL-WHEN or something like that,
        ;; say it is ok for our sublists to start in column 0.
        (AND THIS-IS-COLUMN-0-LIST
             (EQ END-OF-LIST (LOCF LIST))
             (SYMBOLP THING)
             ;; It is usually dangerous for READ to look at properties of symbols,
             ;; but this will only happen for the symbol after a paren in column 0
             ;; and never in lists read in interactively.
             (GET THING 'MAY-SURROUND-DEFUN)
             (SETQ INSIDE-COLUMN-0-LIST NIL))
        (COND ((EQ TYPE 'READER-MACRO)
               (WHEN XR-CORRESPONDENCE-FLAG
                 (SEND STREAM :UNTYI FIFTY)
                 (SETQ BP (SEND STREAM :READ-BP))
                 (SEND STREAM :TYI))
               (LET ((XR-LIST-SO-FAR LIST)
                     (XR-SPLICE-P NIL)
                     VALUES)
                 (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
                 (COND ((EQ XR-SPLICE-P 'MISSING-CLOSEPAREN)
                        ;; This means that the reader macro was (
                        ;; and it was unhappy about being at column 0
                        ;; inside another column 0 list.
                        ;; Pretend we saw a ).
                        (WHEN XR-CORRESPONDENCE-FLAG
                          (SETF (CAR CORRESPONDENCE-ENTRY) LIST)
                          (SETF (CADDR CORRESPONDENCE-ENTRY)
                                (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
                        (RETURN (VALUES LIST 'LIST)))
                       (XR-SPLICE-P
                        (SETQ LIST XR-LIST-SO-FAR)
                        (AND XR-CORRESPONDENCE-FLAG
                             (SETF (CADDR CORRESPONDENCE-ENTRY)
                                   (FIRSTN (LENGTH LIST) (CADDR CORRESPONDENCE-ENTRY))))
                        (SETQ END-OF-LIST (IF (ATOM LIST) (LOCF LIST) (LAST LIST))))
                       (VALUES
                        (SETF (CDR END-OF-LIST) (SETQ VALUES (COPY-LIST VALUES READ-AREA)))
                        (SETQ END-OF-LIST (LAST VALUES))
                        (WHEN XR-CORRESPONDENCE-FLAG
                          (SETQ XR-CORRESPONDENCE
                                `(,(CAR VALUES) ,BP NIL . ,XR-CORRESPONDENCE))))))
               (GO A))
              ((EQ TYPE 'SPECIAL-TOKEN)
               (COND ((EQ THING 'CLOSE)
                      (WHEN XR-CORRESPONDENCE-FLAG
                        (SETF (CADR CORRESPONDENCE-ENTRY) LIST)
                        (SETF (CADDR CORRESPONDENCE-ENTRY)
                              (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
                      (RETURN (VALUES LIST 'LIST)))
                     ((EQ THING 'EOF)
                      (OR (AND READ-CHECK-INDENTATION MISSING-CLOSEPAREN-REPORTED)
                          (SIGNAL-PROCEED-CASE (() 'READ-LIST-END-OF-FILE
                                                   "End of file on ~S in the middle of the list ~:S."
                                                   STREAM LIST)
                            (:NO-ACTION
                             (IF READ-CHECK-INDENTATION
                                 (SETQ MISSING-CLOSEPAREN-REPORTED T)))))
                      (RETURN (VALUES LIST 'LIST)))
                     (*READ-SUPPRESS* NIL)
                     ((EQ THING 'CONSING-DOT)
                      (WHEN (NULL LIST)
                        (READ-ERROR "A dot was read before any list was accumulated.")
                        (GO A))
                      (GO RDOT))
                     (T (READ-ERROR "The special token ~S was read in the middle of the list ~:S."
                                    THING LIST))))
              (T
               (SETF (CDR END-OF-LIST) (SETQ END-OF-LIST (NCONS-IN-AREA THING READ-AREA)))
               (GO A)))
     RDOT
        (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
        (WHEN (EQ TYPE 'SPECIAL-TOKEN)
          (READ-ERROR "The special token ~S was read after a dot." THING)
          (GO RDOT))
        (WHEN (EQ TYPE 'READER-MACRO)
          (LET ((XR-LIST-SO-FAR ':AFTER-DOT)
                (XR-SPLICE-P NIL)
                VALUES)
            (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
            (WHEN XR-SPLICE-P
              (SETQ LIST XR-LIST-SO-FAR)
              (GO RDOT))
            (WHEN (NULL VALUES)
              (GO RDOT))
            (SETQ THING (CAR VALUES))))
        (SETF (CDR END-OF-LIST) THING)
     RDOT-1
        (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
        (COND ((AND (EQ THING 'CLOSE) (EQ TYPE 'SPECIAL-TOKEN))
               (WHEN XR-CORRESPONDENCE-FLAG
                 (SETF (CAR CORRESPONDENCE-ENTRY) LIST)
                 (SETF (CADDR CORRESPONDENCE-ENTRY)
                       (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
               (RETURN (VALUES LIST 'LIST)))
              ((EQ TYPE 'READER-MACRO)
               (LET ((XR-LIST-SO-FAR ':AFTER-DOT)
                     (XR-SPLICE-P NIL)
                     VALUES)
                 (SETQ VALUES (INVOKE-READER-MACRO THING STREAM))
                 (WHEN (OR XR-SPLICE-P (NULL VALUES))
                   (GO RDOT-1)))
               (READ-ERROR "~S was read instead of a close paren (returned by a reader macro)."
                           THING))
              (T (READ-ERROR "~S was read instead of a close paren." THING)))
        ;; Here only if error condition gets handled.
        (SETF (CDR END-OF-LIST) (LIST (CDR END-OF-LIST) THING))
        (SETQ END-OF-LIST (LAST END-OF-LIST))
        (GO A)))

(DEFUN (:PROPERTY MACRO-CHAR STANDARD-READ-FUNCTION) (STREAM SHOULD-BE-NIL LAST-CHAR &AUX TEM)
  (DECLARE (IGNORE STREAM SHOULD-BE-NIL))
  (IF (SETQ TEM (ASSQ LAST-CHAR (RDTBL-MACRO-ALIST *READTABLE*)))
      (IF (AND (CONSP (CDR TEM)) (EQ (CADR TEM) 'XR-CLOSEPAREN-MACRO))
          (VALUES 'CLOSE 'SPECIAL-TOKEN)
          (VALUES (CDR TEM) 'READER-MACRO))
    (READ-ERROR "No reader macro definition found for the character ~C." LAST-CHAR)))

;;;; reading symbols

;;; bound to T by package prefix
(DEFVAR-RESETTABLE *INHIBIT-READER-SYMBOL-SUBSTITUTION* NIL NIL)

(DEFPROP SYMBOL XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFPROP SC-SYMBOL XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFPROP POTENTIAL-NUMBER XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFUN XR-READ-SYMBOL (STREAM STRING)
  (DECLARE (IGNORE STREAM))                     ;doesn't do any additional reading
  (IF *READ-SUPPRESS*
      (VALUES NIL 'SYMBOL)
    (LET ((*PACKAGE* (CURRENT-PACKAGE)))
      (MULTIPLE-VALUE-BIND (READ FLAG)
          (FUNCALL READ-INTERN-FUNCTION STRING)
        (DECLARE (IGNORE FLAG))
        (UNLESS *INHIBIT-READER-SYMBOL-SUBSTITUTION*
          (LET ((SUBST (ASSQ READ (RDTBL-SYMBOL-SUBSTITUTIONS *READTABLE*))))
            (AND SUBST (SETQ READ (CDR SUBST)))))
        (VALUES READ 'SYMBOL)))))

;;; FOO: switches us to the package associated with the string "FOO"
;;; FOO:: is Common Lisp for "allow internal symbols", but we usually do that,
;;;  since *read-single-colon-allow-internal-symbol* is usually T
(DEFUN (:PROPERTY PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING IGNORE)
  (PROG (THING PK
         ;; Help un-screw the user if *PACKAGE* gets set to NIL.
         (*PACKAGE* (OR *PACKAGE* PKG-USER-PACKAGE))
         INTERNAL-OK ENTIRE-LIST-PREFIXED)
        ;; Gobble the second colon, if any, and set flag if found.
        ;; Note that we do not, currently, DO anything with the flag!
        (MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
            (XR-XRTYI STREAM NIL T)
          (if (null ch)
              (return (cerror :no-action nil 'read-symbol-end-of-file
                          "EOF after package prefix"))
            (IF (= CH #/:)
                (SETQ INTERNAL-OK T)
              (IF (= CH #/()
                  (SETQ ENTIRE-LIST-PREFIXED T))
              (XR-XRUNTYI STREAM REAL-CH NUM))))
        ;; Try to find the package.
        ;;don't try to find packages if we're not interning -- eg #+slime (dis:foo)
        (UNLESS *READ-SUPPRESS*
          (DO ((STRING1 (OR STRING "")))
              ((SETQ PK (FIND-PACKAGE STRING1 *PACKAGE*)))
            ;; Package not found.
            (SIGNAL-PROCEED-CASE ((PKG) 'READ-PACKAGE-NOT-FOUND
                                        "Package ~S does not exist."
                                        STRING1)
              (:NO-ACTION
               (RETURN))
              (:NEW-NAME
               (LET ((*PACKAGE* PKG-USER-PACKAGE))
                 (SETQ STRING1 (STRING (READ-FROM-STRING PKG)))))
              (:CREATE-PACKAGE
               (OR (FIND-PACKAGE STRING1 *PACKAGE*)
                   (MAKE-PACKAGE STRING1))))))
        (OR PK (SETQ PK PKG-USER-PACKAGE))
        (WHEN STRING (RETURN-READ-STRING STRING))
        (LET ((*PACKAGE* PK)
              (*INHIBIT-READER-SYMBOL-SUBSTITUTION* T)
              (READ-INTERN-FUNCTION (COND ((OR (AND (PKG-AUTO-EXPORT-P PK)
                                                    (PACKAGE-USED-BY-LIST PK)
                                                    *read-barf-if-auto-export?*)
                                               (PKG-READ-LOCK-P PK))
                                           'READ-INTERN-SOFT)
                                          ((OR ENTIRE-LIST-PREFIXED (EQ PK *PACKAGE*))
                                           ;; Here for, e.g., SI: while in SI already.
                                           ;; Also here for ZWEI:(BP-LINE (POINT));
                                           ;; such constructs are not valid Common Lisp
                                           ;; so let's keep their meaning the same.
                                           READ-INTERN-FUNCTION)
                                          ((OR INTERNAL-OK
                                               (PKG-AUTO-EXPORT-P PK)
                                               (EQ *READ-SINGLE-COLON-ALLOW-INTERNAL-SYMBOL* T))
                                           'INTERN)
                                          (T
                                           'READ-PACKAGE-PREFIX-EXTERNAL-INTERN))))
          (SETQ THING (INTERNAL-READ STREAM T NIL T)))
        (RETURN (VALUES THING
                        (TYPE-OF THING)
                        T))))   ;T means we already did RETURN-READ-STRING

;>> this should signal an error with proceed-types to offering to export the symbol
(defun read-package-prefix-external-intern (string)
  (flet ((foo (&rest args)
           (cond ((eq *package* (find-package 'keyword)) nil)   ;Don't barf on keywords.
                 ((eq *read-single-colon-allow-internal-symbol* ':warn)
                  (fresh-line *error-output*)
                  (apply #'format *error-output* args))
                 (t (apply #'read-error args)))))
    (multiple-value-bind (sym flag pkg)
        (intern-soft string *package*)
      (case flag
        ((nil)
         (foo "Reference to nonexistent symbol ~S in package ~A using a single colon prefix."
              string *package*)
         (intern string pkg))
        (:internal
         (foo "Reference to non-external symbol ~S in package ~A using a single colon prefix."
              sym *package*)
         (values sym flag pkg))
        (t                                      ;:external or :inherited
         (values sym flag pkg))))))

;;; FOO#: is like FOO:: but ignores local nicknames.
;;; Just #: means make uninterned symbol.
(DEFUN (:PROPERTY SHARP-PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
  (DECLARE (IGNORE LAST-CH))
  (PROG (THING TYPE PK
         ;; Help un-screw the user if *PACKAGE* gets set to NIL.
         (*PACKAGE* (OR *PACKAGE* PKG-USER-PACKAGE))
         (PKG-NAME (SUBSTRING STRING 0 (1- (LENGTH STRING)))))
        ;; Try to find the package.
        (UNLESS
          ;;don't try to find packages if we're not interning -- eg #+slime (dis:foo)
          (OR *READ-SUPPRESS* (SETQ PK (FIND-PACKAGE PKG-NAME NIL)))
          ;; Package not found.
          (READ-ERROR "Package ~S does not exist." PKG-NAME))
        (UNLESS PK
          (SETQ PK PKG-USER-PACKAGE))
        (RETURN-READ-STRING STRING)
        (LET ((*PACKAGE* PK)
              (READ-INTERN-FUNCTION
                (COND ((EQUAL PKG-NAME "")
                       'READ-UNINTERNED-SYMBOL)
                      ((OR (AND (PKG-AUTO-EXPORT-P PK)
                                (PACKAGE-USED-BY-LIST PK))
                           (PKG-READ-LOCK-P PK))
                       'READ-INTERN-SOFT)
                      (T
                       'INTERN))))
          (MULTIPLE-VALUE-SETQ (THING TYPE)
            (INTERNAL-READ STREAM T NIL T)))
        (SETQ PKG-NAME NIL)
        (RETURN (VALUES THING TYPE T))))        ;T means we already did RETURN-READ-STRING.

;;; "Intern" function used within #: prefixes that specify uninterned symbols.
(DEFUN READ-UNINTERNED-SYMBOL (STRING)
  (VALUES (MAKE-SYMBOL STRING T) NIL T))        ;Last value T avoids error in XR-READ-SYMBOL.

;;;Used for interning in special packages.  This now signals fatal
;;;errors to avoid interning within read-locked and auto-exporting
;;;packages.  Note the old broken definition. -Keith
;;; (defun read-intern-soft (string)
;;;   (multiple-value-bind (sym flag pkg)
;;;       (find-symbol string)
;;;     (unless flag
;;;       (read-error "Package ~A is ~:[autoexporting~;read-locked~]; ~
;;;                     the reader cannot make a symbol ~S there."
;;;                *package* (pkg-read-lock-p *package*) string)
;;;       (multiple-value-setq (sym flag pkg) (intern string *package*)))  ;was pkg-keyword-package??
;;;     (values sym flag pkg)))
;;;
(defun read-intern-soft (string &optional (error-p t))
  (declare (values symbol actually-found-flag actual-package))
  (multiple-value-bind (ignore flag ignore)
      (find-symbol string)
    (cond
      (flag
       (intern string *package*))
      ((null error-p) nil)
      ((pkg-read-lock-p *package*)
       (read-fatal-error "Package ~A is read-locked; ~
                        the reader cannot make a symbol ~S there."
                  *package* string))
      ((pkg-auto-export-p *package*)
       (read-fatal-error "Package ~A is auto-exporting; ~
                        the reader cannot make a symbol ~S there."
                  *package* string))
      (t
       (read-error "The symbol ~S is not found in package ~A."
                   string *package*)))))

(DEFUN (:PROPERTY MULTI-DOT STANDARD-READ-FUNCTION) (STREAM STRING)
  (READ-ERROR "The illegal token ~S was encountered on input by the reader." STRING)
  (XR-READ-SYMBOL STREAM STRING))

;;; This function is given a string, an index into the string, and the length
;;; of the string.  It looks for a signed, possibly decimal-pointed number,
;;; computes it, and returns two values: the fixnum, and the index in the
;;; string of where the first char was that caused it to stop.  The second
;;; value will equal the "length" argument if it got to the end of the string.
;;; it takes a base as well.
(DEFUN XR-READ-FIXNUM-INTERNAL (STRING II LEN &OPTIONAL (IBS (CURRENT-READ-BASE))
                                &AUX (SIGN 1) (NUM 0) CH)
  (SETQ CH (CHAR STRING II))
  (COND ((CHAR= CH #/+)
         (INCF II))
        ((CHAR= CH #/-)
         (INCF II)
         (SETQ SIGN -1)))
  (DO ((I II (1+ I)))
      (( I LEN)
       (VALUES (* SIGN NUM) LEN))
    (SETQ CH (CHAR STRING I))
    (COND ((CHAR #/0 CH #/9)
           (SETQ NUM (+ (* NUM IBS) (- CH #/0))))
          ((CHAR= CH #/.)
           (COND ((= IBS 10.)
                  (RETURN (VALUES (* SIGN NUM) (1+ I))))
                 (T
                  ;; Start from beginning, but base ten this time.
                  (SETQ IBS 10.)
                  (SETQ NUM 0)
                  (SETQ I (- II 1)))))
          ((CHAR #/A CH #/Z)
           (SETQ NUM (+ (* NUM IBS) (- CH (- #/A 10.)))))
          ((CHAR #/a CH #/z)
           (SETQ NUM (+ (* NUM IBS) (- CH (- #/a 10.)))))
          (T (RETURN (VALUES (* SIGN NUM) I))))))

;; This function takes a string which represents a fixnum (and a stream
;; which it doesn't use), and returns the fixnum.  It ASSUMES that the string
;; follows the format of the standard readtable.
(DEFPROP FIXNUM XR-READ-FIXNUM STANDARD-READ-FUNCTION)
(DEFUN XR-READ-FIXNUM (STREAM STRING)
  (DECLARE (IGNORE STREAM))
  (LET ((B (CURRENT-READ-BASE))
        (LEN (LENGTH STRING)))
    (MULTIPLE-VALUE-BIND (NUM I)
        (XR-READ-FIXNUM-INTERNAL STRING 0 LEN B)
      (VALUES
        (IF (= I LEN)
            NUM
          (LET ((NUM2 (XR-READ-FIXNUM-INTERNAL STRING (1+ I) LEN B)))
            (IF (= (CHAR STRING I) #/_)
                (ASH NUM NUM2)
              (* NUM (^ B NUM2)))))
        'FIXNUM))))




;;;; reading floats

(DEFMACRO SKIP-CHAR ()
  '(PROGN (DECF COUNT)
          (INCF INDEX)))

(DEFVAR POWERS-OF-10f0-TABLE :UNBOUND
  "Vector which indexed by I contains (^ 10. I) as a single-float.")
(DEFVAR NEGATIVE-POWERS-OF-10f0-TABLE :UNBOUND
  "Vector which indexed by i contains (- (^ 10. I)) as a single-float.")
(DEFVAR POWERS-OF-10f0-TABLE-LENGTH 308.)

(DEFUN XR-READ-FLONUM (STRING SFL-P &AUX (POWER-10 0) (INDEX 0) (POSITIVE T)
                                         (HIGH-PART 0) (LOW-PART 0) (NDIGITS 12.)
                                         COUNT CHAR STRING-LENGTH)
  (DECLARE (SPECIAL HIGH-PART LOW-PART NDIGITS INDEX COUNT POWER-10))
  (SETQ COUNT (LENGTH STRING)
        STRING-LENGTH COUNT)
  (SETQ CHAR (CHAR STRING INDEX))
  ;; Check for plus or minus
  (COND ((CHAR= CHAR #/+)
         (SKIP-CHAR))
        ((CHAR= CHAR #/-)
         (SETQ POSITIVE NIL)
         (SKIP-CHAR)))
  ;; skip leading zeros
  (DO ()
      ((CHAR (CHAR STRING INDEX) #/0))
    (SKIP-CHAR))
  (COND ((CHAR= (CHAR STRING INDEX) #/.)       ;If we hit a point, keep stripping 0's
         (SKIP-CHAR)
         (DO ()
             ((OR (< COUNT 2)                   ;Leave one digit at least
                  (NOT (CHAR= (CHAR STRING INDEX) #/0))))      ;Or non-zero digit
           (SKIP-CHAR)
           (INCF POWER-10))
         (XR-ACCUMULATE-DIGITS STRING T))
        ;; Accumulate digits up to the point or exponent (these are free)
        (T (XR-ACCUMULATE-DIGITS STRING NIL)
           (WHEN (CHAR= (CHAR STRING INDEX) #/.)
             (SKIP-CHAR)
             ;; Skip trailing zeros after the point.  This avoids having a
             ;; one in the lsb of 2.0 due to dividing 20. by 10.
             (LET ((IDX (STRING-SEARCH-NOT-SET '(#/0) STRING INDEX)))
               (COND ((NULL IDX) (SETQ COUNT 0))        ;Nothing but zeros there
                     ((NOT (MEMQ (CHAR STRING IDX) '(#/1 #/2 #/3 #/4 #/5
                                                     #/6 #/7 #/8 #/9)))
                      (SETQ INDEX IDX           ;Not digits there except zeros
                            COUNT (- STRING-LENGTH INDEX)))
                     (T                         ;Real digits present, scan normally
                      (XR-ACCUMULATE-DIGITS STRING T)))))))
  ;; Here we have read something up to exponent if it exists, or end of string
  (WHEN (> COUNT 0)
    (SKIP-CHAR)                                 ;Skip the exponent character
    (SETQ POWER-10 (- POWER-10
                      (XR-READ-FIXNUM-INTERNAL STRING
                                               INDEX
                                               STRING-LENGTH
                                               10.))))
  (LET ((NUM (IF SFL-P
                 (SMALL-FLOAT (XR-FLONUM-CONS HIGH-PART LOW-PART POWER-10))
                 (XR-FLONUM-CONS HIGH-PART LOW-PART POWER-10))))
    (VALUES (IF POSITIVE NUM (- NUM))
            (IF SFL-P 'SHORT-FLOAT 'SINGLE-FLOAT))))

(DEFUN XR-ACCUMULATE-DIGITS (STRING POST-DECIMAL &AUX CHAR)
  (DECLARE (SPECIAL HIGH-PART LOW-PART NDIGITS INDEX COUNT POWER-10))
  (DO () ((= COUNT 0))
    (SETQ CHAR (CHAR STRING INDEX))
    (UNLESS (CHAR #/0 CHAR #/9)
      (RETURN NIL))
    (IF ( NDIGITS 0)
        (UNLESS POST-DECIMAL (DECF POWER-10))
      (SETQ HIGH-PART (+ (* HIGH-PART 10.)
                         (LSH (%MULTIPLY-FRACTIONS LOW-PART 10.) 1))
            LOW-PART (%POINTER-TIMES LOW-PART 10.))
      (WHEN (MINUSP LOW-PART)                   ;Carried into sign-bit
        (INCF HIGH-PART)
        (SETQ LOW-PART (LOGAND LOW-PART MOST-POSITIVE-FIXNUM)))
      (SETQ LOW-PART (%POINTER-PLUS LOW-PART (- CHAR #/0)))
      (WHEN (MINUSP LOW-PART)                   ;Carried into sign-bit
        (INCF HIGH-PART)
        (SETQ LOW-PART (LOGAND LOW-PART MOST-POSITIVE-FIXNUM)))
      (WHEN POST-DECIMAL (INCF POWER-10)))
    (SKIP-CHAR)
    (DECF NDIGITS)))

;;; Here POWER-10 is the power of 10 that the number in high,,low should be
;;; divided by.
(DEFUN XR-FLONUM-CONS (HIGH LOW POWER-10 &AUX FLOAT-NUMBER)
  (SETQ FLOAT-NUMBER (%FLOAT-DOUBLE (LSH HIGH -1)
                                    (LOGIOR (ROT (LOGAND HIGH 1) -1) LOW)))
  (COND ((< POWER-10 0) (* FLOAT-NUMBER (XR-GET-POWER-10 (- POWER-10))))
        ((> POWER-10 0) (// FLOAT-NUMBER (XR-GET-POWER-10 POWER-10)))
        (T FLOAT-NUMBER)))

(DEFUN XR-GET-POWER-10 (POWER)
  ;; This check detects things grossly out of range.  Numbers almost out of range
  ;; can still get floating-point overflow errors in the reader when multiplying
  ;; the mantissa by the power of 10
  (IF ( POWER POWERS-OF-10F0-TABLE-LENGTH)
      (PROGN (READ-ERROR "~D is larger than the maximum allowed exponent, ~D."
                         POWER (1- POWERS-OF-10F0-TABLE-LENGTH))
             1)
    (AREF POWERS-OF-10F0-TABLE POWER)))

;;; Make a table of powers of 10 without accumulated roundoff error,
;;; by doing integer arithmetic and then floating.  Thus each table entry
;;; is the closest rounded flonum to that power of ten.
;;; It didn't used to work this way because there didn't use to be bignums.
(DEFUN XR-TABLE-SETUP ()
  (SETQ POWERS-OF-10F0-TABLE (MAKE-ARRAY POWERS-OF-10F0-TABLE-LENGTH
                                         :TYPE 'ART-FLOAT :AREA CONTROL-TABLES))
  (SETQ NEGATIVE-POWERS-OF-10F0-TABLE (MAKE-ARRAY POWERS-OF-10F0-TABLE-LENGTH
                                         :TYPE 'ART-FLOAT :AREA CONTROL-TABLES))
  (DO ((EXPT 0 (1+ EXPT))
       (POWER 1. (* POWER 10.))
       (FLOAT))
      (( EXPT POWERS-OF-10F0-TABLE-LENGTH))
    (SETQ FLOAT (FLOAT POWER))
    (SETF (AREF POWERS-OF-10F0-TABLE EXPT) FLOAT
          (AREF NEGATIVE-POWERS-OF-10F0-TABLE EXPT) (- FLOAT))))

(XR-TABLE-SETUP)

(DEFVAR *READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT
  "Default floating point type for READ to create.
This is the type of flonum that READ makes when there is no exponent
or when the exponent is introduced with /"E/".  If the input being read
uses /"S/", /"F/", /"L/" or /"D/" then that specifies the type explicitly
and this default is not used.")

(DEFUN (:PROPERTY FLOAT STANDARD-READ-FUNCTION) (STREAM STRING)
  (DECLARE (IGNORE STREAM))                     ;doesn't do any additional reading
  (XR-READ-FLONUM STRING (EQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT)))

(DEFUN (:PROPERTY SINGLE-FLOAT STANDARD-READ-FUNCTION) (STREAM STRING)
  (DECLARE (IGNORE STREAM))                     ;doesn't do any additional reading
  (XR-READ-FLONUM STRING NIL))

(DEFUN (:PROPERTY SHORT-FLOAT STANDARD-READ-FUNCTION) (STREAM STRING)
  (DECLARE (IGNORE STREAM))                     ;doesn't do any additional reading
  (XR-READ-FLONUM STRING T))

(defun (:property rational standard-read-function) (stream string
                                                    &aux (len (string-length string))
                                                         (b (current-read-base)))
   (declare (ignore stream))
   (multiple-value-bind (num i)
       (xr-read-fixnum-internal string 0 len b)
     (values
       (cl:// num (xr-read-fixnum-internal string (1+ i) len b))
       'rational)))

;;; this is for the 3+4i input format, not #c(3 4)
(defun (:property complex standard-read-function) (stream string &aux complex-start (zero 0))
  (declare (ignore stream))
  (do ((i 1 (1+ i)))
      ((= i (length string)))
    (when (and (memq (char string i) '(#/+ #/-))
               (not (alpha-char-p (char string (1- i)))))
      (return (setq complex-start i))))
  (values
    (complex
      (cond (complex-start (with-input-from-string (strm string :start zero :end complex-start)
                             (xr-read-thing strm)))
            (t (setq complex-start 0)))
      (with-input-from-string (strm string :start complex-start :end (1- (string-length string)))
        (xr-read-thing strm)))
    'complex))

;;;; Standard reader macros:

(DEFUN XR-QUOTE-MACRO (STREAM IGNORE)
  (LIST-IN-AREA READ-AREA 'QUOTE (INTERNAL-READ STREAM T NIL T)))

(DEFUN XR-COMMENT-MACRO (STREAM IGNORE)
  (SETQ XR-XRTYI-LAST-CHAR #/NEWLINE)
  (LOOP AS CH = (SEND STREAM :TYI)
        WHILE (AND CH ( CH #/NEWLINE))
     FINALLY (RETURN (VALUES))))

(DEFUN XR-OPENPAREN-MACRO (STREAM IGNORE)
  (VALUES (XR-READ-LIST STREAM NIL #/()))

(DEFUN XR-DOUBLEQUOTE-MACRO (STREAM MATCH)
  (LET* (CH NUM REAL-CH (I 0) (LEN #o100)
         (STRING (MAKE-STRING LEN))
         ;; Should have :AREA READ-AREA, but leave this out for now because the
         ;; compiler thinks it can use COPYTREE to copy forms out of the temp area.
         (TEM (RDTBL-SLASH-CODE *READTABLE*)))
    (DO-FOREVER
      (MULTIPLE-VALUE-SETQ (CH NUM REAL-CH) (XR-XRTYI STREAM NIL NIL T))
      (COND ((NULL CH)
             (CERROR :NO-ACTION NIL 'READ-STRING-END-OF-FILE
                     "EOF on ~S in the middle of a string."
                     STREAM STRING)
             (RETURN ""))
            ((AND (= (CHAR-CODE REAL-CH) MATCH)
                  (NOT (= NUM TEM)))
             (ADJUST-ARRAY-SIZE STRING I)
             (RETURN STRING))
            (T (SETF (CHAR STRING I) (CHAR-CODE REAL-CH))
               (INCF I)
               (WHEN (= I LEN)
                 (INCF LEN 32.)
                 (ADJUST-ARRAY-SIZE STRING LEN)))))))

;(DEFUN XR-VERTICALBAR-MACRO (STREAM MATCH)
;  (LET ((STRING (GET-READ-STRING)))
;    (UNWIND-PROTECT
;       (PROG (CH NUM REAL-CH (I 0) (LEN (ARRAY-LENGTH STRING)) R TEM)
;             (SETQ TEM (RDTBL-SLASH-CODE *READTABLE*))
;           L (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
;             (COND ((NULL CH)
;                    (CERROR ':NO-ACTION NIL 'READ-SYMBOL-END-OF-FILE
;                            "EOF on ~S in the middle of a quoted symbol."
;                            STREAM STRING)
;                    (RETURN NIL))
;                   ((AND (= (LDB %%CH-CHAR REAL-CH) MATCH)
;                         (NOT (= NUM TEM)))
;                    (SETF (FILL-POINTER STRING) I)
;                    ;; See if a : or #: follows.
;                    ;; If so, this is really a package prefix.
;                    (MULTIPLE-VALUE-BIND (CH NUM REAL-CH)
;                        (XR-XRTYI STREAM NIL T)
;                      (WHEN CH
;                        (IF (= CH #/:)
;                            (RETURN (FUNCALL (GET 'PACKAGE-PREFIX 'STANDARD-READ-FUNCTION)
;                                             STREAM STRING NIL)))
;                        (IF (NOT (= CH #/#))
;                            (XR-XRUNTYI STREAM REAL-CH NUM)
;                          (SETQ CH (XR-XRTYI STREAM))
;                          (WHEN (= CH #/:)
;                            (ARRAY-PUSH-EXTEND STRING #/#)
;                            (RETURN (FUNCALL (GET 'SHARP-PACKAGE-PREFIX
;                                                  'STANDARD-READ-FUNCTION)
;                                             STREAM STRING NIL)))
;                          (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
;                                  "#~C appeared immediately following a vertical-bar ending a quoted symbol." CH))))
;                    (IF *READ-SUPPRESS*
;                        (RETURN NIL 'SYMBOL))
;                    (SETF R (FUNCALL READ-INTERN-FUNCTION STRING))
;                    (RETURN (OR (CDR (ASSQ R (RDTBL-SYMBOL-SUBSTITUTIONS *READTABLE*))) R)))
;                   (T (AS-1 (LDB %%CH-CHAR REAL-CH) STRING I)
;                      (SETQ I (1+ I))
;                      (COND ((= I LEN)
;                             (SETQ LEN (+ LEN 32.))
;                             (ADJUST-ARRAY-SIZE STRING LEN)
;                             (SETQ STRING (FOLLOW-STRUCTURE-FORWARDING STRING))))
;                      (GO L))))
;      (RETURN-READ-STRING STRING))))

;;;BACKQUOTE:
;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a                    ;the NIL flag is used only when a is NIL
;;;      T: [a] => a                    ;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;    \ car   ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|      |
;;;  cdr \     ||                 |    T or NIL     |                |                |
;;;====================================================================================
;;;    |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a    |
;;; QUOTE or T || LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC  (a [d]) |
;;;   APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC  (a [d]) |
;;;   NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a . d) |
;;;    LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead of ",@a)"

(DEFCONST **BACKQUOTE-/,-FLAG** (MAKE-SYMBOL ","))
(DEFCONST **BACKQUOTE-/,/@-FLAG** (MAKE-SYMBOL ",@"))
(DEFCONST **BACKQUOTE-/,/.-FLAG** (MAKE-SYMBOL ",."))

;Expansions of backquotes actually use these five functions
;so that one can recognize what came from backquote and what did not.

(DEFMACRO XR-BQ-CONS (CAR CDR)
  `(CONS ,CAR ,CDR))

(DEFMACRO XR-BQ-LIST (&REST ELEMENTS)
  `(LIST . ,ELEMENTS))

(DEFMACRO XR-BQ-LIST* (&REST ELEMENTS)
  `(LIST* . ,ELEMENTS))

(DEFMACRO XR-BQ-APPEND (&REST ELEMENTS)
  `(APPEND . ,ELEMENTS))

(DEFMACRO XR-BQ-NCONC (&REST ELEMENTS)
  `(NCONC . ,ELEMENTS))

(DEFMACRO XR-BQ-VECTOR (&REST ELEMENTS)
  `(VECTOR . ,ELEMENTS))

(defmacro XR-BQ-VECTOR* (ELEMENT)
  `(if (consp ,element)
       (apply #'vector ,element)
     (vector ,element)))

(DEFVAR **BACKQUOTE-REPEAT-VARIABLE-LISTS** NIL)

(DEFUN XR-BACKQUOTE-MACRO (STREAM IGNORE)
  (LET* ((**BACKQUOTE-REPEAT-VARIABLE-LISTS** (CONS T **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
         (object (INTERNAL-READ STREAM T NIL T))
         (vector-p (simple-vector-p object)))
    (MULTIPLE-VALUE-BIND (FLAG THING)
        (BACKQUOTIFY (if vector-p (listarray object) object))
      (COND ((EQ FLAG **BACKQUOTE-/,/@-FLAG**)
             (READ-ERROR "/",@/" right after a /"`/": `,@~S." THING))
            ((EQ FLAG **BACKQUOTE-/,/.-FLAG**)
             (READ-ERROR "/",./" right after a /"`/": `,.~S." THING))
            (vector-p
             `(xr-bq-vector* ,(backquotify-1 flag thing)))
            (T
             (BACKQUOTIFY-1 FLAG THING))))))

;>> this one is really bogus (not in CL)
(DEFUN XR-#/`-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET ((**BACKQUOTE-REPEAT-VARIABLE-LISTS** (CONS NIL **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
    (MULTIPLE-VALUE-BIND (FLAG THING)
        (BACKQUOTIFY (INTERNAL-READ STREAM T NIL T))
      (COND ((EQ FLAG **BACKQUOTE-/,/@-FLAG**)
             (READ-ERROR "/",@/" right after a /"#`/": `,@~S." THING))
            ((EQ FLAG **BACKQUOTE-/,/.-FLAG**)
             (READ-ERROR "/",./" right after a /"#`/": `,.~S." THING))
            (T
             (CONS 'PROGN
                   (NREVERSE
                     (EVAL `(LET (ACCUM)
                              (DO ,(CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**)
                                  ((NULL ,(CAAAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
                                   ACCUM)
                                (PUSH ,(BACKQUOTIFY-1 FLAG THING) ACCUM)))))))))))

(DEFUN XR-COMMA-MACRO (STREAM IGNORE)
  (UNLESS **BACKQUOTE-REPEAT-VARIABLE-LISTS**
    (READ-ERROR "Comma not inside a backquote."))
  (LET ((C (NTH-VALUE 2 (XR-XRTYI STREAM NIL T))))
    (OR (= C #/@) (= C #/.)
        (SEND STREAM :UNTYI C))
    (LET ((COMMA-ARG (LET ((**BACKQUOTE-REPEAT-VARIABLE-LISTS**
                             (CDR **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
                       (INTERNAL-READ STREAM T NIL T))))
      (UNLESS (OR (NULL **BACKQUOTE-REPEAT-VARIABLE-LISTS**)
                  (EQ (CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**) T))
        (IF (EQ (CAR COMMA-ARG) **BACKQUOTE-/,-FLAG**)
            (SETQ COMMA-ARG (LIST 'QUOTE COMMA-ARG))
          (LET ((VAR (GENSYM)))
            (PUSH (LIST-IN-AREA READ-AREA VAR (LIST 'QUOTE COMMA-ARG) (LIST 'CDR VAR))
                  (CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
            (SETQ COMMA-ARG (LIST 'CAR VAR)))))
      (COND ((= C #/@)
             (CONS-IN-AREA **BACKQUOTE-/,/@-FLAG** COMMA-ARG READ-AREA))
            ((= C #/.)
             (CONS-IN-AREA **BACKQUOTE-/,/.-FLAG** COMMA-ARG READ-AREA))
            (T (CONS-IN-AREA **BACKQUOTE-/,-FLAG** COMMA-ARG READ-AREA))))))

(DEFUN BACKQUOTIFY (CODE)
  (PROG (AFLAG A DFLAG D)
        (COND ((ATOM CODE)
               (COND ((NULL CODE)
                      (RETURN (VALUES NIL NIL)))
                     ((OR (NUMBERP CODE)
                          (EQ CODE T))
                      (RETURN (VALUES T CODE)))
                     (T
                      (RETURN (VALUES 'QUOTE CODE)))))
              ((EQ (CAR CODE) **BACKQUOTE-/,-FLAG**)
               (SETQ CODE (CDR CODE))
               (GO COMMA))
              ((EQ (CAR CODE) **BACKQUOTE-/,/@-FLAG**)
               (RETURN (VALUES **BACKQUOTE-/,/@-FLAG** (CDR CODE))))
              ((EQ (CAR CODE) **BACKQUOTE-/,/.-FLAG**)
               (RETURN (VALUES **BACKQUOTE-/,/.-FLAG** (CDR CODE)))))
        (MULTIPLE-VALUE-SETQ (AFLAG A) (BACKQUOTIFY (CAR CODE)))
        (MULTIPLE-VALUE-SETQ (DFLAG D) (BACKQUOTIFY (CDR CODE)))
        (AND (EQ DFLAG **BACKQUOTE-/,/@-FLAG**)
             (READ-ERROR " /",@/" after a /"./": .,@~S in ~S." D CODE))
        (AND (EQ DFLAG **BACKQUOTE-/,/.-FLAG**)
             (READ-ERROR " /",./" after a /"./": .,.~S in ~S." D CODE))
        (COND ((EQ AFLAG **BACKQUOTE-/,/@-FLAG**)
               (COND ((NULL DFLAG)
                      (SETQ CODE A)
                      (GO COMMA)))
               (RETURN (VALUES 'APPEND
                               (IF (EQ DFLAG 'APPEND)
                                   (CONS-IN-AREA A D READ-AREA)
                                   (LIST-IN-AREA READ-AREA A (BACKQUOTIFY-1 DFLAG D))))))
              ((EQ AFLAG **BACKQUOTE-/,/.-FLAG**)
               (WHEN (NULL DFLAG)
                 (SETQ CODE A)
                 (GO COMMA))
               (RETURN (VALUES 'NCONC
                               (IF (EQ DFLAG 'NCONC)
                                   (CONS-IN-AREA A D READ-AREA)
                                   (LIST-IN-AREA READ-AREA A (BACKQUOTIFY-1 DFLAG D))))))
              ((NULL DFLAG)
               (IF (MEMQ AFLAG '(QUOTE T NIL))
                   (RETURN (VALUES 'QUOTE (LIST A)))
                   (RETURN (VALUES 'LIST (LIST-IN-AREA READ-AREA (BACKQUOTIFY-1 AFLAG A))))))
              ((MEMQ DFLAG '(QUOTE T))
               (IF (MEMQ AFLAG '(QUOTE T NIL))
                   (RETURN (VALUES 'QUOTE
                                   (CONS-IN-AREA A D READ-AREA)))
                   (RETURN (VALUES 'LIST*
                                   (LIST-IN-AREA READ-AREA
                                                 (BACKQUOTIFY-1 AFLAG A)
                                                 (BACKQUOTIFY-1 DFLAG D)))))))
        (SETQ A (BACKQUOTIFY-1 AFLAG A))
        (IF (MEMQ DFLAG '(LIST LIST*))
            (RETURN (VALUES DFLAG (CONS-IN-AREA A D READ-AREA))))
        (RETURN (VALUES 'LIST* (LIST-IN-AREA READ-AREA A (BACKQUOTIFY-1 DFLAG D))))
     COMMA
        (COND ((ATOM CODE)
               (COND ((NULL CODE)
                      (RETURN (VALUES NIL NIL)))
                     ((OR (NUMBERP CODE)
                          (EQ CODE 'T))
                      (RETURN (VALUES T CODE)))
                     (T
                      (RETURN (VALUES **BACKQUOTE-/,-FLAG** CODE)))))
              ((EQ (CAR CODE) 'QUOTE)
               (RETURN (VALUES 'QUOTE (CADR CODE))))
              ((MEMQ (CAR CODE) '(APPEND LIST LIST* NCONC))
               (RETURN (VALUES (CAR CODE) (CDR CODE))))
              ((EQ (CAR CODE) 'CONS)
               (RETURN (VALUES 'LIST* (CDR CODE))))
              (T
                (RETURN (VALUES **BACKQUOTE-/,-FLAG** CODE))))))

(DEFUN BACKQUOTIFY-1 (FLAG THING)
  (COND ((OR (EQ FLAG **BACKQUOTE-/,-FLAG**)
             (MEMQ FLAG '(T NIL)))
         THING)
        ((EQ FLAG 'QUOTE)
         (LIST-IN-AREA READ-AREA 'QUOTE THING))
        (T
         (CONS-IN-AREA (CDR (ASSQ FLAG '((CONS . XR-BQ-CONS)
                                         (LIST . XR-BQ-LIST)
                                         (LIST* . XR-BQ-LIST*)
                                         (APPEND . XR-BQ-APPEND)
                                         (NCONC . XR-BQ-NCONC)
                                         (VECTOR . XR-BQ-VECTOR))))
                       THING
                       READ-AREA))))

;;;; # submacros.

(DEFUN XR-#B-MACRO (STREAM IGNORE &OPTIONAL arg)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#B/"" arg)))
  (LET ((*READ-BASE* 2.))
    (VALUES (INTERNAL-READ STREAM T NIL T))))

(DEFUN XR-#O-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#O/"" arg)))
  (LET ((*READ-BASE* 8.))
    (VALUES (INTERNAL-READ STREAM T NIL T))))

(DEFUN XR-#X-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#X/"" arg)))
  (LET ((*READ-BASE* 16.))
    (VALUES (INTERNAL-READ STREAM T NIL T))))

(DEFUN XR-#R-MACRO (STREAM IGNORE &OPTIONAL RADIX)
  (cond (*read-suppress*)
        ((null radix)
         (read-error "/"#R/" was read with no digits after the /"#/"")
         (setq radix 10.))
        ((typep radix '(integer 2 36.)))
        (t
         (read-error "~D is an invalid radix in /"#~DR/"" radix radix)
         (setq radix 10.)))
  (LET ((*READ-BASE* RADIX))
    (VALUES (INTERNAL-READ STREAM T NIL T))))

(DEFUN XR-#/'-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#'/"" arg)))
  (LIST-IN-AREA READ-AREA 'FUNCTION (INTERNAL-READ STREAM T NIL T)))

(DEFVAR FILE-IN-COLD-LOAD NIL
  "T while evaluating text from a file which is in the cold load.
FILE-ATTRIBUTE-BINDINGS makes a binding for this from the Cold-load attribute.")

(defun xr-#/,-macro (stream ignore &optional arg)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#,/"" arg))
        (file-in-cold-load (read-error "/"#,/" cannot be used in files in the cold load.")))
  (let ((frob (internal-read stream t nil t)))
    (cond (*read-suppress*
           nil)
          ((and (boundp 'compiler::qc-file-read-in-progress)
                compiler::qc-file-read-in-progress)
           (cons-in-area compiler::eval-at-load-time-marker frob read-area))
          (t (eval frob)))))

(defun xr-#/:-macro (stream ignore arg)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#:/"" arg)))
  (let ((read-intern-function 'read-uninterned-symbol))
    (values (internal-read stream t nil t))))

(defun xr-#/(-macro (stream ignore &optional length)
  (let ((elements (read-delimited-list #/) stream t)))
    (if *read-suppress*
        nil
      (cond ((and length (plusp length) (null elements))
             (read-error "The construct #~D() is illegal; at least one element must be given."
                         length)
             (setq length nil))
            ((and length (< length (length elements)))
             (cerror :no-action nil 'read-error-1
                     "Length of elements list supplied, ~D, is more than specified length in #~D(...)"
                     (length elements) length)
             (setq length nil)))
      (if (null length)
          (apply #'vector elements)
        (let ((vector (make-array (or length (length elements))
                                  :initial-element (car (last elements)))))
          (replace vector elements)
          vector)))))

(defun xr-#*-macro (stream ignore &optional length)
  (if *read-suppress*
      (progn (internal-read stream t nil t) nil)
    (do ((bit-vector (make-array (or length 32.) :element-type 'bit :fill-pointer 0))
         elt)
        (())
      (multiple-value-bind (char index actual) (xr-xrtyi stream nil t)
        (case char
          ((#/0 #/1)
           (setq elt (if (= char #/0) 0 1))
           (cond ((null length)
                  (vector-push-extend elt bit-vector))
                 ((vector-push elt bit-vector))
                 (t (read-error
                      "Number of data bits exceeds specified length in /"#*/" bit vector construct.")
                    (vector-push-extend elt bit-vector)
                    (setq length nil))))
          (t
           (xr-xruntyi stream actual index)
           (if (and length (plusp length) (zerop (fill-pointer bit-vector)))
               (read-error "The construct #~D* is illegal; at least one bit must be given."
                           length))
           (if length
               ;; VECTOR-PUSH returns NIL when the fill pointer is at the end of the array.
               (do () ((vector-push elt bit-vector))))
           (return (values (make-array (length bit-vector) :element-type 'bit :initial-contents bit-vector)))))))))

;>> *** CONS ***
;>> ack! this need MUCH better error-reporting when the dimensionalities of the read-in
;>> sequences aren't appropriate or sonsistent.
(defun xr-#a-macro (stream ignore &optional rank)
  (if *read-suppress*
      (progn (internal-read stream t nil t) nil)
    (typecase rank
      (null
       (read-error "An array-rank must be given as an argument to /"#<rank>A/""))
      ((member 0)
       (values (make-array nil :initial-element (internal-read stream t nil t))))
      ((fixnum 0 #.array-rank-limit)
       (let (dimensions (sequences (internal-read stream t nil t)))
         (do ((dim 0 (1+ dim))
              (stuff sequences (elt stuff 0)))
             ((= dim rank))
           (push (length stuff) dimensions))
         (values (make-array (nreverse dimensions) :initial-contents sequences))))
      (t
       (read-error "~D is too large an array rank; it must be < ~D." rank array-rank-limit)
       (internal-read stream t nil t)
       nil))))

(defun xr-#c-macro (stream ignore &optional arg)
  (block nil
    (cond (*read-suppress*
           (internal-read stream t nil t)
           (return nil))
          (arg
           (read-error "An argument, ~D, was given to /"#C/"" arg)))
    (when ( (xr-xrtyi stream t) #/()
      (read-error "/"#C/" must be followed by a list.")
      (return 0))
    (let (n1 n2)
      (setq n1 (internal-read stream t nil t))
      (unless (typep n1 'non-complex-number)
        (read-error "~S cannot be the real component of a complex number" n1)
        (setq n1 0))
      (setq n2 (internal-read stream t nil t))
      (unless (typep n2 'non-complex-number)
        (read-error "~S cannot be the complex component of a complex number" n2)
        (setq n2 0))
      (if (not (null (read-delimited-list #/) stream t)))
          (read-error "/"#C/" must be followed by a list of exactly 2 numbers"))
      (complex n1 n2))))

;(DEFUN XR-#S-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
;  (IF *READ-SUPPRESS*
;      (PROGN (INTERNAL-READ STREAM T NIL T) NIL)
;    (LET* ((ARGS (INTERNAL-READ STREAM T NIL T))
;          (CONSTRUCTOR
;            (DOLIST (C (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
;                         (GET (CAR ARGS) 'DEFSTRUCT-DESCRIPTION)))
;              (IF (OR (NULL (CDR C))
;                      (AND (STRINGP (CADR C)) (NULL (CDDR C))))
;                  (RETURN (CAR C))))))
;      (IF CONSTRUCTOR
;         (EVAL (CONS CONSTRUCTOR
;                     (LOOP FOR (SLOT VALUE) ON (CDR ARGS) BY 'CDDR
;                           APPEND `(,(INTERN (SYMBOL-NAME SLOT) PKG-KEYWORD-PACKAGE)
;                                    ',VALUE))))
;       (CERROR ':NO-ACTION NIL 'READ-ERROR-1
;               "~S is not a structure type with a standard keyword constructor." (CAR ARGS))
;       NIL))))

;; Copied from LAD: RELEASE-3.IO; READ.LISP#454 on 2-Oct-86 05:32:03
(DEFUN XR-#S-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (COND (*READ-SUPPRESS*
         (INTERNAL-READ STREAM T NIL T)
         NIL)
        ('ELSE
         (LET ((FORM (INTERNAL-READ STREAM T NIL T))
               (DESCRIPTION)
               (CONSTRUCTORS)
               (CALLABLE))
           (COND ((NULL FORM)
                  (READ-ERROR "empty list after #S"))
                 ((NOT (SETQ DESCRIPTION (GET (CAR FORM) 'DEFSTRUCT-DESCRIPTION)))
                  (READ-ERROR "no defstruct description for ~S" (car form)))
                 ((NOT (SETQ CONSTRUCTORS (MAPCAR #'CAR (SUBSET #'(LAMBDA (CS)(NULL (CDR CS)))
                                                                (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS)))))
                  (READ-ERROR "~S has no keyword constructor" (car form)))
                 ((SETQ CALLABLE (SUBSET #'FUNCTIONP CONSTRUCTORS))
                  (VALUES (APPLY (CAR CALLABLE)
                                 (LOOP FOR (SLOT VALUE) ON (CDR FORM) BY 'CDDR
                                       APPEND (LIST (INTERN (SYMBOL-NAME SLOT) PKG-KEYWORD-PACKAGE)
                                                    VALUE)))))
                 ('ELSE
                  (VALUES (EVAL (CONS (CAR CONSTRUCTORS)
                                      (LOOP FOR (SLOT VALUE) ON (CDR FORM) BY 'CDDR
                                            APPEND (LIST SLOT `',VALUE)))))))))))

;(defun xr-#=-macro (stream ignore &optional label)
;  (cond (*read-suppress*
;        (values))
;       ((not label)
;        (read-error "No argument (label number) given to /"#=/"."))
;       (t
;        (with-stack-list (binding label 0 nil)
;          (with-stack-list* (*xr-label-bindings* binding *xr-label-bindings*)
;            (if (assq label (cdr *xr-label-bindings*))
;                (read-error "/"#=/" label ~D is already in use in this expression" label))
;            (let ((frob (internal-read stream t nil t)))
;              (setf (caddr binding) frob)
;              (fix-circularities frob binding nil)
;              (cond ((> (cadr binding) 0)
;                     (read-error "~Not all of the circular usages of the read label ~
;                                       /"#~D=/" were located and substituted!!"
;                                 label))
;                    ((< (cadr binding) 0)
;                     (ferror 'read-error-1 "Lossage in circularity reading!! (/"#~D=/" oversubstituted)"
;                             label)))
;              frob))))))

;(defun fix-circularities (frob binding previous &aux tem)
; (format *trace-output* "~%------ frobbing ~S ------~%" frob)
;  (cond ((eq frob binding)
; (format *trace-output* ">> fixed #~D# in ~S~%" (car binding) frob)
;        (decf (cadr binding))
;        (caddr binding))
;       ((memq frob previous)
;        frob)
;       ((consp frob)
;        (with-stack-list* (previous frob previous)
; (format *trace-output* "Car ")
;          (setq tem (fix-circularities (car frob) binding previous))
;          (with-stack-list* (previous (car frob) previous)
; (format *trace-output* "Cdr ")
;            (if (neq tem (car frob)) (setf (car frob) tem))
;            (setq tem (fix-circularities (cdr frob) binding previous))
;            (if (neq tem (cdr frob)) (setf (cdr frob) tem))))
;        frob)
;       ((arrayp frob)
;        (with-stack-list* (previous frob previous)
;          (when (setq tem (array-leader-length frob))
; (format *trace-output* "Lead ")
;            (dotimes (i tem)
;              (setf (array-leader frob i)
;                    (fix-circularities (array-leader frob i) binding previous))))
;          (if (zerop (array-rank frob))
;              (setf (aref frob) (fix-circularities (aref frob) binding previous))
;            (setq tem (array-total-size frob))
;            (dotimes (i tem)
; (format *trace-output* "Aref ")
;              (setf (cli:ar-1-force frob i)
;                    (fix-circularities (cli:ar-1-force frob i) binding previous))))))
;       ((instancep frob)
;        (with-stack-list* (previous frob previous)
;          (setq tem (1- (%structure-total-size frob)))
;          (dotimes (i tem)
; (format *trace-output* "%ref ")
;            (setf (si:%instance-ref frob (1+ i))
;                  (fix-circularities (si:%instance-ref frob (1+ i)) binding previous)))))
;       (t frob)))

;(defun xr-##-macro (stream ignore &optional label)
;  (declare (ignore stream))
;  (cond (*read-suppress* nil)
;       ((not label)
;        (read-error "No argument (label number) to ## given."))
;       (t (let ((binding (assq label *xr-label-bindings*)))
;            (if binding
;                (incf (cadr binding))
;              (read-error "The ##-label ~S is undefined." label))
;            binding))))

;;; The following two sharp-sign reader macros allow tagged LISP objects to be read in.
;;; #n=object reads object and assigns the n label to it.  #n# refers that object
;;; (in other words it is EQ to it) later or at a lower level of S-expression.
;;; The variable XR-LABEL-BINDINGS is an alist of a cons: a label (number) and
;;; a list of one element; that element is the LISP object to which the label refers.
;;; (It has to be a list so the binding can be a distinct object that you can RPLACA into.
;;; Also, it means that cdr[assq[tag;.xr-label-bindings.]] be () is if the tag is
;;; defined.

(DEFUN FIND-ANY-THINGS (THINGS TREE)
  (IF (NULL THINGS) ()
    (FIND-ANY-THINGS-1 THINGS TREE)))

(DEFUN FIND-ANY-THINGS-1 (THINGS TREE)
  (IF (NULL TREE) ()
    (IF (CONSP TREE)
        (OR (FIND-ANY-THINGS-1 THINGS (CAR TREE))
            (FIND-ANY-THINGS-1 THINGS (CDR TREE)))
      (MEMQ TREE THINGS))))                     ; TREE is an atom

;;>>>>>> **** THIS DOES NOT WORK!!!! ****
(DEFUN XR-#=-MACRO (STREAM IGNORE &OPTIONAL (LABEL XR-SHARP-ARGUMENT) &AUX THING)
  (COND
    (*READ-SUPPRESS*
     (VALUES))
    ((NOT LABEL)
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No argument (label number) to #= given."))
    ((ASSQ LABEL XR-LABEL-BINDINGS)
     ; The label is already defined, but we can't tell what it is yet.
     (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
             "Label ~S already defined in this expression." LABEL))
    (T ; Go for it and BIND
     (LET ((PLACEHOLDER (NCONS '**/#=placeholder**)))
       (PUSH (LIST LABEL PLACEHOLDER)           ; Allocate a slot
             XR-LABEL-BINDINGS)
       (LET ((LABEL-BINDING (ASSQ LABEL XR-LABEL-BINDINGS)))
         (IF (NULL LABEL-BINDING)
             (FERROR 'SYS:READ-ERROR-1 "Internal error in #= after reading in label's value.")
         ;; The preceding line should never happen.
         ;; ** old**  By writing into the slot will RPLACD, we also cause other places
         ;;           that referred to the label to get the value, too.
           (SETF (CAR (CDR LABEL-BINDING)) (SETQ THING (INTERNAL-READ STREAM T NIL T)))
         ;; If THING has no bindings in it, we can make the binding of THING itself instead of
         ;; a locative
         (IF (NOT (CONSP THING))
             ;; Replace locative with object
       ;     (SETF (CDR LABEL-BINDING) (CONTENTS (CDR LABEL-BINDING)))
           ;; If there are no bindings as locatives in it
           ;; Now we examine the list
           (NSUBST-EQ-SAFE THING PLACEHOLDER THING)     ; Substitute for `self'
       ;    (SETF (CDR LABEL-BINDING) (CONTENTS (CDR LABEL-BINDING)))
           ;; Catch the CDR - why does this happen ?
       ;   (LET ((LAST-CONS (NTHCDR (1- (LENGTH THING)) THING)))
       ;     (IF (LOCATIVEP (CDR LAST-CONS))
       ;         (SETF (CDR LAST-CONS) (CONTENTS (CDR LAST-CONS)))))
           )
         ; Replace locative with object
         THING))))))


(DEFUN XR-##-MACRO (STREAM IGNORE &OPTIONAL (LABEL XR-SHARP-ARGUMENT))
  (DECLARE (IGNORE STREAM))
  (COND (*READ-SUPPRESS* NIL)
        ((NOT LABEL)
         (READ-ERROR "No argument (label number) to ## given."))
        ((NULL (ASSQ LABEL XR-LABEL-BINDINGS))
         (READ-ERROR "The ##-label ~S is undefined." LABEL))
        (T
         (CADR (ASSQ LABEL XR-LABEL-BINDINGS)))))

(defun xr-#.-macro (stream ignore &optional arg)
  (cond (*read-suppress*)
        (arg (read-error "An argument, ~D, was given to /"#./"" arg)))
  (let ((frob (internal-read stream t nil t)))
    (values (if *read-suppress*
                nil
              (eval frob)))))

(DEFUN XR-#-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (LET ((CH (XR-XRTYI STREAM NIL T)))                   ;Skip the / that follows.
    (IF ( CH #//)
        (READ-ERROR "The character, /"~C/" following /"#/" was not /"///"" ch))
    (XR-CL-#\-MACRO STREAM NIL ARG)))

(defun xr-cl-#\-macro (stream ignore &optional font)
  (if (null font) (setq font 0))
  (if (< font char-font-limit)
      (%make-pointer dtp-character
                     (%logdpb font %%ch-font
                              (xr-#\-macro stream nil nil)))
    (read-error
      "The font number to /"#\/", ~D, is larger than the maximum character font value ~D"
      font (1- char-font-limit))
    #/null))

(DEFUN XR-#\-MACRO (STREAM IGNORE &OPTIONAL BITS)
  (MULTIPLE-VALUE-BIND (NIL NIL CHAR)
      (XR-XRTYI STREAM NIL T)
    (LOGIOR
      (%LOGDPB (OR BITS 0) %%KBD-CONTROL-META 0)
      (IF (NOT (OR ( #/A CHAR #/Z) ( #/a CHAR #/z)))
          CHAR
        (SEND STREAM :UNTYI CHAR)
        (PKG-BIND PKG-KEYWORD-PACKAGE
          (LET ((FROB (INTERNAL-READ STREAM T NIL T)))  ;Get symbolic name of character
            (IF *READ-SUPPRESS* 0                       ;READ returns NIL in this case; don't bomb.
              (IF (= (STRING-LENGTH FROB) 1)
                  XR-XRTYI-PREV-CHAR
                (OR (CDR (ASSQ FROB XR-SPECIAL-CHARACTER-NAMES))
                    (XR-PARSE-KEYBOARD-CHAR FROB)
                    (READ-ERROR
                      "#\~A is not a defined character-name." FROB))))))))))

(DEFMACRO XR-STR-CMP (STRING)
  `(AND (= LEN ,(STRING-LENGTH STRING))
        (%STRING-EQUAL ,STRING 0 STRING 1+PREV-HYPHEN-POS ,(STRING-LENGTH STRING))))

;;; This function is given a symbol whose print-name is expected to look
;;; like Control-Meta-A or Control-Meta-Abort or something.  It should return
;;; NIL if the print-name doesn't look like that, or the character code if
;;; it does.
(DEFUN XR-PARSE-KEYBOARD-CHAR (SYM)
  (WHEN (OR (SYMBOLP SYM) (STRINGP SYM))
    (LET ((STRING (IF (STRINGP SYM) SYM (GET-PNAME SYM)))
          TOP-FLAG GREEK-FLAG SHIFT-FLAG)
      (LOOP WITH CHAR = 0
            WITH END = (ARRAY-ACTIVE-LENGTH STRING)
            WITH TEM = NIL
            FOR START FIRST 0 THEN (1+ HYPHEN-POS)
            FOR 1+PREV-HYPHEN-POS = 0 THEN (1+ HYPHEN-POS)
            FOR HYPHEN-POS = (OR (STRING-SEARCH-CHAR #/- STRING START END) END)
            DO (LET ((LEN (- HYPHEN-POS 1+PREV-HYPHEN-POS)))
                 (COND ((OR (XR-STR-CMP "CTRL")
                            (XR-STR-CMP "CONTROL"))
                        (SETQ CHAR (DPB 1 %%KBD-CONTROL CHAR)))
                       ((XR-STR-CMP "META")
                        (SETQ CHAR (DPB 1 %%KBD-META CHAR)))
                       ((XR-STR-CMP "HYPER")
                        (SETQ CHAR (%LOGDPB 1 %%KBD-HYPER CHAR)))
                       ((XR-STR-CMP "SUPER")
                        (SETQ CHAR (DPB 1 %%KBD-SUPER CHAR)))
                       ((XR-STR-CMP "GREEK")
                        (SETQ GREEK-FLAG T))
                       ((XR-STR-CMP "FRONT")
                        (SETQ GREEK-FLAG T))
                       ((XR-STR-CMP "TOP")
                        (SETQ TOP-FLAG T))
                       ((OR (XR-STR-CMP "SHIFT")
                            (XR-STR-CMP "SH"))
                        (SETQ SHIFT-FLAG T))
                       ((= 1+PREV-HYPHEN-POS (1- END))
                        (RETURN (GREEKIFY-CHARACTER (AREF STRING 1+PREV-HYPHEN-POS)
                                                    GREEK-FLAG TOP-FLAG SHIFT-FLAG
                                                    CHAR)))
                       ((= 1+PREV-HYPHEN-POS (1- HYPHEN-POS))
                        (LET ((bucky (cdr (ASSQ (CHAR-UPCASE (CHAR-CODE (AREF STRING
                                                                       1+PREV-HYPHEN-POS)))
                                         `((#/C . ,%%KBD-CONTROL)
                                           (#/M . ,%%KBD-META)
                                           (#/H . ,%%KBD-HYPER)
                                           (#/S . ,%%KBD-SUPER))))))
                          (IF (null bucky)
                              (RETURN NIL)
                            (SETQ CHAR (%LOGDPB 1 bucky CHAR)))))
                       ;; See if we have a name of a special character "Return", "SP" etc.
                       ((SETQ TEM
                              (DOLIST (ELEM XR-SPECIAL-CHARACTER-NAMES)
                                (LET ((TARGET (GET-PNAME (CAR ELEM))))
                                  (IF (STRING-EQUAL TARGET STRING :START2 1+PREV-HYPHEN-POS)
                                      (RETURN (CDR ELEM))))))
                        ;; Note: combine with LOGIOR rather than DPB, since mouse
                        ;; characters have the high %%KBD-MOUSE bit on.
                        (RETURN (GREEKIFY-CHARACTER TEM GREEK-FLAG
                                                    TOP-FLAG SHIFT-FLAG
                                                    CHAR)))
                       (T (RETURN NIL))))))))

;;; Given a character, return the greek or top equivalent of it according to
;;; the specified flags.  If the flags are all NIL, the original character is returned.
(DEFUN GREEKIFY-CHARACTER (START-CHAR GREEK-FLAG TOP-FLAG SHIFT-FLAG
                           &OPTIONAL (METABITS 0))
  (COND ((AND TOP-FLAG GREEK-FLAG) NIL)
        (GREEK-FLAG
         (LET* ((GREEK-CHAR
                  (DOTIMES (I #o200)
                    (AND (OR (= START-CHAR (AREF KBD-NEW-TABLE 0 I))
                             (= START-CHAR (AREF KBD-NEW-TABLE 1 I)))
                         (IF SHIFT-FLAG
                             (RETURN (AREF KBD-NEW-TABLE 4 I))
                           (RETURN (AREF KBD-NEW-TABLE 3 I)))))))
           (AND GREEK-CHAR
                (NOT (BIT-TEST (LSH 1 15.) GREEK-CHAR))
                (LOGIOR METABITS GREEK-CHAR))))
        ((AND SHIFT-FLAG
              ( #/A (CHAR-CODE START-CHAR) #/Z))
         ;; Shift on a letter lowercasifies.
         (LOGIOR METABITS (CHAR-DOWNCASE START-CHAR)))
        ;; Otherwise SHIFT is only allowed with GREEK.
        (SHIFT-FLAG NIL)
        (TOP-FLAG
         (LET* ((TOP-CHAR
                  (DOTIMES (I #o200)
                    (AND (= START-CHAR (AREF KBD-NEW-TABLE 1 I))
                         (RETURN (AREF KBD-NEW-TABLE 2 I)))
                    (AND (= START-CHAR (AREF KBD-NEW-TABLE 0 I))
                         (RETURN (AREF KBD-NEW-TABLE 2 I))))))
           (AND TOP-CHAR
                (NOT (BIT-TEST (LSH 1 15.) TOP-CHAR))
                (LOGIOR METABITS TOP-CHAR))))
        (T
         (LOGIOR METABITS START-CHAR))))

(DEFUN XR-#^-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (MULTIPLE-VALUE-BIND (NIL NIL CH)
      (XR-XRTYI STREAM NIL T)
    (DPB 1 %%KBD-CONTROL (CHAR-UPCASE CH))))

(DEFUN XR-#Q-MACRO (STREAM IGNORE &OPTIONAL IGNORE)     ;For Lispm, gobble frob.
  (VALUES (INTERNAL-READ STREAM T NIL T)))

(DEFUN XR-#M-MACRO (STREAM IGNORE &OPTIONAL IGNORE)     ;For Maclisp.  Flush frob.
  (LET ((*READ-SUPPRESS* T))
    (INTERNAL-READ STREAM T NIL T))
  (VALUES))

(DEFUN XR-#N-MACRO (STREAM IGNORE &OPTIONAL IGNORE)     ;For NIL.  Flush frob.
  (LET ((*READ-SUPPRESS* T))
    (INTERNAL-READ STREAM T NIL T))
  (VALUES))

;;; #FOO ... represents an instance of flavor FOO.
;;; The flavor FOO should have a :READ-INSTANCE method, which is called
;;;  with SELF bound to nil, and arguments :READ-INSTANCE, the flavor name, and the stream.
;;; It should return the constructed instance
;;;  with the terminating  as the next character to be read.
;;; Alternatively, the symbol FOO should have a SI:READ-INSTANCE property.
;;; This property overrides the use of the flavor method.
;;; Using a property enables you to put it on any symbol you like,
;;; not necessarily the name of the (or any) flavor.  For example, you can
;;; put it in USER: this way, making it unnecessary to use a package prefix when you print.
(DEFUN XR-#-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (IF *READ-SUPPRESS*
      (PROGN (READ-DELIMITED-LIST #/ STREAM T) NIL)
    (IF ARG (READ-ERROR "An argument, ~D, was given to /"#/"" ARG))
    (LET* ((FLAVOR-NAME (INTERNAL-READ STREAM T NIL T))
;>> BLORP!!!!!!!!! ******************************
           (INSTANCE (LET ((HANDLER (OR (GET FLAVOR-NAME 'READ-INSTANCE)
                                        (GET-FLAVOR-HANDLER-FOR FLAVOR-NAME :READ-INSTANCE)))
                           (SELF NIL))
                       (FUNCALL HANDLER :READ-INSTANCE FLAVOR-NAME STREAM)))
           (CHAR (TYI STREAM)))
      ;; Make sure that the read-instance function read as much as it was supposed to.
      (IF (EQ CHAR #/)
          INSTANCE
        (SEND STREAM :UNTYI CHAR)
        (READ-ERROR
          "Malformatted #~S... encountered during by the reader." FLAVOR-NAME)
        INSTANCE))))

(DEFUN XR-#/|-MACRO (STREAM IGNORE &OPTIONAL ARG)
  (IF ARG (READ-ERROR "An argument, ~D, was given to /"#/|/"" ARG))
  (PROG ((N 0))
        (GO HOME)
     SHARP
        (CASE (SEND STREAM :TYI)
          (#/# (GO SHARP))
          (#/| (INCF N))
          (#// (SEND STREAM :TYI))
          ((NIL) (GO BARF)))
     HOME
        (CASE (SEND STREAM :TYI)
          (#/| (GO BAR))
          (#/# (GO SHARP))
          (#// (SEND STREAM :TYI)
               (GO HOME))
          ((NIL) (GO BARF))
          (T (GO HOME)))
     BAR
        (CASE (SEND STREAM :TYI)
          (#/# (IF (ZEROP N) (RETURN (VALUES))
                 (DECF N)
                 (GO HOME)))
          (#/| (GO BAR))
          (#// (SEND STREAM :TYI)
               (GO HOME))
          ((NIL) (GO BARF))
          (T (GO HOME)))
     BARF
        (READ-ERROR "The end of file was reached while reading a #/| comment.")))

;;;  Read-time conditionalization macros
;;;  <feature-form> ::= <symbol-or-number> | (NOT <feature-form>)
;;;                  | (AND . <feature-forms>) | (OR . <feature-forms>)

;;;  As an example, (AND MACSYMA (OR LISPM AMBER)) is a feature form
;;;  which represents the predicate
;;;  (AND (STATUS FEATURE MACSYMA) (OR (STATUS FEATURE LISPM) (STATUS FEATURE AMBER))).
;;;  The use of these forms in conjuction with the #+ reader macro
;;;  enables the read-time environment to conditionalize the
;;;  reading of forms in a file.

;;;  #+<FEATURE-FORM> <FORM> is read as <FORM> if <FEATURE-FORM> is true,
;;;  i.e. if the predicate associated with <FEATURE-FORM> is non-NIL when
;;;  evaluated in the read-time environment.
;;;  #+<FEATURE-FORM> <FORM> is read as whitespace if <FEATURE-FORM> is false.

(defvar *previous-package* nil)

(DEFUN XR-#+-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET* ((*previous-package* *package*)
         (FEATURE (LET ((*PACKAGE* PKG-KEYWORD-PACKAGE)
                        (*READ-BASE* 10.))
                   (INTERNAL-READ STREAM T NIL T))))    ;feature or feature list
    (COND (*READ-SUPPRESS*
           (VALUES))
          ((NOT (XR-FEATURE-PRESENT FEATURE))
           (LET ((*READ-SUPPRESS* T))
             (INTERNAL-READ STREAM T NIL T))
           (VALUES))
          (T
           (VALUES (INTERNAL-READ STREAM T NIL T))))))

;;;  #-<FEATURE-FORM> is equivalent to #+(NOT FEATURE-FORM).

(DEFUN XR-#--MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (LET* ((*previous-package* *package*)
         (FEATURE (LET ((*PACKAGE* PKG-KEYWORD-PACKAGE)
                        (*READ-BASE* 10.))
                   (INTERNAL-READ STREAM T NIL T))))    ;feature or feature list
    (COND (*READ-SUPPRESS*
           (VALUES))
          ((XR-FEATURE-PRESENT FEATURE)
           (LET ((*READ-SUPPRESS* T))
             (INTERNAL-READ STREAM T NIL T))
           (VALUES))
          (T
           (VALUES (INTERNAL-READ STREAM T NIL T))))))

;;;  Here, FEATURE is either a symbol to be looked up in (STATUS FEATURES) or
;;;  a list whose car is either AND, OR, or NOT.
;;;  Numbers may also be used--they are always taken to be decimal.
;;;  This is useful since people tend to name computers with numbers for some reason.

(DEFUN XR-FEATURE-PRESENT (FEATURE &optional (feature-list *features*))
  (COND ((SYMBOLP FEATURE)
         ;; recent common-lisp flamage claims that this should be memq, not member :test #'string=
         ;;  Personally, I can't think of a more poorly-designed advertised feature in the
         ;;  clisp manual than *features* and *modules*
         (MEMQ FEATURE feature-list))
        ((NUMBERP FEATURE)
         (MEMBER-EQL FEATURE feature-list))
        ((ATOM FEATURE)
         (READ-ERROR "Unknown form ~S in #+ or #- feature list." FEATURE))
        ((EQ (CAR FEATURE) :NOT)
         (NOT (XR-FEATURE-PRESENT (CADR FEATURE) feature-list)))
        ((EQ (CAR FEATURE) :AND)
         (loop for f in (cdr feature)
               always (XR-FEATURE-PRESENT f feature-list)))
        ((EQ (CAR FEATURE) :OR)
         (loop for f in (cdr feature)
               thereis (XR-FEATURE-PRESENT f feature-list)))
        ((eq (car feature) :PACKAGE-ROOT-NAME)  ;extension, 7/18/87, RG.
         (string-equal (si:package-root-name *previous-package*)
                       (cadr feature)))
        ((eq (car feature) :target)             ;cross compilation extension - 3aug88 smh
         (xr-feature-present (cadr feature) (or *target-features* *features*)))
        ((eq (car feature) :local)              ;cross compilation extension - 3aug88 smh
         (xr-feature-present (cadr feature) *features*))
        (T (READ-ERROR "Unknown form ~S in #+ or #- feature list." FEATURE))))

;;; A macro to bind the readtable, useful in patch files.

(DEFUN XR-#!-MACRO (STREAM IGNORE &OPTIONAL IGNORE)
  (let ((readtable-name (LET ((*PACKAGE* PKG-KEYWORD-PACKAGE)
                              (*READ-BASE* 10.))
                          (INTERNAL-READ STREAM T NIL T))))
    (let ((*readtable* (condition-case (errobj)
                           (find-readtable-named readtable-name)
                         (error
                          (let ((report (send errobj :report-string)))
                            (cond ((string-search "cannot find a readtable" report)
                                   (read-error "~A" report))
                                  ('else
                                   (read-error "Cant find readtable named ~S because: ~A"
                                               readtable-name
                                               report))))))))
      (INTERNAL-READ STREAM T NIL T))))


;;;; Common Lisp syntax-setting functions.

(DEFUN SET-SYNTAX-FROM-CHAR (TO-CHAR FROM-CHAR
                             &OPTIONAL (TO-READTABLE *READTABLE*)
                                       (FROM-READTABLE INITIAL-COMMON-LISP-READTABLE))
  "Copy the syntax of FROM-CHAR in FROM-READTABLE to TO-CHAR in TO-READTABLE, Common Lisp style.
Common Lisp has a peculiar definition of just what it is about
the character syntax that can be copied, while other aspects
are supposed to be immutable for a particular character.
This function contains hair to copy only the mutable part of the syntax.
To copy all aspects of the syntax, use COPY-SYNTAX."
; Character lossage.
  (IF (CHARACTERP TO-CHAR) (SETQ TO-CHAR (CHAR-INT TO-CHAR)))
  (IF (CHARACTERP FROM-CHAR) (SETQ FROM-CHAR (CHAR-INT FROM-CHAR)))
  (LET ((BITS (RDTBL-BITS FROM-READTABLE FROM-CHAR))
        (CODE (RDTBL-CODE FROM-READTABLE FROM-CHAR))
        (FROM-AENTRY (ASSQ FROM-CHAR (RDTBL-MACRO-ALIST FROM-READTABLE)))
        (TO-AENTRY (ASSQ TO-CHAR (RDTBL-MACRO-ALIST TO-READTABLE)))
        SYNTAX-STANDARD)
    ;; Find a character which standardly posesses the syntax that is being copied.
    (DOTIMES (I (ARRAY-DIMENSION INITIAL-COMMON-LISP-READTABLE 1))
      (WHEN (AND (= (RDTBL-BITS INITIAL-COMMON-LISP-READTABLE I) BITS)
                 (= (RDTBL-CODE INITIAL-COMMON-LISP-READTABLE I) CODE))
        (SETQ SYNTAX-STANDARD I)))
    ;; Check for Space, Return, etc. set to Constituent syntax.
    (WHEN (AND (NULL SYNTAX-STANDARD)
               (EQUAL (GET-SYNTAX-BITS FROM-CHAR INITIAL-COMMON-LISP-READTABLE)
                      (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE) 'ILLEGAL)))
      (SETQ SYNTAX-STANDARD #/A))
    (COND ((OR (NULL SYNTAX-STANDARD) (EQ SYNTAX-STANDARD #/#))
           ;; Standardize any kind of nonterminating macro.
           ;; NIL for SYNTAX-STANDARD must mean that, since that is the only kind of syntax
           ;; that can get used due to Common Lisp actions
           ;; which isn't present in the standard Common Lisp readtable.
           (SETQ SYNTAX-STANDARD (IF (= TO-CHAR #/#) #/# NIL)))
          ;; Standardize any kind of constituent character to an ordinary one.
          ((OR (ALPHA-CHAR-P SYNTAX-STANDARD)
               (MEMQ SYNTAX-STANDARD '(#/+ #/- #/. #// #/: #/^ #/_ #/!)))
           (IF (OR (ALPHA-CHAR-P TO-CHAR)
                   (MEMQ TO-CHAR '(#/+ #/- #/. #// #/: #/^ #/_)))
               (SETQ SYNTAX-STANDARD TO-CHAR)
             (IF (MEMQ TO-CHAR '(#/SPACE #/RETURN #/LINE #/TAB #/RUBOUT #/BS #/PAGE))
                 (SETQ SYNTAX-STANDARD 'ILLEGAL)
               (SETQ SYNTAX-STANDARD #/!)))))
    (IF (NULL SYNTAX-STANDARD)
        (SET-SYNTAX-BITS TO-CHAR (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE)
                                      'NON-TERMINATING-MACRO)
                         TO-READTABLE)
      (IF (EQ SYNTAX-STANDARD 'ILLEGAL)
          (SET-SYNTAX-BITS TO-CHAR
                           (GETF (RDTBL-PLIST INITIAL-COMMON-LISP-READTABLE) 'ILLEGAL)
                           TO-READTABLE)
        (SET-SYNTAX-BITS TO-CHAR
                         (GET-SYNTAX-BITS SYNTAX-STANDARD INITIAL-COMMON-LISP-READTABLE)
                         TO-READTABLE)))
    (SETF (RDTBL-MACRO-ALIST TO-READTABLE)
          (DELQ TO-AENTRY (RDTBL-MACRO-ALIST TO-READTABLE)))
    (WHEN (NOT (NULL FROM-AENTRY))
      (PUSH (LIST TO-CHAR (CADR FROM-AENTRY)) (RDTBL-MACRO-ALIST TO-READTABLE)))))

(DEFUN SET-MACRO-CHARACTER (CHAR FUNCTION &OPTIONAL NON-TERMINATING-P
                                                    (A-READTABLE *READTABLE*))
  "Set the syntax of CHAR in A-READTABLE to be a macro character that invokes FUNCTION.
NON-TERMINATING-P non-NIL means the macro char is recognized only
 at the start of a token; it can appear unquoted in the middle of a symbol.
FUNCTION is passed the input stream and the character that invoked it.
It should return zero or more values, each of which is an object produced
by the macro.  Zero and one are the most common numbers of values.
More than one may not be accepted in certain contexts.

The function can examine the list of input so far at the current level
 through the special variable XR-LIST-SO-FAR.  It can also be
 :TOPLEVEL if not within list, or :AFTER-DOT if after a dot.

A function can also hairily mung the list so far.
To do this, modify XR-LIST-SO-FAR and return the modified list as
 the only value, after setting the special variable XR-SPLICE-P to T."
  ;;;Check character code, bucky bits, etc.
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (cond ((not (fixnump char))
         (ferror "Can't set syntax for ~s (which cannot be coerced into a character)" char))
        ((not (zerop (char-bits char)))
         (ferror "Can't set syntax for a character (~@c) with modifier bits" (int-char char)))
        ((not (zerop (char-font char)))
         (ferror "Can't set syntax for a character (~@c) with font bits" (int-char char))))
  ;;;Now we have a good character code.
  (LET ((SYNTAX (GETF (RDTBL-PLIST A-READTABLE)
                      (IF NON-TERMINATING-P 'NON-TERMINATING-MACRO 'MACRO))))
    (UNLESS (AND (CONSP SYNTAX)
                 (FIXNUMP (CAR SYNTAX))
                 (FIXNUMP (CDR SYNTAX)))
      (FERROR "No saved syntax found for defining macro characters."))
    (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
    (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX))
    (LET ((X (ASSQ CHAR (RDTBL-MACRO-ALIST A-READTABLE))))
      (IF (NULL X)
          (SETF (RDTBL-MACRO-ALIST A-READTABLE)
                (CONS (LIST CHAR FUNCTION NON-TERMINATING-P) (RDTBL-MACRO-ALIST A-READTABLE)))
        (SETF (CDR X) (LIST FUNCTION NON-TERMINATING-P)))))
  T)

(DEFUN GET-MACRO-CHARACTER (CHAR &OPTIONAL (A-READTABLE *READTABLE*))
  "Return the function called for macro character CHAR in readtable A-READTABLE.
Value is NIL if CHAR is not a macro character.
Second value is non-NIL if CHAR does not terminate symbols."
  (DECLARE (VALUES FUNCTION NON-TERMINATING-P))
;character lossage
  (IF (CHARACTERP CHAR) (SETQ CHAR (CHAR-INT CHAR)))
  (LET* ((AENTRY (ASSQ CHAR (RDTBL-MACRO-ALIST A-READTABLE))))
    (VALUES (CADR AENTRY)
            (CADDR AENTRY))))

(DEFUN MAKE-DISPATCH-MACRO-CHARACTER (CHAR &OPTIONAL NON-TERMINATING-P
                                                     (A-READTABLE *READTABLE*))
  "Make CHAR be a dispatch macro character in readtable A-READTABLE.
NON-TERMINATING-P non-NIL means CHAR should not terminate symbols."
  (SET-MACRO-CHARACTER CHAR 'XR-DISPATCH-MACRO-DRIVER NON-TERMINATING-P A-READTABLE)
  T)

(DEFUN SET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR FUNCTION
                                     &OPTIONAL (A-READTABLE *READTABLE*))
  "Set the function run by SUB-CHAR when it is seen following DISP-CHAR.
DISP-CHAR must be a dispatch macro character, such as made by MAKE-DISPATCH-MACRO-CHARACTER.
FUNCTION is the function to be run.  It will receive three arguments:
 the input stream, the sub-character dispatched on, and the numeric argument
 that followed the dispatch character (or NIL if no numeric argument)."
;character lossage
  (IF (CHARACTERP DISP-CHAR) (SETQ DISP-CHAR (CHAR-INT DISP-CHAR)))
  (IF (CHARACTERP SUB-CHAR) (SETQ SUB-CHAR (CHAR-INT SUB-CHAR)))
  (LET* ((AENTRY (ASSQ DISP-CHAR (RDTBL-MACRO-ALIST A-READTABLE)))
         (SUBAENTRY (ASSQ (CHAR-UPCASE SUB-CHAR) (CDDDR AENTRY))))
    (UNLESS AENTRY
      (FERROR "~@C is not a macro character." DISP-CHAR))
    (UNLESS (EQ (CADR AENTRY) 'XR-DISPATCH-MACRO-DRIVER)
      (FERROR "~@C is not a dispatch macro character." DISP-CHAR))
    (IF SUBAENTRY (SETF (CADR SUBAENTRY) FUNCTION)
      (PUSH (LIST (CHAR-UPCASE SUB-CHAR) FUNCTION) (CDDDR AENTRY)))))

(DEFUN GET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR
                                     &OPTIONAL (A-READTABLE *READTABLE*))
  "Return the function run by SUB-CHAR when encountered after DISP-CHAR.
DISP-CHAR must be a dispatch macro character, such as made by MAKE-DISPATCH-MACRO-CHARACTER.
Value is NIL if SUB-CHAR is not defined (as a subcase of DISP-CHAR)."
;character lossage
  (IF (CHARACTERP DISP-CHAR) (SETQ DISP-CHAR (CHAR-INT DISP-CHAR)))
  (IF (CHARACTERP SUB-CHAR) (SETQ SUB-CHAR (CHAR-INT SUB-CHAR)))
  (LET* ((AENTRY (ASSQ DISP-CHAR (RDTBL-MACRO-ALIST A-READTABLE)))
         (SUBAENTRY (ASSQ (CHAR-UPCASE SUB-CHAR) (CDDDR AENTRY))))
    (UNLESS AENTRY
      (FERROR "~@C is not a macro character." DISP-CHAR))
    (UNLESS (EQ (CADR AENTRY) 'XR-DISPATCH-MACRO-DRIVER)
      (FERROR "~@C is not a dispatch macro character." DISP-CHAR))
    (AND SUBAENTRY
         (NOT ( #/0 SUB-CHAR #/9))
         (CADR SUBAENTRY))))

;;; SETSYNTAX etc.
;;; Note: INITIAL-READTABLE is set up by LISP-REINITIALIZE to be the initial
;;; readtable (not a copy, should it be?).

(DEFUN COPY-SYNTAX (TO-CHAR FROM-CHAR
                    &OPTIONAL (TO-READTABLE *READTABLE*) (FROM-READTABLE *READTABLE*))
  "Copy the syntax of FROM-CHAR in FROM-READTABLE to TO-CHAR in TO-READTABLE.
All aspects of the syntax are copied, including macro definition if any.
However, the meaning as a subdispatch of any dispatch macro characters
is not affected; that is recorded separately under those dispatch macros."
; character lossage
  (IF (CHARACTERP TO-CHAR) (SETQ TO-CHAR (CHAR-INT TO-CHAR)))
  (IF (CHARACTERP FROM-CHAR) (SETQ FROM-CHAR (CHAR-INT FROM-CHAR)))
  (LET ((FROM-AENTRY (ASSQ FROM-CHAR (RDTBL-MACRO-ALIST FROM-READTABLE)))
        (TO-AENTRY (ASSQ TO-CHAR (RDTBL-MACRO-ALIST TO-READTABLE))))
    (SET-SYNTAX-BITS TO-CHAR
                     (GET-SYNTAX-BITS FROM-CHAR FROM-READTABLE)
                     TO-READTABLE)
    (SETF (RDTBL-MACRO-ALIST TO-READTABLE)
          (DELQ TO-AENTRY (RDTBL-MACRO-ALIST TO-READTABLE)))
    (IF FROM-AENTRY
        (PUSH (COPY-TREE FROM-AENTRY) (RDTBL-MACRO-ALIST TO-READTABLE))))
  T)

(DEFUN SET-CHARACTER-TRANSLATION (CHAR VALUE &OPTIONAL (A-READTABLE *READTABLE*))
  "Set CHAR when read with A-READTABLE to translate in to character VALUE."
  (SETF (RDTBL-TRANS A-READTABLE CHAR) VALUE))

(DEFUN SET-SYNTAX-MACRO-CHAR (CHAR FUNCTION &OPTIONAL (A-READTABLE *READTABLE*))
  "Set the syntax of CHAR in A-READTABLE to be a macro character that invokes FUNCTION.
This expects FUNCTION to obey the old conventions for macro character functions
and encapsulates it into a closure that will obey the new conventions.
You should actually convert to using SET-MACRO-CHARACTER with
a function that obeys the new conventions, which are usually easier anyway."
  (SET-MACRO-CHARACTER
    CHAR
    (LET-CLOSED ((MACRO-FUNCTION FUNCTION))
      (LAMBDA (STREAM IGNORE)
        (MULTIPLE-VALUE-BIND (THING NIL SPLICEP)
            (FUNCALL MACRO-FUNCTION XR-LIST-SO-FAR STREAM)
          (IF SPLICEP
              (SETQ XR-SPLICE-P T XR-LIST-SO-FAR THING)
            THING))))
    NIL A-READTABLE)
  CHAR)

(DEFUN SET-SYNTAX-#-MACRO-CHAR (CHAR FUNCTION &OPTIONAL (A-READTABLE *READTABLE*))
  "Set the syntax of # followed by CHAR, in A-READTABLE, to invoke FUNCTION.
This expects FUNCTION to obey the old conventions for macro character functions
and encapsulates it into a closure that will obey the new conventions.
You should actually convert to using SET-DISPATCH-MACRO-CHARACTER with
a function that obeys the new conventions, which are usually easier anyway."
  (SET-DISPATCH-MACRO-CHARACTER
    #/# CHAR
    (LET-CLOSED ((MACRO-FUNCTION FUNCTION))
      (LAMBDA (STREAM IGNORE XR-SHARP-ARGUMENT)
        (MULTIPLE-VALUE-BIND (THING NIL SPLICEP)
            (FUNCALL MACRO-FUNCTION XR-LIST-SO-FAR STREAM)
          (IF SPLICEP
              (SETQ XR-SPLICE-P T XR-LIST-SO-FAR THING)
            THING))))
    A-READTABLE)
  CHAR)

(DEFUN SET-SYNTAX-FROM-DESCRIPTION (CHAR DESCRIPTION &OPTIONAL (A-READTABLE *READTABLE*))
  "Set the syntax of CHAR in A-READTABLE according to DESCRIPTION.
DESCRIPTION is a keyword defined in the readtable, such as SINGLE, SLASH, WHITESPACE."
  (LET ((SYNTAX (GETF (RDTBL-PLIST A-READTABLE) DESCRIPTION)))
    (OR (AND (CONSP SYNTAX)
             (FIXNUMP (CAR SYNTAX))
             (FIXNUMP (CDR SYNTAX)))
        (FERROR "No syntax of description: ~A found." DESCRIPTION))
    (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
    (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX))))

;;; Retrieves the syntax of a character so you can save it or whatever.
(DEFUN GET-SYNTAX-BITS (CHAR &OPTIONAL (A-READTABLE *READTABLE*))
  (CONS (RDTBL-BITS A-READTABLE CHAR)
        (RDTBL-CODE A-READTABLE CHAR)))

(DEFUN SET-SYNTAX-BITS (CHAR SYNTAX &OPTIONAL (A-READTABLE *READTABLE*))
  (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
  (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX)))

;;; Returns a copy of the readtable
;;; or copys the readtable into another readtable (and returns that)
(DEFUN COPY-READTABLE (&OPTIONAL (A-READTABLE *READTABLE*) (ANOTHER-READTABLE NIL))
  "Copy a readtable into another readtable, or make a new readtable.
With two arguments, copies the first into the second.
Otherwise makes a new copy of the first readtable.
The first argument defaults to the current readtable.
If the first argument is explicitly supplied as NIL,
 a copy of the unmodified standard Common Lisp syntax is made.."
  (IF (NULL A-READTABLE) (SETQ A-READTABLE COMMON-LISP-READTABLE))
  (LET ((X (ARRAY-DIMENSION A-READTABLE 0))
        (Y (ARRAY-DIMENSION A-READTABLE 1))
        (L (ARRAY-LEADER-LENGTH A-READTABLE)))
    (LET ((NEW-READTABLE (OR ANOTHER-READTABLE
                             (MAKE-ARRAY (LIST X Y)
                                         :ELEMENT-TYPE '(MOD 65536.)
                                         :LEADER-LENGTH L))))
      (DOTIMES (I X)
        (DOTIMES (J Y)
          (SETF (AREF NEW-READTABLE I J) (AREF A-READTABLE I J))))
      (DOTIMES (I L)
        (SETF (ARRAY-LEADER NEW-READTABLE I) (ARRAY-LEADER A-READTABLE I)))
      ;; Certain elements of the leader should not be shared.
      (SETF (RDTBL-MACRO-ALIST NEW-READTABLE)
            (COPYTREE (RDTBL-MACRO-ALIST NEW-READTABLE)))
      (SETF (RDTBL-PLIST NEW-READTABLE)
            (COPYLIST (RDTBL-PLIST NEW-READTABLE)))
      (AND (NAMED-STRUCTURE-P A-READTABLE)
           (MAKE-ARRAY-INTO-NAMED-STRUCTURE NEW-READTABLE))
      (SETF (RDTBL-NAMES NEW-READTABLE) NIL)
;      (UNLESS ANOTHER-READTABLE                        ;old readtable presumably already there
;       (PUSH NEW-READTABLE *ALL-READTABLES*))
      NEW-READTABLE)))

(DEFUN FIND-READTABLE-NAMED (NAME &OPTIONAL CREATE-P &AUX (RDTBL *READTABLE*))
  "Find or possibly create a readtable named NAME
If there is a readtable which has a name STRING-EQUAL it, we return that readtable.
Otherwise, we may create such a readtable, depending on CREATE-P.
Possible values of CREATE-P:
 NIL or :ERROR means get an error,
 :FIND means return NIL,
 :ASK means ask whether to create a readtable named NAME which is a copy of the current
   readtable, and returns it if so.
 T means create the readtable (a copy of *READTABLE*) and return it."
  (COND ((READTABLEP NAME)
         NAME)
        ((NULL NAME) *READTABLE*)
        (T
         (SETQ NAME (STRING NAME))
         (UNLESS (DOLIST (R *ALL-READTABLES*)
                   (WHEN (SI:MEMBER-EQUALP NAME (RDTBL-NAMES R))
                     (RETURN (SETQ RDTBL R))))
           (ECASE CREATE-P
             (:FIND NIL)
             (:ASK
              (IF (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
                          "~&Readtable ~S not found. Create a copy of ~A and name it ~A? "
                          NAME *READTABLE* NAME)
                  (PROGN (SETQ RDTBL (COPY-READTABLE))
                         (SETF (RDTBL-NAMES RDTBL) (LIST NAME)))
                (CERROR "Proceed, once you have defined the readtable"
                        "Please define a readtable named /"~A/" and then continue." NAME)
                (FIND-READTABLE-NAMED NAME :ERROR)))
             ((T)
              (SETQ RDTBL (COPY-READTABLE RDTBL))
              (SETF (RDTBL-NAMES RDTBL) (LIST NAME)))
             ((NIL :ERROR)
              (MULTIPLE-CERROR 'SI:READTABLE-NOT-FOUND
                               (:READTABLE-NAME NAME)
                               ("Cannot find a readtable named ~S" NAME)
                ((FORMAT NIL "Create it, using a copy of ~A" *READTABLE*)
                 (SETQ RDTBL (COPY-READTABLE RDTBL))
                 (SETF (RDTBL-NAMES RDTBL) (LIST NAME)))
                ((FORMAT NIL "Use ~A" RDTBL))
                ("Try again to find the readtable"
                 (FIND-READTABLE-NAMED NAME CREATE-P))
                ("Supply the name of a different readtable to use"
                 (SETQ RDTBL (FIND-READTABLE-NAMED
                               (LET ((*QUERY-IO* *DEBUG-IO*))
                                 (PROMPT-AND-READ :STRING-TRIM
                                                  "Name of readtable to use instead: "))
                               :ERROR))))))
           (IF (RDTBL-NAMES RDTBL)
               (PUSHNEW RDTBL *ALL-READTABLES* :TEST 'EQ)))
         RDTBL)))

;;;; MacLisp compatible (sort of) setsyntax:

(DEFUN SETSYNTAX (CHAR MAGIC MORE-MAGIC)
  "A function of MacLisp compatibility.  Alters the syntax of CHAR in
the current readtable according to MAGIC and MORE-MAGIC.  CHAR can be a
anything accepted by the function CHARACTER.  MAGIC is usually a
keyword.  The following are some of the accepted keywords:

:MACRO    CHAR becomes a macro character.  MORE-MAGIC is the name of a
          function to be invoked when this character is read.  The
          function takes no arguments, may be TYI or READ from
          STANDARD-INPUT and returns an object which is taken as the
          result of the read.

:SPLICING Like :MACRO but the object returned by the macro function is a
          list that is NCONCed into the list being read.  If CHAR is
          read anywhere except inside a list, then it may return (),
          which means it is ignored, or (OBJ) which means that OBJ is
          read.

:SINGLE   CHAR becomes a self-delimiting single-character symbol.  If
          MORE-MAGIC is a fixnum, CHAr is translated to that character.

NIL       The syntax of CHAR is not changed, but if MORE-MAGIC is a
          fixnum, CHAR is translated to that character.

a symbol  The syntax of the character is changed t be the same as that
          of the character MAGIC in the standard initial readtable.
          MAGIC is converted to a character by taking the first charcter
          of its print name.  If MORE-MAGIC is a fixnum, CHAR is
          translated to that character."
  ;; Convert MacLisp character object (a symbol) to Lispm character object (a fixnum).
;character lossage
  (SETQ CHAR (CHAR-INT (COERCE CHAR 'CHARACTER)))
  (IF (CHARACTERP MAGIC) (SETQ MAGIC (CHAR-INT MAGIC)))
  (IF (CHARACTERP MORE-MAGIC) (SETQ MORE-MAGIC (CHAR-INT MORE-MAGIC)))
  ;; Keywords being used, so disable package feature.
  (IF (SYMBOLP MAGIC) (SETQ MAGIC (INTERN (SYMBOL-NAME MAGIC) PKG-KEYWORD-PACKAGE)))
  (COND ((EQ MAGIC ':MACRO)
         ;; MacLisp reader macros get invoked on zero arguments.
         (SET-SYNTAX-MACRO-CHAR CHAR
                                `(LAMBDA (*IGNORED* *STANDARD-INPUT*)
                                   (,MORE-MAGIC))))
        ((EQ MAGIC ':SPLICING)
         (LET ((SETSYNTAX-FUNCTION MORE-MAGIC))
           (DECLARE (SPECIAL SETSYNTAX-FUNCTION))
           (SET-SYNTAX-MACRO-CHAR CHAR
                                  (CLOSURE '(SETSYNTAX-FUNCTION)
                                           #'SETSYNTAX-1))))
        ((FIXNUMP MAGIC)
         (FERROR "You cannot give a fixnum syntax to ~S (~O)." MAGIC 'SETSYNTAX))
        (T
         (OR (NOT (FIXNUMP MORE-MAGIC))
             (SET-CHARACTER-TRANSLATION CHAR MORE-MAGIC))
         (COND ((EQ MAGIC ':SINGLE)
                (SET-SYNTAX-FROM-DESCRIPTION CHAR 'SINGLE))
               ((NULL MAGIC))
               (T
                (SET-SYNTAX-FROM-CHAR CHAR (CHARACTER MAGIC))))))
  T)

(DEFUN SETSYNTAX-1 (LIST-SO-FAR *STANDARD-INPUT* &AUX LST)
  (DECLARE (SPECIAL SETSYNTAX-FUNCTION))
  (SETQ LST (FUNCALL SETSYNTAX-FUNCTION))
  (COND ((NULL LST)
         (VALUES LIST-SO-FAR NIL T))
        ((MEMQ LIST-SO-FAR '(:TOPLEVEL :AFTER-DOT))
         (IF (AND (NOT (ATOM LST))
                  (NULL (CDR LST)))
             (VALUES (CAR LST) NIL NIL)
           (FERROR "A SPLICING macro defined with SETSYNTAX attempted to return ~S
in the context: ~S." LST LIST-SO-FAR)))
        (T
         (VALUES (NCONC LIST-SO-FAR LST) NIL T))))

;;;; MacLisp compatible (sort of) setsyntax-sharp-macro

(DEFUN SETSYNTAX-SHARP-MACRO (CHAR TYPE FUN &OPTIONAL (RDTBL *READTABLE*))
  "Exists only for MACLISP compatibility.  Use SET-SYNTAX-#-MACRO-CHAR
instead."
  (SETQ CHAR (CHAR-UPCASE (CHAR-INT (COERCE CHAR 'CHARACTER))))
  (AND (SYMBOLP TYPE) (SETQ TYPE (INTERN (SYMBOL-NAME TYPE) PKG-KEYWORD-PACKAGE)))
  (LET ((SETSYNTAX-SHARP-MACRO-FUNCTION FUN)
        (SETSYNTAX-SHARP-MACRO-CHARACTER
          (CASE TYPE
            ((:PEEK-MACRO :PEEK-SPLICING) CHAR)
            ((:MACRO :SPLICING) NIL))))
    (DECLARE (SPECIAL SETSYNTAX-SHARP-MACRO-FUNCTION SETSYNTAX-SHARP-MACRO-CHARACTER))
    (LET ((F (CASE TYPE
               ((:MACRO :PEEK-MACRO)
                (CLOSURE '(SETSYNTAX-SHARP-MACRO-FUNCTION
                            SETSYNTAX-SHARP-MACRO-CHARACTER)
                         #'SETSYNTAX-SHARP-MACRO-1))
               ((:SPLICING :PEEK-SPLICING)
                (CLOSURE '(SETSYNTAX-SHARP-MACRO-FUNCTION
                            SETSYNTAX-SHARP-MACRO-CHARACTER)
                         #'SETSYNTAX-SHARP-MACRO-2))
               (OTHERWISE
                (FERROR "~S never heard of the type ~S." 'SETSYNTAX-SHARP-MACRO TYPE)))))
      (SET-SYNTAX-#-MACRO-CHAR CHAR F RDTBL)))
  CHAR)

(DEFUN SETSYNTAX-SHARP-MACRO-1 (LIST-SO-FAR *STANDARD-INPUT*)
  (DECLARE (SPECIAL SETSYNTAX-SHARP-MACRO-FUNCTION SETSYNTAX-SHARP-MACRO-CHARACTER)
           (IGNORE LIST-SO-FAR))
  (OR (NULL SETSYNTAX-SHARP-MACRO-CHARACTER)
      (SEND *STANDARD-INPUT* :UNTYI SETSYNTAX-SHARP-MACRO-CHARACTER))
  (FUNCALL SETSYNTAX-SHARP-MACRO-FUNCTION XR-SHARP-ARGUMENT))

(DEFUN SETSYNTAX-SHARP-MACRO-2 (LIST-SO-FAR *STANDARD-INPUT* &AUX LST)
  (DECLARE (SPECIAL SETSYNTAX-SHARP-MACRO-FUNCTION SETSYNTAX-SHARP-MACRO-CHARACTER))
  (OR (NULL SETSYNTAX-SHARP-MACRO-CHARACTER)
      (SEND *STANDARD-INPUT* :UNTYI SETSYNTAX-SHARP-MACRO-CHARACTER))
  (SETQ LST (FUNCALL SETSYNTAX-SHARP-MACRO-FUNCTION XR-SHARP-ARGUMENT))
  (COND ((NULL LST)
         (VALUES LIST-SO-FAR NIL T))
        ((MEMQ LIST-SO-FAR '(:TOPLEVEL :AFTER-DOT))
         (IF (AND (NOT (ATOM LST))
                  (NULL (CDR LST)))
             (VALUES (CAR LST) NIL NIL)
           (FERROR "A SPLICING sharp macro defined with SETSYNTAX-SHARP-MACRO attempted
  to return ~S in the context: ~S." LST LIST-SO-FAR)))
        (T
         (VALUES (NCONC LIST-SO-FAR LST) NIL T))))

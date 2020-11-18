;;; -*- Mode:LISP; Package:SI; Cold-Load:T; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **


(DEFVAR *PRINT-LEVEL* NIL
  "If non-NIL, maximum depth for printing list structure.
Any structure nested more deeply that this amount
is replaced by /"#/".")
(DEFVAR PRINLEVEL :UNBOUND
  "If non-NIL, maximum depth for printing list structure.
Any structure nested more deeply that this amount
is replaced by /"#/".")
(FORWARD-VALUE-CELL 'PRINLEVEL '*PRINT-LEVEL*)

(DEFVAR *PRINT-LENGTH* NIL
  "If non-NIL, maximum length of list to print.
Any elements past that many are replaced by /".../".")
(DEFVAR PRINLENGTH :UNBOUND
  "If non-NIL, maximum length of list to print.
Any elements past that many are replaced by /".../".")
(FORWARD-VALUE-CELL 'PRINLENGTH '*PRINT-LENGTH*)

(DEFVAR *STANDARD-OUTPUT* :UNBOUND
  "Default output stream for PRINT and TYO and many other functions.
Normally it is a synonym-stream pointing at *TERMINAL-IO*.")
(DEFVAR STANDARD-OUTPUT :UNBOUND
  "Default output stream for PRINT and TYO and many other functions.
Normally it is a synonym-stream pointing at *TERMINAL-IO*.")
(FORWARD-VALUE-CELL 'STANDARD-OUTPUT '*STANDARD-OUTPUT*)

(DEFVAR-RESETTABLE *PRINT-BASE* 10. 10.
  "Radix for output of integers and rational numbers.")
(DEFVAR BASE :UNBOUND
  "Radix for output of numbers.")
(FORWARD-VALUE-CELL 'BASE '*PRINT-BASE*)

(DEFVAR-RESETTABLE *NOPOINT T T
  "Non-NIL means do not print a period after decimal fixnums.
DO NOT USE THIS VARIABLE!! It will go away! Use *PRINT-RADIX* instead.")

(DEFVAR-RESETTABLE *PRINT-RADIX* NIL NIL
  "Non-NIL means print a radix specifier when printing an integer.")

(DEFVAR-RESETTABLE *PRINT-ESCAPE* T T
  "Non-NIL means print readably (PRIN1).  NIL means print with no quoting chars (PRINC).")

(DEFVAR-RESETTABLE *PRINT-PRETTY* NIL NIL
  "Non-NIL means print objects with extra whitespace for clarity.")

(DEFVAR-RESETTABLE *PRINT-CIRCLE* NIL NIL
  "Non-NIL means try to represent circular structure with #n# and #n= labels when printing.")

(DEFVAR *PRINT-CASE* ':UPCASE
  "Controls case used for printing uppercase letters in symbol pnames.
Value is :UPCASE, :DOWNCASE or :CAPITALIZE.")

(DEFVAR-RESETTABLE *PRINT-GENSYM* T T
  "Non-NIL means print #: before a symbol which claims to be uninterned, such as gensyms.")

(DEFVAR *PRINT-ARRAY* NIL
  "Non-NIL means print arrays so they can be read back in.  NIL means use #< syntax.")

(DEFVAR-RESETTABLE PRINT-READABLY NIL NIL
  "Non-NIL means signal SYS:PRINT-NOT-READABLE if attempt is made to print
some object whose printed representation cannot be read back in.")

(DEFVAR PRIN1 NIL
  "If non-NIL, specifies a function to be used for printing the values returned by
read-eval-print loops. GRIND-TOP-LEVEL is a useful thing to SETQ this to.")

(defun current-print-base (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *print-base*)
    (cond ((and (symbolp val)
                (get val 'princ-function))
           (return val))
          ((and (fixnump val) ( 2 val 36.))
           (return val))
          (t
           (signal-proceed-case (() 'eh:bad-system-parameter
                                    :place '*print-base*
                                    :value val
                                    ;; Who cares about symbols with princ properties?  Not I!!
                                    :type-specifier '(integer 2 36.)
                                    :reset-value (setq *print-base* 10.))
             (:no-action))))))

(defun current-print-length (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *print-length*)
    (if (typep val '(or null (integer 0 *)))
        (return val)
      (signal-proceed-case (() 'eh:bad-system-parameter
                               :place '*print-length*
                               :value val
                               :type-specifier '(or null (integer 0 *))
                               :reset-value (setq *print-length* nil))
        (:no-action)))))

(defun current-print-level (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *print-level*)
    (if (typep val '(or null (integer 0 *)))
        (return val)
      (signal-proceed-case (() 'eh:bad-system-parameter
                               :place '*print-level*
                               :value val
                               :type-specifier '(or null (integer 0 *))
                               :reset-value (setq *print-level* nil))
        (:no-action)))))

(defun current-print-case (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *print-case*)
    (if (typep val '(member :upcase :downcase :capitalize #|:studly|#))
        (return val)
      (signal-proceed-case (() 'eh:bad-system-parameter
                               :place '*print-case*
                               :value val
                               :type-specifier '(member :upcase :downcase :capitalize #|:studly|#)
                               :reset-value (setq *print-case*  #|:studly|# :upcase))
        (:no-action)))))

(defun current-readtable (&aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *readtable*)
    (if (typep val 'readtable)
        (return val)
      (signal-proceed-case (() 'eh:bad-system-parameter
                               :place '*readtable*
                               :value val
                               :type-specifier 'readtable
                               :reset-value (setq *readtable* (copy-readtable nil)))
        (:no-action)))))

(defun current-package (&optional allow-nil-p &aux val)
  (declare (dbg:error-reporter))
  (loop
    (setq val *package*)
    (cond ((typep val 'package)
           (return val))
          ((and (null val)
                allow-nil-p)
           (return val))
          (t
           (signal-proceed-case (() 'eh:bad-system-parameter
                                    :place '*package*
                                    :value val
                                    :type-specifier
                                      (if allow-nil-p '(or null package) 'package)
                                    :reset-value (setq *package* pkg-user-package))
             (:no-action))))))


(DEFVAR-RESETTABLE *PRINT-HASH-TABLE* NIL NIL
  "Hash table that records objects printed when *PRINT-CIRCLE*, for detecting shared structure.
Key is the object printed, value is:
* NIL if key occurs only once.
* T if key occurs more than once, but no occurrences printed yet.
* A number - that is the label.")
(DEFVAR *PRINT-LABEL-NUMBER* :UNBOUND)
(DEFVAR REUSABLE-PRINT-HASH-TABLE NIL)
(DEFUN PRINT-CIRCLE (OBJECT FUNCTION &REST ARGS)
  (IF (COND ((NOT *PRINT-CIRCLE*))
            ((INSTANCEP OBJECT)
             ;;>> print-record-occurrences doesn't hack this case.  It should. (somehow..)
             T)
            ((SYMBOLP OBJECT)
             (SYMBOL-PACKAGE OBJECT))
            ((NOT (%POINTERP OBJECT))
             T)
            ((STRINGP OBJECT))
            ((NUMBERP OBJECT)
             ;; bigna, ratios and single-floats are ok.
             (NOT (COMPLEXP OBJECT))))
      (APPLY FUNCTION ARGS)
    (LET ((*PRINT-LABEL-NUMBER* 0)
          (*PRINT-HASH-TABLE* NIL))
      (UNWIND-PROTECT
          (PROGN
            (SETQ *PRINT-HASH-TABLE*
                  (OR (DO (OLD)
                          ((%STORE-CONDITIONAL (LOCF REUSABLE-PRINT-HASH-TABLE)
                                               (SETQ OLD REUSABLE-PRINT-HASH-TABLE)
                                               NIL)
                           OLD))
                      (MAKE-HASH-TABLE :TEST #'EQ :AREA FORMAT::FORMAT-AREA)))
            (PRINT-RECORD-OCCURRENCES OBJECT)
            (APPLY FUNCTION ARGS))
        (WHEN *PRINT-HASH-TABLE*
          ;; this is actually a moderately significant optimization in the case
          ;; where *print-hash-table* is really big
          ;; Actually, in the case where is is really big, we lose fairly badly
          ;;  in general, since the clear-hash becomes a significant overhead.
          (WHEN (PLUSP (SEND *PRINT-HASH-TABLE* :FILLED-ENTRIES))
            (SEND *PRINT-HASH-TABLE* :CLEAR-HASH))
          (SETQ REUSABLE-PRINT-HASH-TABLE *PRINT-HASH-TABLE*))))))

(defun print-record-occurrences (object)
  (flet ((seen (object)
           (send *print-hash-table* :modify-hash object (lambda (object value foundp)
                                                          (declare (ignore object value))
                                                          foundp))))
    (macrolet ((recursep (object)
                 ;; eliminate simple tail-recursive call if possible, since function-call is
                 ;;  so expensive on a cadr-style machine
                 `(if (symbolp ,object)
                      (null (symbol-package ,object))
                    (%pointerp ,object))))
      (when (and (recursep object)
                 (not (seen object)))
        (typecase object
          (list
           (do ((tail object (cdr tail))
                (first t nil))
               ((atom tail)
                (if (recursep tail) (print-record-occurrences tail)))
             (unless first
               (if (seen tail) (return nil)))
           (if (recursep (car tail)) (print-record-occurrences (car tail)))))
          ;; it is actually wrong not to hack this case, but there is no defined protocol
          ;;  for doing so.
          ;(instance)
          (array
           (cond ((numberp (cdr (assq (array-type object) array-bits-per-element)))
                  ;;>> this actually cheats somewhat in the complex and complex-float cases
                  t)
                 ((named-structure-p object)
                  ;; this is actually wrong, but there is no other defined protocol
                  (memq ':print-self (named-structure-invoke :which-operations object)))
                 ((and (simple-vector-p object)
                       (> (length object) (or *print-length*
                                              *print-simple-vector-length*
                                              -1))))
                 ((null *print-array*))
                 (t
                  (dotimes (i (array-length object))
                    (let ((x (condition-case ()
                                 (cli:ar-1-force object i)
                               ;;; $$$ If we're going to get a subscript error (from a
                               ;;; mal-formed array), don't do it here. <18-Nov-88 keith>
                               (eh:subscript-error (return-from print-record-occurrences nil)))))
                      (if (recursep x) (print-record-occurrences x)))))))
          (complex
           (if (%pointerp (%complex-real-part object))
               (seen (%complex-real-part object)))
           (if (%pointerp (%complex-imag-part object))
               (seen (%complex-real-part object)))))))))

;;; You will notice that there are no constant strings in the printer.
;;; All the strings come out of a part of the current readtable
;;; called the "printtable".  For example, the character to start a list
;;; comes from (PTTBL-OPEN-PAREN *READTABLE*).
;;; See the file RDDEFS for the definitions and the default contents of these slots.

;;;; Main entries.
;;; These are the external entrypoints which are in the usual documentation.
;;; They are compatible with Maclisp.

(DEFUN PRINT (OBJECT &OPTIONAL STREAM)
  "Print OBJECT on STREAM with quoting if needed, with a Return before and a Space after.
Returns OBJECT."
  (SETQ STREAM (DECODE-PRINT-ARG STREAM))
  (SEND STREAM :TYO (PTTBL-NEWLINE *READTABLE*))
  (LET ((*PRINT-ESCAPE* T))
    (PRINT-CIRCLE OBJECT #'PRINT-OBJECT OBJECT 0 STREAM))
  (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
  OBJECT)

(DEFUN PRIN1 (OBJECT &OPTIONAL STREAM)
  "Print OBJECT on STREAM with quoting if needed.  Returns OBJECT."
  (LET ((*PRINT-ESCAPE* T))
    (PRINT-CIRCLE OBJECT #'PRINT-OBJECT OBJECT 0 (DECODE-PRINT-ARG STREAM)))
  OBJECT)

;; loses utterly with circularity
(DEFUN PPRINT (OBJECT &OPTIONAL STREAM)
  "Print OBJECT on STREAM, with quoting and with extra whitespace to make it look pretty.
Returns zero values."
  (LET ((*PRINT-ESCAPE* T))
    (GRIND-TOP-LEVEL OBJECT NIL (DECODE-PRINT-ARG STREAM)))
  (VALUES))

(DEFUN WRITE (OBJECT &KEY &OPTIONAL (STREAM *STANDARD-OUTPUT*)
              ((:ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*)
              ((:RADIX *PRINT-RADIX*) *PRINT-RADIX*)
              ((:BASE *PRINT-BASE*) *PRINT-BASE*)
              ((:CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*)
              ((:PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*)
              ((:LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*)
              ((:LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*)
              ((:CASE *PRINT-CASE*) *PRINT-CASE*)
              ((:GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*)
              ((:ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*))
  "Print OBJECT on STREAM.  Keyword args control parameters affecting printing.
The argument ESCAPE specifies the value for the flag *PRINT-ESCAPE*, and so on.
For any flags not specified by keyword arguments, the current special binding is used."
  (PRINT-CIRCLE OBJECT #'PRINT-OBJECT OBJECT 0 (DECODE-PRINT-ARG STREAM))
  OBJECT)

(DEFUN PRIN1-THEN-SPACE (OBJECT &OPTIONAL STREAM)
  "Print OBJECT on STREAM with quoting if needed, followed by a Space character."
  (SETQ STREAM (DECODE-PRINT-ARG STREAM))
  (LET ((*PRINT-ESCAPE* T))
    (PRINT-CIRCLE OBJECT #'PRINT-OBJECT OBJECT 0 STREAM))
  (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
  OBJECT)

(DEFUN PRINC (OBJECT &OPTIONAL STREAM)
  "Print OBJECT with no quoting, on STREAM.
Strings and characters print just their contents with no delimiters or quoting.
Pathnames, editor buffers, host objects, and many other hairy things
 print as their names with no delimiters."
  (LET ((*PRINT-ESCAPE* NIL))
    (PRINT-CIRCLE OBJECT #'PRINT-OBJECT OBJECT 0 (DECODE-PRINT-ARG STREAM)))
  OBJECT)

(DEFUN WRITE-TO-STRING (&REST ARGS)
  "Like WRITE but conses up a string to contain the output from printing OBJECT."
  (DECLARE (ARGLIST OBJECT &KEY ESCAPE RADIX BASE CIRCLE PRETTY LEVEL LENGTH CASE
                                GENSYM ARRAY))
  (FORMAT:OUTPUT NIL (APPLY #'WRITE ARGS)))

(DEFUN PRIN1-TO-STRING (OBJECT)
  "Like PRIN1 but conses up a string to contain the output from printing OBJECT."
  (FORMAT:OUTPUT NIL (PRIN1 OBJECT)))

(DEFUN PRINC-TO-STRING (OBJECT)
  "Like PRINC but conses up a string to contain the output from printing OBJECT."
  (FORMAT:OUTPUT NIL (PRINC OBJECT)))

;;;; Subroutines

(DEFUN DEFAULT-ERROR-PRINTING-OBJECT-FUNCTION (OBJECT STREAM)
  (FORMAT STREAM "...error printing ")
  (PRINTING-RANDOM-OBJECT (OBJECT STREAM :TYPE))
  (FORMAT STREAM "..."))
; not documented, as I have no idea what the right thing to do is
(DEFVAR-RESETTABLE *ERROR-PRINTING-OBJECT-FUNCTION*
                   'DEFAULT-ERROR-PRINTING-OBJECT-FUNCTION
                   'DEFAULT-ERROR-PRINTING-OBJECT-FUNCTION)

;;; Main routine, to print any lisp object.

(DEFVAR-RESETTABLE *INHIBIT-STREAM-PRINT-OPERATION* NIL NIL)

;; Copied from LAD: RELEASE-3.IO; PRINT.LISP#218 on 26-Mar-87 16:13:56
(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH STREAM  &AUX (RDTBL (CURRENT-READTABLE)))
  (BLOCK PRINT-OBJECT
    (CATCH 'PRINT-OBJECT
      (RETURN-FROM PRINT-OBJECT
        (CONDITION-RESUME '((EH:DEBUGGER-CONDITION)
                            :ABORT-PRINTING T ("Give up trying to print this object.")
                            EH::CATCH-ERROR-RESTART-THROW PRINT-OBJECT)
          (COND ((and (NOT *INHIBIT-STREAM-PRINT-OPERATION*)
                      (OPERATION-HANDLED-P STREAM :PRINT))
                 (LET ((*INHIBIT-STREAM-PRINT-OPERATION* T))
                   (SEND STREAM :PRINT EXP I-PRINDEPTH *PRINT-ESCAPE*)))
                ((AND
                   ;; Testing for *print-circle* loses if the user sets *print-circle* to
                   ;;  t in the middle of printing.  Lusers aren't meant to frob
                   ;;  si::*print-hash-table* (or that's the theory)
                   *PRINT-HASH-TABLE*
                   (IF (SYMBOLP EXP)
                       (NULL (SYMBOL-PACKAGE EXP))
                     (%POINTERP EXP))
                   ;; This is a candidate for circular or shared structure printing.
                   ;; See what the hash table says about the object:
                   ;; NIL - occurs only once.
                   ;; T - occurs more than once, but no occurrences printed yet.
                   ;;  Allocate a label this time and print #label= as prefix.
                   ;; A number - that is the label.  Print only #label#.
                   (CATCH 'LABEL-PRINTED
                     (SEND *PRINT-HASH-TABLE* :MODIFY-HASH EXP
                           (LAMBDA (KEY VALUE KEY-FOUND-P)
                             (DECLARE (IGNORE KEY KEY-FOUND-P)
                                      (SYS:DOWNWARD-FUNCTION))
                             (COND ((NULL VALUE)
                                    ;; this object hasn't been seen
                                    NIL)
                                   ((EQ VALUE T)
                                    ;; this object has been seen but not printed
                                    (LET ((LABEL (INCF *PRINT-LABEL-NUMBER*)))
                                      (SEND STREAM :TYO #/#)
                                      (PRINT-RAW-FIXNUM LABEL 10. STREAM)
                                      (SEND STREAM :TYO #/=)
                                      LABEL))
                                   (T
                                    ;; this object has been seen and printed before
                                    (SEND STREAM :TYO #/#)
                                    (PRINT-RAW-FIXNUM VALUE 10. STREAM)
                                    (SEND STREAM :TYO #/#)
                                    (THROW 'LABEL-PRINTED T)))))
                     NIL)))
                (T
                 ;; should be just a single (send exp :print-self stream i-prindepth) .. sigh
                 (TYPECASE EXP
                   (INSTANCE (SEND EXP :PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*))
                   (FIXNUM (PRINT-FIXNUM EXP STREAM))
                   (SYMBOL (PRINT-SYMBOL EXP STREAM RDTBL))
                   (LIST
                    (IF (AND *PRINT-LEVEL* ( I-PRINDEPTH *PRINT-LEVEL*))
                        (SEND STREAM :STRING-OUT (PTTBL-PRINLEVEL rdtbl))
                      (IF *PRINT-PRETTY*
                          (progn (let ((*print-pretty* nil))
                                   (GRIND-TOP-LEVEL EXP NIL STREAM NIL 'DISPLACED NIL)))
                        (PRINT-LIST EXP I-PRINDEPTH STREAM))))
                   (ENTITY
                    (IF (FUNCALL EXP :OPERATION-HANDLED-P :PRINT-SELF)
                        (FUNCALL EXP :PRINT-SELF STREAM I-PRINDEPTH *PRINT-ESCAPE*)
                      (PRINT-TRULY-RANDOM-OBJECT EXP STREAM I-PRINDEPTH)))
                   (NAMED-STRUCTURE
                    (LET ((NSS (CONDITION-CASE () (NAMED-STRUCTURE-P EXP) (ERROR NIL))))
                      (COND ((AND (SYMBOLP NSS)
                                  (GET NSS 'NAMED-STRUCTURE-INVOKE)
                                  (MEMQ ':PRINT-SELF
                                        (NAMED-STRUCTURE-INVOKE EXP :WHICH-OPERATIONS)))
                             (NAMED-STRUCTURE-INVOKE :PRINT-SELF EXP
                                                     STREAM I-PRINDEPTH *PRINT-ESCAPE*))
                            (T                  ;Named structure that doesn't print itself
                             (PRINT-NAMED-STRUCTURE NSS EXP I-PRINDEPTH STREAM)))))
                   (ARRAY
                    (PRINT-ARRAY EXP STREAM I-PRINDEPTH))
                   (SHORT-FLOAT
                    (PRINT-FLONUM EXP STREAM T))
                   (SINGLE-FLOAT
                    (PRINT-FLONUM EXP STREAM NIL))
                   (BIGNUM
                    (PRINT-BIGNUM EXP STREAM))
                   (RATIONAL
                    (PRINT-RATIO EXP STREAM))
                   (COMPLEX
                    (PRINT-COMPLEX EXP STREAM))
                   (CHARACTER
                    (PRINT-CHARACTER EXP STREAM))
                   (CLOSURE (PRINT-CLOSURE EXP STREAM))
                   (T                           ;Some random type we don't know about
                    (PRINT-TRULY-RANDOM-OBJECT EXP STREAM I-PRINDEPTH))))))))
    ;;>> Would also be nice to pass in the condition instance
    (FUNCALL *ERROR-PRINTING-OBJECT-FUNCTION* EXP STREAM))
  EXP)

(DEFUN PRINT-TRULY-RANDOM-OBJECT (EXP STREAM I-PRINDEPTH)
  (PRINTING-RANDOM-OBJECT (EXP STREAM)
    (PRINT-SYMBOL (Q-DATA-TYPES (%DATA-TYPE EXP)) STREAM)
    (TYPECASE EXP
      ((OR COMPILED-FUNCTION STACK-GROUP)
       (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
       (PRINT-OBJECT (FUNCTION-NAME EXP) I-PRINDEPTH STREAM))
      (MICROCODE-FUNCTION
       (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
       (PRINT-OBJECT (FUNCTION-NAME EXP) I-PRINDEPTH STREAM)
       (let ((symbol-area-index (aref #'si:micro-code-entry-area (%pointer exp))))
         (when (not (fixnump symbol-area-index))
           (send stream :string-out " (current definition ")
           (print-object symbol-area-index
                         i-prindepth
                         stream)
           (send stream :string-out ")")))))))

(defun print-random-object-prefix (object stream &optional type space
                                   &aux (rdtbl (current-readtable)))
  (if print-readably
      (print-not-readable object)
    (send stream :string-out (car (pttbl-random rdtbl)))
    (when type
      (print-symbol (type-of object) stream rdtbl))
    (when space
      (send stream :tyo (pttbl-space rdtbl)))))
(defun print-random-object-suffix (object stream &optional no-pointer)
  (unless no-pointer
    (send stream :tyo (pttbl-space *readtable*))
    (print-raw-fixnum (%pointer object) 8. stream))
  (send stream :string-out (cdr (pttbl-random *readtable*))))

;;; Print a list, hacking *print-length* and *print-level*.
(DEFUN PRINT-LIST (EXP I-PRINDEPTH STREAM)
  (SEND STREAM :TYO (PTTBL-OPEN-PAREN *READTABLE*))
  (DO ((I-PRINLENGTH 1 (1+ I-PRINLENGTH))
       (FIRST T NIL))
      ((OR (ATOM EXP)
           (AND *PRINT-CIRCLE*
                (NOT FIRST)
                (SEND *PRINT-HASH-TABLE* :GET-HASH EXP)))
       (WHEN (NOT (NULL EXP))
         (SEND STREAM :STRING-OUT (PTTBL-CONS-DOT *READTABLE*))
         (PRINT-OBJECT EXP (1+ I-PRINDEPTH) STREAM))
       (SEND STREAM :TYO (PTTBL-CLOSE-PAREN *READTABLE*)))
    (OR FIRST (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*)))
    (PRINT-OBJECT (CAR EXP) (1+ I-PRINDEPTH) STREAM)
    (SETQ EXP (CDR EXP))
    (AND *PRINT-LENGTH* ( I-PRINLENGTH *PRINT-LENGTH*) ;One frob gets printed before test.
         (NOT (ATOM EXP))                       ;Don't do it uselessly
         (PROGN (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
                (SEND STREAM :STRING-OUT (PTTBL-PRINLENGTH *READTABLE*))
                (SEND STREAM :TYO (PTTBL-CLOSE-PAREN *READTABLE*))
                (RETURN NIL)))))

(defvar *print-simple-vector-length* 10.
  "If a simple array of element-type T is of length <= this variable,
then we print it out this many elements, even if *PRINT-ARRAY* is NIL.
However, specifying a smaller number for *PRINT-LENGTH* will override
this value. ")

(defvar *print-simple-bit-vector-length* 40.
  "If a simple array of element-type BIT is of length <= this variable,
then we print it out this many elements, even if *PRINT-ARRAY* is NIL.
However, specifying a smaller number for *PRINT-LENGTH* will override
this value. ")

(defun print-array (exp stream i-prindepth
                    &aux (rank (array-rank exp))
                         (size (array-length exp))
                         (type (array-type exp)))
  (cond ((and (stringp exp)
              ( (length exp) size))
         (print-quoted-string exp stream))
        (*print-array*
         (cond ((and (= rank 1)
                     (eq type 'art-1b))
                (print-bit-vector exp stream))
               (*print-pretty*
                (grind-top-level exp nil stream nil 'displaced nil))
               ((= rank 1)
                (print-vector exp i-prindepth stream))
               (t
                (print-multidimensional-array exp i-prindepth stream))))
        ((and (simple-bit-vector-p exp)
              ( size (or *print-length*
                          *print-simple-bit-vector-length*
                          -1)))
         (print-bit-vector exp stream))
        ((and (simple-vector-p exp)
                ;*print-simple-vector-length* gets bound to nil by print-vector,
                ; avoiding infinite loops where simple vectors contain pointers to each other.
              ( size (cond ((null *print-simple-vector-length*)
                             -1)
                            ((null *print-length*)
                             *print-simple-vector-length*)
                            (t (min *print-length* *print-simple-vector-length*)))))
         (print-vector exp i-prindepth stream))
        (t
         (printing-random-object (exp stream)
           (print-symbol type stream)
           (cond ((= rank 0)
                  (send stream :string-out " 0-dimensional"))
                 ((= rank 1)
                  (send stream :tyo #/space)
                  (print-raw-fixnum size 10. stream))
                 (t
                  (dotimes (i rank)
                    (if (eql i 0) (send stream :tyo #/space) (send stream :tyo #/x))
                    (print-raw-fixnum (array-dimension exp i) 10. stream))))
           (if (simple-array-p exp)
               (send stream :string-out " (simple)")
             (when (array-displaced-p exp)
               ;; if array-indirect-p then is displaced to another array,
               ;;  rather than to a random memory location.
               (send stream :string-out
                     (if (array-indirect-p exp) " (indirect)" " (displaced)")))
             (when (array-has-leader-p exp)
               (let ((length (array-leader-length exp)))
                 (if (and (eql rank 1)
                          (eql length 1)
                          (fixnump (fill-pointer exp)))
                     (if (eq (fill-pointer exp) (array-length exp))
                         (send stream :string-out " (fill-pointer)")
                       (send stream :string-out " fill-pointer ")
                       (print-raw-fixnum (fill-pointer exp) 10. stream))
                   (send stream :string-out " leader-length ")
                   (print-raw-fixnum length 10. stream)))))))))

(DEFUN PRINT-VECTOR (EXP I-PRINDEPTH STREAM)
  (let ((my-print-length (min-ignoring-nils *print-simple-vector-length*
                                            *print-length*))
        (*print-simple-vector-length* nil))  ;avoid recursive loops
    (SEND STREAM :STRING-OUT (CAR (PTTBL-VECTOR *READTABLE*)))
    (DO ((I-PRINLENGTH 0 (1+ I-PRINLENGTH))
         (LENGTH (LENGTH EXP))
         (FIRST T NIL))
        ((= I-PRINLENGTH LENGTH)
         (SEND STREAM :STRING-OUT (CDR (PTTBL-VECTOR *READTABLE*))))
      (OR FIRST (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*)))
      (PRINT-OBJECT (AREF EXP I-PRINLENGTH) (1+ I-PRINDEPTH) STREAM)
      (AND MY-PRINT-LENGTH ( I-PRINLENGTH MY-PRINT-LENGTH) ;One frob gets printed before test.
           (PROGN (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*))
                  (SEND STREAM :STRING-OUT (PTTBL-PRINLENGTH *READTABLE*))
                  (SEND STREAM :TYO (PTTBL-CLOSE-PAREN *READTABLE*))
                  (RETURN NIL))))))

(DEFUN PRINT-BIT-VECTOR (EXP STREAM)
  (SEND STREAM :STRING-OUT (CAR (PTTBL-BIT-VECTOR *READTABLE*)))
  (DOTIMES (I (LENGTH EXP))
    (SEND STREAM :TYO (IF (ZEROP (BIT EXP I)) #/0 #/1)))
  (SEND STREAM :STRING-OUT (CAR (LAST (PTTBL-BIT-VECTOR *READTABLE*)))))

(DEFUN PRINT-MULTIDIMENSIONAL-ARRAY (EXP I-PRINDEPTH STREAM)
  (DOLIST (ELT (PTTBL-ARRAY *READTABLE*))
    (COND ((STRINGP ELT) (SEND STREAM :STRING-OUT ELT))
          ((EQ ELT ':RANK)
           (PRINT-RAW-FIXNUM (ARRAY-RANK EXP) 10. STREAM))
          ((EQ ELT ':SEQUENCES)
           (PRINT-ARRAY-CONTENTS EXP 0 0 I-PRINDEPTH STREAM)))))

(DEFVAR ARRAY-CONTENTS-ARRAY NIL)
(DEFUN PRINT-ARRAY-CONTENTS (ARRAY DIMENSION INDEX-SO-FAR I-PRINDEPTH STREAM &AUX TEM)
  (IF (AND *PRINT-LEVEL* ( I-PRINDEPTH *PRINT-LEVEL*))
      (SEND STREAM :STRING-OUT (PTTBL-PRINLEVEL *READTABLE*))
    (IF (ZEROP (ARRAY-RANK ARRAY))
        (PRINT-OBJECT (AREF ARRAY) I-PRINDEPTH STREAM)
      (LET ((INDEX (* INDEX-SO-FAR (ARRAY-DIMENSION ARRAY DIMENSION)))
            (MODE (CAR (MEMQ (ARRAY-TYPE ARRAY) '(ART-1B ART-STRING ART-FAT-STRING))))
            (LENGTH (ARRAY-DIMENSION ARRAY DIMENSION))
            (RANK (ARRAY-RANK ARRAY)))
        (IF (AND MODE (= (1+ DIMENSION) RANK))
            (LET ((KLUDGE (IF (AND (%STORE-CONDITIONAL (LOCF ARRAY-CONTENTS-ARRAY)
                                                       (SETQ TEM ARRAY-CONTENTS-ARRAY)
                                                       NIL)
                                   TEM)
                              (SI:CHANGE-INDIRECT-ARRAY TEM MODE LENGTH ARRAY INDEX)
                            (MAKE-ARRAY LENGTH
                                        :TYPE MODE :area format::format-area
                                        :DISPLACED-TO ARRAY :DISPLACED-INDEX-OFFSET INDEX))))
              (IF (EQ MODE 'ART-1B)
                  (PRINT-BIT-VECTOR KLUDGE STREAM)
                  (PRINT-QUOTED-STRING KLUDGE STREAM))
              (SETQ ARRAY-CONTENTS-ARRAY KLUDGE))       ;:massachusetts
          (SEND STREAM :TYO (PTTBL-OPEN-PAREN *READTABLE*))
          (DOTIMES (I (ARRAY-DIMENSION ARRAY DIMENSION))
            (OR (ZEROP I) (SEND STREAM :TYO (PTTBL-SPACE *READTABLE*)))
            (COND ((AND *PRINT-LENGTH* (= I *PRINT-LENGTH*))
                   (SEND STREAM :STRING-OUT (PTTBL-PRINLENGTH *READTABLE*))
                   (RETURN NIL))
                  ((= (1+ DIMENSION) (ARRAY-RANK ARRAY))
                   (PRINT-OBJECT (AR-1-FORCE ARRAY (+ INDEX I))
                                 (1+ I-PRINDEPTH) STREAM))
                  ((AND *PRINT-LEVEL*
                        ( (1+ I-PRINDEPTH) *PRINT-LEVEL*))
                   (SEND STREAM :STRING-OUT (PTTBL-PRINLEVEL *READTABLE*)))
                  (T
                   (PRINT-ARRAY-CONTENTS ARRAY (1+ DIMENSION)
                                               (+ INDEX I)
                                               (1+ I-PRINDEPTH)
                                         STREAM))))
          (SEND STREAM :TYO (PTTBL-CLOSE-PAREN *READTABLE*)))))))

(DEFUN PRINT-NAMED-STRUCTURE (NSS EXP I-PRINDEPTH STREAM)
  (LET ((DESCRIPTION (GET NSS 'DEFSTRUCT-DESCRIPTION)))
    (IF (NOT DESCRIPTION)
        (PRINTING-RANDOM-OBJECT (EXP STREAM :TYPEP))
      (SEND STREAM :STRING-OUT "#S")            ;use printtable?
      (LET ((SLOT-ALIST (SI:DEFSTRUCT-DESCRIPTION-SLOT-ALIST))
            (L (LIST NSS)))
        (DOLIST (S SLOT-ALIST)
          (LET* ((KWD (INTERN (SYMBOL-NAME (CAR S)) PKG-KEYWORD-PACKAGE))
                 (FUN (SI:DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR S)))
                 (INIT (SI:DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE (CDR S)))
                 (VAL (EVAL `(,FUN ,EXP))))     ;watch out for macros!
            ;; CONS!
            (UNLESS (EQUAL VAL INIT)
              (PUSH KWD L) (PUSH VAL L))))
        (PRINT-LIST (NREVERSE L) I-PRINDEPTH STREAM)))))

(DEFUN PRINT-CHARACTER (CHAR STREAM &AUX (*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
  (IF (NOT *PRINT-ESCAPE*)
      (SEND STREAM :TYO (CHAR-INT CHAR))
    (SEND STREAM :STRING-OUT (CAR (PTTBL-CHARACTER *READTABLE*)))
    (IF (NOT (ZEROP (CHAR-FONT CHAR)))
        (PRIN1 (CHAR-FONT CHAR) STREAM))
    (SEND STREAM :STRING-OUT (CDR (PTTBL-CHARACTER *READTABLE*)))
    (LET ((BITS (CHAR-BITS CHAR))
          (CODE (CHAR-CODE CHAR)))
      (SEND STREAM :STRING-OUT
            (NTH BITS
                 '("" "c-" "m-" "c-m-"
                   "s-" "c-s-" "m-s-" "c-m-s-"
                   "h-" "c-h-" "m-h-" "c-m-h-"
                   "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")))
      (COND ((TV:CHAR-MOUSE-P CHAR)
             (SEND STREAM :STRING-OUT "Mouse-")
             (let* ((bits (LDB %%KBD-MOUSE-BUTTON char))
                    (chname (nth bits '("Left-" "Middle-" "Right-"))))
               (if chname
                   (SEND stream :STRING-OUT chname)
                 (progn
                   (send stream :string-out "Button:")
                   (prin1 bits stream)
                   (send stream :string-out "-"))))
             (PRIN1 (1+ (LDB %%KBD-MOUSE-N-CLICKS CHAR)) STREAM))
            (T
             (LET ((CHNAME (FORMAT:OCHAR-GET-CHARACTER-NAME CODE)))
               (IF CHNAME
                   (SEND STREAM :STRING-OUT CHNAME)
                 (AND ( BITS 0)
                      (CHARACTER-NEEDS-QUOTING-P CODE)
                      (SEND STREAM :TYO (PTTBL-SLASH *READTABLE*)))
                 (SEND STREAM :TYO CODE))))))))

(defun print-print-radix-prefix (base stream)
  (declare (unspecial base))
  (and (numberp base)
       (neq base 10.)
       (case base
         (2 (send stream :string-out "#b"))
         (8 (send stream :string-out "#o"))
         (16. (send stream :string-out "#x"))
         (t
          (send stream :tyo #/#)
          (print-raw-fixnum base 10. stream)
          (send stream :tyo #/r)))))

;;; Print a fixnum, possibly with negation, decimal point, etc.
(DEFUN PRINT-FIXNUM (X STREAM &AUX (BASE (CURRENT-PRINT-BASE)))
  (DECLARE (UNSPECIAL BASE))
  (WHEN *PRINT-RADIX*
    (print-print-radix-prefix base stream))
  (WHEN (MINUSP X)
    (SEND STREAM :TYO (PTTBL-MINUS-SIGN *READTABLE*))
    (SETQ X (- X)))
  (IF (NUMBERP BASE)
      (PRINT-RAW-FIXNUM X BASE STREAM)
    (FUNCALL (GET BASE 'PRINC-FUNCTION) (- X) STREAM))
  (WHEN (AND (OR *PRINT-RADIX* (NOT *NOPOINT))
             (EQ BASE 10.))
    (SEND STREAM :TYO (PTTBL-DECIMAL-POINT *READTABLE*)))
  X)

(defun print-raw-fixnum (num radix stream)
  (when (minusp num)
    (send stream :tyo (pttbl-minus-sign *readtable*))
    (setq num (- num)))
  (multiple-value-bind (n rem)
      (truncate num radix)
    (or (zerop n)
        (print-raw-fixnum n radix stream))
    (send stream :tyo (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" rem))))

;;; Printing flonums.
;;; Note how the same code works for small flonums without consing and
;;; for big flonums with a certain amount of flonum and bignum consing.
;;; This code probably loses accuracy for large exponents.  Needs investigation.

(DEFUN PRINT-FLONUM (X STREAM SMALL
                     &OPTIONAL MAX-DIGITS (FORCE-E-FORMAT NIL)
                     &AUX EXPT)
  (COND ((ZEROP X)
         (SEND STREAM :STRING-OUT "0.0")
         (IF (NEQ (NULL SMALL)
                  (NEQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT))
             (SEND STREAM :STRING-OUT
                   (IF SMALL "s0" "f0"))))
        (T (IF (PLUSP X)
               (SETQ X (- X))
             (SEND STREAM :TYO (PTTBL-MINUS-SIGN *READTABLE*)))
           ;; X is now negative
           (COND ((OR (> X -1.0s-3) ( X -1.0s7) FORCE-E-FORMAT)
                  ;; Must go to E format.
                  (MULTIPLE-VALUE (X EXPT) (SCALE-FLONUM X))
                  ;; X is now positive
                  (LET ((PLACE-MOVED (PRINT-FLONUM-INTERNAL X SMALL STREAM MAX-DIGITS
                                                            T)))
                    (WHEN PLACE-MOVED (INCF EXPT)))
                  (SEND STREAM :TYO
                               (IF (NEQ (NULL SMALL)
                                        (NEQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT))
                                   (IF SMALL #/s #/f)
                                   #/e))
                  (WHEN (MINUSP EXPT)
                    (SEND STREAM :TYO (PTTBL-MINUS-SIGN *READTABLE*))
                    (SETQ EXPT (- EXPT)))
                  (PRINT-RAW-FIXNUM EXPT 10. STREAM))
                 (T
                  ;; It is in range, don't use E-format.
                  (PRINT-FLONUM-INTERNAL (- X) SMALL STREAM MAX-DIGITS)
                  (IF (NEQ (NULL SMALL)
                           (NEQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT))
                      (SEND STREAM :STRING-OUT
                            (IF SMALL "s0" "f0"))))))))

;;; Scale a positive flonum so that it is  1.0 and < 10.0
;;; Returns two values, the new flonum and the exponent scaled by,
;;; which is positive if the number was large and negative if it was small.
;;; Tries to minimize loss of precision.  Can lose up to 3 bits of precision
;;; for humongous numbers, but usually loses at most 1 bit.
;;; This still needs more work; perhaps it should do the scaling
;;; in fixed-point double-precision rather than in floating point;
;;; the result is consistently too low when expt is large and negative.

;;; Note: X is -ve on entry, +ve on exit
(defun scale-flonum (x &aux (short (typep X 'short-float)) tem expt wastoobig)
  (setq expt (truncate (// (float-exponent x) (log 10s0 2s0))))
  (tagbody
      again
         (if (minusp expt)
             (setq tem (* x (aref powers-of-10f0-table (- expt))))
           (setq tem (// x (aref powers-of-10f0-table expt))))
         (cond ((and ( tem -10s0) (not wastoobig)) (incf expt) (go again))
               ((> tem -1s0)  (decf expt) (setq wastoobig t) (go again))
               (t (return-from scale-flonum (values (- (if short (float tem 0s0) tem))
                                                    expt))))))

;; note: this old calling sequence expected x to be +ve on entry
;(DEFUN SCALE-FLONUM (X &AUX ( EXPT 0) (SMALLP (SMALL-FLOATP X)))
;  (WHEN (OR (< X 1s-15) (> X 1s15))    ;Far off, first guess the exponent via logarithms
;    (SETQ EXPT (TRUNCATE (LOG X) (LOG 10.0s0)))
;    (SETQ X (IF (MINUSP EXPT) (* X (^ 10. (- EXPT))) (// X (^ 10. EXPT)))))
;  (DO ((DIV 10. (* 10. DIV))   ;Divide by powers of 10. to make it less than 10.
;       (Y X (// X DIV)))
;      ((< Y 10.)
;       (AND SMALLP (SETQ Y (SMALL-FLOAT Y)))
;       (SETQ X Y))
;    (INCF EXPT))
;  (DO ((MPY 10. (* 10. MPY))   ;Multiply by powers of 10. to make it not less than 1.
;       (Y X (* X MPY)))
;      (( Y 1)
;       (AND SMALLP (SETQ Y (SMALL-FLOAT Y)))
;       (RETURN Y EXPT))
;    (DECF EXPT)))

;;; Print the mantissa.
;;; X is a positive non-zero flonum, SMALL is T if it's a small-flonum.
;;; Although X's magnitude is constrained to be between 0.1 and 100000.
;;; when called from the normal printer, this code should work for any X
;;; for the benefit of FORMAT.
;;; Note that ASH is the same as LSH except that it works for bignums and
;;; arbitrary amounts of shifting.  It is implemented with multiply and divide.
;;; Documentation in AI:QUUX;RADIX >
;;; Except that the bletcherous E-FORMAT hair in there has been flushed.
;;; It served only to avoid scaling the flonum to between 1 and 10 when
;;; printing in E-format, which this code wasn't using anyway.
;;; The MAX-DIGITS argument allows rounding off to a smaller precision
;;; than the true value.  However, it will only do this after the decimal point.
(DEFUN PRINT-FLONUM-INTERNAL (X SMALL STREAM MAX-DIGITS &OPTIONAL MOVE-DECIMAL-POINT)
  (LET (PLACE-MOVED)
    (MULTIPLE-VALUE-BIND (BUFFER DECIMAL-PLACE)
        (FLONUM-TO-STRING X SMALL MAX-DIGITS NIL)
      (WHEN (AND MOVE-DECIMAL-POINT (= DECIMAL-PLACE 2))
        (SETF (CHAR BUFFER 2) (CHAR BUFFER 1))
        (SETF (CHAR BUFFER 1) #/.)
        (IF (EQL (CHAR BUFFER (1- (LENGTH BUFFER))) #/0)
            (DECF (FILL-POINTER BUFFER)))
        (SETQ PLACE-MOVED T))
      (SEND STREAM :STRING-OUT BUFFER)
      PLACE-MOVED)))

;;; FORMAT also calls this
(DEFUN FLONUM-TO-STRING (FLONUM IGNORE MAX-DIGITS FRACTION-DIGITS &OPTIONAL DROP-LEADING-ZERO)
  "Return a string containing the printed representation of FLONUM.
At most MAX-DIGITS are printed if MAX-DIGITS is non-NIL.
If FRACTION-DIGITS is non-NIL, exactly FRACTION-DIGITS
 digits are printed after the decimal point.  This overrides the effect of MAX-DIGITS.
The second value is the number of digits that preceded the decimal point.
DROP-LEADING-ZERO means print .123 rather than 0.123 ."
  (DECLARE (VALUES BUFFER INTEGER-DIGITS))
  (LET ((EXPONENT (FLONUM-EXPONENT FLONUM))
        (MANTISSA (FLONUM-MANTISSA FLONUM))
        (rdtbl (current-readtable))
        (BAS 10.)
        K M R Q U S DECIMAL-PLACE
        ;; BUFFER is needed when MAX-DIGITS is supplied because the rounding
        ;; can generate a carry that has to propagate back through the digits.
        (BUFFER (MAKE-STRING 100 :FILL-POINTER 0 :AREA format::format-area)))
    (OR MAX-DIGITS (SETQ MAX-DIGITS 1000.))     ;Cause no effect.
    ;; Get integer part
    (SETQ R (ASH MANTISSA EXPONENT))
    (SETQ Q R)
    (SETQ M (ASH 1 (1- EXPONENT)))      ;Actually 0 in most normal cases.
    ;; Instead of using a pdl, precompute S and K.
    ;; S gets the highest power of BAS <= R, and K is its logarithm.
    (SETQ S 1 K 0 U BAS)
    (DO () ((> U R))
      (SETQ S U U (* U BAS) K (1+ K)))
    (DO-FOREVER
      (SETF (VALUES U R) (TRUNCATE R S))
      (COND ((OR (< R M) (> R (- S M)))
             (VECTOR-PUSH (INT-CHAR (+ (CHAR-INT #/0)
                                       (IF ( (* 2 R) S)
                                           U
                                         (1+ U))))
                          BUFFER)
             (DECF MAX-DIGITS)
             ;; This is the LEFTFILL routine in the paper.
             (DOTIMES (I K)
               (VECTOR-PUSH #/0 BUFFER)
               (DECF MAX-DIGITS))
             (RETURN NIL)))
      ;; If number is < 1, and we want all digits as fraction digits,
      ;; optionally omit the usual single leading zero before the decimal point.
      (UNLESS (AND FRACTION-DIGITS
                   DROP-LEADING-ZERO
                   (ZEROP U)
                   (ZEROP (FILL-POINTER BUFFER))
                   (EQ MAX-DIGITS FRACTION-DIGITS))
        (VECTOR-PUSH (INT-CHAR (+ (CHAR-INT #/0) U)) BUFFER))
      (DECF MAX-DIGITS)
      (DECF K)
      (IF (MINUSP K) (RETURN NIL))
      (SETQ S (TRUNCATE S 10.)))
    (SETQ DECIMAL-PLACE (LENGTH BUFFER))
    (VECTOR-PUSH (PTTBL-DECIMAL-POINT rdtbl) BUFFER)
    (IF FRACTION-DIGITS (SETQ MAX-DIGITS FRACTION-DIGITS))
    (IF (OR (NULL MAX-DIGITS) (PLUSP MAX-DIGITS))
        (IF (MINUSP EXPONENT)
            ;; There is a fraction part.
            (LET ((Z (- EXPONENT)))
              ;; R/S is the fraction, M/S is the error tolerance
              ;; The multiplication by 2 causes initial M to be 1/2 LSB
              (SETQ R (* (IF ( Z 23.) (LDB Z MANTISSA) ;If fraction bits fit in a fixnum
                             (LOGAND MANTISSA (1- (ASH 1 Z))))
                         2)
                    S (ASH 2 Z)
                    M 1)
              (DO-FOREVER
                (SETQ R (* R BAS))
                (SETF (VALUES U R) (TRUNCATE R S))
                (SETQ M (* M BAS))
                (AND (OR (< R M) (> R (- S M)) (< MAX-DIGITS 2))
                     (RETURN NIL))
                (VECTOR-PUSH (INT-CHAR (+ U (CHAR-INT #/0))) BUFFER)
                (DECF MAX-DIGITS))
              (VECTOR-PUSH (SETQ Z (INT-CHAR (+ (IF ( (* 2 R) S) U (1+ U)) (CHAR-INT #/0)))) BUFFER)
              (COND ((> Z #/9)
                     ;; Oops, propagate carry backward (MAX-DIGITS case)
                     (DO ((I (- (FILL-POINTER BUFFER) 2) (1- I)))
                         ((MINUSP I))
                       (SETF (CHAR BUFFER (1+ I)) #/0)
                      SKIP-DECIMAL
                       (SETQ Z (ZL:AREF BUFFER I))
                       (COND ((= Z (PTTBL-DECIMAL-POINT rdtbl))
                              (SETQ I (1- I))
                              (GO SKIP-DECIMAL))
                             (( Z #/9)
                              (SETF (AREF BUFFER I) (1+ Z))
                              (RETURN NIL))
                             ((ZEROP I)
                              ;; Double oops, the carry has added a new digit
                              (LET ((LEN (- (FILL-POINTER BUFFER) 2)))
                                (AND (= (AREF BUFFER LEN) (PTTBL-DECIMAL-POINT rdtbl))
                                     ;;Must have some fraction part
                                     (VECTOR-PUSH #/0 BUFFER))
                                (DO ((I LEN (1- I)))
                                    (( I 0))
                                  (SETF (AREF BUFFER (1+ I)) (AREF BUFFER I)))
                                (INCF DECIMAL-PLACE))
                              (SETF (CHAR BUFFER 1) #/0)
                              (SETF (CHAR BUFFER 0) #/1)
                              (RETURN NIL))))
                     ;; Now truncate trailing zeros, except for one after the decimal point
                     (LOOP FOR I FROM (1- (LENGTH BUFFER))
                                 DOWNTO (+ DECIMAL-PLACE 2)
                           WHILE (EQL (CHAR BUFFER I) #/0)
                        DO (SETF (FILL-POINTER BUFFER) I)))))
          ;; There is no fraction part at all.
          (VECTOR-PUSH #/0 BUFFER)))
    ;; Now add trailing zeros if requested
    (IF (AND FRACTION-DIGITS (PLUSP FRACTION-DIGITS))
        (LOOP REPEAT (- (+ DECIMAL-PLACE FRACTION-DIGITS 1) (LENGTH BUFFER))
           DO (VECTOR-PUSH #/0 BUFFER)))
    (VALUES BUFFER DECIMAL-PLACE)))


(DEFUN PRINT-BIGNUM (BIGNUM STREAM &AUX (base (current-print-base)))
  (declare (unspecial base))
  (when *print-radix*
    (print-print-radix-prefix base stream))
  (WHEN (MINUSP BIGNUM)
    (SEND STREAM :TYO (PTTBL-MINUS-SIGN *READTABLE*))
    (setq bignum (minus bignum)))       ;old thing with bignum-to-array ignored sign.
  (IF (FIXNUMP BASE)
      (PRINT-RAW-BIGNUM BIGNUM BASE STREAM)
    (FUNCALL (GET BASE 'PRINC-FUNCTION) (- BIGNUM) STREAM))
  (WHEN (AND (OR *PRINT-RADIX* (NOT *NOPOINT))
             (EQ BASE 10.))
    (SEND STREAM :TYO (PTTBL-DECIMAL-POINT *READTABLE*)))
  BIGNUM)



;;;; Print the digits of a bignum
;;; This does a lot of arithmetic and conses a bunch
;;; but doesn't recurse too deep and is still about as
;;; fast as the old microcode version
(defun print-raw-bignum (num radix stream)
  (print-raw-bignum-1
    num
    ;; number of digits
    (ceiling (quotient (haulong num)   ;(* 32. (%structure-total-size num))
                       (log radix 2)))
    radix stream t))

;;; this leftp is a crock so a leading 0 doesn't get printed
;;; because the above formula for number of digits is a bit bogus
(defun print-raw-bignum-1 (num ndigits radix stream leftp)
  (if (= ndigits 1)
      (if (or (not (zerop num))
               (not leftp))
          (send stream :tyo (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" num)))
    (let ((new-n (truncate ndigits 2)))
      (multiple-value-bind (quo rem)
          (truncate num (^ radix new-n))
        (print-raw-bignum-1 quo (if (oddp ndigits) (1+ new-n) new-n)
                            radix stream leftp)
        (print-raw-bignum-1 rem new-n radix stream nil)))))

;(DEFUN PRINT-RAW-BIGNUM (NUM RADIX STREAM &AUX LENGTH MAX-RADIX DIGITS-PER-Q)
;  (SETQ DIGITS-PER-Q (FLOOR %%Q-POINTER (HAULONG RADIX))
;       MAX-RADIX (^ RADIX DIGITS-PER-Q)
;       NUM (SI:BIGNUM-TO-ARRAY NUM MAX-RADIX)
;       LENGTH (ARRAY-LENGTH NUM))
;  (DO ((INDEX (1- LENGTH) (1- INDEX))
;       (NDIGITS -1 DIGITS-PER-Q))
;      ((MINUSP INDEX))
;    (PRINT-BIGNUM-PIECE (AREF NUM INDEX) RADIX STREAM NDIGITS)))

;(DEFUN PRINT-BIGNUM-PIECE (PIECE RADIX STREAM NDIGITS)
;  (WHEN (OR (> NDIGITS 1) ( PIECE RADIX))
;    (PRINT-BIGNUM-PIECE (TRUNCATE PIECE RADIX) RADIX STREAM (1- NDIGITS)))
;  (SEND STREAM :TYO (AREF "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (CL:REM PIECE RADIX))))


(defun character-needs-quoting-p (char &optional (rdtbl (current-readtable)))
  "Returns T if CHAR needs to be quoted to be read in as a symbol using readtable RDTBL."
;character lossage
  (if (characterp char) (setq char (char-int char)))
  (let ((state (rdtbl-starting-state rdtbl))
        (fsm (rdtbl-fsm rdtbl))
        (code (rdtbl-code rdtbl char)))
    (if (or (not (numberp state))               ;FSM ran out OR
            (>= char RDTBL-ARRAY-SIZE)
            ( char (rdtbl-trans rdtbl char))   ;Translated char? then fsm loses
            (= code (rdtbl-escape-code rdtbl))
            (= code (rdtbl-multiple-escape-code rdtbl))
            (= code (rdtbl-character-code-escape-code rdtbl)))
        t
      (let ((b (current-print-base)))
        (setq state (aref fsm state (if (and (typep b '(integer 10. *))
                                             ( (char-int #/A) char (+ b (- (char-int #/A) 11.))))
                                        (cdr (getf (rdtbl-plist rdtbl) 'extended-digit))
                                      code))))
      (cond ((not (numberp state))
             (dolist (l (rdtbl-make-symbol rdtbl) t)
               (and (eq (car state) (car l))
                    (eq (cdr state) (cdr l))
                    (return nil))))
            ((not (numberp (setq state (aref fsm state (rdtbl-break-code rdtbl)))))
             (dolist (l (rdtbl-make-symbol-but-last rdtbl) t)
               (and (eq (car state) (car l))
                    (eq (cdr state) (cdr l))
                    (return nil))))
            (t t)))))

;(defvar-resettable *print-package* nil nil
;  "Controls the printing of symbol package-prefixes:
;* If NIL, then packages prefixes are printed only when a symbol is not accessible
;  from the current package, or is a keyword or has no home package.
;  This is the normal way in which the printer is supposed to operate.
;* If :ALWAYS, then the home package of the symbol is always printed for symbols
;  with home packages.
;* If :NEVER, then package prefixes are never printed, even for homeless symbols
;  and keywords.
;* If a package object, then print as if the value of *PACKAGE* were that package
;  (in which case this is much the same as the relationship between
;   *PRINT-BASE* and *READ-BASE*)
;Note that if *PRINT-PACKAGE* is not NIL, rather surprising (and in the last two cases,
;downright incorrect) symbol-printing behaviour will be observed.  Use with caution!")

(defun print-symbol (symbol stream &optional (rdtbl (current-readtable)))
  (let ((cp (current-package t))
        (sym-pkg (symbol-package symbol))
        (sym-name (symbol-name symbol))
        tem)
    (when *print-escape*
      (flet ((pp (pkg internal &aux tem)
               (setq internal (if (memq internal '(:external :inherited))
                                  (pttbl-package-prefix rdtbl)
                                  (pttbl-package-internal-prefix rdtbl)))
               (block pp
                 (when (setq tem (pkg-prefix-print-name pkg))
                   (setq tem (assoc-equal tem (si:pkg-refname-alist cp)))
                   (when (or (null tem)
                             (eq (cdr tem) pkg))
                     (print-symbol-name (pkg-prefix-print-name pkg) stream rdtbl)
                     (send stream :string-out internal)
                     (return-from pp)))
                 (setq tem (assoc-equal (pkg-name pkg) (si:pkg-refname-alist cp)))
                 (when (or (null tem)
                           (eq (cdr tem) pkg))
                   (print-symbol-name (pkg-name pkg) stream rdtbl)
                   (send stream :string-out internal)
                   (return-from pp))
                 ;; what a horrible piece of design that there is not pkg-name-and-nicknames
                 (dolist (n (pkg-nicknames pkg))
                   (setq tem (assoc-equal n (si:pkg-refname-alist cp)))
                   (when (or (null tem)
                             (eq (cdr tem) pkg))
                     (print-symbol-name n stream rdtbl)
                     (send stream :string-out internal)
                     (return-from pp)))
                 (print-symbol-name (pkg-name pkg) stream rdtbl)
                 (send stream :string-out "#:"))
               nil)
             (pg ()
                (and *print-gensym*
                     (send stream :string-out (pttbl-uninterned-symbol-prefix rdtbl)))))
        (cond ((null sym-pkg)
               (if (or (null cp)
                       (multiple-value-bind (found foundp)
                           (find-symbol sym-name cp)
                         (or (not foundp)
                             (not (eq found symbol)))))
                   (pg)))
              ((eq sym-pkg pkg-keyword-package)
               (send stream :tyo #/:))
              ;; people bind *package* nil expecting to have symbol home package printed
              ((null cp)
               (multiple-value-bind (nil foundp xp)
                   (find-symbol sym-name sym-pkg)
                 (if (null xp)
                     (pg)
                   (print-symbol-name (or (pkg-prefix-print-name xp) (pkg-name xp))
                                      stream rdtbl)
                   (send stream :string-out (if (memq foundp '(:external :inherited))
                                                (pttbl-package-prefix rdtbl)
                                              (pttbl-package-internal-prefix rdtbl))))))
              (t
               (multiple-value-bind (found foundp xp)
                   (find-symbol sym-name cp)
                 (cond ((not foundp)
                        (multiple-value-setq (nil foundp)
                          (find-symbol sym-name sym-pkg))
                        (pp sym-pkg foundp))
                       ((eq found symbol)
                        (when (assq symbol (rdtbl-symbol-substitutions rdtbl))
                          (pp xp foundp)))
                       (t
                        (unless (and (setq tem (assq found (rdtbl-symbol-substitutions rdtbl)))
                                     (eq (cdr tem) symbol))
                          (multiple-value-setq (nil foundp)
                            (find-symbol sym-name sym-pkg))
                          (pp sym-pkg foundp)))))))))
    (print-symbol-name sym-name stream rdtbl)))

(defun print-symbol-name (string stream &optional (rdtbl (current-readtable))
                                        &aux (length (length string))
                                             (print-case (current-print-case))
                                             (base (current-print-base)))
  (declare (unspecial base))
  ;(check-type string string)
  (IF (NOT *PRINT-ESCAPE*)
      (CASE print-case
        (:DOWNCASE
         ;; speed kills
         (DOTIMES (I LENGTH)
           (SEND STREAM :TYO (CHAR-DOWNCASE (CHAR STRING I)))))
        (:CAPITALIZE
         ;; More Drugz!
         (DO ((LENGTH LENGTH) CHAR PREV-LETTER
              (I 0 (1+ I)))
             ((= I LENGTH))
           (SETQ CHAR (CHAR STRING I))
           (COND ((UPPER-CASE-P CHAR)
                  (SEND STREAM :TYO (IF PREV-LETTER (CHAR-DOWNCASE CHAR) CHAR))
                  (SETQ PREV-LETTER T))
                 ((LOWER-CASE-P CHAR)
                  (SEND STREAM :TYO (IF PREV-LETTER CHAR (CHAR-UPCASE CHAR)))
                  (SETQ PREV-LETTER T))
                 ((CHAR #/0 CHAR #/9)
                  (SEND STREAM :TYO CHAR)
                  (SETQ PREV-LETTER T))
                 (T
                  (SEND STREAM :TYO CHAR)
                  (SETQ PREV-LETTER NIL)))))
        ;(:studly)
        (T
         (SEND STREAM :STRING-OUT STRING)))
    (LET* ((MUST// NIL)
           (FSMWINS
            (WHEN (PLUSP LENGTH)
              (DO ((I 0 (1+ I))
                   (STATE (RDTBL-STARTING-STATE RDTBL))
                   (FSM (RDTBL-FSM RDTBL))
                   (CHAR)
                   (ESCAPE-CODE (RDTBL-ESCAPE-CODE RDTBL))
                   (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE RDTBL))
                   (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE RDTBL)))
                  ((= I LENGTH)
                   (COND ((NOT (NUMBERP STATE))
                          (DOLIST (L (RDTBL-MAKE-SYMBOL RDTBL))
                            (AND (EQ (CAR STATE) (CAR L))
                                 (EQ (CDR STATE) (CDR L))
                                 (RETURN T))))
                         ((NOT (NUMBERP (SETQ STATE
                                              (AREF FSM
                                                    STATE
                                                    (RDTBL-BREAK-CODE RDTBL)))))
                          (DOLIST (L (RDTBL-MAKE-SYMBOL-BUT-LAST RDTBL))
                            (AND (EQ (CAR STATE) (CAR L))
                                 (EQ (CDR STATE) (CDR L))
                                 (RETURN T))))
                         (T NIL)))
                (SETQ CHAR (ZL:AREF STRING I))
                (COND ((OR (NOT (NUMBERP STATE))        ;FSM ran out OR
                           (NOT                 ;Translated char? then fsm loses
                             (= CHAR (RDTBL-TRANS RDTBL CHAR))))
                       (OR MUST//               ;Must we slash?
                           (DO ((I I (1+ I)))
                               ((= I LENGTH))
                             (LET ((CODE (RDTBL-CODE RDTBL (ZL:AREF STRING I))))
                               (WHEN (OR (= CODE ESCAPE-CODE)
                                         (= CODE MULTIPLE-ESCAPE-CODE)
                                         (= CODE CHARACTER-CODE-ESCAPE-CODE))
                                 (SETQ MUST// T)
                                 (RETURN NIL)))))
                       (RETURN NIL)))
                (SETQ STATE
                      (AREF FSM
                            STATE
                            (COND ((LET ((CODE (RDTBL-CODE RDTBL (ZL:AREF STRING I))))
                                     (OR (= CODE ESCAPE-CODE)
                                         (= CODE MULTIPLE-ESCAPE-CODE)
                                         (= CODE CHARACTER-CODE-ESCAPE-CODE)))
                                   (SETQ MUST// T)
                                   (RDTBL-SLASH-CODE RDTBL))
                                  ((AND (TYPEP BASE '(INTEGER 10.))
                                        ( (CHAR-INT #/A) CHAR (+ BASE (CHAR-INT #/A) -11.)))
                                   (CDR (GETF (RDTBL-PLIST RDTBL) 'EXTENDED-DIGIT)))
                                  (T
                                   (RDTBL-CODE RDTBL CHAR)))))))))
      (UNLESS FSMWINS (SEND STREAM :TYO (PTTBL-OPEN-QUOTE-SYMBOL RDTBL)))
      (COND ((OR MUST//
                 (AND FSMWINS (NEQ PRINT-CASE ':UPCASE)))
             (DO ((I 0 (1+ I))
                  (ESCAPE-CODE (RDTBL-ESCAPE-CODE RDTBL))
                  (MULTIPLE-ESCAPE-CODE (RDTBL-MULTIPLE-ESCAPE-CODE RDTBL))
                  (CHARACTER-CODE-ESCAPE-CODE (RDTBL-CHARACTER-CODE-ESCAPE-CODE RDTBL))
                  (PREV-CHAR 0)
                  CODE CHAR)
                 ((= I LENGTH))
               (SETQ CHAR (ZL:AREF STRING I))
               (SETQ CODE (RDTBL-CODE RDTBL CHAR))
               (COND ((OR (= CODE ESCAPE-CODE)
                          (= CODE MULTIPLE-ESCAPE-CODE)
                          (= CODE CHARACTER-CODE-ESCAPE-CODE))
                      (SEND STREAM :TYO (PTTBL-SLASH RDTBL))
                      (SEND STREAM :TYO CHAR))
                     ;;:studly
                     ((OR (EQ print-case ':DOWNCASE)
                          (AND (EQ print-case ':CAPITALIZE)
                               (ALPHANUMERICP PREV-CHAR)))
                      (SEND STREAM :TYO (CHAR-DOWNCASE CHAR)))
                     (T
                      (SEND STREAM :TYO CHAR)))
               (SETQ PREV-CHAR CHAR)))
            (T (send stream :string-out STRING)))
      (UNLESS FSMWINS (SEND STREAM :TYO (PTTBL-CLOSE-QUOTE-SYMBOL RDTBL))))))

;; crufty old function called by old expansion of printing-random-object.
(DEFUN PRINT-PNAME-STRING (SYMBOL STREAM &OPTIONAL IGNORE NO-PACKAGE-PREFIXES)
  (cond ((and (symbolp symbol)
              *print-escape*
              (not no-package-prefixes))
         (print-symbol symbol stream))
        ((symbolp symbol)
         (print-symbol-name (symbol-name symbol) stream))
        (t
         (print-symbol-name (string symbol) stream))))


;;; Print a string, and if slashification is on, slashify it appropriately.
;>> It would be A Good Thing to print "<newline>(" as "<newline>\(" so as not
;>>  to confuse Zmacs or open-paren-in-column-zero-checking-readers
(DEFUN PRINT-QUOTED-STRING (STRING STREAM &OPTIONAL IGNORE &AUX LENGTH CHAR)
  (IF (NOT *PRINT-ESCAPE*)
      (PRINT-RAW-STRING STRING STREAM)
    (SEND STREAM :TYO (PTTBL-OPEN-QUOTE-STRING *READTABLE*))
    (SETQ LENGTH (LENGTH STRING))
    (COND ((AND (EQ (ARRAY-TYPE STRING) 'ART-STRING)
                (DOTIMES (I LENGTH T)
                  (AND (< (SETQ CHAR (AREF STRING I)) #o220)
                       (NOT (ZEROP (LOGAND #o16 (RDTBL-BITS *READTABLE* CHAR))))
                       (RETURN NIL))))
           ;; There are no double quotes, and so no slashifying.
           (SEND STREAM :STRING-OUT STRING))
          (T
           (DOTIMES (I LENGTH)
             (SETQ CHAR (CHAR-CODE (CHAR STRING I)))
             (COND ((AND (< CHAR #o220)
                         (NOT (ZEROP (LOGAND #o16 (RDTBL-BITS *READTABLE* CHAR)))))
                    (SEND STREAM :TYO (PTTBL-SLASH *READTABLE*))))
             (SEND STREAM :TYO CHAR))))
    (SEND STREAM :TYO (PTTBL-CLOSE-QUOTE-STRING *READTABLE*))))

;;; Print the string, with no slashification at all.
(DEFUN PRINT-RAW-STRING (STRING STREAM &OPTIONAL IGNORE)
  (IF (EQ (ARRAY-TYPE STRING) 'ART-STRING)
      (SEND STREAM :STRING-OUT STRING)
    (DOTIMES (I (LENGTH STRING))
      (SEND STREAM :TYO (CHAR-CODE (AREF STRING I))))))

(DEFUN PRINT-CLOSURE (CLOSURE STREAM)
  (LET ((BINDINGS (CLOSURE-BINDINGS CLOSURE)))
    (MULTIPLE-VALUE-BIND (FUNCTION-NAME TEM)
        (FUNCTION-NAME (CLOSURE-FUNCTION CLOSURE))
      (PRINTING-RANDOM-OBJECT (CLOSURE STREAM :TYPE)
        (WHEN TEM
          (PRINT-OBJECT FUNCTION-NAME 0 STREAM)
          (SEND STREAM :TYO #/SPACE))
        (COND ((INTERPRETER-ENVIRONMENT-CLOSURE-P CLOSURE)
               (SEND STREAM :STRING-OUT "(Interpreter environment)"))
              ((NULL BINDINGS)
               (SEND STREAM :STRING-OUT "(No variables)"))
              ((NULL (CDR BINDINGS))
               (SEND STREAM :STRING-OUT "(Lexical environment)"))
              (T (PRINT-FIXNUM (CL:// (LENGTH BINDINGS) 2) STREAM)))))))

(defun print-ratio (number stream &aux (base (current-print-base)))
  (declare (unspecial base))
  (when *print-radix*
    (if (neq base 10.)
        (print-print-radix-prefix base stream)
      (send stream :tyo #/#)
      (print-raw-fixnum 10. 10. stream)
      (send stream :tyo #/r)))
  (let ((n (%rational-numerator number))
        (d (%rational-denominator number)))
    (when (< n 0)
      (send stream :tyo (pttbl-minus-sign *readtable*))
      (setq n (- n)))
    (if (fixnump n)
        (print-raw-fixnum n base stream)
        (print-raw-bignum n base stream))
    (send stream :tyo (pttbl-rational-infix *readtable*))
    (if (fixnump d)
        (print-raw-fixnum d base stream)
        (print-raw-bignum d base stream))))

(defun print-complex (cplx stream)
  ;;>> This doesn't hack *print-level* (it should print "#c##" if we get too deep)
  (send stream :string-out (first (pttbl-complex *readtable*)))
  (princ (%complex-real-part cplx) stream)
  (if (second (pttbl-complex *readtable*))
      (send stream :string-out (second (pttbl-complex *readtable*)))
    (unless (minusp (%complex-imag-part cplx))
      (send stream :tyo #/+)))
  (princ (%complex-imag-part cplx) stream)
  (send stream :string-out (third (pttbl-complex *readtable*))))

(DEFUN PRINT-NOT-READABLE (EXP)
  (DECLARE (DBG:ERROR-REPORTER))
  (LET ((PRINT-READABLY NIL))
    (CERROR :NO-ACTION NIL 'SYS:PRINT-NOT-READABLE "Can't print ~S readably." EXP)))

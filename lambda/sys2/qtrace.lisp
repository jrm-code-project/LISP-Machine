;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Trace package

;; ARGLIST and VALUES are bound special

;;;     "There is always a place for debugging.  No matter how
;;;      hard you try to think of everything in advance, you
;;;      will always find that there is something else that you
;;;      hadn't thought of."
;;;                     - My Life as a Mathematician
;;;                       by Hfpsh Dboups

;;;MISSING:
;;;      - HAIRY DISPLAY FEATURES?
;;;      - "TRACE-EDSUB"

;;;Non-nil to cause the traced definitions to be compiled.
;;;That way, PROG, COND, etc. can be traced.
(DEFVAR TRACE-COMPILE-FLAG NIL)

(DEFVAR TRACED-FUNCTIONS NIL
  "List of all traced function-specs.")

(DEFVAR-RESETTABLE INSIDE-TRACE NIL NIL
  "T to disable tracing, while inside processing the tracing of something.")

(DEFVAR-RESETTABLE TRACE-LEVEL 0 0
  "Total depth within traced functions.  Controls indentation of trace output.")

(DEFVAR *TRACE-OUTPUT* NIL
  "Stream used for trace output.")
(DEFVAR TRACE-OUTPUT :UNBOUND
  "Stream used for trace output.")
(FORWARD-VALUE-CELL 'TRACE-OUTPUT '*TRACE-OUTPUT*)

(DEFVAR *TRACE-OUTPUT-TO-EDITOR* NIL
  "Or use this one if TO-EDITOR specified")

(DEFF TRACE-APPLY 'APPLY)
(DEFF TRACE-STEP-APPLY 'STEP-APPLY)
;; need to do it this way since step-apply isn't loaded when this file is, and
;;  generally for safety... (if APPLY redefined, etc...)
(add-initialization "Trace technology" '(progn (fset 'trace-apply #'apply)
                                               (fset 'trace-step-apply #'step-apply))
                    :cold)


(DEFMACRO TRACE (&REST SPECS)
  "Turn on the tracing of one or more functions.  When a function is
traced, certain actions are taken when it is called and when it returns.
The default tracing action is to print a message when the function is
called, showing its name and arguments, and another message when the
function returns, showing its name and value(s).

Example:

(trace foo)

Note that FOO is not quoted.

TRACE takes a number of options.  Here is an example in which the
function FOO is traced with the options :step and :wherein:

     (trace (foo :step :wherein bar))

More Examples:

(trace ((foo bar) :break (bad-p arglist) :value))

(trace (:function (:method flavor :message) :break t))


Following are the possible options and what they do.

:BREAK pred - Causes a breakpoint to be entered after printing the entry
trace information but before applying the traced function to its
arguments, if and only if pred evaluates to non-nil.  During the
breakpoint, the symbol ARGLIST is bound to a list of the arguments of
the function.

:EXITBREAK pred - This is like break except that the breakpoint is
entered after the function has been executed and the exit trace
information has been printed, but before control returns.  During the
breakpoint, the symbol ARGLIST is bound to a list of the arguments of
the function, and the symbol VALUES is bound to a list of the values
that the function is returning.

:ERROR -  Causes the error handler to be called when the function is
entered.  Use RESUME (or CTRL-C) to continue execution of the function.

:STEP - Causes the function to be single-stepped whenever it is
called.

:STEPCOND pred - Causes the function to be single-stepped only if pred
evaluates to non-nil.

:ENTRYCOND pred - Causes trace information to be printed on function
entry only if pred evaluates to non-nil.

:EXITCOND pred - Causes trace information to be printed on function exit
only if pred evaluates to non-nil.

:COND pred - Specifies both EXITCOND and ENTRYCOND together.

:WHEREIN function - Causes the function given to TRACE to be traced only
when it is called, directly or indirectly, from the function given to
:wherein.  You can give several trace specs to TRACE, all specifying the
same function but with different :wherein options, so that the function
is traced in different ways when called from different functions.

:ARGPDL pdl - Specifies a symbol pdl, whose value is initially set to
nil by TRACE.  When the function is traced, a list of the current
recursion level for the function, the function's name, and a list of
arguments is consed onto the pdl when the function is entered, and
cdr'ed back off when the function is exited.  The pdl can be inspected
from within a breakpoint and used to determine the very recent history
of the function.

:ENTRYPRINT form - Evaluates form and includes its value in the trace
message for calls to the function.

:EXITPRINT form - Evaluates form and includes its value in the trace
message returned from the function.

:PRINT form - Evaluates form and includes its value in the trace
messages for both calls to and returns from the function.  Equivalent to
EXITPRINT and ENTRYPRINT at once.

:ENTRY list - Specifies a list of arbitrary forms whose values are to be
printed along with the usual entry-trace.  The list of resultant values,
when printed, is preceded by \\ to separate it from the other
information.

:EXIT list - Similar to :entry, but specifies expressions whose values
are printed with the exit-trace.  Again, the list of values printed is
preceded by \\.

:ARG :VALUE :BOTH NIL - These specify which of the usual trace printouts
should be enabled.  If :arg is specified, then on function entry the
name of the function and the values of its arguments will be printed.
If :value is specified, then on function exit the returned value(s) of
the function will be printed.  If :both is specified, both of these will
be printed.  If nil is specified, neither will be printed.  If none of
these four options are specified the default is to :both.  If any
further options appear after one of these, they are not treated as
options! Rather, they are considered to be arbitrary forms whose values
are to be printed on entry and/or exit to the function, along with the
normal trace information.  The values printed will be preceded by a //,
and follow any values specified by :entry or :exit.  Note that since
these options ``swallow'' all following options, if one is given it
should be the last option specified.

When called with no arguments, TRACE returns a list of all
function-specs traced.  When you are finished tracing a function, you
must untrace it with the function UNTRACE.

If you prefer a menu interface for turning on a trace, you can use any
of the following:

      The function TV:TRACE-VIA-MENUS.

      The TRACE option on the System Menu.

      The ZMACS command META-X TRACE."
  (IF (NULL SPECS)
      `',TRACED-FUNCTIONS
    `(MAPCAN #'TRACE-1 ',SPECS)))

(DEFMACRO UNTRACE (&REST FNS)
  "Untrace one or more functions.  With no arg, untrace all traced functions."
  ;; try to prevent some lossage.
  ;; It may be best just to make this a special form, since then we can avoid
  ;;  *macroexpand-hook*, etc, lossages
  (LET ((INSIDE-TRACE T))
    `(MAPCAR 'UNTRACE-1 ',(OR FNS TRACED-FUNCTIONS))))

(DEFUN (:PROPERTY TRACE ENCAPSULATION-GRIND-FUNCTION) (FUNCTION DEF WIDTH REAL-IO UNTYO-P)
  (DECLARE (IGNORE FUNCTION DEF WIDTH REAL-IO UNTYO-P))
  (PRINC "
;Traced
"))

;;; A list in the args to UNTRACE is taken as a non-atomic function-name
;;; rather than a wherein-spec, as Maclisp would do, since UNTRACE WHEREIN
;;; is not implemented anyway, and since WHEREIN doesn't work that way in
;;; this TRACE anyway (that is, it still modifies the function cell.)
(DEFUN UNTRACE-1 (SPEC &AUX SPEC1 SPEC2)
  (SETQ SPEC (DWIMIFY-ARG-PACKAGE SPEC 'SPEC))
  (SETQ SPEC1 (UNENCAPSULATE-FUNCTION-SPEC SPEC 'TRACE))
  (COND ((NEQ SPEC1 (SETQ SPEC2 (UNENCAPSULATE-FUNCTION-SPEC SPEC1 '(TRACE))))
         (FDEFINE SPEC1 (FDEFINITION SPEC2) NIL T)
         (SETQ TRACED-FUNCTIONS (DELETE SPEC TRACED-FUNCTIONS))))
  SPEC)

(DEFUN TRACE-1 (SPEC)
  (PROG (BREAK EXITBREAK WHEREIN COND ENTRYCOND EXITCOND STEPCOND ARGPDL ENTRY EXIT
         (ARG T) (VALUE T) STEP (BARFP T) TO-EDITOR
         ENTRYVALS EXITVALS MUMBLE FCN SPEC1 TRFCN ERROR
         (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
        (IF (ATOM SPEC)
            (SETQ FCN SPEC)
          (COND ((EQ (CAR SPEC) ':FUNCTION)
                 (SETQ FCN (CADR SPEC) SPEC (CDR SPEC)))
                ((ATOM (CAR SPEC))
                 (SETQ FCN (CAR SPEC)))
                (T (RETURN (LOOP FOR FCN IN (CAR SPEC)
                                 NCONC (TRACE-1 `(:FUNCTION ,FCN . ,(CDR SPEC)))))))
          (DO ((SPECS (CDR SPEC) (CDR SPECS)))
              ((NULL SPECS))
            (CASE (CAR SPECS)
              (:BREAK (SETQ BARFP SPECS SPECS (CDR SPECS) BREAK (CAR SPECS)))
              (:EXITBREAK (SETQ BARFP SPECS SPECS (CDR SPECS) EXITBREAK (CAR SPECS)))
              (:STEPCOND (SETQ BARFP SPECS SPECS (CDR SPECS) STEPCOND (CAR SPECS)
                               STEP T))
              (:STEP (SETQ STEP T))
              (:TO-EDITOR
               (if (null *trace-output-to-editor*)
                   (setq *trace-output-to-editor*
                         (open "ed-buffer:trace-output" :direction :output)))
               (SETQ TO-EDITOR T))
              (:ERROR (SETQ ERROR T))
              (:COND (SETQ BARFP SPECS SPECS (CDR SPECS) COND (CAR SPECS)))
              (:ENTRYCOND (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRYCOND (CAR SPECS)))
              (:EXITCOND (SETQ BARFP SPECS SPECS (CDR SPECS) EXITCOND (CAR SPECS)))
              (:WHEREIN (SETQ BARFP SPECS SPECS (CDR SPECS) WHEREIN (CAR SPECS)))
              (:ARGPDL (SETQ BARFP SPECS SPECS (CDR SPECS) ARGPDL (CAR SPECS)))
              (:ENTRY (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRY (CAR SPECS)))
              (:EXIT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CAR SPECS)))
              (:PRINT (SETQ BARFP SPECS
                            SPECS (CDR SPECS)
                            ENTRY (CONS (CAR SPECS) ENTRY)
                            EXIT (CONS (CAR SPECS) EXIT)))
              (:ENTRYPRINT (SETQ BARFP SPECS SPECS (CDR SPECS)
                                 ENTRY (CONS (CAR SPECS) ENTRY)))
              (:EXITPRINT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CONS (CAR SPECS) EXIT)))
              ((:ARG :VALUE :BOTH NIL)
               (IF (MEMQ (CAR SPECS) '(:ARG NIL)) (SETQ VALUE NIL))
               (IF (MEMQ (CAR SPECS) '(:VALUE NIL)) (SETQ ARG NIL))
               (AND ARG (SETQ ENTRYVALS (CDR SPECS)))
               (AND VALUE (SETQ EXITVALS (CDR SPECS)))
               (RETURN NIL))
              (OTHERWISE
               (SETQ MUMBLE (CAR SPECS))
               (RETURN NIL)))
            (AND (NULL BARFP) (FERROR "Parameter missing"))))
        (SETQ FCN (DWIMIFY-ARG-PACKAGE FCN 'FCN))
        (UNTRACE-1 FCN)
        (WHEN MUMBLE (FERROR "Meaningless ~S keyword: ~S" 'TRACE MUMBLE))
        (CHECK-TYPE ARGPDL SYMBOL)
        (SETQ SPEC1 (UNENCAPSULATE-FUNCTION-SPEC FCN 'TRACE))

        (SETQ TRFCN (ENCAPSULATE SPEC1 FCN 'TRACE
           `(PROG* (,@(AND ARGPDL `((,ARGPDL (CONS (LIST (1+ ,COPY) ',FCN ARGLIST)
                                                   ,ARGPDL))))
                    (VALUES NIL)
                    (,COPY (1+ ,COPY))
                    (TRACE-LEVEL (1+ TRACE-LEVEL))
                    ,@(and to-editor `((*trace-output* *trace-output-to-editor*))))
                   (DECLARE (SPECIAL ,COPY VALUES))
                   ;; End of PROG var list.
                   ,(IF ERROR
                        `(PROGN (LET ((EH::ERROR-DEPTH (1+ EH::ERROR-DEPTH))
                                      (EH::CONDITION-PROCEED-TYPES '(:NO-ACTION)))
                                  (EH:INVOKE-DEBUGGER
                                    (MAKE-CONDITION 'EH::TRACE-BREAKPOINT
                                                    "~S entered" ',FCN)))
                                (RETURN (APPLY ,ENCAPSULATED-FUNCTION ARGLIST)))
                      `(COND ((OR INSIDE-TRACE
                                  ,(AND COND `(NOT ,COND))
                                  ,(AND WHEREIN `(NOT (FUNCTION-ACTIVE-P ',WHEREIN))))
                              (RETURN (APPLY ,ENCAPSULATED-FUNCTION ARGLIST)))
                             (T (LET ((INSIDE-TRACE T))
                                  ,(TRACE-MAYBE-CONDITIONALIZE ENTRYCOND
                                      `(TRACE-PRINT *TRACE-OUTPUT*
                                                    ,COPY 'ENTER ',FCN ',ARG
                                                    ',ENTRY ',ENTRYVALS))
                                  ,@(AND BREAK
                                         `((AND ,BREAK (LET (INSIDE-TRACE)
                                                         (BREAK "Entering ~S." ',FCN)))))
                                  (SETQ VALUES
                                        (LET ((INSIDE-TRACE NIL))
                                          (MULTIPLE-VALUE-LIST
                                            ,(IF (AND STEP STEPCOND)
                                                 ;; conditionally call the stepper.
                                                 `(IF ,STEPCOND
                                                      (TRACE-STEP-APPLY
                                                        ,ENCAPSULATED-FUNCTION
                                                        ARGLIST)
                                                    (TRACE-APPLY
                                                      ,ENCAPSULATED-FUNCTION
                                                      ARGLIST))
                                               `(,(IF STEP 'TRACE-STEP-APPLY 'TRACE-APPLY)
                                                 ,ENCAPSULATED-FUNCTION
                                                 ARGLIST)))))
                                  ,(TRACE-MAYBE-CONDITIONALIZE EXITCOND
                                      `(TRACE-PRINT *TRACE-OUTPUT*
                                                    ,COPY 'EXIT ',FCN ',VALUE
                                                    ',EXIT ',EXITVALS))
                                  ,@(AND EXITBREAK
                                         `((AND ,EXITBREAK (LET (INSIDE-TRACE)
                                                             (BREAK "Exiting ~S." ',FCN)))))
                                  (RETURN-LIST VALUES))))))))
        (SET TRFCN 0)
        (PUSH FCN TRACED-FUNCTIONS)
        (IF (OR TRACE-COMPILE-FLAG COMPILE-ENCAPSULATIONS-FLAG)
            (COMPILE-ENCAPSULATIONS SPEC1 'TRACE))
        (RETURN (NCONS FCN))))


(DEFUN TRACE-MAYBE-CONDITIONALIZE (CONDITION ACTION)
  (IF CONDITION
      ;;>> Barf
      `(AND ,CONDITION ,ACTION)
    ACTION))


(defvar *trace-grind? nil)

(DEFUN TRACE-PRINT (STREAM DEPTH DIRECTION FUNCTION PRINT-ARGS-FLAG EXTRAS-1 EXTRAS-2)
 (DECLARE (SPECIAL ARGLIST VALUES))
 (TERPRI STREAM)
 (DO ((N (* 2 TRACE-LEVEL) (1- N)))
     ((NOT (> N 2)))
   (WRITE-CHAR #/SP STREAM))
 (FORMAT STREAM "(~D ~A " DEPTH DIRECTION)
 (PRIN1 FUNCTION STREAM)
 (LET ((STUFF (IF (EQ DIRECTION 'ENTER) ARGLIST VALUES)))
   (COND ((AND STUFF PRINT-ARGS-FLAG)
          (PRINC ":" STREAM)
          (DO ((TAIL STUFF (CDR TAIL)))
              ((ATOM TAIL)
               (WHEN TAIL
                 (PRINC " . " STREAM)
                 (PRIN1 TAIL STREAM)))
            (cond ((not *trace-grind?)
                   (WRITE-CHAR #/SP STREAM)
                   (PRIN1 (CAR TAIL) STREAM))
                  ('else
                   (grind-top-level (car tail) nil stream
                                    nil 'displaced t nil (ncons (car tail))
                                    'grind-opti-miser
                                    (* 2 trace-level))))))))
 (WHEN EXTRAS-1
   (PRINC "  \\" STREAM)
   (DOLIST (E EXTRAS-1)
     (PRINC " " STREAM)
     (PRIN1 (let ((*standard-output* stream))
              (EVAL E))
            STREAM)))
 (WHEN EXTRAS-2
   (PRINC "  ////" STREAM)
   (DOLIST (E EXTRAS-2)
     (PRINC " " STREAM)
     (PRIN1 (let ((*standard-output* stream))
              (EVAL E))
            STREAM)))
 (PRINC ")" STREAM))



(DEFUN FUNCTION-ACTIVE-P (FUNCTION-SPEC)
  "T if dynamically within any activation of FUNCTION-SPEC."
  (LET* ((SG %CURRENT-STACK-GROUP)
         (RP (SG-REGULAR-PDL SG))
         (FNVAL (FDEFINITION FUNCTION-SPEC))
         (INIFN (SG-INITIAL-FUNCTION-INDEX SG)))
    (DO ((AP (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER) RP)
             (- AP (RP-DELTA-TO-ACTIVE-BLOCK RP AP))))
        (())
      (COND ((EQ FNVAL (RP-FUNCTION-WORD RP AP))
             (RETURN AP))
            (( AP INIFN) (RETURN NIL))))))

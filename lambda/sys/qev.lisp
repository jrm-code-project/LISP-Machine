; -*-Mode:LISP; Package:SI; Base:8; Cold-load: T -*-
;The Lisp machine EVAL

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;THERE ARE 3 WAYS THE ARGUMENTS TO A FUNCTION CAN BE DESCRIBED:
;       1) "LAMBDA" LISTS - USED IN S-EXPRESSION S
;       2) ARGUMENT DESCRIPTION LISTS - USED IN MACRO-COMPILED CODE
;       3) "NUMERIC FORM" - USED FOR MICRO-CODE ENTRIES (BOTH "HAND" AND
;               MICRO-COMPILED) AND SWITCH-ARRAY S.

; THINGS TO BE SPECIFIED (OR DEFAULTED) ABOUT A VARIABLE.
;   1) "SPECIALNESS"
;       3 states: &LOCAL (FEF-LOCAL), &SPECIAL (FEF-SPECIAL) as in Maclisp,
;       and FEF-REMOTE which is decided by the compiler.
;   2) ARGUMENT SYNTAX.  POSSIBLITIES ARE REQUIRED ARG (FEF-ARG-REQ),
;       OPTIONAL ARG &OPTIONAL (FEF-ARG-OPT).  REST ARG &REST (FEF-ARG-REST),
;       AUX "ARG" &AUX  (FEF-ARG-AUX).  THIS LAST IS EXACTLY A "PROG VARIABLE"
;       REALLY AN ARGUMENT AT ALL.
;   3) QUOTE STATUS.  INFORMATION FOR THE CALLER AS TO WHETHER HE SHOULD
;       EVALUATE THE ARGUMENT BEFORE PASSING IT. (THIS IS ONE COMPONENT OF THE
;       DISTINCTION BETWEEN EXPR AND FEXPR IN MACLISP).  POSSIBILITIES ARE
;       &EVAL (FEF-QT-EVAL), &QUOTE (FEF-QT-QT), &QUOTE-DONTCARE (FEF-QT-DONTCARE).
;       FEF-QT-DONTCARE
;       SPECIFIES THE ABSENCE OF ERROR CHECKING, AND IS TAKEN TO BE EQUIVALENT TO
;       FEF-QT-EVAL OTHERWISE.
;   4) DESIRED DATA TYPE.  IN MACRO-COMPILED CODE, THIS CAN PROVIDE ERROR CHECKING
;       WITH GREATER CONVENIENCE THAN EXPLICITLY PROGRAMMING A TYPE CHECK.  IN
;       MICRO-COMPILED CODE, GREATER EFFICENCY MAY BE OBTAINED IN COMPILED CODE,
;       PARTICULARILY AS A RESULT OF THE &FIXNUM DECLARATION.  AUTOMATIC RUN TIME
;       ERROR CHECKING OF THE SUPPLIED TYPE IS NOT DONE FOR MICRO-COMPILED FUNCTIONS.
;       POSSIBLE DATA TYPE DECLARATIONS ARE &DT-DONTCARE (THE NORMAL DEFAULT,
;       FEF-DT-DONTCARE), &DT-NUMBER (FEF-DT-NUMBER), &DT-FIXNUM (23 BIT INTEGER,
;       FEF-DT-FIXN), &DT-SYMBOL (FEF-DT-SYM), &ATOM (FEF-DT-ATOM),
;       &LIST (FEF-DT-LIST), AND &DT-FRAME (FEF-DT-FRAME).

;THE LAMBDA LIST IS THE MOST GENERAL.
; ELEMENTS OF THE LAMBDA LIST ARE OF THE FOLLOWING FORMS:
;   &DECLARATIONS -- ONE OF A GROUP OF RESERVED SYMBOLS STARTING WITH &.
;   SYMBOLIC ATOMS --  SPECIFYING A VARIABLE IN THE "CURRENT" MODE AS BUILT UP
;       FROM THE DECLARATIONS SEEN SO FAR.
;   LISTS -- THE FIRST ELEMENT OF THE LIST IS THE VARIABLE NAME, AND
;       THE CADR IS THE INITIALIZATION.  VARIABLES OF TYPES FEF-ARG-OPT,
;       AND FEF-ARG-AUX MAY BE INITIALIZED.
; DECLARATIONS:
;   THE INITIAL STATE (ASSIGNED TO VARIABLES SEEN BEFORE ANY DECLARATIONS)
;   IS FEF-ARG-REQ, FEF-DT-DONTCARE, FEF-QT-DONTCARE AND FEF-LOCAL.
;   THESE CAN BE CHANGED BY THE FOLLOWING DECLARATIONS:
;       &OPTIONAL &REST &AUX
;       &EVAL &QUOTE &QUOTE-DONTCARE
;       &DT-DONTCARE &DT-NUMBER &DT-FIXNUM &DT-SYM &DT-ATOM
;               &DT-LIST &DT-FRAME
;       &SPECIAL &LOCAL  ALSO, A PARTICULAR VARIABLE WILL BE MADE SPECIAL
;               IF A SPECIAL PROPERTY IS FOUND ON ITS PROPERTY LIST, AS IN
;               MACLISP.

;THE SIMPLEST FORM OF ARGUMENT DESCRIPTION IS NUMERIC FORM.
;THE FUNCTION %ARGS-INFO WILL RETURN A NUMERIC FORM DESCRIPTION
;GIVEN ANY FUNCTION, HOWEVER BITS MAY BE SET INDICATING THAT THIS
;NUMERIC FORM DESCRIPTION DOES NOT TELL THE WHOLE STORY.
;NAME                   VALUE           MEANING
;%ARG-DESC-QUOTED-REST    10,,000000    HAS QUOTED REST ARGUMENT
;%ARG-DESC-EVALED-REST    04,,000000    HAS EVALUATED REST ARGUMENT
;%ARG-DESC-FEF-QUOTE-HAIR 02,,000000    COMPLICATED FEF, CALLER MUST REFER TO
;                                       ARG DESC LIST FOR ARGUMENT EVAL/QUOTE INFO
;%ARG-DESC-INTERPRETED    01,,000000    INTERPRETER TRAP, VALUE ALWAYS = 01000077
;%ARG-DESC-FEF-BIND-HAIR  00,,400000    COMPLICATED FEF, LINEAR ENTER MUST REFER TO
;                                       ARG DESC LIST
;%%ARG-DESC-MIN-ARGS      00,,007700    MINIMUM NUMBER OF ARGUMENTS FIELD (COUNTING
;                                       REQUIRED ONLY)
;%%ARG-DESC-MAX-ARGS      00,,000077    MAXIMUM NUMBER OF ARGUMENTS FIELD (COUNTING
;                                       REQUIRED AND OPTIONAL BUT NOT REST.)

; ARGUMENT DESCRIPTION LISTS OF MACRO-COMPILED FUNCTIONS.
;   EACH MACRO-COMPILED FUNCTION NORMALLY HAS AN ARGUMENT DESCRIPTION LIST (A-D-L),
;       WHICH HAS AN ENTRY FOR EVERY VARIABLE BOUND OR REFERENCED IN THE FUNCTION.
;       THE ENTRY CONSISTS OF:
;   1) A SINGLE Q HOLDING A FIXNUM WHICH HAS FIELDS DECODING ALL THE FEF-XX-YY
;       OPTIONS MENTIONED ABOVE.
;   2) OPTIONALLY, (AS SPECIFIED IN 1), ANOTHER Q WHICH HOLDS THE VARIABLE'S NAME.
;       THIS IS ONLY FOR DEBUGGING CONVENIENCE AND NEVER USED BY THE SYSTEM.
;   3) POSSIBLY ANOTHER Q SPECIFING INITIALIZING INFORMATION.  THIS IS REQUIRED
;       IN SOME CASES IF NON-NIL INITIALIZATION HAS BEEN SPECIFIED.  IN OTHER CASES
;       THE VARIABLE WILL BE INITIALIZED BY COMPILED CODE.
; ADDITIONALLY, IN THE FIXED ALLOCATED PART OF THE FEF, THERE ARE THE
;   "FAST-OPTION-Q" AND THE "SPECIAL-VARIABLE-MAP" Q.  NORMALLY, THE INFORMATION
;    CONTAINED IN THESE QS IS REDUNDANT, AND THE PURPOSE IS SIMPLY TO SAVE PROCESSING
;    TIME SCANNING THRU THE A-D-L, IF POSSIBLE.  EACH OF THESE QS HAS
;    AND "OPTION" BIT,  WHICH IS TURNED OFF IF THE PARTICULAR FUNCTION
;    DOES NOT CONFORM TO THE RESTRICTIONS NECESSARY TO PERMIT THE STORAGE OF THE
;    RELEVANT INFORMATION IN THE Q.  THESE QS ARE AUTOMATICALLY SET UP BY
;    QLAP FROM THE A-D-L.
; THE FAST ARGUMENT OPTION Q WILL BE STORED IN ANY CASE, BUT ONE OF THE
; %ARG-DESC-FEF-QUOTE-HAIR OR %ARG-DESC-FEF-BIND-HAIR
; BITS WILL BE ON IF IT DOESN'T INCLUDE ALL RELEVANT INFORMATION.
; THIS MAKES LIFE EASIER FOR THE %ARGS-INFO FUNCTION.
;
;  IF IT IS POSSIBLE TO EXPRESS THE A-D-L OF THE PARTICULAR FUNCTION
;    IN "NUMERIC FORM" (SEE BELOW), THIS IS DONE IN THE FAST-OPTION Q.  THIS THEN
;    SAVES THE MACRO-CODE FUNCTION ENTRY OPERATION FROM HAVING TO SCAN DOWN
;    THE A-D-L TO DETERMINE IF THERE ARE THE RIGHT # OF ARGS, RIGHT DATA-TYPES, ETC.
;    NOTE THAT THIS WILL NOT BE POSSIBLE IF NON-NIL VARIABLE INITIALIZATION
;    (NOT DONE BY COMPILED CODE) HAS BEEN USED.
;  THE SPECIAL-VARIABLE-MAP IS A BIT MAP, WITH BITS CORRESPONDING TO THOSE
;    POSITIONS IN THE PDL-FRAME THAT CORRESPOND TO SPECIAL VARIABLES.
;    THUS, DURING BINDING AND CONTEXT SWITCHING OPERATIONS, THE FRAME SWAPPER
;    CAN PROCEED DOWN THE S-V-TABLE (WHICH CONTAINS POINTER TO THE VALUE CELLS
;    OF SPECIAL VARIABLES) AND SWAP THE APPROPRIATE PDL-FRAME QS WITH THE CONTENTS
;    OF THE APPROPRIATE VALUE CELLS.  THE SPECIAL-VARIABLE-MAP Q CAN NOT BE USED
;    IF THERE ARE SPECIAL VARIABLES HIGHER IN THE FRAME THAN ADDRESSED BY THE
;    AVAILABLE BITS, OR IF THE FUNCTION HAS A REST ARG AND EITHER THE REST ARG
;    OR AN AUX ARG IS SPECIAL.  IN THE LATER CASE,  THE VARIABLE # OF ARGS
;    MAKES IT IMPOSSIBLE TO ASSIGN A STATIC CORRESPONDENCE BETWEEN PDL-FRAME
;    INDEXES AND SPECIAL VARIABLES.
;  IF BOTH THE FAST-OPTION-Q AND THE S-V-MAP Q ARE USABLE, IT IS POSSIBLE TO
;    DISPENSE WITH THE A-D-L ITSELF ENTIRELY, IF DESIRED, TO SAVE SPACE.
;    THIS OPTION, IF SELECTED, CORRESPONDS TO THE "ABNORMAL" STATE IN THE
;    VARIOUS "NORMALLY,.. " QUALIFICATIONS ABOVE.

; "NUMERIC FORM" IS USED BY MICRO-COMPILED FUNCTIONS AND MICRO-SWITCH ARRAYS,
;       AND MESA FUNCTIONS.  (ALSO BY FAST ARG OPTION MACRO COMPILED FUNCTIONS.)
;       SEE THE DESCRIPTION OF %ARGS-INFO ABOVE FOR WHAT IS STORED IN THIS CASE.
;       THE DEFAULTING
;       OF &OPTIONAL VARIABLES AND VARIABLE INITIALIZATION IS DONE BY COMPILED CODE.
;       MICRO-COMPILED FUNCTIONS CAN NOT HAVE FEF-ARG-REST ARGS EXCEPT FOR THE FEXPR
;       CASE (AT LEAST FOR NOW.
;       ON THE REAL MACHINE, MAYBE). NO AUTOMATIC TYPE CHECKING IS EVER DONE,
;       SO IT MUST BE PROGRAMMED IF DESIRED.  HOWEVER, VARIABLES DECLARED OF TYPE
;       &FIXNUM CAN ACHIEVE SUBSTANTUAL SPEED EFFICIENCIES IN MANY CASES.
;       (NAMELY, ARITHMETIC OPERATIONS CAN BE COMPILED OPEN INSTEAD OF GOING
;       TO CLOSED SUBROUTINES).

;  A MICRO-SWITCH ARRAY IS A SINGLE DIMENSION ARRAY IN WHICH IS STORED POINTERS TO
;       MICRO-COMPILED FUNCTIONS.
;       THE SWITCH ARRAY HAS STORED IN IT AN NUMERIC ARGUMENT DESCRIPTION,
;       AND ALL FUNCTIONS IN THE ARRAY MUST BE CAPABLE OF HANDLING ARGUMENTS
;       AT LEAST AS THAT "GENERAL".  MICRO-SWITCH ARRAYS ARE EXTREMELY
;       EFFICIENT IN TIME.  RECOMMENDED USES ARE FOR DISPATCH TABLES
;       AND "LINKAGE BLOCKS".  THEY DON'T CURRENTLY EXIST.

;EVAL.
;THE WAY THIS WORKS IS IT IS GIVEN A FORM.
;NON-LIST FORMS ARE EVALUATED APPROPRIATELY. (SIMPLE)
;IN THE CASE OF A LIST FORM, FIRST THE CAR IS TAKEN AND CONVERTED
;TO A KNOWN TYPE OF FUNCTIONAL OBJECT.  CERTAIN FUNCTIONS ARE
;SPECIAL-CASED.  THESE ARE:
;(MACRO . FCN)  APPLY THE FCN TO THE FORM BEING EVALED, THEN
;               START OVER USING THE RESULT AS THE FORM.
; SYMBOL        TAKE FUNCTION CELL CONTENTS AND USE THAT.
;
;OTHERWISE %ARGS-INFO IS CALLED
;ON THAT FUNCTION TO FIND OUT INFORMATION ABOUT THE ARGUMENTS.  IN THE
;CASE OF FEFS WITHOUT THE FAST ARG OPTION, THE A-D-L IS ALSO
;CONSULTED.  IN THE CASE OF INTERPRETED FUNCTIONS, THE LAMBDA LIST
;IS GROVELED OVER TO GET THE INFORMATION.
;
; A CALL TO THE FUNCTIONAL OBJECT IS OPENED WITH %OPEN-CALL-BLOCK,
; THEN THE ARGUMENTS ARE GOBBLED DOWN, PROCESSED ACCORDING
; TO THE %ARGS-INFO, AND %PUSHED ONTO THE PDL.
; ONCE THE ARGUMENTS HAVE BEEN OBTAINED AND PUSHED,
; THE CALL IS MADE USING %ACTIVATE-OPEN-CALL-BLOCK.
; IN THE CASE OF AN ARRAY, A MACRO COMPILED FUNCTION, A MESA
; FUNCTION, A MICRO-COMPILED FUNCTION OR HAND MICRO-CODED FUNCTION,
; OR A STACK GROUP, THE MICRO CODED CALL ROUTINES WILL MAKE THE CALL.
; OTHERWISE, IT WILL TRAP BACK TO APPLY-LAMBDA:
; IF THE FUNCTION IS A LAMBDA-EXPRESSION IT WILL
; GROVEL OVER THE LAMBDA LIST AGAIN, BINDING THE LAMBDA-VARIABLES
; TO THE ARGUMENTS, THEN WILL EVALUATE THE BODY.
; OTHERWISE IF THE FUNCTION IS A LIST WHOSE CAR IS AUTOLOAD,
; IT WILL ATTEMPT TO FASLOAD THAT FILE.  OTHERWISE IT WILL BARF.
;
; THE VALUES RETURNED BY THE INVOCATION OF THE FUNCTIONAL OBJECT
; ARE PASSED BACK TO THE CALLER OF EVAL AS FOLLOWS:
; ALL VALUES INCLUDING THE LAST ARE PASSED BACK BY VIRTUE OF AN INDIRECT
; POINTER IN THE ADDITIONAL INFORMATION CREATED BY %OPEN-CALL-BLOCK,
; IN THE CASE OF A MULTIPLE-VALUE TO CALL TO EVAL.
; THE LAST IS ALSO PASSED BACK BY THE FACT THAT THE DESTINATION SAVED
; BY %OPEN-CALL-BLOCK IS DESTINATION-RETURN, WHICH IS USEFUL MAINLY
; IN THE CASE OF A NON-MULTIPLE-VALUE CALL TO EVAL.
;
; NOTE THAT %ACTIVATE-OPEN-CALL-BLOCK
; MUST CHANGE THE CDR CODE OF THE LAST ARGUMENT STORED TO CDR-NIL,
; UNLESS THERE ARE NO ARGUMENTS.

; THE EVALHOOK FEATURE.
; THE FOLLOWING FUNCTION IS ALWAYS ON THE FUNCTION CELL OF '*EVAL'
; IT IS ALSO ON THE FUNCTION CELL OF 'EVAL', UNLESS THE EVALHOOK
; IS BEING USED, IN WHICH CASE THE LATTER CELL IS LAMBDA-BOUND TO SOMETHING ELSE.

; Note that *EVAL's first local is defined to be the number of the argument
; being evaluated, for backtracing purposes.

(DEFCONST LIST-ETC-FUNCTIONS NIL)
(SETQ LIST-ETC-FUNCTIONS (LIST #'LIST #'LIST* #'LIST-IN-AREA #'LIST*-IN-AREA))

(DEFCONST LIST-ETC-FUNCTION-MAPPINGS NIL)
(SETQ LIST-ETC-FUNCTION-MAPPINGS
      (LIST (CONS #'LIST 'LIST-FOR-EVAL)
            (CONS #'LIST* 'LIST*-FOR-EVAL)
            (CONS #'LIST-IN-AREA 'LIST-IN-AREA-FOR-EVAL)
            (CONS #'LIST*-IN-AREA 'LIST*-IN-AREA-FOR-EVAL)))

(DEFCONST LAMBDA-PARAMETERS-LIMIT 60.
  "Functions accepting less than this many arguments are allowed.")
(DEFCONST CALL-ARGUMENTS-LIMIT 60.
  "Passing fewer than this many arguments in a function call is guaranteed to be ok.
Note that elements of a &rest arg that is never actually spread
do not count in this limit.")
(DEFCONST MULTIPLE-VALUES-LIMIT 60.
  "Ostensible upper bound on number of values a function call can return.
In fact, this is not what is limited, and you can get away with three times
as many if you don't fill up the maximum stack frame size in other ways.")

(DEFUN *EVAL (FORM)
  "Evaluate FORM, returning its values."
  ;; Make sure all instances of ARGNUM, below, are local slot 0.
  (LET (ARGNUM) ARGNUM)
  (COND ((SYMBOLP FORM) (SYMEVAL FORM))
        ((NUMBERP FORM) FORM)
        ((TYPEP FORM ':CHARACTER) FORM)
        ((ARRAYP FORM) FORM)
        ((NOT (CONSP FORM))
         (CERROR T NIL 'SYS:INVALID-FORM "~S is not a valid form" FORM))
        ((EQ (CAR FORM) 'QUOTE)
         (CADR FORM))
        (T (LET ((FCTN (CAR FORM)) ARG-DESC NUM-ARGS CLOSURE-PASSED)
             ;; Trace FCTN through symbols and closures to get the ultimate function
             ;; which will tell us whether to evaluate the args.
             ;; When we actually call the function, we call FCTN
             ;; unless CLOSURE-PASSED is set.  In that case, we call (CAR FORM).
             (DO () (())
               (TYPECASE FCTN
                 (:SYMBOL (SETQ FCTN (FSYMEVAL FCTN)))
                 ((OR :CLOSURE :ENTITY)
                  (SETQ FCTN (CLOSURE-FUNCTION FCTN)
                        CLOSURE-PASSED T))
                 (T (RETURN))))
             (SETQ ARG-DESC (%ARGS-INFO FCTN))
             (IF (BIT-TEST %ARG-DESC-INTERPRETED ARG-DESC)
                 ;; Here if not a FEF.
                 (PROGN
                  ;; Detect ucode entry that is not actually microcoded.
                  (IF (AND (TYPEP FCTN ':MICROCODE-FUNCTION)
                           (NOT (FIXP (SYSTEM:MICRO-CODE-ENTRY-AREA (%POINTER FCTN)))))
                      (SETQ FCTN (SYSTEM:MICRO-CODE-ENTRY-AREA (%POINTER FCTN))))
                  (TYPECASE FCTN
                    (:LIST
                      (SELECTQ (CAR FCTN)
                        ((LAMBDA SUBST NAMED-LAMBDA NAMED-SUBST)
                         (LET ((LAMBDA-LIST (IF (MEMQ (CAR FCTN) '(NAMED-LAMBDA NAMED-SUBST))
                                                (CADDR FCTN) (CADR FCTN))))
                           (SETQ NUM-ARGS 0)
                           ;; Figure out whether there is a quoted rest argument,
                           ;; and open the call block with or without adi accordingly.
                           ;; Set NUM-ARGS to the number of args not counting any quoted rest arg.
                           (DO ((LL LAMBDA-LIST (CDR LL))
                                (QUOTE-STATUS '&EVAL)
                                REST-FLAG)
                               ((OR (NULL LL)
                                    (MEMQ (CAR LL) '(&AUX &KEY)))
                                (IF APPLYHOOK
                                    (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK1 0 4)
                                           (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                                  (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 0 4))
                                (SETQ NUM-ARGS (LENGTH (CDR FORM)))
                                (%ASSURE-PDL-ROOM NUM-ARGS))
                             (COND ((MEMQ (CAR LL) '(&EVAL &QUOTE))
                                    (SETQ QUOTE-STATUS (CAR LL)))
                                   ((EQ (CAR LL) '&REST)
                                    (SETQ REST-FLAG T))
                                   ((MEMQ (CAR LL) LAMBDA-LIST-KEYWORDS))
                                   (REST-FLAG
                                    ;; Here if we encounter a rest arg.
                                    (IF ( (LENGTH (CDR FORM))
                                           (IF (EQ QUOTE-STATUS '&QUOTE)
                                               NUM-ARGS
                                             63.))
                                        ;; If there aren't enough args supplied to actually
                                        ;; reach it, arrange to exit thru the DO's end-test.
                                        (SETQ LL NIL)
                                      ;; If the quoted res arg is non-nil,
                                      ;; set NUM-ARGS to number of spread args,
                                      ;; and call with ADI.
                                      (IF APPLYHOOK
                                          (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK2 0 4)
                                                 (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                                        (%PUSH 0)
                                        (%PUSH 14000000)
                                        (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 1 4))
                                      (%ASSURE-PDL-ROOM (1+ NUM-ARGS))
                                      (RETURN)))
                                   (T (INCF NUM-ARGS))))
                           ;; Now push the args, evalling those that need it.
                           (DO ((LL LAMBDA-LIST (CDR LL))
                                (ARGL (CDR FORM) (CDR ARGL))
                                (QUOTE-STATUS '&EVAL)
                                (ARGNUM 0 (1+ ARGNUM)))
                               (())
                             (DO () ((NULL LL))
                               (COND ((MEMQ (CAR LL) '(&EVAL &QUOTE))
                                      (SETQ QUOTE-STATUS (CAR LL)))
                                     ((MEMQ (CAR LL) '(&REST &AUX &KEY))
                                      (SETQ LL NIL))
                                     ((MEMQ (CAR LL) LAMBDA-LIST-KEYWORDS))
                                     (T (RETURN)))
                               (POP LL))
                             (IF (= ARGNUM NUM-ARGS)
                                 ;; Done with spread args => push the rest arg.
                                 (RETURN
                                   (WHEN ARGL
                                     (%PUSH
                                       (IF (EQ QUOTE-STATUS '&EVAL)
                                           (MAPCAR 'EVAL ARGL)
                                         ARGL)))))
                             (IF (EQ QUOTE-STATUS '&EVAL)
                                 (%PUSH (EVAL (CAR ARGL)))
                               (%PUSH (CAR ARGL))))
                           (%ACTIVATE-OPEN-CALL-BLOCK)))
                        (MACRO (*EVAL (ERROR-RESTART (ERROR "Retry macro expansion.")
                                        (FUNCALL (CDR FCTN) FORM))))
                        ((CURRY-BEFORE CURRY-AFTER)
                         (IF APPLYHOOK
                             (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK1 0 4)
                                    (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                           (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 0 4))
                         (%ASSURE-PDL-ROOM (LENGTH (CDR FORM)))
                         (DO ((ARGL (CDR FORM) (CDR ARGL))
                              (ARGNUM 0 (1+ ARGNUM)))
                             ((NULL ARGL))
                           (%PUSH (EVAL (CAR ARGL))))
                         (%ACTIVATE-OPEN-CALL-BLOCK))
                        (T (IF (LAMBDA-MACRO-CALL-P FCTN)
                               (*EVAL (CONS (LAMBDA-MACRO-EXPAND FCTN) (CDR FORM)))
                             (INVALID-FUNCTION FORM)))))
                    ((OR :SELECT-METHOD :INSTANCE)
                     (IF APPLYHOOK
                         (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK1 0 4)
                                (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                       (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 0 4))
                     (%ASSURE-PDL-ROOM (LENGTH (CDR FORM)))
                     (DO ((ARGL (CDR FORM) (CDR ARGL))
                          (ARGNUM 0 (1+ ARGNUM)))
                         ((NULL ARGL))
                       (%PUSH (EVAL (CAR ARGL))))
                     (%ACTIVATE-OPEN-CALL-BLOCK))
                    (T (INVALID-FUNCTION FORM))))
               ;; FEF (or ucode entry that's microcoded or a FEF).
               ;; Open call block accordingly to whether there's a quoted rest arg.
               ;; Also, if more than 64 args to fn taking evaled rest arg,
               ;; we must make an explicit rest arg to avoid lossage.
               ;; LIST, etc., may not be called directly because the ucode versions
               ;; do not deal with explicitly passed rest arguments.
               ;; Starting in system 92, compiled code will not call "LIST" by that name.
               (AND LIST-ETC-FUNCTION-MAPPINGS
                    (MEMQ FCTN LIST-ETC-FUNCTIONS)
                    (SETQ FCTN (FSYMEVAL
                                 (CDR (ASSQ FCTN LIST-ETC-FUNCTION-MAPPINGS)))))
               (IF (OR (AND (BIT-TEST %ARG-DESC-QUOTED-REST ARG-DESC)
                            (> (LENGTH (CDR FORM)) (LDB %%ARG-DESC-MAX-ARGS ARG-DESC)))
                       (AND (BIT-TEST %ARG-DESC-EVALED-REST ARG-DESC)
                            (> (LENGTH (CDR FORM)) 63.)))
                   (PROGN ;; NUM-ARGS includes only the spread args.
                          (SETQ NUM-ARGS (LDB %%ARG-DESC-MAX-ARGS ARG-DESC))
                          (IF APPLYHOOK
                              (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK2 0 4)
                                     (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                            ;; ADI for fexpr-call.
                            (%PUSH 0)
                            (%PUSH 14000000)
                            (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 1 4))
                          ;; We need room for the spread args, plus one word for the rest arg.
                          (%ASSURE-PDL-ROOM (1+ NUM-ARGS)))
                 (SETQ NUM-ARGS (LENGTH (CDR FORM)))
                 (IF APPLYHOOK
                     (PROGN (%OPEN-CALL-BLOCK 'APPLYHOOK1 0 4)
                            (%PUSH (IF CLOSURE-PASSED (CAR FORM) FCTN)))
                   (%OPEN-CALL-BLOCK (IF CLOSURE-PASSED (CAR FORM) FCTN) 0 4))
                 (%ASSURE-PDL-ROOM NUM-ARGS))
               ;; If some spread args are quoted, use the ADL to tell which.
               (COND ((BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR ARG-DESC)
                      ;; Get the ADL pointer.
                      (LET ((ADL (GET-MACRO-ARG-DESC-POINTER FCTN)))
                        (DO ((ARGL (CDR FORM) (CDR ARGL))
                             (ARGNUM 0 (1+ ARGNUM)))
                            ((= ARGNUM NUM-ARGS)
                             ;; Done with spread args => push rest arg if any.
                             (WHEN ARGL
                               (%PUSH
                                 (IF (BIT-TEST %ARG-DESC-EVALED-REST ARG-DESC)
                                     (MAPCAR 'EVAL ARGL)
                                   ARGL))))
                          (LET ((ITEM (OR (CAR ADL) FEF-QT-EVAL)))
                            ;; Figure out how many extra words of ADL to skip for this arg.
                            (IF (BIT-TEST %FEF-NAME-PRESENT ITEM) (POP ADL))
                            (SELECTOR (LOGAND %FEF-ARG-SYNTAX ITEM) =
                              (FEF-ARG-OPT
                               (IF (MEMQ (LOGAND ITEM %FEF-INIT-OPTION)
                                         '(#.FEF-INI-PNTR #.FEF-INI-C-PNTR
                                           #.FEF-INI-OPT-SA #.FEF-INI-EFF-ADR))
                                   (POP ADL)))
                              (FEF-ARG-REQ)
                              ;; Note: does not get here for quoted rest arg.
                              ;; Gets here for evalled rest arg, or if no more args wanted
                              ;; (eval extra args supplied here; get error later).
                              (T (SETQ ADL NIL)))
                            (POP ADL)
                            ;; Eval the arg if the ADL says to do so.
                            (%PUSH (IF ( (LOGAND %FEF-QUOTE-STATUS ITEM) FEF-QT-QT)
                                       (EVAL (CAR ARGL))
                                     (CAR ARGL)))))))
                     (T
                      ;; No quoted args except possibly the rest arg.  Don't look at ADL.
                      (DO ((ARGNUM 0 (1+ ARGNUM))
                           (ARGL (CDR FORM) (CDR ARGL)))
                          ((= ARGNUM NUM-ARGS)
                           ;; Done with spread args => push the rest arg.
                           (WHEN ARGL
                             (%PUSH
                               (IF (BIT-TEST %ARG-DESC-EVALED-REST ARG-DESC)
                                   (MAPCAR 'EVAL ARGL)
                                 ARGL))))
                        (%PUSH (EVAL (CAR ARGL))))))
               (%ACTIVATE-OPEN-CALL-BLOCK))))))

(DEFUN INVALID-FUNCTION (FORM)
  "Report an invalid-function error in FORM and reevaluate with the function the user gives us."
  (*EVAL (CONS (CERROR ':NEW-FUNCTION NIL 'SYS:INVALID-FUNCTION
                       (IF (SYMBOLP (CAR FORM))
                           "The function ~S has a function definition which is invalid"
                         "The object ~S is not a valid function")
                       (CAR FORM))
               (CDR FORM))))

;;; LISP-REINITIALIZE also sets this back.  This is here for the source file
;;; name recording.
(DEFF EVAL #'*EVAL)

(DEFVAR EVALHOOK NIL "Value is function used on calls to EVAL, inside calls to EVALHOOK.")

(DEFVAR APPLYHOOK NIL
  "Value is function used on applications performed by EVAL, inside calls to EVALHOOK.
The function receives two arguments, like those which APPLY would receive.")

(DEFUN EVALHOOK (FORM EVALHOOK &OPTIONAL APPLYHOOK)
  "Evaluate FORM, using the function EVALHOOK for subexpressions."
  (WHEN EVALHOOK
    (BIND (FUNCTION-CELL-LOCATION 'EVAL) (FUNCTION EVALHOOK1)))
  (*EVAL FORM))

(DEFUN APPLYHOOK (FUNCTION ARGS EVALHOOK APPLYHOOK)
  "Apply FUNCTION to ARGS, with specified eval and apply hooks in place."
  (BIND (FUNCTION-CELL-LOCATION 'EVAL) (FUNCTION EVALHOOK1))
  (APPLY FUNCTION ARGS))

;Standin for EVAL.
(DEFUN EVALHOOK1 (FORM &AUX TEM)
  (SETQ TEM (IF (AND (VARIABLE-BOUNDP EVALHOOK) (NOT (NULL EVALHOOK)))
                EVALHOOK
                #'*EVAL))
  (BIND (LOCF EVALHOOK) NIL)
  (BIND (LOCF APPLYHOOK) NIL)
  (FUNCALL TEM FORM))

;Invoke the applyhook on a function which does not have an explicitly passed rest arg.
(DEFUN APPLYHOOK1 (FUNCTION &REST ARGS)
  (LET (EVALHOOK APPLYHOOK
        (TEM APPLYHOOK))
    (FUNCALL TEM FUNCTION ARGS)))

;Invoke the applyhook for a function with an explicitly passed rest arg.
;ARGS* is like the arguments to LIST*.
(DEFUN APPLYHOOK2 (FUNCTION &REST ARGS*)
  (LET ((ARGS (APPLY 'LIST* ARGS*))
        EVALHOOK APPLYHOOK
        (TEM APPLYHOOK))
    (FUNCALL TEM FUNCTION ARGS)))

(DEFUN LIST-FOR-EVAL (&REST ELEMENTS)
  (COPYLIST ELEMENTS))

(DEFUN LIST-IN-AREA-FOR-EVAL (DEFAULT-CONS-AREA &REST ELEMENTS)
  (COPYLIST ELEMENTS))

(DEFUN LIST*-FOR-EVAL (&REST ELEMENTS)
  (COND ((NULL ELEMENTS) NIL)
        ((NULL (CDR ELEMENTS))
         (CAR ELEMENTS))
        (T
         (LET* ((L (COPYLIST ELEMENTS))
                (LAST (LAST L)))
           (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE LAST 0)
           (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE LAST -1)
           L))))

(DEFUN LIST*-IN-AREA-FOR-EVAL (DEFAULT-CONS-AREA &REST ELEMENTS)
  (COND ((NULL ELEMENTS) NIL)
        ((NULL (CDR ELEMENTS))
         (CAR ELEMENTS))
        (T
         (LET* ((L (COPYLIST ELEMENTS))
                (LAST (LAST L)))
           (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE LAST 0)
           (%P-DPB-OFFSET CDR-NORMAL %%Q-CDR-CODE LAST -1)
           L))))

;UCODE INTERPRETER TRAP COMES HERE
;NOTE WILL NEVER BE CALLED BY FEXPR-CALL OR LEXPR-CALL, INSTEAD UCODE
;WILL PSEUDO-SPREAD THE REST-ARGUMENT-LIST BY HACKING THE CDR CODES.

;AUTOLOAD HERE CAN'T WIN BECAUSE IT DOESNT KNOW WHAT TO REEVALUATE.
; PUT IT INTO EVAL?

;This, if non-NIL, is an instance whose instance variables
;were bound most recently by APPLY-LAMBDA.
(DEFVAR SLOTS-BOUND-INSTANCE NIL)

(DEFUN APPLY-LAMBDA (FCTN A-VALUE-LIST)
    (PROG APPLY-LAMBDA (TEM)
       (OR (CONSP FCTN) (GO BAD-FUNCTION))
       TAIL-RECURSE
       (COND ((EQ (CAR FCTN) 'CURRY-AFTER)
              (PROG ()
                  (SETQ TEM (CDDR FCTN))
                  (%OPEN-CALL-BLOCK (CADR FCTN) 0 4)
                  (%ASSURE-PDL-ROOM (+ (LENGTH TEM) (LENGTH A-VALUE-LIST)))
                  LOOP1
                  (OR A-VALUE-LIST (GO LOOP2))
                  (%PUSH (CAR A-VALUE-LIST))
                  (AND (SETQ A-VALUE-LIST (CDR A-VALUE-LIST))
                       (GO LOOP1))

                  LOOP2
                  (OR TEM (GO DONE))
                  (%PUSH (EVAL (CAR TEM)))
                  (AND (SETQ TEM (CDR TEM))
                       (GO LOOP2))

                  DONE
                  (%ACTIVATE-OPEN-CALL-BLOCK)))
             ((EQ (CAR FCTN) 'CURRY-BEFORE)
              (PROG ()
                  (SETQ TEM (CDDR FCTN))
                  (%OPEN-CALL-BLOCK (CADR FCTN) 0 4)
                  (%ASSURE-PDL-ROOM (+ (LENGTH TEM) (LENGTH A-VALUE-LIST)))
                  LOOP1
                  (OR TEM (GO LOOP2))
                  (%PUSH (EVAL (CAR TEM)))
                  (AND (SETQ TEM (CDR TEM))
                       (GO LOOP1))

                  LOOP2
                  (OR A-VALUE-LIST (GO DONE))
                  (%PUSH (CAR A-VALUE-LIST))
                  (AND (SETQ A-VALUE-LIST (CDR A-VALUE-LIST))
                       (GO LOOP2))

                  DONE
                  (%ACTIVATE-OPEN-CALL-BLOCK)))
             ((OR (EQ (CAR FCTN) 'LAMBDA)
                  (EQ (CAR FCTN) 'SUBST)
                  (EQ (CAR FCTN) 'NAMED-SUBST)
                  (EQ (CAR FCTN) 'NAMED-LAMBDA))
              (LET* (OPTIONALF QUOTEFLAG TEM RESTF INIT THIS-RESTF
                     (FCTN (COND ((EQ (CAR FCTN) 'NAMED-LAMBDA) (CDR FCTN))
                                 ((EQ (CAR FCTN) 'NAMED-SUBST) (CDR FCTN))
                                 (T FCTN)))
                     (LAMBDA-LIST (CADR FCTN))
                     (DT-STATUS '&DT-DONTCARE)
                     (VALUE-LIST A-VALUE-LIST)
                     (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
                     MISSING-REQ-KEYS
                     KEYNAMES KEYVALUES KEYINITS KEYKEYS KEYOPTFS KEYFLAGS
                     KEY-SUPPLIED-FLAGS
                     ALLOW-OTHER-KEYS)
                (SETQ FCTN (CDDR FCTN)) ;throw away lambda list
                (DO () (())
                  (COND ((AND (CDR FCTN) (STRINGP (CAR FCTN)))
                         (POP FCTN))    ;and doc string.
                        ;; Process any (DECLARE) at the front of the function.
                        ;; This does not matter for SPECIAL declarations,
                        ;; but for MACRO declarations it might be important
                        ;; even in interpreted code.
                        ((AND (NOT (ATOM (CAR FCTN)))
                              (MEMQ (CAAR FCTN) '(DECLARE :DECLARE)))
                         (SETQ LOCAL-DECLARATIONS (APPEND (CDAR FCTN) LOCAL-DECLARATIONS))
                         (POP FCTN))
                        (T (RETURN))))
                (PROG ()
                  ;; If SELF is an instance, and its instance vars aren't bound, bind them.
                  (AND (TYPEP SELF ':INSTANCE)
                       (NEQ SELF SLOTS-BOUND-INSTANCE)
                       (PROGN (%USING-BINDING-INSTANCES (SELF-BINDING-INSTANCES))
                              (BIND (LOCF SLOTS-BOUND-INSTANCE) SELF)))
             L    (COND ((NULL VALUE-LIST) (GO LP1))
                        ((OR (NULL LAMBDA-LIST)
                             (EQ (CAR LAMBDA-LIST) '&AUX))
                         (COND (RESTF (GO LP1))
                               (T (GO TOO-MANY-ARGS))))
                        ((EQ (CAR LAMBDA-LIST) '&KEY)
                         (GO KEY))
                        ((EQ (CAR LAMBDA-LIST) '&OPTIONAL)
                         (SETQ OPTIONALF T)
                         (GO L1))                   ;Do next value.
                        ((MEMQ (CAR LAMBDA-LIST) '(&QUOTE &EVAL))
                         (SETQ QUOTEFLAG (EQ (CAR LAMBDA-LIST) '&QUOTE))
                         (GO L1))
                        ((EQ (CAR LAMBDA-LIST) '&REST)
                         (SETQ THIS-RESTF T)
                         (GO L1))                   ;Do next value.

                        ((MEMQ (CAR LAMBDA-LIST)
                               '(&DT-DONTCARE &DT-NUMBER &DT-FIXNUM &DT-SYMBOL &DT-ATOM
                                              &DT-LIST &DT-FRAME))
                         (SETQ DT-STATUS (CAR LAMBDA-LIST))
                         (GO L1))                   ;Do next value.
                        ((MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
                         (GO L1))
                        ((ATOM (CAR LAMBDA-LIST)) (SETQ TEM (CAR LAMBDA-LIST)))
                        ((ATOM (CAAR LAMBDA-LIST))
                         (SETQ TEM (CAAR LAMBDA-LIST))
                         ;; If it's &OPTIONAL (FOO NIL FOOP),
                         ;; bind FOOP to T since FOO was specified.
                         (COND ((AND OPTIONALF (CDDAR LAMBDA-LIST))
                                (AND (NULL (CADDAR LAMBDA-LIST)) (GO BAD-LAMBDA-LIST))
                                (BIND (VALUE-CELL-LOCATION (CADDAR LAMBDA-LIST)) T))))
                        (T (GO BAD-LAMBDA-LIST)))
                  ;; Get here if there was a real argname in (CAR LAMBDA-LIST).
                  ;;  It is in TEM.
                  (AND (NULL TEM) (GO BAD-LAMBDA-LIST))
                  (COND (RESTF (GO BAD-LAMBDA-LIST))    ;Something follows a &REST arg???
                        (THIS-RESTF             ;This IS the &REST arg.
                         ;; If quoted arg, and the list of values is in a pdl, copy it.
                         (AND QUOTEFLAG
                              (LDB-TEST %%PHT2-MAP-ACCESS-CODE
                                        (AREA-REGION-BITS (%AREA-NUMBER VALUE-LIST)))
                              (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
                                (SETQ VALUE-LIST (COPYLIST VALUE-LIST))))
                         (BIND (LOCF (SYMEVAL TEM)) VALUE-LIST)
                         ;; We don't clear out VALUE-LIST
                         ;; in case keyword args follow.
                         (SETQ THIS-RESTF NIL RESTF T)
                         (GO L1)))
                  (BIND (VALUE-CELL-LOCATION TEM) (CAR VALUE-LIST))
                  (SETQ VALUE-LIST (CDR VALUE-LIST))
             L1   (SETQ LAMBDA-LIST (CDR LAMBDA-LIST))
                  (GO L)

             KEY  (SETF (VALUES NIL NIL LAMBDA-LIST NIL NIL
                                KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS
                                ALLOW-OTHER-KEYS)
                        (DECODE-KEYWORD-ARGLIST LAMBDA-LIST))
                  ;; Make a list of all required keywords we haven't seen yet.
                  (IF OPTIONALF
                      (SETQ MISSING-REQ-KEYS NIL)  ;All are optional if &optional already seen
                    (SETQ MISSING-REQ-KEYS
                          (SUBSET-NOT #'PROG2 KEYKEYS KEYOPTFS)))
                  ;; Process the special keyword :ALLOW-OTHER-KEYS if present as an arg.
                  (IF (GET (LOCF VALUE-LIST) ':ALLOW-OTHER-KEYS)
                      (SETQ ALLOW-OTHER-KEYS T))
                  ;; Make alist of (keyword supplied-flag-var supplied-this-time-p)
                  (DO ((KEYL KEYKEYS (CDR KEYL))
                       (FLAGL KEYFLAGS (CDR FLAGL)))
                      ((NULL KEYL))
                    (AND (CAR FLAGL)
                         (PUSH (LIST (CAR KEYL) (CAR FLAGL) NIL)
                               KEY-SUPPLIED-FLAGS)))

                  (SETQ KEYVALUES (MAKE-LIST (LENGTH KEYNAMES)))
                  ;; Now look at what keyword args were actually supplied.
                  ;; Set up KEYVALUES to contain values corresponding
                  ;; with the variable names in KEYNAMES.
                  (DO ((VL VALUE-LIST (CDDR VL))
                       KEYWORD (FOUND-FLAGS 0))
                      ((NULL VL))
                    (OR (CDR VL)
                        (FERROR 'SYS:BAD-KEYWORD-ARGLIST
                                "No argument after keyword ~S"
                                (CAR VL)))
                    (SETQ KEYWORD (CAR VL))
                    RETRY
                    (LET ((TEM (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
                      (COND (TEM
                             (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- TEM))))
                               (SETQ FOUND-FLAGS
                                     (DPB 1 (BYTE TEM 1) FOUND-FLAGS))
                               (SETF (NTH TEM KEYVALUES) (CADR VL))
                               (SETF (NTH TEM KEYINITS) NIL)
                               (SETQ MISSING-REQ-KEYS
                                     (DELQ KEYWORD MISSING-REQ-KEYS))
                               (LET ((TEM1 (ASSQ KEYWORD KEY-SUPPLIED-FLAGS)))
                                 (AND TEM1 (SETF (CADDR TEM1) T)))))
                            ((NOT ALLOW-OTHER-KEYS)
                             (SETQ KEYWORD (CERROR ':NEW-KEYWORD NIL
                                                   'SYS:UNDEFINED-KEYWORD-ARGUMENT
                                                   "Keyword arg keyword ~S, with value ~S, is unrecognized."
                                                   KEYWORD
                                                   (CADR VL)))
                             (AND KEYWORD (GO RETRY))))))
                  ;; Eval the inits of any keyword args that were not supplied.
                  (DO ((KVS KEYVALUES (CDR KVS))
                       (KIS KEYINITS (CDR KIS)))
                      ((NULL KVS))
                    (AND (CAR KIS)
                         (RPLACA KVS (EVAL (CAR KIS)))))
                  ;; Bind the supplied-flags of the optional keyword args.
                  ;; Can't use DO here because the bindings must stay around.
             KEY1 (COND (KEY-SUPPLIED-FLAGS
                         (BIND (LOCF (SYMEVAL (CADAR KEY-SUPPLIED-FLAGS)))
                               (CADDAR KEY-SUPPLIED-FLAGS))
                         (POP KEY-SUPPLIED-FLAGS)
                         (GO KEY1)))
                  ;; If any required keyword args were not specified, barf.
                  (MAPCAR #'(LAMBDA (KEYWORD)
                              (SETF (NTH (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)
                                         KEYVALUES)
                                    (CERROR ':ARGUMENT-VALUE NIL
                                            'SYS:MISSING-KEYWORD-ARGUMENT
                                            "The required keyword arg ~S was not supplied."
                                            KEYWORD)))
                          MISSING-REQ-KEYS)
                  ;; Keyword args always use up all the values that are left...

                  ;; Here when all values used up.
             LP1  (COND ((NULL LAMBDA-LIST) (GO EX1))
                        ((EQ (CAR LAMBDA-LIST) '&REST)
                         (AND RESTF (GO BAD-LAMBDA-LIST))
                         (SETQ THIS-RESTF T)
                         (GO LP2))
                        ((EQ (CAR LAMBDA-LIST) '&KEY)
                         (GO KEY))
                        ((MEMQ (CAR LAMBDA-LIST) '(&OPTIONAL &AUX))
                         (SETQ OPTIONALF T)             ;SUPPRESS TOO FEW ARGS ERROR
                         (GO LP2))
                        ((MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
                         (GO LP2))
                        ((AND (NULL OPTIONALF) (NULL THIS-RESTF))
                         (AND RESTF (GO BAD-LAMBDA-LIST))
                         (GO TOO-FEW-ARGS))
                        ((ATOM (CAR LAMBDA-LIST)) (SETQ TEM (CAR LAMBDA-LIST))
                         (SETQ INIT NIL))
                        ((ATOM (CAAR LAMBDA-LIST))
                         (SETQ TEM (CAAR LAMBDA-LIST))
                         (SETQ INIT (EVAL (CADAR LAMBDA-LIST)))
                         ;; For (FOO NIL FOOP), bind FOOP to NIL since FOO is missing.
                         (COND ((CDDAR LAMBDA-LIST)
                                (AND (NULL (CADDAR LAMBDA-LIST)) (GO BAD-LAMBDA-LIST))
                                (BIND (VALUE-CELL-LOCATION (CADDAR LAMBDA-LIST)) NIL))))
                        (T (GO BAD-LAMBDA-LIST)))
             LP3  (AND (NULL TEM) (GO BAD-LAMBDA-LIST))
                  (BIND (VALUE-CELL-LOCATION TEM) INIT)
                  (AND THIS-RESTF (SETQ RESTF T))
                  (SETQ THIS-RESTF NIL)
             LP2  (SETQ LAMBDA-LIST (CDR LAMBDA-LIST))
                  (GO LP1)

             EX1  ;; Here to evaluate the body.
                  ;; First bind the keyword args if any.
                  (PROGV KEYNAMES KEYVALUES
                         (DO ((L FCTN (CDR L)))
                             ((NULL (CDR L))
                              (RETURN-FROM APPLY-LAMBDA (EVAL (CAR L))))
                           (EVAL (CAR L)))))))
             ((EQ (CAR FCTN) 'MACRO)
              (FERROR 'SYS:FUNCALL-MACRO
                      "Funcalling the macro ~S."
                      (FUNCTION-NAME (CDR FCTN)))
              (RETURN-FROM APPLY-LAMBDA
                           (EVAL (CONS FCTN (MAPCAR #'(LAMBDA (ARG) `',ARG) A-VALUE-LIST)))))
             )

       ;; A list, but don't recognize the keyword.  Check for a LAMBDA position macro.
       (COND ((LAMBDA-MACRO-CALL-P FCTN)
              (SETQ FCTN (LAMBDA-MACRO-EXPAND FCTN))
              (GO RETRY)))

       BAD-FUNCTION
       ;; Can drop through to here for a totally unrecognized function.
       (SETQ FCTN
             (CERROR ':NEW-FUNCTION NIL 'SYS:INVALID-FUNCTION
                     "~S is an invalid function." FCTN))
       (GO RETRY)

       ;; Errors jump out of the inner PROG to unbind any lambda-vars bound with BIND.

       BAD-LAMBDA-LIST
       (SETQ FCTN
             (CERROR ':NEW-FUNCTION NIL 'SYS:INVALID-LAMBDA-LIST
                     "~S has an invalid LAMBDA list" FCTN))
       RETRY
       (AND (CONSP FCTN) (GO TAIL-RECURSE))
       (RETURN (APPLY FCTN A-VALUE-LIST))

       TOO-FEW-ARGS
       (RETURN (SIGNAL-PROCEED-CASE
                 ((ARGS)
                  (MAKE-CONDITION 'SYS:TOO-FEW-ARGUMENTS
                       "Function ~S called with only ~D argument~1G~P."
                       FCTN (LENGTH A-VALUE-LIST) A-VALUE-LIST))
                 (:ADDITIONAL-ARGUMENTS
                   (APPLY FCTN (APPEND A-VALUE-LIST ARGS)))
                 (:RETURN-VALUE ARGS)
                 (:NEW-ARGUMENT-LIST (APPLY FCTN ARGS))))

       TOO-MANY-ARGS
       (RETURN (SIGNAL-PROCEED-CASE
                 ((ARGS)
                  (MAKE-CONDITION 'SYS:TOO-MANY-ARGUMENTS
                       "Function ~S called with too many arguments (~D)."
                       FCTN (LENGTH A-VALUE-LIST) A-VALUE-LIST))
                 (:FEWER-ARGUMENTS
                   (APPLY FCTN (APPEND A-VALUE-LIST ARGS)))
                 (:RETURN-VALUE ARGS)
                 (:NEW-ARGUMENT-LIST (APPLY FCTN ARGS))))))

;DECODE-KEYWORD-ARGLIST

;Given a lambda list, return a decomposition of it and a description
;of all the keyword args in it.
;POSITIONAL-ARGS is the segment of the front of the arglist before any keyword args.
;KEYWORD-ARGS is the segment containing the keyword args.
;AUXVARS is the segment containing the aux vars.
;REST-ARG is the name of the rest arg, if any, else nil.
;POSITIONAL-ARG-NAMES is a list of all positional args
; and the supplied-flags of all optional positional args.
;The rest of the values describe the keyword args.
;There are several lists, equally long, with one element per arg.
;KEYNAMES contains the keyword arg variable names.
;KEYKEYS contains the key symbols themselves (in the keyword package).
;KEYOPTFS contains T for each optional keyword arg, NIL for each required one.
;KEYINITS contains for each arg the init-form, or nil if none.
;KEYFLAGS contains for each arg its supplied-flag's name, or nil if none.
;Finally,
;ALLOW-OTHER-KEYS is T if &ALLOW-OTHER-KEYS appeared among the keyword args.
(DEFUN DECODE-KEYWORD-ARGLIST (LAMBDA-LIST)
  (DECLARE (RETURN-LIST POSITIONAL-ARGS KEYWORD-ARGS AUXVARS
                        REST-ARG POSITIONAL-ARG-NAMES
                        KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS ALLOW-OTHER-KEYS))
  (LET (POSITIONAL-ARGS KEYWORD-ARGS AUXVARS
        OPTIONALF THIS-REST REST-ARG POSITIONAL-ARG-NAMES
        KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
    (SETQ AUXVARS (MEMQ '&AUX LAMBDA-LIST))
    (SETQ POSITIONAL-ARGS (LDIFF LAMBDA-LIST AUXVARS))
    (SETQ KEYWORD-ARGS (MEMQ '&KEY POSITIONAL-ARGS))
    (SETQ POSITIONAL-ARGS (LDIFF POSITIONAL-ARGS KEYWORD-ARGS))

    (SETQ KEYWORD-ARGS (LDIFF KEYWORD-ARGS AUXVARS))
    ;; Get names of all positional args and their supplied-flags.
    ;; Get name of rest arg if any.  Find out whether they end optional.
    (DOLIST (A POSITIONAL-ARGS)
      (COND ((EQ A '&OPTIONAL) (SETQ OPTIONALF T))
            ((EQ A '&REST) (SETQ THIS-REST T))
            ((MEMQ A LAMBDA-LIST-KEYWORDS))
            (T (COND ((SYMBOLP A) (PUSH A POSITIONAL-ARG-NAMES))
                     (T (AND (CDDR A) (PUSH (CADDR A) POSITIONAL-ARG-NAMES))
                        (PUSH (CAR A) POSITIONAL-ARG-NAMES)))
               (AND THIS-REST (NOT REST-ARG) (SETQ REST-ARG (CAR POSITIONAL-ARG-NAMES))))))
    (SETQ POSITIONAL-ARG-NAMES (NREVERSE POSITIONAL-ARG-NAMES))
    ;; Decode the keyword args.  Set up keynames, keyinits, keykeys, keyflags.
    (DOLIST (A (CDR KEYWORD-ARGS))
      (COND ((EQ A '&OPTIONAL) (SETQ OPTIONALF T))
            ((EQ A '&ALLOW-OTHER-KEYS) (SETQ ALLOW-OTHER-KEYS T))
            ((MEMQ A LAMBDA-LIST-KEYWORDS))
            (T (LET (KEYNAME KEYINIT KEYFLAG KEYKEY)
                 (IF (AND (CONSP A) (CONSP (CAR A)))
                     ;; Key symbol specified explicitly.
                     (SETQ KEYKEY (CAAR A) KEYNAME (CADAR A))
                   ;; Else determine it from the variable name.
                   (SETQ KEYNAME (IF (CONSP A) (CAR A) A))
                   (OR (SETQ KEYKEY (GET KEYNAME 'KEYKEY))
                       (PROGN (SETQ KEYKEY (INTERN (GET-PNAME KEYNAME)
                                                   SI:PKG-KEYWORD-PACKAGE))
                              (PUTPROP KEYNAME KEYKEY 'KEYKEY))))
                 (IF (CONSP A)
                     (SETQ KEYINIT (CADR A) KEYFLAG (CADDR A)))
                 (PUSH KEYNAME KEYNAMES)
                 (PUSH OPTIONALF KEYOPTFS)
                 (PUSH KEYINIT KEYINITS)
                 (PUSH KEYFLAG KEYFLAGS)
                 (PUSH KEYKEY KEYKEYS)))))
    ;; Get everything about the keyword args back into forward order.
    (SETQ KEYNAMES (NREVERSE KEYNAMES)
          KEYINITS (NREVERSE KEYINITS)
          KEYOPTFS (NREVERSE KEYOPTFS)
          KEYKEYS (NREVERSE KEYKEYS)
          KEYFLAGS (NREVERSE KEYFLAGS))
    (VALUES POSITIONAL-ARGS KEYWORD-ARGS AUXVARS
            REST-ARG POSITIONAL-ARG-NAMES
            KEYKEYS KEYNAMES KEYOPTFS KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)))

;;;FULL MAPPING FUNCTIONS

(DEFUN MAPCAR (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns a list of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS LP)))                  ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPC (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns second argument."
  (PROG (LP RES)
        (SETQ RES (CAR LISTS))                          ;RESULT WILL BE FIRST ARG
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 0)                      ;DESTINATION IGNORE
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN RES))              ;A LIST ENDS, RETURN SECOND ARG
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (GO L)))

(DEFUN MAPLIST (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns a list of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (RPLACD P (SETQ P (NCONS LP)))                  ;CONS IT ONTO LIST
        (GO L)))

(DEFUN MAPL (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns second argument."
  (PROG (LP RES)
        (SETQ RES (CAR LISTS))                          ;RESULT WILL BE FIRST ARG
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 0)                      ;DESTINATION IGNORE
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN RES))              ;A LIST ENDS, RETURN SECOND ARG
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (GO L)))
(DEFF MAP #'MAPL)

(DEFUN MAPCAN (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive elements, returns NCONC of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAAR LP))                               ;PASS CAR OF THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (AND (ATOM LP) (GO L))                          ;IF NOT A LIST, IGNORE IT
        (RPLACD P LP)                                   ;CONC IT ONTO LIST
        (SETQ P (LAST LP))                              ;SAVE NEW CELL TO BE CONC'ED ONTO
        (GO L)))

(DEFUN MAPCON (&FUNCTIONAL FCN &EVAL &REST LISTS)
  "Maps over successive sublists, returns NCONC of the results."
  (PROG (V P LP)
        (SETQ P (LOCF V))                               ;ACCUMULATE LIST IN P, V
        (%ASSURE-PDL-ROOM (+ (LENGTH LISTS) 4))         ;MAKE SURE %PUSH'S DON'T LOSE
   L    (SETQ LP LISTS)                                 ;PICK UP NEXT ELEMENT OF EACH LIST
        (%OPEN-CALL-BLOCK FCN 0 1)                      ;DESTINATION STACK
   L1   (OR LP (GO L2))                                 ;ALL LISTS PICKED UP
        (AND (NULL (CAR LP)) (RETURN V))                ;A LIST ENDS, RETURN
        (%PUSH (CAR LP))                                ;PASS THIS LIST AS ARG
        (RPLACA LP (CDAR LP))                           ;ADVANCE TO CDR OF THIS LIST
        (SETQ LP (CDR LP))                              ;DO NEXT LIST
        (GO L1)
   L2   (%ACTIVATE-OPEN-CALL-BLOCK)                     ;MAKE THE CALL
        (SETQ LP (%POP))                                ;GRAB RESULT BEFORE PDL CHANGES
        (AND (ATOM LP) (GO L))                          ;IF NOT A LIST, IGNORE IT
        (RPLACD P LP)                                   ;CONC IT ONTO LIST
        (SETQ P (LAST LP))                              ;SAVE NEW CELL TO BE CONC'ED ONTO
        (GO L)))

(DEFUN FUNCALL (&FUNCTIONAL FN &EVAL &REST ARGS)
  "Apply FN to the ARGS."
  (APPLY FN ARGS))

(DEFUN QUOTE (&QUOTE X) X)

;;; "Evaluate a form as in function position"
;;; A symbol turns into its function definition.
;;; A list is either a functional constant, which is self-evaluating,
;;; or a function-spec, which turns into its function definition.
;;; Note that (:property foo bar) is a function spec but (foo bar) is not a function spec!
;;; Atomic functional constants (FEFs and so forth) are also allowed.
(DEFUN FUNCTION (&QUOTE X)
  "If X is a symbol or function spec, return its function definition.
If X is a list which is a function, return it, but signal the compiler
to compile it."
  (COND ((SYMBOLP X) (FSYMEVAL X))              ;"Functional variable"
        ((FUNCTIONP X T) X)                     ;Functional constant
        ((VALIDATE-FUNCTION-SPEC X)             ;Function spec
         (FDEFINITION X))
        (T (FERROR NIL "~S is not a function nor the name of a function" X))))

(DEFUN FUNCTIONAL-ALIST (&QUOTE X)   ;JUST LIKE QUOTE INTERPRETED.  HOWEVER, THE COMPILER
       X)     ;IS TIPPED OFF TO BREAK OFF  AND COMPILE SEPARATELY FUNCTIONS WHICH APPEAR
              ;IN THE CDR POSITION OF AN ALIST ELEMENT

(DEFUN SUBSET (&FUNCTIONAL PRED LIST &EVAL &REST EXTRA-LISTS &AUX VALUE P LP)
  "Return a list of all elements of LIST for which PRED is true.
If extra args are supplied, their successive elements are passed
to PRED along with elements of LIST.  Unlike MAP, etc., we process
every element of LIST even if extra args are exhausted by cdr'ing."
  (SETQ P (LOCF VALUE))                         ;ACCUMULATE LIST IN P, VALUE
  (%ASSURE-PDL-ROOM (+ (LENGTH EXTRA-LISTS) 5)) ;Make sure %PUSH's don't lose.
  (DO () ((NULL LIST) VALUE)
    (SETQ LP EXTRA-LISTS)
    (%OPEN-CALL-BLOCK PRED 0 1)                 ;call with destination=stack.
    (%PUSH (CAR LIST))                          ;push next element of LIST.

    (DO () ((NULL LP))                          ;LP scans down the extra lists.
      (%PUSH (CAAR LP))                         ;Push car of each one.
      (POP (CAR LP))                            ;cdr this list.
      (POP LP))                                 ;advance to next list.

    (%ACTIVATE-OPEN-CALL-BLOCK)                 ;Make the call.
    (IF (%POP)                                  ;If value non-nil, put this one in the value.
        (RPLACD P (SETQ P (NCONS (CAR LIST)))))

    (POP LIST)))

(DEFUN SUBSET-NOT (&FUNCTIONAL PRED LIST &EVAL &REST EXTRA-LISTS &AUX VALUE P LP)
  "Return a list of all elements of LIST for which PRED is false.
If extra args are supplied, their successive elements are passed
to PRED along with elements of LIST.  Unlike MAP, etc., we process
every element of LIST even if extra args are exhausted by cdr'ing."
  (SETQ P (LOCF VALUE))                         ;ACCUMULATE LIST IN P, VALUE
  (%ASSURE-PDL-ROOM (+ (LENGTH EXTRA-LISTS) 5)) ;Make sure %PUSH's don't lose.
  (DO () ((NULL LIST) VALUE)
    (SETQ LP EXTRA-LISTS)
    (%OPEN-CALL-BLOCK PRED 0 1)                 ;call with destination=stack.
    (%PUSH (CAR LIST))                          ;push next element of LIST.

    (DO () ((NULL LP))                          ;LP scans down the extra lists.
      (%PUSH (CAAR LP))                         ;Push car of each one.
      (POP (CAR LP))                            ;cdr this list.
      (POP LP))                                 ;advance to next list.

    (%ACTIVATE-OPEN-CALL-BLOCK)                 ;Make the call.
    (IF (NOT (%POP))                            ;If value nil, put this one in the value.
        (RPLACD P (SETQ P (NCONS (CAR LIST)))))

    (POP LIST)))

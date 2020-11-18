;;; General use parser -*- Mode:LISP; Package:ZWEI; Base:8 -*-
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(DEFSTRUCT (PARSE-GRAMMAR :ARRAY :NAMED :CONC-NAME (:ALTERANT NIL))
  NAME                                          ;A symbol
  TERMINALS                                     ;List of lexemes and EOF
  SYMBOLS                                       ;Above plus all intermediates
  PRODUCTIONS                                   ;List of all productions
  TOP-LEVEL-PRODUCTION                          ;Augmented top-level production
  ACTIONS                                       ;A dispatch table from state,symbol
  GOTOS                                         ;A dispatch table from state,symbol
  INITIAL-STATE                                 ;State of top-level production
  LEFT-ASSOCIATIVE-TERMINALS                    ;Prefer reduce over shift
  RIGHT-ASSOCIATIVE-TERMINALS                   ;Prefer shift over reduce
  NON-ASSOCIATIVE-TERMINALS                     ;Give error if get to ambiguous state
  IGNORED-TERMINALS                             ;Unless otherwise handled
  PRECEDENCE-LIST                               ;List of lexemes, dummies or lists thereof
  LEXER                                         ;A function for producing list of terminals
  )

(DEFSTRUCT (PARSE-PRODUCTION :ARRAY :CONC-NAME (:ALTERANT NIL))
  SYMBOL                                        ;Input
  OUTPUT                                        ;List of symbols
  FUNCTION                                      ;Actual constructor
  PRECEDENCE                                    ;An element of the PRECEDENCE-LIST
  )

(DEFSTRUCT (PARSE-ITEM :LIST :CONC-NAME (:ALTERANT NIL))
  PRODUCTION                                    ;A PARSE-PRODUCTION
  POSITION                                      ;An NTHCDR of PARSE-PRODUCTION-OUTPUT of above
  TERMINAL                                      ;A lexeme or EOF
  )

;;; TABLE[SYMBOL,OFFSET]
(DEFUN MAKE-SYMBOL-DISPATCH (SYMBOLS WIDTH)
  (LOOP FOR SYMBOL IN SYMBOLS
        AS LIST = (MAKE-LIST (1+ WIDTH))
        DO (SETF (CAR LIST) SYMBOL)
        COLLECT LIST))

;;; GRAMMAR is supplied if this is the ACTIONS table
(DEFUN SET-SYMBOL-DISPATCH (VAL DISPATCH SYMBOL OFFSET &OPTIONAL GRAMMAR &AUX TEM OVAL)
  (OR (SETQ TEM (ASSQ SYMBOL DISPATCH))
      (FERROR NIL "~S not found in ~S" SYMBOL DISPATCH))
  (OR (AND (SETQ OVAL (NTH (1+ OFFSET) TEM))
           (NOT (EQUAL VAL OVAL))
           (COND ((NULL GRAMMAR)
                  (FERROR NIL "Parsing conflict and grammar not given"))
                 ((EQ OVAL '(ERROR))
                  T)                            ;Don't change ERROR
                 ((MEMQ SYMBOL (PARSE-GRAMMAR-NON-ASSOCIATIVE-TERMINALS GRAMMAR))
                  (SETQ VAL '(ERROR))
                  NIL)                          ;Do do setting if should be error
                 ((RESOLVE-PARSE-CONFLICT OVAL VAL SYMBOL OFFSET GRAMMAR)
                  NIL)                          ;Do setting if told to
                 (T
                  T)))                          ;Otherwise do not
      (SETF (NTH (1+ OFFSET) TEM) VAL)))

;;; This returns T if NEW should be substituted
(DEFUN RESOLVE-PARSE-CONFLICT (OLD NEW SYMBOL OFFSET GRAMMAR &AUX (T-FOR-NEW T) (ERROR-P T)
                               OLD-TYPE NEW-TYPE OLD-ARG NEW-ARG OLD-PREC NEW-PREC ALLOW-NEW)
  (SETQ OLD-TYPE (CAR OLD) OLD-ARG (CADR OLD))
  (SETQ NEW-TYPE (CAR NEW) NEW-ARG (CADR NEW))
  ;; If SHIFT-REDUCE conflict, make NEW be SHIFT and OLD be REDUCE
  (AND (EQ OLD-TYPE 'SHIFT) (EQ NEW-TYPE 'REDUCE)
       (PSETQ OLD NEW NEW OLD
              OLD-TYPE NEW-TYPE NEW-TYPE OLD-TYPE
              OLD-ARG NEW-ARG NEW-ARG OLD-ARG
              T-FOR-NEW NIL))
  ;; Can handle SHIFT-REDUCE or REDUCE-REDUCE conflicts
  (COND ((EQ OLD-TYPE 'REDUCE)
         (SETQ ERROR-P 'WARN)
         (SETQ OLD-PREC (PARSE-ACTION-PRECEDENCE OLD-TYPE OLD-ARG SYMBOL GRAMMAR)
               NEW-PREC (PARSE-ACTION-PRECEDENCE NEW-TYPE NEW-ARG SYMBOL GRAMMAR))
         (IF ( OLD-PREC NEW-PREC)
             (SETQ ERROR-P NIL
                   ALLOW-NEW (> NEW-PREC OLD-PREC))
             (AND (EQ NEW-TYPE 'SHIFT)
                  (COND ((MEMQ SYMBOL (PARSE-GRAMMAR-LEFT-ASSOCIATIVE-TERMINALS GRAMMAR))
                         (SETQ ERROR-P NIL
                               ALLOW-NEW NIL))  ;Prefer REDUCE
                        ((MEMQ SYMBOL (PARSE-GRAMMAR-RIGHT-ASSOCIATIVE-TERMINALS GRAMMAR))
                         (SETQ ERROR-P NIL
                               ALLOW-NEW T)))))))
  (COND (ERROR-P
         (FORMAT T "~&Warning: parsing conflict at state ~D for ~A, ambiguous grammar.
 Between " OFFSET SYMBOL)
         (PRINT-PARSE-ACTION OLD-TYPE OLD-ARG)
         (FORMAT T " and ")
         (PRINT-PARSE-ACTION NEW-TYPE NEW-ARG)
         (FORMAT T ".~% Preferring the ~:[latter~;former~] (left-associativity).~%"
                 (NOT ALLOW-NEW))
         (AND (EQ ERROR-P T) (FERROR NIL "Unresolvable parsing conflict."))))
  (EQ ALLOW-NEW T-FOR-NEW))

;;; Return the precedence of an action
(DEFUN PARSE-ACTION-PRECEDENCE (TYPE ARG SYMBOL GRAMMAR &AUX TERMINAL)
  (SETQ TERMINAL (SELECTQ TYPE
                   (SHIFT
                    SYMBOL)
                   (REDUCE
                    (OR (PARSE-PRODUCTION-PRECEDENCE ARG)
                        (LOOP FOR SYMBOL IN (PARSE-PRODUCTION-OUTPUT ARG)
                              WITH TERMINALS = (PARSE-GRAMMAR-TERMINALS GRAMMAR)
                              AND TEM
                              WHEN (MEMQ SYMBOL TERMINALS)
                              DO (SETQ TEM SYMBOL)
                              FINALLY (RETURN TEM))))))
  (LOOP WITH PRECEDENCE-LIST = (PARSE-GRAMMAR-PRECEDENCE-LIST GRAMMAR)
        FOR TERMINALS IN PRECEDENCE-LIST
        FOR I DOWNFROM (LENGTH PRECEDENCE-LIST)
        WHEN (IF (LISTP TERMINALS) (MEMQ TERMINAL TERMINALS) (EQ TERMINAL TERMINALS))
        DO (RETURN I)
        FINALLY (RETURN 0)))

(DEFUN PRINT-PARSE-ACTION (TYPE ARG)
  (SELECTQ TYPE
    (SHIFT
     (FORMAT T "Shift ~D" ARG))
    (REDUCE
     (FORMAT T "Reduce ~A  ~S" (PARSE-PRODUCTION-SYMBOL ARG) (PARSE-PRODUCTION-OUTPUT ARG)))
    (OTHERWISE
     (FORMAT T "~A~@[ ~S~]" TYPE ARG))))

(DEFUN SYMBOL-DISPATCH (DISPATCH SYMBOL OFFSET &AUX TEM)
  (AND (SETQ TEM (ASSQ SYMBOL DISPATCH))
       (NTH (1+ OFFSET) TEM)))

;;; This builds the ACTIONS and GOTOS tables for GRAMMAR
(DEFUN CONSTRUCT-PARSE-GRAMMAR-TABLES (GRAMMAR
                                       &AUX TERMINALS NONTERMINALS PARSE-ITEMS
                                            ACTIONS GOTOS INITIAL-STATE XSTATES WIDTH
                                            (DEFAULT-CONS-AREA COMPILER:FASD-TEMPORARY-AREA))
  (SETQ TERMINALS (PARSE-GRAMMAR-TERMINALS GRAMMAR)
        PARSE-ITEMS (PARSE-ITEMS GRAMMAR))
  (LOOP FOR L ON PARSE-ITEMS
        AS ITEMS = (CAR L)
        WITH INDEX = 0
        COLLECT (OR (LOOP FOR XL ON PARSE-ITEMS
                          UNTIL (EQ L XL)
                          AS X = (CAR XL)
                          FOR XINDEX FROM 0
                          WHEN (PARSE-CORE-EQUAL X ITEMS)
                          DO (RETURN (NTH XINDEX TEM)))
                    (PROG1 INDEX (SETQ INDEX (1+ INDEX))))
                INTO TEM
        FINALLY (SETQ WIDTH INDEX
                      XSTATES TEM))
  (LOOP FOR SYMBOL IN (PARSE-GRAMMAR-SYMBOLS GRAMMAR)
        UNLESS (MEMQ SYMBOL TERMINALS)
        COLLECT SYMBOL INTO TEM
        FINALLY (SETQ NONTERMINALS TEM
                      ACTIONS (MAKE-SYMBOL-DISPATCH TERMINALS WIDTH)
                      GOTOS (MAKE-SYMBOL-DISPATCH NONTERMINALS WIDTH)))
  (LOOP FOR ITEMS IN PARSE-ITEMS
        WITH TOP-LEVEL-PRODUCTION = (PARSE-GRAMMAR-TOP-LEVEL-PRODUCTION GRAMMAR)
        FOR XSTATE IN XSTATES
        DO (LOOP FOR ITEM IN ITEMS
                 WITH TEM
                 AS PRODUCTION = (PARSE-ITEM-PRODUCTION ITEM)
                 AND POSITION = (PARSE-ITEM-POSITION ITEM)
                 AND TERMINAL = (PARSE-ITEM-TERMINAL ITEM)
                 DO (COND ((EQ PRODUCTION TOP-LEVEL-PRODUCTION))
                          ((NULL POSITION)
                           (SET-SYMBOL-DISPATCH `(REDUCE ,PRODUCTION)
                                                ACTIONS TERMINAL XSTATE GRAMMAR))
                          ((MEMQ (SETQ TEM (CAR POSITION)) TERMINALS)
                           (LET* ((GOTO (PARSE-GOTO ITEMS TEM GRAMMAR))
                                  (OFFSET (FIND-POSITION-IN-LIST-PARSE-SET-EQUAL GOTO
                                                                               PARSE-ITEMS)))
                             (COND (OFFSET
                                    (SETQ OFFSET (NTH OFFSET XSTATES))
                                    (SET-SYMBOL-DISPATCH `(SHIFT ,OFFSET)
                                                         ACTIONS TEM XSTATE GRAMMAR))))))
                    (AND (EQ PRODUCTION TOP-LEVEL-PRODUCTION)
                         (IF (NULL POSITION)
                             (SET-SYMBOL-DISPATCH '(ACCEPT) ACTIONS 'EOF XSTATE GRAMMAR)
                             (SETQ INITIAL-STATE XSTATE))))
           (LOOP FOR SYMBOL IN NONTERMINALS
                 AS GOTO = (PARSE-GOTO ITEMS SYMBOL GRAMMAR)
                 AS OFFSET = (FIND-POSITION-IN-LIST-PARSE-SET-EQUAL GOTO PARSE-ITEMS)
                 WHEN OFFSET
                 DO (SET-SYMBOL-DISPATCH (NTH OFFSET XSTATES) GOTOS SYMBOL XSTATE)))
  ;; Now fill in everything else
  (LOOP FOR TABLE IN ACTIONS
        AS FILL = (IF (MEMQ (CAR TABLE) (PARSE-GRAMMAR-IGNORED-TERMINALS GRAMMAR))
                      '(DISCARD) '(ERROR))
        DO (LOOP FOR X ON (CDR TABLE)
                 WHEN (NULL (CAR X))
                 DO (SETF (CAR X) FILL)))
  (SETF (PARSE-GRAMMAR-ACTIONS GRAMMAR) (COPYTREE ACTIONS WORKING-STORAGE-AREA))
  (SETF (PARSE-GRAMMAR-GOTOS GRAMMAR) (COPYTREE GOTOS WORKING-STORAGE-AREA))
  (SETF (PARSE-GRAMMAR-INITIAL-STATE GRAMMAR) INITIAL-STATE)
  NIL)

;;; FIRST of a list of symbols
(DEFUN PARSE-FIRST (LIST GRAMMAR &AUX RESULT)
  (LOOP FOR SYMBOL IN LIST
        AS FIRST = (PARSE-FIRST-1 SYMBOL GRAMMAR)
        DO (LOOP FOR X IN FIRST
                 WHEN (NOT (NULL X))
                 DO (PUSH* X RESULT))
        ALWAYS (MEMQ 'NIL FIRST)
        FINALLY (PUSH* 'NIL RESULT))
  RESULT)

;;; FIRST of a single SYMBOL
(DEFUN PARSE-FIRST-1 (SYMBOL GRAMMAR)
  (IF (MEMQ SYMBOL (PARSE-GRAMMAR-TERMINALS GRAMMAR))
      (NCONS SYMBOL)
      (LOOP FOR PRODUCTION IN (PARSE-GRAMMAR-PRODUCTIONS GRAMMAR)
            WHEN (EQ (PARSE-PRODUCTION-SYMBOL PRODUCTION) SYMBOL)
            WITH OUTPUT AND RESULT AND TEM
            DO (SETQ OUTPUT (PARSE-PRODUCTION-OUTPUT PRODUCTION))
               (COND ((NULL OUTPUT)
                      (PUSH* 'NIL RESULT))
                     ((MEMQ (SETQ TEM (CAR OUTPUT)) (PARSE-GRAMMAR-TERMINALS GRAMMAR))
                      (PUSH* TEM RESULT))
                     (T
                      (LOOP FOR OSYMBOL IN OUTPUT
                            NEVER (MEMQ OSYMBOL (PARSE-GRAMMAR-TERMINALS GRAMMAR))
                            WHEN (NEQ OSYMBOL SYMBOL)
                            AS FIRST = (PARSE-FIRST-1 OSYMBOL GRAMMAR)
                            DO (LOOP FOR X IN FIRST
                                     WHEN (NOT (NULL X))
                                     DO (PUSH* X RESULT))
                            ALWAYS (MEMQ 'NIL FIRST)
                            FINALLY (PUSH* 'NIL RESULT))))
            FINALLY (RETURN RESULT))))

;;; CLOSURE of a set of items
(DEFUN PARSE-CLOSURE (ITEMS GRAMMAR)
  (LOOP FOR ITEM IN ITEMS
        WITH RESULT
        DO (SETQ RESULT (ADD-TO-PARSE-CLOSURE ITEM RESULT GRAMMAR))
        FINALLY (RETURN RESULT)))

(DEFUN ADD-TO-PARSE-CLOSURE (ITEM RESULT GRAMMAR)
  (COND ((NOT (MEMBER ITEM RESULT))
         (PUSH ITEM RESULT)
         (LOOP FOR PRODUCTION IN (PARSE-GRAMMAR-PRODUCTIONS GRAMMAR)
               WITH SYMBOL = (CAR (PARSE-ITEM-POSITION ITEM))
               AND TERMINALS = (PARSE-FIRST
                                 (APPEND (CDR (PARSE-ITEM-POSITION ITEM))
                                         (NCONS (PARSE-ITEM-TERMINAL
                                                 ITEM)))
                                  GRAMMAR)
               WHEN (EQ SYMBOL (PARSE-PRODUCTION-SYMBOL PRODUCTION))
               DO (LOOP FOR TERMINAL IN TERMINALS
                        DO (SETQ RESULT (ADD-TO-PARSE-CLOSURE
                                         (LIST PRODUCTION
                                               (PARSE-PRODUCTION-OUTPUT PRODUCTION)
                                               TERMINAL)
                                         RESULT GRAMMAR))))))
  RESULT)

;;; Compute possible next states from these
(DEFUN PARSE-GOTO (ITEMS SYMBOL GRAMMAR)
  (PARSE-CLOSURE (LOOP FOR ITEM IN ITEMS
                     WHEN (EQ (CAR (PARSE-ITEM-POSITION ITEM)) SYMBOL)
                     COLLECT (LIST (PARSE-ITEM-PRODUCTION ITEM)
                                   (CDR (PARSE-ITEM-POSITION ITEM))
                                   (PARSE-ITEM-TERMINAL ITEM)))
              GRAMMAR))

;;; Set of LALR(1) items for a grammar
(DEFUN PARSE-ITEMS (GRAMMAR)
  (LET ((PRODUCTION (PARSE-GRAMMAR-TOP-LEVEL-PRODUCTION GRAMMAR)))
    (ADD-PARSE-ITEMS (PARSE-CLOSURE (LIST (LIST PRODUCTION
                                            (PARSE-PRODUCTION-OUTPUT PRODUCTION)
                                            'EOF))
                               GRAMMAR)
                   NIL GRAMMAR)))

(DEFUN ADD-PARSE-ITEMS (ITEMS RESULT GRAMMAR)
  (COND ((NOT (MEM #'PARSE-SET-EQUAL ITEMS RESULT))
         (PUSH ITEMS RESULT)
         (LOOP FOR SYMBOL IN (PARSE-GRAMMAR-SYMBOLS GRAMMAR)
               AS GOTO = (PARSE-GOTO ITEMS SYMBOL GRAMMAR)
               WHEN (NOT (NULL GOTO))
               DO (SETQ RESULT (ADD-PARSE-ITEMS GOTO RESULT GRAMMAR)))))
  RESULT)

(DEFUN PARSE-SET-EQUAL (ITEMS1 ITEMS2)
  (AND (= (LENGTH ITEMS1) (LENGTH ITEMS2))
       (LOOP FOR ITEM IN ITEMS1
             ALWAYS (MEMBER ITEM ITEMS2))))

(DEFUN FIND-POSITION-IN-LIST-PARSE-SET-EQUAL (ITEMS LIST)
  (LOOP FOR X IN LIST
        FOR INDEX FROM 0
        WHEN (PARSE-SET-EQUAL X ITEMS)
        DO (RETURN INDEX)))

;;; Do these two sets of items differ only in associated terminals?
(DEFUN PARSE-CORE-EQUAL (ITEMS1 ITEMS2)
  (AND (PARSE-CORE-EQUAL-1 ITEMS1 ITEMS2)
       (PARSE-CORE-EQUAL-1 ITEMS2 ITEMS1)))

(DEFUN PARSE-CORE-EQUAL-1 (ITEMS1 ITEMS2)
  (LOOP FOR ITEM1 IN ITEMS1
        AS PRODUCTION = (PARSE-ITEM-PRODUCTION ITEM1)
        AND POSITION = (PARSE-ITEM-POSITION ITEM1)
        ALWAYS (LOOP FOR ITEM2 IN ITEMS2
                     THEREIS (AND (EQUAL PRODUCTION (PARSE-ITEM-PRODUCTION ITEM2))
                                  (EQUAL POSITION (PARSE-ITEM-POSITION ITEM2))))))

(DEFMACRO DEFINE-PARSE-GRAMMAR (NAME &BODY OPTIONS)
  (IF (AND COMPILER:QC-FILE-IN-PROGRESS (NOT COMPILER:QC-FILE-LOAD-FLAG))
      `(EVAL-WHEN (COMPILE)
         (REMPROP ',NAME 'COMPILE-PARSE-GRAMMAR)
         (DEFINE-PARSE-GRAMMAR-1 ',NAME ',(COPYLIST OPTIONS) 'COMPILE-PARSE-GRAMMAR))
      `(DEFINE-PARSE-GRAMMAR-1 ',NAME ',(COPYLIST OPTIONS) 'PARSE-GRAMMAR)))

(DEFUN DEFINE-PARSE-GRAMMAR-1 (NAME OPTIONS PROPNAME &AUX GRAMMAR NEW-P)
  (COND ((NULL (SETQ GRAMMAR (GET NAME PROPNAME)))
         (SETQ NEW-P T)
         (PUTPROP NAME (SETQ GRAMMAR (MAKE-PARSE-GRAMMAR NAME NAME))
                  PROPNAME)
         (LET ((TOP-LEVEL-PRODUCTION (MAKE-PARSE-PRODUCTION SYMBOL (GENSYM)
                                                            OUTPUT (NCONS NAME)
                                                            FUNCTION 'IDENTITY)))
           (SETF (PARSE-GRAMMAR-TOP-LEVEL-PRODUCTION GRAMMAR) TOP-LEVEL-PRODUCTION)
           (SETF (PARSE-GRAMMAR-PRODUCTIONS GRAMMAR) (LIST TOP-LEVEL-PRODUCTION)))))
  (LOOP FOR OPTION IN OPTIONS
        AS TYPE = (IF (LISTP OPTION) (CAR OPTION) OPTION)
        DO (SELECTQ TYPE
             (:LEXER
              (SETF (PARSE-GRAMMAR-LEXER GRAMMAR) (CADR OPTION)))
             (:LEXEMES
              (LET ((LEXEMES (CDR OPTION)))
                (SETF (PARSE-GRAMMAR-SYMBOLS GRAMMAR)
                      (IF NEW-P LEXEMES
                          (NCONC (LOOP WITH OLD = (PARSE-GRAMMAR-SYMBOLS GRAMMAR)
                                       FOR X IN LEXEMES
                                       WHEN (NOT (MEMQ X OLD))
                                       COLLECT X)
                                 (PARSE-GRAMMAR-SYMBOLS GRAMMAR))))
                (PUSH* 'EOF LEXEMES)
                (SETF (PARSE-GRAMMAR-TERMINALS GRAMMAR) LEXEMES)))
             (:LEFT-ASSOCIATIVE
              (SETF (PARSE-GRAMMAR-LEFT-ASSOCIATIVE-TERMINALS GRAMMAR)
                    (IF (LISTP OPTION) (CDR OPTION) T)))
             (:RIGHT-ASSOCIATIVE
              (SETF (PARSE-GRAMMAR-RIGHT-ASSOCIATIVE-TERMINALS GRAMMAR)
                    (IF (LISTP OPTION) (CDR OPTION) T)))
             (:NON-ASSOCIATIVE
              (SETF (PARSE-GRAMMAR-NON-ASSOCIATIVE-TERMINALS GRAMMAR)
                    (IF (LISTP OPTION) (CDR OPTION) T)))
             (:IGNORED
              (SETF (PARSE-GRAMMAR-IGNORED-TERMINALS GRAMMAR) (CDR OPTION)))
             (:PRECEDENCES
              (SETF (PARSE-GRAMMAR-PRECEDENCE-LIST GRAMMAR) (CDR OPTION)))
             (OTHERWISE
              (FERROR NIL "~S is not a known option" OPTION))))
  NAME)

(DEFUN IDENTITY (X) X)

(DEFMACRO ADD-PARSE-GRAMMAR-PRODUCTION ((NAME GRAMMAR) OUTPUT FUNCTION &BODY OPTIONS)
  (IF (AND COMPILER:QC-FILE-IN-PROGRESS (NOT COMPILER:QC-FILE-LOAD-FLAG))
      `(EVAL-WHEN (COMPILE)
         (ADD-PARSE-GRAMMAR-PRODUCTION-1 ',NAME ',GRAMMAR 'COMPILE-PARSE-GRAMMAR ',OUTPUT
                                         ',FUNCTION ',(COPYLIST OPTIONS)))
      `(ADD-PARSE-GRAMMAR-PRODUCTION-1 ',NAME ',GRAMMAR 'PARSE-GRAMMAR ',OUTPUT ',FUNCTION
                                       ',(COPYLIST OPTIONS))))

(DEFUN ADD-PARSE-GRAMMAR-PRODUCTION-1 (NAME GRAMMAR-NAME PROPNAME OUTPUT FUNCTION OPTIONS
                                       &AUX PRODUCTION GRAMMAR)
  (OR (SETQ GRAMMAR (GET GRAMMAR-NAME PROPNAME))
      (FERROR NIL "~A is not the name of a parse grammar" GRAMMAR-NAME))
  (PUSH* NAME (PARSE-GRAMMAR-SYMBOLS GRAMMAR))
  (COND ((NULL (SETQ PRODUCTION (LOOP FOR PRODUCTION IN (PARSE-GRAMMAR-PRODUCTIONS GRAMMAR)
                                      WHEN (AND (EQ (PARSE-PRODUCTION-SYMBOL PRODUCTION)
                                                    NAME)
                                                (EQUAL (PARSE-PRODUCTION-OUTPUT PRODUCTION)
                                                       OUTPUT))
                                      DO (RETURN PRODUCTION))))
         (SETQ PRODUCTION (MAKE-PARSE-PRODUCTION SYMBOL NAME
                                                 OUTPUT OUTPUT))
         (PUSH PRODUCTION (PARSE-GRAMMAR-PRODUCTIONS GRAMMAR))))
  (SETF (PARSE-PRODUCTION-FUNCTION PRODUCTION) FUNCTION)
  (LOOP FOR OPTION IN OPTIONS
        DO (SELECTQ (CAR OPTION)
             (:PRECEDENCE
              (SETF (PARSE-PRODUCTION-PRECEDENCE PRODUCTION) (CADR OPTION)))
             (OTHERWISE
              (FERROR NIL "~S is not a known option" OPTION)))))

(DEFVAR *PARSE-TRACE-P* NIL)

;;; Main parsing LALR function
;;; LIST is a list of lexeme-lists
(DEFUN PARSE-DRIVER (LIST GRAMMAR ERROR-P)
  (OR (PARSE-GRAMMAR-INITIAL-STATE GRAMMAR)
      (CONSTRUCT-PARSE-GRAMMAR-TABLES GRAMMAR))
  (LOOP WITH STATE = (PARSE-GRAMMAR-INITIAL-STATE GRAMMAR)
        WITH STACK = (NCONS STATE)
        AND ACTIONS = (PARSE-GRAMMAR-ACTIONS GRAMMAR)
        AND GOTOS = (PARSE-GRAMMAR-GOTOS GRAMMAR)
        AS TOKEN = (CAR LIST)
        AS ACTION = (SYMBOL-DISPATCH ACTIONS (CAR TOKEN) STATE)
        AS TYPE = (CAR ACTION)
        AND VALUE = (CADR ACTION)
        DO (COND (*PARSE-TRACE-P*
                  (LET ((PRINLENGTH 2) (PRINLEVEL 2))
                    (FORMAT T "~&~D, ~S: " STATE TOKEN))
                  (PRINT-PARSE-ACTION TYPE VALUE)
                  (FORMAT T "~%")))
           (SELECTQ TYPE
             (ERROR
              (RETURN (FUNCALL (IF ERROR-P #'FERROR #'FORMAT) NIL "Bad token ~S" TOKEN)))
             (ACCEPT
              (RETURN (EVAL (CADR STACK))))
             (DISCARD
              (SETQ LIST (CDR LIST)))
             (SHIFT
              (PUSH `',TOKEN STACK)
              (PUSH VALUE STACK)
              (SETQ STATE VALUE
                    LIST (CDR LIST)))
             (REDUCE
              (LOOP FOR FOO IN (PARSE-PRODUCTION-OUTPUT VALUE)
                    WITH ARGS = NIL
                    DO (POP STACK)
                    (PUSH (POP STACK) ARGS)
                    FINALLY (PUSH (CONS (PARSE-PRODUCTION-FUNCTION VALUE) ARGS)
                                  STACK)
                    (SETQ STATE (SYMBOL-DISPATCH GOTOS (PARSE-PRODUCTION-SYMBOL VALUE)
                                                 (CADR STACK)))
                    (PUSH STATE STACK))))))

(DEFUN PARSE (STRING GRAMMAR &OPTIONAL (START 0) END (ERROR-P T)
                             &AUX PARSE-GRAMMAR LEXER LEXEMES)
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (OR (SETQ PARSE-GRAMMAR (GET GRAMMAR 'PARSE-GRAMMAR))
      (FERROR NIL "~A is not the name of a parse grammar" GRAMMAR))
  (OR (SETQ LEXER (PARSE-GRAMMAR-LEXER PARSE-GRAMMAR))
      (FERROR NIL "No lexer for ~S" PARSE-GRAMMAR))
  (SETQ LEXEMES (FUNCALL LEXER STRING START END ERROR-P))
  (IF (STRINGP LEXEMES)                         ;An error
      LEXEMES
      (PARSE-DRIVER LEXEMES PARSE-GRAMMAR ERROR-P)))

(DEFMACRO BUILD-PARSE-GRAMMAR (GRAMMAR)
  (IF (AND COMPILER:QC-FILE-IN-PROGRESS (NOT COMPILER:QC-FILE-LOAD-FLAG))
      (LET ((PARSE-GRAMMAR (GET GRAMMAR 'COMPILE-PARSE-GRAMMAR)))
        (CONSTRUCT-PARSE-GRAMMAR-TABLES PARSE-GRAMMAR)
        `(PUTPROP ',GRAMMAR ',PARSE-GRAMMAR 'PARSE-GRAMMAR))
      `(CONSTRUCT-PARSE-GRAMMAR-TABLES (GET ',GRAMMAR 'PARSE-GRAMMAR))))

;;; Hey, Zwei, this file contains -*- Mode: Lisp; Package: User; Base: 10. ; -*-

;;;; Version of LLOGO;DEFINE for the Lisp machine.

(SSTATUS FEATURE DEFINE)

(SETQ NO-VALUE '?)

(EVAL-WHEN
 (EVAL LOAD COMPILE)

 (DEFVAR GENSYM (GENSYM))
 (DEFVAR INSIDE-OPEN-BRACKET? NIL)

 (DEFUN OPEN-BRACKET-MACRO NIL
        (COND ((EVAL (READ)) NIL)
              (T (DO ((STUFF (READ) (READ))
                      (INSIDE-OPEN-BRACKET? T))
                     ((EQ STUFF GENSYM)))
                 NIL))
        NIL)

 (SETSYNTAX 91. 'SPLICING 'OPEN-BRACKET-MACRO)

 (DEFUN CLOSE-BRACKET-MACRO ()
        (COND (INSIDE-OPEN-BRACKET? (LIST GENSYM))
              (NIL)))

 (SETSYNTAX 93. 'SPLICING 'CLOSE-BRACKET-MACRO))

(DEFMACRO INCREMENT (VARIABLE &OPTIONAL (BY 1.))
          `(SETF ,VARIABLE (+ ,VARIABLE ,BY)))

(DEFMACRO DECREMENT (VARIABLE &OPTIONAL (BY 1.))
          `(SETF ,VARIABLE (- ,VARIABLE ,BY)))

(DEFMACRO BITWISE-AND (&REST ARGS) `(BOOLE 1. ,@ARGS))
(DEFMACRO BITWISE-OR (&REST ARGS) `(BOOLE 7. ,@ARGS))
(DEFMACRO BITWISE-NOT (&REST ARGS) `(BOOLE 6. -1. ,@ARGS))
(DEFMACRO BITWISE-XOR (&REST ARGS) `(BOOLE 6. ,@ARGS))
(DEFMACRO BITWISE-ANDC (&REST ARGS) `(BOOLE 2. ,@ARGS))

(DEFMACRO REPEAT (REPEAT-ITERATIONS . REPEAT-BODY)
          `(DO ((REPEAT-COUNT 1. (1+ REPEAT-COUNT)))
               ((> REPEAT-COUNT ,REPEAT-ITERATIONS))
               . ,REPEAT-BODY))

(DEFMACRO CCONS (&REST ARGS) `(LIST* ,@ARGS))

(DEFMACRO DEFINE (NAME . DEFINITION)
          (LET ((CLAUSE-NAMES '(ABB SYN)))
               (LET ((CLAUSES (DO ((CLAUSES NIL))
                                  ((NOT (AND (LISTP (CAR DEFINITION))
                                             (MEMQ (CAAR DEFINITION) CLAUSE-NAMES)))
                                   CLAUSES)
                                  (PUSH (CAR DEFINITION) CLAUSES)
                                  (POP DEFINITION))))
                    `(PROGN 'COMPILE
                            ,@ (COND ((ASSQ 'SYN CLAUSES)
                                      (MAPCAR '(LAMBDA (ABB) `(FSET ',NAME ',ABB))
                                              (CDR (ASSQ 'SYN CLAUSES))))
                                     (`((DEFUN ,NAME ,@ DEFINITION))))
                            ,@ (COND ((ASSQ 'ABB CLAUSES)
                                      (MAPCAR '(LAMBDA (ABB) `(FSET ',ABB ',NAME))
                                              (CDR (ASSQ 'ABB CLAUSES)))))))))

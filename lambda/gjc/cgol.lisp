;;; -*- Mode:LISP; Package:CGOL; Base:10 -*-

(SPECIAL CIBASE TOKEN STRINGNUD SYNTAX-NEEDED DRBP FUN
                  DENTYPE ISFUN SILENCE DEFBP IVARS WHENVAR RESULT
                  BODY NUDL LEDL LBPL CNUD CLED CLBP LANGUAGE_ALIST
                  ARITHMETIC_ALIST)

(PROGN 'COMPILE
       (DEFUN (ADVANCE NUD) NIL (LIST (PROG2 NIL 'ADVANCE)))
       (DEFUN ADVANCE NIL
         (SETQ STRINGNUD NIL)
         (SETQ TOKEN (CGOLTOKEN))
         TOKEN))

(DEFUN VERIFY (DEN) (COND (DEN (ADVANCE) DEN)))
(PROGN
 'COMPILE
 (DEFUN (NUDERR NUD) NIL (LIST (PROG2 NIL 'NUDERR)))
 (DEFUN NUDERR NIL
   (COND
    ((AND (GETDEN LBPL) (NULL (FUNBOUNDP TOKEN)))
     (CGOLERR (CAT TOKEN '| MISSING PRECEDING EXPRESSION|)
              2.
              T))
    (T
     ((LAMBDA (OP TP)
        (ADVANCE)
        (LIST 'LAMBDA
              NIL
              (LIST 'QUOTE
                    (COND ((AND (FUNBOUNDP OP)
                                (MEMBER TP '(9. 13. 32.))
                                (OR STRINGNUD
                                    (AND (GETDEN NUDL)
                                         (NOT (EQUAL TOKEN
                                                     '|(|)))
                                    (NOT (GETDEN LBPL))))
                           (LIST OP
                                 (PARSE (OR (GET OP 'RBP)
                                            25.))))
                          (T OP)))))
      TOKEN
      (CGOLTYIPEEK))))))

(PROGN 'COMPILE
       (DEFUN (FUNBOUNDP NUD) NIL
         (LIST (PROG2 NIL 'FUNBOUNDP) (PROG2 NIL (PARSE 25.))))
       (DEFUN FUNBOUNDP (X)
         (AND (SYMBOLP X)
              (OR (GETL X
                        '(SUBR FSUBR LSUBR EXPR FEXPR LEXPR MACRO
                          *EXPR *FEXPR *LEXPR AUTOLOAD))
                  (FBOUNDP X)))))
(PROGN
 'COMPILE
 (DEFUN (LEDERR NUD) NIL (LIST (PROG2 NIL 'LEDERR)))
 (DEFUN LEDERR NIL
   (CGOLERR (CAT TOKEN
                 '| IS NOT AN OPERATOR WITH A LEFT ARGUMENT|)
            2.
            T)))

(PROGN 'COMPILE
       (DEFUN (GETDEN NUD) NIL
         (LIST (PROG2 NIL 'GETDEN) (PROG2 NIL (PARSE 25.))))
       (DEFUN GETDEN (INDL)
         (AND INDL (OR (GET TOKEN (CAR INDL)) (GETDEN (CDR INDL))))))

(PROGN 'COMPILE
       (DEFUN (NUD NUD) NIL (LIST (PROG2 NIL 'NUD)))
       (DEFUN NUD NIL
         (OR (VERIFY (OR STRINGNUD
                         (COND ((NUMBERP TOKEN)
                                (LIST 'LAMBDA NIL TOKEN))
                               (T (GETDEN NUDL)))))
             (NUDERR))))

(PROGN 'COMPILE
       (DEFUN (LED NUD) NIL (LIST (PROG2 NIL 'LED)))
       (DEFUN LED NIL (OR (VERIFY (GETDEN LEDL)) (LEDERR))))

(PROGN 'COMPILE
       (DEFUN (PARSE NUD) NIL
         (LIST (PROG2 NIL 'PARSE) (PROG2 NIL (PARSE 25.))))
       (DEFUN PARSE (RBP)
         (DO ((TRANSLATION (FUNCALL (NUD))
                           (FUNCALL (LED) TRANSLATION)))
             ((NOT (LESSP RBP (OR (GETDEN LBPL) 0.))) TRANSLATION)
          NIL)))

(PUTPROP ' (MINUS 1.) 'LBP)


(DEFUN SPEAK (X)
  ((LAMBDA (LANG)
     (COND (LANG (SETQ LANG (CDR LANG)))
           (T (CGOLERR (CAT X '| is an unknown language|)
                       3.
                       T)))
     (SETQ NUDL (CONS (CAR LANG) NUDL))
     (SETQ LEDL (CONS (CADR LANG) LEDL))
     (SETQ LBPL (CONS (CADDR LANG) LBPL))
     NIL)
   (ASSOC X LANGUAGE_ALIST)))

(PROGN 'COMPILE
       (DEFUN (FORGET NUD) NIL (LIST (PROG2 NIL 'FORGET)))
       (DEFUN FORGET NIL
         (AND (CDR NUDL)
              (PROGN (SETQ NUDL (CDR NUDL))
                     (SETQ LEDL (CDR LEDL))
                     (SETQ LBPL (CDR LBPL))))
         NIL))

(PROGN 'COMPILE
       (DEFUN (RESETLANGUAGE NUD) NIL
         (LIST (PROG2 NIL 'RESETLANGUAGE)))
       (DEFUN RESETLANGUAGE NIL
         (SETQ NUDL '(NUD))
         (SETQ LEDL '(LED))
         (SETQ LBPL '(LBP))
         (SETQ CNUD 'NUD)
         (SETQ CLED 'LED)
         (SETQ CLBP 'LBP)
         NIL))

(DEFUN LEARN (X)
  ((LAMBDA (LANG)
     (COND (LANG (SETQ LANG (CDR LANG)))
           (T (SETQ LANG (LIST (CAT X 'NUD)
                               (CAT X 'LED)
                               (CAT X 'LBP)))
              (SETQ LANGUAGE_ALIST (CONS (CONS X LANG)
                                         LANGUAGE_ALIST))))
     (SETQ CNUD (CAR LANG))
     (SETQ CLED (CADR LANG))
     (SETQ CLBP (CADDR LANG))
     (LIST 'OR
           (LIST* 'ASSOC
                  (LIST 'QUOTE X)
                  '(LANGUAGE_ALIST))
           (LIST* 'PUSH
                  (LIST 'QUOTE (LIST* X LANG))
                  '(LANGUAGE_ALIST))))
   (ASSOC X LANGUAGE_ALIST)))

(DEFUN (RIGHT NUD) NIL (LIST 'PARSE DRBP))

(DEFUN (RIGHTLIST NUD) NIL
  (LIST 'PARSELIST DRBP ''/,))

(DEFUN (RIGHTREP NUD) NIL
  (LIST 'PARSELIST DRBP (LIST 'QUOTE FUN)))
(DEFUN DEFFIX (DENTYPE ISFUN FUN DLBP DRBP)
  ((LAMBDA (FORM)
     (COND (DLBP (SETQ FORM
                       (LIST 'PROGN
                             ''COMPILE
                             FORM
                             (LIST 'DEFPROP FUN DLBP CLBP)))))
     (COND (SYNTAX-NEEDED (EVAL FORM)))
     FORM)
   (CONS 'DEFUN
         (CONS (LIST FUN DENTYPE)
               (CONS (COND ((EQUAL DENTYPE CLED) '(LEFT)))
                     (PROGN (ADVANCE) (DEPROGNIFY (PARSE 0.))))))))

(DEFUN (NILFIX NUD) NIL (DEFFIX CNUD 'ISN TOKEN NIL NIL))

(DEFUN (PREFIX NUD) NIL (DEFFIX CNUD 'ISP TOKEN NIL (ADVANCE)))

(DEFUN (SUFFIX NUD) NIL (DEFFIX CLED 'ISS TOKEN (ADVANCE) NIL))

(DEFUN (INFIX NUD) NIL
  (DEFFIX CLED 'ISI TOKEN (ADVANCE) TOKEN))

(DEFUN (INFIXR NUD) NIL
  (DEFFIX CLED 'ISI TOKEN (ADVANCE) (DIFFERENCE TOKEN 1.)))

(DEFUN (INFIXD NUD) NIL
  (DEFFIX CLED 'ISI TOKEN (ADVANCE) (ADVANCE)))

(DEFUN (INFIXM NUD) NIL
  (DEFFIX CLED 'ISM TOKEN (ADVANCE) TOKEN))

(DEFUN (DELIM NUD) NIL
  ((LAMBDA (FORM) (COND (SYNTAX-NEEDED (EVAL FORM))) FORM)
   (CONS 'PROGN
         (MAPCAR
          (FUNCTION (LAMBDA (I) (LIST 'DEFPROP I 0. CLBP)))
          (GETVARLIST)))))

(DEFUN (IS NUD) NIL
  (CONS ISFUN
        (APPEND (COND ((EQUAL DENTYPE CLED) '(LEFT)))
                (LIST (PARSE 25.))
                (COND (DRBP (LIST DRBP)))
                (COND ((EQUAL ISFUN 'ISM)
                       (LIST (LIST 'QUOTE FUN)))))))

(DEFUN ISN (FCN) (LIST FCN))

(DEFUN ISS (LEFT FCN) (LIST FCN LEFT))

(DEFUN ISP (FCN RB) (LIST FCN (PARSE RB)))

(DEFUN ISI (LEFT FCN RB) (LIST FCN LEFT (PARSE RB)))

(DEFUN ISM (LEFT FCN RB CONT)
  (CONS FCN (CONS LEFT (PARSELIST RB CONT))))
(PROGN 'COMPILE
       (DEFUN (CHECK NUD) NIL
         (LIST (PROG2 NIL 'CHECK) (PROG2 NIL (PARSE 25.))))
       (DEFUN CHECK (DEL)
         (COND ((OR (EQUAL TOKEN DEL)
                    (AND (NOT (ATOM DEL)) (MEMBER TOKEN DEL)))
                (ADVANCE))
               (T (CGOLERR (CAT '|MISSING |
                                DEL
                                '| INSERTED BEFORE |
                                TOKEN)
                           0.
                           NIL)))))

(DEFUN CAT N
  (IMPLODE (APPLY (FUNCTION APPEND)
                  (MAPCAR (FUNCTION EXPLODEC)
                          (MAPCAR (FUNCTION ARG) (TO 1. N 1.))))))

(DEFUN PARSELIST (RB CONT)
  (CONS (PARSE RB)
        (COND ((EQ TOKEN CONT) (ADVANCE) (PARSELIST RB CONT)))))

(PROGN 'COMPILE
       (DEFUN (GETVARLIST NUD) NIL
         (LIST (PROG2 NIL 'GETVARLIST)))
       (DEFUN GETVARLIST NIL
         (COND ((OR (NOT (EQUAL TOKEN '/;)) STRINGNUD)
                (CONS (PROG2 NIL TOKEN (ADVANCE))
                      (COND ((EQUAL TOKEN '/,)
                             (ADVANCE)
                             (GETVARLIST))))))))

(PROGN 'COMPILE
       (DEFUN (GETTOKENS NUD) NIL
         (LIST (PROG2 NIL 'GETTOKENS)))
       (DEFUN GETTOKENS NIL
         (COND ((NOT (MEMBER TOKEN '(|)| ] /'  /;)))
                (CONS (PROG2 NIL TOKEN (ADVANCE)) (GETTOKENS))))))

(DEFUN DEPROGNIFY (X)
  (COND ((AND (NOT (ATOM X)) (EQUAL (CAR X) 'PROGN)) (CDR X))
        (T (LIST X))))

(PROGN 'COMPILE
       (DEFUN (NOTIFY NUD) NIL
         (LIST (PROG2 NIL 'NOTIFY) (PROG2 NIL (PARSE 25.))))
       (DEFUN NOTIFY (X)
         (AND (NOT (EQUAL X T))
              (COND ((AND (NOT (ATOM X)) (EQUAL (CAR X) 'NOT))
                     (CADR X))
                    (T (LIST 'NOT X))))))
(PROGN 'COMPILE
       (DEFUN (ORIFY NUD) NIL
         (LIST (PROG2 NIL 'ORIFY) (PROG2 NIL (PARSE 25.))))
       (DEFUN ORIFY (X)
         (AND X
              (COND ((AND (NOT (ATOM X)) (NULL (CDR X))) (CAR X))
                    (T (CONS 'OR X))))))

(DEFUN LITERAL FEXPR (X) (MAPC (FUNCTION (LAMBDA (I) (SET I I))) X))

(PROGN 'COMPILE
       (DEFUN (ARITH NUD) NIL
         (LIST (PROG2 NIL 'ARITH) (PROG2 NIL (PARSE 25.))))
       (DEFUN ARITH (X)
         (COND ((SETQ IT (ASSOC X ARITHMETIC_ALIST)) (CDR IT))
               (T X))))

(DEFUN (DEFINE NUD) NIL
  (PROG (FUN TYPE ARGTS CODE INSTR LB RB EXPR FORM)
        (SETQ EXPR (COND ((MEMBER TOKEN
                                  '(EXPR FEXPR LEXPR MACRO))
                          (PROG2 NIL TOKEN (ADVANCE)))
                         (T 'EXPR)))
        (COND ((OR STRINGNUD (EQUAL (CGOLTYIPEEK) 40.))
               (SETQ ARGTS NIL)
               (SETQ TYPE CNUD)
               (SETQ CODE (LIST 'LIST))
               (SETQ INSTR (LIST 'PROG2
                                 NIL
                                 (LIST 'QUOTE TOKEN))))
              (T (SETQ ARGTS (LIST TOKEN))
                 (ADVANCE)
                 (SETQ TYPE CLED)
                 (SETQ CODE (LIST 'LIST
                                  (LIST 'QUOTE TOKEN)))
                 (SETQ INSTR (LIST 'PROG2 NIL 'LEFT))))
        (SETQ FUN TOKEN)
        (ADVANCE)
        (COND
         ((AND (EQUAL TOKEN '|(|) (NOT STRINGNUD))
          (ADVANCE)
          (SETQ ARGTS (COND ((NOT (EQUAL TOKEN '|)|))
                             (GETVARLIST))))
          (COND ((EQUAL EXPR 'LEXPR)
                 (SETQ ARGTS (CAR ARGTS))
                 (SETQ EXPR 'EXPR)))
          (CHECK '|)|)
          (SETQ CODE NIL)
          (SETQ INSTR NIL))
         (T (DO NIL
                ((NOT (OR (NOT (OR (EQUAL TOKEN '/;)
                                   (EQUAL TOKEN '/,)))
                          STRINGNUD)))
             (DO NIL ((NOT STRINGNUD))
              (SETQ INSTR (APPEND INSTR
                                  (LIST (LIST 'CHECK
                                              (LIST 'QUOTE
                                                    TOKEN)))))
              (SETQ FORM (CONS (LIST 'DEFPROP TOKEN 0. CLBP) FORM))
              (ADVANCE))
             (SETQ CODE (APPEND CODE (LIST INSTR)))
             (COND ((AND (OR (EQUAL TOKEN '/;)
                             (EQUAL TOKEN '/,))
                         (NOT STRINGNUD))
                    (SETQ INSTR NIL))
                   (T (SETQ INSTR (LIST 'PROG2
                                        NIL
                                        (LIST 'PARSE
                                              '/#RBP)))
                      (SETQ ARGTS (APPEND ARGTS (LIST TOKEN)))
                      (ADVANCE))))))
        (SETQ LB (COND ((EQUAL TYPE CLED)
                        (COND ((EQUAL TOKEN '/,)
                               (ADVANCE)
                               (EVAL (PARSE 1.)))
                              (T DEFBP)))))
        (SETQ RB (COND ((EQUAL TOKEN '/,)
                        (ADVANCE)
                        (EVAL (PARSE 1.)))
                       (T (OR LB DEFBP))))
        (SETQ CODE (SUBST RB
                          '/#RBP
                          (APPEND CODE (COND (INSTR (LIST INSTR))))))
        (CHECK '/;)
        (COND
         (CODE
          (SETQ
           FORM
           (CONS
            'PROGN
            (CONS ''COMPILE
                  (CONS (LIST (PROGN 'DEFUN)
                              (LIST FUN TYPE)
                              (COND ((EQUAL TYPE CLED)
                                     '(LEFT)))
                              CODE)
                        (APPEND (COND (LB (LIST (LIST 'DEFPROP
                                                      FUN
                                                      LB
                                                      CLBP))))
                                (NREVERSE FORM))))))
          (COND (SYNTAX-NEEDED (EVAL FORM)))))
        (COND
         ((NOT (EQUAL TOKEN '))
          (SETQ
           FORM
           (APPEND
            FORM
            (LIST (CONS (PROGN 'DEFUN)
                        (CONS FUN
                              (APPEND (COND ((NOT (EQUAL EXPR
                                                         'EXPR))
                                             (LIST EXPR)))
                                      (LIST ARGTS)
                                      (DEPROGNIFY (PARSE 0.))))))))))
        (RETURN (COND (CODE FORM) (T (CAR FORM))))))

(SETQ DEFBP 25.)
(INITIALIZE-MULTI-CHARACTER-TOKEN-TABLE
 '|-+#&'()*,//:;<=>@[\]^`{/|}~!|)

(DEFUN DEFTOK FEXPR (A) (MAPC (FUNCTION PUTTOK) A))

(DEFUN (NEWTOK NUD) NIL
  ((LAMBDA (FORM) (COND (SYNTAX-NEEDED (EVAL FORM))) FORM)
   (CONS 'DEFTOK (GETVARLIST))))

(DEFUN (|(| NUD) NIL (PROG2 NIL (PARSE 0.) (CHECK '|)|)))

(PROGN (DEFPROP |)| 0. LBP))

(PROGN 'COMPILE
       (DEFUN (|(| LED) (LEFT)
         (PROG2 NIL
                (CONS LEFT
                      (COND ((NOT (EQUAL TOKEN '|)|))
                             (PARSELIST 0. '/,))))
                (CHECK '|)|)))
       (DEFPROP |(| 30. LBP))

(PROGN (DEFPROP /, 0. LBP))

(PROGN 'COMPILE
       (DEFUN ({ LED) (LEFT)
         (PROG2 NIL
                (CONS 'APPLY
                      (CONS (LIST 'FUNCTION LEFT)
                            (PARSELIST 0. '/,)))
                (CHECK '})))
       (DEFPROP { 30. LBP))

(PROGN (DEFPROP } 0. LBP))

(DEFUN ([ NUD) NIL
  (PROG2 NIL
         (COND ((NOT (EQUAL TOKEN ']))
                ((LAMBDA (A)
                   (COND ((EQUAL TOKEN '|)|)
                          (LIST 'CIRC A))
                         (T A)))
                 (CONS 'LIST (PARSELIST 0. '/,)))))
         (CHECK '(] |)|))))

(DEFUN CIRC (X) (PROG2 NIL X (RPLACD (LAST X) X)))

(PROGN (DEFPROP ] 0. LBP))
(PROGN
 'COMPILE
 (DEFUN ([ LED) (LEFT)
   (PROG2 NIL
          (COND ((EQUAL TOKEN '{)
                 (PROG2 NIL
                        (PROGN (ADVANCE)
                               (SUBLIS (LIST (CONS 'A LEFT)
                                             (CONS 'B
                                                   (PARSE 0.)))
                                       '(APPLY (FUNCTION MAPCAR)
                                               (CONS (FUNCTION A)
                                                     B))))
                        (CHECK '})))
                (T (CONS 'MAPCAR
                         (CONS (LIST 'FUNCTION LEFT)
                               (PARSELIST 0. '/,)))))
          (CHECK '])))
 (DEFPROP [ 30. LBP))

(DEFUN (OCT NUD) NIL
  (PROG2 NIL
         ((LAMBDA (CIBASE) (CHECK '|(|) (PARSE 0.)) 8.)
         (CHECK '|)|)))

(DEFUN (/' NUD) NIL (PROG2 NIL (ISP 'QUOTE 0.) (CHECK '/')))

(PROGN (DEFPROP /' 0. LBP))

(DEFUN (/# NUD) NIL (PROG2 NIL TOKEN (ADVANCE)))

(DEFUN (= NUD) NIL (EVAL (PARSE 25.)))

(DEFUN (\ NUD) NIL
  (PROG2 NIL
         (CONS 'LAMBDA
               (CONS (PROG2 NIL (GETVARLIST) (CHECK '/;))
                     (DEPROGNIFY (PARSE 0.))))
         (COND ((EQUAL TOKEN '\) (ADVANCE)))))

(PROGN (DEFPROP \ 0. LBP))
(DEFUN (LET NUD) NIL
  (PROG (VARS ARGTS PACKFLAG)
        (DO NIL
            ((MEMBER TOKEN '(/; IN)))
         (SETQ VARS (APPEND VARS (GETVARLIST)))
         (CHECK '(BE /:= =))
         (SETQ ARGTS
               (CONS (COND ((EQUAL TOKEN '{)
                            (LIST '&UNP
                                  (PROG2 NIL
                                         (PROGN (ADVANCE) (PARSE 0.))
                                         (PROGN (SETQ PACKFLAG T)
                                                (CHECK '})))))
                           (T (PARSE 1.)))
                     ARGTS))
         (COND ((EQUAL TOKEN '/,) (ADVANCE))))
        (ADVANCE)
        (RETURN
         (COND
          (PACKFLAG
           (SETQ
            ARGTS
            (REVERSE
             (MAPCAR
              (FUNCTION (LAMBDA (I)
                          (COND ((EQUAL (CAR I) '&UNP)
                                 (CADR I))
                                (T (LIST 'LIST I)))))
              ARGTS)))
           (LIST 'APPLY
                 (LIST 'FUNCTION
                       (CONS 'LAMBDA
                             (CONS VARS (DEPROGNIFY (PARSE 0.)))))
                 (COND ((EQUAL (LENGTH ARGTS) 1.) (CAR ARGTS))
                       (T (CONS 'APPEND ARGTS)))))
          (T (CONS (CONS 'LAMBDA
                         (CONS VARS (DEPROGNIFY (PARSE 0.))))
                   (NREVERSE ARGTS)))))))

(DEFUN (PROG NUD) NIL
  (CONS 'PROG
        (CONS (PROG2 NIL (GETVARLIST) (CHECK '/;))
              (DEPROGNIFY (PARSE 0.)))))

(DEFUN (NEW NUD) NIL
  (CONS 'PROG
        (CONS (PROG2 NIL (GETVARLIST) (CHECK '/;))
              ((LAMBDA (X)
                 ((LAMBDA (Y)
                    (RPLACA Y (LIST 'RETURN (CAR Y)))
                    X)
                  (LAST X)))
               (DEPROGNIFY (PARSE 0.))))))

(DEFUN (SPECIAL NUD) NIL
  (LIST 'DECLARE (CONS 'SPECIAL (GETVARLIST))))

(DEFUN (LITERAL NUD) NIL
  (CONS 'LITERAL (PARSELIST 1. '/,)))
(DEFUN CGOLARRAY FEXPR (X)
  (COND ((EQUAL TOKEN '|(|)
         (PROG2 NIL
                (PROGN (ADVANCE)
                       (CONS (CAR X)
                             (MAPCAR (FUNCTION (LAMBDA (Y)
                                                 (LIST 'SUB1
                                                       Y)))
                                     (PARSELIST 0. '/,))))
                (CHECK '|)|)))
        ((EQUAL TOKEN '/:=)
         (ADVANCE)
         (LIST 'FILLARRAY (CAR X) (PARSE 1.)))
        (T (CAR X))))
(DEFUN (ARRAY NUD) NIL
  (COND
   ((MEMBER TOKEN '(|(| { [)) 'ARRAY)
   (T
    ((LAMBDA (NAMES)
       ((LAMBDA (OLDNUDS)
          (PROG2
           NIL
           (PROGN
            (MAPC
             (FUNCTION (LAMBDA (NAME)
                         (PUTPROP NAME
                                  (LIST 'LAMBDA
                                        NIL
                                        (LIST 'CGOLARRAY NAME))
                                  CNUD)))
             NAMES)
            (COND
             ((EQUAL TOKEN '|(|)
              (ADVANCE)
              ((LAMBDA (DIMS)
                 (CHECK '|)|)
                 ((LAMBDA (TYPE)
                    ((LAMBDA (SOURCE)
                       (COND
                        ((EQUAL TOKEN '/;)
                         (ADVANCE)
                         (CONS
                          (CONS
                           'LAMBDA
                           (CONS
                            NAMES
                            (APPEND
                             (COND
                              (SOURCE
                               (MAPCAR
                                (FUNCTION (LAMBDA (NAME)
                                            (LIST 'FILLARRAY
                                                  NAME
                                                  SOURCE)))
                                NAMES)))
                             (DEPROGNIFY (PARSE 0.)))))
                          (MAPCAR
                           (FUNCTION
                            (LAMBDA (NAME)
                              (CONS 'ARRAY
                                    (CONS NIL (CONS TYPE DIMS)))))
                           NAMES)))
                        (T
                         (CONS
                          'PROG2
                          (CONS
                           NIL
                           (CONS
                            (LIST 'QUOTE (CAR NAMES))
                            (MAPCAN
                             (FUNCTION
                              (LAMBDA (NAME)
                                (CONS
                                 (LIST 'DEFPROP
                                       NAME
                                       (GET NAME 'NUD)
                                       'NUD)
                                 (CONS
                                  (LIST 'SETQ
                                        NAME
                                        (CONS 'ARRAY
                                              (CONS NIL
                                                    (CONS TYPE
                                                          DIMS))))
                                  (COND
                                   (SOURCE (LIST (LIST 'FILLARRAY
                                                       NAME
                                                       SOURCE))))))))
                             NAMES)))))))
                     (COND ((MEMBER TOKEN '(/:= =))
                            (ADVANCE)
                            (PARSE 1.)))))
                  (COND ((MEMBER TOKEN '(FIXNUM FLONUM NIL T))
                         (PROG2 NIL TOKEN (ADVANCE)))
                        (T T))))
               (PARSELIST 0. '/,)))
             ((EQUAL TOKEN '/;) (ADVANCE) (PARSE 0.))))
           (MAPC (FUNCTION (LAMBDA (NAME OLDNUD)
                             (COND (OLDNUD (PUTPROP NAME OLDNUD CNUD))
                                   (T (REMPROP NAME CNUD)))))
                 NAMES
                 OLDNUDS)))
        (MAPCAR (FUNCTION (LAMBDA (NAME) (GET NAME CNUD))) NAMES)))
     (GETVARLIST)))))

(DEFUN (DIM NUD) NIL
  (LIST 'CDR (LIST 'ARRAYDIMS (PARSE 25.))))

(PUTPROP 'EVAL 1. 'RBP)

(PROGN 'COMPILE
       (DEFUN (/; LED) (LEFT) (ISM LEFT 'PROGN 1. '/;))
       (DEFPROP /; 1. LBP))

(PROGN 'COMPILE
       (DEFUN (& LED) (LEFT) (LIST 'PROG2 NIL LEFT (PARSE 0.)))
       (DEFPROP & 1. LBP))
(DEFUN (IF NUD) NIL
  (CONS 'COND
        (CONS (CONS (PARSE 2.)
                    (PROGN (CHECK 'THEN)
                           (DEPROGNIFY (PARSE 2.))))
              (COND ((EQ TOKEN 'ELSE)
                     (ADVANCE)
                     ((LAMBDA (X)
                        (COND ((AND (NOT (ATOM X))
                                    (EQUAL (CAR X) 'COND))
                               (CDR X))
                              (T (LIST (CONS T (DEPROGNIFY X))))))
                      (PARSE 2.)))))))

(PROGN (DEFPROP THEN 0. LBP))

(PROGN (DEFPROP ELSE 0. LBP))

(PUTPROP 'RETURN 1. 'RBP)

(PUTPROP 'GO 1. 'RBP)

(DEFUN (WHILE NUD) NIL
  (CONS 'DO
        (CONS NIL
              (CONS (LIST (NOTIFY (PARSE 2.)))
                    (PROGN (CHECK 'DO)
                           (DEPROGNIFY (PARSE 2.)))))))

(DEFUN (REPEAT NUD) NIL
  (LIST 'DO
        NIL
        (LIST (CONS 'PROG2
                    (APPEND (DEPROGNIFY (PARSE 2.))
                            (DEPROGNIFY (PROGN (CHECK 'UNTIL)
                                               (PARSE 2.))))))))

(PROGN (DEFPROP DO 0. LBP))
(DEFUN (FOR NUD) NIL
  (PROG (PARS ARGTS INON FCN BODY)
        (SETQ PARS (LIST TOKEN))
        (SETQ INON (ADVANCE))
        (ADVANCE)
        (SETQ FCN (ASSOC INON
                         '((IN (DO MAPC) (COLLECT MAPCAR) (COALESCE MAPCAN))
                           (ON (DO MAP)
                               (COLLECT MAPLIST)
                               (COALESCE MAPCON)))))
        (COND (FCN (SETQ FCN (CDR FCN)))
              (T (CGOLERR (CAT INON
                               '| FOUND WHERE IN OR ON EXPECTED|)
                          2.
                          T)))
        (SETQ ARGTS (LIST (PARSE 1.)))
        (DO NIL
            ((NOT (EQ TOKEN '/,)))
         (SETQ PARS (CONS (ADVANCE) PARS))
         (ADVANCE)
         (CHECK INON)
         (SETQ ARGTS (CONS (PARSE 1.) ARGTS)))
        (SETQ FCN (ASSOC TOKEN FCN))
        (COND
         (FCN (SETQ FCN (CADR FCN)))
         (T
          (CGOLERR
           (CAT TOKEN
                '| FOUND WHERE DO, COLLECT OR COALESCE EXPECTED|)
           2.
           T)))
        (ADVANCE)
        (SETQ ARGTS (NREVERSE ARGTS))
        (SETQ PARS (NREVERSE PARS))
        (SETQ BODY (PARSE 1.))
        (RETURN
         (COND
          ((AND
            (EQUAL FCN 'MAPC)
            (APPLY (FUNCTION LAND)
                   (MAPCAR (FUNCTION (LAMBDA (X)
                                       (AND (NOT (ATOM X))
                                            (EQUAL (CAR X)
                                                   'TO))))
                           ARGTS)))
           (CONS
            'DO
            (CONS
             (MAPCAR
              (FUNCTION (LAMBDA (P A)
                          (LIST P
                                (CADR A)
                                (COND ((EQUAL (CADDDR A) 1.)
                                       (LIST 'ADD1 P))
                                      (T (LIST 'PLUS
                                               P
                                               (CADDDR A)))))))
              PARS
              ARGTS)
             (CONS
              (LIST
               (ORIFY (MAPCAR (FUNCTION (LAMBDA (P A)
                                          (LIST 'GREATERP
                                                P
                                                (CADDR A))))
                              PARS
                              ARGTS)))
              (DEPROGNIFY BODY)))))
          (T (CONS FCN
                   (CONS (LIST 'FUNCTION
                               (COND ((AND (EQUAL (CDR BODY) PARS)
                                           (ATOM (CAR BODY)))
                                      (CAR BODY))
                                     (T (LIST 'LAMBDA
                                              PARS
                                              BODY))))
                         ARGTS)))))))

(PROGN (PROGN (DEFPROP IN 0. LBP))
       (PROGN (DEFPROP ON 0. LBP))
       (PROGN (DEFPROP COLLECT 0. LBP))
       (PROGN (DEFPROP COALESCE 0. LBP)))
(DEFUN (ITER NUD) NIL
  (PROG (IVARS WHENVAR RESULT BODY)
        (DO NIL
            ((NOT
              (SETQ
               IT
               (ASSOC TOKEN
                      '((FOR (SETQ IVARS (CONS (CONS TOKEN (COND ((EQUAL (
ADVANCE) '/:=) (CONS (PROGN (ADVANCE) (SETQ IT (PARSE 2.))) (COND ((EQUAL
TOKEN 'STEP) (LIST (COND ((EQUAL (ADVANCE) 'DITTO) (ADVANCE) IT) (T (PARSE 2.)
))))))))) IVARS))) (WHEN (SETQ WHENVAR (PARSE 2.)))
                        (UNTIL (SETQ WHENVAR (PARSE 2.)))
                        (WHILE (SETQ WHENVAR (LIST 'NOT
                                                   (PARSE 2.))))
                        (RETURN (SETQ RESULT (PARSE 2.)))
                        (DO (SETQ BODY (PARSE 2.))))))))
         (ADVANCE)
         (EVAL (CADR IT)))
        (COND ((NOT (OR IVARS WHENVAR RESULT BODY))
               (SETQ BODY (PARSE 2.))))
        (RETURN (APPEND (LIST 'DO
                              (NREVERSE IVARS)
                              (LIST WHENVAR RESULT))
                        (COND ((AND (NOT (ATOM BODY))
                                    (EQ (CAR BODY) 'PROGN))
                               (CDR BODY))
                              (T (NCONS BODY)))))))

(PROGN (DEFPROP FOR 0. LBP)
       (DEFPROP WHEN 0. LBP)
       (DEFPROP UNTIL 0. LBP)
       (DEFPROP WHILE 0. LBP)
       (DEFPROP STEP 0. LBP)
       (DEFPROP RETURN 0. LBP))

(PROGN 'COMPILE
       (DEFUN (TO LED) (LEFT)
         (CONS 'TO
               (CONS LEFT
                     (CONS (PARSE 18.)
                           (LIST (COND ((EQUAL TOKEN 'BY)
                                        (ADVANCE)
                                        (PARSE 18.))
                                       (T 1.)))))))
       (DEFPROP TO 18. LBP))

(PROGN (DEFPROP BY 0. LBP))
(DEFUN TO (AA B C)
  (COND ((GREATERP AA B) NIL)
        (T (PROG (X)
                 (RETURN (PROG2 NIL
                                (SETQ X (LIST AA))
                                (DO NIL
                                    ((LESSP B
                                            (SETQ AA (PLUS AA
                                                           C))))
                                 (SETQ X
                                  (CDR (RPLACD X (LIST AA)))))))))))

(PROGN 'COMPILE
       (DEFUN (LOTSOF LED) (LEFT)
         (LIST 'DO
               '*I
               LEFT
               '(DIFFERENCE *I 1.)
               '(NOT (GREATERP *I 0.))
               (PARSE 1.)))
       (DEFPROP LOTSOF 19. LBP))

(DEFTOK /:=)

(PUTPROP 'CGOLPRINT
         '(LAMBDA NIL (LIST 'CGOLPRINT (PARSE 1.)))
         'NUD)

(PUTPROP 'CGOLPRIN1
         '(LAMBDA NIL (LIST 'CGOLPRIN1 (PARSE 1.)))
         'NUD)

(PROGN 'COMPILE
       (DEFUN (/:= LED) (LEFT)
         (COND ((ATOM LEFT) (ISI LEFT 'SETQ 1.))
               ((EQ (CAR LEFT) 'GET)
                (LIST 'PUTPROP
                      (CADR LEFT)
                      (PARSE 1.)
                      (CADDR LEFT)))
               ((SETQ IT (GET (CAR LEFT) 'STOREFORM))
                ((LAMBDA (X)
                   (SUBLIS (LIST (CONS 'LEFT (CADR LEFT))
                                 (CONS 'RIGHT (PARSE 1.)))
                           X))
                 IT))
               (T (ISI LEFT 'STORE 1.))))
       (DEFPROP /:= 25. LBP))
(PROGN (PUTPROP 'CAR
                '(RPLACA LEFT RIGHT)
                'STOREFORM)
       (PUTPROP 'CDR
                '(RPLACD LEFT RIGHT)
                'STOREFORM)
       (PUTPROP 'ARG
                '(SETARG LEFT RIGHT)
                'STOREFORM)
       (PUTPROP 'PLIST
                '(SETPLIST LEFT RIGHT)
                'STOREFORM)
       (PUTPROP 'STATUS
                '(SSTATUS LEFT RIGHT)
                'STOREFORM))

(MAPC (FUNCTION (LAMBDA (I)
                  (PUTPROP I
                           (SUBST I
                                  'I
                                  '(LAMBDA NIL '(STATUS I)))
                           'NUD)))
      '(TOPLEVEL BREAKLEVEL WHO2 WHO3 TTYSCAN TTYREAD TTYINT GCTIME))

(PROGN 'COMPILE
       (DEFUN (OF LED) (LEFT) (LIST 'GET (PARSE 25.) LEFT))
       (DEFPROP OF 26. LBP))

(PROGN 'COMPILE
       (DEFUN (OFQ LED) (LEFT)
         (LIST 'GET (PARSE 25.) (LIST 'QUOTE LEFT)))
       (DEFPROP OFQ 26. LBP))

(PUTPROP 'NOT 9. 'RBP)

(PROGN 'COMPILE
       (DEFUN (NOT LED) (LEFT)
         (LIST 'NOT (FUNCALL (LED) LEFT)))
       (DEFPROP NOT 10. LBP))

(PROGN 'COMPILE
       (DEFUN (AND LED) (LEFT) (ISM LEFT 'AND 8. 'AND))
       (DEFPROP AND 8. LBP))

(PROGN 'COMPILE
       (DEFUN (OR LED) (LEFT) (ISM LEFT 'OR 7. 'OR))
       (DEFPROP OR 7. LBP))

(PROGN (DEFTOK =/#)
       (DEFTOK =$)
       (DEFTOK </#)
       (DEFTOK >/#)
       (DEFTOK <$)
       (DEFTOK >$)
       (DEFTOK <=)
       (DEFTOK >=))

(PROGN 'COMPILE
       (DEFUN (= LED) (LEFT) (ISI LEFT (ARITH 'EQUAL) 10.))
       (DEFPROP = 10. LBP))

(PROGN 'COMPILE
       (DEFUN (NE LED) (LEFT)
         (LIST 'NOT (ISI LEFT (ARITH 'EQUAL) 10.)))
       (DEFPROP NE 10. LBP))

(PROGN 'COMPILE
       (DEFUN (EQ LED) (LEFT) (ISI LEFT 'EQ 10.))
       (DEFPROP EQ 10. LBP))

(PROGN 'COMPILE
       (DEFUN (< LED) (LEFT)
         (ISM LEFT (ARITH 'LESSP) 10. '<))
       (DEFPROP < 10. LBP))

(PROGN 'COMPILE
       (DEFUN (> LED) (LEFT)
         (ISM LEFT (ARITH 'GREATERP) 10. '>))
       (DEFPROP > 10. LBP))

(PROGN 'COMPILE
       (DEFUN (=/# LED) (LEFT) (ISI LEFT '= 10.))
       (DEFPROP =/# 10. LBP))

(PROGN 'COMPILE
       (DEFUN (=$ LED) (LEFT) (ISI LEFT '= 10.))
       (DEFPROP =$ 10. LBP))

(PROGN 'COMPILE
       (DEFUN (</# LED) (LEFT) (ISI LEFT '< 10.))
       (DEFPROP </# 10. LBP))

(PROGN 'COMPILE
       (DEFUN (>/# LED) (LEFT) (ISI LEFT '> 10.))
       (DEFPROP >/# 10. LBP))

(PROGN 'COMPILE
       (DEFUN (<$ LED) (LEFT) (ISI LEFT '< 10.))
       (DEFPROP <$ 10. LBP))

(PROGN 'COMPILE
       (DEFUN (>$ LED) (LEFT) (ISI LEFT '> 10.))
       (DEFPROP >$ 10. LBP))

(PROGN 'COMPILE
       (DEFUN (<= LED) (LEFT)
         (LIST 'NOT (ISI LEFT (ARITH 'GREATERP) 10.)))
       (DEFPROP <= 10. LBP))

(PROGN 'COMPILE
       (DEFUN (>= LED) (LEFT)
         (LIST 'NOT (ISI LEFT (ARITH 'LESSP) 10.)))
       (DEFPROP >= 10. LBP))
(PROGN 'COMPILE
       (DEFUN (/| LED) (LEFT)
         (LIST (ARITH 'ZEROP)
               (LIST (ARITH 'REMAINDER) (PARSE 10.) LEFT)))
       (DEFPROP /| 10. LBP))

(PROGN 'COMPILE
       (DEFUN (ISIN LED) (LEFT) (ISI LEFT 'MEMBER 10.))
       (DEFPROP ISIN 10. LBP))

(PROGN 'COMPILE
       (DEFUN (ISATOM LED) (LEFT) (ISS LEFT 'ATOM))
       (DEFPROP ISATOM 10. LBP))

(PROGN 'COMPILE
       (DEFUN (ISNUM LED) (LEFT) (ISS LEFT 'NUMBERP))
       (DEFPROP ISNUM 10. LBP))

(PROGN 'COMPILE
       (DEFUN (EXISTS LED) (LEFT) (LIST 'SETQ 'IT LEFT))
       (DEFPROP EXISTS 10. LBP))

(PUTPROP 'NULL 10. 'RBP)

(PROGN 'COMPILE
       (DEFUN (|.| LED) (LEFT) (ISI LEFT 'CONS 14.))
       (DEFPROP |.| 15. LBP))

(PROGN 'COMPILE
       (DEFUN (@ LED) (LEFT) (ISM LEFT 'APPEND 15. '@))
       (DEFPROP @ 15. LBP))

(DEFUN ({ NUD) NIL
  (PROG2 NIL
         (CONS 'GATHER
               (COND ((NOT (EQUAL TOKEN '}))
                      (PARSELIST 0. '/,))))
         (CHECK '})))

(PROGN 'COMPILE
       (DEFUN (|| LED) (LEFT)
         (ISM LEFT 'UNION 16. '||))
       (DEFPROP || 16. LBP))

(PROGN 'COMPILE
       (DEFUN (/ LED) (LEFT)
         (ISM LEFT 'INTERSECT 16. '/))
       (DEFPROP / 16. LBP))

(DEFUN (~ NUD) NIL (ISP 'SETDIFF 16.))

(PROGN 'COMPILE
       (DEFUN (~ LED) (LEFT) (ISM LEFT 'SETDIFF 16. '~))
       (DEFPROP ~ 16. LBP))

(PROGN 'COMPILE
       (DEFUN (/ LED) (LEFT)
         (ISM LEFT 'ELEMENTP 10. '/))
       (DEFPROP / 10. LBP))
(PROGN 'COMPILE
       (DEFUN (/ LED) (LEFT)
         (ISM LEFT 'SUBSETP 10. '/))
       (DEFPROP / 10. LBP))

(PROGN (MAPC
        (FUNCTION (LAMBDA (U)
                    (OR (FBOUNDP U)
                        (PUTPROP U
                                 '((DSK LIBLSP) SETS FASL)
                                 'AUTOLOAD))))
        '(GATHER UNION INTERSECT SETDIFF ELEMENTS ELEMENTP SUBSETP
          SYMDIFF CLEARSETS))
       (IF (FBOUNDP '*LEXPR)
           (EVAL '(*LEXPR UNION INTERSECT SETDIFF SYMDIFF))))

(PROGN 'COMPILE
       (DEFUN (^ LED) (LEFT) (ISM LEFT 'CAT 18. '^))
       (DEFPROP ^ 18. LBP))

(PROGN 'COMPILE
       (DEFUN (CAT LED) (LEFT) (ISM LEFT 'CAT 18. 'CAT))
       (DEFPROP CAT 18. LBP))

(DEFUN (/| NUD) NIL
  (PROG2 NIL (ISP (ARITH 'ABS) 19.) (CHECK '/|)))

(DEFUN (+ NUD) NIL
  (COND ((MEMBER TOKEN '(|(| { [)) (ARITH 'PLUS))
        (T (PARSE 20.))))

(PROGN 'COMPILE
       (DEFUN (+ LED) (LEFT)
         (ISM LEFT (ARITH 'PLUS) 20. '+))
       (DEFPROP + 20. LBP))

(PROGN 'COMPILE
       (DEFUN (- LED) (LEFT)
         (ISM LEFT (ARITH 'DIFFERENCE) 20. '-))
       (DEFPROP - 20. LBP))

(DEFUN (- NUD) NIL (ISP (ARITH 'MINUS) 20.))

(DEFUN (* NUD) NIL
  (COND ((MEMBER TOKEN '(|(| [ {)) (ARITH 'TIMES))
        (T '*)))

(PROGN 'COMPILE
       (DEFUN (* LED) (LEFT)
         (ISM LEFT (ARITH 'TIMES) 21. '*))
       (DEFPROP * 21. LBP))

(PROGN 'COMPILE
       (DEFUN (// LED) (LEFT)
         (LIST (ARITH 'QUOTIENT)
               LEFT
               (LIST (ARITH 'FLOAT) (PARSE 21.))))
       (DEFPROP // 21. LBP))

(DEFTOK ///:)

(PROGN 'COMPILE
       (DEFUN (///: LED) (LEFT)
         (ISM LEFT (ARITH 'QUOTIENT) 21. '///:))
       (DEFPROP ///: 21. LBP))

(PROGN 'COMPILE
       (DEFUN (REM LED) (LEFT)
         (ISI LEFT (ARITH 'REMAINDER) 21.))
       (DEFPROP REM 21. LBP))

(PROGN 'COMPILE
       (DEFUN (MOD LED) (LEFT)
         (LIST 'MOD (PROG2 NIL LEFT) (PROG2 NIL (PARSE 21.))))
       (DEFPROP MOD 21. LBP)
       (DEFUN MOD (A B)
         ((LAMBDA (X)
            (COND ((AND (NOT (EQUAL (MINUSP A) (MINUSP B)))
                        (NOT (ZEROP X)))
                   (PLUS X B))
                  (T X)))
          (REMAINDER A B))))

(DEFTOK **)

(PROGN 'COMPILE
       (DEFUN (** LED) (LEFT) (ISI LEFT (ARITH 'EXPT) 21.))
       (DEFPROP ** 22. LBP))

(PROGN (DEFTOK +/#)
       (DEFTOK -/#)
       (DEFTOK */#)
       (DEFTOK |//#|)
       (DEFTOK \\))

(PROGN 'COMPILE
       (DEFUN (+/# LED) (LEFT) (ISM LEFT '+ 20. '+/#))
       (DEFPROP +/# 20. LBP))

(PROGN 'COMPILE
       (DEFUN (-/# LED) (LEFT) (ISM LEFT '- 20. '-/#))
       (DEFPROP -/# 20. LBP))

(PROGN 'COMPILE
       (DEFUN (*/# LED) (LEFT) (ISM LEFT '* 21. '*/#))
       (DEFPROP */# 21. LBP))

(PROGN 'COMPILE
       (DEFUN (|//#| LED) (LEFT)
         (ISM LEFT '// 21. '|//#|))
       (DEFPROP |//#| 21. LBP))

(PROGN 'COMPILE
       (DEFUN (\\ LED) (LEFT) (ISI LEFT '\\ 19.))
       (DEFPROP \\ 19. LBP))

(PROGN (DEFTOK +$) (DEFTOK -$) (DEFTOK *$) (DEFTOK //$))
(PROGN 'COMPILE
       (DEFUN (+$ LED) (LEFT) (ISM LEFT '+$ 20. '+$))
       (DEFPROP +$ 20. LBP))

(PROGN 'COMPILE
       (DEFUN (-$ LED) (LEFT) (ISM LEFT '-$ 20. '-$))
       (DEFPROP -$ 20. LBP))

(PROGN 'COMPILE
       (DEFUN (*$ LED) (LEFT) (ISM LEFT '*$ 21. '*$))
       (DEFPROP *$ 21. LBP))

(PROGN 'COMPILE
       (DEFUN (//$ LED) (LEFT) (ISM LEFT '//$ 21. '//$))
       (DEFPROP //$ 21. LBP))

(PROGN (DEFTOK /:N/:)
       (DEFTOK /:A/:)
       (DEFTOK /:V/:)
       (DEFTOK /:X/:)
       (DEFTOK /:^/:))

(DEFUN (/:N/: NUD) NIL (LIST 'BOOLE 12. 0. (PARSE 21.)))

(PROGN 'COMPILE
       (DEFUN (/:A/: LED) (LEFT)
         (CONS 'BOOLE
               (CONS 1. (CONS LEFT (PARSELIST 21. '/:A/:)))))
       (DEFPROP /:A/: 21. LBP))

(PROGN 'COMPILE
       (DEFUN (/:V/: LED) (LEFT)
         (CONS 'BOOLE
               (CONS 7. (CONS LEFT (PARSELIST 20. '/:V/:)))))
       (DEFPROP /:V/: 20. LBP))

(PROGN 'COMPILE
       (DEFUN (/:X/: LED) (LEFT)
         (CONS 'BOOLE
               (CONS 6. (CONS LEFT (PARSELIST 20. '/:X/:)))))
       (DEFPROP /:X/: 20. LBP))

(PROGN 'COMPILE
       (DEFUN (/:^/: LED) (LEFT) (ISI LEFT 'LSH 22.))
       (DEFPROP /:^/: 22. LBP))

(PUTPROP 'PRINT 2. 'RBP)

(PUTPROP 'PRINC 2. 'RBP)

(PUTPROP 'PRIN1 2. 'RBP)

(DEFUN (WRITE NUD) NIL
  (SUBST (CONS 'LIST (PARSELIST 2. '/,))
         'X
         '(PROGN (TERPRI)
                 (MAPC (FUNCTION PRINC) X)
                 (PRINC '| |))))

(DEFUN (NEWLINE NUD) NIL (ISN 'TERPRI))

(DEFUN (UREAD NUD) NIL (CONS 'UREAD (GETTOKENS)))

(DEFUN (UWRITE NUD) NIL (CONS 'UWRITE (GETTOKENS)))

(DEFUN (UFILE NUD) NIL (CONS 'UFILE (GETTOKENS)))

(PROGN (SETQ SYNTAX-NEEDED T)
       (SETQ SILENCE (MINUS 1.))
       (SETQ DEFBP 25.)
       (SETQ NUDL '(NUD))
       (SETQ LEDL '(LED))
       (SETQ LBPL '(LBP))
       (SETQ CNUD 'NUD)
       (SETQ CLED 'LED)
       (SETQ CLBP 'LBP)
       (SETQ FUN 'TOP-LEVEL)
       (SETQ LANGUAGE_ALIST NIL)
       (SETQ ARITHMETIC_ALIST NIL))

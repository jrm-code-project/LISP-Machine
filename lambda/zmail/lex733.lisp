;;; RFC733 lexer FSM definition -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **

(EVAL-WHEN (COMPILE LOAD EVAL)
  (FERROR NIL "If you are compiling this, and not using RTC, you are losing"))

(:MAC OPEN #/(
      CLOSE #/)
      LEFT-BRACKET #/<
      RIGHT-BRACKET #/>
      ;; PERIOD #/.
      ATSIGN #/@
      ;; PERCENT #/%
      COMMA #/,
      SEMI #/;
      COLON #/:
      BACKSLASH #/\
      QUOTE-START-CHAR #/"
      AS '(// #/A #/a)
      TS '(// #/T #/t)
      SPECIAL-CHAR (LIST '// OPEN CLOSE LEFT-BRACKET RIGHT-BRACKET ATSIGN COMMA SEMI COLON
                         ;; PERIOD
                         ;; PERCENT
                         QUOTE-START-CHAR #/[ #/])
      EOF-CHAR -1
      SLASHIFIED-CHAR -2
      LWSP-CHAR '(// #/SP #/TAB)
      WHITE-SPACE-CHAR `(// #/CR ,@(CDR LWSP-CHAR))
      BREAK `(// ,@(CDR WHITE-SPACE-CHAR)
                 ,@(CDR SPECIAL-CHAR)
                 ,EOF-CHAR)
      ANY (CONS '// (DO ((I -2 (1+ I))
                         (X NIL (CONS I X)))
                        ((= I SI:RDTBL-ARRAY-SIZE) X))))

(:DEF LEFT-BRACKET
      LEFT-BRACKET
  SINGLE)

(:DEF RIGHT-BRACKET
      RIGHT-BRACKET
  SINGLE)

;(:DEF ATSIGN
;      (:U ATSIGN PERCENT)
;  SINGLE)
(:def atsign
      atsign
  single)

(:DEF COMMA
      COMMA
  SINGLE)

;(:DEF PERIOD
;      PERIOD
;  SINGLE)

(:DEF SEMI
      SEMI
  SINGLE)

(:DEF COLON
      COLON
  SINGLE)

(:DEF QUOTED-STRING
      QUOTE-START-CHAR
  START)

(:DEF COMMENT
      OPEN
  START)

(:DEF CLOSE
      CLOSE
  SINGLE)

(:DEF AT-ATOM
      (:! AS
          TS
          BREAK)
  UNTYI)

(:DEF ATOM
      (:! (+ (- ANY BREAK))
          BREAK)
  UNTYI)

(:DEF EOF EOF-CHAR SINGLE)

(:OPT :EOF-CHAR EOF-CHAR)

(:OPT :QUOTED-CHAR SLASHIFIED-CHAR)

(:OPT :READ-FUNCTION-PROPERTY 'RFC733)

(:OPT :WHITE-SPACE-CHAR (CDR WHITE-SPACE-CHAR))

(:OPT :ESCAPE BACKSLASH)

(:END RFC733)

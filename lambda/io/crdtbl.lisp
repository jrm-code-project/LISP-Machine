;;; -*- Mode:LISP; Package:SI; Base:10; Readtable:ZL -*-

;;; Readtable for Common Lisp.

(EVAL-WHEN (COMPILE LOAD EVAL)
  (FERROR NIL "If you are compiling or loading this, and not using RTC, you are losing!"))

(:MAC DIGIT '(// #/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
      EXTENDED-DIGIT '(// -11)                          ;-11 is extended digit syntax.
      ILLEGAL '(// -12)                                 ;-12 is "illegal token constituent"
                                                        ;which is what Return or Space become
                                                        ;if made into symbol constituents.
      PLUS-MINUS '(// #/+ #/-)
      PLUS #/+
      POINT #/.
      SLASH #//
      BACKSLASH #/\
      VBAR #/|
      CIRCLECROSS #/
      EE '(// #/E #/e)
      SS '(// #/S #/s)
      DDLLFF '(// #/L #/l #/D #/d #/F #/f)
      EXTENSION-CHAR '(// #/^ #/_)
      SHARP-SIGN #/#
      COLON #/:
      NULL '(//)
      QUOTED-CHAR -1                                    ;Quoted chars to be mapepd to -1.
      EOF-CHAR -2                                       ;EOF is mapped to -2.
      WHITE-SPACE-CHAR '(// #/SP #/TAB #/LINE #/PAGE #/CR -5)   ;-5 IS WHITESPACE SYNTAX
      BREAK (NCONC '(// #/( #/) #/' #/` #/, #/" #/; #/ -2 -3 -4 -6)    ;-3 IS BREAK SYNTAX
                   (CDR WHITE-SPACE-CHAR))
      MACRO-CHARACTER '(// #/' #/, #/; #/` #/# #/( #/) #/" #/ -6 -10)
                                                        ;-6 is macro syntax,
                                                        ;-10 nonterminating macro syntax.
      STANDALONE-CHAR '(// -4)                          ;-4 is single syntax
      ANY (CONS '// (DO ((I -11 (1+ I))                 ;-7 is alphabetic syntax
                         (X NIL (CONS I X)))            ;Note ANY does not include -12.
                        ((= I SI:RDTBL-ARRAY-SIZE) X)))
      ANY-BUT-EOF (DELETE -2 ANY)
      LETTER (APPEND '(// -11)
                     (DO ((I #/A (1+ I))
                          (X NIL))
                         ((> I #/Z) X)
                       (PUSH I X))
                     (DO ((I #/a (1+ I))
                          (X NIL))
                         ((> I #/z) X)
                       (PUSH I X)))

      PACKAGE-NAME '(:* (:- ANY-BUT-EOF (:U COLON BREAK)))

      SIGN? '(U NULL PLUS-MINUS)
      POTENTIAL-NUMBER-INSIDES '(:! (:* (:U DIGIT POINT EXTENSION-CHAR SLASH PLUS-MINUS))
                                    (:* (:! LETTER
                                            (:+ (:U DIGIT POINT EXTENSION-CHAR
                                                    SLASH PLUS-MINUS))))
                                    (:U NULL LETTER))
      POTENTIAL-NUMBER-END '(:! (:* (:U DIGIT POINT EXTENSION-CHAR
                                        SLASH PLUS-MINUS))
                                (:* (:! LETTER
                                        (:+ (:U DIGIT POINT EXTENSION-CHAR
                                                SLASH PLUS-MINUS))))
                                (:U LETTER
                                    (:! LETTER
                                        (:U DIGIT POINT EXTENSION-CHAR SLASH))
                                    DIGIT POINT EXTENSION-CHAR SLASH))
      POTENTIAL-NUMBER-NO-POINTS-INSIDES
                           '(:! (:* (:U DIGIT EXTENDED-DIGIT EXTENSION-CHAR SLASH PLUS-MINUS))
                                (:* (:! LETTER
                                        (:+ (:U DIGIT EXTENDED-DIGIT EXTENSION-CHAR
                                                SLASH PLUS-MINUS))))
                                (:U NULL LETTER))
      POTENTIAL-NUMBER-NO-POINTS-END
                           '(:! (:* (:U DIGIT EXTENDED-DIGIT EXTENSION-CHAR
                                        SLASH PLUS-MINUS))
                                (:* (:! LETTER
                                        (:+ (:U DIGIT EXTENDED-DIGIT EXTENSION-CHAR
                                                SLASH PLUS-MINUS))))
                                (:U LETTER
                                    (:! LETTER
                                        (:U DIGIT EXTENDED-DIGIT EXTENSION-CHAR SLASH))
                                    DIGIT EXTENDED-DIGIT EXTENSION-CHAR SLASH))

      RTC-FIXNUM '(:! (:+ (:U DIGIT EXTENDED-DIGIT))
                      (:U NULL POINT))
      RTC-FLOAT-NO-EXP '(:! (:* DIGIT)
                            POINT
                            (:+ DIGIT))
      RTC-DECNUM '(:! (:+ DIGIT)
                      (:U NULL POINT))
      RTC-FLONUM '(:! (:U (:! RTC-FLOAT-NO-EXP
                              (:U NULL
                                  (:! EE
                                      SIGN?
                                      (:+ DIGIT))))
                          (:! RTC-DECNUM
                              EE
                              SIGN?
                              (:+ DIGIT))))
      RTC-SMALL-FLONUM '(:! (:U RTC-FLOAT-NO-EXP
                                RTC-DECNUM)
                            SS
                            SIGN?
                            (:+ DIGIT))
      RTC-SINGLE-FLONUM '(:! (:U RTC-FLOAT-NO-EXP
                                 RTC-DECNUM)
                             DDLLFF
                             SIGN?
                             (:+ DIGIT))
      RTC-RATIONAL '(:! (:+ (:U DIGIT EXTENDED-DIGIT))
                        SLASH
                        (:+ (:U DIGIT EXTENDED-DIGIT)))
      )

;;; A readtable definition looks like (DEF name regular-expression type).
;;; "name" is the name of the kind of token.  It has a function to process the
;;;   string, on its property list, OR it is a symbol to be returned.
;;; "regular-expression" is a regular expression.
;;; "type" is a symbol indicating what to do with the last character
;;; recognized by the regular expression.

;; First, numbers.  Anything that looks like a number really is one,
;; so these can be first; and they must precede SYMBOL
;; since all numbers would be symbols if they weren't numbers.
(:DEF FIXNUM
      (:! SIGN?
         RTC-FIXNUM
         BREAK)
  UNTYI-FUNCTION)

(:DEF FLOAT
      (:! SIGN?
          RTC-FLONUM
          BREAK)
  UNTYI-FUNCTION)

(:DEF SHORT-FLOAT
      (:! SIGN?
          RTC-SMALL-FLONUM
          BREAK)
  UNTYI-FUNCTION)

(:DEF SINGLE-FLOAT
      (:! SIGN?
          RTC-SINGLE-FLONUM
          BREAK)
  UNTYI-FUNCTION)

(:DEF RATIONAL
      (:! SIGN?
          RTC-RATIONAL
          BREAK)
  UNTYI-FUNCTION)

(:DEF SHARP-PACKAGE-PREFIX
      (:! PACKAGE-NAME SHARP-SIGN COLON)
  LAST-CHAR)

(:DEF CONSING-DOT
      (:! POINT BREAK)
  UNTYI-QUOTE)

(:DEF MULTI-DOT
     (:! (:+ POINT) BREAK)
  UNTYI-FUNCTION)

(:DEF EOF EOF-CHAR NO-UNTYI-QUOTE)

(:DEF MACRO-CHAR
      MACRO-CHARACTER
  LAST-CHAR)

(:DEF SC-SYMBOL
      STANDALONE-CHAR
  NO-UNTYI-FUNCTION)

(:DEF PACKAGE-PREFIX
      (:! PACKAGE-NAME COLON)
  LAST-CHAR)

;;; This includes anything that Common Lisp might someday define to be a kind of number.
;;; If something gets this far, it isn't actually a number, so it will be read as a symbol.
;;; But it will be quoted when printed for the sake of other Common Lisp
;;; systems in which it might be a number.
(:DEF POTENTIAL-NUMBER
      (:! (:U DIGIT
              (:! (:U DIGIT PLUS-MINUS POINT EXTENSION-CHAR)
                  POTENTIAL-NUMBER-INSIDES
                  DIGIT
                  POTENTIAL-NUMBER-END)
              ;; Degenerate cases: if first or last is a digit,
              ;; we don't need to insist on a digit somewhere inside.
              (:! DIGIT
                  POTENTIAL-NUMBER-END)
              (:! (:U DIGIT PLUS-MINUS POINT EXTENSION-CHAR)
                  POTENTIAL-NUMBER-INSIDES
                  DIGIT)
              ;; Now an analogous set of cases, which allow extended digits but not points.
              EXTENDED-DIGIT
              (:! (:U DIGIT EXTENDED-DIGIT PLUS-MINUS EXTENSION-CHAR)
                  POTENTIAL-NUMBER-NO-POINTS-INSIDES
                  (:U DIGIT EXTENDED-DIGIT)
                  POTENTIAL-NUMBER-NO-POINTS-END)
              ;; Degenerate cases: if first or last is a digit,
              ;; we don't need to insist on a digit somewhere inside.
              (:! (:U DIGIT EXTENDED-DIGIT)
                  POTENTIAL-NUMBER-NO-POINTS-END)
              (:! (:U DIGIT EXTENDED-DIGIT PLUS-MINUS EXTENSION-CHAR)
                  POTENTIAL-NUMBER-NO-POINTS-INSIDES
                  (:U DIGIT EXTENDED-DIGIT)))
          BREAK)
  UNTYI-FUNCTION)

;;; These are never reached, since slash and vbar are caught at a low level
;;; and only serve to quote other characters.  However,
;;; these do cause slash and vbar to have unique read syntaxes,
;;; which is how the low level checks for them.
(:DEF :CHARACTER-CODE-ESCAPE CIRCLECROSS NO-UNTYI-FUNCTION)
(:DEF :ESCAPE BACKSLASH NO-UNTYI-FUNCTION)
(:DEF :MULTIPLE-ESCAPE VBAR NO-UNTYI-FUNCTION)

;;; Must be last.
(:DEF SYMBOL
      (:! (:* (:- ANY-BUT-EOF BREAK))
          BREAK)
  UNTYI-FUNCTION)

(:DEF SYMBOL-ILLEGAL
      (:! (:+ (:U (:- ANY-BUT-EOF BREAK) ILLEGAL))
          BREAK)
  UNTYI-FUNCTION)

(:OPT :WHITE-SPACE-CHAR (CDR WHITE-SPACE-CHAR))                 ;Options to RTC
(:OPT :MACRO-ALIST '((#/" XR-DOUBLEQUOTE-MACRO)
                     (#/( XR-OPENPAREN-MACRO)
                     (#/) XR-CLOSEPAREN-MACRO)
                     (#/' XR-QUOTE-MACRO)
                     (#/; XR-COMMENT-MACRO)
                     (#/` XR-BACKQUOTE-MACRO)
                     (#/, XR-COMMA-MACRO)
                     (#/# XR-DISPATCH-MACRO-DRIVER T
                      (#/' XR-#/'-MACRO)
                      (#/` XR-#/`-MACRO)
                      ;>>No! (#/ XR-#-MACRO)
                      (#/\ XR-CL-#\-MACRO)
                      (#/^ XR-#^-MACRO)
                      (#/, XR-#/,-MACRO)
                      (#/. XR-#.-MACRO)
                      (#/: XR-#/:-MACRO)
                      (#/= XR-#=-MACRO)
                      (#/# XR-##-MACRO)
                      (#/ XR-#-MACRO)
                      (#/( XR-#/(-MACRO)
                      (#/* XR-#*-MACRO)
                      (#/A XR-#A-MACRO)
                      (#/S XR-#S-MACRO)
                      (#/C XR-#C-MACRO)
                      ;>> if you are using one of the next three, you are losing anyway
                      ; (#/Q XR-#Q-MACRO)
                      ; (#/M XR-#M-MACRO)
                      ; (#/N XR-#N-MACRO)
                      (#/+ XR-#+-MACRO)
                      (#/- XR-#--MACRO)
                      (#/B XR-#B-MACRO)
                      (#/O XR-#O-MACRO)
                      (#/R XR-#R-MACRO)
                      (#/X XR-#X-MACRO)
                      (#/ INFIX-TOPLEVEL-PARSE)
                      (#/| XR-#/|-MACRO)
                      (#/! XR-#!-MACRO) ;; not common-lisp, but we need this for patch files.
                      )))
(:OPT :READ-FUNCTION-PROPERTY 'STANDARD-READ-FUNCTION)
;; The next two are redundant.  They set different variables, but must match.
;(:OPT :QUOTE #/\)
(:OPT :ESCAPE BACKSLASH)
(:OPT :MULTIPLE-ESCAPE VBAR)
;; The next two are redundant.  They set different variables, but must match.
;(:OPT CIRCLECROSS #/)
(:OPT :CHARACTER-CODE-ESCAPE CIRCLECROSS)
(:OPT :QUOTED-CHAR QUOTED-CHAR)
(:OPT :EOF-CHAR EOF-CHAR)
(:OPT :A-BREAK-CHAR -3)                                 ;For the reader to use.
(:OPT :MAKE-SYMBOL '(SC-SYMBOL))                        ;Who makes symbols
(:OPT :MAKE-SYMBOL-BUT-LAST '(SYMBOL))                  ;and how.
(:OPT :BITS '((#/" #o10)))                              ;Bits to be ored into readtable.
(:OPT :SAVE-SYNTAX '(SINGLE -4                          ;Placed in plist of readtable
                     QUOTE #/\                          ; with syntax bits replacing
                     ESCAPE #/\                         ; character numbers.
                     MULTIPLE-ESCAPE #/|
                     RATIO #//
                     CIRCLECROSS #/
                     CHARACTER-CODE-ESCAPE #/
                     WHITESPACE -5
                     MACRO -6
                     NON-TERMINATING-MACRO -10
                     BREAK -3
                     ALPHABETIC -7
                     ILLEGAL -12
                     EXTENDED-DIGIT -11
                   ))
(:OPT :TRANSLATIONS '(((#/a  #/z)  (#/A  #/Z))))        ;Translations may be pairs of
                                                        ;intervals (inclusive) or just chars

(:OPT :PTTBL-SLASH #/\)
(:OPT :PTTBL-CHARACTER '("#" . "\"))
(:OPT :PTTBL-RATIONAL-INFIX #//)
(:OPT :PTTBL-COMPLEX '("#C(" " " ")"))
;;; Also don't forget to update the cruft in the function SI::PKG-INITIALIZE
(:OPT :SYMBOL-SUBSTITUTIONS
     '((// . CLI://)
       (*DEFAULT-PATHNAME-DEFAULTS* . CL:*DEFAULT-PATHNAME-DEFAULTS*)
       (GLOBAL:AR-1 . CL:AR-1)
       (GLOBAL:AR-1-FORCE . CL:AR-1-FORCE)
       (GLOBAL:AREF . CL:AREF)
       (GLOBAL:ASSOC . CL:ASSOC)
       (GLOBAL:ATAN . CL:ATAN)
       (CHARACTER . CL:CHARACTER)
       (GLOBAL:CLOSE . CL:CLOSE)
       (GLOBAL:DEFSTRUCT . CL:DEFSTRUCT)
       (GLOBAL:DELETE . CL:DELETE)
       (GLOBAL:EVERY . CL:EVERY)
       (GLOBAL:FUNCTIONP . CL:FUNCTIONP)
       (GLOBAL:GETHASH . CL:GETHASH)
       (GLOBAL:IF . CL:IF)
       (GLOBAL:INTERSECTION . CL:INTERSECTION)
       (GLOBAL:LISTP . CL:LISTP)
       (GLOBAL:MAKE-ARRAY . CL:MAKE-ARRAY)
       (GLOBAL:MAKE-PACKAGE . CL:MAKE-PACKAGE)
       (GLOBAL:MAP . CL:MAP)
       (GLOBAL:MEMBER . CL:MEMBER)
       (GLOBAL:NINTERSECTION . CL:NINTERSECTION)
       (GLOBAL:NLISTP . CLI:NLISTP) ;cli, not cl
       (GLOBAL:NUNION . CL:NUNION)
       (GLOBAL:RASSOC . CL:RASSOC)
       (GLOBAL:READ . CL:READ)
       (GLOBAL:READ-FROM-STRING . CL:READ-FROM-STRING)
       (GLOBAL:REM . CL:REM)
       (GLOBAL:REMOVE . CL:REMOVE)
       (GLOBAL:SOME . CL:SOME)
       (GLOBAL:SUBST . CL:SUBST)
       (GLOBAL:TIME . CL:TIME)
       (GLOBAL:UNION . CL:UNION))
     )
(:OPT :NAMES '("standard Common-Lisp" "CL" "Common-Lisp" "Generic" "Plebian"))
(:OPT :PROPERTIES '(:SYNTAX :COMMON-LISP))

(:END COMMON-LISP-READTABLE)                            ;The symbol whose value cell will
                                                        ;be loaded with the readtable

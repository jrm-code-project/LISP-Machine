;;; -*- Mode:LISP; Package:SI; Base:10; Readtable:ZL -*-

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

(EVAL-WHEN (COMPILE LOAD EVAL)
  (FERROR NIL "If you are compiling or loading this, and not using RTC, you are losing!"))

;;;Negative codes:
;;; -1          quoted
;;; -2          eof
;;; -3          break
;;; -4          single
;;; -5          whitespace
;;; -6          macro
;;; -7          alphabetic
;;; -8          non-terminating macro
;;; -9          extended digit

;;;Bits
;;; 1           Whitespace (tested only in XR-XRTYI and XR-XRUNTYI)
;;; 2           slash (tested only in printing strings)
;;; 4           circle-cross (tested only in printing strings)
;;; 8           " String quote (tested only in printing strings)

(:MAC DIGIT '(// #/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
      EXTENDED-DIGIT '(// -11)
      PLUS-MINUS '(// #/+ #/-)
      PLUS #/+
      POINT #/.
      BACKSLASH #/\
      SLASH #//
      VBAR #/|
      CIRCLECROSS #/
      EE '(// #/E #/e)
      SS '(// #/S #/s)
      DDLLFF '(// #/D #/d #/L #/l #/F #/f)
      EYE '(// #/i #/I)
      LSH-SCALE '(// #/_ #/^)
      EXTENSION-CHAR '(// #/^ #/_)
      SHARP-SIGN #/#
      COLON #/:
      NULL '(//)
      ;;Quoted chars to be mapped to -1.
      QUOTED-CHAR -1
      ;;EOF mapped to -2.
      EOF-CHAR -2
      ;;-5 is whitespace syntax
      WHITE-SPACE-CHAR '(// #/SP #/TAB #/LINE #/PAGE #/CR -5)
      ;;-3 is break syntax
      BREAK (NCONC '(// #/( #/) #/' #/` #/, #/" #/; #/ -2 -3 -4 -6)
                   (CDR WHITE-SPACE-CHAR))
      ;;-6 is macro syntax
      MACRO-CHARACTER '(// #/' #/, #/; #/` #/# #/( #/) #/" #/ -6 -8)
      ;;-8 is nonterminating macro syntax
      ;;-4 is single syntax
      STANDALONE-CHAR '(// -4)
      ;;-7 is alphabetic syntax
      ANY (CONS '// (DO ((I -11 (1+ I))
                         (X NIL (CONS I X)))
                        ((= I SI:RDTBL-ARRAY-SIZE) X)))
      ANY-BUT-EOF (DELETE -2 ANY)
      LETTER (LIST* '// -11 (DO ((I #/A (1+ I))
                                 (X NIL))
                                ((> I #/Z) X)
                              (PUSH I X)))

      PACKAGE-NAME '(* (- ANY-BUT-EOF (U COLON BREAK)))

      SIGN? '(:U NULL PLUS-MINUS)

      RTC-FIXNUM '(:! (:+ (:U DIGIT EXTENDED-DIGIT))
                     (:U NULL POINT)
                     (:U NULL
                         (:! LSH-SCALE
                             (:U NULL PLUS)
                             (:! (+ DIGIT)
                                 (:U NULL POINT))
                             (:U NULL POINT))))
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
      RTC-SHORT-FLONUM '(:! (:U RTC-FLOAT-NO-EXP
                                RTC-DECNUM)
                            SS
                            SIGN?
                            (+ DIGIT))
      RTC-SINGLE-FLONUM '(:! (:U RTC-FLOAT-NO-EXP
                                 RTC-DECNUM)
                             DDLLFF
                             SIGN?
                             (:+ DIGIT))
      RTC-RATIONAL '(:! (:+ (:U DIGIT EXTENDED-DIGIT))
                        BACKSLASH
                        (:+ (:U DIGIT EXTENDED-DIGIT)))
      )

;;; A readtable definition looks like (:DEF name regular-expression type).
;;; "name" is the name of the kind of token.  It has a function to process the
;;;   string, on its property list, OR it is a symbol to be returned.
;;; "regular-expression" is a regular expression.
;;; "type" is a symbol indicating what to do with the last character
;;; recognized by the regular expression.

;;; First, numbers.  Anything that looks like a number really is one,
;;; so these can be first; and they must precede SYMBOL
;;; since all numbers would be symbols if they weren't numbers.
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
          RTC-SHORT-FLONUM
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

(:DEF COMPLEX
      (:! SIGN?
          (:U (:! (:U RTC-FIXNUM
                      RTC-FLONUM
                      RTC-SHORT-FLONUM
                      RTC-SINGLE-FLONUM
                      RTC-RATIONAL)
                  PLUS-MINUS)
              SIGN?)
          (:U RTC-FIXNUM
              RTC-FLONUM
              RTC-SHORT-FLONUM
              RTC-SINGLE-FLONUM
              RTC-RATIONAL)
          EYE
          BREAK)
  UNTYI-FUNCTION)

(:DEF SHARP-PACKAGE-PREFIX
      (:! PACKAGE-NAME SHARP-SIGN COLON)
  LAST-CHAR)

(:DEF CONSING-DOT
      (:! POINT BREAK)
  UNTYI-QUOTE)

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

;;; These are never reached, since slash and vbar are caught at a low level
;;; and only serve to quote other characters.  However,
;;; these do cause slash and vbar to have unique read syntaxes,
;;; which is how the low level checks for them.
(:DEF CHARACTER-CODE-ESCAPE CIRCLECROSS NO-UNTYI-FUNCTION)
(:DEF ESCAPE SLASH NO-UNTYI-FUNCTION)
(:DEF MULTIPLE-ESCAPE VBAR NO-UNTYI-FUNCTION)

;;; Must be last.
(:DEF SYMBOL
      (:! (:* (:- ANY-BUT-EOF BREAK))
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
                      (#/ XR-#-MACRO)
                      (#/\ XR-#\-MACRO)
                      (#// XR-#\-MACRO)
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
                      (#/Q XR-#Q-MACRO)
                      (#/M XR-#M-MACRO)
                      (#/N XR-#N-MACRO)
                      (#/+ XR-#+-MACRO)
                      (#/- XR-#--MACRO)
                      (#/B XR-#B-MACRO)
                      (#/O XR-#O-MACRO)
                      (#/R XR-#R-MACRO)
                      (#/X XR-#X-MACRO)
                      (#/ INFIX-TOPLEVEL-PARSE)
                      (#/| XR-#/|-MACRO)
                      (#/! XR-#!-MACRO)
                      )))
(:OPT :READ-FUNCTION-PROPERTY 'STANDARD-READ-FUNCTION)
;; The next two are redundant.  They set different variables, but must match.
;(:OPT :QUOTE #//)
(:OPT :ESCAPE SLASH)
(:OPT :MULTIPLE-ESCAPE VBAR)
;; The next two are redundant.  They set different variables, but must match.
;(:OPT :CIRCLECROSS #/)
(:OPT :CHARACTER-CODE-ESCAPE CIRCLECROSS)
(:OPT :QUOTED-CHAR QUOTED-CHAR)
(:OPT :EOF-CHAR EOF-CHAR)
(:OPT :A-BREAK-CHAR -3)                                 ;For the reader to use.
(:OPT :MAKE-SYMBOL '(SC-SYMBOL))                        ;Who makes symbols
(:OPT :MAKE-SYMBOL-BUT-LAST '(SYMBOL))                  ;and how.
(:OPT :BITS '((#/" #o10)))                              ;Bits to be ored into readtable.
(:OPT SAVE-SYNTAX '(SINGLE -4                           ;Placed in plist of readtable
                    SLASH #//                           ; with syntax bits replacing
                    ESCAPE #//                          ; character numbers.
                    MULTIPLE-ESCAPE #/|
                    CHARACTER-CODE-ESCAPE #/
                    CIRCLECROSS #/
                    WHITESPACE -5
                    MACRO -6
                    NON-TERMINATING-MACRO -10
                    BREAK -3
                    ALPHABETIC -7
                    DIGITSCALE #/^
                    BITSCALE #/_
                    EXTENDED-DIGIT -11
                   ))
(:OPT :NAMES '("standard Zetalisp" "ZL" "T" "LM"
               "standard traditional syntax" "Traditional" "Zetalisp"))
(:OPT :PROPERTIES '(:SYNTAX :ZETALISP))
(:OPT :TRANSLATIONS '(((#/a  #/z)  (#/A  #/Z))))        ;Translations may be pairs of
                                                        ;intervals (inclusive) or just chars

(:END *READTABLE*)                                      ;The symbol whose value cell will
                                                        ;be loaded with the readtable

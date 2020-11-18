;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-


;;; found out the hard way
;;; (:mode fs:file-attribute-bindings) is bashed by steve!
;;;


(defpackage steve-si
  (:size 100)
  (:EXPORT "0P" "-P" "+P" "<&" ">&" "=&" ">=&" "<=&" "+&" "-&" "*&" "/&" "1+&" "1-&" "\\&"
           "MAKE-VECTOR" "MIN&" "MAX&" "LOGAND&" "ZEROP&" "PLUSP&" "MINUSP&" "LOGIOR&"
           "LOGANDC2&" "LOGANDC1&" "LOGBITP&" "^&" "GRAPHIC-CHARP" "TO-STRING" "LOGBITP&"
           "MAKE-BITS" "STRING-CHARP" "OUSTR" "%STRING-REPLACE" "%STRING-POSQ" "VECTOR-LENGTH"
           "BITS-LENGTH" "BITSP" "TO-CHARACTER" "PAIRP" "RPLACHAR" "BITS-REPLACE"
           "RESET-FILL-POINTER" "OUTPUT-STREAM" "VREF" "%STRING-EQV" "SGVREF"
           "LOWERCASEP" "OF-TYPE" "INPUT-STREAM" "MERGE-PATHNAME-DEFAULTS" "USER-WORKINGDIR-PATHNAME"
           "SIMPLE-STRING-LENGTH" "BIT1P"
           "GET-A-BYTE"
           "%STRING-MAXPREFIX"
           "ELAPSED-TIME"
           ">$"
           "%DIGIT-WEIGHT-TO-CHAR"
           "%DIGIT-CHAR-TO-WEIGHT"
           "%DIGIT-CHAR-IN-RADIXP"
           "VALRET"
           "ABS&"
           "STRING-SUBSEQ"
           "MOD&"
           "%STRING-TRANSLATE"
           "MAPDIRECTORY"
           "WHEREIS"
           )
  (:SHADOW "TERMINAL-IO"
            "STANDARD-OUTPUT" "STANDARD-INPUT" "CURSORPOS" "PATHNAME"
            "LEXPR-FUNCALL" "STRING-EQUAL"
            ))


(DEFPACKAGE STEVE
  (:SIZE 1000)
  (:SHADOW "BEEP" "ED" "DIRED")
  (:SHADOWING-IMPORT STEVE-SI:TERMINAL-IO
                     STEVE-SI:STANDARD-OUTPUT STEVE-SI:STANDARD-INPUT STEVE-SI:CURSORPOS STEVE-SI:PATHNAME
                     STEVE-SI:LEXPR-FUNCALL STEVE-SI:STRING-EQUAL
                     )
  (:USE STEVE-SI LISP GLOBAL)
  (:RELATIVE-NAMES ("FS" "STEVE-SI") ("SI" "STEVE-SI")
                   ("FILE-SYSTEM" "STEVE-SI")
                   ("SYSTEM-INTERNALS" "STEVE-SI")))



(SI:SET-SYSTEM-SOURCE-FILE "STEVE" "DJ:GJCX.STEVE;SYSDEF")

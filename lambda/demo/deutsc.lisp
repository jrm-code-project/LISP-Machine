;-*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

(DEFUN MAKE-LIST-ARRAY (LIST)
  (COERCE LIST 'VECTOR))

(DEFCONST GERMAN-SMALL #("ein" "zwei" "drei" "vier" "fuenf" "sechs"
                         "sieben" "acht" "neun" "zehn" "elf" "zwoelf"
                         "dreizehn" "veirzehn" "fuenfzehn" "sechszehn"
                         "siebzehn" "achtzehn" "neunzehn"))

(DEFCONST GERMAN-MEDIUM #("zwanzig" "dreissig" "vierzig" "fuenfzig" "sechsig"
                          "siebzig" "achtzig" "neunzig"))

(DEFCONST GERMAN-LARGE #("tausand" "Million" "Milliard" "Billion" "Billiard"
                         "Trillion" "Trilliard" "Quadrillion"))

(DEFCONST GERMAN-ORDINAL-SMALL #("erste" "zwitte" "dritte" "vierte"
                                 "fuenfste" "sechste" "siebenste" "achtste"
                                 "neunte" "zehnte" "elfte" "zwoelfte"
                                 "dreizehnte" "veirzehnte" "fuenfzehnte"
                                 "sechszehnte" "siebzehnte" "achtzehnte"
                                 "neunzehnte"))

(DEFUN GERMAN-PRINT-THOUSAND (N STREAM ORDINAL-P)
  (LET ((N (CL:REM N 100.))
        (H (TRUNCATE N 100.)))
    (COND ((> H 0)
           (SEND STREAM :STRING-OUT (AREF GERMAN-SMALL (1- H)))
           (SEND STREAM :STRING-OUT "hundert")
           (AND ORDINAL-P (ZEROP N)
                (WRITE-CHAR #/e STREAM))))
    (COND ((= N 0))
          ((< N 20.)
           (SEND STREAM :STRING-OUT (AREF (IF ORDINAL-P GERMAN-ORDINAL-SMALL GERMAN-SMALL)
                                          (1- N))))
          (T
           (COND ((PLUSP (SETQ H (CL:REM N 10.)))
                  (SEND STREAM :STRING-OUT (AREF GERMAN-SMALL (1- H)))
                  (AND (= H 1) (WRITE-CHAR #/s STREAM))        ;Handle einsundzwanzig
                  (SEND STREAM :STRING-OUT "und")))
           (SEND STREAM :STRING-OUT (AREF GERMAN-MEDIUM (- (TRUNCATE N 10.) 2)))
           (AND ORDINAL-P (SEND STREAM :STRING-OUT "ste"))))))

(DEFUN GERMAN-PRINT (N &OPTIONAL (STREAM STANDARD-OUTPUT) (EINS-P T) ORDINAL-P)
  (COND ((ZEROP N)
         (SEND STREAM :STRING-OUT (IF ORDINAL-P "nullte" "zero")))
        ((< N 0)
         (SEND STREAM :STRING-OUT "minus")
         (WRITE-CHAR #/SPACE STREAM)
         (GERMAN-PRINT (MINUS N) STREAM))
        (T
         (DO ((N N)
              (P)
              (FLAG)
              (LIMIT (^ 10. 24.) (TRUNCATE LIMIT 1000.))
              (I 7 (1- I)))
             ((< I 0)
              (COND ((> N 0)
                     (AND FLAG (WRITE-CHAR #/space STREAM))
                     (GERMAN-PRINT-THOUSAND N STREAM ORDINAL-P)
                     (AND (= N 1) EINS-P (WRITE-CHAR #/s STREAM)))
                    ((AND ORDINAL-P FLAG)
                     (SEND STREAM :STRING-OUT "te"))))
           (COND ((NOT (< N LIMIT))
                  (SETQ P (TRUNCATE N LIMIT)
                        N (CL:REM N LIMIT))
                  (IF FLAG
                      (WRITE-CHAR #/space STREAM)
                    (SETQ FLAG T))
                  (GERMAN-PRINT P STREAM NIL)
                  (COND (( I 1)
                         (AND (= P 1)           ;Past 1M are feminine
                              (WRITE-CHAR #/e STREAM))
                         (WRITE-CHAR #/space STREAM))
                        (T
                         (SETQ FLAG NIL)))
                  (SEND STREAM :STRING-OUT (AREF GERMAN-LARGE I))))))))

(DEFPROP :GERMAN GERMAN-PRINC SI:PRINC-FUNCTION)
(DEFUN GERMAN-PRINC (N STREAM)
  (IF (OPERATION-HANDLED-P STREAM :SET-FONT-MAP)
      (LET ((OLD-FONT-MAP (SEND STREAM :FONT-MAP))
            (OLD-FONT (SEND STREAM :CURRENT-FONT)))
        (UNWIND-PROTECT
            (PROGN
              (SEND STREAM :SET-FONT-MAP '(FONTS:S35GER))
              (SEND STREAM :SET-CURRENT-FONT 0)
              (MULTIPLE-VALUE-BIND (X Y)
                  (SEND STREAM :READ-CURSORPOS)
                (SEND STREAM :SET-CURSORPOS X (MAX Y 40.)))
              (GERMAN-PRINT (IF (BIGP N) N (- N)) STREAM)
              (TERPRI STREAM))
          (SEND STREAM :SET-FONT-MAP OLD-FONT-MAP)
          (SEND STREAM :SET-CURRENT-FONT OLD-FONT)))
    (GERMAN-PRINT (IF (BIGP N) N (- N)) STREAM)))

(DEFPROP :ASK ASK-PRINC SI:PRINC-FUNCTION)

(DEFUN ASK-PRINC (N STREAM)
  (LET ((*PRINT-BASE* (OR (TV:MENU-CHOOSE '(("Decimal" . 10.)
                                            ("Octal" . 8.)
                                            ("Binary" . 2.)
                                            ("Roman" . :ROMAN)
                                            ("Roman Old" . :ROMAN-OLD)
                                            ("English" . :ENGLISH)
                                            ("German" . :GERMAN)))
                          10.)))
    (PRINC (- N) STREAM)))

(DEFCONST GERMAN-QUARTERS #("" "viertal " "halb " "dreiviertal "))

(DEFUN GERMAN-PRINT-TIME (HOURS MINUTES &OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((QUARTER (TRUNCATE MINUTES 15.))
        (MINUTES (CL:REM MINUTES 15.))
        (BEFORE-P))
    (IF (OR (> MINUTES 10.)
            (AND (> MINUTES 5) (= (CL:REM QUARTER 2) 1)))
        (SETQ QUARTER (1+ QUARTER)
              BEFORE-P T))
    (IF ( QUARTER 0)
        (SETQ HOURS (1+ HOURS)))
    (IF ( MINUTES 0)
        (FORMAT STREAM "~A ~:[nach~;vor~] "
                (AREF GERMAN-SMALL (IF BEFORE-P
                                       (- 14. MINUTES)
                                       (1- MINUTES)))
                BEFORE-P))
    (FORMAT STREAM "~A" (AREF GERMAN-QUARTERS (CL:REM QUARTER 4)))
    (IF (= (SETQ HOURS (CL:REM HOURS 24.)) 0)
        (FORMAT STREAM "mitnacht")
        (GERMAN-PRINT HOURS STREAM NIL))
    ))

(DEFUN WIEVIEL-UHR (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (AND (TIME:UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (NIL MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
           (TIME:GET-TIME)
         (SEND STREAM :STRING-OUT (TIME:DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK ':GERMAN))
         (SEND STREAM :STRING-OUT " das ")
         (GERMAN-PRINT DAY STREAM NIL T)
         (WRITE-CHAR #/SPACE STREAM)
         (SEND STREAM :STRING-OUT (TIME:MONTH-STRING MONTH ':GERMAN))
         (SEND STREAM :STRING-OUT ", ")
         (GERMAN-PRINT YEAR STREAM NIL)
         (FORMAT STREAM ";~%")
         (GERMAN-PRINT-TIME HOURS MINUTES))))

;;;-*-Mode:LISP; Package:USER; Base:8 -*-

(comment
(DECLARE (SPECIAL DL11-RCV-CSR DL11-RCV-DAT DL11-XMT-CSR DL11-XMT-DAT))
(SETQ DL11-RCV-CSR 775630)
(SETQ DL11-RCV-DAT (+ DL11-RCV-CSR 2)
      DL11-XMT-CSR (+ DL11-RCV-CSR 4)
      DL11-XMT-DAT (+ DL11-RCV-CSR 6))

(DEFUN DL11-TYI ()
  (PROCESS-WAIT "DL11-TYI" #'(LAMBDA () (LDB-TEST 0701 (%UNIBUS-READ DL11-RCV-CSR))))
  (LDB %%CH-CHAR (%UNIBUS-READ DL11-RCV-DAT)))

(DEFUN DL11-TYO (CH)
  (PROCESS-WAIT "DL11-TYO" #'(LAMBDA () (LDB-TEST 0701 (%UNIBUS-READ DL11-XMT-CSR))))
  (%UNIBUS-WRITE DL11-XMT-DAT CH))

(DEFUN DL11-STRING-OUT (STR)
  (DO ((I 0 (1+ I))
       (LEN (STRING-LENGTH STR)))
      (( I LEN))
    (DL11-TYO (AR-1 STR I))))  )

(declare (special votrax-stream))

(setq votrax-stream
              (si:make-serial-stream
                ':number-of-stop-bits 2
                ':number-of-data-bits 8
                ':check-parity-errors t
                ':check-framing-errors t
                ':baud 300.
                ':ascii-protocol t) )


(DECLARE (SPECIAL PHONEME-ALIST LAST-UTTERANCE))
(SETQ PHONEME-ALIST '((PA0 . 3) (PA1 . 76)
                      (A . 40) (A1 . 6) (A2 . 5)
                      (AE . 56) (AE1 . 57)
                      (AH . 44) (AH1 . 25) (AH2 . 10)
                      (AW . 75) (AW1 . 23) (AW2 . 60)
                      (AY . 41)
                      (B . 16)
                      (CH . 20)
                      (D . 36)
                      (DT . 4)
                      (E . 54) (E1 . 74)
                      (EH . 73) (EH1 . 2) (EH2 . 1) (EH3 . 0)
                      (ER . 72)
                      (F . 35)
                      (G . 34)
                      (H . 33)
                      (I . 47) (I1 . 13) (I2 . 12) (I3 . 11)
                      (IU . 66)
                      (J . 32)
                      (K . 31)
                      (L . 30)
                      (M . 14)
                      (N . 15)
                      (NG . 24)
                      (O . 46) (O1 . 65) (O2 . 64)
                      (OO . 27) (OO1 . 26)
                      (P . 45)
                      (R . 53)
                      (S . 37)
                      (SH . 21)
                      (T . 52)
                      (TH . 71)
                      (THV . 70)
                      (U . 50) (U1 . 67)
                      (UH . 63) (UH1 . 62) (UH2 . 61) (UH3 . 43)
                      (V . 17)
                      (W . 55)
                      (Y . 51) (Y1 . 42)
                      (Z . 22)
                      (ZH . 7)))

(DEFUN SPEAK-1 (LIST)
  (DO ((LIST LIST (CDR LIST))
       (INT 300)
       (PH))
      ((NULL LIST) (funcall votrax-stream ':TYO -1) T)
    (SETQ PH (CAR LIST))
    (COND ((NUMBERP PH)
           (SETQ INT (- 400 (* PH 100))))
          (T
           (funcall votrax-stream ':tyo (+ INT (CDR (ASSQ PH PHONEME-ALIST))))))))

(DEFUN SPEAK (&OPTIONAL (X LAST-UTTERANCE))
  (SPEAK-1 (SETQ LAST-UTTERANCE X)))

(DEFUN SPEAK-WORDS (&QUOTE &REST LIST-OF-WORDS)
  (DOLIST (WORD LIST-OF-WORDS)
    (SPEAK-1 (OR (GET WORD ':VOTRAX-WORD)
                 (SPEAK-WORDS-GET-WORD WORD)))))

(DEFUN SPEAK-WORD (WORD)
  (SPEAK-1 (OR (GET WORD ':VOTRAX-WORD)
               (SPEAK-WORDS-GET-WORD WORD))))

(DEFUN SPEAK-WORDS-GET-WORD (WORD)
  (FORMAT T "~%I don't know how to say ~A, please tell me: " WORD)
  (PUTPROP WORD (READ) ':VOTRAX-WORD))

(DECLARE (SPECIAL WORD-FILE))
(DEFUN DUMP-WORD-FILE (FILENAME)
  (SETQ WORD-FILE (OPEN FILENAME '(:OUT :ASCII)))
  (MAPATOMS 'DUMP-WORD-FILE-1)
  (CLOSE WORD-FILE))

(DEFUN DUMP-WORD-FILE-1 (ATOM &AUX (WORD-SAYING (GET ATOM ':VOTRAX-WORD)))
  (AND WORD-SAYING
       (PRINT `(DEFPROP ,ATOM ,WORD-SAYING :VOTRAX-WORD) WORD-FILE)))

(DEFUN LOAD-WORD-FILE (FILENAME)
  (READFILE FILENAME))

(DEFUN SPEAK-SENTENCE (SENT)
  (DO ((I 0 (1+ EOW))
       (EOW (STRING-SEARCH-SET '(#\SP #/-) SENT)
            (STRING-SEARCH-SET '(#\SP #/-) SENT (1+ EOW))))
      (())
    (SPEAK-WORD (INTERN (STRING-UPCASE (NSUBSTRING SENT I EOW)) ""))
    (OR EOW (RETURN NIL))))

(DEFUN (:VOTRAX SI:PRINC-FUNCTION) (X STREAM)
  (COND ((EQ (TYPEP X) ':FIXNUM)
         (SPEAK-SENTENCE (FORMAT NIL "~R" (- X)))
         (FORMAT STREAM "~D" (- X)))
        (T (FORMAT STREAM "~D" X))))

(DEFUN SPEAK-RAN (N)
  (DOTIMES (I N)
    (funcall votrax-stream ':TYO (RANDOM 400)))
  (funcall votrax-stream ':TYO -1))

(DEFUN OPERATOR ()
  (SPEAK-SENTENCE "THE NUMBER YOU HAVE REACHED <> TWO FIVE THREE <> SIX SEVEN SIX FIVE <> IS NOT IN SERVICE <> PLEASE CHECK THE NUMBER AND DIAL AGAIN OR ASK YOUR OPERATOR FOR ASSISTANCE <> <> THIS IS A RECORDING"))

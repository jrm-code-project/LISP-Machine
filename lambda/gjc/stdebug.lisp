;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Patch-File:T; Base:8 -*-

(DEFVAR *STRING-PRINLENGTH* 50)


;;; Print a string, and if slashification is on, slashify it appropriately.
(DEFUN PRINT-QUOTED-STRING (STRING STREAM FASTP &AUX LENGTH CHAR)
  (COND ((NOT *PRINT-ESCAPE*)
         (PRINT-RAW-STRING STRING STREAM FASTP))
        (T
         (SEND STREAM ':TYO (PTTBL-OPEN-QUOTE-STRING *READTABLE*))
         (SETQ LENGTH (ARRAY-ACTIVE-LENGTH STRING))
         (COND ((AND (EQ (ARRAY-TYPE STRING) 'ART-STRING)
                     (DOTIMES (I LENGTH T)
                       (AND (< (SETQ CHAR (AREF STRING I)) 220)
                            (NOT (ZEROP (LOGAND 16 (RDTBL-BITS *READTABLE* CHAR))))
                            (RETURN NIL))))
                ;; There are no double quotes, and so no slashifying.
                (IF (NOT *STRING-PRINLENGTH*)
                    (SEND STREAM :STRING-OUT STRING)
                  (SEND STREAM ':STRING-OUT STRING 0 (MIN *STRING-PRINLENGTH* (LENGTH STRING)))))
               (T
                (DOTIMES (I (IF *STRING-PRINLENGTH* (MIN *STRING-PRINLENGTH* LENGTH) LENGTH))
                  (SETQ CHAR (LDB %%CH-CHAR (AREF STRING I)))
                  (COND ((AND (< CHAR 220)
                              (NOT (ZEROP (LOGAND 16 (RDTBL-BITS *READTABLE* CHAR)))))
                         (SEND STREAM ':TYO (PTTBL-SLASH *READTABLE*))))
                    (SEND STREAM ':TYO CHAR))))
         (SEND STREAM ':TYO (PTTBL-CLOSE-QUOTE-STRING *READTABLE*))
         )))

;;; Print the string, with no slashification at all.
(DEFUN PRINT-RAW-STRING (STRING STREAM FASTP)
  (COND ((AND FASTP (EQ (ARRAY-TYPE STRING) 'ART-STRING))
         (SEND STREAM ':STRING-OUT STRING
               0
               (IF *STRING-PRINLENGTH* (MIN *STRING-PRINLENGTH* (LENGTH STRING))
                 (LENGTH STRING))))
        (T
         (DOTIMES (I (IF *STRING-PRINLENGTH* (MIN *STRING-PRINLENGTH* (LENGTH STRING))
                       (LENGTH STRING)))
           (SEND STREAM ':TYO (CHAR-CODE (AREF STRING I)))))))

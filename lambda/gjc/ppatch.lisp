;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Patch-File:T; Readtable:ZL -*-


(DEFVAR *STRING-PRINLENGTH* NIL)

;;; Print a string, and if slashification is on, slashify it appropriately.
(DEFUN PRINT-QUOTED-STRING (STRING STREAM FASTP &AUX LENGTH CHAR HACK)
  (COND ((NOT *PRINT-ESCAPE*)
         (PRINT-RAW-STRING STRING STREAM FASTP))
        (T
         (SEND STREAM ':TYO (PTTBL-OPEN-QUOTE-STRING *READTABLE*))
         (SETQ LENGTH (ARRAY-ACTIVE-LENGTH STRING))
         (WHEN (AND *STRING-PRINLENGTH* (> length *STRING-PRINLENGTH*))
           (SETQ LENGTH *STRING-PRINLENGTH*)
           (SETQ HACK T))
         (COND ((AND (EQ (ARRAY-TYPE STRING) 'ART-STRING)
                     (DOTIMES (I LENGTH T)
                       (AND (< (SETQ CHAR (AREF STRING I)) 220)
                            (NOT (ZEROP (LOGAND 16 (RDTBL-BITS *READTABLE* CHAR))))
                            (RETURN NIL))))
                ;; There are no double quotes, and so no slashifying.
                (SEND STREAM ':STRING-OUT STRING))
               (T
                (DOTIMES (I LENGTH)
                  (SETQ CHAR (LDB %%CH-CHAR (AREF STRING I)))
                  (COND ((AND (< CHAR 220)
                              (NOT (ZEROP (LOGAND 16 (RDTBL-BITS *READTABLE* CHAR)))))
                         (SEND STREAM ':TYO (PTTBL-SLASH *READTABLE*))))
                    (SEND STREAM ':TYO CHAR))))
         (WHEN HACK (SEND STREAM :STRING-OUT " etc..."))
         (SEND STREAM ':TYO (PTTBL-CLOSE-QUOTE-STRING *READTABLE*))
         )))

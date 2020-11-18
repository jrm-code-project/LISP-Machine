;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by pace
;;; Reason:
;;;
;;; Written 31-Jul-86 11:06:56 by pace at site LMI Cambridge
;;; while running on Lene Lovich from band 1
;;; with Experimental System 116.23, Experimental Lambda-Diag 10.1, Experimental Local-File 70.0, Experimental FILE-Server 19.0, microcode 1662, SDU Boot Tape 3.14, SDU ROM 102.



; From modified file DJ: L.IO; PRINT.LISP#216 at 31-Jul-86 11:07:09
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; PRINT  "

(defvar si:*string-max-length* 512.)
(DEFUN PRINT-QUOTED-STRING (STRING STREAM &OPTIONAL IGNORE &AUX LENGTH CHAR)
  (when (> (array-active-length string) *string-max-length*)
    (setq string (string-append (substring string 0 *string-max-length*) "...")))
  (IF (NOT *PRINT-ESCAPE*)
      (PRINT-RAW-STRING STRING STREAM)
    (SEND STREAM :TYO (PTTBL-OPEN-QUOTE-STRING *READTABLE*))
    (SETQ LENGTH (LENGTH STRING))
    (COND ((AND (EQ (ARRAY-TYPE STRING) 'ART-STRING)
                (DOTIMES (I LENGTH T)
                  (AND (< (SETQ CHAR (AREF STRING I)) #o220)
                       (NOT (ZEROP (LOGAND #o16 (RDTBL-BITS *READTABLE* CHAR))))
                       (RETURN NIL))))
           ;; There are no double quotes, and so no slashifying.
           (SEND STREAM :STRING-OUT STRING))
          (T
           (DOTIMES (I LENGTH)
             (SETQ CHAR (CHAR-CODE (CHAR STRING I)))
             (COND ((AND (< CHAR #o220)
                         (NOT (ZEROP (LOGAND #o16 (RDTBL-BITS *READTABLE* CHAR)))))
                    (SEND STREAM :TYO (PTTBL-SLASH *READTABLE*))))
             (SEND STREAM :TYO CHAR))))
    (SEND STREAM :TYO (PTTBL-CLOSE-QUOTE-STRING *READTABLE*))))
))

;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Patch-File:T; Readtable:ZL -*-


;;; KLUDGES.
;;; NIL USES #\^G SYNTAX.

(DEFUN XR-#\-MACRO (STREAM IGNORE &OPTIONAL BITS)
  (MULTIPLE-VALUE-BIND (NIL NIL CHAR)
      (XR-XRTYI STREAM NIL T)
    (LOGIOR
      (%LOGDPB (OR BITS 0) %%KBD-CONTROL-META 0)
      (COND ((= CHAR #/^)
             (SEND STREAM :UNTYI CHAR)
             (PKG-BIND PKG-KEYWORD-PACKAGE
               (LET ((FROB (INTERNAL-READ STREAM T NIL T)))
                 (COND (*READ-SUPPRESS*
                        0)
                       ((STRING-EQUAL FROB "^")
                        #/^)
                       ('ELSE
                        (logxor #o100 (char-upcase (AREF (STRING FROB) 1))))))))
            ((NOT (OR ( #/A CHAR #/Z) ( #/a CHAR #/z)))
             CHAR)
            ('ELSE
             (SEND STREAM :UNTYI CHAR)
             (PKG-BIND PKG-KEYWORD-PACKAGE
               (LET ((FROB (INTERNAL-READ STREAM T NIL T)))     ;Get symbolic name of character
                 (IF *READ-SUPPRESS* 0          ;READ returns NIL in this case; don't bomb.
                   (IF (= (STRING-LENGTH FROB) 1)
                       XR-XRTYI-PREV-CHAR
                     (OR (CDR (ASSQ FROB XR-SPECIAL-CHARACTER-NAMES))
                         (XR-PARSE-KEYBOARD-CHAR FROB)
                         (READ-ERROR
                           "#\~A is not a defined character-name." FROB)))))))))))

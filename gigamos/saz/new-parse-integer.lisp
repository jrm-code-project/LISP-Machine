;;; -*- Mode:LISP; Base:10; Readtable:CL -*-
(DEFUN PARSE-INTEGER (STRING &KEY (START 0) END (RADIX 10.) JUNK-ALLOWED)
  "Return a number parsed from the contents of STRING, or a part of it.
START and END are indices specify the part of the string; END = NIL means the end of it.
RADIX is a number from 2 to 36, which defaults to ten.

If JUNK-ALLOWED is NIL, the string must contain simply a number
surrounded by whitespace.  The number must be just digits (including suitable
letters if RADIX is > 10) and an optional sign.  Otherwise it is an error.

If JUNK-ALLOWED is non-NIL, parsing stops when anything is encountered that
doesn't fit that description, or at non-leading whitespace.
If no number has been found by then, the value is NIL.

The second value is the index in STRING of where parsing stopped."
  (DECLARE (VALUES NUMBER-OR-MAYBE-NIL STOP-INDEX))
  (SETQ STRING (STRING STRING))
  (LET ((INDEX START)
        (STOP (OR END (LENGTH STRING)))
        NUM SIGN TEM letter)
    ;; Skip initial whitespace.
    (DO () ((= INDEX STOP))
      (LET ((CH (CHAR STRING INDEX)))
;character lossage font
        (UNLESS (MEMQ CH '(#/SP #/TAB))
          (RETURN))
        (INCF INDEX)))
    (DO ()
        ((= INDEX STOP))
      (LET ((CH (CHAR-UPCASE (CHAR STRING INDEX))))
        (COND ((SETQ TEM (or (and (> radix 10.) ;calculate digit values for calls with high radix values here
                                  (< radix 37.) ;radix must be between 2 and 36, inclusive...
                                  (setq letter (string-search ch
                                                              "ABCDEFGHIJKLMNOPQRSTUVWXYZ"      ;all 26 needed for base 36...
                                                              0
                                                              (- radix 10.)))   ;use only the legal letters for this radix...
                                  letter        ;string-search returned nil if letter was not a legal one for this radix...
                                  (+ 10. letter))       ;add the proper amount for the letter given
                             (DIGIT-CHAR-P CH)))
               (SETQ NUM (+ (* (OR NUM 0) RADIX) TEM)))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/+)) (SETQ SIGN 1))
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/-)) (SETQ SIGN -1))
              (T
               (UNLESS JUNK-ALLOWED
                 ;; No junk allowed.  Skip any trailing whitespace.
                 (DO () ((= INDEX STOP))
;character lossage font
                   (LET ((CH (CHAR STRING INDEX)))
                     (UNLESS (MEMQ CH '(#/SP #/TAB))
                       (RETURN))
                     (INCF INDEX)))
                 ;; If this did not get us to the end, barf.
                 (UNLESS (= INDEX STOP)
                   (FERROR "~S does not contain simply a number surrounded by whitespace."
                           STRING)))
               (RETURN)))
        (INCF INDEX)))
    (VALUES (AND NUM (IF (EQ SIGN -1) (- NUM) NUM))
            INDEX)))

(DEFUN PARSE-NUMBER (STRING &OPTIONAL (FROM 0) TO (RADIX 10.) FAIL-IF-NOT-WHOLE-STRING)
  "Return a number parsed from the contents of STRING, or a part of it.
FROM and TO specify the part of the string; TO = NIL means the end of it.
RADIX defaults to decimal.

If the string or part doesn't start with a number, NIL is returned.
The second value is the index in STRING of the first non-digit, or NIL if none.
FAIL-IF-NOT-WHOLE-STRING means return NIL and 0 unless the whole string or
specified part can be parsed as a number."
  (LOOP WITH SIGN = NIL
        WITH NUM = NIL
        with letter = nil
        WITH RADIX = (OR RADIX 10.)
        WITH TEM
        FOR I FROM FROM BELOW (OR TO (STRING-LENGTH STRING))
        FOR CH = (CHAR STRING I)
     DO (COND ((SETQ TEM  (or (and (> radix 10.)
                                   (< radix 37.)
                                  (setq letter (string-search ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 (- radix 10.)))
                                  letter
                                  (+ 10. letter))
                             (DIGIT-CHAR-P CH)))
               (SETQ NUM (+ (* (OR NUM 0) RADIX) TEM)))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/+)) (SETQ SIGN 1))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/-)) (SETQ SIGN -1))
              (T
               (IF FAIL-IF-NOT-WHOLE-STRING
                   (RETURN (VALUES NIL 0))
                 (LOOP-FINISH))))
     FINALLY (RETURN (VALUES (AND NUM (IF (EQ SIGN -1) (- NUM) NUM))
                             (IF NUM I 0)))))

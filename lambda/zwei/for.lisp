;;;-*- Mode:LISP; Package:ZWEI; Readtable:T; Base:8 -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Functions in this file know about bps, lines, and intervals.
;;; They use *INTERVAL* for their limit-checking.

;;; Standard motion functions.

(DEFUN FORWARD-CHAR (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is TIMES characters forward from BP.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES)
         (COPY-BP BP))
        ((> TIMES 0)
         (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
              (INDEX (BP-INDEX BP) 0)
              (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
              (LAST-INDEX (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))))
             (NIL)
           (LET ((LL (LINE-LENGTH LINE))
                 (I (+ INDEX TIMES)))
             (COND ((AND (EQ LINE LAST-LINE)
                         (> I LAST-INDEX))
                    (RETURN (IF FIXUP-P (CREATE-BP LINE LAST-INDEX) NIL)))
                   (( I LL)
                    (RETURN (CREATE-BP LINE I))))
             (SETQ TIMES (- TIMES (- LL INDEX) 1)))))
        (T
         (SETQ TIMES (- TIMES))
         (DO ((LINE (BP-LINE BP))
              (INDEX (- (BP-INDEX BP) TIMES))
              (LINE-LENGTH (BP-INDEX BP))
              (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
              (FIRST-INDEX (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))))
             (NIL)
           (COND ((AND (EQ LINE FIRST-LINE) (< INDEX FIRST-INDEX))
                  (RETURN (IF FIXUP-P (CREATE-BP FIRST-LINE FIRST-INDEX) NIL)))
                 (( INDEX 0)
                  (RETURN (CREATE-BP LINE INDEX))))
           (SETQ TIMES (- TIMES LINE-LENGTH 1)
                 LINE (LINE-PREVIOUS LINE)
                 LINE-LENGTH (LINE-LENGTH LINE)
                 INDEX (- LINE-LENGTH TIMES))))))

(DEFUN FORWARD-ITS-CHAR (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is TIMES ITS-equivalent characters forward from BP.
Counting characters as ITS-equivalent means count a line break as two chars.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES)
         (COPY-BP BP))
        ((> TIMES 0)
         (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
              (INDEX (BP-INDEX BP) 0)
              (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
              (LAST-INDEX (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))))
             (NIL)
           (LET ((LL (LINE-LENGTH LINE))
                 (I (+ INDEX TIMES)))
             (COND ((AND (EQ LINE LAST-LINE)
                         (> I LAST-INDEX))
                    (RETURN (IF FIXUP-P (CREATE-BP LINE LAST-INDEX) NIL)))
                   (( I LL)
                    (RETURN (CREATE-BP LINE I))))
             (SETQ TIMES (MAX 0 (- TIMES (- LL INDEX) 2))))))
        (T
         (SETQ TIMES (- TIMES))
         (DO ((LINE (BP-LINE BP))
              (INDEX (- (BP-INDEX BP) TIMES))
              (LINE-LENGTH (BP-INDEX BP))
              (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
              (FIRST-INDEX (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))))
             (NIL)
           (COND ((AND (EQ LINE FIRST-LINE) (< INDEX FIRST-INDEX))
                  (RETURN (IF FIXUP-P (CREATE-BP FIRST-LINE FIRST-INDEX) NIL)))
                 (( INDEX 0)
                  (RETURN (CREATE-BP LINE INDEX))))
           (SETQ TIMES (MAX 0 (- TIMES LINE-LENGTH 2))
                 LINE (LINE-PREVIOUS LINE)
                 LINE-LENGTH (LINE-LENGTH LINE)
                 INDEX (- LINE-LENGTH TIMES))))))

(DEFUN FORWARD-LINE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is TIMES lines forward from BP.
Zero means the front of the same line.
Otherwise the result is at the beginning of a line.
TIMES may be negative meaning go up.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (DO ((LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
              (LINE (BP-LINE BP) (LINE-NEXT LINE))
              (I 0 (1+ I)))
             (( I TIMES)
              (CREATE-BP LINE 0))
           (COND ((EQ LINE LAST-LINE)
                  (RETURN (IF FIXUP-P
                              (CREATE-BP LINE 0)
                              NIL))))))
        (T
         (DO ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
              (LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
              (I 0 (1- I)))
             (( I TIMES)
              (UNLESS LINE (FERROR NIL "Report this ZWEI bug thoroughly!  Get RMS if he's here."))
              (CREATE-BP LINE (IF (EQ LINE FIRST-LINE)
                                  (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
                                  0)))
           (COND ((EQ LINE FIRST-LINE)
                  (RETURN (IF FIXUP-P
                              (CREATE-BP LINE (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
                              NIL))))))))

(DEFUN FORWARD-LIST (BP &OPTIONAL (TIMES 1) FIXUP-P (LEVEL 0) DOWNP NO-UP-P
                        &AUX (ORIGINAL-LEVEL LEVEL) (STATE 'NORMAL))
  "Return a bp which is TIMES lists forward from BP.
Atoms and singlequote-like characters are ignored.  Comments are not ignored.
TIMES may be negative meaning go backward.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
LEVEL nonzero means move up that many levels of list structure.
LEVEL negative means move down; but DOWNP should be T.
NO-UP-P means it is forbidden to move up and then down again.
 NIL is returned if that starts to happen."
  (AND (LISP-BP-SYNTACTIC-CONTEXT BP)
       ;; If point is inside a string, move past the string entirely,
       ;; unless the supposed string is really a #| ... |#,
       ;; in which case move down in the code inside that construct.
       (OR (AND DOWNP (LOOKING-AT (FORWARD-UP-STRING BP T) "#|"))
           (SETQ STATE 'STRING)))
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (LET ((TIME 0)
               (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
           (CHARMAP (BP LAST-BP (IF FIXUP-P (COPY-BP LAST-BP) NIL))
            RESTART
             (LET ((SYNTAX (LIST-SYNTAX (CHARMAP-CHARACTER))))
               (SELECTQ STATE
                 (STRING
                  (SELECT SYNTAX
                    (LIST-DOUBLE-QUOTE
                     (SETQ STATE 'NORMAL))
                    (LIST-SLASH
                     (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))))
                 (NORMAL
                  (SELECT SYNTAX
                    (LIST-SLASH
                     (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))
                    (LIST-DOUBLE-QUOTE
                     (SETQ STATE 'STRING))
                    (LIST-CLOSE
                     (SETQ LEVEL (1- LEVEL))
                     (COND (DOWNP
                            (COND ((< LEVEL ORIGINAL-LEVEL)
                                   (CHARMAP-RETURN (IF FIXUP-P (COPY-BP LAST-BP) NIL)))))
                           ((AND NO-UP-P (< LEVEL 0))
                            (CHARMAP-RETURN NIL))
                           (( LEVEL 0)
                            (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                (CHARMAP-RETURN (CHARMAP-BP-AFTER))))))
                    (LIST-OPEN
                     (COND ((AND ( (SETQ LEVEL (1+ LEVEL)) 0) DOWNP)
                            (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                (CHARMAP-RETURN (CHARMAP-BP-AFTER)))))))))))))
        (T
         (LET ((TIME 0)
               (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
           (RCHARMAP (BP FIRST-BP (IF FIXUP-P (COPY-BP FIRST-BP) NIL))
            RESTART
             (OR (= (LIST-SYNTAX (RCHARMAP-CHAR-BEFORE)) LIST-SLASH)
                 (LET ((SYNTAX (LIST-SYNTAX (RCHARMAP-CHAR))))
                   (SELECTQ STATE
                     (STRING
                      (SELECT SYNTAX
                        (LIST-DOUBLE-QUOTE
                         (SETQ STATE 'NORMAL))))
                     (NORMAL
                      (SELECT SYNTAX
                        (LIST-DOUBLE-QUOTE
                         (SETQ STATE 'STRING))
                        (LIST-CLOSE
                         (AND ( (SETQ LEVEL (1+ LEVEL)) 0) DOWNP
                              (IF ( (SETQ TIME (1- TIME)) TIMES)
                                  (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE)))))
                        (LIST-OPEN
                         (SETQ LEVEL (1- LEVEL))
                         (AND NO-UP-P (< LEVEL 0) (RCHARMAP-RETURN NIL))
                         (AND ( LEVEL 0) (NOT DOWNP)
                              (IF ( (SETQ TIME (1- TIME)) TIMES)
                                  (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE)))))))))))))))

(DEFUN LINE-OPENS-PARENS (LINE)
  "T if LINE contains open parens that are not matched within LINE."
  (DO ((I (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
              (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
              0)
          (1+ I))
       (LIM (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
                (LINE-LENGTH LINE)))
       (STATE 'NORMAL)
       (LEVEL 0))
      (( I LIM) (> LEVEL 0))
    (LET* ((CH (LDB %%CH-CHAR (AREF LINE I)))
           (SYNTAX (LIST-SYNTAX CH)))
      (SELECTQ STATE
        (STRING (SELECT SYNTAX
                  (LIST-DOUBLE-QUOTE (SETQ STATE 'NORMAL))
                  (LIST-SLASH (SETQ I (1+ I)))))
        (NORMAL (SELECT SYNTAX
                  (LIST-SLASH (SETQ I (1+ I)))
                  (LIST-DOUBLE-QUOTE (SETQ STATE 'STRING))
                  (LIST-CLOSE (SETQ LEVEL (MAX (1- LEVEL) 0)))
                  (LIST-OPEN (SETQ LEVEL (1+ LEVEL)))))))))

(DEFUN FORWARD-WORD (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is forward across TIMES words from BP.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (LET ((STATE NIL)
               (TIME 0)
               (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
           (CHARMAP (BP LAST-BP (IF (OR FIXUP-P
                                        (AND STATE (= (1+ TIME) TIMES)))
                                    (COPY-BP LAST-BP)
                                    NIL))
             (LET ((SYNTAX (WORD-SYNTAX (CHARMAP-CHAR))))
               (SELECTQ STATE
                 (NIL
                  (SELECT SYNTAX                ;use select in case more word syntax types
                    (WORD-ALPHABETIC            ;appear
                     (SETQ STATE T))))
                 (T
                  (SELECT SYNTAX
                    (WORD-DELIMITER
                     (SETQ TIME (1+ TIME))
                     (IF ( TIME TIMES)
                         (CHARMAP-RETURN (CHARMAP-BP-BEFORE))
                         (SETQ STATE NIL))))))))))
        (T
         (LET ((STATE NIL)
               (TIME 0)
               (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
           (RCHARMAP (BP FIRST-BP (IF (OR FIXUP-P
                                          (AND STATE (= (1- TIME) TIMES)))
                                      (COPY-BP FIRST-BP)
                                      NIL))
             (LET ((SYNTAX (WORD-SYNTAX (RCHARMAP-CHAR))))
               (SELECTQ STATE
                 (NIL
                  (SELECT SYNTAX
                    (WORD-ALPHABETIC
                     (SETQ STATE T))))
                 (T
                  (SELECT SYNTAX
                    (WORD-DELIMITER
                     (SETQ TIME (1- TIME))
                     (IF ( TIME TIMES)
                         (RCHARMAP-RETURN (RCHARMAP-BP-AFTER))
                         (SETQ STATE NIL))))))))))))

(DEFUN FORWARD-TO-WORD (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is forward up to the TIMES word-beginning from BP.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (*CATCH 'LOSSAGE
    (COND ((ZEROP TIMES) (COPY-BP BP))
          ((PLUSP TIMES)
           (LET ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
             (COND ((> TIMES 1)
                    (SETQ BP (FORWARD-WORD BP (1- TIMES)))
                    (COND ((NULL BP)
                           (*THROW 'LOSSAGE (IF FIXUP-P (COPY-BP LAST-BP) NIL))))))
             (CHARMAP (BP LAST-BP (IF FIXUP-P (COPY-BP LAST-BP) NIL))
               (LET ((SYNTAX (WORD-SYNTAX (CHARMAP-CHAR))))
                 (SELECT SYNTAX
                    (WORD-ALPHABETIC
                     (CHARMAP-RETURN (CHARMAP-BP-BEFORE))))))))
          (T
           (LET ((FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
             (COND ((< TIMES -1)
                    (SETQ BP (FORWARD-WORD BP (1+ TIMES)))
                    (COND ((NULL BP)
                           (*THROW 'LOSSAGE (IF FIXUP-P (COPY-BP FIRST-BP) NIL))))))
             (RCHARMAP (BP FIRST-BP (IF FIXUP-P (COPY-BP FIRST-BP) NIL))
               (LET ((SYNTAX (WORD-SYNTAX (RCHARMAP-CHAR))))
                 (SELECT SYNTAX
                    (WORD-ALPHABETIC
                     (RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))))))))))

(DEFUN FORWARD-DEFUN (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is forward across TIMES defuns from BP.
If BP is within a defun, that is included in the count.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (DO-NAMED LUPO
             ((I 0 (1+ I)))
             (( I TIMES)
              BP)
           (DO () (NIL)
             (SETQ BP (BEG-LINE BP 1))
             (COND ((NULL BP)
                    (RETURN-FROM LUPO (IF FIXUP-P
                                          (COPY-BP (INTERVAL-LAST-BP *INTERVAL*))
                                          NIL)))
                   ((= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN)
                    (RETURN NIL))))))
        (T
         (DO-NAMED LUPO
             ((I 0 (1- I)))
             (( I TIMES)
              BP)
           (DO ((FIRSTP T NIL)) (NIL)
             (SETQ BP (BEG-LINE BP (IF (AND FIRSTP (NOT (BEG-LINE-P BP)))
                                       0
                                       -1)))
             (COND ((NULL BP)
                    (RETURN-FROM LUPO (IF FIXUP-P
                                          (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))
                                          NIL)))
                   ((= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN)
                    (RETURN NIL))))))))

(DEFUN FORWARD-PAGE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is TIMES page-separators forward from BP.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (LET ((STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
               (FIRST-LINE (BP-LINE BP)))
           (COND ((EQ FIRST-LINE STOP-LINE)
                  (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*))))
                 (T (DO ((LINE (LINE-NEXT FIRST-LINE) (LINE-NEXT LINE)))
                        ((EQ LINE STOP-LINE)
                         (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))
                      (COND ((AND ( (LINE-LENGTH LINE) 1)
                                  (MEMQ (LDB %%CH-CHAR (AREF LINE 0)) *PAGE-DELIMITER-LIST*))
                             (AND ( (SETQ TIMES (1- TIMES)) 0)
                                  (RETURN (CREATE-BP LINE 1))))))))))
        (T
         (LET ((STOP-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
               (FIRST-LINE (BP-LINE BP)))
           (COND ((EQ FIRST-LINE STOP-LINE)
                  (AND FIXUP-P (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))))
                 (T (DO ((LINE (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE)))
                        ((EQ LINE STOP-LINE)
                         (AND FIXUP-P (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))))
                      (COND ((AND ( (LINE-LENGTH LINE) 1)
                                  (MEMQ (LDB %%CH-CHAR (AREF LINE 0)) *PAGE-DELIMITER-LIST*))
                             (AND ( (SETQ TIMES (1+ TIMES)) 0)
                                  (RETURN (CREATE-BP LINE 1))))))))))))

(DEFUN FORWARD-INTERVAL (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp to the beginning or end of *INTERVAL*.
The sign of TIMES determines which.
FIXUP-P is ignored; allowed for compatibility with other FORWARD-functions."
  BP FIXUP-P                                    ;Never out of range
  (COPY-BP (IF (MINUSP TIMES)
               (INTERVAL-FIRST-BP *INTERVAL*)
             (INTERVAL-LAST-BP *INTERVAL*))))

(DEFUN FORWARD-PARAGRAPH (BP &OPTIONAL (TIMES 1) FIXUP-P
                             &AUX BACKWARD-P
                                  (FILL-PREFIX-P (PLUSP (STRING-LENGTH *FILL-PREFIX*)))
                                  BLANK-P PREV-BLANK-P)
  "Return a bp to the TIMES'th paragraph-end forward from BP.
TIMES negative means move backwards past that many paragraph-beginnings.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (AND (MINUSP TIMES) (SETQ TIMES (- TIMES) BACKWARD-P T))
  (COND ((NOT BACKWARD-P)                       ;Move to the beginning of a line
         (SETQ BP (BEG-LINE BP)))
        ((NOT (BEG-LINE-P BP))
         (SETQ BP (BEG-LINE BP 1 T)))
        (T
         (LET ((PREV-BP (BEG-LINE BP -1)))
           (AND PREV-BP (NOT (LINE-BLANK-OR-DIAGRAM-P (BP-LINE PREV-BP)))
                (BP-LOOKING-AT-LIST PREV-BP *PAGE-DELIMITER-LIST*)
                (SETQ BP PREV-BP)))))
  (DO ((I 0 (1+ I)))
      ((OR (NULL BP) ( I TIMES)) BP)
    (SETQ BLANK-P T)
    (DO ((FIRST-P (IF (AND BACKWARD-P (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))) 1 0)
                  (1+ FIRST-P)))
        (NIL)
      (SETQ PREV-BLANK-P BLANK-P BLANK-P NIL)
      (AND (SETQ BLANK-P (BP-AT-PARAGRAPH-DELIMITER BP FILL-PREFIX-P))
           (NOT PREV-BLANK-P)
           (OR (> FIRST-P 1)
               (NOT BACKWARD-P))
           (RETURN))
      (COND ((NOT (IF BACKWARD-P
                      (OR (SETQ BP (BEG-LINE BP -1)) (RETURN))
                      (OR (SETQ BP (BEG-LINE BP 1)) (RETURN))
                      (NOT BLANK-P))))
            ((AND BACKWARD-P
                  (LINE-PREVIOUS (BP-LINE BP))
                  (LINE-BLANK-P (LINE-PREVIOUS (BP-LINE BP))))
             ;; Don't stop at a nonblank line (such as an indented one) if prev one is blank.
             )
            ((BP-AT-PARAGRAPH-STARTER BP FILL-PREFIX-P)
             (RETURN)))))
  ;; If going backwards and stopped at a nonblank line which isn't part of this paragraph,
  ;; move to beginning of the following line.
  (AND BP BACKWARD-P (NOT (LINE-BLANK-P (BP-LINE BP)))
       (BP-AT-PARAGRAPH-DELIMITER BP FILL-PREFIX-P)
       (SETQ BP (BEG-LINE BP 1 T)))
  (OR BP
      (AND FIXUP-P
           (COPY-BP (IF BACKWARD-P (INTERVAL-FIRST-BP *INTERVAL*)
                        (INTERVAL-LAST-BP *INTERVAL*))))))

(DEFUN BP-AT-PARAGRAPH-STARTER (BP &OPTIONAL
                                (FILL-PREFIX-P (NOT (ZEROP (STRING-LENGTH *FILL-PREFIX*)))))
  "T if BP is on a line that starts a paragraph."
  (IF FILL-PREFIX-P
      (NOT (LOOKING-AT BP *FILL-PREFIX*))
    (AND (NOT (BP-LOOKING-AT-LIST BP *TEXT-JUSTIFIER-ESCAPE-LIST*))
         (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
             (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*)))))

(DEFUN BP-AT-PARAGRAPH-DELIMITER (BP &OPTIONAL
                                  (FILL-PREFIX-P (NOT (ZEROP (STRING-LENGTH *FILL-PREFIX*)))))
  "T if BP is on a line which separates paragraphs and is not part of them."
  (OR (LINE-BLANK-OR-DIAGRAM-P (BP-LINE BP))
      (IF FILL-PREFIX-P
          NIL
        (AND (BP-LOOKING-AT-LIST BP *TEXT-JUSTIFIER-ESCAPE-LIST*)
             (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
                 (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*))))))

(DEFUN BP-AT-PARAGRAPH-TERMINATOR (BP &OPTIONAL
                                   (FILL-PREFIX-P (NOT (ZEROP (STRING-LENGTH *FILL-PREFIX*)))))
  "T if BP is on a line which terminates a preceding paragraph."
  (OR (LINE-BLANK-OR-DIAGRAM-P (BP-LINE BP))
      (IF FILL-PREFIX-P
          (NOT (LOOKING-AT BP *FILL-PREFIX*))
        (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
            (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*)))))

(DEFUN FORWARD-OVER-BLANK-OR-TEXT-JUSTIFIER-LINES (BP)
  "Return a bp to the first non-paragraph-terminating line past BP,
or to the end of *INTERVAL*."
  (DO ((BP BP (BEG-LINE BP 1)))
      ((OR (NULL BP) (NOT (BP-AT-PARAGRAPH-DELIMITER BP)))
       (OR BP (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN FORWARD-ATOM (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is forward across TIMES Lisp-atoms from BP.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (ATOM-WORD-SYNTAX-BIND
    (FORWARD-WORD BP TIMES FIXUP-P)))

(DEFUN FORWARD-SENTENCE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp to the TIMES'th sentence-end forward from BP.
TIMES negative means move backwards past that many sentence-beginnings.
Every paragraph beginning or end is also a sentence beginning or end.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (DO (PARA-BP
              (TIME 0 (1+ TIME))
              (STATE NIL)
              (FIRST-P T NIL)
              (CH))
             ((OR ( TIME TIMES) (NULL BP)) BP)
           (SETQ BP (FORWARD-OVER (IF FIRST-P '(#\CR) *WHITESPACE-CHARS*) BP))
           (SETQ PARA-BP (FORWARD-PARAGRAPH BP 1))
           (IF PARA-BP
               (LET ((TEM (BACKWARD-OVER *WHITESPACE-CHARS* PARA-BP)))
                 (IF (BP-< BP TEM)
                     (SETQ PARA-BP TEM)
                   (SETQ PARA-BP (FORWARD-PARAGRAPH PARA-BP 1))
                   (IF PARA-BP
                       (SETQ PARA-BP (BACKWARD-OVER *WHITESPACE-CHARS* PARA-BP))
                     (SETQ PARA-BP (INTERVAL-LAST-BP *INTERVAL*)))))
             (SETQ PARA-BP (INTERVAL-LAST-BP *INTERVAL*)))
           (SETQ BP (CHARMAP (BP PARA-BP (AND (OR STATE FIXUP-P) (COPY-BP PARA-BP)))
                      (SETQ CH (CHARMAP-CH-CHAR))
                      (AND STATE                ;If special character last time...
                           (COND ((OR (= CH #\CR)       ;".<cr>" or ". <cr>" win
                                      (AND (EQ STATE 'SP) (= CH #\SP))) ;".  " wins
                                  (CHARMAP-RETURN (COND ((EQ STATE 'DOT)        ;".<cr>"
                                                         (CHARMAP-BP-BEFORE))
                                                        (T (FORWARD-CHAR (CHARMAP-BP-BEFORE)
                                                                         -1)))))
                                 ((AND (EQ STATE 'DOT) (= CH #\SP))     ;". "
                                  (SETQ STATE 'SP))
                                 ((AND (EQ STATE 'DOT) (MEMQ CH '(#/" #/' #/) #/]))))
                                 (T (SETQ STATE NIL))))
                      (COND
;                           ((= CH #\CR)        ;If at end of line, check for another
;                            (SETQ STATE 'CR)
;                            )  ;<cr> next time
                            ((MEMQ CH '(#/. #/! #/?))
                             (SETQ STATE 'DOT)))))))
        (T
         (DO ((TIME 0 (1- TIME))
              (STATE NIL)
              (NFROBS)
              (para-bp)
              (CH))
             (( TIME TIMES) (FORWARD-OVER *WHITESPACE-CHARS* (FORWARD-CHAR BP NFROBS)))
           (setq bp (backward-over '(#\cr #\sp #\tab) bp))
           (setq para-bp (forward-paragraph bp -1 t))
           (SETQ BP (BACKWARD-OVER '(#\CR #\SP #/" #/' #/) #/]) BP)
                 NFROBS 0)
           (SETQ BP (RCHARMAP (BP PARA-BP (AND FIXUP-P (COPY-BP PARA-BP)))
                      (SETQ CH (RCHARMAP-CH-CHAR))
                      (COND ((MEMQ STATE '(CR SPSP))
                             (DO NIL
                                 ((NOT (MEMQ CH '(#\SP #/" #/' #/) #/]))))
                               (RCHARMAP-DECREMENT (AND FIXUP-P (COPY-BP PARA-BP)))
                               (SETQ CH (RCHARMAP-CH-CHAR)
                                     NFROBS (1+ NFROBS)))
                             (AND (MEMQ CH '(#/. #/! #/?))
                                  (RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))
                             (SETQ STATE NIL
                                   NFROBS 0)))
                      (COND ((MEMQ STATE '(SP SPSP))
                             (SETQ STATE (AND (= CH #\SP) 'SPSP)))
                            ((= CH #\SP)
                             (SETQ STATE 'SP))
                            ((= CH #\CR)
                             (SETQ STATE 'CR)))))
           (OR BP (RETURN NIL))))))

;;; Not-so-standard motion functions.

(DEFUN DEFUN-INTERVAL (BP &OPTIONAL (TIMES 1) FIXUP-P (COMMENTS-P T) (TOP-BLANK-P NIL))
  "Return an interval surrounding the defun that BP is within, or NIL.
If TIMES is > 1, includes additional defuns after that one.
COMMENTS-P non-NIL means include comments before the defun.
TOP-BLANK-P non-NIL along with COMMENTS-P means
 include one blank line (if any) before anything else.
The second value is the line which opens the list structure of
 the defun that the interval contains."
  (DECLARE (VALUES INTERVAL DEFINITION-LINE))
  (PROG (BP1 BP2 BP3 BP4 SBP)
        (COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP -1)))
               (SETQ BP1 (BEG-LINE BP 0))
               (COND ((= (LIST-SYNTAX (BP-CHAR BP1)) LIST-OPEN)
                      (GO BUFBEG1))
                     (T (GO BUFBEG)))))
        (OR (SETQ BP2 (FORWARD-SEXP BP1))
            (IF (NOT FIXUP-P) (RETURN NIL)
                (SETQ BP2 (BEG-LINE (BACKWARD-OVER-COMMENT-LINES (FORWARD-DEFUN BP1 1 T)
                                                                 TOP-BLANK-P)
                                    -1))))
        (OR (BP-< (END-LINE BP2) BP)
            ;; We were in the middle of the defun.
            (GO FOUND))
        (SETQ BP BP1)
     BUFBEG
        (COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP)))
               (AND BP2 (SETQ BP1 (FORWARD-DEFUN BP2 -1))
                    (GO FOUND))              ;At end of buffer, take previous
               (RETURN NIL)))
     BUFBEG1
        (OR (SETQ BP2 (FORWARD-SEXP BP1)) (RETURN NIL))
     FOUND
        ;; At this point, BP1 and BP2 surround a "defun".  Now we should grab any
        ;; comment lines and intervening blank lines before the beginning, and the
        ;; rest of the last line.
        (AND (> TIMES 1)
             (SETQ BP2 (FORWARD-SEXP BP2 (1- TIMES) T)))
        (SETQ SBP BP1)                  ;Save real starting line
     CONTIN
        (AND COMMENTS-P (SETQ BP1 (BACKWARD-OVER-COMMENT-LINES BP1 TOP-BLANK-P NIL)))
        (SETQ BP3 (FORWARD-OVER *BLANKS* BP2))
        (AND BP3 (OR (= (LIST-SYNTAX (BP-CHAR BP3)) LIST-COMMENT)
                     (= (BP-CH-CHAR BP3) #\CR))
             (SETQ BP2 (BEG-LINE BP2 1 T)))
        ;; Now try to find any extra close-parens because of a LOCAL-DECLARE
        (SETQ BP3 (FORWARD-OVER '(#/)) BP2))
        (AND (NOT (BP-= BP2 BP3))
             (SETQ BP4 (FORWARD-SEXP BP3 (- TIMES)))
             (BP-< BP4 BP1)
             (SETQ BP1 BP4 BP2 BP3)
             (GO CONTIN))
        ;; Now try to find a package prefix
        (SETQ BP3 (BACKWARD-OVER-PACKAGE-PREFIX BP1))
        (WHEN BP3
          (SETQ BP1 BP3)
          (GO CONTIN))
        (RETURN (VALUES (CREATE-INTERVAL BP1 BP2) SBP))))

(DEFUN BACKWARD-OVER-PACKAGE-PREFIX (BP)
  "Return a bp to the start of the package prefix before BP, or NIL if none."
  (LET (BP3 BP4)
    (SETQ BP3 (BACKWARD-OVER *WHITESPACE-CHARS* BP)
          BP4 (FORWARD-WORD BP3 -1 T))
    (AND (CHAR-EQUAL (BP-CHAR-BEFORE BP3) #/:) (BEG-LINE-P BP4)
         BP4)))

(DEFUN BACKWARD-OVER-COMMENT-LINES (BP &OPTIONAL (TOP-BLANK-P T) (UP-P T)
                                       &AUX (LAST-GOOD-LINE (BP-LINE BP)))
  "Return a bp back from BP across all comment lines.
Blank lines are also skipped if more comment lines are reached thus.
TOP-BLANK-P non-NIL means skip one blank line past the last comment line.
All blank lines at the beginning of the buffer are skipped.
UP-P means skip any list-structure that precedes BP but is not
matched until after BP."
  (DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE)))
      ((NULL LINE)
       (SETQ LAST-GOOD-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
    (SELECTQ (LINE-TYPE LINE)
        (:BLANK)
        (:COMMENT (SETQ LAST-GOOD-LINE LINE))
        (:NORMAL (IF (AND UP-P (LINE-OPENS-PARENS LINE))
                     (SETQ LAST-GOOD-LINE LINE)
                     (RETURN)))
        (OTHERWISE (RETURN))))
  (COND ((EQ LAST-GOOD-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
        ((NOT TOP-BLANK-P))
        ((MEMQ (LINE-TYPE (LINE-PREVIOUS LAST-GOOD-LINE)) ':(BLANK FORM))
         (SETQ LAST-GOOD-LINE (LINE-PREVIOUS LAST-GOOD-LINE))))
  (IF (EQ (LINE-TYPE LAST-GOOD-LINE) ':FORM) (END-OF-LINE LAST-GOOD-LINE)
      (BEG-OF-LINE LAST-GOOD-LINE)))

(DEFUN SKIP-OVER-BLANK-LINES-AND-COMMENTS (BP &OPTIONAL FIXUP-P)
  "Return a bp to the front of the first non-blank non-comment line after BP.
If there is non-blank non-comment data following BP on the same line
we return a pointer to that.
This is good for finding the next interesting piece of lisp code after a point.
FIXUP-P non-NIL means return end of buffer if nothing is found;
 otherwise return NIL."
  (SETQ BP (FORWARD-OVER *BLANKS* BP))
  (AND BP (OR (= (BP-CH-CHAR BP) #/;)
              (= (BP-CH-CHAR BP) #\CR))
       (DO () (NIL)
         (SETQ BP (BEG-LINE BP 1))
         (OR BP (RETURN NIL))
         (SELECTQ (LINE-TYPE (BP-LINE BP))
           ((:BLANK :COMMENT :FORM))
           (OTHERWISE (RETURN BP)))))
  (OR BP (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN FORWARD-OVER-BLANK-OR-PAGE-LINES (BP &OPTIONAL FIXUP-P)
  "Return a bp to the front of the first non-blank non-page-separator line after BP.
FIXUP-P non-NIL means return end of buffer if nothing is found;
 otherwise return NIL."
  (DO () (NIL)
    (SETQ BP (BEG-LINE BP 1))
    (OR BP (RETURN NIL))
    (SELECTQ (LINE-TYPE (BP-LINE BP))
      ((:BLANK :FORM))
      (OTHERWISE (RETURN BP))))
  (OR BP (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN BEG-LINE (BP &OPTIONAL (TIMES 0) FIXUP-P)
  "Return a bp to the start of the line TIMES lines down from BP.
TIMES = 0 means the same line BP is on.  TIMES negative moves up.
FIXUP-P means return beginning or end of buffer if try to move past;
 otherwise return NIL in that case."
  (COND (( TIMES 0)
         (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
              (I TIMES (1- I))
              (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
             (NIL)
           (COND ((EQ LINE LAST-LINE)
                  (RETURN (IF (OR ( I 0) FIXUP-P)
                              (CREATE-BP LINE (IF ( I 0) 0
                                                  (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))))
                              NIL)))
                 (( I 0)
                  (RETURN (CREATE-BP LINE 0))))))
        (T
         (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
              (I TIMES (1+ I))
              (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
             (NIL)
           (COND ((EQ LINE FIRST-LINE)
                  (RETURN (IF (OR ( I 0) FIXUP-P)
                              (CREATE-BP LINE (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
                              NIL)))
                 (( I 0)
                  (RETURN (CREATE-BP LINE 0))))))))

(DEFUN END-LINE (BP &OPTIONAL (TIMES 0) FIXUP-P)
  "Return a bp to the end of the line TIMES lines down from BP.
TIMES = 0 means the same line BP is on.  TIMES negative moves up.
FIXUP-P means return beginning or end of buffer if try to move past;
 otherwise return NIL in that case."
  (COND (( TIMES 0)
         (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
              (I TIMES (1- I))
              (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
             (NIL)
           (COND ((EQ LINE LAST-LINE)
                  (RETURN (IF (OR ( I 0) FIXUP-P)
                              (CREATE-BP LINE (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))
                              NIL)))
                 (( I 0)
                  (RETURN (CREATE-BP LINE (LINE-LENGTH LINE)))))))
        (T
         (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
              (I TIMES (1+ I))
              (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
             (NIL)
           (COND ((EQ LINE FIRST-LINE)
                  (RETURN (IF (OR ( I 0) FIXUP-P)
                              (CREATE-BP LINE (IF ( I 0) (LINE-LENGTH LINE)
                                                  (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))))
                              NIL)))
                 (( I 0)
                  (RETURN (CREATE-BP LINE (LINE-LENGTH LINE)))))))))

(DEFUN FORWARD-OVER (LIST BP &OPTIONAL (LIMIT-BP (INTERVAL-LAST-BP *INTERVAL*)))
  "Return a bp to the first char following BP that is not in LIST.
LIST is a list of characters."
  (CHARMAP (BP LIMIT-BP (CHARMAP-BP-BEFORE))
    (IF (NOT (OR (MEMQ (CHARMAP-CH-CHARACTER) LIST)
;character lossage LIST may contain fixnums
                 (MEMQ (CHAR-INT (RCHARMAP-CH-CHARACTER)) LIST)))
        (CHARMAP-RETURN (CHARMAP-BP-BEFORE)))))

(DEFUN BACKWARD-OVER (LIST BP &OPTIONAL (LIMIT-BP (INTERVAL-FIRST-BP *INTERVAL*)))
  "Return a bp after the first char found backward from BP that is not in LIST.
LIST is a list of characters."
  (RCHARMAP (BP LIMIT-BP (RCHARMAP-BP-AFTER))
    (IF (NOT (OR (MEMQ (RCHARMAP-CH-CHARACTER) LIST)
;character lossage LIST may contain fixnums
                 (MEMQ (CHAR-INT (RCHARMAP-CH-CHARACTER)) LIST)))
        (RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))))

(DEFUN DELETE-OVER (LIST BP)
  "Delete all characters in LIST that follow BP."
  (DELETE-INTERVAL BP (FORWARD-OVER LIST BP) T))

(DEFUN DELETE-BACKWARD-OVER (LIST BP)
  "Delete all characters in LIST that precede BP."
  (DELETE-INTERVAL (BACKWARD-OVER LIST BP) BP T))

(DEFUN DELETE-AROUND (LIST BP)
  "Delete all characters in LIST on either side of BP."
  (DELETE-INTERVAL (BACKWARD-OVER LIST BP) (FORWARD-OVER LIST BP) T))

;;; Make this a variable just in case someone wants to modify it
;;; Strange how some things use the MOVER in this list
;;; while some of them use FORWARD-OVER-MATCHING-DELIMITERS.
(DEFCONST *MATCHING-DELIMITER-LIST*
          '((#/( #/) FORWARD-SEXP) (#/" #/" FORWARD-WORD) (#/[ #/] FORWARD-SEXP)
            (#/{ #/} FORWARD-SEXP) (#/< #/> FORWARD-WORD) (#/* #/* FORWARD-SEXP)
            (#/ #/ FORWARD-WORD) (#/| #/| FORWARD-SEXP) (#/$ #/$ FORWARD-SEXP)
            (#/ #/ FORWARD-SEXP) (#/ #/ FORWARD-SEXP))
  "List of pairs of delimiters that match each other.
Each element looks like (OPEN-CHAR CLOSE-CHAR MOVER).
MOVER is a standard motion function.")

(DEFUN FORWARD-OVER-MATCHING-DELIMITERS (BP &OPTIONAL (TIMES 1) FIXUP-P (LEVEL 0) OPEN CLOSE)
  "Return a bp which is TIMES balanced objects forward from BP.
TIMES may be negative meaning go backward.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
LEVEL nonzero means move up that many levels of list structure.
OPEN and CLOSE can specify the pair of delimiters to use,
 but usually they are deduced form the text being moved over."
  (COND ((ZEROP TIMES) (COPY-BP BP))
        ((PLUSP TIMES)
         (LET ((TIME 0)
               (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
           (CHARMAP (BP LAST-BP (IF FIXUP-P (COPY-BP LAST-BP) NIL))
             (LET ((CHAR (CHARMAP-CH-CHAR)))
               (COND ((NULL OPEN)
                      (DO ((L *MATCHING-DELIMITER-LIST* (CDR L)))
                          ((NULL L))
                        (COND ((= CHAR (CAAR L))
                               (SETQ OPEN CHAR
                                     CLOSE (CADAR L)
                                     LEVEL 1)
                               (RETURN)))))
                     ((= CHAR CLOSE)
                      (AND ( (SETQ LEVEL (1- LEVEL)) 0)
                           ( (SETQ TIME (1+ TIME)) TIMES)
                           (CHARMAP-RETURN (CHARMAP-BP-AFTER))))
                     ((= CHAR OPEN)
                      (SETQ LEVEL (1+ LEVEL))))))))
        (T
         (LET ((TIME 0)
               (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
           (RCHARMAP (BP FIRST-BP (IF FIXUP-P (COPY-BP FIRST-BP) NIL))
             (LET ((CHAR (RCHARMAP-CH-CHAR)))
               (COND ((NULL OPEN)
                      (DO L *MATCHING-DELIMITER-LIST* (CDR L) (NULL L)
                        (COND ((= CHAR (CADAR L))
                               (SETQ CLOSE CHAR
                                     CLOSE (CAAR L)
                                     LEVEL -1)
                               (RETURN)))))
                     ((= CHAR OPEN)
                      (AND ( (SETQ LEVEL (1+ LEVEL)) 0)
                           ( (SETQ TIME (1- TIMES)) TIMES)
                           (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE))))
                     ((= CHAR CLOSE)
                      (SETQ LEVEL (1- LEVEL))))))))))



(DEFUN FORWARD-TO-NEXT-LINE-WITH-NON-WHITE-SPACE-START (BP)
  (DO ((LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE)))
      ((NULL LINE) NIL)
    (WHEN (AND (> (LINE-LENGTH LINE) 0)
               (NOT (MEMQ (AREF LINE 0) '(#\SPACE #\CR #\TAB))))
      (RETURN (CREATE-BP LINE 0)))))

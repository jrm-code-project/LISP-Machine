;;;-*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Functions and commands for doing searching

(DEFUN ZWEI-SEARCH (BP STRING &OPTIONAL REVERSEP FIXUP-P LINES-TO-SEARCH LIMIT-BP &AUX NLPOS)
  "Search for STRING starting at BP.  Return the bp to where it is found, or NIL.
The value points after the string found if searching forward.
REVERSEP non-NIL means search back before BP; then the value points before the occurrence.
FIXUP-P non-NIL means return beginning or end of buffer if search fails.
If LIMIT-BP is reached, that is considered failure.
LINES-TO-SEARCH non-NIL means give up after searching that many lines;
 then the first value is where to resume searching,
 and the second value is T."
  (OR LIMIT-BP (SETQ LIMIT-BP (IF REVERSEP
                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                  (INTERVAL-LAST-BP *INTERVAL*))))
;character lossage
  (IF (NUMBERP STRING) (SETQ STRING (INT-CHAR STRING)))
  (COND ((CHARACTERP STRING)
         (IF (NOT REVERSEP)
             ;; Forward search for a character.
             (CHARMAP-PER-LINE (BP LIMIT-BP (IF FIXUP-P (COPY-BP LIMIT-BP) NIL))
                               ((AND LINES-TO-SEARCH
                                     (ZEROP (SETQ LINES-TO-SEARCH
                                                  (1- LINES-TO-SEARCH)))
                                     (RETURN-FROM ZWEI-SEARCH
                                       (VALUES (CREATE-BP LINE *FIRST-INDEX*)
                                               T))))
               (AND (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                        (CHAR= STRING (CHARMAP-CHARACTER))
                        (CHAR-EQUAL STRING (CHARMAP-CHARACTER)))
                    (CHARMAP-RETURN (CHARMAP-BP-AFTER))))
           ;; Reverse search for a character.
           (RCHARMAP-PER-LINE (BP LIMIT-BP (IF FIXUP-P (COPY-BP LIMIT-BP) NIL))
                              ((AND LINES-TO-SEARCH
                                    (ZEROP (SETQ LINES-TO-SEARCH
                                                 (1- LINES-TO-SEARCH)))
                                    (RETURN-FROM ZWEI-SEARCH (VALUES (END-OF-LINE LINE)
                                                                     T))))
             (AND (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                      (CHAR= STRING (RCHARMAP-CHARACTER))
                      (CHAR-EQUAL STRING (RCHARMAP-CHARACTER)))
                  (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE))))))
        ((SETQ NLPOS (STRING-SEARCH-CHAR #/NEWLINE STRING))
         (SEARCH-CR-FULL BP STRING REVERSEP FIXUP-P NLPOS LINES-TO-SEARCH LIMIT-BP))
        (T
         (IF (NOT REVERSEP)
             ;; Search forward for a 1-line string.
             (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
                  (LEN (STRING-LENGTH STRING))
                  (FROM-INDEX (BP-INDEX BP) 0)
                  (LAST-LINE (BP-LINE LIMIT-BP)))
                 ((AND LINES-TO-SEARCH
                       (ZEROP (SETQ LINES-TO-SEARCH (1- LINES-TO-SEARCH))))
                  (RETURN-FROM ZWEI-SEARCH (VALUES (CREATE-BP LINE FROM-INDEX) T)))
               (LET ((LASTP (EQ LINE LAST-LINE)))
                 (LET ((INDEX (STRING-SEARCH STRING LINE FROM-INDEX NIL 0 NIL
                                             *ALPHABETIC-CASE-AFFECTS-SEARCH*)))
                   (COND ((AND (NOT (NULL INDEX))
                               (OR (NOT LASTP)
                                   ( INDEX (- (BP-INDEX LIMIT-BP) LEN))))
                          (RETURN (CREATE-BP LINE (+ INDEX LEN))))
                         (LASTP
                          (RETURN (IF FIXUP-P (COPY-BP LIMIT-BP) NIL)))))))
           ;; Search backward for a 1-line string.
           (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
                (LEN (STRING-LENGTH STRING))
                (FROM-INDEX (BP-INDEX BP) NIL)
                (FIRST-LINE (BP-LINE LIMIT-BP)))
               ((AND LINES-TO-SEARCH
                     (ZEROP (SETQ LINES-TO-SEARCH (1- LINES-TO-SEARCH))))
                (RETURN-FROM ZWEI-SEARCH (VALUES (END-OF-LINE LINE) T)))
             (LET ((FIRSTP (EQ LINE FIRST-LINE)))
               (LET ((INDEX (STRING-REVERSE-SEARCH STRING LINE FROM-INDEX 0 0 NIL
                                                   *ALPHABETIC-CASE-AFFECTS-SEARCH*)))
                 (COND ((AND (NOT (NULL INDEX))
                             (OR (NOT FIRSTP)
                                 ( INDEX (- (BP-INDEX LIMIT-BP) LEN))))
                        (RETURN (CREATE-BP LINE INDEX)))
                       (FIRSTP
                        (RETURN (IF FIXUP-P (COPY-BP LIMIT-BP) NIL)))))))))))
(DEFF SEARCH 'ZWEI-SEARCH)
(COMPILER:MAKE-OBSOLETE SEARCH "use ZWEI:ZWEI-SEARCH")

;;; Subroutine of SEARCH.  Used to search for a string containing a CR.
;;; NLPOS is the index in STRING of the first CR.
(DEFUN SEARCH-CR-FULL (BP STRING REVERSEP FIXUP-P NLPOS LINES-TO-SEARCH LIMIT-BP)
  (LET ((STRING-LENGTH (STRING-LENGTH STRING)))
    (IF (NOT REVERSEP)
        (LET ((CHAR1 (CHAR STRING 0))
              (CRLEADS (ZEROP NLPOS)))
          (BLOCK LUPO
            (DO ((LINE (IF (AND (NOT CRLEADS)
                                ( (- (LINE-LENGTH (BP-LINE BP)) (BP-INDEX BP)) NLPOS))
                           (BP-LINE BP)
                         (LINE-NEXT (BP-LINE BP)))
                       (LINE-NEXT LINE))
                 (LAST-LINE (BP-LINE LIMIT-BP)))
                ((OR (NULL LINE) (AND (EQ LINE LAST-LINE)
                                      (NOT CRLEADS)))
                 ;; Since string contains a NEWLINE, it cannot start on LAST-LINE.
                 ;; Unless the NEWLINE is at the beginning.
                 (IF FIXUP-P (COPY-BP LIMIT-BP) NIL))
              (AND LINES-TO-SEARCH
                   (ZEROP (SETQ LINES-TO-SEARCH (1- LINES-TO-SEARCH)))
                   (RETURN-FROM SEARCH-CR-FULL (VALUES (BEG-OF-LINE LINE) T)))
              (LET ((START-INDEX (IF CRLEADS 0 (- (LINE-LENGTH LINE) NLPOS))))
                (COND ((OR CRLEADS
                           (AND ( START-INDEX 0)
                                (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                    (CHAR= CHAR1 (CHAR LINE START-INDEX))
                                    (CHAR-EQUAL CHAR1 (CHAR LINE START-INDEX)))))
                       (LET ((I (IF CRLEADS 1 0)))
                         (CHARMAP ((CREATE-BP LINE START-INDEX)
                                   LIMIT-BP
                                   (IF ( I STRING-LENGTH)
                                       (RETURN-FROM LUPO (CHARMAP-BP-BEFORE))))
                           (IF ( I STRING-LENGTH)
                               (RETURN-FROM LUPO (CHARMAP-BP-BEFORE)))
                           (UNLESS
                             (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                 (CHAR= (CHARMAP-CHARACTER) (CHAR STRING I))
                                 (CHAR-EQUAL (CHARMAP-CHARACTER) (CHAR STRING I)))
                             (CHARMAP-RETURN NIL))
                           (INCF I))))))
              (AND CRLEADS (EQ LINE LAST-LINE)
                   (RETURN (IF FIXUP-P (COPY-BP LIMIT-BP) NIL))))))
      (SETQ NLPOS (STRING-REVERSE-SEARCH-CHAR #/NEWLINE STRING))
      (LET ((CHARL (CHAR STRING (1- STRING-LENGTH)))
            ;; One less than number of chars after the last CR.
            (START-INDEX (- STRING-LENGTH NLPOS 2)))
        (BLOCK LUPO
          (DO ((LINE (IF (> (BP-INDEX BP) START-INDEX)
                         (BP-LINE BP)
                       (LINE-PREVIOUS (BP-LINE BP)))
                     (LINE-PREVIOUS LINE))
               (CRTRAILS (CHAR-EQUAL CHARL #/NEWLINE))
               (FIRST-LINE (BP-LINE LIMIT-BP)))
              ((OR (NULL LINE) (EQ LINE FIRST-LINE))
               (IF FIXUP-P (COPY-BP LIMIT-BP) NIL))
            (AND LINES-TO-SEARCH
                 (ZEROP (SETQ LINES-TO-SEARCH (1- LINES-TO-SEARCH)))
                 (RETURN-FROM SEARCH-CR-FULL (VALUES (BEG-LINE (CREATE-BP LINE 0) 1) T)))
            (COND ((OR CRTRAILS (AND (> (LINE-LENGTH LINE) START-INDEX)
                                     (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                         (CHAR= CHARL (CHAR LINE START-INDEX))
                                         (CHAR-EQUAL CHARL (CHAR LINE START-INDEX)))))
                   (LET ((I (1- STRING-LENGTH)))
                     (RCHARMAP ((CREATE-BP LINE (1+ START-INDEX))
                                LIMIT-BP
                                (IF (< I 0)
                                    (RETURN-FROM LUPO (RCHARMAP-BP-AFTER))))
                       (IF (< I 0)
                           (RETURN-FROM LUPO (RCHARMAP-BP-AFTER)))
                       (IF (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                               (CHAR= (RCHARMAP-CHARACTER) (CHAR STRING I))
                               (CHAR-EQUAL (RCHARMAP-CHARACTER) (CHAR STRING I)))
                           (DECF I)
                         (RCHARMAP-RETURN NIL))))))))))))

;;; Word search infernal function
(DEFUN WORD-SEARCH (BP KEY &OPTIONAL REVERSE-P FIXUP-P LIMIT-BP &AUX LEN KEY1 LEN1
                    (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON *ALPHABETIC-CASE-AFFECTS-SEARCH*))
  "Like SEARCH but does a word-wise search.
That is, each word in KEY need only be an abbreviation of a word in the buffer
for a match to occur.  KEY is a string.
The remaining args are as for SEARCH."
  (OR LIMIT-BP (SETQ LIMIT-BP (IF REVERSE-P
                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                  (INTERVAL-LAST-BP *INTERVAL*))))
  ;; Discard any delimiter chars at the front of KEY.
  ;; They would make the code below blow up.
  (SETQ LEN (STRING-LENGTH KEY))
  (DO ((I 0 (1+ I)))
      ((OR ( I LEN)
           (= (WORD-SYNTAX (CHAR KEY I)) WORD-ALPHABETIC))
       (UNLESS (ZEROP I)
         (SETQ KEY (NSUBSTRING KEY I)))))
  (SETQ LEN (STRING-LENGTH KEY))
  ;; KEY1 gets the first word of KEY, and LEN1 gets KEY1's length:
  (DO ((I 0 (1+ I)))
      ((OR ( I LEN)
           ( (WORD-SYNTAX (CHAR KEY I)) WORD-ALPHABETIC))
       (SETQ LEN1 I KEY1 (NSUBSTRING KEY 0 I))))
  (COND ((ZEROP LEN)
         (WHEN FIXUP-P BP))
        ((NOT REVERSE-P)
         (BLOCK LINES
           (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
                (LIMIT (BP-LINE LIMIT-BP))
                (IDX (BP-INDEX BP) 0))
               (NIL)
             (BLOCK PER-LINE
               (DO ((LLEN (LINE-LENGTH LINE)))
                   (NIL)
                 ;; Find first word in line
                 (OR (SETQ IDX (STRING-SEARCH KEY1 LINE IDX NIL 0 NIL
                                              *ALPHABETIC-CASE-AFFECTS-SEARCH*))
                     (RETURN NIL))
                 (BLOCK MATCH-REMAINING-WORDS
                   (DO ((I IDX)                 ;I index of character in line
                        (J LEN1)                ;J index in search key
                        (LINE1 LINE)            ;Copy these in case we advance to next line
                        (LLEN1 LLEN))
                       (NIL)
                     ;; Space forward in line to end of this word
                     (DO-FOREVER
                       (AND (OR ( I LLEN1)
                                ( (WORD-SYNTAX (CHAR LINE1 I)) WORD-ALPHABETIC))
                            (RETURN NIL))
                       (INCF I))
                     ;; Space forward in key to start of next word
                     ;; If key exhausted, the search succeeds
                     (DO-FOREVER
                       (COND (( J LEN)
                              (RETURN-FROM LINES (CREATE-BP LINE1 I)))  ;Point after last word
                             ((= (WORD-SYNTAX (CHAR KEY J)) WORD-ALPHABETIC)
                              (RETURN NIL)))
                       (INCF J))
                     ;; Space forward in line to start of next word.  This may actually
                     ;; be on the next line.
                     (DO-FOREVER
                       (COND (( I LLEN1)       ;This line used up, advance to next
                              (AND (EQ LINE1 LIMIT) (RETURN-FROM MATCH-REMAINING-WORDS))
                              (SETQ LINE1 (LINE-NEXT LINE1)
                                    LLEN1 (LINE-LENGTH LINE1)
                                    I 0))
                             ((= (WORD-SYNTAX (CHAR LINE1 I)) WORD-ALPHABETIC)
                              (RETURN NIL))
                             (T
                              (INCF I))))
                     ;; Check that these two words match
                     (DO ((CH1 (CHAR LINE1 I))
                          (CH2 (CHAR KEY J)))
                         (NIL)
                       (COND (( (WORD-SYNTAX CH2) WORD-ALPHABETIC)
                              ;; key can be shorter than data allowing word abbreviation
                              (RETURN NIL))
                             ((NOT (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                       (CHAR= CH1 CH2)
                                       (CHAR-EQUAL CH1 CH2)))
                              (RETURN-FROM MATCH-REMAINING-WORDS)))
                       (SETQ CH1 (IF ( (INCF I) LLEN1) #/NEWLINE (CHAR LINE1 I)))
                       (SETQ CH2 (IF ( (INCF J) LEN) #/NEWLINE (CHAR KEY J))))))
                 (INCF IDX)))                   ;search for next instance of first word
             (AND (EQ LINE LIMIT)
                  (RETURN (AND FIXUP-P (COPY-BP LIMIT-BP)))))))
        (T
         (BLOCK LINES
           (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
                (LIMIT (BP-LINE LIMIT-BP))
                (FORWARD-LIMIT (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                (IDX (BP-INDEX BP) NIL))
               (NIL)
             (BLOCK PER-LINE
               (DO ((LLEN (LINE-LENGTH LINE)))
                   (NIL)
                 (OR (SETQ IDX (STRING-REVERSE-SEARCH KEY1 LINE IDX 0 0 NIL
                                                      *ALPHABETIC-CASE-AFFECTS-SEARCH*))
                     (RETURN NIL))
                 (BLOCK MATCH-REMAINING-WORDS
                   (DO ((I IDX)                 ;I index of character in line
                        (J LEN1)                ;J index in search key
                        (LINE1 LINE)            ;Copy these in case we advance to next line
                        (LLEN1 LLEN))
                       (NIL)
                     ;; Space forward in line to end of this word
                     (DO-FOREVER
                       (AND (OR ( I LLEN1)
                                ( (WORD-SYNTAX (CHAR LINE1 I)) WORD-ALPHABETIC))
                            (RETURN NIL))
                       (INCF I))
                     ;; Space forward in key to start of next word
                     ;; If key exhausted, the search succeeds
                     (DO-FOREVER
                       (COND (( J LEN)
                              (RETURN-FROM LINES (CREATE-BP LINE IDX))) ;Point before first word
                             ((= (WORD-SYNTAX (CHAR KEY J)) WORD-ALPHABETIC)
                              (RETURN NIL)))
                       (INCF J))
                     ;; Space forward in line to start of next word.  This may actually
                     ;; be on the next line.
                     (DO-FOREVER
                       (COND (( I LLEN1)       ;This line used up, advance to next
                              (AND (EQ LINE1 FORWARD-LIMIT) (RETURN-FROM MATCH-REMAINING-WORDS))
                              (SETQ LINE1 (LINE-NEXT LINE1)
                                    LLEN1 (LINE-LENGTH LINE1)
                                    I 0))
                             ((= (WORD-SYNTAX (CHAR LINE1 I)) WORD-ALPHABETIC)
                              (RETURN NIL))
                             (T
                              (SETQ I (1+ I)))))
                     ;; Check that these two words match
                     (DO ((CH1 (CHAR LINE1 I))
                          (CH2 (CHAR KEY J)))
                         (NIL)
                       (COND (( (WORD-SYNTAX CH2) WORD-ALPHABETIC)     ;key can be shorter than data
                              (RETURN NIL))     ;allowing word abbreviation
                             ((NOT (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                       (CHAR= CH1 CH2)
                                       (CHAR-EQUAL CH1 CH2)))
                              (RETURN-FROM MATCH-REMAINING-WORDS)))
                       (SETQ CH1 (IF ( (INCF I) LLEN1) #/NEWLINE (CHAR LINE1 I)))
                       (SETQ CH2 (IF ( (INCF J) LEN) #/NEWLINE (CHAR KEY J))))))
                 (INCF IDX (1- (STRING-LENGTH KEY1)))))
             (AND (EQ LINE LIMIT)
                  (RETURN (AND FIXUP-P (COPY-BP LIMIT-BP)))))))))

(DEFUN SEARCH-SET (BP LIST &OPTIONAL REVERSEP FIXUP-P LIMIT-BP &AUX CH)
  "Like SEARCH but searches for a character which is in LIST, a list of characters."
  (OR LIMIT-BP (SETQ LIMIT-BP (IF REVERSEP
                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                  (INTERVAL-LAST-BP *INTERVAL*))))
  (IF (NOT REVERSEP)
      (CHARMAP (BP LIMIT-BP (AND FIXUP-P LIMIT-BP))
        (AND (OR (MEMQ (SETQ CH (CHARMAP-CH-CHARACTER)) LIST)
;character lossage LIST may contain fixnums
                 (MEMQ (CHAR-INT CH) LIST))
             (CHARMAP-RETURN (VALUES (CHARMAP-BP-AFTER) CH))))
      (RCHARMAP (BP LIMIT-BP (AND FIXUP-P LIMIT-BP))
        (AND (OR (MEMQ (SETQ CH (RCHARMAP-CH-CHARACTER)) LIST)
;character lossage
                 (MEMQ (CHAR-INT CH) LIST))
             (RCHARMAP-RETURN (VALUES (RCHARMAP-BP-BEFORE) CH))))))

(DEFVAR *LAST-DELIMITED-SEARCH-STRING* NIL)
(DEFVAR *LAST-DELIMITED-SEARCH-STRING-WITH-DELIMITERS*)

(DEFUN DELIMITED-SEARCH (BP STRING &OPTIONAL REVERSE-P FIXUP-P LIMIT-BP)
  "Like SEARCH but accepts only occurrences of STRING with delimiters on both sides."
  (SETQ STRING (IF (EQ STRING *LAST-DELIMITED-SEARCH-STRING*)
                   *LAST-DELIMITED-SEARCH-STRING-WITH-DELIMITERS*
                 (SETQ *LAST-DELIMITED-SEARCH-STRING* STRING)
                 (LET* ((LENGTH (STRING-LENGTH STRING))
                        (NEW-STRING (MAKE-ARRAY (+ LENGTH 2) ':TYPE 'ART-16B)))
                   (ASET #o20005 NEW-STRING 0)
                   (COPY-ARRAY-PORTION STRING 0 LENGTH NEW-STRING 1 (1+ LENGTH))
                   (ASET #o20005 NEW-STRING (1+ LENGTH))
                   (SETQ *LAST-DELIMITED-SEARCH-STRING-WITH-DELIMITERS*
                         (NCONS NEW-STRING)))))
  (SETQ BP (FSM-SEARCH BP STRING REVERSE-P FIXUP-P NIL LIMIT-BP))
  (AND BP (IF REVERSE-P (IBP BP) (DBP BP))))

(DEFUN SEARCH-RING-PUSH (STRING FCN)
  "Push an entry onto the ring of saved search strings.
STRING and FCN go into the entry."
  (AND (NUMBERP (CAAR *SEARCH-RING*)) (SETQ *SEARCH-RING* (CDR *SEARCH-RING*)))
  (PUSH (LIST STRING FCN) *SEARCH-RING*)
  (AND (> (LENGTH *SEARCH-RING*) *SEARCH-RING-MAX*)
       (DELETE-LAST-ELEMENT *SEARCH-RING*)))

(DEFUN SEARCH-RING-POP (&AUX KEY FCN)
  "Pop an entry off the ring of saved search strings, returning two values.
The values are the two arguments given to SEARCH-RING-PUSH to push the entry.
The second is a function (maybe SEARCH), and the first is an argument to give it
/(maybe a string)."
  (COND ((NULL *SEARCH-RING*)
         (BARF))
        (T
         (SETQ KEY (CAAR *SEARCH-RING*)
               FCN (CADAR *SEARCH-RING*))
         (SETQ *SEARCH-RING*
               (NCONC (CDR *SEARCH-RING*)
                      (RPLACD *SEARCH-RING* NIL)))
         (VALUES KEY FCN))))

(DEFUN REPLACE-STRING (BP FROM TO &OPTIONAL TIMES)
  "Replace the first TIMES (or all) occurrences of FROM with TO, after BP.
FROM and TO are strings; TIMES is a number or NIL.
Returns the number of occurrences replaced."
  (OR TIMES (SETQ TIMES -1))
  (DO ((I 0 (1+ I))
       (LEN (STRING-LENGTH FROM)))
      ((= I TIMES) I)
    (OR (SETQ BP (ZWEI-SEARCH BP FROM)) (RETURN I))
    (SETQ BP (CASE-REPLACE (FORWARD-CHAR BP (- LEN)) BP TO T))))

(DEFUN CHAR-UPPERCASE-P (CHAR)
  "T if CHAR is an upper case letter."
  ( #/A (CHAR-CODE CHAR) #/Z))
(compiler:make-obsolete char-uppercase-p "use UPPER-CASE-P")

(DEFUN CHAR-LOWERCASE-P (CHAR)
  "T if CHAR is a lower case letter."
  ( #/a (CHAR-CODE CHAR) #/z))
(compiler:make-obsolete char-lowercase-p "use LOWER-CASE-P")

(DEFUN CASE-REPLACE (BP1 BP2 TO &OPTIONAL IN-ORDER-P &AUX BP FIRST SECOND)
  "Replace the text from BP1 to BP2 with a copy of TO, preserving case if appropriate.
Lowercase characters in TO are converted to uppercase as needed
to match the case pattern of the text being replaced.
Uppercase characters in TO always remain uppercase.
Returns a BP to the end of the inserted text.
IN-ORDER-P non-NIL says assume BP2 is after BP1; otherwise swap them if nec.
If *CASE-REPLACE-P* is NIL, we never preserve case."
  (OR IN-ORDER-P (ORDER-BPS BP1 BP2))
  (WITH-BP (BP3 BP1 ':NORMAL)
    (COND ((NOT *CASE-REPLACE*)
           (DELETE-INTERVAL BP1 BP2 T)
           (INSERT BP3 TO))
          (T
           ;; Get the first alphabetic char, and following char.
           (SETQ BP (COPY-BP BP1))
           (DO ()
               ((OR (BP-= BP BP2)
                    (ALPHA-CHAR-P (BP-CHARACTER BP))))
             (IBP BP))
           (SETQ FIRST (IF (BP-= BP BP2) #/0 (BP-CHARACTER BP)))
           (OR (BP-= BP BP2)
               (SETQ BP (FORWARD-CHAR BP 1 T)))
           (SETQ SECOND (IF (BP-= BP BP2) #/0 (BP-CHARACTER BP)))
           ;; Now do the replacement, leaving BP3 and BP2 denoting the region.
           (DELETE-INTERVAL BP1 BP2)
           (SETQ BP2 (INSERT BP3 (IN-CURRENT-FONT TO (CHAR-FONT FIRST))))
           (COND ((UPPER-CASE-P FIRST)
                  (COND ((OR (UPPER-CASE-P SECOND)
                             (AND (NOT (LOWER-CASE-P SECOND))
                                  (GET *MAJOR-MODE* 'ALL-UPPERCASE)))
                         ;; They are both upper case or the second one is not alphabetic
                         ;; and this mode has all uppercase.  Uppercasify the whole thing.
                         (UPCASE-INTERVAL BP3 BP2 T))
                        (T
                         ;; Only the first letter is uppercase.  Capitalize on this fact.
                         (DO ()
                             ((OR (BP-= BP3 BP2)
                                  (ALPHA-CHAR-P (BP-CHARACTER BP))))
                           (IBP BP3))
                         (OR (BP-= BP3 BP2)
                             (UPCASE-CHAR BP3))))))
           BP2))))

;;;; FSM character search
;character lossage right, left and centre. I don't even want to think about this.

;;; Format of characters in target strings:
(DEFVAR %%FSM-NOT #o1701)                       ;match any but this char
(DEFVAR %%FSM-STAR #o1601)                      ;match zero or more of this char
(DEFVAR %%FSM-SET #o1501)                       ;match member of set, rather than char
(DEFVAR %%FSM-STAR1 #o1401)                     ;match one or more of this char
(DEFVAR %FSM-NOOP (DPB 1 #o1301 0))             ;ignore this when building fsm
(DEFVAR %%FSM-CHAR %%CH-CHAR)                   ;actual character or set index

;;; Set lists, a list in place of a character means all characters in that range
;;; inclusive.  A symbol in place of the whole list means a predicate applied to
;;; all characters.
(DEFVAR *FSM-SEARCH-SET-LIST*
  '((#/SP #/TAB #/BS)                           ;0 - linear whitespace
    (#/SP #/TAB #/BS #/CR)                      ;1 - all whitespace
    ((#/A #/Z) (#/a #/z))                       ;2 - alphabetic
    ((#/0 #/9))                                 ;3 - digits
    ((#/A #/Z))                                 ;4 - uppercase
    FSM-WORD-DELIMITER-CHAR-P                   ;5 - word delimiter
    FSM-ATOM-DELIMITER-CHAR-P                   ;6 - atom delimiter
    FSM-CHAR-TRUE                               ;7 - any charactera
    ))

(DEFVAR *FSM-STRING-LIST* NIL)                  ;strings last searched for
(DEFVAR *FSM-CHARACTER-TABLE*                   ;character  character_type
        (MAKE-ARRAY #o400 ':TYPE 'ART-16B))
(DEFVAR *FSM-STATE-TABLE*                       ;state,character_type  new_state
        (MAKE-ARRAY '(1000. 200) ':TYPE 'ART-16B))
(DEFVAR *FSM-WORD-TABLE*                        ;state  word_found
        (MAKE-ARRAY 1000.))
(DEFVAR *FSM-SEARCH-SET-TABLE*)                 ;search_set  list of character_type's
(DEFVAR *FSM-BACKPOINTER-TABLE*)                ;state  back_state
(DEFVAR *FSM-CHARACTER-SET-TABLE*               ;character  set_number's
        (MAKE-ARRAY '(#o400 16.) ':TYPE 'ART-1B))
(DEFVAR *FSM-CHARACTER-SET-TABLE-16*            ;indirect array to above
        (MAKE-ARRAY #o400
                    ':TYPE 'ART-16B
                    ':DISPLACED-TO
                    *FSM-CHARACTER-SET-TABLE*))

(DEFUN BUILD-FSM (STRING-LIST
                  &OPTIONAL (CASE-DEPENDENT-P *ALPHABETIC-CASE-AFFECTS-SEARCH*)
                  &AUX NCHARN NSTATES)
  "Return a FSM that can be used to search for the strings in STRING-LIST.
These strings can contain extended search characters for pattern matching.
If CASE-DEPENDENT-P is T, the FSM is built so as not to ignore case.
See the code for FSM-SEARCH to see how to use an FSM."
  (SETQ *FSM-STRING-LIST* STRING-LIST)
  (SETQ NCHARN (BUILD-FSM-CHARACTER-SET STRING-LIST CASE-DEPENDENT-P))
  (SETQ NSTATES (BUILD-FSM-TREE STRING-LIST NCHARN))
  (CLEAN-FSM NSTATES NCHARN))

;;; Build up the character translation tables, returns number of character types
(DEFUN BUILD-FSM-CHARACTER-SET (STRING-LIST CASE-DEPENDENT-P &AUX (MAXCHAR 0))
  ;; First pass, get all the alphabetic characters
  (ARRAY-INITIALIZE *FSM-CHARACTER-TABLE* 0)
  (DOLIST (STRING STRING-LIST)
    (DO ((I 0 (1+ I))
         (LEN (ARRAY-LENGTH STRING))
         (CH))
        (( I LEN))
      (OR (LDB-TEST %%FSM-SET (SETQ CH (AREF STRING I)))
          (COND ((ZEROP (AREF *FSM-CHARACTER-TABLE* (SETQ CH (LDB %%FSM-CHAR CH))))
                 (ASET (SETQ MAXCHAR (1+ MAXCHAR)) *FSM-CHARACTER-TABLE* CH)
                 (AND (NOT CASE-DEPENDENT-P)
                      (OR (AND ( CH #/A) ( CH #/Z))
                          (AND ( CH #/a) ( CH #/z)))
                      (ASET MAXCHAR *FSM-CHARACTER-TABLE* (LOGXOR CH 40))))))))
  ;; Second pass, get the character types for all the sets mentioned
  (ARRAY-INITIALIZE *FSM-CHARACTER-SET-TABLE-16* 0)
  (LET ((LEN (LENGTH *FSM-SEARCH-SET-LIST*)))
    (IF (OR (NOT (BOUNDP '*FSM-SEARCH-SET-TABLE*))
            (< (ARRAY-LENGTH *FSM-SEARCH-SET-TABLE*) LEN))
        (SETQ *FSM-SEARCH-SET-TABLE* (MAKE-ARRAY LEN))
        (ARRAY-INITIALIZE *FSM-SEARCH-SET-TABLE* NIL)))
  (DOLIST (STRING STRING-LIST)
    (DO ((I 0 (1+ I))
         (LEN (ARRAY-LENGTH STRING))
         (CH))
        (( I LEN))
      (AND (LDB-TEST %%FSM-SET (SETQ CH (AREF STRING I)))
           (NOT (AREF *FSM-SEARCH-SET-TABLE* (SETQ CH (LDB %%FSM-CHAR CH))))
           (LET ((*LIST* NIL))
             (MAP-OVER-FSM-SEARCH-SET CH
               #'(LAMBDA (SET CH &AUX CHARN)
                   (IF (ZEROP (SETQ CHARN (AREF *FSM-CHARACTER-TABLE* CH)))
                       (ASET 1 *FSM-CHARACTER-SET-TABLE* CH SET)
                     (OR (MEMQ CHARN *LIST*) (PUSH CHARN *LIST*)))))
             (ASET (NREVERSE *LIST*) *FSM-SEARCH-SET-TABLE* CH)))))
  ;; Now assign character types for all the set intersections
  (DO ((CH 0 (1+ CH))
       (SET-ALIST NIL)
       (MASK) (ENTRY))
      (( CH 400))
    (COND ((NOT (ZEROP (SETQ MASK (AREF *FSM-CHARACTER-SET-TABLE-16* CH))))
           (COND ((NOT (SETQ ENTRY (ASSQ MASK SET-ALIST)))
                  (PUSH (SETQ ENTRY (CONS MASK (SETQ MAXCHAR (1+ MAXCHAR)))) SET-ALIST)
                  (DO ((SET 0 (1+ SET))
                       (BIT 0001 (+ BIT 0100)))
                      (( SET 16.))
                    (AND (LDB-TEST BIT MASK)
                         (PUSH MAXCHAR (AREF *FSM-SEARCH-SET-TABLE* SET))))))
           (ASET (CDR ENTRY) *FSM-CHARACTER-TABLE* CH))))
  ;; Finally return the number of character types
  (1+ MAXCHAR))

;;; Apply FUNCTION to all members of a character set,
;;; FUNCTION is caled with SET-NUMBER and character
(DEFUN MAP-OVER-FSM-SEARCH-SET (SET-NUMBER FUNCTION &AUX SET)
  (SETQ SET (NTH SET-NUMBER *FSM-SEARCH-SET-LIST*))
  (IF (ATOM SET)
      (DOTIMES (CH 400)
        (AND (FUNCALL SET CH) (FUNCALL FUNCTION SET-NUMBER CH)))
      (DOLIST (CHAR SET)
        (IF (NUMBERP CHAR)
            (FUNCALL FUNCTION SET-NUMBER CHAR)
            (DO ((CH (CAR CHAR) (1+ CH))
                 (LIM (CADR CHAR)))
                ((> CH LIM))
              (FUNCALL FUNCTION SET-NUMBER CH))))))

(DEFUN FSM-WORD-DELIMITER-CHAR-P (CH)
  (OR (> CH #o220) (= (WORD-SYNTAX CH) WORD-DELIMITER)))

(DEFUN FSM-ATOM-DELIMITER-CHAR-P (CH)
  (OR (> CH #o220) (= (ATOM-WORD-SYNTAX CH) WORD-DELIMITER)))

(DEFUN FSM-CHAR-TRUE (IGNORE) T)

;; Copied from LAD: RELEASE-3.ZWEI; SEARCH.LISP#91 on 2-Oct-86 02:45:20
;;; Build the actual tree from the strings, NCHARN is the number of character types,
;;; Returns the number of states
(DEFUN BUILD-FSM-TREE (STRING-LIST *NCHARN* &AUX (*MAXSTATE* 0))
  (DECLARE (SPECIAL *NCHARN* *MAXSTATE*))
  (array-initialize *fsm-state-table* 0)
; doing array-initialize instead of as below
; way speeds up things approx. 15x
;  (DOTIMES (I (ARRAY-DIMENSION *FSM-STATE-TABLE* 0))
;    (DOTIMES (J (ARRAY-DIMENSION *FSM-STATE-TABLE* 1))
;      (ASET 0 *FSM-STATE-TABLE* I J)))
  (ARRAY-INITIALIZE *FSM-WORD-TABLE* NIL)
  (DOLIST (STRING STRING-LIST)
    (BUILD-FSM-TREE-1 STRING 0 0 (ARRAY-LENGTH STRING)))
  (1+ *MAXSTATE*))

;; Handle a single character
(DEFUN BUILD-FSM-TREE-1 (STRING INDEX STATE LENGTH &AUX CHAR NOT-P STAR-P SET-P CH STAR1-P)
  (DECLARE (SPECIAL *NCHARN* *MAXSTATE*))
  (COND (( INDEX LENGTH)
         (ASET STRING *FSM-WORD-TABLE* STATE))  ;End of string, save winner
        ((= (SETQ CHAR (AREF STRING INDEX)) %FSM-NOOP)
         (BUILD-FSM-TREE-1 STRING (1+ INDEX) STATE LENGTH))
        (T
         (SETQ NOT-P (LDB-TEST %%FSM-NOT CHAR)
               STAR-P (LDB-TEST %%FSM-STAR CHAR)
               SET-P (LDB-TEST %%FSM-SET CHAR)
               STAR1-P (LDB-TEST %%FSM-STAR1 CHAR)
               CH (LDB %%FSM-CHAR CHAR))
         (IF SET-P
             (LET ((SET (AREF *FSM-SEARCH-SET-TABLE* CH)))
               (IF NOT-P
                   (DOTIMES (NCH *NCHARN*)
                     (OR (MEMQ NCH SET)
                         (BUILD-FSM-TREE-2 STRING INDEX STATE LENGTH NCH STAR-P STAR1-P)))
                   (DOLIST (NCH SET)
                     (BUILD-FSM-TREE-2 STRING INDEX STATE LENGTH NCH STAR-P STAR1-P))))
             (SETQ CH (AREF *FSM-CHARACTER-TABLE* CH))
             (IF NOT-P
                 (DOTIMES (NCH *NCHARN*)
                   (OR (= NCH CH)
                       (BUILD-FSM-TREE-2 STRING INDEX STATE LENGTH NCH STAR-P STAR1-P)))
                 (BUILD-FSM-TREE-2 STRING INDEX STATE LENGTH CH STAR-P STAR1-P))))))

;; Handle a single state transition
(DEFUN BUILD-FSM-TREE-2 (STRING INDEX STATE LENGTH CHARN STAR-P STAR1-P &AUX NEW-STATE)
  (DECLARE (SPECIAL *NCHARN* *MAXSTATE*))
  (AND (OR STAR-P STAR1-P) (ASET (SETQ NEW-STATE STATE) *FSM-STATE-TABLE* STATE CHARN))
  (AND (OR (NOT STAR-P) STAR1-P)
       (AND (ZEROP (SETQ NEW-STATE (AREF *FSM-STATE-TABLE* STATE CHARN)))
            (ASET (SETQ NEW-STATE (SETQ *MAXSTATE* (1+ *MAXSTATE*)))
                  *FSM-STATE-TABLE* STATE CHARN)))
  (BUILD-FSM-TREE-1 STRING (1+ INDEX) NEW-STATE LENGTH))

;;; Clean up the fsm and build up the backpointers
(DEFUN CLEAN-FSM (NSTATES NCHARS)
  (IF (OR (NOT (BOUNDP '*FSM-BACKPOINTER-TABLE*))
          (< (ARRAY-LENGTH *FSM-BACKPOINTER-TABLE*) NSTATES))
      (SETQ *FSM-BACKPOINTER-TABLE* (MAKE-ARRAY NSTATES))
    (ARRAY-INITIALIZE *FSM-BACKPOINTER-TABLE* NIL))
  (DOTIMES (STATE NSTATES)
    (DOTIMES (CH NCHARS)
      (LET ((NEW-STATE (AREF *FSM-STATE-TABLE* STATE CH)))
        (AND (> NEW-STATE STATE) (NULL (AREF *FSM-BACKPOINTER-TABLE* NEW-STATE))
             (LET ((GCTAIL (LET ((BACKPTR (AREF *FSM-BACKPOINTER-TABLE* STATE)))
                             (IF BACKPTR (AREF *FSM-STATE-TABLE* BACKPTR CH) 0))))
               (ASET GCTAIL *FSM-BACKPOINTER-TABLE* NEW-STATE)
               (OR (AREF *FSM-WORD-TABLE* NEW-STATE)
                   (ASET (AREF *FSM-WORD-TABLE* GCTAIL) *FSM-WORD-TABLE* NEW-STATE))
               (DOTIMES (NCH NCHARS)
                 (AND (ZEROP (AREF *FSM-STATE-TABLE* NEW-STATE NCH))
                      (ASET (AREF *FSM-STATE-TABLE* GCTAIL NCH)
                            *FSM-STATE-TABLE* NEW-STATE NCH)))))))
    (LET ((BACKPTR (AREF *FSM-BACKPOINTER-TABLE* STATE)))
      (AND (LET ((WORD (AND BACKPTR (AREF *FSM-WORD-TABLE* BACKPTR))))
             (OR (NULL WORD) (EQ WORD (AREF *FSM-WORD-TABLE* STATE))))
           (ASET (AND BACKPTR (AREF *FSM-BACKPOINTER-TABLE* BACKPTR))
                 *FSM-BACKPOINTER-TABLE* STATE)))))

;;; Attempt a matching of a string
(DEFUN MATCH-FSM (STRING)
  (DO ((I 0 (1+ I))
       (LEN (STRING-LENGTH STRING))
       (LIST NIL)
       (STATE 0)
       (CH))
      (( I LEN) (NREVERSE LIST))
    (SETQ CH (AREF *FSM-CHARACTER-TABLE* (CHAR-CODE (AREF STRING I)))
          STATE (AREF *FSM-STATE-TABLE* STATE CH))
    (DO ((STATE STATE (AREF *FSM-BACKPOINTER-TABLE* STATE))
         (OSTATE NIL STATE)
         (WORD))
        ((OR (NULL STATE) (EQ STATE OSTATE)))
      (OR (SETQ WORD (AREF *FSM-WORD-TABLE* STATE))
          (RETURN NIL))
      (PUSH (LIST (1+ I) WORD) LIST))))

(DEFUN FSM-STRING-SEARCH (KEY STRING &OPTIONAL FROM TO
                                     &AUX (LEN (STRING-LENGTH STRING))
                                          (STRINGS (CAR KEY)) (EXPR (CADR KEY))
                                          (CR-P (CADDR KEY)))
  "Search STRING or part of it for a match for KEY.
Mostly like STRING-SEARCH except:
The first element of KEY is a list of strings, and the second
is a filtering function or NIL.  If non-NIL, it is passed a list
of indices within the line, and whatever it returns, we return."
  (OR (EQ STRINGS *FSM-STRING-LIST*)
      (BUILD-FSM STRINGS))
  (OR FROM (SETQ FROM (IF CR-P -1 0)))
  (OR TO (SETQ TO (IF CR-P (1+ LEN) LEN)))
  (DO ((I FROM (1+ I))
       (LIST NIL)
       (STATE 0)
       (CH))
      (( I TO)
       (SETQ LIST (NREVERSE LIST))
       (IF EXPR (FUNCALL EXPR LIST) (CAAR LIST)))
    (SETQ CH (AREF *FSM-CHARACTER-TABLE* (IF (OR (MINUSP I) ( I LEN))
                                             #/NEWLINE
                                             (CHAR-CODE (AREF STRING I))))
          STATE (AREF *FSM-STATE-TABLE* STATE CH))
    (DO ((STATE STATE (AREF *FSM-BACKPOINTER-TABLE* STATE))
         (OSTATE NIL STATE)
         (WORD))
        ((OR (NULL STATE) (EQ STATE OSTATE)))
      (OR (SETQ WORD (AREF *FSM-WORD-TABLE* STATE))
          (RETURN NIL))
      (PUSH (LIST (1+ I) WORD) LIST))))

;;; Do fsm search within lines
(DEFUN FSM-SEARCH-WITHIN-LINES (BP KEY &OPTIONAL REVERSE-P FIXUP-P IGNORE LIMIT-BP
                                       &AUX (START-LINE (BP-LINE BP))
                                            (START-INDEX (BP-INDEX BP)))
  "Search from BP for a match for KEY.
We look for a match for KEY within each line individually.
Mostly called like SEARCH, but the LINES-TO-SEARCH argument is ignored.
The first element of KEY is a list of strings, and the second
is a filtering function or NIL.
If non-NIL, it is passed a list of indices within the line.
If it returns NIL, there is no match; else it is the index
 within the line of the match."
  (OR LIMIT-BP (SETQ LIMIT-BP (IF REVERSE-P
                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                  (INTERVAL-LAST-BP *INTERVAL*))))
  (IF (NOT REVERSE-P)
      (DO ((LINE START-LINE (LINE-NEXT LINE))
           (LAST-LINE (BP-LINE LIMIT-BP))
           (LAST-INDEX (BP-INDEX LIMIT-BP))
           (INDEX))
          (NIL)
        (AND (SETQ INDEX (FSM-STRING-SEARCH KEY LINE
                                            (AND (EQ LINE START-LINE) START-INDEX)
                                            (AND (EQ LINE LAST-LINE) LAST-INDEX)))
             (RETURN (CREATE-BP LINE INDEX)))
        (AND (EQ LINE LAST-LINE)
             (RETURN (AND FIXUP-P LIMIT-BP))))
      (DO ((LINE START-LINE (LINE-NEXT LINE))
           (FIRST-LINE (BP-LINE LIMIT-BP))
           (FIRST-INDEX (BP-INDEX LIMIT-BP))
           (INDEX))
          (NIL)
        (AND (SETQ INDEX (FSM-STRING-SEARCH KEY LINE
                                            (AND (EQ LINE FIRST-LINE) FIRST-INDEX)
                                            (AND (EQ LINE START-LINE) START-INDEX)))
             (RETURN (CREATE-BP LINE INDEX)))
        (AND (EQ LINE FIRST-LINE)
             (RETURN (AND FIXUP-P LIMIT-BP))))))

;;; Search by characters
(DEFUN FSM-SEARCH (BP STRINGS &OPTIONAL REVERSE-P FIXUP-P IGNORE LIMIT-BP (STATE 0)
                              &AUX WORD)
  "Search from BP for a match for one of STRINGS.
The match may cross a line boundary.
Mostly called like SEARCH, but the LINES-TO-SEARCH argument is ignored.
The first value is like that of SEARCH.
The second is the string that was found.
The third is the FSM state, which can be given back as the STATE argument
if you search for another occurrence."
  (OR LIMIT-BP (SETQ LIMIT-BP (IF REVERSE-P
                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                  (INTERVAL-LAST-BP *INTERVAL*))))
  (AND REVERSE-P (FERROR NIL "Backwards FSM search does not work, bitch at MMcM"))
  (OR (EQ STRINGS *FSM-STRING-LIST*)
      (BUILD-FSM STRINGS))
  (CHARMAP (BP LIMIT-BP (AND FIXUP-P LIMIT-BP))
    (SETQ STATE (AREF *FSM-STATE-TABLE* STATE
                      (AREF *FSM-CHARACTER-TABLE*
                            (IF *ALPHABETIC-CASE-AFFECTS-SEARCH*
                                (CHARMAP-CH-CHAR)
                                (CHAR-UPCASE (CHARMAP-CH-CHAR))))))
    (AND (SETQ WORD (AREF *FSM-WORD-TABLE* STATE))
         (CHARMAP-RETURN (VALUES (CHARMAP-BP-AFTER) WORD STATE)))))

(DEFUN FSM-EXPR-SEARCH (BP KEY &OPTIONAL REVERSE-P FIXUP-P IGNORE LIMIT-BP (STATE 0)
                               &AUX (STRINGS (CAR KEY)) (EXPR (CADR KEY)) LIST)
  "Search from BP for a match for KEY.
Mostly called like SEARCH, but the LINES-TO-SEARCH argument is ignored.
We look for a match for KEY, which may cross a line boundary.
The first element of KEY is a list of strings, and the second
is a filtering function or NIL.
If non-NIL, it is passed a list of indices within the line.
If it returns NIL, there is no match; else it is the index
 within the line of the match.
The first value is like that of SEARCH.
The second is the string that was found.
The third is the FSM state, which can be given back as the STATE argument
if you search for another occurrence."
  (AND REVERSE-P (FERROR NIL "Backwards FSM search does not work, bitch at MMcM"))
  (OR (EQ STRINGS *FSM-STRING-LIST*)
      (BUILD-FSM STRINGS))
  (CHARMAP (BP LIMIT-BP (AND FIXUP-P LIMIT-BP))
    (SETQ STATE (AREF *FSM-STATE-TABLE* STATE (AREF *FSM-CHARACTER-TABLE* (CHARMAP-CH-CHAR))))
    (DO ((STATE STATE (AREF *FSM-BACKPOINTER-TABLE* STATE))
         (OSTATE NIL STATE)
         (WORD))
        ((OR (NULL STATE) (EQ STATE OSTATE)))
      (OR (SETQ WORD (AREF *FSM-WORD-TABLE* STATE))
          (RETURN NIL))
      (PUSH (LIST (CHARMAP-BP-BEFORE) WORD) LIST)))
  (SETQ LIST (NREVERSE LIST))
  (IF EXPR (FUNCALL EXPR LIST) (CAAR LIST)))

(DEFVAR *SEARCH-MINI-BUFFER-COMTAB*)
(DEFVAR *STRING-SEARCH-MINI-BUFFER-COMTAB*)
(DEFVAR *SEARCH-CONTROL-H-COMTAB*)
(DEFVAR *STRING-SEARCH-CONTROL-H-COMTAB*)
(DEFVAR *STRING-SEARCH-SINGLE-LINE-COMTAB*)
(DEFUN INITIALIZE-EXTENDED-SEARCH ()
  (COND ((NOT (BOUNDP '*SEARCH-MINI-BUFFER-COMTAB*))
         (SETQ *SEARCH-CONTROL-H-COMTAB*
               (SET-COMTAB NIL '(#/( COM-EXTENDED-SEARCH-OPEN
                                 #/) COM-EXTENDED-SEARCH-CLOSE
                                 #/ COM-EXTENDED-SEARCH-OR
                                 #/C-O COM-EXTENDED-SEARCH-OR
                                 #/ COM-EXTENDED-SEARCH-AND
                                 #/& COM-EXTENDED-SEARCH-AND
                                 #/C-A COM-EXTENDED-SEARCH-AND
                                 #/C-N COM-EXTENDED-SEARCH-NOT
                                 #/~ COM-EXTENDED-SEARCH-NOT
                                 #/ COM-EXTENDED-SEARCH-NOT
                                 #/SP COM-EXTENDED-SEARCH-WHITESPACE
                                 #/- COM-EXTENDED-SEARCH-DELIMITER
                                 #/A COM-EXTENDED-SEARCH-ALPHABETIC
                                 #/* COM-EXTENDED-SEARCH-SOME
                                 #/C-X COM-EXTENDED-SEARCH-ANY
                                 #/HELP COM-DOCUMENT-CONTAINING-PREFIX-COMMAND
                                 )))
         (SET-COMTAB-CONTROL-INDIRECTION *SEARCH-CONTROL-H-COMTAB*)
         (SETQ *SEARCH-MINI-BUFFER-COMTAB*
               (SET-COMTAB NIL (LIST #/HELP 'COM-DOCUMENT-EXTENDED-SEARCH
                                     #/C-H (MAKE-EXTENDED-COMMAND *SEARCH-CONTROL-H-COMTAB*))))
         (SET-COMTAB-INDIRECTION *SEARCH-MINI-BUFFER-COMTAB* *MINI-BUFFER-COMTAB*)))
  (COND ((NOT (BOUNDP '*STRING-SEARCH-CONTROL-H-COMTAB*))
         (SETQ *STRING-SEARCH-CONTROL-H-COMTAB*
               (SET-COMTAB NIL '(#/C-B COM-EXTENDED-SEARCH-BEGINNING
                                 #/C-E COM-EXTENDED-SEARCH-END
                                 #/C-F COM-EXTENDED-SEARCH-TOP-LINE
                                 #/C-R COM-EXTENDED-SEARCH-REVERSE)))
         (SET-COMTAB-INDIRECTION *STRING-SEARCH-CONTROL-H-COMTAB* *SEARCH-CONTROL-H-COMTAB*)
         (SETQ *STRING-SEARCH-MINI-BUFFER-COMTAB*
               (SET-COMTAB NIL (LIST #/ 'COM-END-OF-MINI-BUFFER
                                     #/HELP 'COM-DOCUMENT-EXTENDED-SEARCH
                                     #/C-H (MAKE-EXTENDED-COMMAND
                                             *STRING-SEARCH-CONTROL-H-COMTAB*))))
         (SET-COMTAB-INDIRECTION *STRING-SEARCH-MINI-BUFFER-COMTAB*
                                 *MINI-BUFFER-MULTI-LINE-COMTAB*)))
  (COND ((NOT (BOUNDP '*STRING-SEARCH-SINGLE-LINE-COMTAB*))
         (SETQ *STRING-SEARCH-SINGLE-LINE-COMTAB*
               (SET-COMTAB NIL '(#/CR COM-END-OF-MINI-BUFFER)))
         (SET-COMTAB-INDIRECTION *STRING-SEARCH-SINGLE-LINE-COMTAB*
                                 *STRING-SEARCH-MINI-BUFFER-COMTAB*))))

(ADD-INITIALIZATION "INITIALIZE-EXTENDED-SEARCH" '(INITIALIZE-EXTENDED-SEARCH)
                    '(:NORMAL) '*EDITOR-INITIALIZATION-LIST*)

(DEFUN GET-SEARCH-MINI-BUFFER-WINDOW ()
  (FUNCALL *MODE-LINE-WINDOW* ':SEARCH-MINI-BUFFER-WINDOW))

(DEFVAR *SEARCH-MINI-BUFFER-NAME*)
(DEFUN GET-EXTENDED-SEARCH-STRINGS (*SEARCH-MINI-BUFFER-NAME*
                                    &AUX STR STRINGS EXPR CR-P FUNCTION)
  "Read with the mini buffer some strings to look for as substrings with FSM search.
Use this when you want to match substrings of strings
rather than search through a buffer.
*SEARCH-MINI-BUFFER-NAME* is a string to prompt with, ending in a colon.
Returns two values, a search function and an arg to give it.
The function might be STRING-SEARCH, or might just take args like it.
 The second value should be given as the first arg to the function."
  (DECLARE (VALUES FUNCTION ARG))
  (LET ((*MINI-BUFFER-WINDOW* (GET-SEARCH-MINI-BUFFER-WINDOW)))
    (EDIT-IN-MINI-BUFFER *SEARCH-MINI-BUFFER-COMTAB* NIL NIL
                         '(*SEARCH-MINI-BUFFER-NAME*
                            (:RIGHT-FLUSH " (Extended search characters)"))))
  (SETQ STR (SEARCH-MINI-BUFFER-STRING-INTERVAL))
  (MULTIPLE-VALUE (STRINGS EXPR CR-P)
    (PARSE-EXTENDED-SEARCH-STRING STR))
  (IF (OR (CONSP STRINGS) CR-P)
      (SETQ FUNCTION 'FSM-STRING-SEARCH
            STRINGS (LIST (IF (CONSP STRINGS) STRINGS (NCONS STRINGS)) EXPR CR-P))
      (SETQ FUNCTION 'STRING-SEARCH))
  (VALUES FUNCTION STRINGS STR))

(DEFCOM COM-DOCUMENT-EXTENDED-SEARCH "Simple help for hairy search" ()
  (FORMAT T "~%You are typing in a search string.  Control-H is a prefix for more commands.")
  DIS-NONE)

(DEFCOM COM-EXTENDED-SEARCH-OPEN "Beginning of new nesting level" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 0 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-CLOSE "End of nesting level" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 1 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-OR "Infix or of two strings" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 2 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-AND "Infix and of two string with lines" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 3 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-NOT "Negation on next character" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 4 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-WHITESPACE "Match any whitespace character" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 5 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-SOME "Match any number of next character" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 6 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-DELIMITER "Match any delimiter" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 7 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-ALPHABETIC "Match any alphabetic character" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 8. 0 1))
  DIS-TEXT)

(DEFCOM COM-EXTENDED-SEARCH-ANY "Match any character" ()
  (INSERT-MOVING (POINT) (MAKE-CHAR 9. 0 1))
  DIS-TEXT)

(DEFVAR *EXTENDED-STRING-SEARCH-LAST-ARG* "")
(DEFVAR *EXTENDED-STRING-SEARCH-LAST-FUNCTION* 'ZWEI-SEARCH)
(DEFCOM COM-EXTENDED-STRING-SEARCH "Search for a hairy string
String is read in a mini-buffer in which Control-H is a prefix for special characters" ()
  (EXTENDED-STRING-SEARCH-INTERNAL NIL))

(DEFCOM COM-EXTENDED-REVERSE-STRING-SEARCH "Search for a hairy string backward
String is read in a mini-buffer in which Control-H is a prefix for special characters" ()
  (EXTENDED-STRING-SEARCH-INTERNAL T))

(DEFUN EXTENDED-STRING-SEARCH-INTERNAL (REVERSE-P &AUX (BP (POINT)) BJ-P TOP-LINE-P)
  (OR *NUMERIC-ARG-P*
      (MULTIPLE-VALUE (*EXTENDED-STRING-SEARCH-LAST-FUNCTION*
                       *EXTENDED-STRING-SEARCH-LAST-ARG*
                       REVERSE-P BJ-P TOP-LINE-P)
        (GET-EXTENDED-STRING-SEARCH-STRINGS REVERSE-P)))
  (AND BJ-P
       (SETQ BP (IF REVERSE-P (INTERVAL-LAST-BP *INTERVAL*) (INTERVAL-FIRST-BP *INTERVAL*))))
  (OR (SETQ BP (FUNCALL *EXTENDED-STRING-SEARCH-LAST-FUNCTION* BP
                        *EXTENDED-STRING-SEARCH-LAST-ARG* REVERSE-P))
      (BARF))
  (MOVE-BP (POINT) BP)
  DIS-BPS)

(DEFVAR *EXTENDED-STRING-SEARCH-BJ-P*)
(DEFVAR *EXTENDED-STRING-SEARCH-ZJ-P*)
(DEFVAR *EXTENDED-STRING-SEARCH-REVERSE-P*)
(DEFVAR *EXTENDED-STRING-SEARCH-TOP-LINE-P*)

;;; Read a string for string search and then return the function to use
(DEFUN GET-EXTENDED-STRING-SEARCH-STRINGS (&OPTIONAL *EXTENDED-STRING-SEARCH-REVERSE-P*
                                                     (*SEARCH-MINI-BUFFER-NAME* "Search:")
                                                   (COMTAB *STRING-SEARCH-MINI-BUFFER-COMTAB*)
                                           &AUX (*EXTENDED-STRING-SEARCH-BJ-P* NIL)
                                                (*EXTENDED-STRING-SEARCH-ZJ-P* NIL)
                                                (*EXTENDED-STRING-SEARCH-TOP-LINE-P* NIL)
                                                STRINGS EXPR CR-P FUNCTION)
  "Read with the mini buffer some strings to look for in the buffer with FSM search.
*EXTENDED-STRING-SEARCH-REVERSE-P* should be T
 if the search is going to be backward by default.
*SEARCH-MINI-BUFFER-NAME* is a string to prompt with, ending in a colon.
COMTAB is the comtab to use in the mini buffer.
Returns five values.
The first two are a search function and an arg to give it.
The function takes args like SEARCH (but use NIL as the LINES-TO-SEARCH arg).
 The second value should be given as the first arg to the function.
The third value is T if the search really should be reverse.
Thr fourth value is T if the search should start from the beginning
 or end of the buffer rather than from point (assuming from point is your default).
The fifth value is T if you should put the thing found at the top of the window.
The last three values reflect commands the user typed while in the minibuffer."
  (DECLARE (VALUES FUNCTION ARG REVERSE-P BJ-P TOP-LINE-P))
  (LET ((*MINI-BUFFER-WINDOW* (GET-SEARCH-MINI-BUFFER-WINDOW)))
    (EDIT-IN-MINI-BUFFER COMTAB NIL NIL
                         '((*EXTENDED-STRING-SEARCH-BJ-P* "BJ ")
                           (*EXTENDED-STRING-SEARCH-ZJ-P* "ZJ ")
                           (*EXTENDED-STRING-SEARCH-REVERSE-P* "Reverse ")
                           (*EXTENDED-STRING-SEARCH-TOP-LINE-P* "Top line ")
                           *SEARCH-MINI-BUFFER-NAME*
                           (:RIGHT-FLUSH " (Extended search characters)"))))
  (MULTIPLE-VALUE (STRINGS EXPR CR-P)
    (PARSE-EXTENDED-SEARCH-STRING))
  (IF (CONSP STRINGS)
      (IF EXPR
          (SETQ FUNCTION 'FSM-SEARCH-WITHIN-LINES
                STRINGS (LIST STRINGS EXPR CR-P))
          (SETQ FUNCTION 'FSM-SEARCH))
      (SETQ FUNCTION 'ZWEI-SEARCH))
  (VALUES FUNCTION STRINGS
          *EXTENDED-STRING-SEARCH-REVERSE-P*
          (OR *EXTENDED-STRING-SEARCH-BJ-P* *EXTENDED-STRING-SEARCH-ZJ-P*)
          *EXTENDED-STRING-SEARCH-TOP-LINE-P*))

(DEFCOM COM-EXTENDED-SEARCH-BEGINNING "" ()
  (COND ((SETQ *EXTENDED-STRING-SEARCH-BJ-P* (NOT *EXTENDED-STRING-SEARCH-BJ-P*))
         (SETQ *EXTENDED-STRING-SEARCH-ZJ-P* NIL
               *EXTENDED-STRING-SEARCH-REVERSE-P* NIL)))
  DIS-NONE)

(DEFCOM COM-EXTENDED-SEARCH-END "" ()
  (COND ((SETQ *EXTENDED-STRING-SEARCH-ZJ-P* (NOT *EXTENDED-STRING-SEARCH-ZJ-P*))
         (SETQ *EXTENDED-STRING-SEARCH-BJ-P* NIL
               *EXTENDED-STRING-SEARCH-REVERSE-P* T)))
  DIS-NONE)

(DEFCOM COM-EXTENDED-SEARCH-TOP-LINE "" ()
  (SETQ *EXTENDED-STRING-SEARCH-TOP-LINE-P* (NOT *EXTENDED-STRING-SEARCH-TOP-LINE-P*))
  DIS-NONE)

(DEFCOM COM-EXTENDED-SEARCH-REVERSE "" ()
  (IF (SETQ *EXTENDED-STRING-SEARCH-REVERSE-P* (NOT *EXTENDED-STRING-SEARCH-REVERSE-P*))
      (SETQ *EXTENDED-STRING-SEARCH-BJ-P* NIL)
      (SETQ *EXTENDED-STRING-SEARCH-ZJ-P* NIL))
  DIS-NONE)

(DEFVAR *EXTENDED-SEARCH-CR-P*)
(DEFUN PARSE-EXTENDED-SEARCH-STRING (&OPTIONAL STRING
                                     &AUX *EXTENDED-SEARCH-CR-P* STRINGS EXPR LEN)
  (OR STRING
      (SETQ STRING (STRING-INTERVAL (WINDOW-INTERVAL (GET-SEARCH-MINI-BUFFER-WINDOW)))))
  (MULTIPLE-VALUE (STRINGS EXPR LEN)
    (PARSE-EXTENDED-SEARCH-STRING-1 STRING 0 (STRING-LENGTH STRING) 0))
  (AND (ATOM STRINGS)
       (DOTIMES (I LEN)
         (AND (LDB-TEST #o1010 (AREF STRINGS I))
              (RETURN T)))
       (SETQ STRINGS (NCONS STRINGS)))
  (OR STRINGS (SETQ STRINGS ""))
  (AND EXPR
       (LET ((SYMBOL (GENSYM)))
         (FSET SYMBOL `(LAMBDA (MATCHING-LIST) ,EXPR))
         (COMPILE SYMBOL)
         (SETQ EXPR SYMBOL)))
  (VALUES STRINGS EXPR *EXTENDED-SEARCH-CR-P*))

(DEFUN PARSE-EXTENDED-SEARCH-STRING-1 (STRING INDEX LENGTH PAREN-LEVEL)
  (DO ((I INDEX (1+ I))
       (SYN) (CH)
       (NEW-STRINGS) (NEW-EXPR)
       (OLD-STRINGS) (OLD-EXPR))
      (NIL)
    (MULTIPLE-VALUE (SYN CH I)
      (PARSE-EXTENDED-SEARCH-STRING-SYNTAX STRING I LENGTH))
    (IF (ZEROP SYN)
        (AND OLD-STRINGS (IF (CONSP OLD-STRINGS)
                             (DO ((OS OLD-STRINGS (CDR OS)))
                                 ((NULL OS))
                               (SETF (CAR OS) (STRING-APPEND (CAR OS) CH)))
                             (SETQ OLD-STRINGS (STRING-APPEND OLD-STRINGS CH))))
        (OR OLD-STRINGS
            (SETQ OLD-STRINGS (AND (NOT (= INDEX I)) (SUBSTRING STRING INDEX I))))
        (COND ((BIT-TEST 120000 SYN)            ;EOF or CLOSE
               (IF (PLUSP PAREN-LEVEL)
                   (AND (= SYN 100000)          ;EOF
                        (BARF "End of string inside parenthesis"))
                   (AND (= SYN 20000)           ;CLOSE
                        (BARF "Unmatched close")))
               (RETURN (VALUES OLD-STRINGS OLD-EXPR I))))
        (COND ((= SYN 40000)                    ;OPEN
               (MULTIPLE-VALUE (NEW-STRINGS NEW-EXPR I)
                 (PARSE-EXTENDED-SEARCH-STRING-1 STRING (1+ I) LENGTH (1+ PAREN-LEVEL)))
               (IF (NOT OLD-STRINGS)
                   (SETQ OLD-STRINGS NEW-STRINGS)
                   (AND OLD-EXPR (BARF "I don't know how to combine these"))
                   (COND ((CONSP NEW-STRINGS)
                          (AND (CONSP OLD-STRINGS)
                               (BARF "I don't know how to combine these"))
                          (DO ((NS NEW-STRINGS (CDR NS)))
                              ((NULL NS) (SETQ OLD-STRINGS NEW-STRINGS))
                            (SETF (CAR NS) (STRING-APPEND OLD-STRINGS
                                                          (CAR NS)))))
                         ((CONSP OLD-STRINGS)
                          (DO ((OS OLD-STRINGS (CDR OS)))
                              ((NULL OS))
                            (SETF (CAR OS) (STRING-APPEND (CAR OS) NEW-STRINGS))))
                         (T
                          (SETQ OLD-STRINGS (STRING-APPEND OLD-STRINGS
                                                           NEW-STRINGS)))))
               (SETQ OLD-EXPR NEW-EXPR))
              (T
               (OR OLD-STRINGS (BARF "Special token at the beginning of the string"))
               (AND (OR (= SYN 10000) NEW-EXPR)
                    (OR OLD-EXPR
                        (SETQ OLD-EXPR (IF (CONSP OLD-STRINGS)
                                           (CONS 'OR
                                                 (MAPCAR #'EXTENDED-SEARCH-STRING-MATCHER
                                                         OLD-STRINGS))
                                           (EXTENDED-SEARCH-STRING-MATCHER OLD-STRINGS)))))
               (OR (CONSP OLD-STRINGS)
                   (SETQ OLD-STRINGS (NCONS OLD-STRINGS)))
               (MULTIPLE-VALUE (NEW-STRINGS NEW-EXPR I)
                 (PARSE-EXTENDED-SEARCH-STRING-1 STRING (1+ I) LENGTH PAREN-LEVEL))
               (OR (CONSP NEW-STRINGS)
                   (SETQ NEW-STRINGS (NCONS NEW-STRINGS)))
               (IF (= SYN 4000)                 ;OR
                   (RETURN (VALUES (NCONC OLD-STRINGS NEW-STRINGS)
                                   (AND NEW-EXPR `(OR ,OLD-EXPR ,NEW-EXPR))
                                   I))
                   (OR NEW-EXPR                 ;AND
                       (SETQ NEW-EXPR (CONS 'OR
                                            (MAPCAR #'EXTENDED-SEARCH-STRING-MATCHER
                                                    NEW-STRINGS))))
                   (RETURN (VALUES  (NCONC OLD-STRINGS NEW-STRINGS)
                                    `(AND ,OLD-EXPR ,NEW-EXPR)
                                    I))))))))

(DEFUN PARSE-EXTENDED-SEARCH-STRING-SYNTAX (STRING INDEX LENGTH &OPTIONAL NOT-TOP-LEVEL
                                                               &AUX CH)
  (IF ( INDEX LENGTH)
      (IF NOT-TOP-LEVEL (BARF "EOF in special context") (VALUES #o100000 NIL INDEX))
      (VALUES (IF (ZEROP (CHAR-FONT (SETQ CH (CHAR STRING INDEX))))
                  (PROG1 0 (AND (CHAR= CH #/RETURN) (SETQ *EXTENDED-SEARCH-CR-P* T)))
                  (CASE (SETQ CH (CHAR-CODE CH))
                    (0 #o40000)                 ;OPEN
                    (1 #o20000)                 ;CLOSE
                    (2 #o4000)                  ;AND
                    (3 #o10000)                 ;OR
                    ((4 6 !FOO!)                ;NOT,SOME,!FOO!
                     (MULTIPLE-VALUE-BIND (SYN NCH)
                         (PARSE-EXTENDED-SEARCH-STRING-SYNTAX STRING (1+ INDEX)
                                                              LENGTH T)
                       (OR (ZEROP SYN) (BARF "NOT modifier on a special character"))
                       (ASET (SETQ CH (DPB 1 (CASE CH
                                               (4 %%FSM-NOT)
                                               (6 %%FSM-STAR1)
                                               (!FOO! %%FSM-STAR))
                                           NCH))
                             STRING INDEX)
                       (ASET %FSM-NOOP STRING (INCF INDEX)))
                     0)
                    ((5 7 10 11)                ;WHITESPACE,DELIMITER,ALPHABETIC,ANY
                     (ASET (SETQ CH (DPB 1 %%FSM-SET
                                         (CASE CH
                                           (5 0)
                                           (7 5)
                                           (8. 2)
                                           (9. 7))))
                           STRING INDEX)
                     0)
                    (OTHERWISE 0)))
                CH INDEX)))

(DEFUN EXTENDED-SEARCH-STRING-MATCHER (STRING)
  `(DO ((L MATCHING-LIST (CDR L))) ((NULL L))
     (AND (EQ (CADAR L) ',STRING) (RETURN (CAAR L)))))

;;; Special handling for search strings
(DEFSTRUCT (16B-STRING :ARRAY-LEADER :NAMED
                       (:MAKE-ARRAY (:LENGTH 20. :TYPE 'ART-FAT-STRING))
                       (:ALTERANT NIL))
  (16B-STRING-LENGTH 0))

(DEFPROP 16B-STRING 16B-STRING-NAMED-STRUCTURE-INVOKE NAMED-STRUCTURE-INVOKE)
(DEFSELECT 16B-STRING-NAMED-STRUCTURE-INVOKE
  ((:PRINT-SELF) (STRING &OPTIONAL (STREAM STANDARD-OUTPUT) IGNORE IGNORE)
   (AND *PRINT-ESCAPE* (GET-DISPATCH-MACRO-CHARACTER #/# #/")
        (FUNCALL STREAM ':TYO #/#))
   (SI:PRINT-QUOTED-STRING STRING STREAM
                           (MEMQ ':STRING-OUT (FUNCALL STREAM ':WHICH-OPERATIONS)))))

(DEFUN MY-/#/"-MACRO (IGNORE STREAM)
  (DO ((STRING (MAKE-16B-STRING))
       (CHAR)
       (SLASH-P NIL))
      (NIL)
    (SETQ CHAR (READ-CHAR STREAM))
    (COND (SLASH-P
           (SETQ SLASH-P NIL)
           (VECTOR-PUSH-EXTEND CHAR STRING))
          ((CHAR= CHAR #/")
           (RETURN STRING))
          ((CHAR= CHAR #//)
           (SETQ SLASH-P T))
          (T
           (VECTOR-PUSH-EXTEND CHAR STRING)))))

(DEFVAR SPECIAL-/#/"-READTABLE)
(DEFUN INITIALIZE-SPECIAL-/#/"-READTABLE ()
  (UNLESS (BOUNDP 'SPECIAL-/#/"-READTABLE)
    (SETQ SPECIAL-/#/"-READTABLE (SI:COPY-READTABLE))
    (SETF (SI:RDTBL-NAMES SPECIAL-/#/"-READTABLE) '("Special #/" Readtable"))
    (SET-SYNTAX-/#-MACRO-CHAR #/" 'MY-/#/"-MACRO SPECIAL-/#/"-READTABLE))
  SPECIAL-/#/"-READTABLE)

(DEFUN SEARCH-MINI-BUFFER-STRING-INTERVAL (&AUX INT STRING)
  (SETQ INT (WINDOW-INTERVAL (GET-SEARCH-MINI-BUFFER-WINDOW)))
  (LET ((LEN (COUNT-CHARS INT)))
    (SETQ STRING (MAKE-16B-STRING 16B-STRING-LENGTH LEN
                                  MAKE-ARRAY (:LENGTH LEN))))
  (LET ((INT-STRING (STRING-INTERVAL INT)))
    (COPY-ARRAY-CONTENTS INT-STRING STRING))
  STRING)

;;; Return a string itself, suitable for printing and reading back
(DEFUN GET-EXTENDED-SEARCH-16B-STRING (*SEARCH-MINI-BUFFER-NAME*)
  (LET ((*MINI-BUFFER-WINDOW* (GET-SEARCH-MINI-BUFFER-WINDOW)))
    (EDIT-IN-MINI-BUFFER *SEARCH-MINI-BUFFER-COMTAB* NIL NIL
                         '(*SEARCH-MINI-BUFFER-NAME*
                           (:RIGHT-FLUSH " (Extended search characters)"))))
  (SEARCH-MINI-BUFFER-STRING-INTERVAL))

;;; Parse something read back
(DEFUN PARSE-EXTENDED-SEARCH-16B-STRING (STRING &AUX FUNCTION STRINGS EXPR)
  (DECLARE (VALUES FUNCTION KEY))
  (MULTIPLE-VALUE (STRINGS EXPR)
    (PARSE-EXTENDED-SEARCH-STRING STRING))
  (COND ((ATOM STRINGS)
         (SETQ FUNCTION 'ZWEI-SEARCH))
        (EXPR
         (SETQ FUNCTION 'FSM-EXPR-SEARCH
               STRINGS (LIST STRINGS EXPR)))
        (T
         (SETQ FUNCTION 'FSM-SEARCH)))
  (VALUES FUNCTION STRINGS))

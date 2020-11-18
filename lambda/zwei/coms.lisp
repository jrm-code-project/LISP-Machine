;;; Zwei searching and replacing commands -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; see ZWEI;COMA for comments.

;;; Character search

(DEFCONST *STRING-SEARCH-OPTION-DOCUMENTATION*
  "While you are typing the search string, the following characters have special meanings:
C-B     Search forward from the beginning of the buffer.
C-E     Search backwards from the end of the buffer.
C-F     Leave the point at the top of the window, if the window must be recentered.
C-G     Abort the search.
C-D     Get a string to search for from the ring buffer of previously-searched strings.
C-L     Redisplay the typein line.
C-Q     Quotes the next character.
C-R     Reverse the direction of the search.
C-S     Do the search, then come back to the command loop.
C-U     Flush all characters typed so far.
C-V     Delimited Search: Search for occurrences of the string surrounded by delimiters.
C-W     Word Search: Search for words in this sequence regardless of intervening
             punctuation, whitespace, newlines, and other delimiters.
C-Y     Append the string on top of the ring buffer to the search string.
Rubout  Rub out the previous character typed.
Clear-Input  Flush all characters typed so far.
Altmode Do the search and exit.

If you search for the empty string, the default is used.  Otherwise, the
string you type becomes the default, and the default is saved on a ring
buffer unless it is a single character.")

(DEFCOM COM-CHAR-SEARCH DOC-CHAR-SEARCH (KM)
  (CHAR-SEARCH-INTERNAL NIL))

(DEFUN DOC-CHAR-SEARCH (COMMAND CHAR TYPE)
  (DECLARE (IGNORE CHAR))
  (CASE TYPE
    (:NAME (GET COMMAND 'COMMAND-NAME))
    ((:FULL :SHORT)
     (SEND *STANDARD-OUTPUT* :STRING-OUT "Search for a single character.")
     (COND ((EQ TYPE ':FULL)
            (SEND *STANDARD-OUTPUT* :STRING-OUT "
Special characters:
C-A     Do string search (see below).
C-B     Search forward from the beginning of the buffer.
C-E     Search backwards from the end of the buffer.
C-F     Leave the point at the top of the window, if the window must be recentered.
C-R     Search backwards.
C-S     Repeat the last search.

String search, which you get into from C-A, reads in a string and searches for it.
")
            (SEND *STANDARD-OUTPUT* :STRING-OUT *STRING-SEARCH-OPTION-DOCUMENTATION*))))))

(DEFCOM COM-REVERSE-CHAR-SEARCH DOC-REVERSE-CHAR-SEARCH (KM)
   (CHAR-SEARCH-INTERNAL T))

(DEFUN DOC-REVERSE-CHAR-SEARCH (COMMAND CHAR TYPE)
  (DECLARE (IGNORE CHAR))
  (CASE TYPE
    (:NAME (GET COMMAND 'COMMAND-NAME))
    ((:FULL :SHORT)
     (SEND *STANDARD-OUTPUT* :STRING-OUT "Search backward for a single character.")
     (COND ((EQ TYPE ':FULL)
            (SEND *STANDARD-OUTPUT* :STRING-OUT "
Special characters:
C-A     Do Reverse String Search (see below).
C-B     Search forward from the beginning of the buffer.
C-E     Search backwards from the end of the buffer.
C-F     Put the line containing the search object at the top of the screen
C-R     Repeat the last search.
C-S     Repeat the last search.

Reverse String search, which you get into from C-A, reads in a string and searches for it.
")
            (SEND *STANDARD-OUTPUT* :STRING-OUT *STRING-SEARCH-OPTION-DOCUMENTATION*))))))

(DEFUN CHAR-SEARCH-INTERNAL (REVERSEP)
  (UNWIND-PROTECT
    (PROG (XCHAR CHAR UCHAR BJP ZJP TOP-P STRING BP FAILED-P QUOTE-P
           (ORIG-PT (COPY-BP (POINT))) (ARG *NUMERIC-ARG*)
           (FCN 'ZWEI-SEARCH))
        (AND (MINUSP ARG) (SETQ REVERSEP (NOT REVERSEP) ARG (- ARG)))
     LOOP (COND ((OR FAILED-P                   ;Force redisplay on failing search
                     (NULL (SETQ XCHAR (SEND *STANDARD-INPUT* :TYI-NO-HANG))))
                 (TYPEIN-LINE-WITH-REDISPLAY "~:|")
                 (AND BJP (FORMAT *QUERY-IO* "Begin "))
                 (AND ZJP (FORMAT *QUERY-IO* "End "))
                 (AND TOP-P
                      (FORMAT *QUERY-IO* "Top Line "))
                 (AND REVERSEP (FORMAT *QUERY-IO* "Reverse "))
                 (AND QUOTE-P (FORMAT *QUERY-IO* "Quoted-ascii "))
                 (FORMAT *QUERY-IO* "Search: ")))
          (COND ((NOT FAILED-P)
                 (SETQ CHAR (OR XCHAR
                                (TYPEIN-LINE-ACTIVATE
                                  (SEND *STANDARD-INPUT* :TYI))))
                 (SETQ UCHAR (CHAR-UPCASE CHAR))
                 (COND (QUOTE-P
                        (IF ( (CHAR-BITS CHAR))
                            (SETQ CHAR (LOGAND CHAR 37)))       ;ascii control
                        (SETQ STRING CHAR)
                        (SEARCH-RING-PUSH CHAR FCN))
                       ((= UCHAR #/C-A)
                        (RETURN (COM-STRING-SEARCH-INTERNAL REVERSEP BJP ZJP TOP-P)))
                       ((AND (= UCHAR #/C-R) (NOT REVERSEP))
                        (SETQ REVERSEP (NOT REVERSEP))
                        (GO LOOP))
                       ((= UCHAR #/C-B)
                        (SETQ BJP T ZJP NIL REVERSEP NIL)
                        (GO LOOP))
                       ((= UCHAR #/C-E)
                        (SETQ ZJP T BJP NIL REVERSEP T)
                        (GO LOOP))
                       ((= UCHAR #/C-F)
                        (SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
                        (GO LOOP))
                       ((= UCHAR #/C-G)
                        (BEEP)
                        (SEND *QUERY-IO* :MAKE-COMPLETE)
                        (GO QUIT))
                       ((OR (= UCHAR #/C-S)
                            (AND REVERSEP (= UCHAR #/C-R)))
                        (OR *SEARCH-RING* (BARF))
                        (SETQ STRING (CAAR *SEARCH-RING*)
                              FCN (CADAR *SEARCH-RING*)))
                       ((= UCHAR #/C-Q)         ;Funny ascii compatibility
                        (SETQ QUOTE-P T)
                        (GO LOOP))
                       ((> CHAR 220)            ;Random control character
                        (BEEP)
                        (GO LOOP))
                       (T
                        (SETQ STRING CHAR)
                        (SEARCH-RING-PUSH CHAR FCN)))))
          (AND (OR (NULL XCHAR) FAILED-P)
               (IF (NUMBERP STRING)
                   (FORMAT *QUERY-IO* "~C" STRING)
                   (FORMAT *QUERY-IO* "~A" STRING)))
          (SETQ BP (AND (NOT FAILED-P)
                        (DO ((I 0 (1+ I))
                             (BP (COND (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                       (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                       (T (POINT)))
                                 (FUNCALL FCN BP STRING REVERSEP)))
                            ((OR ( I ARG) (NULL BP))
                             BP))))
          (COND (BP
                 (MOVE-BP (POINT) BP))
                ((OR FAILED-P (NULL XCHAR))
                 (FORMAT *QUERY-IO* " Search failed.")
                 (BARF))
                (T
                 (SETQ FAILED-P T)
                 (GO LOOP)))                    ;Failed search typed ahead
    QUIT (MAYBE-PUSH-POINT ORIG-PT)
         (RETURN DIS-BPS))
    (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)))

(DEFCOM COM-STRING-SEARCH
        (LAMBDA (COMMAND CHAR TYPE)
          (DOC-STRING-SEARCH COMMAND TYPE "Search for a specified string.")) (KM)
    (COM-STRING-SEARCH-INTERNAL NIL NIL NIL NIL))

(DEFCOM COM-REVERSE-STRING-SEARCH
        (LAMBDA (COMMAND CHAR TYPE)
          (DOC-STRING-SEARCH COMMAND TYPE "Search backward for a specified string.")) (KM)
    (COM-STRING-SEARCH-INTERNAL T NIL NIL NIL))

(DEFUN DOC-STRING-SEARCH (COMMAND TYPE SHORT-STRING)
  (CASE TYPE
    (:NAME (GET COMMAND 'COMMAND-NAME))
    ((:SHORT :FULL)
     (SEND *STANDARD-OUTPUT* :STRING-OUT SHORT-STRING)
     (WHEN (EQ TYPE ':FULL)
       (SEND *STANDARD-OUTPUT* :FRESH-LINE)
       (SEND *STANDARD-OUTPUT* :STRING-OUT *STRING-SEARCH-OPTION-DOCUMENTATION*)))))

;; A special hack is needed to stop an altmode that follows a Control-S from searching.
;; That is what HACK1 and HACK2 are for.
(DEFUN COM-STRING-SEARCH-INTERNAL (REVERSEP BJP ZJP TOP-P &AUX TEM)
  (UNWIND-PROTECT
      (PROG ((STRING (MAKE-STRING 8. :FILL-POINTER 0))
             (ORIG-PT (COPY-BP (POINT))) (FCN 'ZWEI-SEARCH)
             XCHAR CHAR HACK1 HACK2 ECHOED-P FAILED-P
             REPEATING-STRING-NOT-ECHOED-FLAG
             SEARCHING-DONE-AT-LEAST-ONCE)
         REDIS
            (COND ((NULL (SETQ XCHAR (AND (NOT ECHOED-P)
                                          (SEND *STANDARD-INPUT* :TYI-NO-HANG))))
                   (SETQ ECHOED-P T)            ;Started to echo now
                   (TYPEIN-LINE-WITH-REDISPLAY "~:|")
                   (AND BJP (FORMAT *QUERY-IO* "Begin "))
                   (AND ZJP (FORMAT *QUERY-IO* "End "))
                   (AND TOP-P (FORMAT *QUERY-IO* "Top Line "))
                   (AND REVERSEP (FORMAT *QUERY-IO* "Reverse "))
                   (FORMAT *QUERY-IO* (CASE FCN
                                        (ZWEI-SEARCH "String search: ")
                                        (WORD-SEARCH "Word search: ")
                                        (DELIMITED-SEARCH "Delimited search: ")))
                   (FORMAT *QUERY-IO* "~A" STRING)))
            (AND FAILED-P (GO FAILED))
            (GO LOP1)
         LOOP
            (SETQ XCHAR (AND (NOT ECHOED-P)
                             (SEND *STANDARD-INPUT* :TYI-NO-HANG)))
         LOP1
            (SETQ CHAR (OR XCHAR (TYPEIN-LINE-ACTIVATE (TYI-WITH-SCROLLING-AND-MOUSING))))
            (SETQ HACK2 HACK1 HACK1 NIL)
            (COND ((CHAR-BIT CHAR :CONTROL)
                   (SETQ CHAR (CHAR-UPCASE (CHAR-CODE CHAR)))
                   (SELECT CHAR
                     (#/B (SETQ BJP T ZJP NIL REVERSEP NIL)
                          (GO REDIS))
                     (#/E (SETQ BJP NIL ZJP T REVERSEP T)
                          (GO REDIS))
                     (#/F (SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
                          (GO REDIS))
                     (#/G (SEND *QUERY-IO* :MAKE-COMPLETE)
                          (WHEN SEARCHING-DONE-AT-LEAST-ONCE
                            (SEARCH-RING-PUSH STRING FCN))
                          (BARF))
                     (#/D (MULTIPLE-VALUE (TEM FCN)
                            (SEARCH-RING-POP))
                          (COND ((NUMBERP TEM)
                                 (SETQ STRING (MAKE-STRING 8. :FILL-POINTER 1))
                                 (SETF (AREF STRING 0) TEM))
                                (T (SETQ STRING TEM)))
                          (GO REDIS))
                     (#/L (GO REDIS))
                     (#/M (IF (NOT (WINDOW-MARK-P *WINDOW*))
                              (BEEP)
                            (REGION (BP1 BP2)
                              (APPEND-TO-ARRAY STRING (STRING-INTERVAL BP1 BP2 T)))
                            (SETF (WINDOW-MARK-P *WINDOW*) NIL)
                            (MUST-REDISPLAY *WINDOW* DIS-MARK-GOES)
                            (REDISPLAY *WINDOW* :NONE))
                          (GO REDIS))
                     (#/Q (TYPEIN-LINE-ACTIVATE
                            (SETQ CHAR (SEND *STANDARD-INPUT* :TYI)))
                          (SETQ CHAR (LOGAND (IF (CHAR-BIT CHAR :CONTROL)
                                                 #o37 #o377)
                                             CHAR))
                          (GO NORMAL))
                     (#/R (SETQ REVERSEP (NOT REVERSEP))
                          (GO REDIS))
                     (#/S (AND (EQUAL "" STRING)
                               *SEARCH-RING*
                               (SETQ STRING (CAAR *SEARCH-RING*)
                                     REPEATING-STRING-NOT-ECHOED-FLAG T
                                     FCN (CADAR *SEARCH-RING*)))
                          (LET ((TEM (FUNCALL FCN
                                              (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                                    (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                                    (T (POINT)))
                                              STRING
                                              REVERSEP)))
                            (COND ((NULL TEM)
                                   ;; Next line commented out for Emacs compatibility
                                                ;(BEEP)
                                   ;; Comment this BARF instead to stay in search if fail
                                   ;; But don't forget to update search default ring
                                   (OR (EQUAL "" STRING)
                                       (SEARCH-RING-PUSH STRING FCN))
                                   (GO FAILED)
                                   )
                                  (T (MOVE-BP (POINT) TEM)
                                     (MUST-REDISPLAY *WINDOW* DIS-BPS)
                                     (AND (WINDOW-READY-P *WINDOW*)     ;Minibuffer
                                          (REDISPLAY *WINDOW* :POINT))
                                     (SETQ BJP NIL ZJP NIL)
                                     (AND TOP-P
                                          (SETQ *CENTERING-FRACTION* 0.0s0))
                                     (SETQ SEARCHING-DONE-AT-LEAST-ONCE T)
                                     (SETQ HACK1 T))))
                          (IF (NULL XCHAR)
                              (GO LOOP)
                            (SETQ ECHOED-P T)
                            (GO REDIS)))
                     (#/U (SETF (FILL-POINTER STRING) 0)
                          (GO REDIS))
                     (#/V (SETQ FCN 'DELIMITED-SEARCH)
                          (GO REDIS))
                     (#/W (SETQ FCN 'WORD-SEARCH)
                          (GO REDIS))
                     (#/Y (SETQ TEM (CAAR *SEARCH-RING*))
                          (COND ((NULL TEM)
                                 (BEEP))
                                ((NUMBERP TEM)
                                 (VECTOR-PUSH-EXTEND TEM STRING))
                                (T
                                 (APPEND-TO-ARRAY STRING TEM)))
                          (GO REDIS))
                     (OTHERWISE (BEEP)
                                (GO REDIS))))
                  ((= CHAR #/RUBOUT)
                   (OR (ZEROP (FILL-POINTER STRING))
                       (VECTOR-POP STRING))
                   (GO REDIS))
                  ((= CHAR #/HELP)
                   (DOC-STRING-SEARCH *CURRENT-COMMAND* :FULL
                                      "Search for a specified string.")
                   (SEND *STANDARD-INPUT* :UNTYI (SEND *STANDARD-INPUT* :ANY-TYI))
                   (GO REDIS))
                  ((= CHAR #/CLEAR-INPUT)
                   (SETF (FILL-POINTER STRING) 0)
                   (GO REDIS))
                  ((OR (= CHAR #/) (= CHAR #/END))
                   (OR XCHAR
                       (FORMAT *QUERY-IO* "~C" CHAR))
                   (OR (EQUAL "" STRING)
                       (SEARCH-RING-PUSH STRING FCN))
                   (OR HACK2
                       (DO ((ARG (ABS *NUMERIC-ARG*) (1- ARG))
                            (KEY (COND ((AND (EQUAL "" STRING)
                                             *SEARCH-RING*)
                                        (SETQ FCN (CADAR *SEARCH-RING*))
                                        (CAAR *SEARCH-RING*))
                                       (T STRING)))
                            (BP (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                      (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                      (T (POINT)))))
                           (( ARG 0) (MOVE-BP (POINT) BP))
                         (OR (SETQ BP (FUNCALL FCN BP KEY REVERSEP))
                             (GO FAILED))))
                   (MAYBE-PUSH-POINT ORIG-PT)
                   (RETURN DIS-BPS)))
            (SETQ CHAR (LOGAND #o377 CHAR))
         NORMAL
            (VECTOR-PUSH-EXTEND CHAR STRING)
            (IF XCHAR
                (GO REDIS)
              (SETQ ECHOED-P T)                 ;Started to echo
              (FORMAT *QUERY-IO* "~C" CHAR)
              (GO LOOP))
         FAILED
            (COND (XCHAR                        ;Typed ahead failing search, force redisplay
                   (SETQ FAILED-P T ECHOED-P T)
                   (GO REDIS)))
            (WHEN REPEATING-STRING-NOT-ECHOED-FLAG
              (FORMAT *QUERY-IO* "~A" STRING))
            (WHEN FAILED-P                      ;Typed ahead last time
              (FORMAT *QUERY-IO* ""))
            (FORMAT *QUERY-IO* " Search failed.")
            (BARF))
    (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)))

;;; Incremental search.


(DEFCOM COM-INCREMENTAL-SEARCH "Search for character string.
As characters are typed in the accumulated string is displayed and searched for.
You can use Rubout to cancel characters characters.
Altmode exits the search (but if search string is empty, it invokes String Search).
Use Control-Q to quote, Control-S to repeat the search with the same string,
Control-R to search backwards, Control-W adds the word at the cursor to the string.
If Control-S or Control-R is the first character typed, the previous search string
is used again." (KM)
   (INCREMENTAL-SEARCH (< *NUMERIC-ARG* 0)))

(DEFCOM COM-REVERSE-INCREMENTAL-SEARCH "Reverse search for character string.
As characters are typed in the accumulated string is displayed and searched for.
You can use Rubout to cancel characters characters.
Altmode exits the search (but if search string is empty, it invokes String Search).
Use Control-Q to quote, Control-R to repeat the search with the same string,
Control-S to search forwards, Control-W adds the word at the cursor to the string.
If Control-S or Control-R is the first character typed, the previous search string
is used again." (KM)
   (INCREMENTAL-SEARCH (> *NUMERIC-ARG* 0)))

(defresource isearch-vectors ()
  :constructor (vector
                 ;; *is-string*
                 (make-string 128. :fill-pointer 0)
                 ;; All of the arrays below constitute a push-down stack.
                 ;; *is-bp*
                 (make-array 128. :fill-pointer 0)
                 ;; *is-status*
                 ;; STATUS is NIL for a failing search, T for a successful one,
                 ;; and :GO for one that is still looking.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-reverse-p*
                 ;; T if the search is reverse at this level.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-pointer*
                 ;; This points to the end of the part of *IS-STRING* active at this level.
                 (make-array 128. :fill-pointer 0)
                 ;; *is-operation*
                 ;; This is what sort of thing the char at this level is:
                 ;; :NORMAL, :REVERSE or :REPEAT.
                 (make-array 128. :fill-pointer 0))
  :initializer (dotimes (x (length object))
                 (setf (fill-pointer (svref object x)) 0)))

(DEFVAR *IS-STRING*)
(DEFVAR *IS-BP*)
(DEFVAR *IS-STATUS*)
(DEFVAR *IS-REVERSE-P*)
(DEFVAR *IS-POINTER*)
(DEFVAR *IS-OPERATION*)

(DEFUN INITIALIZE-INCREMENTAL-SEARCH-GLOBALS ()
  (USING-RESOURCE (TEM ISEARCH-VECTORS)
    TEM
    NIL))

(DEFMACRO PUSH-ISEARCH-STATUS ()
  '(PUSH-ISEARCH-STATUS-1 (SETQ P (1+ P))))

(DEFUN PUSH-ISEARCH-STATUS-1 (P)
  (WHEN (= P (ARRAY-LENGTH *IS-REVERSE-P*))
    (ADJUST-ARRAY-SIZE *IS-REVERSE-P* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-STATUS* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-OPERATION* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-BP* (+ P 64.))
    (ADJUST-ARRAY-SIZE *IS-POINTER* (+ P 64.)))
  (SETF (AREF *IS-REVERSE-P* P) (AREF *IS-REVERSE-P* (1- P)))
  (SETF (AREF *IS-POINTER* P) (AREF *IS-POINTER* (1- P)))
  (SETF (AREF *IS-STATUS* P) ':GO))

;;; This is how incremental search manages to allow both type-ahead and rubout-ahead:
;;; What to do is kept in five stacks, arrays in the *IS-...* variables.
;;; Input of normal characters pushes onto the end using index P,
;;; and rubout pops off at the same index.  *IS-REVERSE-P* remembers the
;;; search direction at each level, *IS-OPERATION* remembers the type of search
;;; (:NORMAL for a normal character, :REVERSE for a Control-R or Control-S that reverses,
;;; or :REPEAT for a Control-R or Control-S that repeats), *IS-POINTER* is the length of
;;; the search string at that level.

;;; In parallel, with lower priority, the entries thus pushed are processed
;;; by searching according to them.  P1 is the index of the entry or "level"
;;; which is currently being worked on.  P1 advances only when the level is
;;; determined to be successful or failing.  Advancing involves examining the three
;;; *IS-...* entries of the next level to see what to do.  If P1 meets P, then there is no
;;; work to do for the moment.  The state of this process is kept in *IS-STATUS*
;;; and *IS-BP*.  *IS-BP* is the bp of the place found at a given level or the
;;; place at which searching is going on.  *IS-STATUS* is T for a successful search,
;;; NIL for a failing one, and :GO if it isn't known yet.  New levels are pushed
;;; (via P) in the :GO state.

;;; Rubbing out decrements P1 if necessary to keep it no greater than P.
;;; The searching process is not confused because it keeps all its state
;;; in *IS-STATUS* and *IS-BP* and all that is needed is to change P1.

;;; Updating the echo area is under input in priority, but above actual searching.
;;; Thus, as soon as there is no type-ahead everything will be correct.
;;; This is because the echo area is presumed to be fast to update.
;;; Buffer redisplay is lower than searching, of course.


; character lossage rampant
(DEFUN INCREMENTAL-SEARCH (REVERSE-P &AUX (ORIG-PT (COPY-BP (POINT)))
                                          ISEARCH-VECTORS)
  (SELECT-WINDOW *WINDOW*)
  (SEND *QUERY-IO* :FRESH-LINE)         ;Necessary if in the mini-buffer
  (UNWIND-PROTECT
      (TYPEIN-LINE-ACTIVATE
        (PROG* (CHAR                    ; The current command.
                XCHAR                   ; Upcase version of character
                MUST-REDIS              ; T => The echo-area must be completely redisplayed.
                (P 0)                   ; The stack pointer into *IS-BP*, etc. for input and rubout
                (P1 0)                  ; The pointer for which search we are doing.
                                        ; Can never exceed P.
                SUPPRESSED-REDISPLAY    ; T if the last input char was read before
                                        ; redisplay had a chance to finish.
                                        ;  A Control-G read that way acts like
                                        ;  a failing search quit.
                BP1                     ; Aux BP used for actual searching.
                NEW-BP
                TIME-OUT                ; Set by SEARCH when it times out so we can check input.
                INPUT-DONE              ; An altmode or control char has been seen.
                                        ; Do not look for input any more; just search, then exit.
                (*is-string*)
                (*is-bp*)
                (*is-status*)
                (*is-reverse-p*)
                (*is-pointer*)
                (*is-operation*)
;               (TV:KBD-INTERCEPTED-CHARACTERS
;                 (COPY-LIST TV:KBD-INTERCEPTED-CHARACTERS))
                )
;              (SETQ TV:KBD-INTERCEPTED-CHARACTERS
;                    (DELQ (ASSQ #/ABORT TV:KBD-INTERCEPTED-CHARACTERS)
;                          TV:KBD-INTERCEPTED-CHARACTERS))
               (SETQ ISEARCH-VECTORS (ALLOCATE-RESOURCE 'ISEARCH-VECTORS)
                     *IS-STRING* (SVREF ISEARCH-VECTORS 0)
                     *IS-BP* (SVREF ISEARCH-VECTORS 1)
                     *IS-STATUS* (SVREF ISEARCH-VECTORS 2)
                     *IS-REVERSE-P* (SVREF ISEARCH-VECTORS 3)
                     *IS-POINTER* (SVREF ISEARCH-VECTORS 4)
                     *IS-OPERATION* (SVREF ISEARCH-VECTORS 5))
               (SETF (AREF *IS-STATUS* 0) T)    ; Initialize the stacks.
               (SETF (AREF *IS-REVERSE-P* 0) REVERSE-P)
               (SETF (AREF *IS-OPERATION* 0) ':NORMAL)
               (SETF (AREF *IS-POINTER* 0) 0)
               (SETF (AREF *IS-BP* 0) (COPY-BP (POINT)))
               (SETQ MUST-REDIS T)      ; Initially we must redisplay.
               (GO CHECK-FOR-INPUT)
               ;; Come here if there is input, or nothing to do until there is input.
            INPUT
               (SETQ SUPPRESSED-REDISPLAY NIL)
               (AND (WINDOW-READY-P *WINDOW*)   ;In case of minibuffer
                    (REDISPLAY *WINDOW* :POINT))        ; Redisplay point position while waiting.
               (IF (= (WINDOW-REDISPLAY-DEGREE *WINDOW*) DIS-NONE)
                   (REDISPLAY-MODE-LINE)        ;Update indication of more above or below.
                 (SETQ SUPPRESSED-REDISPLAY T))
               (IF SUPPRESSED-REDISPLAY
                   (SETQ CHAR (TYI-WITH-SCROLLING NIL T))
                 ;; If must wait for input, make the window's blinker blink
                 ;; even though not selected.
                 (IF (OPERATION-HANDLED-P *WINDOW* :POINT-BLINKER)
                     (UNWIND-PROTECT
                         (PROGN
                           (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY :BLINK)
                           (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :BLINK)
                           (SETQ CHAR (TYI-WITH-SCROLLING NIL T)))
                       (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-VISIBILITY
 ;tv lossage
                             (IF (EQ *WINDOW* TV:SELECTED-WINDOW)
                                 :BLINK
                               (TV:SHEET-EXPOSED-P *WINDOW*)))
                       (SEND (WINDOW-POINT-BLINKER *WINDOW*) :SET-DESELECTED-VISIBILITY :ON))
                   (SETQ CHAR (TYI-WITH-SCROLLING NIL T))))
               (COND ((CONSP CHAR)
                      (SEND *STANDARD-INPUT* :UNTYI CHAR)
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT))
; character lossage
                     ((FIXNUMP CHAR)
                      (SETQ CHAR (INT-CHAR CHAR))))
               (SETQ XCHAR (CHAR-UPCASE CHAR))
               (COND ((NOT (OR ( (CHAR-BITS CHAR) 0) (TV:CHAR-MOUSE-P CHAR)
                               (MEMQ CHAR
                                     '(#/ #/END #/RUBOUT #/HELP #/ABORT #/CLEAR-INPUT))))
                      (GO NORMAL))
                     ((MEMQ XCHAR '(#/C-S #/C-R))
                      (PUSH-ISEARCH-STATUS)
                      (SETF (AREF *IS-OPERATION* P) ':REPEAT)
                      (LET ((NEW-REVERSE-P (EQL XCHAR #/C-R)))
                        (COND   ;; In reverse mode, just go to forward.
                          ((NEQ (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
                           (SETF (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
                           (SETQ MUST-REDIS T)
                           (SETF (AREF *IS-OPERATION* P) ':REVERSE))
                          ((ZEROP (AREF *IS-POINTER* P))
                           (LET ((STRING (STRING (OR (CAAR *SEARCH-RING*) (BARF)))))
                             (COPY-ARRAY-CONTENTS STRING *IS-STRING*)
                             (SETF (AREF *IS-POINTER* P) (LENGTH STRING)))
                           (SETQ MUST-REDIS T))))
                      (GO CHECK-FOR-INPUT))
                     ((eql char #/C-W)         ;snarf word at cursor - smh 24jun88
                      (let* ((saved-point (point))
                             (pointer (if (aref *is-reverse-p* p)
                                          (forward-char saved-point (aref *is-pointer* p))
                                        (copy-bp saved-point)))
                             (other-end (or (forward-word pointer 1)
                                            (progn (beep)
                                                   ;; at end of file, just flash and wait for more
                                                   (go check-for-input)))))
                        (PUSH-ISEARCH-STATUS)
                        (loop while (bp-< pointer other-end)
                              as char = (int-char (bp-char pointer))
                              do
                              (OR MUST-REDIS (FORMAT *QUERY-IO* "~C" CHAR))
                              (LET ((IDX (AREF *IS-POINTER* P)))
                                (AND ( IDX (ARRAY-LENGTH *IS-STRING*))
                                     (ADJUST-ARRAY-SIZE *IS-STRING* (+ IDX 64.)))
                                (SETF (CHAR *IS-STRING* IDX) CHAR)
                                (SETF (AREF *IS-POINTER* P) (1+ IDX)))
                              (SETF (AREF *IS-OPERATION* P) ':NORMAL)
                              (setq pointer (forward-char pointer 1)))
                        (move-bp (point) saved-point)
                        (go check-for-input)))
                     ((EQL XCHAR #/C-Q)
                      (LET ((NEW-CH (READ-CHAR *STANDARD-INPUT*)))
                        (SETQ CHAR (IF (CHAR-BIT NEW-CH :CONTROL)
                                       ;; ascii version of control characters
                                       (INT-CHAR (LOGAND #o37 (CHAR-CODE NEW-CH)))
                                     (INT-CHAR (CHAR-CODE NEW-CH)))))
                      (GO NORMAL))
                     ((EQL CHAR #/HELP)
                      (PRINT-DOC :FULL *CURRENT-COMMAND*)
                      (SEND *STANDARD-INPUT* :UNTYI (SEND *STANDARD-INPUT* :ANY-TYI))
                      (GO INPUT))
                     ((OR (EQL XCHAR #/C-G) (EQL XCHAR #/ABORT))
                      (BEEP)
                      (COND ((AND (OR SUPPRESSED-REDISPLAY (NEQ (AREF *IS-STATUS* P) T))
                                  (PLUSP P))
                             ;; Control-G in other than a successful search
                             ;; rubs out until it becomes successful.
                             (SETQ P (DO ((P (1- P) (1- P)))
                                         ((EQ (AREF *IS-STATUS* P) T) P)))
                             (SETQ P1 (MIN P P1) MUST-REDIS T)
                             (GO CHECK-FOR-INPUT))
                            (T
                             (MOVE-BP (POINT) (AREF *IS-BP* 0))
                             (RETURN))))
                     ((OR (EQL CHAR #/) (EQL CHAR #/END))
                      (AND (ZEROP P)
                           ;; Call string search, and make self-doc print the right thing there.
                           (LET ((*CURRENT-COMMAND* 'COM-STRING-SEARCH-INTERNAL))
                             (RETURN (COM-STRING-SEARCH-INTERNAL REVERSE-P NIL NIL NIL))))
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT))
                     ((OR (EQL CHAR #/RUBOUT) (EQL CHAR #/CLEAR-INPUT))
                      ;; Clear-input rubs out all the way.  Set P to 1 and let it be decremented.
                      (IF (EQL CHAR #/CLEAR-INPUT) (SETQ P 1))
                      (COND (( P 0)    ; If he over-rubbed out,
                             (BEEP)     ;   that is an error.
                             (GO CHECK-FOR-INPUT))
                            (T
                             ;; Rubout pops all of these PDLs.
                             (SETQ P (1- P))
                             (SETQ P1 (MIN P P1))
                             (SETQ MUST-REDIS T)
                             (GO CHECK-FOR-INPUT))))
                     (T
;character lossage
                      (SEND *STANDARD-INPUT* :UNTYI (CHAR-INT CHAR))
                      (SETQ INPUT-DONE T)
                      (GO CHECK-FOR-INPUT)))
               (FERROR "A clause fell through.")

               ;; Normal chars to be searched for come here.
            NORMAL
               (OR MUST-REDIS (FORMAT *QUERY-IO* "~C" CHAR))
               (PUSH-ISEARCH-STATUS)
               (LET ((IDX (AREF *IS-POINTER* P)))
                 (AND ( IDX (ARRAY-LENGTH *IS-STRING*))
                      (ADJUST-ARRAY-SIZE *IS-STRING* (+ IDX 64.)))
                 (SETF (CHAR *IS-STRING* IDX) CHAR)
                 (SETF (AREF *IS-POINTER* P) (1+ IDX)))
               (SETF (AREF *IS-OPERATION* P) ':NORMAL)
               ;; Come here after possibly processing input to update the search tables
               ;; to search for a while.  First, if necessary and not suppressed
               ;; update the search string displayed in the echo area.
            CHECK-FOR-INPUT
               ;; If there is input available, go read it.
               ;; Otherwise, do work if there is work to be done.
               (AND (NOT INPUT-DONE)
                    (SEND *STANDARD-INPUT* :LISTEN)
                    (GO INPUT))
               ;; Now do some work for a while, then go back to CHECK-FOR-INPUT.
               (WHEN MUST-REDIS
                 (SETQ MUST-REDIS NIL)
                 (FORMAT *QUERY-IO* "~&~:|")
                 (OR (AREF *IS-STATUS* P1) (FORMAT *QUERY-IO* "Failing "))
                 (AND (AREF *IS-REVERSE-P* P) (FORMAT *QUERY-IO* "Reverse "))
                 (FORMAT *QUERY-IO* "I-Search: ")
                 (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P))
                 (FORMAT *QUERY-IO* "~A" *IS-STRING*))
               ;; Now see what sort of state the actual search is in, and what work there is to do.
               ;; P1 points at the level of the table on which we are actually working.
               (SETF BP1 (AREF *IS-BP* P1))
               ;; Display point at the end of the last search level which has succeeded.
               (DO ((P0 P1 (1- P0)))
                   ((EQ (AREF *IS-STATUS* P0) T)
                    (MOVE-BP (POINT) (AREF *IS-BP* P0))))
               (MUST-REDISPLAY *WINDOW* DIS-BPS)
               (COND ((EQ (AREF *IS-STATUS* P1) ':GO)
                      (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P1))
                      ;; If the level we were working on is still not finished,
                      ;; search at most 64. more lines.  If we find it or the end of the buffer
                      ;; before then, this level is determined and we can work on the next.
                      ;; Otherwise, we remain in the :GO state and do 64. more lines next time.
                      (MULTIPLE-VALUE (NEW-BP TIME-OUT)
                        (ZWEI-SEARCH BP1 *IS-STRING*
                                     (AREF *IS-REVERSE-P* P1) NIL 64.))
                      ;; What happened?
                      (COND (TIME-OUT
                             ;; Nothing determined.  NEW-BP has where we stopped.
                             (MOVE-BP BP1 NEW-BP)
                             (DBP BP1)) ;Avoids missing occurrences, if string starts with CR
                            ((NULL NEW-BP)
                             ;; This search was determined to be a failure.
                             (OR (SEND-IF-HANDLES *STANDARD-INPUT* :MACRO-ERROR)
                                 (BEEP))
                             (SETF (AREF *IS-STATUS* P1) NIL)
                             (MOVE-BP BP1 (AREF *IS-BP* (1- P1)))
                             (MOVE-BP (POINT) BP1)
                             (SETQ MUST-REDIS T))
                            (T ;; This search level has succeeded.
                             (SETF (AREF *IS-STATUS* P1) T)
                             (MOVE-BP (POINT) NEW-BP)
                             (MOVE-BP BP1 NEW-BP))))
                     (( P P1)
                      ;; This level is finished, but there are more pending levels typed ahead.
                      (INCF P1)
                      (SETF (AREF *IS-BP* P1) (SETQ BP1 (COPY-BP BP1)))
                      (SETF (FILL-POINTER *IS-STRING*) (AREF *IS-POINTER* P1))
                      (COND ((NULL (AREF *IS-STATUS* (1- P1)))
                             (COND ((NEQ (AREF *IS-OPERATION* P1) ':REVERSE)
                                    ;; A failing search remains so unless we reverse direction.
                                    (SETF (AREF *IS-STATUS* P1) NIL))
                                   (T ;; If we reverse direction, change prompt line.
                                    (SETQ MUST-REDIS T))))
                            ((EQ (AREF *IS-OPERATION* P1) ':NORMAL)
                             ;; Normal char to be searched for comes next.
                             ;; We must adjust the bp at which we start to search
                             ;; so as to allow the user to extend the string already found.
                             (MOVE-BP BP1
                                      (FORWARD-CHAR BP1
                                                    (IF (AREF *IS-REVERSE-P* P1)
                                                        (IF (= (LENGTH *IS-STRING*) 1)
                                                            0
                                                          (LENGTH *IS-STRING*))
                                                      (- 1 (LENGTH *IS-STRING*)))
                                                    T)))))
                     ;; If there is nothing left to do, and terminator seen, exit.
                     (INPUT-DONE
                      (SEARCH-RING-PUSH
                        ;; Entries on the search ring should have a leader
                        (STRING-NCONC (MAKE-STRING (LENGTH *IS-STRING*)
                                                   :FILL-POINTER 0)
                                      *IS-STRING*)
                        'ZWEI-SEARCH)
                      (FORMAT *QUERY-IO* "")
                      (MAYBE-PUSH-POINT ORIG-PT)
                      ;(SELECT-WINDOW *WINDOW*)
                      (RETURN))
                     ;; Nothing to do and no terminator, wait for input.
                     (T (GO INPUT)))
               (GO CHECK-FOR-INPUT)
               )
         (SETQ ORIG-PT NIL))
    ;; unwind-protect cleanup
    (IF ISEARCH-VECTORS (DEALLOCATE-RESOURCE 'ISEARCH-VECTORS ISEARCH-VECTORS))
    (IF ORIG-PT (MOVE-BP (POINT) ORIG-PT))
    (SEND-IF-HANDLES *QUERY-IO* :MAKE-COMPLETE)
    (MUST-REDISPLAY *WINDOW* DIS-BPS)
    (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW))
  DIS-BPS)


;; Subroutine used by the WITH-REGION-OR-WHOLE-INTERVAL
(DEFUN REGION-OR-WHOLE-INTERVAL ()
  (DECLARE (VALUES *INTERVAL* REGION-P))
  (IF (NOT (WINDOW-MARK-P *WINDOW*))
      *INTERVAL*
    (LET ((POINT (POINT)) (MARK (MARK)))
      (AND (BP-< MARK POINT) (SWAP-BPS POINT MARK))
      (SETF (WINDOW-MARK-P *WINDOW*) NIL)
      (MUST-REDISPLAY *WINDOW* DIS-MARK-GOES)
      (VALUES (CREATE-INTERVAL (COPY-BP POINT :NORMAL) (COPY-BP MARK :MOVES))
              T))))

(DEFCOM COM-REPLACE-STRING "Replace all occurrences of a given string with another.
Prompts for two string: to replace all FOO's with BAR's, type FOO and BAR.
With no numeric arg, all occurrences after point are replaced.
With numeric arg, that many occurrences are replaced.
If *CASE-REPLACE* is non-null, BAR's initial will be capitalized
if FOO's initial had been (supply it in lower case)." ()
  (LET ((FROM (TYPEIN-LINE-READLINE
                "Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of:"
                *NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)))))
    (IF (ZEROP (STRING-LENGTH FROM))
        (BARF "The string may not be null.")
      (LET ((TO (LET ((*MINI-BUFFER-DEFAULT-STRING* FROM))
                  (TYPEIN-LINE-READLINE
                    "Replace ~:[all~*~;next ~D~] occurrences ~:[in the region ~]of /"~A/" with:"
                    *NUMERIC-ARG-P* *NUMERIC-ARG* (NOT (WINDOW-MARK-P *WINDOW*)) FROM))))
        (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
          (FORMAT *QUERY-IO* "~&~D. replacement~:P made."
                  (REPLACE-STRING (POINT) FROM TO (AND *NUMERIC-ARG-P*
                                                       *NUMERIC-ARG*))))))
    DIS-TEXT))

(DEFUN KIND-OF-QUERY-REPLACE-DOCUMENTATION (COMMAND CHAR OP)
  (CASE OP
    (:NAME (GET COMMAND 'COMMAND-NAME))
    (:SHORT (PRINT-DOC OP COMMAND CHAR T))
    (:FULL
     (FORMAT T "~A~&" (GET COMMAND 'DOCUMENTATION))
     (IF (GET COMMAND 'CONTROL-PERIOD)
         (FORMAT T "If you abort, you can resume using the command Control-Period.~%"))
     (WHEN (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
             (Y-OR-N-P "Do you want to see the documentation of Query Replace? "))
       (TERPRI)
       (PRINT-DOC :FULL 'COM-QUERY-REPLACE)))))

(DEFCOM COM-QUERY-REPLACE "Replace string, asking about each occurrence.
Prompts for each string.  If you first give it FOO, then BAR, it finds the first FOO,
 displays, and reads a character.
Space => replace it with BAR and show next FOO.
Rubout => don't replace, but show next FOO.
Comma => replace this FOO and show result, waiting for a
 Space, Control-R or Altmode.
Period => replace this FOO and exit.  Altmode => just exit.
^ => return to site of previous FOO (actually, pop the point pdl).
Control-W => kill this FOO and enter recursive edit.
Control-R => enter editing mode recursively.  Control-L => Redisplay.
Exclamation mark => replace all remaining FOOs without asking.
Any other character exits and (except Altmode) is read again.
If *CASE-REPLACE* is non-null, BAR's initial will be capitalized if FOO's initial had been.
If you give a numeric argument, it will not consider FOOs that are not
 bounded on both sides by delimiter characters." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (LET ((START-BP (POINT))
          (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
        (UNLESS (BP-< (POINT) (MARK))
          (SWAP-BPS (POINT) (MARK)))
        (SETQ END-BP (MARK)))
      (QUERY-REPLACE START-BP END-BP FROM TO *NUMERIC-ARG-P*)))
  DIS-TEXT)

(DEFPROP COM-ATOM-QUERY-REPLACE KIND-OF-QUERY-REPLACE-DOCUMENTATION DOCUMENTATION-FUNCTION)
(DEFCOM COM-ATOM-QUERY-REPLACE "Query replace, replacing only delimited atoms." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((*NUMERIC-ARG-P* T))
      (COM-QUERY-REPLACE))))

(DEFUN QUERY-REPLACE-STRINGS (REGION-P &OPTIONAL (TYPE "replace") RETURN-EMPTY
                              &AUX FROM TO)
  (SETQ FROM (TYPEIN-LINE-READLINE
               "Query-~A some ~:[delimited ~]occurrences ~:[in the region ~]of:"
               TYPE (NOT *NUMERIC-ARG-P*) (NOT REGION-P)))
  (COND ((NOT (ZEROP (STRING-LENGTH FROM)))
         (LET ((*MINI-BUFFER-DEFAULT-STRING* FROM))
           (SETQ TO (TYPEIN-LINE-READLINE
                      "Query-~A some ~:[delimited ~]occurrences ~:[in the region ~]of /"~A/" with:"
                      TYPE (NOT *NUMERIC-ARG-P*) (NOT REGION-P) FROM)))
         (VALUES FROM TO))
        ((NOT RETURN-EMPTY)
         (BARF "The string may not be null."))
        (T NIL)))

(DEFVAR *QUERY-FROM*)                           ;These are for the mode line
(DEFVAR *QUERY-TO*)

;;; This is the normal form of query replace
(DEFUN QUERY-REPLACE (START-BP END-BP *QUERY-FROM* *QUERY-TO* &OPTIONAL BREAKS
                      &AUX (*CASE-REPLACE* *CASE-REPLACE*))
  "Query replace *QUERY-FROM* with *QUERY-TO* from START-BP to END-BP.
If *QUERY-FROM* is not all lower case, don't try to preserve case.
BREAKS non-NIL means only consider replacing occurrences surrounded by delimiters."
  ;;If from isn't all lowercase, user probably has something specific in mind
  (AND (DO ((I 0 (1+ I))
            (LEN (STRING-LENGTH *QUERY-FROM*)))
           (( I LEN))
         (AND (UPPER-CASE-P (CHAR *QUERY-FROM* I))
              (RETURN T)))
       (SETQ *CASE-REPLACE* NIL))
  (QUERY-REPLACE-INTERNAL START-BP END-BP *QUERY-FROM* *QUERY-TO* 'QUERY-REPLACE-SEARCH BREAKS))

(DEFUN QUERY-REPLACE-SEARCH (BP TO-BP QUERY-FROM IGNORE &AUX BP1)
  (AND (SETQ BP1 (ZWEI-SEARCH BP QUERY-FROM NIL NIL NIL TO-BP))
       (VALUES BP1 (FORWARD-CHAR BP1 (- (STRING-LENGTH QUERY-FROM))))))

;;; General query replace.  Note: BP itself is moved around.  It is usually POINT.
;;; The caller must bind *QUERY-FROM* and *QUERY-TO* to strings for the mode line to use.
;;; BREAKS means only consider things surrounded by delimiters.
;;; FUNCTION is called on with BP and QUERY-FROM and QUERY-to, it should return two bps to
;;; the area of the thing found or NIL.
;;; FLAG-1 and FLAG-2 implement the hairy COMMA command.
(DEFUN QUERY-REPLACE-INTERNAL (BP END-BP QUERY-FROM QUERY-TO FUNCTION BREAKS
                               &AUX BP1 BP2 START-BP DO-THE-REST CHAR UCHAR FLAG-1 FLAG-2)
  (BIND-MODE-LINE '("Query Replacing " *QUERY-FROM* "  " *QUERY-TO*)
    (SETQ START-BP (COPY-BP BP)
          BP1 (COPY-BP BP)
          BP2 (COPY-BP BP))
    (MACROLET ((QREP ()
                `(COND ((NOT FLAG-2)
                        (WITH-UNDO-SAVE ("Replace" BP1 BP2 T)
                          (MOVE-BP BP2 (CASE-REPLACE BP1 BP2 *QUERY-TO*))
                          (MOVE-BP BP BP2))
                        (MUST-REDISPLAY *WINDOW* DIS-TEXT)))))
      (LOOP
        (SETQ FLAG-2 FLAG-1 FLAG-1 NIL)
        (UNLESS FLAG-2
          (MULTIPLE-VALUE (BP2 BP1) (FUNCALL FUNCTION BP2 END-BP QUERY-FROM QUERY-TO))
          (UNLESS BP2
            (FORMAT *QUERY-IO* "~&No more occurences.")
            (RETURN NIL)))
        (WHEN (OR FLAG-2
                  (NOT BREAKS)                  ; If we don't care about breaks, go ahead.
                  (AND                          ; Both beginning and end must be breaks.
                    (OR (BP-= BP2 END-BP)       ; EOB counts as a break.
                        (= (WORD-SYNTAX (BP-CHAR BP2)) WORD-DELIMITER))
                    (OR (BP-= BP1 START-BP)
                        (= (WORD-SYNTAX (BP-CHAR-BEFORE BP1)) WORD-DELIMITER))))
          ;; Move point after checking delimiters
          (UNLESS FLAG-2
            (MOVE-BP BP BP2)
            (MUST-REDISPLAY *WINDOW* DIS-BPS))
          ;; We want to offer this string for replacement.
          (IF DO-THE-REST (QREP)
            (REDISPLAY *WINDOW* :POINT)
            (REDISPLAY-MODE-LINE)
            (POINT-PDL-PUSH BP *WINDOW*)
            (TAGBODY ; do NOT change this to a PROG; RETURN is used for exiting main loop.
             GETCHAR ; ``Special'' requests transfer control here
                (SETQ CHAR (SEND *STANDARD-INPUT* ':TYI))
                (OR (NUMBERP CHAR) (GO GETCHAR))        ;Ignore special request
                (SETQ UCHAR (CHAR-UPCASE CHAR))
                (SELECTOR UCHAR CHAR=
                  (#/^
                   (POINT-PDL-POP *WINDOW*)     ;Already done once
                   (MULTIPLE-VALUE-BIND (BP1 PLINE) (POINT-PDL-POP *WINDOW*)
                     (MOVE-BP BP BP1)
                     (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE))
                   (MUST-REDISPLAY *WINDOW* DIS-BPS)
                   (REDISPLAY *WINDOW* :POINT)
                   (GO GETCHAR))
                  (#/C-R ;C-R: Recurse.
                   (WITH-BP (BP1-COPY BP1 :MOVES)
                     (WITH-BP (BP2-COPY BP2 :NORMAL)
                       (CONTROL-R)
                       (MOVE-BP (POINT) BP2-COPY)
                       (MOVE-BP BP1 BP1-COPY)
                       (MOVE-BP BP2 (IF (BP-< BP2-COPY BP1-COPY) BP1-COPY BP2-COPY))))
                   (MUST-REDISPLAY *WINDOW* DIS-BPS)
                   (REDISPLAY *WINDOW* :POINT)
                   (REDISPLAY-MODE-LINE)
                   (GO GETCHAR))
                  ((#/Clear-Screen #/c-L)
                   (MUST-REDISPLAY *WINDOW*
                                   (IF (CHAR= UCHAR #/CLEAR-SCREEN)
                                       DIS-ALL
                                       (COM-RECENTER-WINDOW)))
                   (REDISPLAY *WINDOW* :POINT)
                   (GO GETCHAR))
                  ((#/? #/Help)
                   (PRINT-DOC :FULL *CURRENT-COMMAND*)
                   (CHECK-FOR-TYPEOUT-WINDOW-TYPEOUT)
                   (REDISPLAY-ALL-WINDOWS)
                   (GO GETCHAR))
                  ;; Other commands.  RETURNing from below here exits the main loop.
                  (#/Space (QREP))             ;Space: Replace and continue.
                  (#/Rubout NIL)               ;Rubout: Continue.
                  (#/,                         ;Comma:
                   (QREP)
                   (SETQ FLAG-1 T))
                  ((#/Altmode #/End) (RETURN NIL))    ;Altmode: Quit.
                  (#/. (QREP)                  ;Point: Replace and quit.
                   (RETURN NIL))
                  (#/c-W                       ;C-W: Delete, and recurse.
                   (DELETE-INTERVAL BP BP1)
                   (MUST-REDISPLAY *WINDOW* DIS-TEXT)
                   (CONTROL-R))
                  (#/! (QREP)          ;!: Do this and rest.
                   (SETQ DO-THE-REST T))
                  (OTHERWISE
                   (SEND *STANDARD-INPUT* :UNTYI CHAR)
                   (RETURN 'ABORTED))))))))))

(DEFPROP COM-QUERY-EXCHANGE KIND-OF-QUERY-REPLACE-DOCUMENTATION
         DOCUMENTATION-FUNCTION)
(DEFCOM COM-QUERY-EXCHANGE "Query replace two strings with one another at the same time.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*) "exchange")
    (LET ((START-BP (POINT))
          (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
        (UNLESS (BP-< (POINT) (MARK))
          (SWAP-BPS (POINT) (MARK)))
        (SETQ END-BP (MARK)))
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
                                          *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
        (QUERY-REPLACE-LIST START-BP END-BP (LIST FROM TO) (LIST TO FROM)
                            *NUMERIC-ARG-P*))))
  DIS-TEXT)

(DEFPROP COM-MULTIPLE-QUERY-REPLACE KIND-OF-QUERY-REPLACE-DOCUMENTATION
         DOCUMENTATION-FUNCTION)
(DEFCOM COM-MULTIPLE-QUERY-REPLACE "Query replace two sets of strings at the same time.
Strings are read in alternate mini-buffers, ended by a null string.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS (WINDOW-MARK-P *WINDOW*))
    (LET ((START-BP (POINT))
          (END-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (WHEN (WINDOW-MARK-P *WINDOW*)
        (UNLESS (BP-< (POINT) (MARK))
          (SWAP-BPS (POINT) (MARK)))
        (SETQ END-BP (MARK)))
      (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
                                          *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
        (QUERY-REPLACE-LIST START-BP END-BP FROM-LIST TO-LIST *NUMERIC-ARG-P*))))
  DIS-TEXT)

(DEFUN QUERY-REPLACE-LIST (*BP* END-BP FROM-LIST TO-LIST &OPTIONAL BREAKS
                           &AUX *QUERY-FROM* *QUERY-TO* (*STATE* 0))
  (DECLARE (SPECIAL *BP* *STATE*))
  (QUERY-REPLACE-INTERNAL *BP* END-BP FROM-LIST TO-LIST 'QUERY-REPLACE-SEARCH-LIST BREAKS))

(DEFUN QUERY-REPLACE-SEARCH-LIST (BP TO-BP FROM-LIST TO-LIST &AUX TEM)
  (DECLARE (SPECIAL *BP* *STATE*))
  (OR (BP-= BP *BP*) (SETQ *STATE* 0))          ;If bp has moved, reset state
  (MULTIPLE-VALUE (*BP* TEM *STATE*)
    (FSM-SEARCH BP FROM-LIST NIL NIL NIL TO-BP *STATE*))
  (COND (*BP*
         (SETQ *QUERY-FROM* TEM
               *QUERY-TO* (NTH (FIND-POSITION-IN-LIST TEM FROM-LIST) TO-LIST))
         (VALUES *BP* (FORWARD-CHAR *BP* (- (STRING-LENGTH TEM)))))))

(DEFUN MULTIPLE-QUERY-REPLACE-STRINGS (REGION-P &AUX FROM-LIST TO-LIST)
  (DO ((FROM) (TO)) (NIL)
    (MULTIPLE-VALUE (FROM TO)
      (QUERY-REPLACE-STRINGS REGION-P "replace" T))
    (OR FROM (RETURN (VALUES (NREVERSE FROM-LIST) (NREVERSE TO-LIST))))
    (PUSH FROM FROM-LIST)
    (PUSH TO TO-LIST)))

;;; Miscellaneous searching commands

(DEFCOM COM-OCCUR "Display text lines that contain a given string.
With an argument, show the next n lines containing the string.  If
no argument is given, all lines are shown." ()
  (COM-LIST-MATCHING-LINES))

(DEFCOM COM-LIST-MATCHING-LINES "Display text lines that contain a given string.
With an argument, show the next n lines containing the string.  If
no argument is given, all lines are shown." ()
  (LET ((CNT (IF *NUMERIC-ARG-P* *NUMERIC-ARG* MOST-POSITIVE-FIXNUM))
        KEY FUNCTION REVERSE-P BJ-P)
    (MULTIPLE-VALUE (FUNCTION KEY REVERSE-P BJ-P)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Show lines containing:"
                                          *STRING-SEARCH-SINGLE-LINE-COMTAB*))
    (DO ((BP (COND ((NOT BJ-P) (POINT))
                   ((NOT REVERSE-P) (INTERVAL-FIRST-BP *INTERVAL*))
                   (T (INTERVAL-LAST-BP *INTERVAL*))))
         (I 0 (1+ I)))
        (( I CNT) NIL)
      (OR (SETQ BP (FUNCALL FUNCTION BP KEY REVERSE-P)) (RETURN NIL))
      (LET ((LINE (BP-LINE BP))
            (INDEX (BP-INDEX BP)))
        (SEND *STANDARD-OUTPUT* ':ITEM 'BP (CREATE-BP LINE INDEX)
              "~A" (STRING-REMOVE-FONTS LINE)))
      (SEND *STANDARD-OUTPUT* ':FRESH-LINE)
      (OR (SETQ BP (BEG-LINE BP 1)) (RETURN NIL)))
    (SEND *STANDARD-OUTPUT* ':LINE-OUT "Done."))
  DIS-NONE)

(DEFCOM COM-KEEP-LINES "Delete all lines not containing the specified string.
Covers from point to the end of the buffer" ()
  (COM-DELETE-NON-MATCHING-LINES))

(DEFCOM COM-DELETE-NON-MATCHING-LINES "Delete all lines not containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Keep lines containing:"
                                          *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (BEG-LINE (POINT) 0))
          (NEW-BP))
      (DO ((N 0 (1+ N))) (())
        ;; BP points just after the last matching line.
        (SETQ NEW-BP (FUNCALL FUNCTION BP KEY NIL))
        (WHEN (NULL NEW-BP)
          ;; No more matching lines => delete all after there.
          (DELETE-INTERVAL BP (INTERVAL-LAST-BP *INTERVAL*) T)
          (IF (ZEROP N)
              (FORMAT *QUERY-IO* "~&No lines matched -- rest of buffer killed.")
            (FORMAT *QUERY-IO* "~&~D line~:P retained." N))
          (RETURN NIL))
        ;; Else delete all from there to beginning of the matching line.
        (DELETE-INTERVAL BP (BEG-LINE NEW-BP 0) T)
        ;; Set BP to point after the new matching line.
        (OR (SETQ BP (BEG-LINE NEW-BP 1)) (RETURN NIL)))))
  DIS-TEXT)

(DEFCOM COM-FLUSH-LINES "Delete all lines containing the specified string.
Covers from point to the end of the buffer" ()
  (COM-DELETE-MATCHING-LINES))

(DEFCOM COM-DELETE-MATCHING-LINES "Delete all lines containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Flush lines containing:"
                                          *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (BEG-LINE (POINT) 0)))
      (DO ((N 0 (1+ N))) (())
        (UNLESS (SETQ BP (FUNCALL FUNCTION BP KEY))
          (IF (ZEROP N)
              (BARF "~&No occurences found.")
            (FORMAT *QUERY-IO* "~&~D line~:P deleted." N))
          (RETURN NIL))
        (DELETE-INTERVAL (BEG-LINE BP 0)
                         (SETQ BP (BEG-LINE BP 1 T))))))
  DIS-TEXT)

(DEFCOM COM-HOW-MANY "Counts occurrences of a substring, after point." ()
  (COM-COUNT-OCCURRENCES))

(DEFCOM COM-COUNT-OCCURRENCES "Counts occurrences of a substring, after point." ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY REVERSE-P BJ-P)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "How many occurrences of:"
                                          *STRING-SEARCH-SINGLE-LINE-COMTAB*)
    (DO ((BP (COND ((NOT BJ-P) (POINT))
                   ((NOT REVERSE-P) (INTERVAL-FIRST-BP *INTERVAL*))
                   (T (INTERVAL-LAST-BP *INTERVAL*)))
             (FUNCALL FUNCTION BP KEY REVERSE-P))
         (N 0 (1+ N)))
        ((NULL BP)
         (FORMAT *QUERY-IO* "~&There ~:[are~;is~] ~D occurrence~:P.~%" (eql (1- N) 1) (1- N)))))
  DIS-NONE)

(DEFCOM COM-COUNT-WORDS "Counts the number of words in the region or buffer." (KM)
  (WITH-REGION-OR-WHOLE-INTERVAL (TEM)
    (let ((number-of-words (COUNT-OBJECTS 'FORWARD-WORD *INTERVAL*)))
      (FORMAT *QUERY-IO* "~&There ~:[are~;is~] ~D word~:P in the ~A.~%"
              (eql number-of-words 1)
              number-of-words
              (IF TEM "region" "buffer"))))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES "Counts the number of lines in the region or buffer." (KM)
  (WITH-REGION-OR-WHOLE-INTERVAL (TEM)
    (let ((number-of-lines (1- (count-lines *interval*))))
      (FORMAT *QUERY-IO* "~&There ~:[are~;is~] ~D line~:P in the ~A.~%"
              (eql number-of-lines 1)
              number-of-lines
              (IF TEM "region" "buffer"))))
  DIS-NONE)

(DEFCOM COM-COUNT-CHARACTERS "Counts the number of characters in the region or buffer." (KM)
  (WITH-REGION-OR-WHOLE-INTERVAL (TEM)
    (let ((number-of-chars (COUNT-CHARS *INTERVAL*)))
      (FORMAT *QUERY-IO* "~&There ~:[are~;is~] ~D character~:P in the ~A.~%"
              (eql number-of-chars 1)
              number-of-chars
              (IF TEM "region" "buffer"))))
  DIS-NONE)
